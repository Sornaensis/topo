{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Spec.DataResourceE2E (spec) where

import Actor.PluginManager
  ( LoadedPlugin(..)
  , PluginLifecycleState(..)
  , PluginStatus(..)
  , getPluginDataResources
  , pluginLifecycleSnapshot
  )
import Actor.UiActions (ActorHandles(..))
import Actor.UI.Setters
  ( setUiConfigTab
  , setUiDataResources
  , setUiPluginNames
  , setUiShowConfig
  )
import Actor.UI.State (ConfigTab(..))
import Control.Concurrent (forkIO, threadDelay)
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (find, sort, sortOn)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (getCurrentTime)
import Data.Word (Word64)
import Hyperspace.Actor (cast)
import Seer.Command.Dispatch (CommandContext(..))
import Seer.Headless
  ( HeadlessApp
  , defaultHeadlessConfig
  , headlessCommandContext
  , headlessServiceContext
  , withHeadlessApp
  )
import Seer.HTTP.Server
  ( HttpRequest(..)
  , HttpResponse(..)
  , defaultHttpServerConfig
  , handleHttpRequest
  , headlessHttpAppService
  )
import Seer.Service.AppService (runServiceOperation)
import Seer.Service.Context (ServiceContext)
import Seer.Service.Types (ServiceError, ServiceResponse(..), serviceErrorCode, serviceErrorText)
import System.Process (createPipe)
import System.Timeout (timeout)
import Test.Hspec
import Topo.Plugin (Capability(..))
import Topo.Plugin.DataResource
  ( DataFieldDef(..)
  , DataFieldType(..)
  , DataOperations(..)
  , DataPagination(..)
  , DataResourceSchema(..)
  , currentDataResourceSchemaVersion
  , defaultDataResourceVersion
  , noOperations
  )
import Topo.Plugin.RPC
  ( RPCConnection(..)
  , RPCManifest(..)
  , defaultRPCManifestRuntime
  , defaultRPCStartPolicy
  , defaultRPCUIHints
  , newRPCConnection
  )
import Topo.Plugin.RPC.DataService
  ( DataMutation(..)
  , DataQuery(..)
  , DataRecord(..)
  , DataResourceErrorCode(..)
  , MutateResource(..)
  , MutateResult(..)
  , QueryResource(..)
  , QueryResult(..)
  )
import Topo.Plugin.RPC.Protocol
  ( HandshakeAck(..)
  , RPCEnvelope(..)
  , RPCMessageType(..)
  , currentProtocolVersion
  , decodeMessage
  , encodeMessage
  )
import Topo.Plugin.RPC.Transport
  ( Transport
  , closeTransport
  , connectPlugin
  , recvMessage
  , sendMessage
  )

spec :: Spec
spec = describe "DataResource CRUD service/API/UI e2e" $ do
  it "crud_e2e" $ do
    result <- timeout 30000000 (withCrudFixtureApp exerciseCrudFixture)
    case result of
      Nothing -> expectationFailure "CRUD e2e fixture test timed out"
      Just () -> pure ()

data CrudFixtureApp = CrudFixtureApp
  { cfaHeadlessApp :: !HeadlessApp
  }

exerciseCrudFixture :: CrudFixtureApp -> IO ()
exerciseCrudFixture app = do
  plugins <- serviceOk app "data_list_plugins" Null
  lookupTextList "plugin" "plugins" plugins `shouldContain` [crudPluginName]

  resources <- httpOk app (mkRequest "GET" ["data", "resources"])
    { hreqQuery = [("plugin", Just crudPluginName)] }
  resourceOperation resources "query_by_hex" `shouldBe` Just True
  resourceOperation resources "query_by_field" `shouldBe` Just True
  resourceOperation resources "sort" `shouldBe` Just True
  resourceOperation resources "filter" `shouldBe` Just True
  resourceOperation resources "page" `shouldBe` Just True

  firstPage <- serviceOk app "data_list_records" $ object
    [ "plugin" .= crudPluginName
    , "resource" .= crudResourceName
    , "page_size" .= (2 :: Int)
    , "page_offset" .= (0 :: Int)
    ]
  recordFieldTexts "name" firstPage `shouldBe` ["Alpha", "Beta"]
  lookupValue "count" firstPage `shouldBe` Just (Number 2)
  lookupValue "total_count" firstPage `shouldBe` Just (Number 3)
  lookupValue "page_size" firstPage `shouldBe` Just (Number 2)
  lookupValue "page_offset" firstPage `shouldBe` Just (Number 0)

  secondPage <- serviceOk app "data_list_records" $ object
    [ "plugin" .= crudPluginName
    , "resource" .= crudResourceName
    , "page_size" .= (2 :: Int)
    , "page_offset" .= (2 :: Int)
    ]
  recordFieldTexts "name" secondPage `shouldBe` ["Delta"]

  _created <- serviceOk app "data_create_record" $ object
    [ "plugin" .= crudPluginName
    , "resource" .= crudResourceName
    , "fields" .= completeRecordObject "gamma" "Gamma" 2 3 "built" "g1"
    ]

  gamma <- httpOk app (mkRequest "POST" ["data", "records", "get"])
    { hreqBody = Just (recordKeyBody "gamma") }
  lookupNestedText ["record", "name"] gamma `shouldBe` Just "Gamma"

  updated <- httpOk app (mkRequest "PUT" ["data", "records"])
    { hreqBody = Just $ object
        [ "plugin" .= crudPluginName
        , "resource" .= crudResourceName
        , "key" .= ("gamma" :: Text)
        , "fields" .= completeRecordObject "gamma" "Gamma Updated" 2 3 "built" "g2"
        ]
    }
  lookupValue "updated" updated `shouldBe` Just (Bool True)

  byHex <- httpOk app (mkRequest "GET" ["data", "records"])
    { hreqQuery =
        [ ("plugin", Just crudPluginName)
        , ("resource", Just crudResourceName)
        , ("query", Just "by_hex")
        , ("chunk", Just "0")
        , ("tile", Just "1")
        ]
    }
  recordFieldTexts "id" byHex `shouldBe` ["alpha", "beta"]

  byField <- httpOk app (mkRequest "GET" ["data", "records"])
    { hreqQuery =
        [ ("plugin", Just crudPluginName)
        , ("resource", Just crudResourceName)
        , ("query", Just "by_field")
        , ("field", Just "category")
        , ("value", Just "built")
        ]
    }
  sort (recordFieldTexts "id" byField) `shouldBe` ["delta", "gamma"]

  conflict <- request app (mkRequest "PUT" ["data", "records"])
    { hreqBody = Just $ object
        [ "plugin" .= crudPluginName
        , "resource" .= crudResourceName
        , "key" .= ("alpha" :: Text)
        , "fields" .= completeRecordObject "alpha" "Alpha" 0 1 "natural" "stale"
        ]
    }
  hresStatusCode conflict `shouldBe` 409
  lookupNestedText ["error", "code"] (hresBody conflict) `shouldBe` Just "conflict"

  invalid <- request app (mkRequest "POST" ["data", "records"])
    { hreqBody = Just $ object
        [ "plugin" .= crudPluginName
        , "resource" .= crudResourceName
        , "fields" .= object
            [ "id" .= (99 :: Int)
            , "name" .= ("Invalid" :: Text)
            , "chunk" .= (0 :: Int)
            , "tile" .= (0 :: Int)
            , "category" .= ("built" :: Text)
            , "etag" .= ("bad" :: Text)
            ]
        ]
    }
  hresStatusCode invalid `shouldBe` 422
  lookupNestedText ["error", "code"] (hresBody invalid) `shouldBe` Just "schema_validation_failed"

  widgetsBefore <- serviceOk app "list_widgets" Null
  widgetTexts widgetsBefore `shouldContain` ["WidgetDataPluginSelect:" <> crudPluginName]

  _ <- clickWidget app ("WidgetDataPluginSelect:" <> crudPluginName)
  waitForWidgets app ("WidgetDataResourceSelect:" <> crudPluginName <> ":" <> crudResourceName)

  _ <- clickWidget app ("WidgetDataResourceSelect:" <> crudPluginName <> ":" <> crudResourceName)
  waitForState app "record_count" (Number 2)
  uiStatePage1 <- serviceOk app "data_get_state" Null
  lookupValue "record_count" uiStatePage1 `shouldBe` Just (Number 2)
  lookupValue "total_count" uiStatePage1 `shouldBe` Just (Number 4)
  lookupValue "page_offset" uiStatePage1 `shouldBe` Just (Number 0)

  _ <- clickWidget app ("WidgetDataPageNext:" <> crudPluginName <> ":" <> crudResourceName)
  waitForState app "page_offset" (Number 2)
  uiStatePage2 <- serviceOk app "data_get_state" Null
  lookupValue "page_offset" uiStatePage2 `shouldBe` Just (Number 2)
  lookupValue "record_count" uiStatePage2 `shouldBe` Just (Number 2)

  _ <- clickWidget app "WidgetDataRecordSelect:0"
  waitForState app "has_selection" (Bool True)
  selected <- serviceOk app "data_get_state" Null
  lookupValue "has_selection" selected `shouldBe` Just (Bool True)
  lookupValue "selected_key" selected `shouldBe` Just (String "delta")

  _ <- clickWidget app "WidgetDataDeleteBtn"
  _ <- clickWidget app "WidgetDataDeleteConfirm"
  deleted <- request app (mkRequest "POST" ["data", "records", "get"])
    { hreqBody = Just (recordKeyBody "delta") }
  hresStatusCode deleted `shouldBe` 404
  lookupNestedText ["error", "code"] (hresBody deleted) `shouldBe` Just "record_not_found"

  unavailable <- serviceResult app "data_list_records" $ object
    [ "plugin" .= ("missing-plugin" :: Text)
    , "resource" .= crudResourceName
    ]
  fmap serviceResponseBody unavailable `shouldSatisfy` either ((== "plugin_unavailable") . serviceErrorCode) (const False)

withCrudFixtureApp :: (CrudFixtureApp -> IO a) -> IO a
withCrudFixtureApp action =
  withHeadlessApp defaultHeadlessConfig $ \headlessApp -> do
    installCrudFixturePlugin headlessApp
    let handles = ccActorHandles (headlessCommandContext headlessApp)
        pluginHandle = ahPluginManagerHandle handles
        uiHandle = ahUiHandle handles
    resources <- getPluginDataResources pluginHandle
    setUiPluginNames uiHandle [crudPluginName]
    setUiDataResources uiHandle resources
    setUiShowConfig uiHandle True
    setUiConfigTab uiHandle ConfigData
    waitForWidgets (CrudFixtureApp headlessApp) ("WidgetDataPluginSelect:" <> crudPluginName)
    action (CrudFixtureApp headlessApp)

installCrudFixturePlugin :: HeadlessApp -> IO ()
installCrudFixturePlugin headlessApp = do
  recordsRef <- newIORef crudInitialRecords
  (hostTransport, pluginTransport) <- createTransportPair crudPluginName
  _ <- forkIO (crudFixtureRpcLoop recordsRef pluginTransport)
  now <- getCurrentTime
  let conn = (newRPCConnection crudManifest hostTransport Map.empty)
        { rpcResources = [crudSchema]
        , rpcProtocolVersion = currentProtocolVersion
        }
      lifecycle = pluginLifecycleSnapshot
        now
        LifecycleReady
        (Just "test fixture connected")
        Nothing
        Nothing
        Nothing
        Nothing
        (Just currentProtocolVersion)
        [crudResourceName]
      loaded = LoadedPlugin
        { lpName = crudPluginName
        , lpManifest = crudManifest
        , lpParams = Map.empty
        , lpStatus = PluginConnected
        , lpLifecycle = lifecycle
        , lpConnection = Just conn
        , lpProcessHandle = Nothing
        , lpStartPolicy = defaultRPCStartPolicy
        , lpRestartHistory = []
        , lpDirectory = "<in-memory-crud-fixture>"
        , lpOverlaySchema = Nothing
        }
      pluginHandle = ahPluginManagerHandle (ccActorHandles (headlessCommandContext headlessApp))
  cast @"finishRefresh" pluginHandle #finishRefresh [loaded]

createTransportPair :: Text -> IO (Transport, Transport)
createTransportPair pluginName = do
  (hostToPluginRead, hostToPluginWrite) <- createPipe
  (pluginToHostRead, pluginToHostWrite) <- createPipe
  hostResult <- connectPlugin pluginName pluginToHostRead hostToPluginWrite
  pluginResult <- connectPlugin pluginName hostToPluginRead pluginToHostWrite
  case (hostResult, pluginResult) of
    (Right hostTransport, Right pluginTransport) -> pure (hostTransport, pluginTransport)
    (Left err, _) -> expectationFailure ("failed to create host transport: " <> show err) >> error "unreachable"
    (_, Left err) -> expectationFailure ("failed to create plugin transport: " <> show err) >> error "unreachable"

crudFixtureRpcLoop :: IORef [DataRecord] -> Transport -> IO ()
crudFixtureRpcLoop recordsRef transport = do
  recvMessage transport >>= \case
    Left _ -> closeTransport transport
    Right bytes -> case decodeMessage bytes of
      Left _ -> crudFixtureRpcLoop recordsRef transport
      Right envelope -> case envType envelope of
        MsgHandshake -> do
          _ <- sendMessage transport (encodeMessage (handshakeAckEnvelope (envRequestId envelope) currentProtocolVersion))
          crudFixtureRpcLoop recordsRef transport
        MsgQueryResource -> do
          case Aeson.fromJSON (envPayload envelope) of
            Aeson.Success queryRequest -> do
              queryResult <- queryCrudRecords recordsRef queryRequest
              _ <- sendMessage transport (encodeMessage (queryResultEnvelope (envRequestId envelope) queryResult))
              crudFixtureRpcLoop recordsRef transport
            Aeson.Error err -> do
              _ <- sendMessage transport (encodeMessage (pluginErrorEnvelope (envRequestId envelope) 1005 (Text.pack err)))
              crudFixtureRpcLoop recordsRef transport
        MsgMutateResource -> do
          case Aeson.fromJSON (envPayload envelope) of
            Aeson.Success mutateRequest -> do
              mutateResult <- mutateCrudRecords recordsRef mutateRequest
              _ <- sendMessage transport (encodeMessage (mutateResultEnvelope (envRequestId envelope) mutateResult))
              crudFixtureRpcLoop recordsRef transport
            Aeson.Error err -> do
              _ <- sendMessage transport (encodeMessage (pluginErrorEnvelope (envRequestId envelope) 1005 (Text.pack err)))
              crudFixtureRpcLoop recordsRef transport
        MsgShutdown -> closeTransport transport
        _ -> crudFixtureRpcLoop recordsRef transport

queryCrudRecords :: IORef [DataRecord] -> QueryResource -> IO QueryResult
queryCrudRecords recordsRef queryRequest = do
  records <- readIORef recordsRef
  let matching = filter (matchesQuery (qrQuery queryRequest)) (sortRecords records)
      paged = applyPagination (qrPageSize queryRequest) (qrPageOffset queryRequest) matching
  pure (QueryResult (qrResource queryRequest) paged (Just (length matching)))

mutateCrudRecords :: IORef [DataRecord] -> MutateResource -> IO MutateResult
mutateCrudRecords recordsRef mutateRequest =
  atomicModifyIORef' recordsRef $ \records ->
    case mrMutation mutateRequest of
      MutCreate newRecord
        | any (recordHasSameKey newRecord) records ->
            (records, MutateResult False (Just "duplicate key") Nothing (Just DuplicateKey))
        | otherwise ->
            (records <> [newRecord], MutateResult True Nothing (Just newRecord) Nothing)
      MutUpdate key newRecord
        | recordHasField "etag" (String "stale") newRecord ->
            (records, MutateResult False (Just "etag mismatch") Nothing (Just Conflict))
        | any (recordHasField "id" key) records ->
            let updated = replaceByKey key newRecord records
            in (updated, MutateResult True Nothing (Just newRecord) Nothing)
        | otherwise ->
            (records, MutateResult False (Just "record not found") Nothing (Just RecordNotFound))
      MutDelete key ->
        if any (recordHasField "id" key) records
          then
            ( filter (not . recordHasField "id" key) records
            , MutateResult True Nothing Nothing Nothing
            )
          else (records, MutateResult False (Just "record not found") Nothing (Just RecordNotFound))
      MutSetHex chunk tile newRecord ->
        let stamped = addHexFields chunk tile newRecord
            updated = stamped : filter (not . recordAtHex chunk tile) records
        in (updated, MutateResult True Nothing (Just stamped) Nothing)

handshakeAckEnvelope :: Maybe Word64 -> Int -> RPCEnvelope
handshakeAckEnvelope requestId protocolVersion = RPCEnvelope
  { envType = MsgHandshakeAck
  , envPayload = Aeson.toJSON HandshakeAck
      { haProtocolVersion = protocolVersion
      , haDataDirectory = Nothing
      , haResources = []
      }
  , envRequestId = requestId
  }

pluginErrorEnvelope :: Maybe Word64 -> Int -> Text -> RPCEnvelope
pluginErrorEnvelope requestId code message = RPCEnvelope
  { envType = MsgError
  , envPayload = object ["code" .= code, "message" .= message]
  , envRequestId = requestId
  }

queryResultEnvelope :: Maybe Word64 -> QueryResult -> RPCEnvelope
queryResultEnvelope requestId result = RPCEnvelope
  { envType = MsgQueryResult
  , envPayload = Aeson.toJSON result
  , envRequestId = requestId
  }

mutateResultEnvelope :: Maybe Word64 -> MutateResult -> RPCEnvelope
mutateResultEnvelope requestId result = RPCEnvelope
  { envType = MsgMutateResult
  , envPayload = Aeson.toJSON result
  , envRequestId = requestId
  }

waitForWidgets :: CrudFixtureApp -> Text -> IO ()
waitForWidgets app expected = waitUntil ("widget " <> expected) $ do
  widgets <- serviceOk app "list_widgets" Null
  pure (expected `elem` widgetTexts widgets)

waitForState :: CrudFixtureApp -> Text -> Value -> IO ()
waitForState app key expected = waitUntil ("state " <> key) $ do
  state <- serviceOk app "data_get_state" Null
  pure (lookupValue key state == Just expected)

waitUntil :: Text -> IO Bool -> IO ()
waitUntil label action = loop (50 :: Int)
  where
    loop 0 = expectationFailure ("timed out waiting for " <> Text.unpack label)
    loop attempts = do
      done <- action
      if done
        then pure ()
        else threadDelay 10000 >> loop (attempts - 1)

serviceOk :: CrudFixtureApp -> Text -> Value -> IO Value
serviceOk app method params = do
  result <- serviceResult app method params
  case result of
    Right (ServiceResponse body) -> pure body
    Left err -> expectationFailure ("service call failed for " <> Text.unpack method <> ": " <> Text.unpack (serviceErrorCode err) <> " - " <> Text.unpack (serviceErrorText err)) >> pure Null

serviceResult :: CrudFixtureApp -> Text -> Value -> IO (Either ServiceError ServiceResponse)
serviceResult app method params =
  runServiceOperation headlessHttpAppService (crudServiceContext app) method params

crudServiceContext :: CrudFixtureApp -> ServiceContext
crudServiceContext app = headlessServiceContext (cfaHeadlessApp app)

httpOk :: CrudFixtureApp -> HttpRequest -> IO Value
httpOk app req = do
  rsp <- request app req
  hresStatusCode rsp `shouldBe` 200
  pure (hresBody rsp)

request :: CrudFixtureApp -> HttpRequest -> IO HttpResponse
request app req = handleHttpRequest defaultHttpServerConfig headlessHttpAppService (crudServiceContext app) req

mkRequest :: Text -> [Text] -> HttpRequest
mkRequest method path = HttpRequest
  { hreqMethod = method
  , hreqPath = path
  , hreqQuery = []
  , hreqHeaders = []
  , hreqBody = Nothing
  }

clickWidget :: CrudFixtureApp -> Text -> IO Value
clickWidget app widgetId = serviceOk app "click_widget" (object ["widget_id" .= widgetId])

matchesQuery :: DataQuery -> DataRecord -> Bool
matchesQuery QueryAll _ = True
matchesQuery (QueryByKey key) row = recordHasField "id" key row
matchesQuery (QueryByHex chunk tile) row = recordAtHex chunk tile row
matchesQuery (QueryByField field value) row = recordHasField field value row

sortRecords :: [DataRecord] -> [DataRecord]
sortRecords = sortOn (textField "name")

applyPagination :: Maybe Int -> Maybe Int -> [DataRecord] -> [DataRecord]
applyPagination pageLimit pageOffset records = take limitValue (drop offsetValue records)
  where
    limitValue = maybe (length records) id pageLimit
    offsetValue = maybe 0 id pageOffset

replaceByKey :: Value -> DataRecord -> [DataRecord] -> [DataRecord]
replaceByKey key newRecord = map (\row -> if recordHasField "id" key row then newRecord else row)

recordHasSameKey :: DataRecord -> DataRecord -> Bool
recordHasSameKey newRecord row = case Map.lookup "id" (unDataRecord newRecord) of
  Just key -> recordHasField "id" key row
  Nothing -> False

recordAtHex :: Int -> Int -> DataRecord -> Bool
recordAtHex chunk tile row =
  recordHasField "chunk" (Number (fromIntegral chunk)) row
    && recordHasField "tile" (Number (fromIntegral tile)) row

addHexFields :: Int -> Int -> DataRecord -> DataRecord
addHexFields chunk tile (DataRecord fields) = DataRecord
  (Map.insert "chunk" (Number (fromIntegral chunk)) (Map.insert "tile" (Number (fromIntegral tile)) fields))

recordHasField :: Text -> Value -> DataRecord -> Bool
recordHasField field value (DataRecord fields) = Map.lookup field fields == Just value

textField :: Text -> DataRecord -> Text
textField field (DataRecord fields) = case Map.lookup field fields of
  Just (String value) -> value
  _ -> ""

recordKeyBody :: Text -> Value
recordKeyBody key = object
  [ "plugin" .= crudPluginName
  , "resource" .= crudResourceName
  , "key" .= key
  ]

completeRecordObject :: Text -> Text -> Int -> Int -> Text -> Text -> Value
completeRecordObject recordId name chunk tile category etag = object
  [ "id" .= recordId
  , "name" .= name
  , "chunk" .= chunk
  , "tile" .= tile
  , "category" .= category
  , "etag" .= etag
  ]

lookupText :: Text -> Value -> Maybe Text
lookupText key value = case lookupValue key value of
  Just (String text) -> Just text
  _ -> Nothing

lookupNestedText :: [Text] -> Value -> Maybe Text
lookupNestedText [] _ = Nothing
lookupNestedText [key] value = lookupText key value
lookupNestedText (key:rest) value = lookupValue key value >>= lookupNestedText rest

lookupValue :: Text -> Value -> Maybe Value
lookupValue key (Object obj) = KM.lookup (Key.fromText key) obj
lookupValue _ _ = Nothing

lookupTextList :: Text -> Text -> Value -> [Text]
lookupTextList field arrayField value =
  [ text
  | Just (Array values) <- [lookupValue arrayField value]
  , Object row <- toList values
  , Just (String text) <- [KM.lookup (Key.fromText field) row]
  ]

recordFieldTexts :: Text -> Value -> [Text]
recordFieldTexts field value = lookupTextList field "records" value

widgetTexts :: Value -> [Text]
widgetTexts value =
  [ text
  | Just (Array values) <- [lookupValue "widgets" value]
  , String text <- toList values
  ]

resourceOperation :: Value -> Text -> Maybe Bool
resourceOperation value operation = do
  Object resource <- findResourceObject crudResourceName value
  Object operations <- KM.lookup "operations" resource
  Bool enabled <- KM.lookup (Key.fromText operation) operations
  pure enabled

findResourceObject :: Text -> Value -> Maybe Value
findResourceObject name value = do
  Array resources <- lookupValue "resources" value
  find (matches name) (toList resources)
  where
    matches expected (Object resource) = KM.lookup "name" resource == Just (String expected)
    matches _ _ = False

crudInitialRecords :: [DataRecord]
crudInitialRecords =
  [ dataRecord "alpha" "Alpha" 0 1 "natural" "a1"
  , dataRecord "beta" "Beta" 0 1 "natural" "b1"
  , dataRecord "delta" "Delta" 1 2 "built" "d1"
  ]

dataRecord :: Text -> Text -> Int -> Int -> Text -> Text -> DataRecord
dataRecord recordId name chunk tile category etag = DataRecord $ Map.fromList
  [ ("id", String recordId)
  , ("name", String name)
  , ("chunk", Number (fromIntegral chunk))
  , ("tile", Number (fromIntegral tile))
  , ("category", String category)
  , ("etag", String etag)
  ]

crudManifest :: RPCManifest
crudManifest = RPCManifest
  { rmManifestVersion = 3
  , rmName = crudPluginName
  , rmVersion = "0.1.0"
  , rmRuntime = defaultRPCManifestRuntime
  , rmDescription = "In-memory CRUD fixture plugin for e2e tests"
  , rmUiHints = defaultRPCUIHints
  , rmGenerator = Nothing
  , rmSimulation = Nothing
  , rmOverlay = Nothing
  , rmCapabilities = [CapDataRead, CapDataWrite]
  , rmParameters = []
  , rmDataResources = [crudSchema]
  , rmDataDirectory = Nothing
  , rmExternalDataSources = []
  , rmExternalDataSourceRefs = []
  , rmStartPolicy = defaultRPCStartPolicy
  }

crudSchema :: DataResourceSchema
crudSchema = DataResourceSchema
  { drsSchemaVersion = currentDataResourceSchemaVersion
  , drsResourceVersion = defaultDataResourceVersion
  , drsName = crudResourceName
  , drsLabel = "Fixture Items"
  , drsHexBound = True
  , drsFields =
      [ DataFieldDef "id" DFText "ID" False Nothing
      , DataFieldDef "name" DFText "Name" False Nothing
      , DataFieldDef "chunk" DFInt "Chunk" False Nothing
      , DataFieldDef "tile" DFInt "Tile" False Nothing
      , DataFieldDef "category" DFText "Category" False Nothing
      , DataFieldDef "etag" DFText "ETag" False Nothing
      ]
  , drsOperations = noOperations
      { doList = True
      , doGet = True
      , doCreate = True
      , doUpdate = True
      , doDelete = True
      , doQueryByHex = True
      , doQueryByField = True
      , doSort = True
      , doFilter = True
      , doPage = True
      }
  , drsKeyField = "id"
  , drsOverlay = Nothing
  , drsPagination = DataPagination pageSize pageSize 0
  }

crudPluginName :: Text
crudPluginName = "fixture-crud"

crudResourceName :: Text
crudResourceName = "items"

pageSize :: Int
pageSize = 2
