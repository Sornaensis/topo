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
  , newConnectionOnlyPluginRuntime
  , pluginLifecycleSnapshot
  , queryPluginResource
  )
import Actor.UiActions (ActorHandles(..))
import Actor.UI.Setters
  ( setUiConfigTab
  , setUiDataResources
  , setUiPluginNames
  , setUiShowConfig
  )
import Actor.UI.State (ConfigTab(..))
import Control.Concurrent
  ( MVar
  , ThreadId
  , forkIO
  , killThread
  , newEmptyMVar
  , putMVar
  , readMVar
  , takeMVar
  , threadDelay
  , tryPutMVar
  , tryReadMVar
  )
import Control.Exception (finally, mask, onException)
import Control.Monad (void)
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
import Hyperspace.Actor (call)
import Seer.Command.Dispatch (CommandContext(..))
import Seer.DataBrowser.Executor (waitDataBrowserExecutorIdle)
import Seer.Headless
  ( HeadlessApp
  , defaultHeadlessConfig
  , headlessAppService
  , headlessCommandContext
  , headlessServiceContext
  , withHeadlessApp
  )
import Seer.HTTP.Server
  ( HttpRequest(..)
  , HttpResponse(..)
  , defaultHttpServerConfig
  , handleHttpRequest
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

  it "returns accepted mutation clicks while direct service and HTTP calls await the same gate" $ do
    gate <- RpcGate <$> newEmptyMVar <*> newEmptyMVar
    result <- timeout 30000000 $ withGatedCrudFixtureApp gate $ \app -> do
      selected <- expectE2EWithin "resource click acceptance" $ clickWidget app
        ("WidgetDataResourceSelect:" <> crudPluginName <> ":" <> crudResourceName)
      lookupValue "status" selected `shouldBe` Just (String "accepted")
      expectE2EWithin "selected-resource RPC start" (takeMVar (rgStarted gate))
      putMVar (rgRelease gate) ()
      expectE2EWithin "selected-resource worker idle" $ waitDataBrowserExecutorIdle
        (ccDataBrowserExecutor (headlessCommandContext (cfaHeadlessApp app)))

      _ <- clickWidget app "WidgetDataRecordSelect:0"
      _ <- clickWidget app "WidgetDataDeleteBtn"
      acceptedDelete <- expectE2EWithin "delete click acceptance" $
        clickWidget app "WidgetDataDeleteConfirm"
      lookupValue "status" acceptedDelete `shouldBe` Just (String "accepted")
      lookupValue "operation" acceptedDelete `shouldBe` Just (String "delete_record")
      lookupValue "request_id" acceptedDelete `shouldSatisfy`
        maybe False (\case Number _ -> True; _ -> False)
      lookupValue "info" acceptedDelete `shouldBe` Nothing
      lookupValue "saved" acceptedDelete `shouldBe` Nothing
      lookupValue "deleted" acceptedDelete `shouldBe` Nothing
      expectE2EWithin "delete RPC start" (takeMVar (rgStarted gate))
      pendingState <- serviceOk app "data_get_state" Null
      lookupValue "loading" pendingState `shouldBe` Just (Bool True)
      tryReadMVar (rgRelease gate) `shouldReturn` Nothing

      busy <- expectE2EWithin "busy click rejection" $ serviceResult app "click_widget" $ object
        [ "widget_id" .= ("WidgetDataDeleteConfirm" :: Text) ]
      case busy of
        Left err -> serviceErrorText err `shouldSatisfy`
          Text.isInfixOf "data browser mutation in progress"
        Right _ -> expectationFailure "duplicate mutation click was accepted while busy"
      invalid <- expectE2EWithin "invalid click rejection" $ serviceResult app "click_widget" $ object
        [ "widget_id" .= ("WidgetDataNotAControl" :: Text) ]
      case invalid of
        Left err -> serviceErrorText err `shouldSatisfy` Text.isInfixOf "unknown widget_id"
        Right _ -> expectationFailure "invalid widget click was accepted"
      tryReadMVar (rgStarted gate) `shouldReturn` Nothing

      putMVar (rgRelease gate) ()
      expectE2EWithin "delete worker idle" $ waitDataBrowserExecutorIdle
        (ccDataBrowserExecutor (headlessCommandContext (cfaHeadlessApp app)))

      serviceDone <- newEmptyMVar
      _ <- forkIO $ serviceResult app "data_list_records" (object
        [ "plugin" .= crudPluginName
        , "resource" .= crudResourceName
        ]) >>= putMVar serviceDone
      expectE2EWithin "direct service RPC start" (takeMVar (rgStarted gate))
      tryReadMVar serviceDone `shouldReturn` Nothing
      putMVar (rgRelease gate) ()
      expectE2EWithin "direct service completion" (takeMVar serviceDone) >>= \case
        Right _ -> pure ()
        Left err -> expectationFailure ("gated direct service failed: " <> Text.unpack (serviceErrorText err))

      httpDone <- newEmptyMVar
      _ <- forkIO $ request app (mkRequest "GET" ["data", "records"])
        { hreqQuery =
            [ ("plugin", Just crudPluginName)
            , ("resource", Just crudResourceName)
            ]
        } >>= putMVar httpDone
      expectE2EWithin "HTTP RPC start" (takeMVar (rgStarted gate))
      tryReadMVar httpDone `shouldReturn` Nothing
      putMVar (rgRelease gate) ()
      response <- expectE2EWithin "HTTP completion" (takeMVar httpDone)
      hresStatusCode response `shouldBe` 200
    result `shouldBe` Just ()

  it "denies direct service/API data calls without manifest capabilities" $ do
    readResult <- timeout 30000000 $ withCrudFixturePlugin readlessCrudManifest [crudSchema] $ \app -> do
      denied <- serviceResult app "data_list_records" $ object
        [ "plugin" .= crudPluginName
        , "resource" .= crudResourceName
        ]
      expectServiceErrorCode "permission_denied" denied
    case readResult of
      Nothing -> expectationFailure "read capability denial test timed out"
      Just () -> pure ()

    writeResult <- timeout 30000000 $ withCrudFixturePlugin writelessCrudManifest [crudSchema] $ \app -> do
      denied <- request app (mkRequest "POST" ["data", "records"])
        { hreqBody = Just $ object
            [ "plugin" .= crudPluginName
            , "resource" .= crudResourceName
            , "fields" .= completeRecordObject "blocked" "Blocked" 2 3 "built" "blocked-etag"
            ]
        }
      hresStatusCode denied `shouldBe` 403
      lookupNestedText ["error", "code"] (hresBody denied) `shouldBe` Just "permission_denied"
    case writeResult of
      Nothing -> expectationFailure "write capability denial test timed out"
      Just () -> pure ()

  it "validates pagination at the plugin-manager router" $ do
    result <- timeout 30000000 $ withCrudFixturePlugin crudManifest [crudSchema] $ \app -> do
      let pluginHandle = ahPluginManagerHandle (ccActorHandles (headlessCommandContext (cfaHeadlessApp app)))
          invalidPageSize = QueryResource crudResourceName QueryAll (Just 0) (Just 0)
          invalidPageOffset = QueryResource crudResourceName QueryAll (Just 1) (Just (-1))
          excessivePageSize = QueryResource crudResourceName QueryAll (Just (pageSize + 1)) (Just 0)
      expectDataResourceErrorCode "schema_validation_failed" =<< queryPluginResource pluginHandle crudPluginName invalidPageSize
      expectDataResourceErrorCode "schema_validation_failed" =<< queryPluginResource pluginHandle crudPluginName invalidPageOffset
      expectDataResourceErrorCode "schema_validation_failed" =<< queryPluginResource pluginHandle crudPluginName excessivePageSize
    case result of
      Nothing -> expectationFailure "router pagination validation test timed out"
      Just () -> pure ()

data CrudFixtureApp = CrudFixtureApp
  { cfaHeadlessApp :: !HeadlessApp
  }

data RpcGate = RpcGate
  { rgStarted :: !(MVar ())
  , rgRelease :: !(MVar ())
  }

data FixtureRpcWorker = FixtureRpcWorker
  { frwThread :: !ThreadId
  , frwDone :: !(MVar ())
  , frwHostTransport :: !Transport
  , frwPluginTransport :: !Transport
  , frwGate :: !(Maybe RpcGate)
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
  withCrudFixturePlugin crudManifest [crudSchema] $ \app -> do
    let headlessApp = cfaHeadlessApp app
        handles = ccActorHandles (headlessCommandContext headlessApp)
        pluginHandle = ahPluginManagerHandle handles
        uiHandle = ahUiHandle handles
    resources <- getPluginDataResources pluginHandle
    setUiPluginNames uiHandle [crudPluginName]
    setUiDataResources uiHandle resources
    setUiShowConfig uiHandle True
    setUiConfigTab uiHandle ConfigData
    waitForWidgets app ("WidgetDataPluginSelect:" <> crudPluginName)
    action app

withCrudFixturePlugin :: RPCManifest -> [DataResourceSchema] -> (CrudFixtureApp -> IO a) -> IO a
withCrudFixturePlugin manifest negotiatedResources =
  withCrudFixturePluginGate Nothing manifest negotiatedResources

withGatedCrudFixtureApp :: RpcGate -> (CrudFixtureApp -> IO a) -> IO a
withGatedCrudFixtureApp gate action =
  withCrudFixturePluginGate (Just gate) crudManifest [crudSchema] $ \app -> do
    let headlessApp = cfaHeadlessApp app
        handles = ccActorHandles (headlessCommandContext headlessApp)
        pluginHandle = ahPluginManagerHandle handles
        uiHandle = ahUiHandle handles
    resources <- getPluginDataResources pluginHandle
    setUiPluginNames uiHandle [crudPluginName]
    setUiDataResources uiHandle resources
    setUiShowConfig uiHandle True
    setUiConfigTab uiHandle ConfigData
    action app

withCrudFixturePluginGate
  :: Maybe RpcGate
  -> RPCManifest
  -> [DataResourceSchema]
  -> (CrudFixtureApp -> IO a)
  -> IO a
withCrudFixturePluginGate rpcGate manifest negotiatedResources action = do
  workerSlot <- newEmptyMVar
  (withHeadlessApp defaultHeadlessConfig $ \headlessApp -> mask $ \restore -> do
    worker <- installCrudFixturePlugin rpcGate headlessApp manifest negotiatedResources
    putMVar workerSlot worker
    restore (action (CrudFixtureApp headlessApp)) `finally` releaseRpcGate rpcGate)
    `finally` do
      maybeWorker <- tryReadMVar workerSlot
      case maybeWorker of
        Nothing -> pure ()
        Just worker -> joinFixtureRpcWorker worker

installCrudFixturePlugin
  :: Maybe RpcGate
  -> HeadlessApp
  -> RPCManifest
  -> [DataResourceSchema]
  -> IO FixtureRpcWorker
installCrudFixturePlugin rpcGate headlessApp manifest negotiatedResources = mask $ \restore -> do
  recordsRef <- newIORef crudInitialRecords
  (hostTransport, pluginTransport) <- createTransportPair (rmName manifest)
  done <- newEmptyMVar
  thread <- forkIO $
    crudFixtureRpcLoop rpcGate recordsRef pluginTransport
      `finally` void (tryPutMVar done ())
  let worker = FixtureRpcWorker thread done hostTransport pluginTransport rpcGate
  restore (registerCrudFixturePlugin headlessApp manifest negotiatedResources hostTransport)
    `onException` abortFixtureRpcWorker worker
  pure worker

registerCrudFixturePlugin
  :: HeadlessApp
  -> RPCManifest
  -> [DataResourceSchema]
  -> Transport
  -> IO ()
registerCrudFixturePlugin headlessApp manifest negotiatedResources hostTransport = do
  now <- getCurrentTime
  let conn = (newRPCConnection manifest hostTransport Map.empty)
        { rpcResources = negotiatedResources
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
        (map drsName negotiatedResources)
      loaded = LoadedPlugin
        { lpName = rmName manifest
        , lpManifest = manifest
        , lpParams = Map.empty
        , lpStatus = PluginConnected
        , lpLifecycle = lifecycle
        , lpRuntime = Just (newConnectionOnlyPluginRuntime conn)
        , lpStartPolicy = defaultRPCStartPolicy
        , lpRestartHistory = []
        , lpDirectory = "<in-memory-crud-fixture>"
        , lpOverlaySchema = Nothing
        }
      pluginHandle = ahPluginManagerHandle (ccActorHandles (headlessCommandContext headlessApp))
  Just (token, _, _) <- call @"refresh" pluginHandle #refresh ()
  accepted <- call @"finishRefresh" pluginHandle #finishRefresh (token, [loaded])
  accepted `shouldBe` True

releaseRpcGate :: Maybe RpcGate -> IO ()
releaseRpcGate Nothing = pure ()
releaseRpcGate (Just gate) = void (tryPutMVar (rgRelease gate) ())

joinFixtureRpcWorker :: FixtureRpcWorker -> IO ()
joinFixtureRpcWorker worker = do
  releaseRpcGate (frwGate worker)
  timeout 5000000 (readMVar (frwDone worker)) >>= \case
    Just () -> pure ()
    Nothing -> abortFixtureRpcWorker worker

abortFixtureRpcWorker :: FixtureRpcWorker -> IO ()
abortFixtureRpcWorker worker = do
  releaseRpcGate (frwGate worker)
  killThread (frwThread worker)
  expectE2EWithin "fixture RPC worker shutdown" (readMVar (frwDone worker))
    `finally` closeTransport (frwPluginTransport worker)
    `finally` closeTransport (frwHostTransport worker)

createTransportPair :: Text -> IO (Transport, Transport)
createTransportPair pluginName = do
  (hostToPluginRead, hostToPluginWrite) <- createPipe
  (pluginToHostRead, pluginToHostWrite) <- createPipe
  hostResult <- connectPlugin pluginName pluginToHostRead hostToPluginWrite
  pluginResult <- connectPlugin pluginName hostToPluginRead pluginToHostWrite
  case (hostResult, pluginResult) of
    (Right hostTransport, Right pluginTransport) -> pure (hostTransport, pluginTransport)
    (Left err, Right pluginTransport) -> do
      closeTransport pluginTransport
      expectationFailure ("failed to create host transport: " <> show err) >> error "unreachable"
    (Right hostTransport, Left err) -> do
      closeTransport hostTransport
      expectationFailure ("failed to create plugin transport: " <> show err) >> error "unreachable"
    (Left hostErr, Left pluginErr) ->
      expectationFailure
        ("failed to create fixture transports: " <> show hostErr <> "; " <> show pluginErr)
        >> error "unreachable"

crudFixtureRpcLoop :: Maybe RpcGate -> IORef [DataRecord] -> Transport -> IO ()
crudFixtureRpcLoop rpcGate recordsRef transport = do
  recvMessage transport >>= \case
    Left _ -> closeTransport transport
    Right bytes -> case decodeMessage bytes of
      Left _ -> crudFixtureRpcLoop rpcGate recordsRef transport
      Right envelope -> case envType envelope of
        MsgHandshake -> do
          _ <- sendMessage transport (encodeMessage (handshakeAckEnvelope (envRequestId envelope) currentProtocolVersion))
          crudFixtureRpcLoop rpcGate recordsRef transport
        MsgQueryResource -> do
          awaitRpcGate rpcGate
          case Aeson.fromJSON (envPayload envelope) of
            Aeson.Success queryRequest -> do
              queryResult <- queryCrudRecords recordsRef queryRequest
              _ <- sendMessage transport (encodeMessage (queryResultEnvelope (envRequestId envelope) queryResult))
              crudFixtureRpcLoop rpcGate recordsRef transport
            Aeson.Error err -> do
              _ <- sendMessage transport (encodeMessage (pluginErrorEnvelope (envRequestId envelope) 1005 (Text.pack err)))
              crudFixtureRpcLoop rpcGate recordsRef transport
        MsgMutateResource -> do
          awaitRpcGate rpcGate
          case Aeson.fromJSON (envPayload envelope) of
            Aeson.Success mutateRequest -> do
              mutateResult <- mutateCrudRecords recordsRef mutateRequest
              _ <- sendMessage transport (encodeMessage (mutateResultEnvelope (envRequestId envelope) mutateResult))
              crudFixtureRpcLoop rpcGate recordsRef transport
            Aeson.Error err -> do
              _ <- sendMessage transport (encodeMessage (pluginErrorEnvelope (envRequestId envelope) 1005 (Text.pack err)))
              crudFixtureRpcLoop rpcGate recordsRef transport
        MsgShutdown -> closeTransport transport
        _ -> crudFixtureRpcLoop rpcGate recordsRef transport

awaitRpcGate :: Maybe RpcGate -> IO ()
awaitRpcGate Nothing = pure ()
awaitRpcGate (Just gate) = do
  putMVar (rgStarted gate) ()
  takeMVar (rgRelease gate)

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
      , haSessionId = Nothing
      , haAuthProof = Nothing
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

expectE2EWithin :: String -> IO a -> IO a
expectE2EWithin label action = do
  result <- timeout 5000000 action
  case result of
    Just value -> pure value
    Nothing -> expectationFailure (label <> " deadlocked") >> fail "unreachable"

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
  runServiceOperation headlessAppService (crudServiceContext app) method params

expectServiceErrorCode :: Text -> Either ServiceError ServiceResponse -> Expectation
expectServiceErrorCode expected result = case result of
  Left err -> serviceErrorCode err `shouldBe` expected
  Right _ -> expectationFailure ("expected service error code " <> Text.unpack expected)

expectDataResourceErrorCode :: Text -> Either Text a -> Expectation
expectDataResourceErrorCode expected result = case result of
  Left err -> err `shouldSatisfy` Text.isInfixOf expected
  Right _ -> expectationFailure ("expected data-resource error code " <> Text.unpack expected)

crudServiceContext :: CrudFixtureApp -> ServiceContext
crudServiceContext app = headlessServiceContext (cfaHeadlessApp app)

httpOk :: CrudFixtureApp -> HttpRequest -> IO Value
httpOk app req = do
  rsp <- request app req
  hresStatusCode rsp `shouldBe` 200
  pure (hresBody rsp)

request :: CrudFixtureApp -> HttpRequest -> IO HttpResponse
request app req = handleHttpRequest defaultHttpServerConfig headlessAppService (crudServiceContext app) req

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

readlessCrudManifest :: RPCManifest
readlessCrudManifest = crudManifest
  { rmCapabilities = []
  , rmDataResources = []
  }

writelessCrudManifest :: RPCManifest
writelessCrudManifest = crudManifest
  { rmCapabilities = [CapDataRead]
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
