{-# LANGUAGE OverloadedStrings #-}

module Spec.HTTP (spec) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (SomeException, finally, throwIO, try)
import Control.Monad (forM_)
import Data.Aeson (Value(..), object, (.=))
import Data.Foldable (toList)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.List (find, sort)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Network.HTTP.Client
  ( Manager
  , RequestBody(..)
  , defaultManagerSettings
  , httpLbs
  , method
  , newManager
  , parseRequest
  , requestBody
  , responseStatus
  )
import qualified Network.HTTP.Types.Status as HTTP
import Test.Hspec

import Seer.Headless
  ( HeadlessApp
  , defaultHeadlessConfig
  , headlessServiceContext
  , withHeadlessApp
  )
import Seer.HTTP.Auth (HttpAuthConfig(..), isLoopbackHost, validateHttpAuthConfig)
import Seer.HTTP.OpenAPI
  ( HttpRouteSpec(..)
  , JsonSchema(..)
  , QueryParamSpec(..)
  , RouteBody(..)
  , openApiDocument
  , routePathText
  )
import Seer.HTTP.Server
  ( HttpRequest(..)
  , HttpResponse(..)
  , HttpServerConfig(..)
  , commandHttpRouteSpecs
  , defaultHttpServerConfig
  , forkHttpServer
  , friendlyHttpRouteSpecs
  , handleHttpRequest
  , headlessHttpAppService
  , httpRouteSpecs
  , parseHttpBind
  )
import Seer.Service.AppService (appServiceOperationMethods)
import Seer.System (runApp)
import System.Environment (withArgs)
import Paths_topo_seer (getDataFileName)

spec :: Spec
spec = describe "Seer.HTTP.Server" $ do
  it "parses loopback bind strings" $
    parseHttpBind "127.0.0.1:7373" `shouldBe` Just ("127.0.0.1", 7373)

  it "requires bearer auth for non-loopback binds" $ do
    isLoopbackHost "127.0.0.1" `shouldBe` True
    isLoopbackHost "127.example" `shouldBe` False
    validateHttpAuthConfig (HttpAuthConfig "0.0.0.0" Nothing)
      `shouldBe` Left "non-loopback HTTP bindings require a bearer token"
    validateHttpAuthConfig (HttpAuthConfig "0.0.0.0" (Just "secret"))
      `shouldBe` Right ()

  it "serves health, OpenAPI, state, and screenshot routes in headless mode" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      health <- request app (mkRequest "GET" ["health"])
      hresStatusCode health `shouldBe` 200
      lookupText "status" (hresBody health) `shouldBe` Just "ok"

      openapi <- request app (mkRequest "GET" ["openapi.json"])
      hresStatusCode openapi `shouldBe` 200
      lookupText "openapi" (hresBody openapi) `shouldBe` Just "3.0.3"

      state <- request app (mkRequest "GET" ["state"])
      hresStatusCode state `shouldBe` 200
      objectHasKey "seed" (hresBody state) `shouldBe` True

      dag <- request app (mkRequest "GET" ["simulation", "dag"])
      hresStatusCode dag `shouldBe` 200
      objectHasKey "available" (hresBody dag) `shouldBe` True

      pluginStatus <- request app (mkRequest "GET" ["plugins", "status"])
      hresStatusCode pluginStatus `shouldBe` 200
      objectHasKey "plugins" (hresBody pluginStatus) `shouldBe` True

      screenshot <- request app (mkRequest "POST" ["screenshots"])
      hresStatusCode screenshot `shouldBe` 200
      lookupText "format" (hresBody screenshot) `shouldBe` Just "png"
      lookupText "source" (hresBody screenshot) `shouldBe` Just "headless"
      lookupText "image_base64" (hresBody screenshot) `shouldSatisfy` maybe False (not . Text.null)

  it "coerces signed numeric query parameters before service validation" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      rsp <- request app (mkRequest "GET" ["terrain", "hex"])
        { hreqQuery = [("q", Just "-1"), ("r", Just "0")] }
      lookupNestedText ["error", "code"] (hresBody rsp) `shouldNotBe` Just "validation_failed"

  it "returns validation errors as HTTP 400 JSON envelopes" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      rsp <- request app (mkRequest "PATCH" ["plugins", "enabled"])
        { hreqBody = Just (object []) }
      hresStatusCode rsp `shouldBe` 400
      lookupNestedText ["error", "code"] (hresBody rsp) `shouldBe` Just "validation_failed"

  it "enforces optional bearer tokens on protected routes" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      let cfg = defaultHttpServerConfig { hscBearerToken = Just "secret" }
          ctx = headlessServiceContext app
      denied <- handleHttpRequest cfg headlessHttpAppService ctx (mkRequest "GET" ["state"])
      hresStatusCode denied `shouldBe` 401

      allowed <- handleHttpRequest cfg headlessHttpAppService ctx
        (mkRequest "GET" ["state"]) { hreqHeaders = [("authorization", "Bearer secret")] }
      hresStatusCode allowed `shouldBe` 200

  it "echoes request ids in protected-route error responses" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      let cfg = defaultHttpServerConfig { hscBearerToken = Just "secret" }
          ctx = headlessServiceContext app
      denied <- handleHttpRequest cfg headlessHttpAppService ctx
        (mkRequest "GET" ["state"]) { hreqHeaders = [("x-request-id", "req-123")] }
      hresStatusCode denied `shouldBe` 401
      lookupHeaderText "x-request-id" (hresHeaders denied) `shouldBe` Just "req-123"
      lookupNestedText ["error", "request_id"] (hresBody denied) `shouldBe` Just "req-123"

  it "lists every implemented route in OpenAPI" $ do
    let doc = openApiDocument httpRouteSpecs
    forM_ httpRouteSpecs $ \route -> do
      let path = routePathText route
          routeMethod = Text.toLower (hrsMethod route)
      pathMethods doc path `shouldSatisfy` maybe False (routeMethod `elem`)

  it "keeps OpenAPI paths and route metadata in lockstep" $ do
    let doc = openApiDocument httpRouteSpecs
    openApiSignatureProblems doc `shouldBe` []
    sort (openApiSignatureLines doc) `shouldBe` sort (map routeSignature httpRouteSpecs)

  it "publishes route metadata from the route table into OpenAPI" $ do
    let doc = openApiDocument httpRouteSpecs
    forM_ httpRouteSpecs $ \route -> do
      let path = routePathText route
          routeMethod = Text.toLower (hrsMethod route)
      pathOperation doc path routeMethod `shouldSatisfy` maybe False (const True)
      operationTags doc path routeMethod `shouldBe` Just [hrsTag route]
      operationQueryParameterInfo doc path routeMethod `shouldBe` Just (routeQueryParameterInfo route)
      operationRequestBodyRequired doc path routeMethod `shouldBe` Just (routeRequestBodyRequired route)
      operationHasSecurity doc path routeMethod "bearerAuth" `shouldBe` (hrsOperationId route /= "meta.health")

  it "publishes named schemas for every friendly resource route" $ do
    let doc = openApiDocument httpRouteSpecs
        missingResponses =
          [ routeSignature route
          | route <- friendlyHttpRouteSpecs
          , hrsResponseSchema route == Nothing
          ]
        missingRequests =
          [ routeSignature route
          | route <- friendlyHttpRouteSpecs
          , routeRequestBodyRequired route /= Nothing
          , hrsRequestSchema route == Nothing
          ]
    missingResponses `shouldBe` []
    missingRequests `shouldBe` []
    forM_ friendlyHttpRouteSpecs $ \route -> do
      let path = routePathText route
          routeMethod = Text.toLower (hrsMethod route)
      case hrsResponseSchema route of
        Nothing -> expectationFailure ("missing response schema for " <> Text.unpack (routeSignature route))
        Just schema ->
          operationResponseSchemaRef doc path routeMethod "200" `shouldBe` Just (jsonSchemaName schema)
      case (routeRequestBodyRequired route, hrsRequestSchema route) of
        (Nothing, _) -> pure ()
        (Just _, Nothing) -> expectationFailure ("missing request schema for " <> Text.unpack (routeSignature route))
        (Just _, Just schema) ->
          operationRequestSchemaRef doc path routeMethod `shouldBe` Just (jsonSchemaName schema)

  it "publishes component schemas for resource route groups" $ do
    let doc = openApiDocument httpRouteSpecs
        responseRefs =
          [ ("/presets", "get", "PresetsListResponse")
          , ("/presets", "post", "PresetsSaveResponse")
          , ("/presets/load", "post", "PresetsLoadResponse")
          , ("/pipeline", "get", "PipelineGetResponse")
          , ("/pipeline/stages", "patch", "PipelineSetStageEnabledResponse")
          , ("/plugins", "get", "PluginListResponse")
          , ("/plugins/status", "get", "PluginListResponse")
          , ("/plugins/state", "get", "PluginListResponse")
          , ("/plugins/dependencies", "get", "PluginListResponse")
          , ("/plugins/enabled", "patch", "PluginSetEnabledResponse")
          , ("/plugins/params", "patch", "PluginSetParamResponse")
          , ("/data/plugins", "get", "DataPluginsListResponse")
          , ("/data/resources", "get", "DataResourcesListResponse")
          , ("/data/records", "get", "DataRecordsListResponse")
          , ("/data/records/get", "post", "DataRecordGetResponse")
          , ("/data/records", "post", "DataRecordCreateResponse")
          , ("/data/records", "put", "DataRecordUpdateResponse")
          , ("/data/records", "delete", "DataRecordDeleteResponse")
          , ("/data/state", "get", "DataStateResponse")
          , ("/simulation", "get", "SimulationStateResponse")
          , ("/simulation/dag", "get", "SimulationDagResponse")
          , ("/simulation/auto-tick", "post", "SimulationAutoTickResponse")
          , ("/simulation/tick", "post", "SimulationTickResponse")
          , ("/logs", "get", "LogGetResponse")
          , ("/screenshots", "post", "ScreenshotTakeResponse")
          ] :: [(Text, Text, Text)]
        requestRefs =
          [ ("/presets", "post", "PresetsSaveRequest")
          , ("/presets/load", "post", "PresetsLoadRequest")
          , ("/pipeline/stages", "patch", "PipelineSetStageEnabledRequest")
          , ("/plugins/enabled", "patch", "PluginSetEnabledRequest")
          , ("/plugins/params", "patch", "PluginSetParamRequest")
          , ("/data/records/get", "post", "DataRecordGetRequest")
          , ("/data/records", "post", "DataRecordCreateRequest")
          , ("/data/records", "put", "DataRecordUpdateRequest")
          , ("/data/records", "delete", "DataRecordDeleteRequest")
          , ("/simulation/auto-tick", "post", "SimulationAutoTickRequest")
          , ("/simulation/tick", "post", "SimulationTickRequest")
          , ("/screenshots", "post", "ScreenshotTakeRequest")
          ] :: [(Text, Text, Text)]
    forM_ responseRefs $ \(path, routeMethod, schemaName) -> do
      operationResponseSchemaRef doc path routeMethod "200" `shouldBe` Just schemaName
      schemaComponentNames doc `shouldSatisfy` elem schemaName
    forM_ requestRefs $ \(path, routeMethod, schemaName) -> do
      operationRequestSchemaRef doc path routeMethod `shouldBe` Just schemaName
      schemaComponentNames doc `shouldSatisfy` elem schemaName
    componentRequiredFields doc "PipelineSetStageEnabledRequest" `shouldBe` Just ["stage", "enabled"]
    componentRequiredFields doc "DataRecordUpdateRequest" `shouldBe` Just ["plugin", "resource", "key", "fields"]
    componentPropertyNullable doc "DataRecordsListResponse" "total_count" `shouldBe` Just True
    sort <$> componentPropertyNames doc "ScreenshotTakeResponse"
      `shouldBe` Just ["format", "image_base64", "saved_path", "source"]
    schemaComponentNames doc `shouldSatisfy` elem "ErrorEnvelope"

  it "has a handler for every route spec" $ do
    let missingHandlers =
          [ routeSignature route
          | route <- httpRouteSpecs
          , not (routeHasHandler route)
          ]
    missingHandlers `shouldBe` []

  it "matches the committed served OpenAPI route golden" $ do
    golden <- readOpenApiRouteGolden
    let doc = openApiDocument httpRouteSpecs
    openApiSignatureProblems doc `shouldBe` []
    sort (openApiSignatureLines doc) `shouldBe` sort golden

  it "publishes query and auth metadata in OpenAPI" $ do
    let doc = openApiDocument httpRouteSpecs
    queryParameterInfo doc "/terrain/hex" "get"
      `shouldBe` Just [("q", True), ("r", True)]
    queryParameterInfo doc "/config/enums" "get"
      `shouldBe` Just [("type", True)]
    queryParameterInfo doc "/logs" "get"
      `shouldBe` Just [("level", False), ("limit", False), ("offset", False)]
    queryParameterSchemaType doc "/data/records" "get" "page_size" `shouldBe` Just "integer"
    queryParameterSchemaType doc "/data/records" "get" "page_offset" `shouldBe` Just "integer"
    queryParameterSchemaType doc "/logs" "get" "limit" `shouldBe` Just "integer"
    queryParameterSchemaType doc "/logs" "get" "offset" `shouldBe` Just "integer"
    queryParameterSchemaEnum doc "/logs" "get" "level"
      `shouldBe` Just ["debug", "info", "warn", "error"]
    operationHasSecurity doc "/state" "get" "bearerAuth" `shouldBe` True
    operationHasSecurity doc "/health" "get" "bearerAuth" `shouldBe` False

  it "rejects unauthorized WAI requests before parsing protected bodies" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      let cfg = defaultHttpServerConfig { hscBindPort = 7374, hscBearerToken = Just "secret" }
      tid <- forkHttpServer cfg headlessHttpAppService (headlessServiceContext app)
      manager <- newManager defaultManagerSettings
      eventually_ (assertUnauthorizedInvalidJson manager)
        `finally` (do
          killThread tid
          threadDelay 100000)

  it "publishes a command HTTP route for every AppService operation" $
    sort (map (last . hrsPath) commandHttpRouteSpecs)
      `shouldBe` sort appServiceOperationMethods

  it "starts topo-seer headless HTTP endpoints through the CLI" $ do
    tid <- forkIO $
      withArgs ["--headless", "--http", "127.0.0.1:7373", "--test-mode"] runApp
    manager <- newManager defaultManagerSettings
    let endpoints = ["/health", "/version", "/openapi.json", "/state"]
    eventually_ (forM_ endpoints (assertEndpoint manager))
      `finally` (do
        killThread tid
        threadDelay 100000)

request :: HeadlessApp -> HttpRequest -> IO HttpResponse
request app req = handleHttpRequest defaultHttpServerConfig headlessHttpAppService (headlessServiceContext app) req

mkRequest :: Text -> [Text] -> HttpRequest
mkRequest method path = HttpRequest
  { hreqMethod = method
  , hreqPath = path
  , hreqQuery = []
  , hreqHeaders = []
  , hreqBody = Nothing
  }

lookupText :: Text -> Value -> Maybe Text
lookupText key value = case lookupValue key value of
  Just (String text) -> Just text
  _ -> Nothing

lookupHeaderText :: Text -> [(Text, Text)] -> Maybe Text
lookupHeaderText name headers = lookup (Text.toLower name)
  [ (Text.toLower key, value)
  | (key, value) <- headers
  ]

lookupNestedText :: [Text] -> Value -> Maybe Text
lookupNestedText [] _ = Nothing
lookupNestedText [key] value = lookupText key value
lookupNestedText (key:rest) value = lookupValue key value >>= lookupNestedText rest

lookupValue :: Text -> Value -> Maybe Value
lookupValue key (Object obj) = KM.lookup (Key.fromText key) obj
lookupValue _ _ = Nothing

objectHasKey :: Text -> Value -> Bool
objectHasKey key (Object obj) = KM.member (Key.fromText key) obj
objectHasKey _ _ = False

pathMethods :: Value -> Text -> Maybe [Text]
pathMethods doc path = do
  paths <- lookupValue "paths" doc
  Object pathObj <- pure paths
  Object methodObj <- KM.lookup (Key.fromText path) pathObj
  pure (map Key.toText (KM.keys methodObj))

pathOperation :: Value -> Text -> Text -> Maybe Value
pathOperation doc path routeMethod = do
  paths <- lookupValue "paths" doc
  Object pathObj <- pure paths
  Object methodObj <- KM.lookup (Key.fromText path) pathObj
  KM.lookup (Key.fromText routeMethod) methodObj

queryParameterInfo :: Value -> Text -> Text -> Maybe [(Text, Bool)]
queryParameterInfo doc path routeMethod = do
  Object operation <- pathOperation doc path routeMethod
  Array params <- KM.lookup "parameters" operation
  traverse queryInfo (toList params)
  where
    queryInfo (Object param) = do
      String name <- KM.lookup "name" param
      Bool required <- KM.lookup "required" param
      pure (name, required)
    queryInfo _ = Nothing

queryParameterSchemaType :: Value -> Text -> Text -> Text -> Maybe Text
queryParameterSchemaType doc path routeMethod name = do
  Object schema <- queryParameterSchema doc path routeMethod name
  String schemaType <- KM.lookup "type" schema
  pure schemaType

queryParameterSchemaEnum :: Value -> Text -> Text -> Text -> Maybe [Text]
queryParameterSchemaEnum doc path routeMethod name = do
  Object schema <- queryParameterSchema doc path routeMethod name
  Array values <- KM.lookup "enum" schema
  traverse enumText (toList values)
  where
    enumText (String value) = Just value
    enumText _ = Nothing

queryParameterSchema :: Value -> Text -> Text -> Text -> Maybe Value
queryParameterSchema doc path routeMethod name = do
  Object operation <- pathOperation doc path routeMethod
  Array params <- KM.lookup "parameters" operation
  Object param <- find (queryParamNamed name) (toList params)
  KM.lookup "schema" param

queryParamNamed :: Text -> Value -> Bool
queryParamNamed name (Object param) = KM.lookup "name" param == Just (String name)
queryParamNamed _ _ = False

operationHasSecurity :: Value -> Text -> Text -> Text -> Bool
operationHasSecurity doc path routeMethod scheme =
  case pathOperation doc path routeMethod of
    Just (Object operation) -> case KM.lookup "security" operation of
      Just (Array entries) -> any (entryHasScheme scheme) (toList entries)
      _ -> False
    _ -> False
  where
    entryHasScheme expected (Object entry) = KM.member (Key.fromText expected) entry
    entryHasScheme _ _ = False

operationTags :: Value -> Text -> Text -> Maybe [Text]
operationTags doc path routeMethod = do
  Object operation <- pathOperation doc path routeMethod
  Array tags <- KM.lookup "tags" operation
  traverse tagText (toList tags)
  where
    tagText (String tag) = Just tag
    tagText _ = Nothing

operationQueryParameterInfo :: Value -> Text -> Text -> Maybe [(Text, Bool)]
operationQueryParameterInfo doc path routeMethod = do
  Object operation <- pathOperation doc path routeMethod
  case KM.lookup "parameters" operation of
    Nothing -> Just []
    Just (Array params) -> traverse queryInfo (toList params)
    Just _ -> Nothing
  where
    queryInfo (Object param) = do
      String name <- KM.lookup "name" param
      Bool required <- KM.lookup "required" param
      pure (name, required)
    queryInfo _ = Nothing

operationRequestBodyRequired :: Value -> Text -> Text -> Maybe (Maybe Bool)
operationRequestBodyRequired doc path routeMethod = do
  Object operation <- pathOperation doc path routeMethod
  case KM.lookup "requestBody" operation of
    Nothing -> Just Nothing
    Just (Object body) -> do
      Bool required <- KM.lookup "required" body
      pure (Just required)
    Just _ -> Nothing

operationRequestSchemaRef :: Value -> Text -> Text -> Maybe Text
operationRequestSchemaRef doc path routeMethod = do
  Object operation <- pathOperation doc path routeMethod
  body <- KM.lookup "requestBody" operation
  schemaRefFromContent body

operationResponseSchemaRef :: Value -> Text -> Text -> Text -> Maybe Text
operationResponseSchemaRef doc path routeMethod status = do
  Object operation <- pathOperation doc path routeMethod
  Object responses <- KM.lookup "responses" operation
  response <- KM.lookup (Key.fromText status) responses
  schemaRefFromContent response

schemaRefFromContent :: Value -> Maybe Text
schemaRefFromContent (Object container) = do
  Object content <- KM.lookup "content" container
  Object json <- KM.lookup "application/json" content
  schema <- KM.lookup "schema" json
  schemaRefName schema
schemaRefFromContent _ = Nothing

schemaRefName :: Value -> Maybe Text
schemaRefName (Object schema) = do
  String ref <- KM.lookup "$ref" schema
  Text.stripPrefix "#/components/schemas/" ref
schemaRefName _ = Nothing

schemaComponentNames :: Value -> [Text]
schemaComponentNames doc =
  case lookupValue "components" doc of
    Just (Object components) -> case KM.lookup "schemas" components of
      Just (Object schemas) -> map Key.toText (KM.keys schemas)
      _ -> []
    _ -> []

componentRequiredFields :: Value -> Text -> Maybe [Text]
componentRequiredFields doc name = do
  Object schema <- schemaComponent doc name
  Array required <- KM.lookup "required" schema
  traverse requiredText (toList required)
  where
    requiredText (String field) = Just field
    requiredText _ = Nothing

componentPropertyNames :: Value -> Text -> Maybe [Text]
componentPropertyNames doc name = do
  Object schema <- schemaComponent doc name
  Object properties <- KM.lookup "properties" schema
  pure (map Key.toText (KM.keys properties))

componentPropertyNullable :: Value -> Text -> Text -> Maybe Bool
componentPropertyNullable doc name property = do
  Object propertySchema <- componentProperty doc name property
  Bool nullable <- KM.lookup "nullable" propertySchema
  pure nullable

componentProperty :: Value -> Text -> Text -> Maybe Value
componentProperty doc name property = do
  Object schema <- schemaComponent doc name
  Object properties <- KM.lookup "properties" schema
  KM.lookup (Key.fromText property) properties

schemaComponent :: Value -> Text -> Maybe Value
schemaComponent doc name = do
  Object components <- lookupValue "components" doc
  Object schemas <- KM.lookup "schemas" components
  KM.lookup (Key.fromText name) schemas

routeQueryParameterInfo :: HttpRouteSpec -> [(Text, Bool)]
routeQueryParameterInfo route =
  [ (qpsName param, qpsRequired param)
  | param <- hrsQueryParams route
  ]

routeRequestBodyRequired :: HttpRouteSpec -> Maybe Bool
routeRequestBodyRequired route = case hrsRequestBody route of
  NoRequestBody -> Nothing
  OptionalJsonRequestBody -> Just False
  RequiredJsonRequestBody -> Just True

routeSignature :: HttpRouteSpec -> Text
routeSignature route =
  Text.unwords [hrsMethod route, routePathText route, hrsOperationId route]

openApiSignatureLines :: Value -> [Text]
openApiSignatureLines doc =
  case lookupValue "paths" doc of
    Just (Object paths) ->
      [ Text.unwords [Text.toUpper (Key.toText methodKey), Key.toText pathKey, operationId]
      | (pathKey, Object methods) <- KM.toList paths
      , (methodKey, Object operation) <- KM.toList methods
      , Just (String operationId) <- [KM.lookup "operationId" operation]
      ]
    _ -> []

openApiSignatureProblems :: Value -> [Text]
openApiSignatureProblems doc =
  case lookupValue "paths" doc of
    Just (Object paths) -> concat
      [ methodProblems pathKey methodKey operation
      | (pathKey, Object methods) <- KM.toList paths
      , (methodKey, operation) <- KM.toList methods
      ]
    Just _ -> ["OpenAPI paths is not an object"]
    Nothing -> ["OpenAPI paths is missing"]
  where
    methodProblems pathKey methodKey (Object operation) =
      case KM.lookup "operationId" operation of
        Just (String _) -> []
        Just _ -> [signaturePrefix pathKey methodKey <> " has non-string operationId"]
        Nothing -> [signaturePrefix pathKey methodKey <> " is missing operationId"]
    methodProblems pathKey methodKey _ =
      [signaturePrefix pathKey methodKey <> " operation is not an object"]

    signaturePrefix pathKey methodKey =
      Text.unwords [Text.toUpper (Key.toText methodKey), Key.toText pathKey]

routeHasHandler :: HttpRouteSpec -> Bool
routeHasHandler route = case hrsServiceMethod route of
  Just methodName -> methodName `elem` appServiceOperationMethods
  Nothing -> hrsOperationId route `elem` specialOperationIds

specialOperationIds :: [Text]
specialOperationIds =
  [ "meta.health"
  , "meta.version"
  , "meta.openapi"
  , "events.list"
  ]

readOpenApiRouteGolden :: IO [Text]
readOpenApiRouteGolden = do
  path <- getDataFileName "test/golden/openapi-routes.txt"
  filter (not . Text.null) . map Text.strip . Text.lines
    <$> TextIO.readFile path

assertEndpoint :: Manager -> String -> IO ()
assertEndpoint manager path = do
  req <- parseRequest ("http://127.0.0.1:7373" <> path)
  rsp <- httpLbs req manager
  HTTP.statusCode (responseStatus rsp) `shouldBe` 200

assertUnauthorizedInvalidJson :: Manager -> IO ()
assertUnauthorizedInvalidJson manager = do
  req0 <- parseRequest "http://127.0.0.1:7374/screenshots"
  let req = req0
        { method = "POST"
        , requestBody = RequestBodyLBS "{not-json"
        }
  rsp <- httpLbs req manager
  HTTP.statusCode (responseStatus rsp) `shouldBe` 401

eventually_ :: IO () -> IO ()
eventually_ action = go (30 :: Int)
  where
    go attempts = do
      result <- try action :: IO (Either SomeException ())
      case result of
        Right () -> pure ()
        Left err
          | attempts <= 0 -> throwIO err
          | otherwise -> do
              threadDelay 100000
              go (attempts - 1)
