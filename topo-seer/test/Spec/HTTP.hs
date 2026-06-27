{-# LANGUAGE OverloadedStrings #-}

module Spec.HTTP (spec) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (SomeException, finally, throwIO, try)
import Control.Monad (forM_)
import Data.Aeson (Value(..), object, (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (toList)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.List (find, nub, sort)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as TextIO
import Actor.Data (getTerrainSnapshot, setTerrainChunkData)
import Actor.SnapshotReceiver (writeTerrainSnapshot)
import Actor.UiActions (ActorHandles(..))
import Network.HTTP.Client
  ( Manager
  , RequestBody(..)
  , defaultManagerSettings
  , httpLbs
  , method
  , newManager
  , parseRequest
  , requestBody
  , requestHeaders
  , responseBody
  , responseHeaders
  , responseStatus
  )
import qualified Network.HTTP.Types.Status as HTTP
import Test.Hspec

import Seer.Headless
  ( HeadlessApp
  , defaultHeadlessConfig
  , headlessCommandContext
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
  , publicHttpRouteSpecs
  , parseHttpBind
  )
import Seer.Command.Dispatch (CommandContext(..))
import Seer.Service.AppService (appServiceOperationMethods)
import Seer.System (runApp)
import System.Environment (withArgs)
import Topo (WorldConfig(..), chunkIdFromCoord, emptyTerrainChunk)
import Topo.Types (ChunkCoord(..))
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
      pathMethods (hresBody openapi) "/commands/get_state" `shouldBe` Nothing

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

  it "buffers HTTP service events and serves them as SSE" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      seedSet <- request app (mkRequest "POST" ["ui", "seed"])
        { hreqBody = Just (object ["seed" .= (123 :: Int)]) }
      hresStatusCode seedSet `shouldBe` 200

      generationStatus <- request app (mkRequest "GET" ["world", "generation-status"])
      hresStatusCode generationStatus `shouldBe` 200

      simState <- request app (mkRequest "GET" ["simulation"])
      hresStatusCode simState `shouldBe` 200

      events <- request app (mkRequest "GET" ["events"])
      hresStatusCode events `shouldBe` 200
      lookupText "mode" (hresBody events) `shouldBe` Just "polling"
      eventsContainTopic "ui.state.changed" (hresBody events) `shouldBe` True
      eventsContainTopic "world.generation.status" (hresBody events) `shouldBe` True
      eventsContainTopic "simulation.status" (hresBody events) `shouldBe` True
      eventPayloadHasKey "ui.state.changed" "result" (hresBody events) `shouldBe` True
      eventPayloadHasKey "ui.state.changed" "body" (hresBody events) `shouldBe` False

      let cfg = defaultHttpServerConfig { hscBindPort = 7375 }
      tid <- forkHttpServer cfg headlessHttpAppService (headlessServiceContext app)
      manager <- newManager defaultManagerSettings
      eventually_ (assertEventStream manager "ui.state.changed")
        `finally` (do
          killThread tid
          threadDelay 100000)

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

  it "lists every public route in OpenAPI" $ do
    let doc = openApiDocument publicHttpRouteSpecs
    forM_ publicHttpRouteSpecs $ \route -> do
      let path = routePathText route
          routeMethod = Text.toLower (hrsMethod route)
      pathMethods doc path `shouldSatisfy` maybe False (routeMethod `elem`)

  it "omits internal command routes from public OpenAPI" $ do
    let doc = openApiDocument publicHttpRouteSpecs
    pathMethods doc "/commands/get_state" `shouldBe` Nothing
    openApiSignatureLines doc `shouldSatisfy` all (not . Text.isInfixOf "/commands/")

  it "keeps OpenAPI paths and public route metadata in lockstep" $ do
    let doc = openApiDocument publicHttpRouteSpecs
    openApiSignatureProblems doc `shouldBe` []
    sort (openApiSignatureLines doc) `shouldBe` sort (map routeSignature publicHttpRouteSpecs)

  it "publishes public route metadata from the route table into OpenAPI" $ do
    let doc = openApiDocument publicHttpRouteSpecs
    forM_ publicHttpRouteSpecs $ \route -> do
      let path = routePathText route
          routeMethod = Text.toLower (hrsMethod route)
      pathOperation doc path routeMethod `shouldSatisfy` maybe False (const True)
      operationTags doc path routeMethod `shouldBe` Just [hrsTag route]
      operationQueryParameterInfo doc path routeMethod `shouldBe` Just (routeQueryParameterInfo route)
      operationRequestBodyRequired doc path routeMethod `shouldBe` Just (routeRequestBodyRequired route)
      operationHasSecurity doc path routeMethod "bearerAuth" `shouldBe` (hrsOperationId route /= "meta.health")

  it "publishes named schemas for every friendly resource route" $ do
    let doc = openApiDocument publicHttpRouteSpecs
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
    let doc = openApiDocument publicHttpRouteSpecs
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
    let doc = openApiDocument publicHttpRouteSpecs
    openApiSignatureProblems doc `shouldBe` []
    sort (openApiSignatureLines doc) `shouldBe` sort golden

  it "publishes query and auth metadata in OpenAPI" $ do
    let doc = openApiDocument publicHttpRouteSpecs
    queryParameterInfo doc "/terrain/hex" "get"
      `shouldBe` Just [("q", True), ("r", True)]
    queryParameterInfo doc "/terrain/chunk-summary" "get"
      `shouldBe` Just [("chunk", True)]
    queryParameterInfo doc "/config/enums" "get"
      `shouldBe` Just [("type", True)]
    queryParameterInfo doc "/config/sliders" "get"
      `shouldBe` Just [("tab", False)]
    operationRequestBodyRequired doc "/config/sliders/get" "post" `shouldBe` Just (Just True)
    operationRequestSchemaRef doc "/config/sliders/get" "post" `shouldBe` Just "SliderGetRequest"
    queryParameterInfo doc "/logs" "get"
      `shouldBe` Just [("level", False), ("limit", False), ("offset", False)]
    queryParameterInfo doc "/events" "get"
      `shouldBe` Just [("stream", False), ("limit", False)]
    queryParameterSchemaType doc "/data/records" "get" "page_size" `shouldBe` Just "integer"
    queryParameterSchemaType doc "/data/records" "get" "page_offset" `shouldBe` Just "integer"
    queryParameterSchemaType doc "/logs" "get" "limit" `shouldBe` Just "integer"
    queryParameterSchemaType doc "/logs" "get" "offset" `shouldBe` Just "integer"
    queryParameterSchemaEnum doc "/logs" "get" "level"
      `shouldBe` Just ["debug", "info", "warn", "error"]
    queryParameterSchemaEnum doc "/config/sliders" "get" "tab"
      `shouldBe` Just ["terrain", "planet", "climate", "weather", "biome", "erosion"]
    operationResponseContentTypes doc "/events" "get" "200"
      `shouldSatisfy` maybe False ("text/event-stream" `elem`)
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

  it "maps retired MCP tools/list and tools/call coverage to HTTP, service, and OpenAPI routes" $ do
    let doc = openApiDocument publicHttpRouteSpecs
        toolNames = map ptLegacyName retiredMcpToolTargets
    length retiredMcpToolTargets `shouldBe` 85
    toolNames `shouldBe` nub toolNames
    forM_ retiredMcpToolTargets $ \target -> do
      assertFriendlyRouteTarget target
      assertOpenApiTarget doc target
      assertInternalCommandRouteTarget target

  it "dispatches representative retired MCP tools/call primary HTTP routes" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      installTerrainFixture app

      sliders <- request app (mkRequest "GET" ["config", "sliders"])
        { hreqQuery = [("tab", Just "terrain")] }
      hresStatusCode sliders `shouldBe` 200
      slidersHaveTab "terrain" (hresBody sliders) `shouldBe` True

      setSeed <- request app (mkRequest "POST" ["ui", "seed"])
        { hreqBody = Just (object ["seed" .= (456 :: Int)]) }
      hresStatusCode setSeed `shouldBe` 200
      lookupValue "seed" (hresBody setSeed) `shouldBe` Just (Number 456)

      setSliders <- request app (mkRequest "PATCH" ["config", "sliders"])
        { hreqBody = Just (object ["values" .= object ["SliderGenScale" .= (0.3 :: Double)]]) }
      hresStatusCode setSliders `shouldBe` 200
      objectHasKey "updated" (hresBody setSliders) `shouldBe` True

      setCamera <- request app (mkRequest "PUT" ["camera"])
        { hreqBody = Just (object ["x" .= (1.0 :: Double), "y" .= (2.0 :: Double), "zoom" .= (1.5 :: Double)]) }
      hresStatusCode setCamera `shouldBe` 200
      lookupValue "zoom" (hresBody setCamera) `shouldBe` Just (Number 1.5)

      deleteRecord <- request app (mkRequest "DELETE" ["data", "records"])
        { hreqBody = Just (object ["plugin" .= ("missing" :: Text), "resource" .= ("missing" :: Text), "key" .= ("missing" :: Text)]) }
      lookupNestedText ["error", "code"] (hresBody deleteRecord) `shouldNotBe` Just "validation_failed"
      isRouteMiss deleteRecord `shouldBe` False

      hexRsp <- request app (mkRequest "GET" ["terrain", "hex"])
        { hreqQuery = [("q", Just "0"), ("r", Just "0")] }
      hresStatusCode hexRsp `shouldBe` 200
      lookupValue "q" (hresBody hexRsp) `shouldBe` Just (Number 0)
      lookupValue "r" (hresBody hexRsp) `shouldBe` Just (Number 0)

  it "maps retired MCP resources/list coverage to OpenAPI resource routes" $ do
    let doc = openApiDocument publicHttpRouteSpecs
        resourceNames = map ptLegacyName retiredMcpResourceTargets
        templateResources = filter (Text.isInfixOf "{" . ptLegacyName) retiredMcpResourceTargets
        staticResources = filter (not . Text.isInfixOf "{" . ptLegacyName) retiredMcpResourceTargets
    length retiredMcpResourceTargets `shouldBe` 16
    length staticResources `shouldBe` 11
    length templateResources `shouldBe` 5
    resourceNames `shouldBe` nub resourceNames
    forM_ retiredMcpResourceTargets $ \target -> do
      assertFriendlyRouteTarget target
      assertOpenApiTarget doc target

  it "serves retired MCP resources/read targets through HTTP JSON routes" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      installTerrainFixture app
      forM_ retiredMcpResourceReadCases $ \resourceCase -> do
        rsp <- request app (resourceReadRequest resourceCase)
        hresStatusCode rsp `shouldBe` 200
        lookupHeaderText "content-type" (hresHeaders rsp) `shouldBe` Just "application/json"
        isRouteMiss rsp `shouldBe` False
      assertTemplateResourceBodies app

  it "replaces the MCP-to-seer IPC bridge with direct HTTP AppService dispatch" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      stateViaCommand <- request app (mkRequest "POST" ["commands", "get_state"])
      hresStatusCode stateViaCommand `shouldBe` 200
      objectHasKey "seed" (hresBody stateViaCommand) `shouldBe` True

      setSeed <- request app (mkRequest "POST" ["commands", "set_seed"])
        { hreqBody = Just (object ["seed" .= (321 :: Int)]) }
      hresStatusCode setSeed `shouldBe` 200
      lookupValue "seed" (hresBody setSeed) `shouldBe` Just (Number 321)

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

data ParityTarget = ParityTarget
  { ptLegacyName :: !Text
  , ptMethod :: !Text
  , ptPath :: ![Text]
  , ptOperationId :: !Text
  , ptServiceMethod :: !Text
  } deriving (Eq, Show)

target :: Text -> Text -> [Text] -> Text -> Text -> ParityTarget
target = ParityTarget

targetPathText :: ParityTarget -> Text
targetPathText t = "/" <> Text.intercalate "/" (ptPath t)

assertFriendlyRouteTarget :: ParityTarget -> Expectation
assertFriendlyRouteTarget t = case find matching friendlyHttpRouteSpecs of
  Nothing -> expectationFailure ("missing primary HTTP route for " <> Text.unpack (ptLegacyName t) <> ": " <> Text.unpack (routeLabel t))
  Just route -> do
    hrsOperationId route `shouldBe` ptOperationId t
    hrsServiceMethod route `shouldBe` Just (ptServiceMethod t)
  where
    matching route = hrsMethod route == ptMethod t && hrsPath route == ptPath t

assertOpenApiTarget :: Value -> ParityTarget -> Expectation
assertOpenApiTarget doc t =
  operationIdAt doc (targetPathText t) (Text.toLower (ptMethod t)) `shouldBe` Just (ptOperationId t)

assertInternalCommandRouteTarget :: ParityTarget -> Expectation
assertInternalCommandRouteTarget t = case find matching commandHttpRouteSpecs of
  Nothing -> expectationFailure ("missing internal command-compatible HTTP route for " <> Text.unpack (ptLegacyName t) <> " via " <> Text.unpack (ptServiceMethod t))
  Just route -> do
    hrsOperationId route `shouldBe` ("command." <> ptServiceMethod t)
    hrsServiceMethod route `shouldBe` Just (ptServiceMethod t)
  where
    matching route = hrsMethod route == "POST" && hrsPath route == ["commands", ptServiceMethod t]

operationIdAt :: Value -> Text -> Text -> Maybe Text
operationIdAt doc path routeMethod = do
  Object operation <- pathOperation doc path routeMethod
  String op <- KM.lookup "operationId" operation
  pure op

routeLabel :: ParityTarget -> Text
routeLabel t = Text.unwords [ptMethod t, targetPathText t, ptOperationId t]

data ResourceReadCase = ResourceReadCase
  { rrcTarget :: !ParityTarget
  , rrcQuery :: ![(Text, Maybe Text)]
  , rrcBody :: !(Maybe Value)
  } deriving (Eq, Show)

resourceReadCase :: ParityTarget -> [(Text, Maybe Text)] -> Maybe Value -> ResourceReadCase
resourceReadCase = ResourceReadCase

resourceReadRequest :: ResourceReadCase -> HttpRequest
resourceReadRequest c = (mkRequest (ptMethod targetSpec) (ptPath targetSpec))
  { hreqQuery = rrcQuery c
  , hreqBody = rrcBody c
  }
  where
    targetSpec = rrcTarget c

isRouteMiss :: HttpResponse -> Bool
isRouteMiss rsp =
  hresStatusCode rsp == 404
    && lookupNestedText ["error", "message"] (hresBody rsp) == Just "route not found"

installTerrainFixture :: HeadlessApp -> IO ()
installTerrainFixture app = do
  let handles = ccActorHandles (headlessCommandContext app)
      cfg = WorldConfig { wcChunkSize = 64 }
      chunkId = chunkIdFromCoord (ChunkCoord 0 0)
  setTerrainChunkData (ahDataHandle handles) (wcChunkSize cfg) [(chunkId, emptyTerrainChunk cfg)]
  terrainSnapshot <- getTerrainSnapshot (ahDataHandle handles)
  writeTerrainSnapshot (ahTerrainSnapshotRef handles) terrainSnapshot

slidersHaveTab :: Text -> Value -> Bool
slidersHaveTab expectedTab (Object obj) = case KM.lookup "sliders" obj of
  Just (Array sliders) ->
    let sliderValues = toList sliders
    in not (null sliderValues) && all (sliderHasTab expectedTab) sliderValues
  _ -> False
slidersHaveTab _ _ = False

sliderHasTab :: Text -> Value -> Bool
sliderHasTab expectedTab (Object slider) = KM.lookup "tab" slider == Just (String expectedTab)
sliderHasTab _ _ = False

assertTemplateResourceBodies :: HeadlessApp -> Expectation
assertTemplateResourceBodies app = do
  slidersByTab <- request app (mkRequest "GET" ["config", "sliders"])
    { hreqQuery = [("tab", Just "terrain")] }
  slidersHaveTab "terrain" (hresBody slidersByTab) `shouldBe` True

  sliderByName <- request app (mkRequest "POST" ["config", "sliders", "get"])
    { hreqBody = Just (object ["name" .= ("SliderGenScale" :: Text)]) }
  lookupText "name" (hresBody sliderByName) `shouldBe` Just "SliderGenScale"

  hexByCoords <- request app (mkRequest "GET" ["terrain", "hex"])
    { hreqQuery = [("q", Just "0"), ("r", Just "0")] }
  lookupValue "q" (hresBody hexByCoords) `shouldBe` Just (Number 0)
  lookupValue "r" (hresBody hexByCoords) `shouldBe` Just (Number 0)

  chunkById <- request app (mkRequest "GET" ["terrain", "chunk-summary"])
    { hreqQuery = [("chunk", Just "0")] }
  lookupValue "chunk" (hresBody chunkById) `shouldBe` Just (Number 0)

  enumsByType <- request app (mkRequest "GET" ["config", "enums"])
    { hreqQuery = [("type", Just "biome")] }
  objectHasKey "values" (hresBody enumsByType) `shouldBe` True

retiredMcpToolTargets :: [ParityTarget]
retiredMcpToolTargets =
  [ target "get_state" "GET" ["state"] "state.get" "get_state"
  , target "list_sliders" "GET" ["config", "sliders"] "config.sliders.list" "get_sliders"
  , target "get_slider" "POST" ["config", "sliders", "get"] "config.sliders.get" "get_slider"
  , target "set_slider" "POST" ["config", "sliders"] "config.sliders.set" "set_slider"
  , target "set_seed" "POST" ["ui", "seed"] "ui.seed.set" "set_seed"
  , target "set_view_mode" "POST" ["ui", "view-mode"] "ui.viewMode.set" "set_view_mode"
  , target "set_config_tab" "POST" ["ui", "config-tab"] "ui.configTab.set" "set_config_tab"
  , target "get_view_modes" "GET" ["state", "view-modes"] "state.viewModes" "get_view_modes"
  , target "generate" "POST" ["world", "generate"] "world.generate" "generate"
  , target "editor_toggle" "POST" ["editor", "toggle"] "editor.toggle" "editor_toggle"
  , target "editor_set_tool" "POST" ["editor", "tool"] "editor.tool.set" "editor_set_tool"
  , target "editor_set_brush" "PATCH" ["editor", "brush"] "editor.brush.set" "editor_set_brush"
  , target "editor_brush_stroke" "POST" ["editor", "brush-stroke"] "editor.brushStroke" "editor_brush_stroke"
  , target "editor_brush_line" "POST" ["editor", "brush-line"] "editor.brushLine" "editor_brush_line"
  , target "editor_set_biome" "POST" ["editor", "biome"] "editor.biome.set" "editor_set_biome"
  , target "editor_set_form" "POST" ["editor", "form"] "editor.form.set" "editor_set_form"
  , target "editor_set_hardness" "POST" ["editor", "hardness"] "editor.hardness.set" "editor_set_hardness"
  , target "editor_undo" "POST" ["editor", "undo"] "editor.undo" "editor_undo"
  , target "editor_redo" "POST" ["editor", "redo"] "editor.redo" "editor_redo"
  , target "editor_get_state" "GET" ["editor"] "editor.state" "editor_get_state"
  , target "get_enums" "GET" ["config", "enums"] "config.enums" "get_enums"
  , target "get_world_meta" "GET" ["world"] "world.meta" "get_world_meta"
  , target "get_generation_status" "GET" ["world", "generation-status"] "world.generationStatus" "get_generation_status"
  , target "inspect_hex" "GET" ["terrain", "hex"] "terrain.hex" "get_hex"
  , target "get_chunks" "GET" ["terrain", "chunks"] "terrain.chunks" "get_chunks"
  , target "get_chunk_summary" "GET" ["terrain", "chunk-summary"] "terrain.chunkSummary" "get_chunk_summary"
  , target "get_terrain_stats" "GET" ["terrain", "stats"] "terrain.stats" "get_terrain_stats"
  , target "get_overlays" "GET" ["overlays"] "overlays.list" "get_overlays"
  , target "list_worlds" "GET" ["worlds"] "worlds.list" "list_worlds"
  , target "set_sliders" "PATCH" ["config", "sliders"] "config.sliders.setMany" "set_sliders"
  , target "reset_sliders" "POST" ["config", "sliders", "reset"] "config.sliders.reset" "reset_sliders"
  , target "select_hex" "POST" ["ui", "select-hex"] "ui.hex.select" "select_hex"
  , target "save_world" "POST" ["worlds", "save"] "worlds.save" "save_world"
  , target "load_world" "POST" ["worlds", "load"] "worlds.load" "load_world"
  , target "list_presets" "GET" ["presets"] "presets.list" "list_presets"
  , target "save_preset" "POST" ["presets"] "presets.save" "save_preset"
  , target "load_preset" "POST" ["presets", "load"] "presets.load" "load_preset"
  , target "take_screenshot" "POST" ["screenshots"] "screenshots.take" "take_screenshot"
  , target "set_camera" "PUT" ["camera"] "camera.set" "set_camera"
  , target "get_camera" "GET" ["camera"] "camera.get" "get_camera"
  , target "zoom_to_chunk" "POST" ["camera", "zoom-to-chunk"] "camera.zoomToChunk" "zoom_to_chunk"
  , target "get_logs" "GET" ["logs"] "logs.get" "get_logs"
  , target "set_world_name" "PATCH" ["world", "name"] "world.name.set" "set_world_name"
  , target "get_pipeline" "GET" ["pipeline"] "pipeline.get" "get_pipeline"
  , target "set_stage_enabled" "PATCH" ["pipeline", "stages"] "pipeline.stage.setEnabled" "set_stage_enabled"
  , target "list_plugins" "GET" ["plugins"] "plugins.list" "list_plugins"
  , target "set_plugin_enabled" "PATCH" ["plugins", "enabled"] "plugins.setEnabled" "set_plugin_enabled"
  , target "set_plugin_param" "PATCH" ["plugins", "params"] "plugins.params.set" "set_plugin_param"
  , target "get_sim_state" "GET" ["simulation"] "simulation.state" "get_sim_state"
  , target "set_sim_auto_tick" "POST" ["simulation", "auto-tick"] "simulation.autoTick.set" "set_sim_auto_tick"
  , target "sim_tick" "POST" ["simulation", "tick"] "simulation.tick" "sim_tick"
  , target "get_config_summary" "GET" ["config", "summary"] "config.summary" "get_config_summary"
  , target "find_hexes" "POST" ["terrain", "search"] "terrain.search" "find_hexes"
  , target "export_terrain_data" "POST" ["terrain", "export"] "terrain.export" "export_terrain_data"
  , target "set_left_panel" "PUT" ["ui", "left-panel"] "ui.leftPanel.set" "set_left_panel"
  , target "set_left_tab" "PUT" ["ui", "left-tab"] "ui.leftTab.set" "set_left_tab"
  , target "toggle_config_panel" "POST" ["ui", "config-panel", "toggle"] "ui.configPanel.toggle" "toggle_config_panel"
  , target "set_log_collapsed" "PUT" ["ui", "log", "collapsed"] "ui.logCollapsed.set" "set_log_collapsed"
  , target "set_log_level" "PUT" ["ui", "log", "level"] "ui.logLevel.set" "set_log_level"
  , target "get_ui_panels" "GET" ["ui", "panels"] "ui.panels.get" "get_ui_panels"
  , target "set_overlay" "PUT" ["overlays", "current"] "overlays.current.set" "set_overlay"
  , target "list_overlay_fields" "GET" ["overlays", "fields"] "overlays.fields.list" "list_overlay_fields"
  , target "cycle_overlay" "POST" ["overlays", "cycle"] "overlays.cycle" "cycle_overlay"
  , target "cycle_overlay_field" "POST" ["overlays", "fields", "cycle"] "overlays.field.cycle" "cycle_overlay_field"
  , target "get_ui_state" "GET" ["ui", "state"] "ui.state" "get_ui_state"
  , target "data_list_plugins" "GET" ["data", "plugins"] "data.plugins.list" "data_list_plugins"
  , target "data_list_resources" "GET" ["data", "resources"] "data.resources.list" "data_list_resources"
  , target "data_list_records" "GET" ["data", "records"] "data.records.list" "data_list_records"
  , target "data_get_record" "POST" ["data", "records", "get"] "data.records.get" "data_get_record"
  , target "data_create_record" "POST" ["data", "records"] "data.records.create" "data_create_record"
  , target "data_update_record" "PUT" ["data", "records"] "data.records.update" "data_update_record"
  , target "data_delete_record" "DELETE" ["data", "records"] "data.records.delete" "data_delete_record"
  , target "data_get_state" "GET" ["data", "state"] "data.state" "data_get_state"
  , target "click_widget" "POST" ["ui", "widgets", "click"] "ui.widgets.click" "click_widget"
  , target "list_widgets" "GET" ["ui", "widgets"] "ui.widgets.list" "list_widgets"
  , target "get_widget_state" "GET" ["ui", "widget-state"] "ui.widgetState.get" "get_widget_state"
  , target "viewport_scroll" "POST" ["ui", "viewport", "scroll"] "ui.viewport.scroll" "viewport_scroll"
  , target "viewport_click" "POST" ["ui", "viewport", "click"] "ui.viewport.click" "viewport_click"
  , target "viewport_drag" "POST" ["ui", "viewport", "drag"] "ui.viewport.drag" "viewport_drag"
  , target "viewport_hover" "POST" ["ui", "viewport", "hover"] "ui.viewport.hover" "viewport_hover"
  , target "get_dialog_state" "GET" ["ui", "dialog"] "ui.dialog.get" "get_dialog_state"
  , target "set_dialog_text" "PUT" ["ui", "dialog", "text"] "ui.dialogText.set" "set_dialog_text"
  , target "dialog_confirm" "POST" ["ui", "dialog", "confirm"] "ui.dialog.confirm" "dialog_confirm"
  , target "dialog_cancel" "POST" ["ui", "dialog", "cancel"] "ui.dialog.cancel" "dialog_cancel"
  , target "send_key" "POST" ["ui", "key"] "ui.key.send" "send_key"
  ]

retiredMcpResourceTargets :: [ParityTarget]
retiredMcpResourceTargets =
  [ target "topo://state" "GET" ["state"] "state.get" "get_state"
  , target "topo://sliders" "GET" ["config", "sliders"] "config.sliders.list" "get_sliders"
  , target "topo://view-modes" "GET" ["state", "view-modes"] "state.viewModes" "get_view_modes"
  , target "topo://editor/state" "GET" ["editor"] "editor.state" "editor_get_state"
  , target "topo://world" "GET" ["world"] "world.meta" "get_world_meta"
  , target "topo://generation-status" "GET" ["world", "generation-status"] "world.generationStatus" "get_generation_status"
  , target "topo://chunks" "GET" ["terrain", "chunks"] "terrain.chunks" "get_chunks"
  , target "topo://terrain-stats" "GET" ["terrain", "stats"] "terrain.stats" "get_terrain_stats"
  , target "topo://overlays" "GET" ["overlays"] "overlays.list" "get_overlays"
  , target "topo://worlds" "GET" ["worlds"] "worlds.list" "list_worlds"
  , target "topo://presets" "GET" ["presets"] "presets.list" "list_presets"
  , target "topo://sliders/{tab}" "GET" ["config", "sliders"] "config.sliders.list" "get_sliders"
  , target "topo://slider/{name}" "POST" ["config", "sliders", "get"] "config.sliders.get" "get_slider"
  , target "topo://hex/{q}/{r}" "GET" ["terrain", "hex"] "terrain.hex" "get_hex"
  , target "topo://chunk/{id}" "GET" ["terrain", "chunk-summary"] "terrain.chunkSummary" "get_chunk_summary"
  , target "topo://enums/{type}" "GET" ["config", "enums"] "config.enums" "get_enums"
  ]

retiredMcpResourceReadCases :: [ResourceReadCase]
retiredMcpResourceReadCases =
  [ resourceReadCase (retiredMcpResourceTargets !! 0) [] Nothing
  , resourceReadCase (retiredMcpResourceTargets !! 1) [] Nothing
  , resourceReadCase (retiredMcpResourceTargets !! 2) [] Nothing
  , resourceReadCase (retiredMcpResourceTargets !! 3) [] Nothing
  , resourceReadCase (retiredMcpResourceTargets !! 4) [] Nothing
  , resourceReadCase (retiredMcpResourceTargets !! 5) [] Nothing
  , resourceReadCase (retiredMcpResourceTargets !! 6) [] Nothing
  , resourceReadCase (retiredMcpResourceTargets !! 7) [] Nothing
  , resourceReadCase (retiredMcpResourceTargets !! 8) [] Nothing
  , resourceReadCase (retiredMcpResourceTargets !! 9) [] Nothing
  , resourceReadCase (retiredMcpResourceTargets !! 10) [] Nothing
  , resourceReadCase (retiredMcpResourceTargets !! 11) [("tab", Just "terrain")] Nothing
  , resourceReadCase (retiredMcpResourceTargets !! 12) [] (Just (object ["name" .= ("SliderGenScale" :: Text)]))
  , resourceReadCase (retiredMcpResourceTargets !! 13) [("q", Just "0"), ("r", Just "0")] Nothing
  , resourceReadCase (retiredMcpResourceTargets !! 14) [("chunk", Just "0")] Nothing
  , resourceReadCase (retiredMcpResourceTargets !! 15) [("type", Just "biome")] Nothing
  ]

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

eventsContainTopic :: Text -> Value -> Bool
eventsContainTopic expectedTopic (Object obj) = case KM.lookup "events" obj of
  Just (Array events) -> any (eventHasTopic expectedTopic) (toList events)
  _ -> False
eventsContainTopic _ _ = False

eventHasTopic :: Text -> Value -> Bool
eventHasTopic expectedTopic (Object event) =
  KM.lookup "topic" event == Just (String expectedTopic)
eventHasTopic _ _ = False

eventPayloadHasKey :: Text -> Text -> Value -> Bool
eventPayloadHasKey expectedTopic key (Object obj) = case KM.lookup "events" obj of
  Just (Array events) -> case find (eventHasTopic expectedTopic) (toList events) of
    Just (Object event) -> case KM.lookup "payload" event of
      Just (Object payload) -> KM.member (Key.fromText key) payload
      _ -> False
    _ -> False
  _ -> False
eventPayloadHasKey _ _ _ = False

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

operationResponseContentTypes :: Value -> Text -> Text -> Text -> Maybe [Text]
operationResponseContentTypes doc path routeMethod status = do
  Object operation <- pathOperation doc path routeMethod
  Object responses <- KM.lookup "responses" operation
  Object response <- KM.lookup (Key.fromText status) responses
  Object content <- KM.lookup "content" response
  pure (map Key.toText (KM.keys content))

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

assertEventStream :: Manager -> Text -> IO ()
assertEventStream manager expectedTopic = do
  req0 <- parseRequest "http://127.0.0.1:7375/events?stream=true&limit=1"
  let req = req0 { requestHeaders = [("Accept", "text/event-stream"), ("X-Request-Id", "evt-123")] }
  rsp <- httpLbs req manager
  HTTP.statusCode (responseStatus rsp) `shouldBe` 200
  lookup "X-Request-Id" (responseHeaders rsp) `shouldBe` Just "evt-123"
  let body = TextEncoding.decodeUtf8 (LBS.toStrict (responseBody rsp))
  body `shouldSatisfy` Text.isInfixOf ("event: " <> expectedTopic)
  body `shouldSatisfy` Text.isInfixOf "data:"

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
