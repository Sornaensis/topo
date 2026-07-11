{-# LANGUAGE OverloadedStrings #-}

module Spec.HTTP (spec) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (SomeException, bracket, finally, throwIO, try)
import Control.Monad (forM_)
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (toList)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.List (find, nub, sort)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as TextIO
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Actor.Data (getTerrainSnapshot, setOverlayStoreData, setTerrainChunkData)
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
  , HeadlessConfig(..)
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
import Seer.Service.AppService
  ( AppService(..)
  , DataResourceService(..)
  , appServiceOperationMethods
  , dataResourceCreateRecordOperation
  )
import Seer.Service.Types (ServiceError(..), rawServiceHandler)
import Seer.System (runApp)
import Spec.Support.OverlayFixtures (mkSparseFloatOverlay)
import System.Directory
  ( createDirectory
  , createDirectoryIfMissing
  , doesFileExist
  , getCurrentDirectory
  , getTemporaryDirectory
  , removeDirectoryRecursive
  )
import System.Environment (lookupEnv, setEnv, unsetEnv, withArgs)
import System.FilePath ((</>), takeDirectory)
import Topo (WorldConfig(..), chunkIdFromCoord, emptyTerrainChunk)
import Topo.Overlay (emptyOverlayStore, insertOverlay, OverlayProvenance(..))
import Topo.Plugin.RPC.DataService (DataResourceErrorCode(..), dataResourceErrorCodeText)
import Topo.Plugin.RPC.Manifest (RPCParamSpec(..), RPCParamType(..), manifestV3)
import Topo.Plugin.RPC.Protocol (currentProtocolVersion)
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

      version <- request app (mkRequest "GET" ["version"])
      hresStatusCode version `shouldBe` 200
      lookupText "name" (hresBody version) `shouldBe` Just "topo-seer"
      lookupText "version" (hresBody version) `shouldBe` Just "1.0.0.0"
      lookupText "api_version" (hresBody version) `shouldBe` Just "1"

      openapi <- request app (mkRequest "GET" ["openapi.json"])
      hresStatusCode openapi `shouldBe` 200
      lookupText "openapi" (hresBody openapi) `shouldBe` Just "3.0.3"
      pathMethods (hresBody openapi) "/commands/get_state" `shouldBe` Nothing

      state <- request app (mkRequest "GET" ["state"])
      hresStatusCode state `shouldBe` 200
      objectHasKey "seed" (hresBody state) `shouldBe` True

      pipeline <- request app (mkRequest "GET" ["pipeline"])
      hresStatusCode pipeline `shouldBe` 200
      objectHasKey "stages" (hresBody pipeline) `shouldBe` True
      objectHasKey "dag" (hresBody pipeline) `shouldBe` True
      objectHasKey "docs" (hresBody pipeline) `shouldBe` True
      pipelineStagesExposeDiagnostics (hresBody pipeline) `shouldBe` True

      dag <- request app (mkRequest "GET" ["simulation", "dag"])
      hresStatusCode dag `shouldBe` 200
      objectHasKey "available" (hresBody dag) `shouldBe` True
      objectHasKey "world_bound" (hresBody dag) `shouldBe` True
      objectHasKey "overlay_names" (hresBody dag) `shouldBe` True
      objectHasKey "tick_logs" (hresBody dag) `shouldBe` True
      objectHasKey "weather_node_status" (hresBody dag) `shouldBe` True
      objectHasKey "last_weather_publication" (hresBody dag) `shouldBe` True
      objectHasKey "cloud_delta" (hresBody dag) `shouldBe` True
      objectHasKey "plugin_nodes" (hresBody dag) `shouldBe` True
      objectHasKey "plugin_declarations" (hresBody dag) `shouldBe` True
      objectHasKey "plugin_simulation_declarations" (hresBody dag) `shouldBe` True

      pluginStatus <- request app (mkRequest "GET" ["plugins", "status"])
      hresStatusCode pluginStatus `shouldBe` 200
      objectHasKey "plugins" (hresBody pluginStatus) `shouldBe` True
      pluginsExposeSurfaceKeys (hresBody pluginStatus) `shouldBe` True

      screenshot <- request app (mkRequest "POST" ["screenshots"])
      hresStatusCode screenshot `shouldBe` 200
      lookupText "format" (hresBody screenshot) `shouldBe` Just "png"
      lookupText "source" (hresBody screenshot) `shouldBe` Just "headless"
      lookupText "image_base64" (hresBody screenshot) `shouldSatisfy` maybe False (not . Text.null)

  it "serves layered view state and legacy compatibility routes in headless mode" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      state0 <- request app (mkRequest "GET" ["state"])
      hresStatusCode state0 `shouldBe` 200
      lookupNestedText ["view", "base_mode"] (hresBody state0) `shouldBe` Just "elevation"
      lookupNestedValue ["view", "overlay_mode"] (hresBody state0) `shouldBe` Just Null

      viewModes <- request app (mkRequest "GET" ["state", "view-modes"])
      hresStatusCode viewModes `shouldBe` 200
      objectHasKey "view_modes" (hresBody viewModes) `shouldBe` True
      lookupNestedText ["view", "weather_basis"] (hresBody viewModes) `shouldBe` Just "current"

      views0 <- request app (mkRequest "GET" ["state", "views"])
      hresStatusCode views0 `shouldBe` 200
      map (`objectHasKey` hresBody views0)
        [ "view", "base_modes", "overlay_modes", "weather_bases", "overlay_names", "legacy_modes" ]
        `shouldBe` replicate 6 True
      lookupText "legacy_view_mode" (hresBody views0) `shouldBe` Just "elevation"

      legacy <- request app (mkRequest "POST" ["ui", "view-mode"])
        { hreqBody = Just (object ["mode" .= ("cloud" :: Text), "basis" .= ("typical" :: Text)]) }
      hresStatusCode legacy `shouldBe` 200
      lookupText "view_mode" (hresBody legacy) `shouldBe` Just "cloud_typical"
      lookupNestedText ["view", "overlay_mode"] (hresBody legacy) `shouldBe` Just "cloud"
      lookupNestedText ["view", "weather_basis"] (hresBody legacy) `shouldBe` Just "average"
      lookupNestedText ["view", "temporal_basis"] (hresBody legacy) `shouldBe` Just "typical_normal"

      layered <- request app (mkRequest "POST" ["ui", "view"])
        { hreqBody = Just (object
            [ "base_mode" .= ("biome" :: Text)
            , "overlay_mode" .= ("cloud" :: Text)
            , "weather_basis" .= ("current" :: Text)
            , "overlay_opacity" .= (0.25 :: Double)
            ]) }
      hresStatusCode layered `shouldBe` 200
      lookupText "view_mode" (hresBody layered) `shouldBe` Just "cloud"
      lookupNestedText ["view", "base_mode"] (hresBody layered) `shouldBe` Just "biome"
      lookupNestedText ["view", "overlay_mode"] (hresBody layered) `shouldBe` Just "cloud"
      lookupNestedText ["view", "weather_basis"] (hresBody layered) `shouldBe` Just "current"
      lookupNestedValue ["view", "overlay_opacity"] (hresBody layered) `shouldBe` Just (Number 0.25)

      uiState <- request app (mkRequest "GET" ["ui", "state"])
      hresStatusCode uiState `shouldBe` 200
      lookupNestedText ["view", "base_mode"] (hresBody uiState) `shouldBe` Just "biome"
      lookupNestedText ["view", "selection", "base_mode"] (hresBody uiState) `shouldBe` Just "biome"
      lookupNestedText ["view", "overlay_mode"] (hresBody uiState) `shouldBe` Just "cloud"
      lookupNestedValue ["view", "overlay_opacity"] (hresBody uiState) `shouldBe` Just (Number 0.25)

      state1 <- request app (mkRequest "GET" ["state"])
      hresStatusCode state1 `shouldBe` 200
      lookupText "view_mode" (hresBody state1) `shouldBe` Just "cloud"
      lookupNestedText ["view", "base_mode"] (hresBody state1) `shouldBe` Just "biome"

      clear <- request app (mkRequest "POST" ["ui", "view-mode"])
        { hreqBody = Just (object ["mode" .= ("elevation" :: Text)]) }
      hresStatusCode clear `shouldBe` 200
      lookupText "view_mode" (hresBody clear) `shouldBe` Just "elevation"
      lookupNestedText ["view", "base_mode"] (hresBody clear) `shouldBe` Just "elevation"
      lookupNestedValue ["view", "overlay_mode"] (hresBody clear) `shouldBe` Just Null

  it "keeps headless screenshot responses stable while a layered view is active" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      setView <- request app (mkRequest "POST" ["ui", "view"])
        { hreqBody = Just (object
            [ "base_mode" .= ("biome" :: Text)
            , "overlay_mode" .= ("cloud" :: Text)
            , "weather_basis" .= ("current" :: Text)
            , "overlay_opacity" .= (0.5 :: Double)
            ]) }
      hresStatusCode setView `shouldBe` 200

      before <- request app (mkRequest "GET" ["ui", "state"])
      screenshot <- request app (mkRequest "POST" ["screenshots"])
      after <- request app (mkRequest "GET" ["ui", "state"])
      hresStatusCode screenshot `shouldBe` 200
      map (`objectHasKey` hresBody screenshot) ["image_base64", "format", "source"]
        `shouldBe` replicate 3 True
      objectHasKey "view" (hresBody screenshot) `shouldBe` False
      lookupText "format" (hresBody screenshot) `shouldBe` Just "png"
      lookupText "source" (hresBody screenshot) `shouldBe` Just "headless"
      lookupText "image_base64" (hresBody screenshot) `shouldSatisfy` maybe False (not . Text.null)
      lookupValue "view" (hresBody after) `shouldBe` lookupValue "view" (hresBody before)

  it "buffers HTTP service events and serves them as SSE" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      seedSet <- request app (mkRequest "POST" ["ui", "seed"])
        { hreqBody = Just (object ["seed" .= (123 :: Int)]) }
      hresStatusCode seedSet `shouldBe` 200

      generationStatus <- request app (mkRequest "GET" ["world", "generation-status"])
      hresStatusCode generationStatus `shouldBe` 200

      simState <- request app (mkRequest "GET" ["simulation"])
      hresStatusCode simState `shouldBe` 200

      pluginStatus <- request app (mkRequest "GET" ["plugins", "status"])
      hresStatusCode pluginStatus `shouldBe` 200

      dataState <- request app (mkRequest "GET" ["data", "state"])
      hresStatusCode dataState `shouldBe` 200

      events <- request app (mkRequest "GET" ["events"])
      hresStatusCode events `shouldBe` 200
      lookupText "mode" (hresBody events) `shouldBe` Just "polling"
      eventsContainTopic "ui.state.changed" (hresBody events) `shouldBe` True
      eventsContainTopic "world.generation.status" (hresBody events) `shouldBe` True
      eventsContainTopic "simulation.status" (hresBody events) `shouldBe` True
      eventsContainTopic "plugins.status" (hresBody events) `shouldBe` True
      eventsContainTopic "data.resources.status" (hresBody events) `shouldBe` True
      eventPayloadHasKey "ui.state.changed" "result" (hresBody events) `shouldBe` True
      eventPayloadHasKey "ui.state.changed" "body" (hresBody events) `shouldBe` False
      eventPayloadResultHasKey "plugins.status" "external_data_sources" (hresBody events) `shouldBe` True
      eventPayloadResultHasKey "data.resources.status" "external_data_sources" (hresBody events) `shouldBe` True

      let cfg = defaultHttpServerConfig { hscBindPort = 7375 }
      tid <- forkHttpServer cfg headlessHttpAppService (headlessServiceContext app)
      manager <- newManager defaultManagerSettings
      eventually_ (assertEventStream manager "ui.state.changed")
        `finally` (do
          killThread tid
          threadDelay 100000)

  it "smoke-tests world generation, save, and load through HTTP" $
    withHttpSmokeTempHome $ \home ->
      withTemporaryTopoHome home $ do
        let smokeName = "smoke-1-0" :: Text
            smokeSeed = 424242 :: Int

        generatedChunkCount <- withHeadlessApp defaultHeadlessConfig $ \app -> do
          setSeed <- request app (mkRequest "POST" ["ui", "seed"])
            { hreqBody = Just (object ["seed" .= smokeSeed]) }
          hresStatusCode setSeed `shouldBe` 200
          lookupValue "seed" (hresBody setSeed) `shouldBe` Just (Number 424242)

          forM_ ([1..7] :: [Int]) $ \_ -> do
            chunkDown <- request app (mkRequest "POST" ["ui", "widgets", "click"])
              { hreqBody = Just (object ["widget_id" .= ("WidgetChunkMinus" :: Text)]) }
            hresStatusCode chunkDown `shouldBe` 200

          setExtents <- request app (mkRequest "PATCH" ["config", "sliders"])
            { hreqBody = Just (object ["values" .= object
                [ "SliderExtentX" .= (0.0625 :: Double)
                , "SliderExtentY" .= (0.0625 :: Double)
                ]]) }
          hresStatusCode setExtents `shouldBe` 200
          objectHasKey "updated" (hresBody setExtents) `shouldBe` True

          generate <- request app (mkRequest "POST" ["world", "generate"])
          hresStatusCode generate `shouldBe` 200
          lookupText "status" (hresBody generate) `shouldBe` Just "generating"

          generatedStatus <- waitForGeneratedWorld app
          lookupValue "generating" (hresBody generatedStatus) `shouldBe` Just (Bool False)
          lookupValue "chunk_count" (hresBody generatedStatus)
            `shouldSatisfy` maybe False positiveNumber
          lookupValue "seed" (hresBody generatedStatus) `shouldBe` Just (Number 424242)

          generatedMeta <- request app (mkRequest "GET" ["world"])
          hresStatusCode generatedMeta `shouldBe` 200
          let generatedChunkCount = lookupValue "chunk_count" (hresBody generatedMeta)
          generatedChunkCount `shouldBe` lookupValue "chunk_count" (hresBody generatedStatus)
          generatedChunkCount `shouldSatisfy` maybe False positiveNumber
          lookupValue "seed" (hresBody generatedMeta) `shouldBe` Just (Number 424242)
          arrayFieldContainsText "overlay_names" "weather" (hresBody generatedMeta) `shouldBe` True

          assertWeatherOverlayAvailable app
          assertBackendNeutralPluginDataSurfaces app

          events <- request app (mkRequest "GET" ["events"])
          hresStatusCode events `shouldBe` 200
          eventsContainTopic "world.generation.requested" (hresBody events) `shouldBe` True
          eventsContainTopic "world.generation.status" (hresBody events) `shouldBe` True

          save <- request app (mkRequest "POST" ["worlds", "save"])
            { hreqBody = Just (object ["name" .= smokeName]) }
          hresStatusCode save `shouldBe` 200
          lookupText "name" (hresBody save) `shouldBe` Just smokeName
          lookupValue "saved" (hresBody save) `shouldBe` Just (Bool True)
          arrayFieldContainsText "formats" "world.topo" (hresBody save) `shouldBe` True
          arrayFieldContainsText "formats" "world.topolay" (hresBody save) `shouldBe` True

          worlds <- request app (mkRequest "GET" ["worlds"])
          hresStatusCode worlds `shouldBe` 200
          worldListContains smokeName (hresBody worlds) `shouldBe` True
          pure generatedChunkCount

        withHeadlessApp defaultHeadlessConfig $ \loadApp -> do
          load <- request loadApp (mkRequest "POST" ["worlds", "load"])
            { hreqBody = Just (object ["name" .= smokeName]) }
          hresStatusCode load `shouldBe` 200
          lookupText "name" (hresBody load) `shouldBe` Just smokeName
          lookupValue "loaded" (hresBody load) `shouldBe` Just (Bool True)
          arrayFieldContainsText "formats" "world.topo" (hresBody load) `shouldBe` True
          arrayFieldContainsText "formats" "world.topolay" (hresBody load) `shouldBe` True
          arrayFieldContainsText "overlay_names" "weather" (hresBody load) `shouldBe` True

          loadedMeta <- waitForWorldName loadApp smokeName
          hresStatusCode loadedMeta `shouldBe` 200
          lookupText "world_name" (hresBody loadedMeta) `shouldBe` Just smokeName
          lookupValue "chunk_count" (hresBody loadedMeta) `shouldBe` generatedChunkCount
          lookupValue "seed" (hresBody loadedMeta) `shouldBe` Just (Number 424242)
          arrayFieldContainsText "overlay_names" "weather" (hresBody loadedMeta) `shouldBe` True

          assertWeatherOverlayAvailable loadApp
          assertBackendNeutralPluginDataSurfaces loadApp

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

  it "enforces route body policy matrices in direct and WAI request paths" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      installTerrainFixture app
      let directCases :: [(Text, HttpRequest, BodyPolicyExpectation)]
          directCases =
            [ ( "no-body-empty"
              , mkRequest "GET" ["state"]
              , ExpectBodyPolicySuccess
              )
            , ( "no-body-object"
              , (mkRequest "GET" ["state"]) { hreqBody = Just (object []) }
              , ExpectBodyPolicyError 400 "validation_failed" "unexpected_body"
              )
            , ( "no-body-non-object"
              , (mkRequest "GET" ["state"]) { hreqBody = Just (String "not-object") }
              , ExpectBodyPolicyError 400 "validation_failed" "unexpected_body"
              )
            , ( "no-body-null"
              , (mkRequest "GET" ["state"]) { hreqBody = Just Null }
              , ExpectBodyPolicyError 400 "validation_failed" "unexpected_body"
              )
            , ( "optional-empty"
              , mkRequest "POST" ["screenshots"]
              , ExpectBodyPolicySuccess
              )
            , ( "optional-object"
              , (mkRequest "POST" ["screenshots"]) { hreqBody = Just (object []) }
              , ExpectBodyPolicySuccess
              )
            , ( "optional-non-object"
              , (mkRequest "POST" ["screenshots"]) { hreqBody = Just (String "not-object") }
              , ExpectBodyPolicyError 400 "validation_failed" "invalid_body"
              )
            , ( "optional-null"
              , (mkRequest "POST" ["screenshots"]) { hreqBody = Just Null }
              , ExpectBodyPolicyError 400 "validation_failed" "invalid_body"
              )
            , ( "required-empty"
              , mkRequest "POST" ["ui", "seed"]
              , ExpectBodyPolicyError 400 "validation_failed" "missing_body"
              )
            , ( "required-object"
              , (mkRequest "POST" ["ui", "seed"]) { hreqBody = Just (object ["seed" .= (321 :: Int)]) }
              , ExpectBodyPolicySuccess
              )
            , ( "required-non-object"
              , (mkRequest "POST" ["ui", "seed"]) { hreqBody = Just (String "not-object") }
              , ExpectBodyPolicyError 400 "validation_failed" "invalid_body"
              )
            , ( "required-null"
              , (mkRequest "POST" ["ui", "seed"]) { hreqBody = Just Null }
              , ExpectBodyPolicyError 400 "validation_failed" "invalid_body"
              )
            ]
      forM_ directCases (assertDirectBodyPolicyCase app)
      assertDirectNoBodyQueryParamsUseQuery app

      let cfg = defaultHttpServerConfig { hscBindPort = 7376 }
      tid <- forkHttpServer cfg headlessHttpAppService (headlessServiceContext app)
      manager <- newManager defaultManagerSettings
      eventually_ (assertWaiRouteBodyPolicyMatrix manager)
        `finally` (do
          killThread tid
          threadDelay 100000)

  it "maps plugin parameter validation and not-found errors to HTTP envelopes" $
    withHttpPluginDir $ do
      let cfg = defaultHeadlessConfig { hcDiscoverPlugins = True }
      withHeadlessApp cfg $ \app -> do
        wrongType <- request app (mkRequest "PATCH" ["plugins", "params"])
          { hreqBody = Just (object
              [ "plugin" .= ("http-example" :: Text)
              , "param" .= ("enabled" :: Text)
              , "value" .= String "yes"
              ]) }
        hresStatusCode wrongType `shouldBe` 400
        lookupNestedText ["error", "code"] (hresBody wrongType) `shouldBe` Just "validation_failed"
        errorDetailPath (hresBody wrongType) `shouldBe` Just ["value"]

        unknownParam <- request app (mkRequest "PATCH" ["plugins", "params"])
          { hreqBody = Just (object
              [ "plugin" .= ("http-example" :: Text)
              , "param" .= ("missing" :: Text)
              , "value" .= Bool True
              ]) }
        hresStatusCode unknownParam `shouldBe` 400
        lookupNestedText ["error", "code"] (hresBody unknownParam) `shouldBe` Just "validation_failed"
        errorDetailPath (hresBody unknownParam) `shouldBe` Just ["param"]

        unknownPlugin <- request app (mkRequest "PATCH" ["plugins", "params"])
          { hreqBody = Just (object
              [ "plugin" .= ("missing" :: Text)
              , "param" .= ("enabled" :: Text)
              , "value" .= Bool True
              ]) }
        hresStatusCode unknownPlugin `shouldBe` 404
        lookupNestedText ["error", "code"] (hresBody unknownPlugin) `shouldBe` Just "not_found"

  it "maps standardized data-resource service errors to HTTP API envelopes" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      let cases =
            [ (SchemaValidationFailed, 422)
            , (PermissionDenied, 403)
            , (OperationNotSupported, 405)
            , (DuplicateKey, 409)
            , (Conflict, 409)
            , (PluginUnavailable, 503)
            , (DataResourceTimeout, 504)
            ]
          requestBody = object
            [ "plugin" .= ("fixture" :: Text)
            , "resource" .= ("records" :: Text)
            , "fields" .= object []
            ]
          appFor code = headlessHttpAppService
            { appDataResources = (appDataResources headlessHttpAppService)
                { dataCreateRecord = rawServiceHandler dataResourceCreateRecordOperation $ \_ _ ->
                    pure (Left (ServiceDataResourceError code "failed" []))
                }
            }
      forM_ cases $ \(code, status) -> do
        rsp <- handleHttpRequest defaultHttpServerConfig (appFor code) (headlessServiceContext app) $
          (mkRequest "POST" ["data", "records"]) { hreqBody = Just requestBody }
        hresStatusCode rsp `shouldBe` status
        lookupNestedText ["error", "code"] (hresBody rsp) `shouldBe` Just (dataResourceErrorCodeText code)

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
          [ ("/state", "get", "AppStateResponse")
          , ("/state/view-modes", "get", "StateViewModesResponse")
          , ("/state/views", "get", "StateViewsResponse")
          , ("/ui/state", "get", "UiStateResponse")
          , ("/presets", "get", "PresetsListResponse")
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
          , ("/ui/view-mode", "post", "UiViewModeSetResponse")
          , ("/ui/view", "post", "UiViewSetResponse")
          , ("/terrain/hex", "get", "TerrainHexResponse")
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
          , ("/ui/view-mode", "post", "UiViewModeSetRequest")
          , ("/ui/view", "post", "UiViewSetRequest")
          ] :: [(Text, Text, Text)]
    forM_ responseRefs $ \(path, routeMethod, schemaName) -> do
      operationResponseSchemaRef doc path routeMethod "200" `shouldBe` Just schemaName
      schemaComponentNames doc `shouldSatisfy` elem schemaName
    forM_ requestRefs $ \(path, routeMethod, schemaName) -> do
      operationRequestSchemaRef doc path routeMethod `shouldBe` Just schemaName
      schemaComponentNames doc `shouldSatisfy` elem schemaName
    componentRequiredFields doc "PipelineSetStageEnabledRequest" `shouldBe` Just ["stage", "enabled"]
    componentRequiredFields doc "DataRecordUpdateRequest" `shouldBe` Just ["plugin", "resource", "key", "fields"]
    componentRequiredFields doc "SimulationDagResponse" `shouldSatisfy` maybe False (\actual -> all (`elem` actual) ["available", "world_bound", "overlay_names", "nodes", "levels", "terrain_writers"])
    componentPropertyNames doc "SimulationDagResponse" `shouldSatisfy` maybe False (\actual -> all (`elem` actual) ["plugin_nodes", "plugin_node_count", "plugin_declarations", "plugin_declaration_count", "plugin_simulation_declarations", "plugin_simulation_declaration_count"])
    componentPropertyDescription doc "SimulationDagResponse" "nodes" `shouldSatisfy` maybe False (Text.isInfixOf "Actor-bound simulation DAG nodes")
    componentPropertyNullable doc "DataRecordsListResponse" "total_count" `shouldBe` Just True
    sort <$> componentPropertyNames doc "ScreenshotTakeResponse"
      `shouldBe` Just ["format", "image_base64", "saved_path", "source"]
    componentPropertyNames doc "StateViewsResponse"
      `shouldSatisfy` maybe False (\actual -> all (`elem` actual) ["view", "base_modes", "overlay_modes", "weather_bases", "overlay_names", "legacy_modes"])
    let layeredViewProps = inlinePropertyNames =<< componentProperty doc "StateViewsResponse" "view"
        uiStateViewProps = inlinePropertyNames =<< componentProperty doc "UiStateResponse" "view"
    layeredViewProps `shouldSatisfy` maybe False (\actual -> all (`elem` actual) ["base", "base_mode", "overlay", "overlay_mode", "plugin_overlay", "overlay_field", "weather_basis", "temporal_basis", "source_kind", "overlay_opacity", "legacy_view_mode"])
    uiStateViewProps `shouldSatisfy` maybe False (\actual -> all (`elem` actual) ["mode", "base_mode", "overlay_mode", "plugin_overlay", "weather_basis", "overlay_opacity", "legacy_view_mode", "temporal_basis", "source_kind", "selection", "overlay_names"])
    componentPropertyNames doc "UiViewSetRequest"
      `shouldSatisfy` maybe False (\actual -> all (`elem` actual) ["base_mode", "base", "overlay_mode", "overlay", "plugin_overlay", "weather_basis", "basis", "temporal_basis", "overlay_opacity", "field_index", "overlay_field"])
    componentPropertyNames doc "UiViewSetResponse"
      `shouldSatisfy` maybe False (\actual -> all (`elem` actual) ["view", "view_mode", "base_mode", "overlay_mode", "plugin_overlay", "overlay_field", "weather_basis", "overlay_opacity", "legacy_view_mode"])
    componentRequiredFields doc "TerrainHexResponse" `shouldSatisfy` maybe False ("soil" `elem`)
    componentRequiredFields doc "TerrainHexResponse" `shouldSatisfy` maybe False ("ocean_currents" `elem`)
    componentRequiredFields doc "TerrainHexResponse" `shouldSatisfy` maybe False ("weather_snapshot" `elem`)
    componentRequiredFields doc "TerrainHexResponse" `shouldSatisfy` maybe False ("weather_normals" `elem`)
    componentRequiredFields doc "TerrainHexResponse" `shouldSatisfy` maybe False ("weather_timeline" `elem`)
    componentRequiredFields doc "TerrainHexResponse" `shouldSatisfy` maybe False ("water_bodies" `elem`)
    componentRequiredFields doc "TerrainHexResponse" `shouldSatisfy` maybe False ("glacier_snow_ice" `elem`)
    componentRequiredFields doc "TerrainHexResponse" `shouldSatisfy` maybe False ("active_view" `elem`)
    let activeViewProps = inlinePropertyNames =<< componentProperty doc "TerrainHexResponse" "active_view"
        terrainProps = inlinePropertyNames =<< componentProperty doc "TerrainHexResponse" "terrain"
        climateProps = inlinePropertyNames =<< componentProperty doc "TerrainHexResponse" "climate"
        weatherProps = inlinePropertyNames =<< componentProperty doc "TerrainHexResponse" "weather"
        weatherNormalsProps = inlinePropertyNames =<< componentProperty doc "TerrainHexResponse" "weather_normals"
    activeViewProps `shouldSatisfy` maybe False (\actual -> all (`elem` actual) ["basis", "color_scale", "export_fields", "inspector_fields", "label", "mode", "source_kind", "temporal_basis", "tooltip_fields", "unit", "values"])
    terrainProps `shouldSatisfy` maybe False (\actual -> all (`elem` actual) ["plate_boundary", "plate_boundary_code", "plate_crust", "plate_crust_code"])
    climateProps `shouldSatisfy` maybe False (\actual -> all (`elem` actual) ["basis", "source_kind", "temporal_basis", "temp_avg", "precip_avg"])
    weatherProps `shouldSatisfy` maybe False (\actual -> all (`elem` actual) ["basis", "source_kind", "temporal_basis", "temp_current", "precip_current", "cloud_cover_current"])
    weatherNormalsProps `shouldSatisfy` maybe False (\actual -> all (`elem` actual) ["basis", "source_kind", "temporal_basis", "temp_typical", "precip_typical", "cloud_cover_typical"])
    componentPropertyNames doc "TerrainExportResponse" `shouldSatisfy` maybe False ("available_fields" `elem`)
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

  it "matches the published OpenAPI artifact" $ do
    mPath <- publishedOpenApiPath
    case mPath of
      Nothing -> pendingWith "repository docs/api/openapi.json is not present in this package-only test run"
      Just path -> do
        published <- LBS.readFile path
        published `shouldBe` Aeson.encode (openApiDocument publicHttpRouteSpecs)

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
    operationRequestBodyRequired doc "/editor/brush" "patch" `shouldBe` Just (Just True)
    operationRequestBodyRequired doc "/overlays/import/validate" "post" `shouldBe` Just (Just True)
    operationRequestSchemaRef doc "/overlays/import/validate" "post" `shouldBe` Just "OverlayImportValidateRequest"
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

  it "documents tags, examples, errors, and versioning in OpenAPI" $ do
    let doc = openApiDocument publicHttpRouteSpecs
    lookupNestedText ["info", "version"] doc `shouldBe` Just "1.0.0"
    lookupNestedText ["info", "x-topo-api-version"] doc `shouldBe` Just "1"
    sort (openApiTags doc) `shouldBe` sort (nub (map hrsTag publicHttpRouteSpecs))
    operationRequestExample doc "/ui/seed" "post" `shouldBe` Just (object ["seed" .= (123 :: Int)])
    operationResponseExample doc "/health" "get" "200" `shouldBe` Just (object ["status" .= ("ok" :: Text)])
    operationResponseErrorCode doc "/data/records" "post" "403" `shouldBe` Just "permission_denied"
    operationResponseErrorCode doc "/data/records" "post" "405" `shouldBe` Just "operation_not_supported"
    operationResponseErrorCode doc "/data/records" "post" "422" `shouldBe` Just "schema_validation_failed"
    operationResponseErrorCode doc "/data/records" "post" "504" `shouldBe` Just "timeout"
    operationResponseStatuses doc "/data/records" "post"
      `shouldSatisfy` maybe False (\statuses -> all (`elem` statuses) ["400", "401", "403", "404", "405", "409", "422", "500", "503", "504"])
    errorCodeEnum doc
      `shouldSatisfy` maybe False (\codes -> all (`elem` codes) ["validation_failed", "schema_validation_failed", "permission_denied", "operation_not_supported", "timeout"])
    errorCodeEnum doc `shouldSatisfy` maybe False (notElem "invalid_json")

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

      setWeatherView <- request app (mkRequest "POST" ["ui", "view-mode"])
        { hreqBody = Just (object ["mode" .= ("weather" :: Text)]) }
      hresStatusCode setWeatherView `shouldBe` 200

      hexRsp <- request app (mkRequest "GET" ["terrain", "hex"])
        { hreqQuery = [("q", Just "0"), ("r", Just "0")] }
      hresStatusCode hexRsp `shouldBe` 200
      lookupValue "q" (hresBody hexRsp) `shouldBe` Just (Number 0)
      lookupValue "r" (hresBody hexRsp) `shouldBe` Just (Number 0)
      map (`objectHasKey` hresBody hexRsp)
        [ "hypsometry"
        , "terrain_form_metrics"
        , "hydrology"
        , "soil"
        , "biome_refinement"
        , "climate_diagnostics"
        , "weather_snapshot"
        , "weather_timeline"
        , "water_bodies"
        , "glacier_snow_ice"
        , "ocean_currents"
        , "units"
        ] `shouldBe` replicate 12 True
      lookupNestedText ["active_view", "temporal_basis"] (hresBody hexRsp) `shouldBe` Just "instantaneous_current"
      lookupNestedText ["active_view", "source_kind"] (hresBody hexRsp) `shouldBe` Just "weather_snapshot"
      lookupNestedText ["weather_timeline", "basis"] (hresBody hexRsp) `shouldBe` Just "instantaneous_current"
      lookupNestedText ["weather_timeline", "temporal_basis"] (hresBody hexRsp) `shouldBe` Just "instantaneous_current"
      lookupNestedText ["weather_timeline", "source_kind"] (hresBody hexRsp) `shouldBe` Just "weather_snapshot"
      fmap (objectHasKey "published_weather_version") (lookupValue "weather_timeline" (hresBody hexRsp)) `shouldBe` Just True
      lookupNestedText ["climate_diagnostics", "temporal_basis"] (hresBody hexRsp) `shouldBe` Just "long_run_average"
      lookupNestedText ["climate_diagnostics", "source_kind"] (hresBody hexRsp) `shouldBe` Just "climate_average"

  it "serves overlay manager, schema/provenance, import validation, mesh, and sample export routes" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      installTerrainFixture app
      installOverlayFixture app

      setPluginView <- request app (mkRequest "POST" ["ui", "view"])
        { hreqBody = Just (object
            [ "overlay_mode" .= ("plugin" :: Text)
            , "plugin_overlay" .= ("weather" :: Text)
            , "overlay_field" .= (0 :: Int)
            , "overlay_opacity" .= (0.5 :: Double)
            ]) }
      hresStatusCode setPluginView `shouldBe` 200
      lookupNestedText ["view", "overlay_mode"] (hresBody setPluginView) `shouldBe` Just "plugin"
      lookupNestedText ["view", "plugin_overlay"] (hresBody setPluginView) `shouldBe` Just "weather"
      lookupNestedValue ["view", "overlay_field"] (hresBody setPluginView) `shouldBe` Just (Number 0)

      overlays <- request app (mkRequest "GET" ["overlays"])
      hresStatusCode overlays `shouldBe` 200
      objectHasKey "overlays" (hresBody overlays) `shouldBe` True
      objectHasKey "diagnostics" (hresBody overlays) `shouldBe` True

      schema <- request app (mkRequest "GET" ["overlays", "schema"])
        { hreqQuery = [("overlay", Just "weather")] }
      hresStatusCode schema `shouldBe` 200
      lookupText "format" (hresBody schema) `shouldBe` Just "toposchema"
      objectHasKey "diagnostics" (hresBody schema) `shouldBe` True

      provenance <- request app (mkRequest "GET" ["overlays", "provenance"])
        { hreqQuery = [("overlay", Just "weather")] }
      hresStatusCode provenance `shouldBe` 200
      lookupText "format" (hresBody provenance) `shouldBe` Just "topolay-provenance"

      exportRsp <- request app (mkRequest "POST" ["overlays", "export"])
        { hreqBody = Just (object ["overlay" .= ("weather" :: Text)]) }
      hresStatusCode exportRsp `shouldBe` 200
      lookupText "format" (hresBody exportRsp) `shouldBe` Just "topolay-json"
      objectHasKey "payload" (hresBody exportRsp) `shouldBe` True

      case (lookupValue "schema" (hresBody exportRsp), lookupValue "payload" (hresBody exportRsp)) of
        (Just schemaValue, Just payloadValue) -> do
          validateRsp <- request app (mkRequest "POST" ["overlays", "import", "validate"])
            { hreqBody = Just (object ["schema" .= schemaValue, "payload" .= payloadValue]) }
          hresStatusCode validateRsp `shouldBe` 200
          lookupValue "valid" (hresBody validateRsp) `shouldBe` Just (Bool True)
          objectHasKey "diagnostics" (hresBody validateRsp) `shouldBe` True
        _ -> expectationFailure "overlay export did not include schema and payload"

      badDenseImport <- request app (mkRequest "POST" ["overlays", "import", "validate"])
        { hreqBody = Just badDenseOverlayImport }
      hresStatusCode badDenseImport `shouldBe` 200
      lookupValue "valid" (hresBody badDenseImport) `shouldBe` Just (Bool False)
      objectHasKey "diagnostics" (hresBody badDenseImport) `shouldBe` True

      missingImportBody <- request app (withRequestIdHeader "overlay-import-missing-body" $
        mkRequest "POST" ["overlays", "import", "validate"])
      assertBodyPolicyResponse "overlay-import-missing-body"
        (ExpectBodyPolicyError 400 "validation_failed" "missing_body")
        missingImportBody

      nonObjectImportBody <- request app (withRequestIdHeader "overlay-import-non-object" $
        (mkRequest "POST" ["overlays", "import", "validate"]) { hreqBody = Just (String "not-object") })
      assertBodyPolicyResponse "overlay-import-non-object"
        (ExpectBodyPolicyError 400 "validation_failed" "invalid_body")
        nonObjectImportBody

      missingSchemaImport <- request app (mkRequest "POST" ["overlays", "import", "validate"])
        { hreqBody = Just (object ["payload" .= object []]) }
      hresStatusCode missingSchemaImport `shouldBe` 200
      lookupValue "valid" (hresBody missingSchemaImport) `shouldBe` Just (Bool False)
      arrayFieldContainsObjectWithText "diagnostics" "code" "missing_schema" (hresBody missingSchemaImport) `shouldBe` True

      missingFields <- request app (mkRequest "GET" ["overlays", "fields"])
        { hreqQuery = [("overlay", Just "missing")] }
      hresStatusCode missingFields `shouldBe` 404
      lookupNestedText ["error", "code"] (hresBody missingFields) `shouldBe` Just "not_found"

      setCurrent <- request app (mkRequest "PUT" ["overlays", "current"])
        { hreqBody = Just (object ["overlay" .= ("weather" :: Text)]) }
      hresStatusCode setCurrent `shouldBe` 200
      cycleField <- request app (mkRequest "POST" ["overlays", "fields", "cycle"])
        { hreqBody = Just (object ["direction" .= (1 :: Int)]) }
      hresStatusCode cycleField `shouldBe` 200
      lookupText "field_name" (hresBody cycleField) `shouldBe` Just "value"

      mesh <- request app (mkRequest "POST" ["terrain", "mesh", "export"])
        { hreqBody = Just (object ["x0" .= (0 :: Int), "y0" .= (0 :: Int), "x1" .= (1 :: Int), "y1" .= (1 :: Int)]) }
      hresStatusCode mesh `shouldBe` 200
      lookupText "format" (hresBody mesh) `shouldBe` Just "topo-mesh-json"
      objectHasKey "diagnostics" (hresBody mesh) `shouldBe` True

      largeMesh <- request app (mkRequest "POST" ["terrain", "mesh", "export"])
        { hreqBody = Just (object ["x0" .= (0 :: Int), "y0" .= (0 :: Int), "x1" .= (10000 :: Int), "y1" .= (10000 :: Int)]) }
      hresStatusCode largeMesh `shouldBe` 400
      lookupNestedText ["error", "code"] (hresBody largeMesh) `shouldBe` Just "invalid_request"

      overflowMesh <- request app (mkRequest "POST" ["terrain", "mesh", "export"])
        { hreqBody = Just (object ["x0" .= (0 :: Int), "y0" .= (0 :: Int), "x1" .= (maxBound :: Int), "y1" .= (0 :: Int)]) }
      hresStatusCode overflowMesh `shouldBe` 400
      lookupNestedText ["error", "code"] (hresBody overflowMesh) `shouldBe` Just "invalid_request"

      sample <- request app (mkRequest "POST" ["terrain", "sample", "export"])
        { hreqBody = Just (object ["x" .= (0 :: Double), "y" .= (0 :: Double)]) }
      hresStatusCode sample `shouldBe` 200
      lookupText "format" (hresBody sample) `shouldBe` Just "topo-sample-json"
      objectHasKey "sample" (hresBody sample) `shouldBe` True

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

withHttpSmokeTempHome :: (FilePath -> IO a) -> IO a
withHttpSmokeTempHome action = do
  tmp <- getTemporaryDirectory
  stamp <- round . (* (1000000 :: POSIXTime)) <$> getPOSIXTime
  let home = tmp </> ("topo-http-world-smoke-home-" <> show (stamp :: Integer))
  bracket (createDirectory home >> pure home) removeDirectoryRecursive action

withTemporaryTopoHome :: FilePath -> IO a -> IO a
withTemporaryTopoHome home action = bracket capture restore $ \_ -> withTempHome action
  where
    envNames = ["HOME", "USERPROFILE"]
    capture = mapM (\name -> do
        value <- lookupEnv name
        pure (name, value)
      ) envNames
    restore saved = forM_ saved $ \(name, value) ->
      case value of
        Just old -> setEnv name old
        Nothing -> unsetEnv name
    withTempHome action = do
      forM_ envNames (`setEnv` home)
      action

waitForGeneratedWorld :: HeadlessApp -> IO HttpResponse
waitForGeneratedWorld app = go (300 :: Int) Nothing
  where
    go attempts latest = do
      status <- request app (mkRequest "GET" ["world", "generation-status"])
      hresStatusCode status `shouldBe` 200
      case lookupValue "chunk_count" (hresBody status) of
        Just count | lookupValue "generating" (hresBody status) == Just (Bool False)
          && positiveNumber count -> pure status
        _ | attempts <= 0 -> fail $ "generation did not complete; last status: " <> show (hresBody status, fmap hresBody latest)
          | otherwise -> do
              threadDelay 100000
              go (attempts - 1) (Just status)

waitForWorldName :: HeadlessApp -> Text -> IO HttpResponse
waitForWorldName app expectedName = go (50 :: Int) Nothing
  where
    go attempts latest = do
      meta <- request app (mkRequest "GET" ["world"])
      hresStatusCode meta `shouldBe` 200
      if lookupText "world_name" (hresBody meta) == Just expectedName
        then pure meta
        else if attempts <= 0
          then fail $ "world name did not update; last meta: " <> show (hresBody meta, fmap hresBody latest)
          else do
            threadDelay 100000
            go (attempts - 1) (Just meta)

assertWeatherOverlayAvailable :: HeadlessApp -> IO ()
assertWeatherOverlayAvailable app = do
  overlays <- request app (mkRequest "GET" ["overlays"])
  hresStatusCode overlays `shouldBe` 200
  arrayFieldContainsText "overlay_names" "weather" (hresBody overlays) `shouldBe` True
  objectHasKey "diagnostics" (hresBody overlays) `shouldBe` True

  schema <- request app (mkRequest "GET" ["overlays", "schema"])
    { hreqQuery = [("overlay", Just "weather")] }
  hresStatusCode schema `shouldBe` 200
  lookupText "format" (hresBody schema) `shouldBe` Just "toposchema"

  provenance <- request app (mkRequest "GET" ["overlays", "provenance"])
    { hreqQuery = [("overlay", Just "weather")] }
  hresStatusCode provenance `shouldBe` 200
  lookupText "format" (hresBody provenance) `shouldBe` Just "topolay-provenance"

  exportRsp <- request app (mkRequest "POST" ["overlays", "export"])
    { hreqBody = Just (object ["overlay" .= ("weather" :: Text)]) }
  hresStatusCode exportRsp `shouldBe` 200
  lookupText "format" (hresBody exportRsp) `shouldBe` Just "topolay-json"
  lookupValue "chunk_count" (hresBody exportRsp) `shouldSatisfy` maybe False positiveNumber
  objectHasKey "schema" (hresBody exportRsp) `shouldBe` True
  objectHasKey "payload" (hresBody exportRsp) `shouldBe` True

assertBackendNeutralPluginDataSurfaces :: HeadlessApp -> IO ()
assertBackendNeutralPluginDataSurfaces app = do
  pluginStatus <- request app (mkRequest "GET" ["plugins", "status"])
  hresStatusCode pluginStatus `shouldBe` 200
  objectHasKey "plugins" (hresBody pluginStatus) `shouldBe` True
  pluginsExposeSurfaceKeys (hresBody pluginStatus) `shouldBe` True

  dataState <- request app (mkRequest "GET" ["data", "state"])
  hresStatusCode dataState `shouldBe` 200
  objectHasKey "external_data_sources" (hresBody dataState) `shouldBe` True
  objectHasKey "external_data_source_count" (hresBody dataState) `shouldBe` True
  objectHasKey "external_data_source_failures" (hresBody dataState) `shouldBe` True

positiveNumber :: Value -> Bool
positiveNumber (Number n) = n > 0
positiveNumber _ = False

data BodyPolicyExpectation
  = ExpectBodyPolicySuccess
  | ExpectBodyPolicyError Int Text Text
  deriving (Eq, Show)

assertDirectBodyPolicyCase :: HeadlessApp -> (Text, HttpRequest, BodyPolicyExpectation) -> IO ()
assertDirectBodyPolicyCase app (caseName, req, expected) = do
  let requestId = "direct-body-policy-" <> caseName
  response <- request app (withRequestIdHeader requestId req)
  assertBodyPolicyResponse requestId expected response

assertDirectNoBodyQueryParamsUseQuery :: HeadlessApp -> IO ()
assertDirectNoBodyQueryParamsUseQuery app = do
  let terrainQuery = [("q", Just "0"), ("r", Just "0")]
      queryRequest = (mkRequest "GET" ["terrain", "hex"]) { hreqQuery = terrainQuery }
      queryRequestId = "direct-body-policy-query-empty"
  queryOnly <- request app (withRequestIdHeader queryRequestId queryRequest)
  assertBodyPolicyResponse queryRequestId ExpectBodyPolicySuccess queryOnly
  lookupValue "q" (hresBody queryOnly) `shouldBe` Just (Number 0)
  lookupValue "r" (hresBody queryOnly) `shouldBe` Just (Number 0)

  let conflictRequestId = "direct-body-policy-query-conflict"
      conflictRequest = queryRequest
        { hreqBody = Just (object ["q" .= (99 :: Int), "r" .= (99 :: Int)]) }
  conflict <- request app (withRequestIdHeader conflictRequestId conflictRequest)
  assertBodyPolicyResponse conflictRequestId
    (ExpectBodyPolicyError 400 "validation_failed" "unexpected_body")
    conflict

withRequestIdHeader :: Text -> HttpRequest -> HttpRequest
withRequestIdHeader requestId req =
  req { hreqHeaders = ("x-request-id", requestId) : hreqHeaders req }

assertBodyPolicyResponse :: Text -> BodyPolicyExpectation -> HttpResponse -> Expectation
assertBodyPolicyResponse requestId expected response = do
  lookupHeaderText "x-request-id" (hresHeaders response) `shouldBe` Just requestId
  case expected of
    ExpectBodyPolicySuccess ->
      hresStatusCode response `shouldBe` 200
    ExpectBodyPolicyError expectedStatus expectedCode expectedDetailCode -> do
      hresStatusCode response `shouldBe` expectedStatus
      lookupNestedText ["error", "code"] (hresBody response) `shouldBe` Just expectedCode
      errorDetailCode (hresBody response) `shouldBe` Just expectedDetailCode
      lookupNestedText ["error", "request_id"] (hresBody response) `shouldBe` Just requestId

arrayFieldContainsText :: Text -> Text -> Value -> Bool
arrayFieldContainsText field expected (Object obj) = case KM.lookup (Key.fromText field) obj of
  Just (Array values) -> String expected `elem` toList values
  _ -> False
arrayFieldContainsText _ _ _ = False

worldListContains :: Text -> Value -> Bool
worldListContains expected (Object obj) = case KM.lookup "worlds" obj of
  Just (Array worlds) -> any worldEntryMatches (toList worlds)
  _ -> False
  where
    worldEntryMatches (Object world) = KM.lookup "name" world == Just (String expected)
    worldEntryMatches _ = False
worldListContains _ _ = False

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

badDenseOverlayImport :: Value
badDenseOverlayImport = object
  [ "schema" .= object
      [ "name" .= ("dense-weather" :: Text)
      , "version" .= ("1.0.0" :: Text)
      , "description" .= ("Dense weather overlay" :: Text)
      , "storage" .= ("dense" :: Text)
      , "fields" .=
          [ object ["name" .= ("temperature" :: Text), "type" .= ("float" :: Text), "default" .= (0 :: Double)]
          , object ["name" .= ("humidity" :: Text), "type" .= ("float" :: Text), "default" .= (0 :: Double)]
          ]
      ]
  , "payload" .= object
      [ "storage" .= ("dense" :: Text)
      , "chunks" .=
          [ object
              [ "chunk_id" .= (0 :: Int)
              , "fields" .= ([[1.0, 2.0]] :: [[Double]])
              ]
          ]
      ]
  ]

installOverlayFixture :: HeadlessApp -> IO ()
installOverlayFixture app = do
  let handles = ccActorHandles (headlessCommandContext app)
      provenance = OverlayProvenance 42 7 "fixture" Nothing
      overlay = mkSparseFloatOverlay "weather" "Weather overlay" 0.5 provenance
      store = insertOverlay overlay emptyOverlayStore
  setOverlayStoreData (ahDataHandle handles) store
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

withHttpPluginDir :: IO a -> IO a
withHttpPluginDir action = bracket setup teardown (const action)
  where
    setup = do
      oldPluginDir <- lookupEnv httpPluginDirEnv
      tmp <- getTemporaryDirectory
      now <- getPOSIXTime
      let root = tmp </> ("topo-http-plugin-" <> show (round (now * 1000000) :: Integer))
          pluginBase = root </> "plugins"
          pluginDir = pluginBase </> "http-example"
      createDirectoryIfMissing True pluginDir
      LBS.writeFile (pluginDir </> "manifest.json") (Aeson.encode httpPluginManifest)
      setEnv httpPluginDirEnv pluginBase
      pure (root, oldPluginDir)

    teardown (root, oldPluginDir) = do
      maybe (unsetEnv httpPluginDirEnv) (setEnv httpPluginDirEnv) oldPluginDir
      removeDirectoryRecursive root

httpPluginDirEnv :: String
httpPluginDirEnv = "TOPO_PLUGIN_DIR"

httpPluginManifest :: Value
httpPluginManifest = object
  [ "manifestVersion" .= manifestV3
  , "name" .= ("http-example" :: Text)
  , "version" .= ("1.0.0" :: Text)
  , "runtime" .= object
      [ "protocol" .= object
          [ "min" .= currentProtocolVersion
          , "max" .= currentProtocolVersion
          ]
      ]
  , "generator" .= object ["insertAfter" .= ("biomes" :: Text)]
  , "config" .= object ["parameters" .= httpPluginParamSpecs]
  ]

httpPluginParamSpecs :: [RPCParamSpec]
httpPluginParamSpecs =
  [ RPCParamSpec
      { rpsName = "enabled"
      , rpsLabel = "Enabled"
      , rpsType = ParamBool
      , rpsRange = Nothing
      , rpsDefault = Bool True
      , rpsTooltip = ""
      }
  ]

errorDetailPath :: Value -> Maybe [Text]
errorDetailPath (Object obj) = do
  Object err <- KM.lookup "error" obj
  Array details <- KM.lookup "details" err
  case toList details of
    Object detail:_ -> case KM.lookup "path" detail of
      Just (Array pathValues) -> traverse valueText (toList pathValues)
      _ -> Nothing
    _ -> Nothing
errorDetailPath _ = Nothing

errorDetailCode :: Value -> Maybe Text
errorDetailCode (Object obj) = do
  Object err <- KM.lookup "error" obj
  Array details <- KM.lookup "details" err
  case toList details of
    Object detail:_ -> valueText =<< KM.lookup "code" detail
    _ -> Nothing
errorDetailCode _ = Nothing

valueText :: Value -> Maybe Text
valueText (String text) = Just text
valueText _ = Nothing

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

lookupNestedValue :: [Text] -> Value -> Maybe Value
lookupNestedValue [] value = Just value
lookupNestedValue [key] value = lookupValue key value
lookupNestedValue (key:rest) value = lookupValue key value >>= lookupNestedValue rest

lookupValue :: Text -> Value -> Maybe Value
lookupValue key (Object obj) = KM.lookup (Key.fromText key) obj
lookupValue _ _ = Nothing

objectHasKey :: Text -> Value -> Bool
objectHasKey key (Object obj) = KM.member (Key.fromText key) obj
objectHasKey _ _ = False

arrayFieldContainsObjectWithText :: Text -> Text -> Text -> Value -> Bool
arrayFieldContainsObjectWithText arrayField objectField expected (Object obj) =
  case KM.lookup (Key.fromText arrayField) obj of
    Just (Array values) -> any objectFieldMatches (toList values)
    _ -> False
  where
    objectFieldMatches (Object item) =
      KM.lookup (Key.fromText objectField) item == Just (String expected)
    objectFieldMatches _ = False
arrayFieldContainsObjectWithText _ _ _ _ = False

pipelineStagesExposeDiagnostics :: Value -> Bool
pipelineStagesExposeDiagnostics (Object obj) = case KM.lookup "stages" obj of
  Just (Array stages) -> any stageHasPipelineDiagnostics (toList stages)
  _ -> False
  where
    stageHasPipelineDiagnostics (Object stage) = all (`KM.member` stage)
      (map Key.fromText
        [ "dependencies"
        , "output_fields"
        , "last_run"
        , "provenance"
        , "diagnostics"
        ])
    stageHasPipelineDiagnostics _ = False
pipelineStagesExposeDiagnostics _ = False

pluginsExposeSurfaceKeys :: Value -> Bool
pluginsExposeSurfaceKeys (Object obj) = case KM.lookup "plugins" obj of
  Just (Array plugins) -> all pluginHasSurfaceKeys (toList plugins)
  _ -> False
  where
    pluginHasSurfaceKeys (Object plugin) = all (`KM.member` plugin)
      (map Key.fromText
        [ "capabilities"
        , "params"
        , "dependencies"
        , "resources"
        , "external_data_sources"
        , "logs"
        , "has_simulation"
        , "has_simulation_declaration"
        , "simulation_declaration"
        ])
    pluginHasSurfaceKeys _ = False
pluginsExposeSurfaceKeys _ = False

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

eventPayloadResultHasKey :: Text -> Text -> Value -> Bool
eventPayloadResultHasKey expectedTopic key (Object obj) = case KM.lookup "events" obj of
  Just (Array events) -> case find (eventHasTopic expectedTopic) (toList events) of
    Just (Object event) -> case KM.lookup "payload" event of
      Just (Object payload) -> case KM.lookup "result" payload of
        Just (Object result) -> KM.member (Key.fromText key) result
        _ -> False
      _ -> False
    _ -> False
  _ -> False
eventPayloadResultHasKey _ _ _ = False

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

operationResponseStatuses :: Value -> Text -> Text -> Maybe [Text]
operationResponseStatuses doc path routeMethod = do
  Object operation <- pathOperation doc path routeMethod
  Object responses <- KM.lookup "responses" operation
  pure (map Key.toText (KM.keys responses))

operationRequestExample :: Value -> Text -> Text -> Maybe Value
operationRequestExample doc path routeMethod = do
  Object operation <- pathOperation doc path routeMethod
  requestBody <- KM.lookup "requestBody" operation
  jsonContentExample requestBody

operationResponseExample :: Value -> Text -> Text -> Text -> Maybe Value
operationResponseExample doc path routeMethod status = do
  Object operation <- pathOperation doc path routeMethod
  Object responses <- KM.lookup "responses" operation
  response <- KM.lookup (Key.fromText status) responses
  jsonContentExample response

operationResponseErrorCode :: Value -> Text -> Text -> Text -> Maybe Text
operationResponseErrorCode doc path routeMethod status =
  operationResponseExample doc path routeMethod status >>= lookupNestedText ["error", "code"]

jsonContentExample :: Value -> Maybe Value
jsonContentExample (Object container) = do
  Object content <- KM.lookup "content" container
  Object json <- KM.lookup "application/json" content
  KM.lookup "example" json
jsonContentExample _ = Nothing

openApiTags :: Value -> [Text]
openApiTags doc = case lookupValue "tags" doc of
  Just (Array tags) ->
    [ tag
    | Object tagObject <- toList tags
    , Just (String tag) <- [KM.lookup "name" tagObject]
    ]
  _ -> []

errorCodeEnum :: Value -> Maybe [Text]
errorCodeEnum doc = do
  Object errorSchema <- componentProperty doc "ErrorEnvelope" "error"
  Object properties <- KM.lookup "properties" errorSchema
  Object codeSchema <- KM.lookup "code" properties
  Array codes <- KM.lookup "enum" codeSchema
  traverse codeText (toList codes)
  where
    codeText (String code) = Just code
    codeText _ = Nothing

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
  inlinePropertyNames (Object schema)

inlinePropertyNames :: Value -> Maybe [Text]
inlinePropertyNames (Object schema) = do
  Object properties <- KM.lookup "properties" schema
  pure (map Key.toText (KM.keys properties))
inlinePropertyNames _ = Nothing

componentPropertyNullable :: Value -> Text -> Text -> Maybe Bool
componentPropertyNullable doc name property = do
  Object propertySchema <- componentProperty doc name property
  Bool nullable <- KM.lookup "nullable" propertySchema
  pure nullable

componentPropertyDescription :: Value -> Text -> Text -> Maybe Text
componentPropertyDescription doc name property = do
  Object propertySchema <- componentProperty doc name property
  String description <- KM.lookup "description" propertySchema
  pure description

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

publishedOpenApiPath :: IO (Maybe FilePath)
publishedOpenApiPath = do
  cwd <- getCurrentDirectory
  -- Keep this repository-only check from escaping package-only sdist test runs
  -- under .stack-work and accidentally reading the outer checkout's docs.
  firstExisting
    [ cwd </> "docs" </> "api" </> "openapi.json"
    , takeDirectory cwd </> "docs" </> "api" </> "openapi.json"
    ]
  where
    firstExisting [] = pure Nothing
    firstExisting (path:rest) = do
      exists <- doesFileExist path
      if exists
        then pure (Just path)
        else firstExisting rest

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

assertWaiRouteBodyPolicyMatrix :: Manager -> IO ()
assertWaiRouteBodyPolicyMatrix manager = do
  let waiCases :: [(Text, BS.ByteString, String, LBS.ByteString, BodyPolicyExpectation)]
      waiCases =
        [ ( "no-body-empty"
          , "GET"
          , "http://127.0.0.1:7376/state"
          , ""
          , ExpectBodyPolicySuccess
          )
        , ( "no-body-malformed"
          , "GET"
          , "http://127.0.0.1:7376/state"
          , "{not-json"
          , ExpectBodyPolicyError 400 "validation_failed" "unexpected_body"
          )
        , ( "no-body-object"
          , "GET"
          , "http://127.0.0.1:7376/state"
          , "{}"
          , ExpectBodyPolicyError 400 "validation_failed" "unexpected_body"
          )
        , ( "no-body-non-object"
          , "GET"
          , "http://127.0.0.1:7376/state"
          , "\"not-object\""
          , ExpectBodyPolicyError 400 "validation_failed" "unexpected_body"
          )
        , ( "no-body-null"
          , "GET"
          , "http://127.0.0.1:7376/state"
          , "null"
          , ExpectBodyPolicyError 400 "validation_failed" "unexpected_body"
          )
        , ( "optional-empty"
          , "POST"
          , "http://127.0.0.1:7376/screenshots"
          , ""
          , ExpectBodyPolicySuccess
          )
        , ( "optional-malformed"
          , "POST"
          , "http://127.0.0.1:7376/screenshots"
          , "{not-json"
          , ExpectBodyPolicyError 400 "validation_failed" "invalid_body"
          )
        , ( "optional-object"
          , "POST"
          , "http://127.0.0.1:7376/screenshots"
          , "{}"
          , ExpectBodyPolicySuccess
          )
        , ( "optional-non-object"
          , "POST"
          , "http://127.0.0.1:7376/screenshots"
          , "\"not-object\""
          , ExpectBodyPolicyError 400 "validation_failed" "invalid_body"
          )
        , ( "optional-null"
          , "POST"
          , "http://127.0.0.1:7376/screenshots"
          , "null"
          , ExpectBodyPolicyError 400 "validation_failed" "invalid_body"
          )
        , ( "required-empty"
          , "POST"
          , "http://127.0.0.1:7376/ui/seed"
          , ""
          , ExpectBodyPolicyError 400 "validation_failed" "missing_body"
          )
        , ( "required-malformed"
          , "POST"
          , "http://127.0.0.1:7376/ui/seed"
          , "{not-json"
          , ExpectBodyPolicyError 400 "validation_failed" "invalid_body"
          )
        , ( "required-object"
          , "POST"
          , "http://127.0.0.1:7376/ui/seed"
          , "{\"seed\":654}"
          , ExpectBodyPolicySuccess
          )
        , ( "required-non-object"
          , "POST"
          , "http://127.0.0.1:7376/ui/seed"
          , "\"not-object\""
          , ExpectBodyPolicyError 400 "validation_failed" "invalid_body"
          )
        , ( "required-null"
          , "POST"
          , "http://127.0.0.1:7376/ui/seed"
          , "null"
          , ExpectBodyPolicyError 400 "validation_failed" "invalid_body"
          )
        ]
  forM_ waiCases (assertWaiBodyPolicyCase manager)
  assertWaiNoBodyQueryParamsUseQuery manager

assertWaiBodyPolicyCase :: Manager -> (Text, BS.ByteString, String, LBS.ByteString, BodyPolicyExpectation) -> IO ()
assertWaiBodyPolicyCase manager (caseName, requestMethod, url, rawBody, expected) = do
  let requestId = "wai-body-policy-" <> caseName
  response <- waiBodyPolicyRequest manager requestId requestMethod url rawBody
  assertBodyPolicyResponse requestId expected response

assertWaiNoBodyQueryParamsUseQuery :: Manager -> IO ()
assertWaiNoBodyQueryParamsUseQuery manager = do
  let url = "http://127.0.0.1:7376/terrain/hex?q=0&r=0"
      queryRequestId = "wai-body-policy-query-empty"
  queryOnly <- waiBodyPolicyRequest manager queryRequestId "GET" url ""
  assertBodyPolicyResponse queryRequestId ExpectBodyPolicySuccess queryOnly
  lookupValue "q" (hresBody queryOnly) `shouldBe` Just (Number 0)
  lookupValue "r" (hresBody queryOnly) `shouldBe` Just (Number 0)

  let conflictRequestId = "wai-body-policy-query-conflict"
  conflict <- waiBodyPolicyRequest manager conflictRequestId "GET" url "{\"q\":99,\"r\":99}"
  assertBodyPolicyResponse conflictRequestId
    (ExpectBodyPolicyError 400 "validation_failed" "unexpected_body")
    conflict

waiBodyPolicyRequest :: Manager -> Text -> BS.ByteString -> String -> LBS.ByteString -> IO HttpResponse
waiBodyPolicyRequest manager requestId requestMethod url rawBody = do
  req0 <- parseRequest url
  let req = req0
        { method = requestMethod
        , requestBody = RequestBodyLBS rawBody
        , requestHeaders = [("X-Request-Id", TextEncoding.encodeUtf8 requestId)]
        }
  rsp <- httpLbs req manager
  body <- decodeWaiJsonBody (responseBody rsp)
  pure HttpResponse
    { hresStatusCode = HTTP.statusCode (responseStatus rsp)
    , hresHeaders = decodeWaiResponseHeaders (responseHeaders rsp)
    , hresBody = body
    }

decodeWaiJsonBody :: LBS.ByteString -> IO Value
decodeWaiJsonBody bytes =
  case Aeson.decode bytes of
    Nothing -> expectationFailure "route body policy response was not JSON" >> pure Null
    Just body -> pure body

decodeWaiResponseHeaders headers =
  maybe [] (\value -> [("x-request-id", TextEncoding.decodeUtf8 value)])
    (lookup "X-Request-Id" headers)

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
