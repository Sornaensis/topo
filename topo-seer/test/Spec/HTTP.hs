{-# LANGUAGE OverloadedStrings #-}

module Spec.HTTP (spec) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (SomeException, bracket, catch, finally, throwIO, try)
import Control.Monad (forM_)
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (toList)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (find, nub, sort)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as TextIO
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Actor.AtlasManager (AtlasJob(..), atlasJobsForSelection, atlasJobsForSelectionTransition, drainAtlasJobs)
import Actor.Data
  ( getDataSnapshot, getTerrainSnapshot, setOverlayStoreData, setTerrainChunkCount
  , setTerrainChunkData
  )
import Actor.Log (LogEntry(..), LogLevel(..), LogSnapshot(..), getLogSnapshot)
import Actor.SnapshotReceiver
  ( RenderSnapshot(..)
  , SnapshotVersion(..)
  , dataAndTerrainSnapshotUpdate
  , publishSnapshot
  , readCommittedRenderSnapshot
  , readDataSnapshot
  , readTerrainSnapshot
  )
import Actor.UI
  ( BaseViewMode(..)
  , ConfigTab(..)
  , LayeredViewState(..)
  , SkyOverlayMode(..)
  , UiState(..)
  , WeatherBasis(..)
  , defaultLayeredViewState
  , effectiveViewSelection
  , getUiSnapshot
  )
import Actor.UiActions (ActorHandles(..))
import Actor.UI.Setters
  ( setUiPluginNames, setUiPluginParam, setUiPluginParamSpecs, setUiWorldConfig )
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
import Network.HTTP.Types (Header)
import qualified Network.HTTP.Types.Status as HTTP
import Test.Hspec

import Seer.Headless
  ( HeadlessApp
  , HeadlessConfig(..)
  , defaultHeadlessConfig
  , headlessAppService
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
  , defaultHttpServerConfig
  , forkHttpServer
  , friendlyHttpRouteSpecs
  , handleHttpRequest
  , httpRouteSpecs
  , publicHttpRouteSpecs
  , parseHttpBind
  )
import Seer.Command.Dispatch (CommandContext(..))
import Seer.Config.Runtime (TopoSeerConfig(..))
import Seer.Config.Snapshot (defaultSnapshot)
import Seer.Editor.Types (EditorState(..), EditorTool(..))
import Seer.Render.ZoomStage (orderedZoomStagesForZoom, stageForZoom)
import Seer.System.Cache (RenderCacheState, initialRenderCacheState)
import Seer.System.Snapshot (SnapshotPollEnv(..), pollRenderSnapshot)
import Seer.World.Persist.Types (WorldSaveManifest(..))
import Seer.Service.AppService
  ( AppService(..)
  , ConfigService(..)
  , ServiceContext(..)
  , DataResourceService(..)
  , LogService(..)
  , StateService(..)
  , TerrainService(..)
  , UiService(..)
  , appServiceOperationMethods
  , appServiceOperationSpecs
  , configGetEnumsOperation
  , configGetSlidersOperation
  , dataResourceCreateRecordOperation
  , dataResourceListRecordsOperation
  , logGetOperation
  , stateGetStateOperation
  , terrainGetChunkSummaryOperation
  , terrainGetHexOperation
  , runServiceOperation
  , uiSetOverlayOperation
  , uiSetSeedOperation
  )
import Seer.Service.Events (readBufferedServiceEvents)
import Seer.Service.Types
  ( ServiceError(..)
  , ServiceEventEnvelope(..)
  , ServiceEventSeverity(..)
  , ServiceEventSource(..)
  , ServiceHandler
  , ServiceRequest(..)
  , ServiceResponse(..)
  , ServiceResult
  , TypedServiceOperation
  , rawServiceHandler
  , serviceErrorCode
  , serviceErrorHTTPStatus
  , serviceErrorMessage
  , serviceOperationMethod
  )
import Seer.System (runApp)
import Spec.Support.OverlayFixtures (mkSparseFloatOverlay)
import System.Directory
  ( createDirectory
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getCurrentDirectory
  , getTemporaryDirectory
  , listDirectory
  , removeDirectoryRecursive
  , removeFile
  )
import System.Environment (lookupEnv, setEnv, unsetEnv, withArgs)
import System.FilePath ((</>), takeDirectory, takeExtension)
import System.IO.Error (isDoesNotExistError)
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
      lookupNestedValue ["saved_path"] (hresBody screenshot) `shouldBe` Just Null
      lookupText "image_base64" (hresBody screenshot) `shouldSatisfy` maybe False (not . Text.null)

  it "validates screenshot paths and reports disabled writes on the screenshot route" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      let requests =
            [ (mkRequest "POST" ["screenshots"])
                { hreqBody = Just (object ["path" .= Null]) }
            , (mkRequest "POST" ["screenshots"])
                { hreqBody = Just (object ["path" .= ("" :: Text)]) }
            , (mkRequest "POST" ["screenshots"])
                { hreqBody = Just (object ["path" .= ("../escape.png" :: Text)]) }
            ]
      forM_ requests $ \screenshotRequest -> do
        response <- request app screenshotRequest
        hresStatusCode response `shouldBe` 400
        lookupNestedText ["error", "code"] (hresBody response)
          `shouldBe` Just "validation_failed"
        firstErrorDetailPath (hresBody response) `shouldBe` Just ["path"]

      response <- request app (mkRequest "POST" ["screenshots"])
        { hreqBody = Just (object ["path" .= ("capture.png" :: Text)]) }
      hresStatusCode response `shouldBe` 503
      lookupNestedText ["error", "code"] (hresBody response)
        `shouldBe` Just "unavailable"

  it "persists normalized screenshot paths through POST /screenshots" $
    withScreenshotHttpRoot $ \root -> do
      let runtimeConfig = (hcRuntimeConfig defaultHeadlessConfig)
            { cfgScreenshotSaveDirectory = Just root }
          headlessConfig = defaultHeadlessConfig
            { hcRuntimeConfig = runtimeConfig }
      withHeadlessApp headlessConfig $ \app -> do
        requireScreenshotPersistence app root
        captureOnly <- request app (mkRequest "POST" ["screenshots"])
        hresStatusCode captureOnly `shouldBe` 200
        lookupNestedValue ["saved_path"] (hresBody captureOnly) `shouldBe` Just Null
        listDirectory root `shouldReturn` []

        let body = object ["path" .= ("nested/capture.png" :: Text)]
        friendly <- request app (mkRequest "POST" ["screenshots"])
          { hreqBody = Just body }
        hresStatusCode friendly `shouldBe` 200
        map (`objectHasKey` hresBody friendly)
          ["image_base64", "format", "source", "saved_path"]
          `shouldBe` replicate 4 True
        lookupText "format" (hresBody friendly) `shouldBe` Just "png"
        lookupText "source" (hresBody friendly) `shouldBe` Just "headless"
        lookupText "saved_path" (hresBody friendly)
          `shouldBe` Just "nested/capture.png"
        saved <- BS.readFile (root </> "nested" </> "capture.png")
        saved `shouldSatisfy` not . BS.null

        createDirectory (root </> "conflict.png")
        conflict <- request app (mkRequest "POST" ["screenshots"])
          { hreqBody = Just (object ["path" .= ("conflict.png" :: Text)]) }
        hresStatusCode conflict `shouldBe` 409
        lookupNestedText ["error", "code"] (hresBody conflict)
          `shouldBe` Just "rejected"

  it "serves layered view state and legacy view aliases in headless mode" $
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

  describe "public headless widget automation parity" $ do
    it "round-trips the canonical live inventory and publishes synchronous clicks immediately" $
      withHeadlessApp defaultHeadlessConfig $ \app -> do
        initial <- assertCanonicalWidgetInventory app
        forM_ ["WidgetChunkMinus", "WidgetLeftTabTopo", "WidgetConfigToggle"] $ \widgetId ->
          widgetIds initial `shouldSatisfy` (elem widgetId)

        chunkDown <- clickWidgetHttp app "WidgetChunkMinus" Nothing Nothing
        assertCompletedWidgetClick "WidgetChunkMinus" True chunkDown
        immediateState <- request app (mkRequest "GET" ["state"])
        immediateUi <- request app (mkRequest "GET" ["ui", "state"])
        lookupValue "chunk_size" (hresBody immediateState) `shouldBe` Just (Number 56)
        lookupValue "chunk_size" (hresBody immediateUi) `shouldBe` Just (Number 56)

        idempotent <- clickWidgetHttp app "WidgetLeftTabTopo" Nothing Nothing
        assertCompletedWidgetClick "WidgetLeftTabTopo" False idempotent
        lookupValue "chunk_size" . hresBody <$> request app (mkRequest "GET" ["state"])
          `shouldReturn` Just (Number 56)

        opened <- clickWidgetHttp app "WidgetConfigToggle" Nothing Nothing
        assertCompletedWidgetClick "WidgetConfigToggle" True opened
        climate <- clickWidgetHttp app "WidgetConfigTabClimate" Nothing Nothing
        assertCompletedWidgetClick "WidgetConfigTabClimate" True climate
        climateInventory <- assertCanonicalWidgetInventory app
        climateSliders <- request app (mkRequest "GET" ["config", "sliders"])
          { hreqQuery = [("tab", Just "climate")] }
        hresStatusCode climateSliders `shouldBe` 200
        length (categoryWidgetIds "sliders" climateInventory)
          `shouldBe` 2 * arrayFieldLength "sliders" (hresBody climateSliders)
        activeClimate <- getWidgetStateHttp app "WidgetConfigTabClimate"
        lookupValue "active" (hresBody activeClimate) `shouldBe` Just (Bool True)
        terrainTab <- getWidgetStateHttp app "WidgetConfigTabTerrain"
        lookupValue "active" (hresBody terrainTab) `shouldBe` Just (Bool False)

        _ <- clickWidgetHttp app "WidgetConfigToggle" Nothing Nothing
        closed <- assertCanonicalWidgetInventory app
        categoryWidgetIds "sliders" closed `shouldBe` []
        _ <- clickWidgetHttp app "WidgetConfigToggle" Nothing Nothing
        reopened <- assertCanonicalWidgetInventory app
        categoryWidgetIds "sliders" reopened `shouldBe` categoryWidgetIds "sliders" climateInventory

    it "converges panel and log widgets with their direct HTTP routes from fresh fixtures" $ do
      let cases =
            [ ( "WidgetLeftToggle"
              , withBody "PUT" ["ui", "left-panel"] (object ["visible" .= False])
              )
            , ( "WidgetLeftTabView"
              , withBody "PUT" ["ui", "left-tab"] (object ["tab" .= ("view" :: Text)])
              )
            , ( "WidgetConfigToggle"
              , withBody "POST" ["ui", "config-panel", "toggle"] (object [])
              )
            , ( "WidgetLogHeader"
              , withBody "PUT" ["ui", "log", "collapsed"] (object ["collapsed" .= True])
              )
            , ( "WidgetLogWarn"
              , withBody "PUT" ["ui", "log", "level"] (object ["level" .= ("warn" :: Text)])
              )
            ]
      forM_ cases $ \(widgetId, directRequest) -> do
        widgetProjection <- withHeadlessApp defaultHeadlessConfig $ \app -> do
          clicked <- clickWidgetHttp app widgetId Nothing Nothing
          hresStatusCode clicked `shouldBe` 200
          immediatePanelLogProjection app
        directProjection <- withHeadlessApp defaultHeadlessConfig $ \app -> do
          direct <- request app directRequest
          hresStatusCode direct `shouldBe` 200
          immediatePanelLogProjection app
        widgetProjection `shouldBe` directProjection

      withHeadlessApp defaultHeadlessConfig $ \app -> do
        initial <- request app (mkRequest "GET" ["ui", "panels"])
        _ <- clickWidgetHttp app "WidgetLeftToggle" Nothing Nothing
        first <- request app (mkRequest "GET" ["ui", "panels"])
        lookupNestedValue ["left_panel", "visible"] (hresBody first)
          `shouldBe` Just (Bool False)
        _ <- clickWidgetHttp app "WidgetLeftToggle" Nothing Nothing
        final <- request app (mkRequest "GET" ["ui", "panels"])
        hresBody final `shouldBe` hresBody initial

    it "converges representative view, slider, plugin, and editor widgets with direct routes" $ do
      widgetView <- withHeadlessApp defaultHeadlessConfig $ \app -> do
        _ <- clickWidgetHttp app "WidgetLeftTabView" Nothing Nothing
        clicked <- clickWidgetHttp app "WidgetViewBaseBiome" Nothing Nothing
        assertCompletedWidgetClick "WidgetViewBaseBiome" True clicked
        _ <- clickWidgetHttp app "WidgetViewOverlayTemperature" Nothing Nothing
        _ <- clickWidgetHttp app "WidgetViewBasisAverage" Nothing Nothing
        viewProjection app
      directView <- withHeadlessApp defaultHeadlessConfig $ \app -> do
        rsp <- request app $ withBody "POST" ["ui", "view"] (object
          [ "base_mode" .= ("biome" :: Text)
          , "overlay_mode" .= ("temperature" :: Text)
          , "weather_basis" .= ("average" :: Text)
          ])
        hresStatusCode rsp `shouldBe` 200
        viewProjection app
      widgetView `shouldBe` directView

      widgetSlider <- withHeadlessApp defaultHeadlessConfig $ \app -> do
        _ <- clickWidgetHttp app "WidgetConfigToggle" Nothing Nothing
        inventory <- request app (mkRequest "GET" ["ui", "widgets"])
        sliderPlus <- case filter (Text.isPrefixOf "WidgetSliderPlus:")
            (categoryWidgetIds "sliders" (hresBody inventory)) of
          wid:_ -> pure wid
          [] -> expectationFailure "terrain slider plus widget was not advertised" >> pure ""
        sliderName <- case Text.stripPrefix "WidgetSliderPlus:" sliderPlus of
          Just name -> pure name
          Nothing -> expectationFailure "unexpected slider widget codec" >> pure ""
        beforeSlider <- request app $ withBody "POST" ["config", "sliders", "get"]
          (object ["name" .= sliderName])
        clicked <- clickWidgetHttp app sliderPlus Nothing Nothing
        hresStatusCode clicked `shouldBe` 200
        slider <- request app $ withBody "POST" ["config", "sliders", "get"]
          (object ["name" .= sliderName])
        hresStatusCode slider `shouldBe` 200
        case (lookupValue "value" (hresBody beforeSlider), lookupValue "value" (hresBody slider)) of
          (Just (Number beforeValue), Just (Number afterValue)) ->
            afterValue `shouldSatisfy`
              (\actual -> abs (actual - min 1 (beforeValue + 0.05)) < 0.000001)
          values -> expectationFailure ("slider values were not numeric: " <> show values)
        pure (sliderName, hresBody slider)
      let (sliderName, sliderState) = widgetSlider
      sliderValue <- case lookupValue "value" sliderState of
        Just value -> pure value
        Nothing -> expectationFailure "slider state omitted value" >> pure Null
      directSlider <- withHeadlessApp defaultHeadlessConfig $ \app -> do
        rsp <- request app $ withBody "POST" ["config", "sliders"]
          (object ["name" .= sliderName, "value" .= sliderValue])
        hresStatusCode rsp `shouldBe` 200
        hresBody <$> request app (withBody "POST" ["config", "sliders", "get"]
          (object ["name" .= sliderName]))
      directSlider `shouldBe` sliderState

      withHttpPluginDir $ do
        let cfg = defaultHeadlessConfig { hcDiscoverPlugins = True }
        widgetPlugin <- withHeadlessApp cfg $ \app -> do
          installHttpPluginUiFixture app
          _ <- clickWidgetHttp app "WidgetConfigToggle" Nothing Nothing
          _ <- clickWidgetHttp app "WidgetConfigTabPipeline" Nothing Nothing
          inventory <- request app (mkRequest "GET" ["ui", "widgets"])
          expandId <- requireAdvertisedWidget "WidgetPluginExpand" (hresBody inventory)
          _ <- clickWidgetHttp app expandId Nothing Nothing
          expanded <- request app (mkRequest "GET" ["ui", "widgets"])
          sliderId <- requireAdvertisedWidget "WidgetPluginParamSlider" (hresBody expanded)
          checkId <- requireAdvertisedWidget "WidgetPluginParamCheck" (hresBody expanded)
          missingArgument <- clickWidgetHttp app sliderId Nothing Nothing
          assertWidgetError 400 "invalid_request" missingArgument
          forM_ [String "nope", Number (-0.1), Number 1.1] $ \invalidPosition -> do
            invalid <- request app $ withBody "POST" ["ui", "widgets", "click"] (object
              [ "widget_id" .= sliderId
              , "normalized_position" .= invalidPosition
              ])
            hresStatusCode invalid `shouldBe` 400
          forM_ [(0, Number 10), (1, Number 20), (0.25, Number 12.5)] $
            \(position, expected) -> do
              clicked <- clickWidgetHttp app sliderId (Just position) Nothing
              assertCompletedWidgetClick sliderId True clicked
              plugins <- request app (mkRequest "GET" ["plugins"])
              pluginParamValue "http-example" "density" (hresBody plugins)
                `shouldBe` Just expected
          checked <- clickWidgetHttp app checkId Nothing Nothing
          assertCompletedWidgetClick checkId True checked
          plugins <- request app (mkRequest "GET" ["plugins"])
          pure
            ( pluginParamValue "http-example" "density" (hresBody plugins)
            , pluginParamValue "http-example" "enabled" (hresBody plugins)
            )
        directPlugin <- withHeadlessApp cfg $ \app -> do
          forM_
            ([ ("density", Number 12.5)
             , ("enabled", Bool False)
             ] :: [(Text, Value)]) $ \(paramName, value) -> do
              rsp <- request app $ withBody "PATCH" ["plugins", "params"] (object
                [ "plugin" .= ("http-example" :: Text)
                , "param" .= paramName
                , "value" .= value
                ])
              hresStatusCode rsp `shouldBe` 200
          plugins <- request app (mkRequest "GET" ["plugins"])
          pure
            ( pluginParamValue "http-example" "density" (hresBody plugins)
            , pluginParamValue "http-example" "enabled" (hresBody plugins)
            )
        widgetPlugin `shouldBe` directPlugin

        widgetEnabled <- withHeadlessApp cfg $ \app -> do
          installHttpPluginUiFixture app
          _ <- clickWidgetHttp app "WidgetConfigToggle" Nothing Nothing
          _ <- clickWidgetHttp app "WidgetConfigTabPipeline" Nothing Nothing
          inventory <- request app (mkRequest "GET" ["ui", "widgets"])
          toggleId <- requireAdvertisedWidget "WidgetPluginToggle" (hresBody inventory)
          toggled <- clickWidgetHttp app toggleId Nothing Nothing
          hresStatusCode toggled `shouldBe` 200
          plugins <- request app (mkRequest "GET" ["plugins"])
          pure (firstArrayObjectValue "plugins" "enabled" (hresBody plugins))
        directEnabled <- withHeadlessApp cfg $ \app -> do
          toggled <- request app $ withBody "PATCH" ["plugins", "enabled"] (object
            [ "name" .= ("http-example" :: Text), "enabled" .= False ])
          hresStatusCode toggled `shouldBe` 200
          plugins <- request app (mkRequest "GET" ["plugins"])
          pure (firstArrayObjectValue "plugins" "enabled" (hresBody plugins))
        widgetEnabled `shouldBe` directEnabled

      withHeadlessApp defaultHeadlessConfig $ \app -> do
        _ <- clickWidgetHttp app "WidgetEditorReopen" Nothing Nothing
        let tools =
              [ "raise", "lower", "smooth", "flatten", "noise"
              , "paint_biome", "paint_form", "set_hardness", "erode"
              ]
        forM_ (zip [0 :: Int ..] tools) $ \(slot, expectedTool) -> do
          clicked <- clickWidgetHttp app
            ("WidgetEditorTool:" <> Text.pack (show slot)) Nothing Nothing
          hresStatusCode clicked `shouldBe` 200
          editor <- request app (mkRequest "GET" ["editor"])
          lookupText "tool" (hresBody editor) `shouldBe` Just expectedTool
        beforeWrongSlot <- request app (mkRequest "GET" ["editor"])
        wrongSlot <- clickWidgetHttp app "WidgetEditorTool:9" Nothing Nothing
        hresStatusCode wrongSlot `shouldBe` 503
        afterWrongSlot <- request app (mkRequest "GET" ["editor"])
        hresBody afterWrongSlot `shouldBe` hresBody beforeWrongSlot
        _ <- clickWidgetHttp app "WidgetEditorClose" Nothing Nothing
        hiddenTool <- clickWidgetHttp app "WidgetEditorTool:0" Nothing Nothing
        hresStatusCode hiddenTool `shouldBe` 503
        reopened <- clickWidgetHttp app "WidgetEditorReopen" Nothing Nothing
        hresStatusCode reopened `shouldBe` 200

    it "covers built-in pipeline, simulation, overlay alternatives, and real dialog persistence" $ do
      widgetPipeline <- withHeadlessApp defaultHeadlessConfig $ \app -> do
        _ <- clickWidgetHttp app "WidgetConfigToggle" Nothing Nothing
        _ <- clickWidgetHttp app "WidgetConfigTabPipeline" Nothing Nothing
        inventory <- request app (mkRequest "GET" ["ui", "widgets"])
        pipelineWidget <- requireAdvertisedWidget "WidgetPipelineToggle" (hresBody inventory)
        pipelineBefore <- request app (mkRequest "GET" ["pipeline"])
        stageName <- case firstArrayObjectText "stages" "id" (hresBody pipelineBefore) of
          Just value | Text.isInfixOf value pipelineWidget -> pure value
          _ -> expectationFailure "advertised pipeline widget did not match the first public stage" >> pure ""
        toggled <- clickWidgetHttp app pipelineWidget Nothing Nothing
        hresStatusCode toggled `shouldBe` 200
        pipelineAfter <- request app (mkRequest "GET" ["pipeline"])
        pure (stageName, hresBody pipelineAfter)
      let (stageName, widgetPipelineState) = widgetPipeline
      directPipelineState <- withHeadlessApp defaultHeadlessConfig $ \app -> do
        toggled <- request app $ withBody "PATCH" ["pipeline", "stages"] (object
          [ "stage" .= stageName, "enabled" .= False ])
        hresStatusCode toggled `shouldBe` 200
        hresBody <$> request app (mkRequest "GET" ["pipeline"])
      widgetPipelineState `shouldBe` directPipelineState

      widgetSimulation <- withHeadlessApp defaultHeadlessConfig $ \app -> do
        installSimulationFixture app
        _ <- clickWidgetHttp app "WidgetConfigToggle" Nothing Nothing
        _ <- clickWidgetHttp app "WidgetConfigTabPipeline" Nothing Nothing
        ticked <- clickWidgetHttp app "WidgetSimTick" Nothing Nothing
        hresStatusCode ticked `shouldBe` 200
        hresBody <$> request app (mkRequest "GET" ["simulation"])
      directSimulation <- withHeadlessApp defaultHeadlessConfig $ \app -> do
        installSimulationFixture app
        ticked <- request app $ withBody "POST" ["simulation", "tick"]
          (object ["count" .= (1 :: Int)])
        hresStatusCode ticked `shouldBe` 200
        hresBody <$> request app (mkRequest "GET" ["simulation"])
      widgetSimulation `shouldBe` directSimulation

      withHeadlessApp defaultHeadlessConfig $ \app -> do
        installHttpOverlayTerrain app
        _ <- clickWidgetHttp app "WidgetLeftTabView" Nothing Nothing
        forM_
          [ "WidgetOverlayManager", "WidgetOverlaySchema", "WidgetOverlayProvenance"
          , "WidgetOverlayExport", "WidgetOverlayImportValidate"
          ] $ \widgetId -> do
            capability <- getWidgetStateHttp app widgetId
            lookupValue "support" (hresBody capability)
              `shouldBe` Just (String "non_clickable")
            rejected <- clickWidgetHttp app widgetId Nothing Nothing
            assertWidgetError 400 "invalid_request" rejected
        hresStatusCode <$> request app (mkRequest "GET" ["overlays"]) `shouldReturn` 200
        hresStatusCode <$> request app (mkRequest "GET" ["overlays", "schema"])
          { hreqQuery = [("overlay", Just "weather")] } `shouldReturn` 200
        hresStatusCode <$> request app (mkRequest "GET" ["overlays", "provenance"])
          { hreqQuery = [("overlay", Just "weather")] } `shouldReturn` 200
        exported <- request app $ withBody "POST" ["overlays", "export"]
          (object ["overlay" .= ("weather" :: Text)])
        hresStatusCode exported `shouldBe` 200
        validated <- request app $ withBody "POST" ["overlays", "import", "validate"] badDenseOverlayImport
        hresStatusCode validated `shouldBe` 200

      withHttpSmokeTempHome $ \home -> withTemporaryTopoHome home $
        withHeadlessApp defaultHeadlessConfig $ \app -> do
          _ <- clickWidgetHttp app "WidgetConfigToggle" Nothing Nothing
          opened <- clickWidgetHttp app "WidgetConfigPresetSave" Nothing Nothing
          hresStatusCode opened `shouldBe` 200
          textSet <- request app $ withBody "PUT" ["ui", "dialog", "text"]
            (object ["text" .= ("http-parity-preset" :: Text)])
          hresStatusCode textSet `shouldBe` 200
          confirmed <- request app $ withBody "POST" ["ui", "key"]
            (object ["key" .= ("enter" :: Text)])
          hresStatusCode confirmed `shouldBe` 200
          presets <- request app (mkRequest "GET" ["presets"])
          arrayFieldContainsText "presets" "http-parity-preset" (hresBody presets)
            `shouldBe` True
          lookupText "menu_mode" . hresBody <$> request app (mkRequest "GET" ["ui", "dialog"])
            `shouldReturn` Just "none"
          _ <- clickWidgetHttp app "WidgetConfigPresetLoad" Nothing Nothing
          missingIndex <- clickWidgetHttp app "WidgetPresetLoadItem" Nothing Nothing
          assertWidgetError 400 "invalid_request" missingIndex
          outOfRange <- clickWidgetHttp app "WidgetPresetLoadItem" Nothing (Just 99)
          assertWidgetError 400 "invalid_request" outOfRange
          selected <- clickWidgetHttp app "WidgetPresetLoadItem" Nothing (Just 0)
          hresStatusCode selected `shouldBe` 200
          loaded <- clickWidgetHttp app "WidgetPresetLoadOk" Nothing Nothing
          hresStatusCode loaded `shouldBe` 200
          _ <- clickWidgetHttp app "WidgetConfigPresetSave" Nothing Nothing
          _ <- request app $ withBody "PUT" ["ui", "dialog", "text"]
            (object ["text" .= ("http-direct-confirm" :: Text)])
          directConfirmed <- request app (mkRequest "POST" ["ui", "dialog", "confirm"])
          hresStatusCode directConfirmed `shouldBe` 200
          _ <- clickWidgetHttp app "WidgetConfigPresetLoad" Nothing Nothing
          cancelled <- request app (mkRequest "POST" ["ui", "dialog", "cancel"])
          hresStatusCode cancelled `shouldBe` 200

    it "locks visibility, modal, unsupported, argument, and error mappings without state drift" $
      withHeadlessApp defaultHeadlessConfig $ \app -> do
        _ <- clickWidgetHttp app "WidgetLeftToggle" Nothing Nothing
        hiddenState <- getWidgetStateHttp app "WidgetChunkMinus"
        lookupValue "visible" (hresBody hiddenState) `shouldBe` Just (Bool False)
        lookupValue "enabled" (hresBody hiddenState) `shouldBe` Just (Bool False)
        beforeHidden <- publicUiReadSet app
        hidden <- clickWidgetHttp app "WidgetChunkMinus" Nothing Nothing
        assertWidgetError 503 "unavailable" hidden
        publicUiReadSet app `shouldReturn` beforeHidden

        _ <- clickWidgetHttp app "WidgetLeftToggle" Nothing Nothing
        _ <- clickWidgetHttp app "WidgetLeftTabView" Nothing Nothing
        basis <- getWidgetStateHttp app "WidgetViewBasisAverage"
        lookupValue "visible" (hresBody basis) `shouldBe` Just (Bool True)
        lookupValue "enabled" (hresBody basis) `shouldBe` Just (Bool False)
        beforeBasis <- viewProjection app
        disabled <- clickWidgetHttp app "WidgetViewBasisAverage" Nothing Nothing
        assertWidgetError 409 "rejected" disabled
        viewProjection app `shouldReturn` beforeBasis

        manager <- getWidgetStateHttp app "WidgetOverlayManager"
        lookupValue "support" (hresBody manager) `shouldBe` Just (String "non_clickable")
        lookupValue "alternative" (hresBody manager) `shouldBe` Just (String "get_overlays")
        unsupported <- clickWidgetHttp app "WidgetOverlayManager" Nothing Nothing
        assertWidgetError 400 "invalid_request" unsupported
        hresStatusCode <$> request app (mkRequest "GET" ["overlays"]) `shouldReturn` 200

        _ <- clickWidgetHttp app "WidgetLeftTabTopo" Nothing Nothing
        localOnly <- clickWidgetHttp app "WidgetSeedValue" Nothing Nothing
        assertWidgetError 400 "invalid_request" localOnly
        randomLocal <- clickWidgetHttp app "WidgetSeedRandom" Nothing Nothing
        assertWidgetError 400 "invalid_request" randomLocal
        compatibility <- getWidgetStateHttp app "WidgetViewElevation"
        lookupValue "support" (hresBody compatibility)
          `shouldBe` Just (String "compatibility_only")
        compatibilityClick <- clickWidgetHttp app "WidgetViewElevation" Nothing Nothing
        hresStatusCode compatibilityClick `shouldBe` 200
        seed <- request app $ withBody "POST" ["ui", "seed"] (object ["seed" .= (9 :: Int)])
        hresStatusCode seed `shouldBe` 200

        unknown <- clickWidgetHttp app "WidgetDefinitelyUnknown" Nothing Nothing
        assertWidgetError 404 "not_found" unknown
        missing <- request app $ withBody "POST" ["ui", "widgets", "click"] (object [])
        hresStatusCode missing `shouldBe` 400

        escape <- request app $ withBody "POST" ["ui", "key"]
          (object ["key" .= ("escape" :: Text)])
        hresStatusCode escape `shouldBe` 200
        dialog <- request app (mkRequest "GET" ["ui", "dialog"])
        lookupText "menu_mode" (hresBody dialog) `shouldBe` Just "escape_menu"
        menuInventory <- request app (mkRequest "GET" ["ui", "widgets"])
        widgetIds (hresBody menuInventory)
          `shouldBe` ["WidgetMenuSave", "WidgetMenuLoad", "WidgetMenuExit"]
        menuExit <- clickWidgetHttp app "WidgetMenuExit" Nothing Nothing
        assertWidgetError 400 "invalid_request" menuExit
        modalBefore <- publicUiReadSet app
        blocked <- clickWidgetHttp app "WidgetChunkMinus" Nothing Nothing
        assertWidgetError 503 "unavailable" blocked
        publicUiReadSet app `shouldReturn` modalBefore
        _ <- clickWidgetHttp app "WidgetMenuSave" Nothing Nothing
        saveDialog <- request app (mkRequest "GET" ["ui", "dialog"])
        lookupText "menu_mode" (hresBody saveDialog) `shouldBe` Just "world_save"
        cancelled <- clickWidgetHttp app "WidgetWorldSaveCancel" Nothing Nothing
        hresStatusCode cancelled `shouldBe` 200
        lookupText "menu_mode" . hresBody <$> request app (mkRequest "GET" ["ui", "dialog"])
          `shouldReturn` Just "none"

  describe "ordered HTTP mutation publication matrix" $ do
    forM_ httpUiPublicationCases $ \(label, setup, routeRequest, atlasExpectation, assertUi) ->
      it label $ withHeadlessApp defaultHeadlessConfig $ \app -> do
        setup app
        expectExactHttpUiPublication app routeRequest atlasExpectation assertUi

    it "publishes log collapse as one coherent log-only epoch" $
      withHeadlessApp defaultHeadlessConfig $ \app -> do
        baseline <- beginHttpPublicationAssertion app
        rsp <- request app (mkRequest "PUT" ["ui", "log", "collapsed"])
          { hreqBody = Just (object ["collapsed" .= True]) }
        hresStatusCode rsp `shouldBe` 200
        (_, committed) <- expectExactHttpPublication app baseline
        lsCollapsed (rsLog committed) `shouldBe` True
        rsUi committed `shouldBe` rsUi (hpbSnapshot baseline)
        rsData committed `shouldBe` rsData (hpbSnapshot baseline)
        rsTerrain committed `shouldBe` rsTerrain (hpbSnapshot baseline)
        jobs <- drainAtlasJobs (ahAtlasManagerHandle (httpHandles app))
        length jobs `shouldBe` 0

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
      tid <- forkHttpServer cfg headlessAppService (headlessServiceContext app)
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

        (generatedChunkCount, savedConfig, savedOverlayNames) <- withHeadlessApp defaultHeadlessConfig $ \app -> do
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

          saveBaseline <- beginHttpPublicationAssertion app
          save <- request app (mkRequest "POST" ["worlds", "save"])
            { hreqBody = Just (object ["name" .= smokeName]) }
          hresStatusCode save `shouldBe` 200
          lookupText "name" (hresBody save) `shouldBe` Just smokeName
          lookupValue "saved" (hresBody save) `shouldBe` Just (Bool True)
          arrayFieldContainsText "formats" "world.topo" (hresBody save) `shouldBe` True
          arrayFieldContainsText "formats" "world.topolay" (hresBody save) `shouldBe` True
          (_, committedSave) <- expectExactHttpPublication app saveBaseline
          uiWorldName (rsUi committedSave) `shouldBe` smokeName
          uiWorldConfig (rsUi committedSave) `shouldSatisfy` maybe False (const True)
          rsLog committedSave `shouldBe` rsLog (hpbSnapshot saveBaseline)
          rsData committedSave `shouldBe` rsData (hpbSnapshot saveBaseline)
          rsTerrain committedSave `shouldBe` rsTerrain (hpbSnapshot saveBaseline)
          saveJobs <- drainAtlasJobs (ahAtlasManagerHandle (httpHandles app))
          length saveJobs `shouldBe` 0

          worlds <- request app (mkRequest "GET" ["worlds"])
          hresStatusCode worlds `shouldBe` 200
          worldListContains smokeName (hresBody worlds) `shouldBe` True
          pure
            ( generatedChunkCount
            , uiWorldConfig (rsUi committedSave)
            , uiOverlayNames (rsUi committedSave)
            )

        withHeadlessApp defaultHeadlessConfig $ \loadApp -> do
          loadBaseline <- beginHttpPublicationAssertion loadApp
          load <- request loadApp (mkRequest "POST" ["worlds", "load"])
            { hreqBody = Just (object ["name" .= smokeName]) }
          hresStatusCode load `shouldBe` 200
          lookupText "name" (hresBody load) `shouldBe` Just smokeName
          lookupValue "loaded" (hresBody load) `shouldBe` Just (Bool True)
          arrayFieldContainsText "formats" "world.topo" (hresBody load) `shouldBe` True
          arrayFieldContainsText "formats" "world.topolay" (hresBody load) `shouldBe` True
          arrayFieldContainsText "overlay_names" "weather" (hresBody load) `shouldBe` True

          (loadVersion, committedLoad) <- expectExactHttpPublication loadApp loadBaseline
          uiWorldName (rsUi committedLoad) `shouldBe` smokeName
          uiWorldConfig (rsUi committedLoad) `shouldBe` savedConfig
          effectiveViewSelection (rsUi committedLoad) `shouldBe` defaultLayeredViewState
          uiSimTickCount (rsUi committedLoad) `shouldBe` 0
          uiOverlayNames (rsUi committedLoad) `shouldBe` savedOverlayNames
          let baselineLog = rsLog (hpbSnapshot loadBaseline)
              loadedLog = rsLog committedLoad
              baselineEntries = lsEntries baselineLog
              appendedEntries = drop (length baselineEntries) (lsEntries loadedLog)
          take (length baselineEntries) (lsEntries loadedLog) `shouldBe` baselineEntries
          loadedLog { lsEntries = baselineEntries } `shouldBe` baselineLog
          case appendedEntries of
            [entry] -> do
              leLevel entry `shouldBe` LogInfo
              leMessage entry `shouldSatisfy` Text.isPrefixOf "simulation: setWorld accepted"
            _ -> expectationFailure "expected one simulation load log entry"
          authoritativeData <- getDataSnapshot (ahDataHandle (httpHandles loadApp))
          authoritativeTerrain <- getTerrainSnapshot (ahDataHandle (httpHandles loadApp))
          rsData committedLoad `shouldBe` authoritativeData
          rsTerrain committedLoad `shouldBe` authoritativeTerrain
          loadJobs <- drainAtlasJobs (ahAtlasManagerHandle (httpHandles loadApp))
          let loadedUi = rsUi committedLoad
              expectedLoadJobs = atlasJobsForSelection
                loadVersion
                (effectiveViewSelection loadedUi)
                (uiRenderWaterLevel loadedUi)
                (rsTerrain committedLoad)
                (orderedZoomStagesForZoom (uiZoom loadedUi))
                Nothing
          assertHttpAtlasJobsMatch loadJobs expectedLoadJobs

          loadedMeta <- request loadApp (mkRequest "GET" ["world"])
          hresStatusCode loadedMeta `shouldBe` 200
          lookupText "world_name" (hresBody loadedMeta) `shouldBe` Just smokeName
          lookupValue "chunk_count" (hresBody loadedMeta) `shouldBe` generatedChunkCount
          lookupValue "seed" (hresBody loadedMeta) `shouldBe` Just (Number 424242)
          arrayFieldContainsText "overlay_names" "weather" (hresBody loadedMeta) `shouldBe` True

          assertWeatherOverlayAvailable loadApp
          assertBackendNeutralPluginDataSurfaces loadApp

          beforeDelete <- beginHttpPublicationAssertion loadApp
          listedBeforeDelete <- request loadApp (mkRequest "GET" ["worlds"])
          worldListContains smokeName (hresBody listedBeforeDelete) `shouldBe` True
          deleted <- request loadApp (mkRequest "DELETE" ["worlds"])
            { hreqBody = Just (object ["name" .= smokeName]) }
          hresStatusCode deleted `shouldBe` 200
          lookupText "name" (hresBody deleted) `shouldBe` Just smokeName
          lookupValue "deleted" (hresBody deleted) `shouldBe` Just (Bool True)
          (_, committedDelete) <- expectExactHttpPublication loadApp beforeDelete
          uiWorldName (rsUi committedDelete) `shouldBe` uiWorldName (rsUi committedLoad)
          uiWorldConfig (rsUi committedDelete) `shouldBe` uiWorldConfig (rsUi committedLoad)
          rsData committedDelete `shouldBe` rsData committedLoad
          rsTerrain committedDelete `shouldBe` rsTerrain committedLoad
          map wsmName (uiWorldList (rsUi committedDelete)) `shouldNotSatisfy` elem smokeName

          listedAfterDelete <- request loadApp (mkRequest "GET" ["worlds"])
          worldListContains smokeName (hresBody listedAfterDelete) `shouldBe` False
          stillLoaded <- request loadApp (mkRequest "GET" ["world"])
          lookupText "world_name" (hresBody stillLoaded) `shouldBe` Just smokeName
          lookupValue "chunk_count" (hresBody stillLoaded) `shouldBe` generatedChunkCount

          missingDelete <- request loadApp (mkRequest "DELETE" ["worlds"])
            { hreqBody = Just (object ["name" .= smokeName]) }
          hresStatusCode missingDelete `shouldBe` 404
          lookupNestedText ["error", "code"] (hresBody missingDelete) `shouldBe` Just "not_found"

          unsafeDelete <- request loadApp (mkRequest "DELETE" ["worlds"])
            { hreqBody = Just (object ["name" .= ("../sibling" :: Text)]) }
          hresStatusCode unsafeDelete `shouldBe` 400
          lookupNestedText ["error", "code"] (hresBody unsafeDelete) `shouldBe` Just "validation_failed"
          errorDetailCode (hresBody unsafeDelete) `shouldBe` Just "invalid_persistence_name"
          deleteEvents <- request loadApp (mkRequest "GET" ["events"])
          eventsContainTopic "world.saved.deleted" (hresBody deleteEvents) `shouldBe` True

  it "coerces query params by declared route schema before service dispatch" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      let echoApp = headlessAppService
            { appDataResources = (appDataResources headlessAppService)
                { dataListRecords = rawServiceHandler dataResourceListRecordsOperation $ \_ serviceReq ->
                    pure . Right . ServiceResponse $
                      case serviceRequestBody serviceReq of
                        Just value -> value
                        Nothing -> Null
                }
            }
          ctx = headlessServiceContext app
          recordsRequest query = (mkRequest "GET" ["data", "records"]) { hreqQuery = query }
          baseRecordsQuery =
            [ ("plugin", Just "plugin-007")
            , ("resource", Just "records")
            ]

      coerced <- handleHttpRequest defaultHttpServerConfig echoApp ctx $
        recordsRequest
          ( baseRecordsQuery <>
            [ ("query", Just "007")
            , ("key", Just "007")
            , ("chunk", Just "-12")
            , ("page_size", Just "+3")
            ]
          )
      hresStatusCode coerced `shouldBe` 200
      lookupValue "plugin" (hresBody coerced) `shouldBe` Just (String "plugin-007")
      lookupValue "query" (hresBody coerced) `shouldBe` Just (String "007")
      lookupValue "key" (hresBody coerced) `shouldBe` Just (String "007")
      lookupValue "chunk" (hresBody coerced) `shouldBe` Just (Number (-12))
      lookupValue "page_size" (hresBody coerced) `shouldBe` Just (Number 3)

      missing <- handleHttpRequest defaultHttpServerConfig echoApp ctx $
        recordsRequest [("resource", Just "records")]
      hresStatusCode missing `shouldBe` 400
      lookupNestedText ["error", "code"] (hresBody missing) `shouldBe` Just "validation_failed"
      errorDetailCode (hresBody missing) `shouldBe` Just "missing_query_param"

      forM_
        [ ("bad", Just "abc")
        , ("null", Nothing)
        , ("fraction", Just "1.5")
        , ("exponent", Just "1e3")
        ] $ \(_, rawPageSize) -> do
          invalidInteger <- handleHttpRequest defaultHttpServerConfig echoApp ctx $
            recordsRequest (baseRecordsQuery <> [("page_size", rawPageSize)])
          hresStatusCode invalidInteger `shouldBe` 400
          lookupNestedText ["error", "code"] (hresBody invalidInteger) `shouldBe` Just "validation_failed"
          errorDetailCode (hresBody invalidInteger) `shouldBe` Just "invalid_query_param"

      validBool <- request app (mkRequest "GET" ["events"])
        { hreqQuery = [("stream", Just "false")] }
      hresStatusCode validBool `shouldBe` 200

      invalidBool <- request app (mkRequest "GET" ["events"])
        { hreqQuery = [("stream", Just "yes")] }
      hresStatusCode invalidBool `shouldBe` 400
      lookupNestedText ["error", "code"] (hresBody invalidBool) `shouldBe` Just "validation_failed"
      errorDetailCode (hresBody invalidBool) `shouldBe` Just "invalid_query_param"

  it "enforces query parser error matrices across representative routes" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      let cases :: [(Text, HttpRequest, QueryParserExpectation)]
          cases =
            [ ( "terrain-hex-valid-ints"
              , (mkRequest "GET" ["terrain", "hex"])
                  { hreqQuery = [("q", Just "0"), ("r", Just "-1")] }
              , ExpectQueryParserSuccess [("q", Number 0), ("r", Number (-1))]
              )
            , ( "terrain-hex-string-for-int"
              , (mkRequest "GET" ["terrain", "hex"])
                  { hreqQuery = [("q", Just "abc"), ("r", Just "0")] }
              , ExpectQueryParserError "invalid_query_param"
              )
            , ( "terrain-hex-null-required"
              , (mkRequest "GET" ["terrain", "hex"])
                  { hreqQuery = [("q", Nothing), ("r", Just "0")] }
              , ExpectQueryParserError "missing_query_param"
              )
            , ( "terrain-hex-missing-required"
              , (mkRequest "GET" ["terrain", "hex"])
                  { hreqQuery = [("q", Just "0")] }
              , ExpectQueryParserError "missing_query_param"
              )
            , ( "terrain-hex-duplicate-q"
              , (mkRequest "GET" ["terrain", "hex"])
                  { hreqQuery = [("q", Just "0"), ("q", Just "1"), ("r", Just "0")] }
              , ExpectQueryParserError "duplicate_query_param"
              )
            , ( "terrain-chunk-valid-int"
              , (mkRequest "GET" ["terrain", "chunk-summary"])
                  { hreqQuery = [("chunk", Just "+42")] }
              , ExpectQueryParserSuccess [("chunk", Number 42)]
              )
            , ( "terrain-chunk-string-for-int"
              , (mkRequest "GET" ["terrain", "chunk-summary"])
                  { hreqQuery = [("chunk", Just "chunk-42")] }
              , ExpectQueryParserError "invalid_query_param"
              )
            , ( "terrain-chunk-null-required"
              , (mkRequest "GET" ["terrain", "chunk-summary"])
                  { hreqQuery = [("chunk", Nothing)] }
              , ExpectQueryParserError "missing_query_param"
              )
            , ( "terrain-chunk-missing-required"
              , mkRequest "GET" ["terrain", "chunk-summary"]
              , ExpectQueryParserError "missing_query_param"
              )
            , ( "config-enums-string-preserved"
              , (mkRequest "GET" ["config", "enums"])
                  { hreqQuery = [("type", Just "007")] }
              , ExpectQueryParserSuccess [("type", String "007")]
              )
            , ( "config-enums-null-required"
              , (mkRequest "GET" ["config", "enums"])
                  { hreqQuery = [("type", Nothing)] }
              , ExpectQueryParserError "missing_query_param"
              )
            , ( "config-enums-missing-required"
              , mkRequest "GET" ["config", "enums"]
              , ExpectQueryParserError "missing_query_param"
              )
            , ( "config-sliders-string-preserved"
              , (mkRequest "GET" ["config", "sliders"])
                  { hreqQuery = [("tab", Just "terrain")] }
              , ExpectQueryParserSuccess [("tab", String "terrain")]
              )
            , ( "config-sliders-null-optional"
              , (mkRequest "GET" ["config", "sliders"])
                  { hreqQuery = [("tab", Nothing)] }
              , ExpectQueryParserError "invalid_query_param"
              )
            , ( "config-sliders-missing-optional"
              , mkRequest "GET" ["config", "sliders"]
              , ExpectQueryParserSuccess []
              )
            , ( "config-sliders-duplicate-tab"
              , (mkRequest "GET" ["config", "sliders"])
                  { hreqQuery = [("tab", Just "terrain"), ("tab", Just "plugins")] }
              , ExpectQueryParserError "duplicate_query_param"
              )
            , ( "data-records-strings-and-ints"
              , (mkRequest "GET" ["data", "records"])
                  { hreqQuery =
                      [ ("plugin", Just "plugin-007")
                      , ("resource", Just "records")
                      , ("query", Just "by_field")
                      , ("key", Just "007")
                      , ("value", Just "007")
                      , ("chunk", Just "12")
                      , ("page_size", Just "3")
                      ]
                  }
              , ExpectQueryParserSuccess
                  [ ("plugin", String "plugin-007")
                  , ("resource", String "records")
                  , ("query", String "by_field")
                  , ("key", String "007")
                  , ("value", String "007")
                  , ("chunk", Number 12)
                  , ("page_size", Number 3)
                  ]
              )
            , ( "data-records-string-for-int"
              , (mkRequest "GET" ["data", "records"])
                  { hreqQuery =
                      [ ("plugin", Just "plugin")
                      , ("resource", Just "records")
                      , ("chunk", Just "abc")
                      ]
                  }
              , ExpectQueryParserError "invalid_query_param"
              )
            , ( "data-records-null-required"
              , (mkRequest "GET" ["data", "records"])
                  { hreqQuery = [("plugin", Nothing), ("resource", Just "records")] }
              , ExpectQueryParserError "missing_query_param"
              )
            , ( "data-records-missing-required"
              , (mkRequest "GET" ["data", "records"])
                  { hreqQuery = [("resource", Just "records")] }
              , ExpectQueryParserError "missing_query_param"
              )
            , ( "data-records-null-optional-int"
              , (mkRequest "GET" ["data", "records"])
                  { hreqQuery =
                      [ ("plugin", Just "plugin")
                      , ("resource", Just "records")
                      , ("page_size", Nothing)
                      ]
                  }
              , ExpectQueryParserError "invalid_query_param"
              )
            , ( "data-records-duplicate-page-size"
              , (mkRequest "GET" ["data", "records"])
                  { hreqQuery =
                      [ ("plugin", Just "plugin")
                      , ("resource", Just "records")
                      , ("page_size", Just "3")
                      , ("page_size", Just "4")
                      ]
                  }
              , ExpectQueryParserError "duplicate_query_param"
              )
            , ( "logs-strings-and-ints"
              , (mkRequest "GET" ["logs"])
                  { hreqQuery = [("level", Just "warn"), ("limit", Just "5"), ("offset", Just "0")] }
              , ExpectQueryParserSuccess [("level", String "warn"), ("limit", Number 5), ("offset", Number 0)]
              )
            , ( "logs-string-for-int"
              , (mkRequest "GET" ["logs"])
                  { hreqQuery = [("limit", Just "many")] }
              , ExpectQueryParserError "invalid_query_param"
              )
            , ( "logs-null-optional-int"
              , (mkRequest "GET" ["logs"])
                  { hreqQuery = [("offset", Nothing)] }
              , ExpectQueryParserError "invalid_query_param"
              )
            , ( "logs-missing-optional"
              , mkRequest "GET" ["logs"]
              , ExpectQueryParserSuccess []
              )
            , ( "events-valid-bool-and-int"
              , (mkRequest "GET" ["events"])
                  { hreqQuery = [("stream", Just "false"), ("limit", Just "1")] }
              , ExpectQueryParserSuccess []
              )
            , ( "events-string-for-bool"
              , (mkRequest "GET" ["events"])
                  { hreqQuery = [("stream", Just "yes")] }
              , ExpectQueryParserError "invalid_query_param"
              )
            , ( "events-null-optional-bool"
              , (mkRequest "GET" ["events"])
                  { hreqQuery = [("stream", Nothing)] }
              , ExpectQueryParserError "invalid_query_param"
              )
            , ( "events-string-for-int"
              , (mkRequest "GET" ["events"])
                  { hreqQuery = [("limit", Just "1.5")] }
              , ExpectQueryParserError "invalid_query_param"
              )
            , ( "events-duplicate-stream"
              , (mkRequest "GET" ["events"])
                  { hreqQuery = [("stream", Just "false"), ("stream", Just "true")] }
              , ExpectQueryParserError "duplicate_query_param"
              )
            , ( "events-missing-optional"
              , mkRequest "GET" ["events"]
              , ExpectQueryParserSuccess []
              )
            ]
      forM_ cases (assertQueryParserCase app echoQueryAppService)

  it "returns JSON error envelopes for duplicate declared query params" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      let cfg = defaultHttpServerConfig { hscBindPort = 7379 }
      tid <- forkHttpServer cfg headlessAppService (headlessServiceContext app)
      manager <- newManager defaultManagerSettings
      eventually_ (assertDuplicateDeclaredQueryParamErrors manager)
        `finally` (do
          killThread tid
          threadDelay 100000)

  it "returns JSON error envelopes for malformed UTF-8 query bytes" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      let cfg = defaultHttpServerConfig { hscBindPort = 7377 }
      tid <- forkHttpServer cfg headlessAppService (headlessServiceContext app)
      manager <- newManager defaultManagerSettings
      eventually_ (assertMalformedUtf8QueryErrors manager)
        `finally` (do
          killThread tid
          threadDelay 100000)

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
      tid <- forkHttpServer cfg headlessAppService (headlessServiceContext app)
      manager <- newManager defaultManagerSettings
      eventually_ (assertWaiRouteBodyPolicyMatrix manager)
        `finally` (do
          killThread tid
          threadDelay 100000)

  describe "friendly HTTP/AppService parity matrix" $ do
    it "matches direct AppService reads and typed query coercion" $
      withHeadlessApp defaultHeadlessConfig $ \app -> do
        installTerrainFixture app
        let ctx = headlessServiceContext app

        directState <- runServiceOperation headlessAppService ctx "get_state" Null
        state <- request app (mkRequest "GET" ["state"])
        assertHttpServiceSuccess directState state

        let hexParams = object ["q" .= (0 :: Int), "r" .= (0 :: Int)]
        directHex <- runServiceOperation headlessAppService ctx "get_hex" hexParams
        hexRsp <- request app (mkRequest "GET" ["terrain", "hex"])
          { hreqQuery = [("q", Just "+0"), ("r", Just "-0")] }
        assertHttpServiceSuccess directHex hexRsp

    it "matches required mutations and optional no-argument screenshots" $
      withHeadlessApp defaultHeadlessConfig $ \app -> do
        let ctx = headlessServiceContext app
            seedBody = object ["seed" .= (640 :: Int)]
        seedRsp <- request app (mkRequest "POST" ["ui", "seed"])
          { hreqBody = Just seedBody }
        directSeed <- runServiceOperation headlessAppService ctx "set_seed" seedBody
        assertHttpServiceSuccess directSeed seedRsp

        directNoArg <- runServiceOperation headlessAppService ctx "take_screenshot" Null
        screenshotNoArg <- request app (mkRequest "POST" ["screenshots"])
        assertHttpServiceSuccess directNoArg screenshotNoArg

        let screenshotBody = object []
        directObject <- runServiceOperation headlessAppService ctx "take_screenshot" screenshotBody
        screenshotObject <- request app (mkRequest "POST" ["screenshots"])
          { hreqBody = Just screenshotBody }
        assertHttpServiceSuccess directObject screenshotObject

    it "keeps viewport commands and friendly routes on identical responses and state" $ do
      forM_
        [ ("viewport_scroll", ["ui", "viewport", "scroll"], object
            ["delta" .= (3 :: Int), "x" .= (120 :: Int), "y" .= (80 :: Int)])
        , ("viewport_drag", ["ui", "viewport", "drag"], object
            [ "x1" .= (0 :: Int), "y1" .= (0 :: Int)
            , "x2" .= (4 :: Int), "y2" .= (0 :: Int)
            ])
        , ("viewport_click", ["ui", "viewport", "click"], object
            ["x" .= (40 :: Int), "y" .= (80 :: Int), "button" .= ("left" :: Text)])
        , ("viewport_click", ["ui", "viewport", "click"], object
            ["x" .= (999999 :: Int), "y" .= (999999 :: Int), "button" .= ("right" :: Text)])
        ] $ \(serviceMethod, path, body) -> do
          (directResult, directUi) <- withHeadlessApp defaultHeadlessConfig $ \app -> do
            installTerrainFixture app
            result <- runServiceOperation headlessAppService (headlessServiceContext app) serviceMethod body
            ui <- getUiSnapshot (ahUiHandle (httpHandles app))
            pure (result, ui)
          (httpRsp, httpUi) <- withHeadlessApp defaultHeadlessConfig $ \app -> do
            installTerrainFixture app
            rsp <- request app (mkRequest "POST" path) { hreqBody = Just body }
            ui <- getUiSnapshot (ahUiHandle (httpHandles app))
            pure (rsp, ui)
          assertHttpServiceSuccess directResult httpRsp
          uiZoom httpUi `shouldBe` uiZoom directUi
          uiPanOffset httpUi `shouldBe` uiPanOffset directUi
          uiHoverHex httpUi `shouldBe` uiHoverHex directUi
          uiContextHex httpUi `shouldBe` uiContextHex directUi
          uiHexTooltipPinned httpUi `shouldBe` uiHexTooltipPinned directUi

    it "keeps list_plugins and set_overlay aliases on one AppService behavior" $ do
      withHeadlessApp defaultHeadlessConfig $ \app -> do
        let ctx = headlessServiceContext app
        directPlugins <- runServiceOperation headlessAppService ctx "list_plugins" Null
        forM_
          [ ["plugins"]
          , ["plugins", "status"]
          , ["plugins", "state"]
          , ["plugins", "dependencies"]
          ] $ \path -> do
            pluginRsp <- request app (mkRequest "GET" path)
            assertHttpServiceSuccess directPlugins pluginRsp

      let overlayBody = object ["overlay" .= ("weather" :: Text), "field_index" .= (0 :: Int)]
          prepareOverlayState app = do
            installOverlayFixture app
            _ <- runServiceOperation headlessAppService (headlessServiceContext app) "set_view" (object
              [ "base" .= ("biome" :: Text)
              , "basis" .= ("average" :: Text)
              , "overlay_opacity" .= (0.37 :: Double)
              ])
            pure ()
      forM_
        [ ("PUT", ["overlays", "current"])
        , ("POST", ["ui", "overlay"])
        , ("PUT", ["ui", "overlay"])
        ] $ \(routeMethod, path) -> do
          (directOverlay, directSelection) <- withHeadlessApp defaultHeadlessConfig $ \app -> do
            prepareOverlayState app
            result <- runServiceOperation headlessAppService
              (headlessServiceContext app) "set_overlay" overlayBody
            selection <- effectiveViewSelection <$> getUiSnapshot (ahUiHandle (httpHandles app))
            pure (result, selection)
          (overlayRsp, httpSelection) <- withHeadlessApp defaultHeadlessConfig $ \app -> do
            prepareOverlayState app
            rsp <- request app (mkRequest routeMethod path) { hreqBody = Just overlayBody }
            selection <- effectiveViewSelection <$> getUiSnapshot (ahUiHandle (httpHandles app))
            pure (rsp, selection)
          assertHttpServiceSuccess directOverlay overlayRsp
          httpSelection `shouldBe` directSelection
          lvsBaseView httpSelection `shouldBe` BaseViewBiome
          lvsWeatherBasis httpSelection `shouldBe` WeatherBasisAverage
          lvsOverlayOpacity httpSelection
            `shouldSatisfy` (\opacity -> abs (opacity - 0.37) < 0.0001)

    it "keeps overlay cycle aliases on preserved layered state" $ do
      let directionBody = object ["direction" .= (1 :: Int)]
          prepare app = do
            installOverlayFixture app
            let ctx = headlessServiceContext app
            _ <- runServiceOperation headlessAppService ctx "set_view" (object
              [ "base" .= ("biome" :: Text)
              , "basis" .= ("average" :: Text)
              , "overlay_opacity" .= (0.37 :: Double)
              ])
            _ <- runServiceOperation headlessAppService ctx "set_overlay" (object
              ["overlay" .= ("weather" :: Text), "field_index" .= (0 :: Int)])
            pure ()
      forM_
        [ ("cycle_overlay", ["overlays", "cycle"])
        , ("cycle_overlay", ["ui", "overlay", "cycle"])
        , ("cycle_overlay_field", ["overlays", "fields", "cycle"])
        , ("cycle_overlay_field", ["ui", "overlay-field", "cycle"])
        ] $ \(serviceMethod, path) -> do
          (directResult, directSelection) <- withHeadlessApp defaultHeadlessConfig $ \app -> do
            prepare app
            result <- runServiceOperation headlessAppService
              (headlessServiceContext app) serviceMethod directionBody
            selection <- effectiveViewSelection <$> getUiSnapshot (ahUiHandle (httpHandles app))
            pure (result, selection)
          (httpRsp, httpSelection) <- withHeadlessApp defaultHeadlessConfig $ \app -> do
            prepare app
            rsp <- request app (mkRequest "POST" path) { hreqBody = Just directionBody }
            selection <- effectiveViewSelection <$> getUiSnapshot (ahUiHandle (httpHandles app))
            pure (rsp, selection)
          assertHttpServiceSuccess directResult httpRsp
          httpSelection `shouldBe` directSelection
          lvsBaseView httpSelection `shouldBe` BaseViewBiome
          lvsWeatherBasis httpSelection `shouldBe` WeatherBasisAverage
          lvsOverlayOpacity httpSelection
            `shouldSatisfy` (\opacity -> abs (opacity - 0.37) < 0.0001)

    it "maps direct validation, service, and data-resource errors to HTTP" $
      withHeadlessApp defaultHeadlessConfig $ \app -> do
        let ctx = headlessServiceContext app
        directValidation <- runServiceOperation headlessAppService ctx "set_seed" (object [])
        validationRsp <- request app (withRequestIdHeader "parity-validation" $
          (mkRequest "POST" ["ui", "seed"]) { hreqBody = Just (object []) })
        assertHttpServiceError "parity-validation" directValidation validationRsp

        let serviceApp = headlessAppService
              { appUi = (appUi headlessAppService)
                  { uiSetOverlay = rawServiceHandler uiSetOverlayOperation $ \_ _ ->
                      pure (Left (ServiceUnavailable "overlay unavailable"))
                  }
              }
            overlayBody = object ["overlay" .= ("weather" :: Text)]
        directService <- runServiceOperation serviceApp ctx "set_overlay" overlayBody
        serviceRsp <- handleHttpRequest defaultHttpServerConfig serviceApp ctx $
          withRequestIdHeader "parity-service" $
            (mkRequest "PUT" ["overlays", "current"]) { hreqBody = Just overlayBody }
        assertHttpServiceError "parity-service" directService serviceRsp
        assertFriendlyHttpEvent ctx "parity-service" "ui.state.changed.failed"
          "overlays.current.set" "PUT" "/overlays/current" "set_overlay" ServiceEventWarn
          (object ["type" .= ("error" :: Text), "code" .= ("unavailable" :: Text), "message" .= ("overlay unavailable" :: Text)])

        let dataApp = headlessAppService
              { appDataResources = (appDataResources headlessAppService)
                  { dataCreateRecord = rawServiceHandler dataResourceCreateRecordOperation $ \_ _ ->
                      pure (Left (ServiceDataResourceError PermissionDenied "denied" []))
                  }
              }
            dataBody = object
              [ "plugin" .= ("fixture" :: Text)
              , "resource" .= ("records" :: Text)
              , "fields" .= object []
              ]
        directData <- runServiceOperation dataApp ctx "data_create_record" dataBody
        dataRsp <- handleHttpRequest defaultHttpServerConfig dataApp ctx $
          withRequestIdHeader "parity-data" $
            (mkRequest "POST" ["data", "records"]) { hreqBody = Just dataBody }
        assertHttpServiceError "parity-data" directData dataRsp
        assertFriendlyHttpEvent ctx "parity-data" "data.resources.changed.failed"
          "data.records.create" "POST" "/data/records" "data_create_record" ServiceEventWarn
          (object ["type" .= ("error" :: Text), "code" .= ("permission_denied" :: Text), "message" .= ("denied" :: Text)])

    it "gates auth before dispatch and publishes one correlated friendly mutation event" $
      withHeadlessApp defaultHeadlessConfig $ \app -> do
        let cfg = defaultHttpServerConfig { hscBearerToken = Just "secret" }
            ctx = headlessServiceContext app
            requestId = "friendly-parity-seed-701"
            seedBody = object ["seed" .= (701 :: Int)]
            seedRequest headers = (mkRequest "POST" ["ui", "seed"])
              { hreqHeaders = ("x-request-id", requestId) : headers
              , hreqBody = Just seedBody
              }
        baseline <- beginHttpPublicationAssertion app
        denied <- handleHttpRequest cfg headlessAppService ctx (seedRequest [])
        hresStatusCode denied `shouldBe` 401
        lookupHeaderText "x-request-id" (hresHeaders denied) `shouldBe` Just requestId
        lookupNestedText ["error", "request_id"] (hresBody denied) `shouldBe` Just requestId
        readCommittedRenderSnapshot (ahSnapshotVersionRef (httpHandles app))
          `shouldReturn` (hpbVersion baseline, hpbSnapshot baseline)
        correlatedEvents ctx requestId `shouldReturn` []

        acceptedBaseline <- beginHttpPublicationAssertion app
        accepted <- handleHttpRequest cfg headlessAppService ctx $
          seedRequest [("authorization", "Bearer secret")]
        lookupHeaderText "x-request-id" (hresHeaders accepted) `shouldBe` Just requestId
        (_, committed) <- expectExactHttpPublication app acceptedBaseline
        uiSeed (rsUi committed) `shouldBe` 701
        direct <- runServiceOperation headlessAppService ctx "set_seed" seedBody
        assertHttpServiceSuccess direct accepted
        assertFriendlyHttpEvent ctx requestId "ui.state.changed"
          "ui.seed.set" "POST" "/ui/seed" "set_seed" ServiceEventInfo
          (object ["type" .= ("object" :: Text), "keys" .= (["seed"] :: [Text]), "seed" .= (701 :: Int)])

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
          appFor code = headlessAppService
            { appDataResources = (appDataResources headlessAppService)
                { dataCreateRecord = rawServiceHandler dataResourceCreateRecordOperation $ \_ _ ->
                    pure (Left (ServiceDataResourceError code "failed" []))
                }
            }
      forM_ cases $ \(code, status) -> do
        rsp <- handleHttpRequest defaultHttpServerConfig (appFor code) (headlessServiceContext app) $
          (mkRequest "POST" ["data", "records"]) { hreqBody = Just requestBody }
        hresStatusCode rsp `shouldBe` status
        lookupNestedText ["error", "code"] (hresBody rsp) `shouldBe` Just (dataResourceErrorCodeText code)

  it "returns a stable internal-error envelope when a service handler throws" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      let rawExceptionText = "raw-handler-exception-secret" :: Text
          throwingApp = headlessAppService
            { appConfig = (appConfig headlessAppService)
                { configGetSliders = rawServiceHandler configGetSlidersOperation $ \_ _ ->
                    throwIO (userError (Text.unpack rawExceptionText))
                }
            }
          requestId = "handler-exception-req-123"
          req = (mkRequest "GET" ["config", "sliders"])
            { hreqHeaders = [("x-request-id", requestId)] }
          responseBodyText = TextEncoding.decodeUtf8 . LBS.toStrict . Aeson.encode . hresBody
          isEmptyArray (Array values) = null (toList values)
          isEmptyArray _ = False

      rsp <- handleHttpRequest defaultHttpServerConfig throwingApp (headlessServiceContext app) req
      hresStatusCode rsp `shouldBe` 500
      lookupHeaderText "x-request-id" (hresHeaders rsp) `shouldBe` Just requestId
      lookupNestedText ["error", "code"] (hresBody rsp) `shouldBe` Just "internal_error"
      lookupNestedText ["error", "message"] (hresBody rsp) `shouldBe` Just "service handler exception"
      lookupNestedText ["error", "request_id"] (hresBody rsp) `shouldBe` Just requestId
      lookupNestedValue ["error", "details"] (hresBody rsp) `shouldSatisfy` maybe False isEmptyArray
      responseBodyText rsp `shouldSatisfy` not . Text.isInfixOf rawExceptionText

  it "enforces optional bearer tokens on protected routes" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      let cfg = defaultHttpServerConfig { hscBearerToken = Just "secret" }
          ctx = headlessServiceContext app
      denied <- handleHttpRequest cfg headlessAppService ctx (mkRequest "GET" ["state"])
      hresStatusCode denied `shouldBe` 401

      allowed <- handleHttpRequest cfg headlessAppService ctx
        (mkRequest "GET" ["state"]) { hreqHeaders = [("authorization", "Bearer secret")] }
      hresStatusCode allowed `shouldBe` 200

  it "echoes request ids in protected-route error responses" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      let cfg = defaultHttpServerConfig { hscBearerToken = Just "secret" }
          ctx = headlessServiceContext app
      denied <- handleHttpRequest cfg headlessAppService ctx
        (mkRequest "GET" ["state"]) { hreqHeaders = [("x-request-id", "req-123")] }
      hresStatusCode denied `shouldBe` 401
      lookupHeaderText "x-request-id" (hresHeaders denied) `shouldBe` Just "req-123"
      lookupNestedText ["error", "request_id"] (hresBody denied) `shouldBe` Just "req-123"

  it "keeps known, unknown, and method-mismatched /commands requests absent before auth or service handling" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      serviceCalls <- newIORef []
      let ctx = headlessServiceContext app
          tokenCfg = defaultHttpServerConfig { hscBearerToken = Just "secret" }
          guardedApp = commandDispatchGuard serviceCalls
          authCases =
            [ ("no-token-config", defaultHttpServerConfig, [])
            , ("token-missing", tokenCfg, [])
            , ("token-invalid", tokenCfg, [("authorization", "Bearer wrong")])
            , ("token-valid", tokenCfg, [("authorization", "Bearer secret")])
            ]
          routeCases =
            [ ("known-get-state", "POST", ["commands", "get_state"], object [])
            , ("known-set-seed", "POST", ["commands", "set_seed"], object ["seed" .= (777 :: Int)])
            , ("unknown", "POST", ["commands", "no_such_method"], object [])
            , ("method-mismatch", "GET", ["commands", "get_state"], object [])
            ]
          commandRequest requestId routeMethod path body headers = (mkRequest routeMethod path)
            { hreqHeaders = ("x-request-id", requestId) : headers
            , hreqQuery = [("invalid", Just "query")]
            , hreqBody = Just body
            }
      baseline <- beginHttpPublicationAssertion app
      eventsBefore <- request app (mkRequest "GET" ["events"])
      (forM_ routeCases $ \(routeName, routeMethod, path, body) ->
        forM_ authCases $ \(authName, cfg, headers) -> do
          let caseName = Text.intercalate " / " [routeName, authName]
              requestId = Text.intercalate "-" ["commands-absent", routeName, authName]
          response <- handleHttpRequest cfg guardedApp ctx
            (commandRequest requestId routeMethod path body headers)
          assertCommandRouteAbsent caseName requestId response)
        `finally` assertCommandSideEffectsAbsent app baseline eventsBefore serviceCalls

  it "returns the same /commands 404 through WAI before auth, content-type, JSON, or body-size handling" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      serviceCalls <- newIORef []
      let noTokenCfg = defaultHttpServerConfig
            { hscBindPort = 7380
            , hscMaxRequestBodyBytes = 1
            }
          tokenCfg = noTokenCfg
            { hscBindPort = 7381
            , hscBearerToken = Just "secret"
            }
          guardedApp = commandDispatchGuard serviceCalls
      baseline <- beginHttpPublicationAssertion app
      eventsBefore <- request app (mkRequest "GET" ["events"])
      noTokenTid <- forkHttpServer noTokenCfg guardedApp (headlessServiceContext app)
      tokenTid <- forkHttpServer tokenCfg guardedApp (headlessServiceContext app)
      manager <- newManager defaultManagerSettings
      (eventually_ (assertWaiCommandRoutesAbsent manager)
        `finally` (do
          killThread tokenTid
          killThread noTokenTid
          threadDelay 100000))
        `finally` assertCommandSideEffectsAbsent app baseline eventsBefore serviceCalls

  describe "friendly HTTP/AppService coverage invariant" $ do
    it "covers every registered AppService method and rejects stale or command-only HTTP projections" $ do
      let catalogMethods = sort (map serviceOperationMethod appServiceOperationSpecs)
          friendlyProjection = serviceRouteProjection friendlyHttpRouteSpecs
          friendlyMethods = map fst friendlyProjection
          missingMethods = filter (`notElem` friendlyMethods) catalogMethods
          staleMethods = filter (`notElem` catalogMethods) friendlyMethods
          commandOnlyRoutes = commandRouteSignatures httpRouteSpecs
      stableMethodSetDiagnostics
        [ ("missing-friendly-route", missingMethods)
        , ("stale-friendly-route-method", staleMethods)
        , ("command-only-http-route", commandOnlyRoutes)
        ] `shouldBe` []
      appServiceOperationMethods `shouldBe` map serviceOperationMethod appServiceOperationSpecs
      serviceRouteProjection publicHttpRouteSpecs `shouldBe` friendlyProjection
      serviceRouteProjection httpRouteSpecs `shouldBe` friendlyProjection

    it "locks the nine reviewed alias groups into public metadata and OpenAPI" $ do
      let aliasProjection = filter ((> 1) . length . snd) $
            serviceRouteProjection friendlyHttpRouteSpecs
          docSignatures = openApiSignatureLines (openApiDocument publicHttpRouteSpecs)
          missingOpenApiAliases =
            [ signature
            | (_, signatures) <- aliasProjection
            , signature <- signatures
            , signature `notElem` docSignatures
            ]
      aliasProjection `shouldBe` expectedFriendlyServiceAliases
      stableMethodSetDiagnostics
        [("friendly-alias-missing-from-openapi", missingOpenApiAliases)]
        `shouldBe` []

  it "lists every public route in OpenAPI" $ do
    let doc = openApiDocument publicHttpRouteSpecs
    forM_ publicHttpRouteSpecs $ \route -> do
      let path = routePathText route
          routeMethod = Text.toLower (hrsMethod route)
      pathMethods doc path `shouldSatisfy` maybe False (routeMethod `elem`)

  it "keeps runtime, public, friendly, and OpenAPI route sets command-free and in lockstep" $ do
    httpRouteSpecs `shouldBe` friendlyHttpRouteSpecs
    publicHttpRouteSpecs `shouldBe` friendlyHttpRouteSpecs
    forM_
      [ ("runtime httpRouteSpecs", httpRouteSpecs)
      , ("public publicHttpRouteSpecs", publicHttpRouteSpecs)
      , ("friendly friendlyHttpRouteSpecs", friendlyHttpRouteSpecs)
      ] $ uncurry assertNoCommandRoutes
    let doc = openApiDocument publicHttpRouteSpecs
    sort (openApiSignatureLines doc) `shouldBe` sort (map routeSignature publicHttpRouteSpecs)
    assertNoCommandOpenApiPaths doc

  it "does not export or define generated command HTTP compatibility metadata" $ do
    mSources <- httpModuleSources
    case mSources of
      Nothing -> pendingWith "Seer/HTTP source modules are unavailable in this package-only test run"
      Just sources -> do
        case lookup "Server.hs" sources of
          Nothing -> expectationFailure "Seer/HTTP/Server.hs was not included in the structural source lock"
          Just serverSource -> httpServerExports serverSource `shouldBe` retainedHttpServerExports
        forM_ sources $ \(sourceName, source) ->
          forM_ ["\"commands\"", "command.", "seer.command"] $ \removedFragment ->
            if removedFragment `Text.isInfixOf` Text.toLower source
              then expectationFailure
                (sourceName <> " reintroduced removed command HTTP compatibility metadata: "
                  <> Text.unpack removedFragment)
              else pure ()

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
          , ("/worlds", "delete", "WorldDeleteResponse")
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
          , ("/ui/overlay", "put", "UiOverlaySetResponse")
          , ("/ui/widgets", "get", "WidgetListResponse")
          , ("/ui/widget-state", "get", "WidgetStateResponse")
          , ("/ui/widgets/click", "post", "WidgetClickResponse")
          , ("/terrain/hex", "get", "TerrainHexResponse")
          ] :: [(Text, Text, Text)]
        requestRefs =
          [ ("/presets", "post", "PresetsSaveRequest")
          , ("/presets/load", "post", "PresetsLoadRequest")
          , ("/worlds", "delete", "WorldDeleteRequest")
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
          , ("/ui/overlay", "put", "UiOverlaySetRequest")
          , ("/ui/widgets/click", "post", "WidgetClickRequest")
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
    componentRequiredFields doc "ScreenshotTakeResponse"
      `shouldBe` Just ["image_base64", "format", "source", "saved_path"]
    componentPropertyNullable doc "ScreenshotTakeResponse" "saved_path"
      `shouldBe` Just True
    componentPropertyEnum doc "ScreenshotTakeResponse" "source"
      `shouldBe` Just ["renderer", "headless"]
    componentPropertyDescription doc "ScreenshotTakeRequest" "path"
      `shouldSatisfy` maybe False (Text.isInfixOf "sandbox-relative")
    componentPropertyNames doc "WidgetClickRequest"
      `shouldBe` Just ["item_index", "normalized_position", "widget_id"]
    componentRequiredFields doc "WidgetClickRequest" `shouldBe` Just ["widget_id"]
    (lookupValue "minimum" =<< componentProperty doc "WidgetClickRequest" "normalized_position")
      `shouldBe` Just (Number 0)
    (lookupValue "maximum" =<< componentProperty doc "WidgetClickRequest" "normalized_position")
      `shouldBe` Just (Number 1)
    (lookupValue "minimum" =<< componentProperty doc "WidgetClickRequest" "item_index")
      `shouldBe` Just (Number 0)
    componentPropertyNames doc "WidgetClickResponse"
      `shouldBe` Just ["changed", "info", "operation", "request_id", "status", "widget_id"]
    componentPropertyEnum doc "WidgetClickResponse" "status"
      `shouldBe` Just ["completed", "accepted"]
    queryParameterInfo doc "/ui/widget-state" "get"
      `shouldBe` Just [("widget_id", True)]
    operationResponseStatuses doc "/ui/widgets/click" "post"
      `shouldSatisfy` maybe False (\statuses -> all (`elem` statuses) ["400", "404", "409", "503"])
    componentPropertyNames doc "WidgetListResponse"
      `shouldSatisfy` maybe False (\actual -> all (`elem` actual)
        ["widgets", "widget_count", "categories", "capabilities", "data_browser_state"])
    componentPropertyNames doc "WidgetStateResponse"
      `shouldSatisfy` maybe False (\actual -> all (`elem` actual)
        [ "widget_id", "component", "category", "active", "visible", "enabled", "preconditions"
        , "support", "required_argument", "alternative", "loading", "pending", "async_error"
        ])
    componentPropertyEnum doc "WidgetStateResponse" "support"
      `shouldBe` Just
        [ "clickable", "argument_required", "local_only"
        , "non_clickable", "compatibility_only"
        ]
    (inlinePropertyNames =<< componentProperty doc "WidgetStateResponse" "required_argument")
      `shouldBe` Just ["description", "maximum", "minimum", "name", "type"]
    (inlinePropertyNames =<< componentProperty doc "WidgetStateResponse" "pending")
      `shouldBe` Just ["operation", "request_id", "target"]
    (inlinePropertyNames =<< componentProperty doc "WidgetStateResponse" "async_error")
      `shouldBe` Just ["message", "operation", "request_id", "target"]
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
      Nothing -> pendingWith "repository docs/operator/openapi.json is not present in this package-only test run"
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
    queryParameterSchemaType doc "/terrain/hex" "get" "q" `shouldBe` Just "integer"
    queryParameterSchemaType doc "/terrain/hex" "get" "r" `shouldBe` Just "integer"
    queryParameterSchemaType doc "/terrain/chunk-summary" "get" "chunk" `shouldBe` Just "integer"
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
    operationResponseErrorCode doc "/data/records" "post" "415" `shouldBe` Just "unsupported_media_type"
    operationResponseErrorCode doc "/data/records" "get" "415" `shouldBe` Nothing
    operationResponseErrorCode doc "/data/records" "post" "422" `shouldBe` Just "schema_validation_failed"
    operationResponseErrorCode doc "/data/records" "post" "504" `shouldBe` Just "timeout"
    operationResponseStatuses doc "/data/records" "post"
      `shouldSatisfy` maybe False (\statuses -> all (`elem` statuses) ["400", "401", "403", "404", "405", "409", "415", "422", "500", "503", "504"])
    errorCodeEnum doc
      `shouldSatisfy` maybe False (\codes -> all (`elem` codes) ["validation_failed", "schema_validation_failed", "permission_denied", "operation_not_supported", "unsupported_media_type", "timeout"])
    errorCodeEnum doc `shouldSatisfy` maybe False (notElem "invalid_json")

  it "rejects unauthorized WAI requests before checking protected body size, content type, or parsing" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      let cfg = defaultHttpServerConfig
            { hscBindPort = 7374
            , hscBearerToken = Just "secret"
            , hscMaxRequestBodyBytes = 8
            }
      tid <- forkHttpServer cfg headlessAppService (headlessServiceContext app)
      manager <- newManager defaultManagerSettings
      eventually_ (assertUnauthorizedProtectedTransportErrors manager)
        `finally` (do
          killThread tid
          threadDelay 100000)

  it "rejects oversized WAI request bodies with JSON 413 envelopes" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      let cfg = defaultHttpServerConfig { hscBindPort = 7378, hscMaxRequestBodyBytes = 10 }
      tid <- forkHttpServer cfg headlessAppService (headlessServiceContext app)
      manager <- newManager defaultManagerSettings
      eventually_ (assertWaiRequestBodySizeLimits manager)
        `finally` (do
          killThread tid
          threadDelay 100000)

  it "maps retired MCP tools/list and tools/call coverage to HTTP, service, and OpenAPI routes" $ do
    let doc = openApiDocument publicHttpRouteSpecs
        toolNames = map ptLegacyName retiredMcpToolTargets
    length retiredMcpToolTargets `shouldBe` 86
    toolNames `shouldBe` nub toolNames
    forM_ retiredMcpToolTargets $ \target -> do
      assertFriendlyRouteTarget target
      assertOpenApiTarget doc target

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

  it "dispatches primary HTTP routes directly through AppService" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      state <- request app (mkRequest "GET" ["state"])
      hresStatusCode state `shouldBe` 200
      objectHasKey "seed" (hresBody state) `shouldBe` True

      setSeed <- request app (mkRequest "POST" ["ui", "seed"])
        { hreqBody = Just (object ["seed" .= (321 :: Int)]) }
      hresStatusCode setSeed `shouldBe` 200
      lookupValue "seed" (hresBody setSeed) `shouldBe` Just (Number 321)

  it "starts topo-seer headless HTTP endpoints through the CLI" $ do
    tid <- forkIO $
      withArgs ["--headless", "--http", "127.0.0.1:7392", "--test-mode"] runApp
    manager <- newManager defaultManagerSettings
    let endpoints = ["/health", "/version", "/openapi.json", "/state"]
    eventually_ (forM_ endpoints (assertEndpoint manager))
      `finally` (do
        killThread tid
        threadDelay 100000)

data HttpPublicationBaseline = HttpPublicationBaseline
  { hpbVersion :: !SnapshotVersion
  , hpbSnapshot :: !RenderSnapshot
  , hpbCache :: !RenderCacheState
  }

data HttpAtlasExpectation
  = ExpectNoHttpAtlas
  | ExpectHttpSelectionTransition
  | ExpectHttpCurrentStage

httpUiPublicationCases
  :: [(String, HeadlessApp -> IO (), HttpRequest, HttpAtlasExpectation, UiState -> Expectation)]
httpUiPublicationCases =
  [ ("POST /ui/seed", noHttpSetup, withBody "POST" ["ui", "seed"]
        (object ["seed" .= (12345 :: Int)]), ExpectNoHttpAtlas, \ui -> do
        uiSeed ui `shouldBe` 12345
        uiSeedInput ui `shouldBe` "12345")
  , ("PUT /ui/left-panel", noHttpSetup, withBody "PUT" ["ui", "left-panel"]
        (object ["visible" .= False]), ExpectNoHttpAtlas,
        \ui -> uiShowLeftPanel ui `shouldBe` False)
  , ("POST /ui/config-tab", noHttpSetup, withBody "POST" ["ui", "config-tab"]
        (object ["tab" .= ("climate" :: Text)]), ExpectNoHttpAtlas, \ui -> do
        uiConfigTab ui `shouldBe` ConfigClimate
        uiConfigScroll ui `shouldBe` 0)
  , ("POST /config/sliders", noHttpSetup, withBody "POST" ["config", "sliders"]
        (object ["name" .= ("SliderGenScale" :: Text), "value" .= (0.42 :: Double)]), ExpectNoHttpAtlas,
        \ui -> uiGenScale ui `shouldSatisfy` (\value -> abs (value - 0.42) < 0.0001))
  , ("POST /ui/view", installTerrainFixture, withBody "POST" ["ui", "view"]
        (object ["base" .= ("biome" :: Text), "overlay" .= ("cloud" :: Text), "basis" .= ("current" :: Text)]), ExpectHttpSelectionTransition,
        \ui -> do
          lvsBaseView (uiViewSelection ui) `shouldBe` BaseViewBiome
          lvsSkyOverlay (uiViewSelection ui) `shouldBe` Just SkyOverlayCloud)
  , ("PUT /ui/overlay", installHttpOverlayTerrain, withBody "PUT" ["ui", "overlay"]
        (object ["overlay" .= ("weather" :: Text), "field_index" .= (0 :: Int)]), ExpectHttpSelectionTransition,
        \ui -> lvsSkyOverlay (effectiveViewSelection ui) `shouldBe` Just (SkyOverlayPlugin "weather" 0))
  , ("PUT /camera", installTerrainFixture, withBody "PUT" ["camera"]
        (object ["x" .= (12.0 :: Double), "y" .= ((-4.0) :: Double), "zoom" .= (1.5 :: Double)]), ExpectHttpCurrentStage,
        \ui -> do
          uiPanOffset ui `shouldBe` (12, -4)
          uiZoom ui `shouldBe` 1.5)
  , ("POST /ui/viewport/drag", installTerrainFixture, withBody "POST" ["ui", "viewport", "drag"]
        (object ["x1" .= (0 :: Int), "y1" .= (0 :: Int), "x2" .= (30 :: Int), "y2" .= ((-10) :: Int)]), ExpectHttpCurrentStage,
        \ui -> uiPanOffset ui `shouldNotBe` (0, 0))
  , ("POST /ui/widgets/click", noHttpSetup, withBody "POST" ["ui", "widgets", "click"]
        (object ["widget_id" .= ("WidgetChunkMinus" :: Text)]), ExpectNoHttpAtlas,
        \ui -> uiChunkSize ui `shouldBe` 56)
  , ("POST /ui/select-hex", noHttpSetup, withBody "POST" ["ui", "select-hex"]
        (object ["q" .= (2 :: Int), "r" .= ((-3) :: Int)]), ExpectNoHttpAtlas, \ui -> do
        uiContextHex ui `shouldBe` Just (2, -3)
        uiHexTooltipPinned ui `shouldBe` True)
  , ("POST /editor/tool", noHttpSetup, withBody "POST" ["editor", "tool"]
        (object ["tool" .= ("erode" :: Text)]), ExpectNoHttpAtlas,
        \ui -> editorTool (uiEditor ui) `shouldBe` ToolErode)
  , ("POST /simulation/auto-tick", noHttpSetup, withBody "POST" ["simulation", "auto-tick"]
        (object ["enabled" .= False, "rate" .= (0.75 :: Double)]), ExpectNoHttpAtlas, \ui -> do
        uiSimAutoTick ui `shouldBe` False
        uiSimTickRate ui `shouldBe` 0.75)
  , ("PATCH /world/name", noHttpSetup, withBody "PATCH" ["world", "name"]
        (object ["name" .= ("HTTP Publication Matrix" :: Text)]), ExpectNoHttpAtlas,
        \ui -> uiWorldName ui `shouldBe` "HTTP Publication Matrix")
  ]

withBody :: Text -> [Text] -> Value -> HttpRequest
withBody routeMethod path body = (mkRequest routeMethod path) { hreqBody = Just body }

noHttpSetup :: HeadlessApp -> IO ()
noHttpSetup _ = pure ()

installHttpOverlayTerrain :: HeadlessApp -> IO ()
installHttpOverlayTerrain app = installTerrainFixture app >> installOverlayFixture app

httpHandles :: HeadlessApp -> ActorHandles
httpHandles = ccActorHandles . headlessCommandContext

httpSnapshotPollEnv :: HeadlessApp -> SnapshotPollEnv
httpSnapshotPollEnv app = SnapshotPollEnv
  { speTimingLogThresholdMs = maxBound
  , speSnapshotPollMs = 1000
  , speSnapshotVersionRef = ahSnapshotVersionRef (httpHandles app)
  , speLogSlowSnapshotPoll = const (pure ())
  }

beginHttpPublicationAssertion :: HeadlessApp -> IO HttpPublicationBaseline
beginHttpPublicationAssertion app = do
  _ <- drainAtlasJobs (ahAtlasManagerHandle (httpHandles app))
  (version, snapshot, cache, _) <- pollRenderSnapshot
    (httpSnapshotPollEnv app) 100 False (initialRenderCacheState 4)
  pure HttpPublicationBaseline
    { hpbVersion = version
    , hpbSnapshot = snapshot
    , hpbCache = cache
    }

expectExactHttpPublication
  :: HeadlessApp
  -> HttpPublicationBaseline
  -> IO (SnapshotVersion, RenderSnapshot)
expectExactHttpPublication app baseline = do
  let handles = httpHandles app
      expectedVersion = SnapshotVersion (unSnapshotVersion (hpbVersion baseline) + 1)
  committed@(version, snapshot) <- readCommittedRenderSnapshot (ahSnapshotVersionRef handles)
  version `shouldBe` expectedVersion
  liveUi <- getUiSnapshot (ahUiHandle handles)
  liveLog <- getLogSnapshot (ahLogHandle handles)
  liveData <- readDataSnapshot (ahDataSnapshotRef handles)
  liveTerrain <- readTerrainSnapshot (ahTerrainSnapshotRef handles)
  liveUi `shouldBe` rsUi snapshot
  liveLog `shouldBe` rsLog snapshot
  liveData `shouldBe` rsData snapshot
  liveTerrain `shouldBe` rsTerrain snapshot
  (polledVersion, polledSnapshot, _, _) <- pollRenderSnapshot
    (httpSnapshotPollEnv app) 101 False (hpbCache baseline)
  (polledVersion, polledSnapshot) `shouldBe` committed
  pure committed

expectExactHttpUiPublication
  :: HeadlessApp
  -> HttpRequest
  -> HttpAtlasExpectation
  -> (UiState -> Expectation)
  -> Expectation
expectExactHttpUiPublication app routeRequest atlasExpectation assertUi = do
  baseline <- beginHttpPublicationAssertion app
  rsp <- request app routeRequest
  hresStatusCode rsp `shouldBe` 200
  (version, committed) <- expectExactHttpPublication app baseline
  let handles = httpHandles app
      ui0 = rsUi (hpbSnapshot baseline)
      ui1 = rsUi committed
      selectionJobs = atlasJobsForSelectionTransition
        version
        (effectiveViewSelection ui0)
        (uiRenderWaterLevel ui0)
        (effectiveViewSelection ui1)
        (uiRenderWaterLevel ui1)
        (rsTerrain committed)
        (orderedZoomStagesForZoom (uiZoom ui1))
        Nothing
      currentStageJobs = atlasJobsForSelection
        version
        (effectiveViewSelection ui1)
        (uiRenderWaterLevel ui1)
        (rsTerrain committed)
        [stageForZoom (uiZoom ui1)]
        Nothing
  assertUi ui1
  rsLog committed `shouldBe` rsLog (hpbSnapshot baseline)
  rsData committed `shouldBe` rsData (hpbSnapshot baseline)
  rsTerrain committed `shouldBe` rsTerrain (hpbSnapshot baseline)
  jobs <- drainAtlasJobs (ahAtlasManagerHandle handles)
  case atlasExpectation of
    ExpectNoHttpAtlas -> length jobs `shouldBe` 0
    ExpectHttpSelectionTransition -> assertHttpAtlasJobsMatch jobs selectionJobs
    ExpectHttpCurrentStage -> assertHttpAtlasJobsMatch jobs currentStageJobs

assertHttpAtlasJobsMatch :: [AtlasJob] -> [AtlasJob] -> Expectation
assertHttpAtlasJobsMatch actual expected = do
  length expected `shouldSatisfy` (> 0)
  map ajKey actual `shouldBe` map ajKey expected
  map ajSnapshotVersion actual `shouldBe` map ajSnapshotVersion expected
  map ajTerrain actual `shouldBe` map ajTerrain expected
  map ajViewSelection actual `shouldBe` map ajViewSelection expected
  map ajWaterLevel actual `shouldBe` map ajWaterLevel expected
  map (\job -> (ajHexRadius job, ajAtlasScale job)) actual
    `shouldBe` map (\job -> (ajHexRadius job, ajAtlasScale job)) expected

widgetIds :: Value -> [Text]
widgetIds body =
  [ widgetId
  | Just (Array values) <- [lookupValue "widgets" body]
  , String widgetId <- toList values
  ]

categoryWidgetIds :: Text -> Value -> [Text]
categoryWidgetIds category body =
  [ widgetId
  | Just (Object categories) <- [lookupValue "categories" body]
  , Just (Array values) <- [KM.lookup (Key.fromText category) categories]
  , String widgetId <- toList values
  ]

widgetCapabilitiesFrom :: Value -> [Value]
widgetCapabilitiesFrom body = case lookupValue "capabilities" body of
  Just (Array values) -> toList values
  _ -> []

assertCanonicalWidgetInventory :: HeadlessApp -> IO Value
assertCanonicalWidgetInventory app = do
  listed <- request app (mkRequest "GET" ["ui", "widgets"])
  hresStatusCode listed `shouldBe` 200
  let body = hresBody listed
      ids = widgetIds body
      capabilities = widgetCapabilitiesFrom body
      categories =
        [ "navigation", "generation", "config", "view_modes", "log"
        , "simulation", "sliders", "pipeline", "plugins", "data_browser"
        , "editor", "menu"
        ]
  lookupValue "widget_count" body `shouldBe` Just (Number (fromIntegral (length ids)))
  length ids `shouldBe` length (nub ids)
  map (lookupText "widget_id") capabilities `shouldBe` map Just ids
  let categorized = concatMap (`categoryWidgetIds` body) categories
  sort categorized `shouldBe` sort ids
  forM_ categories $ \category -> do
    let members = categoryWidgetIds category body
    filter (`elem` members) ids `shouldBe` members
  forM_ capabilities $ \capability ->
    lookupValue "visible" capability `shouldBe` Just (Bool True)
  forM_ (zip ids capabilities) $ \(widgetId, capability) -> do
    state <- getWidgetStateHttp app widgetId
    hresStatusCode state `shouldBe` 200
    forM_
      [ "widget_id", "component", "category", "active", "visible", "enabled"
      , "preconditions", "support", "required_argument", "alternative"
      ] $ \field ->
        lookupValue field (hresBody state) `shouldBe` lookupValue field capability
  pure body

clickWidgetHttp
  :: HeadlessApp
  -> Text
  -> Maybe Double
  -> Maybe Int
  -> IO HttpResponse
clickWidgetHttp app widgetId normalizedPosition itemIndex =
  request app (mkRequest "POST" ["ui", "widgets", "click"])
    { hreqBody = Just $ object $
        [ "widget_id" .= widgetId ]
        ++ maybe [] (\position -> ["normalized_position" .= position]) normalizedPosition
        ++ maybe [] (\index -> ["item_index" .= index]) itemIndex
    }

getWidgetStateHttp :: HeadlessApp -> Text -> IO HttpResponse
getWidgetStateHttp app widgetId = request app (mkRequest "GET" ["ui", "widget-state"])
  { hreqQuery = [("widget_id", Just widgetId)] }

assertCompletedWidgetClick :: Text -> Bool -> HttpResponse -> Expectation
assertCompletedWidgetClick widgetId changed response = do
  hresStatusCode response `shouldBe` 200
  lookupText "widget_id" (hresBody response) `shouldBe` Just widgetId
  lookupText "status" (hresBody response) `shouldBe` Just "completed"
  lookupValue "changed" (hresBody response) `shouldBe` Just (Bool changed)
  objectHasKey "info" (hresBody response) `shouldBe` True

assertWidgetError :: Int -> Text -> HttpResponse -> Expectation
assertWidgetError status code response = do
  hresStatusCode response `shouldBe` status
  lookupNestedText ["error", "code"] (hresBody response) `shouldBe` Just code
  lookupValue "status" (hresBody response) `shouldBe` Nothing

immediatePanelLogProjection :: HeadlessApp -> IO Value
immediatePanelLogProjection app = do
  panels <- request app (mkRequest "GET" ["ui", "panels"])
  uiState <- request app (mkRequest "GET" ["ui", "state"])
  logs <- request app (mkRequest "GET" ["logs"])
  map hresStatusCode [panels, uiState, logs] `shouldBe` [200, 200, 200]
  pure $ object
    [ "panels" .= hresBody panels
    , "ui_state" .= hresBody uiState
    , "logs" .= hresBody logs
    ]

viewProjection :: HeadlessApp -> IO Value
viewProjection app = do
  views <- request app (mkRequest "GET" ["state", "views"])
  uiState <- request app (mkRequest "GET" ["ui", "state"])
  map hresStatusCode [views, uiState] `shouldBe` [200, 200]
  pure $ object
    [ "views" .= lookupValue "view" (hresBody views)
    , "ui_view" .= lookupValue "view" (hresBody uiState)
    ]

publicUiReadSet :: HeadlessApp -> IO [Value]
publicUiReadSet app = mapM (fmap hresBody . request app)
  [ mkRequest "GET" ["state"]
  , mkRequest "GET" ["ui", "state"]
  , mkRequest "GET" ["ui", "widgets"]
  ]

requireAdvertisedWidget :: Text -> Value -> IO Text
requireAdvertisedWidget constructor body = case filter (Text.isInfixOf constructor) (widgetIds body) of
  widgetId:_ -> pure widgetId
  [] -> expectationFailure ("missing advertised widget constructor " <> Text.unpack constructor) >> pure ""

firstArrayObjectValue :: Text -> Text -> Value -> Maybe Value
firstArrayObjectValue arrayField objectField body = do
  Array values <- lookupValue arrayField body
  Object firstValue <- case toList values of
    value:_ -> Just value
    [] -> Nothing
  KM.lookup (Key.fromText objectField) firstValue

firstArrayObjectText :: Text -> Text -> Value -> Maybe Text
firstArrayObjectText arrayField objectField body = do
  String text <- firstArrayObjectValue arrayField objectField body
  pure text

pluginParamValue :: Text -> Text -> Value -> Maybe Value
pluginParamValue pluginName paramName body = do
  Array plugins <- lookupValue "plugins" body
  Object plugin <- find (\value -> lookupText "name" value == Just pluginName) (toList plugins)
  Object params <- KM.lookup "params" plugin
  KM.lookup (Key.fromText paramName) params

request :: HeadlessApp -> HttpRequest -> IO HttpResponse
request app req = handleHttpRequest defaultHttpServerConfig headlessAppService (headlessServiceContext app) req

requireScreenshotPersistence :: HeadlessApp -> FilePath -> IO ()
requireScreenshotPersistence app root = do
  let probeName = ".topo-capability-probe.png"
  response <- request app (mkRequest "POST" ["screenshots"])
    { hreqBody = Just (object ["path" .= Text.pack probeName]) }
  case hresStatusCode response of
    200 -> do
      let probePath = root </> probeName
      persisted <- doesFileExist probePath
      if persisted
        then removeFile probePath `catch` \err ->
          if isDoesNotExistError err then pure () else throwIO err
        else pendingWith "platform/filesystem did not retain the screenshot capability probe"
    503
      | lookupNestedText ["error", "message"] (hresBody response)
          == Just "screenshot storage is unavailable" ->
          pendingWith "platform/filesystem lacks safe screenshot publication support"
    status -> expectationFailure
      ("screenshot capability probe returned HTTP " <> show status)

withScreenshotHttpRoot :: (FilePath -> IO a) -> IO a
withScreenshotHttpRoot action = do
  temporary <- getTemporaryDirectory
  stamp <- round . (* (1000000 :: POSIXTime)) <$> getPOSIXTime
  let root = temporary </> ("topo-http-screenshots-" <> show (stamp :: Integer))
  bracket (createDirectory root >> pure root) removeDirectoryRecursive action

firstErrorDetailPath :: Value -> Maybe [Text]
firstErrorDetailPath body = do
  Array details <- lookupNestedValue ["error", "details"] body
  Object detail <- case toList details of
    first : _ -> Just first
    [] -> Nothing
  Array path <- KM.lookup "path" detail
  traverse pathSegment (toList path)
  where
    pathSegment (String segment) = Just segment
    pathSegment _ = Nothing

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

data QueryParserExpectation
  = ExpectQueryParserSuccess [(Text, Value)]
  | ExpectQueryParserError Text
  deriving (Eq, Show)

echoQueryAppService :: AppService
echoQueryAppService = headlessAppService
  { appConfig = (appConfig headlessAppService)
      { configGetSliders = echoServiceHandler configGetSlidersOperation
      , configGetEnums = echoServiceHandler configGetEnumsOperation
      }
  , appTerrain = (appTerrain headlessAppService)
      { terrainGetHex = echoServiceHandler terrainGetHexOperation
      , terrainGetChunkSummary = echoServiceHandler terrainGetChunkSummaryOperation
      }
  , appDataResources = (appDataResources headlessAppService)
      { dataListRecords = echoServiceHandler dataResourceListRecordsOperation
      }
  , appLogs = (appLogs headlessAppService)
      { logGet = echoServiceHandler logGetOperation
      }
  }

echoServiceHandler :: TypedServiceOperation request response -> ServiceHandler request response
echoServiceHandler operation = rawServiceHandler operation $ \_ serviceReq ->
  pure . Right . ServiceResponse $ case serviceRequestBody serviceReq of
    Just value -> value
    Nothing -> Null

assertQueryParserCase :: HeadlessApp -> AppService -> (Text, HttpRequest, QueryParserExpectation) -> IO ()
assertQueryParserCase app appService (caseName, req, expected) = do
  let requestId = "query-parser-" <> caseName
  response <- handleHttpRequest defaultHttpServerConfig appService (headlessServiceContext app) $
    withRequestIdHeader requestId req
  case expected of
    ExpectQueryParserSuccess expectedFields -> do
      hresStatusCode response `shouldBe` 200
      forM_ expectedFields $ \(field, value) ->
        lookupValue field (hresBody response) `shouldBe` Just value
    ExpectQueryParserError expectedDetailCode -> do
      hresStatusCode response `shouldBe` 400
      lookupHeaderText "content-type" (hresHeaders response) `shouldBe` Just "application/json"
      lookupHeaderText "x-request-id" (hresHeaders response) `shouldBe` Just requestId
      lookupNestedText ["error", "code"] (hresBody response) `shouldBe` Just "validation_failed"
      errorDetailCode (hresBody response) `shouldBe` Just expectedDetailCode
      lookupNestedText ["error", "request_id"] (hresBody response) `shouldBe` Just requestId

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

assertHttpServiceSuccess :: ServiceResult -> HttpResponse -> Expectation
assertHttpServiceSuccess direct response = case direct of
  Right (ServiceResponse body) -> do
    hresStatusCode response `shouldBe` 200
    hresBody response `shouldBe` body
  Left err -> expectationFailure ("direct AppService call failed: " <> show err)

assertHttpServiceError :: Text -> ServiceResult -> HttpResponse -> Expectation
assertHttpServiceError requestId direct response = case direct of
  Left err -> do
    hresStatusCode response `shouldBe` serviceErrorHTTPStatus err
    lookupHeaderText "x-request-id" (hresHeaders response) `shouldBe` Just requestId
    lookupNestedText ["error", "code"] (hresBody response) `shouldBe` Just (serviceErrorCode err)
    lookupNestedText ["error", "message"] (hresBody response) `shouldBe` Just (serviceErrorMessage err)
    lookupNestedText ["error", "request_id"] (hresBody response) `shouldBe` Just requestId
  Right success -> expectationFailure ("direct AppService call unexpectedly succeeded: " <> show success)

correlatedEvents :: ServiceContext -> Text -> IO [ServiceEventEnvelope]
correlatedEvents ctx requestId = case svcEventBus ctx of
  Nothing -> pure []
  Just bus -> filter ((== Just requestId) . serviceEventCorrelationId)
    <$> readBufferedServiceEvents bus

assertFriendlyHttpEvent
  :: ServiceContext
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> Text
  -> ServiceEventSeverity
  -> Value
  -> Expectation
assertFriendlyHttpEvent ctx requestId topic operationId routeMethod path serviceMethod severity expectedResult = do
  events <- correlatedEvents ctx requestId
  case events of
    [event] -> do
      serviceEventTopic event `shouldBe` topic
      serviceEventSource event `shouldBe` ServiceEventFromHttp
      serviceEventSeverity event `shouldBe` severity
      serviceEventCorrelationId event `shouldBe` Just requestId
      lookupText "status" (serviceEventPayload event)
        `shouldBe` Just (if severity == ServiceEventInfo then "ok" else "error")
      lookupText "operation_id" (serviceEventPayload event) `shouldBe` Just operationId
      lookupText "http_method" (serviceEventPayload event) `shouldBe` Just routeMethod
      lookupText "path" (serviceEventPayload event) `shouldBe` Just path
      lookupText "service_method" (serviceEventPayload event) `shouldBe` Just serviceMethod
      lookupValue "result" (serviceEventPayload event) `shouldBe` Just expectedResult
      case serviceEventPayload event of
        Object payload -> sort (map Key.toText (KM.keys payload))
          `shouldBe` ["http_method", "operation_id", "path", "result", "service_method", "status"]
        payload -> expectationFailure ("friendly HTTP event payload was not an object: " <> show payload)
    _ -> expectationFailure
      ("expected one friendly HTTP event for " <> Text.unpack requestId <> ", got " <> show events)

arrayFieldLength :: Text -> Value -> Int
arrayFieldLength field (Object obj) = case KM.lookup (Key.fromText field) obj of
  Just (Array values) -> length values
  _ -> 0
arrayFieldLength _ _ = 0

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
  publishHttpTerrainFixture handles

installSimulationFixture :: HeadlessApp -> IO ()
installSimulationFixture app = do
  installTerrainFixture app
  setTerrainChunkCount (ahDataHandle (httpHandles app)) 1
  setUiWorldConfig (ahUiHandle (httpHandles app)) (Just defaultSnapshot)

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
  publishHttpTerrainFixture handles

publishHttpTerrainFixture :: ActorHandles -> IO ()
publishHttpTerrainFixture handles = do
  dataSnapshot <- getDataSnapshot (ahDataHandle handles)
  terrainSnapshot <- getTerrainSnapshot (ahDataHandle handles)
  _ <- publishSnapshot
    (ahSnapshotVersionRef handles)
    (dataAndTerrainSnapshotUpdate
      (ahDataSnapshotRef handles) dataSnapshot
      (ahTerrainSnapshotRef handles) terrainSnapshot)
  pure ()

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
  , target "delete_world" "DELETE" ["worlds"] "worlds.delete" "delete_world"
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

installHttpPluginUiFixture :: HeadlessApp -> IO ()
installHttpPluginUiFixture app = do
  let uiHandle = ahUiHandle (httpHandles app)
  setUiPluginNames uiHandle ["http-example"]
  setUiPluginParamSpecs uiHandle (Map.singleton "http-example" httpPluginParamSpecs)
  setUiPluginParam uiHandle "http-example" "enabled" (Bool True)
  setUiPluginParam uiHandle "http-example" "density" (Number 15)

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
  , RPCParamSpec
      { rpsName = "density"
      , rpsLabel = "Density"
      , rpsType = ParamFloat
      , rpsRange = Just (Number 10, Number 20)
      , rpsDefault = Number 15
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

componentPropertyEnum :: Value -> Text -> Text -> Maybe [Text]
componentPropertyEnum doc name property = do
  Object propertySchema <- componentProperty doc name property
  Array values <- KM.lookup "enum" propertySchema
  traverse enumText (toList values)
  where
    enumText (String value) = Just value
    enumText _ = Nothing

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

serviceRouteProjection :: [HttpRouteSpec] -> [(Text, [Text])]
serviceRouteProjection routes =
  [ (serviceMethod, sort signatures)
  | (serviceMethod, signatures) <- Map.toAscList $ Map.fromListWith (<>)
      [ (serviceMethod, [routeSignature route])
      | route <- routes
      , Just serviceMethod <- [hrsServiceMethod route]
      ]
  ]

commandRouteSignatures :: [HttpRouteSpec] -> [Text]
commandRouteSignatures routes = sort
  [ routeSignature route
  | route <- routes
  , case hrsPath route of
      "commands" : _ -> True
      _ -> False
  ]

stableMethodSetDiagnostics :: [(Text, [Text])] -> [Text]
stableMethodSetDiagnostics methodSets =
  [ label <> ": " <> methodName
  | (label, methods) <- methodSets
  , methodName <- sort methods
  ]

expectedFriendlyServiceAliases :: [(Text, [Text])]
expectedFriendlyServiceAliases =
  [ ( "cycle_overlay"
    , [ "POST /overlays/cycle overlays.cycle"
      , "POST /ui/overlay/cycle ui.overlay.cycle"
      ]
    )
  , ( "cycle_overlay_field"
    , [ "POST /overlays/fields/cycle overlays.field.cycle"
      , "POST /ui/overlay-field/cycle ui.overlayField.cycle"
      ]
    )
  , ( "get_camera"
    , [ "GET /camera camera.get"
      , "GET /ui/camera ui.camera.get"
      ]
    )
  , ( "get_overlays"
    , [ "GET /overlays overlays.list"
      , "GET /terrain/overlays terrain.overlays"
      ]
    )
  , ( "list_overlay_fields"
    , [ "GET /overlays/fields overlays.fields.list"
      , "GET /ui/overlay-fields ui.overlayFields.list"
      ]
    )
  , ( "list_plugins"
    , [ "GET /plugins plugins.list"
      , "GET /plugins/dependencies plugins.dependencies"
      , "GET /plugins/state plugins.state"
      , "GET /plugins/status plugins.status"
      ]
    )
  , ( "set_camera"
    , [ "PUT /camera camera.set"
      , "PUT /ui/camera ui.camera.set"
      ]
    )
  , ( "set_overlay"
    , [ "POST /ui/overlay ui.overlay.set"
      , "PUT /overlays/current overlays.current.set"
      , "PUT /ui/overlay ui.overlay.replace"
      ]
    )
  , ( "zoom_to_chunk"
    , [ "POST /camera/zoom-to-chunk camera.zoomToChunk"
      , "POST /ui/camera/zoom-to-chunk ui.camera.zoomToChunk"
      ]
    )
  ]

assertNoCommandRoutes :: String -> [HttpRouteSpec] -> Expectation
assertNoCommandRoutes routeSetName routes =
  case filter hasCommandPrefix routes of
    [] -> pure ()
    commandRoutes -> expectationFailure
      (routeSetName <> " reintroduced removed /commands HTTP routes: "
        <> Text.unpack (Text.intercalate ", " (map routeSignature commandRoutes)))
  where
    hasCommandPrefix route = case hrsPath route of
      "commands" : _ -> True
      _ -> False

assertNoCommandOpenApiPaths :: Value -> Expectation
assertNoCommandOpenApiPaths doc =
  case filter isCommandPath (openApiPathTexts doc) of
    [] -> pure ()
    commandPaths -> expectationFailure
      ("OpenAPI reintroduced removed /commands paths: "
        <> Text.unpack (Text.intercalate ", " commandPaths))
  where
    isCommandPath path = path == "/commands" || "/commands/" `Text.isPrefixOf` path

openApiPathTexts :: Value -> [Text]
openApiPathTexts doc = case lookupValue "paths" doc of
  Just (Object paths) -> map (Key.toText . fst) (KM.toList paths)
  _ -> []

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
    [ cwd </> "docs" </> "operator" </> "openapi.json"
    , takeDirectory cwd </> "docs" </> "operator" </> "openapi.json"
    ]
  where
    firstExisting [] = pure Nothing
    firstExisting (path:rest) = do
      exists <- doesFileExist path
      if exists
        then pure (Just path)
        else firstExisting rest

httpModuleSources :: IO (Maybe [(FilePath, Text)])
httpModuleSources = do
  cwd <- getCurrentDirectory
  firstExistingRoot
    [ cwd </> "topo-seer" </> "src" </> "Seer" </> "HTTP"
    , cwd </> "src" </> "Seer" </> "HTTP"
    ]
  where
    firstExistingRoot [] = pure Nothing
    firstExistingRoot (root:rest) = do
      exists <- doesDirectoryExist root
      if not exists
        then firstExistingRoot rest
        else do
          sourceFiles <- sort <$> listHaskellSources root ""
          if null sourceFiles
            then firstExistingRoot rest
            else Just <$> mapM (readSource root) sourceFiles

    listHaskellSources root relative = do
      entries <- listDirectory (root </> relative)
      concat <$> mapM (visit root relative) entries

    visit root relative entry = do
      let relativePath = if null relative then entry else relative </> entry
          absolutePath = root </> relativePath
      directory <- doesDirectoryExist absolutePath
      if directory
        then listHaskellSources root relativePath
        else pure [relativePath | takeExtension entry == ".hs"]

    readSource root sourceFile = do
      source <- TextIO.readFile (root </> sourceFile)
      pure (sourceFile, source)

httpServerExports :: Text -> [Text]
httpServerExports source =
  case Text.breakOn "module Seer.HTTP.Server" source of
    (_, moduleHeader)
      | Text.null moduleHeader -> []
      | otherwise ->
          let afterOpen = Text.drop 1 (snd (Text.breakOn "(" moduleHeader))
              exportBlock = fst (Text.breakOn ") where" afterOpen)
          in filter (not . Text.null)
              (map (Text.strip . Text.dropWhile (== ',') . Text.strip) (Text.lines exportBlock))

retainedHttpServerExports :: [Text]
retainedHttpServerExports =
  [ "HttpServerConfig(..)"
  , "defaultHttpServerConfig"
  , "parseHttpBind"
  , "runHttpServer"
  , "HttpServerHandle"
  , "startHttpServer"
  , "shutdownHttpServer"
  , "forkHttpServer"
  , "httpApplication"
  , "handleHttpRequest"
  , "HttpRequest(..)"
  , "HttpResponse(..)"
  , "httpRouteSpecs"
  , "publicHttpRouteSpecs"
  , "friendlyHttpRouteSpecs"
  ]

assertEndpoint :: Manager -> String -> IO ()
assertEndpoint manager path = do
  req <- parseRequest ("http://127.0.0.1:7392" <> path)
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

assertDuplicateDeclaredQueryParamErrors :: Manager -> IO ()
assertDuplicateDeclaredQueryParamErrors manager =
  forM_
    [ ( "duplicate-terrain-q"
      , "http://127.0.0.1:7379/terrain/hex?q=0&q=1&r=0"
      , []
      )
    , ( "duplicate-config-tab"
      , "http://127.0.0.1:7379/config/sliders?tab=terrain&tab=plugins"
      , []
      )
    , ( "duplicate-events-stream"
      , "http://127.0.0.1:7379/events?stream=false&stream=true"
      , [("Accept", "text/event-stream")]
      )
    , ( "duplicate-data-page-size"
      , "http://127.0.0.1:7379/data/records?plugin=plugin&resource=records&page_size=3&page_size=4"
      , []
      )
    ] $ \(caseName, url, extraHeaders) -> do
      let requestId = "wai-query-duplicate-" <> caseName
      response <- waiJsonRequest manager requestId "GET" url "" extraHeaders
      hresStatusCode response `shouldBe` 400
      lookupHeaderText "x-request-id" (hresHeaders response) `shouldBe` Just requestId
      lookupNestedText ["error", "code"] (hresBody response) `shouldBe` Just "validation_failed"
      errorDetailCode (hresBody response) `shouldBe` Just "duplicate_query_param"
      lookupNestedText ["error", "request_id"] (hresBody response) `shouldBe` Just requestId

assertMalformedUtf8QueryErrors :: Manager -> IO ()
assertMalformedUtf8QueryErrors manager =
  forM_
    [ ( "bad-query-value"
      , "http://127.0.0.1:7377/terrain/hex?q=%FF&r=0"
      , []
      )
    , ( "bad-query-key"
      , "http://127.0.0.1:7377/terrain/hex?%FF=0&r=0"
      , []
      )
    , ( "bad-events-stream-value"
      , "http://127.0.0.1:7377/events?stream=%FF"
      , [("Accept", "text/event-stream")]
      )
    ] $ \(caseName, url, extraHeaders) -> do
      let requestId = "wai-query-utf8-" <> caseName
      response <- waiJsonRequest manager requestId "GET" url "" extraHeaders
      hresStatusCode response `shouldBe` 400
      lookupHeaderText "x-request-id" (hresHeaders response) `shouldBe` Just requestId
      lookupNestedText ["error", "code"] (hresBody response) `shouldBe` Just "validation_failed"
      errorDetailCode (hresBody response) `shouldBe` Just "invalid_query_param"
      lookupNestedText ["error", "request_id"] (hresBody response) `shouldBe` Just requestId

assertWaiCommandRoutesAbsent :: Manager -> IO ()
assertWaiCommandRoutesAbsent manager = do
  let authCases =
        [ ("no-token-config", 7380, [])
        , ("token-missing", 7381, [])
        , ("token-invalid", 7381, [("Authorization", "Bearer wrong")])
        , ("token-valid", 7381, [("Authorization", "Bearer secret")])
        ]
      routeCases =
        [ ("known-get-state", "POST", "/commands/get_state", LBS.fromStrict (BS.replicate 9 123), [("Content-Type", "text/plain")])
        , ("known-set-seed", "POST", "/commands/set_seed", "{\"seed\":777}", [jsonContentTypeHeader])
        , ("unknown", "POST", "/commands/no_such_method", "{", [jsonContentTypeHeader])
        , ("method-mismatch", "GET", "/commands/get_state", "{}", [("Content-Type", "application/x-json")])
        ]
  forM_ authCases $ \(authName, port, authHeaders) ->
    forM_ routeCases $ \(routeName, requestMethod, path, rawBody, bodyHeaders) -> do
      let requestId = Text.intercalate "-" ["wai-commands-absent", authName, routeName]
          url = "http://127.0.0.1:" <> show port <> path
      response <- waiRawRequest manager requestId requestMethod url rawBody
        (bodyHeaders <> authHeaders)
      assertCommandRouteAbsent (routeName <> " / " <> authName) requestId response

commandDispatchGuard :: IORef [Text] -> AppService
commandDispatchGuard serviceCalls = headlessAppService
  { appState = (appState headlessAppService)
      { stateGetState = trappingServiceHandler serviceCalls "get_state" stateGetStateOperation
      }
  , appUi = (appUi headlessAppService)
      { uiSetSeed = trappingServiceHandler serviceCalls "set_seed" uiSetSeedOperation
      }
  }

trappingServiceHandler
  :: IORef [Text]
  -> Text
  -> TypedServiceOperation request response
  -> ServiceHandler request response
trappingServiceHandler serviceCalls methodName operation =
  rawServiceHandler operation $ \_ _ -> do
    atomicModifyIORef' serviceCalls (\calls -> (methodName : calls, ()))
    throwIO (userError ("removed /commands route invoked AppService method " <> Text.unpack methodName))

assertCommandSideEffectsAbsent
  :: HeadlessApp
  -> HttpPublicationBaseline
  -> HttpResponse
  -> IORef [Text]
  -> Expectation
assertCommandSideEffectsAbsent app baseline eventsBefore serviceCalls = do
  calls <- readIORef serviceCalls
  calls `shouldBe` []
  let handles = httpHandles app
  committed <- readCommittedRenderSnapshot (ahSnapshotVersionRef handles)
  committed `shouldBe` (hpbVersion baseline, hpbSnapshot baseline)
  liveUi <- getUiSnapshot (ahUiHandle handles)
  liveUi `shouldBe` rsUi (hpbSnapshot baseline)
  atlasJobs <- drainAtlasJobs (ahAtlasManagerHandle handles)
  length atlasJobs `shouldBe` 0
  eventsAfter <- request app (mkRequest "GET" ["events"])
  lookupValue "events" (hresBody eventsAfter)
    `shouldBe` lookupValue "events" (hresBody eventsBefore)

assertCommandRouteAbsent :: Text -> Text -> HttpResponse -> Expectation
assertCommandRouteAbsent caseName requestId response =
  if actual == expected
    then pure ()
    else expectationFailure
      ("removed /commands regression for " <> Text.unpack caseName
        <> "\nexpected status/request-id/body: " <> show expected
        <> "\n but got status/request-id/body: " <> show actual)
  where
    actual =
      ( hresStatusCode response
      , lookupHeaderText "x-request-id" (hresHeaders response)
      , hresBody response
      )
    expected =
      ( 404
      , Just requestId
      , object
          [ "error" .= object
              [ "code" .= ("not_found" :: Text)
              , "message" .= ("route not found" :: Text)
              , "details" .= ([] :: [Value])
              , "request_id" .= requestId
              ]
          ]
      )

assertUnauthorizedProtectedTransportErrors :: Manager -> IO ()
assertUnauthorizedProtectedTransportErrors manager = do
  let cases :: [(Text, LBS.ByteString, [Header])]
      cases =
        [ ("wai-auth-precedence-malformed", "{", [jsonContentTypeHeader])
        , ("wai-auth-precedence-oversize", LBS.fromStrict (BS.replicate 9 123), [jsonContentTypeHeader])
        , ("wai-auth-precedence-wrong-content-type", "{}", [("Content-Type", "text/plain")])
        ]
  forM_ cases $ \(requestId, rawBody, extraHeaders) -> do
    response <- waiRawRequest manager requestId "POST" "http://127.0.0.1:7374/screenshots" rawBody extraHeaders
    assertUnauthorizedResponse requestId response

assertUnauthorizedResponse :: Text -> HttpResponse -> Expectation
assertUnauthorizedResponse requestId response = do
  hresStatusCode response `shouldBe` 401
  lookupHeaderText "x-request-id" (hresHeaders response) `shouldBe` Just requestId
  lookupNestedText ["error", "code"] (hresBody response) `shouldBe` Just "unauthorized"
  lookupNestedText ["error", "request_id"] (hresBody response) `shouldBe` Just requestId

assertWaiRequestBodySizeLimits :: Manager -> IO ()
assertWaiRequestBodySizeLimits manager = do
  let underRequestId = "wai-body-limit-under"
  under <- waiJsonRequest manager underRequestId "POST" "http://127.0.0.1:7378/screenshots" "{}" []
  assertBodyPolicyResponse underRequestId ExpectBodyPolicySuccess under

  let exactRequestId = "wai-body-limit-exact"
  exact <- waiJsonRequest manager exactRequestId "POST" "http://127.0.0.1:7378/ui/seed" "{\"seed\":1}" []
  assertBodyPolicyResponse exactRequestId ExpectBodyPolicySuccess exact

  let knownRequestId = "wai-body-limit-known"
  known <- waiJsonRequest manager knownRequestId "POST" "http://127.0.0.1:7378/ui/seed"
    (LBS.fromStrict (BS.replicate 11 123)) []
  assertPayloadTooLargeResponse knownRequestId known

  let streamedRequestId = "wai-body-limit-streamed"
  streamed <- waiJsonRequestWithBody manager streamedRequestId "POST" "http://127.0.0.1:7378/ui/seed"
    (chunkedRequestBody [BS.replicate 5 123, BS.replicate 6 123]) []
  assertPayloadTooLargeResponse streamedRequestId streamed

  let noBodyRequestId = "wai-body-limit-no-body"
  noBody <- waiJsonRequest manager noBodyRequestId "GET" "http://127.0.0.1:7378/state"
    (LBS.fromStrict (BS.replicate 11 123)) []
  assertBodyPolicyResponse noBodyRequestId
    (ExpectBodyPolicyError 400 "validation_failed" "unexpected_body")
    noBody

assertPayloadTooLargeResponse :: Text -> HttpResponse -> Expectation
assertPayloadTooLargeResponse requestId response = do
  hresStatusCode response `shouldBe` 413
  lookupHeaderText "x-request-id" (hresHeaders response) `shouldBe` Just requestId
  lookupNestedText ["error", "code"] (hresBody response) `shouldBe` Just "payload_too_large"
  lookupNestedText ["error", "request_id"] (hresBody response) `shouldBe` Just requestId

chunkedRequestBody :: [BS.ByteString] -> RequestBody
chunkedRequestBody chunks = RequestBodyStreamChunked $ \needsPopper -> do
  remaining <- newIORef chunks
  needsPopper $ atomicModifyIORef' remaining popChunk
  where
    popChunk [] = ([], BS.empty)
    popChunk (chunk:rest) = (rest, chunk)

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
  assertWaiJsonContentTypePolicy manager

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

assertWaiJsonContentTypePolicy :: Manager -> IO ()
assertWaiJsonContentTypePolicy manager = do
  let optionalUrl = "http://127.0.0.1:7376/screenshots"
      requiredUrl = "http://127.0.0.1:7376/ui/seed"
      noBodyUrl = "http://127.0.0.1:7376/state"

  optionalMissing <- waiRawRequest manager "wai-content-type-optional-missing" "POST" optionalUrl "{}" []
  assertUnsupportedMediaTypeResponse "wai-content-type-optional-missing" optionalMissing

  optionalWrong <- waiRawRequest manager "wai-content-type-optional-wrong" "POST" optionalUrl "{}"
    [("Content-Type", "text/plain")]
  assertUnsupportedMediaTypeResponse "wai-content-type-optional-wrong" optionalWrong

  optionalWithParams <- waiRawRequest manager "wai-content-type-optional-params" "POST" optionalUrl "{}"
    [("Content-Type", "Application/JSON; Charset=utf-8")]
  assertBodyPolicyResponse "wai-content-type-optional-params" ExpectBodyPolicySuccess optionalWithParams

  optionalEmpty <- waiRawRequest manager "wai-content-type-optional-empty" "POST" optionalUrl "" []
  assertBodyPolicyResponse "wai-content-type-optional-empty" ExpectBodyPolicySuccess optionalEmpty

  requiredMissing <- waiRawRequest manager "wai-content-type-required-missing" "POST" requiredUrl "{\"seed\":987}" []
  assertUnsupportedMediaTypeResponse "wai-content-type-required-missing" requiredMissing

  requiredWrong <- waiRawRequest manager "wai-content-type-required-wrong" "POST" requiredUrl "{\"seed\":987}"
    [("Content-Type", "application/x-json")]
  assertUnsupportedMediaTypeResponse "wai-content-type-required-wrong" requiredWrong

  requiredWithParams <- waiRawRequest manager "wai-content-type-required-params" "POST" requiredUrl "{\"seed\":987}"
    [("Content-Type", "application/json; charset=utf-8")]
  assertBodyPolicyResponse "wai-content-type-required-params" ExpectBodyPolicySuccess requiredWithParams

  requiredEmpty <- waiRawRequest manager "wai-content-type-required-empty" "POST" requiredUrl "" []
  assertBodyPolicyResponse "wai-content-type-required-empty"
    (ExpectBodyPolicyError 400 "validation_failed" "missing_body")
    requiredEmpty

  noBodyWrong <- waiRawRequest manager "wai-content-type-no-body-wrong" "GET" noBodyUrl "{}"
    [("Content-Type", "text/plain")]
  assertBodyPolicyResponse "wai-content-type-no-body-wrong"
    (ExpectBodyPolicyError 400 "validation_failed" "unexpected_body")
    noBodyWrong

assertUnsupportedMediaTypeResponse :: Text -> HttpResponse -> Expectation
assertUnsupportedMediaTypeResponse requestId response = do
  hresStatusCode response `shouldBe` 415
  lookupHeaderText "x-request-id" (hresHeaders response) `shouldBe` Just requestId
  lookupNestedText ["error", "code"] (hresBody response) `shouldBe` Just "unsupported_media_type"
  lookupNestedText ["error", "request_id"] (hresBody response) `shouldBe` Just requestId

waiBodyPolicyRequest :: Manager -> Text -> BS.ByteString -> String -> LBS.ByteString -> IO HttpResponse
waiBodyPolicyRequest manager requestId requestMethod url rawBody =
  waiJsonRequest manager requestId requestMethod url rawBody []

waiJsonRequest
  :: Manager
  -> Text
  -> BS.ByteString
  -> String
  -> LBS.ByteString
  -> [Header]
  -> IO HttpResponse
waiJsonRequest manager requestId requestMethod url rawBody extraHeaders =
  waiJsonRequestWithBody manager requestId requestMethod url (RequestBodyLBS rawBody) extraHeaders

waiJsonRequestWithBody
  :: Manager
  -> Text
  -> BS.ByteString
  -> String
  -> RequestBody
  -> [Header]
  -> IO HttpResponse
waiJsonRequestWithBody manager requestId requestMethod url rawBody extraHeaders =
  waiRawRequestWithBody manager requestId requestMethod url rawBody (jsonContentTypeHeader : extraHeaders)

jsonContentTypeHeader :: Header
jsonContentTypeHeader = ("Content-Type", "application/json")

waiRawRequest
  :: Manager
  -> Text
  -> BS.ByteString
  -> String
  -> LBS.ByteString
  -> [Header]
  -> IO HttpResponse
waiRawRequest manager requestId requestMethod url rawBody extraHeaders =
  waiRawRequestWithBody manager requestId requestMethod url (RequestBodyLBS rawBody) extraHeaders

waiRawRequestWithBody
  :: Manager
  -> Text
  -> BS.ByteString
  -> String
  -> RequestBody
  -> [Header]
  -> IO HttpResponse
waiRawRequestWithBody manager requestId requestMethod url rawBody extraHeaders = do
  req0 <- parseRequest url
  let req = req0
        { method = requestMethod
        , requestBody = rawBody
        , requestHeaders = ("X-Request-Id", TextEncoding.encodeUtf8 requestId) : extraHeaders
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
