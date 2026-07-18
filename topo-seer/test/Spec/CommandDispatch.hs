{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for 'Seer.Command.Dispatch.dispatchCommand' and the handler
-- modules it delegates to.
--
-- Each test spins up a real 'ActorSystem', constructs 'ActorHandles'
-- and a 'CommandContext', dispatches a 'SeerCommand', and asserts on
-- the resulting 'SeerResponse'.
module Spec.CommandDispatch (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, newMVar, takeMVar)
import Control.Exception (bracket, catch, finally)
import Control.Monad (forM_, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (Value(..), object, (.=), Key)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import qualified Data.IntMap.Strict as IntMap
import Data.IORef (newIORef, readIORef)
import qualified Data.Map.Strict as Map
import Data.List (nub, sort)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector.Unboxed as U
import System.Directory (createDirectoryIfMissing, doesFileExist, getTemporaryDirectory, removeFile, removePathForcibly)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import Hyperspace.Actor (ActorHandle, ActorSystem, Protocol, get, newActorSystem, replyTo, shutdownActorSystem)
import Test.Hspec

import Actor.AtlasManager (AtlasJob(..), AtlasManager, ajViewMode, atlasJobsForSelection, atlasJobsForSelectionTransition, drainAtlasJobs)
import Actor.Data (Data, DataSnapshot(..), TerrainGeoContext(..), TerrainSnapshot(..), defaultTerrainGeoContext, getDataSnapshot, getTerrainSnapshot, setOverlayStoreData, setTerrainChunkCount, setTerrainChunkData, setTerrainGeoContextData)
import Actor.Log (Log, LogEntry(..), LogLevel(..), LogSnapshot(..), getLogSnapshot, newLogSnapshotRef, setLogSnapshotRef)
import Actor.PluginManager (LoadedPlugin(..), PluginManager, discoverPlugins, getLoadedPlugins)
import Actor.Simulation (Simulation)
import Actor.SnapshotReceiver
  ( RenderSnapshot(..)
  , SnapshotVersion(..)
  , newDataSnapshotRef
  , newTerrainSnapshotRef
  , newRenderSnapshotVersionRef
  , dataAndTerrainSnapshotUpdate
  , publishSnapshot
  , readCommittedRenderSnapshot
  , readDataSnapshot
  , readSnapshotVersion
  , readTerrainSnapshot
  , writeTerrainSnapshot
  )
import Actor.Terrain (Terrain, TerrainReplyOps)
import Actor.UI
  ( BaseViewMode(..)
  , ConfigTab(..)
  , DataBrowserState(..)
  , emptyDataBrowserState
  , LayeredViewState(..)
  , LeftTab(..)
  , SkyOverlayMode(..)
  , Ui
  , UiState(..)
  , UiMenuMode(..)
  , ViewMode(..)
  , WeatherBasis(..)
  , defaultLayeredViewState
  , effectiveViewSelection
  , getUiSnapshot
  , newUiSnapshotRef
  , setUiDayNightEnabled
  , setUiEditor
  , setUiGenerating
  , setUiSnapshotRef
  , uiViewMode
  )
import Actor.UI.Setters
  ( setUiConfigTab
  , setUiContextHex
  , setUiContextPos
  , setUiDataBrowser
  , setUiLeftTab
  , setUiMenuMode
  , setUiPresetFilter
  , setUiPresetInput
  , setUiPresetList
  , setUiPresetSelected
  , setUiSeedEditing
  , setUiSeedInput
  , setUiPluginExpanded
  , setUiPluginNames
  , setUiPluginParamSpecs
  , setUiShowConfig
  , setUiShowLeftPanel
  , setUiWorldFilter
  , setUiWorldList
  , setUiWorldSaveInput
  , setUiWorldSelected
  )
import Actor.UiActions (ActorHandles(..), UiActions)
import Actor.UiActions.Command (UiAction(..), UiActionRequest(..), runUiAction)

import Seer.Command.AppServiceAdapter
  ( commandAppService
  , dispatchAppServiceCommand
  , runAppServiceOperation
  )
import Seer.Command.Dispatch (CommandContext(..), dispatchCommand)
import Seer.DataBrowser.Executor
  ( newDataBrowserExecutor
  , shutdownDataBrowserExecutor
  )
import Seer.OverlayInspector.Executor
  ( newOverlayInspectorExecutor
  , shutdownOverlayInspectorExecutor
  , waitOverlayInspectorExecutorIdle
  )
import Seer.Config.Snapshot (ConfigSnapshot(..), snapshotDir)
import Seer.Editor.History (emptyHistory)
import Seer.Render.ZoomStage (ZoomStage(..), orderedZoomStagesForZoom, stageForZoom)
import Seer.System.Cache (RenderCacheState, initialRenderCacheState)
import Seer.System.Snapshot (SnapshotPollEnv(..), pollRenderSnapshot)
import Seer.Editor.Types (BrushSettings(..), EditorState(..), EditorTool(..))
import Seer.Screenshot.Request
  ( ScreenshotDelivery(..)
  , ScreenshotResultError(..)
  , claimScreenshotRequest
  , deliverScreenshotRequest
  , newScreenshotRequestRef
  , screenshotRequestActive
  , shutdownScreenshotRequestRef
  , submitScreenshotRequest
  )
import Seer.Screenshot.Storage (ScreenshotStoragePolicy(..))
import Seer.Service.Screenshot (rendererScreenshotHandlerWithDeadline)
import Seer.World.Persist (deleteNamedWorld, listWorlds)
import Seer.World.Persist.Types (WorldSaveManifest(..))
import Seer.Service.AppService
  ( AppService(..)
  , ConfigListPresetsRequest(..)
  , ConfigListPresetsResponse(..)
  , ConfigService(..)
  , TerrainService(..)
  , WorldGenerateRequest(..)
  , WorldGenerateResponse(..)
  , WorldListRequest(..)
  , WorldListResponse(..)
  , WorldService(..)
  , UiSetSeedRequest(..)
  , UiSetSeedResponse(..)
  , appServiceOperationMethods
  , appUi
  , configListPresetsOperation
  , terrainGetOverlaysOperation
  , uiSetSeed
  , uiSetSeedOperation
  , worldGenerateOperation
  , worldListOperation
  )
import Seer.Service.Context (ServiceContext(..), unavailableNestedServiceRunner)
import Seer.Service.Types
  ( ServiceError(..)
  , ServiceErrorDetail(..)
  , ServiceErrorKind(..)
  , ServiceRequest(..)
  , ServiceResponse(..)
  , ServiceResult
  , adaptTypedServiceHandler
  , rawServiceHandler
  , runServiceHandler
  , serviceErrorCode
  , serviceErrorDetails
  , serviceErrorHTTPStatus
  , serviceErrorKind
  , serviceErrorText
  )
import Topo.WorldGen
  ( WorldGenConfig
  , archipelagoWorldGenConfig
  , aridWorldGenConfig
  , continentalWorldGenConfig
  , inlandSeaWorldGenConfig
  , largeOceanWorldGenConfig
  , lushWorldGenConfig
  )
import Topo
  ( ClimateChunk(..)
  , WeatherChunk(..)
  , WorldConfig(..)
  , chunkIdFromCoord
  , chunkTileCount
  , defaultWeatherConfig
  , emptyTerrainChunk
  , weatherNormalsChunkFromClimate
  , weatherNormalsChunkToOverlay
  , weatherNormalsOverlaySchema
  )
import Topo.Calendar (WorldTime(..), simulationTickSeconds)
import Topo.Command.Types (SeerCommand(..), SeerResponse(..))
import Topo.Overlay (Overlay(..), OverlayData(..), OverlayProvenance(..), emptyOverlayStore, insertOverlay)
import Topo.Pipeline.Stage (StageId(..))
import Topo.Plugin.RPC.Manifest
  ( RPCParamSpec(..)
  , RPCParamType(..)
  , manifestV3
  )
import Topo.Plugin.RPC.Protocol (currentProtocolVersion)
import Topo.Types (ChunkCoord(..), ChunkId(..), TerrainChunk(..))

spec :: Spec
spec = describe "CommandDispatch" $ do

  describe "AppService adapter envelope" $ do
    it "preserves request ids for success, handler errors, and unknown commands" $ withCtx $ \ctx -> do
      ok <- dispatchWithId ctx 42 "get_state" Null
      srId ok `shouldBe` 42
      srSuccess ok `shouldBe` True

      handlerErr <- dispatchWithId ctx 43 "get_slider" (object ["name" .= ("NoSuchSlider" :: String)])
      srId handlerErr `shouldBe` 43
      srSuccess handlerErr `shouldBe` False

      unknown <- dispatchWithId ctx 44 "no_such_command" Null
      srId unknown `shouldBe` 44
      srSuccess unknown `shouldBe` False

    it "returns typed unknown-method service errors without parsing message text" $ withCtx $ \ctx -> do
      result <- runService ctx "no_such_command" Null
      case result of
        Left err -> do
          serviceErrorCode err `shouldBe` "unknown_method"
          serviceErrorKind err `shouldBe` ServiceErrorNotFound
          case serviceErrorDetails err of
            [detail] -> do
              serviceErrorDetailPath detail `shouldBe` ["method"]
              serviceErrorDetailCode detail `shouldBe` "unknown_method"
            details -> expectationFailure ("expected one unknown-method detail, got: " <> show details)
        other -> expectationFailure ("expected unknown-method service error, got: " <> show other)

    it "exposes representative successful operations as direct service handlers" $ withCtx $ \ctx -> do
      stateResult <- runService ctx "get_state" Null
      case stateResult of
        Right (ServiceResponse body) -> do
          lookupKey "seed" body `shouldSatisfy` (/= Nothing)
          lookupKey "view_mode" body `shouldSatisfy` (/= Nothing)
        Left err -> expectationFailure ("expected get_state service success, got: " <> show err)

      seedResult <- runService ctx "set_seed" (object ["seed" .= (987 :: Int)])
      case seedResult of
        Right (ServiceResponse body) -> lookupKey "seed" body `shouldBe` Just (Number 987)
        Left err -> expectationFailure ("expected set_seed service success, got: " <> show err)

    it "classifies handler-originated domain failures as service errors" $ withCtx $ \ctx -> do
      result <- runService ctx "get_slider" (object ["name" .= ("NoSuchSlider" :: String)])
      case result of
        Left (ServiceNotFound msg) -> msg `shouldSatisfy` (not . Text.null)
        other -> expectationFailure ("expected ServiceNotFound, got: " <> show other)

      chunkResult <- runService ctx "get_chunk_summary" (object ["chunk" .= (999 :: Int)])
      case chunkResult of
        Left (ServiceNotFound msg) -> msg `shouldSatisfy` Text.isInfixOf "chunk"
        other -> expectationFailure ("expected chunk ServiceNotFound, got: " <> show other)

    it "returns structured plugin parameter update errors from the AppService boundary" $ withPluginCtx $ \ctx -> do
      unknownPlugin <- runService ctx "set_plugin_param" (object
        [ "plugin" .= ("missing" :: String)
        , "param" .= ("enabled" :: String)
        , "value" .= Bool True
        ])
      case unknownPlugin of
        Left (ServiceNotFound msg) -> msg `shouldSatisfy` Text.isInfixOf "unknown plugin"
        other -> expectationFailure ("expected plugin ServiceNotFound, got: " <> show other)

      unknownParam <- runService ctx "set_plugin_param" (object
        [ "plugin" .= ("example" :: String)
        , "param" .= ("missing" :: String)
        , "value" .= Bool True
        ])
      assertSingleValidationDetail unknownParam ["param"] "unknown_param"

      wrongType <- runService ctx "set_plugin_param" (object
        [ "plugin" .= ("example" :: String)
        , "param" .= ("enabled" :: String)
        , "value" .= String "yes"
        ])
      assertSingleValidationDetail wrongType ["value"] "invalid_type"

    it "returns structured service validation errors before handler envelopes" $ withCtx $ \ctx -> do
      result <- runService ctx "set_seed" Null
      case result of
        Left err -> do
          serviceErrorKind err `shouldBe` ServiceErrorInvalidRequest
          serviceErrorCode err `shouldBe` "validation_failed"
          case serviceErrorDetails err of
            [detail] -> do
              serviceErrorDetailPath detail `shouldBe` ["seed"]
              serviceErrorDetailCode detail `shouldBe` "missing_field"
              serviceErrorDetailMessage detail `shouldSatisfy` Text.isInfixOf "seed"
            details -> expectationFailure ("expected one validation detail, got: " <> show details)
        other -> expectationFailure ("expected structured validation error, got: " <> show other)

    it "returns structured validation details for invalid fields and required object bodies" $ withCtx $ \ctx -> do
      negativeSeed <- runService ctx "set_seed" (object ["seed" .= ((-1) :: Int)])
      assertSingleValidationDetail negativeSeed ["seed"] "invalid_field"

      missingEnumType <- runService ctx "get_enums" Null
      assertSingleValidationDetail missingEnumType ["type"] "missing_field"

      brushWithoutObject <- runService ctx "editor_set_brush" Null
      assertSingleValidationDetail brushWithoutObject [] "invalid_body"

    it "validates commandAppService handlers at the service-record boundary" $ withCtx $ \ctx -> do
      result <- runServiceHandler (uiSetSeed (appUi commandAppService)) (serviceContextFromCommand ctx) (ServiceRequest (Just Null))
      assertSingleValidationDetail result ["seed"] "missing_field"

    it "validates adapted typed handlers before caller-supplied decoders" $ withCtx $ \ctx -> do
      let app = commandAppService
            { appUi = (appUi commandAppService)
                { uiSetSeed = adaptTypedServiceHandler
                    uiSetSeedOperation
                    (const (Left (ServiceInvalidRequest "decoder should not run")))
                    (\response -> object ["seed" .= uiSetSeedResponseValue response])
                    (\_ request -> pure (Right (UiSetSeedResponse (uiSetSeedRequestValue request))))
                }
            }
      result <- runAppServiceOperation app ctx "set_seed" Null
      assertSingleValidationDetail result ["seed"] "missing_field"

    it "translates structured service validation errors back to command errors" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "set_seed" Null
      srSuccess rsp `shouldBe` False
      case srError rsp of
        Just msg -> msg `shouldSatisfy` Text.isInfixOf "seed"
        Nothing -> expectationFailure "expected command error text"

    it "has a direct service-level case for every public operation" $ do
      let coveredMethods = map serviceCaseMethod serviceOperationCases
      sort coveredMethods `shouldBe` sort appServiceOperationMethods
      coveredMethods `shouldBe` nub coveredMethods

    it "executes every public operation through the AppService handler surface" $ withCtx $ \ctx ->
      forM_ serviceOperationCases $ \testCase -> do
        serviceCaseSetup testCase ctx
        result <- runService ctx (serviceCaseMethod testCase) (serviceCaseParams testCase)
        assertServiceOutcome testCase result

  describe "shared keyboard intents" $ do
    it "uses filtered modal navigation and shared modal text semantics" $ withCtx $ \ctx -> do
      let uiH = ahUiHandle (ccActorHandles ctx)
      setUiPresetList uiH ["alpha", "beta", "alpine"]
      setUiPresetFilter uiH "alp"
      setUiPresetSelected uiH 0
      setUiMenuMode uiH MenuPresetLoad
      _ <- getUiSnapshot uiH

      forM_ ["down", "up", "down"] $ \key -> do
        rsp <- dispatch ctx "send_key" (object ["key" .= (key :: Text)])
        srSuccess rsp `shouldBe` True
      uiAfterNav <- getUiSnapshot uiH
      uiPresetSelected uiAfterNav `shouldBe` 1

      setUiPresetInput uiH "name"
      setUiMenuMode uiH MenuPresetSave
      _ <- dispatch ctx "send_key" (object ["key" .= ("space" :: Text)])
      _ <- dispatch ctx "send_key" (object ["key" .= ("X" :: Text)])
      _ <- dispatch ctx "send_key" (object ["key" .= ("[" :: Text)])
      uiAfterText <- getUiSnapshot uiH
      uiPresetInput uiAfterText `shouldBe` "name X["

      setUiPresetList uiH []
      setUiPresetFilter uiH "missing"
      setUiPresetSelected uiH 0
      setUiMenuMode uiH MenuPresetLoad
      rejected <- dispatch ctx "dialog_confirm" Null
      srSuccess rejected `shouldBe` False
      uiAfterRejected <- getUiSnapshot uiH
      uiMenuMode uiAfterRejected `shouldBe` MenuPresetLoad

    it "executes real preset persistence before closing confirmation dialogs" $ withCtx $ \ctx -> do
      let uiH = ahUiHandle (ccActorHandles ctx)
          presetName = "pi-input-intent-confirm"
      dir <- snapshotDir
      let presetPath = dir </> Text.unpack presetName <> ".json"
          cleanup = do
            exists <- doesFileExist presetPath
            when exists (removeFile presetPath)
      cleanup
      (do
          setUiPresetInput uiH presetName
          setUiMenuMode uiH MenuPresetSave
          _ <- getUiSnapshot uiH
          rsp <- dispatch ctx "dialog_confirm" Null
          srSuccess rsp `shouldBe` True
          doesFileExist presetPath `shouldReturn` True
          ui <- getUiSnapshot uiH
          uiMenuMode ui `shouldBe` MenuNone)
        `finally` cleanup

    it "executes real world save and load operations before closing dialogs" $ withCtx $ \ctx -> do
      let uiH = ahUiHandle (ccActorHandles ctx)
          worldName = "__topo_input_intent_confirm__"
      _ <- deleteNamedWorld worldName
      (do
          _ <- writeReadyTerrainData ctx
          setUiWorldSaveInput uiH worldName
          setUiMenuMode uiH MenuWorldSave
          saveRsp <- dispatch ctx "dialog_confirm" Null
          srSuccess saveRsp `shouldBe` True
          uiAfterSave <- getUiSnapshot uiH
          uiMenuMode uiAfterSave `shouldBe` MenuNone

          worlds <- listWorlds
          map wsmName worlds `shouldSatisfy` (worldName `elem`)
          setUiWorldList uiH worlds
          setUiWorldFilter uiH worldName
          setUiWorldSelected uiH 0
          setUiMenuMode uiH MenuWorldLoad
          loadRsp <- dispatch ctx "dialog_confirm" Null
          srSuccess loadRsp `shouldBe` True
          uiAfterLoad <- getUiSnapshot uiH
          uiMenuMode uiAfterLoad `shouldBe` MenuNone
          uiWorldName uiAfterLoad `shouldBe` worldName)
        `finally` (deleteNamedWorld worldName >> pure ())

    it "guards saved-world deletion across mouse and keyboard success, cancel, no-selection, and failure paths" $ withCtx $ \ctx -> do
      let uiH = ahUiHandle (ccActorHandles ctx)
          worldName = "__topo_world_dialog_delete__"
          staleName = "__topo_world_dialog_delete_stale__"
          siblingName = "__topo_world_dialog_delete_sibling__"
          click wid = dispatch ctx "click_widget" (object ["widget_id" .= (wid :: Text)])
          saved name = any ((== name) . wsmName) <$> listWorlds
          save name = do
            setUiWorldSaveInput uiH name
            setUiMenuMode uiH MenuWorldSave
            dispatch ctx "dialog_confirm" Null
          openFor name = do
            setUiMenuMode uiH MenuEscape
            _ <- getUiSnapshot uiH
            opened <- click "WidgetMenuLoad"
            srSuccess opened `shouldBe` True
            setUiWorldFilter uiH name
            setUiWorldSelected uiH 0
            _ <- getUiSnapshot uiH
            pure ()
          cleanup = do
            _ <- deleteNamedWorld worldName
            _ <- deleteNamedWorld staleName
            _ <- deleteNamedWorld siblingName
            pure ()
      cleanup
      (do
          _ <- writeReadyTerrainData ctx
          saveRsp <- save worldName
          srSuccess saveRsp `shouldBe` True
          saved worldName `shouldReturn` True

          openFor "no matching saved world"
          noSelection <- dispatch ctx "send_key" (object ["key" .= ("delete" :: Text)])
          srSuccess noSelection `shouldBe` False
          noSelectionUi <- getUiSnapshot uiH
          uiWorldDeleteConfirm noSelectionUi `shouldBe` False
          saved worldName `shouldReturn` True

          setUiWorldFilter uiH worldName
          setUiWorldSelected uiH 0
          _ <- getUiSnapshot uiH
          mouseRequest <- click "WidgetWorldDelete"
          srSuccess mouseRequest `shouldBe` True
          requestedUi <- getUiSnapshot uiH
          uiWorldDeleteConfirm requestedUi `shouldBe` True
          uiWorldDeleteTarget requestedUi `shouldBe` Just worldName
          mouseCancel <- click "WidgetWorldDeleteCancel"
          srSuccess mouseCancel `shouldBe` True
          uiAfterMouseCancel <- getUiSnapshot uiH
          uiMenuMode uiAfterMouseCancel `shouldBe` MenuWorldLoad
          uiWorldDeleteConfirm uiAfterMouseCancel `shouldBe` False
          uiWorldDeleteTarget uiAfterMouseCancel `shouldBe` Nothing
          saved worldName `shouldReturn` True

          keyboardRequest <- dispatch ctx "send_key" (object ["key" .= ("delete" :: Text)])
          srSuccess keyboardRequest `shouldBe` True
          uiWorldDeleteConfirm <$> getUiSnapshot uiH `shouldReturn` True
          keyboardCancel <- dispatch ctx "send_key" (object ["key" .= ("escape" :: Text)])
          srSuccess keyboardCancel `shouldBe` True
          uiAfterKeyboardCancel <- getUiSnapshot uiH
          uiMenuMode uiAfterKeyboardCancel `shouldBe` MenuWorldLoad
          uiWorldDeleteConfirm uiAfterKeyboardCancel `shouldBe` False
          uiWorldDeleteTarget uiAfterKeyboardCancel `shouldBe` Nothing
          saved worldName `shouldReturn` True

          _ <- dispatch ctx "send_key" (object ["key" .= ("delete" :: Text)])
          keyboardConfirm <- dispatch ctx "send_key" (object ["key" .= ("enter" :: Text)])
          srSuccess keyboardConfirm `shouldBe` True
          uiAfterDelete <- getUiSnapshot uiH
          uiMenuMode uiAfterDelete `shouldBe` MenuWorldLoad
          uiWorldDeleteConfirm uiAfterDelete `shouldBe` False
          uiWorldDeleteTarget uiAfterDelete `shouldBe` Nothing
          uiWorldDeleteError uiAfterDelete `shouldBe` Nothing
          uiWorldSelected uiAfterDelete `shouldBe` 0
          map wsmName (uiWorldList uiAfterDelete) `shouldNotContain` [worldName]
          saved worldName `shouldReturn` False

          saveStale <- save staleName
          srSuccess saveStale `shouldBe` True
          saveSibling <- save siblingName
          srSuccess saveSibling `shouldBe` True
          openFor staleName
          _ <- click "WidgetWorldDelete"
          uiWorldDeleteTarget <$> getUiSnapshot uiH `shouldReturn` Just staleName
          externalDelete <- dispatch ctx "delete_world" (object ["name" .= staleName])
          srSuccess externalDelete `shouldBe` True
          uiWorldDeleteTarget <$> getUiSnapshot uiH `shouldReturn` Just staleName
          failed <- click "WidgetWorldDeleteConfirm"
          srSuccess failed `shouldBe` False
          uiAfterFailure <- getUiSnapshot uiH
          uiMenuMode uiAfterFailure `shouldBe` MenuWorldLoad
          uiWorldDeleteConfirm uiAfterFailure `shouldBe` True
          uiWorldDeleteTarget uiAfterFailure `shouldBe` Just staleName
          uiWorldDeleteError uiAfterFailure
            `shouldSatisfy` maybe False (\message ->
              "Delete failed:" `Text.isPrefixOf` message && staleName `Text.isInfixOf` message)
          saved siblingName `shouldReturn` True
          cancelFailure <- click "WidgetWorldDeleteCancel"
          srSuccess cancelFailure `shouldBe` True
          uiAfterFailureCancel <- getUiSnapshot uiH
          uiWorldDeleteConfirm uiAfterFailureCancel `shouldBe` False
          uiWorldDeleteTarget uiAfterFailureCancel `shouldBe` Nothing
          uiWorldDeleteError uiAfterFailureCancel `shouldBe` Nothing)
        `finally` cleanup

    it "parses and commits seed input through the real set_seed operation" $ withCtx $ \ctx -> do
      let uiH = ahUiHandle (ccActorHandles ctx)
      setUiSeedInput uiH "-123"
      setUiSeedEditing uiH True
      _ <- getUiSnapshot uiH
      rsp <- dispatch ctx "dialog_confirm" Null
      srSuccess rsp `shouldBe` True
      ui <- getUiSnapshot uiH
      uiSeed ui `shouldBe` 123
      uiSeedEditing ui `shouldBe` False

    it "applies editor, global, layered-view, modifier, and no-effect outcomes" $ withCtx $ \ctx -> do
      let uiH = ahUiHandle (ccActorHandles ctx)
      forM_
        [ ("c", object ["key" .= ("C" :: Text)], Just "applied")
        , ("view", object ["key" .= ("2" :: Text)], Just "applied")
        , ("space", object ["key" .= ("space" :: Text)], Just "no_effect")
        ] $ \(_, params, expectedOutcome) -> do
          rsp <- dispatch ctx "send_key" params
          srSuccess rsp `shouldBe` True
          lookupKey "outcome" (srResult rsp) `shouldBe` (String <$> expectedOutcome)
      uiGlobal <- getUiSnapshot uiH
      uiShowConfig uiGlobal `shouldBe` True
      lvsBaseView (effectiveViewSelection uiGlobal) `shouldBe` BaseViewBiome

      _ <- dispatch ctx "editor_toggle" (object ["active" .= True])
      toolRsp <- dispatch ctx "send_key" (object ["key" .= ("3" :: Text)])
      srSuccess toolRsp `shouldBe` True
      undoRsp <- dispatch ctx "send_key" (object
        [ "key" .= ("Z" :: Text)
        , "modifiers" .= (["ctrl"] :: [Text])
        ])
      srSuccess undoRsp `shouldBe` True
      redoRsp <- dispatch ctx "send_key" (object
        [ "key" .= ("y" :: Text)
        , "modifiers" .= (["control"] :: [Text])
        ])
      srSuccess redoRsp `shouldBe` True
      lookupKey "modifiers" (srResult redoRsp) `shouldBe` Just (Aeson.toJSON (["ctrl"] :: [Text]))
      beforeRadius <- brushRadius . editorBrush . uiEditor <$> getUiSnapshot uiH
      _ <- dispatch ctx "send_key" (object ["key" .= ("]" :: Text)])
      afterIncrease <- brushRadius . editorBrush . uiEditor <$> getUiSnapshot uiH
      afterIncrease `shouldBe` min 6 (beforeRadius + 1)
      _ <- dispatch ctx "send_key" (object ["key" .= ("[" :: Text)])
      afterDecrease <- brushRadius . editorBrush . uiEditor <$> getUiSnapshot uiH
      afterDecrease `shouldBe` max 0 (afterIncrease - 1)
      _ <- dispatch ctx "send_key" (object ["key" .= ("escape" :: Text)])
      uiEditorState <- getUiSnapshot uiH
      editorTool (uiEditor uiEditorState) `shouldBe` ToolSmooth
      editorActive (uiEditor uiEditorState) `shouldBe` False

    it "preserves Data Browser reducer ownership for cursor editing" $ withCtx $ \ctx -> do
      let uiH = ahUiHandle (ccActorHandles ctx)
          editing = emptyDataBrowserState
            { dbsCreateMode = True
            , dbsFocusedField = Just "name"
            , dbsEditValues = Map.singleton "name" (String "abcd")
            , dbsTextCursor = 4
            }
      forM_
        [ ("home", 4, "abcd", 0)
        , ("right", 1, "abcd", 2)
        , ("delete", 1, "acd", 1)
        , ("backspace", 2, "acd", 1)
        , ("end", 0, "abcd", 4)
        ] $ \(key, initialCursor, expectedText, expectedCursor) -> do
          setUiDataBrowser uiH editing { dbsTextCursor = initialCursor }
          before <- getUiSnapshot uiH
          dbsCreateMode (uiDataBrowser before) `shouldBe` True
          dbsFocusedField (uiDataBrowser before) `shouldBe` Just "name"
          rsp <- dispatch ctx "send_key" (object ["key" .= (key :: Text)])
          srSuccess rsp `shouldBe` True
          after <- getUiSnapshot uiH
          Map.lookup "name" (dbsEditValues (uiDataBrowser after)) `shouldBe` Just (String expectedText)
          dbsTextCursor (uiDataBrowser after) `shouldBe` expectedCursor

    it "matches the Escape context/menu cascade" $ withCtx $ \ctx -> do
      let uiH = ahUiHandle (ccActorHandles ctx)
      setUiContextHex uiH (Just (2, 3))
      setUiContextPos uiH (Just (10, 20))
      setUiMenuMode uiH MenuNone
      _ <- getUiSnapshot uiH
      first <- dispatch ctx "send_key" (object ["key" .= ("escape" :: Text)])
      srSuccess first `shouldBe` True
      ui1 <- getUiSnapshot uiH
      uiContextHex ui1 `shouldBe` Nothing
      uiContextPos ui1 `shouldBe` Nothing
      uiMenuMode ui1 `shouldBe` MenuEscape
      second <- dispatch ctx "dialog_cancel" Null
      srSuccess second `shouldBe` True
      ui2 <- getUiSnapshot uiH
      uiMenuMode ui2 `shouldBe` MenuNone

  -- -------------------------------------------------------------------
  -- get_state
  -- -------------------------------------------------------------------
  describe "get_state" $ do
    it "returns success with state fields" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_state" Null
      srSuccess rsp `shouldBe` True
      -- result should contain at least "seed" and "view_mode"
      case srResult rsp of
        Object o -> do
          KM.member "seed"      o `shouldBe` True
          KM.member "view_mode" o `shouldBe` True
        _ -> expectationFailure "expected JSON object in result"

  -- -------------------------------------------------------------------
  -- get_view_modes
  -- -------------------------------------------------------------------
  describe "get_view_modes" $ do
    it "returns a non-empty list of view modes" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_view_modes" Null
      srSuccess rsp `shouldBe` True
      case lookupKey "view_modes" (srResult rsp) of
        Just (Array arr) -> length arr `shouldSatisfy` (> 0)
        _                -> expectationFailure "expected view_modes array"

    it "marks exactly one view mode as active" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_view_modes" Null
      case lookupKey "view_modes" (srResult rsp) of
        Just (Array arr) ->
          let actives = filter isActive (toList arr)
          in length actives `shouldBe` 1
        _ -> expectationFailure "expected view_modes array"

    it "exposes weather as temperature metadata" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_view_modes" Null
      srSuccess rsp `shouldBe` True
      case lookupKey "view_modes" (srResult rsp) of
        Just (Array arr) ->
          case findViewMode "weather" (toList arr) of
            Just weather -> do
              lookupKey "label" weather `shouldBe` Just (String "Current Weather Temp")
              lookupKey "description" weather `shouldBe`
                Just (String "Current simulated weather temperature with humidity, wind, pressure, and precipitation context; use Current Cloud/Storm for aggregate cloud cover and storm tint.")
            Nothing -> expectationFailure "missing weather view mode summary"
        _ -> expectationFailure "expected view_modes array"

    it "places explicit current/average weather controls in metadata" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_view_modes" Null
      srSuccess rsp `shouldBe` True
      case lookupKey "view_modes" (srResult rsp) of
        Just (Array arr) -> do
          let modes = toList arr
              names = mapMaybe viewModeName modes
          take 8 names `shouldBe`
            [ "elevation", "biome", "climate"
            , "weather", "cloud", "moisture", "precipitation", "precipitation_current"
            ]
          case findViewMode "cloud" modes of
            Just cloud -> do
              lookupKey "label" cloud `shouldBe` Just (String "Current Cloud/Storm")
              lookupKey "description" cloud `shouldBe`
                Just (String "Current simulated aggregate cloud cover and cloud-water density with precipitation-derived storm tint; low/mid/high layer fields are inspector/API context, not separate rendered layers.")
            Nothing -> expectationFailure "missing cloud view mode summary"
          case findViewMode "cloud_typical" modes of
            Just cloudTypical -> do
              lookupKey "label" cloudTypical `shouldBe` Just (String "Typical Cloud Normal")
              lookupKey "temporal_basis" cloudTypical `shouldBe` Just (String "typical_normal")
            Nothing -> expectationFailure "missing typical cloud view mode summary"
          case findViewMode "precipitation_current" modes of
            Just precipCurrent -> do
              lookupKey "label" precipCurrent `shouldBe` Just (String "Current Precipitation")
              lookupKey "temporal_basis" precipCurrent `shouldBe` Just (String "instantaneous_current")
            Nothing -> expectationFailure "missing current precipitation view mode summary"
        _ -> expectationFailure "expected view_modes array"

  -- -------------------------------------------------------------------
  -- get_views
  -- -------------------------------------------------------------------
  describe "get_views" $ do
    it "returns layered view choices and legacy mode summaries" $ withCtx $ \ctx -> do
      _ <- writeSingleChunkTerrainWithNormals ctx
      rsp <- dispatch ctx "get_views" Null
      srSuccess rsp `shouldBe` True
      lookupKey "view" (srResult rsp) `shouldSatisfy` maybe False (const True)
      case lookupKey "base_modes" (srResult rsp) of
        Just (Array arr) -> length arr `shouldSatisfy` (> 0)
        _ -> expectationFailure "expected base_modes array"
      case lookupKey "overlay_modes" (srResult rsp) of
        Just (Array arr) -> length arr `shouldSatisfy` (> 0)
        _ -> expectationFailure "expected overlay_modes array"
      case lookupKey "weather_bases" (srResult rsp) of
        Just (Array arr) -> length arr `shouldBe` 2
        _ -> expectationFailure "expected weather_bases array"
      case lookupKey "overlay_names" (srResult rsp) of
        Just (Array arr) -> toList arr `shouldContain` [String "weather_normals"]
        _ -> expectationFailure "expected overlay_names array"
      case lookupKey "legacy_modes" (srResult rsp) of
        Just (Array arr) -> length arr `shouldSatisfy` (> 0)
        _ -> expectationFailure "expected legacy_modes array"

  -- -------------------------------------------------------------------
  -- get_sliders
  -- -------------------------------------------------------------------
  describe "get_sliders" $ do
    it "returns a non-empty list of sliders" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_sliders" Null
      srSuccess rsp `shouldBe` True
      case lookupKey "sliders" (srResult rsp) of
        Just (Array arr) -> length arr `shouldSatisfy` (> 0)
        _                -> expectationFailure "expected sliders array"

    it "filters sliders by tab" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_sliders" (object ["tab" .= ("terrain" :: String)])
      srSuccess rsp `shouldBe` True
      case lookupKey "sliders" (srResult rsp) of
        Just (Array arr) -> do
          length arr `shouldSatisfy` (> 0)
          -- every slider should have tab == "terrain"
          mapM_ (\s -> lookupKey "tab" s `shouldBe` Just (String "terrain")) (toList arr)
        _ -> expectationFailure "expected sliders array"

  -- -------------------------------------------------------------------
  -- get_slider
  -- -------------------------------------------------------------------
  describe "get_slider" $ do
    it "returns a known slider" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_slider" (object ["name" .= ("SliderGenScale" :: String)])
      srSuccess rsp `shouldBe` True
      lookupKey "name" (srResult rsp) `shouldBe` Just (String "SliderGenScale")

    it "returns error for unknown slider" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_slider" (object ["name" .= ("NoSuchSlider" :: String)])
      srSuccess rsp `shouldBe` False

    it "returns error when name is missing" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_slider" Null
      srSuccess rsp `shouldBe` False

  -- -------------------------------------------------------------------
  -- set_slider
  -- -------------------------------------------------------------------
  describe "set_slider" $ do
    it "sets a slider and returns the clamped value" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "set_slider" (object ["name" .= ("SliderGenScale" :: String), "value" .= (0.42 :: Double)])
      srSuccess rsp `shouldBe` True
      lookupKey "name" (srResult rsp) `shouldBe` Just (String "SliderGenScale")

    it "does not enqueue day/night atlas work when authoritative geometry sliders change" $ withCtx $ \ctx -> do
      prepareReadyDayNight ctx
      let handles = ccActorHandles ctx
      version0 <- readSnapshotVersion (ahSnapshotVersionRef handles)

      rsp <- dispatch ctx "set_slider" (object ["name" .= ("SliderAxialTilt" :: String), "value" .= (0.9 :: Double)])
      srSuccess rsp `shouldBe` True

      readSnapshotVersion (ahSnapshotVersionRef handles) `shouldReturnSatisfying` (> version0)
      jobs <- drainAtlasJobs (ahAtlasManagerHandle handles)
      length jobs `shouldBe` 0

    it "does not enqueue day/night work for a non-key slider" $ withCtx $ \ctx -> do
      prepareReadyDayNight ctx
      let handles = ccActorHandles ctx
      version0 <- readSnapshotVersion (ahSnapshotVersionRef handles)

      rsp <- dispatch ctx "set_slider" (object ["name" .= ("SliderInsolation" :: String), "value" .= (0.9 :: Double)])
      srSuccess rsp `shouldBe` True

      readSnapshotVersion (ahSnapshotVersionRef handles) `shouldReturnSatisfying` (> version0)
      jobs <- drainAtlasJobs (ahAtlasManagerHandle handles)
      length jobs `shouldBe` 0

    it "does not enqueue day/night work while day/night is disabled" $ withCtx $ \ctx -> do
      _ <- writeReadyTerrainData ctx
      let handles = ccActorHandles ctx
      version0 <- readSnapshotVersion (ahSnapshotVersionRef handles)

      rsp <- dispatch ctx "set_slider" (object ["name" .= ("SliderAxialTilt" :: String), "value" .= (0.9 :: Double)])
      srSuccess rsp `shouldBe` True

      readSnapshotVersion (ahSnapshotVersionRef handles) `shouldReturnSatisfying` (> version0)
      jobs <- drainAtlasJobs (ahAtlasManagerHandle handles)
      length jobs `shouldBe` 0

    it "does not enqueue day/night work when terrain is unavailable" $ withCtx $ \ctx -> do
      let handles = ccActorHandles ctx
          uiH = ahUiHandle handles
      setUiDayNightEnabled uiH True
      _ <- getUiSnapshot uiH
      version0 <- readSnapshotVersion (ahSnapshotVersionRef handles)

      rsp <- dispatch ctx "set_slider" (object ["name" .= ("SliderAxialTilt" :: String), "value" .= (0.9 :: Double)])
      srSuccess rsp `shouldBe` True

      readSnapshotVersion (ahSnapshotVersionRef handles) `shouldReturnSatisfying` (> version0)
      jobs <- drainAtlasJobs (ahAtlasManagerHandle handles)
      length jobs `shouldBe` 0

    it "clamps values above 1" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "set_slider" (object ["name" .= ("SliderGenScale" :: String), "value" .= (5.0 :: Double)])
      srSuccess rsp `shouldBe` True
      case lookupKey "value" (srResult rsp) of
        Just (Number n) -> n `shouldSatisfy` (<= 1.0)
        _               -> expectationFailure "expected numeric value"

    it "clamps values below 0" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "set_slider" (object ["name" .= ("SliderGenScale" :: String), "value" .= ((-2.0) :: Double)])
      srSuccess rsp `shouldBe` True
      case lookupKey "value" (srResult rsp) of
        Just (Number n) -> n `shouldSatisfy` (>= 0.0)
        _               -> expectationFailure "expected numeric value"

    it "returns error for unknown slider" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "set_slider" (object ["name" .= ("NoSuchSlider" :: String), "value" .= (0.5 :: Double)])
      srSuccess rsp `shouldBe` False

    it "returns error when params are missing" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "set_slider" Null
      srSuccess rsp `shouldBe` False

  -- -------------------------------------------------------------------
  -- set_seed
  -- -------------------------------------------------------------------
  describe "set_seed" $ do
    it "sets the seed and returns it" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "set_seed" (object ["seed" .= (12345 :: Int)])
      srSuccess rsp `shouldBe` True
      lookupKey "seed" (srResult rsp) `shouldBe` Just (Number 12345)
      (_, committed) <- readCommittedRenderSnapshot
        (ahSnapshotVersionRef (ccActorHandles ctx))
      uiSeed (rsUi committed) `shouldBe` 12345

    it "returns error when seed is missing" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "set_seed" Null
      srSuccess rsp `shouldBe` False

  -- -------------------------------------------------------------------
  -- set_view_mode
  -- -------------------------------------------------------------------
  describe "set_view_mode" $ do
    it "switches to a valid view mode" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "set_view_mode" (object ["mode" .= ("biome" :: String)])
      srSuccess rsp `shouldBe` True
      lookupKey "view_mode" (srResult rsp) `shouldBe` Just (String "biome")

    it "switches weather-related modes by explicit temporal basis" $ withCtx $ \ctx -> do
      precipRsp <- dispatch ctx "set_view_mode" (object ["mode" .= ("precipitation" :: Text), "basis" .= ("current" :: Text)])
      srSuccess precipRsp `shouldBe` True
      lookupKey "view_mode" (srResult precipRsp) `shouldBe` Just (String "precipitation_current")
      lookupKey "temporal_basis" (srResult precipRsp) `shouldBe` Just (String "instantaneous_current")

      cloudRsp <- dispatch ctx "set_view_mode" (object ["mode" .= ("cloud" :: Text), "basis" .= ("typical" :: Text)])
      srSuccess cloudRsp `shouldBe` True
      lookupKey "view_mode" (srResult cloudRsp) `shouldBe` Just (String "cloud_typical")
      lookupKey "temporal_basis" (srResult cloudRsp) `shouldBe` Just (String "typical_normal")

      uiRsp <- dispatch ctx "get_ui_state" Null
      srSuccess uiRsp `shouldBe` True
      case lookupKey "view" (srResult uiRsp) of
        Just (Object view) -> do
          KM.lookup "mode" view `shouldBe` Just (String "cloud_typical")
          KM.lookup "temporal_basis" view `shouldBe` Just (String "typical_normal")
          KM.lookup "source_kind" view `shouldBe` Just (String "weather_normals")
          case KM.lookup "selection" view of
            Just (Object selection) -> do
              KM.lookup "weather_basis" selection `shouldBe` Just (String "average")
              KM.lookup "source_kind" selection `shouldBe` Just (String "weather_normals")
            _ -> expectationFailure "expected get_ui_state.view.selection object"
        _ -> expectationFailure "expected get_ui_state.view object"

    it "rejects unsupported typical basis for average-only temperature and precipitation views" $ withCtx $ \ctx -> do
      tempRsp <- dispatch ctx "set_view_mode" (object ["mode" .= ("weather" :: Text), "basis" .= ("typical" :: Text)])
      srSuccess tempRsp `shouldBe` False

      precipRsp <- dispatch ctx "set_view_mode" (object ["mode" .= ("precipitation" :: Text), "basis" .= ("typical" :: Text)])
      srSuccess precipRsp `shouldBe` False

    it "returns error for unknown view mode" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "set_view_mode" (object ["mode" .= ("nonexistent" :: String)])
      srSuccess rsp `shouldBe` False

    it "returns error when mode is missing" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "set_view_mode" Null
      srSuccess rsp `shouldBe` False

  -- -------------------------------------------------------------------
  -- set_view
  -- -------------------------------------------------------------------
  describe "set_view" $ do
    it "accepts nullable overlay fields when clearing layered overlays" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "set_view" (object
        [ "base_mode" .= ("elevation" :: Text)
        , "overlay_mode" .= Null
        , "plugin_overlay" .= Null
        , "overlay_field" .= Null
        ])
      srSuccess rsp `shouldBe` True
      lookupKey "overlay_mode" (srResult rsp) `shouldBe` Just Null
      lookupKey "plugin_overlay" (srResult rsp) `shouldBe` Just Null

    it "sets builtin layered base, overlay, basis, and opacity together" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "set_view" (object
        [ "base" .= ("biome" :: Text)
        , "overlay" .= ("cloud" :: Text)
        , "basis" .= ("average" :: Text)
        , "overlay_opacity" .= (0.42 :: Double)
        ])
      srSuccess rsp `shouldBe` True
      ui <- getUiSnapshot (ahUiHandle (ccActorHandles ctx))
      lvsBaseView (uiViewSelection ui) `shouldBe` BaseViewBiome
      lvsSkyOverlay (uiViewSelection ui) `shouldBe` Just SkyOverlayCloud
      lvsWeatherBasis (uiViewSelection ui) `shouldBe` WeatherBasisAverage
      lvsOverlayOpacity (uiViewSelection ui) `shouldSatisfy` (\opacity -> abs (opacity - 0.42) < 0.0001)
      lookupKey "view_mode" (srResult rsp) `shouldBe` Just (String "cloud_typical")
      lookupKey "base_mode" (srResult rsp) `shouldBe` Just (String "biome")
      lookupKey "overlay_mode" (srResult rsp) `shouldBe` Just (String "cloud")
      lookupKey "weather_basis" (srResult rsp) `shouldBe` Just (String "average")

    it "uses overlay_field as a plugin field alias" $ withCtx $ \ctx -> do
      _ <- writeSingleChunkTerrainWithNormals ctx
      rsp <- dispatch ctx "set_view" (object
        [ "base_mode" .= ("biome" :: Text)
        , "overlay_mode" .= ("plugin" :: Text)
        , "plugin_overlay" .= ("weather_normals" :: Text)
        , "overlay_field" .= (1 :: Int)
        ])
      srSuccess rsp `shouldBe` True
      ui <- getUiSnapshot (ahUiHandle (ccActorHandles ctx))
      lvsBaseView (uiViewSelection ui) `shouldBe` BaseViewBiome
      lvsSkyOverlay (uiViewSelection ui) `shouldBe` Just (SkyOverlayPlugin "weather_normals" 1)
      lookupKey "overlay_field" (srResult rsp) `shouldBe` Just (Number 1)

    it "treats plugin_overlay without overlay_mode as a plugin overlay selection" $ withCtx $ \ctx -> do
      _ <- writeSingleChunkTerrainWithNormals ctx
      rsp <- dispatch ctx "set_view" (object
        [ "plugin_overlay" .= ("weather_normals" :: Text)
        , "field_index" .= (1 :: Int)
        ])
      srSuccess rsp `shouldBe` True
      ui <- getUiSnapshot (ahUiHandle (ccActorHandles ctx))
      lvsSkyOverlay (uiViewSelection ui) `shouldBe` Just (SkyOverlayPlugin "weather_normals" 1)
      lookupKey "overlay_mode" (srResult rsp) `shouldBe` Just (String "plugin")
      lookupKey "plugin_overlay" (srResult rsp) `shouldBe` Just (String "weather_normals")

    it "updates the active plugin overlay field without resending the overlay name" $ withCtx $ \ctx -> do
      _ <- writeSingleChunkTerrainWithNormals ctx
      firstRsp <- dispatch ctx "set_view" (object
        [ "plugin_overlay" .= ("weather_normals" :: Text)
        , "field_index" .= (0 :: Int)
        ])
      srSuccess firstRsp `shouldBe` True

      rsp <- dispatch ctx "set_view" (object ["overlay_field" .= (1 :: Int)])
      srSuccess rsp `shouldBe` True
      ui <- getUiSnapshot (ahUiHandle (ccActorHandles ctx))
      lvsSkyOverlay (uiViewSelection ui) `shouldBe` Just (SkyOverlayPlugin "weather_normals" 1)
      lookupKey "overlay_mode" (srResult rsp) `shouldBe` Just (String "plugin")
      lookupKey "plugin_overlay" (srResult rsp) `shouldBe` Just (String "weather_normals")
      lookupKey "overlay_field" (srResult rsp) `shouldBe` Just (Number 1)

    it "preserves layered base, basis, and opacity across legacy overlay set and cycles" $ withCtx $ \ctx -> do
      _ <- writeSingleChunkTerrainWithNormals ctx
      initial <- dispatch ctx "set_view" (object
        [ "base" .= ("biome" :: Text)
        , "overlay" .= ("cloud" :: Text)
        , "basis" .= ("average" :: Text)
        , "overlay_opacity" .= (0.37 :: Double)
        ])
      srSuccess initial `shouldBe` True

      setRsp <- dispatch ctx "set_overlay" (object
        [ "overlay" .= ("weather_normals" :: Text)
        , "field_index" .= (0 :: Int)
        ])
      srSuccess setRsp `shouldBe` True
      assertPreservedLayeredSelection ctx (Just (SkyOverlayPlugin "weather_normals" 0))

      fieldRsp <- dispatch ctx "cycle_overlay_field" (object ["direction" .= (1 :: Int)])
      srSuccess fieldRsp `shouldBe` True
      assertPreservedLayeredSelection ctx (Just (SkyOverlayPlugin "weather_normals" 1))

      offRsp <- dispatch ctx "cycle_overlay" (object ["direction" .= (1 :: Int)])
      srSuccess offRsp `shouldBe` True
      lookupKey "view_mode" (srResult offRsp) `shouldBe` Just (String "biome")
      assertPreservedLayeredSelection ctx Nothing

      onRsp <- dispatch ctx "cycle_overlay" (object ["direction" .= (1 :: Int)])
      srSuccess onRsp `shouldBe` True
      assertPreservedLayeredSelection ctx (Just (SkyOverlayPlugin "weather_normals" 0))

    it "rejects plugin overlay field changes without an active plugin overlay" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "set_view" (object ["overlay_field" .= (1 :: Int)])
      srSuccess rsp `shouldBe` False

    it "rejects unknown plugin overlays" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "set_view" (object
        [ "overlay_mode" .= ("plugin" :: Text)
        , "plugin_overlay" .= ("__missing_overlay__" :: Text)
        ])
      srSuccess rsp `shouldBe` False

  -- -------------------------------------------------------------------
  -- set_config_tab
  -- -------------------------------------------------------------------
  describe "set_config_tab" $ do
    it "switches to a valid config tab" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "set_config_tab" (object ["tab" .= ("climate" :: String)])
      srSuccess rsp `shouldBe` True
      lookupKey "config_tab" (srResult rsp) `shouldBe` Just (String "climate")

    it "returns error for unknown config tab" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "set_config_tab" (object ["tab" .= ("nonexistent" :: String)])
      srSuccess rsp `shouldBe` False

    it "returns error when tab is missing" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "set_config_tab" Null
      srSuccess rsp `shouldBe` False

  -- -------------------------------------------------------------------
  -- generate
  -- -------------------------------------------------------------------
  describe "generate" $ do
    it "returns success with generating status" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "generate" Null
      srSuccess rsp `shouldBe` True
      lookupKey "status" (srResult rsp) `shouldBe` Just (String "generating")

  -- -------------------------------------------------------------------
  -- terrain editor
  -- -------------------------------------------------------------------
  describe "editor_get_state" $ do
    it "returns the default editor state" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "editor_get_state" Null
      srSuccess rsp `shouldBe` True
      lookupKey "active" (srResult rsp) `shouldBe` Just (Bool False)
      lookupKey "tool" (srResult rsp) `shouldBe` Just (String "raise")

  describe "editor_toggle" $ do
    it "toggles editor activity when active is omitted" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "editor_toggle" (object [])
      srSuccess rsp `shouldBe` True
      lookupKey "active" (srResult rsp) `shouldBe` Just (Bool True)

    it "sets editor activity explicitly when active is provided" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "editor_toggle" (object ["active" .= True])
      srSuccess rsp `shouldBe` True
      lookupKey "active" (srResult rsp) `shouldBe` Just (Bool True)

  describe "editor_set_tool" $ do
    it "switches to a valid tool" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "editor_set_tool" (object ["tool" .= ("erode" :: String)])
      srSuccess rsp `shouldBe` True
      lookupKey "tool" (srResult rsp) `shouldBe` Just (String "erode")

    it "returns error for an unknown tool" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "editor_set_tool" (object ["tool" .= ("bogus" :: String)])
      srSuccess rsp `shouldBe` False

  describe "editor_set_brush" $ do
    it "updates brush parameters and clamps tool-specific values" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "editor_set_brush" (object
        [ "radius" .= (3 :: Int)
        , "strength" .= (0.4 :: Double)
        , "falloff" .= ("smooth" :: String)
        , "smooth_passes" .= (99 :: Int)
        , "noise_frequency" .= (9.0 :: Double)
        , "erode_passes" .= (0 :: Int)
        ])
      srSuccess rsp `shouldBe` True
      case lookupKey "brush" (srResult rsp) of
        Just brushVal -> do
          lookupKey "radius" brushVal `shouldBe` Just (Number 3)
          lookupKey "falloff" brushVal `shouldBe` Just (String "smooth")
        Nothing -> expectationFailure "expected brush object"
      lookupKey "smooth_passes" (srResult rsp) `shouldBe` Just (Number 5)
      lookupKey "erode_passes" (srResult rsp) `shouldBe` Just (Number 1)

  describe "editor_set_biome" $ do
    it "accepts biome display names" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "editor_set_biome" (object ["biome" .= ("Forest" :: String)])
      srSuccess rsp `shouldBe` True
      case lookupKey "biome" (srResult rsp) of
        Just biomeVal -> lookupKey "name" biomeVal `shouldBe` Just (String "Forest")
        Nothing -> expectationFailure "expected biome object"

  describe "editor_set_form" $ do
    it "accepts terrain form display names" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "editor_set_form" (object ["form" .= ("Hilly" :: String)])
      srSuccess rsp `shouldBe` True
      case lookupKey "terrain_form" (srResult rsp) of
        Just formVal -> lookupKey "name" formVal `shouldBe` Just (String "Hilly")
        Nothing -> expectationFailure "expected terrain_form object"

  describe "editor_set_hardness" $ do
    it "clamps hardness target into range" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "editor_set_hardness" (object ["hardness" .= (9.5 :: Double)])
      srSuccess rsp `shouldBe` True
      lookupKey "hardness_target" (srResult rsp) `shouldBe` Just (Number 1)

  describe "editor_brush_stroke" $ do
    it "queues a single brush stroke and publishes the unavailable-terrain no-delta path" $ withCtx $ \ctx -> do
      baseline <- beginPublicationAssertion ctx
      rsp <- dispatch ctx "editor_brush_stroke" (object ["q" .= (0 :: Int), "r" .= (0 :: Int)])
      srSuccess rsp `shouldBe` True
      lookupKey "status" (srResult rsp) `shouldBe` Just (String "queued")
      lookupKey "strokes_queued" (srResult rsp) `shouldBe` Just (Number 1)
      (_, committed) <- expectExactPublication ctx baseline
      committed `shouldBe` pbSnapshot baseline
      jobs <- drainAtlasJobs (ahAtlasManagerHandle (ccActorHandles ctx))
      length jobs `shouldBe` 0

    it "preserves untouched terrain chunks across brush, undo, and redo" $ withCtx $ \ctx -> do
      let dataHandle = ahDataHandle (ccActorHandles ctx)
          cfg = WorldConfig { wcChunkSize = 64 }
          farChunkId = chunkIdFromCoord (ChunkCoord 4 4)
          initialChunks =
            [ (chunkIdFromCoord (ChunkCoord 0 0), emptyTerrainChunk cfg)
            , (chunkIdFromCoord (ChunkCoord (-1) 0), emptyTerrainChunk cfg)
            , (chunkIdFromCoord (ChunkCoord 0 (-1)), emptyTerrainChunk cfg)
            , (farChunkId, emptyTerrainChunk cfg)
            ]
          request action = UiActionRequest
            { uarAction = action
            , uarActorHandles = ccActorHandles ctx
            , uarTerrainReplyTo = replyTo @TerrainReplyOps (ccUiActionsHandle ctx)
            }
          expectedChunkCount = length initialChunks
          ChunkId farChunkKey = farChunkId

      setTerrainChunkData dataHandle (wcChunkSize cfg) initialChunks

      runUiAction (request (UiActionBrushStroke (0, 0)))
      afterBrush <- getTerrainSnapshot dataHandle
      IntMap.size (tsTerrainChunks afterBrush) `shouldBe` expectedChunkCount
      IntMap.member farChunkKey (tsTerrainChunks afterBrush) `shouldBe` True

      runUiAction (request UiActionUndo)
      afterUndo <- getTerrainSnapshot dataHandle
      IntMap.size (tsTerrainChunks afterUndo) `shouldBe` expectedChunkCount
      IntMap.member farChunkKey (tsTerrainChunks afterUndo) `shouldBe` True

      runUiAction (request UiActionRedo)
      afterRedo <- getTerrainSnapshot dataHandle
      IntMap.size (tsTerrainChunks afterRedo) `shouldBe` expectedChunkCount
      IntMap.member farChunkKey (tsTerrainChunks afterRedo) `shouldBe` True

  describe "editor_brush_line" $ do
    it "queues multiple brush strokes along a line" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "editor_brush_line" (object
        [ "from_q" .= (0 :: Int)
        , "from_r" .= (0 :: Int)
        , "to_q" .= (3 :: Int)
        , "to_r" .= ((-2) :: Int)
        ])
      srSuccess rsp `shouldBe` True
      case lookupKey "strokes_queued" (srResult rsp) of
        Just (Number n) -> n `shouldSatisfy` (> 1)
        _ -> expectationFailure "expected strokes_queued number"

  describe "editor_undo" $ do
    it "queues undo" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "editor_undo" Null
      srSuccess rsp `shouldBe` True
      lookupKey "status" (srResult rsp) `shouldBe` Just (String "queued")

  describe "editor_redo" $ do
    it "queues redo" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "editor_redo" Null
      srSuccess rsp `shouldBe` True
      lookupKey "status" (srResult rsp) `shouldBe` Just (String "queued")

  -- -------------------------------------------------------------------
  -- get_enums
  -- -------------------------------------------------------------------
  describe "get_enums" $ do
    it "returns biome enum values" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_enums" (object ["type" .= ("biome" :: String)])
      srSuccess rsp `shouldBe` True
      case lookupKey "values" (srResult rsp) of
        Just (Array arr) -> length arr `shouldSatisfy` (> 50)  -- 65 biomes
        _ -> expectationFailure "expected values array"

    it "returns terrain_form enum values" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_enums" (object ["type" .= ("terrain_form" :: String)])
      srSuccess rsp `shouldBe` True
      case lookupKey "values" (srResult rsp) of
        Just (Array arr) -> length arr `shouldBe` 15
        _ -> expectationFailure "expected values array"

    it "returns water_body_type enum values" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_enums" (object ["type" .= ("water_body_type" :: String)])
      srSuccess rsp `shouldBe` True
      case lookupKey "values" (srResult rsp) of
        Just (Array arr) -> length arr `shouldBe` 4
        _ -> expectationFailure "expected values array"

    it "returns view_mode enum values" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_enums" (object ["type" .= ("view_mode" :: String)])
      srSuccess rsp `shouldBe` True
      case lookupKey "values" (srResult rsp) of
        Just (Array arr) -> length arr `shouldBe` 18
        _ -> expectationFailure "expected values array"

    it "returns config_tab enum values" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_enums" (object ["type" .= ("config_tab" :: String)])
      srSuccess rsp `shouldBe` True
      case lookupKey "values" (srResult rsp) of
        Just (Array arr) -> length arr `shouldBe` 8
        _ -> expectationFailure "expected values array"

    it "returns slider_tab enum values" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_enums" (object ["type" .= ("slider_tab" :: String)])
      srSuccess rsp `shouldBe` True
      case lookupKey "values" (srResult rsp) of
        Just (Array arr) -> length arr `shouldBe` 6
        _ -> expectationFailure "expected values array"

    it "returns error for unknown enum type" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_enums" (object ["type" .= ("nonsense" :: String)])
      srSuccess rsp `shouldBe` False

    it "returns error when type is missing" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_enums" Null
      srSuccess rsp `shouldBe` False

  -- -------------------------------------------------------------------
  -- get_world_meta
  -- -------------------------------------------------------------------
  describe "get_world_meta" $ do
    it "returns success with world metadata" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_world_meta" Null
      srSuccess rsp `shouldBe` True
      case srResult rsp of
        Object o -> do
          KM.member "seed"       o `shouldBe` True
          KM.member "chunk_size" o `shouldBe` True
          KM.member "chunk_count" o `shouldBe` True
        _ -> expectationFailure "expected JSON object in result"

  -- -------------------------------------------------------------------
  -- get_generation_status
  -- -------------------------------------------------------------------
  describe "get_generation_status" $ do
    it "returns success with generation status" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_generation_status" Null
      srSuccess rsp `shouldBe` True
      case srResult rsp of
        Object o -> KM.member "generating" o `shouldBe` True
        _ -> expectationFailure "expected JSON object in result"

  -- -------------------------------------------------------------------
  -- get_overlays
  -- -------------------------------------------------------------------
  describe "get_overlays" $ do
    it "returns success with overlay info" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_overlays" Null
      srSuccess rsp `shouldBe` True
      case srResult rsp of
        Object o -> do
          KM.member "overlay_count" o `shouldBe` True
          KM.member "overlay_names" o `shouldBe` True
        _ -> expectationFailure "expected JSON object in result"

    it "routes overlay widgets through AppService into the inspector model" $ withCtx $ \ctx -> do
      _ <- writeSingleChunkTerrainWithNormals ctx
      showLeftViewWidgets ctx
      directManager <- dispatch ctx "get_overlays" Null
      managerClick <- dispatch ctx "click_widget"
        (object ["widget_id" .= ("WidgetOverlayManager" :: Text)])
      srSuccess managerClick `shouldBe` True
      lookupKey "status" (srResult managerClick) `shouldBe` Just (String "accepted")
      waitOverlayInspectorExecutorIdle (ccOverlayInspectorExecutor ctx)
      managerState <- dispatch ctx "get_widget_state"
        (object ["widget_id" .= ("WidgetOverlayManager" :: Text)])
      inspectorPayload "manager" (srResult managerState)
        `shouldBe` Just (srResult directManager)

      directSchema <- dispatch ctx "get_overlay_schema"
        (object ["overlay" .= ("weather_normals" :: Text)])
      schemaClick <- dispatch ctx "click_widget"
        (object ["widget_id" .= ("WidgetOverlaySchema" :: Text)])
      srSuccess schemaClick `shouldBe` True
      waitOverlayInspectorExecutorIdle (ccOverlayInspectorExecutor ctx)
      schemaState <- dispatch ctx "get_widget_state"
        (object ["widget_id" .= ("WidgetOverlaySchema" :: Text)])
      inspectorPayload "schema" (srResult schemaState)
        `shouldBe` Just (srResult directSchema)

    it "uses the currently injected AppService for nested overlay work" $ withCtx $ \ctx -> do
      showLeftViewWidgets ctx
      let sentinel = object
            [ "overlay_names" .= ([] :: [Text])
            , "source" .= ("injected" :: Text)
            ]
          terrainService = (appTerrain commandAppService)
            { terrainGetOverlays = rawServiceHandler terrainGetOverlaysOperation $ \_ _ ->
                pure (Right (ServiceResponse sentinel))
            }
          app = commandAppService { appTerrain = terrainService }
      clicked <- dispatchAppServiceCommand app ctx SeerCommand
        { scId = 1
        , scMethod = "click_widget"
        , scParams = object ["widget_id" .= ("WidgetOverlayManager" :: Text)]
        }
      srSuccess clicked `shouldBe` True
      waitOverlayInspectorExecutorIdle (ccOverlayInspectorExecutor ctx)
      state <- dispatchAppServiceCommand app ctx SeerCommand
        { scId = 2
        , scMethod = "get_widget_state"
        , scParams = object ["widget_id" .= ("WidgetOverlayManager" :: Text)]
        }
      inspectorPayload "manager" (srResult state) `shouldBe` Just sentinel

  -- -------------------------------------------------------------------
  -- get_chunks (empty terrain)
  -- -------------------------------------------------------------------
  describe "get_chunks" $ do
    it "returns success with empty chunk list" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_chunks" Null
      srSuccess rsp `shouldBe` True
      lookupKey "chunk_count" (srResult rsp) `shouldBe` Just (Number 0)

  -- -------------------------------------------------------------------
  -- get_chunk_summary (empty terrain)
  -- -------------------------------------------------------------------
  describe "get_chunk_summary" $ do
    it "returns error for non-existent chunk" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_chunk_summary" (object ["chunk" .= (999 :: Int)])
      srSuccess rsp `shouldBe` False

    it "returns error when chunk param is missing" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_chunk_summary" Null
      srSuccess rsp `shouldBe` False

  -- -------------------------------------------------------------------
  -- get_hex (empty terrain)
  -- -------------------------------------------------------------------
  describe "get_hex" $ do
    it "returns error for non-existent chunk" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_hex" (object ["q" .= (0 :: Int), "r" .= (0 :: Int)])
      srSuccess rsp `shouldBe` False

    it "returns active view metadata and values for populated terrain" $ withCtx $ \ctx -> do
      _chunkKey <- writeSingleChunkTerrain ctx
      rsp <- dispatch ctx "get_hex" (object ["q" .= (0 :: Int), "r" .= (0 :: Int)])
      srSuccess rsp `shouldBe` True
      case lookupKey "active_view" (srResult rsp) of
        Just (Object active) -> do
          KM.lookup "mode" active `shouldBe` Just (String "elevation")
          KM.member "tooltip_fields" active `shouldBe` True
          KM.member "inspector_fields" active `shouldBe` True
          KM.member "export_fields" active `shouldBe` True
          case KM.lookup "values" active of
            Just (Object values) -> KM.member "elevation_m" values `shouldBe` True
            _ -> expectationFailure "expected active_view.values object"
        _ -> expectationFailure "expected active_view object"
      case lookupKey "sections" (srResult rsp) of
        Just (Array sections) -> mapMaybe inspectorSectionKey (toList sections) `shouldBe` canonicalInspectorSectionKeys
        _ -> expectationFailure "expected complete inspector sections array"

    it "returns weather temperature label in active view metadata" $ withCtx $ \ctx -> do
      viewRsp <- dispatch ctx "set_view_mode" (object ["mode" .= ("weather" :: Text)])
      srSuccess viewRsp `shouldBe` True
      _chunkKey <- writeSingleChunkTerrain ctx
      rsp <- dispatch ctx "get_hex" (object ["q" .= (0 :: Int), "r" .= (0 :: Int)])
      srSuccess rsp `shouldBe` True
      case lookupKey "active_view" (srResult rsp) of
        Just (Object active) -> do
          KM.lookup "mode" active `shouldBe` Just (String "weather")
          KM.lookup "label" active `shouldBe` Just (String "Current Weather Temp")
          KM.lookup "temporal_basis" active `shouldBe` Just (String "instantaneous_current")
        _ -> expectationFailure "expected active_view object"

    it "returns Cloud/Storm active view metadata and aggregate value fields" $ withCtx $ \ctx -> do
      viewRsp <- dispatch ctx "set_view_mode" (object ["mode" .= ("cloud" :: Text)])
      srSuccess viewRsp `shouldBe` True
      _chunkKey <- writeSingleChunkTerrain ctx
      rsp <- dispatch ctx "get_hex" (object ["q" .= (0 :: Int), "r" .= (0 :: Int)])
      srSuccess rsp `shouldBe` True
      case lookupKey "active_view" (srResult rsp) of
        Just (Object active) -> do
          KM.lookup "mode" active `shouldBe` Just (String "cloud")
          KM.lookup "label" active `shouldBe` Just (String "Current Cloud/Storm")
          KM.lookup "description" active `shouldBe`
            Just (String "Current simulated aggregate cloud cover and cloud-water density with precipitation-derived storm tint; low/mid/high layer fields are inspector/API context, not separate rendered layers.")
          case KM.lookup "values" active of
            Just (Object values) -> mapM_ (`shouldSatisfy` (`KM.member` values))
              [ "cloud_cover", "cloud_water", "storm_intensity"
              , "cloud_cover_low", "cloud_cover_high", "cloud_water_high"
              ]
            _ -> expectationFailure "expected active_view.values object"
        _ -> expectationFailure "expected active_view object"

    it "returns current precipitation active view values from weather data" $ withCtx $ \ctx -> do
      viewRsp <- dispatch ctx "set_view_mode" (object ["mode" .= ("precipitation" :: Text), "basis" .= ("current" :: Text)])
      srSuccess viewRsp `shouldBe` True
      _chunkKey <- writeSingleChunkTerrain ctx
      rsp <- dispatch ctx "get_hex" (object ["q" .= (0 :: Int), "r" .= (0 :: Int)])
      srSuccess rsp `shouldBe` True
      case lookupKey "active_view" (srResult rsp) of
        Just (Object active) -> do
          KM.lookup "mode" active `shouldBe` Just (String "precipitation_current")
          KM.lookup "temporal_basis" active `shouldBe` Just (String "instantaneous_current")
          case KM.lookup "values" active of
            Just (Object values) -> KM.member "precip_mm_year" values `shouldBe` True
            _ -> expectationFailure "expected active_view.values object"
        _ -> expectationFailure "expected active_view object"

    it "returns explicit basis/source fields for current weather and typical normals" $ withCtx $ \ctx -> do
      _chunkKey <- writeSingleChunkTerrainWithClimateWeatherAndNormals ctx
      rsp <- dispatch ctx "get_hex" (object ["q" .= (0 :: Int), "r" .= (0 :: Int)])
      srSuccess rsp `shouldBe` True
      case lookupKey "weather" (srResult rsp) of
        Just (Object weather) -> do
          KM.lookup "basis" weather `shouldBe` Just (String "instantaneous_current")
          KM.lookup "source_kind" weather `shouldBe` Just (String "weather_snapshot")
          KM.member "temp_current" weather `shouldBe` True
          KM.member "precip_current" weather `shouldBe` True
          KM.member "cloud_cover_current" weather `shouldBe` True
        _ -> expectationFailure "expected weather object"
      case lookupKey "weather_normals" (srResult rsp) of
        Just (Object normals) -> do
          KM.lookup "basis" normals `shouldBe` Just (String "typical_normal")
          KM.lookup "source_kind" normals `shouldBe` Just (String "weather_normals")
          KM.member "temp_typical" normals `shouldBe` True
          KM.member "precip_typical" normals `shouldBe` True
          KM.member "cloud_cover_typical" normals `shouldBe` True
        _ -> expectationFailure "expected weather_normals object"

    it "reports typical cloud normals as unavailable rather than falling back to current clouds" $ withCtx $ \ctx -> do
      viewRsp <- dispatch ctx "set_view_mode" (object ["mode" .= ("cloud" :: Text), "basis" .= ("typical" :: Text)])
      srSuccess viewRsp `shouldBe` True
      _chunkKey <- writeSingleChunkTerrain ctx
      rsp <- dispatch ctx "get_hex" (object ["q" .= (0 :: Int), "r" .= (0 :: Int)])
      srSuccess rsp `shouldBe` True
      case lookupKey "active_view" (srResult rsp) of
        Just (Object active) -> do
          KM.lookup "mode" active `shouldBe` Just (String "cloud_typical")
          KM.lookup "temporal_basis" active `shouldBe` Just (String "typical_normal")
          case KM.lookup "values" active of
            Just (Object values) -> do
              KM.lookup "status" values `shouldBe` Just (String "unavailable")
              KM.lookup "cloud_cover" values `shouldBe` Just Null
            _ -> expectationFailure "expected active_view.values object"
        _ -> expectationFailure "expected active_view object"

    it "reports weather normals as unavailable when the overlay is missing" $ withCtx $ \ctx -> do
      _chunkKey <- writeSingleChunkTerrain ctx
      rsp <- dispatch ctx "get_hex" (object ["q" .= (0 :: Int), "r" .= (0 :: Int)])
      srSuccess rsp `shouldBe` True
      case lookupKey "weather_normals" (srResult rsp) of
        Just (Object normals) -> do
          KM.lookup "loaded" normals `shouldBe` Just (Bool False)
          KM.lookup "status" normals `shouldBe` Just (String "unavailable")
          KM.lookup "temporal_basis" normals `shouldBe` Just (String "typical_normal")
        _ -> expectationFailure "expected weather_normals object"

    it "returns generated typical weather normals separately from current weather" $ withCtx $ \ctx -> do
      _chunkKey <- writeSingleChunkTerrainWithNormals ctx
      rsp <- dispatch ctx "get_hex" (object ["q" .= (0 :: Int), "r" .= (0 :: Int)])
      srSuccess rsp `shouldBe` True
      lookupKey "weather" (srResult rsp) `shouldBe` Just Null
      case lookupKey "weather_normals" (srResult rsp) of
        Just (Object normals) -> do
          KM.lookup "loaded" normals `shouldBe` Just (Bool True)
          KM.lookup "temporal_basis" normals `shouldBe` Just (String "typical_normal")
          KM.member "temp" normals `shouldBe` True
          KM.member "precip" normals `shouldBe` True
          KM.member "cloud_cover" normals `shouldBe` True
        _ -> expectationFailure "expected weather_normals object"

    it "returns error when params are missing" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_hex" Null
      srSuccess rsp `shouldBe` False

  -- -------------------------------------------------------------------
  -- export_terrain_data (populated terrain)
  -- -------------------------------------------------------------------
  describe "export_terrain_data" $ do
    it "returns registry export field metadata" $ withCtx $ \ctx -> do
      chunkKey <- writeSingleChunkTerrain ctx
      rsp <- dispatch ctx "export_terrain_data" (object
        [ "chunks" .= [chunkKey]
        , "fields" .= ["plate_boundary_code" :: Text]
        ])
      srSuccess rsp `shouldBe` True
      case lookupKey "available_fields" (srResult rsp) of
        Just (Array fields) -> do
          toList fields `shouldSatisfy` elem (String "plate_boundary_code")
          toList fields `shouldSatisfy` elem (String "normal_temperature")
          toList fields `shouldSatisfy` elem (String "normal_cloud_cover")
        _ -> expectationFailure "expected available_fields array"

    it "exports generated weather normal fields when present" $ withCtx $ \ctx -> do
      chunkKey <- writeSingleChunkTerrainWithNormals ctx
      rsp <- dispatch ctx "export_terrain_data" (object
        [ "chunks" .= [chunkKey]
        , "fields" .= ["normal_temperature" :: Text, "normal_cloud_cover"]
        ])
      srSuccess rsp `shouldBe` True
      case lookupKey "data" (srResult rsp) of
        Just (Object chunks) ->
          case KM.lookup (Key.fromText (Text.pack (show chunkKey))) chunks of
            Just (Object fields) -> do
              KM.lookup "normal_temperature" fields `shouldSatisfy` isArrayValue
              KM.lookup "normal_cloud_cover" fields `shouldSatisfy` isArrayValue
            _ -> expectationFailure "expected exported chunk object"
        _ -> expectationFailure "expected export data object"

    it "exports basis-qualified canonical fields alongside legacy aliases" $ withCtx $ \ctx -> do
      chunkKey <- writeSingleChunkTerrainWithClimateWeatherAndNormals ctx
      rsp <- dispatch ctx "export_terrain_data" (object
        [ "chunks" .= [chunkKey]
        , "fields" .=
            [ "climate_temp_avg" :: Text
            , "temperature"
            , "weather_temp_current"
            , "weather_temperature"
            , "weather_cloud_cover_current"
            , "cloud_cover"
            , "weather_cloud_cover_typical"
            , "normal_cloud_cover"
            ]
        ])
      srSuccess rsp `shouldBe` True
      case lookupKey "available_fields" (srResult rsp) of
        Just (Array fields) -> do
          toList fields `shouldSatisfy` elem (String "climate_temp_avg")
          toList fields `shouldSatisfy` elem (String "weather_temp_current")
          toList fields `shouldSatisfy` elem (String "weather_cloud_cover_current")
          toList fields `shouldSatisfy` elem (String "weather_cloud_cover_typical")
          toList fields `shouldSatisfy` elem (String "temperature")
        _ -> expectationFailure "expected available_fields array"
      case lookupKey "diagnostics" (srResult rsp) of
        Just (Array diagnostics) -> mapMaybe diagnosticCode (toList diagnostics)
          `shouldSatisfy` (\codes -> all (`elem` codes) ["terrain_export_ready", "basis_qualified_fields", "legacy_basis_aliases"])
        _ -> expectationFailure "expected diagnostics array"
      case lookupKey "data" (srResult rsp) of
        Just (Object chunks) ->
          case KM.lookup (Key.fromText (Text.pack (show chunkKey))) chunks of
            Just (Object fields) -> do
              KM.lookup "climate_temp_avg" fields `shouldBe` KM.lookup "temperature" fields
              KM.lookup "weather_temp_current" fields `shouldBe` KM.lookup "weather_temperature" fields
              KM.lookup "weather_cloud_cover_current" fields `shouldBe` KM.lookup "cloud_cover" fields
              KM.lookup "weather_cloud_cover_typical" fields `shouldBe` KM.lookup "normal_cloud_cover" fields
            _ -> expectationFailure "expected exported chunk object"
        _ -> expectationFailure "expected export data object"

  -- -------------------------------------------------------------------
  -- get_terrain_stats (empty terrain)
  -- -------------------------------------------------------------------
  describe "get_terrain_stats" $ do
    it "returns success with no-data status for empty terrain" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "get_terrain_stats" Null
      srSuccess rsp `shouldBe` True
      lookupKey "chunk_count" (srResult rsp) `shouldBe` Just (Number 0)

  -- -------------------------------------------------------------------
  -- list_worlds
  -- -------------------------------------------------------------------
  describe "list_worlds" $ do
    it "returns success with world list" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "list_worlds" Null
      srSuccess rsp `shouldBe` True
      case srResult rsp of
        Object o -> KM.member "world_count" o `shouldBe` True
        _ -> expectationFailure "expected JSON object in result"

  -- -------------------------------------------------------------------
  -- set_sliders (batch)
  -- -------------------------------------------------------------------
  describe "set_sliders" $ do
    it "sets multiple sliders and returns updated list" $ withCtx $ \ctx -> do
      let args = object ["values" .= object
                    [ "SliderGenScale" .= (0.3 :: Double)
                    , "SliderGenCoordScale" .= (0.7 :: Double)
                    ]]
      rsp <- dispatch ctx "set_sliders" args
      srSuccess rsp `shouldBe` True
      case lookupKey "updated" (srResult rsp) of
        Just (Array arr) -> length arr `shouldBe` 2
        _ -> expectationFailure "expected updated array"

    it "does not enqueue day/night rebuilds for authoritative geometry slider batches" $ withCtx $ \ctx -> do
      prepareReadyDayNight ctx
      let handles = ccActorHandles ctx
          args = object ["values" .= object
                    [ "SliderAxialTilt" .= (0.9 :: Double)
                    , "SliderPlanetRadius" .= (0.8 :: Double)
                    ]]
      version0 <- readSnapshotVersion (ahSnapshotVersionRef handles)

      rsp <- dispatch ctx "set_sliders" args
      srSuccess rsp `shouldBe` True

      readSnapshotVersion (ahSnapshotVersionRef handles) `shouldReturnSatisfying` (> version0)
      jobs <- drainAtlasJobs (ahAtlasManagerHandle handles)
      length jobs `shouldBe` 0

    it "reports unknown sliders" $ withCtx $ \ctx -> do
      let args = object ["values" .= object
                    [ "SliderGenScale" .= (0.3 :: Double)
                    , "FakeSlider" .= (0.5 :: Double)
                    ]]
      rsp <- dispatch ctx "set_sliders" args
      srSuccess rsp `shouldBe` True
      case lookupKey "unknown" (srResult rsp) of
        Just (Array arr) -> length arr `shouldBe` 1
        _ -> expectationFailure "expected unknown array"

    it "returns error when values param is missing" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "set_sliders" Null
      srSuccess rsp `shouldBe` False

  -- -------------------------------------------------------------------
  -- reset_sliders
  -- -------------------------------------------------------------------
  describe "reset_sliders" $ do
    it "resets all sliders" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "reset_sliders" Null
      srSuccess rsp `shouldBe` True
      case lookupKey "reset_count" (srResult rsp) of
        Just (Number n) -> n `shouldSatisfy` (> 0)
        _ -> expectationFailure "expected reset_count number"

    it "resets only a specific tab" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "reset_sliders" (object ["tab" .= ("terrain" :: String)])
      srSuccess rsp `shouldBe` True
      lookupKey "tab" (srResult rsp) `shouldBe` Just (String "terrain")

    it "does not enqueue day/night rebuilds when authoritative geometry sliders reset" $ withCtx $ \ctx -> do
      prepareReadyDayNight ctx
      let handles = ccActorHandles ctx
      changed <- dispatch ctx "set_slider" (object ["name" .= ("SliderAxialTilt" :: String), "value" .= (0.9 :: Double)])
      srSuccess changed `shouldBe` True
      _ <- drainAtlasJobs (ahAtlasManagerHandle handles)
      versionBeforeReset <- readSnapshotVersion (ahSnapshotVersionRef handles)

      rsp <- dispatch ctx "reset_sliders" Null
      srSuccess rsp `shouldBe` True

      readSnapshotVersion (ahSnapshotVersionRef handles) `shouldReturnSatisfying` (> versionBeforeReset)
      jobs <- drainAtlasJobs (ahAtlasManagerHandle handles)
      length jobs `shouldBe` 0

  -- -------------------------------------------------------------------
  -- pipeline and plugin mutations
  -- -------------------------------------------------------------------
  describe "get_pipeline" $ do
    it "exposes registry docs, dependency DAG, and stage diagnostics" $ withCtx $ \ctx -> do
      _ <- dispatch ctx "set_stage_enabled" (object ["stage" .= ("climate" :: String), "enabled" .= False])
      rsp <- dispatch ctx "get_pipeline" Null
      srSuccess rsp `shouldBe` True
      lookupKey "dag" (srResult rsp) `shouldSatisfy` maybe False valueHasNodesAndEdges
      lookupKey "docs" (srResult rsp) `shouldSatisfy` maybe False nonEmptyArray
      case lookupKey "stages" (srResult rsp) of
        Just (Array stages) -> do
          length stages `shouldSatisfy` (> 0)
          let stageValues = toList stages
          findStage "climate" stageValues `shouldSatisfy` maybe False (stageHasStatus "disabled")
          findStage "ocean-currents" stageValues `shouldSatisfy` maybe False (stageHasStatus "auto-disabled")
          findStage "weather" stageValues `shouldSatisfy` maybe False (valueHasKey "last_run")
        _ -> expectationFailure "expected stages array"

  describe "set_stage_enabled" $ do
    it "applies the built-in dependency closure used by UI stage toggles" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "set_stage_enabled" (object ["stage" .= ("climate" :: String), "enabled" .= False])
      srSuccess rsp `shouldBe` True
      ui <- getUiSnapshot (ahUiHandle (ccActorHandles ctx))
      Set.member StageClimate (uiDisabledStages ui) `shouldBe` True
      Set.member StageOceanCurrents (uiDisabledStages ui) `shouldBe` True
      Set.member StageWeather (uiDisabledStages ui) `shouldBe` True

    it "preserves overlapping explicit disables across later toggles" $ withCtx $ \ctx -> do
      climateOff <- dispatch ctx "set_stage_enabled" (object ["stage" .= ("climate" :: String), "enabled" .= False])
      srSuccess climateOff `shouldBe` True
      biomesOff <- dispatch ctx "set_stage_enabled" (object ["stage" .= ("biomes" :: String), "enabled" .= False])
      srSuccess biomesOff `shouldBe` True
      climateOn <- dispatch ctx "set_stage_enabled" (object ["stage" .= ("climate" :: String), "enabled" .= True])
      srSuccess climateOn `shouldBe` True
      ui <- getUiSnapshot (ahUiHandle (ccActorHandles ctx))
      Set.member StageClimate (uiDisabledStages ui) `shouldBe` False
      Set.member StageBiomes (uiDisabledStages ui) `shouldBe` True
      Set.member StageBiomes (uiExplicitDisabledStages ui) `shouldBe` True

  describe "set_plugin_param" $ do
    it "updates the UI plugin parameter snapshot as well as the plugin manager" $ withPluginCtx $ \ctx -> do
      rsp <- dispatch ctx "set_plugin_param" (object
        [ "plugin" .= ("example" :: String)
        , "param" .= ("enabled" :: String)
        , "value" .= Bool True
        ])
      srSuccess rsp `shouldBe` True
      ui <- getUiSnapshot (ahUiHandle (ccActorHandles ctx))
      (Map.lookup "example" (uiPluginParams ui) >>= Map.lookup "enabled") `shouldBe` Just (Bool True)

    it "rejects invalid updates without mutating the UI snapshot or plugin manager" $ withPluginCtx $ \ctx -> do
      seedRsp <- dispatch ctx "set_plugin_param" (object
        [ "plugin" .= ("example" :: String)
        , "param" .= ("enabled" :: String)
        , "value" .= Bool True
        ])
      srSuccess seedRsp `shouldBe` True

      let invalidCases =
            [ object ["plugin" .= ("missing" :: String), "param" .= ("enabled" :: String), "value" .= Bool False]
            , object ["plugin" .= ("example" :: String), "param" .= ("missing" :: String), "value" .= Bool False]
            , object ["plugin" .= ("example" :: String), "param" .= ("enabled" :: String), "value" .= String "yes"]
            , object ["plugin" .= ("example" :: String), "param" .= ("iterations" :: String), "value" .= Number 1.5]
            , object ["plugin" .= ("example" :: String), "param" .= ("density" :: String), "value" .= Number 2]
            ]
      forM_ invalidCases $ \payload -> do
        rsp <- dispatch ctx "set_plugin_param" payload
        srSuccess rsp `shouldBe` False

      ui <- getUiSnapshot (ahUiHandle (ccActorHandles ctx))
      (Map.lookup "example" (uiPluginParams ui) >>= Map.lookup "enabled") `shouldBe` Just (Bool True)
      loaded <- getLoadedPlugins (ahPluginManagerHandle (ccActorHandles ctx))
      let managerValue = do
            plugin <- findPlugin "example" loaded
            Map.lookup "enabled" (lpParams plugin)
      managerValue `shouldBe` Just (Bool True)

    it "loads all built-ins through the SDL preset dialog widget path" $ withCtx $ \ctx -> do
      let uiH = ahUiHandle (ccActorHandles ctx)
          click widgetId extra = dispatch ctx "click_widget" (object
            (["widget_id" .= (widgetId :: Text)] ++ extra))
      setUiShowConfig uiH True
      _ <- getUiSnapshot uiH

      openedForFilter <- click "WidgetConfigPresetLoad" []
      srSuccess openedForFilter `shouldBe` True
      setUiPresetFilter uiH "large ocean"
      displayFiltered <- dispatch ctx "get_dialog_state" Null
      lookupKey "preset_count" (srResult displayFiltered) `shouldBe` Just (Number 1)
      setUiPresetFilter uiH "built-in"
      markerFiltered <- dispatch ctx "get_dialog_state" Null
      lookupKey "preset_count" (srResult markerFiltered) `shouldBe` Just (Number 6)
      _ <- click "WidgetPresetLoadCancel" []

      forM_ (zip [0 :: Int ..] builtinWorldGenCases) $ \(index, (presetId, expectedConfig)) -> do
        opened <- click "WidgetConfigPresetLoad" []
        srSuccess opened `shouldBe` True
        dialog <- getUiSnapshot uiH
        uiPresetList dialog `shouldSatisfy` (presetId `elem`)
        selected <- click "WidgetPresetLoadItem" ["item_index" .= index]
        srSuccess selected `shouldBe` True
        loaded <- click "WidgetPresetLoadOk" []
        srSuccess loaded `shouldBe` True
        ui <- getUiSnapshot uiH
        uiMenuMode ui `shouldBe` MenuNone
        (csGenConfig <$> uiWorldConfig ui) `shouldBe` Just expectedConfig

    it "click_widget toggles plugin parameter checkboxes through pipeline action helpers" $ withPluginCtx $ \ctx -> do
      showPipelineWidgets ctx
      setUiPluginExpanded (ahUiHandle (ccActorHandles ctx)) "example" True
      _ <- getUiSnapshot (ahUiHandle (ccActorHandles ctx))
      seedRsp <- dispatch ctx "set_plugin_param" (object
        [ "plugin" .= ("example" :: String)
        , "param" .= ("enabled" :: String)
        , "value" .= Bool True
        ])
      srSuccess seedRsp `shouldBe` True
      seedUi <- getUiSnapshot (ahUiHandle (ccActorHandles ctx))
      (Map.lookup "example" (uiPluginParams seedUi) >>= Map.lookup "enabled") `shouldBe` Just (Bool True)
      rsp <- dispatch ctx "click_widget" (object
        [ "widget_id" .= ("WidgetPluginParamCheck:example:enabled" :: Text) ])
      srSuccess rsp `shouldBe` True
      ui <- getUiSnapshot (ahUiHandle (ccActorHandles ctx))
      (Map.lookup "example" (uiPluginParams ui) >>= Map.lookup "enabled") `shouldBe` Just (Bool False)

    it "returns accepted request metadata for asynchronous Data Browser clicks" $ withCtx $ \ctx -> do
      setUiShowConfig (ahUiHandle (ccActorHandles ctx)) True
      _ <- getUiSnapshot (ahUiHandle (ccActorHandles ctx))
      rsp <- dispatch ctx "click_widget" (object
        [ "widget_id" .= ("WidgetConfigTabData" :: Text) ])
      srSuccess rsp `shouldBe` True
      case srResult rsp of
        Object result -> do
          KM.lookup "status" result `shouldBe` Just (String "accepted")
          KM.lookup "operation" result `shouldBe` Just (String "load_catalog")
          KM.lookup "request_id" result `shouldSatisfy` maybe False (\case Number _ -> True; _ -> False)
          KM.lookup "info" result `shouldBe` Nothing
        value -> expectationFailure ("unexpected click response: " <> show value)

    it "rejects known widgets that are hidden by the current live surface" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "click_widget" (object
        [ "widget_id" .= ("WidgetViewBaseBiome" :: Text) ])
      srSuccess rsp `shouldBe` False
      srError rsp `shouldSatisfy` maybe False (Text.isInfixOf "not visible")

    it "toggles the log header with a real completed transition" $ withCtx $ \ctx -> do
      let logH = ahLogHandle (ccActorHandles ctx)
      before <- getLogSnapshot logH
      rsp <- dispatch ctx "click_widget" (object
        [ "widget_id" .= ("WidgetLogHeader" :: Text) ])
      srSuccess rsp `shouldBe` True
      case srResult rsp of
        Object result -> do
          KM.lookup "status" result `shouldBe` Just (String "completed")
          KM.lookup "changed" result `shouldBe` Just (Bool True)
        value -> expectationFailure ("unexpected click response: " <> show value)
      after <- getLogSnapshot logH
      lsCollapsed after `shouldBe` not (lsCollapsed before)

    it "maps normalized plugin slider positions through the manifest range" $ withPluginCtx $ \ctx -> do
      showPipelineWidgets ctx
      let uiH = ahUiHandle (ccActorHandles ctx)
      setUiPluginExpanded uiH "example" True
      _ <- getUiSnapshot uiH
      rsp <- dispatch ctx "click_widget" (object
        [ "widget_id" .= ("WidgetPluginParamSlider:example:density" :: Text)
        , "normalized_position" .= (0.25 :: Double)
        ])
      srSuccess rsp `shouldBe` True
      ui <- getUiSnapshot uiH
      (Map.lookup "example" (uiPluginParams ui) >>= Map.lookup "density")
        `shouldBe` Just (Number 0.25)

    it "executes all nine editor tools and context-sensitive controls" $ withCtx $ \ctx -> do
      let click wid = dispatch ctx "click_widget" (object ["widget_id" .= (wid :: Text)])
      openRsp <- click "WidgetEditorReopen"
      srSuccess openRsp `shouldBe` True
      forM_ [0 .. 8 :: Int] $ \index -> do
        rsp <- click ("WidgetEditorTool:" <> Text.pack (show index))
        srSuccess rsp `shouldBe` True
      radiusRsp <- click "WidgetEditorRadiusPlus"
      srSuccess radiusRsp `shouldBe` True
      toolRsp <- click "WidgetEditorTool:4"
      srSuccess toolRsp `shouldBe` True
      frequencyRsp <- click "WidgetEditorParamPlus:0"
      strengthRsp <- click "WidgetEditorParamMinus:1"
      falloffRsp <- click "WidgetEditorFalloffNext"
      map srSuccess [frequencyRsp, strengthRsp, falloffRsp] `shouldBe` [True, True, True]
      editorTool . uiEditor <$> getUiSnapshot (ahUiHandle (ccActorHandles ctx))
        `shouldReturn` ToolNoise

  describe "click_widget layered view controls" $ do
    it "selects base view, weather overlay, and basis independently" $ withCtx $ \ctx -> do
      showLeftViewWidgets ctx
      baseBaseline <- beginPublicationAssertion ctx
      baseRsp <- dispatch ctx "click_widget" (object
        [ "widget_id" .= ("WidgetViewBaseBiome" :: Text) ])
      srSuccess baseRsp `shouldBe` True
      (_, baseCommitted) <- expectExactPublication ctx baseBaseline
      lvsBaseView (uiViewSelection (rsUi baseCommitted)) `shouldBe` BaseViewBiome
      _ <- drainAtlasJobs (ahAtlasManagerHandle (ccActorHandles ctx))

      overlayBaseline <- beginPublicationAssertion ctx
      overlayRsp <- dispatch ctx "click_widget" (object
        [ "widget_id" .= ("WidgetViewOverlayTemperature" :: Text) ])
      srSuccess overlayRsp `shouldBe` True
      (_, overlayCommitted) <- expectExactPublication ctx overlayBaseline
      lvsBaseView (uiViewSelection (rsUi overlayCommitted)) `shouldBe` BaseViewBiome
      lvsSkyOverlay (uiViewSelection (rsUi overlayCommitted))
        `shouldBe` Just SkyOverlayWeatherTemperature
      _ <- drainAtlasJobs (ahAtlasManagerHandle (ccActorHandles ctx))

      basisBaseline <- beginPublicationAssertion ctx
      basisRsp <- dispatch ctx "click_widget" (object
        [ "widget_id" .= ("WidgetViewBasisAverage" :: Text) ])
      srSuccess basisRsp `shouldBe` True
      (_, basisCommitted) <- expectExactPublication ctx basisBaseline
      lvsBaseView (uiViewSelection (rsUi basisCommitted)) `shouldBe` BaseViewBiome
      lvsSkyOverlay (uiViewSelection (rsUi basisCommitted))
        `shouldBe` Just SkyOverlayWeatherTemperature
      lvsWeatherBasis (uiViewSelection (rsUi basisCommitted)) `shouldBe` WeatherBasisAverage

    it "keeps legacy weather widget ids accepted as full-mode compatibility" $ withCtx $ \ctx -> do
      showLeftViewWidgets ctx
      let uiH = ahUiHandle (ccActorHandles ctx)
      seedRsp <- dispatch ctx "click_widget" (object
        [ "widget_id" .= ("WidgetViewBaseBiome" :: Text) ])
      srSuccess seedRsp `shouldBe` True
      overlayRsp <- dispatch ctx "click_widget" (object
        [ "widget_id" .= ("WidgetViewOverlayTemperature" :: Text) ])
      srSuccess overlayRsp `shouldBe` True

      rsp <- dispatch ctx "click_widget" (object
        [ "widget_id" .= ("WidgetViewPrecipCurrent" :: Text) ])
      srSuccess rsp `shouldBe` True
      (awaitTrue 50 $ do
        ui <- getUiSnapshot uiH
        pure (uiViewMode ui == ViewPrecipCurrent
          && lvsBaseView (uiViewSelection ui) == BaseViewElevation
          && lvsSkyOverlay (uiViewSelection ui) == Just SkyOverlayPrecipitation
          && lvsWeatherBasis (uiViewSelection ui) == WeatherBasisCurrent))
        `shouldReturn` True

      clearRsp <- dispatch ctx "click_widget" (object
        [ "widget_id" .= ("WidgetViewElevation" :: Text) ])
      srSuccess clearRsp `shouldBe` True
      (awaitTrue 50 $ do
        ui <- getUiSnapshot uiH
        pure (uiViewMode ui == ViewElevation
          && lvsBaseView (uiViewSelection ui) == BaseViewElevation
          && lvsSkyOverlay (uiViewSelection ui) == Nothing))
        `shouldReturn` True

  describe "click_widget day/night" $ do
    it "routes the toggle through UiActions, bumps the snapshot version, and enqueues current atlas jobs" $ withCtx $ \ctx -> do
      showLeftViewWidgets ctx
      let handles = ccActorHandles ctx
          logH = ahLogHandle handles
          atlasH = ahAtlasManagerHandle handles
      startLogs <- toggleLogCount logH
      baseline <- beginPublicationAssertion ctx

      rsp <- dispatch ctx "click_widget" (object
        [ "widget_id" .= ("WidgetDayNightToggle" :: Text) ])
      srSuccess rsp `shouldBe` True

      (version1, committed) <- expectExactPublication ctx baseline
      let ui = rsUi committed
          expected = atlasJobsForSelection
            version1
            (effectiveViewSelection ui)
            (uiRenderWaterLevel ui)
            (rsTerrain committed)
            (orderedZoomStagesForZoom (uiZoom ui))
            Nothing
      uiDayNightEnabled ui `shouldBe` True
      length
        [ ()
        | entry <- lsEntries (rsLog committed)
        , "Toggle Day/Night" `Text.isInfixOf` leMessage entry
        ] `shouldBe` startLogs + 1
      jobs <- drainAtlasJobs atlasH
      assertAtlasJobsMatch jobs expected

    it "publishes the latest authoritative terrain context before toggling day/night" $ withCtx $ \ctx -> do
      showLeftViewWidgets ctx
      _ <- writeReadyTerrainData ctx
      let handles = ccActorHandles ctx
          logH = ahLogHandle handles
          atlasH = ahAtlasManagerHandle handles
          latestTime = WorldTime 7 simulationTickSeconds
      setTerrainGeoContextData (ahDataHandle handles) (defaultTerrainGeoContext { tgcWorldTime = latestTime })
      startLogs <- toggleLogCount logH

      rsp <- dispatch ctx "click_widget" (object
        [ "widget_id" .= ("WidgetDayNightToggle" :: Text) ])
      srSuccess rsp `shouldBe` True
      awaitToggleLogCount logH (startLogs + 1) `shouldReturn` True

      published <- readTerrainSnapshot (ahTerrainSnapshotRef handles)
      tgcWorldTime (tsGeoContext published) `shouldBe` latestTime
      jobs <- drainAtlasJobs atlasH
      length jobs `shouldSatisfy` (> 0)
      map (tgcWorldTime . tsGeoContext . ajTerrain) jobs `shouldSatisfy` all (== latestTime)

    it "serializes rapid day/night clicks against the latest UI state" $ withCtx $ \ctx -> do
      showLeftViewWidgets ctx
      let handles = ccActorHandles ctx
          uiH = ahUiHandle handles
          logH = ahLogHandle handles
          atlasH = ahAtlasManagerHandle handles
      startLogs <- toggleLogCount logH
      version0 <- readSnapshotVersion (ahSnapshotVersionRef handles)

      rsp1 <- dispatch ctx "click_widget" (object
        [ "widget_id" .= ("WidgetDayNightToggle" :: Text) ])
      rsp2 <- dispatch ctx "click_widget" (object
        [ "widget_id" .= ("WidgetDayNightToggle" :: Text) ])
      srSuccess rsp1 `shouldBe` True
      srSuccess rsp2 `shouldBe` True
      awaitToggleLogCount logH (startLogs + 2) `shouldReturn` True

      ui <- getUiSnapshot uiH
      uiDayNightEnabled ui `shouldBe` False
      versionFinal <- readSnapshotVersion (ahSnapshotVersionRef handles)
      unSnapshotVersion versionFinal `shouldSatisfy` (>= unSnapshotVersion version0 + 2)
      jobs <- drainAtlasJobs atlasH
      let expectedStages = map zoomStagePair (orderedZoomStagesForZoom (uiZoom ui))
      length jobs `shouldBe` length expectedStages
      map ajViewMode jobs `shouldBe` replicate (length expectedStages) (uiViewMode ui)
      map atlasJobStage jobs `shouldBe` expectedStages
      map ajSnapshotVersion jobs `shouldSatisfy` all (== versionFinal)

  describe "camera and viewport atlas refreshes" $ do
    it "set_camera bumps the snapshot and queues a current-stage viewport refresh" $ withCtx $ \ctx -> do
      expectViewportRefreshForCommand ctx "set_camera" (object
        [ "x" .= (12.0 :: Double)
        , "y" .= ((-4.0) :: Double)
        , "zoom" .= (1.5 :: Double)
        ]) $ \_ ui -> do
          uiPanOffset ui `shouldBe` (12, -4)
          uiZoom ui `shouldBe` 1.5

    it "zoom_to_chunk bumps the snapshot and queues a current-stage viewport refresh" $ withCtx $ \ctx -> do
      let ChunkId chunkKey = chunkIdFromCoord (ChunkCoord 0 0)
      expectViewportRefreshForCommand ctx "zoom_to_chunk" (object ["chunk" .= chunkKey]) $ \ui0 ui1 -> do
        uiPanOffset ui1 `shouldNotBe` uiPanOffset ui0
        uiZoom ui1 `shouldBe` 1

    it "viewport_scroll bumps the snapshot and queues a current-stage viewport refresh" $ withCtx $ \ctx -> do
      expectViewportRefreshForCommand ctx "viewport_scroll" (object
        [ "delta" .= (1 :: Int)
        , "x" .= (320 :: Int)
        , "y" .= (240 :: Int)
        ]) $ \ui0 ui1 -> uiZoom ui1 `shouldNotBe` uiZoom ui0

    it "applies every viewport_scroll wheel step and preserves cursor anchoring" $ withCtx $ \ctx -> do
      let cursor = (120, 80) :: (Int, Int)
      expectViewportRefreshForCommand ctx "viewport_scroll" (object
        [ "delta" .= (3 :: Int)
        , "x" .= fst cursor
        , "y" .= snd cursor
        ]) $ \ui0 ui1 -> do
          uiZoom ui1 `shouldSatisfy` (\zoom -> abs (zoom - uiZoom ui0 * 1.1 ^ (3 :: Int)) < 0.0001)
          let (x0, y0) = uiPanOffset ui0
              (x1, y1) = uiPanOffset ui1
              expectedX = x0 + fromIntegral (fst cursor) * (1 / uiZoom ui1 - 1 / uiZoom ui0)
              expectedY = y0 + fromIntegral (snd cursor) * (1 / uiZoom ui1 - 1 / uiZoom ui0)
          abs (x1 - expectedX) `shouldSatisfy` (< 0.0001)
          abs (y1 - expectedY) `shouldSatisfy` (< 0.0001)

    it "keeps zero-delta viewport_scroll publication and atlas state unchanged" $ withCtx $ \ctx -> do
      _ <- writeReadyTerrainData ctx
      let handles = ccActorHandles ctx
          atlasH = ahAtlasManagerHandle handles
      _ <- drainAtlasJobs atlasH
      baseline <- beginPublicationAssertion ctx
      rsp <- dispatch ctx "viewport_scroll" (object
        [ "delta" .= (0 :: Int)
        , "x" .= (120 :: Int)
        , "y" .= (80 :: Int)
        ])
      srSuccess rsp `shouldBe` True
      lookupKey "steps" (srResult rsp) `shouldBe` Just (Number 0)
      readCommittedRenderSnapshot (ahSnapshotVersionRef handles)
        `shouldReturn` (pbVersion baseline, pbSnapshot baseline)
      jobs <- drainAtlasJobs atlasH
      length jobs `shouldBe` 0

    it "viewport_drag bumps the snapshot and queues a current-stage viewport refresh" $ withCtx $ \ctx -> do
      expectViewportRefreshForCommand ctx "viewport_drag" (object
        [ "x1" .= (0 :: Int)
        , "y1" .= (0 :: Int)
        , "x2" .= (30 :: Int)
        , "y2" .= ((-10) :: Int)
        ]) $ \ui0 ui1 -> uiPanOffset ui1 `shouldNotBe` uiPanOffset ui0

    it "treats an exact-threshold viewport drag as a right click" $ withCtx $ \ctx -> do
      _ <- writeReadyTerrainData ctx
      let handles = ccActorHandles ctx
          atlasH = ahAtlasManagerHandle handles
      _ <- drainAtlasJobs atlasH
      baseline <- beginPublicationAssertion ctx

      rsp <- dispatch ctx "viewport_drag" (object
        [ "x1" .= (0 :: Int), "y1" .= (0 :: Int)
        , "x2" .= (4 :: Int), "y2" .= (0 :: Int)
        ])
      srSuccess rsp `shouldBe` True
      (_, committed) <- expectExactPublication ctx baseline
      uiPanOffset (rsUi committed) `shouldBe` uiPanOffset (rsUi (pbSnapshot baseline))
      uiHexTooltipPinned (rsUi committed) `shouldBe` True
      jobs <- drainAtlasJobs atlasH
      length jobs `shouldBe` 0

    it "classifies and pans near-boundary viewport coordinates exactly" $ withCtx $ \ctx -> do
      let startX = maxBound - 5 :: Int
          endX = maxBound :: Int
      expectViewportRefreshForCommand ctx "viewport_drag" (object
        [ "x1" .= startX, "y1" .= (0 :: Int)
        , "x2" .= endX, "y2" .= (0 :: Int)
        ]) $ \ui0 ui1 -> do
          let (x0, y0) = uiPanOffset ui0
              (x1, y1) = uiPanOffset ui1
          abs (x1 - (x0 + 5 / uiZoom ui0)) `shouldSatisfy` (< 0.0001)
          y1 `shouldBe` y0
          uiHexTooltipPinned ui1 `shouldBe` uiHexTooltipPinned ui0

    it "toggles tooltip pinning for an explicit right viewport click" $ withCtx $ \ctx -> do
      let handles = ccActorHandles ctx
          atlasH = ahAtlasManagerHandle handles
      baseline <- beginPublicationAssertion ctx
      rsp <- dispatch ctx "viewport_click" (object
        [ "x" .= (999999 :: Int), "y" .= (999999 :: Int)
        , "button" .= ("right" :: Text)
        ])
      srSuccess rsp `shouldBe` True
      lookupKey "tooltip_pinned" (srResult rsp) `shouldBe` Just (Bool True)
      (_, committed) <- expectExactPublication ctx baseline
      uiHexTooltipPinned (rsUi committed) `shouldBe` True
      jobs <- drainAtlasJobs atlasH
      length jobs `shouldBe` 0

    it "clears stale selection and pin state for an invalid left viewport click" $ withCtx $ \ctx -> do
      selected <- dispatch ctx "select_hex" (object ["q" .= (2 :: Int), "r" .= ((-3) :: Int)])
      srSuccess selected `shouldBe` True
      let handles = ccActorHandles ctx
          atlasH = ahAtlasManagerHandle handles
      baseline <- beginPublicationAssertion ctx
      rsp <- dispatch ctx "viewport_click" (object
        [ "x" .= (999999 :: Int), "y" .= (999999 :: Int)
        , "button" .= ("left" :: Text)
        ])
      srSuccess rsp `shouldBe` True
      lookupKey "selected" (srResult rsp) `shouldBe` Just (Bool False)
      (_, committed) <- expectExactPublication ctx baseline
      uiHoverHex (rsUi committed) `shouldBe` Nothing
      uiContextHex (rsUi committed) `shouldBe` Nothing
      uiHexTooltipPinned (rsUi committed) `shouldBe` False
      jobs <- drainAtlasJobs atlasH
      length jobs `shouldBe` 0

    it "selects valid terrain without painting while the editor is inactive" $ withCtx $ \ctx -> do
      _ <- writeReadyTerrainData ctx
      let handles = ccActorHandles ctx
          atlasH = ahAtlasManagerHandle handles
      _ <- drainAtlasJobs atlasH
      rsp <- dispatch ctx "viewport_click" (object
        ["x" .= (40 :: Int), "y" .= (80 :: Int), "button" .= ("left" :: Text)])
      srSuccess rsp `shouldBe` True
      lookupKey "selected" (srResult rsp) `shouldBe` Just (Bool True)
      lookupKey "editor_stroke" (srResult rsp) `shouldBe` Just (Bool False)
      ui <- getUiSnapshot (ahUiHandle handles)
      uiContextHex ui `shouldBe` Just (0, 0)
      uiHexTooltipPinned ui `shouldBe` True
      jobs <- drainAtlasJobs atlasH
      length jobs `shouldBe` 0

    it "bounds discrete flatten viewport clicks with cleared stroke sessions" $ withCtx $ \ctx -> do
      before <- writeReadyTerrainData ctx
      let handles = ccActorHandles ctx
          uiH = ahUiHandle handles
      activeRsp <- dispatch ctx "editor_toggle" (object ["active" .= True])
      toolRsp <- dispatch ctx "editor_set_tool" (object ["tool" .= ("flatten" :: Text)])
      srSuccess activeRsp `shouldBe` True
      srSuccess toolRsp `shouldBe` True

      -- A stale remote-session reference would alter this flat fixture unless
      -- viewport_click clears it before starting its discrete stroke.
      configured <- getUiSnapshot uiH
      setUiEditor uiH ((uiEditor configured) { editorFlattenRef = Just 0.99 })
      _ <- getUiSnapshot uiH

      first <- dispatch ctx "viewport_click" (object ["x" .= (40 :: Int), "y" .= (80 :: Int)])
      srSuccess first `shouldBe` True
      lookupKey "editor_stroke" (srResult first) `shouldBe` Just (Bool True)
      afterFirst <- getTerrainSnapshot (ahDataHandle handles)
      fmap tcElevation (tsTerrainChunks afterFirst)
        `shouldBe` fmap tcElevation (tsTerrainChunks before)
      (editorFlattenRef . uiEditor <$> getUiSnapshot uiH) `shouldReturn` Nothing

      second <- dispatch ctx "viewport_click" (object ["x" .= (40 :: Int), "y" .= (80 :: Int)])
      srSuccess second `shouldBe` True
      lookupKey "editor_stroke" (srResult second) `shouldBe` Just (Bool True)
      (editorFlattenRef . uiEditor <$> getUiSnapshot uiH) `shouldReturn` Nothing

    it "viewport_hover does not enqueue a viewport atlas refresh" $ withCtx $ \ctx -> do
      _ <- writeReadyTerrainData ctx
      let handles = ccActorHandles ctx
          atlasH = ahAtlasManagerHandle handles
      baseline <- beginPublicationAssertion ctx

      rsp <- dispatch ctx "viewport_hover" (object ["x" .= (0 :: Int), "y" .= (0 :: Int)])
      srSuccess rsp `shouldBe` True

      (_, committed) <- expectExactPublication ctx baseline
      lookupKey "valid" (srResult rsp) `shouldBe` Just (Bool False)
      uiHoverHex (rsUi committed) `shouldBe` Nothing
      rsLog committed `shouldBe` rsLog (pbSnapshot baseline)
      rsData committed `shouldBe` rsData (pbSnapshot baseline)
      rsTerrain committed `shouldBe` rsTerrain (pbSnapshot baseline)
      jobs <- drainAtlasJobs atlasH
      length jobs `shouldBe` 0

  describe "set_sim_auto_tick" $ do
    it "sets and clamps the normalized auto tick rate" $ withCtx $ \ctx -> do
      highRsp <- dispatch ctx "set_sim_auto_tick" (object ["enabled" .= True, "rate" .= (5.0 :: Double)])
      srSuccess highRsp `shouldBe` True
      lookupKey "auto_tick" (srResult highRsp) `shouldBe` Just (Bool True)
      lookupKey "rate" (srResult highRsp) `shouldBe` Just (Number 1)
      highUi <- getUiSnapshot (ahUiHandle (ccActorHandles ctx))
      uiSimAutoTick highUi `shouldBe` True
      uiSimTickRate highUi `shouldBe` 1

      zeroRsp <- dispatch ctx "set_sim_auto_tick" (object ["enabled" .= True, "rate" .= (0.0 :: Double)])
      srSuccess zeroRsp `shouldBe` True
      lookupKey "auto_tick" (srResult zeroRsp) `shouldBe` Just (Bool True)
      lookupKey "rate" (srResult zeroRsp) `shouldBe` Just (Number 0)
      zeroUi <- getUiSnapshot (ahUiHandle (ccActorHandles ctx))
      uiSimAutoTick zeroUi `shouldBe` True
      uiSimTickRate zeroUi `shouldBe` 0

  describe "sim_tick" $ do
    it "rejects ticks when no world terrain is loaded" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "sim_tick" Null
      srSuccess rsp `shouldBe` False

    it "rejects ticks while world generation is in progress" $ withCtx $ \ctx -> do
      let handles = ccActorHandles ctx
      setTerrainChunkCount (ahDataHandle handles) 1
      setUiGenerating (ahUiHandle handles) True
      _ <- getUiSnapshot (ahUiHandle handles)
      rsp <- dispatch ctx "sim_tick" (object ["count" .= (1 :: Int)])
      srSuccess rsp `shouldBe` False
      srError rsp `shouldSatisfy` maybe False (Text.isInfixOf "generation")

    it "requests a tick when world terrain is loaded" $ withCtx $ \ctx -> do
      setTerrainChunkCount (ahDataHandle (ccActorHandles ctx)) 1
      rsp <- dispatch ctx "sim_tick" (object ["count" .= (1 :: Int)])
      srSuccess rsp `shouldBe` True
      lookupKey "target_tick" (srResult rsp) `shouldBe` Just (Number 1)

  -- -------------------------------------------------------------------
  -- select_hex
  -- -------------------------------------------------------------------
  describe "select_hex" $ do
    it "selects a hex by q and r" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "select_hex" (object ["q" .= (0 :: Int), "r" .= (5 :: Int)])
      srSuccess rsp `shouldBe` True
      lookupKey "selected" (srResult rsp) `shouldBe` Just (Bool True)
      lookupKey "q" (srResult rsp) `shouldBe` Just (Number 0)
      lookupKey "r" (srResult rsp) `shouldBe` Just (Number 5)

    it "deselects when no chunk/tile given" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "select_hex" Null
      srSuccess rsp `shouldBe` True
      lookupKey "selected" (srResult rsp) `shouldBe` Just (Bool False)

    it "deselects with empty params" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "select_hex" (object [])
      srSuccess rsp `shouldBe` True
      lookupKey "selected" (srResult rsp) `shouldBe` Just (Bool False)

  -- -------------------------------------------------------------------
  -- list_presets
  -- -------------------------------------------------------------------
  describe "list_presets" $ do
    it "returns the six namespaced read-only built-ins through AppService" $ withCtx $ \ctx -> do
      result <- runAppServiceOperation commandAppService ctx "list_presets" Null
      body <- expectServiceBody result
      lookupKey "presets" body `shouldSatisfy` maybe False (allBuiltinIdsPresent . toListValue)
      lookupKey "entries" body `shouldSatisfy` maybe False (allBuiltinEntriesReadOnly . toListValue)

    it "returns success with preset list" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "list_presets" Null
      srSuccess rsp `shouldBe` True
      case srResult rsp of
        Object o -> do
          KM.member "preset_count" o `shouldBe` True
          KM.member "entries" o `shouldBe` True
        _ -> expectationFailure "expected JSON object in result"

  -- -------------------------------------------------------------------
  -- save_preset / load_preset
  -- -------------------------------------------------------------------
  describe "save_preset" $ do
    it "returns error when name is missing" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "save_preset" Null
      srSuccess rsp `shouldBe` False

    it "returns error for empty name" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "save_preset" (object ["name" .= ("" :: String)])
      srSuccess rsp `shouldBe` False

    it "rejects path traversal names" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "save_preset" (object ["name" .= ("../sibling" :: Text)])
      srSuccess rsp `shouldBe` False
      srError rsp `shouldSatisfy` maybe False (Text.isInfixOf "path separators")

    it "cannot overwrite a built-in ID" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "save_preset" (object ["name" .= ("builtin:continental" :: Text)])
      srSuccess rsp `shouldBe` False

    it "preserves user preset round-trips through the service" $ withCtx $ \ctx -> do
      let presetName = "__test_service_user_round_trip__"
          uiH = ahUiHandle (ccActorHandles ctx)
      dir <- snapshotDir
      let path = dir </> Text.unpack presetName <> ".json"
          cleanup = do
            exists <- doesFileExist path
            when exists (removeFile path)
      cleanup
      (do
          _ <- dispatch ctx "set_seed" (object ["seed" .= (777 :: Int)])
          saved <- dispatch ctx "save_preset" (object ["name" .= presetName])
          srSuccess saved `shouldBe` True
          _ <- dispatch ctx "set_seed" (object ["seed" .= (42 :: Int)])
          loaded <- dispatch ctx "load_preset" (object ["name" .= presetName])
          srSuccess loaded `shouldBe` True
          uiSeed <$> getUiSnapshot uiH `shouldReturn` 777)
        `finally` cleanup

  describe "load_preset" $ do
    it "loads every built-in's full WorldGenConfig through AppService" $ withCtx $ \ctx -> do
      let uiH = ahUiHandle (ccActorHandles ctx)
      forM_ builtinWorldGenCases $ \(presetId, expectedConfig) -> do
        result <- runAppServiceOperation commandAppService ctx "load_preset" (object ["name" .= presetId])
        _ <- expectServiceBody result
        ui <- getUiSnapshot uiH
        (csGenConfig <$> uiWorldConfig ui) `shouldBe` Just expectedConfig

    it "returns error when name is missing" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "load_preset" Null
      srSuccess rsp `shouldBe` False

    it "returns error for non-existent preset" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "load_preset" (object ["name" .= ("__nonexistent_test_preset__" :: String)])
      srSuccess rsp `shouldBe` False

    it "rejects drive-form names" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "load_preset" (object ["name" .= ("C:outside" :: Text)])
      srSuccess rsp `shouldBe` False
      srError rsp `shouldSatisfy` maybe False (Text.isInfixOf "drive path")

  -- -------------------------------------------------------------------
  -- save_world / load_world
  -- -------------------------------------------------------------------
  describe "save_world" $ do
    it "returns error when name is missing" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "save_world" Null
      srSuccess rsp `shouldBe` False

    it "returns error for empty name" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "save_world" (object ["name" .= ("" :: String)])
      srSuccess rsp `shouldBe` False

    it "rejects path traversal names" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "save_world" (object ["name" .= ("../sibling" :: Text)])
      srSuccess rsp `shouldBe` False
      srError rsp `shouldSatisfy` maybe False (Text.isInfixOf "path separators")

  describe "load_world" $ do
    it "returns error when name is missing" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "load_world" Null
      srSuccess rsp `shouldBe` False

    it "returns error for non-existent world" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "load_world" (object ["name" .= ("__nonexistent_test_world__" :: String)])
      srSuccess rsp `shouldBe` False

    it "resets UI-only layered view selection to defaults after load" $ withCtx $ \ctx -> do
      let worldName = "__topo_command_dispatch_load_reset__"
      _ <- deleteNamedWorld worldName
      (do
        _ <- writeReadyTerrainData ctx
        saveBaseline <- beginPublicationAssertion ctx
        saveRsp <- dispatch ctx "save_world" (object ["name" .= worldName])
        srSuccess saveRsp `shouldBe` True
        (_, saved) <- expectExactPublication ctx saveBaseline
        uiWorldName (rsUi saved) `shouldBe` worldName
        uiWorldConfig (rsUi saved) `shouldSatisfy` maybe False (const True)
        rsLog saved `shouldBe` rsLog (pbSnapshot saveBaseline)
        rsData saved `shouldBe` rsData (pbSnapshot saveBaseline)
        rsTerrain saved `shouldBe` rsTerrain (pbSnapshot saveBaseline)
        saveJobs <- drainAtlasJobs (ahAtlasManagerHandle (ccActorHandles ctx))
        length saveJobs `shouldBe` 0

        viewRsp <- dispatch ctx "set_view_mode" (object ["mode" .= ("cloud" :: Text), "basis" .= ("typical" :: Text)])
        srSuccess viewRsp `shouldBe` True
        _ <- drainAtlasJobs (ahAtlasManagerHandle (ccActorHandles ctx))
        loadBaseline <- beginPublicationAssertion ctx

        loadRsp <- dispatch ctx "load_world" (object ["name" .= worldName])
        srSuccess loadRsp `shouldBe` True
        (loadVersion, loaded) <- expectExactPublication ctx loadBaseline
        uiWorldName (rsUi loaded) `shouldBe` worldName
        uiWorldConfig (rsUi loaded) `shouldBe` uiWorldConfig (rsUi saved)
        effectiveViewSelection (rsUi loaded) `shouldBe` defaultLayeredViewState
        effectiveViewSelection (rsUi loaded) `shouldBe` effectiveViewSelection (rsUi saved)
        uiSimTickCount (rsUi loaded) `shouldBe` uiSimTickCount (rsUi saved)
        uiOverlayNames (rsUi loaded) `shouldBe` uiOverlayNames (rsUi saved)
        rsLog loaded `shouldBe` rsLog (pbSnapshot loadBaseline)
        authoritativeData <- getDataSnapshot (ahDataHandle (ccActorHandles ctx))
        authoritativeTerrain <- getTerrainSnapshot (ahDataHandle (ccActorHandles ctx))
        rsData loaded `shouldBe` authoritativeData
        rsTerrain loaded `shouldBe` authoritativeTerrain
        loadJobs <- drainAtlasJobs (ahAtlasManagerHandle (ccActorHandles ctx))
        let loadedUi = rsUi loaded
            expectedLoadJobs = atlasJobsForSelection
              loadVersion
              (effectiveViewSelection loadedUi)
              (uiRenderWaterLevel loadedUi)
              (rsTerrain loaded)
              (orderedZoomStagesForZoom (uiZoom loadedUi))
              Nothing
        assertAtlasJobsMatch loadJobs expectedLoadJobs

        uiRsp <- dispatch ctx "get_ui_state" Null
        srSuccess uiRsp `shouldBe` True
        case lookupKey "view" (srResult uiRsp) of
          Just (Object view) -> do
            KM.lookup "mode" view `shouldBe` Just (String "elevation")
            case KM.lookup "selection" view of
              Just (Object selection) -> do
                KM.lookup "base" selection `shouldBe` Just (String "elevation")
                KM.lookup "overlay" selection `shouldBe` Just Null
                KM.lookup "weather_basis" selection `shouldBe` Just (String "current")
              _ -> expectationFailure "expected get_ui_state.view.selection object"
          _ -> expectationFailure "expected get_ui_state.view object")
        `finally` (deleteNamedWorld worldName >> pure ())

  describe "delete_world" $ do
    it "refreshes saved worlds while leaving the current in-memory world unchanged" $ withCtx $ \ctx -> do
      let worldName = "__topo_command_dispatch_delete__"
      _ <- deleteNamedWorld worldName
      finally
        (do
            _ <- writeReadyTerrainData ctx
            saveRsp <- dispatch ctx "save_world" (object ["name" .= worldName])
            srSuccess saveRsp `shouldBe` True
            before <- beginPublicationAssertion ctx
            beforeWorlds <- dispatch ctx "list_worlds" Null
            worldResponseContains worldName beforeWorlds `shouldBe` True

            deleteRsp <- dispatch ctx "delete_world" (object ["name" .= worldName])
            srSuccess deleteRsp `shouldBe` True
            lookupKey "deleted" (srResult deleteRsp) `shouldBe` Just (Bool True)
            (_, afterDelete) <- expectExactPublication ctx before
            uiWorldName (rsUi afterDelete) `shouldBe` uiWorldName (rsUi (pbSnapshot before))
            uiWorldConfig (rsUi afterDelete) `shouldBe` uiWorldConfig (rsUi (pbSnapshot before))
            rsData afterDelete `shouldBe` rsData (pbSnapshot before)
            rsTerrain afterDelete `shouldBe` rsTerrain (pbSnapshot before)
            map wsmName (uiWorldList (rsUi afterDelete))
              `shouldSatisfy` notElem worldName

            afterWorlds <- dispatch ctx "list_worlds" Null
            worldResponseContains worldName afterWorlds `shouldBe` False
            missing <- dispatch ctx "delete_world" (object ["name" .= worldName])
            srSuccess missing `shouldBe` False
        )
        (deleteNamedWorld worldName >> pure ())

    it "rejects traversal before deletion" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "delete_world" (object ["name" .= ("../sibling" :: Text)])
      srSuccess rsp `shouldBe` False
      srError rsp `shouldSatisfy` maybe False (Text.isInfixOf "path separators")

  -- -------------------------------------------------------------------
  -- take_screenshot
  -- -------------------------------------------------------------------
  describe "take_screenshot" $ do
    it "returns the shared renderer response contract for capture-only commands" $
      withCtx $ \ctx -> do
        _ <- forkIO (serveScreenshotRequest ctx (BL.toStrict "png-bytes"))
        rsp <- dispatch ctx "take_screenshot" Null
        srSuccess rsp `shouldBe` True
        lookupKey "format" (srResult rsp) `shouldBe` Just (String "png")
        lookupKey "source" (srResult rsp) `shouldBe` Just (String "renderer")
        lookupKey "saved_path" (srResult rsp) `shouldBe` Just Null
        lookupKey "image_base64" (srResult rsp)
          `shouldSatisfy` \case
            Just (String encoded) -> not (Text.null encoded)
            _ -> False

    it "returns error when a screenshot is already in progress" $ withCtx $ \ctx -> do
      submitted <- submitScreenshotRequest (ccScreenshotRef ctx)
      submitted `shouldSatisfy` either (const False) (const True)
      rsp <- dispatch ctx "take_screenshot" Null
      srId rsp `shouldBe` 1
      srSuccess rsp `shouldBe` False
      srError rsp `shouldBe` Just
        "a screenshot is already in progress; please wait and retry"

    it "maps broker outcomes through the real service handler" $ withCtx $ \ctx -> do
      let run deadline body = runServiceHandler
            (rendererScreenshotHandlerWithDeadline deadline)
            (commandTestServiceContext ctx)
            (ServiceRequest (Just body))
          assertServiceError expectedStatus expectedCode expectedText result =
            case result of
              Left err -> do
                serviceErrorHTTPStatus err `shouldBe` expectedStatus
                serviceErrorCode err `shouldBe` expectedCode
                serviceErrorText err `shouldBe` expectedText
              Right _ -> expectationFailure "expected screenshot service error"

      deadlineResult <- run (pure ()) Null
      assertServiceError 503 "unavailable" "screenshot capture is unavailable"
        deadlineResult

      _ <- forkIO (serveScreenshotFailure ctx)
      neverDeadline <- newEmptyMVar
      failureResult <- run (takeMVar neverDeadline) Null
      assertServiceError 500 "internal_error" "failed to capture screenshot"
        failureResult

    it "maps closed and busy brokers and allocates no ticket for pre-capture failures" $
      withCtx $ \ctx -> do
        let run body = runServiceHandler
              (rendererScreenshotHandlerWithDeadline (pure ()))
              (commandTestServiceContext ctx)
              (ServiceRequest (Just body))
        invalid <- run (object ["path" .= ("../escape.png" :: Text)])
        invalid `shouldSatisfy` isServiceFailureWithStatus 400
        screenshotRequestActive (ccScreenshotRef ctx) `shouldReturn` False
        disabled <- run (object ["path" .= ("capture.png" :: Text)])
        disabled `shouldSatisfy` isServiceFailureWithStatus 503
        screenshotRequestActive (ccScreenshotRef ctx) `shouldReturn` False

        _ <- submitScreenshotRequest (ccScreenshotRef ctx)
        busy <- run Null
        busy `shouldSatisfy` isServiceFailureWithStatus 409

        -- Consume the busy request, then close permanently.
        cancelBusy <- claimScreenshotRequest (ccScreenshotRef ctx)
        cancelBusy `shouldSatisfy` maybe False (const True)
        shutdownScreenshotRequestRef (ccScreenshotRef ctx)
        closed <- run Null
        case closed of
          Left err -> do
            serviceErrorHTTPStatus err `shouldBe` 503
            serviceErrorCode err `shouldBe` "unavailable"
            serviceErrorText err `shouldBe` "screenshot capture is unavailable"
          Right _ -> expectationFailure "expected closed screenshot broker error"

  describe "ordered mutation publication matrix" $ do
    forM_ directUiPublicationCases $ \(label, method, params, assertUi) ->
      it label $ withCtx $ \ctx ->
        expectExactUiPublication ctx method params assertUi

    forM_ directLogPublicationCases $ \(label, method, params, assertLog) ->
      it label $ withCtx $ \ctx ->
        expectExactLogPublication ctx method params assertLog

    it "publishes plugin parameter state in one UI-only epoch" $ withPluginCtx $ \ctx -> do
      baseline <- beginPublicationAssertion ctx
      rsp <- dispatch ctx "set_plugin_param" (object
        [ "plugin" .= ("example" :: Text)
        , "param" .= ("enabled" :: Text)
        , "value" .= Bool True
        ])
      srSuccess rsp `shouldBe` True
      (_, committed) <- expectExactPublication ctx baseline
      (Map.lookup "example" (uiPluginParams (rsUi committed)) >>= Map.lookup "enabled")
        `shouldBe` Just (Bool True)
      rsLog committed `shouldBe` rsLog (pbSnapshot baseline)
      rsData committed `shouldBe` rsData (pbSnapshot baseline)
      rsTerrain committed `shouldBe` rsTerrain (pbSnapshot baseline)
      jobs <- drainAtlasJobs (ahAtlasManagerHandle (ccActorHandles ctx))
      length jobs `shouldBe` 0

    it "publishes a rejected sim_tick warning as one log-only epoch" $ withCtx $ \ctx -> do
      baseline <- beginPublicationAssertion ctx
      rsp <- dispatch ctx "sim_tick" Null
      srSuccess rsp `shouldBe` False
      (_, committed) <- expectExactPublication ctx baseline
      rsUi committed `shouldBe` rsUi (pbSnapshot baseline)
      rsData committed `shouldBe` rsData (pbSnapshot baseline)
      rsTerrain committed `shouldBe` rsTerrain (pbSnapshot baseline)
      rsLog committed `shouldNotBe` rsLog (pbSnapshot baseline)
      map leLevel (lsEntries (rsLog committed)) `shouldSatisfy` elem LogWarn
      jobs <- drainAtlasJobs (ahAtlasManagerHandle (ccActorHandles ctx))
      length jobs `shouldBe` 0

    it "stamps legacy view-mode transition jobs from the exact committed epoch" $ withCtx $ \ctx -> do
      _ <- writeReadyTerrainData ctx
      committed <- expectExactSelectionPublication ctx "set_view_mode"
        (object ["mode" .= ("biome" :: Text)])
      lvsBaseView (effectiveViewSelection (rsUi committed)) `shouldBe` BaseViewBiome

    it "stamps layered view transition jobs from the exact committed epoch" $ withCtx $ \ctx -> do
      _ <- writeReadyTerrainData ctx
      committed <- expectExactSelectionPublication ctx "set_view" (object
        [ "base" .= ("biome" :: Text)
        , "overlay" .= ("cloud" :: Text)
        , "basis" .= ("current" :: Text)
        ])
      let selection = effectiveViewSelection (rsUi committed)
      lvsBaseView selection `shouldBe` BaseViewBiome
      lvsSkyOverlay selection `shouldBe` Just SkyOverlayCloud

    it "publishes plugin overlay field selection with matching transition jobs" $ withCtx $ \ctx -> do
      _ <- writeSingleChunkTerrainWithNormals ctx
      committed <- expectExactSelectionPublication ctx "set_view" (object
        [ "base" .= ("biome" :: Text)
        , "plugin_overlay" .= ("weather_normals" :: Text)
        , "field_index" .= (1 :: Int)
        ])
      lvsSkyOverlay (effectiveViewSelection (rsUi committed))
        `shouldBe` Just (SkyOverlayPlugin "weather_normals" 1)

    it "publishes set_overlay field selection with matching transition jobs" $ withCtx $ \ctx -> do
      _ <- writeSingleChunkTerrainWithNormals ctx
      committed <- expectExactSelectionPublication ctx "set_overlay" (object
        [ "overlay" .= ("weather_normals" :: Text)
        , "field_index" .= (1 :: Int)
        ])
      lvsSkyOverlay (effectiveViewSelection (rsUi committed))
        `shouldBe` Just (SkyOverlayPlugin "weather_normals" 1)

    it "publishes cycled overlay fields with matching transition jobs" $ withCtx $ \ctx -> do
      _ <- writeSingleChunkTerrainWithNormals ctx
      initial <- dispatch ctx "set_overlay" (object
        [ "overlay" .= ("weather_normals" :: Text)
        , "field_index" .= (0 :: Int)
        ])
      srSuccess initial `shouldBe` True
      _ <- drainAtlasJobs (ahAtlasManagerHandle (ccActorHandles ctx))
      committed <- expectExactSelectionPublication ctx "cycle_overlay_field"
        (object ["direction" .= (1 :: Int)])
      lvsSkyOverlay (effectiveViewSelection (rsUi committed))
        `shouldBe` Just (SkyOverlayPlugin "weather_normals" 1)

    it "publishes opacity without atlas work because opacity is draw-time only" $ withCtx $ \ctx -> do
      _ <- writeReadyTerrainData ctx
      initial <- dispatch ctx "set_view" (object ["overlay" .= ("cloud" :: Text)])
      srSuccess initial `shouldBe` True
      _ <- drainAtlasJobs (ahAtlasManagerHandle (ccActorHandles ctx))
      baseline <- beginPublicationAssertion ctx
      rsp <- dispatch ctx "set_view" (object ["overlay_opacity" .= (0.37 :: Double)])
      srSuccess rsp `shouldBe` True
      (_, committed) <- expectExactPublication ctx baseline
      lvsOverlayOpacity (uiViewSelection (rsUi committed))
        `shouldSatisfy` (\value -> abs (value - 0.37) < 0.0001)
      rsLog committed `shouldBe` rsLog (pbSnapshot baseline)
      rsData committed `shouldBe` rsData (pbSnapshot baseline)
      rsTerrain committed `shouldBe` rsTerrain (pbSnapshot baseline)
      jobs <- drainAtlasJobs (ahAtlasManagerHandle (ccActorHandles ctx))
      length jobs `shouldBe` 0

    it "keeps only the final epoch for overlapping back-to-back camera slots" $ withCtx $ \ctx -> do
      _ <- writeReadyTerrainData ctx
      baseline <- beginPublicationAssertion ctx
      first <- dispatch ctx "set_camera" (object
        ["x" .= (5.0 :: Double), "y" .= (2.0 :: Double), "zoom" .= (1.5 :: Double)])
      second <- dispatch ctx "viewport_drag" (object
        ["x1" .= (0 :: Int), "y1" .= (0 :: Int), "x2" .= (20 :: Int), "y2" .= (10 :: Int)])
      srSuccess first `shouldBe` True
      srSuccess second `shouldBe` True
      let handles = ccActorHandles ctx
      committed@(version2, snapshot2) <- readCommittedRenderSnapshot (ahSnapshotVersionRef handles)
      version2 `shouldBe` SnapshotVersion (unSnapshotVersion (pbVersion baseline) + 2)
      rsLog snapshot2 `shouldBe` rsLog (pbSnapshot baseline)
      rsData snapshot2 `shouldBe` rsData (pbSnapshot baseline)
      rsTerrain snapshot2 `shouldBe` rsTerrain (pbSnapshot baseline)
      liveUi <- getUiSnapshot (ahUiHandle handles)
      liveUi `shouldBe` rsUi snapshot2
      (polledVersion, polledSnapshot, _, _) <- pollRenderSnapshot
        (snapshotPollEnvFor ctx) 101 False (pbCache baseline)
      (polledVersion, polledSnapshot) `shouldBe` committed
      jobs <- drainAtlasJobs (ahAtlasManagerHandle handles)
      let ui2 = rsUi snapshot2
          expected = atlasJobsForSelection
            version2
            (effectiveViewSelection ui2)
            (uiRenderWaterLevel ui2)
            (rsTerrain snapshot2)
            [stageForZoom (uiZoom ui2)]
            Nothing
      assertAtlasJobsMatch jobs expected

    it "leaves publication and atlas state unchanged for pure validation failures" $ withCtx $ \ctx -> do
      baseline <- beginPublicationAssertion ctx
      forM_
        [ ("set_seed", Null)
        , ("set_seed", object ["seed" .= ((-1) :: Int)])
        , ("set_slider", object ["name" .= ("NoSuchSlider" :: Text), "value" .= (0.5 :: Double)])
        , ("set_view_mode", object ["mode" .= ("missing" :: Text)])
        , ("set_overlay", object ["overlay" .= ("missing" :: Text)])
        , ("load_world", object ["name" .= ("__missing_publication_world__" :: Text)])
        ] $ \(method, params) -> do
          rsp <- dispatch ctx method params
          srSuccess rsp `shouldBe` False
      readCommittedRenderSnapshot (ahSnapshotVersionRef (ccActorHandles ctx))
        `shouldReturn` (pbVersion baseline, pbSnapshot baseline)
      jobs <- drainAtlasJobs (ahAtlasManagerHandle (ccActorHandles ctx))
      length jobs `shouldBe` 0

  -- -------------------------------------------------------------------
  -- Unknown command
  -- -------------------------------------------------------------------
  describe "unknown command" $ do
    it "returns an error" $ withCtx $ \ctx -> do
      rsp <- dispatch ctx "does_not_exist" Null
      srSuccess rsp `shouldBe` False
      srError rsp `shouldSatisfy` \case
        Just msg -> Text.isInfixOf "unknown command" msg
        Nothing  -> False

-- =====================================================================
-- Test helpers
-- =====================================================================

data PublicationBaseline = PublicationBaseline
  { pbVersion :: !SnapshotVersion
  , pbSnapshot :: !RenderSnapshot
  , pbCache :: !RenderCacheState
  }

directUiPublicationCases :: [(String, Text, Value, UiState -> Expectation)]
directUiPublicationCases =
  [ ("publishes seed and seed input", "set_seed", object ["seed" .= (12345 :: Int)], \ui -> do
      uiSeed ui `shouldBe` 12345
      uiSeedInput ui `shouldBe` "12345")
  , ("publishes left panel visibility", "set_left_panel", object ["visible" .= False],
      \ui -> uiShowLeftPanel ui `shouldBe` False)
  , ("publishes the visible left panel tab", "set_left_tab", object ["tab" .= ("view" :: Text)],
      \ui -> uiLeftTab ui `shouldBe` LeftView)
  , ("publishes explicit config panel visibility", "toggle_config_panel", object ["visible" .= True],
      \ui -> uiShowConfig ui `shouldBe` True)
  , ("publishes config tab and reset scroll", "set_config_tab", object ["tab" .= ("climate" :: Text)], \ui -> do
      uiConfigTab ui `shouldBe` ConfigClimate
      uiConfigScroll ui `shouldBe` 0)
  , ("publishes a slider value", "set_slider", object
      ["name" .= ("SliderGenScale" :: Text), "value" .= (0.42 :: Double)],
      \ui -> uiGenScale ui `shouldSatisfy` (\value -> abs (value - 0.42) < 0.0001))
  , ("publishes selected and pinned hex context", "select_hex", object
      ["q" .= (2 :: Int), "r" .= ((-3) :: Int)], \ui -> do
      uiContextHex ui `shouldBe` Just (2, -3)
      uiHexTooltipPinned ui `shouldBe` True)
  , ("publishes editor tool state", "editor_set_tool", object ["tool" .= ("erode" :: Text)],
      \ui -> editorTool (uiEditor ui) `shouldBe` ToolErode)
  , ("publishes simulation controls", "set_sim_auto_tick", object
      ["enabled" .= True, "rate" .= (0.75 :: Double)], \ui -> do
      uiSimAutoTick ui `shouldBe` True
      uiSimTickRate ui `shouldBe` 0.75)
  , ("publishes world metadata", "set_world_name", object ["name" .= ("Publication Matrix" :: Text)],
      \ui -> uiWorldName ui `shouldBe` "Publication Matrix")
  , ("publishes direct widget mutation before response completion", "click_widget", object
      ["widget_id" .= ("WidgetChunkMinus" :: Text)],
      \ui -> uiChunkSize ui `shouldBe` 56)
  , ("publishes stage configuration", "set_stage_enabled", object
      ["stage" .= ("climate" :: Text), "enabled" .= False],
      \ui -> Set.member StageClimate (uiDisabledStages ui) `shouldBe` True)
  ]

directLogPublicationCases :: [(String, Text, Value, LogSnapshot -> Expectation)]
directLogPublicationCases =
  [ ("publishes log collapsed state", "set_log_collapsed", object ["collapsed" .= True],
      \logSnapshot -> lsCollapsed logSnapshot `shouldBe` True)
  , ("publishes log level", "set_log_level", object ["level" .= ("debug" :: Text)],
      \logSnapshot -> lsMinLevel logSnapshot `shouldBe` LogDebug)
  ]

snapshotPollEnvFor :: CommandContext -> SnapshotPollEnv
snapshotPollEnvFor ctx = SnapshotPollEnv
  { speTimingLogThresholdMs = maxBound
  , speSnapshotPollMs = 1000
  , speSnapshotVersionRef = ahSnapshotVersionRef (ccActorHandles ctx)
  , speLogSlowSnapshotPoll = const (pure ())
  }

beginPublicationAssertion :: CommandContext -> IO PublicationBaseline
beginPublicationAssertion ctx = do
  _ <- drainAtlasJobs (ahAtlasManagerHandle (ccActorHandles ctx))
  (version, snapshot, cache, _) <- pollRenderSnapshot
    (snapshotPollEnvFor ctx) 100 False (initialRenderCacheState 4)
  pure PublicationBaseline
    { pbVersion = version
    , pbSnapshot = snapshot
    , pbCache = cache
    }

expectExactPublication :: CommandContext -> PublicationBaseline -> IO (SnapshotVersion, RenderSnapshot)
expectExactPublication ctx baseline = do
  let handles = ccActorHandles ctx
      expectedVersion = SnapshotVersion (unSnapshotVersion (pbVersion baseline) + 1)
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
    (snapshotPollEnvFor ctx) 101 False (pbCache baseline)
  (polledVersion, polledSnapshot) `shouldBe` committed
  pure committed

expectExactUiPublication
  :: CommandContext
  -> Text
  -> Value
  -> (UiState -> Expectation)
  -> Expectation
expectExactUiPublication ctx method params assertUi = do
  baseline <- beginPublicationAssertion ctx
  rsp <- dispatch ctx method params
  srSuccess rsp `shouldBe` True
  (_, committed) <- expectExactPublication ctx baseline
  assertUi (rsUi committed)
  rsLog committed `shouldBe` rsLog (pbSnapshot baseline)
  rsData committed `shouldBe` rsData (pbSnapshot baseline)
  rsTerrain committed `shouldBe` rsTerrain (pbSnapshot baseline)
  jobs <- drainAtlasJobs (ahAtlasManagerHandle (ccActorHandles ctx))
  length jobs `shouldBe` 0

expectExactLogPublication
  :: CommandContext
  -> Text
  -> Value
  -> (LogSnapshot -> Expectation)
  -> Expectation
expectExactLogPublication ctx method params assertLog = do
  baseline <- beginPublicationAssertion ctx
  rsp <- dispatch ctx method params
  srSuccess rsp `shouldBe` True
  (_, committed) <- expectExactPublication ctx baseline
  assertLog (rsLog committed)
  rsUi committed `shouldBe` rsUi (pbSnapshot baseline)
  rsData committed `shouldBe` rsData (pbSnapshot baseline)
  rsTerrain committed `shouldBe` rsTerrain (pbSnapshot baseline)
  jobs <- drainAtlasJobs (ahAtlasManagerHandle (ccActorHandles ctx))
  length jobs `shouldBe` 0

expectExactSelectionPublication :: CommandContext -> Text -> Value -> IO RenderSnapshot
expectExactSelectionPublication ctx method params = do
  baseline <- beginPublicationAssertion ctx
  rsp <- dispatch ctx method params
  srSuccess rsp `shouldBe` True
  (version, committed) <- expectExactPublication ctx baseline
  let ui0 = rsUi (pbSnapshot baseline)
      ui1 = rsUi committed
      expected = atlasJobsForSelectionTransition
        version
        (effectiveViewSelection ui0)
        (uiRenderWaterLevel ui0)
        (effectiveViewSelection ui1)
        (uiRenderWaterLevel ui1)
        (rsTerrain committed)
        (orderedZoomStagesForZoom (uiZoom ui1))
        Nothing
  rsLog committed `shouldBe` rsLog (pbSnapshot baseline)
  rsData committed `shouldBe` rsData (pbSnapshot baseline)
  rsTerrain committed `shouldBe` rsTerrain (pbSnapshot baseline)
  jobs <- drainAtlasJobs (ahAtlasManagerHandle (ccActorHandles ctx))
  assertAtlasJobsMatch jobs expected
  pure committed

assertAtlasJobsMatch :: [AtlasJob] -> [AtlasJob] -> Expectation
assertAtlasJobsMatch actual expected = do
  length expected `shouldSatisfy` (> 0)
  map ajKey actual `shouldBe` map ajKey expected
  map ajSnapshotVersion actual `shouldBe` map ajSnapshotVersion expected
  map ajTerrain actual `shouldBe` map ajTerrain expected
  map ajViewSelection actual `shouldBe` map ajViewSelection expected
  map ajWaterLevel actual `shouldBe` map ajWaterLevel expected
  map atlasJobStage actual `shouldBe` map atlasJobStage expected

serveScreenshotRequest :: CommandContext -> BS.ByteString -> IO ()
serveScreenshotRequest ctx pngBytes =
  serveScreenshotResult ctx (Right pngBytes)

serveScreenshotFailure :: CommandContext -> IO ()
serveScreenshotFailure ctx =
  serveScreenshotResult ctx (Left ScreenshotInternalError)

serveScreenshotResult
  :: CommandContext
  -> Either ScreenshotResultError BS.ByteString
  -> IO ()
serveScreenshotResult ctx result = waitForRequest
  where
    waitForRequest = claimScreenshotRequest (ccScreenshotRef ctx) >>= \case
      Nothing -> threadDelay 1000 >> waitForRequest
      Just claim -> do
        delivered <- deliverScreenshotRequest (ccScreenshotRef ctx) claim result
        delivered `shouldBe` ScreenshotDelivered

toggleLogCount :: ActorHandle Log (Protocol Log) -> IO Int
toggleLogCount logH = do
  snap <- getLogSnapshot logH
  pure $ length
    [ ()
    | entry <- lsEntries snap
    , "Toggle Day/Night" `Text.isInfixOf` leMessage entry
    ]

awaitToggleLogCount :: ActorHandle Log (Protocol Log) -> Int -> IO Bool
awaitToggleLogCount logH target = awaitTrue 50 $ do
  count <- toggleLogCount logH
  pure (count >= target)

awaitTrue :: Int -> IO Bool -> IO Bool
awaitTrue 0 action = action
awaitTrue retries action = do
  ok <- action
  if ok
    then pure True
    else do
      threadDelay 20000
      awaitTrue (retries - 1) action

atlasJobStage :: AtlasJob -> (Int, Int)
atlasJobStage job = (ajHexRadius job, ajAtlasScale job)

zoomStagePair :: ZoomStage -> (Int, Int)
zoomStagePair stage = (zsHexRadius stage, zsAtlasScale stage)

assertPreservedLayeredSelection
  :: CommandContext
  -> Maybe SkyOverlayMode
  -> Expectation
assertPreservedLayeredSelection ctx expectedOverlay = do
  ui <- getUiSnapshot (ahUiHandle (ccActorHandles ctx))
  let selection = effectiveViewSelection ui
  lvsBaseView selection `shouldBe` BaseViewBiome
  lvsSkyOverlay selection `shouldBe` expectedOverlay
  lvsWeatherBasis selection `shouldBe` WeatherBasisAverage
  lvsOverlayOpacity selection `shouldSatisfy` (\opacity -> abs (opacity - 0.37) < 0.0001)

prepareReadyDayNight :: CommandContext -> IO ()
prepareReadyDayNight ctx = do
  _ <- writeReadyTerrainData ctx
  let handles = ccActorHandles ctx
      uiH = ahUiHandle handles
  setUiDayNightEnabled uiH True
  _ <- getUiSnapshot uiH
  _ <- drainAtlasJobs (ahAtlasManagerHandle handles)
  pure ()

expectViewportRefreshForCommand
  :: CommandContext
  -> Text
  -> Value
  -> (UiState -> UiState -> Expectation)
  -> IO ()
expectViewportRefreshForCommand ctx method params assertUi = do
  _ <- writeReadyTerrainData ctx
  let handles = ccActorHandles ctx
      atlasH = ahAtlasManagerHandle handles
  _ <- drainAtlasJobs atlasH
  baseline <- beginPublicationAssertion ctx

  rsp <- dispatch ctx method params
  srSuccess rsp `shouldBe` True

  (version1, committed) <- expectExactPublication ctx baseline
  let ui0 = rsUi (pbSnapshot baseline)
      ui1 = rsUi committed
      expected = atlasJobsForSelection
        version1
        (effectiveViewSelection ui1)
        (uiRenderWaterLevel ui1)
        (rsTerrain committed)
        [stageForZoom (uiZoom ui1)]
        Nothing
  assertUi ui0 ui1
  rsLog committed `shouldBe` rsLog (pbSnapshot baseline)
  rsData committed `shouldBe` rsData (pbSnapshot baseline)
  rsTerrain committed `shouldBe` rsTerrain (pbSnapshot baseline)
  jobs <- drainAtlasJobs atlasH
  assertAtlasJobsMatch jobs expected

showLeftViewWidgets :: CommandContext -> IO ()
showLeftViewWidgets ctx = do
  let uiH = ahUiHandle (ccActorHandles ctx)
  setUiShowLeftPanel uiH True
  setUiLeftTab uiH LeftView
  _ <- getUiSnapshot uiH
  pure ()

showPipelineWidgets :: CommandContext -> IO ()
showPipelineWidgets ctx = do
  let uiH = ahUiHandle (ccActorHandles ctx)
  setUiShowConfig uiH True
  setUiConfigTab uiH ConfigPipeline
  _ <- getUiSnapshot uiH
  pure ()

-- | Bracket that creates a full actor system with all handles needed
-- for a 'CommandContext'.
withCtx :: (CommandContext -> IO a) -> IO a
withCtx action = bracket newActorSystem shutdownActorSystem $ \system -> do
  uiH        <- get @Ui system
  logH       <- get @Log system
  dataH      <- get @Data system
  terrainH   <- get @Terrain system
  atlasH     <- get @AtlasManager system
  pluginH    <- get @PluginManager system
  simH       <- get @Simulation system
  uiActionsH <- get @UiActions system
  dataSnapRef    <- newDataSnapshotRef (DataSnapshot 0 0 Nothing)
  terrainSnapRef <- newTerrainSnapshotRef (TerrainSnapshot 0 0 0 0 0 0 mempty mempty mempty mempty mempty mempty mempty mempty mempty emptyOverlayStore defaultTerrainGeoContext)
  uiSnapRef      <- newUiSnapshotRef
  logSnapRef     <- newLogSnapshotRef
  setUiSnapshotRef uiH uiSnapRef
  setLogSnapshotRef logH logSnapRef
  _ <- getUiSnapshot uiH
  _ <- getLogSnapshot logH
  versionRef     <- newRenderSnapshotVersionRef uiSnapRef logSnapRef dataSnapRef terrainSnapRef
  screenshotRef  <- newScreenshotRequestRef
  historyRef     <- newIORef (emptyHistory 50)
  widgetActionLock <- newMVar ()
  dataBrowserExecutor <- newDataBrowserExecutor uiH
  let handles = ActorHandles
        { ahUiHandle              = uiH
        , ahLogHandle             = logH
        , ahDataHandle            = dataH
        , ahTerrainHandle         = terrainH
        , ahAtlasManagerHandle    = atlasH
        , ahDataSnapshotRef       = dataSnapRef
        , ahTerrainSnapshotRef    = terrainSnapRef
        , ahSnapshotVersionRef    = versionRef
        , ahPluginManagerHandle   = pluginH
        , ahSimulationHandle      = simH
        , ahHistoryRef            = historyRef
        , ahWidgetActionLock      = widgetActionLock
        }
  overlayInspectorExecutor <- newOverlayInspectorExecutor handles
  let ctx = CommandContext
        { ccNestedServiceRunner = unavailableNestedServiceRunner
        , ccActorHandles    = handles
        , ccUiSnapshotRef   = uiSnapRef
        , ccUiActionsHandle = uiActionsH
        , ccScreenshotRef   = screenshotRef
        , ccScreenshotStoragePolicy = ScreenshotStorageDisabled
        , ccLogSnapshotRef  = Just logSnapRef
        , ccDataBrowserExecutor = dataBrowserExecutor
        , ccOverlayInspectorExecutor = overlayInspectorExecutor
        }
  action ctx `finally` do
    shutdownOverlayInspectorExecutor overlayInspectorExecutor
    shutdownDataBrowserExecutor dataBrowserExecutor

withPluginCtx :: (CommandContext -> IO a) -> IO a
withPluginCtx action = withIsolatedPluginDir $ \_pluginRoot ->
  withCtx $ \ctx -> do
    discoverPlugins (ahPluginManagerHandle (ccActorHandles ctx))
    _ <- getLoadedPlugins (ahPluginManagerHandle (ccActorHandles ctx))
    setUiPluginNames (ahUiHandle (ccActorHandles ctx)) ["example"]
    setUiPluginParamSpecs (ahUiHandle (ccActorHandles ctx)) (Map.singleton "example" commandPluginParamSpecs)
    _ <- getUiSnapshot (ahUiHandle (ccActorHandles ctx))
    action ctx

testPluginDirEnv :: String
testPluginDirEnv = "TOPO_PLUGIN_DIR"

withIsolatedPluginDir :: (FilePath -> IO a) -> IO a
withIsolatedPluginDir action = bracket setup teardown (action . fst)
  where
    setup = do
      oldPluginDir <- lookupEnv testPluginDirEnv
      tmp <- getTemporaryDirectory
      let root = tmp </> "topo-command-dispatch-plugin"
          pluginBase = root </> "plugins"
          pluginDir = pluginBase </> "example"
      removePathForcibly root `catchAny` \_ -> pure ()
      createDirectoryIfMissing True pluginDir
      BL.writeFile (pluginDir </> "manifest.json") (Aeson.encode commandPluginManifest)
      setEnv testPluginDirEnv pluginBase
      pure (root, oldPluginDir)

    teardown (root, oldPluginDir) = do
      maybe (unsetEnv testPluginDirEnv) (setEnv testPluginDirEnv) oldPluginDir
      removePathForcibly root `catchAny` \_ -> pure ()

catchAny :: IO a -> (IOError -> IO a) -> IO a
catchAny = catch

findPlugin :: Text -> [LoadedPlugin] -> Maybe LoadedPlugin
findPlugin name = go
  where
    go [] = Nothing
    go (plugin:rest)
      | lpName plugin == name = Just plugin
      | otherwise = go rest

commandPluginParamSpecs :: [RPCParamSpec]
commandPluginParamSpecs =
  [ RPCParamSpec
      { rpsName = "enabled"
      , rpsLabel = "Enabled"
      , rpsType = ParamBool
      , rpsRange = Nothing
      , rpsDefault = Bool False
      , rpsTooltip = ""
      }
  , RPCParamSpec
      { rpsName = "density"
      , rpsLabel = "Density"
      , rpsType = ParamFloat
      , rpsRange = Just (Number 0, Number 1)
      , rpsDefault = Number 0.5
      , rpsTooltip = ""
      }
  , RPCParamSpec
      { rpsName = "iterations"
      , rpsLabel = "Iterations"
      , rpsType = ParamInt
      , rpsRange = Just (Number 1, Number 10)
      , rpsDefault = Number 3
      , rpsTooltip = ""
      }
  ]

commandPluginManifest :: Value
commandPluginManifest = object
  [ "manifestVersion" .= manifestV3
  , "name" .= ("example" :: Text)
  , "version" .= ("1.0.0" :: Text)
  , "runtime" .= object
      [ "protocol" .= object
          [ "min" .= currentProtocolVersion
          , "max" .= currentProtocolVersion
          ]
      ]
  , "generator" .= object ["insertAfter" .= ("biomes" :: Text)]
  , "config" .= object ["parameters" .= commandPluginParamSpecs]
  ]

inspectorSectionKey :: Value -> Maybe Text
inspectorSectionKey (Object section) = case KM.lookup "key" section of
  Just (String key) -> Just key
  _ -> Nothing
inspectorSectionKey _ = Nothing

canonicalInspectorSectionKeys :: [Text]
canonicalInspectorSectionKeys =
  [ "coordinates"
  , "elevation_hypsometry"
  , "tectonics_plates"
  , "erosion_terrain_form"
  , "hydrology_rivers"
  , "water_bodies"
  , "water_table"
  , "climate_weather"
  , "weather_snapshot"
  , "weather_normals"
  , "weather_timeline"
  , "biome_refinement"
  , "soil"
  , "vegetation"
  , "glacier_snow_ice"
  , "volcanism"
  , "ocean_currents"
  , "overlay_records"
  , "overlay_schema"
  , "overlay_provenance"
  , "plugin_hex_data"
  , "stage_provenance"
  , "unit_conversions"
  , "export_links"
  ]

writeSingleChunkTerrain :: CommandContext -> IO Int
writeSingleChunkTerrain ctx = do
  let cfg = WorldConfig { wcChunkSize = 64 }
      chunkId = chunkIdFromCoord (ChunkCoord 0 0)
      ChunkId chunkKey = chunkId
      snap = TerrainSnapshot
        1
        0
        0
        0
        0
        (wcChunkSize cfg)
        (IntMap.singleton chunkKey (emptyTerrainChunk cfg))
        mempty
        mempty
        mempty
        mempty
        mempty
        mempty
        mempty
        mempty
        emptyOverlayStore
        defaultTerrainGeoContext
  writeTerrainSnapshot (ahTerrainSnapshotRef (ccActorHandles ctx)) snap
  pure chunkKey

writeSingleChunkTerrainWithNormals :: CommandContext -> IO Int
writeSingleChunkTerrainWithNormals ctx = do
  let cfg = WorldConfig { wcChunkSize = 64 }
      n = chunkTileCount cfg
      chunkId = chunkIdFromCoord (ChunkCoord 0 0)
      ChunkId chunkKey = chunkId
      climate = ClimateChunk
        { ccTempAvg = U.replicate n 0.5
        , ccPrecipAvg = U.replicate n 0.4
        , ccWindDirAvg = U.replicate n 0.2
        , ccWindSpdAvg = U.replicate n 0.3
        , ccHumidityAvg = U.replicate n 0.7
        , ccTempRange = U.replicate n 0.1
        , ccPrecipSeasonality = U.replicate n 0.25
        }
      normals = weatherNormalsChunkFromClimate defaultWeatherConfig climate
      normalsOverlay = Overlay
        { ovSchema = weatherNormalsOverlaySchema
        , ovData = DenseData (IntMap.singleton chunkKey (weatherNormalsChunkToOverlay normals))
        , ovProvenance = OverlayProvenance
            { opSeed = 0
            , opVersion = 1
            , opSource = "weather_normals"
            , opSchedule = Nothing
            }
        }
  let handles = ccActorHandles ctx
      dataH = ahDataHandle handles
  setTerrainChunkData dataH (wcChunkSize cfg) [(chunkId, emptyTerrainChunk cfg)]
  setOverlayStoreData dataH (insertOverlay normalsOverlay emptyOverlayStore)
  dataSnapshot <- getDataSnapshot dataH
  terrainSnapshot <- getTerrainSnapshot dataH
  _ <- publishSnapshot
    (ahSnapshotVersionRef handles)
    (dataAndTerrainSnapshotUpdate
      (ahDataSnapshotRef handles) dataSnapshot
      (ahTerrainSnapshotRef handles) terrainSnapshot)
  pure chunkKey

writeSingleChunkTerrainWithClimateWeatherAndNormals :: CommandContext -> IO Int
writeSingleChunkTerrainWithClimateWeatherAndNormals ctx = do
  let cfg = WorldConfig { wcChunkSize = 64 }
      n = chunkTileCount cfg
      chunkId = chunkIdFromCoord (ChunkCoord 0 0)
      ChunkId chunkKey = chunkId
      climate = ClimateChunk
        { ccTempAvg = U.replicate n 0.5
        , ccPrecipAvg = U.replicate n 0.4
        , ccWindDirAvg = U.replicate n 0.2
        , ccWindSpdAvg = U.replicate n 0.3
        , ccHumidityAvg = U.replicate n 0.7
        , ccTempRange = U.replicate n 0.1
        , ccPrecipSeasonality = U.replicate n 0.25
        }
      weather = WeatherChunk
        { wcTemp = U.replicate n 0.61
        , wcHumidity = U.replicate n 0.62
        , wcWindDir = U.replicate n 0.63
        , wcWindSpd = U.replicate n 0.64
        , wcPressure = U.replicate n 0.65
        , wcPrecip = U.replicate n 0.66
        , wcCloudCover = U.replicate n 0.67
        , wcCloudWater = U.replicate n 0.68
        , wcCloudCoverLow = U.replicate n 0.69
        , wcCloudCoverMid = U.replicate n 0.70
        , wcCloudCoverHigh = U.replicate n 0.71
        , wcCloudWaterLow = U.replicate n 0.72
        , wcCloudWaterMid = U.replicate n 0.73
        , wcCloudWaterHigh = U.replicate n 0.74
        }
      normals = weatherNormalsChunkFromClimate defaultWeatherConfig climate
      normalsOverlay = Overlay
        { ovSchema = weatherNormalsOverlaySchema
        , ovData = DenseData (IntMap.singleton chunkKey (weatherNormalsChunkToOverlay normals))
        , ovProvenance = OverlayProvenance
            { opSeed = 0
            , opVersion = 1
            , opSource = "weather_normals"
            , opSchedule = Nothing
            }
        }
      snap = TerrainSnapshot
        1
        1
        1
        0
        1
        (wcChunkSize cfg)
        (IntMap.singleton chunkKey (emptyTerrainChunk cfg))
        (IntMap.singleton chunkKey climate)
        (IntMap.singleton chunkKey weather)
        mempty
        mempty
        mempty
        mempty
        mempty
        mempty
        (insertOverlay normalsOverlay emptyOverlayStore)
        defaultTerrainGeoContext
  writeTerrainSnapshot (ahTerrainSnapshotRef (ccActorHandles ctx)) snap
  pure chunkKey

writeReadyTerrainData :: CommandContext -> IO TerrainSnapshot
writeReadyTerrainData ctx = do
  let handles = ccActorHandles ctx
      cfg = WorldConfig { wcChunkSize = 64 }
      chunkId = chunkIdFromCoord (ChunkCoord 0 0)
  setTerrainChunkData (ahDataHandle handles) (wcChunkSize cfg) [(chunkId, emptyTerrainChunk cfg)]
  dataSnap <- getDataSnapshot (ahDataHandle handles)
  terrainSnap <- getTerrainSnapshot (ahDataHandle handles)
  _ <- publishSnapshot
    (ahSnapshotVersionRef handles)
    (dataAndTerrainSnapshotUpdate
      (ahDataSnapshotRef handles) dataSnap
      (ahTerrainSnapshotRef handles) terrainSnap)
  pure terrainSnap

-- | Convenience: dispatch a command with a given method and params,
-- using request id 1.
commandTestServiceContext :: CommandContext -> ServiceContext
commandTestServiceContext ctx = ServiceContext
  { svcNestedServiceRunner = ccNestedServiceRunner ctx
  , svcActorHandles = ccActorHandles ctx
  , svcUiSnapshotRef = ccUiSnapshotRef ctx
  , svcUiActionsHandle = ccUiActionsHandle ctx
  , svcScreenshotRef = ccScreenshotRef ctx
  , svcScreenshotStoragePolicy = ccScreenshotStoragePolicy ctx
  , svcLogSnapshotRef = ccLogSnapshotRef ctx
  , svcEventBus = Nothing
  , svcDataBrowserExecutor = ccDataBrowserExecutor ctx
  , svcOverlayInspectorExecutor = ccOverlayInspectorExecutor ctx
  }

isServiceFailureWithStatus :: Int -> ServiceResult -> Bool
isServiceFailureWithStatus expected (Left err) =
  serviceErrorHTTPStatus err == expected
isServiceFailureWithStatus _ (Right _) = False

dispatch :: CommandContext -> Text -> Value -> IO SeerResponse
dispatch ctx method params = dispatchCommand ctx SeerCommand
  { scId     = 1
  , scMethod = method
  , scParams = params
  }

dispatchWithId :: CommandContext -> Int -> Text -> Value -> IO SeerResponse
dispatchWithId ctx reqId method params = dispatchCommand ctx SeerCommand
  { scId     = reqId
  , scMethod = method
  , scParams = params
  }

runService :: CommandContext -> Text -> Value -> IO ServiceResult
runService ctx = runAppServiceOperation serviceHarnessApp ctx

serviceHarnessApp :: AppService
serviceHarnessApp = commandAppService
  { appConfig = (appConfig commandAppService)
      { configListPresets = adaptTypedServiceHandler
          configListPresetsOperation
          (const (Right ConfigListPresetsRequest))
          (\response -> object
              [ "preset_count" .= configPresetCount response
              , "presets" .= configPresetNames response
              , "entries" .= ([] :: [Value])
              ])
          (\_ ConfigListPresetsRequest -> pure $ Right ConfigListPresetsResponse
            { configPresetCount = 0
            , configPresetNames = []
            , configPresetEntries = []
            })
      }
  , appWorld = (appWorld commandAppService)
      { worldGenerate = adaptTypedServiceHandler
          worldGenerateOperation
          (const (Right (WorldGenerateRequest Nothing)))
          (\response -> object
              [ "accepted" .= worldGenerateAccepted response
              , "status" .= worldGenerateStatus response
              ])
          (\_ _ -> pure $ Right WorldGenerateResponse
            { worldGenerateAccepted = True
            , worldGenerateStatus = "generating"
            })
      , worldList = adaptTypedServiceHandler
          worldListOperation
          (const (Right WorldListRequest))
          (\response -> object
              [ "world_count" .= worldListCount response
              , "worlds" .= ([] :: [Value])
              ])
          (\_ WorldListRequest -> pure $ Right WorldListResponse
            { worldListCount = 0
            , worldListWorlds = []
            })
      }
  }

serviceContextFromCommand :: CommandContext -> ServiceContext
serviceContextFromCommand ctx = ServiceContext
  { svcNestedServiceRunner = ccNestedServiceRunner ctx
  , svcActorHandles = ccActorHandles ctx
  , svcUiSnapshotRef = ccUiSnapshotRef ctx
  , svcUiActionsHandle = ccUiActionsHandle ctx
  , svcScreenshotRef = ccScreenshotRef ctx
  , svcScreenshotStoragePolicy = ccScreenshotStoragePolicy ctx
  , svcLogSnapshotRef = ccLogSnapshotRef ctx
  , svcEventBus = Nothing
  , svcDataBrowserExecutor = ccDataBrowserExecutor ctx
  , svcOverlayInspectorExecutor = ccOverlayInspectorExecutor ctx
  }

data ExpectedServiceOutcome
  = ExpectServiceSuccess
  | ExpectServiceFailure

data ServiceOperationCase = ServiceOperationCase
  { serviceCaseMethod :: !Text
  , serviceCaseParams :: !Value
  , serviceCaseExpected :: !ExpectedServiceOutcome
  , serviceCaseSetup :: CommandContext -> IO ()
  }

serviceCase :: Text -> Value -> ExpectedServiceOutcome -> ServiceOperationCase
serviceCase method params expected = ServiceOperationCase method params expected (\_ -> pure ())

assertServiceOutcome :: ServiceOperationCase -> ServiceResult -> Expectation
assertServiceOutcome testCase result =
  case (serviceCaseExpected testCase, result) of
    (ExpectServiceSuccess, Right _) -> pure ()
    (ExpectServiceFailure, Left _) -> pure ()
    (ExpectServiceSuccess, Left err) -> expectationFailure $
      Text.unpack (serviceCaseMethod testCase) <> " expected service success, got: " <> show err
    (ExpectServiceFailure, Right (ServiceResponse body)) -> expectationFailure $
      Text.unpack (serviceCaseMethod testCase) <> " expected service failure, got: " <> show body

assertSingleValidationDetail :: ServiceResult -> [Text] -> Text -> Expectation
assertSingleValidationDetail result expectedPath expectedCode =
  case result of
    Left err -> do
      serviceErrorKind err `shouldBe` ServiceErrorInvalidRequest
      serviceErrorCode err `shouldBe` "validation_failed"
      case serviceErrorDetails err of
        [detail] -> do
          serviceErrorDetailPath detail `shouldBe` expectedPath
          serviceErrorDetailCode detail `shouldBe` expectedCode
        details -> expectationFailure ("expected one validation detail, got: " <> show details)
    other -> expectationFailure ("expected structured validation error, got: " <> show other)

serviceOperationCases :: [ServiceOperationCase]
serviceOperationCases =
  [ serviceCase "get_state" Null ExpectServiceSuccess
  , serviceCase "get_view_modes" Null ExpectServiceSuccess
  , serviceCase "get_views" Null ExpectServiceSuccess
  , serviceCase "get_ui_state" Null ExpectServiceSuccess
  , serviceCase "get_sliders" Null ExpectServiceSuccess
  , serviceCase "get_slider" (object ["name" .= ("SliderGenScale" :: String)]) ExpectServiceSuccess
  , serviceCase "set_slider" (object ["name" .= ("SliderGenScale" :: String), "value" .= (0.42 :: Double)]) ExpectServiceSuccess
  , serviceCase "set_sliders" (object ["values" .= object ["SliderGenScale" .= (0.3 :: Double)]]) ExpectServiceSuccess
  , serviceCase "reset_sliders" Null ExpectServiceSuccess
  , serviceCase "get_config_summary" Null ExpectServiceSuccess
  , serviceCase "get_enums" (object ["type" .= ("biome" :: String)]) ExpectServiceSuccess
  , serviceCase "list_presets" Null ExpectServiceSuccess
  , serviceCase "save_preset" Null ExpectServiceFailure
  , serviceCase "load_preset" Null ExpectServiceFailure
  , serviceCase "generate" Null ExpectServiceSuccess
  , serviceCase "get_world_meta" Null ExpectServiceSuccess
  , serviceCase "get_generation_status" Null ExpectServiceSuccess
  , serviceCase "list_worlds" Null ExpectServiceSuccess
  , serviceCase "save_world" Null ExpectServiceFailure
  , serviceCase "load_world" Null ExpectServiceFailure
  , serviceCase "delete_world" Null ExpectServiceFailure
  , serviceCase "set_world_name" (object ["name" .= ("Service Test World" :: String)]) ExpectServiceSuccess
  , serviceCase "get_hex" (object ["q" .= (0 :: Int), "r" .= (0 :: Int)]) ExpectServiceFailure
  , serviceCase "get_chunks" Null ExpectServiceSuccess
  , serviceCase "get_chunk_summary" (object ["chunk" .= (999 :: Int)]) ExpectServiceFailure
  , serviceCase "get_terrain_stats" Null ExpectServiceSuccess
  , serviceCase "get_overlays" Null ExpectServiceSuccess
  , serviceCase "find_hexes" (object ["filters" .= ([] :: [Value])]) ExpectServiceSuccess
  , serviceCase "export_terrain_data" Null ExpectServiceFailure
  , serviceCase "get_overlay_schema" (object ["overlay" .= ("missing" :: String)]) ExpectServiceFailure
  , serviceCase "get_overlay_provenance" (object ["overlay" .= ("missing" :: String)]) ExpectServiceFailure
  , serviceCase "export_overlay_data" (object ["overlay" .= ("missing" :: String)]) ExpectServiceFailure
  , serviceCase "validate_overlay_import" Null ExpectServiceSuccess
  , serviceCase "export_mesh_data" Null ExpectServiceFailure
  , serviceCase "export_sample_data" (object ["x" .= (0 :: Double), "y" .= (0 :: Double)]) ExpectServiceFailure
  , serviceCase "editor_toggle" (object []) ExpectServiceSuccess
  , serviceCase "editor_set_tool" (object ["tool" .= ("erode" :: String)]) ExpectServiceSuccess
  , serviceCase "editor_set_brush" (object
      [ "radius" .= (3 :: Int)
      , "strength" .= (0.4 :: Double)
      , "falloff" .= ("smooth" :: String)
      ]) ExpectServiceSuccess
  , serviceCase "editor_brush_stroke" (object ["q" .= (0 :: Int), "r" .= (0 :: Int)]) ExpectServiceSuccess
  , serviceCase "editor_brush_line" (object
      [ "from_q" .= (0 :: Int)
      , "from_r" .= (0 :: Int)
      , "to_q" .= (1 :: Int)
      , "to_r" .= (0 :: Int)
      ]) ExpectServiceSuccess
  , serviceCase "editor_set_biome" (object ["biome" .= ("Forest" :: String)]) ExpectServiceSuccess
  , serviceCase "editor_set_form" (object ["form" .= ("Hilly" :: String)]) ExpectServiceSuccess
  , serviceCase "editor_set_hardness" (object ["hardness" .= (0.7 :: Double)]) ExpectServiceSuccess
  , serviceCase "editor_undo" Null ExpectServiceSuccess
  , serviceCase "editor_redo" Null ExpectServiceSuccess
  , serviceCase "editor_get_state" Null ExpectServiceSuccess
  , serviceCase "get_pipeline" Null ExpectServiceSuccess
  , serviceCase "set_stage_enabled" (object ["stage" .= ("__missing_stage__" :: String), "enabled" .= True]) ExpectServiceFailure
  , serviceCase "list_plugins" Null ExpectServiceSuccess
  , serviceCase "set_plugin_enabled" Null ExpectServiceFailure
  , serviceCase "set_plugin_param" Null ExpectServiceFailure
  , serviceCase "data_list_plugins" Null ExpectServiceSuccess
  , serviceCase "data_list_resources" (object ["plugin" .= ("__missing_plugin__" :: String)]) ExpectServiceFailure
  , serviceCase "data_list_records" Null ExpectServiceFailure
  , serviceCase "data_get_record" Null ExpectServiceFailure
  , serviceCase "data_create_record" Null ExpectServiceFailure
  , serviceCase "data_update_record" Null ExpectServiceFailure
  , serviceCase "data_delete_record" Null ExpectServiceFailure
  , serviceCase "data_get_state" Null ExpectServiceSuccess
  , serviceCase "get_sim_state" Null ExpectServiceSuccess
  , serviceCase "set_sim_auto_tick" (object ["enabled" .= False, "rate" .= (2.0 :: Double)]) ExpectServiceSuccess
  , serviceCase "sim_tick" Null ExpectServiceFailure
  , serviceCase "get_sim_dag" Null ExpectServiceSuccess
  , serviceCase "get_logs" Null ExpectServiceSuccess
  , ServiceOperationCase "take_screenshot" Null ExpectServiceFailure $ \ctx -> do
      submitted <- submitScreenshotRequest (ccScreenshotRef ctx)
      submitted `shouldSatisfy` either (const False) (const True)
  , serviceCase "set_seed" (object ["seed" .= (2468 :: Int)]) ExpectServiceSuccess
  , serviceCase "set_view_mode" (object ["mode" .= ("biome" :: String)]) ExpectServiceSuccess
  , serviceCase "set_view" (object ["base_mode" .= ("biome" :: String)]) ExpectServiceSuccess
  , serviceCase "set_config_tab" (object ["tab" .= ("climate" :: String)]) ExpectServiceSuccess
  , serviceCase "select_hex" (object ["q" .= (0 :: Int), "r" .= (5 :: Int)]) ExpectServiceSuccess
  , serviceCase "set_overlay" (object ["overlay" .= ("__missing_overlay__" :: String)]) ExpectServiceFailure
  , serviceCase "list_overlay_fields" Null ExpectServiceFailure
  , serviceCase "cycle_overlay" (object ["direction" .= (1 :: Int)]) ExpectServiceFailure
  , serviceCase "cycle_overlay_field" (object ["direction" .= (1 :: Int)]) ExpectServiceFailure
  , serviceCase "set_camera" (object ["x" .= (1.0 :: Double), "y" .= (2.0 :: Double), "zoom" .= (1.5 :: Double)]) ExpectServiceSuccess
  , serviceCase "get_camera" Null ExpectServiceSuccess
  , serviceCase "zoom_to_chunk" (object ["chunk" .= (999 :: Int)]) ExpectServiceFailure
  , serviceCase "set_left_panel" (object ["visible" .= True]) ExpectServiceSuccess
  , serviceCase "set_left_tab" (object ["tab" .= ("topo" :: String)]) ExpectServiceSuccess
  , serviceCase "toggle_config_panel" Null ExpectServiceSuccess
  , serviceCase "set_log_collapsed" (object ["collapsed" .= True]) ExpectServiceSuccess
  , serviceCase "set_log_level" (object ["level" .= ("debug" :: String)]) ExpectServiceSuccess
  , serviceCase "get_ui_panels" Null ExpectServiceSuccess
  , serviceCase "viewport_scroll" (object ["delta" .= (1 :: Int)]) ExpectServiceSuccess
  , serviceCase "viewport_click" (object ["x" .= (0 :: Int), "y" .= (0 :: Int)]) ExpectServiceSuccess
  , serviceCase "viewport_drag" (object
      [ "x1" .= (0 :: Int)
      , "y1" .= (0 :: Int)
      , "x2" .= (10 :: Int)
      , "y2" .= (10 :: Int)
      ]) ExpectServiceSuccess
  , serviceCase "viewport_hover" (object ["x" .= (0 :: Int), "y" .= (0 :: Int)]) ExpectServiceSuccess
  , serviceCase "click_widget" Null ExpectServiceFailure
  , serviceCase "list_widgets" Null ExpectServiceSuccess
  , serviceCase "get_widget_state" Null ExpectServiceFailure
  , serviceCase "get_dialog_state" Null ExpectServiceSuccess
  , serviceCase "set_dialog_text" (object ["target" .= ("seed" :: String), "text" .= ("123" :: String)]) ExpectServiceSuccess
  , serviceCase "dialog_confirm" Null ExpectServiceSuccess
  , serviceCase "dialog_cancel" Null ExpectServiceSuccess
  , serviceCase "send_key" (object ["key" .= ("escape" :: String)]) ExpectServiceSuccess
  ]

-- | Look up a key in a JSON object.
shouldReturnSatisfying :: Show a => IO a -> (a -> Bool) -> Expectation
shouldReturnSatisfying action predicate = action >>= (`shouldSatisfy` predicate)

lookupKey :: Key -> Value -> Maybe Value
lookupKey k (Object o) = KM.lookup k o
lookupKey _ _          = Nothing

inspectorPayload :: Key -> Value -> Maybe Value
inspectorPayload key value = do
  inspector <- lookupKey "overlay_inspector" value
  lookupKey key inspector

expectServiceBody :: ServiceResult -> IO Value
expectServiceBody (Right (ServiceResponse body)) = pure body
expectServiceBody (Left err) = do
  expectationFailure ("expected service success, got: " <> Text.unpack (serviceErrorText err))
  pure Null

toListValue :: Value -> [Value]
toListValue (Array values) = toList values
toListValue _ = []

builtinWorldGenCases :: [(Text, WorldGenConfig)]
builtinWorldGenCases =
  [ ("builtin:continental", continentalWorldGenConfig)
  , ("builtin:archipelago", archipelagoWorldGenConfig)
  , ("builtin:large-ocean", largeOceanWorldGenConfig)
  , ("builtin:inland-sea", inlandSeaWorldGenConfig)
  , ("builtin:arid", aridWorldGenConfig)
  , ("builtin:lush", lushWorldGenConfig)
  ]

allBuiltinIdsPresent :: [Value] -> Bool
allBuiltinIdsPresent values =
  all ((`elem` values) . String . fst) builtinWorldGenCases

allBuiltinEntriesReadOnly :: [Value] -> Bool
allBuiltinEntriesReadOnly entries = all hasReadOnlyEntry builtinWorldGenCases
  where
    hasReadOnlyEntry (presetId, _) = any (matches presetId) entries
    matches presetId entry =
      lookupKey "id" entry == Just (String presetId)
        && lookupKey "source" entry == Just (String "builtin")
        && lookupKey "read_only" entry == Just (Bool True)

worldResponseContains :: Text -> SeerResponse -> Bool
worldResponseContains name response = case lookupKey "worlds" (srResult response) of
  Just (Array worlds) -> any hasName (toList worlds)
  _ -> False
  where
    hasName world = lookupKey "name" world == Just (String name)

valueHasKey :: Key -> Value -> Bool
valueHasKey key (Object obj) = KM.member key obj
valueHasKey _ _ = False

nonEmptyArray :: Value -> Bool
nonEmptyArray (Array values) = not (null (toList values))
nonEmptyArray _ = False

isArrayValue :: Maybe Value -> Bool
isArrayValue (Just (Array _)) = True
isArrayValue _ = False

diagnosticCode :: Value -> Maybe Text
diagnosticCode value = case lookupKey "code" value of
  Just (String code) -> Just code
  _ -> Nothing

valueHasNodesAndEdges :: Value -> Bool
valueHasNodesAndEdges value = valueHasKey "nodes" value && valueHasKey "edges" value

findStage :: Text -> [Value] -> Maybe Value
findStage stageId = go
  where
    go [] = Nothing
    go (value:rest)
      | lookupKey "id" value == Just (String stageId) = Just value
      | otherwise = go rest

findViewMode :: Text -> [Value] -> Maybe Value
findViewMode modeName = go
  where
    go [] = Nothing
    go (value:rest)
      | lookupKey "name" value == Just (String modeName) = Just value
      | otherwise = go rest

viewModeName :: Value -> Maybe Text
viewModeName value = case lookupKey "name" value of
  Just (String name) -> Just name
  _                  -> Nothing

stageHasStatus :: Text -> Value -> Bool
stageHasStatus expected value = lookupKey "status" value == Just (String expected)

-- | Check whether a view-mode entry has @"active": true@.
isActive :: Value -> Bool
isActive (Object o) = KM.lookup "active" o == Just (Bool True)
isActive _          = False
