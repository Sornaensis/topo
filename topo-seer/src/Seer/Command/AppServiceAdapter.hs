{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Command IPC adapter for the AppService boundary.
--
-- This module hosts the transitional AppService implementation backed by the
-- existing command handlers.  The command dispatcher itself stays thin: it
-- translates command envelopes to service requests, invokes AppService
-- operations, and translates service results back to the legacy response
-- envelope.
module Seer.Command.AppServiceAdapter
  ( commandAppService
  , dispatchAppServiceCommand
  , runAppServiceOperation
  , runServiceOperation
  , appServiceCommandMethods
  ) where

import Data.Aeson (Value(..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

import Topo.Command.Types (SeerCommand(..), SeerResponse(..), errResponse, okResponse)

import Seer.Command.Context
  ( CommandContext
  , commandServiceContext
  , serviceCommandContext
  )
import qualified Seer.Command.Handlers.Camera as HCamera
import qualified Seer.Command.Handlers.Data as HData
import qualified Seer.Command.Handlers.Editor as HEditor
import qualified Seer.Command.Handlers.Enums as HEnums
import qualified Seer.Command.Handlers.Generate as HGenerate
import qualified Seer.Command.Handlers.Input as HInput
import qualified Seer.Command.Handlers.Log as HLog
import qualified Seer.Command.Handlers.Panels as HPanels
import qualified Seer.Command.Handlers.Pipeline as HPipeline
import qualified Seer.Command.Handlers.Plugin as HPlugin
import qualified Seer.Command.Handlers.Presets as HPresets
import qualified Seer.Command.Handlers.Query as HQuery
import qualified Seer.Command.Handlers.Screenshot as HScreenshot
import qualified Seer.Command.Handlers.Simulation as HSimulation
import qualified Seer.Command.Handlers.State as HState
import qualified Seer.Command.Handlers.Sliders as HSliders
import qualified Seer.Command.Handlers.Terrain as HTerrain
import qualified Seer.Command.Handlers.View as HView
import qualified Seer.Command.Handlers.Viewport as HViewport
import qualified Seer.Command.Handlers.Widgets as HWidgets
import qualified Seer.Command.Handlers.World as HWorld
import Seer.Service.AppService
import Seer.Service.Types
import Seer.Service.Validation

type RawCommandHandler = CommandContext -> Int -> Value -> IO SeerResponse

-- | Transitional AppService implementation backed by existing command handlers.
commandAppService :: AppService
commandAppService = AppService
  { appState = StateService
      { stateGetState = commandServiceHandler "get_state" HState.handleGetState
      , stateGetViewModes = commandServiceHandler "get_view_modes" HState.handleGetViewModes
      , stateGetUiState = commandServiceHandler "get_ui_state" HState.handleGetUiState
      }
  , appConfig = ConfigService
      { configGetSliders = commandServiceHandler "get_sliders" HSliders.handleGetSliders
      , configGetSlider = commandServiceHandler "get_slider" HSliders.handleGetSlider
      , configSetSlider = commandServiceHandler "set_slider" HSliders.handleSetSlider
      , configSetSliders = commandServiceHandler "set_sliders" HSliders.handleSetSliders
      , configResetSliders = commandServiceHandler "reset_sliders" HSliders.handleResetSliders
      , configGetSummary = commandServiceHandler "get_config_summary" HSliders.handleGetConfigSummary
      , configGetEnums = commandServiceHandler "get_enums" HEnums.handleGetEnums
      , configListPresets = commandServiceHandler "list_presets" HPresets.handleListPresets
      , configSavePreset = commandServiceHandler "save_preset" HPresets.handleSavePreset
      , configLoadPreset = commandServiceHandler "load_preset" HPresets.handleLoadPreset
      }
  , appWorld = WorldService
      { worldGenerate = commandServiceHandler "generate" HGenerate.handleGenerate
      , worldGetMeta = commandServiceHandler "get_world_meta" HWorld.handleGetWorldMeta
      , worldGetGenerationStatus = commandServiceHandler "get_generation_status" HWorld.handleGetGenerationStatus
      , worldList = commandServiceHandler "list_worlds" HWorld.handleListWorlds
      , worldSave = commandServiceHandler "save_world" HWorld.handleSaveWorld
      , worldLoad = commandServiceHandler "load_world" HWorld.handleLoadWorld
      , worldSetName = commandServiceHandler "set_world_name" HWorld.handleSetWorldName
      }
  , appTerrain = TerrainService
      { terrainGetHex = commandServiceHandler "get_hex" HTerrain.handleGetHex
      , terrainGetChunks = commandServiceHandler "get_chunks" HTerrain.handleGetChunks
      , terrainGetChunkSummary = commandServiceHandler "get_chunk_summary" HTerrain.handleGetChunkSummary
      , terrainGetStats = commandServiceHandler "get_terrain_stats" HTerrain.handleGetTerrainStats
      , terrainGetOverlays = commandServiceHandler "get_overlays" HWorld.handleGetOverlays
      , terrainFindHexes = commandServiceHandler "find_hexes" HQuery.handleFindHexes
      , terrainExportData = commandServiceHandler "export_terrain_data" HQuery.handleExportTerrainData
      }
  , appEditor = EditorService
      { editorToggle = commandServiceHandler "editor_toggle" HEditor.handleEditorToggle
      , editorSetTool = commandServiceHandler "editor_set_tool" HEditor.handleEditorSetTool
      , editorSetBrush = commandServiceHandler "editor_set_brush" HEditor.handleEditorSetBrush
      , editorBrushStroke = commandServiceHandler "editor_brush_stroke" HEditor.handleEditorBrushStroke
      , editorBrushLine = commandServiceHandler "editor_brush_line" HEditor.handleEditorBrushLine
      , editorSetBiome = commandServiceHandler "editor_set_biome" HEditor.handleEditorSetBiome
      , editorSetForm = commandServiceHandler "editor_set_form" HEditor.handleEditorSetForm
      , editorSetHardness = commandServiceHandler "editor_set_hardness" HEditor.handleEditorSetHardness
      , editorUndo = commandServiceHandler "editor_undo" HEditor.handleEditorUndo
      , editorRedo = commandServiceHandler "editor_redo" HEditor.handleEditorRedo
      , editorGetState = commandServiceHandler "editor_get_state" HEditor.handleEditorGetState
      }
  , appPipeline = PipelineService
      { pipelineGet = commandServiceHandler "get_pipeline" HPipeline.handleGetPipeline
      , pipelineSetStageEnabled = commandServiceHandler "set_stage_enabled" HPipeline.handleSetStageEnabled
      }
  , appPlugins = PluginService
      { pluginList = commandServiceHandler "list_plugins" HPlugin.handleListPlugins
      , pluginSetEnabled = commandServiceHandler "set_plugin_enabled" HPlugin.handleSetPluginEnabled
      , pluginSetParam = commandServiceHandler "set_plugin_param" HPlugin.handleSetPluginParam
      }
  , appDataResources = DataResourceService
      { dataListPlugins = commandServiceHandler "data_list_plugins" HData.handleDataListPlugins
      , dataListResources = commandServiceHandler "data_list_resources" HData.handleDataListResources
      , dataListRecords = commandServiceHandler "data_list_records" HData.handleDataListRecords
      , dataGetRecord = commandServiceHandler "data_get_record" HData.handleDataGetRecord
      , dataCreateRecord = commandServiceHandler "data_create_record" HData.handleDataCreateRecord
      , dataUpdateRecord = commandServiceHandler "data_update_record" HData.handleDataUpdateRecord
      , dataDeleteRecord = commandServiceHandler "data_delete_record" HData.handleDataDeleteRecord
      , dataGetState = commandServiceHandler "data_get_state" HData.handleDataGetState
      }
  , appSimulation = SimulationService
      { simulationGetState = commandServiceHandler "get_sim_state" HSimulation.handleGetSimState
      , simulationSetAutoTick = commandServiceHandler "set_sim_auto_tick" HSimulation.handleSetSimAutoTick
      , simulationTick = commandServiceHandler "sim_tick" HSimulation.handleSimTick
      , simulationGetDag = commandServiceHandler "get_sim_dag" HSimulation.handleGetSimDag
      }
  , appLogs = LogService
      { logGet = commandServiceHandler "get_logs" HLog.handleGetLogs
      }
  , appScreenshots = ScreenshotService
      { screenshotTake = commandServiceHandler "take_screenshot" HScreenshot.handleTakeScreenshot
      }
  , appUi = UiService
      { uiSetSeed = commandServiceHandler "set_seed" HView.handleSetSeed
      , uiSetViewMode = commandServiceHandler "set_view_mode" HView.handleSetViewMode
      , uiSetConfigTab = commandServiceHandler "set_config_tab" HView.handleSetConfigTab
      , uiSelectHex = commandServiceHandler "select_hex" HView.handleSelectHex
      , uiSetOverlay = commandServiceHandler "set_overlay" HView.handleSetOverlay
      , uiListOverlayFields = commandServiceHandler "list_overlay_fields" HView.handleListOverlayFields
      , uiCycleOverlay = commandServiceHandler "cycle_overlay" HView.handleCycleOverlay
      , uiCycleOverlayField = commandServiceHandler "cycle_overlay_field" HView.handleCycleOverlayField
      , uiSetCamera = commandServiceHandler "set_camera" HCamera.handleSetCamera
      , uiGetCamera = commandServiceHandler "get_camera" HCamera.handleGetCamera
      , uiZoomToChunk = commandServiceHandler "zoom_to_chunk" HCamera.handleZoomToChunk
      , uiSetLeftPanel = commandServiceHandler "set_left_panel" HPanels.handleSetLeftPanel
      , uiSetLeftTab = commandServiceHandler "set_left_tab" HPanels.handleSetLeftTab
      , uiToggleConfigPanel = commandServiceHandler "toggle_config_panel" HPanels.handleToggleConfigPanel
      , uiSetLogCollapsed = commandServiceHandler "set_log_collapsed" HPanels.handleSetLogCollapsed
      , uiSetLogLevel = commandServiceHandler "set_log_level" HPanels.handleSetLogLevel
      , uiGetPanels = commandServiceHandler "get_ui_panels" HPanels.handleGetUiPanels
      , uiViewportScroll = commandServiceHandler "viewport_scroll" HViewport.handleViewportScroll
      , uiViewportClick = commandServiceHandler "viewport_click" HViewport.handleViewportClick
      , uiViewportDrag = commandServiceHandler "viewport_drag" HViewport.handleViewportDrag
      , uiViewportHover = commandServiceHandler "viewport_hover" HViewport.handleViewportHover
      , uiClickWidget = commandServiceHandler "click_widget" HWidgets.handleClickWidget
      , uiListWidgets = commandServiceHandler "list_widgets" HWidgets.handleListWidgets
      , uiGetWidgetState = commandServiceHandler "get_widget_state" HWidgets.handleGetWidgetState
      , uiGetDialogState = commandServiceHandler "get_dialog_state" HInput.handleGetDialogState
      , uiSetDialogText = commandServiceHandler "set_dialog_text" HInput.handleSetDialogText
      , uiDialogConfirm = commandServiceHandler "dialog_confirm" HInput.handleDialogConfirm
      , uiDialogCancel = commandServiceHandler "dialog_cancel" HInput.handleDialogCancel
      , uiSendKey = commandServiceHandler "send_key" HInput.handleSendKey
      }
  }

-- | Dispatch a command envelope through an AppService implementation.
dispatchAppServiceCommand :: AppService -> CommandContext -> SeerCommand -> IO SeerResponse
dispatchAppServiceCommand app ctx cmd = do
  result <- runAppServiceOperation app ctx (scMethod cmd) (scParams cmd)
  pure (serviceResultToCommandResponse (scId cmd) result)

-- | Invoke one AppService operation from the command adapter context.
runAppServiceOperation :: AppService -> CommandContext -> Text -> Value -> IO ServiceResult
runAppServiceOperation app ctx =
  runServiceOperation app (commandServiceContext ctx)

-- | Invoke one AppService operation from an already transport-neutral context.
runServiceOperation :: AppService -> ServiceContext -> Text -> Value -> IO ServiceResult
runServiceOperation app ctx method params =
  case lookup method (appServiceHandlersByMethod app) of
    Just handler ->
      case validateAppServiceRequest method params of
        Left err -> pure (Left err)
        Right () -> handler ctx (ServiceRequest (Just params))
    Nothing -> pure (Left (ServiceNotFound ("unknown command: " <> method)))

appServiceCommandMethods :: AppService -> [Text]
appServiceCommandMethods = map fst . appServiceHandlersByMethod

commandServiceHandler :: Text -> RawCommandHandler -> ServiceHandler
commandServiceHandler method handler ctx request = do
  let params = fromMaybe Null (serviceRequestBody request)
  case validateAppServiceRequest method params of
    Left err -> pure (Left err)
    Right () -> do
      response <- handler (serviceCommandContext ctx) 0 params
      pure (commandResponseToServiceResult response)

commandResponseToServiceResult :: SeerResponse -> ServiceResult
commandResponseToServiceResult response
  | srSuccess response = Right (ServiceResponse (srResult response))
  | otherwise = Left (commandErrorToServiceError (fromMaybe "command failed" (srError response)))

commandErrorToServiceError :: Text -> ServiceError
commandErrorToServiceError msg
  | matches ["not found", "unknown ", "chunk not found", "record not found"] = ServiceNotFound msg
  | matches ["not visible", "not available", "no terrain loaded", "no terrain generated"] = ServiceUnavailable msg
  | matches ["failed", "rejected"] = ServiceRejected msg
  | otherwise = ServiceInvalidRequest msg
  where
    lowered = Text.toLower msg
    matches needles = any (`Text.isInfixOf` lowered) needles

serviceResultToCommandResponse :: Int -> ServiceResult -> SeerResponse
serviceResultToCommandResponse reqId = \case
  Right response -> okResponse reqId (serviceResponseBody response)
  Left err -> errResponse reqId (serviceErrorText err)
