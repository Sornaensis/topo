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
  , appServiceCommandMethods
  ) where

import Data.Aeson (Value(..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)

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

type RawCommandHandler = CommandContext -> Int -> Value -> IO SeerResponse

-- | Transitional AppService implementation backed by existing command handlers.
commandAppService :: AppService
commandAppService = AppService
  { appState = StateService
      { stateGetState = commandServiceHandler HState.handleGetState
      , stateGetViewModes = commandServiceHandler HState.handleGetViewModes
      , stateGetUiState = commandServiceHandler HState.handleGetUiState
      }
  , appConfig = ConfigService
      { configGetSliders = commandServiceHandler HSliders.handleGetSliders
      , configGetSlider = commandServiceHandler HSliders.handleGetSlider
      , configSetSlider = commandServiceHandler HSliders.handleSetSlider
      , configSetSliders = commandServiceHandler HSliders.handleSetSliders
      , configResetSliders = commandServiceHandler HSliders.handleResetSliders
      , configGetSummary = commandServiceHandler HSliders.handleGetConfigSummary
      , configGetEnums = commandServiceHandler HEnums.handleGetEnums
      , configListPresets = commandServiceHandler HPresets.handleListPresets
      , configSavePreset = commandServiceHandler HPresets.handleSavePreset
      , configLoadPreset = commandServiceHandler HPresets.handleLoadPreset
      }
  , appWorld = WorldService
      { worldGenerate = commandServiceHandler HGenerate.handleGenerate
      , worldGetMeta = commandServiceHandler HWorld.handleGetWorldMeta
      , worldGetGenerationStatus = commandServiceHandler HWorld.handleGetGenerationStatus
      , worldList = commandServiceHandler HWorld.handleListWorlds
      , worldSave = commandServiceHandler HWorld.handleSaveWorld
      , worldLoad = commandServiceHandler HWorld.handleLoadWorld
      , worldSetName = commandServiceHandler HWorld.handleSetWorldName
      }
  , appTerrain = TerrainService
      { terrainGetHex = commandServiceHandler HTerrain.handleGetHex
      , terrainGetChunks = commandServiceHandler HTerrain.handleGetChunks
      , terrainGetChunkSummary = commandServiceHandler HTerrain.handleGetChunkSummary
      , terrainGetStats = commandServiceHandler HTerrain.handleGetTerrainStats
      , terrainGetOverlays = commandServiceHandler HWorld.handleGetOverlays
      , terrainFindHexes = commandServiceHandler HQuery.handleFindHexes
      , terrainExportData = commandServiceHandler HQuery.handleExportTerrainData
      }
  , appEditor = EditorService
      { editorToggle = commandServiceHandler HEditor.handleEditorToggle
      , editorSetTool = commandServiceHandler HEditor.handleEditorSetTool
      , editorSetBrush = commandServiceHandler HEditor.handleEditorSetBrush
      , editorBrushStroke = commandServiceHandler HEditor.handleEditorBrushStroke
      , editorBrushLine = commandServiceHandler HEditor.handleEditorBrushLine
      , editorSetBiome = commandServiceHandler HEditor.handleEditorSetBiome
      , editorSetForm = commandServiceHandler HEditor.handleEditorSetForm
      , editorSetHardness = commandServiceHandler HEditor.handleEditorSetHardness
      , editorUndo = commandServiceHandler HEditor.handleEditorUndo
      , editorRedo = commandServiceHandler HEditor.handleEditorRedo
      , editorGetState = commandServiceHandler HEditor.handleEditorGetState
      }
  , appPipeline = PipelineService
      { pipelineGet = commandServiceHandler HPipeline.handleGetPipeline
      , pipelineSetStageEnabled = commandServiceHandler HPipeline.handleSetStageEnabled
      }
  , appPlugins = PluginService
      { pluginList = commandServiceHandler HPlugin.handleListPlugins
      , pluginSetEnabled = commandServiceHandler HPlugin.handleSetPluginEnabled
      , pluginSetParam = commandServiceHandler HPlugin.handleSetPluginParam
      }
  , appDataResources = DataResourceService
      { dataListPlugins = commandServiceHandler HData.handleDataListPlugins
      , dataListResources = commandServiceHandler HData.handleDataListResources
      , dataListRecords = commandServiceHandler HData.handleDataListRecords
      , dataGetRecord = commandServiceHandler HData.handleDataGetRecord
      , dataCreateRecord = commandServiceHandler HData.handleDataCreateRecord
      , dataUpdateRecord = commandServiceHandler HData.handleDataUpdateRecord
      , dataDeleteRecord = commandServiceHandler HData.handleDataDeleteRecord
      , dataGetState = commandServiceHandler HData.handleDataGetState
      }
  , appSimulation = SimulationService
      { simulationGetState = commandServiceHandler HSimulation.handleGetSimState
      , simulationSetAutoTick = commandServiceHandler HSimulation.handleSetSimAutoTick
      , simulationTick = commandServiceHandler HSimulation.handleSimTick
      }
  , appLogs = LogService
      { logGet = commandServiceHandler HLog.handleGetLogs
      }
  , appScreenshots = ScreenshotService
      { screenshotTake = commandServiceHandler HScreenshot.handleTakeScreenshot
      }
  , appUi = UiService
      { uiSetSeed = commandServiceHandler HView.handleSetSeed
      , uiSetViewMode = commandServiceHandler HView.handleSetViewMode
      , uiSetConfigTab = commandServiceHandler HView.handleSetConfigTab
      , uiSelectHex = commandServiceHandler HView.handleSelectHex
      , uiSetOverlay = commandServiceHandler HView.handleSetOverlay
      , uiListOverlayFields = commandServiceHandler HView.handleListOverlayFields
      , uiCycleOverlay = commandServiceHandler HView.handleCycleOverlay
      , uiCycleOverlayField = commandServiceHandler HView.handleCycleOverlayField
      , uiSetCamera = commandServiceHandler HCamera.handleSetCamera
      , uiGetCamera = commandServiceHandler HCamera.handleGetCamera
      , uiZoomToChunk = commandServiceHandler HCamera.handleZoomToChunk
      , uiSetLeftPanel = commandServiceHandler HPanels.handleSetLeftPanel
      , uiSetLeftTab = commandServiceHandler HPanels.handleSetLeftTab
      , uiToggleConfigPanel = commandServiceHandler HPanels.handleToggleConfigPanel
      , uiSetLogCollapsed = commandServiceHandler HPanels.handleSetLogCollapsed
      , uiSetLogLevel = commandServiceHandler HPanels.handleSetLogLevel
      , uiGetPanels = commandServiceHandler HPanels.handleGetUiPanels
      , uiViewportScroll = commandServiceHandler HViewport.handleViewportScroll
      , uiViewportClick = commandServiceHandler HViewport.handleViewportClick
      , uiViewportDrag = commandServiceHandler HViewport.handleViewportDrag
      , uiViewportHover = commandServiceHandler HViewport.handleViewportHover
      , uiClickWidget = commandServiceHandler HWidgets.handleClickWidget
      , uiListWidgets = commandServiceHandler HWidgets.handleListWidgets
      , uiGetWidgetState = commandServiceHandler HWidgets.handleGetWidgetState
      , uiGetDialogState = commandServiceHandler HInput.handleGetDialogState
      , uiSetDialogText = commandServiceHandler HInput.handleSetDialogText
      , uiDialogConfirm = commandServiceHandler HInput.handleDialogConfirm
      , uiDialogCancel = commandServiceHandler HInput.handleDialogCancel
      , uiSendKey = commandServiceHandler HInput.handleSendKey
      }
  }

-- | Dispatch a command envelope through an AppService implementation.
dispatchAppServiceCommand :: AppService -> CommandContext -> SeerCommand -> IO SeerResponse
dispatchAppServiceCommand app ctx cmd = do
  result <- runAppServiceOperation app ctx (scMethod cmd) (scParams cmd)
  pure (serviceResultToCommandResponse (scId cmd) result)

-- | Invoke one AppService operation from the command adapter context.
runAppServiceOperation :: AppService -> CommandContext -> Text -> Value -> IO ServiceResult
runAppServiceOperation app ctx method params =
  case lookup method (appServiceHandlersByMethod app) of
    Just handler -> handler (commandServiceContext ctx) (ServiceRequest (Just params))
    Nothing -> pure (Left (ServiceNotFound ("unknown command: " <> method)))

appServiceCommandMethods :: AppService -> [Text]
appServiceCommandMethods = map fst . appServiceHandlersByMethod

commandServiceHandler :: RawCommandHandler -> ServiceHandler
commandServiceHandler handler ctx request = do
  response <- handler (serviceCommandContext ctx) 0 (fromMaybe Null (serviceRequestBody request))
  pure (commandResponseToServiceResult response)

commandResponseToServiceResult :: SeerResponse -> ServiceResult
commandResponseToServiceResult response
  | srSuccess response = Right (ServiceResponse (srResult response))
  | otherwise = Left (ServiceInvalidRequest (fromMaybe "command failed" (srError response)))

serviceResultToCommandResponse :: Int -> ServiceResult -> SeerResponse
serviceResultToCommandResponse reqId = \case
  Right response -> okResponse reqId (serviceResponseBody response)
  Left err -> errResponse reqId (serviceErrorText err)

serviceErrorText :: ServiceError -> Text
serviceErrorText = \case
  ServiceInvalidRequest msg -> msg
  ServiceNotFound msg -> msg
  ServiceUnavailable msg -> msg
  ServiceRejected msg -> msg
  ServiceInternalError msg -> msg
