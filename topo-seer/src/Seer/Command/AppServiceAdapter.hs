{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- | Internal command IPC compatibility adapter for the AppService boundary.
--
-- This module hosts the transitional AppService implementation backed by the
-- existing command handlers. The command dispatcher itself stays thin: it
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

import Actor.Log (getLogSnapshot)
import Actor.SnapshotReceiver (publishChangedUiAndLog)
import Actor.UI (getUiSnapshot)
import Actor.UiActions.Handles (ActorHandles(..))
import Seer.Command.Context
  ( CommandContext(..)
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
import Topo.Plugin.RPC.DataService (DataResourceFailure(..), dataResourceFailureText, parseDataResourceFailureText)
import Seer.Service.Validation
  ( serviceRequestBodyValue
  , validateServiceHandler
  )

type RawCommandHandler = CommandContext -> Int -> Value -> IO SeerResponse

-- | Transitional AppService implementation backed by existing command handlers.
commandAppService :: AppService
commandAppService = AppService
  { appState = StateService
      { stateGetState = commandServiceHandler stateGetStateOperation HState.handleGetState
      , stateGetViewModes = commandServiceHandler stateGetViewModesOperation HState.handleGetViewModes
      , stateGetViews = commandServiceHandler stateGetViewsOperation HState.handleGetViews
      , stateGetUiState = commandServiceHandler stateGetUiStateOperation HState.handleGetUiState
      }
  , appConfig = ConfigService
      { configGetSliders = commandServiceHandler configGetSlidersOperation HSliders.handleGetSliders
      , configGetSlider = commandServiceHandler configGetSliderOperation HSliders.handleGetSlider
      , configSetSlider = commandServiceHandler configSetSliderOperation HSliders.handleSetSlider
      , configSetSliders = commandServiceHandler configSetSlidersOperation HSliders.handleSetSliders
      , configResetSliders = commandServiceHandler configResetSlidersOperation HSliders.handleResetSliders
      , configGetSummary = commandServiceHandler configGetSummaryOperation HSliders.handleGetConfigSummary
      , configGetEnums = commandServiceHandler configGetEnumsOperation HEnums.handleGetEnums
      , configListPresets = commandServiceHandler configListPresetsOperation HPresets.handleListPresets
      , configSavePreset = commandServiceHandler configSavePresetOperation HPresets.handleSavePreset
      , configLoadPreset = commandServiceHandler configLoadPresetOperation HPresets.handleLoadPreset
      }
  , appWorld = WorldService
      { worldGenerate = commandServiceHandler worldGenerateOperation HGenerate.handleGenerate
      , worldGetMeta = commandServiceHandler worldGetMetaOperation HWorld.handleGetWorldMeta
      , worldGetGenerationStatus = commandServiceHandler worldGetGenerationStatusOperation HWorld.handleGetGenerationStatus
      , worldList = commandServiceHandler worldListOperation HWorld.handleListWorlds
      , worldSave = commandServiceHandler worldSaveOperation HWorld.handleSaveWorld
      , worldLoad = commandServiceHandler worldLoadOperation HWorld.handleLoadWorld
      , worldSetName = commandServiceHandler worldSetNameOperation HWorld.handleSetWorldName
      }
  , appTerrain = TerrainService
      { terrainGetHex = commandServiceHandler terrainGetHexOperation HTerrain.handleGetHex
      , terrainGetChunks = commandServiceHandler terrainGetChunksOperation HTerrain.handleGetChunks
      , terrainGetChunkSummary = commandServiceHandler terrainGetChunkSummaryOperation HTerrain.handleGetChunkSummary
      , terrainGetStats = commandServiceHandler terrainGetStatsOperation HTerrain.handleGetTerrainStats
      , terrainGetOverlays = commandServiceHandler terrainGetOverlaysOperation HWorld.handleGetOverlays
      , terrainFindHexes = commandServiceHandler terrainFindHexesOperation HQuery.handleFindHexes
      , terrainExportData = commandServiceHandler terrainExportDataOperation HQuery.handleExportTerrainData
      , overlayGetSchema = commandServiceHandler overlayGetSchemaOperation HWorld.handleGetOverlaySchema
      , overlayGetProvenance = commandServiceHandler overlayGetProvenanceOperation HWorld.handleGetOverlayProvenance
      , overlayExportData = commandServiceHandler overlayExportDataOperation HWorld.handleExportOverlayData
      , overlayValidateImport = commandServiceHandler overlayValidateImportOperation HWorld.handleValidateOverlayImport
      , terrainExportMesh = commandServiceHandler terrainExportMeshOperation HQuery.handleExportMeshData
      , terrainExportSample = commandServiceHandler terrainExportSampleOperation HQuery.handleExportSampleData
      }
  , appEditor = EditorService
      { editorToggle = commandServiceHandler editorToggleOperation HEditor.handleEditorToggle
      , editorSetTool = commandServiceHandler editorSetToolOperation HEditor.handleEditorSetTool
      , editorSetBrush = commandServiceHandler editorSetBrushOperation HEditor.handleEditorSetBrush
      , editorBrushStroke = commandServiceHandler editorBrushStrokeOperation HEditor.handleEditorBrushStroke
      , editorBrushLine = commandServiceHandler editorBrushLineOperation HEditor.handleEditorBrushLine
      , editorSetBiome = commandServiceHandler editorSetBiomeOperation HEditor.handleEditorSetBiome
      , editorSetForm = commandServiceHandler editorSetFormOperation HEditor.handleEditorSetForm
      , editorSetHardness = commandServiceHandler editorSetHardnessOperation HEditor.handleEditorSetHardness
      , editorUndo = commandServiceHandler editorUndoOperation HEditor.handleEditorUndo
      , editorRedo = commandServiceHandler editorRedoOperation HEditor.handleEditorRedo
      , editorGetState = commandServiceHandler editorGetStateOperation HEditor.handleEditorGetState
      }
  , appPipeline = PipelineService
      { pipelineGet = commandServiceHandler pipelineGetOperation HPipeline.handleGetPipeline
      , pipelineSetStageEnabled = commandServiceHandler pipelineSetStageEnabledOperation HPipeline.handleSetStageEnabled
      }
  , appPlugins = PluginService
      { pluginList = commandServiceHandler pluginListOperation HPlugin.handleListPlugins
      , pluginSetEnabled = commandServiceHandler pluginSetEnabledOperation HPlugin.handleSetPluginEnabled
      , pluginSetParam = commandServiceResultHandler pluginSetParamOperation HPlugin.handleSetPluginParamService
      }
  , appDataResources = DataResourceService
      { dataListPlugins = commandServiceHandler dataResourceListPluginsOperation HData.handleDataListPlugins
      , dataListResources = commandServiceHandler dataResourceListResourcesOperation HData.handleDataListResources
      , dataListRecords = commandServiceHandler dataResourceListRecordsOperation HData.handleDataListRecords
      , dataGetRecord = commandServiceHandler dataResourceGetRecordOperation HData.handleDataGetRecord
      , dataCreateRecord = commandServiceHandler dataResourceCreateRecordOperation HData.handleDataCreateRecord
      , dataUpdateRecord = commandServiceHandler dataResourceUpdateRecordOperation HData.handleDataUpdateRecord
      , dataDeleteRecord = commandServiceHandler dataResourceDeleteRecordOperation HData.handleDataDeleteRecord
      , dataGetState = commandServiceHandler dataResourceStateOperation HData.handleDataGetState
      }
  , appSimulation = SimulationService
      { simulationGetState = commandServiceHandler simulationStateOperation HSimulation.handleGetSimState
      , simulationSetAutoTick = commandServiceHandler simulationSetAutoTickOperation HSimulation.handleSetSimAutoTick
      , simulationTick = commandServiceHandler simulationTickOperation HSimulation.handleSimTick
      , simulationGetDag = commandServiceHandler simulationDagOperation HSimulation.handleGetSimDag
      }
  , appLogs = LogService
      { logGet = commandServiceHandler logGetOperation HLog.handleGetLogs
      }
  , appScreenshots = ScreenshotService
      { screenshotTake = rendererScreenshotHandler
      }
  , appUi = UiService
      { uiSetSeed = commandServiceHandler uiSetSeedOperation HView.handleSetSeed
      , uiSetViewMode = commandServiceHandler uiSetViewModeOperation HView.handleSetViewMode
      , uiSetView = commandServiceHandler uiSetViewOperation HView.handleSetView
      , uiSetConfigTab = commandServiceHandler uiSetConfigTabOperation HView.handleSetConfigTab
      , uiSelectHex = commandServiceHandler uiSelectHexOperation HView.handleSelectHex
      , uiSetOverlay = commandServiceHandler uiSetOverlayOperation HView.handleSetOverlay
      , uiListOverlayFields = commandServiceHandler uiListOverlayFieldsOperation HView.handleListOverlayFields
      , uiCycleOverlay = commandServiceHandler uiCycleOverlayOperation HView.handleCycleOverlay
      , uiCycleOverlayField = commandServiceHandler uiCycleOverlayFieldOperation HView.handleCycleOverlayField
      , uiSetCamera = commandServiceHandler uiSetCameraOperation HCamera.handleSetCamera
      , uiGetCamera = commandServiceHandler uiGetCameraOperation HCamera.handleGetCamera
      , uiZoomToChunk = commandServiceHandler uiZoomToChunkOperation HCamera.handleZoomToChunk
      , uiSetLeftPanel = commandServiceHandler uiSetLeftPanelOperation HPanels.handleSetLeftPanel
      , uiSetLeftTab = commandServiceHandler uiSetLeftTabOperation HPanels.handleSetLeftTab
      , uiToggleConfigPanel = commandServiceHandler uiToggleConfigPanelOperation HPanels.handleToggleConfigPanel
      , uiSetLogCollapsed = commandServiceHandler uiSetLogCollapsedOperation HPanels.handleSetLogCollapsed
      , uiSetLogLevel = commandServiceHandler uiSetLogLevelOperation HPanels.handleSetLogLevel
      , uiGetPanels = commandServiceHandler uiGetPanelsOperation HPanels.handleGetUiPanels
      , uiViewportScroll = commandServiceHandler uiViewportScrollOperation HViewport.handleViewportScroll
      , uiViewportClick = commandServiceHandler uiViewportClickOperation HViewport.handleViewportClick
      , uiViewportDrag = commandServiceHandler uiViewportDragOperation HViewport.handleViewportDrag
      , uiViewportHover = commandServiceHandler uiViewportHoverOperation HViewport.handleViewportHover
      , uiClickWidget = commandServiceHandler uiClickWidgetOperation HWidgets.handleClickWidget
      , uiListWidgets = commandServiceHandler uiListWidgetsOperation HWidgets.handleListWidgets
      , uiGetWidgetState = commandServiceHandler uiGetWidgetStateOperation HWidgets.handleGetWidgetState
      , uiGetDialogState = commandServiceHandler uiGetDialogStateOperation HInput.handleGetDialogState
      , uiSetDialogText = commandServiceHandler uiSetDialogTextOperation HInput.handleSetDialogText
      , uiDialogConfirm = commandServiceHandler uiDialogConfirmOperation HInput.handleDialogConfirm
      , uiDialogCancel = commandServiceHandler uiDialogCancelOperation HInput.handleDialogCancel
      , uiSendKey = commandServiceHandler uiSendKeyOperation HInput.handleSendKey
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

appServiceCommandMethods :: AppService -> [Text]
appServiceCommandMethods = map fst . appServiceHandlersByMethod

commandServiceHandler :: TypedServiceOperation request response -> RawCommandHandler -> ServiceHandler request response
commandServiceHandler operation handler = validateServiceHandler operation $ \ctx request -> do
  let commandCtx = serviceCommandContext ctx
      params = serviceRequestBodyValue request
  response <- withMutationPublication commandCtx srSuccess $
    handler commandCtx 0 params
  pure (commandResponseToServiceResult response)

commandServiceResultHandler
  :: TypedServiceOperation request response
  -> (CommandContext -> Value -> IO ServiceResult)
  -> ServiceHandler request response
commandServiceResultHandler operation handler = validateServiceHandler operation $ \ctx request -> do
  let commandCtx = serviceCommandContext ctx
      params = serviceRequestBodyValue request
  withMutationPublication commandCtx isServiceSuccess $
    handler commandCtx params
  where
    isServiceSuccess (Right _) = True
    isServiceSuccess (Left _) = False

-- | Close legacy handler gaps centrally while each handler is migrated to
-- explicit ordered publication. Mailbox barriers make a successful command's
-- UI/log mutation observable before its response is returned. Handlers that
-- already published (notably atlas-producing mutations) retain their epoch.
withMutationPublication :: CommandContext -> (a -> Bool) -> IO a -> IO a
withMutationPublication ctx succeeded action = do
  let handles = ccActorHandles ctx
  uiBefore <- getUiSnapshot (ahUiHandle handles)
  logBefore <- getLogSnapshot (ahLogHandle handles)
  result <- action
  if not (succeeded result)
    then pure result
    else do
      publishChangedUiAndLog
        (ahSnapshotVersionRef handles)
        uiBefore
        logBefore
        (getUiSnapshot (ahUiHandle handles))
        (getLogSnapshot (ahLogHandle handles))
      pure result

commandResponseToServiceResult :: SeerResponse -> ServiceResult
commandResponseToServiceResult response
  | srSuccess response = Right (ServiceResponse (srResult response))
  | otherwise = Left (commandErrorToServiceError (fromMaybe "command failed" (srError response)))

commandErrorToServiceError :: Text -> ServiceError
commandErrorToServiceError msg
  | Just failure <- parseDataResourceFailureText msg =
      ServiceDataResourceError (drfCode failure) (drfMessage failure) []
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
  Left (ServiceUnknownMethod method) ->
    errResponse reqId ("unknown command: " <> method)
  Left (ServiceDataResourceError code msg _) ->
    errResponse reqId (dataResourceFailureText (DataResourceFailure code msg))
  Left err -> errResponse reqId (serviceErrorText err)
