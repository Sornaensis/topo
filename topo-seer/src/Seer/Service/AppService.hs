{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Top-level service surface for topo-seer.
--
-- 'AppService' composes focused service groups that cover the existing command
-- dispatch categories.  Follow-up extraction work can provide concrete
-- implementations for these interfaces and make internal command IPC
-- compatibility, UI actions, and HTTP call through the same behaviour boundary.
module Seer.Service.AppService
  ( AppService(..)
  , AppServiceOperation(..)
  , appServiceOperationSpec
  , appServiceOperationHandler
  , appServiceGroups
  , appServiceOperations
  , appServiceHandlersByMethod
  , runServiceOperation
  , appServiceOperationSpecs
  , appServiceOperationMethods
    -- * Focused services
  , module Seer.Service.State
  , module Seer.Service.Config
  , module Seer.Service.World
  , module Seer.Service.Terrain
  , module Seer.Service.Editor
  , module Seer.Service.Pipeline
  , module Seer.Service.Plugin
  , module Seer.Service.DataResource
  , module Seer.Service.Simulation
  , module Seer.Service.Log
  , module Seer.Service.Screenshot
  , module Seer.Service.UI
  , module Seer.Service.Context
  , module Seer.Service.Types
  ) where

import Data.Aeson (Value)
import Data.Text (Text)

import Seer.Service.Config
import Seer.Service.Context
import Seer.Service.DataResource
import Seer.Service.Editor
import Seer.Service.Log
import Seer.Service.Pipeline
import Seer.Service.Plugin
import Seer.Service.Screenshot
import Seer.Service.Simulation
import Seer.Service.State
import Seer.Service.Terrain
import Seer.Service.Types
import Seer.Service.UI
import Seer.Service.World

-- | Aggregate service record split along stable behaviour boundaries.
data AppService = AppService
  { appState :: !StateService
  , appConfig :: !ConfigService
  , appWorld :: !WorldService
  , appTerrain :: !TerrainService
  , appEditor :: !EditorService
  , appPipeline :: !PipelineService
  , appPlugins :: !PluginService
  , appDataResources :: !DataResourceService
  , appSimulation :: !SimulationService
  , appLogs :: !LogService
  , appScreenshots :: !ScreenshotService
  , appUi :: !UiService
  }

-- | A concrete AppService operation with existential request/response types.
data AppServiceOperation where
  AppServiceOperation :: TypedServiceOperation request response -> ServiceHandler request response -> AppServiceOperation

appServiceOperationSpec :: AppServiceOperation -> ServiceOperationSpec
appServiceOperationSpec (AppServiceOperation operation _) = typedServiceOperationSpec operation

appServiceOperationHandler :: AppServiceOperation -> RawServiceHandler
appServiceOperationHandler (AppServiceOperation operation handler)
  | serviceHandlerSpec handler == expectedSpec = runServiceHandler handler
  | otherwise = \_ _ ->
      pure (Left (ServiceInternalError ("AppService handler metadata mismatch for method: " <> serviceOperationMethod expectedSpec)))
  where
    expectedSpec = typedServiceOperationSpec operation

-- | Focused service groups in the order used by diagnostics/docs/tests.
appServiceGroups :: [ServiceGroupSpec]
appServiceGroups =
  [ stateServiceGroup
  , configServiceGroup
  , worldServiceGroup
  , terrainServiceGroup
  , editorServiceGroup
  , pipelineServiceGroup
  , pluginServiceGroup
  , dataResourceServiceGroup
  , simulationServiceGroup
  , logServiceGroup
  , screenshotServiceGroup
  , uiServiceGroup
  ]

appServiceOperationSpecs :: [ServiceOperationSpec]
appServiceOperationSpecs = concatMap serviceGroupOperations appServiceGroups

appServiceOperationMethods :: [Text]
appServiceOperationMethods = serviceOperationMethods appServiceOperationSpecs

-- | Invoke one AppService operation by its stable method name.
runServiceOperation :: AppService -> ServiceContext -> Text -> Value -> IO ServiceResult
runServiceOperation app ctx method params =
  case lookup method (appServiceHandlersByMethod app) of
    Just handler -> handler contextWithRunner (ServiceRequest (Just params))
    Nothing -> pure (Left (ServiceUnknownMethod method))
  where
    contextWithRunner = ctx { svcNestedServiceRunner = nestedRunner }
    nestedRunner nestedMethod nestedParams = do
      result <- runServiceOperation app ctx nestedMethod nestedParams
      pure $ case result of
        Left err -> Left (serviceErrorValue err)
        Right response -> Right (serviceResponseBody response)

-- | Concrete operations in the same order as 'appServiceOperationSpecs'.
appServiceOperations :: AppService -> [AppServiceOperation]
appServiceOperations app = concat
  [ [ appOperation stateGetStateOperation (stateGetState stateSvc)
    , appOperation stateGetViewModesOperation (stateGetViewModes stateSvc)
    , appOperation stateGetViewsOperation (stateGetViews stateSvc)
    , appOperation stateGetUiStateOperation (stateGetUiState stateSvc)
    ]
  , [ appOperation configGetSlidersOperation (configGetSliders configSvc)
    , appOperation configGetSliderOperation (configGetSlider configSvc)
    , appOperation configSetSliderOperation (configSetSlider configSvc)
    , appOperation configSetSlidersOperation (configSetSliders configSvc)
    , appOperation configResetSlidersOperation (configResetSliders configSvc)
    , appOperation configGetSummaryOperation (configGetSummary configSvc)
    , appOperation configGetEnumsOperation (configGetEnums configSvc)
    , appOperation configListPresetsOperation (configListPresets configSvc)
    , appOperation configSavePresetOperation (configSavePreset configSvc)
    , appOperation configLoadPresetOperation (configLoadPreset configSvc)
    ]
  , [ appOperation worldGenerateOperation (worldGenerate worldSvc)
    , appOperation worldGetMetaOperation (worldGetMeta worldSvc)
    , appOperation worldGetGenerationStatusOperation (worldGetGenerationStatus worldSvc)
    , appOperation worldListOperation (worldList worldSvc)
    , appOperation worldSaveOperation (worldSave worldSvc)
    , appOperation worldLoadOperation (worldLoad worldSvc)
    , appOperation worldDeleteOperation (worldDelete worldSvc)
    , appOperation worldSetNameOperation (worldSetName worldSvc)
    ]
  , [ appOperation terrainGetHexOperation (terrainGetHex terrainSvc)
    , appOperation terrainGetChunksOperation (terrainGetChunks terrainSvc)
    , appOperation terrainGetChunkSummaryOperation (terrainGetChunkSummary terrainSvc)
    , appOperation terrainGetStatsOperation (terrainGetStats terrainSvc)
    , appOperation terrainGetOverlaysOperation (terrainGetOverlays terrainSvc)
    , appOperation terrainFindHexesOperation (terrainFindHexes terrainSvc)
    , appOperation terrainExportDataOperation (terrainExportData terrainSvc)
    , appOperation overlayGetSchemaOperation (overlayGetSchema terrainSvc)
    , appOperation overlayGetProvenanceOperation (overlayGetProvenance terrainSvc)
    , appOperation overlayExportDataOperation (overlayExportData terrainSvc)
    , appOperation overlayValidateImportOperation (overlayValidateImport terrainSvc)
    , appOperation terrainExportMeshOperation (terrainExportMesh terrainSvc)
    , appOperation terrainExportSampleOperation (terrainExportSample terrainSvc)
    ]
  , [ appOperation editorToggleOperation (editorToggle editorSvc)
    , appOperation editorSetToolOperation (editorSetTool editorSvc)
    , appOperation editorSetBrushOperation (editorSetBrush editorSvc)
    , appOperation editorBrushStrokeOperation (editorBrushStroke editorSvc)
    , appOperation editorBrushLineOperation (editorBrushLine editorSvc)
    , appOperation editorSetBiomeOperation (editorSetBiome editorSvc)
    , appOperation editorSetFormOperation (editorSetForm editorSvc)
    , appOperation editorSetHardnessOperation (editorSetHardness editorSvc)
    , appOperation editorUndoOperation (editorUndo editorSvc)
    , appOperation editorRedoOperation (editorRedo editorSvc)
    , appOperation editorGetStateOperation (editorGetState editorSvc)
    ]
  , [ appOperation pipelineGetOperation (pipelineGet pipelineSvc)
    , appOperation pipelineSetStageEnabledOperation (pipelineSetStageEnabled pipelineSvc)
    ]
  , [ appOperation pluginListOperation (pluginList pluginSvc)
    , appOperation pluginSetEnabledOperation (pluginSetEnabled pluginSvc)
    , appOperation pluginSetParamOperation (pluginSetParam pluginSvc)
    ]
  , [ appOperation dataResourceListPluginsOperation (dataListPlugins dataSvc)
    , appOperation dataResourceListResourcesOperation (dataListResources dataSvc)
    , appOperation dataResourceListRecordsOperation (dataListRecords dataSvc)
    , appOperation dataResourceGetRecordOperation (dataGetRecord dataSvc)
    , appOperation dataResourceCreateRecordOperation (dataCreateRecord dataSvc)
    , appOperation dataResourceUpdateRecordOperation (dataUpdateRecord dataSvc)
    , appOperation dataResourceDeleteRecordOperation (dataDeleteRecord dataSvc)
    , appOperation dataResourceStateOperation (dataGetState dataSvc)
    ]
  , [ appOperation simulationStateOperation (simulationGetState simulationSvc)
    , appOperation simulationSetAutoTickOperation (simulationSetAutoTick simulationSvc)
    , appOperation simulationTickOperation (simulationTick simulationSvc)
    , appOperation simulationDagOperation (simulationGetDag simulationSvc)
    ]
  , [ appOperation logGetOperation (logGet logSvc)
    ]
  , [ appOperation screenshotTakeOperation (screenshotTake screenshotSvc)
    ]
  , [ appOperation uiSetSeedOperation (uiSetSeed uiSvc)
    , appOperation uiSetViewModeOperation (uiSetViewMode uiSvc)
    , appOperation uiSetViewOperation (uiSetView uiSvc)
    , appOperation uiSetConfigTabOperation (uiSetConfigTab uiSvc)
    , appOperation uiSelectHexOperation (uiSelectHex uiSvc)
    , appOperation uiSetOverlayOperation (uiSetOverlay uiSvc)
    , appOperation uiListOverlayFieldsOperation (uiListOverlayFields uiSvc)
    , appOperation uiCycleOverlayOperation (uiCycleOverlay uiSvc)
    , appOperation uiCycleOverlayFieldOperation (uiCycleOverlayField uiSvc)
    , appOperation uiSetCameraOperation (uiSetCamera uiSvc)
    , appOperation uiGetCameraOperation (uiGetCamera uiSvc)
    , appOperation uiZoomToChunkOperation (uiZoomToChunk uiSvc)
    , appOperation uiSetLeftPanelOperation (uiSetLeftPanel uiSvc)
    , appOperation uiSetLeftTabOperation (uiSetLeftTab uiSvc)
    , appOperation uiToggleConfigPanelOperation (uiToggleConfigPanel uiSvc)
    , appOperation uiSetLogCollapsedOperation (uiSetLogCollapsed uiSvc)
    , appOperation uiSetLogLevelOperation (uiSetLogLevel uiSvc)
    , appOperation uiGetPanelsOperation (uiGetPanels uiSvc)
    , appOperation uiViewportScrollOperation (uiViewportScroll uiSvc)
    , appOperation uiViewportClickOperation (uiViewportClick uiSvc)
    , appOperation uiViewportDragOperation (uiViewportDrag uiSvc)
    , appOperation uiViewportHoverOperation (uiViewportHover uiSvc)
    , appOperation uiClickWidgetOperation (uiClickWidget uiSvc)
    , appOperation uiListWidgetsOperation (uiListWidgets uiSvc)
    , appOperation uiGetWidgetStateOperation (uiGetWidgetState uiSvc)
    , appOperation uiGetDialogStateOperation (uiGetDialogState uiSvc)
    , appOperation uiSetDialogTextOperation (uiSetDialogText uiSvc)
    , appOperation uiDialogConfirmOperation (uiDialogConfirm uiSvc)
    , appOperation uiDialogCancelOperation (uiDialogCancel uiSvc)
    , appOperation uiSendKeyOperation (uiSendKey uiSvc)
    ]
  ]
  where
    stateSvc = appState app
    configSvc = appConfig app
    worldSvc = appWorld app
    terrainSvc = appTerrain app
    editorSvc = appEditor app
    pipelineSvc = appPipeline app
    pluginSvc = appPlugins app
    dataSvc = appDataResources app
    simulationSvc = appSimulation app
    logSvc = appLogs app
    screenshotSvc = appScreenshots app
    uiSvc = appUi app

appServiceHandlersByMethod :: AppService -> [(Text, RawServiceHandler)]
appServiceHandlersByMethod =
  map (\op -> ( serviceOperationMethod (appServiceOperationSpec op)
              , appServiceOperationHandler op
              )
      )
    . appServiceOperations

appOperation :: TypedServiceOperation request response -> ServiceHandler request response -> AppServiceOperation
appOperation = AppServiceOperation
