{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Top-level service surface for topo-seer.
--
-- 'AppService' composes focused service groups that cover the existing command
-- dispatch categories.  Follow-up extraction work can provide concrete
-- implementations for these interfaces and make command IPC, UI actions, and
-- HTTP call through the same behaviour boundary.
module Seer.Service.AppService
  ( AppService(..)
  , AppServiceOperation(..)
  , appServiceGroups
  , appServiceOperations
  , appServiceHandlersByMethod
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

-- | A concrete AppService operation paired with its stable metadata.
data AppServiceOperation = AppServiceOperation
  { appServiceOperationSpec :: !ServiceOperationSpec
  , appServiceOperationHandler :: !ServiceHandler
  }

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

-- | Concrete operations in the same order as 'appServiceOperationSpecs'.
appServiceOperations :: AppService -> [AppServiceOperation]
appServiceOperations app = concat
  [ zipOperations stateServiceOperationSpecs
      [ stateGetState stateSvc
      , stateGetViewModes stateSvc
      , stateGetUiState stateSvc
      ]
  , zipOperations configServiceOperationSpecs
      [ configGetSliders configSvc
      , configGetSlider configSvc
      , configSetSlider configSvc
      , configSetSliders configSvc
      , configResetSliders configSvc
      , configGetSummary configSvc
      , configGetEnums configSvc
      , configListPresets configSvc
      , configSavePreset configSvc
      , configLoadPreset configSvc
      ]
  , zipOperations worldServiceOperationSpecs
      [ worldGenerate worldSvc
      , worldGetMeta worldSvc
      , worldGetGenerationStatus worldSvc
      , worldList worldSvc
      , worldSave worldSvc
      , worldLoad worldSvc
      , worldSetName worldSvc
      ]
  , zipOperations terrainServiceOperationSpecs
      [ terrainGetHex terrainSvc
      , terrainGetChunks terrainSvc
      , terrainGetChunkSummary terrainSvc
      , terrainGetStats terrainSvc
      , terrainGetOverlays terrainSvc
      , terrainFindHexes terrainSvc
      , terrainExportData terrainSvc
      ]
  , zipOperations editorServiceOperationSpecs
      [ editorToggle editorSvc
      , editorSetTool editorSvc
      , editorSetBrush editorSvc
      , editorBrushStroke editorSvc
      , editorBrushLine editorSvc
      , editorSetBiome editorSvc
      , editorSetForm editorSvc
      , editorSetHardness editorSvc
      , editorUndo editorSvc
      , editorRedo editorSvc
      , editorGetState editorSvc
      ]
  , zipOperations pipelineServiceOperationSpecs
      [ pipelineGet pipelineSvc
      , pipelineSetStageEnabled pipelineSvc
      ]
  , zipOperations pluginServiceOperationSpecs
      [ pluginList pluginSvc
      , pluginSetEnabled pluginSvc
      , pluginSetParam pluginSvc
      ]
  , zipOperations dataResourceServiceOperationSpecs
      [ dataListPlugins dataSvc
      , dataListResources dataSvc
      , dataListRecords dataSvc
      , dataGetRecord dataSvc
      , dataCreateRecord dataSvc
      , dataUpdateRecord dataSvc
      , dataDeleteRecord dataSvc
      , dataGetState dataSvc
      ]
  , zipOperations simulationServiceOperationSpecs
      [ simulationGetState simulationSvc
      , simulationSetAutoTick simulationSvc
      , simulationTick simulationSvc
      ]
  , zipOperations logServiceOperationSpecs
      [ logGet logSvc
      ]
  , zipOperations screenshotServiceOperationSpecs
      [ screenshotTake screenshotSvc
      ]
  , zipOperations uiServiceOperationSpecs
      [ uiSetSeed uiSvc
      , uiSetViewMode uiSvc
      , uiSetConfigTab uiSvc
      , uiSelectHex uiSvc
      , uiSetOverlay uiSvc
      , uiListOverlayFields uiSvc
      , uiCycleOverlay uiSvc
      , uiCycleOverlayField uiSvc
      , uiSetCamera uiSvc
      , uiGetCamera uiSvc
      , uiZoomToChunk uiSvc
      , uiSetLeftPanel uiSvc
      , uiSetLeftTab uiSvc
      , uiToggleConfigPanel uiSvc
      , uiSetLogCollapsed uiSvc
      , uiSetLogLevel uiSvc
      , uiGetPanels uiSvc
      , uiViewportScroll uiSvc
      , uiViewportClick uiSvc
      , uiViewportDrag uiSvc
      , uiViewportHover uiSvc
      , uiClickWidget uiSvc
      , uiListWidgets uiSvc
      , uiGetWidgetState uiSvc
      , uiGetDialogState uiSvc
      , uiSetDialogText uiSvc
      , uiDialogConfirm uiSvc
      , uiDialogCancel uiSvc
      , uiSendKey uiSvc
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

appServiceHandlersByMethod :: AppService -> [(Text, ServiceHandler)]
appServiceHandlersByMethod =
  map (\op -> ( serviceOperationMethod (appServiceOperationSpec op)
              , appServiceOperationHandler op
              )
      )
    . appServiceOperations

zipOperations :: [ServiceOperationSpec] -> [ServiceHandler] -> [AppServiceOperation]
zipOperations specs handlers
  | length specs == length handlers = zipWith AppServiceOperation specs handlers
  | otherwise = error "AppService operation metadata/handler count mismatch"
