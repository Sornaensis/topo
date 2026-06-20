{-# LANGUAGE OverloadedStrings #-}

module Spec.AppService (spec) where

import Actor.Log (LogLevel(..))
import Data.Aeson (Value(..))
import Data.List (nub, sort)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Test.Hspec
import Topo.Overlay.Schema (OverlayFieldType(..))
import Topo.Simulation (SimNodeId(..))
import Topo.Types (ChunkId(..))

import Seer.Command.AppServiceAdapter (appServiceCommandMethods, commandAppService)
import Seer.Command.Dispatch (dispatchCommandMethods)
import Seer.Service.AppService
  ( AppServiceOperation(..)
  , appServiceGroups
  , appServiceOperationMethods
  , appServiceOperationSpecs
  , appServiceOperations
  , ConfigListPresetsResponse(..)
  , ConfigSliderSummary(..)
  , ConfigSlidersResponse(..)
  , DataResourceStateResponse(..)
  , EditorActionQueuedResponse(..)
  , EditorSetBrushRequest(..)
  , EditorStrokeQueuedResponse(..)
  , LogEntrySummary(..)
  , LogGetRequest(..)
  , LogGetResponse(..)
  , PluginSummary(..)
  , ScreenshotTakeRequest(..)
  , ScreenshotTakeResponse(..)
  , SimulationDagNodeSummary(..)
  , SimulationDagResponse(..)
  , SimulationStateResponse(..)
  , StateHexCoord(..)
  , StateSummaryResponse(..)
  , StateViewModeSummary(..)
  , StateViewModesResponse(..)
  , TerrainExportResponse(..)
  , TerrainFilterOp(..)
  , TerrainFindFilter(..)
  , TerrainFindHexesRequest(..)
  , UiCameraSnapshot(..)
  , UiHexCoord(..)
  , UiListOverlayFieldsResponse(..)
  , UiLogPanelState(..)
  , UiPanelTabState(..)
  , UiPanelsResponse(..)
  , UiOverlayFieldSummary(..)
  , UiViewportClickResponse(..)
  , WorldGenerationStatusResponse(..)
  , WorldMetaResponse(..)
  , dataResourceCreateRecordOperation
  , dataResourceDeleteRecordOperation
  , dataResourceGetRecordOperation
  , dataResourceListPluginsOperation
  , dataResourceListRecordsOperation
  , dataResourceListResourcesOperation
  , dataResourceStateOperation
  , dataResourceUpdateRecordOperation
  , configGetEnumsOperation
  , configGetSliderOperation
  , configGetSlidersOperation
  , configGetSummaryOperation
  , configListPresetsOperation
  , configLoadPresetOperation
  , configResetSlidersOperation
  , configSavePresetOperation
  , configSetSliderOperation
  , configSetSlidersOperation
  , editorBrushLineOperation
  , editorBrushStrokeOperation
  , editorGetStateOperation
  , editorRedoOperation
  , editorSetBiomeOperation
  , editorSetBrushOperation
  , editorSetFormOperation
  , editorSetHardnessOperation
  , editorSetToolOperation
  , editorToggleOperation
  , editorUndoOperation
  , logGetOperation
  , pipelineGetOperation
  , pipelineSetStageEnabledOperation
  , pluginListOperation
  , pluginSetEnabledOperation
  , pluginSetParamOperation
  , screenshotTakeOperation
  , simulationDagOperation
  , simulationSetAutoTickOperation
  , simulationStateOperation
  , simulationTickOperation
  , stateGetStateOperation
  , stateGetUiStateOperation
  , stateGetViewModesOperation
  , terrainExportDataOperation
  , terrainFindHexesOperation
  , terrainGetChunkSummaryOperation
  , terrainGetChunksOperation
  , terrainGetHexOperation
  , terrainGetOverlaysOperation
  , terrainGetStatsOperation
  , uiCycleOverlayFieldOperation
  , uiCycleOverlayOperation
  , uiGetCameraOperation
  , uiGetPanelsOperation
  , uiListOverlayFieldsOperation
  , uiSelectHexOperation
  , uiSetCameraOperation
  , uiSetConfigTabOperation
  , uiSetLeftPanelOperation
  , uiSetLeftTabOperation
  , uiSetLogCollapsedOperation
  , uiSetLogLevelOperation
  , uiSetOverlayOperation
  , uiSetSeedOperation
  , uiSetViewModeOperation
  , uiToggleConfigPanelOperation
  , uiViewportClickOperation
  , uiViewportDragOperation
  , uiViewportHoverOperation
  , uiViewportScrollOperation
  , uiClickWidgetOperation
  , uiListWidgetsOperation
  , uiGetWidgetStateOperation
  , uiGetDialogStateOperation
  , uiSetDialogTextOperation
  , uiDialogConfirmOperation
  , uiDialogCancelOperation
  , uiSendKeyOperation
  , uiZoomToChunkOperation
  , worldGenerateOperation
  , worldGetGenerationStatusOperation
  , worldGetMetaOperation
  , worldListOperation
  , worldLoadOperation
  , worldSaveOperation
  , worldSetNameOperation
  )
import Seer.Service.Types
  ( AsyncStatusPhase(..)
  , AsyncStatusSnapshot(..)
  , ServiceEventEnvelope(..)
  , ServiceEventPublishRequest(..)
  , ServiceEventPublishResponse(..)
  , ServiceEventSeverity(..)
  , ServiceEventSource(..)
  , ServiceGroupSpec(..)
  , ServiceOperationSpec(..)
  , TypedServiceOperation(..)
  , groupOperationMethods
  , serviceEventPublishOperation
  )

spec :: Spec
spec = describe "AppService surface" $ do
  it "covers every current command method exactly once" $ do
    sort appServiceOperationMethods `shouldBe` sort expectedCommandMethods
    appServiceOperationMethods `shouldBe` nub appServiceOperationMethods

  it "is the method catalog exposed by command dispatch" $
    sort dispatchCommandMethods `shouldBe` sort appServiceOperationMethods

  it "has command-backed handlers for every AppService operation" $
    appServiceCommandMethods commandAppService `shouldBe` appServiceOperationMethods

  it "pairs concrete handlers with their typed operation metadata" $
    map appServiceOperationSpec (appServiceOperations commandAppService) `shouldBe` appServiceOperationSpecs

  it "defines stable operation names exactly once" $ do
    let operationNames = map serviceOperationName appServiceOperationSpecs
    operationNames `shouldBe` nub operationNames

  it "defines focused service groups for the M2 behaviour boundary" $
    map (\group -> (serviceGroupName group, groupOperationMethods group)) appServiceGroups
      `shouldBe` expectedServiceGroups

  it "defines typed operation contracts for M2 service groups" $
    typedOperationMethods `shouldBe`
      [ "get_state"
      , "get_view_modes"
      , "get_ui_state"
      , "get_sliders"
      , "get_slider"
      , "set_slider"
      , "set_sliders"
      , "reset_sliders"
      , "get_config_summary"
      , "get_enums"
      , "list_presets"
      , "save_preset"
      , "load_preset"
      , "generate"
      , "get_world_meta"
      , "get_generation_status"
      , "list_worlds"
      , "save_world"
      , "load_world"
      , "set_world_name"
      , "get_hex"
      , "get_chunks"
      , "get_chunk_summary"
      , "get_terrain_stats"
      , "get_overlays"
      , "find_hexes"
      , "export_terrain_data"
      , "editor_toggle"
      , "editor_set_tool"
      , "editor_set_brush"
      , "editor_brush_stroke"
      , "editor_brush_line"
      , "editor_set_biome"
      , "editor_set_form"
      , "editor_set_hardness"
      , "editor_undo"
      , "editor_redo"
      , "editor_get_state"
      , "get_pipeline"
      , "set_stage_enabled"
      , "list_plugins"
      , "set_plugin_enabled"
      , "set_plugin_param"
      , "data_list_plugins"
      , "data_list_resources"
      , "data_list_records"
      , "data_get_record"
      , "data_create_record"
      , "data_update_record"
      , "data_delete_record"
      , "data_get_state"
      , "get_sim_state"
      , "set_sim_auto_tick"
      , "sim_tick"
      , "get_sim_dag"
      , "get_logs"
      , "take_screenshot"
      , "set_seed"
      , "set_view_mode"
      , "set_config_tab"
      , "select_hex"
      , "set_overlay"
      , "list_overlay_fields"
      , "cycle_overlay"
      , "cycle_overlay_field"
      , "set_camera"
      , "get_camera"
      , "zoom_to_chunk"
      , "set_left_panel"
      , "set_left_tab"
      , "toggle_config_panel"
      , "set_log_collapsed"
      , "set_log_level"
      , "get_ui_panels"
      , "viewport_scroll"
      , "viewport_click"
      , "viewport_drag"
      , "viewport_hover"
      , "click_widget"
      , "list_widgets"
      , "get_widget_state"
      , "get_dialog_state"
      , "set_dialog_text"
      , "dialog_confirm"
      , "dialog_cancel"
      , "send_key"
      ]

  it "keeps world, terrain, editor, and overlay contracts typed" $ do
    worldMetaChunkIds worldMetaContract `shouldBe` [ChunkId 0]
    map terrainFindFilterOp (terrainFindFilters terrainFindContract) `shouldBe` [TerrainOpEq]
    terrainFindLimit terrainFindContract `shouldBe` Just 25
    editorSetBrushStrength editorBrushContract `shouldBe` Just 0.4
    editorStrokeQueuedCount editorStrokeQueuedContract `shouldBe` 3
    editorActionQueuedStatus editorUndoQueuedContract `shouldBe` "queued"
    terrainExportedFields terrainExportContract `shouldBe` ["elevation", "moisture"]
    map uiOverlayFieldType (uiOverlayFields uiOverlayFieldsContract) `shouldBe` [OFFloat]

  it "keeps command-visible typed contract fields complete" $ do
    pluginSummaryStatus pluginSummaryContract `shouldBe` "connected"
    asyncStatusPhase (pluginSummaryAsyncStatus pluginSummaryContract) `shouldBe` AsyncStatusRunning
    pluginSummaryEnabled pluginSummaryContract `shouldBe` True
    dataResourceRecordCount dataResourceStateContract `shouldBe` 2
    dataResourceTotalCount dataResourceStateContract `shouldBe` 5
    dataResourcePageOffset dataResourceStateContract `shouldBe` 3
    dataResourceLoading dataResourceStateContract `shouldBe` False
    asyncStatusPhase (dataResourceAsyncStatus dataResourceStateContract) `shouldBe` AsyncStatusIdle
    dataResourceHasSelection dataResourceStateContract `shouldBe` True

  it "keeps logs, screenshots, async status, and event hooks typed" $ do
    logGetMinLevel logRequestContract `shouldBe` Just LogWarn
    logGetResponseTotal logResponseContract `shouldBe` 2
    map logEntrySummaryMessage (logGetResponseEntries logResponseContract) `shouldBe` ["queued"]
    screenshotTakeSavePath screenshotRequestContract `shouldBe` Just "capture.png"
    screenshotTakeFormat screenshotResponseContract `shouldBe` "png"
    worldGenerationInProgress worldGenerationStatusContract `shouldBe` True
    asyncStatusCurrent (worldGenerationAsyncStatus worldGenerationStatusContract) `shouldBe` Just 3
    simulationTickCount simulationStateContract `shouldBe` 9
    asyncStatusPhase (simulationAsyncStatus simulationStateContract) `shouldBe` AsyncStatusRunning
    uiViewportClickSelected uiViewportClickContract `shouldBe` Just True
    uiViewportClickHex uiViewportClickContract `shouldBe` Just (UiHexCoord 1 2)
    typedOperationMethod serviceEventPublishOperation `shouldBe` "publish_event"
    typedOperationMethod serviceEventPublishOperation `shouldNotSatisfy` (`elem` appServiceOperationMethods)
    serviceEventPublishAccepted eventPublishResponseContract `shouldBe` True

  it "keeps state, config, panel, and camera contracts typed" $ do
    stateSummaryViewMode stateSummaryContract `shouldBe` "elevation"
    stateSummaryContextHex stateSummaryContract `shouldBe` Just (StateHexCoord 1 2)
    map stateViewModeName (stateViewModes stateViewModesContract) `shouldBe` ["elevation", "biome"]
    configSliderSummaryId configSliderContract `shouldBe` "SliderWaterLevel"
    configSliderSummaryTab configSliderContract `shouldBe` "climate"
    configSliderSummaryValueKind configSliderContract `shouldBe` "float"
    map configSliderSummaryId (configSlidersResponseSliders configSlidersContract) `shouldBe` ["SliderWaterLevel"]
    configPresetNames configPresetsContract `shouldBe` ["default"]
    uiCameraSnapshotZoom uiCameraContract `shouldBe` 1.25
    uiPanelTabName (uiPanelsLeftPanel uiPanelsContract) `shouldBe` "topo"
    uiLogPanelLevel (uiPanelsLogPanel uiPanelsContract) `shouldBe` "info"

  it "keeps the simulation DAG contract typed and command-backed" $ do
    typedOperationMethod simulationDagOperation `shouldBe` "get_sim_dag"
    typedOperationMethod simulationDagOperation `shouldSatisfy` (`elem` appServiceOperationMethods)
    simulationDagTerrainWriters simulationDagContract `shouldBe` [SimNodeId "weather"]

typedOperationMethods :: [Text]
typedOperationMethods =
  [ typedOperationMethod stateGetStateOperation
  , typedOperationMethod stateGetViewModesOperation
  , typedOperationMethod stateGetUiStateOperation
  , typedOperationMethod configGetSlidersOperation
  , typedOperationMethod configGetSliderOperation
  , typedOperationMethod configSetSliderOperation
  , typedOperationMethod configSetSlidersOperation
  , typedOperationMethod configResetSlidersOperation
  , typedOperationMethod configGetSummaryOperation
  , typedOperationMethod configGetEnumsOperation
  , typedOperationMethod configListPresetsOperation
  , typedOperationMethod configSavePresetOperation
  , typedOperationMethod configLoadPresetOperation
  , typedOperationMethod worldGenerateOperation
  , typedOperationMethod worldGetMetaOperation
  , typedOperationMethod worldGetGenerationStatusOperation
  , typedOperationMethod worldListOperation
  , typedOperationMethod worldSaveOperation
  , typedOperationMethod worldLoadOperation
  , typedOperationMethod worldSetNameOperation
  , typedOperationMethod terrainGetHexOperation
  , typedOperationMethod terrainGetChunksOperation
  , typedOperationMethod terrainGetChunkSummaryOperation
  , typedOperationMethod terrainGetStatsOperation
  , typedOperationMethod terrainGetOverlaysOperation
  , typedOperationMethod terrainFindHexesOperation
  , typedOperationMethod terrainExportDataOperation
  , typedOperationMethod editorToggleOperation
  , typedOperationMethod editorSetToolOperation
  , typedOperationMethod editorSetBrushOperation
  , typedOperationMethod editorBrushStrokeOperation
  , typedOperationMethod editorBrushLineOperation
  , typedOperationMethod editorSetBiomeOperation
  , typedOperationMethod editorSetFormOperation
  , typedOperationMethod editorSetHardnessOperation
  , typedOperationMethod editorUndoOperation
  , typedOperationMethod editorRedoOperation
  , typedOperationMethod editorGetStateOperation
  , typedOperationMethod pipelineGetOperation
  , typedOperationMethod pipelineSetStageEnabledOperation
  , typedOperationMethod pluginListOperation
  , typedOperationMethod pluginSetEnabledOperation
  , typedOperationMethod pluginSetParamOperation
  , typedOperationMethod dataResourceListPluginsOperation
  , typedOperationMethod dataResourceListResourcesOperation
  , typedOperationMethod dataResourceListRecordsOperation
  , typedOperationMethod dataResourceGetRecordOperation
  , typedOperationMethod dataResourceCreateRecordOperation
  , typedOperationMethod dataResourceUpdateRecordOperation
  , typedOperationMethod dataResourceDeleteRecordOperation
  , typedOperationMethod dataResourceStateOperation
  , typedOperationMethod simulationStateOperation
  , typedOperationMethod simulationSetAutoTickOperation
  , typedOperationMethod simulationTickOperation
  , typedOperationMethod simulationDagOperation
  , typedOperationMethod logGetOperation
  , typedOperationMethod screenshotTakeOperation
  , typedOperationMethod uiSetSeedOperation
  , typedOperationMethod uiSetViewModeOperation
  , typedOperationMethod uiSetConfigTabOperation
  , typedOperationMethod uiSelectHexOperation
  , typedOperationMethod uiSetOverlayOperation
  , typedOperationMethod uiListOverlayFieldsOperation
  , typedOperationMethod uiCycleOverlayOperation
  , typedOperationMethod uiCycleOverlayFieldOperation
  , typedOperationMethod uiSetCameraOperation
  , typedOperationMethod uiGetCameraOperation
  , typedOperationMethod uiZoomToChunkOperation
  , typedOperationMethod uiSetLeftPanelOperation
  , typedOperationMethod uiSetLeftTabOperation
  , typedOperationMethod uiToggleConfigPanelOperation
  , typedOperationMethod uiSetLogCollapsedOperation
  , typedOperationMethod uiSetLogLevelOperation
  , typedOperationMethod uiGetPanelsOperation
  , typedOperationMethod uiViewportScrollOperation
  , typedOperationMethod uiViewportClickOperation
  , typedOperationMethod uiViewportDragOperation
  , typedOperationMethod uiViewportHoverOperation
  , typedOperationMethod uiClickWidgetOperation
  , typedOperationMethod uiListWidgetsOperation
  , typedOperationMethod uiGetWidgetStateOperation
  , typedOperationMethod uiGetDialogStateOperation
  , typedOperationMethod uiSetDialogTextOperation
  , typedOperationMethod uiDialogConfirmOperation
  , typedOperationMethod uiDialogCancelOperation
  , typedOperationMethod uiSendKeyOperation
  ]

typedOperationMethod :: TypedServiceOperation request response -> Text
typedOperationMethod = serviceOperationMethod . typedServiceOperationSpec

worldMetaContract :: WorldMetaResponse
worldMetaContract = WorldMetaResponse
  { worldMetaSeed = 42
  , worldMetaChunkSize = 64
  , worldMetaTilesPerChunk = 4096
  , worldMetaChunkCount = 1
  , worldMetaTotalTiles = 4096
  , worldMetaChunkIds = [ChunkId 0]
  , worldMetaOverlayNames = ["weather"]
  , worldMetaName = "demo"
  , worldMetaGenerating = False
  }

terrainFindContract :: TerrainFindHexesRequest
terrainFindContract = TerrainFindHexesRequest
  { terrainFindFilters =
      [ TerrainFindFilter
          { terrainFindFilterField = "biome"
          , terrainFindFilterOp = TerrainOpEq
          , terrainFindFilterValue = String "forest"
          }
      ]
  , terrainFindLimit = Just 25
  }

editorBrushContract :: EditorSetBrushRequest
editorBrushContract = EditorSetBrushRequest
  { editorSetBrushRadius = Just 3
  , editorSetBrushStrength = Just 0.4
  , editorSetBrushFalloff = Nothing
  , editorSetBrushSmoothPasses = Just 2
  , editorSetBrushNoiseFrequency = Nothing
  , editorSetBrushErodePasses = Just 4
  }

editorStrokeQueuedContract :: EditorStrokeQueuedResponse
editorStrokeQueuedContract = EditorStrokeQueuedResponse
  { editorStrokeQueuedStatus = "queued"
  , editorStrokeQueuedCount = 3
  }

editorUndoQueuedContract :: EditorActionQueuedResponse
editorUndoQueuedContract = EditorActionQueuedResponse
  { editorActionQueuedStatus = "queued"
  }

terrainExportContract :: TerrainExportResponse
terrainExportContract = TerrainExportResponse
  { terrainExportChunkCount = 0
  , terrainExportedFields = ["elevation", "moisture"]
  , terrainExportChunkData = Map.empty
  }

uiOverlayFieldsContract :: UiListOverlayFieldsResponse
uiOverlayFieldsContract = UiListOverlayFieldsResponse
  { uiOverlayFieldCount = 1
  , uiOverlayFields =
      [ UiOverlayFieldSummary
          { uiOverlayFieldIndex = 0
          , uiOverlayFieldName = "rainfall"
          , uiOverlayFieldType = OFFloat
          }
      ]
  }

logRequestContract :: LogGetRequest
logRequestContract = LogGetRequest
  { logGetMinLevel = Just LogWarn
  , logGetLimit = Just 10
  , logGetOffset = Just 1
  }

logResponseContract :: LogGetResponse
logResponseContract = LogGetResponse
  { logGetResponseCount = 1
  , logGetResponseTotal = 2
  , logGetResponseEntries =
      [ LogEntrySummary
          { logEntrySummaryLevel = LogWarn
          , logEntrySummaryMessage = "queued"
          }
      ]
  }

screenshotRequestContract :: ScreenshotTakeRequest
screenshotRequestContract = ScreenshotTakeRequest
  { screenshotTakeSavePath = Just "capture.png"
  }

screenshotResponseContract :: ScreenshotTakeResponse
screenshotResponseContract = ScreenshotTakeResponse
  { screenshotTakeImageBase64 = "iVBORw0KGgo="
  , screenshotTakeFormat = "png"
  , screenshotTakeSavedPath = Just "capture.png"
  , screenshotTakeSource = Just "headless"
  }

worldGenerationStatusContract :: WorldGenerationStatusResponse
worldGenerationStatusContract = WorldGenerationStatusResponse
  { worldGenerationInProgress = True
  , worldGenerationChunkCount = 3
  , worldGenerationSeed = 42
  , worldGenerationAsyncStatus = AsyncStatusSnapshot
      { asyncStatusName = "world.generation"
      , asyncStatusPhase = AsyncStatusRunning
      , asyncStatusActive = True
      , asyncStatusCurrent = Just 3
      , asyncStatusTotal = Just 8
      , asyncStatusMessage = Just "generating"
      }
  }

simulationStateContract :: SimulationStateResponse
simulationStateContract = SimulationStateResponse
  { simulationAutoTick = True
  , simulationTickRate = 1.0
  , simulationTickCount = 9
  , simulationAsyncStatus = AsyncStatusSnapshot
      { asyncStatusName = "simulation.tick"
      , asyncStatusPhase = AsyncStatusRunning
      , asyncStatusActive = True
      , asyncStatusCurrent = Just 9
      , asyncStatusTotal = Nothing
      , asyncStatusMessage = Just "auto tick enabled"
      }
  }

uiViewportClickContract :: UiViewportClickResponse
uiViewportClickContract = UiViewportClickResponse
  { uiViewportClickButtonName = "left"
  , uiViewportClickHex = Just (UiHexCoord 1 2)
  , uiViewportClickSelected = Just True
  , uiViewportClickEditorStroke = Just False
  , uiViewportClickTooltipPinned = Just True
  , uiViewportClickReason = Nothing
  }

eventPublishRequestContract :: ServiceEventPublishRequest
eventPublishRequestContract = ServiceEventPublishRequest
  { serviceEventPublishEnvelope = ServiceEventEnvelope
      { serviceEventTopic = "world.generated"
      , serviceEventSource = ServiceEventFromService
      , serviceEventSeverity = ServiceEventInfo
      , serviceEventSequence = Just 1
      , serviceEventCorrelationId = Just "generation-1"
      , serviceEventPayload = String "ready"
      }
  }

eventPublishResponseContract :: ServiceEventPublishResponse
eventPublishResponseContract = ServiceEventPublishResponse
  { serviceEventPublishAccepted = True
  , serviceEventPublishTopic = serviceEventTopic (serviceEventPublishEnvelope eventPublishRequestContract)
  }

stateSummaryContract :: StateSummaryResponse
stateSummaryContract = StateSummaryResponse
  { stateSummarySeed = 42
  , stateSummaryViewMode = "elevation"
  , stateSummaryConfigTab = "terrain"
  , stateSummaryGenerating = False
  , stateSummaryChunkSize = 64
  , stateSummaryShowConfig = True
  , stateSummaryWorldName = "demo"
  , stateSummaryContextHex = Just (StateHexCoord 1 2)
  }

stateViewModesContract :: StateViewModesResponse
stateViewModesContract = StateViewModesResponse
  { stateViewModes =
      [ StateViewModeSummary
          { stateViewModeName = "elevation"
          , stateViewModeActive = True
          }
      , StateViewModeSummary
          { stateViewModeName = "biome"
          , stateViewModeActive = False
          }
      ]
  }

configSliderContract :: ConfigSliderSummary
configSliderContract = ConfigSliderSummary
  { configSliderSummaryId = "SliderWaterLevel"
  , configSliderSummaryTab = "climate"
  , configSliderSummaryValue = 0.43
  , configSliderSummaryDomainValue = 0.43
  , configSliderSummaryDomainMin = 0.0
  , configSliderSummaryDomainMax = 1.0
  , configSliderSummaryValueKind = "float"
  , configSliderSummaryDefault = 0.43
  , configSliderSummaryDefaultDomain = 0.43
  }

configSlidersContract :: ConfigSlidersResponse
configSlidersContract = ConfigSlidersResponse
  { configSlidersResponseSliders = [configSliderContract]
  }

configPresetsContract :: ConfigListPresetsResponse
configPresetsContract = ConfigListPresetsResponse
  { configPresetCount = 1
  , configPresetNames = ["default"]
  }

uiCameraContract :: UiCameraSnapshot
uiCameraContract = UiCameraSnapshot
  { uiCameraSnapshotX = 10.0
  , uiCameraSnapshotY = 20.0
  , uiCameraSnapshotZoom = 1.25
  }

uiPanelsContract :: UiPanelsResponse
uiPanelsContract = UiPanelsResponse
  { uiPanelsLeftPanel = UiPanelTabState
      { uiPanelTabVisible = True
      , uiPanelTabName = "topo"
      }
  , uiPanelsConfigPanel = UiPanelTabState
      { uiPanelTabVisible = False
      , uiPanelTabName = "terrain"
      }
  , uiPanelsLogPanel = UiLogPanelState
      { uiLogPanelCollapsed = False
      , uiLogPanelLevel = "info"
      }
  }

pluginSummaryContract :: PluginSummary
pluginSummaryContract = PluginSummary
  { pluginSummaryName = "weather"
  , pluginSummaryStatus = "connected"
  , pluginSummaryAsyncStatus = AsyncStatusSnapshot
      { asyncStatusName = "plugin.weather"
      , asyncStatusPhase = AsyncStatusRunning
      , asyncStatusActive = True
      , asyncStatusCurrent = Nothing
      , asyncStatusTotal = Nothing
      , asyncStatusMessage = Just "connected"
      }
  , pluginSummaryVersion = "1.0.0"
  , pluginSummaryDescription = "Weather plugin"
  , pluginSummaryEnabled = True
  , pluginSummaryParams = Map.empty
  , pluginSummaryParamSpecs = []
  , pluginSummaryDataResources = []
  , pluginSummaryHasGenerator = False
  , pluginSummaryHasSimulation = True
  }

dataResourceStateContract :: DataResourceStateResponse
dataResourceStateContract = DataResourceStateResponse
  { dataResourceSelectedPlugin = Just "weather"
  , dataResourceSelectedResource = Just "stations"
  , dataResourceRecordCount = 2
  , dataResourceTotalCount = 5
  , dataResourcePageOffset = 3
  , dataResourceLoading = False
  , dataResourceAsyncStatus = AsyncStatusSnapshot
      { asyncStatusName = "data.weather.stations"
      , asyncStatusPhase = AsyncStatusIdle
      , asyncStatusActive = False
      , asyncStatusCurrent = Just 2
      , asyncStatusTotal = Just 5
      , asyncStatusMessage = Nothing
      }
  , dataResourceSelectedRecordKey = Just "station-1"
  , dataResourceEditMode = True
  , dataResourceCreateMode = False
  , dataResourceHasSelection = True
  }

simulationDagContract :: SimulationDagResponse
simulationDagContract = SimulationDagResponse
  { simulationDagNodes =
      [ SimulationDagNodeSummary
          { simulationDagNodeId = SimNodeId "weather"
          , simulationDagNodeOverlay = "rainfall"
          , simulationDagNodeDependencies = [SimNodeId "temperature"]
          , simulationDagNodeWritesTerrain = True
          }
      ]
  , simulationDagLevels = [[SimNodeId "temperature"], [SimNodeId "weather"]]
  , simulationDagTerrainWriters = [SimNodeId "weather"]
  }

expectedCommandMethods :: [Text]
expectedCommandMethods = concatMap snd expectedServiceGroups

expectedServiceGroups :: [(Text, [Text])]
expectedServiceGroups =
  [ ( "state"
    , [ "get_state"
      , "get_view_modes"
      , "get_ui_state"
      ]
    )
  , ( "config"
    , [ "get_sliders"
      , "get_slider"
      , "set_slider"
      , "set_sliders"
      , "reset_sliders"
      , "get_config_summary"
      , "get_enums"
      , "list_presets"
      , "save_preset"
      , "load_preset"
      ]
    )
  , ( "world"
    , [ "generate"
      , "get_world_meta"
      , "get_generation_status"
      , "list_worlds"
      , "save_world"
      , "load_world"
      , "set_world_name"
      ]
    )
  , ( "terrain"
    , [ "get_hex"
      , "get_chunks"
      , "get_chunk_summary"
      , "get_terrain_stats"
      , "get_overlays"
      , "find_hexes"
      , "export_terrain_data"
      ]
    )
  , ( "editor"
    , [ "editor_toggle"
      , "editor_set_tool"
      , "editor_set_brush"
      , "editor_brush_stroke"
      , "editor_brush_line"
      , "editor_set_biome"
      , "editor_set_form"
      , "editor_set_hardness"
      , "editor_undo"
      , "editor_redo"
      , "editor_get_state"
      ]
    )
  , ( "pipeline"
    , [ "get_pipeline"
      , "set_stage_enabled"
      ]
    )
  , ( "plugins"
    , [ "list_plugins"
      , "set_plugin_enabled"
      , "set_plugin_param"
      ]
    )
  , ( "data-resources"
    , [ "data_list_plugins"
      , "data_list_resources"
      , "data_list_records"
      , "data_get_record"
      , "data_create_record"
      , "data_update_record"
      , "data_delete_record"
      , "data_get_state"
      ]
    )
  , ( "simulation"
    , [ "get_sim_state"
      , "set_sim_auto_tick"
      , "sim_tick"
      , "get_sim_dag"
      ]
    )
  , ( "logs"
    , [ "get_logs"
      ]
    )
  , ( "screenshots"
    , [ "take_screenshot"
      ]
    )
  , ( "ui"
    , [ "set_seed"
      , "set_view_mode"
      , "set_config_tab"
      , "select_hex"
      , "set_overlay"
      , "list_overlay_fields"
      , "cycle_overlay"
      , "cycle_overlay_field"
      , "set_camera"
      , "get_camera"
      , "zoom_to_chunk"
      , "set_left_panel"
      , "set_left_tab"
      , "toggle_config_panel"
      , "set_log_collapsed"
      , "set_log_level"
      , "get_ui_panels"
      , "viewport_scroll"
      , "viewport_click"
      , "viewport_drag"
      , "viewport_hover"
      , "click_widget"
      , "list_widgets"
      , "get_widget_state"
      , "get_dialog_state"
      , "set_dialog_text"
      , "dialog_confirm"
      , "dialog_cancel"
      , "send_key"
      ]
    )
  ]
