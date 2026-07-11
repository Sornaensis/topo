{-# LANGUAGE OverloadedStrings #-}

module Spec.AppService (spec) where

import Actor.Log (LogLevel(..))
import Actor.PluginManager
  ( ExternalDataSourceGrantBrokerPhase(..)
  , LoadedPlugin(..)
  , PluginDiagnosticState(..)
  , PluginExternalDataSourceGrantDiagnostic(..)
  , PluginExternalDataSourceDiagnostic(..)
  , PluginLifecycleSnapshot(..)
  , PluginLifecycleState(..)
  , PluginStatus(..)
  , externalDataSourceGrantBrokerPhaseText
  , pluginAvailableDependencyKeys
  , pluginDiagnosticState
  , pluginExternalDataSourceDiagnostics
  , pluginExternalDataSourceDiagnosticsFor
  , pluginLifecycleSnapshot
  , pluginPanelDiagnosticLines
  )
import Data.Aeson (Value(..), object, (.=))
import Data.List (nub, sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime)
import Test.Hspec
import Topo.Overlay.Schema (OverlayFieldType(..))
import Topo.Plugin (Capability(..))
import Topo.Plugin.DataResource (DataFieldDef(..), DataFieldType(..), DataOperations(..), DataResourceSchema(..), currentDataResourceSchemaVersion, defaultDataResourceVersion, defaultDataPagination, noOperations)
import Topo.Plugin.RPC.DataService (DataResourceErrorCode(..))
import Topo.Plugin.RPC.Manifest
  ( RPCExternalDataSourceAccess(..)
  , RPCExternalDataSourceAccessMode(..)
  , RPCExternalDataSourceAvailability(..)
  , RPCExternalDataSourceCapability(..)
  , RPCExternalDataSourceConfigOrigin(..)
  , RPCExternalDataSourceConfigRef(..)
  , RPCExternalDataSourceDecl(..)
  , RPCExternalDataSourceGrant(..)
  , RPCExternalDataSourceHealth(..)
  , RPCExternalDataSourceRef(..)
  , RPCExternalDataSourceStatus(..)
  , RPCExternalDataSourceStatusState(..)
  , RPCGeneratorDecl(..)
  , RPCManifest(..)
  , defaultRPCExternalDataSourceStatus
  , defaultRPCManifestRuntime
  , defaultRPCStartPolicy
  , defaultRPCUIHints
  , manifestV3
  )
import Topo.Simulation (SimNodeId(..))
import Topo.Types (ChunkId(..))

import Seer.Command.AppServiceAdapter (appServiceCommandMethods, commandAppService)
import Seer.Command.Dispatch (dispatchCommandMethods)
import Seer.Service.AppService
  ( AppServiceOperation(..)
  , appServiceOperationSpec
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
  , StateLayeredViewSnapshot(..)
  , StateSummaryResponse(..)
  , StateViewModeSummary(..)
  , StateViewModesResponse(..)
  , StateViewChoice(..)
  , StateViewsResponse(..)
  , StateWeatherBasisSummary(..)
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
  , UiSetViewModeRequest(..)
  , UiSetViewModeResponse(..)
  , UiSetViewRequest(..)
  , UiSetViewResponse(..)
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
  , stateGetViewsOperation
  , terrainExportDataOperation
  , terrainExportMeshOperation
  , terrainExportSampleOperation
  , overlayExportDataOperation
  , overlayGetProvenanceOperation
  , overlayGetSchemaOperation
  , overlayValidateImportOperation
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
  , uiSetViewOperation
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
  , ServiceError(..)
  , ServiceOperationSpec(..)
  , TypedServiceOperation(..)
  , groupOperationMethods
  , serviceErrorCode
  , serviceErrorHTTPStatus
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

  it "maps standardized data-resource service errors to codes and HTTP statuses" $ do
    let cases =
          [ (ResourceNotFound, "resource_not_found", 404)
          , (OperationNotSupported, "operation_not_supported", 405)
          , (RecordNotFound, "record_not_found", 404)
          , (DuplicateKey, "duplicate_key", 409)
          , (SchemaValidationFailed, "schema_validation_failed", 422)
          , (PermissionDenied, "permission_denied", 403)
          , (Conflict, "conflict", 409)
          , (PluginUnavailable, "plugin_unavailable", 503)
          , (ExternalDataSourceUnavailable, "external_data_source_unavailable", 503)
          , (QueryUnsupported, "query_unsupported", 400)
          , (DataResourceTimeout, "timeout", 504)
          ]
    map (\(code, _, _) -> serviceErrorCode (ServiceDataResourceError code "failed" [])) cases
      `shouldBe` map (\(_, textCode, _) -> textCode) cases
    map (\(code, _, _) -> serviceErrorHTTPStatus (ServiceDataResourceError code "failed" [])) cases
      `shouldBe` map (\(_, _, status) -> status) cases

  it "defines typed operation contracts for M2 service groups" $
    typedOperationMethods `shouldBe`
      [ "get_state"
      , "get_view_modes"
      , "get_views"
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
      , "get_overlay_schema"
      , "get_overlay_provenance"
      , "export_overlay_data"
      , "validate_overlay_import"
      , "export_mesh_data"
      , "export_sample_data"
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
      , "set_view"
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
    pluginSummaryDiagnosticStatus pluginSummaryContract `shouldBe` "Ready"
    pluginSummaryEndpointKind pluginSummaryContract `shouldBe` Just "unix"
    pluginSummaryRestartCount pluginSummaryContract `shouldBe` 0
    plsState (pluginSummaryLifecycle pluginSummaryContract) `shouldBe` LifecycleReady
    asyncStatusPhase (pluginSummaryAsyncStatus pluginSummaryContract) `shouldBe` AsyncStatusRunning
    pluginSummaryEnabled pluginSummaryContract `shouldBe` True
    dataResourceRecordCount dataResourceStateContract `shouldBe` 2
    dataResourceTotalCount dataResourceStateContract `shouldBe` 5
    dataResourcePageOffset dataResourceStateContract `shouldBe` 3
    dataResourceLoading dataResourceStateContract `shouldBe` False
    asyncStatusPhase (dataResourceAsyncStatus dataResourceStateContract) `shouldBe` AsyncStatusIdle
    dataResourceHasSelection dataResourceStateContract `shouldBe` True
    dataResourceExternalDataSourceCount dataResourceStateContract `shouldBe` 0
    dataResourceExternalDataSourceFailures dataResourceStateContract `shouldBe` 0

  it "classifies plugin diagnostics and preserves backend-neutral data-source ownership" $ do
    let availableDeps = pluginAvailableDependencyKeys Set.empty [diagnosticReadyPlugin]
        sources = pluginExternalDataSourceDiagnostics diagnosticReadyPlugin
    pluginDiagnosticState Set.empty availableDeps diagnosticReadyPlugin `shouldBe` DiagnosticReady
    pluginDiagnosticState Set.empty Set.empty diagnosticWaitingPlugin `shouldBe` DiagnosticWaitingForDependencies
    pluginDiagnosticState (Set.singleton "provider-x")
      (pluginAvailableDependencyKeys (Set.singleton "provider-x") [diagnosticProviderPlugin, diagnosticConsumerPlugin])
      diagnosticConsumerPlugin `shouldBe` DiagnosticWaitingForDependencies
    map pedsRole sources `shouldBe` ["provider", "consumer"]
    map pedsOwnership sources `shouldBe` ["provider-owned", "provider-owned"]
    map pedsHostRole sources `shouldBe` ["broker", "consumer-router"]
    map pedsLifecycleBoundary sources `shouldBe` ["external-provider-managed", "external-provider-managed"]
    map pedsProvider sources `shouldBe` ["weather", "weather"]
    map pedsConsumer sources `shouldBe` ["brokered-by-topo", "weather"]
    map pedsResources sources `shouldBe` [["stations"], ["stations"]]
    map pedsDataReadGrant sources `shouldBe` [True, True]
    map pedsDataWriteGrant sources `shouldBe` [False, False]
    map pedsFailureReason sources `shouldBe` [Nothing, Just "provider grant not brokered"]
    case sources of
      providerDiag:_ -> do
        map pedsgName (pedsGrants providerDiag) `shouldBe` ["stations-read"]
        map pedsgFailureReason (pedsGrants providerDiag) `shouldBe` [Just "grant pending provider health check"]
      _ -> expectationFailure "expected provider external data-source diagnostic"
    let failedPlugin = diagnosticReadyPlugin
          { lpStatus = PluginError "runtime failed"
          , lpLifecycle = pluginLifecycleSnapshot appServiceTestTime LifecycleFailed
              (Just "runtime failed") (Just "runtime_failed") (Just "runtime failed") Nothing Nothing Nothing []
          }
        failedSources = pluginExternalDataSourceDiagnostics failedPlugin
    map pedsAvailability failedSources `shouldBe` ["unavailable", "unavailable"]
    map pedsFailureReason failedSources `shouldBe` [Just "plugin lifecycle is failed: runtime failed", Just "plugin lifecycle is failed: runtime failed"]
    let disabledSources = pluginExternalDataSourceDiagnosticsFor (Set.singleton "weather") [diagnosticReadyPlugin] diagnosticReadyPlugin
    map pedsAvailability disabledSources `shouldBe` ["unavailable", "unavailable"]
    map pedsFailureReason disabledSources `shouldBe` [Just "plugin is disabled", Just "plugin is disabled"]
    let bareStageOnlyPanel = pluginPanelDiagnosticLines Set.empty (Set.singleton "weather") diagnosticReadyPlugin
    bareStageOnlyPanel `shouldSatisfy` any (Text.isInfixOf "provider plugin is unavailable")

  it "uses typed external data-source broker phase labels" $ do
    map externalDataSourceGrantBrokerPhaseText
      [ ExternalDataSourceGrantPending
      , ExternalDataSourceGrantAcked
      , ExternalDataSourceGrantFailed
      , ExternalDataSourceRevokePending
      , ExternalDataSourceRevokeAcked
      , ExternalDataSourceRevokeFailed
      , ExternalDataSourceGrantUnavailable
      ] `shouldBe`
        [ "grant_pending"
        , "grant_acked"
        , "grant_failed"
        , "revoke_pending"
        , "revoke_acked"
        , "revoke_failed"
        , "unavailable"
        ]

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
    stateLayeredBaseMode (stateSummaryView stateSummaryContract) `shouldBe` "elevation"
    stateSummaryContextHex stateSummaryContract `shouldBe` Just (StateHexCoord 1 2)
    map stateViewModeName (stateViewModes stateViewModesContract) `shouldBe` ["elevation", "biome"]
    stateLayeredOverlayMode (stateViewsCurrent stateViewsContract) `shouldBe` Just "cloud"
    map stateViewChoiceName (stateViewsBaseModes stateViewsContract) `shouldBe` ["elevation"]
    configSliderSummaryId configSliderContract `shouldBe` "SliderWaterLevel"
    configSliderSummaryTab configSliderContract `shouldBe` "climate"
    configSliderSummaryValueKind configSliderContract `shouldBe` "float"
    map configSliderSummaryId (configSlidersResponseSliders configSlidersContract) `shouldBe` ["SliderWaterLevel"]
    configPresetNames configPresetsContract `shouldBe` ["default"]
    uiCameraSnapshotZoom uiCameraContract `shouldBe` 1.25
    uiPanelTabName (uiPanelsLeftPanel uiPanelsContract) `shouldBe` "topo"
    uiLogPanelLevel (uiPanelsLogPanel uiPanelsContract) `shouldBe` "info"

  it "keeps layered view command contracts typed" $ do
    uiSetViewModeRequestName uiSetViewModeRequestContract `shouldBe` "cloud"
    uiSetViewModeRequestBasis uiSetViewModeRequestContract `shouldBe` Just "typical_normal"
    uiSetViewModeRequestFieldIndex uiSetViewModeRequestContract `shouldBe` Nothing
    uiSetViewModeResponseName uiSetViewModeResponseContract `shouldBe` "cloud_typical"
    uiSetViewModeResponseView uiSetViewModeResponseContract `shouldBe` layeredViewValueContract
    uiSetViewRequestBaseMode uiSetViewRequestContract `shouldBe` Just "biome"
    uiSetViewRequestOverlayMode uiSetViewRequestContract `shouldBe` Just "cloud"
    uiSetViewRequestPluginOverlay uiSetViewRequestContract `shouldBe` Nothing
    uiSetViewRequestWeatherBasis uiSetViewRequestContract `shouldBe` Just "average"
    uiSetViewRequestOverlayOpacity uiSetViewRequestContract `shouldBe` Just 0.5
    uiSetViewRequestFieldIndex uiSetViewRequestContract `shouldBe` Nothing
    uiSetViewResponseViewMode uiSetViewResponseContract `shouldBe` Just "cloud_typical"
    uiSetViewResponseView uiSetViewResponseContract `shouldBe` layeredViewValueContract

  it "keeps the simulation DAG contract typed and command-backed" $ do
    typedOperationMethod simulationDagOperation `shouldBe` "get_sim_dag"
    typedOperationMethod simulationDagOperation `shouldSatisfy` (`elem` appServiceOperationMethods)
    simulationDagTerrainWriters simulationDagContract `shouldBe` [SimNodeId "weather"]

typedOperationMethods :: [Text]
typedOperationMethods =
  [ typedOperationMethod stateGetStateOperation
  , typedOperationMethod stateGetViewModesOperation
  , typedOperationMethod stateGetViewsOperation
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
  , typedOperationMethod overlayGetSchemaOperation
  , typedOperationMethod overlayGetProvenanceOperation
  , typedOperationMethod overlayExportDataOperation
  , typedOperationMethod overlayValidateImportOperation
  , typedOperationMethod terrainExportMeshOperation
  , typedOperationMethod terrainExportSampleOperation
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
  , typedOperationMethod uiSetViewOperation
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
  , stateSummaryView = layeredViewContract
  , stateSummaryConfigTab = "terrain"
  , stateSummaryGenerating = False
  , stateSummaryChunkSize = 64
  , stateSummaryShowConfig = True
  , stateSummaryWorldName = "demo"
  , stateSummaryContextHex = Just (StateHexCoord 1 2)
  }

layeredViewContract :: StateLayeredViewSnapshot
layeredViewContract = StateLayeredViewSnapshot
  { stateLayeredBaseMode = "elevation"
  , stateLayeredOverlayMode = Just "cloud"
  , stateLayeredPluginOverlay = Nothing
  , stateLayeredOverlayField = Nothing
  , stateLayeredWeatherBasis = "current"
  , stateLayeredOverlayOpacity = 0.75
  , stateLayeredLegacyViewMode = Just "cloud"
  , stateLayeredTemporalBasis = Just "instantaneous_current"
  , stateLayeredSourceKind = Just "weather_snapshot"
  }

stateViewModesContract :: StateViewModesResponse
stateViewModesContract = StateViewModesResponse
  { stateViewModes =
      [ StateViewModeSummary
          { stateViewModeName = "elevation"
          , stateViewModeActive = True
          , stateViewModeLabel = "Elevation"
          , stateViewModeDescription = "Hypsometric elevation relative to the configured water level."
          , stateViewModeKind = "scalar"
          , stateViewModeTemporalBasis = Nothing
          , stateViewModeSourceKind = Nothing
          , stateViewModeUnit = Just "m"
          , stateViewModeColorScale = "elevation-blue-green"
          , stateViewModeLegend = Null
          , stateViewModeTooltipFields = ["elevation_m"]
          , stateViewModeInspectorFields = ["hypsometry.elevation_m"]
          , stateViewModeExportFields = ["elevation"]
          , stateViewModeHttp = ["GET /state/view-modes"]
          }
      , StateViewModeSummary
          { stateViewModeName = "biome"
          , stateViewModeActive = False
          , stateViewModeLabel = "Biome"
          , stateViewModeDescription = "Biome classification."
          , stateViewModeKind = "categorical"
          , stateViewModeTemporalBasis = Nothing
          , stateViewModeSourceKind = Nothing
          , stateViewModeUnit = Nothing
          , stateViewModeColorScale = "biome-palette"
          , stateViewModeLegend = Null
          , stateViewModeTooltipFields = ["biome"]
          , stateViewModeInspectorFields = ["terrain.biome"]
          , stateViewModeExportFields = ["biome", "biome_code"]
          , stateViewModeHttp = ["GET /state/view-modes"]
          }
      ]
  }

stateViewsContract :: StateViewsResponse
stateViewsContract = StateViewsResponse
  { stateViewsCurrent = layeredViewContract
  , stateViewsBaseModes =
      [ StateViewChoice
          { stateViewChoiceName = "elevation"
          , stateViewChoiceActive = True
          , stateViewChoiceLabel = "Elevation"
          , stateViewChoiceLegacyViewMode = Just "elevation"
          , stateViewChoicePluginOverlay = Nothing
          , stateViewChoiceFieldIndex = Nothing
          , stateViewChoiceMetadata = Null
          }
      ]
  , stateViewsOverlayModes =
      [ StateViewChoice
          { stateViewChoiceName = "cloud"
          , stateViewChoiceActive = True
          , stateViewChoiceLabel = "Cloud / Storm"
          , stateViewChoiceLegacyViewMode = Just "cloud"
          , stateViewChoicePluginOverlay = Nothing
          , stateViewChoiceFieldIndex = Nothing
          , stateViewChoiceMetadata = Null
          }
      ]
  , stateViewsWeatherBases =
      [ StateWeatherBasisSummary
          { stateWeatherBasisName = "current"
          , stateWeatherBasisActive = True
          , stateWeatherBasisTemporalBasis = Just "instantaneous_current"
          , stateWeatherBasisSourceKind = Just "weather_snapshot"
          }
      ]
  , stateViewsOverlayNames = ["roads"]
  , stateViewsLegacyModes = stateViewModes stateViewModesContract
  }

layeredViewValueContract :: Value
layeredViewValueContract = object
  [ "base_mode" .= ("biome" :: Text)
  , "overlay_mode" .= ("cloud" :: Text)
  , "weather_basis" .= ("average" :: Text)
  , "overlay_opacity" .= (0.5 :: Double)
  , "legacy_view_mode" .= ("cloud_typical" :: Text)
  ]

uiSetViewModeRequestContract :: UiSetViewModeRequest
uiSetViewModeRequestContract = UiSetViewModeRequest
  { uiSetViewModeRequestName = "cloud"
  , uiSetViewModeRequestBasis = Just "typical_normal"
  , uiSetViewModeRequestFieldIndex = Nothing
  }

uiSetViewModeResponseContract :: UiSetViewModeResponse
uiSetViewModeResponseContract = UiSetViewModeResponse
  { uiSetViewModeResponseName = "cloud_typical"
  , uiSetViewModeResponseView = layeredViewValueContract
  }

uiSetViewRequestContract :: UiSetViewRequest
uiSetViewRequestContract = UiSetViewRequest
  { uiSetViewRequestBaseMode = Just "biome"
  , uiSetViewRequestOverlayMode = Just "cloud"
  , uiSetViewRequestPluginOverlay = Nothing
  , uiSetViewRequestWeatherBasis = Just "average"
  , uiSetViewRequestOverlayOpacity = Just 0.5
  , uiSetViewRequestFieldIndex = Nothing
  }

uiSetViewResponseContract :: UiSetViewResponse
uiSetViewResponseContract = UiSetViewResponse
  { uiSetViewResponseViewMode = Just "cloud_typical"
  , uiSetViewResponseView = layeredViewValueContract
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
  , pluginSummaryDiagnosticStatus = "Ready"
  , pluginSummaryStatusDetail = "Ready; RPC connection is active."
  , pluginSummaryLifecycle = pluginLifecycleSnapshot appServiceTestTime LifecycleReady
      (Just "connected") Nothing Nothing Nothing (Just "1234") (Just 1) []
  , pluginSummaryAsyncStatus = AsyncStatusSnapshot
      { asyncStatusName = "plugin.weather"
      , asyncStatusPhase = AsyncStatusRunning
      , asyncStatusActive = True
      , asyncStatusCurrent = Nothing
      , asyncStatusTotal = Nothing
      , asyncStatusMessage = Just "connected"
      }
  , pluginSummaryPid = Just "1234"
  , pluginSummaryEndpointKind = Just "unix"
  , pluginSummaryProtocolVersion = Just 1
  , pluginSummaryUptimeSeconds = Just 0
  , pluginSummaryLastError = Nothing
  , pluginSummaryRestartCount = 0
  , pluginSummaryDependencies = []
  , pluginSummaryResources = []
  , pluginSummaryExternalDataSources = []
  , pluginSummaryCapabilities = []
  , pluginSummaryVersion = "1.0.0"
  , pluginSummaryDescription = "Weather plugin"
  , pluginSummaryEnabled = True
  , pluginSummaryParams = Map.empty
  , pluginSummaryParamSpecs = []
  , pluginSummaryDataResources = []
  , pluginSummaryResourceCount = 0
  , pluginSummaryExternalDataSourceCount = 0
  , pluginSummaryExternalDataSourceFailures = 0
  , pluginSummaryHasGenerator = False
  , pluginSummaryHasSimulation = True
  , pluginSummaryLogs = []
  }

diagnosticReadyPlugin :: LoadedPlugin
diagnosticReadyPlugin = diagnosticPlugin diagnosticReadyManifest

diagnosticWaitingPlugin :: LoadedPlugin
diagnosticWaitingPlugin = diagnosticPlugin diagnosticWaitingManifest

diagnosticProviderPlugin :: LoadedPlugin
diagnosticProviderPlugin = diagnosticPlugin diagnosticProviderManifest

diagnosticConsumerPlugin :: LoadedPlugin
diagnosticConsumerPlugin = diagnosticPlugin diagnosticConsumerManifest

diagnosticPlugin :: RPCManifest -> LoadedPlugin
diagnosticPlugin manifest = LoadedPlugin
  { lpName = rmName manifest
  , lpManifest = manifest
  , lpParams = Map.empty
  , lpStatus = PluginConnected
  , lpLifecycle = pluginLifecycleSnapshot appServiceTestTime LifecycleReady
      (Just "handshake complete") Nothing Nothing Nothing (Just "1234") (Just 1) (map drsName (rmDataResources manifest))
  , lpConnection = Nothing
  , lpProcessHandle = Nothing
  , lpStartPolicy = defaultRPCStartPolicy
  , lpRestartHistory = []
  , lpDirectory = ""
  , lpOverlaySchema = Nothing
  }

diagnosticReadyManifest :: RPCManifest
diagnosticReadyManifest = diagnosticBaseManifest
  { rmGenerator = Just (RPCGeneratorDecl "biomes" ["climate"])
  , rmCapabilities = [CapDataRead, CapDataWrite]
  , rmDataResources = [diagnosticResource]
  , rmExternalDataSources = [diagnosticExternalSource]
  , rmExternalDataSourceRefs = [diagnosticExternalRef]
  }

diagnosticWaitingManifest :: RPCManifest
diagnosticWaitingManifest = diagnosticBaseManifest
  { rmGenerator = Just (RPCGeneratorDecl "missing-stage" ["missing-stage"])
  }

diagnosticProviderManifest :: RPCManifest
diagnosticProviderManifest = diagnosticReadyManifest
  { rmName = "provider-x"
  , rmDataResources = []
  }

diagnosticConsumerManifest :: RPCManifest
diagnosticConsumerManifest = diagnosticBaseManifest
  { rmName = "consumer"
  , rmGenerator = Just (RPCGeneratorDecl "provider-x" ["provider-x"])
  }

diagnosticBaseManifest :: RPCManifest
diagnosticBaseManifest = RPCManifest
  { rmManifestVersion = manifestV3
  , rmName = "weather"
  , rmVersion = "1.0.0"
  , rmRuntime = defaultRPCManifestRuntime
  , rmDescription = "Weather plugin"
  , rmUiHints = defaultRPCUIHints
  , rmGenerator = Nothing
  , rmSimulation = Nothing
  , rmOverlay = Nothing
  , rmCapabilities = []
  , rmParameters = []
  , rmDataResources = []
  , rmDataDirectory = Nothing
  , rmExternalDataSources = []
  , rmExternalDataSourceRefs = []
  , rmStartPolicy = defaultRPCStartPolicy
  }

diagnosticResource :: DataResourceSchema
diagnosticResource = DataResourceSchema
  { drsSchemaVersion = currentDataResourceSchemaVersion
  , drsResourceVersion = defaultDataResourceVersion
  , drsName = "stations"
  , drsLabel = "Stations"
  , drsHexBound = False
  , drsFields = [DataFieldDef "id" DFText "Id" False Nothing]
  , drsOperations = noOperations { doList = True, doCreate = True }
  , drsKeyField = "id"
  , drsOverlay = Nothing
  , drsPagination = defaultDataPagination
  }

diagnosticExternalSource :: RPCExternalDataSourceDecl
diagnosticExternalSource = RPCExternalDataSourceDecl
  { redsdName = "station-ledger"
  , redsdLabel = "Station Ledger"
  , redsdDescription = "Provider-owned station ledger"
  , redsdKind = "ledger"
  , redsdCapabilities = [ExternalSourceQuery, ExternalSourceHealth]
  , redsdResources = ["stations"]
  , redsdStatus = defaultRPCExternalDataSourceStatus
      { redssState = ExternalStatusReady
      , redssProviderId = Just "weather"
      , redssAvailability = Just ExternalAvailabilityAvailable
      , redssHealth = Just ExternalHealthHealthy
      , redssAccessMode = Just ExternalAccessModeReadOnly
      , redssCapabilityScope = [ExternalSourceQuery, ExternalSourceHealth]
      , redssObservedAt = Just appServiceTestTime
      , redssFresh = True
      }
  , redsdConnection = Nothing
  , redsdConfigRefs = [diagnosticExternalConfig "station-ledger-binding" ExternalConfigProvider]
  , redsdGrants = [diagnosticExternalGrant]
  , redsdUiHints = defaultRPCUIHints
  }

diagnosticExternalGrant :: RPCExternalDataSourceGrant
diagnosticExternalGrant = RPCExternalDataSourceGrant
  { redsgName = "stations-read"
  , redsgAccess = [ExternalAccessRead]
  , redsgCapabilities = [ExternalSourceQuery]
  , redsgResources = ["stations"]
  , redsgStatus = defaultRPCExternalDataSourceStatus
      { redssState = ExternalStatusDegraded
      , redssMessage = Just "grant pending provider health check"
      , redssProviderId = Just "weather"
      , redssAvailability = Just ExternalAvailabilityDegraded
      , redssHealth = Just ExternalHealthDegraded
      , redssAccessMode = Just ExternalAccessModeReadOnly
      , redssCapabilityScope = [ExternalSourceQuery]
      , redssObservedAt = Just appServiceTestTime
      , redssFresh = True
      }
  , redsgReference = Nothing
  , redsgConfigRefs = [diagnosticExternalConfig "stations-read-binding" ExternalConfigProvider]
  }

diagnosticExternalRef :: RPCExternalDataSourceRef
diagnosticExternalRef = RPCExternalDataSourceRef
  { redsrName = "station-ledger-ref"
  , redsrProvider = Just "weather"
  , redsrSource = "station-ledger"
  , redsrRequired = True
  , redsrAccess = [ExternalAccessRead]
  , redsrResources = ["stations"]
  , redsrGrant = Just "stations-read"
  , redsrStatus = defaultRPCExternalDataSourceStatus
      { redssState = ExternalStatusUnavailable
      , redssMessage = Just "provider grant not brokered"
      , redssProviderId = Just "weather"
      , redssAvailability = Just ExternalAvailabilityUnavailable
      , redssHealth = Just ExternalHealthUnhealthy
      , redssAccessMode = Just ExternalAccessModeDisabled
      }
  , redsrReference = Nothing
  , redsrConfigRefs = [diagnosticExternalConfig "station-ledger-ref-binding" ExternalConfigProvider]
  , redsrUiHints = defaultRPCUIHints
  }

diagnosticExternalConfig :: Text -> RPCExternalDataSourceConfigOrigin -> RPCExternalDataSourceConfigRef
diagnosticExternalConfig name origin = RPCExternalDataSourceConfigRef
  { redscrName = name
  , redscrOrigin = origin
  , redscrKey = name <> "-key"
  , redscrRequired = True
  , redscrCompatibility = Just "manifest-v3"
  , redscrMetadata = Nothing
  }

appServiceTestTime :: UTCTime
appServiceTestTime = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)

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
  , dataResourceExternalDataSources = []
  , dataResourceExternalDataSourceCount = 0
  , dataResourceExternalDataSourceFailures = 0
  }

simulationDagContract :: SimulationDagResponse
simulationDagContract = SimulationDagResponse
  { simulationDagNodes =
      [ SimulationDagNodeSummary
          { simulationDagNodeId = SimNodeId "weather"
          , simulationDagNodeKind = "builtin"
          , simulationDagNodePlugin = Nothing
          , simulationDagNodeOverlay = "rainfall"
          , simulationDagNodeDependencies = [SimNodeId "temperature"]
          , simulationDagNodeWritesTerrain = True
          , simulationDagNodeStatus = "completed"
          , simulationDagNodeStatusDetail = Nothing
          , simulationDagNodeIntervalTicks = Just 1
          , simulationDagNodePhaseTicks = Just 0
          , simulationDagNodeCatchUp = Just "run_once_if_due"
          , simulationDagNodeLastFireTick = Just 41
          , simulationDagNodeNextFireTick = Just 43
          , simulationDagNodeDue = Just False
          }
      ]
  , simulationDagLevels = [[SimNodeId "temperature"], [SimNodeId "weather"]]
  , simulationDagTerrainWriters = [SimNodeId "weather"]
  , simulationDagLastTick = 42
  , simulationDagPendingTick = Nothing
  , simulationDagTickLogs = []
  , simulationDagPluginNodes = []
  }

expectedCommandMethods :: [Text]
expectedCommandMethods = concatMap snd expectedServiceGroups

expectedServiceGroups :: [(Text, [Text])]
expectedServiceGroups =
  [ ( "state"
    , [ "get_state"
      , "get_view_modes"
      , "get_views"
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
      , "get_overlay_schema"
      , "get_overlay_provenance"
      , "export_overlay_data"
      , "validate_overlay_import"
      , "export_mesh_data"
      , "export_sample_data"
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
      , "set_view"
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
