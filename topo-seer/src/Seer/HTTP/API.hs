{-# LANGUAGE OverloadedStrings #-}

-- | HTTP API schema metadata for resource-oriented routes.
--
-- The WAI server currently routes through a compact route table, while this
-- module keeps request/response schema names next to the public HTTP contract
-- so generated OpenAPI is not just a list of paths.
module Seer.HTTP.API
  ( annotateHttpRouteSpec
    -- * Presets
  , presetsListResponseSchema
  , presetsSaveRequestSchema
  , presetsSaveResponseSchema
  , presetsLoadRequestSchema
  , presetsLoadResponseSchema
    -- * Pipeline
  , pipelineGetResponseSchema
  , pipelineSetStageEnabledRequestSchema
  , pipelineSetStageEnabledResponseSchema
    -- * Plugins
  , pluginListResponseSchema
  , pluginSetEnabledRequestSchema
  , pluginSetEnabledResponseSchema
  , pluginSetParamRequestSchema
  , pluginSetParamResponseSchema
    -- * Data resources
  , dataPluginsListResponseSchema
  , dataResourcesListResponseSchema
  , dataRecordsListResponseSchema
  , dataRecordGetRequestSchema
  , dataRecordGetResponseSchema
  , dataRecordCreateRequestSchema
  , dataRecordCreateResponseSchema
  , dataRecordUpdateRequestSchema
  , dataRecordUpdateResponseSchema
  , dataRecordDeleteRequestSchema
  , dataRecordDeleteResponseSchema
  , dataStateResponseSchema
    -- * Simulation
  , simulationStateResponseSchema
  , simulationDagResponseSchema
  , simulationAutoTickRequestSchema
  , simulationAutoTickResponseSchema
  , simulationTickRequestSchema
  , simulationTickResponseSchema
    -- * Logs and screenshots
  , logGetResponseSchema
  , screenshotTakeRequestSchema
  , screenshotTakeResponseSchema
  ) where

import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)

import Actor.UI.State (allBaseViewModes, allBuiltinSkyOverlayModes, allBuiltinViewModes, baseViewModeToText, skyOverlayModeToText, viewModeToText)
import Seer.HTTP.OpenAPI (HttpRouteSpec(..), JsonSchema(..))

-- Route annotation -----------------------------------------------------------

-- | Attach named request/response schemas to route specs by operation id.
-- Existing explicit schemas on a route win, so individual route definitions can
-- still override the shared map when a one-off shape is needed.
annotateHttpRouteSpec :: HttpRouteSpec -> HttpRouteSpec
annotateHttpRouteSpec spec = spec
  { hrsRequestSchema = preferExisting (hrsRequestSchema spec) (lookupSchema op requestSchemasByOperation)
  , hrsResponseSchema = preferExisting (hrsResponseSchema spec) (lookupSchema op responseSchemasByOperation)
  }
  where
    op = hrsOperationId spec

preferExisting :: Maybe JsonSchema -> Maybe JsonSchema -> Maybe JsonSchema
preferExisting existing@(Just _) _ = existing
preferExisting Nothing fallback = fallback

lookupSchema :: Text -> [(Text, JsonSchema)] -> Maybe JsonSchema
lookupSchema = lookup

requestSchemasByOperation :: [(Text, JsonSchema)]
requestSchemasByOperation =
  [ ("config.sliders.get", sliderGetRequestSchema)
  , ("config.sliders.set", sliderSetRequestSchema)
  , ("config.sliders.setMany", slidersSetManyRequestSchema)
  , ("config.sliders.reset", slidersResetRequestSchema)
  , ("presets.save", presetsSaveRequestSchema)
  , ("presets.load", presetsLoadRequestSchema)
  , ("world.generate", worldGenerateRequestSchema)
  , ("worlds.save", worldSaveRequestSchema)
  , ("worlds.load", worldLoadRequestSchema)
  , ("world.name.set", worldNameSetRequestSchema)
  , ("terrain.search", terrainSearchRequestSchema)
  , ("terrain.export", terrainExportRequestSchema)
  , ("terrain.mesh.export", terrainMeshExportRequestSchema)
  , ("terrain.sample.export", terrainSampleExportRequestSchema)
  , ("overlays.export", overlayExportRequestSchema)
  , ("overlays.import.validate", overlayImportValidateRequestSchema)
  , ("editor.toggle", editorToggleRequestSchema)
  , ("editor.tool.set", editorToolSetRequestSchema)
  , ("editor.brush.set", editorBrushSetRequestSchema)
  , ("editor.brushStroke", editorBrushStrokeRequestSchema)
  , ("editor.brushLine", editorBrushLineRequestSchema)
  , ("editor.biome.set", editorBiomeSetRequestSchema)
  , ("editor.form.set", editorFormSetRequestSchema)
  , ("editor.hardness.set", editorHardnessSetRequestSchema)
  , ("pipeline.stage.setEnabled", pipelineSetStageEnabledRequestSchema)
  , ("plugins.setEnabled", pluginSetEnabledRequestSchema)
  , ("plugins.params.set", pluginSetParamRequestSchema)
  , ("data.records.get", dataRecordGetRequestSchema)
  , ("data.records.create", dataRecordCreateRequestSchema)
  , ("data.records.update", dataRecordUpdateRequestSchema)
  , ("data.records.delete", dataRecordDeleteRequestSchema)
  , ("simulation.autoTick.set", simulationAutoTickRequestSchema)
  , ("simulation.tick", simulationTickRequestSchema)
  , ("screenshots.take", screenshotTakeRequestSchema)
  , ("ui.seed.set", uiSeedSetRequestSchema)
  , ("ui.viewMode.set", uiViewModeSetRequestSchema)
  , ("ui.view.set", uiViewSetRequestSchema)
  , ("ui.configTab.set", uiConfigTabSetRequestSchema)
  , ("ui.hex.select", uiSelectHexRequestSchema)
  , ("ui.overlay.set", uiOverlaySetRequestSchema)
  , ("ui.overlay.cycle", uiOverlayCycleRequestSchema)
  , ("ui.overlayField.cycle", uiOverlayFieldCycleRequestSchema)
  , ("overlays.current.set", uiOverlaySetRequestSchema)
  , ("overlays.cycle", uiOverlayCycleRequestSchema)
  , ("overlays.field.cycle", uiOverlayFieldCycleRequestSchema)
  , ("ui.camera.set", cameraSetRequestSchema)
  , ("ui.camera.zoomToChunk", cameraZoomToChunkRequestSchema)
  , ("camera.set", cameraSetRequestSchema)
  , ("camera.zoomToChunk", cameraZoomToChunkRequestSchema)
  , ("ui.leftPanel.set", uiLeftPanelSetRequestSchema)
  , ("ui.leftTab.set", uiLeftTabSetRequestSchema)
  , ("ui.configPanel.toggle", uiConfigPanelToggleRequestSchema)
  , ("ui.logCollapsed.set", uiLogCollapsedSetRequestSchema)
  , ("ui.logLevel.set", uiLogLevelSetRequestSchema)
  , ("ui.viewport.scroll", viewportScrollRequestSchema)
  , ("ui.viewport.click", viewportClickRequestSchema)
  , ("ui.viewport.drag", viewportDragRequestSchema)
  , ("ui.viewport.hover", viewportHoverRequestSchema)
  , ("ui.widgets.click", widgetClickRequestSchema)
  , ("ui.dialogText.set", dialogTextSetRequestSchema)
  , ("ui.key.send", keySendRequestSchema)
  ]

responseSchemasByOperation :: [(Text, JsonSchema)]
responseSchemasByOperation =
  [ ("meta.health", healthResponseSchema)
  , ("meta.version", versionResponseSchema)
  , ("meta.openapi", openApiDocumentResponseSchema)
  , ("events.list", eventsListResponseSchema)
  , ("state.get", appStateResponseSchema)
  , ("state.viewModes", stateViewModesResponseSchema)
  , ("state.views", stateViewsResponseSchema)
  , ("ui.state", uiStateResponseSchema)
  , ("config.sliders.list", slidersListResponseSchema)
  , ("config.sliders.get", sliderResponseSchema)
  , ("config.sliders.set", sliderSetResponseSchema)
  , ("config.sliders.setMany", slidersSetManyResponseSchema)
  , ("config.sliders.reset", slidersResetResponseSchema)
  , ("config.summary", configSummaryResponseSchema)
  , ("config.enums", configEnumsResponseSchema)
  , ("presets.list", presetsListResponseSchema)
  , ("presets.save", presetsSaveResponseSchema)
  , ("presets.load", presetsLoadResponseSchema)
  , ("world.generate", worldGenerateResponseSchema)
  , ("world.meta", worldMetaResponseSchema)
  , ("world.generationStatus", worldGenerationStatusResponseSchema)
  , ("worlds.list", worldsListResponseSchema)
  , ("worlds.save", worldSaveResponseSchema)
  , ("worlds.load", worldLoadResponseSchema)
  , ("world.name.set", worldNameSetResponseSchema)
  , ("terrain.hex", terrainHexResponseSchema)
  , ("terrain.chunks", terrainChunksResponseSchema)
  , ("terrain.chunkSummary", terrainChunkSummaryResponseSchema)
  , ("terrain.stats", terrainStatsResponseSchema)
  , ("terrain.overlays", terrainOverlaysResponseSchema)
  , ("terrain.search", terrainSearchResponseSchema)
  , ("terrain.export", terrainExportResponseSchema)
  , ("terrain.mesh.export", terrainMeshExportResponseSchema)
  , ("terrain.sample.export", terrainSampleExportResponseSchema)
  , ("editor.state", editorStateResponseSchema)
  , ("editor.toggle", editorStateResponseSchema)
  , ("editor.tool.set", editorStateResponseSchema)
  , ("editor.brush.set", editorStateResponseSchema)
  , ("editor.brushStroke", editorBrushQueuedResponseSchema)
  , ("editor.brushLine", editorBrushQueuedResponseSchema)
  , ("editor.biome.set", editorStateResponseSchema)
  , ("editor.form.set", editorStateResponseSchema)
  , ("editor.hardness.set", editorStateResponseSchema)
  , ("editor.undo", editorQueuedResponseSchema)
  , ("editor.redo", editorQueuedResponseSchema)
  , ("pipeline.get", pipelineGetResponseSchema)
  , ("pipeline.stage.setEnabled", pipelineSetStageEnabledResponseSchema)
  , ("plugins.list", pluginListResponseSchema)
  , ("plugins.status", pluginListResponseSchema)
  , ("plugins.state", pluginListResponseSchema)
  , ("plugins.dependencies", pluginListResponseSchema)
  , ("plugins.setEnabled", pluginSetEnabledResponseSchema)
  , ("plugins.params.set", pluginSetParamResponseSchema)
  , ("data.plugins.list", dataPluginsListResponseSchema)
  , ("data.resources.list", dataResourcesListResponseSchema)
  , ("data.records.list", dataRecordsListResponseSchema)
  , ("data.records.get", dataRecordGetResponseSchema)
  , ("data.records.create", dataRecordCreateResponseSchema)
  , ("data.records.update", dataRecordUpdateResponseSchema)
  , ("data.records.delete", dataRecordDeleteResponseSchema)
  , ("data.state", dataStateResponseSchema)
  , ("simulation.state", simulationStateResponseSchema)
  , ("simulation.dag", simulationDagResponseSchema)
  , ("simulation.autoTick.set", simulationAutoTickResponseSchema)
  , ("simulation.tick", simulationTickResponseSchema)
  , ("logs.get", logGetResponseSchema)
  , ("screenshots.take", screenshotTakeResponseSchema)
  , ("ui.seed.set", uiSeedSetResponseSchema)
  , ("ui.viewMode.set", uiViewModeSetResponseSchema)
  , ("ui.view.set", uiViewSetResponseSchema)
  , ("ui.configTab.set", uiConfigTabSetResponseSchema)
  , ("ui.hex.select", uiSelectHexResponseSchema)
  , ("ui.overlay.set", uiOverlaySetResponseSchema)
  , ("ui.overlayFields.list", uiOverlayFieldsResponseSchema)
  , ("ui.overlay.cycle", uiOverlayCycleResponseSchema)
  , ("ui.overlayField.cycle", uiOverlayFieldCycleResponseSchema)
  , ("overlays.list", terrainOverlaysResponseSchema)
  , ("overlays.schema.get", overlaySchemaResponseSchema)
  , ("overlays.provenance.get", overlayProvenanceResponseSchema)
  , ("overlays.export", overlayExportResponseSchema)
  , ("overlays.import.validate", overlayImportValidateResponseSchema)
  , ("overlays.current.set", uiOverlaySetResponseSchema)
  , ("overlays.fields.list", uiOverlayFieldsResponseSchema)
  , ("overlays.cycle", uiOverlayCycleResponseSchema)
  , ("overlays.field.cycle", uiOverlayFieldCycleResponseSchema)
  , ("ui.camera.set", cameraSetResponseSchema)
  , ("ui.camera.get", cameraGetResponseSchema)
  , ("ui.camera.zoomToChunk", cameraZoomToChunkResponseSchema)
  , ("camera.set", cameraSetResponseSchema)
  , ("camera.get", cameraGetResponseSchema)
  , ("camera.zoomToChunk", cameraZoomToChunkResponseSchema)
  , ("ui.leftPanel.set", uiLeftPanelSetResponseSchema)
  , ("ui.leftTab.set", uiLeftTabSetResponseSchema)
  , ("ui.configPanel.toggle", uiConfigPanelToggleResponseSchema)
  , ("ui.logCollapsed.set", uiLogCollapsedSetResponseSchema)
  , ("ui.logLevel.set", uiLogLevelSetResponseSchema)
  , ("ui.panels.get", uiPanelsResponseSchema)
  , ("ui.viewport.scroll", viewportScrollResponseSchema)
  , ("ui.viewport.click", viewportClickResponseSchema)
  , ("ui.viewport.drag", viewportDragResponseSchema)
  , ("ui.viewport.hover", viewportHoverResponseSchema)
  , ("ui.widgets.click", widgetClickResponseSchema)
  , ("ui.widgets.list", widgetListResponseSchema)
  , ("ui.widgetState.get", widgetStateResponseSchema)
  , ("ui.dialog.get", dialogStateResponseSchema)
  , ("ui.dialogText.set", dialogTextSetResponseSchema)
  , ("ui.dialog.confirm", dialogActionResponseSchema)
  , ("ui.dialog.cancel", dialogActionResponseSchema)
  , ("ui.key.send", keySendResponseSchema)
  ]

-- Meta, auth-adjacent, and events ------------------------------------------

healthResponseSchema :: JsonSchema
healthResponseSchema = objectSchema "HealthResponse"
  [ "status" ]
  [ ("status", enumStringSchema ["ok"])
  ]

versionResponseSchema :: JsonSchema
versionResponseSchema = objectSchema "VersionResponse"
  [ "name", "version", "api_version" ]
  [ ("name", stringSchema)
  , ("version", stringSchema)
  , ("api_version", stringSchema)
  ]

openApiDocumentResponseSchema :: JsonSchema
openApiDocumentResponseSchema = JsonSchema "OpenApiDocumentResponse" freeObjectSchema

eventsListResponseSchema :: JsonSchema
eventsListResponseSchema = objectSchema "EventsListResponse"
  [ "events", "mode" ]
  [ ("events", arraySchema eventEnvelopeSchema)
  , ("mode", enumStringSchema ["polling"])
  ]

eventEnvelopeSchema :: Value
eventEnvelopeSchema = inlineObjectSchema
  [ "topic", "source", "severity", "payload" ]
  [ ("topic", stringSchema)
  , ("source", enumStringSchema ["http", "ui", "command", "service", "system"])
  , ("severity", logLevelSchema)
  , ("sequence", nullableSchema integerSchema)
  , ("correlation_id", nullableSchema stringSchema)
  , ("payload", anySchema)
  ]

-- State, UI state, and config -----------------------------------------------

appStateResponseSchema :: JsonSchema
appStateResponseSchema = objectSchema "AppStateResponse"
  [ "seed", "view_mode", "view", "config_tab", "generating", "chunk_size", "show_config", "world_name" ]
  [ ("seed", integerSchema)
  , ("view_mode", viewModeSchema)
  , ("view", layeredViewSelectionSchema)
  , ("config_tab", configTabSchema)
  , ("generating", booleanSchema)
  , ("chunk_size", integerSchema)
  , ("show_config", booleanSchema)
  , ("world_name", stringSchema)
  , ("context_hex", nullableSchema axialCoordSchema)
  ]

stateViewModesResponseSchema :: JsonSchema
stateViewModesResponseSchema = objectSchema "StateViewModesResponse"
  [ "view_modes" ]
  [ ("view_modes", arraySchema viewModeEntrySchema)
  , ("view", layeredViewSelectionSchema)
  ]

stateViewsResponseSchema :: JsonSchema
stateViewsResponseSchema = objectSchema "StateViewsResponse"
  [ "view", "base_modes", "overlay_modes", "weather_bases", "overlay_names", "legacy_modes" ]
  [ ("view", layeredViewSelectionSchema)
  , ("legacy_view_mode", viewModeSchema)
  , ("base_modes", arraySchema baseViewEntrySchema)
  , ("overlay_modes", arraySchema overlayViewEntrySchema)
  , ("weather_bases", arraySchema weatherBasisEntrySchema)
  , ("overlay_names", arraySchema stringSchema)
  , ("legacy_modes", arraySchema viewModeEntrySchema)
  ]

layeredViewSelectionSchema :: Value
layeredViewSelectionSchema = inlineObjectSchema
  [ "base", "base_mode", "weather_basis", "overlay_opacity", "legacy_view_mode" ]
  [ ("base", baseViewModeSchema)
  , ("base_mode", baseViewModeSchema)
  , ("base_label", stringSchema)
  , ("overlay", nullableSchema stringSchema)
  , ("overlay_mode", nullableSchema overlayModeSchema)
  , ("plugin_overlay", nullableSchema stringSchema)
  , ("overlay_field", nullableSchema integerSchema)
  , ("overlay_label", nullableSchema stringSchema)
  , ("weather_basis", weatherBasisSchema)
  , ("temporal_basis", nullableSchema temporalBasisSchema)
  , ("source_kind", nullableSchema sourceKindSchema)
  , ("overlay_opacity", numberSchema)
  , ("legacy_view_mode", nullableSchema viewModeSchema)
  ]

uiStateResponseSchema :: JsonSchema
uiStateResponseSchema = objectSchema "UiStateResponse"
  [ "seed", "generating", "world_name", "chunk_size", "view", "panels", "editor", "data_browser", "hex_selection", "simulation" ]
  [ ("seed", integerSchema)
  , ("generating", booleanSchema)
  , ("world_name", stringSchema)
  , ("chunk_size", integerSchema)
  , ("view", inlineObjectSchema ["mode", "base_mode", "weather_basis", "selection", "overlay_names"]
      [ ("mode", viewModeSchema)
      , ("base_mode", baseViewModeSchema)
      , ("overlay_mode", nullableSchema overlayModeSchema)
      , ("plugin_overlay", nullableSchema stringSchema)
      , ("weather_basis", weatherBasisSchema)
      , ("overlay_opacity", numberSchema)
      , ("legacy_view_mode", nullableSchema viewModeSchema)
      , ("temporal_basis", nullableSchema temporalBasisSchema)
      , ("source_kind", nullableSchema sourceKindSchema)
      , ("selection", layeredViewSelectionSchema)
      , ("overlay_name", nullableSchema stringSchema)
      , ("overlay_field", nullableSchema integerSchema)
      , ("overlay_names", arraySchema stringSchema)
      ])
  , ("panels", uiStatePanelsSchema)
  , ("editor", inlineObjectSchema ["active", "tool"]
      [ ("active", booleanSchema)
      , ("tool", editorToolSchema)
      ])
  , ("data_browser", dataBrowserStateSchema)
  , ("hex_selection", inlineObjectSchema ["pinned"]
      [ ("context_hex", nullableSchema axialCoordSchema)
      , ("pinned", booleanSchema)
      , ("hover_hex", nullableSchema axialCoordSchema)
      ])
  , ("simulation", inlineObjectSchema ["auto_tick", "tick_count"]
      [ ("auto_tick", booleanSchema)
      , ("tick_count", integerSchema)
      ])
  ]

slidersListResponseSchema :: JsonSchema
slidersListResponseSchema = objectSchema "SlidersListResponse"
  [ "sliders" ]
  [ ("sliders", arraySchema sliderSchema)
  ]

sliderGetRequestSchema :: JsonSchema
sliderGetRequestSchema = nameRequestSchema "SliderGetRequest"

sliderSetRequestSchema :: JsonSchema
sliderSetRequestSchema = objectSchema "SliderSetRequest"
  [ "name", "value" ]
  [ ("name", stringSchema)
  , ("value", numberSchema)
  ]

sliderResponseSchema :: JsonSchema
sliderResponseSchema = JsonSchema "SliderResponse" sliderSchema

sliderSetResponseSchema :: JsonSchema
sliderSetResponseSchema = objectSchema "SliderSetResponse"
  [ "name", "value" ]
  [ ("name", stringSchema)
  , ("value", numberSchema)
  ]

slidersSetManyRequestSchema :: JsonSchema
slidersSetManyRequestSchema = objectSchema "SlidersSetManyRequest"
  [ "values" ]
  [ ("values", freeObjectSchema)
  ]

slidersSetManyResponseSchema :: JsonSchema
slidersSetManyResponseSchema = objectSchema "SlidersSetManyResponse"
  [ "updated", "unknown" ]
  [ ("updated", arraySchema sliderSetEntrySchema)
  , ("unknown", arraySchema stringSchema)
  ]

slidersResetRequestSchema :: JsonSchema
slidersResetRequestSchema = objectSchema "SlidersResetRequest"
  []
  [ ("tab", sliderTabSchema)
  ]

slidersResetResponseSchema :: JsonSchema
slidersResetResponseSchema = objectSchema "SlidersResetResponse"
  [ "reset_count" ]
  [ ("reset_count", integerSchema)
  , ("tab", nullableSchema sliderTabSchema)
  ]

configSummaryResponseSchema :: JsonSchema
configSummaryResponseSchema = objectSchema "ConfigSummaryResponse"
  [ "tabs" ]
  [ ("tabs", arraySchema (inlineObjectSchema ["tab", "sliders"]
      [ ("tab", sliderTabSchema)
      , ("sliders", arraySchema sliderSchema)
      ]))
  ]

configEnumsResponseSchema :: JsonSchema
configEnumsResponseSchema = objectSchema "ConfigEnumsResponse"
  [ "values" ]
  [ ("values", arraySchema enumEntrySchema)
  ]

sliderSchema :: Value
sliderSchema = inlineObjectSchema
  [ "name", "tab", "value", "domain_value", "domain_min", "domain_max", "value_kind", "default", "default_domain" ]
  [ ("name", stringSchema)
  , ("tab", sliderTabSchema)
  , ("value", numberSchema)
  , ("domain_value", numberSchema)
  , ("domain_min", numberSchema)
  , ("domain_max", numberSchema)
  , ("value_kind", enumStringSchema ["float", "int", "toggle"])
  , ("default", numberSchema)
  , ("default_domain", numberSchema)
  ]

sliderSetEntrySchema :: Value
sliderSetEntrySchema = inlineObjectSchema
  [ "name", "value" ]
  [ ("name", stringSchema)
  , ("value", numberSchema)
  ]

enumEntrySchema :: Value
enumEntrySchema = inlineObjectSchema
  [ "name", "code" ]
  [ ("name", stringSchema)
  , ("code", integerSchema)
  ]

-- Presets -------------------------------------------------------------------

presetsListResponseSchema :: JsonSchema
presetsListResponseSchema = objectSchema "PresetsListResponse"
  [ "preset_count", "presets" ]
  [ ("preset_count", integerSchema)
  , ("presets", arraySchema stringSchema)
  ]

presetsSaveRequestSchema :: JsonSchema
presetsSaveRequestSchema = nameRequestSchema "PresetsSaveRequest"

presetsSaveResponseSchema :: JsonSchema
presetsSaveResponseSchema = objectSchema "PresetsSaveResponse"
  [ "name", "saved" ]
  [ ("name", stringSchema)
  , ("saved", booleanSchema)
  ]

presetsLoadRequestSchema :: JsonSchema
presetsLoadRequestSchema = nameRequestSchema "PresetsLoadRequest"

presetsLoadResponseSchema :: JsonSchema
presetsLoadResponseSchema = objectSchema "PresetsLoadResponse"
  [ "name", "loaded" ]
  [ ("name", stringSchema)
  , ("loaded", booleanSchema)
  ]

-- Worlds, generation, and terrain ------------------------------------------

worldGenerateRequestSchema :: JsonSchema
worldGenerateRequestSchema = objectSchema "WorldGenerateRequest"
  []
  [ ("seed", integerSchema)
  , ("config", freeObjectSchema)
  ]

worldGenerateResponseSchema :: JsonSchema
worldGenerateResponseSchema = statusResponseSchema "WorldGenerateResponse"

worldMetaResponseSchema :: JsonSchema
worldMetaResponseSchema = objectSchema "WorldMetaResponse"
  [ "seed", "chunk_size", "tiles_per_chunk", "chunk_count", "total_tiles", "chunk_ids", "overlay_names", "world_name", "generating" ]
  [ ("seed", integerSchema)
  , ("chunk_size", integerSchema)
  , ("tiles_per_chunk", integerSchema)
  , ("chunk_count", integerSchema)
  , ("total_tiles", integerSchema)
  , ("chunk_ids", arraySchema integerSchema)
  , ("overlay_names", arraySchema stringSchema)
  , ("world_name", stringSchema)
  , ("generating", booleanSchema)
  ]

worldGenerationStatusResponseSchema :: JsonSchema
worldGenerationStatusResponseSchema = objectSchema "WorldGenerationStatusResponse"
  [ "generating", "chunk_count", "seed" ]
  [ ("generating", booleanSchema)
  , ("chunk_count", integerSchema)
  , ("seed", integerSchema)
  ]

worldsListResponseSchema :: JsonSchema
worldsListResponseSchema = objectSchema "WorldsListResponse"
  [ "world_count", "worlds" ]
  [ ("world_count", integerSchema)
  , ("worlds", arraySchema worldSummarySchema)
  ]

worldSaveRequestSchema :: JsonSchema
worldSaveRequestSchema = nameRequestSchema "WorldSaveRequest"

worldSaveResponseSchema :: JsonSchema
worldSaveResponseSchema = objectSchema "WorldSaveResponse"
  [ "name", "saved" ]
  [ ("name", stringSchema)
  , ("saved", booleanSchema)
  , ("formats", arraySchema stringSchema)
  , ("diagnostics", arraySchema diagnosticSchema)
  ]

worldLoadRequestSchema :: JsonSchema
worldLoadRequestSchema = nameRequestSchema "WorldLoadRequest"

worldLoadResponseSchema :: JsonSchema
worldLoadResponseSchema = objectSchema "WorldLoadResponse"
  [ "name", "loaded" ]
  [ ("name", stringSchema)
  , ("loaded", booleanSchema)
  , ("formats", arraySchema stringSchema)
  , ("overlay_names", arraySchema stringSchema)
  , ("diagnostics", arraySchema diagnosticSchema)
  ]

worldNameSetRequestSchema :: JsonSchema
worldNameSetRequestSchema = nameRequestSchema "WorldNameSetRequest"

worldNameSetResponseSchema :: JsonSchema
worldNameSetResponseSchema = objectSchema "WorldNameSetResponse"
  [ "name" ]
  [ ("name", stringSchema)
  ]

worldSummarySchema :: Value
worldSummarySchema = inlineObjectSchema
  [ "name", "seed", "chunk_size", "chunk_count", "overlay_names", "created_at" ]
  [ ("name", stringSchema)
  , ("seed", integerSchema)
  , ("chunk_size", integerSchema)
  , ("chunk_count", integerSchema)
  , ("overlay_names", arraySchema stringSchema)
  , ("weather_layers", arraySchema weatherLayerManifestSchema)
  , ("created_at", stringSchema)
  ]

terrainHexResponseSchema :: JsonSchema
terrainHexResponseSchema = objectSchema "TerrainHexResponse"
  [ "q", "r", "terrain", "hypsometry", "terrain_form_metrics", "hydrology", "climate", "climate_diagnostics", "weather", "weather_snapshot", "weather_normals", "weather_typical", "weather_timeline", "river", "water_body", "water_bodies", "water_table", "soil", "biome_refinement", "vegetation", "glacier", "glacier_snow_ice", "volcanism", "ocean_currents", "units", "active_view", "sections" ]
  [ ("q", integerSchema)
  , ("r", integerSchema)
  , ("terrain", terrainLayerSchema)
  , ("hypsometry", freeObjectSchema)
  , ("terrain_form_metrics", freeObjectSchema)
  , ("hydrology", freeObjectSchema)
  , ("climate", nullableSchema terrainClimateLayerSchema)
  , ("climate_diagnostics", freeObjectSchema)
  , ("weather", nullableSchema terrainWeatherCurrentLayerSchema)
  , ("weather_snapshot", nullableSchema terrainWeatherCurrentLayerSchema)
  , ("weather_normals", terrainWeatherTypicalLayerSchema)
  , ("weather_typical", terrainWeatherTypicalLayerSchema)
  , ("weather_timeline", freeObjectSchema)
  , ("river", nullableSchema freeObjectSchema)
  , ("water_body", nullableSchema freeObjectSchema)
  , ("water_bodies", nullableSchema freeObjectSchema)
  , ("water_table", nullableSchema freeObjectSchema)
  , ("soil", freeObjectSchema)
  , ("biome_refinement", freeObjectSchema)
  , ("vegetation", nullableSchema freeObjectSchema)
  , ("glacier", nullableSchema freeObjectSchema)
  , ("glacier_snow_ice", nullableSchema freeObjectSchema)
  , ("volcanism", nullableSchema freeObjectSchema)
  , ("ocean_currents", freeObjectSchema)
  , ("units", freeObjectSchema)
  , ("active_view", terrainActiveViewSchema)
  , ("sections", arraySchema terrainInspectorSectionSchema)
  ]

terrainChunksResponseSchema :: JsonSchema
terrainChunksResponseSchema = objectSchema "TerrainChunksResponse"
  [ "chunk_count", "chunk_size", "tiles_per_chunk", "chunks" ]
  [ ("chunk_count", integerSchema)
  , ("chunk_size", integerSchema)
  , ("tiles_per_chunk", integerSchema)
  , ("chunks", arraySchema terrainChunkBriefSchema)
  ]

terrainChunkSummaryResponseSchema :: JsonSchema
terrainChunkSummaryResponseSchema = objectSchema "TerrainChunkSummaryResponse"
  [ "chunk", "tile_count", "elevation_min", "elevation_max", "elevation_mean", "moisture_mean", "dominant_biome", "terrain_form_distribution" ]
  [ ("chunk", integerSchema)
  , ("tile_count", integerSchema)
  , ("elevation_min", numberSchema)
  , ("elevation_max", numberSchema)
  , ("elevation_mean", numberSchema)
  , ("moisture_mean", numberSchema)
  , ("dominant_biome", stringSchema)
  , ("terrain_form_distribution", arraySchema distributionEntrySchema)
  , ("climate", nullableSchema freeObjectSchema)
  , ("river", nullableSchema freeObjectSchema)
  ]

terrainStatsResponseSchema :: JsonSchema
terrainStatsResponseSchema = objectSchema "TerrainStatsResponse"
  [ "chunk_count" ]
  [ ("status", stringSchema)
  , ("chunk_count", integerSchema)
  , ("total_tiles", integerSchema)
  , ("elevation", nullableSchema statsTripletSchema)
  , ("moisture_mean", numberSchema)
  , ("biome_distribution", arraySchema distributionEntrySchema)
  , ("terrain_form_distribution", arraySchema distributionEntrySchema)
  , ("temperature", nullableSchema statsTripletSchema)
  , ("precipitation", nullableSchema statsTripletSchema)
  , ("vegetation", nullableSchema freeObjectSchema)
  , ("river", nullableSchema freeObjectSchema)
  ]

terrainOverlaysResponseSchema :: JsonSchema
terrainOverlaysResponseSchema = objectSchema "TerrainOverlaysResponse"
  [ "overlay_count", "overlay_names" ]
  [ ("overlay_count", integerSchema)
  , ("overlay_names", arraySchema stringSchema)
  , ("active_overlay", nullableSchema stringSchema)
  , ("active_field_index", nullableSchema integerSchema)
  , ("overlays", arraySchema overlaySummarySchema)
  , ("diagnostics", arraySchema diagnosticSchema)
  ]

terrainSearchRequestSchema :: JsonSchema
terrainSearchRequestSchema = objectSchema "TerrainSearchRequest"
  [ "filters" ]
  [ ("filters", arraySchema terrainFilterSchema)
  , ("limit", integerSchema)
  ]

terrainSearchResponseSchema :: JsonSchema
terrainSearchResponseSchema = objectSchema "TerrainSearchResponse"
  [ "matches", "count" ]
  [ ("matches", arraySchema terrainSearchMatchSchema)
  , ("count", integerSchema)
  ]

terrainExportRequestSchema :: JsonSchema
terrainExportRequestSchema = objectSchema "TerrainExportRequest"
  []
  [ ("chunks", arraySchema integerSchema)
  , ("fields", arraySchema stringSchema)
  ]

terrainExportResponseSchema :: JsonSchema
terrainExportResponseSchema = objectSchema "TerrainExportResponse"
  [ "chunk_count", "fields", "available_fields", "data" ]
  [ ("chunk_count", integerSchema)
  , ("fields", arraySchema stringSchema)
  , ("available_fields", arraySchema stringSchema)
  , ("data", freeObjectSchema)
  , ("diagnostics", arraySchema diagnosticSchema)
  ]

terrainMeshExportRequestSchema :: JsonSchema
terrainMeshExportRequestSchema = objectSchema "TerrainMeshExportRequest"
  []
  [ ("x0", integerSchema)
  , ("y0", integerSchema)
  , ("x1", integerSchema)
  , ("y1", integerSchema)
  ]

terrainMeshExportResponseSchema :: JsonSchema
terrainMeshExportResponseSchema = objectSchema "TerrainMeshExportResponse"
  [ "format", "vertex_count", "index_count", "vertices", "indices", "diagnostics" ]
  [ ("format", enumStringSchema ["topo-mesh-json"])
  , ("region", freeObjectSchema)
  , ("vertex_count", integerSchema)
  , ("index_count", integerSchema)
  , ("vertices", arraySchema vertexSchema)
  , ("indices", arraySchema integerSchema)
  , ("diagnostics", arraySchema diagnosticSchema)
  ]

terrainSampleExportRequestSchema :: JsonSchema
terrainSampleExportRequestSchema = objectSchema "TerrainSampleExportRequest"
  [ "x", "y" ]
  [ ("x", numberSchema)
  , ("y", numberSchema)
  , ("real_units", booleanSchema)
  ]

terrainSampleExportResponseSchema :: JsonSchema
terrainSampleExportResponseSchema = objectSchema "TerrainSampleExportResponse"
  [ "format", "position", "sample", "diagnostics" ]
  [ ("format", enumStringSchema ["topo-sample-json"])
  , ("position", freeObjectSchema)
  , ("real_units", booleanSchema)
  , ("sample", freeObjectSchema)
  , ("diagnostics", arraySchema diagnosticSchema)
  ]

overlaySchemaResponseSchema :: JsonSchema
overlaySchemaResponseSchema = objectSchema "OverlaySchemaResponse"
  [ "overlay", "format", "schema", "fields", "diagnostics" ]
  [ ("overlay", stringSchema)
  , ("format", enumStringSchema ["toposchema"])
  , ("schema", freeObjectSchema)
  , ("fields", arraySchema overlayFieldSchema)
  , ("diagnostics", arraySchema diagnosticSchema)
  ]

overlayProvenanceResponseSchema :: JsonSchema
overlayProvenanceResponseSchema = objectSchema "OverlayProvenanceResponse"
  [ "overlay", "format", "provenance", "diagnostics" ]
  [ ("overlay", stringSchema)
  , ("format", enumStringSchema ["topolay-provenance"])
  , ("provenance", overlayProvenanceSchema)
  , ("diagnostics", arraySchema diagnosticSchema)
  ]

overlayExportRequestSchema :: JsonSchema
overlayExportRequestSchema = objectSchema "OverlayExportRequest"
  [ "overlay" ]
  [ ("overlay", stringSchema)
  , ("chunks", arraySchema integerSchema)
  ]

overlayExportResponseSchema :: JsonSchema
overlayExportResponseSchema = objectSchema "OverlayExportResponse"
  [ "overlay", "format", "schema", "provenance", "chunk_count", "payload", "diagnostics" ]
  [ ("overlay", stringSchema)
  , ("format", enumStringSchema ["topolay-json"])
  , ("schema", freeObjectSchema)
  , ("provenance", overlayProvenanceSchema)
  , ("chunk_count", integerSchema)
  , ("payload", freeObjectSchema)
  , ("diagnostics", arraySchema diagnosticSchema)
  ]

overlayImportValidateRequestSchema :: JsonSchema
overlayImportValidateRequestSchema = objectSchema "OverlayImportValidateRequest"
  [ "schema", "payload" ]
  [ ("schema", freeObjectSchema)
  , ("payload", freeObjectSchema)
  ]

overlayImportValidateResponseSchema :: JsonSchema
overlayImportValidateResponseSchema = objectSchema "OverlayImportValidateResponse"
  [ "valid", "diagnostics" ]
  [ ("valid", booleanSchema)
  , ("overlay", nullableSchema stringSchema)
  , ("diagnostics", arraySchema diagnosticSchema)
  ]

terrainActiveViewSchema :: Value
terrainActiveViewSchema = inlineObjectSchema
  [ "mode", "base_mode", "weather_basis", "label", "description", "temporal_basis", "source_kind", "tooltip_fields", "inspector_fields", "export_fields", "active_base", "values" ]
  [ ("mode", viewModeSchema)
  , ("base_mode", baseViewModeSchema)
  , ("overlay_mode", nullableSchema overlayModeSchema)
  , ("plugin_overlay", nullableSchema stringSchema)
  , ("overlay_field", nullableSchema integerSchema)
  , ("weather_basis", weatherBasisSchema)
  , ("overlay_opacity", numberSchema)
  , ("legacy_view_mode", nullableSchema viewModeSchema)
  , ("label", stringSchema)
  , ("description", nullableSchema stringSchema)
  , ("basis", nullableSchema temporalBasisSchema)
  , ("temporal_basis", nullableSchema temporalBasisSchema)
  , ("source_kind", nullableSchema sourceKindSchema)
  , ("unit", nullableSchema stringSchema)
  , ("color_scale", nullableSchema stringSchema)
  , ("tooltip_fields", arraySchema stringSchema)
  , ("inspector_fields", arraySchema stringSchema)
  , ("export_fields", arraySchema stringSchema)
  , ("active_base", terrainActiveLayerSchema)
  , ("active_overlay", nullableSchema terrainActiveLayerSchema)
  , ("values", freeObjectSchema)
  ]

terrainActiveLayerSchema :: Value
terrainActiveLayerSchema = inlineObjectSchema
  [ "mode", "label", "values" ]
  [ ("mode", viewModeSchema)
  , ("plugin_overlay", nullableSchema stringSchema)
  , ("field_index", nullableSchema integerSchema)
  , ("label", stringSchema)
  , ("description", nullableSchema stringSchema)
  , ("basis", nullableSchema temporalBasisSchema)
  , ("temporal_basis", nullableSchema temporalBasisSchema)
  , ("source_kind", nullableSchema sourceKindSchema)
  , ("weather_version", nullableSchema integerSchema)
  , ("published_weather_version", nullableSchema integerSchema)
  , ("data_weather_version", nullableSchema integerSchema)
  , ("unit", nullableSchema stringSchema)
  , ("color_scale", nullableSchema stringSchema)
  , ("tooltip_fields", arraySchema stringSchema)
  , ("inspector_fields", arraySchema stringSchema)
  , ("export_fields", arraySchema stringSchema)
  , ("values", freeObjectSchema)
  ]

terrainClimateLayerSchema :: Value
terrainClimateLayerSchema = inlineObjectSchema
  [ "temporal_basis", "basis", "source_kind" ]
  [ ("temporal_basis", temporalBasisSchema)
  , ("basis", temporalBasisSchema)
  , ("source_kind", sourceKindSchema)
  , ("temp_avg", nullableSchema numberSchema)
  , ("precip_avg", nullableSchema numberSchema)
  , ("wind_dir_avg", nullableSchema numberSchema)
  , ("wind_spd_avg", nullableSchema numberSchema)
  , ("humidity_avg", nullableSchema numberSchema)
  , ("temp_range", nullableSchema numberSchema)
  , ("precip_seasonality", nullableSchema numberSchema)
  ]

terrainWeatherCurrentLayerSchema :: Value
terrainWeatherCurrentLayerSchema = inlineObjectSchema
  [ "temporal_basis", "basis", "source_kind" ]
  [ ("temporal_basis", temporalBasisSchema)
  , ("basis", temporalBasisSchema)
  , ("source_kind", sourceKindSchema)
  , ("temp", nullableSchema numberSchema)
  , ("temp_current", nullableSchema numberSchema)
  , ("temperature_current", nullableSchema numberSchema)
  , ("humidity", nullableSchema numberSchema)
  , ("humidity_current", nullableSchema numberSchema)
  , ("wind_dir", nullableSchema numberSchema)
  , ("wind_dir_current", nullableSchema numberSchema)
  , ("wind_spd", nullableSchema numberSchema)
  , ("wind_spd_current", nullableSchema numberSchema)
  , ("pressure", nullableSchema numberSchema)
  , ("pressure_current", nullableSchema numberSchema)
  , ("precip", nullableSchema numberSchema)
  , ("precip_current", nullableSchema numberSchema)
  , ("precipitation_current", nullableSchema numberSchema)
  , ("cloud_cover", nullableSchema numberSchema)
  , ("cloud_cover_current", nullableSchema numberSchema)
  , ("cloud_water", nullableSchema numberSchema)
  , ("cloud_water_current", nullableSchema numberSchema)
  , ("cloud_cover_low", nullableSchema numberSchema)
  , ("cloud_cover_low_current", nullableSchema numberSchema)
  , ("cloud_cover_mid", nullableSchema numberSchema)
  , ("cloud_cover_mid_current", nullableSchema numberSchema)
  , ("cloud_cover_high", nullableSchema numberSchema)
  , ("cloud_cover_high_current", nullableSchema numberSchema)
  , ("cloud_water_low", nullableSchema numberSchema)
  , ("cloud_water_low_current", nullableSchema numberSchema)
  , ("cloud_water_mid", nullableSchema numberSchema)
  , ("cloud_water_mid_current", nullableSchema numberSchema)
  , ("cloud_water_high", nullableSchema numberSchema)
  , ("cloud_water_high_current", nullableSchema numberSchema)
  ]

terrainWeatherTypicalLayerSchema :: Value
terrainWeatherTypicalLayerSchema = inlineObjectSchema
  [ "loaded", "status", "temporal_basis", "basis", "source_kind" ]
  [ ("loaded", booleanSchema)
  , ("status", stringSchema)
  , ("reason", stringSchema)
  , ("temporal_basis", temporalBasisSchema)
  , ("basis", temporalBasisSchema)
  , ("source_kind", sourceKindSchema)
  , ("temp", nullableSchema numberSchema)
  , ("temp_typical", nullableSchema numberSchema)
  , ("temperature", nullableSchema numberSchema)
  , ("temperature_typical", nullableSchema numberSchema)
  , ("humidity", nullableSchema numberSchema)
  , ("humidity_typical", nullableSchema numberSchema)
  , ("wind_dir", nullableSchema numberSchema)
  , ("wind_dir_typical", nullableSchema numberSchema)
  , ("wind_spd", nullableSchema numberSchema)
  , ("wind_spd_typical", nullableSchema numberSchema)
  , ("wind_speed", nullableSchema numberSchema)
  , ("wind_speed_typical", nullableSchema numberSchema)
  , ("precip", nullableSchema numberSchema)
  , ("precip_typical", nullableSchema numberSchema)
  , ("precipitation", nullableSchema numberSchema)
  , ("precipitation_typical", nullableSchema numberSchema)
  , ("cloud_cover", nullableSchema numberSchema)
  , ("cloud_cover_typical", nullableSchema numberSchema)
  , ("cloud_water", nullableSchema numberSchema)
  , ("cloud_water_typical", nullableSchema numberSchema)
  , ("cloud_cover_low", nullableSchema numberSchema)
  , ("cloud_cover_low_typical", nullableSchema numberSchema)
  , ("cloud_cover_mid", nullableSchema numberSchema)
  , ("cloud_cover_mid_typical", nullableSchema numberSchema)
  , ("cloud_cover_high", nullableSchema numberSchema)
  , ("cloud_cover_high_typical", nullableSchema numberSchema)
  , ("cloud_water_low", nullableSchema numberSchema)
  , ("cloud_water_low_typical", nullableSchema numberSchema)
  , ("cloud_water_mid", nullableSchema numberSchema)
  , ("cloud_water_mid_typical", nullableSchema numberSchema)
  , ("cloud_water_high", nullableSchema numberSchema)
  , ("cloud_water_high_typical", nullableSchema numberSchema)
  ]

weatherLayerManifestSchema :: Value
weatherLayerManifestSchema = inlineObjectSchema
  [ "name", "basis", "source_kind", "storage" ]
  [ ("name", stringSchema)
  , ("basis", temporalBasisSchema)
  , ("source_kind", sourceKindSchema)
  , ("storage", enumStringSchema ["core_topo", "overlay_sidecar"])
  ]

terrainInspectorSectionSchema :: Value
terrainInspectorSectionSchema = inlineObjectSchema
  [ "key", "title", "fields" ]
  [ ("key", stringSchema)
  , ("title", stringSchema)
  , ("fields", arraySchema terrainInspectorFieldSchema)
  ]

terrainInspectorFieldSchema :: Value
terrainInspectorFieldSchema = inlineObjectSchema
  [ "key", "label", "value", "raw" ]
  [ ("key", stringSchema)
  , ("label", stringSchema)
  , ("value", stringSchema)
  , ("raw", anySchema)
  ]

terrainLayerSchema :: Value
terrainLayerSchema = inlineObjectSchema
  []
  [ ("elevation", nullableSchema numberSchema)
  , ("elevation_m", nullableSchema numberSchema)
  , ("curvature", nullableSchema numberSchema)
  , ("hardness", nullableSchema numberSchema)
  , ("moisture", nullableSchema numberSchema)
  , ("fertility", nullableSchema numberSchema)
  , ("roughness", nullableSchema numberSchema)
  , ("rock_density", nullableSchema numberSchema)
  , ("soil_depth", nullableSchema numberSchema)
  , ("soil_grain", nullableSchema numberSchema)
  , ("relief", nullableSchema numberSchema)
  , ("relief_2ring", nullableSchema numberSchema)
  , ("relief_3ring", nullableSchema numberSchema)
  , ("micro_relief", nullableSchema numberSchema)
  , ("ruggedness", nullableSchema numberSchema)
  , ("terrain_form", nullableSchema stringSchema)
  , ("terrain_form_code", nullableSchema integerSchema)
  , ("biome", nullableSchema stringSchema)
  , ("biome_code", nullableSchema integerSchema)
  , ("rock_type", nullableSchema integerSchema)
  , ("soil_type", nullableSchema integerSchema)
  , ("plate_id", nullableSchema integerSchema)
  , ("plate_boundary", nullableSchema stringSchema)
  , ("plate_boundary_code", nullableSchema integerSchema)
  , ("plate_crust", nullableSchema stringSchema)
  , ("plate_crust_code", nullableSchema integerSchema)
  , ("plate_height", nullableSchema numberSchema)
  , ("plate_hardness", nullableSchema numberSchema)
  , ("plate_age", nullableSchema numberSchema)
  , ("plate_velocity_x", nullableSchema numberSchema)
  , ("plate_velocity_y", nullableSchema numberSchema)
  , ("slope_avg", nullableSchema numberSchema)
  , ("slope_max", nullableSchema numberSchema)
  , ("slope_min", nullableSchema numberSchema)
  ]

terrainChunkBriefSchema :: Value
terrainChunkBriefSchema = inlineObjectSchema
  [ "chunk_id", "tile_count", "elevation_min", "elevation_max" ]
  [ ("chunk_id", integerSchema)
  , ("tile_count", integerSchema)
  , ("elevation_min", numberSchema)
  , ("elevation_max", numberSchema)
  ]

statsTripletSchema :: Value
statsTripletSchema = inlineObjectSchema
  [ "min", "max", "mean" ]
  [ ("min", numberSchema)
  , ("max", numberSchema)
  , ("mean", numberSchema)
  ]

distributionEntrySchema :: Value
distributionEntrySchema = inlineObjectSchema
  [ "name", "count" ]
  [ ("name", stringSchema)
  , ("count", integerSchema)
  , ("pct", numberSchema)
  ]

terrainFilterSchema :: Value
terrainFilterSchema = inlineObjectSchema
  [ "field", "op", "value" ]
  [ ("field", stringSchema)
  , ("op", enumStringSchema ["eq", "neq", "gt", "gte", "lt", "lte"])
  , ("value", anySchema)
  ]

terrainSearchMatchSchema :: Value
terrainSearchMatchSchema = inlineObjectSchema
  [ "chunk", "tile" ]
  [ ("chunk", integerSchema)
  , ("tile", integerSchema)
  , ("elevation", nullableSchema numberSchema)
  , ("biome", nullableSchema stringSchema)
  , ("terrain_form", nullableSchema stringSchema)
  ]

-- Editor --------------------------------------------------------------------

editorStateResponseSchema :: JsonSchema
editorStateResponseSchema = JsonSchema "EditorStateResponse" editorStateSchema

editorToggleRequestSchema :: JsonSchema
editorToggleRequestSchema = objectSchema "EditorToggleRequest"
  []
  [ ("active", booleanSchema)
  ]

editorToolSetRequestSchema :: JsonSchema
editorToolSetRequestSchema = objectSchema "EditorToolSetRequest"
  [ "tool" ]
  [ ("tool", editorToolSchema)
  ]

editorBrushSetRequestSchema :: JsonSchema
editorBrushSetRequestSchema = objectSchema "EditorBrushSetRequest"
  []
  [ ("radius", integerSchema)
  , ("strength", numberSchema)
  , ("falloff", brushFalloffSchema)
  , ("smooth_passes", integerSchema)
  , ("noise_frequency", numberSchema)
  , ("erode_passes", integerSchema)
  ]

editorBrushStrokeRequestSchema :: JsonSchema
editorBrushStrokeRequestSchema = objectSchema "EditorBrushStrokeRequest"
  [ "q", "r" ]
  [ ("q", integerSchema)
  , ("r", integerSchema)
  ]

editorBrushLineRequestSchema :: JsonSchema
editorBrushLineRequestSchema = objectSchema "EditorBrushLineRequest"
  [ "from_q", "from_r", "to_q", "to_r" ]
  [ ("from_q", integerSchema)
  , ("from_r", integerSchema)
  , ("to_q", integerSchema)
  , ("to_r", integerSchema)
  ]

editorBiomeSetRequestSchema :: JsonSchema
editorBiomeSetRequestSchema = objectSchema "EditorBiomeSetRequest"
  [ "biome" ]
  [ ("biome", anySchema)
  ]

editorFormSetRequestSchema :: JsonSchema
editorFormSetRequestSchema = objectSchema "EditorFormSetRequest"
  [ "form" ]
  [ ("form", anySchema)
  ]

editorHardnessSetRequestSchema :: JsonSchema
editorHardnessSetRequestSchema = objectSchema "EditorHardnessSetRequest"
  [ "hardness" ]
  [ ("hardness", numberSchema)
  ]

editorBrushQueuedResponseSchema :: JsonSchema
editorBrushQueuedResponseSchema = objectSchema "EditorBrushQueuedResponse"
  [ "status", "strokes_queued" ]
  [ ("status", enumStringSchema ["queued"])
  , ("strokes_queued", integerSchema)
  ]

editorQueuedResponseSchema :: JsonSchema
editorQueuedResponseSchema = statusResponseSchema "EditorQueuedResponse"

editorStateSchema :: Value
editorStateSchema = inlineObjectSchema
  [ "active", "tool", "brush", "smooth_passes", "noise_frequency", "stroke_id", "biome", "terrain_form", "hardness_target", "erode_passes" ]
  [ ("active", booleanSchema)
  , ("tool", editorToolSchema)
  , ("brush", inlineObjectSchema ["radius", "strength", "falloff"]
      [ ("radius", integerSchema)
      , ("strength", numberSchema)
      , ("falloff", brushFalloffSchema)
      ])
  , ("smooth_passes", integerSchema)
  , ("noise_frequency", numberSchema)
  , ("flatten_reference", nullableSchema (arraySchema integerSchema))
  , ("stroke_id", integerSchema)
  , ("biome", codedNameSchema)
  , ("terrain_form", codedNameSchema)
  , ("hardness_target", numberSchema)
  , ("erode_passes", integerSchema)
  ]

codedNameSchema :: Value
codedNameSchema = inlineObjectSchema
  [ "name", "code" ]
  [ ("name", stringSchema)
  , ("code", integerSchema)
  ]

-- Pipeline ------------------------------------------------------------------

pipelineGetResponseSchema :: JsonSchema
pipelineGetResponseSchema = objectSchema "PipelineGetResponse"
  [ "stages", "dag", "docs", "diagnostics" ]
  [ ("stages", arraySchema pipelineStageSchema)
  , ("dag", pipelineDagSchema)
  , ("docs", arraySchema pipelineStageDocSchema)
  , ("diagnostics", freeObjectSchema)
  ]

pipelineSetStageEnabledRequestSchema :: JsonSchema
pipelineSetStageEnabledRequestSchema = objectSchema "PipelineSetStageEnabledRequest"
  [ "stage", "enabled" ]
  [ ("stage", stringSchema)
  , ("enabled", booleanSchema)
  ]

pipelineSetStageEnabledResponseSchema :: JsonSchema
pipelineSetStageEnabledResponseSchema = objectSchema "PipelineSetStageEnabledResponse"
  [ "stage", "enabled" ]
  [ ("stage", stringSchema)
  , ("enabled", booleanSchema)
  , ("disabled_closure", arraySchema stringSchema)
  , ("diagnostics", arraySchema stringSchema)
  ]

pipelineStageSchema :: Value
pipelineStageSchema = inlineObjectSchema
  [ "id", "enabled", "source" ]
  [ ("id", stringSchema)
  , ("name", stringSchema)
  , ("enabled", booleanSchema)
  , ("source", enumStringSchema ["builtin", "plugin"])
  , ("status", stringSchema)
  , ("explicitly_disabled", booleanSchema)
  , ("auto_disabled", booleanSchema)
  , ("dependencies", arraySchema stringSchema)
  , ("dependents", arraySchema stringSchema)
  , ("disabled_by", arraySchema stringSchema)
  , ("config", freeObjectSchema)
  , ("output_fields", arraySchema stringSchema)
  , ("output_overlays", arraySchema stringSchema)
  , ("last_run", freeObjectSchema)
  , ("provenance", freeObjectSchema)
  , ("diagnostics", arraySchema stringSchema)
  , ("plugin_insertion", nullableSchema freeObjectSchema)
  , ("plugin_diagnostics", freeObjectSchema)
  , ("doc", pipelineStageDocSchema)
  ]

pipelineDagSchema :: Value
pipelineDagSchema = inlineObjectSchema ["nodes", "edges"]
  [ ("nodes", arraySchema (inlineObjectSchema ["id", "source", "enabled"]
      [ ("id", stringSchema)
      , ("source", enumStringSchema ["builtin", "plugin"])
      , ("enabled", booleanSchema)
      , ("status", stringSchema)
      ]))
  , ("edges", arraySchema (inlineObjectSchema ["from", "to", "kind"]
      [ ("from", stringSchema)
      , ("to", stringSchema)
      , ("kind", enumStringSchema ["requires", "insert_after"])
      ]))
  ]

pipelineStageDocSchema :: Value
pipelineStageDocSchema = inlineObjectSchema ["id", "title", "summary", "dependencies", "config_keys", "output_fields"]
  [ ("id", stringSchema)
  , ("title", stringSchema)
  , ("summary", stringSchema)
  , ("stage_seed_tag", stringSchema)
  , ("dependencies", arraySchema stringSchema)
  , ("config_keys", arraySchema stringSchema)
  , ("output_fields", arraySchema stringSchema)
  , ("output_overlays", arraySchema stringSchema)
  , ("diagnostic_hints", arraySchema stringSchema)
  ]

-- Plugins -------------------------------------------------------------------

pluginListResponseSchema :: JsonSchema
pluginListResponseSchema = objectSchema "PluginListResponse"
  [ "plugin_count", "plugins" ]
  [ ("plugin_count", integerSchema)
  , ("plugins", arraySchema pluginSummarySchema)
  ]

pluginSetEnabledRequestSchema :: JsonSchema
pluginSetEnabledRequestSchema = objectSchema "PluginSetEnabledRequest"
  [ "name", "enabled" ]
  [ ("name", stringSchema)
  , ("enabled", booleanSchema)
  ]

pluginSetEnabledResponseSchema :: JsonSchema
pluginSetEnabledResponseSchema = objectSchema "PluginSetEnabledResponse"
  [ "name", "enabled" ]
  [ ("name", stringSchema)
  , ("enabled", booleanSchema)
  ]

pluginSetParamRequestSchema :: JsonSchema
pluginSetParamRequestSchema = objectSchema "PluginSetParamRequest"
  [ "plugin", "param", "value" ]
  [ ("plugin", stringSchema)
  , ("param", stringSchema)
  , ("value", anySchema)
  ]

pluginSetParamResponseSchema :: JsonSchema
pluginSetParamResponseSchema = objectSchema "PluginSetParamResponse"
  [ "plugin", "param", "value" ]
  [ ("plugin", stringSchema)
  , ("param", stringSchema)
  , ("value", anySchema)
  ]

pluginSummarySchema :: Value
pluginSummarySchema = inlineObjectSchema
  [ "name"
  , "version"
  , "description"
  , "status"
  , "diagnostic_status"
  , "status_detail"
  , "lifecycle"
  , "start_policy"
  , "restart_attempts"
  , "restart_count"
  , "enabled"
  , "params"
  , "param_specs"
  , "pid"
  , "endpoint_kind"
  , "protocol_version"
  , "uptime_seconds"
  , "last_error"
  , "dependencies"
  , "resources"
  , "resource_count"
  , "data_resources"
  , "external_data_sources"
  , "external_data_source_count"
  , "external_data_source_failures"
  , "capabilities"
  , "has_generator"
  , "has_simulation"
  , "has_simulation_declaration"
  , "logs"
  , "diagnostic_lines"
  ]
  [ ("name", stringSchema)
  , ("version", stringSchema)
  , ("description", stringSchema)
  , ("status", stringSchema)
  , ("diagnostic_status", pluginDiagnosticStatusSchema)
  , ("status_detail", stringSchema)
  , ("lifecycle", pluginLifecycleSchema)
  , ("start_policy", pluginStartPolicySchema)
  , ("restart_attempts", integerSchema)
  , ("restart_count", integerSchema)
  , ("enabled", booleanSchema)
  , ("params", freeObjectSchema)
  , ("param_specs", arraySchema pluginParamSpecSchema)
  , ("pid", nullableSchema stringSchema)
  , ("endpoint_kind", nullableSchema (enumStringSchema ["unix", "named-pipe"]))
  , ("protocol_version", nullableSchema integerSchema)
  , ("uptime_seconds", nullableSchema numberSchema)
  , ("last_error", nullableSchema stringSchema)
  , ("dependencies", arraySchema pluginDependencySchema)
  , ("resources", arraySchema stringSchema)
  , ("resource_count", integerSchema)
  , ("data_resources", arraySchema dataResourceSchema)
  , ("external_data_sources", arraySchema pluginExternalDataSourceSchema)
  , ("external_data_source_count", integerSchema)
  , ("external_data_source_failures", integerSchema)
  , ("capabilities", arraySchema stringSchema)
  , ("has_generator", booleanSchema)
  , ("has_simulation", describedSchema "Backward-compatible alias for has_simulation_declaration; true when this plugin manifest declares a plugin simulation node, not when the host built-in weather simulation exists." booleanSchema)
  , ("has_simulation_declaration", describedSchema "True when this plugin manifest declares a plugin simulation node. Host built-ins such as weather are not plugin declarations." booleanSchema)
  , ("simulation", describedSchema "Backward-compatible alias for simulation_declaration. Describes the plugin manifest's simulation declaration, not the host built-in weather simulation node." (nullableSchema pluginSimulationSchema))
  , ("simulation_declaration", describedSchema "Plugin manifest simulation declaration metadata. Dependencies are simulation node IDs and may reference host built-ins such as weather." (nullableSchema pluginSimulationSchema))
  , ("logs", arraySchema stringSchema)
  , ("diagnostic_lines", arraySchema stringSchema)
  ]

pluginDiagnosticStatusSchema :: Value
pluginDiagnosticStatusSchema = enumStringSchema
  [ "Ready"
  , "WaitingForDependencies"
  , "Degraded"
  , "Disabled"
  , "Failed"
  ]

pluginDependencySchema :: Value
pluginDependencySchema = inlineObjectSchema
  [ "kind", "name", "status", "required", "detail" ]
  [ ("kind", stringSchema)
  , ("name", stringSchema)
  , ("status", enumStringSchema ["available", "waiting"])
  , ("required", booleanSchema)
  , ("detail", nullableSchema stringSchema)
  ]

pluginExternalDataSourceSchema :: Value
pluginExternalDataSourceSchema = inlineObjectSchema
  [ "role"
  , "plugin"
  , "provider"
  , "consumer"
  , "source"
  , "resource"
  , "label"
  , "access"
  , "capabilities"
  , "resources"
  , "status"
  , "status_summary"
  , "status_detail"
  , "availability"
  , "resource_availability"
  , "config_refs"
  , "grants"
  , "grant_summary"
  , "ownership"
  , "host_role"
  , "lifecycle_boundary"
  , "relationship"
  ]
  [ ("role", enumStringSchema ["provider", "consumer"])
  , ("plugin", stringSchema)
  , ("provider", stringSchema)
  , ("consumer", stringSchema)
  , ("source", stringSchema)
  , ("resource", stringSchema)
  , ("grant", nullableSchema stringSchema)
  , ("label", stringSchema)
  , ("kind", stringSchema)
  , ("description", stringSchema)
  , ("required", nullableSchema booleanSchema)
  , ("access", arraySchema externalDataAccessSchema)
  , ("capabilities", arraySchema externalDataCapabilitySchema)
  , ("resources", arraySchema stringSchema)
  , ("status", externalDataStatusStateSchema)
  , ("status_summary", stringSchema)
  , ("status_detail", externalDataStatusDetailSchema)
  , ("availability", externalDataAvailabilitySchema)
  , ("health", externalDataHealthSchema)
  , ("access_mode", externalDataAccessModeSchema)
  , ("failure_reason", stringSchema)
  , ("resource_availability", arraySchema externalDataResourceAvailabilitySchema)
  , ("config_refs", arraySchema externalDataConfigRefSchema)
  , ("grants", arraySchema externalDataGrantSchema)
  , ("grant_summary", inlineObjectSchema ["data_read", "data_write"]
      [ ("data_read", booleanSchema)
      , ("data_write", booleanSchema)
      ])
  , ("ownership", enumStringSchema ["provider-owned"])
  , ("host_role", enumStringSchema ["broker", "consumer-router"])
  , ("lifecycle_boundary", enumStringSchema ["external-provider-managed"])
  , ("relationship", inlineObjectSchema ["role", "plugin", "provider", "consumer", "source"]
      [ ("role", enumStringSchema ["provider", "consumer"])
      , ("plugin", stringSchema)
      , ("provider", stringSchema)
      , ("consumer", stringSchema)
      , ("source", stringSchema)
      , ("grant", nullableSchema stringSchema)
      ])
  ]

externalDataGrantSchema :: Value
externalDataGrantSchema = inlineObjectSchema
  [ "name"
  , "access"
  , "capabilities"
  , "resources"
  , "status"
  , "status_summary"
  , "status_detail"
  , "availability"
  , "resource_availability"
  , "config_refs"
  ]
  [ ("name", stringSchema)
  , ("access", arraySchema externalDataAccessSchema)
  , ("capabilities", arraySchema externalDataCapabilitySchema)
  , ("resources", arraySchema stringSchema)
  , ("status", externalDataStatusStateSchema)
  , ("status_summary", stringSchema)
  , ("status_detail", externalDataStatusDetailSchema)
  , ("availability", externalDataAvailabilitySchema)
  , ("health", externalDataHealthSchema)
  , ("access_mode", externalDataAccessModeSchema)
  , ("failure_reason", stringSchema)
  , ("resource_availability", arraySchema externalDataResourceAvailabilitySchema)
  , ("config_refs", arraySchema externalDataConfigRefSchema)
  ]

externalDataResourceAvailabilitySchema :: Value
externalDataResourceAvailabilitySchema = inlineObjectSchema
  [ "resource", "available", "status" ]
  [ ("resource", stringSchema)
  , ("available", booleanSchema)
  , ("status", externalDataAvailabilitySchema)
  , ("detail", stringSchema)
  ]

externalDataConfigRefSchema :: Value
externalDataConfigRefSchema = inlineObjectSchema
  [ "name", "origin", "required", "key_present" ]
  [ ("name", stringSchema)
  , ("origin", enumStringSchema ["user", "provider", "environment", "deployment"])
  , ("required", booleanSchema)
  , ("key_present", booleanSchema)
  , ("compatibility", stringSchema)
  ]

externalDataStatusDetailSchema :: Value
externalDataStatusDetailSchema = inlineObjectSchema
  [ "state" ]
  [ ("state", externalDataStatusStateSchema)
  , ("message", stringSchema)
  , ("providerId", stringSchema)
  , ("availability", externalDataAvailabilitySchema)
  , ("health", externalDataHealthSchema)
  , ("accessMode", externalDataAccessModeSchema)
  , ("capabilityScope", arraySchema externalDataCapabilitySchema)
  , ("version", stringSchema)
  , ("compatibility", stringSchema)
  ]

externalDataStatusStateSchema :: Value
externalDataStatusStateSchema = enumStringSchema
  [ "unknown", "unconfigured", "ready", "degraded", "unavailable" ]

externalDataAvailabilitySchema :: Value
externalDataAvailabilitySchema = enumStringSchema
  [ "unknown", "available", "degraded", "unavailable", "unconfigured" ]

externalDataHealthSchema :: Value
externalDataHealthSchema = enumStringSchema
  [ "unknown", "healthy", "degraded", "unhealthy" ]

externalDataAccessModeSchema :: Value
externalDataAccessModeSchema = enumStringSchema
  [ "read_only", "read_write", "admin", "disabled", "provider_managed" ]

externalDataAccessSchema :: Value
externalDataAccessSchema = enumStringSchema ["read", "write", "admin"]

externalDataCapabilitySchema :: Value
externalDataCapabilitySchema = enumStringSchema ["query", "mutate", "subscribe", "migrate", "health"]

pluginStartPolicySchema :: Value
pluginStartPolicySchema = inlineObjectSchema
  [ "auto_start"
  , "restart_mode"
  , "max_restarts"
  , "restart_window_ms"
  , "startup_timeout_ms"
  , "request_timeout_ms"
  , "shutdown_timeout_ms"
  , "backoff_ms"
  ]
  [ ("auto_start", booleanSchema)
  , ("restart_mode", enumStringSchema ["never", "on_failure", "always"])
  , ("max_restarts", integerSchema)
  , ("restart_window_ms", integerSchema)
  , ("startup_timeout_ms", integerSchema)
  , ("request_timeout_ms", integerSchema)
  , ("shutdown_timeout_ms", integerSchema)
  , ("backoff_ms", integerSchema)
  ]

pluginParamSpecSchema :: Value
pluginParamSpecSchema = inlineObjectSchema
  [ "name", "label", "type", "default", "tooltip" ]
  [ ("name", stringSchema)
  , ("label", stringSchema)
  , ("type", stringSchema)
  , ("default", anySchema)
  , ("tooltip", nullableSchema stringSchema)
  ]

pluginSimulationSchema :: Value
pluginSimulationSchema = inlineObjectSchema
  [ "dependencies" ]
  [ ("dependencies", describedSchema "Simulation node IDs that must tick before this plugin declaration. Entries may reference host built-in simulation nodes such as weather." (arraySchema stringSchema))
  , ("dependency_kind", enumStringSchema ["simulation_node_ids"])
  , ("interval_ticks", diagnosticScheduleIntervalSchema)
  , ("phase_ticks", integerMinimumSchema 0)
  , ("catch_up", simulationCatchUpSchema)
  ]

pluginLifecycleSchema :: Value
pluginLifecycleSchema = inlineObjectSchema
  [ "state"
  , "updated_at"
  , "reason"
  , "error_code"
  , "error_message"
  , "blocking_dependency"
  , "process_id"
  , "protocol_version"
  , "resources"
  , "state_leases"
  ]
  [ ("state", enumStringSchema
      [ "discovered"
      , "starting"
      , "ready"
      , "degraded"
      , "stopping"
      , "stopped"
      , "failed"
      ])
  , ("updated_at", stringSchema)
  , ("reason", nullableSchema stringSchema)
  , ("error_code", nullableSchema stringSchema)
  , ("error_message", nullableSchema stringSchema)
  , ("blocking_dependency", nullableSchema stringSchema)
  , ("process_id", nullableSchema stringSchema)
  , ("protocol_version", nullableSchema integerSchema)
  , ("resources", arraySchema stringSchema)
  , ("state_leases", arraySchema pluginStateLeaseSchema)
  ]

pluginStateLeaseSchema :: Value
pluginStateLeaseSchema = inlineObjectSchema
  [ "holder", "state", "acquired_at", "expires_at" ]
  [ ("holder", stringSchema)
  , ("state", enumStringSchema
      [ "discovered"
      , "starting"
      , "ready"
      , "degraded"
      , "stopping"
      , "stopped"
      , "failed"
      ])
  , ("acquired_at", stringSchema)
  , ("expires_at", nullableSchema stringSchema)
  ]

-- Data resources -------------------------------------------------------------

dataPluginsListResponseSchema :: JsonSchema
dataPluginsListResponseSchema = objectSchema "DataPluginsListResponse"
  [ "plugins", "count" ]
  [ ("plugins", arraySchema dataPluginSummarySchema)
  , ("count", integerSchema)
  ]

dataResourcesListResponseSchema :: JsonSchema
dataResourcesListResponseSchema = objectSchema "DataResourcesListResponse"
  [ "plugin", "resources" ]
  [ ("plugin", stringSchema)
  , ("resources", arraySchema dataResourceSchema)
  , ("external_data_sources", arraySchema pluginExternalDataSourceSchema)
  , ("external_data_source_count", integerSchema)
  , ("external_data_source_failures", integerSchema)
  ]

dataRecordsListResponseSchema :: JsonSchema
dataRecordsListResponseSchema = objectSchema "DataRecordsListResponse"
  [ "plugin", "resource", "records", "total_count", "count" ]
  [ ("plugin", stringSchema)
  , ("resource", stringSchema)
  , ("records", arraySchema freeObjectSchema)
  , ("total_count", nullableSchema integerSchema)
  , ("count", integerSchema)
  , ("page_size", nullableSchema integerSchema)
  , ("page_offset", nullableSchema integerSchema)
  ]

dataRecordGetRequestSchema :: JsonSchema
dataRecordGetRequestSchema = dataRecordKeyRequestSchema "DataRecordGetRequest"

dataRecordGetResponseSchema :: JsonSchema
dataRecordGetResponseSchema = objectSchema "DataRecordGetResponse"
  [ "plugin", "resource" ]
  [ ("plugin", stringSchema)
  , ("resource", stringSchema)
  , ("record", freeObjectSchema)
  , ("records", arraySchema freeObjectSchema)
  , ("count", integerSchema)
  ]

dataRecordCreateRequestSchema :: JsonSchema
dataRecordCreateRequestSchema = dataRecordFieldsRequestSchema "DataRecordCreateRequest" False

dataRecordCreateResponseSchema :: JsonSchema
dataRecordCreateResponseSchema = objectSchema "DataRecordCreateResponse"
  [ "plugin", "resource", "created" ]
  [ ("plugin", stringSchema)
  , ("resource", stringSchema)
  , ("created", booleanSchema)
  , ("record", nullableSchema freeObjectSchema)
  ]

dataRecordUpdateRequestSchema :: JsonSchema
dataRecordUpdateRequestSchema = dataRecordFieldsRequestSchema "DataRecordUpdateRequest" True

dataRecordUpdateResponseSchema :: JsonSchema
dataRecordUpdateResponseSchema = objectSchema "DataRecordUpdateResponse"
  [ "plugin", "resource", "updated" ]
  [ ("plugin", stringSchema)
  , ("resource", stringSchema)
  , ("updated", booleanSchema)
  , ("record", nullableSchema freeObjectSchema)
  ]

dataRecordDeleteRequestSchema :: JsonSchema
dataRecordDeleteRequestSchema = dataRecordKeyRequestSchema "DataRecordDeleteRequest"

dataRecordDeleteResponseSchema :: JsonSchema
dataRecordDeleteResponseSchema = objectSchema "DataRecordDeleteResponse"
  [ "plugin", "resource", "deleted" ]
  [ ("plugin", stringSchema)
  , ("resource", stringSchema)
  , ("deleted", booleanSchema)
  ]

dataStateResponseSchema :: JsonSchema
dataStateResponseSchema = objectSchema "DataStateResponse"
  [ "record_count", "total_count", "page_offset", "loading", "edit_mode", "create_mode", "has_selection" ]
  [ ("selected_plugin", nullableSchema stringSchema)
  , ("selected_resource", nullableSchema stringSchema)
  , ("record_count", integerSchema)
  , ("total_count", nullableSchema integerSchema)
  , ("page_offset", integerSchema)
  , ("loading", booleanSchema)
  , ("edit_mode", booleanSchema)
  , ("create_mode", booleanSchema)
  , ("has_selection", booleanSchema)
  , ("selected_key", anySchema)
  , ("external_data_sources", arraySchema pluginExternalDataSourceSchema)
  , ("external_data_source_count", integerSchema)
  , ("external_data_source_failures", integerSchema)
  ]

dataPluginSummarySchema :: Value
dataPluginSummarySchema = inlineObjectSchema
  [ "plugin", "resources" ]
  [ ("plugin", stringSchema)
  , ("resources", arraySchema stringSchema)
  , ("external_data_sources", arraySchema pluginExternalDataSourceSchema)
  , ("external_data_source_count", integerSchema)
  , ("external_data_source_failures", integerSchema)
  ]

dataResourceSchema :: Value
dataResourceSchema = inlineObjectSchema
  [ "schema_version", "resource_version", "name", "label", "hex_bound", "key_field", "fields", "operations", "pagination" ]
  [ ("schema_version", integerSchema)
  , ("resource_version", integerSchema)
  , ("name", stringSchema)
  , ("label", stringSchema)
  , ("hex_bound", booleanSchema)
  , ("key_field", stringSchema)
  , ("overlay", nullableSchema stringSchema)
  , ("fields", arraySchema dataFieldSchema)
  , ("operations", dataOperationsSchema)
  , ("pagination", dataPaginationSchema)
  ]

dataFieldSchema :: Value
dataFieldSchema = inlineObjectSchema
  [ "name", "type", "label", "editable", "default" ]
  [ ("name", stringSchema)
  , ("type", dataFieldTypeSchema)
  , ("label", stringSchema)
  , ("editable", booleanSchema)
  , ("default", anySchema)
  ]

dataFieldTypeSchema :: Value
dataFieldTypeSchema = object
  [ "description" .= ("Scalar field type string or composite field type object." :: Text)
  , "oneOf" .=
      [ enumStringSchema ["text", "int", "float", "double", "bool", "fixed2", "fixed3", "fixed4"]
      , freeObjectSchema
      ]
  ]

dataOperationsSchema :: Value
dataOperationsSchema = inlineObjectSchema
  [ "list", "get", "create", "update", "delete", "query_by_hex", "query_by_field", "sort", "filter", "page" ]
  [ ("list", booleanSchema)
  , ("get", booleanSchema)
  , ("create", booleanSchema)
  , ("update", booleanSchema)
  , ("delete", booleanSchema)
  , ("query_by_hex", booleanSchema)
  , ("query_by_field", booleanSchema)
  , ("sort", booleanSchema)
  , ("filter", booleanSchema)
  , ("page", booleanSchema)
  ]

dataPaginationSchema :: Value
dataPaginationSchema = inlineObjectSchema
  [ "default_page_size", "max_page_size", "default_page_offset" ]
  [ ("default_page_size", integerSchema)
  , ("max_page_size", integerSchema)
  , ("default_page_offset", integerSchema)
  ]

dataRecordKeyRequestSchema :: Text -> JsonSchema
dataRecordKeyRequestSchema name = objectSchema name
  [ "plugin", "resource", "key" ]
  [ ("plugin", stringSchema)
  , ("resource", stringSchema)
  , ("key", anySchema)
  ]

dataRecordFieldsRequestSchema :: Text -> Bool -> JsonSchema
dataRecordFieldsRequestSchema name includeKey = objectSchema name required properties
  where
    required
      | includeKey = ["plugin", "resource", "key", "fields"]
      | otherwise = ["plugin", "resource", "fields"]
    properties =
      [ ("plugin", stringSchema)
      , ("resource", stringSchema)
      ]
      <> [ ("key", anySchema) | includeKey ]
      <> [ ("fields", freeObjectSchema) ]

-- Simulation ----------------------------------------------------------------

simulationStateResponseSchema :: JsonSchema
simulationStateResponseSchema = objectSchema "SimulationStateResponse"
  [ "auto_tick", "tick_rate", "tick_count" ]
  [ ("auto_tick", booleanSchema)
  , ("tick_rate", numberSchema)
  , ("tick_count", integerSchema)
  , ("dag_available", booleanSchema)
  , ("dag_node_count", integerSchema)
  , ("pending_tick", nullableSchema integerSchema)
  , ("last_tick_log", nullableSchema simulationTickLogSchema)
  , ("async_status", asyncStatusSchema)
  ]

simulationDagResponseSchema :: JsonSchema
simulationDagResponseSchema = objectSchema "SimulationDagResponse"
  [ "available", "world_bound", "overlay_names", "nodes", "levels", "terrain_writers" ]
  [ ("available", booleanSchema)
  , ("world_bound", describedSchema "True when the Simulation actor currently has a world bound. Plugin declarations can be plan-eligible while still waiting for actor binding." booleanSchema)
  , ("overlay_names", describedSchema "Overlay names in the bound world used to evaluate plugin simulation declaration eligibility." (arraySchema stringSchema))
  , ("nodes", describedSchema "Actor-bound simulation DAG nodes. Built-in weather appears here with kind=builtin and plugin=null; plugin declarations appear here only after they are bound into the actor DAG." (arraySchema simulationDagNodeSchema))
  , ("node_count", describedSchema "Count of actor-bound simulation DAG nodes in nodes." integerSchema)
  , ("levels", arraySchema (arraySchema stringSchema))
  , ("terrain_writers", arraySchema stringSchema)
  , ("last_tick", integerSchema)
  , ("pending_tick", nullableSchema integerSchema)
  , ("tick_logs", arraySchema simulationTickLogSchema)
  , ("weather_node_status", nullableSchema weatherNodeScheduleDiagnosticSchema)
  , ("last_weather_publication", nullableSchema weatherPublicationDiagnosticSchema)
  , ("cloud_delta", nullableSchema cloudDeltaSummarySchema)
  , ("plugin_nodes", describedSchema "Backward-compatible alias for plugin_simulation_declarations; these are plugin declaration diagnostics, not the authoritative actor-bound DAG node list." (arraySchema simulationPluginDeclarationSchema))
  , ("plugin_node_count", describedSchema "Backward-compatible count of plugin simulation declarations, not actor-bound plugin DAG nodes." integerSchema)
  , ("plugin_declarations", describedSchema "Alias for plugin_simulation_declarations." (arraySchema simulationPluginDeclarationSchema))
  , ("plugin_declaration_count", describedSchema "Alias for plugin_simulation_declaration_count." integerSchema)
  , ("plugin_simulation_declarations", describedSchema "Diagnostics for plugin manifest simulation declarations. Bound/executable fields distinguish plan eligibility from actor binding." (arraySchema simulationPluginDeclarationSchema))
  , ("plugin_simulation_declaration_count", describedSchema "Count of plugin_simulation_declarations." integerSchema)
  ]

simulationAutoTickRequestSchema :: JsonSchema
simulationAutoTickRequestSchema = objectSchema "SimulationAutoTickRequest"
  [ "enabled" ]
  [ ("enabled", booleanSchema)
  , ("rate", numberSchema)
  ]

simulationAutoTickResponseSchema :: JsonSchema
simulationAutoTickResponseSchema = objectSchema "SimulationAutoTickResponse"
  [ "auto_tick" ]
  [ ("auto_tick", booleanSchema)
  , ("rate", nullableSchema numberSchema)
  ]

simulationTickRequestSchema :: JsonSchema
simulationTickRequestSchema = objectSchema "SimulationTickRequest"
  []
  [ ("count", integerSchema)
  ]

simulationTickResponseSchema :: JsonSchema
simulationTickResponseSchema = objectSchema "SimulationTickResponse"
  [ "requested_ticks", "target_tick" ]
  [ ("requested_ticks", integerSchema)
  , ("target_tick", integerSchema)
  ]

simulationDagNodeSchema :: Value
simulationDagNodeSchema = inlineObjectSchema
  [ "id", "kind", "overlay", "dependencies", "writes_terrain", "status" ]
  simulationDagNodeProperties

simulationPluginDeclarationSchema :: Value
simulationPluginDeclarationSchema = inlineObjectSchema
  [ "id", "kind", "plugin", "overlay", "dependencies", "writes_terrain", "status", "bound", "plan_executable" ]
  (simulationDagNodeProperties <>
    [ ("declaration_status", describedSchema "Status from the plugin simulation declaration plan before actor binding is considered." stringSchema)
    , ("declaration_status_detail", nullableSchema stringSchema)
    , ("actor_status", describedSchema "Current actor-bound DAG node status when bound; null when the declaration is not in the actor DAG." (nullableSchema stringSchema))
    , ("actor_status_detail", nullableSchema stringSchema)
    , ("enabled", describedSchema "False when disabled by user or plugin start policy." booleanSchema)
    , ("eligible", describedSchema "Alias for plan_executable." booleanSchema)
    , ("eligible_for_binding", describedSchema "Alias for plan_executable; true when the declaration can be bound into the actor DAG plan." booleanSchema)
    , ("plan_executable", describedSchema "True when plugin-manager diagnostics deem this declaration eligible for an executable Simulation actor node plan." booleanSchema)
    , ("executable", describedSchema "Backward-compatible actor-bound executable flag; equivalent to bound/actor_bound, not plan eligibility." booleanSchema)
    , ("bound", describedSchema "True when this plugin declaration currently has an actor-bound DAG node in nodes[]." booleanSchema)
    , ("actor_bound", describedSchema "Alias for bound." booleanSchema)
    ])

simulationDagNodeProperties :: [(Text, Value)]
simulationDagNodeProperties =
  [ ("id", describedSchema "Simulation node ID. Host built-in weather uses id=weather." stringSchema)
  , ("kind", describedSchema "Actor node provenance: builtin for host nodes such as weather, plugin for bound plugin declarations." (enumStringSchema ["builtin", "plugin"]))
  , ("plugin", describedSchema "Owning plugin for plugin nodes; null for host built-ins such as weather." (nullableSchema stringSchema))
  , ("overlay", stringSchema)
  , ("dependencies", describedSchema "Simulation node ID dependencies. Values may reference host built-ins such as weather." (arraySchema stringSchema))
  , ("writes_terrain", booleanSchema)
  , ("status", stringSchema)
  , ("status_detail", nullableSchema stringSchema)
  , ("interval_ticks", nullableSchema diagnosticScheduleIntervalSchema)
  , ("phase_ticks", nullableSchema (integerMinimumSchema 0))
  , ("catch_up", nullableSchema simulationCatchUpSchema)
  , ("last_fire_tick", nullableSchema (integerMinimumSchema 0))
  , ("next_fire_tick", nullableSchema (integerMinimumSchema 0))
  , ("due", nullableSchema booleanSchema)
  ]

diagnosticScheduleIntervalSchema :: Value
-- Diagnostic endpoints echo raw manifest declarations so invalid interval 0
-- values remain visible while validation still rejects them before DAG use.
diagnosticScheduleIntervalSchema = integerMinimumSchema 0

simulationCatchUpSchema :: Value
simulationCatchUpSchema = enumStringSchema ["run_once_if_due", "skip_missed"]

weatherNodeScheduleDiagnosticSchema :: Value
weatherNodeScheduleDiagnosticSchema = inlineObjectSchema
  [ "status" ]
  [ ("status", enumStringSchema ["due", "skipped", "completed", "ready", "running", "idle", "failed"])
  , ("next_fire_tick", nullableSchema (integerMinimumSchema 0))
  , ("cadence_ticks", nullableSchema diagnosticScheduleIntervalSchema)
  , ("skip_reason", nullableSchema stringSchema)
  ]

weatherPublicationDiagnosticSchema :: Value
weatherPublicationDiagnosticSchema = inlineObjectSchema
  [ "tick", "weather_version_before", "weather_version_after", "published_weather_version", "publication_kind" ]
  [ ("tick", integerSchema)
  , ("world_time", inlineObjectSchema
      [ "tick", "tick_rate" ]
      [ ("tick", integerSchema)
      , ("tick_rate", numberSchema)
      ])
  , ("weather_version_before", integerSchema)
  , ("weather_version_after", integerSchema)
  , ("published_weather_version", integerSchema)
  , ("publication_kind", enumStringSchema ["manual", "auto_immediate", "auto_coalesced", "flush"])
  , ("weather_changed", booleanSchema)
  , ("data_published", booleanSchema)
  , ("publication_pending", booleanSchema)
  , ("atlas_work_enqueued", booleanSchema)
  , ("atlas_active_weather_view", nullableSchema (enumStringSchema ["weather", "cloud"]))
  ]

cloudDeltaSummarySchema :: Value
cloudDeltaSummarySchema = inlineObjectSchema
  [ "changed", "cloud_cover", "cloud_water", "precip" ]
  [ ("changed", booleanSchema)
  , ("compared_chunks", integerSchema)
  , ("compared_samples", integerSchema)
  , ("cloud_cover", cloudDeltaMetricSchema)
  , ("cloud_water", cloudDeltaMetricSchema)
  , ("precip", cloudDeltaMetricSchema)
  ]

cloudDeltaMetricSchema :: Value
cloudDeltaMetricSchema = inlineObjectSchema
  [ "min_delta", "max_delta", "mean_abs_delta" ]
  [ ("min_delta", numberSchema)
  , ("max_delta", numberSchema)
  , ("mean_abs_delta", numberSchema)
  ]

simulationTickLogSchema :: Value
simulationTickLogSchema = inlineObjectSchema
  [ "tick", "status", "message" ]
  [ ("tick", integerSchema)
  , ("node_id", nullableSchema stringSchema)
  , ("status", enumStringSchema ["deferred", "running", "completed", "skipped", "failed"])
  , ("message", stringSchema)
  , ("elapsed_ms", nullableSchema numberSchema)
  ]

asyncStatusSchema :: Value
asyncStatusSchema = inlineObjectSchema
  [ "name", "phase", "active" ]
  [ ("name", stringSchema)
  , ("phase", enumStringSchema ["idle", "queued", "running", "succeeded", "failed", "unavailable"])
  , ("active", booleanSchema)
  , ("current", nullableSchema integerSchema)
  , ("total", nullableSchema integerSchema)
  , ("message", nullableSchema stringSchema)
  ]

-- Logs and screenshots -------------------------------------------------------

logGetResponseSchema :: JsonSchema
logGetResponseSchema = objectSchema "LogGetResponse"
  [ "count", "total", "entries" ]
  [ ("count", integerSchema)
  , ("total", integerSchema)
  , ("entries", arraySchema logEntrySchema)
  ]

screenshotTakeRequestSchema :: JsonSchema
screenshotTakeRequestSchema = objectSchema "ScreenshotTakeRequest"
  []
  [ ("path", stringSchema)
  ]

screenshotTakeResponseSchema :: JsonSchema
screenshotTakeResponseSchema = objectSchema "ScreenshotTakeResponse"
  [ "image_base64", "format" ]
  [ ("image_base64", stringSchema)
  , ("format", enumStringSchema ["png"])
  , ("source", stringSchema)
  , ("saved_path", stringSchema)
  ]

logEntrySchema :: Value
logEntrySchema = inlineObjectSchema
  [ "level", "message" ]
  [ ("level", logLevelSchema)
  , ("message", stringSchema)
  ]

-- UI, camera, overlays, widgets, and dialogs --------------------------------

uiSeedSetRequestSchema :: JsonSchema
uiSeedSetRequestSchema = objectSchema "UiSeedSetRequest"
  [ "seed" ]
  [ ("seed", integerSchema)
  ]

uiSeedSetResponseSchema :: JsonSchema
uiSeedSetResponseSchema = objectSchema "UiSeedSetResponse"
  [ "seed" ]
  [ ("seed", integerSchema)
  ]

uiViewModeSetRequestSchema :: JsonSchema
uiViewModeSetRequestSchema = objectSchema "UiViewModeSetRequest"
  [ "mode" ]
  [ ("mode", viewModeSchema)
  , ("basis", temporalBasisSchema)
  , ("temporal_basis", temporalBasisSchema)
  , ("field_index", integerSchema)
  ]

uiViewModeSetResponseSchema :: JsonSchema
uiViewModeSetResponseSchema = objectSchema "UiViewModeSetResponse"
  [ "view_mode" ]
  [ ("view_mode", viewModeSchema)
  , ("temporal_basis", nullableSchema temporalBasisSchema)
  , ("source_kind", nullableSchema sourceKindSchema)
  , ("view", layeredViewSelectionSchema)
  , ("base_mode", baseViewModeSchema)
  , ("overlay_mode", nullableSchema overlayModeSchema)
  , ("plugin_overlay", nullableSchema stringSchema)
  , ("weather_basis", weatherBasisSchema)
  , ("overlay_opacity", numberSchema)
  , ("legacy_view_mode", nullableSchema viewModeSchema)
  ]

uiViewSetRequestSchema :: JsonSchema
uiViewSetRequestSchema = objectSchema "UiViewSetRequest"
  []
  [ ("base_mode", baseViewModeSchema)
  , ("base", baseViewModeSchema)
  , ("overlay_mode", nullableSchema overlayModeSchema)
  , ("overlay", nullableSchema overlayModeSchema)
  , ("plugin_overlay", nullableSchema stringSchema)
  , ("weather_basis", weatherBasisSchema)
  , ("basis", weatherBasisSchema)
  , ("temporal_basis", temporalBasisSchema)
  , ("overlay_opacity", numberSchema)
  , ("field_index", integerSchema)
  , ("overlay_field", nullableSchema integerSchema)
  ]

uiViewSetResponseSchema :: JsonSchema
uiViewSetResponseSchema = objectSchema "UiViewSetResponse"
  [ "view" ]
  [ ("view", layeredViewSelectionSchema)
  , ("view_mode", nullableSchema viewModeSchema)
  , ("base_mode", baseViewModeSchema)
  , ("overlay_mode", nullableSchema overlayModeSchema)
  , ("plugin_overlay", nullableSchema stringSchema)
  , ("overlay_field", nullableSchema integerSchema)
  , ("weather_basis", weatherBasisSchema)
  , ("overlay_opacity", numberSchema)
  , ("legacy_view_mode", nullableSchema viewModeSchema)
  ]

uiConfigTabSetRequestSchema :: JsonSchema
uiConfigTabSetRequestSchema = objectSchema "UiConfigTabSetRequest"
  [ "tab" ]
  [ ("tab", configTabSchema)
  ]

uiConfigTabSetResponseSchema :: JsonSchema
uiConfigTabSetResponseSchema = objectSchema "UiConfigTabSetResponse"
  [ "config_tab" ]
  [ ("config_tab", configTabSchema)
  ]

uiSelectHexRequestSchema :: JsonSchema
uiSelectHexRequestSchema = objectSchema "UiSelectHexRequest"
  []
  [ ("q", integerSchema)
  , ("r", integerSchema)
  ]

uiSelectHexResponseSchema :: JsonSchema
uiSelectHexResponseSchema = objectSchema "UiSelectHexResponse"
  [ "selected" ]
  [ ("q", integerSchema)
  , ("r", integerSchema)
  , ("selected", booleanSchema)
  ]

uiOverlaySetRequestSchema :: JsonSchema
uiOverlaySetRequestSchema = objectSchema "UiOverlaySetRequest"
  [ "overlay" ]
  [ ("overlay", stringSchema)
  , ("field_index", integerSchema)
  ]

uiOverlaySetResponseSchema :: JsonSchema
uiOverlaySetResponseSchema = objectSchema "UiOverlaySetResponse"
  [ "overlay", "field_index", "view_mode" ]
  [ ("overlay", stringSchema)
  , ("field_index", integerSchema)
  , ("view_mode", stringSchema)
  ]

uiOverlayFieldsResponseSchema :: JsonSchema
uiOverlayFieldsResponseSchema = objectSchema "UiOverlayFieldsResponse"
  [ "field_count", "fields" ]
  [ ("field_count", integerSchema)
  , ("fields", arraySchema overlayFieldSchema)
  ]

uiOverlayCycleRequestSchema :: JsonSchema
uiOverlayCycleRequestSchema = directionRequestSchema "UiOverlayCycleRequest"

uiOverlayCycleResponseSchema :: JsonSchema
uiOverlayCycleResponseSchema = objectSchema "UiOverlayCycleResponse"
  [ "view_mode" ]
  [ ("view_mode", stringSchema)
  , ("overlay", nullableSchema stringSchema)
  ]

uiOverlayFieldCycleRequestSchema :: JsonSchema
uiOverlayFieldCycleRequestSchema = directionRequestSchema "UiOverlayFieldCycleRequest"

uiOverlayFieldCycleResponseSchema :: JsonSchema
uiOverlayFieldCycleResponseSchema = objectSchema "UiOverlayFieldCycleResponse"
  [ "overlay", "field_index", "field_name", "field_type" ]
  [ ("overlay", stringSchema)
  , ("field_index", integerSchema)
  , ("field_name", stringSchema)
  , ("field_type", stringSchema)
  ]

cameraSetRequestSchema :: JsonSchema
cameraSetRequestSchema = objectSchema "CameraSetRequest"
  [ "x", "y" ]
  [ ("x", numberSchema)
  , ("y", numberSchema)
  , ("zoom", numberSchema)
  ]

cameraSetResponseSchema :: JsonSchema
cameraSetResponseSchema = objectSchema "CameraSetResponse"
  [ "x", "y" ]
  [ ("x", numberSchema)
  , ("y", numberSchema)
  , ("zoom", nullableSchema numberSchema)
  ]

cameraGetResponseSchema :: JsonSchema
cameraGetResponseSchema = objectSchema "CameraGetResponse"
  [ "x", "y", "zoom" ]
  [ ("x", numberSchema)
  , ("y", numberSchema)
  , ("zoom", numberSchema)
  ]

cameraZoomToChunkRequestSchema :: JsonSchema
cameraZoomToChunkRequestSchema = objectSchema "CameraZoomToChunkRequest"
  [ "chunk" ]
  [ ("chunk", integerSchema)
  ]

cameraZoomToChunkResponseSchema :: JsonSchema
cameraZoomToChunkResponseSchema = objectSchema "CameraZoomToChunkResponse"
  [ "chunk", "x", "y", "zoom" ]
  [ ("chunk", integerSchema)
  , ("x", numberSchema)
  , ("y", numberSchema)
  , ("zoom", numberSchema)
  ]

uiLeftPanelSetRequestSchema :: JsonSchema
uiLeftPanelSetRequestSchema = visibleRequestSchema "UiLeftPanelSetRequest"

uiLeftPanelSetResponseSchema :: JsonSchema
uiLeftPanelSetResponseSchema = visibleResponseSchema "UiLeftPanelSetResponse"

uiLeftTabSetRequestSchema :: JsonSchema
uiLeftTabSetRequestSchema = objectSchema "UiLeftTabSetRequest"
  [ "tab" ]
  [ ("tab", leftTabSchema)
  ]

uiLeftTabSetResponseSchema :: JsonSchema
uiLeftTabSetResponseSchema = objectSchema "UiLeftTabSetResponse"
  [ "tab" ]
  [ ("tab", leftTabSchema)
  ]

uiConfigPanelToggleRequestSchema :: JsonSchema
uiConfigPanelToggleRequestSchema = objectSchema "UiConfigPanelToggleRequest"
  []
  [ ("visible", booleanSchema)
  ]

uiConfigPanelToggleResponseSchema :: JsonSchema
uiConfigPanelToggleResponseSchema = visibleResponseSchema "UiConfigPanelToggleResponse"

uiLogCollapsedSetRequestSchema :: JsonSchema
uiLogCollapsedSetRequestSchema = objectSchema "UiLogCollapsedSetRequest"
  [ "collapsed" ]
  [ ("collapsed", booleanSchema)
  ]

uiLogCollapsedSetResponseSchema :: JsonSchema
uiLogCollapsedSetResponseSchema = objectSchema "UiLogCollapsedSetResponse"
  [ "collapsed" ]
  [ ("collapsed", booleanSchema)
  ]

uiLogLevelSetRequestSchema :: JsonSchema
uiLogLevelSetRequestSchema = objectSchema "UiLogLevelSetRequest"
  [ "level" ]
  [ ("level", logLevelSchema)
  ]

uiLogLevelSetResponseSchema :: JsonSchema
uiLogLevelSetResponseSchema = objectSchema "UiLogLevelSetResponse"
  [ "level" ]
  [ ("level", logLevelSchema)
  ]

uiPanelsResponseSchema :: JsonSchema
uiPanelsResponseSchema = JsonSchema "UiPanelsResponse" uiPanelsObjectSchema

viewportScrollRequestSchema :: JsonSchema
viewportScrollRequestSchema = objectSchema "ViewportScrollRequest"
  [ "delta" ]
  [ ("delta", integerSchema)
  , ("x", integerSchema)
  , ("y", integerSchema)
  ]

viewportScrollResponseSchema :: JsonSchema
viewportScrollResponseSchema = objectSchema "ViewportScrollResponse"
  [ "zoom", "pan_x", "pan_y", "steps" ]
  [ ("zoom", numberSchema)
  , ("pan_x", numberSchema)
  , ("pan_y", numberSchema)
  , ("steps", integerSchema)
  ]

viewportClickRequestSchema :: JsonSchema
viewportClickRequestSchema = objectSchema "ViewportClickRequest"
  [ "x", "y" ]
  [ ("x", integerSchema)
  , ("y", integerSchema)
  , ("button", enumStringSchema ["left", "right"])
  ]

viewportClickResponseSchema :: JsonSchema
viewportClickResponseSchema = objectSchema "ViewportClickResponse"
  [ "button" ]
  [ ("button", enumStringSchema ["left", "right"])
  , ("hex_q", integerSchema)
  , ("hex_r", integerSchema)
  , ("selected", booleanSchema)
  , ("editor_stroke", booleanSchema)
  , ("tooltip_pinned", booleanSchema)
  , ("reason", stringSchema)
  ]

viewportDragRequestSchema :: JsonSchema
viewportDragRequestSchema = objectSchema "ViewportDragRequest"
  [ "x1", "y1", "x2", "y2" ]
  [ ("x1", integerSchema)
  , ("y1", integerSchema)
  , ("x2", integerSchema)
  , ("y2", integerSchema)
  ]

viewportDragResponseSchema :: JsonSchema
viewportDragResponseSchema = objectSchema "ViewportDragResponse"
  [ "pan_x", "pan_y", "dx", "dy" ]
  [ ("pan_x", numberSchema)
  , ("pan_y", numberSchema)
  , ("dx", integerSchema)
  , ("dy", integerSchema)
  ]

viewportHoverRequestSchema :: JsonSchema
viewportHoverRequestSchema = objectSchema "ViewportHoverRequest"
  [ "x", "y" ]
  [ ("x", integerSchema)
  , ("y", integerSchema)
  ]

viewportHoverResponseSchema :: JsonSchema
viewportHoverResponseSchema = objectSchema "ViewportHoverResponse"
  [ "hex_q", "hex_r", "valid" ]
  [ ("hex_q", integerSchema)
  , ("hex_r", integerSchema)
  , ("valid", booleanSchema)
  ]

widgetClickRequestSchema :: JsonSchema
widgetClickRequestSchema = widgetIdRequestSchema "WidgetClickRequest"

widgetClickResponseSchema :: JsonSchema
widgetClickResponseSchema = objectSchema "WidgetClickResponse"
  [ "widget_id", "status", "info" ]
  [ ("widget_id", stringSchema)
  , ("status", enumStringSchema ["clicked"])
  , ("info", stringSchema)
  ]

widgetListResponseSchema :: JsonSchema
widgetListResponseSchema = objectSchema "WidgetListResponse"
  [ "widgets", "widget_count", "categories" ]
  [ ("widgets", arraySchema stringSchema)
  , ("widget_count", integerSchema)
  , ("categories", freeObjectSchema)
  ]

widgetStateResponseSchema :: JsonSchema
widgetStateResponseSchema = objectSchema "WidgetStateResponse"
  [ "widget_id" ]
  [ ("widget_id", stringSchema)
  , ("active", booleanSchema)
  , ("enabled", booleanSchema)
  , ("expanded", booleanSchema)
  , ("edit_mode", booleanSchema)
  , ("confirm_shown", booleanSchema)
  ]

dialogStateResponseSchema :: JsonSchema
dialogStateResponseSchema = objectSchema "DialogStateResponse"
  [ "menu_mode", "seed_editing", "preset_count", "world_count", "data_edit_mode", "data_create_mode", "data_text_cursor" ]
  [ ("menu_mode", enumStringSchema ["none", "escape_menu", "preset_save", "preset_load", "world_save", "world_load"])
  , ("seed_editing", booleanSchema)
  , ("preset_input", stringSchema)
  , ("preset_filter", stringSchema)
  , ("preset_selected", integerSchema)
  , ("preset_count", integerSchema)
  , ("world_save_input", stringSchema)
  , ("world_filter", stringSchema)
  , ("world_selected", integerSchema)
  , ("world_count", integerSchema)
  , ("data_focused_field", nullableSchema stringSchema)
  , ("data_edit_mode", booleanSchema)
  , ("data_create_mode", booleanSchema)
  , ("data_text_cursor", integerSchema)
  ]

dialogTextSetRequestSchema :: JsonSchema
dialogTextSetRequestSchema = objectSchema "DialogTextSetRequest"
  [ "text" ]
  [ ("text", stringSchema)
  , ("target", enumStringSchema ["preset_input", "preset_filter", "world_input", "world_filter", "seed", "data_field"])
  ]

dialogTextSetResponseSchema :: JsonSchema
dialogTextSetResponseSchema = objectSchema "DialogTextSetResponse"
  [ "target", "text" ]
  [ ("target", stringSchema)
  , ("field", stringSchema)
  , ("text", stringSchema)
  ]

dialogActionResponseSchema :: JsonSchema
dialogActionResponseSchema = objectSchema "DialogActionResponse"
  [ "action" ]
  [ ("action", stringSchema)
  , ("name", stringSchema)
  , ("selected", integerSchema)
  , ("menu_mode", stringSchema)
  ]

keySendRequestSchema :: JsonSchema
keySendRequestSchema = objectSchema "KeySendRequest"
  [ "key" ]
  [ ("key", stringSchema)
  ]

keySendResponseSchema :: JsonSchema
keySendResponseSchema = objectSchema "KeySendResponse"
  []
  [ ("key", stringSchema)
  , ("selected", integerSchema)
  , ("text", stringSchema)
  , ("filter", stringSchema)
  , ("field", stringSchema)
  , ("cursor", integerSchema)
  , ("action", stringSchema)
  ]

uiStatePanelsSchema :: Value
uiStatePanelsSchema = inlineObjectSchema
  [ "left", "config", "log" ]
  [ ("left", inlineObjectSchema ["visible", "tab"]
      [ ("visible", booleanSchema)
      , ("tab", leftTabSchema)
      ])
  , ("config", inlineObjectSchema ["visible", "tab", "scroll"]
      [ ("visible", booleanSchema)
      , ("tab", configTabSchema)
      , ("scroll", integerSchema)
      ])
  , ("log", inlineObjectSchema ["collapsed", "level"]
      [ ("collapsed", booleanSchema)
      , ("level", logLevelSchema)
      ])
  ]

uiPanelsObjectSchema :: Value
uiPanelsObjectSchema = inlineObjectSchema
  [ "left_panel", "config_panel", "log_panel" ]
  [ ("left_panel", inlineObjectSchema ["visible", "tab"]
      [ ("visible", booleanSchema)
      , ("tab", leftTabSchema)
      ])
  , ("config_panel", inlineObjectSchema ["visible", "tab"]
      [ ("visible", booleanSchema)
      , ("tab", configTabSchema)
      ])
  , ("log_panel", inlineObjectSchema ["collapsed", "level"]
      [ ("collapsed", booleanSchema)
      , ("level", logLevelSchema)
      ])
  ]

dataBrowserStateSchema :: Value
dataBrowserStateSchema = inlineObjectSchema
  [ "record_count", "total_count", "page_offset", "loading", "edit_mode", "create_mode", "has_selection" ]
  [ ("selected_plugin", nullableSchema stringSchema)
  , ("selected_resource", nullableSchema stringSchema)
  , ("record_count", integerSchema)
  , ("total_count", nullableSchema integerSchema)
  , ("page_offset", integerSchema)
  , ("loading", booleanSchema)
  , ("edit_mode", booleanSchema)
  , ("create_mode", booleanSchema)
  , ("has_selection", booleanSchema)
  ]

baseViewEntrySchema :: Value
baseViewEntrySchema = inlineObjectSchema
  [ "name", "active", "label", "legacy_view_mode" ]
  [ ("name", baseViewModeSchema)
  , ("active", booleanSchema)
  , ("label", stringSchema)
  , ("legacy_view_mode", viewModeSchema)
  , ("description", stringSchema)
  , ("kind", enumStringSchema ["scalar", "categorical"])
  , ("color_scale", stringSchema)
  , ("legend", freeObjectSchema)
  ]

overlayViewEntrySchema :: Value
overlayViewEntrySchema = inlineObjectSchema
  [ "name", "active", "label" ]
  [ ("name", overlayModeSchema)
  , ("overlay_mode", overlayModeSchema)
  , ("active", booleanSchema)
  , ("label", stringSchema)
  , ("legacy_view_mode", nullableSchema viewModeSchema)
  , ("plugin_overlay", nullableSchema stringSchema)
  , ("field_index", nullableSchema integerSchema)
  , ("weather_basis_supported", arraySchema weatherBasisSchema)
  , ("temporal_basis", nullableSchema temporalBasisSchema)
  , ("source_kind", nullableSchema sourceKindSchema)
  ]

weatherBasisEntrySchema :: Value
weatherBasisEntrySchema = inlineObjectSchema
  [ "name", "active" ]
  [ ("name", weatherBasisSchema)
  , ("active", booleanSchema)
  , ("temporal_basis", nullableSchema temporalBasisSchema)
  , ("source_kind", nullableSchema sourceKindSchema)
  ]

viewModeEntrySchema :: Value
viewModeEntrySchema = inlineObjectSchema
  [ "name", "active", "label", "kind", "temporal_basis", "source_kind", "color_scale", "legend", "tooltip_fields", "inspector_fields", "export_fields", "http" ]
  [ ("name", viewModeSchema)
  , ("active", booleanSchema)
  , ("label", stringSchema)
  , ("description", stringSchema)
  , ("kind", enumStringSchema ["scalar", "categorical"])
  , ("temporal_basis", nullableSchema temporalBasisSchema)
  , ("source_kind", nullableSchema sourceKindSchema)
  , ("unit", nullableSchema stringSchema)
  , ("color_scale", stringSchema)
  , ("legend", freeObjectSchema)
  , ("tooltip_fields", arraySchema stringSchema)
  , ("inspector_fields", arraySchema stringSchema)
  , ("export_fields", arraySchema stringSchema)
  , ("http", arraySchema stringSchema)
  ]

overlayFieldSchema :: Value
overlayFieldSchema = inlineObjectSchema
  [ "index", "name", "type" ]
  [ ("index", integerSchema)
  , ("name", stringSchema)
  , ("type", stringSchema)
  , ("default", anySchema)
  , ("indexed", booleanSchema)
  , ("renamed_from", nullableSchema stringSchema)
  ]

overlaySummarySchema :: Value
overlaySummarySchema = inlineObjectSchema
  [ "name", "version", "storage", "field_count", "fields", "chunk_count" ]
  [ ("name", stringSchema)
  , ("version", stringSchema)
  , ("description", stringSchema)
  , ("storage", enumStringSchema ["sparse", "dense"])
  , ("field_count", integerSchema)
  , ("fields", arraySchema overlayFieldSchema)
  , ("chunk_count", integerSchema)
  , ("populated_tile_count", integerSchema)
  , ("dependencies", freeObjectSchema)
  , ("provenance", overlayProvenanceSchema)
  , ("active", booleanSchema)
  , ("active_field_index", nullableSchema integerSchema)
  ]

overlayProvenanceSchema :: Value
overlayProvenanceSchema = inlineObjectSchema
  [ "seed", "version", "source" ]
  [ ("seed", integerSchema)
  , ("version", integerSchema)
  , ("source", stringSchema)
  ]

vertexSchema :: Value
vertexSchema = inlineObjectSchema
  [ "x", "y", "z" ]
  [ ("x", numberSchema)
  , ("y", numberSchema)
  , ("z", numberSchema)
  ]

diagnosticSchema :: Value
diagnosticSchema = inlineObjectSchema
  [ "level", "code", "message" ]
  [ ("level", enumStringSchema ["info", "warn", "error"])
  , ("code", stringSchema)
  , ("message", stringSchema)
  ]

-- Shared schema helpers ------------------------------------------------------

nameRequestSchema :: Text -> JsonSchema
nameRequestSchema name = objectSchema name
  [ "name" ]
  [ ("name", stringSchema)
  ]

statusResponseSchema :: Text -> JsonSchema
statusResponseSchema name = objectSchema name
  [ "status" ]
  [ ("status", stringSchema)
  ]

visibleRequestSchema :: Text -> JsonSchema
visibleRequestSchema name = objectSchema name
  [ "visible" ]
  [ ("visible", booleanSchema)
  ]

visibleResponseSchema :: Text -> JsonSchema
visibleResponseSchema = visibleRequestSchema

directionRequestSchema :: Text -> JsonSchema
directionRequestSchema name = objectSchema name
  [ "direction" ]
  [ ("direction", integerSchema)
  ]

widgetIdRequestSchema :: Text -> JsonSchema
widgetIdRequestSchema name = objectSchema name
  [ "widget_id" ]
  [ ("widget_id", stringSchema)
  ]

axialCoordSchema :: Value
axialCoordSchema = inlineObjectSchema
  [ "q", "r" ]
  [ ("q", integerSchema)
  , ("r", integerSchema)
  ]

viewModeSchema :: Value
viewModeSchema = object
  [ "description" .= ("Built-in view mode name, or a dynamic overlay:<name> mode." :: Text)
  , "oneOf" .=
      [ enumStringSchema (map viewModeToText allBuiltinViewModes)
      , object
          [ "type" .= ("string" :: Text)
          , "pattern" .= ("^overlay:.+" :: Text)
          ]
      ]
  ]

baseViewModeSchema :: Value
baseViewModeSchema = enumStringSchema (map baseViewModeToText allBaseViewModes)

overlayModeSchema :: Value
overlayModeSchema = object
  [ "description" .= ("Built-in overlay mode, plugin overlay selector, or none." :: Text)
  , "oneOf" .=
      [ enumStringSchema ("none" : "plugin" : map skyOverlayModeToText allBuiltinSkyOverlayModes)
      , object
          [ "type" .= ("string" :: Text)
          , "pattern" .= ("^overlay:.+" :: Text)
          ]
      ]
  ]

weatherBasisSchema :: Value
weatherBasisSchema = enumStringSchema ["average", "current"]

configTabSchema :: Value
configTabSchema = enumStringSchema
  [ "terrain", "planet", "climate", "weather", "biome", "erosion", "pipeline", "data" ]

sliderTabSchema :: Value
sliderTabSchema = enumStringSchema
  [ "terrain", "planet", "climate", "weather", "biome", "erosion" ]

leftTabSchema :: Value
leftTabSchema = enumStringSchema ["topo", "view"]

logLevelSchema :: Value
logLevelSchema = enumStringSchema ["debug", "info", "warn", "error"]

temporalBasisSchema :: Value
temporalBasisSchema = enumStringSchema ["long_run_average", "typical_normal", "instantaneous_current"]

sourceKindSchema :: Value
sourceKindSchema = enumStringSchema ["climate_average", "weather_snapshot", "weather_normals", "external_live"]

editorToolSchema :: Value
editorToolSchema = enumStringSchema
  [ "raise", "lower", "smooth", "flatten", "noise", "paint_biome", "paint_form", "set_hardness", "erode" ]

brushFalloffSchema :: Value
brushFalloffSchema = enumStringSchema ["linear", "smooth", "constant"]

objectSchema :: Text -> [Text] -> [(Text, Value)] -> JsonSchema
objectSchema name required properties = JsonSchema name (inlineObjectSchema required properties)

describedSchema :: Text -> Value -> Value
describedSchema description (Object schema) = Object (KM.insert "description" (String description) schema)
describedSchema description schema = object
  [ "description" .= description
  , "allOf" .= [schema]
  ]

inlineObjectSchema :: [Text] -> [(Text, Value)] -> Value
inlineObjectSchema required properties = object $
  [ "type" .= ("object" :: Text)
  , "properties" .= propertiesObject properties
  ]
  <> [ "required" .= required | not (null required) ]

propertiesObject :: [(Text, Value)] -> Value
propertiesObject properties = object
  [ Key.fromText key .= schema
  | (key, schema) <- properties
  ]

arraySchema :: Value -> Value
arraySchema itemSchema = object
  [ "type" .= ("array" :: Text)
  , "items" .= itemSchema
  ]

nullableSchema :: Value -> Value
nullableSchema (Object schema) = Object (KM.insert "nullable" (Bool True) schema)
nullableSchema schema = object
  [ "nullable" .= True
  , "allOf" .= [schema]
  ]

stringSchema :: Value
stringSchema = object ["type" .= ("string" :: Text)]

integerSchema :: Value
integerSchema = object ["type" .= ("integer" :: Text)]

integerMinimumSchema :: Int -> Value
integerMinimumSchema minimumValue = object
  [ "type" .= ("integer" :: Text)
  , "minimum" .= minimumValue
  ]

numberSchema :: Value
numberSchema = object ["type" .= ("number" :: Text)]

booleanSchema :: Value
booleanSchema = object ["type" .= ("boolean" :: Text)]

freeObjectSchema :: Value
freeObjectSchema = object
  [ "type" .= ("object" :: Text)
  , "additionalProperties" .= True
  ]

anySchema :: Value
anySchema = object []

enumStringSchema :: [Text] -> Value
enumStringSchema values = object
  [ "type" .= ("string" :: Text)
  , "enum" .= values
  ]
