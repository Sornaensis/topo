{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Command dispatch: route 'SeerCommand' methods to handler functions.
module Seer.Command.Dispatch
  ( CommandContext(..)
  , dispatchCommand
  ) where

import Topo.Command.Types (SeerCommand(..), SeerResponse, errResponse)
import Seer.Command.Context (CommandContext(..))
import qualified Seer.Command.Handlers.Camera as HCamera
import qualified Seer.Command.Handlers.Data as HData
import qualified Seer.Command.Handlers.Editor as HEditor
import qualified Seer.Command.Handlers.Enums as HEnums
import qualified Seer.Command.Handlers.Generate as HGenerate
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
import qualified Seer.Command.Handlers.Widgets as HWidgets
import qualified Seer.Command.Handlers.World as HWorld

-- | Dispatch a 'SeerCommand' to the appropriate handler based on 'scMethod'.
dispatchCommand :: CommandContext -> SeerCommand -> IO SeerResponse
dispatchCommand ctx cmd = case scMethod cmd of
  -- State queries
  "get_state"        -> HState.handleGetState        ctx (scId cmd) (scParams cmd)
  "get_view_modes"   -> HState.handleGetViewModes    ctx (scId cmd) (scParams cmd)
  "get_ui_state"     -> HState.handleGetUiState      ctx (scId cmd) (scParams cmd)

  -- Slider queries and mutations
  "get_sliders"      -> HSliders.handleGetSliders    ctx (scId cmd) (scParams cmd)
  "get_slider"       -> HSliders.handleGetSlider     ctx (scId cmd) (scParams cmd)
  "set_slider"       -> HSliders.handleSetSlider     ctx (scId cmd) (scParams cmd)
  "set_sliders"      -> HSliders.handleSetSliders    ctx (scId cmd) (scParams cmd)
  "reset_sliders"    -> HSliders.handleResetSliders   ctx (scId cmd) (scParams cmd)

  -- View, seed, and hex mutations
  "set_seed"         -> HView.handleSetSeed          ctx (scId cmd) (scParams cmd)
  "set_view_mode"    -> HView.handleSetViewMode      ctx (scId cmd) (scParams cmd)
  "set_config_tab"   -> HView.handleSetConfigTab     ctx (scId cmd) (scParams cmd)
  "select_hex"       -> HView.handleSelectHex        ctx (scId cmd) (scParams cmd)

  -- Overlay navigation
  "set_overlay"          -> HView.handleSetOverlay          ctx (scId cmd) (scParams cmd)
  "list_overlay_fields"  -> HView.handleListOverlayFields   ctx (scId cmd) (scParams cmd)
  "cycle_overlay"        -> HView.handleCycleOverlay        ctx (scId cmd) (scParams cmd)
  "cycle_overlay_field"  -> HView.handleCycleOverlayField   ctx (scId cmd) (scParams cmd)

  -- Camera controls
  "set_camera"       -> HCamera.handleSetCamera      ctx (scId cmd) (scParams cmd)
  "get_camera"       -> HCamera.handleGetCamera      ctx (scId cmd) (scParams cmd)
  "zoom_to_chunk"    -> HCamera.handleZoomToChunk    ctx (scId cmd) (scParams cmd)

  -- Generation
  "generate"         -> HGenerate.handleGenerate     ctx (scId cmd) (scParams cmd)

  -- Terrain editor
  "editor_toggle"       -> HEditor.handleEditorToggle      ctx (scId cmd) (scParams cmd)
  "editor_set_tool"     -> HEditor.handleEditorSetTool     ctx (scId cmd) (scParams cmd)
  "editor_set_brush"    -> HEditor.handleEditorSetBrush    ctx (scId cmd) (scParams cmd)
  "editor_brush_stroke" -> HEditor.handleEditorBrushStroke ctx (scId cmd) (scParams cmd)
  "editor_brush_line"   -> HEditor.handleEditorBrushLine   ctx (scId cmd) (scParams cmd)
  "editor_set_biome"    -> HEditor.handleEditorSetBiome    ctx (scId cmd) (scParams cmd)
  "editor_set_form"     -> HEditor.handleEditorSetForm     ctx (scId cmd) (scParams cmd)
  "editor_set_hardness" -> HEditor.handleEditorSetHardness ctx (scId cmd) (scParams cmd)
  "editor_undo"         -> HEditor.handleEditorUndo        ctx (scId cmd) (scParams cmd)
  "editor_redo"         -> HEditor.handleEditorRedo        ctx (scId cmd) (scParams cmd)
  "editor_get_state"    -> HEditor.handleEditorGetState    ctx (scId cmd) (scParams cmd)

  -- Enum queries
  "get_enums"        -> HEnums.handleGetEnums        ctx (scId cmd) (scParams cmd)

  -- Terrain data queries
  "get_hex"              -> HTerrain.handleGetHex          ctx (scId cmd) (scParams cmd)
  "get_chunks"           -> HTerrain.handleGetChunks       ctx (scId cmd) (scParams cmd)
  "get_chunk_summary"    -> HTerrain.handleGetChunkSummary ctx (scId cmd) (scParams cmd)
  "get_terrain_stats"    -> HTerrain.handleGetTerrainStats ctx (scId cmd) (scParams cmd)

  -- World / meta queries and mutations
  "get_world_meta"       -> HWorld.handleGetWorldMeta         ctx (scId cmd) (scParams cmd)
  "get_generation_status" -> HWorld.handleGetGenerationStatus ctx (scId cmd) (scParams cmd)
  "get_overlays"         -> HWorld.handleGetOverlays          ctx (scId cmd) (scParams cmd)
  "list_worlds"          -> HWorld.handleListWorlds           ctx (scId cmd) (scParams cmd)
  "save_world"           -> HWorld.handleSaveWorld            ctx (scId cmd) (scParams cmd)
  "load_world"           -> HWorld.handleLoadWorld            ctx (scId cmd) (scParams cmd)
  "set_world_name"       -> HWorld.handleSetWorldName         ctx (scId cmd) (scParams cmd)

  -- Log access
  "get_logs"             -> HLog.handleGetLogs                ctx (scId cmd) (scParams cmd)

  -- Pipeline stage control
  "get_pipeline"         -> HPipeline.handleGetPipeline       ctx (scId cmd) (scParams cmd)
  "set_stage_enabled"    -> HPipeline.handleSetStageEnabled   ctx (scId cmd) (scParams cmd)

  -- Plugin management
  "list_plugins"         -> HPlugin.handleListPlugins         ctx (scId cmd) (scParams cmd)
  "set_plugin_enabled"   -> HPlugin.handleSetPluginEnabled    ctx (scId cmd) (scParams cmd)
  "set_plugin_param"     -> HPlugin.handleSetPluginParam      ctx (scId cmd) (scParams cmd)

  -- Simulation control
  "get_sim_state"        -> HSimulation.handleGetSimState     ctx (scId cmd) (scParams cmd)
  "set_sim_auto_tick"    -> HSimulation.handleSetSimAutoTick  ctx (scId cmd) (scParams cmd)
  "sim_tick"             -> HSimulation.handleSimTick         ctx (scId cmd) (scParams cmd)

  -- Config summary
  "get_config_summary"   -> HSliders.handleGetConfigSummary   ctx (scId cmd) (scParams cmd)

  -- Hex search and terrain export
  "find_hexes"           -> HQuery.handleFindHexes            ctx (scId cmd) (scParams cmd)
  "export_terrain_data"  -> HQuery.handleExportTerrainData    ctx (scId cmd) (scParams cmd)

  -- Preset management
  "list_presets"         -> HPresets.handleListPresets         ctx (scId cmd) (scParams cmd)
  "save_preset"          -> HPresets.handleSavePreset         ctx (scId cmd) (scParams cmd)
  "load_preset"          -> HPresets.handleLoadPreset         ctx (scId cmd) (scParams cmd)

  -- Screenshot
  "take_screenshot"      -> HScreenshot.handleTakeScreenshot  ctx (scId cmd) (scParams cmd)

  -- Data browser
  "data_list_plugins"    -> HData.handleDataListPlugins     ctx (scId cmd) (scParams cmd)
  "data_list_resources"  -> HData.handleDataListResources   ctx (scId cmd) (scParams cmd)
  "data_list_records"    -> HData.handleDataListRecords     ctx (scId cmd) (scParams cmd)
  "data_get_record"      -> HData.handleDataGetRecord       ctx (scId cmd) (scParams cmd)
  "data_create_record"   -> HData.handleDataCreateRecord    ctx (scId cmd) (scParams cmd)
  "data_update_record"   -> HData.handleDataUpdateRecord    ctx (scId cmd) (scParams cmd)
  "data_delete_record"   -> HData.handleDataDeleteRecord    ctx (scId cmd) (scParams cmd)
  "data_get_state"       -> HData.handleDataGetState        ctx (scId cmd) (scParams cmd)

  -- Panel visibility & tab controls
  "set_left_panel"       -> HPanels.handleSetLeftPanel        ctx (scId cmd) (scParams cmd)
  "set_left_tab"         -> HPanels.handleSetLeftTab          ctx (scId cmd) (scParams cmd)
  "toggle_config_panel"  -> HPanels.handleToggleConfigPanel   ctx (scId cmd) (scParams cmd)
  "set_log_collapsed"    -> HPanels.handleSetLogCollapsed     ctx (scId cmd) (scParams cmd)
  "set_log_level"        -> HPanels.handleSetLogLevel         ctx (scId cmd) (scParams cmd)
  "get_ui_panels"        -> HPanels.handleGetUiPanels         ctx (scId cmd) (scParams cmd)

  -- Widget interaction
  "click_widget"         -> HWidgets.handleClickWidget        ctx (scId cmd) (scParams cmd)
  "list_widgets"         -> HWidgets.handleListWidgets        ctx (scId cmd) (scParams cmd)
  "get_widget_state"     -> HWidgets.handleGetWidgetState     ctx (scId cmd) (scParams cmd)

  -- Unknown command
  other              -> pure (errResponse (scId cmd) ("unknown command: " <> other))
