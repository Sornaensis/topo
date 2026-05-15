{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Command dispatch: route 'SeerCommand' methods to handler functions.
module Seer.Command.Dispatch
  ( CommandContext(..)
  , dispatchCommand
  , dispatchCommandMethods
  ) where

import Topo.Command.Types (SeerCommand(..), SeerResponse, errResponse)
import Data.Aeson (Value)
import Data.Text (Text)

import Seer.Command.Context (CommandContext(..))
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

type RawCommandHandler = CommandContext -> Int -> Value -> IO SeerResponse

type CommandHandler = CommandContext -> SeerCommand -> IO SeerResponse

-- | Dispatch a 'SeerCommand' to the appropriate handler based on 'scMethod'.
--
-- The supported method catalog is derived from the same table used by dispatch,
-- so AppService coverage tests compare against the real command adapter surface
-- rather than AppService metadata echoing itself.
dispatchCommand :: CommandContext -> SeerCommand -> IO SeerResponse
dispatchCommand ctx cmd = case lookup (scMethod cmd) dispatchCommandHandlers of
  Just handler -> handler ctx cmd
  Nothing -> pure (errResponse (scId cmd) ("unknown command: " <> scMethod cmd))

commandHandler :: RawCommandHandler -> CommandHandler
commandHandler handler ctx cmd = handler ctx (scId cmd) (scParams cmd)

dispatchCommandHandlers :: [(Text, CommandHandler)]
dispatchCommandHandlers =
  [ -- State queries
    ("get_state", commandHandler HState.handleGetState)
  , ("get_view_modes", commandHandler HState.handleGetViewModes)
  , ("get_ui_state", commandHandler HState.handleGetUiState)

    -- Slider queries and mutations
  , ("get_sliders", commandHandler HSliders.handleGetSliders)
  , ("get_slider", commandHandler HSliders.handleGetSlider)
  , ("set_slider", commandHandler HSliders.handleSetSlider)
  , ("set_sliders", commandHandler HSliders.handleSetSliders)
  , ("reset_sliders", commandHandler HSliders.handleResetSliders)

    -- View, seed, and hex mutations
  , ("set_seed", commandHandler HView.handleSetSeed)
  , ("set_view_mode", commandHandler HView.handleSetViewMode)
  , ("set_config_tab", commandHandler HView.handleSetConfigTab)
  , ("select_hex", commandHandler HView.handleSelectHex)

    -- Overlay navigation
  , ("set_overlay", commandHandler HView.handleSetOverlay)
  , ("list_overlay_fields", commandHandler HView.handleListOverlayFields)
  , ("cycle_overlay", commandHandler HView.handleCycleOverlay)
  , ("cycle_overlay_field", commandHandler HView.handleCycleOverlayField)

    -- Camera controls
  , ("set_camera", commandHandler HCamera.handleSetCamera)
  , ("get_camera", commandHandler HCamera.handleGetCamera)
  , ("zoom_to_chunk", commandHandler HCamera.handleZoomToChunk)

    -- Generation
  , ("generate", commandHandler HGenerate.handleGenerate)

    -- Terrain editor
  , ("editor_toggle", commandHandler HEditor.handleEditorToggle)
  , ("editor_set_tool", commandHandler HEditor.handleEditorSetTool)
  , ("editor_set_brush", commandHandler HEditor.handleEditorSetBrush)
  , ("editor_brush_stroke", commandHandler HEditor.handleEditorBrushStroke)
  , ("editor_brush_line", commandHandler HEditor.handleEditorBrushLine)
  , ("editor_set_biome", commandHandler HEditor.handleEditorSetBiome)
  , ("editor_set_form", commandHandler HEditor.handleEditorSetForm)
  , ("editor_set_hardness", commandHandler HEditor.handleEditorSetHardness)
  , ("editor_undo", commandHandler HEditor.handleEditorUndo)
  , ("editor_redo", commandHandler HEditor.handleEditorRedo)
  , ("editor_get_state", commandHandler HEditor.handleEditorGetState)

    -- Enum queries
  , ("get_enums", commandHandler HEnums.handleGetEnums)

    -- Terrain data queries
  , ("get_hex", commandHandler HTerrain.handleGetHex)
  , ("get_chunks", commandHandler HTerrain.handleGetChunks)
  , ("get_chunk_summary", commandHandler HTerrain.handleGetChunkSummary)
  , ("get_terrain_stats", commandHandler HTerrain.handleGetTerrainStats)

    -- World / meta queries and mutations
  , ("get_world_meta", commandHandler HWorld.handleGetWorldMeta)
  , ("get_generation_status", commandHandler HWorld.handleGetGenerationStatus)
  , ("get_overlays", commandHandler HWorld.handleGetOverlays)
  , ("list_worlds", commandHandler HWorld.handleListWorlds)
  , ("save_world", commandHandler HWorld.handleSaveWorld)
  , ("load_world", commandHandler HWorld.handleLoadWorld)
  , ("set_world_name", commandHandler HWorld.handleSetWorldName)

    -- Log access
  , ("get_logs", commandHandler HLog.handleGetLogs)

    -- Pipeline stage control
  , ("get_pipeline", commandHandler HPipeline.handleGetPipeline)
  , ("set_stage_enabled", commandHandler HPipeline.handleSetStageEnabled)

    -- Plugin management
  , ("list_plugins", commandHandler HPlugin.handleListPlugins)
  , ("set_plugin_enabled", commandHandler HPlugin.handleSetPluginEnabled)
  , ("set_plugin_param", commandHandler HPlugin.handleSetPluginParam)

    -- Simulation control
  , ("get_sim_state", commandHandler HSimulation.handleGetSimState)
  , ("set_sim_auto_tick", commandHandler HSimulation.handleSetSimAutoTick)
  , ("sim_tick", commandHandler HSimulation.handleSimTick)

    -- Config summary
  , ("get_config_summary", commandHandler HSliders.handleGetConfigSummary)

    -- Hex search and terrain export
  , ("find_hexes", commandHandler HQuery.handleFindHexes)
  , ("export_terrain_data", commandHandler HQuery.handleExportTerrainData)

    -- Preset management
  , ("list_presets", commandHandler HPresets.handleListPresets)
  , ("save_preset", commandHandler HPresets.handleSavePreset)
  , ("load_preset", commandHandler HPresets.handleLoadPreset)

    -- Screenshot
  , ("take_screenshot", commandHandler HScreenshot.handleTakeScreenshot)

    -- Data browser
  , ("data_list_plugins", commandHandler HData.handleDataListPlugins)
  , ("data_list_resources", commandHandler HData.handleDataListResources)
  , ("data_list_records", commandHandler HData.handleDataListRecords)
  , ("data_get_record", commandHandler HData.handleDataGetRecord)
  , ("data_create_record", commandHandler HData.handleDataCreateRecord)
  , ("data_update_record", commandHandler HData.handleDataUpdateRecord)
  , ("data_delete_record", commandHandler HData.handleDataDeleteRecord)
  , ("data_get_state", commandHandler HData.handleDataGetState)

    -- Panel visibility & tab controls
  , ("set_left_panel", commandHandler HPanels.handleSetLeftPanel)
  , ("set_left_tab", commandHandler HPanels.handleSetLeftTab)
  , ("toggle_config_panel", commandHandler HPanels.handleToggleConfigPanel)
  , ("set_log_collapsed", commandHandler HPanels.handleSetLogCollapsed)
  , ("set_log_level", commandHandler HPanels.handleSetLogLevel)
  , ("get_ui_panels", commandHandler HPanels.handleGetUiPanels)

    -- Viewport interaction
  , ("viewport_scroll", commandHandler HViewport.handleViewportScroll)
  , ("viewport_click", commandHandler HViewport.handleViewportClick)
  , ("viewport_drag", commandHandler HViewport.handleViewportDrag)
  , ("viewport_hover", commandHandler HViewport.handleViewportHover)

    -- Widget interaction
  , ("click_widget", commandHandler HWidgets.handleClickWidget)
  , ("list_widgets", commandHandler HWidgets.handleListWidgets)
  , ("get_widget_state", commandHandler HWidgets.handleGetWidgetState)

    -- Dialog and text input
  , ("get_dialog_state", commandHandler HInput.handleGetDialogState)
  , ("set_dialog_text", commandHandler HInput.handleSetDialogText)
  , ("dialog_confirm", commandHandler HInput.handleDialogConfirm)
  , ("dialog_cancel", commandHandler HInput.handleDialogCancel)
  , ("send_key", commandHandler HInput.handleSendKey)
  ]

-- | Command methods supported by the dispatch table.
dispatchCommandMethods :: [Text]
dispatchCommandMethods = map fst dispatchCommandHandlers
