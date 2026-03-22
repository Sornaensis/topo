{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Command dispatch: route 'SeerCommand' methods to handler functions.
module Seer.Command.Dispatch
  ( CommandContext(..)
  , dispatchCommand
  ) where

import Topo.Command.Types (SeerCommand(..), SeerResponse, errResponse)
import Seer.Command.Context (CommandContext(..))
import qualified Seer.Command.Handlers.Enums as HEnums
import qualified Seer.Command.Handlers.Generate as HGenerate
import qualified Seer.Command.Handlers.Presets as HPresets
import qualified Seer.Command.Handlers.Screenshot as HScreenshot
import qualified Seer.Command.Handlers.State as HState
import qualified Seer.Command.Handlers.Sliders as HSliders
import qualified Seer.Command.Handlers.Terrain as HTerrain
import qualified Seer.Command.Handlers.View as HView
import qualified Seer.Command.Handlers.World as HWorld

-- | Dispatch a 'SeerCommand' to the appropriate handler based on 'scMethod'.
dispatchCommand :: CommandContext -> SeerCommand -> IO SeerResponse
dispatchCommand ctx cmd = case scMethod cmd of
  -- State queries
  "get_state"        -> HState.handleGetState        ctx (scId cmd) (scParams cmd)
  "get_view_modes"   -> HState.handleGetViewModes    ctx (scId cmd) (scParams cmd)

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

  -- Generation
  "generate"         -> HGenerate.handleGenerate     ctx (scId cmd) (scParams cmd)

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

  -- Preset management
  "list_presets"         -> HPresets.handleListPresets         ctx (scId cmd) (scParams cmd)
  "save_preset"          -> HPresets.handleSavePreset         ctx (scId cmd) (scParams cmd)
  "load_preset"          -> HPresets.handleLoadPreset         ctx (scId cmd) (scParams cmd)

  -- Screenshot
  "take_screenshot"      -> HScreenshot.handleTakeScreenshot  ctx (scId cmd) (scParams cmd)

  -- Unknown command
  other              -> pure (errResponse (scId cmd) ("unknown command: " <> other))
