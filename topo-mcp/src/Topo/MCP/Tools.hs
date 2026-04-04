{-# LANGUAGE OverloadedStrings #-}

-- | MCP tool call routing.
--
-- Maps MCP tool calls to IPC commands sent to topo-seer.
-- Each tool call translates into a 'SeerCommand' sent over IPC,
-- and the 'SeerResponse' result is formatted as MCP 'ToolCallResult'.
module Topo.MCP.Tools
  ( allToolDefs
  , handleToolCall
  , toolToIpc  -- exported for testing
  ) where

import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Lazy as BL

import Topo.Command.Types (SeerResponse(..))
import Topo.MCP.Types
import Topo.MCP.IPC (IpcConnectionRef, sendCommand, withConnection)

-- | All tool definitions advertised by this server.
allToolDefs :: [ToolDef]
allToolDefs =
  [ ToolDef
      { tdName        = "get_state"
      , tdDescription = "Get the current topo-seer application state: seed, view mode, config tab, generation status"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object []
          ]
      }
  , ToolDef
      { tdName        = "list_sliders"
      , tdDescription = "List all sliders with current values, domain ranges, and defaults. Optionally filter by tab."
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "tab" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("Filter by config tab: terrain, planet, climate, weather, biome, erosion" :: Text)
                  , "enum" .= (["terrain", "planet", "climate", "weather", "biome", "erosion"] :: [Text])
                  ]
              ]
          ]
      }
  , ToolDef
      { tdName        = "get_slider"
      , tdDescription = "Get detailed info for a single slider by name"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "name" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("Slider name (e.g., SliderSeaLevel, SliderEquatorTemp)" :: Text)
                  ]
              ]
          , "required" .= (["name"] :: [Text])
          ]
      }
  , ToolDef
      { tdName        = "set_slider"
      , tdDescription = "Set a slider to a normalized [0,1] value. Use list_sliders to see available slider names."
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "name" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("Slider name" :: Text)
                  ]
              , "value" .= object
                  [ "type" .= ("number" :: Text)
                  , "description" .= ("Normalized value between 0.0 and 1.0" :: Text)
                  , "minimum" .= (0 :: Int)
                  , "maximum" .= (1 :: Int)
                  ]
              ]
          , "required" .= (["name", "value"] :: [Text])
          ]
      }
  , ToolDef
      { tdName        = "set_seed"
      , tdDescription = "Set the random seed for terrain generation"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "seed" .= object
                  [ "type" .= ("integer" :: Text)
                  , "description" .= ("Random seed value" :: Text)
                  ]
              ]
          , "required" .= (["seed"] :: [Text])
          ]
      }
  , ToolDef
      { tdName        = "set_view_mode"
      , tdDescription = "Switch the hex map visualization mode. For overlays, use 'overlay:name' syntax or the dedicated set_overlay tool."
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "mode" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("View mode name. Built-in modes or 'overlay:<name>' for plugin overlays." :: Text)
                  ]
              , "field_index" .= object
                  [ "type" .= ("integer" :: Text)
                  , "description" .= ("Field index for overlay modes (default 0). Ignored for built-in modes." :: Text)
                  , "minimum" .= (0 :: Int)
                  ]
              ]
          , "required" .= (["mode"] :: [Text])
          ]
      }
  , ToolDef
      { tdName        = "set_config_tab"
      , tdDescription = "Switch the config panel tab"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "tab" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("Config tab name" :: Text)
                  , "enum" .= (["terrain", "planet", "climate", "weather", "biome", "erosion", "pipeline", "data"] :: [Text])
                  ]
              ]
          , "required" .= (["tab"] :: [Text])
          ]
      }
  , ToolDef
      { tdName        = "get_view_modes"
      , tdDescription = "List all available view modes and which is currently active"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object []
          ]
      }
  , ToolDef
      { tdName        = "generate"
      , tdDescription = "Trigger terrain generation using the current seed and slider values. Returns immediately; generation runs asynchronously."
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object []
          ]
      }
  -- Terrain editor
  , ToolDef
      { tdName        = "editor_toggle"
      , tdDescription = "Toggle the terrain editor overlay, or explicitly set whether it is active"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "active" .= object
                  [ "type" .= ("boolean" :: Text)
                  , "description" .= ("Optional explicit editor active state; omit to toggle" :: Text)
                  ]
              ]
          ]
      }
  , ToolDef
      { tdName        = "editor_set_tool"
      , tdDescription = "Select the active terrain editor tool"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "tool" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("Tool name: raise, lower, smooth, flatten, noise, paint_biome, paint_form, set_hardness, erode" :: Text)
                  ]
              ]
          , "required" .= (["tool"] :: [Text])
          ]
      }
  , ToolDef
      { tdName        = "editor_set_brush"
      , tdDescription = "Update terrain editor brush settings. Any subset of properties may be provided."
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "radius" .= object
                  [ "type" .= ("integer" :: Text)
                  , "description" .= ("Brush radius in hex rings (0-6)" :: Text)
                  , "minimum" .= (0 :: Int)
                  , "maximum" .= (6 :: Int)
                  ]
              , "strength" .= object
                  [ "type" .= ("number" :: Text)
                  , "description" .= ("Brush strength (0.0-1.0)" :: Text)
                  , "minimum" .= (0.0 :: Double)
                  , "maximum" .= (1.0 :: Double)
                  ]
              , "falloff" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("Brush falloff curve" :: Text)
                  , "enum" .= (["linear", "smooth", "constant"] :: [Text])
                  ]
              , "smooth_passes" .= object
                  [ "type" .= ("integer" :: Text)
                  , "description" .= ("Smoothing iterations for the smooth tool (1-5)" :: Text)
                  , "minimum" .= (1 :: Int)
                  , "maximum" .= (5 :: Int)
                  ]
              , "noise_frequency" .= object
                  [ "type" .= ("number" :: Text)
                  , "description" .= ("Noise frequency for the noise tool (0.5-4.0)" :: Text)
                  , "minimum" .= (0.5 :: Double)
                  , "maximum" .= (4.0 :: Double)
                  ]
              , "erode_passes" .= object
                  [ "type" .= ("integer" :: Text)
                  , "description" .= ("Hydraulic and thermal erosion passes for the erode tool (1-20)" :: Text)
                  , "minimum" .= (1 :: Int)
                  , "maximum" .= (20 :: Int)
                  ]
              ]
          ]
      }
  , ToolDef
      { tdName        = "editor_brush_stroke"
      , tdDescription = "Queue a single terrain editor brush stroke at axial hex coordinates"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "q" .= object
                  [ "type" .= ("integer" :: Text)
                  , "description" .= ("Axial q coordinate" :: Text)
                  ]
              , "r" .= object
                  [ "type" .= ("integer" :: Text)
                  , "description" .= ("Axial r coordinate" :: Text)
                  ]
              ]
          , "required" .= (["q", "r"] :: [Text])
          ]
      }
  , ToolDef
      { tdName        = "editor_brush_line"
      , tdDescription = "Queue terrain editor brush strokes along a hex line between two axial coordinates"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "from_q" .= object
                  [ "type" .= ("integer" :: Text)
                  , "description" .= ("Start axial q coordinate" :: Text)
                  ]
              , "from_r" .= object
                  [ "type" .= ("integer" :: Text)
                  , "description" .= ("Start axial r coordinate" :: Text)
                  ]
              , "to_q" .= object
                  [ "type" .= ("integer" :: Text)
                  , "description" .= ("End axial q coordinate" :: Text)
                  ]
              , "to_r" .= object
                  [ "type" .= ("integer" :: Text)
                  , "description" .= ("End axial r coordinate" :: Text)
                  ]
              ]
          , "required" .= (["from_q", "from_r", "to_q", "to_r"] :: [Text])
          ]
      }
  , ToolDef
      { tdName        = "editor_set_biome"
      , tdDescription = "Set the target biome used by the paint_biome tool. Accepts a display name or numeric biome code."
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "biome" .= object
                  [ "description" .= ("Biome display name or numeric biome code" :: Text)
                  ]
              ]
          , "required" .= (["biome"] :: [Text])
          ]
      }
  , ToolDef
      { tdName        = "editor_set_form"
      , tdDescription = "Set the target terrain form used by the paint_form tool. Accepts a display name or numeric terrain form code."
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "form" .= object
                  [ "description" .= ("Terrain form display name or numeric terrain form code" :: Text)
                  ]
              ]
          , "required" .= (["form"] :: [Text])
          ]
      }
  , ToolDef
      { tdName        = "editor_set_hardness"
      , tdDescription = "Set the target hardness used by the set_hardness tool"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "hardness" .= object
                  [ "type" .= ("number" :: Text)
                  , "description" .= ("Hardness target (0.0-1.0)" :: Text)
                  , "minimum" .= (0.0 :: Double)
                  , "maximum" .= (1.0 :: Double)
                  ]
              ]
          , "required" .= (["hardness"] :: [Text])
          ]
      }
  , ToolDef
      { tdName        = "editor_undo"
      , tdDescription = "Queue undo for the most recent terrain editor change"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object []
          ]
      }
  , ToolDef
      { tdName        = "editor_redo"
      , tdDescription = "Queue redo for the most recently undone terrain editor change"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object []
          ]
      }
  , ToolDef
      { tdName        = "editor_get_state"
      , tdDescription = "Get the current terrain editor state, including brush settings and tool-specific targets"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object []
          ]
      }
  -- Phase 3: Query tools
  , ToolDef
      { tdName        = "get_enums"
      , tdDescription = "Enumerate valid values for a domain type. Returns [{name, code}] for each value."
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "type" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("Domain type to enumerate" :: Text)
                  , "enum" .= (["biome", "terrain_form", "water_body_type", "plate_boundary"
                              , "vent_type", "vent_activity", "view_mode", "config_tab", "slider_tab"] :: [Text])
                  ]
              ]
          , "required" .= (["type"] :: [Text])
          ]
      }
  , ToolDef
      { tdName        = "get_world_meta"
      , tdDescription = "Get world metadata: seed, chunk size, chunk count, total tiles, overlay names, world name"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object []
          ]
      }
  , ToolDef
      { tdName        = "get_generation_status"
      , tdDescription = "Check whether terrain generation is in progress and how many chunks exist"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object []
          ]
      }
  , ToolDef
      { tdName        = "inspect_hex"
      , tdDescription = "Get full terrain data at a specific hex coordinate (all layers: terrain, climate, weather, river, vegetation)"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "chunk" .= object
                  [ "type" .= ("integer" :: Text)
                  , "description" .= ("Chunk ID" :: Text)
                  ]
              , "tile" .= object
                  [ "type" .= ("integer" :: Text)
                  , "description" .= ("Tile index within the chunk" :: Text)
                  ]
              ]
          , "required" .= (["chunk", "tile"] :: [Text])
          ]
      }
  , ToolDef
      { tdName        = "get_chunks"
      , tdDescription = "List all terrain chunks with basic stats (elevation min/max per chunk)"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object []
          ]
      }
  , ToolDef
      { tdName        = "get_chunk_summary"
      , tdDescription = "Get detailed aggregate statistics for a specific chunk: elevation, biome distribution, terrain forms, climate, rivers"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "chunk" .= object
                  [ "type" .= ("integer" :: Text)
                  , "description" .= ("Chunk ID" :: Text)
                  ]
              ]
          , "required" .= (["chunk"] :: [Text])
          ]
      }
  , ToolDef
      { tdName        = "get_terrain_stats"
      , tdDescription = "Get global aggregate terrain statistics: elevation, biome distribution, terrain forms, temperature, precipitation, vegetation, rivers"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object []
          ]
      }
  , ToolDef
      { tdName        = "get_overlays"
      , tdDescription = "List loaded overlay names and count"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object []
          ]
      }
  , ToolDef
      { tdName        = "list_worlds"
      , tdDescription = "List saved worlds from ~/.topo/worlds/ with metadata (seed, chunk count, creation time)"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object []
          ]
      }
  -- Phase 4: Mutation tools
  , ToolDef
      { tdName        = "set_sliders"
      , tdDescription = "Batch set multiple sliders at once. Pass a map of slider names to normalized [0,1] values."
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "values" .= object
                  [ "type" .= ("object" :: Text)
                  , "description" .= ("Map of slider names to normalized [0,1] values, e.g. {\"SliderSeaLevel\": 0.5, \"SliderEquatorTemp\": 0.7}" :: Text)
                  , "additionalProperties" .= object
                      [ "type" .= ("number" :: Text)
                      , "minimum" .= (0 :: Int)
                      , "maximum" .= (1 :: Int)
                      ]
                  ]
              ]
          , "required" .= (["values"] :: [Text])
          ]
      }
  , ToolDef
      { tdName        = "reset_sliders"
      , tdDescription = "Reset sliders to default values. Optionally reset only a specific tab."
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "tab" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("Reset only this tab's sliders: terrain, planet, climate, weather, biome, erosion" :: Text)
                  , "enum" .= (["terrain", "planet", "climate", "weather", "biome", "erosion"] :: [Text])
                  ]
              ]
          ]
      }
  , ToolDef
      { tdName        = "select_hex"
      , tdDescription = "Select a hex tile for inspection in the UI, showing its tooltip. Omit chunk/tile to deselect."
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "chunk" .= object
                  [ "type" .= ("integer" :: Text)
                  , "description" .= ("Chunk ID" :: Text)
                  ]
              , "tile" .= object
                  [ "type" .= ("integer" :: Text)
                  , "description" .= ("Tile index within the chunk" :: Text)
                  ]
              ]
          ]
      }
  , ToolDef
      { tdName        = "save_world"
      , tdDescription = "Save the current terrain and config as a named world to ~/.topo/worlds/"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "name" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("World name" :: Text)
                  ]
              ]
          , "required" .= (["name"] :: [Text])
          ]
      }
  , ToolDef
      { tdName        = "load_world"
      , tdDescription = "Load a saved world by name, replacing current terrain and config. Triggers atlas rebuild."
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "name" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("World name" :: Text)
                  ]
              ]
          , "required" .= (["name"] :: [Text])
          ]
      }
  , ToolDef
      { tdName        = "list_presets"
      , tdDescription = "List saved config presets from ~/.topo/configs/"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object []
          ]
      }
  , ToolDef
      { tdName        = "save_preset"
      , tdDescription = "Save the current slider/seed/config state as a named preset"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "name" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("Preset name" :: Text)
                  ]
              ]
          , "required" .= (["name"] :: [Text])
          ]
      }
  , ToolDef
      { tdName        = "load_preset"
      , tdDescription = "Load a named preset, applying its slider/seed/config values to the UI"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "name" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("Preset name" :: Text)
                  ]
              ]
          , "required" .= (["name"] :: [Text])
          ]
      }
  , ToolDef
      { tdName        = "take_screenshot"
      , tdDescription = "Capture a screenshot of the current topo-seer window as a PNG image"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "path" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("Optional absolute file path to save the PNG. Parent directory must exist." :: Text)
                  ]
              ]
          ]
      }
  -- Camera controls
  , ToolDef
      { tdName        = "set_camera"
      , tdDescription = "Set the camera pan offset (screen-space pixels) and optionally zoom level"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "x" .= object
                  [ "type" .= ("number" :: Text)
                  , "description" .= ("Horizontal pan offset in pixels" :: Text)
                  ]
              , "y" .= object
                  [ "type" .= ("number" :: Text)
                  , "description" .= ("Vertical pan offset in pixels" :: Text)
                  ]
              , "zoom" .= object
                  [ "type" .= ("number" :: Text)
                  , "description" .= ("Zoom level (0.4 to 8.0, default 1.0)" :: Text)
                  , "minimum" .= (0.4 :: Double)
                  , "maximum" .= (8.0 :: Double)
                  ]
              ]
          , "required" .= (["x", "y"] :: [Text])
          ]
      }
  , ToolDef
      { tdName        = "get_camera"
      , tdDescription = "Get the current camera position (pan offset and zoom level)"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object []
          ]
      }
  , ToolDef
      { tdName        = "zoom_to_chunk"
      , tdDescription = "Center the camera on a specific chunk, setting zoom to 1.0 for overview"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "chunk" .= object
                  [ "type" .= ("integer" :: Text)
                  , "description" .= ("Chunk ID to center on" :: Text)
                  ]
              ]
          , "required" .= (["chunk"] :: [Text])
          ]
      }
  -- Log access
  , ToolDef
      { tdName        = "get_logs"
      , tdDescription = "Get recent log entries from the topo-seer console. Supports level filtering and pagination."
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "level" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("Minimum log level" :: Text)
                  , "enum" .= (["debug", "info", "warn", "error"] :: [Text])
                  ]
              , "limit" .= object
                  [ "type" .= ("integer" :: Text)
                  , "description" .= ("Maximum entries to return (default 50, max 1000)" :: Text)
                  ]
              , "offset" .= object
                  [ "type" .= ("integer" :: Text)
                  , "description" .= ("Number of entries to skip, newest-first (default 0)" :: Text)
                  ]
              ]
          ]
      }
  -- World naming
  , ToolDef
      { tdName        = "set_world_name"
      , tdDescription = "Set the display name of the current world"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "name" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("World name" :: Text)
                  ]
              ]
          , "required" .= (["name"] :: [Text])
          ]
      }
  -- Pipeline stage control
  , ToolDef
      { tdName        = "get_pipeline"
      , tdDescription = "List all pipeline stages (builtin and plugin) with their enabled/disabled status"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object []
          ]
      }
  , ToolDef
      { tdName        = "set_stage_enabled"
      , tdDescription = "Enable or disable a pipeline stage by canonical name"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "stage" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("Canonical stage name (e.g., base_height, erosion, climate, plugin:<name>)" :: Text)
                  ]
              , "enabled" .= object
                  [ "type" .= ("boolean" :: Text)
                  , "description" .= ("True to enable, false to disable" :: Text)
                  ]
              ]
          , "required" .= (["stage", "enabled"] :: [Text])
          ]
      }
  -- Plugin management
  , ToolDef
      { tdName        = "list_plugins"
      , tdDescription = "List loaded plugins with status, parameters, and RPC param specs"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object []
          ]
      }
  , ToolDef
      { tdName        = "set_plugin_enabled"
      , tdDescription = "Enable or disable a loaded plugin by name"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "plugin" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("Plugin name" :: Text)
                  ]
              , "enabled" .= object
                  [ "type" .= ("boolean" :: Text)
                  , "description" .= ("True to enable, false to disable" :: Text)
                  ]
              ]
          , "required" .= (["plugin", "enabled"] :: [Text])
          ]
      }
  , ToolDef
      { tdName        = "set_plugin_param"
      , tdDescription = "Set a parameter on a loaded plugin"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "plugin" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("Plugin name" :: Text)
                  ]
              , "param" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("Parameter key" :: Text)
                  ]
              , "value" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("Parameter value (string representation)" :: Text)
                  ]
              ]
          , "required" .= (["plugin", "param", "value"] :: [Text])
          ]
      }
  -- Simulation control
  , ToolDef
      { tdName        = "get_sim_state"
      , tdDescription = "Get current simulation state: tick count, auto-tick status, tick rate"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object []
          ]
      }
  , ToolDef
      { tdName        = "set_sim_auto_tick"
      , tdDescription = "Enable or disable automatic simulation ticking, optionally setting the tick rate"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "enabled" .= object
                  [ "type" .= ("boolean" :: Text)
                  , "description" .= ("True to enable auto-tick, false to disable" :: Text)
                  ]
              , "rate" .= object
                  [ "type" .= ("number" :: Text)
                  , "description" .= ("Ticks per second (optional, only if enabling)" :: Text)
                  ]
              ]
          , "required" .= (["enabled"] :: [Text])
          ]
      }
  , ToolDef
      { tdName        = "sim_tick"
      , tdDescription = "Advance the simulation by a given number of ticks (1-100, default 1)"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "count" .= object
                  [ "type" .= ("integer" :: Text)
                  , "description" .= ("Number of ticks to advance (1-100, default 1)" :: Text)
                  , "minimum" .= (1 :: Int)
                  , "maximum" .= (100 :: Int)
                  ]
              ]
          ]
      }
  -- Config summary
  , ToolDef
      { tdName        = "get_config_summary"
      , tdDescription = "Get a compact summary of all configuration sliders grouped by tab, with current and default values"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object []
          ]
      }
  -- Hex search and terrain export
  , ToolDef
      { tdName        = "find_hexes"
      , tdDescription = "Search for hex tiles matching filter criteria. Filters are objects with field, op (eq/ne/gt/lt/gte/lte), and value. Multiple filters are ANDed."
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "filters" .= object
                  [ "type" .= ("array" :: Text)
                  , "description" .= ("Array of filter objects {field, op, value}" :: Text)
                  , "items" .= object
                      [ "type" .= ("object" :: Text)
                      , "properties" .= object
                          [ "field" .= object
                              [ "type" .= ("string" :: Text)
                              , "description" .= ("Field name: elevation, moisture, biome, terrain_form, temperature, precipitation, etc." :: Text)
                              ]
                          , "op" .= object
                              [ "type" .= ("string" :: Text)
                              , "enum" .= (["eq", "ne", "gt", "lt", "gte", "lte"] :: [Text])
                              ]
                          , "value" .= object
                              [ "description" .= ("Comparison value (number or string)" :: Text)
                              ]
                          ]
                      , "required" .= (["field", "op", "value"] :: [Text])
                      ]
                  ]
              , "limit" .= object
                  [ "type" .= ("integer" :: Text)
                  , "description" .= ("Maximum results (default 100, max 1000)" :: Text)
                  ]
              ]
          , "required" .= (["filters"] :: [Text])
          ]
      }
  , ToolDef
      { tdName        = "export_terrain_data"
      , tdDescription = "Export terrain data arrays for specified chunks and fields. Returns arrays of values per chunk."
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "chunks" .= object
                  [ "type" .= ("array" :: Text)
                  , "description" .= ("Chunk IDs to export (omit for all)" :: Text)
                  , "items" .= object
                      [ "type" .= ("integer" :: Text)
                      ]
                  ]
              , "fields" .= object
                  [ "type" .= ("array" :: Text)
                  , "description" .= ("Field names to export: elevation, moisture, biome, temperature, etc." :: Text)
                  , "items" .= object
                      [ "type" .= ("string" :: Text)
                      ]
                  ]
              ]
          , "required" .= (["fields"] :: [Text])
          ]
      }
  -- Panel visibility & tab controls
  , ToolDef
      { tdName        = "set_left_panel"
      , tdDescription = "Show or hide the left sidebar panel"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "visible" .= object
                  [ "type" .= ("boolean" :: Text)
                  , "description" .= ("true to show, false to hide" :: Text)
                  ]
              ]
          , "required" .= (["visible"] :: [Text])
          ]
      }
  , ToolDef
      { tdName        = "set_left_tab"
      , tdDescription = "Switch the left panel tab. The left panel must be visible."
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "tab" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("Left panel tab" :: Text)
                  , "enum" .= (["topo", "view"] :: [Text])
                  ]
              ]
          , "required" .= (["tab"] :: [Text])
          ]
      }
  , ToolDef
      { tdName        = "toggle_config_panel"
      , tdDescription = "Toggle the config panel visibility, or explicitly set it"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "visible" .= object
                  [ "type" .= ("boolean" :: Text)
                  , "description" .= ("Optional explicit state; omit to toggle" :: Text)
                  ]
              ]
          ]
      }
  , ToolDef
      { tdName        = "set_log_collapsed"
      , tdDescription = "Collapse or expand the log panel"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "collapsed" .= object
                  [ "type" .= ("boolean" :: Text)
                  , "description" .= ("true to collapse, false to expand" :: Text)
                  ]
              ]
          , "required" .= (["collapsed"] :: [Text])
          ]
      }
  , ToolDef
      { tdName        = "set_log_level"
      , tdDescription = "Set the minimum log level filter for the log panel"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "level" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("Minimum log level" :: Text)
                  , "enum" .= (["debug", "info", "warn", "error"] :: [Text])
                  ]
              ]
          , "required" .= (["level"] :: [Text])
          ]
      }
  , ToolDef
      { tdName        = "get_ui_panels"
      , tdDescription = "Get the current visibility and tab state of all UI panels (left panel, config panel, log panel)"
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object []
          ]
      }
  -- Overlay navigation
  , ToolDef
      { tdName        = "set_overlay"
      , tdDescription = "Set the view to a specific overlay by name and optional field index. Use get_overlays to see available overlay names."
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "overlay" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("Overlay name" :: Text)
                  ]
              , "field_index" .= object
                  [ "type" .= ("integer" :: Text)
                  , "description" .= ("Field index within overlay (default 0)" :: Text)
                  , "minimum" .= (0 :: Int)
                  ]
              ]
          , "required" .= (["overlay"] :: [Text])
          ]
      }
  , ToolDef
      { tdName        = "list_overlay_fields"
      , tdDescription = "List the fields for an overlay. If overlay is omitted, uses the currently active overlay."
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "overlay" .= object
                  [ "type" .= ("string" :: Text)
                  , "description" .= ("Overlay name (optional — defaults to current)" :: Text)
                  ]
              ]
          ]
      }
  , ToolDef
      { tdName        = "cycle_overlay"
      , tdDescription = "Navigate to the next or previous overlay. Wraps around; position 0 returns to elevation (no overlay)."
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "direction" .= object
                  [ "type" .= ("integer" :: Text)
                  , "description" .= ("+1 for next overlay, -1 for previous" :: Text)
                  , "enum" .= ([(-1) :: Int, 1] :: [Int])
                  ]
              ]
          , "required" .= (["direction"] :: [Text])
          ]
      }
  , ToolDef
      { tdName        = "cycle_overlay_field"
      , tdDescription = "Navigate to the next or previous field within the current overlay. Only works when viewing an overlay."
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "direction" .= object
                  [ "type" .= ("integer" :: Text)
                  , "description" .= ("+1 for next field, -1 for previous" :: Text)
                  , "enum" .= ([(-1) :: Int, 1] :: [Int])
                  ]
              ]
          , "required" .= (["direction"] :: [Text])
          ]
      }
  -- Comprehensive UI state query
  , ToolDef
      { tdName        = "get_ui_state"
      , tdDescription = "Get a comprehensive snapshot of all UI state: panels, editor, data browser, overlays, hex selection, simulation. Use this to understand the full application state as a human would see it."
      , tdInputSchema = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object []
          ]
      }
  ]

-- | Handle a tools/call request.
--
-- Routes the tool name to the appropriate IPC command and returns the result.
handleToolCall
  :: IpcConnectionRef
  -> Text      -- ^ Tool name
  -> Value     -- ^ Tool arguments
  -> IO (Either Text ToolCallResult)
handleToolCall connRef toolName args = case toolToIpc toolName args of
  Nothing -> pure $ Left ("unknown tool: " <> toolName)
  Just (method, params) -> do
    result <- withConnection connRef $ \conn -> do
      (rsp, conn') <- sendCommand conn method params
      pure (rsp, conn')
    case result of
      Left err -> pure $ Right (mkToolError err)
      Right rsp
        | srSuccess rsp ->
            if toolName == "take_screenshot"
              then case srResult rsp of
                Object o ->
                  case (KM.lookup "image_base64" o, KM.lookup "format" o) of
                    (Just (String imageBase64), Just (String format))
                      | format == "png" -> pure $ Right (mkToolImageResult imageBase64 "image/png")
                    _ -> pure $ Right (mkToolError "invalid screenshot response")
                _ -> pure $ Right (mkToolError "invalid screenshot response")
              else
                let jsonText = Text.decodeUtf8 (BL.toStrict (Aeson.encode (srResult rsp)))
                in pure $ Right (mkToolResult jsonText)
        | otherwise ->
            case srError rsp of
              Just err -> pure $ Right (mkToolError err)
              Nothing  -> pure $ Right (mkToolError "unknown error")

-- | Map a tool name + args to an IPC method and params.
--
-- For most tools the mapping is 1:1 — the tool name is the IPC method
-- and the args are the IPC params.
toolToIpc :: Text -> Value -> Maybe (Text, Value)
toolToIpc "get_state"              args = Just ("get_state", args)
toolToIpc "list_sliders"           args = Just ("get_sliders", args)
toolToIpc "get_slider"             args = Just ("get_slider", args)
toolToIpc "set_slider"             args = Just ("set_slider", args)
toolToIpc "set_seed"               args = Just ("set_seed", args)
toolToIpc "set_view_mode"          args = Just ("set_view_mode", args)
toolToIpc "set_config_tab"         args = Just ("set_config_tab", args)
toolToIpc "get_view_modes"         args = Just ("get_view_modes", args)
toolToIpc "generate"               args = Just ("generate", args)
toolToIpc "editor_toggle"          args = Just ("editor_toggle", args)
toolToIpc "editor_set_tool"        args = Just ("editor_set_tool", args)
toolToIpc "editor_set_brush"       args = Just ("editor_set_brush", args)
toolToIpc "editor_brush_stroke"    args = Just ("editor_brush_stroke", args)
toolToIpc "editor_brush_line"      args = Just ("editor_brush_line", args)
toolToIpc "editor_set_biome"       args = Just ("editor_set_biome", args)
toolToIpc "editor_set_form"        args = Just ("editor_set_form", args)
toolToIpc "editor_set_hardness"    args = Just ("editor_set_hardness", args)
toolToIpc "editor_undo"            args = Just ("editor_undo", args)
toolToIpc "editor_redo"            args = Just ("editor_redo", args)
toolToIpc "editor_get_state"       args = Just ("editor_get_state", args)
-- Phase 3: Query tools
toolToIpc "get_enums"              args = Just ("get_enums", args)
toolToIpc "get_world_meta"         args = Just ("get_world_meta", args)
toolToIpc "get_generation_status"  args = Just ("get_generation_status", args)
toolToIpc "inspect_hex"            args = Just ("get_hex", args)
toolToIpc "get_chunks"             args = Just ("get_chunks", args)
toolToIpc "get_chunk_summary"      args = Just ("get_chunk_summary", args)
toolToIpc "get_terrain_stats"      args = Just ("get_terrain_stats", args)
toolToIpc "get_overlays"           args = Just ("get_overlays", args)
toolToIpc "list_worlds"            args = Just ("list_worlds", args)
-- Phase 4: Mutation tools
toolToIpc "set_sliders"            args = Just ("set_sliders", args)
toolToIpc "reset_sliders"          args = Just ("reset_sliders", args)
toolToIpc "select_hex"             args = Just ("select_hex", args)
toolToIpc "save_world"             args = Just ("save_world", args)
toolToIpc "load_world"             args = Just ("load_world", args)
toolToIpc "list_presets"           args = Just ("list_presets", args)
toolToIpc "save_preset"            args = Just ("save_preset", args)
toolToIpc "load_preset"            args = Just ("load_preset", args)
toolToIpc "take_screenshot"        args = Just ("take_screenshot", args)
-- Camera controls
toolToIpc "set_camera"             args = Just ("set_camera", args)
toolToIpc "get_camera"             args = Just ("get_camera", args)
toolToIpc "zoom_to_chunk"          args = Just ("zoom_to_chunk", args)
-- Log access
toolToIpc "get_logs"               args = Just ("get_logs", args)
-- World naming
toolToIpc "set_world_name"         args = Just ("set_world_name", args)
-- Pipeline stage control
toolToIpc "get_pipeline"           args = Just ("get_pipeline", args)
toolToIpc "set_stage_enabled"      args = Just ("set_stage_enabled", args)
-- Plugin management
toolToIpc "list_plugins"           args = Just ("list_plugins", args)
toolToIpc "set_plugin_enabled"     args = Just ("set_plugin_enabled", args)
toolToIpc "set_plugin_param"       args = Just ("set_plugin_param", args)
-- Simulation control
toolToIpc "get_sim_state"          args = Just ("get_sim_state", args)
toolToIpc "set_sim_auto_tick"      args = Just ("set_sim_auto_tick", args)
toolToIpc "sim_tick"               args = Just ("sim_tick", args)
-- Config summary
toolToIpc "get_config_summary"     args = Just ("get_config_summary", args)
-- Hex search and terrain export
toolToIpc "find_hexes"             args = Just ("find_hexes", args)
toolToIpc "export_terrain_data"    args = Just ("export_terrain_data", args)
-- Panel visibility & tab controls
toolToIpc "set_left_panel"         args = Just ("set_left_panel", args)
toolToIpc "set_left_tab"           args = Just ("set_left_tab", args)
toolToIpc "toggle_config_panel"    args = Just ("toggle_config_panel", args)
toolToIpc "set_log_collapsed"      args = Just ("set_log_collapsed", args)
toolToIpc "set_log_level"          args = Just ("set_log_level", args)
toolToIpc "get_ui_panels"          args = Just ("get_ui_panels", args)
-- Overlay navigation
toolToIpc "set_overlay"            args = Just ("set_overlay", args)
toolToIpc "list_overlay_fields"    args = Just ("list_overlay_fields", args)
toolToIpc "cycle_overlay"          args = Just ("cycle_overlay", args)
toolToIpc "cycle_overlay_field"    args = Just ("cycle_overlay_field", args)
-- Comprehensive UI state query
toolToIpc "get_ui_state"           args = Just ("get_ui_state", args)
toolToIpc _                        _    = Nothing
