{-# LANGUAGE OverloadedStrings #-}

-- | Tests for 'Topo.MCP.Tools' — tool definition completeness and
-- tool-to-IPC routing.
module Spec.Tools (spec) where

import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Test.Hspec

import Topo.MCP.Tools (allToolDefs, toolToIpc)
import Topo.MCP.Types (ToolDef(..))

spec :: Spec
spec = describe "Topo.MCP.Tools" $ do

  -- -----------------------------------------------------------------
  -- toolToIpc routing
  -- -----------------------------------------------------------------
  describe "toolToIpc" $ do
    it "maps get_state to get_state" $ do
      toolToIpc "get_state" Null `shouldBe` Just ("get_state", Null)

    it "maps list_sliders to get_sliders" $ do
      let args = object ["tab" .= ("terrain" :: Text)]
      toolToIpc "list_sliders" args `shouldBe` Just ("get_sliders", args)

    it "maps get_slider to get_slider" $ do
      let args = object ["name" .= ("SliderGenScale" :: Text)]
      toolToIpc "get_slider" args `shouldBe` Just ("get_slider", args)

    it "maps set_slider to set_slider" $ do
      let args = object ["name" .= ("SliderGenScale" :: Text), "value" .= (0.5 :: Double)]
      toolToIpc "set_slider" args `shouldBe` Just ("set_slider", args)

    it "maps set_seed to set_seed" $ do
      let args = object ["seed" .= (42 :: Int)]
      toolToIpc "set_seed" args `shouldBe` Just ("set_seed", args)

    it "maps set_view_mode to set_view_mode" $ do
      let args = object ["mode" .= ("biome" :: Text)]
      toolToIpc "set_view_mode" args `shouldBe` Just ("set_view_mode", args)

    it "maps set_config_tab to set_config_tab" $ do
      let args = object ["tab" .= ("climate" :: Text)]
      toolToIpc "set_config_tab" args `shouldBe` Just ("set_config_tab", args)

    it "maps get_view_modes to get_view_modes" $ do
      toolToIpc "get_view_modes" Null `shouldBe` Just ("get_view_modes", Null)

    it "maps generate to generate" $ do
      toolToIpc "generate" Null `shouldBe` Just ("generate", Null)

    it "maps editor_toggle to editor_toggle" $ do
      let args = object ["active" .= True]
      toolToIpc "editor_toggle" args `shouldBe` Just ("editor_toggle", args)

    it "maps editor_set_tool to editor_set_tool" $ do
      let args = object ["tool" .= ("erode" :: Text)]
      toolToIpc "editor_set_tool" args `shouldBe` Just ("editor_set_tool", args)

    it "maps editor_set_brush to editor_set_brush" $ do
      let args = object ["radius" .= (3 :: Int), "falloff" .= ("smooth" :: Text)]
      toolToIpc "editor_set_brush" args `shouldBe` Just ("editor_set_brush", args)

    it "maps editor_brush_stroke to editor_brush_stroke" $ do
      let args = object ["q" .= (2 :: Int), "r" .= ((-1) :: Int)]
      toolToIpc "editor_brush_stroke" args `shouldBe` Just ("editor_brush_stroke", args)

    it "maps editor_brush_line to editor_brush_line" $ do
      let args = object ["from_q" .= (0 :: Int), "from_r" .= (0 :: Int), "to_q" .= (3 :: Int), "to_r" .= ((-2) :: Int)]
      toolToIpc "editor_brush_line" args `shouldBe` Just ("editor_brush_line", args)

    it "maps editor_set_biome to editor_set_biome" $ do
      let args = object ["biome" .= ("Forest" :: Text)]
      toolToIpc "editor_set_biome" args `shouldBe` Just ("editor_set_biome", args)

    it "maps editor_set_form to editor_set_form" $ do
      let args = object ["form" .= ("Hilly" :: Text)]
      toolToIpc "editor_set_form" args `shouldBe` Just ("editor_set_form", args)

    it "maps editor_set_hardness to editor_set_hardness" $ do
      let args = object ["hardness" .= (0.7 :: Double)]
      toolToIpc "editor_set_hardness" args `shouldBe` Just ("editor_set_hardness", args)

    it "maps editor_undo to editor_undo" $ do
      toolToIpc "editor_undo" Null `shouldBe` Just ("editor_undo", Null)

    it "maps editor_redo to editor_redo" $ do
      toolToIpc "editor_redo" Null `shouldBe` Just ("editor_redo", Null)

    it "maps editor_get_state to editor_get_state" $ do
      toolToIpc "editor_get_state" Null `shouldBe` Just ("editor_get_state", Null)

    -- Phase 3 tools
    it "maps get_enums to get_enums" $ do
      let args = object ["type" .= ("biome" :: Text)]
      toolToIpc "get_enums" args `shouldBe` Just ("get_enums", args)

    it "maps get_world_meta to get_world_meta" $ do
      toolToIpc "get_world_meta" Null `shouldBe` Just ("get_world_meta", Null)

    it "maps get_generation_status to get_generation_status" $ do
      toolToIpc "get_generation_status" Null `shouldBe` Just ("get_generation_status", Null)

    it "maps inspect_hex to get_hex" $ do
      let args = object ["chunk" .= (0 :: Int), "tile" .= (5 :: Int)]
      toolToIpc "inspect_hex" args `shouldBe` Just ("get_hex", args)

    it "maps get_chunks to get_chunks" $ do
      toolToIpc "get_chunks" Null `shouldBe` Just ("get_chunks", Null)

    it "maps get_chunk_summary to get_chunk_summary" $ do
      let args = object ["chunk" .= (3 :: Int)]
      toolToIpc "get_chunk_summary" args `shouldBe` Just ("get_chunk_summary", args)

    it "maps get_terrain_stats to get_terrain_stats" $ do
      toolToIpc "get_terrain_stats" Null `shouldBe` Just ("get_terrain_stats", Null)

    it "maps get_overlays to get_overlays" $ do
      toolToIpc "get_overlays" Null `shouldBe` Just ("get_overlays", Null)

    it "maps list_worlds to list_worlds" $ do
      toolToIpc "list_worlds" Null `shouldBe` Just ("list_worlds", Null)

    -- Phase 4 tools
    it "maps set_sliders to set_sliders" $ do
      let args = object ["values" .= object ["SliderGenScale" .= (0.3 :: Double)]]
      toolToIpc "set_sliders" args `shouldBe` Just ("set_sliders", args)

    it "maps reset_sliders to reset_sliders" $ do
      let args = object ["tab" .= ("terrain" :: Text)]
      toolToIpc "reset_sliders" args `shouldBe` Just ("reset_sliders", args)

    it "maps select_hex to select_hex" $ do
      let args = object ["chunk" .= (0 :: Int), "tile" .= (5 :: Int)]
      toolToIpc "select_hex" args `shouldBe` Just ("select_hex", args)

    it "maps save_world to save_world" $ do
      let args = object ["name" .= ("test_world" :: Text)]
      toolToIpc "save_world" args `shouldBe` Just ("save_world", args)

    it "maps load_world to load_world" $ do
      let args = object ["name" .= ("test_world" :: Text)]
      toolToIpc "load_world" args `shouldBe` Just ("load_world", args)

    it "maps list_presets to list_presets" $ do
      toolToIpc "list_presets" Null `shouldBe` Just ("list_presets", Null)

    it "maps save_preset to save_preset" $ do
      let args = object ["name" .= ("my_preset" :: Text)]
      toolToIpc "save_preset" args `shouldBe` Just ("save_preset", args)

    it "maps load_preset to load_preset" $ do
      let args = object ["name" .= ("my_preset" :: Text)]
      toolToIpc "load_preset" args `shouldBe` Just ("load_preset", args)

    it "maps take_screenshot to take_screenshot" $ do
      toolToIpc "take_screenshot" Null `shouldBe` Just ("take_screenshot", Null)

    it "returns Nothing for unknown tool" $ do
      toolToIpc "nonexistent_tool" Null `shouldBe` Nothing

  -- -----------------------------------------------------------------
  -- allToolDefs
  -- -----------------------------------------------------------------
  describe "allToolDefs" $ do
    it "is non-empty" $
      allToolDefs `shouldSatisfy` (not . null)

    it "contains expected tool names" $ do
      let names = map tdName allToolDefs
      -- Phase 1-2 tools
      names `shouldSatisfy` elem "get_state"
      names `shouldSatisfy` elem "list_sliders"
      names `shouldSatisfy` elem "get_slider"
      names `shouldSatisfy` elem "set_slider"
      names `shouldSatisfy` elem "set_seed"
      names `shouldSatisfy` elem "set_view_mode"
      names `shouldSatisfy` elem "set_config_tab"
      names `shouldSatisfy` elem "get_view_modes"
      names `shouldSatisfy` elem "generate"
      names `shouldSatisfy` elem "editor_toggle"
      names `shouldSatisfy` elem "editor_set_tool"
      names `shouldSatisfy` elem "editor_set_brush"
      names `shouldSatisfy` elem "editor_brush_stroke"
      names `shouldSatisfy` elem "editor_brush_line"
      names `shouldSatisfy` elem "editor_set_biome"
      names `shouldSatisfy` elem "editor_set_form"
      names `shouldSatisfy` elem "editor_set_hardness"
      names `shouldSatisfy` elem "editor_undo"
      names `shouldSatisfy` elem "editor_redo"
      names `shouldSatisfy` elem "editor_get_state"
      -- Phase 3 tools
      names `shouldSatisfy` elem "get_enums"
      names `shouldSatisfy` elem "get_world_meta"
      names `shouldSatisfy` elem "get_generation_status"
      names `shouldSatisfy` elem "inspect_hex"
      names `shouldSatisfy` elem "get_chunks"
      names `shouldSatisfy` elem "get_chunk_summary"
      names `shouldSatisfy` elem "get_terrain_stats"
      names `shouldSatisfy` elem "get_overlays"
      names `shouldSatisfy` elem "list_worlds"
      -- Phase 4 tools
      names `shouldSatisfy` elem "set_sliders"
      names `shouldSatisfy` elem "reset_sliders"
      names `shouldSatisfy` elem "select_hex"
      names `shouldSatisfy` elem "save_world"
      names `shouldSatisfy` elem "load_world"
      names `shouldSatisfy` elem "list_presets"
      names `shouldSatisfy` elem "save_preset"
      names `shouldSatisfy` elem "load_preset"
      names `shouldSatisfy` elem "take_screenshot"

    it "has exactly 73 tools" $
      length allToolDefs `shouldBe` 73

    it "all tools have non-empty descriptions" $
      all (not . null . show . tdDescription) allToolDefs `shouldBe` True

    it "all tools have object-type input schemas" $
      all hasObjectTypeSchema allToolDefs `shouldBe` True

    it "every tool with a known name is routable via toolToIpc" $ do
      let names = map tdName allToolDefs
      mapM_ (\n -> toolToIpc n Null `shouldSatisfy` isJust) names

-- =====================================================================
-- Helpers
-- =====================================================================

-- | Check that a tool def's input schema has @"type": "object"@.
hasObjectTypeSchema :: ToolDef -> Bool
hasObjectTypeSchema td = case tdInputSchema td of
  Object o -> KM.lookup "type" o == Just (String "object")
  _        -> False
