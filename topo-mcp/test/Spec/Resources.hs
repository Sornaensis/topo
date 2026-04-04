{-# LANGUAGE OverloadedStrings #-}

-- | Tests for 'Topo.MCP.Resources' — resource URI parsing and
-- resource definition completeness.
module Spec.Resources (spec) where

import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Test.Hspec

import Topo.MCP.Resources (allResourceDefs, allResourceTemplateDefs, parseResourceUri)
import Topo.MCP.Types (ResourceDef(..), ResourceTemplateDef(..))

spec :: Spec
spec = describe "Topo.MCP.Resources" $ do

  -- -----------------------------------------------------------------
  -- parseResourceUri
  -- -----------------------------------------------------------------
  describe "parseResourceUri" $ do
    it "parses topo://state" $ do
      let result = parseResourceUri "topo://state"
      result `shouldBe` Just ("get_state", object [])

    it "parses topo://sliders" $ do
      let result = parseResourceUri "topo://sliders"
      result `shouldBe` Just ("get_sliders", object [])

    it "parses topo://view-modes" $ do
      let result = parseResourceUri "topo://view-modes"
      result `shouldBe` Just ("get_view_modes", object [])

    it "parses topo://editor/state" $ do
      let result = parseResourceUri "topo://editor/state"
      result `shouldBe` Just ("editor_get_state", object [])

    it "parses topo://sliders/{tab}" $ do
      let result = parseResourceUri "topo://sliders/terrain"
      result `shouldBe` Just ("get_sliders", object ["tab" .= ("terrain" :: Text)])

    it "parses topo://sliders/{tab} for climate" $ do
      let result = parseResourceUri "topo://sliders/climate"
      result `shouldBe` Just ("get_sliders", object ["tab" .= ("climate" :: Text)])

    it "parses topo://slider/{name}" $ do
      let result = parseResourceUri "topo://slider/SliderGenScale"
      result `shouldBe` Just ("get_slider", object ["name" .= ("SliderGenScale" :: Text)])

    it "returns Nothing for unknown URIs" $ do
      parseResourceUri "topo://unknown" `shouldBe` Nothing

    it "returns Nothing for empty URI" $ do
      parseResourceUri "" `shouldBe` Nothing

    it "returns Nothing for non-topo URI" $ do
      parseResourceUri "http://example.com" `shouldBe` Nothing

    it "returns Nothing for topo://sliders/ with empty tab" $ do
      parseResourceUri "topo://sliders/" `shouldBe` Nothing

    it "returns Nothing for topo://slider/ with empty name" $ do
      parseResourceUri "topo://slider/" `shouldBe` Nothing

    -- Phase 3 static resources
    it "parses topo://world" $ do
      parseResourceUri "topo://world" `shouldBe` Just ("get_world_meta", object [])

    it "parses topo://generation-status" $ do
      parseResourceUri "topo://generation-status" `shouldBe` Just ("get_generation_status", object [])

    it "parses topo://chunks" $ do
      parseResourceUri "topo://chunks" `shouldBe` Just ("get_chunks", object [])

    it "parses topo://terrain-stats" $ do
      parseResourceUri "topo://terrain-stats" `shouldBe` Just ("get_terrain_stats", object [])

    it "parses topo://overlays" $ do
      parseResourceUri "topo://overlays" `shouldBe` Just ("get_overlays", object [])

    it "parses topo://worlds" $ do
      parseResourceUri "topo://worlds" `shouldBe` Just ("list_worlds", object [])

    -- Phase 3 template URIs
    it "parses topo://hex/{q}/{r}" $ do
      parseResourceUri "topo://hex/0/5" `shouldBe` Just ("get_hex", object ["q" .= (0 :: Int), "r" .= (5 :: Int)])

    it "parses topo://hex with negative q" $ do
      parseResourceUri "topo://hex/-3/10" `shouldBe` Just ("get_hex", object ["q" .= (-3 :: Int), "r" .= (10 :: Int)])

    it "returns Nothing for topo://hex/ with missing r" $ do
      parseResourceUri "topo://hex/0" `shouldBe` Nothing

    it "returns Nothing for topo://hex/ with empty r" $ do
      parseResourceUri "topo://hex/0/" `shouldBe` Nothing

    it "returns Nothing for topo://hex/ with non-numeric q" $ do
      parseResourceUri "topo://hex/abc/5" `shouldBe` Nothing

    it "parses topo://chunk/{id}" $ do
      parseResourceUri "topo://chunk/3" `shouldBe` Just ("get_chunk_summary", object ["chunk" .= (3 :: Int)])

    it "returns Nothing for topo://chunk/ with empty id" $ do
      parseResourceUri "topo://chunk/" `shouldBe` Nothing

    it "returns Nothing for topo://chunk/ with non-numeric id" $ do
      parseResourceUri "topo://chunk/abc" `shouldBe` Nothing

    it "parses topo://enums/{type}" $ do
      parseResourceUri "topo://enums/biome" `shouldBe` Just ("get_enums", object ["type" .= ("biome" :: Text)])

    it "parses topo://enums/terrain_form" $ do
      parseResourceUri "topo://enums/terrain_form" `shouldBe` Just ("get_enums", object ["type" .= ("terrain_form" :: Text)])

    it "returns Nothing for topo://enums/ with empty type" $ do
      parseResourceUri "topo://enums/" `shouldBe` Nothing

    -- Phase 4 static resources
    it "parses topo://presets" $ do
      parseResourceUri "topo://presets" `shouldBe` Just ("list_presets", object [])

  -- -----------------------------------------------------------------
  -- allResourceDefs
  -- -----------------------------------------------------------------
  describe "allResourceDefs" $ do
    it "is non-empty" $
      allResourceDefs `shouldSatisfy` (not . null)

    it "has exactly 11 resources" $
      length allResourceDefs `shouldBe` 11

    it "contains topo://state" $
      any (\rd -> rdUri rd == "topo://state") allResourceDefs `shouldBe` True

    it "contains topo://sliders" $
      any (\rd -> rdUri rd == "topo://sliders") allResourceDefs `shouldBe` True

    it "contains topo://view-modes" $
      any (\rd -> rdUri rd == "topo://view-modes") allResourceDefs `shouldBe` True

    it "contains topo://editor/state" $
      any (\rd -> rdUri rd == "topo://editor/state") allResourceDefs `shouldBe` True

    it "contains topo://world" $
      any (\rd -> rdUri rd == "topo://world") allResourceDefs `shouldBe` True

    it "contains topo://generation-status" $
      any (\rd -> rdUri rd == "topo://generation-status") allResourceDefs `shouldBe` True

    it "contains topo://chunks" $
      any (\rd -> rdUri rd == "topo://chunks") allResourceDefs `shouldBe` True

    it "contains topo://terrain-stats" $
      any (\rd -> rdUri rd == "topo://terrain-stats") allResourceDefs `shouldBe` True

    it "contains topo://overlays" $
      any (\rd -> rdUri rd == "topo://overlays") allResourceDefs `shouldBe` True

    it "contains topo://worlds" $
      any (\rd -> rdUri rd == "topo://worlds") allResourceDefs `shouldBe` True

    it "contains topo://presets" $
      any (\rd -> rdUri rd == "topo://presets") allResourceDefs `shouldBe` True

    it "all resources have application/json mime type" $
      all (\rd -> rdMimeType rd == "application/json") allResourceDefs `shouldBe` True

  -- -----------------------------------------------------------------
  -- allResourceTemplateDefs
  -- -----------------------------------------------------------------
  describe "allResourceTemplateDefs" $ do
    it "is non-empty" $
      allResourceTemplateDefs `shouldSatisfy` (not . null)

    it "has exactly 5 templates" $
      length allResourceTemplateDefs `shouldBe` 5

    it "contains topo://sliders/{tab} template" $
      any (\rtd -> rtdUriTemplate rtd == "topo://sliders/{tab}") allResourceTemplateDefs
        `shouldBe` True

    it "contains topo://slider/{name} template" $
      any (\rtd -> rtdUriTemplate rtd == "topo://slider/{name}") allResourceTemplateDefs
        `shouldBe` True

    it "contains topo://hex/{q}/{r} template" $
      any (\rtd -> rtdUriTemplate rtd == "topo://hex/{q}/{r}") allResourceTemplateDefs
        `shouldBe` True

    it "contains topo://chunk/{id} template" $
      any (\rtd -> rtdUriTemplate rtd == "topo://chunk/{id}") allResourceTemplateDefs
        `shouldBe` True

    it "contains topo://enums/{type} template" $
      any (\rtd -> rtdUriTemplate rtd == "topo://enums/{type}") allResourceTemplateDefs
        `shouldBe` True
