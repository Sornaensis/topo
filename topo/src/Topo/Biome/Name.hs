{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Display names for biome identifiers.
--
-- Provides a total function mapping every known 'BiomeId' to a
-- human-readable 'Text' label. Unknown codes produce an
-- @\"Unknown (n)\"@ fallback so the function is safe for all inputs.
module Topo.Biome.Name
  ( biomeDisplayName
  ) where

import Data.Text (Text, pack)
import Data.Word (Word16)
import Topo.Types (BiomeId, biomeIdToCode)

-- | Total mapping from 'BiomeId' to a user-facing display name.
--
-- Every known biome code (0–60, excluding the unused gap at 9) maps
-- to a descriptive name. Unknown codes produce @\"Unknown (n)\"@.
biomeDisplayName :: BiomeId -> Text
biomeDisplayName bid = nameFromCode (biomeIdToCode bid)

-- Internal lookup by raw code, kept as a case expression for
-- exhaustive coverage and O(1) compilation via jump tables.
nameFromCode :: Word16 -> Text
nameFromCode code = case code of
  -- Primary biomes (family-level)
  0  -> "Desert"
  1  -> "Grassland"
  2  -> "Forest"
  3  -> "Tundra"
  4  -> "Rainforest"
  5  -> "Shrubland"
  6  -> "Savanna"
  7  -> "Taiga"
  8  -> "Swamp"
  10 -> "Ocean"
  11 -> "Snow"
  12 -> "Coastal"
  13 -> "Alpine"
  -- Forest sub-biomes
  14 -> "Tropical Dry Forest"
  15 -> "Temperate Deciduous Forest"
  16 -> "Temperate Coniferous Forest"
  -- Grassland sub-biome
  17 -> "Steppe"
  -- Shrubland sub-biome
  18 -> "Mediterranean"
  -- Swamp/Wetland sub-biome
  19 -> "Wetland"
  -- Forest sub-biome
  20 -> "Montane Forest"
  -- Snow/Ice sub-biome
  21 -> "Ice Cap"
  -- Savanna sub-biome
  22 -> "Tropical Savanna"
  -- Taiga/Boreal sub-biome
  23 -> "Boreal Forest"
  -- Coastal sub-biomes
  24 -> "Salt Marsh"
  25 -> "Coastal Dunes"
  26 -> "Mangrove"
  27 -> "Estuary"
  28 -> "Rocky Shore"
  29 -> "Coastal Scrub"
  -- Desert sub-biomes
  30 -> "Hot Desert"
  31 -> "Cold Desert"
  32 -> "Rocky Desert"
  33 -> "Sand Desert"
  34 -> "Salt Flat"
  -- Grassland sub-biomes
  35 -> "Prairie"
  36 -> "Alpine Meadow"
  37 -> "Floodplain"
  -- Forest sub-biomes
  38 -> "Cloud Forest"
  39 -> "Temperate Rainforest"
  -- Ocean sub-biomes
  40 -> "Deep Ocean"
  41 -> "Shallow Sea"
  42 -> "Coral Reef"
  -- Tundra sub-biomes
  43 -> "Arctic Tundra"
  44 -> "Alpine Tundra"
  45 -> "Polar Desert"
  -- Rainforest sub-biome
  46 -> "Tropical Rainforest"
  -- Shrubland sub-biomes
  47 -> "Xeric Shrubland"
  48 -> "Moorland"
  -- Savanna sub-biomes
  49 -> "Woodland Savanna"
  50 -> "Grassland Savanna"
  -- Taiga/Boreal sub-biome
  51 -> "Boreal Bog"
  -- Swamp/Wetland sub-biomes
  52 -> "Marsh"
  53 -> "Bog"
  54 -> "Fen"
  55 -> "Floodplain Forest"
  -- Snow/Ice sub-biomes
  56 -> "Glacier"
  57 -> "Snowfield"
  -- Alpine sub-biome
  58 -> "Alpine Scree"
  -- Volcanic family
  59 -> "Lava Field"
  60 -> "Volcanic Ash Plain"
  -- Water body biomes
  61 -> "Lake"
  62 -> "Inland Sea"
  -- Unknown fallback
  n  -> "Unknown (" <> pack (show n) <> ")"
