{-# LANGUAGE PatternSynonyms #-}

-- | Ocean sub-biome refinement.
--
-- Discriminates deep ocean, shallow sea, and coral reefs from the
-- family-level 'BiomeOcean' using elevation depth below water level
-- and temperature.
module Topo.Biome.Refine.Ocean
  ( OceanConfig(..)
  , defaultOceanConfig
  , refineOcean
  ) where

import Topo.Types (BiomeId, WaterBodyType,
                   pattern BiomeOcean, pattern BiomeDeepOcean,
                   pattern BiomeShallowSea, pattern BiomeCoralReef,
                   pattern WaterOcean)

-- | Configuration for ocean sub-biome classification.
data OceanConfig = OceanConfig
  { ocDeepThreshold :: !Float
    -- ^ How far below waterLevel counts as deep ocean (default 0.15).
  , ocCoralMinTemp  :: !Float
    -- ^ Minimum temperature for coral reefs (default 0.65).
  , ocCoralMaxDepth :: !Float
    -- ^ Maximum depth below waterLevel for coral (default 0.08).
  } deriving (Eq, Show)

-- | Sensible defaults for ocean refinement.
defaultOceanConfig :: OceanConfig
defaultOceanConfig = OceanConfig
  { ocDeepThreshold = 0.15
  , ocCoralMinTemp  = 0.65
  , ocCoralMaxDepth = 0.08
  }

-- | Refine an ocean tile into a sub-biome.
--
-- Decision cascade:
--
-- 1. Deep ocean: elevation far below water level
-- 2. Coral reef: shallow + warm + ocean-connected (NOT freshwater)
-- 3. Shallow sea: moderately below water level
-- 4. Fallback: 'BiomeOcean'
refineOcean :: OceanConfig -> WaterBodyType -> Float -> Float -> Float -> BiomeId
refineOcean cfg wbt waterLevel elev temp
  | depth > ocDeepThreshold cfg              = BiomeDeepOcean
  | depth <= ocCoralMaxDepth cfg
    && temp >= ocCoralMinTemp cfg
    && wbt == WaterOcean                     = BiomeCoralReef
  | depth > 0                                = BiomeShallowSea
  | otherwise                                = BiomeOcean
  where
    depth = waterLevel - elev
