{-# LANGUAGE PatternSynonyms #-}

-- | Forest sub-biome refinement.
--
-- Discriminates cloud forest, montane forest, temperate rainforest,
-- tropical dry forest, temperate deciduous, and temperate coniferous
-- from the family-level 'BiomeForest'.
module Topo.Biome.Refine.Forest
  ( ForestConfig(..)
  , defaultForestConfig
  , refineForest
  ) where

import Topo.Types (BiomeId, pattern BiomeForest,
                   pattern BiomeTropicalDryForest, pattern BiomeTempDeciduousForest,
                   pattern BiomeTempConiferousForest, pattern BiomeMontaneForest,
                   pattern BiomeCloudForest, pattern BiomeTempRainforest)

-- | Configuration for forest sub-biome classification.
data ForestConfig = ForestConfig
  { fcTropicalDryMinTemp      :: !Float  -- ^ default 0.74
  , fcTropicalDryMaxPrecip    :: !Float  -- ^ default 0.60
  , fcDeciduousMinTemp        :: !Float  -- ^ default 0.35
  , fcDeciduousMinPrecip      :: !Float  -- ^ default 0.40
  , fcConiferousMaxTemp       :: !Float  -- ^ default 0.58
  , fcConiferousMinHardness   :: !Float  -- ^ default 0.40
  , fcMontaneMinElev          :: !Float  -- ^ default 0.50
  , fcCloudForestMinElev      :: !Float  -- ^ default 0.55
  , fcCloudForestMinPrecip    :: !Float  -- ^ default 0.70
  , fcCloudForestMinTemp      :: !Float  -- ^ default 0.55
  , fcCloudForestMinHumidity  :: !Float  -- ^ default 0.65
  , fcTempRainforestMinPrecip :: !Float  -- ^ default 0.80
  , fcTempRainforestMaxTemp   :: !Float  -- ^ default 0.55
  , fcTempRainforestMinHumidity :: !Float  -- ^ default 0.70
  } deriving (Eq, Show)

-- | Sensible defaults for forest refinement.
defaultForestConfig :: ForestConfig
defaultForestConfig = ForestConfig
  { fcTropicalDryMinTemp      = 0.74
  , fcTropicalDryMaxPrecip    = 0.60
  , fcDeciduousMinTemp        = 0.35
  , fcDeciduousMinPrecip      = 0.40
  , fcConiferousMaxTemp       = 0.58
  , fcConiferousMinHardness   = 0.40
  , fcMontaneMinElev          = 0.50
  , fcCloudForestMinElev      = 0.55
  , fcCloudForestMinPrecip    = 0.70
  , fcCloudForestMinTemp      = 0.55
  , fcCloudForestMinHumidity  = 0.65
  , fcTempRainforestMinPrecip = 0.80
  , fcTempRainforestMaxTemp   = 0.55
  , fcTempRainforestMinHumidity = 0.70
  }

-- | Refine a forest tile into a sub-biome.
--
-- Decision cascade:
--
-- 1. Cloud forest: high elevation + tropical + very wet + high humidity
-- 2. Montane forest: high elevation
-- 3. Temperate rainforest: cool + very wet + high humidity
-- 4. Tropical dry forest: hot + moderate precip
-- 5. Temperate deciduous: warm + wet
-- 6. Temperate coniferous: cool + rocky soil
-- 7. Fallback: 'BiomeForest'
refineForest
  :: ForestConfig
  -> Float          -- ^ elevation
  -> Float          -- ^ temperature
  -> Float          -- ^ precipitation
  -> Float          -- ^ hardness
  -> Float          -- ^ humidity average (annual)
  -> BiomeId
refineForest cfg elev temp precip hardness humidity
  | elev >= fcCloudForestMinElev cfg
    && temp >= fcCloudForestMinTemp cfg
    && precip >= fcCloudForestMinPrecip cfg
    && humidity >= fcCloudForestMinHumidity cfg = BiomeCloudForest
  | elev >= fcMontaneMinElev cfg               = BiomeMontaneForest
  | precip >= fcTempRainforestMinPrecip cfg
    && temp <= fcTempRainforestMaxTemp cfg
    && humidity >= fcTempRainforestMinHumidity cfg = BiomeTempRainforest
  | temp >= fcTropicalDryMinTemp cfg
    && precip <= fcTropicalDryMaxPrecip cfg     = BiomeTropicalDryForest
  | temp >= fcDeciduousMinTemp cfg
    && precip >= fcDeciduousMinPrecip cfg       = BiomeTempDeciduousForest
  | temp <= fcConiferousMaxTemp cfg
    && hardness >= fcConiferousMinHardness cfg  = BiomeTempConiferousForest
  | otherwise                                  = BiomeForest
