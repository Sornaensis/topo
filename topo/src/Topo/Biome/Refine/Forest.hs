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

import Topo.Types (BiomeId, TerrainForm,
                   pattern BiomeForest,
                   pattern BiomeTropicalDryForest, pattern BiomeTempDeciduousForest,
                   pattern BiomeTempConiferousForest, pattern BiomeMontaneForest,
                   pattern BiomeCloudForest, pattern BiomeTempRainforest,
                   pattern FormHilly, pattern FormMountainous)

-- | Configuration for forest sub-biome classification.
data ForestConfig = ForestConfig
  { fcTropicalDryMinTemp      :: !Float  -- ^ default 0.74
  , fcTropicalDryMaxPrecip    :: !Float  -- ^ default 0.60
  , fcDeciduousMinTemp        :: !Float  -- ^ default 0.35
  , fcDeciduousMinPrecip      :: !Float  -- ^ default 0.40
  , fcConiferousMaxTemp       :: !Float  -- ^ default 0.58
  , fcConiferousMinHardness   :: !Float  -- ^ default 0.40
  , fcMontaneMinElev          :: !Float  -- ^ default 0.64
  , fcMontaneMinSlope         :: !Float
  -- ^ Minimum slope for montane classification on flat/rolling terrain.
  -- Tiles below 'fcMontaneMinElev' are never montane regardless of slope.
  -- Mountainous / hilly terrain forms bypass this check.  Default: @0.06@.
  , fcCloudForestMinElev      :: !Float  -- ^ default 0.68
  , fcCloudForestMinPrecip    :: !Float  -- ^ default 0.70
  , fcCloudForestMinTemp      :: !Float  -- ^ default 0.55
  , fcCloudForestMinHumidity  :: !Float  -- ^ default 0.65
  , fcTempRainforestMinPrecip :: !Float  -- ^ default 0.80
  , fcTempRainforestMaxTemp   :: !Float  -- ^ default 0.55
  , fcTempRainforestMinHumidity :: !Float  -- ^ default 0.70
  } deriving (Eq, Show)

-- | Sensible defaults for forest refinement.
--
-- Montane elevation threshold (0.64) places the montane line at ~20%
-- above normalised sea level (0.50), corresponding to ~1500–2500 m.
-- Cloud forest threshold (0.68) sits above montane.  Both also require
-- either steep slope or hilly/mountainous terrain form to prevent flat
-- plateaus from being classified as montane.
defaultForestConfig :: ForestConfig
defaultForestConfig = ForestConfig
  { fcTropicalDryMinTemp      = 0.74
  , fcTropicalDryMaxPrecip    = 0.60
  , fcDeciduousMinTemp        = 0.35
  , fcDeciduousMinPrecip      = 0.40
  , fcConiferousMaxTemp       = 0.58
  , fcConiferousMinHardness   = 0.40
  , fcMontaneMinElev          = 0.64
  , fcMontaneMinSlope         = 0.06
  , fcCloudForestMinElev      = 0.68
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
--    (also requires steep slope or hilly/mountainous terrain)
-- 2. Montane forest: high elevation + (steep slope or hilly/mountainous)
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
  -> Float          -- ^ slope
  -> TerrainForm    -- ^ terrain form
  -> BiomeId
refineForest cfg elev temp precip hardness humidity slope tform
  | elev >= fcCloudForestMinElev cfg
    && temp >= fcCloudForestMinTemp cfg
    && precip >= fcCloudForestMinPrecip cfg
    && humidity >= fcCloudForestMinHumidity cfg
    && isMontaneTerrain                        = BiomeCloudForest
  | elev >= fcMontaneMinElev cfg
    && isMontaneTerrain                        = BiomeMontaneForest
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
  where
    -- Montane terrain: hilly/mountainous form, or slope above threshold.
    isMontaneTerrain =
      tform == FormHilly || tform == FormMountainous
      || slope >= fcMontaneMinSlope cfg
