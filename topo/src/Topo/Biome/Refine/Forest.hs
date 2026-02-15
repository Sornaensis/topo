{-# LANGUAGE DeriveGeneric #-}
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

import GHC.Generics (Generic)
import Topo.Config.JSON
  (ToJSON(..), FromJSON(..), configOptions, mergeDefaults,
   genericToJSON, genericParseJSON)
import Topo.Types (BiomeId, TerrainForm,
                   pattern BiomeForest,
                   pattern BiomeTropicalDryForest, pattern BiomeTempDeciduousForest,
                   pattern BiomeTempConiferousForest, pattern BiomeMontaneForest,
                   pattern BiomeCloudForest, pattern BiomeTempRainforest,
                   pattern BiomeTropicalSeasonalForest,
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
  , fcSeasonalForestMinSeason :: !Float
  -- ^ Minimum precipitation seasonality for tropical seasonal / monsoon
  -- forest detection.  High seasonality + warm temperature drives
  -- deciduous canopy patterns characteristic of monsoon forests.
  -- Default: @0.45@.
  , fcSeasonalForestMinTemp   :: !Float
  -- ^ Minimum temperature for tropical seasonal forest classification.
  -- Only warm tiles qualify; cooler forests with high seasonality become
  -- temperate deciduous instead.  Default: @0.65@.
  } deriving (Eq, Show, Generic)

instance ToJSON ForestConfig where
  toJSON = genericToJSON (configOptions "fc")

instance FromJSON ForestConfig where
  parseJSON v = genericParseJSON (configOptions "fc")
                  (mergeDefaults (toJSON defaultForestConfig) v)

-- | Sensible defaults for forest refinement.
--
-- Elevation thresholds for montane and cloud forest are relatively low
-- because the primary classifier's temperature-primary montane guard
-- already ensures only appropriately cold tiles reach the Forest family
-- at elevation.  These thresholds provide a secondary filter.
defaultForestConfig :: ForestConfig
defaultForestConfig = ForestConfig
  { fcTropicalDryMinTemp      = 0.74
  , fcTropicalDryMaxPrecip    = 0.60
  , fcDeciduousMinTemp        = 0.35
  , fcDeciduousMinPrecip      = 0.40
  , fcConiferousMaxTemp       = 0.58
  , fcConiferousMinHardness   = 0.40
  , fcMontaneMinElev          = 0.56
  , fcMontaneMinSlope         = 0.06
  , fcCloudForestMinElev      = 0.60
  , fcCloudForestMinPrecip    = 0.70
  , fcCloudForestMinTemp      = 0.55
  , fcCloudForestMinHumidity  = 0.65
  , fcTempRainforestMinPrecip = 0.80
  , fcTempRainforestMaxTemp   = 0.55
  , fcTempRainforestMinHumidity = 0.70
  , fcSeasonalForestMinSeason = 0.45
  , fcSeasonalForestMinTemp   = 0.65
  }

-- | Refine a forest tile into a sub-biome.
--
-- Decision cascade:
--
-- 1. Cloud forest: high elevation + tropical + very wet + high humidity
--    (also requires steep slope or hilly/mountainous terrain)
-- 2. Montane forest: high elevation + (steep slope or hilly/mountainous)
-- 3. Temperate rainforest: cool + very wet + high humidity
-- 4. Tropical seasonal forest: warm + high precipitation seasonality
-- 5. Tropical dry forest: hot + moderate precip
-- 6. Temperate deciduous: warm + wet
-- 7. Temperate coniferous: cool + rocky soil
-- 8. Fallback: 'BiomeForest'
refineForest
  :: ForestConfig
  -> Float          -- ^ elevation
  -> Float          -- ^ temperature
  -> Float          -- ^ precipitation
  -> Float          -- ^ hardness
  -> Float          -- ^ humidity average (annual)
  -> Float          -- ^ slope
  -> TerrainForm    -- ^ terrain form
  -> Float          -- ^ precipitation seasonality (0–1)
  -> BiomeId
refineForest cfg elev temp precip hardness humidity slope tform precipSeason
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
  -- Tropical seasonal / monsoon forest: warm + high seasonality.
  -- This fires before tropical dry and deciduous so that monsoonal
  -- regions with moderate precipitation get their own sub-biome.
  | temp >= fcSeasonalForestMinTemp cfg
    && precipSeason >= fcSeasonalForestMinSeason cfg = BiomeTropicalSeasonalForest
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
