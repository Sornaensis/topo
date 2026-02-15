{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Savanna sub-biome refinement.
--
-- Discriminates woodland savanna, tropical savanna, and grassland
-- savanna from the family-level 'BiomeSavanna'.
module Topo.Biome.Refine.Savanna
  ( SavannaConfig(..)
  , defaultSavannaConfig
  , refineSavanna
  ) where

import GHC.Generics (Generic)
import Topo.Config.JSON
  (ToJSON(..), FromJSON(..), configOptions, mergeDefaults,
   genericToJSON, genericParseJSON)
import Topo.Types (BiomeId, pattern BiomeSavanna,
                   pattern BiomeWoodlandSavanna,
                   pattern BiomeTropicalSavanna,
                   pattern BiomeGrasslandSavanna)

-- | Configuration for savanna sub-biome classification.
data SavannaConfig = SavannaConfig
  { saWoodlandMinPrecip    :: !Float  -- ^ default 0.35
  , saWoodlandMinFertility :: !Float  -- ^ default 0.40
  , saWoodlandMinHumidity  :: !Float
  -- ^ Minimum annual humidity for woodland savanna.  Higher humidity
  -- supports denser tree cover.  Default: @0.35@.
  , saTropicalMinTemp      :: !Float  -- ^ default 0.75
  , saGrasslandMaxPrecip   :: !Float  -- ^ default 0.25
  , saGrasslandMaxHumidity :: !Float
  -- ^ Maximum annual humidity for grassland savanna.  Drier
  -- conditions favour grass over trees.  Default: @0.30@.
  , saWoodlandMinSeason    :: !Float
  -- ^ Minimum precipitation seasonality for woodland savanna.
  -- High seasonality with adequate moisture and fertility is the
  -- defining characteristic of woodland savanna (e.g. Cerrado).
  -- Default: @0.35@.
  , saGrasslandMinSeason   :: !Float
  -- ^ Minimum precipitation seasonality for grassland savanna (dry
  -- savanna with strong wet/dry contrast).  Default: @0.50@.
  } deriving (Eq, Show, Generic)

instance ToJSON SavannaConfig where
  toJSON = genericToJSON (configOptions "sa")

instance FromJSON SavannaConfig where
  parseJSON v = genericParseJSON (configOptions "sa")
                  (mergeDefaults (toJSON defaultSavannaConfig) v)

-- | Sensible defaults for savanna refinement.
defaultSavannaConfig :: SavannaConfig
defaultSavannaConfig = SavannaConfig
  { saWoodlandMinPrecip    = 0.35
  , saWoodlandMinFertility = 0.40
  , saWoodlandMinHumidity  = 0.35
  , saTropicalMinTemp      = 0.75
  , saGrasslandMaxPrecip   = 0.25
  , saGrasslandMaxHumidity = 0.30
  , saWoodlandMinSeason    = 0.35
  , saGrasslandMinSeason   = 0.50
  }

-- | Refine a savanna tile into a sub-biome.
--
-- Decision cascade (seasonality is the primary discriminant):
--
-- 1. Woodland savanna: wet + fertile + humid + seasonal
-- 2. Tropical savanna: hot
-- 3. Grassland savanna: dry + low humidity + high seasonality
-- 4. Fallback: 'BiomeSavanna'
refineSavanna
  :: SavannaConfig
  -> Float          -- ^ temperature
  -> Float          -- ^ precipitation
  -> Float          -- ^ fertility
  -> Float          -- ^ humidity average (annual)
  -> Float          -- ^ precipitation seasonality (0–1)
  -> BiomeId
refineSavanna cfg temp precip fertility humidity precipSeason
  | precip >= saWoodlandMinPrecip cfg
    && fertility >= saWoodlandMinFertility cfg
    && humidity >= saWoodlandMinHumidity cfg
    && precipSeason >= saWoodlandMinSeason cfg   = BiomeWoodlandSavanna
  | temp >= saTropicalMinTemp cfg               = BiomeTropicalSavanna
  | precip <= saGrasslandMaxPrecip cfg
    && humidity <= saGrasslandMaxHumidity cfg
    && precipSeason >= saGrasslandMinSeason cfg  = BiomeGrasslandSavanna
  | otherwise                                   = BiomeSavanna
