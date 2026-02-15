{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Rainforest sub-biome refinement.
--
-- Discriminates temperate rainforest from tropical rainforest
-- within the family-level 'BiomeRainforest'.
module Topo.Biome.Refine.Rainforest
  ( RainforestConfig(..)
  , defaultRainforestConfig
  , refineRainforest
  ) where

import GHC.Generics (Generic)
import Topo.Config.JSON
  (ToJSON(..), FromJSON(..), configOptions, mergeDefaults,
   genericToJSON, genericParseJSON)
import Topo.Types (BiomeId, pattern BiomeRainforest,
                   pattern BiomeTropicalRainforest,
                   pattern BiomeTempRainforest)

-- | Configuration for rainforest sub-biome classification.
data RainforestConfig = RainforestConfig
  { rfTropicalMinTemp         :: !Float  -- ^ default 0.74
  , rfTempRainforestMaxTemp   :: !Float  -- ^ default 0.76
  , rfTempRainforestMinPrecip :: !Float  -- ^ default 0.65
  } deriving (Eq, Show, Generic)

instance ToJSON RainforestConfig where
  toJSON = genericToJSON (configOptions "rf")

instance FromJSON RainforestConfig where
  parseJSON v = genericParseJSON (configOptions "rf")
                  (mergeDefaults (toJSON defaultRainforestConfig) v)

-- | Sensible defaults for rainforest refinement.
defaultRainforestConfig :: RainforestConfig
defaultRainforestConfig = RainforestConfig
  { rfTropicalMinTemp         = 0.74
  , rfTempRainforestMaxTemp   = 0.76
  , rfTempRainforestMinPrecip = 0.65
  }

-- | Refine a rainforest tile into a sub-biome.
--
-- Decision cascade:
--
-- 1. Temperate rainforest: cool + very wet
-- 2. Tropical rainforest: hot
-- 3. Fallback: 'BiomeRainforest'
refineRainforest
  :: RainforestConfig
  -> Float -> Float
  -> BiomeId
refineRainforest cfg temp precip
  | temp <= rfTempRainforestMaxTemp cfg
    && precip >= rfTempRainforestMinPrecip cfg  = BiomeTempRainforest
  | temp >= rfTropicalMinTemp cfg               = BiomeTropicalRainforest
  | otherwise                                   = BiomeRainforest
