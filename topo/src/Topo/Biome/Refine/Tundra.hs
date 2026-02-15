{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Tundra sub-biome refinement.
--
-- Discriminates polar desert, alpine tundra, and arctic tundra from
-- the family-level 'BiomeTundra'.
module Topo.Biome.Refine.Tundra
  ( TundraConfig(..)
  , defaultTundraConfig
  , refineTundra
  ) where

import GHC.Generics (Generic)
import Topo.Config.JSON
  (ToJSON(..), FromJSON(..), configOptions, mergeDefaults,
   genericToJSON, genericParseJSON)
import Topo.Types (BiomeId, TerrainForm,
                   pattern BiomeTundra, pattern BiomePolarDesert,
                   pattern BiomeAlpineTundra, pattern BiomeArcticTundra,
                   pattern FormHilly, pattern FormMountainous)

-- | Configuration for tundra sub-biome classification.
data TundraConfig = TundraConfig
  { tcArcticMaxTemp        :: !Float  -- ^ default 0.10
  , tcAlpineTundraMinElev  :: !Float  -- ^ default 0.60
  , tcPolarDesertMaxPrecip :: !Float  -- ^ default 0.10
  } deriving (Eq, Show, Generic)

instance ToJSON TundraConfig where
  toJSON = genericToJSON (configOptions "tc")

instance FromJSON TundraConfig where
  parseJSON v = genericParseJSON (configOptions "tc")
                  (mergeDefaults (toJSON defaultTundraConfig) v)

-- | Sensible defaults for tundra refinement.
defaultTundraConfig :: TundraConfig
defaultTundraConfig = TundraConfig
  { tcArcticMaxTemp        = 0.10
  , tcAlpineTundraMinElev  = 0.60
  , tcPolarDesertMaxPrecip = 0.10
  }

-- | Refine a tundra tile into a sub-biome.
--
-- Decision cascade:
--
-- 1. Polar desert: extreme cold + very dry
-- 2. Alpine tundra: high elevation + hilly\/mountainous terrain
-- 3. Arctic tundra: extreme cold
-- 4. Fallback: 'BiomeTundra'
refineTundra
  :: TundraConfig
  -> Float -> Float -> Float -> TerrainForm
  -> BiomeId
refineTundra cfg elev temp precip tf
  | temp <= tcArcticMaxTemp cfg
    && precip <= tcPolarDesertMaxPrecip cfg      = BiomePolarDesert
  | elev >= tcAlpineTundraMinElev cfg
    && (tf == FormHilly || tf == FormMountainous) = BiomeAlpineTundra
  | temp <= tcArcticMaxTemp cfg                  = BiomeArcticTundra
  | otherwise                                    = BiomeTundra
