{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Alpine sub-biome refinement.
--
-- Discriminates alpine scree, alpine tundra, and alpine meadow
-- from the family-level 'BiomeAlpine'.
module Topo.Biome.Refine.Alpine
  ( AlpineConfig(..)
  , defaultAlpineConfig
  , refineAlpine
  ) where

import GHC.Generics (Generic)
import Topo.Config.JSON
  (ToJSON(..), FromJSON(..), configOptions, mergeDefaults,
   genericToJSON, genericParseJSON)
import Topo.Types (BiomeId, pattern BiomeAlpine,
                   pattern BiomeAlpineScree, pattern BiomeAlpineTundra,
                   pattern BiomeAlpineMeadow)

-- | Configuration for alpine sub-biome classification.
--
-- Temperature-primary thresholds: alpine tundra and meadow are now
-- discriminated by temperature within the alpine band, not by fixed
-- elevation.  Humidity is used as a secondary axis for meadow detection.
data AlpineConfig = AlpineConfig
  { alScreeMinRuggedness :: !Float  -- ^ default 0.30
  , alScreeMaxSoilDepth  :: !Float  -- ^ default 0.10
  , alMeadowMaxTemp      :: !Float
  -- ^ Maximum temperature for alpine meadow (lower alpine band).
  --   Tiles warmer than this fall through to generic Alpine.  Default: @0.30@.
  , alMeadowMinPrecip    :: !Float  -- ^ default 0.30
  , alMeadowMinHumidity  :: !Float
  -- ^ Minimum humidity for alpine meadow.  High humidity + moderate precip
  --   at the alpine edge produces meadow instead of scree.  Default: @0.30@.
  , alTundraMaxTemp      :: !Float
  -- ^ Maximum temperature for alpine tundra.  Cold alpine tiles below this
  --   become alpine tundra.  Default: @0.15@.
  } deriving (Eq, Show, Generic)

instance ToJSON AlpineConfig where
  toJSON = genericToJSON (configOptions "al")

instance FromJSON AlpineConfig where
  parseJSON v = genericParseJSON (configOptions "al")
                  (mergeDefaults (toJSON defaultAlpineConfig) v)

-- | Sensible defaults for alpine refinement.
defaultAlpineConfig :: AlpineConfig
defaultAlpineConfig = AlpineConfig
  { alScreeMinRuggedness = 0.30
  , alScreeMaxSoilDepth  = 0.10
  , alMeadowMaxTemp      = 0.30
  , alMeadowMinPrecip    = 0.30
  , alMeadowMinHumidity  = 0.30
  , alTundraMaxTemp      = 0.15
  }

-- | Refine an alpine tile into a sub-biome.
--
-- Decision cascade:
--
-- 1. Alpine scree: very rugged + thin soil
-- 2. Alpine tundra: cold (temperature below 'alTundraMaxTemp')
-- 3. Alpine meadow: warmer alpine band + adequate precip + humidity
-- 4. Fallback: 'BiomeAlpine'
refineAlpine
  :: AlpineConfig
  -> Float -> Float -> Float -> Float -> Float -> Float -> Float
  -> BiomeId
refineAlpine cfg _elev temp precip _slope ruggedness soilDepth humidity
  | ruggedness >= alScreeMinRuggedness cfg
    && soilDepth <= alScreeMaxSoilDepth cfg    = BiomeAlpineScree
  | temp <= alTundraMaxTemp cfg                = BiomeAlpineTundra
  | temp <= alMeadowMaxTemp cfg
    && precip >= alMeadowMinPrecip cfg
    && humidity >= alMeadowMinHumidity cfg      = BiomeAlpineMeadow
  | otherwise                                  = BiomeAlpine
