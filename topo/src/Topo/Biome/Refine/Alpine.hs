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

import Topo.Types (BiomeId, pattern BiomeAlpine,
                   pattern BiomeAlpineScree, pattern BiomeAlpineTundra,
                   pattern BiomeAlpineMeadow)

-- | Configuration for alpine sub-biome classification.
data AlpineConfig = AlpineConfig
  { alScreeMinRuggedness :: !Float  -- ^ default 0.30
  , alScreeMaxSoilDepth  :: !Float  -- ^ default 0.10
  , alMeadowMaxElev      :: !Float  -- ^ default 0.80 (lower alpine band)
  , alMeadowMinPrecip    :: !Float  -- ^ default 0.30
  , alTundraMinElev      :: !Float  -- ^ default 0.80
  , alTundraMaxTemp      :: !Float  -- ^ default 0.15
  } deriving (Eq, Show)

-- | Sensible defaults for alpine refinement.
defaultAlpineConfig :: AlpineConfig
defaultAlpineConfig = AlpineConfig
  { alScreeMinRuggedness = 0.30
  , alScreeMaxSoilDepth  = 0.10
  , alMeadowMaxElev      = 0.80
  , alMeadowMinPrecip    = 0.30
  , alTundraMinElev      = 0.80
  , alTundraMaxTemp      = 0.15
  }

-- | Refine an alpine tile into a sub-biome.
--
-- Decision cascade:
--
-- 1. Alpine scree: very rugged + thin soil
-- 2. Alpine tundra: high elevation + cold
-- 3. Alpine meadow: lower alpine band + adequate precip
-- 4. Fallback: 'BiomeAlpine'
refineAlpine
  :: AlpineConfig
  -> Float -> Float -> Float -> Float -> Float -> Float
  -> BiomeId
refineAlpine cfg elev temp precip _slope ruggedness soilDepth
  | ruggedness >= alScreeMinRuggedness cfg
    && soilDepth <= alScreeMaxSoilDepth cfg    = BiomeAlpineScree
  | elev >= alTundraMinElev cfg
    && temp <= alTundraMaxTemp cfg             = BiomeAlpineTundra
  | elev <= alMeadowMaxElev cfg
    && precip >= alMeadowMinPrecip cfg         = BiomeAlpineMeadow
  | otherwise                                  = BiomeAlpine
