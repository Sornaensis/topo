{-# LANGUAGE PatternSynonyms #-}

-- | Snow / Ice sub-biome refinement.
--
-- Discriminates glacier, ice cap, and snowfield from the
-- family-level 'BiomeSnow'.
module Topo.Biome.Refine.Snow
  ( SnowConfig(..)
  , defaultSnowConfig
  , refineSnow
  ) where

import Topo.Types (BiomeId, pattern BiomeSnow,
                   pattern BiomeGlacier, pattern BiomeIceCap,
                   pattern BiomeSnowfield)

-- | Configuration for snow sub-biome classification.
data SnowConfig = SnowConfig
  { snIceCapMaxTemp        :: !Float  -- ^ default 0.05
  , snGlacierMinIceThick   :: !Float  -- ^ default 0.10
  , snSnowfieldMinSnowpack :: !Float  -- ^ default 0.50
  } deriving (Eq, Show)

-- | Sensible defaults for snow refinement.
defaultSnowConfig :: SnowConfig
defaultSnowConfig = SnowConfig
  { snIceCapMaxTemp        = 0.05
  , snGlacierMinIceThick   = 0.10
  , snSnowfieldMinSnowpack = 0.50
  }

-- | Refine a snow tile into a sub-biome.
--
-- Decision cascade:
--
-- 1. Glacier: thick ice from 'GlacierChunk'
-- 2. Ice cap: extreme cold (permanent ice)
-- 3. Snowfield: heavy snowpack
-- 4. Fallback: 'BiomeSnow'
refineSnow
  :: SnowConfig
  -> Float -> Float -> Float
  -> BiomeId
refineSnow cfg temp iceThickness snowpack
  | iceThickness >= snGlacierMinIceThick cfg   = BiomeGlacier
  | temp <= snIceCapMaxTemp cfg                = BiomeIceCap
  | snowpack >= snSnowfieldMinSnowpack cfg     = BiomeSnowfield
  | otherwise                                  = BiomeSnow
