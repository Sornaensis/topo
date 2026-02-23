{-# LANGUAGE DeriveGeneric #-}
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

import GHC.Generics (Generic)
import Topo.Config.JSON
  (ToJSON(..), FromJSON(..), configOptions, mergeDefaults,
   genericToJSON, genericParseJSON)
import Topo.Types (BiomeId, WaterBodyType,
                   pattern BiomeOcean, pattern BiomeDeepOcean,
                   pattern BiomeShallowSea, pattern BiomeCoralReef,
                   pattern WaterOcean)

-- | Configuration for ocean sub-biome classification.
data OceanConfig = OceanConfig
  { ocDeepThreshold     :: !Float
    -- ^ How far below waterLevel counts as deep ocean (default 0.10).
  , ocCoralMinTemp      :: !Float
    -- ^ Minimum temperature for coral reefs (default 0.72).
  , ocCoralMaxDepth     :: !Float
    -- ^ Maximum depth below waterLevel for coral (default 0.05).
  , ocCoralMaxSlope     :: !Float
    -- ^ Maximum slope for coral reefs — excludes steep drop-offs
    -- where reefs can't form (default 0.04).
  , ocCoralMinHardness  :: !Float
    -- ^ Minimum substrate hardness for coral reef attachment
    -- (default 0.35).
  } deriving (Eq, Show, Generic)

instance ToJSON OceanConfig where
  toJSON = genericToJSON (configOptions "oc")

instance FromJSON OceanConfig where
  parseJSON v = genericParseJSON (configOptions "oc")
                  (mergeDefaults (toJSON defaultOceanConfig) v)

-- | Sensible defaults for ocean refinement.
--
-- Shallow-sea zone extends from 0 to 0.10 depth below water level,
-- giving continental shelves visual identity.  Deep ocean begins at
-- depth > 0.10 (with waterLevel = 0.5 this means elev < 0.40),
-- capturing typical ocean-floor tiles as DeepOcean while continental
-- shelves at 0.42–0.48 remain ShallowSea.  Coral reefs require very
-- shallow (≤ 0.05), warm (≥ 0.704), flat (≤ 0.04 slope), hard
-- (≥ 0.35) substrate — making them a strict subset of warm shallow
-- water.
defaultOceanConfig :: OceanConfig
defaultOceanConfig = OceanConfig
  { ocDeepThreshold     = 0.10
  , ocCoralMinTemp      = 0.704
  , ocCoralMaxDepth     = 0.05
  , ocCoralMaxSlope     = 0.04
  , ocCoralMinHardness  = 0.35
  }

-- | Refine an ocean tile into a sub-biome.
--
-- Decision cascade:
--
-- 1. Deep ocean: elevation far below water level (depth > 'ocDeepThreshold')
-- 2. Coral reef: shallow + warm + ocean-connected + flat + hard substrate
-- 3. Shallow sea: any non-negative depth (depth >= 0)
-- 4. Fallback: 'BiomeShallowSea' (safety net for edge-case tiles)
refineOcean
  :: OceanConfig
  -> WaterBodyType
  -> Float          -- ^ waterLevel
  -> Float          -- ^ elevation
  -> Float          -- ^ temperature
  -> Float          -- ^ slope
  -> Float          -- ^ hardness
  -> BiomeId
refineOcean cfg wbt waterLevel elev temp slope hardness
  | depth > ocDeepThreshold cfg              = BiomeDeepOcean
  | depth <= ocCoralMaxDepth cfg
    && temp >= ocCoralMinTemp cfg
    && slope <= ocCoralMaxSlope cfg
    && hardness >= ocCoralMinHardness cfg
    && wbt == WaterOcean                     = BiomeCoralReef
  | depth >= 0                               = BiomeShallowSea
  | otherwise                                = BiomeShallowSea
  where
    depth = waterLevel - elev
