{-# LANGUAGE PatternSynonyms #-}

-- | Grassland sub-biome refinement.
--
-- Discriminates alpine meadow, floodplain grassland, steppe, and
-- prairie from the family-level 'BiomeGrassland'.
module Topo.Biome.Refine.Grassland
  ( GrasslandConfig(..)
  , defaultGrasslandConfig
  , refineGrassland
  ) where

import Topo.Types (BiomeId, pattern BiomeGrassland,
                   pattern BiomeSteppe, pattern BiomePrairie,
                   pattern BiomeAlpineMeadow, pattern BiomeFloodplainGrassland)

-- | Configuration for grassland sub-biome classification.
data GrasslandConfig = GrasslandConfig
  { gcSteppeMaxPrecip        :: !Float  -- ^ default 0.25
  , gcSteppeMinTempRange     :: !Float
  -- ^ Minimum temperature range for continental steppe classification.
  -- High annual temperature range (continental climate) with low precip
  -- is characteristic of steppe.  Default: @0.15@.
  , gcPrairieMinSoilDepth    :: !Float  -- ^ default 0.60
  , gcPrairieMinMoisture     :: !Float  -- ^ default 0.35
  , gcAlpineMeadowMinElev    :: !Float  -- ^ default 0.55
  , gcFloodplainMinDischarge :: !Float  -- ^ default 0.25
  } deriving (Eq, Show)

-- | Sensible defaults for grassland refinement.
defaultGrasslandConfig :: GrasslandConfig
defaultGrasslandConfig = GrasslandConfig
  { gcSteppeMaxPrecip        = 0.25
  , gcSteppeMinTempRange     = 0.15
  , gcPrairieMinSoilDepth    = 0.60
  , gcPrairieMinMoisture     = 0.35
  , gcAlpineMeadowMinElev    = 0.55
  , gcFloodplainMinDischarge = 0.25
  }

-- | Refine a grassland tile into a sub-biome.
--
-- Decision cascade:
--
-- 1. Alpine meadow: high elevation
-- 2. Floodplain: high river discharge
-- 3. Steppe: low precipitation + continental (high temp range)
-- 4. Prairie: deep soil + adequate moisture
-- 5. Fallback: 'BiomeGrassland'
refineGrassland
  :: GrasslandConfig
  -> Float          -- ^ elevation
  -> Float          -- ^ precipitation
  -> Float          -- ^ soil depth
  -> Float          -- ^ moisture
  -> Float          -- ^ discharge
  -> Float          -- ^ temperature range (annual)
  -> BiomeId
refineGrassland cfg elev precip soilDepth moisture discharge tempRange
  | elev >= gcAlpineMeadowMinElev cfg            = BiomeAlpineMeadow
  | discharge >= gcFloodplainMinDischarge cfg     = BiomeFloodplainGrassland
  | precip <= gcSteppeMaxPrecip cfg
    && tempRange >= gcSteppeMinTempRange cfg      = BiomeSteppe
  | soilDepth >= gcPrairieMinSoilDepth cfg
    && moisture >= gcPrairieMinMoisture cfg        = BiomePrairie
  | otherwise                                     = BiomeGrassland
