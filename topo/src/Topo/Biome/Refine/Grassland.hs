{-# LANGUAGE DeriveGeneric #-}
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

import GHC.Generics (Generic)
import Topo.Config.JSON
  (ToJSON(..), FromJSON(..), configOptions, mergeDefaults,
   genericToJSON, genericParseJSON)
import Topo.Types (BiomeId, pattern BiomeGrassland,
                   pattern BiomeSteppe, pattern BiomePrairie,
                   pattern BiomeAlpineMeadow, pattern BiomeFloodplainGrassland)

-- | Configuration for grassland sub-biome classification.
data GrasslandConfig = GrasslandConfig
  { grcSteppeMaxPrecip        :: !Float  -- ^ default 0.25
  , grcSteppeMinTempRange     :: !Float
  -- ^ Minimum temperature range for continental steppe classification.
  -- High annual temperature range (continental climate) with low precip
  -- is characteristic of steppe.  Default: @0.15@.
  , grcPrairieMinSoilDepth    :: !Float  -- ^ default 0.60
  , grcPrairieMinMoisture     :: !Float  -- ^ default 0.35
  , grcAlpineMeadowMinElev    :: !Float  -- ^ default 0.70
  , grcAlpineMeadowMaxTemp    :: !Float
  -- ^ Maximum temperature for alpine meadow classification.
  -- Prevents warm high-elevation tiles from being classified as alpine
  -- meadow (they should fall through to prairie/steppe/grassland).
  -- Default: @0.40@.
  , grcFloodplainMinDischarge :: !Float  -- ^ default 0.25
  } deriving (Eq, Show, Generic)

instance ToJSON GrasslandConfig where
  toJSON = genericToJSON (configOptions "grc")

instance FromJSON GrasslandConfig where
  parseJSON v = genericParseJSON (configOptions "grc")
                  (mergeDefaults (toJSON defaultGrasslandConfig) v)

-- | Sensible defaults for grassland refinement.
defaultGrasslandConfig :: GrasslandConfig
defaultGrasslandConfig = GrasslandConfig
  { grcSteppeMaxPrecip        = 0.25
  , grcSteppeMinTempRange     = 0.15
  , grcPrairieMinSoilDepth    = 0.60
  , grcPrairieMinMoisture     = 0.35
  , grcAlpineMeadowMinElev    = 0.70
  , grcAlpineMeadowMaxTemp    = 0.40
  , grcFloodplainMinDischarge = 0.25
  }

-- | Refine a grassland tile into a sub-biome.
--
-- Decision cascade:
--
-- 1. Alpine meadow: high elevation AND low temperature
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
  -> Float          -- ^ temperature (annual average, normalised 0–1)
  -> BiomeId
refineGrassland cfg elev precip soilDepth moisture discharge tempRange temp
  | elev >= grcAlpineMeadowMinElev cfg
    && temp < grcAlpineMeadowMaxTemp cfg           = BiomeAlpineMeadow
  | discharge >= grcFloodplainMinDischarge cfg     = BiomeFloodplainGrassland
  | precip <= grcSteppeMaxPrecip cfg
    && tempRange >= grcSteppeMinTempRange cfg      = BiomeSteppe
  | soilDepth >= grcPrairieMinSoilDepth cfg
    && moisture >= grcPrairieMinMoisture cfg        = BiomePrairie
  | otherwise                                     = BiomeGrassland
