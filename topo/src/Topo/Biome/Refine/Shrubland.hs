{-# LANGUAGE PatternSynonyms #-}

-- | Shrubland sub-biome refinement.
--
-- Discriminates moorland, xeric shrubland, and Mediterranean
-- from the family-level 'BiomeShrubland'.
module Topo.Biome.Refine.Shrubland
  ( ShrublandConfig(..)
  , defaultShrublandConfig
  , refineShrubland
  ) where

import Topo.Types (BiomeId, TerrainForm,
                   pattern BiomeShrubland, pattern BiomeMediterranean,
                   pattern BiomeXericShrubland, pattern BiomeMoorland,
                   pattern FormHilly, pattern FormMountainous)

-- | Configuration for shrubland sub-biome classification.
data ShrublandConfig = ShrublandConfig
  { scMediterraneanMinTemp   :: !Float  -- ^ default 0.45
  , scMediterraneanMaxPrecip :: !Float  -- ^ default 0.35
  , scMediterraneanMinPrecipSeason :: !Float
  -- ^ Minimum precipitation seasonality for Mediterranean classification.
  -- Mediterranean biomes are defined by dry summers / wet winters, so
  -- high seasonality is a prerequisite.  Default: @0.35@.
  , scXericMaxMoisture       :: !Float  -- ^ default 0.15
  , scMoorlandMaxTemp        :: !Float  -- ^ default 0.40
  , scMoorlandMinMoisture    :: !Float  -- ^ default 0.50
  , scMoorlandMaxFertility   :: !Float  -- ^ default 0.25
  } deriving (Eq, Show)

-- | Sensible defaults for shrubland refinement.
defaultShrublandConfig :: ShrublandConfig
defaultShrublandConfig = ShrublandConfig
  { scMediterraneanMinTemp   = 0.45
  , scMediterraneanMaxPrecip = 0.35
  , scMediterraneanMinPrecipSeason = 0.35
  , scXericMaxMoisture       = 0.15
  , scMoorlandMaxTemp        = 0.40
  , scMoorlandMinMoisture    = 0.50
  , scMoorlandMaxFertility   = 0.25
  }

-- | Refine a shrubland tile into a sub-biome.
--
-- Decision cascade:
--
-- 1. Moorland: cool + wet + infertile + hilly terrain
-- 2. Xeric shrubland: very dry (bordering desert)
-- 3. Mediterranean: warm + dry + high precipitation seasonality
-- 4. Fallback: 'BiomeShrubland'
refineShrubland
  :: ShrublandConfig
  -> Float          -- ^ temperature
  -> Float          -- ^ precipitation
  -> Float          -- ^ moisture
  -> Float          -- ^ fertility
  -> TerrainForm
  -> Float          -- ^ precipitation seasonality (annual)
  -> BiomeId
refineShrubland cfg temp precip moisture fertility tf precipSeason
  | temp <= scMoorlandMaxTemp cfg
    && moisture >= scMoorlandMinMoisture cfg
    && fertility <= scMoorlandMaxFertility cfg
    && (tf == FormHilly || tf == FormMountainous) = BiomeMoorland
  | moisture <= scXericMaxMoisture cfg            = BiomeXericShrubland
  | temp >= scMediterraneanMinTemp cfg
    && precip <= scMediterraneanMaxPrecip cfg
    && precipSeason >= scMediterraneanMinPrecipSeason cfg = BiomeMediterranean
  | otherwise                                     = BiomeShrubland
