{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Desert sub-biome refinement.
--
-- Discriminates salt flat, rocky desert, sand desert, hot desert and
-- cold desert from the family-level 'BiomeDesert'.
module Topo.Biome.Refine.Desert
  ( DesertConfig(..)
  , defaultDesertConfig
  , refineDesert
  ) where

import GHC.Generics (Generic)
import Topo.Config.JSON
  (ToJSON(..), FromJSON(..), configOptions, mergeDefaults,
   genericToJSON, genericParseJSON)
import Topo.Types (BiomeId, TerrainForm,
                   pattern BiomeDesert, pattern BiomeSaltFlat,
                   pattern BiomeRockyDesert, pattern BiomeSandDesert,
                   pattern BiomeHotDesert, pattern BiomeColdDesert,
                   pattern BiomeFogDesert,
                   pattern FormDepression)

-- | Configuration for desert sub-biome classification.
data DesertConfig = DesertConfig
  { dcHotMinTemp        :: !Float  -- ^ default 0.70
  , dcColdMaxTemp       :: !Float  -- ^ default 0.40
  , dcRockyMinHardness  :: !Float  -- ^ default 0.55
  , dcRockyMaxSoilDepth :: !Float  -- ^ default 0.20
  , dcSandMaxHardness   :: !Float  -- ^ default 0.30
  , dcSaltFlatMaxMoist  :: !Float  -- ^ default 0.02 (extremely dry closed basin)
  , dcFogDesertMinHumidity :: !Float
  -- ^ Minimum humidity for fog-desert classification.  Fog deserts
  -- (Atacama, Namib) receive almost no rainfall but sustain moderate
  -- humidity from coastal fog advection.  Default: @0.30@.
  , dcFogDesertMaxPrecip   :: !Float
  -- ^ Maximum precipitation for fog-desert.  If real rain is reaching
  -- the tile it is not a fog desert.  Default: @0.08@.
  , dcFogDesertMaxSeason   :: !Float
  -- ^ Maximum precipitation seasonality for fog-desert.  Fog moisture
  -- is relatively uniform year-round; high seasonality indicates
  -- monsoonal rather than fog-driven moisture.  Default: @0.25@.
  } deriving (Eq, Show, Generic)

instance ToJSON DesertConfig where
  toJSON = genericToJSON (configOptions "dc")

instance FromJSON DesertConfig where
  parseJSON v = genericParseJSON (configOptions "dc")
                  (mergeDefaults (toJSON defaultDesertConfig) v)

-- | Sensible defaults for desert refinement.
defaultDesertConfig :: DesertConfig
defaultDesertConfig = DesertConfig
  { dcHotMinTemp        = 0.70
  , dcColdMaxTemp       = 0.40
  , dcRockyMinHardness  = 0.55
  , dcRockyMaxSoilDepth = 0.20
  , dcSandMaxHardness   = 0.30
  , dcSaltFlatMaxMoist  = 0.02
  , dcFogDesertMinHumidity = 0.30
  , dcFogDesertMaxPrecip   = 0.08
  , dcFogDesertMaxSeason   = 0.25
  }

-- | Refine a desert tile into a sub-biome.
--
-- Decision cascade:
--
-- 1. Salt flat: depression (closed basin) + extremely dry
-- 2. Fog desert: low precip + moderate humidity + low seasonality
-- 3. Rocky desert: hard rock, thin soil
-- 4. Sand desert: soft substrate
-- 5. Hot desert: high temperature
-- 6. Cold desert: low temperature
-- 7. Fallback: 'BiomeDesert'
refineDesert
  :: DesertConfig
  -> Float          -- ^ temperature
  -> Float          -- ^ moisture
  -> Float          -- ^ hardness
  -> Float          -- ^ soil depth
  -> TerrainForm    -- ^ terrain form
  -> Float          -- ^ humidity average (annual, normalised 0–1)
  -> Float          -- ^ precipitation seasonality (0–1)
  -> BiomeId
refineDesert cfg temp moisture hardness soilDepth tf humidity precipSeason
  | tf == FormDepression
    && moisture <= dcSaltFlatMaxMoist cfg        = BiomeSaltFlat
  -- Fog desert: coastal desert with fog moisture (Atacama, Namib).
  -- Low precipitation but measurable humidity from ocean fog,
  -- with low seasonality (fog is relatively uniform).
  | humidity >= dcFogDesertMinHumidity cfg
    && moisture <= dcFogDesertMaxPrecip cfg
    && precipSeason <= dcFogDesertMaxSeason cfg  = BiomeFogDesert
  | hardness >= dcRockyMinHardness cfg
    && soilDepth <= dcRockyMaxSoilDepth cfg      = BiomeRockyDesert
  | hardness <= dcSandMaxHardness cfg            = BiomeSandDesert
  | temp >= dcHotMinTemp cfg                     = BiomeHotDesert
  | temp <= dcColdMaxTemp cfg                    = BiomeColdDesert
  | otherwise                                    = BiomeDesert
