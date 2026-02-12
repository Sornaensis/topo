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

import Topo.Types (BiomeId, TerrainForm,
                   pattern BiomeDesert, pattern BiomeSaltFlat,
                   pattern BiomeRockyDesert, pattern BiomeSandDesert,
                   pattern BiomeHotDesert, pattern BiomeColdDesert,
                   pattern FormDepression)

-- | Configuration for desert sub-biome classification.
data DesertConfig = DesertConfig
  { dcHotMinTemp        :: !Float  -- ^ default 0.55
  , dcColdMaxTemp       :: !Float  -- ^ default 0.35
  , dcRockyMinHardness  :: !Float  -- ^ default 0.55
  , dcRockyMaxSoilDepth :: !Float  -- ^ default 0.20
  , dcSandMaxHardness   :: !Float  -- ^ default 0.30
  , dcSaltFlatMaxMoist  :: !Float  -- ^ default 0.02 (extremely dry closed basin)
  } deriving (Eq, Show)

-- | Sensible defaults for desert refinement.
defaultDesertConfig :: DesertConfig
defaultDesertConfig = DesertConfig
  { dcHotMinTemp        = 0.55
  , dcColdMaxTemp       = 0.35
  , dcRockyMinHardness  = 0.55
  , dcRockyMaxSoilDepth = 0.20
  , dcSandMaxHardness   = 0.30
  , dcSaltFlatMaxMoist  = 0.02
  }

-- | Refine a desert tile into a sub-biome.
--
-- Decision cascade:
--
-- 1. Salt flat: depression (closed basin) + extremely dry
-- 2. Rocky desert: hard rock, thin soil
-- 3. Sand desert: soft substrate
-- 4. Hot desert: high temperature
-- 5. Cold desert: low temperature
-- 6. Fallback: 'BiomeDesert'
refineDesert
  :: DesertConfig
  -> Float -> Float -> Float -> Float -> TerrainForm
  -> BiomeId
refineDesert cfg temp moisture hardness soilDepth tf
  | tf == FormDepression
    && moisture <= dcSaltFlatMaxMoist cfg        = BiomeSaltFlat
  | hardness >= dcRockyMinHardness cfg
    && soilDepth <= dcRockyMaxSoilDepth cfg      = BiomeRockyDesert
  | hardness <= dcSandMaxHardness cfg            = BiomeSandDesert
  | temp >= dcHotMinTemp cfg                     = BiomeHotDesert
  | temp <= dcColdMaxTemp cfg                    = BiomeColdDesert
  | otherwise                                    = BiomeDesert
