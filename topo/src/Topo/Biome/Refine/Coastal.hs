{-# LANGUAGE PatternSynonyms #-}

-- | Coastal sub-biome refinement.
--
-- Discriminates mangrove, estuary, salt marsh, rocky shore, coastal dunes,
-- and coastal scrub from the family-level 'BiomeCoastal'.
module Topo.Biome.Refine.Coastal
  ( CoastalConfig(..)
  , defaultCoastalConfig
  , refineCoastal
  ) where

import Topo.Types (BiomeId, pattern BiomeCoastal, pattern BiomeMangrove,
                   pattern BiomeEstuary, pattern BiomeSaltMarsh,
                   pattern BiomeRockyShore, pattern BiomeCoastalDunes,
                   pattern BiomeCoastalScrub)

-- | Configuration for coastal sub-biome classification.
data CoastalConfig = CoastalConfig
  { ccMangroveMinTemp     :: !Float  -- ^ default 0.65
  , ccMangroveMinPrecip   :: !Float  -- ^ default 0.50
  , ccSaltMarshMaxTemp    :: !Float  -- ^ default 0.55
  , ccSaltMarshMinMoist   :: !Float  -- ^ default 0.55
  , ccDunesMaxMoisture    :: !Float  -- ^ default 0.25
  , ccDunesMaxPrecip      :: !Float  -- ^ default 0.20
  , ccEstuaryMinDischarge :: !Float  -- ^ default 0.30
  , ccRockyMinHardness    :: !Float  -- ^ default 0.60
  , ccRockyMaxSoilDepth   :: !Float  -- ^ default 0.15
  , ccScrubMaxPrecip      :: !Float  -- ^ default 0.40
  , ccScrubMinTemp        :: !Float  -- ^ default 0.40
  } deriving (Eq, Show)

-- | Sensible defaults for coastal refinement.
defaultCoastalConfig :: CoastalConfig
defaultCoastalConfig = CoastalConfig
  { ccMangroveMinTemp     = 0.65
  , ccMangroveMinPrecip   = 0.50
  , ccSaltMarshMaxTemp    = 0.55
  , ccSaltMarshMinMoist   = 0.55
  , ccDunesMaxMoisture    = 0.25
  , ccDunesMaxPrecip      = 0.20
  , ccEstuaryMinDischarge = 0.30
  , ccRockyMinHardness    = 0.60
  , ccRockyMaxSoilDepth   = 0.15
  , ccScrubMaxPrecip      = 0.40
  , ccScrubMinTemp        = 0.40
  }

-- | Refine a coastal tile into a sub-biome.
--
-- Decision cascade:
--
-- 1. Mangrove: tropical wet coast
-- 2. Estuary: river mouth (high discharge)
-- 3. Salt marsh: cool wet coast
-- 4. Rocky shore: hard rock, thin soil
-- 5. Coastal dunes: dry sandy coast
-- 6. Coastal scrub: Mediterranean-type warm dry coast
-- 7. Fallback: 'BiomeCoastal'
refineCoastal
  :: CoastalConfig
  -> Float -> Float -> Float -> Float -> Float -> Float
  -> BiomeId
refineCoastal cfg temp precip moisture hardness soilDepth discharge
  | temp >= ccMangroveMinTemp cfg
    && precip >= ccMangroveMinPrecip cfg        = BiomeMangrove
  | discharge >= ccEstuaryMinDischarge cfg      = BiomeEstuary
  | temp <= ccSaltMarshMaxTemp cfg
    && moisture >= ccSaltMarshMinMoist cfg       = BiomeSaltMarsh
  | hardness >= ccRockyMinHardness cfg
    && soilDepth <= ccRockyMaxSoilDepth cfg      = BiomeRockyShore
  | moisture <= ccDunesMaxMoisture cfg
    && precip <= ccDunesMaxPrecip cfg            = BiomeCoastalDunes
  | temp >= ccScrubMinTemp cfg
    && precip <= ccScrubMaxPrecip cfg            = BiomeCoastalScrub
  | otherwise                                   = BiomeCoastal
