{-# LANGUAGE DeriveGeneric #-}
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

import GHC.Generics (Generic)
import Topo.Config.JSON
  (ToJSON(..), FromJSON(..), configOptions, mergeDefaults,
   genericToJSON, genericParseJSON)
import Topo.Types (BiomeId, pattern BiomeCoastal, pattern BiomeMangrove,
                   pattern BiomeEstuary, pattern BiomeSaltMarsh,
                   pattern BiomeRockyShore, pattern BiomeCoastalDunes,
                   pattern BiomeCoastalScrub)

-- | Configuration for coastal sub-biome classification.
data CoastalConfig = CoastalConfig
  { cstMangroveMinTemp     :: !Float  -- ^ default 0.65
  , cstMangroveMinPrecip   :: !Float  -- ^ default 0.50
  , cstSaltMarshMaxTemp    :: !Float  -- ^ default 0.55
  , cstSaltMarshMinMoist   :: !Float  -- ^ default 0.55
  , cstDunesMaxMoisture    :: !Float  -- ^ default 0.25
  , cstDunesMaxPrecip      :: !Float  -- ^ default 0.20
  , cstEstuaryMinDischarge :: !Float  -- ^ default 0.30
  , cstRockyMinHardness    :: !Float  -- ^ default 0.60
  , cstRockyMaxSoilDepth   :: !Float  -- ^ default 0.15
  , cstScrubMaxPrecip      :: !Float  -- ^ default 0.40
  , cstScrubMinTemp        :: !Float  -- ^ default 0.40
  } deriving (Eq, Show, Generic)

instance ToJSON CoastalConfig where
  toJSON = genericToJSON (configOptions "cst")

instance FromJSON CoastalConfig where
  parseJSON v = genericParseJSON (configOptions "cst")
                  (mergeDefaults (toJSON defaultCoastalConfig) v)

-- | Sensible defaults for coastal refinement.
defaultCoastalConfig :: CoastalConfig
defaultCoastalConfig = CoastalConfig
  { cstMangroveMinTemp     = 0.65
  , cstMangroveMinPrecip   = 0.50
  , cstSaltMarshMaxTemp    = 0.55
  , cstSaltMarshMinMoist   = 0.55
  , cstDunesMaxMoisture    = 0.25
  , cstDunesMaxPrecip      = 0.20
  , cstEstuaryMinDischarge = 0.30
  , cstRockyMinHardness    = 0.60
  , cstRockyMaxSoilDepth   = 0.15
  , cstScrubMaxPrecip      = 0.40
  , cstScrubMinTemp        = 0.40
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
  | temp >= cstMangroveMinTemp cfg
    && precip >= cstMangroveMinPrecip cfg        = BiomeMangrove
  | discharge >= cstEstuaryMinDischarge cfg      = BiomeEstuary
  | temp <= cstSaltMarshMaxTemp cfg
    && moisture >= cstSaltMarshMinMoist cfg       = BiomeSaltMarsh
  | hardness >= cstRockyMinHardness cfg
    && soilDepth <= cstRockyMaxSoilDepth cfg      = BiomeRockyShore
  | moisture <= cstDunesMaxMoisture cfg
    && precip <= cstDunesMaxPrecip cfg            = BiomeCoastalDunes
  | temp >= cstScrubMinTemp cfg
    && precip <= cstScrubMaxPrecip cfg            = BiomeCoastalScrub
  | otherwise                                   = BiomeCoastal
