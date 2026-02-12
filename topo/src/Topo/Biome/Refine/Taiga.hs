{-# LANGUAGE PatternSynonyms #-}

-- | Taiga / Boreal sub-biome refinement.
--
-- Discriminates boreal bog and boreal forest from the family-level
-- 'BiomeTaiga'.
module Topo.Biome.Refine.Taiga
  ( TaigaConfig(..)
  , defaultTaigaConfig
  , refineTaiga
  ) where

import Topo.Types (BiomeId, TerrainForm,
                   pattern BiomeTaiga, pattern BiomeBorealForest,
                   pattern BiomeBorealBog,
                   pattern FormFlat, pattern FormDepression)

-- | Configuration for taiga sub-biome classification.
data TaigaConfig = TaigaConfig
  { taBorealMinPrecip :: !Float  -- ^ default 0.40
  , taBorealMinTemp   :: !Float  -- ^ default 0.22
  , taBogMaxMoisture  :: !Float  -- ^ moisture above which bog forms (default 0.75)
  } deriving (Eq, Show)

-- | Sensible defaults for taiga refinement.
defaultTaigaConfig :: TaigaConfig
defaultTaigaConfig = TaigaConfig
  { taBorealMinPrecip = 0.40
  , taBorealMinTemp   = 0.22
  , taBogMaxMoisture  = 0.75
  }

-- | Refine a taiga tile into a sub-biome.
--
-- Decision cascade:
--
-- 1. Boreal bog: very wet + flat\/depression terrain
-- 2. Boreal forest: warm enough + wet enough
-- 3. Fallback: 'BiomeTaiga'
refineTaiga
  :: TaigaConfig
  -> Float -> Float -> Float -> TerrainForm
  -> BiomeId
refineTaiga cfg temp precip moisture tf
  | moisture >= taBogMaxMoisture cfg
    && (tf == FormFlat || tf == FormDepression)  = BiomeBorealBog
  | temp >= taBorealMinTemp cfg
    && precip >= taBorealMinPrecip cfg           = BiomeBorealForest
  | otherwise                                    = BiomeTaiga
