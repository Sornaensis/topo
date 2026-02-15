{-# LANGUAGE DeriveGeneric #-}
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

import GHC.Generics (Generic)
import Topo.Config.JSON
  (ToJSON(..), FromJSON(..), configOptions, mergeDefaults,
   genericToJSON, genericParseJSON)
import Topo.Types (BiomeId, TerrainForm,
                   pattern BiomeTaiga, pattern BiomeBorealForest,
                   pattern BiomeBorealBog, pattern BiomeOceanicBoreal,
                   pattern FormFlat, pattern FormDepression)

-- | Configuration for taiga sub-biome classification.
data TaigaConfig = TaigaConfig
  { taBorealMinPrecip :: !Float  -- ^ default 0.40
  , taBorealMinTemp   :: !Float  -- ^ default 0.22
  , taBogMaxMoisture  :: !Float  -- ^ moisture above which bog forms (default 0.75)
  , taOceanicMaxRange :: !Float
  -- ^ Maximum temperature range for oceanic boreal classification.
  -- Low annual range indicates maritime influence (e.g. Norway coast,
  -- Pacific NW Canada).  Default: @0.15@.
  , taOceanicMinHumidity :: !Float
  -- ^ Minimum humidity for oceanic boreal.  Maritime boreal climates
  -- have consistently high moisture.  Default: @0.45@.
  , taContinentalMinRange :: !Float
  -- ^ Minimum temperature range for continental taiga identification.
  -- High range is diagnostic of continental-interior boreal (Siberia,
  -- central Canada).  Used in the boreal-forest branch to require
  -- higher precip for continental taiga.  Default: @0.30@.
  } deriving (Eq, Show, Generic)

instance ToJSON TaigaConfig where
  toJSON = genericToJSON (configOptions "ta")

instance FromJSON TaigaConfig where
  parseJSON v = genericParseJSON (configOptions "ta")
                  (mergeDefaults (toJSON defaultTaigaConfig) v)

-- | Sensible defaults for taiga refinement.
defaultTaigaConfig :: TaigaConfig
defaultTaigaConfig = TaigaConfig
  { taBorealMinPrecip     = 0.40
  , taBorealMinTemp       = 0.22
  , taBogMaxMoisture      = 0.75
  , taOceanicMaxRange     = 0.15
  , taOceanicMinHumidity  = 0.45
  , taContinentalMinRange = 0.30
  }

-- | Refine a taiga tile into a sub-biome.
--
-- Decision cascade:
--
-- 1. Boreal bog: very wet + flat\/depression terrain
-- 2. Oceanic boreal: low temp range + high humidity (maritime coast)
-- 3. Boreal forest: warm enough + wet enough
-- 4. Fallback: 'BiomeTaiga' (continental taiga / cold sparse boreal)
refineTaiga
  :: TaigaConfig
  -> Float          -- ^ temperature
  -> Float          -- ^ precipitation
  -> Float          -- ^ moisture
  -> TerrainForm    -- ^ terrain form
  -> Float          -- ^ temperature range (annual, normalised 0–1)
  -> Float          -- ^ humidity average (annual, normalised 0–1)
  -> BiomeId
refineTaiga cfg temp precip moisture tf tempRange humidity
  | moisture >= taBogMaxMoisture cfg
    && (tf == FormFlat || tf == FormDepression)  = BiomeBorealBog
  | tempRange <= taOceanicMaxRange cfg
    && humidity >= taOceanicMinHumidity cfg
    && precip >= taBorealMinPrecip cfg           = BiomeOceanicBoreal
  | temp >= taBorealMinTemp cfg
    && precip >= taBorealMinPrecip cfg           = BiomeBorealForest
  | otherwise                                    = BiomeTaiga
