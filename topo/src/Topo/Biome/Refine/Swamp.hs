{-# LANGUAGE PatternSynonyms #-}

-- | Swamp / Wetland sub-biome refinement.
--
-- Discriminates floodplain forest, fen, bog, marsh, and wetland
-- from the family-level 'BiomeSwamp'.
module Topo.Biome.Refine.Swamp
  ( SwampConfig(..)
  , defaultSwampConfig
  , refineSwamp
  ) where

import Topo.Types (BiomeId, TerrainForm,
                   pattern BiomeSwamp, pattern BiomeFloodplainForest,
                   pattern BiomeFen, pattern BiomeBog,
                   pattern BiomeMarsh, pattern BiomeWetland,
                   pattern FormFlat, pattern FormDepression)

-- | Configuration for swamp sub-biome classification.
data SwampConfig = SwampConfig
  { swMarshMaxTemp           :: !Float  -- ^ default 0.55
  , swMarshMinMoisture       :: !Float  -- ^ default 0.70
  , swBogMaxTemp             :: !Float  -- ^ default 0.40
  , swBogMaxFertility        :: !Float  -- ^ default 0.20
  , swFenMinGwDischarge      :: !Float  -- ^ default 0.30 (groundwater-fed)
  , swFenMaxTemp             :: !Float  -- ^ default 0.50
  , swFloodplainMinDischarge :: !Float  -- ^ default 0.40
  , swFloodplainMinTemp      :: !Float  -- ^ default 0.50
  , swWetlandMaxTemp         :: !Float  -- ^ default 0.55
  } deriving (Eq, Show)

-- | Sensible defaults for swamp refinement.
defaultSwampConfig :: SwampConfig
defaultSwampConfig = SwampConfig
  { swMarshMaxTemp           = 0.55
  , swMarshMinMoisture       = 0.70
  , swBogMaxTemp             = 0.40
  , swBogMaxFertility        = 0.20
  , swFenMinGwDischarge      = 0.30
  , swFenMaxTemp             = 0.50
  , swFloodplainMinDischarge = 0.40
  , swFloodplainMinTemp      = 0.50
  , swWetlandMaxTemp         = 0.55
  }

-- | Refine a swamp tile into a sub-biome.
--
-- Decision cascade:
--
-- 1. Floodplain forest: high river discharge + warm
-- 2. Fen: groundwater-fed + cool
-- 3. Bog: cool + infertile
-- 4. Marsh: flat terrain + very wet + cool
-- 5. Wetland: cool
-- 6. Fallback: 'BiomeSwamp'
refineSwamp
  :: SwampConfig
  -> Float -> Float -> Float -> Float -> Float -> TerrainForm
  -> BiomeId
refineSwamp cfg temp moisture fertility discharge gwDischarge tf
  | discharge >= swFloodplainMinDischarge cfg
    && temp >= swFloodplainMinTemp cfg          = BiomeFloodplainForest
  | gwDischarge >= swFenMinGwDischarge cfg
    && temp <= swFenMaxTemp cfg                 = BiomeFen
  | temp <= swBogMaxTemp cfg
    && fertility <= swBogMaxFertility cfg       = BiomeBog
  | (tf == FormFlat || tf == FormDepression)
    && moisture >= swMarshMinMoisture cfg
    && temp <= swMarshMaxTemp cfg               = BiomeMarsh
  | temp <= swWetlandMaxTemp cfg                = BiomeWetland
  | otherwise                                   = BiomeSwamp
