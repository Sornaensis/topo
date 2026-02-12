{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Early-pipeline soil derivation.
--
-- Computes soil type, soil depth, soil grain, and fertility from
-- erosion-stage outputs ('tcHardness', 'tcMoisture') and elevation.
-- These fields feed the vegetation bootstrap stage and, transitively,
-- the climate stage's evapotranspiration model.
--
-- Split out from 'Topo.Parameters' so that soil data is available before
-- the climate stage runs (Parameters executes post-glacier for
-- terrain-shape fields that need final elevation).
module Topo.Soil
  ( SoilConfig(..)
  , defaultSoilConfig
  , applySoilStage
  -- * Pure helpers (exported for testing)
  , soilTypeFromMoistureHardness
  , soilDepthFromHardness
  , fertilityFromMoistureDepth
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Word (Word16)
import Topo.Math (clamp01)
import Topo.Pipeline (PipelineStage(..))
import Topo.Plugin (logInfo, modifyWorldP)
import Topo.Types
import Topo.World (TerrainWorld(..))
import qualified Data.Vector.Unboxed as U

---------------------------------------------------------------------------
-- Configuration
---------------------------------------------------------------------------

-- | Soil derivation thresholds and weights.
data SoilConfig = SoilConfig
  { scMoistureThreshold  :: !Float
    -- ^ Moisture level above which soil is classified as wet (type 2).
    -- Default: @0.7@.
  , scHardnessThreshold  :: !Float
    -- ^ Hardness level above which soil is classified as rocky (type 1).
    -- Default: @0.45@.
  , scFertilityMoistWeight :: !Float
    -- ^ Weight of moisture in the fertility blend.
    -- Default: @0.6@.
  , scFertilityDepthWeight :: !Float
    -- ^ Weight of soil depth in the fertility blend.
    -- Default: @0.4@.
  } deriving (Eq, Show)

-- | Sensible defaults for soil derivation.
defaultSoilConfig :: SoilConfig
defaultSoilConfig = SoilConfig
  { scMoistureThreshold  = 0.7
  , scHardnessThreshold  = 0.45
  , scFertilityMoistWeight = 0.6
  , scFertilityDepthWeight = 0.4
  }

---------------------------------------------------------------------------
-- Pipeline stage
---------------------------------------------------------------------------

-- | Derive soil fields from hardness and moisture.
--
-- Populates 'tcSoilType', 'tcSoilDepth', 'tcSoilGrain', and
-- 'tcFertility' in each 'TerrainChunk'.  Must run after hydrology
-- (which provides 'tcMoisture') and before the vegetation bootstrap.
applySoilStage :: SoilConfig -> PipelineStage
applySoilStage cfg =
    PipelineStage "applySoil" "applySoil" $ do
  logInfo "applySoil: deriving soil fields"
  modifyWorldP $ \world ->
    let chunks = twTerrain world
        terrain' = IntMap.map (deriveSoilChunk cfg) chunks
    in world { twTerrain = terrain' }

---------------------------------------------------------------------------
-- Per-chunk derivation
---------------------------------------------------------------------------

deriveSoilChunk :: SoilConfig -> TerrainChunk -> TerrainChunk
deriveSoilChunk cfg chunk =
  let moisture = tcMoisture chunk
      hardness = tcHardness chunk
      n        = U.length moisture

      !soilType  = U.generate n $ \i ->
        soilTypeFromMoistureHardness cfg (moisture U.! i) (hardness U.! i)

      !soilDepth = U.map (soilDepthFromHardness) hardness

      -- Soil grain is inversely related to hardness: soft rock weathers
      -- into finer grains.
      !soilGrain = U.map (\h -> clamp01 (1 - h * 0.8)) hardness

      !fertility = U.zipWith
        (fertilityFromMoistureDepth cfg)
        moisture soilDepth

  in chunk
      { tcSoilType  = soilType
      , tcSoilDepth = soilDepth
      , tcSoilGrain = soilGrain
      , tcFertility = fertility
      }

---------------------------------------------------------------------------
-- Pure classifiers
---------------------------------------------------------------------------

-- | Classify soil type from moisture and hardness.
--
-- Returns:
--
--   * 2 — wet soil (high moisture)
--   * 1 — rocky soil (high hardness, low moisture)
--   * 0 — normal soil
{-# INLINE soilTypeFromMoistureHardness #-}
soilTypeFromMoistureHardness :: SoilConfig -> Float -> Float -> Word16
soilTypeFromMoistureHardness cfg m h
  | m > scMoistureThreshold cfg  = 2
  | h > scHardnessThreshold cfg  = 1
  | otherwise                    = 0

-- | Derive soil depth from rock hardness.
--
-- @soilDepth = clamp01 (1 - hardness)@.
-- Soft rock weathers deeply; hard rock remains exposed.
{-# INLINE soilDepthFromHardness #-}
soilDepthFromHardness :: Float -> Float
soilDepthFromHardness h = clamp01 (1 - h)

-- | Derive fertility from moisture and soil depth.
--
-- @fertility = clamp01 (moisture * moistWeight + depth * depthWeight)@.
{-# INLINE fertilityFromMoistureDepth #-}
fertilityFromMoistureDepth :: SoilConfig -> Float -> Float -> Float
fertilityFromMoistureDepth cfg m d =
  clamp01 (m * scFertilityMoistWeight cfg + d * scFertilityDepthWeight cfg)
