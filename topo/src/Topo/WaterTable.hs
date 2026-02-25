{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Per-tile water table depth and soil infiltration model.
--
-- This module introduces a simplified steady-state water table that
-- bridges the gap between surface hydrology ('tcMoisture') and
-- subsurface water availability for vegetation.  Three key properties
-- are computed per tile:
--
--   1. __Infiltration capacity__ @[0, 1]@ — what fraction of
--      rainfall\/surface moisture actually penetrates the soil.
--   2. __Water table depth__ @[0, 1]@ — normalised depth below the
--      surface where the saturated zone begins (0 = surface,
--      1 = unreachably deep).
--   3. __Root-zone moisture__ @[0, 1]@ — effective water availability
--      for vegetation, derived from water table depth and soil
--      properties.
--
-- The model uses the well-established /topographic water table
-- assumption/: in most landscapes, the water table roughly parallels
-- surface topography but smoothed — valleys have shallow water tables,
-- ridges have deep ones.  A few diffusion passes from a
-- precipitation-weighted initial estimate produce this naturally.
--
-- Pipeline placement: between 'Topo.Parameters.applyParameterLayersStage'
-- (stage 13) and 'Topo.BiomeConfig.classifyBiomesStage' (stage 14).
-- At this point we have authoritative slope, moisture, soil, and
-- precipitation data.  The stage overwrites 'tcFertility' with an
-- improved formula and populates the water-table fields on
-- 'GroundwaterChunk'.
module Topo.WaterTable
  ( -- * Configuration
    WaterTableConfig(..)
  , defaultWaterTableConfig
    -- * Pipeline stage
  , applyWaterTableStage
    -- * Pure helpers (exported for testing)
  , infiltrationCapacity
  , waterTableDiffusion
  , rootZoneMoisture
  , improvedFertility
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Word (Word16)
import GHC.Generics (Generic)
import Topo.Config.JSON
  ( ToJSON(..), FromJSON(..), configOptions, mergeDefaults
  , genericToJSON, genericParseJSON
  )
import Topo.Math (clamp01)
import Topo.Pipeline (PipelineStage(..))
import Topo.Plugin (logInfo, getWorldP, putWorldP, PluginError(..))
import Topo.TerrainGrid
  ( validateTerrainGrid
  , buildElevationGrid
  , buildMoistureGrid
  , buildClimatePrecipGrid
  , buildSlopeGrid
  , buildMaxSlopeGrid
  , buildSoilDepthGrid
  , buildSoilGrainGrid
  , buildSoilTypeGrid
  , chunkGridSlice
  )
import Topo.Types
import Topo.World (TerrainWorld(..))
import qualified Data.Vector.Unboxed as U
import Control.Monad.Except (throwError)

---------------------------------------------------------------------------
-- Configuration
---------------------------------------------------------------------------

-- | Tuning parameters for the water table model.
--
-- All weights and thresholds are configurable so that the model can be
-- tuned per-planet or per-biome without code changes.  The defaults
-- produce reasonable results for an Earth-like world.
data WaterTableConfig = WaterTableConfig
  { wtcGrainPermeabilityCenter :: !Float
    -- ^ Grain size with peak permeability (bell-curve center).
    -- Default: @0.50@ (loamy).
  , wtcGrainPermeabilityWidth  :: !Float
    -- ^ Width of the permeability bell curve.
    -- Default: @0.35@.
  , wtcSlopeInfiltrationMax    :: !Float
    -- ^ Slope at which infiltration drops to zero.
    -- Default: @0.40@.
  , wtcDepthScaling            :: !Float
    -- ^ Soil-depth scaling for infiltration capacity.
    -- Default: @1.2@.
  , wtcRockyPermeabilityPenalty :: !Float
    -- ^ Infiltration multiplier for rocky soil (type 1).
    -- Default: @0.25@.
  , wtcWetPermeabilityBonus     :: !Float
    -- ^ Infiltration multiplier for wet soil (type 2).
    -- Default: @1.15@.
  , wtcDiffusionIterations     :: !Int
    -- ^ Number of water-table diffusion passes.
    -- Default: @12@.
  , wtcDiffusionRate           :: !Float
    -- ^ Per-iteration diffusion blending factor @[0, 1]@.
    -- Default: @0.4@.
  , wtcConductivity            :: !Float
    -- ^ Lateral subsurface hydraulic conductivity scaling.
    -- Default: @0.3@.
  , wtcRootDepthMax            :: !Float
    -- ^ Maximum root-zone depth @[0, 1]@ normalised.
    -- Default: @0.15@.
  , wtcWaterTableWeight        :: !Float
    -- ^ Weight of water-table factor in root-zone moisture.
    -- Default: @0.50@.
  , wtcSurfaceMoistureWeight   :: !Float
    -- ^ Weight of surface moisture in root-zone moisture.
    -- Default: @0.25@.
  , wtcCapillaryWeight         :: !Float
    -- ^ Weight of capillary rise in root-zone moisture.
    -- Default: @0.25@.
  , wtcCapillaryScale          :: !Float
    -- ^ Capillary rise intensity from fine-grained soil.
    -- Default: @0.6@.
  , wtcFertilityMoistWeight    :: !Float
    -- ^ Weight of root-zone moisture in improved fertility.
    -- Default: @0.45@.
  , wtcFertilityDepthWeight    :: !Float
    -- ^ Weight of soil depth in improved fertility.
    -- Default: @0.30@.
  , wtcFertilityOrganicWeight  :: !Float
    -- ^ Weight of organic factor in improved fertility.
    -- Default: @0.25@.
  , wtcRockOrganicPenalty      :: !Float
    -- ^ Grain-proportional organic-matter penalty.
    -- Default: @0.7@.
  } deriving (Eq, Show, Generic)

-- | JSON serialisation strips the @wtc@ prefix.
instance ToJSON WaterTableConfig where
  toJSON = genericToJSON (configOptions "wtc")

-- | JSON deserialisation merges with defaults so every field is optional.
instance FromJSON WaterTableConfig where
  parseJSON v = genericParseJSON (configOptions "wtc")
                  (mergeDefaults (toJSON defaultWaterTableConfig) v)

-- | Sensible defaults for an Earth-like world.
defaultWaterTableConfig :: WaterTableConfig
defaultWaterTableConfig = WaterTableConfig
  { wtcGrainPermeabilityCenter = 0.50
  , wtcGrainPermeabilityWidth  = 0.35
  , wtcSlopeInfiltrationMax    = 0.40
  , wtcDepthScaling            = 1.2
  , wtcRockyPermeabilityPenalty = 0.25
  , wtcWetPermeabilityBonus     = 1.15
  , wtcDiffusionIterations     = 12
  , wtcDiffusionRate           = 0.4
  , wtcConductivity            = 0.3
  , wtcRootDepthMax            = 0.15
  , wtcWaterTableWeight        = 0.50
  , wtcSurfaceMoistureWeight   = 0.25
  , wtcCapillaryWeight         = 0.25
  , wtcCapillaryScale          = 0.6
  , wtcFertilityMoistWeight    = 0.45
  , wtcFertilityDepthWeight    = 0.30
  , wtcFertilityOrganicWeight  = 0.25
  , wtcRockOrganicPenalty      = 0.7
  }

---------------------------------------------------------------------------
-- Pure helpers
---------------------------------------------------------------------------

-- | Per-tile infiltration capacity @[0, 1]@.
--
-- Combines a bell-curve grain-size permeability, a slope penalty, a
-- soil-depth scaling factor, and a soil-type multiplier.
--
-- @
-- infiltration = permeabilityFactor * slopeFactor * depthFactor
-- @
{-# INLINE infiltrationCapacity #-}
infiltrationCapacity
  :: WaterTableConfig
  -> Float   -- ^ Soil grain @[0, 1]@
  -> Float   -- ^ Slope @[0, 1]@
  -> Float   -- ^ Soil depth @[0, 1]@
  -> Word16  -- ^ Soil type (0 = normal, 1 = rocky, 2 = wet)
  -> Float
infiltrationCapacity cfg grain slope depth soilType =
  clamp01 (permeabilityFactor * slopeFactor * depthFactor)
  where
    -- Bell curve: exp(-((grain - center) / width)^2)
    center = wtcGrainPermeabilityCenter cfg
    width  = wtcGrainPermeabilityWidth cfg
    dx     = (grain - center) / max 0.001 width
    basePerm = exp (negate (dx * dx))
    -- Soil-type multiplier
    typeMul = soilTypeMultiplier cfg soilType
    permeabilityFactor = basePerm * typeMul

    -- Steep terrain → water runs off
    slopeFactor = clamp01 (1 - slope / max 0.001 (wtcSlopeInfiltrationMax cfg))

    -- Deeper soil → more capacity
    depthFactor = clamp01 (depth * wtcDepthScaling cfg)

-- | Soil-type multiplier for infiltration.
--
--   * Type 1 (rocky) — severe penalty.
--   * Type 2 (wet) — modest bonus.
--   * Any other — neutral (1.0).
{-# INLINE soilTypeMultiplier #-}
soilTypeMultiplier :: WaterTableConfig -> Word16 -> Float
soilTypeMultiplier cfg 1 = wtcRockyPermeabilityPenalty cfg
soilTypeMultiplier cfg 2 = wtcWetPermeabilityBonus cfg
soilTypeMultiplier _   _ = 1.0

-- | Steady-state water-table diffusion.
--
-- Given elevation and per-tile recharge (infiltration × precipitation),
-- iteratively relaxes a water-table elevation field.  The result is a
-- per-tile __water table depth__ @[0, 1]@ where 0 = at surface and
-- 1 = unreachably deep.
--
-- Algorithm:
--
-- 1. Initial estimate: @wtElev = elevation − (1 − infiltration)@.
-- 2. For each iteration, relax toward the mean of 4-connected
--    neighbours weighted by conductivity and add recharge.
-- 3. Convert: @depth = clamp01(elevation − wtElev)@.
waterTableDiffusion
  :: WaterTableConfig
  -> Int               -- ^ Grid width
  -> Int               -- ^ Grid height
  -> U.Vector Float    -- ^ Elevation grid
  -> U.Vector Float    -- ^ Infiltration capacity grid @[0, 1]@
  -> U.Vector Float    -- ^ Precipitation grid @[0, 1]@
  -> U.Vector Float    -- ^ Water table depth grid @[0, 1]@
waterTableDiffusion cfg gridW gridH elev infilt precip =
  let n = gridW * gridH
      -- Recharge at each tile: infiltration × precipitation
      recharge = U.zipWith (*) infilt precip
      -- Initial water table elevation: higher infiltration → shallower table
      initWtElev = U.zipWith (\e inf -> e - (1 - inf)) elev infilt
      -- Run diffusion iterations
      iters = max 0 (wtcDiffusionIterations cfg)
      rate  = wtcDiffusionRate cfg
      cond  = wtcConductivity cfg
      finalWtElev = iterateDiffusion iters rate cond gridW gridH n
                                     elev recharge initWtElev
      -- Convert to depth: depth = clamp01(elevation - waterTableElev)
  in U.zipWith (\e wt -> clamp01 (e - wt)) elev finalWtElev

-- | One pass of water-table diffusion relaxation.
--
-- For each tile, blend the current water-table elevation toward the
-- mean of its 4-connected neighbours, then add recharge.
diffusionStep
  :: Float             -- ^ Diffusion rate @[0, 1]@
  -> Float             -- ^ Conductivity scaling
  -> Int               -- ^ Grid width
  -> Int               -- ^ Grid height
  -> Int               -- ^ Total tiles
  -> U.Vector Float    -- ^ Surface elevation (cap)
  -> U.Vector Float    -- ^ Recharge per tile
  -> U.Vector Float    -- ^ Current water-table elevation
  -> U.Vector Float    -- ^ Updated water-table elevation
diffusionStep rate cond gridW gridH n elev recharge wtElev =
  U.generate n $ \i ->
    let x = i `mod` gridW
        y = i `div` gridW
        wt0 = wtElev U.! i
        e0  = elev U.! i
        r0  = recharge U.! i
        -- 4-connected neighbour mean (clamped at edges)
        nCount = neighborCount x y gridW gridH
        nSum   = neighborSum x y gridW gridH wtElev
        nMean  = if nCount > 0 then nSum / fromIntegral nCount else wt0
        -- Drainage: difference between this tile and neighbour mean,
        -- scaled by conductivity
        drainage = max 0 (wt0 - nMean) * cond
        -- Relax toward neighbour mean + add recharge − drainage
        wt1 = wt0 + rate * (nMean - wt0) + r0 * rate - drainage * rate
        -- Water table cannot exceed surface elevation
    in min e0 wt1

-- | Iterate diffusion for a fixed number of steps.
iterateDiffusion
  :: Int -> Float -> Float -> Int -> Int -> Int
  -> U.Vector Float -> U.Vector Float -> U.Vector Float
  -> U.Vector Float
iterateDiffusion 0 _rate _cond _gridW _gridH _n _elev _recharge wt = wt
iterateDiffusion iters rate cond gridW gridH n elev recharge wt =
  let !wt' = diffusionStep rate cond gridW gridH n elev recharge wt
  in iterateDiffusion (iters - 1) rate cond gridW gridH n elev recharge wt'

-- | Count 4-connected neighbours within grid bounds.
{-# INLINE neighborCount #-}
neighborCount :: Int -> Int -> Int -> Int -> Int
neighborCount x y gridW gridH =
  (if x > 0          then 1 else 0)
  + (if x < gridW - 1 then 1 else 0)
  + (if y > 0          then 1 else 0)
  + (if y < gridH - 1 then 1 else 0)

-- | Sum of 4-connected neighbour values.
{-# INLINE neighborSum #-}
neighborSum :: Int -> Int -> Int -> Int -> U.Vector Float -> Float
neighborSum x y gridW _gridH v =
  let i = y * gridW + x
      left  = if x > 0          then v U.! (i - 1)     else 0
      right = if x < gridW - 1  then v U.! (i + 1)     else 0
      up    = if y > 0          then v U.! (i - gridW)  else 0
      down  = if y < _gridH - 1 then v U.! (i + gridW) else 0
  in left + right + up + down

-- | Per-tile root-zone moisture @[0, 1]@.
--
-- Weighted blend of:
--
--   * Water-table factor — shallow table → high root access.
--   * Surface moisture — existing 'tcMoisture'.
--   * Capillary rise — fine-grained soils wick water up.
{-# INLINE rootZoneMoisture #-}
rootZoneMoisture
  :: WaterTableConfig
  -> Float   -- ^ Water table depth @[0, 1]@
  -> Float   -- ^ Surface moisture ('tcMoisture') @[0, 1]@
  -> Float   -- ^ Soil grain @[0, 1]@
  -> Float
rootZoneMoisture cfg wtDepth surfMoisture grain =
  clamp01 (waterTableFactor * wtW + surfMoisture * surfW + capillaryFactor * capW)
  where
    wtW   = wtcWaterTableWeight cfg
    surfW = wtcSurfaceMoistureWeight cfg
    capW  = wtcCapillaryWeight cfg

    -- Shallow water table → high root access
    rootMax = max 0.001 (wtcRootDepthMax cfg)
    waterTableFactor = clamp01 (1 - wtDepth / rootMax)

    -- Capillary rise: fine-grained soils wick water from the table
    capillaryFactor = clamp01 ((1 - grain) * wtcCapillaryScale cfg
                                * (1 - wtDepth))

-- | Improved fertility @[0, 1]@.
--
-- Replaces the old @moisture * 0.6 + soilDepth * 0.4@ formula with
-- a three-factor blend of root-zone moisture, soil depth, and an
-- organic-matter proxy.
{-# INLINE improvedFertility #-}
improvedFertility
  :: WaterTableConfig
  -> Float   -- ^ Root-zone moisture @[0, 1]@
  -> Float   -- ^ Soil depth @[0, 1]@
  -> Float   -- ^ Soil grain @[0, 1]@
  -> Float
improvedFertility cfg rzMoist depth grain =
  clamp01 (rzMoist * mW + depth * dW + organicFactor * oW)
  where
    mW = wtcFertilityMoistWeight cfg
    dW = wtcFertilityDepthWeight cfg
    oW = wtcFertilityOrganicWeight cfg
    -- Organic-matter proxy: deeper soil with less rocky grain → more organics
    organicFactor = min 1 (depth * (1 - grain * wtcRockOrganicPenalty cfg))

---------------------------------------------------------------------------
-- Pipeline stage
---------------------------------------------------------------------------

-- | Apply the water-table model and overwrite 'tcFertility'.
--
-- Reads elevation, slope, moisture, soil depth, soil grain, soil type,
-- and precipitation from the current world state.  Computes
-- infiltration, runs water-table diffusion, derives root-zone moisture,
-- and overwrites 'tcFertility' with the improved formula.  Populates
-- 'gwInfiltration', 'gwWaterTableDepth', and 'gwRootZoneMoisture' on
-- each 'GroundwaterChunk'.
applyWaterTableStage :: WaterTableConfig -> PipelineStage
applyWaterTableStage cfg = PipelineStage "applyWaterTable" "applyWaterTable" $ do
  logInfo "applyWaterTable: computing infiltration, water table, root-zone moisture"
  world <- getWorldP
  let config  = twConfig world
      terrain = twTerrain world
      climate = twClimate world
  (ChunkCoord minCx minCy, ChunkCoord maxCx maxCy) <-
    case validateTerrainGrid config terrain of
      Left err -> throwError (PluginInvariantError err)
      Right bounds -> pure bounds
  let size  = wcChunkSize config
      gridW = (maxCx - minCx + 1) * size
      gridH = (maxCy - minCy + 1) * size
      minCC = ChunkCoord minCx minCy

      -- Build global grids from chunks
      elev      = buildElevationGrid  config terrain minCC gridW gridH
      slope     = buildMaxSlopeGrid   config terrain minCC gridW gridH
      moisture  = buildMoistureGrid   config terrain minCC gridW gridH
      soilDepth = buildSoilDepthGrid  config terrain minCC gridW gridH
      soilGrain = buildSoilGrainGrid  config terrain minCC gridW gridH
      soilType  = buildSoilTypeGrid   config terrain minCC gridW gridH
      precip    = buildClimatePrecipGrid config climate minCC gridW gridH

      -- 1. Compute per-tile infiltration capacity
      n = gridW * gridH
      !infiltGrid = U.generate n $ \i ->
        infiltrationCapacity cfg
          (soilGrain U.! i)
          (slope U.! i)
          (soilDepth U.! i)
          (soilType U.! i)

      -- 2. Water table diffusion → depth grid
      !wtDepthGrid = waterTableDiffusion cfg gridW gridH elev infiltGrid precip

      -- 3. Root-zone moisture per tile
      !rzMoistGrid = U.generate n $ \i ->
        rootZoneMoisture cfg
          (wtDepthGrid U.! i)
          (moisture U.! i)
          (soilGrain U.! i)

      -- 4. Improved fertility per tile
      !fertGrid = U.generate n $ \i ->
        improvedFertility cfg
          (rzMoistGrid U.! i)
          (soilDepth U.! i)
          (soilGrain U.! i)

      -- Slice grids back into per-chunk vectors and update
      terrain' = IntMap.mapWithKey
        (updateTerrainFertility config minCC gridW fertGrid) terrain

      groundwater' = IntMap.mapWithKey
        (updateGroundwaterWT config minCC gridW infiltGrid wtDepthGrid rzMoistGrid)
        (twGroundwater world)

  putWorldP world
    { twTerrain     = terrain'
    , twGroundwater = groundwater'
    }

-- | Overwrite 'tcFertility' from the global fertility grid.
updateTerrainFertility
  :: WorldConfig -> ChunkCoord -> Int -> U.Vector Float
  -> Int -> TerrainChunk -> TerrainChunk
updateTerrainFertility config minCoord gridW fertGrid key chunk =
  chunk { tcFertility = chunkGridSlice config minCoord gridW fertGrid key }

-- | Populate water-table fields on 'GroundwaterChunk'.
updateGroundwaterWT
  :: WorldConfig -> ChunkCoord -> Int
  -> U.Vector Float -> U.Vector Float -> U.Vector Float
  -> Int -> GroundwaterChunk -> GroundwaterChunk
updateGroundwaterWT config minCoord gridW infiltGrid wtGrid rzGrid key gw =
  gw { gwInfiltration     = chunkGridSlice config minCoord gridW infiltGrid key
     , gwWaterTableDepth  = chunkGridSlice config minCoord gridW wtGrid key
     , gwRootZoneMoisture = chunkGridSlice config minCoord gridW rzGrid key
     }
