{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Hydrology stages for flow routing and moisture.
module Topo.Hydrology
  ( HydroConfig(..)
  , defaultHydroConfig
  , applyHydrologyStage
  , RiverConfig(..)
  , defaultRiverConfig
  , GroundwaterConfig(..)
  , defaultGroundwaterConfig
  , applyRiverStage
    -- * Flow routing (exported for testing)
  , flowDirections
  , flowDirectionsLand
    -- * Depression filling (exported for testing)
  , fillDepressions
  , breachRemainingSinks
    -- * Sink breaching (exported for testing)
  , breachSinksLand
    -- * Grid operations (exported for testing)
  , flowAccumulation
  , piedmontSmoothGrid
  , carveRiversGrid
  , alluvialDepositGrid
  ) where

import qualified Data.IntMap.Strict as IntMap
import Topo.Hydrology.ChunkSlice (sliceGroundwaterChunk, sliceRiverChunk)
import Topo.Hydrology.Config
  ( GroundwaterConfig(..)
  , HydroConfig(..)
  , RiverConfig(..)
  , defaultGroundwaterConfig
  , defaultHydroConfig
  , defaultRiverConfig
  )
import qualified Topo.Hydrology.Depression as Depression
import qualified Topo.Hydrology.Flow as Flow
import qualified Topo.Hydrology.FlowRouting as FlowRouting
import qualified Topo.Hydrology.River as River
import qualified Topo.Hydrology.RiverCarve as RiverCarve
import qualified Topo.Hydrology.StageHydrology as StageHydrology
import qualified Topo.Hydrology.TerrainModify as TerrainModify
import Topo.Parameters (TerrainFormConfig)
import Topo.Pipeline (PipelineStage(..))
import Topo.Pipeline.Stage (StageId(..))
import Control.Monad.Except (throwError)
import Topo.Plugin (logInfo, getWorldP, putWorldP, PluginError(..))
import Topo.River (RiverTopologyConfig(..))
import Topo.TerrainGrid
  ( buildElevationGrid
  , buildHardnessGrid
  , buildMoistureGrid
  , buildPlateHardnessGrid
  , updateChunkElevationFromGrid
  , updateChunkMoistureFromGrid
  , validateTerrainGrid
  )
import Topo.Types
import Topo.World (TerrainWorld(..))
import qualified Data.Vector.Unboxed as U

-- | Apply hydrology routing and moisture derivation.
--
-- The 'TerrainFormConfig' drives a lightweight pre-classification of
-- terrain forms from the current elevation + plate hardness grids.
-- Per-form modifiers ('defaultTerrainFormModifiers') then adjust river
-- carving, bank erosion, stream-power erosion, deposition, coastal
-- reshaping, piedmont smoothing, and wet-area erosion.
applyHydrologyStage :: HydroConfig -> TerrainFormConfig -> PipelineStage
applyHydrologyStage cfg formCfg = PipelineStage StageHydrology "applyHydrology" "applyHydrology" Nothing [] Nothing $ do
  logInfo "applyHydrology: routing flow + moisture"
  world <- getWorldP
  let config = twConfig world
      terrain = twTerrain world
  (ChunkCoord minCx minCy, ChunkCoord maxCx maxCy) <-
    case validateTerrainGrid config terrain of
      Left err -> throwError (PluginInvariantError err)
      Right bounds -> pure bounds
  let size = wcChunkSize config
      gridW = (maxCx - minCx + 1) * size
      gridH = (maxCy - minCy + 1) * size
      elev0 = buildElevationGrid config terrain (ChunkCoord minCx minCy) gridW gridH
      hardness = buildHardnessGrid config terrain (ChunkCoord minCx minCy) gridW gridH
      plateHardness = buildPlateHardnessGrid config terrain (ChunkCoord minCx minCy) gridW gridH
      (elevFinal, moisture) =
        StageHydrology.computeHydrologyGrids
          cfg
          formCfg
          gridW
          gridH
          elev0
          hardness
          plateHardness
      terrain' = IntMap.mapWithKey (updateChunkElevationFromGrid config (ChunkCoord minCx minCy) gridW elevFinal) terrain
      terrain'' = IntMap.mapWithKey (updateChunkMoistureFromGrid config (ChunkCoord minCx minCy) gridW moisture) terrain'
  putWorldP world { twTerrain = terrain'' }

-- | Apply river routing and basin-level groundwater storage.
--
-- The water level parameter is the global sea-level threshold used for
-- flow sinks during river routing.  It is typically sourced from
-- 'hcWaterLevel' via the pipeline orchestrator.
--
-- A 'RiverTopologyConfig' controls segment extraction parameters
-- (discharge thresholds, network pruning, ocean-reachability, etc.).
applyRiverStage
  :: RiverConfig
  -> RiverTopologyConfig
  -> GroundwaterConfig
  -> Float          -- ^ Water level (from 'HydroConfig')
  -> PipelineStage
applyRiverStage riverCfg topoCfg gwCfg waterLevel = PipelineStage StageRivers "applyRivers" "applyRivers" Nothing [] Nothing $ do
  logInfo "applyRivers: routing rivers + groundwater"
  world <- getWorldP
  let config = twConfig world
      terrain = twTerrain world
  (ChunkCoord minCx minCy, ChunkCoord maxCx maxCy) <-
    case validateTerrainGrid config terrain of
      Left err -> throwError (PluginInvariantError err)
      Right bounds -> pure bounds
  let size = wcChunkSize config
      gridW = (maxCx - minCx + 1) * size
      gridH = (maxCy - minCy + 1) * size
      elev0 = buildElevationGrid config terrain (ChunkCoord minCx minCy) gridW gridH
      hardness = buildHardnessGrid config terrain (ChunkCoord minCx minCy) gridW gridH
      moisture = buildMoistureGrid config terrain (ChunkCoord minCx minCy) gridW gridH
      (rivers, groundwater, elevWithLakes) =
        River.buildRiverStageProducts
          riverCfg
          topoCfg
          gwCfg
          waterLevel
          gridW
          gridH
          elev0
          hardness
          moisture
      terrain' = IntMap.mapWithKey (updateChunkElevationFromGrid config (ChunkCoord minCx minCy) gridW elevWithLakes) terrain
      rivers'' = IntMap.mapWithKey (sliceRiverChunk config (ChunkCoord minCx minCy) gridW rivers) terrain'
      groundwater'' = IntMap.mapWithKey (sliceGroundwaterChunk config (ChunkCoord minCx minCy) gridW groundwater) terrain'
  putWorldP world { twTerrain = terrain', twRivers = rivers'', twGroundwater = groundwater'' }

-- | Breach land-sinks (local minima above @waterLevel@) by lowering them
-- by @breachDepth@.  Submerged tiles are left untouched.  This is the
-- sink-breaching algorithm parameterised directly (rather than through
-- a full 'HydroConfig'), so river routing tests can target this behavior
-- in isolation.
--
-- The breached elevation is floor-clamped at @waterLevel + 1e-5@ so that
-- tiles near sea level (e.g. those placed by the post-erosion clamp at
-- @waterLevel + 1e-5@) are never pushed below @waterLevel@.  Without this,
-- 'fillDepressions' would treat the newly-submerged tile as a seed and
-- 'flowDirectionsLand' would mark it as a terminal sink, causing rivers
-- to display inland delta fans on tiles that still render as land.
breachSinksLand
  :: Float          -- ^ waterLevel
  -> Float          -- ^ breachDepth
  -> Int -> Int     -- ^ gridW, gridH
  -> U.Vector Float -- ^ elevation grid
  -> U.Vector Float
breachSinksLand = FlowRouting.breachSinksLand

-- | Fill land depressions using priority-flood so every land tile has a
-- monotonic flow path to the ocean (or grid boundary).
--
-- Algorithm (Barnes et al. 2014, \"Priority-Flood\"):
--
-- 1. Seed the priority queue with all grid-boundary tiles and all
--    submerged tiles (elevation ≤ @waterLevel@).
-- 2. Pop the lowest-elevation cell from the queue.
-- 3. For each unvisited hex neighbour, set its filled elevation to
--    @max(own_elev, pourer_elev + epsilon)@ and add it to the queue.
-- 4. Repeat until all cells are visited.
--
-- The tiny epsilon (@1e-5@) guarantees a strictly monotonic gradient
-- from filled tiles toward their pour point, so steepest-descent
-- routing always finds a unique downhill path.
--
-- Submerged tiles are never modified.  The result replaces the
-- elevation grid before flow routing.
fillDepressions
  :: Float          -- ^ waterLevel — tiles at or below are ocean seeds
  -> Int -> Int     -- ^ gridW, gridH
  -> U.Vector Float -- ^ elevation grid
  -> U.Vector Float
fillDepressions = Depression.fillDepressions

-- | Breach shallow isolated land-sinks by raising them just above the rim.
--
-- A single pass over the grid that finds every land local-minimum
-- (tile whose elevation is ≤ all 6 hex neighbours) and raises it to
-- @min(neighbour_elev) + epsilon@.  This gives steepest-descent
-- routing an exit towards the lowest neighbour.  Only sinks whose
-- depth (rim − floor) is at most @maxBreachDepth@ are modified —
-- deeper sinks are left intact as natural terrain features (lakes).
--
-- Submerged tiles (≤ @waterLevel@) are never modified.
--
-- __Limitation:__ a single parallel pass can only resolve /isolated/
-- single-tile sinks (tile strictly below all neighbours).  Multi-tile
-- flat depressions or plateaux require 'fillDepressions'.
--
-- Note: this function is /not/ currently wired into the pipeline
-- (river routing uses 'fillDepressions' instead), but it is exported
-- for testing and potential future use.
breachRemainingSinks
  :: Float          -- ^ waterLevel — tiles below this are skipped
  -> Int -> Int     -- ^ gridW, gridH
  -> U.Vector Float -- ^ elevation grid
  -> U.Vector Float
breachRemainingSinks = Depression.breachRemainingSinks

-- | Expand inland depressions that receive river discharge into proper
-- lakes.
--
-- After flow routing, submerged tiles (@elev ≤ waterLevel@) that are not
-- connected to the grid boundary form inland depressions.  When rivers
-- drain into such depressions but the submerged area is too small to be
-- classified as a lake by 'Topo.WaterBody' (< @minLakeSize@ tiles), the
-- river appears to terminate on dry land — an inland delta artifact.
--
-- This function:
--
--  1. Computes an ocean mask (boundary-connected submerged tiles via BFS).
--  2. Groups remaining inland submerged tiles into connected components.
--  3. For each inland component that receives significant river discharge
--     (any adjacent land tile has @discharge ≥ minDischarge@ flowing
--     into the component), expands the basin by lowering the cheapest
--     adjacent land tiles to just below @waterLevel@ until the basin
--     reaches @minLakeSize@ tiles.
--
-- Returns the modified elevation grid.  Only tiles that need lowering
-- to form lakes are changed; everything else is untouched.
createRiverLakes
  :: Float          -- ^ waterLevel
  -> Float          -- ^ minDischarge — minimum river discharge to trigger lake
  -> Int            -- ^ minLakeSize — expand basins to at least this many tiles
  -> Int -> Int     -- ^ gridW, gridH
  -> U.Vector Float -- ^ elevation grid (post-erosion)
  -> U.Vector Int   -- ^ flow directions (-1 = sink)
  -> U.Vector Float -- ^ discharge per tile
  -> U.Vector Float -- ^ modified elevation with lake beds
createRiverLakes = FlowRouting.createRiverLakes

-- | Compute steepest-descent flow directions using all 6 hex neighbours.
-- Returns the index of the lowest neighbour, or @-1@ if no neighbour is
-- lower (sink).
flowDirections :: Int -> Int -> U.Vector Float -> U.Vector Int
flowDirections = FlowRouting.flowDirections

-- | Like 'flowDirections' but treats submerged tiles (elevation < waterLevel)
-- as terminal sinks.  Submerged tiles always receive flow direction @-1@;
-- land tiles may flow into submerged neighbours as terminal sinks (river
-- mouths).  This confines flow /accumulation/ to the land surface so that
-- river discharge totals reflect continental drainage rather than
-- ocean-floor topology.
-- Uses all 6 hex neighbours.
flowDirectionsLand :: Float -> Int -> Int -> U.Vector Float -> U.Vector Int
flowDirectionsLand = FlowRouting.flowDirectionsLand

-- | Accumulate flow from high to low elevation.
--
-- The @flowBonus@ vector carries a per-tile additive multiplier from
-- terrain form modifiers.  When a tile sends its accumulated flow
-- downstream, the amount is scaled by @1 + bonus@.  Badlands (high
-- runoff), passes (topographic funnelling), and valleys (flow
-- convergence) all amplify downstream accumulation via positive
-- bonuses.  Plateaus (negative bonus) reduce it.
flowAccumulation :: HydroConfig -> U.Vector Float -> U.Vector Float -> U.Vector Int -> U.Vector Float
flowAccumulation cfg flowBonus elev flow =
  Flow.flowAccumulation (hcBaseAccumulation cfg) flowBonus elev flow

carveRiversGrid :: HydroConfig -> Int -> Int -> U.Vector Float -> U.Vector Float -> U.Vector Float -> U.Vector Float -> U.Vector Float
carveRiversGrid cfg _gridW _gridH elev acc hardness erosionMult =
  RiverCarve.carveRiversGrid
    (hcMinAccumulation cfg)
    (hcWaterLevel cfg)
    (hcRiverCarveMaxDepth cfg)
    (hcRiverCarveScale cfg)
    (hcHardnessErodeWeight cfg)
    elev
    acc
    hardness
    erosionMult

-- | Deposit alluvial sediment where rivers decelerate.
--
-- Deposition occurs on low-slope land tiles, scaled inversely with slope
-- (more deposit where the gradient is gentler) and by normalized flow.
-- A sink-protection guard prevents deposits from creating new local
-- minima: the deposit is clamped so the tile never rises above the
-- elevation of its lowest neighbour.
--
-- The @depositFactor@ vector (1 \u2212 suppression) scales the raw deposit
-- per tile, allowing terrain form modifiers to suppress deposition in
-- e.g. canyons and cliffs.
alluvialDepositGrid :: HydroConfig -> Int -> Int -> U.Vector Float -> U.Vector Float -> U.Vector Float -> U.Vector Float
alluvialDepositGrid cfg gridW gridH elev acc depositFactor =
  TerrainModify.alluvialDepositGrid
    (hcMinAccumulation cfg)
    (hcAlluvialMaxSlope cfg)
    (hcWaterLevel cfg)
    gridW
    gridH
    elev
    acc
    (U.map (* hcAlluvialDepositScale cfg) depositFactor)

-- | Two-part coastal reshaping: erode land adjacent to water, raise
-- shallow ocean adjacent to land.
--
-- The @erosionMult@ vector scales the land-lowering amount, and
-- @smoothResist@ scales the ocean-raising amount (inverted: higher
-- resistance means less shelf building).
-- | Smooth the piedmont (foothills) transition zone.
--
-- Identifies land tiles in the moderate-slope band between
-- @hcPiedmontSlopeMin@ and @hcPiedmontSlopeMax@, then blends their
-- elevation toward the mean of their hex neighbours.  This creates
-- natural bajadas and pediments at mountain bases where talus
-- and alluvial fans smooth the mountain-to-plain transition.
--
-- The primary gate is 'FormFoothill' from the pre-classified terrain
-- form grid.  Tiles not classified as foothills fall back to the
-- fallback slope-band heuristic (slope in @[slopeMin, slopeMax]@ with
-- a steeper neighbour).  This gives the best of both approaches:
-- the form system captures the transition zone directly, while the
-- fallback handles edge cases the classification might miss.
--
-- The @smoothResist@ vector attenuates the blend per tile, so that
-- terrain forms with high smooth resistance (cliffs, escarpments)
-- preserve their morphology even when they fall in the piedmont zone.
--
-- Tiles below sea level are untouched.
piedmontSmoothGrid
  :: HydroConfig
  -> Int                   -- ^ Grid width
  -> Int                   -- ^ Grid height
  -> U.Vector Float        -- ^ Elevation grid
  -> U.Vector TerrainForm  -- ^ Pre-classified terrain form grid
  -> U.Vector Float        -- ^ Per-tile smooth resistance [0..1]
  -> U.Vector Float        -- ^ Smoothed elevation grid
piedmontSmoothGrid cfg gridW gridH elev formGrid smoothResist =
  TerrainModify.piedmontSmoothGrid
    (hcPiedmontSlopeMin cfg)
    (hcPiedmontSlopeMax cfg)
    (hcPiedmontSmoothStrength cfg)
    (hcWaterLevel cfg)
    gridW
    gridH
    elev
    formGrid
    smoothResist


