-- | River-stage routing assembly for hydrology.
--
-- This module gathers the flow-routing, river topology, and basin-level
-- groundwater computations used by 'Topo.Hydrology.applyRiverStage'.
module Topo.Hydrology.River
  ( buildRiverStageProducts
  , buildRiverStageWorldLayers
  ) where

import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import Data.Word (Word32)
import Topo.Hydrology.ChunkSlice (sliceGroundwaterChunk, sliceRiverChunk)
import Topo.Hydrology.Flow (flowAccumulationWithBase)
import Topo.Hydrology.FlowRouting
  ( breachSinksLand
  , createRiverLakes
  , flowDirectionsLand
  )
import Topo.Hydrology.Depression (fillDepressions)
import qualified Topo.Hydrology.Groundwater as Groundwater
import Topo.Hydrology.Config
  ( GroundwaterConfig(..)
  , RiverConfig(..)
  )
import Topo.Hydrology.RiverCarve
  ( riverDepthWithHardness
  , riverDepositPotential
  , riverErosionPotential
  , strahlerOrder
  )
import Topo.River (RiverTopologyConfig(..), computeRiverSegments)
import Topo.TerrainGrid
  ( buildElevationGrid
  , buildHardnessGrid
  , buildMoistureGrid
  , updateChunkElevationFromGrid
  )
import Topo.Types
  ( ChunkCoord(..)
  , GroundwaterChunk(..)
  , RiverChunk(..)
  , TerrainChunk
  , WorldConfig
  )
import qualified Data.Vector.Unboxed as U

data RiverRoutingState = RiverRoutingState
  { rrElevFilled :: U.Vector Float
  , rrFlow :: U.Vector Int
  , rrAccum :: U.Vector Float
  }

buildRiverRoutingState
  :: RiverConfig
  -> Float
  -> Int
  -> Int
  -> U.Vector Float
  -> RiverRoutingState
buildRiverRoutingState riverCfg waterLevel gridW gridH elev0 =
  let elevBreached = breachSinksLand waterLevel (rcSinkBreachDepth riverCfg) gridW gridH elev0
      elevFilled = fillDepressions waterLevel gridW gridH elevBreached
      flow = flowDirectionsLand waterLevel gridW gridH elevFilled
      acc = flowAccumulationWithBase (rcBaseAccumulation riverCfg) elevFilled flow
  in RiverRoutingState
      { rrElevFilled = elevFilled
      , rrFlow = flow
      , rrAccum = acc
      }

buildRiverChunk
  :: RiverConfig
  -> RiverTopologyConfig
  -> Float
  -> Int
  -> Int
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Word32
  -> U.Vector Float
  -> RiverRoutingState
  -> RiverChunk
buildRiverChunk riverCfg topoCfg waterLevel gridW gridH elev0 hardness basinIds baseflow routingState =
  let elevFilled = rrElevFilled routingState
      flow = rrFlow routingState
      acc = rrAccum routingState
      riverOrder = strahlerOrder gridW gridH elevFilled flow acc (rcOrderMinAccumulation riverCfg)
      discharge = U.zipWith (+) (U.map (* rcDischargeScale riverCfg) acc) baseflow
      depth = riverDepthWithHardness
        (rcMinAccumulation riverCfg)
        (rcChannelMaxDepth riverCfg)
        (rcChannelDepthScale riverCfg)
        (rcHardnessDepthWeight riverCfg)
        acc
        hardness
      erosionPotential = riverErosionPotential
        gridW
        gridH
        elev0
        acc
        hardness
        (rcMinAccumulation riverCfg)
        (rcErosionScale riverCfg)
        (rcHardnessErosionWeight riverCfg)
      depositPotential = riverDepositPotential
        gridW
        gridH
        elev0
        acc
        (rcMinAccumulation riverCfg)
        (rcDepositMaxSlope riverCfg)
        (rcDepositScale riverCfg)
      (segOffsets, segEntry, segExit, segDisc, segOrd) =
        computeRiverSegments topoCfg gridW gridH flow discharge riverOrder elevFilled elev0 waterLevel
  in RiverChunk
      { rcFlowAccum = acc
      , rcDischarge = discharge
      , rcChannelDepth = depth
      , rcRiverOrder = riverOrder
      , rcBasinId = basinIds
      , rcBaseflow = baseflow
      , rcErosionPotential = erosionPotential
      , rcDepositPotential = depositPotential
      , rcFlowDir = flow
      , rcSegOffsets = segOffsets
      , rcSegEntryEdge = segEntry
      , rcSegExitEdge = segExit
      , rcSegDischarge = segDisc
      , rcSegOrder = segOrd
      }

-- | Compute river and groundwater products for the river pipeline stage.
--
-- Returns:
--
-- 1. full-grid river chunk payload,
-- 2. full-grid groundwater chunk payload,
-- 3. lake-adjusted elevation grid.
buildRiverStageProducts
  :: RiverConfig
  -> RiverTopologyConfig
  -> GroundwaterConfig
  -> Float
  -> Int
  -> Int
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Float
  -> (RiverChunk, GroundwaterChunk, U.Vector Float)
buildRiverStageProducts riverCfg topoCfg gwCfg waterLevel gridW gridH elev0 hardness moisture =
  let routingState = buildRiverRoutingState riverCfg waterLevel gridW gridH elev0
      flow = rrFlow routingState
      (basinIds, baseflow, groundwater) =
        Groundwater.buildGroundwaterChunk gwCfg (rcBaseflowScale riverCfg) flow moisture
      rivers = buildRiverChunk
        riverCfg
        topoCfg
        waterLevel
        gridW
        gridH
        elev0
        hardness
        basinIds
        baseflow
        routingState
      elevWithLakes = createRiverLakes
        waterLevel
        (rtMinDischarge topoCfg)
        (rcMinLakeSize riverCfg)
        gridW
        gridH
        elev0
        flow
        (rcDischarge rivers)
  in (rivers, groundwater, elevWithLakes)

-- | Build updated terrain/river/groundwater chunk maps for river stage.
buildRiverStageWorldLayers
  :: WorldConfig
  -> ChunkCoord
  -> Int
  -> Int
  -> IntMap TerrainChunk
  -> RiverConfig
  -> RiverTopologyConfig
  -> GroundwaterConfig
  -> Float
  -> (IntMap TerrainChunk, IntMap RiverChunk, IntMap GroundwaterChunk)
buildRiverStageWorldLayers config minCoord gridW gridH terrain riverCfg topoCfg gwCfg waterLevel =
  let elev0 = buildElevationGrid config terrain minCoord gridW gridH
      hardness = buildHardnessGrid config terrain minCoord gridW gridH
      moisture = buildMoistureGrid config terrain minCoord gridW gridH
      (rivers, groundwater, elevWithLakes) =
        buildRiverStageProducts
          riverCfg
          topoCfg
          gwCfg
          waterLevel
          gridW
          gridH
          elev0
          hardness
          moisture
      terrain' = IntMap.mapWithKey (updateChunkElevationFromGrid config minCoord gridW elevWithLakes) terrain
      rivers'' = IntMap.mapWithKey (sliceRiverChunk config minCoord gridW rivers) terrain'
      groundwater'' = IntMap.mapWithKey (sliceGroundwaterChunk config minCoord gridW groundwater) terrain'
  in (terrain', rivers'', groundwater'')
