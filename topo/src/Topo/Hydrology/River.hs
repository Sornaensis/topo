-- | River-stage routing assembly for hydrology.
--
-- This module gathers the flow-routing, river topology, and basin-level
-- groundwater computations used by 'Topo.Hydrology.applyRiverStage'.
module Topo.Hydrology.River
  ( buildRiverStageProducts
  ) where

import Topo.Hydrology.Flow (flowAccumulationWithBase)
import Topo.Hydrology.FlowRouting
  ( breachSinksLand
  , createRiverLakes
  , flowDirectionsLand
  )
import qualified Topo.Hydrology.Depression as Depression
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
import Topo.Math (clamp01)
import Topo.River (RiverTopologyConfig(..), computeRiverSegments)
import Topo.Types
  ( GroundwaterChunk(..)
  , RiverChunk(..)
  )
import qualified Data.Vector.Unboxed as U

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
  let wl = waterLevel
      elevBreached = breachSinksLand wl 0.02 gridW gridH elev0
      elevFilled = fillDepressions wl gridW gridH elevBreached
      flow = flowDirectionsLand wl gridW gridH elevFilled
      acc = flowAccumulationWithBase (rcBaseAccumulation riverCfg) elevFilled flow
      basinIds = Groundwater.basinIdsFromFlow flow
      basinStats = Groundwater.basinRechargeStats (gwRechargeScale gwCfg) basinIds moisture
      (basinStorage, basinDischarge, basinSize) =
        Groundwater.basinStorageStats
          (gwMinBasinSize gwCfg)
          (gwStorageScale gwCfg)
          (gwDischargeScale gwCfg * gwPermeability gwCfg)
          basinStats
      baseflow = Groundwater.basinBaseflow basinIds basinDischarge basinSize (rcBaseflowScale riverCfg)
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
        computeRiverSegments topoCfg gridW gridH flow discharge riverOrder elevFilled elev0 wl
      rivers = RiverChunk
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
      groundwater = GroundwaterChunk
        { gwStorage = Groundwater.basinPerTile basinIds basinStorage basinSize
        , gwRecharge = U.map (* gwRechargeScale gwCfg) (U.map clamp01 moisture)
        , gwDischarge = Groundwater.basinPerTile basinIds basinDischarge basinSize
        , gwBasinId = basinIds
        , gwInfiltration = U.empty
        , gwWaterTableDepth = U.empty
        , gwRootZoneMoisture = U.empty
        }
      elevWithLakes = createRiverLakes wl (rtMinDischarge topoCfg) 4 gridW gridH elev0 flow discharge
  in (rivers, groundwater, elevWithLakes)

-- Local wrapper keeps the imported symbol list explicit in this module.
fillDepressions :: Float -> Int -> Int -> U.Vector Float -> U.Vector Float
fillDepressions = Depression.fillDepressions
