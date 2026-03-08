{-# LANGUAGE OverloadedStrings #-}

-- | Compatibility and test-facing hydrology helpers.
--
-- This module holds the exported utility surface that historically lived in
-- 'Topo.Hydrology', while delegating to focused internal modules.
module Topo.Hydrology.Compat
  ( breachSinksLand
  , fillDepressions
  , breachRemainingSinks
  , flowDirections
  , flowDirectionsLand
  , flowAccumulation
  , carveRiversGrid
  , alluvialDepositGrid
  , piedmontSmoothGrid
  ) where

import qualified Topo.Hydrology.Depression as Depression
import qualified Topo.Hydrology.Flow as Flow
import qualified Topo.Hydrology.FlowRouting as FlowRouting
import qualified Topo.Hydrology.RiverCarve as RiverCarve
import qualified Topo.Hydrology.TerrainModify as TerrainModify
import Topo.Hydrology.Config (HydroConfig(..))
import Topo.Types (TerrainForm(..))
import qualified Data.Vector.Unboxed as U

-- | Breach land-sinks (local minima above @waterLevel@) by lowering them
-- by @breachDepth@. Submerged tiles are left untouched.
--
-- The breached elevation is floor-clamped at @waterLevel + 1e-5@ so that
-- tiles near sea level are never pushed below @waterLevel@.
breachSinksLand
  :: Float
  -> Float
  -> Int
  -> Int
  -> U.Vector Float
  -> U.Vector Float
breachSinksLand = FlowRouting.breachSinksLand

-- | Fill land depressions using priority-flood so every land tile has a
-- monotonic flow path to the ocean (or grid boundary).
fillDepressions
  :: Float
  -> Int
  -> Int
  -> U.Vector Float
  -> U.Vector Float
fillDepressions = Depression.fillDepressions

-- | Breach shallow isolated land-sinks by raising them just above the rim.
breachRemainingSinks
  :: Float
  -> Int
  -> Int
  -> U.Vector Float
  -> U.Vector Float
breachRemainingSinks = Depression.breachRemainingSinks

-- | Compute steepest-descent flow directions using all 6 hex neighbours.
flowDirections :: Int -> Int -> U.Vector Float -> U.Vector Int
flowDirections = FlowRouting.flowDirections

-- | Like 'flowDirections' but treats submerged tiles as terminal sinks.
flowDirectionsLand :: Float -> Int -> Int -> U.Vector Float -> U.Vector Int
flowDirectionsLand = FlowRouting.flowDirectionsLand

-- | Accumulate flow from high to low elevation.
--
-- The @flowBonus@ vector carries a per-tile additive multiplier.
flowAccumulation
  :: HydroConfig
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Int
  -> U.Vector Float
flowAccumulation cfg flowBonus elev flow =
  Flow.flowAccumulation (hcBaseAccumulation cfg) flowBonus elev flow

-- | Carve river channels based on flow accumulation and hardness.
carveRiversGrid
  :: HydroConfig
  -> Int
  -> Int
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Float
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
alluvialDepositGrid
  :: HydroConfig
  -> Int
  -> Int
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Float
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

-- | Smooth the piedmont (foothills) transition zone.
piedmontSmoothGrid
  :: HydroConfig
  -> Int
  -> Int
  -> U.Vector Float
  -> U.Vector TerrainForm
  -> U.Vector Float
  -> U.Vector Float
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
