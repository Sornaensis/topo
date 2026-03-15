-- | Precipitation assembly for climate generation.
--
-- This module owns the functions that transform raw condensation,
-- orographic uplift, plate-height bias, and polar-latitude terms into
-- the final precipitation grid.
--
-- The main entry point is 'assemblePrecipGrid', called from the
-- climate orchestration layer after moisture transport completes.
-- Individual components are exported for testing.
module Topo.Climate.Precipitation
  ( -- * Grid assembly
    assemblePrecipGrid
    -- * Component helpers
  , orographicAt
  , plateHeightPrecipBiasAt
  , plateHeightTempBiasAt
    -- * Polar floor
  , polarPrecipFloor
  ) where

import Topo.Climate.Config
import Topo.Grid.HexDirection (nearestHexDirection, traceIndexInDirection)
import Topo.Hex (hexOpposite)
import Topo.Math (clamp01, clampLat)
import Topo.Planet (LatitudeMapping(..))
import qualified Data.Vector.Unboxed as U

-- | Assemble the final precipitation grid from accumulated condensation,
-- orographic uplift, plate-height bias, and polar floor.
--
-- Each tile's precipitation is:
--
-- @
-- raw = accumCondensation + orographic + plateBias
-- precip = clamp01 (max raw polarFloor)
-- @
assemblePrecipGrid
  :: Int                  -- ^ grid width
  -> Int                  -- ^ grid height
  -> LatitudeMapping
  -> ClimateConfig
  -> Float                -- ^ water level
  -> Int                  -- ^ min tile Y (global)
  -> U.Vector Float       -- ^ accumulated condensation from transport
  -> U.Vector Float       -- ^ wind direction grid
  -> U.Vector Float       -- ^ elevation grid
  -> U.Vector Float       -- ^ plate-height grid
  -> U.Vector Float
assemblePrecipGrid gridW gridH lm cfg waterLevel minTileY accumCondensation windDir elev plateHeight =
  let prc = ccPrecipitation cfg
      n = gridW * gridH
  in U.generate n $ \i ->
    let o = orographicAt gridW gridH cfg windDir elev i
        plateBias = plateHeightPrecipBiasAt cfg waterLevel (plateHeight U.! i)
        rawPrecip = accumCondensation U.! i + o + plateBias
        y = i `div` gridW
        gy = minTileY + y
        pFloor = polarPrecipFloor lm prc gy
    in clamp01 (max rawPrecip pFloor)

-- ---------------------------------------------------------------------------
-- Orographic uplift
-- ---------------------------------------------------------------------------

-- | Orographic uplift estimated from the nearest upwind hex direction at a
-- tile, sampled over the configured number of hex steps.
orographicAt :: Int -> Int -> ClimateConfig -> U.Vector Float -> U.Vector Float -> Int -> Float
orographicAt gridW gridH cfg windDir elev i =
  let prc = ccPrecipitation cfg
      dir = windDir U.! i
      upwindDir = hexOpposite (nearestHexDirection dir)
      ni = traceIndexInDirection gridW gridH upwindDir (round (precOrographicStep prc)) i
      h0 = elev U.! i
      h1 = elev U.! ni
      rise = max 0 (h0 - h1)
  in clamp01 (rise * precOrographicLift prc * precOrographicScale prc)

-- ---------------------------------------------------------------------------
-- Plate-height biases
-- ---------------------------------------------------------------------------

-- | Continental-elevation temperature effect: high plateaus are slightly
-- cooler.  Uses 'tmpPlateHeightCooling' (not the main lapse rate) to
-- avoid double-counting altitude cooling.
plateHeightTempBiasAt :: ClimateConfig -> Float -> Float -> Float
plateHeightTempBiasAt cfg waterLevel plateHeight =
  let land = clamp01 (plateHeight - waterLevel)
  in -land * tmpPlateHeightCooling (ccTemperature cfg)

-- | Plate-height precipitation bias: elevated continental interiors
-- receive an orographic bonus.
plateHeightPrecipBiasAt :: ClimateConfig -> Float -> Float -> Float
plateHeightPrecipBiasAt cfg waterLevel plateHeight =
  let land = clamp01 (plateHeight - waterLevel)
  in land * precOrographicLift (ccPrecipitation cfg)

-- ---------------------------------------------------------------------------
-- Polar precipitation floor
-- ---------------------------------------------------------------------------

-- | Polar precipitation floor for a single tile.
--
-- Ramp from 0 at 'precPolarLatitude' to 'precPolarFloor' at the pole
-- (90°).  Ensures polar regions receive minimum precipitation even when
-- transport delivers little moisture.
polarPrecipFloor :: LatitudeMapping -> PrecipitationConfig -> Int -> Float
polarPrecipFloor lm prc gy =
  let lat = clampLat (fromIntegral gy * lmRadPerTile lm + lmBiasRad lm)
      latDeg = abs (lat * (180.0 / pi))
      polarLat = max 0.001 (precPolarLatitude prc)
      polarFrac = clamp01 ((latDeg - polarLat) / (90.0 - polarLat))
  in precPolarFloor prc * polarFrac
