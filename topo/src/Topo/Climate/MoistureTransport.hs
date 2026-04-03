{-# LANGUAGE BangPatterns #-}

-- | Moisture transport simulation for climate generation.
--
-- This module owns the iterative moisture-advection loop that produces
-- the equilibrium atmospheric moisture field and cumulative condensation
-- (precipitation) grid.  The physics models implemented here are:
--
--   * __Model E.1__ — wind-driven advection with hex-direction upwind sampling
--   * __Model E.2__ — saturation-based condensation (Clausius-Clapeyron)
--   * __Model E.3__ — vegetation ET recycling
--   * __Model E.4__ — persistent ocean moisture reinjection
--   * __Model E.5__ — ITCZ convergence boost
--   * __Model E.6__ — persistent land ET reinjection
--   * __Model E.7__ — convective precipitation
--
-- The pipeline entry point is 'moistureTransportAccum', which drives
-- 'moistureStepGrid' over a configurable number of iterations and
-- accumulates per-tile condensation into the precipitation field.
--
-- == Transport state
--
-- 'TransportState' packages the atmospheric moisture and accumulated
-- precipitation vectors so conservation and budget terms are
-- self-documenting and testable.
module Topo.Climate.MoistureTransport
  ( -- * Transport state
    TransportState(..)
  , mkTransportState
  , runTransport
    -- * Iterative transport
  , moistureTransportAccum
    -- * Single-step grid transport
  , moistureStepGrid
  , moistureFlowAtGrid
    -- * Legacy chunk-local helpers (for testing)
  , moistureTransport
  , moistureStep
  , moistureFlowAt
    -- * Evaporation initialisation
  , evapAt
  , evapAtXY
    -- * Internal sampling (exported for testing)
  , sampleAlongUpwindHexPath
  ) where

import Topo.Climate.Config
import Topo.Climate.Evaporation (oceanEvaporation, landEvapotranspiration, satNormCfg)
import Topo.Grid.HexDirection
  ( nearestHexDirection
  , stepIndexInDirection
  , traceIndexInDirection
  )
import Topo.Hex (hexOpposite)
import Topo.Math (clamp01, iterateN, lerp)
import Topo.Noise (noise2D)
import Topo.Planet (LatitudeMapping(..))
import Topo.Solar (annualMeanInsolation, defaultSolarConfig)
import Topo.Types
import Data.Word (Word64)
import qualified Data.Vector.Unboxed as U

-- ---------------------------------------------------------------------------
-- Transport state ADT
-- ---------------------------------------------------------------------------

-- | Bundled atmospheric moisture and accumulated precipitation.
--
-- Carrying both fields in a single record makes the conservation
-- relationship explicit: every unit of moisture that condenses
-- (minus recycling) adds to 'trAccumPrecip', and the sum
-- @trMoisture + trAccumPrecip@ should be auditable against the
-- total injected moisture for budget analysis.
data TransportState = TransportState
  { trMoisture     :: !(U.Vector Float)
    -- ^ Current atmospheric moisture field (normalised 0–1).
  , trAccumPrecip  :: !(U.Vector Float)
    -- ^ Cumulative net condensation (precipitation) across all
    --   transport iterations so far.
  } deriving (Show)

-- | Construct an initial 'TransportState' from evaporation-derived
-- moisture with zero accumulated precipitation.
mkTransportState :: U.Vector Float -> TransportState
mkTransportState moisture = TransportState
  { trMoisture    = moisture
  , trAccumPrecip = U.replicate (U.length moisture) 0
  }

-- | Run the full moisture transport loop, returning the final
-- 'TransportState'.
--
-- This is a thin wrapper around 'moistureTransportAccum' that exposes
-- the result as a structured type rather than a bare tuple.
runTransport
  :: Int
  -> (U.Vector Float -> (U.Vector Float, U.Vector Float))
  -> U.Vector Float
  -> TransportState
runTransport iters step initial =
  let (finalMoisture, accumPrecip) = moistureTransportAccum iters step initial
  in TransportState
    { trMoisture    = finalMoisture
    , trAccumPrecip = accumPrecip
    }

-- ---------------------------------------------------------------------------
-- Iterative transport (tuple interface — kept for backwards compat)
-- ---------------------------------------------------------------------------

-- | Run moisture transport for @n@ iterations, accumulating per-tile
-- net condensation (precipitation) across all steps.
--
-- Returns @(finalMoisture, accumulatedPrecipitation)@.  The accumulated
-- precipitation vector sums per-tile net condensation from each
-- iteration, providing a physics-based precipitation field where
-- moisture that condenses (minus ET recycling) contributes to
-- precipitation at the tile where condensation occurs.
--
-- Ocean\/land reinjection and ITCZ boost feed atmospheric moisture
-- only and are /not/ double-counted in the precipitation accumulator.
moistureTransportAccum
  :: Int
  -> (U.Vector Float -> (U.Vector Float, U.Vector Float))
     -- ^ Step function: current moisture -> (new moisture, condensed this step)
  -> U.Vector Float
     -- ^ Initial moisture
  -> (U.Vector Float, U.Vector Float)
     -- ^ (final moisture, accumulated precipitation)
moistureTransportAccum iters step initial =
  go iters initial (U.replicate (U.length initial) 0)
  where
    go 0 moist accum = (moist, accum)
    go !n moist !accum =
      let (moist', condensed) = step moist
          accum' = U.zipWith (+) accum condensed
      in go (n - 1) moist' accum'

-- ---------------------------------------------------------------------------
-- Single transport step — global grid
-- ---------------------------------------------------------------------------

-- | Single transport iteration over the global moisture grid.
--
-- Combines:
--
--   * Wind-driven advection + saturation-based condensation (Models E.1/E.2)
--   * Per-iteration ET recycling through vegetation (Model E.3)
--   * Persistent ocean moisture reinjection (Model E.4)
--   * ITCZ convergence moisture boost (Model E.5)
--   * Persistent land ET reinjection (Model E.6)
--
-- Returns @(newMoisture, condensedThisStep)@.  The condensation vector
-- tracks net precipitation (condensation minus recycling) per tile,
-- for accumulation across iterations in 'moistureTransportAccum'.
moistureStepGrid
  :: Int -> Int -> ClimateConfig
  -> U.Vector Float -> U.Vector Float   -- ^ windDir, windSpd
  -> U.Vector Float -> U.Vector Float   -- ^ elev, tempGrid
  -> U.Vector Float                      -- ^ vegCover
  -> U.Vector Float -> U.Vector Float   -- ^ oceanEvap, oceanMask
  -> U.Vector Float                      -- ^ itczBoost
  -> Float                               -- ^ water level
  -> U.Vector Float                      -- ^ current moisture
  -> (U.Vector Float, U.Vector Float)
moistureStepGrid gridW gridH cfg windDir windSpd elev tempGrid
                 vegCover oceanEvap oceanMask itczBoost waterLevel moisture =
  let mst = ccMoisture cfg
      n = U.length moisture
      pairs = U.generate n $ \i ->
        let (base, precip) = moistureFlowAtGrid gridW gridH cfg windDir windSpd elev
                               tempGrid vegCover moisture i
            -- Ocean reinjection (Model E.4): ocean tiles never drop below
            -- their physical evaporation rate, keeping a persistent moisture
            -- source independent of initial conditions.
            reinject = if oceanMask U.! i > 0.5
                       then max base (oceanEvap U.! i)
                       else base
            -- Land ET reinjection (Model E.6): vegetated land tiles inject
            -- moisture from soil/leaf evapotranspiration each iteration,
            -- sustaining the "flying rivers" mechanism.
            -- Uses moistMinVegFloor to break the cold-start problem:
            -- even bare land contributes a minimum ET fraction so
            -- interior moisture can bootstrap vegetation growth.
            landET = if elev U.! i >= waterLevel
                     then moistBaseRecycleRate mst
                            * max (moistMinVegFloor mst) (vegCover U.! i)
                            * satNormCfg mst (tempGrid U.! i)
                     else 0
            -- ITCZ convergence boost (Model E.5): moisture concentrates
            -- near the intertropical convergence zone.
            boosted = reinject + itczBoost U.! i + landET
        in (clamp01 boosted, precip)
      (newMoist, condensed) = U.unzip pairs
  in (newMoist, condensed)

-- | Compute the new moisture and net condensation at a single grid
-- tile from advection, saturation-based condensation, and vegetation
-- recycling.  Upwind sampling follows the discrete hex path implied by
-- the local wind direction instead of Cartesian x\/y interpolation.
--
-- Returns @(remainingMoisture, netPrecipitation)@ where
-- @netPrecipitation = condensation − recycled@: moisture that actually
-- fell as precipitation at this tile.  Recycled vapour re-enters the
-- atmosphere and may condense in a later iteration.
moistureFlowAtGrid
  :: Int -> Int -> ClimateConfig
  -> U.Vector Float -> U.Vector Float
  -> U.Vector Float -> U.Vector Float
  -> U.Vector Float                      -- ^ vegCover
  -> U.Vector Float -> Int -> (Float, Float)
moistureFlowAtGrid gridW gridH cfg windDir windSpd elev tempGrid vegCover moisture i =
  let prc = ccPrecipitation cfg
      mst = ccMoisture cfg
      dir = windDir U.! i
      spd = windSpd U.! i * moistAdvectSpeed mst
      upwindMoisture = sampleAlongUpwindHexPath gridW gridH moisture i dir spd
      upwindElev = sampleAlongUpwindHexPath gridW gridH elev i dir spd
      h0 = elev U.! i
      cool = max 0 (upwindElev - h0) * precRainShadowLoss prc
      -- Wind-driven advection + local retention
      adv   = upwindMoisture * moistAdvect mst
      local = moisture U.! i * moistLocal mst
      totalMoisture = adv + local - cool
      -- Saturation-based condensation (Model E.2):
      -- The destination atmosphere can hold at most satNorm(T) moisture.
      -- Excess above that capacity condenses, modulated by the
      -- condensation rate to allow gradual precipitation over many
      -- iterations.
      dstCapacity  = satNormCfg mst (tempGrid U.! i)
      excess       = max 0 (totalMoisture - dstCapacity)
      forcedCond   = excess * moistCondensationRate mst
      -- Convective precipitation (Model E.7):
      -- Warm near-saturated air undergoes spontaneous convective
      -- uplift even on flat terrain.  Fires when relative humidity
      -- exceeds a configurable threshold and scales with temperature
      -- (warmer → more CAPE → stronger updrafts) and with saturation
      -- capacity to keep the result in moisture-fraction units.
      rh           = if dstCapacity > 0.001
                       then min 1 (totalMoisture / dstCapacity)
                       else 0
      convExcess   = max 0 (rh - moistConvectiveThreshold mst)
      convective   = convExcess * moistConvectiveRate mst
                       * (tempGrid U.! i) * dstCapacity
      condensation = forcedCond + convective
      -- ET recycling (Model E.3):
      -- Vegetation transpires a fraction of condensed moisture back to
      -- the atmosphere, sustaining continental interior humidity.
      vegC     = vegCover U.! i
      recycled = condensation * vegC * moistRecycleRate mst
                   * satNormCfg mst (tempGrid U.! i)
      -- Net precipitation: condensation minus what is immediately
      -- recycled back to the atmosphere.
      netPrecip = max 0 (condensation - recycled)
  in (clamp01 (totalMoisture - condensation + recycled), netPrecip)

-- ---------------------------------------------------------------------------
-- Upwind hex-path sampling
-- ---------------------------------------------------------------------------

-- | Sample a field from the upwind direction by walking along the nearest
-- hex direction and interpolating between successive hex steps for
-- sub-tile wind distances.
sampleAlongUpwindHexPath :: Int -> Int -> U.Vector Float -> Int -> Float -> Float -> Float
sampleAlongUpwindHexPath gridW gridH field idx dir distance =
  let fullSteps = max 0 (floor distance)
      frac = clamp01 (distance - fromIntegral fullSteps)
      upwindDir = hexOpposite (nearestHexDirection dir)
      baseIdx = traceIndexInDirection gridW gridH upwindDir fullSteps idx
      nextIdx = stepIndexInDirection gridW gridH upwindDir baseIdx
      baseValue = field U.! baseIdx
      nextValue = field U.! nextIdx
  in lerp baseValue nextValue frac

-- ---------------------------------------------------------------------------
-- Evaporation initialisation
-- ---------------------------------------------------------------------------

-- | Ocean evaporation at a global tile, using Dalton's Law (Model B).
--
-- Returns the evaporation intensity for a single tile given its
-- temperature and wind speed.  Used by the grid and per-chunk paths.
-- For ocean tiles the result is scaled by CC-based saturation; for
-- land tiles the result is just small noise.
evapAtXY :: Word64 -> LatitudeMapping -> ClimateConfig -> Float -> Int -> Int -> Float -> Float -> Float -> Float
evapAtXY seed lm cfg waterLevel _gx _gy elevation temp windSpdVal =
  let mst = ccMoisture cfg
      latRad = fromIntegral _gy * lmRadPerTile lm + lmBiasRad lm
      tiltDeg = lmTiltScale lm * 23.44
      insol = lmInsolation lm
            * annualMeanInsolation defaultSolarConfig tiltDeg 24.0 latRad
      n0 = noise2D seed (_gx + 4000) (_gy + 4000)
      noise = n0 * moistEvapNoiseScale mst
  in if elevation < waterLevel
     then clamp01 (oceanEvaporation mst temp windSpdVal insol + noise)
     else clamp01 noise

-- | Per-chunk ocean evaporation using Dalton's Law (Model B).
evapAt :: WorldConfig -> Word64 -> LatitudeMapping -> ClimateConfig -> Float -> TileCoord -> U.Vector Float -> U.Vector Float -> U.Vector Float -> Int -> Float
evapAt config seed lm cfg waterLevel origin elev tempVec windSpdVec i =
  let mst = ccMoisture cfg
      TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
      gy = oy + ly
      latRad = fromIntegral gy * lmRadPerTile lm + lmBiasRad lm
      tiltDeg = lmTiltScale lm * 23.44
      insol = lmInsolation lm
            * annualMeanInsolation defaultSolarConfig tiltDeg 24.0 latRad
      n0 = noise2D seed (ox + lx + 4000) (oy + ly + 4000)
      noise = n0 * moistEvapNoiseScale mst
      t = tempVec U.! i
      w = windSpdVec U.! i
  in if elev U.! i < waterLevel
     then clamp01 (oceanEvaporation mst t w insol + noise)
     else clamp01 noise

-- ---------------------------------------------------------------------------
-- Legacy chunk-local helpers (for testing)
-- ---------------------------------------------------------------------------

-- | Chunk-local iterative moisture transport.
moistureTransport :: WorldConfig -> Word64 -> LatitudeMapping -> ClimateConfig -> Float -> TileCoord -> U.Vector Float -> U.Vector Float -> U.Vector Float -> U.Vector Float -> U.Vector Float
moistureTransport config seed lm cfg waterLevel origin windDir windSpd tempVec elev =
  let n = U.length elev
      initial = U.generate n (evapAt config seed lm cfg waterLevel origin elev tempVec windSpd)
  in iterateN (moistIterations (ccMoisture cfg)) (moistureStep config cfg windDir windSpd elev) initial

-- | Legacy chunk-local moisture step retained for tests; uses the same
-- nearest-hex upwind sampling as the global transport path.
moistureStep :: WorldConfig -> ClimateConfig -> U.Vector Float -> U.Vector Float -> U.Vector Float -> U.Vector Float -> U.Vector Float
moistureStep config cfg windDir windSpd elev moisture =
  U.generate (U.length moisture) (moistureFlowAt config cfg windDir windSpd elev moisture)

-- | Legacy chunk-local moisture transport retained for tests; samples the
-- upwind moisture and elevation fields along the nearest hex path.
moistureFlowAt :: WorldConfig -> ClimateConfig -> U.Vector Float -> U.Vector Float -> U.Vector Float -> U.Vector Float -> Int -> Float
moistureFlowAt config cfg windDir windSpd elev moisture i =
  let prc = ccPrecipitation cfg
      mst = ccMoisture cfg
      size = wcChunkSize config
      dir = windDir U.! i
      spd = windSpd U.! i * moistAdvectSpeed mst
      upwindMoisture = sampleAlongUpwindHexPath size size moisture i dir spd
      upwindElev = sampleAlongUpwindHexPath size size elev i dir spd
      h0 = elev U.! i
      cool = max 0 (upwindElev - h0) * precRainShadowLoss prc
      adv = upwindMoisture * moistAdvect mst
      local = moisture U.! i * moistLocal mst
  in clamp01 (adv + local - cool)
