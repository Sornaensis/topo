{-# LANGUAGE BangPatterns #-}

-- | ITCZ (Inter-Tropical Convergence Zone) modelling.
--
-- Provides a convergence-field–driven ITCZ band that replaces the
-- simple Gaussian moisture injection (former Model E.5).  The key
-- idea: compute where trade winds actually converge on the hex grid,
-- then use that convergence signal — weighted by latitude proximity
-- to the thermal equator — to drive uplift and precipitation.
--
-- The module is shared between the climate generation stage
-- ('Topo.Climate') and the weather simulation ('Topo.Weather.Tick').
--
-- == Convergence field
--
-- 'convergenceField' computes a per-tile scalar from the wind
-- direction and speed grids.  Positive values indicate converging
-- airflow (uplift); negative values indicate divergence (subsidence).
--
-- == ITCZ band
--
-- 'itczBand' combines the convergence field with a latitude-weighted
-- envelope to produce the final ITCZ intensity at each tile.  Config
-- knobs in 'ConvergenceConfig' control sensitivity, latitude
-- weighting, and intensity scaling.
--
-- == Seasonal migration
--
-- 'seasonalITCZShift' computes the latitude offset of the thermal
-- equator as a function of axial tilt and orbital phase, used by the
-- weather module for tick-to-tick ITCZ migration.
module Topo.Climate.ITCZ
  ( -- * Configuration
    ConvergenceConfig(..)
  , defaultConvergenceConfig
    -- * Convergence field
  , convergenceField
    -- * ITCZ intensity band
  , itczBand
    -- * Seasonal dynamics
  , seasonalITCZShift
  ) where

import qualified Data.Vector.Unboxed as U
import Topo.Hex (HexDirection(..), allHexDirections, hexNeighborIndexInDirection)
import Topo.Grid.HexDirection (directionVector)
import Topo.Math (clamp01)

------------------------------------------------------------------------
-- Configuration
------------------------------------------------------------------------

-- | Configuration for the convergence-driven ITCZ model.
data ConvergenceConfig = ConvergenceConfig
  { convIntensityScale   :: !Float
  -- ^ Multiplier applied to raw convergence values when computing the
  -- ITCZ band intensity.  Higher values produce a stronger
  -- precipitation response to convergence.  Default: @1.0@.
  , convLatitudeWidth    :: !Float
  -- ^ Width (degrees latitude) of the Gaussian latitude envelope that
  -- suppresses spurious convergence far from the thermal equator.
  -- Default: @15.0@.
  , convCenterLat        :: !Float
  -- ^ Latitude (degrees) of the ITCZ centre.  For the climate stage
  -- this is typically 0 (equator); the weather module shifts it via
  -- 'seasonalITCZShift'.  Default: @0.0@.
  , convMinThreshold     :: !Float
  -- ^ Minimum convergence value to contribute to the ITCZ band.
  -- Tiles below this threshold are zeroed.  Default: @0.0@.
  , convStrengthTotal    :: !Float
  -- ^ Target total ITCZ moisture boost (same role as the former
  -- @moistITCZStrength@).  The band is normalised so its peak matches
  -- this value when divided by the number of transport iterations.
  -- Default: @0.15@.
  } deriving (Eq, Show)

-- | Sensible defaults for equatorial ITCZ modelling.
defaultConvergenceConfig :: ConvergenceConfig
defaultConvergenceConfig = ConvergenceConfig
  { convIntensityScale = 1.0
  , convLatitudeWidth  = 15.0
  , convCenterLat      = 0.0
  , convMinThreshold   = 0.0
  , convStrengthTotal  = 0.15
  }

------------------------------------------------------------------------
-- Convergence field
------------------------------------------------------------------------

-- | Compute the wind convergence at every tile on a @gridW × gridH@
-- hex grid.
--
-- Convergence is the negative divergence of the horizontal wind
-- vector field.  For each tile we project the wind vectors of its
-- neighbours onto the outward-pointing edge normals and sum; the
-- result is negated so that /positive/ values indicate converging
-- airflow (uplift) and /negative/ values indicate divergence
-- (subsidence).
--
-- Boundary tiles with fewer than 6 neighbours are normalised by the
-- actual neighbour count to avoid edge artefacts.
convergenceField
  :: Int               -- ^ Grid width
  -> Int               -- ^ Grid height
  -> U.Vector Float    -- ^ Wind direction (radians, 0 = east)
  -> U.Vector Float    -- ^ Wind speed (normalised 0–1)
  -> U.Vector Float    -- ^ Per-tile convergence (positive = converging)
convergenceField !gridW !gridH windDir windSpd = U.generate n $ \i ->
  let -- Wind vector at the centre tile
      !wd_i = windDir U.! i
      !ws_i = windSpd U.! i
      !wx_i = ws_i * cos wd_i
      !wy_i = ws_i * sin wd_i

      -- Accumulate flux across hex edges.  For each direction d with
      -- outward normal (nx, ny) and neighbour j, the net outward flux
      -- contribution is:
      --   flux_out += dot(wind_j, normal_d)
      -- where wind_j is the neighbour's wind vector.  We also include
      -- the centre tile's outward flux:
      --   flux_out += dot(wind_i, normal_d)
      -- Divergence ≈ total outward flux.  Convergence = −divergence.
      (!totalFlux, !count) = foldDirections gridW gridH i
                               windDir windSpd wx_i wy_i
  in if count == 0
       then 0
       else negate (totalFlux / fromIntegral count)
  where
    !n = gridW * gridH

-- | Fold over hex directions, accumulating net outward flux and a
-- neighbour count.
foldDirections
  :: Int -> Int -> Int
  -> U.Vector Float -> U.Vector Float
  -> Float -> Float
  -> (Float, Int)
foldDirections !gridW !gridH !i windDir windSpd !wx_i !wy_i =
  go allHexDirections 0 0
  where
    go [] !flux !cnt = (flux, cnt)
    go (d:ds) !flux !cnt =
      let (!nx, !ny) = directionVector d
          -- Centre tile's outward component along this edge
          !centerOut = wx_i * nx + wy_i * ny
      in case hexNeighborIndexInDirection gridW gridH d i of
           Nothing -> go ds flux cnt
           Just j  ->
             let !wd_j = windDir U.! j
                 !ws_j = windSpd U.! j
                 !wx_j = ws_j * cos wd_j
                 !wy_j = ws_j * sin wd_j
                 -- Neighbour's outward component along the same edge
                 !neighborOut = wx_j * nx + wy_j * ny
                 -- Average outward flux across this edge
                 !edgeFlux = (centerOut + neighborOut) * 0.5
             in go ds (flux + edgeFlux) (cnt + 1)
{-# INLINE foldDirections #-}

------------------------------------------------------------------------
-- ITCZ intensity band
------------------------------------------------------------------------

-- | Produce the ITCZ moisture-boost intensity at each tile.
--
-- Combines the convergence field with a latitude-proximity Gaussian
-- to yield a per-tile intensity in @[0, peakIntensity]@.  Tiles with
-- convergence below 'convMinThreshold' are zeroed; the latitude
-- envelope suppresses spurious convergence far from the thermal
-- equator.
--
-- The @latDegs@ vector must contain the latitude of each tile in
-- degrees (positive north).
itczBand
  :: ConvergenceConfig
  -> U.Vector Float    -- ^ Convergence field (from 'convergenceField')
  -> U.Vector Float    -- ^ Latitude of each tile (degrees)
  -> U.Vector Float    -- ^ ITCZ intensity per tile (≥ 0)
itczBand cfg conv latDegs = U.generate n $ \i ->
  let !c = conv U.! i
      !lat = latDegs U.! i
  in if c < convMinThreshold cfg
       then 0
       else
         let -- Latitude envelope: Gaussian centered on ITCZ centre
             !dLat   = lat - convCenterLat cfg
             !w      = max 0.001 (convLatitudeWidth cfg)
             !latEnv = exp (negate (dLat * dLat / (2 * w * w)))
             -- Convergence contribution (clamped to positive)
             !cPos   = max 0 c * convIntensityScale cfg
         in cPos * latEnv * convStrengthTotal cfg
  where
    !n = U.length conv

------------------------------------------------------------------------
-- Seasonal dynamics
------------------------------------------------------------------------

-- | Compute the seasonal latitude shift of the ITCZ centre.
--
-- The thermal equator migrates with the sub-solar point.  This
-- function returns the shifted latitude (degrees) given the base
-- latitude, migration scale, axial tilt (degrees), and orbital phase
-- (radians, 0 at northern spring equinox).
--
-- @
-- seasonalITCZShift baseLat migScale tilt phase
--   = baseLat + migScale × tilt × sin(phase)
-- @
seasonalITCZShift
  :: Float    -- ^ Base ITCZ latitude (degrees)
  -> Float    -- ^ Migration scale (0–1) — fraction of tilt applied
  -> Float    -- ^ Axial tilt (degrees)
  -> Float    -- ^ Orbital phase (radians)
  -> Float
seasonalITCZShift !baseLat !migScale !tilt !phase =
  baseLat + migScale * tilt * sin phase
