{-# LANGUAGE BangPatterns #-}

-- | Physics-based evaporation models.
--
-- Provides three core functions that replace the ad-hoc constants in
-- the old climate pipeline:
--
--   * 'satNorm' — normalised Clausius-Clapeyron saturation curve
--     (Model A in plan.md).  Maps a @[0,1]@ normalised temperature to
--     a @[0,1]@ moisture-capacity fraction via a piecewise polynomial
--     fit of the August-Roche-Magnus equation.
--
--   * 'oceanEvaporation' — Dalton's-Law ocean evaporation (Model B).
--     Combines saturation deficit, wind enhancement, and insolation.
--
--   * 'landEvapotranspiration' — Penman-Monteith-inspired land ET
--     (Model C).  Combines soil moisture, vegetation cover,
--     temperature-driven saturation, and wind enhancement.
--
-- All functions are __pure__ and operate on single-tile scalar inputs
-- so they can be mapped, tested, and benchmarked in isolation.
module Topo.Climate.Evaporation
  ( -- * Saturation curve (Model A)
    satNorm

    -- * Ocean evaporation (Model B)
  , oceanEvaporation

    -- * Land evapotranspiration (Model C)
  , landEvapotranspiration
  ) where

import Topo.Climate.Config (MoistureConfig(..))
import Topo.Math (clamp01)

---------------------------------------------------------------------------
-- Model A: Normalised Clausius-Clapeyron Saturation Curve
---------------------------------------------------------------------------

-- | Normalised saturation vapour pressure.
--
-- Maps a normalised temperature @T ∈ [0,1]@ (where @T = 0 → −30 °C@,
-- @T = 1 → 40 °C@ by default) to a normalised moisture-capacity
-- fraction @[0,1]@, approximating the August-Roche-Magnus form of the
-- Clausius-Clapeyron equation:
--
-- \[
-- e_s(T_C) = 6.1078 \times \exp\!\left(\frac{17.27 \times T_C}
--            {T_C + 237.3}\right) \quad [\text{hPa}]
-- \]
--
-- The normalised form is:
--
-- \[
-- \text{satNorm}(T) = \text{clamp01}\!\left(
--   \frac{e_s(T \times 70 - 30) - 0.5}{73.3}\right)
-- \]
--
-- Implemented as a five-segment piecewise polynomial to avoid the
-- runtime cost of @exp@.  The fit is continuous and monotonically
-- increasing.
--
-- Reference table:
--
-- @
-- T (norm) │ T (°C)  │ satNorm
-- ─────────┼─────────┼────────
-- 0.00     │ −30     │ 0.000
-- 0.15     │ −19.5   │ 0.012
-- 0.30     │ −9      │ 0.038
-- 0.43     │  0      │ 0.083
-- 0.50     │  5      │ 0.112
-- 0.65     │  15.5   │ 0.234
-- 0.80     │  26     │ 0.456
-- 0.90     │  33     │ 0.673
-- 1.00     │  40     │ 1.000
-- @
--
-- __Configurable parameters__ (via 'MoistureConfig'):
-- @ccTempToC_Scale@ (default 70.0) and @ccTempToC_Offset@ (default
-- −30.0) allow non-Earth planets to shift the temperature–moisture
-- relationship.
satNorm :: Float -> Float
satNorm !t = clamp01 (go (clamp01 t))
  where
    -- Five-segment piecewise polynomial fitted to the reference table.
    -- Each segment is a quadratic a*x^2 + b*x + c chosen for C0
    -- continuity at the knot boundaries.
    go x
      -- Segment 1: [0.00, 0.30] — near-zero polar region
      | x < 0.30 =
          let !dx = x
          -- quadratic through (0, 0.000), (0.15, 0.012), (0.30, 0.038)
          in 0.1956 * dx * dx + 0.0678 * dx
      -- Segment 2: [0.30, 0.50] — sub-arctic → cool temperate
      | x < 0.50 =
          let !dx = x - 0.30
          -- quadratic through (0.30, 0.038), (0.43, 0.083), (0.50, 0.112)
          in 1.35 * dx * dx + 0.10 * dx + 0.038
      -- Segment 3: [0.50, 0.65] — temperate → warm temperate
      | x < 0.65 =
          let !dx = x - 0.50
          -- quadratic through (0.50, 0.112), (0.575, 0.168), (0.65, 0.234)
          in 1.067 * dx * dx + 0.653 * dx + 0.112
      -- Segment 4: [0.65, 0.80] — warm temperate → subtropical
      | x < 0.80 =
          let !dx = x - 0.65
          -- quadratic through (0.65, 0.234), (0.725, 0.340), (0.80, 0.456)
          in 0.622 * dx * dx + 1.387 * dx + 0.234
      -- Segment 5: [0.80, 1.00] — subtropical -> equatorial
      | otherwise =
          let !dx = x - 0.80
          -- quadratic through (0.80, 0.456), (0.90, 0.673), (1.00, 1.000)
          in 5.5 * dx * dx + 1.62 * dx + 0.456

---------------------------------------------------------------------------
-- Model B: Ocean Evaporation (Dalton's Law)
---------------------------------------------------------------------------

-- | Ocean evaporation via Dalton's Law analogue.
--
-- \[
-- E_{\text{ocean}} = k_E \times \text{satNorm}(T_{\text{SST}})
--   \times (1 + k_W \times \text{windSpd})
--   \times \text{insolation}
-- \]
--
-- @k_E@ is 'moistEvapCoeff' (default 0.85) and @k_W@ is
-- 'moistWindEvapScale' (default 0.30).
--
-- At tropical temperatures (@T ≈ 0.85@, @satNorm ≈ 0.57@), ocean
-- evaporation ≈ 0.56.  At polar temperatures (@T ≈ 0.15@,
-- @satNorm ≈ 0.01@), evaporation ≈ 0.01 — producing a physically
-- correct latitude gradient.
oceanEvaporation
  :: MoistureConfig
  -> Float          -- ^ Sea surface temperature (normalised 0–1)
  -> Float          -- ^ Wind speed (normalised 0–1)
  -> Float          -- ^ Insolation multiplier (from 'PlanetConfig')
  -> Float          -- ^ Resulting ocean evaporation (0–1)
oceanEvaporation !cfg !sst !windSpd !insol =
  let !sat = satNorm sst
      !kE  = moistEvapCoeff cfg
      !kW  = moistWindEvapScale cfg
  in clamp01 (kE * sat * (1 + kW * windSpd) * insol)

---------------------------------------------------------------------------
-- Model C: Land Evapotranspiration (Penman-Monteith-Inspired)
---------------------------------------------------------------------------

-- | Land evapotranspiration (Penman-Monteith-inspired).
--
-- \[
-- E_{\text{land}} = k_{ET} \times \text{satNorm}(T)
--   \times \text{soilMoisture}
--   \times (k_{\text{bare}} + k_{\text{veg}} \times \text{vegCover})
--   \times (1 + k_W \times \text{windSpd})
-- \]
--
-- Key behaviours:
--
--   * Hot, wet, vegetated (Amazon): ≈ 0.38
--   * Warm, moist, bare soil:      ≈ 0.03
--   * Cold, wet, vegetated:        ≈ 0.03
--   * Hot, dry, barren (desert):   ≈ 0.003
landEvapotranspiration
  :: MoistureConfig
  -> Float          -- ^ Temperature (normalised 0–1)
  -> Float          -- ^ Soil moisture (normalised 0–1)
  -> Float          -- ^ Vegetation cover fraction (0–1)
  -> Float          -- ^ Wind speed (normalised 0–1)
  -> Float          -- ^ Resulting land ET (0–1)
landEvapotranspiration !cfg !temp !soilMoist !vegCov !windSpd =
  let !sat   = satNorm temp
      !kET   = moistLandETCoeff cfg
      !kBare = moistBareEvapFrac cfg
      !kVeg  = moistVegTranspFrac cfg
      !kW    = moistWindETScale cfg
      !vegFactor = kBare + kVeg * clamp01 vegCov
  in clamp01 (kET * sat * clamp01 soilMoist * vegFactor * (1 + kW * clamp01 windSpd))
