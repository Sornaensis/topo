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
  , satNormCfg
  , magnusES

    -- * Ocean evaporation (Model B)
  , oceanEvaporation

    -- * Land evapotranspiration (Model C)
  , landEvapotranspiration
  ) where

import Topo.Climate.Config (MoistureConfig(..), defaultMoistureConfig)
import Topo.Math (clamp01)

---------------------------------------------------------------------------
-- Model A: Normalised Clausius-Clapeyron Saturation Curve
---------------------------------------------------------------------------

-- | Normalised saturation vapour pressure (backward-compatible alias).
--
-- Equivalent to @satNormCfg defaultMoistureConfig@, using the default
-- temperature–Celsius mapping (@scale = 70@, @offset = −30@).
--
-- See 'satNormCfg' for the full configurable implementation.
satNorm :: Float -> Float
satNorm = satNormCfg defaultMoistureConfig
{-# INLINE satNorm #-}

-- | August-Roche-Magnus saturation vapour pressure (hPa).
--
-- \[
-- e_s(T_C) = 6.1078 \times \exp\!\left(\frac{17.27 \times T_C}
--            {T_C + 237.3}\right) \quad [\text{hPa}]
-- \]
--
-- This is the physical Magnus equation—expensive (@exp@) but exact.
-- Used inside 'satNormCfg' where the piecewise polynomial is replaced
-- by the real equation so that 'ccTempToC_Scale' and 'ccTempToC_Offset'
-- take effect.
magnusES :: Float -> Float
magnusES !tc = 6.1078 * exp (17.27 * tc / (tc + 237.3))
{-# INLINE magnusES #-}

-- | Configurable normalised saturation vapour pressure.
--
-- Maps a normalised temperature @T ∈ [0,1]@ to a normalised
-- moisture-capacity fraction @[0,1]@ using the August-Roche-Magnus
-- equation parameterised by 'MoistureConfig':
--
-- \[
-- T_C = T \times \text{ccTempToC\_Scale} + \text{ccTempToC\_Offset}
-- \]
-- \[
-- \text{satNormCfg}(T) = \text{clamp01}\!\left(
--   \frac{e_s(T_C) - e_s(T_{C,\min})}{e_s(T_{C,\max}) - e_s(T_{C,\min})}
-- \right)
-- \]
--
-- With Earth defaults (@scale = 70@, @offset = −30@) the result
-- closely matches the original piecewise-polynomial 'satNorm' to
-- within ±0.002.
--
-- Changing @ccTempToC_Scale@ or @ccTempToC_Offset@ materially shifts
-- the saturation curve—useful for non-Earth planets or ice-age
-- scenarios where the liveable temperature band differs.
satNormCfg :: MoistureConfig -> Float -> Float
satNormCfg !cfg !t =
  let !scale  = ccTempToC_Scale cfg
      !offset = ccTempToC_Offset cfg
      !tc     = clamp01 t * scale + offset
      !esFloor   = magnusES offset
      !esCeiling = magnusES (scale + offset)
      !range     = esCeiling - esFloor
  in if range <= 0
       then 0
       else clamp01 ((magnusES tc - esFloor) / range)

---------------------------------------------------------------------------
-- Model B: Ocean Evaporation (Dalton's Law)
---------------------------------------------------------------------------

-- | Ocean evaporation via Dalton's Law analogue.
--
-- \[
-- E_{\text{ocean}} = \text{satNorm}(T_{\text{SST}})
--   \times (1 + k_W \times \text{windSpd})
--   \times \text{insolation}
-- \]
--
-- @k_W@ is 'moistWindEvapScale' (default 0.30).  The saturation
-- curve provides the full physics-derived temperature dependence;
-- no additional flat scaling coefficient is applied.
--
-- At tropical temperatures (@T ≈ 0.85@, @satNorm ≈ 0.57@), ocean
-- evaporation ≈ 0.66.  At polar temperatures (@T ≈ 0.15@,
-- @satNorm ≈ 0.01@), evaporation ≈ 0.01 — producing a physically
-- correct latitude gradient.
oceanEvaporation
  :: MoistureConfig
  -> Float          -- ^ Sea surface temperature (normalised 0–1)
  -> Float          -- ^ Wind speed (normalised 0–1)
  -> Float          -- ^ Insolation multiplier (from 'PlanetConfig')
  -> Float          -- ^ Resulting ocean evaporation (0–1)
oceanEvaporation !cfg !sst !windSpd !insol =
  let !sat = satNormCfg cfg sst
      !kW  = moistWindEvapScale cfg
  in clamp01 (sat * (1 + kW * windSpd) * insol)

---------------------------------------------------------------------------
-- Model C: Land Evapotranspiration (Penman-Monteith-Inspired)
---------------------------------------------------------------------------

-- | Land evapotranspiration (Penman-Monteith-inspired).
--
-- \[
-- E_{\text{land}} = \text{satNorm}(T)
--   \times \text{soilMoisture}
--   \times (k_{\text{bare}} + k_{\text{veg}} \times \text{vegCover})
--   \times (1 + k_W \times \text{windSpd})
-- \]
--
-- The saturation curve provides the temperature dependence.  No
-- additional flat scaling coefficient is applied — the decomposed
-- sub-terms (bare-soil fraction, vegetation transpiration fraction,
-- wind enhancement) already capture the physical ET drivers.
--
-- Key behaviours:
--
--   * Hot, wet, vegetated (Amazon): ≈ 0.58
--   * Warm, moist, bare soil:      ≈ 0.05
--   * Cold, wet, vegetated:        ≈ 0.05
--   * Hot, dry, barren (desert):   ≈ 0.005
landEvapotranspiration
  :: MoistureConfig
  -> Float          -- ^ Temperature (normalised 0–1)
  -> Float          -- ^ Soil moisture (normalised 0–1)
  -> Float          -- ^ Vegetation cover fraction (0–1)
  -> Float          -- ^ Wind speed (normalised 0–1)
  -> Float          -- ^ Resulting land ET (0–1)
landEvapotranspiration !cfg !temp !soilMoist !vegCov !windSpd =
  let !sat   = satNormCfg cfg temp
      !kBare = moistBareEvapFrac cfg
      !kVeg  = moistVegTranspFrac cfg
      !kW    = moistWindETScale cfg
      !vegFactor = kBare + kVeg * clamp01 vegCov
  in clamp01 (sat * clamp01 soilMoist * vegFactor * (1 + kW * clamp01 windSpd))
