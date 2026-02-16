{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Boundary-only conversion between normalised internal values and
-- real-world physical units.
--
-- The topo pipeline operates entirely in normalised @[0,1]@ (or @[-1,+1]@)
-- 'Float' space.  This module provides pure, lightweight conversion functions
-- that map normalised quantities to human-readable physical units at two
-- boundaries only: __config input__ and __display\/export output__.
--
-- No generator module imports this module.  Only consumers (topo-seer, export
-- tools, tests) should depend on it.
--
-- == Design
--
-- All conversions are thin affine transforms parameterised by a 'UnitScales'
-- record.  This record can be adjusted per-planet or per-scenario without
-- touching generator code.
--
-- @
--                  ┌──────────────────────────────────┐
--   Real-World     │         Topo.Units                │     Real-World
--   Config ───────►│  toNorm \/ fromNorm conversions   │────► Display \/ Export
--   (°C, m, mm\/yr)│                                   │     (°C, m, mm\/yr)
--                  └────────────┬──────────────────────┘
--                               │
--                     Internal pipeline
--                     (normalised 0–1 Float)
--                     UNCHANGED
-- @
module Topo.Units
  ( -- * Scale configuration
    UnitScales(..)
  , defaultUnitScales
    -- * Temperature
  , normToC
  , cToNorm
    -- * Elevation
  , normToMetres
  , metresToNorm
    -- * Precipitation
  , normToMmYear
  , mmYearToNorm
    -- * Humidity
  , normToRH
  , rhToNorm
    -- * Wind Speed
  , normToWindMs
  , windMsToNorm
    -- * Pressure
  , normToHPa
  , hPaToNorm
    -- * Soil Depth
  , normToSoilM
  , soilMToNorm
    -- * Slope
  , normSlopeToDeg
    -- * Generic affine helpers
  , normToLinear
  , linearToNorm
  ) where

import GHC.Generics (Generic)

-- ---------------------------------------------------------------------------
-- Scale configuration
-- ---------------------------------------------------------------------------

-- | Configurable scales for converting between normalised internal values
-- and real-world physical units.
--
-- Every conversion function in this module takes a 'UnitScales' so that
-- the mapping can be adjusted per-planet or per-scenario.  Use
-- 'defaultUnitScales' for Earth-like defaults.
data UnitScales = UnitScales
  { usTempScale     :: !Float
    -- ^ Temperature range in °C.  Default: @70.0@ (maps @[0,1]@ to @[−30,+40]@ °C).
  , usTempOffset    :: !Float
    -- ^ Temperature floor in °C.  Default: @−30.0@.
  , usElevRange     :: !Float
    -- ^ Full elevation span in metres (norm @0 → 1@).  Default: @12000.0@
    -- (±6 000 m around sea level).
  , usWaterLevel    :: !Float
    -- ^ Normalised water level.  Default: @0.5@.
  , usElevGradient  :: !Float
    -- ^ Elevation gradient for slope conversion:
    -- @elevRange / hexSpacingMetres@.  Default: @0.574@ (12 000 / 20 900).
  , usPrecipScale   :: !Float
    -- ^ Precipitation scale in mm\/year.  Default: @6000.0@.
  , usWindScale     :: !Float
    -- ^ Wind speed scale in m\/s.  Default: @50.0@.
  , usPressureMin   :: !Float
    -- ^ Minimum pressure in hPa.  Default: @870.0@ (Typhoon Tip, 1979).
  , usPressureRange :: !Float
    -- ^ Pressure range in hPa.  Default: @214.0@ (870 → 1084).
  , usSoilScale     :: !Float
    -- ^ Soil depth scale in metres.  Default: @5.0@.
  } deriving stock (Show, Eq, Generic)

-- | Earth-like default unit scales.
--
-- @
-- Temperature:   norm × 70 − 30        → [−30, +40] °C
-- Elevation:     (norm − 0.5) × 12000  → [−6000, +6000] m
-- Precipitation: norm × 6000           → [0, 6000] mm\/yr
-- Humidity:      norm × 100            → [0, 100] % RH
-- Wind Speed:    norm × 50             → [0, 50] m\/s
-- Pressure:      norm × 214 + 870      → [870, 1084] hPa
-- Soil Depth:    norm × 5              → [0, 5] m
-- @
defaultUnitScales :: UnitScales
defaultUnitScales = UnitScales
  { usTempScale     = 70.0
  , usTempOffset    = -30.0
  , usElevRange     = 12000.0
  , usWaterLevel    = 0.5
  , usElevGradient  = 0.574
  , usPrecipScale   = 6000.0
  , usWindScale     = 50.0
  , usPressureMin   = 870.0
  , usPressureRange = 214.0
  , usSoilScale     = 5.0
  }

-- ---------------------------------------------------------------------------
-- Temperature  (°C)
-- ---------------------------------------------------------------------------

-- | Convert a normalised temperature to degrees Celsius.
--
-- @normToC defaultUnitScales 0.0 ≈ −30@, @normToC defaultUnitScales 1.0 ≈ 40@.
normToC :: UnitScales -> Float -> Float
normToC s n = n * usTempScale s + usTempOffset s
{-# INLINE normToC #-}

-- | Convert degrees Celsius to normalised temperature.
cToNorm :: UnitScales -> Float -> Float
cToNorm s c = (c - usTempOffset s) / usTempScale s
{-# INLINE cToNorm #-}

-- ---------------------------------------------------------------------------
-- Elevation  (metres a.s.l.)
-- ---------------------------------------------------------------------------

-- | Convert a normalised elevation to metres above sea level.
--
-- Sea level is at 'usWaterLevel' (default @0.5@).
--
-- @normToMetres defaultUnitScales 0.5 ≈ 0@, @normToMetres defaultUnitScales 1.0 ≈ 6000@.
normToMetres :: UnitScales -> Float -> Float
normToMetres s n = (n - usWaterLevel s) * usElevRange s
{-# INLINE normToMetres #-}

-- | Convert metres above sea level to normalised elevation.
metresToNorm :: UnitScales -> Float -> Float
metresToNorm s m = m / usElevRange s + usWaterLevel s
{-# INLINE metresToNorm #-}

-- ---------------------------------------------------------------------------
-- Precipitation  (mm/year)
-- ---------------------------------------------------------------------------

-- | Convert normalised precipitation to millimetres per year.
--
-- @normToMmYear defaultUnitScales 0.8 ≈ 4800@.
normToMmYear :: UnitScales -> Float -> Float
normToMmYear s n = n * usPrecipScale s
{-# INLINE normToMmYear #-}

-- | Convert millimetres per year to normalised precipitation.
mmYearToNorm :: UnitScales -> Float -> Float
mmYearToNorm s mm = mm / usPrecipScale s
{-# INLINE mmYearToNorm #-}

-- ---------------------------------------------------------------------------
-- Humidity  (% RH)
-- ---------------------------------------------------------------------------

-- | Convert normalised humidity to relative humidity percentage.
--
-- @normToRH 0.65 ≈ 65@.
normToRH :: Float -> Float
normToRH n = n * 100.0
{-# INLINE normToRH #-}

-- | Convert relative humidity percentage to normalised humidity.
rhToNorm :: Float -> Float
rhToNorm rh = rh / 100.0
{-# INLINE rhToNorm #-}

-- ---------------------------------------------------------------------------
-- Wind Speed  (m/s)
-- ---------------------------------------------------------------------------

-- | Convert normalised wind speed to metres per second.
--
-- @normToWindMs defaultUnitScales 0.2 ≈ 10@.
normToWindMs :: UnitScales -> Float -> Float
normToWindMs s n = n * usWindScale s
{-# INLINE normToWindMs #-}

-- | Convert metres per second to normalised wind speed.
windMsToNorm :: UnitScales -> Float -> Float
windMsToNorm s ms = ms / usWindScale s
{-# INLINE windMsToNorm #-}

-- ---------------------------------------------------------------------------
-- Pressure  (hPa)
-- ---------------------------------------------------------------------------

-- | Convert normalised pressure to hectopascals.
--
-- @normToHPa defaultUnitScales 0.67 ≈ 1013@ (mean sea-level pressure).
normToHPa :: UnitScales -> Float -> Float
normToHPa s n = n * usPressureRange s + usPressureMin s
{-# INLINE normToHPa #-}

-- | Convert hectopascals to normalised pressure.
hPaToNorm :: UnitScales -> Float -> Float
hPaToNorm s hpa = (hpa - usPressureMin s) / usPressureRange s
{-# INLINE hPaToNorm #-}

-- ---------------------------------------------------------------------------
-- Soil Depth  (metres)
-- ---------------------------------------------------------------------------

-- | Convert normalised soil depth to metres.
--
-- @normToSoilM defaultUnitScales 1.0 ≈ 5@.
normToSoilM :: UnitScales -> Float -> Float
normToSoilM s n = n * usSoilScale s
{-# INLINE normToSoilM #-}

-- | Convert metres to normalised soil depth.
soilMToNorm :: UnitScales -> Float -> Float
soilMToNorm s m = m / usSoilScale s
{-# INLINE soilMToNorm #-}

-- ---------------------------------------------------------------------------
-- Slope  (degrees)
-- ---------------------------------------------------------------------------

-- | Convert a normalised slope magnitude to degrees.
--
-- Uses the real-world rise/run ratio: @atan(norm × elevGradient) × 180 / π@.
-- A normalised slope of @0.20@ with default scales → @≈ 6.5°@.
normSlopeToDeg :: UnitScales -> Float -> Float
normSlopeToDeg s n = atan (n * usElevGradient s) * (180.0 / pi)
{-# INLINE normSlopeToDeg #-}

-- ---------------------------------------------------------------------------
-- Generic affine helpers
-- ---------------------------------------------------------------------------

-- | Generic normalised-to-linear conversion: @n × scale + offset@.
normToLinear :: Float   -- ^ Scale
             -> Float   -- ^ Offset
             -> Float   -- ^ Normalised value
             -> Float
normToLinear scale offset n = n * scale + offset
{-# INLINE normToLinear #-}

-- | Generic linear-to-normalised conversion (inverse of 'normToLinear').
linearToNorm :: Float   -- ^ Scale (must be non-zero)
             -> Float   -- ^ Offset
             -> Float   -- ^ Real-world value
             -> Float
linearToNorm scale offset v = (v - offset) / scale
{-# INLINE linearToNorm #-}
