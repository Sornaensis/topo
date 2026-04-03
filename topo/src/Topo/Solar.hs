{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Solar geometry for topo worlds.
--
-- Provides solar position (altitude, azimuth, zenith) as a function of
-- latitude, axial tilt, orbital position, and hour of day.  All internal
-- calculations use radians; the public API accepts the same units used
-- elsewhere in topo (degrees for config values, radians for latitudes,
-- fractional hours for time).
--
-- The model follows the standard astronomical formulae for a planet with
-- circular orbit and fixed axial tilt.
module Topo.Solar
  ( -- * Solar position
    SolarPosition(..)
  , solarPosition
  , tileSolarPos
    -- * Day/night info
  , DayInfo(..)
  , dayInfo
  , tileDayInfo
    -- * Irradiance
  , SolarConfig(..)
  , defaultSolarConfig
  , solarIrradiance
  , tileIrradiance
  , annualMeanInsolation
    -- * Low-level helpers
  , solarDeclination
  , hourAngle
  , localSolarHour
  ) where

import GHC.Generics (Generic)
import Topo.Config.JSON
  (ToJSON(..), FromJSON(..), configOptions, mergeDefaults,
   genericToJSON, genericParseJSON)

-- ---------------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------------

-- | Instantaneous solar position as seen from a surface point.
--
-- All angles are in radians.  Altitude is measured from the horizon
-- (negative means below horizon).  Azimuth is measured clockwise from
-- north.  Zenith is the complement of altitude (@π/2 − altitude@).
data SolarPosition = SolarPosition
  { spAltitude :: !Float
    -- ^ Solar altitude angle (radians, negative = below horizon)
  , spAzimuth  :: !Float
    -- ^ Solar azimuth angle (radians, 0 = north, clockwise)
  , spZenith   :: !Float
    -- ^ Solar zenith angle (radians, 0 = overhead)
  } deriving (Show, Eq, Generic)

-- | Summary of daylight for a given latitude and declination.
--
-- Hours are in the same unit as 'Topo.Calendar.CalendarConfig' hours
-- (i.e. a 24-hour scale by default).  Polar day/night are represented
-- by sunrise=0, sunset=hoursPerDay (or both 0).
data DayInfo = DayInfo
  { diSunriseHour :: !Float
    -- ^ Hour of sunrise (local solar time)
  , diSunsetHour  :: !Float
    -- ^ Hour of sunset (local solar time)
  , diDayLength   :: !Float
    -- ^ Length of daylight (hours)
  } deriving (Show, Eq, Generic)

-- ---------------------------------------------------------------------------
-- Solar declination
-- ---------------------------------------------------------------------------

-- | Solar declination from axial tilt and orbital fraction.
--
-- @solarDeclination axialTiltDeg yearFrac@ returns the declination in
-- radians.  @yearFrac@ is 0..1 where 0 = northern-hemisphere vernal
-- equinox.  The result ranges from @−tilt@ to @+tilt@.
solarDeclination :: Float  -- ^ Axial tilt (degrees)
                 -> Float  -- ^ Year fraction (0–1, from 'Topo.Calendar.yearFraction')
                 -> Float  -- ^ Declination (radians)
solarDeclination tiltDeg yearFrac =
  let tiltRad = tiltDeg * degToRad
      -- yearFrac 0 = vernal equinox; peak at 0.25  (summer solstice)
      -- sin(2π(f − 0.0)) gives 0 at equinox, 1 at 0.25 -- wait,
      -- yearFrac 0 is vernal equinox. Declination is max at summer
      -- solstice which is 0.25 of the way through the year.
      -- δ = tilt * sin(2π * yearFrac)  peaks at yearFrac = 0.25
  in  tiltRad * sin (2 * pi * yearFrac)
{-# INLINE solarDeclination #-}

-- ---------------------------------------------------------------------------
-- Hour angle
-- ---------------------------------------------------------------------------

-- | Hour angle from local solar hour (radians).
--
-- Solar noon corresponds to hour angle 0.  The result is in @[−π, π]@
-- where negative values are morning and positive values are afternoon.
hourAngle :: Float  -- ^ Local solar hour (0–hoursPerDay)
          -> Float  -- ^ Hours per day (e.g. 24)
          -> Float  -- ^ Hour angle (radians)
hourAngle localHour hpd =
  let noon = hpd / 2
  in  (localHour - noon) * (2 * pi / hpd)
{-# INLINE hourAngle #-}

-- ---------------------------------------------------------------------------
-- Local solar time
-- ---------------------------------------------------------------------------

-- | Compute local solar hour from calendar hour and longitude.
--
-- Convention: calendar hour 'cdHourOfDay' represents solar noon at
-- longitude 0 (or equivalently at the world-slice centre when
-- @lonCenter = 0@).  The returned hour wraps into @[0, hoursPerDay)@.
localSolarHour :: Float  -- ^ Hours per day
               -> Float  -- ^ Calendar hour of day (0–hoursPerDay)
               -> Float  -- ^ Tile longitude (degrees, −180..180)
               -> Float  -- ^ Local solar hour (0–hoursPerDay)
localSolarHour hpd calHour lonDeg =
  let offset = lonDeg / 360.0 * hpd
      raw    = calHour + offset
  in  raw - hpd * fromIntegral (floor (raw / hpd) :: Int)
{-# INLINE localSolarHour #-}

-- ---------------------------------------------------------------------------
-- Solar position
-- ---------------------------------------------------------------------------

-- | Full solar position for a surface point.
--
-- @solarPosition latRad declinationRad ha@ computes the sun's altitude,
-- azimuth, and zenith angle given:
--
--   * @latRad@ — observer latitude (radians, positive north)
--   * @declinationRad@ — solar declination (radians, from 'solarDeclination')
--   * @ha@ — hour angle (radians, from 'hourAngle')
solarPosition :: Float  -- ^ Latitude (radians)
              -> Float  -- ^ Solar declination (radians)
              -> Float  -- ^ Hour angle (radians)
              -> SolarPosition
solarPosition latRad dec ha =
  let sinLat = sin latRad
      cosLat = cos latRad
      sinDec = sin dec
      cosDec = cos dec
      cosHA  = cos ha

      -- Altitude  (sin α = sin φ sin δ + cos φ cos δ cos h)
      sinAlt = sinLat * sinDec + cosLat * cosDec * cosHA
      alt    = asin (clampF (-1) 1 sinAlt)

      -- Zenith
      zen = pi / 2 - alt

      -- Azimuth  (cos A = (sin δ − sin α sin φ) / (cos α cos φ))
      cosAlt = cos alt
      cosAz  = if cosAlt * cosLat > 1e-8
               then clampF (-1) 1 ((sinDec - sinAlt * sinLat) / (cosAlt * cosLat))
               else 0  -- degenerate at poles / horizon
      az0    = acos cosAz
      -- Azimuth is measured clockwise from north; flip when hour angle < 0 (morning)
      az     = if sin ha < 0 then 2 * pi - az0 else az0
  in SolarPosition
       { spAltitude = alt
       , spAzimuth  = az
       , spZenith   = zen
       }
{-# INLINE solarPosition #-}

-- ---------------------------------------------------------------------------
-- Day info
-- ---------------------------------------------------------------------------

-- | Compute sunrise, sunset, and day length for a given latitude and
-- solar declination.
--
-- Returns polar-day or polar-night 'DayInfo' when the sun never sets
-- or never rises.
dayInfo :: Float  -- ^ Latitude (radians)
        -> Float  -- ^ Solar declination (radians)
        -> Float  -- ^ Hours per day
        -> DayInfo
dayInfo latRad dec hpd =
  let -- cos(hour-angle at sunrise) = −tan(φ) tan(δ)
      cosHASunrise = -(tan latRad * tan dec)
  in
    if cosHASunrise <= -1
      then -- Polar day: sun never sets
        DayInfo { diSunriseHour = 0, diSunsetHour = hpd, diDayLength = hpd }
    else if cosHASunrise >= 1
      then -- Polar night: sun never rises
        DayInfo { diSunriseHour = 0, diSunsetHour = 0, diDayLength = 0 }
    else
      let haSunrise  = acos cosHASunrise          -- radians, positive
          halfDayH   = haSunrise / (2 * pi) * hpd -- hours of half-day
          noon       = hpd / 2
          sunrise    = noon - halfDayH
          sunset     = noon + halfDayH
      in DayInfo
           { diSunriseHour = sunrise
           , diSunsetHour  = sunset
           , diDayLength   = sunset - sunrise
           }
{-# INLINE dayInfo #-}

-- ---------------------------------------------------------------------------
-- Irradiance
-- ---------------------------------------------------------------------------

-- | Configuration for the atmospheric irradiance model.
--
-- These parameters control how much of the top-of-atmosphere solar flux
-- reaches the surface through atmospheric attenuation and scattering.
data SolarConfig = SolarConfig
  { scAtmosphericK    :: !Float
    -- ^ Optical depth parameter for Beer–Lambert attenuation.
    -- Higher values mean a thicker/hazier atmosphere.  Earth-like: 0.2.
  , scDiffuseFraction :: !Float
    -- ^ Base diffuse irradiance fraction (scattered skylight).
    -- Added to direct beam when the sun is above the horizon.  0.12 typical.
  } deriving (Show, Eq, Generic)

instance ToJSON SolarConfig where
  toJSON = genericToJSON (configOptions "sc")

instance FromJSON SolarConfig where
  parseJSON v = genericParseJSON (configOptions "sc")
                  (mergeDefaults (toJSON defaultSolarConfig) v)

-- | Earth-like defaults for the irradiance model.
defaultSolarConfig :: SolarConfig
defaultSolarConfig = SolarConfig
  { scAtmosphericK    = 0.2
  , scDiffuseFraction = 0.12
  }

-- | Surface irradiance as a fraction of top-of-atmosphere insolation.
--
-- Uses Beer–Lambert atmospheric attenuation for direct beam, plus a
-- constant diffuse (scattered) component when the sun is above the
-- horizon.  Returns 0 at night.
--
-- @solarIrradiance atmosphericK diffuseFrac zenith@
solarIrradiance :: Float  -- ^ Atmospheric optical depth /k/
                -> Float  -- ^ Diffuse fraction
                -> Float  -- ^ Zenith angle (radians, from 'SolarPosition')
                -> Float  -- ^ Irradiance fraction [0, 1]
solarIrradiance k diffuseFrac zenith
  | zenith >= pi / 2 = 0   -- sun below horizon
  | otherwise =
      let cosZ          = cos zenith
          -- Air mass ≈ 1/cos(z), clamped to avoid singularity at horizon
          airMass       = min 38.0 (1.0 / max 0.01 cosZ)
          transmittance = exp (negate k * airMass)
          direct        = cosZ * transmittance
          diffuse       = diffuseFrac
      in  clampF 0 1 (direct + diffuse)
{-# INLINE solarIrradiance #-}

-- | Per-tile irradiance combining solar geometry and atmospheric model.
--
-- Computes the sun position for the tile and then applies the
-- Beer–Lambert + diffuse irradiance model.  The result is in [0, 1]
-- relative to top-of-atmosphere insolation and does /not/ include the
-- 'pcInsolation' multiplier — multiply the result by 'pcInsolation'
-- (or 'lmInsolation') at the call site to get absolute irradiance.
tileIrradiance :: SolarConfig
               -> Float  -- ^ Axial tilt (degrees)
               -> Float  -- ^ Year fraction (0–1)
               -> Float  -- ^ Hours per day
               -> Float  -- ^ Calendar hour of day
               -> Float  -- ^ Tile latitude (radians)
               -> Float  -- ^ Tile longitude (degrees)
               -> Float  -- ^ Irradiance fraction [0, 1]
tileIrradiance cfg tiltDeg yearFrac hpd calHour latRad lonDeg =
  let sp = tileSolarPos tiltDeg yearFrac hpd calHour latRad lonDeg
  in  solarIrradiance (scAtmosphericK cfg) (scDiffuseFraction cfg) (spZenith sp)
{-# INLINE tileIrradiance #-}

-- | Annual-mean surface insolation at a given latitude.
--
-- Numerically integrates irradiance over 12 months × 8 hour samples
-- to produce a latitude-dependent annual average fraction of
-- top-of-atmosphere flux.  The result is in [0, 1] and can replace
-- the constant 'pcInsolation' scalar in climate models.
--
-- This is meant for climate initialisation, not per-tick computation.
annualMeanInsolation
  :: SolarConfig
  -> Float  -- ^ Axial tilt (degrees)
  -> Float  -- ^ Hours per day
  -> Float  -- ^ Latitude (radians)
  -> Float  -- ^ Annual-mean irradiance fraction [0, 1]
annualMeanInsolation cfg tiltDeg hpd latRad =
  let k    = scAtmosphericK cfg
      diff = scDiffuseFraction cfg
      nMonths = 12 :: Int
      nHours  = 8  :: Int
      -- Sum irradiance over monthly samples
      monthlyTotal = go 0 0.0
        where
          go m !acc
            | m >= nMonths = acc
            | otherwise =
                let yf  = fromIntegral m / fromIntegral nMonths
                    dec = solarDeclination tiltDeg yf
                    di  = dayInfo latRad dec hpd
                    dl  = diDayLength di
                    dayAvg
                      | dl <= 0   = 0
                      | otherwise =
                          let sr  = diSunriseHour di
                              step = dl / fromIntegral nHours
                              -- Sample irradiance at nHours points during daylight
                              hourSum = goH 0 0.0
                                where
                                  goH h !hAcc
                                    | h >= nHours = hAcc
                                    | otherwise =
                                        let hr  = sr + (fromIntegral h + 0.5) * step
                                            ha  = hourAngle hr hpd
                                            sp  = solarPosition latRad dec ha
                                            irr = solarIrradiance k diff (spZenith sp)
                                        in goH (h + 1) (hAcc + irr)
                          in hourSum / fromIntegral nHours * (dl / hpd)
                in go (m + 1) (acc + dayAvg)
  in clampF 0 1 (monthlyTotal / fromIntegral nMonths)

-- ---------------------------------------------------------------------------
-- Tile-level convenience
-- ---------------------------------------------------------------------------

-- | Compute the full solar position for a tile in one call.
--
-- Combines 'solarDeclination', 'localSolarHour', 'hourAngle', and
-- 'solarPosition'.
--
-- __Convention:__ @calendarHour@ ('cdHourOfDay') is the hour at the
-- reference meridian (longitude 0).  Tile longitude offsets the local
-- solar time so that solar noon occurs when the sun is highest at
-- each tile.
tileSolarPos :: Float  -- ^ Axial tilt (degrees, 'pcAxialTilt')
             -> Float  -- ^ Year fraction (0–1, from 'yearFraction')
             -> Float  -- ^ Hours per day ('ccHoursPerDay')
             -> Float  -- ^ Calendar hour of day ('cdHourOfDay')
             -> Float  -- ^ Tile latitude (radians)
             -> Float  -- ^ Tile longitude (degrees)
             -> SolarPosition
tileSolarPos tiltDeg yearFrac hpd calHour latRad lonDeg =
  let dec  = solarDeclination tiltDeg yearFrac
      lsh  = localSolarHour hpd calHour lonDeg
      ha   = hourAngle lsh hpd
  in  solarPosition latRad dec ha
{-# INLINE tileSolarPos #-}

-- | Day information for a tile on a given day.
tileDayInfo :: Float  -- ^ Axial tilt (degrees)
            -> Float  -- ^ Year fraction (0–1)
            -> Float  -- ^ Hours per day
            -> Float  -- ^ Tile latitude (radians)
            -> DayInfo
tileDayInfo tiltDeg yearFrac hpd latRad =
  let dec = solarDeclination tiltDeg yearFrac
  in  dayInfo latRad dec hpd
{-# INLINE tileDayInfo #-}

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

degToRad :: Float
degToRad = pi / 180.0
{-# INLINE degToRad #-}

clampF :: Float -> Float -> Float -> Float
clampF lo hi x
  | x < lo    = lo
  | x > hi    = hi
  | otherwise  = x
{-# INLINE clampF #-}
