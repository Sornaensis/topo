{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Calendar and temporal model for topo worlds.
--
-- Time in topo is tracked by a monotonic tick counter ('WorldTime')
-- combined with a tick rate (seconds per tick).  Calendar dates
-- ('CalendarDate') are derived from the tick counter and the planet's
-- orbital parameters ('CalendarConfig').
--
-- 'CalendarConfig' is derived from 'PlanetConfig' via 'mkCalendarConfig',
-- using Earth-like defaults for rotation and orbital periods scaled by
-- planet radius.
--
-- 'PlanetAge' tracks geological-scale planet age (years since
-- formation), independent of the simulation tick clock.
module Topo.Calendar
  ( -- * World time
    WorldTime(..)
  , defaultWorldTime
  , advanceTicks
  , worldTimeElapsedSeconds
    -- * Calendar configuration
  , CalendarConfig(..)
  , mkCalendarConfig
  , defaultCalendarConfig
    -- * Calendar date
  , CalendarDate(..)
  , tickToDate
  , yearFraction
    -- * Planet age
  , PlanetAge(..)
  , defaultPlanetAge
  ) where

import GHC.Generics (Generic)
import Data.Word (Word64)
import Topo.Planet (PlanetConfig(..))

-- ---------------------------------------------------------------------------
-- WorldTime
-- ---------------------------------------------------------------------------

-- | A point in simulation time, tracked by a monotonic tick counter.
--
-- The tick counter never resets.  'wtTickRate' is fixed at world
-- creation and determines how many seconds elapse per tick.
data WorldTime = WorldTime
  { wtTick     :: !Word64
    -- ^ Monotonic tick counter (never resets).
  , wtTickRate :: !Double
    -- ^ Seconds per tick (from config, immutable after creation).
  } deriving (Eq, Show, Generic)

-- | Default world time: tick 0, 1 second per tick.
defaultWorldTime :: WorldTime
defaultWorldTime = WorldTime
  { wtTick     = 0
  , wtTickRate = 1.0
  }

-- | Advance the world time by a given number of ticks.
advanceTicks :: Word64 -> WorldTime -> WorldTime
advanceTicks dt wt = wt { wtTick = wtTick wt + dt }

-- | Total elapsed simulation seconds as a 'Double'.
worldTimeElapsedSeconds :: WorldTime -> Double
worldTimeElapsedSeconds wt =
  fromIntegral (wtTick wt) * wtTickRate wt

-- ---------------------------------------------------------------------------
-- CalendarConfig
-- ---------------------------------------------------------------------------

-- | Calendar constants derived from planet physics.
--
-- These are computed once at world creation from 'PlanetConfig' via
-- 'mkCalendarConfig'.  They translate between the tick counter and
-- human-readable calendar positions.
data CalendarConfig = CalendarConfig
  { ccSecondsPerDay :: !Double
    -- ^ Seconds in one planetary rotation.
    -- Derived from planet radius relative to Earth (same angular
    -- velocity assumption): @86400.0 * pcRadius / 6371.0@.
  , ccDaysPerYear   :: !Int
    -- ^ Integer days per orbital year.
    -- Derived as @round (ccSecondsPerYear / ccSecondsPerDay)@.
  , ccHoursPerDay   :: !Double
    -- ^ Hours per planetary day (default 24.0 for Earth-like).
  } deriving (Eq, Show, Generic)

-- | Earth-like default calendar configuration.
--
-- 86400 seconds/day, 365 days/year, 24 hours/day.
defaultCalendarConfig :: CalendarConfig
defaultCalendarConfig = CalendarConfig
  { ccSecondsPerDay = 86400.0
  , ccDaysPerYear   = 365
  , ccHoursPerDay   = 24.0
  }

-- | Earth orbital period in seconds (365.25 days).
earthOrbitalPeriodSeconds :: Double
earthOrbitalPeriodSeconds = 365.25 * 86400.0

-- | Earth radius in km (reference for scaling).
earthRadiusKm :: Double
earthRadiusKm = 6371.0

-- | Derive calendar constants from planet physics.
--
-- Assumptions:
--
-- * Rotation period scales linearly with radius (same angular velocity
--   as Earth).  A larger planet has longer days.
-- * Orbital period scales as @radius^(3/2)@ (Kepler's third law
--   approximation for similar stellar mass).
-- * Hours per day is fixed at 24.0 (user-friendly; the "hour" is a
--   display unit, not a physical constant).
mkCalendarConfig :: PlanetConfig -> CalendarConfig
mkCalendarConfig pc =
  let radiusRatio = realToFrac (pcRadius pc) / earthRadiusKm :: Double
      secondsPerDay  = 86400.0 * radiusRatio
      secondsPerYear = earthOrbitalPeriodSeconds * (radiusRatio ** 1.5)
      daysPerYear    = max 1 (round (secondsPerYear / secondsPerDay))
  in CalendarConfig
      { ccSecondsPerDay = secondsPerDay
      , ccDaysPerYear   = daysPerYear
      , ccHoursPerDay   = 24.0
      }

-- ---------------------------------------------------------------------------
-- CalendarDate
-- ---------------------------------------------------------------------------

-- | Calendar position derived from 'WorldTime' and 'CalendarConfig'.
--
-- This is a pure derivation — no state is stored.  Call 'tickToDate'
-- to convert a 'WorldTime' to a 'CalendarDate'.
data CalendarDate = CalendarDate
  { cdYear      :: !Int
    -- ^ Year since world creation (0-based).
  , cdDayOfYear :: !Int
    -- ^ Day within current orbital year (0-based, < 'ccDaysPerYear').
  , cdHourOfDay :: !Double
    -- ^ Hour within current rotation (0.0 <= h < 'ccHoursPerDay').
  } deriving (Eq, Show)

-- | Convert a world time to a calendar date.
--
-- @tickToDate cal wt@ computes the year, day-of-year, and hour-of-day
-- from the elapsed seconds implied by the tick counter and tick rate.
tickToDate :: CalendarConfig -> WorldTime -> CalendarDate
tickToDate cal wt =
  let elapsed     = worldTimeElapsedSeconds wt
      secPerDay   = ccSecondsPerDay cal
      totalDays   = floor (elapsed / secPerDay) :: Int
      dpy         = max 1 (ccDaysPerYear cal)
      (yr, doy)   = totalDays `divMod` dpy
      dayFraction = (elapsed - fromIntegral totalDays * secPerDay) / secPerDay
      hourOfDay   = dayFraction * ccHoursPerDay cal
  in CalendarDate
      { cdYear      = yr
      , cdDayOfYear = doy
      , cdHourOfDay = hourOfDay
      }

-- | Fractional year position in [0, 1) for seasonal calculations.
--
-- @yearFraction cal wt@ returns how far through the current orbital
-- year the world time is.  0.0 = start of year, 0.5 = mid-year,
-- approaching 1.0 = end of year.
yearFraction :: CalendarConfig -> WorldTime -> Double
yearFraction cal wt =
  let elapsed   = worldTimeElapsedSeconds wt
      secPerDay = ccSecondsPerDay cal
      totalDays = elapsed / secPerDay
      dpy       = fromIntegral (max 1 (ccDaysPerYear cal)) :: Double
      frac      = (totalDays / dpy) - fromIntegral (floor (totalDays / dpy) :: Int)
  in frac

-- ---------------------------------------------------------------------------
-- PlanetAge
-- ---------------------------------------------------------------------------

-- | Geological-scale planet age, separate from the simulation tick clock.
--
-- This records how old the planet is in years (e.g. 4.5 billion for
-- Earth).  It is metadata carried in the @.topo@ file, not derived
-- from the tick counter.
data PlanetAge = PlanetAge
  { paYears :: !Double
    -- ^ Planet age in years.
  } deriving (Eq, Show, Generic)

-- | Default planet age: 0 years (newly created world).
defaultPlanetAge :: PlanetAge
defaultPlanetAge = PlanetAge { paYears = 0.0 }
