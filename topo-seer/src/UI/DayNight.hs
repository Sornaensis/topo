-- | Day/night brightness computation for terrain rendering.
module UI.DayNight
  ( DayNightFn
  , DayNightKey(..)
  , DayNightSpec
  , mkDayNightKey
  , mkDayNightSpec
  , mkDayNightFn
  , dayNightMinBrightness
  ) where

import Actor.Data (TerrainGeoContext(..), TerrainSnapshot(..))
import Topo.Calendar (CalendarConfig(..), CalendarDate(..), WorldTime, mkCalendarConfig, tickToDate, yearFraction)
import Topo.Hex (HexGridMeta(..))
import Topo.Planet (PlanetConfig(..), WorldSlice(..), tileLatLon)
import Topo.Solar (SolarPosition(..), tileSolarPos)
import Topo.Types (TileCoord(..), WorldConfig(..))

-- | Day/night brightness function for global tile coordinates @(q, r)@.
type DayNightFn = Int -> Int -> Float

-- | Stable identity for the authoritative world inputs used to build a
-- day/night brightness function.
data DayNightKey = DayNightKey
  { dnkWorldTime :: !WorldTime
  , dnkChunkSize :: !Int
  , dnkPlanetRadiusKm :: !Float
  , dnkAxialTiltDeg :: !Float
  , dnkHexSizeKm :: !Float
  , dnkSliceLatCenterDeg :: !Float
  , dnkSliceLonCenterDeg :: !Float
  } deriving (Eq, Show)

type DayNightSpec = (DayNightKey, DayNightFn)

data DayNightInputs = DayNightInputs
  { dniKey :: !DayNightKey
  , dniPlanet :: !PlanetConfig
  , dniSlice :: !WorldSlice
  , dniHex :: !HexGridMeta
  , dniConfig :: !WorldConfig
  , dniYearFraction :: !Float
  , dniHoursPerDay :: !Float
  , dniCalendarHour :: !Float
  , dniTiltDeg :: !Float
  }

-- | Build the freshness key for a day/night brightness function from the
-- terrain snapshot's authoritative world context.
--
-- Returns 'Nothing' when the chunk size is not yet available.
mkDayNightKey :: TerrainSnapshot -> Maybe DayNightKey
mkDayNightKey terrainSnap = dniKey <$> mkDayNightInputs terrainSnap

-- | Build a day/night freshness key and brightness function from the same
-- authoritative terrain snapshot context.
--
-- Returns @Just (key, f)@ where @f q r@ gives a brightness multiplier in
-- @[0.15, 1.0]@ for global tile coordinates @(q, r)@.  Returns 'Nothing'
-- when the chunk size is not yet available (no terrain data).
mkDayNightSpec :: TerrainSnapshot -> Maybe DayNightSpec
mkDayNightSpec terrainSnap = do
  inputs <- mkDayNightInputs terrainSnap
  pure (dniKey inputs, dayNightFnFromInputs inputs)

-- | Build a day/night brightness function from a terrain snapshot.
--
-- Returns @Just f@ where @f q r@ gives a brightness multiplier in @[0.15, 1.0]@
-- for global tile coordinates @(q, r)@.  Returns @Nothing@ when the chunk
-- size is not yet available (no terrain data).
mkDayNightFn :: TerrainSnapshot -> Maybe DayNightFn
mkDayNightFn terrainSnap = snd <$> mkDayNightSpec terrainSnap

mkDayNightInputs :: TerrainSnapshot -> Maybe DayNightInputs
mkDayNightInputs terrainSnap
  | chunkSize <= 0 = Nothing
  | otherwise = Just DayNightInputs
      { dniKey = DayNightKey
          { dnkWorldTime = worldTime
          , dnkChunkSize = chunkSize
          , dnkPlanetRadiusKm = pcRadius planet
          , dnkAxialTiltDeg = tiltDeg
          , dnkHexSizeKm = hexSizeKm hex
          , dnkSliceLatCenterDeg = wsLatCenter slice
          , dnkSliceLonCenterDeg = wsLonCenter slice
          }
      , dniPlanet = planet
      , dniSlice = slice
      , dniHex = hex
      , dniConfig = config
      , dniYearFraction = realToFrac (yearFraction calCfg worldTime)
      , dniHoursPerDay = realToFrac (ccHoursPerDay calCfg)
      , dniCalendarHour = realToFrac (cdHourOfDay calDate)
      , dniTiltDeg = tiltDeg
      }
  where
    chunkSize = tsChunkSize terrainSnap
    geo = tsGeoContext terrainSnap
    planet = tgcPlanet geo
    slice = tgcSlice geo
    hex = tgcHexGrid geo
    worldTime = tgcWorldTime geo
    tiltDeg = pcAxialTilt planet
    config = WorldConfig { wcChunkSize = chunkSize }
    calCfg = mkCalendarConfig planet
    calDate = tickToDate calCfg worldTime

dayNightFnFromInputs :: DayNightInputs -> DayNightFn
dayNightFnFromInputs inputs q r =
  let tile = TileCoord q r
      (latDeg, lonDeg) = tileLatLon (dniPlanet inputs) (dniHex inputs) (dniSlice inputs) (dniConfig inputs) tile
      latRad = latDeg * pi / 180
      sp = tileSolarPos (dniTiltDeg inputs) (dniYearFraction inputs) (dniHoursPerDay inputs) (dniCalendarHour inputs) latRad lonDeg
      altDeg = spAltitude sp * 180 / pi
  in solarBrightness altDeg

-- | Deep-night floor for day/night brightness.
dayNightMinBrightness :: Float
dayNightMinBrightness = 0.15

-- | Map solar altitude (degrees) to a brightness multiplier.
--
-- @>= 30@°: 1.0 (full daylight).
-- @0–30@°: linear ramp 0.4 → 1.0 (low sun / golden hour).
-- @-18–0@°: linear ramp 0.15 → 0.4 (twilight).
-- @< -18@°: 0.15 (deep night).
solarBrightness :: Float -> Float
solarBrightness altDeg
  | altDeg >= 30  = 1.0
  | altDeg >= 0   = 0.4 + 0.6 * (altDeg / 30)
  | altDeg >= -18 =
      dayNightMinBrightness + (0.4 - dayNightMinBrightness) * ((altDeg + 18) / 18)
  | otherwise     = dayNightMinBrightness
