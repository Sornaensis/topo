-- | Day/night brightness computation for terrain rendering.
module UI.DayNight
  ( DayNightFn
  , DayNightKey(..)
  , DayNightSpec
  , mkDayNightKey
  , mkDayNightSpec
  , mkDayNightFn
  ) where

import Actor.UI (UiState(..), uiWorldTime)
import Seer.Config (mapRange)
import Seer.Config.SliderConversion (sliderToDomainFloat)
import Seer.Config.SliderSpec (SliderId(..))
import Topo.Calendar (CalendarConfig(..), CalendarDate(..), WorldTime, mkCalendarConfig, tickToDate, yearFraction)
import Topo.Hex (HexGridMeta(..))
import Topo.Planet (PlanetConfig(..), WorldSlice(..), tileLatitude, tileLongitude)
import Topo.Solar (SolarPosition(..), tileSolarPos)
import Topo.Types (TileCoord(..), WorldConfig(..))

-- | Day/night brightness function for global tile coordinates @(q, r)@.
type DayNightFn = Int -> Int -> Float

-- | Stable identity for all UI and world inputs currently used to build a
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

-- | Build the freshness key for a day/night brightness function from UI state.
--
-- Returns 'Nothing' when the chunk size is not yet available.
mkDayNightKey :: UiState -> Int -> Maybe DayNightKey
mkDayNightKey ui chunkSize = dniKey <$> mkDayNightInputs ui chunkSize

-- | Build a day/night freshness key and brightness function from the same
-- derived input path.
--
-- Returns @Just (key, f)@ where @f q r@ gives a brightness multiplier in
-- @[0.15, 1.0]@ for global tile coordinates @(q, r)@.  Returns 'Nothing'
-- when the chunk size is not yet available (no terrain data).
mkDayNightSpec :: UiState -> Int -> Maybe DayNightSpec
mkDayNightSpec ui chunkSize = do
  inputs <- mkDayNightInputs ui chunkSize
  pure (dniKey inputs, dayNightFnFromInputs inputs)

-- | Build a day/night brightness function from UI state.
--
-- Returns @Just f@ where @f q r@ gives a brightness multiplier in @[0.15, 1.0]@
-- for global tile coordinates @(q, r)@.  Returns @Nothing@ when the chunk
-- size is not yet available (no terrain data).
mkDayNightFn :: UiState -> Int -> Maybe DayNightFn
mkDayNightFn ui chunkSize = snd <$> mkDayNightSpec ui chunkSize

mkDayNightInputs :: UiState -> Int -> Maybe DayNightInputs
mkDayNightInputs ui chunkSize
  | chunkSize <= 0 = Nothing
  | otherwise = Just DayNightInputs
      { dniKey = DayNightKey
          { dnkWorldTime = worldTime
          , dnkChunkSize = chunkSize
          , dnkPlanetRadiusKm = radiusKm
          , dnkAxialTiltDeg = tiltDeg
          , dnkHexSizeKm = hexSizeKmValue
          , dnkSliceLatCenterDeg = latCenterDeg
          , dnkSliceLonCenterDeg = lonCenterDeg
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
    radiusKm = mapRange 4778 9557 (uiPlanetRadius ui)
    tiltDeg = mapRange 0 45 (uiAxialTilt ui)
    hexSizeKmValue = sliderToDomainFloat SliderHexSizeKm (uiHexSizeKm ui)
    latCenterDeg = mapRange (-90) 90 (uiSliceLatCenter ui)
    lonCenterDeg = mapRange (-180) 180 (uiSliceLonCenter ui)
    planet = PlanetConfig
      { pcRadius = radiusKm
      , pcAxialTilt = tiltDeg
      , pcInsolation = mapRange 0.7 1.3 (uiInsolation ui)
      }
    slice = WorldSlice
      { wsLatCenter = latCenterDeg
      , wsLatExtent = 0
      , wsLonCenter = lonCenterDeg
      , wsLonExtent = 0
      }
    hex = HexGridMeta { hexSizeKm = hexSizeKmValue }
    config = WorldConfig { wcChunkSize = chunkSize }
    calCfg = mkCalendarConfig planet
    worldTime = uiWorldTime ui
    calDate = tickToDate calCfg worldTime

dayNightFnFromInputs :: DayNightInputs -> DayNightFn
dayNightFnFromInputs inputs q r =
  let tile = TileCoord q r
      latDeg = tileLatitude (dniPlanet inputs) (dniHex inputs) (dniSlice inputs) (dniConfig inputs) tile
      latRad = latDeg * pi / 180
      lonDeg = tileLongitude (dniPlanet inputs) (dniHex inputs) (dniSlice inputs) (dniConfig inputs) tile
      sp = tileSolarPos (dniTiltDeg inputs) (dniYearFraction inputs) (dniHoursPerDay inputs) (dniCalendarHour inputs) latRad lonDeg
      altDeg = spAltitude sp * 180 / pi
  in solarBrightness altDeg

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
  | altDeg >= -18 = 0.15 + 0.25 * ((altDeg + 18) / 18)
  | otherwise     = 0.15
