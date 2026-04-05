-- | Day/night brightness computation for terrain rendering.
module UI.DayNight
  ( mkDayNightFn
  ) where

import Actor.UI (UiState(..))
import Seer.Config (mapRange)
import Seer.Config.SliderConversion (sliderToDomainFloat)
import Seer.Config.SliderSpec (SliderId(..))
import Topo.Calendar (CalendarConfig(..), CalendarDate(..), WorldTime(..), mkCalendarConfig, tickToDate, yearFraction)
import Topo.Hex (HexGridMeta(..))
import Topo.Planet (PlanetConfig(..), WorldSlice(..), tileLatitude, tileLongitude)
import Topo.Solar (SolarPosition(..), tileSolarPos)
import Topo.Types (TileCoord(..), WorldConfig(..))

-- | Build a day/night brightness function from UI state.
--
-- Returns @Just f@ where @f q r@ gives a brightness multiplier in @[0.15, 1.0]@
-- for global tile coordinates @(q, r)@.  Returns @Nothing@ when the chunk
-- size is not yet available (no terrain data).
mkDayNightFn :: UiState -> Int -> Maybe (Int -> Int -> Float)
mkDayNightFn ui chunkSize
  | chunkSize <= 0 = Nothing
  | otherwise = Just $ \q r ->
      let tile = TileCoord q r
          latDeg = tileLatitude planet hex slice config tile
          latRad = latDeg * pi / 180
          lonDeg = tileLongitude planet hex slice config tile
          sp = tileSolarPos tiltDeg yf hpd calHour latRad lonDeg
          altDeg = spAltitude sp * 180 / pi
      in solarBrightness altDeg
  where
    planet = PlanetConfig
      { pcRadius = mapRange 4778 9557 (uiPlanetRadius ui)
      , pcAxialTilt = mapRange 0 45 (uiAxialTilt ui)
      , pcInsolation = mapRange 0.7 1.3 (uiInsolation ui)
      }
    slice = WorldSlice
      { wsLatCenter = mapRange (-90) 90 (uiSliceLatCenter ui)
      , wsLatExtent = 0
      , wsLonCenter = mapRange (-180) 180 (uiSliceLonCenter ui)
      , wsLonExtent = 0
      }
    hex = HexGridMeta { hexSizeKm = sliderToDomainFloat SliderHexSizeKm (uiHexSizeKm ui) }
    config = WorldConfig { wcChunkSize = chunkSize }
    calCfg = mkCalendarConfig planet
    worldTime = WorldTime
      { wtTick     = uiSimTickCount ui
      , wtTickRate = realToFrac (uiSimTickRate ui)
      }
    calDate = tickToDate calCfg worldTime
    yf      = realToFrac (yearFraction calCfg worldTime) :: Float
    hpd     = realToFrac (ccHoursPerDay calCfg) :: Float
    calHour = realToFrac (cdHourOfDay calDate) :: Float
    tiltDeg = pcAxialTilt planet

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
