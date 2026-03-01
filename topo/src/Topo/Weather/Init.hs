{-# LANGUAGE OverloadedStrings #-}

-- | Weather overlay initialisation from climate state.
module Topo.Weather.Init
  ( initWeatherStage
  , seasonalITCZLatitude
  ) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector.Unboxed as U
import Topo.Calendar (yearFraction, mkCalendarConfig)
import Topo.Math (clamp01)
import Topo.Overlay (Overlay(..), OverlayData(..), OverlayProvenance(..), insertOverlay)
import Topo.Pipeline (PipelineStage(..))
import Topo.Pipeline.Stage (StageId(..))
import Topo.Planet (LatitudeMapping(..), PlanetConfig(..))
import Topo.Plugin (logInfo, modifyWorldP)
import Topo.Types
import Topo.Weather.Config (WeatherConfig(..))
import Topo.Weather.Grid (weatherChunkToOverlay, weatherOverlaySchema)
import Topo.World (TerrainWorld(..))

-- | Initialise weather overlay from climate fields without stochastic
-- jitter.
initWeatherStage :: WeatherConfig -> PipelineStage
initWeatherStage cfg = PipelineStage StageWeather "initWeather" "initWeather" Nothing [] Nothing $ do
  logInfo "initWeather: deriving initial weather from climate"
  modifyWorldP $ \world ->
    let config = twConfig world
        climateMap = twClimate world
        lm = twLatMapping world
        planet = twPlanet world
        radPerTile = lmRadPerTile lm
        latBiasRad = lmBiasRad lm
        tiltScale = lmTiltScale lm
        worldTime = twWorldTime world
        calCfg = mkCalendarConfig planet
        yf = yearFraction calCfg worldTime
        dynamicPhase = wcSeasonPhase cfg + realToFrac yf * 2 * pi
        dynamicITCZLat = seasonalITCZLatitude
                           (wcITCZLatitude cfg)
                           (wcITCZMigrationScale cfg)
                           (pcAxialTilt planet)
                           dynamicPhase
        cfg' = cfg
          { wcSeasonAmplitude = wcSeasonAmplitude cfg * tiltScale
          , wcSeasonPhase = dynamicPhase
          , wcITCZLatitude = dynamicITCZLat
          }
        weather' = IntMap.mapWithKey
          (buildInitialWeatherChunk config cfg' radPerTile latBiasRad)
          climateMap
        overlayChunks = IntMap.map weatherChunkToOverlay weather'
        weatherOverlay = Overlay
          { ovSchema = weatherOverlaySchema
          , ovData   = DenseData overlayChunks
          , ovProvenance = OverlayProvenance
              { opSeed = twSeed world
              , opVersion = 1
              , opSource = "weather"
              }
          }
        overlays' = insertOverlay weatherOverlay (twOverlays world)
    in world { twOverlays = overlays' }

buildInitialWeatherChunk
  :: WorldConfig -> WeatherConfig
  -> Float -> Float
  -> Int -> ClimateChunk -> WeatherChunk
buildInitialWeatherChunk config cfg radPerTile latBiasRad key climate =
  let origin = chunkOriginTile config (chunkCoordFromId (ChunkId key))
      n = chunkTileCount config
      temp = U.generate n
        (initialWeatherTempAt config cfg radPerTile latBiasRad origin (ccTempAvg climate))
      humidity = ccHumidityAvg climate
      windDir = ccWindDirAvg climate
      pressure = U.generate n
        (initialWeatherPressureAt config cfg radPerTile latBiasRad origin temp humidity)
      windSpd = ccWindSpdAvg climate
      precip = U.generate n
        (initialWeatherPrecipAt config cfg radPerTile latBiasRad origin (ccPrecipAvg climate))
  in WeatherChunk
      { wcTemp = temp
      , wcHumidity = humidity
      , wcWindDir = windDir
      , wcWindSpd = windSpd
      , wcPressure = pressure
      , wcPrecip = precip
      }

initialWeatherTempAt
  :: WorldConfig -> WeatherConfig
  -> Float -> Float -> TileCoord
  -> U.Vector Float -> Int -> Float
initialWeatherTempAt config cfg radPerTile latBiasRad origin climate i =
  let TileCoord _lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord _ox oy = origin
      latRad = fromIntegral (oy + ly) * radPerTile + latBiasRad
      latScale = abs (sin latRad)
      seasonal = sin (wcSeasonPhase cfg + latRad)
               * wcSeasonAmplitude cfg * latScale
  in clamp01 (climate U.! i + seasonal)

initialWeatherPressureAt
  :: WorldConfig -> WeatherConfig
  -> Float -> Float -> TileCoord
  -> U.Vector Float -> U.Vector Float -> Int -> Float
initialWeatherPressureAt config cfg radPerTile latBiasRad origin tempVec humVec i =
  let TileCoord _lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord _ox oy = origin
      latRad = fromIntegral (oy + ly) * radPerTile + latBiasRad
      tempVal = tempVec U.! i
      humVal  = humVec U.! i
      tempPressure = wcPressureBase cfg
                   - tempVal * wcPressureTempScale cfg
                   - humVal  * wcPressureHumidityScale cfg
      coriolis = cos (3 * latRad) * wcPressureCoriolisScale cfg
  in clamp01 (tempPressure + coriolis)

initialWeatherPrecipAt
  :: WorldConfig -> WeatherConfig
  -> Float -> Float -> TileCoord
  -> U.Vector Float -> Int -> Float
initialWeatherPrecipAt config cfg radPerTile latBiasRad origin climate i =
  let TileCoord _lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord _ox oy = origin
      latRad = fromIntegral (oy + ly) * radPerTile + latBiasRad
      latScale = abs (sin latRad)
      rawSeasonal = (sin (wcSeasonPhase cfg + latRad) * latScale + 1) * 0.5
      seasonalFactor = wcSeasonalBase cfg + rawSeasonal * wcSeasonalRange cfg
  in clamp01 ((climate U.! i) * seasonalFactor)

-- | Dynamic ITCZ latitude from base position, migration scale, axial
-- tilt, and current season phase.
seasonalITCZLatitude
  :: Float
  -> Float
  -> Float
  -> Float
  -> Float
seasonalITCZLatitude baseLat migScale tilt phase =
  baseLat + migScale * tilt * sin phase
