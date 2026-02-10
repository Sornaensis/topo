{-# LANGUAGE OverloadedStrings #-}

-- | Weather tick configuration and update stage.
module Topo.Weather
  ( WeatherConfig(..)
  , defaultWeatherConfig
  , tickWeatherStage
  ) where

import Control.Monad.Reader (asks)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Word (Word64)
import Topo.Math (clamp01)
import Topo.Noise (noise2D)
import Topo.Pipeline (PipelineStage(..))
import Topo.Planet (PlanetConfig(..), WorldSlice(..), hexesPerDegreeLatitude)
import Topo.Plugin (logInfo, modifyWorldP, peSeed)
import Topo.Types
import Topo.World (TerrainWorld(..))
import qualified Data.Vector.Unboxed as U

-- | Weather update configuration.
data WeatherConfig = WeatherConfig
  { wcTickSeconds :: !Float
  , wcSeasonPhase :: !Float
  , wcSeasonAmplitude :: !Float
  } deriving (Eq, Show)

-- | Default weather configuration.
defaultWeatherConfig :: WeatherConfig
defaultWeatherConfig = WeatherConfig
  { wcTickSeconds = 1
  , wcSeasonPhase = 0
  , wcSeasonAmplitude = 0.15
  }

-- | Update per-chunk weather snapshots from climate.
--
-- Latitude is derived from 'PlanetConfig' and 'WorldSlice' stored in the world.
-- Season amplitude is scaled by axial tilt (0° tilt → no seasons, 23.44° → Earth-like).
tickWeatherStage :: WeatherConfig -> PipelineStage
tickWeatherStage cfg = PipelineStage "tickWeather" "tickWeather" $ do
  logInfo "tickWeather: updating weather"
  seed <- asks peSeed
  modifyWorldP $ \world ->
    let config = twConfig world
        planet = twPlanet world
        slice  = twSlice world
        hpd    = hexesPerDegreeLatitude planet
        degPerTile = 1.0 / max 0.001 hpd
        cs     = wcChunkSize config
        latBiasDeg = wsLatCenter slice - fromIntegral (cs `div` 2) * degPerTile
        radPerTile = degPerTile * (pi / 180.0)
        latBiasRad = latBiasDeg * (pi / 180.0)
        -- Scale season amplitude by axial tilt relative to Earth's 23.44°.
        tiltScale  = pcAxialTilt planet / 23.44
        cfg' = cfg { wcSeasonAmplitude = wcSeasonAmplitude cfg * tiltScale }
        weather' = IntMap.mapWithKey (buildWeatherChunk config seed cfg' radPerTile latBiasRad) (twClimate world)
    in world { twWeather = weather' }

buildWeatherChunk :: WorldConfig -> Word64 -> WeatherConfig -> Float -> Float -> Int -> ClimateChunk -> WeatherChunk
buildWeatherChunk config seed cfg radPerTile latBiasRad key climate =
  let origin = chunkOriginTile config (chunkCoordFromId (ChunkId key))
      n = chunkTileCount config
      temp = U.generate n (weatherTempAt config seed cfg radPerTile latBiasRad origin (ccTempAvg climate))
      humidity = U.generate n (weatherHumidityAt config seed origin (ccPrecipAvg climate))
      windDir = U.generate n (weatherWindDirAt config seed origin (ccWindDirAvg climate))
      windSpd = U.generate n (weatherWindSpdAt config seed origin (ccWindSpdAvg climate))
      pressure = U.generate n (weatherPressureAt config seed origin)
      precip = U.generate n (weatherPrecipAt config seed cfg radPerTile latBiasRad origin (ccPrecipAvg climate))
  in WeatherChunk
      { wcTemp = temp
      , wcHumidity = humidity
      , wcWindDir = windDir
      , wcWindSpd = windSpd
      , wcPressure = pressure
      , wcPrecip = precip
      }

weatherTempAt :: WorldConfig -> Word64 -> WeatherConfig -> Float -> Float -> TileCoord -> U.Vector Float -> Int -> Float
weatherTempAt config seed cfg radPerTile latBiasRad origin climate i =
  let TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
      latRad = fromIntegral (oy + ly) * radPerTile + latBiasRad
      seasonal = sin (latRad + wcSeasonPhase cfg) * wcSeasonAmplitude cfg
      n0 = noise2D seed (ox + lx + 5000) (oy + ly + 5000)
      jitter = n0 * 0.1 * wcTickSeconds cfg
  in clamp01 (climate U.! i + jitter + seasonal)

weatherHumidityAt :: WorldConfig -> Word64 -> TileCoord -> U.Vector Float -> Int -> Float
weatherHumidityAt config seed origin climate i =
  let TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
      n0 = noise2D seed (ox + lx + 6000) (oy + ly + 6000)
  in clamp01 (climate U.! i * 0.7 + n0 * 0.3)

weatherWindDirAt :: WorldConfig -> Word64 -> TileCoord -> U.Vector Float -> Int -> Float
weatherWindDirAt config seed origin climate i =
  let TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
      n0 = noise2D seed (ox + lx + 7000) (oy + ly + 7000)
  in climate U.! i + n0 * 0.2

weatherWindSpdAt :: WorldConfig -> Word64 -> TileCoord -> U.Vector Float -> Int -> Float
weatherWindSpdAt config seed origin climate i =
  let TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
      n0 = noise2D seed (ox + lx + 8000) (oy + ly + 8000)
  in clamp01 (climate U.! i * 0.8 + n0 * 0.2)

weatherPressureAt :: WorldConfig -> Word64 -> TileCoord -> Int -> Float
weatherPressureAt config seed origin i =
  let TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
      n0 = noise2D seed (ox + lx + 9000) (oy + ly + 9000)
  in clamp01 (0.9 + n0 * 0.1)

weatherPrecipAt :: WorldConfig -> Word64 -> WeatherConfig -> Float -> Float -> TileCoord -> U.Vector Float -> Int -> Float
weatherPrecipAt config seed cfg radPerTile latBiasRad origin climate i =
  let TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
      n0 = noise2D seed (ox + lx + 10000) (oy + ly + 10000)
      latRad = fromIntegral (oy + ly) * radPerTile + latBiasRad
      seasonal = (sin (latRad + wcSeasonPhase cfg) + 1) * 0.5
  in clamp01 (climate U.! i * (0.6 + seasonal * 0.2) + n0 * 0.3)
