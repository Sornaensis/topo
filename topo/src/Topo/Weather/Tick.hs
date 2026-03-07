{-# LANGUAGE OverloadedStrings #-}

-- | Stateful weather simulation tick pipeline.
module Topo.Weather.Tick
  ( weatherSimNode
  , cloudFraction
  ) where

import qualified Data.IntMap.Strict as IntMap
import Data.Text (Text)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word64)
import Topo.Calendar (mkCalendarConfig, yearFraction, WorldTime(..))
import Topo.Climate.Evaporation (satNorm)
import Topo.Grid.Diffusion (diffuseFieldGrid)
import Topo.Math (clamp01)
import Topo.Noise (noise2D, noise2DContinuous)
import Topo.Overlay (Overlay(..), OverlayData(..), OverlayProvenance(..))
import Topo.Planet (LatitudeMapping(..), PlanetConfig(..))
import Topo.Seed (deriveOverlaySeed)
import Topo.Simulation (SimNode(..), SimNodeId(..), SimContext(..))
import Topo.TerrainGrid (chunkCoordBounds)
import Topo.Types
import Topo.Weather.Config (WeatherConfig(..))
import Topo.Weather.Grid
  ( WeatherGridState(..)
  , buildChunkWeatherFromOverlayOrClimate
  , buildClimateFieldGrid
  , buildWeatherFieldGrid
  , globalTileXY
  , weatherGridToDenseOverlay
  , weatherOverlaySchema
  )
import Topo.Weather.Init (seasonalITCZLatitude)
import Topo.Weather.Operators
  ( advectFieldGrid
  , climatePull
  , condensation
  , diffuseFieldGridWeather
  , normalizeAngle
  , windDirectionFromPressure
  , windSpeedFromPressure
  )
import Topo.World (TerrainWorld(..))

weatherPressureDiffuseIterations :: Int
weatherPressureDiffuseIterations = 1

weatherPressureDiffuseFactor :: Float
weatherPressureDiffuseFactor = 0.3

-- | Cloud fraction from relative humidity.
cloudFraction :: Float -> Float -> Float
cloudFraction rhExp rh = clamp01 (rh ** rhExp)

deriveWeatherSeed :: Word64 -> Word64 -> Word64
deriveWeatherSeed = deriveOverlaySeed

coherentNoiseAt :: Word64 -> Float -> Int -> Int -> Float
coherentNoiseAt seed frequency x y =
  noise2DContinuous seed (fromIntegral x * frequency) (fromIntegral y * frequency)

buildWeatherChunk
  :: WorldConfig -> Word64 -> WeatherConfig
  -> Float -> Float -> Word64
  -> Int -> ClimateChunk -> WeatherChunk
buildWeatherChunk config seed cfg radPerTile latBiasRad timeHash key climate =
  let origin = chunkOriginTile config (chunkCoordFromId (ChunkId key))
      chunkSize = wcChunkSize config
      n = chunkTileCount config
      tempRaw = U.generate n
        (weatherTempAt config seed cfg radPerTile latBiasRad timeHash origin
           (ccTempAvg climate))
      humidity = U.generate n
        (weatherHumidityAt config seed cfg timeHash origin
           (ccPrecipAvg climate) tempRaw)
      cloudFracV = U.generate n (\i ->
        cloudFraction (wcCloudRHExponent cfg) (humidity U.! i))
      tempCloud = U.generate n (\i ->
        let t = tempRaw U.! i
            cf = cloudFracV U.! i
        in clamp01 (t * (1 - wcCloudAlbedoEffect cfg * cf)))
      temp = diffuseFieldGrid chunkSize chunkSize
               (wcTempDiffuseIterations cfg)
               (wcTempDiffuseFactor cfg)
               tempCloud
      windDir = U.generate n
        (weatherWindDirAt config seed timeHash origin (ccWindDirAvg climate))
      pressureRaw = U.generate n
        (weatherPressureAt config seed cfg radPerTile latBiasRad timeHash origin temp humidity)
      pressure = diffuseFieldGrid chunkSize chunkSize
                   weatherPressureDiffuseIterations
                   weatherPressureDiffuseFactor
                   pressureRaw
      windSpd = U.generate n
        (weatherWindSpdAt config seed cfg timeHash origin (ccWindSpdAvg climate) pressure)
      precipRaw = U.generate n
        (weatherPrecipAt config seed cfg radPerTile latBiasRad timeHash origin
           (ccPrecipAvg climate))
      precip = U.generate n (\i ->
        let p = precipRaw U.! i
            cf = cloudFracV U.! i
        in clamp01 (p * (1 + wcCloudPrecipBoost cfg * cf)))
  in WeatherChunk
      { wcTemp = temp
      , wcHumidity = humidity
      , wcWindDir = windDir
      , wcWindSpd = windSpd
      , wcPressure = pressure
      , wcPrecip = precip
      }

weatherTempAt
  :: WorldConfig -> Word64 -> WeatherConfig
  -> Float -> Float -> Word64 -> TileCoord
  -> U.Vector Float -> Int -> Float
weatherTempAt config seed cfg radPerTile latBiasRad timeHash origin climate i =
  let TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
      latRad = fromIntegral (oy + ly) * radPerTile + latBiasRad
      latScale = abs (sin latRad)
      seasonal = sin (wcSeasonPhase cfg + latRad)
               * wcSeasonAmplitude cfg * latScale
      timeSeed = deriveWeatherSeed seed timeHash
      n0 = coherentNoiseAt timeSeed (wcTempNoiseFrequency cfg) (ox + lx + 5000) (oy + ly + 5000)
      jitter = (n0 * 2 - 1) * wcJitterAmplitude cfg
  in clamp01 (climate U.! i + jitter + seasonal)

weatherHumidityAt
  :: WorldConfig -> Word64 -> WeatherConfig -> Word64 -> TileCoord
  -> U.Vector Float -> U.Vector Float -> Int -> Float
weatherHumidityAt config seed cfg timeHash origin climatePrecip tempVec i =
  let TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
      timeSeed = deriveWeatherSeed seed timeHash
      n0 = coherentNoiseAt timeSeed (wcHumidityNoiseFrequency cfg) (ox + lx + 6000) (oy + ly + 6000)
      tVal = tempVec U.! i
      sat  = max 0.001 (satNorm tVal)
      rh   = clamp01 ((climatePrecip U.! i) / sat)
      noiseMult = 1 + (n0 * 2 - 1) * wcHumidityNoiseScale cfg
  in clamp01 (rh * noiseMult)

weatherWindDirAt
  :: WorldConfig -> Word64 -> Word64 -> TileCoord
  -> U.Vector Float -> Int -> Float
weatherWindDirAt config seed timeHash origin climate i =
  let TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
      timeSeed = deriveWeatherSeed seed timeHash
      n0 = noise2D timeSeed (ox + lx + 7000) (oy + ly + 7000)
  in climate U.! i + n0 * 0.2

weatherWindSpdAt
  :: WorldConfig -> Word64 -> WeatherConfig -> Word64 -> TileCoord
  -> U.Vector Float -> U.Vector Float -> Int -> Float
weatherWindSpdAt config seed cfg timeHash origin climateWind pressureVec i =
  let TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
      timeSeed = deriveWeatherSeed seed timeHash
      n0 = noise2D timeSeed (ox + lx + 8000) (oy + ly + 8000)
      cs = wcChunkSize config
      baseWind = windSpeedFromPressure
                   cs
                   cs
                   pressureVec
                   (climateWind U.! i)
                   (wcPressureGradientWindScale cfg)
                   i
      noiseMult = 1.0 + (n0 * 2 - 1) * wcWindNoiseScale cfg
  in clamp01 (baseWind * noiseMult)

weatherPressureAt
  :: WorldConfig -> Word64 -> WeatherConfig
  -> Float -> Float -> Word64 -> TileCoord
  -> U.Vector Float -> U.Vector Float -> Int -> Float
weatherPressureAt config seed cfg radPerTile latBiasRad timeHash origin tempVec humVec i =
  let TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
      timeSeed = deriveWeatherSeed seed timeHash
      n0 = coherentNoiseAt timeSeed (wcPressureNoiseFrequency cfg) (ox + lx + 9000) (oy + ly + 9000)
      latRad = fromIntegral (oy + ly) * radPerTile + latBiasRad
      tempVal = tempVec U.! i
      humVal  = humVec U.! i
      tempPressure = wcPressureBase cfg
                   - tempVal * wcPressureTempScale cfg
                   - humVal  * wcPressureHumidityScale cfg
      coriolis = cos (3 * latRad) * wcPressureCoriolisScale cfg
      noiseVal = (n0 * 2 - 1) * 0.05
  in clamp01 (tempPressure + coriolis + noiseVal)

weatherPrecipAt
  :: WorldConfig -> Word64 -> WeatherConfig
  -> Float -> Float -> Word64 -> TileCoord
  -> U.Vector Float -> Int -> Float
weatherPrecipAt config seed cfg radPerTile latBiasRad timeHash origin climate i =
  let TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
      timeSeed = deriveWeatherSeed seed timeHash
      n0 = noise2D timeSeed (ox + lx + 10000) (oy + ly + 10000)
      latRad = fromIntegral (oy + ly) * radPerTile + latBiasRad
      latDeg = latRad * (180.0 / pi)
      latScale = abs (sin latRad)
      rawSeasonal = (sin (wcSeasonPhase cfg + latRad) * latScale + 1) * 0.5
      seasonalFactor = wcSeasonalBase cfg
                     + rawSeasonal * wcSeasonalRange cfg
      dLat = latDeg - wcITCZLatitude cfg
      itczW = max 0.1 (wcITCZWidth cfg)
      convergenceFactor = 1.0 + wcITCZPrecipBoost cfg
                        * exp (negate (dLat * dLat) / (2 * itczW * itczW))
      noiseMult = (n0 * 2 - 1) * wcPrecipNoiseScale cfg
      baseP = (climate U.! i) * seasonalFactor * convergenceFactor
  in clamp01 (baseP + noiseMult * (climate U.! i))

tickWeatherGrid
  :: WorldConfig
  -> Word64
  -> WeatherConfig
  -> Float
  -> Float
  -> Word64
  -> ChunkCoord
  -> Int
  -> Int
  -> IntMap.IntMap ClimateChunk
  -> WeatherGridState
  -> WeatherGridState
tickWeatherGrid config seed cfg radPerTile latBiasRad timeHash minCoord gridW gridH climate prev =
  let n = gridW * gridH
      dt = min 0.49 (max 0 (wcAdvectDt cfg))
      pull = clamp01 (wcClimatePullStrength cfg)
      condensationRate = clamp01 (wcCondensationRate cfg)
      weatherDiffuse = clamp01 (wcWeatherDiffuseFactor cfg)
      windResponse = clamp01 (wcWindResponseRate cfg)

      climateTempBase = buildClimateFieldGrid config climate minCoord gridW gridH ccTempAvg
      climateHumBase = buildClimateFieldGrid config climate minCoord gridW gridH ccHumidityAvg
      climatePrecipBase = buildClimateFieldGrid config climate minCoord gridW gridH ccPrecipAvg

      seasonalTempBaseline = U.generate n (\i ->
        let (_, gy) = globalTileXY config minCoord gridW i
            latRad = fromIntegral gy * radPerTile + latBiasRad
            latScale = abs (sin latRad)
            seasonal = sin (wcSeasonPhase cfg + latRad) * wcSeasonAmplitude cfg * latScale
        in clamp01 (climateTempBase U.! i + seasonal))

      seasonalPrecipBaseline = U.generate n (\i ->
        let (_, gy) = globalTileXY config minCoord gridW i
            latRad = fromIntegral gy * radPerTile + latBiasRad
            latScale = abs (sin latRad)
            rawSeasonal = (sin (wcSeasonPhase cfg + latRad) * latScale + 1) * 0.5
            factor = wcSeasonalBase cfg + rawSeasonal * wcSeasonalRange cfg
        in clamp01 ((climatePrecipBase U.! i) * factor))

      tempAdv = advectFieldGrid gridW gridH dt (wgsWindDir prev) (wgsWindSpd prev) (wgsTemp prev)
      humAdv = advectFieldGrid gridW gridH dt (wgsWindDir prev) (wgsWindSpd prev) (wgsHumidity prev)
      precipAdv = advectFieldGrid gridW gridH dt (wgsWindDir prev) (wgsWindSpd prev) (wgsPrecip prev)

      tempSource = U.generate n (\i ->
        let t = tempAdv U.! i
            tgt = seasonalTempBaseline U.! i
        in climatePull t tgt pull)

      humSource = U.generate n (\i ->
        let h = humAdv U.! i
            tgt = climateHumBase U.! i
        in climatePull h tgt pull)

      precipSource = U.generate n (\i ->
        let pAdv = precipAdv U.! i
            pRelaxed = pAdv + pull * ((seasonalPrecipBaseline U.! i) - pAdv)
            h = humSource U.! i
            t = tempSource U.! i
            condense = condensation h t condensationRate
        in pRelaxed + condense)

      timeSeed = deriveWeatherSeed seed timeHash

      pressureRaw = U.generate n (\i ->
        let (gx, gy) = globalTileXY config minCoord gridW i
            latRad = fromIntegral gy * radPerTile + latBiasRad
            t = tempSource U.! i
            h = humSource U.! i
            n0 = coherentNoiseAt timeSeed (wcPressureNoiseFrequency cfg) (gx + 9000) (gy + 9000)
            tempPressure = wcPressureBase cfg
                         - t * wcPressureTempScale cfg
                         - h * wcPressureHumidityScale cfg
            coriolis = cos (3 * latRad) * wcPressureCoriolisScale cfg
            noiseVal = (n0 * 2 - 1) * 0.05
        in tempPressure + coriolis + noiseVal)

      pressure = diffuseFieldGridWeather gridW gridH weatherPressureDiffuseIterations weatherPressureDiffuseFactor pressureRaw

      windDir = U.generate n (\i ->
        windDirectionFromPressure
          gridW
          gridH
          pressure
          (wgsWindDir prev U.! i)
          windResponse
          i)

      windSpdRaw = U.generate n (\i ->
        windSpeedFromPressure
          gridW
          gridH
          pressure
          (wgsWindSpd prev U.! i)
          (wcPressureGradientWindScale cfg)
          i)

      tempDiffused = diffuseFieldGridWeather gridW gridH 1 weatherDiffuse tempSource
      humDiffused = diffuseFieldGridWeather gridW gridH 1 weatherDiffuse humSource
      precipDiffused = diffuseFieldGridWeather gridW gridH 1 weatherDiffuse precipSource
      pressureDiffused = diffuseFieldGridWeather gridW gridH 1 weatherDiffuse pressure

      tempFinal = U.generate n (\i ->
        let t = tempDiffused U.! i
            target = seasonalTempBaseline U.! i
        in clamp01 (climatePull t target pull))
      humidityFinal = U.generate n (\i ->
        let h = humDiffused U.! i
            target = climateHumBase U.! i
        in clamp01 (climatePull h target pull))
      precipFinal = U.generate n (\i ->
        let p = precipDiffused U.! i
            target = seasonalPrecipBaseline U.! i
        in clamp01 (climatePull p target pull))
      windSpdFinal = U.map clamp01 windSpdRaw
      pressureFinal = U.map clamp01 pressureDiffused

  in WeatherGridState
      { wgsTemp = tempFinal
      , wgsHumidity = humidityFinal
      , wgsWindDir = U.map normalizeAngle windDir
      , wgsWindSpd = windSpdFinal
      , wgsPressure = pressureFinal
      , wgsPrecip = precipFinal
      }

weatherSimNode :: WeatherConfig -> SimNode
weatherSimNode cfg = SimNodeReader
  { snrId           = SimNodeId "weather"
  , snrOverlayName  = "weather"
  , snrDependencies = []
  , snrReadTick     = weatherTick cfg
  }

weatherTick :: WeatherConfig -> SimContext -> Overlay -> IO (Either Text Overlay)
weatherTick cfg ctx overlay = do
  let terrain = scTerrain ctx
      wtime = scWorldTime ctx
      config = twConfig terrain
      planet = twPlanet terrain
      lm = twLatMapping terrain
      radPerTile = lmRadPerTile lm
      latBiasRad = lmBiasRad lm
      tiltScale = lmTiltScale lm
      seed = twSeed terrain
      calCfg = mkCalendarConfig planet
      yf = yearFraction calCfg wtime
      dynamicPhase = wcSeasonPhase cfg + realToFrac yf * 2 * pi
      timeHash = wtTick wtime
      dynamicITCZLat = seasonalITCZLatitude
                         (wcITCZLatitude cfg)
                         (wcITCZMigrationScale cfg)
                         (pcAxialTilt planet)
                         dynamicPhase
      cfg' = cfg
        { wcSeasonAmplitude = wcSeasonAmplitude cfg * tiltScale
        , wcSeasonPhase     = dynamicPhase
        , wcITCZLatitude    = dynamicITCZLat
        }
      climateMap = twClimate terrain

      denseChunks =
        case chunkCoordBounds climateMap of
          Nothing -> IntMap.empty
          Just (minCoord@(ChunkCoord minCx minCy), ChunkCoord maxCx maxCy) ->
            let chunkSize = wcChunkSize config
                gridW = (maxCx - minCx + 1) * chunkSize
                gridH = (maxCy - minCy + 1) * chunkSize
                fallback key climate = buildWeatherChunk config seed cfg' radPerTile latBiasRad timeHash key climate
                prevChunkWeather =
                  buildChunkWeatherFromOverlayOrClimate fallback climateMap (ovData overlay)
                prevState = WeatherGridState
                  { wgsTemp = buildWeatherFieldGrid config prevChunkWeather minCoord gridW gridH wcTemp
                  , wgsHumidity = buildWeatherFieldGrid config prevChunkWeather minCoord gridW gridH wcHumidity
                  , wgsWindDir = buildWeatherFieldGrid config prevChunkWeather minCoord gridW gridH wcWindDir
                  , wgsWindSpd = buildWeatherFieldGrid config prevChunkWeather minCoord gridW gridH wcWindSpd
                  , wgsPressure = buildWeatherFieldGrid config prevChunkWeather minCoord gridW gridH wcPressure
                  , wgsPrecip = buildWeatherFieldGrid config prevChunkWeather minCoord gridW gridH wcPrecip
                  }
                nextState = tickWeatherGrid
                              config seed cfg' radPerTile latBiasRad timeHash
                              minCoord gridW gridH climateMap prevState
            in weatherGridToDenseOverlay config minCoord gridW climateMap nextState

  pure $ Right Overlay
    { ovSchema = weatherOverlaySchema
    , ovData   = DenseData denseChunks
    , ovProvenance =
        let prov = ovProvenance overlay
        in prov
          { opVersion = opVersion prov + 1
          }
    }
