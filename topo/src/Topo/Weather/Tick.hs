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
import Topo.Calendar (CalendarConfig(..), CalendarDate(..), mkCalendarConfig, tickToDate, yearFraction, WorldTime(..))
import Topo.Climate.Evaporation (satNorm)
import Topo.Climate.ITCZ (convergenceField, itczBand, defaultConvergenceConfig, ConvergenceConfig(..))
import Topo.Grid.Diffusion (diffuseFieldGrid)
import Topo.Math (clamp01)
import Topo.Noise (noise2D, noise2DContinuous)
import Topo.Overlay (Overlay(..), OverlayData(..), OverlayProvenance(..))
import Topo.Planet (LatitudeMapping(..), PlanetConfig(..), WorldSlice(..), hexesPerDegreeLatitude)
import Topo.Seed (deriveOverlaySeed)
import Topo.Simulation (SimNode(..), SimNodeId(..), SimContext(..))
import Topo.Solar (SolarConfig(..), defaultSolarConfig, tileIrradiance)
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
            cw = clamp01 (cf * (humidity U.! i))
            -- Optical-depth albedo: saturates around 0.8 for thick clouds
            tau = cw * wcCloudOpticalScale cfg
            cloudAlbedo = 1 - exp (negate tau)
        in clamp01 (t * (1 - cloudAlbedo * cf)))
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
      cloudWater = U.generate n (\i ->
        let cf = cloudFracV U.! i
            h  = humidity U.! i
        in clamp01 (cf * h))
      -- Layer distribution for buildWeatherChunk (noise-driven initial).
      -- Humidity → mostly low; some mid and high.
      cloudCoverLow  = U.map (\cf -> clamp01 (cf * 0.60)) cloudFracV
      cloudCoverMid  = U.map (\cf -> clamp01 (cf * 0.25)) cloudFracV
      cloudCoverHigh = U.map (\cf -> clamp01 (cf * 0.15)) cloudFracV
      cloudWaterLow  = U.map (\cw -> clamp01 (cw * 0.60)) cloudWater
      cloudWaterMid  = U.map (\cw -> clamp01 (cw * 0.25)) cloudWater
      cloudWaterHigh = U.map (\cw -> clamp01 (cw * 0.15)) cloudWater
  in WeatherChunk
      { wcTemp = temp
      , wcHumidity = humidity
      , wcWindDir = windDir
      , wcWindSpd = windSpd
      , wcPressure = pressure
      , wcPrecip = precip
      , wcCloudCover = cloudFracV
      , wcCloudWater = cloudWater
      , wcCloudCoverLow  = cloudCoverLow
      , wcCloudCoverMid  = cloudCoverMid
      , wcCloudCoverHigh = cloudCoverHigh
      , wcCloudWaterLow  = cloudWaterLow
      , wcCloudWaterMid  = cloudWaterMid
      , wcCloudWaterHigh = cloudWaterHigh
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
      -- ITCZ boost: latitude-based Gaussian (per-chunk fallback;
      -- the grid-level tickWeatherGrid uses convergence-driven ITCZ).
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
  -> U.Vector Float   -- ^ Per-tile solar irradiance [0,1]
  -> WeatherGridState
  -> WeatherGridState
tickWeatherGrid config seed cfg radPerTile latBiasRad timeHash minCoord gridW gridH climate solarIrr prev =
  let n = gridW * gridH
      dt = min 0.49 (max 0 (wcAdvectDt cfg))
      pull = clamp01 (wcClimatePullStrength cfg)
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
      cloudCoverAdv = advectFieldGrid gridW gridH dt (wgsWindDir prev) (wgsWindSpd prev) (wgsCloudCover prev)
      cloudWaterAdv = advectFieldGrid gridW gridH dt (wgsWindDir prev) (wgsWindSpd prev) (wgsCloudWater prev)

      -- Per-layer advection (independent — mimics wind shear at altitude)
      ccLowAdv  = advectFieldGrid gridW gridH dt (wgsWindDir prev) (wgsWindSpd prev) (wgsCloudCoverLow prev)
      ccMidAdv  = advectFieldGrid gridW gridH dt (wgsWindDir prev) (wgsWindSpd prev) (wgsCloudCoverMid prev)
      ccHighAdv = advectFieldGrid gridW gridH dt (wgsWindDir prev) (wgsWindSpd prev) (wgsCloudCoverHigh prev)
      cwLowAdv  = advectFieldGrid gridW gridH dt (wgsWindDir prev) (wgsWindSpd prev) (wgsCloudWaterLow prev)
      cwMidAdv  = advectFieldGrid gridW gridH dt (wgsWindDir prev) (wgsWindSpd prev) (wgsCloudWaterMid prev)
      cwHighAdv = advectFieldGrid gridW gridH dt (wgsWindDir prev) (wgsWindSpd prev) (wgsCloudWaterHigh prev)

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
            -- ITCZ precipitation boost from wind convergence
            convBoost = itczIntensity U.! i * wcITCZPrecipBoost cfg
        in pRelaxed + convBoost)

      timeSeed = deriveWeatherSeed seed timeHash

      -- Compute convergence field from current wind state for
      -- ITCZ-driven pressure trough and precipitation boost.
      convGrid = convergenceField gridW gridH (wgsWindDir prev) (wgsWindSpd prev)
      latDegsGrid = U.generate n (\i ->
        let (_, gy) = globalTileXY config minCoord gridW i
            latRad = fromIntegral gy * radPerTile + latBiasRad
        in latRad * (180.0 / pi))
      itczConvCfg = defaultConvergenceConfig
        { convStrengthTotal = 1.0
        , convLatitudeWidth = wcITCZWidth cfg
        , convCenterLat     = wcITCZLatitude cfg
        }
      itczIntensity = itczBand itczConvCfg convGrid latDegsGrid

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
            -- ITCZ pressure trough: convergence zones have lower pressure
            itczDrop = itczIntensity U.! i * wcITCZPrecipBoost cfg * 0.1
            noiseVal = (n0 * 2 - 1) * 0.05
        in tempPressure + coriolis - itczDrop + noiseVal)

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
            tPulled = clamp01 (climatePull t target pull)
            -- Per-layer cloud-radiation model.
            -- Low clouds: high albedo (optically thick), weak greenhouse.
            -- Mid clouds: moderate albedo, moderate greenhouse.
            -- High clouds: low albedo (optically thin), strong greenhouse.
            irr = solarIrr U.! i
            optScale = wcCloudOpticalScale cfg
            ghCoeff  = wcCloudGreenhouseCoeff cfg

            -- Low layer: thick, high albedo
            cfLo = ccLowSmooth U.! i
            cwLo = clamp01 (cwLowSmooth U.! i)
            tauLo = cwLo * optScale * 1.5      -- optically thickest
            albLo = 1 - exp (negate tauLo)

            -- Mid layer: moderate
            cfMi = ccMidSmooth U.! i
            cwMi = clamp01 (cwMidSmooth U.! i)
            tauMi = cwMi * optScale
            albMi = 1 - exp (negate tauMi)

            -- High layer: thin, low albedo
            cfHi = ccHighSmooth U.! i
            cwHi = clamp01 (cwHighSmooth U.! i)
            tauHi = cwHi * optScale * 0.4       -- optically thinnest
            albHi = 1 - exp (negate tauHi)

            -- Combined shortwave albedo: product of per-layer transmittances
            swTransmit = (1 - albLo * cfLo) * (1 - albMi * cfMi) * (1 - albHi * cfHi)
            effectiveIrr = irr * swTransmit
            solarDelta = 0.025 * (effectiveIrr - 0.35)

            -- Longwave greenhouse: high clouds dominate (3× coefficient),
            -- low clouds have weak greenhouse.  Strongest at night.
            nightFactor = max 0 (1 - irr * 2)
            ghLo = ghCoeff * 0.5 * cfLo * cwLo * nightFactor
            ghMi = ghCoeff * 1.0 * cfMi * cwMi * nightFactor
            ghHi = ghCoeff * 3.0 * cfHi * cwHi * nightFactor
            greenhouse = ghLo + ghMi + ghHi

            -- Convective cooling: ITCZ convergence + precipitation
            p = precipDiffused U.! i
            conv = max 0 (itczIntensity U.! i)
            cooling = 0.08 * p * conv
        in clamp01 (tPulled + solarDelta + greenhouse - cooling))
      humidityFinal = U.generate n (\i ->
        let h = humDiffused U.! i
            target = climateHumBase U.! i
        in clamp01 (climatePull h target pull))
      precipFinal = U.generate n (\i ->
        let p = precipDiffused U.! i
            target = seasonalPrecipBaseline U.! i
            autoP = autoconversion U.! i
        in clamp01 (climatePull p target pull + autoP))
      windSpdFinal = U.map clamp01 windSpdRaw
      pressureFinal = U.map clamp01 pressureDiffused

      -- Cloud evolution: advect → diffuse → per-layer formation/dissipation.
      -- Each layer (low/mid/high) evolves independently, then composites
      -- are derived for the total cloud_cover and cloud_water fields.
      cloudCoverSmooth = diffuseFieldGridWeather gridW gridH 1 weatherDiffuse cloudCoverAdv
      cloudWaterSmooth = diffuseFieldGridWeather gridW gridH 1 weatherDiffuse cloudWaterAdv

      -- Diffuse per-layer fields
      ccLowSmooth  = diffuseFieldGridWeather gridW gridH 1 weatherDiffuse ccLowAdv
      ccMidSmooth  = diffuseFieldGridWeather gridW gridH 1 weatherDiffuse ccMidAdv
      ccHighSmooth = diffuseFieldGridWeather gridW gridH 1 weatherDiffuse ccHighAdv
      cwLowSmooth  = diffuseFieldGridWeather gridW gridH 1 weatherDiffuse cwLowAdv
      cwMidSmooth  = diffuseFieldGridWeather gridW gridH 1 weatherDiffuse cwMidAdv
      cwHighSmooth = diffuseFieldGridWeather gridW gridH 1 weatherDiffuse cwHighAdv

      formRate      = wcCloudFormationRate cfg
      dissRate      = wcCloudDissipationRate cfg
      satThresh     = wcCloudSaturationThreshold cfg
      convThresh    = wcConvectiveThreshold cfg
      subsidenceDiss = wcSubsidenceDissipation cfg

      -- Per-layer cloud cover evolution.
      -- Formation sources are routed to layers by physical mechanism:
      --   humidity supersaturation → 60% low, 30% mid, 10% high
      --   convective uplift       → 20% low, 30% mid, 50% high (tower)
      --   frontal convergence     → 20% low, 50% mid, 30% high
      -- Dissipation applies uniformly to each layer.
      evolveLayerCover :: Float -> Float -> Float  -- formation fractions
                       -> U.Vector Float           -- smoothed layer cover
                       -> U.Vector Float
      evolveLayerCover humF convF frontF layerSmooth = U.generate n (\i ->
        let prevCf = clamp01 (layerSmooth U.! i)
            h      = humidityFinal U.! i
            t      = tempFinal U.! i
            p      = pressureFinal U.! i
            conv   = max 0 (convGrid U.! i)
            room   = 1 - prevCf

            humFormation = formRate * max 0 (h - satThresh) * room * humF
            tExcess = max 0 (t - (seasonalTempBaseline U.! i) - convThresh)
            convectiveFormation = formRate * tExcess * h * room * convF
            frontalFormation = formRate * conv * h * 0.5 * room * frontF
            totalFormation = humFormation + convectiveFormation + frontalFormation

            baseDissipation = dissRate * prevCf * (1 - h)
            subsidenceFactor = subsidenceDiss * max 0 (p - 0.5) * prevCf
            totalDissipation = baseDissipation + subsidenceFactor
        in clamp01 (prevCf + totalFormation - totalDissipation))

      ccLowFinal  = evolveLayerCover 0.60 0.20 0.20 ccLowSmooth
      ccMidFinal  = evolveLayerCover 0.30 0.30 0.50 ccMidSmooth
      ccHighFinal = evolveLayerCover 0.10 0.50 0.30 ccHighSmooth

      -- Total cloud cover: random-overlap assumption.
      -- total = 1 − (1−low)(1−mid)(1−high)
      cloudCoverFinal = U.generate n (\i ->
        let lo = ccLowFinal U.! i
            mi = ccMidFinal U.! i
            hi = ccHighFinal U.! i
        in clamp01 (1 - (1 - lo) * (1 - mi) * (1 - hi)))

      -- Autoconversion: excess cloud water above threshold → precipitation.
      -- Low and mid clouds produce rain efficiently; high clouds (ice) much less.
      autoThresh = wcAutoconversionThreshold cfg
      precipEff  = wcPrecipEfficiency cfg

      cwLowRaw = U.generate n (\i ->
        clamp01 (ccLowFinal U.! i * humidityFinal U.! i))
      cwMidRaw = U.generate n (\i ->
        clamp01 (ccMidFinal U.! i * humidityFinal U.! i))
      cwHighRaw = U.generate n (\i ->
        clamp01 (ccHighFinal U.! i * humidityFinal U.! i * 0.5))

      autoLow = U.generate n (\i ->
        max 0 (cwLowRaw U.! i - autoThresh) * precipEff)
      autoMid = U.generate n (\i ->
        max 0 (cwMidRaw U.! i - autoThresh) * precipEff)
      -- High clouds: very low precipitation efficiency (virga/ice)
      autoHigh = U.generate n (\i ->
        max 0 (cwHighRaw U.! i - autoThresh) * precipEff * 0.15)

      autoconversion = U.generate n (\i ->
        autoLow U.! i + autoMid U.! i + autoHigh U.! i)

      cwLowFinal  = U.generate n (\i -> clamp01 (cwLowRaw U.! i  - autoLow U.! i))
      cwMidFinal  = U.generate n (\i -> clamp01 (cwMidRaw U.! i  - autoMid U.! i))
      cwHighFinal = U.generate n (\i -> clamp01 (cwHighRaw U.! i - autoHigh U.! i))

      -- Total cloud water: sum of layers, clamped.
      cloudWaterFinal = U.generate n (\i ->
        clamp01 (cwLowFinal U.! i + cwMidFinal U.! i + cwHighFinal U.! i))

  in WeatherGridState
      { wgsTemp = tempFinal
      , wgsHumidity = humidityFinal
      , wgsWindDir = U.map normalizeAngle windDir
      , wgsWindSpd = windSpdFinal
      , wgsPressure = pressureFinal
      , wgsPrecip = precipFinal
      , wgsCloudCover = cloudCoverFinal
      , wgsCloudWater = cloudWaterFinal
      , wgsCloudCoverLow  = ccLowFinal
      , wgsCloudCoverMid  = ccMidFinal
      , wgsCloudCoverHigh = ccHighFinal
      , wgsCloudWaterLow  = cwLowFinal
      , wgsCloudWaterMid  = cwMidFinal
      , wgsCloudWaterHigh = cwHighFinal
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

      -- Solar parameters for diurnal cycle
      slice = twSlice terrain
      hex   = twHexGrid terrain
      calDate = tickToDate calCfg wtime
      hpd     = realToFrac (ccHoursPerDay calCfg) :: Float
      calHour = realToFrac (cdHourOfDay calDate) :: Float
      tiltDeg = pcAxialTilt planet
      yfF     = realToFrac yf :: Float
      solarCfg = defaultSolarConfig
      hpdLat  = hexesPerDegreeLatitude planet hex

      denseChunks =
        case chunkCoordBounds climateMap of
          Nothing -> IntMap.empty
          Just (minCoord@(ChunkCoord minCx minCy), ChunkCoord maxCx maxCy) ->
            let chunkSize = wcChunkSize config
                gridW = (maxCx - minCx + 1) * chunkSize
                gridH = (maxCy - minCy + 1) * chunkSize

                -- Pre-compute per-tile solar irradiance
                solarIrr = U.generate (gridW * gridH) (\i ->
                  let (gx, gy) = globalTileXY config minCoord gridW i
                      latRad = fromIntegral gy * radPerTile + latBiasRad
                      -- Approximate longitude: use latitude-corrected tiles/degree
                      hpdLon = hpdLat * max 0.001 (cos latRad)
                      cs = wcChunkSize config
                      centerTileX = cs `div` 2
                      lonDeg = wsLonCenter slice
                             + fromIntegral (gx - centerTileX) / hpdLon
                  in tileIrradiance solarCfg tiltDeg yfF hpd calHour latRad lonDeg
                        * lmInsolation lm)

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
                  , wgsCloudCover = buildWeatherFieldGrid config prevChunkWeather minCoord gridW gridH wcCloudCover
                  , wgsCloudWater = buildWeatherFieldGrid config prevChunkWeather minCoord gridW gridH wcCloudWater
                  , wgsCloudCoverLow  = buildWeatherFieldGrid config prevChunkWeather minCoord gridW gridH wcCloudCoverLow
                  , wgsCloudCoverMid  = buildWeatherFieldGrid config prevChunkWeather minCoord gridW gridH wcCloudCoverMid
                  , wgsCloudCoverHigh = buildWeatherFieldGrid config prevChunkWeather minCoord gridW gridH wcCloudCoverHigh
                  , wgsCloudWaterLow  = buildWeatherFieldGrid config prevChunkWeather minCoord gridW gridH wcCloudWaterLow
                  , wgsCloudWaterMid  = buildWeatherFieldGrid config prevChunkWeather minCoord gridW gridH wcCloudWaterMid
                  , wgsCloudWaterHigh = buildWeatherFieldGrid config prevChunkWeather minCoord gridW gridH wcCloudWaterHigh
                  }
                nextState = tickWeatherGrid
                              config seed cfg' radPerTile latBiasRad timeHash
                              minCoord gridW gridH climateMap solarIrr prevState
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
