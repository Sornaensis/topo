{-# LANGUAGE OverloadedStrings #-}

-- | Weather tick configuration and update stage.
--
-- Each call to 'tickWeatherStage' advances 'twWorldTime' by
-- 'wcTickSeconds' and recomputes per-chunk weather snapshots with
-- time-varying noise, latitude-dependent seasonal amplitude, and
-- temperature-derived pressure.
module Topo.Weather
  ( WeatherConfig(..)
  , defaultWeatherConfig
  , tickWeatherStage
  -- * Pure helpers (exported for testing)
  , cloudFraction
  , seasonalITCZLatitude
  ) where

import Control.Monad.Reader (asks)
import qualified Data.IntMap.Strict as IntMap
import Data.Word (Word64)
import Topo.Climate.Evaporation (satNorm)
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
  { -- | Simulation seconds per weather tick.
    wcTickSeconds :: !Float
    -- | Initial seasonal phase offset (radians).  Combined with the
    -- dynamic phase from 'twWorldTime' and 'wcSeasonCycleLength'.
  , wcSeasonPhase :: !Float
    -- | Base seasonal temperature amplitude before latitude scaling
    -- and tilt scaling.
  , wcSeasonAmplitude :: !Float
    -- | Number of weather ticks in a full seasonal (year) cycle.
    -- The dynamic season phase is @twWorldTime * 2π / wcSeasonCycleLength@.
  , wcSeasonCycleLength :: !Float
    -- | Temperature jitter amplitude from time-varying noise.
  , wcJitterAmplitude :: !Float
    -- | Base pressure level (high-pressure reference).
  , wcPressureBase :: !Float
    -- | Strength of temperature → pressure coupling.
    -- Warmer tiles get lower pressure; magnitude controlled here.
  , wcPressureTempScale :: !Float
    -- | Coriolis band strength in the pressure field.  Adds latitude-
    -- dependent structure matching Hadley/Ferrel/Polar cells.
  , wcPressureCoriolisScale :: !Float
    -- | Dry-season minimum seasonal multiplier (Model F.2).
  , wcSeasonalBase :: !Float
    -- | Range added to base for wet-season maximum (Model F.2).
    -- Wet-season max = 'wcSeasonalBase' + 'wcSeasonalRange'.
  , wcSeasonalRange :: !Float
    -- | Amplitude of noise perturbation on relative humidity (Model F.1).
  , wcHumidityNoiseScale :: !Float
    -- | Amplitude of multiplicative precipitation noise (Model F.2).
  , wcPrecipNoiseScale :: !Float
    -- | ITCZ centre latitude in degrees (Model F.2).
    -- Typically derived; base default 0 (equator).
  , wcITCZLatitude :: !Float
    -- | Half-width of the ITCZ precipitation band in degrees (Model F.2).
  , wcITCZWidth :: !Float
    -- | Peak precipitation boost at ITCZ centre (Model F.2).
  , wcITCZPrecipBoost :: !Float
    -- | Strength of humidity → pressure coupling (Model F.3).
    -- Moist air is lighter, lowering surface pressure.
  , wcPressureHumidityScale :: !Float
    -- | Scaling of pressure gradient on wind speed (Model F.4).
  , wcPressureGradientWindScale :: !Float
    -- | Amplitude of multiplicative wind-speed noise (Model F.4).
  , wcWindNoiseScale :: !Float
    -- | Scale of seasonal ITCZ latitude migration (Phase 7.3).
    -- The ITCZ centre moves north\/south by
    -- @migrationScale * axialTilt * sin(seasonPhase)@ degrees over the
    -- year.  Default 0.70 gives ~16 deg migration for Earth tilt.
  , wcITCZMigrationScale :: !Float
    -- | Exponent applied to relative humidity when deriving cloud
    -- fraction (Phase 7.1).  Higher values suppress cloud formation
    -- except at very high humidity.  Default 1.50.
  , wcCloudRHExponent :: !Float
    -- | Strength of cloud-albedo cooling (Phase 7.1).  Each unit of
    -- cloud fraction reduces effective temperature by this factor.
    -- Default 0.08.
  , wcCloudAlbedoEffect :: !Float
    -- | Strength of cloud-precipitation enhancement (Phase 7.1).
    -- Higher cloud fraction increases precipitation probability.
    -- Default 0.12.
  , wcCloudPrecipBoost :: !Float
  } deriving (Eq, Show)

-- | Default weather configuration.
defaultWeatherConfig :: WeatherConfig
defaultWeatherConfig = WeatherConfig
  { wcTickSeconds              = 1
  , wcSeasonPhase              = 0
  , wcSeasonAmplitude          = 0.30
  , wcSeasonCycleLength        = 365
  , wcJitterAmplitude          = 0.18
  , wcPressureBase             = 0.7
  , wcPressureTempScale        = 0.4
  , wcPressureCoriolisScale    = 0.1
  , wcSeasonalBase             = 0.40
  , wcSeasonalRange            = 1.20
  , wcHumidityNoiseScale       = 0.10
  , wcPrecipNoiseScale         = 0.15
  , wcITCZLatitude             = 0
  , wcITCZWidth                = 10.0
  , wcITCZPrecipBoost          = 0.30
  , wcPressureHumidityScale    = 0.10
  , wcPressureGradientWindScale = 0.30
  , wcWindNoiseScale           = 0.10
  , wcITCZMigrationScale       = 0.70
  , wcCloudRHExponent          = 1.50
  , wcCloudAlbedoEffect        = 0.08
  , wcCloudPrecipBoost         = 0.12
  }

-- ---------------------------------------------------------------------------
-- Pipeline stage
-- ---------------------------------------------------------------------------

-- | Update per-chunk weather snapshots from climate.
--
-- 1. Increments 'twWorldTime' by 'wcTickSeconds'.
-- 2. Computes a dynamic season phase from world time.
-- 3. Scales seasonal amplitude by latitude (maximal at poles, ~zero at
--    equator) and by axial tilt.
-- 4. Uses time-varying noise for jitter so successive ticks differ.
-- 5. Derives pressure from temperature (warm → low, cold → high).
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
        -- 5.1.3: advance world time
        worldTime' = twWorldTime world + wcTickSeconds cfg
        -- 5.5: dynamic season phase = initial offset + time-derived angle
        cycleLen   = max 1 (wcSeasonCycleLength cfg)
        dynamicPhase = wcSeasonPhase cfg
                     + worldTime' * 2 * pi / cycleLen
        -- 5.3.1: time hash for noise variation
        timeHash = floor (worldTime' / max 0.001 (wcTickSeconds cfg)) :: Int
        -- 7.3: Seasonal ITCZ migration — move convergence zone with
        -- the sub-solar point.
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
        weather' = IntMap.mapWithKey
          (buildWeatherChunk config seed cfg' radPerTile latBiasRad timeHash)
          (twClimate world)
    in world { twWeather = weather', twWorldTime = worldTime' }

-- ---------------------------------------------------------------------------
-- Per-chunk weather builder
-- ---------------------------------------------------------------------------

buildWeatherChunk
  :: WorldConfig -> Word64 -> WeatherConfig
  -> Float -> Float -> Int
  -> Int -> ClimateChunk -> WeatherChunk
buildWeatherChunk config seed cfg radPerTile latBiasRad timeHash key climate =
  let origin = chunkOriginTile config (chunkCoordFromId (ChunkId key))
      n = chunkTileCount config
      -- Pre-cloud temperature (seasonal + jitter, before cloud albedo)
      tempRaw = U.generate n
        (weatherTempAt config seed cfg radPerTile latBiasRad timeHash origin
           (ccTempAvg climate))
      humidity = U.generate n
        (weatherHumidityAt config seed cfg timeHash origin
           (ccPrecipAvg climate) tempRaw)
      -- 7.1: Cloud fraction derived from relative humidity
      cloudFracV = U.generate n (\i ->
        cloudFraction (wcCloudRHExponent cfg) (humidity U.! i))
      -- Cloud-albedo cooling: overcast skies lower the effective
      -- surface temperature (reduced incoming radiation).
      temp = U.generate n (\i ->
        let t  = tempRaw U.! i
            cf = cloudFracV U.! i
        in clamp01 (t * (1 - wcCloudAlbedoEffect cfg * cf)))
      windDir = U.generate n
        (weatherWindDirAt config seed timeHash origin (ccWindDirAvg climate))
      -- Model F.3: pressure derived from temperature + humidity
      pressure = U.generate n
        (weatherPressureAt config seed cfg radPerTile latBiasRad timeHash origin temp humidity)
      -- Model F.4: wind speed driven by pressure gradient
      windSpd = U.generate n
        (weatherWindSpdAt config seed cfg timeHash origin (ccWindSpdAvg climate) pressure)
      precipRaw = U.generate n
        (weatherPrecipAt config seed cfg radPerTile latBiasRad timeHash origin
           (ccPrecipAvg climate))
      -- 7.1: Cloud-precipitation boost — more clouds -> more
      -- condensation -> more rain.
      precip = U.generate n (\i ->
        let p  = precipRaw U.! i
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

-- ---------------------------------------------------------------------------
-- Per-tile weather functions
-- ---------------------------------------------------------------------------

-- | 5.2 + 5.3: Temperature with latitude-dependent seasonal amplitude
-- and time-varying jitter.
weatherTempAt
  :: WorldConfig -> Word64 -> WeatherConfig
  -> Float -> Float -> Int -> TileCoord
  -> U.Vector Float -> Int -> Float
weatherTempAt config seed cfg radPerTile latBiasRad timeHash origin climate i =
  let TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
      latRad = fromIntegral (oy + ly) * radPerTile + latBiasRad
      -- 5.2.1: seasonal amplitude scaled by |sin(lat)| — maximal at
      -- poles, near-zero at equator.
      latScale = abs (sin latRad)
      seasonal = sin (wcSeasonPhase cfg + latRad)
               * wcSeasonAmplitude cfg * latScale
      -- 5.3.1: time-varying noise via seed offset from timeHash
      timeSeed = seed + fromIntegral timeHash
      n0 = noise2D timeSeed (ox + lx + 5000) (oy + ly + 5000)
      jitter = (n0 * 2 - 1) * wcJitterAmplitude cfg
  in clamp01 (climate U.! i + jitter + seasonal)

-- | Model F.1: Saturation-based relative humidity.
--
-- @RH = clamp01(climatePrecip / satNorm(T)) * (1 + noise * k)@
--
-- Hot deserts get low RH even with some moisture; cool foggy coasts
-- get high RH even with moderate moisture.
weatherHumidityAt
  :: WorldConfig -> Word64 -> WeatherConfig -> Int -> TileCoord
  -> U.Vector Float -> U.Vector Float -> Int -> Float
weatherHumidityAt config seed cfg timeHash origin climatePrecip tempVec i =
  let TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
      timeSeed = seed + fromIntegral timeHash
      n0 = noise2D timeSeed (ox + lx + 6000) (oy + ly + 6000)
      tVal = tempVec U.! i
      sat  = max 0.001 (satNorm tVal)
      rh   = clamp01 ((climatePrecip U.! i) / sat)
      noiseMult = 1 + (n0 * 2 - 1) * wcHumidityNoiseScale cfg
  in clamp01 (rh * noiseMult)

-- | 5.3.3: Wind direction with time-varying noise.
weatherWindDirAt
  :: WorldConfig -> Word64 -> Int -> TileCoord
  -> U.Vector Float -> Int -> Float
weatherWindDirAt config seed timeHash origin climate i =
  let TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
      timeSeed = seed + fromIntegral timeHash
      n0 = noise2D timeSeed (ox + lx + 7000) (oy + ly + 7000)
  in climate U.! i + n0 * 0.2

-- | Model F.4: Wind speed driven by pressure gradient.
--
-- @W_weather = W_climate * (1 + k_pressure * |nabla P|)
--            * (1 + noise * k_noise)@
--
-- |∇P| is approximated from the 4 axial neighbours in the chunk grid.
-- Wind accelerates where pressure gradients are steep (storm systems,
-- fronts) and calms where pressure is uniform.
weatherWindSpdAt
  :: WorldConfig -> Word64 -> WeatherConfig -> Int -> TileCoord
  -> U.Vector Float -> U.Vector Float -> Int -> Float
weatherWindSpdAt config seed cfg timeHash origin climateWind pressureVec i =
  let TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
      timeSeed = seed + fromIntegral timeHash
      n0 = noise2D timeSeed (ox + lx + 8000) (oy + ly + 8000)
      cs = wcChunkSize config
      -- Pressure at tile (x,y) in the chunk grid
      pAt x y
        | x < 0 || x >= cs || y < 0 || y >= cs = pressureVec U.! i
        | otherwise = pressureVec U.! (y * cs + x)
      pCenter = pressureVec U.! i
      -- Finite-difference gradient (one-sided at edges)
      dPdx
        | lx <= 0       = pAt (lx + 1) ly - pCenter
        | lx >= cs - 1  = pCenter - pAt (lx - 1) ly
        | otherwise      = (pAt (lx + 1) ly - pAt (lx - 1) ly) * 0.5
      dPdy
        | ly <= 0       = pAt lx (ly + 1) - pCenter
        | ly >= cs - 1  = pCenter - pAt lx (ly - 1)
        | otherwise      = (pAt lx (ly + 1) - pAt lx (ly - 1)) * 0.5
      gradMag = sqrt (dPdx * dPdx + dPdy * dPdy)
      noiseMult = 1.0 + (n0 * 2 - 1) * wcWindNoiseScale cfg
      baseWind = (climateWind U.! i)
               * (1.0 + wcPressureGradientWindScale cfg * gradMag)
  in clamp01 (baseWind * noiseMult)

-- | Model F.3: Pressure from temperature + humidity.
--
-- @P = P_base - T * k_T - RH * k_RH + coriolis(lat) + noise@
--
-- Humidity makes air lighter (lower pressure), enhancing convection
-- in moist regions.
weatherPressureAt
  :: WorldConfig -> Word64 -> WeatherConfig
  -> Float -> Float -> Int -> TileCoord
  -> U.Vector Float -> U.Vector Float -> Int -> Float
weatherPressureAt config seed cfg radPerTile latBiasRad timeHash origin tempVec humVec i =
  let TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
      timeSeed = seed + fromIntegral timeHash
      n0 = noise2D timeSeed (ox + lx + 9000) (oy + ly + 9000)
      latRad = fromIntegral (oy + ly) * radPerTile + latBiasRad
      tempVal = tempVec U.! i
      humVal  = humVec U.! i
      -- Temperature-derived component: warm = low pressure
      -- Humidity component: moist air = lower pressure
      tempPressure = wcPressureBase cfg
                   - tempVal * wcPressureTempScale cfg
                   - humVal  * wcPressureHumidityScale cfg
      -- Coriolis band structure: subtropical highs ~30deg, lows at
      -- equator and ~60deg.  cos(3lat) approximates this pattern.
      coriolis = cos (3 * latRad) * wcPressureCoriolisScale cfg
      noiseVal = (n0 * 2 - 1) * 0.05
  in clamp01 (tempPressure + coriolis + noiseVal)

-- | Model F.2: Physics-based weather precipitation.
--
-- @P_weather = P_climate * seasonalFactor * convergenceFactor
--            + noise * precipNoiseScale * P_climate@
--
-- - seasonalFactor ∈ [base, base+range] (default [0.4, 1.6])
-- - convergenceFactor = 1 + ITCZ Gaussian boost near equator
-- - Noise is multiplicative on climate precip (dry stays dry)
weatherPrecipAt
  :: WorldConfig -> Word64 -> WeatherConfig
  -> Float -> Float -> Int -> TileCoord
  -> U.Vector Float -> Int -> Float
weatherPrecipAt config seed cfg radPerTile latBiasRad timeHash origin climate i =
  let TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
      timeSeed = seed + fromIntegral timeHash
      n0 = noise2D timeSeed (ox + lx + 10000) (oy + ly + 10000)
      latRad = fromIntegral (oy + ly) * radPerTile + latBiasRad
      latDeg = latRad * (180.0 / pi)
      -- Seasonal factor: ranges from base to base+range.
      -- |sin(lat)| scales seasonality (poles maximal, equator ~0).
      latScale = abs (sin latRad)
      rawSeasonal = (sin (wcSeasonPhase cfg + latRad) * latScale + 1) * 0.5
      seasonalFactor = wcSeasonalBase cfg
                     + rawSeasonal * wcSeasonalRange cfg
      -- ITCZ convergence: Gaussian boost centred at wcITCZLatitude.
      dLat = latDeg - wcITCZLatitude cfg
      itczW = max 0.1 (wcITCZWidth cfg)
      convergenceFactor = 1.0 + wcITCZPrecipBoost cfg
                        * exp (negate (dLat * dLat) / (2 * itczW * itczW))
      -- Multiplicative noise on climate precip: dry tiles stay dry.
      noiseMult = (n0 * 2 - 1) * wcPrecipNoiseScale cfg
      baseP = (climate U.! i) * seasonalFactor * convergenceFactor
  in clamp01 (baseP + noiseMult * (climate U.! i))

-- ---------------------------------------------------------------------------
-- Exported pure helpers
-- ---------------------------------------------------------------------------

-- | Cloud fraction from relative humidity (Phase 7.1).
--
-- @cloudFraction exponent rh = clamp01 (rh ** exponent)@
--
-- With the default exponent of 1.5 humid tiles (RH > 0.7) produce
-- substantial cloud cover while dry tiles remain clear.
{-# INLINE cloudFraction #-}
cloudFraction :: Float -> Float -> Float
cloudFraction rhExp rh = clamp01 (rh ** rhExp)

-- | Dynamic ITCZ latitude from base position, migration scale, axial
-- tilt, and current season phase (Phase 7.3).
--
-- @seasonalITCZLatitude base scale tilt phase
--     = base + scale * tilt * sin(phase)@
--
-- With Earth defaults (base = 0, scale = 0.7, tilt = 23.44) this
-- oscillates between roughly +16 deg and -16 deg over the year.
{-# INLINE seasonalITCZLatitude #-}
seasonalITCZLatitude
  :: Float  -- ^ base ITCZ latitude (degrees)
  -> Float  -- ^ migration scale (0-1)
  -> Float  -- ^ axial tilt (degrees)
  -> Float  -- ^ season phase (radians)
  -> Float  -- ^ dynamic ITCZ latitude (degrees)
seasonalITCZLatitude baseLat migScale tilt phase =
  baseLat + migScale * tilt * sin phase
