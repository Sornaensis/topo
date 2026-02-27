{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Weather tick configuration and update stage.
--
-- Each call to 'tickWeatherStage' advances 'twWorldTime' by one tick
-- and recomputes per-chunk weather snapshots with time-varying noise,
-- latitude-dependent seasonal amplitude, and temperature-derived
-- pressure.  Seasonal phase is derived from
-- 'Topo.Calendar.yearFraction'.
module Topo.Weather
  ( WeatherConfig(..)
  , defaultWeatherConfig
  , tickWeatherStage
  -- * Simulation node
  , weatherSimNode
  , weatherOverlaySchema
  -- * Overlay conversion
  , weatherChunkToOverlay
  , overlayToWeatherChunk
  , getWeatherFromOverlay
  , getWeatherChunk
  , weatherFieldCount
  -- * Pure helpers (exported for testing)
  , cloudFraction
  , seasonalITCZLatitude
  ) where

import Control.Monad.Reader (asks)
import Data.Aeson (Value(..))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word (Word64)
import GHC.Generics (Generic)
import Topo.Calendar (WorldTime(..), CalendarConfig, yearFraction, mkCalendarConfig, advanceTicks)
import Topo.Climate.Evaporation (satNorm)
import Topo.Config.JSON
  (ToJSON(..), FromJSON(..), configOptions, mergeDefaults,
   genericToJSON, genericParseJSON)
import Topo.Math (clamp01)
import Topo.Noise (noise2D)
import Topo.Overlay (Overlay(..), OverlayData(..), insertOverlay, lookupOverlay)
import Topo.Overlay.Schema
  ( OverlaySchema(..), OverlayFieldDef(..), OverlayFieldType(..)
  , OverlayStorage(..), OverlayDeps(..)
  )
import Topo.Pipeline (PipelineStage(..))
import Topo.Pipeline.Stage (StageId(..))
import Topo.Planet (PlanetConfig(..), LatitudeMapping(..))
import Topo.Plugin (logInfo, modifyWorldP, peSeed)
import Topo.Simulation (SimNode(..), SimNodeId(..), SimContext(..))
import Topo.Types
import Topo.World (TerrainWorld(..))

-- | Weather update configuration.
data WeatherConfig = WeatherConfig
  { -- | Simulation seconds per weather tick.  Also used as the
    -- 'wtTickRate' when initialising 'WorldTime'.
    wcTickSeconds :: !Float
    -- | Initial seasonal phase offset (radians).  Combined with the
    -- dynamic phase from 'Topo.Calendar.yearFraction'.
  , wcSeasonPhase :: !Float
    -- | Base seasonal temperature amplitude before latitude scaling
    -- and tilt scaling.
  , wcSeasonAmplitude :: !Float
    -- | Number of weather ticks in a full seasonal (year) cycle.
    -- Used as a fallback when 'CalendarConfig' is not available.
    -- When the calendar is active, the cycle length is derived from
    -- 'ccDaysPerYear' instead.
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
  } deriving (Eq, Show, Generic)

instance ToJSON WeatherConfig where
  toJSON = genericToJSON (configOptions "wc")

instance FromJSON WeatherConfig where
  parseJSON v = genericParseJSON (configOptions "wc")
                  (mergeDefaults (toJSON defaultWeatherConfig) v)

-- | Default weather configuration.
defaultWeatherConfig :: WeatherConfig
defaultWeatherConfig = WeatherConfig
  { wcTickSeconds              = 1
  , wcSeasonPhase              = 0
  , wcSeasonAmplitude          = 0.21
  , wcSeasonCycleLength        = 365
  , wcJitterAmplitude          = 0.126
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
-- 1. Advances 'twWorldTime' by one tick.
-- 2. Computes a dynamic season phase from 'yearFraction'.
-- 3. Scales seasonal amplitude by latitude (maximal at poles, ~zero at
--    equator) and by axial tilt.
-- 4. Uses time-varying noise for jitter so successive ticks differ.
-- 5. Derives pressure from temperature (warm → low, cold → high).
--
-- Weather data is stored exclusively in the weather 'Overlay'
-- (in 'twOverlays').  The overlay uses 'weatherChunkToOverlay'
-- to convert each 'WeatherChunk' to a dense SoA layout.
-- Use 'getWeatherFromOverlay' or 'getWeatherChunk' to read it back.
tickWeatherStage :: WeatherConfig -> PipelineStage
tickWeatherStage cfg = PipelineStage StageWeather "tickWeather" "tickWeather" Nothing [] Nothing $ do
  logInfo "tickWeather: updating weather"
  seed <- asks peSeed
  modifyWorldP $ \world ->
    let config = twConfig world
        planet = twPlanet world
        lm     = twLatMapping world
        radPerTile = lmRadPerTile lm
        latBiasRad = lmBiasRad lm
        tiltScale  = lmTiltScale lm
        -- Advance world time by one tick
        worldTime' = advanceTicks 1 (twWorldTime world)
        -- Derive seasonal phase from calendar year fraction
        calCfg = mkCalendarConfig planet
        yf     = yearFraction calCfg worldTime'
        dynamicPhase = wcSeasonPhase cfg + realToFrac yf * 2 * pi
        -- Time hash for noise variation (tick count)
        timeHash = fromIntegral (wtTick worldTime') :: Int
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
        -- Weather data lives exclusively in the overlay
        overlayChunks = IntMap.map weatherChunkToOverlay weather'
        weatherOverlay = Overlay
          { ovSchema = weatherOverlaySchema
          , ovData   = DenseData overlayChunks
          }
        overlays' = insertOverlay weatherOverlay (twOverlays world)
    in world { twWorldTime = worldTime'
             , twOverlays  = overlays'
             }

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

-- ---------------------------------------------------------------------------
-- Weather overlay schema
-- ---------------------------------------------------------------------------

-- | Field index constants for the weather overlay (dense SoA layout).
--
-- These must match the order of fields in 'weatherOverlaySchema'.
weatherFieldTemperature, weatherFieldHumidity, weatherFieldWindDir,
  weatherFieldWindSpeed, weatherFieldPressure, weatherFieldPrecip :: Int
weatherFieldTemperature = 0
weatherFieldHumidity    = 1
weatherFieldWindDir     = 2
weatherFieldWindSpeed   = 3
weatherFieldPressure    = 4
weatherFieldPrecip      = 5

-- | Number of fields in the weather overlay schema.
weatherFieldCount :: Int
weatherFieldCount = 6

-- | The canonical schema for the weather overlay.
--
-- Six dense float fields matching 'WeatherChunk':
--
-- @[temperature, humidity, wind_dir, wind_speed, pressure, precipitation]@
--
-- Storage is dense (SoA): every hex in every chunk is populated with
-- one @U.Vector Float@ per field.
weatherOverlaySchema :: OverlaySchema
weatherOverlaySchema = OverlaySchema
  { osName         = "weather"
  , osVersion      = "1.0.0"
  , osDescription  = "Per-tick weather snapshot (temperature, humidity, wind, pressure, precipitation)"
  , osFields       = fields
  , osStorage      = StorageDense
  , osDependencies = OverlayDeps { odTerrain = True, odOverlays = [] }
  , osFieldIndex   = Map.fromList [(ofdName f, i) | (i, f) <- zip [0..] fields]
  }
  where
    fields =
      [ OverlayFieldDef "temperature"   OFFloat (Number 0.5)  False Nothing
      , OverlayFieldDef "humidity"      OFFloat (Number 0.5)  False Nothing
      , OverlayFieldDef "wind_dir"      OFFloat (Number 0.0)  False Nothing
      , OverlayFieldDef "wind_speed"    OFFloat (Number 0.0)  False Nothing
      , OverlayFieldDef "pressure"      OFFloat (Number 0.5)  False Nothing
      , OverlayFieldDef "precipitation" OFFloat (Number 0.0)  False Nothing
      ]

-- ---------------------------------------------------------------------------
-- Weather simulation node
-- ---------------------------------------------------------------------------

-- | Convert a 'WeatherChunk' to a dense overlay chunk (SoA layout).
--
-- The returned 'Vector' has exactly 'weatherFieldCount' elements,
-- one @U.Vector Float@ per field in schema order.
weatherChunkToOverlay :: WeatherChunk -> Vector (U.Vector Float)
weatherChunkToOverlay wc = V.fromList
  [ wcTemp wc
  , wcHumidity wc
  , wcWindDir wc
  , wcWindSpd wc
  , wcPressure wc
  , wcPrecip wc
  ]

-- | Convert a dense overlay chunk back to a 'WeatherChunk'.
--
-- Expects exactly 'weatherFieldCount' field vectors.  Returns
-- 'Nothing' if the vector has the wrong length.
overlayToWeatherChunk :: Vector (U.Vector Float) -> Maybe WeatherChunk
overlayToWeatherChunk v
  | V.length v /= weatherFieldCount = Nothing
  | otherwise = Just WeatherChunk
      { wcTemp     = v V.! weatherFieldTemperature
      , wcHumidity = v V.! weatherFieldHumidity
      , wcWindDir  = v V.! weatherFieldWindDir
      , wcWindSpd  = v V.! weatherFieldWindSpeed
      , wcPressure = v V.! weatherFieldPressure
      , wcPrecip   = v V.! weatherFieldPrecip
      }

-- | Extract the current weather data from the overlay store.
--
-- Reads the @\"weather\"@ overlay from 'twOverlays' and converts each
-- dense chunk back to a 'WeatherChunk'.  Returns 'IntMap.empty' when
-- no weather overlay exists or the overlay uses a non-dense layout.
--
-- This is the canonical way to read weather data after Phase 8B.
-- All consumers should use this instead of the former @twWeather@ field.
getWeatherFromOverlay :: TerrainWorld -> IntMap.IntMap WeatherChunk
getWeatherFromOverlay world =
  case lookupOverlay "weather" (twOverlays world) of
    Nothing -> IntMap.empty
    Just ov -> case ovData ov of
      DenseData m  -> IntMap.mapMaybe overlayToWeatherChunk m
      SparseData _ -> IntMap.empty

-- | Look up a single weather chunk by 'ChunkId' from the overlay store.
--
-- Convenience wrapper around 'getWeatherFromOverlay'.
getWeatherChunk :: ChunkId -> TerrainWorld -> Maybe WeatherChunk
getWeatherChunk (ChunkId cid) world =
  IntMap.lookup cid (getWeatherFromOverlay world)

-- | Simulation node for the weather overlay.
--
-- This is a 'SimNodeReader' — it reads 'TerrainWorld' data (config,
-- planet, latitude mapping, climate chunks) and produces an updated
-- weather overlay.  It does not mutate terrain.
--
-- The tick function:
--
-- 1. Reads the current world time from 'SimContext'.
-- 2. Derives seasonal phase via 'yearFraction'.
-- 3. For each climate chunk, runs 'buildWeatherChunk' (the same
--    per-chunk logic used by 'tickWeatherStage').
-- 4. Packs results as a dense overlay (@DenseData@).
--
-- Unlike 'tickWeatherStage', this node does __not__ advance
-- 'twWorldTime' — the simulation executor handles time management.
weatherSimNode :: WeatherConfig -> SimNode
weatherSimNode cfg = SimNodeReader
  { snrId           = SimNodeId "weather"
  , snrOverlayName  = "weather"
  , snrDependencies = []
  , snrReadTick     = weatherTick cfg
  }

-- | The weather tick implementation.
weatherTick :: WeatherConfig -> SimContext -> Overlay -> IO (Either Text Overlay)
weatherTick cfg ctx _overlay = do
  let terrain    = scTerrain ctx
      wtime      = scWorldTime ctx
      config     = twConfig terrain
      planet     = twPlanet terrain
      lm         = twLatMapping terrain
      radPerTile = lmRadPerTile lm
      latBiasRad = lmBiasRad lm
      tiltScale  = lmTiltScale lm

      -- Use the tick count as a deterministic noise seed.
      -- In the generator pipeline, the seed comes from peSeed;
      -- for simulation ticks the tick counter provides equivalent
      -- per-invocation variation.
      seed       = wtTick wtime

      -- Derive seasonal phase from calendar year fraction
      calCfg       = mkCalendarConfig planet
      yf           = yearFraction calCfg wtime
      dynamicPhase = wcSeasonPhase cfg + realToFrac yf * 2 * pi

      -- Time hash for noise variation (tick count)
      timeHash     = fromIntegral (wtTick wtime) :: Int

      -- Seasonal ITCZ migration
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

      -- Build weather for each climate chunk and pack as dense overlay
      denseChunks = IntMap.mapWithKey
        (\key climate ->
          weatherChunkToOverlay $
            buildWeatherChunk config seed cfg' radPerTile latBiasRad timeHash key climate
        )
        (twClimate terrain)

  pure $ Right Overlay
    { ovSchema = weatherOverlaySchema
    , ovData   = DenseData denseChunks
    }
