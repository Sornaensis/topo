{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Climate generation and precipitation fields.
--
-- The pipeline-facing entry point is 'generateClimateStage'.  All
-- configuration knobs live in 'ClimateConfig' and its five
-- sub-configs (re-exported from "Topo.Climate.Config").
--
-- Physics-based evaporation models are provided by
-- "Topo.Climate.Evaporation" (re-exported).
module Topo.Climate
  ( -- * Re-exported configuration types
    module Topo.Climate.Config
    -- * Re-exported evaporation models
  , module Topo.Climate.Evaporation
    -- * Pipeline stage
  , generateClimateStage
    -- * Moisture transport internals (for testing)
  , moistureTransportAccum
  , moistureStepGrid
  , moistureFlowAtGrid
  ) where

import Control.Monad.Reader (asks)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Word (Word64)
import Topo.Climate.Config
import Topo.Climate.Evaporation
import Topo.Math (clamp01, clampLat, iterateN, lerp)
import Topo.Noise (noise2D)
import Topo.Pipeline (PipelineStage(..))
import Control.Monad.Except (throwError)
import Topo.Planet (LatitudeMapping(..))
import Topo.Plugin (logInfo, getWorldP, putWorldP, peSeed, PluginError(..))
import Topo.TerrainGrid
  ( buildElevationGrid
  , buildPlateBoundaryGrid
  , chunkGridSlice
  , clampCoordGrid
  , validateTerrainGrid
  )
import Topo.Types
import Topo.Weather (WeatherConfig(..))
import Topo.World (TerrainWorld(..))
import qualified Data.Vector.Unboxed as U

-- | Generate climate chunks using the current terrain and water level.
--
-- Latitude mapping is read from 'TerrainWorld'.  Seasonality parameters
-- ('wcSeasonAmplitude', 'wcSeasonalBase', 'wcSeasonalRange') are read
-- from the 'WeatherConfig' to avoid duplication.
generateClimateStage :: ClimateConfig -> WeatherConfig -> Float -> PipelineStage
generateClimateStage cfg wcfg waterLevel = PipelineStage "generateClimate" "generateClimate" $ do
  logInfo "generateClimate: generating climate"
  seed <- asks peSeed
  world <- getWorldP
  let config = twConfig world
      terrain = twTerrain world
      lm     = twLatMapping world
      -- Tilt-scaled season amplitude
      seasonAmp = wcSeasonAmplitude wcfg * lmTiltScale lm
      sBase     = wcSeasonalBase wcfg
      sRange    = wcSeasonalRange wcfg
  (ChunkCoord minCx minCy, ChunkCoord maxCx maxCy) <-
    case validateTerrainGrid config terrain of
      Left err -> throwError (PluginInvariantError err)
      Right bounds -> pure bounds
  let size = wcChunkSize config
      gridW = (maxCx - minCx + 1) * size
      gridH = (maxCy - minCy + 1) * size
      vegMap = twVegetation world
      (precipGrid, coastalGrid) = buildClimateGrids config seed lm cfg waterLevel terrain vegMap (ChunkCoord minCx minCy) gridW gridH
      climate' = IntMap.mapWithKey (buildClimateChunkWithPrecip config seed lm cfg seasonAmp sBase sRange waterLevel vegMap precipGrid coastalGrid (ChunkCoord minCx minCy) gridW) terrain
  putWorldP world { twClimate = climate' }

-- | Build a single chunk's climate data using global precipitation and
-- coastal proximity grids.  Temperature, wind, and seasonality fields
-- are computed here; precipitation comes from the pre-computed global
-- moisture-transport grid.
buildClimateChunkWithPrecip
  :: WorldConfig
  -> Word64
  -> LatitudeMapping
  -> ClimateConfig
  -> Float              -- ^ tilt-scaled season amplitude
  -> Float              -- ^ seasonal base
  -> Float              -- ^ seasonal range
  -> Float
  -> IntMap VegetationChunk -- ^ vegetation data for albedo feedback
  -> U.Vector Float   -- ^ global precipitation grid
  -> U.Vector Float   -- ^ global coastal proximity grid
  -> ChunkCoord       -- ^ min chunk coordinate
  -> Int              -- ^ grid width in tiles
  -> Int              -- ^ chunk key
  -> TerrainChunk
  -> ClimateChunk
buildClimateChunkWithPrecip config seed lm cfg seasonAmp sBase sRange waterLevel vegMap precipGrid coastalGrid minCoord gridW key terrain =
  let origin = chunkOriginTile config (chunkCoordFromId (ChunkId key))
      TileCoord _ox oy = origin
      n = chunkTileCount config
      wnd = ccWind cfg
      tmp = ccTemperature cfg

      -- Albedo: fall back to reference (no correction) when chunk is absent.
      albedoRef = tmpAlbedoReference tmp
      albedoVec = case IntMap.lookup key vegMap of
        Nothing  -> U.replicate n albedoRef
        Just veg -> vegAlbedo veg

      -- Base temperature (pre-ocean-moderation)
      temp0 = U.generate n (tempAt config seed lm cfg waterLevel origin (tcElevation terrain) (tcPlateBoundary terrain) (tcPlateHeight terrain) (tcPlateVelX terrain) (tcPlateVelY terrain) albedoVec)

      -- Wind fields
      windDir0 = U.generate n (windDirAt config seed lm cfg origin)
      windSpd0 = U.generate n (windSpdAt config seed lm cfg origin)
      windDir = diffuseField n (windIterations wnd) (windDiffuse wnd) windDir0
      windSpd = diffuseField n (windIterations wnd) (windDiffuse wnd) windSpd0

      -- Global-grid slices for this chunk
      precip  = chunkGridSlice config minCoord gridW precipGrid key
      coastal = chunkGridSlice config minCoord gridW coastalGrid key

      -- Ocean thermal moderation: pull coastal temps toward the
      -- latitude-dependent SST.
      moderation = tmpOceanModeration tmp
      temp = U.generate n $ \i ->
        let t = temp0 U.! i
            c = coastal U.! i
            TileCoord _lx ly = tileCoordFromIndex config (TileIndex i)
            gy = oy + ly
            lat = clampLat (fromIntegral gy * lmRadPerTile lm + lmBiasRad lm)
            latF = clamp01 (abs (cos lat) ** tmpOceanLatExponent tmp)
            moderateTarget = lerp (tmpOceanPoleSST tmp) (tmpOceanEquatorSST tmp) latF
                           + tmpOceanModerateTemp tmp
            pull = c * moderation * (moderateTarget - t)
        in clamp01 (t + pull)

      -- Seasonality fields with terrain-aware modulations
      seasonCfg = ccSeasonality cfg
      amp       = seasonAmp
      soilMoist = tcMoisture terrain
      humidityAvg   = climateHumidityAvg (ccMoisture cfg) seasonCfg precip temp coastal soilMoist n
      tempRange     = climateTempRange config lm seasonCfg amp origin temp coastal n
      precipSeason  = climatePrecipSeasonality config lm seasonCfg sBase sRange origin coastal (tcElevation terrain) windDir n

  in ClimateChunk
      { ccTempAvg            = temp
      , ccPrecipAvg          = precip
      , ccWindDirAvg         = windDir
      , ccWindSpdAvg         = windSpd
      , ccHumidityAvg        = humidityAvg
      , ccTempRange          = tempRange
      , ccPrecipSeasonality  = precipSeason
      }

-- ---------------------------------------------------------------------------
-- Seasonality helper functions (Phase 2)
-- ---------------------------------------------------------------------------

-- | Annual-average relative humidity from precipitation, temperature,
-- coastal proximity, and soil moisture.
--
-- Base formula: @RH = clamp01(precip / satNorm(temp))@
--
-- Phase 2 additions:
--
--   * __Coastal boost__: @+ coastalProximity × scHumidityCoastalBoost@
--     — ocean vapour advection raises humidity for maritime tiles.
--   * __Soil moisture__: @+ soilMoisture × scHumiditySoilContribution@
--     — wet soils (from hydrology) contribute humidity independent of
--     current-cycle precipitation.
--
-- Together these break humidity's pure dependence on T×P, giving the
-- biome classifier a genuine third axis.
climateHumidityAvg
  :: MoistureConfig
  -> SeasonalityConfig
  -> U.Vector Float    -- ^ precipitation
  -> U.Vector Float    -- ^ temperature
  -> U.Vector Float    -- ^ coastal proximity (0 = interior, 1 = ocean)
  -> U.Vector Float    -- ^ soil moisture (from hydrology tcMoisture)
  -> Int
  -> U.Vector Float
climateHumidityAvg mst seasonCfg precip temp coastal soilMoist n =
  let coastBoost = scHumidityCoastalBoost seasonCfg
      soilContrib = scHumiditySoilContribution seasonCfg
  in U.generate n $ \i ->
    let t = temp U.! i
        p = precip U.! i
        sat = max 0.001 (satNormCfg mst t)
        baseRH = p / sat
        c = coastal U.! i
        s = soilMoist U.! i
    in clamp01 (baseRH + c * coastBoost + s * soilContrib)

-- | Annual temperature range from seasonal amplitude and coastal proximity.
--
-- Base formula:
-- @tempRange = clamp01(tempAvg + amp × |sin lat|)
--            - clamp01(tempAvg - amp × |sin lat|)@
--
-- Phase 2 addition:
--
--   * __Ocean damping__: @tempRange *= 1 - scTempRangeOceanDamping × coastalProximity@
--     — maritime tiles have moderated annual temperature range (oceanic\n--     thermal inertia), while continental interiors retain full range.
--     This breaks the pure latitude dependence of continentality.
climateTempRange
  :: WorldConfig -> LatitudeMapping -> SeasonalityConfig -> Float
  -> TileCoord -> U.Vector Float -> U.Vector Float -> Int -> U.Vector Float
climateTempRange config lm seasonCfg amp origin temp coastal n =
  let TileCoord _ox oy = origin
      damping = scTempRangeOceanDamping seasonCfg
  in U.generate n $ \i ->
    let TileCoord _lx ly = tileCoordFromIndex config (TileIndex i)
        gy = oy + ly
        latRad = clampLat (fromIntegral gy * lmRadPerTile lm + lmBiasRad lm)
        latScale = abs (sin latRad)
        t = temp U.! i
        tHigh = clamp01 (t + amp * latScale)
        tLow  = clamp01 (t - amp * latScale)
        baseRange = tHigh - tLow
        c = coastal U.! i
    in clamp01 (baseRange * (1 - damping * c))

-- | Precipitation seasonality index, modulated by terrain.
--
-- Base formula (latitude-only):
--
-- @
-- rawMax = (|sin lat| + 1) × 0.5
-- rawMin = (1 - |sin lat|) × 0.5
-- maxFactor = seasonalBase + rawMax × seasonalRange
-- minFactor = seasonalBase + rawMin × seasonalRange
-- baseSeason = 1 - minFactor / max 0.001 maxFactor
-- @
--
-- Phase 2 terrain modulations:
--
--   * __Continentality amplification__:
--     @baseSeason *= 1 + scSeasonalityContinentalityFactor × (1 - coastalProx)@
--     — interior tiles have amplified seasonality; coastal tiles stay moderate.
--
--   * __Rain-shadow boost__:
--     @baseSeason += scSeasonalityRainShadowBoost × max 0 (upwindElev - localElev)@
--     — leeward tiles receive a seasonality boost because orographic rain
--     falls primarily in the wet season.
--
-- Range: 0 (uniform year-round) to ~1 (extreme seasonality).
climatePrecipSeasonality
  :: WorldConfig -> LatitudeMapping -> SeasonalityConfig
  -> Float              -- ^ seasonal base
  -> Float              -- ^ seasonal range
  -> TileCoord
  -> U.Vector Float    -- ^ coastal proximity
  -> U.Vector Float    -- ^ elevation
  -> U.Vector Float    -- ^ wind direction (radians)
  -> Int
  -> U.Vector Float
climatePrecipSeasonality config lm seasonCfg sBase sRange origin coastal elev windDir n =
  let TileCoord ox oy = origin
      contFactor = scSeasonalityContinentalityFactor seasonCfg
      rsFactor = scSeasonalityRainShadowBoost seasonCfg
      size = case config of WorldConfig cs -> cs
  in U.generate n $ \i ->
    let TileCoord _lx ly = tileCoordFromIndex config (TileIndex i)
        gx = ox + (i `mod` size)
        gy = oy + ly
        latRad = clampLat (fromIntegral gy * lmRadPerTile lm + lmBiasRad lm)
        latScale = abs (sin latRad)
        rawMax = (latScale + 1) * 0.5
        rawMin = (1 - latScale) * 0.5
        maxF = sBase + rawMax * sRange
        minF = sBase + rawMin * sRange
        baseSeason = 1 - minF / max 0.001 maxF
        -- Continentality: interior tiles amplify seasonality
        c = coastal U.! i
        contModulated = baseSeason * (1 + contFactor * (1 - c))
        -- Rain shadow: sample upwind elevation
        wdir = windDir U.! i
        -- Upwind sample offset (1 tile distance)
        udx = round (cos wdir) :: Int
        udy = round (sin wdir) :: Int
        -- Compute upwind tile index within the chunk (clamp to bounds)
        lx0 = i `mod` size
        ly0 = i `div` size
        ux = max 0 (min (size - 1) (lx0 - udx))
        uy = max 0 (min (size - 1) (ly0 - udy))
        upIdx = uy * size + ux
        upElev = elev U.! upIdx
        localElev = elev U.! i
        rsBias = rsFactor * max 0 (upElev - localElev)
        _ = gx  -- suppress unused warning for gx (kept for clarity)
    in clamp01 (contModulated + rsBias)

tempAt
  :: WorldConfig
  -> Word64
  -> LatitudeMapping
  -> ClimateConfig
  -> Float
  -> TileCoord
  -> U.Vector Float
  -> U.Vector PlateBoundary
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Float          -- ^ Per-tile surface albedo (from vegetation)
  -> Int
  -> Float
tempAt config seed lm cfg waterLevel origin elev boundaries plateHeight velX velY albedoVec i =
  let tmp = ccTemperature cfg
      bnd = ccBoundary cfg
      TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
      gy = oy + ly
      lat = clampLat (fromIntegral gy * lmRadPerTile lm + lmBiasRad lm)
      height = elev U.! i
      isOcean = height < waterLevel
      insol = lmInsolation lm
      -- Ocean tiles: dedicated SST profile (Phase 1 temperature fix).
      -- Land tiles: original latitude curve with lapse rate.
      base
        | isOcean =
            let latF = clamp01 (abs (cos lat) ** tmpOceanLatExponent tmp)
            in insol * lerp (tmpOceanPoleSST tmp) (tmpOceanEquatorSST tmp) latF
        | otherwise =
            let latF = clamp01 (abs (cos lat) ** tmpLatitudeExponent tmp)
            in insol * lerp (tmpPoleTemp tmp) (tmpEquatorTemp tmp) latF
      lapse = if isOcean
              then 0
              else (height - waterLevel) * tmpLapseRate tmp
      n0 = noise2D seed (ox + lx) (oy + ly) * tmpNoiseScale tmp
      tectonic = boundaryTempBiasAt cfg waterLevel (boundaries U.! i) height
      motion = clamp01 (plateVelocityMagAt (velX U.! i) (velY U.! i) * bndMotionTemp bnd)
      plateBias = plateHeightTempBiasAt cfg waterLevel (plateHeight U.! i)
      rawTemp = base - lapse + n0 + tectonic * (1 + motion) + plateBias
      -- Model H: albedo feedback — land only.
      -- Ocean SST profile already accounts for albedo/evaporative balance.
      albedo = albedoVec U.! i
      albedoCorr
        | isOcean   = 1
        | otherwise = 1 - tmpAlbedoSensitivity tmp
                          * (albedo - tmpAlbedoReference tmp)
  in clamp01 (rawTemp * albedoCorr)

boundaryTempBiasAt :: ClimateConfig -> Float -> PlateBoundary -> Float -> Float
boundaryTempBiasAt cfg waterLevel boundary height =
  let bnd = ccBoundary cfg
      landRange = max 0.0001 (bndLandRange bnd)
      land = clamp01 ((height - waterLevel) / landRange)
      bias = case boundary of
        PlateBoundaryConvergent -> bndTempConvergent bnd
        PlateBoundaryDivergent -> bndTempDivergent bnd
        PlateBoundaryTransform -> bndTempTransform bnd
        PlateBoundaryNone -> 0
  in bias * land

windDirAt :: WorldConfig -> Word64 -> LatitudeMapping -> ClimateConfig -> TileCoord -> Int -> Float
windDirAt config seed lm cfg origin i =
  let TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
  in windDirAtXY seed lm cfg (ox + lx) (oy + ly)

windSpdAt :: WorldConfig -> Word64 -> LatitudeMapping -> ClimateConfig -> TileCoord -> Int -> Float
windSpdAt config seed lm cfg origin i =
  let TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
  in windSpdAtXY seed lm cfg (ox + lx) (oy + ly)

moistureTransport :: WorldConfig -> Word64 -> LatitudeMapping -> ClimateConfig -> Float -> TileCoord -> U.Vector Float -> U.Vector Float -> U.Vector Float -> U.Vector Float -> U.Vector Float
moistureTransport config seed lm cfg waterLevel origin windDir windSpd tempVec elev =
  let n = U.length elev
      initial = U.generate n (evapAt config seed lm cfg waterLevel origin elev tempVec windSpd)
  in iterateN (moistIterations (ccMoisture cfg)) (moistureStep config cfg windDir windSpd elev) initial

-- | Build the global precipitation and coastal proximity grids.
--
-- Returns @(precipGrid, coastalGrid)@.  The precipitation grid uses
-- physics-based evaporation: Dalton's-Law ocean evaporation (Model B)
-- and Penman-Monteith-inspired land ET (Model C), both driven by the
-- Clausius-Clapeyron saturation curve.  Moisture is then transported
-- by wind with temperature-dependent condensation.
buildClimateGrids
  :: WorldConfig
  -> Word64
  -> LatitudeMapping
  -> ClimateConfig
  -> Float
  -> IntMap TerrainChunk
  -> IntMap VegetationChunk  -- ^ Vegetation bootstrap data
  -> ChunkCoord
  -> Int -> Int
  -> (U.Vector Float, U.Vector Float)
buildClimateGrids config seed lm cfg waterLevel terrain vegMap (ChunkCoord minCx minCy) gridW gridH =
  let prc = ccPrecipitation cfg
      wnd = ccWind cfg
      mst = ccMoisture cfg
      bnd = ccBoundary cfg
      tmp = ccTemperature cfg
      size = wcChunkSize config
      minTileX = minCx * size
      minTileY = minCy * size
      insol = lmInsolation lm
      elev = buildElevationGrid config terrain (ChunkCoord minCx minCy) gridW gridH
      boundaries = buildPlateBoundaryGrid config terrain (ChunkCoord minCx minCy) gridW gridH
      plateHeight = buildPlateHeightGrid config terrain (ChunkCoord minCx minCy) gridW gridH
      plateVelocity = buildPlateVelocityGrid config terrain (ChunkCoord minCx minCy) gridW gridH
      oceanMask = U.map (\h -> if h < waterLevel then 1 else 0) elev
      coastal = coastalProximityGrid gridW gridH (precCoastalIterations prc) (precCoastalDiffuse prc) oceanMask
      n = gridW * gridH
      -- Soil moisture grid (from hydrology-stage tcMoisture)
      soilMoistGrid = buildFieldGrid config terrain tcMoisture (ChunkCoord minCx minCy) gridW gridH
      -- Vegetation cover grid (from bootstrap stage)
      vegCoverGrid  = buildVegFieldGrid config vegMap vegCover (ChunkCoord minCx minCy) gridW gridH
      -- Vegetation albedo grid (for temperature feedback, Model H)
      vegAlbedoGrid = buildVegFieldGrid config vegMap vegAlbedo (ChunkCoord minCx minCy) gridW gridH
      -- Build a global temperature grid for use in moisture computation.
      -- Includes albedo correction (Model H).
      albedoRef = tmpAlbedoReference tmp
      tempGrid = U.generate n (\i ->
        let _x = i `mod` gridW
            y = i `div` gridW
            gy = minTileY + y
            -- Use vegAlbedo if available, else reference (no correction)
            a = let v = vegAlbedoGrid U.! i
                in if v == 0 then albedoRef else v
        in tempAtXY lm cfg waterLevel gy (elev U.! i)
             (boundaries U.! i) (plateHeight U.! i) (plateVelocity U.! i) a)
      windDir0 = U.generate n (\i ->
        let x = i `mod` gridW
            y = i `div` gridW
            gx = minTileX + x
            gy = minTileY + y
        in windDirAtXY seed lm cfg gx gy)
      windSpd0 = U.generate n (\i ->
        let x = i `mod` gridW
            y = i `div` gridW
            gx = minTileX + x
            gy = minTileY + y
        in windSpdAtXY seed lm cfg gx gy)
      windDir = diffuseFieldGrid gridW gridH (windIterations wnd) (windDiffuse wnd) windDir0
      windSpd = diffuseFieldGrid gridW gridH (windIterations wnd) (windDiffuse wnd) windSpd0
      -- Physics-based initial moisture (Models B + C)
      initial = U.generate n (\i ->
        let x = i `mod` gridW
            y = i `div` gridW
            gx = minTileX + x
            gy = minTileY + y
            h = elev U.! i
            t = tempGrid U.! i
            w = windSpd U.! i
            n0 = noise2D seed (gx + 4000) (gy + 4000)
            noise = n0 * moistEvapNoiseScale mst
        in if h < waterLevel
           then -- Ocean tile: Dalton's Law evaporation (Model B)
             clamp01 (oceanEvaporation mst t w insol + noise)
           else -- Land tile: Penman-Monteith ET (Model C) + coastal boost
             -- Use max of hydrology soil moisture and internal land base
             -- to break the circular dependency (RC-1).
             let soilM = max (soilMoistGrid U.! i) (moistInternalLandBase mst)
                 vegC  = vegCoverGrid U.! i
                 coast = coastal U.! i * precCoastalMoistureBoost prc
             in clamp01 (landEvapotranspiration mst t soilM vegC w + coast + noise))
      -- Moisture transport with physics-based condensation,
      -- per-iteration ET recycling, ocean reinjection, and ITCZ boost.
      --
      -- Precompute ocean evaporation field for reinjection (Model E.4):
      -- each iteration, ocean tiles are held at or above their
      -- physical evaporation rate.
      oceanEvapGrid = U.generate n (\i ->
        let t = tempGrid U.! i
            w = windSpd U.! i
        in if elev U.! i < waterLevel
           then oceanEvaporation mst t w insol
           else 0)
      -- Precompute ITCZ convergence boost (Model E.5):
      -- Gaussian enhancement centered on the equator (lat = 0).
      -- The configured strength is the *total* target enhancement;
      -- divide by iterations so it accumulates correctly.
      itczPerIter = moistITCZStrength mst
                      / max 1 (fromIntegral (moistIterations mst))
      itczBoostGrid = U.generate n (\i ->
        let y = i `div` gridW
            gy = minTileY + y
            lat = clampLat (fromIntegral gy * lmRadPerTile lm + lmBiasRad lm)
            latDeg = lat * (180.0 / pi)
            w = max 0.001 (moistITCZWidth mst)
            d = latDeg / w
        in itczPerIter * exp (negate (d * d)))
      -- Moisture transport with condensation accumulation:
      -- Each iteration yields (newMoisture, netCondensation); the
      -- accumulated condensation across all iterations forms the
      -- physics-based precipitation field.
      (_finalMoisture, accumCondensation) =
        moistureTransportAccum (moistIterations mst)
          (moistureStepGrid gridW gridH cfg windDir windSpd elev tempGrid
            vegCoverGrid oceanEvapGrid oceanMask itczBoostGrid waterLevel)
          initial
      precipGrid = U.generate n (\i ->
        let o = orographicAt gridW gridH cfg windDir elev i
            motion = clamp01 (plateVelocity U.! i * bndMotionPrecip bnd)
            tectonic = boundaryOrogenyAt cfg waterLevel elev boundaries motion i
            plateBias = plateHeightPrecipBiasAt cfg waterLevel (plateHeight U.! i)
            rawPrecip = accumCondensation U.! i + o + tectonic + plateBias
            -- Polar precipitation floor: ramp from 0 at precPolarLatitude
            -- to precPolarFloor at the pole (90°).
            y = i `div` gridW
            gy = minTileY + y
            lat = clampLat (fromIntegral gy * lmRadPerTile lm + lmBiasRad lm)
            latDeg = abs (lat * (180.0 / pi))
            polarLat = max 0.001 (precPolarLatitude prc)
            polarFrac = clamp01 ((latDeg - polarLat) / (90.0 - polarLat))
            polarFloor = precPolarFloor prc * polarFrac
        in clamp01 (max rawPrecip polarFloor))
    in (precipGrid, coastal)

-- | Build a scalar grid from a 'TerrainChunk' field accessor.
--
-- Follows the same tile-lookup pattern as 'buildElevationGrid' but
-- works for any @TerrainChunk -> U.Vector Float@ field.
buildFieldGrid
  :: WorldConfig
  -> IntMap TerrainChunk
  -> (TerrainChunk -> U.Vector Float)  -- ^ Field accessor
  -> ChunkCoord
  -> Int -> Int
  -> U.Vector Float
buildFieldGrid config terrain accessor (ChunkCoord minCx minCy) gridW gridH =
  let size = wcChunkSize config
      minTileX = minCx * size
      minTileY = minCy * size
      sampleAt idx =
        let x = idx `mod` gridW
            y = idx `div` gridW
            gx = minTileX + x
            gy = minTileY + y
            tile = TileCoord gx gy
            (chunkCoord, local) = chunkCoordFromTile config tile
            ChunkId key = chunkIdFromCoord chunkCoord
        in case IntMap.lookup key terrain of
            Nothing -> 0
            Just chunk ->
              case tileIndex config local of
                Nothing -> 0
                Just (TileIndex ti) -> accessor chunk U.! ti
  in U.generate (gridW * gridH) sampleAt

-- | Build a scalar grid from a 'VegetationChunk' field accessor.
--
-- Falls back to @0@ for missing chunks.
buildVegFieldGrid
  :: WorldConfig
  -> IntMap VegetationChunk
  -> (VegetationChunk -> U.Vector Float)  -- ^ Field accessor
  -> ChunkCoord
  -> Int -> Int
  -> U.Vector Float
buildVegFieldGrid config vegMap accessor (ChunkCoord minCx minCy) gridW gridH =
  let size = wcChunkSize config
      minTileX = minCx * size
      minTileY = minCy * size
      sampleAt idx =
        let x = idx `mod` gridW
            y = idx `div` gridW
            gx = minTileX + x
            gy = minTileY + y
            tile = TileCoord gx gy
            (chunkCoord, local) = chunkCoordFromTile config tile
            ChunkId key = chunkIdFromCoord chunkCoord
        in case IntMap.lookup key vegMap of
            Nothing -> 0
            Just chunk ->
              case tileIndex config local of
                Nothing -> 0
                Just (TileIndex ti) -> accessor chunk U.! ti
  in U.generate (gridW * gridH) sampleAt

-- | Simplified temperature at a global Y coordinate, for use in the
-- global moisture grid.  Mirrors 'tempAt' but without per-tile noise
-- (which varies per seed / x-offset) — good enough for moisture/evap
-- interactions where we need the thermal structure, not pixel-level detail.
--
-- Applies albedo correction (Model H) when albedo is available.
tempAtXY :: LatitudeMapping -> ClimateConfig -> Float -> Int -> Float -> PlateBoundary -> Float -> Float -> Float -> Float
tempAtXY lm cfg waterLevel gy height boundary plateHt velocity albedo =
  let tmp = ccTemperature cfg
      bnd = ccBoundary cfg
      lat = clampLat (fromIntegral gy * lmRadPerTile lm + lmBiasRad lm)
      isOcean = height < waterLevel
      insol = lmInsolation lm
      -- Ocean: dedicated SST profile.  Land: original latitude curve.
      base
        | isOcean =
            let latF = clamp01 (abs (cos lat) ** tmpOceanLatExponent tmp)
            in insol * lerp (tmpOceanPoleSST tmp) (tmpOceanEquatorSST tmp) latF
        | otherwise =
            let latF = clamp01 (abs (cos lat) ** tmpLatitudeExponent tmp)
            in insol * lerp (tmpPoleTemp tmp) (tmpEquatorTemp tmp) latF
      lapse = if isOcean
              then 0
              else (height - waterLevel) * tmpLapseRate tmp
      tectonic = boundaryTempBiasRaw cfg waterLevel boundary height
      motion = clamp01 (velocity * bndMotionTemp bnd)
      plateBias = plateHeightTempBiasAt cfg waterLevel plateHt
      rawTemp = base - lapse + tectonic * (1 + motion) + plateBias
      -- Model H: albedo correction — land only.
      albedoCorr
        | isOcean   = 1
        | otherwise = 1 - tmpAlbedoSensitivity tmp
                          * (albedo - tmpAlbedoReference tmp)
  in clamp01 (rawTemp * albedoCorr)

-- | Raw boundary temperature bias (without per-tile noise).
boundaryTempBiasRaw :: ClimateConfig -> Float -> PlateBoundary -> Float -> Float
boundaryTempBiasRaw cfg waterLevel boundary height =
  let bnd = ccBoundary cfg
      landRange = max 0.0001 (bndLandRange bnd)
      land = clamp01 ((height - waterLevel) / landRange)
      bias = case boundary of
        PlateBoundaryConvergent -> bndTempConvergent bnd
        PlateBoundaryDivergent  -> bndTempDivergent bnd
        PlateBoundaryTransform  -> bndTempTransform bnd
        PlateBoundaryNone       -> 0
  in bias * land

windDirAtXY :: Word64 -> LatitudeMapping -> ClimateConfig -> Int -> Int -> Float
windDirAtXY seed lm cfg gx gy =
  let wnd = ccWind cfg
      noiseDir = noise2D seed (gx + 2000) (gy + 2000) * 6.283185
      latRad = fromIntegral gy * lmRadPerTile lm + lmBiasRad lm
      belt = sin (latRad * 2 * windBeltHarmonics wnd)
      beltDir = if belt >= 0 then 0 else pi
  in blendAngle noiseDir beltDir (windBeltStrength wnd)

blendAngle :: Float -> Float -> Float -> Float
blendAngle a b t =
  let t' = clamp01 t
      x = lerp (cos a) (cos b) t'
      y = lerp (sin a) (sin b) t'
  in atan2 y x

windSpdAtXY :: Word64 -> LatitudeMapping -> ClimateConfig -> Int -> Int -> Float
windSpdAtXY seed lm cfg gx gy =
  let wnd = ccWind cfg
      noiseSpd = clamp01 (noise2D seed (gx + 3000) (gy + 3000))
      latRad = fromIntegral gy * lmRadPerTile lm + lmBiasRad lm
      belt = clamp01 (windBeltBase wnd + windBeltRange wnd * abs (sin (latRad * 2 * windBeltHarmonics wnd)))
      strength = clamp01 (windBeltStrength wnd * windBeltSpeedScale wnd)
  in clamp01 (lerp noiseSpd belt strength)

-- | Ocean evaporation at a global tile, using Dalton's Law (Model B).
--
-- Returns the evaporation intensity for a single tile given its
-- temperature and wind speed.  Used by the grid and per-chunk paths.
-- For ocean tiles the result is scaled by CC-based saturation; for
-- land tiles the result is just small noise.
evapAtXY :: Word64 -> LatitudeMapping -> ClimateConfig -> Float -> Int -> Int -> Float -> Float -> Float -> Float
evapAtXY seed lm cfg waterLevel gx gy elevation temp windSpdVal =
  let mst = ccMoisture cfg
      insol = lmInsolation lm
      n0 = noise2D seed (gx + 4000) (gy + 4000)
      noise = n0 * moistEvapNoiseScale mst
  in if elevation < waterLevel
     then clamp01 (oceanEvaporation mst temp windSpdVal insol + noise)
     else clamp01 noise

-- | Run moisture transport for @n@ iterations, accumulating per-tile
-- net condensation (precipitation) across all steps.
--
-- Returns @(finalMoisture, accumulatedPrecipitation)@.  The accumulated
-- precipitation vector sums per-tile net condensation from each
-- iteration, providing a physics-based precipitation field where
-- moisture that condenses (minus ET recycling) contributes to
-- precipitation at the tile where condensation occurs.
--
-- Ocean\/land reinjection and ITCZ boost feed atmospheric moisture
-- only and are /not/ double-counted in the precipitation accumulator.
moistureTransportAccum
  :: Int
  -> (U.Vector Float -> (U.Vector Float, U.Vector Float))
     -- ^ Step function: current moisture -> (new moisture, condensed this step)
  -> U.Vector Float
     -- ^ Initial moisture
  -> (U.Vector Float, U.Vector Float)
     -- ^ (final moisture, accumulated precipitation)
moistureTransportAccum iters step initial =
  go iters initial (U.replicate (U.length initial) 0)
  where
    go 0 moist accum = (moist, accum)
    go !n moist !accum =
      let (moist', condensed) = step moist
          accum' = U.zipWith (+) accum condensed
      in go (n - 1) moist' accum'

-- | Single transport iteration over the global moisture grid.
--
-- Combines:
--
--   * Wind-driven advection + saturation-based condensation (Models E.1/E.2)
--   * Per-iteration ET recycling through vegetation (Model E.3)
--   * Persistent ocean moisture reinjection (Model E.4)
--   * ITCZ convergence moisture boost (Model E.5)
--   * Persistent land ET reinjection (Model E.6)
--
-- Returns @(newMoisture, condensedThisStep)@.  The condensation vector
-- tracks net precipitation (condensation minus recycling) per tile,
-- for accumulation across iterations in 'moistureTransportAccum'.
moistureStepGrid
  :: Int -> Int -> ClimateConfig
  -> U.Vector Float -> U.Vector Float   -- ^ windDir, windSpd
  -> U.Vector Float -> U.Vector Float   -- ^ elev, tempGrid
  -> U.Vector Float                      -- ^ vegCover
  -> U.Vector Float -> U.Vector Float   -- ^ oceanEvap, oceanMask
  -> U.Vector Float                      -- ^ itczBoost
  -> Float                               -- ^ water level
  -> U.Vector Float                      -- ^ current moisture
  -> (U.Vector Float, U.Vector Float)
moistureStepGrid gridW gridH cfg windDir windSpd elev tempGrid
                 vegCover oceanEvap oceanMask itczBoost waterLevel moisture =
  let mst = ccMoisture cfg
      n = U.length moisture
      pairs = U.generate n $ \i ->
        let (base, precip) = moistureFlowAtGrid gridW gridH cfg windDir windSpd elev
                               tempGrid vegCover moisture i
            -- Ocean reinjection (Model E.4): ocean tiles never drop below
            -- their physical evaporation rate, keeping a persistent moisture
            -- source independent of initial conditions.
            reinject = if oceanMask U.! i > 0.5
                       then max base (oceanEvap U.! i)
                       else base
            -- Land ET reinjection (Model E.6): vegetated land tiles inject
            -- moisture from soil/leaf evapotranspiration each iteration,
            -- sustaining the "flying rivers" mechanism.
            landET = if elev U.! i >= waterLevel
                     then moistBaseRecycleRate mst
                            * (vegCover U.! i)
                            * satNormCfg mst (tempGrid U.! i)
                     else 0
            -- ITCZ convergence boost (Model E.5): moisture concentrates
            -- near the intertropical convergence zone.
            boosted = reinject + itczBoost U.! i + landET
        in (clamp01 boosted, precip)
      (newMoist, condensed) = U.unzip pairs
  in (newMoist, condensed)

-- | Compute the new moisture and net condensation at a single grid
-- tile from advection, saturation-based condensation, and vegetation
-- recycling.
--
-- Returns @(remainingMoisture, netPrecipitation)@ where
-- @netPrecipitation = condensation − recycled@: moisture that actually
-- fell as precipitation at this tile.  Recycled vapour re-enters the
-- atmosphere and may condense in a later iteration.
moistureFlowAtGrid
  :: Int -> Int -> ClimateConfig
  -> U.Vector Float -> U.Vector Float
  -> U.Vector Float -> U.Vector Float
  -> U.Vector Float                      -- ^ vegCover
  -> U.Vector Float -> Int -> (Float, Float)
moistureFlowAtGrid gridW gridH cfg windDir windSpd elev tempGrid vegCover moisture i =
  let prc = ccPrecipitation cfg
      mst = ccMoisture cfg
      x = i `mod` gridW
      y = i `div` gridW
      dir = windDir U.! i
      spd = windSpd U.! i * moistAdvectSpeed mst
      -- Fractional upwind offset: subtract the wind vector to sample
      -- the location the air came FROM (upwind), not where it is going.
      (fdx, fdy) = windOffsetFrac dir spd
      ux = fromIntegral x - fdx
      uy = fromIntegral y - fdy
      -- Bilinear-sampled upwind moisture
      upwindMoisture = bilinearSample gridW gridH moisture ux uy
      upwindElev     = bilinearSample gridW gridH elev ux uy
      h0 = elev U.! i
      cool = max 0 (upwindElev - h0) * precRainShadowLoss prc
      -- Wind-driven advection + local retention
      adv   = upwindMoisture * moistAdvect mst
      local = moisture U.! i * moistLocal mst
      totalMoisture = adv + local - cool
      -- Saturation-based condensation (Model E.2):
      -- The destination atmosphere can hold at most satNorm(T) moisture.
      -- Excess above that capacity condenses, modulated by the
      -- condensation rate to allow gradual precipitation over many
      -- iterations.
      dstCapacity  = satNormCfg mst (tempGrid U.! i)
      excess       = max 0 (totalMoisture - dstCapacity)
      condensation = excess * moistCondensationRate mst
      -- ET recycling (Model E.3):
      -- Vegetation transpires a fraction of condensed moisture back to
      -- the atmosphere, sustaining continental interior humidity.
      vegC     = vegCover U.! i
      recycled = condensation * vegC * moistRecycleRate mst
                   * satNormCfg mst (tempGrid U.! i)
      -- Net precipitation: condensation minus what is immediately
      -- recycled back to the atmosphere.
      netPrecip = max 0 (condensation - recycled)
  in (clamp01 (totalMoisture - condensation + recycled), netPrecip)

orographicAt :: Int -> Int -> ClimateConfig -> U.Vector Float -> U.Vector Float -> Int -> Float
orographicAt gridW gridH cfg windDir elev i =
  let prc = ccPrecipitation cfg
      x = i `mod` gridW
      y = i `div` gridW
      dir = windDir U.! i
      (dx, dy) = windOffset dir (precOrographicStep prc)
      nx = clampCoordGrid gridW (x - dx)
      ny = clampCoordGrid gridH (y - dy)
      ni = ny * gridW + nx
      h0 = elev U.! i
      h1 = elev U.! ni
      rise = max 0 (h0 - h1)
  in clamp01 (rise * precOrographicLift prc * precOrographicScale prc)

boundaryOrogenyAt :: ClimateConfig -> Float -> U.Vector Float -> U.Vector PlateBoundary -> Float -> Int -> Float
boundaryOrogenyAt cfg waterLevel elev boundaries motion i =
  let bnd = ccBoundary cfg
      prc = ccPrecipitation cfg
      boundary = boundaries U.! i
      h0 = elev U.! i
      landRange = max 0.0001 (bndLandRange bnd)
      land = clamp01 ((h0 - waterLevel) / landRange)
      strength = case boundary of
        PlateBoundaryConvergent -> bndPrecipConvergent bnd
        PlateBoundaryDivergent -> bndPrecipDivergent bnd
        PlateBoundaryTransform -> bndPrecipTransform bnd
        PlateBoundaryNone -> 0
  in strength * land * precOrographicLift prc * (1 + motion)

-- | Continental-elevation temperature effect: high plateaus are slightly
-- cooler.  Uses 'ccPlateHeightCooling' (not the main lapse rate) to avoid
-- double-counting altitude cooling.
plateHeightTempBiasAt :: ClimateConfig -> Float -> Float -> Float
plateHeightTempBiasAt cfg waterLevel plateHeight =
  let land = clamp01 (plateHeight - waterLevel)
  in -land * tmpPlateHeightCooling (ccTemperature cfg)

plateHeightPrecipBiasAt :: ClimateConfig -> Float -> Float -> Float
plateHeightPrecipBiasAt cfg waterLevel plateHeight =
  let land = clamp01 (plateHeight - waterLevel)
  in land * precOrographicLift (ccPrecipitation cfg)

buildPlateHeightGrid :: WorldConfig -> IntMap TerrainChunk -> ChunkCoord -> Int -> Int -> U.Vector Float
buildPlateHeightGrid config terrain (ChunkCoord minCx minCy) gridW gridH =
  let size = wcChunkSize config
      minTileX = minCx * size
      minTileY = minCy * size
      sampleAt idx =
        let x = idx `mod` gridW
            y = idx `div` gridW
            gx = minTileX + x
            gy = minTileY + y
            tile = TileCoord gx gy
            (chunkCoord, local) = chunkCoordFromTile config tile
            ChunkId key = chunkIdFromCoord chunkCoord
        in case IntMap.lookup key terrain of
            Nothing -> 0
            Just chunk ->
              case tileIndex config local of
                Nothing -> 0
                Just (TileIndex i) -> tcPlateHeight chunk U.! i
  in U.generate (gridW * gridH) sampleAt

buildPlateVelocityGrid :: WorldConfig -> IntMap TerrainChunk -> ChunkCoord -> Int -> Int -> U.Vector Float
buildPlateVelocityGrid config terrain (ChunkCoord minCx minCy) gridW gridH =
  let size = wcChunkSize config
      minTileX = minCx * size
      minTileY = minCy * size
      sampleAt idx =
        let x = idx `mod` gridW
            y = idx `div` gridW
            gx = minTileX + x
            gy = minTileY + y
            tile = TileCoord gx gy
            (chunkCoord, local) = chunkCoordFromTile config tile
            ChunkId key = chunkIdFromCoord chunkCoord
        in case IntMap.lookup key terrain of
            Nothing -> 0
            Just chunk ->
              case tileIndex config local of
                Nothing -> 0
                Just (TileIndex i) ->
                  plateVelocityMagAt (tcPlateVelX chunk U.! i) (tcPlateVelY chunk U.! i)
  in U.generate (gridW * gridH) sampleAt

plateVelocityMagAt :: Float -> Float -> Float
plateVelocityMagAt vx vy =
  sqrt (vx * vx + vy * vy)

-- | Per-chunk ocean evaporation using Dalton's Law (Model B).
evapAt :: WorldConfig -> Word64 -> LatitudeMapping -> ClimateConfig -> Float -> TileCoord -> U.Vector Float -> U.Vector Float -> U.Vector Float -> Int -> Float
evapAt config seed lm cfg waterLevel origin elev tempVec windSpdVec i =
  let mst = ccMoisture cfg
      TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
      insol = lmInsolation lm
      n0 = noise2D seed (ox + lx + 4000) (oy + ly + 4000)
      noise = n0 * moistEvapNoiseScale mst
      t = tempVec U.! i
      w = windSpdVec U.! i
  in if elev U.! i < waterLevel
     then clamp01 (oceanEvaporation mst t w insol + noise)
     else clamp01 noise

moistureStep :: WorldConfig -> ClimateConfig -> U.Vector Float -> U.Vector Float -> U.Vector Float -> U.Vector Float -> U.Vector Float
moistureStep config cfg windDir windSpd elev moisture =
  U.generate (U.length moisture) (moistureFlowAt config cfg windDir windSpd elev moisture)

moistureFlowAt :: WorldConfig -> ClimateConfig -> U.Vector Float -> U.Vector Float -> U.Vector Float -> U.Vector Float -> Int -> Float
moistureFlowAt config cfg windDir windSpd elev moisture i =
  let prc = ccPrecipitation cfg
      mst = ccMoisture cfg
      size = wcChunkSize config
      x = i `mod` size
      y = i `div` size
      dir = windDir U.! i
      spd = windSpd U.! i * moistAdvectSpeed mst
      -- Subtract wind vector to sample the upwind location.
      (fdx, fdy) = windOffsetFrac dir spd
      ux = fromIntegral x - fdx
      uy = fromIntegral y - fdy
      upwindMoisture = bilinearSample size size moisture ux uy
      upwindElev     = bilinearSample size size elev ux uy
      h0 = elev U.! i
      cool = max 0 (upwindElev - h0) * precRainShadowLoss prc
      adv = upwindMoisture * moistAdvect mst
      local = moisture U.! i * moistLocal mst
  in clamp01 (adv + local - cool)

-- | Fractional wind offset without integer rounding.
--
-- Returns @(dx, dy)@ as floating-point tile offsets.  Used by
-- 'bilinearSample' for sub-tile moisture advection.
windOffsetFrac :: Float -> Float -> (Float, Float)
windOffsetFrac dir spd =
  let dx = cos dir * spd
      dy = sin dir * spd
  in (dx, dy)

-- | Integer-rounded wind offset (retained for orographic sampling and
-- legacy per-chunk path where bilinear is unnecessary).
windOffset :: Float -> Float -> (Int, Int)
windOffset dir spd =
  let dx = round (cos dir * spd)
      dy = round (sin dir * spd)
  in (dx, dy)

-- | Bilinear interpolation of a grid value at a fractional coordinate.
--
-- Clamps the coordinate to the grid bounds, then interpolates between
-- the four surrounding integer tiles.  This is critical for smooth
-- moisture advection at sub-tile wind speeds (fixing RC-2).
{-# INLINE bilinearSample #-}
bilinearSample :: Int -> Int -> U.Vector Float -> Float -> Float -> Float
bilinearSample gridW gridH field fx fy =
  let clampX v
        | v < 0     = 0
        | v >= gridW - 1 = gridW - 2
        | otherwise = v
      clampY v
        | v < 0     = 0
        | v >= gridH - 1 = gridH - 2
        | otherwise = v
      x0 = clampX (floor fx)
      y0 = clampY (floor fy)
      x1 = min (gridW - 1) (x0 + 1)
      y1 = min (gridH - 1) (y0 + 1)
      tx = max 0 (min 1 (fx - fromIntegral x0))
      ty = max 0 (min 1 (fy - fromIntegral y0))
      v00 = field U.! (y0 * gridW + x0)
      v10 = field U.! (y0 * gridW + x1)
      v01 = field U.! (y1 * gridW + x0)
      v11 = field U.! (y1 * gridW + x1)
      top    = v00 * (1 - tx) + v10 * tx
      bottom = v01 * (1 - tx) + v11 * tx
  in top * (1 - ty) + bottom * ty

clampCoord :: Int -> Int -> Int
clampCoord size v
  | v < 0 = 0
  | v >= size = size - 1
  | otherwise = v

diffuseField :: Int -> Int -> Float -> U.Vector Float -> U.Vector Float
diffuseField n iterations factor field =
  iterateN iterations (diffuseOnce n factor) field

diffuseFieldGrid :: Int -> Int -> Int -> Float -> U.Vector Float -> U.Vector Float
diffuseFieldGrid gridW gridH iterations factor field =
  iterateN iterations (diffuseOnceGrid gridW gridH factor) field

coastalProximityGrid :: Int -> Int -> Int -> Float -> U.Vector Float -> U.Vector Float
coastalProximityGrid gridW gridH iterations factor oceanMask =
  iterateN iterations (diffuseOnceGrid gridW gridH factor) oceanMask

diffuseOnceGrid :: Int -> Int -> Float -> U.Vector Float -> U.Vector Float
diffuseOnceGrid gridW gridH factor field =
  U.generate (gridW * gridH) (diffuseAtGrid gridW gridH factor field)

diffuseAtGrid :: Int -> Int -> Float -> U.Vector Float -> Int -> Float
diffuseAtGrid gridW gridH factor field i =
  let x = i `mod` gridW
      y = i `div` gridW
      c = field U.! i
      l = if x > 0 then field U.! (i - 1) else c
      r = if x + 1 < gridW then field U.! (i + 1) else c
      u = if y > 0 then field U.! (i - gridW) else c
      d = if y + 1 < gridH then field U.! (i + gridW) else c
      avg = (l + r + u + d + c) / 5
  in clamp01 (c * (1 - factor) + avg * factor)

diffuseOnce :: Int -> Float -> U.Vector Float -> U.Vector Float
diffuseOnce n factor field =
  U.generate n (diffuseAt n factor field)

diffuseAt :: Int -> Float -> U.Vector Float -> Int -> Float
diffuseAt n factor field i =
  let size = round (sqrt (fromIntegral n))
      x = i `mod` size
      y = i `div` size
      c = field U.! i
      l = if x > 0 then field U.! (i - 1) else c
      r = if x + 1 < size then field U.! (i + 1) else c
      u = if y > 0 then field U.! (i - size) else c
      d = if y + 1 < size then field U.! (i + size) else c
      avg = (c + l + r + u + d) / 5
  in c * (1 - factor) + avg * factor
