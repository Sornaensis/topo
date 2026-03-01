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
    -- * Wind internals (for testing)
  , windDirAtXY
  ) where

import Control.Monad.Reader (asks)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Word (Word64)
import Topo.Climate.Config
import Topo.Climate.Evaporation
import Topo.Math (clamp01, clampLat, iterateN, lerp, smoothstep)
import Topo.Noise (fbm2D, noise2D)
import Topo.Pipeline (PipelineStage(..))
import Topo.Pipeline.Stage (StageId(..))
import Control.Monad.Except (throwError)
import Topo.Planet (LatitudeMapping(..))
import Topo.Plugin (logInfo, getWorldP, putWorldP, peSeed, PluginError(..))
import Topo.TerrainGrid
  ( buildElevationGrid
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
generateClimateStage cfg wcfg waterLevel = PipelineStage StageClimate "generateClimate" "generateClimate" Nothing [] Nothing $ do
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
      (precipGrid, coastalGrid, moistureGrid, tempGrid) = buildClimateGrids config seed lm cfg waterLevel terrain vegMap (ChunkCoord minCx minCy) gridW gridH
      climate' = IntMap.mapWithKey (buildClimateChunkWithPrecip config seed lm cfg seasonAmp sBase sRange precipGrid coastalGrid moistureGrid tempGrid (ChunkCoord minCx minCy) gridW) terrain
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
  -> U.Vector Float   -- ^ global precipitation grid
  -> U.Vector Float   -- ^ global coastal proximity grid
  -> U.Vector Float   -- ^ global equilibrium moisture grid (from transport)
  -> U.Vector Float   -- ^ global temperature grid
  -> ChunkCoord       -- ^ min chunk coordinate
  -> Int              -- ^ grid width in tiles
  -> Int              -- ^ chunk key
  -> TerrainChunk
  -> ClimateChunk
buildClimateChunkWithPrecip config seed lm cfg seasonAmp sBase sRange precipGrid coastalGrid moistureGrid tempGrid minCoord gridW key terrain =
  let origin = chunkOriginTile config (chunkCoordFromId (ChunkId key))
      n = chunkTileCount config
      wnd = ccWind cfg

      -- Wind fields
      windDir0 = U.generate n (windDirAt config seed lm cfg origin)
      windSpd0 = U.generate n (windSpdAt config seed lm cfg origin)
      windDir = diffuseField n (windIterations wnd) (windDiffuse wnd) windDir0
      windSpd = diffuseField n (windIterations wnd) (windDiffuse wnd) windSpd0

      -- Global-grid slices for this chunk
      precip   = chunkGridSlice config minCoord gridW precipGrid key
      coastal  = chunkGridSlice config minCoord gridW coastalGrid key
      moisture = chunkGridSlice config minCoord gridW moistureGrid key
      temp     = chunkGridSlice config minCoord gridW tempGrid key

      -- Seasonality fields with terrain-aware modulations
      seasonCfg = ccSeasonality cfg
      amp       = seasonAmp
      soilMoist = tcMoisture terrain
      humidityAvg   = climateHumidityAvg (ccMoisture cfg) seasonCfg moisture temp soilMoist n
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

-- | Annual-average relative humidity from the moisture transport model.
--
-- Uses the equilibrium atmospheric moisture from the transport
-- simulation divided by the local saturation capacity
-- (@satNormCfg mst T@), giving a physics-based relative humidity
-- that accounts for advection, evaporation, condensation, and
-- recycling.
--
-- @RH = clamp01(moisture / satNorm(T) + soilMoisture × soilContrib)@
--
-- The soil moisture term captures surface boundary-layer humidity
-- (fog, dew, microclimate) that the column-average transport model
-- does not resolve.
climateHumidityAvg
  :: MoistureConfig
  -> SeasonalityConfig
  -> U.Vector Float    -- ^ equilibrium atmospheric moisture (from transport)
  -> U.Vector Float    -- ^ temperature
  -> U.Vector Float    -- ^ soil moisture (from hydrology tcMoisture)
  -> Int
  -> U.Vector Float
climateHumidityAvg mst seasonCfg moisture temp soilMoist n =
  let soilContrib = scHumiditySoilContribution seasonCfg
  in U.generate n $ \i ->
    let t = temp U.! i
        m = moisture U.! i
        sat = max 0.001 (satNormCfg mst t)
        transportRH = m / sat
        s = soilMoist U.! i
    in clamp01 (transportRH + s * soilContrib)

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
  -> U.Vector Float
  -> U.Vector Float          -- ^ Per-tile surface albedo (from vegetation)
  -> Int
  -> Float
tempAt config seed lm cfg waterLevel origin elev plateHeight albedoVec i =
  let tmp = ccTemperature cfg
      TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
      gy = oy + ly
      lat = clampLat (fromIntegral gy * lmRadPerTile lm + lmBiasRad lm)
      height = elev U.! i
      blendWidth = max 0.0001 (tmpCoastalBlendWidth tmp)
      blend = smoothstep (waterLevel - blendWidth) (waterLevel + blendWidth) height
      isOcean = height < waterLevel
      insol = lmInsolation lm
      -- Coastal blending: smoothly transition between ocean and land
      -- base curves around sea level.
      latFOcean = clamp01 (abs (cos lat) ** tmpOceanLatExponent tmp)
      baseOcean = insol * lerp (tmpOceanPoleSST tmp) (tmpOceanEquatorSST tmp) latFOcean
      latFLand = clamp01 (abs (cos lat) ** tmpLatitudeExponent tmp)
      baseLand = insol * lerp (tmpPoleTemp tmp) (tmpEquatorTemp tmp) latFLand
      base = lerp baseOcean baseLand blend
      lapseLand = (height - waterLevel) * tmpLapseRate tmp
      lapse = blend * lapseLand
      n0 = coherentNoise2D
             seed
             (tmpNoiseOctaves tmp)
             (tmpNoiseFrequency tmp)
             (ox + lx)
             (oy + ly)
           * tmpNoiseScale tmp
      plateBias = plateHeightTempBiasAt cfg waterLevel (plateHeight U.! i)
      rawTemp = base - lapse + n0 + plateBias
      -- Model H: albedo feedback — land only.
      -- Ocean SST profile already accounts for albedo/evaporative balance.
      albedo = albedoVec U.! i
      albedoCorr
        | isOcean   = 1
        | otherwise = 1 - tmpAlbedoSensitivity tmp
                          * (albedo - tmpAlbedoReference tmp)
  in clamp01 (rawTemp * albedoCorr)

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

-- | Build the global precipitation, coastal proximity, and equilibrium
-- moisture grids.
--
-- Returns @(precipGrid, coastalGrid, moistureGrid)@.  The precipitation
-- grid uses physics-based evaporation: Dalton's-Law ocean evaporation
-- (Model B) and Penman-Monteith-inspired land ET (Model C), both
-- driven by the Clausius-Clapeyron saturation curve.  Moisture is then
-- transported by wind with temperature-dependent condensation.
--
-- The moisture grid is the equilibrium atmospheric moisture field after
-- all transport iterations, used downstream to compute transport-model
-- relative humidity (@moisture / satNorm(T)@).
--
-- The returned temperature grid is the /same/ globally-diffused,
-- ocean-moderated field that is ultimately sliced into @ccTempAvg@,
-- ensuring moisture transport and stored climate temperature stay
-- consistent.
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
  -> (U.Vector Float, U.Vector Float, U.Vector Float, U.Vector Float)
buildClimateGrids config seed lm cfg waterLevel terrain vegMap (ChunkCoord minCx minCy) gridW gridH =
  let prc = ccPrecipitation cfg
      wnd = ccWind cfg
      mst = ccMoisture cfg
      tmp = ccTemperature cfg
      size = wcChunkSize config
      minTileX = minCx * size
      minTileY = minCy * size
      insol = lmInsolation lm
      elev = buildElevationGrid config terrain (ChunkCoord minCx minCy) gridW gridH
      plateHeight = buildPlateHeightGrid config terrain (ChunkCoord minCx minCy) gridW gridH
      oceanMask = U.map (\h -> if h < waterLevel then 1 else 0) elev
      coastal = coastalProximityGrid gridW gridH (precCoastalIterations prc) (precCoastalDiffuse prc) oceanMask
      n = gridW * gridH
      -- Soil moisture grid (from hydrology-stage tcMoisture)
      soilMoistGrid = buildFieldGrid config terrain tcMoisture (ChunkCoord minCx minCy) gridW gridH
      -- Vegetation cover grid (from bootstrap stage)
      vegCoverGrid  = buildVegFieldGrid config vegMap vegCover (ChunkCoord minCx minCy) gridW gridH
      -- Vegetation albedo grid (for temperature feedback, Model H)
      vegAlbedoGrid = buildVegFieldGrid config vegMap vegAlbedo (ChunkCoord minCx minCy) gridW gridH
      -- Build a global detailed temperature grid (same logic used for
      -- final climate temperature output): coherent noise + albedo,
      -- then global diffusion and global ocean moderation.
      albedoRef = tmpAlbedoReference tmp
      tempRaw = U.generate n (\i ->
        let x = i `mod` gridW
            y = i `div` gridW
            gx = minTileX + x
            gy = minTileY + y
            -- Use vegAlbedo if available, else reference (no correction)
            a = let v = vegAlbedoGrid U.! i
                in if v == 0 then albedoRef else v
        in tempAtGlobal seed lm cfg waterLevel gx gy (elev U.! i) (plateHeight U.! i) a)
      tempDiffused = diffuseFieldGrid gridW gridH
                       (tmpDiffuseIterations tmp)
                       (tmpDiffuseFactor tmp)
                       tempRaw
      moderation = tmpOceanModeration tmp
      tempGrid = U.generate n (\i ->
        let t = tempDiffused U.! i
            c = coastal U.! i
            y = i `div` gridW
            gy = minTileY + y
            lat = clampLat (fromIntegral gy * lmRadPerTile lm + lmBiasRad lm)
            latF = clamp01 (abs (cos lat) ** tmpOceanLatExponent tmp)
            moderateTarget = lerp (tmpOceanPoleSST tmp) (tmpOceanEquatorSST tmp) latF
                           + tmpOceanModerateTemp tmp
            pull = c * moderation * (moderateTarget - t)
        in clamp01 (t + pull))
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
      (finalMoisture, accumCondensation) =
        moistureTransportAccum (moistIterations mst)
          (moistureStepGrid gridW gridH cfg windDir windSpd elev tempGrid
            vegCoverGrid oceanEvapGrid oceanMask itczBoostGrid waterLevel)
          initial
      precipGrid = U.generate n (\i ->
        let o = orographicAt gridW gridH cfg windDir elev i
            plateBias = plateHeightPrecipBiasAt cfg waterLevel (plateHeight U.! i)
            rawPrecip = accumCondensation U.! i + o + plateBias
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
    in (precipGrid, coastal, finalMoisture, tempGrid)

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

-- | Detailed temperature at global tile coordinates.
--
-- This is the canonical temperature model used to build the global
-- climate temperature grid: latitude curve, lapse term, coherent noise,
-- plate-height bias, and albedo correction.
tempAtGlobal :: Word64 -> LatitudeMapping -> ClimateConfig -> Float -> Int -> Int -> Float -> Float -> Float -> Float
tempAtGlobal seed lm cfg waterLevel gx gy height plateHt albedo =
  let tmp = ccTemperature cfg
      lat = clampLat (fromIntegral gy * lmRadPerTile lm + lmBiasRad lm)
      blendWidth = max 0.0001 (tmpCoastalBlendWidth tmp)
      blend = smoothstep (waterLevel - blendWidth) (waterLevel + blendWidth) height
      isOcean = height < waterLevel
      insol = lmInsolation lm
      latFOcean = clamp01 (abs (cos lat) ** tmpOceanLatExponent tmp)
      baseOcean = insol * lerp (tmpOceanPoleSST tmp) (tmpOceanEquatorSST tmp) latFOcean
      latFLand = clamp01 (abs (cos lat) ** tmpLatitudeExponent tmp)
      baseLand = insol * lerp (tmpPoleTemp tmp) (tmpEquatorTemp tmp) latFLand
      base = lerp baseOcean baseLand blend
      lapseLand = (height - waterLevel) * tmpLapseRate tmp
      lapse = blend * lapseLand
      n0 = coherentNoise2D
             seed
             (tmpNoiseOctaves tmp)
             (tmpNoiseFrequency tmp)
             gx
             gy
           * tmpNoiseScale tmp
      plateBias = plateHeightTempBiasAt cfg waterLevel plateHt
      rawTemp = base - lapse + n0 + plateBias
      -- Model H: albedo correction — land only.
      albedoCorr
        | isOcean   = 1
        | otherwise = 1 - tmpAlbedoSensitivity tmp
                          * (albedo - tmpAlbedoReference tmp)
  in clamp01 (rawTemp * albedoCorr)

-- | Compute the prevailing wind direction at a global tile.
--
-- Uses a three-band Coriolis model that produces realistic:
--
-- * __Trade winds__ (0–30° lat): westward + equatorward deflection
-- * __Westerlies__ (30–60° lat): eastward + poleward deflection
-- * __Polar easterlies__ (60–90° lat): westward, reduced deflection
--
-- The meridional deflection magnitude is controlled by
-- 'windCoriolisDeflection'.  NH deflects right (positive), SH deflects
-- left (negative).  Band transitions use a cosine blend over ±5° to
-- avoid sharp discontinuities.
--
-- The final direction is a weighted blend of the Coriolis belt
-- direction and spatially-coherent noise, controlled by
-- 'windBeltStrength'.
windDirAtXY :: Word64 -> LatitudeMapping -> ClimateConfig -> Int -> Int -> Float
windDirAtXY seed lm cfg gx gy =
  let wnd = ccWind cfg
      noiseDir = noise2D seed (gx + 2000) (gy + 2000) * 6.283185
      latRad = fromIntegral gy * lmRadPerTile lm + lmBiasRad lm
      latDeg = latRad * (180.0 / pi)
      absLat = abs latDeg
      hemisphere = if latDeg >= 0 then 1.0 else -1.0
      deflect = windCoriolisDeflection wnd

      -- Smooth transition weights using cosine blend over ±5°
      -- tradeFrac: 1.0 at |lat|<25, 0.0 at |lat|>35
      tradeFrac = smoothBandWeight absLat 25.0 35.0
      -- westerlyFrac: 1.0 at 35<|lat|<55, blends in/out at edges
      westerlyFrac = (1.0 - smoothBandWeight absLat 25.0 35.0)
                   * smoothBandWeight absLat 0.0 55.0
      -- polarFrac: 1.0 at |lat|>65, 0.0 at |lat|<55
      polarFrac = 1.0 - smoothBandWeight absLat 55.0 65.0

      -- Trade winds: blow westward (pi) with equatorward deflection
      tradeDir = pi + hemisphere * deflect
      -- Westerlies: blow eastward (0) with poleward deflection
      westerlyDir = 0 + hemisphere * deflect
      -- Polar easterlies: blow westward (pi) with reduced deflection
      polarDir = pi + hemisphere * (deflect * 0.7)

      -- Blend the three bands
      totalW = tradeFrac + westerlyFrac + polarFrac
      safeW  = max 0.001 totalW
      beltDir = blendAngle3
                  tradeDir    (tradeFrac / safeW)
                  westerlyDir (westerlyFrac / safeW)
                  polarDir    (polarFrac / safeW)
  in blendAngle noiseDir beltDir (windBeltStrength wnd)

-- | Smooth transition weight for latitude bands.
-- Returns 1.0 when @absLat < lo@, 0.0 when @absLat > hi@,
-- and a cosine-smoothed value in between.
smoothBandWeight :: Float -> Float -> Float -> Float
smoothBandWeight absLat lo hi
  | absLat <= lo = 1.0
  | absLat >= hi = 0.0
  | otherwise    = let t = (absLat - lo) / (hi - lo)
                   in 0.5 + 0.5 * cos (t * pi)

-- | Blend three angular directions by weighted vector averaging.
blendAngle3 :: Float -> Float -> Float -> Float -> Float -> Float -> Float
blendAngle3 a1 w1 a2 w2 a3 w3 =
  let !x = w1 * cos a1 + w2 * cos a2 + w3 * cos a3
      !y = w1 * sin a1 + w2 * sin a2 + w3 * sin a3
  in atan2 y x

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
            -- Uses moistMinVegFloor to break the cold-start problem:
            -- even bare land contributes a minimum ET fraction so
            -- interior moisture can bootstrap vegetation growth.
            landET = if elev U.! i >= waterLevel
                     then moistBaseRecycleRate mst
                            * max (moistMinVegFloor mst) (vegCover U.! i)
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
      forcedCond   = excess * moistCondensationRate mst
      -- Convective precipitation (Model E.7):
      -- Warm near-saturated air undergoes spontaneous convective
      -- uplift even on flat terrain.  Fires when relative humidity
      -- exceeds a configurable threshold and scales with temperature
      -- (warmer → more CAPE → stronger updrafts) and with saturation
      -- capacity to keep the result in moisture-fraction units.
      rh           = if dstCapacity > 0.001
                       then min 1 (totalMoisture / dstCapacity)
                       else 0
      convExcess   = max 0 (rh - moistConvectiveThreshold mst)
      convective   = convExcess * moistConvectiveRate mst
                       * (tempGrid U.! i) * dstCapacity
      condensation = forcedCond + convective
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

coherentNoise2D :: Word64 -> Int -> Float -> Int -> Int -> Float
coherentNoise2D seed octaves freq x y =
  let fx = fromIntegral x * freq
      fy = fromIntegral y * freq
      oct = max 1 octaves
      raw = fbm2D seed oct coherentNoiseLacunarity coherentNoiseGain fx fy
      amp = max 0.0001 (fbmAmplitude oct coherentNoiseGain)
      signed = raw / amp
  in clampSigned signed

fbmAmplitude :: Int -> Float -> Float
fbmAmplitude octaves gain =
  go (max 1 octaves) 1 0
  where
    go 0 _ acc = acc
    go n amp acc = go (n - 1) (amp * gain) (acc + amp)

clampSigned :: Float -> Float
clampSigned v
  | v < (-1) = -1
  | v > 1 = 1
  | otherwise = v

coherentNoiseLacunarity :: Float
coherentNoiseLacunarity = 2.0

coherentNoiseGain :: Float
coherentNoiseGain = 0.5
