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
--
-- Moisture transport simulation is provided by
-- "Topo.Climate.MoistureTransport" (re-exported).
--
-- Precipitation assembly is provided by
-- "Topo.Climate.Precipitation" (re-exported).
module Topo.Climate
  ( -- * Re-exported configuration types
    module Topo.Climate.Config
    -- * Re-exported evaporation models
  , module Topo.Climate.Evaporation
    -- * Re-exported moisture transport
  , module Topo.Climate.MoistureTransport
    -- * Re-exported precipitation assembly
  , module Topo.Climate.Precipitation
    -- * Pipeline stage
  , generateClimateStage
    -- * Wind internals (for testing)
  , windDirAtXY
  ) where

import Control.Monad.Reader (asks)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Word (Word64)
import Topo.Climate.Config
import Topo.Climate.Evaporation
import Topo.Climate.MoistureTransport
import Topo.Climate.Precipitation
import Topo.Grid.HexDirection
  ( nearestHexDirection
  , stepIndexInDirection
  )
import Topo.Grid.Diffusion (coastalProximityGrid, diffuseAngleFieldGrid, diffuseFieldGrid)
import Topo.Math (clamp01, clampLat, lerp, smoothstep)
import Topo.Noise (fbm2D, noise2D)
import Topo.Pipeline (PipelineStage(..))
import Topo.Pipeline.Stage (StageId(..))
import Control.Monad.Except (throwError)
import Topo.Hex (hexOpposite)
import Topo.Planet (LatitudeMapping(..))
import Topo.Plugin (logInfo, getWorldP, putWorldP, peSeed, PluginError(..))
import Topo.TerrainGrid
  ( buildElevationGrid
  , buildPlateHeightGrid
  , chunkGridSlice
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
      chunkSize = wcChunkSize config
      wnd = ccWind cfg

      -- Wind fields
      windDir0 = U.generate n (windDirAt config seed lm cfg origin)
      windSpd0 = U.generate n (windSpdAt config seed lm cfg origin)
      windDir = diffuseAngleFieldGrid chunkSize chunkSize (windIterations wnd) (windDiffuse wnd) windDir0
      windSpd = diffuseFieldGrid chunkSize chunkSize (windIterations wnd) (windDiffuse wnd) windSpd0

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
--     falls primarily in the wet season.  The upwind elevation sample now
--     follows the nearest hex direction implied by the local wind field.
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
        upwindDir = hexOpposite (nearestHexDirection wdir)
        upIdx = stepIndexInDirection size size upwindDir i
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
      precipGrid = assemblePrecipGrid gridW gridH lm cfg waterLevel minTileY
                     accumCondensation windDir elev plateHeight
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
