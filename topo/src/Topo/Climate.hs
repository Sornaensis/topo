{-# LANGUAGE OverloadedStrings #-}

-- | Climate generation and precipitation fields.
module Topo.Climate
  ( ClimateConfig(..)
  , defaultClimateConfig
  , generateClimateStage
  ) where

import Control.Monad.Reader (asks)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Word (Word64)
import Topo.Math (clamp01, iterateN, lerp)
import Topo.Noise (noise2D)
import Topo.Pipeline (PipelineStage(..))
import Control.Monad.Except (throwError)
import Topo.Planet (PlanetConfig(..), WorldSlice(..), hexesPerDegreeLatitude)
import Topo.Plugin (logInfo, getWorldP, putWorldP, peSeed, PluginError(..))
import Topo.TerrainGrid
  ( buildElevationGrid
  , buildPlateBoundaryGrid
  , chunkGridSlice
  , clampCoordGrid
  , validateTerrainGrid
  )
import Topo.Types
import Topo.World (TerrainWorld(..))
import qualified Data.Vector.Unboxed as U

-- | Climate generation configuration.
data ClimateConfig = ClimateConfig
  { ccLatitudeBias :: !Float
  , ccLatitudeScale :: !Float
  , ccEquatorTemp :: !Float
  , ccPoleTemp :: !Float
  , ccLapseRate :: !Float
  , ccTempNoiseScale :: !Float
  , ccWindIterations :: !Int
  , ccWindDiffuse :: !Float
  , ccWindBeltStrength :: !Float
  , ccWindBeltScale :: !Float
  , ccWindBeltHarmonics :: !Float
  , ccWindBeltBase :: !Float
  , ccWindBeltRange :: !Float
  , ccWindBeltSpeedScale :: !Float
  , ccMoistureIterations :: !Int
  , ccEvaporation :: !Float
  , ccEvapNoiseScale :: !Float
  , ccRainShadow :: !Float
  , ccBoundaryMotionTemp :: !Float
  , ccBoundaryMotionPrecip :: !Float
  , ccBoundaryLandRange :: !Float
  , ccBoundaryTempConvergent :: !Float
  , ccBoundaryTempDivergent :: !Float
  , ccBoundaryTempTransform :: !Float
  , ccBoundaryPrecipConvergent :: !Float
  , ccBoundaryPrecipDivergent :: !Float
  , ccBoundaryPrecipTransform :: !Float
  , ccMoistureAdvect :: !Float
  , ccMoistureLocal :: !Float
  , ccOrographicScale :: !Float
  , ccOrographicStep :: !Float
  , ccCoastalIterations :: !Int
  , ccCoastalDiffuse :: !Float
  , ccCoastalMoistureBoost :: !Float
  , ccInsolation :: !Float
  } deriving (Eq, Show)

-- | Default climate configuration.
defaultClimateConfig :: ClimateConfig
defaultClimateConfig = ClimateConfig
  { ccLatitudeBias = 0
  , ccLatitudeScale = 0.01
  , ccEquatorTemp = 1
  , ccPoleTemp = 0
  , ccLapseRate = 0.25
  , ccTempNoiseScale = 0.1
  , ccWindIterations = 4
  , ccWindDiffuse = 0.5
  , ccWindBeltStrength = 0.6
  , ccWindBeltScale = 0.004
  , ccWindBeltHarmonics = 3
  , ccWindBeltBase = 0.4
  , ccWindBeltRange = 0.6
  , ccWindBeltSpeedScale = 0.6
  , ccMoistureIterations = 6
  , ccEvaporation = 0.25
  , ccEvapNoiseScale = 0.1
  , ccRainShadow = 0.4
  , ccBoundaryMotionTemp = 0.5
  , ccBoundaryMotionPrecip = 0.5
  , ccBoundaryLandRange = 0.6
  , ccBoundaryTempConvergent = -0.06
  , ccBoundaryTempDivergent = 0.02
  , ccBoundaryTempTransform = -0.01
  , ccBoundaryPrecipConvergent = 0.08
  , ccBoundaryPrecipDivergent = -0.05
  , ccBoundaryPrecipTransform = 0.02
  , ccMoistureAdvect = 0.85
  , ccMoistureLocal = 0.15
  , ccOrographicScale = 0.6
  , ccOrographicStep = 1
  , ccCoastalIterations = 3
  , ccCoastalDiffuse = 0.5
  , ccCoastalMoistureBoost = 0.35
  , ccInsolation = 1.0
  }

-- | Generate climate chunks using the current terrain and water level.
generateClimateStage :: ClimateConfig -> Float -> PipelineStage
generateClimateStage cfg waterLevel = PipelineStage "generateClimate" "generateClimate" $ do
  logInfo "generateClimate: generating climate"
  seed <- asks peSeed
  world <- getWorldP
  let config = twConfig world
      terrain = twTerrain world
      -- Derive latitude mapping from planet / world-slice / chunk layout.
      -- Chunks range from -ry to ry, so the center of chunk (0,0) at
      -- tile Y = cs `div` 2 maps to wsLatCenter.
      planet = twPlanet world
      slice  = twSlice world
      hpd    = hexesPerDegreeLatitude planet
      degPerTile = 1.0 / max 0.001 hpd
      cs     = wcChunkSize config
      latBiasDeg = wsLatCenter slice - fromIntegral (cs `div` 2) * degPerTile
      radPerTile = degPerTile * (pi / 180.0)
      latBiasRad = latBiasDeg * (pi / 180.0)
      cfg' = cfg
        { ccLatitudeScale = radPerTile
        , ccLatitudeBias  = latBiasRad
        , ccWindBeltScale = radPerTile
        , ccInsolation    = pcInsolation planet
        }
  (ChunkCoord minCx minCy, ChunkCoord maxCx maxCy) <-
    case validateTerrainGrid config terrain of
      Left err -> throwError (PluginInvariantError err)
      Right bounds -> pure bounds
  let size = wcChunkSize config
      gridW = (maxCx - minCx + 1) * size
      gridH = (maxCy - minCy + 1) * size
      precipGrid = buildPrecipGrid config seed cfg' waterLevel terrain (ChunkCoord minCx minCy) gridW gridH
      climate' = IntMap.mapWithKey (buildClimateChunkWithPrecip config seed cfg' waterLevel precipGrid (ChunkCoord minCx minCy) gridW) terrain
  putWorldP world { twClimate = climate' }

buildClimateChunk :: WorldConfig -> Word64 -> ClimateConfig -> Float -> Int -> TerrainChunk -> ClimateChunk
buildClimateChunk config seed cfg waterLevel key terrain =
  let origin = chunkOriginTile config (chunkCoordFromId (ChunkId key))
      n = chunkTileCount config
      temp = U.generate n (tempAt config seed cfg waterLevel origin (tcElevation terrain) (tcPlateBoundary terrain) (tcPlateHeight terrain) (tcPlateVelX terrain) (tcPlateVelY terrain))
      windDir0 = U.generate n (windDirAt config seed cfg origin)
      windSpd0 = U.generate n (windSpdAt config seed cfg origin)
      windDir = diffuseField n (ccWindIterations cfg) (ccWindDiffuse cfg) windDir0
      windSpd = diffuseField n (ccWindIterations cfg) (ccWindDiffuse cfg) windSpd0
      precip = moistureTransport config seed cfg waterLevel origin windDir windSpd (tcElevation terrain)
  in ClimateChunk
      { ccTempAvg = temp
      , ccPrecipAvg = precip
      , ccWindDirAvg = windDir
      , ccWindSpdAvg = windSpd
      }

buildClimateChunkWithPrecip
  :: WorldConfig
  -> Word64
  -> ClimateConfig
  -> Float
  -> U.Vector Float
  -> ChunkCoord
  -> Int
  -> Int
  -> TerrainChunk
  -> ClimateChunk
buildClimateChunkWithPrecip config seed cfg waterLevel precipGrid minCoord gridW key terrain =
  let base = buildClimateChunk config seed cfg waterLevel key terrain
      precip = chunkGridSlice config minCoord gridW precipGrid key
  in base { ccPrecipAvg = precip }

tempAt
  :: WorldConfig
  -> Word64
  -> ClimateConfig
  -> Float
  -> TileCoord
  -> U.Vector Float
  -> U.Vector PlateBoundary
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Float
  -> Int
  -> Float
tempAt config seed cfg waterLevel origin elev boundaries plateHeight velX velY i =
  let TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
      gy = oy + ly
      lat = fromIntegral gy * ccLatitudeScale cfg + ccLatitudeBias cfg
      base = ccInsolation cfg * lerp (ccPoleTemp cfg) (ccEquatorTemp cfg) (1 - clamp01 (abs (sin lat)))
      height = elev U.! i
      lapse = height * ccLapseRate cfg
      n0 = noise2D seed (ox + lx) (oy + ly) * ccTempNoiseScale cfg
      tectonic = boundaryTempBiasAt cfg waterLevel (boundaries U.! i) height
      motion = clamp01 (plateVelocityMagAt (velX U.! i) (velY U.! i) * ccBoundaryMotionTemp cfg)
      plateBias = plateHeightTempBiasAt cfg waterLevel (plateHeight U.! i)
  in clamp01 (base - lapse + n0 + tectonic * (1 + motion) + plateBias)

boundaryTempBiasAt :: ClimateConfig -> Float -> PlateBoundary -> Float -> Float
boundaryTempBiasAt cfg waterLevel boundary height =
  let landRange = max 0.0001 (ccBoundaryLandRange cfg)
      land = clamp01 ((height - waterLevel) / landRange)
      bias = case boundary of
        PlateBoundaryConvergent -> ccBoundaryTempConvergent cfg
        PlateBoundaryDivergent -> ccBoundaryTempDivergent cfg
        PlateBoundaryTransform -> ccBoundaryTempTransform cfg
        PlateBoundaryNone -> 0
  in bias * land

windDirAt :: WorldConfig -> Word64 -> ClimateConfig -> TileCoord -> Int -> Float
windDirAt config seed cfg origin i =
  let TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
  in windDirAtXY seed cfg (ox + lx) (oy + ly)

windSpdAt :: WorldConfig -> Word64 -> ClimateConfig -> TileCoord -> Int -> Float
windSpdAt config seed cfg origin i =
  let TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
  in windSpdAtXY seed cfg (ox + lx) (oy + ly)

ccWindMoisture :: ClimateConfig -> Float
ccWindMoisture cfg =
  clamp01 (fromIntegral (ccWindIterations cfg) / 10)

moistureTransport :: WorldConfig -> Word64 -> ClimateConfig -> Float -> TileCoord -> U.Vector Float -> U.Vector Float -> U.Vector Float -> U.Vector Float
moistureTransport config seed cfg waterLevel origin windDir windSpd elev =
  let n = U.length elev
      initial = U.generate n (evapAt config seed cfg waterLevel origin elev)
  in iterateN (ccMoistureIterations cfg) (moistureStep config cfg windDir windSpd elev) initial

buildPrecipGrid :: WorldConfig -> Word64 -> ClimateConfig -> Float -> IntMap TerrainChunk -> ChunkCoord -> Int -> Int -> U.Vector Float
buildPrecipGrid config seed cfg waterLevel terrain (ChunkCoord minCx minCy) gridW gridH =
  let size = wcChunkSize config
      minTileX = minCx * size
      minTileY = minCy * size
      elev = buildElevationGrid config terrain (ChunkCoord minCx minCy) gridW gridH
      boundaries = buildPlateBoundaryGrid config terrain (ChunkCoord minCx minCy) gridW gridH
      plateHeight = buildPlateHeightGrid config terrain (ChunkCoord minCx minCy) gridW gridH
      plateVelocity = buildPlateVelocityGrid config terrain (ChunkCoord minCx minCy) gridW gridH
      oceanMask = U.map (\h -> if h < waterLevel then 1 else 0) elev
      coastal = coastalProximityGrid gridW gridH (ccCoastalIterations cfg) (ccCoastalDiffuse cfg) oceanMask
      n = gridW * gridH
      windDir0 = U.generate n (\i ->
        let x = i `mod` gridW
            y = i `div` gridW
            gx = minTileX + x
            gy = minTileY + y
        in windDirAtXY seed cfg gx gy)
      windSpd0 = U.generate n (\i ->
        let x = i `mod` gridW
            y = i `div` gridW
            gx = minTileX + x
            gy = minTileY + y
        in windSpdAtXY seed cfg gx gy)
      windDir = diffuseFieldGrid gridW gridH (ccWindIterations cfg) (ccWindDiffuse cfg) windDir0
      windSpd = diffuseFieldGrid gridW gridH (ccWindIterations cfg) (ccWindDiffuse cfg) windSpd0
      initial = U.generate n (\i ->
        let x = i `mod` gridW
            y = i `div` gridW
            gx = minTileX + x
            gy = minTileY + y
            base = evapAtXY seed cfg waterLevel gx gy (elev U.! i)
            coast = coastal U.! i * ccCoastalMoistureBoost cfg
        in clamp01 (base + coast))
      moisture = iterateN (ccMoistureIterations cfg) (moistureStepGrid gridW gridH cfg windDir windSpd elev) initial
    in U.generate n (\i ->
      let { o = orographicAt gridW gridH cfg windDir elev i
        ; motion = clamp01 (plateVelocity U.! i * ccBoundaryMotionPrecip cfg)
        ; tectonic = boundaryOrogenyAt cfg waterLevel elev boundaries motion i
        ; plateBias = plateHeightPrecipBiasAt cfg waterLevel (plateHeight U.! i)
        }
      in clamp01 (moisture U.! i + o + tectonic + plateBias))

windDirAtXY :: Word64 -> ClimateConfig -> Int -> Int -> Float
windDirAtXY seed cfg gx gy =
  let noiseDir = noise2D seed (gx + 2000) (gy + 2000) * 6.283185
      latRad = fromIntegral gy * ccWindBeltScale cfg + ccLatitudeBias cfg
      belt = sin (latRad * 2 * ccWindBeltHarmonics cfg)
      beltDir = if belt >= 0 then 0 else pi
  in blendAngle noiseDir beltDir (ccWindBeltStrength cfg)

blendAngle :: Float -> Float -> Float -> Float
blendAngle a b t =
  let t' = clamp01 t
      x = lerp (cos a) (cos b) t'
      y = lerp (sin a) (sin b) t'
  in atan2 y x

windSpdAtXY :: Word64 -> ClimateConfig -> Int -> Int -> Float
windSpdAtXY seed cfg gx gy =
  let noiseSpd = clamp01 (noise2D seed (gx + 3000) (gy + 3000))
      latRad = fromIntegral gy * ccWindBeltScale cfg + ccLatitudeBias cfg
      belt = clamp01 (ccWindBeltBase cfg + ccWindBeltRange cfg * abs (sin (latRad * 2 * ccWindBeltHarmonics cfg)))
      strength = clamp01 (ccWindBeltStrength cfg * ccWindBeltSpeedScale cfg)
  in clamp01 (lerp noiseSpd belt strength)

evapAtXY :: Word64 -> ClimateConfig -> Float -> Int -> Int -> Float -> Float
evapAtXY seed cfg waterLevel gx gy elevation =
  let insolFactor = ccInsolation cfg
      n0 = noise2D seed (gx + 4000) (gy + 4000)
      ocean = if elevation < waterLevel then 1 else 0
  in clamp01 (ocean * ccEvaporation cfg * insolFactor + n0 * ccEvapNoiseScale cfg)

moistureStepGrid :: Int -> Int -> ClimateConfig -> U.Vector Float -> U.Vector Float -> U.Vector Float -> U.Vector Float -> U.Vector Float
moistureStepGrid gridW gridH cfg windDir windSpd elev moisture =
  U.generate (U.length moisture) (moistureFlowAtGrid gridW gridH cfg windDir windSpd elev moisture)

moistureFlowAtGrid :: Int -> Int -> ClimateConfig -> U.Vector Float -> U.Vector Float -> U.Vector Float -> U.Vector Float -> Int -> Float
moistureFlowAtGrid gridW gridH cfg windDir windSpd elev moisture i =
  let x = i `mod` gridW
      y = i `div` gridW
      dir = windDir U.! i
      spd = windSpd U.! i
      (dx, dy) = windOffset dir spd
      nx = clampCoordGrid gridW (x + dx)
      ny = clampCoordGrid gridH (y + dy)
      ni = ny * gridW + nx
      h0 = elev U.! i
      h1 = elev U.! ni
      cool = max 0 (h1 - h0) * ccRainShadow cfg
      adv = moisture U.! ni * ccMoistureAdvect cfg
      local = moisture U.! i * ccMoistureLocal cfg
  in clamp01 (adv + local - cool)

orographicAt :: Int -> Int -> ClimateConfig -> U.Vector Float -> U.Vector Float -> Int -> Float
orographicAt gridW gridH cfg windDir elev i =
  let x = i `mod` gridW
      y = i `div` gridW
      dir = windDir U.! i
      (dx, dy) = windOffset dir (ccOrographicStep cfg)
      nx = clampCoordGrid gridW (x - dx)
      ny = clampCoordGrid gridH (y - dy)
      ni = ny * gridW + nx
      h0 = elev U.! i
      h1 = elev U.! ni
      rise = max 0 (h0 - h1)
  in clamp01 (rise * ccRainShadow cfg * ccOrographicScale cfg)

boundaryOrogenyAt :: ClimateConfig -> Float -> U.Vector Float -> U.Vector PlateBoundary -> Float -> Int -> Float
boundaryOrogenyAt cfg waterLevel elev boundaries motion i =
  let boundary = boundaries U.! i
      h0 = elev U.! i
      landRange = max 0.0001 (ccBoundaryLandRange cfg)
      land = clamp01 ((h0 - waterLevel) / landRange)
      strength = case boundary of
        PlateBoundaryConvergent -> ccBoundaryPrecipConvergent cfg
        PlateBoundaryDivergent -> ccBoundaryPrecipDivergent cfg
        PlateBoundaryTransform -> ccBoundaryPrecipTransform cfg
        PlateBoundaryNone -> 0
  in strength * land * ccRainShadow cfg * (1 + motion)

plateHeightTempBiasAt :: ClimateConfig -> Float -> Float -> Float
plateHeightTempBiasAt cfg waterLevel plateHeight =
  let land = clamp01 (plateHeight - waterLevel)
  in -land * ccLapseRate cfg

plateHeightPrecipBiasAt :: ClimateConfig -> Float -> Float -> Float
plateHeightPrecipBiasAt cfg waterLevel plateHeight =
  let land = clamp01 (plateHeight - waterLevel)
  in land * ccRainShadow cfg

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

evapAt :: WorldConfig -> Word64 -> ClimateConfig -> Float -> TileCoord -> U.Vector Float -> Int -> Float
evapAt config seed cfg waterLevel origin elev i =
  let TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
      insolFactor = ccInsolation cfg
      n0 = noise2D seed (ox + lx + 4000) (oy + ly + 4000)
      ocean = if elev U.! i < waterLevel then 1 else 0
  in clamp01 (ocean * ccEvaporation cfg * insolFactor + n0 * ccEvapNoiseScale cfg)

moistureStep :: WorldConfig -> ClimateConfig -> U.Vector Float -> U.Vector Float -> U.Vector Float -> U.Vector Float -> U.Vector Float
moistureStep config cfg windDir windSpd elev moisture =
  U.generate (U.length moisture) (moistureFlowAt config cfg windDir windSpd elev moisture)

moistureFlowAt :: WorldConfig -> ClimateConfig -> U.Vector Float -> U.Vector Float -> U.Vector Float -> U.Vector Float -> Int -> Float
moistureFlowAt config cfg windDir windSpd elev moisture i =
  let size = wcChunkSize config
      x = i `mod` size
      y = i `div` size
      dir = windDir U.! i
      spd = windSpd U.! i
      (dx, dy) = windOffset dir spd
      nx = clampCoord size (x + dx)
      ny = clampCoord size (y + dy)
      ni = ny * size + nx
      h0 = elev U.! i
      h1 = elev U.! ni
      cool = max 0 (h1 - h0) * ccRainShadow cfg
      adv = moisture U.! ni * ccMoistureAdvect cfg
      local = moisture U.! i * ccMoistureLocal cfg
  in clamp01 (adv + local - cool)

windOffset :: Float -> Float -> (Int, Int)
windOffset dir spd =
  let dx = round (cos dir * spd)
      dy = round (sin dir * spd)
  in (dx, dy)

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
