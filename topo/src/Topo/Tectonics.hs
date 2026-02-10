{-# LANGUAGE OverloadedStrings #-}

-- | Plate tectonics configuration and terrain synthesis.
module Topo.Tectonics
  ( TectonicsConfig(..)
  , defaultTectonicsConfig
  , generateTectonicsStage
  , applyTectonicsChunk
  ) where

import Control.Monad.Reader (asks)
import Data.Bits (xor)
import Data.List (foldl', sortBy)
import Data.Ord (comparing)
import Data.Word (Word16, Word64)
import qualified Data.IntMap.Strict as IntMap
import Topo.BaseHeight (GenConfig(..), OceanEdgeDepth, defaultGenConfig, oceanEdgeBiasAt, sampleBaseHeightAt)
import Topo.Math (clamp01, lerp, smoothstep)
import Topo.Noise (directionalRidge2D, domainWarp2D, fbm2D, noise2DContinuous, ridgedFbm2D)
import Topo.Pipeline (PipelineStage(..))
import Topo.Planet (PlanetConfig(..), WorldSlice(..), hexesPerDegreeLatitude)
import Topo.Plugin (logInfo, modifyWorldP, peSeed)
import Topo.Types
import Topo.World (TerrainWorld(..))
import qualified Data.Vector.Unboxed as U

-- | Configuration parameters for plate tectonics.
data TectonicsConfig = TectonicsConfig
  { tcPlateSize :: !Int
  , tcPlateCount :: !(Maybe Int)
  , tcPlateSpeed :: !Float
  , tcBoundarySharpness :: !Float
  , tcBoundaryNoiseScale :: !Float
  , tcBoundaryNoiseStrength :: !Float
  , tcBoundaryWarpOctaves :: !Int
  , tcBoundaryWarpLacunarity :: !Float
  , tcBoundaryWarpGain :: !Float
  , tcPlateMergeScale :: !Float
  , tcPlateMergeBias :: !Float
  , tcPlateDetailScale :: !Float
  , tcPlateDetailStrength :: !Float
  , tcPlateRidgeStrength :: !Float
  , tcPlateBiasStrength :: !Float
  , tcPlateBiasCenter :: !Float
  , tcPlateBiasEdge :: !Float
  , tcPlateBiasNorth :: !Float
  , tcPlateBiasSouth :: !Float
  , tcCenterFalloffScale :: !Float
  , tcBoundaryFadeScale :: !Float
  , tcCenterWeightScale :: !Float
  , tcEdgeWeightScale :: !Float
  , tcPlateHeightBase :: !Float
  , tcPlateHeightVariance :: !Float
  , tcCrustContinentalBias :: !Float
  , tcCrustOceanicBias :: !Float
  , tcPlateHardnessBase :: !Float
  , tcPlateHardnessVariance :: !Float
  , tcUplift :: !Float
  , tcRiftDepth :: !Float
  , tcTrenchDepth :: !Float
  , tcRidgeHeight :: !Float
  -- | Latitude degrees per tile Y step (derived from planet; default 0 = use tile Y fallback).
  , tcLatDegPerTile :: !Float
  -- | Latitude bias in degrees for tile Y = 0 (derived from planet/slice; default 0).
  , tcLatBiasDeg :: !Float
  -- | Slice latitude extent in degrees for north/south weight normalization (default 0 = use tile Y fallback).
  , tcSliceLatExtent :: !Float
  } deriving (Eq, Show)

-- | Default tectonics configuration.
defaultTectonicsConfig :: TectonicsConfig
defaultTectonicsConfig = TectonicsConfig
  { tcPlateSize = 64
  , tcPlateCount = Nothing
  , tcPlateSpeed = 0.6
  , tcBoundarySharpness = 1.2
  , tcBoundaryNoiseScale = 0.008
  , tcBoundaryNoiseStrength = 12
  , tcBoundaryWarpOctaves = 3
  , tcBoundaryWarpLacunarity = 2
  , tcBoundaryWarpGain = 0.5
  , tcPlateMergeScale = 0.11
  , tcPlateMergeBias = 0.52
  , tcPlateDetailScale = 0.02
  , tcPlateDetailStrength = 0.35
  , tcPlateRidgeStrength = 0.25
  , tcPlateBiasStrength = 0.25
  , tcPlateBiasCenter = 0
  , tcPlateBiasEdge = 0
  , tcPlateBiasNorth = 0
  , tcPlateBiasSouth = 0
  , tcCenterFalloffScale = 0.8
  , tcBoundaryFadeScale = 1.1
  , tcCenterWeightScale = 1.2
  , tcEdgeWeightScale = 1.2
  , tcPlateHeightBase = 0.18
  , tcPlateHeightVariance = 0.85
  , tcCrustContinentalBias = 0.12
  , tcCrustOceanicBias = -0.12
  , tcPlateHardnessBase = 0.45
  , tcPlateHardnessVariance = 0.3
  , tcUplift = 0.15
  , tcRiftDepth = 0.2
  , tcTrenchDepth = 0.25
  , tcRidgeHeight = 0.08
  , tcLatDegPerTile = 0
  , tcLatBiasDeg = 0
  , tcSliceLatExtent = 0
  }

-- | Apply tectonic generation to the existing terrain chunks in a world.
--
-- Derives latitude mapping from 'PlanetConfig' and 'WorldSlice' so that
-- north/south plate bias is latitude-aware for non-equator slices.
generateTectonicsStage :: TectonicsConfig -> PipelineStage
generateTectonicsStage cfg = PipelineStage "generateTectonics" "generateTectonics" $ do
  logInfo "generateTectonics: generating plate terrain"
  seed <- asks peSeed
  modifyWorldP $ \world ->
    let config = twConfig world
        planet = twPlanet world
        slice  = twSlice world
        hpd    = hexesPerDegreeLatitude planet
        degPerTile = 1.0 / max 0.001 hpd
        cs     = wcChunkSize config
        latBiasDeg = wsLatCenter slice - fromIntegral (cs `div` 2) * degPerTile
        cfg' = cfg
          { tcLatDegPerTile  = degPerTile
          , tcLatBiasDeg     = latBiasDeg
          , tcSliceLatExtent = wsLatExtent slice
          }
        terrain' = IntMap.mapWithKey (applyTectonicsChunk config seed defaultGenConfig cfg') (twTerrain world)
    in world { twTerrain = terrain' }

-- | Apply tectonic shaping for a single terrain chunk.
applyTectonicsChunk :: WorldConfig -> Word64 -> GenConfig -> TectonicsConfig -> Int -> TerrainChunk -> TerrainChunk
applyTectonicsChunk config seed gcfg tcfg key chunk =
  let tcfg' = normalizeTectonicsConfig config gcfg tcfg
      origin = chunkOriginTile config (chunkCoordFromId (ChunkId key))
      n = chunkTileCount config
      extent = gcWorldExtent gcfg
      edgeCfg = gcOceanEdgeDepth gcfg
      baseHeightRaw = U.generate n (plateHeightAt config seed gcfg tcfg' origin)
      edgeBias = U.generate n (edgeBiasAt config extent edgeCfg origin)
      baseHeight = U.zipWith (+) baseHeightRaw edgeBias
      baseHardness = U.generate n (plateHardnessAt config seed tcfg' origin)
      plateIds = U.generate n (plateIdAt config seed tcfg' origin)
      plateBoundary = U.generate n (plateBoundaryAt config seed tcfg' origin)
      plateCrust = U.generate n (plateCrustAt config seed tcfg' origin)
      plateAge = U.generate n (plateAgeAt config seed tcfg' origin)
      plateVelX = U.generate n (plateVelXAt config seed tcfg' origin)
      plateVelY = U.generate n (plateVelYAt config seed tcfg' origin)
      edgeDelta = U.generate n (tectonicDelta config seed tcfg' origin baseHeight)
      elev' = U.zipWith (+) baseHeight edgeDelta
  in chunk
    { tcElevation = elev'
    , tcHardness = U.map clamp01 baseHardness
    , tcPlateId = plateIds
    , tcPlateBoundary = plateBoundary
    , tcPlateHeight = baseHeight
    , tcPlateHardness = baseHardness
    , tcPlateCrust = plateCrust
    , tcPlateAge = plateAge
    , tcPlateVelX = plateVelX
    , tcPlateVelY = plateVelY
    }

normalizeTectonicsConfig :: WorldConfig -> GenConfig -> TectonicsConfig -> TectonicsConfig
normalizeTectonicsConfig config gcfg tcfg =
  let plateSize = effectivePlateSize config gcfg tcfg
  in tcfg { tcPlateSize = plateSize }

effectivePlateSize :: WorldConfig -> GenConfig -> TectonicsConfig -> Int
effectivePlateSize config gcfg tcfg =
  case tcPlateCount tcfg of
    Nothing -> max minPlateSize (tcPlateSize tcfg)
    Just count ->
      let (rx, ry) = worldExtentRadii (gcWorldExtent gcfg)
          chunksAcrossX = max 1 (rx * chunkDiameterMultiplier + chunkDiameterBias)
          chunksAcrossY = max 1 (ry * chunkDiameterMultiplier + chunkDiameterBias)
          tileSpanX = chunksAcrossX * wcChunkSize config
          tileSpanY = chunksAcrossY * wcChunkSize config
          totalTiles = tileSpanX * tileSpanY
          count' = max minPlateCount count
          rawSize = sqrt (fromIntegral totalTiles / fromIntegral count')
      in max minPlateSize (round rawSize)
  where
    chunkDiameterMultiplier = 2 :: Int
    chunkDiameterBias = 1 :: Int
    minPlateSize = 1
    minPlateCount = 1

tectonicDelta :: WorldConfig -> Word64 -> TectonicsConfig -> TileCoord -> U.Vector Float -> Int -> Float
tectonicDelta config seed tcfg origin elev i =
  let TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
      gx = ox + lx
      gy = oy + ly
      info = plateInfoAtXY seed tcfg gx gy
      infoX = plateInfoAtXY seed tcfg (gx + 1) gy
      infoY = plateInfoAtXY seed tcfg gx (gy + 1)
      strength = boundaryStrengthAtXY seed tcfg gx gy
      e0 = elev U.! i
      boundaryType = classifyBoundary info infoX infoY
      (dirX, dirY) = boundaryDirection info infoX infoY
      motionX = boundaryMotionBetween info infoX
      motionY = boundaryMotionBetween info infoY
      motion = dominantMotion motionX motionY
      motionScale =
        if tcPlateSpeed tcfg <= 0
          then 0
          else clamp01 (abs motion / tcPlateSpeed tcfg)
      ridge = directionalRidge2D (seed + 9001) 4 2 0.55 0.004 (fromIntegral gx) (fromIntegral gy) dirX dirY
      rift = directionalRidge2D (seed + 9103) 3 2 0.6 0.006 (fromIntegral gx) (fromIntegral gy) dirX dirY
  in if strength <= 0 || motionScale <= 0
      then 0
      else
        let s = strength
        in case boundaryType of
          PlateBoundaryConvergent -> applyConvergentEdge tcfg s motionScale ridge e0
          PlateBoundaryDivergent -> applyRiftEdge tcfg s motionScale rift e0
          PlateBoundaryTransform -> applyTransformEdge tcfg s motionScale ridge
          PlateBoundaryNone -> 0

applyConvergentEdge :: TectonicsConfig -> Float -> Float -> Float -> Float -> Float
applyConvergentEdge tcfg strength motionScale ridge elevation =
  if elevation >= 0
    then strength * (tcUplift tcfg + ridge * tcUplift tcfg) * motionScale
    else strength * (-tcTrenchDepth tcfg) * motionScale

applyRiftEdge :: TectonicsConfig -> Float -> Float -> Float -> Float -> Float
applyRiftEdge tcfg strength motionScale rift elevation =
  if elevation >= 0
    then strength * ((-tcRiftDepth tcfg) - rift * tcRiftDepth tcfg) * motionScale
    else strength * (tcRidgeHeight tcfg + rift * tcRidgeHeight tcfg) * motionScale

applyTransformEdge :: TectonicsConfig -> Float -> Float -> Float -> Float
applyTransformEdge tcfg strength motionScale ridge =
  strength * ridge * tcUplift tcfg * tcPlateRidgeStrength tcfg * motionScale

dominantMotion :: Float -> Float -> Float
dominantMotion motionX motionY =
  if abs motionX >= abs motionY then motionX else motionY

boundaryStrengthAtXY :: Word64 -> TectonicsConfig -> Int -> Int -> Float
boundaryStrengthAtXY seed tcfg x y =
  let size = fromIntegral (max 1 (tcPlateSize tcfg))
      (d0, d1) = plateDistancePair seed tcfg x y
      gap = max 0 (d1 - d0)
      sharp = max 0.2 (tcBoundarySharpness tcfg)
      width = max 1e-3 (size * (0.35 / sharp))
      t = clamp01 (1 - gap / width)
  in smoothstep 0 1 t

plateDistancePair :: Word64 -> TectonicsConfig -> Int -> Int -> (Float, Float)
plateDistancePair seed tcfg x y =
  let size = max 1 (tcPlateSize tcfg)
      (wx, wy) = plateWarpXY seed tcfg x y
      fx = wx / fromIntegral size
      fy = wy / fromIntegral size
      cx = floor fx
      cy = floor fy
      cells = [ (ix, iy) | iy <- [cy - 1 .. cy + 1], ix <- [cx - 1 .. cx + 1] ]
      activeCells = filter (uncurry (plateCellActive seed tcfg)) cells
      cells' = if null activeCells then cells else activeCells
      dists = map (plateDistSq seed tcfg (wx, wy)) cells'
      sorted = sortBy (comparing fst) dists
  in case sorted of
      (d0, _) : (d1, _) : _ -> (sqrt d0, sqrt d1)
      (d0, _) : _ -> (sqrt d0, sqrt d0 + fromIntegral size)
      [] -> (0, fromIntegral size)

plateNearestPairAtXY :: Word64 -> TectonicsConfig -> Int -> Int -> (PlateInfo, PlateInfo, Float, Float)
plateNearestPairAtXY seed tcfg x y =
  let size = max 1 (tcPlateSize tcfg)
      (wx, wy) = plateWarpXY seed tcfg x y
      fx = wx / fromIntegral size
      fy = wy / fromIntegral size
      cx = floor fx
      cy = floor fy
      cells = [ (ix, iy) | iy <- [cy - 1 .. cy + 1], ix <- [cx - 1 .. cx + 1] ]
      activeCells = filter (uncurry (plateCellActive seed tcfg)) cells
      cells' = if null activeCells then cells else activeCells
      dists = sortBy (comparing fst) (map (plateDistSq seed tcfg (wx, wy)) cells')
  in case dists of
      (d0, info0) : (d1, info1) : _ -> (info0, info1, sqrt d0, sqrt d1)
      (d0, info0) : _ -> (info0, info0, sqrt d0, sqrt d0 + fromIntegral size)
      [] ->
        let info0 = plateInfoForCell seed tcfg cx cy
        in (info0, info0, 0, fromIntegral size)

plateDistSq :: Word64 -> TectonicsConfig -> (Float, Float) -> (Int, Int) -> (Float, PlateInfo)
plateDistSq seed tcfg (x, y) (cx, cy) =
  let info = plateInfoForCell seed tcfg cx cy
      (px, py) = plateInfoCenter info
      dx = x - px
      dy = y - py
  in (dx * dx + dy * dy, info)

plateIdAt :: WorldConfig -> Word64 -> TectonicsConfig -> TileCoord -> Int -> Word16
plateIdAt config seed tcfg origin i =
  let TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
      gx = ox + lx
      gy = oy + ly
  in plateInfoId (plateInfoAtXY seed tcfg gx gy)

plateBoundaryAt :: WorldConfig -> Word64 -> TectonicsConfig -> TileCoord -> Int -> PlateBoundary
plateBoundaryAt config seed tcfg origin i =
  let TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
      gx = ox + lx
      gy = oy + ly
  in boundaryTypeAtXY seed tcfg gx gy

plateCrustAt :: WorldConfig -> Word64 -> TectonicsConfig -> TileCoord -> Int -> Word16
plateCrustAt config seed tcfg origin i =
  let TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
      gx = ox + lx
      gy = oy + ly
      crust = plateInfoCrust (plateInfoAtXY seed tcfg gx gy)
  in plateCrustCode crust

plateAgeAt :: WorldConfig -> Word64 -> TectonicsConfig -> TileCoord -> Int -> Float
plateAgeAt config seed tcfg origin i =
  let TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
      gx = ox + lx
      gy = oy + ly
  in plateInfoAge (plateInfoAtXY seed tcfg gx gy)

plateVelXAt :: WorldConfig -> Word64 -> TectonicsConfig -> TileCoord -> Int -> Float
plateVelXAt config seed tcfg origin i =
  let TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
      gx = ox + lx
      gy = oy + ly
      (vx, _) = plateInfoVelocity (plateInfoAtXY seed tcfg gx gy)
  in vx

plateVelYAt :: WorldConfig -> Word64 -> TectonicsConfig -> TileCoord -> Int -> Float
plateVelYAt config seed tcfg origin i =
  let TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
      gx = ox + lx
      gy = oy + ly
      (_, vy) = plateInfoVelocity (plateInfoAtXY seed tcfg gx gy)
  in vy

plateHeightAt :: WorldConfig -> Word64 -> GenConfig -> TectonicsConfig -> TileCoord -> Int -> Float
plateHeightAt config seed gcfg tcfg origin i =
  let TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
      gx = ox + lx
      gy = oy + ly
      (infoA, infoB, d0, d1) = plateNearestPairAtXY seed tcfg gx gy
      pidA = plateInfoId infoA
      pidB = plateInfoId infoB
      (lxA, lyA) = plateLocalCoord seed tcfg pidA gx gy
      (lxB, lyB) = plateLocalCoord seed tcfg pidB gx gy
      baseA = sampleBaseHeightAt seed gcfg lxA lyA
      baseB = sampleBaseHeightAt seed gcfg lxB lyB
      plateBaseA = plateInfoBaseHeight infoA
      plateBaseB = plateInfoBaseHeight infoB
      detailScale = max 0.001 (tcPlateDetailScale tcfg)
      detailA = fbm2D (seed + 5555) 4 2 0.5 (lxA * detailScale) (lyA * detailScale)
      detailB = fbm2D (seed + 5555) 4 2 0.5 (lxB * detailScale) (lyB * detailScale)
      ridgeA = ridgedFbm2D (seed + 6666) 4 2 0.55 (lxA * detailScale * 0.8) (lyA * detailScale * 0.8)
      ridgeB = ridgedFbm2D (seed + 6666) 4 2 0.55 (lxB * detailScale * 0.8) (lyB * detailScale * 0.8)
      blend = boundaryStrengthAtXY seed tcfg gx gy
      denom = max 1e-6 (d0 + d1)
      wB = d0 / denom
      baseRaw = lerp baseA baseB (blend * wB)
      plateBase = lerp plateBaseA plateBaseB (blend * wB)
      detailRaw = lerp detailA detailB (blend * wB)
      ridgeRaw = lerp ridgeA ridgeB (blend * wB)
      base = baseRaw * tcPlateHeightVariance tcfg + plateBase
      (px, py) = plateInfoCenter infoA
      dx = fromIntegral gx - px
      dy = fromIntegral gy - py
      dist = sqrt (dx * dx + dy * dy)
      size = fromIntegral (max 1 (tcPlateSize tcfg))
      centerT = clamp01 (1 - dist / (size * max 0.01 (tcCenterFalloffScale tcfg)))
      centerFade = smoothstep 0 1 centerT
      boundaryFade = smoothstep 0 1 (clamp01 (1 - boundaryStrengthAtXY seed tcfg gx gy * tcBoundaryFadeScale tcfg))
      centerWeight = clamp01 (1 - dist / (size * max 0.01 (tcCenterWeightScale tcfg)))
      edgeWeight = clamp01 (dist / (size * max 0.01 (tcEdgeWeightScale tcfg)))
      maxCoordY = fromIntegral (max 1 (worldExtentRadiusY (gcWorldExtent gcfg))) * fromIntegral (wcChunkSize config)
      -- Use real latitude for north/south weights when available.
      (northWeight, southWeight)
        | tcSliceLatExtent tcfg > 0 =
            let latDeg = fromIntegral gy * tcLatDegPerTile tcfg + tcLatBiasDeg tcfg
                latCenter = tcLatBiasDeg tcfg + fromIntegral (wcChunkSize config `div` 2) * tcLatDegPerTile tcfg
                halfExtent = tcSliceLatExtent tcfg / 2
                norm = (latDeg - latCenter) / max 0.001 halfExtent
            in (clamp01 norm, clamp01 (-norm))
        | otherwise =
            -- Fallback: use tile-Y-based weights (pre-Phase 9 behavior).
            ( clamp01 ((-fromIntegral gy) / max 1 maxCoordY)
            , clamp01 ((fromIntegral gy) / max 1 maxCoordY)
            )
      bias = tcPlateBiasStrength tcfg *
        (tcPlateBiasCenter tcfg * centerWeight
        + tcPlateBiasEdge tcfg * edgeWeight
        + tcPlateBiasNorth tcfg * northWeight
        + tcPlateBiasSouth tcfg * southWeight)
      crustBias = case plateInfoCrust infoA of
        PlateContinental -> tcCrustContinentalBias tcfg
        PlateOceanic -> tcCrustOceanicBias tcfg
      crustBiasScaled = crustBias * (0.5 + 0.5 * centerFade)
      interior = lerp base (base * 0.98) (0.15 * (1 - centerFade) + 0.15 * (1 - boundaryFade))
      detail = detailRaw * tcPlateDetailStrength tcfg * (0.5 + 0.5 * centerFade)
      ridge = ridgeRaw * tcPlateRidgeStrength tcfg * (0.4 + 0.6 * boundaryFade)
  in interior + detail + ridge + bias + crustBiasScaled

plateHardnessAt :: WorldConfig -> Word64 -> TectonicsConfig -> TileCoord -> Int -> Float
plateHardnessAt config seed tcfg origin i =
  let TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
      gx = ox + lx
      gy = oy + ly
      info = plateInfoAtXY seed tcfg gx gy
      pid = plateInfoId info
      base = plateInfoBaseHardness info
      local = plateLocalNoise (seed + 1357) pid gx gy
      (px, py) = plateInfoCenter info
      dx = fromIntegral gx - px
      dy = fromIntegral gy - py
      dist = sqrt (dx * dx + dy * dy)
      size = fromIntegral (max 1 (tcPlateSize tcfg))
      centerT = clamp01 (1 - dist / (size * 0.85))
      centerFade = smoothstep 0 1 centerT
      boundaryFade = clamp01 (1 - boundaryStrengthAtXY seed tcfg gx gy)
      detail = local * tcPlateHardnessVariance tcfg * centerFade * boundaryFade
  in clamp01 (base + detail)

edgeBiasAt :: WorldConfig -> WorldExtent -> OceanEdgeDepth -> TileCoord -> Int -> Float
edgeBiasAt config extent edgeCfg origin i =
  let TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
      gx = ox + lx
      gy = oy + ly
  in oceanEdgeBiasAt config extent edgeCfg (TileCoord gx gy)

plateLocalNoise :: Word64 -> Word16 -> Int -> Int -> Float
plateLocalNoise seed pid gx gy =
  let s = seed + fromIntegral pid * 4099
  in fbm2D s 4 2 0.5 (fromIntegral gx * 0.01) (fromIntegral gy * 0.01)

plateLocalCoord :: Word64 -> TectonicsConfig -> Word16 -> Int -> Int -> (Float, Float)
plateLocalCoord seed tcfg pid gx gy =
  let (wx, wy) = plateWarpXY seed tcfg gx gy
      scale = max 0.0001 (tcBoundaryNoiseScale tcfg * 0.8)
      strength = tcBoundaryNoiseStrength tcfg * 0.6
      (lx, ly) = warpOctaves (tcBoundaryWarpOctaves tcfg) (tcBoundaryWarpLacunarity tcfg) (tcBoundaryWarpGain tcfg)
        (seed + fromIntegral pid * 977) scale strength wx wy
      size = fromIntegral (max 1 (tcPlateSize tcfg))
      ox = (noise2DContinuous (seed + fromIntegral pid * 131) 0 0 - 0.5) * size * 1.2
      oy = (noise2DContinuous (seed + fromIntegral pid * 131 + 7) 0 0 - 0.5) * size * 1.2
  in (lx + ox, ly + oy)

data PlateInfo = PlateInfo
  { plateInfoId :: !Word16
  , plateInfoCenter :: !(Float, Float)
  , plateInfoVelocity :: !(Float, Float)
  , plateInfoBaseHeight :: !Float
  , plateInfoBaseHardness :: !Float
  , plateInfoAge :: !Float
  , plateInfoCrust :: !PlateCrust
  }

boundaryTypeAtXY :: Word64 -> TectonicsConfig -> Int -> Int -> PlateBoundary
boundaryTypeAtXY seed tcfg gx gy =
  let info = plateInfoAtXY seed tcfg gx gy
      infoX = plateInfoAtXY seed tcfg (gx + 1) gy
      infoY = plateInfoAtXY seed tcfg gx (gy + 1)
  in classifyBoundary info infoX infoY

plateCrustCode :: PlateCrust -> Word16
plateCrustCode crust =
  case crust of
    PlateOceanic -> 0
    PlateContinental -> 1

plateInfoAtXY :: Word64 -> TectonicsConfig -> Int -> Int -> PlateInfo
plateInfoAtXY seed tcfg x y =
  let (wx, wy) = plateWarpXY seed tcfg x y
      size = max 1 (tcPlateSize tcfg)
      fx = wx / fromIntegral size
      fy = wy / fromIntegral size
      cx = floor fx
      cy = floor fy
      cells = [ (ix, iy) | iy <- [cy - 1 .. cy + 1], ix <- [cx - 1 .. cx + 1] ]
      activeCells = filter (uncurry (plateCellActive seed tcfg)) cells
      cells' = if null activeCells then cells else activeCells
      pickNearest = foldl' (chooseNearest (wx, wy) seed tcfg) Nothing cells'
  in case pickNearest of
      Just (_, info) -> info
      Nothing -> plateInfoForCell seed tcfg cx cy

chooseNearest :: (Float, Float) -> Word64 -> TectonicsConfig -> Maybe (Float, PlateInfo) -> (Int, Int) -> Maybe (Float, PlateInfo)
chooseNearest (x, y) seed tcfg best (cx, cy) =
  let info = plateInfoForCell seed tcfg cx cy
      (px, py) = plateInfoCenter info
      dx = x - px
      dy = y - py
      d2 = dx * dx + dy * dy
  in case best of
      Nothing -> Just (d2, info)
      Just (d2Best, bestInfo) -> if d2 < d2Best then Just (d2, info) else Just (d2Best, bestInfo)

plateInfoForCell :: Word64 -> TectonicsConfig -> Int -> Int -> PlateInfo
plateInfoForCell seed tcfg cx cy =
  let size = fromIntegral (max 1 (tcPlateSize tcfg))
      jx = noise2DContinuous (seed + 1001) (fromIntegral cx * 0.9) (fromIntegral cy * 0.9)
      jy = noise2DContinuous (seed + 2003) (fromIntegral cx * 0.9 + 11.7) (fromIntegral cy * 0.9 - 3.4)
      jitterN = noise2DContinuous (seed + 11001) (fromIntegral cx * 0.43) (fromIntegral cy * 0.43)
      jitter = min 1.1 (0.6 + jitterN * 0.6)
      (wcx, wcy) = plateWarpXY (seed + 12345) tcfg (round (fromIntegral cx * size)) (round (fromIntegral cy * size))
      baseCx = wcx / size
      baseCy = wcy / size
      center = ((baseCx + (jx - 0.5) * jitter) * size, (baseCy + (jy - 0.5) * jitter) * size)
      angle = noise2DContinuous (seed + 3001) (fromIntegral cx + 2.3) (fromIntegral cy - 9.1) * 2 * pi
      speed = tcPlateSpeed tcfg
      velocity = (cos angle * speed, sin angle * speed)
      pid = hashPlateId seed cx cy
      pid' = mergePlateId seed tcfg cx cy pid
      baseHeight = plateBaseHeight seed tcfg cx cy
      baseHardness = plateBaseHardness seed tcfg cx cy
      age = plateAge seed tcfg cx cy
      crust = plateCrust seed tcfg cx cy baseHeight
  in PlateInfo
      { plateInfoId = pid'
      , plateInfoCenter = center
      , plateInfoVelocity = velocity
      , plateInfoBaseHeight = baseHeight
      , plateInfoBaseHardness = baseHardness
      , plateInfoAge = age
      , plateInfoCrust = crust
      }

plateCellActive :: Word64 -> TectonicsConfig -> Int -> Int -> Bool
plateCellActive seed tcfg cx cy =
  let scale = max 0.01 (tcPlateMergeScale tcfg)
      n0 = noise2DContinuous (seed + 91011) (fromIntegral cx * scale) (fromIntegral cy * scale)
  in n0 >= tcPlateMergeBias tcfg

plateWarpXY :: Word64 -> TectonicsConfig -> Int -> Int -> (Float, Float)
plateWarpXY seed tcfg x y =
  let scale = max 0.0001 (tcBoundaryNoiseScale tcfg)
      strength = tcBoundaryNoiseStrength tcfg
      octaves = max 1 (tcBoundaryWarpOctaves tcfg)
      lac = max 1.2 (tcBoundaryWarpLacunarity tcfg)
      gain = clamp01 (tcBoundaryWarpGain tcfg)
  in warpOctaves octaves lac gain (seed + 7777) scale strength (fromIntegral x) (fromIntegral y)

warpOctaves :: Int -> Float -> Float -> Word64 -> Float -> Float -> Float -> Float -> (Float, Float)
warpOctaves octaves lac gain seed scale strength x y =
  let step 0 _ _ accX accY = (x + accX * strength, y + accY * strength)
      step o freq amp accX accY =
        let (wx, wy) = domainWarp2D (seed + fromIntegral o) (scale * freq) 1 x y
            dx = (wx - x) * amp
            dy = (wy - y) * amp
        in step (o - 1) (freq * lac) (amp * gain) (accX + dx) (accY + dy)
  in step octaves 1 1 0 0

mergePlateId :: Word64 -> TectonicsConfig -> Int -> Int -> Word16 -> Word16
mergePlateId seed tcfg cx cy pid =
  let scale = max 0.01 (tcPlateMergeScale tcfg)
      bias = clamp01 (tcPlateMergeBias tcfg)
      n0 = noise2DContinuous (seed + 91011) (fromIntegral cx * scale) (fromIntegral cy * scale)
      n1 = noise2DContinuous (seed + 92021) (fromIntegral cx * scale + 7.7) (fromIntegral cy * scale - 2.1)
      bucket = floor (n0 * 8) :: Int
      salt = fromIntegral ((bucket `mod` 8) * 8191) :: Word16
  in if n1 > bias then pid else pid `xor` salt

plateBaseHeight :: Word64 -> TectonicsConfig -> Int -> Int -> Float
plateBaseHeight seed tcfg cx cy =
  let n0 = noise2DContinuous (seed + 5001) (fromIntegral cx * 0.37) (fromIntegral cy * 0.37) * 2 - 1
  in tcPlateHeightBase tcfg + n0 * tcPlateHeightVariance tcfg

plateBaseHardness :: Word64 -> TectonicsConfig -> Int -> Int -> Float
plateBaseHardness seed tcfg cx cy =
  let n0 = noise2DContinuous (seed + 6001) (fromIntegral cx * 0.29) (fromIntegral cy * 0.29) * 2 - 1
  in clamp01 (tcPlateHardnessBase tcfg + n0 * tcPlateHardnessVariance tcfg)

plateAge :: Word64 -> TectonicsConfig -> Int -> Int -> Float
plateAge seed _ cx cy =
  noise2DContinuous (seed + 7001) (fromIntegral cx * 0.19) (fromIntegral cy * 0.19)

plateCrust :: Word64 -> TectonicsConfig -> Int -> Int -> Float -> PlateCrust
plateCrust seed _ cx cy baseHeight =
  let n0 = noise2DContinuous (seed + 8001) (fromIntegral cx * 0.23) (fromIntegral cy * 0.23)
  in if baseHeight + (n0 - 0.5) * 0.2 >= 0 then PlateContinental else PlateOceanic

hashPlateId :: Word64 -> Int -> Int -> Word16
hashPlateId seed x y =
  let n0 = noise2DContinuous (seed + 424242) (fromIntegral x * 0.13) (fromIntegral y * 0.13)
  in fromIntegral (floor (n0 * 65535))

classifyBoundary :: PlateInfo -> PlateInfo -> PlateInfo -> PlateBoundary
classifyBoundary info infoX infoY =
  let typeX = boundaryTypeBetween info infoX
      typeY = boundaryTypeBetween info infoY
  in if typeX == typeY then typeX else PlateBoundaryTransform

boundaryDirection :: PlateInfo -> PlateInfo -> PlateInfo -> (Float, Float)
boundaryDirection info infoX infoY =
  let (ax, ay) = plateInfoCenter info
      (bx, by) = plateInfoCenter infoX
      (cx, cy) = plateInfoCenter infoY
      dx = bx - ax
      dy = by - ay
      ex = cx - ax
      ey = cy - ay
      nx = dx + ex
      ny = dy + ey
      len = sqrt (nx * nx + ny * ny)
  in if len == 0 then (1, 0) else (nx / len, ny / len)

boundaryMotionBetween :: PlateInfo -> PlateInfo -> Float
boundaryMotionBetween a b =
  let (ax, ay) = plateInfoCenter a
      (bx, by) = plateInfoCenter b
      (vax, vay) = plateInfoVelocity a
      (vbx, vby) = plateInfoVelocity b
      nx0 = bx - ax
      ny0 = by - ay
      len = sqrt (nx0 * nx0 + ny0 * ny0)
      nx = if len == 0 then 0 else nx0 / len
      ny = if len == 0 then 0 else ny0 / len
      rvx = vbx - vax
      rvy = vby - vay
  in rvx * nx + rvy * ny

boundaryTypeBetween :: PlateInfo -> PlateInfo -> PlateBoundary
boundaryTypeBetween a b =
  let rel = boundaryMotionBetween a b
      eps = 0.05
  in if rel < -eps
      then PlateBoundaryConvergent
      else if rel > eps
        then PlateBoundaryDivergent
        else PlateBoundaryTransform
