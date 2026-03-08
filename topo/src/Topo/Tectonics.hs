{-# LANGUAGE OverloadedStrings #-}

-- | Plate tectonics configuration and terrain synthesis.
module Topo.Tectonics
  ( TectonicsConfig(..)
  , defaultTectonicsConfig
  , generateTectonicsStage
  , applyTectonicsChunk
  -- * Rift helpers
  , riftProfile
  , boundaryTangent
  , boundaryDistanceNormalised
  -- * Internal types (for testing)
  , PlateInfo(..)
  ) where

import Control.Monad.Reader (asks)
import Data.Word (Word16, Word64)
import qualified Data.IntMap.Strict as IntMap
import Topo.BaseHeight (GenConfig(..), OceanEdgeDepth, defaultGenConfig, oceanEdgeBiasAt, sampleBaseHeightAt)
import Topo.Math (clamp01, lerp, smoothstep)
import Topo.Noise (directionalRidge2D, directionalRidge2DAniso, domainWarp2D, fbm2D, noise2DContinuous, ridgedFbm2D)
import Topo.Pipeline (PipelineStage(..))
import Topo.Pipeline.Stage (StageId(..))
import Topo.Planet (LatitudeMapping(..))
import Topo.Plugin (logInfo, modifyWorldP, peSeed)
import Topo.Tectonics.Boundary
  ( boundaryDirection
  , boundaryDistanceNormalised
  , boundaryMotionBetween
  , boundaryStrengthAtXY
  , boundaryTangent
  , boundaryTypeAtXY
  , classifyBoundary
  )
import Topo.Tectonics.Config (TectonicsConfig(..), defaultTectonicsConfig)
import Topo.Tectonics.PlateVoronoi
  ( PlateInfo(..)
  , plateInfoAtXY
  , plateNearestPairAtXY
  , plateWarpXY
  , warpOctaves
  )
import Topo.Types
import Topo.World (TerrainWorld(..))
import qualified Data.Vector.Unboxed as U

-- | Apply tectonic generation to the existing terrain chunks in a world.
--
-- Uses the pre-computed 'LatitudeMapping' from 'TerrainWorld' so that
-- north/south plate bias is latitude-aware for non-equator slices.
generateTectonicsStage :: TectonicsConfig -> PipelineStage
generateTectonicsStage cfg = PipelineStage StageTectonics "generateTectonics" "generateTectonics" Nothing [] Nothing $ do
  logInfo "generateTectonics: generating plate terrain"
  seed <- asks peSeed
  modifyWorldP $ \world ->
    let config = twConfig world
        lm     = twLatMapping world
        terrain' = IntMap.mapWithKey (applyTectonicsChunk config seed defaultGenConfig lm cfg) (twTerrain world)
    in world { twTerrain = terrain' }

-- | Apply tectonic shaping for a single terrain chunk.
applyTectonicsChunk :: WorldConfig -> Word64 -> GenConfig -> LatitudeMapping -> TectonicsConfig -> Int -> TerrainChunk -> TerrainChunk
applyTectonicsChunk config seed gcfg lm tcfg key chunk =
  let tcfg' = normalizeTectonicsConfig config gcfg tcfg
      origin = chunkOriginTile config (chunkCoordFromId (ChunkId key))
      n = chunkTileCount config
      extent = gcWorldExtent gcfg
      edgeCfg = gcOceanEdgeDepth gcfg
      baseHeightRaw = U.generate n (plateHeightAt config seed gcfg lm tcfg' origin)
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
      -- Clamp to [0,1] after stacking base height + tectonic delta.
      -- Raw values routinely exceed this range; downstream stages
      -- (erosion, hydrology, climate) assume normalised elevation.
      elev' = U.map clamp01 (U.zipWith (+) baseHeight edgeDelta)
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
      -- Raw linear distance (before smoothstep) for the rift profile.
      rawDist = boundaryDistanceNormalised seed tcfg gx gy
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
      -- Rift uses boundary tangent (Phase 2) and anisotropic noise (Phase 3)
      -- to produce elongated features that follow the plate boundary.
      nearestNeighbour = if abs motionX >= abs motionY then infoX else infoY
      (tanX, tanY) = boundaryTangent info nearestNeighbour
      riftScaleAlong = tcRiftNoiseScale tcfg
      riftScaleAcross = tcRiftNoiseScale tcfg * max 1 (tcRiftElongation tcfg)
      rift = directionalRidge2DAniso
               (seed + 9103)
               (tcRiftNoiseOctaves tcfg)
               (tcRiftNoiseLacunarity tcfg)
               (tcRiftNoiseGain tcfg)
               riftScaleAlong
               riftScaleAcross
               (fromIntegral gx) (fromIntegral gy)
               tanX tanY
  in if strength <= 0 || motionScale <= 0
      then 0
      else
        let s = strength
        in case boundaryType of
          PlateBoundaryConvergent -> applyConvergentEdge tcfg s motionScale ridge e0
          PlateBoundaryDivergent  -> applyRiftEdge tcfg rawDist motionScale rift e0
          PlateBoundaryTransform  -> applyTransformEdge tcfg s motionScale ridge
          PlateBoundaryNone       -> 0

applyConvergentEdge :: TectonicsConfig -> Float -> Float -> Float -> Float -> Float
applyConvergentEdge tcfg strength motionScale ridge elevation =
  if elevation >= 0
    then strength * (tcUplift tcfg + ridge * tcUplift tcfg) * motionScale
    else strength * (-tcTrenchDepth tcfg) * motionScale

-- | Apply rift (divergent boundary) effects with a graben+horst profile.
--
-- Continental rifts produce a depression modulated by the rift profile;
-- oceanic divergent boundaries produce a mid-ocean ridge uplift.
-- The total delta is clamped so that it never exceeds the configured
-- depth budget (@tcRiftDepth + tcRiftShoulderHeight@).
--
-- The first parameter after the config is the *raw normalised distance*
-- from 'boundaryDistanceNormalised' (linear, before smoothstep), which
-- feeds into 'riftProfile' for a proper cross-section shape.
applyRiftEdge :: TectonicsConfig -> Float -> Float -> Float -> Float -> Float
applyRiftEdge tcfg rawDist motionScale rift elevation =
  if elevation >= 0
    then
      -- Continental rift: graben profile modulated by noise.
      let profile = riftProfile tcfg rawDist
          rawDelta = profile * tcRiftDepth tcfg * (0.5 + 0.5 * rift) * motionScale
          -- Safety clamp: never exceed configured depth budget.
          maxDepth = tcRiftDepth tcfg + tcRiftShoulderHeight tcfg
      in max (-maxDepth) (min maxDepth rawDelta)
    else
      -- Oceanic divergent: mid-ocean ridge uplift (uses smoothstepped
      -- strength implicitly via rawDist for a softer falloff).
      let s = smoothstep 0 1 rawDist
      in s * (tcRidgeHeight tcfg + rift * tcRidgeHeight tcfg) * motionScale

-- | Pure rift cross-section profile.
--
-- Maps a boundary strength value (0 = boundary edge, 1 = boundary centre)
-- to an elevation-delta multiplier:
--
--   * Centre (strength close to 1): flat floor at @-1@.
--   * Ramp zone: smooth transition from @-1@ up to @0@.
--   * Shoulder zone (strength near 0): positive hump peaking at
--     @tcRiftShoulderHeight / tcRiftDepth@ (normalised).
--   * Outside (strength = 0): @0@.
--
-- The profile is symmetric around the boundary centre by construction
-- since 'boundaryStrengthAtXY' is itself symmetric about the
-- nearest-plate midpoint.
riftProfile :: TectonicsConfig -> Float -> Float
riftProfile tcfg strength
  | strength <= 0 = 0
  | otherwise =
      let floorW = clamp01 (tcRiftFloorWidth tcfg)
          shoulderW = clamp01 (tcRiftShoulderWidth tcfg)
          shoulderH = tcRiftShoulderHeight tcfg / max 1e-6 (tcRiftDepth tcfg)
          -- strength ∈ (0,1]: 1 = boundary centre, 0 = boundary edge.
          -- Invert to get a distance-from-centre measure: 0 = centre, 1 = edge.
          d = 1 - strength
      in if d <= floorW / 2
           -- Flat bottom zone
           then -1
         else if d <= 1 - shoulderW
           -- Ramp from -1 up to 0
           then let t = (d - floorW / 2) / max 1e-6 (1 - shoulderW - floorW / 2)
                in lerp (-1) 0 (smoothstep 0 1 t)
         else if d < 1
           -- Shoulder hump (positive)
           then let t = (d - (1 - shoulderW)) / max 1e-6 shoulderW
                    -- Bell-shaped shoulder: rises to peak then falls.
                    bell = sin (t * pi)
                in shoulderH * bell
         else 0

applyTransformEdge :: TectonicsConfig -> Float -> Float -> Float -> Float
applyTransformEdge tcfg strength motionScale ridge =
  strength * ridge * tcUplift tcfg * tcPlateRidgeStrength tcfg * motionScale

dominantMotion :: Float -> Float -> Float
dominantMotion motionX motionY =
  if abs motionX >= abs motionY then motionX else motionY


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

plateHeightAt :: WorldConfig -> Word64 -> GenConfig -> LatitudeMapping -> TectonicsConfig -> TileCoord -> Int -> Float
plateHeightAt config seed gcfg lm tcfg origin i =
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
        | lmLatExtent lm > 0 =
            let latDeg = fromIntegral gy * lmDegPerTile lm + lmBiasDeg lm
                latCenter = lmBiasDeg lm + fromIntegral (wcChunkSize config `div` 2) * lmDegPerTile lm
                halfExtent = lmLatExtent lm / 2
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
      -- Scale detail/ridge noise by distance from sea level:
      -- coastal/lowland plates get 20% amplitude, mountains get full.
      -- This keeps plains flat while mountains stay rough.
      elevAboveSea = clamp01 ((base - 0.5) * 3)
      detail = detailRaw * tcPlateDetailStrength tcfg * (0.2 + 0.8 * elevAboveSea) * (0.5 + 0.5 * centerFade)
      ridge = ridgeRaw * tcPlateRidgeStrength tcfg * (0.1 + 0.9 * elevAboveSea) * (0.4 + 0.6 * boundaryFade)
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

plateCrustCode :: PlateCrust -> Word16
plateCrustCode crust =
  case crust of
    PlateOceanic -> 0
    PlateContinental -> 1
