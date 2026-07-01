{-# LANGUAGE BangPatterns #-}
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
import Control.Monad.ST (runST)
import Data.Word (Word16, Word64)
import qualified Data.IntMap.Strict as IntMap
import Topo.BaseHeight (GenConfig(..), defaultGenConfig, oceanEdgeBiasAt, sampleBaseHeightAt)
import Topo.Math (clamp01, lerp, smoothstep)
import Topo.Noise (directionalRidge2D, directionalRidge2DAniso, domainWarp2D, fbm2D, noise2DContinuous, ridgedFbm2D)
import Topo.Pipeline (PipelineStage(..))
import Topo.Pipeline.Stage (StageId(..))
import Topo.Planet (LatitudeMapping(..))
import Topo.Plugin (logInfo, modifyWorldP, peSeed)
import Topo.Tectonics.Boundary
  ( boundaryDirection
  , boundaryDistanceFromPair
  , boundaryDistanceNormalised
  , boundaryMotionBetween
  , boundaryTangent
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
import qualified Data.Vector.Unboxed.Mutable as UM

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
      fields = buildTectonicsChunkFields config seed gcfg lm tcfg' origin n
  in chunk
    { tcElevation = tcfElevation fields
    , tcHardness = tcfHardness fields
    , tcPlateId = tcfPlateId fields
    , tcPlateBoundary = tcfPlateBoundary fields
    , tcPlateHeight = tcfPlateHeight fields
    , tcPlateHardness = tcfPlateHardness fields
    , tcPlateCrust = tcfPlateCrust fields
    , tcPlateAge = tcfPlateAge fields
    , tcPlateVelX = tcfPlateVelX fields
    , tcPlateVelY = tcfPlateVelY fields
    }

-- | Strict terrain vectors produced by the tectonics pass.
data TectonicsChunkFields = TectonicsChunkFields
  { tcfElevation :: !(U.Vector Float)
  , tcfHardness :: !(U.Vector Float)
  , tcfPlateId :: !(U.Vector Word16)
  , tcfPlateBoundary :: !(U.Vector PlateBoundary)
  , tcfPlateHeight :: !(U.Vector Float)
  , tcfPlateHardness :: !(U.Vector Float)
  , tcfPlateCrust :: !(U.Vector Word16)
  , tcfPlateAge :: !(U.Vector Float)
  , tcfPlateVelX :: !(U.Vector Float)
  , tcfPlateVelY :: !(U.Vector Float)
  }

buildTectonicsChunkFields :: WorldConfig -> Word64 -> GenConfig -> LatitudeMapping -> TectonicsConfig -> TileCoord -> Int -> TectonicsChunkFields
buildTectonicsChunkFields config seed gcfg lm tcfg origin n = runST $ do
  mElevation <- UM.unsafeNew n
  mPlateId <- UM.unsafeNew n
  mPlateBoundary <- UM.unsafeNew n
  mPlateHeight <- UM.unsafeNew n
  mPlateHardness <- UM.unsafeNew n
  mPlateCrust <- UM.unsafeNew n
  mPlateAge <- UM.unsafeNew n
  mPlateVelX <- UM.unsafeNew n
  mPlateVelY <- UM.unsafeNew n

  let extent = gcWorldExtent gcfg
      edgeCfg = gcOceanEdgeDepth gcfg
      go !i
        | i >= n = pure ()
        | otherwise = do
            let !sample = plateSampleAt config seed gcfg tcfg origin i
                !baseHeightRaw = plateHeightFromSample config seed gcfg lm tcfg sample
                !edgeBias = oceanEdgeBiasAt config extent edgeCfg (TileCoord (psX sample) (psY sample))
                !baseHeight = baseHeightRaw + edgeBias
                !baseHardness = plateHardnessFromSample seed tcfg sample
                !edgeDelta = tectonicDeltaFromSample seed tcfg sample baseHeight
                -- Clamp to [0,1] after stacking base height + tectonic delta.
                -- Raw values routinely exceed this range; downstream stages
                -- (erosion, hydrology, climate) assume normalised elevation.
                !elevation = clamp01 (baseHeight + edgeDelta)
                !plateId = psPlateId sample
                !plateBoundary = psBoundaryType sample
                !plateCrust = plateCrustCode (psPlateCrust sample)
                !plateAge = psPlateAge sample
                (!plateVelX, !plateVelY) = psPlateVelocity sample
            UM.unsafeWrite mElevation i elevation
            UM.unsafeWrite mPlateId i plateId
            UM.unsafeWrite mPlateBoundary i plateBoundary
            UM.unsafeWrite mPlateHeight i baseHeight
            UM.unsafeWrite mPlateHardness i baseHardness
            UM.unsafeWrite mPlateCrust i plateCrust
            UM.unsafeWrite mPlateAge i plateAge
            UM.unsafeWrite mPlateVelX i plateVelX
            UM.unsafeWrite mPlateVelY i plateVelY
            go (i + 1)

  go 0
  elevation <- U.unsafeFreeze mElevation
  plateId <- U.unsafeFreeze mPlateId
  plateBoundary <- U.unsafeFreeze mPlateBoundary
  plateHeight <- U.unsafeFreeze mPlateHeight
  plateHardness <- U.unsafeFreeze mPlateHardness
  plateCrust <- U.unsafeFreeze mPlateCrust
  plateAge <- U.unsafeFreeze mPlateAge
  plateVelX <- U.unsafeFreeze mPlateVelX
  plateVelY <- U.unsafeFreeze mPlateVelY
  pure TectonicsChunkFields
    { tcfElevation = elevation
      -- plateHardnessFromSample already clamps; both fields can share
      -- the same immutable vector.
    , tcfHardness = plateHardness
    , tcfPlateId = plateId
    , tcfPlateBoundary = plateBoundary
    , tcfPlateHeight = plateHeight
    , tcfPlateHardness = plateHardness
    , tcfPlateCrust = plateCrust
    , tcfPlateAge = plateAge
    , tcfPlateVelX = plateVelX
    , tcfPlateVelY = plateVelY
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

tectonicDeltaFromSample :: Word64 -> TectonicsConfig -> PlateSample -> Float -> Float
tectonicDeltaFromSample seed tcfg sample e0 =
  let gx = psX sample
      gy = psY sample
      strength = psBoundaryStrength sample
      -- Raw linear distance (before smoothstep) for the rift profile.
      rawDist = psBoundaryRawDistance sample
      (dirX, dirY) = psBoundaryDirection sample
      motionX = psMotionX sample
      motionY = psMotionY sample
      motion = dominantMotion motionX motionY
      motionScale =
        if tcPlateSpeed tcfg <= 0
          then 0
          else clamp01 (abs motion / tcPlateSpeed tcfg)
      ridge = directionalRidge2D (seed + 9001) 4 2 0.55 0.004 (fromIntegral gx) (fromIntegral gy) dirX dirY
      -- Rift uses boundary tangent (Phase 2) and anisotropic noise (Phase 3)
      -- to produce elongated features that follow the plate boundary.
      (tanX, tanY) = psBoundaryTangent sample
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
        in case psBoundaryType sample of
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


-- | Internal per-tile plate snapshot shared by terrain field synthesis.
data PlateSample = PlateSample
  { psX :: !Int
  , psY :: !Int
  , psInfo :: !PlateInfo
  , psNeighbourInfo :: !PlateInfo
  , psDistance :: !Float
  , psNeighbourDistance :: !Float
  , psBoundaryRawDistance :: !Float
  , psBoundaryStrength :: !Float
  , psLocalA :: !(Float, Float)
  , psLocalB :: !(Float, Float)
  , psBaseA :: !Float
  , psBaseB :: !Float
  , psPlateId :: !Word16
  , psPlateVelocity :: !(Float, Float)
  , psPlateBaseA :: !Float
  , psPlateBaseB :: !Float
  , psPlateBaseHardness :: !Float
  , psPlateAge :: !Float
  , psPlateCrust :: !PlateCrust
  , psCenterDistance :: !Float
  , psTileCenterDistance :: !Float
  , psBoundaryType :: !PlateBoundary
  , psBoundaryDirection :: !(Float, Float)
  , psMotionX :: !Float
  , psMotionY :: !Float
  , psBoundaryTangent :: !(Float, Float)
  }

plateSampleAt :: WorldConfig -> Word64 -> GenConfig -> TectonicsConfig -> TileCoord -> Int -> PlateSample
plateSampleAt config seed gcfg tcfg origin i =
  let TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
      gx = ox + lx
      gy = oy + ly
      (infoA, infoB, d0, d1) = plateNearestPairAtXY seed tcfg gx gy
      tileInfo = plateInfoAtXY seed tcfg gx gy
      infoX = plateInfoAtXY seed tcfg (gx + 1) gy
      infoY = plateInfoAtXY seed tcfg gx (gy + 1)
      pidA = plateInfoId infoA
      pidB = plateInfoId infoB
      localA@(lxA, lyA) = plateLocalCoord seed tcfg pidA gx gy
      localB@(lxB, lyB) = plateLocalCoord seed tcfg pidB gx gy
      baseA = sampleBaseHeightAt seed gcfg lxA lyA
      baseB = sampleBaseHeightAt seed gcfg lxB lyB
      rawDistance = boundaryDistanceFromPair tcfg d0 d1
      strength = smoothstep 0 1 rawDistance
      (px, py) = plateInfoCenter infoA
      dx = fromIntegral gx - px
      dy = fromIntegral gy - py
      dist = sqrt (dx * dx + dy * dy)
      (tilePx, tilePy) = plateInfoCenter tileInfo
      tileDx = fromIntegral gx - tilePx
      tileDy = fromIntegral gy - tilePy
      tileDist = sqrt (tileDx * tileDx + tileDy * tileDy)
      motionX = boundaryMotionBetween tileInfo infoX
      motionY = boundaryMotionBetween tileInfo infoY
      nearestNeighbour = if abs motionX >= abs motionY then infoX else infoY
  in PlateSample
      { psX = gx
      , psY = gy
      , psInfo = infoA
      , psNeighbourInfo = infoB
      , psDistance = d0
      , psNeighbourDistance = d1
      , psBoundaryRawDistance = rawDistance
      , psBoundaryStrength = strength
      , psLocalA = localA
      , psLocalB = localB
      , psBaseA = baseA
      , psBaseB = baseB
      , psPlateId = plateInfoId tileInfo
      , psPlateVelocity = plateInfoVelocity tileInfo
      , psPlateBaseA = plateInfoBaseHeight infoA
      , psPlateBaseB = plateInfoBaseHeight infoB
      , psPlateBaseHardness = plateInfoBaseHardness tileInfo
      , psPlateAge = plateInfoAge tileInfo
      , psPlateCrust = plateInfoCrust tileInfo
      , psCenterDistance = dist
      , psTileCenterDistance = tileDist
      , psBoundaryType = classifyBoundary tileInfo infoX infoY
      , psBoundaryDirection = boundaryDirection tileInfo infoX infoY
      , psMotionX = motionX
      , psMotionY = motionY
      , psBoundaryTangent = boundaryTangent tileInfo nearestNeighbour
      }

plateHeightFromSample :: WorldConfig -> Word64 -> GenConfig -> LatitudeMapping -> TectonicsConfig -> PlateSample -> Float
plateHeightFromSample config seed gcfg lm tcfg sample =
  let gx = psX sample
      gy = psY sample
      d0 = psDistance sample
      d1 = psNeighbourDistance sample
      (lxA, lyA) = psLocalA sample
      (lxB, lyB) = psLocalB sample
      baseA = psBaseA sample
      baseB = psBaseB sample
      plateBaseA = psPlateBaseA sample
      plateBaseB = psPlateBaseB sample
      detailScale = max 0.001 (tcPlateDetailScale tcfg)
      detailA = fbm2D (seed + 5555) 4 2 0.5 (lxA * detailScale) (lyA * detailScale)
      detailB = fbm2D (seed + 5555) 4 2 0.5 (lxB * detailScale) (lyB * detailScale)
      ridgeA = ridgedFbm2D (seed + 6666) 4 2 0.55 (lxA * detailScale * 0.8) (lyA * detailScale * 0.8)
      ridgeB = ridgedFbm2D (seed + 6666) 4 2 0.55 (lxB * detailScale * 0.8) (lyB * detailScale * 0.8)
      blend = psBoundaryStrength sample
      denom = max 1e-6 (d0 + d1)
      wB = d0 / denom
      baseRaw = lerp baseA baseB (blend * wB)
      plateBase = lerp plateBaseA plateBaseB (blend * wB)
      detailRaw = lerp detailA detailB (blend * wB)
      ridgeRaw = lerp ridgeA ridgeB (blend * wB)
      base = baseRaw * tcPlateHeightVariance tcfg + plateBase
      dist = psCenterDistance sample
      size = fromIntegral (max 1 (tcPlateSize tcfg))
      centerT = clamp01 (1 - dist / (size * max 0.01 (tcCenterFalloffScale tcfg)))
      centerFade = smoothstep 0 1 centerT
      boundaryFade = smoothstep 0 1 (clamp01 (1 - blend * tcBoundaryFadeScale tcfg))
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
      crustBias = case plateInfoCrust (psInfo sample) of
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

plateHardnessFromSample :: Word64 -> TectonicsConfig -> PlateSample -> Float
plateHardnessFromSample seed tcfg sample =
  let gx = psX sample
      gy = psY sample
      pid = psPlateId sample
      base = psPlateBaseHardness sample
      local = plateLocalNoise (seed + 1357) pid gx gy
      dist = psTileCenterDistance sample
      size = fromIntegral (max 1 (tcPlateSize tcfg))
      centerT = clamp01 (1 - dist / (size * 0.85))
      centerFade = smoothstep 0 1 centerT
      boundaryFade = clamp01 (1 - psBoundaryStrength sample)
      detail = local * tcPlateHardnessVariance tcfg * centerFade * boundaryFade
  in clamp01 (base + detail)

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
