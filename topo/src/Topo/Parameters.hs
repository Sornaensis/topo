{-# LANGUAGE OverloadedStrings #-}

-- | Derived terrain parameter layers.
module Topo.Parameters
  ( ParameterConfig(..)
  , defaultParameterConfig
  , applyParameterLayersStage
  ) where

import Control.Monad.Reader (asks)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Word (Word16, Word64)
import Topo.Math (clamp01)
import Topo.Noise (noise2D)
import Topo.Pipeline (PipelineStage(..))
import Topo.Plugin (logInfo, modifyWorldP, peSeed)
import Topo.Types
import Topo.World (TerrainWorld(..))
import qualified Data.Vector.Unboxed as U

-- | Parameter derivation configuration.
data ParameterConfig = ParameterConfig
  { pcDetailScale :: !Float
  , pcHardnessBaseWeight :: !Float
  , pcHardnessNoiseWeight :: !Float
  , pcRoughnessScale :: !Float
  , pcFertilityMoistureWeight :: !Float
  , pcFertilitySlopeWeight :: !Float
  , pcRockElevationThreshold :: !Float
  , pcRockHardnessThreshold :: !Float
  , pcRockHardnessSecondary :: !Float
  , pcSoilMoistureThreshold :: !Float
  , pcSoilSlopeThreshold :: !Float
  } deriving (Eq, Show)

-- | Default parameter configuration.
defaultParameterConfig :: ParameterConfig
defaultParameterConfig = ParameterConfig
  { pcDetailScale = 1
  , pcHardnessBaseWeight = 0.7
  , pcHardnessNoiseWeight = 0.3
  , pcRoughnessScale = 0.75
  , pcFertilityMoistureWeight = 0.6
  , pcFertilitySlopeWeight = 0.4
  , pcRockElevationThreshold = 0.6
  , pcRockHardnessThreshold = 0.6
  , pcRockHardnessSecondary = 0.45
  , pcSoilMoistureThreshold = 0.7
  , pcSoilSlopeThreshold = 0.4
  }

-- | Compute derived parameter layers from the terrain fields.
applyParameterLayersStage :: ParameterConfig -> PipelineStage
applyParameterLayersStage cfg = PipelineStage "applyParameterLayers" "applyParameterLayers" $ do
  logInfo "applyParameterLayers: deriving fields"
  seed <- asks peSeed
  modifyWorldP $ \world ->
    let config = twConfig world
        terrain' = IntMap.mapWithKey (deriveChunk config seed cfg) (twTerrain world)
    in world { twTerrain = terrain' }

deriveChunk :: WorldConfig -> Word64 -> ParameterConfig -> Int -> TerrainChunk -> TerrainChunk
deriveChunk config seed cfg key chunk =
  let origin = chunkOriginTile config (chunkCoordFromId (ChunkId key))
      size = wcChunkSize config
      elev = tcElevation chunk
      n = U.length elev
      slope = U.generate n (slopeAt size elev)
      curvature = U.generate n (curvatureAt size elev)
      hardness = U.generate n (hardnessAt config seed cfg origin elev)
      rockType = U.generate n (rockTypeAt cfg elev hardness)
      soilType = U.generate n (soilTypeAt cfg (tcMoisture chunk) slope)
      roughness = U.map (clamp01 . (* pcRoughnessScale cfg) . abs) curvature
      soilDepth = U.map (\h -> clamp01 (1 - h)) hardness
      fertility = U.zipWith (\m s -> clamp01 (m * pcFertilityMoistureWeight cfg + (1 - s) * pcFertilitySlopeWeight cfg)) (tcMoisture chunk) slope
  in chunk
      { tcSlope = slope
      , tcCurvature = curvature
      , tcHardness = hardness
      , tcRockType = rockType
      , tcSoilType = soilType
      , tcRoughness = roughness
      , tcSoilDepth = soilDepth
      , tcFertility = fertility
      }

slopeAt :: Int -> U.Vector Float -> Int -> Float
slopeAt size elev i =
  let x = i `mod` size
      y = i `div` size
      e0 = elev U.! i
      ex = if x + 1 < size then elev U.! (i + 1) else e0
      ey = if y + 1 < size then elev U.! (i + size) else e0
      dx = ex - e0
      dy = ey - e0
  in sqrt (dx * dx + dy * dy)

curvatureAt :: Int -> U.Vector Float -> Int -> Float
curvatureAt size elev i =
  let x = i `mod` size
      y = i `div` size
      e0 = elev U.! i
      ex = if x + 1 < size then elev U.! (i + 1) else e0
      ey = if y + 1 < size then elev U.! (i + size) else e0
  in (ex + ey - 2 * e0)

hardnessAt :: WorldConfig -> Word64 -> ParameterConfig -> TileCoord -> U.Vector Float -> Int -> Float
hardnessAt config seed cfg origin elev i =
  let TileCoord lx ly = tileCoordFromIndex config (TileIndex i)
      TileCoord ox oy = origin
      gx = ox + lx
      gy = oy + ly
      base = clamp01 ((elev U.! i + 1) / 2)
      scale = pcDetailScale cfg
      n0 = noise2D seed (floor (fromIntegral gx * scale)) (floor (fromIntegral gy * scale))
  in clamp01 (base * pcHardnessBaseWeight cfg + n0 * pcHardnessNoiseWeight cfg)

rockTypeAt :: ParameterConfig -> U.Vector Float -> U.Vector Float -> Int -> Word16
rockTypeAt cfg elev hardness i =
  let e0 = elev U.! i
      h0 = hardness U.! i
  in if e0 > pcRockElevationThreshold cfg && h0 > pcRockHardnessThreshold cfg
      then 2
      else if h0 > pcRockHardnessSecondary cfg
        then 1
        else 0

soilTypeAt :: ParameterConfig -> U.Vector Float -> U.Vector Float -> Int -> Word16
soilTypeAt cfg moisture slope i =
  let m0 = moisture U.! i
      s0 = slope U.! i
  in if m0 > pcSoilMoistureThreshold cfg
      then 2
      else if s0 > pcSoilSlopeThreshold cfg
        then 1
        else 0
