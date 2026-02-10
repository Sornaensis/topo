{-# LANGUAGE OverloadedStrings #-}

-- | Base terrain sampling and plate-driven terrain generation.
module Topo.Gen
  ( GenConfig(..)
  , defaultGenConfig
  , generateBaseHeightStage
  , generatePlateTerrainStage
  , sampleBaseHeightAt
  ) where

import Control.Monad.Reader (asks)
import Data.List (foldl')
import Data.Word (Word64)
import Topo.BaseHeight (GenConfig(..), defaultGenConfig, oceanEdgeBiasAt, sampleBaseHeightAt)
import Topo.Pipeline (PipelineStage(..))
import Topo.Plugin (logInfo, modifyWorldP, peSeed)
import Topo.Tectonics (TectonicsConfig, applyTectonicsChunk)
import Topo.Types
import Topo.World (TerrainWorld(..), emptyTerrainChunk, generateTerrainChunk, setTerrainChunk)
import qualified Data.Vector.Unboxed as U

-- | Generate a base-height world using procedural noise.
generateBaseHeightStage :: GenConfig -> PipelineStage
generateBaseHeightStage cfg = PipelineStage "generateBaseHeight" "generateBaseHeight" $ do
  logInfo "generateBaseHeight: generating base height"
  seed <- asks peSeed
  modifyWorldP $ \world ->
    let config = twConfig world
        chunkCoords = baseHeightChunkCoords (gcWorldExtent cfg)
        chunks =
          [ (chunkIdFromCoord coord, generateTerrainChunkAt config seed cfg coord)
          | coord <- chunkCoords
          ]
    in foldl' (\w (cid, chunk) -> setTerrainChunk cid chunk w) world chunks

-- | Generate plate-based terrain as the primary heightmap.
generatePlateTerrainStage :: GenConfig -> TectonicsConfig -> PipelineStage
generatePlateTerrainStage cfg tcfg = PipelineStage "generatePlateTerrain" "generatePlateTerrain" $ do
  logInfo "generatePlateTerrain: generating plate-based terrain"
  seed <- asks peSeed
  modifyWorldP $ \world ->
    let config = twConfig world
        chunkCoords = baseHeightChunkCoords (gcWorldExtent cfg)
        chunks =
          [ let cid@(ChunkId key) = chunkIdFromCoord coord
                baseChunk = emptyTerrainChunk config
                plateChunk = applyTectonicsChunk config seed cfg tcfg key baseChunk
            in (cid, plateChunk)
          | coord <- chunkCoords
          ]
    in foldl' (\w (cid, chunk) -> setTerrainChunk cid chunk w) world chunks

baseHeightChunkCoords :: WorldExtent -> [ChunkCoord]
baseHeightChunkCoords extent =
  let (rx, ry) = worldExtentRadii extent
      rx' = max 0 rx
      ry' = max 0 ry
  in [ ChunkCoord cx cy
     | cy <- [-ry'..ry']
     , cx <- [-rx'..rx']
     ]

generateTerrainChunkAt :: WorldConfig -> Word64 -> GenConfig -> ChunkCoord -> TerrainChunk
generateTerrainChunkAt config seed cfg coord =
  let TileCoord ox oy = chunkOriginTile config coord
      extent = gcWorldExtent cfg
      edgeCfg = gcOceanEdgeDepth cfg
  in generateTerrainChunk config (\(TileCoord x y) ->
      let gx = ox + x
          gy = oy + y
          base = sampleBaseHeightAt seed cfg (fromIntegral gx) (fromIntegral gy)
          bias = oceanEdgeBiasAt config extent edgeCfg (TileCoord gx gy)
      in base + bias)

