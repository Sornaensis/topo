{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Base terrain sampling and plate-driven terrain generation.
module Topo.Gen
  ( GenConfig(..)
  , defaultGenConfig
  , generateBaseHeightStage
  , generatePlateTerrainStage
  , plateTerrainWorkerCount
  , sampleBaseHeightAt
  ) where

import Control.Concurrent.Async (mapConcurrently_)
import Control.Concurrent.MVar (modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (evaluate)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.List (foldl')
import Data.Word (Word64)
import qualified Data.IntMap.Strict as IntMap
import GHC.Conc (getNumCapabilities)
import Topo.BaseHeight (GenConfig(..), defaultGenConfig, oceanEdgeBiasAt, sampleBaseHeightAt)
import Topo.Pipeline (PipelineStage(..))
import Topo.Pipeline.Stage (StageId(..))
import Topo.Planet (LatitudeMapping)
import Topo.Plugin (getWorldP, logInfo, modifyWorldP, peSeed, putWorldP)
import Topo.Tectonics (TectonicsConfig, applyTectonicsChunk)
import Topo.Types
import Topo.World (TerrainWorld(..), emptyTerrainChunk, generateTerrainChunk, setTerrainChunk)

-- | Generate a base-height world using procedural noise.
generateBaseHeightStage :: GenConfig -> PipelineStage
generateBaseHeightStage cfg = PipelineStage StageBaseHeight "generateBaseHeight" "generateBaseHeight" Nothing [] Nothing $ do
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
generatePlateTerrainStage cfg tcfg = PipelineStage StagePlateTerrain "generatePlateTerrain" "generatePlateTerrain" Nothing [] Nothing $ do
  logInfo "generatePlateTerrain: generating plate-based terrain"
  seed <- asks peSeed
  world <- getWorldP
  chunks <- liftIO (generatePlateTerrainChunks cfg tcfg seed world)
  putWorldP $! installTerrainChunks world chunks

-- | Bound plate chunk workers by both RTS capabilities and available work.
plateTerrainWorkerCount :: Int -> Int -> Int
plateTerrainWorkerCount capabilities chunkCount =
  max 1 (min (max 1 capabilities) (max 1 chunkCount))

generatePlateTerrainChunks :: GenConfig -> TectonicsConfig -> Word64 -> TerrainWorld -> IO [(ChunkId, TerrainChunk)]
generatePlateTerrainChunks cfg tcfg seed world = do
  capabilities <- getNumCapabilities
  let workerCount = plateTerrainWorkerCount capabilities (length chunkCoords)
      buildChunk = evaluate . generatePlateTerrainChunk config seed cfg lm tcfg
  boundedMapConcurrentlyN workerCount buildChunk chunkCoords
  where
    config = twConfig world
    lm = twLatMapping world
    chunkCoords = baseHeightChunkCoords (gcWorldExtent cfg)

generatePlateTerrainChunk :: WorldConfig -> Word64 -> GenConfig -> LatitudeMapping -> TectonicsConfig -> ChunkCoord -> (ChunkId, TerrainChunk)
generatePlateTerrainChunk config seed cfg lm tcfg coord =
  let !cid@(ChunkId key) = chunkIdFromCoord coord
      !baseChunk = emptyTerrainChunk config
      !plateChunk = applyTectonicsChunk config seed cfg lm tcfg key baseChunk
      !result = (cid, plateChunk)
  in result

installTerrainChunks :: TerrainWorld -> [(ChunkId, TerrainChunk)] -> TerrainWorld
installTerrainChunks = foldl' (\w (cid, chunk) -> setTerrainChunk cid chunk w)

boundedMapConcurrentlyN :: Int -> (a -> IO b) -> [a] -> IO [b]
boundedMapConcurrentlyN workerCount f xs
  | workerCount <= 1 = traverse f xs
  | null xs = pure []
  | otherwise = do
      workVar <- newMVar (zip [0..] xs)
      resultsVar <- newMVar IntMap.empty
      mapConcurrently_ (const (worker workVar resultsVar)) [1..workerCount]
      results <- readMVar resultsVar
      traverse (lookupResult results) [0 .. length xs - 1]
  where
    worker workVar resultsVar = do
      mJob <- modifyMVar workVar $ \jobs ->
        case jobs of
          [] -> pure ([], Nothing)
          job:rest -> pure (rest, Just job)
      case mJob of
        Nothing -> pure ()
        Just (idx, x) -> do
          !y <- f x
          modifyMVar_ resultsVar $ \results -> pure $! IntMap.insert idx y results
          worker workVar resultsVar

    lookupResult results idx =
      case IntMap.lookup idx results of
        Just y -> pure y
        Nothing -> fail ("boundedMapConcurrentlyN: missing result " <> show idx)

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

