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
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (evaluate)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.List (foldl')
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import qualified Data.IntMap.Strict as IntMap
import GHC.Conc (getNumCapabilities)
import Topo.BaseHeight (GenConfig(..), defaultGenConfig, oceanEdgeBiasAt, sampleBaseHeightAt)
import Topo.Pipeline (PipelineStage(..))
import Topo.Pipeline.Stage (StageId(..))
import Topo.Planet (LatitudeMapping)
import Topo.Plugin (getWorldP, logInfo, modifyWorldP, peProgress, peSeed, putWorldP)
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
  progress <- asks peProgress
  world <- getWorldP
  chunks <- liftIO (generatePlateTerrainChunks cfg tcfg seed world progress)
  putWorldP $! installTerrainChunks world chunks

-- | Bound plate chunk workers by both RTS capabilities and available work.
plateTerrainWorkerCount :: Int -> Int -> Int
plateTerrainWorkerCount capabilities chunkCount =
  max 1 (min (max 1 capabilities) (max 1 chunkCount))

generatePlateTerrainChunks :: GenConfig -> TectonicsConfig -> Word64 -> TerrainWorld -> (Text -> IO ()) -> IO [(ChunkId, TerrainChunk)]
generatePlateTerrainChunks cfg tcfg seed world report = do
  capabilities <- getNumCapabilities
  completedRef <- newIORef 0
  lastReportedVar <- newMVar 0
  let chunkCount = length chunkCoords
      workerCount = plateTerrainWorkerCount capabilities chunkCount
      stride = plateProgressStride chunkCount
      diagnostics = plateTerrainDiagnostics config cfg chunkCount workerCount
      buildChunk coord = do
        !chunk <- evaluate (generatePlateTerrainChunk config seed cfg lm tcfg coord)
        completed <- atomicModifyIORef' completedRef $ \n ->
          let !n' = n + 1
          in (n', n')
        when (shouldReportPlateProgress completed chunkCount stride) $
          emitPlateProgress lastReportedVar report diagnostics completed chunkCount
        pure chunk
  report ("generatePlateTerrain: starting " <> diagnostics)
  boundedMapConcurrentlyN workerCount buildChunk chunkCoords
  where
    config = twConfig world
    lm = twLatMapping world
    chunkCoords = baseHeightChunkCoords (gcWorldExtent cfg)

plateTerrainDiagnostics :: WorldConfig -> GenConfig -> Int -> Int -> Text
plateTerrainDiagnostics config cfg chunkCount workerCount =
  let chunkSize = wcChunkSize config
      (rx0, ry0) = worldExtentRadii (gcWorldExtent cfg)
      rx = max 0 rx0
      ry = max 0 ry0
      gridW = rx * 2 + 1
      gridH = ry * 2 + 1
      tileCount = chunkCount * chunkSize * chunkSize
  in "chunkSize=" <> showText chunkSize
     <> " extent=" <> showText rx <> "x" <> showText ry
     <> " grid=" <> showText gridW <> "x" <> showText gridH
     <> " chunks=" <> showText chunkCount
     <> " tiles=" <> showText tileCount
     <> " workers=" <> showText workerCount

plateProgressStride :: Int -> Int
plateProgressStride chunkCount = max 1 ((chunkCount + 19) `div` 20)

shouldReportPlateProgress :: Int -> Int -> Int -> Bool
shouldReportPlateProgress completed total stride =
  completed == 1 || completed == total || completed `mod` stride == 0

emitPlateProgress :: MVar Int -> (Text -> IO ()) -> Text -> Int -> Int -> IO ()
emitPlateProgress lastReportedVar report diagnostics completed total =
  modifyMVar_ lastReportedVar $ \lastReported ->
    if completed <= lastReported
      then pure lastReported
      else do
        report ("generatePlateTerrain: " <> plateProgressText completed total <> " " <> diagnostics)
        pure completed

plateProgressText :: Int -> Int -> Text
plateProgressText completed total =
  "chunks=" <> showText completed <> "/" <> showText total
  <> " (" <> percentText completed total <> ")"

percentText :: Int -> Int -> Text
percentText _ total | total <= 0 = "100.0%"
percentText completed total =
  let tenths = completed * 1000 `div` total
  in showText (tenths `div` 10) <> "." <> showText (tenths `mod` 10) <> "%"

showText :: Show a => a -> Text
showText = Text.pack . show

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

