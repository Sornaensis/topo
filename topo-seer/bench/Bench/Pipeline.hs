{-# LANGUAGE OverloadedStrings #-}

-- | Benchmarks for world generation pipeline execution.
module Bench.Pipeline (benchmarks) where

import Data.Word (Word64)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as U
import Test.Tasty.Bench

import Fixtures
import Topo

data PlateTerrainBenchCase = PlateTerrainBenchCase
  { ptbcLabel :: !String
  , ptbcChunkSize :: !Int
  , ptbcExtentX :: !Int
  , ptbcExtentY :: !Int
  , ptbcSeed :: !Word64
  }

benchmarks :: Benchmark
benchmarks = bgroup "Pipeline"
  [ bench "full pipeline (chunkSize=4)" $ whnfIO (runFull 4)
  , bench "full pipeline (chunkSize=8)" $ whnfIO (runFull 8)
  , bench "terrain-only (chunkSize=8)"  $ whnfIO (runTerrainOnly 8)
  , bench "base height only (chunkSize=8)" $ whnfIO (runBaseHeightOnly 8)
  , plateTerrainBenchmarks
  ]

plateTerrainBenchmarks :: Benchmark
plateTerrainBenchmarks = bgroup "PlateTerrain"
  [ bgroup "applyTectonicsChunk single-chunk checksum"
      [ bench (ptbcLabel c) $ nf applyTectonicsChunkChecksum c
      | c <- plateTerrainBenchCases
      ]
  , bgroup "generatePlateTerrainStage wall-clock (plate only, no SDL)"
      [ bench (ptbcLabel c) $ whnfIO (runPlateTerrainOnly c)
      | c <- plateTerrainBenchCases
      ]
  , bgroup "downstream comparison wall-clock (no SDL)"
      [ bench (ptbcLabel defaultPlateTerrainCase) $
          whnfIO (runPlateTerrainOnly defaultPlateTerrainCase)
      , bench "terrain downstream after StagePlateTerrain chunkSize=64 extent=2x2 seed=42" $
          whnfIO (runTerrainDownstreamComparison defaultPlateTerrainCase)
      ]
  ]

-- Extents are chunk radii: extent=2x2 is the default UI/generated-world
-- coverage (5x5 chunks), while the larger cases capture reported slowness as
-- users increase the terrain extent slider.
plateTerrainBenchCases :: [PlateTerrainBenchCase]
plateTerrainBenchCases =
  [ PlateTerrainBenchCase
      "StagePlateTerrain/generatePlateTerrain chunkSize=64 extent=2x2 seed=42 default-ui"
      64 2 2 42
  , PlateTerrainBenchCase
      "StagePlateTerrain/generatePlateTerrain chunkSize=64 extent=4x4 seed=42 larger-extent"
      64 4 4 42
  , PlateTerrainBenchCase
      "StagePlateTerrain/generatePlateTerrain chunkSize=64 extent=8x8 seed=42 reported-slow"
      64 8 8 42
  ]

defaultPlateTerrainCase :: PlateTerrainBenchCase
defaultPlateTerrainCase = case plateTerrainBenchCases of
  c:_ -> c
  [] -> PlateTerrainBenchCase
          "StagePlateTerrain/generatePlateTerrain chunkSize=64 extent=2x2 seed=42 default-ui"
          64 2 2 42

runFull :: Int -> IO (Either PipelineError (TerrainWorld, [PipelineSnapshot]))
runFull cs =
  let cfg   = WorldConfig { wcChunkSize = cs }
      world = emptyWorld cfg defaultHexGridMeta
      -- Use extent 0 for a single chunk to keep benchmarks fast
      gen   = defaultWorldGenConfig
                { worldTerrain = (worldTerrain defaultWorldGenConfig)
                    { terrainGen = (terrainGen (worldTerrain defaultWorldGenConfig))
                        { gcWorldExtent = worldExtentSquareOrDefault 0 }
                    }
                }
      pipe  = buildFullPipelineConfig gen cfg 42
  in runPipeline pipe benchTopoEnv world

runTerrainOnly :: Int -> IO (Either PipelineError (TerrainWorld, [PipelineSnapshot]))
runTerrainOnly cs =
  let cfg   = WorldConfig { wcChunkSize = cs }
      world = emptyWorld cfg defaultHexGridMeta
      gen   = defaultWorldGenConfig
                { worldTerrain = (worldTerrain defaultWorldGenConfig)
                    { terrainGen = (terrainGen (worldTerrain defaultWorldGenConfig))
                        { gcWorldExtent = worldExtentSquareOrDefault 0 }
                    }
                }
      pipe  = (buildFullPipelineConfig gen cfg 42)
                { pipelineDisabled = terrainOnlyDisabledStages }
  in runPipeline pipe benchTopoEnv world

runBaseHeightOnly :: Int -> IO (Either PipelineError (TerrainWorld, [PipelineSnapshot]))
runBaseHeightOnly cs =
  let cfg   = WorldConfig { wcChunkSize = cs }
      world = emptyWorld cfg defaultHexGridMeta
      gen   = defaultGenConfig { gcWorldExtent = worldExtentSquareOrDefault 0 }
      pipe  = buildBaseHeightPipelineConfig gen 42
  in runPipeline pipe benchTopoEnv world

runPlateTerrainOnly :: PlateTerrainBenchCase -> IO (Either PipelineError Float)
runPlateTerrainOnly c = do
  result <- runPipeline (buildPipelineConfig (caseWorldGenConfig c) (ptbcSeed c))
                        benchTopoEnv
                        (caseWorld c)
  pure (forcePipelineSummary (summarizePipelineResult result))

runTerrainDownstreamComparison :: PlateTerrainBenchCase -> IO (Either PipelineError Float)
runTerrainDownstreamComparison c = do
  let pipe = (buildFullPipelineConfig (caseWorldGenConfig c) (caseWorldConfig c) (ptbcSeed c))
        { pipelineDisabled = terrainOnlyDisabledStages }
  result <- runPipeline pipe benchTopoEnv (caseWorld c)
  pure (forcePipelineSummary (summarizePipelineResult result))

applyTectonicsChunkChecksum :: PlateTerrainBenchCase -> Float
applyTectonicsChunkChecksum c =
  let cfg = caseWorldConfig c
      world = caseWorld c
      gen = caseGenConfig c
      tcfg = caseTectonicsConfig c
      ChunkId key = chunkIdFromCoord (ChunkCoord 0 0)
      chunk = applyTectonicsChunk cfg (ptbcSeed c) gen (twLatMapping world) tcfg key (emptyTerrainChunk cfg)
  in terrainMapChecksum (IntMap.singleton key chunk)

caseWorldConfig :: PlateTerrainBenchCase -> WorldConfig
caseWorldConfig c = WorldConfig { wcChunkSize = ptbcChunkSize c }

caseWorld :: PlateTerrainBenchCase -> TerrainWorld
caseWorld c = emptyWorld (caseWorldConfig c) defaultHexGridMeta

caseWorldGenConfig :: PlateTerrainBenchCase -> WorldGenConfig
caseWorldGenConfig c =
  let terrain = worldTerrain defaultWorldGenConfig
  in defaultWorldGenConfig { worldTerrain = terrain { terrainGen = caseGenConfig c } }

caseGenConfig :: PlateTerrainBenchCase -> GenConfig
caseGenConfig c =
  (terrainGen (worldTerrain defaultWorldGenConfig))
    { gcWorldExtent = worldExtentOrDefault (ptbcExtentX c) (ptbcExtentY c) }

caseTectonicsConfig :: PlateTerrainBenchCase -> TectonicsConfig
caseTectonicsConfig = const (terrainTectonics (worldTerrain defaultWorldGenConfig))

terrainOnlyDisabledStages :: Set.Set StageId
terrainOnlyDisabledStages = Set.fromList
  [ StageClimate, StageOceanCurrents, StageGlacier
  , StageBiomes, StageVegetationFeedback, StageConvergence
  , StageWeather
  ]

summarizePipelineResult :: Either PipelineError (TerrainWorld, [PipelineSnapshot]) -> Either PipelineError Float
summarizePipelineResult = fmap (terrainWorldChecksum . fst)

forcePipelineSummary :: Either PipelineError Float -> Either PipelineError Float
forcePipelineSummary (Right checksum) = checksum `seq` Right checksum
forcePipelineSummary (Left err) = Left err

terrainWorldChecksum :: TerrainWorld -> Float
terrainWorldChecksum = terrainMapChecksum . twTerrain

terrainMapChecksum :: IntMap.IntMap TerrainChunk -> Float
terrainMapChecksum chunks =
  IntMap.foldl' (\acc chunk -> acc + terrainChunkChecksum chunk) 0 chunks

terrainChunkChecksum :: TerrainChunk -> Float
terrainChunkChecksum chunk =
  U.sum (tcElevation chunk)
  + U.sum (tcHardness chunk)
  + U.sum (tcPlateHeight chunk)
  + U.sum (tcPlateHardness chunk)
  + U.sum (tcPlateAge chunk)
  + U.sum (tcPlateVelX chunk)
  + U.sum (tcPlateVelY chunk)
  + fromIntegral (U.length (tcPlateId chunk))
