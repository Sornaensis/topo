{-# LANGUAGE OverloadedStrings #-}

-- | Benchmarks for world generation pipeline execution.
module Bench.Pipeline (benchmarks) where

import qualified Data.Set as Set
import Test.Tasty.Bench

import Fixtures
import Topo

benchmarks :: Benchmark
benchmarks = bgroup "Pipeline"
  [ bench "full pipeline (chunkSize=4)" $ whnfIO (runFull 4)
  , bench "full pipeline (chunkSize=8)" $ whnfIO (runFull 8)
  , bench "terrain-only (chunkSize=8)"  $ whnfIO (runTerrainOnly 8)
  , bench "base height only (chunkSize=8)" $ whnfIO (runBaseHeightOnly 8)
  ]

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
                { pipelineDisabled = Set.fromList
                    [ StageClimate, StageOceanCurrents, StageGlacier
                    , StageBiomes, StageVegetationFeedback, StageConvergence
                    , StageWeather
                    ]
                }
  in runPipeline pipe benchTopoEnv world

runBaseHeightOnly :: Int -> IO (Either PipelineError (TerrainWorld, [PipelineSnapshot]))
runBaseHeightOnly cs =
  let cfg   = WorldConfig { wcChunkSize = cs }
      world = emptyWorld cfg defaultHexGridMeta
      gen   = defaultGenConfig { gcWorldExtent = worldExtentSquareOrDefault 0 }
      pipe  = buildBaseHeightPipelineConfig gen 42
  in runPipeline pipe benchTopoEnv world
