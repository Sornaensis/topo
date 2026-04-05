{-# LANGUAGE OverloadedStrings #-}

-- | Benchmarks for full terrain cache rebuild.
module Bench.TerrainCache (benchmarks) where

import Control.DeepSeq (rnf)
import qualified Data.IntMap.Strict as IntMap
import Test.Tasty.Bench

import Fixtures
import Seer.Render.Terrain (TerrainCache(..), buildTerrainCache)

benchmarks :: Benchmark
benchmarks = bgroup "TerrainCache"
  [ bgroup "buildTerrainCache"
    [ bench "16chunks" $ whnfIO $ do
        let cache = buildTerrainCache benchUiState benchTerrainSnapshot
        -- Force all chunk geometry (the expensive work)
        _ <- pure $! IntMap.foldl' (\acc cg -> rnf cg `seq` acc) () (tcGeometry cache)
        pure ()
    ]
  ]
