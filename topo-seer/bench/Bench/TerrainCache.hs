{-# LANGUAGE OverloadedStrings #-}

-- | Benchmarks for full terrain cache rebuild.
module Bench.TerrainCache (benchmarks) where

import Test.Tasty.Bench

import Fixtures
import Seer.Render.Terrain (TerrainCache(..), buildTerrainCache)

benchmarks :: Benchmark
benchmarks = bgroup "TerrainCache"
  [ bgroup "buildTerrainCache"
    -- Pass the snapshot as the nf argument so GHC cannot float the
    -- pure application to a CAF (the old whnfIO+let was optimised
    -- to a single evaluation across all iterations → 216 ps).
    [ bench "16chunks" $ nf (tcGeometry . buildTerrainCache benchUiState) benchTerrainSnapshot
    ]
  ]
