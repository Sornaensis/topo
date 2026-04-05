-- | Benchmarks for viewport chunk culling.
module Bench.Viewport (benchmarks) where

import qualified Data.IntMap.Strict as IntMap
import Test.Tasty.Bench

import Fixtures
import Seer.Render.Viewport (visibleChunkKeys)

benchmarks :: Benchmark
benchmarks = bgroup "Viewport"
  [ bgroup "visibleChunkKeys"
    [ bgroup "16 chunks"
      [ bench "zoom 1.0 center" $ nf (cull benchChunkMap 1.0 (0, 0)) (800, 600)
      , bench "zoom 4.0 center" $ nf (cull benchChunkMap 4.0 (0, 0)) (800, 600)
      , bench "zoom 1.0 offset" $ nf (cull benchChunkMap 1.0 (200, 200)) (800, 600)
      ]
    , bgroup "256 chunks"
      [ bench "zoom 1.0 center" $ nf (cull largeChunkMap 1.0 (0, 0)) (800, 600)
      , bench "zoom 4.0 center" $ nf (cull largeChunkMap 4.0 (0, 0)) (800, 600)
      , bench "zoom 1.0 offset" $ nf (cull largeChunkMap 1.0 (200, 200)) (800, 600)
      , bench "zoom 8.0 center" $ nf (cull largeChunkMap 8.0 (0, 0)) (1920, 1080)
      ]
    ]
  ]

cull :: IntMap.IntMap () -> Float -> (Float, Float) -> (Int, Int) -> [Int]
cull chunks zoom pan winSize =
  visibleChunkKeys benchWorldConfig pan zoom winSize chunks
