module Main (main) where

import Test.Tasty.Bench

import qualified Bench.AtlasCache
import qualified Bench.AtlasOps
import qualified Bench.FrameAlloc
import qualified Bench.Pipeline
import qualified Bench.QueueDedup
import qualified Bench.RiverGeometry
import qualified Bench.TerrainCache
import qualified Bench.TerrainGeometry
import qualified Bench.Viewport

main :: IO ()
main = defaultMain
  [ Bench.TerrainGeometry.benchmarks
  , Bench.RiverGeometry.benchmarks
  , Bench.Viewport.benchmarks
  , Bench.Pipeline.benchmarks
  , Bench.TerrainCache.benchmarks
  , Bench.AtlasCache.benchmarks
  , Bench.AtlasOps.benchmarks
  , Bench.QueueDedup.benchmarks
  , Bench.FrameAlloc.benchmarks
  ]
