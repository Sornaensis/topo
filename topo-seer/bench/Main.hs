module Main (main) where

import Test.Tasty.Bench

import qualified Bench.TerrainGeometry
import qualified Bench.RiverGeometry
import qualified Bench.Viewport
import qualified Bench.Pipeline
import qualified Bench.Simulation

main :: IO ()
main = defaultMain
  [ Bench.TerrainGeometry.benchmarks
  , Bench.RiverGeometry.benchmarks
  , Bench.Viewport.benchmarks
  , Bench.Pipeline.benchmarks
  , Bench.Simulation.benchmarks
  ]
