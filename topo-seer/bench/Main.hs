module Main (main) where

import Test.Tasty.Bench

import qualified Bench.AtlasCache
import qualified Bench.DayNight
import qualified Bench.HoverHex
import qualified Bench.OverlayExtract
import qualified Bench.Pipeline
import qualified Bench.RiverGeometry
import qualified Bench.Simulation
import qualified Bench.TerrainCache
import qualified Bench.TerrainGeometry
import qualified Bench.Viewport
import qualified Bench.Widgets

main :: IO ()
main = defaultMain
  [ Bench.TerrainGeometry.benchmarks
  , Bench.RiverGeometry.benchmarks
  , Bench.Viewport.benchmarks
  , Bench.Pipeline.benchmarks
  , Bench.Simulation.benchmarks
  , Bench.DayNight.benchmarks
  , Bench.OverlayExtract.benchmarks
  , Bench.HoverHex.benchmarks
  , Bench.TerrainCache.benchmarks
  , Bench.Widgets.benchmarks
  , Bench.AtlasCache.benchmarks
  ]
