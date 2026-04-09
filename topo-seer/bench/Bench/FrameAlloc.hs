{-# LANGUAGE OverloadedStrings #-}

-- | Allocation-focused benchmarks for per-frame operations.
--
-- Run with @+RTS -T@ to see allocated bytes per iteration.  These
-- benchmarks exercise the allocation-intensive paths the render thread
-- hits each frame: geometry composition, viewport culling, cache
-- rebuild, and the combined frame cycle.
module Bench.FrameAlloc (benchmarks) where

import qualified Data.IntMap.Strict as IntMap
import Test.Tasty.Bench

import Actor.UI.State (ViewMode(..))
import Bench.AtlasCache ()  -- NFData AtlasTileGeometry, AtlasChunkGeometry
import Fixtures
import Seer.Render.Terrain (TerrainCache(..), buildTerrainCache)
import Seer.Render.Viewport (visibleChunkKeys)
import UI.TerrainAtlas (composeTilesFromGeometry)
import UI.TerrainRender (ChunkGeometry, buildChunkGeometry)

------------------------------------------------------------------------
-- Geometry map fixtures
------------------------------------------------------------------------

-- | 16-chunk geometry map (pre-built from terrain/climate/weather/veg).
geoMap16 :: IntMap.IntMap ChunkGeometry
geoMap16 = IntMap.mapWithKey (\k tc ->
  buildChunkGeometry 6 benchWorldConfig ViewElevation 0.3
    climateMap16 weatherMap16 vegMap16
    Nothing k tc) terrainMap16

-- | 64-chunk geometry map.
geoMap64 :: IntMap.IntMap ChunkGeometry
geoMap64 = IntMap.mapWithKey (\k tc ->
  buildChunkGeometry 6 benchWorldConfig ViewElevation 0.3
    climateMap64 weatherMap64 vegMap64
    Nothing k tc) terrainMap64

------------------------------------------------------------------------
-- Benchmarks
------------------------------------------------------------------------

benchmarks :: Benchmark
benchmarks = bgroup "FrameAlloc"
  [ bgroup "composeTilesFromGeometry"
    [ bench "16chunks/scale1" $ nf (composeTilesFromGeometry geoMap16 6) 1
    , bench "64chunks/scale1" $ nf (composeTilesFromGeometry geoMap64 6) 1
    , bench "64chunks/scale2" $ nf (composeTilesFromGeometry geoMap64 6) 2
    ]
  , bgroup "visibleChunkKeys"
    [ bench "256chunks/zoom1"  $ nf (cull largeChunkMap 1.0 (0, 0)) (800, 600)
    , bench "1024chunks/zoom1" $ nf (cull chunkMap1024 1.0 (0, 0)) (800, 600)
    , bench "4096chunks/zoom1" $ nf (cull chunkMap4096 1.0 (0, 0)) (800, 600)
    , bench "4096chunks/zoom0.5" $ nf (cull chunkMap4096 0.5 (0, 0)) (1920, 1080)
    ]
  , bgroup "buildTerrainCache"
    [ bench "16chunks" $ nf (tcGeometry . buildTerrainCache benchUiState) benchTerrainSnapshot
    ]
  , bgroup "frameCycle"
    [ bench "compose+cull/16chunks" $
        nf (\gm ->
          let tiles = composeTilesFromGeometry gm 6 1
              visible = visibleChunkKeys benchWorldConfig (0, 0) 1.0 (800, 600) benchChunkMap
          in (tiles, visible)) geoMap16
    , bench "compose+cull/64chunks" $
        nf (\gm ->
          let tiles = composeTilesFromGeometry gm 6 1
              visible = visibleChunkKeys benchWorldConfig (0, 0) 1.0 (800, 600) chunkMap1024
          in (tiles, visible)) geoMap64
    ]
  ]

cull :: IntMap.IntMap () -> Float -> (Float, Float) -> (Int, Int) -> [Int]
cull chunks zoom pan winSize =
  visibleChunkKeys benchWorldConfig pan zoom winSize chunks
