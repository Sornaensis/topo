{-# LANGUAGE OverloadedStrings #-}

-- | Benchmarks for terrain chunk geometry building.
module Bench.TerrainGeometry (benchmarks) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector.Unboxed as U
import Test.Tasty.Bench

import Actor.UI.State (ViewMode(..))
import Fixtures
import Topo (TerrainChunk)
import UI.OverlayExtract (extractOverlayField)
import UI.TerrainRender (ChunkGeometry, buildChunkGeometry)

benchmarks :: Benchmark
benchmarks = bgroup "TerrainGeometry"
  [ bgroup "buildChunkGeometry"
    [ bench "ViewElevation"  $ nf (buildGeo ViewElevation)  benchTerrainChunk
    , bench "ViewBiome"      $ nf (buildGeo ViewBiome)      benchTerrainChunk
    , bench "ViewMoisture"   $ nf (buildGeo ViewMoisture)   benchTerrainChunk
    , bench "ViewClimate"    $ nf (buildGeo ViewClimate)    benchTerrainChunk
    , bench "ViewWeather"    $ nf (buildGeo ViewWeather)    benchTerrainChunk
    , bench "ViewTerrainForm"$ nf (buildGeo ViewTerrainForm) benchTerrainChunk
    , bench "ViewPlateId"    $ nf (buildGeo ViewPlateId)    benchTerrainChunk
    ]
  , bgroup "buildChunkGeometry/dayNight"
    [ bench "ViewElevation" $ nf (buildGeoDayNight ViewElevation) benchTerrainChunk
    , bench "ViewBiome"     $ nf (buildGeoDayNight ViewBiome)     benchTerrainChunk
    ]
  , bgroup "buildChunkGeometry/overlay"
    [ bench "ViewOverlay/dense"  $ nf (buildGeoOverlay denseOverlayVec)  benchTerrainChunk
    , bench "ViewOverlay/sparse" $ nf (buildGeoOverlay sparseOverlayVec) benchTerrainChunk
    ]
  ]

buildGeo :: ViewMode -> TerrainChunk -> ChunkGeometry
buildGeo vm = buildChunkGeometry
  6                              -- hex radius
  benchWorldConfig
  vm
  0.3                            -- water level
  (IntMap.singleton 0 benchClimateChunk)
  (IntMap.singleton 0 benchWeatherChunk)
  (IntMap.singleton 0 benchVegetationChunk)
  Nothing                        -- no overlay data
  0                              -- chunk id

buildGeoDayNight :: ViewMode -> TerrainChunk -> ChunkGeometry
buildGeoDayNight vm = buildChunkGeometry
  6
  benchWorldConfig
  vm
  0.3
  (IntMap.singleton 0 benchClimateChunk)
  (IntMap.singleton 0 benchWeatherChunk)
  (IntMap.singleton 0 benchVegetationChunk)
  Nothing
  0

-- | Pre-extracted overlay vectors for ViewOverlay benchmark.
denseOverlayVec :: Maybe (U.Vector Float)
denseOverlayVec =
  case extractOverlayField "bench_overlay" 0 64 benchOverlayStoreDense of
    Just m  -> IntMap.lookup 0 m
    Nothing -> Nothing

sparseOverlayVec :: Maybe (U.Vector Float)
sparseOverlayVec =
  case extractOverlayField "bench_overlay" 0 64 benchOverlayStoreSparse of
    Just m  -> IntMap.lookup 0 m
    Nothing -> Nothing

buildGeoOverlay :: Maybe (U.Vector Float) -> TerrainChunk -> ChunkGeometry
buildGeoOverlay mOvl = buildChunkGeometry
  6
  benchWorldConfig
  (ViewOverlay "bench_overlay" 0)
  0.3
  (IntMap.singleton 0 benchClimateChunk)
  (IntMap.singleton 0 benchWeatherChunk)
  (IntMap.singleton 0 benchVegetationChunk)
  mOvl
  0
