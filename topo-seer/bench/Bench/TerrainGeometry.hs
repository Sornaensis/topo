{-# LANGUAGE OverloadedStrings #-}

-- | Benchmarks for terrain chunk geometry building.
module Bench.TerrainGeometry (benchmarks) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector.Unboxed as U
import Test.Tasty.Bench

import Actor.UI.State
  ( BaseViewMode(..)
  , LayeredViewState(..)
  , SkyOverlayMode(..)
  , WeatherBasis(..)
  , defaultLayeredViewState
  )
import Fixtures
import Topo (TerrainChunk)
import UI.OverlayExtract (extractOverlayField)
import UI.TerrainRender (ChunkGeometry, buildChunkGeometryForSelection)

benchmarks :: Benchmark
benchmarks = bgroup "TerrainGeometry"
  [ bgroup "buildChunkGeometry"
    [ bench "BaseElevation"  $ nf (buildGeo defaultLayeredViewState) benchTerrainChunk
    , bench "BaseBiome"      $ nf (buildGeo (baseSelection BaseViewBiome)) benchTerrainChunk
    , bench "BaseMoisture"   $ nf (buildGeo (baseSelection BaseViewMoisture)) benchTerrainChunk
    , bench "AverageTemperature" $ nf (buildGeo (weatherSelection WeatherBasisAverage)) benchTerrainChunk
    , bench "CurrentTemperature" $ nf (buildGeo (weatherSelection WeatherBasisCurrent)) benchTerrainChunk
    , bench "BaseTerrainForm" $ nf (buildGeo (baseSelection BaseViewTerrainForm)) benchTerrainChunk
    , bench "BasePlateId"    $ nf (buildGeo (baseSelection BaseViewPlateId)) benchTerrainChunk
    ]
  , bgroup "buildChunkGeometry/dayNight"
    [ bench "BaseElevation" $ nf (buildGeoDayNight defaultLayeredViewState) benchTerrainChunk
    , bench "BaseBiome"     $ nf (buildGeoDayNight (baseSelection BaseViewBiome)) benchTerrainChunk
    ]
  , bgroup "buildChunkGeometry/overlay"
    [ bench "ViewOverlay/dense"  $ nf (buildGeoOverlay denseOverlayVec)  benchTerrainChunk
    , bench "ViewOverlay/sparse" $ nf (buildGeoOverlay sparseOverlayVec) benchTerrainChunk
    ]
  ]

buildGeo :: LayeredViewState -> TerrainChunk -> ChunkGeometry
buildGeo selection = buildChunkGeometryForSelection
  6                              -- hex radius
  benchWorldConfig
  selection
  0.3                            -- water level
  (IntMap.singleton 0 benchClimateChunk)
  (IntMap.singleton 0 benchWeatherChunk)
  IntMap.empty                   -- no weather normals data
  (IntMap.singleton 0 benchVegetationChunk)
  Nothing                        -- no overlay data
  0                              -- chunk id

buildGeoDayNight :: LayeredViewState -> TerrainChunk -> ChunkGeometry
buildGeoDayNight selection = buildChunkGeometryForSelection
  6
  benchWorldConfig
  selection
  0.3
  (IntMap.singleton 0 benchClimateChunk)
  (IntMap.singleton 0 benchWeatherChunk)
  IntMap.empty
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
buildGeoOverlay mOvl = buildChunkGeometryForSelection
  6
  benchWorldConfig
  pluginSelection
  0.3
  (IntMap.singleton 0 benchClimateChunk)
  (IntMap.singleton 0 benchWeatherChunk)
  IntMap.empty
  (IntMap.singleton 0 benchVegetationChunk)
  mOvl
  0

baseSelection :: BaseViewMode -> LayeredViewState
baseSelection base = defaultLayeredViewState { lvsBaseView = base }

weatherSelection :: WeatherBasis -> LayeredViewState
weatherSelection basis = defaultLayeredViewState
  { lvsSkyOverlay = Just SkyOverlayWeatherTemperature
  , lvsWeatherBasis = basis
  }

pluginSelection :: LayeredViewState
pluginSelection = defaultLayeredViewState
  { lvsSkyOverlay = Just (SkyOverlayPlugin "bench_overlay" 0) }
