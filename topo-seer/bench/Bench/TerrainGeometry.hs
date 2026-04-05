{-# LANGUAGE OverloadedStrings #-}

-- | Benchmarks for terrain chunk geometry building.
module Bench.TerrainGeometry (benchmarks) where

import Control.DeepSeq (NFData(..), rwhnf)
import qualified Data.IntMap.Strict as IntMap
import Test.Tasty.Bench

import Actor.UI.State (ViewMode(..))
import Fixtures
import Topo (TerrainChunk)
import UI.TerrainRender (ChunkGeometry(..), buildChunkGeometry)

instance NFData ChunkGeometry where
  rnf (ChunkGeometry b v i) = rwhnf b `seq` rnf v `seq` rnf i

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
  Nothing                        -- no overlay lookup
  0                              -- chunk id
