{-# LANGUAGE OverloadedStrings #-}

-- | Benchmarks for river geometry building.
module Bench.RiverGeometry (benchmarks) where

import Control.DeepSeq (NFData(..), rwhnf)
import qualified Data.IntMap.Strict as IntMap
import Foreign.C.Types (CInt)
import SDL.Raw.Types as Raw
import Test.Tasty.Bench

import Fixtures
import UI.RiverRender
  ( RiverGeometry(..)
  , buildChunkRiverGeometry
  , buildDeltaFan
  , defaultRiverRenderConfig
  )

instance NFData RiverGeometry where
  rnf (RiverGeometry b v i) = rwhnf b `seq` rnf v `seq` rnf i

benchmarks :: Benchmark
benchmarks = bgroup "RiverGeometry"
  [ bgroup "buildChunkRiverGeometry"
    [ bench "empty rivers" $ nf buildEmpty ()
    , bench "single chunk" $ nf buildSingle ()
    ]
  , bgroup "buildDeltaFan"
    [ bench "small fan (4 tris)"  $ whnf (fanBench 4)  ()
    , bench "medium fan (8 tris)" $ whnf (fanBench 8)  ()
    , bench "large fan (16 tris)" $ whnf (fanBench 16) ()
    ]
  ]

buildEmpty :: () -> Maybe RiverGeometry
buildEmpty _ = buildChunkRiverGeometry
  defaultRiverRenderConfig
  benchWorldConfig
  6    -- hex radius
  0    -- chunk id
  IntMap.empty
  (IntMap.singleton 0 benchTerrainChunk)

buildSingle :: () -> Maybe RiverGeometry
buildSingle _ = buildChunkRiverGeometry
  defaultRiverRenderConfig
  benchWorldConfig
  6    -- hex radius
  0    -- chunk id
  (IntMap.singleton 0 benchRiverChunk)
  (IntMap.singleton 0 benchTerrainChunk)

fanBench :: Int -> () -> ([Raw.Vertex], [CInt], CInt)
fanBench triCount _ = buildDeltaFan
  (Raw.Color 60 120 200 255)  -- color
  3.0                          -- fan radius (pixels)
  1.5                          -- spread angle (radians)
  triCount                     -- triangle count
  100.0                        -- center X
  100.0                        -- center Y
  0.0                          -- direction angle
  0                            -- base index
