{-# LANGUAGE OverloadedStrings #-}

-- | Benchmarks for river geometry building.
module Bench.RiverGeometry (benchmarks) where

import Control.DeepSeq (NFData(..), rwhnf)
import qualified Data.IntMap.Strict as IntMap
import Foreign.C.Types (CInt)
import SDL.Raw.Types as Raw
import Test.Tasty.Bench

import Fixtures
import Topo (RiverChunk)
import UI.RiverRender
  ( RiverGeometry(..)
  , buildChunkRiverGeometry
  , buildDeltaFan
  , defaultRiverRenderConfig
  )

instance NFData RiverGeometry where
  rnf (RiverGeometry b v i) = rwhnf b `seq` rnf v `seq` rnf i

-- SDL raw types are strict C structs — rwhnf is sufficient.
instance NFData Raw.Vertex where rnf = rwhnf
instance NFData Raw.FPoint where rnf = rwhnf
instance NFData Raw.Color  where rnf = rwhnf

benchmarks :: Benchmark
benchmarks = bgroup "RiverGeometry"
  [ bgroup "buildChunkRiverGeometry"
    -- Each benchmark passes the river map as the nf argument so GHC
    -- cannot float the pure application to a CAF.
    [ bench "no rivers"       $ nf (buildGeo 0) IntMap.empty
    , bench "empty segments"  $ nf (buildGeo 0) (IntMap.singleton 0 benchRiverChunk)
    , bench "populated chunk" $ nf (buildGeo 0) (IntMap.singleton 0 populatedRiverChunk)
    ]
  , bgroup "buildDeltaFan"
    -- Pass triCount as the nf argument to prevent CAF sharing.
    [ bench "small fan (4 tris)"  $ nf fanBench 4
    , bench "medium fan (8 tris)" $ nf fanBench 8
    , bench "large fan (16 tris)" $ nf fanBench 16
    ]
  ]

-- | Build river geometry for a single chunk.  Passes 'IntMap.empty'
-- for the terrain map so 'isTileWater' is always False and the
-- benchmark measures pure geometry building without biome filtering.
buildGeo :: Int -> IntMap.IntMap RiverChunk -> Maybe RiverGeometry
buildGeo key rm = buildChunkRiverGeometry
  defaultRiverRenderConfig
  benchWorldConfig
  6     -- hex radius
  key
  rm
  IntMap.empty

fanBench :: Int -> ([Raw.Vertex], [CInt], CInt)
fanBench triCount = buildDeltaFan
  (Raw.Color 60 120 200 255)  -- color
  3.0                          -- fan radius (pixels)
  1.5                          -- spread angle (radians)
  triCount                     -- triangle count
  100.0                        -- center X
  100.0                        -- center Y
  0.0                          -- direction angle
  0                            -- base index
