{-# LANGUAGE OverloadedStrings #-}

-- | Benchmarks for river geometry building.
module Bench.RiverGeometry (benchmarks) where

import Control.DeepSeq (NFData(..), rwhnf)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8, Word16, Word32)
import Foreign.C.Types (CInt)
import SDL.Raw.Types as Raw
import Test.Tasty.Bench

import Fixtures
import Topo (RiverChunk(..))
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
    [ bench "empty rivers"    $ nf buildEmpty ()
    , bench "single chunk"    $ nf buildSingle ()
    , bench "populated chunk" $ nf buildPopulated ()
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

buildPopulated :: () -> Maybe RiverGeometry
buildPopulated _ = buildChunkRiverGeometry
  defaultRiverRenderConfig
  benchWorldConfig
  6    -- hex radius
  0    -- chunk id
  (IntMap.singleton 0 populatedRiverChunk)
  (IntMap.singleton 0 benchTerrainChunk)

-- | A river chunk with ~22 river tiles out of 64.
populatedRiverChunk :: RiverChunk
populatedRiverChunk =
  let n = 64
      nSegs = 22
  in RiverChunk
    { rcFlowAccum        = U.generate n (\i -> fromIntegral i * 10)
    , rcDischarge         = U.generate n (\i -> fromIntegral i * 5)
    , rcChannelDepth      = U.replicate n 0.5
    , rcRiverOrder        = U.replicate n (2 :: Word16)
    , rcBasinId           = U.replicate n (1 :: Word32)
    , rcBaseflow          = U.replicate n 0
    , rcErosionPotential  = U.replicate n 0
    , rcDepositPotential  = U.replicate n 0
    , rcFlowDir           = U.generate n (\i -> if i < n-1 then i+1 else -1)
    , rcSegOffsets        = U.generate (n+1) (\i -> min nSegs (i `div` 3))
    , rcSegEntryEdge      = U.replicate nSegs (2 :: Word8)
    , rcSegExitEdge       = U.replicate nSegs (5 :: Word8)
    , rcSegDischarge      = U.generate nSegs (\i -> fromIntegral i * 5)
    , rcSegOrder          = U.replicate nSegs (2 :: Word16)
    }

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
