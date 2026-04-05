{-# LANGUAGE OverloadedStrings #-}

-- | Benchmarks for per-hex day/night brightness computation.
module Bench.DayNight (benchmarks) where

import Test.Tasty.Bench

import Fixtures
import UI.DayNight (mkDayNightFn)

benchmarks :: Benchmark
benchmarks = bgroup "DayNight"
  [ bgroup "mkDayNightFn"
    [ bench "build function" $ whnf (mkDayNightFn benchUiState) 8
    ]
  , bgroup "per-hex brightness"
    [ bench "single tile" $ whnf (applyDayNight 4 4) ()
    , bench "64 tiles (1 chunk)" $ nf applyChunk ()
    ]
  ]

-- | The day/night function for chunkSize=8.
dayNightFn :: Int -> Int -> Float
dayNightFn = case mkDayNightFn benchUiState 8 of
  Just f  -> f
  Nothing -> \_ _ -> 1.0

applyDayNight :: Int -> Int -> () -> Float
applyDayNight q r _ = dayNightFn q r

applyChunk :: () -> [Float]
applyChunk _ = [ dayNightFn q r | q <- [0..7], r <- [0..7] ]
