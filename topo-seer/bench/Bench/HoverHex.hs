{-# LANGUAGE OverloadedStrings #-}

-- | Benchmarks for hover-hex scanline geometry.
module Bench.HoverHex (benchmarks) where

import Test.Tasty.Bench

import Seer.Draw.Overlay (hexSpans)

benchmarks :: Benchmark
benchmarks = bgroup "HoverHex"
  [ bgroup "hexSpans"
    [ bench "radius 6"  $ nf hexSpans 6
    , bench "radius 12" $ nf hexSpans 12
    , bench "radius 24" $ nf hexSpans 24
    ]
  ]
