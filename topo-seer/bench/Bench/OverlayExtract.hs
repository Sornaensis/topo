{-# LANGUAGE OverloadedStrings #-}

-- | Benchmarks for overlay field extraction (dense vs sparse).
module Bench.OverlayExtract (benchmarks) where

import Test.Tasty.Bench

import Fixtures
import UI.OverlayExtract (extractOverlayField)

benchmarks :: Benchmark
benchmarks = bgroup "OverlayExtract"
  [ bgroup "extractOverlayField"
    [ bench "dense/16chunks"  $ nf extractDense ()
    , bench "sparse/16chunks" $ nf extractSparse ()
    ]
  ]

extractDense :: () -> ()
extractDense _ =
  case extractOverlayField "bench_overlay" 0 64 benchOverlayStoreDense of
    Nothing -> ()
    Just m  -> m `seq` ()

extractSparse :: () -> ()
extractSparse _ =
  case extractOverlayField "bench_overlay" 0 64 benchOverlayStoreSparse of
    Nothing -> ()
    Just m  -> m `seq` ()
