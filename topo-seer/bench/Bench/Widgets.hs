{-# LANGUAGE OverloadedStrings #-}

-- | Benchmarks for widget hit testing.
module Bench.Widgets (benchmarks) where

import Linear (V2(..))
import Test.Tasty.Bench

import UI.WidgetId (WidgetId(..))
import UI.WidgetTree (Widget(..), hitTest)
import UI.Widgets (Rect(..))

benchmarks :: Benchmark
benchmarks = bgroup "Widgets"
  [ bgroup "hitTest"
    [ bench "10 widgets"  $ whnf (hitTest (makeWidgets 10))  (V2 150 150)
    , bench "50 widgets"  $ whnf (hitTest (makeWidgets 50))  (V2 750 750)
    , bench "100 widgets" $ whnf (hitTest (makeWidgets 100)) (V2 1500 1500)
    ]
  ]

makeWidgets :: Int -> [Widget]
makeWidgets n =
  [ Widget
    { widgetId   = WidgetGenerate
    , widgetRect = Rect (V2 (i * 30) (i * 15), V2 25 25)
    }
  | i <- [0 .. n - 1]
  ]
