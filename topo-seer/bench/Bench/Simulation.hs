{-# LANGUAGE OverloadedStrings #-}

-- | Benchmarks for simulation DAG construction and tick execution.
module Bench.Simulation (benchmarks) where

import qualified Data.Text as T
import Test.Tasty.Bench

import Fixtures
import Topo

benchmarks :: Benchmark
benchmarks = bgroup "Simulation"
  [ bgroup "buildSimDAG"
    [ bench "empty"      $ whnf buildSimDAG []
    , bench "3 readers"  $ whnf buildSimDAG threeReaders
    , bench "10 readers" $ whnf buildSimDAG tenReaders
    , bench "chain of 5" $ whnf buildSimDAG chain5
    ]
  , bgroup "tickSimulation"
    [ bench "3 readers (no-op)" $ whnfIO (tickSmall threeReaderDAG)
    , bench "10 readers (no-op)" $ whnfIO (tickSmall tenReaderDAG)
    ]
  ]

-- | Three independent reader nodes with no-op tick actions.
threeReaders :: [SimNode]
threeReaders =
  [ SimNodeReader
      { snrId           = SimNodeId "a"
      , snrOverlayName  = "overlay_a"
      , snrDependencies = []
      , snrReadTick     = \_ o -> pure (Right o)
      }
  , SimNodeReader
      { snrId           = SimNodeId "b"
      , snrOverlayName  = "overlay_b"
      , snrDependencies = []
      , snrReadTick     = \_ o -> pure (Right o)
      }
  , SimNodeReader
      { snrId           = SimNodeId "c"
      , snrOverlayName  = "overlay_c"
      , snrDependencies = []
      , snrReadTick     = \_ o -> pure (Right o)
      }
  ]

-- | Ten independent reader nodes with no-op tick actions.
tenReaders :: [SimNode]
tenReaders =
  [ SimNodeReader
      { snrId           = SimNodeId (T.pack ("node_" ++ show i))
      , snrOverlayName  = T.pack ("overlay_" ++ show i)
      , snrDependencies = []
      , snrReadTick     = \_ o -> pure (Right o)
      }
  | i <- [0..9 :: Int]
  ]

-- | A chain of 5 readers where each depends on the previous.
chain5 :: [SimNode]
chain5 =
  [ SimNodeReader
      { snrId           = SimNodeId (T.pack ("chain_" ++ show i))
      , snrOverlayName  = T.pack ("chain_overlay_" ++ show i)
      , snrDependencies = [SimNodeId (T.pack ("chain_" ++ show (i - 1))) | i > 0]
      , snrReadTick     = \_ o -> pure (Right o)
      }
  | i <- [0..4 :: Int]
  ]

threeReaderDAG :: SimDAG
threeReaderDAG = case buildSimDAG threeReaders of
  Right dag -> dag
  Left e    -> error ("Failed to build 3-reader DAG: " ++ show e)

tenReaderDAG :: SimDAG
tenReaderDAG = case buildSimDAG tenReaders of
  Right dag -> dag
  Left e    -> error ("Failed to build 10-reader DAG: " ++ show e)

tickSmall :: SimDAG -> IO (Either T.Text (OverlayStore, TerrainWrites))
tickSmall dag =
  tickSimulation dag (\_ -> pure ()) benchWorld emptyOverlayStore
    (CalendarDate 0 0 0.0) defaultWorldTime 1
