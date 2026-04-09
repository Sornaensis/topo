{-# LANGUAGE OverloadedStrings #-}

-- | Benchmarks for atlas manager queue dedup at various queue depths.
--
-- The 'AtlasManager' actor deduplicates enqueue requests by scanning the
-- queue list with @filter@ and appending the new job.  This is O(n) per
-- enqueue.  These benchmarks exercise the pure dedup+append pattern at
-- realistic queue depths to quantify the scheduling overhead under load.
module Bench.QueueDedup (benchmarks) where

import Control.DeepSeq (NFData(..))
import Test.Tasty.Bench

import Actor.AtlasCache (AtlasKey(..))
import Actor.UI.State (ViewMode(..))
import Fixtures ()  -- NFData AtlasKey instance

------------------------------------------------------------------------
-- Pure dedup logic (mirrors AtlasManager on_enqueue)
------------------------------------------------------------------------

-- | A stripped-down job for benchmarking: only the fields used by dedup.
data BenchJob = BenchJob
  { bjKey       :: !AtlasKey
  , bjHexRadius :: !Int
  } deriving (Eq)

instance NFData BenchJob where
  rnf (BenchJob k hr) = rnf k `seq` rnf hr

-- | Pure dedup+append: filter out matching (key, hexRadius), then append.
-- This is exactly what the AtlasManager actor does on each enqueue.
dedupEnqueue :: BenchJob -> [BenchJob] -> [BenchJob]
dedupEnqueue job queue =
  let pruned = filter (\j -> not (bjKey j == bjKey job && bjHexRadius j == bjHexRadius job)) queue
  in pruned ++ [job]

------------------------------------------------------------------------
-- Queue fixtures at various depths
------------------------------------------------------------------------

-- | Build a queue of n jobs with distinct (key, hexRadius) pairs.
-- Uses alternating ViewModes and hexRadii [6,10,18,32,50] to mimic
-- real scheduling patterns.
buildQueue :: Int -> [BenchJob]
buildQueue n =
  [ BenchJob (AtlasKey (pickMode i) 0.3 1) (pickRadius i)
  | i <- [0 .. n - 1]
  ]

pickMode :: Int -> ViewMode
pickMode i = case i `mod` 5 of
  0 -> ViewElevation
  1 -> ViewBiome
  2 -> ViewClimate
  3 -> ViewMoisture
  _ -> ViewWeather

pickRadius :: Int -> Int
pickRadius i = [6, 10, 18, 32, 50] !! (i `mod` 5)

-- | A new job that does NOT match any existing queue entry (worst case:
-- full scan, no pruning).
newJobNoMatch :: BenchJob
newJobNoMatch = BenchJob (AtlasKey ViewTerrainForm 0.5 99) 6

-- | A new job that DOES match an existing entry (triggers pruning).
newJobMatch :: Int -> BenchJob
newJobMatch queueSize =
  let midIdx = queueSize `div` 2
  in BenchJob (AtlasKey (pickMode midIdx) 0.3 1) (pickRadius midIdx)

------------------------------------------------------------------------
-- Benchmarks
------------------------------------------------------------------------

benchmarks :: Benchmark
benchmarks = bgroup "QueueDedup"
  [ bgroup "enqueue (no match, full scan)"
    [ bench "depth 10"  $ nf (dedupEnqueue newJobNoMatch) (buildQueue 10)
    , bench "depth 50"  $ nf (dedupEnqueue newJobNoMatch) (buildQueue 50)
    , bench "depth 200" $ nf (dedupEnqueue newJobNoMatch) (buildQueue 200)
    , bench "depth 500" $ nf (dedupEnqueue newJobNoMatch) (buildQueue 500)
    ]
  , bgroup "enqueue (match, prune + append)"
    [ bench "depth 10"  $ nf (dedupEnqueue (newJobMatch 10))  (buildQueue 10)
    , bench "depth 50"  $ nf (dedupEnqueue (newJobMatch 50))  (buildQueue 50)
    , bench "depth 200" $ nf (dedupEnqueue (newJobMatch 200)) (buildQueue 200)
    , bench "depth 500" $ nf (dedupEnqueue (newJobMatch 500)) (buildQueue 500)
    ]
  , bgroup "burst enqueue (10 jobs into queue)"
    [ bench "into depth 10"  $ nf (burstEnqueue 10)  (buildQueue 10)
    , bench "into depth 50"  $ nf (burstEnqueue 10)  (buildQueue 50)
    , bench "into depth 200" $ nf (burstEnqueue 10)  (buildQueue 200)
    ]
  ]

-- | Enqueue a burst of n distinct jobs (simulates rapid view-mode cycling).
burstEnqueue :: Int -> [BenchJob] -> [BenchJob]
burstEnqueue n queue = foldl (flip dedupEnqueue) queue burst
  where
    burst = [ BenchJob (AtlasKey (pickMode (100 + i)) 0.3 (fromIntegral i)) (pickRadius i)
            | i <- [0 .. n - 1]
            ]
