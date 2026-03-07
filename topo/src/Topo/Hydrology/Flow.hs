{-# LANGUAGE BangPatterns #-}

-- | Flow accumulation and moisture derivation helpers for hydrology.
module Topo.Hydrology.Flow
  ( flowAccumulation
  , flowAccumulationWithBase
  , moistureFromAccumulation
  ) where

import Control.Monad (forM_)
import Data.List (sortBy)
import Data.Ord (comparing)
import Topo.Math (clamp01)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- | Accumulate flow from high to low elevation with per-tile flow bonuses.
flowAccumulation
  :: Float
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Int
  -> U.Vector Float
flowAccumulation baseAccum flowBonus elev flow = U.create $ do
  let n = U.length elev
  acc <- UM.replicate n baseAccum
  let indices = sortBy (comparing (\i -> negate (elev U.! i))) [0 .. n - 1]
  forM_ indices $ \i -> do
    let d = flow U.! i
    if d >= 0
      then do
        v <- UM.read acc i
        let bonus = flowBonus U.! i
        UM.modify acc (+ v * (1 + bonus)) d
      else pure ()
  pure acc

-- | Baseline flow accumulation without per-tile bonus terms.
flowAccumulationWithBase :: Float -> U.Vector Float -> U.Vector Int -> U.Vector Float
flowAccumulationWithBase baseAccum elev flow = U.create $ do
  let n = U.length elev
  acc <- UM.replicate n baseAccum
  let indices = sortBy (comparing (\i -> negate (elev U.! i))) [0 .. n - 1]
  forM_ indices $ \i -> do
    let d = flow U.! i
    if d >= 0
      then do
        v <- UM.read acc i
        UM.modify acc (+ v) d
      else pure ()
  pure acc

-- | Blend base (water-level relative) and flow-derived moisture terms.
moistureFromAccumulation
  :: Float
  -> Float
  -> Float
  -> Float
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Float
moistureFromAccumulation minAccum waterLevel baseWeight flowWeight elev acc =
  let maxAcc = max minAccum (U.maximum acc)
  in U.imap
      (\i a ->
        let base = clamp01 (waterLevel - elev U.! i)
            flowM = clamp01 (a / maxAcc)
        in clamp01 (base * baseWeight + flowM * flowWeight))
      acc
