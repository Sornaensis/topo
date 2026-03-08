{-# LANGUAGE BangPatterns #-}

-- | Flow accumulation and moisture derivation helpers for hydrology.
module Topo.Hydrology.Flow
  ( flowAccumulation
  , flowAccumulationWithBase
  , moistureFromAccumulation
  ) where

import Control.Monad (forM_)
import Topo.Math (clamp01, descendingIndicesByValue, maxVectorOr)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

accumulateFlowGeneric
  :: Float
  -> U.Vector Float
  -> U.Vector Int
  -> (Int -> Float -> Float)
  -> U.Vector Float
accumulateFlowGeneric baseAccum elev flow downstreamContribution = U.create $ do
  let n = U.length elev
      indices = descendingIndicesByValue elev
  acc <- UM.replicate n baseAccum
  forM_ indices $ \i -> do
    let d = flow U.! i
    if d >= 0
      then do
        v <- UM.read acc i
        UM.modify acc (+ downstreamContribution i v) d
      else pure ()
  pure acc

-- | Accumulate flow from high to low elevation with per-tile flow bonuses.
flowAccumulation
  :: Float
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Int
  -> U.Vector Float
flowAccumulation baseAccum flowBonus elev flow =
  accumulateFlowGeneric baseAccum elev flow
    (\i v ->
      let bonus = flowBonus U.! i
      in v * (1 + bonus))

-- | Baseline flow accumulation without per-tile bonus terms.
flowAccumulationWithBase :: Float -> U.Vector Float -> U.Vector Int -> U.Vector Float
flowAccumulationWithBase baseAccum elev flow =
  accumulateFlowGeneric baseAccum elev flow (\_ v -> v)

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
  let maxAcc = max minAccum (maxVectorOr minAccum acc)
  in U.imap
      (\i a ->
        let base = clamp01 (waterLevel - elev U.! i)
            flowM = clamp01 (a / maxAcc)
        in clamp01 (base * baseWeight + flowM * flowWeight))
      acc
