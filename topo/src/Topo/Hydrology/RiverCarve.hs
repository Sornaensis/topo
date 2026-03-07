{-# LANGUAGE BangPatterns #-}

-- | River carving and routing support helpers used by hydrology stages.
module Topo.Hydrology.RiverCarve
  ( carveRiversGrid
  , riverBankErodeGrid
  , riverDepthWithHardness
  , riverErosionPotential
  , riverDepositPotential
  , riverDepthFactor
  , riverErosionFactor
  , strahlerOrder
  ) where

import Control.Monad (forM_)
import Control.Monad.ST (ST)
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Word (Word16)
import Topo.Hex (hexNeighborIndices)
import Topo.Math (clamp01)
import Topo.TerrainGrid (gridSlopeAt)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

hardnessFactor :: Float -> Float -> Float
hardnessFactor hardnessErodeWeight hard =
  clamp01 (1 - clamp01 hard * hardnessErodeWeight)

-- | Carve river channels based on normalized flow and hardness.
carveRiversGrid
  :: Float
  -> Float
  -> Float
  -> Float
  -> Float
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Float
carveRiversGrid minAccum waterLevel riverCarveMaxDepth riverCarveScale hardnessErodeWeight elev acc hardness erosionMult =
  let maxAcc = max minAccum (U.maximum acc)
  in U.imap (carveAt maxAcc) elev
  where
    carveAt maxAcc i h0 =
      let a = acc U.! i
          flowNorm = clamp01 (a / maxAcc)
          depth = min riverCarveMaxDepth
                      (flowNorm * riverCarveScale * hardnessFactor hardnessErodeWeight (hardness U.! i))
                * (erosionMult U.! i)
      in if h0 > waterLevel
          then h0 - depth
          else h0

-- | Erode river banks adjacent to strong-flow neighbours.
riverBankErodeGrid
  :: Int
  -> Int
  -> Float
  -> Float
  -> Float
  -> Float
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Float
riverBankErodeGrid gridW gridH minAccum riverBankThreshold riverBankDepth hardnessErodeWeight elev acc hardness erosionMult =
  let maxAcc = max minAccum (U.maximum acc)
      threshold = maxAcc * riverBankThreshold
  in U.generate (U.length elev) (bankAt threshold)
  where
    bankAt threshold i =
      let h0 = elev U.! i
          neighborHigh = any (\j -> acc U.! j > threshold)
                             (hexNeighborIndices gridW gridH i)
          bankDepth = riverBankDepth * hardnessFactor hardnessErodeWeight (hardness U.! i)
                    * (erosionMult U.! i)
      in if neighborHigh then h0 - bankDepth else h0

-- | Compute per-tile channel depth from accumulation and bed hardness.
riverDepthWithHardness
  :: Float
  -> Float
  -> Float
  -> Float
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Float
riverDepthWithHardness minAccum channelMaxDepth channelDepthScale hardnessDepthWeight acc hardness =
  U.imap
    (\i a ->
      if a < minAccum
        then 0
        else min channelMaxDepth (a * channelDepthScale * riverDepthFactor hardnessDepthWeight (hardness U.! i)))
    acc

-- | Compute per-tile river erosion potential.
riverErosionPotential
  :: Int
  -> Int
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Float
  -> Float
  -> Float
  -> Float
  -> U.Vector Float
riverErosionPotential gridW gridH elev acc hardness minAccum erosionScale hardnessErosionWeight =
  let maxAcc = max minAccum (U.maximum acc)
  in U.imap
      (\i a ->
        if a < minAccum
          then 0
          else
            let flowNorm = clamp01 (a / maxAcc)
                slope = gridSlopeAt gridW gridH elev i
                hard = riverErosionFactor hardnessErosionWeight (hardness U.! i)
            in flowNorm * slope * erosionScale * hard)
      acc

-- | Compute per-tile river deposition potential.
riverDepositPotential
  :: Int
  -> Int
  -> U.Vector Float
  -> U.Vector Float
  -> Float
  -> Float
  -> Float
  -> U.Vector Float
riverDepositPotential gridW gridH elev acc minAccum depositMaxSlope depositScale =
  let maxAcc = max minAccum (U.maximum acc)
      maxSlope = depositMaxSlope
  in U.imap
      (\i a ->
        if a < minAccum
          then 0
          else
            let slope = gridSlopeAt gridW gridH elev i
                flowNorm = clamp01 (a / maxAcc)
                slopeFactor = if slope >= maxSlope then 0 else clamp01 (1 - slope / maxSlope)
            in flowNorm * slopeFactor * depositScale)
      acc

-- | Hardness attenuation factor for channel depth.
riverDepthFactor :: Float -> Float -> Float
riverDepthFactor hardnessDepthWeight hard =
  clamp01 (1 - clamp01 hard * hardnessDepthWeight)

-- | Hardness attenuation factor for erosion potential.
riverErosionFactor :: Float -> Float -> Float
riverErosionFactor hardnessErosionWeight hard =
  clamp01 (1 - clamp01 hard * hardnessErosionWeight)

-- | Compute Strahler stream orders using 6-neighbour upstream detection.
strahlerOrder
  :: Int
  -> Int
  -> U.Vector Float
  -> U.Vector Int
  -> U.Vector Float
  -> Float
  -> U.Vector Word16
strahlerOrder gridW gridH elev flow acc minAccum = U.create $ do
  let n = U.length elev
  orders <- UM.replicate n (0 :: Word16)
  let indices = sortBy (comparing (\i -> negate (elev U.! i))) [0 .. n - 1]
  forM_ indices $ \i -> do
    let a = acc U.! i
    if a < minAccum
      then UM.write orders i 0
      else do
        ups <- upstreamOrders gridW gridH flow orders i
        case ups of
          [] -> UM.write orders i 1
          _ -> do
            let maxO = maximum ups
                countMax = length (filter (== maxO) ups)
                nextOrder = if countMax >= 2 then maxO + 1 else maxO
            UM.write orders i nextOrder
  pure orders

upstreamOrders :: Int -> Int -> U.Vector Int -> UM.MVector s Word16 -> Int -> ST s [Word16]
upstreamOrders gridW gridH flow orders i =
  let incoming = filter (\j -> flow U.! j == i) (hexNeighborIndices gridW gridH i)
  in mapM (UM.read orders) incoming
