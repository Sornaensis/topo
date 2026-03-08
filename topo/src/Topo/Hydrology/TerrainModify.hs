{-# LANGUAGE BangPatterns #-}

-- | Terrain modification helpers used by hydrology stages.
module Topo.Hydrology.TerrainModify
  ( applyStreamPowerErosion
  , alluvialDepositGrid
  , wetErodeGrid
  , coastalErodeGrid
  , piedmontSmoothGrid
  ) where

import Topo.Hex (hexNeighborIndices)
import Topo.Math (clamp01, maxVectorOr)
import Topo.TerrainGrid (gridSlopeAt)
import Topo.Types
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Control.Monad (forM_)

maximumOr :: Ord a => a -> [a] -> a
maximumOr fallback xs =
  case xs of
    [] -> fallback
    _ -> maximum xs

hardnessFactor :: Float -> Float -> Float
hardnessFactor hardnessErodeWeight hard =
  clamp01 (1 - clamp01 hard * hardnessErodeWeight)

neighborMin :: Int -> Int -> U.Vector Float -> Int -> Float
neighborMin gridW gridH elev i =
  let h0 = elev U.! i
      nbrHeights = map (elev U.!) (hexNeighborIndices gridW gridH i)
  in minimum (h0 : nbrHeights)

-- | Apply stream-power erosion and downstream sediment deposition.
applyStreamPowerErosion
  :: Float
  -> Float
  -> Float
  -> Float
  -> U.Vector Float
  -> U.Vector Int
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Float
applyStreamPowerErosion streamPowerMaxErosion streamPowerScale hardnessErodeWeight streamDepositRatio elev flow acc hardness erosionMult depositFactor = U.create $ do
  let n = U.length elev
  base <- U.thaw elev
  deposit <- UM.replicate n 0
  let erosionAt i d =
        let h0 = elev U.! i
            h1 = elev U.! d
            slope = max 0 (h0 - h1)
            power = (acc U.! i) * slope
        in min streamPowerMaxErosion
              (power * streamPowerScale * hardnessFactor hardnessErodeWeight (hardness U.! i))
              * (erosionMult U.! i)
  forM_ [0 .. n - 1] $ \i -> do
    let d = flow U.! i
    if d >= 0
      then do
        let erosion = erosionAt i d
            depositAmt = erosion * streamDepositRatio * (depositFactor U.! d)
        UM.modify base (\v -> v - erosion) i
        UM.modify deposit (+ depositAmt) d
      else pure ()
  forM_ [0 .. n - 1] $ \i -> do
    d <- UM.read deposit i
    UM.modify base (+ d) i
  pure base

-- | Deposit alluvial sediment where rivers decelerate.
alluvialDepositGrid
  :: Float
  -> Float
  -> Float
  -> Int
  -> Int
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Float
alluvialDepositGrid minAccum alluvialMaxSlope waterLevel gridW gridH elev acc depositFactor =
  let maxAcc = max minAccum (maxVectorOr minAccum acc)
  in U.generate (U.length elev) (depositAt maxAcc)
  where
    depositAt maxAcc i =
      let h0 = elev U.! i
          a = acc U.! i
          flowNorm = clamp01 (a / maxAcc)
          slope = gridSlopeAt gridW gridH elev i
          slopeFactor = if slope >= alluvialMaxSlope then 0 else clamp01 (1 - slope / alluvialMaxSlope)
          rawDeposit = if h0 > waterLevel
                         then flowNorm * slopeFactor * (depositFactor U.! i)
                         else 0
          nbrMin = neighborMin gridW gridH elev i
          maxDeposit = if nbrMin > h0 then nbrMin - h0 else 0
      in h0 + min rawDeposit maxDeposit

-- | Apply moisture-driven erosion on land tiles.
wetErodeGrid
  :: Float
  -> Float
  -> Float
  -> Float
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Float
wetErodeGrid minMoisture waterLevel wetErodeScale hardnessErodeWeight elev moisture hardness erosionMult =
  let maxMoist = max minMoisture (maxVectorOr minMoisture moisture)
  in U.generate (U.length elev) (wetErodeAt maxMoist)
  where
    wetErodeAt maxMoist i =
      let h0 = elev U.! i
          m = moisture U.! i / maxMoist
          depth = clamp01 m * wetErodeScale * hardnessFactor hardnessErodeWeight (hardness U.! i)
                * (erosionMult U.! i)
      in if h0 > waterLevel then h0 - depth else h0

-- | Two-part coastal reshaping: erode land adjacent to water and
-- raise shallow ocean adjacent to land.
coastalErodeGrid
  :: Float
  -> Float
  -> Float
  -> Float
  -> Int
  -> Int
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Float
coastalErodeGrid waterLevel coastalErodeStrength coastalRaiseFactor hardnessErodeWeight gridW gridH elev hardness erosionMult smoothResist =
  U.generate (U.length elev) coastalAt
  where
    coastalAt i =
      let h0 = elev U.! i
          localStrength = coastalErodeStrength * hardnessFactor hardnessErodeWeight (hardness U.! i)
          neighbors = map (elev U.!) (hexNeighborIndices gridW gridH i)
          anyWater = any (< waterLevel) neighbors
          anyLand = any (>= waterLevel) neighbors
          lower = min (localStrength * (erosionMult U.! i)) (h0 - waterLevel)
          resist = smoothResist U.! i
          raise = min (localStrength * coastalRaiseFactor * (1 - resist)) (waterLevel - h0)
      in if h0 >= waterLevel && anyWater
          then h0 - max 0 lower
          else if h0 < waterLevel && anyLand
            then h0 + max 0 raise
            else h0

-- | Smooth the piedmont (foothills) transition zone.
piedmontSmoothGrid
  :: Float
  -> Float
  -> Float
  -> Float
  -> Int
  -> Int
  -> U.Vector Float
  -> U.Vector TerrainForm
  -> U.Vector Float
  -> U.Vector Float
piedmontSmoothGrid slopeMin slopeMax strength waterLevel gridW gridH elev formGrid smoothResist =
  U.generate (U.length elev) $ \i ->
    let h0 = elev U.! i
        form = formGrid U.! i
        nbrs = hexNeighborIndices gridW gridH i
    in case nbrs of
         [] -> h0
         _ ->
           let slope = maximumOr 0 [abs (elev U.! j - h0) | j <- nbrs]
               isFoothill = form == FormFoothill
               hasSteeperNbr = any
                 (\j ->
                   let nSlope = maximumOr 0 [abs (elev U.! k - elev U.! j)
                                           | k <- hexNeighborIndices gridW gridH j]
                   in nSlope > slopeMax)
                 nbrs
               inSlopeBand = slope >= slopeMin && slope <= slopeMax && hasSteeperNbr
               eligible = isFoothill || inSlopeBand
               nbrMean = sum (map (elev U.!) nbrs) / fromIntegral (length nbrs)
               slopeDenominatorEpsilon = 1e-6
               t = clamp01 ((slope - slopeMin) / max slopeDenominatorEpsilon (slopeMax - slopeMin))
               resist = smoothResist U.! i
               blend = strength * t * (1 - t) * 4 * (1 - resist)
           in if h0 > waterLevel && eligible
                then h0 + blend * (nbrMean - h0)
                else h0
