{-# LANGUAGE BangPatterns #-}

-- | Basin and groundwater helper functions for hydrology stages.
module Topo.Hydrology.Groundwater
  ( basinIdsFromFlow
  , basinRechargeStats
  , basinStorageStats
  , basinBaseflow
  , basinPerTile
  ) where

import Control.Monad (forM_)
import Topo.Math (clamp01)
import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import Data.Word (Word32)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- | Resolve each tile to its terminal basin id by following flow links.
basinIdsFromFlow :: U.Vector Int -> U.Vector Word32
basinIdsFromFlow flow = U.map fromIntegral $ U.create $ do
  let n = U.length flow
  ids <- UM.replicate n (-1)
  let resolve i = do
        v <- UM.read ids i
        if v >= 0
          then pure v
          else do
            let d = flow U.! i
            if d < 0
              then UM.write ids i i >> pure i
              else do
                r <- resolve d
                UM.write ids i r
                pure r
  forM_ [0 .. n - 1] resolve
  pure ids

-- | Aggregate recharge and tile counts per basin.
basinRechargeStats :: Float -> U.Vector Word32 -> U.Vector Float -> IntMap (Float, Int)
basinRechargeStats rechargeScale basinIds moisture =
  let recharge = U.map (* rechargeScale) (U.map clamp01 moisture)
  in U.ifoldl'
      (\acc i bid ->
        let key = fromIntegral bid
            value = recharge U.! i
        in IntMap.insertWith
            (\(r1, c1) (r0, c0) -> (r0 + r1, c0 + c1))
            key
            (value, 1)
            acc)
      IntMap.empty
      basinIds

-- | Derive per-basin storage/discharge maps from recharge stats.
basinStorageStats
  :: Int
  -> Float
  -> Float
  -> IntMap (Float, Int)
  -> (IntMap Float, IntMap Float, IntMap Int)
basinStorageStats minBasinSize storageScale permeability stats =
  let toStorage (rechargeSum, count) =
        let eligible = count >= minBasinSize
            storage = if eligible then rechargeSum * storageScale else 0
            discharge = if eligible then storage * permeability else 0
        in (storage, discharge, count)
  in IntMap.foldlWithKey'
      (\(storageMap, dischargeMap, sizeMap) key value ->
        let (storage, discharge, count) = toStorage value
        in ( IntMap.insert key storage storageMap
           , IntMap.insert key discharge dischargeMap
           , IntMap.insert key count sizeMap
           ))
      (IntMap.empty, IntMap.empty, IntMap.empty)
      stats

-- | Spread basin discharge uniformly to tiles and scale for baseflow.
basinBaseflow :: U.Vector Word32 -> IntMap Float -> IntMap Int -> Float -> U.Vector Float
basinBaseflow basinIds dischargeMap sizeMap scale =
  U.imap
    (\_ bid ->
      let key = fromIntegral bid
          discharge = IntMap.findWithDefault 0 key dischargeMap
          size = max 1 (IntMap.findWithDefault 1 key sizeMap)
      in (discharge / fromIntegral size) * scale)
    basinIds

-- | Expand per-basin values to a per-tile vector.
basinPerTile :: U.Vector Word32 -> IntMap Float -> IntMap Int -> U.Vector Float
basinPerTile basinIds valueMap sizeMap =
  U.imap
    (\_ bid ->
      let key = fromIntegral bid
          value = IntMap.findWithDefault 0 key valueMap
          size = max 1 (IntMap.findWithDefault 1 key sizeMap)
      in value / fromIntegral size)
    basinIds
