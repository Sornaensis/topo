-- | Shared helpers for converting between normalized slider positions and
-- domain values.
module Seer.Config.Range
  ( mapRange
  , mapIntRange
  , unmapRange
  , unmapIntRange
  ) where

-- | Map a normalized value in @[0,1]@ into a domain range.
mapRange :: Float -> Float -> Float -> Float
mapRange lo hi t =
  let t' = max 0 (min 1 t)
  in lo + (hi - lo) * t'

-- | Map a normalized value in @[0,1]@ into an integral domain range.
mapIntRange :: Int -> Int -> Float -> Int
mapIntRange lo hi t =
  round (mapRange (fromIntegral lo) (fromIntegral hi) t)

-- | Inverse of 'mapRange': given a domain value in @[lo, hi]@, return the
-- normalized slider position in @[0,1]@.
unmapRange :: Float -> Float -> Float -> Float
unmapRange lo hi v
  | hi == lo = 0
  | otherwise = max 0 (min 1 ((v - lo) / (hi - lo)))

-- | Inverse of 'mapIntRange': given an integral domain value, return the
-- normalized slider position in @[0,1]@.
unmapIntRange :: Int -> Int -> Int -> Float
unmapIntRange lo hi v =
  unmapRange (fromIntegral lo) (fromIntegral hi) (fromIntegral v)