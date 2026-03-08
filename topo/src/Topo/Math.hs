{-# LANGUAGE BangPatterns #-}

-- | Small numeric helpers shared across terrain generators.
module Topo.Math
  ( clamp01
  , clampLat
  , lerp
  , smoothstep
  , iterateN
  , maxVectorOr
  , descendingIndicesByValue
  ) where

import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Vector.Unboxed as U

-- | Clamp a value into the inclusive $[0,1]$ range.
clamp01 :: Float -> Float
clamp01 v
  | v < 0 = 0
  | v > 1 = 1
  | otherwise = v

-- | Clamp a latitude (in radians) to $[-\pi/2, \pi/2]$.
--
-- Prevents the cosine curve from rebounding past the poles when
-- tile coordinates extend beyond 90\u00b0 latitude.
clampLat :: Float -> Float
clampLat l
  | l < negate halfPi = negate halfPi
  | l > halfPi        = halfPi
  | otherwise         = l
  where
    halfPi = pi / 2
{-# INLINE clampLat #-}

-- | Linearly interpolate between two values using $t$.
lerp :: Float -> Float -> Float -> Float
lerp a b t = a + (b - a) * t

-- | Smoothly interpolate between two edges.
smoothstep :: Float -> Float -> Float -> Float
smoothstep edge0 edge1 x =
  let t = clamp01 ((x - edge0) / (edge1 - edge0))
  in t * t * (3 - 2 * t)

-- | Apply a function $n$ times to a value, strict in the accumulator.
--
-- Without the bang on @x@, @iterateN 6 f v@ builds a thunk tower
-- @f(f(f(f(f(f(v))))))@ whose forcing cascades all at once.  For
-- grid-level erosion passes where each @f@ produces a fully-evaluated
-- @U.Vector Float@, the lazy version only defers work; it never saves
-- any.  Making the accumulator strict ensures each iteration is
-- evaluated before the next begins.
iterateN :: Int -> (a -> a) -> a -> a
iterateN n f !x
  | n <= 0 = x
  | otherwise = iterateN (n - 1) f (f x)

-- | Return vector maximum, or a fallback value when the vector is empty.
maxVectorOr :: (Ord a, U.Unbox a) => a -> U.Vector a -> a
maxVectorOr fallback vec
  | U.null vec = fallback
  | otherwise = U.maximum vec

-- | Return vector indices sorted by descending element value.
descendingIndicesByValue :: U.Vector Float -> [Int]
descendingIndicesByValue vec =
  let n = U.length vec
  in sortBy (comparing (\i -> negate (vec U.! i))) [0 .. n - 1]
