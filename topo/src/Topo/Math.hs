-- | Small numeric helpers shared across terrain generators.
module Topo.Math
  ( clamp01
  , clampLat
  , lerp
  , smoothstep
  , iterateN
  ) where

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

-- | Apply a function $n$ times to a value.
iterateN :: Int -> (a -> a) -> a -> a
iterateN n f x
  | n <= 0 = x
  | otherwise = iterateN (n - 1) f (f x)
