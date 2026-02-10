module Topo.Noise
  ( noise2D
  , noise2DContinuous
  , fbm2D
  , ridgedFbm2D
  , domainWarp2D
  , directionalRidge2D
  ) where

import Data.Bits (xor, shiftR, (.&.))
import Data.Word (Word32, Word64)

noise2D :: Word64 -> Int -> Int -> Float
noise2D seed x y =
  let h = mix32 (fromIntegral seed + fromIntegral x * 0x45d9f3b + fromIntegral y * 0x27d4eb2d)
  in fromIntegral h / fromIntegral (maxBound :: Word32)

noise2DContinuous :: Word64 -> Float -> Float -> Float
noise2DContinuous seed x y =
  let x0 = floor x
      y0 = floor y
      x1 = x0 + 1
      y1 = y0 + 1
      fx = x - fromIntegral x0
      fy = y - fromIntegral y0
      u = fade fx
      v = fade fy
      n00 = gradDot seed x0 y0 fx fy
      n10 = gradDot seed x1 y0 (fx - 1) fy
      n01 = gradDot seed x0 y1 fx (fy - 1)
      n11 = gradDot seed x1 y1 (fx - 1) (fy - 1)
      ix0 = lerp n00 n10 u
      ix1 = lerp n01 n11 u
      value = lerp ix0 ix1 v
  in clamp01 (0.5 + 0.5 * value)

fbm2D :: Word64 -> Int -> Float -> Float -> Float -> Float -> Float
fbm2D seed octaves lacunarity gain x y
  | octaves <= 0 = 0
  | otherwise = go octaves 1 1 0
  where
    go 0 _ _ acc = acc
    go o freq amp acc =
      let nx = x * freq
          ny = y * freq
          n0 = noise2DContinuous (seed + fromIntegral o) nx ny * 2 - 1
      in go (o - 1) (freq * lacunarity) (amp * gain) (acc + n0 * amp)

ridgedFbm2D :: Word64 -> Int -> Float -> Float -> Float -> Float -> Float
ridgedFbm2D seed octaves lacunarity gain x y
  | octaves <= 0 = 0
  | otherwise = go octaves 1 1 0
  where
    go 0 _ _ acc = acc
    go o freq amp acc =
      let nx = x * freq
          ny = y * freq
          n0 = noise2DContinuous (seed + fromIntegral o) nx ny * 2 - 1
          ridge = 1 - abs n0
          ridge' = ridge * ridge
      in go (o - 1) (freq * lacunarity) (amp * gain) (acc + ridge' * amp)

domainWarp2D :: Word64 -> Float -> Float -> Float -> Float -> (Float, Float)
domainWarp2D seed scale strength x y =
  let nx = noise2DContinuous (seed + 101) (x * scale) (y * scale) * 2 - 1
      ny = noise2DContinuous (seed + 103) (x * scale + 57) (y * scale + 23) * 2 - 1
  in (x + nx * strength, y + ny * strength)

directionalRidge2D :: Word64 -> Int -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float
directionalRidge2D seed octaves lacunarity gain scale x y dirX dirY =
  let angle = atan2 dirY dirX
      c = cos angle
      s = sin angle
      rx = x * c - y * s
      ry = x * s + y * c
  in ridgedFbm2D seed octaves lacunarity gain (rx * scale) (ry * scale)

fade :: Float -> Float
fade t = t * t * t * (t * (t * 6 - 15) + 10)

lerp :: Float -> Float -> Float -> Float
lerp a b t = a + (b - a) * t

gradDot :: Word64 -> Int -> Int -> Float -> Float -> Float
gradDot seed x y dx dy =
  let (gx, gy) = grad2 seed x y
  in gx * dx + gy * dy

grad2 :: Word64 -> Int -> Int -> (Float, Float)
grad2 seed x y =
  let h = mix32 (fromIntegral seed + fromIntegral x * 0x45d9f3b + fromIntegral y * 0x27d4eb2d)
      idx = fromIntegral (h .&. 7) :: Int
  in case idx of
    0 -> (1, 0)
    1 -> (-1, 0)
    2 -> (0, 1)
    3 -> (0, -1)
    4 -> (0.7071, 0.7071)
    5 -> (-0.7071, 0.7071)
    6 -> (0.7071, -0.7071)
    _ -> (-0.7071, -0.7071)

clamp01 :: Float -> Float
clamp01 v
  | v < 0 = 0
  | v > 1 = 1
  | otherwise = v

mix32 :: Word64 -> Word32
mix32 v =
  let v1 = fromIntegral v :: Word32
      v2 = v1 `xor` (v1 `shiftR` 16)
      v3 = v2 * 0x7feb352d
      v4 = v3 `xor` (v3 `shiftR` 15)
  in v4 * 0x846ca68b
