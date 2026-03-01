module Topo.Weather.Operators
  ( sampleGridClamped
  , advectFieldGrid
  , diffuseFieldGridWeather
  , normalizeAngle
  , lerpAngle
  , climatePull
  , condensation
  , pressureGradientAt
  , windDirectionFromPressure
  , windSpeedFromPressure
  ) where

import qualified Data.Vector.Unboxed as U
import Topo.Climate.Evaporation (satNorm)
import Topo.TerrainGrid (clampCoordGrid)

sampleGridClamped :: Int -> Int -> U.Vector Float -> Int -> Int -> Float
sampleGridClamped gridWidth gridHeight grid x y =
  let clampedX = clampCoordGrid gridWidth x
      clampedY = clampCoordGrid gridHeight y
  in grid U.! (clampedY * gridWidth + clampedX)

advectFieldGrid
  :: Int
  -> Int
  -> Float
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Float
advectFieldGrid gridWidth gridHeight dt windDir windSpd field =
  U.generate (gridWidth * gridHeight) step
  where
    step idx =
      let x = idx `mod` gridWidth
          y = idx `div` gridWidth
          center = field U.! idx
          direction = windDir U.! idx
          speed = windSpd U.! idx
          windX = speed * cos direction
          windY = speed * sin direction
          left = sampleGridClamped gridWidth gridHeight field (x - 1) y
          right = sampleGridClamped gridWidth gridHeight field (x + 1) y
          up = sampleGridClamped gridWidth gridHeight field x (y - 1)
          down = sampleGridClamped gridWidth gridHeight field x (y + 1)
          dFieldDx = if windX >= 0 then center - left else right - center
          dFieldDy = if windY >= 0 then center - up else down - center
      in center - dt * (windX * dFieldDx + windY * dFieldDy)

diffuseFieldGridWeather :: Int -> Int -> Int -> Float -> U.Vector Float -> U.Vector Float
diffuseFieldGridWeather gridWidth gridHeight iterations factor field =
  iterateN iterations field
  where
    iterateN passCount current
      | passCount <= 0 = current
      | otherwise = iterateN (passCount - 1) (diffuseOnce current)
    diffuseOnce vec = U.generate (gridWidth * gridHeight) (diffuseAt vec)
    diffuseAt vec idx =
      let x = idx `mod` gridWidth
          y = idx `div` gridWidth
          center = vec U.! idx
          left = sampleGridClamped gridWidth gridHeight vec (x - 1) y
          right = sampleGridClamped gridWidth gridHeight vec (x + 1) y
          up = sampleGridClamped gridWidth gridHeight vec x (y - 1)
          down = sampleGridClamped gridWidth gridHeight vec x (y + 1)
          avg = (center + left + right + up + down) / 5
      in center * (1 - factor) + avg * factor

normalizeAngle :: Float -> Float
normalizeAngle angle =
  let tau = 2 * pi
      wrapped = angle - tau * fromIntegral (floor (angle / tau) :: Int)
  in if wrapped < 0 then wrapped + tau else wrapped

lerpAngle :: Float -> Float -> Float -> Float
lerpAngle start end t =
  let delta = atan2 (sin (end - start)) (cos (end - start))
  in normalizeAngle (start + t * delta)

climatePull :: Float -> Float -> Float -> Float
climatePull current target pullStrength = current + pullStrength * (target - current)

condensation :: Float -> Float -> Float -> Float
condensation humidity temperature rate =
  let saturation = max 0.001 (satNorm temperature)
  in max 0 (humidity - saturation) * rate

pressureGradientAt :: Int -> Int -> U.Vector Float -> Int -> (Float, Float)
pressureGradientAt gridWidth gridHeight pressure idx =
  let x = idx `mod` gridWidth
      y = idx `div` gridWidth
      pressureLeft = sampleGridClamped gridWidth gridHeight pressure (x - 1) y
      pressureRight = sampleGridClamped gridWidth gridHeight pressure (x + 1) y
      pressureUp = sampleGridClamped gridWidth gridHeight pressure x (y - 1)
      pressureDown = sampleGridClamped gridWidth gridHeight pressure x (y + 1)
      dPressureDx = (pressureRight - pressureLeft) * 0.5
      dPressureDy = (pressureDown - pressureUp) * 0.5
  in (dPressureDx, dPressureDy)

windDirectionFromPressure :: Int -> Int -> U.Vector Float -> Float -> Float -> Int -> Float
windDirectionFromPressure gridWidth gridHeight pressure previousDirection response idx =
  let (dPressureDx, dPressureDy) = pressureGradientAt gridWidth gridHeight pressure idx
      gradientDirection = atan2 (negate dPressureDy) (negate dPressureDx)
  in lerpAngle previousDirection gradientDirection response

windSpeedFromPressure :: Int -> Int -> U.Vector Float -> Float -> Float -> Int -> Float
windSpeedFromPressure gridWidth gridHeight pressure previousSpeed gradientScale idx =
  let (dPressureDx, dPressureDy) = pressureGradientAt gridWidth gridHeight pressure idx
      gradientMagnitude = sqrt (dPressureDx * dPressureDx + dPressureDy * dPressureDy)
  in previousSpeed * (1 + gradientScale * gradientMagnitude)