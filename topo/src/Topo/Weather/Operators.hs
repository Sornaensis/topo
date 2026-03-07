-- | Weather-grid operators defined over the hex-topological dense climate
-- buffer representation.
module Topo.Weather.Operators
  ( advectFieldGrid
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
import Topo.Grid.HexDirection
  ( countHexNeighbors
  , directionVector
  , foldHexDirections
  , foldHexNeighbors
  , sampleUpwindHex
  )
import Topo.Hex (hexNeighborIndexInDirection)

-- | Advect a scalar field by following the continuous wind direction over
-- the six upwind hex neighbours of each tile.
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
      let center = field U.! idx
          direction = windDir U.! idx
          speed = windSpd U.! idx
          upwindValue = sampleUpwindHex gridWidth gridHeight (field U.!) idx direction
          gradient
            | dt <= 0 || speed <= 0 = 0
            | otherwise = center - upwindValue
      in center - dt * speed * gradient

        -- | Diffuse a weather field over the in-bounds hex neighbours of each tile.
diffuseFieldGridWeather :: Int -> Int -> Int -> Float -> U.Vector Float -> U.Vector Float
diffuseFieldGridWeather gridWidth gridHeight iterations factor field =
  iterateN iterations field
  where
    iterateN passCount current
      | passCount <= 0 = current
      | otherwise = iterateN (passCount - 1) (diffuseOnce current)
    diffuseOnce vec = U.generate (gridWidth * gridHeight) (diffuseAt vec)
    diffuseAt vec idx =
      let center = vec U.! idx
          neighborSum = foldHexNeighbors gridWidth gridHeight idx (\acc j -> acc + vec U.! j) 0
          neighborCount = countHexNeighbors gridWidth gridHeight idx
          avg = (center + neighborSum) / fromIntegral (1 + neighborCount)
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

-- | Approximate the local pressure gradient from the in-bounds hex
-- neighbours of a tile.
pressureGradientAt :: Int -> Int -> U.Vector Float -> Int -> (Float, Float)
pressureGradientAt gridWidth gridHeight pressure idx =
  let center = pressure U.! idx
      accumulate (dxAcc, dyAcc) dir =
        case hexNeighborIndexInDirection gridWidth gridHeight dir idx of
          Nothing -> (dxAcc, dyAcc)
          Just neighborIdx ->
            let neighborPressure = pressure U.! neighborIdx
                delta = neighborPressure - center
                (dirX, dirY) = directionVector dir
            in (dxAcc + delta * dirX, dyAcc + delta * dirY)
      (sumDx, sumDy) = foldHexDirections accumulate (0, 0)
      neighborCount = max 1 (countHexNeighbors gridWidth gridHeight idx)
      dPressureDx = sumDx / fromIntegral neighborCount
      dPressureDy = sumDy / fromIntegral neighborCount
  in (dPressureDx, dPressureDy)

-- | Turn the local hex-derived pressure gradient into a wind direction by
-- steering toward lower pressure.
windDirectionFromPressure :: Int -> Int -> U.Vector Float -> Float -> Float -> Int -> Float
windDirectionFromPressure gridWidth gridHeight pressure previousDirection response idx =
  let (dPressureDx, dPressureDy) = pressureGradientAt gridWidth gridHeight pressure idx
      gradientDirection = atan2 (negate dPressureDy) (negate dPressureDx)
  in lerpAngle previousDirection gradientDirection response

-- | Scale wind speed by the magnitude of the local hex-derived pressure
-- gradient.
windSpeedFromPressure :: Int -> Int -> U.Vector Float -> Float -> Float -> Int -> Float
windSpeedFromPressure gridWidth gridHeight pressure previousSpeed gradientScale idx =
  let (dPressureDx, dPressureDy) = pressureGradientAt gridWidth gridHeight pressure idx
      gradientMagnitude = sqrt (dPressureDx * dPressureDx + dPressureDy * dPressureDy)
  in previousSpeed * (1 + gradientScale * gradientMagnitude)