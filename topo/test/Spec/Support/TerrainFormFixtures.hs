module Spec.Support.TerrainFormFixtures
  ( classifyFixture
  , classifyFixtureWith
  , shieldPlateauElev
  , canyonElev
  , glaciatedHighlandElev
  , coastalEscarpmentElev
  , badlandsElev
  , badlandsHardness
  , midSlopeReliefElev
  , midSlopeReliefHardness
  ) where

import qualified Data.Vector.Unboxed as U
import Topo.Parameters (TerrainFormConfig, defaultTerrainFormConfig)
import Topo.TerrainGrid (classifyTerrainFormGrid)
import Topo.Types (TerrainForm)

gridW, gridH :: Int
gridW = 48
gridH = 48

classifyFixture :: (Int -> Int -> Float) -> (Int -> Int -> Float) -> U.Vector TerrainForm
classifyFixture elevFn hardFn =
  classifyFixtureWith defaultTerrainFormConfig elevFn hardFn

classifyFixtureWith
  :: TerrainFormConfig
  -> (Int -> Int -> Float)
  -> (Int -> Int -> Float)
  -> U.Vector TerrainForm
classifyFixtureWith cfg elevFn hardFn =
  let n = gridW * gridH
      elev = U.generate n $ \i ->
        let x = i `mod` gridW
            y = i `div` gridW
        in clamp01 (elevFn x y)
      hardness = U.generate n $ \i ->
        let x = i `mod` gridW
            y = i `div` gridW
        in clamp01 (hardFn x y)
  in classifyTerrainFormGrid cfg 0.5 gridW gridH elev hardness

shieldPlateauElev :: Int -> Int -> Float
shieldPlateauElev x y =
  let xf = norm x gridW
      yf = norm y gridH
      dx = xf - 0.5
      dy = yf - 0.5
      r2 = dx * dx + dy * dy
      dome = 0.72 - 0.22 * r2
      gentle = 0.01 * sin (6 * xf) * cos (5 * yf)
  in dome + gentle

canyonElev :: Int -> Int -> Float
canyonElev x y =
  let xf = norm x gridW
      yf = norm y gridH
      axis = abs (xf - 0.5)
      ridge = exp (negate (axis * axis) / 0.0018)
      longWave = 0.02 * sin (8 * yf)
  in 0.52 + longWave + 0.22 * ridge

glaciatedHighlandElev :: Int -> Int -> Float
glaciatedHighlandElev x y =
  let xf = norm x gridW
      yf = norm y gridH
      ridgeA = 0.08 * sin (18 * xf + 3 * yf)
      ridgeB = 0.07 * cos (16 * yf - 2 * xf)
      massif = 0.58 + 0.10 * sin (4 * xf) * sin (4 * yf)
  in massif + ridgeA + ridgeB

coastalEscarpmentElev :: Int -> Int -> Float
coastalEscarpmentElev x y =
  let xf = norm x gridW
      yf = norm y gridH
      cliff = 0.50 / (1 + exp (-120 * (xf - 0.42)))
      coastUndulate = 0.03 * sin (12 * yf)
  in 0.22 + cliff + coastUndulate

badlandsElev :: Int -> Int -> Float
badlandsElev x y =
  let xf = norm x gridW
      yf = norm y gridH
      stepBand = if xf < 0.5 then 0.0 else 0.18
      rills  = 0.012 * sin (14 * yf)
      bands  = 0.008 * sin (22 * xf + 3 * yf)
  in 0.36 + stepBand + rills + bands

badlandsHardness :: Int -> Int -> Float
badlandsHardness x y =
  let xf = norm x gridW
      yf = norm y gridH
  in 0.18 + 0.02 * sin (6 * xf + 5 * yf)

midSlopeReliefElev :: Int -> Int -> Float
midSlopeReliefElev x y =
  let xf = norm x gridW
      yf = norm y gridH
      broadSlope = 0.60 * xf
      lowFreqRelief = 0.018 * sin (6 * xf) * cos (5 * yf)
      fineRelief = 0.010 * sin (18 * xf + 7 * yf)
      diagonalRidge = 0.008 * sin (9 * (xf + yf))
  in 0.16 + broadSlope + lowFreqRelief + fineRelief + diagonalRidge

midSlopeReliefHardness :: Int -> Int -> Float
midSlopeReliefHardness x y =
  let xf = norm x gridW
      yf = norm y gridH
      base = 0.60
      undulate = 0.05 * sin (4 * xf + 3 * yf)
      pockets = 0.03 * cos (9 * xf - 6 * yf)
  in base + undulate + pockets

norm :: Int -> Int -> Float
norm idx maxN
  | maxN <= 1 = 0
  | otherwise = fromIntegral idx / fromIntegral (maxN - 1)

clamp01 :: Float -> Float
clamp01 v
  | v < 0 = 0
  | v > 1 = 1
  | otherwise = v