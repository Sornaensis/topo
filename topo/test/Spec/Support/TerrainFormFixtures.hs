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
  , graniteIntrusionElev
  , graniteIntrusionHardness
  , hardnessGradientElev
  , hardnessGradientHardness
  , undulatingPlainsElev
  , undulatingPlainsHardness
  , dissectedFoothillsElev
  , dissectedFoothillsHardness
  , ruggedUplandsElev
  , ruggedUplandsHardness
  , badlandsGradientElev
  , badlandsGradientHardness
  , gridW
  , gridH
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

-- | Granite intrusions in sandstone: patchy high/low hardness at moderate
-- elevation with undulating terrain near rolling/hilly thresholds. Tests
-- classifier behavior under heterogeneous hardness where soft-attenuation
-- applies to some tiles and not others.
graniteIntrusionElev :: Int -> Int -> Float
graniteIntrusionElev x y =
  let xf = norm x gridW
      yf = norm y gridH
      base = 0.42
      -- High-frequency undulations producing slopes near rolling/hilly boundary
      broadUndulation = 0.08 * sin (12 * xf) * cos (10 * yf)
      localRelief = 0.05 * sin (20 * xf + 14 * yf)
      pocket = 0.03 * cos (18 * xf - 12 * yf)
  in base + broadUndulation + localRelief + pocket

-- | Patchy hardness: alternating granite (hard) and sandstone (soft) patches.
graniteIntrusionHardness :: Int -> Int -> Float
graniteIntrusionHardness x y =
  let xf = norm x gridW
      yf = norm y gridH
      -- Sharp-ish patches via squared sines
      patchX = sin (7 * xf + 2 * yf)
      patchY = cos (5 * yf - 3 * xf)
      patch = patchX * patchX * patchY * patchY
      -- Range: granite ~0.75, sandstone ~0.20
  in 0.20 + 0.55 * patch

-- | Hardness gradient across elevation band: undulating terrain near the
-- rolling/hilly boundary with soft lowlands grading into hard highlands.
-- Tests that soft-attenuation correctly dampens micro-relief promotion
-- in the soft-rock west while allowing it in the hard-rock east.
hardnessGradientElev :: Int -> Int -> Float
hardnessGradientElev x y =
  let xf = norm x gridW
      yf = norm y gridH
      -- Moderate base with undulations producing slopes near rolling threshold
      base = 0.35 + 0.15 * xf
      ridge = 0.06 * sin (14 * yf + 6 * xf)
      spur = 0.04 * sin (20 * xf + 10 * yf)
      cross = 0.03 * cos (16 * xf - 8 * yf)
  in base + ridge + spur + cross

-- | Hardness increases with x-coordinate: soft sedimentary in the west,
-- hard crystalline in the east.
hardnessGradientHardness :: Int -> Int -> Float
hardnessGradientHardness x _y =
  let xf = norm x gridW
  in 0.15 + 0.65 * xf

-- | Low-relief undulating plains: gentle multi-scale sine terrain
-- just above sea level. Produces mostly Flat with some Rolling near
-- slope maxima where multi-directional undulations constructively
-- interfere.
undulatingPlainsElev :: Int -> Int -> Float
undulatingPlainsElev x y =
  let xf = norm x gridW
      yf = norm y gridH
      base = 0.53
      broad = 0.04 * sin (8 * xf) * cos (6 * yf)
      cross = 0.025 * sin (12 * xf + 5 * yf)
  in base + broad + cross

-- | Uniform moderate hardness for undulating plains.
undulatingPlainsHardness :: Int -> Int -> Float
undulatingPlainsHardness _ _ = 0.50

-- | Dissected foothills: moderate elevation with multi-directional
-- ridges creating slopes in the foothill-to-hilly range. Elevation
-- spans the foothill ASL band at the mean but extends above and below
-- at ridge crests and incision troughs.
dissectedFoothillsElev :: Int -> Int -> Float
dissectedFoothillsElev x y =
  let xf = norm x gridW
      yf = norm y gridH
      base = 0.56
      ridge1 = 0.07 * sin (12 * xf + 4 * yf)
      ridge2 = 0.05 * cos (9 * yf - 3 * xf)
      incision = 0.04 * sin (16 * xf + 8 * yf)
  in base + ridge1 + ridge2 + incision

-- | Uniform moderate hardness for dissected foothills.
dissectedFoothillsHardness :: Int -> Int -> Float
dissectedFoothillsHardness _ _ = 0.55

-- | Hard-rock rugged uplands: high elevation with high-amplitude
-- multi-scale ridges and high hardness. Produces predominantly
-- Mountainous and Hilly forms via both slope and relief paths.
ruggedUplandsElev :: Int -> Int -> Float
ruggedUplandsElev x y =
  let xf = norm x gridW
      yf = norm y gridH
      base = 0.72
      ridge1 = 0.10 * sin (14 * xf + 5 * yf)
      ridge2 = 0.08 * cos (10 * yf - 4 * xf)
      cut = 0.05 * sin (22 * xf) * cos (18 * yf)
  in base + ridge1 + ridge2 + cut

-- | High hardness (crystalline rock) for rugged uplands.
ruggedUplandsHardness :: Int -> Int -> Float
ruggedUplandsHardness _ _ = 0.78

-- | Soft-rock eroded badlands gradient: terraced steps creating steep
-- slopes at step boundaries, with hardness increasing from west to
-- east. The soft west should produce badlands at step edges while the
-- harder east classifies as mountainous or hilly.
badlandsGradientElev :: Int -> Int -> Float
badlandsGradientElev x y =
  let yf = norm y gridH
      xf = norm x gridW
      -- 4 terraces of 12 tiles each with 0.16 elevation steps.
      -- Step raw slope = 0.16, physical = 0.092 (> badlandsMinMaxSlope 0.085).
      terrace = fromIntegral (x `div` 12) * 0.16
      rills = 0.012 * sin (14 * yf)
      bands = 0.008 * sin (22 * yf + 3 * xf)
  in 0.32 + terrace + rills + bands

-- | Hardness gradient for badlands: very soft sedimentary rock in the
-- west (0.12) grading to moderate (0.42) in the east. Badlands
-- threshold (0.35) crossed at approximately x = 36.
badlandsGradientHardness :: Int -> Int -> Float
badlandsGradientHardness x _y =
  let xf = norm x gridW
  in 0.12 + 0.30 * xf

norm :: Int -> Int -> Float
norm idx maxN
  | maxN <= 1 = 0
  | otherwise = fromIntegral idx / fromIntegral (maxN - 1)

clamp01 :: Float -> Float
clamp01 v
  | v < 0 = 0
  | v > 1 = 1
  | otherwise = v