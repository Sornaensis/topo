{-# LANGUAGE ScopedTypeVariables #-}

module Spec.WaterTable (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Topo.WaterTable
  ( WaterTableConfig(..)
  , defaultWaterTableConfig
  , infiltrationCapacity
  , waterTableDiffusion
  , rootZoneMoisture
  , improvedFertility
  )
import qualified Data.Vector.Unboxed as U

spec :: Spec
spec = describe "WaterTable" $ do

  ---------------------------------------------------------------------------
  -- infiltrationCapacity
  ---------------------------------------------------------------------------

  describe "infiltrationCapacity" $ do
    let cfg = defaultWaterTableConfig

    it "returns low values for rocky soil (type 1)" $ do
      let rocky  = infiltrationCapacity cfg 0.5 0.0 1.0 1
          normal = infiltrationCapacity cfg 0.5 0.0 1.0 0
      rocky `shouldSatisfy` (< normal)
      rocky `shouldSatisfy` (< 0.4)

    it "returns low values for steep slopes" $ do
      let steep = infiltrationCapacity cfg 0.5 0.8 1.0 0
          flat  = infiltrationCapacity cfg 0.5 0.0 1.0 0
      steep `shouldSatisfy` (< flat)

    it "peaks at medium grain size" $ do
      let low   = infiltrationCapacity cfg 0.1 0.0 1.0 0
          mid   = infiltrationCapacity cfg 0.5 0.0 1.0 0
          high  = infiltrationCapacity cfg 0.9 0.0 1.0 0
      mid `shouldSatisfy` (> low)
      mid `shouldSatisfy` (> high)

    it "returns 0 for zero soil depth" $
      infiltrationCapacity cfg 0.5 0.0 0.0 0 `shouldBe` 0.0

    it "returns higher values for wet soil (type 2) than normal" $ do
      let wet    = infiltrationCapacity cfg 0.5 0.0 1.0 2
          normal = infiltrationCapacity cfg 0.5 0.0 1.0 0
      wet `shouldSatisfy` (>= normal)

    prop "output is always in [0, 1]" $
      \(grain :: Float) (slope :: Float) (depth :: Float) ->
        let g = abs grain `mod'` 1.0
            s = abs slope `mod'` 1.0
            d = abs depth `mod'` 1.0
            v = infiltrationCapacity cfg g s d 0
        in v >= 0 && v <= 1

    prop "slope decreases infiltration" $
      \(g :: Float) (d :: Float) ->
        let g' = abs g `mod'` 1.0
            d' = max 0.01 (abs d `mod'` 1.0)
            flat  = infiltrationCapacity cfg g' 0.0 d' 0
            steep = infiltrationCapacity cfg g' 0.8 d' 0
        in steep <= flat + 1e-6

  ---------------------------------------------------------------------------
  -- waterTableDiffusion
  ---------------------------------------------------------------------------

  describe "waterTableDiffusion" $ do
    let cfg = defaultWaterTableConfig

    it "produces shallower tables in valleys than ridges" $ do
      -- 3×3 grid: center is a valley (low elevation), edges are ridges.
      -- Precipitation is zero so only diffusion shapes the result;
      -- non-zero recharge would saturate all tiles to their surface.
      let gridW = 3
          gridH = 3
          -- Ridge-valley-ridge pattern
          elev = U.fromList
            [ 0.9, 0.9, 0.9
            , 0.9, 0.3, 0.9
            , 0.9, 0.9, 0.9
            ]
          infilt = U.replicate 9 0.5
          precip = U.replicate 9 0.0
          depth  = waterTableDiffusion cfg gridW gridH elev infilt precip
          valleyDepth = depth U.! 4           -- center tile
          ridgeDepth  = depth U.! 0           -- corner tile
      valleyDepth `shouldSatisfy` (< ridgeDepth)

    prop "output is always in [0, 1]" $ do
      -- Small 2×2 grid with arbitrary values
      let cfg' = defaultWaterTableConfig { wtcDiffusionIterations = 3 }
      forAll (vectorOf 4 (choose (0, 1 :: Float))) $ \es ->
        forAll (vectorOf 4 (choose (0, 1 :: Float))) $ \is' ->
          forAll (vectorOf 4 (choose (0, 1 :: Float))) $ \ps ->
            let elev   = U.fromList es
                infilt = U.fromList is'
                precip = U.fromList ps
                depth  = waterTableDiffusion cfg' 2 2 elev infilt precip
            in U.all (\v -> v >= 0 && v <= 1) depth

  ---------------------------------------------------------------------------
  -- rootZoneMoisture
  ---------------------------------------------------------------------------

  describe "rootZoneMoisture" $ do
    let cfg = defaultWaterTableConfig

    it "is higher for tiles with shallow water tables" $ do
      let shallow = rootZoneMoisture cfg 0.01 0.5 0.5
          deep    = rootZoneMoisture cfg 0.90 0.5 0.5
      shallow `shouldSatisfy` (> deep)

    it "increases with surface moisture" $ do
      let low  = rootZoneMoisture cfg 0.5 0.1 0.5
          high = rootZoneMoisture cfg 0.5 0.9 0.5
      high `shouldSatisfy` (> low)

    it "fine grain increases capillary contribution" $ do
      let fine   = rootZoneMoisture cfg 0.3 0.0 0.1  -- fine grain, low surface
          coarse = rootZoneMoisture cfg 0.3 0.0 0.9  -- coarse grain, low surface
      fine `shouldSatisfy` (>= coarse)

    prop "output is always in [0, 1]" $
      \(wt :: Float) (sm :: Float) (g :: Float) ->
        let wt' = abs wt `mod'` 1.0
            sm' = abs sm `mod'` 1.0
            g'  = abs g  `mod'` 1.0
            v = rootZoneMoisture cfg wt' sm' g'
        in v >= 0 && v <= 1

  ---------------------------------------------------------------------------
  -- improvedFertility
  ---------------------------------------------------------------------------

  describe "improvedFertility" $ do
    let cfg = defaultWaterTableConfig

    it "is lower for rocky steep terrain than alluvial plains" $ do
      let rocky    = improvedFertility cfg 0.04 0.2 0.9
          alluvial = improvedFertility cfg 0.72 0.8 0.4
      alluvial `shouldSatisfy` (> rocky)

    it "returns 0 for zero inputs" $
      improvedFertility cfg 0.0 0.0 0.0 `shouldBe` 0.0

    it "increases with root-zone moisture" $ do
      let low  = improvedFertility cfg 0.1 0.5 0.5
          high = improvedFertility cfg 0.9 0.5 0.5
      high `shouldSatisfy` (> low)

    it "increases with soil depth" $ do
      let shallow = improvedFertility cfg 0.5 0.1 0.5
          deep    = improvedFertility cfg 0.5 0.9 0.5
      deep `shouldSatisfy` (> shallow)

    prop "output is always in [0, 1]" $
      \(rz :: Float) (d :: Float) (g :: Float) ->
        let rz' = abs rz `mod'` 1.0
            d'  = abs d  `mod'` 1.0
            g'  = abs g  `mod'` 1.0
            v = improvedFertility cfg rz' d' g'
        in v >= 0 && v <= 1

  ---------------------------------------------------------------------------
  -- defaultWaterTableConfig
  ---------------------------------------------------------------------------

  describe "defaultWaterTableConfig" $ do
    it "has sensible defaults" $ do
      let cfg = defaultWaterTableConfig
      wtcGrainPermeabilityCenter cfg `shouldSatisfy` (> 0)
      wtcGrainPermeabilityWidth cfg `shouldSatisfy` (> 0)
      wtcSlopeInfiltrationMax cfg `shouldSatisfy` (> 0)
      wtcDiffusionIterations cfg `shouldSatisfy` (> 0)

    it "root-zone weights sum to ~1" $ do
      let cfg = defaultWaterTableConfig
          s = wtcWaterTableWeight cfg + wtcSurfaceMoistureWeight cfg
              + wtcCapillaryWeight cfg
      abs (s - 1.0) `shouldSatisfy` (< 0.01)

    it "fertility weights sum to ~1" $ do
      let cfg = defaultWaterTableConfig
          s = wtcFertilityMoistWeight cfg + wtcFertilityDepthWeight cfg
              + wtcFertilityOrganicWeight cfg
      abs (s - 1.0) `shouldSatisfy` (< 0.01)

-- | Safe floating-point modulo for keeping values in [0, 1].
mod' :: Float -> Float -> Float
mod' x m
  | m == 0    = 0
  | otherwise = let r = x - m * fromIntegral (floor (x / m) :: Int)
                in if r < 0 then r + m else r
