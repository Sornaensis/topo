{-# LANGUAGE ScopedTypeVariables #-}

module Spec.TerrainFormMetrics (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Data.Vector.Unboxed as U
import Topo.Parameters
  ( classifyTerrainForm
  , defaultTerrainFormConfig
  )
import Topo.TerrainForm.Metrics
  ( TerrainNeighborhood(..)
  , terrainNeighborhoodAt
  )
import Topo.TerrainGrid (classifyTerrainFormGrid, clampCoordGrid)
import Topo.Types (zeroDirSlope)

spec :: Spec
spec = describe "TerrainForm.Metrics" $ do
  it "produces zero slope/relief on a 1x1 grid" $ do
    let w = 1
        h = 1
        elev = U.singleton (0.42 :: Float)
        elevAt gx gy =
          let cx = clampCoordGrid w gx
              cy = clampCoordGrid h gy
          in elev U.! (cy * w + cx)
        metrics = terrainNeighborhoodAt elevAt 0 0
    tnDirectionalSlope metrics `shouldBe` zeroDirSlope
    tnRelief metrics `shouldBe` 0
    tnRelief2Ring metrics `shouldBe` 0
    tnRelief3Ring metrics `shouldBe` 0
    tnRuggedness metrics `shouldBe` 0
    tnIsLocalMinimum metrics `shouldBe` True

  it "classifyTerrainFormGrid matches per-tile metric classification" $ do
    let w = 9
        h = 7
        n = w * h
        formCfg = defaultTerrainFormConfig
        waterLevel = 0.5
        elev = U.generate n $ \i ->
          let x = i `mod` w
              y = i `div` w
              fx = fromIntegral x / fromIntegral (w - 1)
              fy = fromIntegral y / fromIntegral (h - 1)
          in min 1 (max 0 (0.2 + 0.6 * fx + 0.2 * fy + 0.12 * sin (fromIntegral (x * 3 + y * 5))))
        hardness = U.generate n $ \i ->
          let x = i `mod` w
              y = i `div` w
          in min 1 (max 0 (0.4 + 0.2 * sin (fromIntegral (x * 7 + y * 11))))
        elevAt gx gy =
          let cx = clampCoordGrid w gx
              cy = clampCoordGrid h gy
          in elev U.! (cy * w + cx)
        expected = U.generate n $ \i ->
          let x = i `mod` w
              y = i `div` w
              metrics = terrainNeighborhoodAt elevAt x y
              hard = hardness U.! i
              elevASL = tnElevation metrics - waterLevel
          in classifyTerrainForm
               formCfg
               (tnDirectionalSlope metrics)
               (tnRelief metrics)
               (tnRelief2Ring metrics)
               (tnRelief3Ring metrics)
               (tnCurvature metrics)
               (tnIsLocalMinimum metrics)
               hard
               0.5
               elevASL
        actual = classifyTerrainFormGrid formCfg waterLevel w h elev hardness
    actual `shouldBe` expected

  prop "ring relief is monotone: r1 <= r2 <= r3" $ \(seed :: Int) ->
    let w = 8
        h = 8
        n = w * h
        elev = U.generate n $ \i ->
          let x = i `mod` w
              y = i `div` w
              raw = fromIntegral ((seed * 31 + x * 17 + y * 13) `mod` 1000) / 1000
          in realToFrac raw
        elevAt gx gy =
          let cx = clampCoordGrid w gx
              cy = clampCoordGrid h gy
          in elev U.! (cy * w + cx)
        ok i =
          let x = i `mod` w
              y = i `div` w
              metrics = terrainNeighborhoodAt elevAt x y
              r1 = tnRelief metrics
              r2 = tnRelief2Ring metrics
              r3 = tnRelief3Ring metrics
          in r1 <= r2 && r2 <= r3 && r1 >= 0 && r2 >= 0 && r3 >= 0
    in all ok [0 .. n - 1]
