{-# LANGUAGE ScopedTypeVariables #-}

module Spec.Soil (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Topo.Soil
  ( SoilConfig(..)
  , defaultSoilConfig
  , fertilityFromMoistureDepth
  , soilDepthFromHardness
  , soilTypeFromMoistureHardness
  )

spec :: Spec
spec = describe "Soil" $ do

  describe "soilTypeFromMoistureHardness" $ do
    it "returns 2 (wet) when moisture exceeds threshold" $ do
      let cfg = defaultSoilConfig
      soilTypeFromMoistureHardness cfg 0.9 0.1 `shouldBe` 2

    it "returns 1 (rocky) when hardness exceeds threshold and moisture is low" $ do
      let cfg = defaultSoilConfig
      soilTypeFromMoistureHardness cfg 0.1 0.9 `shouldBe` 1

    it "returns 0 (normal) when both are below thresholds" $ do
      let cfg = defaultSoilConfig
      soilTypeFromMoistureHardness cfg 0.3 0.2 `shouldBe` 0

    it "prefers wet over rocky when both exceed thresholds" $ do
      let cfg = defaultSoilConfig
      soilTypeFromMoistureHardness cfg 0.9 0.9 `shouldBe` 2

  describe "soilDepthFromHardness" $ do
    it "yields deeper soil for softer rock" $ do
      let soft = soilDepthFromHardness 0.1
          hard = soilDepthFromHardness 0.9
      soft `shouldSatisfy` (> hard)

    it "returns 1.0 for zero hardness" $
      soilDepthFromHardness 0.0 `shouldBe` 1.0

    it "returns 0.0 for max hardness" $
      soilDepthFromHardness 1.0 `shouldBe` 0.0

    prop "clamps to [0, 1]" $
      \(NonNegative (h :: Float)) ->
        let depth = soilDepthFromHardness h
        in depth >= 0 && depth <= 1

  describe "fertilityFromMoistureDepth" $ do
    it "increases with moisture and depth" $ do
      let cfg = defaultSoilConfig
          low  = fertilityFromMoistureDepth cfg 0.1 0.1
          high = fertilityFromMoistureDepth cfg 0.9 0.9
      high `shouldSatisfy` (> low)

    it "returns 0 for zero moisture and zero depth" $ do
      let cfg = defaultSoilConfig
      fertilityFromMoistureDepth cfg 0.0 0.0 `shouldBe` 0.0

    prop "result is in [0, 1]" $
      \(NonNegative (m :: Float)) (NonNegative (d :: Float)) ->
        let cfg = defaultSoilConfig
            m' = min 1 m
            d' = min 1 d
            f = fertilityFromMoistureDepth cfg m' d'
        in f >= 0 && f <= 1

  describe "defaultSoilConfig" $ do
    it "has sensible defaults" $ do
      let cfg = defaultSoilConfig
      scMoistureThreshold cfg `shouldSatisfy` (> 0)
      scHardnessThreshold cfg `shouldSatisfy` (> 0)
      scFertilityMoistWeight cfg `shouldSatisfy` (> 0)
      scFertilityDepthWeight cfg `shouldSatisfy` (> 0)
      -- Weights should sum to 1
      (scFertilityMoistWeight cfg + scFertilityDepthWeight cfg) `shouldBe` 1.0
