module Spec.OceanCurrent (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Topo.OceanCurrent

spec :: Spec
spec = describe "OceanCurrent" $ do
  let cfg = defaultOceanCurrentConfig

  describe "oceanCurrentOffset" $ do
    it "is 0 for non-coastal ocean tiles" $ do
      -- No land in either direction -> no current effect
      oceanCurrentOffset cfg 0.5 False False `shouldBe` 0.0

    it "warms western boundary (land to the east)" $ do
      let latRad = 35 * (pi / 180)  -- peak latitude
          offset = oceanCurrentOffset cfg latRad True False
      offset `shouldSatisfy` (> 0)

    it "cools eastern boundary (land to the west)" $ do
      let latRad = 35 * (pi / 180)
          offset = oceanCurrentOffset cfg latRad False True
      offset `shouldSatisfy` (< 0)

    it "cancels in narrow straits (land on both sides)" $ do
      let latRad = 35 * (pi / 180)
          offset = oceanCurrentOffset cfg latRad True True
      -- Warm and cold partially cancel
      abs offset `shouldSatisfy` (< occWarmScale cfg)

    it "peaks at the configured latitude" $ do
      let atPeak = abs (oceanCurrentOffset cfg (35 * pi / 180) True False)
          atEquator = abs (oceanCurrentOffset cfg 0 True False)
          atPole = abs (oceanCurrentOffset cfg (85 * pi / 180) True False)
      atPeak `shouldSatisfy` (> atEquator)
      atPeak `shouldSatisfy` (> atPole)

    prop "offset magnitude never exceeds max(warmScale, coldScale)" $
      forAll (choose (-90 :: Float, 90)) $ \latDeg ->
        forAll (elements [(True, False), (False, True), (True, True), (False, False)]) $ \(le, lw) ->
          let latRad = latDeg * (pi / 180)
              offset = oceanCurrentOffset cfg latRad le lw
              maxOffset = max (occWarmScale cfg) (occColdScale cfg)
          in abs offset <= maxOffset + 0.001

  describe "defaultOceanCurrentConfig" $ do
    it "warm scale is positive" $
      occWarmScale defaultOceanCurrentConfig `shouldSatisfy` (> 0)

    it "cold scale is positive" $
      occColdScale defaultOceanCurrentConfig `shouldSatisfy` (> 0)

    it "lat peak is at mid-latitudes" $ do
      occLatPeakDeg defaultOceanCurrentConfig `shouldSatisfy` (> 20)
      occLatPeakDeg defaultOceanCurrentConfig `shouldSatisfy` (< 50)
