{-# LANGUAGE ScopedTypeVariables #-}

module Spec.Noise (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.Word (Word64)
import Topo.Noise (noise2DContinuous, ridgedFbm2D)

spec :: Spec
spec = describe "Noise" $ do
  it "continuous noise is deterministic for the same inputs" $ do
    let seed = 1337
        samples = [(-3.25, -7.5), (0, 0), (5.75, 9.125), (42, -11.5), (128.5, 256.25)]
        values = map (uncurry (noise2DContinuous seed)) samples
    values `shouldBe` map (uncurry (noise2DContinuous seed)) samples
  prop "continuous noise stays within [0,1]" $ \(seed :: Word64) ->
    forAll (choose (-1000, 1000)) $ \x ->
      forAll (choose (-1000, 1000)) $ \y ->
        let v = noise2DContinuous seed x y
        in v >= 0 && v <= 1

  prop "ridged FBM is non-negative" $ \(seed :: Word64) ->
    forAll (choose (-100, 100)) $ \x ->
      forAll (choose (-100, 100)) $ \y ->
        let v = ridgedFbm2D seed 5 2.0 0.5 x y
        in v >= 0
