{-# LANGUAGE ScopedTypeVariables #-}

module Spec.Evaporation (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Topo.Climate.Config (MoistureConfig(..), defaultMoistureConfig)
import Topo.Climate.Evaporation

---------------------------------------------------------------------------
-- Generators
---------------------------------------------------------------------------

-- | Normalised @[0,1]@ float suitable for temperature, moisture, etc.
unit :: Gen Float
unit = choose (0.0, 1.0)

---------------------------------------------------------------------------
-- satNorm (Model A)
---------------------------------------------------------------------------

satNormSpec :: Spec
satNormSpec = describe "satNorm" $ do
  it "satNorm(0) ≈ 0 (polar)" $
    satNorm 0.0 `shouldSatisfy` (< 0.01)

  it "satNorm(1) ≈ 1 (equatorial)" $
    satNorm 1.0 `shouldSatisfy` (> 0.99)

  it "satNorm(0.43) ≈ 0.083 (freezing point)" $
    abs (satNorm 0.43 - 0.083) `shouldSatisfy` (< 0.03)

  it "satNorm(0.50) ≈ 0.112 (cool temperate)" $
    abs (satNorm 0.50 - 0.112) `shouldSatisfy` (< 0.03)

  it "satNorm(0.80) ≈ 0.456 (subtropical)" $
    abs (satNorm 0.80 - 0.456) `shouldSatisfy` (< 0.03)

  prop "monotonically increasing" $
    \(a' :: Float) (b' :: Float) ->
      let a = min 1 (max 0 a')
          b = min 1 (max 0 b')
      in a <= b ==> satNorm a <= satNorm b

  prop "output in [0,1]" $
    forAll unit $ \t ->
      let s = satNorm t
      in s >= 0 && s <= 1

  prop "clamped inputs in [0,1]" $
    \(t :: Float) ->
      let s = satNorm t
      in s >= 0 && s <= 1

---------------------------------------------------------------------------
-- oceanEvaporation (Model B)
---------------------------------------------------------------------------

oceanEvaporationSpec :: Spec
oceanEvaporationSpec = describe "oceanEvaporation" $ do
  let cfg = defaultMoistureConfig

  it "tropical >> polar" $ do
    let tropical = oceanEvaporation cfg 0.85 0.5 1.0
        polar    = oceanEvaporation cfg 0.15 0.5 1.0
    tropical `shouldSatisfy` (> polar * 5)

  it "zero temperature -> near-zero evaporation" $
    oceanEvaporation cfg 0.0 0.5 1.0 `shouldSatisfy` (< 0.05)

  it "wind enhances evaporation" $ do
    let calm  = oceanEvaporation cfg 0.7 0.0 1.0
        windy = oceanEvaporation cfg 0.7 1.0 1.0
    windy `shouldSatisfy` (> calm)

  it "insolation scales output" $ do
    let bright = oceanEvaporation cfg 0.7 0.5 1.5
        dim    = oceanEvaporation cfg 0.7 0.5 0.5
    bright `shouldSatisfy` (> dim)

  prop "output in [0,1]" $
    forAll ((,,) <$> unit <*> unit <*> choose (0.5, 2.0)) $
      \(t, w, insol) ->
        let e = oceanEvaporation cfg t w insol
        in e >= 0 && e <= 1

---------------------------------------------------------------------------
-- landEvapotranspiration (Model C)
---------------------------------------------------------------------------

landETSpec :: Spec
landETSpec = describe "landEvapotranspiration" $ do
  let cfg = defaultMoistureConfig

  it "zero soil moisture -> zero ET" $
    landEvapotranspiration cfg 0.8 0.0 0.9 0.5 `shouldSatisfy` (< 0.001)

  it "scales with vegetation cover" $ do
    let bare      = landEvapotranspiration cfg 0.7 0.6 0.0 0.3
        vegetated = landEvapotranspiration cfg 0.7 0.6 0.9 0.3
    vegetated `shouldSatisfy` (> bare * 2)

  it "cold limiting: low temp -> low ET regardless of moisture" $
    landEvapotranspiration cfg 0.05 0.9 0.9 0.5 `shouldSatisfy` (< 0.05)

  it "hot dry desert: low soil moisture + no vegetation -> near-zero" $
    landEvapotranspiration cfg 0.9 0.05 0.0 0.3 `shouldSatisfy` (< 0.01)

  it "Amazon-like: hot + wet + vegetated -> high ET" $
    landEvapotranspiration cfg 0.85 0.8 0.9 0.3 `shouldSatisfy` (> 0.20)

  prop "output in [0,1]" $
    forAll ((,,,) <$> unit <*> unit <*> unit <*> unit) $
      \(t, m, v, w) ->
        let e = landEvapotranspiration cfg t m v w
        in e >= 0 && e <= 1

  prop "increases with soil moisture (temperature > 0.2)" $
    forAll ((,,,,) <$> choose (0.2, 1.0) <*> unit <*> unit <*> unit <*> unit) $
      \(t, m1', m2', v, w) ->
        let m1 = min m1' m2'
            m2 = max m1' m2'
            e1 = landEvapotranspiration cfg t m1 v w
            e2 = landEvapotranspiration cfg t m2 v w
        in e1 <= e2

---------------------------------------------------------------------------
-- Default config sanity
---------------------------------------------------------------------------

configSpec :: Spec
configSpec = describe "defaultMoistureConfig" $ do
  it "bare + veg fractions sum to 1" $
    let cfg = defaultMoistureConfig
    in abs (moistBareEvapFrac cfg + moistVegTranspFrac cfg - 1.0) `shouldSatisfy` (< 0.001)

  it "CC scale and offset are Earth-like" $ do
    let cfg = defaultMoistureConfig
    ccTempToC_Scale cfg `shouldBe` 70.0
    ccTempToC_Offset cfg `shouldBe` (-30.0)

---------------------------------------------------------------------------
-- Entry point
---------------------------------------------------------------------------

spec :: Spec
spec = describe "Evaporation" $ do
  satNormSpec
  oceanEvaporationSpec
  landETSpec
  configSpec
