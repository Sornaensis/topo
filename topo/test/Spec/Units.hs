module Spec.Units (spec) where

import Test.Hspec
import Test.QuickCheck
import Topo.Units

-- | Tolerance for floating-point comparisons.
epsilon :: Float
epsilon = 1e-3

-- | Helper: check approximate equality.
approxEq :: Float -> Float -> Float -> Bool
approxEq tol expected actual = abs (actual - expected) < tol

spec :: Spec
spec = describe "Units" $ do

  -- -----------------------------------------------------------------------
  -- Known-value unit tests
  -- -----------------------------------------------------------------------

  describe "normToC / cToNorm" $ do
    let s = defaultUnitScales

    it "norm 0.0 → −30 °C" $
      normToC s 0.0 `shouldSatisfy` approxEq epsilon (-30.0)

    it "norm ~0.4286 → ≈0 °C" $
      normToC s (30.0 / 70.0) `shouldSatisfy` approxEq 0.01 0.0

    it "norm 1.0 → +40 °C" $
      normToC s 1.0 `shouldSatisfy` approxEq epsilon 40.0

    it "norm 0.78 → ≈24.6 °C (equator)" $
      normToC s 0.78 `shouldSatisfy` approxEq 0.1 24.6

  describe "normToMetres / metresToNorm" $ do
    let s = defaultUnitScales

    it "norm 0.5 → 0 m (sea level)" $
      normToMetres s 0.5 `shouldSatisfy` approxEq epsilon 0.0

    it "norm 1.0 → +6000 m" $
      normToMetres s 1.0 `shouldSatisfy` approxEq epsilon 6000.0

    it "norm 0.0 → −6000 m" $
      normToMetres s 0.0 `shouldSatisfy` approxEq epsilon (-6000.0)

    it "norm 0.85 → +4200 m (mountains)" $
      normToMetres s 0.85 `shouldSatisfy` approxEq epsilon 4200.0

  describe "normToMmYear / mmYearToNorm" $ do
    let s = defaultUnitScales

    it "norm 0.0 → 0 mm/yr" $
      normToMmYear s 0.0 `shouldSatisfy` approxEq epsilon 0.0

    it "norm 0.8 → 4800 mm/yr" $
      normToMmYear s 0.8 `shouldSatisfy` approxEq epsilon 4800.0

    it "norm 0.5 → 3000 mm/yr" $
      normToMmYear s 0.5 `shouldSatisfy` approxEq epsilon 3000.0

  describe "normToRH / rhToNorm" $ do
    it "norm 0.65 → 65 %" $
      normToRH 0.65 `shouldSatisfy` approxEq epsilon 65.0

    it "norm 0.0 → 0 %" $
      normToRH 0.0 `shouldSatisfy` approxEq epsilon 0.0

    it "norm 1.0 → 100 %" $
      normToRH 1.0 `shouldSatisfy` approxEq epsilon 100.0

  describe "normToWindMs / windMsToNorm" $ do
    let s = defaultUnitScales

    it "norm 0.2 → 10 m/s" $
      normToWindMs s 0.2 `shouldSatisfy` approxEq epsilon 10.0

    it "norm 0.6 → 30 m/s (storm)" $
      normToWindMs s 0.6 `shouldSatisfy` approxEq epsilon 30.0

  describe "normToHPa / hPaToNorm" $ do
    let s = defaultUnitScales

    it "norm 0.0 → 870 hPa" $
      normToHPa s 0.0 `shouldSatisfy` approxEq epsilon 870.0

    it "norm ~0.668 → ≈1013 hPa (mean sea level)" $
      normToHPa s (143.0 / 214.0) `shouldSatisfy` approxEq 0.5 1013.0

    it "norm 1.0 → 1084 hPa" $
      normToHPa s 1.0 `shouldSatisfy` approxEq epsilon 1084.0

  describe "normToSoilM / soilMToNorm" $ do
    let s = defaultUnitScales

    it "norm 0.0 → 0 m (bare rock)" $
      normToSoilM s 0.0 `shouldSatisfy` approxEq epsilon 0.0

    it "norm 0.4 → 2 m" $
      normToSoilM s 0.4 `shouldSatisfy` approxEq epsilon 2.0

    it "norm 1.0 → 5 m (deep tropical)" $
      normToSoilM s 1.0 `shouldSatisfy` approxEq epsilon 5.0

  describe "normSlopeToDeg" $ do
    let s = defaultUnitScales

    it "norm 0.0 → 0°" $
      normSlopeToDeg s 0.0 `shouldSatisfy` approxEq epsilon 0.0

    it "norm 0.20 → ≈6.5° (mountain threshold)" $
      normSlopeToDeg s 0.20 `shouldSatisfy` approxEq 0.5 6.5

  describe "normToLinear / linearToNorm" $ do
    it "normToLinear 70 (-30) 0.5 → 5" $
      normToLinear 70 (-30) 0.5 `shouldSatisfy` approxEq epsilon 5.0

    it "linearToNorm 70 (-30) 5 → 0.5" $
      linearToNorm 70 (-30) 5.0 `shouldSatisfy` approxEq epsilon 0.5

  -- -----------------------------------------------------------------------
  -- Round-trip property tests
  -- -----------------------------------------------------------------------

  describe "round-trip properties" $ do
    let s = defaultUnitScales

    it "temperature: cToNorm . normToC ≈ id" $ property $ \(UnitNorm n) ->
      approxEq 1e-4 n (cToNorm s (normToC s n))

    it "elevation: metresToNorm . normToMetres ≈ id" $ property $ \(UnitNorm n) ->
      approxEq 1e-4 n (metresToNorm s (normToMetres s n))

    it "precipitation: mmYearToNorm . normToMmYear ≈ id" $ property $ \(UnitNorm n) ->
      approxEq 1e-4 n (mmYearToNorm s (normToMmYear s n))

    it "humidity: rhToNorm . normToRH ≈ id" $ property $ \(UnitNorm n) ->
      approxEq 1e-4 n (rhToNorm (normToRH n))

    it "wind speed: windMsToNorm . normToWindMs ≈ id" $ property $ \(UnitNorm n) ->
      approxEq 1e-4 n (windMsToNorm s (normToWindMs s n))

    it "pressure: hPaToNorm . normToHPa ≈ id" $ property $ \(UnitNorm n) ->
      approxEq 1e-4 n (hPaToNorm s (normToHPa s n))

    it "soil depth: soilMToNorm . normToSoilM ≈ id" $ property $ \(UnitNorm n) ->
      approxEq 1e-4 n (soilMToNorm s (normToSoilM s n))

    it "generic affine: linearToNorm . normToLinear ≈ id (non-zero scale)" $
      property $ \(NonZero scale, offset, UnitNorm n) ->
        approxEq 1e-3 n (linearToNorm scale offset (normToLinear scale offset n))

  -- -----------------------------------------------------------------------
  -- Edge cases
  -- -----------------------------------------------------------------------

  describe "edge cases" $ do
    let s = defaultUnitScales

    it "normToMetres at norm 0.0 is negative (ocean floor)" $
      normToMetres s 0.0 `shouldSatisfy` (< 0)

    it "normToMetres at norm 1.0 is positive (mountain peak)" $
      normToMetres s 1.0 `shouldSatisfy` (> 0)

    it "normToHPa at norm 0.0 is the historical minimum" $
      normToHPa s 0.0 `shouldSatisfy` approxEq epsilon 870.0

    it "normToC at norm 0.0 matches offset exactly" $
      normToC s 0.0 `shouldSatisfy` approxEq epsilon (usTempOffset s)

    it "normSlopeToDeg is non-negative for non-negative input" $
      property $ \(NonNegative n) ->
        normSlopeToDeg s n >= 0

    it "normSlopeToDeg is monotonically increasing" $
      property $ \(UnitNorm a, UnitNorm b) ->
        a <= b ==> normSlopeToDeg s a <= normSlopeToDeg s b

-- | Newtype for generating values in [0, 1].
newtype UnitNorm = UnitNorm Float
  deriving (Show)

instance Arbitrary UnitNorm where
  arbitrary = UnitNorm <$> choose (0.0, 1.0)
  shrink (UnitNorm n) = UnitNorm <$> filter (\x -> x >= 0 && x <= 1) (shrink n)
