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
-- satNormCfg — configurable saturation (Phase 2.2)
---------------------------------------------------------------------------

satNormCfgSpec :: Spec
satNormCfgSpec = describe "satNormCfg" $ do
  it "default config matches satNorm" $ do
    abs (satNormCfg defaultMoistureConfig 0.43 - satNorm 0.43)
      `shouldSatisfy` (< 1e-6)
    abs (satNormCfg defaultMoistureConfig 0.80 - satNorm 0.80)
      `shouldSatisfy` (< 1e-6)

  it "narrower scale → steeper curve at mid-range" $ do
    let narrow = defaultMoistureConfig { ccTempToC_Scale = 40.0
                                       , ccTempToC_Offset = -10.0 }
    -- At T=0.5: default maps to 5°C, narrow maps to 10°C.
    -- Narrower warm band pushes midpoint saturation higher.
    satNormCfg narrow 0.5 `shouldSatisfy` (> satNormCfg defaultMoistureConfig 0.5)

  it "colder offset shifts whole curve down" $ do
    let cold = defaultMoistureConfig { ccTempToC_Offset = -50.0 }
    -- At T=0.5: default maps to 5°C, cold maps to -15°C.
    satNormCfg cold 0.5 `shouldSatisfy` (< satNormCfg defaultMoistureConfig 0.5)

  it "endpoints are 0 and 1 regardless of config" $ do
    let cfg = defaultMoistureConfig { ccTempToC_Scale = 100.0
                                    , ccTempToC_Offset = -50.0 }
    satNormCfg cfg 0.0 `shouldSatisfy` (< 0.01)
    satNormCfg cfg 1.0 `shouldSatisfy` (> 0.99)

  prop "always in [0,1] for arbitrary configs" $
    forAll ((,,,) <$> choose (20.0, 200.0) <*> choose (-60.0, 0.0)
                  <*> unit <*> pure ()) $
      \(scale, offset, t, _) ->
        let cfg = defaultMoistureConfig { ccTempToC_Scale = scale
                                        , ccTempToC_Offset = offset }
            s = satNormCfg cfg t
        in s >= 0 && s <= 1

  it "changing scale/offset materially shifts ocean evaporation" $ do
    let hot = defaultMoistureConfig { ccTempToC_Scale = 40.0
                                    , ccTempToC_Offset = 0.0 }
    -- With a warm-planet mapping, T=0.5 maps to 20°C vs default 5°C.
    -- Ocean evaporation should be substantially higher.
    let eDefault = oceanEvaporation defaultMoistureConfig 0.5 0.5 1.0
        eHot     = oceanEvaporation hot 0.5 0.5 1.0
    eHot `shouldSatisfy` (> eDefault * 1.5)

  it "changing scale/offset materially shifts land ET" $ do
    let hot = defaultMoistureConfig { ccTempToC_Scale = 40.0
                                    , ccTempToC_Offset = 0.0 }
    let etDefault = landEvapotranspiration defaultMoistureConfig 0.5 0.6 0.8 0.3
        etHot     = landEvapotranspiration hot 0.5 0.6 0.8 0.3
    etHot `shouldSatisfy` (> etDefault * 1.5)

---------------------------------------------------------------------------
-- magnusES
---------------------------------------------------------------------------

magnusESSpec :: Spec
magnusESSpec = describe "magnusES" $ do
  it "freezing point ≈ 6.1 hPa" $
    abs (magnusES 0.0 - 6.1078) `shouldSatisfy` (< 0.01)

  it "100°C → high vapour pressure" $
    magnusES 100.0 `shouldSatisfy` (> 1000.0)

  prop "monotonically increasing" $
    \(a' :: Float) (b' :: Float) ->
      let a = min a' b'
          b = max a' b'
      -- Restrict to physically meaningful range to avoid Float overflow
      in (a >= -60 && b <= 120) ==> magnusES a <= magnusES b

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
  satNormCfgSpec
  magnusESSpec
  oceanEvaporationSpec
  landETSpec
  configSpec
