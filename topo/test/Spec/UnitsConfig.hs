module Spec.UnitsConfig (spec) where

import Test.Hspec
import Test.QuickCheck

import Topo.Units (UnitScales(..), defaultUnitScales)
import Topo.Units.Config
import Topo.Climate.Config
  ( TemperatureConfig(..)
  , defaultTemperatureConfig
  )
import Topo.Glacier (GlacierConfig(..), defaultGlacierConfig)
import Topo.Biome
  ( BiomeThresholds(..)
  , defaultBiomeThresholds
  )
import Topo.Vegetation
  ( VegetationBootstrapConfig(..)
  , defaultVegetationBootstrapConfig
  )
import Topo.Biome.Refine.Forest (ForestConfig(..), defaultForestConfig)
import Topo.Biome.Refine.Desert (DesertConfig(..), defaultDesertConfig)
import Topo.Biome.Refine.Ocean  (OceanConfig(..), defaultOceanConfig)
import Topo.Biome.Refine.Tundra (TundraConfig(..), defaultTundraConfig)
import Topo.Biome.Refine.Snow   (SnowConfig(..), defaultSnowConfig)

-- | Tolerance for floating-point comparisons.
epsilon :: Float
epsilon = 1e-3

-- | Helper: check approximate equality.
approxEq :: Float -> Float -> Float -> Bool
approxEq tol expected actual = abs (actual - expected) < tol

-- | Check two Floats are approximately equal.
(~=) :: Float -> Float -> Bool
(~=) = approxEq epsilon

spec :: Spec
spec = describe "Units.Config" $ do

  -- =====================================================================
  -- Known-value sanity checks
  -- =====================================================================

  describe "known values" $ do
    let s = defaultUnitScales

    describe "TemperatureConfig" $ do
      let real = temperatureConfigToReal s defaultTemperatureConfig

      it "equator temp ≈ 24.6 °C" $
        rtcEquatorTemp real `shouldSatisfy` approxEq 0.1 24.6

      it "pole temp ≈ −50.0 °C" $
        rtcPoleTemp real `shouldSatisfy` approxEq 0.1 (-50.0)

      it "ocean equator SST ≈ 28.1 °C" $
        rtcOceanEquatorSST real `shouldSatisfy` approxEq 0.1 28.1

      it "ocean pole SST ≈ 0.1 °C" $
        rtcOceanPoleSST real `shouldSatisfy` approxEq 0.1 0.1

    describe "GlacierConfig" $ do
      let real = glacierConfigToReal s defaultGlacierConfig

      it "snow temp ≈ −12.5 °C" $
        rgcSnowTemp real `shouldSatisfy` approxEq 0.1 (-12.5)

      it "melt temp ≈ −2.0 °C" $
        rgcMeltTemp real `shouldSatisfy` approxEq 0.1 (-2.0)

    describe "BiomeThresholds" $ do
      let real = biomeThresholdsToReal s defaultBiomeThresholds

      it "ice cap temp ≈ −26.5 °C" $
        rbtIceCapTemp real `shouldSatisfy` approxEq 0.1 (-26.5)

      it "snow min ASL ≈ 240 m" $
        rbtSnowMinASL real `shouldSatisfy` approxEq 1.0 240.0

      it "alpine min ASL ≈ 480 m" $
        rbtAlpineMinASL real `shouldSatisfy` approxEq 1.0 480.0

      it "montane max temp ≈ 8.5 °C" $
        rbtMontaneMaxTemp real `shouldSatisfy` approxEq 0.1 8.5

      it "coastal band ≈ 360 m" $
        rbtCoastalBand real `shouldSatisfy` approxEq 1.0 360.0

    describe "OceanConfig" $ do
      let real = oceanConfigToReal s defaultOceanConfig

      it "deep threshold ≈ −3000 m" $
        rocDeepThreshold real `shouldSatisfy` approxEq 1.0 (-3000.0)

      it "coral min temp ≈ 20.4 °C" $
        rocCoralMinTemp real `shouldSatisfy` approxEq 0.1 20.4

    describe "ForestConfig" $ do
      let real = forestConfigToReal s defaultForestConfig

      it "tropical dry min temp ≈ 21.8 °C" $
        rfcTropicalDryMinTemp real `shouldSatisfy` approxEq 0.1 21.8

      it "deciduous min temp ≈ −5.5 °C" $
        rfcDeciduousMinTemp real `shouldSatisfy` approxEq 0.1 (-5.5)

      it "coniferous max temp ≈ 10.6 °C" $
        rfcConiferousMaxTemp real `shouldSatisfy` approxEq 0.1 10.6

    describe "DesertConfig" $ do
      let real = desertConfigToReal s defaultDesertConfig

      it "hot min temp ≈ 19.0 °C" $
        rdcHotMinTemp real `shouldSatisfy` approxEq 0.1 19.0

      it "cold max temp ≈ −2.0 °C" $
        rdcColdMaxTemp real `shouldSatisfy` approxEq 0.1 (-2.0)

    describe "TundraConfig" $ do
      let real = tundraConfigToReal s defaultTundraConfig

      it "arctic max temp ≈ −23.0 °C" $
        rtcArcticMaxTemp real `shouldSatisfy` approxEq 0.1 (-23.0)

      it "alpine tundra min elev ≈ 1200 m" $
        rtcAlpineTundraMinElev real `shouldSatisfy` approxEq 1.0 1200.0

    describe "SnowConfig" $ do
      let real = snowConfigToReal s defaultSnowConfig

      it "ice cap max temp ≈ −26.5 °C" $
        rsnIceCapMaxTemp real `shouldSatisfy` approxEq 0.1 (-26.5)

      it "warm escape temp ≈ −12.5 °C" $
        rsnWarmEscapeTemp real `shouldSatisfy` approxEq 0.1 (-12.5)

    describe "lapse rate helpers" $ do
      it "default lapse rate ≈ 11.0 °C/km" $
        lapseRateCPerKm s (tmpLapseRate defaultTemperatureConfig)
          `shouldSatisfy` approxEq 0.1 11.0

      it "round-trip lapse rate" $
        let cPerKm = 6.5 :: Float
            norm = lapseRateFromCPerKm s cPerKm
            back = lapseRateCPerKm s norm
        in back `shouldSatisfy` approxEq 0.01 cPerKm

  -- =====================================================================
  -- Round-trip property tests
  -- =====================================================================

  describe "round-trip properties" $ do
    let s = defaultUnitScales
        tol = 1e-3

    it "TemperatureConfig: realTo . toReal ≈ id" $
      property $ \(TempPatch p) ->
        let base = defaultTemperatureConfig
            patched = p base
            real = temperatureConfigToReal s patched
            back = realToTemperatureConfig s real
        in allFieldsClose tol patched back

    it "GlacierConfig: realTo . toReal ≈ id" $
      let g = defaultGlacierConfig
          real = glacierConfigToReal s g
          back = realToGlacierConfig s real
      in allGlacierFieldsClose tol g back

    it "BiomeThresholds: realTo . toReal ≈ id" $
      let bt = defaultBiomeThresholds
          real = biomeThresholdsToReal s bt
          back = realToBiomeThresholds s real
      in allBiomeThresholdsClose tol bt back

    it "VegetationBootstrapConfig: realTo . toReal ≈ id" $
      let v = defaultVegetationBootstrapConfig
          real = vegetationBootstrapConfigToReal s v
          back = realToVegetationBootstrapConfig s real
      in allVegFieldsClose tol v back

    it "ForestConfig: realTo . toReal ≈ id" $
      let f = defaultForestConfig
          real = forestConfigToReal s f
          back = realToForestConfig s real
      in allForestFieldsClose tol f back

    it "DesertConfig: realTo . toReal ≈ id" $
      let d = defaultDesertConfig
          real = desertConfigToReal s d
          back = realToDesertConfig s real
      in allDesertFieldsClose tol d back

    it "OceanConfig: realTo . toReal ≈ id" $
      let o = defaultOceanConfig
          real = oceanConfigToReal s o
          back = realToOceanConfig s real
      in allOceanFieldsClose tol o back

    it "TundraConfig: realTo . toReal ≈ id" $
      let t = defaultTundraConfig
          real = tundraConfigToReal s t
          back = realToTundraConfig s real
      in allTundraFieldsClose tol t back

    it "SnowConfig: realTo . toReal ≈ id" $
      let sn = defaultSnowConfig
          real = snowConfigToReal s sn
          back = realToSnowConfig s real
      in allSnowFieldsClose tol sn back

    it "lapse rate: fromCPerKm . toCPerKm ≈ id" $
      property $ \(Positive rate) ->
        let rate' = min 20.0 (rate :: Float)
            norm = lapseRateFromCPerKm s rate'
            back = lapseRateCPerKm s norm
        in approxEq 0.01 rate' back

-- =====================================================================
-- Field-by-field comparators
-- =====================================================================

allFieldsClose :: Float -> TemperatureConfig -> TemperatureConfig -> Bool
allFieldsClose tol a b = all id
  [ approxEq tol (tmpEquatorTemp a) (tmpEquatorTemp b)
  , approxEq tol (tmpPoleTemp a) (tmpPoleTemp b)
  , approxEq tol (tmpLapseRate a) (tmpLapseRate b)
  , approxEq tol (tmpLatitudeExponent a) (tmpLatitudeExponent b)
  , approxEq tol (tmpPlateHeightCooling a) (tmpPlateHeightCooling b)
  , approxEq tol (tmpNoiseScale a) (tmpNoiseScale b)
  , tmpNoiseOctaves a == tmpNoiseOctaves b
  , approxEq tol (tmpNoiseFrequency a) (tmpNoiseFrequency b)
  , tmpDiffuseIterations a == tmpDiffuseIterations b
  , approxEq tol (tmpDiffuseFactor a) (tmpDiffuseFactor b)
  , approxEq tol (tmpCoastalBlendWidth a) (tmpCoastalBlendWidth b)
  , approxEq tol (tmpOceanModeration a) (tmpOceanModeration b)
  , approxEq tol (tmpOceanModerateTemp a) (tmpOceanModerateTemp b)
  , approxEq tol (tmpAlbedoSensitivity a) (tmpAlbedoSensitivity b)
  , approxEq tol (tmpAlbedoReference a) (tmpAlbedoReference b)
  , approxEq tol (tmpOceanEquatorSST a) (tmpOceanEquatorSST b)
  , approxEq tol (tmpOceanPoleSST a) (tmpOceanPoleSST b)
  , approxEq tol (tmpOceanLatExponent a) (tmpOceanLatExponent b)
  ]

allGlacierFieldsClose :: Float -> GlacierConfig -> GlacierConfig -> Bool
allGlacierFieldsClose tol a b = all id
  [ approxEq tol (gcSnowTemp a) (gcSnowTemp b)
  , approxEq tol (gcSnowRange a) (gcSnowRange b)
  , approxEq tol (gcMeltTemp a) (gcMeltTemp b)
  , approxEq tol (gcMeltRate a) (gcMeltRate b)
  , approxEq tol (gcAccumScale a) (gcAccumScale b)
  , gcFlowIterations a == gcFlowIterations b
  , approxEq tol (gcFlowRate a) (gcFlowRate b)
  , approxEq tol (gcHardnessErosionWeight a) (gcHardnessErosionWeight b)
  , approxEq tol (gcErosionScale a) (gcErosionScale b)
  , approxEq tol (gcDepositScale a) (gcDepositScale b)
  , approxEq tol (gcDepositMaxSlope a) (gcDepositMaxSlope b)
  , approxEq tol (gcCarveScale a) (gcCarveScale b)
  , approxEq tol (gcDepositRaiseScale a) (gcDepositRaiseScale b)
  ]

allBiomeThresholdsClose :: Float -> BiomeThresholds -> BiomeThresholds -> Bool
allBiomeThresholdsClose tol a b = all id
  [ approxEq tol (btCoastalBand a) (btCoastalBand b)
  , btFallbackBiome a == btFallbackBiome b
  , approxEq tol (btIceCapTemp a) (btIceCapTemp b)
  , approxEq tol (btMontanePrecip a) (btMontanePrecip b)
  , approxEq tol (btCliffSlope a) (btCliffSlope b)
  , approxEq tol (btValleyMoisture a) (btValleyMoisture b)
  , approxEq tol (btDepressionMoisture a) (btDepressionMoisture b)
  , approxEq tol (btLakeshoreMoisture a) (btLakeshoreMoisture b)
  , approxEq tol (btLakeshoreMinTemp a) (btLakeshoreMinTemp b)
  , approxEq tol (btPrecipWeight a) (btPrecipWeight b)
  , approxEq tol (btSeasonalitySavannaThreshold a) (btSeasonalitySavannaThreshold b)
  , approxEq tol (btTempRangeGrasslandThreshold a) (btTempRangeGrasslandThreshold b)
  , approxEq tol (btSnowPolarDesertMaxPrecip a) (btSnowPolarDesertMaxPrecip b)
  , approxEq tol (btReliefSavannaThreshold a) (btReliefSavannaThreshold b)
  , approxEq tol (btSnowMaxTemp a) (btSnowMaxTemp b)
  , approxEq tol (btSnowMinASL a) (btSnowMinASL b)
  , approxEq tol (btAlpineMaxTemp a) (btAlpineMaxTemp b)
  , approxEq tol (btAlpineMinASL a) (btAlpineMinASL b)
  , approxEq tol (btMontaneMaxTemp a) (btMontaneMaxTemp b)
  , approxEq tol (btMontaneMinASL a) (btMontaneMinASL b)
  , approxEq tol (btMontaneMinSlope a) (btMontaneMinSlope b)
  , approxEq tol (btMontaneMinHumidity a) (btMontaneMinHumidity b)
  ]

allVegFieldsClose :: Float -> VegetationBootstrapConfig -> VegetationBootstrapConfig -> Bool
allVegFieldsClose tol a b = all id
  [ approxEq tol (vbcTempMin a) (vbcTempMin b)
  , approxEq tol (vbcTempRange a) (vbcTempRange b)
  , approxEq tol (vbcFertilityBoost a) (vbcFertilityBoost b)
  , approxEq tol (vbcAlbedoBase a) (vbcAlbedoBase b)
  , approxEq tol (vbcAlbedoBare a) (vbcAlbedoBare b)
  , approxEq tol (vbcAlbedoVeg a) (vbcAlbedoVeg b)
  , approxEq tol (vbcOceanAlbedo a) (vbcOceanAlbedo b)
  , approxEq tol (vbcIceAlbedo a) (vbcIceAlbedo b)
  , approxEq tol (vbcMinMoisture a) (vbcMinMoisture b)
  , vbcCoastalIterations a == vbcCoastalIterations b
  , approxEq tol (vbcCoastalDiffuse a) (vbcCoastalDiffuse b)
  , approxEq tol (vbcCoastalBoost a) (vbcCoastalBoost b)
  ]

allForestFieldsClose :: Float -> ForestConfig -> ForestConfig -> Bool
allForestFieldsClose tol a b = all id
  [ approxEq tol (fcTropicalDryMinTemp a) (fcTropicalDryMinTemp b)
  , approxEq tol (fcTropicalDryMaxPrecip a) (fcTropicalDryMaxPrecip b)
  , approxEq tol (fcDeciduousMinTemp a) (fcDeciduousMinTemp b)
  , approxEq tol (fcDeciduousMinPrecip a) (fcDeciduousMinPrecip b)
  , approxEq tol (fcConiferousMaxTemp a) (fcConiferousMaxTemp b)
  , approxEq tol (fcConiferousMinHardness a) (fcConiferousMinHardness b)
  , approxEq tol (fcMontaneMinElev a) (fcMontaneMinElev b)
  , approxEq tol (fcMontaneMinSlope a) (fcMontaneMinSlope b)
  , approxEq tol (fcCloudForestMinElev a) (fcCloudForestMinElev b)
  , approxEq tol (fcCloudForestMinPrecip a) (fcCloudForestMinPrecip b)
  , approxEq tol (fcCloudForestMinTemp a) (fcCloudForestMinTemp b)
  , approxEq tol (fcCloudForestMinHumidity a) (fcCloudForestMinHumidity b)
  , approxEq tol (fcTempRainforestMinPrecip a) (fcTempRainforestMinPrecip b)
  , approxEq tol (fcTempRainforestMaxTemp a) (fcTempRainforestMaxTemp b)
  , approxEq tol (fcTempRainforestMinHumidity a) (fcTempRainforestMinHumidity b)
  , approxEq tol (fcSeasonalForestMinSeason a) (fcSeasonalForestMinSeason b)
  , approxEq tol (fcSeasonalForestMinTemp a) (fcSeasonalForestMinTemp b)
  ]

allDesertFieldsClose :: Float -> DesertConfig -> DesertConfig -> Bool
allDesertFieldsClose tol a b = all id
  [ approxEq tol (dcHotMinTemp a) (dcHotMinTemp b)
  , approxEq tol (dcColdMaxTemp a) (dcColdMaxTemp b)
  , approxEq tol (dcRockyMinHardness a) (dcRockyMinHardness b)
  , approxEq tol (dcRockyMaxSoilDepth a) (dcRockyMaxSoilDepth b)
  , approxEq tol (dcSandMaxHardness a) (dcSandMaxHardness b)
  , approxEq tol (dcSaltFlatMaxMoist a) (dcSaltFlatMaxMoist b)
  , approxEq tol (dcFogDesertMinHumidity a) (dcFogDesertMinHumidity b)
  , approxEq tol (dcFogDesertMaxPrecip a) (dcFogDesertMaxPrecip b)
  , approxEq tol (dcFogDesertMaxSeason a) (dcFogDesertMaxSeason b)
  ]

allOceanFieldsClose :: Float -> OceanConfig -> OceanConfig -> Bool
allOceanFieldsClose tol a b = all id
  [ approxEq tol (ocDeepThreshold a) (ocDeepThreshold b)
  , approxEq tol (ocCoralMinTemp a) (ocCoralMinTemp b)
  , approxEq tol (ocCoralMaxDepth a) (ocCoralMaxDepth b)
  , approxEq tol (ocCoralMaxSlope a) (ocCoralMaxSlope b)
  , approxEq tol (ocCoralMinHardness a) (ocCoralMinHardness b)
  ]

allTundraFieldsClose :: Float -> TundraConfig -> TundraConfig -> Bool
allTundraFieldsClose tol a b = all id
  [ approxEq tol (tcArcticMaxTemp a) (tcArcticMaxTemp b)
  , approxEq tol (tcAlpineTundraMinElev a) (tcAlpineTundraMinElev b)
  , approxEq tol (tcPolarDesertMaxPrecip a) (tcPolarDesertMaxPrecip b)
  ]

allSnowFieldsClose :: Float -> SnowConfig -> SnowConfig -> Bool
allSnowFieldsClose tol a b = all id
  [ approxEq tol (snIceCapMaxTemp a) (snIceCapMaxTemp b)
  , approxEq tol (snGlacierMinIceThick a) (snGlacierMinIceThick b)
  , approxEq tol (snGlacierMaxTemp a) (snGlacierMaxTemp b)
  , approxEq tol (snSnowfieldMinSnowpack a) (snSnowfieldMinSnowpack b)
  , approxEq tol (snMarginalMinTemp a) (snMarginalMinTemp b)
  , approxEq tol (snSteepSlopeThreshold a) (snSteepSlopeThreshold b)
  , approxEq tol (snWarmEscapeTemp a) (snWarmEscapeTemp b)
  ]

-- =====================================================================
-- QuickCheck generators
-- =====================================================================

-- | Small perturbation of TemperatureConfig for property tests.
newtype TempPatch = TempPatch (TemperatureConfig -> TemperatureConfig)

instance Show TempPatch where
  show _ = "<TempPatch>"

instance Arbitrary TempPatch where
  arbitrary = do
    eq <- choose (0.5, 0.95)
    po <- choose (0.0, 0.2)
    lr <- choose (0.3, 0.9)
    oeq <- choose (0.7, 0.95)
    opo <- choose (0.2, 0.55)
    pure $ TempPatch $ \t -> t
      { tmpEquatorTemp     = eq
      , tmpPoleTemp        = po
      , tmpLapseRate       = lr
      , tmpOceanEquatorSST = oeq
      , tmpOceanPoleSST    = opo
      }
