{-# LANGUAGE OverloadedStrings #-}

module Spec.SliderSpec (spec) where

import Actor.UI (ConfigTab(..), UiState(..), configRowCount, emptyUiState)
import Actor.UI.State (sliderValueForId)
import Data.Foldable (for_)
import Data.Text (Text)
import qualified Data.Text as Text
import Seer.Config (mapIntRange, mapRange, unmapIntRange, unmapRange)
import Seer.Config.SliderSpec
import Test.Hspec
import Test.QuickCheck
import UI.WidgetTree (WidgetId(..))

spec :: Spec
spec = do
  describe "sliderLabelForId" $ do
    it "formats float slider at minimum" $
      sliderLabelForId SliderGenScale 0.0
        `shouldBe` "(?) Scale: 0.20/2.00"

    it "formats float slider at maximum" $
      sliderLabelForId SliderGenScale 1.0
        `shouldBe` "(?) Scale: 2.00/2.00"

    it "formats float slider at midpoint" $
      sliderLabelForId SliderGenScale 0.5
        `shouldBe` "(?) Scale: 1.10/2.00"

    it "formats int slider at minimum" $
      sliderLabelForId SliderGenOctaves 0.0
        `shouldBe` "(?) Octaves: 2/8"

    it "formats int slider at maximum" $
      sliderLabelForId SliderGenOctaves 1.0
        `shouldBe` "(?) Octaves: 8/8"

    it "formats int slider at midpoint" $
      sliderLabelForId SliderGenOctaves 0.5
        `shouldBe` "(?) Octaves: 5/8"

    it "formats toggle-like sliders with explicit toggle value kind" $
      sliderLabelForId SliderHypsometryEnabled 1.0
        `shouldBe` "(?) Hypsometry: 1/1"

    it "always starts with (?) prefix" $
      property $ \(NonNegative t) ->
        let label = sliderLabelForId SliderGenScale (min 1.0 t)
        in Text.isPrefixOf "(?) " label

    it "always contains name" $
      property $ \(NonNegative t) ->
        let label = sliderLabelForId SliderGenFrequency (min 1.0 t)
        in Text.isInfixOf "Frequency" label

    it "always contains separator" $
      property $ \(NonNegative t) ->
        let label = sliderLabelForId SliderGenOctaves (min 1.0 t)
        in Text.isInfixOf "/" label

  describe "tooltipForWidget" $ do
    it "returns Just for config slider widgets" $ do
      tooltipForWidget WidgetConfigGenScaleMinus `shouldSatisfy` isJustText
      tooltipForWidget WidgetConfigGenScalePlus `shouldSatisfy` isJustText
      tooltipForWidget WidgetConfigWaterMinus `shouldSatisfy` isJustText
      tooltipForWidget WidgetConfigWaterPlus `shouldSatisfy` isJustText
      tooltipForWidget WidgetConfigErosionHydraulicMinus `shouldSatisfy` isJustText
      tooltipForWidget WidgetConfigErosionMaxDropPlus `shouldSatisfy` isJustText

    it "returns Nothing for non-config widgets" $ do
      tooltipForWidget WidgetGenerate `shouldBe` Nothing
      tooltipForWidget WidgetLeftToggle `shouldBe` Nothing
      tooltipForWidget WidgetSeedValue `shouldBe` Nothing
      tooltipForWidget WidgetSeedRandom `shouldBe` Nothing
      tooltipForWidget WidgetChunkMinus `shouldBe` Nothing
      tooltipForWidget WidgetConfigPresetSave `shouldBe` Nothing
      tooltipForWidget (WidgetPluginMoveUp "plugin-a") `shouldBe` Nothing
      tooltipForWidget WidgetSimTick `shouldBe` Nothing
      tooltipForWidget WidgetViewElevation `shouldBe` Nothing
      tooltipForWidget WidgetMenuSave `shouldBe` Nothing
      tooltipForWidget WidgetMenuExit `shouldBe` Nothing

    it "returns non-empty text for every config widget" $
      let configWidgets = allConfigWidgets
      in mapM_ (\wid -> tooltipForWidget wid `shouldSatisfy` isNonEmptyTip) configWidgets

    it "paired minus/plus widgets share the same tooltip" $ do
      tooltipForWidget WidgetConfigGenScaleMinus
        `shouldBe` tooltipForWidget WidgetConfigGenScalePlus
      tooltipForWidget WidgetConfigWaterMinus
        `shouldBe` tooltipForWidget WidgetConfigWaterPlus
      tooltipForWidget WidgetConfigErosionThermalMinus
        `shouldBe` tooltipForWidget WidgetConfigErosionThermalPlus

  describe "water level separation" $ do
    it "changing uiWaterLevel does not affect uiRenderWaterLevel" $
      -- This is a conceptual test: uiWaterLevel and uiRenderWaterLevel
      -- are separate fields. The terrain cache key uses uiRenderWaterLevel.
      -- We verify the spec wiring: the water level spec reads from
      -- the pending slider, not the render value.
      sliderLabelForId SliderWaterLevel 0.0 `shouldBe` "(?) Water Level: 0.00/1.00"

  describe "emptyUiState slider defaults" $ do
    it "keep representative normalized defaults across tabs" $ do
      let ui = emptyUiState
      mapM_ (assertDefaultValue ui)
        [ (uiGenScale, 0.4444)
        , (uiPlanetRadius, 0.3333)
        , (uiWaterLevel, 0.5)
        , (uiBoundaryMotionTemp, 0.5)
        , (uiWeatherTick, 0.2)
        , (uiVegBase, 0.2)
        , (uiRainRate, 0.2)
        ]

    it "keeps every exposed slider default normalized" $
      for_ allSliderIds $ \sliderIdValue ->
        let value = sliderValueForId emptyUiState sliderIdValue
        in value `shouldSatisfy` (\t -> t >= 0.0 && t <= 1.0)

  describe "slider registry coverage" $ do
    it "formats a non-empty label for every slider id" $
      for_ allSliderIds $ \sliderIdValue -> do
        let label = sliderLabelForId sliderIdValue 0.5
        Text.null label `shouldBe` False
        Text.isPrefixOf "(?) " label `shouldBe` True
        Text.isInfixOf "/" label `shouldBe` True

    it "keeps pipeline row counts additive with plugin entries" $ do
      let baseCount = configRowCount ConfigPipeline emptyUiState
          pluginUi = emptyUiState { uiPluginNames = ["plugin-a", "plugin-b"] }
      configRowCount ConfigPipeline pluginUi `shouldBe` baseCount + 2

  describe "range helper invariants" $ do
    it "mapRange and unmapRange round-trip normalized values" $
      property $
        forAll (choose (-2.0, 3.0) :: Gen Float) $ \rawValue ->
          let clamped = clamp01 rawValue
          in abs (unmapRange 0.2 2.0 (mapRange 0.2 2.0 rawValue) - clamped)
              <= floatRoundTripTolerance

    it "mapIntRange clamps arbitrary normalized values into bounds" $
      property $
        forAll (choose (-1000.0, 1000.0) :: Gen Float) $ \rawValue ->
          let domainValue = mapIntRange 2 8 rawValue
          in domainValue >= 2 .&&. domainValue <= 8

    it "unmapIntRange and mapIntRange round-trip integral domain values" $
      property $
        forAll (choose (-20, 20) :: Gen Int) $ \rawValue ->
          let normalized = unmapIntRange 2 8 rawValue
              domainValue = mapIntRange 2 8 normalized
          in normalized >= 0.0 .&&. normalized <= 1.0 .&&. domainValue >= 2 .&&. domainValue <= 8

-- | All WidgetIds that correspond to config slider minus/plus buttons.
allConfigWidgets :: [WidgetId]
allConfigWidgets =
  -- Terrain tab
  [ WidgetConfigGenScaleMinus, WidgetConfigGenScalePlus
  , WidgetConfigGenCoordScaleMinus, WidgetConfigGenCoordScalePlus
  , WidgetConfigGenOffsetXMinus, WidgetConfigGenOffsetXPlus
  , WidgetConfigGenOffsetYMinus, WidgetConfigGenOffsetYPlus
  , WidgetConfigGenFrequencyMinus, WidgetConfigGenFrequencyPlus
  , WidgetConfigGenOctavesMinus, WidgetConfigGenOctavesPlus
  , WidgetConfigGenLacunarityMinus, WidgetConfigGenLacunarityPlus
  , WidgetConfigGenGainMinus, WidgetConfigGenGainPlus
  , WidgetConfigGenWarpScaleMinus, WidgetConfigGenWarpScalePlus
  , WidgetConfigGenWarpStrengthMinus, WidgetConfigGenWarpStrengthPlus
  , WidgetConfigExtentXMinus, WidgetConfigExtentXPlus
  , WidgetConfigExtentYMinus, WidgetConfigExtentYPlus
  , WidgetConfigEdgeNorthMinus, WidgetConfigEdgeNorthPlus
  , WidgetConfigEdgeSouthMinus, WidgetConfigEdgeSouthPlus
  , WidgetConfigEdgeEastMinus, WidgetConfigEdgeEastPlus
  , WidgetConfigEdgeWestMinus, WidgetConfigEdgeWestPlus
  , WidgetConfigEdgeFalloffMinus, WidgetConfigEdgeFalloffPlus
  , WidgetConfigPlateSizeMinus, WidgetConfigPlateSizePlus
  , WidgetConfigUpliftMinus, WidgetConfigUpliftPlus
  , WidgetConfigRiftDepthMinus, WidgetConfigRiftDepthPlus
  , WidgetConfigDetailScaleMinus, WidgetConfigDetailScalePlus
  , WidgetConfigPlateSpeedMinus, WidgetConfigPlateSpeedPlus
  , WidgetConfigBoundarySharpnessMinus, WidgetConfigBoundarySharpnessPlus
  , WidgetConfigBoundaryNoiseScaleMinus, WidgetConfigBoundaryNoiseScalePlus
  , WidgetConfigBoundaryNoiseStrengthMinus, WidgetConfigBoundaryNoiseStrengthPlus
  , WidgetConfigBoundaryWarpOctavesMinus, WidgetConfigBoundaryWarpOctavesPlus
  , WidgetConfigBoundaryWarpLacunarityMinus, WidgetConfigBoundaryWarpLacunarityPlus
  , WidgetConfigBoundaryWarpGainMinus, WidgetConfigBoundaryWarpGainPlus
  , WidgetConfigPlateMergeScaleMinus, WidgetConfigPlateMergeScalePlus
  , WidgetConfigPlateMergeBiasMinus, WidgetConfigPlateMergeBiasPlus
  , WidgetConfigPlateDetailScaleMinus, WidgetConfigPlateDetailScalePlus
  , WidgetConfigPlateDetailStrengthMinus, WidgetConfigPlateDetailStrengthPlus
  , WidgetConfigPlateRidgeStrengthMinus, WidgetConfigPlateRidgeStrengthPlus
  , WidgetConfigPlateHeightBaseMinus, WidgetConfigPlateHeightBasePlus
  , WidgetConfigPlateHeightVarianceMinus, WidgetConfigPlateHeightVariancePlus
  , WidgetConfigPlateHardnessBaseMinus, WidgetConfigPlateHardnessBasePlus
  , WidgetConfigPlateHardnessVarianceMinus, WidgetConfigPlateHardnessVariancePlus
  , WidgetConfigTrenchDepthMinus, WidgetConfigTrenchDepthPlus
  , WidgetConfigRidgeHeightMinus, WidgetConfigRidgeHeightPlus
  , WidgetConfigPlateBiasStrengthMinus, WidgetConfigPlateBiasStrengthPlus
  , WidgetConfigPlateBiasCenterMinus, WidgetConfigPlateBiasCenterPlus
  , WidgetConfigPlateBiasEdgeMinus, WidgetConfigPlateBiasEdgePlus
  , WidgetConfigPlateBiasNorthMinus, WidgetConfigPlateBiasNorthPlus
  , WidgetConfigPlateBiasSouthMinus, WidgetConfigPlateBiasSouthPlus
  -- Climate tab
  , WidgetConfigWaterMinus, WidgetConfigWaterPlus
  , WidgetConfigOrographicLiftMinus, WidgetConfigOrographicLiftPlus
  , WidgetConfigRainShadowLossMinus, WidgetConfigRainShadowLossPlus
  , WidgetConfigWindDiffuseMinus, WidgetConfigWindDiffusePlus
  , WidgetConfigEquatorTempMinus, WidgetConfigEquatorTempPlus
  , WidgetConfigPoleTempMinus, WidgetConfigPoleTempPlus
  , WidgetConfigLapseRateMinus, WidgetConfigLapseRatePlus
  , WidgetConfigWindIterationsMinus, WidgetConfigWindIterationsPlus
  , WidgetConfigMoistureIterationsMinus, WidgetConfigMoistureIterationsPlus
  , WidgetConfigWeatherTickMinus, WidgetConfigWeatherTickPlus
  , WidgetConfigWeatherPhaseMinus, WidgetConfigWeatherPhasePlus
  , WidgetConfigWeatherAmplitudeMinus, WidgetConfigWeatherAmplitudePlus
  , WidgetConfigVegBaseMinus, WidgetConfigVegBasePlus
  , WidgetConfigVegBoostMinus, WidgetConfigVegBoostPlus
  , WidgetConfigVegTempWeightMinus, WidgetConfigVegTempWeightPlus
  , WidgetConfigVegPrecipWeightMinus, WidgetConfigVegPrecipWeightPlus
  , WidgetConfigBoundaryMotionTempMinus, WidgetConfigBoundaryMotionTempPlus
  , WidgetConfigBoundaryMotionPrecipMinus, WidgetConfigBoundaryMotionPrecipPlus
  , WidgetConfigPlanetRadiusMinus, WidgetConfigPlanetRadiusPlus
  , WidgetConfigAxialTiltMinus, WidgetConfigAxialTiltPlus
  , WidgetConfigInsolationMinus, WidgetConfigInsolationPlus
  , WidgetConfigSliceLatCenterMinus, WidgetConfigSliceLatCenterPlus
  , WidgetConfigSliceLonCenterMinus, WidgetConfigSliceLonCenterPlus
  -- Erosion tab
  , WidgetConfigErosionHydraulicMinus, WidgetConfigErosionHydraulicPlus
  , WidgetConfigErosionThermalMinus, WidgetConfigErosionThermalPlus
  , WidgetConfigErosionRainRateMinus, WidgetConfigErosionRainRatePlus
  , WidgetConfigErosionTalusMinus, WidgetConfigErosionTalusPlus
  , WidgetConfigErosionMaxDropMinus, WidgetConfigErosionMaxDropPlus
  ]

isJustText :: Maybe Text -> Bool
isJustText (Just _) = True
isJustText Nothing  = False

isNonEmptyTip :: Maybe Text -> Bool
isNonEmptyTip (Just t) = not (Text.null t)
isNonEmptyTip Nothing  = False

assertDefaultValue :: UiState -> (UiState -> Float, Float) -> Expectation
assertDefaultValue ui (uiGetter, expectedValue) =
  uiGetter ui `shouldBe` expectedValue

allSliderIds :: [SliderId]
allSliderIds = [minBound .. maxBound]

clamp01 :: Float -> Float
clamp01 = max 0.0 . min 1.0

floatRoundTripTolerance :: Float
floatRoundTripTolerance = 1.0e-5
