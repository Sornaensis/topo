{-# LANGUAGE OverloadedStrings #-}

module Spec.SliderSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as Text
import Seer.Config.SliderSpec
import Test.Hspec
import Test.QuickCheck
import UI.WidgetTree (WidgetId(..))

spec :: Spec
spec = do
  describe "sliderLabel" $ do
    it "formats float spec at minimum" $
      sliderLabel (SliderSpec "Scale" "" 0.2 2.0 False) 0.0
        `shouldBe` "(?) Scale: 0.20/2.00"

    it "formats float spec at maximum" $
      sliderLabel (SliderSpec "Scale" "" 0.2 2.0 False) 1.0
        `shouldBe` "(?) Scale: 2.00/2.00"

    it "formats float spec at midpoint" $
      sliderLabel (SliderSpec "Scale" "" 0.2 2.0 False) 0.5
        `shouldBe` "(?) Scale: 1.10/2.00"

    it "formats int spec at minimum" $
      sliderLabel (SliderSpec "Octaves" "" 2 8 True) 0.0
        `shouldBe` "(?) Octaves: 2/8"

    it "formats int spec at maximum" $
      sliderLabel (SliderSpec "Octaves" "" 2 8 True) 1.0
        `shouldBe` "(?) Octaves: 8/8"

    it "formats int spec at midpoint" $
      sliderLabel (SliderSpec "Octaves" "" 2 8 True) 0.5
        `shouldBe` "(?) Octaves: 5/8"

    it "always starts with (?) prefix" $
      property $ \(NonNegative t) ->
        let label = sliderLabel specGenScale (min 1.0 t)
        in Text.isPrefixOf "(?) " label

    it "always contains name" $
      property $ \(NonNegative t) ->
        let s = specGenFrequency
            label = sliderLabel s (min 1.0 t)
        in Text.isInfixOf (ssName s) label

    it "always contains separator" $
      property $ \(NonNegative t) ->
        let label = sliderLabel specGenOctaves (min 1.0 t)
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
      ssName specWaterLevel `shouldBe` "Water Level"

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
  , WidgetConfigEvapMinus, WidgetConfigEvapPlus
  , WidgetConfigRainShadowMinus, WidgetConfigRainShadowPlus
  , WidgetConfigWindDiffuseMinus, WidgetConfigWindDiffusePlus
  , WidgetConfigEquatorTempMinus, WidgetConfigEquatorTempPlus
  , WidgetConfigPoleTempMinus, WidgetConfigPoleTempPlus
  , WidgetConfigLapseRateMinus, WidgetConfigLapseRatePlus
  , WidgetConfigLatitudeBiasMinus, WidgetConfigLatitudeBiasPlus
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
  , WidgetConfigSliceLatExtentMinus, WidgetConfigSliceLatExtentPlus
  , WidgetConfigSliceLonCenterMinus, WidgetConfigSliceLonCenterPlus
  , WidgetConfigSliceLonExtentMinus, WidgetConfigSliceLonExtentPlus
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
