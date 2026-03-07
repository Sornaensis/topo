module Spec.WidgetTree (spec) where

import Test.Hspec
import Linear (V2(..))
import UI.Layout
import UI.WidgetTree
import UI.Widgets (Rect(..))

rectHitPoint :: Rect -> V2 Int
rectHitPoint (Rect (V2 x y, V2 w h)) = V2 (x + w `div` 2) (y + h `div` 2)

spec :: Spec
spec = describe "UI.WidgetTree" $ do
  it "hit tests generate button in left panel" $ do
    let layout = layoutFor (V2 800 600) 160
        widgets = buildWidgets layout
        -- Gen button is now inside left panel at Row 4
        Rect (V2 gx gy, V2 gw gh) = leftGenButtonRect layout
        result = hitTest widgets (V2 (gx + 5) (gy + 5))
    result `shouldBe` Just WidgetGenerate

  it "hit tests chunk buttons" $ do
    let layout = layoutFor (V2 800 600) 160
        widgets = buildWidgets layout
    hitTest widgets (rectHitPoint (leftChunkMinusRect layout)) `shouldBe` Just WidgetChunkMinus
    hitTest widgets (rectHitPoint (leftChunkPlusRect layout)) `shouldBe` Just WidgetChunkPlus

  it "hit tests moisture view button" $ do
    let layout = layoutFor (V2 800 600) 160
        widgets = buildWidgets layout
        (_, _, _, moistureView, _, _, _, _, _, _, _, _) = leftViewRects layout
    hitTest widgets (rectHitPoint moistureView) `shouldBe` Just WidgetViewMoisture

  it "hit tests log filter buttons" $ do
    let layout = layoutFor (V2 800 600) 160
        widgets = buildWidgets layout
        (debugRect, _, _, _) = logFilterRects layout
    hitTest widgets (rectHitPoint debugRect) `shouldBe` Just WidgetLogDebug

  it "hit tests config toggle" $ do
    let layout = layoutFor (V2 800 600) 160
        widgets = buildWidgets layout
    hitTest widgets (rectHitPoint (configToggleRect layout)) `shouldBe` Just WidgetConfigToggle

  it "hit tests config preset save/load/reset/revert" $ do
    let layout = layoutFor (V2 800 600) 160
        widgets = buildWidgets layout
        Rect (V2 sx sy, _) = configPresetSaveRect layout
        Rect (V2 lx ly, _) = configPresetLoadRect layout
        Rect (V2 rstx rsty, _) = configResetRect layout
        Rect (V2 rvx rvy, _) = configRevertRect layout
    hitTest widgets (V2 (sx + 5) (sy + 5)) `shouldBe` Just WidgetConfigPresetSave
    hitTest widgets (V2 (lx + 5) (ly + 5)) `shouldBe` Just WidgetConfigPresetLoad
    hitTest widgets (V2 (rstx + 5) (rsty + 5)) `shouldBe` Just WidgetConfigReset
    hitTest widgets (V2 (rvx + 5) (rvy + 5)) `shouldBe` Just WidgetConfigRevert

  it "hit tests config slider buttons" $ do
    let layout = layoutFor (V2 800 960) 0
        widgets = buildWidgets layout
    hitTest widgets (rectHitPoint (configWaterMinusRect layout)) `shouldBe` Just WidgetConfigWaterMinus
    hitTest widgets (rectHitPoint (configWaterPlusRect layout)) `shouldBe` Just WidgetConfigWaterPlus
    hitTest widgets (rectHitPoint (configOrographicLiftMinusRect layout)) `shouldBe` Just WidgetConfigOrographicLiftMinus
    hitTest widgets (rectHitPoint (configOrographicLiftPlusRect layout)) `shouldBe` Just WidgetConfigOrographicLiftPlus
    hitTest widgets (rectHitPoint (configRainShadowLossMinusRect layout)) `shouldBe` Just WidgetConfigRainShadowLossMinus
    hitTest widgets (rectHitPoint (configRainShadowLossPlusRect layout)) `shouldBe` Just WidgetConfigRainShadowLossPlus
    hitTest widgets (rectHitPoint (configWindDiffuseMinusRect layout)) `shouldBe` Just WidgetConfigWindDiffuseMinus
    hitTest widgets (rectHitPoint (configWindDiffusePlusRect layout)) `shouldBe` Just WidgetConfigWindDiffusePlus
    hitTest widgets (rectHitPoint (configEquatorTempMinusRect layout)) `shouldBe` Just WidgetConfigEquatorTempMinus
    hitTest widgets (rectHitPoint (configEquatorTempPlusRect layout)) `shouldBe` Just WidgetConfigEquatorTempPlus
    hitTest widgets (rectHitPoint (configPoleTempMinusRect layout)) `shouldBe` Just WidgetConfigPoleTempMinus
    hitTest widgets (rectHitPoint (configPoleTempPlusRect layout)) `shouldBe` Just WidgetConfigPoleTempPlus
    hitTest widgets (rectHitPoint (configLapseRateMinusRect layout)) `shouldBe` Just WidgetConfigLapseRateMinus
    hitTest widgets (rectHitPoint (configLapseRatePlusRect layout)) `shouldBe` Just WidgetConfigLapseRatePlus
