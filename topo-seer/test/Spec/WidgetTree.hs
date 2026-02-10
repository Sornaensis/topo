module Spec.WidgetTree (spec) where

import Test.Hspec
import Linear (V2(..))
import UI.Layout
import UI.WidgetTree

spec :: Spec
spec = describe "UI.WidgetTree" $ do
  it "hit tests widgets" $ do
    let layout = layoutFor (V2 800 600) 160
        widgets = buildWidgets layout
        result = hitTest widgets (V2 20 20)
    result `shouldBe` Just WidgetGenerate

  it "hit tests chunk buttons" $ do
    let layout = layoutFor (V2 800 600) 160
        widgets = buildWidgets layout
    hitTest widgets (V2 30 140) `shouldBe` Just WidgetChunkMinus
    hitTest widgets (V2 230 140) `shouldBe` Just WidgetChunkPlus

  it "hit tests moisture view button" $ do
    let layout = layoutFor (V2 800 600) 160
        widgets = buildWidgets layout
    hitTest widgets (V2 150 176) `shouldBe` Just WidgetViewMoisture

  it "hit tests log filter buttons" $ do
    let layout = layoutFor (V2 800 600) 160
        widgets = buildWidgets layout
    hitTest widgets (V2 704 447) `shouldBe` Just WidgetLogDebug

  it "hit tests config toggle" $ do
    let layout = layoutFor (V2 800 600) 160
        widgets = buildWidgets layout
    hitTest widgets (V2 560 30) `shouldBe` Just WidgetConfigToggle

  it "hit tests config apply/reset" $ do
    let layout = layoutFor (V2 800 600) 160
        widgets = buildWidgets layout
    hitTest widgets (V2 600 328) `shouldBe` Just WidgetConfigApply
    hitTest widgets (V2 600 360) `shouldBe` Just WidgetConfigReplay
    hitTest widgets (V2 600 392) `shouldBe` Just WidgetConfigReset

  it "hit tests config slider buttons" $ do
    let layout = layoutFor (V2 800 600) 160
        widgets = buildWidgets layout
    hitTest widgets (V2 560 118) `shouldBe` Just WidgetConfigWaterMinus
    hitTest widgets (V2 740 118) `shouldBe` Just WidgetConfigWaterPlus
    hitTest widgets (V2 560 152) `shouldBe` Just WidgetConfigEvapMinus
    hitTest widgets (V2 740 152) `shouldBe` Just WidgetConfigEvapPlus
    hitTest widgets (V2 560 186) `shouldBe` Just WidgetConfigRainShadowMinus
    hitTest widgets (V2 740 186) `shouldBe` Just WidgetConfigRainShadowPlus
    hitTest widgets (V2 560 220) `shouldBe` Just WidgetConfigWindDiffuseMinus
    hitTest widgets (V2 740 220) `shouldBe` Just WidgetConfigWindDiffusePlus
    hitTest widgets (V2 560 254) `shouldBe` Just WidgetConfigEquatorTempMinus
    hitTest widgets (V2 740 254) `shouldBe` Just WidgetConfigEquatorTempPlus
    hitTest widgets (V2 560 288) `shouldBe` Just WidgetConfigPoleTempMinus
    hitTest widgets (V2 740 288) `shouldBe` Just WidgetConfigPoleTempPlus
    hitTest widgets (V2 560 312) `shouldBe` Just WidgetConfigLapseRateMinus
    hitTest widgets (V2 740 312) `shouldBe` Just WidgetConfigLapseRatePlus
