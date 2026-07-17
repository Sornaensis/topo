{-# LANGUAGE OverloadedStrings #-}

module Spec.ConfigSliders (spec) where

import Actor.UI (ConfigTab(..), UiState(..), configRowCount, emptyUiState)
import Data.Foldable (for_)
import Data.Maybe (isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Linear (V2(..))
import Test.Hspec
import UI.Components.ConfigSliders
import UI.DrawCommand (DrawCommand(..), TextPlacement(..))
import UI.Layout
import UI.WidgetId (WidgetId(..))
import UI.Widgets (Rect(..))

spec :: Spec
spec = describe "config tab and slider component extraction" $ do
  it "builds config tab views and labels from layout tab rects" $ do
    let views = configTabViews ConfigWeather (configTabRects wideLayout)
    map ctvLabel views `shouldBe`
      [ "Terrain", "Planet", "Climate", "Weather"
      , "Biome", "Erosion", "Pipeline", "Data"
      ]
    map ctvTab views `shouldBe`
      [ ConfigTerrain, ConfigPlanet, ConfigClimate, ConfigWeather
      , ConfigBiome, ConfigErosion, ConfigPipeline, ConfigData
      ]
    map ctvActive views `shouldBe`
      [False, False, False, True, False, False, False, False]
    mapMaybe fillRectOf (configTabDrawCommands views) `shouldBe` map ctvRect views
    mapMaybe centeredTextOf (configTabLabelCommands views) `shouldBe` map ctvLabel views

  it "creates slider row view models for the active terrain/climate style tabs" $ do
    let rows = configSliderRowsForTab ConfigClimate emptyUiState wideLayout
    firstRow <- requireHead "expected climate slider rows" rows
    length rows `shouldBe` configRowCount ConfigClimate emptyUiState
    csrSliderId firstRow `shouldBe` SliderWaterLevel
    csrRawValue firstRow `shouldBe` uiWaterLevel emptyUiState
    csrValue firstRow `shouldBe` uiWaterLevel emptyUiState
    csrLabel firstRow `shouldBe` "(?) Water Level: 0.43/1.00"
    csrTooltip firstRow `shouldSatisfy` isJust
    csrMinusWidget firstRow `shouldBe` WidgetSliderMinus SliderWaterLevel
    csrPlusWidget firstRow `shouldBe` WidgetSliderPlus SliderWaterLevel
    csrHitRect firstRow `shouldBe` configParamRowHitRect (configParamRects 0 wideLayout)

  it "keeps retained slider rows contiguous in every slider tab" $
    for_
      [ ConfigTerrain, ConfigPlanet, ConfigClimate
      , ConfigWeather, ConfigBiome, ConfigErosion
      ] $ \tab -> do
        let rows = configSliderRowsForTab tab emptyUiState wideLayout
        map csrHitRect rows `shouldBe`
          [ configParamRowHitRect (configParamRects idx wideLayout)
          | idx <- [0 .. length rows - 1]
          ]

  it "renders slider chrome and labels as pure draw commands" $ do
    firstRow <- requireHead "expected climate slider rows" $
      configSliderRowsForTab ConfigClimate emptyUiState wideLayout
    let drawRects = mapMaybe fillRectOf (configSliderDrawCommands [firstRow])
        labelTexts = mapMaybe commandText (configSliderLabelCommands [firstRow])
    drawRects `shouldBe`
      [ csrMinusRect firstRow
      , csrPlusRect firstRow
      , csrBarRect firstRow
      , expectedFillRect (csrValue firstRow) (csrBarRect firstRow)
      ]
    labelTexts `shouldBe` ["-", "+", csrLabel firstRow]

  it "omits button labels for bar-only erosion sliders" $ do
    let rows = configSliderRowsForTab ConfigErosion emptyUiState wideLayout
    hydroDeposit <- requireHead "expected hydraulic deposit slider row"
      [row | row <- rows, csrSliderId row == SliderErosionHydDeposit]
    mapMaybe commandText (configSliderLabelCommands [hydroDeposit])
      `shouldBe` [csrLabel hydroDeposit]

  it "clamps slider input steps before dispatch" $ do
    row <- requireHead "expected climate slider rows" $
      configSliderRowsForTab ConfigClimate emptyUiState wideLayout
    let highRow = row { csrRawValue = 0.99 }
        lowRow = row { csrRawValue = 0.01 }
    configSliderInputValueForRow row SliderPartPlus `shouldBeApprox` 0.48
    configSliderInputValueForRow lowRow SliderPartMinus `shouldBe` 0.0
    configSliderInputValueForRow highRow SliderPartPlus `shouldBe` 1.0

  it "scrolls active rows and surfaces invalid normalized values" $ do
    let ui = emptyUiState
          { uiConfigTab = ConfigClimate
          , uiConfigScroll = 12
          , uiWaterLevel = 1.25
          }
    unscrolled <- requireHead "expected unscrolled climate row" $
      configSliderRowsForActiveTab ui shortLayout
    scrolled <- requireHead "expected scrolled climate row" $
      configSliderRowsScrolled ui shortLayout
    let validations = configSliderValidations [scrolled]
    csrValue scrolled `shouldBe` 1.0
    rectY (csrBarRect scrolled) `shouldBe` rectY (csrBarRect unscrolled) - 12
    map cslvSliderId validations `shouldBe` [SliderWaterLevel]
    map cslvMessage validations `shouldSatisfy` all (Text.isInfixOf "outside [0,1]")
    mapMaybe commandText (configSliderValidationCommands validations)
      `shouldBe` map cslvMessage validations

wideLayout :: Layout
wideLayout = layoutFor (V2 800 960) 0

shortLayout :: Layout
shortLayout = layoutFor (V2 800 600) 0

fillRectOf :: DrawCommand -> Maybe Rect
fillRectOf command = case command of
  DrawFillRect _ rect -> Just rect
  _ -> Nothing

centeredTextOf :: DrawCommand -> Maybe Text
centeredTextOf command = case command of
  DrawText _ (TextCentered _) label -> Just label
  _ -> Nothing

commandText :: DrawCommand -> Maybe Text
commandText command = case command of
  DrawText _ _ label -> Just label
  _ -> Nothing

expectedFillRect :: Float -> Rect -> Rect
expectedFillRect value (Rect (V2 x y, V2 w h)) =
  Rect (V2 x y, V2 fillW h)
  where
    fillW = max 0 (min w (round (fromIntegral w * value)))

rectY :: Rect -> Int
rectY (Rect (V2 _ y, _)) = y

requireHead :: String -> [a] -> IO a
requireHead _ (value:_) = pure value
requireHead label [] = expectationFailure label >> fail label

shouldBeApprox :: Float -> Float -> Expectation
shouldBeApprox actual expected = abs (actual - expected) `shouldSatisfy` (< 1.0e-5)
