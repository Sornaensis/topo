module Spec.WidgetTree (spec) where

import Actor.UI (ConfigTab(..), configRowCount, emptyUiState)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Linear (V2(..))
import Seer.Config.SliderSpec (SliderId(..))
import Seer.Editor.Types (EditorTool(..))
import Test.Hspec
import Topo.Pipeline.Stage (allBuiltinStageIds)
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
        viewRects = leftViewRects layout
        moistureRect = viewRects !! 4  -- index 4: Moisture
    hitTest widgets (rectHitPoint moistureRect) `shouldBe` Just WidgetViewMoisture

  it "hit tests all 15 view mode buttons" $ do
    let layout = layoutFor (V2 800 800) 160
        widgets = buildWidgets layout
        viewRects = leftViewRects layout
        expectedIds =
          [ WidgetViewElevation, WidgetViewBiome, WidgetViewClimate
          , WidgetViewWeather, WidgetViewMoisture, WidgetViewPrecip
          , WidgetViewVegetation, WidgetViewTerrainForm
          , WidgetViewPlateId, WidgetViewPlateBoundary
          , WidgetViewPlateHardness, WidgetViewPlateCrust
          , WidgetViewPlateAge, WidgetViewPlateHeight
          , WidgetViewPlateVelocity
          ]
    length viewRects `shouldBe` 15
    -- Rows 5-7 (indices 10-14) are below LeftTopo controls and can be
    -- hit-tested unambiguously.  Rows 0-4 overlap LeftTopo widgets
    -- (chunk, seed, generate) which shadow view buttons in the
    -- unfiltered hit list; the runtime click handler guards by active
    -- tab so the overlap is harmless.
    mapM_ (\(rect, wid) -> hitTest widgets (rectHitPoint rect) `shouldBe` Just wid)
      (drop 10 (zip viewRects expectedIds))

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
    mapM_ (assertSliderButtons widgets layout)
      [ (0, WidgetSliderMinus SliderWaterLevel, WidgetSliderPlus SliderWaterLevel)
      , (1, WidgetSliderMinus SliderOrographicLift, WidgetSliderPlus SliderOrographicLift)
      , (2, WidgetSliderMinus SliderRainShadowLoss, WidgetSliderPlus SliderRainShadowLoss)
      , (3, WidgetSliderMinus SliderWindDiffuse, WidgetSliderPlus SliderWindDiffuse)
      , (4, WidgetSliderMinus SliderEquatorTemp, WidgetSliderPlus SliderEquatorTemp)
      , (5, WidgetSliderMinus SliderPoleTemp, WidgetSliderPlus SliderPoleTemp)
      , (6, WidgetSliderMinus SliderLapseRate, WidgetSliderPlus SliderLapseRate)
      ]

  it "builds slider row widgets from the registry definitions for each tab" $ do
    let layout = layoutFor (V2 800 960) 0
        (terrain, planet, climate, weather, biome, erosion) = buildSliderRowWidgets layout
    length terrain `shouldBe` configRowCount ConfigTerrain emptyUiState
    length planet `shouldBe` configRowCount ConfigPlanet emptyUiState
    length climate `shouldBe` configRowCount ConfigClimate emptyUiState
    length weather `shouldBe` configRowCount ConfigWeather emptyUiState
    length biome `shouldBe` configRowCount ConfigBiome emptyUiState
    length erosion `shouldBe` configRowCount ConfigErosion emptyUiState

  it "keeps slider row widgets aligned to dense live row rects within each tab" $ do
    let layout = layoutFor (V2 800 960) 0
        (terrain, planet, climate, weather, biome, erosion) = buildSliderRowWidgets layout
    assertDenseRowRects layout terrain
    assertDenseRowRects layout planet
    assertDenseRowRects layout climate
    assertDenseRowRects layout weather
    assertDenseRowRects layout biome
    assertDenseRowRects layout erosion

  it "preserves representative slider row order per tab" $ do
    let layout = layoutFor (V2 800 960) 0
        (terrain, planet, climate, weather, biome, erosion) = buildSliderRowWidgets layout
    map widgetId (take 3 terrain)
      `shouldBe` [WidgetSliderMinus SliderGenScale, WidgetSliderMinus SliderGenCoordScale, WidgetSliderMinus SliderGenOffsetX]
    map widgetId (take 3 planet)
      `shouldBe` [WidgetSliderMinus SliderPlanetRadius, WidgetSliderMinus SliderAxialTilt, WidgetSliderMinus SliderInsolation]
    map widgetId (take 3 climate)
      `shouldBe` [WidgetSliderMinus SliderWaterLevel, WidgetSliderMinus SliderOrographicLift, WidgetSliderMinus SliderRainShadowLoss]
    map widgetId (take 3 weather)
      `shouldBe` [WidgetSliderMinus SliderWeatherTick, WidgetSliderMinus SliderWeatherPhase, WidgetSliderMinus SliderWeatherAmplitude]
    map widgetId (take 3 biome)
      `shouldBe` [WidgetSliderMinus SliderVegBase, WidgetSliderMinus SliderVegBoost, WidgetSliderMinus SliderVegTempWeight]
    map widgetId (take 3 erosion)
      `shouldBe` [WidgetSliderMinus SliderErosionHydraulic, WidgetSliderMinus SliderErosionThermal, WidgetSliderMinus SliderErosionRainRate]

  it "anchors slider row hit tests to live row rects for each tab" $ do
    let layout = layoutFor (V2 800 960) 0
        (terrain, planet, climate, weather, biome, erosion) = buildSliderRowWidgets layout
        rowHit = rectHitPoint (configParamRowHitRect (configParamRects 0 layout))
    hitTest terrain rowHit `shouldBe` Just (WidgetSliderMinus SliderGenScale)
    hitTest planet rowHit `shouldBe` Just (WidgetSliderMinus SliderPlanetRadius)
    hitTest climate rowHit `shouldBe` Just (WidgetSliderMinus SliderWaterLevel)
    hitTest weather rowHit `shouldBe` Just (WidgetSliderMinus SliderWeatherTick)
    hitTest biome rowHit `shouldBe` Just (WidgetSliderMinus SliderVegBase)
    hitTest erosion rowHit `shouldBe` Just (WidgetSliderMinus SliderErosionHydraulic)

  it "anchors plugin and simulation widgets to bespoke pipeline row helpers" $ do
    let layout = layoutFor (V2 800 960) 0
        pluginNames = map Text.pack ["plugin-a", "plugin-b"]
        widgets = buildPluginWidgets pluginNames Map.empty Map.empty layout
        builtinCount = length allBuiltinStageIds
        simBase = builtinCount + length pluginNames
    hitTest widgets (rectHitPoint (pipelineMoveUpRect builtinCount layout))
      `shouldBe` Just (WidgetPluginMoveUp (Text.pack "plugin-a"))
    hitTest widgets (rectHitPoint (pipelineMoveDownRect builtinCount layout))
      `shouldBe` Just (WidgetPluginMoveDown (Text.pack "plugin-a"))
    hitTest widgets (rectHitPoint (pipelineMoveUpRect (builtinCount + 1) layout))
      `shouldBe` Just (WidgetPluginMoveUp (Text.pack "plugin-b"))
    hitTest widgets (rectHitPoint (pipelineMoveDownRect (builtinCount + 1) layout))
      `shouldBe` Just (WidgetPluginMoveDown (Text.pack "plugin-b"))
    hitTest widgets (rectHitPoint (pipelineTickButtonRect simBase layout))
      `shouldBe` Just WidgetSimTick
    hitTest widgets (rectHitPoint (pipelineCheckboxRect (simBase + 1) layout))
      `shouldBe` Just WidgetSimAutoTick

  it "hit tests editor toolbar tool buttons" $ do
    let layout = layoutFor (V2 1200 800) 160
        widgets = buildEditorWidgets layout ToolRaise
    hitTest widgets (rectHitPoint (editorToolButtonRect 0 layout))
      `shouldBe` Just (WidgetEditorTool 0)
    hitTest widgets (rectHitPoint (editorToolButtonRect 3 layout))
      `shouldBe` Just (WidgetEditorTool 3)
    hitTest widgets (rectHitPoint (editorToolButtonRect (editorToolButtonCount - 1) layout))
      `shouldBe` Just (WidgetEditorTool (editorToolButtonCount - 1))

  it "hit tests editor radius and close buttons" $ do
    let layout = layoutFor (V2 1200 800) 160
        widgets = buildEditorWidgets layout ToolRaise
    hitTest widgets (rectHitPoint (editorRadiusMinusRect layout))
      `shouldBe` Just WidgetEditorRadiusMinus
    hitTest widgets (rectHitPoint (editorRadiusPlusRect layout))
      `shouldBe` Just WidgetEditorRadiusPlus
    hitTest widgets (rectHitPoint (editorCloseRect layout))
      `shouldBe` Just WidgetEditorClose

  it "builds correct number of editor widgets" $ do
    let layout = layoutFor (V2 1200 800) 160
        widgets = buildEditorWidgets layout ToolRaise
    -- 9 tool buttons + 3 (radius−, radius+, close)
    -- + 4 param bar widgets (2 numeric slots for ToolRaise)
    -- + 2 falloff widgets (ToolRaise has falloff)
    length widgets `shouldBe` 18

  it "hit tests editor reopen button" $ do
    let layout = layoutFor (V2 1200 800) 160
        widgets = buildEditorReopenWidget layout
    length widgets `shouldBe` 1
    hitTest widgets (rectHitPoint (editorReopenRect layout))
      `shouldBe` Just WidgetEditorReopen

assertSliderButtons :: [Widget] -> Layout -> (Int, WidgetId, WidgetId) -> Expectation
assertSliderButtons widgets layout (rowIndex, minusWidgetId, plusWidgetId) = do
  let rects = configParamRects rowIndex layout
      minusRect = configParamRowMinusRect rects
      plusRect = configParamRowPlusRect rects
  hitTest widgets (rectHitPoint minusRect) `shouldBe` Just minusWidgetId
  hitTest widgets (rectHitPoint plusRect) `shouldBe` Just plusWidgetId

assertDenseRowRects :: Layout -> [Widget] -> Expectation
assertDenseRowRects layout widgets =
  map widgetRect widgets `shouldBe`
    map (configParamRowHitRect . (`configParamRects` layout)) [0 .. length widgets - 1]
