{-# LANGUAGE OverloadedStrings #-}

module Spec.WidgetTree (spec) where

import Actor.UI (ConfigTab(..), configRowCount, emptyUiState)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Linear (V2(..))
import Seer.Config.SliderSpec (SliderId(..))
import Seer.Editor.Types (EditorTool(..))
import Test.Hspec
import Topo.Pipeline.Stage (allBuiltinStageIds)
import Topo.Plugin.DataResource (DataConstructorDef(..), DataFieldDef(..), DataFieldType(..))
import UI.Layout
import UI.WidgetTree
import UI.Widgets (Rect(..), containsPoint)

rectHitPoint :: Rect -> V2 Int
rectHitPoint (Rect (V2 x y, V2 w h)) = V2 (x + w `div` 2) (y + h `div` 2)

rectTop :: Rect -> Int
rectTop (Rect (V2 _ y, _)) = y

rectBottom :: Rect -> Int
rectBottom (Rect (V2 _ y, V2 _ h)) = y + h

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

  it "hit tests a base view button" $ do
    let layout = layoutFor (V2 800 600) 160
        widgets = buildViewModeWidgets layout 0
        baseRects = leftBaseViewRects layout
        moistureRect = baseRects !! 2
    hitTest widgets (rectHitPoint moistureRect) `shouldBe` Just WidgetViewBaseMoisture

  it "orders base, weather overlay, and basis controls in the View widget tree" $ do
    let layout = layoutFor (V2 800 1200) 160
        viewIds = map widgetId (buildViewModeWidgets layout 0)
    take 12 viewIds `shouldBe`
      [ WidgetViewBaseElevation, WidgetViewBaseBiome, WidgetViewBaseMoisture
      , WidgetViewBaseVegetation, WidgetViewBaseTerrainForm, WidgetViewBasePlateId
      , WidgetViewBasePlateBoundary, WidgetViewBasePlateHardness, WidgetViewBasePlateCrust
      , WidgetViewBasePlateAge, WidgetViewBasePlateHeight, WidgetViewBasePlateVelocity
      ]
    take 4 (drop 12 viewIds) `shouldBe`
      [ WidgetViewOverlayNone, WidgetViewOverlayTemperature
      , WidgetViewOverlayPrecipitation, WidgetViewOverlayCloud
      ]
    drop 16 viewIds `shouldBe`
      [ WidgetViewBasisAverage, WidgetViewBasisCurrent ]

  it "lays out layered View tab sections in scroll order" $ do
    let layout = layoutFor (V2 800 1200) 160
        baseRects = leftBaseViewRects layout
        overlayRects = leftWeatherOverlayRects layout
        basisRects = leftWeatherBasisRects layout
        dayNightRect = dayNightToggleRect layout
        (pluginPrevRect, _pluginNextRect, _fieldPrevRect, _fieldNextRect) = overlayViewRects layout
        actionRects = overlayActionRects layout
        sectionTops = map rectTop
          [ head baseRects
          , head overlayRects
          , head basisRects
          , dayNightRect
          , pluginPrevRect
          , head actionRects
          ]
    sectionTops `shouldSatisfy` strictlyIncreasing
    rectBottom (last actionRects) - leftControlsTop layout
      `shouldSatisfy` (<= leftViewContentHeight layout)

  it "hit tests all layered View tab selector buttons" $ do
    let layout = layoutFor (V2 800 1200) 160
        widgets = buildViewModeWidgets layout 0
        selectorRects = leftBaseViewRects layout ++ leftWeatherOverlayRects layout ++ leftWeatherBasisRects layout
        expectedIds =
          [ WidgetViewBaseElevation, WidgetViewBaseBiome, WidgetViewBaseMoisture
          , WidgetViewBaseVegetation, WidgetViewBaseTerrainForm, WidgetViewBasePlateId
          , WidgetViewBasePlateBoundary, WidgetViewBasePlateHardness, WidgetViewBasePlateCrust
          , WidgetViewBasePlateAge, WidgetViewBasePlateHeight, WidgetViewBasePlateVelocity
          , WidgetViewOverlayNone, WidgetViewOverlayTemperature
          , WidgetViewOverlayPrecipitation, WidgetViewOverlayCloud
          , WidgetViewBasisAverage, WidgetViewBasisCurrent
          ]
    length selectorRects `shouldBe` 18
    mapM_ (\(rect, wid) -> hitTest widgets (rectHitPoint rect) `shouldBe` Just wid)
      (zip selectorRects expectedIds)

  it "clips scrolled left View hits to content below chrome" $ do
    let layout = layoutFor (V2 800 360) 80
        scrollY = leftViewScrollMax layout
        viewWidgets = filter (isLeftViewWidget . widgetId) (buildWidgets layout)
        clipR = leftViewContentClipRect layout
        candidates =
          [ p
          | Widget _ rect <- viewWidgets
          , let V2 px py = rectHitPoint rect
                p = V2 px (py - scrollY)
          , containsPoint (leftPanelRect layout) p
          , not (containsPoint clipR p)
          ]
    scrollY `shouldSatisfy` (> 0)
    case candidates of
      [] -> expectationFailure "expected a scrolled View widget candidate above the content clip"
      p : _ -> do
        let adjusted = p + V2 0 scrollY
        hitTest viewWidgets adjusted `shouldSatisfy` (/= Nothing)
        (if containsPoint clipR p then hitTest viewWidgets adjusted else Nothing) `shouldBe` Nothing

  it "keeps bottom View controls reachable after scrolling" $ do
    let layout = layoutFor (V2 800 360) 80
        scrollY = leftViewScrollMax layout
        viewWidgets = filter (isLeftViewWidget . widgetId) (buildWidgets layout)
        screenPoint = rectHitPoint (last (overlayActionRects layout)) - V2 0 scrollY
        contentPoint = screenPoint + V2 0 scrollY
    scrollY `shouldSatisfy` (> 0)
    containsPoint (leftViewContentClipRect layout) screenPoint `shouldBe` True
    hitTest viewWidgets contentPoint `shouldBe` Just WidgetOverlayImportValidate

  it "keeps legacy View widget ids classified with the View tab" $ do
    let ids =
          [ WidgetViewElevation, WidgetViewBiome, WidgetViewClimate, WidgetViewWeather
          , WidgetViewMoisture, WidgetViewPrecip, WidgetViewPrecipCurrent
          , WidgetViewVegetation, WidgetViewTerrainForm, WidgetViewPlateId
          , WidgetViewPlateBoundary, WidgetViewPlateHardness, WidgetViewPlateCrust
          , WidgetViewPlateAge, WidgetViewPlateHeight, WidgetViewPlateVelocity
          , WidgetViewCloud, WidgetViewCloudTypical
          ]
    map isLeftViewWidget ids `shouldBe` replicate (length ids) True

  it "hit tests overlay action buttons" $ do
    let layout = layoutFor (V2 800 1200) 160
        widgets = buildWidgets layout
        expectedIds =
          [ WidgetOverlayManager
          , WidgetOverlaySchema
          , WidgetOverlayProvenance
          , WidgetOverlayExport
          , WidgetOverlayImportValidate
          ]
    mapM_ (\(rect, wid) -> hitTest widgets (rectHitPoint rect) `shouldBe` Just wid)
      (zip (overlayActionRects layout) expectedIds)

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
        widgets = buildPluginWidgets pluginNames Map.empty Map.empty Map.empty layout
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

  it "aligns data detail widgets to validation-adjusted popover geometry" $ do
    let layout = layoutFor (V2 1200 800) 160
        rowIndex = 2
        validationRows = 2
        rowCount = detailWidgetFieldCount + validationRows
        widgets = buildDataDetailWidgets
          rowIndex
          detailWidgetFields
          detailWidgetExpanded
          validationRows
          True
          True
          False
          False
          layout
    hitTest widgets (rectHitPoint (dataDetailSaveRect rowIndex rowCount layout))
      `shouldBe` Just WidgetDataEditSave

  it "emits delete confirmation dialog widgets before the popover dismiss hit area" $ do
    let layout = layoutFor (V2 1200 800) 160
        rowIndex = 2
        widgets = buildDataDetailWidgets
          rowIndex
          detailWidgetFields
          detailWidgetExpanded
          1
          False
          True
          True
          True
          layout
    hitTest widgets (rectHitPoint (deleteConfirmOkRect layout))
      `shouldBe` Just WidgetDataDeleteConfirm
    hitTest widgets (rectHitPoint (deleteConfirmCancelRect layout))
      `shouldBe` Just WidgetDataDeleteCancel

  it "uses ADT positional field types for data detail edit widgets" $ do
    let layout = layoutFor (V2 1200 800) 160
        rowIndex = 2
        circleTextIndex = 6
        widgets = buildDataDetailWidgets
          rowIndex
          detailWidgetFields
          detailWidgetExpanded
          0
          True
          True
          False
          False
          layout
    hitTest widgets (rectHitPoint (dataDetailFieldInputRect rowIndex detailWidgetFieldCount circleTextIndex layout))
      `shouldBe` Just (WidgetDataFieldTextClick "shape.Circle.1")

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

detailWidgetExpanded :: Set.Set Text.Text
detailWidgetExpanded = Set.fromList ["profile", "shape"]

detailWidgetFieldCount :: Int
detailWidgetFieldCount = 10

detailWidgetFields :: [DataFieldDef]
detailWidgetFields =
  [ DataFieldDef "id" DFInt "ID" False Nothing
  , DataFieldDef "profile" (DFRecord
      [ DataFieldDef "age" DFInt "Age" True Nothing
      , DataFieldDef "name" DFText "Name" True Nothing
      ]) "Profile" True Nothing
  , DataFieldDef "shape" (DFAdt
      [ DataConstructorDef "Circle" [DFFloat, DFText]
      , DataConstructorDef "Point" [DFInt, DFInt]
      ]) "Shape" True Nothing
  , DataFieldDef "active" DFBool "Active" True Nothing
  ]

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

strictlyIncreasing :: [Int] -> Bool
strictlyIncreasing values = and (zipWith (<) values (drop 1 values))
