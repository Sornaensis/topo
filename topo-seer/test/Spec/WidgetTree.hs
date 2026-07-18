{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.WidgetTree (spec) where

import Actor.UI
  ( ConfigTab(..)
  , DataBrowserState(..)
  , UiMenuMode(..)
  , UiState(..)
  , configRowCount
  , emptyDataBrowserState
  , emptyUiState
  )
import Data.Aeson (Value(..))
import Data.List (nub)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Linear (V2(..))
import Seer.DataBrowser.Model
  ( DataBrowserAsyncError(..)
  , DataBrowserOperation(..)
  , DataBrowserPendingEnvelope(..)
  , DataBrowserPendingRequest(..)
  , DataBrowserRequestId(..)
  , DataBrowserWorkerRequest(..)
  )
import Seer.Command.Handlers.Widgets
  ( WidgetCapability(..)
  , WidgetClickSupport(..)
  , dataBrowserWidgetIds
  , widgetCapabilities
  , widgetCapability
  , widgetState
  )
import Seer.Config.SliderSpec (SliderId(..))
import Seer.Config.Snapshot (presetCatalogueMatches)
import Seer.Draw.Dialog (presetListLabel)
import Seer.Editor.Types (EditorState(..), EditorTool(..))
import Seer.OverlayInspector.Model
import Seer.World.Persist.Types (WorldSaveManifest(..))
import Test.Hspec
import Topo.Pipeline.Stage (allBuiltinStageIds)
import Topo.Plugin.DataResource
  ( DataConstructorDef(..)
  , DataFieldDef(..)
  , DataFieldType(..)
  , DataOperations(..)
  , DataPagination(..)
  , DataResourceSchema(..)
  , allOperations
  , currentDataResourceSchemaVersion
  , defaultDataResourceVersion
  )
import Topo.Plugin.RPC.DataService (DataRecord(..))
import Topo.Plugin.RPC.Manifest (RPCParamSpec(..), RPCParamType(..))
import UI.Layout
import UI.WidgetId (widgetIdFromText, widgetIdToText)
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
  it "labels and filters built-ins through catalogue display metadata" $ do
    presetListLabel "builtin:large-ocean" `shouldBe` "Large Ocean [built-in]"
    presetListLabel "My saved preset" `shouldBe` "My saved preset"
    presetCatalogueMatches "large ocean" "builtin:large-ocean" `shouldBe` True
    presetCatalogueMatches "built-in" "builtin:large-ocean" `shouldBe` True

  it "round-trips canonical widget IDs with adversarial text arguments" $ do
    let ids =
          [ WidgetGenerate
          , WidgetSliderMinus SliderWaterLevel
          , WidgetPluginExpand "plugin:with:%:separators"
          , WidgetPluginParamSlider "" "density:μ"
          , WidgetDataResourceSelect "@catalog" ""
          , WidgetDataFieldToggle "profile:name:%"
          , WidgetEditorTool 8
          , WidgetOverlayInspectorItem 7
          ]
        encoded = map widgetIdToText ids
    map widgetIdFromText encoded `shouldBe` map Just ids
    encoded `shouldSatisfy` any (Text.isPrefixOf "@1:")
    widgetIdFromText "WidgetPluginParamSlider:legacy:param"
      `shouldBe` Just (WidgetPluginParamSlider "legacy" "param")
    widgetIdFromText "WidgetPluginExpand:@1foo"
      `shouldBe` Just (WidgetPluginExpand "@1foo")
    widgetIdFromText "WidgetPluginExpand:@11#a"
      `shouldBe` Just (WidgetPluginExpand "@11#a")
    widgetIdFromText "WidgetViewElevation" `shouldBe` Nothing

  it "derives a unique, codec-round-trippable active inventory" $ do
    let widgets = buildActiveWidgets emptyUiState wideWidgetLayout
        ids = map widgetId widgets
    length ids `shouldBe` length (nub ids)
    map (widgetIdFromText . widgetIdToText) ids `shouldBe` map Just ids
    map widgetIdToText ids `shouldNotContain` ["WidgetViewElevation"]

  it "advertises exactly the live slider controls for all six slider tabs" $ do
    let tabs =
          [ ConfigTerrain, ConfigPlanet, ConfigClimate
          , ConfigWeather, ConfigBiome, ConfigErosion
          ]
        sliderIdsFor tab =
          [ wid
          | Widget wid _ <- buildActiveWidgets
              emptyUiState { uiShowConfig = True, uiConfigTab = tab }
              wideWidgetLayout
          , isSliderWidget wid
          ]
    map (length . sliderIdsFor) tabs
      `shouldBe` [2 * configRowCount tab emptyUiState | tab <- tabs]

  it "includes expanded plugin parameters and all tool-dependent editor controls" $ do
    let pipelineUi = emptyUiState
          { uiShowConfig = True
          , uiConfigTab = ConfigPipeline
          , uiPluginNames = ["plugin:one"]
          , uiPluginExpanded = Map.singleton "plugin:one" True
          , uiPluginParamSpecs = Map.singleton "plugin:one" [boolParamSpec, numericParamSpec]
          }
        pipelineIds = map widgetId (buildActiveWidgets pipelineUi wideWidgetLayout)
    pipelineIds `shouldContain`
      [ WidgetPluginParamCheck "plugin:one" "enabled"
      , WidgetPluginParamSlider "plugin:one" "density:value"
      ]
    mapM_ (assertEditorSurface wideWidgetLayout) [minBound .. maxBound]

  it "treats every menu as a background-input barrier" $ do
    let idsFor mode state = map widgetId $ buildActiveWidgets
          state { uiMenuMode = mode } wideWidgetLayout
    idsFor MenuEscape emptyUiState
      `shouldBe` [WidgetMenuSave, WidgetMenuLoad, WidgetMenuExit]
    idsFor MenuPresetSave emptyUiState
      `shouldBe` [WidgetPresetSaveOk, WidgetPresetSaveCancel]
    idsFor MenuPresetLoad emptyUiState { uiPresetList = ["alpha"] }
      `shouldBe` [WidgetPresetLoadOk, WidgetPresetLoadCancel, WidgetPresetLoadItem]
    idsFor MenuWorldSave emptyUiState
      `shouldBe` [WidgetWorldSaveOk, WidgetWorldSaveCancel]
    idsFor MenuWorldLoad emptyUiState
      `shouldBe` [WidgetWorldLoadOk, WidgetWorldLoadCancel]
    let inspector = openOverlayInspectorView OverlayInspectorExportView
          emptyOverlayInspectorModel
            { oimSelectedOverlay = Just "roads"
            , oimExportPayload = Just (Object mempty)
            }
        overlayUi = emptyUiState
          { uiMenuMode = MenuOverlayInspector
          , uiOverlayInspector = inspector
          }
    idsFor MenuOverlayInspector overlayUi `shouldBe`
      [ WidgetOverlayInspectorClose
      , WidgetOverlayInspectorCopy
      , WidgetOverlayInspectorSave
      ]
    map (wcSupport . widgetCapability overlayUi)
      [WidgetOverlayInspectorCopy, WidgetOverlayInspectorSave]
      `shouldBe` [WidgetClickable, WidgetClickable]

  it "exposes manager rows and import controls through modal hit testing" $ do
    let manager = openOverlayInspectorView OverlayInspectorManagerView
          emptyOverlayInspectorModel { oimOverlayNames = ["roads", "climate"] }
        managerWidgets = buildOverlayInspectorWidgets manager wideWidgetLayout
    hitTest managerWidgets (rectHitPoint (overlayInspectorRowRect 1 wideWidgetLayout))
      `shouldBe` Just (WidgetOverlayInspectorItem 1)
    let longManager = manager
          { oimOverlayNames = ["overlay-" <> Text.pack (show index) | index <- [0 :: Int .. 30]]
          , oimScroll = 20
          }
        longIds = map widgetId (buildOverlayInspectorWidgets longManager wideWidgetLayout)
    longIds `shouldContain` [WidgetOverlayInspectorItem 20]
    longIds `shouldNotContain` [WidgetOverlayInspectorItem 0, WidgetOverlayInspectorItem 19]
    length longIds `shouldSatisfy` (< 20)
    let importer = openOverlayInspectorView OverlayInspectorImportView manager
        importWidgets = buildOverlayInspectorWidgets importer wideWidgetLayout
    map widgetId importWidgets `shouldBe`
      [ WidgetOverlayInspectorClose
      , WidgetOverlayInspectorImportInput
      , WidgetOverlayInspectorValidate
      ]

  it "exposes saved-world deletion only for a valid selection and barriers confirmation" $ do
    let manifest = WorldSaveManifest
          "alpha" 1 64 (read "2025-01-01 00:00:00 UTC") 0 [] [] [] []
        base = emptyUiState
          { uiMenuMode = MenuWorldLoad
          , uiWorldList = [manifest]
          , uiWorldSelected = 0
          }
        ids state = map widgetId (buildActiveWidgets state wideWidgetLayout)
    ids base `shouldBe`
      [ WidgetWorldLoadOk
      , WidgetWorldLoadCancel
      , WidgetWorldDelete
      , WidgetWorldLoadItem
      ]
    ids base { uiWorldSelected = 1 }
      `shouldBe` [WidgetWorldLoadOk, WidgetWorldLoadCancel, WidgetWorldLoadItem]
    ids base { uiWorldFilter = "missing" }
      `shouldBe` [WidgetWorldLoadOk, WidgetWorldLoadCancel]
    let confirming = base
          { uiWorldDeleteConfirm = True
          , uiWorldDeleteTarget = Just "alpha"
          }
    ids confirming
      `shouldBe` [WidgetWorldDeleteConfirm, WidgetWorldDeleteCancel]
    hitTest (buildMenuWidgets base wideWidgetLayout)
      (rectHitPoint (worldLoadDeleteRect wideWidgetLayout))
      `shouldBe` Just WidgetWorldDelete
    hitTest (buildMenuWidgets confirming wideWidgetLayout)
      (rectHitPoint (worldDeleteConfirmOkRect wideWidgetLayout))
      `shouldBe` Just WidgetWorldDeleteConfirm
    let deleteCapability = widgetCapability base WidgetWorldDelete
        invalidDelete = widgetCapability (base { uiWorldSelected = 1 }) WidgetWorldDelete
    wcVisible deleteCapability `shouldBe` True
    wcEnabled deleteCapability `shouldBe` True
    wcSupport deleteCapability `shouldBe` WidgetClickable
    wcVisible invalidDelete `shouldBe` False

  it "treats Data Browser delete confirmation as a full widget barrier" $ do
    let dbs = browserState
          { dbsSelectedRowIndex = Just 0
          , dbsDeleteConfirm = True
          }
        ids = map widgetId (buildActiveWidgets (browserUi dbs) wideWidgetLayout)
    ids `shouldBe` [WidgetDataDeleteConfirm, WidgetDataDeleteCancel]
    let hiddenIds = map widgetId $ buildActiveWidgets
          (browserUi dbs) { uiShowConfig = False } wideWidgetLayout
    hiddenIds `shouldNotContain` [WidgetDataDeleteConfirm, WidgetDataDeleteCancel]
    hiddenIds `shouldContain` [WidgetLeftToggle]

  it "reports capability support and representative state preconditions truthfully" $ do
    let basis = widgetCapability emptyUiState WidgetViewBasisAverage
        pluginUi = emptyUiState
          { uiShowConfig = True
          , uiConfigTab = ConfigPipeline
          , uiPluginNames = ["first", "last"]
          , uiPluginParams = Map.singleton "first" (Map.singleton "enabled" (Bool True))
          }
        firstUp = widgetCapability pluginUi (WidgetPluginMoveUp "first")
        lastDown = widgetCapability pluginUi (WidgetPluginMoveDown "last")
        pluginSlider = widgetCapability pluginUi (WidgetPluginParamSlider "first" "density")
        pluginCheck = widgetCapability pluginUi (WidgetPluginParamCheck "first" "enabled")
        simTick = widgetCapability pluginUi WidgetSimTick
        smoothEditor = (uiEditor emptyUiState)
          { editorActive = True, editorTool = ToolSmooth, editorSmoothPasses = 1 }
        smoothMinus = widgetCapability
          emptyUiState { uiEditor = smoothEditor }
          (WidgetEditorParamMinus 0)
        pending = DataBrowserPendingEnvelope
          (DataBrowserRequestId 90)
          DataBrowserLoadCatalogOperation
          DataBrowserLoadCatalogRequest
        pendingDataTab = widgetCapability
          emptyUiState { uiDataBrowser = emptyDataBrowserState
            { dbsPendingRequest = Just pending, dbsLoading = True } }
          WidgetConfigTabData
    wcEnabled basis `shouldBe` False
    wcPreconditions basis `shouldContain` ["weather basis requires an active built-in weather overlay"]
    wcEnabled firstUp `shouldBe` False
    wcEnabled lastDown `shouldBe` False
    wcSupport pluginSlider `shouldBe` WidgetArgumentRequired
    wcAlternative pluginSlider `shouldBe` Just "set_plugin_param"
    wcActive pluginCheck `shouldBe` Just True
    wcEnabled simTick `shouldBe` False
    wcEnabled smoothMinus `shouldBe` False
    wcPreconditions smoothMinus `shouldContain` ["editor parameter is at its minimum"]
    wcEnabled pendingDataTab `shouldBe` False
    wcPreconditions pendingDataTab `shouldContain` ["a Data Browser request is already pending"]
    map wcWidgetId (widgetCapabilities pluginUi)
      `shouldBe` map widgetId (buildActiveWidgets pluginUi wideWidgetLayout)

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

  it "classifies canonical layered View widget ids with the View tab" $ do
    let ids =
          [ WidgetViewBaseElevation, WidgetViewBaseBiome, WidgetViewBaseMoisture
          , WidgetViewOverlayNone, WidgetViewOverlayTemperature
          , WidgetViewOverlayPrecipitation, WidgetViewOverlayCloud
          , WidgetViewBasisAverage, WidgetViewBasisCurrent
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

  it "keeps only same/different resource replacement navigation during reads" $ do
    let layout = layoutFor (V2 800 960) 0
        dbs = browserState
          { dbsPendingRequest = Just (DataBrowserPendingEnvelope
              (DataBrowserRequestId 2)
              DataBrowserListOperation
              (DataBrowserRecordRequest
                (DataBrowserListRecordsRequest "atlas" "cities" (Just 10) (Just 10))))
          , dbsLoading = True
          }
        ids = map widgetId (buildDataBrowserWidgets browserResources dbs layout)
    ids `shouldContain`
      [ WidgetDataResourceSelect "atlas" "cities"
      , WidgetDataResourceSelect "atlas" "towns"
      ]
    ids `shouldNotContain` [WidgetDataRecordSelect 0]
    ids `shouldNotContain` [WidgetDataPageNext "atlas" "cities"]
    ids `shouldNotContain` [WidgetDataCreateNew]

  it "removes every Data Browser hit target during mutations" $ do
    let layout = layoutFor (V2 800 960) 0
        request = DataBrowserRecordRequest
          (DataBrowserCreateRecordRequest "atlas" "cities" (DataRecord Map.empty))
        dbs = browserState
          { dbsCreateMode = True
          , dbsPendingRequest = Just (DataBrowserPendingEnvelope
              (DataBrowserRequestId 3) DataBrowserCreateOperation request)
          , dbsLoading = True
          }
    buildDataBrowserWidgets browserResources dbs layout `shouldBe` []
    buildDataDetailWidgets 2 [] Set.empty 1 True False False False True layout
      `shouldBe` []

  it "keeps command widget listings exact across every async operation class" $ do
    let readOperations =
          [ (DataBrowserLoadCatalogOperation, DataBrowserLoadCatalogRequest)
          , (DataBrowserLoadPluginOperation, DataBrowserLoadPluginRequest "atlas")
          , (DataBrowserSelectResourceOperation, DataBrowserSelectResourceRequest "atlas" "cities")
          , (DataBrowserListOperation, DataBrowserRecordRequest
              (DataBrowserListRecordsRequest "atlas" "cities" (Just 10) (Just 10)))
          ]
        mutationOperations =
          [ (DataBrowserCreateOperation, DataBrowserRecordRequest
              (DataBrowserCreateRecordRequest "atlas" "cities" (DataRecord Map.empty)))
          , (DataBrowserUpdateOperation, DataBrowserRecordRequest
              (DataBrowserUpdateRecordRequest "atlas" "cities" Null (DataRecord Map.empty)))
          , (DataBrowserDeleteOperation, DataBrowserRecordRequest
              (DataBrowserDeleteRecordRequest "atlas" "cities" Null))
          ]
        idsFor (requestId, (operation, request)) =
          dataBrowserWidgetIds (browserUi browserState
            { dbsPendingRequest = Just (DataBrowserPendingEnvelope
                (DataBrowserRequestId requestId) operation request)
            , dbsLoading = True
            })
    map idsFor (zip [10..] readOperations) `shouldSatisfy` all (\ids ->
      widgetIdToText (WidgetDataResourceSelect "atlas" "cities") `elem` ids
        && widgetIdToText (WidgetDataResourceSelect "atlas" "towns") `elem` ids
        && "WidgetDataRecordSelect:0" `notElem` ids
        && "WidgetDataCreateNew" `notElem` ids)
    map idsFor (zip [20..] mutationOperations) `shouldBe` [[], [], []]

  it "lists nested edit widgets and exposes pending/error command state" $ do
    let editDbs = browserState
          { dbsSelectedRecord = Just (DataRecord (Map.singleton "name" (String "Alpha")))
          , dbsSelectedRowIndex = Just 0
          , dbsEditMode = True
          , dbsEditValues = Map.singleton "name" (String "Alpha")
          }
    let editWidgetIds = dataBrowserWidgetIds (browserUi editDbs)
    editWidgetIds `shouldContain` [widgetIdToText (WidgetDataFieldTextClick "name")]
    editWidgetIds `shouldContain` ["WidgetDataEditSave"]

    let pending = DataBrowserPendingEnvelope
          (DataBrowserRequestId 42)
          DataBrowserLoadPluginOperation
          (DataBrowserLoadPluginRequest "atlas")
        pendingState = widgetState
          (browserUi browserState { dbsPendingRequest = Just pending, dbsLoading = True })
          WidgetConfigTabData
    case pendingState of
      Object fields -> do
        KM.lookup "loading" fields `shouldBe` Just (Bool True)
        case KM.lookup "pending" fields of
          Just (Object pendingFields) -> do
            KM.lookup "request_id" pendingFields `shouldBe` Just (Number 42)
            KM.lookup "operation" pendingFields `shouldBe` Just (String "load_plugin_resources")
            KM.lookup "target" pendingFields `shouldSatisfy` maybe False (\case Object _ -> True; _ -> False)
          value -> expectationFailure ("expected pending object, got " <> show value)
      value -> expectationFailure ("expected widget state object, got " <> show value)

    let asyncError = DataBrowserAsyncError
          (DataBrowserRequestId 43)
          DataBrowserLoadCatalogOperation
          DataBrowserLoadCatalogRequest
          "catalog unavailable"
        errorState = widgetState
          (browserUi browserState { dbsAsyncError = Just asyncError })
          WidgetConfigTabData
    case errorState of
      Object fields -> KM.lookup "async_error" fields
        `shouldSatisfy` maybe False (\case Object _ -> True; _ -> False)
      value -> expectationFailure ("expected widget state object, got " <> show value)

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
          False
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
    -- + 2 param bar widgets (1 numeric slot for ToolRaise)
    -- + 2 falloff widgets (ToolRaise has falloff)
    length widgets `shouldBe` 16

  it "hit tests editor reopen button" $ do
    let layout = layoutFor (V2 1200 800) 160
        widgets = buildEditorReopenWidget layout
    length widgets `shouldBe` 1
    hitTest widgets (rectHitPoint (editorReopenRect layout))
      `shouldBe` Just WidgetEditorReopen

wideWidgetLayout :: Layout
wideWidgetLayout = layoutFor (V2 1200 900) 0

isSliderWidget :: WidgetId -> Bool
isSliderWidget wid = case wid of
  WidgetSliderMinus _ -> True
  WidgetSliderPlus _ -> True
  _ -> False

isEditorWidget :: WidgetId -> Bool
isEditorWidget wid = case wid of
  WidgetEditorTool _ -> True
  WidgetEditorRadiusMinus -> True
  WidgetEditorRadiusPlus -> True
  WidgetEditorClose -> True
  WidgetEditorReopen -> True
  WidgetEditorParamMinus _ -> True
  WidgetEditorParamPlus _ -> True
  WidgetEditorCyclePrev _ -> True
  WidgetEditorCycleNext _ -> True
  WidgetEditorFalloffPrev -> True
  WidgetEditorFalloffNext -> True
  _ -> False

assertEditorSurface :: Layout -> EditorTool -> Expectation
assertEditorSurface layout tool = do
  let editor = (uiEditor emptyUiState) { editorActive = True, editorTool = tool }
      actual =
        [ wid
        | Widget wid _ <- buildActiveWidgets emptyUiState { uiEditor = editor } layout
        , isEditorWidget wid
        ]
      expected = map widgetId (buildEditorWidgets layout tool)
  actual `shouldBe` expected

boolParamSpec :: RPCParamSpec
boolParamSpec = RPCParamSpec
  { rpsName = "enabled"
  , rpsLabel = "Enabled"
  , rpsType = ParamBool
  , rpsRange = Nothing
  , rpsDefault = Bool False
  , rpsTooltip = ""
  }

numericParamSpec :: RPCParamSpec
numericParamSpec = RPCParamSpec
  { rpsName = "density:value"
  , rpsLabel = "Density"
  , rpsType = ParamFloat
  , rpsRange = Just (Number 0, Number 1)
  , rpsDefault = Number 0.5
  , rpsTooltip = ""
  }

browserState :: DataBrowserState
browserState = emptyDataBrowserState
  { dbsSelectedPlugin = Just "atlas"
  , dbsSelectedResource = Just "cities"
  , dbsRecords = [DataRecord Map.empty]
  }

browserUi :: DataBrowserState -> UiState
browserUi dbs = emptyUiState
  { uiShowConfig = True
  , uiConfigTab = ConfigData
  , uiDataResources = browserResources
  , uiDataBrowser = dbs
  }

browserResources :: Map.Map Text.Text [DataResourceSchema]
browserResources = Map.singleton "atlas" [browserSchema "cities", browserSchema "towns"]

browserSchema :: Text.Text -> DataResourceSchema
browserSchema name = DataResourceSchema
  { drsSchemaVersion = currentDataResourceSchemaVersion
  , drsResourceVersion = defaultDataResourceVersion
  , drsName = name
  , drsLabel = name
  , drsHexBound = False
  , drsKeyField = "id"
  , drsOverlay = Nothing
  , drsFields = [DataFieldDef "name" DFText "Name" True Nothing]
  , drsOperations = allOperations { doCreate = True, doPage = True }
  , drsPagination = DataPagination 10 100 0
  }

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
