{-# LANGUAGE BangPatterns #-}
module UI.WidgetTree
  ( WidgetId(..)
  , Widget(..)
  , buildWidgets
  , buildEditorWidgets
  , buildEditorReopenWidget
  , buildViewModeWidgets
  , buildPluginWidgets
  , buildDataBrowserWidgets
  , buildSliderRowWidgets
  , hitTest
  , isLeftViewWidget
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Linear (V2(..))
import Seer.Config.SliderRegistry (SliderTab(..), SliderDef(..), allSliderDefs, sliderDefsForTab)
import Topo.Pipeline.Stage (allBuiltinStageIds, stageCanonicalName)
import Topo.Plugin.DataResource (DataResourceSchema(..))
import Topo.Plugin.RPC.Manifest (RPCParamSpec(..), RPCParamType(..))
import UI.Layout
import UI.WidgetId (WidgetId(..))
import UI.Widgets (Rect(..), containsPoint)

data Widget = Widget
  { widgetId :: !WidgetId
  , widgetRect :: !Rect
  } deriving (Eq, Show)

buildWidgets :: Layout -> [Widget]
buildWidgets layout =
  let (overlayPrev, overlayNext, fieldPrev, fieldNext) = overlayViewRects layout
      (logDebug, logInfo, logWarn, logError) = logFilterRects layout
      (tabTerrain, tabPlanet, tabClimate, tabWeather, tabBiome, tabErosion, tabPipeline, tabData) = configTabRects layout
      (leftTabTopo, leftTabView) = leftTabRects layout
      sliderWidgets = concatMap (buildSliderWidgets layout) sliderDefsInWidgetOrder
  in [ Widget WidgetGenerate (leftGenButtonRect layout)
     , Widget WidgetLeftToggle (leftToggleRect layout)
     , Widget WidgetLeftTabTopo leftTabTopo
     , Widget WidgetLeftTabView leftTabView
      , Widget WidgetSeedValue (configSeedValueRect layout)
      , Widget WidgetSeedRandom (configSeedRandomRect layout)
     , Widget WidgetChunkMinus (leftChunkMinusRect layout)
     , Widget WidgetChunkPlus (leftChunkPlusRect layout)
     , Widget WidgetConfigToggle (configToggleRect layout)
     , Widget WidgetConfigTabTerrain tabTerrain
     , Widget WidgetConfigTabPlanet tabPlanet
     , Widget WidgetConfigTabClimate tabClimate
     , Widget WidgetConfigTabWeather tabWeather
     , Widget WidgetConfigTabBiome tabBiome
     , Widget WidgetConfigTabErosion tabErosion
     , Widget WidgetConfigTabPipeline tabPipeline
     , Widget WidgetConfigTabData tabData
     , Widget WidgetConfigPresetSave (configPresetSaveRect layout)
     , Widget WidgetConfigPresetLoad (configPresetLoadRect layout)
     , Widget WidgetConfigReset (configResetRect layout)
     , Widget WidgetConfigRevert (configRevertRect layout)
    ] ++
    sliderWidgets ++
    -- Pipeline stage toggle checkboxes
    [ Widget (WidgetPipelineToggle (stageCanonicalName sid))
             (pipelineCheckboxRect idx layout)
    | (idx, sid) <- zip [0..] allBuiltinStageIds
    ] ++
    -- View mode buttons (unscrolled / content-space; use isLeftViewWidget +
    -- scroll adjustment in the hit-test layer when left-view scroll is non-zero)
    buildViewModeWidgets layout 0 ++
    [ Widget WidgetViewOverlayPrev overlayPrev
    , Widget WidgetViewOverlayNext overlayNext
    , Widget WidgetViewFieldPrev fieldPrev
    , Widget WidgetViewFieldNext fieldNext
     , Widget WidgetLogDebug logDebug
     , Widget WidgetLogInfo logInfo
     , Widget WidgetLogWarn logWarn
     , Widget WidgetLogError logError
     , Widget WidgetLogHeader (logHeaderRect layout)
    , Widget WidgetMenuSave (menuSaveRect layout)
    , Widget WidgetMenuLoad (menuLoadRect layout)
    , Widget WidgetMenuExit (menuExitRect layout)
      -- Preset save dialog
    , Widget WidgetPresetSaveOk (presetSaveOkRect layout)
    , Widget WidgetPresetSaveCancel (presetSaveCancelRect layout)
      -- Preset load dialog
    , Widget WidgetPresetLoadOk (presetLoadOkRect layout)
    , Widget WidgetPresetLoadCancel (presetLoadCancelRect layout)
      -- World save dialog
    , Widget WidgetWorldSaveOk (worldSaveOkRect layout)
    , Widget WidgetWorldSaveCancel (worldSaveCancelRect layout)
      -- World load dialog
    , Widget WidgetWorldLoadOk (worldLoadOkRect layout)
    , Widget WidgetWorldLoadCancel (worldLoadCancelRect layout)
     ]

-- | Preserve the pre-registry raw widget precedence for overlapping config-row
-- hit tests. Runtime input filters these by active tab, but the widget-tree
-- spec exercises the unfiltered list directly.
sliderDefsInWidgetOrder :: [SliderDef]
sliderDefsInWidgetOrder =
  let climateDefs = sliderDefsForTab SliderTabClimate
      weatherDefs = sliderDefsForTab SliderTabWeather
      biomeDefs = sliderDefsForTab SliderTabBiome
      planetDefs = sliderDefsForTab SliderTabPlanet
      erosionDefs = sliderDefsForTab SliderTabErosion
      terrainDefs = sliderDefsForTab SliderTabTerrain
      (climatePrimary, climateAfterPrimary) = splitAt 9 climateDefs
      (climateBoundaryMotion, climateRemainder) = splitAt 2 climateAfterPrimary
  in climatePrimary
      ++ weatherDefs
      ++ biomeDefs
      ++ climateBoundaryMotion
      ++ planetDefs
      ++ climateRemainder
      ++ erosionDefs
      ++ terrainDefs

-- | Build dynamic widgets for the Pipeline tab that depend on the current
-- set of discovered plugin names.
--
-- Produces:
--
-- * Move-up / move-down / expand buttons for each plugin row;
-- * Parameter slider\/checkbox widgets for expanded plugins;
-- * Simulation tick controls positioned after all plugin rows.
--
-- These must be merged with the static 'buildWidgets' list before hit
-- testing.
buildPluginWidgets
  :: [Text]
  -> Map Text Bool
  -> Map Text [RPCParamSpec]
  -> Layout
  -> [Widget]
buildPluginWidgets pluginNames expanded paramSpecs layout =
  let builtinCount = length allBuiltinStageIds
      -- Build widgets for each plugin, tracking absolute row index
      (pluginWidgets, nextRow) = foldl buildOne ([], builtinCount) (zip [0..] pluginNames)
      buildOne (!accWidgets, !rowIdx) (_idx, name) =
        let moveWidgets =
              [ Widget (WidgetPluginMoveUp name)   (pipelineMoveUpRect   rowIdx layout)
              , Widget (WidgetPluginMoveDown name)  (pipelineMoveDownRect rowIdx layout)
              , Widget (WidgetPluginToggle name)     (pipelineCheckboxRect rowIdx layout)
              , Widget (WidgetPluginExpand name)     (pipelineExpandRect   rowIdx layout)
              ]
            isExpanded = Map.findWithDefault False name expanded
            specs = Map.findWithDefault [] name paramSpecs
            paramWidgets
              | isExpanded =
                  concatMap (\(pIdx, spec) ->
                    let paramRow = rowIdx + 1 + pIdx
                    in case rpsType spec of
                         ParamBool ->
                           [ Widget (WidgetPluginParamCheck name (rpsName spec))
                                    (pipelineParamCheckRect paramRow layout) ]
                         _ ->
                           [ Widget (WidgetPluginParamSlider name (rpsName spec))
                                    (pipelineParamBarRect paramRow layout) ]
                  ) (zip [0..] specs)
              | otherwise = []
            paramCount = if isExpanded then length specs else 0
        in (accWidgets ++ moveWidgets ++ paramWidgets, rowIdx + 1 + paramCount)
      -- Simulation controls after all plugins
      simWidgets =
        [ Widget WidgetSimTick     (pipelineTickButtonRect nextRow layout)
        , Widget WidgetSimAutoTick (pipelineCheckboxRect (nextRow + 1) layout)
        ]
  in pluginWidgets ++ simWidgets

-- | Build dynamic widgets for the Data Browser tab.
--
-- Row layout mirrors 'dataBrowserRowCount' and the draw code:
--
-- * Plugin name rows (clickable to select);
-- * Resource rows for the selected plugin (clickable to select);
-- * Record rows for the selected resource;
-- * Page-prev \/ page-next buttons on the last row (when records present).
buildDataBrowserWidgets
  :: Map Text [DataResourceSchema]
  -> Maybe Text      -- ^ Selected plugin
  -> Maybe Text      -- ^ Selected resource
  -> Int             -- ^ Number of loaded records
  -> Layout
  -> [Widget]
buildDataBrowserWidgets resources selectedPlugin selectedResource recordCount layout =
  let pluginNames = Map.keys resources
      pluginWidgets =
        [ Widget (WidgetDataPluginSelect pName) (dataBrowserItemRect idx layout)
        | (idx, pName) <- zip [0..] pluginNames
        ]
      resourceOffset = length pluginNames
      (resourceWidgets, resourceCount) = case selectedPlugin of
        Nothing -> ([], 0)
        Just pName ->
          let schemas = Map.findWithDefault [] pName resources
              ws = [ Widget (WidgetDataResourceSelect pName (drsName schema))
                            (dataBrowserItemRect (resourceOffset + rIdx) layout)
                   | (rIdx, schema) <- zip [0..] schemas
                   ]
          in (ws, length schemas)
      recordOffset = resourceOffset + resourceCount
      pageRow = recordOffset + recordCount
      pageWidgets = case (selectedPlugin, selectedResource) of
        (Just pName, Just rName)
          | recordCount > 0 ->
              [ Widget (WidgetDataPagePrev pName rName) (dataBrowserPagePrevRect pageRow layout)
              , Widget (WidgetDataPageNext pName rName) (dataBrowserPageNextRect pageRow layout)
              ]
        _ -> []
  in pluginWidgets ++ resourceWidgets ++ pageWidgets

-- | Build full-row tooltip hit areas for config sliders, grouped by tab.
--
-- Returns @(terrain, planet, climate, weather, biome, erosion)@ widget lists.
-- Each slider row
-- is represented by a single 'Widget' using the slider's minus 'WidgetId',
-- with the shared config slider row hit rect that covers buttons, bar, and
-- label area.
-- The caller selects the appropriate list based on the active config tab
-- and applies scroll offset before 'hitTest'.
buildSliderRowWidgets :: Layout -> ([Widget], [Widget], [Widget], [Widget], [Widget], [Widget])
buildSliderRowWidgets layout = (terrain, planet, climate, weather, biome, erosion)
  where
    row :: SliderDef -> Widget
    row sliderDef =
      let rects = configParamRects (sliderRowIndex sliderDef) layout
      in Widget (sliderMinusWidgetId sliderDef) (configParamRowHitRect rects)

    terrain = map row (sliderDefsForTab SliderTabTerrain)

    planet = map row (sliderDefsForTab SliderTabPlanet)

    climate = map row (sliderDefsForTab SliderTabClimate)

    weather = map row (sliderDefsForTab SliderTabWeather)

    biome = map row (sliderDefsForTab SliderTabBiome)

    erosion = map row (sliderDefsForTab SliderTabErosion)

buildSliderWidgets :: Layout -> SliderDef -> [Widget]
buildSliderWidgets layout sliderDef =
  let rects = configParamRects (sliderRowIndex sliderDef) layout
  in [ Widget (sliderMinusWidgetId sliderDef) (configParamRowMinusRect rects)
     , Widget (sliderPlusWidgetId sliderDef) (configParamRowPlusRect rects)
     ]

-- | Build widgets for the editor toolbar (tool buttons, radius controls,
-- close button).  Only included in hit-testing when the editor is active.
buildEditorWidgets :: Layout -> [Widget]
buildEditorWidgets layout =
  [ Widget (WidgetEditorTool idx) (editorToolButtonRect idx layout)
  | idx <- [0 .. editorToolButtonCount - 1]
  ] ++
  [ Widget WidgetEditorRadiusMinus (editorRadiusMinusRect layout)
  , Widget WidgetEditorRadiusPlus  (editorRadiusPlusRect layout)
  , Widget WidgetEditorClose       (editorCloseRect layout)
  ]

-- | Single-widget list for the editor reopen button, used when the
-- toolbar is closed.
buildEditorReopenWidget :: Layout -> [Widget]
buildEditorReopenWidget layout =
  [ Widget WidgetEditorReopen (editorReopenRect layout) ]

-- | Build widgets for the view mode buttons in the left panel.
-- The @scrollY@ offset is subtracted from each button's y position so
-- the returned rects are in screen space.
buildViewModeWidgets :: Layout -> Int -> [Widget]
buildViewModeWidgets layout scrollY =
  zipWith Widget viewWidgetIds (map (shiftY (-scrollY)) (leftViewRects layout))
  where
    shiftY dy (Rect (V2 x y, V2 w h)) = Rect (V2 x (y + dy), V2 w h)
    viewWidgetIds =
      [ WidgetViewElevation, WidgetViewBiome, WidgetViewClimate
      , WidgetViewWeather, WidgetViewMoisture, WidgetViewPrecip
      , WidgetViewVegetation, WidgetViewTerrainForm
      , WidgetViewPlateId, WidgetViewPlateBoundary
      , WidgetViewPlateHardness, WidgetViewPlateCrust
      , WidgetViewPlateAge, WidgetViewPlateHeight
      , WidgetViewPlateVelocity
      ]

hitTest :: [Widget] -> V2 Int -> Maybe WidgetId
hitTest widgets point =
  case filter (\w -> containsPoint (widgetRect w) point) widgets of
    (w:_) -> Just (widgetId w)
    [] -> Nothing

-- | Returns 'True' for widgets that live in the left View tab
-- (view-mode buttons and overlay selector buttons), which are
-- subject to left-view scroll offset.
isLeftViewWidget :: WidgetId -> Bool
isLeftViewWidget wid = case wid of
  WidgetViewElevation     -> True
  WidgetViewBiome         -> True
  WidgetViewClimate       -> True
  WidgetViewWeather       -> True
  WidgetViewMoisture      -> True
  WidgetViewPrecip        -> True
  WidgetViewVegetation    -> True
  WidgetViewTerrainForm   -> True
  WidgetViewPlateId       -> True
  WidgetViewPlateBoundary -> True
  WidgetViewPlateHardness -> True
  WidgetViewPlateCrust    -> True
  WidgetViewPlateAge      -> True
  WidgetViewPlateHeight   -> True
  WidgetViewPlateVelocity -> True
  WidgetViewOverlayPrev   -> True
  WidgetViewOverlayNext   -> True
  WidgetViewFieldPrev     -> True
  WidgetViewFieldNext     -> True
  _                       -> False
