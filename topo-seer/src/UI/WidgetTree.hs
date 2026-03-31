{-# LANGUAGE BangPatterns #-}
module UI.WidgetTree
  ( WidgetId(..)
  , Widget(..)
  , buildWidgets
  , buildEditorWidgets
  , buildPluginWidgets
  , buildDataBrowserWidgets
  , buildSliderRowWidgets
  , hitTest
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
  let (view1, view2, view3, view4, view5, view6, view7, view8, view9, view10, view11, view12) = leftViewRects layout
      (overlayPrev, overlayNext, fieldPrev, fieldNext) = overlayViewRects layout
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
    [ Widget WidgetViewElevation view1
    , Widget WidgetViewBiome view2
    , Widget WidgetViewClimate view3
    , Widget WidgetViewMoisture view4
    , Widget WidgetViewPrecip view5
    , Widget WidgetViewPlateId view6
    , Widget WidgetViewPlateBoundary view7
    , Widget WidgetViewPlateHardness view8
    , Widget WidgetViewPlateCrust view9
    , Widget WidgetViewPlateAge view10
    , Widget WidgetViewPlateHeight view11
    , Widget WidgetViewPlateVelocity view12
    , Widget WidgetViewOverlayPrev overlayPrev
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

hitTest :: [Widget] -> V2 Int -> Maybe WidgetId
hitTest widgets point =
  case filter (\w -> containsPoint (widgetRect w) point) widgets of
    (w:_) -> Just (widgetId w)
    [] -> Nothing
