{-# LANGUAGE OverloadedStrings #-}
module UI.WidgetTree
  ( WidgetId(..)
  , Widget(..)
  , buildWidgets
  , buildEditorWidgets
  , buildEditorReopenWidget
  , buildViewModeWidgets
  , buildPluginWidgets
  , buildDataBrowserWidgets
  , buildDataDetailWidgets
  , buildSliderRowWidgets
  , hitTest
  , isLeftViewWidget
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Linear (V2(..))
import Seer.Config.SliderRegistry (SliderTab(..), SliderDef(..), allSliderDefs, sliderDefsForTab, sliderMinusWidgetId, sliderPlusWidgetId)
import Seer.Editor.Types (EditorTool(..))
import Topo.Plugin.DataResource (DataResourceSchema(..), DataFieldDef(..), DataFieldType(..), DataConstructorDef(..))
import Topo.Plugin.RPC.Manifest (RPCParamSpec)
import UI.Components.PipelineControls (pipelinePluginWidgetRects, pipelineStageWidgetRects)
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
      overlayActionWidgets = zipWith Widget overlayActionWidgetIds (overlayActionRects layout)
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
    map (uncurry Widget) (pipelineStageWidgetRects layout) ++
    -- View mode buttons (unscrolled / content-space; use isLeftViewWidget +
    -- scroll adjustment in the hit-test layer when left-view scroll is non-zero)
    buildViewModeWidgets layout 0 ++
    [ Widget WidgetDayNightToggle (dayNightToggleRect layout)
    , Widget WidgetViewOverlayPrev overlayPrev
    , Widget WidgetViewOverlayNext overlayNext
    , Widget WidgetViewFieldPrev fieldPrev
    , Widget WidgetViewFieldNext fieldNext
    ] ++
    overlayActionWidgets ++
    [ Widget WidgetLogDebug logDebug
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
overlayActionWidgetIds :: [WidgetId]
overlayActionWidgetIds =
  [ WidgetOverlayManager
  , WidgetOverlaySchema
  , WidgetOverlayProvenance
  , WidgetOverlayExport
  , WidgetOverlayImportValidate
  ]

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
-- * Parameter slider\/checkbox widgets for expanded plugins (after
--   diagnostic text rows);
-- * Simulation tick controls positioned after all plugin rows.
--
-- These must be merged with the static 'buildWidgets' list before hit
-- testing.
buildPluginWidgets
  :: [Text]
  -> Map Text Bool
  -> Map Text [RPCParamSpec]
  -> Map Text [Text]
  -> Layout
  -> [Widget]
buildPluginWidgets pluginNames expanded paramSpecs diagnosticLines layout =
  map (uncurry Widget) $
    pipelinePluginWidgetRects pluginNames expanded paramSpecs diagnosticLines layout

-- | Build dynamic widgets for the Data Browser tab.
--
-- Row layout mirrors 'dataBrowserRowCount' and the draw code:
--
-- * Plugin name rows (clickable to select);
-- * Resource rows for the selected plugin (clickable to select);
-- * Record rows for the selected resource;
-- * Page-prev \/ page-next buttons on the last row (when records present and supported).
buildDataBrowserWidgets
  :: Map Text [DataResourceSchema]
  -> Maybe Text      -- ^ Selected plugin
  -> Maybe Text      -- ^ Selected resource
  -> Int             -- ^ Number of loaded records
  -> Bool            -- ^ Whether the selected resource supports create
  -> Bool            -- ^ Whether the selected resource supports pagination
  -> Layout
  -> [Widget]
buildDataBrowserWidgets resources selectedPlugin selectedResource recordCount canCreate canPage layout =
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
      recordWidgets =
        [ Widget (WidgetDataRecordSelect rIdx)
                 (dataBrowserItemRect (recordOffset + rIdx) layout)
        | rIdx <- [0 .. recordCount - 1]
        ]
      pageRow = recordOffset + recordCount
      pageWidgets = case (selectedPlugin, selectedResource) of
        (Just pName, Just rName)
          | canPage && recordCount > 0 ->
              [ Widget (WidgetDataPagePrev pName rName) (dataBrowserPagePrevRect pageRow layout)
              , Widget (WidgetDataPageNext pName rName) (dataBrowserPageNextRect pageRow layout)
              ]
        _ -> []
      createRow = pageRow + (if null pageWidgets then 0 else 1)
      createWidget
        | canCreate =
            [ Widget WidgetDataCreateNew (dataBrowserCreateButtonRect createRow layout) ]
        | otherwise = []
  in pluginWidgets ++ resourceWidgets ++ recordWidgets ++ pageWidgets ++ createWidget

-- | Build clickable widgets for the record detail popover.
--
-- Includes a dismiss backdrop, plus expand\/collapse toggles for nested
-- fields, and mutation controls (edit\/save\/cancel\/delete buttons plus
-- per-field input widgets when in edit mode).
buildDataDetailWidgets
  :: Int            -- ^ Row index the popover is anchored to
  -> [DataFieldDef] -- ^ Field definitions from the schema
  -> Set Text       -- ^ Currently expanded field paths
  -> Int            -- ^ Validation rows rendered after fields
  -> Bool           -- ^ Edit mode active
  -> Bool           -- ^ Show edit toggle
  -> Bool           -- ^ Can delete
  -> Bool           -- ^ Delete confirmation dialog shown
  -> Layout
  -> [Widget]
buildDataDetailWidgets rowIndex fields expanded validationRowCount editMode showEditToggle canDelete deleteConfirmShown layout =
  let flatFields = enumerateVisibleFields "" fields expanded
      fieldCount = length flatFields
      rowCount = fieldCount + validationRowCount
      toggleWidgets =
        [ Widget (WidgetDataFieldToggle path)
                 (dataDetailFieldRect rowIndex rowCount fIdx layout)
        | (fIdx, (path, True)) <- zip [0..] flatFields
        ]
      dismissWidget =
        Widget WidgetDataDetailDismiss
               (dataDetailPopoverRect rowIndex rowCount layout)
      -- Mutation header buttons
      editToggleWidget
        | showEditToggle =
            [ Widget WidgetDataEditToggle
                     (dataDetailEditToggleRect rowIndex rowCount layout) ]
        | otherwise = []
      saveWidget
        | editMode =
            [ Widget WidgetDataEditSave
                     (dataDetailSaveRect rowIndex rowCount layout) ]
        | otherwise = []
      cancelWidget
        | editMode =
            [ Widget WidgetDataEditCancel
                     (dataDetailCancelRect rowIndex rowCount layout) ]
        | otherwise = []
      deleteWidget
        | canDelete && not editMode =
            [ Widget WidgetDataDeleteBtn
                     (dataDetailDeleteRect rowIndex rowCount layout) ]
        | otherwise = []
      deleteConfirmWidgets
        | deleteConfirmShown =
            [ Widget WidgetDataDeleteConfirm (deleteConfirmOkRect layout)
            , Widget WidgetDataDeleteCancel (deleteConfirmCancelRect layout)
            ]
        | otherwise = []
      -- Per-field input widgets (only in edit mode)
      fieldInputWidgets
        | editMode = concatMap (fieldInputsFor flatFields) (zip [0..] flatFields)
        | otherwise = []
      fieldInputsFor _allFields (fIdx, (path, nestable))
        | nestable  = []
        | otherwise =
            let fType = lookupFieldType path fields
            in case fType of
              Just DFBool ->
                [ Widget (WidgetDataFieldBoolToggle path)
                         (dataDetailFieldInputRect rowIndex rowCount fIdx layout) ]
              Just (DFEnum _) ->
                [ Widget (WidgetDataFieldEnumPrev path)
                         (dataDetailFieldStepMinusRect rowIndex rowCount fIdx layout)
                , Widget (WidgetDataFieldEnumNext path)
                         (dataDetailFieldStepPlusRect rowIndex rowCount fIdx layout)
                ]
              Just DFInt ->
                [ Widget (WidgetDataFieldStepMinus path)
                         (dataDetailFieldStepMinusRect rowIndex rowCount fIdx layout)
                , Widget (WidgetDataFieldStepPlus path)
                         (dataDetailFieldStepPlusRect rowIndex rowCount fIdx layout)
                ]
              Just DFFloat ->
                [ Widget (WidgetDataFieldStepMinus path)
                         (dataDetailFieldStepMinusRect rowIndex rowCount fIdx layout)
                , Widget (WidgetDataFieldStepPlus path)
                         (dataDetailFieldStepPlusRect rowIndex rowCount fIdx layout)
                ]
              Just DFDouble ->
                [ Widget (WidgetDataFieldStepMinus path)
                         (dataDetailFieldStepMinusRect rowIndex rowCount fIdx layout)
                , Widget (WidgetDataFieldStepPlus path)
                         (dataDetailFieldStepPlusRect rowIndex rowCount fIdx layout)
                ]
              Just DFText ->
                [ Widget (WidgetDataFieldTextClick path)
                         (dataDetailFieldInputRect rowIndex rowCount fIdx layout) ]
              _ ->
                -- Fixed-point types use stepper
                [ Widget (WidgetDataFieldStepMinus path)
                         (dataDetailFieldStepMinusRect rowIndex rowCount fIdx layout)
                , Widget (WidgetDataFieldStepPlus path)
                         (dataDetailFieldStepPlusRect rowIndex rowCount fIdx layout)
                ]
  in deleteConfirmWidgets ++ editToggleWidget ++ deleteWidget ++ saveWidget ++ cancelWidget
       ++ fieldInputWidgets ++ toggleWidgets ++ [dismissWidget]

-- | Enumerate the visible field rows, returning @(dotPath, isExpandable)@.
--
-- Expanded nested fields recursively add their children immediately after
-- the parent row.
enumerateVisibleFields :: Text -> [DataFieldDef] -> Set Text -> [(Text, Bool)]
enumerateVisibleFields prefix defs expanded = concatMap go defs
  where
    qualify name
      | T.null prefix = name
      | otherwise     = prefix <> "." <> name
    go fdef =
      let path = qualify (dfName fdef)
          expandable = isNestable (dfType fdef)
          thisRow = [(path, expandable)]
      in if expandable && Set.member path expanded
         then thisRow ++ childRows path (dfType fdef)
         else thisRow
    childRows path (DFRecord subFields) =
      enumerateVisibleFields path subFields expanded
    childRows path (DFAdt ctors) =
      concatMap (\c -> childRows (path <> "." <> dcdName c) (DFRecord (zipWith (\i t -> DataFieldDef (T.pack (show i)) t (T.pack (show i)) False Nothing) [(0::Int)..] (dcdFields c)))) ctors
    -- ADT constructors have positional types, so we synthesise numbered field defs.
    childRows _ _ = []

-- | Whether a field type can be expanded to show nested fields.
isNestable :: DataFieldType -> Bool
isNestable (DFRecord _) = True
isNestable (DFAdt _)    = True
isNestable _            = False

-- | Look up the 'DataFieldType' for a dot-separated path within a flat
-- list of top-level field definitions.
lookupFieldType :: Text -> [DataFieldDef] -> Maybe DataFieldType
lookupFieldType path defs = case T.splitOn "." path of
  []    -> Nothing
  (k:ks) -> case filter (\d -> dfName d == k) defs of
    (d:_) -> resolveRest ks (dfType d)
    []    -> Nothing
  where
    resolveRest [] ft = Just ft
    resolveRest (s:ss) (DFRecord subDefs) =
      case filter (\d -> dfName d == s) subDefs of
        (d:_) -> resolveRest ss (dfType d)
        []    -> Nothing
    resolveRest (ctorName:indexText:ss) (DFAdt ctors) = do
      ctor <- findConstructor ctorName ctors
      index <- parseIndex indexText
      fieldType <- safeIndex (dcdFields ctor) index
      resolveRest ss fieldType
    resolveRest _ _ = Nothing

findConstructor :: Text -> [DataConstructorDef] -> Maybe DataConstructorDef
findConstructor ctorName = foldr (\ctor acc -> if dcdName ctor == ctorName then Just ctor else acc) Nothing

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs index
  | index < 0 = Nothing
  | otherwise = case drop index xs of
      value:_ -> Just value
      []      -> Nothing

parseIndex :: Text -> Maybe Int
parseIndex text = case reads (T.unpack text) of
  [(n, "")] | n >= 0 -> Just n
  _                  -> Nothing

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
-- close button) and the context-sensitive parameter bar.
-- Only included in hit-testing when the editor is active.
buildEditorWidgets :: Layout -> EditorTool -> [Widget]
buildEditorWidgets layout tool =
  [ Widget (WidgetEditorTool idx) (editorToolButtonRect idx layout)
  | idx <- [0 .. editorToolButtonCount - 1]
  ] ++
  [ Widget WidgetEditorRadiusMinus (editorRadiusMinusRect layout)
  , Widget WidgetEditorRadiusPlus  (editorRadiusPlusRect layout)
  , Widget WidgetEditorClose       (editorCloseRect layout)
  ] ++
  paramBarWidgets ++
  falloffWidgets
  where
    (falloffPrev, _, falloffNext) = editorParamFalloffRects layout
    -- Falloff is shown for tools where falloff is meaningful
    hasFalloff = tool `notElem` [ToolPaintBiome, ToolPaintForm, ToolSetHardness]
    falloffWidgets
      | hasFalloff =
          [ Widget WidgetEditorFalloffPrev falloffPrev
          , Widget WidgetEditorFalloffNext falloffNext
          ]
      | otherwise = []
    paramBarWidgets = case tool of
      ToolRaise -> numericSlot 0 ++ numericSlot 1
      ToolLower -> numericSlot 0 ++ numericSlot 1
      ToolSmooth -> numericSlot 0
      ToolFlatten -> numericSlot 0
      ToolNoise -> numericSlot 0 ++ numericSlot 1
      ToolPaintBiome -> cycleSlot 0
      ToolPaintForm -> cycleSlot 0
      ToolSetHardness -> numericSlot 0
      ToolErode -> numericSlot 0
    numericSlot n =
      let (minR, _, plusR) = editorParamNumericRects n layout
      in [ Widget (WidgetEditorParamMinus n) minR
         , Widget (WidgetEditorParamPlus  n) plusR
         ]
    cycleSlot n =
      let (prevR, _, nextR) = editorParamCycleRects n layout
      in [ Widget (WidgetEditorCyclePrev n) prevR
         , Widget (WidgetEditorCycleNext n) nextR
         ]

-- | Single-widget list for the editor reopen button, used when the
-- toolbar is closed.
buildEditorReopenWidget :: Layout -> [Widget]
buildEditorReopenWidget layout =
  [ Widget WidgetEditorReopen (editorReopenRect layout) ]

-- | Build widgets for the layered View tab controls in the left panel.
-- The @scrollY@ offset is subtracted from each button's y position so
-- the returned rects are in screen space.
buildViewModeWidgets :: Layout -> Int -> [Widget]
buildViewModeWidgets layout scrollY =
  map shiftWidget unscrolled
  where
    shiftY dy (Rect (V2 x y, V2 w h)) = Rect (V2 x (y + dy), V2 w h)
    shiftWidget (Widget wid rect) = Widget wid (shiftY (-scrollY) rect)
    unscrolled =
      zipWith Widget baseWidgetIds (leftBaseViewRects layout)
      ++ zipWith Widget overlayWidgetIds (leftWeatherOverlayRects layout)
      ++ zipWith Widget basisWidgetIds (leftWeatherBasisRects layout)
    baseWidgetIds =
      [ WidgetViewBaseElevation
      , WidgetViewBaseBiome
      , WidgetViewBaseMoisture
      , WidgetViewBaseVegetation
      , WidgetViewBaseTerrainForm
      , WidgetViewBasePlateId
      , WidgetViewBasePlateBoundary
      , WidgetViewBasePlateHardness
      , WidgetViewBasePlateCrust
      , WidgetViewBasePlateAge
      , WidgetViewBasePlateHeight
      , WidgetViewBasePlateVelocity
      ]
    overlayWidgetIds =
      [ WidgetViewOverlayNone
      , WidgetViewOverlayTemperature
      , WidgetViewOverlayPrecipitation
      , WidgetViewOverlayCloud
      ]
    basisWidgetIds =
      [ WidgetViewBasisAverage
      , WidgetViewBasisCurrent
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
  WidgetViewBaseElevation -> True
  WidgetViewBaseBiome -> True
  WidgetViewBaseMoisture -> True
  WidgetViewBaseVegetation -> True
  WidgetViewBaseTerrainForm -> True
  WidgetViewBasePlateId -> True
  WidgetViewBasePlateBoundary -> True
  WidgetViewBasePlateHardness -> True
  WidgetViewBasePlateCrust -> True
  WidgetViewBasePlateAge -> True
  WidgetViewBasePlateHeight -> True
  WidgetViewBasePlateVelocity -> True
  WidgetViewOverlayNone -> True
  WidgetViewOverlayTemperature -> True
  WidgetViewOverlayPrecipitation -> True
  WidgetViewOverlayCloud -> True
  WidgetViewBasisAverage -> True
  WidgetViewBasisCurrent -> True
  WidgetViewElevation     -> True
  WidgetViewBiome         -> True
  WidgetViewClimate       -> True
  WidgetViewWeather       -> True
  WidgetViewMoisture      -> True
  WidgetViewPrecip        -> True
  WidgetViewPrecipCurrent -> True
  WidgetViewVegetation    -> True
  WidgetViewTerrainForm   -> True
  WidgetViewPlateId       -> True
  WidgetViewPlateBoundary -> True
  WidgetViewPlateHardness -> True
  WidgetViewPlateCrust    -> True
  WidgetViewPlateAge      -> True
  WidgetViewPlateHeight   -> True
  WidgetViewPlateVelocity -> True
  WidgetViewCloud         -> True
  WidgetViewCloudTypical  -> True
  WidgetDayNightToggle    -> True
  WidgetViewOverlayPrev   -> True
  WidgetViewOverlayNext   -> True
  WidgetViewFieldPrev     -> True
  WidgetViewFieldNext     -> True
  WidgetOverlayManager    -> True
  WidgetOverlaySchema     -> True
  WidgetOverlayProvenance -> True
  WidgetOverlayExport     -> True
  WidgetOverlayImportValidate -> True
  _                       -> False
