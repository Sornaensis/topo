{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Renderer-neutral component boundaries, normalized input events, and pure
-- reducer plumbing for UI code.
module UI.Component
  ( ComponentId(..)
  , componentRoot
  , componentTopBar
  , componentLeftPanel
  , componentViewPanel
  , componentConfigPanel
  , componentConfigSliders
  , componentPipelinePanel
  , componentDataBrowser
  , componentLogPanel
  , componentDialogs
  , componentEditor
  , componentForWidget
  , UiPointerButton(..)
  , UiModifier(..)
  , UiKey(..)
  , UiEvent(..)
  , uiEventPosition
  , RoutedUiEvent(..)
  , HitRegion(..)
  , widgetHitRegion
  , routeUiEvent
  , routeWidgets
  , ReducerResult(..)
  , noEffects
  , withEffects
  , Component(..)
  , componentRegions
  ) where

import Data.Text (Text)
import Linear (V2)
import UI.DrawCommand (DrawCommand)
import UI.WidgetId (WidgetId(..))
import UI.WidgetTree (Widget(..))
import UI.Widgets (Rect, containsPoint)

-- | Stable identifier for a coarse UI component boundary.
newtype ComponentId = ComponentId { unComponentId :: Text }
  deriving (Eq, Ord, Show)

componentRoot :: ComponentId
componentRoot = ComponentId "root"

componentTopBar :: ComponentId
componentTopBar = ComponentId "top-bar"

componentLeftPanel :: ComponentId
componentLeftPanel = ComponentId "left-panel"

componentViewPanel :: ComponentId
componentViewPanel = ComponentId "view-panel"

componentConfigPanel :: ComponentId
componentConfigPanel = ComponentId "config-panel"

componentConfigSliders :: ComponentId
componentConfigSliders = ComponentId "config-sliders"

componentPipelinePanel :: ComponentId
componentPipelinePanel = ComponentId "pipeline-panel"

componentDataBrowser :: ComponentId
componentDataBrowser = ComponentId "data-browser"

componentLogPanel :: ComponentId
componentLogPanel = ComponentId "log-panel"

componentDialogs :: ComponentId
componentDialogs = ComponentId "dialogs"

componentEditor :: ComponentId
componentEditor = ComponentId "editor"

-- | Assign the existing stable widget ids to coarse component boundaries.
componentForWidget :: WidgetId -> ComponentId
componentForWidget wid = case wid of
  WidgetConfigToggle -> componentConfigPanel

  WidgetGenerate -> componentLeftPanel
  WidgetLeftToggle -> componentLeftPanel
  WidgetLeftTabTopo -> componentLeftPanel
  WidgetLeftTabView -> componentLeftPanel
  WidgetSeedValue -> componentLeftPanel
  WidgetSeedRandom -> componentLeftPanel
  WidgetChunkMinus -> componentLeftPanel
  WidgetChunkPlus -> componentLeftPanel

  WidgetViewBaseElevation -> componentViewPanel
  WidgetViewBaseBiome -> componentViewPanel
  WidgetViewBaseMoisture -> componentViewPanel
  WidgetViewBaseVegetation -> componentViewPanel
  WidgetViewBaseTerrainForm -> componentViewPanel
  WidgetViewBasePlateId -> componentViewPanel
  WidgetViewBasePlateBoundary -> componentViewPanel
  WidgetViewBasePlateHardness -> componentViewPanel
  WidgetViewBasePlateCrust -> componentViewPanel
  WidgetViewBasePlateAge -> componentViewPanel
  WidgetViewBasePlateHeight -> componentViewPanel
  WidgetViewBasePlateVelocity -> componentViewPanel
  WidgetViewOverlayNone -> componentViewPanel
  WidgetViewOverlayTemperature -> componentViewPanel
  WidgetViewOverlayPrecipitation -> componentViewPanel
  WidgetViewOverlayCloud -> componentViewPanel
  WidgetViewBasisAverage -> componentViewPanel
  WidgetViewBasisCurrent -> componentViewPanel
  WidgetDayNightToggle -> componentViewPanel
  WidgetViewOverlayPrev -> componentViewPanel
  WidgetViewOverlayNext -> componentViewPanel
  WidgetViewFieldPrev -> componentViewPanel
  WidgetViewFieldNext -> componentViewPanel
  WidgetOverlayManager -> componentViewPanel
  WidgetOverlaySchema -> componentViewPanel
  WidgetOverlayProvenance -> componentViewPanel
  WidgetOverlayExport -> componentViewPanel
  WidgetOverlayImportValidate -> componentViewPanel

  WidgetConfigTabTerrain -> componentConfigPanel
  WidgetConfigTabPlanet -> componentConfigPanel
  WidgetConfigTabClimate -> componentConfigPanel
  WidgetConfigTabWeather -> componentConfigPanel
  WidgetConfigTabBiome -> componentConfigPanel
  WidgetConfigTabErosion -> componentConfigPanel
  WidgetConfigTabPipeline -> componentConfigPanel
  WidgetConfigTabData -> componentConfigPanel
  WidgetConfigPresetSave -> componentConfigPanel
  WidgetConfigPresetLoad -> componentConfigPanel
  WidgetConfigReset -> componentConfigPanel
  WidgetConfigRevert -> componentConfigPanel
  WidgetSliderMinus _ -> componentConfigSliders
  WidgetSliderPlus _ -> componentConfigSliders

  WidgetPipelineToggle _ -> componentPipelinePanel
  WidgetPluginMoveUp _ -> componentPipelinePanel
  WidgetPluginMoveDown _ -> componentPipelinePanel
  WidgetPluginToggle _ -> componentPipelinePanel
  WidgetPluginExpand _ -> componentPipelinePanel
  WidgetPluginParamSlider _ _ -> componentPipelinePanel
  WidgetPluginParamCheck _ _ -> componentPipelinePanel
  WidgetSimTick -> componentPipelinePanel
  WidgetSimAutoTick -> componentPipelinePanel

  WidgetDataPluginSelect _ -> componentDataBrowser
  WidgetDataResourceSelect _ _ -> componentDataBrowser
  WidgetDataPagePrev _ _ -> componentDataBrowser
  WidgetDataPageNext _ _ -> componentDataBrowser
  WidgetDataRecordSelect _ -> componentDataBrowser
  WidgetDataDetailDismiss -> componentDataBrowser
  WidgetDataFieldToggle _ -> componentDataBrowser
  WidgetDataEditToggle -> componentDataBrowser
  WidgetDataEditSave -> componentDataBrowser
  WidgetDataEditCancel -> componentDataBrowser
  WidgetDataCreateNew -> componentDataBrowser
  WidgetDataDeleteBtn -> componentDataBrowser
  WidgetDataDeleteConfirm -> componentDataBrowser
  WidgetDataDeleteCancel -> componentDataBrowser
  WidgetDataFieldTextClick _ -> componentDataBrowser
  WidgetDataFieldStepMinus _ -> componentDataBrowser
  WidgetDataFieldStepPlus _ -> componentDataBrowser
  WidgetDataFieldBoolToggle _ -> componentDataBrowser
  WidgetDataFieldEnumPrev _ -> componentDataBrowser
  WidgetDataFieldEnumNext _ -> componentDataBrowser

  WidgetLogDebug -> componentLogPanel
  WidgetLogInfo -> componentLogPanel
  WidgetLogWarn -> componentLogPanel
  WidgetLogError -> componentLogPanel
  WidgetLogHeader -> componentLogPanel

  WidgetMenuSave -> componentDialogs
  WidgetMenuLoad -> componentDialogs
  WidgetMenuExit -> componentDialogs
  WidgetPresetSaveOk -> componentDialogs
  WidgetPresetSaveCancel -> componentDialogs
  WidgetPresetLoadOk -> componentDialogs
  WidgetPresetLoadCancel -> componentDialogs
  WidgetPresetLoadItem -> componentDialogs
  WidgetWorldSaveOk -> componentDialogs
  WidgetWorldSaveCancel -> componentDialogs
  WidgetWorldLoadOk -> componentDialogs
  WidgetWorldLoadCancel -> componentDialogs
  WidgetWorldLoadItem -> componentDialogs
  WidgetWorldDelete -> componentDialogs
  WidgetWorldDeleteConfirm -> componentDialogs
  WidgetWorldDeleteCancel -> componentDialogs
  WidgetOverlayInspectorClose -> componentDialogs
  WidgetOverlayInspectorItem _ -> componentDialogs
  WidgetOverlayInspectorCopy -> componentDialogs
  WidgetOverlayInspectorSave -> componentDialogs
  WidgetOverlayInspectorImportInput -> componentDialogs
  WidgetOverlayInspectorValidate -> componentDialogs

  WidgetEditorTool _ -> componentEditor
  WidgetEditorRadiusMinus -> componentEditor
  WidgetEditorRadiusPlus -> componentEditor
  WidgetEditorClose -> componentEditor
  WidgetEditorReopen -> componentEditor
  WidgetEditorParamMinus _ -> componentEditor
  WidgetEditorParamPlus _ -> componentEditor
  WidgetEditorCyclePrev _ -> componentEditor
  WidgetEditorCycleNext _ -> componentEditor
  WidgetEditorFalloffPrev -> componentEditor
  WidgetEditorFalloffNext -> componentEditor

-- | Renderer-neutral pointer buttons used by normalized UI events.
data UiPointerButton
  = UiPointerPrimary
  | UiPointerMiddle
  | UiPointerSecondary
  | UiPointerAuxiliary !Int
  deriving (Eq, Show)

data UiModifier
  = UiModShift
  | UiModCtrl
  | UiModAlt
  | UiModMeta
  deriving (Eq, Ord, Show)

newtype UiKey = UiKey { unUiKey :: Text }
  deriving (Eq, Ord, Show)

-- | Normalized event shape for component routing and reducer tests.
data UiEvent
  = UiPointerDown !(V2 Int) !UiPointerButton
  | UiPointerUp !(V2 Int) !UiPointerButton
  | UiPointerMove !(V2 Int)
  | UiScroll !(V2 Int) !(V2 Int)
    -- ^ Cursor position and wheel delta.
  | UiTextInput !Text
  | UiKeyDown !UiKey ![UiModifier]
  | UiKeyUp !UiKey ![UiModifier]
  deriving (Eq, Show)

uiEventPosition :: UiEvent -> Maybe (V2 Int)
uiEventPosition event = case event of
  UiPointerDown pos _ -> Just pos
  UiPointerUp pos _ -> Just pos
  UiPointerMove pos -> Just pos
  UiScroll pos _ -> Just pos
  UiTextInput _ -> Nothing
  UiKeyDown _ _ -> Nothing
  UiKeyUp _ _ -> Nothing

data RoutedUiEvent = RoutedUiEvent
  { routedEvent :: !UiEvent
  , routedComponent :: !(Maybe ComponentId)
  , routedWidget :: !(Maybe WidgetId)
  } deriving (Eq, Show)

data HitRegion = HitRegion
  { hitRegionComponent :: !ComponentId
  , hitRegionWidget :: !WidgetId
  , hitRegionRect :: !Rect
  } deriving (Eq, Show)

widgetHitRegion :: ComponentId -> Widget -> HitRegion
widgetHitRegion cid widget = HitRegion
  { hitRegionComponent = cid
  , hitRegionWidget = widgetId widget
  , hitRegionRect = widgetRect widget
  }

routeUiEvent :: [HitRegion] -> UiEvent -> RoutedUiEvent
routeUiEvent regions event =
  case uiEventPosition event of
    Nothing -> untargeted
    Just point ->
      case filter (\region -> containsPoint (hitRegionRect region) point) regions of
        region:_ -> RoutedUiEvent
          { routedEvent = event
          , routedComponent = Just (hitRegionComponent region)
          , routedWidget = Just (hitRegionWidget region)
          }
        [] -> untargeted
  where
    untargeted = RoutedUiEvent
      { routedEvent = event
      , routedComponent = Nothing
      , routedWidget = Nothing
      }

routeWidgets :: [Widget] -> UiEvent -> RoutedUiEvent
routeWidgets widgets = routeUiEvent (map toRegion widgets)
  where
    toRegion widget = widgetHitRegion (componentForWidget (widgetId widget)) widget

-- | A pure reducer result with explicit effects for runtime interpreters.
data ReducerResult model effect = ReducerResult
  { reducerModel :: !model
  , reducerEffects :: ![effect]
  } deriving (Eq, Show)

noEffects :: model -> ReducerResult model effect
noEffects model = ReducerResult model []

withEffects :: model -> [effect] -> ReducerResult model effect
withEffects = ReducerResult

-- | Minimal component boundary: hit areas, event-to-action routing, pure
-- reducer, and renderer-neutral draw commands.
data Component model action effect = Component
  { componentId :: !ComponentId
  , componentWidgets :: model -> [Widget]
  , componentHandleEvent :: model -> RoutedUiEvent -> Maybe action
  , componentReduce :: model -> action -> ReducerResult model effect
  , componentDrawCommands :: model -> [DrawCommand]
  }

componentRegions :: Component model action effect -> model -> [HitRegion]
componentRegions component model =
  map (widgetHitRegion (componentId component)) (componentWidgets component model)
