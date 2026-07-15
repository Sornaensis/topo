{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Seer.Input.Widgets
  ( handleClick
  ) where

import Actor.Data (TerrainSnapshot(..))
import Actor.Log (LogEntry(..), LogLevel(..), LogSnapshot(..), appendLog, getLogSnapshot, setLogCollapsed, setLogMinLevel, setLogScroll)
import Data.Aeson (Value(..), object, (.=))

import Actor.UI
  ( BaseViewMode(..)
  , ConfigTab(..)
  , DataBrowserState(..)
  , LeftTab(..)
  , Ui
  , UiMenuMode(..)
  , SkyOverlayMode(..)
  , UiState(..)
  , ViewMode(..)
  , WeatherBasis(..)
  , LayeredViewState(..)
  , configRowCount
  , effectiveViewSelection
  , getUiSnapshot
  , setUiConfigScroll
  , setUiContextHex
  , setUiContextPos
  , setUiLeftTab
  , setUiSeedEditing
  , setUiSeedInput
  , setUiShowConfig
  , setUiShowLeftPanel
  , setUiMenuMode
  , setUiPresetInput
  , setUiPresetList
  , setUiPresetSelected
  , setUiPresetFilter
  , setUiWorldSaveInput
  , setUiWorldList
  , setUiWorldSelected
  , setUiWorldFilter
  , setUiPluginExpanded
  , setUiOverlayFields
  )
import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.IORef (writeIORef)
import Data.Int (Int32)
import Data.List (find, findIndex, partition)
import Data.Word (Word64)
import Data.Maybe (isJust, listToMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Linear (V2(..))
import qualified SDL
import qualified SDL.Raw.Types as Raw
import Seer.Config.SliderRegistry
  ( SliderDef(..)
  , sliderDefForWidget
  , sliderWidgetPart
  )
import Seer.Config.SliderUi (configTabForSliderTab)
import Seer.Draw (seedMaxDigits)
import Seer.Config.Snapshot (listSnapshots)
import Seer.World.Persist (listWorlds)
import Seer.World.Persist.Types (WorldSaveManifest(..))
import Topo.Pipeline.Stage (parseStageId)
import qualified Seer.DataBrowser.AppService as DataBrowser
import Seer.DataBrowser.Executor (submitDataBrowserAction)
import Seer.DataBrowser.Model (DataBrowserPageAction(..))
import Topo.Plugin.DataResource (DataResourceSchema(..), DataOperations(..), noOperations)
import Topo.Plugin.RPC.Manifest (RPCParamSpec(..))
import UI.Components.ConfigSliders (configSliderInputValueForId)
import UI.Components.PipelineControls (pipelineParamToggleValue, pipelineParamValueFromClick, pipelineScrollOffset)
import UI.Layout
import UI.WidgetTree (Widget(..), WidgetId(..), buildWidgets, buildPluginWidgets, buildDataBrowserWidgets, buildDataDetailWidgets, hitTest, isLeftViewWidget)
import UI.Widgets (Rect(..), containsPoint)
import System.Random (randomIO)
import Hyperspace.Actor (ActorHandle, Protocol)
import Actor.UiActions (ActorHandles(..), UiAction(..))
import Seer.Input.Actions (InputEnv(..), runInputService, submitAction)
import Topo.Overlay (Overlay(..), lookupOverlay, overlayNames)
import Topo.Overlay.Schema (OverlayFieldDef(..), OverlayFieldType(..), OverlaySchema(..))
import Seer.Input.Context (InputContext(..))
import Seer.Service.Types (ServiceResponse(..), serviceErrorText)
handleClick
  :: InputContext
  -> SDL.Point V2 Int32
  -> IO ()
handleClick inputContext (SDL.P (V2 x y)) = do
  let window = icWindow inputContext
      widgetEnv = icInputEnv inputContext
      actorHandles = ieActorHandles widgetEnv
      uiHandle = ahUiHandle actorHandles
      logHandle = ahLogHandle actorHandles
      quitRef = icQuitRef inputContext
  (V2 winW winH) <- SDL.get (SDL.windowSize window)
  let logSnap = ieLogSnapshot widgetEnv
      uiSnap = ieUiSnapshot widgetEnv
      point = V2 (fromIntegral x) (fromIntegral y)
      logHeight = if lsCollapsed logSnap then 24 else 160
      seedWidth = max 120 (seedMaxDigits * 10)
      layout = layoutForSeed (V2 (fromIntegral winW) (fromIntegral winH)) logHeight seedWidth
      seedValue = configSeedValueRect layout
      scrollArea = configScrollAreaRect layout
      scrollBar = configScrollBarRect layout
      inScrollBar = uiShowConfig uiSnap && containsPoint scrollBar point
      inConfigScroll = uiShowConfig uiSnap && containsPoint scrollArea point
      configScrollForHit = case uiConfigTab uiSnap of
        ConfigPipeline -> pipelineScrollOffset uiSnap layout
        _ -> uiConfigScroll uiSnap
      scrollPoint = if inConfigScroll
        then V2 (fromIntegral x) (fromIntegral y + configScrollForHit)
        else point
      dbs_ = uiDataBrowser uiSnap
      detailWidgets = case (dbsSelectedRowIndex dbs_, dbsSelectedPlugin dbs_, dbsSelectedResource dbs_) of
        (Just rowIdx, Just pName, Just rName) ->
          let schemas = Map.findWithDefault [] pName (uiDataResources uiSnap)
              mSchema = find (\s -> drsName s == rName) schemas
              fields = maybe [] drsFields mSchema
              ops = maybe noOperations drsOperations mSchema
              isEditing = dbsEditMode dbs_ || dbsCreateMode dbs_
              showEditToggle = doUpdate ops && not (dbsCreateMode dbs_)
              validationRowCount = length (dbsValidationErrors dbs_)
          in buildDataDetailWidgets
               rowIdx
               fields
               (dbsExpandedFields dbs_)
               validationRowCount
               isEditing
               showEditToggle
               (doDelete ops)
               (dbsDeleteConfirm dbs_)
               layout
        _ -> []
      selectedSchema = do
        pName <- dbsSelectedPlugin (uiDataBrowser uiSnap)
        rName <- dbsSelectedResource (uiDataBrowser uiSnap)
        schemas <- Map.lookup pName (uiDataResources uiSnap)
        find (\s -> drsName s == rName) schemas
      canCreate = maybe False (doCreate . drsOperations) selectedSchema
      canPage = maybe False (doPage . drsOperations) selectedSchema
      widgetsAll = buildWidgets layout
                ++ buildPluginWidgets (uiPluginNames uiSnap) (uiPluginExpanded uiSnap) (uiPluginParamSpecs uiSnap) (uiPluginDiagnosticLines uiSnap) layout
                ++ buildDataBrowserWidgets (uiDataResources uiSnap) (dbsSelectedPlugin (uiDataBrowser uiSnap)) (dbsSelectedResource (uiDataBrowser uiSnap)) (length (dbsRecords (uiDataBrowser uiSnap))) canCreate canPage layout
                ++ detailWidgets
      widgets =
        if uiShowConfig uiSnap
          then filter (configWidgetAllowed (uiConfigTab uiSnap)) widgetsAll
          else widgetsAll
      isConfigSliderWidget = isJust . sliderDefForWidget
      (configSliderWidgets, nonSliderWidgets) = partition (isConfigSliderWidget . widgetId) widgets
      leftPanelBounds = leftPanelRect layout
      inLeftViewPanel = uiShowLeftPanel uiSnap && uiLeftTab uiSnap == LeftView
                        && containsPoint leftPanelBounds point
      inLeftViewContent = inLeftViewPanel && containsPoint (leftViewContentClipRect layout) point
      leftViewAdjPoint = V2 (fromIntegral x) (fromIntegral y + uiLeftViewScroll uiSnap)
      (leftViewWidgets, otherWidgets) = partition (isLeftViewWidget . widgetId) nonSliderWidgets
      (pipelineWidgets, nonPipelineWidgets) = partition (isPipelineScrollWidget . widgetId) otherWidgets
      detailHit = hitTest detailWidgets point
      hitWidget = detailHit <|>
        if inConfigScroll
          then case hitTest configSliderWidgets scrollPoint <|> hitTest pipelineWidgets scrollPoint of
            Just wid -> Just wid
            Nothing ->
              let viewHit = if inLeftViewContent then hitTest leftViewWidgets leftViewAdjPoint else Nothing
              in viewHit <|> hitTest nonPipelineWidgets point
          else if inLeftViewContent
            then hitTest leftViewWidgets leftViewAdjPoint <|> hitTest otherWidgets point
            else if inLeftViewPanel
              then hitTest otherWidgets point
            else hitTest widgets point
      configWidgetAllowed tab widget =
        case sliderDefForWidget (widgetId widget) of
          Just sliderDef -> tab == configTabForSliderTab (sliderTab sliderDef)
          Nothing -> bespokeConfigWidgetAllowed tab (widgetId widget)
  if inScrollBar
    then do
      let rowHeight = 24
          gap = 10
          rows = configRowCount (uiConfigTab uiSnap) uiSnap
          contentHeight = max rowHeight (configRowTopPad + rows * rowHeight + max 0 (rows - 1) * gap)
          Rect (V2 _ sy, V2 _ sh) = scrollArea
          maxOffset = max 0 (contentHeight - sh)
          ratio
            | sh <= 1 = 0
            | otherwise = max 0 (min 1 (fromIntegral (fromIntegral y - sy) / fromIntegral sh))
          newScroll = round (ratio * fromIntegral maxOffset)
      setUiConfigScroll uiHandle newScroll
    else do
      case uiContextHex uiSnap of
        Just _ -> do
          setUiContextHex uiHandle Nothing
          setUiContextPos uiHandle Nothing
        Nothing -> pure ()
      let whenLeftPanel action = when (uiShowLeftPanel uiSnap) action
          whenLeftTopo action = when (uiShowLeftPanel uiSnap && uiLeftTab uiSnap == LeftTopo) action
          whenLeftView action = when (uiShowLeftPanel uiSnap && uiLeftTab uiSnap == LeftView) action
          whenConfigVisible action = when (uiShowConfig uiSnap) action
      if uiMenuMode uiSnap /= MenuNone
        then handleMenuClick layout widgets point
        else case hitWidget of
          { Just WidgetGenerate -> whenLeftTopo (runService "generate" Null)
          ; Just WidgetLeftToggle -> setUiShowLeftPanel uiHandle (not (uiShowLeftPanel uiSnap))
          ; Just WidgetLeftTabTopo -> whenLeftPanel (setUiLeftTab uiHandle LeftTopo)
          ; Just WidgetLeftTabView -> whenLeftPanel (setUiLeftTab uiHandle LeftView)
          ; Just WidgetSeedValue -> whenLeftTopo (startSeedEdit seedValue)
          ; Just WidgetSeedRandom -> whenLeftTopo randomSeed
          ; Just WidgetChunkMinus -> whenLeftTopo (runService "click_widget" (object ["widget_id" .= ("WidgetChunkMinus" :: Text.Text)]))
          ; Just WidgetChunkPlus -> whenLeftTopo (runService "click_widget" (object ["widget_id" .= ("WidgetChunkPlus" :: Text.Text)]))
          ; Just WidgetConfigToggle -> toggleConfig
          ; Just WidgetConfigTabTerrain -> whenConfigVisible (setConfigTab "terrain")
          ; Just WidgetConfigTabPlanet -> whenConfigVisible (setConfigTab "planet")
          ; Just WidgetConfigTabClimate -> whenConfigVisible (setConfigTab "climate")
          ; Just WidgetConfigTabWeather -> whenConfigVisible (setConfigTab "weather")
          ; Just WidgetConfigTabBiome -> whenConfigVisible (setConfigTab "biome")
          ; Just WidgetConfigTabErosion -> whenConfigVisible (setConfigTab "erosion")
          ; Just WidgetConfigTabPipeline -> whenConfigVisible (setConfigTab "pipeline")
          ; Just WidgetConfigTabData -> whenConfigVisible $ do
              setConfigTab "data"
              applyDataBrowserAction uiSnap DataBrowser.DataBrowserLoadPlugins
          ; Just wid
          | Just (sliderDef, sliderPart) <- sliderWidgetPart wid
              -> whenConfigVisible (setConfigSlider sliderDef sliderPart)
          ; Just wid
          | isBespokeConfigWidget wid ->
              handleBespokeConfigWidget layout wid scrollPoint whenConfigVisible uiSnap
          ; Just WidgetViewBaseElevation -> whenLeftView (selectBase BaseViewElevation)
          ; Just WidgetViewBaseBiome -> whenLeftView (selectBase BaseViewBiome)
          ; Just WidgetViewBaseMoisture -> whenLeftView (selectBase BaseViewMoisture)
          ; Just WidgetViewBaseVegetation -> whenLeftView (selectBase BaseViewVegetation)
          ; Just WidgetViewBaseTerrainForm -> whenLeftView (selectBase BaseViewTerrainForm)
          ; Just WidgetViewBasePlateId -> whenLeftView (selectBase BaseViewPlateId)
          ; Just WidgetViewBasePlateBoundary -> whenLeftView (selectBase BaseViewPlateBoundary)
          ; Just WidgetViewBasePlateHardness -> whenLeftView (selectBase BaseViewPlateHardness)
          ; Just WidgetViewBasePlateCrust -> whenLeftView (selectBase BaseViewPlateCrust)
          ; Just WidgetViewBasePlateAge -> whenLeftView (selectBase BaseViewPlateAge)
          ; Just WidgetViewBasePlateHeight -> whenLeftView (selectBase BaseViewPlateHeight)
          ; Just WidgetViewBasePlateVelocity -> whenLeftView (selectBase BaseViewPlateVelocity)
          ; Just WidgetViewOverlayNone -> whenLeftView (selectOverlay Nothing)
          ; Just WidgetViewOverlayTemperature -> whenLeftView (selectOverlay (Just SkyOverlayWeatherTemperature))
          ; Just WidgetViewOverlayPrecipitation -> whenLeftView (selectOverlay (Just SkyOverlayPrecipitation))
          ; Just WidgetViewOverlayCloud -> whenLeftView (selectOverlay (Just SkyOverlayCloud))
          ; Just WidgetViewBasisAverage -> whenLeftView (selectBasis WeatherBasisAverage)
          ; Just WidgetViewBasisCurrent -> whenLeftView (selectBasis WeatherBasisCurrent)
          ; Just WidgetViewElevation -> whenLeftView (selectLegacyView ViewElevation)
          ; Just WidgetViewBiome -> whenLeftView (selectLegacyView ViewBiome)
          ; Just WidgetViewClimate -> whenLeftView (selectLegacyView ViewClimate)
          ; Just WidgetViewWeather -> whenLeftView (selectLegacyView ViewWeather)
          ; Just WidgetViewMoisture -> whenLeftView (selectLegacyView ViewMoisture)
          ; Just WidgetViewPrecip -> whenLeftView (selectLegacyView ViewPrecip)
          ; Just WidgetViewPrecipCurrent -> whenLeftView (selectLegacyView ViewPrecipCurrent)
          ; Just WidgetViewVegetation -> whenLeftView (selectLegacyView ViewVegetation)
          ; Just WidgetViewTerrainForm -> whenLeftView (selectLegacyView ViewTerrainForm)
          ; Just WidgetViewPlateId -> whenLeftView (selectLegacyView ViewPlateId)
          ; Just WidgetViewPlateBoundary -> whenLeftView (selectLegacyView ViewPlateBoundary)
          ; Just WidgetViewPlateHardness -> whenLeftView (selectLegacyView ViewPlateHardness)
          ; Just WidgetViewPlateCrust -> whenLeftView (selectLegacyView ViewPlateCrust)
          ; Just WidgetViewPlateAge -> whenLeftView (selectLegacyView ViewPlateAge)
          ; Just WidgetViewPlateHeight -> whenLeftView (selectLegacyView ViewPlateHeight)
          ; Just WidgetViewPlateVelocity -> whenLeftView (selectLegacyView ViewPlateVelocity)
          ; Just WidgetViewCloud -> whenLeftView (selectLegacyView ViewCloud)
          ; Just WidgetViewCloudTypical -> whenLeftView (selectLegacyView ViewCloudTypical)
          ; Just WidgetDayNightToggle -> whenLeftView (submit UiActionToggleDayNight)
          -- Overlay cycling: prev/next overlay name, prev/next field
          ; Just WidgetViewOverlayPrev -> whenLeftView $ cycleOverlay uiSnap uiHandle (ieTerrainSnapshot widgetEnv) (-1) submit
          ; Just WidgetViewOverlayNext -> whenLeftView $ cycleOverlay uiSnap uiHandle (ieTerrainSnapshot widgetEnv) 1 submit
          ; Just WidgetViewFieldPrev -> whenLeftView $ cycleOverlayField uiSnap uiHandle (ieTerrainSnapshot widgetEnv) (-1) submit
          ; Just WidgetViewFieldNext -> whenLeftView $ cycleOverlayField uiSnap uiHandle (ieTerrainSnapshot widgetEnv) 1 submit
          ; Just WidgetOverlayManager -> whenLeftView (runServiceWithLog "Overlay manager" "get_overlays" Null)
          ; Just WidgetOverlaySchema -> whenLeftView (withCurrentOverlay uiSnap $ \name -> runServiceWithLog "Overlay schema" "get_overlay_schema" (object ["overlay" .= name]))
          ; Just WidgetOverlayProvenance -> whenLeftView (withCurrentOverlay uiSnap $ \name -> runServiceWithLog "Overlay provenance" "get_overlay_provenance" (object ["overlay" .= name]))
          ; Just WidgetOverlayExport -> whenLeftView (withCurrentOverlay uiSnap $ \name -> runServiceWithLog "Overlay export" "export_overlay_data" (object ["overlay" .= name]))
          ; Just WidgetOverlayImportValidate -> whenLeftView (runServiceWithLog "Overlay import validation" "validate_overlay_import" Null)
          ; Just WidgetLogDebug -> setLogMinLevel logHandle LogDebug
          ; Just WidgetLogInfo -> setLogMinLevel logHandle LogInfo
          ; Just WidgetLogWarn -> setLogMinLevel logHandle LogWarn
          ; Just WidgetLogError -> setLogMinLevel logHandle LogError
          ; Just WidgetLogHeader -> toggleLog
          ; Just _ -> pure ()
          ; Nothing -> pure ()
          }
  where
    widgetEnv :: InputEnv
    widgetEnv = icInputEnv inputContext

    actorHandles :: ActorHandles
    actorHandles = ieActorHandles widgetEnv

    uiHandle :: ActorHandle Ui (Protocol Ui)
    uiHandle = ahUiHandle actorHandles

    logHandle = ahLogHandle actorHandles

    quitRef = icQuitRef inputContext

    isPipelineScrollWidget :: WidgetId -> Bool
    isPipelineScrollWidget wid = case wid of
      WidgetPipelineToggle _ -> True
      WidgetSimTick -> True
      WidgetSimAutoTick -> True
      WidgetPluginMoveUp _ -> True
      WidgetPluginMoveDown _ -> True
      WidgetPluginToggle _ -> True
      WidgetPluginExpand _ -> True
      WidgetPluginParamSlider _ _ -> True
      WidgetPluginParamCheck _ _ -> True
      _ -> False

    -- Controls like presets, pipeline toggles, and sim/plugin management do
    -- not fit the generic slider model and stay on an explicit bespoke path.
    isBespokeConfigWidget :: WidgetId -> Bool
    isBespokeConfigWidget wid = case wid of
      WidgetConfigPresetSave -> True
      WidgetConfigPresetLoad -> True
      WidgetConfigReset -> True
      WidgetConfigRevert -> True
      WidgetPipelineToggle _ -> True
      WidgetSimTick -> True
      WidgetSimAutoTick -> True
      WidgetPluginMoveUp _ -> True
      WidgetPluginMoveDown _ -> True
      WidgetPluginToggle _ -> True
      WidgetPluginExpand _ -> True
      WidgetPluginParamSlider _ _ -> True
      WidgetPluginParamCheck _ _ -> True
      WidgetDataPluginSelect _ -> True
      WidgetDataResourceSelect _ _ -> True
      WidgetDataPagePrev _ _ -> True
      WidgetDataPageNext _ _ -> True
      WidgetDataRecordSelect _ -> True
      WidgetDataDetailDismiss -> True
      WidgetDataFieldToggle _ -> True
      WidgetDataEditToggle -> True
      WidgetDataEditSave -> True
      WidgetDataEditCancel -> True
      WidgetDataCreateNew -> True
      WidgetDataDeleteBtn -> True
      WidgetDataDeleteConfirm -> True
      WidgetDataDeleteCancel -> True
      WidgetDataFieldTextClick _ -> True
      WidgetDataFieldStepMinus _ -> True
      WidgetDataFieldStepPlus _ -> True
      WidgetDataFieldBoolToggle _ -> True
      WidgetDataFieldEnumPrev _ -> True
      WidgetDataFieldEnumNext _ -> True
      _ -> False

    bespokeConfigWidgetAllowed :: ConfigTab -> WidgetId -> Bool
    bespokeConfigWidgetAllowed tab wid = case wid of
      WidgetPipelineToggle _ -> tab == ConfigPipeline
      WidgetSimTick -> tab == ConfigPipeline
      WidgetSimAutoTick -> tab == ConfigPipeline
      WidgetPluginMoveUp _ -> tab == ConfigPipeline
      WidgetPluginMoveDown _ -> tab == ConfigPipeline
      WidgetPluginToggle _ -> tab == ConfigPipeline
      WidgetPluginExpand _ -> tab == ConfigPipeline
      WidgetPluginParamSlider _ _ -> tab == ConfigPipeline
      WidgetPluginParamCheck _ _ -> tab == ConfigPipeline
      WidgetDataPluginSelect _ -> tab == ConfigData
      WidgetDataResourceSelect _ _ -> tab == ConfigData
      WidgetDataPagePrev _ _ -> tab == ConfigData
      WidgetDataPageNext _ _ -> tab == ConfigData
      WidgetDataRecordSelect _ -> tab == ConfigData
      WidgetDataDetailDismiss -> tab == ConfigData
      WidgetDataFieldToggle _ -> tab == ConfigData
      WidgetDataEditToggle -> tab == ConfigData
      WidgetDataEditSave -> tab == ConfigData
      WidgetDataEditCancel -> tab == ConfigData
      WidgetDataCreateNew -> tab == ConfigData
      WidgetDataDeleteBtn -> tab == ConfigData
      WidgetDataDeleteConfirm -> tab == ConfigData
      WidgetDataDeleteCancel -> tab == ConfigData
      WidgetDataFieldTextClick _ -> tab == ConfigData
      WidgetDataFieldStepMinus _ -> tab == ConfigData
      WidgetDataFieldStepPlus _ -> tab == ConfigData
      WidgetDataFieldBoolToggle _ -> tab == ConfigData
      WidgetDataFieldEnumPrev _ -> tab == ConfigData
      WidgetDataFieldEnumNext _ -> tab == ConfigData
      _ -> True

    handleBespokeConfigWidget :: Layout -> WidgetId -> V2 Int -> (IO () -> IO ()) -> UiState -> IO ()
    handleBespokeConfigWidget currentLayout wid clickPoint whenConfigVisible uiState = case wid of
      WidgetConfigPresetSave -> whenConfigVisible (openPresetSaveDialog currentLayout uiState)
      WidgetConfigPresetLoad -> whenConfigVisible (openPresetLoadDialog currentLayout)
      WidgetConfigReset -> whenConfigVisible (SDL.stopTextInput >> clickWidget "WidgetConfigReset")
      WidgetConfigRevert -> whenConfigVisible (clickWidget "WidgetConfigRevert")
      WidgetPipelineToggle name -> whenConfigVisible $
        case parseStageId name of
          Nothing -> pure ()
          Just sid -> do
            let enabled = Set.member sid (uiDisabledStages uiState)
            runService "set_stage_enabled" (object ["stage" .= name, "enabled" .= enabled])
      WidgetSimTick -> whenConfigVisible $
        runService "sim_tick" (object ["count" .= (1 :: Int)])
      WidgetSimAutoTick -> whenConfigVisible $
        runService "set_sim_auto_tick" (object ["enabled" .= not (uiSimAutoTick uiState)])
      WidgetPluginMoveUp name -> whenConfigVisible $
        clickWidget ("WidgetPluginMoveUp:" <> name)
      WidgetPluginMoveDown name -> whenConfigVisible $
        clickWidget ("WidgetPluginMoveDown:" <> name)
      WidgetPluginToggle name -> whenConfigVisible $ do
        let enabled = Set.member name (uiDisabledPlugins uiState)
        runService "set_plugin_enabled" (object ["name" .= name, "enabled" .= enabled])
      WidgetPluginExpand name -> whenConfigVisible $ do
        let current = Map.findWithDefault False name (uiPluginExpanded uiState)
        setUiPluginExpanded uiHandle name (not current)
      WidgetPluginParamSlider pluginName paramName -> whenConfigVisible $ do
        let specs = Map.findWithDefault [] pluginName (uiPluginParamSpecs uiState)
            mSpec = find (\s -> rpsName s == paramName) specs
            value = pipelineParamValueFromClick currentLayout clickPoint mSpec
        runService "set_plugin_param" (object ["plugin" .= pluginName, "param" .= paramName, "value" .= value])
      WidgetPluginParamCheck pluginName paramName -> whenConfigVisible $
        runService "set_plugin_param" (object
          [ "plugin" .= pluginName
          , "param" .= paramName
          , "value" .= pipelineParamToggleValue uiState pluginName paramName
          ])
      WidgetDataPluginSelect pluginName -> whenConfigVisible $
        applyDataBrowserAction uiState (DataBrowser.DataBrowserSelectPlugin pluginName)
      WidgetDataResourceSelect pluginName resourceName -> whenConfigVisible $
        applyDataBrowserAction uiState (DataBrowser.DataBrowserSelectResource pluginName resourceName)
      WidgetDataPagePrev _pluginName _resourceName -> whenConfigVisible $
        applyDataBrowserAction uiState (DataBrowser.DataBrowserQueryRecords DataBrowserPagePrevious)
      WidgetDataPageNext _pluginName _resourceName -> whenConfigVisible $
        applyDataBrowserAction uiState (DataBrowser.DataBrowserQueryRecords DataBrowserPageNext)
      WidgetDataRecordSelect idx -> whenConfigVisible $
        applyDataBrowserAction uiState (DataBrowser.DataBrowserSelectRecord idx)
      WidgetDataDetailDismiss -> whenConfigVisible $ do
        applyDataBrowserAction uiState DataBrowser.DataBrowserDismissRecord
        SDL.stopTextInput
      WidgetDataFieldToggle path -> whenConfigVisible $
        applyDataBrowserAction uiState (DataBrowser.DataBrowserToggleField path)
      WidgetDataEditToggle -> whenConfigVisible $ do
        let action = if dbsEditMode (uiDataBrowser uiState)
              then DataBrowser.DataBrowserCancelEdit
              else DataBrowser.DataBrowserStartEdit
        applyDataBrowserAction uiState action
        when (action == DataBrowser.DataBrowserCancelEdit) SDL.stopTextInput
      WidgetDataEditSave -> whenConfigVisible $ do
        let action = if dbsCreateMode (uiDataBrowser uiState)
              then DataBrowser.DataBrowserCreateRecord
              else DataBrowser.DataBrowserUpdateRecord
        applyDataBrowserAction uiState action
        SDL.stopTextInput
      WidgetDataEditCancel -> whenConfigVisible $ do
        applyDataBrowserAction uiState DataBrowser.DataBrowserCancelEdit
        SDL.stopTextInput
      WidgetDataCreateNew -> whenConfigVisible $
        applyDataBrowserAction uiState DataBrowser.DataBrowserStartCreate
      WidgetDataDeleteBtn -> whenConfigVisible $
        applyDataBrowserAction uiState DataBrowser.DataBrowserRequestDelete
      WidgetDataDeleteConfirm -> whenConfigVisible $
        applyDataBrowserAction uiState DataBrowser.DataBrowserDeleteRecord
      WidgetDataDeleteCancel -> whenConfigVisible $
        applyDataBrowserAction uiState DataBrowser.DataBrowserCancelDelete
      WidgetDataFieldTextClick path -> whenConfigVisible $ do
        applyDataBrowserAction uiState (DataBrowser.DataBrowserFocusField path)
        let Rect (V2 rx ry, V2 rw rh) = configPanelRect currentLayout
            rawRect = Raw.Rect (fromIntegral rx) (fromIntegral ry) (fromIntegral rw) (fromIntegral rh)
        SDL.startTextInput rawRect
      WidgetDataFieldStepMinus path -> whenConfigVisible $
        applyDataBrowserAction uiState (DataBrowser.DataBrowserStepNumberField path (-1))
      WidgetDataFieldStepPlus path -> whenConfigVisible $
        applyDataBrowserAction uiState (DataBrowser.DataBrowserStepNumberField path 1)
      WidgetDataFieldBoolToggle path -> whenConfigVisible $
        applyDataBrowserAction uiState (DataBrowser.DataBrowserToggleBoolField path)
      WidgetDataFieldEnumPrev path -> whenConfigVisible $
        applyDataBrowserAction uiState (DataBrowser.DataBrowserCycleEnumField path (-1))
      WidgetDataFieldEnumNext path -> whenConfigVisible $
        applyDataBrowserAction uiState (DataBrowser.DataBrowserCycleEnumField path 1)
      _ -> pure ()

    applyDataBrowserAction _uiState action = do
      _ <- submitDataBrowserAction
        (ieDataBrowserExecutor widgetEnv)
        (runInputService widgetEnv)
        action
      pure ()

    handleMenuClick ly _widgets point = do
      uiSnap <- getUiSnapshot uiHandle
      case uiMenuMode uiSnap of
        MenuEscape -> handleEscapeMenuClick ly point
        MenuPresetSave -> handlePresetSaveClick ly point uiSnap
        MenuPresetLoad -> handlePresetLoadClick ly point uiSnap
        MenuWorldSave  -> handleWorldSaveClick ly point uiSnap
        MenuWorldLoad  -> handleWorldLoadClick ly point uiSnap
        _ -> pure ()

    handleEscapeMenuClick ly point
      | containsPoint (menuExitRect ly) point = do
          setUiMenuMode uiHandle MenuNone
          writeIORef quitRef True
      | containsPoint (menuSaveRect ly) point = do
          uiSnap <- getUiSnapshot uiHandle
          openWorldSaveDialog ly uiSnap
      | containsPoint (menuLoadRect ly) point =
          openWorldLoadDialog ly
      | otherwise = setUiMenuMode uiHandle MenuNone

    handlePresetSaveClick ly point uiSnap
      | containsPoint (presetSaveOkRect ly) point = confirmPresetSave uiSnap
      | containsPoint (presetSaveCancelRect ly) point = cancelPresetDialog
      | otherwise = pure ()

    handlePresetLoadClick ly point uiSnap
      | containsPoint (presetLoadOkRect ly) point = confirmPresetLoad uiSnap
      | containsPoint (presetLoadCancelRect ly) point = cancelPresetDialog
      | containsPoint (presetLoadListRect ly) point = do
          let V2 _mx my = point
              Rect (V2 _lx listY, _) = presetLoadListRect ly
              fText = Text.toLower (uiPresetFilter uiSnap)
              filteredCount = length (filter (\n -> Text.isInfixOf fText (Text.toLower n)) (uiPresetList uiSnap))
              idx = min (max 0 (filteredCount - 1)) ((my - listY) `div` 24)
          when (filteredCount > 0) $ setUiPresetSelected uiHandle idx
      | otherwise = pure ()

    -- Preset dialog helpers

    openPresetSaveDialog ly uiSnap = do
      let defaultName = "preset-" <> Text.pack (show (uiSeed uiSnap))
          Rect (V2 rx ry, V2 rw rh) = presetSaveInputRect ly
          rawRect = Raw.Rect (fromIntegral rx) (fromIntegral ry) (fromIntegral rw) (fromIntegral rh)
      setUiPresetInput uiHandle defaultName
      setUiMenuMode uiHandle MenuPresetSave
      SDL.startTextInput rawRect

    openPresetLoadDialog ly = do
      names <- listSnapshots
      setUiPresetList uiHandle names
      setUiPresetSelected uiHandle 0
      setUiPresetFilter uiHandle Text.empty
      setUiMenuMode uiHandle MenuPresetLoad
      let Rect (V2 rx ry, V2 rw rh) = presetLoadFilterRect ly
          rawRect = Raw.Rect (fromIntegral rx) (fromIntegral ry) (fromIntegral rw) (fromIntegral rh)
      SDL.startTextInput rawRect

    cancelPresetDialog = do
      setUiMenuMode uiHandle MenuNone
      SDL.stopTextInput

    confirmPresetSave uiSnap = do
      let name = uiPresetInput uiSnap
      _ <- runInputService widgetEnv "save_preset" (object ["name" .= name])
      setUiMenuMode uiHandle MenuNone
      SDL.stopTextInput

    confirmPresetLoad uiSnap = do
      let fText = Text.toLower (uiPresetFilter uiSnap)
          names = filter (\n -> Text.isInfixOf fText (Text.toLower n)) (uiPresetList uiSnap)
          sel = uiPresetSelected uiSnap
      when (sel >= 0 && sel < length names) $ do
        let name = names !! sel
        _ <- runInputService widgetEnv "load_preset" (object ["name" .= name])
        pure ()
      setUiMenuMode uiHandle MenuNone

    -- World save/load click handlers

    handleWorldSaveClick ly point uiSnap
      | containsPoint (worldSaveOkRect ly) point = confirmWorldSave uiSnap
      | containsPoint (worldSaveCancelRect ly) point = cancelWorldDialog
      | otherwise = pure ()

    handleWorldLoadClick ly point uiSnap
      | containsPoint (worldLoadOkRect ly) point = confirmWorldLoad uiSnap
      | containsPoint (worldLoadCancelRect ly) point = cancelWorldDialog
      | containsPoint (worldLoadListRect ly) point = do
          let V2 _mx my = point
              Rect (V2 _lx listY, _) = worldLoadListRect ly
              fText = Text.toLower (uiWorldFilter uiSnap)
              filteredCount = length (filter (\m -> Text.isInfixOf fText (Text.toLower (wsmName m))) (uiWorldList uiSnap))
              idx = min (max 0 (filteredCount - 1)) ((my - listY) `div` 28)
          when (filteredCount > 0) $ setUiWorldSelected uiHandle idx
      | otherwise = pure ()

    -- World dialog helpers

    openWorldSaveDialog ly uiSnap = do
      let defaultName = uiWorldName uiSnap
          Rect (V2 rx ry, V2 rw rh) = worldSaveInputRect ly
          rawRect = Raw.Rect (fromIntegral rx) (fromIntegral ry) (fromIntegral rw) (fromIntegral rh)
      setUiWorldSaveInput uiHandle defaultName
      setUiMenuMode uiHandle MenuWorldSave
      SDL.startTextInput rawRect

    openWorldLoadDialog ly = do
      worlds <- listWorlds
      setUiWorldList uiHandle worlds
      setUiWorldSelected uiHandle 0
      setUiWorldFilter uiHandle Text.empty
      setUiMenuMode uiHandle MenuWorldLoad
      let Rect (V2 rx ry, V2 rw rh) = worldLoadFilterRect ly
          rawRect = Raw.Rect (fromIntegral rx) (fromIntegral ry) (fromIntegral rw) (fromIntegral rh)
      SDL.startTextInput rawRect

    cancelWorldDialog = do
      setUiMenuMode uiHandle MenuNone
      SDL.stopTextInput

    confirmWorldSave uiSnap = do
      let name = uiWorldSaveInput uiSnap
      when (not (Text.null name)) $ do
        _ <- runInputService widgetEnv "save_world" (object ["name" .= name])
        pure ()
      setUiMenuMode uiHandle MenuNone
      SDL.stopTextInput

    confirmWorldLoad uiSnap = do
      let fText = Text.toLower (uiWorldFilter uiSnap)
          manifests = filter (\m -> Text.isInfixOf fText (Text.toLower (wsmName m))) (uiWorldList uiSnap)
          sel = uiWorldSelected uiSnap
      when (sel >= 0 && sel < length manifests) $ do
        let manifest = manifests !! sel
            name = wsmName manifest
        _ <- runInputService widgetEnv "load_world" (object ["name" .= name])
        pure ()
      setUiMenuMode uiHandle MenuNone

    toggleLog = do
      logSnap <- getLogSnapshot logHandle
      setLogCollapsed logHandle (not (lsCollapsed logSnap))
    whenConfigVisible action = do
      let uiSnap = ieUiSnapshot widgetEnv
      when (uiShowConfig uiSnap) action
    toggleConfig = do
      uiSnap <- getUiSnapshot uiHandle
      setUiShowConfig uiHandle (not (uiShowConfig uiSnap))
    submit action =
      submitAction widgetEnv action
    selectBase baseMode =
      submit (UiActionSetBaseViewMode baseMode)
    selectOverlay overlayMode =
      submit (UiActionSetSkyOverlayMode overlayMode)
    selectBasis basis =
      let uiState = ieUiSnapshot widgetEnv
      in when (weatherBasisEnabled uiState) (submit (UiActionSetWeatherBasis basis))
    selectLegacyView mode =
      submit (UiActionSetViewMode mode)
    weatherBasisEnabled uiState = case lvsSkyOverlay (effectiveViewSelection uiState) of
      Just (SkyOverlayPlugin _ _) -> False
      Just _ -> True
      Nothing -> False
    runService method params =
      runInputService widgetEnv method params >> pure ()
    runServiceWithLog label method params = do
      result <- runInputService widgetEnv method params
      case result of
        Left err -> appendLog logHandle (LogEntry LogError (label <> " failed: " <> serviceErrorText err))
        Right (ServiceResponse body) -> appendLog logHandle (LogEntry LogInfo (label <> ": " <> summarizeServicePayload body))
      setLogCollapsed logHandle False
    summarizeServicePayload body =
      let rendered = Text.pack (show body)
      in if Text.length rendered > 240
           then Text.take 240 rendered <> "…"
           else rendered
    clickWidget :: Text.Text -> IO ()
    clickWidget widgetId =
      runService "click_widget" (object ["widget_id" .= widgetId])
    withCurrentOverlay uiState action =
      case currentOverlayName uiState (ieTerrainSnapshot widgetEnv) of
        Just name -> action name
        Nothing -> do
          appendLog logHandle (LogEntry LogWarn "Overlay action requires an active or available overlay")
          setLogCollapsed logHandle False
    setConfigTab tabName =
      runService "set_config_tab" (object ["tab" .= (tabName :: Text.Text)])
    setConfigSlider sliderDef sliderPart = do
      uiSnap <- getUiSnapshot uiHandle
      let sid = sliderId sliderDef
          newValue = configSliderInputValueForId uiSnap sid sliderPart
      runService "set_slider" (object ["name" .= Text.pack (show sid), "value" .= newValue])
    startSeedEdit rect = do
      let uiSnap = ieUiSnapshot widgetEnv
          Rect (V2 rx ry, V2 rw rh) = rect
          rawRect = Raw.Rect (fromIntegral rx) (fromIntegral ry) (fromIntegral rw) (fromIntegral rh)
      SDL.startTextInput rawRect
      setUiSeedEditing uiHandle True
      setUiSeedInput uiHandle (Text.pack (show (uiSeed uiSnap)))
    randomSeed = do
      seed <- (randomIO :: IO Word64)
      runService "set_seed" (object ["seed" .= seed])
      setUiSeedEditing uiHandle False
      SDL.stopTextInput

-- | Cycle through available overlay names by @dir@ (+1 or -1).
--
-- When no overlays are available, does nothing.  When cycling past the
-- end, clears the overlay; from no overlay, wraps to
-- the first overlay.
cycleOverlay :: UiState -> ActorHandle Ui (Protocol Ui) -> TerrainSnapshot -> Int -> (UiAction -> IO ()) -> IO ()
cycleOverlay uiSnap uiHandle terrainSnap dir submit = do
  let names = availableOverlayNames uiSnap terrainSnap
      selection = effectiveViewSelection uiSnap
  if null names
    then pure ()
    else do
      let currentIdx = case lvsSkyOverlay selection of
            Just (SkyOverlayPlugin name _) ->
              case findIndex (== name) names of
                Just i  -> i + 1  -- +1 because index 0 = "no overlay"
                Nothing -> 0
            _ -> 0
          total = length names + 1  -- +1 for "no overlay" position
          newIdx = (currentIdx + dir) `mod` total
      if newIdx == 0
        then submit (UiActionSetViewSelection selection { lvsSkyOverlay = Nothing })
        else do
          let overlayName = names !! (newIdx - 1)
              fields = fieldsForOverlayName uiSnap terrainSnap overlayName
          setUiOverlayFields uiHandle fields
          -- Reset to first field when switching plugin overlay.
          submit (UiActionSetViewSelection selection { lvsSkyOverlay = Just (SkyOverlayPlugin overlayName 0) })

-- | Cycle through fields within the currently-selected overlay.
--
-- Only effective when a plugin sky overlay with fields is selected.
cycleOverlayField :: UiState -> ActorHandle Ui (Protocol Ui) -> TerrainSnapshot -> Int -> (UiAction -> IO ()) -> IO ()
cycleOverlayField uiSnap uiHandle terrainSnap dir submit =
  case lvsSkyOverlay selection of
    Just (SkyOverlayPlugin name fieldIdx) -> do
      let fields = fieldsForOverlayName uiSnap terrainSnap name
          fieldCount = length fields
      if fieldCount <= 0
        then pure ()
        else do
          let newIdx = (fieldIdx + dir) `mod` fieldCount
          setUiOverlayFields uiHandle fields
          submit (UiActionSetViewSelection selection { lvsSkyOverlay = Just (SkyOverlayPlugin name newIdx) })
    _ -> pure ()
  where
    selection = effectiveViewSelection uiSnap

availableOverlayNames :: UiState -> TerrainSnapshot -> [Text.Text]
availableOverlayNames uiSnap terrainSnap =
  uiOverlayNames uiSnap ++ [n | n <- overlayNames (tsOverlayStore terrainSnap), n `notElem` uiOverlayNames uiSnap]

currentOverlayName :: UiState -> TerrainSnapshot -> Maybe Text.Text
currentOverlayName uiSnap terrainSnap =
  case lvsSkyOverlay (effectiveViewSelection uiSnap) of
    Just (SkyOverlayPlugin name _) -> Just name
    _ -> listToMaybe (availableOverlayNames uiSnap terrainSnap)

fieldsForOverlayName :: UiState -> TerrainSnapshot -> Text.Text -> [(Text.Text, OverlayFieldType)]
fieldsForOverlayName uiSnap terrainSnap name =
  case lookupOverlay name (tsOverlayStore terrainSnap) of
    Just ov -> fieldsForOverlay ov
    Nothing -> uiOverlayFields uiSnap

fieldsForOverlay :: Overlay -> [(Text.Text, OverlayFieldType)]
fieldsForOverlay ov = map fieldPair (osFields (ovSchema ov))
  where
    fieldPair fd = (ofdName fd, ofdType fd)

