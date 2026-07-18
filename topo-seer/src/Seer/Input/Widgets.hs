{-# LANGUAGE OverloadedStrings #-}

module Seer.Input.Widgets
  ( handleClick
  , submitDataBrowserInputAction
  , clipboardFromWidgetResponse
  ) where

import Actor.Log (LogSnapshot(..))
import Data.Aeson (Value, object, (.=), (.:?))
import qualified Data.Aeson.Types as Aeson

import Actor.UI
  ( ConfigTab(..)
  , DataBrowserState(..)
  , LeftTab(..)
  , Ui
  , UiMenuMode(..)
  , UiState(..)
  , configRowCount
  , getUiSnapshot
  , setUiConfigScroll
  , setUiContextHex
  , setUiContextPos
  , setUiMenuMode
  , setUiSeedEditing
  , setUiSeedInput
  , setUiOverlayInspectorNotice
  )
import Control.Applicative ((<|>))
import Control.Exception (SomeException, onException, try)
import Control.Monad (void, when)
import Data.IORef (writeIORef)
import Data.Int (Int32)
import Data.List (find, partition)
import Data.Word (Word64)
import Data.Maybe (isJust)
import qualified Data.Text as Text
import Linear (V2(..))
import qualified SDL
import qualified SDL.Raw.Types as Raw
import Seer.Config.PresetCatalogue (presetCatalogueMatches)
import Seer.Config.SliderRegistry
  ( SliderDef(..)
  , sliderDefForWidget
  )
import Seer.Config.SliderUi (configTabForSliderTab)
import Seer.Draw (seedMaxDigits)
import Seer.World.Persist.Types (WorldSaveManifest(..))
import qualified Seer.DataBrowser.AppService as DataBrowser
import Seer.DataBrowser.Executor (DataBrowserExecutor, submitDataBrowserAction)
import UI.Components.PipelineControls (pipelineScrollOffset)
import UI.Layout
import UI.WidgetTree
  ( Widget(..)
  , WidgetId(..)
  , buildWidgets
  , buildMenuWidgets
  , buildPluginWidgets
  , buildDataBrowserWidgets
  , buildDataDetailWidgetsForState
  , hitTest
  , isLeftViewWidget
  )
import UI.Widgets (Rect(..), containsPoint)
import UI.WidgetId (widgetIdToText)
import System.Random (randomIO)
import Hyperspace.Actor (ActorHandle, Protocol)
import Actor.UiActions (ActorHandles(..))
import Seer.Command.Handlers.Widgets (executeLocalWidgetInvocation)
import Seer.Input.Actions (InputEnv(..), runInputService)
import Seer.Input.Context
  ( InputContext(..)
  , enqueueInputAction
  , enqueueInputMainThreadAction
  )
import Seer.Service.Types (ServiceResponse(..), serviceErrorText)

-- | Non-blocking Data Browser submission boundary used by SDL widget input.
-- Completion remains owned by the application-scoped executor.
submitDataBrowserInputAction
  :: DataBrowserExecutor
  -> DataBrowser.DataBrowserRunService
  -> DataBrowser.DataBrowserAppAction
  -> IO ()
submitDataBrowserInputAction executor runService action =
  void (submitDataBrowserAction executor runService action)

clipboardFromWidgetResponse :: Value -> Maybe Text.Text
clipboardFromWidgetResponse response =
  Aeson.parseMaybe (Aeson.withObject "widget response" (.:? "clipboard")) response >>= id

handleClick
  :: InputContext
  -> SDL.Point V2 Int32
  -> IO ()
handleClick inputContext (SDL.P (V2 x y)) = do
  let window = icWindow inputContext
      widgetEnv = icInputEnv inputContext
      actorHandles = ieActorHandles widgetEnv
      uiHandle = ahUiHandle actorHandles
      quitRef = icQuitRef inputContext
  (V2 winW winH) <- SDL.get (SDL.windowSize window)
  let logSnap = ieLogSnapshot widgetEnv
      uiSnap = ieUiSnapshot widgetEnv
      point = V2 (fromIntegral x) (fromIntegral y)
      logHeight = if lsCollapsed logSnap then 24 else 160
      seedWidth = max 120 (seedMaxDigits * 10)
      layout = layoutForSeed (V2 (fromIntegral winW) (fromIntegral winH)) logHeight seedWidth
      scrollArea = configScrollAreaRect layout
      scrollBar = configScrollBarRect layout
      inScrollBar = uiShowConfig uiSnap && containsPoint scrollBar point
      inConfigScroll = uiShowConfig uiSnap && containsPoint scrollArea point
      configScrollForHit = case uiConfigTab uiSnap of
        ConfigPipeline -> pipelineScrollOffset uiSnap layout
        tab -> configScrollOffsetForRows
          (configRowCount tab uiSnap) (uiConfigScroll uiSnap) layout
      scrollPoint = if inConfigScroll
        then V2 (fromIntegral x) (fromIntegral y + configScrollForHit)
        else point
      dbs_ = uiDataBrowser uiSnap
      detailWidgets
        | uiShowConfig uiSnap
        , uiConfigTab uiSnap == ConfigData =
            buildDataDetailWidgetsForState (uiDataResources uiSnap) dbs_ layout
        | otherwise = []
      widgetsAll = buildWidgets layout
                ++ buildPluginWidgets (uiPluginNames uiSnap) (uiPluginExpanded uiSnap) (uiPluginParamSpecs uiSnap) (uiPluginDiagnosticLines uiSnap) layout
                ++ buildDataBrowserWidgets (uiDataResources uiSnap) dbs_ layout
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
      (pipelineWidgets, afterPipelineWidgets) = partition (isPipelineScrollWidget . widgetId) otherWidgets
      (dataBrowserWidgets, nonScrollWidgets) = partition (isDataBrowserScrollWidget . widgetId) afterPipelineWidgets
      dataDeleteConfirmationVisible =
        uiShowConfig uiSnap
          && uiConfigTab uiSnap == ConfigData
          && dbsDeleteConfirm dbs_
      detailHit = hitTest detailWidgets point
      hitWidget
        | dataDeleteConfirmationVisible = detailHit
        | otherwise = detailHit <|>
            if inConfigScroll
              then case hitTest configSliderWidgets scrollPoint
                  <|> hitTest pipelineWidgets scrollPoint
                  <|> hitTest dataBrowserWidgets scrollPoint of
                Just wid -> Just wid
                Nothing ->
                  let viewHit = if inLeftViewContent then hitTest leftViewWidgets leftViewAdjPoint else Nothing
                  in viewHit <|> hitTest nonScrollWidgets point
              else if inLeftViewContent
                then hitTest leftViewWidgets leftViewAdjPoint <|> hitTest nonScrollWidgets point
                else hitTest nonScrollWidgets point
      configWidgetAllowed tab widget =
        case sliderDefForWidget (widgetId widget) of
          Just sliderDef -> tab == configTabForSliderTab (sliderTab sliderDef)
          Nothing -> bespokeConfigWidgetAllowed tab (widgetId widget)
  if uiMenuMode uiSnap /= MenuNone
    then case hitTest (buildMenuWidgets uiSnap layout) point of
      Just wid -> dispatchSemanticClick layout widgets point uiSnap wid
      Nothing -> when (uiMenuMode uiSnap == MenuEscape) $ setUiMenuMode uiHandle MenuNone
    else if inScrollBar && not dataDeleteConfirmationVisible
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
      case hitWidget of
        Just wid -> dispatchSemanticClick layout widgets scrollPoint uiSnap wid
        Nothing -> pure ()
  where
    widgetEnv :: InputEnv
    widgetEnv = icInputEnv inputContext

    actorHandles :: ActorHandles
    actorHandles = ieActorHandles widgetEnv

    uiHandle :: ActorHandle Ui (Protocol Ui)
    uiHandle = ahUiHandle actorHandles

    quitRef = icQuitRef inputContext

    -- Coordinate routing stays local; every semantic action enters the same
    -- serialized interpreter as AppService automation.
    dispatchSemanticClick currentLayout currentWidgets clickPoint uiState wid = case wid of
      WidgetSeedValue -> runLocalWidget wid $ \freshUi ->
        startSeedEdit freshUi (configSeedValueRect currentLayout)
      WidgetSeedRandom -> runLocalWidget wid (const randomSeed)
      WidgetMenuExit -> runLocalWidget wid $ \_ -> do
        setUiMenuMode uiHandle MenuNone
        _ <- getUiSnapshot uiHandle
        writeIORef quitRef True
        pure (Right ())
      _ -> do
        when (opensOverlayInspector wid) $
          writeIORef (icModalBarrierLatchRef inputContext) True
        enqueueInputAction (icActionDispatcher inputContext) $ do
          result <- runInputService widgetEnv "click_widget" (object $
            ["widget_id" .= widgetIdToText wid]
              ++ normalizedArgument currentWidgets clickPoint wid
              ++ itemIndexArgument currentLayout clickPoint uiState wid)
            `onException` when (opensOverlayInspector wid)
              (writeIORef (icModalBarrierLatchRef inputContext) False)
          case result of
            Right (ServiceResponse response) ->
              queueLocalHook currentLayout wid (clipboardFromWidgetResponse response)
            Left _ -> when (opensOverlayInspector wid) $
              writeIORef (icModalBarrierLatchRef inputContext) False

    opensOverlayInspector wid = wid `elem`
      [ WidgetOverlayManager
      , WidgetOverlaySchema
      , WidgetOverlayProvenance
      , WidgetOverlayExport
      , WidgetOverlayImportValidate
      ]

    runLocalWidget wid action =
      void (executeLocalWidgetInvocation actorHandles wid action)

    normalizedArgument currentWidgets (V2 clickX _) wid = case wid of
      WidgetPluginParamSlider _ _ -> case find ((== wid) . widgetId) currentWidgets of
        Just (Widget _ (Rect (V2 rectX _, V2 rectW _))) ->
          let normalized = max 0 (min 1 (fromIntegral (clickX - rectX) / fromIntegral (max 1 rectW) :: Double))
          in ["normalized_position" .= normalized]
        Nothing -> []
      _ -> []

    itemIndexArgument currentLayout (V2 _ clickY) uiState wid = case wid of
      WidgetPresetLoadItem ->
        let Rect (V2 _ listY, _) = presetLoadListRect currentLayout
            count = length (filter (presetCatalogueMatches (uiPresetFilter uiState)) (uiPresetList uiState))
        in if count <= 0 then [] else ["item_index" .= min (count - 1) (max 0 ((clickY - listY) `div` 24))]
      WidgetWorldLoadItem ->
        let Rect (V2 _ listY, _) = worldLoadListRect currentLayout
            query = Text.toLower (uiWorldFilter uiState)
            count = length (filter (Text.isInfixOf query . Text.toLower . wsmName) (uiWorldList uiState))
        in if count <= 0 then [] else ["item_index" .= min (count - 1) (max 0 ((clickY - listY) `div` 28))]
      _ -> []

    queueLocalHook currentLayout wid clipboard = case wid of
      WidgetDataEditToggle -> do
        latest <- getUiSnapshot uiHandle
        let dbs = uiDataBrowser latest
        when (not (dbsEditMode dbs || dbsCreateMode dbs)) $
          enqueueInputMainThreadAction (icActionDispatcher inputContext) SDL.stopTextInput
      _ -> enqueueInputMainThreadAction (icActionDispatcher inputContext)
        (runLocalHook currentLayout wid clipboard)

    -- These SDL calls are drained by the event pump on its owning thread.
    runLocalHook currentLayout wid clipboard = case wid of
      WidgetConfigReset -> SDL.stopTextInput
      WidgetConfigPresetSave -> startTextAt (presetSaveInputRect currentLayout)
      WidgetConfigPresetLoad -> startTextAt (presetLoadFilterRect currentLayout)
      WidgetMenuSave -> startTextAt (worldSaveInputRect currentLayout)
      WidgetMenuLoad -> startTextAt (worldLoadFilterRect currentLayout)
      WidgetPresetSaveOk -> SDL.stopTextInput
      WidgetPresetSaveCancel -> SDL.stopTextInput
      WidgetPresetLoadOk -> SDL.stopTextInput
      WidgetPresetLoadCancel -> SDL.stopTextInput
      WidgetWorldSaveOk -> SDL.stopTextInput
      WidgetWorldSaveCancel -> SDL.stopTextInput
      WidgetWorldLoadOk -> SDL.stopTextInput
      WidgetWorldLoadCancel -> SDL.stopTextInput
      WidgetDataFieldTextClick _ -> startTextAt (configPanelRect currentLayout)
      WidgetDataDetailDismiss -> SDL.stopTextInput
      WidgetDataEditSave -> SDL.stopTextInput
      WidgetDataEditCancel -> SDL.stopTextInput
      WidgetOverlayInspectorClose -> SDL.stopTextInput
      WidgetOverlayManager -> SDL.stopTextInput
      WidgetOverlaySchema -> SDL.stopTextInput
      WidgetOverlayProvenance -> SDL.stopTextInput
      WidgetOverlayExport -> SDL.stopTextInput
      WidgetOverlayImportValidate -> startTextAt (overlayInspectorImportInputRect currentLayout)
      WidgetOverlayInspectorImportInput -> startTextAt (overlayInspectorImportInputRect currentLayout)
      WidgetOverlayInspectorValidate -> SDL.stopTextInput
      WidgetOverlayInspectorCopy ->
        case clipboard of
          Just payloadText -> do
            copied <- try (SDL.setClipboardText payloadText) :: IO (Either SomeException ())
            setUiOverlayInspectorNotice uiHandle $ Just $ case copied of
              Right () -> "Copied exact export JSON to the clipboard."
              Left err -> "Clipboard copy failed: " <> Text.pack (show err)
          Nothing -> pure ()
      _ -> pure ()

    startTextAt (Rect (V2 rx ry, V2 rw rh)) =
      SDL.startTextInput (Raw.Rect (fromIntegral rx) (fromIntegral ry) (fromIntegral rw) (fromIntegral rh))

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

    isDataBrowserScrollWidget :: WidgetId -> Bool
    isDataBrowserScrollWidget wid = case wid of
      WidgetDataPluginSelect _ -> True
      WidgetDataResourceSelect _ _ -> True
      WidgetDataPagePrev _ _ -> True
      WidgetDataPageNext _ _ -> True
      WidgetDataRecordSelect _ -> True
      WidgetDataCreateNew -> True
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

    startSeedEdit uiSnap rect = do
      let Rect (V2 rx ry, V2 rw rh) = rect
          rawRect = Raw.Rect (fromIntegral rx) (fromIntegral ry) (fromIntegral rw) (fromIntegral rh)
      setUiSeedEditing uiHandle True
      setUiSeedInput uiHandle (Text.pack (show (uiSeed uiSnap)))
      _ <- getUiSnapshot uiHandle
      SDL.startTextInput rawRect
      pure (Right ())
    randomSeed = do
      seed <- (randomIO :: IO Word64)
      result <- runInputService widgetEnv "set_seed" (object ["seed" .= seed])
      case result of
        Left err -> pure (Left (serviceErrorText err))
        Right _ -> do
          setUiSeedEditing uiHandle False
          _ <- getUiSnapshot uiHandle
          SDL.stopTextInput
          pure (Right ())

