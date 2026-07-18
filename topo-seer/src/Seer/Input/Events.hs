{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Seer.Input.Events
  ( handleEvent
  , ModalEventRoute(..)
  , modalEventRoute
  , modalInputBarrierVisible
  , TooltipHover
  , tickTooltipHover
  , tooltipDelayMs
  ) where

import Actor.Data (getTerrainSnapshot)
import Actor.Log (LogSnapshot(..), getLogSnapshot, setLogScroll)
import Actor.UI
  ( ConfigTab(..)
  , DataBrowserState(..)
  , LeftTab(..)
  , Ui
  , UiMenuMode(..)
  , UiState(..)
  , getUiSnapshot
  , setUiConfigScroll
  , setUiHexTooltipPinned
  , setUiPanOffset
  , setUiHoverHex
  , setUiHoverWidget
  , setUiZoom
  , setUiLeftViewScroll
  , setUiOverlayInspectorScroll
  , setUiOverlayInspectorNotice
  )
import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.Aeson (object, (.=))
import Data.IORef (IORef, readIORef, writeIORef)
import qualified Data.Text as Text
import Data.Word (Word32)
import Linear (V2(..))
import qualified SDL
import qualified SDL.Raw.Types as Raw
import Hyperspace.Actor (ActorHandle, Protocol)
import Seer.Draw (seedMaxDigits)
import Seer.Draw.OverlayInspector (overlayInspectorViewScrollLimit)
import Seer.Input.ConfigScroll
  ( computeScrollUpdates
  , defaultScrollSettings
  )
import Seer.Input.Context
  ( DragState(..)
  , InputContext(..)
  , TooltipHover
  , enqueueInputAction
  , enqueueInputMainThreadAction
  )
import Seer.Input.Intent
  ( InputIntentEnv(..)
  , InputIntentResult(..)
  , InputKey(..)
  , KeyModifiers(..)
  , executeKeyIntent
  , executeTextIntent
  )
import Seer.Input.ViewControls
  ( applyZoomAtCursor
  , defaultZoomSettings
  , isViewportDrag
  , panViewportForDrag
  , pickTerrainHex
  )
import Seer.DataBrowser.Executor (submitDataBrowserAction)
import Seer.DataBrowser.Model (DataBrowserBeginResult(..))
import Seer.OverlayInspector.Model
  ( OverlayInspectorFocus(..)
  , OverlayInspectorModel(..)
  , OverlayInspectorView(..)
  )
import UI.Layout
import UI.WidgetTree (Widget(..), WidgetId(..), buildEditorWidgets, buildEditorReopenWidget, buildViewModeWidgets, buildSliderRowWidgets, hitTest)
import UI.WidgetId (widgetIdToText)
import UI.Widgets (Rect(..), containsPoint)
import Seer.Input.Actions (InputEnv(..), runInputService, submitAction)
import qualified Seer.Input.Actions as InputActions
import Seer.Editor.Types (EditorState(..), EditorTool(..))
import Actor.UiActions (UiAction(..))
import Actor.UiActions.Handles (ActorHandles(..))
import Seer.Input.Widgets (clipboardFromWidgetResponse, handleClick)
import Seer.Service.Types (ServiceResponse(..))

-- | Wall-clock delay (milliseconds) the cursor must remain still on a
-- widget before the tooltip appears.
tooltipDelayMs :: Word32
tooltipDelayMs = 500

-- | Routing decision for an SDL event while a modal barrier is active.
-- Keyboard and text events pass only while actor-owned modal state is live so
-- each dialog can retain its local controls. Pointer motion performs cleanup,
-- overlay-inspector wheel input remains local, and all other modal input is
-- either hit-tested against the dialog or discarded.
data ModalEventRoute
  = PassEvent
  | BlockEvent
  | BarrierPointerMotion
  | BarrierPointerButton
  | OverlayInspectorWheel
  deriving (Eq, Show)

-- | Whether the current UI presents an input-blocking dialog.
modalInputBarrierVisible :: UiState -> Bool
modalInputBarrierVisible ui =
  uiMenuMode ui /= MenuNone
    || (uiShowConfig ui
        && uiConfigTab ui == ConfigData
        && dbsDeleteConfirm (uiDataBrowser ui))

-- | Classify an SDL event against live and frame-latched modal state.
-- The latch keeps the rendered dialog authoritative for the whole event batch,
-- even when an earlier event closes it or an async action is still opening it.
modalEventRoute :: Bool -> UiState -> SDL.EventPayload -> ModalEventRoute
modalEventRoute modalLatched ui payload
  | not (modalLatched || modalInputBarrierVisible ui) = PassEvent
  | otherwise = case payload of
      SDL.MouseMotionEvent _ -> BarrierPointerMotion
      SDL.MouseButtonEvent _ -> BarrierPointerButton
      SDL.MouseWheelEvent _
        | uiMenuMode ui == MenuOverlayInspector -> OverlayInspectorWheel
        | otherwise -> BlockEvent
      SDL.TextInputEvent _
        | modalInputBarrierVisible ui && modalOwnsTextInput ui -> PassEvent
        | otherwise -> BlockEvent
      SDL.KeyboardEvent keyboardEvent
        | modalInputBarrierVisible ui
        , Just key <- inputKeyForSdl
            (SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent))
        , modalOwnsKey ui key -> PassEvent
        | otherwise -> BlockEvent
      _ -> BlockEvent

handleEvent
  :: InputContext
  -> SDL.Event
  -> IO ()
handleEvent inputContext event = do
  let window = icWindow inputContext
      inputEnv = icInputEnv inputContext
      actorHandles = ieActorHandles inputEnv
      uiHandle = ahUiHandle actorHandles
      logHandle = ahLogHandle actorHandles
      dataHandle = ahDataHandle actorHandles
      mousePosRef = icMousePosRef inputContext
      dragRef = icDragRef inputContext
      tooltipHoverRef = icTooltipHoverRef inputContext
  barrierUi <- getUiSnapshot uiHandle
  modalBarrierLatched <- readIORef (icModalBarrierLatchRef inputContext)
  let payload = SDL.eventPayload event
      eventRoute = modalEventRoute modalBarrierLatched barrierUi payload
  case payload of
    _ | eventRoute == BlockEvent -> pure ()
    SDL.MouseMotionEvent motionEvent
      | eventRoute == BarrierPointerMotion -> do
          let SDL.P (V2 mx my) = SDL.mouseMotionEventPos motionEvent
          writeIORef mousePosRef (fromIntegral mx, fromIntegral my)
          writeIORef dragRef Nothing
          writeIORef tooltipHoverRef Nothing
          setUiHoverWidget uiHandle Nothing
          setUiHoverHex uiHandle Nothing
    SDL.MouseWheelEvent wheelEvent
      | eventRoute == OverlayInspectorWheel -> do
          let SDL.V2 _ dy = SDL.mouseWheelEventPos wheelEvent
          when (dy /= 0) $ scrollOverlayInspector barrierUi (fromIntegral dy)
    SDL.MouseButtonEvent btnEvent
      | eventRoute == BarrierPointerButton -> do
          writeIORef dragRef Nothing
          when (SDL.mouseButtonEventMotion btnEvent == SDL.Released
              && SDL.mouseButtonEventButton btnEvent == SDL.ButtonLeft) $ do
            let editor = uiEditor barrierUi
            when (editorActive editor && editorTool editor == ToolFlatten) $
              clearEditorStrokeSession
          -- The frozen InputEnv supplies click geometry. Only dispatch when it
          -- describes the same still-live modal scope; otherwise this batch is
          -- between modal states and the click must remain swallowed.
          let cachedUi = ieUiSnapshot inputEnv
          when (sameModalInputScope cachedUi barrierUi
              && SDL.mouseButtonEventMotion btnEvent == SDL.Pressed
              && SDL.mouseButtonEventButton btnEvent == SDL.ButtonLeft) $
            handleClick inputContext (SDL.mouseButtonEventPos btnEvent)
    SDL.MouseMotionEvent motionEvent -> do
      let SDL.P (V2 mx my) = SDL.mouseMotionEventPos motionEvent
      writeIORef mousePosRef (fromIntegral mx, fromIntegral my)
      dragState <- readIORef dragRef
      case dragState of
        Just state -> do
          uiSnap <- InputActions.getUiSnapshot inputEnv
          let DragState { dsStart = start, dsLast = previous, dsDragging = dragging } = state
              current = (fromIntegral mx, fromIntegral my)
              startDrag = isViewportDrag start current
              dragOrigin = if dragging then previous else start
              newOffset = panViewportForDrag uiSnap dragOrigin current
          if dragging || startDrag
            then do
              setUiPanOffset uiHandle newOffset
              writeIORef dragRef (Just state { dsLast = (fromIntegral mx, fromIntegral my), dsDragging = True })
            else writeIORef dragRef (Just state { dsLast = (fromIntegral mx, fromIntegral my) })
        Nothing -> pure ()
      uiSnap <- getUiSnapshot uiHandle
      terrainSnap <- getTerrainSnapshot dataHandle
      let ((q, r), hexExists) = pickTerrainHex terrainSnap uiSnap (fromIntegral mx, fromIntegral my)
      if hexExists
        then setUiHoverHex uiHandle (Just (q, r))
        else setUiHoverHex uiHandle Nothing
      -- Editor drag-to-paint: apply brush continuously while left button held
      let buttons = SDL.mouseMotionEventState motionEvent
      when (SDL.ButtonLeft `elem` buttons) $ do
        uiSnapDrag <- getUiSnapshot uiHandle
        let editor = uiEditor uiSnapDrag
        when (editorActive editor) $
          case uiHoverHex uiSnapDrag of
            Just hex -> submitEditorBrushStroke hex
            Nothing  -> pure ()
      -- Widget hover detection for tooltips
      do (V2 winW winH) <- SDL.get (SDL.windowSize window)
         logSnap <- getLogSnapshot logHandle
         let logHeight = if lsCollapsed logSnap then 24 else 160
             seedWidth = max 120 (seedMaxDigits * 10)
             hoverLayout = layoutForSeed (V2 (fromIntegral winW) (fromIntegral winH)) logHeight seedWidth
             point = V2 (fromIntegral mx) (fromIntegral my)
             editor = uiEditor uiSnap
             -- Slider rows (scroll-adjusted, only when config panel open)
             sliderHit
               | uiShowConfig uiSnap =
                   let scrollArea = configScrollAreaRect hoverLayout
                       scrollOffset = uiConfigScroll uiSnap
                       scrolledPoint = V2 (fromIntegral mx) (fromIntegral my + scrollOffset)
                       (terrainRows, planetRows, climateRows, weatherRows, biomeRows, erosionRows) = buildSliderRowWidgets hoverLayout
                       activeRows = case uiConfigTab uiSnap of
                         ConfigTerrain -> terrainRows
                         ConfigPlanet -> planetRows
                         ConfigClimate -> climateRows
                         ConfigWeather -> weatherRows
                         ConfigBiome -> biomeRows
                         ConfigErosion -> erosionRows
                         ConfigPipeline -> []
                         ConfigData -> []
                   in if containsPoint scrollArea point
                        then hitTest activeRows scrolledPoint
                        else Nothing
               | otherwise = Nothing
             -- Chrome widgets are screen-space; scrolled View-tab widgets
             -- share the rendered content clip so off-screen buttons do not
             -- produce tooltips over the panel tabs/toggle.
             chromeWidgets =
               if editorActive editor
                 then buildEditorWidgets hoverLayout (editorTool editor)
                 else buildEditorReopenWidget hoverLayout
             viewHoverHit
               | uiShowLeftPanel uiSnap
                   && uiLeftTab uiSnap == LeftView
                   && containsPoint (leftViewContentClipRect hoverLayout) point =
                   hitTest (buildViewModeWidgets hoverLayout (uiLeftViewScroll uiSnap)) point
               | otherwise = Nothing
             chromeHit = hitTest chromeWidgets point
             hoverResult = sliderHit <|> viewHoverHit <|> chromeHit
         -- Record which widget the cursor is over and reset the
         -- deadline.  The actual tooltip is fired by
         -- 'tickTooltipHover' once wall-clock time passes the deadline.
         pending <- readIORef tooltipHoverRef
         now <- SDL.ticks
         let deadline = now + tooltipDelayMs
         case hoverResult of
           Nothing -> do
             writeIORef tooltipHoverRef Nothing
             setUiHoverWidget uiHandle Nothing
           Just wid -> do
             case pending of
               Just (prevWid, _)
                 | prevWid == wid ->
                     -- Same widget; reset the deadline so tooltip only
                     -- appears after the cursor stops moving.
                     writeIORef tooltipHoverRef (Just (wid, deadline))
                 | otherwise -> do
                     -- Different widget; restart deadline, hide tooltip.
                     writeIORef tooltipHoverRef (Just (wid, deadline))
                     setUiHoverWidget uiHandle Nothing
               Nothing -> do
                 writeIORef tooltipHoverRef (Just (wid, deadline))
                 setUiHoverWidget uiHandle Nothing
    SDL.MouseWheelEvent wheelEvent -> do
      let SDL.V2 _ dy = SDL.mouseWheelEventPos wheelEvent
      when (dy /= 0) $ do
        logSnap <- InputActions.getLogSnapshot inputEnv
        uiSnap <- InputActions.getUiSnapshot inputEnv
        (mx, my) <- readIORef mousePosRef
        (V2 winW winH) <- SDL.get (SDL.windowSize window)
        lineHeight <- readIORef (icLineHeightRef inputContext)
        let logHeight = if lsCollapsed logSnap then 24 else 160
            seedWidth = max 120 (seedMaxDigits * 10)
            eventLayout = layoutForSeed (V2 (fromIntegral winW) (fromIntegral winH)) logHeight seedWidth
            inspector = uiOverlayInspector uiSnap
            overInspector = uiMenuMode uiSnap == MenuOverlayInspector
              && containsPoint (overlayInspectorDialogRect eventLayout) (V2 mx my)
            (configUpdate, logUpdate, leftViewUpdate) =
              computeScrollUpdates defaultScrollSettings uiSnap logSnap lineHeight (V2 (fromIntegral winW) (fromIntegral winH)) (V2 mx my) (fromIntegral dy)
        if overInspector
          then scrollOverlayInspector uiSnap (fromIntegral dy)
          else case (configUpdate, logUpdate, leftViewUpdate) of
            (Just newConfigScroll, _, _) ->
              setUiConfigScroll uiHandle newConfigScroll
            (Nothing, Just newScroll, _) ->
              setLogScroll logHandle newScroll
            (Nothing, Nothing, Just newLVScroll) ->
              setUiLeftViewScroll uiHandle newLVScroll
            (Nothing, Nothing, Nothing) -> do
              let (newZoom, newOffset) = applyZoomAtCursor defaultZoomSettings uiSnap (mx, my) (fromIntegral dy)
              setUiZoom uiHandle newZoom
              setUiPanOffset uiHandle newOffset
              submitAction inputEnv (UiActionRefreshViewport (Just (fromIntegral winW, fromIntegral winH)))
    SDL.MouseButtonEvent btnEvent
      | SDL.mouseButtonEventMotion btnEvent == SDL.Pressed ->
          case SDL.mouseButtonEventButton btnEvent of
            SDL.ButtonRight -> do
              let SDL.P (V2 mx my) = SDL.mouseButtonEventPos btnEvent
              writeIORef dragRef (Just DragState
                { dsStart = (fromIntegral mx, fromIntegral my)
                , dsLast = (fromIntegral mx, fromIntegral my)
                , dsDragging = False
                })
            _ -> do
              -- SDL terrain clicks retain the editor policy: they paint only
              -- while the editor is active and never select inspection state.
              -- Automation uses viewport_click for select-and-optional-paint;
              -- select_hex remains the explicit coordinate selection command.
              uiSnap' <- getUiSnapshot uiHandle
              let editor = uiEditor uiSnap'
                  inputBarrierActive =
                    uiMenuMode uiSnap' /= MenuNone
                      || (uiShowConfig uiSnap'
                          && uiConfigTab uiSnap' == ConfigData
                          && dbsDeleteConfirm (uiDataBrowser uiSnap'))
              if inputBarrierActive
                then handleClick inputContext (SDL.mouseButtonEventPos btnEvent)
                else if editorActive editor
                then do
                  -- Check editor toolbar widgets first
                  let SDL.P (V2 bx by) = SDL.mouseButtonEventPos btnEvent
                      bPoint = V2 (fromIntegral bx) (fromIntegral by)
                  (V2 winW' winH') <- SDL.get (SDL.windowSize (icWindow inputContext))
                  logSnap' <- getLogSnapshot logHandle
                  let logH' = if lsCollapsed logSnap' then 24 else 160
                      seedW' = max 120 (seedMaxDigits * 10)
                      btnLayout = layoutForSeed (V2 (fromIntegral winW') (fromIntegral winH')) logH' seedW'
                      edWidgets = buildEditorWidgets btnLayout (editorTool editor)
                      edHit = hitTest edWidgets bPoint
                  case edHit of
                    Just wid -> handleEditorWidgetClick editor wid
                    Nothing  ->
                      case uiHoverHex uiSnap' of
                        Just hex -> submitEditorBrushStroke hex
                        Nothing  -> handleClick inputContext (SDL.mouseButtonEventPos btnEvent)
                else do
                  -- Editor is inactive — check if the reopen button was hit
                  let SDL.P (V2 rx ry) = SDL.mouseButtonEventPos btnEvent
                      rPoint = V2 (fromIntegral rx) (fromIntegral ry)
                  (V2 rwinW rwinH) <- SDL.get (SDL.windowSize (icWindow inputContext))
                  rLogSnap <- getLogSnapshot logHandle
                  let rLogH = if lsCollapsed rLogSnap then 24 else 160
                      rSeedW = max 120 (seedMaxDigits * 10)
                      rLayout = layoutForSeed (V2 (fromIntegral rwinW) (fromIntegral rwinH)) rLogH rSeedW
                      reopenWidgets = buildEditorReopenWidget rLayout
                  case hitTest reopenWidgets rPoint of
                    Just WidgetEditorReopen ->
                      dispatchEditorWidget WidgetEditorReopen
                    _ -> handleClick inputContext (SDL.mouseButtonEventPos btnEvent)
      | SDL.mouseButtonEventMotion btnEvent == SDL.Released ->
          case SDL.mouseButtonEventButton btnEvent of
            SDL.ButtonRight -> do
              state <- readIORef dragRef
              writeIORef dragRef Nothing
              case state of
                Just DragState { dsDragging = False } -> do
                  uiSnap <- getUiSnapshot uiHandle
                  setUiHexTooltipPinned uiHandle (not (uiHexTooltipPinned uiSnap))
                Just DragState { dsDragging = True } -> do
                  (V2 winW winH) <- SDL.get (SDL.windowSize window)
                  submitAction inputEnv (UiActionRefreshViewport (Just (fromIntegral winW, fromIntegral winH)))
                _ -> pure ()
            SDL.ButtonLeft -> do
              -- Reset flatten reference on stroke end
              uiSnap <- getUiSnapshot uiHandle
              let editor = uiEditor uiSnap
              when (editorActive editor && editorTool editor == ToolFlatten) $
                clearEditorStrokeSession
            _ -> pure ()
    SDL.TextInputEvent textEvent ->
      when (not (modalInputBarrierVisible barrierUi)
          || modalOwnsTextInput barrierUi) $ do
        let semanticEnv
              | modalInputBarrierVisible barrierUi =
                  intentEnv { iieGetUi = pure barrierUi }
              | otherwise = intentEnv
        _ <- executeTextIntent semanticEnv (SDL.textInputEventText textEvent)
        pure ()
    SDL.KeyboardEvent keyboardEvent
      | SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed -> do
          let keysym = SDL.keyboardEventKeysym keyboardEvent
          case inputKeyForSdl (SDL.keysymKeycode keysym) of
            Nothing -> pure ()
            Just key -> do
              let semanticEnv
                    | modalInputBarrierVisible barrierUi =
                        intentEnv { iieGetUi = pure barrierUi }
                    | otherwise = intentEnv
              if (modalInputBarrierVisible barrierUi && not (modalOwnsKey barrierUi key))
                  || sdlTextInputOwnsKey barrierUi key
                then pure ()
                else do
                  result <- executeKeyIntent semanticEnv (modifiersForSdl (SDL.keysymModifier keysym)) key
                  case result of
                    Right outcome -> do
                      when (iirStopTextInput outcome) SDL.stopTextInput
                      when (iirStartTextInput outcome) $ do
                        (V2 keyWinW keyWinH) <- SDL.get (SDL.windowSize window)
                        keyLog <- getLogSnapshot logHandle
                        let keyLogH = if lsCollapsed keyLog then 24 else 160
                            keyLayout = layoutForSeed
                              (V2 (fromIntegral keyWinW) (fromIntegral keyWinH)) keyLogH
                              (max 120 (seedMaxDigits * 10))
                            Rect (V2 rx ry, V2 rw rh) = overlayInspectorImportInputRect keyLayout
                        SDL.startTextInput (Raw.Rect (fromIntegral rx) (fromIntegral ry) (fromIntegral rw) (fromIntegral rh))
                      case iirClipboard outcome of
                        Nothing -> pure ()
                        Just clipboardText -> copyOverlayClipboard clipboardText
                      case iirDeferredWidget outcome of
                        Nothing -> pure ()
                        Just deferred -> enqueueInputAction dispatcher $ do
                          deferredResult <- runInputService inputEnv "click_widget"
                            (object ["widget_id" .= widgetIdToText deferred])
                          case deferredResult of
                            Right (ServiceResponse response)
                              | Just clipboardText <- clipboardFromWidgetResponse response ->
                                  enqueueInputMainThreadAction dispatcher
                                    (copyOverlayClipboard clipboardText)
                            _ -> pure ()
                    _ -> pure ()
    _ -> pure ()
  where
    window :: SDL.Window
    window = icWindow inputContext

    inputEnv :: InputEnv
    inputEnv = icInputEnv inputContext

    actorHandles :: ActorHandles
    actorHandles = ieActorHandles inputEnv

    uiHandle :: ActorHandle Ui (Protocol Ui)
    uiHandle = ahUiHandle actorHandles

    logHandle = ahLogHandle actorHandles

    dataHandle = ahDataHandle actorHandles

    dispatcher = icActionDispatcher inputContext

    copyOverlayClipboard clipboardText = do
      copied <- try (SDL.setClipboardText clipboardText) :: IO (Either SomeException ())
      setUiOverlayInspectorNotice uiHandle $ Just $ case copied of
        Right () -> "Copied exact export JSON to the clipboard."
        Left err -> "Clipboard copy failed: " <> Text.pack (show err)

    intentEnv = InputIntentEnv
      { iieActorHandles = actorHandles
      , iieGetUi = getUiSnapshot uiHandle
      , iieGetLog = getLogSnapshot logHandle
      , iieRunService = runInputService inputEnv
      , iieApplyDataBrowser = applyDataBrowserIntent
      , iieDeferBlockingWidgets = True
      , iieOverlayInspectorScrollLimit = currentInspectorScrollLimit
      }

    scrollOverlayInspector ui dy = do
      (V2 winW winH) <- SDL.get (SDL.windowSize window)
      currentLog <- getLogSnapshot logHandle
      let logHeight = if lsCollapsed currentLog then 24 else 160
          layout = layoutForSeed (V2 (fromIntegral winW) (fromIntegral winH))
            logHeight (max 120 (seedMaxDigits * 10))
          inspector = uiOverlayInspector ui
          limit = overlayInspectorViewScrollLimit layout inspector
          next = max 0 (min limit (oimScroll inspector - dy * 3))
      setUiOverlayInspectorScroll uiHandle next

    currentInspectorScrollLimit inspector = do
      (V2 winW winH) <- SDL.get (SDL.windowSize window)
      currentLog <- getLogSnapshot logHandle
      let logHeight = if lsCollapsed currentLog then 24 else 160
          layout = layoutForSeed (V2 (fromIntegral winW) (fromIntegral winH))
            logHeight (max 120 (seedMaxDigits * 10))
      pure (overlayInspectorViewScrollLimit layout inspector)

    applyDataBrowserIntent action = do
      result <- submitDataBrowserAction
        (ieDataBrowserExecutor inputEnv)
        (runInputService inputEnv)
        action
      pure $ case result of
        DataBrowserBeginRejected err -> Left err
        _ -> Right ()

    submitEditorBrushStroke hex =
      -- Local drag strokes stay on the shared UiActions stroke stream so
      -- ToolFlatten keeps one reference height for the whole drag gesture;
      -- discrete service strokes enter through editor_brush_stroke.
      submitAction inputEnv (UiActionBrushStroke hex)

    clearEditorStrokeSession =
      submitAction inputEnv UiActionClearFlattenRef

    handleEditorWidgetClick :: EditorState -> WidgetId -> IO ()
    handleEditorWidgetClick _editor = dispatchEditorWidget

    dispatchEditorWidget wid = enqueueInputAction
      (icActionDispatcher inputContext) $ do
        _ <- runInputService inputEnv "click_widget"
          (object ["widget_id" .= widgetIdToText wid])
        pure ()

modalOwnsTextInput :: UiState -> Bool
modalOwnsTextInput ui = case uiMenuMode ui of
  MenuPresetSave -> True
  MenuPresetLoad -> True
  MenuWorldSave -> True
  MenuWorldLoad -> not (uiWorldDeleteConfirm ui)
  MenuOverlayInspector ->
    let inspector = uiOverlayInspector ui
    in oimView inspector == Just OverlayInspectorImportView
      && oimFocus inspector == OverlayInspectorImportInputFocus
  _ -> False

modalOwnsKey :: UiState -> InputKey -> Bool
modalOwnsKey ui key = case uiMenuMode ui of
  MenuEscape -> key == KeyEscape
  MenuNone -> key == KeyEscape
  _ -> True

sameModalInputScope :: UiState -> UiState -> Bool
sameModalInputScope cached live
  | not (modalInputBarrierVisible cached && modalInputBarrierVisible live) = False
  | uiMenuMode cached /= uiMenuMode live = False
  | otherwise = case uiMenuMode live of
      MenuWorldLoad ->
        uiWorldDeleteConfirm cached == uiWorldDeleteConfirm live
      MenuOverlayInspector ->
        oimView (uiOverlayInspector cached) == oimView (uiOverlayInspector live)
      MenuNone ->
        dbsDeleteConfirm (uiDataBrowser cached)
          && dbsDeleteConfirm (uiDataBrowser live)
      _ -> True

-- SDL normalization is intentionally thin; all routing and semantics live in
-- Seer.Input.Intent and are therefore shared with automation.
inputKeyForSdl :: SDL.Keycode -> Maybe InputKey
inputKeyForSdl key = case key of
  SDL.KeycodeEscape -> Just KeyEscape
  SDL.KeycodeReturn -> Just KeyEnter
  SDL.KeycodeBackspace -> Just KeyBackspace
  SDL.KeycodeDelete -> Just KeyDelete
  SDL.KeycodeTab -> Just KeyTab
  SDL.KeycodeUp -> Just KeyUp
  SDL.KeycodeDown -> Just KeyDown
  SDL.KeycodeLeft -> Just KeyLeft
  SDL.KeycodeRight -> Just KeyRight
  SDL.KeycodeHome -> Just KeyHome
  SDL.KeycodeEnd -> Just KeyEnd
  SDL.KeycodeSpace -> Just KeySpace
  SDL.KeycodeLeftBracket -> Just KeyLeftBracket
  SDL.KeycodeRightBracket -> Just KeyRightBracket
  SDL.Keycode0 -> Just (KeyCharacter '0')
  SDL.Keycode1 -> Just (KeyCharacter '1')
  SDL.Keycode2 -> Just (KeyCharacter '2')
  SDL.Keycode3 -> Just (KeyCharacter '3')
  SDL.Keycode4 -> Just (KeyCharacter '4')
  SDL.Keycode5 -> Just (KeyCharacter '5')
  SDL.Keycode6 -> Just (KeyCharacter '6')
  SDL.Keycode7 -> Just (KeyCharacter '7')
  SDL.Keycode8 -> Just (KeyCharacter '8')
  SDL.Keycode9 -> Just (KeyCharacter '9')
  SDL.KeycodeA -> Just (KeyCharacter 'a')
  SDL.KeycodeB -> Just (KeyCharacter 'b')
  SDL.KeycodeC -> Just (KeyCharacter 'c')
  SDL.KeycodeD -> Just (KeyCharacter 'd')
  SDL.KeycodeE -> Just (KeyCharacter 'e')
  SDL.KeycodeF -> Just (KeyCharacter 'f')
  SDL.KeycodeG -> Just (KeyCharacter 'g')
  SDL.KeycodeH -> Just (KeyCharacter 'h')
  SDL.KeycodeI -> Just (KeyCharacter 'i')
  SDL.KeycodeJ -> Just (KeyCharacter 'j')
  SDL.KeycodeK -> Just (KeyCharacter 'k')
  SDL.KeycodeL -> Just (KeyCharacter 'l')
  SDL.KeycodeM -> Just (KeyCharacter 'm')
  SDL.KeycodeN -> Just (KeyCharacter 'n')
  SDL.KeycodeO -> Just (KeyCharacter 'o')
  SDL.KeycodeP -> Just (KeyCharacter 'p')
  SDL.KeycodeQ -> Just (KeyCharacter 'q')
  SDL.KeycodeR -> Just (KeyCharacter 'r')
  SDL.KeycodeS -> Just (KeyCharacter 's')
  SDL.KeycodeT -> Just (KeyCharacter 't')
  SDL.KeycodeU -> Just (KeyCharacter 'u')
  SDL.KeycodeV -> Just (KeyCharacter 'v')
  SDL.KeycodeW -> Just (KeyCharacter 'w')
  SDL.KeycodeX -> Just (KeyCharacter 'x')
  SDL.KeycodeY -> Just (KeyCharacter 'y')
  SDL.KeycodeZ -> Just (KeyCharacter 'z')
  _ -> Nothing

sdlTextInputOwnsKey :: UiState -> InputKey -> Bool
sdlTextInputOwnsKey ui key = isTextKey key && textScope
  where
    isTextKey (KeyCharacter _) = True
    isTextKey KeySpace = True
    isTextKey _ = False
    dbs = uiDataBrowser ui
    dataFieldFocused = case dbsFocusedField dbs of
      Just _ -> dbsEditMode dbs || dbsCreateMode dbs
      Nothing -> False
    inspector = uiOverlayInspector ui
    overlayImportFocused = uiMenuMode ui == MenuOverlayInspector
      && oimView inspector == Just OverlayInspectorImportView
      && oimFocus inspector == OverlayInspectorImportInputFocus
    textScope = uiSeedEditing ui
      || dataFieldFocused
      || overlayImportFocused
      || uiMenuMode ui `elem`
          [MenuPresetSave, MenuPresetLoad, MenuWorldSave, MenuWorldLoad]

modifiersForSdl :: SDL.KeyModifier -> KeyModifiers
modifiersForSdl mods = KeyModifiers
  { kmCtrl = SDL.keyModifierLeftCtrl mods || SDL.keyModifierRightCtrl mods
  , kmShift = SDL.keyModifierLeftShift mods || SDL.keyModifierRightShift mods
  , kmAlt = SDL.keyModifierLeftAlt mods || SDL.keyModifierRightAlt mods
  , kmMeta = SDL.keyModifierLeftGUI mods || SDL.keyModifierRightGUI mods
  }

-- | Per-frame tick for the tooltip hover delay.  Compares the stored
-- wall-clock deadline against the current SDL tick time and promotes
-- the pending hover to a visible tooltip when it expires.  Returns
-- 'True' (once) when it fires so the caller can request a UI snapshot
-- refresh.
tickTooltipHover
  :: IORef TooltipHover
  -> ActorHandle Ui (Protocol Ui)
  -> IO Bool
tickTooltipHover tooltipHoverRef uiHandle = do
  pending <- readIORef tooltipHoverRef
  case pending of
    Just (wid, deadline)
      -- Already fired (sentinel 0); tooltip is visible, no action needed.
      | deadline == 0 -> pure False
      | otherwise -> do
          nowMs <- SDL.ticks
          if nowMs >= deadline
            then do
              -- Deadline reached; lock at 0 and show tooltip.
              writeIORef tooltipHoverRef (Just (wid, 0))
              setUiHoverWidget uiHandle (Just wid)
              pure True
            else pure False
    Nothing -> pure False

