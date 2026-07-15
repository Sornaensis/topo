{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Seer.Input.Events
  ( handleEvent
  , TooltipHover
  , tickTooltipHover
  , tooltipDelayMs
  ) where

import Actor.Data (TerrainSnapshot(..), getTerrainSnapshot)
import Actor.Log (LogSnapshot(..), getLogSnapshot, setLogCollapsed, setLogScroll)
import Actor.UI
  ( ConfigTab(..)
  , DataBrowserState(..)
  , LayeredViewState(..)
  , LeftTab(..)
  , Ui
  , UiMenuMode(..)
  , SkyOverlayMode(..)
  , UiState(..)
  , WeatherBasis(..)
  , effectiveViewSelection
  , getUiSnapshot
  , setUiConfigScroll
  , setUiContextHex
  , setUiContextPos
  , setUiHexTooltipPinned
  , setUiPanOffset
  , setUiHoverHex
  , setUiHoverWidget
  , setUiShowConfig
  , setUiMenuMode
  , setUiPresetInput
  , setUiPresetSelected
  , setUiPresetFilter
  , setUiWorldSaveInput
  , setUiWorldSelected
  , setUiWorldFilter
  , setUiZoom
  , setUiLeftViewScroll
  )
import Control.Applicative ((<|>))
import Control.Monad (void, when)
import Data.Aeson (Value(..), object, (.=))
import Data.IORef (IORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Word (Word32, Word64)
import qualified Data.Text as Text
import qualified Data.IntMap.Strict as IntMap
import Linear (V2(..))
import qualified SDL
import Hyperspace.Actor (ActorHandle, Protocol)
import Seer.Draw (seedMaxDigits)
import Seer.Input.ConfigScroll
  ( computeScrollUpdates
  , defaultScrollSettings
  )
import Seer.Input.Context (DragState(..), InputContext(..), TooltipHover)
import Seer.Input.Modal (handleModalListKey, handleModalTextKey, handleModalTextInput)
import Seer.Input.Seed (bumpSeed, handleSeedKey, handleSeedTextInput)
import Seer.Input.ViewControls
  ( ViewHotkey(..)
  , applyZoomAtCursor
  , defaultZoomSettings
  , viewHotkeyForKey
  )
import Seer.DataBrowser.Executor (submitDataBrowserAction)
import qualified Seer.DataBrowser.Lifecycle as DataBrowser
import Topo (ChunkCoord(..), ChunkId(..), TileCoord(..), WorldConfig(..), chunkCoordFromTile, chunkIdFromCoord)
import Topo.Types (BiomeId, TerrainForm, biomeIdToCode, terrainFormToCode)
import UI.Layout
import UI.WidgetTree (Widget(..), WidgetId(..), buildEditorWidgets, buildEditorReopenWidget, buildViewModeWidgets, buildSliderRowWidgets, hitTest)
import UI.Widgets (Rect(..), containsPoint)
import Seer.Input.Actions (InputEnv(..), runInputService, submitAction)
import qualified Seer.Input.Actions as InputActions
import Seer.Editor.Types (EditorState(..), EditorTool(..), BrushSettings(..), Falloff(..), paintableBiomes, allTerrainForms)
import Actor.UiActions (UiAction(..))
import Actor.UiActions.Handles (ActorHandles(..))
import Seer.Input.Widgets (handleClick)
import Seer.World.Persist.Types (WorldSaveManifest(..))
import UI.HexGeometry (renderHexRadiusPx, screenPixelToAxial)

-- | Wall-clock delay (milliseconds) the cursor must remain still on a
-- widget before the tooltip appears.
tooltipDelayMs :: Word32
tooltipDelayMs = 500

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
  case SDL.eventPayload event of
    SDL.MouseMotionEvent motionEvent -> do
      let SDL.P (V2 mx my) = SDL.mouseMotionEventPos motionEvent
      writeIORef mousePosRef (fromIntegral mx, fromIntegral my)
      dragState <- readIORef dragRef
      case dragState of
        Just state -> do
          uiSnap <- InputActions.getUiSnapshot inputEnv
          let DragState { dsStart = (sx, sy), dsLast = (px, py), dsDragging = dragging } = state
              dx0 = fromIntegral mx - fromIntegral sx
              dy0 = fromIntegral my - fromIntegral sy
              dist2 = dx0 * dx0 + dy0 * dy0
              startDrag = dist2 > dragThreshold * dragThreshold
              (ox, oy) = uiPanOffset uiSnap
              dx = fromIntegral mx - fromIntegral px
              dy = fromIntegral my - fromIntegral py
              zoom = uiZoom uiSnap
              newOffset = (ox + dx / zoom, oy + dy / zoom)
          if dragging || startDrag
            then do
              setUiPanOffset uiHandle newOffset
              writeIORef dragRef (Just state { dsLast = (fromIntegral mx, fromIntegral my), dsDragging = True })
            else writeIORef dragRef (Just state { dsLast = (fromIntegral mx, fromIntegral my) })
        Nothing -> pure ()
      uiSnap <- getUiSnapshot uiHandle
      terrainSnap <- getTerrainSnapshot dataHandle
      let (q, r) = screenPixelToAxial renderHexRadiusPx (uiPanOffset uiSnap) (uiZoom uiSnap) (fromIntegral mx, fromIntegral my)
      if isTerrainHex terrainSnap (q, r)
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
        let (configUpdate, logUpdate, leftViewUpdate) =
              computeScrollUpdates defaultScrollSettings uiSnap logSnap lineHeight (V2 (fromIntegral winW) (fromIntegral winH)) (V2 mx my) (fromIntegral dy)
        case (configUpdate, logUpdate, leftViewUpdate) of
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
            submitAction inputEnv (UiActionRefreshViewport (uiViewMode uiSnap) (Just (fromIntegral winW, fromIntegral winH)))
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
              uiSnap' <- getUiSnapshot uiHandle
              let editor = uiEditor uiSnap'
              if editorActive editor
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
                      setEditorActive True
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
                  uiSnap <- getUiSnapshot uiHandle
                  (V2 winW winH) <- SDL.get (SDL.windowSize window)
                  submitAction inputEnv (UiActionRefreshViewport (uiViewMode uiSnap) (Just (fromIntegral winW, fromIntegral winH)))
                _ -> pure ()
            SDL.ButtonLeft -> do
              -- Reset flatten reference on stroke end
              uiSnap <- getUiSnapshot uiHandle
              let editor = uiEditor uiSnap
              when (editorActive editor && editorTool editor == ToolFlatten) $
                clearEditorStrokeSession
            _ -> pure ()
    SDL.TextInputEvent textEvent -> do
      uiSnap <- InputActions.getUiSnapshot inputEnv
      let txt = SDL.textInputEventText textEvent
      when (uiSeedEditing uiSnap) $
        handleSeedTextInput uiHandle (getUiSnapshot uiHandle) txt
      when (uiMenuMode uiSnap == MenuPresetSave) $
        handleModalTextInput (uiPresetInput uiSnap) txt
          (setUiPresetInput uiHandle)
      when (uiMenuMode uiSnap == MenuWorldSave) $
        handleModalTextInput (uiWorldSaveInput uiSnap) txt
          (setUiWorldSaveInput uiHandle)
      when (uiMenuMode uiSnap == MenuPresetLoad) $
        handleModalTextInput (uiPresetFilter uiSnap) txt
          (\f -> setUiPresetFilter uiHandle f >> setUiPresetSelected uiHandle 0)
      when (uiMenuMode uiSnap == MenuWorldLoad) $
        handleModalTextInput (uiWorldFilter uiSnap) txt
          (\f -> setUiWorldFilter uiHandle f >> setUiWorldSelected uiHandle 0)
      -- Data browser text editing is reduced against the latest Ui-owned state.
      let dbs = uiDataBrowser uiSnap
      case dbsFocusedField dbs of
        Just _ | dbsEditMode dbs || dbsCreateMode dbs -> do
          let filtered = Text.filter (\c -> c >= ' ') txt
          when (not (Text.null filtered)) $
            applyDataBrowserAction (DataBrowser.DataBrowserInsertText filtered)
        _ -> pure ()
    SDL.KeyboardEvent keyboardEvent
      | SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed ->
          do
            uiSnap <- InputActions.getUiSnapshot inputEnv
            let keycode = SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent)
            if uiSeedEditing uiSnap
              then handleSeedKey uiHandle (getUiSnapshot uiHandle) setSeedValue keycode
              else let dbs = uiDataBrowser uiSnap
                   in case dbsFocusedField dbs of
                        Just path | dbsEditMode dbs || dbsCreateMode dbs ->
                          handleDataFieldKey uiSnap dbs path keycode
                        _ -> case uiMenuMode uiSnap of
                              MenuPresetSave -> handlePresetSaveKey uiSnap keycode
                              MenuPresetLoad -> handlePresetLoadKey uiSnap keycode
                              MenuWorldSave  -> handleWorldSaveKey uiSnap keycode
                              MenuWorldLoad  -> handleWorldLoadKey uiSnap keycode
                              _ -> do
                                uiSnap2 <- getUiSnapshot uiHandle
                                let editor = uiEditor uiSnap2
                                    mods = SDL.keysymModifier (SDL.keyboardEventKeysym keyboardEvent)
                                    ctrl = SDL.keyModifierLeftCtrl mods || SDL.keyModifierRightCtrl mods
                                if editorActive editor
                                  then if ctrl
                                    then case keycode of
                                      SDL.KeycodeZ -> runEditorService "editor_undo" Null
                                      SDL.KeycodeY -> runEditorService "editor_redo" Null
                                      _ -> handleEditorKey editor keycode
                                    else handleEditorKey editor keycode
                                  else case keycode of
                                    SDL.KeycodeEscape -> closeContextOrMenu
                                    SDL.KeycodeG -> runInputService inputEnv "generate" Null >> pure ()
                                    SDL.KeycodeC -> toggleConfig
                                    SDL.KeycodeE -> toggleEditor
                                    SDL.KeycodeUp -> bumpSeed (getUiSnapshot uiHandle) setSeedValue 1
                                    SDL.KeycodeDown -> bumpSeed (getUiSnapshot uiHandle) setSeedValue (-1)
                                    SDL.KeycodeL -> do
                                      logSnap <- getLogSnapshot logHandle
                                      setLogCollapsed logHandle (not (lsCollapsed logSnap))
                                    _ ->
                                      case viewHotkeyForKey keycode of
                                        Just hotkey -> handleViewHotkey uiSnap2 hotkey
                                        Nothing -> pure ()
    _ -> pure ()
  where
    inputEnv :: InputEnv
    inputEnv = icInputEnv inputContext

    actorHandles :: ActorHandles
    actorHandles = ieActorHandles inputEnv

    uiHandle :: ActorHandle Ui (Protocol Ui)
    uiHandle = ahUiHandle actorHandles

    logHandle = ahLogHandle actorHandles

    dataHandle = ahDataHandle actorHandles

    runService method params =
      runInputService inputEnv method params >> pure ()

    applyDataBrowserAction action = void $ submitDataBrowserAction
      (ieDataBrowserExecutor inputEnv)
      (runInputService inputEnv)
      action

    setSeedValue :: Word64 -> IO ()
    setSeedValue seed =
      runService "set_seed" (object ["seed" .= seed])

    runEditorService method params =
      runService method params

    setEditorActive active =
      runEditorService "editor_toggle" (object ["active" .= active])

    setEditorTool tool =
      runEditorService "editor_set_tool" (object ["tool" .= editorToolName tool])

    setEditorBrushRadius radius =
      runEditorService "editor_set_brush" (object ["radius" .= radius])

    setEditorBrushStrength strength =
      runEditorService "editor_set_brush" (object ["strength" .= strength])

    setEditorBrushFalloff falloff =
      runEditorService "editor_set_brush" (object ["falloff" .= falloffName falloff])

    setEditorSmoothPasses passes =
      runEditorService "editor_set_brush" (object ["smooth_passes" .= passes])

    setEditorNoiseFrequency frequency =
      runEditorService "editor_set_brush" (object ["noise_frequency" .= frequency])

    setEditorErodePasses passes =
      runEditorService "editor_set_brush" (object ["erode_passes" .= passes])

    setEditorBiome biome =
      runEditorService "editor_set_biome" (object ["biome" .= biomeIdToCode biome])

    setEditorForm form =
      runEditorService "editor_set_form" (object ["form" .= terrainFormToCode form])

    setEditorHardness hardness =
      runEditorService "editor_set_hardness" (object ["hardness" .= hardness])

    submitEditorBrushStroke hex =
      -- Local drag strokes stay on the shared UiActions stroke stream so
      -- ToolFlatten keeps one reference height for the whole drag gesture;
      -- discrete API/IPC strokes enter through editor_brush_stroke.
      submitAction inputEnv (UiActionBrushStroke hex)

    clearEditorStrokeSession =
      submitAction inputEnv UiActionClearFlattenRef

    editorToolName :: EditorTool -> Text.Text
    editorToolName tool = case tool of
      ToolRaise -> "raise"
      ToolLower -> "lower"
      ToolSmooth -> "smooth"
      ToolFlatten -> "flatten"
      ToolNoise -> "noise"
      ToolPaintBiome -> "paint_biome"
      ToolPaintForm -> "paint_form"
      ToolSetHardness -> "set_hardness"
      ToolErode -> "erode"

    falloffName :: Falloff -> Text.Text
    falloffName falloff = case falloff of
      FalloffLinear -> "linear"
      FalloffSmooth -> "smooth"
      FalloffConstant -> "constant"

    dragThreshold :: Float
    dragThreshold = 4

    isTerrainHex terrainSnap (q, r) =
      let size = tsChunkSize terrainSnap
      in size > 0
        &&
          let cfg = WorldConfig { wcChunkSize = size }
              (chunkCoord, _) = chunkCoordFromTile cfg (TileCoord q r)
              ChunkId key = chunkIdFromCoord chunkCoord
          in IntMap.member key (tsTerrainChunks terrainSnap)

    toggleConfig = do
      uiSnap <- getUiSnapshot uiHandle
      setUiShowConfig uiHandle (not (uiShowConfig uiSnap))
    toggleEditor = do
      uiSnap <- getUiSnapshot uiHandle
      let editor = uiEditor uiSnap
      setEditorActive (not (editorActive editor))

    handleViewHotkey :: UiState -> ViewHotkey -> IO ()
    handleViewHotkey uiSnap hotkey =
      case hotkey of
        ViewHotkeySetBase baseMode ->
          submitAction inputEnv (UiActionSetBaseViewMode baseMode)
        ViewHotkeySetOverlay overlayMode ->
          submitAction inputEnv (UiActionSetSkyOverlayMode overlayMode)
        ViewHotkeyCycleOverlay ->
          submitAction inputEnv (UiActionSetSkyOverlayMode (nextBuiltinOverlay selection))
        ViewHotkeyCycleWeatherBasis ->
          when (weatherBasisEnabled selection) $
            submitAction inputEnv (UiActionSetWeatherBasis (nextWeatherBasis (lvsWeatherBasis selection)))
      where
        selection = effectiveViewSelection uiSnap

    nextBuiltinOverlay :: LayeredViewState -> Maybe SkyOverlayMode
    nextBuiltinOverlay selection = case lvsSkyOverlay selection of
      Nothing -> Just SkyOverlayWeatherTemperature
      Just SkyOverlayWeatherTemperature -> Just SkyOverlayPrecipitation
      Just SkyOverlayPrecipitation -> Just SkyOverlayCloud
      Just SkyOverlayCloud -> Nothing
      Just (SkyOverlayPlugin _ _) -> Just SkyOverlayWeatherTemperature

    weatherBasisEnabled :: LayeredViewState -> Bool
    weatherBasisEnabled selection = case lvsSkyOverlay selection of
      Just (SkyOverlayPlugin _ _) -> False
      Just _ -> True
      Nothing -> False

    nextWeatherBasis :: WeatherBasis -> WeatherBasis
    nextWeatherBasis WeatherBasisAverage = WeatherBasisCurrent
    nextWeatherBasis WeatherBasisCurrent = WeatherBasisAverage

    handleEditorKey :: EditorState -> SDL.Keycode -> IO ()
    handleEditorKey editor keycode = case keycode of
      SDL.KeycodeEscape ->
        setEditorActive False
      SDL.KeycodeE ->
        setEditorActive False
      SDL.Keycode1 ->
        setEditorTool ToolRaise
      SDL.Keycode2 ->
        setEditorTool ToolLower
      SDL.Keycode3 ->
        setEditorTool ToolSmooth
      SDL.Keycode4 ->
        setEditorTool ToolFlatten
      SDL.Keycode5 ->
        setEditorTool ToolNoise
      SDL.Keycode6 ->
        setEditorTool ToolPaintBiome
      SDL.Keycode7 ->
        setEditorTool ToolPaintForm
      SDL.Keycode8 ->
        setEditorTool ToolSetHardness
      SDL.Keycode9 ->
        setEditorTool ToolErode
      SDL.KeycodeLeftBracket ->
        let brush = editorBrush editor
            r = max 0 (brushRadius brush - 1)
        in setEditorBrushRadius r
      SDL.KeycodeRightBracket ->
        let brush = editorBrush editor
            r = min 6 (brushRadius brush + 1)
        in setEditorBrushRadius r
      _ -> pure ()
    handleEditorWidgetClick :: EditorState -> WidgetId -> IO ()
    handleEditorWidgetClick editor wid = case wid of
      WidgetEditorTool idx ->
        let tools = [minBound .. maxBound] :: [EditorTool]
        in case drop idx tools of
          (tool:_) -> setEditorTool tool
          []       -> pure ()
      WidgetEditorRadiusMinus ->
        let brush = editorBrush editor
            r = max 0 (brushRadius brush - 1)
        in setEditorBrushRadius r
      WidgetEditorRadiusPlus ->
        let brush = editorBrush editor
            r = min 6 (brushRadius brush + 1)
        in setEditorBrushRadius r
      WidgetEditorClose ->
        setEditorActive False
      -- Param bar: numeric minus/plus
      WidgetEditorParamMinus slot -> applyNumericDelta editor slot (-1)
      WidgetEditorParamPlus  slot -> applyNumericDelta editor slot 1
      -- Param bar: cycle selectors
      WidgetEditorCyclePrev _slot -> applyCycleStep editor (-1)
      WidgetEditorCycleNext _slot -> applyCycleStep editor 1
      -- Falloff cycle
      WidgetEditorFalloffPrev -> applyFalloffStep editor (-1)
      WidgetEditorFalloffNext -> applyFalloffStep editor 1
      _ -> pure ()

    applyNumericDelta :: EditorState -> Int -> Int -> IO ()
    applyNumericDelta editor slot dir =
      let brush = editorBrush editor
          sign  = fromIntegral dir :: Float
      in case editorTool editor of
        ToolRaise -> setEditorBrushStrength (clamp 0.005 0.2 (brushStrength brush + sign * 0.005))
        ToolLower -> setEditorBrushStrength (clamp 0.005 0.2 (brushStrength brush + sign * 0.005))
        ToolSmooth -> setEditorSmoothPasses (clampI 1 5 (editorSmoothPasses editor + dir))
        ToolFlatten -> setEditorBrushStrength (clamp 0.01 0.5 (brushStrength brush + sign * 0.01))
        ToolNoise
          | slot == 0 -> setEditorNoiseFrequency (clamp 0.5 4.0 (editorNoiseFrequency editor + sign * 0.1))
          | otherwise -> setEditorBrushStrength (clamp 0.005 0.2 (brushStrength brush + sign * 0.005))
        ToolSetHardness -> setEditorHardness (clamp 0.0 1.0 (editorHardnessTarget editor + sign * 0.05))
        ToolErode -> setEditorErodePasses (clampI 1 20 (editorErodePasses editor + dir))
        _ -> pure ()

    applyCycleStep :: EditorState -> Int -> IO ()
    applyCycleStep editor dir = case editorTool editor of
      ToolPaintBiome ->
        let next = cycleBiome (editorBiomeId editor) dir
        in setEditorBiome next
      ToolPaintForm ->
        let next = cycleForm (editorFormOverride editor) dir
        in setEditorForm next
      _ -> pure ()

    applyFalloffStep :: EditorState -> Int -> IO ()
    applyFalloffStep editor dir =
      let brush  = editorBrush editor
          next   = cycleFalloff (brushFalloff brush) dir
      in setEditorBrushFalloff next

    clamp :: Float -> Float -> Float -> Float
    clamp lo hi v = max lo (min hi v)

    clampI :: Int -> Int -> Int -> Int
    clampI lo hi v = max lo (min hi v)

    cycleBiome :: BiomeId -> Int -> BiomeId
    cycleBiome cur dir =
      let bs  = paintableBiomes
          idx = maybe 0 id (lookup cur (zip bs [0..]))
          n   = length bs
      in bs !! ((idx + dir + n) `mod` n)

    cycleForm :: TerrainForm -> Int -> TerrainForm
    cycleForm cur dir =
      let fs  = allTerrainForms
          idx = maybe 0 id (lookup cur (zip fs [0..]))
          n   = length fs
      in fs !! ((idx + dir + n) `mod` n)

    cycleFalloff :: Falloff -> Int -> Falloff
    cycleFalloff cur dir =
      let fs  = [FalloffLinear, FalloffSmooth, FalloffConstant]
          idx = maybe 0 id (lookup cur (zip fs [0..]))
          n   = length fs
      in fs !! ((idx + dir + n) `mod` n)

    -- | Handle keyboard events when a data browser text field is focused.
    handleDataFieldKey :: UiState -> DataBrowserState -> Text.Text -> SDL.Keycode -> IO ()
    handleDataFieldKey _uiSnap dbs path keycode = do
      let cursor = dbsTextCursor dbs
          currentText = case Map.lookup path (dbsEditValues dbs) of
            Just (String t) -> t
            _ -> ""
          unfocus = applyDataBrowserAction DataBrowser.DataBrowserBlurField >> SDL.stopTextInput
      case keycode of
        SDL.KeycodeEscape -> unfocus
        SDL.KeycodeReturn -> unfocus
        SDL.KeycodeTab -> unfocus
        SDL.KeycodeBackspace -> applyDataBrowserAction DataBrowser.DataBrowserBackspace
        SDL.KeycodeDelete -> applyDataBrowserAction DataBrowser.DataBrowserDeleteText
        SDL.KeycodeLeft -> applyDataBrowserAction (DataBrowser.DataBrowserSetTextCursor (cursor - 1))
        SDL.KeycodeRight -> applyDataBrowserAction (DataBrowser.DataBrowserSetTextCursor (cursor + 1))
        SDL.KeycodeHome -> applyDataBrowserAction (DataBrowser.DataBrowserSetTextCursor 0)
        SDL.KeycodeEnd -> applyDataBrowserAction (DataBrowser.DataBrowserSetTextCursor (Text.length currentText))
        _ -> pure ()

    closeContextOrMenu = do
      uiSnap <- getUiSnapshot uiHandle
      let dbs = uiDataBrowser uiSnap
      if dbsDeleteConfirm dbs
        then applyDataBrowserAction DataBrowser.DataBrowserCancelDelete
        else if dbsEditMode dbs || dbsCreateMode dbs
          then do
            applyDataBrowserAction DataBrowser.DataBrowserCancelEdit
            SDL.stopTextInput
          else case dbsSelectedRecord dbs of
            Just _ -> applyDataBrowserAction DataBrowser.DataBrowserDismissRecord
            Nothing ->
              case uiContextHex uiSnap of
                Just _ -> do
                  setUiContextHex uiHandle Nothing
                  setUiContextPos uiHandle Nothing
                  setUiMenuMode uiHandle MenuEscape
                Nothing ->
                  case uiMenuMode uiSnap of
                    MenuNone -> setUiMenuMode uiHandle MenuEscape
                    _        -> setUiMenuMode uiHandle MenuNone

    handlePresetSaveKey :: UiState -> SDL.Keycode -> IO ()
    handlePresetSaveKey _uiSnap keycode =
      handleModalTextKey keycode
        -- onConfirm
        (do uiSnap' <- getUiSnapshot uiHandle
            let name = uiPresetInput uiSnap'
            _ <- runInputService inputEnv "save_preset" (object ["name" .= name])
            setUiMenuMode uiHandle MenuNone
            SDL.stopTextInput)
        -- onCancel
        (setUiMenuMode uiHandle MenuNone >> SDL.stopTextInput)
        -- onBackspace
        (do uiSnap' <- getUiSnapshot uiHandle
            setUiPresetInput uiHandle (Text.dropEnd 1 (uiPresetInput uiSnap')))

    handlePresetLoadKey :: UiState -> SDL.Keycode -> IO ()
    handlePresetLoadKey uiSnap keycode = do
      let fText = Text.toLower (uiPresetFilter uiSnap)
          filteredItems = filter (\n -> Text.isInfixOf fText (Text.toLower n)) (uiPresetList uiSnap)
      case keycode of
        SDL.KeycodeBackspace ->
          setUiPresetFilter uiHandle (Text.dropEnd 1 (uiPresetFilter uiSnap))
            >> setUiPresetSelected uiHandle 0
        _ ->
          handleModalListKey keycode
            (uiPresetSelected uiSnap)
            (length filteredItems - 1)
            -- onConfirm
            (do let sel = uiPresetSelected uiSnap
                when (sel >= 0 && sel < length filteredItems) $ do
                  let name = filteredItems !! sel
                  _ <- runInputService inputEnv "load_preset" (object ["name" .= name])
                  pure ()
                setUiMenuMode uiHandle MenuNone)
            -- onCancel
            (setUiMenuMode uiHandle MenuNone >> SDL.stopTextInput)
            -- setSelection
            (setUiPresetSelected uiHandle)

    handleWorldSaveKey :: UiState -> SDL.Keycode -> IO ()
    handleWorldSaveKey _uiSnap keycode =
      handleModalTextKey keycode
        -- onConfirm
        (do uiSnap' <- getUiSnapshot uiHandle
            let name = uiWorldSaveInput uiSnap'
            when (not (Text.null name)) $ do
              _ <- runInputService inputEnv "save_world" (object ["name" .= name])
              pure ()
            setUiMenuMode uiHandle MenuNone
            SDL.stopTextInput)
        -- onCancel
        (setUiMenuMode uiHandle MenuNone >> SDL.stopTextInput)
        -- onBackspace
        (do uiSnap' <- getUiSnapshot uiHandle
            setUiWorldSaveInput uiHandle (Text.dropEnd 1 (uiWorldSaveInput uiSnap')))

    handleWorldLoadKey :: UiState -> SDL.Keycode -> IO ()
    handleWorldLoadKey uiSnap keycode = do
      let fText = Text.toLower (uiWorldFilter uiSnap)
          filteredItems = filter (\m -> Text.isInfixOf fText (Text.toLower (wsmName m))) (uiWorldList uiSnap)
      case keycode of
        SDL.KeycodeBackspace ->
          setUiWorldFilter uiHandle (Text.dropEnd 1 (uiWorldFilter uiSnap))
            >> setUiWorldSelected uiHandle 0
        _ ->
          handleModalListKey keycode
            (uiWorldSelected uiSnap)
            (length filteredItems - 1)
            -- onConfirm
            (do let sel = uiWorldSelected uiSnap
                when (sel >= 0 && sel < length filteredItems) $ do
                  let manifest = filteredItems !! sel
                      name = wsmName manifest
                  _ <- runInputService inputEnv "load_world" (object ["name" .= name])
                  pure ()
                setUiMenuMode uiHandle MenuNone)
            -- onCancel
            (setUiMenuMode uiHandle MenuNone >> SDL.stopTextInput)
            -- setSelection
            (setUiWorldSelected uiHandle)

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

