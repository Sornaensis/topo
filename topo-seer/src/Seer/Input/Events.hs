{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Seer.Input.Events
  ( handleEvent
  , TooltipHover
  , tickTooltipHover
  , tooltipDelayFrames
  ) where

import Actor.Data (DataSnapshot(..), TerrainSnapshot(..), getDataSnapshot, getTerrainSnapshot, replaceTerrainData)
import Actor.SnapshotReceiver (writeDataSnapshot, writeTerrainSnapshot, bumpSnapshotVersion)
import Actor.Log (LogEntry(..), LogLevel(..), LogSnapshot(..), appendLog, getLogSnapshot, setLogCollapsed, setLogMinLevel, setLogScroll)
import Actor.UI
  ( ConfigTab(..)
  , Ui
  , UiMenuMode(..)
  , UiState(..)
  , getUiSnapshot
  , setUiConfigScroll
  , setUiContextHex
  , setUiContextPos
  , setUiEditor
  , setUiHexTooltipPinned
  , setUiPanOffset
  , setUiHoverHex
  , setUiHoverWidget
  , setUiShowConfig
  , setUiMenuMode
  , setUiPresetInput
  , setUiPresetSelected
  , setUiWorldName
  , setUiWorldConfig
  , setUiWorldSaveInput
  , setUiWorldSelected
  , setUiZoom
  , setUiOverlayNames
  )
import Control.Monad (when)
import Data.IORef (IORef, modifyIORef', readIORef, writeIORef)
import qualified Data.Text as Text
import qualified Data.IntMap.Strict as IntMap
import Linear (V2(..))
import qualified SDL
import Hyperspace.Actor (ActorHandle, Protocol)
import System.FilePath ((</>))
import Seer.Draw (seedMaxDigits)
import Seer.Input.ConfigScroll
  ( computeScrollUpdates
  , defaultScrollSettings
  )
import Seer.Input.Context (DragState(..), InputContext(..), TooltipHover)
import Seer.Input.Modal (handleModalListKey, handleModalTextKey, handleModalTextInput)
import Seer.Input.Seed (bumpSeed, handleSeedKey, handleSeedTextInput)
import Seer.Input.ViewControls
  ( applyZoomAtCursor
  , defaultZoomSettings
  , viewModeForKey
  )
import Topo (ChunkCoord(..), ChunkId(..), TileCoord(..), WorldConfig(..), chunkCoordFromTile, chunkIdFromCoord)
import UI.HexPick (screenToAxial)
import UI.Layout
import UI.WidgetTree (Widget(..), WidgetId(..), buildWidgets, buildPluginWidgets, buildSliderRowWidgets, hitTest)
import UI.Widgets (Rect(..), containsPoint)
import Seer.Input.Actions (InputEnv(..), submitAction)
import qualified Seer.Input.Actions as InputActions
import Seer.Editor.Types (EditorState(..), EditorTool(..), BrushSettings(..))
import Actor.UiActions (UiAction(..))
import Actor.UiActions.Handles (ActorHandles(..))
import Actor.PluginManager (getPluginDataDirectories, notifyWorldChanged)
import Actor.Simulation (setSimWorld)
import Seer.Config.Snapshot (applySnapshotToUi, loadSnapshot, saveSnapshot, snapshotDir, snapshotFromUi)
import Seer.Input.Widgets (handleClick)
import Seer.World.Persist (loadNamedWorld, saveNamedWorldWithPlugins, snapshotToWorld, worldDir)
import Seer.World.Persist.Types (WorldSaveManifest(..))
import Topo.Overlay (overlayNames)
import Topo.World (TerrainWorld(..))
import UI.HexPick (renderHexRadiusPx, screenToAxial)

-- | Number of consecutive frames the cursor must remain still on a
-- slider row before the tooltip appears.
tooltipDelayFrames :: Int
tooltipDelayFrames = 15

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
      let (wx, wy) = screenToWorld uiSnap (fromIntegral mx, fromIntegral my)
          (q, r) = screenToAxial renderHexRadiusPx (round wx) (round wy)
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
            Just hex -> submitAction inputEnv (UiActionBrushStroke hex)
            Nothing  -> pure ()
      -- Widget hover detection for tooltips (only for active tab sliders)
      do (V2 winW winH) <- SDL.get (SDL.windowSize window)
         logSnap <- getLogSnapshot logHandle
         let logHeight = if lsCollapsed logSnap then 24 else 160
             seedWidth = max 120 (seedMaxDigits * 10)
             hoverLayout = layoutForSeed (V2 (fromIntegral winW) (fromIntegral winH)) logHeight seedWidth
             point = V2 (fromIntegral mx) (fromIntegral my)
         if uiShowConfig uiSnap
           then do
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
                 hoverResult
                   | containsPoint scrollArea point = hitTest activeRows scrolledPoint
                   | otherwise = Nothing
             -- Record which widget the cursor is over and reset the
             -- frame counter.  The actual tooltip is fired by
             -- 'tickTooltipHover' once the counter reaches 0.
             pending <- readIORef tooltipHoverRef
             case hoverResult of
               Nothing -> do
                 writeIORef tooltipHoverRef Nothing
                 setUiHoverWidget uiHandle Nothing
               Just wid -> do
                 case pending of
                   Just (prevWid, _)
                     | prevWid == wid ->
                         -- Same widget; reset the frame counter so
                         -- tooltip only appears after the cursor stops.
                         writeIORef tooltipHoverRef (Just (wid, tooltipDelayFrames))
                     | otherwise -> do
                         -- Different widget; restart counter, hide tooltip
                         writeIORef tooltipHoverRef (Just (wid, tooltipDelayFrames))
                         setUiHoverWidget uiHandle Nothing
                   Nothing -> do
                     writeIORef tooltipHoverRef (Just (wid, tooltipDelayFrames))
                     setUiHoverWidget uiHandle Nothing
           else do
             writeIORef tooltipHoverRef Nothing
             setUiHoverWidget uiHandle Nothing
    SDL.MouseWheelEvent wheelEvent -> do
      let SDL.V2 _ dy = SDL.mouseWheelEventPos wheelEvent
      when (dy /= 0) $ do
        logSnap <- InputActions.getLogSnapshot inputEnv
        uiSnap <- InputActions.getUiSnapshot inputEnv
        (mx, my) <- readIORef mousePosRef
        (V2 winW winH) <- SDL.get (SDL.windowSize window)
        lineHeight <- readIORef (icLineHeightRef inputContext)
        let (configUpdate, logUpdate) =
              computeScrollUpdates defaultScrollSettings uiSnap logSnap lineHeight (V2 (fromIntegral winW) (fromIntegral winH)) (V2 mx my) (fromIntegral dy)
        case (configUpdate, logUpdate) of
          (Just newConfigScroll, _) ->
            setUiConfigScroll uiHandle newConfigScroll
          (Nothing, Just newScroll) ->
            setLogScroll logHandle newScroll
          (Nothing, Nothing) -> do
            let (newZoom, newOffset) = applyZoomAtCursor defaultZoomSettings uiSnap (mx, my) (fromIntegral dy)
            setUiZoom uiHandle newZoom
            setUiPanOffset uiHandle newOffset
            dataSnap <- InputActions.getDataSnapshot inputEnv
            terrainSnap <- InputActions.getTerrainSnapshot inputEnv
            let hasMissing = tsChunkSize terrainSnap > 0
                  && IntMap.size (tsTerrainChunks terrainSnap) < dsTerrainChunks dataSnap
            when hasMissing $
              submitAction inputEnv (UiActionRebuildAtlas (uiViewMode uiSnap))
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
                then case uiHoverHex uiSnap' of
                  Just hex -> submitAction inputEnv (UiActionBrushStroke hex)
                  Nothing  -> handleClick inputContext (SDL.mouseButtonEventPos btnEvent)
                else handleClick inputContext (SDL.mouseButtonEventPos btnEvent)
      | SDL.mouseButtonEventMotion btnEvent == SDL.Released ->
          case SDL.mouseButtonEventButton btnEvent of
            SDL.ButtonRight -> do
              state <- readIORef dragRef
              writeIORef dragRef Nothing
              case state of
                Just DragState { dsDragging = False } -> do
                  uiSnap <- getUiSnapshot uiHandle
                  setUiHexTooltipPinned uiHandle (not (uiHexTooltipPinned uiSnap))
                _ -> pure ()
            SDL.ButtonLeft -> do
              -- Reset flatten reference on stroke end
              uiSnap <- getUiSnapshot uiHandle
              let editor = uiEditor uiSnap
              when (editorActive editor && editorTool editor == ToolFlatten) $
                setUiEditor uiHandle (editor { editorFlattenRef = Nothing })
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
    SDL.KeyboardEvent keyboardEvent
      | SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed ->
          do
            uiSnap <- InputActions.getUiSnapshot inputEnv
            let keycode = SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent)
            if uiSeedEditing uiSnap
              then handleSeedKey uiHandle (getUiSnapshot uiHandle) keycode
              else case uiMenuMode uiSnap of
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
                        SDL.KeycodeZ -> submitAction inputEnv UiActionUndo
                        SDL.KeycodeY -> submitAction inputEnv UiActionRedo
                        _ -> handleEditorKey editor keycode
                      else handleEditorKey editor keycode
                    else case keycode of
                      SDL.KeycodeEscape -> closeContextOrMenu
                      SDL.KeycodeG -> submitAction inputEnv UiActionGenerate
                      SDL.KeycodeC -> toggleConfig
                      SDL.KeycodeE -> toggleEditor
                      SDL.KeycodeUp -> bumpSeed uiHandle (getUiSnapshot uiHandle) 1
                      SDL.KeycodeDown -> bumpSeed uiHandle (getUiSnapshot uiHandle) (-1)
                      SDL.KeycodeL -> do
                        logSnap <- getLogSnapshot logHandle
                        setLogCollapsed logHandle (not (lsCollapsed logSnap))
                      _ ->
                        case viewModeForKey keycode of
                          Just mode -> submitAction inputEnv (UiActionSetViewMode mode)
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

    simulationHandle = ahSimulationHandle actorHandles

    dragThreshold :: Float
    dragThreshold = 4
    screenToWorld uiSnap (sx, sy) =
      let (ox, oy) = uiPanOffset uiSnap
          z = uiZoom uiSnap
      in (sx / z - ox, sy / z - oy)

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
      setUiEditor uiHandle (editor { editorActive = not (editorActive editor) })
    handleEditorKey :: EditorState -> SDL.Keycode -> IO ()
    handleEditorKey editor keycode = case keycode of
      SDL.KeycodeEscape ->
        setUiEditor uiHandle (editor { editorActive = False })
      SDL.KeycodeE ->
        setUiEditor uiHandle (editor { editorActive = False })
      SDL.Keycode1 ->
        setUiEditor uiHandle (editor { editorTool = ToolRaise })
      SDL.Keycode2 ->
        setUiEditor uiHandle (editor { editorTool = ToolLower })
      SDL.Keycode3 ->
        setUiEditor uiHandle (editor { editorTool = ToolSmooth })
      SDL.Keycode4 ->
        setUiEditor uiHandle (editor { editorTool = ToolFlatten })
      SDL.Keycode5 ->
        setUiEditor uiHandle (editor { editorTool = ToolNoise })
      SDL.Keycode6 ->
        setUiEditor uiHandle (editor { editorTool = ToolPaintBiome })
      SDL.Keycode7 ->
        setUiEditor uiHandle (editor { editorTool = ToolPaintForm })
      SDL.Keycode8 ->
        setUiEditor uiHandle (editor { editorTool = ToolSetHardness })
      SDL.KeycodeLeftBracket ->
        let brush = editorBrush editor
            r = max 0 (brushRadius brush - 1)
        in setUiEditor uiHandle (editor { editorBrush = brush { brushRadius = r } })
      SDL.KeycodeRightBracket ->
        let brush = editorBrush editor
            r = min 6 (brushRadius brush + 1)
        in setUiEditor uiHandle (editor { editorBrush = brush { brushRadius = r } })
      _ -> pure ()
    closeContextOrMenu = do
      uiSnap <- getUiSnapshot uiHandle
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
            dir <- snapshotDir
            let path = dir </> Text.unpack name <> ".json"
            _result <- saveSnapshot path (snapshotFromUi uiSnap' name)
            setUiMenuMode uiHandle MenuNone
            SDL.stopTextInput)
        -- onCancel
        (setUiMenuMode uiHandle MenuNone >> SDL.stopTextInput)
        -- onBackspace
        (do uiSnap' <- getUiSnapshot uiHandle
            setUiPresetInput uiHandle (Text.dropEnd 1 (uiPresetInput uiSnap')))

    handlePresetLoadKey :: UiState -> SDL.Keycode -> IO ()
    handlePresetLoadKey uiSnap keycode =
      handleModalListKey keycode
        (uiPresetSelected uiSnap)
        (length (uiPresetList uiSnap) - 1)
        -- onConfirm
        (do let names = uiPresetList uiSnap
                sel = uiPresetSelected uiSnap
            when (sel >= 0 && sel < length names) $ do
              let name = names !! sel
              dir <- snapshotDir
              let path = dir </> Text.unpack name <> ".json"
              result <- loadSnapshot path
              case result of
                Right cp -> applySnapshotToUi cp uiHandle
                Left _err -> pure ()
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
                pmHandle = ahPluginManagerHandle actorHandles
            when (not (Text.null name)) $ do
              terrainSnap <- getTerrainSnapshot dataHandle
              let world = snapshotToWorld uiSnap' terrainSnap
              pluginDirs <- getPluginDataDirectories pmHandle
              _result <- saveNamedWorldWithPlugins name uiSnap' world pluginDirs
              wDir <- worldDir
              notifyWorldChanged pmHandle (Just (Text.pack (wDir </> Text.unpack name)))
              setUiWorldName uiHandle name
              setUiWorldConfig uiHandle (Just (snapshotFromUi uiSnap' name))
            setUiMenuMode uiHandle MenuNone
            SDL.stopTextInput)
        -- onCancel
        (setUiMenuMode uiHandle MenuNone >> SDL.stopTextInput)
        -- onBackspace
        (do uiSnap' <- getUiSnapshot uiHandle
            setUiWorldSaveInput uiHandle (Text.dropEnd 1 (uiWorldSaveInput uiSnap')))

    handleWorldLoadKey :: UiState -> SDL.Keycode -> IO ()
    handleWorldLoadKey uiSnap keycode =
      handleModalListKey keycode
        (uiWorldSelected uiSnap)
        (length (uiWorldList uiSnap) - 1)
        -- onConfirm
        (do let manifests = uiWorldList uiSnap
                sel = uiWorldSelected uiSnap
            when (sel >= 0 && sel < length manifests) $ do
              let manifest = manifests !! sel
                  name = wsmName manifest
              result <- loadNamedWorld name
              case result of
                Right (_manifest, snapshot, world) -> do
                  replaceTerrainData dataHandle world
                  setSimWorld simulationHandle world
                  setUiOverlayNames uiHandle (overlayNames (twOverlays world))
                  dataSnap <- getDataSnapshot dataHandle
                  terrainSnap' <- getTerrainSnapshot dataHandle
                  writeDataSnapshot (ahDataSnapshotRef actorHandles) dataSnap
                  writeTerrainSnapshot (ahTerrainSnapshotRef actorHandles) terrainSnap'
                  bumpSnapshotVersion (ahSnapshotVersionRef actorHandles)
                  applySnapshotToUi snapshot uiHandle
                  setUiWorldName uiHandle name
                  setUiWorldConfig uiHandle (Just snapshot)
                  -- Notify plugins of the loaded world path
                  let pmHandle = ahPluginManagerHandle actorHandles
                  wDir <- worldDir
                  notifyWorldChanged pmHandle (Just (Text.pack (wDir </> Text.unpack name)))
                  submitAction inputEnv (UiActionRebuildAtlas (uiViewMode uiSnap))
                Left _err -> pure ()
            setUiMenuMode uiHandle MenuNone)
        -- onCancel
        (setUiMenuMode uiHandle MenuNone >> SDL.stopTextInput)
        -- setSelection
        (setUiWorldSelected uiHandle)

-- | Per-frame tick for the tooltip hover delay.  Decrements the
-- remaining frame count and promotes the pending hover to a visible
-- tooltip once it reaches zero.  Returns 'True' when it fired so
-- the caller can request a UI snapshot refresh.
tickTooltipHover
  :: IORef TooltipHover
  -> ActorHandle Ui (Protocol Ui)
  -> IO Bool
tickTooltipHover tooltipHoverRef uiHandle = do
  pending <- readIORef tooltipHoverRef
  case pending of
    Just (wid, n)
      | n <= 1 -> do
          -- Counter expired; lock it at 0 and show tooltip
          writeIORef tooltipHoverRef (Just (wid, 0))
          setUiHoverWidget uiHandle (Just wid)
          pure True
      | otherwise -> do
          modifyIORef' tooltipHoverRef (fmap (\(w, c) -> (w, c - 1)))
          pure False
    Nothing -> pure False

