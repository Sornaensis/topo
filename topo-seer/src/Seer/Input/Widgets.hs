{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Seer.Input.Widgets
  ( handleClick
  ) where

import Actor.Data (DataSnapshot(..), DataSnapshotReply, getTerrainSnapshot, replaceTerrainData, requestDataSnapshot)
import Actor.Log (LogEntry(..), LogLevel(..), LogSnapshot(..), appendLog, getLogSnapshot, setLogCollapsed, setLogMinLevel, setLogScroll)
import Actor.UI
  ( ConfigTab(..)
  , LeftTab(..)
  , Ui
  , UiMenuMode(..)
  , UiState(..)
  , ViewMode(..)
  , configRowCount
  , getUiSnapshot
  , setUiChunkSize
  , setUiConfigTab
  , setUiConfigScroll
  , setUiContextHex
  , setUiContextPos
  , setUiDisabledStages
  , setUiLeftTab
  , setUiSeed
  , setUiSeedEditing
  , setUiSeedInput
  , setUiShowConfig
  , setUiShowLeftPanel
  , setUiMenuMode
  , setUiPresetInput
  , setUiPresetList
  , setUiPresetSelected
  , setUiWorldName
  , setUiWorldConfig
  , setUiWorldSaveInput
  , setUiWorldList
  , setUiWorldSelected
  , setUiZoom
  , setUiSimAutoTick
  , setUiSimTickCount
  , setUiPluginNames
  , setUiOverlayNames
  )
import Control.Monad (when)
import Data.IORef (writeIORef)
import Data.Int (Int32)
import Data.List (findIndex, partition)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Linear (V2(..))
import qualified SDL
import qualified SDL.Raw.Types as Raw
import Seer.Config.SliderRegistry
  ( SliderPart(..)
  , SliderDef(..)
  , sliderDefForWidget
  , sliderWidgetPart
  )
import Seer.Config.SliderState (bumpSliderValue)
import Seer.Config.SliderStyle (SliderStyle(..), sliderStyleForId)
import Seer.Config.SliderUi (configTabForSliderTab)
import System.FilePath ((</>))
import Seer.Draw (seedMaxDigits)
import Seer.Config.Snapshot (listSnapshots, loadSnapshot, snapshotDir, snapshotFromUi, saveSnapshot, applySnapshotToUi)
import Seer.World.Persist (listWorlds, saveNamedWorld, loadNamedWorld, snapshotToWorld)
import Seer.World.Persist.Types (WorldSaveManifest(..))
import Seer.Input.ConfigScroll
  ( ScrollSettings
  , computeScrollUpdates
  , defaultScrollSettings
  )
import Seer.Input.Modal (handleModalTextKey, handleModalTextInput, handleModalListKey)
import Seer.Input.Router (isQuit)
import Seer.Input.Seed (bumpSeed, handleSeedKey, handleSeedTextInput)
import Seer.Input.ViewControls
  ()
import Topo.Overlay (overlayNames)
import Topo.Pipeline.Dep (builtinDependencies, disabledClosure)
import Topo.Pipeline.Stage (StageId, parseStageId)
import Topo.World (TerrainWorld(..))
import UI.Layout
import UI.WidgetTree (Widget(..), WidgetId(..), buildWidgets, buildPluginWidgets, buildSliderRowWidgets, hitTest)
import UI.Widgets (Rect(..), containsPoint)
import System.Random (randomIO)
import Hyperspace.Actor (ActorHandle, Protocol, replyTo)
import Actor.PluginManager (setPluginOrder)
import Actor.Simulation (requestSimTick, setSimWorld)
import Actor.UiActions (ActorHandles(..), UiAction(..))
import Seer.Input.Actions (InputEnv(..), submitAction)
import Seer.Input.Context (InputContext(..))
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
      dataHandle = ahDataHandle actorHandles
      snapshotReceiverHandle = ahSnapshotReceiverHandle actorHandles
      pluginManagerHandle = ahPluginManagerHandle actorHandles
      simulationHandle = ahSimulationHandle actorHandles
      quitRef = icQuitRef inputContext
  (V2 winW winH) <- SDL.get (SDL.windowSize window)
  let logSnap = ieLogSnapshot widgetEnv
      uiSnap = ieUiSnapshot widgetEnv
      point = V2 (fromIntegral x) (fromIntegral y)
      logHeight = if lsCollapsed logSnap then 24 else 160
      seedWidth = max 120 (seedMaxDigits * 10)
      layout = layoutForSeed (V2 (fromIntegral winW) (fromIntegral winH)) logHeight seedWidth
      simWorldReady = dsTerrainChunks (ieDataSnapshot widgetEnv) > 0
      seedValue = configSeedValueRect layout
      scrollArea = configScrollAreaRect layout
      scrollBar = configScrollBarRect layout
      inScrollBar = uiShowConfig uiSnap && containsPoint scrollBar point
      inConfigScroll = uiShowConfig uiSnap && containsPoint scrollArea point
      scrollPoint = if inConfigScroll
        then V2 (fromIntegral x) (fromIntegral y + uiConfigScroll uiSnap)
        else point
      widgetsAll = buildWidgets layout ++ buildPluginWidgets (uiPluginNames uiSnap) layout
      widgets =
        if uiShowConfig uiSnap
          then filter (configWidgetAllowed simWorldReady (uiConfigTab uiSnap)) widgetsAll
          else widgetsAll
      isConfigSliderWidget = isJust . sliderDefForWidget
      (configSliderWidgets, otherWidgets) = partition (isConfigSliderWidget . widgetId) widgets
      hitWidget =
        if inConfigScroll
          then case hitTest configSliderWidgets scrollPoint of
            Just wid -> Just wid
            Nothing -> hitTest otherWidgets point
          else hitTest widgets point
      configWidgetAllowed simReady tab widget =
        case sliderDefForWidget (widgetId widget) of
          Just sliderDef -> tab == configTabForSliderTab (sliderTab sliderDef)
          Nothing -> bespokeConfigWidgetAllowed simReady tab (widgetId widget)
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
          { Just WidgetGenerate -> whenLeftTopo (submit UiActionGenerate)
          ; Just WidgetLeftToggle -> setUiShowLeftPanel uiHandle (not (uiShowLeftPanel uiSnap))
          ; Just WidgetLeftTabTopo -> whenLeftPanel (setUiLeftTab uiHandle LeftTopo)
          ; Just WidgetLeftTabView -> whenLeftPanel (setUiLeftTab uiHandle LeftView)
          ; Just WidgetSeedValue -> whenLeftTopo (startSeedEdit seedValue)
          ; Just WidgetSeedRandom -> whenLeftTopo randomSeed
          ; Just WidgetChunkMinus -> whenLeftTopo (bumpChunk (-8))
          ; Just WidgetChunkPlus -> whenLeftTopo (bumpChunk 8)
          ; Just WidgetConfigToggle -> toggleConfig
          ; Just WidgetConfigTabTerrain -> whenConfigVisible (setUiConfigTab uiHandle ConfigTerrain >> setUiConfigScroll uiHandle 0)
          ; Just WidgetConfigTabPlanet -> whenConfigVisible (setUiConfigTab uiHandle ConfigPlanet >> setUiConfigScroll uiHandle 0)
          ; Just WidgetConfigTabClimate -> whenConfigVisible (setUiConfigTab uiHandle ConfigClimate >> setUiConfigScroll uiHandle 0)
          ; Just WidgetConfigTabWeather -> whenConfigVisible (setUiConfigTab uiHandle ConfigWeather >> setUiConfigScroll uiHandle 0)
          ; Just WidgetConfigTabBiome -> whenConfigVisible (setUiConfigTab uiHandle ConfigBiome >> setUiConfigScroll uiHandle 0)
          ; Just WidgetConfigTabErosion -> whenConfigVisible (setUiConfigTab uiHandle ConfigErosion >> setUiConfigScroll uiHandle 0)
          ; Just WidgetConfigTabPipeline -> whenConfigVisible (setUiConfigTab uiHandle ConfigPipeline >> setUiConfigScroll uiHandle 0)
          ; Just wid
          | Just (sliderDef, sliderPart) <- sliderWidgetPart wid
              -> whenConfigVisible
                (let sliderStyle = sliderStyleForId (sliderId sliderDef)
                 in bumpSliderValue uiHandle
                      (sliderId sliderDef)
                      (signedSliderDelta sliderPart (sliderStyleStep sliderStyle)))
          ; Just wid
          | isBespokeConfigWidget wid ->
              handleBespokeConfigWidget layout wid whenConfigVisible uiSnap
          ; Just WidgetViewElevation -> whenLeftView (submit (UiActionSetViewMode ViewElevation))
          ; Just WidgetViewBiome -> whenLeftView (submit (UiActionSetViewMode ViewBiome))
          ; Just WidgetViewClimate -> whenLeftView (submit (UiActionSetViewMode ViewClimate))
          ; Just WidgetViewMoisture -> whenLeftView (submit (UiActionSetViewMode ViewMoisture))
          ; Just WidgetViewPrecip -> whenLeftView (submit (UiActionSetViewMode ViewWeather))
          ; Just WidgetViewPlateId -> whenLeftView (submit (UiActionSetViewMode ViewPlateId))
          ; Just WidgetViewPlateBoundary -> whenLeftView (submit (UiActionSetViewMode ViewPlateBoundary))
          ; Just WidgetViewPlateHardness -> whenLeftView (submit (UiActionSetViewMode ViewPlateHardness))
          ; Just WidgetViewPlateCrust -> whenLeftView (submit (UiActionSetViewMode ViewPlateCrust))
          ; Just WidgetViewPlateAge -> whenLeftView (submit (UiActionSetViewMode ViewPlateAge))
          ; Just WidgetViewPlateHeight -> whenLeftView (submit (UiActionSetViewMode ViewPlateHeight))
          ; Just WidgetViewPlateVelocity -> whenLeftView (submit (UiActionSetViewMode ViewPlateVelocity))
          -- Overlay cycling: prev/next overlay name, prev/next field
          ; Just WidgetViewOverlayPrev -> whenLeftView $ cycleOverlay uiSnap uiHandle (-1) submit
          ; Just WidgetViewOverlayNext -> whenLeftView $ cycleOverlay uiSnap uiHandle 1 submit
          ; Just WidgetViewFieldPrev -> whenLeftView $ cycleOverlayField uiSnap uiHandle (-1) submit
          ; Just WidgetViewFieldNext -> whenLeftView $ cycleOverlayField uiSnap uiHandle 1 submit
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

    dataHandle = ahDataHandle actorHandles

    snapshotReceiverHandle = ahSnapshotReceiverHandle actorHandles

    pluginManagerHandle = ahPluginManagerHandle actorHandles

    simulationHandle = ahSimulationHandle actorHandles

    quitRef = icQuitRef inputContext

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
      _ -> False

    bespokeConfigWidgetAllowed :: Bool -> ConfigTab -> WidgetId -> Bool
    bespokeConfigWidgetAllowed simReady tab wid = case wid of
      WidgetSimTick -> tab == ConfigPipeline && simReady
      WidgetSimAutoTick -> tab == ConfigPipeline
      WidgetPluginMoveUp _ -> tab == ConfigPipeline
      WidgetPluginMoveDown _ -> tab == ConfigPipeline
      _ -> True

    handleBespokeConfigWidget :: Layout -> WidgetId -> (IO () -> IO ()) -> UiState -> IO ()
    handleBespokeConfigWidget currentLayout wid whenConfigVisible uiState = case wid of
      WidgetConfigPresetSave -> whenConfigVisible (openPresetSaveDialog currentLayout uiState)
      WidgetConfigPresetLoad -> whenConfigVisible openPresetLoadDialog
      WidgetConfigReset -> whenConfigVisible (SDL.stopTextInput >> submit UiActionReset)
      WidgetConfigRevert -> whenConfigVisible (submit UiActionRevert)
      WidgetPipelineToggle name -> whenConfigVisible $ do
        case parseStageId name of
          Nothing -> pure ()
          Just sid -> do
            let current = uiDisabledStages uiState
                toggled :: Set.Set StageId
                toggled
                  | Set.member sid current = Set.delete sid current
                  | otherwise              = Set.insert sid current
                closed = disabledClosure builtinDependencies toggled
            setUiDisabledStages uiHandle closed
      WidgetSimTick -> whenConfigVisible $ do
        let count = uiSimTickCount uiState
            hasTerrain = dsTerrainChunks (ieDataSnapshot widgetEnv) > 0
        if hasTerrain
          then do
            appendLog logHandle (LogEntry LogInfo (Text.pack ("ui: tick button pressed -> request tick " <> show (count + 1))))
            requestSimTick simulationHandle (count + 1)
          else
            appendLog logHandle (LogEntry LogWarn "ui: tick ignored (no world terrain loaded yet)")
      WidgetSimAutoTick -> whenConfigVisible $ do
        setUiSimAutoTick uiHandle (not (uiSimAutoTick uiState))
      WidgetPluginMoveUp name -> whenConfigVisible $ do
        let names = uiPluginNames uiState
            swapped = swapWithPrev name names
        setUiPluginNames uiHandle swapped
        setPluginOrder pluginManagerHandle swapped
      WidgetPluginMoveDown name -> whenConfigVisible $ do
        let names = uiPluginNames uiState
            swapped = swapWithNext name names
        setUiPluginNames uiHandle swapped
        setPluginOrder pluginManagerHandle swapped
      _ -> pure ()

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
          openWorldLoadDialog
      | otherwise = setUiMenuMode uiHandle MenuNone

    handlePresetSaveClick ly point uiSnap
      | containsPoint (presetSaveOkRect ly) point = confirmPresetSave uiSnap
      | containsPoint (presetSaveCancelRect ly) point = cancelPresetDialog
      | otherwise = pure ()

    handlePresetLoadClick ly point uiSnap
      | containsPoint (presetLoadOkRect ly) point = confirmPresetLoad uiSnap
      | containsPoint (presetLoadCancelRect ly) point = cancelPresetDialog
      | containsPoint (presetLoadListRect ly) point = do
          -- Determine which item was clicked by y position
          let V2 _mx my = point
              Rect (V2 _lx listY, _) = presetLoadListRect ly
              idx = max 0 (my - listY) `div` 24
          setUiPresetSelected uiHandle idx
      | otherwise = pure ()

    -- Preset dialog helpers

    openPresetSaveDialog ly uiSnap = do
      let defaultName = "preset-" <> Text.pack (show (uiSeed uiSnap))
          Rect (V2 rx ry, V2 rw rh) = presetSaveInputRect ly
          rawRect = Raw.Rect (fromIntegral rx) (fromIntegral ry) (fromIntegral rw) (fromIntegral rh)
      setUiPresetInput uiHandle defaultName
      setUiMenuMode uiHandle MenuPresetSave
      SDL.startTextInput rawRect

    openPresetLoadDialog = do
      names <- listSnapshots
      setUiPresetList uiHandle names
      setUiPresetSelected uiHandle 0
      setUiMenuMode uiHandle MenuPresetLoad

    cancelPresetDialog = do
      setUiMenuMode uiHandle MenuNone
      SDL.stopTextInput

    confirmPresetSave uiSnap = do
      let name = uiPresetInput uiSnap
      dir <- snapshotDir
      let path = dir </> Text.unpack name <> ".json"
      _result <- saveSnapshot path (snapshotFromUi uiSnap name)
      setUiMenuMode uiHandle MenuNone
      SDL.stopTextInput

    confirmPresetLoad uiSnap = do
      let names = uiPresetList uiSnap
          sel = uiPresetSelected uiSnap
      when (sel >= 0 && sel < length names) $ do
        let name = names !! sel
        dir <- snapshotDir
        let path = dir </> Text.unpack name <> ".json"
        result <- loadSnapshot path
        case result of
          Right cp -> applySnapshotToUi cp uiHandle
          Left _err -> pure ()
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
              idx = max 0 (my - listY) `div` 28
          setUiWorldSelected uiHandle idx
      | otherwise = pure ()

    -- World dialog helpers

    openWorldSaveDialog ly uiSnap = do
      let defaultName = uiWorldName uiSnap
          Rect (V2 rx ry, V2 rw rh) = worldSaveInputRect ly
          rawRect = Raw.Rect (fromIntegral rx) (fromIntegral ry) (fromIntegral rw) (fromIntegral rh)
      setUiWorldSaveInput uiHandle defaultName
      setUiMenuMode uiHandle MenuWorldSave
      SDL.startTextInput rawRect

    openWorldLoadDialog = do
      worlds <- listWorlds
      setUiWorldList uiHandle worlds
      setUiWorldSelected uiHandle 0
      setUiMenuMode uiHandle MenuWorldLoad

    cancelWorldDialog = do
      setUiMenuMode uiHandle MenuNone
      SDL.stopTextInput

    confirmWorldSave uiSnap = do
      let name = uiWorldSaveInput uiSnap
      when (not (Text.null name)) $ do
        terrainSnap <- getTerrainSnapshot dataHandle
        let world = snapshotToWorld terrainSnap
        _result <- saveNamedWorld name uiSnap world
        setUiWorldName uiHandle name
        setUiWorldConfig uiHandle (Just (snapshotFromUi uiSnap name))
      setUiMenuMode uiHandle MenuNone
      SDL.stopTextInput

    confirmWorldLoad uiSnap = do
      let manifests = uiWorldList uiSnap
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
            requestDataSnapshot dataHandle (replyTo @DataSnapshotReply snapshotReceiverHandle)
            applySnapshotToUi snapshot uiHandle
            setUiWorldName uiHandle name
            setUiWorldConfig uiHandle (Just snapshot)
            submit (UiActionRebuildAtlas (uiViewMode uiSnap))
          Left _err -> pure ()
      setUiMenuMode uiHandle MenuNone

    bumpSeed delta = do
      uiSnap <- getUiSnapshot uiHandle
      let newSeed = uiSeed uiSnap + fromIntegral delta
      setUiSeed uiHandle newSeed
      setUiSeedInput uiHandle (Text.pack (show newSeed))
    bumpChunk delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiChunkSize uiHandle (uiChunkSize uiSnap + delta)
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
    startSeedEdit rect = do
      let uiSnap = ieUiSnapshot widgetEnv
          Rect (V2 rx ry, V2 rw rh) = rect
          rawRect = Raw.Rect (fromIntegral rx) (fromIntegral ry) (fromIntegral rw) (fromIntegral rh)
      SDL.startTextInput rawRect
      setUiSeedEditing uiHandle True
      setUiSeedInput uiHandle (Text.pack (show (uiSeed uiSnap)))
    randomSeed = do
      seed <- randomIO
      setUiSeed uiHandle seed
      setUiSeedInput uiHandle (Text.pack (show seed))
      setUiSeedEditing uiHandle False
      SDL.stopTextInput

    signedSliderDelta sliderPart step = case sliderPart of
      SliderPartMinus -> negate step
      SliderPartPlus -> step
-- | Cycle through available overlay names by @dir@ (+1 or -1).
--
-- When no overlays are available, does nothing.  When cycling past the
-- end, wraps to ViewElevation (no overlay); from ViewElevation, wraps to
-- the first overlay.
cycleOverlay :: UiState -> ActorHandle Ui (Protocol Ui) -> Int -> (UiAction -> IO ()) -> IO ()
cycleOverlay uiSnap uiHandle dir submit = do
  let names = uiOverlayNames uiSnap
  if null names
    then pure ()
    else do
      let currentIdx = case uiViewMode uiSnap of
            ViewOverlay name _ ->
              case findIndex (== name) names of
                Just i  -> i + 1  -- +1 because index 0 = "no overlay"
                Nothing -> 0
            _ -> 0
          total = length names + 1  -- +1 for "no overlay" position
          newIdx = (currentIdx + dir) `mod` total
      if newIdx == 0
        then submit (UiActionSetViewMode ViewElevation)
        else do
          let overlayName = names !! (newIdx - 1)
          -- Reset to first field when switching overlay
          submit (UiActionSetViewMode (ViewOverlay overlayName 0))

-- | Cycle through fields within the currently-selected overlay.
--
-- Only effective when in 'ViewOverlay' mode with fields available.
cycleOverlayField :: UiState -> ActorHandle Ui (Protocol Ui) -> Int -> (UiAction -> IO ()) -> IO ()
cycleOverlayField uiSnap _uiHandle dir submit =
  case uiViewMode uiSnap of
    ViewOverlay name fieldIdx -> do
      let fields = uiOverlayFields uiSnap
          fieldCount = length fields
      if fieldCount <= 0
        then pure ()
        else do
          let newIdx = (fieldIdx + dir) `mod` fieldCount
          submit (UiActionSetViewMode (ViewOverlay name newIdx))
    _ -> pure ()

-- | Swap an element with the one before it in a list.
--
-- If the element is at index 0 or not found, the list is returned unchanged.
swapWithPrev :: Eq a => a -> [a] -> [a]
swapWithPrev _ [] = []
swapWithPrev target (x:xs)
  | x == target = x : xs  -- already first, no swap
  | otherwise   = go x xs
  where
    go prev []     = [prev]  -- target not found, reconstruct
    go prev (y:ys)
      | y == target = target : prev : ys
      | otherwise   = prev : go y ys

-- | Swap an element with the one after it in a list.
--
-- If the element is last or not found, the list is returned unchanged.
swapWithNext :: Eq a => a -> [a] -> [a]
swapWithNext _ [] = []
swapWithNext target xs = go xs
  where
    go []         = []
    go [y]        = [y]  -- last element, no swap
    go (y:z:ys)
      | y == target = z : y : ys
      | otherwise   = y : go (z:ys)
