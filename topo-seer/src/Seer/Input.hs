{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Seer.Input
  ( isQuit
  , handleEvent
  , handleClick
  , TooltipHover
  , tickTooltipHover
  , tooltipDelayFrames
  ) where

import Actor.Data (Data, DataSnapshot(..), DataSnapshotReply, TerrainSnapshot(..), replaceTerrainData, requestDataSnapshot)
import Actor.Log (Log, LogLevel(..), LogSnapshot(..), setLogCollapsed, setLogMinLevel, setLogScroll)
import Actor.Terrain (Terrain, TerrainReplyOps)
import Actor.UI
  ( ConfigTab(..)
  , LeftTab(..)
  , Ui
  , UiMenuMode(..)
  , UiState(..)
  , ViewMode(..)
  , setUiChunkSize
  , setUiConfigTab
  , setUiConfigScroll
  , setUiContextHex
  , setUiContextPos
  , setUiErosionHydraulic
  , setUiErosionMaxDrop
  , setUiErosionTalus
  , setUiErosionThermal
  , setUiEquatorTemp
  , setUiEvaporation
  , setUiGenFrequency
  , setUiGenGain
  , setUiGenLacunarity
  , setUiGenOctaves
  , setUiGenScale
  , setUiGenCoordScale
  , setUiGenOffsetX
  , setUiGenOffsetY
  , setUiGenWarpScale
  , setUiGenWarpStrength
  , setUiPanOffset
  , setUiWorldExtentX
  , setUiWorldExtentY
  , setUiEdgeDepthNorth
  , setUiEdgeDepthSouth
  , setUiEdgeDepthEast
  , setUiEdgeDepthWest
  , setUiEdgeDepthFalloff
  , setUiDetailScale
  , setUiPlateSpeed
  , setUiBoundarySharpness
  , setUiBoundaryNoiseScale
  , setUiBoundaryNoiseStrength
  , setUiBoundaryWarpOctaves
  , setUiBoundaryWarpLacunarity
  , setUiBoundaryWarpGain
  , setUiPlateMergeScale
  , setUiPlateMergeBias
  , setUiPlateDetailScale
  , setUiPlateDetailStrength
  , setUiPlateRidgeStrength
  , setUiPlateHeightBase
  , setUiPlateHeightVariance
  , setUiPlateHardnessBase
  , setUiPlateHardnessVariance
  , setUiTrenchDepth
  , setUiRidgeHeight
  , setUiPlateBiasStrength
  , setUiPlateBiasCenter
  , setUiPlateBiasEdge
  , setUiPlateBiasNorth
  , setUiPlateBiasSouth
  , setUiMoistureIterations
  , setUiBoundaryMotionTemp
  , setUiBoundaryMotionPrecip
  , setUiPlanetRadius
  , setUiAxialTilt
  , setUiInsolation
  , setUiSliceLatCenter
  , setUiSliceLonCenter
  , setUiPlateSize
  , setUiRiftDepth
  , setUiUplift
  , setUiWeatherAmplitude
  , setUiWeatherPhase
  , setUiWeatherTick
  , setUiWindIterations
  , setUiVegBase
  , setUiVegBoost
  , setUiVegTempWeight
  , setUiVegPrecipWeight
  , setUiHoverHex
  , setUiHoverWidget
  , setUiLapseRate
  , setUiLeftTab
  , setUiPoleTemp
  , setUiRainRate
  , setUiRainShadow
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
  , setUiViewMode
  , setUiWaterLevel
  , setUiWindDiffuse
  , setUiZoom
  )
import Control.Monad (when)
import Data.Int (Int32)
import Data.IORef (IORef, readIORef, writeIORef, modifyIORef')
import Data.List (partition)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.IntMap.Strict as IntMap
import Linear (V2(..))
import qualified SDL
import qualified SDL.Raw.Types as Raw
import System.FilePath ((</>))
import Seer.Draw (seedMaxDigits)
import Seer.Config.Preset (listPresets, loadPreset, presetDir, presetFromUi, savePreset, applyPresetToUi)
import Seer.World.Persist (listWorlds, saveNamedWorld, loadNamedWorld, snapshotToWorld)
import Seer.World.Persist.Types (WorldSaveManifest(..))
import Seer.Input.ConfigScroll
  ( ScrollSettings
  , computeScrollUpdates
  , configRowCount
  , defaultScrollSettings
  )
import Seer.Input.Modal (handleModalTextKey, handleModalTextInput, handleModalListKey)
import Seer.Input.Router (isQuit)
import Seer.Input.Seed (bumpSeed, handleSeedKey, handleSeedTextInput)
import Seer.Input.ViewControls
  ( ZoomSettings
  , applyZoomAtCursor
  , defaultZoomSettings
  , viewModeForKey
  )
import Topo (ChunkCoord(..), ChunkId(..), TileCoord(..), WorldConfig(..), chunkCoordFromTile, chunkIdFromCoord)
import UI.HexPick (screenToAxial)
import UI.Layout
import UI.WidgetTree (Widget(..), WidgetId(..), buildWidgets, buildSliderRowWidgets, hitTest)
import UI.Widgets (Rect(..), containsPoint)
import System.Random (randomIO)
import Hyperspace.Actor (ActorHandle, Protocol, replyTo)
import Actor.AtlasManager (AtlasManager)
import Actor.SnapshotReceiver (SnapshotReceiver)
import Actor.UiActions (UiActions, UiAction(..), UiActionRequest(..), submitUiAction)

data DragState = DragState
  { dsStart :: !(Int, Int)
  , dsLast :: !(Int, Int)
  , dsDragging :: !Bool
  }

-- | Pending tooltip hover: which widget is under the cursor and how
-- many frames remain before the tooltip fires.
type TooltipHover = Maybe (WidgetId, Int)

-- | Number of consecutive frames the cursor must remain still on a
-- slider row before the tooltip appears.
tooltipDelayFrames :: Int
tooltipDelayFrames = 15

handleEvent
  :: SDL.Window
  -> ActorHandle Ui (Protocol Ui)
  -> ActorHandle Log (Protocol Log)
  -> ActorHandle Data (Protocol Data)
  -> ActorHandle Terrain (Protocol Terrain)
  -> ActorHandle AtlasManager (Protocol AtlasManager)
  -> ActorHandle UiActions (Protocol UiActions)
  -> ActorHandle SnapshotReceiver (Protocol SnapshotReceiver)
  -> UiState
  -> LogSnapshot
  -> DataSnapshot
  -> TerrainSnapshot
  -> IORef Bool
  -> IORef Int
  -> IORef (Int, Int)
  -> IORef (Maybe DragState)
  -> IORef TooltipHover
  -> SDL.Event
  -> IO ()
handleEvent window uiHandle logHandle dataHandle terrainHandle atlasManagerHandle uiActionsHandle snapshotReceiverHandle uiSnapCached logSnapCached dataSnapCached terrainSnapCached quitRef lineHeightRef mousePosRef dragRef tooltipHoverRef event = do
  case SDL.eventPayload event of
    SDL.MouseMotionEvent motionEvent -> do
      let SDL.P (V2 mx my) = SDL.mouseMotionEventPos motionEvent
      writeIORef mousePosRef (fromIntegral mx, fromIntegral my)
      dragState <- readIORef dragRef
      case dragState of
        Just state -> do
          uiSnap <- getUiSnapshot uiHandle
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
          (q, r) = screenToAxial 6 (round wx) (round wy)
      if isTerrainHex terrainSnap (q, r)
        then setUiHoverHex uiHandle (Just (q, r))
        else setUiHoverHex uiHandle Nothing
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
                 (terrainRows, climateRows, erosionRows) = buildSliderRowWidgets hoverLayout
                 activeRows = case uiConfigTab uiSnap of
                   ConfigTerrain -> terrainRows
                   ConfigClimate -> climateRows
                   ConfigErosion -> erosionRows
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
        logSnap <- getLogSnapshot logHandle
        uiSnap <- getUiSnapshot uiHandle
        (mx, my) <- readIORef mousePosRef
        (V2 winW winH) <- SDL.get (SDL.windowSize window)
        lineHeight <- readIORef lineHeightRef
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
            dataSnap <- getDataSnapshot dataHandle
            terrainSnap <- getTerrainSnapshot dataHandle
            let hasMissing = tsChunkSize terrainSnap > 0
                  && IntMap.size (tsTerrainChunks terrainSnap) < dsTerrainChunks dataSnap
            when hasMissing $
              submitUiAction uiActionsHandle (actionRequest (UiActionRebuildAtlas (uiViewMode uiSnap)))
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
            _ ->
              handleClick window uiHandle logHandle dataHandle terrainHandle atlasManagerHandle uiActionsHandle snapshotReceiverHandle uiSnapCached logSnapCached dataSnapCached terrainSnapCached quitRef (SDL.mouseButtonEventPos btnEvent)
      | SDL.mouseButtonEventMotion btnEvent == SDL.Released ->
          case SDL.mouseButtonEventButton btnEvent of
            SDL.ButtonRight -> do
              let SDL.P (V2 mx my) = SDL.mouseButtonEventPos btnEvent
              state <- readIORef dragRef
              writeIORef dragRef Nothing
              case state of
                Just DragState { dsDragging = False } -> do
                  uiSnap <- getUiSnapshot uiHandle
                  terrainSnap <- getTerrainSnapshot dataHandle
                  let (wx, wy) = screenToWorld uiSnap (fromIntegral mx, fromIntegral my)
                      (q, r) = screenToAxial 6 (round wx) (round wy)
                  if isTerrainHex terrainSnap (q, r)
                    then setUiContextHex uiHandle (Just (q, r))
                    else setUiContextHex uiHandle Nothing
                  setUiContextPos uiHandle (Just (fromIntegral mx, fromIntegral my))
                _ -> pure ()
            _ -> pure ()
    SDL.TextInputEvent textEvent -> do
      uiSnap <- getUiSnapshot uiHandle
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
            uiSnap <- getUiSnapshot uiHandle
            let keycode = SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent)
            if uiSeedEditing uiSnap
              then handleSeedKey uiHandle (getUiSnapshot uiHandle) keycode
              else case uiMenuMode uiSnap of
                MenuPresetSave -> handlePresetSaveKey uiSnap keycode
                MenuPresetLoad -> handlePresetLoadKey uiSnap keycode
                MenuWorldSave  -> handleWorldSaveKey uiSnap keycode
                MenuWorldLoad  -> handleWorldLoadKey uiSnap keycode
                _ ->
                  case keycode of
                    SDL.KeycodeEscape -> closeContextOrMenu
                    SDL.KeycodeG -> submitUiAction uiActionsHandle (actionRequest UiActionGenerate)
                    SDL.KeycodeC -> toggleConfig
                    SDL.KeycodeUp -> bumpSeed uiHandle (getUiSnapshot uiHandle) 1
                    SDL.KeycodeDown -> bumpSeed uiHandle (getUiSnapshot uiHandle) (-1)
                    SDL.KeycodeL -> do
                      logSnap <- getLogSnapshot logHandle
                      setLogCollapsed logHandle (not (lsCollapsed logSnap))
                    _ ->
                      case viewModeForKey keycode of
                        Just mode -> submitUiAction uiActionsHandle (actionRequest (UiActionSetViewMode mode))
                        Nothing -> pure ()
    _ -> pure ()
  where
    getUiSnapshot :: ActorHandle Ui (Protocol Ui) -> IO UiState
    getUiSnapshot _ = pure uiSnapCached

    getLogSnapshot :: ActorHandle Log (Protocol Log) -> IO LogSnapshot
    getLogSnapshot _ = pure logSnapCached

    getDataSnapshot :: ActorHandle Data (Protocol Data) -> IO DataSnapshot
    getDataSnapshot _ = pure dataSnapCached

    getTerrainSnapshot :: ActorHandle Data (Protocol Data) -> IO TerrainSnapshot
    getTerrainSnapshot _ = pure terrainSnapCached

    dragThreshold :: Float
    dragThreshold = 4
    actionRequest action = UiActionRequest
      { uarAction = action
      , uarUiHandle = uiHandle
      , uarLogHandle = logHandle
      , uarDataHandle = dataHandle
      , uarTerrainHandle = terrainHandle
      , uarAtlasHandle = atlasManagerHandle
      , uarTerrainReplyTo = replyTo @TerrainReplyOps uiActionsHandle
      , uarSnapshotHandle = snapshotReceiverHandle
      }
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
            dir <- presetDir
            let path = dir </> Text.unpack name <> ".json"
            _result <- savePreset path (presetFromUi uiSnap' name)
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
              dir <- presetDir
              let path = dir </> Text.unpack name <> ".json"
              result <- loadPreset path
              case result of
                Right cp -> applyPresetToUi cp uiHandle
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
            when (not (Text.null name)) $ do
              terrainSnap <- getTerrainSnapshot dataHandle
              let world = snapshotToWorld terrainSnap
              _result <- saveNamedWorld name uiSnap' world
              setUiWorldName uiHandle name
              setUiWorldConfig uiHandle (Just (presetFromUi uiSnap' name))
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
                Right (_manifest, preset, world) -> do
                  replaceTerrainData dataHandle world
                  requestDataSnapshot dataHandle (replyTo @DataSnapshotReply snapshotReceiverHandle)
                  applyPresetToUi preset uiHandle
                  setUiWorldName uiHandle name
                  setUiWorldConfig uiHandle (Just preset)
                  submitUiAction uiActionsHandle (actionRequest (UiActionRebuildAtlas (uiViewMode uiSnap)))
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

handleClick
  :: SDL.Window
  -> ActorHandle Ui (Protocol Ui)
  -> ActorHandle Log (Protocol Log)
  -> ActorHandle Data (Protocol Data)
  -> ActorHandle Terrain (Protocol Terrain)
  -> ActorHandle AtlasManager (Protocol AtlasManager)
  -> ActorHandle UiActions (Protocol UiActions)
  -> ActorHandle SnapshotReceiver (Protocol SnapshotReceiver)
  -> UiState
  -> LogSnapshot
  -> DataSnapshot
  -> TerrainSnapshot
  -> IORef Bool
  -> SDL.Point V2 Int32
  -> IO ()
handleClick window uiHandle logHandle dataHandle terrainHandle atlasManagerHandle uiActionsHandle snapshotReceiverHandle uiSnapCached logSnapCached dataSnapCached terrainSnapCached quitRef (SDL.P (V2 x y)) = do
  (V2 winW winH) <- SDL.get (SDL.windowSize window)
  logSnap <- getLogSnapshot logHandle
  uiSnap <- getUiSnapshot uiHandle
  let point = V2 (fromIntegral x) (fromIntegral y)
      logHeight = if lsCollapsed logSnap then 24 else 160
      seedWidth = max 120 (seedMaxDigits * 10)
      layout = layoutForSeed (V2 (fromIntegral winW) (fromIntegral winH)) logHeight seedWidth
      seedValue = seedValueRect layout
      scrollArea = configScrollAreaRect layout
      scrollBar = configScrollBarRect layout
      inScrollBar = uiShowConfig uiSnap && containsPoint scrollBar point
      inConfigScroll = uiShowConfig uiSnap && containsPoint scrollArea point
      scrollPoint = if inConfigScroll
        then V2 (fromIntegral x) (fromIntegral y + uiConfigScroll uiSnap)
        else point
      widgetsAll = buildWidgets layout
      widgets =
        if uiShowConfig uiSnap
          then filter (configWidgetAllowed (uiConfigTab uiSnap)) widgetsAll
          else widgetsAll
      isConfigSliderWidget wid =
        case wid of
          WidgetConfigWaterMinus -> True
          WidgetConfigWaterPlus -> True
          WidgetConfigEvapMinus -> True
          WidgetConfigEvapPlus -> True
          WidgetConfigRainShadowMinus -> True
          WidgetConfigRainShadowPlus -> True
          WidgetConfigWindDiffuseMinus -> True
          WidgetConfigWindDiffusePlus -> True
          WidgetConfigEquatorTempMinus -> True
          WidgetConfigEquatorTempPlus -> True
          WidgetConfigPoleTempMinus -> True
          WidgetConfigPoleTempPlus -> True
          WidgetConfigLapseRateMinus -> True
          WidgetConfigLapseRatePlus -> True
          WidgetConfigWindIterationsMinus -> True
          WidgetConfigWindIterationsPlus -> True
          WidgetConfigMoistureIterationsMinus -> True
          WidgetConfigMoistureIterationsPlus -> True
          WidgetConfigBoundaryMotionTempMinus -> True
          WidgetConfigBoundaryMotionTempPlus -> True
          WidgetConfigBoundaryMotionPrecipMinus -> True
          WidgetConfigBoundaryMotionPrecipPlus -> True
          WidgetConfigPlanetRadiusMinus -> True
          WidgetConfigPlanetRadiusPlus -> True
          WidgetConfigAxialTiltMinus -> True
          WidgetConfigAxialTiltPlus -> True
          WidgetConfigInsolationMinus -> True
          WidgetConfigInsolationPlus -> True
          WidgetConfigSliceLatCenterMinus -> True
          WidgetConfigSliceLatCenterPlus -> True
          WidgetConfigSliceLonCenterMinus -> True
          WidgetConfigSliceLonCenterPlus -> True
          WidgetConfigWeatherTickMinus -> True
          WidgetConfigWeatherTickPlus -> True
          WidgetConfigWeatherPhaseMinus -> True
          WidgetConfigWeatherPhasePlus -> True
          WidgetConfigWeatherAmplitudeMinus -> True
          WidgetConfigWeatherAmplitudePlus -> True
          WidgetConfigVegBaseMinus -> True
          WidgetConfigVegBasePlus -> True
          WidgetConfigVegBoostMinus -> True
          WidgetConfigVegBoostPlus -> True
          WidgetConfigVegTempWeightMinus -> True
          WidgetConfigVegTempWeightPlus -> True
          WidgetConfigVegPrecipWeightMinus -> True
          WidgetConfigVegPrecipWeightPlus -> True
          WidgetConfigGenScaleMinus -> True
          WidgetConfigGenScalePlus -> True
          WidgetConfigGenCoordScaleMinus -> True
          WidgetConfigGenCoordScalePlus -> True
          WidgetConfigGenOffsetXMinus -> True
          WidgetConfigGenOffsetXPlus -> True
          WidgetConfigGenOffsetYMinus -> True
          WidgetConfigGenOffsetYPlus -> True
          WidgetConfigGenFrequencyMinus -> True
          WidgetConfigGenFrequencyPlus -> True
          WidgetConfigGenOctavesMinus -> True
          WidgetConfigGenOctavesPlus -> True
          WidgetConfigGenLacunarityMinus -> True
          WidgetConfigGenLacunarityPlus -> True
          WidgetConfigGenGainMinus -> True
          WidgetConfigGenGainPlus -> True
          WidgetConfigGenWarpScaleMinus -> True
          WidgetConfigGenWarpScalePlus -> True
          WidgetConfigGenWarpStrengthMinus -> True
          WidgetConfigGenWarpStrengthPlus -> True
          WidgetConfigExtentXMinus -> True
          WidgetConfigExtentXPlus -> True
          WidgetConfigExtentYMinus -> True
          WidgetConfigExtentYPlus -> True
          WidgetConfigEdgeNorthMinus -> True
          WidgetConfigEdgeNorthPlus -> True
          WidgetConfigEdgeSouthMinus -> True
          WidgetConfigEdgeSouthPlus -> True
          WidgetConfigEdgeEastMinus -> True
          WidgetConfigEdgeEastPlus -> True
          WidgetConfigEdgeWestMinus -> True
          WidgetConfigEdgeWestPlus -> True
          WidgetConfigEdgeFalloffMinus -> True
          WidgetConfigEdgeFalloffPlus -> True
          WidgetConfigPlateSizeMinus -> True
          WidgetConfigPlateSizePlus -> True
          WidgetConfigUpliftMinus -> True
          WidgetConfigUpliftPlus -> True
          WidgetConfigRiftDepthMinus -> True
          WidgetConfigRiftDepthPlus -> True
          WidgetConfigDetailScaleMinus -> True
          WidgetConfigDetailScalePlus -> True
          WidgetConfigPlateSpeedMinus -> True
          WidgetConfigPlateSpeedPlus -> True
          WidgetConfigBoundarySharpnessMinus -> True
          WidgetConfigBoundarySharpnessPlus -> True
          WidgetConfigBoundaryNoiseScaleMinus -> True
          WidgetConfigBoundaryNoiseScalePlus -> True
          WidgetConfigBoundaryNoiseStrengthMinus -> True
          WidgetConfigBoundaryNoiseStrengthPlus -> True
          WidgetConfigBoundaryWarpOctavesMinus -> True
          WidgetConfigBoundaryWarpOctavesPlus -> True
          WidgetConfigBoundaryWarpLacunarityMinus -> True
          WidgetConfigBoundaryWarpLacunarityPlus -> True
          WidgetConfigBoundaryWarpGainMinus -> True
          WidgetConfigBoundaryWarpGainPlus -> True
          WidgetConfigPlateMergeScaleMinus -> True
          WidgetConfigPlateMergeScalePlus -> True
          WidgetConfigPlateMergeBiasMinus -> True
          WidgetConfigPlateMergeBiasPlus -> True
          WidgetConfigPlateDetailScaleMinus -> True
          WidgetConfigPlateDetailScalePlus -> True
          WidgetConfigPlateDetailStrengthMinus -> True
          WidgetConfigPlateDetailStrengthPlus -> True
          WidgetConfigPlateRidgeStrengthMinus -> True
          WidgetConfigPlateRidgeStrengthPlus -> True
          WidgetConfigPlateHeightBaseMinus -> True
          WidgetConfigPlateHeightBasePlus -> True
          WidgetConfigPlateHeightVarianceMinus -> True
          WidgetConfigPlateHeightVariancePlus -> True
          WidgetConfigPlateHardnessBaseMinus -> True
          WidgetConfigPlateHardnessBasePlus -> True
          WidgetConfigPlateHardnessVarianceMinus -> True
          WidgetConfigPlateHardnessVariancePlus -> True
          WidgetConfigTrenchDepthMinus -> True
          WidgetConfigTrenchDepthPlus -> True
          WidgetConfigRidgeHeightMinus -> True
          WidgetConfigRidgeHeightPlus -> True
          WidgetConfigPlateBiasStrengthMinus -> True
          WidgetConfigPlateBiasStrengthPlus -> True
          WidgetConfigPlateBiasCenterMinus -> True
          WidgetConfigPlateBiasCenterPlus -> True
          WidgetConfigPlateBiasEdgeMinus -> True
          WidgetConfigPlateBiasEdgePlus -> True
          WidgetConfigPlateBiasNorthMinus -> True
          WidgetConfigPlateBiasNorthPlus -> True
          WidgetConfigPlateBiasSouthMinus -> True
          WidgetConfigPlateBiasSouthPlus -> True
          WidgetConfigErosionHydraulicMinus -> True
          WidgetConfigErosionHydraulicPlus -> True
          WidgetConfigErosionThermalMinus -> True
          WidgetConfigErosionThermalPlus -> True
          WidgetConfigErosionRainRateMinus -> True
          WidgetConfigErosionRainRatePlus -> True
          WidgetConfigErosionTalusMinus -> True
          WidgetConfigErosionTalusPlus -> True
          WidgetConfigErosionMaxDropMinus -> True
          WidgetConfigErosionMaxDropPlus -> True
          _ -> False
      (configSliderWidgets, otherWidgets) = partition (isConfigSliderWidget . widgetId) widgets
      hitWidget =
        if inConfigScroll
          then case hitTest configSliderWidgets scrollPoint of
            Just wid -> Just wid
            Nothing -> hitTest otherWidgets point
          else hitTest widgets point
      configWidgetAllowed tab widget =
        case widgetId widget of
          WidgetConfigWaterMinus -> tab == ConfigClimate
          WidgetConfigWaterPlus -> tab == ConfigClimate
          WidgetConfigEvapMinus -> tab == ConfigClimate
          WidgetConfigEvapPlus -> tab == ConfigClimate
          WidgetConfigRainShadowMinus -> tab == ConfigClimate
          WidgetConfigRainShadowPlus -> tab == ConfigClimate
          WidgetConfigWindDiffuseMinus -> tab == ConfigClimate
          WidgetConfigWindDiffusePlus -> tab == ConfigClimate
          WidgetConfigEquatorTempMinus -> tab == ConfigClimate
          WidgetConfigEquatorTempPlus -> tab == ConfigClimate
          WidgetConfigPoleTempMinus -> tab == ConfigClimate
          WidgetConfigPoleTempPlus -> tab == ConfigClimate
          WidgetConfigLapseRateMinus -> tab == ConfigClimate
          WidgetConfigLapseRatePlus -> tab == ConfigClimate
          WidgetConfigWindIterationsMinus -> tab == ConfigClimate
          WidgetConfigWindIterationsPlus -> tab == ConfigClimate
          WidgetConfigMoistureIterationsMinus -> tab == ConfigClimate
          WidgetConfigMoistureIterationsPlus -> tab == ConfigClimate
          WidgetConfigBoundaryMotionTempMinus -> tab == ConfigClimate
          WidgetConfigBoundaryMotionTempPlus -> tab == ConfigClimate
          WidgetConfigBoundaryMotionPrecipMinus -> tab == ConfigClimate
          WidgetConfigBoundaryMotionPrecipPlus -> tab == ConfigClimate
          WidgetConfigPlanetRadiusMinus -> tab == ConfigClimate
          WidgetConfigPlanetRadiusPlus -> tab == ConfigClimate
          WidgetConfigAxialTiltMinus -> tab == ConfigClimate
          WidgetConfigAxialTiltPlus -> tab == ConfigClimate
          WidgetConfigInsolationMinus -> tab == ConfigClimate
          WidgetConfigInsolationPlus -> tab == ConfigClimate
          WidgetConfigSliceLatCenterMinus -> tab == ConfigClimate
          WidgetConfigSliceLatCenterPlus -> tab == ConfigClimate
          WidgetConfigSliceLonCenterMinus -> tab == ConfigClimate
          WidgetConfigSliceLonCenterPlus -> tab == ConfigClimate
          WidgetConfigWeatherTickMinus -> tab == ConfigClimate
          WidgetConfigWeatherTickPlus -> tab == ConfigClimate
          WidgetConfigWeatherPhaseMinus -> tab == ConfigClimate
          WidgetConfigWeatherPhasePlus -> tab == ConfigClimate
          WidgetConfigWeatherAmplitudeMinus -> tab == ConfigClimate
          WidgetConfigWeatherAmplitudePlus -> tab == ConfigClimate
          WidgetConfigVegBaseMinus -> tab == ConfigClimate
          WidgetConfigVegBasePlus -> tab == ConfigClimate
          WidgetConfigVegBoostMinus -> tab == ConfigClimate
          WidgetConfigVegBoostPlus -> tab == ConfigClimate
          WidgetConfigVegTempWeightMinus -> tab == ConfigClimate
          WidgetConfigVegTempWeightPlus -> tab == ConfigClimate
          WidgetConfigVegPrecipWeightMinus -> tab == ConfigClimate
          WidgetConfigVegPrecipWeightPlus -> tab == ConfigClimate
          WidgetConfigGenScaleMinus -> tab == ConfigTerrain
          WidgetConfigGenScalePlus -> tab == ConfigTerrain
          WidgetConfigGenCoordScaleMinus -> tab == ConfigTerrain
          WidgetConfigGenCoordScalePlus -> tab == ConfigTerrain
          WidgetConfigGenOffsetXMinus -> tab == ConfigTerrain
          WidgetConfigGenOffsetXPlus -> tab == ConfigTerrain
          WidgetConfigGenOffsetYMinus -> tab == ConfigTerrain
          WidgetConfigGenOffsetYPlus -> tab == ConfigTerrain
          WidgetConfigGenFrequencyMinus -> tab == ConfigTerrain
          WidgetConfigGenFrequencyPlus -> tab == ConfigTerrain
          WidgetConfigGenOctavesMinus -> tab == ConfigTerrain
          WidgetConfigGenOctavesPlus -> tab == ConfigTerrain
          WidgetConfigGenLacunarityMinus -> tab == ConfigTerrain
          WidgetConfigGenLacunarityPlus -> tab == ConfigTerrain
          WidgetConfigGenGainMinus -> tab == ConfigTerrain
          WidgetConfigGenGainPlus -> tab == ConfigTerrain
          WidgetConfigGenWarpScaleMinus -> tab == ConfigTerrain
          WidgetConfigGenWarpScalePlus -> tab == ConfigTerrain
          WidgetConfigGenWarpStrengthMinus -> tab == ConfigTerrain
          WidgetConfigGenWarpStrengthPlus -> tab == ConfigTerrain
          WidgetConfigExtentXMinus -> tab == ConfigTerrain
          WidgetConfigExtentXPlus -> tab == ConfigTerrain
          WidgetConfigExtentYMinus -> tab == ConfigTerrain
          WidgetConfigExtentYPlus -> tab == ConfigTerrain
          WidgetConfigEdgeNorthMinus -> tab == ConfigTerrain
          WidgetConfigEdgeNorthPlus -> tab == ConfigTerrain
          WidgetConfigEdgeSouthMinus -> tab == ConfigTerrain
          WidgetConfigEdgeSouthPlus -> tab == ConfigTerrain
          WidgetConfigEdgeEastMinus -> tab == ConfigTerrain
          WidgetConfigEdgeEastPlus -> tab == ConfigTerrain
          WidgetConfigEdgeWestMinus -> tab == ConfigTerrain
          WidgetConfigEdgeWestPlus -> tab == ConfigTerrain
          WidgetConfigEdgeFalloffMinus -> tab == ConfigTerrain
          WidgetConfigEdgeFalloffPlus -> tab == ConfigTerrain
          WidgetConfigPlateSizeMinus -> tab == ConfigTerrain
          WidgetConfigPlateSizePlus -> tab == ConfigTerrain
          WidgetConfigUpliftMinus -> tab == ConfigTerrain
          WidgetConfigUpliftPlus -> tab == ConfigTerrain
          WidgetConfigRiftDepthMinus -> tab == ConfigTerrain
          WidgetConfigRiftDepthPlus -> tab == ConfigTerrain
          WidgetConfigDetailScaleMinus -> tab == ConfigTerrain
          WidgetConfigDetailScalePlus -> tab == ConfigTerrain
          WidgetConfigPlateSpeedMinus -> tab == ConfigTerrain
          WidgetConfigPlateSpeedPlus -> tab == ConfigTerrain
          WidgetConfigBoundarySharpnessMinus -> tab == ConfigTerrain
          WidgetConfigBoundarySharpnessPlus -> tab == ConfigTerrain
          WidgetConfigBoundaryNoiseScaleMinus -> tab == ConfigTerrain
          WidgetConfigBoundaryNoiseScalePlus -> tab == ConfigTerrain
          WidgetConfigBoundaryNoiseStrengthMinus -> tab == ConfigTerrain
          WidgetConfigBoundaryNoiseStrengthPlus -> tab == ConfigTerrain
          WidgetConfigBoundaryWarpOctavesMinus -> tab == ConfigTerrain
          WidgetConfigBoundaryWarpOctavesPlus -> tab == ConfigTerrain
          WidgetConfigBoundaryWarpLacunarityMinus -> tab == ConfigTerrain
          WidgetConfigBoundaryWarpLacunarityPlus -> tab == ConfigTerrain
          WidgetConfigBoundaryWarpGainMinus -> tab == ConfigTerrain
          WidgetConfigBoundaryWarpGainPlus -> tab == ConfigTerrain
          WidgetConfigPlateMergeScaleMinus -> tab == ConfigTerrain
          WidgetConfigPlateMergeScalePlus -> tab == ConfigTerrain
          WidgetConfigPlateMergeBiasMinus -> tab == ConfigTerrain
          WidgetConfigPlateMergeBiasPlus -> tab == ConfigTerrain
          WidgetConfigPlateDetailScaleMinus -> tab == ConfigTerrain
          WidgetConfigPlateDetailScalePlus -> tab == ConfigTerrain
          WidgetConfigPlateDetailStrengthMinus -> tab == ConfigTerrain
          WidgetConfigPlateDetailStrengthPlus -> tab == ConfigTerrain
          WidgetConfigPlateRidgeStrengthMinus -> tab == ConfigTerrain
          WidgetConfigPlateRidgeStrengthPlus -> tab == ConfigTerrain
          WidgetConfigPlateHeightBaseMinus -> tab == ConfigTerrain
          WidgetConfigPlateHeightBasePlus -> tab == ConfigTerrain
          WidgetConfigPlateHeightVarianceMinus -> tab == ConfigTerrain
          WidgetConfigPlateHeightVariancePlus -> tab == ConfigTerrain
          WidgetConfigPlateHardnessBaseMinus -> tab == ConfigTerrain
          WidgetConfigPlateHardnessBasePlus -> tab == ConfigTerrain
          WidgetConfigPlateHardnessVarianceMinus -> tab == ConfigTerrain
          WidgetConfigPlateHardnessVariancePlus -> tab == ConfigTerrain
          WidgetConfigTrenchDepthMinus -> tab == ConfigTerrain
          WidgetConfigTrenchDepthPlus -> tab == ConfigTerrain
          WidgetConfigRidgeHeightMinus -> tab == ConfigTerrain
          WidgetConfigRidgeHeightPlus -> tab == ConfigTerrain
          WidgetConfigPlateBiasStrengthMinus -> tab == ConfigTerrain
          WidgetConfigPlateBiasStrengthPlus -> tab == ConfigTerrain
          WidgetConfigPlateBiasCenterMinus -> tab == ConfigTerrain
          WidgetConfigPlateBiasCenterPlus -> tab == ConfigTerrain
          WidgetConfigPlateBiasEdgeMinus -> tab == ConfigTerrain
          WidgetConfigPlateBiasEdgePlus -> tab == ConfigTerrain
          WidgetConfigPlateBiasNorthMinus -> tab == ConfigTerrain
          WidgetConfigPlateBiasNorthPlus -> tab == ConfigTerrain
          WidgetConfigPlateBiasSouthMinus -> tab == ConfigTerrain
          WidgetConfigPlateBiasSouthPlus -> tab == ConfigTerrain
          WidgetConfigErosionHydraulicMinus -> tab == ConfigErosion
          WidgetConfigErosionHydraulicPlus -> tab == ConfigErosion
          WidgetConfigErosionThermalMinus -> tab == ConfigErosion
          WidgetConfigErosionThermalPlus -> tab == ConfigErosion
          WidgetConfigErosionRainRateMinus -> tab == ConfigErosion
          WidgetConfigErosionRainRatePlus -> tab == ConfigErosion
          WidgetConfigErosionTalusMinus -> tab == ConfigErosion
          WidgetConfigErosionTalusPlus -> tab == ConfigErosion
          WidgetConfigErosionMaxDropMinus -> tab == ConfigErosion
          WidgetConfigErosionMaxDropPlus -> tab == ConfigErosion
          _ -> True
  if inScrollBar
    then do
      let rowHeight = 24
          gap = 10
          rows = configRowCount (uiConfigTab uiSnap)
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
          ; Just WidgetConfigTabClimate -> whenConfigVisible (setUiConfigTab uiHandle ConfigClimate >> setUiConfigScroll uiHandle 0)
          ; Just WidgetConfigTabErosion -> whenConfigVisible (setUiConfigTab uiHandle ConfigErosion >> setUiConfigScroll uiHandle 0)
          ; Just WidgetConfigPresetSave -> whenConfigVisible (openPresetSaveDialog layout uiSnap)
          ; Just WidgetConfigPresetLoad -> whenConfigVisible openPresetLoadDialog
          ; Just WidgetConfigReset -> whenConfigVisible (SDL.stopTextInput >> submit UiActionReset)
          ; Just WidgetConfigRevert -> whenConfigVisible (submit UiActionRevert)
          ; Just WidgetConfigWaterMinus -> whenConfigVisible (bumpWater (-0.05))
          ; Just WidgetConfigWaterPlus -> whenConfigVisible (bumpWater 0.05)
          ; Just WidgetConfigEvapMinus -> whenConfigVisible (bumpEvap (-0.05))
          ; Just WidgetConfigEvapPlus -> whenConfigVisible (bumpEvap 0.05)
          ; Just WidgetConfigRainShadowMinus -> whenConfigVisible (bumpRainShadow (-0.05))
          ; Just WidgetConfigRainShadowPlus -> whenConfigVisible (bumpRainShadow 0.05)
          ; Just WidgetConfigWindDiffuseMinus -> whenConfigVisible (bumpWindDiffuse (-0.05))
          ; Just WidgetConfigWindDiffusePlus -> whenConfigVisible (bumpWindDiffuse 0.05)
          ; Just WidgetConfigEquatorTempMinus -> whenConfigVisible (bumpEquatorTemp (-0.05))
          ; Just WidgetConfigEquatorTempPlus -> whenConfigVisible (bumpEquatorTemp 0.05)
          ; Just WidgetConfigPoleTempMinus -> whenConfigVisible (bumpPoleTemp (-0.05))
          ; Just WidgetConfigPoleTempPlus -> whenConfigVisible (bumpPoleTemp 0.05)
          ; Just WidgetConfigLapseRateMinus -> whenConfigVisible (bumpLapseRate (-0.05))
          ; Just WidgetConfigLapseRatePlus -> whenConfigVisible (bumpLapseRate 0.05)
          ; Just WidgetConfigGenScaleMinus -> whenConfigVisible (bumpGenScale (-0.05))
          ; Just WidgetConfigGenScalePlus -> whenConfigVisible (bumpGenScale 0.05)
          ; Just WidgetConfigGenCoordScaleMinus -> whenConfigVisible (bumpGenCoordScale (-0.05))
          ; Just WidgetConfigGenCoordScalePlus -> whenConfigVisible (bumpGenCoordScale 0.05)
          ; Just WidgetConfigGenOffsetXMinus -> whenConfigVisible (bumpGenOffsetX (-0.05))
          ; Just WidgetConfigGenOffsetXPlus -> whenConfigVisible (bumpGenOffsetX 0.05)
          ; Just WidgetConfigGenOffsetYMinus -> whenConfigVisible (bumpGenOffsetY (-0.05))
          ; Just WidgetConfigGenOffsetYPlus -> whenConfigVisible (bumpGenOffsetY 0.05)
          ; Just WidgetConfigGenFrequencyMinus -> whenConfigVisible (bumpGenFrequency (-0.05))
          ; Just WidgetConfigGenFrequencyPlus -> whenConfigVisible (bumpGenFrequency 0.05)
          ; Just WidgetConfigGenOctavesMinus -> whenConfigVisible (bumpGenOctaves (-0.05))
          ; Just WidgetConfigGenOctavesPlus -> whenConfigVisible (bumpGenOctaves 0.05)
          ; Just WidgetConfigGenLacunarityMinus -> whenConfigVisible (bumpGenLacunarity (-0.05))
          ; Just WidgetConfigGenLacunarityPlus -> whenConfigVisible (bumpGenLacunarity 0.05)
          ; Just WidgetConfigGenGainMinus -> whenConfigVisible (bumpGenGain (-0.05))
          ; Just WidgetConfigGenGainPlus -> whenConfigVisible (bumpGenGain 0.05)
          ; Just WidgetConfigGenWarpScaleMinus -> whenConfigVisible (bumpGenWarpScale (-0.05))
          ; Just WidgetConfigGenWarpScalePlus -> whenConfigVisible (bumpGenWarpScale 0.05)
          ; Just WidgetConfigGenWarpStrengthMinus -> whenConfigVisible (bumpGenWarpStrength (-0.05))
          ; Just WidgetConfigGenWarpStrengthPlus -> whenConfigVisible (bumpGenWarpStrength 0.05)
          ; Just WidgetConfigExtentXMinus -> whenConfigVisible (bumpWorldExtentX (-extentStep))
          ; Just WidgetConfigExtentXPlus -> whenConfigVisible (bumpWorldExtentX extentStep)
          ; Just WidgetConfigExtentYMinus -> whenConfigVisible (bumpWorldExtentY (-extentStep))
          ; Just WidgetConfigExtentYPlus -> whenConfigVisible (bumpWorldExtentY extentStep)
          ; Just WidgetConfigEdgeNorthMinus -> whenConfigVisible (bumpEdgeNorth (-0.05))
          ; Just WidgetConfigEdgeNorthPlus -> whenConfigVisible (bumpEdgeNorth 0.05)
          ; Just WidgetConfigEdgeSouthMinus -> whenConfigVisible (bumpEdgeSouth (-0.05))
          ; Just WidgetConfigEdgeSouthPlus -> whenConfigVisible (bumpEdgeSouth 0.05)
          ; Just WidgetConfigEdgeEastMinus -> whenConfigVisible (bumpEdgeEast (-0.05))
          ; Just WidgetConfigEdgeEastPlus -> whenConfigVisible (bumpEdgeEast 0.05)
          ; Just WidgetConfigEdgeWestMinus -> whenConfigVisible (bumpEdgeWest (-0.05))
          ; Just WidgetConfigEdgeWestPlus -> whenConfigVisible (bumpEdgeWest 0.05)
          ; Just WidgetConfigEdgeFalloffMinus -> whenConfigVisible (bumpEdgeFalloff (-0.01))
          ; Just WidgetConfigEdgeFalloffPlus -> whenConfigVisible (bumpEdgeFalloff 0.01)
          ; Just WidgetConfigPlateSizeMinus -> whenConfigVisible (bumpPlateSize (-0.05))
          ; Just WidgetConfigPlateSizePlus -> whenConfigVisible (bumpPlateSize 0.05)
          ; Just WidgetConfigUpliftMinus -> whenConfigVisible (bumpUplift (-0.05))
          ; Just WidgetConfigUpliftPlus -> whenConfigVisible (bumpUplift 0.05)
          ; Just WidgetConfigRiftDepthMinus -> whenConfigVisible (bumpRiftDepth (-0.05))
          ; Just WidgetConfigRiftDepthPlus -> whenConfigVisible (bumpRiftDepth 0.05)
          ; Just WidgetConfigDetailScaleMinus -> whenConfigVisible (bumpDetailScale (-0.05))
          ; Just WidgetConfigDetailScalePlus -> whenConfigVisible (bumpDetailScale 0.05)
          ; Just WidgetConfigPlateSpeedMinus -> whenConfigVisible (bumpPlateSpeed (-0.05))
          ; Just WidgetConfigPlateSpeedPlus -> whenConfigVisible (bumpPlateSpeed 0.05)
          ; Just WidgetConfigBoundarySharpnessMinus -> whenConfigVisible (bumpBoundarySharpness (-0.05))
          ; Just WidgetConfigBoundarySharpnessPlus -> whenConfigVisible (bumpBoundarySharpness 0.05)
          ; Just WidgetConfigBoundaryNoiseScaleMinus -> whenConfigVisible (bumpBoundaryNoiseScale (-0.05))
          ; Just WidgetConfigBoundaryNoiseScalePlus -> whenConfigVisible (bumpBoundaryNoiseScale 0.05)
          ; Just WidgetConfigBoundaryNoiseStrengthMinus -> whenConfigVisible (bumpBoundaryNoiseStrength (-0.05))
          ; Just WidgetConfigBoundaryNoiseStrengthPlus -> whenConfigVisible (bumpBoundaryNoiseStrength 0.05)
          ; Just WidgetConfigBoundaryWarpOctavesMinus -> whenConfigVisible (bumpBoundaryWarpOctaves (-0.1))
          ; Just WidgetConfigBoundaryWarpOctavesPlus -> whenConfigVisible (bumpBoundaryWarpOctaves 0.1)
          ; Just WidgetConfigBoundaryWarpLacunarityMinus -> whenConfigVisible (bumpBoundaryWarpLacunarity (-0.05))
          ; Just WidgetConfigBoundaryWarpLacunarityPlus -> whenConfigVisible (bumpBoundaryWarpLacunarity 0.05)
          ; Just WidgetConfigBoundaryWarpGainMinus -> whenConfigVisible (bumpBoundaryWarpGain (-0.05))
          ; Just WidgetConfigBoundaryWarpGainPlus -> whenConfigVisible (bumpBoundaryWarpGain 0.05)
          ; Just WidgetConfigPlateMergeScaleMinus -> whenConfigVisible (bumpPlateMergeScale (-0.05))
          ; Just WidgetConfigPlateMergeScalePlus -> whenConfigVisible (bumpPlateMergeScale 0.05)
          ; Just WidgetConfigPlateMergeBiasMinus -> whenConfigVisible (bumpPlateMergeBias (-0.05))
          ; Just WidgetConfigPlateMergeBiasPlus -> whenConfigVisible (bumpPlateMergeBias 0.05)
          ; Just WidgetConfigPlateDetailScaleMinus -> whenConfigVisible (bumpPlateDetailScale (-0.05))
          ; Just WidgetConfigPlateDetailScalePlus -> whenConfigVisible (bumpPlateDetailScale 0.05)
          ; Just WidgetConfigPlateDetailStrengthMinus -> whenConfigVisible (bumpPlateDetailStrength (-0.05))
          ; Just WidgetConfigPlateDetailStrengthPlus -> whenConfigVisible (bumpPlateDetailStrength 0.05)
          ; Just WidgetConfigPlateRidgeStrengthMinus -> whenConfigVisible (bumpPlateRidgeStrength (-0.05))
          ; Just WidgetConfigPlateRidgeStrengthPlus -> whenConfigVisible (bumpPlateRidgeStrength 0.05)
          ; Just WidgetConfigPlateHeightBaseMinus -> whenConfigVisible (bumpPlateHeightBase (-0.05))
          ; Just WidgetConfigPlateHeightBasePlus -> whenConfigVisible (bumpPlateHeightBase 0.05)
          ; Just WidgetConfigPlateHeightVarianceMinus -> whenConfigVisible (bumpPlateHeightVariance (-0.05))
          ; Just WidgetConfigPlateHeightVariancePlus -> whenConfigVisible (bumpPlateHeightVariance 0.05)
          ; Just WidgetConfigPlateHardnessBaseMinus -> whenConfigVisible (bumpPlateHardnessBase (-0.05))
          ; Just WidgetConfigPlateHardnessBasePlus -> whenConfigVisible (bumpPlateHardnessBase 0.05)
          ; Just WidgetConfigPlateHardnessVarianceMinus -> whenConfigVisible (bumpPlateHardnessVariance (-0.05))
          ; Just WidgetConfigPlateHardnessVariancePlus -> whenConfigVisible (bumpPlateHardnessVariance 0.05)
          ; Just WidgetConfigTrenchDepthMinus -> whenConfigVisible (bumpTrenchDepth (-0.05))
          ; Just WidgetConfigTrenchDepthPlus -> whenConfigVisible (bumpTrenchDepth 0.05)
          ; Just WidgetConfigRidgeHeightMinus -> whenConfigVisible (bumpRidgeHeight (-0.05))
          ; Just WidgetConfigRidgeHeightPlus -> whenConfigVisible (bumpRidgeHeight 0.05)
          ; Just WidgetConfigPlateBiasStrengthMinus -> whenConfigVisible (bumpPlateBiasStrength (-0.05))
          ; Just WidgetConfigPlateBiasStrengthPlus -> whenConfigVisible (bumpPlateBiasStrength 0.05)
          ; Just WidgetConfigPlateBiasCenterMinus -> whenConfigVisible (bumpPlateBiasCenter (-0.05))
          ; Just WidgetConfigPlateBiasCenterPlus -> whenConfigVisible (bumpPlateBiasCenter 0.05)
          ; Just WidgetConfigPlateBiasEdgeMinus -> whenConfigVisible (bumpPlateBiasEdge (-0.05))
          ; Just WidgetConfigPlateBiasEdgePlus -> whenConfigVisible (bumpPlateBiasEdge 0.05)
          ; Just WidgetConfigPlateBiasNorthMinus -> whenConfigVisible (bumpPlateBiasNorth (-0.05))
          ; Just WidgetConfigPlateBiasNorthPlus -> whenConfigVisible (bumpPlateBiasNorth 0.05)
          ; Just WidgetConfigPlateBiasSouthMinus -> whenConfigVisible (bumpPlateBiasSouth (-0.05))
          ; Just WidgetConfigPlateBiasSouthPlus -> whenConfigVisible (bumpPlateBiasSouth 0.05)
          ; Just WidgetConfigErosionHydraulicMinus -> whenConfigVisible (bumpErosionHydraulic (-0.05))
          ; Just WidgetConfigErosionHydraulicPlus -> whenConfigVisible (bumpErosionHydraulic 0.05)
          ; Just WidgetConfigErosionThermalMinus -> whenConfigVisible (bumpErosionThermal (-0.05))
          ; Just WidgetConfigErosionThermalPlus -> whenConfigVisible (bumpErosionThermal 0.05)
          ; Just WidgetConfigErosionRainRateMinus -> whenConfigVisible (bumpRainRate (-0.05))
          ; Just WidgetConfigErosionRainRatePlus -> whenConfigVisible (bumpRainRate 0.05)
          ; Just WidgetConfigErosionTalusMinus -> whenConfigVisible (bumpErosionTalus (-0.05))
          ; Just WidgetConfigErosionTalusPlus -> whenConfigVisible (bumpErosionTalus 0.05)
          ; Just WidgetConfigErosionMaxDropMinus -> whenConfigVisible (bumpErosionMaxDrop (-0.05))
          ; Just WidgetConfigErosionMaxDropPlus -> whenConfigVisible (bumpErosionMaxDrop 0.05)
          ; Just WidgetConfigWindIterationsMinus -> whenConfigVisible (bumpWindIterations (-0.1))
          ; Just WidgetConfigWindIterationsPlus -> whenConfigVisible (bumpWindIterations 0.1)
          ; Just WidgetConfigMoistureIterationsMinus -> whenConfigVisible (bumpMoistureIterations (-0.1))
          ; Just WidgetConfigMoistureIterationsPlus -> whenConfigVisible (bumpMoistureIterations 0.1)
          ; Just WidgetConfigBoundaryMotionTempMinus -> whenConfigVisible (bumpBoundaryMotionTemp (-0.05))
          ; Just WidgetConfigBoundaryMotionTempPlus -> whenConfigVisible (bumpBoundaryMotionTemp 0.05)
          ; Just WidgetConfigBoundaryMotionPrecipMinus -> whenConfigVisible (bumpBoundaryMotionPrecip (-0.05))
          ; Just WidgetConfigBoundaryMotionPrecipPlus -> whenConfigVisible (bumpBoundaryMotionPrecip 0.05)
          ; Just WidgetConfigPlanetRadiusMinus -> whenConfigVisible (bumpPlanetRadius (-0.05))
          ; Just WidgetConfigPlanetRadiusPlus -> whenConfigVisible (bumpPlanetRadius 0.05)
          ; Just WidgetConfigAxialTiltMinus -> whenConfigVisible (bumpAxialTilt (-0.05))
          ; Just WidgetConfigAxialTiltPlus -> whenConfigVisible (bumpAxialTilt 0.05)
          ; Just WidgetConfigInsolationMinus -> whenConfigVisible (bumpInsolation (-0.05))
          ; Just WidgetConfigInsolationPlus -> whenConfigVisible (bumpInsolation 0.05)
          ; Just WidgetConfigSliceLatCenterMinus -> whenConfigVisible (bumpSliceLatCenter (-0.05))
          ; Just WidgetConfigSliceLatCenterPlus -> whenConfigVisible (bumpSliceLatCenter 0.05)
          ; Just WidgetConfigSliceLonCenterMinus -> whenConfigVisible (bumpSliceLonCenter (-0.05))
          ; Just WidgetConfigSliceLonCenterPlus -> whenConfigVisible (bumpSliceLonCenter 0.05)
          ; Just WidgetConfigWeatherTickMinus -> whenConfigVisible (bumpWeatherTick (-0.05))
          ; Just WidgetConfigWeatherTickPlus -> whenConfigVisible (bumpWeatherTick 0.05)
          ; Just WidgetConfigWeatherPhaseMinus -> whenConfigVisible (bumpWeatherPhase (-0.05))
          ; Just WidgetConfigWeatherPhasePlus -> whenConfigVisible (bumpWeatherPhase 0.05)
          ; Just WidgetConfigWeatherAmplitudeMinus -> whenConfigVisible (bumpWeatherAmplitude (-0.05))
          ; Just WidgetConfigWeatherAmplitudePlus -> whenConfigVisible (bumpWeatherAmplitude 0.05)
          ; Just WidgetConfigVegBaseMinus -> whenConfigVisible (bumpVegBase (-0.05))
          ; Just WidgetConfigVegBasePlus -> whenConfigVisible (bumpVegBase 0.05)
          ; Just WidgetConfigVegBoostMinus -> whenConfigVisible (bumpVegBoost (-0.05))
          ; Just WidgetConfigVegBoostPlus -> whenConfigVisible (bumpVegBoost 0.05)
          ; Just WidgetConfigVegTempWeightMinus -> whenConfigVisible (bumpVegTempWeight (-0.05))
          ; Just WidgetConfigVegTempWeightPlus -> whenConfigVisible (bumpVegTempWeight 0.05)
          ; Just WidgetConfigVegPrecipWeightMinus -> whenConfigVisible (bumpVegPrecipWeight (-0.05))
          ; Just WidgetConfigVegPrecipWeightPlus -> whenConfigVisible (bumpVegPrecipWeight 0.05)
          ; Just WidgetViewElevation -> whenLeftView (submit (UiActionSetViewMode ViewElevation))
          ; Just WidgetViewBiome -> whenLeftView (submit (UiActionSetViewMode ViewBiome))
          ; Just WidgetViewClimate -> whenLeftView (submit (UiActionSetViewMode ViewClimate))
          ; Just WidgetViewMoisture -> whenLeftView (submit (UiActionSetViewMode ViewMoisture))
          ; Just WidgetViewPrecip -> whenLeftView (submit (UiActionSetViewMode ViewPrecip))
          ; Just WidgetViewPlateId -> whenLeftView (submit (UiActionSetViewMode ViewPlateId))
          ; Just WidgetViewPlateBoundary -> whenLeftView (submit (UiActionSetViewMode ViewPlateBoundary))
          ; Just WidgetViewPlateHardness -> whenLeftView (submit (UiActionSetViewMode ViewPlateHardness))
          ; Just WidgetViewPlateCrust -> whenLeftView (submit (UiActionSetViewMode ViewPlateCrust))
          ; Just WidgetViewPlateAge -> whenLeftView (submit (UiActionSetViewMode ViewPlateAge))
          ; Just WidgetViewPlateHeight -> whenLeftView (submit (UiActionSetViewMode ViewPlateHeight))
          ; Just WidgetViewPlateVelocity -> whenLeftView (submit (UiActionSetViewMode ViewPlateVelocity))
          ; Just WidgetLogDebug -> setLogMinLevel logHandle LogDebug
          ; Just WidgetLogInfo -> setLogMinLevel logHandle LogInfo
          ; Just WidgetLogWarn -> setLogMinLevel logHandle LogWarn
          ; Just WidgetLogError -> setLogMinLevel logHandle LogError
          ; Just WidgetLogHeader -> toggleLog
          ; Just _ -> pure ()
          ; Nothing -> pure ()
          }
  where
    getUiSnapshot :: ActorHandle Ui (Protocol Ui) -> IO UiState
    getUiSnapshot _ = pure uiSnapCached

    getLogSnapshot :: ActorHandle Log (Protocol Log) -> IO LogSnapshot
    getLogSnapshot _ = pure logSnapCached

    getDataSnapshot :: ActorHandle Data (Protocol Data) -> IO DataSnapshot
    getDataSnapshot _ = pure dataSnapCached

    getTerrainSnapshot :: ActorHandle Data (Protocol Data) -> IO TerrainSnapshot
    getTerrainSnapshot _ = pure terrainSnapCached

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
      names <- listPresets
      setUiPresetList uiHandle names
      setUiPresetSelected uiHandle 0
      setUiMenuMode uiHandle MenuPresetLoad

    cancelPresetDialog = do
      setUiMenuMode uiHandle MenuNone
      SDL.stopTextInput

    confirmPresetSave uiSnap = do
      let name = uiPresetInput uiSnap
      dir <- presetDir
      let path = dir </> Text.unpack name <> ".json"
      _result <- savePreset path (presetFromUi uiSnap name)
      setUiMenuMode uiHandle MenuNone
      SDL.stopTextInput

    confirmPresetLoad uiSnap = do
      let names = uiPresetList uiSnap
          sel = uiPresetSelected uiSnap
      when (sel >= 0 && sel < length names) $ do
        let name = names !! sel
        dir <- presetDir
        let path = dir </> Text.unpack name <> ".json"
        result <- loadPreset path
        case result of
          Right cp -> applyPresetToUi cp uiHandle
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
        setUiWorldConfig uiHandle (Just (presetFromUi uiSnap name))
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
          Right (_manifest, preset, world) -> do
            replaceTerrainData dataHandle world
            requestDataSnapshot dataHandle (replyTo @DataSnapshotReply snapshotReceiverHandle)
            applyPresetToUi preset uiHandle
            setUiWorldName uiHandle name
            setUiWorldConfig uiHandle (Just preset)
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
      uiSnap <- getUiSnapshot uiHandle
      when (uiShowConfig uiSnap) action
    toggleConfig = do
      uiSnap <- getUiSnapshot uiHandle
      setUiShowConfig uiHandle (not (uiShowConfig uiSnap))
    actionRequest action = UiActionRequest
      { uarAction = action
      , uarUiHandle = uiHandle
      , uarLogHandle = logHandle
      , uarDataHandle = dataHandle
      , uarTerrainHandle = terrainHandle
      , uarAtlasHandle = atlasManagerHandle
      , uarTerrainReplyTo = replyTo @TerrainReplyOps uiActionsHandle
      , uarSnapshotHandle = snapshotReceiverHandle
      }
    submit action =
      submitUiAction uiActionsHandle (actionRequest action)
    startSeedEdit rect = do
      uiSnap <- getUiSnapshot uiHandle
      let Rect (V2 rx ry, V2 rw rh) = rect
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
    extentStep = 1 / 16
    bumpWater delta = do
      uiSnap <- getUiSnapshot uiHandle
      let next = uiWaterLevel uiSnap + delta
      setUiWaterLevel uiHandle next
    bumpEvap delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiEvaporation uiHandle (uiEvaporation uiSnap + delta)
    bumpRainShadow delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiRainShadow uiHandle (uiRainShadow uiSnap + delta)
    bumpWindDiffuse delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiWindDiffuse uiHandle (uiWindDiffuse uiSnap + delta)
    bumpRainRate delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiRainRate uiHandle (uiRainRate uiSnap + delta)
    bumpEquatorTemp delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiEquatorTemp uiHandle (uiEquatorTemp uiSnap + delta)
    bumpPoleTemp delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiPoleTemp uiHandle (uiPoleTemp uiSnap + delta)
    bumpLapseRate delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiLapseRate uiHandle (uiLapseRate uiSnap + delta)
    bumpGenScale delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiGenScale uiHandle (uiGenScale uiSnap + delta)
    bumpGenCoordScale delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiGenCoordScale uiHandle (uiGenCoordScale uiSnap + delta)
    bumpGenOffsetX delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiGenOffsetX uiHandle (uiGenOffsetX uiSnap + delta)
    bumpGenOffsetY delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiGenOffsetY uiHandle (uiGenOffsetY uiSnap + delta)
    bumpGenFrequency delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiGenFrequency uiHandle (uiGenFrequency uiSnap + delta)
    bumpGenOctaves delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiGenOctaves uiHandle (uiGenOctaves uiSnap + delta)
    bumpGenLacunarity delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiGenLacunarity uiHandle (uiGenLacunarity uiSnap + delta)
    bumpGenGain delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiGenGain uiHandle (uiGenGain uiSnap + delta)
    bumpGenWarpScale delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiGenWarpScale uiHandle (uiGenWarpScale uiSnap + delta)
    bumpGenWarpStrength delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiGenWarpStrength uiHandle (uiGenWarpStrength uiSnap + delta)
    bumpWorldExtentX delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiWorldExtentX uiHandle (uiWorldExtentX uiSnap + delta)
    bumpWorldExtentY delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiWorldExtentY uiHandle (uiWorldExtentY uiSnap + delta)
    bumpEdgeNorth delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiEdgeDepthNorth uiHandle (uiEdgeDepthNorth uiSnap + delta)
    bumpEdgeSouth delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiEdgeDepthSouth uiHandle (uiEdgeDepthSouth uiSnap + delta)
    bumpEdgeEast delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiEdgeDepthEast uiHandle (uiEdgeDepthEast uiSnap + delta)
    bumpEdgeWest delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiEdgeDepthWest uiHandle (uiEdgeDepthWest uiSnap + delta)
    bumpEdgeFalloff delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiEdgeDepthFalloff uiHandle (uiEdgeDepthFalloff uiSnap + delta)
    bumpPlateSize delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiPlateSize uiHandle (uiPlateSize uiSnap + delta)
    bumpUplift delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiUplift uiHandle (uiUplift uiSnap + delta)
    bumpRiftDepth delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiRiftDepth uiHandle (uiRiftDepth uiSnap + delta)
    bumpDetailScale delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiDetailScale uiHandle (uiDetailScale uiSnap + delta)
    bumpPlateSpeed delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiPlateSpeed uiHandle (uiPlateSpeed uiSnap + delta)
    bumpBoundarySharpness delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiBoundarySharpness uiHandle (uiBoundarySharpness uiSnap + delta)
    bumpBoundaryNoiseScale delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiBoundaryNoiseScale uiHandle (uiBoundaryNoiseScale uiSnap + delta)
    bumpBoundaryNoiseStrength delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiBoundaryNoiseStrength uiHandle (uiBoundaryNoiseStrength uiSnap + delta)
    bumpBoundaryWarpOctaves delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiBoundaryWarpOctaves uiHandle (uiBoundaryWarpOctaves uiSnap + delta)
    bumpBoundaryWarpLacunarity delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiBoundaryWarpLacunarity uiHandle (uiBoundaryWarpLacunarity uiSnap + delta)
    bumpBoundaryWarpGain delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiBoundaryWarpGain uiHandle (uiBoundaryWarpGain uiSnap + delta)
    bumpPlateMergeScale delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiPlateMergeScale uiHandle (uiPlateMergeScale uiSnap + delta)
    bumpPlateMergeBias delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiPlateMergeBias uiHandle (uiPlateMergeBias uiSnap + delta)
    bumpPlateDetailScale delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiPlateDetailScale uiHandle (uiPlateDetailScale uiSnap + delta)
    bumpPlateDetailStrength delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiPlateDetailStrength uiHandle (uiPlateDetailStrength uiSnap + delta)
    bumpPlateRidgeStrength delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiPlateRidgeStrength uiHandle (uiPlateRidgeStrength uiSnap + delta)
    bumpPlateHeightBase delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiPlateHeightBase uiHandle (uiPlateHeightBase uiSnap + delta)
    bumpPlateHeightVariance delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiPlateHeightVariance uiHandle (uiPlateHeightVariance uiSnap + delta)
    bumpPlateHardnessBase delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiPlateHardnessBase uiHandle (uiPlateHardnessBase uiSnap + delta)
    bumpPlateHardnessVariance delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiPlateHardnessVariance uiHandle (uiPlateHardnessVariance uiSnap + delta)
    bumpTrenchDepth delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiTrenchDepth uiHandle (uiTrenchDepth uiSnap + delta)
    bumpRidgeHeight delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiRidgeHeight uiHandle (uiRidgeHeight uiSnap + delta)
    bumpPlateBiasStrength delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiPlateBiasStrength uiHandle (uiPlateBiasStrength uiSnap + delta)
    bumpPlateBiasCenter delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiPlateBiasCenter uiHandle (uiPlateBiasCenter uiSnap + delta)
    bumpPlateBiasEdge delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiPlateBiasEdge uiHandle (uiPlateBiasEdge uiSnap + delta)
    bumpPlateBiasNorth delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiPlateBiasNorth uiHandle (uiPlateBiasNorth uiSnap + delta)
    bumpPlateBiasSouth delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiPlateBiasSouth uiHandle (uiPlateBiasSouth uiSnap + delta)
    bumpWindIterations delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiWindIterations uiHandle (uiWindIterations uiSnap + delta)
    bumpMoistureIterations delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiMoistureIterations uiHandle (uiMoistureIterations uiSnap + delta)
    bumpBoundaryMotionTemp delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiBoundaryMotionTemp uiHandle (uiBoundaryMotionTemp uiSnap + delta)
    bumpBoundaryMotionPrecip delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiBoundaryMotionPrecip uiHandle (uiBoundaryMotionPrecip uiSnap + delta)
    bumpPlanetRadius delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiPlanetRadius uiHandle (uiPlanetRadius uiSnap + delta)
    bumpAxialTilt delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiAxialTilt uiHandle (uiAxialTilt uiSnap + delta)
    bumpInsolation delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiInsolation uiHandle (uiInsolation uiSnap + delta)
    bumpSliceLatCenter delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiSliceLatCenter uiHandle (uiSliceLatCenter uiSnap + delta)
    bumpSliceLonCenter delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiSliceLonCenter uiHandle (uiSliceLonCenter uiSnap + delta)
    bumpWeatherTick delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiWeatherTick uiHandle (uiWeatherTick uiSnap + delta)
    bumpWeatherPhase delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiWeatherPhase uiHandle (uiWeatherPhase uiSnap + delta)
    bumpWeatherAmplitude delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiWeatherAmplitude uiHandle (uiWeatherAmplitude uiSnap + delta)
    bumpVegBase delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiVegBase uiHandle (uiVegBase uiSnap + delta)
    bumpVegBoost delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiVegBoost uiHandle (uiVegBoost uiSnap + delta)
    bumpVegTempWeight delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiVegTempWeight uiHandle (uiVegTempWeight uiSnap + delta)
    bumpVegPrecipWeight delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiVegPrecipWeight uiHandle (uiVegPrecipWeight uiSnap + delta)
    bumpErosionHydraulic delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiErosionHydraulic uiHandle (uiErosionHydraulic uiSnap + delta)
    bumpErosionThermal delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiErosionThermal uiHandle (uiErosionThermal uiSnap + delta)
    bumpErosionTalus delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiErosionTalus uiHandle (uiErosionTalus uiSnap + delta)
    bumpErosionMaxDrop delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiErosionMaxDrop uiHandle (uiErosionMaxDrop uiSnap + delta)

