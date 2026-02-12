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
  , setUiGlacierSnowTemp
  , setUiGlacierSnowRange
  , setUiGlacierMeltTemp
  , setUiGlacierMeltRate
  , setUiGlacierAccumScale
  , setUiGlacierFlowIters
  , setUiGlacierFlowRate
  , setUiGlacierErosionScale
  , setUiGlacierCarveScale
  , setUiGlacierDepositScale
  , setUiVentDensity
  , setUiVentThreshold
  , setUiHotspotScale
  , setUiHotspotThreshold
  , setUiMagmaRecharge
  , setUiLavaScale
  , setUiAshScale
  , setUiVolcanicDepositScale
  , setUiSoilMoistureThreshold
  , setUiSoilHardnessThreshold
  , setUiSoilFertilityMoistWeight
  , setUiSoilFertilityDepthWeight
  , setUiSinkBreachDepth
  , setUiStreamPowerMaxErosion
  , setUiRiverCarveMaxDepth
  , setUiCoastalErodeStrength
  , setUiHydroHardnessWeight
  , setUiMinLakeSize
  , setUiInlandSeaMinSize
  , setUiRoughnessScale
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
  , setUiTfcCliffSlope
  , setUiTfcMountainSlope
  , setUiTfcMountainRelief
  , setUiTfcHillSlope
  , setUiTfcRollingSlope
  , setUiValleyCurvature
  , setUiRockElevationThreshold
  , setUiRockHardnessThreshold
  , setUiRockHardnessSecondary
  , setUiMoistureIterations
  , setUiBoundaryMotionTemp
  , setUiBoundaryMotionPrecip
  , setUiPlanetRadius
  , setUiAxialTilt
  , setUiInsolation
  , setUiOccWarmScale
  , setUiOccColdScale
  , setUiOccLatPeakDeg
  , setUiOccLatWidthDeg
  , setUiSliceLatCenter
  , setUiSliceLonCenter
  , setUiAlbedoReference
  , setUiAlbedoSensitivity
  , setUiOceanModeration
  , setUiOceanModerateTemp
  , setUiPlateHeightCooling
  , setUiTempNoiseScale
  , setUiMoistAdvect
  , setUiMoistLocal
  , setUiMoistWindEvapScale
  , setUiMoistEvapNoiseScale
  , setUiMoistLandETCoeff
  , setUiMoistBareEvapFrac
  , setUiMoistVegTranspFrac
  , setUiMoistWindETScale
  , setUiMoistCondensationRate
  , setUiMoistRecycleRate
  , setUiMoistITCZStrength
  , setUiMoistITCZWidth
  , setUiOrographicScale
  , setUiOrographicStep
  , setUiCoastalIterations
  , setUiCoastalDiffuse
  , setUiCoastalMoistureBoost
  , setUiWindBeltStrength
  , setUiWindBeltHarmonics
  , setUiWindBeltBase
  , setUiWindBeltRange
  , setUiWindBeltSpeedScale
  , setUiBndLandRange
  , setUiBndTempConvergent
  , setUiBndTempDivergent
  , setUiBndTempTransform
  , setUiBndPrecipConvergent
  , setUiBndPrecipDivergent
  , setUiBndPrecipTransform
  , setUiPlateSize
  , setUiRiftDepth
  , setUiUplift
  , setUiWeatherAmplitude
  , setUiSeasonCycleLength
  , setUiJitterAmplitude
  , setUiPressureBase
  , setUiPressureTempScale
  , setUiPressureCoriolisScale
  , setUiSeasonalBase
  , setUiSeasonalRange
  , setUiHumidityNoiseScale
  , setUiPrecipNoiseScale
  , setUiWeatherITCZWidth
  , setUiWeatherITCZPrecipBoost
  , setUiPressureHumidityScale
  , setUiPressureGradientWindScale
  , setUiWindNoiseScale
  , setUiITCZMigrationScale
  , setUiCloudRHExponent
  , setUiCloudAlbedoEffect
  , setUiCloudPrecipBoost
  , setUiWeatherPhase
  , setUiWeatherTick
  , setUiWindIterations
  , setUiVegBase
  , setUiVegBoost
  , setUiVegTempWeight
  , setUiVegPrecipWeight
  , setUiBtCoastalBand
  , setUiBtSnowElevation
  , setUiBtAlpineElevation
  , setUiBtIceCapTemp
  , setUiBtMontaneLow
  , setUiBtMontanePrecip
  , setUiBtCliffSlope
  , setUiBtValleyMoisture
  , setUiBtDepressionMoisture
  , setUiBtPrecipWeight
  , setUiVbcTempMin
  , setUiVbcTempRange
  , setUiVbcFertilityBoost
  , setUiVbcAlbedoBase
  , setUiVbcAlbedoBare
  , setUiVbcAlbedoVeg
  , setUiVbcOceanAlbedo
  , setUiVbcIceAlbedo
  , setUiBiomeSmoothing
  , setUiVolcanicAshBoost
  , setUiVolcanicLavaPenalty
  , setUiBiomeFeedbackBlend
  , setUiHoverHex
  , setUiHoverWidget
  , setUiLapseRate
  , setUiLatitudeExponent
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
                 (terrainRows, planetRows, climateRows, weatherRows, biomeRows, erosionRows) = buildSliderRowWidgets hoverLayout
                 activeRows = case uiConfigTab uiSnap of
                   ConfigTerrain -> terrainRows
                   ConfigPlanet -> planetRows
                   ConfigClimate -> climateRows
                   ConfigWeather -> weatherRows
                   ConfigBiome -> biomeRows
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
          WidgetConfigOccWarmScaleMinus -> True
          WidgetConfigOccWarmScalePlus -> True
          WidgetConfigOccColdScaleMinus -> True
          WidgetConfigOccColdScalePlus -> True
          WidgetConfigOccLatPeakDegMinus -> True
          WidgetConfigOccLatPeakDegPlus -> True
          WidgetConfigOccLatWidthDegMinus -> True
          WidgetConfigOccLatWidthDegPlus -> True
          WidgetConfigSliceLatCenterMinus -> True
          WidgetConfigSliceLatCenterPlus -> True
          WidgetConfigSliceLonCenterMinus -> True
          WidgetConfigSliceLonCenterPlus -> True
          WidgetConfigLatitudeExponentMinus -> True
          WidgetConfigLatitudeExponentPlus -> True
          WidgetConfigPlateHeightCoolingMinus -> True
          WidgetConfigPlateHeightCoolingPlus -> True
          WidgetConfigTempNoiseScaleMinus -> True
          WidgetConfigTempNoiseScalePlus -> True
          WidgetConfigOceanModerationMinus -> True
          WidgetConfigOceanModerationPlus -> True
          WidgetConfigOceanModerateTempMinus -> True
          WidgetConfigOceanModerateTempPlus -> True
          WidgetConfigAlbedoSensitivityMinus -> True
          WidgetConfigAlbedoSensitivityPlus -> True
          WidgetConfigAlbedoReferenceMinus -> True
          WidgetConfigAlbedoReferencePlus -> True
          WidgetConfigMoistAdvectMinus -> True
          WidgetConfigMoistAdvectPlus -> True
          WidgetConfigMoistLocalMinus -> True
          WidgetConfigMoistLocalPlus -> True
          WidgetConfigMoistWindEvapScaleMinus -> True
          WidgetConfigMoistWindEvapScalePlus -> True
          WidgetConfigMoistEvapNoiseScaleMinus -> True
          WidgetConfigMoistEvapNoiseScalePlus -> True
          WidgetConfigMoistLandETCoeffMinus -> True
          WidgetConfigMoistLandETCoeffPlus -> True
          WidgetConfigMoistBareEvapFracMinus -> True
          WidgetConfigMoistBareEvapFracPlus -> True
          WidgetConfigMoistVegTranspFracMinus -> True
          WidgetConfigMoistVegTranspFracPlus -> True
          WidgetConfigMoistWindETScaleMinus -> True
          WidgetConfigMoistWindETScalePlus -> True
          WidgetConfigMoistCondensationRateMinus -> True
          WidgetConfigMoistCondensationRatePlus -> True
          WidgetConfigMoistRecycleRateMinus -> True
          WidgetConfigMoistRecycleRatePlus -> True
          WidgetConfigMoistITCZStrengthMinus -> True
          WidgetConfigMoistITCZStrengthPlus -> True
          WidgetConfigMoistITCZWidthMinus -> True
          WidgetConfigMoistITCZWidthPlus -> True
          WidgetConfigOrographicScaleMinus -> True
          WidgetConfigOrographicScalePlus -> True
          WidgetConfigOrographicStepMinus -> True
          WidgetConfigOrographicStepPlus -> True
          WidgetConfigCoastalIterationsMinus -> True
          WidgetConfigCoastalIterationsPlus -> True
          WidgetConfigCoastalDiffuseMinus -> True
          WidgetConfigCoastalDiffusePlus -> True
          WidgetConfigCoastalMoistureBoostMinus -> True
          WidgetConfigCoastalMoistureBoostPlus -> True
          WidgetConfigWindBeltStrengthMinus -> True
          WidgetConfigWindBeltStrengthPlus -> True
          WidgetConfigWindBeltHarmonicsMinus -> True
          WidgetConfigWindBeltHarmonicsPlus -> True
          WidgetConfigWindBeltBaseMinus -> True
          WidgetConfigWindBeltBasePlus -> True
          WidgetConfigWindBeltRangeMinus -> True
          WidgetConfigWindBeltRangePlus -> True
          WidgetConfigWindBeltSpeedScaleMinus -> True
          WidgetConfigWindBeltSpeedScalePlus -> True
          WidgetConfigBndLandRangeMinus -> True
          WidgetConfigBndLandRangePlus -> True
          WidgetConfigBndTempConvergentMinus -> True
          WidgetConfigBndTempConvergentPlus -> True
          WidgetConfigBndTempDivergentMinus -> True
          WidgetConfigBndTempDivergentPlus -> True
          WidgetConfigBndTempTransformMinus -> True
          WidgetConfigBndTempTransformPlus -> True
          WidgetConfigBndPrecipConvergentMinus -> True
          WidgetConfigBndPrecipConvergentPlus -> True
          WidgetConfigBndPrecipDivergentMinus -> True
          WidgetConfigBndPrecipDivergentPlus -> True
          WidgetConfigBndPrecipTransformMinus -> True
          WidgetConfigBndPrecipTransformPlus -> True
          WidgetConfigWeatherTickMinus -> True
          WidgetConfigWeatherTickPlus -> True
          WidgetConfigWeatherPhaseMinus -> True
          WidgetConfigWeatherPhasePlus -> True
          WidgetConfigWeatherAmplitudeMinus -> True
          WidgetConfigWeatherAmplitudePlus -> True
          WidgetConfigSeasonCycleLengthMinus -> True
          WidgetConfigSeasonCycleLengthPlus -> True
          WidgetConfigJitterAmplitudeMinus -> True
          WidgetConfigJitterAmplitudePlus -> True
          WidgetConfigPressureBaseMinus -> True
          WidgetConfigPressureBasePlus -> True
          WidgetConfigPressureTempScaleMinus -> True
          WidgetConfigPressureTempScalePlus -> True
          WidgetConfigPressureCoriolisScaleMinus -> True
          WidgetConfigPressureCoriolisScalePlus -> True
          WidgetConfigSeasonalBaseMinus -> True
          WidgetConfigSeasonalBasePlus -> True
          WidgetConfigSeasonalRangeMinus -> True
          WidgetConfigSeasonalRangePlus -> True
          WidgetConfigHumidityNoiseScaleMinus -> True
          WidgetConfigHumidityNoiseScalePlus -> True
          WidgetConfigPrecipNoiseScaleMinus -> True
          WidgetConfigPrecipNoiseScalePlus -> True
          WidgetConfigWeatherITCZWidthMinus -> True
          WidgetConfigWeatherITCZWidthPlus -> True
          WidgetConfigWeatherITCZPrecipBoostMinus -> True
          WidgetConfigWeatherITCZPrecipBoostPlus -> True
          WidgetConfigPressureHumidityScaleMinus -> True
          WidgetConfigPressureHumidityScalePlus -> True
          WidgetConfigPressureGradientWindScaleMinus -> True
          WidgetConfigPressureGradientWindScalePlus -> True
          WidgetConfigWindNoiseScaleMinus -> True
          WidgetConfigWindNoiseScalePlus -> True
          WidgetConfigITCZMigrationScaleMinus -> True
          WidgetConfigITCZMigrationScalePlus -> True
          WidgetConfigCloudRHExponentMinus -> True
          WidgetConfigCloudRHExponentPlus -> True
          WidgetConfigCloudAlbedoEffectMinus -> True
          WidgetConfigCloudAlbedoEffectPlus -> True
          WidgetConfigCloudPrecipBoostMinus -> True
          WidgetConfigCloudPrecipBoostPlus -> True
          WidgetConfigVegBaseMinus -> True
          WidgetConfigVegBasePlus -> True
          WidgetConfigVegBoostMinus -> True
          WidgetConfigVegBoostPlus -> True
          WidgetConfigVegTempWeightMinus -> True
          WidgetConfigVegTempWeightPlus -> True
          WidgetConfigVegPrecipWeightMinus -> True
          WidgetConfigVegPrecipWeightPlus -> True
          WidgetConfigBtCoastalBandMinus -> True
          WidgetConfigBtCoastalBandPlus -> True
          WidgetConfigBtSnowElevationMinus -> True
          WidgetConfigBtSnowElevationPlus -> True
          WidgetConfigBtAlpineElevationMinus -> True
          WidgetConfigBtAlpineElevationPlus -> True
          WidgetConfigBtIceCapTempMinus -> True
          WidgetConfigBtIceCapTempPlus -> True
          WidgetConfigBtMontaneLowMinus -> True
          WidgetConfigBtMontaneLowPlus -> True
          WidgetConfigBtMontanePrecipMinus -> True
          WidgetConfigBtMontanePrecipPlus -> True
          WidgetConfigBtCliffSlopeMinus -> True
          WidgetConfigBtCliffSlopePlus -> True
          WidgetConfigBtValleyMoistureMinus -> True
          WidgetConfigBtValleyMoisturePlus -> True
          WidgetConfigBtDepressionMoistureMinus -> True
          WidgetConfigBtDepressionMoisturePlus -> True
          WidgetConfigBtPrecipWeightMinus -> True
          WidgetConfigBtPrecipWeightPlus -> True
          WidgetConfigVbcTempMinMinus -> True
          WidgetConfigVbcTempMinPlus -> True
          WidgetConfigVbcTempRangeMinus -> True
          WidgetConfigVbcTempRangePlus -> True
          WidgetConfigVbcFertilityBoostMinus -> True
          WidgetConfigVbcFertilityBoostPlus -> True
          WidgetConfigVbcAlbedoBaseMinus -> True
          WidgetConfigVbcAlbedoBasePlus -> True
          WidgetConfigVbcAlbedoBareMinus -> True
          WidgetConfigVbcAlbedoBarePlus -> True
          WidgetConfigVbcAlbedoVegMinus -> True
          WidgetConfigVbcAlbedoVegPlus -> True
          WidgetConfigVbcOceanAlbedoMinus -> True
          WidgetConfigVbcOceanAlbedoPlus -> True
          WidgetConfigVbcIceAlbedoMinus -> True
          WidgetConfigVbcIceAlbedoPlus -> True
          WidgetConfigBiomeSmoothingMinus -> True
          WidgetConfigBiomeSmoothingPlus -> True
          WidgetConfigVolcanicAshBoostMinus -> True
          WidgetConfigVolcanicAshBoostPlus -> True
          WidgetConfigVolcanicLavaPenaltyMinus -> True
          WidgetConfigVolcanicLavaPenaltyPlus -> True
          WidgetConfigBiomeFeedbackBlendMinus -> True
          WidgetConfigBiomeFeedbackBlendPlus -> True
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
          WidgetConfigTfcCliffSlopeMinus -> True
          WidgetConfigTfcCliffSlopePlus -> True
          WidgetConfigTfcMountainSlopeMinus -> True
          WidgetConfigTfcMountainSlopePlus -> True
          WidgetConfigTfcMountainReliefMinus -> True
          WidgetConfigTfcMountainReliefPlus -> True
          WidgetConfigTfcHillSlopeMinus -> True
          WidgetConfigTfcHillSlopePlus -> True
          WidgetConfigTfcRollingSlopeMinus -> True
          WidgetConfigTfcRollingSlopePlus -> True
          WidgetConfigValleyCurvatureMinus -> True
          WidgetConfigValleyCurvaturePlus -> True
          WidgetConfigRockElevationThresholdMinus -> True
          WidgetConfigRockElevationThresholdPlus -> True
          WidgetConfigRockHardnessThresholdMinus -> True
          WidgetConfigRockHardnessThresholdPlus -> True
          WidgetConfigRockHardnessSecondaryMinus -> True
          WidgetConfigRockHardnessSecondaryPlus -> True
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
          WidgetConfigGlacierSnowTempMinus -> True
          WidgetConfigGlacierSnowTempPlus -> True
          WidgetConfigGlacierSnowRangeMinus -> True
          WidgetConfigGlacierSnowRangePlus -> True
          WidgetConfigGlacierMeltTempMinus -> True
          WidgetConfigGlacierMeltTempPlus -> True
          WidgetConfigGlacierMeltRateMinus -> True
          WidgetConfigGlacierMeltRatePlus -> True
          WidgetConfigGlacierAccumScaleMinus -> True
          WidgetConfigGlacierAccumScalePlus -> True
          WidgetConfigGlacierFlowItersMinus -> True
          WidgetConfigGlacierFlowItersPlus -> True
          WidgetConfigGlacierFlowRateMinus -> True
          WidgetConfigGlacierFlowRatePlus -> True
          WidgetConfigGlacierErosionScaleMinus -> True
          WidgetConfigGlacierErosionScalePlus -> True
          WidgetConfigGlacierCarveScaleMinus -> True
          WidgetConfigGlacierCarveScalePlus -> True
          WidgetConfigGlacierDepositScaleMinus -> True
          WidgetConfigGlacierDepositScalePlus -> True
          WidgetConfigVentDensityMinus -> True
          WidgetConfigVentDensityPlus -> True
          WidgetConfigVentThresholdMinus -> True
          WidgetConfigVentThresholdPlus -> True
          WidgetConfigHotspotScaleMinus -> True
          WidgetConfigHotspotScalePlus -> True
          WidgetConfigHotspotThresholdMinus -> True
          WidgetConfigHotspotThresholdPlus -> True
          WidgetConfigMagmaRechargeMinus -> True
          WidgetConfigMagmaRechargePlus -> True
          WidgetConfigLavaScaleMinus -> True
          WidgetConfigLavaScalePlus -> True
          WidgetConfigAshScaleMinus -> True
          WidgetConfigAshScalePlus -> True
          WidgetConfigVolcanicDepositScaleMinus -> True
          WidgetConfigVolcanicDepositScalePlus -> True
          WidgetConfigSoilMoistureThresholdMinus -> True
          WidgetConfigSoilMoistureThresholdPlus -> True
          WidgetConfigSoilHardnessThresholdMinus -> True
          WidgetConfigSoilHardnessThresholdPlus -> True
          WidgetConfigSoilFertilityMoistWeightMinus -> True
          WidgetConfigSoilFertilityMoistWeightPlus -> True
          WidgetConfigSoilFertilityDepthWeightMinus -> True
          WidgetConfigSoilFertilityDepthWeightPlus -> True
          WidgetConfigSinkBreachDepthMinus -> True
          WidgetConfigSinkBreachDepthPlus -> True
          WidgetConfigStreamPowerMaxErosionMinus -> True
          WidgetConfigStreamPowerMaxErosionPlus -> True
          WidgetConfigRiverCarveMaxDepthMinus -> True
          WidgetConfigRiverCarveMaxDepthPlus -> True
          WidgetConfigCoastalErodeStrengthMinus -> True
          WidgetConfigCoastalErodeStrengthPlus -> True
          WidgetConfigHydroHardnessWeightMinus -> True
          WidgetConfigHydroHardnessWeightPlus -> True
          WidgetConfigMinLakeSizeMinus -> True
          WidgetConfigMinLakeSizePlus -> True
          WidgetConfigInlandSeaMinSizeMinus -> True
          WidgetConfigInlandSeaMinSizePlus -> True
          WidgetConfigRoughnessScaleMinus -> True
          WidgetConfigRoughnessScalePlus -> True
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
          WidgetConfigPlanetRadiusMinus -> tab == ConfigPlanet
          WidgetConfigPlanetRadiusPlus -> tab == ConfigPlanet
          WidgetConfigAxialTiltMinus -> tab == ConfigPlanet
          WidgetConfigAxialTiltPlus -> tab == ConfigPlanet
          WidgetConfigInsolationMinus -> tab == ConfigPlanet
          WidgetConfigInsolationPlus -> tab == ConfigPlanet
          WidgetConfigOccWarmScaleMinus -> tab == ConfigPlanet
          WidgetConfigOccWarmScalePlus -> tab == ConfigPlanet
          WidgetConfigOccColdScaleMinus -> tab == ConfigPlanet
          WidgetConfigOccColdScalePlus -> tab == ConfigPlanet
          WidgetConfigOccLatPeakDegMinus -> tab == ConfigPlanet
          WidgetConfigOccLatPeakDegPlus -> tab == ConfigPlanet
          WidgetConfigOccLatWidthDegMinus -> tab == ConfigPlanet
          WidgetConfigOccLatWidthDegPlus -> tab == ConfigPlanet
          WidgetConfigSliceLatCenterMinus -> tab == ConfigClimate
          WidgetConfigSliceLatCenterPlus -> tab == ConfigClimate
          WidgetConfigSliceLonCenterMinus -> tab == ConfigClimate
          WidgetConfigSliceLonCenterPlus -> tab == ConfigClimate
          WidgetConfigLatitudeExponentMinus -> tab == ConfigClimate
          WidgetConfigLatitudeExponentPlus -> tab == ConfigClimate
          WidgetConfigPlateHeightCoolingMinus -> tab == ConfigClimate
          WidgetConfigPlateHeightCoolingPlus -> tab == ConfigClimate
          WidgetConfigTempNoiseScaleMinus -> tab == ConfigClimate
          WidgetConfigTempNoiseScalePlus -> tab == ConfigClimate
          WidgetConfigOceanModerationMinus -> tab == ConfigClimate
          WidgetConfigOceanModerationPlus -> tab == ConfigClimate
          WidgetConfigOceanModerateTempMinus -> tab == ConfigClimate
          WidgetConfigOceanModerateTempPlus -> tab == ConfigClimate
          WidgetConfigAlbedoSensitivityMinus -> tab == ConfigClimate
          WidgetConfigAlbedoSensitivityPlus -> tab == ConfigClimate
          WidgetConfigAlbedoReferenceMinus -> tab == ConfigClimate
          WidgetConfigAlbedoReferencePlus -> tab == ConfigClimate
          WidgetConfigMoistAdvectMinus -> tab == ConfigClimate
          WidgetConfigMoistAdvectPlus -> tab == ConfigClimate
          WidgetConfigMoistLocalMinus -> tab == ConfigClimate
          WidgetConfigMoistLocalPlus -> tab == ConfigClimate
          WidgetConfigMoistWindEvapScaleMinus -> tab == ConfigClimate
          WidgetConfigMoistWindEvapScalePlus -> tab == ConfigClimate
          WidgetConfigMoistEvapNoiseScaleMinus -> tab == ConfigClimate
          WidgetConfigMoistEvapNoiseScalePlus -> tab == ConfigClimate
          WidgetConfigMoistLandETCoeffMinus -> tab == ConfigClimate
          WidgetConfigMoistLandETCoeffPlus -> tab == ConfigClimate
          WidgetConfigMoistBareEvapFracMinus -> tab == ConfigClimate
          WidgetConfigMoistBareEvapFracPlus -> tab == ConfigClimate
          WidgetConfigMoistVegTranspFracMinus -> tab == ConfigClimate
          WidgetConfigMoistVegTranspFracPlus -> tab == ConfigClimate
          WidgetConfigMoistWindETScaleMinus -> tab == ConfigClimate
          WidgetConfigMoistWindETScalePlus -> tab == ConfigClimate
          WidgetConfigMoistCondensationRateMinus -> tab == ConfigClimate
          WidgetConfigMoistCondensationRatePlus -> tab == ConfigClimate
          WidgetConfigMoistRecycleRateMinus -> tab == ConfigClimate
          WidgetConfigMoistRecycleRatePlus -> tab == ConfigClimate
          WidgetConfigMoistITCZStrengthMinus -> tab == ConfigClimate
          WidgetConfigMoistITCZStrengthPlus -> tab == ConfigClimate
          WidgetConfigMoistITCZWidthMinus -> tab == ConfigClimate
          WidgetConfigMoistITCZWidthPlus -> tab == ConfigClimate
          WidgetConfigOrographicScaleMinus -> tab == ConfigClimate
          WidgetConfigOrographicScalePlus -> tab == ConfigClimate
          WidgetConfigOrographicStepMinus -> tab == ConfigClimate
          WidgetConfigOrographicStepPlus -> tab == ConfigClimate
          WidgetConfigCoastalIterationsMinus -> tab == ConfigClimate
          WidgetConfigCoastalIterationsPlus -> tab == ConfigClimate
          WidgetConfigCoastalDiffuseMinus -> tab == ConfigClimate
          WidgetConfigCoastalDiffusePlus -> tab == ConfigClimate
          WidgetConfigCoastalMoistureBoostMinus -> tab == ConfigClimate
          WidgetConfigCoastalMoistureBoostPlus -> tab == ConfigClimate
          WidgetConfigWindBeltStrengthMinus -> tab == ConfigClimate
          WidgetConfigWindBeltStrengthPlus -> tab == ConfigClimate
          WidgetConfigWindBeltHarmonicsMinus -> tab == ConfigClimate
          WidgetConfigWindBeltHarmonicsPlus -> tab == ConfigClimate
          WidgetConfigWindBeltBaseMinus -> tab == ConfigClimate
          WidgetConfigWindBeltBasePlus -> tab == ConfigClimate
          WidgetConfigWindBeltRangeMinus -> tab == ConfigClimate
          WidgetConfigWindBeltRangePlus -> tab == ConfigClimate
          WidgetConfigWindBeltSpeedScaleMinus -> tab == ConfigClimate
          WidgetConfigWindBeltSpeedScalePlus -> tab == ConfigClimate
          WidgetConfigBndLandRangeMinus -> tab == ConfigClimate
          WidgetConfigBndLandRangePlus -> tab == ConfigClimate
          WidgetConfigBndTempConvergentMinus -> tab == ConfigClimate
          WidgetConfigBndTempConvergentPlus -> tab == ConfigClimate
          WidgetConfigBndTempDivergentMinus -> tab == ConfigClimate
          WidgetConfigBndTempDivergentPlus -> tab == ConfigClimate
          WidgetConfigBndTempTransformMinus -> tab == ConfigClimate
          WidgetConfigBndTempTransformPlus -> tab == ConfigClimate
          WidgetConfigBndPrecipConvergentMinus -> tab == ConfigClimate
          WidgetConfigBndPrecipConvergentPlus -> tab == ConfigClimate
          WidgetConfigBndPrecipDivergentMinus -> tab == ConfigClimate
          WidgetConfigBndPrecipDivergentPlus -> tab == ConfigClimate
          WidgetConfigBndPrecipTransformMinus -> tab == ConfigClimate
          WidgetConfigBndPrecipTransformPlus -> tab == ConfigClimate
          WidgetConfigWeatherTickMinus -> tab == ConfigWeather
          WidgetConfigWeatherTickPlus -> tab == ConfigWeather
          WidgetConfigWeatherPhaseMinus -> tab == ConfigWeather
          WidgetConfigWeatherPhasePlus -> tab == ConfigWeather
          WidgetConfigWeatherAmplitudeMinus -> tab == ConfigWeather
          WidgetConfigWeatherAmplitudePlus -> tab == ConfigWeather
          WidgetConfigSeasonCycleLengthMinus -> tab == ConfigWeather
          WidgetConfigSeasonCycleLengthPlus -> tab == ConfigWeather
          WidgetConfigJitterAmplitudeMinus -> tab == ConfigWeather
          WidgetConfigJitterAmplitudePlus -> tab == ConfigWeather
          WidgetConfigPressureBaseMinus -> tab == ConfigWeather
          WidgetConfigPressureBasePlus -> tab == ConfigWeather
          WidgetConfigPressureTempScaleMinus -> tab == ConfigWeather
          WidgetConfigPressureTempScalePlus -> tab == ConfigWeather
          WidgetConfigPressureCoriolisScaleMinus -> tab == ConfigWeather
          WidgetConfigPressureCoriolisScalePlus -> tab == ConfigWeather
          WidgetConfigSeasonalBaseMinus -> tab == ConfigWeather
          WidgetConfigSeasonalBasePlus -> tab == ConfigWeather
          WidgetConfigSeasonalRangeMinus -> tab == ConfigWeather
          WidgetConfigSeasonalRangePlus -> tab == ConfigWeather
          WidgetConfigHumidityNoiseScaleMinus -> tab == ConfigWeather
          WidgetConfigHumidityNoiseScalePlus -> tab == ConfigWeather
          WidgetConfigPrecipNoiseScaleMinus -> tab == ConfigWeather
          WidgetConfigPrecipNoiseScalePlus -> tab == ConfigWeather
          WidgetConfigWeatherITCZWidthMinus -> tab == ConfigWeather
          WidgetConfigWeatherITCZWidthPlus -> tab == ConfigWeather
          WidgetConfigWeatherITCZPrecipBoostMinus -> tab == ConfigWeather
          WidgetConfigWeatherITCZPrecipBoostPlus -> tab == ConfigWeather
          WidgetConfigPressureHumidityScaleMinus -> tab == ConfigWeather
          WidgetConfigPressureHumidityScalePlus -> tab == ConfigWeather
          WidgetConfigPressureGradientWindScaleMinus -> tab == ConfigWeather
          WidgetConfigPressureGradientWindScalePlus -> tab == ConfigWeather
          WidgetConfigWindNoiseScaleMinus -> tab == ConfigWeather
          WidgetConfigWindNoiseScalePlus -> tab == ConfigWeather
          WidgetConfigITCZMigrationScaleMinus -> tab == ConfigWeather
          WidgetConfigITCZMigrationScalePlus -> tab == ConfigWeather
          WidgetConfigCloudRHExponentMinus -> tab == ConfigWeather
          WidgetConfigCloudRHExponentPlus -> tab == ConfigWeather
          WidgetConfigCloudAlbedoEffectMinus -> tab == ConfigWeather
          WidgetConfigCloudAlbedoEffectPlus -> tab == ConfigWeather
          WidgetConfigCloudPrecipBoostMinus -> tab == ConfigWeather
          WidgetConfigCloudPrecipBoostPlus -> tab == ConfigWeather
          WidgetConfigVegBaseMinus -> tab == ConfigBiome
          WidgetConfigVegBasePlus -> tab == ConfigBiome
          WidgetConfigVegBoostMinus -> tab == ConfigBiome
          WidgetConfigVegBoostPlus -> tab == ConfigBiome
          WidgetConfigVegTempWeightMinus -> tab == ConfigBiome
          WidgetConfigVegTempWeightPlus -> tab == ConfigBiome
          WidgetConfigVegPrecipWeightMinus -> tab == ConfigBiome
          WidgetConfigVegPrecipWeightPlus -> tab == ConfigBiome
          WidgetConfigBtCoastalBandMinus -> tab == ConfigBiome
          WidgetConfigBtCoastalBandPlus -> tab == ConfigBiome
          WidgetConfigBtSnowElevationMinus -> tab == ConfigBiome
          WidgetConfigBtSnowElevationPlus -> tab == ConfigBiome
          WidgetConfigBtAlpineElevationMinus -> tab == ConfigBiome
          WidgetConfigBtAlpineElevationPlus -> tab == ConfigBiome
          WidgetConfigBtIceCapTempMinus -> tab == ConfigBiome
          WidgetConfigBtIceCapTempPlus -> tab == ConfigBiome
          WidgetConfigBtMontaneLowMinus -> tab == ConfigBiome
          WidgetConfigBtMontaneLowPlus -> tab == ConfigBiome
          WidgetConfigBtMontanePrecipMinus -> tab == ConfigBiome
          WidgetConfigBtMontanePrecipPlus -> tab == ConfigBiome
          WidgetConfigBtCliffSlopeMinus -> tab == ConfigBiome
          WidgetConfigBtCliffSlopePlus -> tab == ConfigBiome
          WidgetConfigBtValleyMoistureMinus -> tab == ConfigBiome
          WidgetConfigBtValleyMoisturePlus -> tab == ConfigBiome
          WidgetConfigBtDepressionMoistureMinus -> tab == ConfigBiome
          WidgetConfigBtDepressionMoisturePlus -> tab == ConfigBiome
          WidgetConfigBtPrecipWeightMinus -> tab == ConfigBiome
          WidgetConfigBtPrecipWeightPlus -> tab == ConfigBiome
          WidgetConfigVbcTempMinMinus -> tab == ConfigBiome
          WidgetConfigVbcTempMinPlus -> tab == ConfigBiome
          WidgetConfigVbcTempRangeMinus -> tab == ConfigBiome
          WidgetConfigVbcTempRangePlus -> tab == ConfigBiome
          WidgetConfigVbcFertilityBoostMinus -> tab == ConfigBiome
          WidgetConfigVbcFertilityBoostPlus -> tab == ConfigBiome
          WidgetConfigVbcAlbedoBaseMinus -> tab == ConfigBiome
          WidgetConfigVbcAlbedoBasePlus -> tab == ConfigBiome
          WidgetConfigVbcAlbedoBareMinus -> tab == ConfigBiome
          WidgetConfigVbcAlbedoBarePlus -> tab == ConfigBiome
          WidgetConfigVbcAlbedoVegMinus -> tab == ConfigBiome
          WidgetConfigVbcAlbedoVegPlus -> tab == ConfigBiome
          WidgetConfigVbcOceanAlbedoMinus -> tab == ConfigBiome
          WidgetConfigVbcOceanAlbedoPlus -> tab == ConfigBiome
          WidgetConfigVbcIceAlbedoMinus -> tab == ConfigBiome
          WidgetConfigVbcIceAlbedoPlus -> tab == ConfigBiome
          WidgetConfigBiomeSmoothingMinus -> tab == ConfigBiome
          WidgetConfigBiomeSmoothingPlus -> tab == ConfigBiome
          WidgetConfigVolcanicAshBoostMinus -> tab == ConfigBiome
          WidgetConfigVolcanicAshBoostPlus -> tab == ConfigBiome
          WidgetConfigVolcanicLavaPenaltyMinus -> tab == ConfigBiome
          WidgetConfigVolcanicLavaPenaltyPlus -> tab == ConfigBiome
          WidgetConfigBiomeFeedbackBlendMinus -> tab == ConfigBiome
          WidgetConfigBiomeFeedbackBlendPlus -> tab == ConfigBiome
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
          WidgetConfigTfcCliffSlopeMinus -> tab == ConfigTerrain
          WidgetConfigTfcCliffSlopePlus -> tab == ConfigTerrain
          WidgetConfigTfcMountainSlopeMinus -> tab == ConfigTerrain
          WidgetConfigTfcMountainSlopePlus -> tab == ConfigTerrain
          WidgetConfigTfcMountainReliefMinus -> tab == ConfigTerrain
          WidgetConfigTfcMountainReliefPlus -> tab == ConfigTerrain
          WidgetConfigTfcHillSlopeMinus -> tab == ConfigTerrain
          WidgetConfigTfcHillSlopePlus -> tab == ConfigTerrain
          WidgetConfigTfcRollingSlopeMinus -> tab == ConfigTerrain
          WidgetConfigTfcRollingSlopePlus -> tab == ConfigTerrain
          WidgetConfigValleyCurvatureMinus -> tab == ConfigTerrain
          WidgetConfigValleyCurvaturePlus -> tab == ConfigTerrain
          WidgetConfigRockElevationThresholdMinus -> tab == ConfigTerrain
          WidgetConfigRockElevationThresholdPlus -> tab == ConfigTerrain
          WidgetConfigRockHardnessThresholdMinus -> tab == ConfigTerrain
          WidgetConfigRockHardnessThresholdPlus -> tab == ConfigTerrain
          WidgetConfigRockHardnessSecondaryMinus -> tab == ConfigTerrain
          WidgetConfigRockHardnessSecondaryPlus -> tab == ConfigTerrain
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
          WidgetConfigGlacierSnowTempMinus -> tab == ConfigErosion
          WidgetConfigGlacierSnowTempPlus -> tab == ConfigErosion
          WidgetConfigGlacierSnowRangeMinus -> tab == ConfigErosion
          WidgetConfigGlacierSnowRangePlus -> tab == ConfigErosion
          WidgetConfigGlacierMeltTempMinus -> tab == ConfigErosion
          WidgetConfigGlacierMeltTempPlus -> tab == ConfigErosion
          WidgetConfigGlacierMeltRateMinus -> tab == ConfigErosion
          WidgetConfigGlacierMeltRatePlus -> tab == ConfigErosion
          WidgetConfigGlacierAccumScaleMinus -> tab == ConfigErosion
          WidgetConfigGlacierAccumScalePlus -> tab == ConfigErosion
          WidgetConfigGlacierFlowItersMinus -> tab == ConfigErosion
          WidgetConfigGlacierFlowItersPlus -> tab == ConfigErosion
          WidgetConfigGlacierFlowRateMinus -> tab == ConfigErosion
          WidgetConfigGlacierFlowRatePlus -> tab == ConfigErosion
          WidgetConfigGlacierErosionScaleMinus -> tab == ConfigErosion
          WidgetConfigGlacierErosionScalePlus -> tab == ConfigErosion
          WidgetConfigGlacierCarveScaleMinus -> tab == ConfigErosion
          WidgetConfigGlacierCarveScalePlus -> tab == ConfigErosion
          WidgetConfigGlacierDepositScaleMinus -> tab == ConfigErosion
          WidgetConfigGlacierDepositScalePlus -> tab == ConfigErosion
          WidgetConfigVentDensityMinus -> tab == ConfigErosion
          WidgetConfigVentDensityPlus -> tab == ConfigErosion
          WidgetConfigVentThresholdMinus -> tab == ConfigErosion
          WidgetConfigVentThresholdPlus -> tab == ConfigErosion
          WidgetConfigHotspotScaleMinus -> tab == ConfigErosion
          WidgetConfigHotspotScalePlus -> tab == ConfigErosion
          WidgetConfigHotspotThresholdMinus -> tab == ConfigErosion
          WidgetConfigHotspotThresholdPlus -> tab == ConfigErosion
          WidgetConfigMagmaRechargeMinus -> tab == ConfigErosion
          WidgetConfigMagmaRechargePlus -> tab == ConfigErosion
          WidgetConfigLavaScaleMinus -> tab == ConfigErosion
          WidgetConfigLavaScalePlus -> tab == ConfigErosion
          WidgetConfigAshScaleMinus -> tab == ConfigErosion
          WidgetConfigAshScalePlus -> tab == ConfigErosion
          WidgetConfigVolcanicDepositScaleMinus -> tab == ConfigErosion
          WidgetConfigVolcanicDepositScalePlus -> tab == ConfigErosion
          WidgetConfigSoilMoistureThresholdMinus -> tab == ConfigErosion
          WidgetConfigSoilMoistureThresholdPlus -> tab == ConfigErosion
          WidgetConfigSoilHardnessThresholdMinus -> tab == ConfigErosion
          WidgetConfigSoilHardnessThresholdPlus -> tab == ConfigErosion
          WidgetConfigSoilFertilityMoistWeightMinus -> tab == ConfigErosion
          WidgetConfigSoilFertilityMoistWeightPlus -> tab == ConfigErosion
          WidgetConfigSoilFertilityDepthWeightMinus -> tab == ConfigErosion
          WidgetConfigSoilFertilityDepthWeightPlus -> tab == ConfigErosion
          WidgetConfigSinkBreachDepthMinus -> tab == ConfigErosion
          WidgetConfigSinkBreachDepthPlus -> tab == ConfigErosion
          WidgetConfigStreamPowerMaxErosionMinus -> tab == ConfigErosion
          WidgetConfigStreamPowerMaxErosionPlus -> tab == ConfigErosion
          WidgetConfigRiverCarveMaxDepthMinus -> tab == ConfigErosion
          WidgetConfigRiverCarveMaxDepthPlus -> tab == ConfigErosion
          WidgetConfigCoastalErodeStrengthMinus -> tab == ConfigErosion
          WidgetConfigCoastalErodeStrengthPlus -> tab == ConfigErosion
          WidgetConfigHydroHardnessWeightMinus -> tab == ConfigErosion
          WidgetConfigHydroHardnessWeightPlus -> tab == ConfigErosion
          WidgetConfigMinLakeSizeMinus -> tab == ConfigErosion
          WidgetConfigMinLakeSizePlus -> tab == ConfigErosion
          WidgetConfigInlandSeaMinSizeMinus -> tab == ConfigErosion
          WidgetConfigInlandSeaMinSizePlus -> tab == ConfigErosion
          WidgetConfigRoughnessScaleMinus -> tab == ConfigErosion
          WidgetConfigRoughnessScalePlus -> tab == ConfigErosion
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
          ; Just WidgetConfigTabPlanet -> whenConfigVisible (setUiConfigTab uiHandle ConfigPlanet >> setUiConfigScroll uiHandle 0)
          ; Just WidgetConfigTabClimate -> whenConfigVisible (setUiConfigTab uiHandle ConfigClimate >> setUiConfigScroll uiHandle 0)
          ; Just WidgetConfigTabWeather -> whenConfigVisible (setUiConfigTab uiHandle ConfigWeather >> setUiConfigScroll uiHandle 0)
          ; Just WidgetConfigTabBiome -> whenConfigVisible (setUiConfigTab uiHandle ConfigBiome >> setUiConfigScroll uiHandle 0)
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
          ; Just WidgetConfigTfcCliffSlopeMinus -> whenConfigVisible (bumpTfcCliffSlope (-0.05))
          ; Just WidgetConfigTfcCliffSlopePlus -> whenConfigVisible (bumpTfcCliffSlope 0.05)
          ; Just WidgetConfigTfcMountainSlopeMinus -> whenConfigVisible (bumpTfcMountainSlope (-0.05))
          ; Just WidgetConfigTfcMountainSlopePlus -> whenConfigVisible (bumpTfcMountainSlope 0.05)
          ; Just WidgetConfigTfcMountainReliefMinus -> whenConfigVisible (bumpTfcMountainRelief (-0.05))
          ; Just WidgetConfigTfcMountainReliefPlus -> whenConfigVisible (bumpTfcMountainRelief 0.05)
          ; Just WidgetConfigTfcHillSlopeMinus -> whenConfigVisible (bumpTfcHillSlope (-0.05))
          ; Just WidgetConfigTfcHillSlopePlus -> whenConfigVisible (bumpTfcHillSlope 0.05)
          ; Just WidgetConfigTfcRollingSlopeMinus -> whenConfigVisible (bumpTfcRollingSlope (-0.05))
          ; Just WidgetConfigTfcRollingSlopePlus -> whenConfigVisible (bumpTfcRollingSlope 0.05)
          ; Just WidgetConfigValleyCurvatureMinus -> whenConfigVisible (bumpValleyCurvature (-0.05))
          ; Just WidgetConfigValleyCurvaturePlus -> whenConfigVisible (bumpValleyCurvature 0.05)
          ; Just WidgetConfigRockElevationThresholdMinus -> whenConfigVisible (bumpRockElevationThreshold (-0.05))
          ; Just WidgetConfigRockElevationThresholdPlus -> whenConfigVisible (bumpRockElevationThreshold 0.05)
          ; Just WidgetConfigRockHardnessThresholdMinus -> whenConfigVisible (bumpRockHardnessThreshold (-0.05))
          ; Just WidgetConfigRockHardnessThresholdPlus -> whenConfigVisible (bumpRockHardnessThreshold 0.05)
          ; Just WidgetConfigRockHardnessSecondaryMinus -> whenConfigVisible (bumpRockHardnessSecondary (-0.05))
          ; Just WidgetConfigRockHardnessSecondaryPlus -> whenConfigVisible (bumpRockHardnessSecondary 0.05)
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
          ; Just WidgetConfigGlacierSnowTempMinus -> whenConfigVisible (bumpGlacierSnowTemp (-0.05))
          ; Just WidgetConfigGlacierSnowTempPlus -> whenConfigVisible (bumpGlacierSnowTemp 0.05)
          ; Just WidgetConfigGlacierSnowRangeMinus -> whenConfigVisible (bumpGlacierSnowRange (-0.05))
          ; Just WidgetConfigGlacierSnowRangePlus -> whenConfigVisible (bumpGlacierSnowRange 0.05)
          ; Just WidgetConfigGlacierMeltTempMinus -> whenConfigVisible (bumpGlacierMeltTemp (-0.05))
          ; Just WidgetConfigGlacierMeltTempPlus -> whenConfigVisible (bumpGlacierMeltTemp 0.05)
          ; Just WidgetConfigGlacierMeltRateMinus -> whenConfigVisible (bumpGlacierMeltRate (-0.05))
          ; Just WidgetConfigGlacierMeltRatePlus -> whenConfigVisible (bumpGlacierMeltRate 0.05)
          ; Just WidgetConfigGlacierAccumScaleMinus -> whenConfigVisible (bumpGlacierAccumScale (-0.05))
          ; Just WidgetConfigGlacierAccumScalePlus -> whenConfigVisible (bumpGlacierAccumScale 0.05)
          ; Just WidgetConfigGlacierFlowItersMinus -> whenConfigVisible (bumpGlacierFlowIters (-0.05))
          ; Just WidgetConfigGlacierFlowItersPlus -> whenConfigVisible (bumpGlacierFlowIters 0.05)
          ; Just WidgetConfigGlacierFlowRateMinus -> whenConfigVisible (bumpGlacierFlowRate (-0.05))
          ; Just WidgetConfigGlacierFlowRatePlus -> whenConfigVisible (bumpGlacierFlowRate 0.05)
          ; Just WidgetConfigGlacierErosionScaleMinus -> whenConfigVisible (bumpGlacierErosionScale (-0.05))
          ; Just WidgetConfigGlacierErosionScalePlus -> whenConfigVisible (bumpGlacierErosionScale 0.05)
          ; Just WidgetConfigGlacierCarveScaleMinus -> whenConfigVisible (bumpGlacierCarveScale (-0.05))
          ; Just WidgetConfigGlacierCarveScalePlus -> whenConfigVisible (bumpGlacierCarveScale 0.05)
          ; Just WidgetConfigGlacierDepositScaleMinus -> whenConfigVisible (bumpGlacierDepositScale (-0.05))
          ; Just WidgetConfigGlacierDepositScalePlus -> whenConfigVisible (bumpGlacierDepositScale 0.05)
          ; Just WidgetConfigVentDensityMinus -> whenConfigVisible (bumpVentDensity (-0.05))
          ; Just WidgetConfigVentDensityPlus -> whenConfigVisible (bumpVentDensity 0.05)
          ; Just WidgetConfigVentThresholdMinus -> whenConfigVisible (bumpVentThreshold (-0.05))
          ; Just WidgetConfigVentThresholdPlus -> whenConfigVisible (bumpVentThreshold 0.05)
          ; Just WidgetConfigHotspotScaleMinus -> whenConfigVisible (bumpHotspotScale (-0.05))
          ; Just WidgetConfigHotspotScalePlus -> whenConfigVisible (bumpHotspotScale 0.05)
          ; Just WidgetConfigHotspotThresholdMinus -> whenConfigVisible (bumpHotspotThreshold (-0.05))
          ; Just WidgetConfigHotspotThresholdPlus -> whenConfigVisible (bumpHotspotThreshold 0.05)
          ; Just WidgetConfigMagmaRechargeMinus -> whenConfigVisible (bumpMagmaRecharge (-0.05))
          ; Just WidgetConfigMagmaRechargePlus -> whenConfigVisible (bumpMagmaRecharge 0.05)
          ; Just WidgetConfigLavaScaleMinus -> whenConfigVisible (bumpLavaScale (-0.05))
          ; Just WidgetConfigLavaScalePlus -> whenConfigVisible (bumpLavaScale 0.05)
          ; Just WidgetConfigAshScaleMinus -> whenConfigVisible (bumpAshScale (-0.05))
          ; Just WidgetConfigAshScalePlus -> whenConfigVisible (bumpAshScale 0.05)
          ; Just WidgetConfigVolcanicDepositScaleMinus -> whenConfigVisible (bumpVolcanicDepositScale (-0.05))
          ; Just WidgetConfigVolcanicDepositScalePlus -> whenConfigVisible (bumpVolcanicDepositScale 0.05)
          ; Just WidgetConfigSoilMoistureThresholdMinus -> whenConfigVisible (bumpSoilMoistureThreshold (-0.05))
          ; Just WidgetConfigSoilMoistureThresholdPlus -> whenConfigVisible (bumpSoilMoistureThreshold 0.05)
          ; Just WidgetConfigSoilHardnessThresholdMinus -> whenConfigVisible (bumpSoilHardnessThreshold (-0.05))
          ; Just WidgetConfigSoilHardnessThresholdPlus -> whenConfigVisible (bumpSoilHardnessThreshold 0.05)
          ; Just WidgetConfigSoilFertilityMoistWeightMinus -> whenConfigVisible (bumpSoilFertilityMoistWeight (-0.05))
          ; Just WidgetConfigSoilFertilityMoistWeightPlus -> whenConfigVisible (bumpSoilFertilityMoistWeight 0.05)
          ; Just WidgetConfigSoilFertilityDepthWeightMinus -> whenConfigVisible (bumpSoilFertilityDepthWeight (-0.05))
          ; Just WidgetConfigSoilFertilityDepthWeightPlus -> whenConfigVisible (bumpSoilFertilityDepthWeight 0.05)
          ; Just WidgetConfigSinkBreachDepthMinus -> whenConfigVisible (bumpSinkBreachDepth (-0.05))
          ; Just WidgetConfigSinkBreachDepthPlus -> whenConfigVisible (bumpSinkBreachDepth 0.05)
          ; Just WidgetConfigStreamPowerMaxErosionMinus -> whenConfigVisible (bumpStreamPowerMaxErosion (-0.05))
          ; Just WidgetConfigStreamPowerMaxErosionPlus -> whenConfigVisible (bumpStreamPowerMaxErosion 0.05)
          ; Just WidgetConfigRiverCarveMaxDepthMinus -> whenConfigVisible (bumpRiverCarveMaxDepth (-0.05))
          ; Just WidgetConfigRiverCarveMaxDepthPlus -> whenConfigVisible (bumpRiverCarveMaxDepth 0.05)
          ; Just WidgetConfigCoastalErodeStrengthMinus -> whenConfigVisible (bumpCoastalErodeStrength (-0.05))
          ; Just WidgetConfigCoastalErodeStrengthPlus -> whenConfigVisible (bumpCoastalErodeStrength 0.05)
          ; Just WidgetConfigHydroHardnessWeightMinus -> whenConfigVisible (bumpHydroHardnessWeight (-0.05))
          ; Just WidgetConfigHydroHardnessWeightPlus -> whenConfigVisible (bumpHydroHardnessWeight 0.05)
          ; Just WidgetConfigMinLakeSizeMinus -> whenConfigVisible (bumpMinLakeSize (-0.05))
          ; Just WidgetConfigMinLakeSizePlus -> whenConfigVisible (bumpMinLakeSize 0.05)
          ; Just WidgetConfigInlandSeaMinSizeMinus -> whenConfigVisible (bumpInlandSeaMinSize (-0.05))
          ; Just WidgetConfigInlandSeaMinSizePlus -> whenConfigVisible (bumpInlandSeaMinSize 0.05)
          ; Just WidgetConfigRoughnessScaleMinus -> whenConfigVisible (bumpRoughnessScale (-0.05))
          ; Just WidgetConfigRoughnessScalePlus -> whenConfigVisible (bumpRoughnessScale 0.05)
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
          ; Just WidgetConfigOccWarmScaleMinus -> whenConfigVisible (bumpOccWarmScale (-0.05))
          ; Just WidgetConfigOccWarmScalePlus -> whenConfigVisible (bumpOccWarmScale 0.05)
          ; Just WidgetConfigOccColdScaleMinus -> whenConfigVisible (bumpOccColdScale (-0.05))
          ; Just WidgetConfigOccColdScalePlus -> whenConfigVisible (bumpOccColdScale 0.05)
          ; Just WidgetConfigOccLatPeakDegMinus -> whenConfigVisible (bumpOccLatPeakDeg (-0.05))
          ; Just WidgetConfigOccLatPeakDegPlus -> whenConfigVisible (bumpOccLatPeakDeg 0.05)
          ; Just WidgetConfigOccLatWidthDegMinus -> whenConfigVisible (bumpOccLatWidthDeg (-0.05))
          ; Just WidgetConfigOccLatWidthDegPlus -> whenConfigVisible (bumpOccLatWidthDeg 0.05)
          ; Just WidgetConfigSliceLatCenterMinus -> whenConfigVisible (bumpSliceLatCenter (-0.05))
          ; Just WidgetConfigSliceLatCenterPlus -> whenConfigVisible (bumpSliceLatCenter 0.05)
          ; Just WidgetConfigSliceLonCenterMinus -> whenConfigVisible (bumpSliceLonCenter (-0.05))
          ; Just WidgetConfigSliceLonCenterPlus -> whenConfigVisible (bumpSliceLonCenter 0.05)
          ; Just WidgetConfigLatitudeExponentMinus -> whenConfigVisible (bumpLatitudeExponent (-0.05))
          ; Just WidgetConfigLatitudeExponentPlus -> whenConfigVisible (bumpLatitudeExponent 0.05)
          ; Just WidgetConfigPlateHeightCoolingMinus -> whenConfigVisible (bumpPlateHeightCooling (-0.05))
          ; Just WidgetConfigPlateHeightCoolingPlus -> whenConfigVisible (bumpPlateHeightCooling 0.05)
          ; Just WidgetConfigTempNoiseScaleMinus -> whenConfigVisible (bumpTempNoiseScale (-0.05))
          ; Just WidgetConfigTempNoiseScalePlus -> whenConfigVisible (bumpTempNoiseScale 0.05)
          ; Just WidgetConfigOceanModerationMinus -> whenConfigVisible (bumpOceanModeration (-0.05))
          ; Just WidgetConfigOceanModerationPlus -> whenConfigVisible (bumpOceanModeration 0.05)
          ; Just WidgetConfigOceanModerateTempMinus -> whenConfigVisible (bumpOceanModerateTemp (-0.05))
          ; Just WidgetConfigOceanModerateTempPlus -> whenConfigVisible (bumpOceanModerateTemp 0.05)
          ; Just WidgetConfigAlbedoSensitivityMinus -> whenConfigVisible (bumpAlbedoSensitivity (-0.05))
          ; Just WidgetConfigAlbedoSensitivityPlus -> whenConfigVisible (bumpAlbedoSensitivity 0.05)
          ; Just WidgetConfigAlbedoReferenceMinus -> whenConfigVisible (bumpAlbedoReference (-0.05))
          ; Just WidgetConfigAlbedoReferencePlus -> whenConfigVisible (bumpAlbedoReference 0.05)
          ; Just WidgetConfigMoistAdvectMinus -> whenConfigVisible (bumpMoistAdvect (-0.05))
          ; Just WidgetConfigMoistAdvectPlus -> whenConfigVisible (bumpMoistAdvect 0.05)
          ; Just WidgetConfigMoistLocalMinus -> whenConfigVisible (bumpMoistLocal (-0.05))
          ; Just WidgetConfigMoistLocalPlus -> whenConfigVisible (bumpMoistLocal 0.05)
          ; Just WidgetConfigMoistWindEvapScaleMinus -> whenConfigVisible (bumpMoistWindEvapScale (-0.05))
          ; Just WidgetConfigMoistWindEvapScalePlus -> whenConfigVisible (bumpMoistWindEvapScale 0.05)
          ; Just WidgetConfigMoistEvapNoiseScaleMinus -> whenConfigVisible (bumpMoistEvapNoiseScale (-0.05))
          ; Just WidgetConfigMoistEvapNoiseScalePlus -> whenConfigVisible (bumpMoistEvapNoiseScale 0.05)
          ; Just WidgetConfigMoistLandETCoeffMinus -> whenConfigVisible (bumpMoistLandETCoeff (-0.05))
          ; Just WidgetConfigMoistLandETCoeffPlus -> whenConfigVisible (bumpMoistLandETCoeff 0.05)
          ; Just WidgetConfigMoistBareEvapFracMinus -> whenConfigVisible (bumpMoistBareEvapFrac (-0.05))
          ; Just WidgetConfigMoistBareEvapFracPlus -> whenConfigVisible (bumpMoistBareEvapFrac 0.05)
          ; Just WidgetConfigMoistVegTranspFracMinus -> whenConfigVisible (bumpMoistVegTranspFrac (-0.05))
          ; Just WidgetConfigMoistVegTranspFracPlus -> whenConfigVisible (bumpMoistVegTranspFrac 0.05)
          ; Just WidgetConfigMoistWindETScaleMinus -> whenConfigVisible (bumpMoistWindETScale (-0.05))
          ; Just WidgetConfigMoistWindETScalePlus -> whenConfigVisible (bumpMoistWindETScale 0.05)
          ; Just WidgetConfigMoistCondensationRateMinus -> whenConfigVisible (bumpMoistCondensationRate (-0.05))
          ; Just WidgetConfigMoistCondensationRatePlus -> whenConfigVisible (bumpMoistCondensationRate 0.05)
          ; Just WidgetConfigMoistRecycleRateMinus -> whenConfigVisible (bumpMoistRecycleRate (-0.05))
          ; Just WidgetConfigMoistRecycleRatePlus -> whenConfigVisible (bumpMoistRecycleRate 0.05)
          ; Just WidgetConfigMoistITCZStrengthMinus -> whenConfigVisible (bumpMoistITCZStrength (-0.05))
          ; Just WidgetConfigMoistITCZStrengthPlus -> whenConfigVisible (bumpMoistITCZStrength 0.05)
          ; Just WidgetConfigMoistITCZWidthMinus -> whenConfigVisible (bumpMoistITCZWidth (-0.05))
          ; Just WidgetConfigMoistITCZWidthPlus -> whenConfigVisible (bumpMoistITCZWidth 0.05)
          ; Just WidgetConfigOrographicScaleMinus -> whenConfigVisible (bumpOrographicScale (-0.05))
          ; Just WidgetConfigOrographicScalePlus -> whenConfigVisible (bumpOrographicScale 0.05)
          ; Just WidgetConfigOrographicStepMinus -> whenConfigVisible (bumpOrographicStep (-0.05))
          ; Just WidgetConfigOrographicStepPlus -> whenConfigVisible (bumpOrographicStep 0.05)
          ; Just WidgetConfigCoastalIterationsMinus -> whenConfigVisible (bumpCoastalIterations (-0.05))
          ; Just WidgetConfigCoastalIterationsPlus -> whenConfigVisible (bumpCoastalIterations 0.05)
          ; Just WidgetConfigCoastalDiffuseMinus -> whenConfigVisible (bumpCoastalDiffuse (-0.05))
          ; Just WidgetConfigCoastalDiffusePlus -> whenConfigVisible (bumpCoastalDiffuse 0.05)
          ; Just WidgetConfigCoastalMoistureBoostMinus -> whenConfigVisible (bumpCoastalMoistureBoost (-0.05))
          ; Just WidgetConfigCoastalMoistureBoostPlus -> whenConfigVisible (bumpCoastalMoistureBoost 0.05)
          ; Just WidgetConfigWindBeltStrengthMinus -> whenConfigVisible (bumpWindBeltStrength (-0.05))
          ; Just WidgetConfigWindBeltStrengthPlus -> whenConfigVisible (bumpWindBeltStrength 0.05)
          ; Just WidgetConfigWindBeltHarmonicsMinus -> whenConfigVisible (bumpWindBeltHarmonics (-0.05))
          ; Just WidgetConfigWindBeltHarmonicsPlus -> whenConfigVisible (bumpWindBeltHarmonics 0.05)
          ; Just WidgetConfigWindBeltBaseMinus -> whenConfigVisible (bumpWindBeltBase (-0.05))
          ; Just WidgetConfigWindBeltBasePlus -> whenConfigVisible (bumpWindBeltBase 0.05)
          ; Just WidgetConfigWindBeltRangeMinus -> whenConfigVisible (bumpWindBeltRange (-0.05))
          ; Just WidgetConfigWindBeltRangePlus -> whenConfigVisible (bumpWindBeltRange 0.05)
          ; Just WidgetConfigWindBeltSpeedScaleMinus -> whenConfigVisible (bumpWindBeltSpeedScale (-0.05))
          ; Just WidgetConfigWindBeltSpeedScalePlus -> whenConfigVisible (bumpWindBeltSpeedScale 0.05)
          ; Just WidgetConfigBndLandRangeMinus -> whenConfigVisible (bumpBndLandRange (-0.05))
          ; Just WidgetConfigBndLandRangePlus -> whenConfigVisible (bumpBndLandRange 0.05)
          ; Just WidgetConfigBndTempConvergentMinus -> whenConfigVisible (bumpBndTempConvergent (-0.05))
          ; Just WidgetConfigBndTempConvergentPlus -> whenConfigVisible (bumpBndTempConvergent 0.05)
          ; Just WidgetConfigBndTempDivergentMinus -> whenConfigVisible (bumpBndTempDivergent (-0.05))
          ; Just WidgetConfigBndTempDivergentPlus -> whenConfigVisible (bumpBndTempDivergent 0.05)
          ; Just WidgetConfigBndTempTransformMinus -> whenConfigVisible (bumpBndTempTransform (-0.05))
          ; Just WidgetConfigBndTempTransformPlus -> whenConfigVisible (bumpBndTempTransform 0.05)
          ; Just WidgetConfigBndPrecipConvergentMinus -> whenConfigVisible (bumpBndPrecipConvergent (-0.05))
          ; Just WidgetConfigBndPrecipConvergentPlus -> whenConfigVisible (bumpBndPrecipConvergent 0.05)
          ; Just WidgetConfigBndPrecipDivergentMinus -> whenConfigVisible (bumpBndPrecipDivergent (-0.05))
          ; Just WidgetConfigBndPrecipDivergentPlus -> whenConfigVisible (bumpBndPrecipDivergent 0.05)
          ; Just WidgetConfigBndPrecipTransformMinus -> whenConfigVisible (bumpBndPrecipTransform (-0.05))
          ; Just WidgetConfigBndPrecipTransformPlus -> whenConfigVisible (bumpBndPrecipTransform 0.05)
          ; Just WidgetConfigWeatherTickMinus -> whenConfigVisible (bumpWeatherTick (-0.05))
          ; Just WidgetConfigWeatherTickPlus -> whenConfigVisible (bumpWeatherTick 0.05)
          ; Just WidgetConfigWeatherPhaseMinus -> whenConfigVisible (bumpWeatherPhase (-0.05))
          ; Just WidgetConfigWeatherPhasePlus -> whenConfigVisible (bumpWeatherPhase 0.05)
          ; Just WidgetConfigWeatherAmplitudeMinus -> whenConfigVisible (bumpWeatherAmplitude (-0.05))
          ; Just WidgetConfigWeatherAmplitudePlus -> whenConfigVisible (bumpWeatherAmplitude 0.05)
          ; Just WidgetConfigSeasonCycleLengthMinus -> whenConfigVisible (bumpSeasonCycleLength (-0.05))
          ; Just WidgetConfigSeasonCycleLengthPlus -> whenConfigVisible (bumpSeasonCycleLength 0.05)
          ; Just WidgetConfigJitterAmplitudeMinus -> whenConfigVisible (bumpJitterAmplitude (-0.05))
          ; Just WidgetConfigJitterAmplitudePlus -> whenConfigVisible (bumpJitterAmplitude 0.05)
          ; Just WidgetConfigPressureBaseMinus -> whenConfigVisible (bumpPressureBase (-0.05))
          ; Just WidgetConfigPressureBasePlus -> whenConfigVisible (bumpPressureBase 0.05)
          ; Just WidgetConfigPressureTempScaleMinus -> whenConfigVisible (bumpPressureTempScale (-0.05))
          ; Just WidgetConfigPressureTempScalePlus -> whenConfigVisible (bumpPressureTempScale 0.05)
          ; Just WidgetConfigPressureCoriolisScaleMinus -> whenConfigVisible (bumpPressureCoriolisScale (-0.05))
          ; Just WidgetConfigPressureCoriolisScalePlus -> whenConfigVisible (bumpPressureCoriolisScale 0.05)
          ; Just WidgetConfigSeasonalBaseMinus -> whenConfigVisible (bumpSeasonalBase (-0.05))
          ; Just WidgetConfigSeasonalBasePlus -> whenConfigVisible (bumpSeasonalBase 0.05)
          ; Just WidgetConfigSeasonalRangeMinus -> whenConfigVisible (bumpSeasonalRange (-0.05))
          ; Just WidgetConfigSeasonalRangePlus -> whenConfigVisible (bumpSeasonalRange 0.05)
          ; Just WidgetConfigHumidityNoiseScaleMinus -> whenConfigVisible (bumpHumidityNoiseScale (-0.05))
          ; Just WidgetConfigHumidityNoiseScalePlus -> whenConfigVisible (bumpHumidityNoiseScale 0.05)
          ; Just WidgetConfigPrecipNoiseScaleMinus -> whenConfigVisible (bumpPrecipNoiseScale (-0.05))
          ; Just WidgetConfigPrecipNoiseScalePlus -> whenConfigVisible (bumpPrecipNoiseScale 0.05)
          ; Just WidgetConfigWeatherITCZWidthMinus -> whenConfigVisible (bumpWeatherITCZWidth (-0.05))
          ; Just WidgetConfigWeatherITCZWidthPlus -> whenConfigVisible (bumpWeatherITCZWidth 0.05)
          ; Just WidgetConfigWeatherITCZPrecipBoostMinus -> whenConfigVisible (bumpWeatherITCZPrecipBoost (-0.05))
          ; Just WidgetConfigWeatherITCZPrecipBoostPlus -> whenConfigVisible (bumpWeatherITCZPrecipBoost 0.05)
          ; Just WidgetConfigPressureHumidityScaleMinus -> whenConfigVisible (bumpPressureHumidityScale (-0.05))
          ; Just WidgetConfigPressureHumidityScalePlus -> whenConfigVisible (bumpPressureHumidityScale 0.05)
          ; Just WidgetConfigPressureGradientWindScaleMinus -> whenConfigVisible (bumpPressureGradientWindScale (-0.05))
          ; Just WidgetConfigPressureGradientWindScalePlus -> whenConfigVisible (bumpPressureGradientWindScale 0.05)
          ; Just WidgetConfigWindNoiseScaleMinus -> whenConfigVisible (bumpWindNoiseScale (-0.05))
          ; Just WidgetConfigWindNoiseScalePlus -> whenConfigVisible (bumpWindNoiseScale 0.05)
          ; Just WidgetConfigITCZMigrationScaleMinus -> whenConfigVisible (bumpITCZMigrationScale (-0.05))
          ; Just WidgetConfigITCZMigrationScalePlus -> whenConfigVisible (bumpITCZMigrationScale 0.05)
          ; Just WidgetConfigCloudRHExponentMinus -> whenConfigVisible (bumpCloudRHExponent (-0.05))
          ; Just WidgetConfigCloudRHExponentPlus -> whenConfigVisible (bumpCloudRHExponent 0.05)
          ; Just WidgetConfigCloudAlbedoEffectMinus -> whenConfigVisible (bumpCloudAlbedoEffect (-0.05))
          ; Just WidgetConfigCloudAlbedoEffectPlus -> whenConfigVisible (bumpCloudAlbedoEffect 0.05)
          ; Just WidgetConfigCloudPrecipBoostMinus -> whenConfigVisible (bumpCloudPrecipBoost (-0.05))
          ; Just WidgetConfigCloudPrecipBoostPlus -> whenConfigVisible (bumpCloudPrecipBoost 0.05)
          ; Just WidgetConfigVegBaseMinus -> whenConfigVisible (bumpVegBase (-0.05))
          ; Just WidgetConfigVegBasePlus -> whenConfigVisible (bumpVegBase 0.05)
          ; Just WidgetConfigVegBoostMinus -> whenConfigVisible (bumpVegBoost (-0.05))
          ; Just WidgetConfigVegBoostPlus -> whenConfigVisible (bumpVegBoost 0.05)
          ; Just WidgetConfigVegTempWeightMinus -> whenConfigVisible (bumpVegTempWeight (-0.05))
          ; Just WidgetConfigVegTempWeightPlus -> whenConfigVisible (bumpVegTempWeight 0.05)
          ; Just WidgetConfigVegPrecipWeightMinus -> whenConfigVisible (bumpVegPrecipWeight (-0.05))
          ; Just WidgetConfigVegPrecipWeightPlus -> whenConfigVisible (bumpVegPrecipWeight 0.05)
          ; Just WidgetConfigBtCoastalBandMinus -> whenConfigVisible (bumpBtCoastalBand (-0.05))
          ; Just WidgetConfigBtCoastalBandPlus -> whenConfigVisible (bumpBtCoastalBand 0.05)
          ; Just WidgetConfigBtSnowElevationMinus -> whenConfigVisible (bumpBtSnowElevation (-0.05))
          ; Just WidgetConfigBtSnowElevationPlus -> whenConfigVisible (bumpBtSnowElevation 0.05)
          ; Just WidgetConfigBtAlpineElevationMinus -> whenConfigVisible (bumpBtAlpineElevation (-0.05))
          ; Just WidgetConfigBtAlpineElevationPlus -> whenConfigVisible (bumpBtAlpineElevation 0.05)
          ; Just WidgetConfigBtIceCapTempMinus -> whenConfigVisible (bumpBtIceCapTemp (-0.05))
          ; Just WidgetConfigBtIceCapTempPlus -> whenConfigVisible (bumpBtIceCapTemp 0.05)
          ; Just WidgetConfigBtMontaneLowMinus -> whenConfigVisible (bumpBtMontaneLow (-0.05))
          ; Just WidgetConfigBtMontaneLowPlus -> whenConfigVisible (bumpBtMontaneLow 0.05)
          ; Just WidgetConfigBtMontanePrecipMinus -> whenConfigVisible (bumpBtMontanePrecip (-0.05))
          ; Just WidgetConfigBtMontanePrecipPlus -> whenConfigVisible (bumpBtMontanePrecip 0.05)
          ; Just WidgetConfigBtCliffSlopeMinus -> whenConfigVisible (bumpBtCliffSlope (-0.05))
          ; Just WidgetConfigBtCliffSlopePlus -> whenConfigVisible (bumpBtCliffSlope 0.05)
          ; Just WidgetConfigBtValleyMoistureMinus -> whenConfigVisible (bumpBtValleyMoisture (-0.05))
          ; Just WidgetConfigBtValleyMoisturePlus -> whenConfigVisible (bumpBtValleyMoisture 0.05)
          ; Just WidgetConfigBtDepressionMoistureMinus -> whenConfigVisible (bumpBtDepressionMoisture (-0.05))
          ; Just WidgetConfigBtDepressionMoisturePlus -> whenConfigVisible (bumpBtDepressionMoisture 0.05)
          ; Just WidgetConfigBtPrecipWeightMinus -> whenConfigVisible (bumpBtPrecipWeight (-0.05))
          ; Just WidgetConfigBtPrecipWeightPlus -> whenConfigVisible (bumpBtPrecipWeight 0.05)
          ; Just WidgetConfigVbcTempMinMinus -> whenConfigVisible (bumpVbcTempMin (-0.05))
          ; Just WidgetConfigVbcTempMinPlus -> whenConfigVisible (bumpVbcTempMin 0.05)
          ; Just WidgetConfigVbcTempRangeMinus -> whenConfigVisible (bumpVbcTempRange (-0.05))
          ; Just WidgetConfigVbcTempRangePlus -> whenConfigVisible (bumpVbcTempRange 0.05)
          ; Just WidgetConfigVbcFertilityBoostMinus -> whenConfigVisible (bumpVbcFertilityBoost (-0.05))
          ; Just WidgetConfigVbcFertilityBoostPlus -> whenConfigVisible (bumpVbcFertilityBoost 0.05)
          ; Just WidgetConfigVbcAlbedoBaseMinus -> whenConfigVisible (bumpVbcAlbedoBase (-0.05))
          ; Just WidgetConfigVbcAlbedoBasePlus -> whenConfigVisible (bumpVbcAlbedoBase 0.05)
          ; Just WidgetConfigVbcAlbedoBareMinus -> whenConfigVisible (bumpVbcAlbedoBare (-0.05))
          ; Just WidgetConfigVbcAlbedoBarePlus -> whenConfigVisible (bumpVbcAlbedoBare 0.05)
          ; Just WidgetConfigVbcAlbedoVegMinus -> whenConfigVisible (bumpVbcAlbedoVeg (-0.05))
          ; Just WidgetConfigVbcAlbedoVegPlus -> whenConfigVisible (bumpVbcAlbedoVeg 0.05)
          ; Just WidgetConfigVbcOceanAlbedoMinus -> whenConfigVisible (bumpVbcOceanAlbedo (-0.05))
          ; Just WidgetConfigVbcOceanAlbedoPlus -> whenConfigVisible (bumpVbcOceanAlbedo 0.05)
          ; Just WidgetConfigVbcIceAlbedoMinus -> whenConfigVisible (bumpVbcIceAlbedo (-0.05))
          ; Just WidgetConfigVbcIceAlbedoPlus -> whenConfigVisible (bumpVbcIceAlbedo 0.05)
          ; Just WidgetConfigBiomeSmoothingMinus -> whenConfigVisible (bumpBiomeSmoothing (-0.05))
          ; Just WidgetConfigBiomeSmoothingPlus -> whenConfigVisible (bumpBiomeSmoothing 0.05)
          ; Just WidgetConfigVolcanicAshBoostMinus -> whenConfigVisible (bumpVolcanicAshBoost (-0.05))
          ; Just WidgetConfigVolcanicAshBoostPlus -> whenConfigVisible (bumpVolcanicAshBoost 0.05)
          ; Just WidgetConfigVolcanicLavaPenaltyMinus -> whenConfigVisible (bumpVolcanicLavaPenalty (-0.05))
          ; Just WidgetConfigVolcanicLavaPenaltyPlus -> whenConfigVisible (bumpVolcanicLavaPenalty 0.05)
          ; Just WidgetConfigBiomeFeedbackBlendMinus -> whenConfigVisible (bumpBiomeFeedbackBlend (-0.05))
          ; Just WidgetConfigBiomeFeedbackBlendPlus -> whenConfigVisible (bumpBiomeFeedbackBlend 0.05)
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
    bumpTfcCliffSlope delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiTfcCliffSlope uiHandle (uiTfcCliffSlope uiSnap + delta)
    bumpTfcMountainSlope delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiTfcMountainSlope uiHandle (uiTfcMountainSlope uiSnap + delta)
    bumpTfcMountainRelief delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiTfcMountainRelief uiHandle (uiTfcMountainRelief uiSnap + delta)
    bumpTfcHillSlope delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiTfcHillSlope uiHandle (uiTfcHillSlope uiSnap + delta)
    bumpTfcRollingSlope delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiTfcRollingSlope uiHandle (uiTfcRollingSlope uiSnap + delta)
    bumpValleyCurvature delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiValleyCurvature uiHandle (uiValleyCurvature uiSnap + delta)
    bumpRockElevationThreshold delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiRockElevationThreshold uiHandle (uiRockElevationThreshold uiSnap + delta)
    bumpRockHardnessThreshold delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiRockHardnessThreshold uiHandle (uiRockHardnessThreshold uiSnap + delta)
    bumpRockHardnessSecondary delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiRockHardnessSecondary uiHandle (uiRockHardnessSecondary uiSnap + delta)
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
    bumpOccWarmScale delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiOccWarmScale uiHandle (uiOccWarmScale uiSnap + delta)
    bumpOccColdScale delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiOccColdScale uiHandle (uiOccColdScale uiSnap + delta)
    bumpOccLatPeakDeg delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiOccLatPeakDeg uiHandle (uiOccLatPeakDeg uiSnap + delta)
    bumpOccLatWidthDeg delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiOccLatWidthDeg uiHandle (uiOccLatWidthDeg uiSnap + delta)
    bumpSliceLatCenter delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiSliceLatCenter uiHandle (uiSliceLatCenter uiSnap + delta)
    bumpSliceLonCenter delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiSliceLonCenter uiHandle (uiSliceLonCenter uiSnap + delta)
    bumpLatitudeExponent delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiLatitudeExponent uiHandle (uiLatitudeExponent uiSnap + delta)
    bumpPlateHeightCooling delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiPlateHeightCooling uiHandle (uiPlateHeightCooling uiSnap + delta)
    bumpTempNoiseScale delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiTempNoiseScale uiHandle (uiTempNoiseScale uiSnap + delta)
    bumpOceanModeration delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiOceanModeration uiHandle (uiOceanModeration uiSnap + delta)
    bumpOceanModerateTemp delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiOceanModerateTemp uiHandle (uiOceanModerateTemp uiSnap + delta)
    bumpAlbedoSensitivity delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiAlbedoSensitivity uiHandle (uiAlbedoSensitivity uiSnap + delta)
    bumpAlbedoReference delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiAlbedoReference uiHandle (uiAlbedoReference uiSnap + delta)
    bumpMoistAdvect delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiMoistAdvect uiHandle (uiMoistAdvect uiSnap + delta)
    bumpMoistLocal delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiMoistLocal uiHandle (uiMoistLocal uiSnap + delta)
    bumpMoistWindEvapScale delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiMoistWindEvapScale uiHandle (uiMoistWindEvapScale uiSnap + delta)
    bumpMoistEvapNoiseScale delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiMoistEvapNoiseScale uiHandle (uiMoistEvapNoiseScale uiSnap + delta)
    bumpMoistLandETCoeff delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiMoistLandETCoeff uiHandle (uiMoistLandETCoeff uiSnap + delta)
    bumpMoistBareEvapFrac delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiMoistBareEvapFrac uiHandle (uiMoistBareEvapFrac uiSnap + delta)
    bumpMoistVegTranspFrac delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiMoistVegTranspFrac uiHandle (uiMoistVegTranspFrac uiSnap + delta)
    bumpMoistWindETScale delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiMoistWindETScale uiHandle (uiMoistWindETScale uiSnap + delta)
    bumpMoistCondensationRate delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiMoistCondensationRate uiHandle (uiMoistCondensationRate uiSnap + delta)
    bumpMoistRecycleRate delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiMoistRecycleRate uiHandle (uiMoistRecycleRate uiSnap + delta)
    bumpMoistITCZStrength delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiMoistITCZStrength uiHandle (uiMoistITCZStrength uiSnap + delta)
    bumpMoistITCZWidth delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiMoistITCZWidth uiHandle (uiMoistITCZWidth uiSnap + delta)
    bumpOrographicScale delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiOrographicScale uiHandle (uiOrographicScale uiSnap + delta)
    bumpOrographicStep delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiOrographicStep uiHandle (uiOrographicStep uiSnap + delta)
    bumpCoastalIterations delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiCoastalIterations uiHandle (uiCoastalIterations uiSnap + delta)
    bumpCoastalDiffuse delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiCoastalDiffuse uiHandle (uiCoastalDiffuse uiSnap + delta)
    bumpCoastalMoistureBoost delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiCoastalMoistureBoost uiHandle (uiCoastalMoistureBoost uiSnap + delta)
    bumpWindBeltStrength delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiWindBeltStrength uiHandle (uiWindBeltStrength uiSnap + delta)
    bumpWindBeltHarmonics delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiWindBeltHarmonics uiHandle (uiWindBeltHarmonics uiSnap + delta)
    bumpWindBeltBase delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiWindBeltBase uiHandle (uiWindBeltBase uiSnap + delta)
    bumpWindBeltRange delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiWindBeltRange uiHandle (uiWindBeltRange uiSnap + delta)
    bumpWindBeltSpeedScale delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiWindBeltSpeedScale uiHandle (uiWindBeltSpeedScale uiSnap + delta)
    bumpBndLandRange delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiBndLandRange uiHandle (uiBndLandRange uiSnap + delta)
    bumpBndTempConvergent delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiBndTempConvergent uiHandle (uiBndTempConvergent uiSnap + delta)
    bumpBndTempDivergent delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiBndTempDivergent uiHandle (uiBndTempDivergent uiSnap + delta)
    bumpBndTempTransform delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiBndTempTransform uiHandle (uiBndTempTransform uiSnap + delta)
    bumpBndPrecipConvergent delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiBndPrecipConvergent uiHandle (uiBndPrecipConvergent uiSnap + delta)
    bumpBndPrecipDivergent delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiBndPrecipDivergent uiHandle (uiBndPrecipDivergent uiSnap + delta)
    bumpBndPrecipTransform delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiBndPrecipTransform uiHandle (uiBndPrecipTransform uiSnap + delta)
    bumpWeatherTick delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiWeatherTick uiHandle (uiWeatherTick uiSnap + delta)
    bumpWeatherPhase delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiWeatherPhase uiHandle (uiWeatherPhase uiSnap + delta)
    bumpWeatherAmplitude delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiWeatherAmplitude uiHandle (uiWeatherAmplitude uiSnap + delta)
    bumpSeasonCycleLength delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiSeasonCycleLength uiHandle (uiSeasonCycleLength uiSnap + delta)
    bumpJitterAmplitude delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiJitterAmplitude uiHandle (uiJitterAmplitude uiSnap + delta)
    bumpPressureBase delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiPressureBase uiHandle (uiPressureBase uiSnap + delta)
    bumpPressureTempScale delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiPressureTempScale uiHandle (uiPressureTempScale uiSnap + delta)
    bumpPressureCoriolisScale delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiPressureCoriolisScale uiHandle (uiPressureCoriolisScale uiSnap + delta)
    bumpSeasonalBase delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiSeasonalBase uiHandle (uiSeasonalBase uiSnap + delta)
    bumpSeasonalRange delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiSeasonalRange uiHandle (uiSeasonalRange uiSnap + delta)
    bumpHumidityNoiseScale delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiHumidityNoiseScale uiHandle (uiHumidityNoiseScale uiSnap + delta)
    bumpPrecipNoiseScale delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiPrecipNoiseScale uiHandle (uiPrecipNoiseScale uiSnap + delta)
    bumpWeatherITCZWidth delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiWeatherITCZWidth uiHandle (uiWeatherITCZWidth uiSnap + delta)
    bumpWeatherITCZPrecipBoost delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiWeatherITCZPrecipBoost uiHandle (uiWeatherITCZPrecipBoost uiSnap + delta)
    bumpPressureHumidityScale delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiPressureHumidityScale uiHandle (uiPressureHumidityScale uiSnap + delta)
    bumpPressureGradientWindScale delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiPressureGradientWindScale uiHandle (uiPressureGradientWindScale uiSnap + delta)
    bumpWindNoiseScale delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiWindNoiseScale uiHandle (uiWindNoiseScale uiSnap + delta)
    bumpITCZMigrationScale delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiITCZMigrationScale uiHandle (uiITCZMigrationScale uiSnap + delta)
    bumpCloudRHExponent delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiCloudRHExponent uiHandle (uiCloudRHExponent uiSnap + delta)
    bumpCloudAlbedoEffect delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiCloudAlbedoEffect uiHandle (uiCloudAlbedoEffect uiSnap + delta)
    bumpCloudPrecipBoost delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiCloudPrecipBoost uiHandle (uiCloudPrecipBoost uiSnap + delta)
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
    bumpBtCoastalBand delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiBtCoastalBand uiHandle (uiBtCoastalBand uiSnap + delta)
    bumpBtSnowElevation delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiBtSnowElevation uiHandle (uiBtSnowElevation uiSnap + delta)
    bumpBtAlpineElevation delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiBtAlpineElevation uiHandle (uiBtAlpineElevation uiSnap + delta)
    bumpBtIceCapTemp delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiBtIceCapTemp uiHandle (uiBtIceCapTemp uiSnap + delta)
    bumpBtMontaneLow delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiBtMontaneLow uiHandle (uiBtMontaneLow uiSnap + delta)
    bumpBtMontanePrecip delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiBtMontanePrecip uiHandle (uiBtMontanePrecip uiSnap + delta)
    bumpBtCliffSlope delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiBtCliffSlope uiHandle (uiBtCliffSlope uiSnap + delta)
    bumpBtValleyMoisture delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiBtValleyMoisture uiHandle (uiBtValleyMoisture uiSnap + delta)
    bumpBtDepressionMoisture delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiBtDepressionMoisture uiHandle (uiBtDepressionMoisture uiSnap + delta)
    bumpBtPrecipWeight delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiBtPrecipWeight uiHandle (uiBtPrecipWeight uiSnap + delta)
    bumpVbcTempMin delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiVbcTempMin uiHandle (uiVbcTempMin uiSnap + delta)
    bumpVbcTempRange delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiVbcTempRange uiHandle (uiVbcTempRange uiSnap + delta)
    bumpVbcFertilityBoost delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiVbcFertilityBoost uiHandle (uiVbcFertilityBoost uiSnap + delta)
    bumpVbcAlbedoBase delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiVbcAlbedoBase uiHandle (uiVbcAlbedoBase uiSnap + delta)
    bumpVbcAlbedoBare delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiVbcAlbedoBare uiHandle (uiVbcAlbedoBare uiSnap + delta)
    bumpVbcAlbedoVeg delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiVbcAlbedoVeg uiHandle (uiVbcAlbedoVeg uiSnap + delta)
    bumpVbcOceanAlbedo delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiVbcOceanAlbedo uiHandle (uiVbcOceanAlbedo uiSnap + delta)
    bumpVbcIceAlbedo delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiVbcIceAlbedo uiHandle (uiVbcIceAlbedo uiSnap + delta)
    bumpBiomeSmoothing delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiBiomeSmoothing uiHandle (uiBiomeSmoothing uiSnap + delta)
    bumpVolcanicAshBoost delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiVolcanicAshBoost uiHandle (uiVolcanicAshBoost uiSnap + delta)
    bumpVolcanicLavaPenalty delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiVolcanicLavaPenalty uiHandle (uiVolcanicLavaPenalty uiSnap + delta)
    bumpBiomeFeedbackBlend delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiBiomeFeedbackBlend uiHandle (uiBiomeFeedbackBlend uiSnap + delta)
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
    bumpGlacierSnowTemp delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiGlacierSnowTemp uiHandle (uiGlacierSnowTemp uiSnap + delta)
    bumpGlacierSnowRange delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiGlacierSnowRange uiHandle (uiGlacierSnowRange uiSnap + delta)
    bumpGlacierMeltTemp delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiGlacierMeltTemp uiHandle (uiGlacierMeltTemp uiSnap + delta)
    bumpGlacierMeltRate delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiGlacierMeltRate uiHandle (uiGlacierMeltRate uiSnap + delta)
    bumpGlacierAccumScale delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiGlacierAccumScale uiHandle (uiGlacierAccumScale uiSnap + delta)
    bumpGlacierFlowIters delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiGlacierFlowIters uiHandle (uiGlacierFlowIters uiSnap + delta)
    bumpGlacierFlowRate delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiGlacierFlowRate uiHandle (uiGlacierFlowRate uiSnap + delta)
    bumpGlacierErosionScale delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiGlacierErosionScale uiHandle (uiGlacierErosionScale uiSnap + delta)
    bumpGlacierCarveScale delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiGlacierCarveScale uiHandle (uiGlacierCarveScale uiSnap + delta)
    bumpGlacierDepositScale delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiGlacierDepositScale uiHandle (uiGlacierDepositScale uiSnap + delta)
    bumpVentDensity delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiVentDensity uiHandle (uiVentDensity uiSnap + delta)
    bumpVentThreshold delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiVentThreshold uiHandle (uiVentThreshold uiSnap + delta)
    bumpHotspotScale delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiHotspotScale uiHandle (uiHotspotScale uiSnap + delta)
    bumpHotspotThreshold delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiHotspotThreshold uiHandle (uiHotspotThreshold uiSnap + delta)
    bumpMagmaRecharge delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiMagmaRecharge uiHandle (uiMagmaRecharge uiSnap + delta)
    bumpLavaScale delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiLavaScale uiHandle (uiLavaScale uiSnap + delta)
    bumpAshScale delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiAshScale uiHandle (uiAshScale uiSnap + delta)
    bumpVolcanicDepositScale delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiVolcanicDepositScale uiHandle (uiVolcanicDepositScale uiSnap + delta)
    bumpSoilMoistureThreshold delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiSoilMoistureThreshold uiHandle (uiSoilMoistureThreshold uiSnap + delta)
    bumpSoilHardnessThreshold delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiSoilHardnessThreshold uiHandle (uiSoilHardnessThreshold uiSnap + delta)
    bumpSoilFertilityMoistWeight delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiSoilFertilityMoistWeight uiHandle (uiSoilFertilityMoistWeight uiSnap + delta)
    bumpSoilFertilityDepthWeight delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiSoilFertilityDepthWeight uiHandle (uiSoilFertilityDepthWeight uiSnap + delta)
    bumpSinkBreachDepth delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiSinkBreachDepth uiHandle (uiSinkBreachDepth uiSnap + delta)
    bumpStreamPowerMaxErosion delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiStreamPowerMaxErosion uiHandle (uiStreamPowerMaxErosion uiSnap + delta)
    bumpRiverCarveMaxDepth delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiRiverCarveMaxDepth uiHandle (uiRiverCarveMaxDepth uiSnap + delta)
    bumpCoastalErodeStrength delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiCoastalErodeStrength uiHandle (uiCoastalErodeStrength uiSnap + delta)
    bumpHydroHardnessWeight delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiHydroHardnessWeight uiHandle (uiHydroHardnessWeight uiSnap + delta)
    bumpMinLakeSize delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiMinLakeSize uiHandle (uiMinLakeSize uiSnap + delta)
    bumpInlandSeaMinSize delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiInlandSeaMinSize uiHandle (uiInlandSeaMinSize uiSnap + delta)
    bumpRoughnessScale delta = do
      uiSnap <- getUiSnapshot uiHandle
      setUiRoughnessScale uiHandle (uiRoughnessScale uiSnap + delta)
