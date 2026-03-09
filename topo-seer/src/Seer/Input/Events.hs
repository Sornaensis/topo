{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Seer.Input.Events
  ( handleEvent
  , TooltipHover
  , tickTooltipHover
  , tooltipDelayFrames
  ) where

import Actor.Data (Data, DataSnapshot(..), DataSnapshotReply, TerrainSnapshot(..), replaceTerrainData, requestDataSnapshot)
import Actor.Log (Log, LogEntry(..), LogLevel(..), LogSnapshot(..), appendLog, setLogCollapsed, setLogMinLevel, setLogScroll)
import Actor.Terrain (Terrain)
import Actor.UI
  ( ConfigTab(..)
  , LeftTab(..)
  , Ui
  , UiMenuMode(..)
  , UiState(..)
  , ViewMode(..)
  , configRowCount
  , setUiChunkSize
  , setUiConfigTab
  , setUiConfigScroll
  , setUiContextHex
  , setUiContextPos
  , setUiHexTooltipPinned
  , setUiErosionHydraulic
  , setUiErosionMaxDrop
  , setUiErosionHydDeposit
  , setUiErosionDepositSlope
  , setUiErosionThermDeposit
  , setUiErosionCoastZone
  , setUiErosionCoastStrength
  , setUiErosionCoastIter
  , setUiHypsometryEnabled
  , setUiHypsometryLowlandExp
  , setUiHypsometryHighlandExp
  , setUiHypsometryPlateauBreak
  , setUiHypsometryOceanExp
  , setUiHypsometryCoastalRampWidth
  , setUiHypsometryCoastalRampStr
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
  , setUiTfcElevGradient
  , setUiTfcPlateauMaxRelief2Ring
  , setUiRockElevationThreshold
  , setUiRockHardnessThreshold
  , setUiRockHardnessSecondary
  , setUiDisabledStages
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
  , setUiMoistBareEvapFrac
  , setUiMoistVegTranspFrac
  , setUiMoistWindETScale
  , setUiMoistCondensationRate
  , setUiMoistRecycleRate
  , setUiMoistITCZStrength
  , setUiMoistITCZWidth
  , setUiMoistMinVegFloor
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
  , setUiWindCoriolisDeflection
  , setUiBndLandRange
  , setUiPiedmontSmooth
  , setUiPiedmontSlopeMin
  , setUiPiedmontSlopeMax
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
  , setUiBtSnowMaxTemp
  , setUiBtAlpineMaxTemp
  , setUiBtIceCapTemp
  , setUiBtMontaneMaxTemp
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
  , setUiOrographicLift
  , setUiRainShadowLoss
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
  , setUiSimAutoTick
  , setUiSimTickCount
  , setUiPluginNames
  , setUiOverlayNames
  )
import Control.Monad (when)
import Data.Int (Int32)
import Data.IORef (IORef, readIORef, writeIORef, modifyIORef')
import Data.List (findIndex, partition)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.IntMap.Strict as IntMap
import Linear (V2(..))
import qualified SDL
import qualified SDL.Raw.Types as Raw
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
  ( ZoomSettings
  , applyZoomAtCursor
  , defaultZoomSettings
  , viewModeForKey
  )
import Topo (ChunkCoord(..), ChunkId(..), TileCoord(..), WorldConfig(..), chunkCoordFromTile, chunkIdFromCoord)
import Topo.Overlay (overlayNames)
import Topo.Pipeline.Dep (builtinDependencies, disabledClosure)
import Topo.Pipeline.Stage (StageId, parseStageId)
import Topo.World (TerrainWorld(..))
import UI.HexPick (screenToAxial)
import UI.Layout
import UI.WidgetTree (Widget(..), WidgetId(..), buildWidgets, buildPluginWidgets, buildSliderRowWidgets, hitTest)
import UI.Widgets (Rect(..), containsPoint)
import System.Random (randomIO)
import Hyperspace.Actor (ActorHandle, Protocol, replyTo)
import Actor.AtlasManager (AtlasManager)
import Actor.PluginManager (PluginManager, setPluginOrder)
import Actor.Simulation (Simulation, requestSimTick, setSimWorld)
import Actor.SnapshotReceiver (SnapshotReceiver)
import Actor.UiActions (UiActions, UiAction(..))
import Seer.Input.Actions (actionRequest, mkInputEnv, submitAction)
import Seer.Input.Widgets (handleClick)

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
  -> ActorHandle PluginManager (Protocol PluginManager)
  -> ActorHandle Simulation (Protocol Simulation)
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
handleEvent window uiHandle logHandle dataHandle terrainHandle atlasManagerHandle uiActionsHandle snapshotReceiverHandle pluginManagerHandle simulationHandle uiSnapCached logSnapCached dataSnapCached terrainSnapCached quitRef lineHeightRef mousePosRef dragRef tooltipHoverRef event = do
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
                   ConfigPipeline -> []
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
              submitAction eventEnv (UiActionRebuildAtlas (uiViewMode uiSnap))
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
              handleClick window uiHandle logHandle dataHandle terrainHandle atlasManagerHandle uiActionsHandle snapshotReceiverHandle pluginManagerHandle simulationHandle uiSnapCached logSnapCached dataSnapCached terrainSnapCached quitRef (SDL.mouseButtonEventPos btnEvent)
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
                    SDL.KeycodeG -> submitAction eventEnv UiActionGenerate
                    SDL.KeycodeC -> toggleConfig
                    SDL.KeycodeUp -> bumpSeed uiHandle (getUiSnapshot uiHandle) 1
                    SDL.KeycodeDown -> bumpSeed uiHandle (getUiSnapshot uiHandle) (-1)
                    SDL.KeycodeL -> do
                      logSnap <- getLogSnapshot logHandle
                      setLogCollapsed logHandle (not (lsCollapsed logSnap))
                    _ ->
                      case viewModeForKey keycode of
                        Just mode -> submitAction eventEnv (UiActionSetViewMode mode)
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
    eventEnv =
      mkInputEnv uiHandle logHandle dataHandle terrainHandle atlasManagerHandle uiActionsHandle snapshotReceiverHandle pluginManagerHandle simulationHandle uiSnapCached logSnapCached dataSnapCached terrainSnapCached
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
            when (not (Text.null name)) $ do
              terrainSnap <- getTerrainSnapshot dataHandle
              let world = snapshotToWorld terrainSnap
              _result <- saveNamedWorld name uiSnap' world
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
                  requestDataSnapshot dataHandle (replyTo @DataSnapshotReply snapshotReceiverHandle)
                  applySnapshotToUi snapshot uiHandle
                  setUiWorldName uiHandle name
                  setUiWorldConfig uiHandle (Just snapshot)
                  submitAction eventEnv (UiActionRebuildAtlas (uiViewMode uiSnap))
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

