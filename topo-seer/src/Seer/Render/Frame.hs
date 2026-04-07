module Seer.Render.Frame
  ( RenderContext(..)
  , renderFrame
  ) where

import Actor.AtlasResultBroker (AtlasResultRef)
import Actor.AtlasScheduleBroker (AtlasScheduleRef)
import Actor.AtlasScheduler (AtlasScheduler)
import Actor.Data (DataSnapshot(..), TerrainSnapshot(..))
import Actor.Log (Log, LogEntry(..), LogLevel(..), appendLog)
import Actor.Log (LogSnapshot(..))
import Actor.Render (RenderSnapshot(..))
import Actor.SnapshotReceiver (SnapshotVersion)
import Actor.UI (LeftTab(..), UiState(..), ViewMode(..))
import Control.Monad (forM_, when)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (isNothing)
import Data.Word (Word32, Word64, Word8)
import GHC.Clock (getMonotonicTimeNSec)
import System.IO (Handle, hPutStrLn, hFlush)
import Hyperspace.Actor (ActorHandle, Protocol)
import Linear (V2(..), V4(..))
import qualified SDL
import qualified Data.Text as Text
import Seer.Draw
  ( drawChunkControl
  , drawConfigPanel
  , drawDataDetailPopover
  , drawHoverHex
  , drawLeftTabs
  , drawSeedControl
  , drawStatusBars
  , drawDayNightToggle
  , drawOverlayButtons
  , drawViewModeButtons
  , seedMaxDigits
  , viewColor
  )
import Actor.AtlasCache (AtlasKey(..))
import Seer.Render.Atlas
  ( AtlasTextureCache(..)
  , drawAtlas
  , drawAtlasAlpha
  , drainAtlasBuildResults
  , getNearestAtlas
  , getNearestDayNight
  , resolveAtlasTiles
  , resolveEffectiveStage
  , scheduleAtlasBuilds
  , setAtlasKey
  , zoomTextureScale
  )
import Seer.Render.ZoomStage (ZoomStage(..), stageForZoom)
import Seer.Render.Context (RenderContext(..))
import Seer.Render.Terrain
  ( TerrainCache(..)
  , updateChunkTextures
  , drawTerrain
  )
import Seer.Render.Ui (drawUiOverlay)
import UI.Theme
import Seer.Screenshot (serviceScreenshotRequest)
import Seer.Timing (nsToMs, timedMs)
import Seer.Editor.Preview (drawBrushPreview)
import Seer.Editor.Toolbar (drawEditorToolbar, drawEditorReopenButton)
import Seer.Editor.Types (EditorState(..))
import UI.Font (FontCache)
import UI.Layout
import UI.TerrainCache (ChunkTextureCache(..), emptyChunkTextureCache)
import UI.TerrainAtlas (TerrainAtlasTile(..))
import UI.Widgets (Rect(..))
import UI.WidgetsDraw (rectToSDL)

-- | Render one UI frame and schedule atlas work if needed.
renderFrame
  :: RenderContext
  -> IO (Bool, ChunkTextureCache, AtlasTextureCache, Bool)
renderFrame context = do
  let renderer = rcRenderer context
      window = rcWindow context
      snapshotVersion = rcSnapshotVersion context
      snapshot = rcSnapshot context
      terrainCache = rcTerrainCache context
      textureCache = rcChunkTextureCache context
      atlasCache = rcAtlasTextureCache context
      logHandle = rcLogHandle context
      atlasSchedulerHandle = rcAtlasSchedulerHandle context
      scheduleRef = rcAtlasScheduleRef context
      resultRef = rcAtlasResultRef context
      atlasUploadsPerFrame = rcAtlasUploadsPerFrame context
      shouldDrainAtlas = rcShouldDrainAtlas context
      shouldScheduleAtlas = rcShouldScheduleAtlas context
      shouldUpdateChunkTextures = rcShouldUpdateChunkTextures context
      timingLogThresholdMs = rcTimingLogThresholdMs context
      fontCache = rcFontCache context
      renderTargetOk = rcRenderTargetOk context
      traceH = rcTraceHandle context
      pool = rcTexturePool context
  tStart <- getMonotonicTimeNSec
  SDL.rendererDrawBlendMode renderer SDL.$= SDL.BlendAlphaBlend
  ((V2 winW winH), windowSizeElapsed) <- timedMs (SDL.get (SDL.windowSize window))
  loggedWindowSize <- logTiming logHandle timingLogThresholdMs (Text.pack "window size") windowSizeElapsed Nothing
  let UiState { uiViewMode = mode } = rsUi snapshot
      logSnap = rsLog snapshot
      dataSnap = rsData snapshot
      terrainSnap = rsTerrain snapshot
      generating = uiGenerating (rsUi snapshot)
      (r, g, b) = viewColor mode (dsTerrainChunks dataSnap) (dsBiomeChunks dataSnap)
      logHeight = if lsCollapsed logSnap then 24 else 160
      seedWidth = max 120 (seedMaxDigits * 10)
      layout = layoutForSeed (V2 (fromIntegral winW) (fromIntegral winH)) logHeight seedWidth
      Rect (V2 _ panelY, V2 _ panelH) = logPanelRect layout
      buttonRect = leftGenButtonRect layout
      configToggle = configToggleRect layout
      configPanel = configPanelRect layout
      (tabTerrain, tabPlanet, tabClimate, tabWeather, tabBiome, tabErosion, tabPipeline, tabData) = configTabRects layout
      configPresetSave = configPresetSaveRect layout
      configPresetLoad = configPresetLoadRect layout
      configReset = configResetRect layout
      configRevert = configRevertRect layout
      configScrollArea = configScrollAreaRect layout
      configScrollBar = configScrollBarRect layout
      leftPanel = leftPanelRect layout
      leftToggle = leftToggleRect layout
      (leftTabTopo, leftTabView) = leftTabRects layout
      seedLabel = configSeedLabelRect layout
      seedValue = configSeedValueRect layout
      seedRandom = configSeedRandomRect layout
      chunkMinus = leftChunkMinusRect layout
      chunkPlus = leftChunkPlusRect layout
      chunkValue = configChunkValueRect layout
      logFilters = logFilterRects layout
      viewRects = leftViewRects layout
      buttonLabel = if uiGenerating (rsUi snapshot) then V4 120 120 120 255 else V4 80 160 240 255
  tAfterLet <- getMonotonicTimeNSec
  SDL.rendererDrawColor renderer SDL.$= V4 r g b 255
  SDL.clear renderer
  tAfterClear <- getMonotonicTimeNSec
  let rawStage = stageForZoom (uiZoom (rsUi snapshot))
      (stage, mbBlend, atlasCacheWithStage) = resolveEffectiveStage tAfterClear rawStage atlasCache
      -- Synchronise the render-thread cache key with the current UI state
      -- BEFORE draining results.  This ensures stale worker results (from a
      -- superseded view mode) are discarded rather than thrashing the key.
      expectedAtlasKey = AtlasKey mode (uiRenderWaterLevel (rsUi snapshot)) (tsVersion terrainSnap)
      atlasCacheKeyed = setAtlasKey expectedAtlasKey atlasCacheWithStage
      dataReady = tsChunkSize terrainSnap > 0 && not (IntMap.null (tsTerrainChunks terrainSnap))
  (loggedSchedule, loggedScheduleDrain, loggedScheduleEnqueue) <-
    if shouldScheduleAtlas
      then do
        (jobCount, drainMs, enqueueMs) <- scheduleAtlasBuilds renderTargetOk dataReady atlasSchedulerHandle scheduleRef snapshotVersion snapshot (fromIntegral winW, fromIntegral winH)
        let totalMs = drainMs + enqueueMs
        totalLogged <- logTiming logHandle timingLogThresholdMs (Text.pack "atlas schedule") totalMs (Just jobCount)
        drainLogged <- logTiming logHandle timingLogThresholdMs (Text.pack "atlas schedule drain") drainMs (Just jobCount)
        enqueueLogged <- logTiming logHandle timingLogThresholdMs (Text.pack "atlas schedule enqueue") enqueueMs (Just jobCount)
        pure (totalLogged, drainLogged, enqueueLogged)
      else pure (False, False, False)
  tAfterSchedule <- getMonotonicTimeNSec
  (atlasCache', uploadCount, uploadMs, uploadTextureMs) <- if shouldDrainAtlas
    then do
      ((cache', count, createMs), elapsed) <- timedMs $ do
        (cacheNext, count, createMs) <- drainAtlasBuildResults renderTargetOk atlasUploadsPerFrame pool renderer atlasCacheKeyed resultRef
        pure (cacheNext, count, createMs)
      pure (cache', count, elapsed, createMs)
    else pure (atlasCacheKeyed, 0, 0, 0)
  loggedUpload <-
    if shouldDrainAtlas && uploadCount > 0
      then logTiming logHandle timingLogThresholdMs (Text.pack "atlas upload") uploadMs (Just uploadCount)
      else pure False
  loggedTextureCreate <-
    if shouldDrainAtlas && uploadTextureMs >= timingLogThresholdMs
      then logTiming logHandle timingLogThresholdMs (Text.pack "atlas texture create") uploadTextureMs (Just uploadCount)
      else pure False
  tAfterDrain <- getMonotonicTimeNSec
  (atlasToDraw, atlasKeyMismatch, atlasCache'', loggedAtlasResolve) <- do
    ((resolvedTiles, mismatch, resolvedCache), elapsed) <- timedMs (resolveAtlasTiles renderTargetOk pool snapshot atlasCache' stage)
    logged <- logTiming logHandle timingLogThresholdMs (Text.pack "atlas resolve") elapsed Nothing
    pure (resolvedTiles, mismatch, resolvedCache, logged)
  tAfterResolve <- getMonotonicTimeNSec
  (textureCache', loggedChunkTexture) <-
    if renderTargetOk
      then pure (emptyChunkTextureCache, False)
      else if shouldUpdateChunkTextures
        then do
          (updatedCache, elapsed) <- timedMs (updateChunkTextures renderer terrainCache (zsAtlasScale stage) textureCache)
          logged <- logTiming logHandle timingLogThresholdMs (Text.pack "chunk texture build") elapsed Nothing
          pure (updatedCache, logged)
        else pure (textureCache, False)
  loggedDraw <- case atlasToDraw of
    Just tiles -> do
      (_, elapsed) <- timedMs $ case mbBlend of
        Just (targetStage, blend) | blend > 0 -> do
          -- Cross-fade: draw committed tiles at reduced alpha, target tiles on top
          let oldAlpha = round ((1.0 - blend) * 255) :: Word8
              newAlpha = round (blend * 255) :: Word8
              targetTiles = getNearestAtlas expectedAtlasKey (zsHexRadius targetStage) atlasCache''
              pan = uiPanOffset (rsUi snapshot)
              z = uiZoom (rsUi snapshot)
              win = V2 (fromIntegral winW) (fromIntegral winH)
          drawAtlasAlpha renderer tiles pan z win oldAlpha
          case targetTiles of
            Just tt | not (null tt) -> drawAtlasAlpha renderer tt pan z win newAlpha
            _ -> pure ()  -- target not yet cached; committed tiles still visible
        _ ->
          drawAtlas renderer tiles (uiPanOffset (rsUi snapshot)) (uiZoom (rsUi snapshot)) (V2 (fromIntegral winW) (fromIntegral winH))
      logTiming logHandle timingLogThresholdMs (Text.pack "draw atlas") elapsed Nothing
    Nothing -> do
      (_, elapsed) <- timedMs (drawTerrain renderer terrainSnap terrainCache textureCache' (uiPanOffset (rsUi snapshot)) (uiZoom (rsUi snapshot)) (V2 (fromIntegral winW) (fromIntegral winH)))
      logTiming logHandle timingLogThresholdMs (Text.pack "draw terrain") elapsed Nothing
  tAfterDraw <- getMonotonicTimeNSec
  -- Draw day/night overlay on top of base atlas when enabled.
  when (uiDayNightEnabled (rsUi snapshot)) $ do
    let dnTiles = getNearestDayNight (zsHexRadius stage) atlasCache''
    case dnTiles of
      Just dt | not (null dt) -> do
        -- Set alpha blending on each day/night texture so black+alpha
        -- composites correctly over the base atlas.
        forM_ dt $ \tile ->
          SDL.textureBlendMode (tatTexture tile) SDL.$= SDL.BlendAlphaBlend
        drawAtlas renderer dt (uiPanOffset (rsUi snapshot)) (uiZoom (rsUi snapshot)) (V2 (fromIntegral winW) (fromIntegral winH))
      _ -> pure ()
  let hexHoverRadius = zsHexRadius stage
  loggedHover <- do
    (_, elapsed) <- timedMs (drawHoverHex renderer (rsUi snapshot) hexHoverRadius)
    logTiming logHandle timingLogThresholdMs (Text.pack "draw hover") elapsed Nothing
  -- Brush preview overlay (between terrain and UI chrome)
  when (editorActive (uiEditor (rsUi snapshot))) $
    drawBrushPreview renderer (rsUi snapshot) hexHoverRadius
  loggedChrome <- do
    (_, elapsed) <- timedMs $ do
      let configColor = if uiShowConfig (rsUi snapshot) then V4 140 160 200 255 else V4 100 120 160 255
      SDL.rendererDrawColor renderer SDL.$= configColor
      SDL.fillRect renderer (Just (rectToSDL configToggle))
      let leftColor = if uiShowLeftPanel (rsUi snapshot) then V4 140 160 200 255 else V4 100 120 160 255
      SDL.rendererDrawColor renderer SDL.$= leftColor
      SDL.fillRect renderer (Just (rectToSDL leftToggle))
      when (uiShowLeftPanel (rsUi snapshot)) $ do
        SDL.rendererDrawColor renderer SDL.$= colConfigPanel
        SDL.fillRect renderer (Just (rectToSDL leftPanel))
        drawLeftTabs renderer (rsUi snapshot) (leftTabTopo, leftTabView)
        case uiLeftTab (rsUi snapshot) of
          LeftTopo -> do
            drawChunkControl renderer (rsUi snapshot) chunkMinus chunkValue chunkPlus
            drawSeedControl renderer (rsUi snapshot) seedValue seedRandom
            SDL.rendererDrawColor renderer SDL.$= buttonLabel
            SDL.fillRect renderer (Just (rectToSDL buttonRect))
            drawStatusBars renderer fontCache (rsUi snapshot) dataSnap layout
          LeftView -> do
            let scrollY = uiLeftViewScroll (rsUi snapshot)
                Rect (V2 lpx _, V2 lpw _) = leftPanel
                Rect (V2 _ lpy, V2 _ lpH) = leftPanel
                ctop = leftControlsTop layout
                clipR = SDL.Rectangle (SDL.P (V2 (fromIntegral lpx) (fromIntegral ctop)))
                                      (V2 (fromIntegral lpw) (fromIntegral (lpy + lpH - ctop)))
                shiftY dy (Rect (V2 rx ry, V2 rw rh)) = Rect (V2 rx (ry - dy), V2 rw rh)
                scrolledViewRects = map (shiftY scrollY) viewRects
                (op, on, fp, fn) = overlayViewRects layout
                scrolledOR = (shiftY scrollY op, shiftY scrollY on, shiftY scrollY fp, shiftY scrollY fn)
            SDL.rendererClipRect renderer SDL.$= Just clipR
            drawViewModeButtons renderer mode scrolledViewRects
            drawDayNightToggle renderer (uiDayNightEnabled (rsUi snapshot)) (shiftY scrollY (dayNightToggleRect layout))
            drawOverlayButtons renderer fontCache (rsUi snapshot) scrolledOR
            SDL.rendererClipRect renderer SDL.$= Nothing
      drawConfigPanel renderer fontCache (rsUi snapshot) dataSnap layout
      -- Record detail popover (floats over config panel).
      case fontCache of
        Just fc -> when (uiShowConfig (rsUi snapshot)) $
          drawDataDetailPopover renderer fc (rsUi snapshot) layout
        Nothing -> pure ()
      -- Editor toolbar (drawn above config panel, on top of chrome)
      if editorActive (uiEditor (rsUi snapshot))
        then drawEditorToolbar renderer fontCache (rsUi snapshot) layout
        else drawEditorReopenButton renderer fontCache layout
    logTiming logHandle timingLogThresholdMs (Text.pack "draw chrome") elapsed Nothing
  tAfterChrome <- getMonotonicTimeNSec
  loggedUi <- do
    (_, elapsed) <- timedMs (drawUiOverlay renderer fontCache snapshot terrainSnap layout logFilters (V2 (fromIntegral winW) (fromIntegral winH)))
    logTiming logHandle timingLogThresholdMs (Text.pack "draw ui") elapsed Nothing
  tAfterUi <- getMonotonicTimeNSec
  -- Service any pending screenshot request before presenting.
  -- This reads the back-buffer while all drawing is complete.
  serviceScreenshotRequest (rcScreenshotRef context) renderer (fromIntegral winW) (fromIntegral winH)
  loggedPresent <- do
    (_, elapsed) <- timedMs (SDL.present renderer)
    logTiming logHandle timingLogThresholdMs (Text.pack "present") elapsed Nothing
  tEnd <- getMonotonicTimeNSec
  let totalMs = nsToMs tStart tEnd
  when (totalMs >= 100) $ forM_ traceH $ \h -> do
    hPutStrLn h $ "  FRAME total=" <> show totalMs
      <> " winSize=" <> show windowSizeElapsed
      <> " let=" <> show (nsToMs tStart tAfterLet)
      <> " clear=" <> show (nsToMs tAfterLet tAfterClear)
      <> " sched=" <> show (nsToMs tAfterClear tAfterSchedule)
      <> " drain=" <> show (nsToMs tAfterSchedule tAfterDrain)
      <> " resolve=" <> show (nsToMs tAfterDrain tAfterResolve)
      <> " draw=" <> show (nsToMs tAfterResolve tAfterDraw)
      <> " hover=" <> show (nsToMs tAfterDraw tAfterDraw)
      <> " chrome=" <> show (nsToMs tAfterDraw tAfterChrome)
      <> " ui=" <> show (nsToMs tAfterChrome tAfterUi)
      <> " present=" <> show (nsToMs tAfterUi tEnd)
      <> " rtOk=" <> show renderTargetOk
      <> " dataReady=" <> show dataReady
      <> " atlas=" <> show (isNothing atlasToDraw)
    hFlush h
  let didLog = loggedWindowSize || loggedSchedule || loggedScheduleDrain || loggedScheduleEnqueue || loggedUpload || loggedTextureCreate || loggedAtlasResolve || loggedChunkTexture || loggedDraw || loggedHover || loggedChrome || loggedUi || loggedPresent
  pure (renderTargetOk && dataReady && (isNothing atlasToDraw || atlasKeyMismatch), textureCache', atlasCache'', didLog)

logTiming :: ActorHandle Log (Protocol Log) -> Word32 -> Text.Text -> Word32 -> Maybe Int -> IO Bool
logTiming handle thresholdMs label elapsed maybeCount =
  if elapsed >= thresholdMs
    then do
      let countText = case maybeCount of
            Nothing -> ""
            Just count -> " (count=" <> show count <> ")"
          message = Text.pack (Text.unpack label <> " took " <> show elapsed <> "ms" <> countText)
      appendLog handle (LogEntry LogInfo message)
      pure True
    else pure False
