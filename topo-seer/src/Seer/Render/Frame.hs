module Seer.Render.Frame
  ( RenderContext(..)
  , RenderFrameOutcome(..)
  , AtlasFrameStepPolicy(..)
  , AtlasQueuedWork(..)
  , noAtlasQueuedWork
  , applyAtlasFrameStepTimestamps
  , atlasFrameStepPolicy
  , fallbackFrameMaintenanceDue
  , renderFrame
  ) where

import Actor.AtlasFreshness (readAtlasFreshnessRef)
import Actor.AtlasManager (AtlasManagerQueueState, formatAtlasManagerQueueState)
import Actor.AtlasResultBroker (AtlasResultDrainStats(..), AtlasResultRef, formatAtlasResultDrainStats)
import Actor.AtlasScheduleBroker (AtlasScheduleRef, AtlasScheduleReport(..), formatAtlasScheduleReport)
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
  , drawOverlayActionButtons
  , drawOverlayButtons
  , drawViewModeButtons
  , seedMaxDigits
  , viewColor
  )
import Actor.AtlasCache (AtlasKey, atlasKeyFor)
import Seer.Render.Atlas
  ( AtlasCacheSummary(..)
  , AtlasResolveStatus
  , AtlasTextureCache(..)
  , AtlasTileSetSummary(..)
  , atlasCacheSummary
  , atlasResolveDiagnosticWithCoverage
  , atlasResolveNeedsRetry
  , dayNightOverlayNeedsRetry
  , drawAtlas
  , drawAtlasAlpha
  , drainAtlasBuildResults
  , formatAtlasCacheSummary
  , formatAtlasResolveDiagnostic
  , getCurrentCompleteAtlasForTargetWithCoverage
  , resolveDayNightOverlayForTarget
  , retireDayNightOverlaysExcept
  , resolveAtlasTiles
  , resolveEffectiveStage
  , scheduleAtlasBuilds
  , setAtlasKey
  , zoomTextureScale
  )
import Seer.Render.ZoomStage (ZoomStage(..), stageForZoom)
import Seer.Render.Context (RenderContext(..))
import Seer.Render.Viewport (AtlasViewportCoverage, currentAtlasViewportCoverage)
import Seer.Render.Terrain
  ( TerrainCache(..)
  , chunkTextureCacheNeedsUpdate
  , fallbackTerrainNeedsRefresh
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
import UI.DayNight (mkDayNightKey)
import UI.TerrainAtlas (TerrainAtlasTile(..))
import UI.Widgets (Rect(..))
import UI.WidgetsDraw (rectToSDL)
import Topo (WorldConfig(..))

data RenderFrameOutcome = RenderFrameOutcome
  { rfoAtlasNeedsRetry :: !Bool
  , rfoFallbackNeedsRetry :: !Bool
  , rfoChunkTextureCache :: !ChunkTextureCache
  , rfoAtlasTextureCache :: !AtlasTextureCache
  , rfoAtlasResolveStatus :: !AtlasResolveStatus
  , rfoAtlasDrainStats :: !(Maybe AtlasResultDrainStats)
  , rfoAtlasScheduleReport :: !(Maybe AtlasScheduleReport)
  , rfoDidLog :: !Bool
  }

-- | Render-loop atlas maintenance decisions shared by the frame step and tests.
data AtlasFrameStepPolicy = AtlasFrameStepPolicy
  { afspShouldDrainAtlas :: !Bool
  , afspShouldScheduleAtlas :: !Bool
  , afspAtlasMaintenanceDue :: !Bool
  } deriving (Eq, Show)

-- | Non-destructive manager-queue signal used to wake unchanged snapshots.
data AtlasQueuedWork = AtlasQueuedWork
  { aqwQueuedForCurrentKey :: !Bool
  , aqwQueueRevision :: !(Maybe Word64)
  , aqwLastScheduledRevision :: !(Maybe Word64)
  } deriving (Eq, Show)

noAtlasQueuedWork :: AtlasQueuedWork
noAtlasQueuedWork = AtlasQueuedWork
  { aqwQueuedForCurrentKey = False
  , aqwQueueRevision = Nothing
  , aqwLastScheduledRevision = Nothing
  }

atlasFrameStepPolicy
  :: Word32
  -> Int
  -> Int
  -> Bool
  -> Bool
  -> Bool
  -> AtlasQueuedWork
  -> Bool
  -> Maybe Word32
  -> Maybe Word32
  -> AtlasFrameStepPolicy
atlasFrameStepPolicy nowMs drainPollMs schedulePollMs generating renderTargetOk atlasPending queuedWork atlasNeedsRetry lastDrain lastSchedule =
  let scheduleDue = atlasPolicyShouldPoll nowMs schedulePollMs lastSchedule
      queuedRevisionNew = aqwQueuedForCurrentKey queuedWork
        && maybe False (\rev -> Just rev /= aqwLastScheduledRevision queuedWork) (aqwQueueRevision queuedWork)
      queuedScheduleDue = aqwQueuedForCurrentKey queuedWork && (queuedRevisionNew || scheduleDue)
  in AtlasFrameStepPolicy
    { afspShouldDrainAtlas = not generating
        && (atlasPending || atlasPolicyShouldPoll nowMs drainPollMs lastDrain)
    , afspShouldScheduleAtlas = not generating && (scheduleDue || queuedRevisionNew)
    , afspAtlasMaintenanceDue = not generating
        && renderTargetOk
        && (atlasPending || queuedScheduleDue || (atlasNeedsRetry && scheduleDue))
    }

applyAtlasFrameStepTimestamps
  :: Word32
  -> AtlasFrameStepPolicy
  -> Maybe Word32
  -> Maybe Word32
  -> (Maybe Word32, Maybe Word32)
applyAtlasFrameStepTimestamps nowMs policy lastDrain lastSchedule =
  ( if afspShouldDrainAtlas policy then Just nowMs else lastDrain
  , if afspShouldScheduleAtlas policy then Just nowMs else lastSchedule
  )

-- | Whether non-render-target fallback terrain maintenance should wake a frame.
fallbackFrameMaintenanceDue
  :: Bool
  -> UiState
  -> TerrainSnapshot
  -> Int
  -> TerrainCache
  -> ChunkTextureCache
  -> Bool
fallbackFrameMaintenanceDue renderTargetOk uiSnap terrainSnap scale terrainCache textureCache =
  not (uiGenerating uiSnap)
    && not renderTargetOk
    && fallbackTerrainNeedsRefresh uiSnap terrainSnap scale terrainCache textureCache

atlasPolicyShouldPoll :: Word32 -> Int -> Maybe Word32 -> Bool
atlasPolicyShouldPoll nowMs pollMs lastPoll =
  case lastPoll of
    Nothing -> True
    Just prev -> nowMs - prev >= fromIntegral pollMs

-- | Render one UI frame and schedule atlas work if needed.
renderFrame
  :: RenderContext
  -> IO RenderFrameOutcome
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
      freshnessRef = rcAtlasFreshnessRef context
      atlasQueueState = rcAtlasQueueState context
      atlasUploadsPerFrame = rcAtlasUploadsPerFrame context
      shouldDrainAtlas = rcShouldDrainAtlas context
      shouldScheduleAtlas = rcShouldScheduleAtlas context
      shouldUpdateChunkTextures = rcShouldUpdateChunkTextures context
      shouldRefreshViewportAtlas = rcShouldRefreshViewportAtlas context
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
      expectedAtlasKey = atlasKeyFor mode (uiRenderWaterLevel (rsUi snapshot)) terrainSnap
      atlasCacheKeyed = setAtlasKey expectedAtlasKey atlasCacheWithStage
      dataReady = tsChunkSize terrainSnap > 0 && not (IntMap.null (tsTerrainChunks terrainSnap))
      windowSize = (fromIntegral winW, fromIntegral winH)
      coverageFor targetStage = if dataReady
        then Just (currentAtlasViewportCoverage (WorldConfig { wcChunkSize = tsChunkSize terrainSnap }) (uiPanOffset (rsUi snapshot)) (uiZoom (rsUi snapshot)) windowSize terrainSnap targetStage)
        else Nothing
  (mbScheduleReport, loggedSchedule, loggedScheduleDrain, loggedScheduleEnqueue) <-
    if shouldScheduleAtlas
      then do
        scheduleReport <- scheduleAtlasBuilds renderTargetOk dataReady shouldRefreshViewportAtlas stage atlasSchedulerHandle scheduleRef snapshotVersion snapshot windowSize
        let jobCount = asrJobCount scheduleReport
            drainMs = asrDrainMs scheduleReport
            enqueueMs = asrEnqueueMs scheduleReport
            totalMs = drainMs + enqueueMs
        totalLogged <- logTiming logHandle timingLogThresholdMs (Text.pack "atlas schedule") totalMs (Just jobCount)
        drainLogged <- logTiming logHandle timingLogThresholdMs (Text.pack "atlas schedule drain") drainMs (Just jobCount)
        enqueueLogged <- logTiming logHandle timingLogThresholdMs (Text.pack "atlas schedule enqueue") enqueueMs (Just jobCount)
        pure (Just scheduleReport, totalLogged, drainLogged, enqueueLogged)
      else pure (Nothing, False, False, False)
  tAfterSchedule <- getMonotonicTimeNSec
  (atlasCache', uploadCount, uploadMs, uploadTextureMs, mbDrainStats) <- if shouldDrainAtlas
    then do
      ((cache', count, createMs, drainStats), elapsed) <- timedMs $ do
        (cacheNext, count, createMs, drainStats) <- drainAtlasBuildResults renderTargetOk atlasUploadsPerFrame pool renderer atlasCacheKeyed resultRef freshnessRef
        pure (cacheNext, count, createMs, drainStats)
      pure (cache', count, elapsed, createMs, Just drainStats)
    else pure (atlasCacheKeyed, 0, 0, 0, Nothing)
  loggedUpload <-
    if shouldDrainAtlas && uploadCount > 0
      then logTiming logHandle timingLogThresholdMs (Text.pack "atlas upload") uploadMs (Just uploadCount)
      else pure False
  loggedTextureCreate <-
    if shouldDrainAtlas && uploadTextureMs >= timingLogThresholdMs
      then logTiming logHandle timingLogThresholdMs (Text.pack "atlas texture create") uploadTextureMs (Just uploadCount)
      else pure False
  tAfterDrain <- getMonotonicTimeNSec
  latestAtlasFreshness <- readAtlasFreshnessRef freshnessRef
  (atlasToDraw, atlasResolveStatus, atlasCache'', loggedAtlasResolve) <- do
    ((resolvedTiles, resolveStatus, resolvedCache), elapsed) <- timedMs (resolveAtlasTiles latestAtlasFreshness renderTargetOk pool snapshot atlasCache' stage windowSize)
    logged <- logTiming logHandle timingLogThresholdMs (Text.pack "atlas resolve") elapsed Nothing
    pure (resolvedTiles, resolveStatus, resolvedCache, logged)
  let currentDayNightKey =
        if uiDayNightEnabled (rsUi snapshot)
          then mkDayNightKey (rsUi snapshot) (tsChunkSize terrainSnap)
          else Nothing
      atlasCacheForOverlay = maybe atlasCache'' (\dnKey -> retireDayNightOverlaysExcept dnKey atlasCache'') currentDayNightKey
  tAfterResolve <- getMonotonicTimeNSec
  let chunkTexturesNeedRefresh = not renderTargetOk
        && chunkTextureCacheNeedsUpdate terrainCache (zsAtlasScale stage) textureCache
      updateFallbackChunkTextures = shouldUpdateChunkTextures || chunkTexturesNeedRefresh
  (textureCache', loggedChunkTexture) <-
    if renderTargetOk
      then pure (emptyChunkTextureCache, False)
      else if updateFallbackChunkTextures
        then do
          (updatedCache, elapsed) <- timedMs (updateChunkTextures renderer terrainCache (zsAtlasScale stage) textureCache)
          logged <- logTiming logHandle timingLogThresholdMs (Text.pack "chunk texture build") elapsed Nothing
          pure (updatedCache, logged)
        else pure (textureCache, False)
  let mbDrawnCrossFadeTarget = case (atlasToDraw, mbBlend) of
        (Just _, Just (targetStage, blend)) | blend > 0 -> do
          targetTiles <- getCurrentCompleteAtlasForTargetWithCoverage latestAtlasFreshness (coverageFor targetStage) expectedAtlasKey (zsHexRadius targetStage) (zsAtlasScale targetStage) atlasCache''
          if null targetTiles
            then Nothing
            else Just (targetStage, blend, targetTiles)
        _ -> Nothing
      (dayNightTiles, dayNightStatus) =
        resolveDayNightOverlayForTarget latestAtlasFreshness renderTargetOk dataReady atlasResolveStatus currentDayNightKey stage atlasCacheForOverlay
      mbTargetDayNight = do
        (targetStage, blend, _) <- mbDrawnCrossFadeTarget
        let (targetTiles, targetStatus) =
              resolveDayNightOverlayForTarget latestAtlasFreshness renderTargetOk dataReady atlasResolveStatus currentDayNightKey targetStage atlasCacheForOverlay
        pure (targetStage, blend, targetTiles, targetStatus)
      dayNightStatuses = dayNightStatus : maybe [] (\(_, _, _, status) -> [status]) mbTargetDayNight
  loggedDraw <- case atlasToDraw of
    Just tiles -> do
      (_, elapsed) <- timedMs $ case mbDrawnCrossFadeTarget of
        Just (_targetStage, blend, targetTiles) -> do
          -- Cross-fade: only reduce committed alpha when exact target tiles are
          -- actually drawn. Without target tiles, draw committed at full opacity
          -- to prevent the viewColor background bleeding through.
          let pan = uiPanOffset (rsUi snapshot)
              z = uiZoom (rsUi snapshot)
              win = V2 (fromIntegral winW) (fromIntegral winH)
              oldAlpha = round ((1.0 - blend) * 255) :: Word8
              newAlpha = round (blend * 255) :: Word8
          drawAtlasAlpha renderer tiles pan z win oldAlpha
          drawAtlasAlpha renderer targetTiles pan z win newAlpha
        Nothing ->
          drawAtlas renderer tiles (uiPanOffset (rsUi snapshot)) (uiZoom (rsUi snapshot)) (V2 (fromIntegral winW) (fromIntegral winH))
      logTiming logHandle timingLogThresholdMs (Text.pack "draw atlas") elapsed Nothing
    Nothing -> do
      (_, elapsed) <- timedMs (drawTerrain renderer terrainSnap terrainCache textureCache' (uiPanOffset (rsUi snapshot)) (uiZoom (rsUi snapshot)) (V2 (fromIntegral winW) (fromIntegral winH)))
      logTiming logHandle timingLogThresholdMs (Text.pack "draw terrain") elapsed Nothing
  tAfterDraw <- getMonotonicTimeNSec
  -- Draw day/night overlay only on top of atlas base tiles.  When atlas tiles
  -- are unavailable, drawTerrain handles the non-render-target fallback overlay.
  case atlasToDraw of
    Just _ -> when (uiDayNightEnabled (rsUi snapshot)) $ do
      let pan = uiPanOffset (rsUi snapshot)
          z = uiZoom (rsUi snapshot)
          win = V2 (fromIntegral winW) (fromIntegral winH)
      case (dayNightTiles, mbTargetDayNight) of
        (Just currentTiles, Just (_targetStage, blend, Just targetTiles, _targetStatus)) -> do
          let oldAlpha = round ((1.0 - blend) * 255) :: Word8
              newAlpha = round (blend * 255) :: Word8
          drawAtlasAlpha renderer currentTiles pan z win oldAlpha
          drawAtlasAlpha renderer targetTiles pan z win newAlpha
        (Just currentTiles, Just (_targetStage, blend, Nothing, _targetStatus)) -> do
          let oldAlpha = round ((1.0 - blend) * 255) :: Word8
          drawAtlasAlpha renderer currentTiles pan z win oldAlpha
        (Just currentTiles, Nothing) ->
          drawAtlas renderer currentTiles pan z win
        (Nothing, Just (_targetStage, blend, Just targetTiles, _targetStatus)) -> do
          let newAlpha = round (blend * 255) :: Word8
          drawAtlasAlpha renderer targetTiles pan z win newAlpha
        _ -> pure ()
    Nothing -> pure ()
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
            drawOverlayActionButtons renderer fontCache (map (shiftY scrollY) (overlayActionRects layout))
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
  loggedAtlasTraceEvents <- traceAtlasFrame
    traceH
    logHandle
    (atcCommittedStage atlasCache)
    (atcCommittedStage atlasCacheWithStage)
    expectedAtlasKey
    stage
    atlasResolveStatus
    atlasToDraw
    atlasCache'
    atlasCacheForOverlay
    shouldDrainAtlas
    shouldScheduleAtlas
    mbScheduleReport
    atlasQueueState
    (coverageFor stage)
    uploadCount
    mbDrainStats
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
      <> " atlasStatus=" <> show atlasResolveStatus
      <> " uploadCount=" <> show uploadCount
      <> " budgetExhausted=" <> maybe "False" (show . ardsBudgetExhausted) mbDrainStats
    hFlush h
  let didLog = loggedWindowSize || loggedSchedule || loggedScheduleDrain || loggedScheduleEnqueue || loggedUpload || loggedTextureCreate || loggedAtlasResolve || loggedAtlasTraceEvents || loggedChunkTexture || loggedDraw || loggedHover || loggedChrome || loggedUi || loggedPresent
      baseAtlasNeedsRetry = renderTargetOk && dataReady && atlasResolveNeedsRetry atlasResolveStatus
      dayNightNeedsRetry = not generating && any dayNightOverlayNeedsRetry dayNightStatuses
      atlasNeedsRetry = baseAtlasNeedsRetry || dayNightNeedsRetry
      fallbackChunkNeedsRetry = not renderTargetOk
        && chunkTextureCacheNeedsUpdate terrainCache (zsAtlasScale stage) textureCache'
  pure RenderFrameOutcome
    { rfoAtlasNeedsRetry = atlasNeedsRetry
    , rfoFallbackNeedsRetry = fallbackChunkNeedsRetry
    , rfoChunkTextureCache = textureCache'
    , rfoAtlasTextureCache = atlasCacheForOverlay
    , rfoAtlasResolveStatus = atlasResolveStatus
    , rfoAtlasDrainStats = mbDrainStats
    , rfoAtlasScheduleReport = mbScheduleReport
    , rfoDidLog = didLog
    }

traceAtlasFrame
  :: Maybe Handle
  -> ActorHandle Log (Protocol Log)
  -> Maybe ZoomStage
  -> Maybe ZoomStage
  -> AtlasKey
  -> ZoomStage
  -> AtlasResolveStatus
  -> Maybe [TerrainAtlasTile]
  -> AtlasTextureCache
  -> AtlasTextureCache
  -> Bool
  -> Bool
  -> Maybe AtlasScheduleReport
  -> AtlasManagerQueueState
  -> Maybe AtlasViewportCoverage
  -> Int
  -> Maybe AtlasResultDrainStats
  -> IO Bool
traceAtlasFrame Nothing _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ = pure False
traceAtlasFrame traceH@(Just h) logHandle oldStage newStage expectedKey stage resolveStatus atlasToDraw cacheBeforeResolve cacheAfterResolve drainAttempted scheduleAttempted mbScheduleReport atlasQueueState requiredCoverage uploadCount mbDrainStats = do
  let resolveDiag = atlasResolveDiagnosticWithCoverage requiredCoverage expectedKey stage resolveStatus atlasToDraw cacheBeforeResolve cacheAfterResolve
      cacheSummaryBeforeResolve = atlasCacheSummary (Just expectedKey) (Just stage) cacheBeforeResolve
      cacheSummary = atlasCacheSummary (Just expectedKey) (Just stage) cacheAfterResolve
  loggedStageChange <- logAtlasStageChange traceH logHandle oldStage newStage (acsLastGood cacheSummary)
  loggedPromotion <- logAtlasPromotion traceH logHandle (acsLastGood cacheSummaryBeforeResolve) (acsLastGood cacheSummary)
  hPutStrLn h $ "  ATLAS drainAttempted=" <> show drainAttempted
    <> " scheduleAttempted=" <> show scheduleAttempted
    <> " scheduleJobs=" <> maybe "0" (show . asrJobCount) mbScheduleReport
    <> " uploadCount=" <> show uploadCount
    <> maybe "" ((" " <>) . formatAtlasScheduleReport) mbScheduleReport
    <> " " <> formatAtlasManagerQueueState atlasQueueState
    <> maybe "" ((" " <>) . formatAtlasResultDrainStats) mbDrainStats
    <> " " <> formatAtlasResolveDiagnostic resolveDiag
    <> " " <> formatAtlasCacheSummary cacheSummary
  hFlush h
  pure (loggedStageChange || loggedPromotion)

logAtlasStageChange
  :: Maybe Handle
  -> ActorHandle Log (Protocol Log)
  -> Maybe ZoomStage
  -> Maybe ZoomStage
  -> Maybe AtlasTileSetSummary
  -> IO Bool
logAtlasStageChange traceH logHandle oldStage newStage currentLast =
  case (oldStage, newStage) of
    (Just old, Just new) | old /= new ->
      logAtlasEvent traceH logHandle $
        "stage-commit old=" <> formatStage old <> " new=" <> formatStage new
          <> " last=" <> formatTileSet currentLast
    _ -> pure False

logAtlasPromotion
  :: Maybe Handle
  -> ActorHandle Log (Protocol Log)
  -> Maybe AtlasTileSetSummary
  -> Maybe AtlasTileSetSummary
  -> IO Bool
logAtlasPromotion traceH logHandle oldLast newLast =
  case newLast of
    Just summary | oldLast /= newLast && atssComplete summary ->
      logAtlasEvent traceH logHandle $
        "last-good-promote old=" <> formatTileSet oldLast <> " new=" <> formatTileSet newLast
    _ -> pure False

logAtlasEvent :: Maybe Handle -> ActorHandle Log (Protocol Log) -> String -> IO Bool
logAtlasEvent Nothing _ _ = pure False
logAtlasEvent (Just h) logHandle message = do
  let line = "atlas " <> message
  hPutStrLn h ("  ATLAS-EVENT " <> message)
  hFlush h
  appendLog logHandle (LogEntry LogInfo (Text.pack line))
  pure True

formatStage :: ZoomStage -> String
formatStage stage = "hex=" <> show (zsHexRadius stage) <> "/scale=" <> show (zsAtlasScale stage)

formatTileSet :: Maybe AtlasTileSetSummary -> String
formatTileSet Nothing = "none"
formatTileSet (Just summary) =
  "key=" <> show (atssKey summary)
    <> "/hex=" <> show (atssHexRadius summary)
    <> "/scale=" <> show (atssAtlasScale summary)
    <> "/build=" <> maybe "unknown" show (atssBuildId summary)
    <> "/tiles=" <> show (atssTileCount summary) <> "/" <> show (atssExpectedTileCount summary)
    <> "/coverageChunks=" <> show (atssCoveredChunkCount summary)

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
