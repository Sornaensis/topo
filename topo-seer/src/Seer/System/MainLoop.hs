{-# LANGUAGE OverloadedStrings #-}

module Seer.System.MainLoop
  ( runMainLoop
  , shouldSkipUnchangedFrame
  ) where

import Actor.AtlasManager (AtlasManagerQueueState, atlasManagerQueuedState, emptyAtlasManagerQueueState)
import Actor.AtlasResultBroker (atlasResultsPendingCount)
import Actor.Log (LogEntry(..), LogLevel(..), appendLog)
import Actor.Render (RenderSnapshot(..))
import Actor.SnapshotReceiver (SnapshotVersion(..))
import Actor.UI (UiState(..))
import Control.Monad (forM_, when)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Text as Text
import Data.Word (Word32)
import GHC.Clock (getMonotonicTimeNSec)
import qualified SDL
import Seer.Config.Runtime (TopoSeerConfig(..))
import Seer.Draw (logLineHeight)
import Seer.System.Actors (AppActors(..))
import Seer.System.Cache
  ( RenderCacheState(..)
  , initialRenderCacheState
  , renderStepSummary
  )
import Seer.System.EventPump (EventPumpEnv(..), hasQuitEvent, processEvents)
import Seer.System.RenderFrame
  ( RenderFrameEnv(..)
  , RenderFrameSettings(..)
  , RenderFrameMaintenanceDiagnostics(..)
  , RenderFrameStepResult(..)
  , renderFrameStep
  , renderFrameStepMaintenance
  )
import Seer.System.Screenshot (screenshotRequestPending)
import Seer.System.Sdl (SdlResources(..))
import Seer.System.Snapshot (SnapshotPollEnv(..), pollRenderSnapshot)
import Seer.Timing (nsToMs)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.IO (IOMode(..), hFlush, hPutStrLn, openFile)

emptyAtlasQueueStateForMainLoop :: AtlasManagerQueueState
emptyAtlasQueueStateForMainLoop = emptyAtlasManagerQueueState

-- | Decide whether an unchanged frame can sleep. Queued screenshots always
-- force one render even when the world snapshot itself has not changed.
shouldSkipUnchangedFrame :: Bool -> Bool -> Bool -> Bool -> Bool
shouldSkipUnchangedFrame unchanged generating screenshotPending maintenanceDue =
  unchanged && not generating && not screenshotPending && not maintenanceDue

renderMaintenanceWakeSummary :: Int -> RenderFrameMaintenanceDiagnostics -> String
renderMaintenanceWakeSummary atlasPendingCount diag =
  "render maintenance wake: pendingAtlasResults=" <> show atlasPendingCount
    <> " queuedAtlasJobs=" <> show (rfmdAtlasQueuedCount diag)
    <> " queuedAtlasRev=" <> maybe "none" show (rfmdAtlasQueuedRevision diag)
    <> " queuedStaleDrops=" <> show (rfmdAtlasQueuedStaleDrops diag)
    <> " queuedDuplicateDrops=" <> show (rfmdAtlasQueuedDuplicateDrops diag)
    <> " queuedLatestWinsPrunes=" <> show (rfmdAtlasQueuedLatestWinsPrunes diag)
    <> " scheduleDeferred=" <> show (rfmdAtlasScheduleDeferred diag)
    <> " scheduleStaleDropped=" <> show (rfmdAtlasScheduleDroppedStale diag)
    <> " workerCapacity=" <> show (rfmdAtlasWorkerCapacity diag)
    <> " workerInFlight=" <> show (rfmdAtlasWorkerInFlight diag)
    <> " pendingAtlasWake=" <> show (rfmdAtlasPendingWake diag)
    <> " queuedAtlasWake=" <> show (rfmdAtlasQueuedWake diag)
    <> " scheduleRetryWake=" <> show (rfmdAtlasScheduleRetryWake diag)
    <> " dayNightOverlayRetryWake=" <> show (rfmdAtlasDayNightOverlayRetryWake diag)
    <> " refreshViewportAtlas=" <> show (rfmdAtlasRefreshViewport diag)
    <> " fallbackTerrainWake=" <> show (rfmdFallbackTerrainWake diag)
    <> " drainAttempted=" <> show (rfmdDrainAttempted diag)
    <> " scheduleAttempted=" <> show (rfmdScheduleAttempted diag)

runMainLoop :: TopoSeerConfig -> AppActors -> SdlResources -> IO RenderCacheState
runMainLoop runtimeCfg actors sdl = do
  quitRef <- newIORef False
  lineHeightRef <- logLineHeight (srFontCache sdl) >>= newIORef
  mousePosRef <- newIORef (0, 0)
  dragRef <- newIORef Nothing
  tooltipHoverRef <- newIORef Nothing
  lastSnapshotChangeNs <- getMonotonicTimeNSec >>= newIORef
  staleLoggedRef <- newIORef False
  home <- getHomeDirectory
  traceH <- if cfgRenderTraceEnabled runtimeCfg
    then do
      h <- openFile (home </> ".topo" </> "RENDER_TRACE.txt") WriteMode
      hPutStrLn h "=== render trace start ==="
      hFlush h
      pure (Just h)
    else pure Nothing

  let frameDelayMs         = cfgFrameDelayMs runtimeCfg
      snapshotPollMs       = cfgSnapshotPollMs runtimeCfg
      timingLogThresholdMs = fromIntegral (cfgTimingLogThresholdMs runtimeCfg) :: Word32
      renderTraceEnabled   = cfgRenderTraceEnabled runtimeCfg
      initialCacheState    = initialRenderCacheState (cfgAtlasCacheEntries runtimeCfg)
      eventPumpEnv = EventPumpEnv
        { epeWindow = srWindow sdl
        , epeActorHandles = aaActorHandles actors
        , epeUiActionsHandle = aaUiActionsHandle actors
        , epeUiSnapshotRef = aaUiSnapshotRef actors
        , epeScreenshotRef = aaScreenshotRef actors
        , epeScreenshotStoragePolicy = aaScreenshotStoragePolicy actors
        , epeLogSnapshotRef = Just (aaLogSnapshotRef actors)
        , epeQuitRef = quitRef
        , epeLineHeightRef = lineHeightRef
        , epeMousePosRef = mousePosRef
        , epeDragRef = dragRef
        , epeTooltipHoverRef = tooltipHoverRef
        }
      snapshotEnv = SnapshotPollEnv
        { speLogHandle = aaLogHandle actors
        , speTimingLogThresholdMs = timingLogThresholdMs
        , speSnapshotPollMs = snapshotPollMs
        , speSnapshotVersionRef = aaSnapshotVersionRef actors
        , speDataSnapshotRef = aaDataSnapshotRef actors
        , speTerrainSnapshotRef = aaTerrainSnapshotRef actors
        , speLogSnapshotRef = aaLogSnapshotRef actors
        , speUiSnapshotRef = aaUiSnapshotRef actors
        }
      renderFrameEnv = RenderFrameEnv
        { rfeRenderer = srRenderer sdl
        , rfeWindow = srWindow sdl
        , rfeRenderTargetOk = srRenderTargetOk sdl
        , rfeFontCache = srFontCache sdl
        , rfeTexturePool = srTexturePool sdl
        , rfeLogHandle = aaLogHandle actors
        , rfeTerrainCacheWorkerHandle = aaTerrainCacheWorkerHandle actors
        , rfeTerrainCacheRef = aaTerrainCacheRef actors
        , rfeAtlasSchedulerHandle = aaAtlasSchedulerHandle actors
        , rfeAtlasScheduleRef = aaAtlasScheduleRef actors
        , rfeAtlasResultRef = aaAtlasResultRef actors
        , rfeAtlasFreshnessRef = aaAtlasFreshnessRef actors
        , rfeAtlasQueueRef = aaAtlasManagerQueueRef actors
        , rfeScreenshotRef = aaScreenshotRef actors
        , rfeTraceHandle = traceH
        }
      renderFrameSettings = RenderFrameSettings
        { rfsetAtlasUploadsPerFrame = cfgAtlasUploadsPerFrame runtimeCfg
        , rfsetTerrainCachePollMs = cfgTerrainCachePollMs runtimeCfg
        , rfsetAtlasDrainPollMs = cfgAtlasDrainPollMs runtimeCfg
        , rfsetAtlasSchedulePollMs = cfgAtlasSchedulePollMs runtimeCfg
        , rfsetChunkTexturePollMs = cfgChunkTexturePollMs runtimeCfg
        , rfsetTimingLogThresholdMs = timingLogThresholdMs
        }

      loop cacheState = do
        loopStart <- getMonotonicTimeNSec
        events <- SDL.pollEvents
        tPoll <- getMonotonicTimeNSec
        let eventsElapsed = nsToMs loopStart tPoll
        when (eventsElapsed >= timingLogThresholdMs) $
          appendLog (aaLogHandle actors) (LogEntry LogInfo (Text.pack ("poll events took " <> show eventsElapsed <> "ms")))
        nowMs <- SDL.ticks
        let hasEvents = not (null events)
        (snapVersion, renderSnap, cacheState0, snapshotElapsed) <-
          pollRenderSnapshot snapshotEnv nowMs hasEvents False cacheState
        tSnap <- getMonotonicTimeNSec
        quitFlag <- readIORef quitRef
        let quit = quitFlag || hasQuitEvent events
        handleElapsed <- processEvents eventPumpEnv timingLogThresholdMs events renderSnap
        tHandle <- getMonotonicTimeNSec
        let generating = uiGenerating (rsUi renderSnap)
        atlasPendingCount <- if generating
          then pure 0
          else atlasResultsPendingCount (aaAtlasResultRef actors)
        atlasQueueState <- if generating
          then pure (emptyAtlasQueueStateForMainLoop)
          else atlasManagerQueuedState (aaAtlasManagerQueueRef actors)
        let atlasPending = atlasPendingCount > 0
            isVersionUnchanged = rcsLastSnapshot cacheState0 == Just snapVersion
            maintenanceDiagnostics = renderFrameStepMaintenance
              renderFrameSettings
              (srRenderTargetOk sdl)
              nowMs
              atlasPending
              atlasQueueState
              renderSnap
              cacheState0
            maintenanceDue = rfmdMaintenanceDue maintenanceDiagnostics
        screenshotPending <- screenshotRequestPending (aaScreenshotRef actors)
        if shouldSkipUnchangedFrame
            isVersionUnchanged generating screenshotPending maintenanceDue
          then do
            -- Stale-snapshot detection: log once if version unchanged for >1s.
            now <- getMonotonicTimeNSec
            lastChange <- readIORef lastSnapshotChangeNs
            staleLogged <- readIORef staleLoggedRef
            let staleSec = fromIntegral (now - lastChange) / (1e9 :: Double)
            when (staleSec > 1.0 && not staleLogged) $ do
              writeIORef staleLoggedRef True
              appendLog (aaLogHandle actors) (LogEntry LogInfo (Text.pack
                ("stale snapshot: version unchanged for " <> show (round staleSec :: Int) <> "s (v=" <> show (unSnapshotVersion snapVersion) <> ")")))
            delayStart <- getMonotonicTimeNSec
            SDL.delay (fromIntegral frameDelayMs)
            delayEnd <- getMonotonicTimeNSec
            let delayElapsed = nsToMs delayStart delayEnd
            when renderTraceEnabled $
              appendLog (aaLogHandle actors) (LogEntry LogInfo (Text.pack (renderStepSummary eventsElapsed snapshotElapsed handleElapsed 0 0 delayElapsed True)))
            tEnd <- getMonotonicTimeNSec
            let loopMs = nsToMs loopStart tEnd
            when (loopMs >= 100) $ forM_ traceH $ \h -> do
              hPutStrLn h $ "IDLE loop=" <> show loopMs <> "ms poll=" <> show (nsToMs loopStart tPoll) <> " snap=" <> show (nsToMs tPoll tSnap) <> " handle=" <> show (nsToMs tSnap tHandle) <> " stale=" <> show (nsToMs tHandle tEnd) <> " events=" <> show (length events) <> " v=" <> show (unSnapshotVersion snapVersion)
              hFlush h
            if quit
              then pure cacheState0
              else loop cacheState0
          else do
            -- Snapshot changed or render-thread maintenance is due; reset stale tracking before rendering.
            when (renderTraceEnabled && isVersionUnchanged && maintenanceDue) $
              appendLog (aaLogHandle actors) (LogEntry LogInfo (Text.pack (renderMaintenanceWakeSummary atlasPendingCount maintenanceDiagnostics)))
            forM_ traceH $ \h -> when (isVersionUnchanged && maintenanceDue) $ do
              hPutStrLn h ("WAKE " <> renderMaintenanceWakeSummary atlasPendingCount maintenanceDiagnostics <> " v=" <> show (unSnapshotVersion snapVersion))
              hFlush h
            (frameSnapVersion, frameRenderSnap, frameCacheState0, snapshotElapsedForSummary) <-
              if isVersionUnchanged && rfmdAtlasQueuedWake maintenanceDiagnostics
                then pollRenderSnapshot snapshotEnv nowMs False True cacheState0
                else pure (snapVersion, renderSnap, cacheState0, snapshotElapsed)
            tElseBranch <- getMonotonicTimeNSec
            writeIORef lastSnapshotChangeNs =<< getMonotonicTimeNSec
            writeIORef staleLoggedRef False
            tAfterWrite <- getMonotonicTimeNSec
            frameResult <- renderFrameStep renderFrameEnv renderFrameSettings nowMs frameSnapVersion frameRenderSnap frameCacheState0
            let cacheState' = rfrCacheState frameResult
                terrainElapsed = rfrTerrainElapsed frameResult
                frameElapsed = rfrFrameElapsed frameResult
                letsElapsed = rfrLetsElapsed frameResult
                postFrameElapsed = rfrPostFrameElapsed frameResult
            delayStart <- getMonotonicTimeNSec
            SDL.delay (fromIntegral frameDelayMs)
            delayEnd <- getMonotonicTimeNSec
            let delayElapsed = nsToMs delayStart delayEnd
            tPostDelay <- getMonotonicTimeNSec
            when renderTraceEnabled $
              appendLog (aaLogHandle actors) (LogEntry LogInfo (Text.pack (renderStepSummary eventsElapsed snapshotElapsedForSummary handleElapsed terrainElapsed frameElapsed delayElapsed False)))
            tEnd <- getMonotonicTimeNSec
            let loopMs = nsToMs loopStart tEnd
            when (loopMs >= 100) $ forM_ traceH $ \h -> do
              hPutStrLn h $ "RENDER loop=" <> show loopMs <> "ms poll=" <> show (nsToMs loopStart tPoll) <> " snap=" <> show (nsToMs tPoll tSnap) <> " handle=" <> show (nsToMs tSnap tHandle) <> " branch=" <> show (nsToMs tHandle tElseBranch) <> " write=" <> show (nsToMs tElseBranch tAfterWrite) <> " lets=" <> show letsElapsed <> " tPoll=" <> show terrainElapsed <> " tLet=0 terrain=" <> show terrainElapsed <> " frame=" <> show frameElapsed <> " postFrame=" <> show postFrameElapsed <> " delay=" <> show delayElapsed <> " postDelay=" <> show (nsToMs tPostDelay tEnd) <> " events=" <> show (length events) <> " v=" <> show (unSnapshotVersion frameSnapVersion)
              hFlush h
            if quit
              then pure cacheState'
              else loop cacheState'

  finalState <- loop initialCacheState
  forM_ traceH $ \h -> do
    hPutStrLn h "=== render trace end ==="
    hFlush h
  pure finalState
