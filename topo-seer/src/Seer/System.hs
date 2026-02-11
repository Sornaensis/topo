{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Seer.System
  ( runApp
  ) where

import Actor.Data
  ( Data
  , DataSnapshot(..)
  , TerrainSnapshot(..)
  , dataActorDef
  )
import Actor.Log
  ( Log
  , LogEntry(..)
  , LogLevel(..)
  , LogSnapshot(..)
  , LogSnapshotRef
  , appendLog
  , resetLogFile
  , setLogFileHandle
  , setLogSnapshotRef
  , newLogSnapshotRef
  , readLogSnapshotRef
  , logActorDef
  )
import Actor.Render (RenderSnapshot(..))
import Actor.SnapshotReceiver (SnapshotReceiver, SnapshotVersion(..), snapshotReceiverActorDef, setSnapshotRef)
import Actor.UI
  ( Ui
  , UiSnapshotReply
  , UiState(..)
  , getUiSnapshot
  , requestUiSnapshot
  , setUiSeed
  , setUiSeedInput
  , uiActorDef
  )
import Actor.Terrain (terrainActorDef)
import Actor.AtlasManager (atlasManagerActorDef)
import Actor.AtlasResultBroker (atlasResultBrokerActorDef, setAtlasResultRef)
import Actor.AtlasScheduleBroker (atlasScheduleBrokerActorDef, setAtlasScheduleRef)
import Actor.AtlasScheduler
  ( AtlasSchedulerHandles(..)
  , atlasSchedulerActorDef
  , setAtlasSchedulerHandles
  )
import Actor.AtlasWorker (atlasWorkerActorDef)
import Actor.TerrainCacheBroker
  ( TerrainCacheBroker
  , TerrainCacheRef
  , readTerrainCacheRef
  , setTerrainCacheRef
  , terrainCacheBrokerActorDef
  )
import Actor.TerrainCacheWorker
  ( TerrainCacheBuildRequest(..)
  , TerrainCacheKey
  , TerrainCacheResultReply
  , TerrainCacheWorker
  , requestTerrainCacheBuild
  , tcrResultCache
  , tcrResultKey
  , terrainCacheKeyFrom
  , terrainCacheWorkerActorDef
  )
import Actor.UiActions (uiActionsActorDef, setUiActionsSnapshotRef)
import Control.Monad (forM_, unless, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Text as Text
import Data.Word (Word32, Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Linear (V2(..))
import qualified SDL
import qualified SDL.Font as Font
import Hyperspace.Actor (ActorHandle, Protocol, cast, getSingleton, newActorSystem, replyTo, shutdownActorSystem)
import Seer.Draw (logLineHeight)
import Seer.Input (handleEvent, isQuit, tickTooltipHover)
import Seer.Render
  ( TerrainCache(..)
  , emptyTerrainCache
  , renderFrame
  )
import Seer.Render.Atlas (AtlasTextureCache(..), collectAtlasTextures, emptyAtlasTextureCache)
import UI.TerrainCache (ChunkTextureCache(..), emptyChunkTextureCache)
import UI.Font (destroyFontCache, initFontCacheMaybe)
import qualified Data.IntMap.Strict as IntMap
import UI.TerrainRender (destroyChunkTexture)
import System.Random (randomIO)
import System.IO (Handle, IOMode(..), hFlush, hPutStrLn, openFile)
import Seer.System.ThreadPriority (boostMainThreadPriority, pinMainThreadToCore0)
import Seer.Config.Runtime (TopoSeerConfig(..), loadConfig)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

runApp :: IO ()
runApp = do
  boostMainThreadPriority
  pinMainThreadToCore0
  system <- newActorSystem
  logHandle <- getSingleton system logActorDef
  logFileH <- resetLogFile
  setLogFileHandle logHandle logFileH
  logSnapshotRef <- newLogSnapshotRef
  setLogSnapshotRef logHandle logSnapshotRef
  uiHandle <- getSingleton system uiActorDef
  dataHandle <- getSingleton system dataActorDef
  terrainHandle <- getSingleton system terrainActorDef
  atlasManagerHandle <- getSingleton system atlasManagerActorDef
  atlasWorkerHandle <- getSingleton system atlasWorkerActorDef
  atlasResultBrokerHandle <- getSingleton system atlasResultBrokerActorDef
  atlasScheduleBrokerHandle <- getSingleton system atlasScheduleBrokerActorDef
  atlasSchedulerHandle <- getSingleton system atlasSchedulerActorDef
  terrainCacheWorkerHandle <- getSingleton system terrainCacheWorkerActorDef
  setAtlasSchedulerHandles atlasSchedulerHandle AtlasSchedulerHandles
    { ashManager = atlasManagerHandle
    , ashWorker = atlasWorkerHandle
    , ashResultBroker = atlasResultBrokerHandle
    , ashScheduleBroker = atlasScheduleBrokerHandle
    }
  terrainCacheBrokerHandle <- getSingleton system terrainCacheBrokerActorDef
  uiActionsHandle <- getSingleton system uiActionsActorDef
  snapshotReceiverHandle <- getSingleton system snapshotReceiverActorDef
  seed <- randomIO
  setUiSeed uiHandle seed
  setUiSeedInput uiHandle (Text.pack (show seed))
  uiSnap <- getUiSnapshot uiHandle
  let logSnap = LogSnapshot [] False 0 LogDebug
      dataSnap = DataSnapshot 0 0 Nothing
      terrainSnap = TerrainSnapshot 0 0 mempty mempty mempty
  snapshotRef <- newIORef (SnapshotVersion 0, RenderSnapshot
    { rsUi = uiSnap
    , rsLog = logSnap
    , rsData = dataSnap
    , rsTerrain = terrainSnap
    })
  setSnapshotRef snapshotReceiverHandle snapshotRef
  -- Give UiActions direct write access to the snapshot IORef so terrain
  -- data propagates to the render thread immediately, even when the
  -- SnapshotReceiver actor is CPU-starved by atlas workers.
  setUiActionsSnapshotRef uiActionsHandle snapshotRef
  -- Lock-free IORef channels for broker results (render thread reads these)
  terrainCacheRef <- newIORef Nothing
  setTerrainCacheRef terrainCacheBrokerHandle terrainCacheRef
  atlasResultRef <- newIORef []
  setAtlasResultRef atlasResultBrokerHandle atlasResultRef
  atlasScheduleRef <- newIORef Nothing
  setAtlasScheduleRef atlasScheduleBrokerHandle atlasScheduleRef
  cast @"uiSnapshot" snapshotReceiverHandle #uiSnapshot uiSnap
  cast @"logSnapshot" snapshotReceiverHandle #logSnapshot logSnap
  cast @"dataSnapshot" snapshotReceiverHandle #dataSnapshot dataSnap
  cast @"terrainSnapshot" snapshotReceiverHandle #terrainSnapshot terrainSnap
  SDL.initialize [SDL.InitVideo]
  Font.initialize
  window <- SDL.createWindow "Topo Seer" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  SDL.setWindowMode window SDL.FullscreenDesktop
  renderTargetOk <- SDL.renderTargetSupported renderer
  fontCache <- initFontCacheMaybe renderer 14
    [ "C:\\Windows\\Fonts\\segoeui.ttf"
    , "C:\\Windows\\Fonts\\consola.ttf"
    , "C:\\Windows\\Fonts\\arial.ttf"
    ]
  quitRef <- newIORef False
  lineHeightRef <- logLineHeight fontCache >>= newIORef
  mousePosRef <- newIORef (0, 0)
  dragRef <- newIORef Nothing
  tooltipHoverRef <- newIORef Nothing
  runtimeCfg <- loadConfig
  let frameDelayMs         = cfgFrameDelayMs runtimeCfg
      atlasUploadsPerFrame = cfgAtlasUploadsPerFrame runtimeCfg
      atlasCacheEntries    = cfgAtlasCacheEntries runtimeCfg
      terrainCachePollMs   = cfgTerrainCachePollMs runtimeCfg
      atlasDrainPollMs     = cfgAtlasDrainPollMs runtimeCfg
      atlasSchedulePollMs  = cfgAtlasSchedulePollMs runtimeCfg
      chunkTexturePollMs   = cfgChunkTexturePollMs runtimeCfg
      snapshotPollMs       = cfgSnapshotPollMs runtimeCfg
      timingLogThresholdMs = fromIntegral (cfgTimingLogThresholdMs runtimeCfg) :: Word32
      renderTraceEnabled   = cfgRenderTraceEnabled runtimeCfg
  lastSnapshotChangeNs <- getMonotonicTimeNSec >>= newIORef
  staleLoggedRef <- newIORef False
  home <- getHomeDirectory
  traceH <- openFile (home </> ".topo" </> "RENDER_TRACE.txt") WriteMode
  hPutStrLn traceH "=== render trace start ==="
  hFlush traceH
  let initialCacheState = RenderCacheState
        { rcsTerrainCache = emptyTerrainCache
        , rcsCacheKey = Nothing
        , rcsLastRequest = Nothing
        , rcsChunkTextures = emptyChunkTextureCache
        , rcsAtlasCache = emptyAtlasTextureCache atlasCacheEntries
        , rcsLastSnapshot = Nothing
      , rcsLastPolledSnapshot = Nothing
        , rcsLastTerrainPoll = Nothing
        , rcsLastSnapshotPoll = Nothing
        , rcsLastSnapshotData = Nothing
        , rcsLastAtlasDrain = Nothing
        , rcsLastAtlasSchedule = Nothing
        , rcsLastChunkTexturePoll = Nothing
        }
      loop cacheState = do
        loopStart <- getMonotonicTimeNSec
        events <- SDL.pollEvents
        tPoll <- getMonotonicTimeNSec
        let eventsElapsed = nsToMs loopStart tPoll
        when (eventsElapsed >= timingLogThresholdMs) $
          appendLog logHandle (LogEntry LogInfo (Text.pack ("poll events took " <> show eventsElapsed <> "ms")))
        nowMs <- SDL.ticks
        let hasEvents = not (null events)
            shouldPollSnapshot = hasEvents
              || shouldPoll nowMs snapshotPollMs (rcsLastSnapshotPoll cacheState)
              || rcsLastSnapshotData cacheState == Nothing
        (snapVersion, renderSnap0, cacheState0, snapshotElapsed) <-
          case (shouldPollSnapshot, rcsLastSnapshotData cacheState, rcsLastPolledSnapshot cacheState) of
            (False, Just cachedSnap, Just cachedVersion) ->
              pure (cachedVersion, cachedSnap, cacheState, 0)
            _ -> do
              snapshotStart <- getMonotonicTimeNSec
              (version, snap) <- readIORef snapshotRef
              snapshotEnd <- getMonotonicTimeNSec
              let snapshotElapsed' = nsToMs snapshotStart snapshotEnd
              when (snapshotElapsed' >= timingLogThresholdMs) $
                appendLog logHandle (LogEntry LogInfo (Text.pack ("snapshot poll took " <> show snapshotElapsed' <> "ms")))
              pure (version, snap, cacheState
                { rcsLastPolledSnapshot = Just version
                , rcsLastSnapshotData = Just snap
                , rcsLastSnapshotPoll = Just nowMs
                }, snapshotElapsed')
        -- Overlay the log snapshot from the self-publishing IORef so the
        -- render thread always sees the latest log entries without casting
        -- to the Log actor.
        latestLogSnap <- readLogSnapshotRef logSnapshotRef
        let renderSnap = renderSnap0 { rsLog = latestLogSnap }
        tSnap <- getMonotonicTimeNSec
        quitFlag <- readIORef quitRef
        let quit = quitFlag || any isQuit events
        let coalescedEvents = coalesceMouseMotion events
        handleElapsed <-
          if null events
            then do
              -- Even when idle, tick the tooltip frame counter; if it
              -- fires we must request a fresh UI snapshot so the render
              -- thread picks up the hover widget.
              fired <- tickTooltipHover tooltipHoverRef uiHandle
              when fired $
                requestUiSnapshot uiHandle (replyTo @UiSnapshotReply snapshotReceiverHandle)
              pure 0
            else do
              handleStart <- getMonotonicTimeNSec
              forM_ coalescedEvents (handleEvent window uiHandle logHandle dataHandle terrainHandle atlasManagerHandle uiActionsHandle snapshotReceiverHandle (rsUi renderSnap) (rsLog renderSnap) (rsData renderSnap) (rsTerrain renderSnap) quitRef lineHeightRef mousePosRef dragRef tooltipHoverRef)
              _ <- tickTooltipHover tooltipHoverRef uiHandle
              afterEvents <- getMonotonicTimeNSec
              requestUiSnapshot uiHandle (replyTo @UiSnapshotReply snapshotReceiverHandle)
              handleEnd <- getMonotonicTimeNSec
              let elapsed = nsToMs handleStart handleEnd
                  eventsMs = nsToMs handleStart afterEvents
                  uiSnapMs = nsToMs afterEvents handleEnd
              when (elapsed >= timingLogThresholdMs) $
                appendLog logHandle (LogEntry LogInfo (Text.pack
                  ("handle events took " <> show elapsed <> "ms [" <> show (length events) <> " raw, " <> show (length coalescedEvents) <> " coalesced] dispatch=" <> show eventsMs <> "ms uiSnap=" <> show uiSnapMs <> "ms")))
              pure elapsed
        tHandle <- getMonotonicTimeNSec
        let isVersionUnchanged = rcsLastSnapshot cacheState0 == Just snapVersion
            generating = uiGenerating (rsUi renderSnap)
        if isVersionUnchanged && not generating
          then do
            -- Stale-snapshot detection: log once if version unchanged for >1s
            now <- getMonotonicTimeNSec
            lastChange <- readIORef lastSnapshotChangeNs
            staleLogged <- readIORef staleLoggedRef
            let staleSec = fromIntegral (now - lastChange) / (1e9 :: Double)
            when (staleSec > 1.0 && not staleLogged) $ do
              writeIORef staleLoggedRef True
              appendLog logHandle (LogEntry LogInfo (Text.pack
                ("stale snapshot: version unchanged for " <> show (round staleSec :: Int) <> "s (v=" <> show (unSnapshotVersion snapVersion) <> ")")))
            delayStart <- getMonotonicTimeNSec
            SDL.delay (fromIntegral frameDelayMs)
            delayEnd <- getMonotonicTimeNSec
            let delayElapsed = nsToMs delayStart delayEnd
            when renderTraceEnabled $
              appendLog logHandle (LogEntry LogInfo (Text.pack (renderStepSummary eventsElapsed snapshotElapsed handleElapsed 0 0 delayElapsed True)))
            tEnd <- getMonotonicTimeNSec
            let loopMs = nsToMs loopStart tEnd
            when (loopMs >= 100) $ do
              hPutStrLn traceH $ "IDLE loop=" <> show loopMs <> "ms poll=" <> show (nsToMs loopStart tPoll) <> " snap=" <> show (nsToMs tPoll tSnap) <> " handle=" <> show (nsToMs tSnap tHandle) <> " stale=" <> show (nsToMs tHandle tEnd) <> " events=" <> show (length events) <> " v=" <> show (unSnapshotVersion snapVersion)
              hFlush traceH
            if quit
              then pure cacheState0
              else loop cacheState0
          else do
            -- Snapshot version changed; reset stale tracking
            tElseBranch <- getMonotonicTimeNSec
            writeIORef lastSnapshotChangeNs =<< getMonotonicTimeNSec
            writeIORef staleLoggedRef False
            tAfterWrite <- getMonotonicTimeNSec
            -- Skip terrain cache, atlas, and chunk texture polling while
            -- generating — those operations are no-ops during generation
            -- and running them exposes the render thread to GC / scheduling
            -- stalls that manifest as multi-second freezes.
            let shouldPollTerrain = not generating && shouldPoll nowMs terrainCachePollMs (rcsLastTerrainPoll cacheState0)
                shouldDrainAtlas = not generating && shouldPoll nowMs atlasDrainPollMs (rcsLastAtlasDrain cacheState0)
                shouldScheduleAtlas = not generating && shouldPoll nowMs atlasSchedulePollMs (rcsLastAtlasSchedule cacheState0)
                shouldUpdateChunkTextures = not generating && shouldPoll nowMs chunkTexturePollMs (rcsLastChunkTexturePoll cacheState0)
            tAfterLets <- getMonotonicTimeNSec
            (cacheState', terrainElapsed) <-
              if shouldPollTerrain
                then do
                  cacheStart <- getMonotonicTimeNSec
                  updated <- applyTerrainCacheUpdate renderSnap terrainCacheWorkerHandle terrainCacheBrokerHandle terrainCacheRef cacheState0
                  cacheEnd <- getMonotonicTimeNSec
                  let cacheElapsed = nsToMs cacheStart cacheEnd
                  when (cacheElapsed >= timingLogThresholdMs) $
                    appendLog logHandle (LogEntry LogInfo (Text.pack ("terrain cache check took " <> show cacheElapsed <> "ms")))
                  pure (updated { rcsLastTerrainPoll = Just nowMs }, cacheElapsed)
                else pure (cacheState0, 0)
            tAfterTerrain <- getMonotonicTimeNSec
            let cache'' = rcsTerrainCache cacheState'
            frameStart <- getMonotonicTimeNSec
            (needsRetry, nextChunkTextures, nextAtlasCache, didLog) <-
              renderFrame renderer window snapVersion renderSnap cache'' (rcsChunkTextures cacheState') (rcsAtlasCache cacheState')
                logHandle atlasSchedulerHandle atlasScheduleRef atlasResultRef atlasUploadsPerFrame
                shouldDrainAtlas shouldScheduleAtlas shouldUpdateChunkTextures timingLogThresholdMs fontCache renderTargetOk traceH
            frameEnd <- getMonotonicTimeNSec
            let frameElapsed = nsToMs frameStart frameEnd
            let cacheState'' = cacheState'
                  { rcsChunkTextures = nextChunkTextures
                  , rcsAtlasCache = nextAtlasCache
                  , rcsLastSnapshot = if needsRetry then Nothing else Just snapVersion
                  , rcsLastSnapshotData = Just renderSnap
                  , rcsLastAtlasDrain = if shouldDrainAtlas then Just nowMs else rcsLastAtlasDrain cacheState'
                  , rcsLastAtlasSchedule = if shouldScheduleAtlas then Just nowMs else rcsLastAtlasSchedule cacheState'
                  , rcsLastChunkTexturePoll = if shouldUpdateChunkTextures then Just nowMs else rcsLastChunkTexturePoll cacheState'
                  }
            when (frameElapsed >= timingLogThresholdMs) $
              appendLog logHandle (LogEntry LogInfo (renderMetrics frameElapsed renderSnap cacheState''))
            -- Log snapshots are self-published by the Log actor to LogSnapshotRef;
            -- no cast needed from the render thread.
            delayStart <- getMonotonicTimeNSec
            SDL.delay (fromIntegral frameDelayMs)
            delayEnd <- getMonotonicTimeNSec
            let delayElapsed = nsToMs delayStart delayEnd
            tPostDelay <- getMonotonicTimeNSec
            when renderTraceEnabled $
              appendLog logHandle (LogEntry LogInfo (Text.pack (renderStepSummary eventsElapsed snapshotElapsed handleElapsed terrainElapsed frameElapsed delayElapsed False)))
            tEnd <- getMonotonicTimeNSec
            let loopMs = nsToMs loopStart tEnd
            when (loopMs >= 100) $ do
              hPutStrLn traceH $ "RENDER loop=" <> show loopMs <> "ms poll=" <> show (nsToMs loopStart tPoll) <> " snap=" <> show (nsToMs tPoll tSnap) <> " handle=" <> show (nsToMs tSnap tHandle) <> " branch=" <> show (nsToMs tHandle tElseBranch) <> " write=" <> show (nsToMs tElseBranch tAfterWrite) <> " lets=" <> show (nsToMs tAfterWrite tAfterLets) <> " tPoll=" <> show (nsToMs tAfterLets tAfterTerrain) <> " tLet=" <> show (nsToMs tAfterTerrain frameStart) <> " terrain=" <> show terrainElapsed <> " frame=" <> show frameElapsed <> " postFrame=" <> show (nsToMs frameEnd delayStart) <> " delay=" <> show delayElapsed <> " postDelay=" <> show (nsToMs tPostDelay tEnd) <> " events=" <> show (length events) <> " v=" <> show (unSnapshotVersion snapVersion)
              hFlush traceH
            if quit
              then pure cacheState''
              else loop cacheState''
  finalState <- loop initialCacheState
  hPutStrLn traceH "=== render trace end ==="
  hFlush traceH
  let finalChunkTextures = rcsChunkTextures finalState
      finalAtlasCache = rcsAtlasCache finalState
  mapM_ destroyChunkTexture (IntMap.elems (ctcTextures finalChunkTextures))
  mapM_ SDL.destroyTexture (collectAtlasTextures finalAtlasCache)
  maybe (pure ()) destroyFontCache fontCache
  Font.quit
  SDL.destroyWindow window
  SDL.quit
  shutdownActorSystem system

shouldStartTerrainCacheBuild :: RenderSnapshot -> TerrainCache -> Bool
shouldStartTerrainCacheBuild renderSnap cache =
  let uiSnap = rsUi renderSnap
      terrainSnap = rsTerrain renderSnap
  in not (uiGenerating uiSnap)
      && terrainCacheNeedsRebuild uiSnap terrainSnap cache

terrainCacheNeedsRebuild :: UiState -> TerrainSnapshot -> TerrainCache -> Bool
terrainCacheNeedsRebuild uiSnap terrainSnap cache
  | tsChunkSize terrainSnap <= 0 =
      tcChunkSize cache /= 0
        || not (IntMap.null (tcTerrainChunks cache))
        || not (IntMap.null (tcClimateChunks cache))
        || not (IntMap.null (tcWeatherChunks cache))
  | tcViewMode cache /= uiViewMode uiSnap = True
  | tcWaterLevel cache /= uiRenderWaterLevel uiSnap = True
  | tcChunkSize cache /= tsChunkSize terrainSnap = True
  | tcVersion cache /= tsVersion terrainSnap = True
  | otherwise = False

data RenderCacheState = RenderCacheState
  { rcsTerrainCache :: !TerrainCache
  , rcsCacheKey :: !(Maybe TerrainCacheKey)
  , rcsLastRequest :: !(Maybe TerrainCacheKey)
  , rcsChunkTextures :: !ChunkTextureCache
  , rcsAtlasCache :: !AtlasTextureCache
  , rcsLastSnapshot :: !(Maybe SnapshotVersion)
  , rcsLastPolledSnapshot :: !(Maybe SnapshotVersion)
  , rcsLastTerrainPoll :: !(Maybe Word32)
  , rcsLastSnapshotPoll :: !(Maybe Word32)
  , rcsLastSnapshotData :: !(Maybe RenderSnapshot)
  , rcsLastAtlasDrain :: !(Maybe Word32)
  , rcsLastAtlasSchedule :: !(Maybe Word32)
  , rcsLastChunkTexturePoll :: !(Maybe Word32)
  }

applyTerrainCacheUpdate
  :: RenderSnapshot
  -> ActorHandle TerrainCacheWorker (Protocol TerrainCacheWorker)
  -> ActorHandle TerrainCacheBroker (Protocol TerrainCacheBroker)
  -> TerrainCacheRef
  -> RenderCacheState
  -> IO RenderCacheState
applyTerrainCacheUpdate renderSnap workerHandle brokerHandle cacheRef cacheState = do
  let uiSnap = rsUi renderSnap
      terrainSnap = rsTerrain renderSnap
      desiredKey = terrainCacheKeyFrom uiSnap terrainSnap
  latest <- readTerrainCacheRef cacheRef
  let stateAfterResult =
        case (desiredKey, latest) of
          (Nothing, _) -> cacheState
            { rcsTerrainCache = emptyTerrainCache
            , rcsCacheKey = Nothing
            , rcsLastRequest = Nothing
            }
          (Just key, Just result)
            | tcrResultKey result == key -> cacheState
                { rcsTerrainCache = tcrResultCache result
                , rcsCacheKey = Just key
                }
            | otherwise -> cacheState
          (Just _key, Nothing) -> cacheState
      cache = rcsTerrainCache stateAfterResult
      currentKey = rcsCacheKey stateAfterResult
      lastRequest = rcsLastRequest stateAfterResult
  if shouldStartTerrainCacheBuild renderSnap cache && currentKey /= desiredKey && lastRequest /= desiredKey
    then case desiredKey of
      Nothing -> pure stateAfterResult { rcsLastRequest = Nothing }
      Just key -> do
        requestTerrainCacheBuild workerHandle TerrainCacheBuildRequest
          { tcrKey = key
          , tcrUi = uiSnap
          , tcrTerrain = terrainSnap
          , tcrReplyTo = replyTo @TerrainCacheResultReply brokerHandle
          }
        pure stateAfterResult { rcsLastRequest = Just key }
    else pure stateAfterResult



renderMetrics :: Word32 -> RenderSnapshot -> RenderCacheState -> Text.Text
renderMetrics frameMs _snap cacheState =
  let terrainCount = IntMap.size (tcTerrainChunks (rcsTerrainCache cacheState))
      chunkTextures = IntMap.size (ctcTextures (rcsChunkTextures cacheState))
      atlasScales = length (atcLru (rcsAtlasCache cacheState))
      atlasTiles = sum (map length (IntMap.elems (atcCaches (rcsAtlasCache cacheState))))
      snapshotVer = case rcsLastSnapshot cacheState of
        Nothing -> "none"
        Just (SnapshotVersion v) -> show v
  in Text.pack
      ("render: frame=" <> show frameMs <> "ms"
        <> " terrainChunks=" <> show terrainCount
        <> " chunkTextures=" <> show chunkTextures
        <> " atlasScales=" <> show atlasScales
        <> " atlasTiles=" <> show atlasTiles
        <> " snapshot=" <> snapshotVer)

shouldPoll :: Word32 -> Int -> Maybe Word32 -> Bool
shouldPoll nowMs pollMs lastPoll =
  case lastPoll of
    Nothing -> True
    Just prev -> nowMs - prev >= fromIntegral pollMs

nsToMs :: Word64 -> Word64 -> Word32
nsToMs start end =
  fromIntegral ((end - start) `div` 1000000)

renderStepSummary :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Bool -> String
renderStepSummary pollMs snapshotMs handleMs terrainMs frameMs delayMs unchangedSnapshot =
  "render steps: poll=" <> show pollMs <> "ms"
    <> " snapshot=" <> show snapshotMs <> "ms"
    <> " handle=" <> show handleMs <> "ms"
    <> " terrain=" <> show terrainMs <> "ms"
    <> " frame=" <> show frameMs <> "ms"
    <> " delay=" <> show delayMs <> "ms"
    <> " unchangedSnapshot=" <> show unchangedSnapshot

-- | Coalesce consecutive mouse motion events, keeping only the last one in
-- each run of consecutive motions.  Non-motion events retain their original
-- relative order.  This prevents thousands of queued motion events from
-- flooding the Ui actor mailbox and causing back-pressure stalls on the
-- render thread.  It also fixes a pan-drift bug where intermediate motions
-- overwrote each other's @setUiPanOffset@ casts against a stale cached
-- snapshot.
coalesceMouseMotion :: [SDL.Event] -> [SDL.Event]
coalesceMouseMotion = go
  where
    go []  = []
    go [x] = [x]
    go (x : xs@(y : _))
      | isMotion x, isMotion y = go xs   -- drop x, advance
      | otherwise              = x : go xs

    isMotion :: SDL.Event -> Bool
    isMotion e = case SDL.eventPayload e of
      SDL.MouseMotionEvent _ -> True
      _                      -> False
