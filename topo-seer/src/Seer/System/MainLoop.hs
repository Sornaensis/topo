{-# LANGUAGE OverloadedStrings #-}

module Seer.System.MainLoop
  ( runMainLoop
  , shouldSkipUnchangedFrame
  , atlasQueueTargetsNewerThan
  , atlasQueueSnapshotRefreshRequired
  , renderSnapshotForceRequired
  , RenderWakeInputs(..)
  , RenderFreshnessDecision(..)
  , SnapshotGatePhase(..)
  , SnapshotGateDiagnostic(..)
  , SnapshotAdvanceState(..)
  , SnapshotLagReasons(..)
  , SnapshotLagState(..)
  , initialSnapshotAdvanceState
  , recordRenderedSnapshot
  , snapshotAdvanceAgeMs
  , initialSnapshotLagState
  , snapshotLagTransition
  , snapshotGateDiagnostic
  , formatSnapshotGateDiagnostic
  , prepareRenderFreshnessDecision
  ) where

import Actor.AtlasManager (AtlasManagerQueueState(..), AtlasQueuedTarget(..), atlasManagerQueuedState)
import Actor.AtlasResultBroker (atlasResultsPendingCount)
import Actor.Log (LogEntry(..), LogLevel(..), appendLog)
import Actor.Render (RenderSnapshot(..))
import Actor.SnapshotReceiver (SnapshotVersion(..))
import Actor.UI (UiState(..))
import Control.Monad (forM_, when)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (intercalate)
import Data.Maybe (isJust)
import qualified Data.Text as Text
import Data.Word (Word32, Word64)
import GHC.Clock (getMonotonicTimeNSec)
import qualified SDL
import Seer.Command.AppServiceAdapter (commandAppService)
import Seer.Config.Runtime (TopoSeerConfig(..))
import Seer.Draw (logLineHeight)
import Seer.System.Actors (AppActors(..))
import Seer.System.Cache
  ( RenderCacheState(..)
  , initialRenderCacheState
  , renderStepSummary
  )
import Seer.Input.Context (newInputActionDispatcher)
import Seer.System.EventPump (EventPumpEnv(..), EventPumpResult(..), hasQuitEvent, processEvents)
import Seer.System.RenderFrame
  ( RenderFrameEnv(..)
  , RenderFrameSettings(..)
  , RenderFrameMaintenanceDiagnostics(..)
  , RenderFrameStepResult(..)
  , renderFrameStep
  , renderFrameStepMaintenance
  )
import Seer.Screenshot.Request (ScreenshotClaim, claimScreenshotRequest)
import Seer.System.Sdl (SdlResources(..))
import Seer.System.Snapshot
  ( SnapshotPollEnv(..)
  , SnapshotPollReasons(..)
  , SnapshotPollRequest(..)
  , SnapshotPollResult(..)
  , noSnapshotPollRequest
  , pollRenderSnapshot
  , pollRenderSnapshotDetailed
  )
import Seer.Timing (nsToMs)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.IO (IOMode(..), hFlush, hPutStrLn, openFile)

-- | Decide whether an unchanged frame can sleep. Queued screenshots always
-- force one render even when the world snapshot itself has not changed.
shouldSkipUnchangedFrame :: Bool -> Bool -> Bool -> Bool -> Bool
shouldSkipUnchangedFrame unchanged generating screenshotPending maintenanceDue =
  unchanged && not generating && not screenshotPending && not maintenanceDue

-- | A queued target from a later publication must be considered before
-- current-key matching, because the current key itself may change at that
-- publication.
atlasQueueTargetsNewerThan :: SnapshotVersion -> AtlasManagerQueueState -> Bool
atlasQueueTargetsNewerThan version =
  any ((> version) . aqtSnapshotVersion) . amqsQueuedTargets

-- | Force at most one coherent reread for a queue revision. Normal publication
-- probes will still notice the target version when it commits.
atlasQueueSnapshotRefreshRequired
  :: SnapshotVersion
  -> AtlasManagerQueueState
  -> Maybe Word64
  -> Bool
atlasQueueSnapshotRefreshRequired version queueState lastPollRevision =
  atlasQueueTargetsNewerThan version queueState
    && lastPollRevision /= Just (amqsQueuedRevision queueState)

renderSnapshotForceRequired
  :: Bool
  -> SnapshotVersion
  -> AtlasManagerQueueState
  -> Maybe Word64
  -> Bool
renderSnapshotForceRequired screenshotPending version queueState lastPollRevision =
  screenshotPending
    || atlasQueueSnapshotRefreshRequired version queueState lastPollRevision

data RenderWakeInputs = RenderWakeInputs
  { rwiGenerating :: !Bool
  , rwiAtlasPendingCount :: !Int
  , rwiAtlasQueueState :: !AtlasManagerQueueState
  } deriving (Eq, Show)

data RenderFreshnessDecision = RenderFreshnessDecision
  { rfdSnapshotVersion :: !SnapshotVersion
  , rfdRenderSnapshot :: !RenderSnapshot
  , rfdCacheState :: !RenderCacheState
  , rfdWakeInputs :: !RenderWakeInputs
  , rfdScreenshotClaim :: !(Maybe ScreenshotClaim)
  , rfdSnapshotElapsed :: !Word32
  , rfdFinalSnapshotPollResult :: !SnapshotPollResult
  , rfdSnapshotPollResults :: ![SnapshotPollResult]
  }

data SnapshotGatePhase
  = SnapshotGateSkip
  | SnapshotGateWake
  | SnapshotGateRender
  deriving (Eq, Show)

data SnapshotGateDiagnostic = SnapshotGateDiagnostic
  { sgdPhase :: !SnapshotGatePhase
  , sgdProbedPublishedVersion :: !SnapshotVersion
  , sgdPublishedVersion :: !SnapshotVersion
  , sgdPolledVersion :: !SnapshotVersion
  , sgdLastPolledVersion :: !(Maybe SnapshotVersion)
  , sgdRenderedVersion :: !(Maybe SnapshotVersion)
  , sgdPollDidRead :: !Bool
  , sgdPollReasons :: !SnapshotPollReasons
  , sgdScreenshotPending :: !Bool
  , sgdAtlasPendingCount :: !Int
  , sgdAtlasQueuedCount :: !Int
  , sgdAtlasQueueRevision :: !Word64
  , sgdAtlasQueueVersionRange :: !(Maybe (SnapshotVersion, SnapshotVersion))
  , sgdGenerating :: !Bool
  , sgdMaintenanceDue :: !Bool
  , sgdForcedGateSignalled :: !Bool
  , sgdForcedGateSatisfied :: !Bool
  , sgdSnapshotAdvanceAgeMs :: !Word64
  } deriving (Eq, Show)

data SnapshotAdvanceState = SnapshotAdvanceState
  { sasLastRenderedVersion :: !(Maybe SnapshotVersion)
  , sasLastVersionAdvanceNs :: !Word64
  } deriving (Eq, Show)

data SnapshotLagReasons = SnapshotLagReasons
  { slrPublicationAheadOfPoll :: !Bool
  , slrPublicationAheadOfRender :: !Bool
  , slrForcedFreshGateFailed :: !Bool
  } deriving (Eq, Show)

data SnapshotLagState = SnapshotLagState
  { slsActiveReasons :: !(Maybe SnapshotLagReasons)
  , slsLagSinceNs :: !(Maybe Word64)
  , slsLastWarningNs :: !(Maybe Word64)
  } deriving (Eq, Show)

initialSnapshotAdvanceState :: Word64 -> SnapshotAdvanceState
initialSnapshotAdvanceState nowNs = SnapshotAdvanceState Nothing nowNs

-- | Maintenance rendering of the same version must not pretend that snapshot
-- freshness advanced. Only a strictly newer rendered version resets the age.
recordRenderedSnapshot
  :: Word64
  -> SnapshotVersion
  -> SnapshotAdvanceState
  -> SnapshotAdvanceState
recordRenderedSnapshot nowNs version state =
  case sasLastRenderedVersion state of
    Nothing -> SnapshotAdvanceState (Just version) nowNs
    Just previous
      | version > previous -> SnapshotAdvanceState (Just version) nowNs
      | otherwise -> state

snapshotAdvanceAgeMs :: Word64 -> SnapshotAdvanceState -> Word64
snapshotAdvanceAgeMs nowNs state = (nowNs - sasLastVersionAdvanceNs state) `div` 1000000

initialSnapshotLagState :: SnapshotLagState
initialSnapshotLagState = SnapshotLagState Nothing Nothing Nothing

snapshotLagTransition
  :: Word64
  -> Word64
  -> Word64
  -> SnapshotGateDiagnostic
  -> SnapshotLagState
  -> (SnapshotLagState, Maybe SnapshotLagReasons)
snapshotLagTransition thresholdNs repeatNs nowNs diagnostic state =
  let reasons = SnapshotLagReasons
        { slrPublicationAheadOfPoll = sgdPublishedVersion diagnostic > sgdPolledVersion diagnostic
        , slrPublicationAheadOfRender = maybe
            True
            (sgdPublishedVersion diagnostic >)
            (sgdRenderedVersion diagnostic)
        , slrForcedFreshGateFailed =
            sgdForcedGateSignalled diagnostic && not (sgdForcedGateSatisfied diagnostic)
        }
      lagging = or
        [ slrPublicationAheadOfPoll reasons
        , slrPublicationAheadOfRender reasons
        , slrForcedFreshGateFailed reasons
        ]
  in if not lagging
      then (initialSnapshotLagState, Nothing)
      else case slsLagSinceNs state of
        Nothing ->
          ( SnapshotLagState (Just reasons) (Just nowNs) Nothing
          , Nothing
          )
        Just lagSince ->
          let thresholdReached = nowNs - lagSince >= thresholdNs
              rateLimitReached = maybe
                True
                (\lastWarning -> nowNs - lastWarning >= repeatNs)
                (slsLastWarningNs state)
              activeState = state { slsActiveReasons = Just reasons }
          in if thresholdReached && rateLimitReached
              then
                ( activeState { slsLastWarningNs = Just nowNs }
                , Just reasons
                )
              else (activeState, Nothing)

snapshotGateDiagnostic
  :: SnapshotGatePhase
  -> SnapshotPollResult
  -> Maybe SnapshotVersion
  -> Bool
  -> RenderWakeInputs
  -> Bool
  -> Word64
  -> SnapshotGateDiagnostic
snapshotGateDiagnostic phase pollResult renderedVersion screenshotPending wake maintenanceDue advanceAgeMs =
  let queueState = rwiAtlasQueueState wake
      targetVersions = map aqtSnapshotVersion (amqsQueuedTargets queueState)
      versionRange = case targetVersions of
        [] -> Nothing
        versions -> Just (minimum versions, maximum versions)
      polledVersion = spresCoherentVersion pollResult
      probedPublishedVersion = spresProbedPublishedVersion pollResult
      latestKnownPublishedVersion = max probedPublishedVersion polledVersion
      pollReasons = spresReasons pollResult
      futureAtlasTarget = maybe False ((> polledVersion) . snd) versionRange
      screenshotGateSignalled = screenshotPending || sprForcedScreenshot pollReasons
      atlasGateSignalled = sprFutureAtlasTarget pollReasons || futureAtlasTarget
      forcedGateSignalled = screenshotGateSignalled || atlasGateSignalled
      forcedGateSatisfied =
        (not screenshotGateSignalled || spresDidReadCoherent pollResult)
          && (not atlasGateSignalled || not futureAtlasTarget)
  in SnapshotGateDiagnostic
      { sgdPhase = phase
      , sgdProbedPublishedVersion = probedPublishedVersion
      , sgdPublishedVersion = latestKnownPublishedVersion
      , sgdPolledVersion = polledVersion
      , sgdLastPolledVersion = spresLastPolledVersion pollResult
      , sgdRenderedVersion = renderedVersion
      , sgdPollDidRead = spresDidReadCoherent pollResult
      , sgdPollReasons = spresReasons pollResult
      , sgdScreenshotPending = screenshotPending
      , sgdAtlasPendingCount = rwiAtlasPendingCount wake
      , sgdAtlasQueuedCount = amqsQueuedCount queueState
      , sgdAtlasQueueRevision = amqsQueuedRevision queueState
      , sgdAtlasQueueVersionRange = versionRange
      , sgdGenerating = rwiGenerating wake
      , sgdMaintenanceDue = maintenanceDue
      , sgdForcedGateSignalled = forcedGateSignalled
      , sgdForcedGateSatisfied = forcedGateSatisfied
      , sgdSnapshotAdvanceAgeMs = advanceAgeMs
      }

formatSnapshotGateDiagnostic :: SnapshotGateDiagnostic -> String
formatSnapshotGateDiagnostic diagnostic =
  let versionText (SnapshotVersion version) = show version
      maybeVersionText = maybe "none" versionText
      (queueMin, queueMax) = case sgdAtlasQueueVersionRange diagnostic of
        Nothing -> ("none", "none")
        Just (minimumVersion, maximumVersion) ->
          (versionText minimumVersion, versionText maximumVersion)
      reasons = sgdPollReasons diagnostic
      reasonLabels =
        [ label
        | (enabled, label) <-
            [ (sprBootstrap reasons, "bootstrap")
            , (sprPublishedVersionAdvanced reasons, "published-version-advanced")
            , (sprForcedScreenshot reasons, "forced-screenshot")
            , (sprFutureAtlasTarget reasons, "future-atlas-target")
            , (sprIntervalHealthReread reasons, "interval-health-reread")
            , (sprCacheReuse reasons, "cache-reuse")
            ]
        , enabled
        ]
      phaseText = case sgdPhase diagnostic of
        SnapshotGateSkip -> "skip"
        SnapshotGateWake -> "wake"
        SnapshotGateRender -> "render"
  in "snapshot-gate phase=" <> phaseText
      <> " published=" <> versionText (sgdPublishedVersion diagnostic)
      <> " probed=" <> versionText (sgdProbedPublishedVersion diagnostic)
      <> " polled=" <> versionText (sgdPolledVersion diagnostic)
      <> " lastPolled=" <> maybeVersionText (sgdLastPolledVersion diagnostic)
      <> " rendered=" <> maybeVersionText (sgdRenderedVersion diagnostic)
      <> " pollRead=" <> show (sgdPollDidRead diagnostic)
      <> " pollReasons=" <> intercalate "," reasonLabels
      <> " screenshot=" <> show (sgdScreenshotPending diagnostic)
      <> " atlasPending=" <> show (sgdAtlasPendingCount diagnostic)
      <> " atlasQueued=" <> show (sgdAtlasQueuedCount diagnostic)
      <> " queueRevision=" <> show (sgdAtlasQueueRevision diagnostic)
      <> " queueVersionMin=" <> queueMin
      <> " queueVersionMax=" <> queueMax
      <> " generating=" <> show (sgdGenerating diagnostic)
      <> " maintenance=" <> show (sgdMaintenanceDue diagnostic)
      <> " forcedGate=" <> show (sgdForcedGateSignalled diagnostic)
      <> " forcedGateSatisfied=" <> show (sgdForcedGateSatisfied diagnostic)
      <> " snapshotAdvanceAgeMs=" <> show (sgdSnapshotAdvanceAgeMs diagnostic)

summarizeSnapshotPollResults
  :: SnapshotPollResult
  -> [SnapshotPollResult]
  -> SnapshotPollResult
summarizeSnapshotPollResults finalResult results =
  let mergeReasons left right = SnapshotPollReasons
        { sprBootstrap = sprBootstrap left || sprBootstrap right
        , sprPublishedVersionAdvanced =
            sprPublishedVersionAdvanced left || sprPublishedVersionAdvanced right
        , sprForcedScreenshot = sprForcedScreenshot left || sprForcedScreenshot right
        , sprFutureAtlasTarget = sprFutureAtlasTarget left || sprFutureAtlasTarget right
        , sprIntervalHealthReread =
            sprIntervalHealthReread left || sprIntervalHealthReread right
        , sprCacheReuse = sprCacheReuse left || sprCacheReuse right
        }
      combinedReasons = foldr
        (mergeReasons . spresReasons)
        (spresReasons finalResult)
        results
      probedVersion = maximum
        (spresProbedPublishedVersion finalResult
          : map spresProbedPublishedVersion results)
      firstLastPolled = case results of
        firstResult : _ -> spresLastPolledVersion firstResult
        [] -> spresLastPolledVersion finalResult
  in finalResult
      { spresProbedPublishedVersion = probedVersion
      , spresLastPolledVersion = firstLastPolled
      , spresElapsedMs = sum (map spresElapsedMs results)
      , spresDidReadCoherent = any spresDidReadCoherent results
      , spresReasons = combinedReasons
      }

-- | Run the post-event and final freshness gates with injectable wake and claim
-- reads. Keeping this orchestration SDL-free makes the exact production order
-- deterministic under tests.
prepareRenderFreshnessDecision
  :: SnapshotPollEnv
  -> Word32
  -> (RenderSnapshot -> IO RenderWakeInputs)
  -> IO (Maybe ScreenshotClaim)
  -> RenderCacheState
  -> IO RenderFreshnessDecision
prepareRenderFreshnessDecision snapshotEnv nowMs readWakeInputs claimScreenshot cacheState = do
  postEventPoll <- pollRenderSnapshotDetailed
    snapshotEnv nowMs noSnapshotPollRequest cacheState
  let postEventVersion = spresCoherentVersion postEventPoll
      postEventSnap = spresRenderSnapshot postEventPoll
      postEventCache = spresCacheState postEventPoll
  hintWake <- readWakeInputs postEventSnap
  hintScreenshotClaim <- claimScreenshot
  let hintQueueState = rwiAtlasQueueState hintWake
      hintAtlasForce = atlasQueueSnapshotRefreshRequired
        postEventVersion
        hintQueueState
        (rcsLastAtlasSnapshotPollRevision postEventCache)
      hintRequest = SnapshotPollRequest
        { spqForcedScreenshot = isJust hintScreenshotClaim
        , spqFutureAtlasTarget = hintAtlasForce
        }
  finalPollRaw0 <- pollRenderSnapshotDetailed
    snapshotEnv nowMs hintRequest postEventCache
  let finalCache0 = markAtlasSnapshotPoll
        hintAtlasForce hintQueueState (spresCacheState finalPollRaw0)
      finalPoll0 = finalPollRaw0 { spresCacheState = finalCache0 }
      finalVersion0 = spresCoherentVersion finalPoll0
      finalSnap0 = spresRenderSnapshot finalPoll0
  finalWake0 <- readWakeInputs finalSnap0
  lateScreenshotClaim <- case hintScreenshotClaim of
    Just _ -> pure Nothing
    Nothing -> claimScreenshot
  let finalQueueState0 = rwiAtlasQueueState finalWake0
      lateAtlasForce = atlasQueueSnapshotRefreshRequired
        finalVersion0
        finalQueueState0
        (rcsLastAtlasSnapshotPollRevision finalCache0)
      lateRequest = SnapshotPollRequest
        { spqForcedScreenshot = isJust lateScreenshotClaim
        , spqFutureAtlasTarget = lateAtlasForce
        }
      lateForce = spqForcedScreenshot lateRequest || spqFutureAtlasTarget lateRequest
      screenshotClaim = case hintScreenshotClaim of
        Just claim -> Just claim
        Nothing -> lateScreenshotClaim
  if lateForce
    then do
      latePollRaw <- pollRenderSnapshotDetailed
        snapshotEnv nowMs lateRequest finalCache0
      let cache = markAtlasSnapshotPoll
            lateAtlasForce finalQueueState0 (spresCacheState latePollRaw)
          latePoll = latePollRaw { spresCacheState = cache }
          version = spresCoherentVersion latePoll
          snapshot = spresRenderSnapshot latePoll
      wake <- readWakeInputs snapshot
      let pollResults = [postEventPoll, finalPoll0, latePoll]
          pollSummary = summarizeSnapshotPollResults latePoll pollResults
      pure RenderFreshnessDecision
        { rfdSnapshotVersion = version
        , rfdRenderSnapshot = snapshot
        , rfdCacheState = cache
        , rfdWakeInputs = wake
        , rfdScreenshotClaim = screenshotClaim
        , rfdSnapshotElapsed = sum (map spresElapsedMs pollResults)
        , rfdFinalSnapshotPollResult = pollSummary
        , rfdSnapshotPollResults = pollResults
        }
    else
      let pollResults = [postEventPoll, finalPoll0]
          pollSummary = summarizeSnapshotPollResults finalPoll0 pollResults
      in pure RenderFreshnessDecision
      { rfdSnapshotVersion = finalVersion0
      , rfdRenderSnapshot = finalSnap0
      , rfdCacheState = finalCache0
      , rfdWakeInputs = finalWake0
      , rfdScreenshotClaim = screenshotClaim
      , rfdSnapshotElapsed = sum (map spresElapsedMs pollResults)
      , rfdFinalSnapshotPollResult = pollSummary
      , rfdSnapshotPollResults = pollResults
      }
  where
    markAtlasSnapshotPoll forced queueState state =
      if forced
        then state { rcsLastAtlasSnapshotPollRevision = Just (amqsQueuedRevision queueState) }
        else state

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
  modalBarrierLatchRef <- newIORef False
  inputActionDispatcher <- newInputActionDispatcher
  diagnosticStartNs <- getMonotonicTimeNSec
  snapshotAdvanceRef <- newIORef (initialSnapshotAdvanceState diagnosticStartNs)
  snapshotLagRef <- newIORef initialSnapshotLagState
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
      snapshotLagThresholdNs = 1000000000
      snapshotLagRepeatNs = 5000000000
      initialCacheState    = initialRenderCacheState (cfgAtlasCacheEntries runtimeCfg)
      eventPumpEnv = EventPumpEnv
        { epeWindow = srWindow sdl
        , epeAppService = commandAppService
        , epeActorHandles = aaActorHandles actors
        , epeUiActionsHandle = aaUiActionsHandle actors
        , epeUiSnapshotRef = aaUiSnapshotRef actors
        , epeScreenshotRef = aaScreenshotRef actors
        , epeScreenshotStoragePolicy = aaScreenshotStoragePolicy actors
        , epeLogSnapshotRef = Just (aaLogSnapshotRef actors)
        , epeDataBrowserExecutor = aaDataBrowserExecutor actors
        , epeOverlayInspectorExecutor = aaOverlayInspectorExecutor actors
        , epeQuitRef = quitRef
        , epeLineHeightRef = lineHeightRef
        , epeMousePosRef = mousePosRef
        , epeDragRef = dragRef
        , epeTooltipHoverRef = tooltipHoverRef
        , epeModalBarrierLatchRef = modalBarrierLatchRef
        , epeInputActionDispatcher = inputActionDispatcher
        }
      snapshotEnv = SnapshotPollEnv
        { speTimingLogThresholdMs = timingLogThresholdMs
        , speSnapshotPollMs = snapshotPollMs
        , speSnapshotVersionRef = aaSnapshotVersionRef actors
        , speLogSlowSnapshotPoll = \elapsed ->
            appendLog (aaLogHandle actors) (LogEntry LogInfo (Text.pack ("snapshot poll took " <> show elapsed <> "ms")))
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

      readWakeInputs renderSnap = do
        let generating = uiGenerating (rsUi renderSnap)
        atlasPendingCount <- if generating
          then pure 0
          else atlasResultsPendingCount (aaAtlasResultRef actors)
        -- Queue inspection is also a snapshot-freshness input, so cached
        -- generating state must not hide a target stamped for a newer version.
        atlasQueueState <- atlasManagerQueuedState (aaAtlasManagerQueueRef actors)
        pure RenderWakeInputs
          { rwiGenerating = generating
          , rwiAtlasPendingCount = atlasPendingCount
          , rwiAtlasQueueState = atlasQueueState
          }

      writeTraceLine line = forM_ traceH $ \h -> do
        hPutStrLn h line
        hFlush h

      observeSnapshotGate nowNs diagnostic = do
        lagState <- readIORef snapshotLagRef
        let (nextLagState, warning) = snapshotLagTransition
              snapshotLagThresholdNs snapshotLagRepeatNs nowNs diagnostic lagState
        writeIORef snapshotLagRef nextLagState
        writeTraceLine (formatSnapshotGateDiagnostic diagnostic)
        forM_ warning $ \reasons ->
          writeTraceLine
            ("WARN snapshot-gate lag publicationAheadPoll="
              <> show (slrPublicationAheadOfPoll reasons)
              <> " publicationAheadRender="
              <> show (slrPublicationAheadOfRender reasons)
              <> " forcedFreshGateFailed="
              <> show (slrForcedFreshGateFailed reasons)
              <> " " <> formatSnapshotGateDiagnostic diagnostic)

      loop cacheState = do
        loopStart <- getMonotonicTimeNSec
        events <- SDL.pollEvents
        tPoll <- getMonotonicTimeNSec
        let eventsElapsed = nsToMs loopStart tPoll
        when (eventsElapsed >= timingLogThresholdMs) $
          appendLog (aaLogHandle actors) (LogEntry LogInfo (Text.pack ("poll events took " <> show eventsElapsed <> "ms")))
        nowMs <- SDL.ticks
        (_eventSnapVersion, eventRenderSnap, eventCacheState, initialSnapshotElapsed) <-
          pollRenderSnapshot snapshotEnv nowMs False cacheState
        tSnap <- getMonotonicTimeNSec
        quitFlag <- readIORef quitRef
        let quit = quitFlag || hasQuitEvent events
        eventResult <- processEvents eventPumpEnv timingLogThresholdMs events eventRenderSnap
        tHandle <- getMonotonicTimeNSec

        -- Event handlers (including the idle tooltip tick) may publish. The
        -- extracted decision seam performs the mandatory post-event probe,
        -- exact screenshot claim, atlas gate, and final coherent check.
        decision <- prepareRenderFreshnessDecision
          snapshotEnv
          nowMs
          readWakeInputs
          (claimScreenshotRequest (aaScreenshotRef actors))
          eventCacheState
        let snapVersion = rfdSnapshotVersion decision
            renderSnap = rfdRenderSnapshot decision
            cacheState0 = rfdCacheState decision
            wakeInputs = rfdWakeInputs decision
            generating = rwiGenerating wakeInputs
            atlasPendingCount = rwiAtlasPendingCount wakeInputs
            atlasQueueState = rwiAtlasQueueState wakeInputs
            screenshotClaim = rfdScreenshotClaim decision
            handleElapsed = eprElapsedMs eventResult
            screenshotPending = isJust screenshotClaim
            snapshotElapsed = initialSnapshotElapsed + rfdSnapshotElapsed decision
            atlasPending = atlasPendingCount > 0
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
            finalPollResult = rfdFinalSnapshotPollResult decision
        if shouldSkipUnchangedFrame
            isVersionUnchanged generating screenshotPending maintenanceDue
          then do
            diagnosticNow <- getMonotonicTimeNSec
            advanceState <- readIORef snapshotAdvanceRef
            let gateDiagnostic = snapshotGateDiagnostic
                  SnapshotGateSkip
                  finalPollResult
                  (sasLastRenderedVersion advanceState)
                  screenshotPending
                  wakeInputs
                  maintenanceDue
                  (snapshotAdvanceAgeMs diagnosticNow advanceState)
            observeSnapshotGate diagnosticNow gateDiagnostic
            delayStart <- getMonotonicTimeNSec
            SDL.delay (fromIntegral frameDelayMs)
            delayEnd <- getMonotonicTimeNSec
            let delayElapsed = nsToMs delayStart delayEnd
            writeTraceLine
              (renderStepSummary eventsElapsed snapshotElapsed handleElapsed 0 0 delayElapsed True)
            tEnd <- getMonotonicTimeNSec
            let loopMs = nsToMs loopStart tEnd
            when (loopMs >= 100) $ forM_ traceH $ \h -> do
              hPutStrLn h $ "IDLE loop=" <> show loopMs <> "ms poll=" <> show (nsToMs loopStart tPoll) <> " snap=" <> show (nsToMs tPoll tSnap) <> " handle=" <> show (nsToMs tSnap tHandle) <> " delayBranch=" <> show (nsToMs tHandle tEnd) <> " events=" <> show (length events) <> " v=" <> show (unSnapshotVersion snapVersion)
              hFlush h
            if quit
              then pure cacheState0
              else loop cacheState0
          else do
            wakeNow <- getMonotonicTimeNSec
            advanceStateBefore <- readIORef snapshotAdvanceRef
            let wakeDiagnostic = snapshotGateDiagnostic
                  SnapshotGateWake
                  finalPollResult
                  (sasLastRenderedVersion advanceStateBefore)
                  screenshotPending
                  wakeInputs
                  maintenanceDue
                  (snapshotAdvanceAgeMs wakeNow advanceStateBefore)
            observeSnapshotGate wakeNow wakeDiagnostic
            when (isVersionUnchanged && maintenanceDue) $
              writeTraceLine
                ("WAKE " <> renderMaintenanceWakeSummary atlasPendingCount maintenanceDiagnostics
                  <> " v=" <> show (unSnapshotVersion snapVersion))
            let frameSnapVersion = snapVersion
                frameRenderSnap = renderSnap
                frameCacheState0 = cacheState0
                snapshotElapsedForSummary = snapshotElapsed
            tElseBranch <- getMonotonicTimeNSec
            tAfterWrite <- getMonotonicTimeNSec
            frameResult <- renderFrameStep
              renderFrameEnv
              renderFrameSettings
              nowMs
              frameSnapVersion
              frameRenderSnap
              atlasPendingCount
              atlasQueueState
              screenshotClaim
              frameCacheState0
            let cacheState' = rfrCacheState frameResult
                terrainElapsed = rfrTerrainElapsed frameResult
                frameElapsed = rfrFrameElapsed frameResult
                letsElapsed = rfrLetsElapsed frameResult
                postFrameElapsed = rfrPostFrameElapsed frameResult
            renderedAt <- getMonotonicTimeNSec
            let advanceStateAfter = recordRenderedSnapshot
                  renderedAt frameSnapVersion advanceStateBefore
                renderDiagnostic = snapshotGateDiagnostic
                  SnapshotGateRender
                  finalPollResult
                  (sasLastRenderedVersion advanceStateAfter)
                  screenshotPending
                  wakeInputs
                  maintenanceDue
                  (snapshotAdvanceAgeMs renderedAt advanceStateAfter)
            writeIORef snapshotAdvanceRef advanceStateAfter
            observeSnapshotGate renderedAt renderDiagnostic
            delayStart <- getMonotonicTimeNSec
            SDL.delay (fromIntegral frameDelayMs)
            delayEnd <- getMonotonicTimeNSec
            let delayElapsed = nsToMs delayStart delayEnd
            tPostDelay <- getMonotonicTimeNSec
            writeTraceLine
              (renderStepSummary eventsElapsed snapshotElapsedForSummary handleElapsed terrainElapsed frameElapsed delayElapsed False)
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
