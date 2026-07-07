module Seer.System.RenderFrame
  ( RenderFrameEnv(..)
  , RenderFrameSettings(..)
  , RenderFrameStepResult(..)
  , RenderFrameMaintenanceDiagnostics(..)
  , renderFrameStep
  , renderFrameStepMaintenance
  , renderFrameStepMaintenanceDue
  ) where

import Actor.AtlasFreshness (AtlasFreshnessRef)
import Actor.AtlasResultBroker (AtlasResultRef, atlasResultsPendingCount)
import Actor.AtlasScheduleBroker (AtlasScheduleRef)
import Actor.AtlasScheduler (AtlasScheduler)
import Actor.Log (Log, LogEntry(..), LogLevel(..), appendLog)
import Actor.Render (RenderSnapshot(..))
import Actor.SnapshotReceiver (SnapshotVersion)
import Actor.TerrainCacheBroker (TerrainCacheRef)
import Actor.TerrainCacheWorker (TerrainCacheWorker)
import Actor.UI (UiState(..))
import qualified Data.Text as Text
import Data.Word (Word32)
import GHC.Clock (getMonotonicTimeNSec)
import Hyperspace.Actor (ActorHandle, Protocol)
import qualified SDL
import Seer.Render
  ( RenderContext(..)
  , RenderFrameOutcome(..)
  , fallbackTerrainNeedsRefresh
  , renderFrame
  , terrainCacheNeedsRefresh
  )
import Seer.Render.Atlas (AtlasTextureCache(..))
import Seer.Render.Frame
  ( AtlasFrameStepPolicy(..)
  , applyAtlasFrameStepTimestamps
  , atlasFrameStepPolicy
  , fallbackFrameMaintenanceDue
  )
import Seer.Render.ZoomStage (ZoomStage(..), stageForZoom)
import Seer.Screenshot.Request (ScreenshotRequestRef)
import Seer.System.Cache
  ( RenderCacheState(..)
  , applyTerrainCacheUpdate
  , renderMetrics
  , shouldPoll
  )
import Seer.Timing (nsToMs)
import UI.Font (FontCache)
import UI.TexturePool (TexturePool)
import System.IO (Handle)

data RenderFrameEnv = RenderFrameEnv
  { rfeRenderer :: !SDL.Renderer
  , rfeWindow :: !SDL.Window
  , rfeRenderTargetOk :: !Bool
  , rfeFontCache :: !(Maybe FontCache)
  , rfeTexturePool :: !TexturePool
  , rfeLogHandle :: !(ActorHandle Log (Protocol Log))
  , rfeTerrainCacheWorkerHandle :: !(ActorHandle TerrainCacheWorker (Protocol TerrainCacheWorker))
  , rfeTerrainCacheRef :: !TerrainCacheRef
  , rfeAtlasSchedulerHandle :: !(ActorHandle AtlasScheduler (Protocol AtlasScheduler))
  , rfeAtlasScheduleRef :: !AtlasScheduleRef
  , rfeAtlasResultRef :: !AtlasResultRef
  , rfeAtlasFreshnessRef :: !AtlasFreshnessRef
  , rfeScreenshotRef :: !ScreenshotRequestRef
  , rfeTraceHandle :: !(Maybe Handle)
  }

data RenderFrameSettings = RenderFrameSettings
  { rfsetAtlasUploadsPerFrame :: !Int
  , rfsetTerrainCachePollMs :: !Int
  , rfsetAtlasDrainPollMs :: !Int
  , rfsetAtlasSchedulePollMs :: !Int
  , rfsetChunkTexturePollMs :: !Int
  , rfsetTimingLogThresholdMs :: !Word32
  }

data RenderFrameStepResult = RenderFrameStepResult
  { rfrCacheState :: !RenderCacheState
  , rfrTerrainElapsed :: !Word32
  , rfrFrameElapsed :: !Word32
  , rfrLetsElapsed :: !Word32
  , rfrPostFrameElapsed :: !Word32
  }

data RenderFrameMaintenanceDiagnostics = RenderFrameMaintenanceDiagnostics
  { rfmdAtlasPendingWake :: !Bool
  , rfmdAtlasScheduleRetryWake :: !Bool
  , rfmdFallbackTerrainWake :: !Bool
  , rfmdDrainAttempted :: !Bool
  , rfmdScheduleAttempted :: !Bool
  , rfmdMaintenanceDue :: !Bool
  } deriving (Eq, Show)

-- | Explain whether an unchanged-snapshot loop has render-thread maintenance
-- that should wake the renderer instead of idling.
renderFrameStepMaintenance
  :: RenderFrameSettings
  -> Bool
  -> Word32
  -> Bool
  -> RenderSnapshot
  -> RenderCacheState
  -> RenderFrameMaintenanceDiagnostics
renderFrameStepMaintenance settings renderTargetOk nowMs atlasPending renderSnap cacheState =
  let generating = uiGenerating (rsUi renderSnap)
      fallbackMaintenanceDue = fallbackFrameMaintenanceDue
        renderTargetOk
        (rsUi renderSnap)
        (rsTerrain renderSnap)
        (fallbackTextureScale renderSnap (rcsAtlasCache cacheState))
        (rcsTerrainCache cacheState)
        (rcsChunkTextures cacheState)
      atlasPolicy = atlasFrameStepPolicy
        nowMs
        (rfsetAtlasDrainPollMs settings)
        (rfsetAtlasSchedulePollMs settings)
        generating
        renderTargetOk
        atlasPending
        (rcsAtlasNeedsRetry cacheState)
        (rcsLastAtlasDrain cacheState)
        (rcsLastAtlasSchedule cacheState)
      scheduleRetryWake = not generating
        && renderTargetOk
        && rcsAtlasNeedsRetry cacheState
        && afspShouldScheduleAtlas atlasPolicy
      atlasPendingWake = not generating && renderTargetOk && atlasPending && afspShouldDrainAtlas atlasPolicy
      maintenanceDue = not generating && (fallbackMaintenanceDue || afspAtlasMaintenanceDue atlasPolicy)
  in RenderFrameMaintenanceDiagnostics
    { rfmdAtlasPendingWake = atlasPendingWake
    , rfmdAtlasScheduleRetryWake = scheduleRetryWake
    , rfmdFallbackTerrainWake = fallbackMaintenanceDue
    , rfmdDrainAttempted = afspShouldDrainAtlas atlasPolicy
    , rfmdScheduleAttempted = afspShouldScheduleAtlas atlasPolicy
    , rfmdMaintenanceDue = maintenanceDue
    }

-- | Whether an unchanged-snapshot loop has render-thread maintenance that
-- should wake the renderer instead of idling.
renderFrameStepMaintenanceDue
  :: RenderFrameSettings
  -> Bool
  -> Word32
  -> Bool
  -> RenderSnapshot
  -> RenderCacheState
  -> Bool
renderFrameStepMaintenanceDue settings renderTargetOk nowMs atlasPending renderSnap cacheState =
  rfmdMaintenanceDue $
    renderFrameStepMaintenance settings renderTargetOk nowMs atlasPending renderSnap cacheState

fallbackTextureScale :: RenderSnapshot -> AtlasTextureCache -> Int
fallbackTextureScale renderSnap atlasCache =
  maybe rawScale zsAtlasScale (atcCommittedStage atlasCache)
  where
    rawScale = zsAtlasScale (stageForZoom (uiZoom (rsUi renderSnap)))

renderFrameStep
  :: RenderFrameEnv
  -> RenderFrameSettings
  -> Word32
  -> SnapshotVersion
  -> RenderSnapshot
  -> RenderCacheState
  -> IO RenderFrameStepResult
renderFrameStep env settings nowMs snapVersion renderSnap cacheState0 = do
  stepStart <- getMonotonicTimeNSec
  -- Skip terrain cache, atlas, and chunk texture polling while generating; those
  -- operations are no-ops then and can otherwise stall the SDL render thread.
  let generating = uiGenerating (rsUi renderSnap)
  atlasPendingCount <- if generating
    then pure 0
    else atlasResultsPendingCount (rfeAtlasResultRef env)
  let atlasPending = atlasPendingCount > 0
  let fallbackTerrainStale = not (rfeRenderTargetOk env)
        && terrainCacheNeedsRefresh (rsUi renderSnap) (rsTerrain renderSnap) (rcsTerrainCache cacheState0)
      shouldPollTerrain = not generating
        && (fallbackTerrainStale || shouldPoll nowMs (rfsetTerrainCachePollMs settings) (rcsLastTerrainPoll cacheState0))
      atlasPolicy = atlasFrameStepPolicy
        nowMs
        (rfsetAtlasDrainPollMs settings)
        (rfsetAtlasSchedulePollMs settings)
        generating
        (rfeRenderTargetOk env)
        atlasPending
        (rcsAtlasNeedsRetry cacheState0)
        (rcsLastAtlasDrain cacheState0)
        (rcsLastAtlasSchedule cacheState0)
      shouldDrainAtlas = afspShouldDrainAtlas atlasPolicy
      shouldScheduleAtlas = afspShouldScheduleAtlas atlasPolicy
  tAfterLets <- getMonotonicTimeNSec
  (cacheState', terrainElapsed) <-
    if shouldPollTerrain
      then do
        cacheStart <- getMonotonicTimeNSec
        updated <- applyTerrainCacheUpdate renderSnap (rfeTerrainCacheWorkerHandle env) (rfeTerrainCacheRef env) cacheState0
        cacheEnd <- getMonotonicTimeNSec
        let cacheElapsed = nsToMs cacheStart cacheEnd
        if cacheElapsed >= rfsetTimingLogThresholdMs settings
          then appendLog (rfeLogHandle env) (LogEntry LogInfo (Text.pack ("terrain cache check took " <> show cacheElapsed <> "ms")))
          else pure ()
        pure (updated { rcsLastTerrainPoll = Just nowMs }, cacheElapsed)
      else pure (cacheState0, 0)
  let fallbackTerrainFresh = not (terrainCacheNeedsRefresh (rsUi renderSnap) (rsTerrain renderSnap) (rcsTerrainCache cacheState'))
      fallbackTextureStale = not (rfeRenderTargetOk env)
        && fallbackTerrainFresh
        && fallbackTerrainNeedsRefresh
             (rsUi renderSnap)
             (rsTerrain renderSnap)
             (fallbackTextureScale renderSnap (rcsAtlasCache cacheState'))
             (rcsTerrainCache cacheState')
             (rcsChunkTextures cacheState')
      shouldUpdateChunkTextures = not generating
        && not (rfeRenderTargetOk env)
        && fallbackTerrainFresh
        && (fallbackTextureStale || shouldPoll nowMs (rfsetChunkTexturePollMs settings) (rcsLastChunkTexturePoll cacheState'))
  frameStart <- getMonotonicTimeNSec
  frameOutcome <-
    renderFrame RenderContext
      { rcRenderer = rfeRenderer env
      , rcWindow = rfeWindow env
      , rcSnapshotVersion = snapVersion
      , rcSnapshot = renderSnap
      , rcTerrainCache = rcsTerrainCache cacheState'
      , rcChunkTextureCache = rcsChunkTextures cacheState'
      , rcAtlasTextureCache = rcsAtlasCache cacheState'
      , rcLogHandle = rfeLogHandle env
      , rcAtlasSchedulerHandle = rfeAtlasSchedulerHandle env
      , rcAtlasScheduleRef = rfeAtlasScheduleRef env
      , rcAtlasResultRef = rfeAtlasResultRef env
      , rcAtlasFreshnessRef = rfeAtlasFreshnessRef env
      , rcAtlasUploadsPerFrame = rfsetAtlasUploadsPerFrame settings
      , rcShouldDrainAtlas = shouldDrainAtlas
      , rcShouldScheduleAtlas = shouldScheduleAtlas
      , rcShouldUpdateChunkTextures = shouldUpdateChunkTextures
      , rcTimingLogThresholdMs = rfsetTimingLogThresholdMs settings
      , rcFontCache = rfeFontCache env
      , rcRenderTargetOk = rfeRenderTargetOk env
      , rcTraceHandle = rfeTraceHandle env
      , rcScreenshotRef = rfeScreenshotRef env
      , rcTexturePool = rfeTexturePool env
      }
  frameEnd <- getMonotonicTimeNSec
  let frameElapsed = nsToMs frameStart frameEnd
      nextChunkTextures = rfoChunkTextureCache frameOutcome
      nextAtlasCache = rfoAtlasTextureCache frameOutcome
      fallbackNeedsRetry = rfoFallbackNeedsRetry frameOutcome
        || (not (rfeRenderTargetOk env)
              && fallbackTerrainNeedsRefresh
                   (rsUi renderSnap)
                   (rsTerrain renderSnap)
                   (fallbackTextureScale renderSnap nextAtlasCache)
                   (rcsTerrainCache cacheState')
                   nextChunkTextures)
      cacheStateBeforeAtlasTimestamps = cacheState'
        { rcsChunkTextures = nextChunkTextures
        , rcsAtlasCache = nextAtlasCache
        , rcsAtlasNeedsRetry = rfoAtlasNeedsRetry frameOutcome
        , rcsLastAtlasResolveStatus = rfoAtlasResolveStatus frameOutcome
        , rcsLastAtlasDrainStats = rfoAtlasDrainStats frameOutcome
        , rcsLastAtlasDrainAttempted = shouldDrainAtlas
        , rcsLastAtlasScheduleAttempted = shouldScheduleAtlas
        , rcsLastAtlasPendingCount = atlasPendingCount
        , rcsLastSnapshot = if fallbackNeedsRetry then Nothing else Just snapVersion
        , rcsLastSnapshotData = Just renderSnap
        , rcsLastChunkTexturePoll = if shouldUpdateChunkTextures then Just nowMs else rcsLastChunkTexturePoll cacheState'
        }
      (lastAtlasDrain, lastAtlasSchedule) = applyAtlasFrameStepTimestamps
        nowMs
        atlasPolicy
        (rcsLastAtlasDrain cacheStateBeforeAtlasTimestamps)
        (rcsLastAtlasSchedule cacheStateBeforeAtlasTimestamps)
      cacheState = cacheStateBeforeAtlasTimestamps
        { rcsLastAtlasDrain = lastAtlasDrain
        , rcsLastAtlasSchedule = lastAtlasSchedule
        }
  if frameElapsed >= rfsetTimingLogThresholdMs settings
    then appendLog (rfeLogHandle env) (LogEntry LogInfo (renderMetrics frameElapsed renderSnap cacheState))
    else pure ()
  stepEnd <- getMonotonicTimeNSec
  pure RenderFrameStepResult
    { rfrCacheState = cacheState
    , rfrTerrainElapsed = terrainElapsed
    , rfrFrameElapsed = frameElapsed
    , rfrLetsElapsed = nsToMs stepStart tAfterLets
    , rfrPostFrameElapsed = nsToMs frameEnd stepEnd
    }
