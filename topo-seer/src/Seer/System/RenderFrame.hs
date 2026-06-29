module Seer.System.RenderFrame
  ( RenderFrameEnv(..)
  , RenderFrameSettings(..)
  , RenderFrameStepResult(..)
  , renderFrameStep
  ) where

import Actor.AtlasResultBroker (AtlasResultRef)
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
import Seer.Render (RenderContext(..), renderFrame)
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
      shouldPollTerrain = not generating && shouldPoll nowMs (rfsetTerrainCachePollMs settings) (rcsLastTerrainPoll cacheState0)
      shouldDrainAtlas = not generating && shouldPoll nowMs (rfsetAtlasDrainPollMs settings) (rcsLastAtlasDrain cacheState0)
      shouldScheduleAtlas = not generating && shouldPoll nowMs (rfsetAtlasSchedulePollMs settings) (rcsLastAtlasSchedule cacheState0)
      shouldUpdateChunkTextures = not generating && shouldPoll nowMs (rfsetChunkTexturePollMs settings) (rcsLastChunkTexturePoll cacheState0)
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
  frameStart <- getMonotonicTimeNSec
  (needsRetry, nextChunkTextures, nextAtlasCache, _didLog) <-
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
      cacheState = cacheState'
        { rcsChunkTextures = nextChunkTextures
        , rcsAtlasCache = nextAtlasCache
        , rcsLastSnapshot = if needsRetry then Nothing else Just snapVersion
        , rcsLastSnapshotData = Just renderSnap
        , rcsLastAtlasDrain = if shouldDrainAtlas then Just nowMs else rcsLastAtlasDrain cacheState'
        , rcsLastAtlasSchedule = if shouldScheduleAtlas then Just nowMs else rcsLastAtlasSchedule cacheState'
        , rcsLastChunkTexturePoll = if shouldUpdateChunkTextures then Just nowMs else rcsLastChunkTexturePoll cacheState'
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
