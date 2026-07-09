module Seer.System.Cache
  ( RenderCacheState(..)
  , initialRenderCacheState
  , destroyRenderCacheState
  , applyTerrainCacheUpdate
  , renderMetrics
  , renderAtlasDiagnosticSummary
  , renderStepSummary
  , shouldPoll
  ) where

import Actor.AtlasCache (atlasKeyForSelection)
import Actor.AtlasManager (AtlasManagerQueueState(..), emptyAtlasManagerQueueState, formatAtlasManagerQueueState)
import Actor.AtlasResultBroker (AtlasResultDrainStats(..), formatAtlasResultDrainStats)
import Actor.AtlasScheduleBroker (AtlasScheduleReport(..), formatAtlasScheduleReport)
import Actor.Data (TerrainSnapshot(..))
import Actor.Render (RenderSnapshot(..))
import Actor.SnapshotReceiver (SnapshotVersion(..))
import Actor.TerrainCacheBroker (TerrainCacheRef, readTerrainCacheRef)
import Actor.TerrainCacheWorker
  ( TerrainCacheBuildRequest(..)
  , TerrainCacheKey
  , TerrainCacheWorker
  , requestTerrainCacheBuild
  , tcrResultCache
  , tcrResultKey
  , terrainCacheKeyFrom
  )
import Actor.UI (UiState(..), effectiveViewSelection)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Word (Word32, Word64)
import Hyperspace.Actor (ActorHandle, Protocol)
import Seer.Render (TerrainCache(..), emptyTerrainCache, terrainCacheNeedsRefresh)
import Seer.Render.Atlas
  ( AtlasResolveStatus(..)
  , AtlasTextureCache(..)
  , CachedAtlasTileSet(..)
  , atlasCacheSummary
  , atlasResolveStatusLabel
  , collectAtlasTextures
  , emptyAtlasTextureCache
  , formatAtlasCacheSummary
  )
import Seer.Render.ZoomStage (stageForZoom)
import UI.TerrainCache (ChunkTextureCache(..), emptyChunkTextureCache)
import UI.TerrainRender (destroyChunkTexture)
import UI.TexturePool (TexturePool, releaseTexture)

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
  , rcsAtlasNeedsRetry :: !Bool
  , rcsLastAtlasResolveStatus :: !AtlasResolveStatus
  , rcsLastAtlasDrainStats :: !(Maybe AtlasResultDrainStats)
  , rcsLastAtlasDrainAttempted :: !Bool
  , rcsLastAtlasScheduleAttempted :: !Bool
  , rcsLastAtlasScheduleReport :: !(Maybe AtlasScheduleReport)
  , rcsLastAtlasPendingCount :: !Int
  , rcsLastAtlasQueueState :: !AtlasManagerQueueState
  , rcsLastAtlasQueuedCount :: !Int
  , rcsLastAtlasQueuedRevision :: !(Maybe Word64)
  , rcsLastAtlasQueuedRevisionScheduled :: !(Maybe Word64)
  , rcsLastChunkTexturePoll :: !(Maybe Word32)
  }

initialRenderCacheState :: Int -> RenderCacheState
initialRenderCacheState atlasCacheEntries = RenderCacheState
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
  , rcsAtlasNeedsRetry = False
  , rcsLastAtlasResolveStatus = Missing
  , rcsLastAtlasDrainStats = Nothing
  , rcsLastAtlasDrainAttempted = False
  , rcsLastAtlasScheduleAttempted = False
  , rcsLastAtlasScheduleReport = Nothing
  , rcsLastAtlasPendingCount = 0
  , rcsLastAtlasQueueState = emptyAtlasManagerQueueState
  , rcsLastAtlasQueuedCount = 0
  , rcsLastAtlasQueuedRevision = Nothing
  , rcsLastAtlasQueuedRevisionScheduled = Nothing
  , rcsLastChunkTexturePoll = Nothing
  }

destroyRenderCacheState :: TexturePool -> RenderCacheState -> IO ()
destroyRenderCacheState texturePool cacheState = do
  mapM_ destroyChunkTexture (IntMap.elems (ctcTextures (rcsChunkTextures cacheState)))
  mapM_ (releaseTexture texturePool) (collectAtlasTextures (rcsAtlasCache cacheState))

applyTerrainCacheUpdate
  :: RenderSnapshot
  -> ActorHandle TerrainCacheWorker (Protocol TerrainCacheWorker)
  -> TerrainCacheRef
  -> RenderCacheState
  -> IO RenderCacheState
applyTerrainCacheUpdate renderSnap workerHandle cacheRef cacheState = do
  let uiSnap = rsUi renderSnap
      terrainSnap = rsTerrain renderSnap
      dataReady = tsChunkSize terrainSnap > 0 && not (IntMap.null (tsTerrainChunks terrainSnap))
      desiredKey =
        if dataReady
          then terrainCacheKeyFrom uiSnap terrainSnap
          else Nothing
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
                , rcsLastRequest = Nothing
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
          , tcrResultRef = cacheRef
          }
        pure stateAfterResult { rcsLastRequest = Just key }
    else pure stateAfterResult

shouldStartTerrainCacheBuild :: RenderSnapshot -> TerrainCache -> Bool
shouldStartTerrainCacheBuild renderSnap cache =
  let uiSnap = rsUi renderSnap
      terrainSnap = rsTerrain renderSnap
      dataReady = tsChunkSize terrainSnap > 0 && not (IntMap.null (tsTerrainChunks terrainSnap))
  in not (uiGenerating uiSnap)
      && dataReady
      && terrainCacheNeedsRefresh uiSnap terrainSnap cache

renderMetrics :: Word32 -> RenderSnapshot -> RenderCacheState -> Text.Text
renderMetrics frameMs snap cacheState =
  let terrainCount = IntMap.size (tcTerrainChunks (rcsTerrainCache cacheState))
      chunkTextures = IntMap.size (ctcTextures (rcsChunkTextures cacheState))
      atlasScales = length (atcLru (rcsAtlasCache cacheState))
      atlasTiles = sum (map (length . catsTiles) (concatMap IntMap.elems (Map.elems (atcCaches (rcsAtlasCache cacheState)))))
      snapshotVer = case rcsLastSnapshot cacheState of
        Nothing -> "none"
        Just (SnapshotVersion v) -> show v
  in Text.pack
      ("render: frame=" <> show frameMs <> "ms"
        <> " terrainChunks=" <> show terrainCount
        <> " chunkTextures=" <> show chunkTextures
        <> " atlasScales=" <> show atlasScales
        <> " atlasTiles=" <> show atlasTiles
        <> " snapshot=" <> snapshotVer
        <> " " <> renderAtlasDiagnosticSummary snap cacheState)

renderAtlasDiagnosticSummary :: RenderSnapshot -> RenderCacheState -> String
renderAtlasDiagnosticSummary snap cacheState =
  let uiSnap = rsUi snap
      terrainSnap = rsTerrain snap
      rawStage = stageForZoom (uiZoom uiSnap)
      targetStage = maybe rawStage id (atcCommittedStage (rcsAtlasCache cacheState))
      targetKey = atlasKeyForSelection (effectiveViewSelection uiSnap) (uiRenderWaterLevel uiSnap) terrainSnap
      cacheSummary = atlasCacheSummary (Just targetKey) (Just targetStage) (rcsAtlasCache cacheState)
      lastSnapshotDecision = case rcsLastSnapshot cacheState of
        Nothing -> "retry"
        Just (SnapshotVersion v) -> "committed:" <> show v
  in "atlasRetry=" <> show (rcsAtlasNeedsRetry cacheState)
    <> " atlasResolve=" <> atlasResolveStatusLabel (rcsLastAtlasResolveStatus cacheState)
    <> " atlasPending=" <> show (rcsLastAtlasPendingCount cacheState)
    <> " atlasQueued=" <> show (rcsLastAtlasQueuedCount cacheState)
    <> " atlasQueuedRev=" <> maybe "none" show (rcsLastAtlasQueuedRevision cacheState)
    <> " atlasQueuedScheduledRev=" <> maybe "none" show (rcsLastAtlasQueuedRevisionScheduled cacheState)
    <> " atlasDrainAttempted=" <> show (rcsLastAtlasDrainAttempted cacheState)
    <> " atlasScheduleAttempted=" <> show (rcsLastAtlasScheduleAttempted cacheState)
    <> " scheduleJobs=" <> maybe "none" (show . asrJobCount) (rcsLastAtlasScheduleReport cacheState)
    <> " lastSnapshotDecision=" <> lastSnapshotDecision
    <> maybe "" ((" " <>) . formatAtlasScheduleReport) (rcsLastAtlasScheduleReport cacheState)
    <> " " <> formatAtlasManagerQueueState (rcsLastAtlasQueueState cacheState)
    <> maybe "" ((" " <>) . formatAtlasResultDrainStats) (rcsLastAtlasDrainStats cacheState)
    <> " " <> formatAtlasCacheSummary cacheSummary

shouldPoll :: Word32 -> Int -> Maybe Word32 -> Bool
shouldPoll nowMs pollMs lastPoll =
  case lastPoll of
    Nothing -> True
    Just prev -> nowMs - prev >= fromIntegral pollMs

renderStepSummary :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Bool -> String
renderStepSummary pollMs snapshotMs handleMs terrainMs frameMs delayMs unchangedSnapshot =
  "render steps: poll=" <> show pollMs <> "ms"
    <> " snapshot=" <> show snapshotMs <> "ms"
    <> " handle=" <> show handleMs <> "ms"
    <> " terrain=" <> show terrainMs <> "ms"
    <> " frame=" <> show frameMs <> "ms"
    <> " delay=" <> show delayMs <> "ms"
    <> " unchangedSnapshot=" <> show unchangedSnapshot
