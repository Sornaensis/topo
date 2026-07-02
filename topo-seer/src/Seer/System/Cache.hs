module Seer.System.Cache
  ( RenderCacheState(..)
  , initialRenderCacheState
  , destroyRenderCacheState
  , applyTerrainCacheUpdate
  , renderMetrics
  , renderStepSummary
  , shouldPoll
  ) where

import Actor.AtlasCache (terrainSnapshotViewVersion)
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
import Actor.UI (UiState(..))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Word (Word32)
import Hyperspace.Actor (ActorHandle, Protocol)
import Seer.Render (TerrainCache(..), emptyTerrainCache)
import Seer.Render.Atlas
  ( AtlasTextureCache(..)
  , collectAtlasTextures
  , emptyAtlasTextureCache
  )
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
            { rcsLastRequest = Nothing
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
          , tcrResultRef = cacheRef
          }
        pure stateAfterResult { rcsLastRequest = Just key }
    else pure stateAfterResult

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
  | tcVersion cache /= terrainSnapshotViewVersion (uiViewMode uiSnap) terrainSnap = True
  | otherwise = False

renderMetrics :: Word32 -> RenderSnapshot -> RenderCacheState -> Text.Text
renderMetrics frameMs _snap cacheState =
  let terrainCount = IntMap.size (tcTerrainChunks (rcsTerrainCache cacheState))
      chunkTextures = IntMap.size (ctcTextures (rcsChunkTextures cacheState))
      atlasScales = length (atcLru (rcsAtlasCache cacheState))
      atlasTiles = sum (map length (concatMap IntMap.elems (Map.elems (atcCaches (rcsAtlasCache cacheState)))))
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

renderStepSummary :: Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Word32 -> Bool -> String
renderStepSummary pollMs snapshotMs handleMs terrainMs frameMs delayMs unchangedSnapshot =
  "render steps: poll=" <> show pollMs <> "ms"
    <> " snapshot=" <> show snapshotMs <> "ms"
    <> " handle=" <> show handleMs <> "ms"
    <> " terrain=" <> show terrainMs <> "ms"
    <> " frame=" <> show frameMs <> "ms"
    <> " delay=" <> show delayMs <> "ms"
    <> " unchangedSnapshot=" <> show unchangedSnapshot
