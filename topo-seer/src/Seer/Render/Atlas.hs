module Seer.Render.Atlas
  ( AtlasTextureCache(..)
  , emptyAtlasTextureCache
  , collectAtlasTextures
  , drawAtlas
  , drainAtlasBuildResults
  , resolveAtlasTiles
  , scheduleAtlasBuilds
  , zoomTextureScale
  ) where

import Actor.AtlasCache (AtlasKey(..))
import Actor.AtlasResult (AtlasBuildResult(..))
import Actor.AtlasResultBroker (AtlasResultRef, drainAtlasResultsN)
import Actor.AtlasScheduleBroker
  ( AtlasScheduleReport(..)
  , AtlasScheduleRef
  , readAtlasScheduleRef
  )
import Actor.AtlasScheduler
  ( AtlasScheduleRequest(..)
  , AtlasScheduler
  , requestAtlasSchedule
  )
import Actor.Data (TerrainSnapshot(..))
import Actor.Render (RenderSnapshot(..))
import Actor.SnapshotReceiver (SnapshotVersion)
import Actor.UI (UiState(..))
import Control.Monad (foldM, unless, when)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Word (Word32, Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Hyperspace.Actor (ActorHandle, Protocol)
import Linear (V2(..))
import Data.IORef ()
import qualified SDL
import Seer.Render.ZoomStage (ZoomStage(..))
import Seer.Timing (nsToMs, timedMs)
import UI.TerrainAtlas (TerrainAtlasTile(..), renderAtlasTileTextures)
import UI.Widgets (Rect(..))
import UI.WidgetsDraw (rectToSDL)

-- | Render-thread-owned cache of atlas textures keyed by scale.
data AtlasTextureCache = AtlasTextureCache
  { atcKey :: !(Maybe AtlasKey)
  , atcCaches :: !(IntMap.IntMap [TerrainAtlasTile])
  , atcMaxEntries :: !Int
  , atcLru :: ![Int]
  , atcPending :: ![SDL.Texture]
  , atcLast :: !(Maybe (AtlasKey, [TerrainAtlasTile]))
  }

-- | Create an empty atlas texture cache.
emptyAtlasTextureCache :: Int -> AtlasTextureCache
emptyAtlasTextureCache maxEntries = AtlasTextureCache
  { atcKey = Nothing
  , atcCaches = IntMap.empty
  , atcMaxEntries = maxEntries
  , atcLru = []
  , atcPending = []
  , atcLast = Nothing
  }

-- | Collect all textures currently held by the atlas cache.
collectAtlasTextures :: AtlasTextureCache -> [SDL.Texture]
collectAtlasTextures cache =
  atcPending cache ++ collectTextures (atcCaches cache)

-- | Draw atlas tiles to the renderer.
drawAtlas :: SDL.Renderer -> [TerrainAtlasTile] -> (Float, Float) -> Float -> V2 Int -> IO ()
drawAtlas renderer tiles (panX, panY) zoom (V2 winW winH) =
  mapM_ drawTile tiles
  where
    drawTile tile = do
      let Rect (V2 x y, V2 w h) = tatBounds tile
          Rect (V2 tx ty, V2 tw th) = transformRect panX panY zoom (Rect (V2 x y, V2 w h))
          outside = tx > winW || ty > winH || tx + tw < 0 || ty + th < 0
      if outside
        then pure ()
        else SDL.copy renderer (tatTexture tile) Nothing (Just (rectToSDL (Rect (V2 tx ty, V2 tw th))))

    transformRect px py z (Rect (V2 rx ry, V2 rw rh)) =
      let fx = (fromIntegral rx + px) * z
          fy = (fromIntegral ry + py) * z
          fw = fromIntegral rw * z
          fh = fromIntegral rh * z
      in Rect (V2 (round fx) (round fy), V2 (max 1 (round fw)) (max 1 (round fh)))

-- | Drain atlas build results and upload textures.
--
-- Reads from a shared 'AtlasResultRef' (lock-free) instead of a
-- synchronous actor call.
drainAtlasBuildResults
  :: Bool
  -> Int
  -> SDL.Renderer
  -> AtlasTextureCache
  -> AtlasResultRef
  -> IO (AtlasTextureCache, Int, Word32)
drainAtlasBuildResults renderTargetOk perFrame renderer atlasCache resultRef = do
  results <- drainAtlasResultsN resultRef perFrame
  let resultCount = length results
  if renderTargetOk
    then do
      (cache', totalMs) <- foldM cacheStep (atlasCache, 0) results
      pure (cache', resultCount, totalMs)
    else pure (atlasCache, resultCount, 0)
  where
    cacheStep (cache, totalMs) result = do
      start <- getMonotonicTimeNSec
      tiles <- renderAtlasTileTextures renderer [abrTile result]
      end <- getMonotonicTimeNSec
      let elapsedMs = nsToMs start end
          cache' = if null tiles
            then cache
            else storeAtlasTiles (abrKey result) (abrHexRadius result) tiles cache
      pure (cache', totalMs + elapsedMs)

-- | Schedule atlas build work when rendering with atlas tiles.
--
-- Sends a scheduling request (cast), then reads the latest report from
-- a shared 'AtlasScheduleRef' (lock-free) instead of a synchronous call.
scheduleAtlasBuilds
  :: Bool
  -> Bool
  -> ActorHandle AtlasScheduler (Protocol AtlasScheduler)
  -> AtlasScheduleRef
  -> SnapshotVersion
  -> RenderSnapshot
  -> (Int, Int)
  -> IO (Int, Word32, Word32)
scheduleAtlasBuilds renderTargetOk dataReady atlasSchedulerHandle scheduleRef snapshotVersion snapshot windowSize = do
  requestAtlasSchedule atlasSchedulerHandle AtlasScheduleRequest
    { asqSnapshotVersion = snapshotVersion
    , asqRenderTargetOk = renderTargetOk
    , asqDataReady = dataReady
    , asqSnapshot = snapshot
    , asqWindowSize = windowSize
    }
  mbReport <- readAtlasScheduleRef scheduleRef
  case mbReport of
    Just report | asrSnapshotVersion report == snapshotVersion ->
      pure (asrJobCount report, asrDrainMs report, asrEnqueueMs report)
    _ -> pure (0, 0, 0)

-- | Resolve which atlas tiles to draw and clean up pending textures.
resolveAtlasTiles
  :: Bool
  -> RenderSnapshot
  -> AtlasTextureCache
  -> ZoomStage
  -> IO (Maybe [TerrainAtlasTile], AtlasTextureCache)
resolveAtlasTiles renderTargetOk snapshot atlasCache stage = do
  let terrainSnap = rsTerrain snapshot
      atlasKey = AtlasKey (uiViewMode (rsUi snapshot)) (uiRenderWaterLevel (rsUi snapshot)) (tsVersion terrainSnap)
      dataReady = tsChunkSize terrainSnap > 0 && not (IntMap.null (tsTerrainChunks terrainSnap))
      atlasTiles = if renderTargetOk && dataReady
        then getNearestAtlas atlasKey (zsHexRadius stage) atlasCache
        else Nothing
      atlasToDraw = case atlasTiles of
        Just tiles | not (null tiles) -> Just tiles
        _ -> case atcLast atlasCache of
          Just (_key, tiles) | not (null tiles) -> Just tiles
          _ -> Nothing
      cacheWithLast = case atlasTiles of
        Just tiles | not (null tiles) ->
          let retiredTextures = case atcLast atlasCache of
                Just (oldKey, oldTiles)
                  | oldKey /= atlasKey -> map tatTexture oldTiles
                _ -> []
          in atlasCache
              { atcLast = Just (atlasKey, tiles)
              , atcPending = retiredTextures ++ atcPending atlasCache
              }
        _ -> atlasCache
      cacheTouched = case atlasToDraw of
        Just (t:_) -> touchAtlasScale (tatHexRadius t) cacheWithLast
        _ -> cacheWithLast
      (pending, cacheDrained) = drainAtlasPending cacheTouched
      (keepAlive, destroyNow) = case atcLast cacheDrained of
        Just (_key, tiles) ->
          let alive = map tatTexture tiles
              shouldKeep texture = texture `elem` alive
          in (filter shouldKeep pending, filter (not . shouldKeep) pending)
        _ -> ([], pending)
      cacheFinal = cacheDrained { atcPending = keepAlive }
  unless (null destroyNow) $
    mapM_ SDL.destroyTexture destroyNow
  pure (atlasToDraw, cacheFinal)

zoomTextureScale :: Float -> Int
zoomTextureScale zoom =
  let target = ceiling (zoom * 2)
  in max 1 (min 6 target)

setAtlasKey :: AtlasKey -> AtlasTextureCache -> AtlasTextureCache
setAtlasKey key cache =
  if atcKey cache == Just key
    then cache
    else cache
      { atcKey = Just key
      , atcCaches = IntMap.empty
      , atcLru = []
      , atcPending = collectTextures (atcCaches cache) ++ atcPending cache
      , atcLast = Nothing
      }

storeAtlasTiles :: AtlasKey -> Int -> [TerrainAtlasTile] -> AtlasTextureCache -> AtlasTextureCache
storeAtlasTiles key scale tiles cache =
  let targetCache =
        if atcKey cache == Just key
          then cache
          else setAtlasKey key cache
      (merged, pending) = mergeTiles (IntMap.lookup scale (atcCaches targetCache)) tiles
      cache' = targetCache
        { atcCaches = IntMap.insert scale merged (atcCaches targetCache)
        , atcLru = touch scale (atcLru targetCache)
        , atcPending = pending ++ atcPending targetCache
        }
  in evictIfNeeded cache'

getNearestAtlas :: AtlasKey -> Int -> AtlasTextureCache -> Maybe [TerrainAtlasTile]
getNearestAtlas key target cache =
  if atcKey cache == Just key
    then nearestAtlas target (atcCaches cache)
    else Nothing

touchAtlasScale :: Int -> AtlasTextureCache -> AtlasTextureCache
touchAtlasScale scale cache =
  cache { atcLru = touch scale (atcLru cache) }

drainAtlasPending :: AtlasTextureCache -> ([SDL.Texture], AtlasTextureCache)
drainAtlasPending cache =
  (atcPending cache, cache { atcPending = [] })

nearestAtlas :: Int -> IntMap.IntMap [TerrainAtlasTile] -> Maybe [TerrainAtlasTile]
nearestAtlas _ caches | IntMap.null caches = Nothing
nearestAtlas target caches =
  let best = IntMap.foldlWithKey' (pickBest target) Nothing caches
  in fmap snd best

pickBest :: Int -> Maybe (Int, [TerrainAtlasTile]) -> Int -> [TerrainAtlasTile] -> Maybe (Int, [TerrainAtlasTile])
pickBest target current scale atlas =
  case current of
    Nothing -> Just (scale, atlas)
    Just (bestScale, _)
      | abs (scale - target) < abs (bestScale - target) -> Just (scale, atlas)
      | otherwise -> current

cleanLru :: Int -> [Int] -> [Int]
cleanLru scale = filter (/= scale)

touch :: Int -> [Int] -> [Int]
touch scale lru = scale : cleanLru scale lru

evictIfNeeded :: AtlasTextureCache -> AtlasTextureCache
evictIfNeeded cache
  | length (atcLru cache) <= atcMaxEntries cache = cache
  | otherwise =
      let (toKeep, toDrop) = splitAt (atcMaxEntries cache) (atcLru cache)
          dropSet = IntSet.fromList toDrop
          (evicted, remaining) = IntMap.partitionWithKey (\k _ -> IntSet.member k dropSet) (atcCaches cache)
          pending = collectTextures evicted
      in cache
        { atcCaches = remaining
        , atcLru = toKeep
        , atcPending = pending ++ atcPending cache
        }

collectTextures :: IntMap.IntMap [TerrainAtlasTile] -> [SDL.Texture]
collectTextures caches =
  concatMap (map tatTexture) (IntMap.elems caches)

mergeTiles :: Maybe [TerrainAtlasTile] -> [TerrainAtlasTile] -> ([TerrainAtlasTile], [SDL.Texture])
mergeTiles Nothing newTiles = (newTiles, [])
mergeTiles (Just existing) newTiles =
  let newBounds = map tatBounds newTiles
      (replaced, kept) = partitionByBounds newBounds existing
      pending = map tatTexture replaced
  in (newTiles ++ kept, pending)

partitionByBounds :: [Rect] -> [TerrainAtlasTile] -> ([TerrainAtlasTile], [TerrainAtlasTile])
partitionByBounds bounds tiles =
  foldr (splitOne bounds) ([], []) tiles
  where
    splitOne bounds' tile (replaced, kept)
      | tatBounds tile `elem` bounds' = (tile : replaced, kept)
      | otherwise = (replaced, tile : kept)
