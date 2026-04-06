module Seer.Render.Atlas
  ( AtlasTextureCache(..)
  , emptyAtlasTextureCache
  , collectAtlasTextures
  , drawAtlas
  , drawAtlasAlpha
  , drainAtlasBuildResults
  , getNearestAtlas
  , resolveAtlasTiles
  , resolveEffectiveStage
  , scheduleAtlasBuilds
  , setAtlasKey
  , storeAtlasTiles
  , touchAtlasScale
  , drainAtlasPending
  , evictIfNeeded
  , zoomTextureScale
  ) where

import Actor.AtlasCache (AtlasKey(..))
import Actor.AtlasResult (AtlasBuildResult(..))
import Actor.AtlasResultBroker (AtlasResultRef, drainAtlasResultsN, drainFreshResultsN)
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
import qualified Data.Map.Strict as Map
import Data.Word (Word8, Word32, Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Hyperspace.Actor (ActorHandle, Protocol)
import Linear (V2(..))
import Data.IORef ()
import qualified SDL
import Seer.Render.ZoomStage (ZoomStage(..))
import Seer.Timing (nsToMs, timedMs)
import UI.TerrainAtlas (TerrainAtlasTile(..), renderAtlasTileTextures)
import UI.TexturePool (TexturePool, releaseTexture)
import UI.Widgets (Rect(..))
import UI.WidgetsDraw (rectToSDL)

-- | Render-thread-owned cache of atlas textures keyed by (AtlasKey, scale).
--
-- Tiles for different view modes, water levels, and day\/night states
-- coexist in a 'Map.Map AtlasKey (IntMap [TerrainAtlasTile])'.
-- Switching the active key ('atcKey') is an O(1) pointer update —
-- no textures are flushed.
data AtlasTextureCache = AtlasTextureCache
  { atcKey :: !(Maybe AtlasKey)
  , atcCaches :: !(Map.Map AtlasKey (IntMap.IntMap [TerrainAtlasTile]))
  , atcMaxEntries :: !Int
  , atcLru :: ![(AtlasKey, Int)]
  , atcPending :: ![SDL.Texture]
  , atcLast :: !(Maybe (AtlasKey, [TerrainAtlasTile]))
  , atcCommittedStage :: !(Maybe ZoomStage)
  , atcStageChangeNs :: !Word64
  }

-- | Create an empty atlas texture cache.
emptyAtlasTextureCache :: Int -> AtlasTextureCache
emptyAtlasTextureCache maxEntries = AtlasTextureCache
  { atcKey = Nothing
  , atcCaches = Map.empty
  , atcMaxEntries = maxEntries
  , atcLru = []
  , atcPending = []
  , atcLast = Nothing
  , atcCommittedStage = Nothing
  , atcStageChangeNs = 0
  }

-- | Hysteresis threshold: do not switch zoom stage until the camera has
-- been in the new range for at least this many nanoseconds (300 ms).
stageHysteresisNs :: Word64
stageHysteresisNs = 300000000

-- | Resolve the effective zoom stage with hysteresis.
--
-- If the raw stage (from 'stageForZoom') differs from the previously
-- committed stage, the switch is delayed until the camera has remained
-- in the new stage's range for 'stageHysteresisNs'.  Returns the
-- effective stage, an optional transition target @(targetStage, blendFactor)@
-- during the cross-fade window, and the updated cache.
--
-- The blend factor uses smoothstep interpolation: 0 at the start of
-- the transition, 1 when the hysteresis expires and the new stage commits.
resolveEffectiveStage :: Word64 -> ZoomStage -> AtlasTextureCache -> (ZoomStage, Maybe (ZoomStage, Float), AtlasTextureCache)
resolveEffectiveStage nowNs rawStage cache =
  case atcCommittedStage cache of
    Nothing ->
      (rawStage, Nothing, cache { atcCommittedStage = Just rawStage, atcStageChangeNs = 0 })
    Just committed
      | committed == rawStage ->
          (committed, Nothing, cache { atcStageChangeNs = 0 })
      | atcStageChangeNs cache == 0 ->
          (committed, Just (rawStage, 0), cache { atcStageChangeNs = nowNs })
      | nowNs - atcStageChangeNs cache >= stageHysteresisNs ->
          (rawStage, Nothing, cache { atcCommittedStage = Just rawStage, atcStageChangeNs = 0 })
      | otherwise ->
          let elapsed = fromIntegral (nowNs - atcStageChangeNs cache) :: Float
              duration = fromIntegral stageHysteresisNs :: Float
              t = min 1.0 (max 0.0 (elapsed / duration))
              blend = t * t * (3 - 2 * t)  -- smoothstep
          in (committed, Just (rawStage, blend), cache)

-- | Collect all textures currently held by the atlas cache.
collectAtlasTextures :: AtlasTextureCache -> [SDL.Texture]
collectAtlasTextures cache =
  atcPending cache ++ concatMap collectTextures (Map.elems (atcCaches cache))

-- | Draw atlas tiles to the renderer.
drawAtlas :: SDL.Renderer -> [TerrainAtlasTile] -> (Float, Float) -> Float -> V2 Int -> IO ()
drawAtlas renderer tiles pan zoom winSize = drawAtlasAlpha renderer tiles pan zoom winSize 255

-- | Draw atlas tiles with a global alpha multiplier for cross-fade blending.
drawAtlasAlpha :: SDL.Renderer -> [TerrainAtlasTile] -> (Float, Float) -> Float -> V2 Int -> Word8 -> IO ()
drawAtlasAlpha renderer tiles (panX, panY) zoom (V2 winW winH) alpha = do
  when (alpha < 255) $
    mapM_ (\tile -> SDL.textureAlphaMod (tatTexture tile) SDL.$= alpha) tiles
  mapM_ drawTile tiles
  when (alpha < 255) $
    mapM_ (\tile -> SDL.textureAlphaMod (tatTexture tile) SDL.$= 255) tiles
  where
    drawTile tile = do
      let Rect (V2 x y, V2 w h) = tatBounds tile
          Rect (V2 tx ty, V2 tw th) = transformRect panX panY zoom (Rect (V2 x y, V2 w h))
          outside = tx > winW || ty > winH || tx + tw < 0 || ty + th < 0
      if outside
        then pure ()
        else SDL.copy renderer (tatTexture tile) Nothing (Just (rectToSDL (Rect (V2 tx ty, V2 tw th))))

    -- Compute screen rect from tile bounds + pan + zoom.
    -- Uses floor/ceiling to guarantee adjacent tiles touch without gaps.
    transformRect px py z (Rect (V2 rx ry, V2 rw rh)) =
      let fx  = (fromIntegral rx + px) * z
          fy  = (fromIntegral ry + py) * z
          fx2 = (fromIntegral (rx + rw) + px) * z
          fy2 = (fromIntegral (ry + rh) + py) * z
          ix  = floor fx  :: Int
          iy  = floor fy  :: Int
          ix2 = ceiling fx2 :: Int
          iy2 = ceiling fy2 :: Int
      in Rect (V2 ix iy, V2 (max 1 (ix2 - ix)) (max 1 (iy2 - iy)))

-- | Drain atlas build results and upload textures.
--
-- Reads from a shared 'AtlasResultRef' (lock-free) instead of a
-- synchronous actor call.  Textures are acquired from the given
-- 'TexturePool'.
--
-- Stale results (key mismatch) are discarded without creating GPU
-- textures so that view mode switches are not blocked by a long queue
-- of superseded results.
drainAtlasBuildResults
  :: Bool
  -> Int
  -> TexturePool
  -> SDL.Renderer
  -> AtlasTextureCache
  -> AtlasResultRef
  -> IO (AtlasTextureCache, Int, Word32)
drainAtlasBuildResults renderTargetOk perFrame pool renderer atlasCache resultRef =
  if renderTargetOk
    then do
      -- With multi-key caching, results for any view mode are valuable
      -- as long as the terrain version matches.  Only discard results
      -- from an outdated generation pass.
      let isFresh r = case atcKey atlasCache of
            Just (AtlasKey _ _ _ currentV) ->
              let AtlasKey _ _ _ resultV = abrKey r in resultV == currentV
            Nothing -> True
      (results, _staleCount) <- drainFreshResultsN resultRef isFresh perFrame
      (cache', totalMs) <- foldM cacheStep (atlasCache, 0) results
      pure (cache', length results, totalMs)
    else do
      results <- drainAtlasResultsN resultRef perFrame
      pure (atlasCache, length results, 0)
  where
    cacheStep (cache, totalMs) result = do
      start <- getMonotonicTimeNSec
      tiles <- renderAtlasTileTextures pool renderer [abrTile result]
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
--
-- Evicted textures are returned to the given 'TexturePool' for reuse.
resolveAtlasTiles
  :: Bool
  -> TexturePool
  -> RenderSnapshot
  -> AtlasTextureCache
  -> ZoomStage
  -> IO (Maybe [TerrainAtlasTile], AtlasTextureCache)
resolveAtlasTiles renderTargetOk pool snapshot atlasCache stage = do
  let terrainSnap = rsTerrain snapshot
      atlasKey = AtlasKey (uiViewMode (rsUi snapshot)) (uiRenderWaterLevel (rsUi snapshot)) (uiDayNightEnabled (rsUi snapshot)) (tsVersion terrainSnap)
      dataReady = tsChunkSize terrainSnap > 0 && not (IntMap.null (tsTerrainChunks terrainSnap))
      atlasTiles = if renderTargetOk && dataReady
        then getNearestAtlas atlasKey (zsHexRadius stage) atlasCache
        else Nothing
      atlasToDraw = case atlasTiles of
        Just tiles | not (null tiles) -> Just tiles
        _ -> case atcLast atlasCache of
          Just (_key, tiles) | not (null tiles) -> Just tiles
          _ -> Nothing
      -- With multi-key caching, old atcLast tiles are NOT explicitly
      -- retired here because they may still live in atcCaches under
      -- their original key.  Eviction handles texture lifecycle;
      -- the keepAlive filter below protects atcLast textures that
      -- were evicted but not yet replaced.
      cacheWithLast = case atlasTiles of
        Just tiles | not (null tiles) ->
          atlasCache { atcLast = Just (atlasKey, tiles) }
        _ -> atlasCache
      cacheTouched = case atlasToDraw of
        Just (t:_) -> touchAtlasScale (tatHexRadius t) cacheWithLast
        _ -> cacheWithLast
      (pending, cacheDrained) = drainAtlasPending cacheTouched
      -- Protect textures that atcLast still references from being
      -- released.  Only truly orphaned textures may be destroyed.
      aliveTextures = case atcLast cacheDrained of
        Just (_key, tiles) -> map tatTexture tiles
        _                  -> []
      (keepAlive, destroyNow) =
        if null aliveTextures
          then ([], pending)
          else (filter (`elem` aliveTextures) pending,
                filter (`notElem` aliveTextures) pending)
      cacheFinal = cacheDrained { atcPending = keepAlive }
  unless (null destroyNow) $
    mapM_ (releaseTexture pool) destroyNow
  pure (atlasToDraw, cacheFinal)

zoomTextureScale :: Float -> Int
zoomTextureScale zoom =
  let target = ceiling (zoom * 2)
  in max 1 (min 6 target)

-- | Select the active atlas key.
--
-- With multi-key caching this is an O(1) pointer update — no textures
-- are flushed.  Tiles for the previous key remain in the cache and can
-- be looked up again if the user switches back.
setAtlasKey :: AtlasKey -> AtlasTextureCache -> AtlasTextureCache
setAtlasKey key cache = cache { atcKey = Just key }

-- | Store freshly-built atlas tiles.
--
-- Tiles are indexed by ('AtlasKey', scale) in the nested map.  Results
-- for any key are accepted as long as the terrain version matches the
-- active key.  Results from an outdated terrain generation are
-- discarded to 'atcPending'.
storeAtlasTiles :: AtlasKey -> Int -> [TerrainAtlasTile] -> AtlasTextureCache -> AtlasTextureCache
storeAtlasTiles key scale tiles cache =
  let isStale = case atcKey cache of
        Just (AtlasKey _ _ _ currentV) ->
          let AtlasKey _ _ _ resultV = key in resultV /= currentV
        Nothing -> False
  in if isStale
    then cache { atcPending = map tatTexture tiles ++ atcPending cache }
    else
      let targetCache = case atcKey cache of
            Nothing -> cache { atcKey = Just key }
            _       -> cache
          bucket = maybe IntMap.empty id (Map.lookup key (atcCaches targetCache))
          (merged, pending) = mergeTiles (IntMap.lookup scale bucket) tiles
          bucket' = IntMap.insert scale merged bucket
          cache' = targetCache
            { atcCaches = Map.insert key bucket' (atcCaches targetCache)
            , atcLru = touch (key, scale) (atcLru targetCache)
            , atcPending = pending ++ atcPending targetCache
            }
      in evictIfNeeded cache'

-- | Look up the nearest-scale tiles for a given key.
--
-- Any key can be looked up, not just the active one.
getNearestAtlas :: AtlasKey -> Int -> AtlasTextureCache -> Maybe [TerrainAtlasTile]
getNearestAtlas key target cache =
  case Map.lookup key (atcCaches cache) of
    Just bucket -> nearestAtlas target bucket
    Nothing     -> Nothing

-- | Touch an atlas entry in the LRU, moving it to the front.
--
-- The active key is used.  Call sites that know the exact key should
-- use the key directly by updating 'atcLru'.
touchAtlasScale :: Int -> AtlasTextureCache -> AtlasTextureCache
touchAtlasScale scale cache =
  case atcKey cache of
    Just key -> cache { atcLru = touch (key, scale) (atcLru cache) }
    Nothing  -> cache

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

touch :: Eq a => a -> [a] -> [a]
touch entry lru = entry : filter (/= entry) lru

evictIfNeeded :: AtlasTextureCache -> AtlasTextureCache
evictIfNeeded cache
  | length (atcLru cache) <= atcMaxEntries cache = cache
  | otherwise =
      let (toKeep, toEvict) = splitAt (atcMaxEntries cache) (atcLru cache)
          (caches', pending) = foldl evictOne (atcCaches cache, []) toEvict
      in cache
        { atcCaches = caches'
        , atcLru = toKeep
        , atcPending = pending ++ atcPending cache
        }
  where
    evictOne (caches, pending) (key, scale) =
      case Map.lookup key caches of
        Nothing -> (caches, pending)
        Just bucket ->
          let tiles = maybe [] id (IntMap.lookup scale bucket)
              bucket' = IntMap.delete scale bucket
              caches' = if IntMap.null bucket'
                then Map.delete key caches
                else Map.insert key bucket' caches
          in (caches', map tatTexture tiles ++ pending)

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
