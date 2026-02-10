{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Actor.AtlasCache
  ( AtlasCache
  , AtlasKey(..)
  , atlasCacheActorDef
  , setAtlasKey
  , storeAtlasTiles
  , getNearestAtlas
  , touchAtlasScale
  , drainAtlasPending
  ) where

import Actor.Data (TerrainSnapshot(..))
import Actor.UI (ViewMode(..))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Word (Word64)
import Hyperspace.Actor
import Hyperspace.Actor.QQ (hyperspace)
import UI.TerrainAtlas (TerrainAtlasTile(..))
import UI.Widgets (Rect)
import qualified SDL

-- | Lightweight atlas cache key for O(1) equality checks.
--
-- Stores the terrain version stamp rather than the full snapshot, avoiding
-- deep structural equality on every frame.
data AtlasKey = AtlasKey !ViewMode !Float !Word64
  deriving (Eq, Show)


data AtlasCacheState = AtlasCacheState
  { acKey :: !(Maybe AtlasKey)
  , acCaches :: !(IntMap [TerrainAtlasTile])
  , acMaxEntries :: !Int
  , acLru :: ![Int]
  , acPending :: ![SDL.Texture]
  }

emptyAtlasCacheState :: AtlasCacheState
emptyAtlasCacheState = AtlasCacheState
  { acKey = Nothing
  , acCaches = IntMap.empty
  , acMaxEntries = 4
  , acLru = []
  , acPending = []
  }

[hyperspace|
actor AtlasCache
  state AtlasCacheState
  lifetime Singleton
  schedule pinned 4
  noDeps
  mailbox Unbounded

  cast storeAtlas :: (AtlasKey, Int, [TerrainAtlasTile])
  cast setKey :: AtlasKey
  cast touchScale :: Int
  call getNearest :: (AtlasKey, Int) -> Maybe [TerrainAtlasTile]
  call drainPending :: () -> [SDL.Texture]

  initial emptyAtlasCacheState
  on_ storeAtlas = \(key, scale, tiles) st -> do
    if acKey st /= Just key
      then do
        let pending = map tatTexture tiles
        pure st { acPending = pending ++ acPending st }
      else do
        (merged, pending) <- mergeTiles (IntMap.lookup scale (acCaches st)) tiles
        let st' = st
              { acCaches = IntMap.insert scale merged (acCaches st)
              , acLru = touch scale (acLru st)
              , acPending = pending ++ acPending st
              }
        evictIfNeeded st'
  on_ setKey = \key st -> do
    let pending = collectTextures (acCaches st)
    pure st
      { acKey = Just key
      , acCaches = IntMap.empty
      , acLru = []
      , acPending = pending ++ acPending st
      }
  onPure getNearest = \(key, target) st ->
    if acKey st == Just key
      then (st, nearestAtlas target (acCaches st))
      else (st, Nothing)
  onPure_ touchScale = \scale st -> st { acLru = touch scale (acLru st) }
  onPure drainPending = \() st -> (st { acPending = [] }, acPending st)
|]

storeAtlasTiles :: ActorHandle AtlasCache (Protocol AtlasCache) -> AtlasKey -> Int -> [TerrainAtlasTile] -> IO ()
storeAtlasTiles handle key scale tiles =
  cast @"storeAtlas" handle #storeAtlas (key, scale, tiles)

setAtlasKey :: ActorHandle AtlasCache (Protocol AtlasCache) -> AtlasKey -> IO ()
setAtlasKey handle key =
  cast @"setKey" handle #setKey key

getNearestAtlas :: ActorHandle AtlasCache (Protocol AtlasCache) -> AtlasKey -> Int -> IO (Maybe [TerrainAtlasTile])
getNearestAtlas handle key scale =
  call @"getNearest" handle #getNearest (key, scale)

touchAtlasScale :: ActorHandle AtlasCache (Protocol AtlasCache) -> Int -> IO ()
touchAtlasScale handle scale =
  cast @"touchScale" handle #touchScale scale

drainAtlasPending :: ActorHandle AtlasCache (Protocol AtlasCache) -> IO [SDL.Texture]
drainAtlasPending handle =
  call @"drainPending" handle #drainPending ()

nearestAtlas :: Int -> IntMap [TerrainAtlasTile] -> Maybe [TerrainAtlasTile]
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

evictIfNeeded :: AtlasCacheState -> IO AtlasCacheState
evictIfNeeded st =
  if length (acLru st) <= acMaxEntries st
    then pure st
    else do
      let (toKeep, toDrop) = splitAt (acMaxEntries st) (acLru st)
          dropSet = IntSet.fromList toDrop
          (evicted, remaining) = IntMap.partitionWithKey (\k _ -> IntSet.member k dropSet) (acCaches st)
          pending = collectTextures evicted
      pure st
        { acCaches = remaining
        , acLru = toKeep
        , acPending = pending ++ acPending st
        }

collectTextures :: IntMap [TerrainAtlasTile] -> [SDL.Texture]
collectTextures caches =
  concatMap (map tatTexture) (IntMap.elems caches)

mergeTiles :: Maybe [TerrainAtlasTile] -> [TerrainAtlasTile] -> IO ([TerrainAtlasTile], [SDL.Texture])
mergeTiles Nothing newTiles = pure (newTiles, [])
mergeTiles (Just existing) newTiles = do
  let newBounds = map tatBounds newTiles
      (replaced, kept) = partitionByBounds newBounds existing
      pending = map tatTexture replaced
  pure (newTiles ++ kept, pending)

partitionByBounds :: [Rect] -> [TerrainAtlasTile] -> ([TerrainAtlasTile], [TerrainAtlasTile])
partitionByBounds bounds tiles =
  foldr (splitOne bounds) ([], []) tiles
  where
    splitOne bounds' tile (replaced, kept)
      | tatBounds tile `elem` bounds' = (tile : replaced, kept)
      | otherwise = (replaced, tile : kept)
