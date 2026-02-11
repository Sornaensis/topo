{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Asynchronous terrain cache builder actor.
module Actor.TerrainCacheWorker
  ( TerrainCacheWorker
  , TerrainCacheKey(..)
  , TerrainCacheBuildRequest(..)
  , TerrainCacheBuildResult(..)
  , TerrainCacheResultReply
  , terrainCacheWorkerActorDef
  , requestTerrainCacheBuild
  , terrainCacheKeyFrom
  ) where

import Actor.Data (TerrainSnapshot(..))
import Actor.UI (UiState(..), ViewMode(..))
import Control.Exception (evaluate)
import qualified Data.IntMap.Strict as IntMap
import Data.Word (Word64)
import Hyperspace.Actor
import Hyperspace.Actor.QQ (hyperspace)
import Hyperspace.Actor.Spec (OpTag(..))
import Seer.Render (TerrainCache(..), buildTerrainCache, emptyTerrainCache)

-- | Lightweight cache key for O(1) staleness checks.
--
-- Uses 'tsVersion' from the terrain snapshot rather than embedding full
-- 'IntMap's, avoiding deep structural equality on the render thread.
data TerrainCacheKey = TerrainCacheKey
  { tckViewMode :: !ViewMode
  , tckWaterLevel :: !Float
  , tckChunkSize :: !Int
  , tckVersion :: !Word64
  } deriving (Eq, Show)

-- | Terrain cache build request payload.
data TerrainCacheBuildRequest = TerrainCacheBuildRequest
  { tcrKey :: !TerrainCacheKey
  , tcrUi :: !UiState
  , tcrTerrain :: !TerrainSnapshot
  , tcrReplyTo :: !(ReplyTo TerrainCacheResultReply)
  }

-- | Terrain cache build result payload.
data TerrainCacheBuildResult = TerrainCacheBuildResult
  { tcrResultKey :: !TerrainCacheKey
  , tcrResultCache :: !TerrainCache
  }

terrainCacheResultTag :: OpTag "terrainCacheResult"
terrainCacheResultTag = OpTag

-- | Reply protocol for terrain cache build results.
[hyperspace|
replyprotocol TerrainCacheResultReply =
  cast terrainCacheResult :: TerrainCacheBuildResult

actor TerrainCacheWorker
  state ()
  lifetime Singleton
  schedule pinned 4
  noDeps
  mailbox Unbounded

  cast build :: TerrainCacheBuildRequest

  initial ()
  on_ build = \req st -> do
    let cache =
          case terrainCacheKeyFrom (tcrUi req) (tcrTerrain req) of
            Nothing -> emptyTerrainCache
            Just _ -> buildTerrainCache (tcrUi req) (tcrTerrain req)
    -- Force the full IntMap spine of tcGeometry on the worker thread.
    -- IntMap.Strict.mapWithKey is strict at Tip nodes ($!) but has a lazy
    -- spine — without this, the render thread pays ~350ms to evaluate
    -- buildChunkGeometry thunks when it first traverses the IntMap.
    _ <- evaluate (IntMap.size (tcGeometry cache))
    let result = TerrainCacheBuildResult
          { tcrResultKey = tcrKey req
          , tcrResultCache = cache
          }
    replyCast (tcrReplyTo req) terrainCacheResultTag result
    pure st
|]

-- | Submit a terrain cache build request.
requestTerrainCacheBuild
  :: ActorHandle TerrainCacheWorker (Protocol TerrainCacheWorker)
  -> TerrainCacheBuildRequest
  -> IO ()
requestTerrainCacheBuild handle req =
  cast @"build" handle #build req

-- | Build a cache key when terrain data is available.
terrainCacheKeyFrom :: UiState -> TerrainSnapshot -> Maybe TerrainCacheKey
terrainCacheKeyFrom uiSnap terrainSnap
  | tsChunkSize terrainSnap <= 0 = Nothing
  | otherwise = Just TerrainCacheKey
      { tckViewMode = uiViewMode uiSnap
      , tckWaterLevel = uiRenderWaterLevel uiSnap
      , tckChunkSize = tsChunkSize terrainSnap
      , tckVersion = tsVersion terrainSnap
      }
