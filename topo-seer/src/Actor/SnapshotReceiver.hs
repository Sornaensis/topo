{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Receives snapshot replies and publishes them via a shared 'IORef' for
-- lock-free reads by the render loop.
--
-- Other actors send snapshot updates via fire-and-forget casts.  After each
-- update the receiver bumps a monotonic version counter and, when a
-- 'SnapshotRef' has been registered with 'setSnapshotRef', writes the
-- latest @(SnapshotVersion, RenderSnapshot)@ pair to the shared ref so the
-- render thread can poll without a synchronous actor call.
module Actor.SnapshotReceiver
  ( SnapshotReceiver
  , RenderSnapshot(..)
  , SnapshotVersion(..)
  , SnapshotRef
  , snapshotReceiverActorDef
  , getSnapshot
  , setSnapshotRef
  ) where

import Actor.Data (DataSnapshot(..), DataSnapshotReply, TerrainSnapshot(..))
import Actor.Log (LogLevel(..), LogSnapshot(..), LogSnapshotReply)
import Actor.UI (UiSnapshotReply, UiState(..), emptyUiState)
import Control.Exception (evaluate)
import Data.IORef (IORef, writeIORef)
import Data.Word (Word64)
import Hyperspace.Actor
import Hyperspace.Actor.QQ (hyperspace)

-- | Composite render snapshot published to the render loop.
--
-- Contains one sub-snapshot per domain: UI state, log, data, and terrain.
data RenderSnapshot = RenderSnapshot
  { rsUi      :: !UiState
  , rsLog     :: !LogSnapshot
  , rsData    :: !DataSnapshot
  , rsTerrain :: !TerrainSnapshot
  } deriving (Eq, Show)

-- | Shared reference for lock-free snapshot reads by the render loop.
type SnapshotRef = IORef (SnapshotVersion, RenderSnapshot)

data SnapshotReceiverState = SnapshotReceiverState
  { srsUi :: !UiState
  , srsLog :: !LogSnapshot
  , srsData :: !DataSnapshot
  , srsTerrain :: !TerrainSnapshot
  , srsVersion :: !Word64
  , srsRef :: !(Maybe SnapshotRef)
  }

emptySnapshotReceiverState :: SnapshotReceiverState
emptySnapshotReceiverState = SnapshotReceiverState
  { srsUi = emptyUiState
  , srsLog = LogSnapshot [] False 0 LogDebug
  , srsData = DataSnapshot 0 0 Nothing
  , srsTerrain = TerrainSnapshot 0 0 mempty mempty mempty mempty
  , srsVersion = 0
  , srsRef = Nothing
  }

-- | Monotonic version for the latest cached snapshot.
newtype SnapshotVersion = SnapshotVersion { unSnapshotVersion :: Word64 }
  deriving (Eq, Ord, Show)

-- | Build the version/snapshot pair from current actor state.
buildSnapshotPair :: SnapshotReceiverState -> (SnapshotVersion, RenderSnapshot)
buildSnapshotPair st =
  ( SnapshotVersion (srsVersion st)
  , RenderSnapshot
      { rsUi = srsUi st
      , rsLog = srsLog st
      , rsData = srsData st
      , rsTerrain = srsTerrain st
      }
  )

-- | Publish the current snapshot to the shared 'IORef', if one is registered.
--
-- The snapshot pair is forced to WHNF before writing to the IORef so the
-- render thread never pays for lazy thunk evaluation when reading.
publishSnapshot :: SnapshotReceiverState -> IO ()
publishSnapshot st =
  case srsRef st of
    Nothing -> pure ()
    Just ref -> do
      let pair = buildSnapshotPair st
      _ <- evaluate pair
      writeIORef ref pair

[hyperspace|
actor SnapshotReceiver
  state SnapshotReceiverState
  lifetime Singleton
  schedule pinned 3
  noDeps

  reply UiSnapshotReply, LogSnapshotReply, DataSnapshotReply

  mailbox Unbounded

  cast uiSnapshot :: UiState
  cast logSnapshot :: LogSnapshot
  cast dataSnapshot :: DataSnapshot
  cast terrainSnapshot :: TerrainSnapshot
  cast setRef :: SnapshotRef
  call snapshot :: () -> (SnapshotVersion, RenderSnapshot)

  initial emptySnapshotReceiverState
  on_ uiSnapshot = \uiSnap st -> do
    let st' = bumpVersion st { srsUi = uiSnap }
    publishSnapshot st'
    pure st'
  on_ logSnapshot = \logSnap st -> do
    let st' = bumpVersion st { srsLog = logSnap }
    publishSnapshot st'
    pure st'
  on_ dataSnapshot = \dataSnap st -> do
    let st' = bumpVersion st { srsData = dataSnap }
    publishSnapshot st'
    pure st'
  on_ terrainSnapshot = \terrainSnap st -> do
    let st' = bumpVersion st { srsTerrain = terrainSnap }
    publishSnapshot st'
    pure st'
  on_ setRef = \ref st -> do
    let st' = st { srsRef = Just ref }
    publishSnapshot st'
    pure st'
  onPure snapshot = \() st ->
    ( st
    , buildSnapshotPair st
    )
|]

-- | Poll the latest render snapshot along with its monotonic version.
--
-- This performs a synchronous actor call and may block if the actor is busy.
-- Prefer reading the 'SnapshotRef' directly via 'Data.IORef.readIORef' on
-- the render thread for lock-free access.
--
-- __Test-only:__ No production callers — kept exported for test coverage.
getSnapshot :: ActorHandle SnapshotReceiver (Protocol SnapshotReceiver) -> IO (SnapshotVersion, RenderSnapshot)
getSnapshot handle =
  call @"snapshot" handle #snapshot ()

-- | Register a shared 'IORef' for lock-free snapshot reads.
--
-- Once set, every snapshot update (UI, log, data, terrain) is published to
-- this ref after the actor's internal state is updated.  The render loop can
-- then use 'Data.IORef.readIORef' instead of the blocking 'getSnapshot'.
setSnapshotRef
  :: ActorHandle SnapshotReceiver (Protocol SnapshotReceiver)
  -> SnapshotRef
  -> IO ()
setSnapshotRef handle ref =
  cast @"setRef" handle #setRef ref

bumpVersion :: SnapshotReceiverState -> SnapshotReceiverState
bumpVersion st =
  st { srsVersion = srsVersion st + 1 }
