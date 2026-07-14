module Seer.System.Snapshot
  ( SnapshotPollEnv(..)
  , pollRenderSnapshot
  ) where

import Actor.Log
  ( Log
  , LogEntry(..)
  , LogLevel(..)
  , LogSnapshotRef
  , appendLog
  )
import Actor.Render (RenderSnapshot(..))
import Actor.SnapshotReceiver
  ( DataSnapshotRef
  , SnapshotVersion
  , SnapshotVersionRef
  , TerrainSnapshotRef
  , readCommittedRenderSnapshot
  )
import Actor.UI (UiSnapshotRef)
import qualified Data.Text as Text
import Data.Word (Word32)
import GHC.Clock (getMonotonicTimeNSec)
import Hyperspace.Actor (ActorHandle, Protocol)
import Seer.System.Cache (RenderCacheState(..), shouldPoll)
import Seer.Timing (nsToMs)

data SnapshotPollEnv = SnapshotPollEnv
  { speLogHandle :: !(ActorHandle Log (Protocol Log))
  , speTimingLogThresholdMs :: !Word32
  , speSnapshotPollMs :: !Int
  , speSnapshotVersionRef :: !SnapshotVersionRef
  , speDataSnapshotRef :: !DataSnapshotRef
  , speTerrainSnapshotRef :: !TerrainSnapshotRef
  , speLogSnapshotRef :: !LogSnapshotRef
  , speUiSnapshotRef :: !UiSnapshotRef
  }

pollRenderSnapshot
  :: SnapshotPollEnv
  -> Word32
  -> Bool
  -> Bool
  -> RenderCacheState
  -> IO (SnapshotVersion, RenderSnapshot, RenderCacheState, Word32)
pollRenderSnapshot env nowMs hasEvents forcePoll cacheState = do
  let shouldPollSnapshot = forcePoll
        || hasEvents
        || shouldPoll nowMs (speSnapshotPollMs env) (rcsLastSnapshotPoll cacheState)
        || rcsLastSnapshotData cacheState == Nothing
  case (shouldPollSnapshot, rcsLastSnapshotData cacheState, rcsLastPolledSnapshot cacheState) of
    (False, Just cachedSnap, Just cachedVersion) ->
      pure (cachedVersion, cachedSnap, cacheState, 0)
    _ -> do
      snapshotStart <- getMonotonicTimeNSec
      (version, snap) <- readCommittedRenderSnapshot (speSnapshotVersionRef env)
      snapshotEnd <- getMonotonicTimeNSec
      let snapshotElapsed = nsToMs snapshotStart snapshotEnd
      if snapshotElapsed >= speTimingLogThresholdMs env
        then appendLog (speLogHandle env) (LogEntry LogInfo (Text.pack ("snapshot poll took " <> show snapshotElapsed <> "ms")))
        else pure ()
      pure (version, snap, cacheState
        { rcsLastPolledSnapshot = Just version
        , rcsLastSnapshotData = Just snap
        , rcsLastSnapshotPoll = Just nowMs
        }, snapshotElapsed)
