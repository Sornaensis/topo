module Seer.System.Snapshot
  ( SnapshotPollEnv(..)
  , pollRenderSnapshot
  ) where

import Actor.Render (RenderSnapshot)
import Actor.SnapshotReceiver
  ( SnapshotVersion
  , SnapshotVersionRef
  , readCommittedRenderSnapshot
  , readSnapshotVersion
  )
import Data.Word (Word32)
import GHC.Clock (getMonotonicTimeNSec)
import Seer.System.Cache (RenderCacheState(..), shouldPoll)
import Seer.Timing (nsToMs)

data SnapshotPollEnv = SnapshotPollEnv
  { speTimingLogThresholdMs :: !Word32
  , speSnapshotPollMs :: !Int
  , speSnapshotVersionRef :: !SnapshotVersionRef
  , speLogSlowSnapshotPoll :: !(Word32 -> IO ())
  }

-- | Probe publication on every decision. The interval remains a health check
-- that periodically re-reads the coherent tuple even when its version is
-- unchanged; it is not the freshness mechanism.
pollRenderSnapshot
  :: SnapshotPollEnv
  -> Word32
  -> Bool
  -> RenderCacheState
  -> IO (SnapshotVersion, RenderSnapshot, RenderCacheState, Word32)
pollRenderSnapshot env nowMs forcePoll cacheState = do
  publishedVersion <- readSnapshotVersion (speSnapshotVersionRef env)
  let committedChanged = rcsLastPolledSnapshot cacheState /= Just publishedVersion
      fallbackPollDue = shouldPoll nowMs (speSnapshotPollMs env) (rcsLastSnapshotPoll cacheState)
      shouldReadCoherent = forcePoll
        || committedChanged
        || fallbackPollDue
        || rcsLastSnapshotData cacheState == Nothing
  case (shouldReadCoherent, rcsLastSnapshotData cacheState, rcsLastPolledSnapshot cacheState) of
    (False, Just cachedSnap, Just cachedVersion) ->
      pure (cachedVersion, cachedSnap, cacheState, 0)
    _ -> do
      snapshotStart <- getMonotonicTimeNSec
      -- Trust this version rather than the earlier probe: a publisher may have
      -- committed between the two reads.
      (version, snap) <- readCommittedRenderSnapshot (speSnapshotVersionRef env)
      snapshotEnd <- getMonotonicTimeNSec
      let snapshotElapsed = nsToMs snapshotStart snapshotEnd
      if snapshotElapsed >= speTimingLogThresholdMs env
        then speLogSlowSnapshotPoll env snapshotElapsed
        else pure ()
      pure (version, snap, cacheState
        { rcsLastPolledSnapshot = Just version
        , rcsLastSnapshotData = Just snap
        , rcsLastSnapshotPoll = Just nowMs
        }, snapshotElapsed)
