module Seer.System.Snapshot
  ( SnapshotPollEnv(..)
  , SnapshotPollRequest(..)
  , SnapshotPollReasons(..)
  , SnapshotPollResult(..)
  , noSnapshotPollRequest
  , classifySnapshotPollReasons
  , pollRenderSnapshotDetailed
  , pollRenderSnapshot
  ) where

import Actor.SnapshotReceiver
  ( RenderSnapshot
  , SnapshotVersion
  , SnapshotVersionRef
  , readCommittedRenderSnapshot
  , readSnapshotVersion
  )
import Data.Maybe (isJust)
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

-- | Explicit freshness gates. Keeping screenshot and atlas forcing separate
-- makes a forced reread actionable in diagnostics instead of an opaque Bool.
data SnapshotPollRequest = SnapshotPollRequest
  { spqForcedScreenshot :: !Bool
  , spqFutureAtlasTarget :: !Bool
  } deriving (Eq, Show)

noSnapshotPollRequest :: SnapshotPollRequest
noSnapshotPollRequest = SnapshotPollRequest False False

data SnapshotPollReasons = SnapshotPollReasons
  { sprBootstrap :: !Bool
  , sprPublishedVersionAdvanced :: !Bool
  , sprForcedScreenshot :: !Bool
  , sprFutureAtlasTarget :: !Bool
  , sprIntervalHealthReread :: !Bool
  , sprCacheReuse :: !Bool
  } deriving (Eq, Show)

-- | One publication probe and its resulting coherent tuple. The version read
-- before the coherent read is deliberately retained: it distinguishes a real
-- publication/poll lag from healthy unchanged idle.
data SnapshotPollResult = SnapshotPollResult
  { spresProbedPublishedVersion :: !SnapshotVersion
  , spresCoherentVersion :: !SnapshotVersion
  , spresLastPolledVersion :: !(Maybe SnapshotVersion)
  , spresRenderSnapshot :: !RenderSnapshot
  , spresCacheState :: !RenderCacheState
  , spresElapsedMs :: !Word32
  , spresDidReadCoherent :: !Bool
  , spresReasons :: !SnapshotPollReasons
  }

classifySnapshotPollReasons
  :: SnapshotPollRequest
  -> SnapshotVersion
  -> Maybe SnapshotVersion
  -> Bool
  -> Bool
  -> SnapshotPollReasons
classifySnapshotPollReasons request published lastPolled hasCachedSnapshot intervalDue =
  let bootstrap = not hasCachedSnapshot || lastPolled == Nothing
      advanced = maybe False (/= published) lastPolled
      forced = spqForcedScreenshot request || spqFutureAtlasTarget request
      shouldRead = bootstrap || advanced || forced || intervalDue
  in SnapshotPollReasons
      { sprBootstrap = bootstrap
      , sprPublishedVersionAdvanced = advanced
      , sprForcedScreenshot = spqForcedScreenshot request
      , sprFutureAtlasTarget = spqFutureAtlasTarget request
      , sprIntervalHealthReread = intervalDue
      , sprCacheReuse = not shouldRead
      }

-- | Probe publication on every decision. The interval remains a health check
-- that periodically re-reads the coherent tuple even when its version is
-- unchanged; it is not the freshness mechanism.
pollRenderSnapshotDetailed
  :: SnapshotPollEnv
  -> Word32
  -> SnapshotPollRequest
  -> RenderCacheState
  -> IO SnapshotPollResult
pollRenderSnapshotDetailed env nowMs request cacheState = do
  publishedVersion <- readSnapshotVersion (speSnapshotVersionRef env)
  let lastPolled = rcsLastPolledSnapshot cacheState
      cachedSnapshot = rcsLastSnapshotData cacheState
      fallbackPollDue = shouldPoll nowMs (speSnapshotPollMs env) (rcsLastSnapshotPoll cacheState)
      reasons = classifySnapshotPollReasons
        request
        publishedVersion
        lastPolled
        (isJust cachedSnapshot)
        fallbackPollDue
  case (sprCacheReuse reasons, cachedSnapshot, lastPolled) of
    (True, Just cachedSnap, Just cachedVersion) ->
      pure SnapshotPollResult
        { spresProbedPublishedVersion = publishedVersion
        , spresCoherentVersion = cachedVersion
        , spresLastPolledVersion = lastPolled
        , spresRenderSnapshot = cachedSnap
        , spresCacheState = cacheState
        , spresElapsedMs = 0
        , spresDidReadCoherent = False
        , spresReasons = reasons
        }
    _ -> do
      snapshotStart <- getMonotonicTimeNSec
      -- Trust this version rather than the earlier probe: a publisher may have
      -- committed between the two reads.
      (version, snap) <- readCommittedRenderSnapshot (speSnapshotVersionRef env)
      snapshotEnd <- getMonotonicTimeNSec
      let snapshotElapsed = nsToMs snapshotStart snapshotEnd
          nextCache = cacheState
            { rcsLastPolledSnapshot = Just version
            , rcsLastSnapshotData = Just snap
            , rcsLastSnapshotPoll = Just nowMs
            }
      if snapshotElapsed >= speTimingLogThresholdMs env
        then speLogSlowSnapshotPoll env snapshotElapsed
        else pure ()
      pure SnapshotPollResult
        { spresProbedPublishedVersion = publishedVersion
        , spresCoherentVersion = version
        , spresLastPolledVersion = lastPolled
        , spresRenderSnapshot = snap
        , spresCacheState = nextCache
        , spresElapsedMs = snapshotElapsed
        , spresDidReadCoherent = True
        , spresReasons = reasons
        }

-- | Compatibility wrapper for call sites that only consume the coherent tuple.
pollRenderSnapshot
  :: SnapshotPollEnv
  -> Word32
  -> Bool
  -> RenderCacheState
  -> IO (SnapshotVersion, RenderSnapshot, RenderCacheState, Word32)
pollRenderSnapshot env nowMs forcePoll cacheState = do
  result <- pollRenderSnapshotDetailed env nowMs
    noSnapshotPollRequest { spqForcedScreenshot = forcePoll }
    cacheState
  pure
    ( spresCoherentVersion result
    , spresRenderSnapshot result
    , spresCacheState result
    , spresElapsedMs result
    )
