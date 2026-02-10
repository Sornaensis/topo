{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Broker for atlas build results, drained by the render loop.
--
-- Results are published to a shared 'AtlasResultRef' so the render thread
-- can drain via lock-free 'atomicModifyIORef'' instead of a synchronous
-- actor call.
module Actor.AtlasResultBroker
  ( AtlasResultBroker
  , AtlasResultRef
  , atlasResultBrokerActorDef
  , enqueueAtlasResult
  , drainAtlasResults
  , drainAtlasResultsN
  , setAtlasResultRef
  , drainAtlasResultRefN
  ) where

import Actor.AtlasResult (AtlasBuildResult)
import Data.IORef (IORef, atomicModifyIORef', readIORef)
import Hyperspace.Actor
import Hyperspace.Actor.QQ (hyperspace)

-- | Shared reference for lock-free render-thread drain.
--
-- The list is stored in *push order* (newest first).
-- Readers atomically swap it out and reverse to get FIFO order.
type AtlasResultRef = IORef [AtlasBuildResult]

newtype AtlasResultBrokerState = AtlasResultBrokerState
  { arbRef :: Maybe AtlasResultRef
  }

emptyAtlasResultBrokerState :: AtlasResultBrokerState
emptyAtlasResultBrokerState = AtlasResultBrokerState
  { arbRef = Nothing
  }

[hyperspace|
actor AtlasResultBroker
  state AtlasResultBrokerState
  lifetime Singleton
  schedule pinned 4
  noDeps
  mailbox Unbounded

  cast enqueue :: AtlasBuildResult
  cast setRef :: AtlasResultRef

  initial emptyAtlasResultBrokerState
  on_ enqueue = \result st -> do
    case arbRef st of
      Nothing  -> pure ()
      Just ref -> atomicModifyIORef' ref (\xs -> (result : xs, ()))
    pure st
  on_ setRef = \ref st -> pure (st { arbRef = Just ref })
|]

-- | Enqueue an atlas build result for the render loop.
enqueueAtlasResult :: ActorHandle AtlasResultBroker (Protocol AtlasResultBroker) -> AtlasBuildResult -> IO ()
enqueueAtlasResult handle result =
  cast @"enqueue" handle #enqueue result

-- | Drain all pending atlas results via the IORef (lock-free).
drainAtlasResults :: AtlasResultRef -> IO [AtlasBuildResult]
drainAtlasResults ref =
  atomicModifyIORef' ref (\xs -> ([], reverse xs))

-- | Drain up to N pending atlas results in FIFO order via the IORef (lock-free).
drainAtlasResultsN :: AtlasResultRef -> Int -> IO [AtlasBuildResult]
drainAtlasResultsN ref count = do
  let safeCount = max 0 count
  atomicModifyIORef' ref $ \xs ->
    let fifo = reverse xs
        (taken, rest) = splitAt safeCount fifo
    in (reverse rest, taken)

-- | Register a shared IORef for lock-free publishing.
setAtlasResultRef
  :: ActorHandle AtlasResultBroker (Protocol AtlasResultBroker)
  -> AtlasResultRef
  -> IO ()
setAtlasResultRef handle ref =
  cast @"setRef" handle #setRef ref

-- | Read the current queue length without draining (for diagnostics).
drainAtlasResultRefN :: AtlasResultRef -> Int -> IO [AtlasBuildResult]
drainAtlasResultRefN = drainAtlasResultsN
