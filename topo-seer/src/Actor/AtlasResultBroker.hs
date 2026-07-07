-- | Lock-free IORef channel for atlas build results.
--
-- Results are published to a shared 'AtlasResultRef' so the render thread
-- can drain via lock-free 'atomicModifyIORef'' without actor indirection.
module Actor.AtlasResultBroker
  ( AtlasResultRef
  , AtlasResultDrainStats(..)
  , newAtlasResultRef
  , pushAtlasResult
  , atlasResultsPending
  , atlasResultsPendingCount
  , drainAtlasResultsN
  , drainAtlasResultsNWithStats
  , drainFreshResultsN
  , drainFreshResultsNWithStats
  , formatAtlasResultDrainStats
  ) where

import Actor.AtlasResult (AtlasBuildResult)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.List (partition)

-- | Shared reference for lock-free render-thread drain.
--
-- The list is stored in *push order* (newest first).
-- Readers atomically swap it out and reverse to get FIFO order.
type AtlasResultRef = IORef [AtlasBuildResult]

-- | Create a new empty 'AtlasResultRef'.
newAtlasResultRef :: IO AtlasResultRef
newAtlasResultRef = newIORef []

-- | Atomically push an atlas build result for the render loop.
pushAtlasResult :: AtlasResultRef -> AtlasBuildResult -> IO ()
pushAtlasResult ref result =
  atomicModifyIORef' ref (\xs -> (result : xs, ()))

-- | Lightweight diagnostics from a single lock-free broker drain.
--
-- Counts are measured inside the same atomic operation that drains the queue,
-- so trace output can distinguish stale drops from upload-budget backlog
-- without a second destructive read.
data AtlasResultDrainStats = AtlasResultDrainStats
  { ardsPendingBefore :: !Int
  , ardsPendingAfter :: !Int
  , ardsFreshDrained :: !Int
  , ardsStaleDropped :: !Int
  , ardsFreshPreserved :: !Int
  , ardsBudgetExhausted :: !Bool
  } deriving (Eq, Show)

-- | Check whether any atlas build results are pending without draining them.
--
-- Uses a no-op atomic modification so concurrent worker pushes are observed
-- with the same synchronisation semantics as the render-thread drain path.
atlasResultsPending :: AtlasResultRef -> IO Bool
atlasResultsPending ref = (> 0) <$> atlasResultsPendingCount ref

-- | Count pending atlas build results without draining them.
atlasResultsPendingCount :: AtlasResultRef -> IO Int
atlasResultsPendingCount ref =
  atomicModifyIORef' ref (\xs -> (xs, length xs))

-- | Drain up to N pending atlas results in FIFO order (lock-free).
drainAtlasResultsN :: AtlasResultRef -> Int -> IO [AtlasBuildResult]
drainAtlasResultsN ref count = fst <$> drainAtlasResultsNWithStats ref count

-- | Drain up to N pending atlas results with non-destructive queue counters.
drainAtlasResultsNWithStats :: AtlasResultRef -> Int -> IO ([AtlasBuildResult], AtlasResultDrainStats)
drainAtlasResultsNWithStats ref count = do
  let safeCount = max 0 count
  atomicModifyIORef' ref $ \xs ->
    let fifo = reverse xs
        (taken, rest) = splitAt safeCount fifo
        stats = AtlasResultDrainStats
          { ardsPendingBefore = length fifo
          , ardsPendingAfter = length rest
          , ardsFreshDrained = length taken
          , ardsStaleDropped = 0
          , ardsFreshPreserved = length rest
          , ardsBudgetExhausted = not (null rest)
          }
    in (reverse rest, (taken, stats))

-- | Drain pending results, discarding stale entries and returning up to
-- @n@ fresh results in FIFO order (lock-free).
--
-- The predicate selects \"fresh\" results; those for which it returns
-- @False@ are silently dropped from the queue.  At most @n@ fresh
-- results are returned; any remaining fresh results stay in the queue
-- for the next drain.  Returns the fresh results and the count of
-- dropped stale entries.
drainFreshResultsN :: AtlasResultRef -> (AtlasBuildResult -> Bool) -> Int -> IO ([AtlasBuildResult], Int)
drainFreshResultsN ref isFresh count = do
  (results, stats) <- drainFreshResultsNWithStats ref isFresh count
  pure (results, ardsStaleDropped stats)

-- | Drain pending results with stale-drop and upload-budget diagnostics.
drainFreshResultsNWithStats :: AtlasResultRef -> (AtlasBuildResult -> Bool) -> Int -> IO ([AtlasBuildResult], AtlasResultDrainStats)
drainFreshResultsNWithStats ref isFresh count = do
  let safeCount = max 0 count
  atomicModifyIORef' ref $ \xs ->
    let fifo = reverse xs
        (stale, fresh) = partition (not . isFresh) fifo
        (taken, rest) = splitAt safeCount fresh
        stats = AtlasResultDrainStats
          { ardsPendingBefore = length fifo
          , ardsPendingAfter = length rest
          , ardsFreshDrained = length taken
          , ardsStaleDropped = length stale
          , ardsFreshPreserved = length rest
          , ardsBudgetExhausted = not (null rest)
          }
    in (reverse rest, (taken, stats))

formatAtlasResultDrainStats :: AtlasResultDrainStats -> String
formatAtlasResultDrainStats stats =
  "brokerBefore=" <> show (ardsPendingBefore stats)
    <> " brokerAfter=" <> show (ardsPendingAfter stats)
    <> " drained=" <> show (ardsFreshDrained stats)
    <> " staleDropped=" <> show (ardsStaleDropped stats)
    <> " preserved=" <> show (ardsFreshPreserved stats)
    <> " uploadBudgetExhausted=" <> show (ardsBudgetExhausted stats)


