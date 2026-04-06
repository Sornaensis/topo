-- | Lock-free IORef channel for atlas build results.
--
-- Results are published to a shared 'AtlasResultRef' so the render thread
-- can drain via lock-free 'atomicModifyIORef'' without actor indirection.
module Actor.AtlasResultBroker
  ( AtlasResultRef
  , newAtlasResultRef
  , pushAtlasResult
  , drainAtlasResultsN
  , drainFreshResultsN
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

-- | Drain up to N pending atlas results in FIFO order (lock-free).
drainAtlasResultsN :: AtlasResultRef -> Int -> IO [AtlasBuildResult]
drainAtlasResultsN ref count = do
  let safeCount = max 0 count
  atomicModifyIORef' ref $ \xs ->
    let fifo = reverse xs
        (taken, rest) = splitAt safeCount fifo
    in (reverse rest, taken)

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
  let safeCount = max 0 count
  atomicModifyIORef' ref $ \xs ->
    let fifo = reverse xs
        (stale, fresh) = partition (not . isFresh) fifo
        (taken, rest) = splitAt safeCount fresh
    in (reverse rest, (taken, length stale))


