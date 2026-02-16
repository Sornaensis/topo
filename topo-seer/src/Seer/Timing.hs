-- | Shared timing utilities for nanosecond-to-millisecond conversion
-- and timed IO actions.
--
-- Consolidates identical definitions that previously lived in
-- @Seer.System@, @Seer.Render.Frame@, @Seer.Render.Atlas@,
-- @Actor.UiActions.Terrain@, and @Actor.AtlasScheduler@.
module Seer.Timing
  ( nsToMs
  , timedMs
  ) where

import Data.Word (Word32, Word64)
import GHC.Clock (getMonotonicTimeNSec)

-- | Convert a start\/end pair of monotonic nanosecond timestamps to
-- elapsed milliseconds (truncated).
nsToMs :: Word64 -> Word64 -> Word32
nsToMs start end =
  fromIntegral ((end - start) `div` 1000000)

-- | Run an IO action and return its result paired with the elapsed wall
-- time in milliseconds.
timedMs :: IO a -> IO (a, Word32)
timedMs action = do
  start <- getMonotonicTimeNSec
  result <- action
  end <- getMonotonicTimeNSec
  pure (result, nsToMs start end)
