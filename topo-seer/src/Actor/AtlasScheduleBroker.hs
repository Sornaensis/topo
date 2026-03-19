-- | Lock-free IORef channel for atlas scheduling reports.
--
-- Reports are published to a shared 'AtlasScheduleRef' so the render thread
-- can poll via lock-free 'readIORef' without actor indirection.
module Actor.AtlasScheduleBroker
  ( AtlasScheduleReport(..)
  , AtlasScheduleRef
  , newAtlasScheduleRef
  , writeAtlasScheduleReport
  , readAtlasScheduleRef
  ) where

import Actor.SnapshotReceiver (SnapshotVersion)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word32)

-- | Report emitted after an atlas scheduling pass.
data AtlasScheduleReport = AtlasScheduleReport
  { asrSnapshotVersion :: !SnapshotVersion
  , asrJobCount :: !Int
  , asrDrainMs :: !Word32
  , asrEnqueueMs :: !Word32
  } deriving (Eq, Show)

-- | Shared reference for lock-free render-thread reads.
type AtlasScheduleRef = IORef (Maybe AtlasScheduleReport)

-- | Create a new empty 'AtlasScheduleRef'.
newAtlasScheduleRef :: IO AtlasScheduleRef
newAtlasScheduleRef = newIORef Nothing

-- | Write the latest scheduling report to the shared IORef.
writeAtlasScheduleReport :: AtlasScheduleRef -> AtlasScheduleReport -> IO ()
writeAtlasScheduleReport ref report = writeIORef ref (Just report)

-- | Read the latest atlas scheduling report from the shared IORef (lock-free).
readAtlasScheduleRef :: AtlasScheduleRef -> IO (Maybe AtlasScheduleReport)
readAtlasScheduleRef = readIORef
