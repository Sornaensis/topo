-- | Lock-free IORef channel for atlas scheduling reports.
--
-- Reports are published to a shared 'AtlasScheduleRef' so the render thread
-- can poll via lock-free 'readIORef' without actor indirection.
module Actor.AtlasScheduleBroker
  ( AtlasScheduleReport(..)
  , AtlasScheduleRef
  , emptyAtlasScheduleReport
  , formatAtlasScheduleReport
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
  , asrJobsAvailable :: !Int
  , asrJobsDispatched :: !Int
  , asrJobsDeferred :: !Int
  , asrJobsDroppedStale :: !Int
  , asrCurrentStageDispatches :: !Int
  , asrBackfillDispatches :: !Int
  , asrWorkerCapacity :: !Int
  , asrWorkerAvailable :: !Int
  , asrWorkerInFlight :: !Int
  , asrWorkerStarted :: !Int
  , asrWorkerCompleted :: !Int
  , asrWorkerStaleSkippedAtStart :: !Int
  , asrWorkerStaleCancelledDuringGeometry :: !Int
  , asrWorkerStaleCancelledBeforePublish :: !Int
  } deriving (Eq, Show)

emptyAtlasScheduleReport :: SnapshotVersion -> AtlasScheduleReport
emptyAtlasScheduleReport snapshotVersion = AtlasScheduleReport
  { asrSnapshotVersion = snapshotVersion
  , asrJobCount = 0
  , asrDrainMs = 0
  , asrEnqueueMs = 0
  , asrJobsAvailable = 0
  , asrJobsDispatched = 0
  , asrJobsDeferred = 0
  , asrJobsDroppedStale = 0
  , asrCurrentStageDispatches = 0
  , asrBackfillDispatches = 0
  , asrWorkerCapacity = 0
  , asrWorkerAvailable = 0
  , asrWorkerInFlight = 0
  , asrWorkerStarted = 0
  , asrWorkerCompleted = 0
  , asrWorkerStaleSkippedAtStart = 0
  , asrWorkerStaleCancelledDuringGeometry = 0
  , asrWorkerStaleCancelledBeforePublish = 0
  }

formatAtlasScheduleReport :: AtlasScheduleReport -> String
formatAtlasScheduleReport report =
  "jobsAvailable=" <> show (asrJobsAvailable report)
    <> " jobsDispatched=" <> show (asrJobsDispatched report)
    <> " jobsDeferred=" <> show (asrJobsDeferred report)
    <> " jobsStaleDropped=" <> show (asrJobsDroppedStale report)
    <> " currentStageDispatches=" <> show (asrCurrentStageDispatches report)
    <> " backfillDispatches=" <> show (asrBackfillDispatches report)
    <> " workerCapacity=" <> show (asrWorkerCapacity report)
    <> " workerAvailable=" <> show (asrWorkerAvailable report)
    <> " workerInFlight=" <> show (asrWorkerInFlight report)
    <> " workerStarted=" <> show (asrWorkerStarted report)
    <> " workerCompleted=" <> show (asrWorkerCompleted report)
    <> " workerStaleStart=" <> show (asrWorkerStaleSkippedAtStart report)
    <> " workerStaleGeometry=" <> show (asrWorkerStaleCancelledDuringGeometry report)
    <> " workerStalePublish=" <> show (asrWorkerStaleCancelledBeforePublish report)

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
