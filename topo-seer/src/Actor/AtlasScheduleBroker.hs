{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Broker for atlas scheduling reports, polled by the render thread.
--
-- Reports are published to a shared 'AtlasScheduleRef' so the render thread
-- can poll via lock-free 'readIORef' instead of a synchronous actor call.
module Actor.AtlasScheduleBroker
  ( AtlasScheduleBroker
  , AtlasScheduleReport(..)
  , AtlasScheduleRef
  , atlasScheduleBrokerActorDef
  , updateAtlasScheduleReport
  , getAtlasScheduleReport
  , setAtlasScheduleRef
  , readAtlasScheduleRef
  ) where

import Actor.SnapshotReceiver (SnapshotVersion)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Word (Word32)
import Hyperspace.Actor
import Hyperspace.Actor.QQ (hyperspace)

-- | Report emitted after an atlas scheduling pass.
data AtlasScheduleReport = AtlasScheduleReport
  { asrSnapshotVersion :: !SnapshotVersion
  , asrJobCount :: !Int
  , asrDrainMs :: !Word32
  , asrEnqueueMs :: !Word32
  } deriving (Eq, Show)

-- | Shared reference for lock-free render-thread reads.
type AtlasScheduleRef = IORef (Maybe AtlasScheduleReport)

data AtlasScheduleBrokerState = AtlasScheduleBrokerState
  { asbLastReport :: !(Maybe AtlasScheduleReport)
  , asbRef        :: !(Maybe AtlasScheduleRef)
  }

emptyAtlasScheduleBrokerState :: AtlasScheduleBrokerState
emptyAtlasScheduleBrokerState = AtlasScheduleBrokerState
  { asbLastReport = Nothing
  , asbRef        = Nothing
  }

[hyperspace|
actor AtlasScheduleBroker
  state AtlasScheduleBrokerState
  lifetime Singleton
  schedule pinned 4
  noDeps
  mailbox Unbounded

  cast updateReport :: AtlasScheduleReport
  cast setRef :: AtlasScheduleRef
  call getReport :: () -> Maybe AtlasScheduleReport

  initial emptyAtlasScheduleBrokerState
  on_ updateReport = \report st -> do
    let st' = st { asbLastReport = Just report }
    publishReport st' report
    pure st'
  on_ setRef = \ref st -> pure (st { asbRef = Just ref })
  onPure getReport = \() st -> (st, asbLastReport st)
|]

-- | Write the latest report to the shared IORef, if registered.
publishReport :: AtlasScheduleBrokerState -> AtlasScheduleReport -> IO ()
publishReport st report =
  case asbRef st of
    Nothing  -> pure ()
    Just ref -> writeIORef ref (Just report)

-- | Store the latest atlas scheduling report.
updateAtlasScheduleReport
  :: ActorHandle AtlasScheduleBroker (Protocol AtlasScheduleBroker)
  -> AtlasScheduleReport
  -> IO ()
updateAtlasScheduleReport handle report =
  cast @"updateReport" handle #updateReport report

-- | Retrieve the latest atlas scheduling report (synchronous, for tests).
getAtlasScheduleReport
  :: ActorHandle AtlasScheduleBroker (Protocol AtlasScheduleBroker)
  -> IO (Maybe AtlasScheduleReport)
getAtlasScheduleReport handle =
  call @"getReport" handle #getReport ()

-- | Register a shared IORef for lock-free publishing.
setAtlasScheduleRef
  :: ActorHandle AtlasScheduleBroker (Protocol AtlasScheduleBroker)
  -> AtlasScheduleRef
  -> IO ()
setAtlasScheduleRef handle ref =
  cast @"setRef" handle #setRef ref

-- | Read the latest atlas scheduling report from the shared IORef (lock-free).
readAtlasScheduleRef :: AtlasScheduleRef -> IO (Maybe AtlasScheduleReport)
readAtlasScheduleRef = readIORef
