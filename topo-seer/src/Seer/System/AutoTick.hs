{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Runtime-owned automatic simulation tick scheduler.
--
-- The scheduler is deliberately outside the UI, command, and simulation
-- actors: it observes the published UI snapshot, submits one auto step to the
-- Simulation actor's background worker, and arms the next due time only after
-- the worker reply is folded.  That gives automatic ticking one-in-flight,
-- no-backlog semantics while Simulation/UI control calls remain responsive.
module Seer.System.AutoTick
  ( AutoTickHandles(..)
  , AutoTickScheduler
  , startAutoTickScheduler
  , stopAutoTickScheduler
  , normaliseAutoTickRate
  , effectiveAutoTickHz
  , autoTickPeriodMicros
  ) where

import Actor.Log (Log, LogEntry(..), LogLevel(..), appendLog)
import Actor.Simulation
  ( AutoTickStepResult(..)
  , Simulation
  , SimulationDagSnapshot(..)
  , autoTickStepArmed
  , flushSimWeatherPublication
  , getSimDagSnapshot
  )
import Actor.SnapshotReceiver
  ( SnapshotVersionRef
  , bumpSnapshotVersion
  )
import Actor.UI
  ( Ui
  , UiSnapshotRef
  , UiState(..)
  , getUiSnapshot
  , readUiSnapshotRef
  , setUiSimAutoTick
  )
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
  ( MVar
  , isEmptyMVar
  , newEmptyMVar
  , putMVar
  , readMVar
  , tryPutMVar
  )
import Control.Exception (finally)
import Control.Monad (unless)
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Hyperspace.Actor (ActorHandle, Protocol)
import System.Timeout (timeout)

-- | Handles needed by the runtime scheduler.
data AutoTickHandles = AutoTickHandles
  { athUiHandle :: !(ActorHandle Ui (Protocol Ui))
  , athUiSnapshotRef :: !UiSnapshotRef
  , athSimulationHandle :: !(ActorHandle Simulation (Protocol Simulation))
  , athLogHandle :: !(ActorHandle Log (Protocol Log))
  , athSnapshotVersionRef :: !SnapshotVersionRef
  }

-- | Opaque running scheduler token.
data AutoTickScheduler = AutoTickScheduler
  { atsStop :: !(MVar ())
  , atsStopped :: !(MVar ())
  }

-- | Active scheduler state.  A change to any signature component rearms the
-- first due tick at @now + period@, covering auto-enable, rate changes,
-- manual tick/reset, and world binding epoch changes.
data SchedulerState
  = SchedulerIdle
  | SchedulerArmed !AutoTickSignature !Word64
  deriving (Eq, Show)

-- | The observed state that determines whether an armed due time is still
-- valid.
data AutoTickSignature = AutoTickSignature
  { atsRate :: !Float
  , atsTickCount :: !Word64
  , atsWorldEpoch :: !Word64
  } deriving (Eq, Show)

-- | Clamp the UI-normalised tick-rate value to @[0,1]@.
normaliseAutoTickRate :: Float -> Float
normaliseAutoTickRate = max 0 . min 1

-- | Convert the UI-normalised tick-rate value to effective ticks per second.
-- A value of @0@ pauses automatic ticking; @1@ is @10Hz@.
effectiveAutoTickHz :: Float -> Double
effectiveAutoTickHz rate = realToFrac (normaliseAutoTickRate rate) * 10

-- | Period in microseconds for a UI-normalised auto-tick rate.
autoTickPeriodMicros :: Float -> Maybe Int
autoTickPeriodMicros rate
  | hz <= 0 = Nothing
  | otherwise = Just (max 1 (round (1000000 / hz)))
  where
    hz = effectiveAutoTickHz rate

-- | Start the runtime-owned scheduler thread.
startAutoTickScheduler :: AutoTickHandles -> IO AutoTickScheduler
startAutoTickScheduler handles = do
  stopVar <- newEmptyMVar
  stoppedVar <- newEmptyMVar
  _ <- forkIO $ schedulerLoop handles stopVar SchedulerIdle
    `finally` putMVar stoppedVar ()
  pure AutoTickScheduler
    { atsStop = stopVar
    , atsStopped = stoppedVar
    }

-- | Stop the scheduler and wait for the thread to exit.  If an auto tick is
-- in-flight, shutdown waits for that single attempt to complete rather than
-- killing the thread mid-call.
stopAutoTickScheduler :: AutoTickScheduler -> IO ()
stopAutoTickScheduler AutoTickScheduler{atsStop, atsStopped} = do
  _ <- tryPutMVar atsStop ()
  readMVar atsStopped

schedulerLoop :: AutoTickHandles -> MVar () -> SchedulerState -> IO ()
schedulerLoop handles stopVar state = do
  stopped <- stopRequested stopVar
  unless stopped $ do
    now <- getMonotonicTimeNSec
    ui <- readUiSnapshotRef (athUiSnapshotRef handles)
    case activePeriod ui of
      Nothing -> waitAndContinue stopVar schedulerPollMicros SchedulerIdle
      Just periodUs -> do
        signature <- currentSignature handles ui
        case state of
          SchedulerArmed previous dueNs
            | previous == signature ->
                if now >= dueNs
                  then fireAutoTick handles stopVar signature periodUs
                  else waitAndContinue stopVar (waitMicrosUntil now dueNs) state
          _ -> do
            let dueNs = now + microsToNanos periodUs
            waitAndContinue stopVar (waitMicrosUntil now dueNs) (SchedulerArmed signature dueNs)
  where
    waitAndContinue stopVar delayMicros nextState = do
      stopped <- waitForStop stopVar delayMicros
      unless stopped (schedulerLoop handles stopVar nextState)

activePeriod :: UiState -> Maybe Int
activePeriod ui
  | not (uiSimAutoTick ui) = Nothing
  | uiGenerating ui = Nothing
  | otherwise = autoTickPeriodMicros (uiSimTickRate ui)

currentSignature :: AutoTickHandles -> UiState -> IO AutoTickSignature
currentSignature handles ui = do
  dag <- getSimDagSnapshot (athSimulationHandle handles)
  pure AutoTickSignature
    { atsRate = normaliseAutoTickRate (uiSimTickRate ui)
    , atsTickCount = uiSimTickCount ui
    , atsWorldEpoch = sdsWorldEpoch dag
    }

fireAutoTick :: AutoTickHandles -> MVar () -> AutoTickSignature -> Int -> IO ()
fireAutoTick handles stopVar expectedSig _periodUs = do
  ui <- readUiSnapshotRef (athUiSnapshotRef handles)
  case activePeriod ui of
    Nothing -> schedulerLoop handles stopVar SchedulerIdle
    Just _freshPeriodUs -> do
      freshSig <- currentSignature handles ui
      if freshSig /= expectedSig
        then schedulerLoop handles stopVar SchedulerIdle
        else do
          result <- autoTickStepArmed (athSimulationHandle handles) (Just (atsWorldEpoch freshSig)) True
          case result of
            AutoTickFailed err -> do
              disableAutoAfterFailure handles err
              schedulerLoop handles stopVar SchedulerIdle
            AutoTickApplied{} -> rearmFromCompletion handles stopVar
            AutoTickSkipped{} -> rearmFromCompletion handles stopVar

rearmFromCompletion :: AutoTickHandles -> MVar () -> IO ()
rearmFromCompletion handles stopVar = do
  completion <- getMonotonicTimeNSec
  ui <- readUiSnapshotRef (athUiSnapshotRef handles)
  case activePeriod ui of
    Nothing -> schedulerLoop handles stopVar SchedulerIdle
    Just periodUs -> do
      signature <- currentSignature handles ui
      let dueNs = completion + microsToNanos periodUs
      stopped <- waitForStop stopVar (waitMicrosUntil completion dueNs)
      unless stopped (schedulerLoop handles stopVar (SchedulerArmed signature dueNs))

disableAutoAfterFailure :: AutoTickHandles -> Text -> IO ()
disableAutoAfterFailure handles err = do
  appendLog (athLogHandle handles) $ LogEntry LogWarn $
    "simulation: auto tick disabled after failure: " <> err
  setUiSimAutoTick (athUiHandle handles) False
  -- Barrier: make sure the UI actor has published the disabled state before
  -- bumping the shared snapshot version for render/HTTP readers.
  _ <- getUiSnapshot (athUiHandle handles)
  flushed <- flushSimWeatherPublication (athSimulationHandle handles)
  if flushed
    then pure ()
    else bumpSnapshotVersion (athSnapshotVersionRef handles)

waitMicrosUntil :: Word64 -> Word64 -> Int
waitMicrosUntil now due
  | due <= now = 0
  | otherwise = min schedulerPollMicros (nanosToMicros (due - now))

schedulerPollMicros :: Int
schedulerPollMicros = 50000

microsToNanos :: Int -> Word64
microsToNanos micros = fromIntegral micros * 1000

nanosToMicros :: Word64 -> Int
nanosToMicros nanos = max 1 (fromIntegral ((nanos + 999) `div` 1000))

stopRequested :: MVar () -> IO Bool
stopRequested stopVar = not <$> isEmptyMVar stopVar

waitForStop :: MVar () -> Int -> IO Bool
waitForStop stopVar delayMicros
  | delayMicros <= 0 = stopRequested stopVar
  | otherwise = do
      result <- timeout delayMicros (readMVar stopVar)
      pure (maybe False (const True) result)
