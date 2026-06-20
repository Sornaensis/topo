{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | In-memory service event buffer and broadcaster used by HTTP polling/SSE.
module Seer.Service.Events
  ( ServiceEventBus
  , defaultServiceEventBufferSize
  , newServiceEventBus
  , newDefaultServiceEventBus
  , publishServiceEvent
  , readBufferedServiceEvents
  , serviceEventSnapshotAndSubscribe
  , serviceEventEnvelopeJson
  , serviceEventSourceText
  , serviceEventSeverityText
  ) where

import Control.Concurrent.STM
  ( TChan
  , TVar
  , atomically
  , dupTChan
  , newBroadcastTChanIO
  , newTVarIO
  , readTVar
  , writeTChan
  , writeTVar
  )
import Data.Aeson (Value, object, (.=))
import Data.Foldable (toList)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Data.Word (Word64)

import Seer.Service.EventTypes
  ( ServiceEventEnvelope(..)
  , ServiceEventSeverity(..)
  , ServiceEventSource(..)
  )

-- | Shared event state. The TVar stores a bounded replay buffer while the TChan
-- wakes live SSE subscribers for events published after they connect.
data ServiceEventBus = ServiceEventBus
  { sebCapacity :: !Int
  , sebNextSequence :: !(TVar Word64)
  , sebBufferedEvents :: !(TVar (Seq ServiceEventEnvelope))
  , sebBroadcastEvents :: !(TChan ServiceEventEnvelope)
  }

defaultServiceEventBufferSize :: Int
defaultServiceEventBufferSize = 256

newDefaultServiceEventBus :: IO ServiceEventBus
newDefaultServiceEventBus = newServiceEventBus defaultServiceEventBufferSize

newServiceEventBus :: Int -> IO ServiceEventBus
newServiceEventBus requestedCapacity = do
  nextSequence <- newTVarIO 1
  bufferedEvents <- newTVarIO Seq.empty
  broadcastEvents <- newBroadcastTChanIO
  pure ServiceEventBus
    { sebCapacity = max 1 requestedCapacity
    , sebNextSequence = nextSequence
    , sebBufferedEvents = bufferedEvents
    , sebBroadcastEvents = broadcastEvents
    }

-- | Append an event, assigning a monotonically increasing sequence number when
-- the caller did not provide one. The published event is returned so callers can
-- include the assigned sequence in diagnostics if needed.
publishServiceEvent :: ServiceEventBus -> ServiceEventEnvelope -> IO ServiceEventEnvelope
publishServiceEvent bus event = atomically $ do
  nextSequence <- readTVar (sebNextSequence bus)
  let assignedSequence = maybe nextSequence id (serviceEventSequence event)
      published = event { serviceEventSequence = Just assignedSequence }
      nextSequence' = max (nextSequence + 1) (assignedSequence + 1)
  buffered <- readTVar (sebBufferedEvents bus)
  writeTVar (sebNextSequence bus) nextSequence'
  writeTVar (sebBufferedEvents bus) (trimBuffer (sebCapacity bus) (buffered Seq.|> published))
  writeTChan (sebBroadcastEvents bus) published
  pure published

readBufferedServiceEvents :: ServiceEventBus -> IO [ServiceEventEnvelope]
readBufferedServiceEvents bus = toList <$> atomically (readTVar (sebBufferedEvents bus))

-- | Atomically read the replay buffer and subscribe to future events. Keeping
-- both actions in one STM transaction prevents events from being missed between
-- the replay snapshot and live stream subscription.
serviceEventSnapshotAndSubscribe :: ServiceEventBus -> IO ([ServiceEventEnvelope], TChan ServiceEventEnvelope)
serviceEventSnapshotAndSubscribe bus = atomically $ do
  buffered <- readTVar (sebBufferedEvents bus)
  live <- dupTChan (sebBroadcastEvents bus)
  pure (toList buffered, live)

trimBuffer :: Int -> Seq a -> Seq a
trimBuffer capacity events
  | overflow <= 0 = events
  | otherwise = Seq.drop overflow events
  where
    overflow = Seq.length events - capacity

serviceEventEnvelopeJson :: ServiceEventEnvelope -> Value
serviceEventEnvelopeJson event = object
  [ "topic" .= serviceEventTopic event
  , "source" .= serviceEventSourceText (serviceEventSource event)
  , "severity" .= serviceEventSeverityText (serviceEventSeverity event)
  , "sequence" .= serviceEventSequence event
  , "correlation_id" .= serviceEventCorrelationId event
  , "payload" .= serviceEventPayload event
  ]

serviceEventSourceText :: ServiceEventSource -> Text
serviceEventSourceText source = case source of
  ServiceEventFromHttp -> "http"
  ServiceEventFromUi -> "ui"
  ServiceEventFromCommand -> "command"
  ServiceEventFromService -> "service"
  ServiceEventFromSystem -> "system"

serviceEventSeverityText :: ServiceEventSeverity -> Text
serviceEventSeverityText severity = case severity of
  ServiceEventDebug -> "debug"
  ServiceEventInfo -> "info"
  ServiceEventWarn -> "warn"
  ServiceEventError -> "error"
