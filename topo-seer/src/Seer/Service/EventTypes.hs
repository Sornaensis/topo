{-# LANGUAGE StrictData #-}

-- | Transport-neutral event contracts shared by service metadata and HTTP SSE.
module Seer.Service.EventTypes
  ( ServiceEventSource(..)
  , ServiceEventSeverity(..)
  , ServiceEventEnvelope(..)
  , ServiceEventPublishRequest(..)
  , ServiceEventPublishResponse(..)
  ) where

import Data.Aeson (Value)
import Data.Text (Text)
import Data.Word (Word64)

-- | Event origin metadata shared by HTTP event streams and UI publication hooks.
data ServiceEventSource
  = ServiceEventFromHttp
  | ServiceEventFromUi
  | ServiceEventFromCommand
  | ServiceEventFromService
  | ServiceEventFromSystem
  deriving (Eq, Show)

data ServiceEventSeverity
  = ServiceEventDebug
  | ServiceEventInfo
  | ServiceEventWarn
  | ServiceEventError
  deriving (Eq, Show)

-- | Event envelope with a topic-specific JSON payload. Sequence numbers are
-- assigned by the event bus when omitted by publishers.
data ServiceEventEnvelope = ServiceEventEnvelope
  { serviceEventTopic :: !Text
  , serviceEventSource :: !ServiceEventSource
  , serviceEventSeverity :: !ServiceEventSeverity
  , serviceEventSequence :: !(Maybe Word64)
  , serviceEventCorrelationId :: !(Maybe Text)
  , serviceEventPayload :: !Value
  } deriving (Eq, Show)

newtype ServiceEventPublishRequest = ServiceEventPublishRequest
  { serviceEventPublishEnvelope :: ServiceEventEnvelope
  } deriving (Eq, Show)

data ServiceEventPublishResponse = ServiceEventPublishResponse
  { serviceEventPublishAccepted :: !Bool
  , serviceEventPublishTopic :: !Text
  } deriving (Eq, Show)
