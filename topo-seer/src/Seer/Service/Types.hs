{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Shared service-layer interface types.
--
-- The service layer is the stable behaviour boundary used by command IPC,
-- future HTTP routes, UI mutations, and direct service tests.  This module is
-- intentionally independent of the old command response envelope so callers can
-- translate structured service errors into command, HTTP, or UI responses.
module Seer.Service.Types
  ( ServiceHandler
  , ServiceRequest(..)
  , ServiceResponse(..)
  , ServiceResult
  , ServiceError(..)
  , ServiceErrorKind(..)
  , ServiceErrorDetail(..)
  , AsyncStatusPhase(..)
  , AsyncStatusSnapshot(..)
  , ServiceEventSource(..)
  , ServiceEventSeverity(..)
  , ServiceEventEnvelope(..)
  , ServiceEventPublishRequest(..)
  , ServiceEventPublishResponse(..)
  , ServiceEventPublishHook
  , serviceEventPublishOperation
  , serviceErrorKind
  , serviceErrorCode
  , serviceErrorMessage
  , serviceErrorDetails
  , serviceErrorText
  , validationError
  , missingField
  , invalidField
  , TypedServiceOperation(..)
  , ServiceOperationSpec(..)
  , ServiceGroupSpec(..)
  , operationSpec
  , typedOperation
  , groupOperationMethods
  , serviceOperationMethods
  ) where

import Data.Aeson (Value)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)

import Seer.Service.Context (ServiceContext)

-- | Generic service handler shape used by the initial AppService surface.
--
-- Focused services expose named operations using this shape while later M2
-- tasks can refine individual requests/responses into domain-specific types.
type ServiceHandler = ServiceContext -> ServiceRequest -> IO ServiceResult

-- | Transport-neutral request envelope for an operation.
--
-- The initial AppService surface still carries JSON payloads because existing
-- command handlers are JSON-oriented.  Keeping the payload behind a service
-- request type prevents command IPC's parameter shape from becoming the public
-- service contract and leaves room for typed request records in follow-up M2
-- extraction work.
data ServiceRequest = ServiceRequest
  { serviceRequestBody :: !(Maybe Value)
  } deriving (Eq, Show)

-- | Transport-neutral response envelope for an operation.
newtype ServiceResponse = ServiceResponse
  { serviceResponseBody :: Value
  } deriving (Eq, Show)

type ServiceResult = Either ServiceError ServiceResponse

data ServiceErrorKind
  = ServiceErrorInvalidRequest
  | ServiceErrorNotFound
  | ServiceErrorUnavailable
  | ServiceErrorRejected
  | ServiceErrorInternal
  deriving (Eq, Show)

data ServiceErrorDetail = ServiceErrorDetail
  { serviceErrorDetailPath :: ![Text]
  , serviceErrorDetailCode :: !Text
  , serviceErrorDetailMessage :: !Text
  } deriving (Eq, Show)

-- | Shared async lifecycle vocabulary for generation, plugin, data, and
-- simulation status surfaces.
data AsyncStatusPhase
  = AsyncStatusIdle
  | AsyncStatusQueued
  | AsyncStatusRunning
  | AsyncStatusSucceeded
  | AsyncStatusFailed
  | AsyncStatusUnavailable
  deriving (Eq, Show)

data AsyncStatusSnapshot = AsyncStatusSnapshot
  { asyncStatusName :: !Text
  , asyncStatusPhase :: !AsyncStatusPhase
  , asyncStatusActive :: !Bool
  , asyncStatusCurrent :: !(Maybe Int)
  , asyncStatusTotal :: !(Maybe Int)
  , asyncStatusMessage :: !(Maybe Text)
  } deriving (Eq, Show)

-- | Transport-neutral event metadata shared by future HTTP event streams and UI
-- publication hooks.  These types deliberately carry 'Value' payloads so each
-- event topic can evolve independently while retaining typed origin/severity
-- metadata at the service boundary.
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

type ServiceEventPublishHook = ServiceEventPublishRequest -> IO (Either ServiceError ServiceEventPublishResponse)

-- | Reusable service errors that are not tied to command IPC envelopes.
data ServiceError
  = ServiceInvalidRequest !Text
  | ServiceValidationError !Text ![ServiceErrorDetail]
  | ServiceNotFound !Text
  | ServiceUnavailable !Text
  | ServiceRejected !Text
  | ServiceInternalError !Text
  deriving (Eq, Show)

serviceErrorKind :: ServiceError -> ServiceErrorKind
serviceErrorKind (ServiceInvalidRequest _) = ServiceErrorInvalidRequest
serviceErrorKind (ServiceValidationError _ _) = ServiceErrorInvalidRequest
serviceErrorKind (ServiceNotFound _) = ServiceErrorNotFound
serviceErrorKind (ServiceUnavailable _) = ServiceErrorUnavailable
serviceErrorKind (ServiceRejected _) = ServiceErrorRejected
serviceErrorKind (ServiceInternalError _) = ServiceErrorInternal

serviceErrorCode :: ServiceError -> Text
serviceErrorCode (ServiceInvalidRequest _) = "invalid_request"
serviceErrorCode (ServiceValidationError _ _) = "validation_failed"
serviceErrorCode (ServiceNotFound _) = "not_found"
serviceErrorCode (ServiceUnavailable _) = "unavailable"
serviceErrorCode (ServiceRejected _) = "rejected"
serviceErrorCode (ServiceInternalError _) = "internal_error"

serviceErrorMessage :: ServiceError -> Text
serviceErrorMessage (ServiceInvalidRequest msg) = msg
serviceErrorMessage (ServiceValidationError msg _) = msg
serviceErrorMessage (ServiceNotFound msg) = msg
serviceErrorMessage (ServiceUnavailable msg) = msg
serviceErrorMessage (ServiceRejected msg) = msg
serviceErrorMessage (ServiceInternalError msg) = msg

serviceErrorDetails :: ServiceError -> [ServiceErrorDetail]
serviceErrorDetails (ServiceValidationError _ details) = details
serviceErrorDetails _ = []

serviceErrorText :: ServiceError -> Text
serviceErrorText err = case serviceErrorDetails err of
  [] -> serviceErrorMessage err
  details -> serviceErrorMessage err <> ": " <> Text.intercalate "; " (map serviceErrorDetailMessage details)

validationError :: [ServiceErrorDetail] -> ServiceError
validationError = ServiceValidationError "validation failed"

missingField :: Text -> ServiceErrorDetail
missingField field = ServiceErrorDetail
  { serviceErrorDetailPath = [field]
  , serviceErrorDetailCode = "missing_field"
  , serviceErrorDetailMessage = "missing required field '" <> field <> "'"
  }

invalidField :: Text -> Text -> ServiceErrorDetail
invalidField field expected = ServiceErrorDetail
  { serviceErrorDetailPath = [field]
  , serviceErrorDetailCode = "invalid_field"
  , serviceErrorDetailMessage = "invalid field '" <> field <> "' (expected " <> expected <> ")"
  }

-- | Type-level association between an operation and its request/response pair.
--
-- This lets focused service modules publish typed contracts while the
-- transitional AppService adapter still carries JSON through 'ServiceHandler'.
newtype TypedServiceOperation request response = TypedServiceOperation
  { typedServiceOperationSpec :: ServiceOperationSpec
  } deriving (Eq, Show)

-- | Stable metadata for one public service operation.
data ServiceOperationSpec = ServiceOperationSpec
  { serviceOperationName :: !Text
    -- ^ Stable typed operation name used in service tests and diagnostics.
  , serviceOperationMethod :: !Text
    -- ^ Current command/IPC method or internal hook name bridged by this operation.
  , serviceOperationDescription :: !Text
    -- ^ Short human-readable summary.
  } deriving (Eq, Show)

-- | A focused service group and its current command-method coverage.
data ServiceGroupSpec = ServiceGroupSpec
  { serviceGroupName :: !Text
  , serviceGroupOperations :: ![ServiceOperationSpec]
  } deriving (Eq, Show)

operationSpec :: Text -> Text -> Text -> ServiceOperationSpec
operationSpec = ServiceOperationSpec

typedOperation :: ServiceOperationSpec -> TypedServiceOperation request response
typedOperation = TypedServiceOperation

groupOperationMethods :: ServiceGroupSpec -> [Text]
groupOperationMethods = map serviceOperationMethod . serviceGroupOperations

serviceOperationMethods :: [ServiceOperationSpec] -> [Text]
serviceOperationMethods = map serviceOperationMethod

serviceEventPublishOperation :: TypedServiceOperation ServiceEventPublishRequest ServiceEventPublishResponse
serviceEventPublishOperation = typedOperation $
  operationSpec "events.publish" "publish_event" "Publish an application event to HTTP/event and UI observers."
