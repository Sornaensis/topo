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
  , ServiceOperationSpec(..)
  , ServiceGroupSpec(..)
  , operationSpec
  , groupOperationMethods
  , serviceOperationMethods
  ) where

import Data.Aeson (Value)
import Data.Text (Text)

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

-- | Reusable service errors that are not tied to command IPC envelopes.
data ServiceError
  = ServiceInvalidRequest !Text
  | ServiceNotFound !Text
  | ServiceUnavailable !Text
  | ServiceRejected !Text
  | ServiceInternalError !Text
  deriving (Eq, Show)

-- | Stable metadata for one public service operation.
data ServiceOperationSpec = ServiceOperationSpec
  { serviceOperationName :: !Text
    -- ^ Stable typed operation name used in service tests and diagnostics.
  , serviceOperationMethod :: !Text
    -- ^ Current command/IPC method bridged by this operation.
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

groupOperationMethods :: ServiceGroupSpec -> [Text]
groupOperationMethods = map serviceOperationMethod . serviceGroupOperations

serviceOperationMethods :: [ServiceOperationSpec] -> [Text]
serviceOperationMethods = map serviceOperationMethod
