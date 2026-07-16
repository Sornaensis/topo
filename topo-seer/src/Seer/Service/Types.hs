{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Shared service-layer interface types.
--
-- The service layer is the stable behaviour boundary used by internal command
-- IPC compatibility, HTTP routes, UI mutations, and direct service tests. This
-- module is intentionally independent of the old command response envelope so
-- callers can translate structured service errors into command, HTTP, or UI
-- responses.
module Seer.Service.Types
  ( RawServiceHandler
  , ServiceHandler(..)
  , TypedServiceHandler
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
  , serviceHandlerSpec
  , serviceHandlerMethod
  , runServiceHandler
  , rawServiceHandler
  , serviceErrorKind
  , serviceErrorCode
  , serviceErrorMessage
  , serviceErrorDetails
  , serviceErrorText
  , serviceErrorHTTPStatus
  , validationError
  , missingField
  , invalidField
  , RequestValidator
  , FieldSpec
  , FieldValueKind(..)
  , serviceRequestBodyValue
  , validateAppServiceRequest
  , appServiceRequestValidators
  , validateFields
  , validateObjectFields
  , requiredText
  , requiredBool
  , requiredNumber
  , requiredInt
  , requiredWord64
  , requiredObject
  , requiredArray
  , requiredAny
  , optionalText
  , optionalBool
  , optionalNumber
  , optionalInt
  , optionalWord64
  , optionalObject
  , optionalArray
  , optionalAny
  , TypedServiceOperation(..)
  , ServiceOperationSpec(..)
  , ServiceGroupSpec(..)
  , operationSpec
  , typedOperation
  , adaptTypedServiceHandler
  , groupOperationMethods
  , serviceOperationMethods
  ) where

import Data.Aeson (Value(..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as Aeson
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)

import Seer.Service.Context (ServiceContext)
import Seer.Service.EventTypes
import Topo.Plugin.RPC.DataService
  ( DataResourceErrorCode(..)
  , dataResourceErrorCodeText
  , dataResourceErrorHTTPStatus
  )

-- | Raw transport-neutral handler shape used by adapters and runners.
type RawServiceHandler = ServiceContext -> ServiceRequest -> IO ServiceResult

-- | A service handler bound to the typed operation contract it implements.
--
-- The raw request/response envelope is still available for transitional command
-- and HTTP adapters, but focused service records now carry the request/response
-- type parameters of each 'TypedServiceOperation'.
data ServiceHandler request response = ServiceHandler
  { serviceHandlerOperation :: !(TypedServiceOperation request response)
  , serviceHandlerRaw :: !RawServiceHandler
  }

-- | Native service handler shape for operations that have typed contracts.
type TypedServiceHandler request response = ServiceContext -> request -> IO (Either ServiceError response)

rawServiceHandler :: TypedServiceOperation request response -> RawServiceHandler -> ServiceHandler request response
rawServiceHandler = ServiceHandler

runServiceHandler :: ServiceHandler request response -> RawServiceHandler
runServiceHandler = serviceHandlerRaw

serviceHandlerSpec :: ServiceHandler request response -> ServiceOperationSpec
serviceHandlerSpec = typedServiceOperationSpec . serviceHandlerOperation

serviceHandlerMethod :: ServiceHandler request response -> Text
serviceHandlerMethod = serviceOperationMethod . serviceHandlerSpec

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

type ServiceEventPublishHook = ServiceEventPublishRequest -> IO (Either ServiceError ServiceEventPublishResponse)

-- | Reusable service errors that are not tied to command IPC envelopes.
data ServiceError
  = ServiceInvalidRequest !Text
  | ServiceValidationError !Text ![ServiceErrorDetail]
  | ServiceUnknownMethod !Text
  | ServiceNotFound !Text
  | ServiceUnavailable !Text
  | ServiceRejected !Text
  | ServiceInternalError !Text
  | ServiceDataResourceError !DataResourceErrorCode !Text ![ServiceErrorDetail]
  deriving (Eq, Show)

serviceErrorKind :: ServiceError -> ServiceErrorKind
serviceErrorKind (ServiceInvalidRequest _) = ServiceErrorInvalidRequest
serviceErrorKind (ServiceValidationError _ _) = ServiceErrorInvalidRequest
serviceErrorKind (ServiceUnknownMethod _) = ServiceErrorNotFound
serviceErrorKind (ServiceNotFound _) = ServiceErrorNotFound
serviceErrorKind (ServiceUnavailable _) = ServiceErrorUnavailable
serviceErrorKind (ServiceRejected _) = ServiceErrorRejected
serviceErrorKind (ServiceInternalError _) = ServiceErrorInternal
serviceErrorKind (ServiceDataResourceError code _ _) = dataResourceServiceErrorKind code

dataResourceServiceErrorKind :: DataResourceErrorCode -> ServiceErrorKind
dataResourceServiceErrorKind ResourceNotFound = ServiceErrorNotFound
dataResourceServiceErrorKind RecordNotFound = ServiceErrorNotFound
dataResourceServiceErrorKind PluginUnavailable = ServiceErrorUnavailable
dataResourceServiceErrorKind ExternalDataSourceUnavailable = ServiceErrorUnavailable
dataResourceServiceErrorKind DuplicateKey = ServiceErrorRejected
dataResourceServiceErrorKind Conflict = ServiceErrorRejected
dataResourceServiceErrorKind DataResourceTimeout = ServiceErrorUnavailable
dataResourceServiceErrorKind DataResourceInternalError = ServiceErrorInternal
dataResourceServiceErrorKind OperationNotSupported = ServiceErrorInvalidRequest
dataResourceServiceErrorKind SchemaValidationFailed = ServiceErrorInvalidRequest
dataResourceServiceErrorKind PermissionDenied = ServiceErrorRejected
dataResourceServiceErrorKind QueryUnsupported = ServiceErrorInvalidRequest

serviceErrorCode :: ServiceError -> Text
serviceErrorCode (ServiceInvalidRequest _) = "invalid_request"
serviceErrorCode (ServiceValidationError _ _) = "validation_failed"
serviceErrorCode (ServiceUnknownMethod _) = "unknown_method"
serviceErrorCode (ServiceNotFound _) = "not_found"
serviceErrorCode (ServiceUnavailable _) = "unavailable"
serviceErrorCode (ServiceRejected _) = "rejected"
serviceErrorCode (ServiceInternalError _) = "internal_error"
serviceErrorCode (ServiceDataResourceError code _ _) = dataResourceErrorCodeText code

serviceErrorMessage :: ServiceError -> Text
serviceErrorMessage (ServiceInvalidRequest msg) = msg
serviceErrorMessage (ServiceValidationError msg _) = msg
serviceErrorMessage (ServiceUnknownMethod method) = "unknown service method: " <> method
serviceErrorMessage (ServiceNotFound msg) = msg
serviceErrorMessage (ServiceUnavailable msg) = msg
serviceErrorMessage (ServiceRejected msg) = msg
serviceErrorMessage (ServiceInternalError msg) = msg
serviceErrorMessage (ServiceDataResourceError _ msg _) = msg

serviceErrorDetails :: ServiceError -> [ServiceErrorDetail]
serviceErrorDetails (ServiceValidationError _ details) = details
serviceErrorDetails (ServiceUnknownMethod method) = [unknownMethodDetail method]
serviceErrorDetails (ServiceDataResourceError _ _ details) = details
serviceErrorDetails _ = []

unknownMethodDetail :: Text -> ServiceErrorDetail
unknownMethodDetail method = ServiceErrorDetail
  { serviceErrorDetailPath = ["method"]
  , serviceErrorDetailCode = "unknown_method"
  , serviceErrorDetailMessage = "service method is not registered: " <> method
  }

serviceErrorText :: ServiceError -> Text
serviceErrorText err = case serviceErrorDetails err of
  [] -> serviceErrorMessage err
  details -> serviceErrorMessage err <> ": " <> Text.intercalate "; " (map serviceErrorDetailMessage details)

-- | HTTP status code for a service error.  Data-resource errors have a
-- standardized domain-specific mapping shared with plugin RPC codes.
serviceErrorHTTPStatus :: ServiceError -> Int
serviceErrorHTTPStatus err = case err of
  ServiceDataResourceError code _ _ -> dataResourceErrorHTTPStatus code
  _ -> case serviceErrorKind err of
    ServiceErrorInvalidRequest -> 400
    ServiceErrorNotFound -> 404
    ServiceErrorUnavailable -> 503
    ServiceErrorRejected -> 409
    ServiceErrorInternal -> 500

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

type RequestValidator = Value -> Either ServiceError ()

data FieldValueKind
  = FieldAny
  | FieldText
  | FieldBool
  | FieldNumber
  | FieldInt
  | FieldWord64
  | FieldObject
  | FieldArray
  deriving (Eq, Show)

data FieldSpec = FieldSpec
  { fieldSpecName :: !Text
  , fieldSpecKind :: !FieldValueKind
  , fieldSpecRequired :: !Bool
  } deriving (Eq, Show)

validateFields :: Text -> [FieldSpec] -> RequestValidator
validateFields = validateFieldsWith False

validateObjectFields :: Text -> [FieldSpec] -> RequestValidator
validateObjectFields = validateFieldsWith True

validateFieldsWith :: Bool -> Text -> [FieldSpec] -> RequestValidator
validateFieldsWith requireObjectBody label specs = \case
  Object fields -> finish (concatMap (validateField fields) specs)
  Null
    | requireObjectBody -> Left (validationError [invalidBody label])
  Null
    | null requiredSpecs -> Right ()
    | otherwise -> finish (map (missingField . fieldSpecName) requiredSpecs)
  _ -> Left (validationError [invalidBody label])
  where
    requiredSpecs = filter fieldSpecRequired specs

invalidBody :: Text -> ServiceErrorDetail
invalidBody label = ServiceErrorDetail
  { serviceErrorDetailPath = []
  , serviceErrorDetailCode = "invalid_body"
  , serviceErrorDetailMessage = label <> " request body must be an object"
  }

finish :: [ServiceErrorDetail] -> Either ServiceError ()
finish [] = Right ()
finish details = Left (validationError details)

validateField :: KM.KeyMap Value -> FieldSpec -> [ServiceErrorDetail]
validateField fields spec =
  case KM.lookup (Key.fromText (fieldSpecName spec)) fields of
    Nothing
      | fieldSpecRequired spec -> [missingField (fieldSpecName spec)]
      | otherwise -> []
    Just value
      | fieldMatches (fieldSpecKind spec) value -> []
      | otherwise -> [invalidField (fieldSpecName spec) (fieldKindDescription (fieldSpecKind spec))]

fieldMatches :: FieldValueKind -> Value -> Bool
fieldMatches FieldAny _ = True
fieldMatches FieldText (String _) = True
fieldMatches FieldText _ = False
fieldMatches FieldBool (Bool _) = True
fieldMatches FieldBool _ = False
fieldMatches FieldNumber (Number _) = True
fieldMatches FieldNumber _ = False
fieldMatches FieldInt value = isJust (Aeson.parseMaybe Aeson.parseJSON value :: Maybe Int)
fieldMatches FieldWord64 value = isJust (Aeson.parseMaybe Aeson.parseJSON value :: Maybe Word64)
fieldMatches FieldObject (Object _) = True
fieldMatches FieldObject _ = False
fieldMatches FieldArray (Array _) = True
fieldMatches FieldArray _ = False

fieldKindDescription :: FieldValueKind -> Text
fieldKindDescription FieldAny = "any value"
fieldKindDescription FieldText = "text"
fieldKindDescription FieldBool = "boolean"
fieldKindDescription FieldNumber = "number"
fieldKindDescription FieldInt = "integer"
fieldKindDescription FieldWord64 = "non-negative integer"
fieldKindDescription FieldObject = "object"
fieldKindDescription FieldArray = "array"

required :: Text -> FieldValueKind -> FieldSpec
required name kind = FieldSpec name kind True

optional :: Text -> FieldValueKind -> FieldSpec
optional name kind = FieldSpec name kind False

requiredText, requiredBool, requiredNumber, requiredInt, requiredWord64, requiredObject, requiredArray, requiredAny :: Text -> FieldSpec
requiredText name = required name FieldText
requiredBool name = required name FieldBool
requiredNumber name = required name FieldNumber
requiredInt name = required name FieldInt
requiredWord64 name = required name FieldWord64
requiredObject name = required name FieldObject
requiredArray name = required name FieldArray
requiredAny name = required name FieldAny

optionalText, optionalBool, optionalNumber, optionalInt, optionalWord64, optionalObject, optionalArray, optionalAny :: Text -> FieldSpec
optionalText name = optional name FieldText
optionalBool name = optional name FieldBool
optionalNumber name = optional name FieldNumber
optionalInt name = optional name FieldInt
optionalWord64 name = optional name FieldWord64
optionalObject name = optional name FieldObject
optionalArray name = optional name FieldArray
optionalAny name = optional name FieldAny

serviceRequestBodyValue :: ServiceRequest -> Value
serviceRequestBodyValue = fromMaybe Null . serviceRequestBody

validateAppServiceRequest :: Text -> Value -> Either ServiceError ()
validateAppServiceRequest method params =
  maybe (Right ()) ($ params) (lookup method appServiceRequestValidators)

appServiceRequestValidators :: [(Text, RequestValidator)]
appServiceRequestValidators =
  [ ("get_slider", fields "get_slider" [requiredText "name"])
  , ("set_slider", fields "set_slider" [requiredText "name", requiredNumber "value"])
  , ("set_sliders", fields "set_sliders" [requiredObject "values"])
  , ("get_enums", fields "get_enums" [requiredText "type"])
  , ("save_preset", fields "save_preset" [requiredText "name"])
  , ("load_preset", fields "load_preset" [requiredText "name"])

  , ("save_world", fields "save_world" [requiredText "name"])
  , ("load_world", fields "load_world" [requiredText "name"])
  , ("set_world_name", fields "set_world_name" [requiredText "name"])

  , ("get_hex", fields "get_hex" [requiredInt "q", requiredInt "r"])
  , ("get_chunk_summary", fields "get_chunk_summary" [requiredInt "chunk"])
  , ("find_hexes", fields "find_hexes" [requiredArray "filters", optionalInt "limit"])
  , ("export_terrain_data", fields "export_terrain_data" [optionalArray "chunks", optionalArray "fields"])
  , ("get_overlay_schema", fields "get_overlay_schema" [requiredText "overlay"])
  , ("get_overlay_provenance", fields "get_overlay_provenance" [requiredText "overlay"])
  , ("export_overlay_data", fields "export_overlay_data" [requiredText "overlay", optionalArray "chunks"])
  , ("export_mesh_data", fields "export_mesh_data" [optionalInt "x0", optionalInt "y0", optionalInt "x1", optionalInt "y1"])
  , ("export_sample_data", fields "export_sample_data" [requiredNumber "x", requiredNumber "y", optionalBool "real_units"])

  , ("editor_toggle", fields "editor_toggle" [optionalBool "active"])
  , ("editor_set_tool", fields "editor_set_tool" [requiredText "tool"])
  , ("editor_set_brush", objectFields "editor_set_brush"
      [ optionalInt "radius"
      , optionalNumber "strength"
      , optionalText "falloff"
      , optionalInt "smooth_passes"
      , optionalNumber "noise_frequency"
      , optionalInt "erode_passes"
      ])
  , ("editor_brush_stroke", fields "editor_brush_stroke" [requiredInt "q", requiredInt "r"])
  , ("editor_brush_line", fields "editor_brush_line" [requiredInt "from_q", requiredInt "from_r", requiredInt "to_q", requiredInt "to_r"])
  , ("editor_set_biome", fields "editor_set_biome" [requiredAny "biome"])
  , ("editor_set_form", fields "editor_set_form" [requiredAny "form"])
  , ("editor_set_hardness", fields "editor_set_hardness" [requiredNumber "hardness"])

  , ("set_stage_enabled", fields "set_stage_enabled" [requiredText "stage", requiredBool "enabled"])
  , ("set_plugin_enabled", fields "set_plugin_enabled" [requiredText "name", requiredBool "enabled"])
  , ("set_plugin_param", fields "set_plugin_param" [requiredText "plugin", requiredText "param", requiredAny "value"])

  , ("data_list_resources", fields "data_list_resources" [requiredText "plugin"])
  , ("data_list_records", fields "data_list_records" [requiredText "plugin", requiredText "resource", optionalInt "page_size", optionalInt "page_offset"])
  , ("data_get_record", fields "data_get_record" [requiredText "plugin", requiredText "resource", requiredAny "key"])
  , ("data_create_record", fields "data_create_record" [requiredText "plugin", requiredText "resource", requiredObject "fields"])
  , ("data_update_record", fields "data_update_record" [requiredText "plugin", requiredText "resource", requiredAny "key", requiredObject "fields"])
  , ("data_delete_record", fields "data_delete_record" [requiredText "plugin", requiredText "resource", requiredAny "key"])

  , ("set_sim_auto_tick", fields "set_sim_auto_tick" [requiredBool "enabled", optionalNumber "rate"])
  , ("sim_tick", fields "sim_tick" [optionalInt "count"])

  , ("set_seed", fields "set_seed" [requiredWord64 "seed"])
  , ("set_view_mode", fields "set_view_mode" [requiredText "mode", optionalText "basis", optionalText "temporal_basis", optionalInt "field_index"])
  , ("set_view", fields "set_view"
      [ optionalText "base_mode"
      , optionalText "base"
      , optionalAny "overlay_mode"
      , optionalAny "overlay"
      , optionalAny "plugin_overlay"
      , optionalText "weather_basis"
      , optionalText "basis"
      , optionalText "temporal_basis"
      , optionalNumber "overlay_opacity"
      , optionalInt "field_index"
      , optionalAny "overlay_field"
      ])
  , ("set_config_tab", fields "set_config_tab" [requiredText "tab"])
  , ("select_hex", fields "select_hex" [optionalInt "q", optionalInt "r"])
  , ("set_overlay", fields "set_overlay" [requiredText "overlay", optionalInt "field_index"])
  , ("list_overlay_fields", fields "list_overlay_fields" [optionalText "overlay"])
  , ("cycle_overlay", fields "cycle_overlay" [requiredInt "direction"])
  , ("cycle_overlay_field", fields "cycle_overlay_field" [requiredInt "direction"])
  , ("set_camera", fields "set_camera" [requiredNumber "x", requiredNumber "y", optionalNumber "zoom"])
  , ("zoom_to_chunk", fields "zoom_to_chunk" [requiredInt "chunk"])
  , ("set_left_panel", fields "set_left_panel" [requiredBool "visible"])
  , ("set_left_tab", fields "set_left_tab" [requiredText "tab"])
  , ("toggle_config_panel", fields "toggle_config_panel" [optionalBool "visible"])
  , ("set_log_collapsed", fields "set_log_collapsed" [requiredBool "collapsed"])
  , ("set_log_level", fields "set_log_level" [requiredText "level"])
  , ("viewport_scroll", fields "viewport_scroll" [requiredInt "delta", optionalInt "x", optionalInt "y"])
  , ("viewport_click", fields "viewport_click" [requiredInt "x", requiredInt "y", optionalText "button"])
  , ("viewport_drag", fields "viewport_drag" [requiredInt "x1", requiredInt "y1", requiredInt "x2", requiredInt "y2"])
  , ("viewport_hover", fields "viewport_hover" [requiredInt "x", requiredInt "y"])
  , ("click_widget", fields "click_widget"
      [ requiredText "widget_id"
      , optionalNumber "normalized_position"
      , optionalInt "item_index"
      ])
  , ("get_widget_state", fields "get_widget_state" [requiredText "widget_id"])
  , ("set_dialog_text", fields "set_dialog_text" [requiredText "text", optionalText "target"])
  , ("send_key", fields "send_key" [requiredText "key"])
  ]
  where
    fields = validateFields
    objectFields = validateObjectFields

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

adaptTypedServiceHandler
  :: TypedServiceOperation request response
  -> (Value -> Either ServiceError request)
  -> (response -> Value)
  -> TypedServiceHandler request response
  -> ServiceHandler request response
adaptTypedServiceHandler operation decodeRequest encodeResponse handler =
  rawServiceHandler operation $ \ctx request -> do
    let params = serviceRequestBodyValue request
        method = serviceOperationMethod (typedServiceOperationSpec operation)
    case validateAppServiceRequest method params of
      Left err -> pure (Left err)
      Right () -> case decodeRequest params of
        Left err -> pure (Left err)
        Right typedRequest -> do
          result <- handler ctx typedRequest
          pure (ServiceResponse . encodeResponse <$> result)

groupOperationMethods :: ServiceGroupSpec -> [Text]
groupOperationMethods = map serviceOperationMethod . serviceGroupOperations

serviceOperationMethods :: [ServiceOperationSpec] -> [Text]
serviceOperationMethods = map serviceOperationMethod

serviceEventPublishOperation :: TypedServiceOperation ServiceEventPublishRequest ServiceEventPublishResponse
serviceEventPublishOperation = typedOperation $
  operationSpec "events.publish" "publish_event" "Publish an application event to HTTP/event and UI observers."
