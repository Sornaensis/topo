{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | CRUD message payload types for plugin data services.
--
-- These types define the query and mutation payloads exchanged between
-- the host and plugins that manage data resources.  They are
-- transported inside 'Topo.Plugin.RPC.Protocol.RPCEnvelope' messages
-- using the @query_resource@, @query_result@, @mutate_resource@, and
-- @mutate_result@ message types.
--
-- = Wire format
--
-- __Query request__ (@query_resource@):
--
-- @
-- { "resource": "cultures",
--   "query": { "type": "all" },
--   "page_size": 50,
--   "page_offset": 0 }
-- @
--
-- __Query result__ (@query_result@):
--
-- @
-- { "resource": "cultures",
--   "records": [ { "id": 1, "name": "Dwarves" }, … ],
--   "total_count": 42 }
-- @
--
-- __Mutate request__ (@mutate_resource@):
--
-- @
-- { "resource": "cultures",
--   "mutation": { "type": "create", "record": { "name": "Elves" } } }
-- @
--
-- __Mutate result__ (@mutate_result@):
--
-- @
-- { "success": true, "record": { "id": 2, "name": "Elves" } }
-- @
module Topo.Plugin.RPC.DataService
  ( -- * Records
    DataRecord(..)
    -- * Query
  , QueryResource(..)
  , DataQuery(..)
  , QueryResult(..)
    -- * Mutation
  , MutateResource(..)
  , DataMutation(..)
  , MutateResult(..)
    -- * Standardized data-resource errors
  , DataResourceErrorCode(..)
  , DataResourceFailure(..)
  , dataResourceErrorCodeText
  , dataResourceErrorCodeFromText
  , dataResourceErrorCodeFromValue
  , dataResourceErrorHTTPStatus
  , dataResourceErrorRPCCode
  , dataResourceErrorFromRPCCode
  , dataResourceFailureText
  , parseDataResourceFailureText
  , classifyDataResourceErrorText
  , dataResourceFailureFromText
  ) where

import Control.Applicative ((<|>))
import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Value(..)
  , (.:)
  , (.:?)
  , (.=)
  , object
  , withObject
  , withText
  )
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as Aeson (Parser, parseMaybe)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

------------------------------------------------------------------------
-- Standardized errors
------------------------------------------------------------------------

-- | Stable error codes for plugin-owned data-resource operations.
--
-- The textual JSON form is the snake_case code used by service and HTTP
-- error envelopes.  Constructors intentionally use domain names so host,
-- plugin, UI, and HTTP boundaries can share one vocabulary.
data DataResourceErrorCode
  = ResourceNotFound
  | OperationNotSupported
  | RecordNotFound
  | DuplicateKey
  | SchemaValidationFailed
  | PermissionDenied
  | Conflict
  | PluginUnavailable
  | ExternalDataSourceUnavailable
  | QueryUnsupported
  | DataResourceTimeout
  | DataResourceInternalError
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic)

instance ToJSON DataResourceErrorCode where
  toJSON = String . dataResourceErrorCodeText

instance FromJSON DataResourceErrorCode where
  parseJSON = withText "DataResourceErrorCode" $ \t ->
    case dataResourceErrorCodeFromText t of
      Just code -> pure code
      Nothing -> fail ("unknown data resource error code: " <> Text.unpack t)

-- | Structured data-resource failure propagated across plugin/host/service
-- boundaries before being rendered for legacy command or UI surfaces.
data DataResourceFailure = DataResourceFailure
  { drfCode :: !DataResourceErrorCode
  , drfMessage :: !Text
  } deriving (Eq, Show, Generic)

instance ToJSON DataResourceFailure where
  toJSON failure = object
    [ "code" .= drfCode failure
    , "message" .= drfMessage failure
    ]

instance FromJSON DataResourceFailure where
  parseJSON = withObject "DataResourceFailure" $ \o ->
    DataResourceFailure
      <$> o .: "code"
      <*> o .: "message"

-- | Wire/service text for a standardized data-resource error code.
dataResourceErrorCodeText :: DataResourceErrorCode -> Text
dataResourceErrorCodeText ResourceNotFound = "resource_not_found"
dataResourceErrorCodeText OperationNotSupported = "operation_not_supported"
dataResourceErrorCodeText RecordNotFound = "record_not_found"
dataResourceErrorCodeText DuplicateKey = "duplicate_key"
dataResourceErrorCodeText SchemaValidationFailed = "schema_validation_failed"
dataResourceErrorCodeText PermissionDenied = "permission_denied"
dataResourceErrorCodeText Conflict = "conflict"
dataResourceErrorCodeText PluginUnavailable = "plugin_unavailable"
dataResourceErrorCodeText ExternalDataSourceUnavailable = "external_data_source_unavailable"
dataResourceErrorCodeText QueryUnsupported = "query_unsupported"
dataResourceErrorCodeText DataResourceTimeout = "timeout"
dataResourceErrorCodeText DataResourceInternalError = "data_resource_error"

-- | Parse a stable data-resource error code from its wire/service text.
dataResourceErrorCodeFromText :: Text -> Maybe DataResourceErrorCode
dataResourceErrorCodeFromText t =
  lookup normalized
    [ (dataResourceErrorCodeText code, code)
    | code <- [minBound .. maxBound]
    ]
  <|> parseNumericRPCCode normalized
  where
    normalized = Text.toLower (Text.strip t)

-- | Tolerantly parse an optional standardized code from arbitrary JSON.
-- Unknown/vendor values are ignored so older or extended plugins can still
-- fall back to numeric RPC codes or free-text classification.
dataResourceErrorCodeFromValue :: Value -> Maybe DataResourceErrorCode
dataResourceErrorCodeFromValue (String t) = dataResourceErrorCodeFromText t
dataResourceErrorCodeFromValue value@(Number _) = Aeson.parseMaybe parseJSON value >>= dataResourceErrorFromRPCCode
dataResourceErrorCodeFromValue _ = Nothing

parseNumericRPCCode :: Text -> Maybe DataResourceErrorCode
parseNumericRPCCode t = case reads (Text.unpack t) of
  [(n, "")] -> dataResourceErrorFromRPCCode (n :: Int)
  _ -> Nothing

-- | HTTP status code for a standardized data-resource error.
dataResourceErrorHTTPStatus :: DataResourceErrorCode -> Int
dataResourceErrorHTTPStatus ResourceNotFound = 404
dataResourceErrorHTTPStatus OperationNotSupported = 405
dataResourceErrorHTTPStatus RecordNotFound = 404
dataResourceErrorHTTPStatus DuplicateKey = 409
dataResourceErrorHTTPStatus SchemaValidationFailed = 422
dataResourceErrorHTTPStatus PermissionDenied = 403
dataResourceErrorHTTPStatus Conflict = 409
dataResourceErrorHTTPStatus PluginUnavailable = 503
dataResourceErrorHTTPStatus ExternalDataSourceUnavailable = 503
dataResourceErrorHTTPStatus QueryUnsupported = 400
dataResourceErrorHTTPStatus DataResourceTimeout = 504
dataResourceErrorHTTPStatus DataResourceInternalError = 500

-- | Numeric RPC error code used when a plugin returns a protocol-level
-- @error@ envelope for a data-resource operation.
dataResourceErrorRPCCode :: DataResourceErrorCode -> Int
dataResourceErrorRPCCode ResourceNotFound = 1001
dataResourceErrorRPCCode OperationNotSupported = 1002
dataResourceErrorRPCCode RecordNotFound = 1003
dataResourceErrorRPCCode DuplicateKey = 1004
dataResourceErrorRPCCode SchemaValidationFailed = 1005
dataResourceErrorRPCCode PermissionDenied = 1006
dataResourceErrorRPCCode Conflict = 1007
dataResourceErrorRPCCode PluginUnavailable = 1008
dataResourceErrorRPCCode ExternalDataSourceUnavailable = 1009
dataResourceErrorRPCCode QueryUnsupported = 1010
dataResourceErrorRPCCode DataResourceTimeout = 1011
dataResourceErrorRPCCode DataResourceInternalError = 1099

-- | Recover a standardized error from an RPC numeric code.
dataResourceErrorFromRPCCode :: Int -> Maybe DataResourceErrorCode
dataResourceErrorFromRPCCode n = lookup n
  [ (dataResourceErrorRPCCode code, code)
  | code <- [minBound .. maxBound]
  ]

-- | Render a failure into the legacy command/UI text form while preserving a
-- machine-readable prefix for the service adapter.
dataResourceFailureText :: DataResourceFailure -> Text
dataResourceFailureText failure = dataResourceErrorCodeText (drfCode failure) <> ": " <> drfMessage failure

-- | Parse the legacy command/UI text form emitted by 'dataResourceFailureText'.
parseDataResourceFailureText :: Text -> Maybe DataResourceFailure
parseDataResourceFailureText raw = do
  let (codeText, rest) = Text.breakOn ":" raw
  code <- dataResourceErrorCodeFromText (Text.strip codeText)
  let msg = Text.strip (Text.drop 1 rest)
  pure DataResourceFailure
    { drfCode = code
    , drfMessage = if Text.null msg then dataResourceErrorCodeText code else msg
    }

-- | Heuristic mapping for legacy plugin errors that only carry free text.
classifyDataResourceErrorText :: Text -> DataResourceErrorCode
classifyDataResourceErrorText msg
  | anyIn ["timed out", "timeout"] = DataResourceTimeout
  | anyIn ["unknown resource", "resource not found", "no such resource"] = ResourceNotFound
  | anyIn ["record not found", "no such record"] = RecordNotFound
  | anyIn ["duplicate", "unique constraint", "already exists"] = DuplicateKey
  | anyIn ["schema", "validation", "invalid payload", "invalid query", "invalid mutate"] = SchemaValidationFailed
  | anyIn ["permission", "forbidden", "unauthorized", "access denied"] = PermissionDenied
  | anyIn ["conflict", "version", "etag", "precondition"] = Conflict
  | anyIn ["external data-source", "external datasource", "external data source", "data-source unavailable", "data source unavailable"] = ExternalDataSourceUnavailable
  | anyIn ["plugin unavailable", "unknown plugin", "not connected"] = PluginUnavailable
  | anyIn ["query unsupported", "unsupported query"] = QueryUnsupported
  | anyIn ["not supported", "does not support", "unsupported operation"] = OperationNotSupported
  | otherwise = DataResourceInternalError
  where
    lowered = Text.toLower msg
    anyIn needles = any (`Text.isInfixOf` lowered) needles

-- | Build a structured failure from either the standardized legacy text form
-- or free-form plugin text.
dataResourceFailureFromText :: Text -> DataResourceFailure
dataResourceFailureFromText msg = case parseDataResourceFailureText msg of
  Just failure -> failure
  Nothing -> DataResourceFailure (classifyDataResourceErrorText msg) msg

------------------------------------------------------------------------
-- Data records
------------------------------------------------------------------------

-- | A flat key→value record returned by or sent to a plugin data
-- service.
--
-- On the wire this is a plain JSON object: @{ "id": 1, "name": "…" }@.
newtype DataRecord = DataRecord { unDataRecord :: Map Text Value }
  deriving (Eq, Show)

instance ToJSON DataRecord where
  toJSON (DataRecord m) =
    object [ Key.fromText k .= v | (k, v) <- Map.toList m ]

instance FromJSON DataRecord where
  parseJSON = withObject "DataRecord" $ \o ->
    pure . DataRecord . Map.fromList $
      [ (Key.toText k, v)
      | (k, v) <- KM.toList o
      ]

------------------------------------------------------------------------
-- Query
------------------------------------------------------------------------

-- | What kind of data query to perform.
data DataQuery
  = QueryAll
    -- ^ List all records.
  | QueryByKey !Value
    -- ^ Fetch a single record by primary key.
  | QueryByHex !Int !Int
    -- ^ Fetch records at a hex coordinate (chunk index, tile index).
  | QueryByField !Text !Value
    -- ^ Fetch records where a named field equals a value.
  deriving (Eq, Show, Generic)

instance ToJSON DataQuery where
  toJSON QueryAll = object [ "type" .= ("all" :: Text) ]
  toJSON (QueryByKey k) = object
    [ "type" .= ("by_key" :: Text)
    , "key"  .= k
    ]
  toJSON (QueryByHex chunk tile) = object
    [ "type"  .= ("by_hex" :: Text)
    , "chunk" .= chunk
    , "tile"  .= tile
    ]
  toJSON (QueryByField field val) = object
    [ "type"  .= ("by_field" :: Text)
    , "field" .= field
    , "value" .= val
    ]

instance FromJSON DataQuery where
  parseJSON = withObject "DataQuery" $ \o -> do
    ty <- o .: "type" :: Aeson.Parser Text
    case ty of
      "all"      -> pure QueryAll
      "by_key"   -> QueryByKey <$> o .: "key"
      "by_hex"   -> QueryByHex <$> o .: "chunk" <*> o .: "tile"
      "by_field" -> QueryByField <$> o .: "field" <*> o .: "value"
      _          -> fail ("unknown query type: " <> Text.unpack ty)

-- | A data query request sent from host to plugin.
data QueryResource = QueryResource
  { qrResource   :: !Text
    -- ^ Target resource name.
  , qrQuery      :: !DataQuery
    -- ^ Query discriminator.
  , qrPageSize   :: !(Maybe Int)
    -- ^ Optional maximum number of records to return.
  , qrPageOffset :: !(Maybe Int)
    -- ^ Optional offset for pagination.
  } deriving (Eq, Show, Generic)

instance ToJSON QueryResource where
  toJSON qr = object $
    [ "resource" .= qrResource qr
    , "query"    .= qrQuery qr
    ] <>
    [ "page_size"   .= ps | Just ps <- [qrPageSize qr] ] <>
    [ "page_offset" .= po | Just po <- [qrPageOffset qr] ]

instance FromJSON QueryResource where
  parseJSON = withObject "QueryResource" $ \o ->
    QueryResource
      <$> o .:  "resource"
      <*> o .:  "query"
      <*> o .:? "page_size"
      <*> o .:? "page_offset"

-- | Query result returned from plugin to host.
data QueryResult = QueryResult
  { qrsResource   :: !Text
    -- ^ Resource name (echoed from request).
  , qrsRecords    :: ![DataRecord]
    -- ^ Matching records.
  , qrsTotalCount :: !(Maybe Int)
    -- ^ Total number of matching records (for pagination).
  } deriving (Eq, Show, Generic)

instance ToJSON QueryResult where
  toJSON qr = object $
    [ "resource" .= qrsResource qr
    , "records"  .= qrsRecords qr
    ] <>
    [ "total_count" .= tc | Just tc <- [qrsTotalCount qr] ]

instance FromJSON QueryResult where
  parseJSON = withObject "QueryResult" $ \o ->
    QueryResult
      <$> o .:  "resource"
      <*> o .:  "records"
      <*> o .:? "total_count"

------------------------------------------------------------------------
-- Mutation
------------------------------------------------------------------------

-- | What kind of data mutation to perform.
data DataMutation
  = MutCreate !DataRecord
    -- ^ Create a new record.
  | MutUpdate !Value !DataRecord
    -- ^ Update an existing record by primary key.
  | MutDelete !Value
    -- ^ Delete a record by primary key.
  | MutSetHex !Int !Int !DataRecord
    -- ^ Create or update a record at a hex coordinate.
  deriving (Eq, Show, Generic)

instance ToJSON DataMutation where
  toJSON (MutCreate record) = object
    [ "type"   .= ("create" :: Text)
    , "record" .= record
    ]
  toJSON (MutUpdate key record) = object
    [ "type"   .= ("update" :: Text)
    , "key"    .= key
    , "record" .= record
    ]
  toJSON (MutDelete key) = object
    [ "type" .= ("delete" :: Text)
    , "key"  .= key
    ]
  toJSON (MutSetHex chunk tile record) = object
    [ "type"   .= ("set_hex" :: Text)
    , "chunk"  .= chunk
    , "tile"   .= tile
    , "record" .= record
    ]

instance FromJSON DataMutation where
  parseJSON = withObject "DataMutation" $ \o -> do
    ty <- o .: "type" :: Aeson.Parser Text
    case ty of
      "create"  -> MutCreate <$> o .: "record"
      "update"  -> MutUpdate <$> o .: "key" <*> o .: "record"
      "delete"  -> MutDelete <$> o .: "key"
      "set_hex" -> MutSetHex <$> o .: "chunk" <*> o .: "tile" <*> o .: "record"
      _         -> fail ("unknown mutation type: " <> Text.unpack ty)

-- | A data mutation request sent from host to plugin.
data MutateResource = MutateResource
  { mrResource :: !Text
    -- ^ Target resource name.
  , mrMutation :: !DataMutation
    -- ^ Mutation discriminator.
  } deriving (Eq, Show, Generic)

instance ToJSON MutateResource where
  toJSON mr = object
    [ "resource" .= mrResource mr
    , "mutation" .= mrMutation mr
    ]

instance FromJSON MutateResource where
  parseJSON = withObject "MutateResource" $ \o ->
    MutateResource
      <$> o .: "resource"
      <*> o .: "mutation"

-- | Result of a data mutation returned from plugin to host.
data MutateResult = MutateResult
  { mrsSuccess :: !Bool
    -- ^ Whether the mutation succeeded.
  , mrsError   :: !(Maybe Text)
    -- ^ Error message if the mutation failed.
  , mrsRecord  :: !(Maybe DataRecord)
    -- ^ The created or updated record (if applicable).
  , mrsErrorCode :: !(Maybe DataResourceErrorCode)
    -- ^ Stable error code for failed mutations when the plugin can classify it.
  } deriving (Eq, Show, Generic)

instance ToJSON MutateResult where
  toJSON mr = object $
    [ "success" .= mrsSuccess mr
    ] <>
    [ "error"      .= e    | Just e <- [mrsError mr] ] <>
    [ "record"     .= r    | Just r <- [mrsRecord mr] ] <>
    [ "error_code" .= code | Just code <- [mrsErrorCode mr] ]

instance FromJSON MutateResult where
  parseJSON = withObject "MutateResult" $ \o -> do
    success <- o .: "success"
    err <- o .:? "error"
    record <- o .:? "record"
    mErrorCodeValue <- o .:? "error_code"
    pure MutateResult
      { mrsSuccess = success
      , mrsError = err
      , mrsRecord = record
      , mrsErrorCode = mErrorCodeValue >>= dataResourceErrorCodeFromValue
      }
