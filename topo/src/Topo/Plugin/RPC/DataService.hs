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
  ) where

import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Value(..)
  , (.:)
  , (.:?)
  , (.=)
  , object
  , withObject
  )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as Aeson (Parser)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

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
  } deriving (Eq, Show, Generic)

instance ToJSON MutateResult where
  toJSON mr = object $
    [ "success" .= mrsSuccess mr
    ] <>
    [ "error"  .= e | Just e <- [mrsError mr] ] <>
    [ "record" .= r | Just r <- [mrsRecord mr] ]

instance FromJSON MutateResult where
  parseJSON = withObject "MutateResult" $ \o ->
    MutateResult
      <$> o .:  "success"
      <*> o .:? "error"
      <*> o .:? "record"
