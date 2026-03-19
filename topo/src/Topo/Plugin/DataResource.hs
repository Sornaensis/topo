{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Plugin data resource schema types.
--
-- Plugins that manage their own datastores (databases, files, etc.)
-- declare the shape of their data through 'DataResourceSchema'.  The
-- host uses these schemas to auto-generate browsing\/editing UI and to
-- validate CRUD operations.
--
-- = Field types
--
-- Beyond the four basic scalars (@text@, @int@, @float@, @bool@), this
-- module exposes fixed-point types (@fixed2@, @fixed3@, @fixed4@) for
-- precise decimal values (e.g. currency, coordinates) and @double@ for
-- 64-bit IEEE 754 quantities that need more range or precision than
-- @float@.
--
-- = Serialisation
--
-- All types round-trip through JSON via 'ToJSON' \/ 'FromJSON'.
-- Field-type tags are lowercase strings: @\"text\"@, @\"int\"@,
-- @\"float\"@, @\"double\"@, @\"bool\"@, @\"fixed2\"@, @\"fixed3\"@,
-- @\"fixed4\"@.
module Topo.Plugin.DataResource
  ( -- * Resource schema
    DataResourceSchema(..)
    -- * Field definitions
  , DataFieldDef(..)
  , DataFieldType(..)
    -- ** Field-type helpers
  , dataFieldTypeName
  , parseDataFieldType
  , fixedDecimalPlaces
    -- * CRUD operation flags
  , DataOperations(..)
  , noOperations
  , allOperations
    -- * Validation
  , DataResourceError(..)
  , validateDataResource
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
  , withText
  )
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

------------------------------------------------------------------------
-- Field types
------------------------------------------------------------------------

-- | Scalar types permitted in data-resource fields.
data DataFieldType
  = DFText
  -- ^ UTF-8 text.
  | DFInt
  -- ^ Machine-word signed integer.
  | DFFloat
  -- ^ 32-bit IEEE 754 float.
  | DFDouble
  -- ^ 64-bit IEEE 754 double.
  | DFBool
  -- ^ Boolean.
  | DFFixed2
  -- ^ Fixed-point with 2 decimal places (e.g. @12345@ represents @123.45@).
  | DFFixed3
  -- ^ Fixed-point with 3 decimal places (e.g. @12345@ represents @12.345@).
  | DFFixed4
  -- ^ Fixed-point with 4 decimal places (e.g. @12345@ represents @1.2345@).
  deriving (Eq, Ord, Show, Read, Generic)

-- | Canonical lowercase tag used in JSON serialisation.
dataFieldTypeName :: DataFieldType -> Text
dataFieldTypeName DFText   = "text"
dataFieldTypeName DFInt    = "int"
dataFieldTypeName DFFloat  = "float"
dataFieldTypeName DFDouble = "double"
dataFieldTypeName DFBool   = "bool"
dataFieldTypeName DFFixed2 = "fixed2"
dataFieldTypeName DFFixed3 = "fixed3"
dataFieldTypeName DFFixed4 = "fixed4"

-- | Parse a field type from its canonical tag.
parseDataFieldType :: Text -> Maybe DataFieldType
parseDataFieldType "text"   = Just DFText
parseDataFieldType "int"    = Just DFInt
parseDataFieldType "float"  = Just DFFloat
parseDataFieldType "double" = Just DFDouble
parseDataFieldType "bool"   = Just DFBool
parseDataFieldType "fixed2" = Just DFFixed2
parseDataFieldType "fixed3" = Just DFFixed3
parseDataFieldType "fixed4" = Just DFFixed4
parseDataFieldType _        = Nothing

-- | Number of fractional decimal digits for fixed-point types.
--
-- Returns 'Nothing' for non-fixed types.
fixedDecimalPlaces :: DataFieldType -> Maybe Int
fixedDecimalPlaces DFFixed2 = Just 2
fixedDecimalPlaces DFFixed3 = Just 3
fixedDecimalPlaces DFFixed4 = Just 4
fixedDecimalPlaces _        = Nothing

instance ToJSON DataFieldType where
  toJSON = toJSON . dataFieldTypeName

instance FromJSON DataFieldType where
  parseJSON = withText "DataFieldType" $ \t ->
    case parseDataFieldType t of
      Just ft -> pure ft
      Nothing -> fail ("unknown data field type: " <> Text.unpack t)

------------------------------------------------------------------------
-- Field definitions
------------------------------------------------------------------------

-- | One field in a data-resource record.
data DataFieldDef = DataFieldDef
  { dfName     :: !Text
  -- ^ Unique field name within the resource.
  , dfType     :: !DataFieldType
  -- ^ Scalar type of this field.
  , dfLabel    :: !Text
  -- ^ Human-readable display label.
  , dfEditable :: !Bool
  -- ^ Whether the host UI should allow editing this field.
  , dfDefault  :: !(Maybe Value)
  -- ^ Default value for new records (JSON-encoded).
  } deriving (Eq, Show, Generic)

instance ToJSON DataFieldDef where
  toJSON fd = object $
    [ "name"  .= dfName fd
    , "type"  .= dfType fd
    , "label" .= dfLabel fd
    ] <>
    [ "editable" .= True        | dfEditable fd ] <>
    [ "default"  .= v           | Just v <- [dfDefault fd] ]

instance FromJSON DataFieldDef where
  parseJSON = withObject "DataFieldDef" $ \o ->
    DataFieldDef
      <$> o .:  "name"
      <*> o .:  "type"
      <*> o .:  "label"
      <*> (o .:? "editable" >>= pure . maybe False id)
      <*> o .:? "default"

------------------------------------------------------------------------
-- CRUD operation flags
------------------------------------------------------------------------

-- | Which CRUD operations a data resource supports.
data DataOperations = DataOperations
  { doList       :: !Bool
  -- ^ List / query all records.
  , doGet        :: !Bool
  -- ^ Get a single record by primary key.
  , doCreate     :: !Bool
  -- ^ Create a new record.
  , doUpdate     :: !Bool
  -- ^ Update an existing record.
  , doDelete     :: !Bool
  -- ^ Delete a record by primary key.
  , doQueryByHex :: !Bool
  -- ^ Query records associated with a hex coordinate.
  } deriving (Eq, Show, Generic)

-- | No operations enabled.
noOperations :: DataOperations
noOperations = DataOperations
  { doList       = False
  , doGet        = False
  , doCreate     = False
  , doUpdate     = False
  , doDelete     = False
  , doQueryByHex = False
  }

-- | All operations enabled.
allOperations :: DataOperations
allOperations = DataOperations
  { doList       = True
  , doGet        = True
  , doCreate     = True
  , doUpdate     = True
  , doDelete     = True
  , doQueryByHex = True
  }

instance ToJSON DataOperations where
  toJSON ops = object
    [ "list"        .= doList ops
    , "get"         .= doGet ops
    , "create"      .= doCreate ops
    , "update"      .= doUpdate ops
    , "delete"      .= doDelete ops
    , "queryByHex"  .= doQueryByHex ops
    ]

instance FromJSON DataOperations where
  parseJSON = withObject "DataOperations" $ \o ->
    DataOperations
      <$> (o .:? "list"       >>= pure . maybe False id)
      <*> (o .:? "get"        >>= pure . maybe False id)
      <*> (o .:? "create"     >>= pure . maybe False id)
      <*> (o .:? "update"     >>= pure . maybe False id)
      <*> (o .:? "delete"     >>= pure . maybe False id)
      <*> (o .:? "queryByHex" >>= pure . maybe False id)

------------------------------------------------------------------------
-- Resource schema
------------------------------------------------------------------------

-- | Schema for a plugin-declared data resource.
--
-- Plugins advertise data resources in their manifest and refine them
-- at runtime via the handshake acknowledgement.  The host uses these
-- schemas to generate browsing\/editing UI and validate CRUD messages.
data DataResourceSchema = DataResourceSchema
  { drsName       :: !Text
  -- ^ Machine-readable resource name (e.g. @\"cultures\"@, @\"settlements\"@).
  , drsLabel      :: !Text
  -- ^ Human-readable display name.
  , drsHexBound   :: !Bool
  -- ^ Whether records can be queried by hex coordinate.
  , drsFields     :: ![DataFieldDef]
  -- ^ Ordered list of field definitions.
  , drsOperations :: !DataOperations
  -- ^ Which CRUD operations this resource supports.
  , drsKeyField   :: !Text
  -- ^ Name of the primary-key field (must appear in 'drsFields').
  } deriving (Eq, Show, Generic)

instance ToJSON DataResourceSchema where
  toJSON drs = object
    [ "name"       .= drsName drs
    , "label"      .= drsLabel drs
    , "hexBound"   .= drsHexBound drs
    , "fields"     .= drsFields drs
    , "operations" .= drsOperations drs
    , "keyField"   .= drsKeyField drs
    ]

instance FromJSON DataResourceSchema where
  parseJSON = withObject "DataResourceSchema" $ \o ->
    DataResourceSchema
      <$> o .: "name"
      <*> o .: "label"
      <*> (o .:? "hexBound" >>= pure . maybe False id)
      <*> o .: "fields"
      <*> o .: "operations"
      <*> o .: "keyField"

------------------------------------------------------------------------
-- Validation
------------------------------------------------------------------------

-- | Errors detected by 'validateDataResource'.
data DataResourceError
  = DREEmptyName
  -- ^ Resource name is empty.
  | DREEmptyLabel
  -- ^ Display label is empty.
  | DRENoFields
  -- ^ No fields declared.
  | DREKeyFieldMissing !Text
  -- ^ The declared key field does not appear in the fields list.
  | DREQueryByHexNotHexBound
  -- ^ 'doQueryByHex' is enabled but 'drsHexBound' is 'False'.
  | DREDuplicateField !Text
  -- ^ Two fields share the same name.
  deriving (Eq, Ord, Show, Read)

-- | Validate a data resource schema, returning all detected errors.
validateDataResource :: DataResourceSchema -> [DataResourceError]
validateDataResource drs = concat
  [ [ DREEmptyName  | Text.null (drsName drs) ]
  , [ DREEmptyLabel | Text.null (drsLabel drs) ]
  , [ DRENoFields   | null (drsFields drs) ]
  , [ DREKeyFieldMissing (drsKeyField drs)
    | not (any (\f -> dfName f == drsKeyField drs) (drsFields drs))
    ]
  , [ DREQueryByHexNotHexBound
    | doQueryByHex (drsOperations drs)
    , not (drsHexBound drs)
    ]
  , duplicateFieldErrors
  ]
  where
    fieldNames = map dfName (drsFields drs)
    duplicateFieldErrors =
      [ DREDuplicateField n
      | n <- unique (filter (\n -> length (filter (== n) fieldNames) > 1) fieldNames)
      ]
    unique [] = []
    unique (x:xs) = x : unique (filter (/= x) xs)
