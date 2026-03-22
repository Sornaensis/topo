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
-- __Scalars:__  @text@, @int@, @float@, @double@, @bool@, @fixed2@,
-- @fixed3@, @fixed4@.
--
-- __Composites:__
--
-- * @enum@ — a fixed set of string choices (e.g. faction alignment).
-- * @record@ — a nested product type with named fields (recursive).
-- * @adt@ — a sum type where each constructor carries positional
--   arguments (recursive).
--
-- Scalar types serialise as plain JSON strings.  Composite types
-- serialise as JSON objects with a single discriminator key
-- (@\"enum\"@, @\"record\"@, or @\"adt\"@).
--
-- = Overlay-backed resources
--
-- A resource may declare @drsOverlay = Just overlayName@ to indicate
-- that its data is stored in the named overlay.  The host stores the
-- raw overlay data; all queries and mutations are forwarded to the
-- plugin, which owns interpretation.
module Topo.Plugin.DataResource
  ( -- * Resource schema
    DataResourceSchema(..)
    -- * Field definitions
  , DataFieldDef(..)
  , DataFieldType(..)
  , DataConstructorDef(..)
    -- ** Field-type helpers
  , dataFieldTypeName
  , parseScalarFieldType
  , fixedDecimalPlaces
  , isScalarType
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
  )
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

------------------------------------------------------------------------
-- Field types
------------------------------------------------------------------------

-- | Types permitted in data-resource fields.
--
-- Scalar constructors carry no payload and serialise as a plain JSON
-- string (@\"text\"@, @\"int\"@, …).  Composite constructors carry
-- structural information and serialise as a single-key JSON object.
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
  | DFEnum ![Text]
  -- ^ One of a fixed set of string choices.
  | DFRecord ![DataFieldDef]
  -- ^ A nested product type with named fields (mutually recursive
  --   with 'DataFieldDef').
  | DFAdt ![DataConstructorDef]
  -- ^ A sum type.  Each constructor carries positional arguments
  --   whose types may themselves be composite (recursive).
  deriving (Eq, Show, Generic)

-- | A single constructor in a 'DFAdt' sum type.
--
-- @
-- { "constructor": "Circle",  "fields": ["float"] }
-- { "constructor": "Pair",    "fields": [{"record": [...]}, "int"] }
-- @
data DataConstructorDef = DataConstructorDef
  { dcdName   :: !Text
  -- ^ Constructor tag (e.g. @\"Circle\"@, @\"Rectangle\"@).
  , dcdFields :: ![DataFieldType]
  -- ^ Positional argument types (may be empty for nullary constructors).
  } deriving (Eq, Show, Generic)

instance ToJSON DataConstructorDef where
  toJSON dcd = object
    [ "constructor" .= dcdName dcd
    , "fields"      .= dcdFields dcd
    ]

instance FromJSON DataConstructorDef where
  parseJSON = withObject "DataConstructorDef" $ \o ->
    DataConstructorDef
      <$> o .: "constructor"
      <*> (o .:? "fields" >>= pure . maybe [] id)

------------------------------------------------------------------------
-- Field-type helpers
------------------------------------------------------------------------

-- | Human-readable category name for any field type.
--
-- Scalar types return their canonical tag (@\"text\"@, @\"int\"@, …).
-- Composite types return @\"enum\"@, @\"record\"@, or @\"adt\"@.
dataFieldTypeName :: DataFieldType -> Text
dataFieldTypeName DFText       = "text"
dataFieldTypeName DFInt        = "int"
dataFieldTypeName DFFloat      = "float"
dataFieldTypeName DFDouble     = "double"
dataFieldTypeName DFBool       = "bool"
dataFieldTypeName DFFixed2     = "fixed2"
dataFieldTypeName DFFixed3     = "fixed3"
dataFieldTypeName DFFixed4     = "fixed4"
dataFieldTypeName (DFEnum _)   = "enum"
dataFieldTypeName (DFRecord _) = "record"
dataFieldTypeName (DFAdt _)    = "adt"

-- | Parse a /scalar/ field type from its canonical tag.
--
-- Returns 'Nothing' for composite type names (@\"enum\"@, @\"record\"@,
-- @\"adt\"@) since those require structured JSON — use the 'FromJSON'
-- instance on 'DataFieldType' instead.
parseScalarFieldType :: Text -> Maybe DataFieldType
parseScalarFieldType "text"   = Just DFText
parseScalarFieldType "int"    = Just DFInt
parseScalarFieldType "float"  = Just DFFloat
parseScalarFieldType "double" = Just DFDouble
parseScalarFieldType "bool"   = Just DFBool
parseScalarFieldType "fixed2" = Just DFFixed2
parseScalarFieldType "fixed3" = Just DFFixed3
parseScalarFieldType "fixed4" = Just DFFixed4
parseScalarFieldType _        = Nothing

-- | Whether the field type is a scalar (non-composite).
isScalarType :: DataFieldType -> Bool
isScalarType (DFEnum _)   = False
isScalarType (DFRecord _) = False
isScalarType (DFAdt _)    = False
isScalarType _            = True

-- | Number of fractional decimal digits for fixed-point types.
--
-- Returns 'Nothing' for non-fixed types.
fixedDecimalPlaces :: DataFieldType -> Maybe Int
fixedDecimalPlaces DFFixed2 = Just 2
fixedDecimalPlaces DFFixed3 = Just 3
fixedDecimalPlaces DFFixed4 = Just 4
fixedDecimalPlaces _        = Nothing

------------------------------------------------------------------------
-- Field-type JSON instances
------------------------------------------------------------------------

-- Scalars serialise as plain strings:
--   "text", "int", "float", "double", "bool", "fixed2", "fixed3", "fixed4"
--
-- Composites serialise as single-key objects:
--   { "enum":   ["a","b","c"] }
--   { "record": [ {field}, {field}, … ] }
--   { "adt":    [ {constructor}, {constructor}, … ] }

instance ToJSON DataFieldType where
  toJSON DFText   = "text"
  toJSON DFInt    = "int"
  toJSON DFFloat  = "float"
  toJSON DFDouble = "double"
  toJSON DFBool   = "bool"
  toJSON DFFixed2 = "fixed2"
  toJSON DFFixed3 = "fixed3"
  toJSON DFFixed4 = "fixed4"
  toJSON (DFEnum choices)  = object ["enum"   .= choices]
  toJSON (DFRecord fields) = object ["record" .= fields]
  toJSON (DFAdt ctors)     = object ["adt"    .= ctors]

instance FromJSON DataFieldType where
  parseJSON (String t) =
    case parseScalarFieldType t of
      Just ft -> pure ft
      Nothing -> fail ("unknown scalar data field type: " <> Text.unpack t)
  parseJSON val = flip (withObject "DataFieldType") val $ \o -> do
    mEnum   <- o .:? "enum"
    mRecord <- o .:? "record"
    mAdt    <- o .:? "adt"
    case ( mEnum   :: Maybe [Text]
         , mRecord :: Maybe [DataFieldDef]
         , mAdt    :: Maybe [DataConstructorDef]
         ) of
      (Just choices, Nothing, Nothing) -> pure (DFEnum choices)
      (Nothing, Just fields, Nothing)  -> pure (DFRecord fields)
      (Nothing, Nothing, Just ctors)   -> pure (DFAdt ctors)
      _ -> fail "data field type object must contain exactly one of: enum, record, adt"

------------------------------------------------------------------------
-- Field definitions
------------------------------------------------------------------------

-- | One field in a data-resource record.
data DataFieldDef = DataFieldDef
  { dfName     :: !Text
  -- ^ Unique field name within the resource.
  , dfType     :: !DataFieldType
  -- ^ Type of this field (scalar or composite).
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
--
-- When 'drsOverlay' is @Just overlayName@, the resource's data is
-- stored in the named overlay.  The host persists the raw overlay; all
-- queries and mutations are forwarded to the plugin, which owns
-- interpretation.
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
  , drsOverlay    :: !(Maybe Text)
  -- ^ If @Just name@, data is backed by the named overlay.
  --   The plugin owns storage and answers all queries; the host only
  --   persists the raw overlay blob alongside the world save.
  } deriving (Eq, Show, Generic)

instance ToJSON DataResourceSchema where
  toJSON drs = object $
    [ "name"       .= drsName drs
    , "label"      .= drsLabel drs
    , "hexBound"   .= drsHexBound drs
    , "fields"     .= drsFields drs
    , "operations" .= drsOperations drs
    , "keyField"   .= drsKeyField drs
    ] <>
    [ "overlay" .= ov | Just ov <- [drsOverlay drs] ]

instance FromJSON DataResourceSchema where
  parseJSON = withObject "DataResourceSchema" $ \o ->
    DataResourceSchema
      <$> o .: "name"
      <*> o .: "label"
      <*> (o .:? "hexBound" >>= pure . maybe False id)
      <*> o .: "fields"
      <*> o .: "operations"
      <*> o .: "keyField"
      <*> o .:? "overlay"

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
  | DREOverlayNotHexBound
  -- ^ Overlay-backed resource must be hex-bound.
  | DREEmptyEnum !Text
  -- ^ An enum field has no choices.  Carries the field name.
  | DRENullaryAdt !Text
  -- ^ An ADT field has no constructors.  Carries the field name.
  | DREEmptyConstructorName !Text
  -- ^ An ADT constructor has an empty name.  Carries the field name.
  | DREDuplicateConstructor !Text !Text
  -- ^ Duplicate constructor name within an ADT.
  --   Carries (field name, constructor name).
  deriving (Eq, Show)

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
  , [ DREOverlayNotHexBound
    | Just _ <- [drsOverlay drs]
    , not (drsHexBound drs)
    ]
  , duplicateFieldErrors
  , concatMap validateFieldType (drsFields drs)
  ]
  where
    fieldNames = map dfName (drsFields drs)
    duplicateFieldErrors =
      [ DREDuplicateField n
      | n <- unique (filter (\n -> length (filter (== n) fieldNames) > 1) fieldNames)
      ]
    unique [] = []
    unique (x:xs) = x : unique (filter (/= x) xs)

-- | Validate composite field types within a field definition.
validateFieldType :: DataFieldDef -> [DataResourceError]
validateFieldType fd = case dfType fd of
  DFEnum choices
    | null choices -> [DREEmptyEnum (dfName fd)]
    | otherwise    -> []
  DFAdt ctors
    | null ctors   -> [DRENullaryAdt (dfName fd)]
    | otherwise    -> concat
        [ [ DREEmptyConstructorName (dfName fd)
          | Text.null (dcdName c)
          ]
        | c <- ctors
        ]
        <>
        [ DREDuplicateConstructor (dfName fd) n
        | n <- unique (filter (\n -> length (filter (== n) cnames) > 1) cnames)
        ]
    where
      cnames = map dcdName ctors
      unique [] = []
      unique (x:xs) = x : unique (filter (/= x) xs)
  DFRecord subFields ->
    concatMap validateFieldType subFields
  _ -> []
