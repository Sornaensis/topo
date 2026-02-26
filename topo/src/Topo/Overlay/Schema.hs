{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Overlay schema definitions and JSON parsing.
--
-- An overlay schema declares the typed record structure for a geographic
-- data layer over the hex grid.  Schemas are stored as @.toposchema@ JSON
-- files alongside the overlay binary data in the @.topolay/@ directory.
--
-- = Storage modes
--
-- * __Sparse__ ('StorageSparse'): boxed array-of-structs in 'IntMap'.
--   Good for partially-populated overlays (e.g. civilisation — only
--   inhabited tiles carry data).
--
-- * __Dense__ ('StorageDense'): one unboxed @U.Vector Float@ per field per
--   chunk (struct-of-arrays).  Requires all-numeric fields
--   ('OFFloat', 'OFInt', 'OFBool' — bools as 0.0\/1.0).  Intended for
--   full-coverage overlays like weather.
module Topo.Overlay.Schema
  ( -- * Field types
    OverlayFieldType(..)
  , overlayFieldTypeName
  , parseOverlayFieldType
    -- * Field definitions
  , OverlayFieldDef(..)
    -- * Storage mode
  , OverlayStorage(..)
    -- * Dependencies
  , OverlayDeps(..)
  , emptyOverlayDeps
    -- * Schema
  , OverlaySchema(..)
  , parseOverlaySchema
  , encodeOverlaySchema
  , validateSchema
  , SchemaError(..)
    -- * Field-index lookup
  , fieldIndex
  , fieldIndices
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
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

------------------------------------------------------------------------
-- Field types
------------------------------------------------------------------------

-- | Scalar types allowed in overlay records.
data OverlayFieldType
  = OFFloat
  -- ^ 32-bit IEEE 754 float.
  | OFInt
  -- ^ Machine-word signed integer.
  | OFBool
  -- ^ Boolean (stored as 0.0\/1.0 in dense mode).
  | OFText
  -- ^ Length-prefixed UTF-8 text.  Not permitted in dense overlays.
  deriving (Eq, Ord, Show, Read, Generic)

-- | Canonical lowercase name for serialisation.
overlayFieldTypeName :: OverlayFieldType -> Text
overlayFieldTypeName OFFloat = "float"
overlayFieldTypeName OFInt   = "int"
overlayFieldTypeName OFBool  = "bool"
overlayFieldTypeName OFText  = "text"

-- | Parse a field type from its canonical name.
parseOverlayFieldType :: Text -> Maybe OverlayFieldType
parseOverlayFieldType "float" = Just OFFloat
parseOverlayFieldType "int"   = Just OFInt
parseOverlayFieldType "bool"  = Just OFBool
parseOverlayFieldType "text"  = Just OFText
parseOverlayFieldType _       = Nothing

instance ToJSON OverlayFieldType where
  toJSON = toJSON . overlayFieldTypeName

instance FromJSON OverlayFieldType where
  parseJSON = withText "OverlayFieldType" $ \t ->
    case parseOverlayFieldType t of
      Just ft -> pure ft
      Nothing -> fail ("unknown overlay field type: " <> Text.unpack t)

------------------------------------------------------------------------
-- Field definitions
------------------------------------------------------------------------

-- | One field in an overlay record.
data OverlayFieldDef = OverlayFieldDef
  { ofdName        :: !Text
  -- ^ Unique field name within the schema.
  , ofdType        :: !OverlayFieldType
  -- ^ Scalar type of this field.
  , ofdDefault     :: !Value
  -- ^ JSON-encoded default value for new/missing records.
  , ofdIndexed     :: !Bool
  -- ^ Build a secondary index on load?  See 'Topo.Overlay.Index'.
  , ofdRenamedFrom :: !(Maybe Text)
  -- ^ Previous field name, for schema migration (@renamed_from@).
  } deriving (Eq, Show, Generic)

instance ToJSON OverlayFieldDef where
  toJSON fd = object $
    [ "name"    .= ofdName fd
    , "type"    .= ofdType fd
    , "default" .= ofdDefault fd
    ] ++
    [ "indexed"      .= True       | ofdIndexed fd ] ++
    [ "renamed_from" .= n          | Just n <- [ofdRenamedFrom fd] ]

instance FromJSON OverlayFieldDef where
  parseJSON = withObject "OverlayFieldDef" $ \o -> do
    name     <- o .:  "name"
    typ      <- o .:  "type"
    def'     <- o .:  "default"
    indexed  <- o .:? "indexed"     >>= pure . maybe False id
    renamed  <- o .:? "renamed_from"
    pure OverlayFieldDef
      { ofdName        = name
      , ofdType        = typ
      , ofdDefault     = def'
      , ofdIndexed     = indexed
      , ofdRenamedFrom = renamed
      }

------------------------------------------------------------------------
-- Storage mode
------------------------------------------------------------------------

-- | How overlay data is laid out on disk and in memory.
data OverlayStorage
  = StorageSparse
  -- ^ AoS: boxed records in 'IntMap'.  Supports all field types.
  | StorageDense
  -- ^ SoA: one unboxed @U.Vector Float@ per field per chunk.
  --   Requires all fields to be numeric ('OFFloat', 'OFInt', 'OFBool').
  deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON OverlayStorage where
  toJSON StorageSparse = toJSON ("sparse" :: Text)
  toJSON StorageDense  = toJSON ("dense"  :: Text)

instance FromJSON OverlayStorage where
  parseJSON = withText "OverlayStorage" $ \t ->
    case t of
      "sparse" -> pure StorageSparse
      "dense"  -> pure StorageDense
      _        -> fail ("unknown storage mode: " <> Text.unpack t)

------------------------------------------------------------------------
-- Dependencies
------------------------------------------------------------------------

-- | Declares what data an overlay's simulation node reads.
data OverlayDeps = OverlayDeps
  { odTerrain  :: !Bool
  -- ^ Does this overlay read terrain data?
  , odOverlays :: ![Text]
  -- ^ Names of other overlays this one depends on.
  } deriving (Eq, Show, Generic)

-- | No dependencies.
emptyOverlayDeps :: OverlayDeps
emptyOverlayDeps = OverlayDeps
  { odTerrain  = False
  , odOverlays = []
  }

instance ToJSON OverlayDeps where
  toJSON od = object
    [ "terrain"  .= odTerrain od
    , "overlays" .= odOverlays od
    ]

instance FromJSON OverlayDeps where
  parseJSON = withObject "OverlayDeps" $ \o ->
    OverlayDeps
      <$> o .:? "terrain"  .!= False
      <*> o .:? "overlays" .!= []
    where
      (.!=) p d = fmap (maybe d id) p

------------------------------------------------------------------------
-- Schema
------------------------------------------------------------------------

-- | Complete overlay schema: type/version metadata, field definitions,
-- storage mode, and dependency declarations.
data OverlaySchema = OverlaySchema
  { osName         :: !Text
  -- ^ Unique overlay name (e.g. @\"weather\"@, @\"civilization\"@).
  , osVersion      :: !Text
  -- ^ Semver-style version string for schema evolution.
  , osDescription  :: !Text
  -- ^ Human-readable description.
  , osFields       :: ![OverlayFieldDef]
  -- ^ Ordered list of record fields.
  , osStorage      :: !OverlayStorage
  -- ^ Sparse (AoS) or Dense (SoA) layout.
  , osDependencies :: !OverlayDeps
  -- ^ What this overlay reads at simulation time.
  , osFieldIndex   :: !(Map Text Int)
  -- ^ Cached field-name → positional index.  Not serialised;
  -- rebuilt by 'parseOverlaySchema' and 'validateSchema'.
  } deriving (Eq, Show, Generic)

instance ToJSON OverlaySchema where
  toJSON os = object
    [ "name"         .= osName os
    , "version"      .= osVersion os
    , "description"  .= osDescription os
    , "storage"      .= osStorage os
    , "fields"       .= osFields os
    , "dependencies" .= osDependencies os
    ]

instance FromJSON OverlaySchema where
  parseJSON = withObject "OverlaySchema" $ \o -> do
    name    <- o .:  "name"
    ver     <- o .:  "version"
    desc    <- o .:? "description" >>= pure . maybe "" id
    storage <- o .:? "storage"     >>= pure . maybe StorageSparse id
    fields  <- o .:  "fields"
    deps    <- o .:? "dependencies" >>= pure . maybe emptyOverlayDeps id
    let idx = buildFieldIndex fields
    pure OverlaySchema
      { osName         = name
      , osVersion      = ver
      , osDescription  = desc
      , osFields       = fields
      , osStorage      = storage
      , osDependencies = deps
      , osFieldIndex   = idx
      }

-- | Parse a @.toposchema@ JSON file from raw bytes.
parseOverlaySchema :: BS.ByteString -> Either Text OverlaySchema
parseOverlaySchema bytes =
  case Aeson.eitherDecodeStrict' bytes of
    Left err -> Left (Text.pack err)
    Right schema ->
      case validateSchema schema of
        [] -> Right schema
        errs -> Left (Text.intercalate "; " (map renderSchemaError errs))

-- | Encode an 'OverlaySchema' to JSON bytes (pretty-ish).
encodeOverlaySchema :: OverlaySchema -> BS.ByteString
encodeOverlaySchema = BL.toStrict . Aeson.encode

------------------------------------------------------------------------
-- Validation
------------------------------------------------------------------------

-- | Errors detected by 'validateSchema'.
data SchemaError
  = DuplicateFieldName !Text
  -- ^ Two fields share the same name.
  | DenseTextFieldDisallowed !Text
  -- ^ A dense overlay may not contain text fields.
  | EmptySchemaName
  -- ^ Schema name is empty.
  | EmptySchemaVersion
  -- ^ Schema version is empty.
  | NoFields
  -- ^ Schema declares zero fields.
  deriving (Eq, Show)

-- | Render a 'SchemaError' for display.
renderSchemaError :: SchemaError -> Text
renderSchemaError (DuplicateFieldName n)       = "duplicate field name: " <> n
renderSchemaError (DenseTextFieldDisallowed n)  = "dense overlay may not have text field: " <> n
renderSchemaError EmptySchemaName               = "schema name is empty"
renderSchemaError EmptySchemaVersion            = "schema version is empty"
renderSchemaError NoFields                      = "schema has no fields"

-- | Validate an 'OverlaySchema', returning all detected errors.
validateSchema :: OverlaySchema -> [SchemaError]
validateSchema schema = concat
  [ [ EmptySchemaName    | Text.null (osName schema) ]
  , [ EmptySchemaVersion | Text.null (osVersion schema) ]
  , [ NoFields           | null (osFields schema) ]
  , duplicateErrors
  , denseTextErrors
  ]
  where
    names = map ofdName (osFields schema)
    duplicateErrors =
      let counts = Map.fromListWith (+) [ (n, 1 :: Int) | n <- names ]
      in  [ DuplicateFieldName n | (n, c) <- Map.toList counts, c > 1 ]
    denseTextErrors
      | osStorage schema == StorageDense =
          [ DenseTextFieldDisallowed (ofdName f)
          | f <- osFields schema
          , ofdType f == OFText
          ]
      | otherwise = []

------------------------------------------------------------------------
-- Field-index helpers
------------------------------------------------------------------------

-- | Build the field-name → index map from an ordered field list.
buildFieldIndex :: [OverlayFieldDef] -> Map Text Int
buildFieldIndex fields =
  Map.fromList [ (ofdName f, i) | (i, f) <- zip [0..] fields ]

-- | Look up a field's positional index in a schema.
--
-- Returns 'Nothing' if the field name is not declared.
fieldIndex :: OverlaySchema -> Text -> Maybe Int
fieldIndex schema name = Map.lookup name (osFieldIndex schema)

-- | Return the complete field-name → index map.
fieldIndices :: OverlaySchema -> Map Text Int
fieldIndices = osFieldIndex
