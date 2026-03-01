{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | In-memory overlay data representation.
--
-- An overlay is a user-extensible geographic data layer on the hex grid.
-- Each overlay type is described by an 'OverlaySchema' and stores
-- per-hex records in one of two layouts:
--
-- * __Sparse__ ('SparseData'): boxed array-of-structs ('OverlayRecord')
--   in per-chunk 'IntMap's.  Unpopulated hexes are not stored; queries
--   return the schema-declared default.
--
-- * __Dense__ ('DenseData'): struct-of-arrays with one unboxed
--   @U.Vector Float@ per field per chunk.  Every hex in every chunk is
--   populated.  Only numeric field types are permitted.
module Topo.Overlay
  ( -- * Field values
    OverlayValue(..)
  , overlayValueToFloat
  , floatToOverlayValue
  , defaultValue
  , matchesFieldType
    -- * Records (sparse)
  , OverlayRecord(..)
  , mkOverlayRecord
  , mkOverlayRecordUnchecked
  , recordField
  , setRecordField
  , setRecordFieldChecked
  , defaultRecord
    -- * Per-chunk data (sparse)
  , OverlayChunk(..)
  , emptyOverlayChunk
  , chunkLookup
  , chunkInsert
  , chunkDelete
  , chunkSize
    -- * Storage-mode–specific payload
  , OverlayData(..)
    -- * Full overlay
  , Overlay(..)
  , OverlayProvenance(..)
  , emptyOverlayProvenance
  , emptyOverlay
  , overlayName
    -- * Overlay store
  , OverlayStore(..)
  , emptyOverlayStore
  , lookupOverlay
  , insertOverlay
  , deleteOverlay
  , overlayNames
  , overlayCount
  ) where

import Data.Aeson (Value(..))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import GHC.Generics (Generic)

import Topo.Overlay.Provenance (OverlayProvenance(..), emptyOverlayProvenance)
import Topo.Overlay.Schema
  ( OverlayFieldDef(..)
  , OverlayFieldType(..)
  , OverlaySchema(..)
  , OverlayStorage(..)
  , overlayFieldTypeName
  )

------------------------------------------------------------------------
-- Field values
------------------------------------------------------------------------

-- | A single typed field value in a sparse overlay record.
data OverlayValue
  = OVFloat  !Float
  | OVInt    !Int
  | OVBool   !Bool
  | OVText   !Text
  deriving (Eq, Show, Generic)

-- | Convert an 'OverlayValue' to a 'Float' for dense storage.
--
-- * 'OVFloat' → identity
-- * 'OVInt'   → 'fromIntegral'
-- * 'OVBool'  → 1.0 / 0.0
-- * 'OVText'  → always 0.0 (text is not representable in dense mode)
overlayValueToFloat :: OverlayValue -> Float
overlayValueToFloat (OVFloat f) = f
overlayValueToFloat (OVInt   i) = fromIntegral i
overlayValueToFloat (OVBool  b) = if b then 1.0 else 0.0
overlayValueToFloat (OVText  _) = 0.0

-- | Convert a 'Float' back to an 'OverlayValue' given the target field type.
floatToOverlayValue :: OverlayFieldType -> Float -> OverlayValue
floatToOverlayValue OFFloat f = OVFloat f
floatToOverlayValue OFInt   f = OVInt (round f)
floatToOverlayValue OFBool  f = OVBool (f >= 0.5)
floatToOverlayValue OFText  _ = OVText ""

-- | Produce a default 'OverlayValue' from a field definition's JSON default.
--
-- Falls back to a sensible zero if the JSON value doesn't match the
-- declared type.
defaultValue :: OverlayFieldDef -> OverlayValue
defaultValue fd = case ofdType fd of
  OFFloat -> OVFloat (jsonToFloat (ofdDefault fd))
  OFInt   -> OVInt   (jsonToInt   (ofdDefault fd))
  OFBool  -> OVBool  (jsonToBool  (ofdDefault fd))
  OFText  -> OVText  (jsonToText  (ofdDefault fd))

jsonToFloat :: Value -> Float
jsonToFloat (Number n) = realToFrac n
jsonToFloat _          = 0.0

jsonToInt :: Value -> Int
jsonToInt (Number n) = round n
jsonToInt _          = 0

jsonToBool :: Value -> Bool
jsonToBool (Bool b) = b
jsonToBool _        = False

jsonToText :: Value -> Text
jsonToText (String t) = t
jsonToText _          = ""

-- | Check whether an 'OverlayValue' matches an 'OverlayFieldType'.
--
-- Returns 'True' when the value constructor agrees with the field type:
--
-- * 'OVFloat' ↔ 'OFFloat'
-- * 'OVInt'   ↔ 'OFInt'
-- * 'OVBool'  ↔ 'OFBool'
-- * 'OVText'  ↔ 'OFText'
matchesFieldType :: OverlayFieldType -> OverlayValue -> Bool
matchesFieldType OFFloat (OVFloat _) = True
matchesFieldType OFInt   (OVInt   _) = True
matchesFieldType OFBool  (OVBool  _) = True
matchesFieldType OFText  (OVText  _) = True
matchesFieldType _       _           = False

------------------------------------------------------------------------
-- Records (sparse)
------------------------------------------------------------------------

-- | One overlay tile record: all fields for one hex, ordered to match
-- the schema's field list.
newtype OverlayRecord = OverlayRecord (Vector OverlayValue)
  deriving (Eq, Show)

-- | Read a field by positional index.  Returns 'Nothing' if out of range.
recordField :: Int -> OverlayRecord -> Maybe OverlayValue
recordField i (OverlayRecord v)
  | i >= 0 && i < V.length v = Just (v V.! i)
  | otherwise                = Nothing

-- | Set a field by positional index.  No-op if out of range.
setRecordField :: Int -> OverlayValue -> OverlayRecord -> OverlayRecord
setRecordField i val (OverlayRecord v)
  | i >= 0 && i < V.length v = OverlayRecord (v V.// [(i, val)])
  | otherwise                = OverlayRecord v

-- | Construct an 'OverlayRecord' validated against a schema.
--
-- Returns 'Left' with a description if:
--
-- * The field count does not match the schema.
-- * Any value's type does not match the corresponding field type.
mkOverlayRecord :: OverlaySchema -> [OverlayValue] -> Either Text OverlayRecord
mkOverlayRecord schema vals
  | length vals /= length fields =
      Left $ "field count mismatch: expected "
          <> showT (length fields)
          <> " but got "
          <> showT (length vals)
  | otherwise =
      case checkFieldTypes fields vals of
        Just err -> Left err
        Nothing  -> Right (OverlayRecord (V.fromList vals))
  where
    fields = osFields schema
    showT = Data.Text.pack . show

-- | Construct an 'OverlayRecord' without validation.
--
-- Use this only in trusted internal paths where the values are known
-- to match the schema (e.g. after binary decode with schema-driven
-- field parsing).
mkOverlayRecordUnchecked :: [OverlayValue] -> OverlayRecord
mkOverlayRecordUnchecked = OverlayRecord . V.fromList

-- | Set a field by positional index with type checking against the schema.
--
-- Returns 'Left' if the index is out of range or the value type does
-- not match the schema's field type at that position.
setRecordFieldChecked
  :: OverlaySchema -> Int -> OverlayValue -> OverlayRecord -> Either Text OverlayRecord
setRecordFieldChecked schema i val (OverlayRecord v)
  | i < 0 || i >= V.length v =
      Left $ "field index out of range: " <> showT i
  | i >= length (osFields schema) =
      Left $ "field index exceeds schema field count: " <> showT i
  | not (matchesFieldType expectedType val) =
      Left $ "type mismatch at field " <> showT i
          <> ": expected " <> overlayFieldTypeName expectedType
          <> " but got " <> valueTypeName val
  | otherwise = Right (OverlayRecord (v V.// [(i, val)]))
  where
    expectedType = ofdType (osFields schema !! i)
    showT = Data.Text.pack . show

-- | Return the type name tag of an 'OverlayValue'.
valueTypeName :: OverlayValue -> Text
valueTypeName (OVFloat _) = "float"
valueTypeName (OVInt   _) = "int"
valueTypeName (OVBool  _) = "bool"
valueTypeName (OVText  _) = "text"

-- | Check field types pairwise, returning 'Just' error on the first mismatch.
checkFieldTypes :: [OverlayFieldDef] -> [OverlayValue] -> Maybe Text
checkFieldTypes [] [] = Nothing
checkFieldTypes (fd:fds) (v:vs)
  | matchesFieldType (ofdType fd) v = checkFieldTypes fds vs
  | otherwise = Just $ "type mismatch for field '" <> ofdName fd
      <> "': expected " <> overlayFieldTypeName (ofdType fd)
      <> " but got " <> valueTypeName v
checkFieldTypes _ _ = Just "field count mismatch (internal error)"

-- | Build a default record from a schema (all fields at their declared defaults).
defaultRecord :: OverlaySchema -> OverlayRecord
defaultRecord schema =
  OverlayRecord (V.fromList (map defaultValue (osFields schema)))

------------------------------------------------------------------------
-- Per-chunk (sparse)
------------------------------------------------------------------------

-- | Per-chunk sparse overlay data, keyed by tile index within the chunk.
newtype OverlayChunk = OverlayChunk (IntMap OverlayRecord)
  deriving (Eq, Show)

-- | Empty chunk with no populated hexes.
emptyOverlayChunk :: OverlayChunk
emptyOverlayChunk = OverlayChunk IntMap.empty

-- | Look up a tile's record in a sparse chunk.
chunkLookup :: Int -> OverlayChunk -> Maybe OverlayRecord
chunkLookup tileIdx (OverlayChunk m) = IntMap.lookup tileIdx m

-- | Insert or replace a tile's record.
chunkInsert :: Int -> OverlayRecord -> OverlayChunk -> OverlayChunk
chunkInsert tileIdx rec (OverlayChunk m) =
  OverlayChunk (IntMap.insert tileIdx rec m)

-- | Remove a tile's record (revert to default).
chunkDelete :: Int -> OverlayChunk -> OverlayChunk
chunkDelete tileIdx (OverlayChunk m) =
  OverlayChunk (IntMap.delete tileIdx m)

-- | Number of populated tiles in a sparse chunk.
chunkSize :: OverlayChunk -> Int
chunkSize (OverlayChunk m) = IntMap.size m

------------------------------------------------------------------------
-- Storage-mode–specific payload
------------------------------------------------------------------------

-- | The overlay data, tagged by storage mode.
data OverlayData
  = SparseData !(IntMap OverlayChunk)
  -- ^ Per-chunk 'IntMap' of per-hex records.
  | DenseData !(IntMap (Vector (U.Vector Float)))
  -- ^ Per-chunk vector of per-field unboxed float arrays (SoA).
  --   Outer 'IntMap' keyed by chunk ID; 'Vector' indexed by field
  --   position; each 'U.Vector Float' has @chunkTileCount@ elements.
  deriving (Eq, Show)

------------------------------------------------------------------------
-- Full overlay
------------------------------------------------------------------------

-- | A complete overlay: schema plus data.
data Overlay = Overlay
  { ovSchema :: !OverlaySchema
  -- ^ The schema governing this overlay's structure.
  , ovData   :: !OverlayData
  -- ^ The overlay's tile data, in the layout declared by the schema.
  , ovProvenance :: !OverlayProvenance
  -- ^ Overlay provenance metadata (seed/version/source).
  } deriving (Eq, Show)

-- | Create an empty overlay from a schema (no populated data).
--
-- * Sparse schemas start with an empty 'IntMap'.
-- * Dense schemas must be initialised per-chunk with the correct
--   tile count; this constructor produces an empty 'IntMap' — the caller
--   is responsible for populating chunks before first use.
emptyOverlay :: OverlaySchema -> Overlay
emptyOverlay schema = Overlay
  { ovSchema = schema
  , ovData   = case osStorage schema of
      StorageSparse -> SparseData IntMap.empty
      StorageDense  -> DenseData  IntMap.empty
  , ovProvenance = emptyOverlayProvenance
  }

-- | Convenience: extract the overlay name from its schema.
overlayName :: Overlay -> Text
overlayName = osName . ovSchema

------------------------------------------------------------------------
-- Overlay store
------------------------------------------------------------------------

-- | Collection of all active overlays, keyed by overlay name.
newtype OverlayStore = OverlayStore (Map Text Overlay)
  deriving (Eq, Show)

-- | Empty store with no registered overlays.
emptyOverlayStore :: OverlayStore
emptyOverlayStore = OverlayStore Map.empty

-- | Look up an overlay by name.
lookupOverlay :: Text -> OverlayStore -> Maybe Overlay
lookupOverlay name (OverlayStore m) = Map.lookup name m

-- | Insert or replace an overlay in the store.
insertOverlay :: Overlay -> OverlayStore -> OverlayStore
insertOverlay ov (OverlayStore m) =
  OverlayStore (Map.insert (overlayName ov) ov m)

-- | Remove an overlay by name.
deleteOverlay :: Text -> OverlayStore -> OverlayStore
deleteOverlay name (OverlayStore m) =
  OverlayStore (Map.delete name m)

-- | List all overlay names in the store.
overlayNames :: OverlayStore -> [Text]
overlayNames (OverlayStore m) = Map.keys m

-- | Number of overlays in the store.
overlayCount :: OverlayStore -> Int
overlayCount (OverlayStore m) = Map.size m
