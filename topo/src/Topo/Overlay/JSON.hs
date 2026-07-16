{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | JSON serialization for overlay payloads used by plugin RPC.
--
-- The encoded payload is storage-aware and schema-guided:
--
-- * sparse overlays: chunk records with typed field arrays
-- * dense overlays: per-field float arrays
--
-- This representation is transport-oriented and intentionally separate from
-- binary persistence in 'Topo.Overlay.Export'.
module Topo.Overlay.JSON
  ( overlayToJSON
  , overlayToScopedJSON
  , overlayFromJSON
  , overlayFromScopedJSON
  ) where

import Control.Monad (unless, when)
import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Value(..)
  , (.:)
  , (.=)
  , object
  , withObject
  )
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser, parseEither)
import Data.Bifunctor (first)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Topo.Overlay
  ( Overlay(..)
  , OverlayChunk(..)
  , OverlayData(..)
  , emptyOverlayProvenance
  , OverlayRecord(..)
  , OverlayValue(..)
  )
import Topo.Overlay.Schema
  ( OverlayFieldDef(..)
  , OverlayFieldType(..)
  , OverlaySchema(..)
  , OverlayStorage(..)
  )

data SparseTilePayload = SparseTilePayload
  { stpTile :: !Int
  , stpFields :: ![Value]
  }

instance ToJSON SparseTilePayload where
  toJSON payload = object
    [ "tile" .= stpTile payload
    , "fields" .= stpFields payload
    ]

instance FromJSON SparseTilePayload where
  parseJSON = withObject "SparseTilePayload" $ \o ->
    SparseTilePayload
      <$> o .: "tile"
      <*> o .: "fields"

data SparseChunkPayload = SparseChunkPayload
  { scpChunkId :: !Int
  , scpTiles :: ![SparseTilePayload]
  }

instance ToJSON SparseChunkPayload where
  toJSON payload = object
    [ "chunk_id" .= scpChunkId payload
    , "tiles" .= scpTiles payload
    ]

instance FromJSON SparseChunkPayload where
  parseJSON = withObject "SparseChunkPayload" $ \o ->
    SparseChunkPayload
      <$> o .: "chunk_id"
      <*> o .: "tiles"

data DenseChunkPayload = DenseChunkPayload
  { dcpChunkId :: !Int
  , dcpFields :: ![[Float]]
  }

instance ToJSON DenseChunkPayload where
  toJSON payload = object
    [ "chunk_id" .= dcpChunkId payload
    , "fields" .= dcpFields payload
    ]

instance FromJSON DenseChunkPayload where
  parseJSON = withObject "DenseChunkPayload" $ \o ->
    DenseChunkPayload
      <$> o .: "chunk_id"
      <*> o .: "fields"

-- | Encode an 'Overlay' into the RPC JSON payload shape.
--
-- The resulting object includes:
--
-- * @storage@: @"sparse"@ or @"dense"@
-- * @chunks@: storage-specific chunk payloads
overlayToJSON :: Overlay -> Value
overlayToJSON overlay =
  object
    [ "storage" .= storageTag (osStorage (ovSchema overlay))
    , "chunks" .= chunksToJSON (ovData overlay)
    ]

-- | Encode only the selected chunk IDs without first constructing a complete
-- overlay JSON value.
overlayToScopedJSON :: IntSet.IntSet -> Overlay -> Value
overlayToScopedJSON chunkIds overlay =
  overlayToJSON (overlay { ovData = restrictOverlayData chunkIds (ovData overlay) })

restrictOverlayData :: IntSet.IntSet -> OverlayData -> OverlayData
restrictOverlayData chunkIds overlayData = case overlayData of
  SparseData chunks -> SparseData (IntMap.restrictKeys chunks chunkIds)
  DenseData chunks -> DenseData (IntMap.restrictKeys chunks chunkIds)

chunksToJSON :: OverlayData -> Value
chunksToJSON overlayData =
  case overlayData of
    SparseData chunks -> Aeson.toJSON (map toSparseChunkPayload (IntMap.toList chunks))
    DenseData chunks -> Aeson.toJSON (map toDenseChunkPayload (IntMap.toList chunks))

-- | Decode an overlay payload against a known 'OverlaySchema'.
--
-- Fails when payload structure is malformed, value types do not match
-- schema field types, or the payload storage tag differs from
-- 'osStorage'.
overlayFromJSON :: OverlaySchema -> Value -> Either Text Overlay
overlayFromJSON schema value =
  first toText (parseEither (parseOverlayPayload schema) value)

-- | Scoped output decoder with fail-closed keys, IDs, and duplicate checks.
-- The legacy decoder intentionally remains permissive for protocol-v4
-- no-scope compatibility.
overlayFromScopedJSON :: OverlaySchema -> Value -> Either Text Overlay
overlayFromScopedJSON schema value = first toText $ parseEither
  (\payload -> validateScopedOverlayValue payload >> parseOverlayPayload schema payload)
  value

validateScopedOverlayValue :: Value -> Parser ()
validateScopedOverlayValue = withObject "scoped overlay payload" $ \o -> do
  ensureObjectKeys "overlay payload" ["storage", "chunks"] o
  storage <- o .: "storage" :: Parser Text
  chunks <- o .: "chunks" :: Parser [Value]
  case storage of
    "sparse" -> do
      chunkIds <- traverse validateSparseChunk chunks
      ensureUniqueIds "sparse overlay chunk" chunkIds
    "dense" -> do
      chunkIds <- traverse validateDenseChunk chunks
      ensureUniqueIds "dense overlay chunk" chunkIds
    _ -> fail "unknown scoped overlay storage"
  where
    validateSparseChunk = withObject "scoped sparse chunk" $ \chunk -> do
      ensureObjectKeys "sparse chunk" ["chunk_id", "tiles"] chunk
      chunkId <- chunk .: "chunk_id"
      when (chunkId < 0) (fail "sparse overlay chunk ID must be non-negative")
      tiles <- chunk .: "tiles" :: Parser [Value]
      tileIds <- traverse validateSparseTile tiles
      ensureUniqueIds "sparse overlay tile" tileIds
      pure chunkId
    validateSparseTile = withObject "scoped sparse tile" $ \tile -> do
      ensureObjectKeys "sparse tile" ["tile", "fields"] tile
      tileId <- tile .: "tile"
      when (tileId < 0) (fail "sparse overlay tile index must be non-negative")
      _ <- tile .: "fields" :: Parser Value
      pure tileId
    validateDenseChunk = withObject "scoped dense chunk" $ \chunk -> do
      ensureObjectKeys "dense chunk" ["chunk_id", "fields"] chunk
      chunkId <- chunk .: "chunk_id"
      when (chunkId < 0) (fail "dense overlay chunk ID must be non-negative")
      _ <- chunk .: "fields" :: Parser Value
      pure chunkId

parseOverlayPayload :: OverlaySchema -> Value -> Parser Overlay
parseOverlayPayload schema = withObject "overlay payload" $ \o -> do
  storage <- o .: "storage"
  expectStorage (osStorage schema) storage
  chunksVal <- o .: "chunks"
  overlayData <- case osStorage schema of
    StorageSparse -> SparseData <$> parseSparseChunks schema chunksVal
    StorageDense -> DenseData <$> parseDenseChunks schema chunksVal
  pure Overlay
    { ovSchema = schema
    , ovData = overlayData
    , ovProvenance = emptyOverlayProvenance
    }

parseSparseChunks :: OverlaySchema -> Value -> Parser (IntMap OverlayChunk)
parseSparseChunks schema value = do
  payload <- parseJSON value :: Parser [SparseChunkPayload]
  pairs <- traverse (decodeSparseChunk schema) payload
  pure (IntMap.fromList pairs)

parseDenseChunks :: OverlaySchema -> Value -> Parser (IntMap (V.Vector (U.Vector Float)))
parseDenseChunks schema value = do
  payload <- parseJSON value :: Parser [DenseChunkPayload]
  pairs <- traverse (decodeDenseChunk schema) payload
  pure (IntMap.fromList pairs)

decodeSparseChunk :: OverlaySchema -> SparseChunkPayload -> Parser (Int, OverlayChunk)
decodeSparseChunk schema payload = do
  tiles <- traverse (decodeSparseTile schema) (scpTiles payload)
  pure (scpChunkId payload, OverlayChunk (IntMap.fromList tiles))

decodeSparseTile :: OverlaySchema -> SparseTilePayload -> Parser (Int, OverlayRecord)
decodeSparseTile schema payload = do
  values <- decodeRecordValues (osFields schema) (stpFields payload)
  pure (stpTile payload, OverlayRecord (V.fromList values))

decodeRecordValues :: [OverlayFieldDef] -> [Value] -> Parser [OverlayValue]
decodeRecordValues defs values =
  if length defs /= length values
    then fail "record field count does not match schema"
    else traverse decodeOne (zip defs values)
  where
    decodeOne (fd, value) = decodeOverlayValue (ofdType fd) value

decodeOverlayValue :: OverlayFieldType -> Value -> Parser OverlayValue
decodeOverlayValue OFFloat value = OVFloat <$> parseJSON value
decodeOverlayValue OFInt value = OVInt <$> parseJSON value
decodeOverlayValue OFBool value = OVBool <$> parseJSON value
decodeOverlayValue OFText value = OVText <$> parseJSON value
decodeOverlayValue (OFList elemType) value = do
  arr <- parseJSON value :: Parser [Value]
  elems <- traverse (decodeOverlayValue elemType) arr
  pure (OVList (V.fromList elems))

decodeDenseChunk :: OverlaySchema -> DenseChunkPayload -> Parser (Int, V.Vector (U.Vector Float))
decodeDenseChunk schema payload = do
  let fieldArrays = dcpFields payload
      expectedFieldCount = length (osFields schema)
      lengths = map length fieldArrays
  if length fieldArrays /= expectedFieldCount
    then fail "dense chunk field count does not match schema"
    else case lengths of
      [] -> pure ()
      firstLen:rest
        | any (/= firstLen) rest -> fail "dense chunk field arrays must have equal length"
        | otherwise -> pure ()
  pure
    ( dcpChunkId payload
    , V.fromList (map U.fromList fieldArrays)
    )

toSparseChunkPayload :: (Int, OverlayChunk) -> SparseChunkPayload
toSparseChunkPayload (chunkId, OverlayChunk records) =
  SparseChunkPayload
    { scpChunkId = chunkId
    , scpTiles = map toSparseTilePayload (IntMap.toList records)
    }

toSparseTilePayload :: (Int, OverlayRecord) -> SparseTilePayload
toSparseTilePayload (tileIdx, OverlayRecord fieldValues) =
  SparseTilePayload
    { stpTile = tileIdx
    , stpFields = map encodeOverlayValue (V.toList fieldValues)
    }

toDenseChunkPayload :: (Int, V.Vector (U.Vector Float)) -> DenseChunkPayload
toDenseChunkPayload (chunkId, fieldVecs) =
  DenseChunkPayload
    { dcpChunkId = chunkId
    , dcpFields = map U.toList (V.toList fieldVecs)
    }

encodeOverlayValue :: OverlayValue -> Value
encodeOverlayValue (OVFloat f) = toJSON f
encodeOverlayValue (OVInt i) = toJSON i
encodeOverlayValue (OVBool b) = toJSON b
encodeOverlayValue (OVText t) = toJSON t
encodeOverlayValue (OVList vs) = toJSON (map encodeOverlayValue (V.toList vs))

expectStorage :: OverlayStorage -> Text -> Parser ()
expectStorage expected actual =
  if actual == storageTag expected
    then pure ()
    else fail "storage does not match schema"

storageTag :: OverlayStorage -> Text
storageTag StorageSparse = "sparse"
storageTag StorageDense = "dense"

ensureObjectKeys :: Text -> [Text] -> KM.KeyMap Value -> Parser ()
ensureObjectKeys label allowed objectValue =
  unless (null unknown) $
    fail (Text.unpack (label <> " contains unsupported keys: " <> Text.intercalate ", " unknown))
  where
    allowedSet = Set.fromList allowed
    unknown = filter (`Set.notMember` allowedSet) (map Key.toText (KM.keys objectValue))

ensureUniqueIds :: Text -> [Int] -> Parser ()
ensureUniqueIds label values =
  case duplicates of
    duplicateId:_ -> fail (Text.unpack (label <> " ID is duplicated: " <> Text.pack (show duplicateId)))
    [] -> pure ()
  where
    counts = IntMap.fromListWith (+) [(value, 1 :: Int) | value <- values]
    duplicates = IntMap.keys (IntMap.filter (> 1) counts)

toText :: String -> Text
toText = Text.pack
