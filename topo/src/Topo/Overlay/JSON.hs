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
  , overlayFromJSON
  ) where

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
import Data.Aeson.Types (Parser, parseEither)
import Data.Bifunctor (first)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Topo.Overlay
  ( Overlay(..)
  , OverlayChunk(..)
  , OverlayData(..)
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

parseOverlayPayload :: OverlaySchema -> Value -> Parser Overlay
parseOverlayPayload schema = withObject "overlay payload" $ \o -> do
  storage <- o .: "storage"
  expectStorage (osStorage schema) storage
  chunksVal <- o .: "chunks"
  overlayData <- case osStorage schema of
    StorageSparse -> SparseData <$> parseSparseChunks schema chunksVal
    StorageDense -> DenseData <$> parseDenseChunks chunksVal
  pure Overlay
    { ovSchema = schema
    , ovData = overlayData
    }

parseSparseChunks :: OverlaySchema -> Value -> Parser (IntMap OverlayChunk)
parseSparseChunks schema value = do
  payload <- parseJSON value :: Parser [SparseChunkPayload]
  pairs <- traverse (decodeSparseChunk schema) payload
  pure (IntMap.fromList pairs)

parseDenseChunks :: Value -> Parser (IntMap (V.Vector (U.Vector Float)))
parseDenseChunks value = do
  payload <- parseJSON value :: Parser [DenseChunkPayload]
  let pairs = map decodeDenseChunk payload
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

decodeDenseChunk :: DenseChunkPayload -> (Int, V.Vector (U.Vector Float))
decodeDenseChunk payload =
  ( dcpChunkId payload
  , V.fromList (map U.fromList (dcpFields payload))
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

expectStorage :: OverlayStorage -> Text -> Parser ()
expectStorage expected actual =
  if actual == storageTag expected
    then pure ()
    else fail "storage does not match schema"

storageTag :: OverlayStorage -> Text
storageTag StorageSparse = "sparse"
storageTag StorageDense = "dense"

toText :: String -> Text
toText = Text.pack
