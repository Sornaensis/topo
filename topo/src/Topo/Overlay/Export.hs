{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Chunk-level overlay encode\/decode for RPC and downstream consumers.
--
-- This module provides raw chunk-level binary encoding suitable for
-- embedding in MessagePack RPC messages.  It is distinct from
-- 'Topo.Overlay.Storage', which handles full-file persistence with
-- headers and (future) compression.
--
-- = Wire format — sparse chunk
--
-- @
-- Word32le  tileCount        — number of populated tiles
-- per tile:
--   Word32le  tileIndex      — hex index within chunk
--   per field (in schema order):
--     Float/Int: Float32le
--     Bool:      Byte (0x00 = False, 0x01 = True)
--     Text:      Word32le(len) ++ UTF-8 bytes
-- @
--
-- = Wire format — dense chunk
--
-- @
-- Word32le  fieldCount       — number of fields (== length of schema fields)
-- per field (in schema order):
--   Word32le  elementCount   — number of Float32 elements (== chunkTileCount)
--   Float32le × elementCount — raw field data
-- @
module Topo.Overlay.Export
  ( -- * Sparse
    encodeSparseChunk
  , decodeSparseChunk
    -- * Dense
  , encodeDenseChunk
  , decodeDenseChunk
    -- * Typed dispatch
  , encodeChunkData
  , decodeChunkData
    -- * Bulk export
  , exportOverlayChunks
  ) where

import Control.Monad (replicateM)
import Data.Binary.Get
  ( Get
  , getByteString
  , getFloatle
  , getWord32le
  , getWord8
  , runGetOrFail
  )
import Data.Binary.Put
  ( Put
  , putByteString
  , putFloatle
  , putWord32le
  , putWord8
  , runPut
  )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)

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

------------------------------------------------------------------------
-- Sparse encode/decode
------------------------------------------------------------------------

-- | Encode a sparse overlay chunk to bytes.
encodeSparseChunk :: OverlaySchema -> OverlayChunk -> BS.ByteString
encodeSparseChunk schema (OverlayChunk tiles) =
  BL.toStrict $ runPut $ do
    putWord32le (fromIntegral (IntMap.size tiles))
    mapM_ (putSparseRecord (osFields schema)) (IntMap.toList tiles)

-- | Decode a sparse overlay chunk from bytes.
decodeSparseChunk :: OverlaySchema -> BS.ByteString -> Either Text OverlayChunk
decodeSparseChunk schema bytes =
  case runGetOrFail (getSparseChunk (osFields schema)) (BL.fromStrict bytes) of
    Left (_, _, err) -> Left (Text.pack err)
    Right (_, _, chunk) -> Right chunk

getSparseChunk :: [OverlayFieldDef] -> Get OverlayChunk
getSparseChunk fields = do
  count <- fromIntegral <$> getWord32le
  entries <- replicateM count (getSparseRecord fields)
  pure (OverlayChunk (IntMap.fromList entries))

putSparseRecord :: [OverlayFieldDef] -> (Int, OverlayRecord) -> Put
putSparseRecord fields (tileIdx, OverlayRecord vals) = do
  putWord32le (fromIntegral tileIdx)
  mapM_ putFieldValue (zip fields (V.toList vals))

getSparseRecord :: [OverlayFieldDef] -> Get (Int, OverlayRecord)
getSparseRecord fields = do
  tileIdx <- fromIntegral <$> getWord32le
  vals <- mapM (getFieldValue . ofdType) fields
  pure (tileIdx, OverlayRecord (V.fromList vals))

putFieldValue :: (OverlayFieldDef, OverlayValue) -> Put
putFieldValue (fd, val) = case ofdType fd of
  OFFloat -> putFloatle (getFloat val)
  OFInt   -> putWord32le (fromIntegral (getInt val))
  OFBool  -> putWord8 (if getBool val then 1 else 0)
  OFText  -> do
    let txt = getText val
        encoded = encodeUtf8 txt
    putWord32le (fromIntegral (BS.length encoded))
    putByteString encoded
  OFList elemType -> do
    let elems = getList val
    putWord32le (fromIntegral (V.length elems))
    V.forM_ elems $ \elemVal ->
      putElementValue elemType elemVal

-- | Encode a single list element value.
putElementValue :: OverlayFieldType -> OverlayValue -> Put
putElementValue OFFloat v = putFloatle (getFloat v)
putElementValue OFInt   v = putWord32le (fromIntegral (getInt v))
putElementValue OFBool  v = putWord8 (if getBool v then 1 else 0)
putElementValue OFText  v = do
  let encoded = encodeUtf8 (getText v)
  putWord32le (fromIntegral (BS.length encoded))
  putByteString encoded
putElementValue (OFList _) _ = pure ()  -- nested lists are rejected by schema validation

getFieldValue :: OverlayFieldType -> Get OverlayValue
getFieldValue OFFloat = OVFloat <$> getFloatle
getFieldValue OFInt   = OVInt . fromIntegral <$> (getWord32le :: Get Word32)
getFieldValue OFBool  = do
  b <- getWord8
  pure (OVBool (b /= 0))
getFieldValue OFText  = do
  len <- fromIntegral <$> getWord32le
  bytes <- getByteString len
  case decodeUtf8' bytes of
    Left _    -> fail "overlay: invalid utf8 in text field"
    Right txt -> pure (OVText txt)
getFieldValue (OFList elemType) = do
  count <- fromIntegral <$> getWord32le
  elems <- replicateM count (getElementValue elemType)
  pure (OVList (V.fromList elems))

-- | Decode a single list element value.
getElementValue :: OverlayFieldType -> Get OverlayValue
getElementValue OFFloat = OVFloat <$> getFloatle
getElementValue OFInt   = OVInt . fromIntegral <$> (getWord32le :: Get Word32)
getElementValue OFBool  = do
  b <- getWord8
  pure (OVBool (b /= 0))
getElementValue OFText  = do
  len <- fromIntegral <$> getWord32le
  bytes <- getByteString len
  case decodeUtf8' bytes of
    Left _    -> fail "overlay: invalid utf8 in list text element"
    Right txt -> pure (OVText txt)
getElementValue (OFList _) = pure (OVList V.empty)  -- nested lists rejected by schema validation

-- | Extract a 'Float' from an 'OverlayValue', defaulting to 0.
getFloat :: OverlayValue -> Float
getFloat (OVFloat f) = f
getFloat _           = 0.0

-- | Extract an 'Int' from an 'OverlayValue', defaulting to 0.
getInt :: OverlayValue -> Int
getInt (OVInt i) = i
getInt _         = 0

-- | Extract a 'Bool' from an 'OverlayValue', defaulting to False.
getBool :: OverlayValue -> Bool
getBool (OVBool b) = b
getBool _          = False

-- | Extract a 'Text' from an 'OverlayValue', defaulting to empty.
getText :: OverlayValue -> Text
getText (OVText t) = t
getText _          = ""

-- | Extract a list from an 'OverlayValue', defaulting to empty.
getList :: OverlayValue -> Vector OverlayValue
getList (OVList vs) = vs
getList _           = V.empty

------------------------------------------------------------------------
-- Dense encode/decode
------------------------------------------------------------------------

-- | Encode a dense overlay chunk (SoA float vectors) to bytes.
encodeDenseChunk :: OverlaySchema -> Vector (U.Vector Float) -> BS.ByteString
encodeDenseChunk _schema fieldVecs =
  BL.toStrict $ runPut $ do
    putWord32le (fromIntegral (V.length fieldVecs))
    V.forM_ fieldVecs $ \vec -> do
      putWord32le (fromIntegral (U.length vec))
      U.forM_ vec putFloatle

-- | Decode a dense overlay chunk from bytes.
decodeDenseChunk :: OverlaySchema -> BS.ByteString -> Either Text (Vector (U.Vector Float))
decodeDenseChunk _schema bytes =
  case runGetOrFail getDenseChunk (BL.fromStrict bytes) of
    Left (_, _, err) -> Left (Text.pack err)
    Right (_, _, vecs) -> Right vecs

getDenseChunk :: Get (Vector (U.Vector Float))
getDenseChunk = do
  fieldCount <- fromIntegral <$> getWord32le
  V.replicateM fieldCount $ do
    elemCount <- fromIntegral <$> getWord32le
    U.replicateM elemCount getFloatle

------------------------------------------------------------------------
-- Typed dispatch
------------------------------------------------------------------------

-- | Encode a chunk of overlay data to bytes, dispatching on the
-- storage mode embedded in 'OverlayData'.
--
-- For a sparse chunk, the @chunkId@ selects which 'OverlayChunk' to
-- encode.  For a dense chunk, it selects which field-vector set.
-- Returns 'Nothing' if the chunk ID is not present in the data.
encodeChunkData :: OverlaySchema -> OverlayData -> Int -> Maybe BS.ByteString
encodeChunkData schema (SparseData chunks) chunkId =
  encodeSparseChunk schema <$> IntMap.lookup chunkId chunks
encodeChunkData schema (DenseData chunks) chunkId =
  encodeDenseChunk schema <$> IntMap.lookup chunkId chunks

-- | Decode a raw chunk payload into the appropriate 'OverlayData'
-- branch, dispatching on the schema's storage mode.
--
-- The result is a single-chunk 'OverlayData' with the given chunk ID
-- populated.
decodeChunkData
  :: OverlaySchema
  -> Int
  -> BS.ByteString
  -> Either Text OverlayData
decodeChunkData schema chunkId bytes =
  case osStorage schema of
    StorageSparse -> do
      chunk <- decodeSparseChunk schema bytes
      Right (SparseData (IntMap.singleton chunkId chunk))
    StorageDense -> do
      vecs <- decodeDenseChunk schema bytes
      Right (DenseData (IntMap.singleton chunkId vecs))

------------------------------------------------------------------------
-- Bulk export
------------------------------------------------------------------------

-- | Export all overlay chunks as (chunkId, encodedBytes) pairs.
--
-- Dispatches on the overlay's storage mode to select the appropriate
-- chunk encoder.
exportOverlayChunks :: Overlay -> [(Int, BS.ByteString)]
exportOverlayChunks ov =
  case ovData ov of
    SparseData chunks ->
      [ (cid, encodeSparseChunk (ovSchema ov) chunk)
      | (cid, chunk) <- IntMap.toList chunks
      ]
    DenseData chunks ->
      [ (cid, encodeDenseChunk (ovSchema ov) fieldVecs)
      | (cid, fieldVecs) <- IntMap.toList chunks
      ]
