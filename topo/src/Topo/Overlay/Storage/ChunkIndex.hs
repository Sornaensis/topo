{-# LANGUAGE OverloadedStrings #-}

module Topo.Overlay.Storage.ChunkIndex
  ( overlayFlagChunkIndex
  , overlayFlagZstd
  , overlaySupportsFlags
  , overlayHasChunkIndex
  , chunkEntriesForData
  , chunkIndexEntries
  , compressChunkPayload
  , decodeChunkEntryPayload
  , loadSparseChunkFromTopolayBytes
  ) where

import Control.Monad (replicateM, replicateM_)
import qualified Codec.Compression.Zstd as Zstd
import Data.Bits ((.&.), (.|.), complement)
import Data.Binary.Get
  ( Get
  , getByteString
  , getWord32le
  , getWord64le
  , getWord8
  , runGetOrFail
  , skip
  )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap.Strict as IntMap
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word32, Word64, Word8)
import Topo.Overlay.Storage.Compression
  ( OverlayCompression(..)
  , OverlayStorageOptions(..)
  )
import Topo.Overlay
  ( OverlayChunk
  , OverlayData(..)
  , emptyOverlayChunk
  )
import Topo.Overlay.Export
  ( decodeSparseChunk
  , encodeDenseChunk
  , encodeSparseChunk
  )
import Topo.Overlay.Schema
  ( OverlaySchema(..)
  )

overlayFlagChunkIndex :: Word8
overlayFlagChunkIndex = 0x01

overlayFlagZstd :: Word8
overlayFlagZstd = 0x02

overlaySupportsFlags :: Word8 -> Bool
overlaySupportsFlags flags = (flags .&. complement (overlayFlagChunkIndex .|. overlayFlagZstd)) == 0

overlayHasChunkIndex :: Word8 -> Bool
overlayHasChunkIndex flags = (flags .&. overlayFlagChunkIndex) /= 0

chunkEntriesForData :: OverlaySchema -> OverlayData -> [(Int, BS.ByteString)]
chunkEntriesForData schema ovd =
  case ovd of
    SparseData chunks ->
      [ (cid, encodeSparseChunk schema chunk)
      | (cid, chunk) <- IntMap.toList chunks
      ]
    DenseData chunks ->
      [ (cid, encodeDenseChunk schema fieldVecs)
      | (cid, fieldVecs) <- IntMap.toList chunks
      ]

chunkIndexEntries :: Word64 -> [(Int, Int)] -> [(Word32, Word64)]
chunkIndexEntries startOffset entries = snd (foldl step (startOffset, []) entries)
  where
    step (offset, acc) (cid, entryBytes) =
      let next = offset + fromIntegral entryBytes
      in (next, acc <> [(fromIntegral cid, offset)])

compressChunkPayload :: OverlayStorageOptions -> BS.ByteString -> Either Text (BS.ByteString, Maybe Int)
compressChunkPayload options rawPayload =
  case osoCompression options of
    CompressionNone -> Right (rawPayload, Nothing)
    CompressionZstd ->
      let level = normalizeZstdLevel (osoZstdLevel options)
          compressed = Zstd.compress level rawPayload
      in Right (compressed, Just (BS.length rawPayload))

decodeChunkEntryPayload :: Word8 -> Maybe Int -> BS.ByteString -> Either Text BS.ByteString
decodeChunkEntryPayload flags maybeRawLen payload
  | (flags .&. overlayFlagZstd) == 0 = Right payload
  | otherwise =
      case maybeRawLen of
        Nothing -> Left "overlay: compressed chunk missing uncompressed length"
        Just expectedRawLen ->
          case Zstd.decompress payload of
            Zstd.Error err -> Left ("overlay: zstd decompression failed: " <> Text.pack err)
            Zstd.Skip -> Left "overlay: zstd frame missing decompressed-size metadata"
            Zstd.Decompress raw
              | BS.length raw /= expectedRawLen ->
                  Left "overlay: zstd decompressed length mismatch"
              | otherwise -> Right raw

normalizeZstdLevel :: Int -> Int
normalizeZstdLevel level = max 1 (min Zstd.maxCLevel level)

loadSparseChunkFromTopolayBytes
  :: OverlaySchema
  -> Int
  -> BS.ByteString
  -> Either Text OverlayChunk
loadSparseChunkFromTopolayBytes schema chunkId bytes
  | chunkId < 0 = Left "chunk id must be non-negative"
  | otherwise =
      case runGetOrFail (getSparseChunkFromBytes schema chunkId bytes) (BL.fromStrict bytes) of
        Left (_, _, err) -> Left errText
          where errText = fromStringText err
        Right (_, _, chunk) -> Right chunk

fromStringText :: String -> Text
fromStringText = Text.pack

getSparseChunkFromBytes
  :: OverlaySchema
  -> Int
  -> BS.ByteString
  -> Get OverlayChunk
getSparseChunkFromBytes schema targetCid fullBytes = do
  _ <- getWord32le
  verLen <- fromIntegral <$> getWord32le
  _ <- getByteString verLen
  mode <- getWord8
  if mode /= 0x00
    then fail "loadOverlayChunk currently supports sparse overlays only"
    else pure ()
  fieldCount <- fromIntegral <$> getWord32le
  replicateM_ fieldCount $ do
    nameLen <- fromIntegral <$> getWord32le
    _ <- getByteString nameLen
    _ <- getWord8
    pure ()
  _ <- getWord64le
  _ <- getWord32le
  sourceLen <- fromIntegral <$> getWord32le
  _ <- getByteString sourceLen
  flags <- getWord8
  if not (overlaySupportsFlags flags)
    then fail ("overlay: unsupported flags byte 0x" <> show flags)
    else pure ()
  chunkCount <- fromIntegral <$> getWord32le
  if overlayHasChunkIndex flags
    then do
      replicateM_ chunkCount $ do
        _ <- getWord32le
        payloadLen <- fromIntegral <$> getWord32le
        maybeRawLen <- if (flags .&. overlayFlagZstd) /= 0
          then Just . fromIntegral <$> getWord32le
          else pure Nothing
        let skipLen = case maybeRawLen of
              Nothing -> payloadLen
              Just _  -> payloadLen
        skip skipLen
      idxCount <- fromIntegral <$> getWord32le
      idx <- replicateM idxCount $ do
        cid <- getWord32le
        off <- getWord64le
        pure (cid, off)
      case lookup (fromIntegral targetCid) idx of
        Nothing -> pure emptyOverlayChunk
        Just off ->
          case runGetOrFail (decodeEntryAtOffset flags schema targetCid) (BL.drop (fromIntegral off) (BL.fromStrict fullBytes)) of
            Left (_, _, err) -> fail err
            Right (_, _, chunk) -> pure chunk
    else scanEntries flags schema targetCid chunkCount

scanEntries :: Word8 -> OverlaySchema -> Int -> Int -> Get OverlayChunk
scanEntries _ _ _ 0 = pure emptyOverlayChunk
scanEntries flags schema targetCid remaining = do
  cid <- fromIntegral <$> getWord32le
  compressedLen <- fromIntegral <$> getWord32le
  maybeRawLen <- if (flags .&. overlayFlagZstd) /= 0
    then Just . fromIntegral <$> getWord32le
    else pure Nothing
  payload <- getByteString compressedLen
  if cid == targetCid
    then case decodeChunkEntryPayload flags maybeRawLen payload >>= decodeSparseChunk schema of
      Left err -> fail (show err)
      Right chunk -> pure chunk
    else scanEntries flags schema targetCid (remaining - 1)

decodeEntryAtOffset :: Word8 -> OverlaySchema -> Int -> Get OverlayChunk
decodeEntryAtOffset flags schema expectedCid = do
  cid <- fromIntegral <$> getWord32le
  compressedLen <- fromIntegral <$> getWord32le
  maybeRawLen <- if (flags .&. overlayFlagZstd) /= 0
    then Just . fromIntegral <$> getWord32le
    else pure Nothing
  payload <- getByteString compressedLen
  if cid /= expectedCid
    then fail "overlay: chunk index entry points to mismatched chunk"
    else case decodeChunkEntryPayload flags maybeRawLen payload >>= decodeSparseChunk schema of
      Left err -> fail (show err)
      Right chunk -> pure chunk
