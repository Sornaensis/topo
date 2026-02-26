{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Overlay persistence: @.topolay\/@ directory I\/O.
--
-- Each overlay type is stored as two files in the @.topolay\/@ directory:
--
-- * @\<name\>.toposchema@ — JSON schema file.
-- * @\<name\>.dat@ — binary overlay data.
--
-- = Binary @.dat@ format
--
-- @
-- Bytes 0–3:   overlay name hash (Word32le, FNV-1a)
-- Bytes 4–7:   schema version length (Word32le)
-- Bytes 8–…:   schema version string (UTF-8)
-- Next Word32le: storage mode (0x00 = sparse, 0x01 = dense)
-- Next Word32le: field count
-- Per field:
--   Word32le(nameLen) + UTF-8 name
--   Byte     fieldType (0=Float, 1=Int, 2=Bool, 3=Text)
-- Next Word32le: chunk count
-- Per chunk:
--   Word32le  chunkId
--   Word32le  payloadLen
--   Bytes     payload (sparse or dense chunk encoding)
-- @
--
-- __Note__: zstd compression is planned but not yet implemented.
-- The current format stores uncompressed payloads.  When zstd is
-- added, the header will include a compression flag byte.
module Topo.Overlay.Storage
  ( -- * Single overlay I/O
    saveOverlay
  , loadOverlay
    -- * Store-level I/O
  , saveOverlayStore
  , loadOverlayStore
    -- * Schema migration
  , migrateOverlayData
  , MigrationResult(..)
    -- * Errors
  , OverlayStorageError(..)
  , renderOverlayStorageError
    -- * Directory helpers
  , overlayDirPath
  , overlaySchemaPath
  , overlayDataPath
  ) where

import Control.Exception (IOException, try)
import Control.Monad (forM, forM_, replicateM)
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
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32, Word8)
import Data.Bits (xor)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>), dropExtension, takeExtension)

import Topo.Overlay
  ( Overlay(..)
  , OverlayChunk(..)
  , OverlayData(..)
  , OverlayRecord(..)
  , OverlayStore(..)
  , OverlayValue(..)
  , defaultValue
  , emptyOverlay
  )
import Topo.Overlay.Export
  ( decodeDenseChunk
  , decodeSparseChunk
  , encodeDenseChunk
  , encodeSparseChunk
  )
import Topo.Overlay.Schema
  ( OverlayFieldDef(..)
  , OverlayFieldType(..)
  , OverlaySchema(..)
  , OverlayStorage(..)
  , parseOverlaySchema
  , encodeOverlaySchema
  )

------------------------------------------------------------------------
-- Errors
------------------------------------------------------------------------

-- | Errors during overlay storage operations.
data OverlayStorageError
  = OverlayIOError !Text !IOException
  -- ^ I/O error with context description.
  | OverlayDecodeError !Text !Text
  -- ^ Overlay name + decode error message.
  | OverlaySchemaParseError !Text !Text
  -- ^ Overlay name + parse error message.
  | OverlayMissingSchema !Text
  -- ^ Expected schema file not found.
  | OverlayMissingData !Text
  -- ^ Expected data file not found.
  | OverlayManifestMismatch ![Text]
  -- ^ Overlay names in manifest that have no files on disk.
  deriving (Show)

-- | Render an error for user display.
renderOverlayStorageError :: OverlayStorageError -> Text
renderOverlayStorageError (OverlayIOError ctx e) =
  ctx <> ": " <> Text.pack (show e)
renderOverlayStorageError (OverlayDecodeError name msg) =
  "failed to decode overlay " <> name <> ": " <> msg
renderOverlayStorageError (OverlaySchemaParseError name msg) =
  "failed to parse schema for overlay " <> name <> ": " <> msg
renderOverlayStorageError (OverlayMissingSchema name) =
  "missing schema file for overlay: " <> name
renderOverlayStorageError (OverlayMissingData name) =
  "missing data file for overlay: " <> name
renderOverlayStorageError (OverlayManifestMismatch names) =
  "overlays listed in manifest but missing on disk: "
    <> Text.intercalate ", " names

------------------------------------------------------------------------
-- Path helpers
------------------------------------------------------------------------

-- | Given the base path of a @.topo@ file, compute the @.topolay/@ dir.
--
-- e.g. @overlayDirPath \"myworld.topo\" = \"myworld.topolay\"@
overlayDirPath :: FilePath -> FilePath
overlayDirPath topoPath = dropExtension topoPath <> ".topolay"

-- | Path to a specific overlay's schema file.
overlaySchemaPath :: FilePath -> Text -> FilePath
overlaySchemaPath dir name = dir </> Text.unpack name <> ".toposchema"

-- | Path to a specific overlay's binary data file.
overlayDataPath :: FilePath -> Text -> FilePath
overlayDataPath dir name = dir </> Text.unpack name <> ".dat"

------------------------------------------------------------------------
-- Single overlay save/load
------------------------------------------------------------------------

-- | Save a single overlay (schema + data) to a directory.
saveOverlay :: FilePath -> Overlay -> IO (Either OverlayStorageError ())
saveOverlay dir ov = do
  result <- try $ do
    createDirectoryIfMissing True dir
    let name = osName (ovSchema ov)
    -- Write schema
    BS.writeFile (overlaySchemaPath dir name) (encodeOverlaySchema (ovSchema ov))
    -- Write data
    let datBytes = encodeOverlayData (ovSchema ov) (ovData ov)
    BS.writeFile (overlayDataPath dir name) datBytes
  case result of
    Left ex  -> pure (Left (OverlayIOError "saveOverlay" ex))
    Right () -> pure (Right ())

-- | Load a single overlay, given a schema to validate against.
--
-- If the on-disk schema version differs from the provided schema,
-- data is migrated via 'migrateOverlayData'.
loadOverlay :: FilePath -> OverlaySchema -> IO (Either OverlayStorageError Overlay)
loadOverlay dir schema = do
  let name = osName schema
  let schemaFile = overlaySchemaPath dir name
  let datFile    = overlayDataPath dir name
  schemaExists <- doesFileExist schemaFile
  if not schemaExists
    then pure (Left (OverlayMissingSchema name))
    else do
      datExists <- doesFileExist datFile
      if not datExists
        then pure (Left (OverlayMissingData name))
        else do
          schemaResult <- try (BS.readFile schemaFile)
          case schemaResult of
            Left ex -> pure (Left (OverlayIOError "loadOverlay/schema" ex))
            Right schemaBytes ->
              case parseOverlaySchema schemaBytes of
                Left err -> pure (Left (OverlaySchemaParseError name err))
                Right diskSchema -> do
                  datResult <- try (BS.readFile datFile)
                  case datResult of
                    Left ex -> pure (Left (OverlayIOError "loadOverlay/data" ex))
                    Right datBytes ->
                      case decodeOverlayData diskSchema datBytes of
                        Left err -> pure (Left (OverlayDecodeError name err))
                        Right ovData' ->
                          if osVersion diskSchema == osVersion schema
                            then pure (Right (Overlay schema ovData'))
                            else
                              let migrated = migrateOverlayData diskSchema schema ovData'
                              in  pure (Right (Overlay schema (mrData migrated)))

------------------------------------------------------------------------
-- Store-level save/load
------------------------------------------------------------------------

-- | Save all overlays in a store to a @.topolay/@ directory.
saveOverlayStore :: FilePath -> OverlayStore -> IO (Either OverlayStorageError ())
saveOverlayStore dir (OverlayStore store) = do
  result <- try (createDirectoryIfMissing True dir)
  case result of
    Left ex -> pure (Left (OverlayIOError "saveOverlayStore" ex))
    Right () -> do
      results <- forM (Map.elems store) $ \ov ->
        saveOverlay dir ov
      case [ e | Left e <- results ] of
        (e : _) -> pure (Left e)
        []      -> pure (Right ())

-- | Load all overlays from a @.topolay/@ directory, validated against
-- the provided schemas.
--
-- __Strict loading__: all overlay names in the provided list must have
-- both a @.toposchema@ and a @.dat@ file on disk.  Missing files cause
-- an immediate error.
loadOverlayStore :: FilePath -> [OverlaySchema] -> IO (Either OverlayStorageError OverlayStore)
loadOverlayStore dir schemas = do
  dirExists <- doesDirectoryExist dir
  if not dirExists
    then
      -- No overlay dir: if no schemas expected, return empty store;
      -- otherwise error.
      if null schemas
        then pure (Right (OverlayStore Map.empty))
        else pure (Left (OverlayManifestMismatch (map osName schemas)))
    else do
      results <- forM schemas $ \schema -> do
        r <- loadOverlay dir schema
        pure (osName schema, r)
      let errors  = [ e | (_, Left e)  <- results ]
          loaded  = [ (n, ov) | (n, Right ov) <- results ]
      case errors of
        (e : _) -> pure (Left e)
        []      -> pure (Right (OverlayStore (Map.fromList loaded)))

------------------------------------------------------------------------
-- Autodiscovery (alternative to manifest-based loading)
------------------------------------------------------------------------

-- | Discover all overlay schemas in a @.topolay\/@ directory.
--
-- Scans for @.toposchema@ files and parses each one.
-- Returns successfully parsed schemas; parse errors are silently dropped.
-- Use 'loadOverlayStore' for strict manifest-validated loading.
discoverOverlaySchemas :: FilePath -> IO [OverlaySchema]
discoverOverlaySchemas dir = do
  dirExists <- doesDirectoryExist dir
  if not dirExists
    then pure []
    else do
      files <- listDirectory dir
      let schemaFiles = filter (\f -> takeExtension f == ".toposchema") files
      schemas <- forM schemaFiles $ \f -> do
        bytes <- BS.readFile (dir </> f)
        pure (parseOverlaySchema bytes)
      pure [ s | Right s <- schemas ]

------------------------------------------------------------------------
-- Binary encode/decode (full .dat file)
------------------------------------------------------------------------

-- | Encode overlay data to binary bytes (header + chunk payloads).
encodeOverlayData :: OverlaySchema -> OverlayData -> BS.ByteString
encodeOverlayData schema ovd =
  BL.toStrict $ runPut $ do
    -- Header: name hash
    putWord32le (fnvHash (osName schema))
    -- Schema version
    let verBytes = encodeUtf8 (osVersion schema)
    putWord32le (fromIntegral (BS.length verBytes))
    putByteString verBytes
    -- Storage mode
    putWord8 (case osStorage schema of
      StorageSparse -> 0x00
      StorageDense  -> 0x01)
    -- Field descriptors (for migration)
    let fields = osFields schema
    putWord32le (fromIntegral (length fields))
    forM_ fields $ \fd -> do
      let nameBytes = encodeUtf8 (ofdName fd)
      putWord32le (fromIntegral (BS.length nameBytes))
      putByteString nameBytes
      putWord8 (fieldTypeByte (ofdType fd))
    -- Chunks
    case ovd of
      SparseData chunks -> do
        putWord32le (fromIntegral (IntMap.size chunks))
        forM_ (IntMap.toList chunks) $ \(cid, chunk) -> do
          putWord32le (fromIntegral cid)
          let payload = encodeSparseChunk schema chunk
          putWord32le (fromIntegral (BS.length payload))
          putByteString payload
      DenseData chunks -> do
        putWord32le (fromIntegral (IntMap.size chunks))
        forM_ (IntMap.toList chunks) $ \(cid, fieldVecs) -> do
          putWord32le (fromIntegral cid)
          let payload = encodeDenseChunk schema fieldVecs
          putWord32le (fromIntegral (BS.length payload))
          putByteString payload

-- | Decode overlay data from binary bytes.
decodeOverlayData :: OverlaySchema -> BS.ByteString -> Either Text OverlayData
decodeOverlayData schema bytes =
  case runGetOrFail (getOverlayData schema) (BL.fromStrict bytes) of
    Left  (_, _, err) -> Left (Text.pack err)
    Right (_, _, d)   -> Right d

getOverlayData :: OverlaySchema -> Get OverlayData
getOverlayData schema = do
  -- Header: skip name hash
  _ <- getWord32le
  -- Schema version (skip — migration handled by caller)
  verLen <- fromIntegral <$> getWord32le
  _ <- getByteString verLen
  -- Storage mode
  modeByte <- getWord8
  -- Field descriptors (skip — used for migration by caller)
  fieldCount <- fromIntegral <$> getWord32le
  _ <- replicateM fieldCount $ do
    nameLen <- fromIntegral <$> getWord32le
    _ <- getByteString nameLen
    _ <- getWord8
    pure ()
  -- Chunks
  chunkCount <- fromIntegral <$> getWord32le
  case modeByte of
    0x00 -> do
      -- Sparse
      entries <- replicateM chunkCount $ do
        cid <- fromIntegral <$> getWord32le
        payloadLen <- fromIntegral <$> getWord32le
        payload <- getByteString payloadLen
        case decodeSparseChunk schema payload of
          Left err -> fail (Text.unpack err)
          Right chunk -> pure (cid, chunk)
      pure (SparseData (IntMap.fromList entries))
    0x01 -> do
      -- Dense
      entries <- replicateM chunkCount $ do
        cid <- fromIntegral <$> getWord32le
        payloadLen <- fromIntegral <$> getWord32le
        payload <- getByteString payloadLen
        case decodeDenseChunk schema payload of
          Left err -> fail (Text.unpack err)
          Right vecs -> pure (cid, vecs)
      pure (DenseData (IntMap.fromList entries))
    other -> fail ("overlay: unknown storage mode byte 0x" <> show other)

------------------------------------------------------------------------
-- Schema migration
------------------------------------------------------------------------

-- | Result of a schema migration operation.
data MigrationResult = MigrationResult
  { mrData         :: !OverlayData
  -- ^ The migrated overlay data.
  , mrAddedFields  :: ![Text]
  -- ^ Fields that were added with defaults.
  , mrDroppedFields :: ![Text]
  -- ^ Fields from the old schema not present in the new schema.
  , mrRenamedFields :: ![(Text, Text)]
  -- ^ @(oldName, newName)@ pairs for renamed fields.
  } deriving (Show)

-- | Migrate overlay data from an old schema to a new schema.
--
-- = Rules
--
-- For each field in the new schema:
--
-- * If present in old data with same type → copy value.
-- * If present with different type → apply type coercion if safe
--   (int↔float), otherwise use default.
-- * If absent → check @renamed_from@ annotation; if matched, use old
--   value under old name; otherwise use default.
--
-- Fields in old data not present in new schema → dropped silently.
--
-- Only sparse data is migrated field-by-field.  Dense data is rebuilt
-- from defaults (since field count/order may change).
migrateOverlayData :: OverlaySchema -> OverlaySchema -> OverlayData -> MigrationResult
migrateOverlayData oldSchema newSchema ovd =
  let oldFields = osFields oldSchema
      newFields = osFields newSchema
      -- Build old field name → (index, type) map
      oldFieldMap :: Map Text (Int, OverlayFieldType)
      oldFieldMap = Map.fromList
        [ (ofdName f, (i, ofdType f))
        | (i, f) <- zip [0..] oldFields
        ]
      -- Compute added/dropped/renamed
      added = [ ofdName f
              | f <- newFields
              , Map.notMember (ofdName f) oldFieldMap
              , case ofdRenamedFrom f of
                  Just old -> Map.notMember old oldFieldMap
                  Nothing  -> True
              ]
      dropped = [ ofdName f
                | f <- oldFields
                , not (any (\nf -> ofdName nf == ofdName f
                                || ofdRenamedFrom nf == Just (ofdName f)) newFields)
                ]
      renamed = [ (old, ofdName f)
                | f <- newFields
                , Just old <- [ofdRenamedFrom f]
                , Map.member old oldFieldMap
                ]
  in case ovd of
    SparseData chunks ->
      let migratedChunks = IntMap.map (migrateChunk oldFieldMap newFields) chunks
      in  MigrationResult
            { mrData = SparseData migratedChunks
            , mrAddedFields = added
            , mrDroppedFields = dropped
            , mrRenamedFields = renamed
            }
    DenseData _chunks ->
      -- Dense migration: since field layout changed, we can't simply
      -- remap vectors.  Return empty dense data; the caller (or first
      -- sim tick) is responsible for re-populating.
      MigrationResult
        { mrData = DenseData IntMap.empty
        , mrAddedFields = added
        , mrDroppedFields = dropped
        , mrRenamedFields = renamed
        }

-- | Migrate a single sparse chunk to a new field layout.
migrateChunk
  :: Map Text (Int, OverlayFieldType)  -- ^ old field map
  -> [OverlayFieldDef]                 -- ^ new fields
  -> OverlayChunk
  -> OverlayChunk
migrateChunk oldFieldMap newFields (OverlayChunk tiles) =
  OverlayChunk (IntMap.map (migrateRecord oldFieldMap newFields) tiles)

-- | Migrate a single sparse record to a new field layout.
migrateRecord
  :: Map Text (Int, OverlayFieldType)
  -> [OverlayFieldDef]
  -> OverlayRecord
  -> OverlayRecord
migrateRecord oldFieldMap newFields (OverlayRecord oldVals) =
  let newVals = V.fromList
        [ resolveField oldFieldMap oldVals fd
        | fd <- newFields
        ]
  in  OverlayRecord newVals

-- | Resolve a new field's value from old data.
resolveField
  :: Map Text (Int, OverlayFieldType)
  -> Vector OverlayValue
  -> OverlayFieldDef
  -> OverlayValue
resolveField oldFieldMap oldVals fd =
  -- Try direct name match first, then renamed_from
  let directLookup = Map.lookup (ofdName fd) oldFieldMap
      renameLookup = ofdRenamedFrom fd >>= \old -> Map.lookup old oldFieldMap
      resolved = case directLookup of
        Just x  -> Just x
        Nothing -> renameLookup
  in case resolved of
    Just (idx, oldType)
      | idx >= 0 && idx < V.length oldVals ->
          let oldVal = oldVals V.! idx
          in  coerceValue oldType (ofdType fd) oldVal
      | otherwise -> defaultValue fd
    Nothing -> defaultValue fd

-- | Attempt type coercion between field types.
coerceValue :: OverlayFieldType -> OverlayFieldType -> OverlayValue -> OverlayValue
coerceValue oldT newT val
  | oldT == newT = val
  | otherwise = case (oldT, newT, val) of
      (OFInt,   OFFloat, OVInt i)   -> OVFloat (fromIntegral i)
      (OFFloat, OFInt,   OVFloat f) -> OVInt (round f)
      (OFBool,  OFInt,   OVBool b)  -> OVInt (if b then 1 else 0)
      (OFInt,   OFBool,  OVInt i)   -> OVBool (i /= 0)
      (OFBool,  OFFloat, OVBool b)  -> OVFloat (if b then 1.0 else 0.0)
      (OFFloat, OFBool,  OVFloat f) -> OVBool (f >= 0.5)
      _                             -> val  -- incompatible, keep as-is

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | FNV-1a hash of a text value (for the .dat header name hash).
fnvHash :: Text -> Word32
fnvHash txt = BS.foldl' step fnvBasis (encodeUtf8 txt)
  where
    fnvBasis :: Word32
    fnvBasis = 2166136261
    fnvPrime :: Word32
    fnvPrime = 16777619
    step :: Word32 -> Word8 -> Word32
    step h b = (h `xor` fromIntegral b) * fnvPrime

-- | Encode a field type as a byte.
fieldTypeByte :: OverlayFieldType -> Word8
fieldTypeByte OFFloat = 0
fieldTypeByte OFInt   = 1
fieldTypeByte OFBool  = 2
fieldTypeByte OFText  = 3

-- | Decode a field type from a byte.
_parseFieldTypeByte :: Word8 -> Maybe OverlayFieldType
_parseFieldTypeByte 0 = Just OFFloat
_parseFieldTypeByte 1 = Just OFInt
_parseFieldTypeByte 2 = Just OFBool
_parseFieldTypeByte 3 = Just OFText
_parseFieldTypeByte _ = Nothing
