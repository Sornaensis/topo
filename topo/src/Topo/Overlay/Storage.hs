{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Overlay persistence: @.topolay\/@ directory I\/O.
--
-- Each overlay type is stored as two files in the @.topolay\/@ directory:
--
-- * @\<name\>.toposchema@ — JSON schema file.
-- * @\<name\>.topolay@ — binary overlay data.
--
-- = Binary @.topolay@ format
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
-- Next Word64le: provenance seed
-- Next Word32le: provenance version
-- Next Word32le: provenance source length
-- Next Bytes:    provenance source (UTF-8)
-- Next Byte:     flags (0x00 for this format revision)
-- Next Word32le: chunk count
-- Per chunk:
--   Word32le  chunkId
--   Word32le  payloadLen
--   Bytes     payload (sparse or dense chunk encoding)
-- @
--
-- __Compression__: optional per-chunk zstd compression is supported.
-- When enabled at write time, header flags bit 1 is set and each chunk
-- stores compressed length + uncompressed length before the payload.
-- Readers continue to support uncompressed files (flags = @0x00@).
module Topo.Overlay.Storage
  ( -- * Single overlay I/O
    saveOverlay
  , saveOverlayWithOptions
  , loadOverlay
  , loadOverlayChunk
  , loadOverlayWithLifecycle
    -- * Store-level I/O
  , saveOverlayStore
  , saveOverlayStoreWithOptions
  , loadOverlayStore
    -- * Compression options
  , OverlayCompression(..)
  , OverlayStorageOptions(..)
  , defaultOverlayStorageOptions
    -- * Schema migration
  , OverlayLifecycle(..)
  , migrateOverlayData
  , MigrationResult(..)
  , migrationLifecycle
  , migrationWarnings
    -- * Errors
  , OverlayStorageError(..)
  , renderOverlayStorageError
    -- * Directory helpers
  , overlayDirPath
  , overlaySchemaPath
  , overlayDataPath
  ) where

import Control.Exception (IOException, try)
import Control.Monad (forM, forM_, replicateM, replicateM_)
import Data.Aeson (Value(..))
import Data.Binary.Get
  ( Get
  , getByteString
  , getFloatle
  , getWord32le
  , getWord64le
  , getWord8
  , runGetOrFail
  )
import Data.Binary.Put
  ( Put
  , putByteString
  , putFloatle
  , putWord32le
  , putWord64le
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
import Data.Word (Word32, Word64, Word8)
import Data.Bits (xor, (.&.), (.|.))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>), dropExtension, takeExtension)

import Topo.Overlay
  ( Overlay(..)
  , OverlayChunk(..)
  , OverlayData(..)
  , OverlayProvenance(..)
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
import Topo.Overlay.Storage.Compression
  ( OverlayCompression(..)
  , OverlayStorageOptions(..)
  , defaultOverlayStorageOptions
  )
import Topo.Overlay.Storage.ChunkIndex
  ( chunkEntriesForData
  , chunkIndexEntries
  , compressChunkPayload
  , decodeChunkEntryPayload
  , loadSparseChunkFromTopolayBytes
  , overlayFlagChunkIndex
  , overlayFlagZstd
  , overlayHasChunkIndex
  , overlaySupportsFlags
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
overlayDataPath dir name = dir </> Text.unpack name <> ".topolay"

------------------------------------------------------------------------
-- Single overlay save/load
------------------------------------------------------------------------

-- | Save a single overlay (schema + data) to a directory.
saveOverlay :: FilePath -> Overlay -> IO (Either OverlayStorageError ())
saveOverlay dir ov = saveOverlayWithOptions defaultOverlayStorageOptions dir ov

-- | Save a single overlay with explicit storage options.
saveOverlayWithOptions :: OverlayStorageOptions -> FilePath -> Overlay -> IO (Either OverlayStorageError ())
saveOverlayWithOptions options dir ov = do
  result <- try $ do
    createDirectoryIfMissing True dir
    let name = osName (ovSchema ov)
    -- Write schema
    BS.writeFile (overlaySchemaPath dir name) (encodeOverlaySchema (ovSchema ov))
    -- Write data
    datBytes <- case encodeOverlayData options ov of
      Left msg -> ioError (userError (Text.unpack msg))
      Right bytes -> pure bytes
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
  result <- loadOverlayWithLifecycle dir schema
  pure (fmap (\(overlay, _, _) -> overlay) result)

-- | Load one sparse overlay chunk by overlay name and chunk id.
--
-- This is an API-surface sketch for viewport-aware loading.  The current
-- implementation intentionally reuses whole-file decode via 'loadOverlay'
-- and then extracts one chunk.
--
-- Returns an empty chunk when the requested chunk id is not present.
-- Dense overlays are currently unsupported by this chunk-level API.
loadOverlayChunk
  :: FilePath
  -> Text
  -> Int
  -> IO (Either OverlayStorageError OverlayChunk)
loadOverlayChunk dir overlayName chunkId
  | chunkId < 0 =
      pure (Left (OverlayDecodeError overlayName "chunk id must be non-negative"))
  | otherwise = do
      let schemaFile = overlaySchemaPath dir overlayName
      schemaExists <- doesFileExist schemaFile
      if not schemaExists
        then pure (Left (OverlayMissingSchema overlayName))
        else do
          schemaResult <- try (BS.readFile schemaFile)
          case schemaResult of
            Left ex -> pure (Left (OverlayIOError "loadOverlayChunk/schema" ex))
            Right schemaBytes ->
              case parseOverlaySchema schemaBytes of
                Left err -> pure (Left (OverlaySchemaParseError overlayName err))
                Right schema -> do
                  datResult <- try (BS.readFile (overlayDataPath dir overlayName))
                  case datResult of
                    Left ex -> pure (Left (OverlayIOError "loadOverlayChunk/data" ex))
                    Right datBytes ->
                      pure $ case loadSparseChunkFromTopolayBytes schema chunkId datBytes of
                        Left err -> Left (OverlayDecodeError overlayName err)
                        Right chunk -> Right chunk

-- | Load a single overlay and compute lifecycle state/warnings.
--
-- Returned lifecycle semantics:
--
-- * 'OverlayActive' when data loaded cleanly (with or without
--   migration that preserved data compatibility).
-- * 'OverlayNeedsRepopulation' when migration detected an
--   incompatible dense field change.
loadOverlayWithLifecycle
  :: FilePath
  -> OverlaySchema
  -> IO (Either OverlayStorageError (Overlay, OverlayLifecycle, [Text]))
loadOverlayWithLifecycle dir schema = do
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
                        Right (ovData', prov) ->
                          if osVersion diskSchema == osVersion schema
                            then pure (Right (Overlay schema ovData' prov, OverlayActive, []))
                            else
                              let migrated = migrateOverlayData diskSchema schema ovData'
                                  lifecycle = migrationLifecycle migrated
                                  warnings = migrationWarnings schema migrated
                              in  pure (Right (Overlay schema (mrData migrated) prov, lifecycle, warnings))

-- | Lifecycle state for an overlay across generation/simulation/storage.
data OverlayLifecycle
  = OverlayRegistered
  | OverlaySeeded
  | OverlayActive
  | OverlayNeedsRepopulation
  deriving (Eq, Show)

------------------------------------------------------------------------
-- Store-level save/load
------------------------------------------------------------------------

-- | Save all overlays in a store to a @.topolay/@ directory.
saveOverlayStore :: FilePath -> OverlayStore -> IO (Either OverlayStorageError ())
saveOverlayStore dir store = saveOverlayStoreWithOptions defaultOverlayStorageOptions dir store

-- | Save all overlays in a store to a @.topolay/@ directory with explicit options.
saveOverlayStoreWithOptions :: OverlayStorageOptions -> FilePath -> OverlayStore -> IO (Either OverlayStorageError ())
saveOverlayStoreWithOptions options dir (OverlayStore store) = do
  result <- try (createDirectoryIfMissing True dir)
  case result of
    Left ex -> pure (Left (OverlayIOError "saveOverlayStore" ex))
    Right () -> do
      results <- forM (Map.elems store) $ \ov ->
        saveOverlayWithOptions options dir ov
      case [ e | Left e <- results ] of
        (e : _) -> pure (Left e)
        []      -> pure (Right ())

-- | Load all overlays from a @.topolay/@ directory, validated against
-- the provided schemas.
--
-- __Strict loading__: all overlay names in the provided list must have
-- both a @.toposchema@ and a @.topolay@ file on disk.  Missing files cause
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
-- Binary encode/decode (full .topolay file)
------------------------------------------------------------------------

-- | Encode overlay data to binary bytes (header + chunk payloads).
encodeOverlayData :: OverlayStorageOptions -> Overlay -> Either Text BS.ByteString
encodeOverlayData options ov = do
  payloadEntries <- prepareChunkEntries options (ovSchema ov) (ovData ov)
  pure $ BL.toStrict $ runPut $ do
    let schema = ovSchema ov
        prov = ovProvenance ov
        hasZstd = osoCompression options == CompressionZstd
        headerFlags = overlayFlagChunkIndex .|. if hasZstd then overlayFlagZstd else 0x00
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
    -- Provenance block
    putWord64le (opSeed prov)
    putWord32le (opVersion prov)
    let sourceBytes = encodeUtf8 (opSource prov)
    putWord32le (fromIntegral (BS.length sourceBytes))
    putByteString sourceBytes
    -- Header flags (bit 0 = chunk index, bit 1 = zstd)
    putWord8 headerFlags
    -- Chunks
    putWord32le (fromIntegral (length payloadEntries))
    forM_ payloadEntries $ \(cid, payloadLenWord, maybeRawLen, payload) -> do
      putWord32le (fromIntegral cid)
      putWord32le payloadLenWord
      forM_ maybeRawLen putWord32le
      putByteString payload
    let fieldDescriptorBytes = sum [4 + BS.length (encodeUtf8 (ofdName fd)) + 1 | fd <- fields]
        headerBytes = 4 + 4 + BS.length verBytes + 1 + 4 + fieldDescriptorBytes + 8 + 4 + 4 + BS.length sourceBytes + 1 + 4
        indexEntries = chunkIndexEntries (fromIntegral headerBytes) (map entryToIndex payloadEntries)
    putWord32le (fromIntegral (length indexEntries))
    forM_ indexEntries $ \(cid, offset) -> do
      putWord32le cid
      putWord64le offset
  where
    entryToIndex (cid, payloadLenWord, maybeRawLen, _payload) =
      let framingBytes = case maybeRawLen of
            Nothing -> 8
            Just _  -> 12
          entryBytes = framingBytes + fromIntegral payloadLenWord
      in (cid, entryBytes)

prepareChunkEntries
  :: OverlayStorageOptions
  -> OverlaySchema
  -> OverlayData
  -> Either Text [(Int, Word32, Maybe Word32, BS.ByteString)]
prepareChunkEntries options schema ovd =
  traverse oneEntry (chunkEntriesForData schema ovd)
  where
    oneEntry (cid, rawPayload) =
      case compressChunkPayload options rawPayload of
        Left msg -> Left msg
        Right (payload, maybeRawLen) ->
          Right (cid, fromIntegral (BS.length payload), fmap fromIntegral maybeRawLen, payload)

-- | Decode overlay data from binary bytes.
decodeOverlayData :: OverlaySchema -> BS.ByteString -> Either Text (OverlayData, OverlayProvenance)
decodeOverlayData schema bytes =
  case runGetOrFail (getOverlayData schema) (BL.fromStrict bytes) of
    Left  (_, _, err) -> Left (Text.pack err)
    Right (_, _, d)   -> Right d

getOverlayData :: OverlaySchema -> Get (OverlayData, OverlayProvenance)
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
  -- Provenance block
  provSeed <- getWord64le
  provVersion <- getWord32le
  sourceLen <- fromIntegral <$> getWord32le
  sourceBytes <- getByteString sourceLen
  source <- case decodeUtf8' sourceBytes of
    Left _ -> fail "overlay: invalid UTF-8 in provenance source"
    Right txt -> pure txt
  flags <- getWord8
  if not (overlaySupportsFlags flags)
    then fail ("overlay: unsupported flags byte 0x" <> show flags)
    else pure ()
  let prov = OverlayProvenance
        { opSeed = provSeed
        , opVersion = provVersion
        , opSource = source
        }
  -- Chunks
  chunkCount <- fromIntegral <$> getWord32le
  case modeByte of
    0x00 -> do
      -- Sparse
      entries <- replicateM chunkCount $ do
        cid <- fromIntegral <$> getWord32le
        compressedLen <- fromIntegral <$> getWord32le
        maybeRawLen <- if (flags .&. overlayFlagZstd) /= 0
          then Just . fromIntegral <$> getWord32le
          else pure Nothing
        payloadBytes <- getByteString compressedLen
        payload <- case decodeChunkEntryPayload flags maybeRawLen payloadBytes of
          Left err -> fail (Text.unpack err)
          Right p -> pure p
        case decodeSparseChunk schema payload of
          Left err -> fail (Text.unpack err)
          Right chunk -> pure (cid, chunk)
      if overlayHasChunkIndex flags
        then do
          idxCount <- fromIntegral <$> getWord32le
          replicateM_ idxCount (getWord32le >> getWord64le >> pure ())
        else pure ()
      pure (SparseData (IntMap.fromList entries), prov)
    0x01 -> do
      -- Dense
      entries <- replicateM chunkCount $ do
        cid <- fromIntegral <$> getWord32le
        compressedLen <- fromIntegral <$> getWord32le
        maybeRawLen <- if (flags .&. overlayFlagZstd) /= 0
          then Just . fromIntegral <$> getWord32le
          else pure Nothing
        payloadBytes <- getByteString compressedLen
        payload <- case decodeChunkEntryPayload flags maybeRawLen payloadBytes of
          Left err -> fail (Text.unpack err)
          Right p -> pure p
        case decodeDenseChunk schema payload of
          Left err -> fail (Text.unpack err)
          Right vecs -> pure (cid, vecs)
      if overlayHasChunkIndex flags
        then do
          idxCount <- fromIntegral <$> getWord32le
          replicateM_ idxCount (getWord32le >> getWord64le >> pure ())
        else pure ()
      pure (DenseData (IntMap.fromList entries), prov)
    other -> fail ("overlay: unknown storage mode byte 0x" <> show other)

------------------------------------------------------------------------
-- Schema migration
------------------------------------------------------------------------

-- | Result of a schema migration operation.
data MigrationResult = MigrationResult
  { mrData              :: !OverlayData
  -- ^ The migrated overlay data.
  , mrAddedFields       :: ![Text]
  -- ^ Fields that were added with defaults.
  , mrDroppedFields     :: ![Text]
  -- ^ Fields from the old schema not present in the new schema.
  , mrRenamedFields     :: ![(Text, Text)]
  -- ^ @(oldName, newName)@ pairs for renamed fields.
  , mrNeedsRepopulation :: !Bool
  -- ^ 'True' when the migration could not preserve dense data
  -- (e.g. an incompatible type change forced data loss).  The
  -- caller or first simulation tick should re-populate the overlay.
  } deriving (Show)

-- | Derive lifecycle state from migration outcome.
migrationLifecycle :: MigrationResult -> OverlayLifecycle
migrationLifecycle migrationResult
  | mrNeedsRepopulation migrationResult = OverlayNeedsRepopulation
  | otherwise = OverlayActive

-- | Human-readable migration warnings for callers that surface logs/UI.
migrationWarnings :: OverlaySchema -> MigrationResult -> [Text]
migrationWarnings schema migrationResult
  | mrNeedsRepopulation migrationResult =
      [ "overlay " <> osName schema
          <> " contains incompatible dense field changes and requires repopulation"
      ]
  | otherwise = []

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
            , mrNeedsRepopulation = False
            }
    DenseData chunks ->
      migrateDenseData oldFieldMap newFields added dropped renamed chunks

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
-- Dense migration
------------------------------------------------------------------------

-- | Migrate dense overlay data by remapping field vectors.
--
-- For each field in the new schema:
--
-- * If present in old data with a compatible type → copy the vector.
--   All dense-legal types (float, int, bool) share the same float
--   representation, so no per-element conversion is needed.
-- * If absent → create a default-filled vector using the field's
--   declared default value.
-- * If the source field used an incompatible type (text ↔ numeric,
--   which should be prevented by schema validation but we handle
--   defensively) → fill with defaults and flag 'mrNeedsRepopulation'.
migrateDenseData
  :: Map Text (Int, OverlayFieldType)  -- ^ old field map
  -> [OverlayFieldDef]                 -- ^ new fields
  -> [Text]                            -- ^ added field names
  -> [Text]                            -- ^ dropped field names
  -> [(Text, Text)]                    -- ^ renamed (old, new) pairs
  -> IntMap (Vector (U.Vector Float))  -- ^ old dense chunks
  -> MigrationResult
migrateDenseData oldFieldMap newFields added dropped renamed chunks
  | IntMap.null chunks =
      -- No data to migrate — produce empty dense data.
      MigrationResult
        { mrData              = DenseData IntMap.empty
        , mrAddedFields       = added
        , mrDroppedFields     = dropped
        , mrRenamedFields     = renamed
        , mrNeedsRepopulation = False
        }
  | otherwise =
      let -- Build the field mapping: for each new field, either an old
          -- field index or Nothing (needs default).
          fieldMapping :: [(OverlayFieldDef, Maybe Int)]
          fieldMapping = map resolveFieldMapping newFields

          resolveFieldMapping :: OverlayFieldDef -> (OverlayFieldDef, Maybe Int)
          resolveFieldMapping fd =
            let directLookup = Map.lookup (ofdName fd) oldFieldMap
                renameLookup = ofdRenamedFrom fd >>= \old -> Map.lookup old oldFieldMap
                resolved = case directLookup of
                  Just x  -> Just x
                  Nothing -> renameLookup
            in case resolved of
              Just (idx, oldType)
                | isDenseCompatible oldType (ofdType fd) -> (fd, Just idx)
                | otherwise -> (fd, Nothing)  -- incompatible → default
              Nothing -> (fd, Nothing)

          -- Any field that was present but had an incompatible type?
          hasIncompatible = any isIncompatibleMapping newFields
          isIncompatibleMapping fd =
            let directLookup = Map.lookup (ofdName fd) oldFieldMap
                renameLookup = ofdRenamedFrom fd >>= \old -> Map.lookup old oldFieldMap
                resolved = case directLookup of
                  Just x  -> Just x
                  Nothing -> renameLookup
            in case resolved of
              Just (_idx, oldType) -> not (isDenseCompatible oldType (ofdType fd))
              Nothing -> False

          migratedChunks = IntMap.map (migrateDenseChunk fieldMapping) chunks
      in  MigrationResult
            { mrData              = DenseData migratedChunks
            , mrAddedFields       = added
            , mrDroppedFields     = dropped
            , mrRenamedFields     = renamed
            , mrNeedsRepopulation = hasIncompatible
            }

-- | Check whether two field types are compatible for dense migration.
--
-- Dense overlays store all values as 'Float', so any numeric type
-- (float, int, bool) can be remapped to any other numeric type
-- without data loss in the float representation.  Only text is
-- incompatible (and should never appear in dense mode per schema
-- validation).
isDenseCompatible :: OverlayFieldType -> OverlayFieldType -> Bool
isDenseCompatible OFText _ = False
isDenseCompatible _ OFText = False
isDenseCompatible _ _      = True

-- | Remap a single dense chunk's field vectors according to the mapping.
migrateDenseChunk
  :: [(OverlayFieldDef, Maybe Int)]
  -> Vector (U.Vector Float)
  -> Vector (U.Vector Float)
migrateDenseChunk fieldMapping oldVecs =
  let -- Determine tile count from the first old vector, or 0 if empty
      tileCount
        | V.null oldVecs = 0
        | otherwise      = U.length (V.head oldVecs)
  in  V.fromList
        [ case mOldIdx of
            Just idx
              | idx >= 0 && idx < V.length oldVecs -> oldVecs V.! idx
              | otherwise -> defaultDenseVector tileCount fd
            Nothing -> defaultDenseVector tileCount fd
        | (fd, mOldIdx) <- fieldMapping
        ]

-- | Create a default-filled dense vector for a field.
defaultDenseVector :: Int -> OverlayFieldDef -> U.Vector Float
defaultDenseVector tileCount fd =
  U.replicate tileCount (defaultFloatValue fd)

-- | Extract the default float value for a dense field from its
-- JSON default declaration.
defaultFloatValue :: OverlayFieldDef -> Float
defaultFloatValue fd = case ofdDefault fd of
  Number n -> realToFrac n
  Bool   b -> if b then 1.0 else 0.0
  _        -> 0.0

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | FNV-1a hash of a text value (for the .topolay header name hash).
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
