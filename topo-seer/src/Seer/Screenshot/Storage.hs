{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

-- | Capability-confined, create-only persistence for optional screenshots.
--
-- Linux retains directory FDs and publishes an unnamed @O_TMPFILE@ directly
-- with @linkat(AT_EMPTY_PATH)@. Windows retains directory handles and performs
-- handle-relative create-new publication while denying readers until the full
-- image has been flushed. Other POSIX systems deliberately fail closed.
module Seer.Screenshot.Storage
  ( ScreenshotStoragePolicy(..)
  , ScreenshotStorageRoot
  , ScreenshotSaveError(..)
  , initialiseScreenshotStorage
  , saveScreenshotPng
  , saveScreenshotPngWithCommitHook
  , validateScreenshotPngPath
  ) where

import Control.Exception
  ( IOException
  , bracket
  , displayException
  , mask
  , try
  )
import Control.Monad (unless, when)
import qualified Data.ByteString as ByteString
import Data.Char (isControl)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import Foreign.Storable (peek, poke)
import System.Directory
  ( canonicalizePath
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesPathExist
  , pathIsSymbolicLink
  )
import System.FilePath
  ( dropTrailingPathSeparator
  , isAbsolute
  , normalise
  )

-- Native objects are opaque: neither their original path nor any native error
-- is retained in public values.
data CStorageRoot
data CStorageTransaction

foreign import ccall unsafe "topo_storage_root_open"
  c_storageRootOpen :: CString -> Ptr (Ptr CStorageRoot) -> IO CInt
foreign import ccall unsafe "&topo_storage_root_free"
  c_storageRootFreeFinalizer :: FunPtr (Ptr CStorageRoot -> IO ())
foreign import ccall unsafe "topo_storage_begin"
  c_storageBegin
    :: Ptr CStorageRoot
    -> CString
    -> Ptr a
    -> CSize
    -> Ptr (Ptr CStorageTransaction)
    -> IO CInt
foreign import ccall unsafe "topo_storage_commit"
  c_storageCommit :: Ptr CStorageTransaction -> IO CInt
foreign import ccall unsafe "topo_storage_txn_free"
  c_storageTransactionFree :: Ptr CStorageTransaction -> IO ()

-- | Retained filesystem authority. 'UnsupportedStorageRoot' exists only so a
-- configured writer can fail closed on platforms without a safe primitive.
data ScreenshotStorageRoot
  = NativeStorageRoot !(ForeignPtr CStorageRoot)
  | UnsupportedStorageRoot
  deriving (Eq)

data ScreenshotStoragePolicy
  = ScreenshotStorageDisabled
  | ScreenshotStorageEnabled !ScreenshotStorageRoot
  deriving (Eq)

instance Show ScreenshotStoragePolicy where
  show ScreenshotStorageDisabled = "ScreenshotStorageDisabled"
  show (ScreenshotStorageEnabled _) = "ScreenshotStorageEnabled"

-- | Stable, non-sensitive failure classes for screenshot persistence.
data ScreenshotSaveError
  = ScreenshotInvalidPath
  | ScreenshotUnsafePath
  | ScreenshotDestinationConflict
  | ScreenshotStorageUnavailable
  | ScreenshotUnexpectedIO
  deriving (Eq, Show)

nativeOk, nativeUnsafe, nativeConflict, nativeUnavailable :: CInt
nativeOk = 0
nativeUnsafe = 2
nativeConflict = 3
nativeUnavailable = 4

-- | Validate and initialise an explicitly configured screenshot storage root.
-- The configured entry is opened once and its filesystem identity is retained;
-- later saves never resolve this pathname again.
initialiseScreenshotStorage :: Maybe FilePath -> IO ScreenshotStoragePolicy
initialiseScreenshotStorage Nothing = pure ScreenshotStorageDisabled
initialiseScreenshotStorage (Just root)
  | not (isAbsolute root) =
      fail ("topo-seer startup: screenshotSaveDirectory must be absolute: " <> show root)
  | otherwise = do
      let normalisedRoot = dropTrailingPathSeparator (normalise root)
      prepared <- try (prepareRoot normalisedRoot) :: IO (Either IOException ())
      case prepared of
        Left err -> fail
          ( "topo-seer startup: invalid screenshotSaveDirectory "
          <> show root <> ": " <> displayException err
          )
        Right () -> ScreenshotStorageEnabled <$> openStorageRoot normalisedRoot

prepareRoot :: FilePath -> IO ()
prepareRoot root = do
  exists <- doesPathExist root
  unless exists (createDirectoryIfMissing True root)
  rejectLinkOrNonDirectory root
  -- Canonicalisation is validation only. The native layer opens the configured
  -- entry itself with no-follow semantics and retains that resulting identity.
  _ <- canonicalizePath root
  rejectLinkOrNonDirectory root

rejectLinkOrNonDirectory :: FilePath -> IO ()
rejectLinkOrNonDirectory root = do
  isLink <- pathIsSymbolicLink root
  when isLink $ fail "the configured root is a symbolic link or reparse point"
  isDirectory <- doesDirectoryExist root
  unless isDirectory $ fail "the configured root is not a directory"

openStorageRoot :: FilePath -> IO ScreenshotStorageRoot
openStorageRoot root =
#if defined(mingw32_HOST_OS) || defined(linux_HOST_OS)
  withUtf8CString (Text.pack root) $ \rootBytes -> alloca $ \result -> do
    poke result nullPtr
    status <- c_storageRootOpen rootBytes result
    if status == nativeOk
      then do
        pointer <- peek result
        NativeStorageRoot <$> newForeignPtr c_storageRootFreeFinalizer pointer
      else fail "topo-seer startup: screenshot storage capability is unavailable"
#else
  let _ = root
  in pure UnsupportedStorageRoot
#endif

-- | Validate an API path and return its canonical portable spelling.
validateScreenshotPngPath :: Text -> Either ScreenshotSaveError Text
validateScreenshotPngPath apiPath
  | Text.null apiPath = invalid
  | Text.head apiPath == '/' = invalid
  | Text.any (== '\\') apiPath = invalid
  | otherwise = do
      let segments = Text.splitOn "/" apiPath
      unlessEither (all validSegment segments)
      case reverse segments of
        [] -> invalid
        finalSegment : _ -> unlessEither (validPngBasename finalSegment)
      pure (Text.intercalate "/" segments)
  where
    invalid = Left ScreenshotInvalidPath
    unlessEither True = Right ()
    unlessEither False = invalid

validSegment :: Text -> Bool
validSegment segment =
  not (Text.null segment)
    && ByteString.length (TextEncoding.encodeUtf8 segment) <= 255
    && segment /= "."
    && segment /= ".."
    && not (Text.any invalidCharacter segment)
    && not (Text.isSuffixOf " " segment)
    && not (Text.isSuffixOf "." segment)
    && not (isWindowsDeviceBasename segment)
  where
    invalidCharacter character =
      character == ':' || character == '\0' || isControl character

validPngBasename :: Text -> Bool
validPngBasename basename =
  case Text.stripSuffix ".png" basename of
    Just stem -> not (Text.null stem)
    Nothing -> False

isWindowsDeviceBasename :: Text -> Bool
isWindowsDeviceBasename segment =
  let basename = Text.toUpper (Text.takeWhile (/= '.') segment)
  in basename `elem`
      [ "CON", "PRN", "AUX", "NUL", "CLOCK$", "CONIN$", "CONOUT$"
      ]
      || isNumberedDevice "COM" basename
      || isNumberedDevice "LPT" basename
  where
    isNumberedDevice prefix basename =
      case Text.stripPrefix prefix basename of
        Just suffix -> suffix `elem` ["1", "2", "3", "4", "5", "6", "7", "8", "9", "¹", "²", "³"]
        Nothing -> False

-- | Create a new PNG below a retained storage capability. Existing names are
-- conflicts and are never replaced.
saveScreenshotPng
  :: ScreenshotStorageRoot
  -> Text
  -> ByteString.ByteString
  -> IO (Either ScreenshotSaveError Text)
saveScreenshotPng = saveScreenshotPngWithCommitHook (pure ())

-- | Fault-injection variant. The hook runs after Linux has fully flushed its
-- unnamed file (or after Windows has retained its parent handle), immediately
-- before create-new publication.
saveScreenshotPngWithCommitHook
  :: IO ()
  -> ScreenshotStorageRoot
  -> Text
  -> ByteString.ByteString
  -> IO (Either ScreenshotSaveError Text)
saveScreenshotPngWithCommitHook beforeCommit storageRoot apiPath pngBytes =
  case validateScreenshotPngPath apiPath of
    Left err -> pure (Left err)
    Right relativePath -> do
      result <- try (persist beforeCommit storageRoot relativePath pngBytes)
        :: IO (Either IOException (Either ScreenshotSaveError ()))
      pure $ case result of
        Left _ -> Left ScreenshotUnexpectedIO
        Right (Left err) -> Left err
        Right (Right ()) -> Right relativePath

persist
  :: IO ()
  -> ScreenshotStorageRoot
  -> Text
  -> ByteString.ByteString
  -> IO (Either ScreenshotSaveError ())
persist _ UnsupportedStorageRoot _ _ = pure (Left ScreenshotStorageUnavailable)
persist beforeCommit (NativeStorageRoot root) relativePath pngBytes =
  withForeignPtr root $ \rootPointer ->
    withUtf8CString relativePath $ \pathPointer ->
      ByteString.useAsCStringLen pngBytes $ \(bytesPointer, bytesLength) ->
        alloca $ \transactionResult -> mask $ \restore -> do
          poke transactionResult nullPtr
          beginStatus <- c_storageBegin rootPointer pathPointer bytesPointer
            (fromIntegral bytesLength) transactionResult
          case nativeError beginStatus of
            Just err -> pure (Left err)
            Nothing -> do
              transaction <- peek transactionResult
              bracket
                (pure transaction)
                c_storageTransactionFree
                (\active -> do
                  restore beforeCommit
                  commitStatus <- c_storageCommit active
                  pure $ maybe (Right ()) Left (nativeError commitStatus))

nativeError :: CInt -> Maybe ScreenshotSaveError
nativeError status
  | status == nativeOk = Nothing
  | status == nativeUnsafe = Just ScreenshotUnsafePath
  | status == nativeConflict = Just ScreenshotDestinationConflict
  | status == nativeUnavailable = Just ScreenshotStorageUnavailable
  | otherwise = Just ScreenshotUnexpectedIO

withUtf8CString :: Text -> (CString -> IO a) -> IO a
withUtf8CString value action =
  ByteString.useAsCString (TextEncoding.encodeUtf8 value) action
