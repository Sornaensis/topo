{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

-- | Startup policy and sandboxed persistence for optional screenshots.
--
-- The writer validates the complete API path before touching the filesystem,
-- rejects links and non-directory parents, and repeats canonical containment
-- and type checks immediately before its atomic rename. These portable checks
-- narrow, but cannot eliminate, TOCTOU races with a hostile local process that
-- can mutate the sandbox. Full adversarial guarantees require handle-relative
-- operating-system APIs throughout.
module Seer.Screenshot.Storage
  ( ScreenshotStoragePolicy(..)
  , ScreenshotSaveError(..)
  , initialiseScreenshotStorage
  , saveScreenshotPng
  , saveScreenshotPngWithCommitHook
  , validateScreenshotPngPath
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception
  ( Exception
  , IOException
  , bracketOnError
  , catch
  , displayException
  , throwIO
  , try
  )
import Control.Monad (foldM, unless, when)
import qualified Data.ByteString as ByteString
import Data.Char (isControl)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory
  ( canonicalizePath
  , createDirectory
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , doesPathExist
  , pathIsSymbolicLink
  , removeFile
  , renameFile
  )
import System.FilePath
  ( dropTrailingPathSeparator
  , equalFilePath
  , isAbsolute
  , isRelative
  , makeRelative
  , normalise
  , splitDirectories
  , (</>)
  )
import System.IO
  ( Handle
  , hClose
  , hFlush
  , openBinaryTempFile
  )
import System.IO.Error (isDoesNotExistError)
#ifndef mingw32_HOST_OS
import qualified System.Posix.Files as Posix
#endif

-- | Immutable screenshot persistence authority carried by service contexts.
-- Capture is available in both cases; only an enabled policy permits a writer
-- to resolve requested paths beneath the canonical root.
data ScreenshotStoragePolicy
  = ScreenshotStorageDisabled
  | ScreenshotStorageEnabled !FilePath
  deriving (Eq, Show)

-- | Stable, non-sensitive failure classes for screenshot persistence.
-- Constructors intentionally carry no host paths or raw I/O exception text.
data ScreenshotSaveError
  = ScreenshotInvalidPath
  | ScreenshotUnsafePath
  | ScreenshotDestinationConflict
  | ScreenshotStorageUnavailable
  | ScreenshotUnexpectedIO
  deriving (Eq, Show)

newtype ScreenshotSaveAbort = ScreenshotSaveAbort ScreenshotSaveError
  deriving (Show)

instance Exception ScreenshotSaveAbort

-- | Validate and initialise an explicitly configured screenshot storage root.
-- A missing setting performs no filesystem work. Configured roots are created
-- when absent and retained only after canonicalisation.
initialiseScreenshotStorage :: Maybe FilePath -> IO ScreenshotStoragePolicy
initialiseScreenshotStorage Nothing = pure ScreenshotStorageDisabled
initialiseScreenshotStorage (Just root)
  | not (isAbsolute root) =
      fail ("topo-seer startup: screenshotSaveDirectory must be absolute: " <> show root)
  | otherwise = do
      -- Remove trailing separators and dot components before inspecting the
      -- configured leaf; otherwise link queries may observe its target.
      let normalisedRoot = dropTrailingPathSeparator (normalise root)
      result <- try (prepareRoot normalisedRoot) :: IO (Either IOException FilePath)
      case result of
        Left err -> fail
          ( "topo-seer startup: invalid screenshotSaveDirectory "
          <> show root <> ": " <> displayException err
          )
        Right canonicalRoot -> pure (ScreenshotStorageEnabled canonicalRoot)

prepareRoot :: FilePath -> IO FilePath
prepareRoot root = do
  exists <- doesPathExist root
  unless exists (createDirectoryIfMissing True root)
  rejectLinkOrNonDirectory root
  canonicalRoot <- canonicalizePath root
  -- Recheck the configured entry after canonicalisation so a root replaced
  -- during startup cannot be accepted merely because it resolved once.
  rejectLinkOrNonDirectory root
  pure canonicalRoot

rejectLinkOrNonDirectory :: FilePath -> IO ()
rejectLinkOrNonDirectory root = do
  isLink <- pathIsSymbolicLink root
  when isLink $
    fail "the configured root is a symbolic link or reparse point"
  isDirectory <- doesDirectoryExist root
  unless isDirectory $
    fail "the configured root is not a directory"

-- | Validate an API path and return its canonical portable spelling.
-- The returned value is always nonempty, relative, and @/@-separated.
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

-- | Persist strict PNG bytes below an initialized canonical root.
--
-- Missing parent directories are created one segment at a time. The bytes are
-- written to an exclusively created temporary file in the destination
-- directory, flushed and closed, then atomically renamed over an existing
-- regular file. The result never exposes the absolute sandbox location.
saveScreenshotPng
  :: FilePath
  -> Text
  -> ByteString.ByteString
  -> IO (Either ScreenshotSaveError Text)
saveScreenshotPng = saveScreenshotPngWithCommitHook (pure ())

-- | Low-level variant for fault injection and commit instrumentation.
-- The hook runs after the temporary file is flushed and closed, but before the
-- final safety rechecks and atomic rename. Normal callers should use
-- 'saveScreenshotPng'.
saveScreenshotPngWithCommitHook
  :: IO ()
  -> FilePath
  -> Text
  -> ByteString.ByteString
  -> IO (Either ScreenshotSaveError Text)
saveScreenshotPngWithCommitHook beforeCommit canonicalRoot apiPath pngBytes =
  case validateScreenshotPngPath apiPath of
    Left err -> pure (Left err)
    Right relativePath -> do
      let segments = Text.splitOn "/" relativePath
          parentSegments = map Text.unpack (init segments)
          destinationName = Text.unpack (last segments)
      ioResult <- try
        (try (persist beforeCommit canonicalRoot parentSegments destinationName pngBytes)
          :: IO (Either ScreenshotSaveAbort ()))
        :: IO (Either IOException (Either ScreenshotSaveAbort ()))
      pure $ case ioResult of
        Left _ -> Left ScreenshotUnexpectedIO
        Right (Left (ScreenshotSaveAbort err)) -> Left err
        Right (Right ()) -> Right relativePath

persist :: IO () -> FilePath -> [FilePath] -> FilePath -> ByteString.ByteString -> IO ()
persist beforeCommit root parentSegments destinationName pngBytes = do
  checkedRoot <- checkRoot root
  destinationDirectory <- foldM (prepareParent checkedRoot) checkedRoot parentSegments
  checkDestination destinationDirectory destinationName
  withExclusiveTemp destinationDirectory $ \temporaryPath temporaryHandle -> do
    ioOr ScreenshotUnexpectedIO (ByteString.hPut temporaryHandle pngBytes)
    ioOr ScreenshotUnexpectedIO (hFlush temporaryHandle)
    ioOr ScreenshotUnexpectedIO (hClose temporaryHandle)
    ioOr ScreenshotUnexpectedIO beforeCommit
    commitWithRetries
      checkedRoot
      parentSegments
      destinationDirectory
      destinationName
      temporaryPath

checkRoot :: FilePath -> IO FilePath
checkRoot root = do
  unless (isAbsolute root) (abort ScreenshotStorageUnavailable)
  linked <- ioOr ScreenshotStorageUnavailable (pathIsLink root)
  when linked (abort ScreenshotUnsafePath)
  directory <- ioOr ScreenshotStorageUnavailable (doesDirectoryExist root)
  unless directory (abort ScreenshotStorageUnavailable)
  canonical <- ioOr ScreenshotStorageUnavailable (canonicalizePath root)
  unless (equalFilePath canonical (normalise root)) (abort ScreenshotUnsafePath)
  pure canonical

prepareParent :: FilePath -> FilePath -> FilePath -> IO FilePath
prepareParent root parent segment = do
  let candidate = parent </> segment
  linked <- ioOr ScreenshotStorageUnavailable (pathIsLink candidate)
  when linked (abort ScreenshotUnsafePath)
  exists <- ioOr ScreenshotStorageUnavailable (doesPathExist candidate)
  if exists
    then checkExistingParent root candidate
    else do
      creation <- try (createDirectory candidate) :: IO (Either IOException ())
      case creation of
        Right () -> pure ()
        Left _ -> do
          -- Another writer may have created this segment concurrently.
          nowExists <- ioOr ScreenshotStorageUnavailable (doesPathExist candidate)
          unless nowExists (abort ScreenshotStorageUnavailable)
      checkExistingParent root candidate
  pure candidate

checkExistingParent :: FilePath -> FilePath -> IO ()
checkExistingParent root candidate = do
  linked <- ioOr ScreenshotStorageUnavailable (pathIsLink candidate)
  when linked (abort ScreenshotUnsafePath)
  directory <- ioOr ScreenshotStorageUnavailable (doesDirectoryExist candidate)
  unless directory (abort ScreenshotDestinationConflict)
  ensureCanonicalContainment root candidate

checkDestination :: FilePath -> FilePath -> IO ()
checkDestination parent destinationName = do
  let destination = parent </> destinationName
  linked <- ioOr ScreenshotStorageUnavailable (pathIsLink destination)
  when linked (abort ScreenshotUnsafePath)
  exists <- ioOr ScreenshotStorageUnavailable (doesPathExist destination)
  when exists $ do
    regular <- ioOr ScreenshotStorageUnavailable (isRegularFile destination)
    unless regular (abort ScreenshotDestinationConflict)

commitWithRetries :: FilePath -> [FilePath] -> FilePath -> FilePath -> FilePath -> IO ()
commitWithRetries root parentSegments destinationDirectory destinationName temporaryPath =
  go (100 :: Int)
  where
    destination = destinationDirectory </> destinationName
    go attemptsRemaining = do
      -- Repeat the portable checks for every retry so they remain adjacent to
      -- the successful commit even when a Windows reader briefly blocks it.
      recheckCommitState root parentSegments destinationName temporaryPath
      result <- try (renameFile temporaryPath destination) :: IO (Either IOException ())
      case result of
        Right () -> pure ()
        Left _
          | attemptsRemaining > 0 -> do
              threadDelay 2000
              go (attemptsRemaining - 1)
          | otherwise -> abort ScreenshotUnexpectedIO

recheckCommitState :: FilePath -> [FilePath] -> FilePath -> FilePath -> IO ()
recheckCommitState root parentSegments destinationName temporaryPath = do
  checkedRoot <- checkRoot root
  destinationDirectory <- foldM recheckParent checkedRoot parentSegments
  checkDestination destinationDirectory destinationName
  linkedTemp <- ioOr ScreenshotStorageUnavailable (pathIsLink temporaryPath)
  when linkedTemp (abort ScreenshotUnsafePath)
  regularTemp <- ioOr ScreenshotStorageUnavailable (isRegularFile temporaryPath)
  unless regularTemp (abort ScreenshotUnsafePath)
  ensureCanonicalContainment checkedRoot temporaryPath
  where
    recheckParent parent segment = do
      let candidate = parent </> segment
      checkExistingParent root candidate
      pure candidate

ensureCanonicalContainment :: FilePath -> FilePath -> IO ()
ensureCanonicalContainment root path = do
  canonical <- ioOr ScreenshotStorageUnavailable (canonicalizePath path)
  let relative = makeRelative root canonical
      components = splitDirectories relative
      contained =
        equalFilePath root canonical
          || (isRelative relative && not (null components) && all (/= "..") components)
  unless contained (abort ScreenshotUnsafePath)

pathIsLink :: FilePath -> IO Bool
pathIsLink path =
  pathIsSymbolicLink path `catch` \(err :: IOException) ->
    if isDoesNotExistError err then pure False else throwIO err

isRegularFile :: FilePath -> IO Bool
#ifdef mingw32_HOST_OS
isRegularFile = doesFileExist
#else
isRegularFile path = Posix.isRegularFile <$> Posix.getSymbolicLinkStatus path
#endif

withExclusiveTemp :: FilePath -> (FilePath -> Handle -> IO a) -> IO a
withExclusiveTemp directory action =
  bracketOnError
    (ioOr ScreenshotStorageUnavailable
      (openBinaryTempFile directory ".topo-screenshot-.tmp"))
    cleanupTemporary
    (uncurry action)

cleanupTemporary :: (FilePath, Handle) -> IO ()
cleanupTemporary (path, handle) = do
  ignoreIOException (hClose handle)
  ignoreIOException (removeFile path)

ignoreIOException :: IO () -> IO ()
ignoreIOException action = action `catch` \(_ :: IOException) -> pure ()

ioOr :: ScreenshotSaveError -> IO a -> IO a
ioOr err action = action `catch` \(_ :: IOException) -> abort err

abort :: ScreenshotSaveError -> IO a
abort = throwIO . ScreenshotSaveAbort
