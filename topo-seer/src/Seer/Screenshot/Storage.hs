{-# LANGUAGE StrictData #-}

-- | Startup policy for optional screenshot persistence.
module Seer.Screenshot.Storage
  ( ScreenshotStoragePolicy(..)
  , initialiseScreenshotStorage
  ) where

import Control.Exception (IOException, displayException, try)
import Control.Monad (unless, when)
import System.Directory
  ( canonicalizePath
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesPathExist
  , pathIsSymbolicLink
  )
import System.FilePath (dropTrailingPathSeparator, isAbsolute, normalise)

-- | Immutable screenshot persistence authority carried by service contexts.
-- Capture is available in both cases; only an enabled policy permits a future
-- writer to resolve requested paths beneath the canonical root.
data ScreenshotStoragePolicy
  = ScreenshotStorageDisabled
  | ScreenshotStorageEnabled !FilePath
  deriving (Eq, Show)

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
