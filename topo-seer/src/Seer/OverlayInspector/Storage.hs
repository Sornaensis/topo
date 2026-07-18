{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Application-worker persistence for exact overlay export service payloads.
module Seer.OverlayInspector.Storage
  ( overlayExportDirectory
  , overlayExportName
  , saveOverlayExport
  , saveOverlayExportUnder
  ) where

import Control.Exception (IOException, onException, try)
import Data.Aeson (Value, encode)
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , getHomeDirectory
  , removeFile
  )
import System.FilePath ((</>))
import System.IO (hClose, openBinaryTempFile)

-- | User-owned destination for SDL export saves.
overlayExportDirectory :: IO FilePath
overlayExportDirectory = do
  home <- getHomeDirectory
  pure (home </> ".topo" </> "exports")

-- | Stable filesystem-safe basename derived from the selected overlay.
overlayExportName :: Text -> Text
overlayExportName raw =
  let sanitized = Text.map replace raw
      trimmed = Text.dropAround (== '-') sanitized
  in if Text.null trimmed then "overlay" else Text.take 96 trimmed
  where
    replace character
      | isAlphaNum character || character `elem` ['-', '_'] = character
      | otherwise = '-'

saveOverlayExport :: Text -> Value -> IO (Either Text FilePath)
saveOverlayExport overlayName payload = do
  attemptedDirectory <- try @IOException overlayExportDirectory
  case attemptedDirectory of
    Left err -> pure (Left (Text.pack (show err)))
    Right directory -> saveOverlayExportUnder directory overlayName payload

-- | Injected-root variant used by tests. The final export itself is created
-- with an exclusive temporary-file primitive in the destination directory.
-- This avoids replacement races and never leaves a link to a removed target.
saveOverlayExportUnder :: FilePath -> Text -> Value -> IO (Either Text FilePath)
saveOverlayExportUnder directory overlayName payload = do
  attempted <- try @IOException $ do
    createDirectoryIfMissing True directory
    (path, handle) <- openBinaryTempFile directory
      (Text.unpack (overlayExportName overlayName) <> "-.json")
    let cleanup = do
          _ <- try @IOException (hClose handle)
          exists <- doesFileExist path
          if exists then removeFile path else pure ()
    (do
        LazyByteString.hPut handle (encode payload)
        hClose handle
        pure path)
      `onException` cleanup
  pure $ case attempted of
    Left err -> Left (Text.pack (show err))
    Right path -> Right path
