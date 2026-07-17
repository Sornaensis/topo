{-# LANGUAGE OverloadedStrings #-}

-- | Validation for user-supplied logical names used below persistence roots.
module Seer.Persistence.Name
  ( validatePersistenceName
  , persistenceNameExpectation
  ) where

import Data.Char (isAlpha, isControl)
import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath (isAbsolute, takeDrive)

-- | Stable validation detail shared by command and service boundaries.
persistenceNameExpectation :: Text
persistenceNameExpectation =
  "a nonblank basename without path separators, dot segments, absolute or drive forms, reserved path/device names, or control characters"

-- | Reject names that could influence path composition. The original logical
-- name is retained on success; whitespace inside an otherwise nonblank name is
-- valid and is not silently normalized.
validatePersistenceName :: Text -> Either Text ()
validatePersistenceName name
  | Text.null (Text.strip name) = invalid "must not be blank"
  | name == "." || name == ".." = invalid "must not be a dot segment"
  | Text.any isControl name = invalid "must not contain control characters"
  | Text.any isPathSeparator name = invalid "must not contain path separators"
  | isAbsolute path || hasDriveForm path = invalid "must not be an absolute or drive path"
  | Text.any isWindowsReservedCharacter name = invalid "must not contain reserved path characters"
  | isWindowsDeviceName name = invalid "must not be a reserved device name"
  | Text.isSuffixOf "." name || Text.isSuffixOf " " name =
      invalid "must not end in a dot or space"
  | otherwise = Right ()
  where
    path = Text.unpack name
    invalid reason = Left ("persistence name " <> reason)

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'

isWindowsReservedCharacter :: Char -> Bool
isWindowsReservedCharacter c = c `elem` ("<>:\"|?*" :: String)

isWindowsDeviceName :: Text -> Bool
isWindowsDeviceName name =
  normalized `elem` ["CON", "PRN", "AUX", "NUL"] || numberedDevice "COM" || numberedDevice "LPT"
  where
    normalized = Text.toUpper . Text.dropWhileEnd (== ' ') $ Text.takeWhile (/= '.') name
    numberedDevice prefix = case Text.stripPrefix prefix normalized of
      Just suffix -> Text.length suffix == 1 && suffix >= "1" && suffix <= "9"
      Nothing -> False

-- System.FilePath follows the host platform, so recognize Windows drive forms
-- explicitly as well to keep validation identical on every supported host.
hasDriveForm :: FilePath -> Bool
hasDriveForm path =
  not (null (takeDrive path)) || case path of
    drive:':':_ -> isAlpha drive
    _ -> False
