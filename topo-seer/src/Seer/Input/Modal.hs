{-# LANGUAGE OverloadedStrings #-}

-- | Shared keyboard\/text-input handlers for modal dialogs.
--
-- Both preset and world save\/load dialogs follow the same
-- interaction pattern:
--
-- * __Text input modals__ — printable characters are appended,
--   @Backspace@ deletes the last character, @Return@ confirms,
--   @Escape@ cancels.
-- * __List selection modals__ — @Up@\/@Down@ change the selection,
--   @Return@ confirms, @Escape@ cancels.
--
-- This module provides generic handlers parameterised by callbacks so
-- each dialog only specifies its domain-specific behaviour.
module Seer.Input.Modal
  ( -- * Text input modals
    handleModalTextKey
  , handleModalTextInput
    -- * List selection modals
  , handleModalListKey
  ) where

import Data.Char (isPrint)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified SDL

-- | Handle keyboard events for a text-input modal dialog.
--
-- * @Escape@  → call @onCancel@
-- * @Return@  → call @onConfirm@
-- * @Backspace@ → call @onBackspace@
-- * anything else → ignored
handleModalTextKey
  :: SDL.Keycode
  -> IO ()        -- ^ onConfirm (Enter pressed)
  -> IO ()        -- ^ onCancel  (Escape pressed)
  -> IO ()        -- ^ onBackspace
  -> IO ()
handleModalTextKey keycode onConfirm onCancel onBackspace =
  case keycode of
    SDL.KeycodeEscape    -> onCancel
    SDL.KeycodeReturn    -> onConfirm
    SDL.KeycodeBackspace -> onBackspace
    _                    -> pure ()

-- | Filter and append printable characters from an SDL text-input
-- event to the current buffer, then call the setter with the updated
-- text.
--
-- @handleModalTextInput currentText inputText setter@ filters
-- @inputText@ to printable characters, appends them to @currentText@,
-- and calls @setter@ with the result.
handleModalTextInput
  :: Text          -- ^ Current text buffer
  -> Text          -- ^ Raw SDL text-input text
  -> (Text -> IO ()) -- ^ Setter for new buffer value
  -> IO ()
handleModalTextInput current inputText setter = do
  let accepted = Text.filter isPrint inputText
  setter (current <> accepted)

-- | Handle keyboard events for a list-selection modal dialog.
--
-- * @Escape@ → call @onCancel@
-- * @Return@ → call @onConfirm@
-- * @Up@     → decrement selection (clamped to 0)
-- * @Down@   → increment selection (clamped to @maxIndex@)
-- * anything else → ignored
handleModalListKey
  :: SDL.Keycode
  -> Int           -- ^ Current selection index
  -> Int           -- ^ Maximum valid index (inclusive)
  -> IO ()         -- ^ onConfirm (Enter pressed)
  -> IO ()         -- ^ onCancel  (Escape pressed)
  -> (Int -> IO ()) -- ^ setSelection
  -> IO ()
handleModalListKey keycode sel maxIdx onConfirm onCancel setSelection =
  case keycode of
    SDL.KeycodeEscape -> onCancel
    SDL.KeycodeReturn -> onConfirm
    SDL.KeycodeUp     -> setSelection (max 0 (sel - 1))
    SDL.KeycodeDown   -> setSelection (min maxIdx (sel + 1))
    _                 -> pure ()
