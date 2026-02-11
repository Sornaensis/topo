{-# LANGUAGE OverloadedStrings #-}

-- | Seed editing helpers for the UI input layer.
module Seer.Input.Seed
  ( bumpSeed
  , handleSeedKey
  , handleSeedTextInput
  , parseSeedText
  ) where

import Actor.UI
  ( Ui
  , UiMenuMode(..)
  , UiState(..)
  , setUiSeed
  , setUiSeedEditing
  , setUiSeedInput
  , setUiMenuMode
  )
import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Read (decimal)
import Hyperspace.Actor (ActorHandle, Protocol)
import qualified SDL

-- | Increment or decrement the seed value and sync the input field.
bumpSeed :: ActorHandle Ui (Protocol Ui) -> IO UiState -> Int -> IO ()
bumpSeed uiHandle getUiSnapshot delta = do
  uiSnap <- getUiSnapshot
  let newSeed = uiSeed uiSnap + fromIntegral delta
  setUiSeed uiHandle newSeed
  setUiSeedInput uiHandle (Text.pack (show newSeed))

-- | Handle key events while editing the seed input.
handleSeedKey :: ActorHandle Ui (Protocol Ui) -> IO UiState -> SDL.Keycode -> IO ()
handleSeedKey uiHandle getUiSnapshot key =
  case key of
    SDL.KeycodeEscape -> do
      cancelSeedEdit uiHandle getUiSnapshot
      uiSnap <- getUiSnapshot
      case uiMenuMode uiSnap of
        MenuNone -> setUiMenuMode uiHandle MenuEscape
        _        -> setUiMenuMode uiHandle MenuNone
    SDL.KeycodeReturn -> commitSeedEdit uiHandle getUiSnapshot
    SDL.KeycodeBackspace -> do
      uiSnap <- getUiSnapshot
      let current = uiSeedInput uiSnap
          trimmed = Text.dropEnd 1 current
      setUiSeedInput uiHandle trimmed
    _ -> pure ()

-- | Handle text input for the seed field.
handleSeedTextInput :: ActorHandle Ui (Protocol Ui) -> IO UiState -> Text -> IO ()
handleSeedTextInput uiHandle getUiSnapshot input = do
  uiSnap <- getUiSnapshot
  let current = uiSeedInput uiSnap
      accepted = Text.filter isSeedDigit input
      next
        | Text.null accepted = current
        | current == "0" = accepted
        | otherwise = current <> accepted
  setUiSeedInput uiHandle next

cancelSeedEdit :: ActorHandle Ui (Protocol Ui) -> IO UiState -> IO ()
cancelSeedEdit uiHandle getUiSnapshot = do
  uiSnap <- getUiSnapshot
  setUiSeedInput uiHandle (Text.pack (show (uiSeed uiSnap)))
  setUiSeedEditing uiHandle False
  SDL.stopTextInput

commitSeedEdit :: ActorHandle Ui (Protocol Ui) -> IO UiState -> IO ()
commitSeedEdit uiHandle getUiSnapshot = do
  uiSnap <- getUiSnapshot
  let current = uiSeedInput uiSnap
      seedValue =
        case parseSeedText current of
          Just n | n >= 0 -> fromIntegral n
          Just n -> fromIntegral (abs n)
          Nothing -> uiSeed uiSnap
  setUiSeed uiHandle seedValue
  setUiSeedInput uiHandle (Text.pack (show seedValue))
  setUiSeedEditing uiHandle False
  SDL.stopTextInput

isSeedDigit :: Char -> Bool
isSeedDigit = isDigit

-- | Parse a seed input string into an integer value.
parseSeedText :: Text -> Maybe Integer
parseSeedText t =
  case Text.uncons t of
    Just ('-', rest) ->
      case decimal rest of
        Right (n, trailing) | Text.null trailing -> Just (negate n)
        _ -> Nothing
    _ ->
      case decimal t of
        Right (n, trailing) | Text.null trailing -> Just n
        _ -> Nothing
