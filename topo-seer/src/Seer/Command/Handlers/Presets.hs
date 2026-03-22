{-# LANGUAGE OverloadedStrings #-}

-- | Handlers for preset commands: @list_presets@, @save_preset@, @load_preset@.
module Seer.Command.Handlers.Presets
  ( handleListPresets
  , handleSavePreset
  , handleLoadPreset
  ) where

import Data.Aeson (Value(..), object, (.=), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath ((</>))

import Actor.UiActions.Handles (ActorHandles(..))
import Actor.UI.State (readUiSnapshotRef)
import Seer.Command.Context (CommandContext(..))
import Seer.Config.Snapshot
  ( listSnapshots
  , snapshotDir
  , saveSnapshot
  , loadSnapshot
  , snapshotFromUi
  , applySnapshotToUi
  )
import Topo.Command.Types (SeerResponse, okResponse, errResponse)

-- | Handle @list_presets@ — return all saved config preset names.
handleListPresets :: CommandContext -> Int -> Value -> IO SeerResponse
handleListPresets _ctx reqId _params = do
  names <- listSnapshots
  pure $ okResponse reqId $ object
    [ "preset_count" .= length names
    , "presets"       .= names
    ]

-- | Handle @save_preset@ — save current UI state as a named preset.
-- Params: @{ "name": "my-preset" }@
handleSavePreset :: CommandContext -> Int -> Value -> IO SeerResponse
handleSavePreset ctx reqId params = do
  case Aeson.parseMaybe parseName params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'name' parameter"
    Just name
      | Text.null name ->
          pure $ errResponse reqId "preset name must not be empty"
      | otherwise -> do
          ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
          dir <- snapshotDir
          let path = dir </> Text.unpack name <> ".json"
          result <- saveSnapshot path (snapshotFromUi ui name)
          case result of
            Right () ->
              pure $ okResponse reqId $ object
                [ "name" .= name
                , "saved" .= True
                ]
            Left err ->
              pure $ errResponse reqId ("failed to save preset: " <> err)

-- | Handle @load_preset@ — load a named preset and apply it to the UI.
-- Params: @{ "name": "my-preset" }@
handleLoadPreset :: CommandContext -> Int -> Value -> IO SeerResponse
handleLoadPreset ctx reqId params = do
  case Aeson.parseMaybe parseName params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'name' parameter"
    Just name
      | Text.null name ->
          pure $ errResponse reqId "preset name must not be empty"
      | otherwise -> do
          dir <- snapshotDir
          let path = dir </> Text.unpack name <> ".json"
          result <- loadSnapshot path
          case result of
            Right cs -> do
              applySnapshotToUi cs (ahUiHandle (ccActorHandles ctx))
              pure $ okResponse reqId $ object
                [ "name" .= name
                , "loaded" .= True
                ]
            Left err ->
              pure $ errResponse reqId ("failed to load preset: " <> err)

-- --------------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------------

parseName :: Value -> Aeson.Parser Text
parseName = Aeson.withObject "params" (.: "name")
