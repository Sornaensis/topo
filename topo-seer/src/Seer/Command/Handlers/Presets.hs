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

import Actor.UiActions.Handles (ActorHandles(..), publishUiMutation)
import Actor.UI.State (readUiSnapshotRef)
import Seer.Command.Context (CommandContext(..))
import Seer.Config.Snapshot
  ( listSnapshots
  , saveNamedSnapshot
  , loadNamedSnapshot
  , snapshotFromUi
  , applySnapshotToUi
  )
import Seer.Persistence.Name (validatePersistenceName)
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
    Just name -> case validatePersistenceName name of
      Left err -> pure $ errResponse reqId ("invalid preset name: " <> err)
      Right () -> do
        ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
        result <- saveNamedSnapshot name (snapshotFromUi ui name)
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
    Just name -> case validatePersistenceName name of
      Left err -> pure $ errResponse reqId ("invalid preset name: " <> err)
      Right () -> do
        result <- loadNamedSnapshot name
        case result of
          Right cs -> do
            let handles = ccActorHandles ctx
            applySnapshotToUi cs (ahUiHandle handles)
            _ <- publishUiMutation handles
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
