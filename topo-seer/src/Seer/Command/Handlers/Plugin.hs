{-# LANGUAGE OverloadedStrings #-}

-- | IPC handlers for plugin management: @list_plugins@,
-- @set_plugin_enabled@, @set_plugin_param@.
module Seer.Command.Handlers.Plugin
  ( handleListPlugins
  , handleSetPluginEnabled
  , handleSetPluginParam
  ) where

import Data.Aeson (Value(..), object, (.=), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)

import Actor.PluginManager
  ( LoadedPlugin(..)
  , PluginStatus(..)
  , getLoadedPlugins
  , setDisabledPlugins
  , getDisabledPlugins
  , setPluginParam
  )
import Actor.UI.Setters (setUiDisabledPlugins)
import Actor.UI.State (UiState(..), readUiSnapshotRef)
import Actor.UiActions.Handles (ActorHandles(..))
import Seer.Command.Context (CommandContext(..))
import Topo.Command.Types (SeerResponse, okResponse, errResponse)
import Topo.Plugin.RPC.Manifest (RPCParamSpec(..))

-- | Handle @list_plugins@ — return loaded plugins with status and params.
handleListPlugins :: CommandContext -> Int -> Value -> IO SeerResponse
handleListPlugins ctx reqId _params = do
  let handles = ccActorHandles ctx
  ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
  plugins <- getLoadedPlugins (ahPluginManagerHandle handles)
  disabled <- getDisabledPlugins (ahPluginManagerHandle handles)
  let entries = map (pluginToJSON disabled (uiPluginParamSpecs ui)) plugins
  pure $ okResponse reqId $ object
    [ "plugin_count" .= length entries
    , "plugins"      .= entries
    ]

-- | Handle @set_plugin_enabled@ — enable or disable a plugin.
--
-- Params: @{ "name": "my-plugin", "enabled": true }@
handleSetPluginEnabled :: CommandContext -> Int -> Value -> IO SeerResponse
handleSetPluginEnabled ctx reqId params = do
  case Aeson.parseMaybe parsePluginToggle params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'name' and/or 'enabled' parameters"
    Just (name, enabled) -> do
      let handles = ccActorHandles ctx
          pmH = ahPluginManagerHandle handles
          uiH = ahUiHandle handles
      disabled <- getDisabledPlugins pmH
      let disabled'
            | enabled   = Set.delete name disabled
            | otherwise = Set.insert name disabled
      setDisabledPlugins pmH disabled'
      setUiDisabledPlugins uiH disabled'
      pure $ okResponse reqId $ object
        [ "name"    .= name
        , "enabled" .= enabled
        ]

-- | Handle @set_plugin_param@ — set a plugin parameter value.
--
-- Params: @{ "plugin": "my-plugin", "param": "density", "value": 0.5 }@
handleSetPluginParam :: CommandContext -> Int -> Value -> IO SeerResponse
handleSetPluginParam ctx reqId params = do
  case Aeson.parseMaybe parsePluginParam params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'plugin', 'param', and/or 'value' parameters"
    Just (pluginName, paramName, value) -> do
      let handles = ccActorHandles ctx
      setPluginParam (ahPluginManagerHandle handles) pluginName paramName value
      pure $ okResponse reqId $ object
        [ "plugin" .= pluginName
        , "param"  .= paramName
        , "value"  .= value
        ]

-- --------------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------------

pluginToJSON :: Set.Set Text -> Map.Map Text [RPCParamSpec] -> LoadedPlugin -> Value
pluginToJSON disabled paramSpecs lp =
  let name = lpName lp
  in object
    [ "name"        .= name
    , "status"      .= statusToText (lpStatus lp)
    , "enabled"     .= not (Set.member name disabled)
    , "params"      .= lpParams lp
    , "param_specs" .= map paramSpecToJSON (maybe [] id (Map.lookup name paramSpecs))
    ]

statusToText :: PluginStatus -> Text
statusToText PluginIdle         = "idle"
statusToText PluginConnected    = "connected"
statusToText (PluginError err)  = "error: " <> err
statusToText PluginDisconnected = "disconnected"

paramSpecToJSON :: RPCParamSpec -> Value
paramSpecToJSON spec = object
  [ "name"    .= rpsName spec
  , "label"   .= rpsLabel spec
  , "type"    .= show (rpsType spec)
  , "default" .= rpsDefault spec
  , "tooltip" .= rpsTooltip spec
  ]

parsePluginToggle :: Value -> Aeson.Parser (Text, Bool)
parsePluginToggle = Aeson.withObject "set_plugin_enabled" $ \o ->
  (,) <$> o .: "name" <*> o .: "enabled"

parsePluginParam :: Value -> Aeson.Parser (Text, Text, Value)
parsePluginParam = Aeson.withObject "set_plugin_param" $ \o ->
  (,,) <$> o .: "plugin" <*> o .: "param" <*> o .: "value"
