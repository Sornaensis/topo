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
import Data.Time (UTCTime, getCurrentTime)

import Actor.PluginManager
  ( LoadedPlugin(..)
  , PluginLifecycleSnapshot(..)
  , getLoadedPlugins
  , setDisabledPlugins
  , getDisabledPlugins
  , setPluginParam
  , pluginAvailableDependencyKeys
  , pluginCapabilitiesText
  , pluginDependencyDiagnostics
  , pluginDiagnosticDetail
  , pluginDiagnosticState
  , pluginDiagnosticStateText
  , pluginEndpointKind
  , pluginExternalDataSourceDiagnostics
  , pluginLastError
  , pluginPanelDiagnosticLines
  , pluginResourceNames
  , pluginStatusText
  , pluginUptimeSeconds
  )
import Actor.UI.Setters (setUiDisabledPlugins, setUiPluginDiagnosticLines, setUiPluginDiagnosticStatuses, setUiPluginParam)
import Actor.UI.State (UiState(..), readUiSnapshotRef)
import Actor.UiActions.Handles (ActorHandles(..))
import Seer.Command.Context (CommandContext(..))
import Topo.Command.Types (SeerResponse, okResponse, errResponse)
import Topo.Plugin.DataResource (DataFieldDef(..), DataOperations(..), DataPagination(..), DataResourceSchema(..))
import Topo.Plugin.RPC.Manifest (RPCManifest(..), RPCParamSpec(..))

-- | Handle @list_plugins@ — return loaded plugins with status and params.
handleListPlugins :: CommandContext -> Int -> Value -> IO SeerResponse
handleListPlugins ctx reqId _params = do
  let handles = ccActorHandles ctx
  ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
  plugins <- getLoadedPlugins (ahPluginManagerHandle handles)
  disabled <- getDisabledPlugins (ahPluginManagerHandle handles)
  now <- getCurrentTime
  let availableDeps = pluginAvailableDependencyKeys disabled plugins
      entries = map (pluginToJSON now disabled availableDeps (uiPluginParamSpecs ui)) plugins
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
      loaded <- getLoadedPlugins pmH
      let availableDeps = pluginAvailableDependencyKeys disabled' loaded
          diagnosticLines = Map.fromList [(lpName lp, pluginPanelDiagnosticLines availableDeps lp) | lp <- loaded]
          diagnosticStatuses = Map.fromList
            [ (lpName lp, pluginDiagnosticStateText (pluginDiagnosticState disabled' availableDeps lp))
            | lp <- loaded
            ]
      setUiPluginDiagnosticLines uiH diagnosticLines
      setUiPluginDiagnosticStatuses uiH diagnosticStatuses
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
      setUiPluginParam (ahUiHandle handles) pluginName paramName value
      pure $ okResponse reqId $ object
        [ "plugin" .= pluginName
        , "param"  .= paramName
        , "value"  .= value
        ]

-- --------------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------------

pluginToJSON :: UTCTime -> Set.Set Text -> Set.Set Text -> Map.Map Text [RPCParamSpec] -> LoadedPlugin -> Value
pluginToJSON now disabled availableDeps paramSpecs lp =
  let name = lpName lp
      manifest = lpManifest lp
      restartCount = length (lpRestartHistory lp)
  in object
    [ "name"                  .= name
    , "status"                .= pluginStatusText (lpStatus lp)
    , "diagnostic_status"     .= pluginDiagnosticState disabled availableDeps lp
    , "status_detail"         .= pluginDiagnosticDetail disabled availableDeps lp
    , "lifecycle"             .= lpLifecycle lp
    , "pid"                   .= plsProcessId (lpLifecycle lp)
    , "endpoint_kind"         .= pluginEndpointKind lp
    , "protocol_version"      .= plsProtocolVersion (lpLifecycle lp)
    , "uptime_seconds"        .= pluginUptimeSeconds now lp
    , "last_error"            .= pluginLastError lp
    , "start_policy"          .= lpStartPolicy lp
    , "restart_attempts"      .= restartCount
    , "restart_count"         .= restartCount
    , "dependencies"          .= pluginDependencyDiagnostics availableDeps lp
    , "resources"             .= pluginResourceNames lp
    , "data_resources"        .= map dataResourceToJSON (rmDataResources manifest)
    , "external_data_sources" .= pluginExternalDataSourceDiagnostics lp
    , "capabilities"          .= pluginCapabilitiesText manifest
    , "enabled"               .= not (Set.member name disabled)
    , "params"                .= lpParams lp
    , "param_specs"           .= map paramSpecToJSON (maybe [] id (Map.lookup name paramSpecs))
    ]

dataResourceToJSON :: DataResourceSchema -> Value
dataResourceToJSON drs = object
  [ "schema_version"   .= drsSchemaVersion drs
  , "resource_version" .= drsResourceVersion drs
  , "name"             .= drsName drs
  , "label"            .= drsLabel drs
  , "hex_bound"        .= drsHexBound drs
  , "key_field"        .= drsKeyField drs
  , "overlay"          .= drsOverlay drs
  , "fields"           .= map dataFieldToJSON (drsFields drs)
  , "operations"       .= dataOperationsToJSON (drsOperations drs)
  , "pagination"       .= dataPaginationToJSON (drsPagination drs)
  ]

dataFieldToJSON :: DataFieldDef -> Value
dataFieldToJSON field = object
  [ "name"     .= dfName field
  , "type"     .= dfType field
  , "label"    .= dfLabel field
  , "editable" .= dfEditable field
  , "default"  .= dfDefault field
  ]

dataOperationsToJSON :: DataOperations -> Value
dataOperationsToJSON ops = object
  [ "list"           .= doList ops
  , "get"            .= doGet ops
  , "create"         .= doCreate ops
  , "update"         .= doUpdate ops
  , "delete"         .= doDelete ops
  , "query_by_hex"   .= doQueryByHex ops
  , "query_by_field" .= doQueryByField ops
  , "sort"           .= doSort ops
  , "filter"         .= doFilter ops
  , "page"           .= doPage ops
  ]

dataPaginationToJSON :: DataPagination -> Value
dataPaginationToJSON pagination = object
  [ "default_page_size" .= dpDefaultPageSize pagination
  , "max_page_size" .= dpMaxPageSize pagination
  , "default_page_offset" .= dpDefaultPageOffset pagination
  ]

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
