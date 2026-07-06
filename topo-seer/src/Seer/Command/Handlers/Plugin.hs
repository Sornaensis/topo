{-# LANGUAGE OverloadedStrings #-}

-- | IPC handlers for plugin management: @list_plugins@,
-- @set_plugin_enabled@, @set_plugin_param@.
module Seer.Command.Handlers.Plugin
  ( handleListPlugins
  , handleSetPluginEnabled
  , handleSetPluginParam
  , handleSetPluginParamService
  ) where

import Control.Monad (when)
import Data.Aeson (Value(..), object, (.=), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)

import Actor.PluginManager
  ( LoadedPlugin(..)
  , PluginExternalDataSourceDiagnostic(..)
  , PluginExternalDataSourceGrantDiagnostic(..)
  , PluginLifecycleSnapshot(..)
  , PluginParamUpdateError(..)
  , PluginSimulationPlan(..)
  , getLoadedPlugins
  , setDisabledPlugins
  , getDisabledPlugins
  , getPluginSimulationPlan
  , setPluginParam
  , pluginAvailableDependencyKeys
  , pluginCapabilitiesText
  , pluginDependencyDiagnostics
  , pluginDiagnosticDetail
  , pluginDiagnosticState
  , pluginDiagnosticStateText
  , pluginEndpointKind
  , pluginExternalDataSourceDiagnosticsFor
  , pluginLastError
  , pluginPanelDiagnosticLines
  , pluginResourceNames
  , pluginStatusText
  , pluginUptimeSeconds
  )
import Actor.Simulation (SimulationDagSnapshot(..), getSimDagSnapshot, rebindSimNodes)
import Actor.UI.Setters (setUiDisabledPlugins, setUiPluginDiagnosticLines, setUiPluginDiagnosticStatuses, setUiPluginParam)
import Actor.UI.State (UiState(..), readUiSnapshotRef)
import Actor.UiActions.Handles (ActorHandles(..))
import Seer.Command.Context (CommandContext(..))
import Seer.Service.Types
  ( ServiceError(..)
  , ServiceErrorDetail(..)
  , ServiceResponse(..)
  , ServiceResult
  , serviceErrorText
  )
import Topo.Command.Types (SeerResponse, okResponse, errResponse)
import Topo.Plugin.DataResource (DataFieldDef(..), DataOperations(..), DataPagination(..), DataResourceSchema(..))
import Topo.Plugin.RPC.Manifest
  ( RPCManifest(..)
  , RPCParamSpec(..)
  , RPCParamValidationError(..)
  , RPCSimulationDecl(..)
  )
import Topo.Simulation.Schedule (SimulationScheduleDecl(..), catchUpPolicyText)

-- | Handle @list_plugins@ — return loaded plugins with status and params.
handleListPlugins :: CommandContext -> Int -> Value -> IO SeerResponse
handleListPlugins ctx reqId _params = do
  let handles = ccActorHandles ctx
  ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
  plugins <- getLoadedPlugins (ahPluginManagerHandle handles)
  disabled <- getDisabledPlugins (ahPluginManagerHandle handles)
  now <- getCurrentTime
  let availableDeps = pluginAvailableDependencyKeys disabled plugins
      entries = map (pluginToJSON now disabled plugins availableDeps (uiPluginParamSpecs ui)) plugins
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
          diagnosticLines = Map.fromList [(lpName lp, pluginPanelDiagnosticLines disabled' availableDeps lp) | lp <- loaded]
          diagnosticStatuses = Map.fromList
            [ (lpName lp, pluginDiagnosticStateText (pluginDiagnosticState disabled' availableDeps lp))
            | lp <- loaded
            ]
      setUiPluginDiagnosticLines uiH diagnosticLines
      setUiPluginDiagnosticStatuses uiH diagnosticStatuses
      rebindSimulationForCurrentWorld handles
      pure $ okResponse reqId $ object
        [ "name"    .= name
        , "enabled" .= enabled
        ]

-- | Handle @set_plugin_param@ — set a plugin parameter value.
--
-- Params: @{ "plugin": "my-plugin", "param": "density", "value": 0.5 }@
handleSetPluginParam :: CommandContext -> Int -> Value -> IO SeerResponse
handleSetPluginParam ctx reqId params = do
  result <- handleSetPluginParamService ctx params
  pure $ case result of
    Right response -> okResponse reqId (serviceResponseBody response)
    Left err -> errResponse reqId (serviceErrorText err)

handleSetPluginParamService :: CommandContext -> Value -> IO ServiceResult
handleSetPluginParamService ctx params = do
  case Aeson.parseMaybe parsePluginParam params of
    Nothing ->
      pure $ Left $ ServiceInvalidRequest "missing or invalid 'plugin', 'param', and/or 'value' parameters"
    Just (pluginName, paramName, value) -> do
      let handles = ccActorHandles ctx
      result <- setPluginParam (ahPluginManagerHandle handles) pluginName paramName value
      case result of
        Left err -> pure (Left (pluginParamUpdateServiceError err))
        Right sanitized -> do
          setUiPluginParam (ahUiHandle handles) pluginName paramName sanitized
          rebindSimulationForCurrentWorld handles
          pure $ Right $ ServiceResponse $ object
            [ "plugin" .= pluginName
            , "param"  .= paramName
            , "value"  .= sanitized
            ]

-- --------------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------------

pluginParamUpdateServiceError :: PluginParamUpdateError -> ServiceError
pluginParamUpdateServiceError (PluginParamUnknownPlugin pluginName) =
  ServiceNotFound ("unknown plugin: " <> pluginName)
pluginParamUpdateServiceError (PluginParamValidationFailed err) =
  ServiceValidationError "validation failed"
    [ ServiceErrorDetail
        { serviceErrorDetailPath = rpvPath err
        , serviceErrorDetailCode = rpvCode err
        , serviceErrorDetailMessage = rpvMessage err
        }
    ]

rebindSimulationForCurrentWorld :: ActorHandles -> IO ()
rebindSimulationForCurrentWorld handles = do
  dag <- getSimDagSnapshot (ahSimulationHandle handles)
  when (sdsWorldBound dag) $ do
    simPlan <- getPluginSimulationPlan (ahPluginManagerHandle handles) (Just (sdsOverlayNames dag))
    _ <- rebindSimNodes (ahSimulationHandle handles) (pspExecutableNodes simPlan)
    pure ()

pluginToJSON :: UTCTime -> Set.Set Text -> [LoadedPlugin] -> Set.Set Text -> Map.Map Text [RPCParamSpec] -> LoadedPlugin -> Value
pluginToJSON now disabled allPlugins availableDeps paramSpecs lp =
  let name = lpName lp
      manifest = lpManifest lp
      restartCount = length (lpRestartHistory lp)
      externalDiagnostics = pluginExternalDataSourceDiagnosticsFor disabled allPlugins lp
      diagnosticLines = pluginPanelDiagnosticLines disabled availableDeps lp
  in object
    [ "name"                  .= name
    , "version"               .= rmVersion manifest
    , "description"           .= rmDescription manifest
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
    , "resource_count"        .= length (pluginResourceNames lp)
    , "data_resources"        .= map dataResourceToJSON (rmDataResources manifest)
    , "external_data_sources" .= externalDiagnostics
    , "external_data_source_count" .= length externalDiagnostics
    , "external_data_source_failures" .= externalDiagnosticsFailureCount externalDiagnostics
    , "capabilities"          .= pluginCapabilitiesText manifest
    , "enabled"               .= not (Set.member name disabled)
    , "params"                .= lpParams lp
    , "param_specs"           .= map paramSpecToJSON (maybe [] id (Map.lookup name paramSpecs))
    , "has_generator"         .= isJust (rmGenerator manifest)
    , "has_simulation"        .= isJust (rmSimulation manifest)
    , "has_simulation_declaration" .= isJust (rmSimulation manifest)
    , "simulation"            .= maybe Null simulationDeclToJSON (rmSimulation manifest)
    , "simulation_declaration" .= maybe Null simulationDeclToJSON (rmSimulation manifest)
    , "logs"                  .= diagnosticLines
    , "diagnostic_lines"      .= diagnosticLines
    ]

simulationDeclToJSON :: RPCSimulationDecl -> Value
simulationDeclToJSON sim = object
  [ "dependencies" .= rsdDependencies sim
  , "dependency_kind" .= ("simulation_node_ids" :: Text)
  , "interval_ticks" .= schedDeclIntervalTicks schedule
  , "phase_ticks" .= schedDeclPhaseTicks schedule
  , "catch_up" .= catchUpPolicyText (schedDeclCatchUpPolicy schedule)
  ]
  where
    schedule = rsdSchedule sim

externalDiagnosticsFailureCount :: [PluginExternalDataSourceDiagnostic] -> Int
externalDiagnosticsFailureCount = length . filter externalDiagnosticHasFailure

externalDiagnosticHasFailure :: PluginExternalDataSourceDiagnostic -> Bool
externalDiagnosticHasFailure diagnostic =
  isJust (pedsFailureReason diagnostic)
    || any (isJust . pedsgFailureReason) (pedsGrants diagnostic)

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
