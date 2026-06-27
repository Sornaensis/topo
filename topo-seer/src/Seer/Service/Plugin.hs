{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Seer.Service.Plugin
  ( PluginService(..)
  , PluginListRequest(..)
  , PluginListResponse(..)
  , PluginSummary(..)
  , PluginSetEnabledRequest(..)
  , PluginSetEnabledResponse(..)
  , PluginSetParamRequest(..)
  , PluginSetParamResponse(..)
  , pluginListOperation
  , pluginSetEnabledOperation
  , pluginSetParamOperation
  , pluginServiceGroup
  , pluginServiceOperationSpecs
  ) where

import Actor.PluginManager.Types
  ( PluginDependencyDiagnostic
  , PluginExternalDataSourceDiagnostic
  , PluginLifecycleSnapshot
  )
import Data.Aeson (Value)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Topo.Plugin.DataResource (DataResourceSchema)
import Topo.Plugin.RPC.Manifest (RPCParamSpec)

import Seer.Service.Types

data PluginService = PluginService
  { pluginList :: !ServiceHandler
  , pluginSetEnabled :: !ServiceHandler
  , pluginSetParam :: !ServiceHandler
  }

data PluginListRequest = PluginListRequest
  deriving (Eq, Show)

data PluginSummary = PluginSummary
  { pluginSummaryName :: !Text
  , pluginSummaryStatus :: !Text
  , pluginSummaryDiagnosticStatus :: !Text
  , pluginSummaryStatusDetail :: !Text
  , pluginSummaryLifecycle :: !PluginLifecycleSnapshot
  , pluginSummaryAsyncStatus :: !AsyncStatusSnapshot
  , pluginSummaryPid :: !(Maybe Text)
  , pluginSummaryEndpointKind :: !(Maybe Text)
  , pluginSummaryProtocolVersion :: !(Maybe Int)
  , pluginSummaryUptimeSeconds :: !(Maybe Double)
  , pluginSummaryLastError :: !(Maybe Text)
  , pluginSummaryRestartCount :: !Int
  , pluginSummaryDependencies :: ![PluginDependencyDiagnostic]
  , pluginSummaryResources :: ![Text]
  , pluginSummaryExternalDataSources :: ![PluginExternalDataSourceDiagnostic]
  , pluginSummaryCapabilities :: ![Text]
  , pluginSummaryVersion :: !Text
  , pluginSummaryDescription :: !Text
  , pluginSummaryEnabled :: !Bool
  , pluginSummaryParams :: !(Map Text Value)
  , pluginSummaryParamSpecs :: ![RPCParamSpec]
  , pluginSummaryDataResources :: ![DataResourceSchema]
  , pluginSummaryHasGenerator :: !Bool
  , pluginSummaryHasSimulation :: !Bool
  } deriving (Eq, Show)

newtype PluginListResponse = PluginListResponse
  { pluginSummaries :: [PluginSummary]
  } deriving (Eq, Show)

data PluginSetEnabledRequest = PluginSetEnabledRequest
  { pluginSetEnabledName :: !Text
  , pluginSetEnabledValue :: !Bool
  } deriving (Eq, Show)

data PluginSetEnabledResponse = PluginSetEnabledResponse
  { pluginEnabledName :: !Text
  , pluginEnabledValue :: !Bool
  } deriving (Eq, Show)

data PluginSetParamRequest = PluginSetParamRequest
  { pluginParamPlugin :: !Text
  , pluginParamName :: !Text
  , pluginParamValue :: !Value
  } deriving (Eq, Show)

data PluginSetParamResponse = PluginSetParamResponse
  { pluginUpdatedParamPlugin :: !Text
  , pluginUpdatedParamName :: !Text
  , pluginUpdatedParamValue :: !Value
  } deriving (Eq, Show)

pluginServiceGroup :: ServiceGroupSpec
pluginServiceGroup = ServiceGroupSpec "plugins" pluginServiceOperationSpecs

pluginServiceOperationSpecs :: [ServiceOperationSpec]
pluginServiceOperationSpecs =
  [ typedServiceOperationSpec pluginListOperation
  , typedServiceOperationSpec pluginSetEnabledOperation
  , typedServiceOperationSpec pluginSetParamOperation
  ]

pluginListOperation :: TypedServiceOperation PluginListRequest PluginListResponse
pluginListOperation = typedOperation $
  operationSpec "plugins.list" "list_plugins" "List discovered plugins and status."

pluginSetEnabledOperation :: TypedServiceOperation PluginSetEnabledRequest PluginSetEnabledResponse
pluginSetEnabledOperation = typedOperation $
  operationSpec "plugins.setEnabled" "set_plugin_enabled" "Enable or disable a plugin."

pluginSetParamOperation :: TypedServiceOperation PluginSetParamRequest PluginSetParamResponse
pluginSetParamOperation = typedOperation $
  operationSpec "plugins.params.set" "set_plugin_param" "Set one plugin parameter."
