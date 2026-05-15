{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Seer.Service.Plugin
  ( PluginService(..)
  , pluginServiceGroup
  , pluginServiceOperationSpecs
  ) where

import Seer.Service.Types

data PluginService = PluginService
  { pluginList :: !ServiceHandler
  , pluginSetEnabled :: !ServiceHandler
  , pluginSetParam :: !ServiceHandler
  }

pluginServiceGroup :: ServiceGroupSpec
pluginServiceGroup = ServiceGroupSpec "plugins" pluginServiceOperationSpecs

pluginServiceOperationSpecs :: [ServiceOperationSpec]
pluginServiceOperationSpecs =
  [ operationSpec "plugins.list" "list_plugins" "List discovered plugins and status."
  , operationSpec "plugins.setEnabled" "set_plugin_enabled" "Enable or disable a plugin."
  , operationSpec "plugins.params.set" "set_plugin_param" "Set one plugin parameter."
  ]
