{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Seer.Service.Config
  ( ConfigService(..)
  , configServiceGroup
  , configServiceOperationSpecs
  ) where

import Seer.Service.Types

data ConfigService = ConfigService
  { configGetSliders :: !ServiceHandler
  , configGetSlider :: !ServiceHandler
  , configSetSlider :: !ServiceHandler
  , configSetSliders :: !ServiceHandler
  , configResetSliders :: !ServiceHandler
  , configGetSummary :: !ServiceHandler
  , configGetEnums :: !ServiceHandler
  , configListPresets :: !ServiceHandler
  , configSavePreset :: !ServiceHandler
  , configLoadPreset :: !ServiceHandler
  }

configServiceGroup :: ServiceGroupSpec
configServiceGroup = ServiceGroupSpec "config" configServiceOperationSpecs

configServiceOperationSpecs :: [ServiceOperationSpec]
configServiceOperationSpecs =
  [ operationSpec "config.sliders.list" "get_sliders" "List slider definitions and current values."
  , operationSpec "config.sliders.get" "get_slider" "Read a single slider value."
  , operationSpec "config.sliders.set" "set_slider" "Set a single slider value."
  , operationSpec "config.sliders.setMany" "set_sliders" "Set multiple slider values."
  , operationSpec "config.sliders.reset" "reset_sliders" "Reset slider values."
  , operationSpec "config.summary" "get_config_summary" "Read summarized terrain-generation configuration."
  , operationSpec "config.enums" "get_enums" "List command/API enum values."
  , operationSpec "config.presets.list" "list_presets" "List saved configuration presets."
  , operationSpec "config.presets.save" "save_preset" "Save a configuration preset."
  , operationSpec "config.presets.load" "load_preset" "Load a configuration preset."
  ]
