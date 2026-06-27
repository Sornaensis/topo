{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Seer.Service.Config
  ( ConfigService(..)
  , ConfigSlidersRequest(..)
  , ConfigSlidersResponse(..)
  , ConfigSliderRequest(..)
  , ConfigSliderSummary(..)
  , ConfigSetSliderRequest(..)
  , ConfigSetSliderResponse(..)
  , ConfigSetSlidersRequest(..)
  , ConfigSetSlidersResponse(..)
  , ConfigResetSlidersRequest(..)
  , ConfigResetSlidersResponse(..)
  , ConfigSummaryRequest(..)
  , ConfigSummaryResponse(..)
  , ConfigTabSummary(..)
  , ConfigEnumType(..)
  , ConfigEnumsRequest(..)
  , ConfigEnumValue(..)
  , ConfigEnumsResponse(..)
  , ConfigListPresetsRequest(..)
  , ConfigListPresetsResponse(..)
  , ConfigSavePresetRequest(..)
  , ConfigSavePresetResponse(..)
  , ConfigLoadPresetRequest(..)
  , ConfigLoadPresetResponse(..)
  , configGetSlidersOperation
  , configGetSliderOperation
  , configSetSliderOperation
  , configSetSlidersOperation
  , configResetSlidersOperation
  , configGetSummaryOperation
  , configGetEnumsOperation
  , configListPresetsOperation
  , configSavePresetOperation
  , configLoadPresetOperation
  , configServiceGroup
  , configServiceOperationSpecs
  ) where

import Data.Map.Strict (Map)
import Data.Text (Text)

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

newtype ConfigSlidersRequest = ConfigSlidersRequest
  { configSlidersRequestedTab :: Maybe Text
  } deriving (Eq, Show)

newtype ConfigSlidersResponse = ConfigSlidersResponse
  { configSlidersResponseSliders :: [ConfigSliderSummary]
  } deriving (Eq, Show)

newtype ConfigSliderRequest = ConfigSliderRequest
  { configSliderRequestId :: Text
  } deriving (Eq, Show)

data ConfigSliderSummary = ConfigSliderSummary
  { configSliderSummaryId :: !Text
  , configSliderSummaryTab :: !Text
  , configSliderSummaryValue :: !Float
  , configSliderSummaryDomainValue :: !Float
  , configSliderSummaryDomainMin :: !Float
  , configSliderSummaryDomainMax :: !Float
  , configSliderSummaryValueKind :: !Text
  , configSliderSummaryDefault :: !Float
  , configSliderSummaryDefaultDomain :: !Float
  } deriving (Eq, Show)

data ConfigSetSliderRequest = ConfigSetSliderRequest
  { configSetSliderRequestId :: !Text
  , configSetSliderRequestValue :: !Float
  } deriving (Eq, Show)

data ConfigSetSliderResponse = ConfigSetSliderResponse
  { configSetSliderResponseId :: !Text
  , configSetSliderResponseValue :: !Float
  } deriving (Eq, Show)

newtype ConfigSetSlidersRequest = ConfigSetSlidersRequest
  { configSetSlidersRequestValues :: Map Text Float
  } deriving (Eq, Show)

data ConfigSetSlidersResponse = ConfigSetSlidersResponse
  { configSetSlidersUpdated :: ![ConfigSetSliderResponse]
  , configSetSlidersUnknown :: ![Text]
  } deriving (Eq, Show)

newtype ConfigResetSlidersRequest = ConfigResetSlidersRequest
  { configResetSlidersRequestTab :: Maybe Text
  } deriving (Eq, Show)

data ConfigResetSlidersResponse = ConfigResetSlidersResponse
  { configResetSlidersCount :: !Int
  , configResetSlidersTab :: !(Maybe Text)
  } deriving (Eq, Show)

data ConfigSummaryRequest = ConfigSummaryRequest
  deriving (Eq, Show)

data ConfigTabSummary = ConfigTabSummary
  { configTabSummaryTab :: !Text
  , configTabSummarySliders :: ![ConfigSliderSummary]
  } deriving (Eq, Show)

newtype ConfigSummaryResponse = ConfigSummaryResponse
  { configSummaryTabs :: [ConfigTabSummary]
  } deriving (Eq, Show)

data ConfigEnumType
  = ConfigEnumBiome
  | ConfigEnumTerrainForm
  | ConfigEnumWaterBodyType
  | ConfigEnumPlateBoundary
  | ConfigEnumVentType
  | ConfigEnumVentActivity
  | ConfigEnumViewMode
  | ConfigEnumConfigTab
  | ConfigEnumSliderTab
  deriving (Eq, Show)

newtype ConfigEnumsRequest = ConfigEnumsRequest
  { configEnumsRequestType :: ConfigEnumType
  } deriving (Eq, Show)

data ConfigEnumValue = ConfigEnumValue
  { configEnumValueName :: !Text
  , configEnumValueCode :: !Int
  } deriving (Eq, Show)

newtype ConfigEnumsResponse = ConfigEnumsResponse
  { configEnumsValues :: [ConfigEnumValue]
  } deriving (Eq, Show)

data ConfigListPresetsRequest = ConfigListPresetsRequest
  deriving (Eq, Show)

data ConfigListPresetsResponse = ConfigListPresetsResponse
  { configPresetCount :: !Int
  , configPresetNames :: ![Text]
  } deriving (Eq, Show)

newtype ConfigSavePresetRequest = ConfigSavePresetRequest
  { configSavePresetName :: Text
  } deriving (Eq, Show)

data ConfigSavePresetResponse = ConfigSavePresetResponse
  { configSavePresetResponseName :: !Text
  , configSavePresetSaved :: !Bool
  } deriving (Eq, Show)

newtype ConfigLoadPresetRequest = ConfigLoadPresetRequest
  { configLoadPresetName :: Text
  } deriving (Eq, Show)

data ConfigLoadPresetResponse = ConfigLoadPresetResponse
  { configLoadPresetResponseName :: !Text
  , configLoadPresetLoaded :: !Bool
  } deriving (Eq, Show)

configServiceGroup :: ServiceGroupSpec
configServiceGroup = ServiceGroupSpec "config" configServiceOperationSpecs

configServiceOperationSpecs :: [ServiceOperationSpec]
configServiceOperationSpecs =
  [ typedServiceOperationSpec configGetSlidersOperation
  , typedServiceOperationSpec configGetSliderOperation
  , typedServiceOperationSpec configSetSliderOperation
  , typedServiceOperationSpec configSetSlidersOperation
  , typedServiceOperationSpec configResetSlidersOperation
  , typedServiceOperationSpec configGetSummaryOperation
  , typedServiceOperationSpec configGetEnumsOperation
  , typedServiceOperationSpec configListPresetsOperation
  , typedServiceOperationSpec configSavePresetOperation
  , typedServiceOperationSpec configLoadPresetOperation
  ]

configGetSlidersOperation :: TypedServiceOperation ConfigSlidersRequest ConfigSlidersResponse
configGetSlidersOperation = typedOperation $
  operationSpec "config.sliders.list" "get_sliders" "List slider definitions and current values."

configGetSliderOperation :: TypedServiceOperation ConfigSliderRequest ConfigSliderSummary
configGetSliderOperation = typedOperation $
  operationSpec "config.sliders.get" "get_slider" "Read a single slider value."

configSetSliderOperation :: TypedServiceOperation ConfigSetSliderRequest ConfigSetSliderResponse
configSetSliderOperation = typedOperation $
  operationSpec "config.sliders.set" "set_slider" "Set a single slider value."

configSetSlidersOperation :: TypedServiceOperation ConfigSetSlidersRequest ConfigSetSlidersResponse
configSetSlidersOperation = typedOperation $
  operationSpec "config.sliders.setMany" "set_sliders" "Set multiple slider values."

configResetSlidersOperation :: TypedServiceOperation ConfigResetSlidersRequest ConfigResetSlidersResponse
configResetSlidersOperation = typedOperation $
  operationSpec "config.sliders.reset" "reset_sliders" "Reset slider values."

configGetSummaryOperation :: TypedServiceOperation ConfigSummaryRequest ConfigSummaryResponse
configGetSummaryOperation = typedOperation $
  operationSpec "config.summary" "get_config_summary" "Read summarized terrain-generation configuration."

configGetEnumsOperation :: TypedServiceOperation ConfigEnumsRequest ConfigEnumsResponse
configGetEnumsOperation = typedOperation $
  operationSpec "config.enums" "get_enums" "List enum values for HTTP and internal command compatibility."

configListPresetsOperation :: TypedServiceOperation ConfigListPresetsRequest ConfigListPresetsResponse
configListPresetsOperation = typedOperation $
  operationSpec "config.presets.list" "list_presets" "List saved configuration presets."

configSavePresetOperation :: TypedServiceOperation ConfigSavePresetRequest ConfigSavePresetResponse
configSavePresetOperation = typedOperation $
  operationSpec "config.presets.save" "save_preset" "Save a configuration preset."

configLoadPresetOperation :: TypedServiceOperation ConfigLoadPresetRequest ConfigLoadPresetResponse
configLoadPresetOperation = typedOperation $
  operationSpec "config.presets.load" "load_preset" "Load a configuration preset."
