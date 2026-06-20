{-# LANGUAGE OverloadedStrings #-}

-- | HTTP API schema metadata for resource-oriented routes.
--
-- The WAI server currently routes through a compact route table, while this
-- module keeps request/response schema names next to the public HTTP contract
-- so generated OpenAPI is not just a list of paths.
module Seer.HTTP.API
  ( -- * Presets
    presetsListResponseSchema
  , presetsSaveRequestSchema
  , presetsSaveResponseSchema
  , presetsLoadRequestSchema
  , presetsLoadResponseSchema
    -- * Pipeline
  , pipelineGetResponseSchema
  , pipelineSetStageEnabledRequestSchema
  , pipelineSetStageEnabledResponseSchema
    -- * Plugins
  , pluginListResponseSchema
  , pluginSetEnabledRequestSchema
  , pluginSetEnabledResponseSchema
  , pluginSetParamRequestSchema
  , pluginSetParamResponseSchema
    -- * Data resources
  , dataPluginsListResponseSchema
  , dataResourcesListResponseSchema
  , dataRecordsListResponseSchema
  , dataRecordGetRequestSchema
  , dataRecordGetResponseSchema
  , dataRecordCreateRequestSchema
  , dataRecordCreateResponseSchema
  , dataRecordUpdateRequestSchema
  , dataRecordUpdateResponseSchema
  , dataRecordDeleteRequestSchema
  , dataRecordDeleteResponseSchema
  , dataStateResponseSchema
    -- * Simulation
  , simulationStateResponseSchema
  , simulationDagResponseSchema
  , simulationAutoTickRequestSchema
  , simulationAutoTickResponseSchema
  , simulationTickRequestSchema
  , simulationTickResponseSchema
    -- * Logs and screenshots
  , logGetResponseSchema
  , screenshotTakeRequestSchema
  , screenshotTakeResponseSchema
  ) where

import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)

import Seer.HTTP.OpenAPI (JsonSchema(..))

-- Presets -------------------------------------------------------------------

presetsListResponseSchema :: JsonSchema
presetsListResponseSchema = objectSchema "PresetsListResponse"
  [ "preset_count", "presets" ]
  [ ("preset_count", integerSchema)
  , ("presets", arraySchema stringSchema)
  ]

presetsSaveRequestSchema :: JsonSchema
presetsSaveRequestSchema = nameRequestSchema "PresetsSaveRequest"

presetsSaveResponseSchema :: JsonSchema
presetsSaveResponseSchema = objectSchema "PresetsSaveResponse"
  [ "name", "saved" ]
  [ ("name", stringSchema)
  , ("saved", booleanSchema)
  ]

presetsLoadRequestSchema :: JsonSchema
presetsLoadRequestSchema = nameRequestSchema "PresetsLoadRequest"

presetsLoadResponseSchema :: JsonSchema
presetsLoadResponseSchema = objectSchema "PresetsLoadResponse"
  [ "name", "loaded" ]
  [ ("name", stringSchema)
  , ("loaded", booleanSchema)
  ]

-- Pipeline ------------------------------------------------------------------

pipelineGetResponseSchema :: JsonSchema
pipelineGetResponseSchema = objectSchema "PipelineGetResponse"
  [ "stages" ]
  [ ("stages", arraySchema pipelineStageSchema)
  ]

pipelineSetStageEnabledRequestSchema :: JsonSchema
pipelineSetStageEnabledRequestSchema = objectSchema "PipelineSetStageEnabledRequest"
  [ "stage", "enabled" ]
  [ ("stage", stringSchema)
  , ("enabled", booleanSchema)
  ]

pipelineSetStageEnabledResponseSchema :: JsonSchema
pipelineSetStageEnabledResponseSchema = objectSchema "PipelineSetStageEnabledResponse"
  [ "stage", "enabled" ]
  [ ("stage", stringSchema)
  , ("enabled", booleanSchema)
  ]

pipelineStageSchema :: Value
pipelineStageSchema = inlineObjectSchema
  [ "id", "enabled", "source" ]
  [ ("id", stringSchema)
  , ("name", stringSchema)
  , ("enabled", booleanSchema)
  , ("source", enumStringSchema ["builtin", "plugin"])
  ]

-- Plugins -------------------------------------------------------------------

pluginListResponseSchema :: JsonSchema
pluginListResponseSchema = objectSchema "PluginListResponse"
  [ "plugin_count", "plugins" ]
  [ ("plugin_count", integerSchema)
  , ("plugins", arraySchema pluginSummarySchema)
  ]

pluginSetEnabledRequestSchema :: JsonSchema
pluginSetEnabledRequestSchema = objectSchema "PluginSetEnabledRequest"
  [ "name", "enabled" ]
  [ ("name", stringSchema)
  , ("enabled", booleanSchema)
  ]

pluginSetEnabledResponseSchema :: JsonSchema
pluginSetEnabledResponseSchema = objectSchema "PluginSetEnabledResponse"
  [ "name", "enabled" ]
  [ ("name", stringSchema)
  , ("enabled", booleanSchema)
  ]

pluginSetParamRequestSchema :: JsonSchema
pluginSetParamRequestSchema = objectSchema "PluginSetParamRequest"
  [ "plugin", "param", "value" ]
  [ ("plugin", stringSchema)
  , ("param", stringSchema)
  , ("value", anySchema)
  ]

pluginSetParamResponseSchema :: JsonSchema
pluginSetParamResponseSchema = objectSchema "PluginSetParamResponse"
  [ "plugin", "param", "value" ]
  [ ("plugin", stringSchema)
  , ("param", stringSchema)
  , ("value", anySchema)
  ]

pluginSummarySchema :: Value
pluginSummarySchema = inlineObjectSchema
  [ "name", "status", "enabled", "params", "param_specs" ]
  [ ("name", stringSchema)
  , ("status", stringSchema)
  , ("enabled", booleanSchema)
  , ("params", freeObjectSchema)
  , ("param_specs", arraySchema pluginParamSpecSchema)
  ]

pluginParamSpecSchema :: Value
pluginParamSpecSchema = inlineObjectSchema
  [ "name", "label", "type", "default", "tooltip" ]
  [ ("name", stringSchema)
  , ("label", stringSchema)
  , ("type", stringSchema)
  , ("default", anySchema)
  , ("tooltip", nullableSchema stringSchema)
  ]

-- Data resources -------------------------------------------------------------

dataPluginsListResponseSchema :: JsonSchema
dataPluginsListResponseSchema = objectSchema "DataPluginsListResponse"
  [ "plugins", "count" ]
  [ ("plugins", arraySchema dataPluginSummarySchema)
  , ("count", integerSchema)
  ]

dataResourcesListResponseSchema :: JsonSchema
dataResourcesListResponseSchema = objectSchema "DataResourcesListResponse"
  [ "plugin", "resources" ]
  [ ("plugin", stringSchema)
  , ("resources", arraySchema dataResourceSchema)
  ]

dataRecordsListResponseSchema :: JsonSchema
dataRecordsListResponseSchema = objectSchema "DataRecordsListResponse"
  [ "plugin", "resource", "records", "total_count", "count" ]
  [ ("plugin", stringSchema)
  , ("resource", stringSchema)
  , ("records", arraySchema freeObjectSchema)
  , ("total_count", nullableSchema integerSchema)
  , ("count", integerSchema)
  ]

dataRecordGetRequestSchema :: JsonSchema
dataRecordGetRequestSchema = dataRecordKeyRequestSchema "DataRecordGetRequest"

dataRecordGetResponseSchema :: JsonSchema
dataRecordGetResponseSchema = objectSchema "DataRecordGetResponse"
  [ "plugin", "resource" ]
  [ ("plugin", stringSchema)
  , ("resource", stringSchema)
  , ("record", freeObjectSchema)
  , ("records", arraySchema freeObjectSchema)
  , ("count", integerSchema)
  ]

dataRecordCreateRequestSchema :: JsonSchema
dataRecordCreateRequestSchema = dataRecordFieldsRequestSchema "DataRecordCreateRequest" False

dataRecordCreateResponseSchema :: JsonSchema
dataRecordCreateResponseSchema = objectSchema "DataRecordCreateResponse"
  [ "plugin", "resource", "created" ]
  [ ("plugin", stringSchema)
  , ("resource", stringSchema)
  , ("created", booleanSchema)
  , ("record", nullableSchema freeObjectSchema)
  ]

dataRecordUpdateRequestSchema :: JsonSchema
dataRecordUpdateRequestSchema = dataRecordFieldsRequestSchema "DataRecordUpdateRequest" True

dataRecordUpdateResponseSchema :: JsonSchema
dataRecordUpdateResponseSchema = objectSchema "DataRecordUpdateResponse"
  [ "plugin", "resource", "updated" ]
  [ ("plugin", stringSchema)
  , ("resource", stringSchema)
  , ("updated", booleanSchema)
  , ("record", nullableSchema freeObjectSchema)
  ]

dataRecordDeleteRequestSchema :: JsonSchema
dataRecordDeleteRequestSchema = dataRecordKeyRequestSchema "DataRecordDeleteRequest"

dataRecordDeleteResponseSchema :: JsonSchema
dataRecordDeleteResponseSchema = objectSchema "DataRecordDeleteResponse"
  [ "plugin", "resource", "deleted" ]
  [ ("plugin", stringSchema)
  , ("resource", stringSchema)
  , ("deleted", booleanSchema)
  ]

dataStateResponseSchema :: JsonSchema
dataStateResponseSchema = objectSchema "DataStateResponse"
  [ "record_count", "total_count", "page_offset", "loading", "edit_mode", "create_mode", "has_selection" ]
  [ ("selected_plugin", nullableSchema stringSchema)
  , ("selected_resource", nullableSchema stringSchema)
  , ("record_count", integerSchema)
  , ("total_count", nullableSchema integerSchema)
  , ("page_offset", integerSchema)
  , ("loading", booleanSchema)
  , ("edit_mode", booleanSchema)
  , ("create_mode", booleanSchema)
  , ("has_selection", booleanSchema)
  , ("selected_key", anySchema)
  ]

dataPluginSummarySchema :: Value
dataPluginSummarySchema = inlineObjectSchema
  [ "plugin", "resources" ]
  [ ("plugin", stringSchema)
  , ("resources", arraySchema stringSchema)
  ]

dataResourceSchema :: Value
dataResourceSchema = inlineObjectSchema
  [ "name", "label", "hex_bound", "key_field", "fields", "operations" ]
  [ ("name", stringSchema)
  , ("label", stringSchema)
  , ("hex_bound", booleanSchema)
  , ("key_field", stringSchema)
  , ("overlay", nullableSchema stringSchema)
  , ("fields", arraySchema dataFieldSchema)
  , ("operations", dataOperationsSchema)
  ]

dataFieldSchema :: Value
dataFieldSchema = inlineObjectSchema
  [ "name", "type", "label", "editable", "default" ]
  [ ("name", stringSchema)
  , ("type", dataFieldTypeSchema)
  , ("label", stringSchema)
  , ("editable", booleanSchema)
  , ("default", anySchema)
  ]

dataFieldTypeSchema :: Value
dataFieldTypeSchema = object
  [ "description" .= ("Scalar field type string or composite field type object." :: Text)
  , "oneOf" .=
      [ enumStringSchema ["text", "int", "float", "double", "bool", "fixed2", "fixed3", "fixed4"]
      , freeObjectSchema
      ]
  ]

dataOperationsSchema :: Value
dataOperationsSchema = inlineObjectSchema
  [ "list", "get", "create", "update", "delete", "query_by_hex" ]
  [ ("list", booleanSchema)
  , ("get", booleanSchema)
  , ("create", booleanSchema)
  , ("update", booleanSchema)
  , ("delete", booleanSchema)
  , ("query_by_hex", booleanSchema)
  ]

dataRecordKeyRequestSchema :: Text -> JsonSchema
dataRecordKeyRequestSchema name = objectSchema name
  [ "plugin", "resource", "key" ]
  [ ("plugin", stringSchema)
  , ("resource", stringSchema)
  , ("key", anySchema)
  ]

dataRecordFieldsRequestSchema :: Text -> Bool -> JsonSchema
dataRecordFieldsRequestSchema name includeKey = objectSchema name required properties
  where
    required
      | includeKey = ["plugin", "resource", "key", "fields"]
      | otherwise = ["plugin", "resource", "fields"]
    properties =
      [ ("plugin", stringSchema)
      , ("resource", stringSchema)
      ]
      <> [ ("key", anySchema) | includeKey ]
      <> [ ("fields", freeObjectSchema) ]

-- Simulation ----------------------------------------------------------------

simulationStateResponseSchema :: JsonSchema
simulationStateResponseSchema = objectSchema "SimulationStateResponse"
  [ "auto_tick", "tick_rate", "tick_count" ]
  [ ("auto_tick", booleanSchema)
  , ("tick_rate", numberSchema)
  , ("tick_count", integerSchema)
  ]

simulationDagResponseSchema :: JsonSchema
simulationDagResponseSchema = objectSchema "SimulationDagResponse"
  [ "available", "nodes", "levels", "terrain_writers" ]
  [ ("available", booleanSchema)
  , ("nodes", arraySchema simulationDagNodeSchema)
  , ("levels", arraySchema (arraySchema stringSchema))
  , ("terrain_writers", arraySchema stringSchema)
  ]

simulationAutoTickRequestSchema :: JsonSchema
simulationAutoTickRequestSchema = objectSchema "SimulationAutoTickRequest"
  [ "enabled" ]
  [ ("enabled", booleanSchema)
  , ("rate", numberSchema)
  ]

simulationAutoTickResponseSchema :: JsonSchema
simulationAutoTickResponseSchema = objectSchema "SimulationAutoTickResponse"
  [ "auto_tick" ]
  [ ("auto_tick", booleanSchema)
  , ("rate", nullableSchema numberSchema)
  ]

simulationTickRequestSchema :: JsonSchema
simulationTickRequestSchema = objectSchema "SimulationTickRequest"
  []
  [ ("count", integerSchema)
  ]

simulationTickResponseSchema :: JsonSchema
simulationTickResponseSchema = objectSchema "SimulationTickResponse"
  [ "requested_ticks", "target_tick" ]
  [ ("requested_ticks", integerSchema)
  , ("target_tick", integerSchema)
  ]

simulationDagNodeSchema :: Value
simulationDagNodeSchema = inlineObjectSchema
  [ "id", "overlay", "dependencies", "writes_terrain" ]
  [ ("id", stringSchema)
  , ("overlay", stringSchema)
  , ("dependencies", arraySchema stringSchema)
  , ("writes_terrain", booleanSchema)
  ]

-- Logs and screenshots -------------------------------------------------------

logGetResponseSchema :: JsonSchema
logGetResponseSchema = objectSchema "LogGetResponse"
  [ "count", "total", "entries" ]
  [ ("count", integerSchema)
  , ("total", integerSchema)
  , ("entries", arraySchema logEntrySchema)
  ]

screenshotTakeRequestSchema :: JsonSchema
screenshotTakeRequestSchema = objectSchema "ScreenshotTakeRequest"
  []
  [ ("path", stringSchema)
  ]

screenshotTakeResponseSchema :: JsonSchema
screenshotTakeResponseSchema = objectSchema "ScreenshotTakeResponse"
  [ "image_base64", "format" ]
  [ ("image_base64", stringSchema)
  , ("format", enumStringSchema ["png"])
  , ("source", stringSchema)
  , ("saved_path", stringSchema)
  ]

logEntrySchema :: Value
logEntrySchema = inlineObjectSchema
  [ "level", "message" ]
  [ ("level", enumStringSchema ["debug", "info", "warn", "error"])
  , ("message", stringSchema)
  ]

-- Shared schema helpers ------------------------------------------------------

nameRequestSchema :: Text -> JsonSchema
nameRequestSchema name = objectSchema name
  [ "name" ]
  [ ("name", stringSchema)
  ]

objectSchema :: Text -> [Text] -> [(Text, Value)] -> JsonSchema
objectSchema name required properties = JsonSchema name (inlineObjectSchema required properties)

inlineObjectSchema :: [Text] -> [(Text, Value)] -> Value
inlineObjectSchema required properties = object $
  [ "type" .= ("object" :: Text)
  , "properties" .= propertiesObject properties
  ]
  <> [ "required" .= required | not (null required) ]

propertiesObject :: [(Text, Value)] -> Value
propertiesObject properties = object
  [ Key.fromText key .= schema
  | (key, schema) <- properties
  ]

arraySchema :: Value -> Value
arraySchema itemSchema = object
  [ "type" .= ("array" :: Text)
  , "items" .= itemSchema
  ]

nullableSchema :: Value -> Value
nullableSchema (Object schema) = Object (KM.insert "nullable" (Bool True) schema)
nullableSchema schema = object
  [ "nullable" .= True
  , "allOf" .= [schema]
  ]

stringSchema :: Value
stringSchema = object ["type" .= ("string" :: Text)]

integerSchema :: Value
integerSchema = object ["type" .= ("integer" :: Text)]

numberSchema :: Value
numberSchema = object ["type" .= ("number" :: Text)]

booleanSchema :: Value
booleanSchema = object ["type" .= ("boolean" :: Text)]

freeObjectSchema :: Value
freeObjectSchema = object
  [ "type" .= ("object" :: Text)
  , "additionalProperties" .= True
  ]

anySchema :: Value
anySchema = object []

enumStringSchema :: [Text] -> Value
enumStringSchema values = object
  [ "type" .= ("string" :: Text)
  , "enum" .= values
  ]
