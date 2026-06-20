{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Seer.Service.Validation
  ( RequestValidator
  , FieldSpec
  , FieldValueKind(..)
  , appServiceRequestValidators
  , serviceRequestBodyValue
  , validateAppServiceRequest
  , validateServiceHandler
  , validateFields
  , validateObjectFields
  , requiredText
  , requiredBool
  , requiredNumber
  , requiredInt
  , requiredWord64
  , requiredObject
  , requiredArray
  , requiredAny
  , optionalText
  , optionalBool
  , optionalNumber
  , optionalInt
  , optionalWord64
  , optionalObject
  , optionalArray
  , optionalAny
  ) where

import Data.Aeson (Value(..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as Aeson
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Word (Word64)

import Seer.Service.Types

type RequestValidator = Value -> Either ServiceError ()

data FieldValueKind
  = FieldAny
  | FieldText
  | FieldBool
  | FieldNumber
  | FieldInt
  | FieldWord64
  | FieldObject
  | FieldArray
  deriving (Eq, Show)

data FieldSpec = FieldSpec
  { fieldSpecName :: !Text
  , fieldSpecKind :: !FieldValueKind
  , fieldSpecRequired :: !Bool
  } deriving (Eq, Show)

validateFields :: Text -> [FieldSpec] -> RequestValidator
validateFields = validateFieldsWith False

validateObjectFields :: Text -> [FieldSpec] -> RequestValidator
validateObjectFields = validateFieldsWith True

validateFieldsWith :: Bool -> Text -> [FieldSpec] -> RequestValidator
validateFieldsWith requireObjectBody label specs = \case
  Object fields -> finish (concatMap (validateField fields) specs)
  Null
    | requireObjectBody -> Left (validationError [invalidBody label])
  Null
    | null requiredSpecs -> Right ()
    | otherwise -> finish (map (missingField . fieldSpecName) requiredSpecs)
  _ -> Left (validationError [invalidBody label])
  where
    requiredSpecs = filter fieldSpecRequired specs

invalidBody :: Text -> ServiceErrorDetail
invalidBody label = ServiceErrorDetail
  { serviceErrorDetailPath = []
  , serviceErrorDetailCode = "invalid_body"
  , serviceErrorDetailMessage = label <> " request body must be an object"
  }

finish :: [ServiceErrorDetail] -> Either ServiceError ()
finish [] = Right ()
finish details = Left (validationError details)

validateField :: KM.KeyMap Value -> FieldSpec -> [ServiceErrorDetail]
validateField fields spec =
  case KM.lookup (Key.fromText (fieldSpecName spec)) fields of
    Nothing
      | fieldSpecRequired spec -> [missingField (fieldSpecName spec)]
      | otherwise -> []
    Just value
      | fieldMatches (fieldSpecKind spec) value -> []
      | otherwise -> [invalidField (fieldSpecName spec) (fieldKindDescription (fieldSpecKind spec))]

fieldMatches :: FieldValueKind -> Value -> Bool
fieldMatches FieldAny _ = True
fieldMatches FieldText (String _) = True
fieldMatches FieldText _ = False
fieldMatches FieldBool (Bool _) = True
fieldMatches FieldBool _ = False
fieldMatches FieldNumber (Number _) = True
fieldMatches FieldNumber _ = False
fieldMatches FieldInt value = isJust (Aeson.parseMaybe Aeson.parseJSON value :: Maybe Int)
fieldMatches FieldWord64 value = isJust (Aeson.parseMaybe Aeson.parseJSON value :: Maybe Word64)
fieldMatches FieldObject (Object _) = True
fieldMatches FieldObject _ = False
fieldMatches FieldArray (Array _) = True
fieldMatches FieldArray _ = False

fieldKindDescription :: FieldValueKind -> Text
fieldKindDescription FieldAny = "any value"
fieldKindDescription FieldText = "text"
fieldKindDescription FieldBool = "boolean"
fieldKindDescription FieldNumber = "number"
fieldKindDescription FieldInt = "integer"
fieldKindDescription FieldWord64 = "non-negative integer"
fieldKindDescription FieldObject = "object"
fieldKindDescription FieldArray = "array"

required :: Text -> FieldValueKind -> FieldSpec
required name kind = FieldSpec name kind True

optional :: Text -> FieldValueKind -> FieldSpec
optional name kind = FieldSpec name kind False

requiredText, requiredBool, requiredNumber, requiredInt, requiredWord64, requiredObject, requiredArray, requiredAny :: Text -> FieldSpec
requiredText name = required name FieldText
requiredBool name = required name FieldBool
requiredNumber name = required name FieldNumber
requiredInt name = required name FieldInt
requiredWord64 name = required name FieldWord64
requiredObject name = required name FieldObject
requiredArray name = required name FieldArray
requiredAny name = required name FieldAny

optionalText, optionalBool, optionalNumber, optionalInt, optionalWord64, optionalObject, optionalArray, optionalAny :: Text -> FieldSpec
optionalText name = optional name FieldText
optionalBool name = optional name FieldBool
optionalNumber name = optional name FieldNumber
optionalInt name = optional name FieldInt
optionalWord64 name = optional name FieldWord64
optionalObject name = optional name FieldObject
optionalArray name = optional name FieldArray
optionalAny name = optional name FieldAny

serviceRequestBodyValue :: ServiceRequest -> Value
serviceRequestBodyValue = fromMaybe Null . serviceRequestBody

validateAppServiceRequest :: Text -> Value -> Either ServiceError ()
validateAppServiceRequest method params =
  maybe (Right ()) ($ params) (lookup method appServiceRequestValidators)

validateServiceHandler :: Text -> ServiceHandler -> ServiceHandler
validateServiceHandler method handler ctx request = do
  let params = serviceRequestBodyValue request
      normalizedRequest = ServiceRequest (Just params)
  case validateAppServiceRequest method params of
    Left err -> pure (Left err)
    Right () -> handler ctx normalizedRequest

appServiceRequestValidators :: [(Text, RequestValidator)]
appServiceRequestValidators =
  [ ("get_slider", fields "get_slider" [requiredText "name"])
  , ("set_slider", fields "set_slider" [requiredText "name", requiredNumber "value"])
  , ("set_sliders", fields "set_sliders" [requiredObject "values"])
  , ("get_enums", fields "get_enums" [requiredText "type"])
  , ("save_preset", fields "save_preset" [requiredText "name"])
  , ("load_preset", fields "load_preset" [requiredText "name"])

  , ("save_world", fields "save_world" [requiredText "name"])
  , ("load_world", fields "load_world" [requiredText "name"])
  , ("set_world_name", fields "set_world_name" [requiredText "name"])

  , ("get_hex", fields "get_hex" [requiredInt "q", requiredInt "r"])
  , ("get_chunk_summary", fields "get_chunk_summary" [requiredInt "chunk"])
  , ("find_hexes", fields "find_hexes" [requiredArray "filters", optionalInt "limit"])
  , ("export_terrain_data", fields "export_terrain_data" [optionalArray "chunks", optionalArray "fields"])

  , ("editor_toggle", fields "editor_toggle" [optionalBool "active"])
  , ("editor_set_tool", fields "editor_set_tool" [requiredText "tool"])
  , ("editor_set_brush", objectFields "editor_set_brush"
      [ optionalInt "radius"
      , optionalNumber "strength"
      , optionalText "falloff"
      , optionalInt "smooth_passes"
      , optionalNumber "noise_frequency"
      , optionalInt "erode_passes"
      ])
  , ("editor_brush_stroke", fields "editor_brush_stroke" [requiredInt "q", requiredInt "r"])
  , ("editor_brush_line", fields "editor_brush_line" [requiredInt "from_q", requiredInt "from_r", requiredInt "to_q", requiredInt "to_r"])
  , ("editor_set_biome", fields "editor_set_biome" [requiredAny "biome"])
  , ("editor_set_form", fields "editor_set_form" [requiredAny "form"])
  , ("editor_set_hardness", fields "editor_set_hardness" [requiredNumber "hardness"])

  , ("set_stage_enabled", fields "set_stage_enabled" [requiredText "stage", requiredBool "enabled"])
  , ("set_plugin_enabled", fields "set_plugin_enabled" [requiredText "name", requiredBool "enabled"])
  , ("set_plugin_param", fields "set_plugin_param" [requiredText "plugin", requiredText "param", requiredAny "value"])

  , ("data_list_resources", fields "data_list_resources" [requiredText "plugin"])
  , ("data_list_records", fields "data_list_records" [requiredText "plugin", requiredText "resource", optionalInt "page_size", optionalInt "page_offset"])
  , ("data_get_record", fields "data_get_record" [requiredText "plugin", requiredText "resource", requiredAny "key"])
  , ("data_create_record", fields "data_create_record" [requiredText "plugin", requiredText "resource", requiredObject "fields"])
  , ("data_update_record", fields "data_update_record" [requiredText "plugin", requiredText "resource", requiredAny "key", requiredObject "fields"])
  , ("data_delete_record", fields "data_delete_record" [requiredText "plugin", requiredText "resource", requiredAny "key"])

  , ("set_sim_auto_tick", fields "set_sim_auto_tick" [requiredBool "enabled", optionalNumber "rate"])
  , ("sim_tick", fields "sim_tick" [optionalInt "count"])

  , ("set_seed", fields "set_seed" [requiredWord64 "seed"])
  , ("set_view_mode", fields "set_view_mode" [requiredText "mode", optionalInt "field_index"])
  , ("set_config_tab", fields "set_config_tab" [requiredText "tab"])
  , ("select_hex", fields "select_hex" [optionalInt "q", optionalInt "r"])
  , ("set_overlay", fields "set_overlay" [requiredText "overlay", optionalInt "field_index"])
  , ("list_overlay_fields", fields "list_overlay_fields" [optionalText "overlay"])
  , ("cycle_overlay", fields "cycle_overlay" [requiredInt "direction"])
  , ("cycle_overlay_field", fields "cycle_overlay_field" [requiredInt "direction"])
  , ("set_camera", fields "set_camera" [requiredNumber "x", requiredNumber "y", optionalNumber "zoom"])
  , ("zoom_to_chunk", fields "zoom_to_chunk" [requiredInt "chunk"])
  , ("set_left_panel", fields "set_left_panel" [requiredBool "visible"])
  , ("set_left_tab", fields "set_left_tab" [requiredText "tab"])
  , ("toggle_config_panel", fields "toggle_config_panel" [optionalBool "visible"])
  , ("set_log_collapsed", fields "set_log_collapsed" [requiredBool "collapsed"])
  , ("set_log_level", fields "set_log_level" [requiredText "level"])
  , ("viewport_scroll", fields "viewport_scroll" [requiredInt "delta", optionalInt "x", optionalInt "y"])
  , ("viewport_click", fields "viewport_click" [requiredInt "x", requiredInt "y", optionalText "button"])
  , ("viewport_drag", fields "viewport_drag" [requiredInt "x1", requiredInt "y1", requiredInt "x2", requiredInt "y2"])
  , ("viewport_hover", fields "viewport_hover" [requiredInt "x", requiredInt "y"])
  , ("click_widget", fields "click_widget" [requiredText "widget_id"])
  , ("get_widget_state", fields "get_widget_state" [requiredText "widget_id"])
  , ("set_dialog_text", fields "set_dialog_text" [requiredText "text", optionalText "target"])
  , ("send_key", fields "send_key" [requiredText "key"])
  ]
  where
    fields = validateFields
    objectFields = validateObjectFields
