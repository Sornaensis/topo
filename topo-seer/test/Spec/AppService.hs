{-# LANGUAGE OverloadedStrings #-}

module Spec.AppService (spec) where

import Data.List (nub, sort)
import Data.Text (Text)
import Test.Hspec

import Seer.Command.AppServiceAdapter (appServiceCommandMethods, commandAppService)
import Seer.Command.Dispatch (dispatchCommandMethods)
import Seer.Service.AppService
  ( appServiceGroups
  , appServiceOperationMethods
  , appServiceOperationSpecs
  )
import Seer.Service.Types
  ( ServiceGroupSpec(..)
  , ServiceOperationSpec(..)
  , groupOperationMethods
  )

spec :: Spec
spec = describe "AppService surface" $ do
  it "covers every current command method exactly once" $ do
    sort appServiceOperationMethods `shouldBe` sort expectedCommandMethods
    appServiceOperationMethods `shouldBe` nub appServiceOperationMethods

  it "is the method catalog exposed by command dispatch" $
    sort dispatchCommandMethods `shouldBe` sort appServiceOperationMethods

  it "has command-backed handlers for every AppService operation" $
    appServiceCommandMethods commandAppService `shouldBe` appServiceOperationMethods

  it "defines stable operation names exactly once" $ do
    let operationNames = map serviceOperationName appServiceOperationSpecs
    operationNames `shouldBe` nub operationNames

  it "defines focused service groups for the M2 behaviour boundary" $
    map (\group -> (serviceGroupName group, groupOperationMethods group)) appServiceGroups
      `shouldBe` expectedServiceGroups

expectedCommandMethods :: [Text]
expectedCommandMethods = concatMap snd expectedServiceGroups

expectedServiceGroups :: [(Text, [Text])]
expectedServiceGroups =
  [ ( "state"
    , [ "get_state"
      , "get_view_modes"
      , "get_ui_state"
      ]
    )
  , ( "config"
    , [ "get_sliders"
      , "get_slider"
      , "set_slider"
      , "set_sliders"
      , "reset_sliders"
      , "get_config_summary"
      , "get_enums"
      , "list_presets"
      , "save_preset"
      , "load_preset"
      ]
    )
  , ( "world"
    , [ "generate"
      , "get_world_meta"
      , "get_generation_status"
      , "list_worlds"
      , "save_world"
      , "load_world"
      , "set_world_name"
      ]
    )
  , ( "terrain"
    , [ "get_hex"
      , "get_chunks"
      , "get_chunk_summary"
      , "get_terrain_stats"
      , "get_overlays"
      , "find_hexes"
      , "export_terrain_data"
      ]
    )
  , ( "editor"
    , [ "editor_toggle"
      , "editor_set_tool"
      , "editor_set_brush"
      , "editor_brush_stroke"
      , "editor_brush_line"
      , "editor_set_biome"
      , "editor_set_form"
      , "editor_set_hardness"
      , "editor_undo"
      , "editor_redo"
      , "editor_get_state"
      ]
    )
  , ( "pipeline"
    , [ "get_pipeline"
      , "set_stage_enabled"
      ]
    )
  , ( "plugins"
    , [ "list_plugins"
      , "set_plugin_enabled"
      , "set_plugin_param"
      ]
    )
  , ( "data-resources"
    , [ "data_list_plugins"
      , "data_list_resources"
      , "data_list_records"
      , "data_get_record"
      , "data_create_record"
      , "data_update_record"
      , "data_delete_record"
      , "data_get_state"
      ]
    )
  , ( "simulation"
    , [ "get_sim_state"
      , "set_sim_auto_tick"
      , "sim_tick"
      ]
    )
  , ( "logs"
    , [ "get_logs"
      ]
    )
  , ( "screenshots"
    , [ "take_screenshot"
      ]
    )
  , ( "ui"
    , [ "set_seed"
      , "set_view_mode"
      , "set_config_tab"
      , "select_hex"
      , "set_overlay"
      , "list_overlay_fields"
      , "cycle_overlay"
      , "cycle_overlay_field"
      , "set_camera"
      , "get_camera"
      , "zoom_to_chunk"
      , "set_left_panel"
      , "set_left_tab"
      , "toggle_config_panel"
      , "set_log_collapsed"
      , "set_log_level"
      , "get_ui_panels"
      , "viewport_scroll"
      , "viewport_click"
      , "viewport_drag"
      , "viewport_hover"
      , "click_widget"
      , "list_widgets"
      , "get_widget_state"
      , "get_dialog_state"
      , "set_dialog_text"
      , "dialog_confirm"
      , "dialog_cancel"
      , "send_key"
      ]
    )
  ]
