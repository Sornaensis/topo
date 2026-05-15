{-# LANGUAGE OverloadedStrings #-}

module Spec.AppService (spec) where

import Data.List (nub, sort)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Test.Hspec
import Topo.Simulation (SimNodeId(..))

import Seer.Command.AppServiceAdapter (appServiceCommandMethods, commandAppService)
import Seer.Command.Dispatch (dispatchCommandMethods)
import Seer.Service.AppService
  ( appServiceGroups
  , appServiceOperationMethods
  , appServiceOperationSpecs
  , DataResourceStateResponse(..)
  , PluginSummary(..)
  , SimulationDagNodeSummary(..)
  , SimulationDagResponse(..)
  , dataResourceCreateRecordOperation
  , dataResourceDeleteRecordOperation
  , dataResourceGetRecordOperation
  , dataResourceListPluginsOperation
  , dataResourceListRecordsOperation
  , dataResourceListResourcesOperation
  , dataResourceStateOperation
  , dataResourceUpdateRecordOperation
  , pipelineGetOperation
  , pipelineSetStageEnabledOperation
  , pluginListOperation
  , pluginSetEnabledOperation
  , pluginSetParamOperation
  , simulationDagOperation
  , simulationSetAutoTickOperation
  , simulationStateOperation
  , simulationTickOperation
  )
import Seer.Service.Types
  ( ServiceGroupSpec(..)
  , ServiceOperationSpec(..)
  , TypedServiceOperation(..)
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

  it "defines typed operation contracts for pipeline/plugin/data/simulation services" $
    typedOperationMethods `shouldBe`
      [ "get_pipeline"
      , "set_stage_enabled"
      , "list_plugins"
      , "set_plugin_enabled"
      , "set_plugin_param"
      , "data_list_plugins"
      , "data_list_resources"
      , "data_list_records"
      , "data_get_record"
      , "data_create_record"
      , "data_update_record"
      , "data_delete_record"
      , "data_get_state"
      , "get_sim_state"
      , "set_sim_auto_tick"
      , "sim_tick"
      , "get_sim_dag"
      ]

  it "keeps command-visible typed contract fields complete" $ do
    pluginSummaryStatus pluginSummaryContract `shouldBe` "connected"
    pluginSummaryEnabled pluginSummaryContract `shouldBe` True
    dataResourceRecordCount dataResourceStateContract `shouldBe` 2
    dataResourceTotalCount dataResourceStateContract `shouldBe` 5
    dataResourcePageOffset dataResourceStateContract `shouldBe` 3
    dataResourceLoading dataResourceStateContract `shouldBe` False
    dataResourceHasSelection dataResourceStateContract `shouldBe` True

  it "keeps the simulation DAG contract typed but not command-backed yet" $ do
    typedOperationMethod simulationDagOperation `shouldBe` "get_sim_dag"
    typedOperationMethod simulationDagOperation `shouldNotSatisfy` (`elem` appServiceOperationMethods)
    simulationDagTerrainWriters simulationDagContract `shouldBe` [SimNodeId "weather"]

typedOperationMethods :: [Text]
typedOperationMethods =
  [ typedOperationMethod pipelineGetOperation
  , typedOperationMethod pipelineSetStageEnabledOperation
  , typedOperationMethod pluginListOperation
  , typedOperationMethod pluginSetEnabledOperation
  , typedOperationMethod pluginSetParamOperation
  , typedOperationMethod dataResourceListPluginsOperation
  , typedOperationMethod dataResourceListResourcesOperation
  , typedOperationMethod dataResourceListRecordsOperation
  , typedOperationMethod dataResourceGetRecordOperation
  , typedOperationMethod dataResourceCreateRecordOperation
  , typedOperationMethod dataResourceUpdateRecordOperation
  , typedOperationMethod dataResourceDeleteRecordOperation
  , typedOperationMethod dataResourceStateOperation
  , typedOperationMethod simulationStateOperation
  , typedOperationMethod simulationSetAutoTickOperation
  , typedOperationMethod simulationTickOperation
  , typedOperationMethod simulationDagOperation
  ]

typedOperationMethod :: TypedServiceOperation request response -> Text
typedOperationMethod = serviceOperationMethod . typedServiceOperationSpec

pluginSummaryContract :: PluginSummary
pluginSummaryContract = PluginSummary
  { pluginSummaryName = "weather"
  , pluginSummaryStatus = "connected"
  , pluginSummaryVersion = "1.0.0"
  , pluginSummaryDescription = "Weather plugin"
  , pluginSummaryEnabled = True
  , pluginSummaryParams = Map.empty
  , pluginSummaryParamSpecs = []
  , pluginSummaryDataResources = []
  , pluginSummaryHasGenerator = False
  , pluginSummaryHasSimulation = True
  }

dataResourceStateContract :: DataResourceStateResponse
dataResourceStateContract = DataResourceStateResponse
  { dataResourceSelectedPlugin = Just "weather"
  , dataResourceSelectedResource = Just "stations"
  , dataResourceRecordCount = 2
  , dataResourceTotalCount = 5
  , dataResourcePageOffset = 3
  , dataResourceLoading = False
  , dataResourceSelectedRecordKey = Just "station-1"
  , dataResourceEditMode = True
  , dataResourceCreateMode = False
  , dataResourceHasSelection = True
  }

simulationDagContract :: SimulationDagResponse
simulationDagContract = SimulationDagResponse
  { simulationDagNodes =
      [ SimulationDagNodeSummary
          { simulationDagNodeId = SimNodeId "weather"
          , simulationDagNodeOverlay = "rainfall"
          , simulationDagNodeDependencies = [SimNodeId "temperature"]
          , simulationDagNodeWritesTerrain = True
          }
      ]
  , simulationDagLevels = [[SimNodeId "temperature"], [SimNodeId "weather"]]
  , simulationDagTerrainWriters = [SimNodeId "weather"]
  }

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
