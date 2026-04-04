{-# LANGUAGE OverloadedStrings #-}

-- | Handlers for state query commands: @get_state@, @get_view_modes@, @get_ui_state@.
module Seer.Command.Handlers.State
  ( handleGetState
  , handleGetViewModes
  , handleGetUiState
  ) where

import Data.Aeson (Value(..), object, (.=))
import Data.IORef (readIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)

import Actor.Log (LogLevel(..), LogSnapshot(..), LogSnapshotRef, readLogSnapshotRef)
import Actor.UiActions.Handles (ActorHandles(..))
import Actor.UI.State
  ( UiState(..)
  , DataBrowserState(..)
  , UiSnapshotRef
  , ViewMode(..)
  , ConfigTab(..)
  , LeftTab(..)
  , readUiSnapshotRef
  )
import Seer.Command.Context (CommandContext(..))
import Seer.Editor.Types (EditorState(..), EditorTool(..))
import Topo.Command.Types (SeerResponse, okResponse)

-- | Handle @get_state@ — return high-level application state.
handleGetState :: CommandContext -> Int -> Value -> IO SeerResponse
handleGetState ctx reqId _params = do
  ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
  pure $ okResponse reqId $ object
    [ "seed"              .= uiSeed ui
    , "view_mode"         .= viewModeToText (uiViewMode ui)
    , "config_tab"        .= configTabToText (uiConfigTab ui)
    , "generating"        .= uiGenerating ui
    , "chunk_size"        .= uiChunkSize ui
    , "show_config"       .= uiShowConfig ui
    , "world_name"        .= uiWorldName ui
    , "context_hex"       .= fmap (\(q, r) -> object ["q" .= q, "r" .= r]) (uiContextHex ui)
    ]

-- | Handle @get_view_modes@ — return all view mode names and active flag.
handleGetViewModes :: CommandContext -> Int -> Value -> IO SeerResponse
handleGetViewModes ctx reqId _params = do
  ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
  let current = uiViewMode ui
      modes = map (\vm -> object
        [ "name"   .= viewModeToText vm
        , "active" .= (vm == current)
        ]) allViewModes
  pure $ okResponse reqId $ object ["view_modes" .= modes]

-- | All non-parameterized view modes.
allViewModes :: [ViewMode]
allViewModes =
  [ ViewElevation
  , ViewBiome
  , ViewClimate
  , ViewWeather
  , ViewMoisture
  , ViewPrecip
  , ViewPlateId
  , ViewPlateBoundary
  , ViewPlateHardness
  , ViewPlateCrust
  , ViewPlateAge
  , ViewPlateHeight
  , ViewPlateVelocity
  , ViewVegetation
  , ViewTerrainForm
  ]

viewModeToText :: ViewMode -> Text.Text
viewModeToText ViewElevation     = "elevation"
viewModeToText ViewBiome         = "biome"
viewModeToText ViewClimate       = "climate"
viewModeToText ViewWeather       = "weather"
viewModeToText ViewMoisture      = "moisture"
viewModeToText ViewPrecip        = "precipitation"
viewModeToText ViewPlateId       = "plate_id"
viewModeToText ViewPlateBoundary = "plate_boundary"
viewModeToText ViewPlateHardness = "plate_hardness"
viewModeToText ViewPlateCrust    = "plate_crust"
viewModeToText ViewPlateAge      = "plate_age"
viewModeToText ViewPlateHeight   = "plate_height"
viewModeToText ViewPlateVelocity = "plate_velocity"
viewModeToText ViewVegetation    = "vegetation"
viewModeToText ViewTerrainForm   = "terrain_form"
viewModeToText (ViewOverlay name _idx) = "overlay:" <> name

textToViewMode :: Text.Text -> Maybe ViewMode
textToViewMode "elevation"      = Just ViewElevation
textToViewMode "biome"          = Just ViewBiome
textToViewMode "climate"        = Just ViewClimate
textToViewMode "weather"        = Just ViewWeather
textToViewMode "moisture"       = Just ViewMoisture
textToViewMode "precipitation"  = Just ViewPrecip
textToViewMode "plate_id"       = Just ViewPlateId
textToViewMode "plate_boundary" = Just ViewPlateBoundary
textToViewMode "plate_hardness" = Just ViewPlateHardness
textToViewMode "plate_crust"    = Just ViewPlateCrust
textToViewMode "plate_age"      = Just ViewPlateAge
textToViewMode "plate_height"   = Just ViewPlateHeight
textToViewMode "plate_velocity" = Just ViewPlateVelocity
textToViewMode "vegetation"     = Just ViewVegetation
textToViewMode "terrain_form"   = Just ViewTerrainForm
textToViewMode _                = Nothing

configTabToText :: ConfigTab -> Text.Text
configTabToText ConfigTerrain  = "terrain"
configTabToText ConfigPlanet   = "planet"
configTabToText ConfigClimate  = "climate"
configTabToText ConfigWeather  = "weather"
configTabToText ConfigBiome    = "biome"
configTabToText ConfigErosion  = "erosion"
configTabToText ConfigPipeline = "pipeline"
configTabToText ConfigData     = "data"

textToConfigTab :: Text.Text -> Maybe ConfigTab
textToConfigTab "terrain"  = Just ConfigTerrain
textToConfigTab "planet"   = Just ConfigPlanet
textToConfigTab "climate"  = Just ConfigClimate
textToConfigTab "weather"  = Just ConfigWeather
textToConfigTab "biome"    = Just ConfigBiome
textToConfigTab "erosion"  = Just ConfigErosion
textToConfigTab "pipeline" = Just ConfigPipeline
textToConfigTab "data"     = Just ConfigData
textToConfigTab _          = Nothing

-- --------------------------------------------------------------------------
-- Comprehensive UI state query
-- --------------------------------------------------------------------------

-- | Handle @get_ui_state@ — return full UI state snapshot for LLM testing.
--
-- Aggregates panel visibility, editor state, data browser state,
-- overlay info, hex selection, and simulation state into one response.
handleGetUiState :: CommandContext -> Int -> Value -> IO SeerResponse
handleGetUiState ctx reqId _params = do
  ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
  logSnap <- case ccLogSnapshotRef ctx of
    Nothing  -> pure Nothing
    Just ref -> Just <$> readLogSnapshotRef ref
  let editor = uiEditor ui
      dbs    = uiDataBrowser ui
  pure $ okResponse reqId $ object
    [ "seed"        .= uiSeed ui
    , "generating"  .= uiGenerating ui
    , "world_name"  .= uiWorldName ui
    , "chunk_size"  .= uiChunkSize ui
    , "view" .= object
        [ "mode"           .= viewModeToText (uiViewMode ui)
        , "overlay_name"   .= case uiViewMode ui of
            ViewOverlay n _ -> Just n
            _               -> Nothing
        , "overlay_field"  .= case uiViewMode ui of
            ViewOverlay _ i -> Just i
            _               -> Nothing
        , "overlay_names"  .= uiOverlayNames ui
        ]
    , "panels" .= object
        [ "left" .= object
            [ "visible" .= uiShowLeftPanel ui
            , "tab"     .= leftTabToText (uiLeftTab ui)
            ]
        , "config" .= object
            [ "visible" .= uiShowConfig ui
            , "tab"     .= configTabToText (uiConfigTab ui)
            , "scroll"  .= uiConfigScroll ui
            ]
        , "log" .= case logSnap of
            Nothing -> object
              [ "collapsed" .= False
              , "level"     .= ("info" :: Text)
              ]
            Just ls -> object
              [ "collapsed" .= lsCollapsed ls
              , "level"     .= logLevelToText (lsMinLevel ls)
              ]
        ]
    , "editor" .= object
        [ "active"       .= editorActive editor
        , "tool"         .= editorToolToText (editorTool editor)
        ]
    , "data_browser" .= object
        [ "selected_plugin"   .= dbsSelectedPlugin dbs
        , "selected_resource" .= dbsSelectedResource dbs
        , "record_count"      .= length (dbsRecords dbs)
        , "total_count"       .= dbsTotalCount dbs
        , "page_offset"       .= dbsPageOffset dbs
        , "loading"           .= dbsLoading dbs
        , "edit_mode"         .= dbsEditMode dbs
        , "create_mode"       .= dbsCreateMode dbs
        , "has_selection"     .= case dbsSelectedRecord dbs of
            Just _  -> True
            Nothing -> False
        ]
    , "hex_selection" .= object
        [ "context_hex" .= fmap (\(q, r) -> object ["q" .= q, "r" .= r]) (uiContextHex ui)
        , "pinned"      .= uiHexTooltipPinned ui
        , "hover_hex"   .= fmap (\(q, r) -> object ["q" .= q, "r" .= r]) (uiHoverHex ui)
        ]
    , "simulation" .= object
        [ "auto_tick"  .= uiSimAutoTick ui
        , "tick_count" .= uiSimTickCount ui
        ]
    ]

-- --------------------------------------------------------------------------
-- Additional text conversions for get_ui_state
-- --------------------------------------------------------------------------

leftTabToText :: LeftTab -> Text
leftTabToText LeftTopo = "topo"
leftTabToText LeftView = "view"

logLevelToText :: LogLevel -> Text
logLevelToText LogDebug = "debug"
logLevelToText LogInfo  = "info"
logLevelToText LogWarn  = "warn"
logLevelToText LogError = "error"

editorToolToText :: EditorTool -> Text
editorToolToText ToolRaise      = "raise"
editorToolToText ToolLower      = "lower"
editorToolToText ToolSmooth     = "smooth"
editorToolToText ToolFlatten    = "flatten"
editorToolToText ToolNoise      = "noise"
editorToolToText ToolPaintBiome = "paint_biome"
editorToolToText ToolPaintForm  = "paint_form"
editorToolToText ToolSetHardness = "set_hardness"
editorToolToText ToolErode      = "erode"
