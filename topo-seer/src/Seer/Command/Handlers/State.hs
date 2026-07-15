{-# LANGUAGE OverloadedStrings #-}

-- | Handlers for state query commands: @get_state@, @get_view_modes@,
-- @get_views@, @get_ui_state@.
module Seer.Command.Handlers.State
  ( handleGetState
  , handleGetViewModes
  , handleGetViews
  , handleGetUiState
  ) where

import Data.Aeson (Value(..), object, (.=))
import Data.IORef (readIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)

import Actor.Data (TerrainSnapshot(..))
import Actor.Log (LogLevel(..), LogSnapshot(..), LogSnapshotRef, readLogSnapshotRef)
import Actor.SnapshotReceiver (readTerrainSnapshot)
import Actor.UiActions.Handles (ActorHandles(..))
import Actor.UI.State
  ( BaseViewMode
  , DataBrowserState(..)
  , ConfigTab(..)
  , LayeredViewState(..)
  , LeftTab(..)
  , SkyOverlayMode(..)
  , SourceKind
  , TemporalBasis
  , UiSnapshotRef
  , UiState(..)
  , ViewMode(..)
  , ViewModeDataSemantics(..)
  , WeatherBasis(..)
  , allBaseViewModes
  , allBuiltinSkyOverlayModes
  , allBuiltinViewModes
  , baseViewModeSummaryToJSON
  , baseViewModeToText
  , dataBrowserScopedError
  , effectiveViewSelection
  , layeredViewStateToJSON
  , layeredViewStateToViewMode
  , skyOverlayModeSummaryToJSON
  , skyOverlayModeToText
  , sourceKindToText
  , temporalBasisToText
  , viewModeDataSemantics
  , viewModeMetadata
  , viewModeSummaryToJSON
  , viewModeToText
  , weatherBasisToText
  , weatherOverlaySourceKind
  , weatherOverlayTemporalBasis
  , readUiSnapshotRef
  )
import Seer.Command.Context (CommandContext(..))
import Seer.DataBrowser.Model
  ( dataBrowserAsyncErrorValue
  , dataBrowserPendingEnvelopeValue
  )
import Seer.Editor.Types (EditorState(..), EditorTool(..))
import Topo.Command.Types (SeerResponse, okResponse)
import Topo.Overlay (overlayNames)

-- | Handle @get_state@ — return high-level application state.
handleGetState :: CommandContext -> Int -> Value -> IO SeerResponse
handleGetState ctx reqId _params = do
  ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
  let selection = effectiveViewSelection ui
  pure $ okResponse reqId $ object
    [ "seed"              .= uiSeed ui
    , "view_mode"         .= viewModeToText (uiViewMode ui)
    , "view"              .= layeredViewStateToJSON selection
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
  let selection = effectiveViewSelection ui
  pure $ okResponse reqId $ object
    [ "view_modes" .= legacyViewModeSummaries ui
    , "view" .= layeredViewStateToJSON selection
    ]

legacyViewModeSummaries :: UiState -> [Value]
legacyViewModeSummaries ui = map modeSummary allBuiltinViewModes
  where
    current = uiViewMode ui
    modeSummary vm =
      maybe
        (object ["name" .= viewModeToText vm, "active" .= (vm == current)])
        (viewModeSummaryToJSON (vm == current))
        (viewModeMetadata vm)

-- | Handle @get_views@ — return layered view state and available choices.
handleGetViews :: CommandContext -> Int -> Value -> IO SeerResponse
handleGetViews ctx reqId _params = do
  ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
  terrainSnap <- readTerrainSnapshot (ahTerrainSnapshotRef (ccActorHandles ctx))
  let selection = effectiveViewSelection ui
      availableNames = availableOverlayNames ui terrainSnap
  pure $ okResponse reqId $ viewChoicesJSON availableNames ui selection

viewChoicesJSON :: [Text] -> UiState -> LayeredViewState -> Value
viewChoicesJSON availableNames ui selection = object
  [ "view" .= layeredViewStateToJSON selection
  , "legacy_view_mode" .= viewModeToText (uiViewMode ui)
  , "base_modes" .= map (baseChoice selection) allBaseViewModes
  , "overlay_modes" .= overlayChoices availableNames selection
  , "weather_bases" .= map (weatherBasisChoice selection) [WeatherBasisAverage, WeatherBasisCurrent]
  , "overlay_names" .= availableNames
  , "legacy_modes" .= legacyViewModeSummaries ui
  ]

baseChoice :: LayeredViewState -> BaseViewMode -> Value
baseChoice selection mode = baseViewModeSummaryToJSON (lvsBaseView selection == mode) mode

overlayChoices :: [Text] -> LayeredViewState -> [Value]
overlayChoices availableNames selection = noneChoice : builtinChoices <> pluginChoices
  where
    activeOverlay = lvsSkyOverlay selection
    noneChoice = object
      [ "name" .= ("none" :: Text)
      , "active" .= maybe True (const False) activeOverlay
      , "label" .= ("No overlay" :: Text)
      , "legacy_view_mode" .= (Nothing :: Maybe Text)
      , "plugin_overlay" .= (Nothing :: Maybe Text)
      , "field_index" .= (Nothing :: Maybe Int)
      ]
    builtinChoices =
      [ skyOverlayModeSummaryToJSON (activeOverlay == Just overlay) (lvsWeatherBasis selection) overlay
      | overlay <- allBuiltinSkyOverlayModes
      ]
    pluginChoices =
      [ object
          [ "name" .= ("plugin" :: Text)
          , "overlay_mode" .= ("plugin" :: Text)
          , "active" .= (activeOverlay == Just (SkyOverlayPlugin name 0) || pluginActive name activeOverlay)
          , "label" .= ("Plugin overlay: " <> name)
          , "legacy_view_mode" .= (Just ("overlay:" <> name) :: Maybe Text)
          , "plugin_overlay" .= Just name
          , "field_index" .= pluginFieldIndex name activeOverlay
          ]
      | name <- availableNames
      ]

availableOverlayNames :: UiState -> TerrainSnapshot -> [Text]
availableOverlayNames ui snap =
  uiOverlayNames ui <> [name | name <- overlayNames (tsOverlayStore snap), name `notElem` uiOverlayNames ui]

weatherBasisChoice :: LayeredViewState -> WeatherBasis -> Value
weatherBasisChoice selection basis = object
  [ "name" .= weatherBasisToText basis
  , "active" .= (lvsWeatherBasis selection == basis)
  , "temporal_basis" .= fmap temporalBasisToText (activeOverlayTemporalBasis selection basis)
  , "source_kind" .= fmap sourceKindToText (activeOverlaySourceKind selection basis)
  ]

pluginActive :: Text -> Maybe SkyOverlayMode -> Bool
pluginActive name (Just (SkyOverlayPlugin activeName _)) = name == activeName
pluginActive _ _ = False

pluginFieldIndex :: Text -> Maybe SkyOverlayMode -> Maybe Int
pluginFieldIndex name (Just (SkyOverlayPlugin activeName idx))
  | name == activeName = Just idx
pluginFieldIndex _ _ = Nothing

overlayModeName :: Maybe SkyOverlayMode -> Maybe Text
overlayModeName Nothing = Nothing
overlayModeName (Just (SkyOverlayPlugin _ _)) = Just "plugin"
overlayModeName (Just overlayMode) = Just (skyOverlayModeToText overlayMode)

pluginOverlayName :: Maybe SkyOverlayMode -> Maybe Text
pluginOverlayName (Just (SkyOverlayPlugin name _)) = Just name
pluginOverlayName _ = Nothing

activeOverlayTemporalBasis :: LayeredViewState -> WeatherBasis -> Maybe TemporalBasis
activeOverlayTemporalBasis selection basis = weatherOverlayTemporalBasis <$> lvsSkyOverlay selection <*> pure basis

activeOverlaySourceKind :: LayeredViewState -> WeatherBasis -> Maybe SourceKind
activeOverlaySourceKind selection basis = lvsSkyOverlay selection >>= \overlay -> weatherOverlaySourceKind overlay basis

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
      selection = effectiveViewSelection ui
      activeSemantics = viewModeDataSemantics (uiViewMode ui)
  pure $ okResponse reqId $ object
    [ "seed"        .= uiSeed ui
    , "generating"  .= uiGenerating ui
    , "world_name"  .= uiWorldName ui
    , "chunk_size"  .= uiChunkSize ui
    , "view" .= object
        [ "mode"           .= viewModeToText (uiViewMode ui)
        , "base_mode"      .= baseViewModeToText (lvsBaseView selection)
        , "overlay_mode"   .= overlayModeName (lvsSkyOverlay selection)
        , "plugin_overlay" .= pluginOverlayName (lvsSkyOverlay selection)
        , "weather_basis"  .= weatherBasisToText (lvsWeatherBasis selection)
        , "overlay_opacity" .= lvsOverlayOpacity selection
        , "legacy_view_mode" .= fmap viewModeToText (layeredViewStateToViewMode selection)
        , "temporal_basis" .= fmap (temporalBasisToText . vmdsTemporalBasis) activeSemantics
        , "source_kind"    .= fmap (sourceKindToText . vmdsSourceKind) activeSemantics
        , "selection"      .= layeredViewStateToJSON selection
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
        , "pending"           .= fmap dataBrowserPendingEnvelopeValue (dbsPendingRequest dbs)
        , "async_error"       .= fmap dataBrowserAsyncErrorValue (dataBrowserScopedError dbs)
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
