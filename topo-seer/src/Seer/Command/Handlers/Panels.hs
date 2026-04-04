{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | IPC handlers for UI panel visibility and tab controls:
-- @set_left_panel@, @set_left_tab@, @toggle_config_panel@,
-- @set_log_collapsed@, @set_log_level@, @get_ui_panels@.
module Seer.Command.Handlers.Panels
  ( handleSetLeftPanel
  , handleSetLeftTab
  , handleToggleConfigPanel
  , handleSetLogCollapsed
  , handleSetLogLevel
  , handleGetUiPanels
  ) where

import Data.Aeson (Value(..), object, (.=), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Text (Text)

import Actor.Log (LogLevel(..), LogSnapshot(..), readLogSnapshotRef, setLogCollapsed, setLogMinLevel)
import Actor.UiActions.Handles (ActorHandles(..))
import Actor.UI.State (ConfigTab(..), LeftTab(..), UiState(..), readUiSnapshotRef)
import Actor.UI.Setters (setUiShowLeftPanel, setUiLeftTab, setUiShowConfig)
import Seer.Command.Context (CommandContext(..))
import Topo.Command.Types (SeerResponse, okResponse, errResponse)

-- | Handle @set_left_panel@ — show or hide the left panel.
--
-- Params: @{ "visible": bool }@
handleSetLeftPanel :: CommandContext -> Int -> Value -> IO SeerResponse
handleSetLeftPanel ctx reqId params = do
  case Aeson.parseMaybe parseVisible params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'visible' parameter (expected boolean)"
    Just visible -> do
      let uiH = ahUiHandle (ccActorHandles ctx)
      setUiShowLeftPanel uiH visible
      pure $ okResponse reqId $ object ["visible" .= visible]

-- | Handle @set_left_tab@ — switch the left panel tab.
--
-- Params: @{ "tab": "topo"|"view" }@
handleSetLeftTab :: CommandContext -> Int -> Value -> IO SeerResponse
handleSetLeftTab ctx reqId params = do
  case Aeson.parseMaybe parseTab params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'tab' parameter (expected \"topo\" or \"view\")"
    Just tabName ->
      case textToLeftTab tabName of
        Nothing ->
          pure $ errResponse reqId ("unknown left tab: " <> tabName)
        Just tab -> do
          let uiH = ahUiHandle (ccActorHandles ctx)
          ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
          if not (uiShowLeftPanel ui)
            then pure $ errResponse reqId "left panel is not visible; call set_left_panel first"
            else do
              setUiLeftTab uiH tab
              pure $ okResponse reqId $ object ["tab" .= tabName]

-- | Handle @toggle_config_panel@ — toggle config panel visibility.
--
-- Params: @{ "visible": bool }@ (optional — omit to toggle)
handleToggleConfigPanel :: CommandContext -> Int -> Value -> IO SeerResponse
handleToggleConfigPanel ctx reqId params = do
  let uiH = ahUiHandle (ccActorHandles ctx)
  ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
  let mExplicit = Aeson.parseMaybe parseVisible params
      newState = case mExplicit of
        Just v  -> v
        Nothing -> not (uiShowConfig ui)
  setUiShowConfig uiH newState
  pure $ okResponse reqId $ object ["visible" .= newState]

-- | Handle @set_log_collapsed@ — collapse or expand the log panel.
--
-- Params: @{ "collapsed": bool }@
handleSetLogCollapsed :: CommandContext -> Int -> Value -> IO SeerResponse
handleSetLogCollapsed ctx reqId params = do
  case Aeson.parseMaybe parseCollapsed params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'collapsed' parameter (expected boolean)"
    Just collapsed -> do
      let logH = ahLogHandle (ccActorHandles ctx)
      setLogCollapsed logH collapsed
      pure $ okResponse reqId $ object ["collapsed" .= collapsed]

-- | Handle @set_log_level@ — set the minimum log level filter.
--
-- Params: @{ "level": "debug"|"info"|"warn"|"error" }@
handleSetLogLevel :: CommandContext -> Int -> Value -> IO SeerResponse
handleSetLogLevel ctx reqId params = do
  case Aeson.parseMaybe parseLevel params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'level' parameter (expected \"debug\", \"info\", \"warn\", or \"error\")"
    Just levelName ->
      case textToLogLevel levelName of
        Nothing ->
          pure $ errResponse reqId ("unknown log level: " <> levelName)
        Just level -> do
          let logH = ahLogHandle (ccActorHandles ctx)
          setLogMinLevel logH level
          pure $ okResponse reqId $ object ["level" .= levelName]

-- | Handle @get_ui_panels@ — query current panel visibility state.
--
-- Returns left panel visible/tab, config visible/tab, log collapsed/level.
handleGetUiPanels :: CommandContext -> Int -> Value -> IO SeerResponse
handleGetUiPanels ctx reqId _params = do
  ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
  logSnap <- case ccLogSnapshotRef ctx of
    Nothing  -> pure Nothing
    Just ref -> Just <$> readLogSnapshotRef ref
  pure $ okResponse reqId $ object
    [ "left_panel" .= object
        [ "visible" .= uiShowLeftPanel ui
        , "tab"     .= leftTabToText (uiLeftTab ui)
        ]
    , "config_panel" .= object
        [ "visible" .= uiShowConfig ui
        , "tab"     .= configTabToText (uiConfigTab ui)
        ]
    , "log_panel" .= case logSnap of
        Nothing -> object
          [ "collapsed" .= False
          , "level"     .= ("info" :: Text)
          ]
        Just ls -> object
          [ "collapsed" .= lsCollapsed ls
          , "level"     .= logLevelToText (lsMinLevel ls)
          ]
    ]

-- --------------------------------------------------------------------------
-- Parsers
-- --------------------------------------------------------------------------

parseVisible :: Value -> Aeson.Parser Bool
parseVisible = Aeson.withObject "params" (.: "visible")

parseTab :: Value -> Aeson.Parser Text
parseTab = Aeson.withObject "params" (.: "tab")

parseCollapsed :: Value -> Aeson.Parser Bool
parseCollapsed = Aeson.withObject "params" (.: "collapsed")

parseLevel :: Value -> Aeson.Parser Text
parseLevel = Aeson.withObject "params" (.: "level")

-- --------------------------------------------------------------------------
-- Conversions
-- --------------------------------------------------------------------------

textToLeftTab :: Text -> Maybe LeftTab
textToLeftTab "topo" = Just LeftTopo
textToLeftTab "view" = Just LeftView
textToLeftTab _      = Nothing

leftTabToText :: LeftTab -> Text
leftTabToText LeftTopo = "topo"
leftTabToText LeftView = "view"

textToLogLevel :: Text -> Maybe LogLevel
textToLogLevel "debug" = Just LogDebug
textToLogLevel "info"  = Just LogInfo
textToLogLevel "warn"  = Just LogWarn
textToLogLevel "error" = Just LogError
textToLogLevel _       = Nothing

logLevelToText :: LogLevel -> Text
logLevelToText LogDebug = "debug"
logLevelToText LogInfo  = "info"
logLevelToText LogWarn  = "warn"
logLevelToText LogError = "error"

configTabToText :: ConfigTab -> Text
configTabToText ConfigTerrain  = "terrain"
configTabToText ConfigPlanet   = "planet"
configTabToText ConfigClimate  = "climate"
configTabToText ConfigWeather  = "weather"
configTabToText ConfigBiome    = "biome"
configTabToText ConfigErosion  = "erosion"
configTabToText ConfigPipeline = "pipeline"
configTabToText ConfigData     = "data"
