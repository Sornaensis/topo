{-# LANGUAGE OverloadedStrings #-}

-- | IPC handlers for pipeline stage control: @get_pipeline@,
-- @set_stage_enabled@.
--
-- The generation pipeline consists of 18 built-in stages plus any
-- plugin-contributed stages.  Stages can be individually disabled;
-- changes take effect on the next generation.
module Seer.Command.Handlers.Pipeline
  ( handleGetPipeline
  , handleSetStageEnabled
  ) where

import Data.Aeson (Value(..), object, (.=), (.:))
import qualified Data.Aeson.Types as Aeson
import qualified Data.Set as Set
import Data.Text (Text)

import Actor.PluginManager (getPluginStages)
import Actor.UI.Setters (setUiDisabledStages)
import Actor.UI.State (UiState(..), readUiSnapshotRef)
import Actor.UiActions.Handles (ActorHandles(..))
import Seer.Command.Context (CommandContext(..))
import Topo.Command.Types (SeerResponse, okResponse, errResponse)
import Topo.Pipeline (PipelineStage(..))
import Topo.Pipeline.Stage
  ( StageId(..)
  , allBuiltinStageIds
  , stageCanonicalName
  , parseStageId
  )

-- | Handle @get_pipeline@ — list all stages with enabled/disabled status.
handleGetPipeline :: CommandContext -> Int -> Value -> IO SeerResponse
handleGetPipeline ctx reqId _params = do
  ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
  let handles = ccActorHandles ctx
      disabled = uiDisabledStages ui
  pluginStages <- getPluginStages (ahPluginManagerHandle handles)
  let builtinEntries = map (stageEntry disabled "builtin") allBuiltinStageIds
      pluginEntries  = map (pluginStageEntry disabled) pluginStages
  pure $ okResponse reqId $ object
    [ "stages" .= (builtinEntries ++ pluginEntries) ]

-- | Handle @set_stage_enabled@ — enable or disable a pipeline stage.
--
-- Params: @{ "stage": "erosion", "enabled": true }@
handleSetStageEnabled :: CommandContext -> Int -> Value -> IO SeerResponse
handleSetStageEnabled ctx reqId params = do
  case Aeson.parseMaybe parseStageToggle params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'stage' and/or 'enabled' parameters"
    Just (stageName, enabled) ->
      case parseStageId stageName of
        Nothing ->
          pure $ errResponse reqId ("unknown stage: " <> stageName)
        Just sid -> do
          let handles = ccActorHandles ctx
              uiH = ahUiHandle handles
          ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
          let disabled = uiDisabledStages ui
              disabled'
                | enabled   = Set.delete sid disabled
                | otherwise = Set.insert sid disabled
          setUiDisabledStages uiH disabled'
          pure $ okResponse reqId $ object
            [ "stage"   .= stageName
            , "enabled" .= enabled
            ]

-- --------------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------------

stageEntry :: Set.Set StageId -> Text -> StageId -> Value
stageEntry disabled source sid = object
  [ "id"      .= stageCanonicalName sid
  , "enabled" .= not (Set.member sid disabled)
  , "source"  .= source
  ]

pluginStageEntry :: Set.Set StageId -> PipelineStage -> Value
pluginStageEntry disabled ps = object
  [ "id"      .= stageCanonicalName (stageId ps)
  , "name"    .= stageName ps
  , "enabled" .= not (Set.member (stageId ps) disabled)
  , "source"  .= ("plugin" :: Text)
  ]

parseStageToggle :: Value -> Aeson.Parser (Text, Bool)
parseStageToggle = Aeson.withObject "set_stage_enabled" $ \o ->
  (,) <$> o .: "stage" <*> o .: "enabled"
