{-# LANGUAGE OverloadedStrings #-}

-- | IPC handlers for pipeline stage control and diagnostics:
-- @get_pipeline@, @set_stage_enabled@.
module Seer.Command.Handlers.Pipeline
  ( handleGetPipeline
  , handleSetStageEnabled
  ) where

import Actor.Data (DataSnapshot(..))
import Actor.PluginManager
  ( LoadedPlugin(..)
  , PluginDependencyDiagnostic(..)
  , getDisabledPlugins
  , getLoadedPlugins
  , pluginAvailableDependencyKeys
  , pluginDependencyDiagnostics
  , pluginDiagnosticDetail
  , pluginDiagnosticState
  , pluginDiagnosticStateText
  )
import Actor.SnapshotReceiver (readDataSnapshot)
import Actor.UI.Setters (setUiDisabledStages, setUiExplicitDisabledStages)
import Actor.UI.State (PipelineStageRunState(..), UiState(..), getUiSnapshot)
import Actor.UiActions.Handles (ActorHandles(..))
import Data.Aeson (Value(..), object, (.=), (.:))
import qualified Data.Aeson.Types as Aeson
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Seer.Command.Context (CommandContext(..))
import Topo.Command.Types (SeerResponse, okResponse, errResponse)
import Topo.Overlay.Schema (OverlayFieldDef(..), OverlaySchema(..))
import Topo.Pipeline
  ( PipelineStageDiagnostic(..)
  , PipelineStageDoc(..)
  , builtinDependencies
  , builtinStageDocs
  , disabledClosure
  , inferExplicitDisabledRoots
  , pipelineStageDiagnostics
  , stageDiagnosticStatusText
  , stageDocDependencies
  , stageDocFor
  )
import Topo.Pipeline.Stage
  ( StageId(..)
  , allBuiltinStageIds
  , stageCanonicalName
  , parseStageId
  )
import Topo.Plugin.RPC.Manifest (RPCGeneratorDecl(..), RPCManifest(..))
import Topo.Pipeline (StageStatus(..))

-- | Handle @get_pipeline@ — list stages, DAG edges, registry docs, last-run
-- state, and plugin insertion diagnostics.
handleGetPipeline :: CommandContext -> Int -> Value -> IO SeerResponse
handleGetPipeline ctx reqId _params = do
  let handles = ccActorHandles ctx
  ui <- getUiSnapshot (ahUiHandle handles)
  let disabledClosureSet = uiDisabledStages ui
      explicitDisabled = explicitDisabledRootsFor ui
      builtinDiags = pipelineStageDiagnostics builtinDependencies allBuiltinStageIds explicitDisabled
  dataSnap <- readDataSnapshot (ahDataSnapshotRef handles)
  plugins <- getLoadedPlugins (ahPluginManagerHandle handles)
  disabledPlugins <- getDisabledPlugins (ahPluginManagerHandle handles)
  let stageDisabledPluginNames = pluginNamesDisabledByStage disabledClosureSet
      effectiveDisabledPlugins = disabledPlugins <> stageDisabledPluginNames
      availableDeps = pipelineAvailableDependencyKeys ui effectiveDisabledPlugins plugins
      orderedPlugins = orderLoadedPlugins (uiPluginNames ui) plugins
      pluginGenerators = filter (isJust . rmGenerator . lpManifest) orderedPlugins
      builtinEntries = map (builtinStageEntry ui dataSnap) builtinDiags
      pluginEntries = map (pluginStageEntry ui dataSnap disabledPlugins effectiveDisabledPlugins availableDeps) pluginGenerators
      builtinNodes = map builtinDagNode builtinDiags
      pluginNodes = map (pluginDagNode ui effectiveDisabledPlugins availableDeps) pluginGenerators
      builtinEdges = concatMap builtinDagEdges builtinDiags
      pluginEdges = concatMap pluginDagEdges pluginGenerators
      docs = map stageDocJson builtinStageDocs
  pure $ okResponse reqId $ object
    [ "stages" .= (builtinEntries ++ pluginEntries)
    , "dag" .= object
        [ "nodes" .= (builtinNodes ++ pluginNodes)
        , "edges" .= (builtinEdges ++ pluginEdges)
        ]
    , "docs" .= docs
    , "diagnostics" .= object
        [ "disabled_closure" .= map stageCanonicalName (Set.toList disabledClosureSet)
        , "explicit_disabled" .= map stageCanonicalName (Set.toList explicitDisabled)
        , "stage_count" .= length builtinEntries
        , "plugin_stage_count" .= length pluginEntries
        ]
    ]

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
          ui <- getUiSnapshot (ahUiHandle handles)
          let explicitDisabled = explicitDisabledRootsFor ui
              toggled
                | enabled   = Set.delete sid explicitDisabled
                | otherwise = Set.insert sid explicitDisabled
              disabled' = disabledClosure builtinDependencies toggled
              diagnostics = pipelineStageDiagnostics builtinDependencies allBuiltinStageIds toggled
          setUiExplicitDisabledStages uiH toggled
          setUiDisabledStages uiH disabled'
          pure $ okResponse reqId $ object
            [ "stage" .= stageName
            , "enabled" .= not (Set.member sid disabled')
            , "disabled_closure" .= map stageCanonicalName (Set.toList disabled')
            , "diagnostics" .= maybe [] psdiagDiagnostics (stageDiagnosticForLocal sid diagnostics)
            ]

-- --------------------------------------------------------------------------
-- Stage JSON helpers
-- --------------------------------------------------------------------------

builtinStageEntry :: UiState -> DataSnapshot -> PipelineStageDiagnostic -> Value
builtinStageEntry ui dataSnap diag =
  let sid = psdiagStageId diag
      doc = fromMaybe (fallbackDoc sid) (stageDocFor sid)
  in object
    [ "id" .= stageCanonicalName sid
    , "name" .= psdTitle doc
    , "enabled" .= psdiagEnabled diag
    , "source" .= ("builtin" :: Text)
    , "status" .= stageDiagnosticStatusText diag
    , "explicitly_disabled" .= psdiagExplicitlyDisabled diag
    , "auto_disabled" .= psdiagAutoDisabled diag
    , "dependencies" .= stageNames (psdiagDependencies diag)
    , "dependents" .= stageNames (psdiagDependents diag)
    , "disabled_by" .= stageNames (psdiagDisabledBy diag)
    , "config" .= object
        [ "keys" .= psdConfigKeys doc
        , "parameters" .= ([] :: [Value])
        ]
    , "output_fields" .= psdOutputFields doc
    , "output_overlays" .= psdOutputOverlays doc
    , "last_run" .= lastRunJson dataSnap (Map.lookup sid (uiPipelineStageRuns ui)) (psdStageSeedTag doc) "builtin-registry" sid
    , "provenance" .= provenanceJson "builtin-registry" (psdStageSeedTag doc) sid
    , "diagnostics" .= (psdiagDiagnostics diag <> psdDiagnosticHints doc)
    , "plugin_insertion" .= Null
    , "doc" .= stageDocJson doc
    ]

pluginStageEntry :: UiState -> DataSnapshot -> Set.Set Text -> Set.Set Text -> Set.Set Text -> LoadedPlugin -> Value
pluginStageEntry ui dataSnap disabledPlugins effectiveDisabledPlugins availableDeps lp =
  let manifest = lpManifest lp
      name = lpName lp
      sid = StagePlugin name
      disabledByPluginToggle = Set.member name disabledPlugins
      disabledByStageToggle = Set.member sid (uiDisabledStages ui)
      enabled = not (Set.member name effectiveDisabledPlugins) && not disabledByStageToggle
      gen = rmGenerator manifest
      overlayNames = maybe [] ((:[]) . osName) (lpOverlaySchema lp)
      overlayFields = maybe [] (map ofdName . osFields) (lpOverlaySchema lp)
      pluginDiagnostics = pluginDependencyDiagnostics availableDeps lp
      statusText = pluginDiagnosticStateText (pluginDiagnosticState effectiveDisabledPlugins availableDeps lp)
  in object
    [ "id" .= stageCanonicalName sid
    , "name" .= name
    , "enabled" .= enabled
    , "source" .= ("plugin" :: Text)
    , "status" .= if enabled then statusText else ("Disabled" :: Text)
    , "explicitly_disabled" .= disabledByStageToggle
    , "auto_disabled" .= False
    , "dependencies" .= maybe [] rgdRequires gen
    , "dependents" .= ([] :: [Text])
    , "disabled_by" .= (["plugin-toggle" :: Text | disabledByPluginToggle] <> ["stage-toggle" | disabledByStageToggle])
    , "config" .= object
        [ "parameters" .= rmParameters manifest
        , "parameter_values" .= lpParams lp
        ]
    , "output_fields" .= overlayFields
    , "output_overlays" .= overlayNames
    , "last_run" .= lastRunJson dataSnap (Map.lookup sid (uiPipelineStageRuns ui)) ("plugin:" <> name) "plugin-manifest" sid
    , "provenance" .= provenanceJson "plugin-manifest" ("plugin:" <> name) sid
    , "diagnostics" .= (pluginDiagnosticDetail effectiveDisabledPlugins availableDeps lp : map dependencyDiagnosticLine pluginDiagnostics)
    , "plugin_insertion" .= pluginInsertionJson gen
    , "plugin_diagnostics" .= object
        [ "diagnostic_status" .= statusText
        , "dependencies" .= pluginDiagnostics
        , "manifest_version" .= rmManifestVersion manifest
        , "plugin_version" .= rmVersion manifest
        ]
    ]

stageDocJson :: PipelineStageDoc -> Value
stageDocJson doc = object
  [ "id" .= stageCanonicalName (psdStageId doc)
  , "title" .= psdTitle doc
  , "summary" .= psdSummary doc
  , "stage_seed_tag" .= psdStageSeedTag doc
  , "dependencies" .= stageNames (stageDocDependencies doc)
  , "config_keys" .= psdConfigKeys doc
  , "output_fields" .= psdOutputFields doc
  , "output_overlays" .= psdOutputOverlays doc
  , "diagnostic_hints" .= psdDiagnosticHints doc
  ]

lastRunJson :: DataSnapshot -> Maybe PipelineStageRunState -> Text -> Text -> StageId -> Value
lastRunJson dataSnap runState seedTag source sid = object
  [ "status" .= maybe ("not_run" :: Text) (stageRunStatusText . psrsStatus) runState
  , "seed" .= dsLastSeed dataSnap
  , "stage" .= stageCanonicalName sid
  , "stage_seed_tag" .= seedTag
  , "elapsed_ms" .= (runState >>= psrsElapsedMs)
  , "provenance" .= provenanceJson source seedTag sid
  ]

provenanceJson :: Text -> Text -> StageId -> Value
provenanceJson source seedTag sid = object
  [ "source" .= source
  , "stage" .= stageCanonicalName sid
  , "stage_seed_tag" .= seedTag
  ]

pluginInsertionJson :: Maybe RPCGeneratorDecl -> Value
pluginInsertionJson Nothing = Null
pluginInsertionJson (Just gen) = object
  [ "insert_after" .= rgdInsertAfter gen
  , "requires" .= rgdRequires gen
  ]

fallbackDoc :: StageId -> PipelineStageDoc
fallbackDoc sid = PipelineStageDoc
  { psdStageId = sid
  , psdTitle = stageCanonicalName sid
  , psdSummary = "Pipeline stage."
  , psdStageSeedTag = stageCanonicalName sid
  , psdConfigKeys = []
  , psdOutputFields = []
  , psdOutputOverlays = []
  , psdDiagnosticHints = []
  }

-- --------------------------------------------------------------------------
-- DAG helpers
-- --------------------------------------------------------------------------

builtinDagNode :: PipelineStageDiagnostic -> Value
builtinDagNode diag = object
  [ "id" .= stageCanonicalName (psdiagStageId diag)
  , "source" .= ("builtin" :: Text)
  , "enabled" .= psdiagEnabled diag
  , "status" .= stageDiagnosticStatusText diag
  ]

pluginDagNode :: UiState -> Set.Set Text -> Set.Set Text -> LoadedPlugin -> Value
pluginDagNode ui effectiveDisabledPlugins availableDeps lp =
  let sid = StagePlugin (lpName lp)
      enabled = not (Set.member (lpName lp) effectiveDisabledPlugins) && not (Set.member sid (uiDisabledStages ui))
      statusText = pluginDiagnosticStateText (pluginDiagnosticState effectiveDisabledPlugins availableDeps lp)
  in object
    [ "id" .= stageCanonicalName sid
    , "source" .= ("plugin" :: Text)
    , "enabled" .= enabled
    , "status" .= statusText
    ]

builtinDagEdges :: PipelineStageDiagnostic -> [Value]
builtinDagEdges diag =
  [ object
      [ "from" .= stageCanonicalName dep
      , "to" .= stageCanonicalName (psdiagStageId diag)
      , "kind" .= ("requires" :: Text)
      ]
  | dep <- psdiagDependencies diag
  ]

pluginDagEdges :: LoadedPlugin -> [Value]
pluginDagEdges lp = case rmGenerator (lpManifest lp) of
  Nothing -> []
  Just gen ->
    [ object
        [ "from" .= rgdInsertAfter gen
        , "to" .= stageCanonicalName (StagePlugin (lpName lp))
        , "kind" .= ("insert_after" :: Text)
        ]
    ] ++
    [ object
        [ "from" .= dep
        , "to" .= stageCanonicalName (StagePlugin (lpName lp))
        , "kind" .= ("requires" :: Text)
        ]
    | dep <- rgdRequires gen
    ]

-- --------------------------------------------------------------------------
-- Small helpers
-- --------------------------------------------------------------------------

explicitDisabledRootsFor :: UiState -> Set.Set StageId
explicitDisabledRootsFor ui
  | Set.null explicit && not (Set.null closure) = inferExplicitDisabledRoots builtinDependencies closure
  | otherwise = explicit
  where
    explicit = uiExplicitDisabledStages ui
    closure = uiDisabledStages ui

pipelineAvailableDependencyKeys :: UiState -> Set.Set Text -> [LoadedPlugin] -> Set.Set Text
pipelineAvailableDependencyKeys ui effectiveDisabledPlugins plugins =
  pluginAvailableDependencyKeys effectiveDisabledPlugins plugins
    `Set.difference` Set.map stageCanonicalName (uiDisabledStages ui)
    `Set.difference` pluginProviderKeys plugins (pluginNamesDisabledByStage (uiDisabledStages ui))

pluginNamesDisabledByStage :: Set.Set StageId -> Set.Set Text
pluginNamesDisabledByStage disabledStages = Set.fromList
  [ name | StagePlugin name <- Set.toList disabledStages ]

pluginProviderKeys :: [LoadedPlugin] -> Set.Set Text -> Set.Set Text
pluginProviderKeys plugins disabledNames = Set.fromList $ concat
  [ [lpName plugin, "plugin:" <> lpName plugin] <> overlayNames plugin
  | plugin <- plugins
  , Set.member (lpName plugin) disabledNames
  ]
  where
    overlayNames plugin = maybe [] ((:[]) . osName) (lpOverlaySchema plugin)

orderLoadedPlugins :: [Text] -> [LoadedPlugin] -> [LoadedPlugin]
orderLoadedPlugins desiredOrder plugins = ordered ++ remaining
  where
    byName = Map.fromList [(lpName plugin, plugin) | plugin <- plugins]
    ordered = [plugin | name <- desiredOrder, Just plugin <- [Map.lookup name byName]]
    remaining = [plugin | plugin <- plugins, lpName plugin `notElem` desiredOrder]

stageNames :: [StageId] -> [Text]
stageNames = map stageCanonicalName

stageRunStatusText :: StageStatus -> Text
stageRunStatusText StageStarted = "started"
stageRunStatusText StageCompleted = "completed"
stageRunStatusText StageSkipped = "skipped"

dependencyDiagnosticLine :: PluginDependencyDiagnostic -> Text
dependencyDiagnosticLine dep =
  pddKind dep <> ":" <> pddName dep <> "=" <> pddStatus dep <> requiredSuffix <> detailSuffix
  where
    requiredSuffix
      | pddRequired dep = " (required)"
      | otherwise = ""
    detailSuffix = maybe "" ("; " <>) (pddDetail dep)

stageDiagnosticForLocal :: StageId -> [PipelineStageDiagnostic] -> Maybe PipelineStageDiagnostic
stageDiagnosticForLocal sid diagnostics =
  case [diag | diag <- diagnostics, psdiagStageId diag == sid] of
    diag:_ -> Just diag
    [] -> Nothing

parseStageToggle :: Value -> Aeson.Parser (Text, Bool)
parseStageToggle = Aeson.withObject "set_stage_enabled" $ \o ->
  (,) <$> o .: "stage" <*> o .: "enabled"
