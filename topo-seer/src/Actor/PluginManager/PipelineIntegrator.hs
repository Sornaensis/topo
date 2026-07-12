{-# LANGUAGE OverloadedStrings #-}

-- | Pipeline-facing views of loaded plugins.
module Actor.PluginManager.PipelineIntegrator
  ( PluginPipelineInput(..)
  , PluginPipelinePlan(..)
  , PluginGeneratorResolution(..)
  , PluginPipelineDiagnostic(..)
  , buildPluginPipelinePlan
  , integratePluginStages
  , buildPluginStages
  , buildPluginOverlaySchemas
  , orderPlugins
  , pluginToStage
  , pluginNamesDisabledByStage
  , pluginPipelineAvailableDependencyKeys
  , pluginProviderKeys
  ) where

import Data.List (foldl', sortOn)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)

import Actor.PluginManager.PluginSupervisor (loadedPluginDependencyProvider)
import Actor.PluginManager.Types
  ( LoadedPlugin(..)
  , lpConnection
  , PluginManagerState(..)
  , PluginStatus(..)
  , pluginAvailableDependencyKeys
  )
import Topo.Overlay.Schema (OverlaySchema(..))
import Topo.Pipeline (PipelineConfig(..), PipelineStage(..))
import Topo.Pipeline.Stage (StageId(..), parseStageId, stageCanonicalName)
import Topo.Plugin.Dependency
  ( DependencyDecl(..)
  , DependencyDiagnostic(..)
  , DependencyDiagnosticStatus(..)
  , DependencyProvider(..)
  , DependencyResolvedOrder(..)
  , DependencyResolverInput(..)
  , DependencyPipelineInsertion(..)
  , defaultDependencyResolverInput
  , resolveDependencyOrder
  )
import Topo.Plugin.RPC (RPCManifest(..), rpcGeneratorStage)
import Topo.Plugin.RPC.Manifest (RPCGeneratorDecl(..))

-- | Plugin data needed to integrate generator stages into a concrete pipeline.
--
-- The disabled stage set must be the effective pipeline closure (explicitly
-- disabled built-ins plus auto-disabled dependents).  'StagePlugin' entries in
-- that set are treated the same as disabled plugin toggles.
data PluginPipelineInput = PluginPipelineInput
  { ppiPlugins :: ![LoadedPlugin]
  , ppiPluginOrder :: ![Text]
  , ppiDisabledPlugins :: !(Set Text)
  , ppiDisabledStages :: !(Set StageId)
  }

-- | Dependency and anchoring diagnostic produced while planning generator
-- stages.  Blocking diagnostics omit the plugin stage from execution.
data PluginPipelineDiagnostic = PluginPipelineDiagnostic
  { ppdPlugin :: !Text
  , ppdStatus :: !DependencyDiagnosticStatus
  , ppdBlocking :: !Bool
  , ppdMessage :: !Text
  } deriving (Eq, Show)

-- | Resolution for a plugin generator stage, whether executable or skipped.
data PluginGeneratorResolution = PluginGeneratorResolution
  { pgrPlugin :: !Text
  , pgrStageId :: !StageId
  , pgrAnchor :: !(Maybe StageId)
    -- ^ Resolved manifest anchor.  Plugin-to-plugin anchors remain
    -- 'StagePlugin' values here even though they share the provider's concrete
    -- built-in insertion slot.
  , pgrConcreteAnchorStage :: !(Maybe StageId)
    -- ^ Concrete built-in stage occurrence that the generator is inserted
    -- after.  'StageConvergence' resolves to the final concrete convergence
    -- boundary (the last climate/biome/vegetation-feedback occurrence).
  , pgrDisplayAnchor :: !(Maybe StageId)
    -- ^ Logical anchor used by diagnostics/UI.  This preserves
    -- 'StageConvergence' even though the concrete anchor is a repeated built-in
    -- occurrence.
  , pgrAnchorIndex :: !(Maybe Int)
  , pgrEnabled :: !Bool
  , pgrRuntimeOrder :: !(Maybe Int)
  , pgrDiagnostics :: ![PluginPipelineDiagnostic]
  } deriving (Eq, Show)

-- | Planned concrete pipeline and the resolver diagnostics used to derive it.
data PluginPipelinePlan = PluginPipelinePlan
  { pppPipelineConfig :: !PipelineConfig
  , pppResolutions :: ![PluginGeneratorResolution]
  , pppDependencyOrder :: !DependencyResolvedOrder
  , pppDiagnostics :: ![PluginPipelineDiagnostic]
  }

-- | Build a concrete pipeline with plugin generator stages interleaved after
-- their resolved built-in/plugin anchors.
buildPluginPipelinePlan :: PluginPipelineInput -> PipelineConfig -> PluginPipelinePlan
buildPluginPipelinePlan input baseConfig = PluginPipelinePlan
  { pppPipelineConfig = baseConfig { pipelineStages = finalStages }
  , pppResolutions = resolutions
  , pppDependencyOrder = dependencyOrder
  , pppDiagnostics = planDiagnostics
  }
  where
    baseStages = pipelineStages baseConfig
    disabledStages = ppiDisabledStages input
    plugins = ppiPlugins input
    loadedByName = Map.fromList [(lpName plugin, plugin) | plugin <- plugins]
    generatorPlugins = [plugin | plugin <- plugins, isJust (rmGenerator (lpManifest plugin))]
    generatorNames = Set.fromList (map lpName generatorPlugins)
    executableStages = Map.fromList
      [ (lpName plugin, stage)
      | plugin <- generatorPlugins
      , stage <- pluginToStage plugin
      ]
    executableNames = Map.keysSet executableStages
    disabledByStage = pluginNamesDisabledByStage disabledStages
    providerNames = Set.fromList (map lpName plugins)
    nonExecutableProviders = Set.difference providerNames executableNames
    disabledProviders = Set.unions
      [ ppiDisabledPlugins input
      , disabledByStage
      , nonExecutableProviders
      ]
    providers = map pipelineDependencyProvider plugins
    resolverInput = (defaultDependencyResolverInput providers)
      { driAvailableStages = availableStageIds baseStages disabledStages
      , driDisabledStages = disabledStages
      , driDisabledPlugins = disabledProviders
      }
    dependencyOrder = resolveDependencyOrder resolverInput
    rawAnchors = Map.fromList
      [ (dpiPlugin insertion, dpiInsertAfter insertion)
      | insertion <- droPipelineOrder dependencyOrder
      ]
    blockedNames = Set.fromList (droBlockedPlugins dependencyOrder)
    eligibleNames = Set.difference executableNames blockedNames
    initialAnchorInfo = Map.fromList
      [ (name, info)
      | name <- Set.toList eligibleNames
      , Just info <- [resolvePluginAnchor baseStages disabledStages rawAnchors name]
      ]
    builtinLiftedAnchorInfo = liftConcreteBuiltinAnchors baseStages disabledStages loadedByName initialAnchorInfo
    unanchoredNames = Set.difference eligibleNames (Map.keysSet builtinLiftedAnchorInfo)
    anchoredNames = Map.keysSet builtinLiftedAnchorInfo
    orderEdges = generatorPluginEdges loadedByName anchoredNames
    anchorInfo = liftConcreteProviderAnchors orderEdges builtinLiftedAnchorInfo
    orderedNames = topologicalPluginOrder anchoredNames orderEdges (pluginPriority anchorInfo (ppiPluginOrder input))
    orderIndex = Map.fromList (zip orderedNames [0 :: Int ..])
    finalStages = interleaveStages baseStages executableStages anchorInfo orderedNames
    customDiagnostics = concat
      [ disabledPluginDiagnostics generatorNames disabledProviders
      , unanchoredPluginDiagnostics unanchoredNames rawAnchors
      ]
    resolverDiagnostics = mapMaybe resolverDiagnostic (droDiagnostics dependencyOrder)
    planDiagnostics = resolverDiagnostics <> customDiagnostics
    diagnosticsByPlugin = Map.fromListWith (<>)
      [ (ppdPlugin diag, [diag])
      | diag <- planDiagnostics
      ]
    orderedSet = Set.fromList orderedNames
    enabledResolutions =
      [ resolutionForName True orderIndex anchorInfo rawAnchors diagnosticsByPlugin name
      | name <- orderedNames
      ]
    skippedOrder = orderPlugins (ppiPluginOrder input) generatorPlugins
    skippedResolutions =
      [ resolutionForName False orderIndex anchorInfo rawAnchors diagnosticsByPlugin (lpName plugin)
      | plugin <- skippedOrder
      , not (Set.member (lpName plugin) orderedSet)
      ]
    resolutions = enabledResolutions <> skippedResolutions

-- | Apply 'buildPluginPipelinePlan' and return only the concrete pipeline.
integratePluginStages :: PluginPipelineInput -> PipelineConfig -> PipelineConfig
integratePluginStages input = pppPipelineConfig . buildPluginPipelinePlan input

pipelineDependencyProvider :: LoadedPlugin -> DependencyProvider
pipelineDependencyProvider plugin = provider
  { dpDependencies = filter isGeneratorDependency (dpDependencies provider)
  }
  where
    provider = loadedPluginDependencyProvider plugin

isGeneratorDependency :: DependencyDecl -> Bool
isGeneratorDependency dep = ddReason dep `elem`
  [ Just "generator insertAfter"
  , Just "generator requires"
  ]

availableStageIds :: [PipelineStage] -> Set StageId -> Set StageId
availableStageIds stages disabledStages = Set.filter (`Set.notMember` disabledStages) $
  Set.fromList (map stageId stages) <> convergenceStage
  where
    convergenceStage = case resolveConcreteBuiltinAnchor stages disabledStages StageConvergence of
      Just _ -> Set.singleton StageConvergence
      Nothing -> Set.empty

resolverDiagnostic :: DependencyDiagnostic -> Maybe PluginPipelineDiagnostic
resolverDiagnostic diag
  | dgdStatus diag == DependencyAvailable && not (dgdBlocking diag) = Nothing
  | otherwise = Just PluginPipelineDiagnostic
      { ppdPlugin = dgdConsumer diag
      , ppdStatus = dgdStatus diag
      , ppdBlocking = dgdBlocking diag
      , ppdMessage = dgdMessage diag
      }

disabledPluginDiagnostics :: Set Text -> Set Text -> [PluginPipelineDiagnostic]
disabledPluginDiagnostics generatorNames disabledProviders =
  [ PluginPipelineDiagnostic
      { ppdPlugin = name
      , ppdStatus = DependencyDisabled
      , ppdBlocking = True
      , ppdMessage = "Plugin '" <> name <> "' is unavailable as a generator provider and is omitted from pipeline execution."
      }
  | name <- Set.toList (Set.intersection generatorNames disabledProviders)
  ]

unanchoredPluginDiagnostics :: Set Text -> Map Text (Maybe StageId) -> [PluginPipelineDiagnostic]
unanchoredPluginDiagnostics names rawAnchors =
  [ PluginPipelineDiagnostic
      { ppdPlugin = name
      , ppdStatus = DependencyMissing
      , ppdBlocking = True
      , ppdMessage = "Plugin '" <> name <> "' has no concrete enabled pipeline anchor for generator insertion"
          <> maybe "." ((" after '" <>) . (<> "'.") . stageCanonicalName) (Map.findWithDefault Nothing name rawAnchors)
      }
  | name <- Set.toList names
  ]

resolutionForName
  :: Bool
  -> Map Text Int
  -> Map Text PluginAnchorInfo
  -> Map Text (Maybe StageId)
  -> Map Text [PluginPipelineDiagnostic]
  -> Text
  -> PluginGeneratorResolution
resolutionForName enabled orderIndex anchorInfo rawAnchors diagnosticsByPlugin name = PluginGeneratorResolution
  { pgrPlugin = name
  , pgrStageId = StagePlugin name
  , pgrAnchor = paiAnchor <$> info
  , pgrConcreteAnchorStage = paiConcreteStage <$> info
  , pgrDisplayAnchor = paiDisplayAnchor <$> info
  , pgrAnchorIndex = paiIndex <$> info
  , pgrEnabled = enabled
  , pgrRuntimeOrder = Map.lookup name orderIndex
  , pgrDiagnostics = Map.findWithDefault [] name diagnosticsByPlugin
      <> [missingAnchorDiagnostic name raw | not enabled, null (Map.findWithDefault [] name diagnosticsByPlugin), let raw = Map.findWithDefault Nothing name rawAnchors]
  }
  where
    info = Map.lookup name anchorInfo

missingAnchorDiagnostic :: Text -> Maybe StageId -> PluginPipelineDiagnostic
missingAnchorDiagnostic name rawAnchor = PluginPipelineDiagnostic
  { ppdPlugin = name
  , ppdStatus = DependencyMissing
  , ppdBlocking = True
  , ppdMessage = "Plugin '" <> name <> "' is not executable in the resolved generator pipeline"
      <> maybe "." ((" after '" <>) . (<> "'.") . stageCanonicalName) rawAnchor
  }

data PluginAnchorInfo = PluginAnchorInfo
  { paiAnchor :: !StageId
  , paiConcreteStage :: !StageId
  , paiDisplayAnchor :: !StageId
  , paiIndex :: !Int
  } deriving (Eq, Show)

resolvePluginAnchor
  :: [PipelineStage]
  -> Set StageId
  -> Map Text (Maybe StageId)
  -> Text
  -> Maybe PluginAnchorInfo
resolvePluginAnchor stages disabledStages rawAnchors = go Set.empty
  where
    go seen name
      | Set.member name seen = Nothing
      | otherwise = case Map.lookup name rawAnchors of
          Just (Just (StagePlugin providerName)) -> do
            providerInfo <- go (Set.insert name seen) providerName
            pure providerInfo { paiAnchor = StagePlugin providerName }
          Just (Just anchorStage) -> do
            idx <- resolveConcreteBuiltinAnchor stages disabledStages anchorStage
            concreteStage <- stageIdAt stages idx
            let displayStage = case anchorStage of
                  StageConvergence -> StageConvergence
                  _ -> concreteStage
            pure PluginAnchorInfo
              { paiAnchor = anchorStage
              , paiConcreteStage = concreteStage
              , paiDisplayAnchor = displayStage
              , paiIndex = idx
              }
          _ -> Nothing

liftConcreteBuiltinAnchors
  :: [PipelineStage]
  -> Set StageId
  -> Map Text LoadedPlugin
  -> Map Text PluginAnchorInfo
  -> Map Text PluginAnchorInfo
liftConcreteBuiltinAnchors stages disabledStages loadedByName = Map.mapWithKey liftOne
  where
    liftOne pluginName currentInfo =
      case latestBuiltinAnchorInfo pluginName of
        Just builtinInfo | paiIndex builtinInfo >= paiIndex currentInfo -> builtinInfo
        _ -> currentInfo
    latestBuiltinAnchorInfo pluginName = do
      plugin <- Map.lookup pluginName loadedByName
      latestAnchorInfo (generatorBuiltinAnchorInfos (lpManifest plugin))
    generatorBuiltinAnchorInfos manifest =
      [ info
      | raw <- generatorDependencyRefs manifest
      , Just stage <- [builtinDependencyStage raw]
      , Just info <- [concreteBuiltinAnchorInfo stages disabledStages stage]
      ]

liftConcreteProviderAnchors :: Set (Text, Text) -> Map Text PluginAnchorInfo -> Map Text PluginAnchorInfo
liftConcreteProviderAnchors edges = go
  where
    orderedEdges = Set.toList edges
    go anchorInfo =
      let (changed, anchorInfo') = foldl' liftOne (False, anchorInfo) orderedEdges
      in if changed then go anchorInfo' else anchorInfo'
    liftOne (changed, anchorInfo) (providerName, consumerName) =
      case (Map.lookup providerName anchorInfo, Map.lookup consumerName anchorInfo) of
        (Just providerInfo, Just consumerInfo)
          | paiIndex providerInfo >= paiIndex consumerInfo ->
              let liftedInfo = providerInfo { paiAnchor = StagePlugin providerName }
                  changed' = changed || Map.lookup consumerName anchorInfo /= Just liftedInfo
              in (changed', Map.insert consumerName liftedInfo anchorInfo)
        _ -> (changed, anchorInfo)

resolveConcreteBuiltinAnchor :: [PipelineStage] -> Set StageId -> StageId -> Maybe Int
resolveConcreteBuiltinAnchor _ _ (StagePlugin _) = Nothing
resolveConcreteBuiltinAnchor stages disabledStages StageConvergence
  | Set.member StageConvergence disabledStages = Nothing
  | any (`Set.member` disabledStages) convergenceMemberStages = Nothing
  | otherwise = lastMaybe
      [ idx
      | (idx, stage) <- zip [0..] stages
      , stageId stage `elem` convergenceMemberStages
      ]
resolveConcreteBuiltinAnchor stages disabledStages sid
  | Set.member sid disabledStages = Nothing
  | otherwise = lastMaybe
      [ idx
      | (idx, stage) <- zip [0..] stages
      , stageId stage == sid
      ]

convergenceMemberStages :: [StageId]
convergenceMemberStages = [StageClimate, StageBiomes, StageVegetationFeedback]

stageIdAt :: [PipelineStage] -> Int -> Maybe StageId
stageIdAt stages idx = case drop idx stages of
  stage:_ -> Just (stageId stage)
  [] -> Nothing

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe xs = Just (last xs)

pluginPriority :: Map Text PluginAnchorInfo -> [Text] -> Text -> (Int, Bool, Int, Text)
pluginPriority anchorInfo userOrder name =
  ( maybe maxBound paiIndex (Map.lookup name anchorInfo)
  , maybe True (const False) rank
  , fromMaybe maxBound rank
  , name
  )
  where
    rank = Map.lookup name (Map.fromList (zip userOrder [0 :: Int ..]))

generatorPluginEdges :: Map Text LoadedPlugin -> Set Text -> Set (Text, Text)
generatorPluginEdges loadedByName nodes = Set.fromList
  [ (providerName, consumerName)
  | consumerName <- Set.toList nodes
  , Just plugin <- [Map.lookup consumerName loadedByName]
  , providerName <- generatorPluginDependencies (lpManifest plugin)
  , Set.member providerName nodes
  , providerName /= consumerName
  ]

generatorPluginDependencies :: RPCManifest -> [Text]
generatorPluginDependencies = mapMaybe dependencyPluginName . generatorDependencyRefs

generatorDependencyRefs :: RPCManifest -> [Text]
generatorDependencyRefs manifest = case rmGenerator manifest of
  Nothing -> []
  Just gen -> rgdInsertAfter gen : rgdRequires gen

builtinDependencyStage :: Text -> Maybe StageId
builtinDependencyStage raw = case parseStageId raw of
  Just stage@(StagePlugin _) -> Nothing
  Just stage -> Just stage
  Nothing -> Nothing

concreteBuiltinAnchorInfo :: [PipelineStage] -> Set StageId -> StageId -> Maybe PluginAnchorInfo
concreteBuiltinAnchorInfo stages disabledStages anchorStage = do
  idx <- resolveConcreteBuiltinAnchor stages disabledStages anchorStage
  concreteStage <- stageIdAt stages idx
  let displayStage = case anchorStage of
        StageConvergence -> StageConvergence
        _ -> concreteStage
  pure PluginAnchorInfo
    { paiAnchor = anchorStage
    , paiConcreteStage = concreteStage
    , paiDisplayAnchor = displayStage
    , paiIndex = idx
    }

latestAnchorInfo :: [PluginAnchorInfo] -> Maybe PluginAnchorInfo
latestAnchorInfo [] = Nothing
latestAnchorInfo (info:infos) = Just (foldl' pick info infos)
  where
    pick current candidate
      | paiIndex current <= paiIndex candidate = candidate
      | otherwise = current

-- Generator declarations name built-ins by canonical stage id.  Any unknown
-- name is interpreted as a plugin id, matching 'manifestDependencyDecls'.
dependencyPluginName :: Text -> Maybe Text
dependencyPluginName raw = case parseStageId raw of
  Just (StagePlugin name) -> Just name
  Just _ -> Nothing
  Nothing -> Just raw

topologicalPluginOrder :: Set Text -> Set (Text, Text) -> (Text -> (Int, Bool, Int, Text)) -> [Text]
topologicalPluginOrder nodes edges priority = go nodes indegree0 []
  where
    filteredEdges = Set.filter
      (\(providerName, consumerName) -> Set.member providerName nodes && Set.member consumerName nodes)
      edges
    adjacency = Set.foldl' insertEdge Map.empty filteredEdges
    indegree0 = foldl'
      (\m (_, consumerName) -> Map.adjust (+ 1) consumerName m)
      (Map.fromList [(name, 0 :: Int) | name <- Set.toList nodes])
      (Set.toList filteredEdges)
    insertEdge m (providerName, consumerName) =
      Map.insertWith Set.union providerName (Set.singleton consumerName) m
    go remaining indegree acc
      | Set.null remaining = acc
      | otherwise = case ready of
          [] -> acc <> sortOn priority (Set.toList remaining)
          name:_ ->
            let remaining' = Set.delete name remaining
                dependents = Map.findWithDefault Set.empty name adjacency
                indegree' = Set.foldl' (\m dependent -> Map.adjust (subtract 1) dependent m) indegree dependents
            in go remaining' indegree' (acc <> [name])
      where
        ready = sortOn priority
          [ name
          | name <- Set.toList remaining
          , Map.findWithDefault 0 name indegree <= 0
          ]

interleaveStages
  :: [PipelineStage]
  -> Map Text PipelineStage
  -> Map Text PluginAnchorInfo
  -> [Text]
  -> [PipelineStage]
interleaveStages baseStages executableStages anchorInfo orderedNames = concat
  [ stage : pluginStagesAfter idx
  | (idx, stage) <- zip [0..] baseStages
  ]
  where
    pluginStagesAfter idx =
      [ pluginStage
      | name <- orderedNames
      , Just info <- [Map.lookup name anchorInfo]
      , paiIndex info == idx
      , Just pluginStage <- [Map.lookup name executableStages]
      ]

-- | Build pipeline stages from all loaded plugins that declare a connected
-- generator section, ordered by the user-defined plugin order.  This legacy
-- view is kept for actor/API callers that request plugin stages alone; terrain
-- generation uses 'buildPluginPipelinePlan' so it can interleave against the
-- concrete built-in pipeline without losing manifest metadata.
buildPluginStages :: PluginManagerState -> [PipelineStage]
buildPluginStages st =
  let plugins = pmsPlugins st
      disabled = pmsDisabledPlugins st
      ordered = orderPlugins (pmsPluginOrder st) (Map.elems plugins)
      enabled = filter (\lp -> not (Set.member (lpName lp) disabled)) ordered
  in concatMap pluginToStage enabled

buildPluginOverlaySchemas :: PluginManagerState -> [OverlaySchema]
buildPluginOverlaySchemas st =
  let disabled = pmsDisabledPlugins st
      ordered = orderPlugins (pmsPluginOrder st) (Map.elems (pmsPlugins st))
      enabled = filter (\lp -> not (Set.member (lpName lp) disabled)) ordered
  in [schema | plugin <- enabled, Just schema <- [lpOverlaySchema plugin]]

-- | Reorder plugins according to the user's saved ordering.
-- Plugins not in the ordering list appear at the end in deterministic name
-- order.
orderPlugins :: [Text] -> [LoadedPlugin] -> [LoadedPlugin]
orderPlugins order plugins =
  let byName = Map.fromList [(lpName p, p) | p <- plugins]
      ordered = [p | name <- order, Just p <- [Map.lookup name byName]]
      remaining = sortOn lpName [p | p <- plugins, lpName p `notElem` order]
  in ordered ++ remaining

-- | Convert a loaded plugin to a pipeline stage if it has a connected generator
-- declaration.
pluginToStage :: LoadedPlugin -> [PipelineStage]
pluginToStage lp =
  case (lpStatus lp, rmGenerator (lpManifest lp), lpConnection lp) of
    (PluginConnected, Just _genDecl, Just conn) -> [rpcGeneratorStage conn]
    _ -> []

pluginNamesDisabledByStage :: Set StageId -> Set Text
pluginNamesDisabledByStage disabledStages = Set.fromList
  [ name | StagePlugin name <- Set.toList disabledStages ]

pluginPipelineAvailableDependencyKeys :: Set StageId -> Set Text -> [LoadedPlugin] -> Set Text
pluginPipelineAvailableDependencyKeys disabledStages effectiveDisabledPlugins plugins =
  pluginAvailableDependencyKeys effectiveDisabledPlugins plugins
    `Set.difference` Set.map stageCanonicalName disabledStages
    `Set.difference` pluginProviderKeys plugins (pluginNamesDisabledByStage disabledStages)

pluginProviderKeys :: [LoadedPlugin] -> Set Text -> Set Text
pluginProviderKeys plugins disabledNames = Set.fromList $ concat
  [ [lpName plugin, "plugin:" <> lpName plugin] <> overlayNames plugin
  | plugin <- plugins
  , Set.member (lpName plugin) disabledNames
  ]
  where
    overlayNames plugin = maybe [] ((:[]) . osName) (lpOverlaySchema plugin)
