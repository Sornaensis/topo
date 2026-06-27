{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Typed dependency declarations for plugin resolver inputs.
--
-- This module models dependency edges and derives resolver diagnostics plus
-- deterministic startup, pipeline-insertion, and simulation-insertion orders.
-- Derived 'Ord' instances are for @Map@/@Set@ keys, not resolver execution
-- order.
module Topo.Plugin.Dependency
  ( DependencyMode(..)
  , dependencyModeBlocks
  , VersionConstraint(..)
  , PluginDependency(..)
  , OverlayDependency(..)
  , ResourceOperation(..)
  , ResourceDependency(..)
  , ExternalDataSourceDependency(..)
  , DependencyTarget(..)
  , DependencyDecl(..)
  , dependencyTargetKind
  , dependencyTargetLabel
  , dependencyDeclBlocks
  , DependencyProvider(..)
  , DependencyResourceProvider(..)
  , DependencyExternalDataSourceProvider(..)
  , DependencyResolverInput(..)
  , defaultDependencyResolverInput
  , DependencyDiagnosticStatus(..)
  , dependencyDiagnosticStatusText
  , DependencyDiagnostic(..)
  , DependencyPipelineInsertion(..)
  , DependencyResolvedOrder(..)
  , resolveDependencyOrder
  , dependencyStartupOrder
  , dependencyPipelineOrder
  , dependencySimulationOrder
  , validateDependencies
  , blockingDependencyDiagnostics
  , manifestDependencyDecls
  , RPCExternalDataSourceAccess(..)
  ) where

import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , Value(..)
  , (.:)
  , (.:?)
  , (.=)
  , object
  , withObject
  , withText
  )
import Data.Aeson.Types (Parser)
import Data.List (foldl', sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

import Topo.Pipeline.Stage (StageId(..), allBuiltinStageIds, parseStageId, stageCanonicalName)
import Topo.Plugin (Capability(..))
import Topo.Plugin.RPC.Manifest
  ( RPCExternalDataSourceAccess(..)
  , RPCExternalDataSourceRef(..)
  , RPCGeneratorDecl(..)
  , RPCManifest(..)
  , RPCSimulationDecl(..)
  )

-- | Resolver treatment for a dependency edge.
data DependencyMode
  = DependencyRequired
    -- ^ Missing dependency blocks readiness/startup.
  | DependencyOptional
    -- ^ Missing dependency is acceptable and may disable optional features.
  | DependencySoft
    -- ^ Prefer the dependency when present, but never block readiness.
  deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON DependencyMode where
  parseJSON = withText "DependencyMode" $ \t -> case t of
    "required" -> pure DependencyRequired
    "optional" -> pure DependencyOptional
    "soft" -> pure DependencySoft
    _ -> fail ("unknown dependency mode: " <> Text.unpack t)

instance ToJSON DependencyMode where
  toJSON DependencyRequired = "required"
  toJSON DependencyOptional = "optional"
  toJSON DependencySoft = "soft"

-- | Whether a dependency mode should block resolver readiness when missing.
dependencyModeBlocks :: DependencyMode -> Bool
dependencyModeBlocks DependencyRequired = True
dependencyModeBlocks DependencyOptional = False
dependencyModeBlocks DependencySoft = False

-- | Opaque plugin version constraint strings.
--
-- Topo does not parse semantic versions here; the resolver can compare these
-- strings using whatever version policy the manifest contract later selects.
data VersionConstraint
  = VersionAny
  | VersionExact !Text
  | VersionAtLeast !Text
  | VersionAtMost !Text
  | VersionBetween !Text !Text
  deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON VersionConstraint where
  parseJSON (String version) = pure (VersionExact version)
  parseJSON value = flip (withObject "VersionConstraint") value $ \o -> do
    anyFlag <- (o .:? "any" :: Parser (Maybe Bool))
    exact <- (o .:? "exact" :: Parser (Maybe Text))
    minVersion <- (o .:? "min" :: Parser (Maybe Text))
    maxVersion <- (o .:? "max" :: Parser (Maybe Text))
    case (anyFlag, exact, minVersion, maxVersion) of
      (Just True, Nothing, Nothing, Nothing) -> pure VersionAny
      (Nothing, Just version, Nothing, Nothing) -> pure (VersionExact version)
      (Nothing, Nothing, Just lo, Nothing) -> pure (VersionAtLeast lo)
      (Nothing, Nothing, Nothing, Just hi) -> pure (VersionAtMost hi)
      (Nothing, Nothing, Just lo, Just hi) -> pure (VersionBetween lo hi)
      (Nothing, Nothing, Nothing, Nothing) -> pure VersionAny
      _ -> fail "version constraint must be any, exact, min, max, or min+max"

instance ToJSON VersionConstraint where
  toJSON VersionAny = object ["any" .= True]
  toJSON (VersionExact version) = object ["exact" .= version]
  toJSON (VersionAtLeast lo) = object ["min" .= lo]
  toJSON (VersionAtMost hi) = object ["max" .= hi]
  toJSON (VersionBetween lo hi) = object ["min" .= lo, "max" .= hi]

-- | A plugin dependency with an optional version constraint.
data PluginDependency = PluginDependency
  { pdepName :: !Text
  , pdepVersion :: !VersionConstraint
  } deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON PluginDependency where
  parseJSON = withObject "PluginDependency" $ \o ->
    PluginDependency
      <$> o .: "name"
      <*> (o .:? "version" >>= pure . maybe VersionAny id)

instance ToJSON PluginDependency where
  toJSON dep = object $
    [ "name" .= pdepName dep ] <>
    [ "version" .= version
    | let version = pdepVersion dep
    , version /= VersionAny
    ]

-- | Dependency on an overlay, optionally naming the plugin expected to provide it.
data OverlayDependency = OverlayDependency
  { odepName :: !Text
  , odepProducer :: !(Maybe Text)
  } deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON OverlayDependency where
  parseJSON = withObject "OverlayDependency" $ \o ->
    OverlayDependency
      <$> o .: "name"
      <*> o .:? "producer"

instance ToJSON OverlayDependency where
  toJSON dep = object $
    [ "name" .= odepName dep ] <>
    [ "producer" .= producer | Just producer <- [odepProducer dep] ]

-- | Operations a plugin may require from a declared resource.
data ResourceOperation
  = ResourceList
  | ResourceGet
  | ResourceCreate
  | ResourceUpdate
  | ResourceDelete
  | ResourceQueryByHex
  | ResourceQueryByField
  | ResourceSort
  | ResourceFilter
  | ResourcePage
  deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON ResourceOperation where
  parseJSON = withText "ResourceOperation" $ \t -> case t of
    "list" -> pure ResourceList
    "get" -> pure ResourceGet
    "create" -> pure ResourceCreate
    "update" -> pure ResourceUpdate
    "delete" -> pure ResourceDelete
    "queryByHex" -> pure ResourceQueryByHex
    "query_by_hex" -> pure ResourceQueryByHex
    "queryByField" -> pure ResourceQueryByField
    "query_by_field" -> pure ResourceQueryByField
    "sort" -> pure ResourceSort
    "filter" -> pure ResourceFilter
    "page" -> pure ResourcePage
    _ -> fail ("unknown resource operation: " <> Text.unpack t)

instance ToJSON ResourceOperation where
  toJSON ResourceList = "list"
  toJSON ResourceGet = "get"
  toJSON ResourceCreate = "create"
  toJSON ResourceUpdate = "update"
  toJSON ResourceDelete = "delete"
  toJSON ResourceQueryByHex = "queryByHex"
  toJSON ResourceQueryByField = "queryByField"
  toJSON ResourceSort = "sort"
  toJSON ResourceFilter = "filter"
  toJSON ResourcePage = "page"

-- | Dependency on a plugin-owned data resource.
data ResourceDependency = ResourceDependency
  { rdepProvider :: !(Maybe Text)
  , rdepName :: !Text
  , rdepOperations :: ![ResourceOperation]
  , rdepOverlay :: !(Maybe Text)
  } deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON ResourceDependency where
  parseJSON = withObject "ResourceDependency" $ \o ->
    ResourceDependency
      <$> o .:? "provider"
      <*> o .: "name"
      <*> (o .:? "operations" >>= pure . maybe [] id)
      <*> o .:? "overlay"

instance ToJSON ResourceDependency where
  toJSON dep = object $
    [ "name" .= rdepName dep ] <>
    [ "provider" .= provider | Just provider <- [rdepProvider dep] ] <>
    [ "operations" .= rdepOperations dep | not (null (rdepOperations dep)) ] <>
    [ "overlay" .= overlay | Just overlay <- [rdepOverlay dep] ]

-- | Backend-neutral dependency on a provider-owned external data source.
--
-- The provider is a plugin boundary, not a storage backend.  The optional
-- provider field supports late binding by source name, while the consumer field
-- records which plugin requested the source.
data ExternalDataSourceDependency = ExternalDataSourceDependency
  { edsdProvider :: !(Maybe Text)
  , edsdConsumer :: !Text
  , edsdSource :: !Text
  , edsdAccess :: ![RPCExternalDataSourceAccess]
  , edsdResources :: ![Text]
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON ExternalDataSourceDependency where
  parseJSON = withObject "ExternalDataSourceDependency" $ \o ->
    ExternalDataSourceDependency
      <$> o .:? "provider"
      <*> o .: "consumer"
      <*> o .: "source"
      <*> o .: "access"
      <*> (o .:? "resources" >>= pure . maybe [] id)

instance ToJSON ExternalDataSourceDependency where
  toJSON dep = object $
    [ "consumer" .= edsdConsumer dep
    , "source" .= edsdSource dep
    , "access" .= edsdAccess dep
    ] <>
    [ "provider" .= provider | Just provider <- [edsdProvider dep] ] <>
    [ "resources" .= edsdResources dep | not (null (edsdResources dep)) ]

-- | Concrete kinds of dependencies the resolver can reason about.
data DependencyTarget
  = DependencyBuiltInStage !StageId
  | DependencyPlugin !PluginDependency
  | DependencyOverlay !OverlayDependency
  | DependencyResource !ResourceDependency
  | DependencyExternalDataSource !ExternalDataSourceDependency
  | DependencyCapability !Capability
  deriving (Eq, Ord, Show, Generic)

instance FromJSON DependencyTarget where
  parseJSON = withObject "DependencyTarget" $ \o -> do
    depType <- (o .: "type" :: Parser Text)
    case depType of
      "builtInStage" -> do
        rawStage <- (o .: "stage" :: Parser Text)
        case parseStageId rawStage of
          Just stage | isBuiltInStage stage -> pure (DependencyBuiltInStage stage)
          Just _ -> fail "builtInStage dependencies cannot name plugin stages"
          Nothing -> fail ("unknown built-in stage: " <> Text.unpack rawStage)
      "plugin" -> DependencyPlugin <$> parseJSON (Object o)
      "overlay" -> DependencyOverlay <$> parseJSON (Object o)
      "resource" -> DependencyResource <$> parseJSON (Object o)
      "externalDataSource" -> DependencyExternalDataSource <$> parseJSON (Object o)
      "capability" -> DependencyCapability <$> o .: "capability"
      _ -> fail ("unknown dependency target type: " <> Text.unpack depType)

instance ToJSON DependencyTarget where
  toJSON target = case target of
    DependencyBuiltInStage stage -> object
      [ "type" .= ("builtInStage" :: Text)
      , "stage" .= stageCanonicalName stage
      ]
    DependencyPlugin dep -> object $
      [ "type" .= ("plugin" :: Text)
      , "name" .= pdepName dep
      ] <>
      [ "version" .= version
      | let version = pdepVersion dep
      , version /= VersionAny
      ]
    DependencyOverlay dep -> object $
      [ "type" .= ("overlay" :: Text)
      , "name" .= odepName dep
      ] <>
      [ "producer" .= producer | Just producer <- [odepProducer dep] ]
    DependencyResource dep -> object $
      [ "type" .= ("resource" :: Text)
      , "name" .= rdepName dep
      ] <>
      [ "provider" .= provider | Just provider <- [rdepProvider dep] ] <>
      [ "operations" .= rdepOperations dep | not (null (rdepOperations dep)) ] <>
      [ "overlay" .= overlay | Just overlay <- [rdepOverlay dep] ]
    DependencyExternalDataSource dep -> object $
      [ "type" .= ("externalDataSource" :: Text)
      , "consumer" .= edsdConsumer dep
      , "source" .= edsdSource dep
      , "access" .= edsdAccess dep
      ] <>
      [ "provider" .= provider | Just provider <- [edsdProvider dep] ] <>
      [ "resources" .= edsdResources dep | not (null (edsdResources dep)) ]
    DependencyCapability capability -> object
      [ "type" .= ("capability" :: Text)
      , "capability" .= capability
      ]

-- | Stable text tag for a dependency target constructor.
dependencyTargetKind :: DependencyTarget -> Text
dependencyTargetKind target = case target of
  DependencyBuiltInStage _ -> "builtInStage"
  DependencyPlugin _ -> "plugin"
  DependencyOverlay _ -> "overlay"
  DependencyResource _ -> "resource"
  DependencyExternalDataSource _ -> "externalDataSource"
  DependencyCapability _ -> "capability"

-- | Stable human-readable label used in resolver diagnostics.
dependencyTargetLabel :: DependencyTarget -> Text
dependencyTargetLabel target = case target of
  DependencyBuiltInStage stage -> "stage:" <> stageCanonicalName stage
  DependencyPlugin dep -> "plugin:" <> pdepName dep
  DependencyOverlay dep ->
    maybe "" ((<> "/") . ("plugin:" <>)) (odepProducer dep)
      <> "overlay:" <> odepName dep
  DependencyResource dep ->
    maybe "" ((<> "/") . ("plugin:" <>)) (rdepProvider dep)
      <> "resource:" <> rdepName dep
  DependencyExternalDataSource dep ->
    maybe "" ((<> "/") . ("plugin:" <>)) (edsdProvider dep)
      <> "externalDataSource:" <> edsdSource dep
      <> ":consumer:" <> edsdConsumer dep
  DependencyCapability capability -> "capability:" <> capabilityName capability

-- | A resolver input dependency edge.
data DependencyDecl = DependencyDecl
  { ddTarget :: !DependencyTarget
  , ddMode :: !DependencyMode
  , ddReason :: !(Maybe Text)
  } deriving (Eq, Ord, Show, Generic)

instance FromJSON DependencyDecl where
  parseJSON = withObject "DependencyDecl" $ \o ->
    DependencyDecl
      <$> o .: "target"
      <*> (o .:? "mode" >>= pure . maybe DependencyRequired id)
      <*> o .:? "reason"

instance ToJSON DependencyDecl where
  toJSON dep = object $
    [ "target" .= ddTarget dep
    , "mode" .= ddMode dep
    ] <>
    [ "reason" .= reason | Just reason <- [ddReason dep] ]

-- | Whether a dependency declaration blocks readiness if unresolved.
dependencyDeclBlocks :: DependencyDecl -> Bool
dependencyDeclBlocks = dependencyModeBlocks . ddMode

-- | A data resource advertised by a dependency provider.
data DependencyResourceProvider = DependencyResourceProvider
  { drpName :: !Text
  , drpOperations :: ![ResourceOperation]
  , drpOverlay :: !(Maybe Text)
  } deriving (Eq, Ord, Show, Read, Generic)

-- | An external data source advertised by a dependency provider.
data DependencyExternalDataSourceProvider = DependencyExternalDataSourceProvider
  { despName :: !Text
  , despResources :: ![Text]
  } deriving (Eq, Ord, Show, Read, Generic)

-- | Resolver-facing view of one loaded or declared plugin.
data DependencyProvider = DependencyProvider
  { dpName :: !Text
  , dpVersion :: !Text
  , dpDependencies :: ![DependencyDecl]
  , dpCapabilities :: ![Capability]
  , dpOverlays :: ![Text]
  , dpResources :: ![DependencyResourceProvider]
  , dpExternalDataSources :: ![DependencyExternalDataSourceProvider]
  } deriving (Eq, Show, Generic)

-- | Inputs needed for dependency validation without imposing execution order.
data DependencyResolverInput = DependencyResolverInput
  { driProviders :: ![DependencyProvider]
  , driAvailableStages :: !(Set StageId)
  , driDisabledStages :: !(Set StageId)
  , driAvailableCapabilities :: !(Set Capability)
  , driDisabledCapabilities :: !(Set Capability)
  , driDisabledPlugins :: !(Set Text)
  } deriving (Eq, Show, Generic)

-- | Resolver defaults: built-in stages are present and no plugins are
-- disabled. Host capabilities must be supplied explicitly by the caller.
defaultDependencyResolverInput :: [DependencyProvider] -> DependencyResolverInput
defaultDependencyResolverInput providers = DependencyResolverInput
  { driProviders = providers
  , driAvailableStages = Set.fromList allBuiltinStageIds
  , driDisabledStages = Set.empty
  , driAvailableCapabilities = Set.empty
  , driDisabledCapabilities = Set.empty
  , driDisabledPlugins = Set.empty
  }

-- | Availability state for one dependency declaration.
data DependencyDiagnosticStatus
  = DependencyAvailable
  | DependencyMissing
  | DependencyDisabled
  | DependencyCycle
  deriving (Eq, Ord, Show, Read, Generic)

dependencyDiagnosticStatusText :: DependencyDiagnosticStatus -> Text
dependencyDiagnosticStatusText status = case status of
  DependencyAvailable -> "available"
  DependencyMissing -> "missing"
  DependencyDisabled -> "disabled"
  DependencyCycle -> "cycle"

instance ToJSON DependencyDiagnosticStatus where
  toJSON = String . dependencyDiagnosticStatusText

-- | Actionable resolver diagnostic for one declared dependency.
data DependencyDiagnostic = DependencyDiagnostic
  { dgdConsumer :: !Text
  , dgdDependency :: !DependencyDecl
  , dgdStatus :: !DependencyDiagnosticStatus
  , dgdBlocking :: !Bool
  , dgdProvider :: !(Maybe Text)
  , dgdCycle :: ![Text]
  , dgdMessage :: !Text
  } deriving (Eq, Show, Generic)

instance ToJSON DependencyDiagnostic where
  toJSON diag = object
    [ "consumer" .= dgdConsumer diag
    , "dependency" .= dgdDependency diag
    , "target" .= dependencyTargetLabel (ddTarget (dgdDependency diag))
    , "status" .= dgdStatus diag
    , "blocking" .= dgdBlocking diag
    , "provider" .= dgdProvider diag
    , "cycle" .= dgdCycle diag
    , "message" .= dgdMessage diag
    ]

-- | A plugin generator insertion point selected by the dependency resolver.
--
-- The anchor is either a built-in stage, a previously inserted plugin stage, or
-- 'Nothing' when the provider has no pipeline-stage dependency.  Callers may
-- filter these entries to plugins that actually declare generator stages.
data DependencyPipelineInsertion = DependencyPipelineInsertion
  { dpiPlugin :: !Text
  , dpiInsertAfter :: !(Maybe StageId)
  } deriving (Eq, Ord, Show, Read, Generic)

-- | Deterministic dependency-derived orderings for host integration points.
--
-- Disabled plugins and plugins blocked by required dependency diagnostics are
-- omitted from the orders.  The blocked set includes transitive consumers of a
-- blocked provider so startup, pipeline insertion, and simulation DAG insertion
-- cannot run a plugin before a required provider is ready.
data DependencyResolvedOrder = DependencyResolvedOrder
  { droStartupOrder :: ![Text]
  , droPipelineOrder :: ![DependencyPipelineInsertion]
  , droSimulationOrder :: ![Text]
  , droBlockedPlugins :: ![Text]
  , droDiagnostics :: ![DependencyDiagnostic]
  } deriving (Eq, Show, Generic)

-- | Resolve dependency diagnostics and deterministic integration orders.
resolveDependencyOrder :: DependencyResolverInput -> DependencyResolvedOrder
resolveDependencyOrder input = DependencyResolvedOrder
  { droStartupOrder = startupOrder
  , droPipelineOrder = pipelineInsertions
  , droSimulationOrder = simulationOrder
  , droBlockedPlugins = Set.toList blockedPlugins
  , droDiagnostics = diagnostics
  }
  where
    diagnostics = validateDependencies input
    providerNames = Set.fromList (map dpName (driProviders input))
    disabledProviders = Set.intersection providerNames (driDisabledPlugins input)
    providerEdges = dependencyProviderEdges input
    blockedPlugins = dependencyBlockedPlugins providerNames disabledProviders diagnostics providerEdges
    eligibleProviders = Set.difference providerNames blockedPlugins
    orderEdges = dependencyOrderEdges eligibleProviders providerEdges
    startupOrder = deterministicPluginOrder eligibleProviders (const 0) orderEdges
    simulationOrder = deterministicPluginOrder eligibleProviders (const 0) orderEdges
    pipelineOrder = deterministicPluginOrder eligibleProviders pipelineRank orderEdges
    pipelineInsertions = dependencyPipelineInsertions input pipelineOrder orderEdges
    pipelineRank name = maybe (length allBuiltinStageIds) stageRank (providerStageAnchor input name)

-- | Startup order only, derived from 'resolveDependencyOrder'.
dependencyStartupOrder :: DependencyResolverInput -> [Text]
dependencyStartupOrder = droStartupOrder . resolveDependencyOrder

-- | Pipeline generator insertion order only, derived from 'resolveDependencyOrder'.
dependencyPipelineOrder :: DependencyResolverInput -> [DependencyPipelineInsertion]
dependencyPipelineOrder = droPipelineOrder . resolveDependencyOrder

-- | Simulation DAG insertion order only, derived from 'resolveDependencyOrder'.
dependencySimulationOrder :: DependencyResolverInput -> [Text]
dependencySimulationOrder = droSimulationOrder . resolveDependencyOrder

-- | Return diagnostics for every dependency declaration, including available
-- dependencies.  Missing, disabled, and required-cycle diagnostics have
-- 'dgdBlocking' set when their declaration mode blocks readiness.
validateDependencies :: DependencyResolverInput -> [DependencyDiagnostic]
validateDependencies input = map markCycle baseDiagnostics
  where
    providerMap = dependencyProviderMap input
    baseDiagnostics = concatMap (diagnosticsForProvider input) (driProviders input)
    selectedProviders = selectRequiredProviders (dependencyProviderEdges input)
    adjacency = Map.fromListWith (<>)
      [ (consumer, [provider])
      | ((consumer, _dep), provider) <- Map.toList selectedProviders
      ]
    markCycle diag = case selectedProviderFor diag of
      Just provider
        | dgdStatus diag == DependencyAvailable
        , dependencyDeclBlocks (dgdDependency diag)
        , Map.member provider providerMap
        , Just path <- dependencyCyclePath adjacency (dgdConsumer diag) provider ->
            diag
              { dgdProvider = Just provider
              , dgdStatus = DependencyCycle
              , dgdBlocking = True
              , dgdCycle = path
              , dgdMessage =
                  "Dependency cycle blocks plugin '" <> dgdConsumer diag <> "': "
                    <> Text.intercalate " -> " path
              }
        | dgdStatus diag == DependencyAvailable
        , dependencyDeclBlocks (dgdDependency diag) ->
            diag { dgdProvider = Just provider }
      _ -> diag
    selectedProviderFor diag =
      Map.lookup (dependencyEdgeKey (dgdConsumer diag) (dgdDependency diag)) selectedProviders

-- | Blocking diagnostics only, for readiness checks.
blockingDependencyDiagnostics :: [DependencyDiagnostic] -> [DependencyDiagnostic]
blockingDependencyDiagnostics = filter dgdBlocking

-- | Translate legacy manifest declarations into typed dependency declarations.
-- Generator stage names are parsed as built-in stages when possible; unknown
-- names are treated as plugin dependencies.  Simulation dependencies remain
-- overlay dependencies because simulation declarations name overlay inputs.
manifestDependencyDecls :: RPCManifest -> [DependencyDecl]
manifestDependencyDecls manifest = generatorDeps <> simulationDeps <> externalDeps <> capabilityDeps
  where
    generatorDeps = case rmGenerator manifest of
      Nothing -> []
      Just gen ->
        [ stageOrPluginDependency (rgdInsertAfter gen) (Just "generator insertAfter") ]
        <> map (\name -> stageOrPluginDependency name (Just "generator requires")) (rgdRequires gen)
    simulationDeps = case rmSimulation manifest of
      Nothing -> []
      Just sim ->
        [ DependencyDecl
            { ddTarget = DependencyOverlay (OverlayDependency name Nothing)
            , ddMode = DependencyRequired
            , ddReason = Just "simulation dependency"
            }
        | name <- rsdDependencies sim
        ]
    externalDeps =
      [ DependencyDecl
          { ddTarget = DependencyExternalDataSource ExternalDataSourceDependency
              { edsdProvider = redsrProvider ref
              , edsdConsumer = rmName manifest
              , edsdSource = redsrSource ref
              , edsdAccess = redsrAccess ref
              , edsdResources = redsrResources ref
              }
          , ddMode = if redsrRequired ref then DependencyRequired else DependencyOptional
          , ddReason = Just ("external data source ref:" <> redsrName ref)
          }
      | ref <- rmExternalDataSourceRefs manifest
      ]
    capabilityDeps =
      [ DependencyDecl
          { ddTarget = DependencyCapability capability
          , ddMode = DependencyRequired
          , ddReason = Just "manifest capability"
          }
      | capability <- rmCapabilities manifest
      ]

stageOrPluginDependency :: Text -> Maybe Text -> DependencyDecl
stageOrPluginDependency raw reason = DependencyDecl
  { ddTarget = case parseStageId raw of
      Just stage | isBuiltInStage stage -> DependencyBuiltInStage stage
      Just (StagePlugin name) -> DependencyPlugin (PluginDependency name VersionAny)
      Just stage -> DependencyPlugin (PluginDependency (stageCanonicalName stage) VersionAny)
      Nothing -> DependencyPlugin (PluginDependency raw VersionAny)
  , ddMode = DependencyRequired
  , ddReason = reason
  }

data DependencyProviderEdge = DependencyProviderEdge
  { dpeConsumer :: !Text
  , dpeProviders :: ![Text]
  , dpeDependency :: !DependencyDecl
  , dpeMode :: !DependencyMode
  , dpeStatus :: !DependencyDiagnosticStatus
  } deriving (Eq, Ord, Show)

dependencyProviderEdges :: DependencyResolverInput -> [DependencyProviderEdge]
dependencyProviderEdges input =
  [ DependencyProviderEdge
      { dpeConsumer = dpName consumer
      , dpeProviders = loadedProviders providerMap (drProviders resolution)
      , dpeDependency = dep
      , dpeMode = ddMode dep
      , dpeStatus = drStatus resolution
      }
  | consumer <- driProviders input
  , dep <- dpDependencies consumer
  , let resolution = resolveDependency input consumer dep
  , not (null (loadedProviders providerMap (drProviders resolution)))
  ]
  where
    providerMap = dependencyProviderMap input

loadedProviders :: Map Text DependencyProvider -> [Text] -> [Text]
loadedProviders providerMap providers =
  [ name
  | name <- Set.toList (Set.fromList providers)
  , Map.member name providerMap
  ]

dependencyEdgeKey :: Text -> DependencyDecl -> (Text, DependencyDecl)
dependencyEdgeKey consumer dep = (consumer, dep)

selectRequiredProviders :: [DependencyProviderEdge] -> Map (Text, DependencyDecl) Text
selectRequiredProviders providerEdges = case selectRequiredProvidersAcyclic providerEdges of
  Just selectedProviders -> selectedProviders
  Nothing -> selectRequiredProvidersGreedy providerEdges

selectRequiredProvidersAcyclic :: [DependencyProviderEdge] -> Maybe (Map (Text, DependencyDecl) Text)
selectRequiredProvidersAcyclic providerEdges = chooseAcyclic Map.empty Map.empty (requiredProviderEdges providerEdges)
  where
    chooseAcyclic _adjacency selected [] = Just selected
    chooseAcyclic adjacency selected (edge:edges) = listToMaybe
      [ selected'
      | providerName <- dpeProviders edge
      , not (pathExists adjacency providerName (dpeConsumer edge))
      , let key = dependencyEdgeKey (dpeConsumer edge) (dpeDependency edge)
            adjacency' = insertAdjacency (dpeConsumer edge, providerName) adjacency
            selectedWithEdge = Map.insert key providerName selected
      , Just selected' <- [chooseAcyclic adjacency' selectedWithEdge edges]
      ]

selectRequiredProvidersGreedy :: [DependencyProviderEdge] -> Map (Text, DependencyDecl) Text
selectRequiredProvidersGreedy providerEdges = snd (foldl' selectProvider (Map.empty, Map.empty) (requiredProviderEdges providerEdges))
  where
    selectProvider (adjacency, selected) edge =
      case preferredProvider adjacency edge of
        Nothing -> (adjacency, selected)
        Just providerName ->
          let key = dependencyEdgeKey (dpeConsumer edge) (dpeDependency edge)
              adjacency' = insertAdjacency (dpeConsumer edge, providerName) adjacency
          in (adjacency', Map.insert key providerName selected)
    preferredProvider adjacency edge = case nonCyclingProviders of
      providerName:_ -> Just providerName
      [] -> listToMaybe (dpeProviders edge)
      where
        nonCyclingProviders =
          [ providerName
          | providerName <- dpeProviders edge
          , not (pathExists adjacency providerName (dpeConsumer edge))
          ]

requiredProviderEdges :: [DependencyProviderEdge] -> [DependencyProviderEdge]
requiredProviderEdges providerEdges = sortOn edgePriority
  [ edge
  | edge <- providerEdges
  , dpeStatus edge == DependencyAvailable
  , dependencyModeBlocks (dpeMode edge)
  ]
  where
    edgePriority edge =
      ( dpeConsumer edge
      , dependencyTargetLabel (ddTarget (dpeDependency edge))
      , dpeProviders edge
      )

dependencyBlockedPlugins
  :: Set Text
  -> Set Text
  -> [DependencyDiagnostic]
  -> [DependencyProviderEdge]
  -> Set Text
dependencyBlockedPlugins providerNames disabledProviders diagnostics providerEdges = go initialBlocked
  where
    initialBlocked = Set.unions
      [ disabledProviders
      , Set.fromList
          [ dgdConsumer diag
          | diag <- diagnostics
          , dgdBlocking diag
          , Set.member (dgdConsumer diag) providerNames
          ]
      , Set.fromList
          [ pluginName
          | diag <- diagnostics
          , dgdBlocking diag
          , pluginName <- dgdCycle diag
          , Set.member pluginName providerNames
          ]
      ]
    requiredEdges =
      [ edge
      | edge <- providerEdges
      , dependencyModeBlocks (dpeMode edge)
      ]
    go blocked =
      let providerBlocked = Set.fromList
            [ dpeConsumer edge
            | edge <- requiredEdges
            , all (`Set.member` blocked) (dpeProviders edge)
            , Set.member (dpeConsumer edge) providerNames
            , not (Set.member (dpeConsumer edge) blocked)
            ]
          blockedAfterProviders = Set.union blocked providerBlocked
          eligible = Set.difference providerNames blockedAfterProviders
          cycleBlocked = dependencyRequiredCyclePlugins eligible providerEdges
          newlyBlocked = Set.union providerBlocked (Set.difference cycleBlocked blockedAfterProviders)
      in if Set.null newlyBlocked
           then blockedAfterProviders
           else go (Set.unions [blocked, providerBlocked, cycleBlocked])

dependencyRequiredCyclePlugins :: Set Text -> [DependencyProviderEdge] -> Set Text
dependencyRequiredCyclePlugins eligible providerEdges = case selectRequiredProvidersAcyclic eligibleEdges of
  Just _ -> Set.empty
  Nothing -> selectedProviderCyclePlugins (selectRequiredProvidersGreedy eligibleEdges)
  where
    eligibleEdges =
      [ edge { dpeProviders = filter (`Set.member` eligible) (dpeProviders edge) }
      | edge <- providerEdges
      , dpeStatus edge == DependencyAvailable
      , Set.member (dpeConsumer edge) eligible
      , any (`Set.member` eligible) (dpeProviders edge)
      ]

selectedProviderCyclePlugins :: Map (Text, DependencyDecl) Text -> Set Text
selectedProviderCyclePlugins selectedProviders = Set.fromList
  [ pluginName
  | (consumer, providerName) <- selectedEdges
  , Just path <- [dependencyCyclePath adjacency consumer providerName]
  , pluginName <- path
  ]
  where
    selectedEdges =
      [ (consumer, providerName)
      | ((consumer, _dep), providerName) <- Map.toList selectedProviders
      ]
    adjacency = Map.fromListWith (<>)
      [ (consumer, [providerName])
      | (consumer, providerName) <- selectedEdges
      ]

dependencyOrderEdges :: Set Text -> [DependencyProviderEdge] -> Set (Text, Text)
dependencyOrderEdges eligible providerEdges = safePreferenceEdges eligible hardEdges preferredEdges
  where
    eligibleEdges =
      [ edge { dpeProviders = filter (`Set.member` eligible) (dpeProviders edge) }
      | edge <- providerEdges
      , dpeStatus edge == DependencyAvailable
      , Set.member (dpeConsumer edge) eligible
      , any (`Set.member` eligible) (dpeProviders edge)
      ]
    selectedRequired = selectRequiredProviders eligibleEdges
    availableEdges =
      [ (providerName, dpeConsumer edge, dpeMode edge)
      | edge <- eligibleEdges
      , Just providerName <- [providerForOrderEdge selectedRequired edge]
      , providerName /= dpeConsumer edge
      ]
    hardEdges = Set.fromList
      [ (providerName, consumerName)
      | (providerName, consumerName, mode) <- availableEdges
      , dependencyModeBlocks mode
      ]
    preferredEdges =
      [ (providerName, consumerName)
      | (providerName, consumerName, mode) <- availableEdges
      , not (dependencyModeBlocks mode)
      ]

providerForOrderEdge :: Map (Text, DependencyDecl) Text -> DependencyProviderEdge -> Maybe Text
providerForOrderEdge selectedRequired edge
  | dependencyModeBlocks (dpeMode edge) =
      Map.lookup (dependencyEdgeKey (dpeConsumer edge) (dpeDependency edge)) selectedRequired
  | otherwise = listToMaybe (dpeProviders edge)

safePreferenceEdges :: Set Text -> Set (Text, Text) -> [(Text, Text)] -> Set (Text, Text)
safePreferenceEdges eligible hardEdges preferredEdges = finalEdges
  where
    (_, finalEdges) = foldl' addPreference (adjacencyFromEdges hardEdges, hardEdges) sortedPreferences
    sortedPreferences = sortOn (\(providerName, consumerName) -> (providerName, consumerName)) preferredEdges
    addPreference (adjacency, edges) edge@(providerName, consumerName)
      | not (Set.member providerName eligible && Set.member consumerName eligible) = (adjacency, edges)
      | providerName == consumerName = (adjacency, edges)
      | pathExists adjacency consumerName providerName = (adjacency, edges)
      | otherwise = (insertAdjacency edge adjacency, Set.insert edge edges)

deterministicPluginOrder :: Set Text -> (Text -> Int) -> Set (Text, Text) -> [Text]
deterministicPluginOrder nodes rank edges = go nodes indegree0 []
  where
    filteredEdges = Set.filter
      (\(providerName, consumerName) -> Set.member providerName nodes && Set.member consumerName nodes)
      edges
    adjacency = adjacencyFromEdges filteredEdges
    indegree0 = foldl'
      (\m (_, consumerName) -> Map.adjust (+ 1) consumerName m)
      (Map.fromList [(name, 0 :: Int) | name <- Set.toList nodes])
      (Set.toList filteredEdges)
    priority name = (rank name, name)
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

adjacencyFromEdges :: Set (Text, Text) -> Map Text (Set Text)
adjacencyFromEdges = Set.foldl' (flip insertAdjacency) Map.empty

insertAdjacency :: (Text, Text) -> Map Text (Set Text) -> Map Text (Set Text)
insertAdjacency (providerName, consumerName) =
  Map.insertWith Set.union providerName (Set.singleton consumerName)

pathExists :: Map Text (Set Text) -> Text -> Text -> Bool
pathExists adjacency start target = go Set.empty start
  where
    go seen current
      | current == target = True
      | Set.member current seen = False
      | otherwise = any
          (go (Set.insert current seen))
          (Set.toList (Map.findWithDefault Set.empty current adjacency))

dependencyPipelineInsertions
  :: DependencyResolverInput
  -> [Text]
  -> Set (Text, Text)
  -> [DependencyPipelineInsertion]
dependencyPipelineInsertions input orderedPlugins orderEdges = reverse insertions
  where
    providerEdgesByConsumer = Map.fromListWith (<>)
      [ (consumerName, [providerName])
      | (providerName, consumerName) <- Set.toList orderEdges
      ]
    (_, _, insertions) = foldl' addInsertion (Map.empty, 0 :: Int, []) orderedPlugins
    addInsertion (positions, ordinal, acc) pluginName =
      let stageAnchor = providerStageAnchor input pluginName
          stageCandidate = fmap (\stage -> (PipelineBuiltIn stage, (stageRank stage, 0 :: Int))) stageAnchor
          providerCandidates =
            [ (PipelinePlugin providerName, position)
            | providerName <- Map.findWithDefault [] pluginName providerEdgesByConsumer
            , Just (position, True) <- [Map.lookup providerName positions]
            ]
          selectedAnchor = latestPipelineAnchor (maybe [] (:[]) stageCandidate <> providerCandidates)
          selectedStageRank = case selectedAnchor of
            Nothing -> maybe (length allBuiltinStageIds) stageRank stageAnchor
            Just (PipelineBuiltIn stage, _) -> stageRank stage
            Just (PipelinePlugin _providerName, (providerStageRank, _)) -> providerStageRank
          anchorStageId = case selectedAnchor of
            Nothing -> stageAnchor
            Just (PipelineBuiltIn stage, _) -> Just stage
            Just (PipelinePlugin providerName, _) -> Just (StagePlugin providerName)
          position = (selectedStageRank, ordinal + 1)
          insertion = DependencyPipelineInsertion
            { dpiPlugin = pluginName
            , dpiInsertAfter = anchorStageId
            }
      in
        ( Map.insert pluginName (position, maybe False (const True) anchorStageId) positions
        , ordinal + 1
        , insertion : acc
        )

data PipelineAnchor
  = PipelineBuiltIn !StageId
  | PipelinePlugin !Text
  deriving (Eq, Ord, Show)

latestPipelineAnchor :: [(PipelineAnchor, (Int, Int))] -> Maybe (PipelineAnchor, (Int, Int))
latestPipelineAnchor [] = Nothing
latestPipelineAnchor (candidate:candidates) = Just (foldl' pick candidate candidates)
  where
    pick current@(_, currentPosition) next@(_, nextPosition)
      | currentPosition <= nextPosition = next
      | otherwise = current

providerStageAnchor :: DependencyResolverInput -> Text -> Maybe StageId
providerStageAnchor input providerName = do
  provider <- Map.lookup providerName (dependencyProviderMap input)
  latestStage
    [ stage
    | dep <- dpDependencies provider
    , DependencyBuiltInStage stage <- [ddTarget dep]
    , isBuiltInStage stage
    , drStatus (resolveDependency input provider dep) == DependencyAvailable
    ]

latestStage :: [StageId] -> Maybe StageId
latestStage [] = Nothing
latestStage (stage:stages) = Just (foldl' pick stage stages)
  where
    pick current candidate
      | stageOrderKey current <= stageOrderKey candidate = candidate
      | otherwise = current

stageRank :: StageId -> Int
stageRank stage = Map.findWithDefault (length allBuiltinStageIds) stage stageRankMap

stageOrderKey :: StageId -> (Int, Text)
stageOrderKey stage = (stageRank stage, stageCanonicalName stage)

stageRankMap :: Map StageId Int
stageRankMap = Map.fromList (zip allBuiltinStageIds [0..])

data DependencyResolution = DependencyResolution
  { drStatus :: !DependencyDiagnosticStatus
  , drProviders :: ![Text]
  , drMessage :: !Text
  }

diagnosticsForProvider :: DependencyResolverInput -> DependencyProvider -> [DependencyDiagnostic]
diagnosticsForProvider input provider =
  [ let resolution = resolveDependency input provider dep
        status = drStatus resolution
    in DependencyDiagnostic
        { dgdConsumer = dpName provider
        , dgdDependency = dep
        , dgdStatus = status
        , dgdBlocking = status /= DependencyAvailable && dependencyDeclBlocks dep
        , dgdProvider = listToMaybe (drProviders resolution)
        , dgdCycle = []
        , dgdMessage = drMessage resolution
        }
  | dep <- dpDependencies provider
  ]

resolveDependency :: DependencyResolverInput -> DependencyProvider -> DependencyDecl -> DependencyResolution
resolveDependency input consumer decl = case ddTarget decl of
  DependencyBuiltInStage stage -> resolveStage input consumer stage
  DependencyPlugin dep -> resolvePlugin input consumer (pdepName dep)
  DependencyOverlay dep -> resolveOverlay input consumer dep
  DependencyResource dep -> resolveResource input consumer dep
  DependencyExternalDataSource dep -> resolveExternalDataSource input consumer dep
  DependencyCapability capability -> resolveCapability input consumer capability

resolveStage :: DependencyResolverInput -> DependencyProvider -> StageId -> DependencyResolution
resolveStage input consumer stage
  | Set.member stage (driDisabledStages input) = disabled [] $ consumerName <> " depends on disabled stage '" <> stageName <> "'."
  | Set.member stage (driAvailableStages input) = available [] $ consumerName <> " dependency stage '" <> stageName <> "' is available."
  | otherwise = missing [] $ consumerName <> " depends on missing stage '" <> stageName <> "'."
  where
    stageName = stageCanonicalName stage
    consumerName = pluginPhrase (dpName consumer)

resolvePlugin :: DependencyResolverInput -> DependencyProvider -> Text -> DependencyResolution
resolvePlugin input consumer pluginName
  | Set.member pluginName (driDisabledPlugins input) = disabled [pluginName] $
      consumerName <> " depends on disabled plugin '" <> pluginName <> "'."
  | Map.member pluginName providerMap = available [pluginName] $
      consumerName <> " dependency plugin '" <> pluginName <> "' is available."
  | otherwise = missing [] $
      consumerName <> " depends on missing plugin '" <> pluginName <> "'."
  where
    providerMap = dependencyProviderMap input
    consumerName = pluginPhrase (dpName consumer)

resolveOverlay :: DependencyResolverInput -> DependencyProvider -> OverlayDependency -> DependencyResolution
resolveOverlay input consumer dep = case odepProducer dep of
  Just producer -> resolveProviderOwned
    input
    consumer
    producer
    (\provider -> odepName dep `elem` dpOverlays provider)
    "overlay"
    (odepName dep)
  Nothing -> resolveUnqualifiedProvider
    input
    consumer
    (\provider -> odepName dep `elem` dpOverlays provider)
    "overlay"
    (odepName dep)

resolveResource :: DependencyResolverInput -> DependencyProvider -> ResourceDependency -> DependencyResolution
resolveResource input consumer dep = case rdepProvider dep of
  Just providerName -> resolveProviderOwned
    input
    consumer
    providerName
    (providerHasResource dep)
    "resource"
    (rdepName dep)
  Nothing -> resolveUnqualifiedProvider
    input
    consumer
    (providerHasResource dep)
    "resource"
    (rdepName dep)

resolveExternalDataSource :: DependencyResolverInput -> DependencyProvider -> ExternalDataSourceDependency -> DependencyResolution
resolveExternalDataSource input consumer dep = case edsdProvider dep of
  Just providerName -> resolveProviderOwned
    input
    consumer
    providerName
    (providerHasExternalDataSource dep)
    "external data-source"
    (edsdSource dep)
  Nothing -> resolveUnqualifiedProvider
    input
    consumer
    (providerHasExternalDataSource dep)
    "external data-source"
    (edsdSource dep)

resolveCapability :: DependencyResolverInput -> DependencyProvider -> Capability -> DependencyResolution
resolveCapability input consumer capability
  | Set.member capability (driDisabledCapabilities input) = disabled [] $
      consumerName <> " depends on disabled capability '" <> capName <> "'."
  | Set.member capability (driAvailableCapabilities input) = available [] $
      consumerName <> " dependency capability '" <> capName <> "' is available."
  | otherwise = missing [] $
      consumerName <> " depends on missing capability '" <> capName <> "'."
  where
    capName = capabilityName capability
    consumerName = pluginPhrase (dpName consumer)

resolveProviderOwned
  :: DependencyResolverInput
  -> DependencyProvider
  -> Text
  -> (DependencyProvider -> Bool)
  -> Text
  -> Text
  -> DependencyResolution
resolveProviderOwned input consumer providerName predicate kind name
  | Set.member providerName (driDisabledPlugins input) = disabled [providerName] $
      consumerName <> " depends on " <> kind <> " '" <> name <> "' from disabled provider plugin '" <> providerName <> "'."
  | Just provider <- Map.lookup providerName providerMap
  , predicate provider = available [providerName] $
      consumerName <> " dependency " <> kind <> " '" <> name <> "' is available from plugin '" <> providerName <> "'."
  | Just _ <- Map.lookup providerName providerMap = missing [] $
      consumerName <> " depends on missing " <> kind <> " '" <> name <> "' from provider plugin '" <> providerName <> "'."
  | otherwise = missing [] $
      consumerName <> " depends on missing provider plugin '" <> providerName <> "' for " <> kind <> " '" <> name <> "'."
  where
    providerMap = dependencyProviderMap input
    consumerName = pluginPhrase (dpName consumer)

resolveUnqualifiedProvider
  :: DependencyResolverInput
  -> DependencyProvider
  -> (DependencyProvider -> Bool)
  -> Text
  -> Text
  -> DependencyResolution
resolveUnqualifiedProvider input consumer predicate kind name =
  case (enabledMatches, disabledMatches) of
    (firstEnabled:_, _) -> available (map dpName enabledMatches) $
      consumerName <> " dependency " <> kind <> " '" <> name <> "' is available from plugin '" <> dpName firstEnabled <> "'."
    ([], firstDisabled:_) -> disabled (map dpName disabledMatches) $
      consumerName <> " depends on " <> kind <> " '" <> name <> "' that is only provided by disabled plugin '" <> dpName firstDisabled <> "'."
    _ -> missing [] $
      consumerName <> " depends on missing " <> kind <> " '" <> name <> "'."
  where
    providers = sortOn dpName (driProviders input)
    disabledNames = driDisabledPlugins input
    matches = filter predicate providers
    enabledMatches = filter (not . (`Set.member` disabledNames) . dpName) matches
    disabledMatches = filter ((`Set.member` disabledNames) . dpName) matches
    consumerName = pluginPhrase (dpName consumer)

providerHasResource :: ResourceDependency -> DependencyProvider -> Bool
providerHasResource dep provider = any matches (dpResources provider)
  where
    matches resource =
      drpName resource == rdepName dep
        && maybe True (\overlay -> drpOverlay resource == Just overlay) (rdepOverlay dep)
        && all (`elem` drpOperations resource) (rdepOperations dep)

providerHasExternalDataSource :: ExternalDataSourceDependency -> DependencyProvider -> Bool
providerHasExternalDataSource dep provider = any matches (dpExternalDataSources provider)
  where
    matches source =
      despName source == edsdSource dep
        && all (`elem` despResources source) (edsdResources dep)

available :: [Text] -> Text -> DependencyResolution
available providers message = DependencyResolution DependencyAvailable providers message

missing :: [Text] -> Text -> DependencyResolution
missing providers message = DependencyResolution DependencyMissing providers message

disabled :: [Text] -> Text -> DependencyResolution
disabled providers message = DependencyResolution DependencyDisabled providers message

dependencyProviderMap :: DependencyResolverInput -> Map Text DependencyProvider
dependencyProviderMap input = Map.fromList [(dpName provider, provider) | provider <- driProviders input]

dependencyCyclePath :: Map Text [Text] -> Text -> Text -> Maybe [Text]
dependencyCyclePath adjacency consumer provider = do
  pathFromProvider <- findPath Set.empty provider consumer
  pure (consumer : pathFromProvider)
  where
    findPath seen current target
      | current == target = Just [current]
      | Set.member current seen = Nothing
      | otherwise = listToMaybe
          [ current : path
          | next <- Map.findWithDefault [] current adjacency
          , Just path <- [findPath (Set.insert current seen) next target]
          ]

pluginPhrase :: Text -> Text
pluginPhrase name = "plugin '" <> name <> "'"

capabilityName :: Capability -> Text
capabilityName capability = case capability of
  CapLog -> "log"
  CapNoise -> "noise"
  CapReadTerrain -> "readTerrain"
  CapWriteTerrain -> "writeTerrain"
  CapReadOverlay -> "readOverlay"
  CapWriteOverlay -> "writeOverlay"
  CapReadWorld -> "readWorld"
  CapWriteWorld -> "writeWorld"
  CapDataRead -> "dataRead"
  CapDataWrite -> "dataWrite"

isBuiltInStage :: StageId -> Bool
isBuiltInStage (StagePlugin _) = False
isBuiltInStage _ = True
