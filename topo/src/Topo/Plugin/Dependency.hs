{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Typed dependency declarations for plugin resolver inputs.
--
-- This module models dependency edges only.  It intentionally does not decide
-- whether a graph is valid, acyclic, or executable; resolver validation and
-- ordering are layered on top of these types.  Derived 'Ord' instances are for
-- @Map@/@Set@ keys, not resolver execution order.
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
  deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON ResourceOperation where
  parseJSON = withText "ResourceOperation" $ \t -> case t of
    "list" -> pure ResourceList
    "get" -> pure ResourceGet
    "create" -> pure ResourceCreate
    "update" -> pure ResourceUpdate
    "delete" -> pure ResourceDelete
    "queryByHex" -> pure ResourceQueryByHex
    _ -> fail ("unknown resource operation: " <> Text.unpack t)

instance ToJSON ResourceOperation where
  toJSON ResourceList = "list"
  toJSON ResourceGet = "get"
  toJSON ResourceCreate = "create"
  toJSON ResourceUpdate = "update"
  toJSON ResourceDelete = "delete"
  toJSON ResourceQueryByHex = "queryByHex"

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

-- | Return diagnostics for every dependency declaration, including available
-- dependencies.  Missing, disabled, and required-cycle diagnostics have
-- 'dgdBlocking' set when their declaration mode blocks readiness.
validateDependencies :: DependencyResolverInput -> [DependencyDiagnostic]
validateDependencies input = map markCycle baseDiagnostics
  where
    providerMap = dependencyProviderMap input
    baseDiagnostics = concatMap (diagnosticsForProvider input) (driProviders input)
    requiredAvailableEdges =
      [ (dgdConsumer diag, provider)
      | diag <- baseDiagnostics
      , dgdStatus diag == DependencyAvailable
      , dependencyDeclBlocks (dgdDependency diag)
      , Just provider <- [dgdProvider diag]
      , Map.member provider providerMap
      ]
    adjacency = Map.fromListWith (<>)
      [ (consumer, [provider])
      | (consumer, provider) <- requiredAvailableEdges
      ]
    markCycle diag = case dgdProvider diag of
      Just provider
        | dgdStatus diag == DependencyAvailable
        , dependencyDeclBlocks (dgdDependency diag)
        , Map.member provider providerMap
        , Just path <- dependencyCyclePath adjacency (dgdConsumer diag) provider ->
            diag
              { dgdStatus = DependencyCycle
              , dgdBlocking = True
              , dgdCycle = path
              , dgdMessage =
                  "Dependency cycle blocks plugin '" <> dgdConsumer diag <> "': "
                    <> Text.intercalate " -> " path
              }
      _ -> diag

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
    providers = driProviders input
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
