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
  , dependencyDeclBlocks
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
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

import Topo.Pipeline.Stage (StageId(..), parseStageId, stageCanonicalName)
import Topo.Plugin (Capability(..))
import Topo.Plugin.RPC.Manifest (RPCExternalDataSourceAccess(..))

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

isBuiltInStage :: StageId -> Bool
isBuiltInStage (StagePlugin _) = False
isBuiltInStage _ = True
