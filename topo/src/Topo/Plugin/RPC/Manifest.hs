{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Plugin manifest parsing and validation.
--
-- A plugin manifest is a JSON file (@manifest.json@) that declares
-- the plugin's identity, capabilities, pipeline participation
-- (generator and\/or simulation), overlay schema location, and
-- user-facing configuration parameters.
--
-- = Example manifest
--
-- @
-- {
--   "name": "civilization",
--   "version": "1.0.0",
--   "description": "Civilization simulation overlay",
--   "generator": { "insertAfter": "biomes", "requires": ["biomes","rivers"] },
--   "simulation": { "dependencies": ["weather"] },
--   "overlay": { "schemaFile": "civilization.toposchema" },
--   "capabilities": ["readTerrain","readOverlay","writeOverlay","log"],
--   "config": { "parameters": [...] }
-- }
-- @
module Topo.Plugin.RPC.Manifest
  ( -- * Manifest types
    RPCManifest(..)
  , RPCGeneratorDecl(..)
  , RPCSimulationDecl(..)
  , RPCOverlayDecl(..)
  , Capability(..)
    , RPCCapability
  , RPCParamSpec(..)
  , RPCParamType(..)
    -- * Parsing
  , parseManifest
  , parseManifestFile
    -- * Validation
  , ManifestError(..)
  , validateManifest
    -- * Queries
  , manifestWritesTerrain
  , manifestHasGenerator
  , manifestHasSimulation
  , manifestHasOverlay
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
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)
import Topo.Plugin (Capability(..))
import Topo.Plugin.DataResource (DataResourceSchema(..), DataOperations(..))

------------------------------------------------------------------------
-- Capability
------------------------------------------------------------------------

-- | Capabilities a plugin may request.  The host validates these
-- against the manifest and only provides matching data.
type RPCCapability = Capability

------------------------------------------------------------------------
-- Parameter specification
------------------------------------------------------------------------

-- | Scalar type for a plugin parameter.
data RPCParamType
  = ParamFloat
  | ParamInt
  | ParamBool
  deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON RPCParamType where
  parseJSON = withText "RPCParamType" $ \t -> case t of
    "float" -> pure ParamFloat
    "int"   -> pure ParamInt
    "bool"  -> pure ParamBool
    _       -> fail ("unknown parameter type: " <> Text.unpack t)

instance ToJSON RPCParamType where
  toJSON ParamFloat = "float"
  toJSON ParamInt   = "int"
  toJSON ParamBool  = "bool"

-- | A user-facing configuration parameter declared by a plugin.
-- These are rendered as sliders\/checkboxes in topo-seer.
data RPCParamSpec = RPCParamSpec
  { rpsName    :: !Text
    -- ^ Internal parameter name (used as key in config maps).
  , rpsLabel   :: !Text
    -- ^ Human-readable label for UI display.
  , rpsType    :: !RPCParamType
    -- ^ Scalar type.
  , rpsRange   :: !(Maybe (Value, Value))
    -- ^ Optional @(min, max)@ range for numeric parameters.
  , rpsDefault :: !Value
    -- ^ Default value (JSON scalar).
  , rpsTooltip :: !Text
    -- ^ Tooltip text for UI hover.
  } deriving (Eq, Show, Generic)

instance FromJSON RPCParamSpec where
  parseJSON = withObject "RPCParamSpec" $ \o -> do
    name    <- o .:  "name"
    label   <- o .:  "label"
    ty      <- o .:  "type"
    range   <- o .:? "range"
    def     <- o .:  "default"
    tooltip <- o .:? "tooltip"
    let parsedRange = case range of
          Just (Aeson.Array arr)
            | [lo, hi] <- toList arr -> Just (lo, hi)
          _                          -> Nothing
    pure RPCParamSpec
      { rpsName    = name
      , rpsLabel   = label
      , rpsType    = ty
      , rpsRange   = parsedRange
      , rpsDefault = def
      , rpsTooltip = maybe "" id tooltip
      }
    where
      toList :: Aeson.Array -> [Value]
      toList = foldr (:) []

instance ToJSON RPCParamSpec where
  toJSON rps = object $
    [ "name"    .= rpsName rps
    , "label"   .= rpsLabel rps
    , "type"    .= rpsType rps
    , "default" .= rpsDefault rps
    ] <>
    [ "range"   .= Aeson.Array (fromList [lo, hi])
    | Just (lo, hi) <- [rpsRange rps]
    ] <>
    [ "tooltip" .= rpsTooltip rps
    | rpsTooltip rps /= ""
    ]
    where
      fromList :: [Value] -> Aeson.Array
      fromList = foldMap (\v -> pure v)

------------------------------------------------------------------------
-- Generator declaration
------------------------------------------------------------------------

-- | Generator pipeline participation.
data RPCGeneratorDecl = RPCGeneratorDecl
  { rgdInsertAfter :: !Text
    -- ^ Canonical stage name after which this plugin's generator stage
    -- is inserted (e.g. @\"biomes\"@).
  , rgdRequires    :: ![Text]
    -- ^ Canonical stage names that must have run before this plugin
    -- (dependency declarations).
  } deriving (Eq, Show, Generic)

instance FromJSON RPCGeneratorDecl where
  parseJSON = withObject "RPCGeneratorDecl" $ \o ->
    RPCGeneratorDecl
      <$> o .:  "insertAfter"
      <*> (o .:? "requires" >>= pure . maybe [] id)

instance ToJSON RPCGeneratorDecl where
  toJSON rgd = object
    [ "insertAfter" .= rgdInsertAfter rgd
    , "requires"    .= rgdRequires rgd
    ]

------------------------------------------------------------------------
-- Simulation declaration
------------------------------------------------------------------------

-- | Simulation DAG participation.
data RPCSimulationDecl = RPCSimulationDecl
  { rsdDependencies :: ![Text]
    -- ^ Overlay names that must tick before this plugin's sim node.
  } deriving (Eq, Show, Generic)

instance FromJSON RPCSimulationDecl where
  parseJSON = withObject "RPCSimulationDecl" $ \o ->
    RPCSimulationDecl
      <$> (o .:? "dependencies" >>= pure . maybe [] id)

instance ToJSON RPCSimulationDecl where
  toJSON rsd = object
    [ "dependencies" .= rsdDependencies rsd
    ]

------------------------------------------------------------------------
-- Overlay declaration
------------------------------------------------------------------------

-- | Overlay schema reference.
data RPCOverlayDecl = RPCOverlayDecl
  { rodSchemaFile :: !Text
    -- ^ Path to the @.toposchema@ file (relative to plugin directory).
  } deriving (Eq, Show, Generic)

instance FromJSON RPCOverlayDecl where
  parseJSON = withObject "RPCOverlayDecl" $ \o ->
    RPCOverlayDecl <$> o .: "schemaFile"

instance ToJSON RPCOverlayDecl where
  toJSON rod = object [ "schemaFile" .= rodSchemaFile rod ]

------------------------------------------------------------------------
-- Full manifest
------------------------------------------------------------------------

-- | A plugin's complete manifest, parsed from @manifest.json@.
data RPCManifest = RPCManifest
  { rmName          :: !Text
    -- ^ Unique plugin identifier.
  , rmVersion       :: !Text
    -- ^ Plugin version string (informational).
  , rmDescription   :: !Text
    -- ^ Human-readable description.
  , rmGenerator     :: !(Maybe RPCGeneratorDecl)
    -- ^ Generator pipeline declaration (if the plugin seeds data).
  , rmSimulation    :: !(Maybe RPCSimulationDecl)
    -- ^ Simulation DAG declaration (if the plugin ticks an overlay).
  , rmOverlay       :: !(Maybe RPCOverlayDecl)
    -- ^ Overlay schema reference (if the plugin owns an overlay).
  , rmCapabilities  :: ![RPCCapability]
    -- ^ Requested capabilities.
  , rmParameters    :: ![RPCParamSpec]
    -- ^ User-facing configuration parameters.
  , rmDataResources :: ![DataResourceSchema]
    -- ^ Plugin-declared data resource schemas.
  , rmDataDirectory :: !(Maybe Text)
    -- ^ Data subdirectory relative to the world save path.
  } deriving (Eq, Show, Generic)

instance FromJSON RPCManifest where
  parseJSON = withObject "RPCManifest" $ \o -> do
    name   <- o .:  "name"
    ver    <- o .:  "version"
    desc   <- o .:? "description"
    gen    <- o .:? "generator"
    sim    <- o .:? "simulation"
    ov     <- o .:? "overlay"
    caps   <- o .:? "capabilities"
    config <- o .:? "config"
    params <- case config of
      Just co -> withObject "config" (\c -> c .:? "parameters") co
      Nothing -> pure Nothing
    dataRes  <- o .:? "dataResources"
    dataDir  <- o .:? "dataDirectory"
    pure RPCManifest
      { rmName          = name
      , rmVersion       = ver
      , rmDescription   = maybe "" id desc
      , rmGenerator     = gen
      , rmSimulation    = sim
      , rmOverlay       = ov
      , rmCapabilities  = maybe [] id caps
      , rmParameters    = maybe [] id params
      , rmDataResources = maybe [] id dataRes
      , rmDataDirectory = dataDir
      }

instance ToJSON RPCManifest where
  toJSON rm = object $
    [ "name"    .= rmName rm
    , "version" .= rmVersion rm
    ] <>
    [ "description" .= rmDescription rm | rmDescription rm /= "" ] <>
    [ "generator"   .= g  | Just g  <- [rmGenerator rm] ] <>
    [ "simulation"  .= s  | Just s  <- [rmSimulation rm] ] <>
    [ "overlay"     .= ov | Just ov <- [rmOverlay rm] ] <>
    [ "capabilities" .= rmCapabilities rm | not (null (rmCapabilities rm)) ] <>
    [ "config" .= object ["parameters" .= rmParameters rm]
    | not (null (rmParameters rm))
    ] <>
    [ "dataResources" .= rmDataResources rm
    | not (null (rmDataResources rm))
    ] <>
    [ "dataDirectory" .= dd
    | Just dd <- [rmDataDirectory rm]
    ]

------------------------------------------------------------------------
-- Parsing
------------------------------------------------------------------------

-- | Parse a manifest from a raw JSON 'BS.ByteString'.
parseManifest :: BS.ByteString -> Either Text RPCManifest
parseManifest bs =
  case Aeson.eitherDecodeStrict' bs of
    Left err   -> Left (Text.pack err)
    Right mani -> Right mani

-- | Parse a manifest from a file on disk.
parseManifestFile :: FilePath -> IO (Either Text RPCManifest)
parseManifestFile path = do
  bs <- BS.readFile path
  pure (parseManifest bs)

------------------------------------------------------------------------
-- Validation
------------------------------------------------------------------------

-- | Errors found during manifest validation.
data ManifestError
  = ManifestEmptyName
    -- ^ Plugin name is empty.
  | ManifestEmptyVersion
    -- ^ Plugin version is empty.
  | ManifestSimWithoutOverlay
    -- ^ Simulation declared without an overlay to own.
  | ManifestWriteTerrainWithoutSim
    -- ^ @writeTerrain@ capability without a simulation declaration.
  | ManifestNoParticipation
    -- ^ Plugin declares neither generator, simulation, nor data resources.
  | ManifestDataReadWithoutCapability
    -- ^ Data resources declared without @dataRead@ capability.
  | ManifestDataWriteWithoutCapability
    -- ^ Data resources with write operations declared without
    --   @dataWrite@ capability.
  deriving (Eq, Ord, Show, Read)

-- | Validate structural invariants of a parsed manifest.
--
-- Returns a list of errors (empty means valid).
validateManifest :: RPCManifest -> [ManifestError]
validateManifest rm = concat
  [ [ ManifestEmptyName | Text.null (rmName rm) ]
  , [ ManifestEmptyVersion | Text.null (rmVersion rm) ]
  , [ ManifestSimWithoutOverlay
    | Just _ <- [rmSimulation rm]
    , Nothing <- [rmOverlay rm]
    ]
  , [ ManifestWriteTerrainWithoutSim
    | CapWriteTerrain `elem` rmCapabilities rm
    , Nothing <- [rmSimulation rm]
    ]
  , [ ManifestNoParticipation
    | Nothing <- [rmGenerator rm]
    , Nothing <- [rmSimulation rm]
    , null (rmDataResources rm)
    ]
  , [ ManifestDataReadWithoutCapability
    | not (null (rmDataResources rm))
    , CapDataRead `notElem` rmCapabilities rm
    ]
  , [ ManifestDataWriteWithoutCapability
    | any hasWriteOps (rmDataResources rm)
    , CapDataWrite `notElem` rmCapabilities rm
    ]
  ]
  where
    hasWriteOps drs =
      let ops = drsOperations drs
      in doCreate ops || doUpdate ops || doDelete ops

------------------------------------------------------------------------
-- Queries
------------------------------------------------------------------------

-- | Does this plugin declare @writeTerrain@ capability?
-- If so, its sim node should be a 'SimNodeWriter'.
manifestWritesTerrain :: RPCManifest -> Bool
manifestWritesTerrain manifest =
  elem CapWriteTerrain (rmCapabilities manifest)
    || elem CapWriteWorld (rmCapabilities manifest)

-- | Does this plugin participate in the generator pipeline?
manifestHasGenerator :: RPCManifest -> Bool
manifestHasGenerator rm = case rmGenerator rm of
  Just _  -> True
  Nothing -> False

-- | Does this plugin participate in the simulation DAG?
manifestHasSimulation :: RPCManifest -> Bool
manifestHasSimulation rm = case rmSimulation rm of
  Just _  -> True
  Nothing -> False

-- | Does this plugin own an overlay?
manifestHasOverlay :: RPCManifest -> Bool
manifestHasOverlay rm = case rmOverlay rm of
  Just _  -> True
  Nothing -> False
