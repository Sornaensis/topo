{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Core types for defining topo plugins.
--
-- A plugin is defined by a 'PluginDef' which declares:
--
-- * Identity and manifest metadata ('pdName', 'pdVersion', 'pdDescription')
-- * User-facing parameters ('pdParams')
-- * Optional generator stage ('pdGenerator')
-- * Optional simulation node ('pdSimulation')
-- * Inferred plus explicit manifest capabilities ('pdCapabilities')
-- * Optional external data-source provider and consumer declarations whose
--   migrations, schemas, connection details, and consistency rules stay owned
--   by provider plugins, adapters, or external systems.
--
-- The SDK runtime ('Topo.Plugin.SDK.Runner') uses these definitions
-- to handle the RPC lifecycle: manifest serving, parameter exchange,
-- and invocation dispatch.
module Topo.Plugin.SDK.Types
  ( -- * Plugin definition
    PluginDef(..)
  , ParamDef(..)
  , ParamType(..)
    -- * Capabilities
  , GeneratorDef(..)
  , SimulationDef(..)
  , GeneratorTickResult(..)
  , SimulationTickResult(..)
  , defaultGeneratorTickResult
  , defaultSimulationTickResult
    -- * Data service
  , DataResourceDef(..)
  , DataHandler(..)
  , noDataHandler
    -- * Plugin context
  , PluginContext(..)
  , reportPluginProgress
    -- * Defaults
  , defaultPluginDef
  ) where

import Data.Aeson (Value(..))
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Word (Word64)
import Topo.Plugin.DataResource (DataResourceSchema)
import Topo.Plugin.RPC.Manifest
  ( RPCCapability
  , RPCExternalDataSourceDecl, RPCExternalDataSourceRef
  , RPCStartPolicy, RPCUIHints
  , defaultRPCStartPolicy, defaultRPCUIHints
  )
import Topo.Plugin.RPC.ExternalDataSource
  ( RPCExternalDataSourceGrantMessage
  , RPCExternalDataSourceGrantRevocation
  )
import Topo.Plugin.RPC.DataService
  ( DataQuery, DataRecord, DataMutation
  , QueryResult, MutateResult
  )
import Topo.Simulation.Schedule (SimulationScheduleDecl)
import Topo.World (TerrainWorld)

------------------------------------------------------------------------
-- Parameters
------------------------------------------------------------------------

-- | Scalar type for a plugin parameter.
data ParamType
  = PFloat
  -- ^ Floating-point parameter.
  | PInt
  -- ^ Integer parameter.
  | PBool
  -- ^ Boolean parameter.
  deriving (Eq, Ord, Show, Read)

-- | A user-facing configuration parameter.
--
-- These are rendered as sliders or checkboxes in topo-seer.
data ParamDef = ParamDef
  { paramName    :: !Text
    -- ^ Internal parameter name (used as key in config maps).
  , paramLabel   :: !Text
    -- ^ Human-readable label for UI display.
  , paramType    :: !ParamType
    -- ^ Parameter scalar type.
  , paramDefault :: !Value
    -- ^ Default value (JSON scalar).
  , paramMin     :: !(Maybe Value)
    -- ^ Optional minimum value for numeric parameters.
  , paramMax     :: !(Maybe Value)
    -- ^ Optional maximum value for numeric parameters.
  , paramTooltip :: !Text
    -- ^ Tooltip text shown on hover.
  } deriving (Eq, Show)

------------------------------------------------------------------------
-- Generator
------------------------------------------------------------------------

-- | Generator pipeline participation.
--
-- Defines a stage that runs during world generation, inserted after
-- a specified built-in stage.
data GeneratorDef = GeneratorDef
  { gdInsertAfter :: !Text
    -- ^ Canonical stage name after which this generator runs
    -- (e.g. @\"biomes\"@, @\"rivers\"@).
  , gdRequires    :: ![Text]
    -- ^ Stage names that must have run before this generator.
  , gdRun         :: PluginContext -> IO (Either Text GeneratorTickResult)
    -- ^ Generator implementation. Receives the current world and
    -- parameters; returns transport payloads for generator result.
  }

------------------------------------------------------------------------
-- Simulation
------------------------------------------------------------------------

-- | Simulation DAG participation.
--
-- Defines a simulation node that ticks during the simulation phase,
-- after the world has been generated.
data SimulationDef = SimulationDef
  { sdDependencies :: ![Text]
    -- ^ Simulation node IDs that must tick before this node.
  , sdSchedule :: !(Maybe SimulationScheduleDecl)
    -- ^ Optional static cadence declaration. 'Nothing' emits the hourly default.
  , sdTick         :: PluginContext -> IO (Either Text SimulationTickResult)
    -- ^ Simulation tick implementation.
  }

-- | Result payload returned by a generator callback.
data GeneratorTickResult = GeneratorTickResult
  { gtrTerrain  :: !Value
    -- ^ Generator terrain payload for @generator_result.terrain@. The host
    -- merges this for any manifest with @generator@ participation; it does not
    -- require @writeTerrain@ or @writeWorld@.
  , gtrOverlay  :: !(Maybe Value)
    -- ^ Optional overlay seed payload for @generator_result.overlay@. The host
    -- applies it only when the manifest owns an overlay and has @writeOverlay@
    -- or @writeWorld@.
  , gtrMetadata :: !(Maybe Value)
    -- ^ Optional metadata payload for @generator_result.metadata@.
  } deriving (Eq, Show)

-- | Result payload returned by a simulation callback.
data SimulationTickResult = SimulationTickResult
  { strOverlay       :: !Value
    -- ^ Updated overlay payload for @simulation_result.overlay@.
  , strTerrainWrites :: !(Maybe Value)
    -- ^ Optional terrain writes payload for @simulation_result.terrain_writes@.
  } deriving (Eq, Show)

-- | Default empty generator payload.
defaultGeneratorTickResult :: GeneratorTickResult
defaultGeneratorTickResult = GeneratorTickResult
  { gtrTerrain = Object mempty
  , gtrOverlay = Nothing
  , gtrMetadata = Nothing
  }

-- | Default empty simulation payload.
defaultSimulationTickResult :: SimulationTickResult
defaultSimulationTickResult = SimulationTickResult
  { strOverlay = Object mempty
  , strTerrainWrites = Nothing
  }

------------------------------------------------------------------------
-- Context
------------------------------------------------------------------------

-- | Runtime context passed to generator, simulation, and data callbacks.
--
-- Provides access to the current terrain, parameters, seed, host logging, and
-- interim progress reporting for the current invocation.
data PluginContext = PluginContext
  { pcWorld  :: !TerrainWorld
    -- ^ Current terrain world state.
  , pcParams :: !(Map Text Value)
    -- ^ Current parameter values (user may have changed defaults).
  , pcTerrain :: !Value
    -- ^ Raw terrain payload from host invocation.
  , pcOwnOverlay :: !(Maybe Value)
    -- ^ Raw overlay payload owned by this plugin, for simulation ticks.
  , pcOverlays :: !(Map Text Value)
    -- ^ Raw dependency overlay payloads keyed by overlay name.
  , pcSeed   :: !Word64
    -- ^ World generation seed.
  , pcLog    :: Text -> IO ()
    -- ^ Logging callback (messages appear in topo-seer log panel).
  , pcProgress :: Text -> Double -> IO ()
    -- ^ Progress callback for the current invocation. The fraction is absolute
    -- progress for this callback, conventionally from @0.0@ through @1.0@;
    -- callers do not need to emit either endpoint, and the final result or
    -- error remains authoritative.
  , pcWorldPath :: !(Maybe FilePath)
    -- ^ Path to the current world save directory, if known.
  }

-- | Report progress for the current plugin callback.
--
-- This is a named helper for 'pcProgress' to avoid ambiguity with the host-side
-- @Topo.Plugin.reportProgress@ helper.
reportPluginProgress :: PluginContext -> Text -> Double -> IO ()
reportPluginProgress context message fraction =
  pcProgress context message fraction

------------------------------------------------------------------------
-- Data service
------------------------------------------------------------------------

-- | A data resource definition pairing a schema with its handler.
--
-- Plugins declare data resources via 'pdDataResources' on 'PluginDef'.
-- The 'drdSchema' describes the resource to the host; the 'drdHandler'
-- implements the actual query and mutation logic.
data DataResourceDef = DataResourceDef
  { drdSchema  :: !DataResourceSchema
    -- ^ Schema describing fields, operations, and key.
  , drdHandler :: !DataHandler
    -- ^ Callbacks that implement the resource's CRUD operations.
  }

-- | Callbacks for handling data queries and mutations.
--
-- Each callback is optional (@Maybe@).  When the host sends a
-- query or mutation for an operation whose callback is @Nothing@,
-- the SDK returns a "not supported" error automatically.
data DataHandler = DataHandler
  { dhQuery      :: !(Maybe (PluginContext -> DataQuery -> IO (Either Text QueryResult)))
    -- ^ Handle a data query (list, by-key, by-hex, by-field).
  , dhMutate     :: !(Maybe (PluginContext -> DataMutation -> IO (Either Text MutateResult)))
    -- ^ Handle a data mutation (create, update, delete, set-hex).
  }

-- | A data handler with no capabilities — all callbacks are 'Nothing'.
--
-- Use this as a starting point and override individual fields:
--
-- @
-- myHandler = noDataHandler
--   { dhQuery = Just $ \\ctx query -> ...
--   }
-- @
noDataHandler :: DataHandler
noDataHandler = DataHandler
  { dhQuery  = Nothing
  , dhMutate = Nothing
  }

------------------------------------------------------------------------
-- Plugin definition
------------------------------------------------------------------------

-- | Complete definition of a topo plugin.
--
-- The SDK treats this value as the source of truth for manifest v3
-- generation. Runtime protocol bounds are supplied by the SDK; plugin authors
-- can fill in optional Topo host bounds, supervision policy, UI metadata,
-- explicit capabilities, and backend-neutral external data-source declarations
-- here instead of hand-editing @manifest.json@.
--
-- Create a 'PluginDef' and pass it to
-- 'Topo.Plugin.SDK.runPluginWithManifestCommand' so plugin authors can write
-- @manifest.json@ during install before normal host-launched RPC startup.
--
-- === Minimal example
--
-- @
-- myPlugin :: PluginDef
-- myPlugin = defaultPluginDef
--   { pdName    = "my-terrain-mod"
--   , pdVersion = "0.1.0"
--   , pdGenerator = Just GeneratorDef
--       { gdInsertAfter = "erosion"
--       , gdRequires    = ["erosion"]
--       , gdRun         = \\ctx -> pure (Right defaultGeneratorTickResult)
--       }
--   }
-- @
data PluginDef = PluginDef
  { pdName       :: !Text
    -- ^ Unique plugin identifier. Must match the directory name
    -- under @~\/.topo\/plugins\/@.
  , pdVersion    :: !Text
    -- ^ Plugin version string (informational).
  , pdDescription :: !(Maybe Text)
    -- ^ Optional human-readable manifest description. When 'Nothing', the SDK
    -- emits a concise @name vversion@ description for transition continuity.
  , pdRuntimeTopoMin :: !(Maybe Text)
    -- ^ Optional minimum compatible Topo host version for manifest v3
    -- @runtime.topo.min@. The RPC protocol range is set by the SDK.
  , pdRuntimeTopoMax :: !(Maybe Text)
    -- ^ Optional maximum compatible Topo host version for manifest v3
    -- @runtime.topo.max@. The RPC protocol range is set by the SDK.
  , pdParams     :: ![ParamDef]
    -- ^ User-facing configuration parameters.
  , pdSchemaFile :: !(Maybe FilePath)
    -- ^ Path to an overlay @.toposchema@ file (relative to plugin dir).
  , pdGenerator  :: !(Maybe GeneratorDef)
    -- ^ Generator pipeline participation.
  , pdSimulation :: !(Maybe SimulationDef)
    -- ^ Simulation DAG participation.
  , pdCapabilities :: ![RPCCapability]
    -- ^ Explicit additional manifest capabilities. The SDK adds these to its
    -- safe inferred capabilities. Use this for capabilities that cannot be
    -- inferred statically, such as @writeTerrain@ for simulation terrain writes
    -- or @writeOverlay@ for generator-only overlay output. @writeWorld@ also
    -- satisfies host overlay-write checks on manifests that can request it.
    -- Generator terrain output itself is implicit in @generator@ participation.
  , pdDataDirectory :: !(Maybe FilePath)
    -- ^ Data subdirectory relative to the world save path.
  , pdDataResources :: ![DataResourceDef]
    -- ^ Data resource definitions with schemas and handlers.
  , pdUiHints :: !RPCUIHints
    -- ^ Optional UI presentation hints emitted into manifest v3.
  , pdExternalDataSources :: ![RPCExternalDataSourceDecl]
    -- ^ Provider-owned external data sources advertised in manifest v3.  The SDK
    -- serializes backend-neutral declarations, grants, status, provider IDs,
    -- availability/health, access-mode/capability-scope policy, opaque config
    -- references, diagnostics, and opaque metadata only; providers/adapters/
    -- external systems own migrations, schemas, connection details, and
    -- consistency rules.
  , pdExternalDataSourceRefs :: ![RPCExternalDataSourceRef]
    -- ^ External data sources consumed by this plugin.  References bind to
    -- provider-owned contracts, backend-neutral status metadata, and opaque
    -- config references without making topo prescribe backend-specific migration
    -- tables or schema rules.
  , pdOnExternalDataSourceGrant :: !(Maybe (RPCExternalDataSourceGrantMessage -> IO ()))
    -- ^ Optional callback invoked when the host brokers an external data-source
    -- grant for this plugin. The payload remains backend-neutral and carries
    -- operation identity, provider identity, capability scope, opaque
    -- references, and diagnostics.
  , pdOnExternalDataSourceRevocation :: !(Maybe (RPCExternalDataSourceGrantRevocation -> IO ()))
    -- ^ Optional callback invoked when the host revokes or disables a brokered
    -- external data-source grant. The revocation payload carries the broker
    -- operation identity needed by ACK/result messages.
  , pdStartPolicy :: !RPCStartPolicy
    -- ^ Host-side process supervision policy emitted into manifest v3 when it
    -- differs from the default policy.
  }

-- | A minimal plugin definition with no capabilities.
--
-- Override fields as needed.
defaultPluginDef :: PluginDef
defaultPluginDef = PluginDef
  { pdName          = "unnamed-plugin"
  , pdVersion       = "0.0.0"
  , pdDescription   = Nothing
  , pdRuntimeTopoMin = Nothing
  , pdRuntimeTopoMax = Nothing
  , pdParams        = []
  , pdSchemaFile    = Nothing
  , pdGenerator     = Nothing
  , pdSimulation    = Nothing
  , pdCapabilities  = []
  , pdDataDirectory = Nothing
  , pdDataResources = []
  , pdUiHints = defaultRPCUIHints
  , pdExternalDataSources = []
  , pdExternalDataSourceRefs = []
  , pdOnExternalDataSourceGrant = Nothing
  , pdOnExternalDataSourceRevocation = Nothing
  , pdStartPolicy = defaultRPCStartPolicy
  }
