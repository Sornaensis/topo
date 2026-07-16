{-# LANGUAGE OverloadedStrings #-}

-- | Topo Plugin SDK — the public API for building topo plugins.
--
-- This module re-exports everything a plugin author needs to define
-- and run a topo plugin.  Import this single module in your plugin's
-- @Main.hs@.
--
-- === Quick Start
--
-- @
-- import Topo.Plugin.SDK
--
-- main :: IO ()
-- main = runPluginWithManifestCommand myPlugin
--
-- myPlugin :: PluginDef
-- myPlugin = defaultPluginDef
--   { pdName    = "my-terrain-mod"
--   , pdVersion = "0.1.0"
--   , pdGenerator = Just GeneratorDef
--       { gdInsertAfter = "erosion"
--       , gdRequires    = ["erosion"]
--       , gdRun         = \\ctx -> do
--           pcLog ctx "Hello from my plugin!"
--           reportPluginProgress ctx "working" 0.5
--           pure (Right defaultGeneratorTickResult)
--       }
--   }
-- @
--
-- === Architecture
--
-- A topo plugin is a standalone executable that communicates with
-- topo-seer via length-prefixed JSON messages over named pipes
-- (Windows) or Unix domain sockets.
--
-- The SDK handles:
--
-- * Manifest generation from your 'PluginDef'
-- * Explicit manifest-only install command before topo-seer discovery
-- * Transport connection and message framing
-- * RPC message dispatch (generator invocation, simulation ticks)
-- * Correlated logging and progress reporting from callbacks
-- * Clean shutdown handling
--
-- You only need to implement the business logic in 'GeneratorDef'
-- and\/or 'SimulationDef' callbacks.
module Topo.Plugin.SDK
  ( -- * Plugin definition
    PluginDef(..)
  , defaultPluginDef
    -- * Parameters
  , ParamDef(..)
  , ParamType(..)
    -- * Generator
  , GeneratorDef(..)
  , GeneratorScopeDef(..)
  , GeneratorTickResult(..)
  , defaultGeneratorTickResult
    -- * Simulation
  , SimulationDef(..)
  , SimulationScopeDef(..)
  , SimulationTickResult(..)
  , defaultSimulationTickResult
  , SimulationCatchUpPolicy(..)
  , SimulationScheduleDecl(..)
  , defaultScheduleDecl
  , hourlyScheduleDecl
    -- * Context
  , PluginContext(..)
  , GeneratorContext(..)
  , SimulationContext(..)
  , DataContext(..)
  , reportPluginProgress
  , reportGeneratorProgress
  , reportSimulationProgress
  , reportDataProgress
    -- * Invocation scopes
  , RPCInvocationScopeDecl(..)
  , RPCScopeInput(..)
  , RPCScopeOutput(..)
  , RPCScopeBudgets(..)
  , RPCChunkSelector(..)
  , TerrainSection(..)
  , ResolvedInvocationScope(..)
  , ResolvedDataResourceScope(..)
  , RPCDataOperation(..)
  , legacyGeneratorScope
  , legacySimulationScope
    -- * Data resource schemas
  , DataResourceSchema(..)
  , DataPagination(..)
  , currentDataResourceSchemaVersion
  , defaultDataResourceVersion
  , defaultDataPagination
  , DataFieldDef(..)
  , DataFieldType(..)
  , DataConstructorDef(..)
  , DataOperations(..)
  , noOperations
  , allOperations
    -- * Data service handlers
  , DataResourceDef(..)
  , DataHandler(..)
  , noDataHandler
  , ScopedDataHandler(..)
  , noScopedDataHandler
  , DataQuery(..)
  , DataRecord(..)
  , DataMutation(..)
  , QueryResult(..)
  , MutateResult(..)
  , DataResourceErrorCode(..)
  , DataResourceFailure(..)
  , dataResourceErrorCodeText
  , dataResourceFailureText
    -- * Typed payload helpers
  , decodeOwnOverlay
  , decodeDependencyOverlay
  , decodeScopedOwnOverlay
  , decodeScopedDependencyOverlay
  , decodeTerrainPayload
  , decodeTerrainPayloadWithLimits
  , decodeTerrainWritesPayload
  , decodeTerrainWritesPayloadWithLimits
  , encodeOverlayPayload
  , encodeTerrainPayload
  , encodeTerrainPayloadWithLimits
  , encodeTerrainWritesPayload
  , encodeTerrainWritesPayloadWithLimits
  , simulationResultFromOverlay
  , simulationResultWithTerrainWrites
  , simulationResultWithTerrainWritesWithLimits
  , generatorResultFromTerrain
  , generatorResultFromScopedTerrain
  , generatorResultFromTerrainWithLimits
  , generatorResultFromTerrainAndOverlay
  , generatorResultFromTerrainAndOverlayWithLimits
    -- * Overlay data access (re-exported from "Topo.Plugin.SDK.Overlay")
  , module Topo.Plugin.SDK.Overlay
    -- * Terrain data access (re-exported from "Topo.Plugin.SDK.Terrain")
  , module Topo.Plugin.SDK.Terrain
    -- * Entry point
  , runPlugin
  , runPluginWithManifestCommand
  , runPluginSession
  , runPluginSessionWithLimits
  , SDKSessionError(..)
  , RPCPayloadLimits
  , mkRPCPayloadLimits
  , defaultRPCPayloadLimits
  , rplMaxFrameSizeBytes
  , rplMaxDecodedTerrainBytes
    -- * Manifest utilities
  , pluginManifestFileName
  , Capability(..)
  , RPCCapability
  , RPCStartPolicy(..)
  , RPCRestartMode(..)
  , RPCUIHints(..)
  , RPCExternalDataSourceCapability(..)
  , RPCExternalDataSourceAccess(..)
  , RPCExternalDataSourceConfigOrigin(..)
  , RPCExternalDataSourceConfigRef(..)
  , RPCExternalDataSourceStatusState(..)
  , RPCExternalDataSourceAvailability(..)
  , RPCExternalDataSourceHealth(..)
  , RPCExternalDataSourceAccessMode(..)
  , RPCExternalDataSourceStatus(..)
  , RPCExternalDataSourceGrant(..)
  , RPCExternalDataSourceDecl(..)
  , RPCExternalDataSourceRef(..)
  , RPCExternalDataSourceGrantMessage(..)
  , RPCExternalDataSourceGrantRevocation(..)
  , RPCExternalDataSourceOperation(..)
  , RPCExternalDataSourceOperationResult(..)
  , RPCExternalDataSourceStatusRequest(..)
  , RPCExternalDataSourceStatusEntry(..)
  , RPCExternalDataSourceStatusReport(..)
  , defaultRPCStartPolicy
  , defaultRPCUIHints
  , defaultRPCExternalDataSourceStatus
  , generateManifest
  , writeManifest
  , writePluginManifest
  , writePluginManifestToDirectory
  ) where

import Topo.Plugin.SDK.Types
import Topo.Plugin.SDK.Overlay
import Topo.Plugin.SDK.Terrain
import Topo.Plugin.SDK.Payload
import Topo.Plugin.SDK.Runner
  ( runPlugin
  , runPluginWithManifestCommand
  , runPluginSession
  , runPluginSessionWithLimits
  , SDKSessionError(..)
  , pluginManifestFileName
  , generateManifest
  , writeManifest
  , writePluginManifest
  , writePluginManifestToDirectory
  )
import Topo.Plugin.RPC.Transport
  ( RPCPayloadLimits
  , mkRPCPayloadLimits
  , defaultRPCPayloadLimits
  , rplMaxFrameSizeBytes
  , rplMaxDecodedTerrainBytes
  )
import Topo.Plugin.DataResource
  ( DataResourceSchema(..)
  , DataPagination(..)
  , currentDataResourceSchemaVersion
  , defaultDataResourceVersion
  , defaultDataPagination
  , DataFieldDef(..)
  , DataFieldType(..)
  , DataConstructorDef(..)
  , DataOperations(..)
  , noOperations
  , allOperations
  )
import Topo.Plugin.RPC.DataService
  ( DataQuery(..)
  , DataRecord(..)
  , DataMutation(..)
  , QueryResult(..)
  , MutateResult(..)
  , DataResourceErrorCode(..)
  , DataResourceFailure(..)
  , dataResourceErrorCodeText
  , dataResourceFailureText
  )
import Topo.Plugin.RPC.Manifest
  ( Capability(..)
  , RPCCapability
  , RPCStartPolicy(..)
  , RPCRestartMode(..)
  , RPCUIHints(..)
  , RPCExternalDataSourceCapability(..)
  , RPCExternalDataSourceAccess(..)
  , RPCExternalDataSourceConfigOrigin(..)
  , RPCExternalDataSourceConfigRef(..)
  , RPCExternalDataSourceStatusState(..)
  , RPCExternalDataSourceAvailability(..)
  , RPCExternalDataSourceHealth(..)
  , RPCExternalDataSourceAccessMode(..)
  , RPCExternalDataSourceStatus(..)
  , RPCExternalDataSourceGrant(..)
  , RPCExternalDataSourceDecl(..)
  , RPCExternalDataSourceRef(..)
  , defaultRPCStartPolicy
  , defaultRPCUIHints
  , defaultRPCExternalDataSourceStatus
  )
import Topo.Plugin.RPC.Scope
  ( RPCInvocationScopeDecl(..)
  , RPCScopeInput(..)
  , RPCScopeOutput(..)
  , RPCScopeBudgets(..)
  , RPCChunkSelector(..)
  , TerrainSection(..)
  , ResolvedInvocationScope(..)
  , ResolvedDataResourceScope(..)
  , RPCDataOperation(..)
  , legacyGeneratorScope
  , legacySimulationScope
  )
import Topo.Plugin.RPC.ExternalDataSource
  ( RPCExternalDataSourceGrantMessage(..)
  , RPCExternalDataSourceGrantRevocation(..)
  , RPCExternalDataSourceOperation(..)
  , RPCExternalDataSourceOperationResult(..)
  , RPCExternalDataSourceStatusRequest(..)
  , RPCExternalDataSourceStatusEntry(..)
  , RPCExternalDataSourceStatusReport(..)
  )
import Topo.Simulation.Schedule
  ( SimulationCatchUpPolicy(..)
  , SimulationScheduleDecl(..)
  , defaultScheduleDecl
  , hourlyScheduleDecl
  )
