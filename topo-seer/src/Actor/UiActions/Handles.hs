-- | Shared actor-handle bundle reused across input and UI action plumbing.
module Actor.UiActions.Handles
  ( ActorHandles(..)
  , mkActorHandles
  ) where

import Actor.AtlasManager (AtlasManager)
import Actor.Data (Data)
import Actor.Log (Log)
import Actor.PluginManager (PluginManager)
import Actor.Simulation (Simulation)
import Actor.SnapshotReceiver (DataSnapshotRef, TerrainSnapshotRef, SnapshotVersionRef)
import Actor.Terrain (Terrain)
import Actor.UI (Ui)
import Data.IORef (IORef)
import Hyperspace.Actor (ActorHandle, Protocol)
import Seer.Editor.History (EditHistory)

-- | Shared actor handles needed by UI-triggered commands and input routing.
data ActorHandles = ActorHandles
  { ahUiHandle :: !(ActorHandle Ui (Protocol Ui))
  , ahLogHandle :: !(ActorHandle Log (Protocol Log))
  , ahDataHandle :: !(ActorHandle Data (Protocol Data))
  , ahTerrainHandle :: !(ActorHandle Terrain (Protocol Terrain))
  , ahAtlasManagerHandle :: !(ActorHandle AtlasManager (Protocol AtlasManager))
  , ahDataSnapshotRef :: !DataSnapshotRef
  , ahTerrainSnapshotRef :: !TerrainSnapshotRef
  , ahSnapshotVersionRef :: !SnapshotVersionRef
  , ahPluginManagerHandle :: !(ActorHandle PluginManager (Protocol PluginManager))
  , ahSimulationHandle :: !(ActorHandle Simulation (Protocol Simulation))
  , ahHistoryRef :: !(IORef EditHistory)
    -- ^ Mutable undo\/redo history for terrain editor edits.
  }

-- | Build a shared actor-handle bundle from the live application actors.
mkActorHandles
  :: ActorHandle Ui (Protocol Ui)
  -> ActorHandle Log (Protocol Log)
  -> ActorHandle Data (Protocol Data)
  -> ActorHandle Terrain (Protocol Terrain)
  -> ActorHandle AtlasManager (Protocol AtlasManager)
  -> DataSnapshotRef
  -> TerrainSnapshotRef
  -> SnapshotVersionRef
  -> ActorHandle PluginManager (Protocol PluginManager)
  -> ActorHandle Simulation (Protocol Simulation)
  -> IORef EditHistory
  -> ActorHandles
mkActorHandles uiHandle logHandle dataHandle terrainHandle atlasManagerHandle dataSnapRef terrainSnapRef versionRef pluginManagerHandle simulationHandle historyRef =
  ActorHandles
    { ahUiHandle = uiHandle
    , ahLogHandle = logHandle
    , ahDataHandle = dataHandle
    , ahTerrainHandle = terrainHandle
    , ahAtlasManagerHandle = atlasManagerHandle
    , ahDataSnapshotRef = dataSnapRef
    , ahTerrainSnapshotRef = terrainSnapRef
    , ahSnapshotVersionRef = versionRef
    , ahPluginManagerHandle = pluginManagerHandle
    , ahSimulationHandle = simulationHandle
    , ahHistoryRef = historyRef
    }