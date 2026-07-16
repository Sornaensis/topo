-- | Shared actor-handle bundle reused across input and UI action plumbing.
module Actor.UiActions.Handles
  ( ActorHandles(..)
  , mkActorHandles
  , publishUiMutation
  , publishLogMutation
  , publishUiAndLogMutation
  ) where

import Actor.AtlasManager (AtlasManager)
import Actor.Data (Data)
import Actor.Log (Log, LogSnapshot, getLogSnapshot)
import Actor.PluginManager (PluginManager)
import Actor.Simulation (Simulation)
import Actor.SnapshotReceiver
  ( DataSnapshotRef
  , SnapshotVersion
  , SnapshotVersionRef
  , TerrainSnapshotRef
  , logSnapshotUpdate
  , publishSnapshot
  , uiSnapshotUpdate
  , withLogSnapshot
  )
import Actor.Terrain (Terrain)
import Actor.UI (Ui, UiState, getUiSnapshot)
import Control.Concurrent.MVar (MVar, newMVar)
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
  , ahWidgetActionLock :: !(MVar ())
    -- ^ Per-application serialization owner for transport-neutral widget actions.
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
  -> IO ActorHandles
mkActorHandles uiHandle logHandle dataHandle terrainHandle atlasManagerHandle dataSnapRef terrainSnapRef versionRef pluginManagerHandle simulationHandle historyRef = do
  widgetActionLock <- newMVar ()
  pure ActorHandles
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
    , ahWidgetActionLock = widgetActionLock
    }

-- | Drain the UI mailbox and publish the resulting state in one epoch.
publishUiMutation :: ActorHandles -> IO (SnapshotVersion, UiState)
publishUiMutation handles = do
  uiSnapshot <- getUiSnapshot (ahUiHandle handles)
  version <- publishSnapshot (ahSnapshotVersionRef handles) (uiSnapshotUpdate uiSnapshot)
  pure (version, uiSnapshot)

-- | Drain the log mailbox and publish the resulting state in one epoch.
publishLogMutation :: ActorHandles -> IO (SnapshotVersion, LogSnapshot)
publishLogMutation handles = do
  logSnapshot <- getLogSnapshot (ahLogHandle handles)
  version <- publishSnapshot (ahSnapshotVersionRef handles) (logSnapshotUpdate logSnapshot)
  pure (version, logSnapshot)

-- | Drain both actor mailboxes and publish their state in one epoch.
publishUiAndLogMutation :: ActorHandles -> IO (SnapshotVersion, UiState, LogSnapshot)
publishUiAndLogMutation handles = do
  uiSnapshot <- getUiSnapshot (ahUiHandle handles)
  logSnapshot <- getLogSnapshot (ahLogHandle handles)
  version <- publishSnapshot
    (ahSnapshotVersionRef handles)
    (withLogSnapshot logSnapshot (uiSnapshotUpdate uiSnapshot))
  pure (version, uiSnapshot, logSnapshot)