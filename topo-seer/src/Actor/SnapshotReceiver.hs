-- | Lock-free snapshot infrastructure for the render loop.
--
-- Each domain (UI, Log, Data, Terrain) publishes to its own 'IORef'.
-- A shared 'SnapshotVersionRef' is atomically bumped by any writer so
-- the render thread can detect changes with a single read.
--
-- The render loop composes a 'RenderSnapshot' by reading all four
-- domain refs each frame, eliminating the former SnapshotReceiver actor
-- and the data-clobbering risk of a single composite IORef.
module Actor.SnapshotReceiver
  ( RenderSnapshot(..)
  , SnapshotVersion(..)
    -- * Data snapshot ref
  , DataSnapshotRef
  , newDataSnapshotRef
  , writeDataSnapshot
  , readDataSnapshot
    -- * Terrain snapshot ref
  , TerrainSnapshotRef
  , newTerrainSnapshotRef
  , writeTerrainSnapshot
  , readTerrainSnapshot
    -- * Version counter
  , SnapshotVersionRef
  , newSnapshotVersionRef
  , readSnapshotVersion
  , bumpSnapshotVersion
  ) where

import Actor.Data (DataSnapshot(..), TerrainSnapshot(..))
import Actor.Log (LogSnapshot(..))
import Actor.UI (UiState(..))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Word (Word64)

-- | Composite render snapshot consumed by the render loop.
--
-- Contains one sub-snapshot per domain: UI state, log, data, and terrain.
data RenderSnapshot = RenderSnapshot
  { rsUi      :: !UiState
  , rsLog     :: !LogSnapshot
  , rsData    :: !DataSnapshot
  , rsTerrain :: !TerrainSnapshot
  } deriving (Eq, Show)

-- | Monotonic version for the latest cached snapshot.
newtype SnapshotVersion = SnapshotVersion { unSnapshotVersion :: Word64 }
  deriving (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- Data snapshot
-- ---------------------------------------------------------------------------

-- | Lock-free ref for the latest 'DataSnapshot'.
type DataSnapshotRef = IORef DataSnapshot

-- | Create a new 'DataSnapshotRef' with the given initial value.
newDataSnapshotRef :: DataSnapshot -> IO DataSnapshotRef
newDataSnapshotRef = newIORef

-- | Write a new 'DataSnapshot'.  Not atomic with respect to other
-- sub-snapshot refs — each domain ref is independent.
writeDataSnapshot :: DataSnapshotRef -> DataSnapshot -> IO ()
writeDataSnapshot = writeIORef

-- | Read the latest 'DataSnapshot'.
readDataSnapshot :: DataSnapshotRef -> IO DataSnapshot
readDataSnapshot = readIORef

-- ---------------------------------------------------------------------------
-- Terrain snapshot
-- ---------------------------------------------------------------------------

-- | Lock-free ref for the latest 'TerrainSnapshot'.
type TerrainSnapshotRef = IORef TerrainSnapshot

-- | Create a new 'TerrainSnapshotRef' with the given initial value.
newTerrainSnapshotRef :: TerrainSnapshot -> IO TerrainSnapshotRef
newTerrainSnapshotRef = newIORef

-- | Write a new 'TerrainSnapshot'.
writeTerrainSnapshot :: TerrainSnapshotRef -> TerrainSnapshot -> IO ()
writeTerrainSnapshot = writeIORef

-- | Read the latest 'TerrainSnapshot'.
readTerrainSnapshot :: TerrainSnapshotRef -> IO TerrainSnapshot
readTerrainSnapshot = readIORef

-- ---------------------------------------------------------------------------
-- Version counter
-- ---------------------------------------------------------------------------

-- | Shared monotonic version counter bumped by any snapshot writer.
-- The render loop uses this for O(1) staleness detection.
type SnapshotVersionRef = IORef SnapshotVersion

-- | Create a new version counter starting at 0.
newSnapshotVersionRef :: IO SnapshotVersionRef
newSnapshotVersionRef = newIORef (SnapshotVersion 0)

-- | Read the current snapshot version.
readSnapshotVersion :: SnapshotVersionRef -> IO SnapshotVersion
readSnapshotVersion = readIORef

-- | Atomically bump the version counter.  Safe to call from any thread.
--
-- __Ordering contract__: when updating domain refs (e.g. 'writeDataSnapshot',
-- 'writeTerrainSnapshot'), call this /after/ the domain writes so the render
-- loop never observes a new version with stale domain data.  Bumping without
-- prior domain writes is fine for UI-only changes (the reader will simply
-- re-read unchanged refs).
bumpSnapshotVersion :: SnapshotVersionRef -> IO ()
bumpSnapshotVersion ref =
  atomicModifyIORef' ref (\(SnapshotVersion v) -> (SnapshotVersion (v + 1), ()))
