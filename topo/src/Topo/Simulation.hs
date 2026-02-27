-- | Core types for the simulation pipeline.
--
-- The simulation pipeline evolves overlays over time via a DAG of
-- 'SimNode's.  Each node owns one overlay and can read terrain plus
-- any declared overlay dependencies.
--
-- Two node constructors enforce a separation between read-only
-- overlay updates ('SimNodeReader') and nodes that may also mutate
-- terrain chunks ('SimNodeWriter').  Writers run sequentially after
-- all readers complete, accumulating 'TerrainWrites' that the caller
-- merges via 'applyTerrainWrites'.
--
-- See 'Topo.Simulation.DAG' for DAG construction and the wavefront
-- executor.
module Topo.Simulation
  ( -- * Node identity
    SimNodeId(..)
    -- * Simulation context
  , SimContext(..)
    -- * Simulation nodes
  , SimNode(..)
  , simNodeId
  , simNodeOverlayName
  , simNodeDependencies
    -- * Terrain writes
  , TerrainWrites(..)
  , emptyTerrainWrites
  , mergeTerrainWrites
  , applyTerrainWrites
    -- * Progress reporting
  , SimProgress(..)
  , SimStatus(..)
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Word (Word64)

import Topo.Calendar (CalendarDate, WorldTime)
import Topo.Overlay (Overlay, OverlayStore)
import Topo.Types (TerrainChunk, ClimateChunk, VegetationChunk)
import Topo.World (TerrainWorld(..))

-- ---------------------------------------------------------------------------
-- Node identity
-- ---------------------------------------------------------------------------

-- | Identifies a simulation node (one per overlay type).
newtype SimNodeId = SimNodeId { unSimNodeId :: Text }
  deriving (Eq, Ord, Show)

-- ---------------------------------------------------------------------------
-- Simulation context
-- ---------------------------------------------------------------------------

-- | Read-only view provided to a simulation node's tick function.
--
-- The terrain world is a snapshot from before the current tick
-- (for reader nodes) or the accumulated state including prior
-- writer mutations (for writer nodes).
data SimContext = SimContext
  { scTerrain    :: !TerrainWorld
    -- ^ Read-only terrain snapshot.  'twOverlays' is always empty —
    -- the DAG executor strips overlays to prevent stale-data access.
    -- Use 'scOverlays' for dependency overlay data instead.
  , scCalendar   :: !CalendarDate
    -- ^ Current calendar position derived from world time and planet.
  , scWorldTime  :: !WorldTime
    -- ^ Current simulation time.
  , scDeltaTicks :: !Word64
    -- ^ Number of ticks elapsed since the last simulation run.
  , scOverlays   :: !(Map Text Overlay)
    -- ^ Read-only: dependency overlays (only those declared).
  }

-- ---------------------------------------------------------------------------
-- Simulation nodes
-- ---------------------------------------------------------------------------

-- | A simulation node.  Two constructors enforce at the type level
-- that only writer nodes may produce 'TerrainWrites'.
data SimNode
  = SimNodeReader
    { snrId           :: !SimNodeId
      -- ^ Unique node identifier.
    , snrOverlayName  :: !Text
      -- ^ Which overlay this node owns and updates.
    , snrDependencies :: ![SimNodeId]
      -- ^ Which other sim nodes must run before this one.
    , snrReadTick     :: SimContext -> Overlay -> IO (Either Text Overlay)
      -- ^ Given read-only context and current overlay state,
      -- produce an updated overlay.  No terrain mutation.
    }
  | SimNodeWriter
    { snwId           :: !SimNodeId
      -- ^ Unique node identifier.
    , snwOverlayName  :: !Text
      -- ^ Which overlay this node owns and updates.
    , snwDependencies :: ![SimNodeId]
      -- ^ Which other sim nodes must run before this one.
    , snwWriteTick    :: SimContext -> Overlay -> IO (Either Text (Overlay, TerrainWrites))
      -- ^ Overlay update plus terrain mutations (whole-chunk
      -- replacements).
    }

-- | Extract the 'SimNodeId' from either constructor.
simNodeId :: SimNode -> SimNodeId
simNodeId (SimNodeReader{snrId = nid}) = nid
simNodeId (SimNodeWriter{snwId = nid}) = nid

-- | Extract the overlay name from either constructor.
simNodeOverlayName :: SimNode -> Text
simNodeOverlayName (SimNodeReader{snrOverlayName = n}) = n
simNodeOverlayName (SimNodeWriter{snwOverlayName = n}) = n

-- | Extract the dependency list from either constructor.
simNodeDependencies :: SimNode -> [SimNodeId]
simNodeDependencies (SimNodeReader{snrDependencies = ds}) = ds
simNodeDependencies (SimNodeWriter{snwDependencies = ds}) = ds

-- ---------------------------------------------------------------------------
-- Terrain writes
-- ---------------------------------------------------------------------------

-- | Accumulated terrain mutations from simulation writer nodes.
--
-- Each entry is a whole-chunk replacement keyed by chunk ID.
-- The caller applies these via 'applyTerrainWrites'.
data TerrainWrites = TerrainWrites
  { twrTerrain    :: !(IntMap TerrainChunk)
    -- ^ Replacement terrain chunks.
  , twrClimate    :: !(IntMap ClimateChunk)
    -- ^ Replacement climate chunks.
  , twrVegetation :: !(IntMap VegetationChunk)
    -- ^ Replacement vegetation chunks.
  } deriving (Show)

-- | No terrain mutations.
emptyTerrainWrites :: TerrainWrites
emptyTerrainWrites = TerrainWrites
  { twrTerrain    = IntMap.empty
  , twrClimate    = IntMap.empty
  , twrVegetation = IntMap.empty
  }

-- | Merge two 'TerrainWrites'.  Later writes (right argument) take
-- precedence for the same chunk ID.
mergeTerrainWrites :: TerrainWrites -> TerrainWrites -> TerrainWrites
mergeTerrainWrites a b = TerrainWrites
  { twrTerrain    = IntMap.union (twrTerrain b) (twrTerrain a)
  , twrClimate    = IntMap.union (twrClimate b) (twrClimate a)
  , twrVegetation = IntMap.union (twrVegetation b) (twrVegetation a)
  }

-- | Merge terrain writes into the world.  Chunks present in the
-- writes replace the corresponding chunks in the world.
applyTerrainWrites :: TerrainWrites -> TerrainWorld -> TerrainWorld
applyTerrainWrites tw world = world
  { twTerrain    = IntMap.union (twrTerrain tw) (twTerrain world)
  , twClimate    = IntMap.union (twrClimate tw) (twClimate world)
  , twVegetation = IntMap.union (twrVegetation tw) (twVegetation world)
  }

-- ---------------------------------------------------------------------------
-- Progress reporting
-- ---------------------------------------------------------------------------

-- | Progress update emitted during simulation execution.
data SimProgress = SimProgress
  { simpNodeIndex :: !Int
    -- ^ 0-based index of the current node in execution order.
  , simpNodeCount :: !Int
    -- ^ Total number of nodes in the DAG.
  , simpNodeId    :: !SimNodeId
    -- ^ Which node this progress refers to.
  , simpStatus    :: !SimStatus
    -- ^ Current status of that node.
  } deriving (Eq, Show)

-- | Status of a single simulation node.
data SimStatus
  = SimStarted
  | SimCompleted
  | SimFailed !Text
  deriving (Eq, Show)
