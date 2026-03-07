{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Simulation DAG construction and wavefront executor.
--
-- 'buildSimDAG' topologically sorts registered 'SimNode's, validates
-- acyclicity, and partitions into wavefront levels.  'tickSimulation'
-- executes one simulation tick:
--
-- 1. __Read-only phase__: 'SimNodeReader' nodes run concurrently per
--    wavefront level (all nodes at the same depth in parallel, barrier
--    between levels).  All readers see the terrain snapshot from before
--    this tick.
--
-- 2. __Terrain-write phase__: 'SimNodeWriter' nodes run sequentially
--    after all readers complete.  Each writer receives the terrain with
--    accumulated writes from previous writers applied.
module Topo.Simulation.DAG
  ( -- * DAG type
    SimDAG(..)
    -- * Construction
  , buildSimDAG
    -- * Execution
  , tickSimulation
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException, try)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)

import Topo.Calendar (CalendarDate)
import Topo.Overlay (Overlay(..), OverlayStore(..), emptyOverlayStore, lookupOverlay, insertOverlay)
import Topo.Simulation
  ( SimNode(..), SimNodeId(..)
  , SimContext(..), SimProgress(..), SimStatus(..)
  , TerrainWrites(..), emptyTerrainWrites, mergeTerrainWrites, applyTerrainWrites
  , simNodeId, simNodeOverlayName, simNodeDependencies
  )
import Topo.Calendar (WorldTime)
import Topo.World (TerrainWorld(..))

-- ---------------------------------------------------------------------------
-- DAG type
-- ---------------------------------------------------------------------------

-- | A validated, topologically-sorted simulation DAG.
--
-- Constructed via 'buildSimDAG'.  The 'sdLevels' field contains
-- wavefront levels: nodes at the same level have no
-- interdependencies and may be executed concurrently.
data SimDAG = SimDAG
  { sdNodes          :: ![SimNode]
    -- ^ All registered nodes, in topological order.
  , sdLevels         :: ![[SimNodeId]]
    -- ^ Wavefront concurrency levels (readers only).
  , sdTerrainWriters :: ![SimNodeId]
    -- ^ Writer node IDs in execution order (after all readers).
  }

-- ---------------------------------------------------------------------------
-- Construction
-- ---------------------------------------------------------------------------

-- | Build a simulation DAG from a list of nodes.
--
-- Returns 'Left' if:
--
-- * A dependency references an unknown node ID.
-- * The dependency graph contains a cycle.
-- * Two nodes claim the same 'SimNodeId'.
--
-- On success, returns a 'SimDAG' with computed wavefront levels.
buildSimDAG :: [SimNode] -> Either Text SimDAG
buildSimDAG nodes = do
  -- Check for duplicate IDs
  let ids     = map simNodeId nodes
      idSet   = Set.fromList ids
  if Set.size idSet /= length ids
    then Left "Duplicate SimNodeId detected"
    else pure ()

  -- Build name → node map
  let nodeMap = Map.fromList [(simNodeId n, n) | n <- nodes]

  -- Validate all dependencies exist
  let missingDeps = [ (simNodeId n, depId)
                    | n <- nodes
                    , depId <- simNodeDependencies n
                    , not (Map.member depId nodeMap)
                    ]
  case missingDeps of
    ((nid, depId):_) ->
      Left $ "Node " <> unSimNodeId nid
          <> " depends on unknown node " <> unSimNodeId depId
    [] -> pure ()

  -- Topological sort with cycle detection (Kahn's algorithm)
  let -- Build adjacency: for each node, which nodes depend on it
      -- (i.e. it is a prerequisite of those nodes)
      dependents :: Map SimNodeId [SimNodeId]
      dependents = Map.fromListWith (++)
        [ (depId, [simNodeId n])
        | n <- nodes
        , depId <- simNodeDependencies n
        ]

      inDegree0 :: Map SimNodeId Int
      inDegree0 = Map.fromList
        [ (simNodeId n, length (simNodeDependencies n))
        | n <- nodes
        ]

  case kahnLevels inDegree0 dependents of
    Left _cycle -> Left "Cycle detected in simulation DAG"
    Right levels -> do
      -- Partition into reader levels and writer list
      let isWriter nid = case Map.lookup nid nodeMap of
            Just SimNodeWriter{} -> True
            _                    -> False
          readerLevels = map (filter (not . isWriter)) levels
          -- Filter out empty levels
          nonEmptyReaderLevels = filter (not . null) readerLevels
          -- Collect writers in topological order
          writerIds = filter isWriter (concatMap id levels)
          -- All nodes in topological order
          topoOrder = concatMap id levels
          orderedNodes = [ n | nid <- topoOrder
                             , Just n <- [Map.lookup nid nodeMap]
                             ]
      Right SimDAG
        { sdNodes          = orderedNodes
        , sdLevels         = nonEmptyReaderLevels
        , sdTerrainWriters = writerIds
        }

-- | Kahn's algorithm producing wavefront levels.
--
-- Each level contains nodes whose in-degree became zero at the same
-- iteration.  Returns 'Left' if a cycle is detected (remaining
-- nodes with non-zero in-degree).
kahnLevels
  :: Map SimNodeId Int              -- ^ Initial in-degrees
  -> Map SimNodeId [SimNodeId]      -- ^ Adjacency: node → its dependents
  -> Either Text [[SimNodeId]]      -- ^ Wavefront levels or cycle error
kahnLevels inDeg0 adj = go inDeg0 []
  where
    go inDeg acc
      | Map.null inDeg = Right (reverse acc)
      | null ready     = Left "Cycle detected"
      | otherwise      = go inDeg'' (ready : acc)
      where
        ready   = Map.keys (Map.filter (<= 0) inDeg)
        inDeg'  = foldr Map.delete inDeg ready
        -- Decrement in-degree for dependents of ready nodes
        inDeg'' = foldr decrementDeps inDeg' ready
        decrementDeps nid m =
          let deps = maybe [] id (Map.lookup nid adj)
          in foldr (\d -> Map.adjust (subtract 1) d) m deps

-- ---------------------------------------------------------------------------
-- Execution
-- ---------------------------------------------------------------------------

-- | Execute one tick of the simulation DAG.
--
-- __Read-only phase__: Reader nodes run concurrently per wavefront
-- level.  All readers see the same terrain snapshot.
--
-- __Terrain-write phase__: Writer nodes run sequentially.  Each
-- writer sees terrain with accumulated writes from prior writers.
--
-- The caller is responsible for advancing 'WorldTime' after
-- applying the returned writes.
tickSimulation
  :: SimDAG
  -> (SimProgress -> IO ())       -- ^ Progress callback
  -> TerrainWorld                 -- ^ Terrain snapshot
  -> OverlayStore                 -- ^ Current overlay state
  -> CalendarDate                 -- ^ Current calendar position
  -> WorldTime                    -- ^ Current world time
  -> Word64                       -- ^ Delta ticks since last sim
  -> IO (Either Text (OverlayStore, TerrainWrites))
tickSimulation dag progress terrain store0 calDate wtime dticks = do
  let nodeMap    = Map.fromList [(simNodeId n, n) | n <- sdNodes dag]
      totalNodes = length (sdNodes dag)

  -- Phase 1: Reader wavefront
  readerResult <- runReaderPhase
                    nodeMap totalNodes progress
                    terrain store0 calDate wtime dticks
                    (sdLevels dag)

  case readerResult of
    Left err -> pure (Left err)
    Right store1 -> do
      -- Phase 2: Writer phase (sequential)
      writerResult <- runWriterPhase
                        nodeMap totalNodes progress
                        terrain store1 calDate wtime dticks
                        (sdTerrainWriters dag)
      pure writerResult

-- | Run all reader nodes level by level, concurrently within each level.
runReaderPhase
  :: Map SimNodeId SimNode
  -> Int                          -- ^ Total node count for progress
  -> (SimProgress -> IO ())
  -> TerrainWorld
  -> OverlayStore
  -> CalendarDate
  -> WorldTime
  -> Word64
  -> [[SimNodeId]]                -- ^ Wavefront reader levels
  -> IO (Either Text OverlayStore)
runReaderPhase _nodeMap _total _progress _terrain store _cal _wt _dt [] =
  pure (Right store)
runReaderPhase nodeMap total progress terrain store cal wt dt (level:rest) = do
  -- Build tasks for this level
  let tasks = [ (nid, node)
              | nid <- level
              , Just node <- [Map.lookup nid nodeMap]
              ]

  -- Run all nodes at this level concurrently
  results <- mapConcurrently (runOneReader nodeMap terrain store cal wt dt progress total) tasks

  -- Collect results; first error aborts
  case collectResults results store of
    Left err     -> pure (Left err)
    Right store' -> runReaderPhase nodeMap total progress terrain store' cal wt dt rest

-- | Run a single reader node.
runOneReader
  :: Map SimNodeId SimNode
  -> TerrainWorld
  -> OverlayStore
  -> CalendarDate
  -> WorldTime
  -> Word64
  -> (SimProgress -> IO ())
  -> Int
  -> (SimNodeId, SimNode)
  -> IO (Either Text (Maybe (SimNodeId, Text, Overlay)))
runOneReader nodeMap terrain store cal wt dt progress total (nid, node) =
  case node of
    SimNodeReader{snrOverlayName = name, snrDependencies = deps, snrReadTick = tick} -> do
      let depOverlays = gatherDependencyOverlays store deps nodeMap
          ctx = SimContext
            { scTerrain    = terrain { twOverlays = emptyOverlayStore }
            , scCalendar   = cal
            , scWorldTime  = wt
            , scDeltaTicks = dt
            , scOverlays   = depOverlays
            }
      case lookupOverlay name store of
        Nothing -> pure (Left (
          "Simulation node " <> unSimNodeId nid
            <> " references missing overlay: " <> name))
        Just overlay -> do

          progress SimProgress
            { simpNodeIndex = 0  -- approximate; refined if needed
            , simpNodeCount = total
            , simpNodeId    = nid
            , simpStatus    = SimStarted
            }

          result <- try (tick ctx overlay)
          case result of
            Left (ex :: SomeException) -> do
              let msg = "Exception in node " <> unSimNodeId nid <> ": " <> T.pack (show ex)
              progress SimProgress
                { simpNodeIndex = 0
                , simpNodeCount = total
                , simpNodeId    = nid
                , simpStatus    = SimFailed msg
                }
              pure (Left msg)
            Right (Left err) -> do
              progress SimProgress
                { simpNodeIndex = 0
                , simpNodeCount = total
                , simpNodeId    = nid
                , simpStatus    = SimFailed err
                }
              pure (Left err)
            Right (Right updatedOverlay) -> do
              progress SimProgress
                { simpNodeIndex = 0
                , simpNodeCount = total
                , simpNodeId    = nid
                , simpStatus    = SimCompleted
                }
              pure (Right (Just (nid, name, updatedOverlay)))

    -- Writer nodes are skipped in reader phase
    SimNodeWriter{} -> pure (Right Nothing)

-- | Collect reader results into the overlay store.
collectResults
  :: [Either Text (Maybe (SimNodeId, Text, Overlay))]
  -> OverlayStore
  -> Either Text OverlayStore
collectResults [] store = Right store
collectResults (Left err : _) _store = Left err
collectResults (Right Nothing : rest) store =
  collectResults rest store
collectResults (Right (Just (_nid, _name, ov)) : rest) store =
  collectResults rest (insertOverlay ov store)

-- | Run writer nodes sequentially, accumulating terrain writes.
runWriterPhase
  :: Map SimNodeId SimNode
  -> Int
  -> (SimProgress -> IO ())
  -> TerrainWorld
  -> OverlayStore
  -> CalendarDate
  -> WorldTime
  -> Word64
  -> [SimNodeId]
  -> IO (Either Text (OverlayStore, TerrainWrites))
runWriterPhase _nodeMap _total _progress _terrain store _cal _wt _dt [] =
  pure (Right (store, emptyTerrainWrites))
runWriterPhase nodeMap total progress terrain store cal wt dt (nid:rest) = do
  case Map.lookup nid nodeMap of
    Nothing -> pure (Left $ "Unknown writer node: " <> unSimNodeId nid)
    Just node -> case node of
      SimNodeWriter{snwOverlayName = name, snwDependencies = deps, snwWriteTick = tick} -> do
        let depOverlays = gatherDependencyOverlays store deps nodeMap
            ctx = SimContext
              { scTerrain    = terrain { twOverlays = emptyOverlayStore }
              , scCalendar   = cal
              , scWorldTime  = wt
              , scDeltaTicks = dt
              , scOverlays   = depOverlays
              }

        case lookupOverlay name store of
          Nothing ->
            pure (Left (
              "Simulation writer node " <> unSimNodeId nid
                <> " references missing overlay: " <> name))
          Just overlay -> do
            progress SimProgress
              { simpNodeIndex = 0
              , simpNodeCount = total
              , simpNodeId    = nid
              , simpStatus    = SimStarted
              }

            result <- try (tick ctx overlay)
            case result of
              Left (ex :: SomeException) -> do
                let msg = "Exception in writer node " <> unSimNodeId nid <> ": " <> T.pack (show ex)
                progress SimProgress
                  { simpNodeIndex = 0
                  , simpNodeCount = total
                  , simpNodeId    = nid
                  , simpStatus    = SimFailed msg
                  }
                pure (Left msg)
              Right (Left err) -> do
                progress SimProgress
                  { simpNodeIndex = 0
                  , simpNodeCount = total
                  , simpNodeId    = nid
                  , simpStatus    = SimFailed err
                  }
                pure (Left err)
              Right (Right (updatedOverlay, writes)) -> do
                progress SimProgress
                  { simpNodeIndex = 0
                  , simpNodeCount = total
                  , simpNodeId    = nid
                  , simpStatus    = SimCompleted
                  }
                let store'   = insertOverlay updatedOverlay store
                    terrain' = applyTerrainWrites writes terrain
                restResult <- runWriterPhase nodeMap total progress terrain' store' cal wt dt rest
                case restResult of
                  Left err -> pure (Left err)
                  Right (finalStore, moreWrites) ->
                    pure (Right (finalStore, mergeTerrainWrites writes moreWrites))

      -- Reader nodes shouldn't appear in writer phase
      SimNodeReader{} -> runWriterPhase nodeMap total progress terrain store cal wt dt rest

-- | Gather overlay data for declared dependencies.
gatherDependencyOverlays
  :: OverlayStore
  -> [SimNodeId]
  -> Map SimNodeId SimNode
  -> Map Text Overlay
gatherDependencyOverlays store deps nodeMap =
  Map.fromList
    [ (depName, ov)
    | depId <- deps
    , Just depNode <- [Map.lookup depId nodeMap]
    , let depName = simNodeOverlayName depNode
    , Just ov <- [lookupOverlay depName store]
    ]
