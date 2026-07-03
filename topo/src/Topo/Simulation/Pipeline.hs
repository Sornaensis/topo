{-# LANGUAGE OverloadedStrings #-}

-- | Hourly simulation tick pipeline.
--
-- This module wraps the lower-level DAG executor with scheduling, world-time
-- advancement, terrain-write application, and schedule-cursor updates for one
-- canonical world hour.
module Topo.Simulation.Pipeline
  ( SimulationTickPipelineResult(..)
  , SimulationTickNodeStatus(..)
  , SimulationScheduleUpdate(..)
  , runSimulationTickPipeline
  ) where

import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)

import Topo.Calendar (WorldTime(..), advanceTicks, mkCalendarConfig, tickToDate)
import Topo.Overlay
  ( Overlay(..)
  , OverlayProvenance(..)
  , OverlayStore
  , insertOverlay
  , lookupOverlay
  )
import Topo.Simulation
  ( SimNode
  , SimNodeId(..)
  , SimProgress(..)
  , SimStatus(..)
  , TerrainWrites
  , applyTerrainWrites
  , initialScheduleAt
  , markScheduleFired
  , normalizeScheduleState
  , scheduleDue
  , simNodeId
  , simNodeOverlayName
  , simNodeSchedule
  )
import Topo.Simulation.DAG (SimDAG(..), tickSimulation)
import Topo.Simulation.Schedule (SimulationScheduleState(..))
import Topo.World (TerrainWorld(..))

-- | Result of one successful hourly simulation tick pipeline run.
data SimulationTickPipelineResult = SimulationTickPipelineResult
  { stprWorld :: !TerrainWorld
    -- ^ Updated world with terrain writes applied, overlays replaced, and time
    -- advanced to 'stprTargetWorldTime'.
  , stprOverlayStore :: !OverlayStore
    -- ^ Final overlay store including successful node outputs and schedule
    -- cursor updates.
  , stprTerrainWrites :: !TerrainWrites
    -- ^ Terrain writes returned by due writer nodes, also applied to
    -- 'stprWorld'.
  , stprNodeStatuses :: ![SimulationTickNodeStatus]
    -- ^ Final per-node statuses for every node in DAG order, including
    -- skipped non-due nodes.
  , stprProgressLog :: ![SimProgress]
    -- ^ Progress events emitted by this wrapper and the lower-level executor.
  , stprAppliedTick :: !Word64
    -- ^ The target tick that was applied by this hourly pipeline run.
  , stprTargetWorldTime :: !WorldTime
    -- ^ World time represented by node contexts and by the returned world.
  , stprScheduleUpdates :: ![SimulationScheduleUpdate]
    -- ^ Schedule cursor updates applied for due nodes that completed.
  }

-- | Final status for a single DAG node in a successful pipeline run.
data SimulationTickNodeStatus = SimulationTickNodeStatus
  { stnsNodeIndex :: !Int
  , stnsNodeId :: !SimNodeId
  , stnsOverlayName :: !Text
  , stnsStatus :: !SimStatus
  , stnsScheduleBefore :: !SimulationScheduleState
  , stnsScheduleAfter :: !(Maybe SimulationScheduleState)
  } deriving (Eq, Show)

-- | Schedule cursor change applied to one successfully completed due node.
data SimulationScheduleUpdate = SimulationScheduleUpdate
  { ssuNodeId :: !SimNodeId
  , ssuOverlayName :: !Text
  , ssuScheduleBefore :: !SimulationScheduleState
  , ssuScheduleAfter :: !SimulationScheduleState
  } deriving (Eq, Show)

-- | Execute exactly one hourly simulation tick.
--
-- The optional target tick, when supplied, must be the next hourly tick
-- (@wtTick current + 1@).  This keeps the wrapper atomic and prevents a single
-- call from silently representing multiple world hours.
runSimulationTickPipeline
  :: TerrainWorld
  -> SimDAG
  -> (SimProgress -> IO ())
  -> Maybe Word64
  -> IO (Either Text SimulationTickPipelineResult)
runSimulationTickPipeline world dag progress explicitTargetTick = do
  let currentWorldTime = twWorldTime world
      targetWorldTime = advanceTicks 1 currentWorldTime
      targetTick = wtTick targetWorldTime
  case explicitTargetTick of
    Just requested | requested /= targetTick ->
      pure $ Left $
        "Simulation tick pipeline advances exactly one hour; requested target tick "
          <> showText requested
          <> " does not match next tick "
          <> showText targetTick
    _ -> runValidatedTick world dag progress targetWorldTime targetTick

runValidatedTick
  :: TerrainWorld
  -> SimDAG
  -> (SimProgress -> IO ())
  -> WorldTime
  -> Word64
  -> IO (Either Text SimulationTickPipelineResult)
runValidatedTick world dag progress targetWorldTime targetTick = do
  progressLogRef <- newIORef []
  let nodes = sdNodes dag
      totalNodes = length nodes
      currentTick = wtTick (twWorldTime world)
      store0 = twOverlays world
      plans = zipWith (makeNodePlan currentTick targetTick store0) [0..] nodes
      dueIds = Set.fromList [ simNodeId (npNode plan) | plan <- plans, npDue plan ]
      dueDAG = filterDueDAG dueIds dag
      indexByNode = Map.fromList
        [ (simNodeId node, ix) | (ix, node) <- zip [0..] nodes ]
      emit prog = do
        atomicModifyIORef' progressLogRef $ \events -> (events <> [prog], ())
        progress prog
      emitExecutorProgress prog = emit prog
        { simpNodeIndex = Map.findWithDefault (simpNodeIndex prog) (simpNodeId prog) indexByNode
        , simpNodeCount = totalNodes
        }

  mapM_ (emitSkippedProgress totalNodes emit) (filter (not . npDue) plans)

  let calendar = tickToDate (mkCalendarConfig (twPlanet world)) targetWorldTime
      terrainForTick = world { twWorldTime = targetWorldTime }
  tickResult <- tickSimulation dueDAG emitExecutorProgress terrainForTick store0 calendar targetWorldTime 1
  case tickResult of
    Left err -> pure (Left err)
    Right (storeAfterTick, terrainWrites) ->
      case applyScheduleUpdates targetTick (filter npDue plans) storeAfterTick of
        Left err -> pure (Left err)
        Right (finalStore, scheduleUpdates) -> do
          progressLog <- readIORef progressLogRef
          let scheduleUpdateMap = Map.fromList
                [ (ssuNodeId update, ssuScheduleAfter update)
                | update <- scheduleUpdates
                ]
              finalStatuses = map (finalNodeStatus scheduleUpdateMap) plans
              worldWithWrites = applyTerrainWrites terrainWrites world
              finalWorld = worldWithWrites
                { twOverlays = finalStore
                , twWorldTime = targetWorldTime
                }
          pure $ Right SimulationTickPipelineResult
            { stprWorld = finalWorld
            , stprOverlayStore = finalStore
            , stprTerrainWrites = terrainWrites
            , stprNodeStatuses = finalStatuses
            , stprProgressLog = progressLog
            , stprAppliedTick = targetTick
            , stprTargetWorldTime = targetWorldTime
            , stprScheduleUpdates = scheduleUpdates
            }

-- | Per-node scheduling decision for this target tick.
data NodePlan = NodePlan
  { npNodeIndex :: !Int
  , npNode :: !SimNode
  , npSchedule :: !SimulationScheduleState
  , npDue :: !Bool
  , npSkipReason :: !Text
  }

makeNodePlan :: Word64 -> Word64 -> OverlayStore -> Int -> SimNode -> NodePlan
makeNodePlan currentTick targetTick store ix node =
  let schedule = nodeScheduleState currentTick store node
      due = scheduleDue targetTick schedule
      reason = "not due until tick " <> showText (schedNextFire schedule)
  in NodePlan
    { npNodeIndex = ix
    , npNode = node
    , npSchedule = schedule
    , npDue = due
    , npSkipReason = reason
    }

nodeScheduleState :: Word64 -> OverlayStore -> SimNode -> SimulationScheduleState
nodeScheduleState currentTick store node =
  case lookupOverlay (simNodeOverlayName node) store >>= opSchedule . ovProvenance of
    Just persisted -> normalizeScheduleState persisted
    Nothing -> initialScheduleAt currentTick (simNodeSchedule node)

schedNextFire :: SimulationScheduleState -> Word64
schedNextFire = schedNextFireTick

filterDueDAG :: Set SimNodeId -> SimDAG -> SimDAG
filterDueDAG dueIds dag = dag
  { sdLevels = map (filter (`Set.member` dueIds)) (sdLevels dag)
  , sdTerrainWriters = filter (`Set.member` dueIds) (sdTerrainWriters dag)
  }

emitSkippedProgress :: Int -> (SimProgress -> IO ()) -> NodePlan -> IO ()
emitSkippedProgress totalNodes emit plan =
  let node = npNode plan
  in emit SimProgress
    { simpNodeIndex = npNodeIndex plan
    , simpNodeCount = totalNodes
    , simpNodeId = simNodeId node
    , simpStatus = SimSkipped (npSkipReason plan)
    }

applyScheduleUpdates
  :: Word64
  -> [NodePlan]
  -> OverlayStore
  -> Either Text (OverlayStore, [SimulationScheduleUpdate])
applyScheduleUpdates targetTick plans store0 = go store0 [] plans
  where
    go store updates [] = Right (store, reverse updates)
    go store updates (plan:rest) =
      let node = npNode plan
          nodeId = simNodeId node
          overlayName = simNodeOverlayName node
      in case lookupOverlay overlayName store of
        Nothing -> Left $
          "Simulation node " <> unSimNodeId nodeId
            <> " completed but final overlay is missing: " <> overlayName
        Just overlay ->
          let scheduleBefore = npSchedule plan
              scheduleAfter = markScheduleFired targetTick scheduleBefore
              overlay' = overlay
                { ovProvenance = (ovProvenance overlay)
                    { opSchedule = Just scheduleAfter }
                }
              update = SimulationScheduleUpdate
                { ssuNodeId = nodeId
                , ssuOverlayName = overlayName
                , ssuScheduleBefore = scheduleBefore
                , ssuScheduleAfter = scheduleAfter
                }
          in go (insertOverlay overlay' store) (update : updates) rest

finalNodeStatus :: Map SimNodeId SimulationScheduleState -> NodePlan -> SimulationTickNodeStatus
finalNodeStatus scheduleUpdateMap plan =
  let node = npNode plan
      nodeId = simNodeId node
      status
        | npDue plan = SimCompleted
        | otherwise = SimSkipped (npSkipReason plan)
      scheduleAfter
        | npDue plan = Map.lookup nodeId scheduleUpdateMap
        | otherwise = Just (npSchedule plan)
  in SimulationTickNodeStatus
    { stnsNodeIndex = npNodeIndex plan
    , stnsNodeId = nodeId
    , stnsOverlayName = simNodeOverlayName node
    , stnsStatus = status
    , stnsScheduleBefore = npSchedule plan
    , stnsScheduleAfter = scheduleAfter
    }

showText :: Show a => a -> Text
showText = T.pack . show
