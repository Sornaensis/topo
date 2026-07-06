{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Simulation actor: holds the generated 'TerrainWorld' and drives
-- the hourly simulation tick pipeline in response to tick requests from the UI.
--
-- After terrain generation, the Terrain actor sends the full world here via
-- 'setSimWorld'.  Each background worker runs one or more one-hour pipeline
-- steps, then publishes the final world/overlay state to the Data actor.
module Actor.Simulation
  ( Simulation
  , simulationActorDef
    -- * World lifecycle
  , setSimWorld
  , setSimWorldWithNodes
  , rebindSimNodes
  , normalizeWorldSchedulesForBindings
  , clearSimWorld
  , beginSimWorldTransition
  , cancelSimWorldTransition
    -- * Tick control
  , requestSimTick
  , autoTickStep
  , autoTickStepArmed
  , flushSimWeatherPublication
  , autoTickWeatherPublishIntervalNs
  , simulationAtlasBackfillRateThreshold
  , beginSimShutdown
  , waitForSimIdle
  , AutoTickStepResult(..)
  , AutoTickSkipReason(..)
    -- * DAG status
  , SimulationDagSnapshot(..)
  , SimulationDagNodeSnapshot(..)
  , SimulationNodeBinding(..)
  , SimulationTickLogEntry(..)
  , getSimDagSnapshot
    -- * Handles setup
  , setSimHandles
  , simulationHandlesConfigured
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, readMVar, tryPutMVar)
import Control.Exception (SomeException, displayException, try)
import Control.Monad (when)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Hyperspace.Actor
import Hyperspace.Actor.QQ (hyperspace)
import Hyperspace.Actor.Spec (OpTag(..))
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map

import Actor.AtlasCache (atlasKeyFor, terrainSnapshotViewVersion)
import Actor.AtlasManager
  ( AtlasManager
  , AtlasJob(..)
  , enqueueAtlasBuild
  )
import Actor.Data
  ( Data
  , TerrainSnapshot(..)
  , getTerrainSnapshot
  , setOverlayStoreData
  , setWeatherChunkData
  , updateClimateChunkData
  , updateTerrainChunkData
  , updateVegetationChunkData
  )
import Actor.Log
  ( Log
  , LogEntry(..)
  , LogLevel(..)
  , appendLog
  )
import Actor.SnapshotReceiver
  ( DataSnapshotRef
  , TerrainSnapshotRef
  , SnapshotVersion
  , SnapshotVersionRef
  , readSnapshotVersion
  , readTerrainSnapshot
  , writeTerrainSnapshot
  , bumpSnapshotVersion
  )
import Actor.UI
  ( Ui
  , UiState(..)
  , ViewMode(..)
  , getUiSnapshot
  , setUiSimTickCount
  , setUiOverlayNames
  )
import Seer.Render.ZoomStage (ZoomStage(..), orderedZoomStagesForZoom, stageForZoom)

import Topo.Calendar
  ( CalendarConfig
  , WorldTime(..)
  , mkCalendarConfig
  )
import Topo.Weather (WeatherConfig, defaultWeatherConfig, weatherSimNode)
import Topo.Simulation
  ( SimNode(..)
  , SimProgress(..)
  , SimStatus(..)
  , SimNodeId(..)
  , SimulationScheduleState(..)
  , TerrainWrites(..)
  , emptyTerrainWrites
  , mergeTerrainWrites
  , catchUpPolicyText
  , ensureOverlaySchedule
  , reconcileOverlaySchedule
  , normalizeScheduleState
  , scheduleDue
  , simNodeDependencies
  , simNodeId
  , simNodeOverlayName
  , simNodeSchedule
  )
import Topo.Simulation.DAG
  ( SimDAG(..)
  , buildSimDAG
  )
import Topo.Simulation.Pipeline
  ( SimulationTickNodeStatus(..)
  , SimulationTickPipelineResult(..)
  , runSimulationTickPipeline
  )
import Topo (ChunkId(..), getWeatherFromOverlay)
import Topo.World (TerrainWorld(..))
import Topo.Overlay (Overlay(..), OverlayProvenance(..), OverlayStore, insertOverlay, lookupOverlay, overlayNames)
import Topo.WorldGen (WorldGenConfig(..))
import Topo.Types (WorldConfig(..))
import Data.Aeson (fromJSON, Result(..), Value)

-- ---------------------------------------------------------------------------
-- State
-- ---------------------------------------------------------------------------

-- | Handles the simulation actor needs for pushing results.
data SimHandles = SimHandles
  { shDataHandle     :: !(ActorHandle Data (Protocol Data))
  , shLogHandle      :: !(ActorHandle Log (Protocol Log))
  , shUiHandle       :: !(ActorHandle Ui (Protocol Ui))
  , shDataSnapshotRef :: !DataSnapshotRef
  , shTerrainSnapshotRef :: !TerrainSnapshotRef
  , shSnapshotVersionRef :: !SnapshotVersionRef
  , shAtlasHandle    :: !(ActorHandle AtlasManager (Protocol AtlasManager))
  }

-- | A simulation node plus provenance metadata for diagnostics.
-- Built-in callers can keep using 'setSimWorld'; plugin integration uses this
-- binding so the actor-owned DAG can report executable plugin nodes truthfully.
data SimulationNodeBinding = SimulationNodeBinding
  { snbNode :: !SimNode
  , snbKind :: !Text
  , snbPlugin :: !(Maybe Text)
  }

data SimulationDagNodeSnapshot = SimulationDagNodeSnapshot
  { sdnsNodeId :: !Text
  , sdnsKind :: !Text
  , sdnsPlugin :: !(Maybe Text)
  , sdnsOverlay :: !Text
  , sdnsDependencies :: ![Text]
  , sdnsWritesTerrain :: !Bool
  , sdnsStatus :: !Text
  , sdnsStatusDetail :: !(Maybe Text)
  , sdnsScheduleIntervalTicks :: !(Maybe Word64)
  , sdnsSchedulePhaseTicks :: !(Maybe Word64)
  , sdnsScheduleCatchUp :: !(Maybe Text)
  , sdnsScheduleLastFireTick :: !(Maybe Word64)
  , sdnsScheduleNextFireTick :: !(Maybe Word64)
  , sdnsScheduleDue :: !(Maybe Bool)
  } deriving (Eq, Show)

data SimulationTickLogEntry = SimulationTickLogEntry
  { stleTick :: !Word64
  , stleNodeId :: !(Maybe Text)
  , stleStatus :: !Text
  , stleMessage :: !Text
  , stleElapsedMs :: !(Maybe Double)
  } deriving (Eq, Show)

data AutoTickSkipReason
  = AutoTickNoWorld
  | AutoTickUnready
  | AutoTickEpochChanged
  deriving (Eq, Show)

data AutoTickStepResult
  = AutoTickApplied !Word64
  | AutoTickSkipped !AutoTickSkipReason
  | AutoTickFailed !Text
  deriving (Eq, Show)

tickResultTag :: OpTag "tickResult"
tickResultTag = OpTag

data SimulationTickCompletion
  = SimulationTickNoCompletion
  | SimulationTickAutoCompletion !Bool !(MVar AutoTickStepResult)

data SimulationTickKind
  = SimulationManualTick !Word64
  | SimulationAutoTick !(Maybe Word64) !Bool !(MVar AutoTickStepResult)

data SimInFlight = SimInFlight
  { sifToken :: !Word64
  , sifRequestedTick :: !Word64
  , sifAppliedTick :: !Word64
  , sifWorldEpoch :: !Word64
  , sifDone :: !(MVar ())
  }

data SimulationTickWork = SimulationTickWork
  { stwToken :: !Word64
  , stwRequestedTick :: !Word64
  , stwAppliedTick :: !Word64
  , stwDeltaTicks :: !Word64
  , stwExpectedEpoch :: !(Maybe Word64)
  , stwWorldEpoch :: !Word64
  , stwWorld :: !TerrainWorld
  , stwDAG :: !SimDAG
  , stwHandles :: !SimHandles
  , stwNodeStatuses :: !(Map.Map Text (Text, Maybe Text))
  , stwCompletion :: !SimulationTickCompletion
  }

data SimulationTickSuccess = SimulationTickSuccess
  { stsWorld :: !TerrainWorld
  , stsTerrainWrites :: !TerrainWrites
  }

data SimulationTickResult = SimulationTickResult
  { strToken :: !Word64
  , strRequestedTick :: !Word64
  , strAppliedTick :: !Word64
  , strDeltaTicks :: !Word64
  , strExpectedEpoch :: !(Maybe Word64)
  , strWorldEpoch :: !Word64
  , strBaseWorld :: !TerrainWorld
  , strHandles :: !SimHandles
  , strElapsedMs :: !Double
  , strNodeStatuses :: !(Map.Map Text (Text, Maybe Text))
  , strProgressLogs :: ![SimulationTickLogEntry]
  , strResult :: !(Either Text SimulationTickSuccess)
  , strCompletion :: !SimulationTickCompletion
  }

type TickResultSink = SimulationTickResult -> IO ()

data SimulationTickControl = SimulationTickControl
  { stcKind :: !SimulationTickKind
  , stcResultSink :: !TickResultSink
  }

data PendingTick = PendingTick
  { ptRequestedTick :: !Word64
  , ptResultSink :: !TickResultSink
  }

data SimulationDagSnapshot = SimulationDagSnapshot
  { sdsAvailable :: !Bool
  , sdsWorldBound :: !Bool
  , sdsOverlayNames :: ![Text]
  , sdsNodes :: ![SimulationDagNodeSnapshot]
  , sdsLevels :: ![[Text]]
  , sdsTerrainWriters :: ![Text]
  , sdsLastTick :: !Word64
  , sdsPendingTick :: !(Maybe Word64)
  , sdsWorldEpoch :: !Word64
  , sdsTickLogs :: ![SimulationTickLogEntry]
  } deriving (Eq, Show)

-- | Internal simulation state.
data SimState = SimState
  { ssWorld      :: !(Maybe TerrainWorld)
    -- ^ The full terrain world; set after generation.
  , ssDAG        :: !(Maybe SimDAG)
    -- ^ Pre-built simulation DAG; rebuilt when the world changes.
  , ssCalCfg     :: !(Maybe CalendarConfig)
    -- ^ Calendar config derived from the world's planet.
  , ssLastTick   :: !Word64
    -- ^ Last tick count processed (for delta computation).
  , ssHandles    :: !(Maybe SimHandles)
    -- ^ Actor handles for pushing results.
  , ssPendingTick :: !(Maybe PendingTick)
    -- ^ Latest requested manual tick queued while simulation is not ready or
    -- while a worker is already running.
  , ssInFlightTick :: !(Maybe SimInFlight)
    -- ^ At most one background tick worker may be running at a time.
  , ssNextTickToken :: !Word64
    -- ^ Monotonic token for dropping stale worker replies after world changes.
  , ssWorldEpoch :: !Word64
    -- ^ Monotonic world binding epoch used by the auto scheduler to avoid
    -- ticking a world that has been cleared or replaced.
  , ssWorldTransition :: !Bool
    -- ^ True while generation/load is replacing the world.  Manual requests
    -- defer and auto requests skip instead of ticking the previous world.
  , ssShuttingDown :: !Bool
    -- ^ Irreversible shutdown latch; once set, no new tick workers start.
  , ssNodeStatuses :: !(Map.Map Text (Text, Maybe Text))
    -- ^ Latest observable per-node status and detail for UI/API DAG diagnostics.
  , ssNodeMetadata :: !(Map.Map Text (Text, Maybe Text))
    -- ^ Node provenance metadata: kind plus optional plugin name.
  , ssTickLogs :: ![SimulationTickLogEntry]
    -- ^ Bounded tick and per-node status log exposed through the DAG surface.
  , ssLastAutoStatusPublishNs :: !Word64
    -- ^ Last snapshot-version bump for auto-tick status-only publication.
  , ssLastAutoWeatherPublishNs :: !Word64
    -- ^ Last render-facing weather/cloud/day-night publication during auto-tick.
  , ssAutoWeatherPublicationPending :: !Bool
    -- ^ True when a coalesced weather/cloud/day-night publication was skipped.
  }

emptySimState :: SimState
emptySimState = SimState
  { ssWorld    = Nothing
  , ssDAG      = Nothing
  , ssCalCfg   = Nothing
  , ssLastTick = 0
  , ssHandles  = Nothing
  , ssPendingTick = Nothing
  , ssInFlightTick = Nothing
  , ssNextTickToken = 1
  , ssWorldEpoch = 0
  , ssWorldTransition = False
  , ssShuttingDown = False
  , ssNodeStatuses = Map.empty
  , ssNodeMetadata = Map.empty
  , ssTickLogs = []
  , ssLastAutoStatusPublishNs = 0
  , ssLastAutoWeatherPublishNs = 0
  , ssAutoWeatherPublicationPending = False
  }

-- ---------------------------------------------------------------------------
-- Actor definition
-- ---------------------------------------------------------------------------

[hyperspace|
replyprotocol SimulationReplyOps =
  cast tickResult :: SimulationTickResult

actor Simulation
  state SimState
  lifetime Singleton
  schedule pinned 4 sticky
  noDeps

  reply SimulationReplyOps

  mailbox Unbounded

  cast setWorld   :: TerrainWorld
  cast setWorldWithNodes :: (TerrainWorld, [SimulationNodeBinding])
  call rebindNodes :: [SimulationNodeBinding] -> Bool
  call clearWorld :: () -> ()
  call beginTransition :: () -> ()
  call cancelTransition :: () -> ()
  call beginShutdown :: () -> Maybe (MVar ())
  cast tick       :: SimulationTickControl
  cast tickResult :: SimulationTickResult
  cast setHandles :: SimHandles
  call handlesConfigured :: () -> Bool
  call dagSnapshot :: () -> SimulationDagSnapshot
  call flushWeatherPublication :: () -> Bool

  initial emptySimState
  on_ setWorld = \world st ->
    bindWorld world [] st
  on_ setWorldWithNodes = \(world, pluginNodes) st ->
    bindWorld world pluginNodes st
  on rebindNodes = \pluginNodes st ->
    case ssWorld st of
      Just world | not (ssWorldTransition st) -> do
        st' <- bindWorld world pluginNodes st
        pure (st', True)
      _ -> pure (st, False)
  onPure clearWorld = \() st ->
    ( st
      { ssWorld  = Nothing
      , ssDAG    = Nothing
      , ssCalCfg = Nothing
      , ssLastTick = 0
      , ssPendingTick = Nothing
      , ssWorldEpoch = ssWorldEpoch st + 1
      , ssWorldTransition = True
      , ssNodeStatuses = Map.empty
      , ssNodeMetadata = Map.empty
      , ssTickLogs = []
      , ssLastAutoStatusPublishNs = 0
      , ssLastAutoWeatherPublishNs = 0
      , ssAutoWeatherPublicationPending = False
      }
    , ()
    )
  onPure beginTransition = \() st ->
    ( st
      { ssWorldTransition = True
      , ssWorldEpoch = ssWorldEpoch st + 1
      , ssPendingTick = Nothing
      , ssLastAutoStatusPublishNs = 0
      , ssLastAutoWeatherPublishNs = 0
      , ssAutoWeatherPublicationPending = False
      }
    , ()
    )
  on cancelTransition = \() st -> do
    st' <- maybeProcessPendingTick st { ssWorldTransition = False }
    pure (st', ())
  onPure beginShutdown = \() st ->
    ( st
      { ssPendingTick = Nothing
      , ssWorldTransition = True
      , ssShuttingDown = True
      }
    , sifDone <$> ssInFlightTick st
    )
  on_ tick = \req st ->
    submitTickRequest req st
  on_ tickResult = \result st ->
    integrateTickResult result st
  on_ setHandles = \handles st -> do
    let st' = st { ssHandles = Just handles }
    maybeProcessPendingTick st'
  onPure handlesConfigured = \() st ->
    (st, maybe False (const True) (ssHandles st))
  onPure dagSnapshot = \() st ->
    (st, simulationDagSnapshotFromState st)
  on flushWeatherPublication = \() st -> do
    (st', published) <- flushLatestWeatherPublication st
    pure (st', published)
|]

-- ---------------------------------------------------------------------------
-- Public API
-- ---------------------------------------------------------------------------

-- | Store the generated 'TerrainWorld' for simulation.
-- Rebuilds the simulation DAG.
setSimWorld :: ActorHandle Simulation (Protocol Simulation) -> TerrainWorld -> IO ()
setSimWorld handle world =
  cast @"setWorld" handle #setWorld world

-- | Store the generated 'TerrainWorld' with additional executable plugin
-- simulation nodes.  All nodes are executed by the Simulation actor for both
-- manual and automatic ticks.
setSimWorldWithNodes
  :: ActorHandle Simulation (Protocol Simulation)
  -> TerrainWorld
  -> [SimulationNodeBinding]
  -> IO ()
setSimWorldWithNodes handle world pluginNodes =
  cast @"setWorldWithNodes" handle #setWorldWithNodes (world, pluginNodes)

-- | Rebuild the current world's simulation DAG with a new executable plugin
-- node set. Returns 'False' when no stable world is currently bound.
rebindSimNodes
  :: ActorHandle Simulation (Protocol Simulation)
  -> [SimulationNodeBinding]
  -> IO Bool
rebindSimNodes handle pluginNodes =
  call @"rebindNodes" handle #rebindNodes pluginNodes

-- | Clear the stored world (e.g. before a new generation).
clearSimWorld :: ActorHandle Simulation (Protocol Simulation) -> () -> IO ()
clearSimWorld handle () =
  call @"clearWorld" handle #clearWorld ()

beginSimWorldTransition :: ActorHandle Simulation (Protocol Simulation) -> IO ()
beginSimWorldTransition handle =
  call @"beginTransition" handle #beginTransition ()

cancelSimWorldTransition :: ActorHandle Simulation (Protocol Simulation) -> IO ()
cancelSimWorldTransition handle =
  call @"cancelTransition" handle #cancelTransition ()

-- | Stop accepting queued simulation work and return an action that waits for
-- the current background worker, if any. Shutdown callers should call this
-- before stopping the auto scheduler, then run the returned wait action after
-- signalling the scheduler so no pending/manual work can start behind it.
beginSimShutdown :: ActorHandle Simulation (Protocol Simulation) -> IO (IO ())
beginSimShutdown handle = do
  mbDone <- call @"beginShutdown" handle #beginShutdown ()
  pure (maybe (pure ()) readMVar mbDone)

-- | Stop accepting queued simulation work and wait for the current background
-- worker, if any.
waitForSimIdle :: ActorHandle Simulation (Protocol Simulation) -> IO ()
waitForSimIdle handle = do
  waitForIdle <- beginSimShutdown handle
  waitForIdle

-- | Request a single simulation tick.  The argument is the target
-- tick count (usually @uiSimTickCount + 1@).
requestSimTick :: ActorHandle Simulation (Protocol Simulation) -> Word64 -> IO ()
requestSimTick handle tickTarget =
  cast @"tick" handle #tick SimulationTickControl
    { stcKind = SimulationManualTick tickTarget
    , stcResultSink = simulationTickResultSink handle
    }

-- | Attempt one automatic tick and wait for the background worker result.  The
-- Simulation actor only schedules and folds the worker reply, so concurrent
-- simulation/UI control calls remain responsive while this caller waits.
autoTickStep
  :: ActorHandle Simulation (Protocol Simulation)
  -> Maybe Word64
  -> IO AutoTickStepResult
autoTickStep handle expectedVersion =
  autoTickStepArmed handle expectedVersion False

-- | Attempt one automatic scheduler-owned tick. The boolean records that the
-- scheduler had auto-tick armed when it fired, so completion can flush if the
-- UI disables or slows auto-tick while the worker is in-flight.
autoTickStepArmed
  :: ActorHandle Simulation (Protocol Simulation)
  -> Maybe Word64
  -> Bool
  -> IO AutoTickStepResult
autoTickStepArmed handle expectedVersion flushOnIdle = do
  completion <- newEmptyMVar
  cast @"tick" handle #tick SimulationTickControl
    { stcKind = SimulationAutoTick expectedVersion flushOnIdle completion
    , stcResultSink = simulationTickResultSink handle
    }
  readMVar completion

-- | Flush the latest authoritative weather/cloud/day-night render publication
-- when auto-tick is disabled or slowed before the next coalesced tick.
flushSimWeatherPublication :: ActorHandle Simulation (Protocol Simulation) -> IO Bool
flushSimWeatherPublication handle =
  call @"flushWeatherPublication" handle #flushWeatherPublication ()

simulationTickResultSink :: ActorHandle Simulation (Protocol Simulation) -> TickResultSink
simulationTickResultSink handle =
  replyCast (replyTo @SimulationReplyOps handle) tickResultTag

getSimDagSnapshot :: ActorHandle Simulation (Protocol Simulation) -> IO SimulationDagSnapshot
getSimDagSnapshot handle =
  call @"dagSnapshot" handle #dagSnapshot ()

simulationDagSnapshotFromState :: SimState -> SimulationDagSnapshot
simulationDagSnapshotFromState st = case ssDAG st of
  Nothing -> SimulationDagSnapshot
    { sdsAvailable = False
    , sdsWorldBound = maybe False (const True) (ssWorld st)
    , sdsOverlayNames = maybe [] (overlayNames . twOverlays) (ssWorld st)
    , sdsNodes = []
    , sdsLevels = []
    , sdsTerrainWriters = []
    , sdsLastTick = ssLastTick st
    , sdsPendingTick = pendingTickTarget st
    , sdsWorldEpoch = ssWorldEpoch st
    , sdsTickLogs = ssTickLogs st
    }
  Just dag -> SimulationDagSnapshot
    { sdsAvailable = True
    , sdsWorldBound = maybe False (const True) (ssWorld st)
    , sdsOverlayNames = maybe [] (overlayNames . twOverlays) (ssWorld st)
    , sdsNodes = map (nodeSnapshot (ssWorld st) (ssLastTick st) (ssNodeStatuses st) (ssNodeMetadata st)) (sdNodes dag)
    , sdsLevels = map (map simNodeIdText) (sdLevels dag)
    , sdsTerrainWriters = map simNodeIdText (sdTerrainWriters dag)
    , sdsLastTick = ssLastTick st
    , sdsPendingTick = pendingTickTarget st
    , sdsWorldEpoch = ssWorldEpoch st
    , sdsTickLogs = ssTickLogs st
    }

pendingTickTarget :: SimState -> Maybe Word64
pendingTickTarget st = ptRequestedTick <$> ssPendingTick st

nodeSnapshot :: Maybe TerrainWorld -> Word64 -> Map.Map Text (Text, Maybe Text) -> Map.Map Text (Text, Maybe Text) -> SimNode -> SimulationDagNodeSnapshot
nodeSnapshot maybeWorld lastTick statuses metadata node = SimulationDagNodeSnapshot
  { sdnsNodeId = nodeId
  , sdnsKind = kind
  , sdnsPlugin = pluginName
  , sdnsOverlay = simNodeOverlayName node
  , sdnsDependencies = map simNodeIdText (simNodeDependencies node)
  , sdnsWritesTerrain = case node of
      SimNodeWriter{} -> True
      SimNodeReader{} -> False
  , sdnsStatus = status
  , sdnsStatusDetail = detail
  , sdnsScheduleIntervalTicks = schedIntervalTicks <$> scheduleState
  , sdnsSchedulePhaseTicks = schedPhaseTicks <$> scheduleState
  , sdnsScheduleCatchUp = catchUpPolicyText . schedCatchUpPolicy <$> scheduleState
  , sdnsScheduleLastFireTick = scheduleState >>= schedLastFireTick
  , sdnsScheduleNextFireTick = schedNextFireTick <$> scheduleState
  , sdnsScheduleDue = scheduleDue lastTick <$> scheduleState
  }
  where
    nodeId = simNodeIdText (simNodeId node)
    (status, detail) = Map.findWithDefault ("idle", Nothing) nodeId statuses
    (kind, pluginName) = Map.findWithDefault ("builtin", Nothing) nodeId metadata
    scheduleState = do
      world <- maybeWorld
      overlay <- lookupOverlay (simNodeOverlayName node) (twOverlays world)
      normalizeScheduleState <$> opSchedule (ovProvenance overlay)

readyNodeStatuses :: [SimNode] -> Map.Map Text (Text, Maybe Text)
readyNodeStatuses nodes = Map.fromList
  [ (simNodeIdText (simNodeId node), ("ready", Nothing))
  | node <- nodes
  ]

runningNodeStatuses :: [SimNode] -> Map.Map Text (Text, Maybe Text)
runningNodeStatuses nodes = Map.fromList
  [ (simNodeIdText (simNodeId node), ("running", Nothing))
  | node <- nodes
  ]

simNodeIdText :: SimNodeId -> Text
simNodeIdText (SimNodeId value) = value

-- | Wire the data, log, UI, snapshot refs, and atlas handles into the simulation actor.
-- Must be called before any tick requests.
setSimHandles
  :: ActorHandle Simulation (Protocol Simulation)
  -> ActorHandle Data (Protocol Data)
  -> ActorHandle Log (Protocol Log)
  -> ActorHandle Ui (Protocol Ui)
  -> DataSnapshotRef
  -> TerrainSnapshotRef
  -> SnapshotVersionRef
  -> ActorHandle AtlasManager (Protocol AtlasManager)
  -> IO ()
setSimHandles simH dataH logH uiH dataSnapRef terrainSnapRef versionRef atlasH =
  cast @"setHandles" simH #setHandles SimHandles
    { shDataHandle = dataH
    , shLogHandle = logH
    , shUiHandle = uiH
    , shDataSnapshotRef = dataSnapRef
    , shTerrainSnapshotRef = terrainSnapRef
    , shSnapshotVersionRef = versionRef
    , shAtlasHandle = atlasH
    }

-- | Return 'True' once simulation handles have been processed. Because this is
-- an actor call, issuing it after 'setSimHandles' also acts as a startup
-- mailbox barrier for headless/test setup.
simulationHandlesConfigured :: ActorHandle Simulation (Protocol Simulation) -> IO Bool
simulationHandlesConfigured handle =
  call @"handlesConfigured" handle #handlesConfigured ()

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Extract the 'WeatherConfig' from the stored JSON generation config.
-- Falls back to 'defaultWeatherConfig' if the JSON is absent or
-- unparseable.
extractWeatherConfig :: Maybe Value -> WeatherConfig
extractWeatherConfig Nothing = defaultWeatherConfig
extractWeatherConfig (Just val) =
  case fromJSON val :: Result WorldGenConfig of
    Success cfg -> worldWeather cfg
    _           -> defaultWeatherConfig

-- | Built-in simulation nodes.  Currently just weather.
builtinSimNodes :: WeatherConfig -> [SimNode]
builtinSimNodes weatherCfg =
  [ weatherSimNode weatherCfg
  ]

builtinSimulationBindings :: TerrainWorld -> [SimulationNodeBinding]
builtinSimulationBindings world =
  [ SimulationNodeBinding
      { snbNode = node
      , snbKind = "builtin"
      , snbPlugin = Nothing
      }
  | node <- builtinSimNodes (extractWeatherConfig (twGenConfig world))
  ]

simulationBindingsForWorld :: TerrainWorld -> [SimulationNodeBinding] -> [SimulationNodeBinding]
simulationBindingsForWorld world pluginBindings =
  builtinSimulationBindings world <> pluginBindings

-- | Fill overlay schedules from the same built-in and plugin nodes used by the
-- Simulation actor. Plugin cursors are preserved; the built-in weather cursor
-- is rebased when the stored cadence no longer matches the world config.
normalizeWorldSchedulesForBindings :: TerrainWorld -> [SimulationNodeBinding] -> TerrainWorld
normalizeWorldSchedulesForBindings world pluginBindings =
  world { twOverlays = foldr ensureBindingSchedule (twOverlays world) bindings }
  where
    bindings = simulationBindingsForWorld world pluginBindings
    currentTick = wtTick (twWorldTime world)

    ensureBindingSchedule binding store =
      let node = snbNode binding
          overlayName = simNodeOverlayName node
      in case lookupOverlay overlayName store of
        Nothing -> store
        Just overlay -> insertOverlay (normalizeBindingOverlay binding overlay) store

    normalizeBindingOverlay binding overlay
      | isBuiltinWeatherBinding binding =
          reconcileOverlaySchedule currentTick (simNodeSchedule (snbNode binding)) overlay
      | otherwise =
          ensureOverlaySchedule currentTick (simNodeSchedule (snbNode binding)) overlay

    isBuiltinWeatherBinding binding =
      snbKind binding == "builtin"
        && snbPlugin binding == Nothing
        && simNodeIdText (simNodeId (snbNode binding)) == "weather"
        && simNodeOverlayName (snbNode binding) == "weather"

bindWorld :: TerrainWorld -> [SimulationNodeBinding] -> SimState -> IO SimState
bindWorld world pluginBindings st = do
  let calCfg = mkCalendarConfig (twPlanet world)
      bindings = simulationBindingsForWorld world pluginBindings
      nodes = map snbNode bindings
      nodeMetadata = bindingMetadata bindings
      scheduledWorld = normalizeWorldSchedulesForBindings world pluginBindings
      worldTick = wtTick (twWorldTime scheduledWorld)
  case ssHandles st of
    Just handles -> setUiSimTickCount (shUiHandle handles) worldTick
    Nothing -> pure ()
  let drainPending = not (ssWorldTransition st)
  case buildSimDAG nodes of
    Left err -> do
      logMsg st ("simulation: failed to build DAG: " <> err)
      let st' = st { ssWorld = Just scheduledWorld
                   , ssDAG = Nothing
                   , ssCalCfg = Just calCfg
                   , ssLastTick = worldTick
                   , ssWorldEpoch = ssWorldEpoch st + 1
                   , ssWorldTransition = False
                   , ssNodeStatuses = Map.empty
                   , ssNodeMetadata = nodeMetadata
                   , ssLastAutoStatusPublishNs = 0
                   , ssLastAutoWeatherPublishNs = 0
                   , ssAutoWeatherPublicationPending = False
                   }
      if drainPending then maybeProcessPendingTick st' else pure st' { ssPendingTick = Nothing }
    Right dag -> do
      logMsg st ("simulation: setWorld accepted"
        <> " tick=" <> Text.pack (show worldTick)
        <> " terrainChunks=" <> Text.pack (show (IntMap.size (twTerrain scheduledWorld)))
        <> " climateChunks=" <> Text.pack (show (IntMap.size (twClimate scheduledWorld)))
        <> " nodes=" <> Text.pack (show (length nodes)))
      let st' = st { ssWorld = Just scheduledWorld
                   , ssDAG = Just dag
                   , ssCalCfg = Just calCfg
                   , ssLastTick = worldTick
                   , ssWorldEpoch = ssWorldEpoch st + 1
                   , ssWorldTransition = False
                   , ssNodeStatuses = readyNodeStatuses nodes
                   , ssNodeMetadata = nodeMetadata
                   , ssLastAutoStatusPublishNs = 0
                   , ssLastAutoWeatherPublishNs = 0
                   , ssAutoWeatherPublicationPending = False
                   }
      if drainPending then maybeProcessPendingTick st' else pure st' { ssPendingTick = Nothing }

bindingMetadata :: [SimulationNodeBinding] -> Map.Map Text (Text, Maybe Text)
bindingMetadata bindings = Map.fromList
  [ (simNodeIdText (simNodeId (snbNode binding)), (snbKind binding, snbPlugin binding))
  | binding <- bindings
  ]

-- | Log a message via the handles (if available).
logMsg :: SimState -> Text -> IO ()
logMsg st msg =
  case ssHandles st of
    Nothing      -> pure ()
    Just handles -> appendLog (shLogHandle handles) (LogEntry LogInfo msg)

-- | Progress callback that logs and records each simulation node's status.
simProgressCb
  :: SimHandles
  -> IORef (Map.Map Text (Text, Maybe Text))
  -> IORef [SimulationTickLogEntry]
  -> Word64
  -> SimProgress
  -> IO ()
simProgressCb handles statusesRef tickLogsRef tickValue prog = do
  let SimNodeId nid = simpNodeId prog
      (status, detail) = simStatusSnapshot (simpStatus prog)
      msg = "sim: node " <> nid <> " " <> maybe status ((status <> ": ") <>) detail
  modifyIORef' statusesRef (Map.insert nid (status, detail))
  modifyIORef' tickLogsRef (<> [SimulationTickLogEntry tickValue (Just nid) status msg Nothing])
  appendLog (shLogHandle handles) (LogEntry LogDebug msg)

simStatusSnapshot :: SimStatus -> (Text, Maybe Text)
simStatusSnapshot SimStarted = ("running", Nothing)
simStatusSnapshot (SimRunning detail) = ("running", Just detail)
simStatusSnapshot SimCompleted = ("completed", Nothing)
simStatusSnapshot (SimFailed err) = ("failed", Just err)
simStatusSnapshot (SimSkipped reason) = ("skipped", Just reason)

isReadyForTick :: SimState -> Bool
isReadyForTick st =
  not (ssWorldTransition st) &&
    not (ssShuttingDown st) &&
    maybe True (const False) (ssInFlightTick st) &&
    case (ssWorld st, ssDAG st, ssCalCfg st, ssHandles st) of
      (Just _, Just _, Just _, Just _) -> True
      _ -> False

maybeProcessPendingTick :: SimState -> IO SimState
maybeProcessPendingTick st =
  case ssPendingTick st of
    Nothing -> pure st
    Just pending
      | isReadyForTick st ->
          startManualTick (ptResultSink pending) (ptRequestedTick pending) st { ssPendingTick = Nothing }
      | otherwise -> pure st

boundedTickLogs :: [SimulationTickLogEntry] -> [SimulationTickLogEntry]
boundedTickLogs logs = drop (max 0 (length logs - 100)) logs

worldEpochMatches :: SimState -> Maybe Word64 -> Bool
worldEpochMatches _ Nothing = True
worldEpochMatches st (Just expectedEpoch) = ssWorldEpoch st == expectedEpoch

submitTickRequest :: SimulationTickControl -> SimState -> IO SimState
submitTickRequest req st = case stcKind req of
  SimulationManualTick requestedTick ->
    startManualTick (stcResultSink req) requestedTick st
  SimulationAutoTick expectedEpoch flushOnIdle completion ->
    startAutoTick (stcResultSink req) expectedEpoch flushOnIdle completion st

startManualTick :: TickResultSink -> Word64 -> SimState -> IO SimState
startManualTick sink requestedTick st
  | isReadyForTick st =
      case (ssWorld st, ssDAG st, ssCalCfg st, ssHandles st) of
        (Just world, Just dag, Just calCfg, Just handles) ->
          startTickWorker sink SimulationTickNoCompletion Nothing requestedTick st world dag calCfg handles
        _ -> deferManualTick sink requestedTick st
  | otherwise = deferManualTick sink requestedTick st

startAutoTick
  :: TickResultSink
  -> Maybe Word64
  -> Bool
  -> MVar AutoTickStepResult
  -> SimState
  -> IO SimState
startAutoTick sink expectedEpoch flushOnIdle completion st
  | ssWorldTransition st = completeAuto AutoTickUnready
  | ssShuttingDown st = completeAuto AutoTickUnready
  | maybe False (const True) (ssInFlightTick st) = completeAuto AutoTickUnready
  | otherwise =
      case (ssWorld st, ssDAG st, ssCalCfg st, ssHandles st) of
        (Nothing, _, _, _) -> completeAuto AutoTickNoWorld
        (_, Nothing, _, _) -> completeAuto AutoTickUnready
        (_, _, Nothing, _) -> completeAuto AutoTickUnready
        (_, _, _, Nothing) -> completeAuto AutoTickUnready
        (Just world, Just dag, Just calCfg, Just handles)
          | not (worldEpochMatches st expectedEpoch) -> completeAuto AutoTickEpochChanged
          | otherwise ->
              startTickWorker sink (SimulationTickAutoCompletion flushOnIdle completion) expectedEpoch (ssLastTick st + 1) st world dag calCfg handles
  where
    completeAuto reason = do
      completeTickRequest (SimulationTickAutoCompletion flushOnIdle completion) (AutoTickSkipped reason)
      pure st

startTickWorker
  :: TickResultSink
  -> SimulationTickCompletion
  -> Maybe Word64
  -> Word64
  -> SimState
  -> TerrainWorld
  -> SimDAG
  -> CalendarConfig
  -> SimHandles
  -> IO SimState
startTickWorker sink completion expectedEpoch requestedTick st world dag _calCfg handles = do
  done <- newEmptyMVar
  let dt
        | requestedTick > ssLastTick st = requestedTick - ssLastTick st
        | otherwise = 1
      appliedTick = ssLastTick st + dt
      token = ssNextTickToken st
      work = SimulationTickWork
        { stwToken = token
        , stwRequestedTick = requestedTick
        , stwAppliedTick = appliedTick
        , stwDeltaTicks = dt
        , stwExpectedEpoch = expectedEpoch
        , stwWorldEpoch = ssWorldEpoch st
        , stwWorld = world
        , stwDAG = dag
        , stwHandles = handles
        , stwNodeStatuses = ssNodeStatuses st
        , stwCompletion = completion
        }
  when (requestedTick <= ssLastTick st) $
    appendLog (shLogHandle handles)
      (LogEntry LogInfo
        ("simulation: requested tick " <> Text.pack (show requestedTick)
          <> " <= last tick " <> Text.pack (show (ssLastTick st))
          <> "; applying single-step tick to " <> Text.pack (show appliedTick)))
  _ <- forkIO (runSimulationTickWorker sink work)
  pure st
    { ssInFlightTick = Just SimInFlight
        { sifToken = token
        , sifRequestedTick = requestedTick
        , sifAppliedTick = appliedTick
        , sifWorldEpoch = ssWorldEpoch st
        , sifDone = done
        }
    , ssNextTickToken = token + 1
    , ssNodeStatuses = runningNodeStatuses (sdNodes dag)
    }

runSimulationTickWorker :: TickResultSink -> SimulationTickWork -> IO ()
runSimulationTickWorker sink work = do
  let world = stwWorld work
      handles = stwHandles work
  statusesRef <- newIORef (stwNodeStatuses work)
  tickLogsRef <- newIORef []
  tStart <- getMonotonicTimeNSec
  runResult <- try $
    runHourlyPipelineSteps handles statusesRef tickLogsRef (stwDAG work) (stwDeltaTicks work) world
  tEnd <- getMonotonicTimeNSec
  let elapsedMs = fromIntegral (tEnd - tStart) / (1e6 :: Double)
      tickResult = case runResult of
        Left (err :: SomeException) -> Left (Text.pack (displayException err))
        Right result -> result
  nodeStatuses <- readIORef statusesRef
  progressLogs <- readIORef tickLogsRef
  sink SimulationTickResult
    { strToken = stwToken work
    , strRequestedTick = stwRequestedTick work
    , strAppliedTick = stwAppliedTick work
    , strDeltaTicks = stwDeltaTicks work
    , strExpectedEpoch = stwExpectedEpoch work
    , strWorldEpoch = stwWorldEpoch work
    , strBaseWorld = world
    , strHandles = handles
    , strElapsedMs = elapsedMs
    , strNodeStatuses = nodeStatuses
    , strProgressLogs = progressLogs
    , strResult = tickResult
    , strCompletion = stwCompletion work
    }

runHourlyPipelineSteps
  :: SimHandles
  -> IORef (Map.Map Text (Text, Maybe Text))
  -> IORef [SimulationTickLogEntry]
  -> SimDAG
  -> Word64
  -> TerrainWorld
  -> IO (Either Text SimulationTickSuccess)
runHourlyPipelineSteps handles statusesRef tickLogsRef dag totalSteps world0 =
  go totalSteps world0 emptyTerrainWrites
  where
    go 0 world terrainWrites = pure $ Right SimulationTickSuccess
      { stsWorld = world
      , stsTerrainWrites = terrainWrites
      }
    go remaining world terrainWrites = do
      let targetTick = wtTick (twWorldTime world) + 1
      tickResult <- runSimulationTickPipeline
        world
        dag
        (simProgressCb handles statusesRef tickLogsRef targetTick)
        (Just targetTick)
      case tickResult of
        Left err -> pure (Left err)
        Right pipelineResult -> do
          recordPipelineNodeStatuses statusesRef (stprNodeStatuses pipelineResult)
          go
            (remaining - 1)
            (stprWorld pipelineResult)
            (mergeTerrainWrites terrainWrites (stprTerrainWrites pipelineResult))

recordPipelineNodeStatuses
  :: IORef (Map.Map Text (Text, Maybe Text))
  -> [SimulationTickNodeStatus]
  -> IO ()
recordPipelineNodeStatuses statusesRef nodeStatuses =
  modifyIORef' statusesRef $ \statuses -> foldr applyStatus statuses nodeStatuses
  where
    applyStatus nodeStatus statuses =
      let SimNodeId nid = stnsNodeId nodeStatus
      in Map.insert nid (simStatusSnapshot (stnsStatus nodeStatus)) statuses

integrateTickResult :: SimulationTickResult -> SimState -> IO SimState
integrateTickResult result st =
  case ssInFlightTick st of
    Just inFlight | sifToken inFlight == strToken result -> do
      st' <- integrateFreshTickResult result st { ssInFlightTick = Nothing }
      signalInFlightDone inFlight
      pure st'
    _ -> do
      completeTickRequest (strCompletion result) (AutoTickSkipped AutoTickEpochChanged)
      pure st

integrateFreshTickResult :: SimulationTickResult -> SimState -> IO SimState
integrateFreshTickResult result st
  | strWorldEpoch result /= ssWorldEpoch st || not (worldEpochMatches st (strExpectedEpoch result)) = do
      completeTickRequest (strCompletion result) (AutoTickSkipped AutoTickEpochChanged)
      maybeProcessPendingTick st
  | otherwise =
      case strResult result of
        Left err -> do
          logMsg st ("simulation: tick failed: " <> err)
          let failureLog = SimulationTickLogEntry (strAppliedTick result) Nothing "failed" ("simulation: tick failed: " <> err) (Just (strElapsedMs result))
              st' = st
                { ssNodeStatuses = strNodeStatuses result
                , ssTickLogs = boundedTickLogs (ssTickLogs st <> strProgressLogs result <> [failureLog])
                }
          completeTickRequest (strCompletion result) (AutoTickFailed err)
          maybeProcessPendingTick st'
        Right success -> do
          let baseWorld = strBaseWorld result
              world' = stsWorld success
              newStore = twOverlays world'
              terrainWrites = stsTerrainWrites success
              appliedTick = wtTick (twWorldTime world')
              handles = case ssHandles st of
                Just h -> h
                Nothing -> strHandles result
              chunkSize = wcChunkSize (twConfig world')
              terrainChanged = not (IntMap.null (twrTerrain terrainWrites))
              climateChanged = not (IntMap.null (twrClimate terrainWrites))
              vegetationChanged = not (IntMap.null (twrVegetation terrainWrites))
              weatherChunksBefore = getWeatherFromOverlay baseWorld
              weatherChunksAfter = getWeatherFromOverlay world'
              weatherChanged = weatherChunksBefore /= weatherChunksAfter
              overlayChanged = newStore /= twOverlays baseWorld
              overlayNamesChanged = overlayNames newStore /= overlayNames (twOverlays baseWorld)
          when terrainChanged $
            updateTerrainChunkData (shDataHandle handles) chunkSize (twrTerrain terrainWrites)
          when climateChanged $
            updateClimateChunkData (shDataHandle handles) chunkSize (twrClimate terrainWrites)
          when weatherChanged $
            setWeatherChunkData (shDataHandle handles) chunkSize (chunkList weatherChunksAfter)
          when vegetationChanged $
            updateVegetationChunkData (shDataHandle handles) chunkSize (twrVegetation terrainWrites)
          when overlayChanged $
            setOverlayStoreData (shDataHandle handles) newStore
          when overlayNamesChanged $
            setUiOverlayNames (shUiHandle handles) (overlayNames newStore)
          setUiSimTickCount (shUiHandle handles) appliedTick
          uiSnap <- getUiSnapshot (shUiHandle handles)
          let visibleOverlayChanged = selectedOverlayChanged (uiViewMode uiSnap) (twOverlays baseWorld) newStore
          publication <- publishTickSnapshot
            handles
            st
            (isAutoTickCompletion (strCompletion result))
            (autoTickFlushOnIdleCompletion (strCompletion result))
            uiSnap
            terrainChanged
            climateChanged
            weatherChanged
            vegetationChanged
            visibleOverlayChanged
          case (tprTerrainSnapshot publication, tprSnapshotVersion publication) of
            (Just terrainSnap, Just snapshotVersion)
              | atlasPublicationAffected
                  (tprCoalescedWeatherAffected publication)
                  uiSnap
                  terrainChanged
                  climateChanged
                  weatherChanged
                  vegetationChanged
                  visibleOverlayChanged ->
                  enqueueAtlasJobsForPublication handles (strCompletion result) uiSnap terrainSnap snapshotVersion
            _ -> pure ()
          let stPublished = tprState publication
              completeMsg = "simulation: tick " <> Text.pack (show appliedTick)
                <> " completed in " <> Text.pack (show (round (strElapsedMs result) :: Int)) <> "ms"
              completeLog = SimulationTickLogEntry appliedTick Nothing "completed" completeMsg (Just (strElapsedMs result))
              st' = stPublished
                { ssWorld = Just world'
                , ssLastTick = appliedTick
                , ssNodeStatuses = strNodeStatuses result
                , ssTickLogs = boundedTickLogs (ssTickLogs stPublished <> strProgressLogs result <> [completeLog])
                }
          appendLog (shLogHandle handles) (LogEntry LogInfo completeMsg)
          completeTickRequest (strCompletion result) (AutoTickApplied appliedTick)
          maybeProcessPendingTick st'

chunkList :: IntMap.IntMap a -> [(ChunkId, a)]
chunkList = map (\(k, v) -> (ChunkId k, v)) . IntMap.toList

selectedOverlayChanged :: ViewMode -> OverlayStore -> OverlayStore -> Bool
selectedOverlayChanged (ViewOverlay name _) before after =
  lookupOverlay name before /= lookupOverlay name after
selectedOverlayChanged _ _ _ = False

viewAffectedBySimulationPublication
  :: ViewMode
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> Bool
viewAffectedBySimulationPublication mode terrainChanged climateChanged weatherChanged vegetationChanged overlayChanged =
  case mode of
    ViewClimate    -> terrainChanged || climateChanged
    ViewPrecip     -> terrainChanged || climateChanged
    ViewWeather    -> terrainChanged || weatherChanged
    ViewCloud      -> terrainChanged || weatherChanged
    ViewVegetation -> terrainChanged || vegetationChanged
    ViewOverlay{}  -> terrainChanged || overlayChanged
    _              -> terrainChanged

isAutoTickCompletion :: SimulationTickCompletion -> Bool
isAutoTickCompletion SimulationTickNoCompletion = False
isAutoTickCompletion SimulationTickAutoCompletion{} = True

autoTickFlushOnIdleCompletion :: SimulationTickCompletion -> Bool
autoTickFlushOnIdleCompletion SimulationTickNoCompletion = False
autoTickFlushOnIdleCompletion (SimulationTickAutoCompletion flushOnIdle _) = flushOnIdle

data SimulationPublicationPlan = SimulationPublicationPlan
  { sppPublishData :: !Bool
  , sppCoalescedWeatherAffected :: !Bool
  }

data TickPublicationResult = TickPublicationResult
  { tprState :: !SimState
  , tprTerrainSnapshot :: !(Maybe TerrainSnapshot)
  , tprSnapshotVersion :: !(Maybe SnapshotVersion)
  , tprCoalescedWeatherAffected :: !Bool
  }

publishTickSnapshot
  :: SimHandles
  -> SimState
  -> Bool
  -> Bool
  -> UiState
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> IO TickPublicationResult
publishTickSnapshot handles st isAutoTick flushOnIdle uiSnap terrainChanged climateChanged weatherChanged vegetationChanged overlayChanged = do
  now <- getMonotonicTimeNSec
  let plan = simulationPublicationPlan
        st
        isAutoTick
        now
        flushOnIdle
        uiSnap
        terrainChanged
        climateChanged
        weatherChanged
        vegetationChanged
        overlayChanged
  if sppPublishData plan
    then do
      terrainSnap <- getTerrainSnapshot (shDataHandle handles)
      (st', snapshotVersion) <- publishDataSnapshot handles st isAutoTick now terrainSnap
      pure TickPublicationResult
        { tprState = st'
        , tprTerrainSnapshot = Just terrainSnap
        , tprSnapshotVersion = Just snapshotVersion
        , tprCoalescedWeatherAffected = sppCoalescedWeatherAffected plan
        }
    else do
      stStatus <- publishStatusSnapshot handles st now
      let st' = if sppCoalescedWeatherAffected plan
            then stStatus { ssAutoWeatherPublicationPending = True }
            else stStatus
      pure TickPublicationResult
        { tprState = st'
        , tprTerrainSnapshot = Nothing
        , tprSnapshotVersion = Nothing
        , tprCoalescedWeatherAffected = sppCoalescedWeatherAffected plan
        }

simulationPublicationPlan
  :: SimState
  -> Bool
  -> Word64
  -> Bool
  -> UiState
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> SimulationPublicationPlan
simulationPublicationPlan st isAutoTick now flushOnIdle uiSnap terrainChanged climateChanged weatherChanged vegetationChanged overlayChanged =
  SimulationPublicationPlan
    { sppPublishData = publishData
    , sppCoalescedWeatherAffected = coalescedWeatherAffected
    }
  where
    immediateAffected = immediatePublicationAffected
      (uiViewMode uiSnap)
      terrainChanged
      climateChanged
      vegetationChanged
      overlayChanged
    coalescedWeatherAffected = isAutoTick && weatherPublicationAffected st uiSnap weatherChanged
    publishData =
      not isAutoTick
        || immediateAffected
        || (coalescedWeatherAffected && (autoWeatherPublicationDue now st || (flushOnIdle && autoWeatherPublicationFlushBeforeIdle uiSnap)))

immediatePublicationAffected
  :: ViewMode
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> Bool
immediatePublicationAffected mode terrainChanged climateChanged vegetationChanged overlayChanged =
  case mode of
    ViewClimate    -> terrainChanged || climateChanged
    ViewPrecip     -> terrainChanged || climateChanged
    ViewWeather    -> terrainChanged
    ViewCloud      -> terrainChanged
    ViewVegetation -> terrainChanged || vegetationChanged
    ViewOverlay{}  -> terrainChanged || overlayChanged
    _              -> terrainChanged

weatherPublicationAffected :: SimState -> UiState -> Bool -> Bool
weatherPublicationAffected st uiSnap weatherChanged =
  uiDayNightEnabled uiSnap || weatherViewAffected
  where
    weatherViewAffected = case uiViewMode uiSnap of
      ViewWeather -> weatherChanged || ssAutoWeatherPublicationPending st
      ViewCloud   -> weatherChanged || ssAutoWeatherPublicationPending st
      _           -> False

autoWeatherPublicationDue :: Word64 -> SimState -> Bool
autoWeatherPublicationDue now st =
  now - ssLastAutoWeatherPublishNs st >= autoTickWeatherPublishIntervalNs

autoWeatherPublicationFlushBeforeIdle :: UiState -> Bool
autoWeatherPublicationFlushBeforeIdle uiSnap
  | not (uiSimAutoTick uiSnap) = True
  | otherwise = uiSimTickRate uiSnap <= simulationAtlasBackfillRateThreshold

publishDataSnapshot
  :: SimHandles
  -> SimState
  -> Bool
  -> Word64
  -> TerrainSnapshot
  -> IO (SimState, SnapshotVersion)
publishDataSnapshot handles st isAutoTick now terrainSnap = do
  writeTerrainSnapshot (shTerrainSnapshotRef handles) terrainSnap
  bumpSnapshotVersion (shSnapshotVersionRef handles)
  snapshotVersion <- readSnapshotVersion (shSnapshotVersionRef handles)
  let st' = if isAutoTick
        then st
          { ssLastAutoStatusPublishNs = now
          , ssLastAutoWeatherPublishNs = now
          , ssAutoWeatherPublicationPending = False
          }
        else st { ssAutoWeatherPublicationPending = False }
  pure (st', snapshotVersion)

publishStatusSnapshot :: SimHandles -> SimState -> Word64 -> IO SimState
publishStatusSnapshot handles st now
  | now - ssLastAutoStatusPublishNs st >= autoTickStatusPublishIntervalNs = do
      bumpSnapshotVersion (shSnapshotVersionRef handles)
      pure st { ssLastAutoStatusPublishNs = now }
  | otherwise = pure st

atlasPublicationAffected
  :: Bool
  -> UiState
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> Bool
  -> Bool
atlasPublicationAffected coalescedWeatherAffected uiSnap terrainChanged climateChanged weatherChanged vegetationChanged overlayChanged =
  uiDayNightEnabled uiSnap
    || coalescedWeatherAffected
    || viewAffectedBySimulationPublication (uiViewMode uiSnap) terrainChanged climateChanged weatherChanged vegetationChanged overlayChanged

-- | Normalized auto-tick rates at or below this value are slow enough to
-- backfill all zoom stages whenever a weather/cloud/day-night publication is
-- allowed. Faster active auto-tick publishes only the visible stage first.
simulationAtlasBackfillRateThreshold :: Float
simulationAtlasBackfillRateThreshold = 0.4

simulationAtlasBackfillAllowed :: SimulationTickCompletion -> UiState -> Bool
simulationAtlasBackfillAllowed SimulationTickNoCompletion _ = True
simulationAtlasBackfillAllowed SimulationTickAutoCompletion{} uiSnap =
  not (uiSimAutoTick uiSnap)
    || uiSimTickRate uiSnap <= simulationAtlasBackfillRateThreshold

simulationAtlasCurrentStageOnly :: SimulationTickCompletion -> UiState -> Bool
simulationAtlasCurrentStageOnly completion uiSnap =
  simulationAtlasCurrentStageOnlyEligible uiSnap
    && not (simulationAtlasBackfillAllowed completion uiSnap)

simulationAtlasCurrentStageOnlyEligible :: UiState -> Bool
simulationAtlasCurrentStageOnlyEligible uiSnap =
  uiDayNightEnabled uiSnap || case uiViewMode uiSnap of
    ViewWeather -> True
    ViewCloud -> True
    ViewOverlay name _ -> name == "weather"
    _ -> False

simulationAtlasStagesForTick :: SimulationTickCompletion -> UiState -> [ZoomStage]
simulationAtlasStagesForTick completion uiSnap
  | simulationAtlasCurrentStageOnly completion uiSnap = [stageForZoom (uiZoom uiSnap)]
  | otherwise = orderedZoomStagesForZoom (uiZoom uiSnap)

enqueueAtlasJobsForPublication
  :: SimHandles
  -> SimulationTickCompletion
  -> UiState
  -> TerrainSnapshot
  -> SnapshotVersion
  -> IO ()
enqueueAtlasJobsForPublication handles completion uiSnap terrainSnap snapshotVersion = do
  let atlasKey = atlasKeyFor (uiViewMode uiSnap) (uiRenderWaterLevel uiSnap) terrainSnap
      mkJob stage = AtlasJob
        { ajKey = atlasKey
        , ajViewMode = uiViewMode uiSnap
        , ajWaterLevel = uiRenderWaterLevel uiSnap
        , ajSnapshotVersion = snapshotVersion
        , ajTerrain = terrainSnap
        , ajHexRadius = zsHexRadius stage
        , ajAtlasScale = zsAtlasScale stage
        }
  mapM_
    (enqueueAtlasBuild (shAtlasHandle handles) . mkJob)
    (simulationAtlasStagesForTick completion uiSnap)

flushLatestWeatherPublication :: SimState -> IO (SimState, Bool)
flushLatestWeatherPublication st =
  case ssHandles st of
    Nothing -> pure (st, False)
    Just handles
      | maybe False (const True) (ssInFlightTick st) -> pure (st, False)
      | otherwise -> do
          uiSnap <- getUiSnapshot (shUiHandle handles)
          if not (flushWeatherPublicationViewAffected st uiSnap)
            then pure (st, False)
            else do
              latest <- getTerrainSnapshot (shDataHandle handles)
              published <- readTerrainSnapshot (shTerrainSnapshotRef handles)
              if not (flushWeatherPublicationNeeded st uiSnap latest published)
                then pure (st { ssAutoWeatherPublicationPending = False }, False)
                else do
                  now <- getMonotonicTimeNSec
                  (st', snapshotVersion) <- publishDataSnapshot handles st True now latest
                  enqueueAtlasJobsForPublication handles SimulationTickNoCompletion uiSnap latest snapshotVersion
                  pure (st', True)

flushWeatherPublicationViewAffected :: SimState -> UiState -> Bool
flushWeatherPublicationViewAffected _ uiSnap =
  uiDayNightEnabled uiSnap || case uiViewMode uiSnap of
    ViewWeather -> True
    ViewCloud   -> True
    ViewOverlay name _ -> name == "weather"
    _           -> False

flushWeatherPublicationNeeded :: SimState -> UiState -> TerrainSnapshot -> TerrainSnapshot -> Bool
flushWeatherPublicationNeeded st uiSnap latest published
  | uiDayNightEnabled uiSnap = True
  | otherwise = case uiViewMode uiSnap of
      ViewWeather -> ssAutoWeatherPublicationPending st || terrainSnapshotViewVersion ViewWeather latest /= terrainSnapshotViewVersion ViewWeather published
      ViewCloud   -> ssAutoWeatherPublicationPending st || terrainSnapshotViewVersion ViewCloud latest /= terrainSnapshotViewVersion ViewCloud published
      mode@(ViewOverlay name _) ->
        name == "weather"
          && ( ssAutoWeatherPublicationPending st
               || terrainSnapshotViewVersion mode latest /= terrainSnapshotViewVersion mode published
             )
      _           -> False

autoTickWeatherPublishIntervalNs :: Word64
autoTickWeatherPublishIntervalNs = 250000000

autoTickStatusPublishIntervalNs :: Word64
autoTickStatusPublishIntervalNs = autoTickWeatherPublishIntervalNs

signalInFlightDone :: SimInFlight -> IO ()
signalInFlightDone inFlight = do
  _ <- tryPutMVar (sifDone inFlight) ()
  pure ()

completeTickRequest :: SimulationTickCompletion -> AutoTickStepResult -> IO ()
completeTickRequest SimulationTickNoCompletion _ = pure ()
completeTickRequest (SimulationTickAutoCompletion _ completion) result = do
  _ <- tryPutMVar completion result
  pure ()

deferManualTick :: TickResultSink -> Word64 -> SimState -> IO SimState
deferManualTick sink requestedTick st
  | ssShuttingDown st = do
      logMsg st ("simulation: tick ignored during shutdown requested=" <> Text.pack (show requestedTick))
      pure st
  | otherwise = do
      let hasWorld  = maybe "False" (const "True") (ssWorld st)
          hasDag    = maybe "False" (const "True") (ssDAG st)
          hasCalCfg = maybe "False" (const "True") (ssCalCfg st)
          hasHandles = maybe "False" (const "True") (ssHandles st)
          inFlight = maybe "False" (const "True") (ssInFlightTick st)
          queuedTarget = maybe "none" (Text.pack . show . ptRequestedTick) (ssPendingTick st)
          queued' = requestedTick
      let deferredMsg = "simulation: tick deferred (not ready)"
            <> " requested=" <> Text.pack (show requestedTick)
            <> " hasWorld=" <> hasWorld
            <> " hasDag=" <> hasDag
            <> " hasCalCfg=" <> hasCalCfg
            <> " hasHandles=" <> hasHandles
            <> " inFlight=" <> inFlight
            <> " pending=" <> queuedTarget
          deferredLog = SimulationTickLogEntry requestedTick Nothing "deferred" deferredMsg Nothing
      logMsg st deferredMsg
      pure st
        { ssPendingTick = Just PendingTick
            { ptRequestedTick = queued'
            , ptResultSink = sink
            }
        , ssTickLogs = boundedTickLogs (ssTickLogs st <> [deferredLog])
        }
