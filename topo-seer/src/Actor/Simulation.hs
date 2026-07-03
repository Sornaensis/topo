{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Simulation actor: holds the generated 'TerrainWorld' and drives
-- the overlay simulation DAG ('tickSimulation') in response to tick
-- requests from the UI.
--
-- After terrain generation, the Terrain actor sends the full world
-- here via 'setSimWorld'.  Each tick request runs one step of the
-- simulation DAG, applies 'TerrainWrites', advances 'WorldTime',
-- and pushes the updated chunk data to the Data actor.
module Actor.Simulation
  ( Simulation
  , simulationActorDef
    -- * World lifecycle
  , setSimWorld
  , setSimWorldWithNodes
  , rebindSimNodes
  , clearSimWorld
  , beginSimWorldTransition
  , cancelSimWorldTransition
    -- * Tick control
  , requestSimTick
  , autoTickStep
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

import Actor.AtlasCache (atlasKeyFor)
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
import Actor.SnapshotReceiver (DataSnapshotRef, TerrainSnapshotRef, SnapshotVersionRef, readSnapshotVersion, writeTerrainSnapshot, bumpSnapshotVersion)
import Actor.UI
  ( Ui
  , UiState(..)
  , ViewMode(..)
  , getUiSnapshot
  , setUiSimTickCount
  , setUiOverlayNames
  )
import Seer.Render.ZoomStage (ZoomStage(..), allZoomStages)

import Topo.Calendar
  ( CalendarConfig
  , WorldTime(..)
  , advanceTicks
  , mkCalendarConfig
  , tickToDate
  )
import Topo.Weather (WeatherConfig, defaultWeatherConfig, weatherSimNode)
import Topo.Simulation
  ( SimNode(..)
  , SimProgress(..)
  , SimulationScheduleState(..)
  , SimStatus(..)
  , SimNodeId(..)
  , TerrainWrites(..)
  , applyTerrainWrites
  , ensureWorldOverlaySchedules
  , catchUpPolicyText
  , scheduleDue
  , simNodeDependencies
  , simNodeId
  , simNodeOverlayName
  )
import Topo.Simulation.DAG
  ( SimDAG(..)
  , buildSimDAG
  , tickSimulation
  )
import Topo (ChunkId(..), getWeatherFromOverlay)
import Topo.World (TerrainWorld(..))
import Topo.Overlay (Overlay(..), OverlayProvenance(..), OverlayStore, lookupOverlay, overlayNames)
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
  | SimulationTickAutoCompletion !(MVar AutoTickStepResult)

data SimulationTickKind
  = SimulationManualTick !Word64
  | SimulationAutoTick !(Maybe Word64) !(MVar AutoTickStepResult)

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
  , stwCalCfg :: !CalendarConfig
  , stwHandles :: !SimHandles
  , stwNodeStatuses :: !(Map.Map Text (Text, Maybe Text))
  , stwCompletion :: !SimulationTickCompletion
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
  , strResult :: !(Either Text (OverlayStore, TerrainWrites))
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
      }
    , ()
    )
  onPure beginTransition = \() st ->
    ( st
      { ssWorldTransition = True
      , ssWorldEpoch = ssWorldEpoch st + 1
      , ssPendingTick = Nothing
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
autoTickStep handle expectedVersion = do
  completion <- newEmptyMVar
  cast @"tick" handle #tick SimulationTickControl
    { stcKind = SimulationAutoTick expectedVersion completion
    , stcResultSink = simulationTickResultSink handle
    }
  readMVar completion

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
      opSchedule (ovProvenance overlay)

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

bindWorld :: TerrainWorld -> [SimulationNodeBinding] -> SimState -> IO SimState
bindWorld world pluginBindings st = do
  let calCfg = mkCalendarConfig (twPlanet world)
      weatherCfg = extractWeatherConfig (twGenConfig world)
      builtinNodes = builtinSimNodes weatherCfg
      builtinBindings =
        [ SimulationNodeBinding
            { snbNode = node
            , snbKind = "builtin"
            , snbPlugin = Nothing
            }
        | node <- builtinNodes
        ]
      bindings = builtinBindings <> pluginBindings
      nodes = map snbNode bindings
      nodeMetadata = bindingMetadata bindings
      worldTick = wtTick (twWorldTime world)
  case ssHandles st of
    Just handles -> setUiSimTickCount (shUiHandle handles) worldTick
    Nothing -> pure ()
  let drainPending = not (ssWorldTransition st)
  case buildSimDAG nodes of
    Left err -> do
      logMsg st ("simulation: failed to build DAG: " <> err)
      let st' = st { ssWorld = Just world
                   , ssDAG = Nothing
                   , ssCalCfg = Just calCfg
                   , ssLastTick = worldTick
                   , ssWorldEpoch = ssWorldEpoch st + 1
                   , ssWorldTransition = False
                   , ssNodeStatuses = Map.empty
                   , ssNodeMetadata = nodeMetadata
                   , ssLastAutoStatusPublishNs = 0
                   }
      if drainPending then maybeProcessPendingTick st' else pure st' { ssPendingTick = Nothing }
    Right dag -> do
      let scheduledWorld = ensureWorldOverlaySchedules nodes world
      logMsg st ("simulation: setWorld accepted"
        <> " tick=" <> Text.pack (show worldTick)
        <> " terrainChunks=" <> Text.pack (show (IntMap.size (twTerrain world)))
        <> " climateChunks=" <> Text.pack (show (IntMap.size (twClimate world)))
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
      (status, detail) = case simpStatus prog of
        SimStarted    -> ("running", Nothing)
        SimCompleted  -> ("completed", Nothing)
        SimFailed e   -> ("failed", Just e)
        SimSkipped e  -> ("skipped", Just e)
      msg = "sim: node " <> nid <> " " <> maybe status ((status <> ": ") <>) detail
  modifyIORef' statusesRef (Map.insert nid (status, detail))
  modifyIORef' tickLogsRef (<> [SimulationTickLogEntry tickValue (Just nid) status msg Nothing])
  appendLog (shLogHandle handles) (LogEntry LogDebug msg)

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
  SimulationAutoTick expectedEpoch completion ->
    startAutoTick (stcResultSink req) expectedEpoch completion st

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
  -> MVar AutoTickStepResult
  -> SimState
  -> IO SimState
startAutoTick sink expectedEpoch completion st
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
              startTickWorker sink (SimulationTickAutoCompletion completion) expectedEpoch (ssLastTick st + 1) st world dag calCfg handles
  where
    completeAuto reason = do
      completeTickRequest (SimulationTickAutoCompletion completion) (AutoTickSkipped reason)
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
startTickWorker sink completion expectedEpoch requestedTick st world dag calCfg handles = do
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
        , stwCalCfg = calCfg
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
      wt = twWorldTime world
      calDate = tickToDate (stwCalCfg work) wt
      store = twOverlays world
      handles = stwHandles work
  statusesRef <- newIORef (stwNodeStatuses work)
  tickLogsRef <- newIORef []
  tStart <- getMonotonicTimeNSec
  runResult <- try $ tickSimulation (stwDAG work)
    (simProgressCb handles statusesRef tickLogsRef (stwAppliedTick work))
    world store calDate wt (stwDeltaTicks work)
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
        Right (newStore, terrainWrites) -> do
          let baseWorld = strBaseWorld result
              wt = twWorldTime baseWorld
              world' = applyTerrainWrites terrainWrites baseWorld
              world'' = world'
                { twOverlays = newStore
                , twWorldTime = advanceTicks (strDeltaTicks result) wt
                }
              handles = case ssHandles st of
                Just h -> h
                Nothing -> strHandles result
              chunkSize = wcChunkSize (twConfig world'')
              terrainChanged = not (IntMap.null (twrTerrain terrainWrites))
              climateChanged = not (IntMap.null (twrClimate terrainWrites))
              vegetationChanged = not (IntMap.null (twrVegetation terrainWrites))
              weatherChunksBefore = getWeatherFromOverlay baseWorld
              weatherChunksAfter = getWeatherFromOverlay world''
              weatherChanged = weatherChunksBefore /= weatherChunksAfter
              overlayChanged = newStore /= twOverlays baseWorld
              overlayNamesChanged = overlayNames newStore /= overlayNames (twOverlays baseWorld)
              dataChanged = terrainChanged || climateChanged || weatherChanged || vegetationChanged || overlayChanged
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
          setUiSimTickCount (shUiHandle handles) (strAppliedTick result)
          terrainSnapMaybe <- if dataChanged
            then Just <$> getTerrainSnapshot (shDataHandle handles)
            else pure Nothing
          uiSnap <- getUiSnapshot (shUiHandle handles)
          case terrainSnapMaybe of
            Just terrainSnap -> writeTerrainSnapshot (shTerrainSnapshotRef handles) terrainSnap
            Nothing -> pure ()
          let visibleDataChanged = viewAffectedBySimulationPublication (uiViewMode uiSnap) terrainChanged climateChanged weatherChanged vegetationChanged overlayChanged
          stPublished <- publishTickSnapshot handles st (isAutoTickCompletion (strCompletion result)) (uiDayNightEnabled uiSnap || visibleDataChanged)
          snapshotVersion <- readSnapshotVersion (shSnapshotVersionRef handles)
          terrainSnapForAtlas <- case terrainSnapMaybe of
            Just terrainSnap -> pure (Just terrainSnap)
            Nothing
              | uiDayNightEnabled uiSnap -> Just <$> getTerrainSnapshot (shDataHandle handles)
              | otherwise -> pure Nothing
          case terrainSnapForAtlas of
            Just terrainSnap
              | uiDayNightEnabled uiSnap || viewAffectedBySimulationPublication (uiViewMode uiSnap) terrainChanged climateChanged weatherChanged vegetationChanged overlayChanged -> do
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
                  mapM_ (enqueueAtlasBuild (shAtlasHandle handles) . mkJob) allZoomStages
            _ -> pure ()
          let completeMsg = "simulation: tick " <> Text.pack (show (strAppliedTick result))
                <> " completed in " <> Text.pack (show (round (strElapsedMs result) :: Int)) <> "ms"
              completeLog = SimulationTickLogEntry (strAppliedTick result) Nothing "completed" completeMsg (Just (strElapsedMs result))
              st' = stPublished
                { ssWorld = Just world''
                , ssLastTick = strAppliedTick result
                , ssNodeStatuses = strNodeStatuses result
                , ssTickLogs = boundedTickLogs (ssTickLogs stPublished <> strProgressLogs result <> [completeLog])
                }
          appendLog (shLogHandle handles) (LogEntry LogInfo completeMsg)
          completeTickRequest (strCompletion result) (AutoTickApplied (strAppliedTick result))
          maybeProcessPendingTick st'

chunkList :: IntMap.IntMap a -> [(ChunkId, a)]
chunkList = map (\(k, v) -> (ChunkId k, v)) . IntMap.toList

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

publishTickSnapshot :: SimHandles -> SimState -> Bool -> Bool -> IO SimState
publishTickSnapshot handles st isAutoTick immediateDataVisible
  | not isAutoTick || immediateDataVisible = do
      bumpSnapshotVersion (shSnapshotVersionRef handles)
      stampAutoPublish st isAutoTick
  | otherwise = do
      now <- getMonotonicTimeNSec
      if now - ssLastAutoStatusPublishNs st >= autoTickStatusPublishIntervalNs
        then do
          bumpSnapshotVersion (shSnapshotVersionRef handles)
          pure st { ssLastAutoStatusPublishNs = now }
        else pure st

stampAutoPublish :: SimState -> Bool -> IO SimState
stampAutoPublish st False = pure st
stampAutoPublish st True = do
  now <- getMonotonicTimeNSec
  pure st { ssLastAutoStatusPublishNs = now }

autoTickStatusPublishIntervalNs :: Word64
autoTickStatusPublishIntervalNs = 250000000

signalInFlightDone :: SimInFlight -> IO ()
signalInFlightDone inFlight = do
  _ <- tryPutMVar (sifDone inFlight) ()
  pure ()

completeTickRequest :: SimulationTickCompletion -> AutoTickStepResult -> IO ()
completeTickRequest SimulationTickNoCompletion _ = pure ()
completeTickRequest (SimulationTickAutoCompletion completion) result = do
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
