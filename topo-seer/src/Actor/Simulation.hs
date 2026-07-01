{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
  , clearSimWorld
  , beginSimWorldTransition
  , cancelSimWorldTransition
    -- * Tick control
  , requestSimTick
  , autoTickStep
  , AutoTickStepResult(..)
  , AutoTickSkipReason(..)
    -- * DAG status
  , SimulationDagSnapshot(..)
  , SimulationDagNodeSnapshot(..)
  , SimulationTickLogEntry(..)
  , getSimDagSnapshot
    -- * Handles setup
  , setSimHandles
  , simulationHandlesConfigured
  ) where

import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import Control.Monad (when)
import GHC.Clock (getMonotonicTimeNSec)
import Hyperspace.Actor
import Hyperspace.Actor.QQ (hyperspace)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map

import Actor.AtlasCache (AtlasKey(..))
import Actor.AtlasManager
  ( AtlasManager
  , AtlasJob(..)
  , enqueueAtlasBuild
  )
import Actor.Data
  ( Data
  , TerrainSnapshot(..)
  , getDataSnapshot
  , getTerrainSnapshot
  , replaceTerrainData
  )
import Actor.Log
  ( Log
  , LogEntry(..)
  , LogLevel(..)
  , appendLog
  )
import Actor.SnapshotReceiver (DataSnapshotRef, TerrainSnapshotRef, SnapshotVersionRef, writeDataSnapshot, writeTerrainSnapshot, bumpSnapshotVersion)
import Actor.UI
  ( Ui
  , UiState(..)
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
  , SimStatus(..)
  , SimNodeId(..)
  , TerrainWrites
  , applyTerrainWrites
  , simNodeDependencies
  , simNodeId
  , simNodeOverlayName
  )
import Topo.Simulation.DAG
  ( SimDAG(..)
  , buildSimDAG
  , tickSimulation
  )
import Topo.World (TerrainWorld(..))
import Topo.Overlay (overlayNames)
import Topo.WorldGen (WorldGenConfig(..))
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

data SimulationDagNodeSnapshot = SimulationDagNodeSnapshot
  { sdnsNodeId :: !Text
  , sdnsKind :: !Text
  , sdnsPlugin :: !(Maybe Text)
  , sdnsOverlay :: !Text
  , sdnsDependencies :: ![Text]
  , sdnsWritesTerrain :: !Bool
  , sdnsStatus :: !Text
  , sdnsStatusDetail :: !(Maybe Text)
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

data SimulationDagSnapshot = SimulationDagSnapshot
  { sdsAvailable :: !Bool
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
  , ssPendingTick :: !(Maybe Word64)
    -- ^ Latest requested tick queued while simulation is not ready.
  , ssWorldEpoch :: !Word64
    -- ^ Monotonic world binding epoch used by the auto scheduler to avoid
    -- ticking a world that has been cleared or replaced.
  , ssWorldTransition :: !Bool
    -- ^ True while generation/load is replacing the world.  Manual requests
    -- defer and auto requests skip instead of ticking the previous world.
  , ssNodeStatuses :: !(Map.Map Text (Text, Maybe Text))
    -- ^ Latest observable per-node status and detail for UI/API DAG diagnostics.
  , ssTickLogs :: ![SimulationTickLogEntry]
    -- ^ Bounded tick and per-node status log exposed through the DAG surface.
  }

emptySimState :: SimState
emptySimState = SimState
  { ssWorld    = Nothing
  , ssDAG      = Nothing
  , ssCalCfg   = Nothing
  , ssLastTick = 0
  , ssHandles  = Nothing
  , ssPendingTick = Nothing
  , ssWorldEpoch = 0
  , ssWorldTransition = False
  , ssNodeStatuses = Map.empty
  , ssTickLogs = []
  }

-- ---------------------------------------------------------------------------
-- Actor definition
-- ---------------------------------------------------------------------------

[hyperspace|
actor Simulation
  state SimState
  lifetime Singleton
  schedule pinned 4 sticky
  noDeps
  mailbox Unbounded

  cast setWorld   :: TerrainWorld
  call clearWorld :: () -> ()
  call beginTransition :: () -> ()
  call cancelTransition :: () -> ()
  cast tick       :: Word64
  cast setHandles :: SimHandles
  call autoTick :: Maybe Word64 -> AutoTickStepResult
  call handlesConfigured :: () -> Bool
  call dagSnapshot :: () -> SimulationDagSnapshot

  initial emptySimState
  on_ setWorld = \world st -> do
    let calCfg = mkCalendarConfig (twPlanet world)
    let weatherCfg = extractWeatherConfig (twGenConfig world)
    let nodes = builtinSimNodes weatherCfg
    let worldTick = wtTick (twWorldTime world)
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
                     }
        if drainPending then maybeProcessPendingTick st' else pure st' { ssPendingTick = Nothing }
      Right dag -> do
        logMsg st ("simulation: setWorld accepted"
          <> " tick=" <> Text.pack (show worldTick)
          <> " terrainChunks=" <> Text.pack (show (IntMap.size (twTerrain world)))
          <> " climateChunks=" <> Text.pack (show (IntMap.size (twClimate world)))
          <> " nodes=" <> Text.pack (show (length nodes)))
        let st' = st { ssWorld = Just world
                     , ssDAG = Just dag
                     , ssCalCfg = Just calCfg
                     , ssLastTick = worldTick
                     , ssWorldEpoch = ssWorldEpoch st + 1
                     , ssWorldTransition = False
                     , ssNodeStatuses = readyNodeStatuses nodes
                     }
        if drainPending then maybeProcessPendingTick st' else pure st' { ssPendingTick = Nothing }
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
      , ssTickLogs = []
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
  on_ tick = \requestedTick st ->
    processTick requestedTick st
  on autoTick = \expectedVersion st ->
    processAutoTick expectedVersion st
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

-- | Request a single simulation tick.  The argument is the target
-- tick count (usually @uiSimTickCount + 1@).
requestSimTick :: ActorHandle Simulation (Protocol Simulation) -> Word64 -> IO ()
requestSimTick handle tickTarget =
  cast @"tick" handle #tick tickTarget

-- | Attempt one automatic tick and wait for completion.  The Simulation actor
-- computes the next target from its own state so auto ticking never queues an
-- absolute target from stale UI state.
autoTickStep
  :: ActorHandle Simulation (Protocol Simulation)
  -> Maybe Word64
  -> IO AutoTickStepResult
autoTickStep handle expectedVersion =
  call @"autoTick" handle #autoTick expectedVersion

getSimDagSnapshot :: ActorHandle Simulation (Protocol Simulation) -> IO SimulationDagSnapshot
getSimDagSnapshot handle =
  call @"dagSnapshot" handle #dagSnapshot ()

simulationDagSnapshotFromState :: SimState -> SimulationDagSnapshot
simulationDagSnapshotFromState st = case ssDAG st of
  Nothing -> SimulationDagSnapshot
    { sdsAvailable = False
    , sdsNodes = []
    , sdsLevels = []
    , sdsTerrainWriters = []
    , sdsLastTick = ssLastTick st
    , sdsPendingTick = ssPendingTick st
    , sdsWorldEpoch = ssWorldEpoch st
    , sdsTickLogs = ssTickLogs st
    }
  Just dag -> SimulationDagSnapshot
    { sdsAvailable = True
    , sdsNodes = map (nodeSnapshot (ssNodeStatuses st)) (sdNodes dag)
    , sdsLevels = map (map simNodeIdText) (sdLevels dag)
    , sdsTerrainWriters = map simNodeIdText (sdTerrainWriters dag)
    , sdsLastTick = ssLastTick st
    , sdsPendingTick = ssPendingTick st
    , sdsWorldEpoch = ssWorldEpoch st
    , sdsTickLogs = ssTickLogs st
    }

nodeSnapshot :: Map.Map Text (Text, Maybe Text) -> SimNode -> SimulationDagNodeSnapshot
nodeSnapshot statuses node = SimulationDagNodeSnapshot
  { sdnsNodeId = nodeId
  , sdnsKind = "builtin"
  , sdnsPlugin = Nothing
  , sdnsOverlay = simNodeOverlayName node
  , sdnsDependencies = map simNodeIdText (simNodeDependencies node)
  , sdnsWritesTerrain = case node of
      SimNodeWriter{} -> True
      SimNodeReader{} -> False
  , sdnsStatus = status
  , sdnsStatusDetail = detail
  }
  where
    nodeId = simNodeIdText (simNodeId node)
    (status, detail) = Map.findWithDefault ("idle", Nothing) nodeId statuses

readyNodeStatuses :: [SimNode] -> Map.Map Text (Text, Maybe Text)
readyNodeStatuses nodes = Map.fromList
  [ (simNodeIdText (simNodeId node), ("ready", Nothing))
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
        SimStarted   -> ("running", Nothing)
        SimCompleted -> ("completed", Nothing)
        SimFailed e  -> ("failed", Just e)
      msg = "sim: node " <> nid <> " " <> maybe status ((status <> ": ") <>) detail
  modifyIORef' statusesRef (Map.insert nid (status, detail))
  modifyIORef' tickLogsRef (<> [SimulationTickLogEntry tickValue (Just nid) status msg Nothing])
  appendLog (shLogHandle handles) (LogEntry LogDebug msg)

isReadyForTick :: SimState -> Bool
isReadyForTick st =
  not (ssWorldTransition st) &&
    case (ssWorld st, ssDAG st, ssCalCfg st, ssHandles st) of
      (Just _, Just _, Just _, Just _) -> True
      _ -> False

maybeProcessPendingTick :: SimState -> IO SimState
maybeProcessPendingTick st =
  case ssPendingTick st of
    Nothing -> pure st
    Just pending
      | isReadyForTick st ->
          processTick pending st { ssPendingTick = Nothing }
      | otherwise -> pure st

boundedTickLogs :: [SimulationTickLogEntry] -> [SimulationTickLogEntry]
boundedTickLogs logs = drop (max 0 (length logs - 100)) logs

data TickProcessResult
  = TickProcessApplied !Word64
  | TickProcessFailed !Word64 !Text
  | TickProcessEpochChanged
  deriving (Eq, Show)

processTick :: Word64 -> SimState -> IO SimState
processTick requestedTick st
  | isReadyForTick st =
      case (ssWorld st, ssDAG st, ssCalCfg st, ssHandles st) of
        (Just world, Just dag, Just calCfg, Just handles) ->
          fst <$> processTickReady Nothing requestedTick st world dag calCfg handles
        _ -> deferManualTick requestedTick st
  | otherwise = deferManualTick requestedTick st

processAutoTick :: Maybe Word64 -> SimState -> IO (SimState, AutoTickStepResult)
processAutoTick expectedVersion st
  | ssWorldTransition st = pure (st, AutoTickSkipped AutoTickUnready)
  | otherwise =
      case (ssWorld st, ssDAG st, ssCalCfg st, ssHandles st) of
        (Nothing, _, _, _) -> pure (st, AutoTickSkipped AutoTickNoWorld)
        (_, Nothing, _, _) -> pure (st, AutoTickSkipped AutoTickUnready)
        (_, _, Nothing, _) -> pure (st, AutoTickSkipped AutoTickUnready)
        (_, _, _, Nothing) -> pure (st, AutoTickSkipped AutoTickUnready)
        (Just world, Just dag, Just calCfg, Just handles) -> do
          let epochOk = worldEpochMatches st expectedVersion
          if not epochOk
            then pure (st, AutoTickSkipped AutoTickEpochChanged)
            else do
              let requestedTick = ssLastTick st + 1
              (st', result) <- processTickReady expectedVersion requestedTick st world dag calCfg handles
              pure (st', autoTickResultFromProcess result)

autoTickResultFromProcess :: TickProcessResult -> AutoTickStepResult
autoTickResultFromProcess result = case result of
  TickProcessApplied tickValue -> AutoTickApplied tickValue
  TickProcessFailed _tickValue err -> AutoTickFailed err
  TickProcessEpochChanged -> AutoTickSkipped AutoTickEpochChanged

worldEpochMatches :: SimState -> Maybe Word64 -> Bool
worldEpochMatches _ Nothing = True
worldEpochMatches st (Just expectedEpoch) = ssWorldEpoch st == expectedEpoch

processTickReady
  :: Maybe Word64
  -> Word64
  -> SimState
  -> TerrainWorld
  -> SimDAG
  -> CalendarConfig
  -> SimHandles
  -> IO (SimState, TickProcessResult)
processTickReady expectedVersion requestedTick st world dag calCfg handles = do
  let wt      = twWorldTime world
      dt
        | requestedTick > ssLastTick st = requestedTick - ssLastTick st
        | otherwise = 1
      appliedTick = ssLastTick st + dt
      calDate = tickToDate calCfg wt
      store   = twOverlays world
  when (requestedTick <= ssLastTick st) $
    appendLog (shLogHandle handles)
      (LogEntry LogInfo
        ("simulation: requested tick " <> Text.pack (show requestedTick)
          <> " <= last tick " <> Text.pack (show (ssLastTick st))
          <> "; applying single-step tick to " <> Text.pack (show appliedTick)))
  statusesRef <- newIORef (ssNodeStatuses st)
  tickLogsRef <- newIORef []
  tStart <- getMonotonicTimeNSec
  result <- tickSimulation dag
              (simProgressCb handles statusesRef tickLogsRef appliedTick)
              world store calDate wt dt
  tEnd <- getMonotonicTimeNSec
  let elapsedMs = fromIntegral (tEnd - tStart) / (1e6 :: Double)
  nodeStatuses <- readIORef statusesRef
  progressLogs <- readIORef tickLogsRef
  case result of
    Left err -> do
      logMsg st ("simulation: tick failed: " <> err)
      let failureLog = SimulationTickLogEntry appliedTick Nothing "failed" ("simulation: tick failed: " <> err) (Just elapsedMs)
      pure ( st
        { ssNodeStatuses = nodeStatuses
        , ssTickLogs = boundedTickLogs (ssTickLogs st <> progressLogs <> [failureLog])
        }
        , TickProcessFailed appliedTick err
        )
    Right (newStore, terrainWrites) -> do
      let epochOk = worldEpochMatches st expectedVersion
      if not epochOk
        then pure (st, TickProcessEpochChanged)
        else do
          let world'  = applyTerrainWrites terrainWrites world
              world'' = world'
                { twOverlays  = newStore
                , twWorldTime = advanceTicks dt wt
                }
          replaceTerrainData (shDataHandle handles) world''
          setUiOverlayNames (shUiHandle handles) (overlayNames (twOverlays world''))
          setUiSimTickCount (shUiHandle handles) appliedTick
          dataSnap <- getDataSnapshot (shDataHandle handles)
          terrainSnap <- getTerrainSnapshot (shDataHandle handles)
          writeDataSnapshot (shDataSnapshotRef handles) dataSnap
          writeTerrainSnapshot (shTerrainSnapshotRef handles) terrainSnap
          bumpSnapshotVersion (shSnapshotVersionRef handles)
          uiSnap <- getUiSnapshot (shUiHandle handles)
          let atlasKey = AtlasKey (uiViewMode uiSnap) (uiRenderWaterLevel uiSnap) (tsVersion terrainSnap)
              mkJob stage = AtlasJob
                { ajKey        = atlasKey
                , ajViewMode   = uiViewMode uiSnap
                , ajWaterLevel = uiRenderWaterLevel uiSnap
                , ajTerrain    = terrainSnap
                , ajHexRadius  = zsHexRadius stage
                , ajAtlasScale = zsAtlasScale stage
                }
          mapM_ (enqueueAtlasBuild (shAtlasHandle handles) . mkJob) allZoomStages
          let completeMsg = "simulation: tick " <> Text.pack (show appliedTick)
                <> " completed in " <> Text.pack (show (round elapsedMs :: Int)) <> "ms"
              completeLog = SimulationTickLogEntry appliedTick Nothing "completed" completeMsg (Just elapsedMs)
          appendLog (shLogHandle handles) (LogEntry LogInfo completeMsg)
          pure ( st
            { ssWorld    = Just world''
            , ssLastTick = appliedTick
            , ssNodeStatuses = nodeStatuses
            , ssTickLogs = boundedTickLogs (ssTickLogs st <> progressLogs <> [completeLog])
            }
            , TickProcessApplied appliedTick
            )

deferManualTick :: Word64 -> SimState -> IO SimState
deferManualTick requestedTick st = do
  let hasWorld  = maybe "False" (const "True") (ssWorld st)
      hasDag    = maybe "False" (const "True") (ssDAG st)
      hasCalCfg = maybe "False" (const "True") (ssCalCfg st)
      hasHandles = maybe "False" (const "True") (ssHandles st)
      queuedTarget = maybe "none" (Text.pack . show) (ssPendingTick st)
      queued' = case ssPendingTick st of
        Nothing -> requestedTick
        Just prev -> max prev requestedTick
  let deferredMsg = "simulation: tick deferred (not ready)"
        <> " requested=" <> Text.pack (show requestedTick)
        <> " hasWorld=" <> hasWorld
        <> " hasDag=" <> hasDag
        <> " hasCalCfg=" <> hasCalCfg
        <> " hasHandles=" <> hasHandles
        <> " pending=" <> queuedTarget
      deferredLog = SimulationTickLogEntry requestedTick Nothing "deferred" deferredMsg Nothing
  logMsg st deferredMsg
  pure st
    { ssPendingTick = Just queued'
    , ssTickLogs = boundedTickLogs (ssTickLogs st <> [deferredLog])
    }
