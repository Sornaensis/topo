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
    -- * Tick control
  , requestSimTick
    -- * Handles setup
  , setSimHandles
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import Control.Monad (when)
import GHC.Clock (getMonotonicTimeNSec)
import Hyperspace.Actor
import Hyperspace.Actor.QQ (hyperspace)
import qualified Data.IntMap.Strict as IntMap

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

import Topo.Calendar
  ( CalendarConfig
  , WorldTime(..)
  , advanceTicks
  , mkCalendarConfig
  , tickToDate
  )
import Topo.Weather (WeatherConfig, defaultWeatherConfig, weatherSimNode)
import Topo.Simulation
  ( SimNode
  , SimProgress(..)
  , SimStatus(..)
  , SimNodeId(..)
  , TerrainWrites
  , applyTerrainWrites
  )
import Topo.Simulation.DAG
  ( SimDAG
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
  }

emptySimState :: SimState
emptySimState = SimState
  { ssWorld    = Nothing
  , ssDAG      = Nothing
  , ssCalCfg   = Nothing
  , ssLastTick = 0
  , ssHandles  = Nothing
  , ssPendingTick = Nothing
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
  cast clearWorld :: ()
  cast tick       :: Word64
  cast setHandles :: SimHandles

  initial emptySimState
  on_ setWorld = \world st -> do
    let calCfg = mkCalendarConfig (twPlanet world)
    let weatherCfg = extractWeatherConfig (twGenConfig world)
    let nodes = builtinSimNodes weatherCfg
    let worldTick = wtTick (twWorldTime world)
    case ssHandles st of
      Just handles -> setUiSimTickCount (shUiHandle handles) worldTick
      Nothing -> pure ()
    case buildSimDAG nodes of
      Left err -> do
        logMsg st ("simulation: failed to build DAG: " <> err)
        let st' = st { ssWorld = Just world
                     , ssDAG = Nothing
                     , ssCalCfg = Just calCfg
                     , ssLastTick = worldTick
                     }
        maybeProcessPendingTick st'
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
                     }
        maybeProcessPendingTick st'
  onPure_ clearWorld = \() st -> st
    { ssWorld  = Nothing
    , ssDAG    = Nothing
    , ssCalCfg = Nothing
    , ssLastTick = 0
    }
  on_ tick = \requestedTick st ->
    processTick requestedTick st
  on_ setHandles = \handles st -> do
    let st' = st { ssHandles = Just handles }
    maybeProcessPendingTick st'
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
  cast @"clearWorld" handle #clearWorld ()

-- | Request a single simulation tick.  The argument is the target
-- tick count (usually @uiSimTickCount + 1@).
requestSimTick :: ActorHandle Simulation (Protocol Simulation) -> Word64 -> IO ()
requestSimTick handle tickTarget =
  cast @"tick" handle #tick tickTarget

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

-- | Progress callback that logs each simulation node's status.
simProgressCb :: SimHandles -> SimProgress -> IO ()
simProgressCb handles prog =
  let SimNodeId nid = simpNodeId prog
      status = case simpStatus prog of
        SimStarted   -> "started"
        SimCompleted -> "completed"
        SimFailed e  -> "FAILED: " <> e
      msg = "sim: node " <> nid <> " " <> status
  in appendLog (shLogHandle handles) (LogEntry LogDebug msg)

isReadyForTick :: SimState -> Bool
isReadyForTick st =
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

processTick :: Word64 -> SimState -> IO SimState
processTick requestedTick st =
  case (ssWorld st, ssDAG st, ssCalCfg st, ssHandles st) of
    (Just world, Just dag, Just calCfg, Just handles) -> do
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
      tStart <- getMonotonicTimeNSec
      result <- tickSimulation dag
                  (simProgressCb handles)
                  world store calDate wt dt
      tEnd <- getMonotonicTimeNSec
      let elapsedMs = fromIntegral (tEnd - tStart) / (1e6 :: Double)
      case result of
        Left err -> do
          logMsg st ("simulation: tick failed: " <> err)
          pure st
        Right (newStore, terrainWrites) -> do
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
              mkJob scale = AtlasJob
                { ajKey = atlasKey
                , ajViewMode = uiViewMode uiSnap
                , ajWaterLevel = uiRenderWaterLevel uiSnap
                , ajTerrain = terrainSnap
                , ajScale = scale
                }
          mapM_ (enqueueAtlasBuild (shAtlasHandle handles) . mkJob) [1 .. 6]
          appendLog (shLogHandle handles)
            (LogEntry LogInfo
              ("simulation: tick " <> Text.pack (show appliedTick)
                <> " completed in " <> Text.pack (show (round elapsedMs :: Int)) <> "ms"))
          pure st
            { ssWorld    = Just world''
            , ssLastTick = appliedTick
            }
    _ -> do
      let hasWorld  = maybe "False" (const "True") (ssWorld st)
          hasDag    = maybe "False" (const "True") (ssDAG st)
          hasCalCfg = maybe "False" (const "True") (ssCalCfg st)
          hasHandles = maybe "False" (const "True") (ssHandles st)
          queuedTarget = maybe "none" (Text.pack . show) (ssPendingTick st)
          queued' = case ssPendingTick st of
            Nothing -> requestedTick
            Just prev -> max prev requestedTick
      logMsg st ("simulation: tick deferred (not ready)"
        <> " requested=" <> Text.pack (show requestedTick)
        <> " hasWorld=" <> hasWorld
        <> " hasDag=" <> hasDag
        <> " hasCalCfg=" <> hasCalCfg
        <> " hasHandles=" <> hasHandles
        <> " pending=" <> queuedTarget)
      pure st { ssPendingTick = Just queued' }
