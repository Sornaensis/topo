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
import GHC.Clock (getMonotonicTimeNSec)
import Hyperspace.Actor
import Hyperspace.Actor.QQ (hyperspace)

import Actor.Data
  ( Data
  , replaceTerrainData
  )
import Actor.Log
  ( Log
  , LogEntry(..)
  , LogLevel(..)
  , appendLog
  )
import Actor.UI
  ( Ui
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
  { shDataHandle :: !(ActorHandle Data (Protocol Data))
  , shLogHandle  :: !(ActorHandle Log (Protocol Log))
  , shUiHandle   :: !(ActorHandle Ui (Protocol Ui))
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
  }

emptySimState :: SimState
emptySimState = SimState
  { ssWorld    = Nothing
  , ssDAG      = Nothing
  , ssCalCfg   = Nothing
  , ssLastTick = 0
  , ssHandles  = Nothing
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
    let calCfg   = mkCalendarConfig (twPlanet world)
        -- Extract weather config from the stored generation config.
        weatherCfg = extractWeatherConfig (twGenConfig world)
        nodes    = builtinSimNodes weatherCfg
    case buildSimDAG nodes of
      Left err -> do
        logMsg st ("simulation: failed to build DAG: " <> err)
        pure st
          { ssWorld  = Just world
          , ssDAG    = Nothing
          , ssCalCfg = Just calCfg
          , ssLastTick = wtTick (twWorldTime world)
          }
      Right dag -> do
        logMsg st ("simulation: DAG built, "
          <> Text.pack (show (length nodes)) <> " nodes")
        pure st
          { ssWorld  = Just world
          , ssDAG    = Just dag
          , ssCalCfg = Just calCfg
          , ssLastTick = wtTick (twWorldTime world)
          }
  onPure_ clearWorld = \() st -> st
    { ssWorld  = Nothing
    , ssDAG    = Nothing
    , ssCalCfg = Nothing
    , ssLastTick = 0
    }
  on_ tick = \requestedTick st ->
    case (ssWorld st, ssDAG st, ssCalCfg st, ssHandles st) of
      (Just world, Just dag, Just calCfg, Just handles) -> do
        let wt      = twWorldTime world
            dt      = requestedTick - ssLastTick st
            calDate = tickToDate calCfg wt
            store   = twOverlays world
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
            -- Push updated chunks to the Data actor for rendering
            replaceTerrainData (shDataHandle handles) world''
            -- Push overlay names to the UI for the overlay selector
            setUiOverlayNames (shUiHandle handles) (overlayNames (twOverlays world''))
            -- Update the tick counter in the UI
            setUiSimTickCount (shUiHandle handles) requestedTick
            appendLog (shLogHandle handles)
              (LogEntry LogDebug
                ("simulation: tick " <> Text.pack (show requestedTick)
                  <> " completed in " <> Text.pack (show (round elapsedMs :: Int)) <> "ms"))
            pure st
              { ssWorld    = Just world''
              , ssLastTick = requestedTick
              }
      _ -> do
        logMsg st "simulation: tick ignored (no world or DAG)"
        pure st
  onPure_ setHandles = \handles _st -> emptySimState
    { ssHandles = Just handles }
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

-- | Wire the data, log, and UI handles into the simulation actor.
-- Must be called before any tick requests.
setSimHandles
  :: ActorHandle Simulation (Protocol Simulation)
  -> ActorHandle Data (Protocol Data)
  -> ActorHandle Log (Protocol Log)
  -> ActorHandle Ui (Protocol Ui)
  -> IO ()
setSimHandles simH dataH logH uiH =
  cast @"setHandles" simH #setHandles SimHandles
    { shDataHandle = dataH
    , shLogHandle  = logH
    , shUiHandle   = uiH
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
