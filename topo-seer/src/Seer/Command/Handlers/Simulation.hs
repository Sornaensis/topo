{-# LANGUAGE OverloadedStrings #-}

-- | IPC handlers for simulation control: @get_sim_state@,
-- @set_sim_auto_tick@, @sim_tick@, and @get_sim_dag@.
module Seer.Command.Handlers.Simulation
  ( handleGetSimState
  , handleSetSimAutoTick
  , handleSimTick
  , handleGetSimDag
  ) where

import Data.Aeson (Value(..), object, (.=), (.:), (.:?))
import qualified Data.Aeson.Types as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)

import Actor.Data (DataSnapshot(..), getDataSnapshot)
import Actor.Log (LogEntry(..), LogLevel(..), appendLog)
import Actor.PluginManager
  ( LoadedPlugin(..)
  , getDisabledPlugins
  , getLoadedPlugins
  , pluginAvailableDependencyKeys
  , pluginDiagnosticDetail
  , pluginDiagnosticState
  , pluginDiagnosticStateText
  )
import Actor.Simulation
  ( SimulationDagNodeSnapshot(..)
  , SimulationDagSnapshot(..)
  , SimulationTickLogEntry(..)
  , getSimDagSnapshot
  , requestSimTick
  )
import Actor.UI.Setters (setUiSimAutoTick, setUiSimTickRate)
import Actor.UI.State (UiState(..), readUiSnapshotRef)
import Actor.UiActions.Handles (ActorHandles(..))
import Seer.Command.Context (CommandContext(..))
import Topo.Command.Types (SeerResponse, okResponse, errResponse)
import Topo.Plugin (Capability(..))
import Topo.Plugin.RPC.Manifest (RPCManifest(..), RPCSimulationDecl(..))

-- | Handle @get_sim_state@ — return current simulation state.
handleGetSimState :: CommandContext -> Int -> Value -> IO SeerResponse
handleGetSimState ctx reqId _params = do
  let handles = ccActorHandles ctx
  ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
  dataSnap <- getDataSnapshot (ahDataHandle handles)
  dag <- getSimDagSnapshot (ahSimulationHandle handles)
  pure $ okResponse reqId $ object
    [ "auto_tick"  .= uiSimAutoTick ui
    , "tick_rate"  .= uiSimTickRate ui
    , "tick_count" .= uiSimTickCount ui
    , "dag_available" .= sdsAvailable dag
    , "dag_node_count" .= length (sdsNodes dag)
    , "pending_tick" .= sdsPendingTick dag
    , "last_tick_log" .= case reverse (sdsTickLogs dag) of
        [] -> Null
        entry:_ -> tickLogToJSON entry
    , "async_status" .= object
        [ "name" .= ("simulation" :: Text)
        , "phase" .= simulationPhase dataSnap dag
        , "active" .= uiSimAutoTick ui
        , "current" .= uiSimTickCount ui
        , "total" .= sdsPendingTick dag
        , "message" .= simulationStatusMessage dataSnap dag
        ]
    ]

-- | Handle @set_sim_auto_tick@ — enable/disable auto-tick and optionally set rate.
--
-- Params: @{ "enabled": bool, "rate"?: float }@
handleSetSimAutoTick :: CommandContext -> Int -> Value -> IO SeerResponse
handleSetSimAutoTick ctx reqId params = do
  case Aeson.parseMaybe parseAutoTick params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'enabled' parameter"
    Just (enabled, mRate) -> do
      let handles = ccActorHandles ctx
          uiH = ahUiHandle handles
      setUiSimAutoTick uiH enabled
      case mRate of
        Just rate | rate > 0 -> setUiSimTickRate uiH rate
        _ -> pure ()
      pure $ okResponse reqId $ object
        [ "auto_tick" .= enabled
        , "rate"      .= mRate
        ]

-- | Handle @sim_tick@ — request a number of manual simulation ticks.
--
-- Params: @{ "count"?: int }@ (default 1)
handleSimTick :: CommandContext -> Int -> Value -> IO SeerResponse
handleSimTick ctx reqId params = do
  let count = maybe 1 id (Aeson.parseMaybe parseTickCount params)
      handles = ccActorHandles ctx
  dataSnap <- getDataSnapshot (ahDataHandle handles)
  if dsTerrainChunks dataSnap <= 0
    then do
      appendLog (ahLogHandle handles) (LogEntry LogWarn "sim tick ignored (no world terrain loaded yet)")
      pure $ errResponse reqId "no world terrain loaded yet"
    else do
      ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
      let currentTick = uiSimTickCount ui
          targetTick  = currentTick + fromIntegral (max 1 (min 100 count :: Int))
      requestSimTick (ahSimulationHandle handles) targetTick
      pure $ okResponse reqId $ object
        [ "requested_ticks" .= count
        , "target_tick"     .= targetTick
        ]

-- | Handle @get_sim_dag@ — return current simulation DAG topology.
handleGetSimDag :: CommandContext -> Int -> Value -> IO SeerResponse
handleGetSimDag ctx reqId _params = do
  let handles = ccActorHandles ctx
      pmH = ahPluginManagerHandle handles
  snapshot <- getSimDagSnapshot (ahSimulationHandle handles)
  plugins <- getLoadedPlugins pmH
  disabled <- getDisabledPlugins pmH
  let availableDeps = pluginAvailableDependencyKeys disabled plugins
      builtinNodes = map builtinSurfaceNode (sdsNodes snapshot)
      pluginNodes = pluginSimulationNodes disabled availableDeps plugins
      allSurfaceNodes = builtinNodes <> pluginNodes
      allNodeValues = map dsnJSON allSurfaceNodes
      pluginNodeValues = map dsnJSON pluginNodes
      topoLevels = surfaceTopoLevels allSurfaceNodes
  pure $ okResponse reqId $ object
    [ "available" .= sdsAvailable snapshot
    , "nodes" .= allNodeValues
    , "node_count" .= length allNodeValues
    , "levels" .= surfaceReaderLevels allSurfaceNodes topoLevels
    , "terrain_writers" .= surfaceTerrainWriters allSurfaceNodes topoLevels
    , "last_tick" .= sdsLastTick snapshot
    , "pending_tick" .= sdsPendingTick snapshot
    , "tick_logs" .= map tickLogToJSON (sdsTickLogs snapshot)
    , "plugin_nodes" .= pluginNodeValues
    , "plugin_node_count" .= length pluginNodeValues
    ]

-- --------------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------------

parseAutoTick :: Value -> Aeson.Parser (Bool, Maybe Float)
parseAutoTick = Aeson.withObject "set_sim_auto_tick" $ \o ->
  (,) <$> o .: "enabled" <*> o .:? "rate"

parseTickCount :: Value -> Aeson.Parser Int
parseTickCount = Aeson.withObject "sim_tick" $ \o ->
  maybe 1 id <$> o .:? "count"

simulationPhase :: DataSnapshot -> SimulationDagSnapshot -> Text
simulationPhase dataSnap dag
  | dsTerrainChunks dataSnap <= 0 = "unavailable"
  | sdsPendingTick dag /= Nothing = "queued"
  | sdsAvailable dag = "idle"
  | otherwise = "unavailable"

simulationStatusMessage :: DataSnapshot -> SimulationDagSnapshot -> Maybe Text
simulationStatusMessage dataSnap dag
  | dsTerrainChunks dataSnap <= 0 = Just "no terrain loaded"
  | not (sdsAvailable dag) = Just "simulation DAG unavailable"
  | otherwise = Nothing

dagNodeToJSON :: SimulationDagNodeSnapshot -> Value
dagNodeToJSON node = object
  [ "id" .= sdnsNodeId node
  , "kind" .= sdnsKind node
  , "plugin" .= sdnsPlugin node
  , "overlay" .= sdnsOverlay node
  , "dependencies" .= sdnsDependencies node
  , "writes_terrain" .= sdnsWritesTerrain node
  , "status" .= sdnsStatus node
  , "status_detail" .= sdnsStatusDetail node
  ]

tickLogToJSON :: SimulationTickLogEntry -> Value
tickLogToJSON entry = object
  [ "tick" .= stleTick entry
  , "node_id" .= stleNodeId entry
  , "status" .= stleStatus entry
  , "message" .= stleMessage entry
  , "elapsed_ms" .= stleElapsedMs entry
  ]

data DagSurfaceNode = DagSurfaceNode
  { dsnId :: !Text
  , dsnDependencies :: ![Text]
  , dsnWritesTerrain :: !Bool
  , dsnJSON :: !Value
  }

builtinSurfaceNode :: SimulationDagNodeSnapshot -> DagSurfaceNode
builtinSurfaceNode node = DagSurfaceNode
  { dsnId = sdnsNodeId node
  , dsnDependencies = sdnsDependencies node
  , dsnWritesTerrain = sdnsWritesTerrain node
  , dsnJSON = dagNodeToJSON node
  }

pluginSimulationNodes :: Set.Set Text -> Set.Set Text -> [LoadedPlugin] -> [DagSurfaceNode]
pluginSimulationNodes disabled availableDeps plugins =
  [ pluginSimulationNode disabled availableDeps plugin sim
  | plugin <- plugins
  , Just sim <- [rmSimulation (lpManifest plugin)]
  ]

pluginSimulationNode :: Set.Set Text -> Set.Set Text -> LoadedPlugin -> RPCSimulationDecl -> DagSurfaceNode
pluginSimulationNode disabled availableDeps plugin sim = DagSurfaceNode
  { dsnId = pluginName
  , dsnDependencies = rsdDependencies sim
  , dsnWritesTerrain = writesTerrain
  , dsnJSON = object
      [ "id" .= pluginName
      , "kind" .= ("plugin" :: Text)
      , "plugin" .= pluginName
      , "overlay" .= pluginName
      , "dependencies" .= rsdDependencies sim
      , "writes_terrain" .= writesTerrain
      , "status" .= pluginDiagnosticStateText diagnostic
      , "status_detail" .= pluginDiagnosticDetail disabled availableDeps plugin
      , "enabled" .= not (Set.member pluginName disabled)
      ]
  }
  where
    pluginName = lpName plugin
    writesTerrain = pluginWritesTerrain (lpManifest plugin)
    diagnostic = pluginDiagnosticState disabled availableDeps plugin

surfaceTopoLevels :: [DagSurfaceNode] -> [[Text]]
surfaceTopoLevels nodes = go initialInDegrees []
  where
    nodeIds = Set.fromList (map dsnId nodes)
    dependenciesInGraph node = filter (`Set.member` nodeIds) (dsnDependencies node)
    initialInDegrees = Map.fromList
      [ (dsnId node, length (dependenciesInGraph node))
      | node <- nodes
      ]
    dependents = Map.fromListWith (<>)
      [ (dependency, [dsnId node])
      | node <- nodes
      , dependency <- dependenciesInGraph node
      ]
    orderedReady inDegrees =
      [ dsnId node
      | node <- nodes
      , Map.lookup (dsnId node) inDegrees == Just 0
      ]
    orderedRemaining inDegrees =
      [ dsnId node
      | node <- nodes
      , Map.member (dsnId node) inDegrees
      ]
    go inDegrees acc
      | Map.null inDegrees = reverse acc
      | null ready = reverse (orderedRemaining inDegrees : acc)
      | otherwise = go inDegreesAfterReady (ready : acc)
      where
        ready = orderedReady inDegrees
        withoutReady = foldr Map.delete inDegrees ready
        inDegreesAfterReady = foldr decrementDependents withoutReady ready
    decrementDependents nodeId inDegrees =
      foldr (Map.adjust (subtract 1)) inDegrees (Map.findWithDefault [] nodeId dependents)

surfaceReaderLevels :: [DagSurfaceNode] -> [[Text]] -> [[Text]]
surfaceReaderLevels nodes topoLevels = filter (not . null) (map (filter (not . writesTerrain)) topoLevels)
  where
    writesTerrain nodeId = maybe False dsnWritesTerrain (Map.lookup nodeId nodeMap)
    nodeMap = Map.fromList [(dsnId node, node) | node <- nodes]

surfaceTerrainWriters :: [DagSurfaceNode] -> [[Text]] -> [Text]
surfaceTerrainWriters nodes topoLevels =
  [ nodeId
  | level <- topoLevels
  , nodeId <- level
  , maybe False dsnWritesTerrain (Map.lookup nodeId nodeMap)
  ]
  where
    nodeMap = Map.fromList [(dsnId node, node) | node <- nodes]

pluginWritesTerrain :: RPCManifest -> Bool
pluginWritesTerrain manifest = any (`elem` rmCapabilities manifest) [CapWriteTerrain, CapWriteWorld]
