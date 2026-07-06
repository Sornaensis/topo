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
import Data.List (find)
import Data.Text (Text)

import Actor.Data (DataSnapshot(..), getDataSnapshot)
import Actor.Log (LogEntry(..), LogLevel(..), appendLog)
import Actor.PluginManager
  ( PluginSimulationNodeDiagnostic(..)
  , PluginSimulationPlan(..)
  , getPluginSimulationPlan
  )
import Actor.SnapshotReceiver (bumpSnapshotVersion)
import Actor.Simulation
  ( SimulationDagNodeSnapshot(..)
  , SimulationDagSnapshot(..)
  , SimulationTickLogEntry(..)
  , autoTickWeatherPublishIntervalNs
  , flushSimWeatherPublication
  , getSimDagSnapshot
  , requestSimTick
  )
import Actor.UI.Setters (setUiSimAutoTick, setUiSimTickRate)
import Actor.UI.State (UiState(..), getUiSnapshot, readUiSnapshotRef)
import Actor.UiActions.Handles (ActorHandles(..))
import Seer.Command.Context (CommandContext(..))
import Seer.System.AutoTick (autoTickPeriodMicros)
import Topo.Command.Types (SeerResponse, okResponse, errResponse)

-- | Handle @get_sim_state@ — return current simulation state.
handleGetSimState :: CommandContext -> Int -> Value -> IO SeerResponse
handleGetSimState ctx reqId _params = do
  let handles = ccActorHandles ctx
  ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
  dataSnap <- getDataSnapshot (ahDataHandle handles)
  dag <- getSimDagSnapshot (ahSimulationHandle handles)
  pure $ okResponse reqId $ object
    [ "auto_tick"  .= uiSimAutoTick ui
      -- Compatibility field: normalized wall-clock auto rate, not hours per tick.
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
      mapM_ (setUiSimTickRate uiH) mRate
      ui <- getUiSnapshot uiH
      flushed <- if shouldFlushAutoTickPublication ui mRate
        then flushSimWeatherPublication (ahSimulationHandle handles)
        else pure False
      if flushed
        then pure ()
        else bumpSnapshotVersion (ahSnapshotVersionRef handles)
      pure $ okResponse reqId $ object
        [ "auto_tick" .= uiSimAutoTick ui
        , "rate"      .= fmap (const (uiSimTickRate ui)) mRate
        ]

-- | Handle @sim_tick@ — request a number of manual simulation ticks.
--
-- Params: @{ "count"?: int }@ (default 1)
handleSimTick :: CommandContext -> Int -> Value -> IO SeerResponse
handleSimTick ctx reqId params = do
  let count = maybe 1 id (Aeson.parseMaybe parseTickCount params)
      handles = ccActorHandles ctx
  ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
  dataSnap <- getDataSnapshot (ahDataHandle handles)
  if uiGenerating ui
    then do
      appendLog (ahLogHandle handles) (LogEntry LogWarn "sim tick ignored (world generation in progress)")
      pure $ errResponse reqId "world generation in progress"
    else if dsTerrainChunks dataSnap <= 0
      then do
        appendLog (ahLogHandle handles) (LogEntry LogWarn "sim tick ignored (no world terrain loaded yet)")
        pure $ errResponse reqId "no world terrain loaded yet"
      else do
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
  simPlan <- getPluginSimulationPlan pmH $
    if sdsWorldBound snapshot then Just (sdsOverlayNames snapshot) else Nothing
  let nodeValues = map dagNodeToJSON (sdsNodes snapshot)
      boundPluginNodes = filter ((== "plugin") . sdnsKind) (sdsNodes snapshot)
      pluginDeclarationValues = map (pluginSimulationDiagnosticToJSON boundPluginNodes) (pspDiagnostics simPlan)
      pluginDeclarationCount = length pluginDeclarationValues
  pure $ okResponse reqId $ object
    [ "available" .= sdsAvailable snapshot
    , "world_bound" .= sdsWorldBound snapshot
    , "overlay_names" .= sdsOverlayNames snapshot
    , "nodes" .= nodeValues
    , "node_count" .= length nodeValues
    , "levels" .= sdsLevels snapshot
    , "terrain_writers" .= sdsTerrainWriters snapshot
    , "last_tick" .= sdsLastTick snapshot
    , "pending_tick" .= sdsPendingTick snapshot
    , "tick_logs" .= map tickLogToJSON (sdsTickLogs snapshot)
      -- Backward-compatible aliases: these are plugin simulation declaration
      -- diagnostics, not the authoritative actor-bound DAG node list.
    , "plugin_nodes" .= pluginDeclarationValues
    , "plugin_node_count" .= pluginDeclarationCount
    , "plugin_declarations" .= pluginDeclarationValues
    , "plugin_declaration_count" .= pluginDeclarationCount
    , "plugin_simulation_declarations" .= pluginDeclarationValues
    , "plugin_simulation_declaration_count" .= pluginDeclarationCount
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

shouldFlushAutoTickPublication :: UiState -> Maybe Float -> Bool
shouldFlushAutoTickPublication ui mRate =
  not (uiSimAutoTick ui) || case mRate of
    Nothing -> False
    Just _  -> case autoTickPeriodMicros (uiSimTickRate ui) of
      Nothing -> True
      Just micros -> fromIntegral micros * 1000 > autoTickWeatherPublishIntervalNs

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
  , "interval_ticks" .= sdnsScheduleIntervalTicks node
  , "phase_ticks" .= sdnsSchedulePhaseTicks node
  , "catch_up" .= sdnsScheduleCatchUp node
  , "last_fire_tick" .= sdnsScheduleLastFireTick node
  , "next_fire_tick" .= sdnsScheduleNextFireTick node
  , "due" .= sdnsScheduleDue node
  ]

tickLogToJSON :: SimulationTickLogEntry -> Value
tickLogToJSON entry = object
  [ "tick" .= stleTick entry
  , "node_id" .= stleNodeId entry
  , "status" .= stleStatus entry
  , "message" .= stleMessage entry
  , "elapsed_ms" .= stleElapsedMs entry
  ]

pluginSimulationDiagnosticToJSON :: [SimulationDagNodeSnapshot] -> PluginSimulationNodeDiagnostic -> Value
pluginSimulationDiagnosticToJSON boundNodes node = object
  [ "id" .= psndId node
  , "kind" .= ("plugin" :: Text)
  , "plugin" .= psndPlugin node
  , "overlay" .= psndOverlay node
  , "dependencies" .= psndDependencies node
  , "writes_terrain" .= psndWritesTerrain node
  , "status" .= status
  , "status_detail" .= detail
  , "declaration_status" .= psndStatus node
  , "declaration_status_detail" .= psndStatusDetail node
  , "actor_status" .= fmap sdnsStatus mBoundNode
  , "actor_status_detail" .= (mBoundNode >>= sdnsStatusDetail)
  , "interval_ticks" .= maybe (Just (psndScheduleIntervalTicks node)) sdnsScheduleIntervalTicks mBoundNode
  , "phase_ticks" .= maybe (Just (psndSchedulePhaseTicks node)) sdnsSchedulePhaseTicks mBoundNode
  , "catch_up" .= maybe (Just (psndScheduleCatchUp node)) sdnsScheduleCatchUp mBoundNode
  , "last_fire_tick" .= (mBoundNode >>= sdnsScheduleLastFireTick)
  , "next_fire_tick" .= (mBoundNode >>= sdnsScheduleNextFireTick)
  , "due" .= (mBoundNode >>= sdnsScheduleDue)
  , "enabled" .= psndEnabled node
  , "eligible" .= psndExecutable node
  , "eligible_for_binding" .= psndExecutable node
  , "plan_executable" .= psndExecutable node
  , "executable" .= actorBound
  , "bound" .= actorBound
  , "actor_bound" .= actorBound
  ]
  where
    mBoundNode = find ((== psndId node) . sdnsNodeId) boundNodes
    actorBound = maybe False (const True) mBoundNode
    (status, detail) = case mBoundNode of
      Just boundNode -> (sdnsStatus boundNode, sdnsStatusDetail boundNode)
      Nothing
        | psndExecutable node ->
            ( "WaitingForRebind"
            , Just "Plugin simulation declaration is plan-eligible, but it is not bound in the current Simulation actor DAG. Regenerate, reload, or rebind the world to bind this runtime."
            )
        | otherwise -> (psndStatus node, psndStatusDetail node)
