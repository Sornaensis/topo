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

import Actor.Simulation
  ( SimulationDagNodeSnapshot(..)
  , SimulationDagSnapshot(..)
  , getSimDagSnapshot
  , requestSimTick
  )
import Actor.UI.Setters (setUiSimAutoTick, setUiSimTickRate)
import Actor.UI.State (UiState(..), readUiSnapshotRef)
import Actor.UiActions.Handles (ActorHandles(..))
import Seer.Command.Context (CommandContext(..))
import Topo.Command.Types (SeerResponse, okResponse, errResponse)

-- | Handle @get_sim_state@ — return current simulation state.
handleGetSimState :: CommandContext -> Int -> Value -> IO SeerResponse
handleGetSimState ctx reqId _params = do
  ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
  pure $ okResponse reqId $ object
    [ "auto_tick"  .= uiSimAutoTick ui
    , "tick_rate"  .= uiSimTickRate ui
    , "tick_count" .= uiSimTickCount ui
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
  ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
  let handles = ccActorHandles ctx
      currentTick = uiSimTickCount ui
      targetTick  = currentTick + fromIntegral (max 1 (min 100 count :: Int))
  requestSimTick (ahSimulationHandle handles) targetTick
  pure $ okResponse reqId $ object
    [ "requested_ticks" .= count
    , "target_tick"     .= targetTick
    ]

-- | Handle @get_sim_dag@ — return current simulation DAG topology.
handleGetSimDag :: CommandContext -> Int -> Value -> IO SeerResponse
handleGetSimDag ctx reqId _params = do
  snapshot <- getSimDagSnapshot (ahSimulationHandle (ccActorHandles ctx))
  pure $ okResponse reqId $ object
    [ "available" .= sdsAvailable snapshot
    , "nodes" .= map dagNodeToJSON (sdsNodes snapshot)
    , "levels" .= sdsLevels snapshot
    , "terrain_writers" .= sdsTerrainWriters snapshot
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

dagNodeToJSON :: SimulationDagNodeSnapshot -> Value
dagNodeToJSON node = object
  [ "id" .= sdnsNodeId node
  , "overlay" .= sdnsOverlay node
  , "dependencies" .= sdnsDependencies node
  , "writes_terrain" .= sdnsWritesTerrain node
  ]
