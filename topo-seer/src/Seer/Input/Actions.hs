{-# LANGUAGE TypeApplications #-}

-- | Shared cached handles and action helpers for Seer input routing.
module Seer.Input.Actions
  ( InputEnv(..)
  , mkInputEnv
  , getUiSnapshot
  , getLogSnapshot
  , getDataSnapshot
  , getTerrainSnapshot
  , actionRequest
  , submitAction
  ) where

import Actor.UiActions.Handles (ActorHandles(..))
import Actor.AtlasManager (AtlasManager)
import Actor.Data (Data, DataSnapshot, TerrainSnapshot)
import Actor.Log (Log, LogSnapshot)
import Actor.PluginManager (PluginManager)
import Actor.Simulation (Simulation)
import Actor.SnapshotReceiver (SnapshotReceiver)
import Actor.Terrain (Terrain, TerrainReplyOps)
import Actor.UI (Ui, UiState)
import Actor.UiActions (UiAction, UiActionRequest(..), UiActions, submitUiAction)
import Hyperspace.Actor (ActorHandle, Protocol, replyTo)

-- | Cached actor handles and render snapshots used while routing a single input event.
data InputEnv = InputEnv
  { ieActorHandles :: !ActorHandles
  , ieUiActionsHandle :: !(ActorHandle UiActions (Protocol UiActions))
  , ieUiSnapshot :: !UiState
  , ieLogSnapshot :: !LogSnapshot
  , ieDataSnapshot :: !DataSnapshot
  , ieTerrainSnapshot :: !TerrainSnapshot
  }

-- | Build an 'InputEnv' from the current input actors and cached snapshots.
mkInputEnv
  :: ActorHandles
  -> ActorHandle UiActions (Protocol UiActions)
  -> UiState
  -> LogSnapshot
  -> DataSnapshot
  -> TerrainSnapshot
  -> InputEnv
mkInputEnv actorHandles uiActionsHandle uiSnapshot logSnapshot dataSnapshot terrainSnapshot =
  InputEnv
    { ieActorHandles = actorHandles
    , ieUiActionsHandle = uiActionsHandle
    , ieUiSnapshot = uiSnapshot
    , ieLogSnapshot = logSnapshot
    , ieDataSnapshot = dataSnapshot
    , ieTerrainSnapshot = terrainSnapshot
    }

-- | Read the cached UI snapshot for the current event.
getUiSnapshot :: InputEnv -> IO UiState
getUiSnapshot = pure . ieUiSnapshot

-- | Read the cached log snapshot for the current event.
getLogSnapshot :: InputEnv -> IO LogSnapshot
getLogSnapshot = pure . ieLogSnapshot

-- | Read the cached data snapshot for the current event.
getDataSnapshot :: InputEnv -> IO DataSnapshot
getDataSnapshot = pure . ieDataSnapshot

-- | Read the cached terrain snapshot for the current event.
getTerrainSnapshot :: InputEnv -> IO TerrainSnapshot
getTerrainSnapshot = pure . ieTerrainSnapshot

-- | Construct a 'UiActionRequest' using the cached input actor handles.
actionRequest :: InputEnv -> UiAction -> UiActionRequest
actionRequest env action =
  UiActionRequest
    { uarAction = action
    , uarActorHandles = ieActorHandles env
    , uarTerrainReplyTo = replyTo @TerrainReplyOps (ieUiActionsHandle env)
    }

-- | Submit a UI action using the cached input actor handles.
submitAction :: InputEnv -> UiAction -> IO ()
submitAction env action =
  submitUiAction (ieUiActionsHandle env) (actionRequest env action)