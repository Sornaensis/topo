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
  , inputServiceContext
  , runInputService
  ) where

import Actor.UiActions.Handles (ActorHandles(..))
import Actor.AtlasManager (AtlasManager)
import Actor.Data (Data, DataSnapshot, TerrainSnapshot)
import Actor.Log (Log, LogSnapshot, LogSnapshotRef)
import Actor.PluginManager (PluginManager)
import Actor.Simulation (Simulation)
import Actor.Terrain (Terrain, TerrainReplyOps)
import Actor.UI (Ui, UiSnapshotRef, UiState)
import Actor.UiActions (UiAction, UiActionRequest(..), UiActions, submitUiAction)
import Data.Aeson (Value)
import Data.Text (Text)
import Hyperspace.Actor (ActorHandle, Protocol, replyTo)
import Seer.Command.AppServiceAdapter (runServiceOperation)
import Seer.DataBrowser.Executor (DataBrowserExecutor)
import Seer.OverlayInspector.Executor.Types (OverlayInspectorExecutor)
import Seer.Service.AppService (AppService)
import Seer.Screenshot.Request (ScreenshotRequestRef)
import Seer.Screenshot.Storage (ScreenshotStoragePolicy)
import Seer.Service.Context (ServiceContext(..), unavailableNestedServiceRunner)
import Seer.Service.Types (ServiceResult)

-- | Cached actor handles and render snapshots used while routing a single input event.
data InputEnv = InputEnv
  { ieAppService :: !AppService
  , ieActorHandles :: !ActorHandles
  , ieUiActionsHandle :: !(ActorHandle UiActions (Protocol UiActions))
  , ieUiSnapshotRef :: !UiSnapshotRef
  , ieScreenshotRef :: !ScreenshotRequestRef
  , ieScreenshotStoragePolicy :: !ScreenshotStoragePolicy
  , ieLogSnapshotRef :: !(Maybe LogSnapshotRef)
  , ieDataBrowserExecutor :: !DataBrowserExecutor
  , ieOverlayInspectorExecutor :: !OverlayInspectorExecutor
  , ieUiSnapshot :: !UiState
  , ieLogSnapshot :: !LogSnapshot
  , ieDataSnapshot :: !DataSnapshot
  , ieTerrainSnapshot :: !TerrainSnapshot
  }

-- | Build an 'InputEnv' from the current input actors and cached snapshots.
mkInputEnv
  :: AppService
  -> ActorHandles
  -> ActorHandle UiActions (Protocol UiActions)
  -> UiSnapshotRef
  -> ScreenshotRequestRef
  -> ScreenshotStoragePolicy
  -> Maybe LogSnapshotRef
  -> DataBrowserExecutor
  -> OverlayInspectorExecutor
  -> UiState
  -> LogSnapshot
  -> DataSnapshot
  -> TerrainSnapshot
  -> InputEnv
mkInputEnv appService actorHandles uiActionsHandle uiSnapshotRef screenshotRef screenshotStoragePolicy logSnapshotRef dataBrowserExecutor overlayInspectorExecutor uiSnapshot logSnapshot dataSnapshot terrainSnapshot =
  InputEnv
    { ieAppService = appService
    , ieActorHandles = actorHandles
    , ieUiActionsHandle = uiActionsHandle
    , ieUiSnapshotRef = uiSnapshotRef
    , ieScreenshotRef = screenshotRef
    , ieScreenshotStoragePolicy = screenshotStoragePolicy
    , ieLogSnapshotRef = logSnapshotRef
    , ieDataBrowserExecutor = dataBrowserExecutor
    , ieOverlayInspectorExecutor = overlayInspectorExecutor
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

inputServiceContext :: InputEnv -> ServiceContext
inputServiceContext env = ServiceContext
  { svcNestedServiceRunner = unavailableNestedServiceRunner
  , svcActorHandles = ieActorHandles env
  , svcUiSnapshotRef = ieUiSnapshotRef env
  , svcUiActionsHandle = ieUiActionsHandle env
  , svcScreenshotRef = ieScreenshotRef env
  , svcScreenshotStoragePolicy = ieScreenshotStoragePolicy env
  , svcLogSnapshotRef = ieLogSnapshotRef env
  , svcEventBus = Nothing
  , svcDataBrowserExecutor = ieDataBrowserExecutor env
  , svcOverlayInspectorExecutor = ieOverlayInspectorExecutor env
  }

-- | Invoke the app service from the UI/input path without constructing a
-- command envelope.
runInputService :: InputEnv -> Text -> Value -> IO ServiceResult
runInputService env =
  runServiceOperation (ieAppService env) (inputServiceContext env)
