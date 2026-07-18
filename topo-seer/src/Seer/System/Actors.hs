{-# LANGUAGE TypeApplications #-}

module Seer.System.Actors
  ( AppActors(..)
  , initialiseAppActors
  , startHttpService
  , shutdownAppActors
  ) where

import Actor.AtlasFreshness (AtlasFreshnessRef, newAtlasFreshnessRef)
import Actor.AtlasManager
  ( AtlasManager
  , AtlasManagerQueueRef
  , newAtlasManagerQueueRef
  , setAtlasManagerFreshnessRef
  , setAtlasManagerQueueRef
  )
import Actor.AtlasResultBroker (AtlasResultRef, newAtlasResultRef)
import Actor.AtlasScheduleBroker (AtlasScheduleRef, newAtlasScheduleRef)
import Actor.AtlasScheduler
  ( AtlasScheduler
  , AtlasSchedulerHandles(..)
  , setAtlasSchedulerHandles
  )
import Actor.AtlasWorker (atlasWorkerActorDef, newAtlasWorkerLoadRef)
import Actor.Data
  ( Data
  , DataSnapshot(..)
  , TerrainSnapshot(..)
  , defaultTerrainGeoContext
  )
import Actor.Log
  ( Log
  , LogSnapshotRef
  , newLogSnapshotRef
  , resetLogFile
  , setLogFileHandle
  , setLogSnapshotRef
  )
import Actor.PluginManager (PluginManager, discoverPlugins, shutdownPlugins)
import Actor.Simulation (Simulation, beginSimShutdown, setSimHandles, simulationHandlesConfigured)
import Actor.SnapshotReceiver
  ( DataSnapshotRef
  , SnapshotVersionRef
  , TerrainSnapshotRef
  , newDataSnapshotRef
  , newRenderSnapshotVersionRef
  , newTerrainSnapshotRef
  )
import Actor.Terrain (Terrain)
import Actor.TerrainCacheBroker (TerrainCacheRef, newTerrainCacheRef)
import Actor.TerrainCacheWorker (TerrainCacheWorker)
import Actor.UI
  ( Ui
  , UiSnapshotRef
  , getUiSnapshot
  , newUiSnapshotRef
  , setUiSeed
  , setUiSeedInput
  , setUiSnapshotRef
  )
import Actor.UiActions (UiActions)
import Actor.UiActions.Handles (ActorHandles(..), mkActorHandles)
import Control.Concurrent
  ( MVar
  , modifyMVar_
  , newMVar
  , readMVar
  )
import Control.Exception (mask, onException)
import Control.Monad (replicateM, when)
import Data.IORef (newIORef)
import qualified Data.Text as Text
import Hyperspace.Actor
  ( ActorHandle
  , ActorSystem
  , Protocol
  , get
  , newActorSystem
  , shutdownActorSystem
  , spawnActor
  )
import Seer.Command.AppServiceAdapter (commandAppService)
import Seer.Command.Context (CommandContext(..), commandServiceContext)
import Seer.Config.Runtime (TopoSeerConfig(..))
import Seer.DataBrowser.Executor
  ( DataBrowserExecutor
  , newDataBrowserExecutor
  , shutdownDataBrowserExecutor
  )
import Seer.Editor.History (emptyHistory)
import Seer.OverlayInspector.Executor
  ( OverlayInspectorExecutor
  , newOverlayInspectorExecutor
  , shutdownOverlayInspectorExecutor
  )
import Seer.HTTP.Server
  ( HttpServerHandle
  , shutdownHttpServer
  , startHttpServer
  )
import Seer.Service.Context (ServiceContext(..), unavailableNestedServiceRunner)
import Seer.Service.Events (newDefaultServiceEventBus)
import Seer.Screenshot.Request
  ( ScreenshotRequestRef
  , newScreenshotRequestRef
  , shutdownScreenshotRequestRef
  )
import Seer.Screenshot.Storage
  ( ScreenshotStoragePolicy
  , initialiseScreenshotStorage
  )
import Seer.System.AutoTick
  ( AutoTickHandles(..)
  , AutoTickScheduler
  , startAutoTickScheduler
  , stopAutoTickScheduler
  )
import Seer.System.Runtime (RuntimeOptions(..))
import System.Random (randomIO)
import Topo.Overlay (emptyOverlayStore)

data AppActors = AppActors
  { aaSystem :: !ActorSystem
  , aaLogHandle :: !(ActorHandle Log (Protocol Log))
  , aaLogSnapshotRef :: !LogSnapshotRef
  , aaUiSnapshotRef :: !UiSnapshotRef
  , aaUiActionsHandle :: !(ActorHandle UiActions (Protocol UiActions))
  , aaDataSnapshotRef :: !DataSnapshotRef
  , aaTerrainSnapshotRef :: !TerrainSnapshotRef
  , aaSnapshotVersionRef :: !SnapshotVersionRef
  , aaActorHandles :: !ActorHandles
  , aaTerrainCacheWorkerHandle :: !(ActorHandle TerrainCacheWorker (Protocol TerrainCacheWorker))
  , aaTerrainCacheRef :: !TerrainCacheRef
  , aaAtlasSchedulerHandle :: !(ActorHandle AtlasScheduler (Protocol AtlasScheduler))
  , aaAtlasScheduleRef :: !AtlasScheduleRef
  , aaAtlasResultRef :: !AtlasResultRef
  , aaAtlasFreshnessRef :: !AtlasFreshnessRef
  , aaAtlasManagerQueueRef :: !AtlasManagerQueueRef
  , aaScreenshotRef :: !ScreenshotRequestRef
  , aaScreenshotStoragePolicy :: !ScreenshotStoragePolicy
  , aaAutoTickScheduler :: !AutoTickScheduler
  , aaDataBrowserExecutor :: !DataBrowserExecutor
  , aaOverlayInspectorExecutor :: !OverlayInspectorExecutor
  , aaHttpServerHandle :: !(MVar (Maybe HttpServerHandle))
  }

initialiseAppActors :: TopoSeerConfig -> IO AppActors
initialiseAppActors runtimeCfg = do
  screenshotStoragePolicy <-
    initialiseScreenshotStorage (cfgScreenshotSaveDirectory runtimeCfg)
  system <- newActorSystem
  logHandle <- get @Log system
  logFileH <- resetLogFile
  setLogFileHandle logHandle logFileH
  logSnapshotRef <- newLogSnapshotRef
  setLogSnapshotRef logHandle logSnapshotRef
  uiHandle <- get @Ui system
  uiSnapshotRef <- newUiSnapshotRef
  setUiSnapshotRef uiHandle uiSnapshotRef
  dataHandle <- get @Data system
  terrainHandle <- get @Terrain system
  atlasManagerHandle <- get @AtlasManager system
  atlasWorkerHandles <- replicateM (cfgAtlasWorkerCount runtimeCfg) (spawnActor atlasWorkerActorDef)
  atlasWorkerNextRef <- newIORef (0 :: Int)
  atlasWorkerLoadRef <- newAtlasWorkerLoadRef
  atlasSchedulerHandle <- get @AtlasScheduler system
  terrainCacheWorkerHandle <- get @TerrainCacheWorker system
  atlasResultRef <- newAtlasResultRef
  atlasScheduleRef <- newAtlasScheduleRef
  atlasFreshnessRef <- newAtlasFreshnessRef
  atlasManagerQueueRef <- newAtlasManagerQueueRef
  terrainCacheRef <- newTerrainCacheRef
  setAtlasManagerFreshnessRef atlasManagerHandle atlasFreshnessRef
  setAtlasManagerQueueRef atlasManagerHandle atlasManagerQueueRef
  setAtlasSchedulerHandles atlasSchedulerHandle AtlasSchedulerHandles
    { ashManager = atlasManagerHandle
    , ashWorkers = atlasWorkerHandles
    , ashWorkerNext = atlasWorkerNextRef
    , ashWorkerLoadRef = atlasWorkerLoadRef
    , ashResultRef = atlasResultRef
    , ashScheduleRef = atlasScheduleRef
    , ashFreshnessRef = atlasFreshnessRef
    }
  uiActionsHandle <- get @UiActions system
  pluginManagerHandle <- get @PluginManager system
  simulationHandle <- get @Simulation system
  discoverPlugins pluginManagerHandle
  let dataSnap = DataSnapshot 0 0 Nothing
      terrainSnap = TerrainSnapshot 0 0 0 0 0 0 mempty mempty mempty mempty mempty mempty mempty mempty mempty emptyOverlayStore defaultTerrainGeoContext
  dataSnapshotRef <- newDataSnapshotRef dataSnap
  terrainSnapshotRef <- newTerrainSnapshotRef terrainSnap
  seed <- randomIO
  setUiSeed uiHandle seed
  setUiSeedInput uiHandle (Text.pack (show seed))
  -- Synchronise seed initialization before capturing committed generation zero.
  _ <- getUiSnapshot uiHandle
  snapshotVersionRef <- newRenderSnapshotVersionRef
    uiSnapshotRef logSnapshotRef dataSnapshotRef terrainSnapshotRef
  setSimHandles simulationHandle dataHandle logHandle uiHandle dataSnapshotRef terrainSnapshotRef snapshotVersionRef atlasManagerHandle
  simReady <- simulationHandlesConfigured simulationHandle
  when (not simReady) (fail "topo-seer startup: simulation handles were not configured")
  autoTickScheduler <- startAutoTickScheduler AutoTickHandles
    { athUiHandle = uiHandle
    , athUiSnapshotRef = uiSnapshotRef
    , athSimulationHandle = simulationHandle
    , athLogHandle = logHandle
    , athSnapshotVersionRef = snapshotVersionRef
    }
  screenshotRef <- newScreenshotRequestRef
  historyRef <- newIORef (emptyHistory 50)
  actorHandles <- mkActorHandles uiHandle logHandle dataHandle terrainHandle atlasManagerHandle dataSnapshotRef terrainSnapshotRef snapshotVersionRef pluginManagerHandle simulationHandle historyRef
  dataBrowserExecutor <- newDataBrowserExecutor uiHandle
  overlayInspectorExecutor <- newOverlayInspectorExecutor actorHandles
  httpServerHandle <- newMVar Nothing
  pure AppActors
    { aaSystem = system
    , aaLogHandle = logHandle
    , aaLogSnapshotRef = logSnapshotRef
    , aaUiSnapshotRef = uiSnapshotRef
    , aaUiActionsHandle = uiActionsHandle
    , aaDataSnapshotRef = dataSnapshotRef
    , aaTerrainSnapshotRef = terrainSnapshotRef
    , aaSnapshotVersionRef = snapshotVersionRef
    , aaActorHandles = actorHandles
    , aaTerrainCacheWorkerHandle = terrainCacheWorkerHandle
    , aaTerrainCacheRef = terrainCacheRef
    , aaAtlasSchedulerHandle = atlasSchedulerHandle
    , aaAtlasScheduleRef = atlasScheduleRef
    , aaAtlasResultRef = atlasResultRef
    , aaAtlasFreshnessRef = atlasFreshnessRef
    , aaAtlasManagerQueueRef = atlasManagerQueueRef
    , aaScreenshotRef = screenshotRef
    , aaScreenshotStoragePolicy = screenshotStoragePolicy
    , aaAutoTickScheduler = autoTickScheduler
    , aaDataBrowserExecutor = dataBrowserExecutor
    , aaOverlayInspectorExecutor = overlayInspectorExecutor
    , aaHttpServerHandle = httpServerHandle
    }

startHttpService :: RuntimeOptions -> AppActors -> IO ()
startHttpService opts actors = mask $ \_ -> case roHttp opts of
  Nothing -> pure ()
  Just httpCfg -> do
    eventBus <- newDefaultServiceEventBus
    let httpServiceContext =
          (commandServiceContext (commandContextForActors actors))
            { svcEventBus = Just eventBus }
    httpHandle <- startHttpServer httpCfg commandAppService httpServiceContext
    modifyMVar_ (aaHttpServerHandle actors) (const (pure (Just httpHandle)))
      `onException` shutdownHttpServer httpHandle

stopHttpService :: AppActors -> IO ()
stopHttpService actors = mask $ \_ -> do
  httpHandle <- readMVar (aaHttpServerHandle actors)
  mapM_ shutdownHttpServer httpHandle
  modifyMVar_ (aaHttpServerHandle actors) (const (pure Nothing))

shutdownAppActors :: AppActors -> IO ()
shutdownAppActors actors = do
  shutdownScreenshotRequestRef (aaScreenshotRef actors)
  -- No HTTP request may acquire new executor ownership once close begins.
  -- Worker completion still has live plugin and actor dependencies.
  stopHttpService actors
  shutdownDataBrowserExecutor (aaDataBrowserExecutor actors)
  shutdownOverlayInspectorExecutor (aaOverlayInspectorExecutor actors)
  waitForSimIdle <- beginSimShutdown (ahSimulationHandle (aaActorHandles actors))
  stopAutoTickScheduler (aaAutoTickScheduler actors)
  waitForSimIdle
  shutdownPlugins (ahPluginManagerHandle (aaActorHandles actors))
  shutdownActorSystem (aaSystem actors)

commandContextForActors :: AppActors -> CommandContext
commandContextForActors actors = CommandContext
  { ccNestedServiceRunner = unavailableNestedServiceRunner
  , ccActorHandles = aaActorHandles actors
  , ccUiSnapshotRef = aaUiSnapshotRef actors
  , ccUiActionsHandle = aaUiActionsHandle actors
  , ccScreenshotRef = aaScreenshotRef actors
  , ccScreenshotStoragePolicy = aaScreenshotStoragePolicy actors
  , ccLogSnapshotRef = Just (aaLogSnapshotRef actors)
  , ccDataBrowserExecutor = aaDataBrowserExecutor actors
  , ccOverlayInspectorExecutor = aaOverlayInspectorExecutor actors
  }

