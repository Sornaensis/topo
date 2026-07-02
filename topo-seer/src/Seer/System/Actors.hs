{-# LANGUAGE TypeApplications #-}

module Seer.System.Actors
  ( AppActors(..)
  , initialiseAppActors
  , startCommandServices
  , shutdownAppActors
  ) where

import Actor.AtlasFreshness (AtlasFreshnessRef, newAtlasFreshnessRef)
import Actor.AtlasManager (AtlasManager, setAtlasManagerFreshnessRef)
import Actor.AtlasResultBroker (AtlasResultRef, newAtlasResultRef)
import Actor.AtlasScheduleBroker (AtlasScheduleRef, newAtlasScheduleRef)
import Actor.AtlasScheduler
  ( AtlasScheduler
  , AtlasSchedulerHandles(..)
  , setAtlasSchedulerHandles
  )
import Actor.AtlasWorker (atlasWorkerActorDef)
import Actor.Data
  ( Data
  , DataSnapshot(..)
  , TerrainSnapshot(..)
  )
import Actor.Log
  ( Log
  , LogSnapshotRef
  , newLogSnapshotRef
  , resetLogFile
  , setLogFileHandle
  , setLogSnapshotRef
  )
import Actor.PluginManager (PluginManager, discoverPlugins)
import Actor.Simulation (Simulation, beginSimShutdown, setSimHandles, simulationHandlesConfigured)
import Actor.SnapshotReceiver
  ( DataSnapshotRef
  , SnapshotVersionRef
  , TerrainSnapshotRef
  , newDataSnapshotRef
  , newSnapshotVersionRef
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
import Control.Concurrent (forkIO)
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
import Seer.Command.Channel (CommandChannelEnv(..), runCommandChannel)
import Seer.Command.Context (CommandContext(..), commandServiceContext)
import Seer.Config.Runtime (TopoSeerConfig(..))
import Seer.Editor.History (emptyHistory)
import Seer.HTTP.Server (forkHttpServer)
import Seer.Service.Context (ServiceContext(..))
import Seer.Service.Events (newDefaultServiceEventBus)
import Seer.Screenshot.Request (ScreenshotRequestRef, newScreenshotRequestRef)
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
  , aaScreenshotRef :: !ScreenshotRequestRef
  , aaAutoTickScheduler :: !AutoTickScheduler
  }

initialiseAppActors :: TopoSeerConfig -> IO AppActors
initialiseAppActors runtimeCfg = do
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
  atlasSchedulerHandle <- get @AtlasScheduler system
  terrainCacheWorkerHandle <- get @TerrainCacheWorker system
  atlasResultRef <- newAtlasResultRef
  atlasScheduleRef <- newAtlasScheduleRef
  atlasFreshnessRef <- newAtlasFreshnessRef
  terrainCacheRef <- newTerrainCacheRef
  setAtlasManagerFreshnessRef atlasManagerHandle atlasFreshnessRef
  setAtlasSchedulerHandles atlasSchedulerHandle AtlasSchedulerHandles
    { ashManager = atlasManagerHandle
    , ashWorkers = atlasWorkerHandles
    , ashWorkerNext = atlasWorkerNextRef
    , ashResultRef = atlasResultRef
    , ashScheduleRef = atlasScheduleRef
    , ashFreshnessRef = atlasFreshnessRef
    }
  uiActionsHandle <- get @UiActions system
  pluginManagerHandle <- get @PluginManager system
  simulationHandle <- get @Simulation system
  discoverPlugins pluginManagerHandle
  let dataSnap = DataSnapshot 0 0 Nothing
      terrainSnap = TerrainSnapshot 0 0 0 0 0 0 mempty mempty mempty mempty mempty mempty mempty mempty mempty emptyOverlayStore
  dataSnapshotRef <- newDataSnapshotRef dataSnap
  terrainSnapshotRef <- newTerrainSnapshotRef terrainSnap
  snapshotVersionRef <- newSnapshotVersionRef
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
  seed <- randomIO
  setUiSeed uiHandle seed
  setUiSeedInput uiHandle (Text.pack (show seed))
  -- Synchronise the async seed casts before render/HTTP readers use the UI snapshot.
  _ <- getUiSnapshot uiHandle
  screenshotRef <- newScreenshotRequestRef
  historyRef <- newIORef (emptyHistory 50)
  let actorHandles = mkActorHandles uiHandle logHandle dataHandle terrainHandle atlasManagerHandle dataSnapshotRef terrainSnapshotRef snapshotVersionRef pluginManagerHandle simulationHandle historyRef
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
    , aaScreenshotRef = screenshotRef
    , aaAutoTickScheduler = autoTickScheduler
    }

startCommandServices :: RuntimeOptions -> AppActors -> IO ()
startCommandServices opts actors = do
  let cmdContext = commandContextForActors actors
      cmdEnv = commandEnvForActors actors
  _ <- forkIO (runCommandChannel cmdEnv)
  eventBus <- newDefaultServiceEventBus
  let httpServiceContext = (commandServiceContext cmdContext) { svcEventBus = Just eventBus }
  case roHttp opts of
    Nothing -> pure ()
    Just httpCfg -> do
      _ <- forkHttpServer httpCfg commandAppService httpServiceContext
      pure ()

shutdownAppActors :: AppActors -> IO ()
shutdownAppActors actors = do
  waitForSimIdle <- beginSimShutdown (ahSimulationHandle (aaActorHandles actors))
  stopAutoTickScheduler (aaAutoTickScheduler actors)
  waitForSimIdle
  shutdownActorSystem (aaSystem actors)

commandContextForActors :: AppActors -> CommandContext
commandContextForActors actors = CommandContext
  { ccActorHandles = aaActorHandles actors
  , ccUiSnapshotRef = aaUiSnapshotRef actors
  , ccUiActionsHandle = aaUiActionsHandle actors
  , ccScreenshotRef = aaScreenshotRef actors
  , ccLogSnapshotRef = Just (aaLogSnapshotRef actors)
  }

commandEnvForActors :: AppActors -> CommandChannelEnv
commandEnvForActors actors = CommandChannelEnv
  { cceActorHandles = aaActorHandles actors
  , cceUiSnapshotRef = aaUiSnapshotRef actors
  , cceUiActionsHandle = aaUiActionsHandle actors
  , cceScreenshotRef = aaScreenshotRef actors
  , cceLogSnapshotRef = Just (aaLogSnapshotRef actors)
  }
