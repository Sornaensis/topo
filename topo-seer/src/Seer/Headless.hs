{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Headless topo-seer runtime for service, HTTP, and integration tests.
--
-- This module starts the actor graph and command context without initialising
-- SDL, opening a window, creating a renderer, or entering the render loop. It
-- is intentionally small: later M1/M2/M3 work can build service and HTTP test
-- fixtures on top of 'withHeadlessApp' instead of duplicating actor setup in
-- individual specs.
module Seer.Headless
  ( HeadlessConfig(..)
  , defaultHeadlessConfig
  , HeadlessApp
  , headlessAppService
  , headlessAppServiceWithScreenshotWriter
  , deterministicHeadlessPng
  , headlessCommandContext
  , headlessServiceContext
  , headlessDispatchCommand
  , headlessRuntimeConfig
  , startHeadlessApp
  , stopHeadlessApp
  , withHeadlessApp
  ) where

import Actor.AtlasFreshness (AtlasFreshnessRef, newAtlasFreshnessRef)
import Actor.AtlasManager (AtlasManager, setAtlasManagerFreshnessRef)
import Actor.AtlasResultBroker (AtlasResultRef, newAtlasResultRef)
import Actor.AtlasScheduleBroker (AtlasScheduleRef, newAtlasScheduleRef)
import Actor.AtlasScheduler
  ( AtlasScheduler
  , AtlasSchedulerHandles(..)
  , atlasSchedulerConfigured
  , setAtlasSchedulerHandles
  )
import Actor.AtlasWorker (AtlasWorker, atlasWorkerActorDef, newAtlasWorkerLoadRef)
import Actor.Data (Data, DataSnapshot(..), TerrainSnapshot(..), defaultTerrainGeoContext)
import Actor.Log
  ( Log
  , LogSnapshotRef
  , newLogSnapshotRef
  , resetLogFile
  , setLogFileHandle
  , setLogSnapshotRef
  )
import Actor.PluginManager
  ( PluginManager
  , discoverPlugins
  , shutdownPlugins
  )
import Actor.Simulation
  ( Simulation
  , beginSimShutdown
  , setSimHandles
  , simulationHandlesConfigured
  )
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
  , newUiSnapshotRef
  , getUiSnapshot
  , setUiSeed
  , setUiSeedInput
  , setUiSnapshotRef
  )
import Actor.UiActions (UiActions)
import Actor.UiActions.Handles (ActorHandles, mkActorHandles)
import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Exception (bracket, onException)
import Control.Monad (replicateM, unless, when)
import Data.IORef (IORef, newIORef)
import qualified Data.Text as Text
import Data.Word (Word64)
import Hyperspace.Actor
  ( ActorHandle
  , ActorSystem
  , Protocol
  , get
  , newActorSystem
  , shutdownActorSystem
  , spawnActor
  )
import Seer.Command.Channel
  ( CommandChannelEnv(..)
  , dispatchCommandChannel
  , runCommandChannel
  )
import Seer.Command.Context (CommandContext(..), commandServiceContext)
import Seer.Config.Runtime (TopoSeerConfig(..), defaultConfig)
import Seer.Service.Context (ServiceContext(..))
import Seer.Service.Headless
  ( deterministicHeadlessPng
  , headlessAppService
  , headlessAppServiceWithScreenshotWriter
  )
import Seer.Service.Events (ServiceEventBus, newDefaultServiceEventBus)
import Seer.Editor.History (EditHistory, emptyHistory)
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
import System.IO (Handle, hClose)
import Topo.Command.Types (SeerCommand, SeerResponse)
import Topo.Overlay (emptyOverlayStore)

-- | Options for constructing a headless topo-seer runtime.
data HeadlessConfig = HeadlessConfig
  { hcRuntimeConfig :: !TopoSeerConfig
    -- ^ Runtime knobs used for worker counts and future headless service tests.
  , hcSeed :: !Word64
    -- ^ Deterministic seed installed in the UI actor at startup.
  , hcDiscoverPlugins :: !Bool
    -- ^ Whether to scan the user's plugin directory on startup. Disabled by
    -- default so CI tests do not depend on local plugin state.
  , hcStartCommandChannel :: !Bool
    -- ^ Whether to start the internal/test compatibility command IPC channel.
    -- Most tests can dispatch directly through 'headlessCommandContext' and
    -- should leave this off.
  , hcUseLogFile :: !Bool
    -- ^ Whether to create @~/.topo/LOG.txt@ and attach it to the log actor.
    -- Disabled by default to keep headless tests filesystem-light.
  } deriving (Eq, Show)

-- | Default headless runtime: deterministic, no SDL, no plugin discovery, no
-- command IPC, and no log file writes.
defaultHeadlessConfig :: HeadlessConfig
defaultHeadlessConfig = HeadlessConfig
  { hcRuntimeConfig = defaultConfig
  , hcSeed = 0
  , hcDiscoverPlugins = False
  , hcStartCommandChannel = False
  , hcUseLogFile = False
  }

-- | Live headless actor graph and reusable test/service context.
data HeadlessApp = HeadlessApp
  { haActorSystem :: !ActorSystem
  , haRuntimeConfig :: !TopoSeerConfig
  , haLogHandle :: !(ActorHandle Log (Protocol Log))
  , haLogSnapshotRef :: !LogSnapshotRef
  , haLogFileHandle :: !(Maybe Handle)
  , haUiHandle :: !(ActorHandle Ui (Protocol Ui))
  , haUiSnapshotRef :: !UiSnapshotRef
  , haDataHandle :: !(ActorHandle Data (Protocol Data))
  , haTerrainHandle :: !(ActorHandle Terrain (Protocol Terrain))
  , haAtlasManagerHandle :: !(ActorHandle AtlasManager (Protocol AtlasManager))
  , haAtlasWorkerHandles :: ![ActorHandle AtlasWorker (Protocol AtlasWorker)]
  , haAtlasSchedulerHandle :: !(ActorHandle AtlasScheduler (Protocol AtlasScheduler))
  , haAtlasResultRef :: !AtlasResultRef
  , haAtlasScheduleRef :: !AtlasScheduleRef
  , haAtlasFreshnessRef :: !AtlasFreshnessRef
  , haTerrainCacheRef :: !TerrainCacheRef
  , haTerrainCacheWorkerHandle :: !(ActorHandle TerrainCacheWorker (Protocol TerrainCacheWorker))
  , haUiActionsHandle :: !(ActorHandle UiActions (Protocol UiActions))
  , haPluginManagerHandle :: !(ActorHandle PluginManager (Protocol PluginManager))
  , haSimulationHandle :: !(ActorHandle Simulation (Protocol Simulation))
  , haDataSnapshotRef :: !DataSnapshotRef
  , haTerrainSnapshotRef :: !TerrainSnapshotRef
  , haSnapshotVersionRef :: !SnapshotVersionRef
  , haScreenshotRef :: !ScreenshotRequestRef
  , haHistoryRef :: !(IORef EditHistory)
  , haEventBus :: !ServiceEventBus
  , haActorHandles :: !ActorHandles
  , haCommandContext :: !CommandContext
  , haCommandChannelEnv :: !CommandChannelEnv
  , haCommandChannelThread :: !(Maybe ThreadId)
  , haAutoTickScheduler :: !AutoTickScheduler
  }

-- | Command context wired to the headless actor graph. Service and HTTP tests
-- can dispatch compatibility commands through this while AppService extraction
-- is still in progress.
headlessCommandContext :: HeadlessApp -> CommandContext
headlessCommandContext = haCommandContext

-- | Service context wired to the headless actor graph.
headlessServiceContext :: HeadlessApp -> ServiceContext
headlessServiceContext app = (commandServiceContext (haCommandContext app))
  { svcEventBus = Just (haEventBus app)
  }

-- | Dispatch a legacy envelope through the same service selected by the
-- optional headless command channel.
headlessDispatchCommand :: HeadlessApp -> SeerCommand -> IO SeerResponse
headlessDispatchCommand app = dispatchCommandChannel (haCommandChannelEnv app)

-- | Runtime configuration used to construct this headless runtime.
headlessRuntimeConfig :: HeadlessApp -> TopoSeerConfig
headlessRuntimeConfig = haRuntimeConfig

-- | Start the headless actor graph. The caller must eventually call
-- 'stopHeadlessApp' or use 'withHeadlessApp'.
startHeadlessApp :: HeadlessConfig -> IO HeadlessApp
startHeadlessApp cfg = do
  screenshotStoragePolicy <-
    initialiseScreenshotStorage (cfgScreenshotSaveDirectory (hcRuntimeConfig cfg))
  system <- newActorSystem
  startHeadlessAppWithSystem cfg screenshotStoragePolicy system
    `onException` shutdownActorSystem system

startHeadlessAppWithSystem
  :: HeadlessConfig
  -> ScreenshotStoragePolicy
  -> ActorSystem
  -> IO HeadlessApp
startHeadlessAppWithSystem cfg screenshotStoragePolicy system = do
  let runtimeCfg = hcRuntimeConfig cfg
  logHandle <- get @Log system
  logFileH <- if hcUseLogFile cfg
    then do
      h <- resetLogFile
      setLogFileHandle logHandle h
      pure (Just h)
    else pure Nothing
  logSnapshotRef <- newLogSnapshotRef
  setLogSnapshotRef logHandle logSnapshotRef

  uiHandle <- get @Ui system
  uiSnapshotRef <- newUiSnapshotRef
  setUiSnapshotRef uiHandle uiSnapshotRef
  setUiSeed uiHandle (hcSeed cfg)
  setUiSeedInput uiHandle (Text.pack (show (hcSeed cfg)))
  _ <- getUiSnapshot uiHandle

  dataHandle <- get @Data system
  terrainHandle <- get @Terrain system
  atlasManagerHandle <- get @AtlasManager system
  atlasWorkerHandles <- replicateM (cfgAtlasWorkerCount runtimeCfg) (spawnActor atlasWorkerActorDef)
  atlasWorkerNextRef <- newIORef (0 :: Int)
  atlasWorkerLoadRef <- newAtlasWorkerLoadRef
  atlasSchedulerHandle <- get @AtlasScheduler system
  atlasResultRef <- newAtlasResultRef
  atlasScheduleRef <- newAtlasScheduleRef
  atlasFreshnessRef <- newAtlasFreshnessRef
  setAtlasManagerFreshnessRef atlasManagerHandle atlasFreshnessRef
  setAtlasSchedulerHandles atlasSchedulerHandle AtlasSchedulerHandles
    { ashManager = atlasManagerHandle
    , ashWorkers = atlasWorkerHandles
    , ashWorkerNext = atlasWorkerNextRef
    , ashWorkerLoadRef = atlasWorkerLoadRef
    , ashResultRef = atlasResultRef
    , ashScheduleRef = atlasScheduleRef
    , ashFreshnessRef = atlasFreshnessRef
    }
  schedulerReady <- atlasSchedulerConfigured atlasSchedulerHandle
  unless schedulerReady (fail "topo-seer headless startup: atlas scheduler handles were not configured")
  terrainCacheWorkerHandle <- get @TerrainCacheWorker system
  terrainCacheRef <- newTerrainCacheRef
  uiActionsHandle <- get @UiActions system
  pluginManagerHandle <- get @PluginManager system
  when (hcDiscoverPlugins cfg) (discoverPlugins pluginManagerHandle)
  simulationHandle <- get @Simulation system

  dataSnapshotRef <- newDataSnapshotRef (DataSnapshot 0 0 Nothing)
  terrainSnapshotRef <- newTerrainSnapshotRef (TerrainSnapshot 0 0 0 0 0 0 mempty mempty mempty mempty mempty mempty mempty mempty mempty emptyOverlayStore defaultTerrainGeoContext)
  snapshotVersionRef <- newRenderSnapshotVersionRef
    uiSnapshotRef logSnapshotRef dataSnapshotRef terrainSnapshotRef
  setSimHandles simulationHandle dataHandle logHandle uiHandle dataSnapshotRef terrainSnapshotRef snapshotVersionRef atlasManagerHandle
  simReady <- simulationHandlesConfigured simulationHandle
  unless simReady (fail "topo-seer headless startup: simulation handles were not configured")
  autoTickScheduler <- startAutoTickScheduler AutoTickHandles
    { athUiHandle = uiHandle
    , athUiSnapshotRef = uiSnapshotRef
    , athSimulationHandle = simulationHandle
    , athLogHandle = logHandle
    , athSnapshotVersionRef = snapshotVersionRef
    }

  screenshotRef <- newScreenshotRequestRef
  historyRef <- newIORef (emptyHistory 50)
  eventBus <- newDefaultServiceEventBus
  let actorHandles = mkActorHandles uiHandle logHandle dataHandle terrainHandle atlasManagerHandle dataSnapshotRef terrainSnapshotRef snapshotVersionRef pluginManagerHandle simulationHandle historyRef
      commandContext = CommandContext
        { ccActorHandles = actorHandles
        , ccUiSnapshotRef = uiSnapshotRef
        , ccUiActionsHandle = uiActionsHandle
        , ccScreenshotRef = screenshotRef
        , ccScreenshotStoragePolicy = screenshotStoragePolicy
        , ccLogSnapshotRef = Just logSnapshotRef
        }
      commandEnv = CommandChannelEnv
        { cceAppService = headlessAppService
        , cceActorHandles = actorHandles
        , cceUiSnapshotRef = uiSnapshotRef
        , cceUiActionsHandle = uiActionsHandle
        , cceScreenshotRef = screenshotRef
        , cceScreenshotStoragePolicy = screenshotStoragePolicy
        , cceLogSnapshotRef = Just logSnapshotRef
        }
  commandThread <- if hcStartCommandChannel cfg
    then Just <$> forkIO (runCommandChannel commandEnv)
    else pure Nothing

  pure HeadlessApp
    { haActorSystem = system
    , haRuntimeConfig = runtimeCfg
    , haLogHandle = logHandle
    , haLogSnapshotRef = logSnapshotRef
    , haLogFileHandle = logFileH
    , haUiHandle = uiHandle
    , haUiSnapshotRef = uiSnapshotRef
    , haDataHandle = dataHandle
    , haTerrainHandle = terrainHandle
    , haAtlasManagerHandle = atlasManagerHandle
    , haAtlasWorkerHandles = atlasWorkerHandles
    , haAtlasSchedulerHandle = atlasSchedulerHandle
    , haAtlasResultRef = atlasResultRef
    , haAtlasScheduleRef = atlasScheduleRef
    , haAtlasFreshnessRef = atlasFreshnessRef
    , haTerrainCacheRef = terrainCacheRef
    , haTerrainCacheWorkerHandle = terrainCacheWorkerHandle
    , haUiActionsHandle = uiActionsHandle
    , haPluginManagerHandle = pluginManagerHandle
    , haSimulationHandle = simulationHandle
    , haDataSnapshotRef = dataSnapshotRef
    , haTerrainSnapshotRef = terrainSnapshotRef
    , haSnapshotVersionRef = snapshotVersionRef
    , haScreenshotRef = screenshotRef
    , haHistoryRef = historyRef
    , haEventBus = eventBus
    , haActorHandles = actorHandles
    , haCommandContext = commandContext
    , haCommandChannelEnv = commandEnv
    , haCommandChannelThread = commandThread
    , haAutoTickScheduler = autoTickScheduler
    }

-- | Stop a headless runtime and release actor resources.
stopHeadlessApp :: HeadlessApp -> IO ()
stopHeadlessApp app = do
  shutdownScreenshotRequestRef (haScreenshotRef app)
  waitForSimIdle <- beginSimShutdown (haSimulationHandle app)
  stopAutoTickScheduler (haAutoTickScheduler app)
  waitForSimIdle
  mapM_ killThread (haCommandChannelThread app)
  shutdownPlugins (haPluginManagerHandle app)
  shutdownActorSystem (haActorSystem app)
  mapM_ hClose (haLogFileHandle app)

-- | Bracketed headless runtime helper for specs and future HTTP/service tests.
withHeadlessApp :: HeadlessConfig -> (HeadlessApp -> IO a) -> IO a
withHeadlessApp cfg = bracket (startHeadlessApp cfg) stopHeadlessApp
