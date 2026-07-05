{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Terrain generation actor and reply payloads.

module Actor.Terrain
  ( Terrain
  , TerrainGenRequest(..)
  , TerrainGenProgress(..)
  , TerrainGenResult(..)
  , TerrainReplyOps
  , prepareGeneratedWorldForSimulation
  , terrainActorDef
  , startTerrainGen
  ) where

import Actor.Log (LogEntry(..), LogLevel(..))
import Control.Concurrent (threadDelay)
import Control.Exception (evaluate)
import Data.Aeson (toJSON)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Data.Word (Word64)
import Hyperspace.Actor
import Hyperspace.Actor.QQ (hyperspace)
import Hyperspace.Actor.Spec (OpTag(..))
import qualified Data.IntMap.Strict as IntMap
import Topo
  ( TopoEnv(..)
  , WorldConfig(..)
  , ChunkId(..)
  , ClimateChunk
  , GlacierChunk
  , GroundwaterChunk
  , OverlaySchema
  , RiverChunk
  , TerrainChunk
  , VegetationChunk
  , VolcanismChunk
  , WaterBodyChunk
  , WeatherChunk
  , TerrainWorld(..)
  , emptyOverlay
  , emptyWorldWithPlanet
  , getWeatherFromOverlay
  , insertOverlay
  )
import Actor.PluginManager.PipelineIntegrator (PluginPipelineInput, integratePluginStages)
import Topo.Pipeline (PipelineConfig(..), StageProgress(..), StageStatus(..), runPipeline)
import Topo.Pipeline.Stage (StageId)
import Topo.Overlay (OverlayStore)
import Topo.WorldGen (WorldGenConfig(..), buildFullPipelineConfig)
import Actor.Simulation (Simulation, SimulationNodeBinding, normalizeWorldSchedulesForBindings, setSimWorldWithNodes)

progressTag :: OpTag "progress"
progressTag = OpTag

resultTag :: OpTag "result"
resultTag = OpTag

logMessageTag :: OpTag "logMessage"
logMessageTag = OpTag

-- | Terrain generation request payload.
data TerrainGenRequest = TerrainGenRequest
  { tgrSeed :: !Word64
  , tgrWorldConfig :: !WorldConfig
  , tgrGenConfig :: !WorldGenConfig
  , tgrDisabledStages :: !(Set StageId)
    -- ^ Pipeline stages the user has disabled via the Pipeline tab.
  , tgrPluginPipeline :: !PluginPipelineInput
    -- ^ Loaded plugin manifests/connections and user ordering for generator-stage integration.
  , tgrOverlaySchemas :: ![OverlaySchema]
    -- ^ Plugin overlay schemas pre-registered in generated worlds.
  , tgrSimHandle :: !(ActorHandle Simulation (Protocol Simulation))
    -- ^ Simulation actor handle for posting the generated world.
  , tgrSimNodes :: ![SimulationNodeBinding]
    -- ^ Executable plugin simulation nodes to bind with the generated world.
  }

-- | Stage progress during terrain generation.
--   Indices are 1-based when known.
data TerrainGenProgress = TerrainGenProgress
  { tgpStageIndex :: !Int
  , tgpStageCount :: !Int
  , tgpStageId :: !StageId
  , tgpStageName :: !Text
  , tgpStageStatus :: !StageStatus
  , tgpStageElapsedMs :: !(Maybe Int)
  , tgpStageDetail :: !(Maybe Text)
  } deriving (Eq, Show)

-- | Final terrain generation result payload.
data TerrainGenResult = TerrainGenResult
  { tgrResultSeed :: !Word64
  , tgrResultChunkSize :: !Int
  , tgrResultTerrainChunks :: ![(ChunkId, TerrainChunk)]
  , tgrResultClimateChunks :: ![(ChunkId, ClimateChunk)]
  , tgrResultWeatherChunks :: ![(ChunkId, WeatherChunk)]
  , tgrResultRiverChunks :: ![(ChunkId, RiverChunk)]
  , tgrResultGroundwaterChunks :: ![(ChunkId, GroundwaterChunk)]
  , tgrResultVolcanismChunks :: ![(ChunkId, VolcanismChunk)]
  , tgrResultGlacierChunks :: ![(ChunkId, GlacierChunk)]
  , tgrResultWaterBodyChunks :: ![(ChunkId, WaterBodyChunk)]
  , tgrResultVegetationChunks :: ![(ChunkId, VegetationChunk)]
  , tgrResultOverlayStore :: !OverlayStore
  , tgrResultTerrainCount :: !Int
  , tgrResultBiomeCount :: !Int
  } deriving (Eq, Show)

data TerrainState = TerrainState
  { tsLastSeed :: !(Maybe Word64)
  , tsRunning :: !Bool
  } deriving (Eq, Show)

emptyTerrainState :: TerrainState
emptyTerrainState = TerrainState
  { tsLastSeed = Nothing
  , tsRunning = False
  }

[hyperspace|
replyprotocol TerrainReplyOps =
  cast progress :: TerrainGenProgress
  cast result :: TerrainGenResult
  cast logMessage :: LogEntry

actor Terrain
  state TerrainState
  lifetime Singleton
  schedule pinned 2 sticky
  noDeps
  mailbox Unbounded

  cast start :: TerrainGenRequest reply TerrainReplyOps

  initial emptyTerrainState
  onReply start = \req replyTo st -> do
    replyCast replyTo logMessageTag (LogEntry LogInfo (Text.pack "terrain: start generation"))
    genStart <- getCurrentTime
    let cfg = tgrGenConfig req
        worldCfg = tgrWorldConfig req
        baseWorld = emptyWorldWithPlanet worldCfg (worldHexGrid cfg)
                   (worldPlanet cfg) (worldSlice cfg)
        world0 = registerPluginOverlays (tgrOverlaySchemas req) baseWorld
        pipeline0 = buildFullPipelineConfig cfg worldCfg (tgrSeed req)
        pipelineWithPlugins = integratePluginStages (tgrPluginPipeline req) pipeline0
        pipelineWithoutProgress = pipelineWithPlugins
          { pipelineDisabled = tgrDisabledStages req
          }
    stageStartRef <- newIORef Nothing
    let pipeline = pipelineWithoutProgress
          { pipelineOnProgress = publishPipelineProgress replyTo stageStartRef
          }
        topoEnv = TopoEnv
          { teLogger = \_msg -> do
              -- Yield between pipeline stages so the render thread's
              -- bound OS thread can re-acquire a GHC capability.
              -- Every safe FFI call in the render loop (all SDL
              -- bindings) temporarily releases the capability, and
              -- without this yield the monolithic pipeline can hold
              -- the terrain capability for the entire ~5s generation.
              threadDelay stageYieldUs
          }
    runResult <- runPipeline pipeline topoEnv world0
    genEnd <- getCurrentTime
    let totalElapsed = diffToMs genEnd genStart
    replyCast replyTo logMessageTag (LogEntry LogInfo (Text.pack ("terrain: total generation took " <> show totalElapsed <> "ms")))
    case runResult of
      Left err -> do
        replyCast replyTo logMessageTag (LogEntry LogError (Text.pack ("terrain: pipeline failed: " <> show err)))
        pure st { tsRunning = False }
      Right (world1, _) -> do
        let scheduledWorld = prepareGeneratedWorldForSimulation cfg (tgrSimNodes req) world1
            terrainChunks = map (\(key, chunk) -> (ChunkId key, chunk)) (IntMap.toList (twTerrain scheduledWorld))
            climateChunks = map (\(key, chunk) -> (ChunkId key, chunk)) (IntMap.toList (twClimate scheduledWorld))
            weatherChunks = map (\(key, chunk) -> (ChunkId key, chunk)) (IntMap.toList (getWeatherFromOverlay scheduledWorld))
            riverChunks   = map (\(key, chunk) -> (ChunkId key, chunk)) (IntMap.toList (twRivers scheduledWorld))
            groundwaterChunks = map (\(key, chunk) -> (ChunkId key, chunk)) (IntMap.toList (twGroundwater scheduledWorld))
            volcanismChunks = map (\(key, chunk) -> (ChunkId key, chunk)) (IntMap.toList (twVolcanism scheduledWorld))
            glacierChunks = map (\(key, chunk) -> (ChunkId key, chunk)) (IntMap.toList (twGlaciers scheduledWorld))
            waterBodyChunks = map (\(key, chunk) -> (ChunkId key, chunk)) (IntMap.toList (twWaterBodies scheduledWorld))
            vegetationChunks = map (\(key, chunk) -> (ChunkId key, chunk)) (IntMap.toList (twVegetation scheduledWorld))
            overlayStore = twOverlays scheduledWorld
            terrainCount = IntMap.size (twTerrain scheduledWorld)
            biomeCount = terrainCount
        -- Force the full list spines + tuple WHNF on the terrain actor's
        -- capability.  Without this, the lazy map/toList chains travel
        -- through three actor boundaries (Terrain → UiActions → Data →
        -- SnapshotReceiver → IORef → render thread) and the entire
        -- IntMap.toList + map cost is paid on the render thread (~4.8 s).
        _ <- evaluate (length terrainChunks)
        _ <- evaluate (length climateChunks)
        _ <- evaluate (length weatherChunks)
        _ <- evaluate (length riverChunks)
        _ <- evaluate (length groundwaterChunks)
        _ <- evaluate (length volcanismChunks)
        _ <- evaluate (length glacierChunks)
        _ <- evaluate (length waterBodyChunks)
        _ <- evaluate (length vegetationChunks)
        let result = TerrainGenResult
              { tgrResultSeed = tgrSeed req
              , tgrResultChunkSize = wcChunkSize worldCfg
              , tgrResultTerrainChunks = terrainChunks
              , tgrResultClimateChunks = climateChunks
              , tgrResultWeatherChunks = weatherChunks
              , tgrResultRiverChunks = riverChunks
              , tgrResultGroundwaterChunks = groundwaterChunks
              , tgrResultVolcanismChunks = volcanismChunks
              , tgrResultGlacierChunks = glacierChunks
              , tgrResultWaterBodyChunks = waterBodyChunks
              , tgrResultVegetationChunks = vegetationChunks
              , tgrResultOverlayStore = overlayStore
              , tgrResultTerrainCount = terrainCount
              , tgrResultBiomeCount = biomeCount
              }
        replyCast replyTo resultTag result
        -- Send the full world to the Simulation actor so it can
        -- run tick-based overlay simulation on demand.
        setSimWorldWithNodes (tgrSimHandle req) scheduledWorld (tgrSimNodes req)
        pure st { tsLastSeed = Just (tgrSeed req), tsRunning = False }
|]

-- | Start an asynchronous terrain generation run.
startTerrainGen
  :: ActorHandle Terrain (Protocol Terrain)
  -> ReplyTo TerrainReplyOps
  -> TerrainGenRequest
  -> IO ()
startTerrainGen handle replyTo req =
  castReply @"start" handle replyTo #start req

publishPipelineProgress :: ReplyTo TerrainReplyOps -> IORef (Maybe UTCTime) -> StageProgress -> IO ()
publishPipelineProgress replyTo stageStartRef progress = do
  now <- getCurrentTime
  elapsed <- case spStatus progress of
    StageStarted -> writeIORef stageStartRef (Just now) >> pure Nothing
    StageRunning -> peekStageStart now stageStartRef
    StageCompleted -> consumeStageStart now stageStartRef
    StageSkipped -> pure Nothing
  replyCast replyTo progressTag TerrainGenProgress
    { tgpStageIndex = spStageIndex progress + 1
    , tgpStageCount = spStageCount progress
    , tgpStageId = spStageId progress
    , tgpStageName = spStageName progress
    , tgpStageStatus = spStatus progress
    , tgpStageElapsedMs = elapsed
    , tgpStageDetail = spDetail progress
    }

peekStageStart :: UTCTime -> IORef (Maybe UTCTime) -> IO (Maybe Int)
peekStageStart now ref = do
  mbPrev <- readIORef ref
  pure (fmap (diffToMs now) mbPrev)

consumeStageStart :: UTCTime -> IORef (Maybe UTCTime) -> IO (Maybe Int)
consumeStageStart now ref = do
  mbPrev <- readIORef ref
  writeIORef ref Nothing
  pure (fmap (diffToMs now) mbPrev)

diffToMs :: UTCTime -> UTCTime -> Int
diffToMs now prev =
  round (realToFrac (diffUTCTime now prev) * (1000 :: Double))

-- | Microseconds to sleep between pipeline stage callbacks.
-- Gives the render loop's bound OS thread a chance to re-acquire a
-- GHC capability after safe FFI calls that release it (all SDL bindings).
-- 1 000 µs ≈ 1 ms; on Windows the actual sleep rounds up to ~ 1–2 ms.
stageYieldUs :: Int
stageYieldUs = 1000

-- | Attach the request's generation config before deriving built-in simulation bindings.
prepareGeneratedWorldForSimulation :: WorldGenConfig -> [SimulationNodeBinding] -> TerrainWorld -> TerrainWorld
prepareGeneratedWorldForSimulation cfg simNodes world =
  normalizeWorldSchedulesForBindings
    (world { twGenConfig = Just (toJSON cfg) })
    simNodes

registerPluginOverlays :: [OverlaySchema] -> TerrainWorld -> TerrainWorld
registerPluginOverlays schemas world =
  world
    { twOverlays = foldr (insertOverlay . emptyOverlay) (twOverlays world) schemas
    }
