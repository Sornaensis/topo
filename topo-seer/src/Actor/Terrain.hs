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
  , terrainActorDef
  , startTerrainGen
  ) where

import Actor.Log (LogEntry(..), LogLevel(..))
import Control.Concurrent (threadDelay)
import Control.Exception (evaluate)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
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
  , TerrainChunk
  , WeatherChunk
  , TerrainWorld(..)
  , defaultHexGridMeta
  , emptyWorldWithPlanet
  )
import Topo.Pipeline (PipelineConfig(..), runPipeline)
import Topo.WorldGen (WorldGenConfig(..), buildFullPipelineConfig)

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
  }

-- | Stage progress during terrain generation.
--   Indices are 1-based when known.
data TerrainGenProgress = TerrainGenProgress
  { tgpStageIndex :: !Int
  , tgpStageCount :: !Int
  , tgpStageName :: !Text
  , tgpStageElapsedMs :: !(Maybe Int)
  } deriving (Eq, Show)

-- | Final terrain generation result payload.
data TerrainGenResult = TerrainGenResult
  { tgrResultSeed :: !Word64
  , tgrResultChunkSize :: !Int
  , tgrResultTerrainChunks :: ![(ChunkId, TerrainChunk)]
  , tgrResultClimateChunks :: ![(ChunkId, ClimateChunk)]
  , tgrResultWeatherChunks :: ![(ChunkId, WeatherChunk)]
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
        world0 = emptyWorldWithPlanet worldCfg defaultHexGridMeta
                   (worldPlanet cfg) (worldSlice cfg)
        pipeline = buildFullPipelineConfig cfg worldCfg (tgrSeed req)
        stageCount = length (pipelineStages pipeline)
    stageRef <- newIORef 0
    stageStartRef <- newIORef Nothing
    let topoEnv = TopoEnv
          { teLogger = \msg -> do
              -- Yield between pipeline stages so the render thread's
              -- bound OS thread can re-acquire a GHC capability.
              -- Every safe FFI call in the render loop (all SDL
              -- bindings) temporarily releases the capability, and
              -- without this yield the monolithic pipeline can hold
              -- the terrain capability for the entire ~5s generation.
              threadDelay stageYieldUs
              case Text.stripPrefix (Text.pack "stage:start ") msg of
                Just stageName -> do
                  stageIndex <- bumpStageIndex stageRef
                  elapsed <- bumpStageStart stageStartRef
                  replyCast replyTo progressTag TerrainGenProgress
                    { tgpStageIndex = stageIndex
                    , tgpStageCount = stageCount
                    , tgpStageName = stageName
                    , tgpStageElapsedMs = elapsed
                    }
                Nothing -> pure ()
          }
    runResult <- runPipeline pipeline topoEnv world0
    genEnd <- getCurrentTime
    logFinalStageDuration replyTo stageStartRef
    let totalElapsed = diffToMs genEnd genStart
    replyCast replyTo logMessageTag (LogEntry LogInfo (Text.pack ("terrain: total generation took " <> show totalElapsed <> "ms")))
    case runResult of
      Left err -> do
        replyCast replyTo logMessageTag (LogEntry LogError (Text.pack ("terrain: pipeline failed: " <> show err)))
        pure st { tsRunning = False }
      Right (world1, _) -> do
        let terrainChunks = map (\(key, chunk) -> (ChunkId key, chunk)) (IntMap.toList (twTerrain world1))
            climateChunks = map (\(key, chunk) -> (ChunkId key, chunk)) (IntMap.toList (twClimate world1))
            weatherChunks = map (\(key, chunk) -> (ChunkId key, chunk)) (IntMap.toList (twWeather world1))
            terrainCount = IntMap.size (twTerrain world1)
            biomeCount = terrainCount
        -- Force the full list spines + tuple WHNF on the terrain actor's
        -- capability.  Without this, the lazy map/toList chains travel
        -- through three actor boundaries (Terrain → UiActions → Data →
        -- SnapshotReceiver → IORef → render thread) and the entire
        -- IntMap.toList + map cost is paid on the render thread (~4.8 s).
        _ <- evaluate (length terrainChunks)
        _ <- evaluate (length climateChunks)
        _ <- evaluate (length weatherChunks)
        let result = TerrainGenResult
              { tgrResultSeed = tgrSeed req
              , tgrResultChunkSize = wcChunkSize worldCfg
              , tgrResultTerrainChunks = terrainChunks
              , tgrResultClimateChunks = climateChunks
              , tgrResultWeatherChunks = weatherChunks
              , tgrResultTerrainCount = terrainCount
              , tgrResultBiomeCount = biomeCount
              }
        replyCast replyTo resultTag result
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

bumpStageIndex :: IORef Int -> IO Int
bumpStageIndex ref =
  atomicModifyIORef' ref (\n -> let n' = n + 1 in (n', n'))

bumpStageStart :: IORef (Maybe UTCTime) -> IO (Maybe Int)
bumpStageStart ref = do
  now <- getCurrentTime
  atomicModifyIORef' ref $ \prev ->
    let elapsed = fmap (diffToMs now) prev
    in (Just now, elapsed)

logFinalStageDuration :: ReplyTo TerrainReplyOps -> IORef (Maybe UTCTime) -> IO ()
logFinalStageDuration replyTo ref = do
  now <- getCurrentTime
  mbPrev <- readIORef ref
  case mbPrev of
    Nothing -> pure ()
    Just prev -> do
      let elapsed = diffToMs now prev
      replyCast replyTo logMessageTag (LogEntry LogInfo (Text.pack ("terrain: last stage took " <> show elapsed <> "ms")))

diffToMs :: UTCTime -> UTCTime -> Int
diffToMs now prev =
  round (realToFrac (diffUTCTime now prev) * (1000 :: Double))

-- | Microseconds to sleep between pipeline stage callbacks.
-- Gives the render loop's bound OS thread a chance to re-acquire a
-- GHC capability after safe FFI calls that release it (all SDL bindings).
-- 1 000 µs ≈ 1 ms; on Windows the actual sleep rounds up to ~ 1–2 ms.
stageYieldUs :: Int
stageYieldUs = 1000
