{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Spec.TerrainCacheWorker (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Hyperspace.Actor
  ( ActorHandle
  , ActorSystem
  , Protocol
  , getSingleton
  , newActorSystem
  , replyTo
  , shutdownActorSystem
  )
import Test.Hspec
import Actor.Data (TerrainSnapshot(..))
import Actor.TerrainCacheBroker
  ( TerrainCacheBroker
  , getLatestTerrainCacheResult
  , terrainCacheBrokerActorDef
  )
import Actor.TerrainCacheWorker
  ( TerrainCacheBuildRequest(..)
  , TerrainCacheBuildResult(..)
  , TerrainCacheResultReply
  , requestTerrainCacheBuild
  , terrainCacheKeyFrom
  , terrainCacheWorkerActorDef
  )
import Actor.UI (emptyUiState)
import Seer.Render.Terrain (TerrainCache(..))
import Topo (ClimateChunk, TerrainChunk, WeatherChunk)

withSystem :: (ActorSystem -> IO a) -> IO a
withSystem = bracket newActorSystem shutdownActorSystem

spec :: Spec
spec = describe "TerrainCacheWorker" $ do
  it "sends cache results to the broker" $ withSystem $ \system -> do
    workerHandle <- getSingleton system terrainCacheWorkerActorDef
    brokerHandle <- getSingleton system terrainCacheBrokerActorDef
    let uiSnap = emptyUiState
        terrainSnap = TerrainSnapshot 0 sampleChunkSize sampleTerrainChunks sampleClimateChunks sampleWeatherChunks mempty
    case terrainCacheKeyFrom uiSnap terrainSnap of
      Nothing -> expectationFailure "Expected terrain cache key to be available"
      Just key -> do
        requestTerrainCacheBuild workerHandle TerrainCacheBuildRequest
          { tcrKey = key
          , tcrUi = uiSnap
          , tcrTerrain = terrainSnap
          , tcrReplyTo = replyTo @TerrainCacheResultReply brokerHandle
          }
        latest <- awaitLatestResult brokerHandle
        case latest of
          Nothing -> expectationFailure "Expected a terrain cache build result"
          Just result -> do
            tcrResultKey result `shouldBe` key
            let cache = tcrResultCache result
            tcChunkSize cache `shouldBe` sampleChunkSize
            tcTerrainChunks cache `shouldBe` sampleTerrainChunks
            tcClimateChunks cache `shouldBe` sampleClimateChunks
            tcWeatherChunks cache `shouldBe` sampleWeatherChunks

sampleChunkSize :: Int
sampleChunkSize = 1

sampleTerrainChunks :: IntMap TerrainChunk
sampleTerrainChunks = IntMap.empty

sampleClimateChunks :: IntMap ClimateChunk
sampleClimateChunks = IntMap.empty

sampleWeatherChunks :: IntMap WeatherChunk
sampleWeatherChunks = IntMap.empty

pollAttempts :: Int
pollAttempts = 25

pollDelayMicros :: Int
pollDelayMicros = 1000

awaitLatestResult
  :: ActorHandle TerrainCacheBroker (Protocol TerrainCacheBroker)
  -> IO (Maybe TerrainCacheBuildResult)
awaitLatestResult brokerHandle = go pollAttempts
  where
    go 0 = getLatestTerrainCacheResult brokerHandle
    go retries = do
      latest <- getLatestTerrainCacheResult brokerHandle
      case latest of
        Just _ -> pure latest
        Nothing -> do
          threadDelay pollDelayMicros
          go (retries - 1)
