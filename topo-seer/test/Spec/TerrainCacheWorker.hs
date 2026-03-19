{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Spec.TerrainCacheWorker (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Hyperspace.Actor
  ( ActorSystem
  , getSingleton
  , newActorSystem
  , shutdownActorSystem
  )
import Test.Hspec
import Actor.Data (TerrainSnapshot(..))
import Topo.Overlay (emptyOverlayStore)
import Actor.TerrainCacheBroker
  ( TerrainCacheRef
  , newTerrainCacheRef
  , readTerrainCacheRef
  )
import Actor.TerrainCacheWorker
  ( TerrainCacheBuildRequest(..)
  , TerrainCacheBuildResult(..)
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
  it "sends cache results to the IORef" $ withSystem $ \system -> do
    workerHandle <- getSingleton system terrainCacheWorkerActorDef
    cacheRef <- newTerrainCacheRef
    let uiSnap = emptyUiState
        terrainSnap = TerrainSnapshot 0 sampleChunkSize sampleTerrainChunks sampleClimateChunks sampleWeatherChunks mempty mempty emptyOverlayStore
    case terrainCacheKeyFrom uiSnap terrainSnap of
      Nothing -> expectationFailure "Expected terrain cache key to be available"
      Just key -> do
        requestTerrainCacheBuild workerHandle TerrainCacheBuildRequest
          { tcrKey = key
          , tcrUi = uiSnap
          , tcrTerrain = terrainSnap
          , tcrResultRef = cacheRef
          }
        latest <- awaitLatestResult cacheRef
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
  :: TerrainCacheRef
  -> IO (Maybe TerrainCacheBuildResult)
awaitLatestResult cacheRef = go pollAttempts
  where
    go 0 = readTerrainCacheRef cacheRef
    go retries = do
      latest <- readTerrainCacheRef cacheRef
      case latest of
        Just _ -> pure latest
        Nothing -> do
          threadDelay pollDelayMicros
          go (retries - 1)
