module Spec.RenderLoopSnapshotFreshness (spec) where

import Actor.AtlasManager
  ( AtlasManagerQueueState(..)
  , AtlasQueuedTarget(..)
  , emptyAtlasManagerQueueState
  )
import Actor.AtlasResult (AtlasBuildId(..))
import Actor.Data (DataSnapshot(..), TerrainSnapshot(..), defaultTerrainGeoContext)
import Actor.Log (newLogSnapshotRef)
import Actor.Render (RenderSnapshot(..))
import Actor.SnapshotReceiver
  ( SnapshotVersion(..)
  , SnapshotVersionRef
  , newDataSnapshotRef
  , newRenderSnapshotVersionRef
  , newTerrainSnapshotRef
  , publishSnapshot
  , uiSnapshotUpdate
  )
import Actor.UI (UiState(..), ViewMode(..), emptyUiState, newUiSnapshotRef)
import Control.Monad (foldM, when)
import Data.IORef (atomicModifyIORef', newIORef)
import Data.Maybe (isJust)
import Data.Word (Word32)
import Seer.Screenshot.Request
  ( claimScreenshotRequest
  , newScreenshotRequestRef
  , submitScreenshotRequest
  )
import Seer.System
  ( RenderFreshnessDecision(..)
  , RenderWakeInputs(..)
  , prepareRenderFreshnessDecision
  , retryPublicationRace
  )
import Seer.System.Cache (RenderCacheState(..), initialRenderCacheState)
import Seer.System.Snapshot (SnapshotPollEnv(..), pollRenderSnapshot)
import Test.Hspec
import Topo.Overlay (emptyOverlayStore)

spec :: Spec
spec = describe "render-loop snapshot freshness" $ do
  it "refreshes a committed publication immediately before the fallback interval" $ do
    (env, versionRef) <- newFixture
    (_, _, cache0, _) <- pollRenderSnapshot env 100 False initialCache
    publishSnapshot versionRef (uiSnapshotUpdate (uiFor 1))
      `shouldReturn` SnapshotVersion 1

    (version, snapshot, cache1, _) <- pollRenderSnapshot env 101 False cache0

    version `shouldBe` SnapshotVersion 1
    uiSeed (rsUi snapshot) `shouldBe` 1
    rcsLastSnapshotPoll cache1 `shouldBe` Just 101

  it "reuses the cached coherent tuple when publication is unchanged" $ do
    (env, _) <- newFixture
    (version0, snapshot0, cache0, _) <- pollRenderSnapshot env 100 False initialCache

    (version1, snapshot1, cache1, elapsed) <- pollRenderSnapshot env 101 False cache0

    version1 `shouldBe` version0
    snapshot1 `shouldBe` snapshot0
    elapsed `shouldBe` 0
    rcsLastSnapshotPoll cache1 `shouldBe` Just 100

  it "observes an event-like invalidation through the production same-loop decision gates" $ do
    (env, versionRef) <- newFixture
    (_, eventContext, cache0, _) <- pollRenderSnapshot env 200 False initialCache
    uiSeed (rsUi eventContext) `shouldBe` 0

    _ <- publishSnapshot versionRef (uiSnapshotUpdate (uiFor 7))
    decision <- prepareRenderFreshnessDecision env 201 idleWake (pure Nothing) cache0

    rfdSnapshotVersion decision `shouldBe` SnapshotVersion 1
    uiSeed (rsUi (rfdRenderSnapshot decision)) `shouldBe` 7

  it "does not remain one generation behind repeated event-like publications" $ do
    (env, versionRef) <- newFixture
    (_, _, cache0, _) <- pollRenderSnapshot env 300 False initialCache

    _ <- foldM (publishAndObserve env versionRef) cache0 [1 .. 8]
    pure ()

  it "forces a coherent reread for screenshot capture even at the same version" $ do
    (env, _) <- newFixture
    (version0, snapshot0, cache0, _) <- pollRenderSnapshot env 400 False initialCache

    (version1, snapshot1, cache1, _) <- pollRenderSnapshot env 401 True cache0

    version1 `shouldBe` version0
    snapshot1 `shouldBe` snapshot0
    rcsLastSnapshotPoll cache1 `shouldBe` Just 401

  it "carries an exact mutation-then-screenshot claim through the production gates" $ do
    (env, versionRef) <- newFixture
    (_, _, cache0, _) <- pollRenderSnapshot env 450 False initialCache
    broker <- newScreenshotRequestRef
    submitted <- submitScreenshotRequest broker
    submitted `shouldSatisfy` either (const False) (const True)
    _ <- publishSnapshot versionRef (uiSnapshotUpdate (uiFor 12))

    decision <- prepareRenderFreshnessDecision
      env 451 idleWake (claimScreenshotRequest broker) cache0

    rfdSnapshotVersion decision `shouldBe` SnapshotVersion 1
    uiSeed (rsUi (rfdRenderSnapshot decision)) `shouldBe` 12
    rfdScreenshotClaim decision `shouldSatisfy` isJust
    rcsLastSnapshotPoll (rfdCacheState decision) `shouldBe` Just 451

  it "claims a request arriving between the final probe and decision, then rereads coherently" $ do
    (env, _) <- newFixture
    (_, _, cache0, _) <- pollRenderSnapshot env 460 False initialCache
    broker <- newScreenshotRequestRef
    wakeCount <- newIORef (0 :: Int)
    let submitDuringFinalWake snapshot = do
          call <- atomicModifyIORef' wakeCount (\count -> (count + 1, count))
          when (call == 1) $ do
            submitted <- submitScreenshotRequest broker
            submitted `shouldSatisfy` either (const False) (const True)
          idleWake snapshot

    decision <- prepareRenderFreshnessDecision
      env 461 submitDuringFinalWake (claimScreenshotRequest broker) cache0

    rfdScreenshotClaim decision `shouldSatisfy` isJust
    rcsLastSnapshotPoll (rfdCacheState decision) `shouldBe` Just 461

  it "refreshes a future atlas target even when the cached wake says generating" $ do
    (env, versionRef) <- newFixture
    (_, _, cache0, _) <- pollRenderSnapshot env 470 False initialCache
    wakeCount <- newIORef (0 :: Int)
    let readWake snapshot = do
          call <- atomicModifyIORef' wakeCount (\count -> (count + 1, count))
          when (call == 0) $ do
            _ <- publishSnapshot versionRef (uiSnapshotUpdate (uiFor 13))
            pure ()
          pure RenderWakeInputs
            { rwiGenerating = call == 0
            , rwiAtlasPendingCount = 0
            , rwiAtlasQueueState = futureQueue
            }

    decision <- prepareRenderFreshnessDecision env 471 readWake (pure Nothing) cache0

    rfdSnapshotVersion decision `shouldBe` SnapshotVersion 1
    uiSeed (rsUi (rfdRenderSnapshot decision)) `shouldBe` 13
    rwiGenerating (rfdWakeInputs decision) `shouldBe` False
    rcsLastAtlasSnapshotPollRevision (rfdCacheState decision) `shouldBe` Just 3

  it "proves maintenance wake inputs against the current published snapshot" $ do
    (env, versionRef) <- newFixture
    (_, _, cache0, _) <- pollRenderSnapshot env 480 False initialCache
    _ <- publishSnapshot versionRef (uiSnapshotUpdate (uiFor 14))
    let pendingWake _ = pure RenderWakeInputs
          { rwiGenerating = False
          , rwiAtlasPendingCount = 1
          , rwiAtlasQueueState = emptyAtlasManagerQueueState
          }

    decision <- prepareRenderFreshnessDecision env 481 pendingWake (pure Nothing) cache0

    rfdSnapshotVersion decision `shouldBe` SnapshotVersion 1
    uiSeed (rsUi (rfdRenderSnapshot decision)) `shouldBe` 14
    rwiAtlasPendingCount (rfdWakeInputs decision) `shouldBe` 1
    rcsLastPolledSnapshot (rfdCacheState decision) `shouldBe` Just (SnapshotVersion 1)

  it "retries conditional event publication beyond the obsolete fixed limit" $ do
    attempts <- newIORef (0 :: Int)
    result <- retryPublicationRace $ do
      attempt <- atomicModifyIORef' attempts (\count -> (count + 1, count))
      pure $ if attempt < 5 then Left () else Right ("committed" :: String)

    result `shouldBe` "committed"

  it "retains the interval as a coherent health reread" $ do
    (env, _) <- newFixture
    (_, _, cache0, _) <- pollRenderSnapshot env 500 False initialCache

    (_, _, cache1, _) <- pollRenderSnapshot env 1500 False cache0

    rcsLastSnapshotPoll cache1 `shouldBe` Just 1500

publishAndObserve
  :: SnapshotPollEnv
  -> SnapshotVersionRef
  -> RenderCacheState
  -> Int
  -> IO RenderCacheState
publishAndObserve env versionRef cache marker = do
  published <- publishSnapshot versionRef (uiSnapshotUpdate (uiFor marker))
  let nowMs = 300 + fromIntegral marker :: Word32
  decision <- prepareRenderFreshnessDecision env nowMs idleWake (pure Nothing) cache
  rfdSnapshotVersion decision `shouldBe` published
  uiSeed (rsUi (rfdRenderSnapshot decision)) `shouldBe` fromIntegral marker
  pure (rfdCacheState decision)

idleWake :: RenderSnapshot -> IO RenderWakeInputs
idleWake _ = pure RenderWakeInputs
  { rwiGenerating = False
  , rwiAtlasPendingCount = 0
  , rwiAtlasQueueState = emptyAtlasManagerQueueState
  }

futureQueue :: AtlasManagerQueueState
futureQueue = emptyAtlasManagerQueueState
  { amqsQueuedCount = 1
  , amqsQueuedRevision = 3
  , amqsQueuedTargets =
      [ AtlasQueuedTarget
          { aqtBuildId = AtlasBuildId 3
          , aqtViewMode = ViewWeather
          , aqtWaterLevel = 0
          , aqtKeyVersion = 1
          , aqtSnapshotVersion = SnapshotVersion 1
          , aqtHexRadius = 6
          , aqtAtlasScale = 1
          , aqtCurrentStageVisible = True
          }
      ]
  }

newFixture :: IO (SnapshotPollEnv, SnapshotVersionRef)
newFixture = do
  uiRef <- newUiSnapshotRef
  logRef <- newLogSnapshotRef
  dataRef <- newDataSnapshotRef (DataSnapshot 0 0 Nothing)
  terrainRef <- newTerrainSnapshotRef initialTerrain
  versionRef <- newRenderSnapshotVersionRef uiRef logRef dataRef terrainRef
  pure
    ( SnapshotPollEnv
        { speTimingLogThresholdMs = maxBound
        , speSnapshotPollMs = 1000
        , speSnapshotVersionRef = versionRef
        , speLogSlowSnapshotPoll = const (pure ())
        }
    , versionRef
    )

initialCache :: RenderCacheState
initialCache = initialRenderCacheState 4

uiFor :: Int -> UiState
uiFor marker = emptyUiState { uiSeed = fromIntegral marker }

initialTerrain :: TerrainSnapshot
initialTerrain = TerrainSnapshot
  0 0 0 0 0 0
  mempty mempty mempty mempty mempty mempty mempty mempty mempty
  emptyOverlayStore
  defaultTerrainGeoContext
