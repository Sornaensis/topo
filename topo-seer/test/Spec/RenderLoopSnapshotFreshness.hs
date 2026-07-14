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
import Data.List (isInfixOf)
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
  , SnapshotGatePhase(..)
  , SnapshotGateDiagnostic(..)
  , SnapshotAdvanceState(..)
  , SnapshotLagReasons(..)
  , formatSnapshotGateDiagnostic
  , initialSnapshotAdvanceState
  , initialSnapshotLagState
  , prepareRenderFreshnessDecision
  , recordRenderedSnapshot
  , snapshotAdvanceAgeMs
  , snapshotGateDiagnostic
  , snapshotLagTransition
  , retryPublicationRace
  )
import Seer.System.Cache (RenderCacheState(..), initialRenderCacheState)
import Seer.System.Snapshot
  ( SnapshotPollEnv(..)
  , SnapshotPollReasons(..)
  , SnapshotPollRequest(..)
  , SnapshotPollResult(..)
  , classifySnapshotPollReasons
  , noSnapshotPollRequest
  , pollRenderSnapshot
  , pollRenderSnapshotDetailed
  )
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

  it "classifies every poll reason without wall-clock sleeps" $ do
    let version = SnapshotVersion 4
        cached = Just version
        classify request published lastPolled hasCache intervalDue =
          classifySnapshotPollReasons request published lastPolled hasCache intervalDue
        bootstrap = classify noSnapshotPollRequest version Nothing False False
        changed = classify noSnapshotPollRequest version (Just (SnapshotVersion 3)) True False
        reused = classify noSnapshotPollRequest version cached True False
        interval = classify noSnapshotPollRequest version cached True True
        screenshot = classify
          (SnapshotPollRequest True False) version cached True False
        futureAtlas = classify
          (SnapshotPollRequest False True) version cached True False

    sprBootstrap bootstrap `shouldBe` True
    sprPublishedVersionAdvanced changed `shouldBe` True
    sprCacheReuse reused `shouldBe` True
    sprIntervalHealthReread interval `shouldBe` True
    sprForcedScreenshot screenshot `shouldBe` True
    sprFutureAtlasTarget futureAtlas `shouldBe` True
    map sprCacheReuse [bootstrap, changed, interval, screenshot, futureAtlas]
      `shouldBe` replicate 5 False

  it "returns probed, coherent, previous-polled, elapsed, and read fields" $ do
    (env, _) <- newFixture
    first <- pollRenderSnapshotDetailed env 100 noSnapshotPollRequest initialCache
    second <- pollRenderSnapshotDetailed env 101 noSnapshotPollRequest (spresCacheState first)

    spresProbedPublishedVersion first `shouldBe` SnapshotVersion 0
    spresCoherentVersion first `shouldBe` SnapshotVersion 0
    spresLastPolledVersion first `shouldBe` Nothing
    spresDidReadCoherent first `shouldBe` True
    spresLastPolledVersion second `shouldBe` Just (SnapshotVersion 0)
    spresDidReadCoherent second `shouldBe` False
    spresElapsedMs second `shouldBe` 0

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
    any (sprForcedScreenshot . spresReasons) (rfdSnapshotPollResults decision)
      `shouldBe` True
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
    any (sprFutureAtlasTarget . spresReasons) (rfdSnapshotPollResults decision)
      `shouldBe` True
    let gateDiagnostic = snapshotGateDiagnostic
          SnapshotGateWake
          (rfdFinalSnapshotPollResult decision)
          (Just (SnapshotVersion 0))
          False
          (rfdWakeInputs decision)
          False
          0
    sgdForcedGateSignalled gateDiagnostic `shouldBe` True
    sgdForcedGateSatisfied gateDiagnostic `shouldBe` True
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

  it "suppresses healthy-idle warnings regardless of snapshot age" $ do
    base <- basePollResult
    let version = SnapshotVersion 0
        advance = recordRenderedSnapshot 0 version (initialSnapshotAdvanceState 0)
        diagnostic = snapshotGateDiagnostic
          SnapshotGateSkip base (Just version) False idleWakeInputs False
          (snapshotAdvanceAgeMs 20000000000 advance)
        (state, warning) = snapshotLagTransition
          1000000000 5000000000 20000000000 diagnostic initialSnapshotLagState

    warning `shouldBe` Nothing
    state `shouldBe` initialSnapshotLagState

  it "warns for real publication lag and clears immediately on recovery" $ do
    base <- basePollResult
    let lagged = snapshotGateDiagnostic
          SnapshotGateSkip
          base
            { spresProbedPublishedVersion = SnapshotVersion 2
            , spresCoherentVersion = SnapshotVersion 1
            }
          (Just (SnapshotVersion 1))
          False idleWakeInputs False 1000
        recovered = lagged
          { sgdPolledVersion = SnapshotVersion 2
          , sgdRenderedVersion = Just (SnapshotVersion 2)
          }
        (started, firstWarning) = snapshotLagTransition
          100 500 1000 lagged initialSnapshotLagState
        (warned, warning) = snapshotLagTransition 100 500 1100 lagged started
        (cleared, recoveryWarning) = snapshotLagTransition 100 500 1101 recovered warned

    firstWarning `shouldBe` Nothing
    warning `shouldBe` Just (SnapshotLagReasons True True False)
    recoveryWarning `shouldBe` Nothing
    cleared `shouldBe` initialSnapshotLagState

  it "rate-limits a persistent forced-fresh gate warning" $ do
    base <- basePollResult
    let diagnostic = snapshotGateDiagnostic
          SnapshotGateSkip
          base { spresCoherentVersion = SnapshotVersion 0 }
          (Just (SnapshotVersion 0)) False futureWakeInputs False 0
        (started, _) = snapshotLagTransition 100 500 0 diagnostic initialSnapshotLagState
        (warned, firstWarning) = snapshotLagTransition 100 500 100 diagnostic started
        (limited, limitedWarning) = snapshotLagTransition 100 500 200 diagnostic warned
        (_, repeatedWarning) = snapshotLagTransition 100 500 600 diagnostic limited

    sgdForcedGateSignalled diagnostic `shouldBe` True
    sgdForcedGateSatisfied diagnostic `shouldBe` False
    firstWarning `shouldBe` Just (SnapshotLagReasons False False True)
    limitedWarning `shouldBe` Nothing
    repeatedWarning `shouldBe` Just (SnapshotLagReasons False False True)

  it "preserves threshold and rate-limit timing when lag reasons change" $ do
    base <- basePollResult
    let forcedOnly = snapshotGateDiagnostic
          SnapshotGateSkip base (Just (SnapshotVersion 0)) False futureWakeInputs False 0
        publicationAndForced = forcedOnly { sgdPublishedVersion = SnapshotVersion 1 }
        (started, _) = snapshotLagTransition 100 500 0 forcedOnly initialSnapshotLagState
        (changed, beforeThreshold) = snapshotLagTransition 100 500 50 publicationAndForced started
        (warned, warning) = snapshotLagTransition 100 500 100 publicationAndForced changed
        (changedAgain, limitedWarning) = snapshotLagTransition 100 500 200 forcedOnly warned
        (_, repeatedWarning) = snapshotLagTransition 100 500 600 forcedOnly changedAgain

    beforeThreshold `shouldBe` Nothing
    warning `shouldBe` Just (SnapshotLagReasons True True True)
    limitedWarning `shouldBe` Nothing
    repeatedWarning `shouldBe` Just (SnapshotLagReasons False False True)

  it "uses the coherent version as latest-known publication when it beats the probe" $ do
    base <- basePollResult
    let diagnostic = snapshotGateDiagnostic
          SnapshotGateWake
          base
            { spresProbedPublishedVersion = SnapshotVersion 0
            , spresCoherentVersion = SnapshotVersion 1
            }
          (Just (SnapshotVersion 0)) False idleWakeInputs False 0

    let (started, _) = snapshotLagTransition 0 1 0 diagnostic initialSnapshotLagState
        (_, warning) = snapshotLagTransition 0 1 0 diagnostic started
    sgdProbedPublishedVersion diagnostic `shouldBe` SnapshotVersion 0
    sgdPublishedVersion diagnostic `shouldBe` SnapshotVersion 1
    warning `shouldBe` Just (SnapshotLagReasons False True False)

  it "tracks maintenance rendering separately from frame-version advancement" $ do
    let initial = initialSnapshotAdvanceState 0
        renderedV1 = recordRenderedSnapshot 1000000 (SnapshotVersion 1) initial
        maintenanceV1 = recordRenderedSnapshot 9000000 (SnapshotVersion 1) renderedV1
        renderedV2 = recordRenderedSnapshot 10000000 (SnapshotVersion 2) maintenanceV1

    sasLastVersionAdvanceNs maintenanceV1 `shouldBe` 1000000
    snapshotAdvanceAgeMs 9000000 maintenanceV1 `shouldBe` 8
    sasLastVersionAdvanceNs renderedV2 `shouldBe` 10000000
    snapshotAdvanceAgeMs 10000000 renderedV2 `shouldBe` 0

  it "formats complete version, gate, and atlas queue diagnostics" $ do
    base <- basePollResult
    let diagnostic = snapshotGateDiagnostic
          SnapshotGateWake base (Just (SnapshotVersion 0)) True futureWakeInputs True 17
        formatted = formatSnapshotGateDiagnostic diagnostic
        requiredFields =
          [ "phase=wake"
          , "published=0"
          , "probed=0"
          , "polled=0"
          , "lastPolled=none"
          , "rendered=0"
          , "pollRead=True"
          , "pollReasons=bootstrap"
          , "screenshot=True"
          , "atlasPending=2"
          , "atlasQueued=1"
          , "queueRevision=3"
          , "queueVersionMin=1"
          , "queueVersionMax=1"
          , "generating=True"
          , "maintenance=True"
          , "forcedGate=True"
          , "forcedGateSatisfied=False"
          , "snapshotAdvanceAgeMs=17"
          ]

    map (`isInfixOf` formatted) requiredFields `shouldBe` replicate (length requiredFields) True

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
idleWake _ = pure idleWakeInputs

idleWakeInputs :: RenderWakeInputs
idleWakeInputs = RenderWakeInputs
  { rwiGenerating = False
  , rwiAtlasPendingCount = 0
  , rwiAtlasQueueState = emptyAtlasManagerQueueState
  }

futureWakeInputs :: RenderWakeInputs
futureWakeInputs = RenderWakeInputs
  { rwiGenerating = True
  , rwiAtlasPendingCount = 2
  , rwiAtlasQueueState = futureQueue
  }

basePollResult :: IO SnapshotPollResult
basePollResult = do
  (env, _) <- newFixture
  pollRenderSnapshotDetailed env 100 noSnapshotPollRequest initialCache

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
