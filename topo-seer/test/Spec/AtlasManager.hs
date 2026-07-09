{-# LANGUAGE TypeApplications #-}

module Spec.AtlasManager (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Data.Bits (shiftL)
import Data.IORef (readIORef)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import Test.Hspec

import Actor.AtlasCache (AtlasKey(..), atlasKeyDataVersion, atlasKeyForSelection, atlasKeySelectionTag, atlasKeyVersion, atlasKeyViewMode)
import Actor.AtlasManager
  ( AtlasDispatchJob(..)
  , AtlasFreshDrainRequest(..)
  , AtlasJob(..)
  , AtlasManager
  , AtlasManagerQueueState(..)
  , AtlasQueuedTarget(..)
  , atlasManagerHasQueuedWorkFor
  , atlasManagerQueuedCount
  , atlasManagerQueuedRevision
  , atlasManagerQueuedState
  , drainAtlasJobs
  , drainFreshAtlasJobs
  , drainFreshAtlasJobsLimited
  , enqueueAtlasBuild
  , newAtlasManagerQueueRef
  , setAtlasManagerFreshnessRef
  , setAtlasManagerQueueRef
  )
import Actor.AtlasResult (AtlasBuildTarget(..))
import Actor.AtlasScheduler (AtlasFreshness(..), newAtlasFreshnessRef)
import Actor.Data (TerrainSnapshot(..), defaultTerrainGeoContext)
import Actor.SnapshotReceiver (SnapshotVersion(..))
import Actor.UI (BaseViewMode(..), LayeredViewState(..), SkyOverlayMode(..), UiState(..), ViewMode(..), WeatherBasis(..), defaultLayeredViewState, emptyUiState, legacyViewModeToLayeredViewState)
import Hyperspace.Actor
  ( ActorSystem
  , get
  , newActorSystem
  , shutdownActorSystem
  )
import Seer.Render.Viewport (atlasViewportCoverageFromKeys)
import Topo.Overlay (emptyOverlayStore)

withSystem :: (ActorSystem -> IO a) -> IO a
withSystem = bracket newActorSystem shutdownActorSystem

defaultWaterLevel :: Float
defaultWaterLevel = uiRenderWaterLevel emptyUiState

emptyTerrainSnapshotWithVersion :: Word64 -> TerrainSnapshot
emptyTerrainSnapshotWithVersion version =
  TerrainSnapshot version 0 0 0 0 0 mempty mempty mempty mempty mempty mempty mempty mempty mempty emptyOverlayStore defaultTerrainGeoContext

atlasJobFor :: ViewMode -> Word64 -> AtlasJob
atlasJobFor mode version =
  let terrainSnap = emptyTerrainSnapshotWithVersion version
      atlasKey = AtlasKey mode defaultWaterLevel version
  in AtlasJob
    { ajKey = atlasKey
    , ajViewMode = mode
    , ajViewSelection = legacyViewModeToLayeredViewState mode
    , ajWaterLevel = defaultWaterLevel
    , ajSnapshotVersion = SnapshotVersion version
    , ajTerrain = terrainSnap
    , ajHexRadius = 6
    , ajAtlasScale = 1
    , ajViewportCoverage = Nothing
    }

atlasJobForSelection :: LayeredViewState -> Word64 -> AtlasJob
atlasJobForSelection selection version =
  let terrainSnap = emptyTerrainSnapshotWithVersion version
      atlasKey = atlasKeyForSelection selection defaultWaterLevel terrainSnap
      keyMode = atlasKeyViewMode atlasKey
  in AtlasJob
    { ajKey = atlasKey
    , ajViewMode = keyMode
    , ajViewSelection = selection
    , ajWaterLevel = defaultWaterLevel
    , ajSnapshotVersion = SnapshotVersion version
    , ajTerrain = terrainSnap
    , ajHexRadius = 6
    , ajAtlasScale = 1
    , ajViewportCoverage = Nothing
    }

freshnessFor :: AtlasKey -> SnapshotVersion -> AtlasFreshness
freshnessFor key snapshotVersion = AtlasFreshness
  { afKey = key
  , afSnapshotVersion = snapshotVersion
  , afLatestBuildIds = mempty
  }

targetFor :: AtlasJob -> AtlasBuildTarget
targetFor job = AtlasBuildTarget
  { abtKey = ajKey job
  , abtSnapshotVersion = ajSnapshotVersion job
  , abtHexRadius = ajHexRadius job
  , abtAtlasScale = ajAtlasScale job
  }

waitForManagerCasts :: IO ()
waitForManagerCasts = threadDelay 10000

spec :: Spec
spec = describe "AtlasManager" $ do
  it "treats high-bit legacy atlas versions as legacy comparable versions" $ do
    let version = (1 `shiftL` 63) + 99 :: Word64
        key = AtlasKey ViewCloudTypical defaultWaterLevel version
    atlasKeyDataVersion key `shouldBe` version
    atlasKeySelectionTag key `shouldBe` 0

  it "publishes non-destructive queued count and revision helpers" $ withSystem $ \system -> do
    managerHandle <- get @AtlasManager system
    queueRef <- newAtlasManagerQueueRef
    setAtlasManagerQueueRef managerHandle queueRef
    let job = atlasJobFor ViewElevation 1
    enqueueAtlasBuild managerHandle job
    waitForManagerCasts

    atlasManagerQueuedCount queueRef `shouldReturn` 1
    atlasManagerQueuedRevision queueRef `shouldReturn` 1
    atlasManagerHasQueuedWorkFor queueRef (ajKey job) `shouldReturn` True
    queueState <- atlasManagerQueuedState queueRef
    Map.lookup (ajKey job) (amqsQueuedByKey queueState) `shouldBe` Just 1
    fmap aqtViewMode (amqsQueuedTargets queueState) `shouldBe` [ViewElevation]
    fmap aqtKeyVersion (amqsQueuedTargets queueState) `shouldBe` [1]
    fmap aqtSnapshotVersion (amqsQueuedTargets queueState) `shouldBe` [SnapshotVersion 1]
    fmap aqtHexRadius (amqsQueuedTargets queueState) `shouldBe` [6]
    fmap aqtAtlasScale (amqsQueuedTargets queueState) `shouldBe` [1]
    fmap aqtCurrentStageVisible (amqsQueuedTargets queueState) `shouldBe` [False]
    amqsLatestAcceptedBuildId queueState `shouldSatisfy` maybe False (const True)

    enqueueAtlasBuild managerHandle job
    enqueueAtlasBuild managerHandle (atlasJobFor ViewElevation 0)
    waitForManagerCasts
    dropState <- atlasManagerQueuedState queueRef
    amqsDuplicateEnqueueDrops dropState `shouldBe` 1
    amqsStaleEnqueueDrops dropState `shouldBe` 1
    amqsLatestWinsPrunes dropState `shouldBe` 0
    atlasManagerQueuedCount queueRef `shouldReturn` 1

    jobs <- drainAtlasJobs managerHandle
    length jobs `shouldBe` 1

  it "updates queue revision for latest-wins replacements without growing pending count" $ withSystem $ \system -> do
    managerHandle <- get @AtlasManager system
    queueRef <- newAtlasManagerQueueRef
    setAtlasManagerQueueRef managerHandle queueRef
    let enqueueVersion version = enqueueAtlasBuild managerHandle (atlasJobFor ViewElevation version)
    enqueueVersion 1
    waitForManagerCasts
    revision1 <- atlasManagerQueuedRevision queueRef
    atlasManagerQueuedCount queueRef `shouldReturn` 1

    enqueueVersion 2
    waitForManagerCasts
    revision2 <- atlasManagerQueuedRevision queueRef
    atlasManagerQueuedCount queueRef `shouldReturn` 1
    replacementState <- atlasManagerQueuedState queueRef
    amqsLatestWinsPrunes replacementState `shouldBe` 1
    revision2 `shouldSatisfy` (> revision1)

    enqueueVersion 1
    waitForManagerCasts
    revisionAfterStale <- atlasManagerQueuedRevision queueRef
    revisionAfterStale `shouldBe` revision2
    jobs <- drainAtlasJobs managerHandle
    length jobs `shouldBe` 1
    map (atlasKeyVersion . ajKey) jobs `shouldBe` [2]

  it "coalesces duplicate same-coverage viewport refreshes without build-id churn" $ withSystem $ \system -> do
    managerHandle <- get @AtlasManager system
    queueRef <- newAtlasManagerQueueRef
    freshnessRef <- newAtlasFreshnessRef
    setAtlasManagerQueueRef managerHandle queueRef
    setAtlasManagerFreshnessRef managerHandle freshnessRef
    let coverage = Just (atlasViewportCoverageFromKeys [1, 2, 3])
        job1 = (atlasJobFor ViewElevation 1) { ajViewportCoverage = coverage }
        job2 = job1 { ajSnapshotVersion = SnapshotVersion 2 }
    enqueueAtlasBuild managerHandle job1
    waitForManagerCasts
    revision1 <- atlasManagerQueuedRevision queueRef
    latest1 <- readIORef freshnessRef
    let build1 = latest1 >>= \freshness -> Map.lookup (targetFor job1) (afLatestBuildIds freshness)
    atlasManagerQueuedCount queueRef `shouldReturn` 1

    enqueueAtlasBuild managerHandle job2
    waitForManagerCasts
    revision2 <- atlasManagerQueuedRevision queueRef
    latest2 <- readIORef freshnessRef
    let build2 = latest2 >>= \freshness -> Map.lookup (targetFor job1) (afLatestBuildIds freshness)
    revision2 `shouldBe` revision1
    atlasManagerQueuedCount queueRef `shouldReturn` 1
    build2 `shouldBe` build1

  it "does not coalesce newer same-coverage refreshes after the slot was dispatched" $ withSystem $ \system -> do
    managerHandle <- get @AtlasManager system
    queueRef <- newAtlasManagerQueueRef
    setAtlasManagerQueueRef managerHandle queueRef
    let coverage = Just (atlasViewportCoverageFromKeys [1, 2, 3])
        job1 = (atlasJobFor ViewElevation 1) { ajViewportCoverage = coverage }
        job2 = job1 { ajSnapshotVersion = SnapshotVersion 2 }
    enqueueAtlasBuild managerHandle job1
    waitForManagerCasts
    dispatched <- drainFreshAtlasJobs managerHandle (freshnessFor (ajKey job1) (SnapshotVersion 1))
    length dispatched `shouldBe` 1
    revisionAfterDispatch <- atlasManagerQueuedRevision queueRef

    enqueueAtlasBuild managerHandle job2
    waitForManagerCasts
    revisionAfterRefresh <- atlasManagerQueuedRevision queueRef
    revisionAfterRefresh `shouldSatisfy` (> revisionAfterDispatch)
    atlasManagerQueuedCount queueRef `shouldReturn` 1

  it "accepts same-snapshot viewport refreshes when required coverage changes" $ withSystem $ \system -> do
    managerHandle <- get @AtlasManager system
    queueRef <- newAtlasManagerQueueRef
    freshnessRef <- newAtlasFreshnessRef
    setAtlasManagerQueueRef managerHandle queueRef
    setAtlasManagerFreshnessRef managerHandle freshnessRef
    let job1 = (atlasJobFor ViewElevation 1) { ajViewportCoverage = Just (atlasViewportCoverageFromKeys [1]) }
        job2 = job1 { ajViewportCoverage = Just (atlasViewportCoverageFromKeys [2]) }
    enqueueAtlasBuild managerHandle job1
    waitForManagerCasts
    revision1 <- atlasManagerQueuedRevision queueRef
    latest1 <- readIORef freshnessRef
    let build1 = latest1 >>= \freshness -> Map.lookup (targetFor job1) (afLatestBuildIds freshness)

    enqueueAtlasBuild managerHandle job2
    waitForManagerCasts
    revision2 <- atlasManagerQueuedRevision queueRef
    latest2 <- readIORef freshnessRef
    let build2 = latest2 >>= \freshness -> Map.lookup (targetFor job2) (afLatestBuildIds freshness)
    atlasManagerQueuedCount queueRef `shouldReturn` 1
    revision2 `shouldSatisfy` (> revision1)
    build2 `shouldSatisfy` (/= build1)

  it "clears queued summary and advances revision when fresh jobs drain" $ withSystem $ \system -> do
    managerHandle <- get @AtlasManager system
    queueRef <- newAtlasManagerQueueRef
    setAtlasManagerQueueRef managerHandle queueRef
    let job = atlasJobFor ViewElevation 1
    enqueueAtlasBuild managerHandle job
    waitForManagerCasts
    revisionBeforeDrain <- atlasManagerQueuedRevision queueRef

    dispatchJobs <- drainFreshAtlasJobs managerHandle (freshnessFor (ajKey job) (SnapshotVersion 1))
    length dispatchJobs `shouldBe` 1
    atlasManagerQueuedCount queueRef `shouldReturn` 0
    atlasManagerHasQueuedWorkFor queueRef (ajKey job) `shouldReturn` False
    revisionAfterDrain <- atlasManagerQueuedRevision queueRef
    revisionAfterDrain `shouldSatisfy` (> revisionBeforeDrain)

  it "limited fresh drain dispatches the preferred target and leaves deferred work queued" $ withSystem $ \system -> do
    managerHandle <- get @AtlasManager system
    queueRef <- newAtlasManagerQueueRef
    setAtlasManagerQueueRef managerHandle queueRef
    let preferred = atlasJobFor ViewElevation 1
        backfill = preferred { ajHexRadius = 10, ajAtlasScale = 1 }
        dispatchCoverage = atlasViewportCoverageFromKeys [99]
    enqueueAtlasBuild managerHandle backfill
    enqueueAtlasBuild managerHandle preferred
    waitForManagerCasts

    dispatchJobs <- drainFreshAtlasJobsLimited managerHandle AtlasFreshDrainRequest
      { afdrFreshness = freshnessFor (ajKey preferred) (SnapshotVersion 1)
      , afdrLimit = 1
      , afdrPreferredTarget = Just (ajHexRadius preferred, ajAtlasScale preferred)
      , afdrDispatchCoverage = Just dispatchCoverage
      }
    map (\dispatchJob -> (ajHexRadius (adjJob dispatchJob), ajAtlasScale (adjJob dispatchJob))) dispatchJobs `shouldBe` [(6, 1)]
    map (ajViewportCoverage . adjJob) dispatchJobs `shouldBe` [Just dispatchCoverage]
    atlasManagerQueuedCount queueRef `shouldReturn` 1
    leftovers <- drainAtlasJobs managerHandle
    map (\job -> (ajHexRadius job, ajAtlasScale job)) leftovers `shouldBe` [(10, 1)]

  it "limited newer drain drops stale deferred backfill instead of preserving it" $ withSystem $ \system -> do
    managerHandle <- get @AtlasManager system
    let current = (atlasJobFor ViewElevation 1) { ajSnapshotVersion = SnapshotVersion 2 }
        staleBackfill = (atlasJobFor ViewElevation 1) { ajHexRadius = 10, ajAtlasScale = 1 }
    enqueueAtlasBuild managerHandle staleBackfill
    enqueueAtlasBuild managerHandle current

    dispatchJobs <- drainFreshAtlasJobsLimited managerHandle AtlasFreshDrainRequest
      { afdrFreshness = freshnessFor (ajKey current) (SnapshotVersion 2)
      , afdrLimit = 1
      , afdrPreferredTarget = Just (ajHexRadius current, ajAtlasScale current)
      , afdrDispatchCoverage = Nothing
      }
    map (\dispatchJob -> (ajSnapshotVersion (adjJob dispatchJob), ajHexRadius (adjJob dispatchJob))) dispatchJobs `shouldBe` [(SnapshotVersion 2, 6)]
    leftovers <- drainAtlasJobs managerHandle
    length leftovers `shouldBe` 0

  it "unrelated higher-version queued keys do not block current-key drains" $ withSystem $ \system -> do
    managerHandle <- get @AtlasManager system
    let current = atlasJobFor ViewElevation 1
        unrelatedWeather = atlasJobFor ViewWeather 50
    enqueueAtlasBuild managerHandle unrelatedWeather
    enqueueAtlasBuild managerHandle current

    dispatchJobs <- drainFreshAtlasJobs managerHandle (freshnessFor (ajKey current) (SnapshotVersion 1))
    map (ajViewMode . adjJob) dispatchJobs `shouldBe` [ViewElevation]
    leftovers <- drainAtlasJobs managerHandle
    length leftovers `shouldBe` 0

    enqueueAtlasBuild managerHandle unrelatedWeather
    weatherDispatch <- drainFreshAtlasJobs managerHandle (freshnessFor (ajKey unrelatedWeather) (SnapshotVersion 50))
    map (ajViewMode . adjJob) weatherDispatch `shouldBe` [ViewWeather]

  it "different layered compositions with the same base mode do not block current drains" $ withSystem $ \system -> do
    managerHandle <- get @AtlasManager system
    let selectionA = defaultLayeredViewState
          { lvsBaseView = BaseViewBiome
          , lvsSkyOverlay = Just SkyOverlayWeatherTemperature
          , lvsWeatherBasis = WeatherBasisCurrent
          , lvsOverlayOpacity = 0.25
          }
        selectionB = selectionA { lvsOverlayOpacity = 0.75 }
        unrelatedFuture = (atlasJobForSelection selectionA 50) { ajHexRadius = 10 }
        current = atlasJobForSelection selectionB 7
    enqueueAtlasBuild managerHandle unrelatedFuture
    enqueueAtlasBuild managerHandle current

    dispatchJobs <- drainFreshAtlasJobs managerHandle (freshnessFor (ajKey current) (SnapshotVersion 7))
    map (ajKey . adjJob) dispatchJobs `shouldBe` [ajKey current]
    leftovers <- drainAtlasJobs managerHandle
    length leftovers `shouldBe` 0

  it "restamps same-key queued jobs to the requested snapshot and publishes build freshness" $ withSystem $ \system -> do
    managerHandle <- get @AtlasManager system
    freshnessRef <- newAtlasFreshnessRef
    setAtlasManagerFreshnessRef managerHandle freshnessRef
    let job = atlasJobFor ViewElevation 1
        requestedVersion = SnapshotVersion 2
    enqueueAtlasBuild managerHandle job
    dispatchJobs <- drainFreshAtlasJobs managerHandle (freshnessFor (ajKey job) requestedVersion)
    case dispatchJobs of
      [dispatchJob] -> do
        let restampedJob = adjJob dispatchJob
            restampedTarget = targetFor restampedJob
        ajSnapshotVersion restampedJob `shouldBe` requestedVersion
        ajKey restampedJob `shouldBe` ajKey job
        latest <- readIORef freshnessRef
        latest `shouldSatisfy` maybe False (\freshness ->
          afKey freshness == ajKey job
            && afSnapshotVersion freshness == requestedVersion
            && Map.lookup restampedTarget (afLatestBuildIds freshness) == Just (adjBuildId dispatchJob))
      _ -> expectationFailure ("expected one dispatch job, got " <> show (length dispatchJobs))

  it "does not drain stale backfill when newer same-key work is queued" $ withSystem $ \system -> do
    managerHandle <- get @AtlasManager system
    let current = (atlasJobFor ViewElevation 1) { ajSnapshotVersion = SnapshotVersion 2 }
        staleBackfill = (atlasJobFor ViewElevation 1) { ajHexRadius = 10, ajAtlasScale = 1 }
    enqueueAtlasBuild managerHandle staleBackfill
    enqueueAtlasBuild managerHandle current
    staleDispatch <- drainFreshAtlasJobs managerHandle (freshnessFor (ajKey current) (SnapshotVersion 1))
    length staleDispatch `shouldBe` 0
    leftovers <- drainAtlasJobs managerHandle
    map (\job -> (ajSnapshotVersion job, ajHexRadius job, ajAtlasScale job)) leftovers `shouldBe` [(SnapshotVersion 2, 6, 1)]

  it "leaves newer same-key jobs queued when the scheduler request is stale" $ withSystem $ \system -> do
    managerHandle <- get @AtlasManager system
    let job = (atlasJobFor ViewElevation 1) { ajSnapshotVersion = SnapshotVersion 2 }
    enqueueAtlasBuild managerHandle job
    staleDispatch <- drainFreshAtlasJobs managerHandle (freshnessFor (ajKey job) (SnapshotVersion 1))
    length staleDispatch `shouldBe` 0
    currentDispatch <- drainFreshAtlasJobs managerHandle (freshnessFor (ajKey job) (SnapshotVersion 2))
    case currentDispatch of
      [dispatchJob] -> ajSnapshotVersion (adjJob dispatchJob) `shouldBe` SnapshotVersion 2
      _ -> expectationFailure ("expected queued newer job to survive stale drain, got " <> show (length currentDispatch))

  it "ignores delayed older same-slot enqueues after a restamped drain" $ withSystem $ \system -> do
    managerHandle <- get @AtlasManager system
    let job = atlasJobFor ViewElevation 1
    enqueueAtlasBuild managerHandle job
    dispatchJobs <- drainFreshAtlasJobs managerHandle (freshnessFor (ajKey job) (SnapshotVersion 2))
    length dispatchJobs `shouldBe` 1
    enqueueAtlasBuild managerHandle job
    leftovers <- drainAtlasJobs managerHandle
    length leftovers `shouldBe` 0
