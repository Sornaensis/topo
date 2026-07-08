{-# LANGUAGE TypeApplications #-}

module Spec.AtlasManager (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Data.IORef (readIORef)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import Test.Hspec

import Actor.AtlasCache (AtlasKey(..), atlasKeyVersion)
import Actor.AtlasManager
  ( AtlasDispatchJob(..)
  , AtlasJob(..)
  , AtlasManager
  , AtlasManagerQueueState(..)
  , atlasManagerHasQueuedWorkFor
  , atlasManagerQueuedCount
  , atlasManagerQueuedRevision
  , atlasManagerQueuedState
  , drainAtlasJobs
  , drainFreshAtlasJobs
  , enqueueAtlasBuild
  , newAtlasManagerQueueRef
  , setAtlasManagerFreshnessRef
  , setAtlasManagerQueueRef
  )
import Actor.AtlasResult (AtlasBuildTarget(..))
import Actor.AtlasScheduler (AtlasFreshness(..), newAtlasFreshnessRef)
import Actor.Data (TerrainSnapshot(..))
import Actor.SnapshotReceiver (SnapshotVersion(..))
import Actor.UI (UiState(..), ViewMode(..), emptyUiState)
import Hyperspace.Actor
  ( ActorSystem
  , get
  , newActorSystem
  , shutdownActorSystem
  )
import Topo.Overlay (emptyOverlayStore)

withSystem :: (ActorSystem -> IO a) -> IO a
withSystem = bracket newActorSystem shutdownActorSystem

defaultWaterLevel :: Float
defaultWaterLevel = uiRenderWaterLevel emptyUiState

emptyTerrainSnapshotWithVersion :: Word64 -> TerrainSnapshot
emptyTerrainSnapshotWithVersion version =
  TerrainSnapshot version 0 0 0 0 0 mempty mempty mempty mempty mempty mempty mempty mempty mempty emptyOverlayStore

atlasJobFor :: ViewMode -> Word64 -> AtlasJob
atlasJobFor mode version =
  let terrainSnap = emptyTerrainSnapshotWithVersion version
      atlasKey = AtlasKey mode defaultWaterLevel version
  in AtlasJob
    { ajKey = atlasKey
    , ajViewMode = mode
    , ajWaterLevel = defaultWaterLevel
    , ajSnapshotVersion = SnapshotVersion version
    , ajTerrain = terrainSnap
    , ajHexRadius = 6
    , ajAtlasScale = 1
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
    revision2 `shouldSatisfy` (> revision1)

    enqueueVersion 1
    waitForManagerCasts
    revisionAfterStale <- atlasManagerQueuedRevision queueRef
    revisionAfterStale `shouldBe` revision2
    jobs <- drainAtlasJobs managerHandle
    length jobs `shouldBe` 1
    map (atlasKeyVersion . ajKey) jobs `shouldBe` [2]

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
