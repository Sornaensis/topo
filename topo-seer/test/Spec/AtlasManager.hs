{-# LANGUAGE TypeApplications #-}

module Spec.AtlasManager (spec) where

import Control.Exception (bracket)
import Data.IORef (readIORef)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import Test.Hspec

import Actor.AtlasCache (AtlasKey(..))
import Actor.AtlasManager
  ( AtlasDispatchJob(..)
  , AtlasJob(..)
  , AtlasManager
  , drainAtlasJobs
  , drainFreshAtlasJobs
  , enqueueAtlasBuild
  , setAtlasManagerFreshnessRef
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

spec :: Spec
spec = describe "AtlasManager" $ do
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
