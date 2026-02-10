{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Spec.SnapshotReceiver (spec) where

import Control.Exception (bracket)
import Hyperspace.Actor (ActorSystem, cast, getSingleton, newActorSystem, shutdownActorSystem)
import Test.Hspec
import Actor.Data (DataSnapshot(..), TerrainSnapshot(..))
import Actor.Log (LogLevel(..), LogSnapshot(..))
import Actor.Render (RenderSnapshot(..))
import Actor.SnapshotReceiver (SnapshotVersion(..), getSnapshot, snapshotReceiverActorDef)
import Actor.UI (emptyUiState)

withSystem :: (ActorSystem -> IO a) -> IO a
withSystem = bracket newActorSystem shutdownActorSystem

spec :: Spec
spec = describe "SnapshotReceiver" $ do
  it "starts with the empty snapshot" $ withSystem $ \system -> do
    handle <- getSingleton system snapshotReceiverActorDef
    (version, snapshot) <- getSnapshot handle
    version `shouldBe` SnapshotVersion 0
    snapshot `shouldBe` emptyRenderSnapshot

  it "increments the version when snapshots update" $ withSystem $ \system -> do
    handle <- getSingleton system snapshotReceiverActorDef
    let logSnap = LogSnapshot [] False 1 LogInfo
    cast @"logSnapshot" handle #logSnapshot logSnap
    (version1, snapshot1) <- getSnapshot handle
    version1 `shouldBe` SnapshotVersion 1
    rsLog snapshot1 `shouldBe` logSnap

    let dataSnap = DataSnapshot 2 3 (Just 10)
    cast @"dataSnapshot" handle #dataSnapshot dataSnap
    (version2, snapshot2) <- getSnapshot handle
    version2 `shouldBe` SnapshotVersion 2
    rsData snapshot2 `shouldBe` dataSnap
    rsLog snapshot2 `shouldBe` logSnap

emptyRenderSnapshot :: RenderSnapshot
emptyRenderSnapshot = RenderSnapshot
  { rsUi = emptyUiState
  , rsLog = LogSnapshot [] False 0 LogDebug
  , rsData = DataSnapshot 0 0 Nothing
  , rsTerrain = TerrainSnapshot 0 0 mempty mempty mempty
  }
