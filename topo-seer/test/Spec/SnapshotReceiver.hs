{-# LANGUAGE DataKinds #-}

module Spec.SnapshotReceiver (spec) where

import Test.Hspec
import Actor.Data (DataSnapshot(..), TerrainSnapshot(..))
import Topo.Overlay (emptyOverlayStore)
import Actor.SnapshotReceiver
  ( SnapshotVersion(..)
  , newDataSnapshotRef
  , newTerrainSnapshotRef
  , newSnapshotVersionRef
  , readDataSnapshot
  , readTerrainSnapshot
  , readSnapshotVersion
  , writeDataSnapshot
  , writeTerrainSnapshot
  , bumpSnapshotVersion
  )

spec :: Spec
spec = describe "SnapshotReceiver IORef helpers" $ do
  it "data ref starts with default and round-trips" $ do
    let initial = DataSnapshot 0 0 Nothing
    ref <- newDataSnapshotRef initial
    readDataSnapshot ref >>= (`shouldBe` initial)
    let updated = DataSnapshot 2 3 (Just 10)
    writeDataSnapshot ref updated
    readDataSnapshot ref >>= (`shouldBe` updated)

  it "terrain ref starts with default and round-trips" $ do
    let initial = TerrainSnapshot 0 0 mempty mempty mempty mempty mempty emptyOverlayStore
    ref <- newTerrainSnapshotRef initial
    readTerrainSnapshot ref >>= (`shouldBe` initial)

  it "version ref starts at 0 and bumps monotonically" $ do
    ref <- newSnapshotVersionRef
    readSnapshotVersion ref >>= (`shouldBe` SnapshotVersion 0)
    bumpSnapshotVersion ref
    readSnapshotVersion ref >>= (`shouldBe` SnapshotVersion 1)
    bumpSnapshotVersion ref
    bumpSnapshotVersion ref
    readSnapshotVersion ref >>= (`shouldBe` SnapshotVersion 3)
