{-# LANGUAGE DataKinds #-}

module Spec.SnapshotReceiver (spec) where

import Actor.Data (DataSnapshot(..), TerrainSnapshot(..), defaultTerrainGeoContext)
import Actor.Log (LogLevel(..), LogSnapshot(..), newLogSnapshotRef)
import Actor.Render (RenderSnapshot(..))
import Actor.SnapshotReceiver
  ( SnapshotVersion(..)
  , dataAndTerrainSnapshotUpdate
  , invalidatePublishedSnapshot
  , newDataSnapshotRef
  , newRenderSnapshotVersionRef
  , newSnapshotVersionRef
  , newTerrainSnapshotRef
  , publishChangedUiAndLog
  , publishSnapshot
  , publishSnapshotIfVersion
  , readCommittedRenderSnapshot
  , readDataSnapshot
  , readSnapshotVersion
  , readTerrainSnapshot
  , uiSnapshotUpdate
  , withCommittedRenderSnapshot
  , withLogSnapshot
  , withUiSnapshot
  , writeDataSnapshot
  )
import Actor.UI (UiState(..), emptyUiState, newUiSnapshotRef)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
  ( newEmptyMVar
  , putMVar
  , readMVar
  , takeMVar
  , tryTakeMVar
  )
import Control.Monad (forM, forM_, when)
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List (sort)
import Test.Hspec
import Topo.Overlay (emptyOverlayStore)

spec :: Spec
spec = describe "SnapshotReceiver publication protocol" $ do
  it "low-level refs round-trip for initialization and tests" $ do
    let initial = DataSnapshot 0 0 Nothing
    ref <- newDataSnapshotRef initial
    readDataSnapshot ref >>= (`shouldBe` initial)
    let updated = DataSnapshot 2 3 (Just 10)
    writeDataSnapshot ref updated
    readDataSnapshot ref >>= (`shouldBe` updated)

    terrainRef <- newTerrainSnapshotRef initialTerrain
    readTerrainSnapshot terrainRef >>= (`shouldBe` initialTerrain)

  it "writes every domain before committing exactly one returned version" $ do
    dataRef <- newDataSnapshotRef (dataFor 0)
    terrainRef <- newTerrainSnapshotRef (terrainFor 0)
    versionRef <- newSnapshotVersionRef

    committed <- publishSnapshot versionRef
      (dataAndTerrainSnapshotUpdate dataRef (dataFor 7) terrainRef (terrainFor 7))

    committed `shouldBe` SnapshotVersion 1
    readSnapshotVersion versionRef `shouldReturn` committed
    observedVersion <- readSnapshotVersion versionRef
    observedData <- readDataSnapshot dataRef
    observedTerrain <- readTerrainSnapshot terrainRef
    observedVersion `shouldBe` committed
    observedData `shouldBe` dataFor 7
    observedTerrain `shouldBe` terrainFor 7

  it "rejects stale conditional publications without writing refs" $ do
    dataRef <- newDataSnapshotRef (dataFor 0)
    terrainRef <- newTerrainSnapshotRef (terrainFor 0)
    versionRef <- newSnapshotVersionRef
    _ <- publishSnapshot versionRef
      (dataAndTerrainSnapshotUpdate dataRef (dataFor 1) terrainRef (terrainFor 1))

    result <- publishSnapshotIfVersion versionRef (SnapshotVersion 0)
      (dataAndTerrainSnapshotUpdate dataRef (dataFor 2) terrainRef (terrainFor 2))

    result `shouldBe` Nothing
    readSnapshotVersion versionRef `shouldReturn` SnapshotVersion 1
    readDataSnapshot dataRef `shouldReturn` dataFor 1
    readTerrainSnapshot terrainRef `shouldReturn` terrainFor 1

  it "returns monotonic versions for publications and named invalidations" $ do
    dataRef <- newDataSnapshotRef (dataFor 0)
    terrainRef <- newTerrainSnapshotRef (terrainFor 0)
    versionRef <- newSnapshotVersionRef
    first <- publishSnapshot versionRef
      (dataAndTerrainSnapshotUpdate dataRef (dataFor 1) terrainRef (terrainFor 1))
    second <- invalidatePublishedSnapshot versionRef
    third <- publishSnapshot versionRef
      (dataAndTerrainSnapshotUpdate dataRef (dataFor 2) terrainRef (terrainFor 2))
    [first, second, third] `shouldBe`
      [SnapshotVersion 1, SnapshotVersion 2, SnapshotVersion 3]

  it "serializes concurrent publishers with unique monotonic commits" $ do
    dataRef <- newDataSnapshotRef (dataFor 0)
    terrainRef <- newTerrainSnapshotRef (terrainFor 0)
    versionRef <- newSnapshotVersionRef
    start <- newEmptyMVar
    results <- newEmptyMVar
    let publicationCount = 64
    forM_ [1 .. publicationCount] $ \marker -> do
      _ <- forkIO $ do
        readMVar start
        version <- publishSnapshot versionRef
          (dataAndTerrainSnapshotUpdate
            dataRef (dataFor marker)
            terrainRef (terrainFor marker))
        putMVar results version
      pure ()
    putMVar start ()
    versions <- forM [1 .. publicationCount] (const (takeMVar results))
    sort versions `shouldBe` map (SnapshotVersion . fromIntegral) [1 .. publicationCount]
    readSnapshotVersion versionRef `shouldReturn`
      SnapshotVersion (fromIntegral publicationCount)

  it "serializes a publisher against an in-flight coherent reader" $ do
    uiRef <- newUiSnapshotRef
    logRef <- newLogSnapshotRef
    dataRef <- newDataSnapshotRef (dataFor 0)
    terrainRef <- newTerrainSnapshotRef (terrainFor 0)
    versionRef <- newRenderSnapshotVersionRef uiRef logRef dataRef terrainRef
    readerEntered <- newEmptyMVar
    releaseReader <- newEmptyMVar
    readerResult <- newEmptyMVar
    publisherStarted <- newEmptyMVar
    publisherResult <- newEmptyMVar

    _ <- forkIO $ do
      result <- withCommittedRenderSnapshot versionRef $ \version snapshot -> do
        putMVar readerEntered ()
        takeMVar releaseReader
        pure (version, renderMarkers snapshot)
      putMVar readerResult result
    takeMVar readerEntered

    _ <- forkIO $ do
      putMVar publisherStarted ()
      version <- publishSnapshot versionRef
        (withUiSnapshot (uiFor 1)
          (dataAndTerrainSnapshotUpdate
            dataRef (dataFor 1)
            terrainRef (terrainFor 1)))
      putMVar publisherResult version
    takeMVar publisherStarted
    -- The publisher cannot commit while the reader owns the generation.
    tryTakeMVar publisherResult `shouldReturn` Nothing
    putMVar releaseReader ()
    takeMVar readerResult `shouldReturn` (SnapshotVersion 0, replicate 4 0)
    takeMVar publisherResult `shouldReturn` SnapshotVersion 1

  it "recaptures UI/log after a concurrent commit instead of overwriting it" $ do
    uiRef <- newUiSnapshotRef
    logRef <- newLogSnapshotRef
    dataRef <- newDataSnapshotRef (dataFor 0)
    terrainRef <- newTerrainSnapshotRef (terrainFor 0)
    versionRef <- newRenderSnapshotVersionRef uiRef logRef dataRef terrainRef
    firstUiCapture <- newEmptyMVar
    releaseFirstCapture <- newEmptyMVar
    finished <- newEmptyMVar
    isFirstCaptureRef <- newIORef True
    writeIORef uiRef (uiFor 1)
    writeIORef logRef (logFor 1)
    let captureUi = do
          captured <- readIORef uiRef
          isFirst <- atomicModifyIORef' isFirstCaptureRef (\first -> (False, first))
          when isFirst $ do
            putMVar firstUiCapture ()
            takeMVar releaseFirstCapture
          pure captured

    _ <- forkIO $ do
      publishChangedUiAndLog
        versionRef (uiFor 0) (logFor 0) captureUi (readIORef logRef)
      putMVar finished ()
    takeMVar firstUiCapture

    -- The helper has captured UI 1 against generation zero. A newer complete
    -- publication must make that stale conditional write fail and recapture.
    writeIORef uiRef (uiFor 2)
    writeIORef logRef (logFor 2)
    publishSnapshot versionRef
      (withLogSnapshot (logFor 2) (uiSnapshotUpdate (uiFor 2)))
      `shouldReturn` SnapshotVersion 1
    putMVar releaseFirstCapture ()
    takeMVar finished

    (version, snapshot) <- readCommittedRenderSnapshot versionRef
    version `shouldBe` SnapshotVersion 1
    renderMarkers snapshot `shouldBe` [2, 2, 0, 0]

  it "publishes a changed log even when UI was explicitly published" $ do
    uiRef <- newUiSnapshotRef
    logRef <- newLogSnapshotRef
    dataRef <- newDataSnapshotRef (dataFor 0)
    terrainRef <- newTerrainSnapshotRef (terrainFor 0)
    versionRef <- newRenderSnapshotVersionRef uiRef logRef dataRef terrainRef
    writeIORef uiRef (uiFor 1)
    writeIORef logRef (logFor 1)

    publishSnapshot versionRef (uiSnapshotUpdate (uiFor 1))
      `shouldReturn` SnapshotVersion 1
    publishChangedUiAndLog
      versionRef (uiFor 0) (logFor 0) (readIORef uiRef) (readIORef logRef)

    (version, snapshot) <- readCommittedRenderSnapshot versionRef
    version `shouldBe` SnapshotVersion 2
    renderMarkers snapshot `shouldBe` [1, 1, 0, 0]

  it "does not absorb staged UI/log values into an unrelated data publication" $ do
    uiRef <- newUiSnapshotRef
    logRef <- newLogSnapshotRef
    dataRef <- newDataSnapshotRef (dataFor 0)
    terrainRef <- newTerrainSnapshotRef (terrainFor 0)
    versionRef <- newRenderSnapshotVersionRef uiRef logRef dataRef terrainRef
    writeIORef uiRef (uiFor 9)
    writeIORef logRef (logFor 9)

    _ <- publishSnapshot versionRef
      (dataAndTerrainSnapshotUpdate dataRef (dataFor 1) terrainRef (terrainFor 1))
    (version, snapshot) <- readCommittedRenderSnapshot versionRef
    version `shouldBe` SnapshotVersion 1
    renderMarkers snapshot `shouldBe` [0, 0, 1, 1]

    _ <- publishSnapshot versionRef
      (withLogSnapshot (logFor 9)
        (withUiSnapshot (uiFor 9)
          (dataAndTerrainSnapshotUpdate dataRef (dataFor 9) terrainRef (terrainFor 9))))
    (_, intendedSnapshot) <- readCommittedRenderSnapshot versionRef
    renderMarkers intendedSnapshot `shouldBe` replicate 4 9

  it "publishes a coherent five-domain tuple under deterministic concurrency" $ do
    uiRef <- newUiSnapshotRef
    logRef <- newLogSnapshotRef
    dataRef <- newDataSnapshotRef (dataFor 0)
    terrainRef <- newTerrainSnapshotRef (terrainFor 0)
    versionRef <- newRenderSnapshotVersionRef uiRef logRef dataRef terrainRef
    staged <- newEmptyMVar
    allowCommit <- newEmptyMVar
    committed <- newEmptyMVar
    let publicationCount = 100
    _ <- forkIO $ forM_ [1 .. publicationCount] $ \marker -> do
      -- These actor-owned refs become visible to render readers only when the
      -- ordered publication captures the complete tuple below.
      writeIORef uiRef (uiFor marker)
      writeIORef logRef (logFor marker)
      putMVar staged marker
      takeMVar allowCommit
      version <- publishSnapshot versionRef
        (withLogSnapshot (logFor marker)
          (withUiSnapshot (uiFor marker)
            (dataAndTerrainSnapshotUpdate
              dataRef (dataFor marker)
              terrainRef (terrainFor marker))))
      putMVar committed version

    forM_ [1 .. publicationCount] $ \marker -> do
      takeMVar staged `shouldReturn` marker
      -- While the next UI/log values are staged, readers still see the prior
      -- immutable generation rather than old-version/new-domain mixtures.
      (beforeVersion, beforeSnapshot) <- readCommittedRenderSnapshot versionRef
      beforeVersion `shouldBe` SnapshotVersion (fromIntegral (marker - 1))
      renderMarkers beforeSnapshot `shouldBe` replicate 4 (marker - 1)
      putMVar allowCommit ()
      takeMVar committed `shouldReturn` SnapshotVersion (fromIntegral marker)
      (afterVersion, afterSnapshot) <- readCommittedRenderSnapshot versionRef
      afterVersion `shouldBe` SnapshotVersion (fromIntegral marker)
      renderMarkers afterSnapshot `shouldBe` replicate 4 marker

initialTerrain :: TerrainSnapshot
initialTerrain =
  TerrainSnapshot
    0 0 0 0 0 0
    mempty mempty mempty mempty mempty mempty mempty mempty mempty
    emptyOverlayStore
    defaultTerrainGeoContext

dataFor :: Int -> DataSnapshot
dataFor marker = DataSnapshot marker marker (Just (fromIntegral marker))

terrainFor :: Int -> TerrainSnapshot
terrainFor marker = initialTerrain { tsVersion = fromIntegral marker }

uiFor :: Int -> UiState
uiFor marker = emptyUiState { uiSeed = fromIntegral marker }

logFor :: Int -> LogSnapshot
logFor marker = LogSnapshot [] False marker LogDebug

renderMarkers :: RenderSnapshot -> [Int]
renderMarkers snapshot =
  [ fromIntegral (uiSeed (rsUi snapshot))
  , lsScroll (rsLog snapshot)
  , dsTerrainChunks (rsData snapshot)
  , fromIntegral (tsVersion (rsTerrain snapshot))
  ]
