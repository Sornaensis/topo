{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Spec.WorldPersist (spec) where

import Control.Exception (bracket, try, IOException)
import Data.Aeson (Value(..), encode, eitherDecodeStrict')
import qualified Data.ByteString.Lazy as BSL
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime)
import qualified Data.Vector as V
import System.Directory
  ( createDirectoryIfMissing
  , removeDirectoryRecursive
  )
import System.FilePath ((</>))
import Test.Hspec
import Spec.Support.OverlayFixtures (mkSparseFloatOverlay)

import Actor.UI (emptyUiState, UiState(..))
import Seer.Config.Snapshot (snapshotFromUi)
import Seer.Config.Snapshot.Types (ConfigSnapshot(..), defaultSnapshot)
import Seer.World.Persist
  ( WorldSaveManifest(..)
  , saveNamedWorld
  , loadNamedWorld
  , listWorlds
  , deleteNamedWorld
  , worldDir
  )
import Seer.World.Persist.Types (defaultManifestTime)
import Topo.Hex (defaultHexGridMeta)
import Topo.Storage (emptyProvenance, saveWorldWithProvenance)
import Topo.Overlay
  ( OverlayData(..)
  , OverlayStore(..)
  , emptyOverlayProvenance
  , emptyOverlayStore
  , insertOverlay
  , lookupOverlay
  , ovData
  )
import Topo.Types (WorldConfig(..))
import Topo.World (TerrainWorld(..), emptyWorld)

-- ---------------------------------------------------------------------------
-- Spec entry point
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "WorldPersist" $ do
  manifestJsonSpec
  worldRoundTripSpec
  listWorldsSpec

-- ---------------------------------------------------------------------------
-- WorldSaveManifest JSON round-trip
-- ---------------------------------------------------------------------------

manifestJsonSpec :: Spec
manifestJsonSpec = describe "WorldSaveManifest JSON round-trip" $ do
  it "round-trips a manifest" $ do
    now <- getCurrentTime
    let manifest = WorldSaveManifest
          { wsmName       = "test-world"
          , wsmSeed       = 9876
          , wsmChunkSize  = 64
          , wsmCreatedAt  = now
          , wsmChunkCount = 16
          , wsmOverlayNames = ["weather", "persist_sparse_test"]
          }
    let bytes = BSL.toStrict (encode manifest)
    eitherDecodeStrict' bytes `shouldBe` Right manifest

  it "parses empty JSON object with defaults" $ do
    case eitherDecodeStrict' "{}" of
      Left err -> expectationFailure err
      Right m  -> do
        wsmName m `shouldBe` ""
        wsmSeed m `shouldBe` 0
        wsmChunkSize m `shouldBe` 64
        wsmCreatedAt m `shouldBe` defaultManifestTime
        wsmChunkCount m `shouldBe` 0
        wsmOverlayNames m `shouldBe` []

-- ---------------------------------------------------------------------------
-- saveNamedWorld / loadNamedWorld round-trip
-- ---------------------------------------------------------------------------

-- | Unique test world name to avoid collisions with user data.
testWorldName :: Text.Text
testWorldName = "__topo_test_world_roundtrip__"

worldRoundTripSpec :: Spec
worldRoundTripSpec = describe "saveNamedWorld / loadNamedWorld" $
  do
    it "round-trips an empty world preserving config and metadata" $
      bracket
        (pure ())
        (\_ -> do
            _ <- deleteNamedWorld testWorldName
            pure ()
        )
        (\_ -> do
            let config = WorldConfig { wcChunkSize = 64 }
                world  = emptyWorld config defaultHexGridMeta
                ui     = emptyUiState { uiSeed = 42, uiChunkSize = 64 }

            saveResult <- saveNamedWorld testWorldName ui world
            saveResult `shouldBe` Right ()

            loadResult <- loadNamedWorld testWorldName
            case loadResult of
              Left err -> expectationFailure (Text.unpack err)
              Right (manifest, snapshot, loadedWorld) -> do
                wsmName manifest `shouldBe` testWorldName
                wsmSeed manifest `shouldBe` 42
                wsmChunkSize manifest `shouldBe` 64
                wsmChunkCount manifest `shouldBe` 0
                wsmOverlayNames manifest `shouldBe` []

                csSeed snapshot `shouldBe` 42
                csChunkSize snapshot `shouldBe` 64
                csName snapshot `shouldBe` testWorldName

                IntMap.size (twTerrain loadedWorld) `shouldBe` 0
                IntMap.size (twClimate loadedWorld) `shouldBe` 0
        )

    it "round-trips sparse overlays through unified world bundle persistence" $
      bracket
        (pure ())
        (\_ -> do
            _ <- deleteNamedWorld testWorldName
            pure ()
        )
        (\_ -> do
            let config = WorldConfig { wcChunkSize = 64 }
                baseWorld = emptyWorld config defaultHexGridMeta
                ui = emptyUiState { uiSeed = 42, uiChunkSize = 64 }

                overlay = mkSparseFloatOverlay
                  "persist_sparse_test"
                  "sparse overlay persistence test"
                  0.75
                  emptyOverlayProvenance
                world = baseWorld { twOverlays = insertOverlay overlay emptyOverlayStore }

            saveResult <- saveNamedWorld testWorldName ui world
            saveResult `shouldBe` Right ()

            loadResult <- loadNamedWorld testWorldName
            case loadResult of
              Left err -> expectationFailure (Text.unpack err)
              Right (manifest, _snapshot, loadedWorld) -> do
                case lookupOverlay "persist_sparse_test" (twOverlays loadedWorld) of
                  Nothing -> expectationFailure "sparse overlay missing after load"
                  Just loadedOverlay ->
                    case ovData loadedOverlay of
                      DenseData _ -> expectationFailure "expected sparse overlay"
                      SparseData chunks ->
                        IntMap.member 0 chunks `shouldBe` True
                wsmOverlayNames manifest `shouldBe` ["persist_sparse_test"]
        )

    it "normalizes metadata overlay names from world manifest and discovered overlays" $
      bracket
        (pure ())
        (\_ -> do
            _ <- deleteNamedWorld testWorldName
            pure ()
        )
        (\_ -> do
            let config = WorldConfig { wcChunkSize = 64 }
                baseWorld = emptyWorld config defaultHexGridMeta
                ui = emptyUiState { uiSeed = 42, uiChunkSize = 64 }

                overlay = mkSparseFloatOverlay
                  "persist_sparse_test"
                  "sparse overlay persistence test"
                  0.75
                  emptyOverlayProvenance
                world = (baseWorld { twOverlays = insertOverlay overlay emptyOverlayStore })
                  { twOverlayManifest = ["ghost", "persist_sparse_test"] }

            saveResult <- saveNamedWorld testWorldName ui world
            saveResult `shouldBe` Right ()

            loadResult <- loadNamedWorld testWorldName
            case loadResult of
              Left err -> expectationFailure (Text.unpack err)
              Right (manifest, _snapshot, _loadedWorld) -> do
                wsmOverlayNames manifest `shouldBe` ["persist_sparse_test"]
        )

    it "loads old-format world directories without sidecar when manifest is empty" $
      bracket
        (pure ())
        (\_ -> do
            _ <- deleteNamedWorld testWorldName
            pure ()
        )
        (\_ -> do
            let config = WorldConfig { wcChunkSize = 64 }
                world = emptyWorld config defaultHexGridMeta
                ui = emptyUiState { uiSeed = 101, uiChunkSize = 64 }

            dir <- worldDir
            let worldPath = dir </> Text.unpack testWorldName
                topoPath = worldPath </> "world.topo"
                metaPath = worldPath </> "meta.json"
                configPath = worldPath </> "config.json"
                manifest = WorldSaveManifest
                  { wsmName = testWorldName
                  , wsmSeed = 101
                  , wsmChunkSize = 64
                  , wsmCreatedAt = defaultManifestTime
                  , wsmChunkCount = 0
                  , wsmOverlayNames = []
                  }

            createDirectoryIfMissing True worldPath
            topoSave <- saveWorldWithProvenance topoPath emptyProvenance world
            topoSave `shouldBe` Right ()
            BSL.writeFile metaPath (encode manifest)
            BSL.writeFile configPath (encode (snapshotFromUi ui testWorldName))

            loadResult <- loadNamedWorld testWorldName
            case loadResult of
              Left err -> expectationFailure (Text.unpack err)
              Right (loadedManifest, _snapshot, loadedWorld) -> do
                wsmName loadedManifest `shouldBe` testWorldName
                wsmOverlayNames loadedManifest `shouldBe` []
                lookupOverlay "weather" (twOverlays loadedWorld) `shouldBe` Nothing
        )

-- ---------------------------------------------------------------------------
-- listWorlds
-- ---------------------------------------------------------------------------

-- | Unique names for list test.
testListNameA :: Text.Text
testListNameA = "__topo_test_list_world_A__"

testListNameB :: Text.Text
testListNameB = "__topo_test_list_world_B__"

testListNameInvalid :: Text.Text
testListNameInvalid = "__topo_test_list_invalid__"

listWorldsSpec :: Spec
listWorldsSpec = describe "listWorlds" $
  it "returns manifests sorted by date (newest first), skips invalid dirs" $
    bracket
      (pure ())
      (\_ -> do
          _ <- deleteNamedWorld testListNameA
          _ <- deleteNamedWorld testListNameB
          -- Clean up invalid directory manually
          dir <- worldDir
          _ <- try @IOException
            (removeDirectoryRecursive (dir </> Text.unpack testListNameInvalid))
          pure ()
      )
      (\_ -> do
          let config = WorldConfig { wcChunkSize = 64 }
              world  = emptyWorld config defaultHexGridMeta

          -- Save world A first
          let uiA = emptyUiState { uiSeed = 100, uiChunkSize = 64 }
          _ <- saveNamedWorld testListNameA uiA world

          -- Save world B second (will have a later timestamp)
          let uiB = emptyUiState { uiSeed = 200, uiChunkSize = 64 }
          _ <- saveNamedWorld testListNameB uiB world

          -- Create an invalid directory (no meta.json)
          dir <- worldDir
          let invalidPath = dir </> Text.unpack testListNameInvalid
          createDirectoryIfMissing True invalidPath

          -- List
          worlds <- listWorlds
          let testWorlds = filter
                (\m -> Text.pack "__topo_test_list_" `Text.isPrefixOf` wsmName m)
                worlds

          -- Should have exactly 2 valid entries (invalid dir skipped)
          length testWorlds `shouldBe` 2

          -- Newest first: B should come before A
          case testWorlds of
            (first:second:_) -> do
              wsmName first `shouldBe` testListNameB
              wsmName second `shouldBe` testListNameA
              wsmSeed first `shouldBe` 200
              wsmSeed second `shouldBe` 100
            _ -> expectationFailure "Expected at least 2 test worlds"
      )
