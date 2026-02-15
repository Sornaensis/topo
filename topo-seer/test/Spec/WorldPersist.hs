{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Spec.WorldPersist (spec) where

import Control.Exception (bracket, try, IOException)
import Data.Aeson (encode, eitherDecodeStrict')
import qualified Data.ByteString.Lazy as BSL
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text as Text
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime)
import System.Directory
  ( createDirectoryIfMissing
  , removeDirectoryRecursive
  )
import System.FilePath ((</>))
import Test.Hspec

import Actor.UI (emptyUiState, UiState(..))
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

-- ---------------------------------------------------------------------------
-- saveNamedWorld / loadNamedWorld round-trip
-- ---------------------------------------------------------------------------

-- | Unique test world name to avoid collisions with user data.
testWorldName :: Text.Text
testWorldName = "__topo_test_world_roundtrip__"

worldRoundTripSpec :: Spec
worldRoundTripSpec = describe "saveNamedWorld / loadNamedWorld" $
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

          -- Save
          saveResult <- saveNamedWorld testWorldName ui world
          saveResult `shouldBe` Right ()

          -- Load
          loadResult <- loadNamedWorld testWorldName
          case loadResult of
            Left err -> expectationFailure (Text.unpack err)
            Right (manifest, snapshot, loadedWorld) -> do
              -- Manifest metadata
              wsmName manifest `shouldBe` testWorldName
              wsmSeed manifest `shouldBe` 42
              wsmChunkSize manifest `shouldBe` 64
              wsmChunkCount manifest `shouldBe` 0   -- empty world

              -- Config snapshot values
              csSeed snapshot `shouldBe` 42
              csChunkSize snapshot `shouldBe` 64
              csName snapshot `shouldBe` testWorldName

              -- Terrain data (empty world → empty IntMaps)
              IntMap.size (twTerrain loadedWorld) `shouldBe` 0
              IntMap.size (twClimate loadedWorld) `shouldBe` 0
              IntMap.size (twWeather loadedWorld) `shouldBe` 0
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
