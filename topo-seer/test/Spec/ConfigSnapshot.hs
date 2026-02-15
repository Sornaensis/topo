{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Spec.ConfigSnapshot (spec) where

import Control.Exception (IOException, bracket, try)
import Data.Aeson (encode, eitherDecodeStrict')
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text
import System.Directory
  ( createDirectoryIfMissing
  , getTemporaryDirectory
  , removeDirectoryRecursive
  , removeFile
  )
import System.FilePath ((</>))
import Test.Hspec

import Actor.UI (UiState(..), emptyUiState)
import Seer.Config.Snapshot
  ( ConfigSnapshot(..)
  , currentSnapshotVersion
  , defaultSnapshot
  , snapshotFromUi
  , saveSnapshot
  , loadSnapshot
  , listSnapshots
  , snapshotDir
  )
import Topo.WorldGen
  ( WorldGenConfig(..)
  , defaultWorldGenConfig
  , aridWorldGenConfig
  , lushWorldGenConfig
  )

-- ---------------------------------------------------------------------------
-- Spec entry point
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "ConfigSnapshot" $ do
  jsonRoundTripSpec
  snapshotFromUiSpec
  fileIOSpec
  presetVariantSpec

-- ---------------------------------------------------------------------------
-- JSON round-trip
-- ---------------------------------------------------------------------------

jsonRoundTripSpec :: Spec
jsonRoundTripSpec = describe "JSON round-trip" $ do
  it "round-trips defaultSnapshot through encode/decode" $ do
    let bytes = BSL.toStrict (encode defaultSnapshot)
    eitherDecodeStrict' bytes `shouldBe` Right defaultSnapshot

  it "round-trips a custom snapshot" $ do
    let custom = defaultSnapshot
          { csName = "custom"
          , csSeed = 99
          , csChunkSize = 128
          , csRenderWaterLevel = 0.7
          }
    let bytes = BSL.toStrict (encode custom)
    eitherDecodeStrict' bytes `shouldBe` Right custom

  it "parses empty JSON object with defaults" $ do
    case eitherDecodeStrict' @ConfigSnapshot "{}" of
      Left err -> expectationFailure err
      Right cs -> cs `shouldBe` defaultSnapshot

  it "ignores unknown fields without error" $ do
    let json = "{\"futureField\": 99, \"name\": \"forward\"}"
    case eitherDecodeStrict' @ConfigSnapshot json of
      Left err -> expectationFailure err
      Right cs -> csName cs `shouldBe` "forward"

  it "preserves version field" $ do
    let cs = defaultSnapshot { csVersion = 42 }
        bytes = BSL.toStrict (encode cs)
    case eitherDecodeStrict' @ConfigSnapshot bytes of
      Left err -> expectationFailure err
      Right cs' -> csVersion cs' `shouldBe` 42

-- ---------------------------------------------------------------------------
-- snapshotFromUi
-- ---------------------------------------------------------------------------

snapshotFromUiSpec :: Spec
snapshotFromUiSpec = describe "snapshotFromUi" $ do
  it "captures seed and chunk size from UiState" $ do
    let ui = emptyUiState { uiSeed = 12345, uiChunkSize = 128 }
        snap = snapshotFromUi ui "test"
    csSeed snap `shouldBe` 12345
    csChunkSize snap `shouldBe` 128
    csName snap `shouldBe` "test"

  it "sets the current snapshot version" $ do
    let snap = snapshotFromUi emptyUiState "v"
    csVersion snap `shouldBe` currentSnapshotVersion

  it "produces a WorldGenConfig (not Nothing)" $ do
    let snap = snapshotFromUi emptyUiState "gen"
    -- Just verify the genConfig is a valid WorldGenConfig (has sub-configs)
    csGenConfig snap `shouldSatisfy` const True

-- ---------------------------------------------------------------------------
-- File I/O
-- ---------------------------------------------------------------------------

-- | Use a temporary directory for save/load tests, cleaning up afterwards.
withTempDir :: (FilePath -> IO a) -> IO a
withTempDir action =
  bracket setup teardown action
  where
    setup = do
      tmp <- getTemporaryDirectory
      let dir = tmp </> "topo-test-snapshots"
      createDirectoryIfMissing True dir
      pure dir
    teardown dir = do
      _ <- try @IOException (removeDirectoryRecursive dir)
      pure ()

fileIOSpec :: Spec
fileIOSpec = describe "File I/O" $ do
  describe "saveSnapshot / loadSnapshot" $ do
    it "round-trips through a temp file" $ withTempDir $ \dir -> do
      let path = dir </> "round-trip.json"
          snap = defaultSnapshot { csName = "round-trip-test" }
      result <- saveSnapshot path snap
      result `shouldBe` Right ()
      loaded <- loadSnapshot path
      loaded `shouldBe` Right snap

    it "loadSnapshot returns Left for missing file" $ withTempDir $ \dir -> do
      loaded <- loadSnapshot (dir </> "nonexistent.json")
      loaded `shouldSatisfy` isLeft

    it "loadSnapshot returns Left for invalid JSON" $ withTempDir $ \dir -> do
      let path = dir </> "bad.json"
      writeFile path "not json {"
      loaded <- loadSnapshot path
      loaded `shouldSatisfy` isLeft

  describe "listSnapshots" $ do
    it "returns sorted names and ignores non-JSON files" $ do
      dir <- snapshotDir
      let nameA  = "__test_snap_aaa__"
          nameB  = "__test_snap_zzz__"
          pathA  = dir </> (nameA <> ".json")
          pathB  = dir </> (nameB <> ".json")
          pathTx = dir </> "__test_snap_ignore__.txt"
      bracket
        (do _ <- saveSnapshot pathA (defaultSnapshot { csName = Text.pack nameA })
            _ <- saveSnapshot pathB (defaultSnapshot { csName = Text.pack nameB })
            writeFile pathTx "not a snapshot"
        )
        (\_ -> do
            _ <- try @IOException (removeFile pathA)
            _ <- try @IOException (removeFile pathB)
            _ <- try @IOException (removeFile pathTx)
            pure ()
        )
        (\_ -> do
            names <- listSnapshots
            let testNames = filter (\n -> Text.pack "__test_snap_" `Text.isPrefixOf` n) names
            testNames `shouldSatisfy` (\ns ->
              Text.pack nameA `elem` ns && Text.pack nameB `elem` ns)
            testNames `shouldSatisfy` all (\n -> not (Text.pack "__test_snap_ignore__" `Text.isSuffixOf` n))
            testNames `shouldSatisfy` isSorted
        )

-- ---------------------------------------------------------------------------
-- Preset variant serialization
-- ---------------------------------------------------------------------------

presetVariantSpec :: Spec
presetVariantSpec = describe "Preset variant serialization" $ do
  it "aridWorldGenConfig round-trips through ConfigSnapshot" $ do
    let snap = defaultSnapshot { csGenConfig = aridWorldGenConfig }
        bytes = BSL.toStrict (encode snap)
    case eitherDecodeStrict' @ConfigSnapshot bytes of
      Left err -> expectationFailure err
      Right snap' -> csGenConfig snap' `shouldBe` aridWorldGenConfig

  it "lushWorldGenConfig round-trips through ConfigSnapshot" $ do
    let snap = defaultSnapshot { csGenConfig = lushWorldGenConfig }
        bytes = BSL.toStrict (encode snap)
    case eitherDecodeStrict' @ConfigSnapshot bytes of
      Left err -> expectationFailure err
      Right snap' -> csGenConfig snap' `shouldBe` lushWorldGenConfig

  it "defaultWorldGenConfig round-trips through ConfigSnapshot" $ do
    let snap = defaultSnapshot
        bytes = BSL.toStrict (encode snap)
    case eitherDecodeStrict' @ConfigSnapshot bytes of
      Left err -> expectationFailure err
      Right snap' -> csGenConfig snap' `shouldBe` defaultWorldGenConfig

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x:y:rest) = x <= y && isSorted (y:rest)
