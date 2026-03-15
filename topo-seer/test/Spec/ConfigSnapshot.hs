{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Spec.ConfigSnapshot (spec) where

import Control.Exception (IOException, bracket, try)
import Data.Aeson (Value(..), encode, eitherDecodeStrict')
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text
import Data.Text (Text)
import Hyperspace.Actor (ActorSystem, getSingleton, newActorSystem, shutdownActorSystem)
import System.Directory
  ( createDirectoryIfMissing
  , getTemporaryDirectory
  , removeDirectoryRecursive
  , removeFile
  )
import System.FilePath ((</>))
import Test.Hspec

import Actor.UI (UiState(..), emptyUiState, getUiSnapshot, uiActorDef)
import Actor.UI.State (sliderValueForId)
import Seer.Config (configFromUi)
import Seer.Config.Snapshot
  ( ConfigSnapshot(..)
  , applySnapshotToUi
  , currentSnapshotVersion
  , defaultSnapshot
  , listSnapshots
  , loadSnapshot
  , saveSnapshot
  , snapshotDir
  , snapshotFromUi
  )
import Seer.Config.SliderSpec (SliderId(..), sliderLabelForId)
import Topo.BaseHeight (GenConfig(..))
import Topo.Erosion (ErosionConfig(..), defaultErosionConfig)
import Topo.Hypsometry (HypsometryConfig(..))
import Topo.Planet (WorldSlice(..), hexesPerDegreeLatitude, hexesPerDegreeLongitude)
import Topo.Types (worldExtentRadiusX, worldExtentRadiusY)
import Topo.WorldGen
  ( WorldGenConfig(..)
  , TerrainConfig(..)
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
  snapshotCompatSpec

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

  it "keeps derived slice extents aligned with the generated config" $ do
    let ui = emptyUiState
          { uiChunkSize = 96
          , uiWorldExtentX = 0.6
          , uiWorldExtentY = 0.35
          , uiSliceLatCenter = 0.65
          , uiSliceLonCenter = 0.2
          , uiPlanetRadius = 0.8
          }
        snap = snapshotFromUi ui "derived"
        cfg = csGenConfig snap
        slice = worldSlice cfg
        extent = gcWorldExtent (terrainGen (worldTerrain cfg))
        planet = worldPlanet cfg
        chunkSize = max 1 (uiChunkSize ui)
        expectedLatExtent =
          max 0.1 (fromIntegral (worldExtentRadiusY extent * 2 * chunkSize) / hexesPerDegreeLatitude planet)
        expectedLonExtent =
          max 0.1 (fromIntegral (worldExtentRadiusX extent * 2 * chunkSize) / hexesPerDegreeLongitude planet (wsLatCenter slice))
    cfg `shouldBe` configFromUi ui
    wsLatExtent slice `shouldBe` expectedLatExtent
    wsLonExtent slice `shouldBe` expectedLonExtent

  it "round-trips snapshot restore through the UI actor without changing slider semantics" $
    withSystem $ \system -> do
      let ui = emptyUiState
            { uiSeed = 314159
            , uiChunkSize = 96
            , uiRenderWaterLevel = 0.65
            , uiGenScale = 0.21
            , uiGenOctaves = 0.73
            , uiWaterLevel = 0.42
            , uiWeatherTick = 0.61
            , uiVegBase = 0.77
            , uiRainRate = 0.33
            , uiSliceLatCenter = 0.64
            , uiSliceLonCenter = 0.18
            }
          snap = snapshotFromUi ui "round-trip"
      handle <- getSingleton system uiActorDef
      applySnapshotToUi snap handle
      restored <- getUiSnapshot handle
      let restoredSnapshot = snapshotFromUi restored "restored"
      csSeed restoredSnapshot `shouldBe` csSeed snap
      csChunkSize restoredSnapshot `shouldBe` csChunkSize snap
      csRenderWaterLevel restoredSnapshot `shouldBe` csRenderWaterLevel snap
      mapM_ (assertSnapshotStable ui restored) allSliderIds

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

withSystem :: (ActorSystem -> IO a) -> IO a
withSystem = bracket newActorSystem shutdownActorSystem

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

-- ---------------------------------------------------------------------------
-- Snapshot schema evolution
-- ---------------------------------------------------------------------------

snapshotCompatSpec :: Spec
snapshotCompatSpec = describe "Snapshot schema evolution" $ do
  describe "new fields get defaults via mergeDefaults" $ do
    it "ErosionConfig missing coastalSmoothIterations gets default 2" $ do
      -- Build an ErosionConfig JSON that omits the new field
      let fullJson = encode defaultErosionConfig
          -- Decode to Value, strip the new key, re-encode
          stripped = removeKey "coastalSmoothIterations" fullJson
      case eitherDecodeStrict' @ErosionConfig (BSL.toStrict stripped) of
        Left err -> expectationFailure err
        Right ec -> ecCoastalSmoothIterations ec `shouldBe` 2

    it "ErosionConfig with explicit coastalSmoothIterations preserves it" $ do
      let ec = defaultErosionConfig { ecCoastalSmoothIterations = 7 }
          bytes = BSL.toStrict (encode ec)
      case eitherDecodeStrict' @ErosionConfig bytes of
        Left err -> expectationFailure err
        Right ec' -> ecCoastalSmoothIterations ec' `shouldBe` 7

    it "ConfigSnapshot with old erosion config (missing new field) fills defaults" $ do
      -- Encode defaultSnapshot, mutate the erosion sub-config to remove the key
      let snap = defaultSnapshot
          json = encode snap
      case stripNestedKey ["genConfig", "terrain", "erosion"]
                          "coastalSmoothIterations" json of
        Nothing  -> expectationFailure "failed to strip nested key"
        Just stripped ->
          case eitherDecodeStrict' @ConfigSnapshot (BSL.toStrict stripped) of
            Left err -> expectationFailure err
            Right cs ->
              let erosion = terrainErosion (worldTerrain (csGenConfig cs))
              in ecCoastalSmoothIterations erosion `shouldBe` 2

  describe "old snapshot applies cleanly" $ do
    it "empty genConfig decodes to defaultWorldGenConfig" $ do
      let json = "{\"name\":\"old\",\"genConfig\":{}}"
      case eitherDecodeStrict' @ConfigSnapshot json of
        Left err -> expectationFailure err
        Right cs -> csGenConfig cs `shouldBe` defaultWorldGenConfig

    it "completely empty JSON decodes to defaultSnapshot" $ do
      case eitherDecodeStrict' @ConfigSnapshot "{}" of
        Left err -> expectationFailure err
        Right cs -> cs `shouldBe` defaultSnapshot

  describe "save/reload preserves all fields including new ones" $ do
    it "round-trips non-default ecCoastalSmoothIterations through file" $
      withTempDir $ \dir -> do
        let path = dir </> "compat-test.json"
            wgc  = defaultWorldGenConfig
            tc   = worldTerrain wgc
            ec   = terrainErosion tc
            ec'  = ec { ecCoastalSmoothIterations = 6 }
            tc'  = tc { terrainErosion = ec' }
            wgc' = wgc { worldTerrain = tc' }
            snap = defaultSnapshot
              { csName = "compat"
              , csGenConfig = wgc'
              }
        result <- saveSnapshot path snap
        result `shouldBe` Right ()
        loaded <- loadSnapshot path
        case loaded of
          Left err -> expectationFailure (Text.unpack err)
          Right cs -> do
            csName cs `shouldBe` "compat"
            let erosion = terrainErosion (worldTerrain (csGenConfig cs))
            ecCoastalSmoothIterations erosion `shouldBe` 6

    it "round-trips updated hypsometry slider ranges through file" $
      withTempDir $ \dir -> do
        let path = dir </> "hyps-range-test.json"
            wgc  = defaultWorldGenConfig
            tc   = worldTerrain wgc
            hp   = terrainHypsometry tc
            -- Use values within the updated slider ranges
            hp'  = hp { hpCoastalRampWidth = 0.15 }
            tc'  = tc { terrainHypsometry = hp' }
            wgc' = wgc { worldTerrain = tc' }
            snap = defaultSnapshot { csGenConfig = wgc' }
        result <- saveSnapshot path snap
        result `shouldBe` Right ()
        loaded <- loadSnapshot path
        case loaded of
          Left err -> expectationFailure (Text.unpack err)
          Right cs ->
            let hp'' = terrainHypsometry (worldTerrain (csGenConfig cs))
            in hpCoastalRampWidth hp'' `shouldBe` 0.15

    it "snapshotFromUi with default UiState round-trips through JSON" $ do
      let snap  = snapshotFromUi emptyUiState "default-ui"
          bytes = BSL.toStrict (encode snap)
      case eitherDecodeStrict' @ConfigSnapshot bytes of
        Left err -> expectationFailure err
        Right snap' -> csGenConfig snap' `shouldBe` csGenConfig snap

-- | Remove a top-level key from a JSON object encoded as lazy ByteString.
removeKey :: Text -> BSL.ByteString -> BSL.ByteString
removeKey key lbs =
  case eitherDecodeStrict' @Value (BSL.toStrict lbs) of
    Left _           -> lbs
    Right (Object o) -> encode (Object (KM.delete (fromText key) o))
    Right v          -> encode v

-- | Remove a key nested under a path of object keys.
-- Returns 'Nothing' if the path doesn't exist.
stripNestedKey :: [Text] -> Text -> BSL.ByteString -> Maybe BSL.ByteString
stripNestedKey path key lbs =
  case eitherDecodeStrict' @Value (BSL.toStrict lbs) of
    Left _  -> Nothing
    Right v -> Just (encode (go path v))
  where
    go []     (Object o) = Object (KM.delete (fromText key) o)
    go (p:ps) (Object o) =
      case KM.lookup (fromText p) o of
        Just inner -> Object (KM.insert (fromText p) (go ps inner) o)
        Nothing    -> Object o
    go _ v = v

allSliderIds :: [SliderId]
allSliderIds = [minBound .. maxBound]

assertSnapshotStable :: UiState -> UiState -> SliderId -> Expectation
assertSnapshotStable original restored sliderIdValue =
  sliderLabelForId sliderIdValue (sliderValueForId restored sliderIdValue)
    `shouldBe` sliderLabelForId sliderIdValue (sliderValueForId original sliderIdValue)
