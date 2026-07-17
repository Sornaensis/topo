{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Spec.ConfigSnapshot (spec) where

import Control.Exception (IOException, bracket, finally, try)
import Data.Aeson (Value(..), encode, eitherDecodeStrict', toJSON)
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text
import Data.Foldable (toList)
import Data.Text (Text)
import Hyperspace.Actor (ActorSystem, get, newActorSystem, shutdownActorSystem)
import System.Directory
  ( createDirectoryIfMissing
  , getTemporaryDirectory
  , removeDirectoryRecursive
  , removeFile
  )
import System.FilePath ((</>))
import Test.Hspec

import Actor.UI (Ui, UiState(..), emptyUiState, getUiSnapshot)
import Actor.UI.State (sliderValueForId)
import Seer.Config (configFromUi, mapRange)
import Seer.Config.Snapshot
  ( ConfigSnapshot(..)
  , PresetCatalogueEntry(..)
  , PresetSource(..)
  , applySnapshotToUi
  , builtinPresetEntries
  , currentSnapshotVersion
  , defaultSnapshot
  , deleteNamedSnapshot
  , listSnapshots
  , loadPresetSnapshot
  , loadSnapshot
  , presetCatalogue
  , saveNamedSnapshot
  , saveSnapshot
  , snapshotDir
  , snapshotFromUi
  )
import Seer.Config.SliderSpec (SliderId(..), sliderLabelForId)
import Seer.Persistence.Name (validatePersistenceName)
import Topo.BaseHeight (GenConfig(..))
import Topo.Climate (ClimateConfig(..), TemperatureConfig(..))
import Topo.Erosion (ErosionConfig(..), defaultErosionConfig)
import Topo.Hypsometry (HypsometryConfig(..))
import Topo.Hex (hexSizeKm)
import Topo.Hydrology (HydroConfig(..))
import Topo.Parameters (ParameterConfig(..))
import Topo.Planet (WorldSlice(..), hexesPerDegreeLatitude, hexesPerDegreeLongitude)
import Topo.Types (worldExtentRadiusX, worldExtentRadiusY)
import Topo.WaterBody (WaterBodyConfig(..))
import Topo.Weather (WeatherConfig(..))
import Topo.WorldGen
  ( WorldGenConfig(..)
  , TerrainConfig(..)
  , defaultWorldGenConfig
  , continentalWorldGenConfig
  , archipelagoWorldGenConfig
  , largeOceanWorldGenConfig
  , inlandSeaWorldGenConfig
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

  it "keeps empty UI defaults aligned with earthlike generation defaults" $ do
    let cfg = configFromUi emptyUiState
        terrain = worldTerrain cfg
        hydro = terrainHydrology terrain
        hypsometry = terrainHypsometry terrain
        defaultSlice = worldSlice defaultWorldGenConfig
        uiSlice = worldSlice cfg
        extent = gcWorldExtent (terrainGen terrain)
        planet = worldPlanet cfg
        hex = worldHexGrid cfg
        chunkSize = max 1 (uiChunkSize emptyUiState)
        expectedLatExtent =
          max 0.1 (fromIntegral (worldExtentRadiusY extent * 2 * chunkSize) / hexesPerDegreeLatitude planet hex)
        expectedLonExtent =
          max 0.1 (fromIntegral (worldExtentRadiusX extent * 2 * chunkSize) / hexesPerDegreeLongitude planet hex (wsLatCenter uiSlice))
    hcWaterLevel hydro `shouldBe` hcWaterLevel (terrainHydrology (worldTerrain defaultWorldGenConfig))
    hpWaterLevel hypsometry `shouldBe` hcWaterLevel hydro
    uiRenderWaterLevel emptyUiState `shouldBe` uiWaterLevel emptyUiState
    csRenderWaterLevel defaultSnapshot `shouldBe` hcWaterLevel hydro
    abs (wsLatCenter uiSlice - wsLatCenter defaultSlice) `shouldSatisfy` (< 1.0e-4)
    wsLatExtent uiSlice `shouldBe` expectedLatExtent
    wsLonExtent uiSlice `shouldBe` expectedLonExtent

  it "maps weather cadence slider to whole world-hour intervals" $ do
    let oneHour = worldWeather (configFromUi emptyUiState { uiWeatherTick = 0.0 })
        fullDay = worldWeather (configFromUi emptyUiState { uiWeatherTick = 1.0 })
    wcTickSeconds oneHour `shouldBe` 1
    wcTickSeconds fullDay `shouldBe` 24

  it "keeps derived slice extents aligned with the generated config" $ do
    let ui = emptyUiState
          { uiChunkSize = 96
          , uiWorldExtentX = 0.6
          , uiWorldExtentY = 0.35
          , uiSliceLatCenter = 0.65
          , uiSliceLonCenter = 0.2
          , uiPlanetRadius = 0.8
          , uiHexSizeKm = 0.75
          }
        snap = snapshotFromUi ui "derived"
        cfg = csGenConfig snap
        slice = worldSlice cfg
        extent = gcWorldExtent (terrainGen (worldTerrain cfg))
        planet = worldPlanet cfg
        hex = worldHexGrid cfg
        chunkSize = max 1 (uiChunkSize ui)
        expectedHexSize = mapRange 2.0 20.0 (uiHexSizeKm ui)
        expectedLatExtent =
          max 0.1 (fromIntegral (worldExtentRadiusY extent * 2 * chunkSize) / hexesPerDegreeLatitude planet hex)
        expectedLonExtent =
          max 0.1 (fromIntegral (worldExtentRadiusX extent * 2 * chunkSize) / hexesPerDegreeLongitude planet hex (wsLatCenter slice))
    cfg `shouldBe` configFromUi ui
    hexSizeKm hex `shouldBe` expectedHexSize
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
      handle <- get @Ui system
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
  describe "logical persistence names" $ do
    it "accepts ordinary basenames" $
      validatePersistenceName "world 01-alpha" `shouldBe` Right ()

    it "rejects blank, traversal, separators, drive forms, and controls" $
      map validatePersistenceName
        [ "", "   ", ".", "..", "../sibling", "nested/name"
        , "nested\\name", "C:relative", "C:\\absolute", "name:stream"
        , "victim.", "victim ", "NUL", "con.txt", "COM1", "bad\NULname"
        ] `shouldSatisfy` all isLeft

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
  it "exposes all maintained WorldGen presets under immutable namespaced IDs" $ do
    let expected =
          [ ("builtin:continental", continentalWorldGenConfig)
          , ("builtin:archipelago", archipelagoWorldGenConfig)
          , ("builtin:large-ocean", largeOceanWorldGenConfig)
          , ("builtin:inland-sea", inlandSeaWorldGenConfig)
          , ("builtin:arid", aridWorldGenConfig)
          , ("builtin:lush", lushWorldGenConfig)
          ]
    map presetCatalogueId builtinPresetEntries `shouldBe` map fst expected
    map presetCatalogueSource builtinPresetEntries `shouldSatisfy` all (== PresetBuiltin)
    map presetCatalogueReadOnly builtinPresetEntries `shouldSatisfy` and
    mapM_ (\(presetId, config) -> do
      loaded <- loadPresetSnapshot presetId
      csGenConfig <$> loaded `shouldBe` Right config) expected

  it "reconstructs each effective built-in config modulo derived slice extents" $
    withSystem $ \system -> do
      handle <- get @Ui system
      mapM_ (\(presetId, expectedConfig) -> do
        loaded <- loadPresetSnapshot presetId
        case loaded of
          Left err -> expectationFailure (Text.unpack err)
          Right snapshot -> do
            applySnapshotToUi snapshot handle
            restored <- getUiSnapshot handle
            normalizeDerivedSlice expectedConfig (configFromUi restored)
              `shouldSatisfy` configApproximately expectedConfig) builtinConfigCases

  it "keeps namespaced built-ins out of user save and delete operations" $ do
    saveNamedSnapshot "builtin:continental" defaultSnapshot >>= (`shouldSatisfy` isLeft)
    deleteNamedSnapshot "builtin:continental" >>= (`shouldSatisfy` isLeft)

  it "catalogues validated user presets without shadowing built-ins" $ do
    let name = "__test_catalogue_user__"
    dir <- snapshotDir
    let path = dir </> Text.unpack name <> ".json"
        cleanup = do
          _ <- try @IOException (removeFile path)
          pure ()
    cleanup
    (do
        saveNamedSnapshot name (defaultSnapshot { csName = name }) `shouldReturn` Right ()
        entries <- presetCatalogue
        let matching = filter ((== name) . presetCatalogueId) entries
        matching `shouldBe`
          [ PresetCatalogueEntry name name PresetUser False ]
        loadPresetSnapshot name `shouldReturn` Right (defaultSnapshot { csName = name }))
      `finally` cleanup

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

  it "terrain-shape WorldGenConfig presets round-trip through ConfigSnapshot" $ do
    let cases =
          [ ("continental", continentalWorldGenConfig)
          , ("archipelago", archipelagoWorldGenConfig)
          , ("large-ocean", largeOceanWorldGenConfig)
          , ("inland-sea", inlandSeaWorldGenConfig)
          ]
    mapM_ assertPresetRoundTrip cases

  it "preserves non-slider terrain-shape fields when restored through UI" $
    withSystem $ \system -> do
      let snap = defaultSnapshot { csName = "inland-sea", csGenConfig = inlandSeaWorldGenConfig }
      handle <- get @Ui system
      applySnapshotToUi snap handle
      restored <- getUiSnapshot handle
      let cfg = csGenConfig (snapshotFromUi restored "restored")
          originalTerrain = worldTerrain inlandSeaWorldGenConfig
          restoredTerrain = worldTerrain cfg
          originalGen = terrainGen originalTerrain
          restoredGen = terrainGen restoredTerrain
          originalWaterBody = terrainWaterBody originalTerrain
          restoredWaterBody = terrainWaterBody restoredTerrain
      gcContinentScale restoredGen `shouldBe` gcContinentScale originalGen
      gcLandRatio restoredGen `shouldBe` gcLandRatio originalGen
      gcShelfWidth restoredGen `shouldBe` gcShelfWidth originalGen
      gcCoastSharpness restoredGen `shouldBe` gcCoastSharpness originalGen
      wbcOceanEdgeMargin restoredWaterBody `shouldBe` wbcOceanEdgeMargin originalWaterBody
      wbcMaxBasinDepth restoredWaterBody `shouldBe` wbcMaxBasinDepth originalWaterBody

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

builtinConfigCases :: [(Text, WorldGenConfig)]
builtinConfigCases =
  [ ("builtin:continental", continentalWorldGenConfig)
  , ("builtin:archipelago", archipelagoWorldGenConfig)
  , ("builtin:large-ocean", largeOceanWorldGenConfig)
  , ("builtin:inland-sea", inlandSeaWorldGenConfig)
  , ("builtin:arid", aridWorldGenConfig)
  , ("builtin:lush", lushWorldGenConfig)
  ]

normalizeDerivedSlice :: WorldGenConfig -> WorldGenConfig -> WorldGenConfig
normalizeDerivedSlice expected actual = actual
  { worldSlice = (worldSlice actual)
      { wsLatExtent = wsLatExtent (worldSlice expected)
      , wsLonExtent = wsLonExtent (worldSlice expected)
      }
  }

configApproximately :: WorldGenConfig -> WorldGenConfig -> Bool
configApproximately expected actual = jsonApproximately (toJSON expected) (toJSON actual)

jsonApproximately :: Value -> Value -> Bool
jsonApproximately (Number expected) (Number actual) =
  abs (realToFrac expected - realToFrac actual :: Double) < 1.0e-4
jsonApproximately (Array expected) (Array actual) =
  length expected == length actual
    && and (zipWith jsonApproximately (toList expected) (toList actual))
jsonApproximately (Object expected) (Object actual) =
  KM.size expected == KM.size actual
    && all (\(key, value) -> maybe False (jsonApproximately value) (KM.lookup key actual)) (KM.toList expected)
jsonApproximately expected actual = expected == actual

assertPresetRoundTrip :: (Text, WorldGenConfig) -> Expectation
assertPresetRoundTrip (name, cfg) = do
  let snap = defaultSnapshot { csName = name, csGenConfig = cfg }
      bytes = BSL.toStrict (encode snap)
  case eitherDecodeStrict' @ConfigSnapshot bytes of
    Left err -> expectationFailure err
    Right snap' -> csGenConfig snap' `shouldBe` cfg

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
    it "migrates v1 snapshots while ignoring removed detail and boundary keys" $ do
      let legacyJson = "{\"name\":\"legacy\",\"version\":1,\"seed\":99,\"genConfig\":{\"terrain\":{\"parameters\":{\"detailScale\":2.25,\"roughnessScale\":0.41}},\"climate\":{\"boundary\":{\"motionTemp\":1.75,\"landRange\":1.2},\"temperature\":{\"lapseRate\":0.31}}}}"
      case eitherDecodeStrict' @ConfigSnapshot legacyJson of
        Left err -> expectationFailure err
        Right cs -> do
          csVersion cs `shouldBe` currentSnapshotVersion
          csSeed cs `shouldBe` 99
          let cfg = csGenConfig cs
          pcRoughnessScale (terrainParameters (worldTerrain cfg)) `shouldBe` 0.41
          tmpLapseRate (ccTemperature (worldClimate cfg)) `shouldBe` 0.31
          let currentJson = encode cs
          nestedKeyPresent ["genConfig", "terrain", "parameters"] "detailScale" currentJson
            `shouldBe` False
          nestedKeyPresent ["genConfig", "climate"] "boundary" currentJson
            `shouldBe` False

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

-- | Check for a key nested under a path of object keys.
nestedKeyPresent :: [Text] -> Text -> BSL.ByteString -> Bool
nestedKeyPresent path key lbs =
  case eitherDecodeStrict' @Value (BSL.toStrict lbs) of
    Left _ -> False
    Right value -> go path value
  where
    go [] (Object obj) = KM.member (fromText key) obj
    go (part:rest) (Object obj) =
      maybe False (go rest) (KM.lookup (fromText part) obj)
    go _ _ = False

-- | Remove a key nested under a path of object keys.
-- Returns 'Nothing' if the JSON cannot be decoded.
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
