{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Spec.ConfigPreset (spec) where

import Control.Exception (bracket, try, IOException)
import Data.Aeson (encode, eitherDecodeStrict')
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text
import System.Directory
  ( createDirectoryIfMissing
  , getTemporaryDirectory
  , removeFile
  , removeDirectoryRecursive
  )
import System.FilePath ((</>))
import Test.Hspec
import Test.QuickCheck

import Actor.UI (UiState(..), emptyUiState)
import Seer.Config.Preset
  ( ConfigPreset(..)
  , currentPresetVersion
  , defaultPreset
  , presetFromUi
  , savePreset
  , loadPreset
  , listPresets
  , presetDir
  )

-- ---------------------------------------------------------------------------
-- Spec entry point
-- ---------------------------------------------------------------------------

spec :: Spec
spec = describe "ConfigPreset" $ do
  jsonRoundTripSpec
  presetFromUiSpec
  fileIOSpec
  propertySpec

-- ---------------------------------------------------------------------------
-- JSON round-trip
-- ---------------------------------------------------------------------------

jsonRoundTripSpec :: Spec
jsonRoundTripSpec = describe "JSON round-trip" $ do
  it "round-trips defaultPreset through encode/decode" $ do
    let bytes = BSL.toStrict (encode defaultPreset)
    eitherDecodeStrict' bytes `shouldBe` Right defaultPreset

  it "round-trips a custom preset" $ do
    let custom = defaultPreset
          { cpName       = "custom"
          , cpSeed       = 42
          , cpWaterLevel = 0.7
          , cpGenScale   = 0.9
          , cpUplift     = 0.8
          }
    let bytes = BSL.toStrict (encode custom)
    eitherDecodeStrict' bytes `shouldBe` Right custom

  it "parses empty JSON object with defaults" $ do
    case eitherDecodeStrict' "{}" of
      Left err -> expectationFailure err
      Right cp -> cp `shouldBe` defaultPreset

  it "ignores unknown fields without error" $ do
    let json = "{\"futureField\": 99, \"name\": \"forward\"}"
    case eitherDecodeStrict' json of
      Left err -> expectationFailure err
      Right cp -> cpName cp `shouldBe` "forward"

-- ---------------------------------------------------------------------------
-- presetFromUi
-- ---------------------------------------------------------------------------

presetFromUiSpec :: Spec
presetFromUiSpec = describe "presetFromUi" $ do
  it "captures all slider fields from emptyUiState matching defaultPreset" $ do
    let preset = presetFromUi emptyUiState "test"
    -- Normalise metadata fields so we compare only slider values
    preset { cpName = cpName defaultPreset }
      `shouldBe` defaultPreset

  it "captures custom seed and chunk size" $ do
    let ui = emptyUiState { uiSeed = 12345, uiChunkSize = 128 }
        preset = presetFromUi ui "custom"
    cpSeed preset `shouldBe` 12345
    cpChunkSize preset `shouldBe` 128
    cpName preset `shouldBe` "custom"

-- ---------------------------------------------------------------------------
-- File I/O
-- ---------------------------------------------------------------------------

-- | Use a temporary directory for save/load tests, cleaning up afterwards.
withTempPresetDir :: (FilePath -> IO a) -> IO a
withTempPresetDir action =
  bracket setup teardown action
  where
    setup = do
      tmp <- getTemporaryDirectory
      let dir = tmp </> "topo-test-presets"
      createDirectoryIfMissing True dir
      pure dir
    teardown dir = do
      _ <- try @IOException (removeDirectoryRecursive dir)
      pure ()

fileIOSpec :: Spec
fileIOSpec = describe "File I/O" $ do
  describe "savePreset / loadPreset" $ do
    it "round-trips through a temp file" $ withTempPresetDir $ \dir -> do
      let path = dir </> "round-trip.json"
          preset = defaultPreset { cpName = "round-trip-test" }
      result <- savePreset path preset
      result `shouldBe` Right ()
      loaded <- loadPreset path
      loaded `shouldBe` Right preset

    it "loadPreset returns Left for missing file" $ withTempPresetDir $ \dir -> do
      loaded <- loadPreset (dir </> "nonexistent.json")
      loaded `shouldSatisfy` isLeft

    it "loadPreset returns Left for invalid JSON" $ withTempPresetDir $ \dir -> do
      let path = dir </> "bad.json"
      writeFile path "not json {"
      loaded <- loadPreset path
      loaded `shouldSatisfy` isLeft

  describe "listPresets" $ do
    it "returns sorted names and ignores non-JSON files" $ do
      dir <- presetDir
      -- Write two test presets and a non-JSON file
      let nameA  = "__test_aaa__"
          nameB  = "__test_zzz__"
          pathA  = dir </> (nameA <> ".json")
          pathB  = dir </> (nameB <> ".json")
          pathTx = dir </> "__test_ignore__.txt"
      bracket
        (do savePreset pathA (defaultPreset { cpName = Text.pack nameA })
            savePreset pathB (defaultPreset { cpName = Text.pack nameB })
            writeFile pathTx "not a preset"
        )
        (\_ -> do
            _ <- try @IOException (removeFile pathA)
            _ <- try @IOException (removeFile pathB)
            _ <- try @IOException (removeFile pathTx)
            pure ()
        )
        (\_ -> do
            names <- listPresets
            -- Our test presets should appear in sorted order
            let testNames = filter (\n -> Text.pack "__test_" `Text.isPrefixOf` n) names
            testNames `shouldSatisfy` (\ns ->
              Text.pack nameA `elem` ns && Text.pack nameB `elem` ns)
            -- The .txt file should not appear
            testNames `shouldSatisfy` all (\n -> not (Text.pack "__test_ignore__" `Text.isSuffixOf` n))
            -- Verify sorted
            testNames `shouldSatisfy` isSorted
        )

-- ---------------------------------------------------------------------------
-- QuickCheck property
-- ---------------------------------------------------------------------------

-- | Arbitrary instance for ConfigPreset: all Float fields in [0, 1],
-- realistic seed and chunk size values.
instance Arbitrary ConfigPreset where
  arbitrary = ConfigPreset
    <$> (Text.pack <$> listOf (elements (['a'..'z'] <> ['0'..'9'])))
    <*> pure currentPresetVersion
    <*> arbitrary                   -- seed (Word64)
    <*> choose (8, 256)             -- chunkSize
    <*> unitFloat                   -- waterLevel
    <*> unitFloat                   -- renderWaterLevel
    -- Climate
    <*> unitFloat <*> unitFloat <*> unitFloat         -- evap, rainShadow, windDiffuse
    <*> unitFloat <*> unitFloat <*> unitFloat         -- equatorTemp, poleTemp, lapseRate
    <*> unitFloat <*> unitFloat                       -- windIter, moistureIter
    <*> unitFloat <*> unitFloat                       -- boundaryMotionTemp, boundaryMotionPrecip
    -- Temperature
    <*> unitFloat <*> unitFloat <*> unitFloat         -- latitudeExponent, plateHeightCooling, tempNoiseScale
    <*> unitFloat <*> unitFloat                       -- oceanModeration, oceanModerateTemp
    <*> unitFloat <*> unitFloat                       -- albedoSensitivity, albedoReference
    -- Moisture
    <*> unitFloat <*> unitFloat                       -- moistAdvect, moistLocal
    <*> unitFloat <*> unitFloat                       -- moistWindEvapScale, moistEvapNoiseScale
    <*> unitFloat <*> unitFloat                       -- moistLandETCoeff, moistBareEvapFrac
    <*> unitFloat <*> unitFloat                       -- moistVegTranspFrac, moistWindETScale
    <*> unitFloat <*> unitFloat                       -- moistCondensationRate, moistRecycleRate
    <*> unitFloat <*> unitFloat                       -- moistITCZStrength, moistITCZWidth
    -- Precipitation
    <*> unitFloat <*> unitFloat                       -- orographicScale, orographicStep
    <*> unitFloat <*> unitFloat <*> unitFloat         -- coastalIterations, coastalDiffuse, coastalMoistureBoost
    -- Wind
    <*> unitFloat <*> unitFloat <*> unitFloat         -- windBeltStrength, windBeltHarmonics, windBeltBase
    <*> unitFloat <*> unitFloat                       -- windBeltRange, windBeltSpeedScale
    -- Boundary model
    <*> unitFloat <*> unitFloat <*> unitFloat         -- bndLandRange, bndTempConvergent, bndTempDivergent
    <*> unitFloat                                     -- bndTempTransform
    <*> unitFloat <*> unitFloat <*> unitFloat         -- bndPrecipConvergent, bndPrecipDivergent, bndPrecipTransform
    -- Erosion
    <*> unitFloat                                     -- rainRate
    <*> unitFloat <*> unitFloat <*> unitFloat <*> unitFloat  -- erosionHydraulic, erosionThermal, erosionTalus, erosionMaxDrop
    -- Glacier
    <*> unitFloat <*> unitFloat <*> unitFloat         -- glacierSnowTemp, glacierSnowRange, glacierMeltTemp
    <*> unitFloat <*> unitFloat <*> unitFloat         -- glacierMeltRate, glacierAccumScale, glacierFlowIters
    <*> unitFloat <*> unitFloat                       -- glacierFlowRate, glacierErosionScale
    <*> unitFloat <*> unitFloat                       -- glacierCarveScale, glacierDepositScale
    -- Volcanism
    <*> unitFloat <*> unitFloat                       -- ventDensity, ventThreshold
    <*> unitFloat <*> unitFloat                       -- hotspotScale, hotspotThreshold
    <*> unitFloat <*> unitFloat                       -- magmaRecharge, lavaScale
    <*> unitFloat <*> unitFloat                       -- ashScale, volcanicDepositScale
    -- Soil
    <*> unitFloat <*> unitFloat                       -- soilMoistureThreshold, soilHardnessThreshold
    <*> unitFloat <*> unitFloat                       -- soilFertilityMoistWeight, soilFertilityDepthWeight
    -- Hydrology / WaterBody / Parameters
    <*> unitFloat <*> unitFloat                       -- sinkBreachDepth, streamPowerMaxErosion
    <*> unitFloat <*> unitFloat                       -- riverCarveMaxDepth, coastalErodeStrength
    <*> unitFloat <*> unitFloat                       -- hydroHardnessWeight, minLakeSize
    <*> unitFloat <*> unitFloat                       -- inlandSeaMinSize, roughnessScale
    -- Generation
    <*> unitFloat <*> unitFloat                       -- genScale, genCoordScale
    <*> unitFloat <*> unitFloat                       -- genOffsetX, genOffsetY
    <*> unitFloat <*> unitFloat <*> unitFloat <*> unitFloat  -- genFreq, genOct, genLac, genGain
    <*> unitFloat <*> unitFloat                       -- genWarpScale, genWarpStrength
    <*> unitFloat <*> unitFloat                       -- worldExtentX, worldExtentY
    -- Ocean edge depth
    <*> unitFloat <*> unitFloat <*> unitFloat <*> unitFloat <*> unitFloat  -- edgeDepth N/S/E/W + falloff
    -- Tectonics
    <*> unitFloat <*> unitFloat                       -- plateSize, plateSpeed
    <*> unitFloat <*> unitFloat <*> unitFloat         -- boundarySharp, noiseScale, noiseStrength
    <*> unitFloat <*> unitFloat <*> unitFloat         -- warpOct, warpLac, warpGain
    <*> unitFloat <*> unitFloat                       -- plateMergeScale, plateMergeBias
    <*> unitFloat <*> unitFloat <*> unitFloat         -- plateDetailScale, plateDetailStrength, plateRidgeStrength
    <*> unitFloat <*> unitFloat                       -- plateHeightBase, plateHeightVariance
    <*> unitFloat <*> unitFloat                       -- plateHardnessBase, plateHardnessVariance
    <*> unitFloat <*> unitFloat <*> unitFloat <*> unitFloat  -- uplift, riftDepth, trenchDepth, ridgeHeight
    <*> unitFloat                                     -- detailScale
    <*> unitFloat <*> unitFloat <*> unitFloat         -- plateBiasStrength, plateBiasCenter, plateBiasEdge
    <*> unitFloat <*> unitFloat                       -- plateBiasNorth, plateBiasSouth
    <*> unitFloat <*> unitFloat <*> unitFloat         -- tfcCliffSlope, tfcMountainSlope, tfcMountainRelief
    <*> unitFloat <*> unitFloat <*> unitFloat         -- tfcHillSlope, tfcRollingSlope, valleyCurvature
    <*> unitFloat <*> unitFloat <*> unitFloat         -- rockElevThreshold, rockHardThreshold, rockHardSecondary
    -- Weather
    <*> unitFloat <*> unitFloat <*> unitFloat         -- weatherTick, weatherPhase, weatherAmplitude
    <*> unitFloat <*> unitFloat                       -- seasonCycleLength, jitterAmplitude
    <*> unitFloat <*> unitFloat <*> unitFloat         -- pressureBase, pressureTempScale, pressureCoriolisScale
    <*> unitFloat <*> unitFloat                       -- seasonalBase, seasonalRange
    <*> unitFloat <*> unitFloat                       -- humidityNoiseScale, precipNoiseScale
    <*> unitFloat <*> unitFloat                       -- weatherITCZWidth, weatherITCZPrecipBoost
    <*> unitFloat <*> unitFloat                       -- pressureHumidityScale, pressureGradientWindScale
    <*> unitFloat <*> unitFloat                       -- windNoiseScale, itczMigrationScale
    <*> unitFloat <*> unitFloat <*> unitFloat         -- cloudRHExponent, cloudAlbedoEffect, cloudPrecipBoost
    -- Vegetation
    <*> unitFloat <*> unitFloat                       -- vegBase, vegBoost
    <*> unitFloat <*> unitFloat                       -- vegTempWeight, vegPrecipWeight
    -- Biome Thresholds
    <*> unitFloat <*> unitFloat <*> unitFloat         -- btCoastalBand, btSnowElevation, btAlpineElevation
    <*> unitFloat <*> unitFloat <*> unitFloat         -- btIceCapTemp, btMontaneLow, btMontanePrecip
    <*> unitFloat <*> unitFloat                       -- btCliffSlope, btValleyMoisture
    <*> unitFloat <*> unitFloat                       -- btDepressionMoisture, btPrecipWeight
    -- Vegetation Bootstrap
    <*> unitFloat <*> unitFloat <*> unitFloat         -- vbcTempMin, vbcTempRange, vbcFertilityBoost
    <*> unitFloat <*> unitFloat                       -- vbcAlbedoBase, vbcAlbedoBare
    <*> unitFloat <*> unitFloat <*> unitFloat         -- vbcAlbedoVeg, vbcOceanAlbedo, vbcIceAlbedo
    -- Biome Misc
    <*> unitFloat <*> unitFloat                       -- biomeSmoothing, volcanicAshBoost
    <*> unitFloat <*> unitFloat                       -- volcanicLavaPenalty, biomeFeedbackBlend
    -- Planet
    <*> unitFloat <*> unitFloat <*> unitFloat         -- planetRadius, axialTilt, insolation
    -- Ocean currents
    <*> unitFloat <*> unitFloat                       -- occWarmScale, occColdScale
    <*> unitFloat <*> unitFloat                       -- occLatPeakDeg, occLatWidthDeg
    -- World slice
    <*> unitFloat <*> unitFloat                       -- sliceLatCenter, sliceLonCenter

propertySpec :: Spec
propertySpec = describe "QuickCheck" $
  it "any ConfigPreset round-trips through JSON" $
    property $ \cp ->
      let bytes = BSL.toStrict (encode (cp :: ConfigPreset))
      in eitherDecodeStrict' bytes === Right cp

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Generate a Float in [0, 1].
unitFloat :: Gen Float
unitFloat = choose (0, 1)

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [_] = True
isSorted (x:y:rest) = x <= y && isSorted (y:rest)
