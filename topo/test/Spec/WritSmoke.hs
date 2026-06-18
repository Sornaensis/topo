{-# LANGUAGE OverloadedStrings #-}

module Spec.WritSmoke (spec) where

import Data.Aeson (Value(..), toJSON)
import Data.Aeson.Key (Key)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector.Unboxed as U
import qualified Data.Text as Text
import Data.Word (Word64)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

import Topo
import Topo.Persistence.WorldBundle
  ( BundleLoadPolicy(..)
  , loadWorldBundle
  , saveWorldBundle
  )
import Topo.Plugin.RPC (terrainWorldToCompletePayload)

spec :: Spec
spec = describe "Writ worldbuilding smoke workflow" $ do
  it "generates, persists, inspects, and exports a deterministic Writ seed" $
    withSystemTempDirectory "topo-writ-smoke" $ \tmp -> do
      worldA <- generateWritSmokeWorld
      worldB <- generateWritSmokeWorld

      smokeFingerprint worldA `shouldBe` smokeFingerprint worldB
      inspectWritFields worldA
      assertEarthlikeBalance worldA
      assertWritPayload worldA

      let topoPath = tmp </> "writ-smoke.topo"
      saveResult <- saveWorldBundle topoPath worldA
      case saveResult of
        Left err -> expectationFailure ("save failed: " <> show err)
        Right () -> pure ()

      loadResult <- loadWorldBundle StrictManifest topoPath
      loaded <- expectRight "load failed" loadResult
      twSeed loaded `shouldBe` writSmokeSeed
      twGenConfig loaded `shouldBe` Just (toJSON writSmokeConfig)
      smokeFingerprint loaded `shouldBe` smokeFingerprint worldA
      inspectWritFields loaded
      assertEarthlikeBalance loaded
      assertWritPayload loaded

writSmokeSeed :: Word64
writSmokeSeed = 0x57524954

writWorldConfig :: WorldConfig
writWorldConfig = WorldConfig { wcChunkSize = 32 }

writExpectedChunkCount :: Int
writExpectedChunkCount = 81

writSmokeConfig :: WorldGenConfig
writSmokeConfig = defaultWorldGenConfig

writSmokeSlice :: WorldSlice
writSmokeSlice = worldSlice writSmokeConfig

generateWritSmokeWorld :: IO TerrainWorld
generateWritSmokeWorld = do
  let pipeline = buildFullPipelineConfig writSmokeConfig writWorldConfig writSmokeSeed
      env = TopoEnv { teLogger = \_ -> pure () }
      world0 = emptyWorldWithPlanet
        writWorldConfig
        (worldHexGrid writSmokeConfig)
        (worldPlanet writSmokeConfig)
        (worldSlice writSmokeConfig)
  result <- runPipeline pipeline env world0
  (world, _) <- expectRight "pipeline failed" result
  pure world
    { twGenConfig = Just (toJSON writSmokeConfig)
    }

inspectWritFields :: TerrainWorld -> IO ()
inspectWritFields world = do
  (chunkKey, terrain) <- expectJust "expected generated terrain chunk" $
    IntMap.lookupMin (twTerrain world)
  IntMap.size (twTerrain world) `shouldBe` writExpectedChunkCount
  wcChunkSize (twConfig world) `shouldBe` wcChunkSize writWorldConfig

  let chunkId = ChunkId chunkKey
      expectedTiles = chunkTileCount (twConfig world)

  U.length (tcElevation terrain) `shouldBe` expectedTiles
  U.length (tcMoisture terrain) `shouldBe` expectedTiles
  U.length (tcFlags terrain) `shouldBe` expectedTiles
  assertFiniteVector "terrain elevation" (tcElevation terrain)
  assertFiniteVector "terrain moisture" (tcMoisture terrain)

  climate <- expectJust "expected climate chunk" $
    IntMap.lookup chunkKey (twClimate world)
  U.length (ccTempAvg climate) `shouldBe` expectedTiles
  U.length (ccPrecipAvg climate) `shouldBe` expectedTiles
  assertFiniteVector "climate temperature" (ccTempAvg climate)
  assertFiniteVector "climate precipitation" (ccPrecipAvg climate)

  river <- expectJust "expected hydrology river chunk" $
    IntMap.lookup chunkKey (twRivers world)
  U.length (rcFlowAccum river) `shouldBe` expectedTiles
  U.length (rcDischarge river) `shouldBe` expectedTiles
  assertFiniteVector "river flow accumulation" (rcFlowAccum river)
  assertFiniteVector "river discharge" (rcDischarge river)

  groundwater <- expectJust "expected groundwater chunk" $
    IntMap.lookup chunkKey (twGroundwater world)
  U.length (gwWaterTableDepth groundwater) `shouldBe` expectedTiles
  U.length (gwRootZoneMoisture groundwater) `shouldBe` expectedTiles
  assertFiniteVector "groundwater depth" (gwWaterTableDepth groundwater)
  assertFiniteVector "groundwater root-zone moisture" (gwRootZoneMoisture groundwater)

  water <- expectJust "expected water body chunk" $
    IntMap.lookup chunkKey (twWaterBodies world)
  U.length (wbType water) `shouldBe` expectedTiles

  vegetation <- expectJust "expected vegetation chunk" $
    IntMap.lookup chunkKey (twVegetation world)
  U.length (vegCover vegetation) `shouldBe` expectedTiles
  assertFiniteVector "vegetation cover" (vegCover vegetation)

  weather <- expectJust "expected weather overlay chunk" $
    getWeatherChunk chunkId world
  U.length (wcTemp weather) `shouldBe` expectedTiles
  U.length (wcPrecip weather) `shouldBe` expectedTiles
  assertFiniteVector "weather temperature" (wcTemp weather)
  assertFiniteVector "weather precipitation" (wcPrecip weather)

assertEarthlikeBalance :: TerrainWorld -> IO ()
assertEarthlikeBalance world = do
  let waterTypes = concatMap (U.toList . wbType) (IntMap.elems (twWaterBodies world))
      totalTiles = length waterTypes
      waterTiles = length (filter (/= WaterDry) waterTypes)
      landTiles = totalTiles - waterTiles
      fraction n = fromIntegral n / fromIntegral (max 1 totalTiles) :: Double
      temps = concatMap (U.toList . ccTempAvg) (IntMap.elems (twClimate world))
      precips = concatMap (U.toList . ccPrecipAvg) (IntMap.elems (twClimate world))
  totalTiles `shouldSatisfy` (> 0)
  fraction landTiles `shouldSatisfy` (> 0.25)
  fraction waterTiles `shouldSatisfy` (> 0.25)
  valueSpan temps `shouldSatisfy` (> 0.30)
  valueSpan precips `shouldSatisfy` (> 0.40)

assertWritPayload :: TerrainWorld -> IO ()
assertWritPayload world = do
  payload <- expectRight "complete terrain payload export failed" (terrainWorldToCompletePayload world)
  case payload of
    Object obj -> do
      KM.lookup "chunk_count" obj `shouldBe` Just (toJSON (IntMap.size (twTerrain world)))
      KM.lookup "climate_count" obj `shouldBe` Just (toJSON (IntMap.size (twClimate world)))
      KM.lookup "river_count" obj `shouldBe` Just (toJSON (IntMap.size (twRivers world)))
      KM.lookup "groundwater_count" obj `shouldBe` Just (toJSON (IntMap.size (twGroundwater world)))
      KM.lookup "volcanism_count" obj `shouldBe` Just (toJSON (IntMap.size (twVolcanism world)))
      KM.lookup "glacier_count" obj `shouldBe` Just (toJSON (IntMap.size (twGlaciers world)))
      KM.lookup "water_body_count" obj `shouldBe` Just (toJSON (IntMap.size (twWaterBodies world)))
      KM.lookup "vegetation_count" obj `shouldBe` Just (toJSON (IntMap.size (twVegetation world)))
      KM.lookup "biome_count" obj `shouldBe` Just (toJSON (IntMap.size (twTerrain world)))
      KM.lookup "weather_count" obj `shouldBe` Just (toJSON (IntMap.size (getWeatherFromOverlay world)))
      KM.lookup "overlay_count" obj `shouldBe` Just (toJSON (length (overlayNames (twOverlays world))))
      KM.lookup "chunk_size" obj `shouldBe` Just (toJSON (wcChunkSize (twConfig world)))
      KM.lookup "seed" obj `shouldBe` Just (toJSON (twSeed world))
      KM.lookup "encoding" obj `shouldBe` Just (String "base64")
      assertObjectField "world_time" obj
      assertObjectField "planet_age" obj
      assertUnitScales obj
      assertObjectField "gen_config" obj
      assertObjectField "metadata" obj
      assertOverlaySection obj
      assertEncodedChunkSection "terrain" obj
      assertEncodedChunkSection "climate" obj
      assertEncodedChunkSection "rivers" obj
      assertEncodedChunkSection "groundwater" obj
      assertEncodedChunkSection "volcanism" obj
      assertEncodedChunkSection "glaciers" obj
      assertEncodedChunkSection "water_bodies" obj
      assertEncodedChunkSection "vegetation" obj
      assertEncodedChunkSection "biomes" obj
      assertEncodedChunkSection "weather" obj
    _ -> expectationFailure "expected terrain payload object"

smokeFingerprint
  :: TerrainWorld
  -> Maybe
       ( U.Vector Float
       , U.Vector BiomeId
       , U.Vector Float
       , U.Vector Float
       , U.Vector WaterBodyType
       , U.Vector Float
       , U.Vector Float
       )
smokeFingerprint world = do
  (chunkKey, terrain) <- IntMap.lookupMin (twTerrain world)
  climate <- IntMap.lookup chunkKey (twClimate world)
  river <- IntMap.lookup chunkKey (twRivers world)
  water <- IntMap.lookup chunkKey (twWaterBodies world)
  vegetation <- IntMap.lookup chunkKey (twVegetation world)
  weather <- getWeatherChunk (ChunkId chunkKey) world
  pure
    ( tcElevation terrain
    , tcFlags terrain
    , ccTempAvg climate
    , rcFlowAccum river
    , wbType water
    , vegCover vegetation
    , wcTemp weather
    )

expectRight :: Show e => String -> Either e a -> IO a
expectRight _ (Right value) = pure value
expectRight label (Left err) = fail (label <> ": " <> show err)

expectJust :: String -> Maybe a -> IO a
expectJust _ (Just value) = pure value
expectJust label Nothing = fail label

valueSpan :: [Float] -> Float
valueSpan [] = 0
valueSpan values = maximum values - minimum values

assertFiniteVector :: String -> U.Vector Float -> IO ()
assertFiniteVector label values =
  if U.all isFinite values
    then pure ()
    else expectationFailure ("expected finite values for " <> label)
  where
    isFinite x = not (isNaN x || isInfinite x)

assertObjectField :: Key -> KM.KeyMap Value -> IO ()
assertObjectField key obj =
  case KM.lookup key obj of
    Just (Object _) -> pure ()
    _ -> expectationFailure ("expected payload object field: " <> show key)

assertUnitScales :: KM.KeyMap Value -> IO ()
assertUnitScales obj =
  case KM.lookup "unit_scales" obj of
    Just (Object scales) ->
      KM.lookup "water_level" scales `shouldBe`
        Just (toJSON (hcWaterLevel (terrainHydrology (worldTerrain writSmokeConfig))))
    Just _ -> expectationFailure "expected unit_scales object"
    Nothing -> expectationFailure "missing unit_scales"

assertOverlaySection :: KM.KeyMap Value -> IO ()
assertOverlaySection obj =
  case KM.lookup "overlays" obj of
    Just (Object overlays) -> do
      KM.member "weather" overlays `shouldBe` True
      case KM.lookup "weather" overlays of
        Just (Object weatherOverlay) -> do
          assertObjectField "schema" weatherOverlay
          assertObjectField "payload" weatherOverlay
          assertObjectField "provenance" weatherOverlay
        _ -> expectationFailure "expected weather overlay payload"
    _ -> expectationFailure "expected overlays payload object"

assertEncodedChunkSection :: Key -> KM.KeyMap Value -> IO ()
assertEncodedChunkSection key obj =
  case KM.lookup key obj of
    Just (Object chunks) -> do
      KM.size chunks `shouldBe` writExpectedChunkCount
      mapM_ assertEncodedChunk (KM.elems chunks)
    _ -> expectationFailure ("expected payload object section: " <> show key)

assertEncodedChunk :: Value -> IO ()
assertEncodedChunk (String encoded) = encoded `shouldSatisfy` (not . Text.null)
assertEncodedChunk _ = expectationFailure "expected base64 chunk string"
