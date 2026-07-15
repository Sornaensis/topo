{-# LANGUAGE OverloadedStrings #-}

module Spec.Export (spec) where

import qualified Data.ByteString as BS
import Test.Hspec
import qualified Data.Text as Text
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8, Word16)
import Topo
import Topo.Plugin.RPC (encodeBase64Text)

spec :: Spec
spec = describe "Export" $ do
  it "defines canonical basis-qualified weather/climate export fields and legacy aliases" $ do
    canonicalBasisQualifiedExportFields `shouldSatisfy` (\fields -> all (`elem` fields)
      [ "climate_temp_avg"
      , "climate_precip_avg"
      , "weather_temp_current"
      , "weather_precip_current"
      , "weather_cloud_cover_current"
      , "weather_cloud_cover_typical"
      ])
    legacyBasisExportAliases `shouldSatisfy` (\aliases -> all (`elem` aliases)
      [ ("temperature", "climate_temp_avg")
      , ("precipitation", "climate_precip_avg")
      , ("weather_temperature", "weather_temp_current")
      , ("cloud_cover", "weather_cloud_cover_current")
      ])
    map fst legacyBasisExportAliases `shouldSatisfy` all (not . Text.isSuffixOf "_current")

  it "keeps the protocol-v4 little-endian biome/base64 fixture stable" $ do
    let config = WorldConfig { wcChunkSize = 1 }
    case encodeBiomeChunk config (emptyTerrainChunk config) of
      Left err -> expectationFailure (show err)
      Right bytes -> do
        bytes `shouldBe` BS.pack [1, 0, 0, 0, 0, 0]
        encodeBase64Text bytes `shouldBe` "AQAAAAAA"

  it "round-trips every plugin terrain-layer binary decoder without lists" $ do
    let config = WorldConfig { wcChunkSize = 2 }
        n = chunkTileCount config
        zeros = U.replicate n 0
        river = (emptyRiverChunk config)
          { rcSegOffsets = U.fromList [0, 1, 1, 1, 1]
          , rcSegEntryEdge = U.singleton 255
          , rcSegExitEdge = U.singleton 0
          , rcSegDischarge = U.singleton 0.75
          , rcSegOrder = U.singleton 2
          }
        groundwater = (emptyGroundwaterChunk config)
          { gwInfiltration = U.replicate n 0.25
          , gwWaterTableDepth = U.replicate n 0.5
          , gwRootZoneMoisture = U.replicate n 0.75
          }
        weather = WeatherChunk
          { wcTemp = zeros, wcHumidity = zeros, wcWindDir = zeros
          , wcWindSpd = zeros, wcPressure = zeros, wcPrecip = zeros
          , wcCloudCover = zeros, wcCloudWater = zeros
          , wcCloudCoverLow = zeros, wcCloudCoverMid = zeros
          , wcCloudCoverHigh = zeros, wcCloudWaterLow = zeros
          , wcCloudWaterMid = zeros, wcCloudWaterHigh = zeros
          }
    assertChunkRoundTrip config encodeTerrainChunk decodeTerrainChunk (emptyTerrainChunk config)
    assertChunkRoundTrip config encodeClimateChunk decodeClimateChunk (emptyClimateChunk config)
    assertChunkRoundTrip config encodeWeatherChunk decodeWeatherChunk weather
    assertChunkRoundTrip config encodeRiverChunk decodeRiverChunk river
    assertChunkRoundTrip config encodeGroundwaterChunk decodeGroundwaterChunk (emptyGroundwaterChunk config)
    assertChunkRoundTrip config encodeGroundwaterChunk decodeGroundwaterChunk groundwater
    assertChunkRoundTrip config encodeVolcanismChunk decodeVolcanismChunk (emptyVolcanismChunk config)
    assertChunkRoundTrip config encodeGlacierChunk decodeGlacierChunk (emptyGlacierChunk config)
    assertChunkRoundTrip config encodeVegetationChunk decodeVegetationChunk (emptyVegetationChunk config)
    assertChunkRoundTrip config encodeWaterBodyChunk decodeWaterBodyChunk (emptyWaterBodyChunk config)

  it "preserves every mapped-enum decode failure" $ do
    let config = WorldConfig { wcChunkSize = 1 }
    terrainBytes <- expectEncoded (encodeTerrainChunk config (emptyTerrainChunk config))
    assertDecodeFailure "invalid terrain form" decodeTerrainChunk config
      (replaceWord8 88 255 terrainBytes)
    assertDecodeFailure "invalid biome id" decodeTerrainChunk config
      (replaceWord16 89 65535 terrainBytes)
    assertDecodeFailure "invalid plate boundary" decodeTerrainChunk config
      (replaceWord16 93 65535 terrainBytes)
    volcanismBytes <- expectEncoded (encodeVolcanismChunk config (emptyVolcanismChunk config))
    assertDecodeFailure "invalid vent type" decodeVolcanismChunk config
      (replaceWord16 4 65535 volcanismBytes)
    assertDecodeFailure "invalid vent activity" decodeVolcanismChunk config
      (replaceWord16 6 65535 volcanismBytes)
    waterBodyBytes <- expectEncoded (encodeWaterBodyChunk config (emptyWaterBodyChunk config))
    assertDecodeFailure "invalid water body type" decodeWaterBodyChunk config
      (replaceWord8 4 255 waterBodyBytes)
    assertDecodeFailure "invalid water body type" decodeWaterBodyChunk config
      (replaceWord8 17 255 waterBodyBytes)

  it "selects chunks that intersect a region" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        world0 = emptyWorld config defaultHexGridMeta
        chunkA = generateTerrainChunk config (\_ -> 1)
        chunkB = generateTerrainChunk config (\_ -> 2)
        world1 = setTerrainChunk (chunkIdFromCoord (ChunkCoord 0 0)) chunkA
                  (setTerrainChunk (chunkIdFromCoord (ChunkCoord 1 0)) chunkB world0)
        region = RegionRect (TileCoord 0 0) (TileCoord 5 3)
    case exportTerrainChunksRegion world1 region of
      Left err -> expectationFailure (show err)
      Right exported -> do
        let ids = map fst exported
        ids `shouldContain` [chunkIdFromCoord (ChunkCoord 0 0), chunkIdFromCoord (ChunkCoord 1 0)]
        length ids `shouldBe` 2

  it "exports biome chunks" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        world0 = emptyWorld config defaultHexGridMeta
        terrain = (emptyTerrainChunk config) { tcFlags = U.replicate (chunkTileCount config) BiomeForest }
        world1 = setTerrainChunk (ChunkId 0) terrain world0
    case exportBiomeChunks world1 of
      Left err -> expectationFailure (show err)
      Right exported -> length exported `shouldBe` 1

  it "keeps climate chunk exports consistent" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        world0 = emptyWorld config defaultHexGridMeta
        climate = (emptyClimateChunk config) { ccTempAvg = U.replicate (chunkTileCount config) 5 }
        world1 = setClimateChunk (ChunkId 0) climate world0
        region = RegionRect (TileCoord 0 0) (TileCoord 1 1)
    case (exportClimateChunks world1, exportClimateChunksRegion world1 region) of
      (Right full, Right regionExport) ->
        lookup (ChunkId 0) regionExport `shouldBe` lookup (ChunkId 0) full
      (Left err, _) -> expectationFailure (show err)
      (_, Left err) -> expectationFailure (show err)

assertChunkRoundTrip
  :: (Eq a, Show a)
  => WorldConfig
  -> (WorldConfig -> a -> Either ExportError BS.ByteString)
  -> (WorldConfig -> BS.ByteString -> Either ExportError a)
  -> a
  -> Expectation
assertChunkRoundTrip config encodeChunk decodeChunk chunk =
  case encodeChunk config chunk >>= decodeChunk config of
    Left err -> expectationFailure (show err)
    Right decoded -> decoded `shouldBe` chunk

expectEncoded :: Either ExportError BS.ByteString -> IO BS.ByteString
expectEncoded (Left err) = expectationFailure (show err) >> fail "encode"
expectEncoded (Right bytes) = pure bytes

assertDecodeFailure
  :: Show a
  => Text.Text
  -> (WorldConfig -> BS.ByteString -> Either ExportError a)
  -> WorldConfig
  -> BS.ByteString
  -> Expectation
assertDecodeFailure expected decodeChunk config bytes =
  case decodeChunk config bytes of
    Left (ExportDecodeError message) -> message `shouldSatisfy` Text.isInfixOf expected
    other -> expectationFailure ("expected mapped decode failure, got " <> show other)

replaceWord8 :: Int -> Word8 -> BS.ByteString -> BS.ByteString
replaceWord8 offset value bytes =
  BS.take offset bytes <> BS.singleton value <> BS.drop (offset + 1) bytes

replaceWord16 :: Int -> Word16 -> BS.ByteString -> BS.ByteString
replaceWord16 offset value bytes =
  BS.take offset bytes
    <> BS.pack [fromIntegral value, fromIntegral (value `div` 256)]
    <> BS.drop (offset + 2) bytes
