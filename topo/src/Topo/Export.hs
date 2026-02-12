module Topo.Export
  ( ExportError(..)
  , encodeTerrainChunk
  , decodeTerrainChunk
  , decodeTerrainChunkV2
  , encodeClimateChunk
  , decodeClimateChunk
  , decodeClimateChunkV1
  , encodeWeatherChunk
  , decodeWeatherChunk
  , encodeRiverChunk
  , decodeRiverChunk
  , decodeRiverChunkV2
  , decodeRiverChunkV1
  , encodeGroundwaterChunk
  , decodeGroundwaterChunk
  , encodeVolcanismChunk
  , decodeVolcanismChunk
  , encodeGlacierChunk
  , decodeGlacierChunk
  , encodeVegetationChunk
  , decodeVegetationChunk
  , decodeVegetationChunkV1
  , encodeWaterBodyChunk
  , decodeWaterBodyChunk
  , exportTerrainChunks
  , exportTerrainChunksRegion
  , exportClimateChunks
  , exportClimateChunksRegion
  , exportWeatherChunks
  , exportWeatherChunksRegion
  , exportRiverChunks
  , exportRiverChunksRegion
  , exportGroundwaterChunks
  , exportGroundwaterChunksRegion
  , exportVolcanismChunks
  , exportVolcanismChunksRegion
  , exportGlacierChunks
  , exportGlacierChunksRegion
  , exportVegetationChunks
  , exportVegetationChunksRegion
  , exportWaterBodyChunks
  , exportWaterBodyChunksRegion
  , exportBiomeChunks
  , exportBiomeChunksRegion
  , encodeBiomeChunk
  , chunksForRegion
  ) where

import Control.Monad (replicateM)
import Data.Binary.Get (Get, getFloatle, getInt32le, getWord8, getWord16le, getWord32le, runGetOrFail)
import Data.Binary.Put (Put, putFloatle, putInt32le, putWord8, putWord16le, putWord32le, runPut)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8, Word16, Word32)
import qualified Data.Vector.Unboxed as U
import Topo.Types
import Topo.World (TerrainWorld(..))

-- | Encoding/decoding failures during chunk export.
data ExportError
  = ExportLengthMismatch
      { eeLabel :: !Text
      , eeExpected :: !Int
      , eeActual :: !Int
      }
  | ExportDecodeError !Text
  deriving (Eq, Show)

-- | Encode a terrain chunk.
--
-- Terrain chunk schema (version 4+ in world files):
--   elementCount, elevation, slope, curvature, hardness, rockType,
--   soilType, soilDepth, moisture, fertility, roughness, rockDensity,
--   soilGrain, relief, ruggedness, terrainForm, biomeFlags, plateId,
--   plateBoundary, plateHeight, plateHardness, plateCrust, plateAge,
--   plateVelX, plateVelY.
--
-- Version 3 lacks relief/ruggedness/terrainForm fields and is handled
-- by getTerrainChunkV3.
--
-- Version 2 lacks plate crust/age/velocity fields and is handled by
-- getTerrainChunkV2 when loading legacy files.
encodeTerrainChunk :: WorldConfig -> TerrainChunk -> Either ExportError BS.ByteString
encodeTerrainChunk config chunk = do
  let n = chunkTileCount config
  ensureLength n (Text.pack "elevation") (tcElevation chunk)
  ensureLength n (Text.pack "slope") (tcSlope chunk)
  ensureLength n (Text.pack "curvature") (tcCurvature chunk)
  ensureLength n (Text.pack "hardness") (tcHardness chunk)
  ensureLength n (Text.pack "rockType") (tcRockType chunk)
  ensureLength n (Text.pack "soilType") (tcSoilType chunk)
  ensureLength n (Text.pack "soilDepth") (tcSoilDepth chunk)
  ensureLength n (Text.pack "moisture") (tcMoisture chunk)
  ensureLength n (Text.pack "fertility") (tcFertility chunk)
  ensureLength n (Text.pack "roughness") (tcRoughness chunk)
  ensureLength n (Text.pack "rockDensity") (tcRockDensity chunk)
  ensureLength n (Text.pack "soilGrain") (tcSoilGrain chunk)
  ensureLength n (Text.pack "relief") (tcRelief chunk)
  ensureLength n (Text.pack "ruggedness") (tcRuggedness chunk)
  ensureLength n (Text.pack "terrainForm") (tcTerrainForm chunk)
  ensureLength n (Text.pack "flags") (tcFlags chunk)
  ensureLength n (Text.pack "plateId") (tcPlateId chunk)
  ensureLength n (Text.pack "plateBoundary") (tcPlateBoundary chunk)
  ensureLength n (Text.pack "plateHeight") (tcPlateHeight chunk)
  ensureLength n (Text.pack "plateHardness") (tcPlateHardness chunk)
  ensureLength n (Text.pack "plateCrust") (tcPlateCrust chunk)
  ensureLength n (Text.pack "plateAge") (tcPlateAge chunk)
  ensureLength n (Text.pack "plateVelX") (tcPlateVelX chunk)
  ensureLength n (Text.pack "plateVelY") (tcPlateVelY chunk)
  pure $ BL.toStrict $ runPut $ do
    putWord32le (fromIntegral n)
    putVectorFloat n (tcElevation chunk)
    putVectorFloat n (tcSlope chunk)
    putVectorFloat n (tcCurvature chunk)
    putVectorFloat n (tcHardness chunk)
    putVectorWord16 n (tcRockType chunk)
    putVectorWord16 n (tcSoilType chunk)
    putVectorFloat n (tcSoilDepth chunk)
    putVectorFloat n (tcMoisture chunk)
    putVectorFloat n (tcFertility chunk)
    putVectorFloat n (tcRoughness chunk)
    putVectorFloat n (tcRockDensity chunk)
    putVectorFloat n (tcSoilGrain chunk)
    putVectorFloat n (tcRelief chunk)
    putVectorFloat n (tcRuggedness chunk)
    putVectorWord8 n (U.map terrainFormToCode (tcTerrainForm chunk))
    putVectorWord16 n (U.map biomeIdToCode (tcFlags chunk))
    putVectorWord16 n (tcPlateId chunk)
    putVectorWord16 n (U.map plateBoundaryToCode (tcPlateBoundary chunk))
    putVectorFloat n (tcPlateHeight chunk)
    putVectorFloat n (tcPlateHardness chunk)
    putVectorWord16 n (tcPlateCrust chunk)
    putVectorFloat n (tcPlateAge chunk)
    putVectorFloat n (tcPlateVelX chunk)
    putVectorFloat n (tcPlateVelY chunk)

decodeTerrainChunk :: WorldConfig -> BS.ByteString -> Either ExportError TerrainChunk
decodeTerrainChunk config bytes =
  decodeWith (getTerrainChunk config) bytes

-- | Decode the legacy v2 terrain chunk schema (no plate crust/age/velocity).
decodeTerrainChunkV2 :: WorldConfig -> BS.ByteString -> Either ExportError TerrainChunk
decodeTerrainChunkV2 config bytes =
  decodeWith (getTerrainChunkV2 config) bytes

encodeClimateChunk :: WorldConfig -> ClimateChunk -> Either ExportError BS.ByteString
encodeClimateChunk config chunk = do
  let n = chunkTileCount config
  ensureLength n (Text.pack "tempAvg") (ccTempAvg chunk)
  ensureLength n (Text.pack "precipAvg") (ccPrecipAvg chunk)
  ensureLength n (Text.pack "windDirAvg") (ccWindDirAvg chunk)
  ensureLength n (Text.pack "windSpdAvg") (ccWindSpdAvg chunk)
  ensureLength n (Text.pack "humidityAvg") (ccHumidityAvg chunk)
  ensureLength n (Text.pack "tempRange") (ccTempRange chunk)
  ensureLength n (Text.pack "precipSeasonality") (ccPrecipSeasonality chunk)
  pure $ BL.toStrict $ runPut $ do
    putWord32le (fromIntegral n)
    putVectorFloat n (ccTempAvg chunk)
    putVectorFloat n (ccPrecipAvg chunk)
    putVectorFloat n (ccWindDirAvg chunk)
    putVectorFloat n (ccWindSpdAvg chunk)
    putVectorFloat n (ccHumidityAvg chunk)
    putVectorFloat n (ccTempRange chunk)
    putVectorFloat n (ccPrecipSeasonality chunk)

decodeClimateChunk :: WorldConfig -> BS.ByteString -> Either ExportError ClimateChunk
decodeClimateChunk config bytes =
  decodeWith (getClimateChunk config) bytes

-- | Decode the legacy climate chunk format (v1–v12) without seasonality fields.
decodeClimateChunkV1 :: WorldConfig -> BS.ByteString -> Either ExportError ClimateChunk
decodeClimateChunkV1 config bytes =
  decodeWith (getClimateChunkV1 config) bytes

encodeWeatherChunk :: WorldConfig -> WeatherChunk -> Either ExportError BS.ByteString
encodeWeatherChunk config chunk = do
  let n = chunkTileCount config
  ensureLength n (Text.pack "temp") (wcTemp chunk)
  ensureLength n (Text.pack "humidity") (wcHumidity chunk)
  ensureLength n (Text.pack "windDir") (wcWindDir chunk)
  ensureLength n (Text.pack "windSpd") (wcWindSpd chunk)
  ensureLength n (Text.pack "pressure") (wcPressure chunk)
  ensureLength n (Text.pack "precip") (wcPrecip chunk)
  pure $ BL.toStrict $ runPut $ do
    putWord32le (fromIntegral n)
    putVectorFloat n (wcTemp chunk)
    putVectorFloat n (wcHumidity chunk)
    putVectorFloat n (wcWindDir chunk)
    putVectorFloat n (wcWindSpd chunk)
    putVectorFloat n (wcPressure chunk)
    putVectorFloat n (wcPrecip chunk)

decodeWeatherChunk :: WorldConfig -> BS.ByteString -> Either ExportError WeatherChunk
decodeWeatherChunk config bytes =
  decodeWith (getWeatherChunk config) bytes

encodeRiverChunk :: WorldConfig -> RiverChunk -> Either ExportError BS.ByteString
encodeRiverChunk config chunk = do
  let n = chunkTileCount config
  ensureLength n (Text.pack "flowAccum") (rcFlowAccum chunk)
  ensureLength n (Text.pack "discharge") (rcDischarge chunk)
  ensureLength n (Text.pack "channelDepth") (rcChannelDepth chunk)
  ensureLength n (Text.pack "riverOrder") (rcRiverOrder chunk)
  ensureLength n (Text.pack "basinId") (rcBasinId chunk)
  ensureLength n (Text.pack "baseflow") (rcBaseflow chunk)
  ensureLength n (Text.pack "erosionPotential") (rcErosionPotential chunk)
  ensureLength n (Text.pack "depositPotential") (rcDepositPotential chunk)
  ensureLength n (Text.pack "flowDir") (rcFlowDir chunk)
  pure $ BL.toStrict $ runPut $ do
    putWord32le (fromIntegral n)
    putVectorFloat n (rcFlowAccum chunk)
    putVectorFloat n (rcDischarge chunk)
    putVectorFloat n (rcChannelDepth chunk)
    putVectorWord16 n (rcRiverOrder chunk)
    putVectorWord32 n (rcBasinId chunk)
    putVectorFloat n (rcBaseflow chunk)
    putVectorFloat n (rcErosionPotential chunk)
    putVectorFloat n (rcDepositPotential chunk)
    -- River topology data (v11)
    putVectorInt32 n (rcFlowDir chunk)
    let segCount = U.length (rcSegEntryEdge chunk)
    putWord32le (fromIntegral segCount)
    putVectorInt32 (n + 1) (rcSegOffsets chunk)
    putVectorWord8 segCount (rcSegEntryEdge chunk)
    putVectorWord8 segCount (rcSegExitEdge chunk)
    putVectorFloat segCount (rcSegDischarge chunk)
    putVectorWord16 segCount (rcSegOrder chunk)

decodeRiverChunk :: WorldConfig -> BS.ByteString -> Either ExportError RiverChunk
decodeRiverChunk config bytes =
  decodeWith (getRiverChunkV3 config) bytes

-- | Decode river chunks from format version 6–10 (8 per-tile fields, no topology).
decodeRiverChunkV2 :: WorldConfig -> BS.ByteString -> Either ExportError RiverChunk
decodeRiverChunkV2 config bytes =
  decodeWith (getRiverChunk config) bytes

decodeRiverChunkV1 :: WorldConfig -> BS.ByteString -> Either ExportError RiverChunk
decodeRiverChunkV1 config bytes =
  decodeWith (getRiverChunkV1 config) bytes

encodeGroundwaterChunk :: WorldConfig -> GroundwaterChunk -> Either ExportError BS.ByteString
encodeGroundwaterChunk config chunk = do
  let n = chunkTileCount config
  ensureLength n (Text.pack "storage") (gwStorage chunk)
  ensureLength n (Text.pack "recharge") (gwRecharge chunk)
  ensureLength n (Text.pack "discharge") (gwDischarge chunk)
  ensureLength n (Text.pack "basinId") (gwBasinId chunk)
  pure $ BL.toStrict $ runPut $ do
    putWord32le (fromIntegral n)
    putVectorFloat n (gwStorage chunk)
    putVectorFloat n (gwRecharge chunk)
    putVectorFloat n (gwDischarge chunk)
    putVectorWord32 n (gwBasinId chunk)

decodeGroundwaterChunk :: WorldConfig -> BS.ByteString -> Either ExportError GroundwaterChunk
decodeGroundwaterChunk config bytes =
  decodeWith (getGroundwaterChunk config) bytes

encodeVolcanismChunk :: WorldConfig -> VolcanismChunk -> Either ExportError BS.ByteString
encodeVolcanismChunk config chunk = do
  let n = chunkTileCount config
  ensureLength n (Text.pack "ventType") (vcVentType chunk)
  ensureLength n (Text.pack "activity") (vcActivity chunk)
  ensureLength n (Text.pack "magma") (vcMagma chunk)
  ensureLength n (Text.pack "eruptionCount") (vcEruptionCount chunk)
  ensureLength n (Text.pack "eruptedTotal") (vcEruptedTotal chunk)
  ensureLength n (Text.pack "lavaPotential") (vcLavaPotential chunk)
  ensureLength n (Text.pack "ashPotential") (vcAshPotential chunk)
  ensureLength n (Text.pack "depositPotential") (vcDepositPotential chunk)
  pure $ BL.toStrict $ runPut $ do
    putWord32le (fromIntegral n)
    putVectorWord16 n (U.map ventTypeToCode (vcVentType chunk))
    putVectorWord16 n (U.map ventActivityToCode (vcActivity chunk))
    putVectorFloat n (vcMagma chunk)
    putVectorWord16 n (vcEruptionCount chunk)
    putVectorFloat n (vcEruptedTotal chunk)
    putVectorFloat n (vcLavaPotential chunk)
    putVectorFloat n (vcAshPotential chunk)
    putVectorFloat n (vcDepositPotential chunk)

decodeVolcanismChunk :: WorldConfig -> BS.ByteString -> Either ExportError VolcanismChunk
decodeVolcanismChunk config bytes =
  decodeWith (getVolcanismChunk config) bytes

encodeGlacierChunk :: WorldConfig -> GlacierChunk -> Either ExportError BS.ByteString
encodeGlacierChunk config chunk = do
  let n = chunkTileCount config
  ensureLength n (Text.pack "snowpack") (glSnowpack chunk)
  ensureLength n (Text.pack "iceThickness") (glIceThickness chunk)
  ensureLength n (Text.pack "melt") (glMelt chunk)
  ensureLength n (Text.pack "flow") (glFlow chunk)
  ensureLength n (Text.pack "erosionPotential") (glErosionPotential chunk)
  ensureLength n (Text.pack "depositPotential") (glDepositPotential chunk)
  pure $ BL.toStrict $ runPut $ do
    putWord32le (fromIntegral n)
    putVectorFloat n (glSnowpack chunk)
    putVectorFloat n (glIceThickness chunk)
    putVectorFloat n (glMelt chunk)
    putVectorFloat n (glFlow chunk)
    putVectorFloat n (glErosionPotential chunk)
    putVectorFloat n (glDepositPotential chunk)

decodeGlacierChunk :: WorldConfig -> BS.ByteString -> Either ExportError GlacierChunk
decodeGlacierChunk config bytes =
  decodeWith (getGlacierChunk config) bytes

-- | Encode a 'VegetationChunk' to binary (v12+: cover, albedo, density).
encodeVegetationChunk :: WorldConfig -> VegetationChunk -> Either ExportError BS.ByteString
encodeVegetationChunk config chunk = do
  let n = chunkTileCount config
  ensureLength n (Text.pack "vegCover") (vegCover chunk)
  ensureLength n (Text.pack "vegAlbedo") (vegAlbedo chunk)
  ensureLength n (Text.pack "vegDensity") (vegDensity chunk)
  pure $ BL.toStrict $ runPut $ do
    putWord32le (fromIntegral n)
    putVectorFloat n (vegCover chunk)
    putVectorFloat n (vegAlbedo chunk)
    putVectorFloat n (vegDensity chunk)

-- | Decode a 'VegetationChunk' from binary (v12+: cover, albedo, density).
decodeVegetationChunk :: WorldConfig -> BS.ByteString -> Either ExportError VegetationChunk
decodeVegetationChunk config bytes =
  decodeWith (getVegetationChunkV2 config) bytes

-- | Decode the legacy v11 vegetation chunk (cover + albedo only, no density).
decodeVegetationChunkV1 :: WorldConfig -> BS.ByteString -> Either ExportError VegetationChunk
decodeVegetationChunkV1 config bytes =
  decodeWith (getVegetationChunk config) bytes

-- | Encode a 'WaterBodyChunk' to binary.
--
-- Schema: elementCount, wbType (Word8), wbSurfaceElev (Float),
-- wbBasinId (Word32), wbDepth (Float), wbAdjacentType (Word8).
encodeWaterBodyChunk :: WorldConfig -> WaterBodyChunk -> Either ExportError BS.ByteString
encodeWaterBodyChunk config chunk = do
  let n = chunkTileCount config
  ensureLength n (Text.pack "wbType") (wbType chunk)
  ensureLength n (Text.pack "wbSurfaceElev") (wbSurfaceElev chunk)
  ensureLength n (Text.pack "wbBasinId") (wbBasinId chunk)
  ensureLength n (Text.pack "wbDepth") (wbDepth chunk)
  ensureLength n (Text.pack "wbAdjacentType") (wbAdjacentType chunk)
  pure $ BL.toStrict $ runPut $ do
    putWord32le (fromIntegral n)
    putVectorWord8 n (U.map waterBodyToCode (wbType chunk))
    putVectorFloat n (wbSurfaceElev chunk)
    putVectorWord32 n (wbBasinId chunk)
    putVectorFloat n (wbDepth chunk)
    putVectorWord8 n (U.map waterBodyToCode (wbAdjacentType chunk))

-- | Decode a 'WaterBodyChunk' from binary.
decodeWaterBodyChunk :: WorldConfig -> BS.ByteString -> Either ExportError WaterBodyChunk
decodeWaterBodyChunk config bytes =
  decodeWith (getWaterBodyChunkBin config) bytes

exportTerrainChunks :: TerrainWorld -> Either ExportError [(ChunkId, BS.ByteString)]
exportTerrainChunks world =
  exportFromMap (twConfig world) encodeTerrainChunk (twTerrain world)

exportTerrainChunksRegion :: TerrainWorld -> Region -> Either ExportError [(ChunkId, BS.ByteString)]
exportTerrainChunksRegion world region =
  exportFromMapRegion (twConfig world) encodeTerrainChunk region (twTerrain world)

exportClimateChunks :: TerrainWorld -> Either ExportError [(ChunkId, BS.ByteString)]
exportClimateChunks world =
  exportFromMap (twConfig world) encodeClimateChunk (twClimate world)

exportClimateChunksRegion :: TerrainWorld -> Region -> Either ExportError [(ChunkId, BS.ByteString)]
exportClimateChunksRegion world region =
  exportFromMapRegion (twConfig world) encodeClimateChunk region (twClimate world)

exportWeatherChunks :: TerrainWorld -> Either ExportError [(ChunkId, BS.ByteString)]
exportWeatherChunks world =
  exportFromMap (twConfig world) encodeWeatherChunk (twWeather world)

exportWeatherChunksRegion :: TerrainWorld -> Region -> Either ExportError [(ChunkId, BS.ByteString)]
exportWeatherChunksRegion world region =
  exportFromMapRegion (twConfig world) encodeWeatherChunk region (twWeather world)

exportRiverChunks :: TerrainWorld -> Either ExportError [(ChunkId, BS.ByteString)]
exportRiverChunks world =
  exportFromMap (twConfig world) encodeRiverChunk (twRivers world)

exportRiverChunksRegion :: TerrainWorld -> Region -> Either ExportError [(ChunkId, BS.ByteString)]
exportRiverChunksRegion world region =
  exportFromMapRegion (twConfig world) encodeRiverChunk region (twRivers world)

exportGroundwaterChunks :: TerrainWorld -> Either ExportError [(ChunkId, BS.ByteString)]
exportGroundwaterChunks world =
  exportFromMap (twConfig world) encodeGroundwaterChunk (twGroundwater world)

exportGroundwaterChunksRegion :: TerrainWorld -> Region -> Either ExportError [(ChunkId, BS.ByteString)]
exportGroundwaterChunksRegion world region =
  exportFromMapRegion (twConfig world) encodeGroundwaterChunk region (twGroundwater world)

exportVolcanismChunks :: TerrainWorld -> Either ExportError [(ChunkId, BS.ByteString)]
exportVolcanismChunks world =
  exportFromMap (twConfig world) encodeVolcanismChunk (twVolcanism world)

exportVolcanismChunksRegion :: TerrainWorld -> Region -> Either ExportError [(ChunkId, BS.ByteString)]
exportVolcanismChunksRegion world region =
  exportFromMapRegion (twConfig world) encodeVolcanismChunk region (twVolcanism world)

exportGlacierChunks :: TerrainWorld -> Either ExportError [(ChunkId, BS.ByteString)]
exportGlacierChunks world =
  exportFromMap (twConfig world) encodeGlacierChunk (twGlaciers world)

exportGlacierChunksRegion :: TerrainWorld -> Region -> Either ExportError [(ChunkId, BS.ByteString)]
exportGlacierChunksRegion world region =
  exportFromMapRegion (twConfig world) encodeGlacierChunk region (twGlaciers world)

exportVegetationChunks :: TerrainWorld -> Either ExportError [(ChunkId, BS.ByteString)]
exportVegetationChunks world =
  exportFromMap (twConfig world) encodeVegetationChunk (twVegetation world)

exportVegetationChunksRegion :: TerrainWorld -> Region -> Either ExportError [(ChunkId, BS.ByteString)]
exportVegetationChunksRegion world region =
  exportFromMapRegion (twConfig world) encodeVegetationChunk region (twVegetation world)

exportWaterBodyChunks :: TerrainWorld -> Either ExportError [(ChunkId, BS.ByteString)]
exportWaterBodyChunks world =
  exportFromMap (twConfig world) encodeWaterBodyChunk (twWaterBodies world)

exportWaterBodyChunksRegion :: TerrainWorld -> Region -> Either ExportError [(ChunkId, BS.ByteString)]
exportWaterBodyChunksRegion world region =
  exportFromMapRegion (twConfig world) encodeWaterBodyChunk region (twWaterBodies world)

exportBiomeChunks :: TerrainWorld -> Either ExportError [(ChunkId, BS.ByteString)]
exportBiomeChunks world =
  exportFromMap (twConfig world) encodeBiomeChunk (twTerrain world)

exportBiomeChunksRegion :: TerrainWorld -> Region -> Either ExportError [(ChunkId, BS.ByteString)]
exportBiomeChunksRegion world region =
  exportFromMapRegion (twConfig world) encodeBiomeChunk region (twTerrain world)

exportFromMap
  :: WorldConfig
  -> (WorldConfig -> a -> Either ExportError BS.ByteString)
  -> IntMap a
  -> Either ExportError [(ChunkId, BS.ByteString)]
exportFromMap config encoder chunks =
  traverse (encodeEntry config encoder) (IntMap.toList chunks)

exportFromMapRegion
  :: WorldConfig
  -> (WorldConfig -> a -> Either ExportError BS.ByteString)
  -> Region
  -> IntMap a
  -> Either ExportError [(ChunkId, BS.ByteString)]
exportFromMapRegion config encoder region chunks =
  let ids = IntSet.fromList (map chunkKey (chunksForRegion config region))
      selected = filter (\(key, _) -> IntSet.member key ids) (IntMap.toList chunks)
  in traverse (encodeEntry config encoder) selected

chunksForRegion :: WorldConfig -> Region -> [ChunkId]
chunksForRegion config (RegionRect (TileCoord x0 y0) (TileCoord x1 y1)) =
  let size = wcChunkSize config
      minX = min x0 x1
      maxX = max x0 x1
      minY = min y0 y1
      maxY = max y0 y1
      cx0 = minX `div` size
      cx1 = maxX `div` size
      cy0 = minY `div` size
      cy1 = maxY `div` size
  in [chunkIdFromCoord (ChunkCoord cx cy) | cy <- [cy0..cy1], cx <- [cx0..cx1]]

chunkKey :: ChunkId -> Int
chunkKey (ChunkId i) = i

putVectorFloat :: Int -> U.Vector Float -> Put
putVectorFloat n vec =
  mapM_ putFloatle (U.toList (U.take n vec))

putVectorWord16 :: Int -> U.Vector Word16 -> Put
putVectorWord16 n vec =
  mapM_ putWord16le (U.toList (U.take n vec))

putVectorWord32 :: Int -> U.Vector Word32 -> Put
putVectorWord32 n vec =
  mapM_ putWord32le (U.toList (U.take n vec))

putVectorWord8 :: Int -> U.Vector Word8 -> Put
putVectorWord8 n vec =
  mapM_ putWord8 (U.toList (U.take n vec))

putVectorInt32 :: Int -> U.Vector Int -> Put
putVectorInt32 n vec =
  mapM_ (putInt32le . fromIntegral) (U.toList (U.take n vec))

encodeBiomeChunk :: WorldConfig -> TerrainChunk -> Either ExportError BS.ByteString
encodeBiomeChunk config chunk = do
  let n = chunkTileCount config
  ensureLength n (Text.pack "biomes") (tcFlags chunk)
  pure $ BL.toStrict $ runPut $ do
    putWord32le (fromIntegral n)
    putVectorWord16 n (U.map biomeIdToCode (tcFlags chunk))

ensureLength :: U.Unbox a => Int -> Text -> U.Vector a -> Either ExportError ()
ensureLength n label vec =
  let actual = U.length vec
  in if actual == n
      then Right ()
      else Left (ExportLengthMismatch label n actual)

encodeEntry
  :: WorldConfig
  -> (WorldConfig -> a -> Either ExportError BS.ByteString)
  -> (Int, a)
  -> Either ExportError (ChunkId, BS.ByteString)
encodeEntry config encoder (key, chunk) = do
  bytes <- encoder config chunk
  pure (ChunkId key, bytes)

decodeWith :: Get a -> BS.ByteString -> Either ExportError a
decodeWith getter bytes =
  case runGetOrFail getter (BL.fromStrict bytes) of
    Left (_, _, err) -> Left (ExportDecodeError (Text.pack err))
    Right (_, _, chunk) -> Right chunk

getTerrainChunk :: WorldConfig -> Get TerrainChunk
getTerrainChunk config = do
  n <- getCount config
  tcElevation <- getVectorFloat n
  tcSlope <- getVectorFloat n
  tcCurvature <- getVectorFloat n
  tcHardness <- getVectorFloat n
  tcRockType <- getVectorWord16 n
  tcSoilType <- getVectorWord16 n
  tcSoilDepth <- getVectorFloat n
  tcMoisture <- getVectorFloat n
  tcFertility <- getVectorFloat n
  tcRoughness <- getVectorFloat n
  tcRockDensity <- getVectorFloat n
  tcSoilGrain <- getVectorFloat n
  tcRelief <- getVectorFloat n
  tcRuggedness <- getVectorFloat n
  tcTerrainForm <- getVectorTerrainForm n
  tcFlags <- getVectorBiomeId n
  tcPlateId <- getVectorWord16 n
  tcPlateBoundary <- getVectorPlateBoundary n
  tcPlateHeight <- getVectorFloat n
  tcPlateHardness <- getVectorFloat n
  tcPlateCrust <- getVectorWord16 n
  tcPlateAge <- getVectorFloat n
  tcPlateVelX <- getVectorFloat n
  tcPlateVelY <- getVectorFloat n
  pure TerrainChunk
    { tcElevation = tcElevation
    , tcSlope = tcSlope
    , tcCurvature = tcCurvature
    , tcHardness = tcHardness
    , tcRockType = tcRockType
    , tcSoilType = tcSoilType
    , tcSoilDepth = tcSoilDepth
    , tcMoisture = tcMoisture
    , tcFertility = tcFertility
    , tcRoughness = tcRoughness
    , tcRockDensity = tcRockDensity
    , tcSoilGrain = tcSoilGrain
    , tcRelief = tcRelief
    , tcRuggedness = tcRuggedness
    , tcTerrainForm = tcTerrainForm
    , tcFlags = tcFlags
    , tcPlateId = tcPlateId
    , tcPlateBoundary = tcPlateBoundary
    , tcPlateHeight = tcPlateHeight
    , tcPlateHardness = tcPlateHardness
    , tcPlateCrust = tcPlateCrust
    , tcPlateAge = tcPlateAge
    , tcPlateVelX = tcPlateVelX
    , tcPlateVelY = tcPlateVelY
    }

getTerrainChunkV2 :: WorldConfig -> Get TerrainChunk
getTerrainChunkV2 config = do
  n <- getCount config
  tcElevation <- getVectorFloat n
  tcSlope <- getVectorFloat n
  tcCurvature <- getVectorFloat n
  tcHardness <- getVectorFloat n
  tcRockType <- getVectorWord16 n
  tcSoilType <- getVectorWord16 n
  tcSoilDepth <- getVectorFloat n
  tcMoisture <- getVectorFloat n
  tcFertility <- getVectorFloat n
  tcRoughness <- getVectorFloat n
  tcRockDensity <- getVectorFloat n
  tcSoilGrain <- getVectorFloat n
  tcFlags <- getVectorBiomeId n
  tcPlateId <- getVectorWord16 n
  tcPlateBoundary <- getVectorPlateBoundary n
  tcPlateHeight <- getVectorFloat n
  tcPlateHardness <- getVectorFloat n
  let zeros = U.replicate n 0
      zeros16 = U.replicate n 0
      formZeros = U.replicate n FormFlat
  pure TerrainChunk
    { tcElevation = tcElevation
    , tcSlope = tcSlope
    , tcCurvature = tcCurvature
    , tcHardness = tcHardness
    , tcRockType = tcRockType
    , tcSoilType = tcSoilType
    , tcSoilDepth = tcSoilDepth
    , tcMoisture = tcMoisture
    , tcFertility = tcFertility
    , tcRoughness = tcRoughness
    , tcRockDensity = tcRockDensity
    , tcSoilGrain = tcSoilGrain
    , tcRelief = zeros
    , tcRuggedness = zeros
    , tcTerrainForm = formZeros
    , tcFlags = tcFlags
    , tcPlateId = tcPlateId
    , tcPlateBoundary = tcPlateBoundary
    , tcPlateHeight = tcPlateHeight
    , tcPlateHardness = tcPlateHardness
    , tcPlateCrust = zeros16
    , tcPlateAge = zeros
    , tcPlateVelX = zeros
    , tcPlateVelY = zeros
    }

getClimateChunk :: WorldConfig -> Get ClimateChunk
getClimateChunk config = do
  n <- getCount config
  ccTempAvg <- getVectorFloat n
  ccPrecipAvg <- getVectorFloat n
  ccWindDirAvg <- getVectorFloat n
  ccWindSpdAvg <- getVectorFloat n
  ccHumidityAvg <- getVectorFloat n
  ccTempRange <- getVectorFloat n
  ccPrecipSeasonality <- getVectorFloat n
  pure ClimateChunk
    { ccTempAvg = ccTempAvg
    , ccPrecipAvg = ccPrecipAvg
    , ccWindDirAvg = ccWindDirAvg
    , ccWindSpdAvg = ccWindSpdAvg
    , ccHumidityAvg = ccHumidityAvg
    , ccTempRange = ccTempRange
    , ccPrecipSeasonality = ccPrecipSeasonality
    }

-- | Decode the legacy climate chunk format (v1–v12) without seasonality
-- fields.  Missing fields default to zero vectors.
getClimateChunkV1 :: WorldConfig -> Get ClimateChunk
getClimateChunkV1 config = do
  n <- getCount config
  ccTempAvg <- getVectorFloat n
  ccPrecipAvg <- getVectorFloat n
  ccWindDirAvg <- getVectorFloat n
  ccWindSpdAvg <- getVectorFloat n
  let zeros = U.replicate n 0
  pure ClimateChunk
    { ccTempAvg = ccTempAvg
    , ccPrecipAvg = ccPrecipAvg
    , ccWindDirAvg = ccWindDirAvg
    , ccWindSpdAvg = ccWindSpdAvg
    , ccHumidityAvg = zeros
    , ccTempRange = zeros
    , ccPrecipSeasonality = zeros
    }

getWeatherChunk :: WorldConfig -> Get WeatherChunk
getWeatherChunk config = do
  n <- getCount config
  wcTemp <- getVectorFloat n
  wcHumidity <- getVectorFloat n
  wcWindDir <- getVectorFloat n
  wcWindSpd <- getVectorFloat n
  wcPressure <- getVectorFloat n
  wcPrecip <- getVectorFloat n
  pure WeatherChunk
    { wcTemp = wcTemp
    , wcHumidity = wcHumidity
    , wcWindDir = wcWindDir
    , wcWindSpd = wcWindSpd
    , wcPressure = wcPressure
    , wcPrecip = wcPrecip
    }

getRiverChunk :: WorldConfig -> Get RiverChunk
getRiverChunk config = do
  n <- getCount config
  rcFlowAccum <- getVectorFloat n
  rcDischarge <- getVectorFloat n
  rcChannelDepth <- getVectorFloat n
  rcRiverOrder <- getVectorWord16 n
  rcBasinId <- getVectorWord32 n
  rcBaseflow <- getVectorFloat n
  rcErosionPotential <- getVectorFloat n
  rcDepositPotential <- getVectorFloat n
  let sinkFlow = U.replicate n (-1 :: Int)
      emptyOffsets = U.replicate (n + 1) (0 :: Int)
  pure RiverChunk
    { rcFlowAccum = rcFlowAccum
    , rcDischarge = rcDischarge
    , rcChannelDepth = rcChannelDepth
    , rcRiverOrder = rcRiverOrder
    , rcBasinId = rcBasinId
    , rcBaseflow = rcBaseflow
    , rcErosionPotential = rcErosionPotential
    , rcDepositPotential = rcDepositPotential
    , rcFlowDir = sinkFlow
    , rcSegOffsets = emptyOffsets
    , rcSegEntryEdge = U.empty
    , rcSegExitEdge = U.empty
    , rcSegDischarge = U.empty
    , rcSegOrder = U.empty
    }

-- | Decode river chunk from v11+ format with topology.
getRiverChunkV3 :: WorldConfig -> Get RiverChunk
getRiverChunkV3 config = do
  n <- getCount config
  rcFlowAccum <- getVectorFloat n
  rcDischarge <- getVectorFloat n
  rcChannelDepth <- getVectorFloat n
  rcRiverOrder <- getVectorWord16 n
  rcBasinId <- getVectorWord32 n
  rcBaseflow <- getVectorFloat n
  rcErosionPotential <- getVectorFloat n
  rcDepositPotential <- getVectorFloat n
  rcFlowDir <- getVectorInt32 n
  segCount <- fromIntegral <$> getWord32le
  rcSegOffsets <- getVectorInt32 (n + 1)
  rcSegEntryEdge <- getVectorWord8 segCount
  rcSegExitEdge <- getVectorWord8 segCount
  rcSegDischarge <- getVectorFloat segCount
  rcSegOrder <- getVectorWord16 segCount
  pure RiverChunk
    { rcFlowAccum = rcFlowAccum
    , rcDischarge = rcDischarge
    , rcChannelDepth = rcChannelDepth
    , rcRiverOrder = rcRiverOrder
    , rcBasinId = rcBasinId
    , rcBaseflow = rcBaseflow
    , rcErosionPotential = rcErosionPotential
    , rcDepositPotential = rcDepositPotential
    , rcFlowDir = rcFlowDir
    , rcSegOffsets = rcSegOffsets
    , rcSegEntryEdge = rcSegEntryEdge
    , rcSegExitEdge = rcSegExitEdge
    , rcSegDischarge = rcSegDischarge
    , rcSegOrder = rcSegOrder
    }

getRiverChunkV1 :: WorldConfig -> Get RiverChunk
getRiverChunkV1 config = do
  n <- getCount config
  rcFlowAccum <- getVectorFloat n
  rcDischarge <- getVectorFloat n
  rcChannelDepth <- getVectorFloat n
  rcRiverOrder <- getVectorWord16 n
  rcBasinId <- getVectorWord32 n
  rcBaseflow <- getVectorFloat n
  let zeros = U.replicate n 0
      sinkFlow = U.replicate n (-1 :: Int)
      emptyOffsets = U.replicate (n + 1) (0 :: Int)
  pure RiverChunk
    { rcFlowAccum = rcFlowAccum
    , rcDischarge = rcDischarge
    , rcChannelDepth = rcChannelDepth
    , rcRiverOrder = rcRiverOrder
    , rcBasinId = rcBasinId
    , rcBaseflow = rcBaseflow
    , rcErosionPotential = zeros
    , rcDepositPotential = zeros
    , rcFlowDir = sinkFlow
    , rcSegOffsets = emptyOffsets
    , rcSegEntryEdge = U.empty
    , rcSegExitEdge = U.empty
    , rcSegDischarge = U.empty
    , rcSegOrder = U.empty
    }

getGroundwaterChunk :: WorldConfig -> Get GroundwaterChunk
getGroundwaterChunk config = do
  n <- getCount config
  gwStorage <- getVectorFloat n
  gwRecharge <- getVectorFloat n
  gwDischarge <- getVectorFloat n
  gwBasinId <- getVectorWord32 n
  pure GroundwaterChunk
    { gwStorage = gwStorage
    , gwRecharge = gwRecharge
    , gwDischarge = gwDischarge
    , gwBasinId = gwBasinId
    }

getVolcanismChunk :: WorldConfig -> Get VolcanismChunk
getVolcanismChunk config = do
  n <- getCount config
  vcVentType <- getVectorVentType n
  vcActivity <- getVectorVentActivity n
  vcMagma <- getVectorFloat n
  vcEruptionCount <- getVectorWord16 n
  vcEruptedTotal <- getVectorFloat n
  vcLavaPotential <- getVectorFloat n
  vcAshPotential <- getVectorFloat n
  vcDepositPotential <- getVectorFloat n
  pure VolcanismChunk
    { vcVentType = vcVentType
    , vcActivity = vcActivity
    , vcMagma = vcMagma
    , vcEruptionCount = vcEruptionCount
    , vcEruptedTotal = vcEruptedTotal
    , vcLavaPotential = vcLavaPotential
    , vcAshPotential = vcAshPotential
    , vcDepositPotential = vcDepositPotential
    }

getGlacierChunk :: WorldConfig -> Get GlacierChunk
getGlacierChunk config = do
  n <- getCount config
  glSnowpack <- getVectorFloat n
  glIceThickness <- getVectorFloat n
  glMelt <- getVectorFloat n
  glFlow <- getVectorFloat n
  glErosionPotential <- getVectorFloat n
  glDepositPotential <- getVectorFloat n
  pure GlacierChunk
    { glSnowpack = glSnowpack
    , glIceThickness = glIceThickness
    , glMelt = glMelt
    , glFlow = glFlow
    , glErosionPotential = glErosionPotential
    , glDepositPotential = glDepositPotential
    }

getVegetationChunk :: WorldConfig -> Get VegetationChunk
getVegetationChunk config = do
  n <- getCount config
  cover <- getVectorFloat n
  albedo <- getVectorFloat n
  pure VegetationChunk
    { vegCover   = cover
    , vegAlbedo  = albedo
    , vegDensity = U.replicate n 0
    }

-- | Decode v12+ vegetation chunk (cover, albedo, density).
getVegetationChunkV2 :: WorldConfig -> Get VegetationChunk
getVegetationChunkV2 config = do
  n <- getCount config
  cover <- getVectorFloat n
  albedo <- getVectorFloat n
  density <- getVectorFloat n
  pure VegetationChunk
    { vegCover   = cover
    , vegAlbedo  = albedo
    , vegDensity = density
    }

-- | Decode a 'WaterBodyChunk' from binary.
getWaterBodyChunkBin :: WorldConfig -> Get WaterBodyChunk
getWaterBodyChunkBin config = do
  n <- getCount config
  wbType' <- getVectorWaterBodyType n
  surfElev <- getVectorFloat n
  basinId <- getVectorWord32 n
  depth <- getVectorFloat n
  adjType <- getVectorWaterBodyType n
  pure WaterBodyChunk
    { wbType         = wbType'
    , wbSurfaceElev  = surfElev
    , wbBasinId      = basinId
    , wbDepth        = depth
    , wbAdjacentType = adjType
    }

getVectorWaterBodyType :: Int -> Get (U.Vector WaterBodyType)
getVectorWaterBodyType n = do
  codes <- getVectorWord8 n
  case traverse waterBodyFromCode (U.toList codes) of
    Left err -> fail ("decode: invalid water body type (" <> show err <> ")")
    Right values -> pure (U.fromList values)

getCount :: WorldConfig -> Get Int
getCount config = do
  n <- fromIntegral <$> getWord32le
  let expected = chunkTileCount config
  if n == expected
    then pure n
    else fail ("decode: unexpected element count " <> show n <> ", expected " <> show expected)

getVectorFloat :: Int -> Get (U.Vector Float)
getVectorFloat n = U.fromList <$> replicateM n getFloatle

getVectorWord16 :: Int -> Get (U.Vector Word16)
getVectorWord16 n = U.fromList <$> replicateM n getWord16le

getVectorWord32 :: Int -> Get (U.Vector Word32)
getVectorWord32 n = U.fromList <$> replicateM n getWord32le

getVectorWord8 :: Int -> Get (U.Vector Word8)
getVectorWord8 n = U.fromList <$> replicateM n getWord8

getVectorInt32 :: Int -> Get (U.Vector Int)
getVectorInt32 n = U.fromList . map fromIntegral <$> replicateM n getInt32le

getVectorBiomeId :: Int -> Get (U.Vector BiomeId)
getVectorBiomeId = getVectorMapped "biome id" biomeIdFromCode

getVectorPlateBoundary :: Int -> Get (U.Vector PlateBoundary)
getVectorPlateBoundary = getVectorMapped "plate boundary" plateBoundaryFromCode

getVectorVentType :: Int -> Get (U.Vector VentType)
getVectorVentType = getVectorMapped "vent type" ventTypeFromCode

getVectorVentActivity :: Int -> Get (U.Vector VentActivity)
getVectorVentActivity = getVectorMapped "vent activity" ventActivityFromCode

getVectorTerrainForm :: Int -> Get (U.Vector TerrainForm)
getVectorTerrainForm n = do
  codes <- getVectorWord8 n
  case traverse terrainFormFromCode (U.toList codes) of
    Left err -> fail ("decode: invalid terrain form (" <> show err <> ")")
    Right values -> pure (U.fromList values)

getVectorMapped :: (U.Unbox a, Show e) => String -> (Word16 -> Either e a) -> Int -> Get (U.Vector a)
getVectorMapped label decode n = do
  codes <- getVectorWord16 n
  case traverse decode (U.toList codes) of
    Left err -> fail ("decode: invalid " <> label <> " (" <> show err <> ")")
    Right values -> pure (U.fromList values)
