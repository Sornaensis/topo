{-# LANGUAGE TypeApplications #-}

module Topo.Storage
  ( StorageError(..)
  , initWorld
  , initWorldWithHex
  , saveWorld
  , loadWorld
  , encodeWorld
  , decodeWorld
  , saveWorldWithProvenance
  , loadWorldWithProvenance
  , encodeWorldWithProvenance
  , decodeWorldWithProvenance
  , decodeWorldWithMetadata
  , WorldProvenance(..)
  , MapProvenance(..)
  , emptyProvenance
  , emptyMapProvenance
  , encodeProvenance
  , decodeProvenance
  ) where

import Control.Monad (replicateM)
import Data.Aeson (Value, decode, encode)
import Data.Binary.Get (Get, getByteString, getDoublele, getFloatle, getInt32le, getWord32le, getWord64le, runGetOrFail)
import Data.Binary.Put (Put, putByteString, putDoublele, putFloatle, putInt32le, putWord32le, putWord64le, runPut)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy(..))
import Data.Word (Word32, Word64)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Topo.Export
  ( ExportError
  , decodeClimateChunk
  , decodeGroundwaterChunk
  , decodeVolcanismChunk
  , decodeGlacierChunk
  , decodeRiverChunk
  , decodeTerrainChunk
  , decodeVegetationChunk
  , decodeWaterBodyChunk
  , encodeClimateChunk
  , encodeGroundwaterChunk
  , encodeVolcanismChunk
  , encodeGlacierChunk
  , encodeRiverChunk
  , encodeTerrainChunk
  , encodeVegetationChunk
  , encodeWaterBodyChunk
  )
import Topo.Calendar (WorldTime(..), PlanetAge(..))
import Topo.Hex (HexGridMeta(..), defaultHexGridMeta)
import Topo.Overlay (OverlayStore, emptyOverlayStore)
import Topo.Metadata
  ( Metadata(..)
  , MetadataCodec(..)
  , MetadataDecodeError
  , MetadataStore(..)
  , SomeMetadata(..)
  , emptyMetadataStore
  , metadataCodec
  )
import Topo.PlateMetadata (PlateHexMeta)
import Topo.Planet (PlanetConfig(..), WorldSlice(..), mkLatitudeMapping)
import Topo.Types
import Topo.Units (UnitScales(..))
import Topo.World

-- | Storage-layer failures while encoding or decoding worlds.
data StorageError
  = StorageEncodeError !ExportError
  | StorageDecodeError !Text
  deriving (Eq, Show)

initWorld :: WorldConfig -> IO TerrainWorld
initWorld config = pure (emptyWorld config defaultHexGridMeta)

initWorldWithHex :: WorldConfig -> HexGridMeta -> IO TerrainWorld
initWorldWithHex config meta = pure (emptyWorld config meta)

-- | Internal raw @.topo@ save helper.
--
-- Application-level persistence should use
-- 'Topo.Persistence.WorldBundle.saveWorldBundle' so terrain and
-- overlays are saved together atomically.
saveWorld :: FilePath -> TerrainWorld -> IO (Either StorageError ())
saveWorld path world =
  case encodeWorld world of
    Left err -> pure (Left err)
    Right bytes -> Right <$> BS.writeFile path bytes

-- | Internal raw @.topo@ load helper.
--
-- Application-level persistence should use
-- 'Topo.Persistence.WorldBundle.loadWorldBundle' so terrain and
-- overlays are loaded and validated together.
loadWorld :: FilePath -> IO (Either StorageError TerrainWorld)
loadWorld path = do
  bytes <- BS.readFile path
  pure (decodeWorld bytes)

saveWorldWithProvenance :: FilePath -> WorldProvenance -> TerrainWorld -> IO (Either StorageError ())
saveWorldWithProvenance path prov world =
  case encodeWorldWithProvenance prov world of
    Left err -> pure (Left err)
    Right bytes -> Right <$> BS.writeFile path bytes

loadWorldWithProvenance :: FilePath -> IO (Either StorageError (WorldProvenance, TerrainWorld))
loadWorldWithProvenance path = do
  bytes <- BS.readFile path
  pure (decodeWorldWithProvenance bytes)

encodeWorld :: TerrainWorld -> Either StorageError BS.ByteString
encodeWorld world =
  encodeWorldWithProvenance emptyProvenance world

-- | Encode a full world file including provenance and metadata.
--
-- Layout (version 14):
--   magic, version, chunkSize, hexSize, provenance, metadata,
--   planetConfig, worldSlice, worldTime, genConfig (length-prefixed JSON blob),
--   terrain chunks, climate chunks, river chunks, groundwater chunks,
--   glacier chunks, volcanism chunks, waterBody chunks, vegetation chunks.
encodeWorldWithProvenance :: WorldProvenance -> TerrainWorld -> Either StorageError BS.ByteString
encodeWorldWithProvenance prov world = do
  let config = twConfig world
  terrain <- encodeChunkMap config encodeTerrainChunk (twTerrain world)
  climate <- encodeChunkMap config encodeClimateChunk (twClimate world)
  rivers <- encodeChunkMap config encodeRiverChunk (twRivers world)
  groundwater <- encodeChunkMap config encodeGroundwaterChunk (twGroundwater world)
  glaciers <- encodeChunkMap config encodeGlacierChunk (twGlaciers world)
  volcanism <- encodeChunkMap config encodeVolcanismChunk (twVolcanism world)
  waterBodies <- encodeChunkMap config encodeWaterBodyChunk (twWaterBodies world)
  vegetation <- encodeChunkMap config encodeVegetationChunk (twVegetation world)
  pure $ BL.toStrict $ runPut $ do
    putByteString magic
    putWord32le fileVersion
    putWord32le (fromIntegral (wcChunkSize config))
    putFloatle (hexSizeKm (twHexGrid world))
    encodeProvenance prov
    putMetadataStore (twMeta world)
    putPlanetConfig (twPlanet world)
    putWorldSlice (twSlice world)
    putWorldTime (twWorldTime world)
    putWord64le (twSeed world)
    putPlanetAge (twPlanetAge world)
    putGenConfig (twGenConfig world)
    putUnitScales (twUnitScales world)
    putOverlayManifest (twOverlayManifest world)
    putChunkMapBytes terrain
    putChunkMapBytes climate
    putChunkMapBytes rivers
    putChunkMapBytes groundwater
    putChunkMapBytes glaciers
    putChunkMapBytes volcanism
    putChunkMapBytes waterBodies
    putChunkMapBytes vegetation

decodeWorld :: BS.ByteString -> Either StorageError TerrainWorld
decodeWorld bytes =
  fmap snd (decodeWorldWithProvenance bytes)

decodeWorldWithProvenance :: BS.ByteString -> Either StorageError (WorldProvenance, TerrainWorld)
decodeWorldWithProvenance = decodeWorldWithMetadata defaultMetadataCodecs

-- | Decode a world with explicit metadata codecs for custom entries.
decodeWorldWithMetadata :: [MetadataCodec] -> BS.ByteString -> Either StorageError (WorldProvenance, TerrainWorld)
decodeWorldWithMetadata codecs bytes =
  case runGetOrFail (getWorldWithProvenance codecs) (BL.fromStrict bytes) of
    Left (_, _, err) -> Left (StorageDecodeError (Text.pack err))
    Right (_, _, result) -> Right result

-- | World file magic header: "TOPO".
magic :: BS.ByteString
magic = BS.pack [0x54, 0x4f, 0x50, 0x4f]

-- | Current world file format version.
fileVersion :: Word32
fileVersion = 23

defaultMetadataCodecs :: [MetadataCodec]
defaultMetadataCodecs =
  [ metadataCodec (Proxy :: Proxy PlateHexMeta)
  ]

encodeChunkMap
  :: WorldConfig
  -> (WorldConfig -> a -> Either ExportError BS.ByteString)
  -> IntMap a
  -> Either StorageError [(Int, BS.ByteString)]
encodeChunkMap config encoder chunks =
  traverse (encodeChunk encoder config) (IntMap.toList chunks)

encodeChunk
  :: (WorldConfig -> a -> Either ExportError BS.ByteString)
  -> WorldConfig
  -> (Int, a)
  -> Either StorageError (Int, BS.ByteString)
encodeChunk encoder config (key, chunk) =
  case encoder config chunk of
    Left err -> Left (StorageEncodeError err)
    Right bytes -> Right (key, bytes)

putChunkMapBytes :: [(Int, BS.ByteString)] -> Put
putChunkMapBytes chunks = do
  putWord32le (fromIntegral (length chunks))
  mapM_ putChunkBytes chunks

putChunkBytes :: (Int, BS.ByteString) -> Put
putChunkBytes (key, bytes) = do
  putWord32le (fromIntegral key)
  putWord32le (fromIntegral (BS.length bytes))
  putByteString bytes

-- | Decode a world file with provenance and metadata.
--
-- Only accepts the current file format version ('fileVersion').
getWorldWithProvenance :: [MetadataCodec] -> Get (WorldProvenance, TerrainWorld)
getWorldWithProvenance codecs = do
  fileMagic <- getByteString 4
  if fileMagic /= magic
    then fail "decode: invalid magic"
    else pure ()
  version <- getWord32le
  if version /= fileVersion
    then fail "decode: unsupported version"
    else pure ()
  size <- fromIntegral <$> getWord32le
  hex <- getFloatle
  prov <- decodeProvenance
  metaStore <- getMetadataStore codecs
  config <- case mkWorldConfig size of
    Left err -> fail ("decode: invalid chunk size (" <> show err <> ")")
    Right cfg -> pure cfg
  let hexMeta = HexGridMeta { hexSizeKm = hex }
  planet <- getPlanetConfig
  slice <- getWorldSlice
  worldTime <- getWorldTime
  worldSeed <- getWord64le
  planetAge <- getPlanetAge
  genConfig <- getGenConfig
  unitScales <- getUnitScales
  overlayManifest <- getOverlayManifest
  terrain <- getChunkMap decodeTerrainChunk config
  climate <- getChunkMap decodeClimateChunk config
  rivers <- getChunkMap decodeRiverChunk config
  groundwater <- getChunkMap decodeGroundwaterChunk config
  glaciers <- getChunkMap decodeGlacierChunk config
  volcanism <- getChunkMap decodeVolcanismChunk config
  waterBodies <- getChunkMap decodeWaterBodyChunk config
  vegetation <- getChunkMap decodeVegetationChunk config
  pure
    ( prov
    , TerrainWorld
        { twTerrain = terrain
        , twClimate = climate
        , twRivers = rivers
        , twGroundwater = groundwater
        , twVolcanism = volcanism
        , twGlaciers = glaciers
        , twWaterBodies = waterBodies
        , twVegetation = vegetation
        , twHexGrid = hexMeta
        , twMeta = metaStore
        , twConfig = config
        , twPlanet = planet
        , twSlice = slice
        , twLatMapping = mkLatitudeMapping planet hexMeta slice config
        , twWorldTime = worldTime
        , twSeed = worldSeed
        , twPlanetAge = planetAge
        , twGenConfig = genConfig
        , twUnitScales = unitScales
        , twOverlays = emptyOverlayStore
        , twOverlayManifest = overlayManifest
        }
    )

data MapProvenance = MapProvenance
  { mpSeed :: !Word64
  , mpVersion :: !Word32
  , mpParams :: !Text
  } deriving (Eq, Show)

data WorldProvenance = WorldProvenance
  { wpSeed :: !Word64
  , wpVersion :: !Word32
  , wpNotes :: !Text
  , wpTerrain :: !MapProvenance
  , wpClimate :: !MapProvenance
  , wpBiome :: !MapProvenance
  } deriving (Eq, Show)

emptyMapProvenance :: MapProvenance
emptyMapProvenance = MapProvenance
  { mpSeed = 0
  , mpVersion = 1
  , mpParams = Text.empty
  }

emptyProvenance :: WorldProvenance
emptyProvenance = WorldProvenance
  { wpSeed = 0
  , wpVersion = 1
  , wpNotes = Text.empty
  , wpTerrain = emptyMapProvenance
  , wpClimate = emptyMapProvenance
  , wpBiome = emptyMapProvenance
  }

encodeProvenance :: WorldProvenance -> Put
encodeProvenance prov = do
  putWord64le (wpSeed prov)
  putWord32le (wpVersion prov)
  putText (wpNotes prov)
  encodeMapProvenance (wpTerrain prov)
  encodeMapProvenance (wpClimate prov)
  encodeMapProvenance (wpBiome prov)

decodeProvenance :: Get WorldProvenance
decodeProvenance = do
  seed <- getWord64le
  ver <- getWord32le
  notes <- getText
  terrain <- decodeMapProvenance
  climate <- decodeMapProvenance
  biome <- decodeMapProvenance
  pure WorldProvenance
    { wpSeed = seed
    , wpVersion = ver
    , wpNotes = notes
    , wpTerrain = terrain
    , wpClimate = climate
    , wpBiome = biome
    }

encodeMapProvenance :: MapProvenance -> Put
encodeMapProvenance prov = do
  putWord64le (mpSeed prov)
  putWord32le (mpVersion prov)
  putText (mpParams prov)

decodeMapProvenance :: Get MapProvenance
decodeMapProvenance = do
  seed <- getWord64le
  ver <- getWord32le
  params <- getText
  pure MapProvenance
    { mpSeed = seed
    , mpVersion = ver
    , mpParams = params
    }

putText :: Text -> Put
putText txt = do
  let bytes = encodeUtf8 txt
  putWord32le (fromIntegral (BS.length bytes))
  putByteString bytes

getText :: Get Text
getText = do
  len <- fromIntegral <$> getWord32le
  bytes <- getByteString len
  case decodeUtf8' bytes of
    Left _ -> fail "decode: invalid utf8"
    Right txt -> pure txt

putMetadataStore :: MetadataStore -> Put
putMetadataStore (MetadataStore hex region) = do
  putMetadataMap putHexCoord hex
  putMetadataMap putRegionId region

getMetadataStore :: [MetadataCodec] -> Get MetadataStore
getMetadataStore codecs = do
  hex <- getMetadataMap codecs getHexCoord
  region <- getMetadataMap codecs getRegionId
  pure (MetadataStore hex region)

putMetadataMap :: (k -> Put) -> Map k (Map Text SomeMetadata) -> Put
putMetadataMap putKey entries = do
  putWord32le (fromIntegral (Map.size entries))
  mapM_ (putMetadataEntry putKey) (Map.toList entries)

putMetadataEntry :: (k -> Put) -> (k, Map Text SomeMetadata) -> Put
putMetadataEntry putKey (key, entries) = do
  putKey key
  putWord32le (fromIntegral (Map.size entries))
  mapM_ putMetadataValue (Map.toList entries)

putMetadataValue :: (Text, SomeMetadata) -> Put
putMetadataValue (key, SomeMetadata version val) = do
  putText key
  putWord32le version
  putText (metadataEncode val)

getMetadataMap :: Ord k => [MetadataCodec] -> Get k -> Get (Map k (Map Text SomeMetadata))
getMetadataMap codecs getKey = do
  count <- fromIntegral <$> getWord32le
  entries <- replicateM count (getMetadataEntry codecs getKey)
  pure (Map.fromList entries)

getMetadataEntry :: [MetadataCodec] -> Get k -> Get (k, Map Text SomeMetadata)
getMetadataEntry codecs getKey = do
  key <- getKey
  count <- fromIntegral <$> getWord32le
  values <- replicateM count (getMetadataValue codecs)
  pure (key, Map.fromList values)

getMetadataValue :: [MetadataCodec] -> Get (Text, SomeMetadata)
getMetadataValue codecs = do
  key <- getText
  version <- getWord32le
  payload <- getText
  case lookupCodec key codecs of
    Nothing -> fail ("decode: unknown metadata key " <> Text.unpack key)
    Just decoder ->
      case decoder version payload of
        Left err -> fail ("decode: " <> show err)
        Right val -> pure (key, val)

lookupCodec :: Text -> [MetadataCodec] -> Maybe (Word32 -> Text -> Either MetadataDecodeError SomeMetadata)
lookupCodec _ [] = Nothing
lookupCodec key (codec:rest)
  | mcKey codec == key = Just (mcDecode codec)
  | otherwise = lookupCodec key rest

-- | Encode a 'PlanetConfig' to the binary stream.
putPlanetConfig :: PlanetConfig -> Put
putPlanetConfig pc = do
  putFloatle (pcRadius pc)
  putFloatle (pcAxialTilt pc)
  putFloatle (pcInsolation pc)

-- | Decode a 'PlanetConfig' from the binary stream.
getPlanetConfig :: Get PlanetConfig
getPlanetConfig = do
  radius <- getFloatle
  tilt <- getFloatle
  insol <- getFloatle
  pure PlanetConfig
    { pcRadius = radius
    , pcAxialTilt = tilt
    , pcInsolation = insol
    }

-- | Encode a 'WorldSlice' to the binary stream.
putWorldSlice :: WorldSlice -> Put
putWorldSlice ws = do
  putFloatle (wsLatCenter ws)
  putFloatle (wsLatExtent ws)
  putFloatle (wsLonCenter ws)
  putFloatle (wsLonExtent ws)

-- | Decode a 'WorldSlice' from the binary stream.
getWorldSlice :: Get WorldSlice
getWorldSlice = do
  latC <- getFloatle
  latE <- getFloatle
  lonC <- getFloatle
  lonE <- getFloatle
  pure WorldSlice
    { wsLatCenter = latC
    , wsLatExtent = latE
    , wsLonCenter = lonC
    , wsLonExtent = lonE
    }

-- | Encode the generation config as a length-prefixed JSON blob.
--
-- @Nothing@ is stored as a zero-length blob.  @Just cfg@ is encoded
-- as @word32le(len) ++ jsonBytes@.
putGenConfig :: Maybe Value -> Put
putGenConfig Nothing = putWord32le 0
putGenConfig (Just cfg) = do
  let jsonBytes = BL.toStrict (encode cfg)
  putWord32le (fromIntegral (BS.length jsonBytes))
  putByteString jsonBytes

-- | Decode the generation config from a length-prefixed JSON blob.
--
-- A zero-length blob yields 'Nothing'.  A non-zero blob is decoded
-- via aeson; parse failure falls back to 'Nothing' (forward
-- compatibility: if a future version adds fields that the current
-- version's type doesn't know, we still load the world).
getGenConfig :: Get (Maybe Value)
getGenConfig = do
  len <- fromIntegral <$> getWord32le
  if len == 0
    then pure Nothing
    else do
      jsonBytes <- getByteString len
      pure (decode (BL.fromStrict jsonBytes))

-- | Encode a 'UnitScales' as 10 little-endian Float32 fields.
putUnitScales :: UnitScales -> Put
putUnitScales us = do
  putFloatle (usTempScale us)
  putFloatle (usTempOffset us)
  putFloatle (usElevRange us)
  putFloatle (usWaterLevel us)
  putFloatle (usElevGradient us)
  putFloatle (usPrecipScale us)
  putFloatle (usWindScale us)
  putFloatle (usPressureMin us)
  putFloatle (usPressureRange us)
  putFloatle (usSoilScale us)

-- | Decode a 'UnitScales' from 10 little-endian Float32 fields.
getUnitScales :: Get UnitScales
getUnitScales = do
  tempScale     <- getFloatle
  tempOffset    <- getFloatle
  elevRange     <- getFloatle
  waterLevel    <- getFloatle
  elevGradient  <- getFloatle
  precipScale   <- getFloatle
  windScale     <- getFloatle
  pressureMin   <- getFloatle
  pressureRange <- getFloatle
  soilScale     <- getFloatle
  pure UnitScales
    { usTempScale     = tempScale
    , usTempOffset    = tempOffset
    , usElevRange     = elevRange
    , usWaterLevel    = waterLevel
    , usElevGradient  = elevGradient
    , usPrecipScale   = precipScale
    , usWindScale     = windScale
    , usPressureMin   = pressureMin
    , usPressureRange = pressureRange
    , usSoilScale     = soilScale
    }

-- | Encode a 'WorldTime' as Word64 (tick) + Float64 (tickRate).
putWorldTime :: WorldTime -> Put
putWorldTime wt = do
  putWord64le (wtTick wt)
  putDoublele (wtTickRate wt)

-- | Decode a 'WorldTime' from Word64 + Float64.
getWorldTime :: Get WorldTime
getWorldTime = do
  tick     <- getWord64le
  tickRate <- getDoublele
  pure WorldTime { wtTick = tick, wtTickRate = tickRate }

-- | Encode a 'PlanetAge' as Float64 (years).
putPlanetAge :: PlanetAge -> Put
putPlanetAge pa =
  putDoublele (paYears pa)

-- | Decode a 'PlanetAge' from Float64.
getPlanetAge :: Get PlanetAge
getPlanetAge = do
  years <- getDoublele
  pure PlanetAge { paYears = years }

-- | Encode the overlay manifest as a counted list of length-prefixed texts.
putOverlayManifest :: [Text] -> Put
putOverlayManifest names = do
  putWord32le (fromIntegral (length names))
  mapM_ putText names

-- | Decode the overlay manifest: a counted list of overlay names.
getOverlayManifest :: Get [Text]
getOverlayManifest = do
  count <- fromIntegral <$> getWord32le
  replicateM count getText

putHexCoord :: HexCoord -> Put
putHexCoord coord =
  case coord of
    HexAxial q r -> do
      putWord32le hexCoordTagAxial
      putInt32le (fromIntegral q)
      putInt32le (fromIntegral r)
    HexCube x y z -> do
      putWord32le hexCoordTagCube
      putInt32le (fromIntegral x)
      putInt32le (fromIntegral y)
      putInt32le (fromIntegral z)

getHexCoord :: Get HexCoord
getHexCoord = do
  tag <- getWord32le
  case tag of
    _ | tag == hexCoordTagAxial -> do
          q <- fromIntegral <$> getInt32le
          r <- fromIntegral <$> getInt32le
          pure (HexAxial q r)
      | tag == hexCoordTagCube -> do
          x <- fromIntegral <$> getInt32le
          y <- fromIntegral <$> getInt32le
          z <- fromIntegral <$> getInt32le
          pure (HexCube x y z)
      | otherwise -> fail "decode: unknown hex coord tag"

hexCoordTagAxial :: Word32
hexCoordTagAxial = 0

hexCoordTagCube :: Word32
hexCoordTagCube = 1

putRegionId :: RegionId -> Put
putRegionId (RegionId rid) =
  putInt32le (fromIntegral rid)

getRegionId :: Get RegionId
getRegionId =
  RegionId . fromIntegral <$> getInt32le

getChunkMap :: (WorldConfig -> BS.ByteString -> Either ExportError a) -> WorldConfig -> Get (IntMap a)
getChunkMap decoder config = do
  count <- fromIntegral <$> getWord32le
  entries <- replicateM count (getChunkEntry decoder config)
  pure (IntMap.fromList entries)

getChunkEntry :: (WorldConfig -> BS.ByteString -> Either ExportError a) -> WorldConfig -> Get (Int, a)
getChunkEntry decoder config = do
  key <- fromIntegral <$> getWord32le
  len <- fromIntegral <$> getWord32le
  bytes <- getByteString len
  case decoder config bytes of
    Left err -> fail ("decode: " <> show err)
    Right chunk -> pure (key, chunk)
