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
  , emptyMapProvenance
  , encodeProvenance
  , decodeProvenance
  ) where

import Control.Monad (replicateM)
import Data.Binary.Get (Get, getByteString, getFloatle, getInt32le, getWord32le, getWord64le, runGetOrFail)
import Data.Binary.Put (Put, putByteString, putFloatle, putInt32le, putWord32le, putWord64le, runPut)
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
  , decodeClimateChunkV1
  , decodeGroundwaterChunk
  , decodeVolcanismChunk
  , decodeGlacierChunk
  , decodeRiverChunk
  , decodeRiverChunkV1
  , decodeRiverChunkV2
  , decodeTerrainChunk
  , decodeTerrainChunkV2
  , decodeVegetationChunk
  , decodeVegetationChunkV1
  , decodeWaterBodyChunk
  , decodeWeatherChunk
  , encodeClimateChunk
  , encodeGroundwaterChunk
  , encodeVolcanismChunk
  , encodeGlacierChunk
  , encodeRiverChunk
  , encodeTerrainChunk
  , encodeVegetationChunk
  , encodeWaterBodyChunk
  , encodeWeatherChunk
  )
import Topo.Hex (HexGridMeta(..), defaultHexGridMeta)
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
import Topo.Planet (PlanetConfig(..), WorldSlice(..), defaultPlanetConfig, defaultWorldSlice)
import Topo.Types
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

saveWorld :: FilePath -> TerrainWorld -> IO (Either StorageError ())
saveWorld path world =
  case encodeWorld world of
    Left err -> pure (Left err)
    Right bytes -> Right <$> BS.writeFile path bytes

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
-- Layout (version 9):
--   magic, version, chunkSize, hexSize, provenance, metadata,
--   planetConfig, worldSlice,
--   terrain chunks, climate chunks, weather chunks, river chunks, groundwater chunks,
--   glacier chunks, volcanism chunks.
encodeWorldWithProvenance :: WorldProvenance -> TerrainWorld -> Either StorageError BS.ByteString
encodeWorldWithProvenance prov world = do
  let config = twConfig world
  terrain <- encodeChunkMap config encodeTerrainChunk (twTerrain world)
  climate <- encodeChunkMap config encodeClimateChunk (twClimate world)
  weather <- encodeChunkMap config encodeWeatherChunk (twWeather world)
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
    putFloatle (hexSize (twHexGrid world))
    encodeProvenance prov
    putMetadataStore (twMeta world)
    putPlanetConfig (twPlanet world)
    putWorldSlice (twSlice world)
    putFloatle (twWorldTime world)
    putChunkMapBytes terrain
    putChunkMapBytes climate
    putChunkMapBytes weather
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
--
-- Version history:
--   * 1: base terrain/climate/weather chunks with provenance.
--   * 2: expanded provenance fields for pipeline stages.
--   * 3: terrain chunk schema includes plate height/hardness fields.
--   * 4: metadata store included (twMeta) with typed codecs.
--   * 5: river + groundwater chunk maps added.
--   * 6: river chunks include erosion/deposition potentials.
--   * 7: glacier chunks added.
--   * 8: volcanism chunks added.
--   * 9: planet config + world slice fields added.
--         PlanetConfig: radius (Float32), axialTilt (Float32),
--         insolation (Float32).
--         WorldSlice: latCenter (Float32), latExtent (Float32),
--         lonCenter (Float32), lonExtent (Float32).
--         Files < v9 decode with 'defaultPlanetConfig' / 'defaultWorldSlice'.
--   * 10: world time field (twWorldTime :: Float32) added.
--         Files < v10 decode with twWorldTime = 0.
--   * 11: river chunks include flow-direction and segment topology.
--   * 12: water body chunks and vegetation chunks serialized.
--         Vegetation chunks now include 'vegDensity'.
--         Files < v12 decode with empty waterBodies/vegetation.
--   * 13: climate chunks include humidity average, temperature range,
--         and precipitation seasonality.
--         Files < v13 decode with legacy 4-field climate chunks.
fileVersion :: Word32
fileVersion = 13

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

-- | Decode a world file with provenance and optional metadata.
--
-- The decoder is versioned and selects the correct terrain chunk schema
-- (see decodeTerrainChunkV2 for legacy v2 formats).
getWorldWithProvenance :: [MetadataCodec] -> Get (WorldProvenance, TerrainWorld)
getWorldWithProvenance codecs = do
  fileMagic <- getByteString 4
  if fileMagic /= magic
    then fail "decode: invalid magic"
    else pure ()
  version <- getWord32le
  if version < 1 || version > fileVersion
    then fail "decode: unsupported version"
    else pure ()
  size <- fromIntegral <$> getWord32le
  hex <- getFloatle
  prov <- decodeProvenanceForVersion version
  metaStore <- if version >= 4
    then getMetadataStore codecs
    else pure emptyMetadataStore
  config <- case mkWorldConfig size of
    Left err -> fail ("decode: invalid chunk size (" <> show err <> ")")
    Right cfg -> pure cfg
  let hexMeta = HexGridMeta { hexSize = hex }
  (planet, slice) <- if version >= 9
    then (,) <$> getPlanetConfig <*> getWorldSlice
    else pure (defaultPlanetConfig, defaultWorldSlice)
  worldTime <- if version >= 10
    then getFloatle
    else pure 0
  let terrainDecoder = if version < 3 then decodeTerrainChunkV2 else decodeTerrainChunk
  terrain <- getChunkMap terrainDecoder config
  climate <- getChunkMap (if version >= 13 then decodeClimateChunk else decodeClimateChunkV1) config
  weather <- getChunkMap decodeWeatherChunk config
  rivers <- if version >= 5
    then getChunkMap (if version >= 11 then decodeRiverChunk
                      else if version >= 6 then decodeRiverChunkV2
                      else decodeRiverChunkV1) config
    else pure IntMap.empty
  groundwater <- if version >= 5 then getChunkMap decodeGroundwaterChunk config else pure IntMap.empty
  glaciers <- if version >= 7 then getChunkMap decodeGlacierChunk config else pure IntMap.empty
  volcanism <- if version >= 8 then getChunkMap decodeVolcanismChunk config else pure IntMap.empty
  waterBodies <- if version >= 12 then getChunkMap decodeWaterBodyChunk config else pure IntMap.empty
  vegetation <- if version >= 12
    then getChunkMap decodeVegetationChunk config
    else pure IntMap.empty
  pure
    ( prov
    , TerrainWorld
        { twTerrain = terrain
        , twClimate = climate
        , twWeather = weather
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
        , twWorldTime = worldTime
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
  , wpWeather :: !MapProvenance
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
  , wpWeather = emptyMapProvenance
  , wpBiome = emptyMapProvenance
  }

encodeProvenance :: WorldProvenance -> Put
encodeProvenance prov = do
  putWord64le (wpSeed prov)
  putWord32le (wpVersion prov)
  putText (wpNotes prov)
  encodeMapProvenance (wpTerrain prov)
  encodeMapProvenance (wpClimate prov)
  encodeMapProvenance (wpWeather prov)
  encodeMapProvenance (wpBiome prov)

decodeProvenance :: Get WorldProvenance
decodeProvenance = decodeProvenanceV2

decodeProvenanceForVersion :: Word32 -> Get WorldProvenance
decodeProvenanceForVersion version
  | version == 1 = decodeProvenanceV1
  | otherwise = decodeProvenanceV2

decodeProvenanceV1 :: Get WorldProvenance
decodeProvenanceV1 = do
  seed <- getWord64le
  ver <- getWord32le
  notes <- getText
  pure WorldProvenance
    { wpSeed = seed
    , wpVersion = ver
    , wpNotes = notes
    , wpTerrain = emptyMapProvenance
    , wpClimate = emptyMapProvenance
    , wpWeather = emptyMapProvenance
    , wpBiome = emptyMapProvenance
    }

decodeProvenanceV2 :: Get WorldProvenance
decodeProvenanceV2 = do
  seed <- getWord64le
  ver <- getWord32le
  notes <- getText
  terrain <- decodeMapProvenance
  climate <- decodeMapProvenance
  weather <- decodeMapProvenance
  biome <- decodeMapProvenance
  pure WorldProvenance
    { wpSeed = seed
    , wpVersion = ver
    , wpNotes = notes
    , wpTerrain = terrain
    , wpClimate = climate
    , wpWeather = weather
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
