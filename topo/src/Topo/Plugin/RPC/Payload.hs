{-# LANGUAGE OverloadedStrings #-}

-- | Terrain payload codecs for plugin RPC.
--
-- This module owns the payload contract for terrain/chunk transport,
-- including base64 chunk encoding and decode/apply helpers.
module Topo.Plugin.RPC.Payload
  ( terrainWorldToPayload
  , terrainWorldToCompletePayload
  , decodeTerrainWritesValue
  , applyGeneratorTerrainValue
  , encodeBase64Text
  , decodeBase64Text
  ) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import qualified Data.ByteString as BS
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8)
import Text.Read (readMaybe)

import qualified Data.Aeson as Aeson
import Data.Aeson (Value(..), (.=), object)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM

import Topo.Calendar (PlanetAge(..), WorldTime(..))
import Topo.Export
  ( ExportError(..)
  , decodeClimateChunk
  , decodeTerrainChunk
  , decodeVegetationChunk
  , encodeBiomeChunk
  , encodeClimateChunk
  , encodeGlacierChunk
  , encodeGroundwaterChunk
  , encodeRiverChunk
  , encodeTerrainChunk
  , encodeVegetationChunk
  , encodeVolcanismChunk
  , encodeWaterBodyChunk
  , encodeWeatherChunk
  )
import Topo.Metadata (MetadataStore(..), SomeMetadata(..), metadataEncode)
import Topo.Overlay (Overlay(..), OverlayProvenance(..), OverlayStore(..), overlayCount, overlayNames)
import Topo.Overlay.JSON (overlayToJSON)
import Topo.Simulation
  ( TerrainWrites(..)
  , applyTerrainWrites
  , emptyTerrainWrites
  )
import Topo.Planet (mkLatitudeMapping)
import qualified Topo.Types
import Topo.Units (UnitScales(..))
import Topo.Weather (getWeatherFromOverlay)
import Topo.World (TerrainWorld(..))
import qualified Topo.World

-- | Encode the capability-scoped terrain RPC payload.
--
-- This is used for plugin @readTerrain@ delivery and intentionally excludes
-- overlays/weather, which are delivered through separate capability gates.
terrainWorldToPayload :: Topo.World.TerrainWorld -> Either Text Value
terrainWorldToPayload world = do
  let config = Topo.World.twConfig world
  terrainObj <- encodeChunkMap (Topo.World.twTerrain world) (encodeTerrainChunk config)
  climateObj <- encodeChunkMap (Topo.World.twClimate world) (encodeClimateChunk config)
  vegetationObj <- encodeChunkMap (Topo.World.twVegetation world) (encodeVegetationChunk config)
  Right $ object
    [ "chunk_count" .= IntMap.size (Topo.World.twTerrain world)
    , "climate_count" .= IntMap.size (Topo.World.twClimate world)
    , "river_count" .= IntMap.size (Topo.World.twRivers world)
    , "vegetation_count" .= IntMap.size (Topo.World.twVegetation world)
    , "chunk_size" .= Topo.Types.wcChunkSize config
    , "hex_grid" .= Topo.World.twHexGrid world
    , "planet" .= Topo.World.twPlanet world
    , "slice" .= Topo.World.twSlice world
    , "encoding" .= ("base64" :: Text)
    , "terrain" .= Object terrainObj
    , "climate" .= Object climateObj
    , "vegetation" .= Object vegetationObj
    ]

-- | Encode the complete Writ/full-world export payload.
--
-- This is not used for capability-scoped plugin terrain reads; callers that
-- need every generated/persisted layer must opt in explicitly.
terrainWorldToCompletePayload :: Topo.World.TerrainWorld -> Either Text Value
terrainWorldToCompletePayload world = do
  let config = Topo.World.twConfig world
      weather = getWeatherFromOverlay world
  terrainObj <- encodeChunkMap (Topo.World.twTerrain world) (encodeTerrainChunk config)
  climateObj <- encodeChunkMap (Topo.World.twClimate world) (encodeClimateChunk config)
  riverObj <- encodeChunkMap (Topo.World.twRivers world) (encodeRiverChunk config)
  groundwaterObj <- encodeChunkMap (Topo.World.twGroundwater world) (encodeGroundwaterChunk config)
  volcanismObj <- encodeChunkMap (Topo.World.twVolcanism world) (encodeVolcanismChunk config)
  glacierObj <- encodeChunkMap (Topo.World.twGlaciers world) (encodeGlacierChunk config)
  waterBodyObj <- encodeChunkMap (Topo.World.twWaterBodies world) (encodeWaterBodyChunk config)
  vegetationObj <- encodeChunkMap (Topo.World.twVegetation world) (encodeVegetationChunk config)
  biomeObj <- encodeChunkMap (Topo.World.twTerrain world) (encodeBiomeChunk config)
  weatherObj <- encodeChunkMap weather (encodeWeatherChunk config)
  Right $ object
    [ "chunk_count" .= IntMap.size (Topo.World.twTerrain world)
    , "climate_count" .= IntMap.size (Topo.World.twClimate world)
    , "river_count" .= IntMap.size (Topo.World.twRivers world)
    , "groundwater_count" .= IntMap.size (Topo.World.twGroundwater world)
    , "volcanism_count" .= IntMap.size (Topo.World.twVolcanism world)
    , "glacier_count" .= IntMap.size (Topo.World.twGlaciers world)
    , "water_body_count" .= IntMap.size (Topo.World.twWaterBodies world)
    , "vegetation_count" .= IntMap.size (Topo.World.twVegetation world)
    , "biome_count" .= IntMap.size (Topo.World.twTerrain world)
    , "weather_count" .= IntMap.size weather
    , "overlay_count" .= overlayCount (Topo.World.twOverlays world)
    , "chunk_size" .= Topo.Types.wcChunkSize config
    , "seed" .= Topo.World.twSeed world
    , "world_time" .= worldTimeToJSON (Topo.World.twWorldTime world)
    , "planet_age" .= planetAgeToJSON (Topo.World.twPlanetAge world)
    , "hex_grid" .= Topo.World.twHexGrid world
    , "planet" .= Topo.World.twPlanet world
    , "slice" .= Topo.World.twSlice world
    , "unit_scales" .= unitScalesToJSON (Topo.World.twUnitScales world)
    , "gen_config" .= Topo.World.twGenConfig world
    , "overlay_manifest" .= Topo.World.twOverlayManifest world
    , "overlay_names" .= overlayNames (Topo.World.twOverlays world)
    , "encoding" .= ("base64" :: Text)
    , "terrain" .= Object terrainObj
    , "climate" .= Object climateObj
    , "rivers" .= Object riverObj
    , "groundwater" .= Object groundwaterObj
    , "volcanism" .= Object volcanismObj
    , "glaciers" .= Object glacierObj
    , "water_bodies" .= Object waterBodyObj
    , "vegetation" .= Object vegetationObj
    , "biomes" .= Object biomeObj
    , "weather" .= Object weatherObj
    , "overlays" .= overlaysToJSON (Topo.World.twOverlays world)
    , "metadata" .= metadataStoreToJSON (Topo.World.twMeta world)
    ]

worldTimeToJSON :: WorldTime -> Value
worldTimeToJSON time = object
  [ "tick" .= wtTick time
  , "tick_rate" .= wtTickRate time
  ]

planetAgeToJSON :: PlanetAge -> Value
planetAgeToJSON age = object
  [ "years" .= paYears age
  ]

unitScalesToJSON :: UnitScales -> Value
unitScalesToJSON scales = object
  [ "temp_scale" .= usTempScale scales
  , "temp_offset" .= usTempOffset scales
  , "elev_range" .= usElevRange scales
  , "water_level" .= usWaterLevel scales
  , "elev_gradient" .= usElevGradient scales
  , "precip_scale" .= usPrecipScale scales
  , "wind_scale" .= usWindScale scales
  , "pressure_min" .= usPressureMin scales
  , "pressure_range" .= usPressureRange scales
  , "soil_scale" .= usSoilScale scales
  ]

overlaysToJSON :: OverlayStore -> Value
overlaysToJSON (OverlayStore overlays) =
  Object (KM.fromList (map overlayEntryToJSON (Map.toList overlays)))

overlayEntryToJSON :: (Text, Overlay) -> (Key.Key, Value)
overlayEntryToJSON (name, overlay) =
  ( Key.fromText name
  , object
      [ "schema" .= ovSchema overlay
      , "payload" .= overlayToJSON overlay
      , "provenance" .= overlayProvenanceToJSON (ovProvenance overlay)
      ]
  )

overlayProvenanceToJSON :: OverlayProvenance -> Value
overlayProvenanceToJSON provenance = object
  [ "seed" .= opSeed provenance
  , "version" .= opVersion provenance
  , "source" .= opSource provenance
  ]

metadataStoreToJSON :: MetadataStore -> Value
metadataStoreToJSON (MetadataStore hexMeta regionMeta) = object
  [ "hex" .= map hexMetadataToJSON (Map.toList hexMeta)
  , "region" .= map regionMetadataToJSON (Map.toList regionMeta)
  ]

hexMetadataToJSON :: (Topo.Types.HexCoord, Map.Map Text SomeMetadata) -> Value
hexMetadataToJSON (coord, entries) = object
  [ "coord" .= show coord
  , "entries" .= map metadataEntryToJSON (Map.toList entries)
  ]

regionMetadataToJSON :: (Topo.Types.RegionId, Map.Map Text SomeMetadata) -> Value
regionMetadataToJSON (regionId, entries) = object
  [ "region_id" .= show regionId
  , "entries" .= map metadataEntryToJSON (Map.toList entries)
  ]

metadataEntryToJSON :: (Text, SomeMetadata) -> Value
metadataEntryToJSON (name, SomeMetadata version value) = object
  [ "key" .= name
  , "version" .= version
  , "payload" .= metadataEncode value
  ]

-- | Decode a simulation @terrain_writes@ payload into structured writes.
decodeTerrainWritesValue :: Maybe Value -> Either Text TerrainWrites
decodeTerrainWritesValue = decodeTerrainWrites

-- | Apply a generator terrain payload to a world by decoding and merging
-- chunk updates.
applyGeneratorTerrainValue :: Topo.World.TerrainWorld -> Value -> Either Text Topo.World.TerrainWorld
applyGeneratorTerrainValue world Null = Right world
applyGeneratorTerrainValue world (Object obj)
  | KM.null obj = Right world
  | hasOnlySummaryKeys obj = Right world
  | otherwise = applyTerrainPayload world obj
applyGeneratorTerrainValue _ _ = Left "generator terrain payload must be an object or null"

decodeTerrainWrites :: Maybe Value -> Either Text TerrainWrites
decodeTerrainWrites Nothing = Right emptyTerrainWrites
decodeTerrainWrites (Just Null) = Right emptyTerrainWrites
decodeTerrainWrites (Just (Object obj))
  | KM.null obj = Right emptyTerrainWrites
  | hasOnlySummaryKeys obj = Right emptyTerrainWrites
  | otherwise = terrainWritesFromPayload obj
decodeTerrainWrites (Just _) = Left "terrain_writes payload must be an object"

hasOnlySummaryKeys :: KM.KeyMap Value -> Bool
hasOnlySummaryKeys keyMap = all (`elem` allowedKeys) (map Key.toText (KM.keys keyMap))
  where
    allowedKeys =
      [ "chunk_count"
      , "climate_count"
      , "river_count"
      , "groundwater_count"
      , "volcanism_count"
      , "glacier_count"
      , "water_body_count"
      , "vegetation_count"
      , "biome_count"
      , "weather_count"
      , "overlay_count"
      , "chunk_size"
      , "seed"
      , "world_time"
      , "planet_age"
      , "unit_scales"
      , "gen_config"
      , "overlay_manifest"
      , "overlay_names"
      , "encoding"
      , "metadata"
      ]

encodeChunkMap
  :: IntMap.IntMap a
  -> (a -> Either ExportError BS.ByteString)
  -> Either Text (KM.KeyMap Value)
encodeChunkMap chunks encodeChunk = do
  pairs <- traverse encodeOne (IntMap.toList chunks)
  Right (KM.fromList pairs)
  where
    encodeOne (chunkId, chunk) = do
      encoded <- firstExportError (encodeChunk chunk)
      Right
        ( Key.fromText (Text.pack (show chunkId))
        , Aeson.String (encodeBase64Text encoded)
        )

firstExportError :: Either ExportError a -> Either Text a
firstExportError (Right value) = Right value
firstExportError (Left err) = Left (renderExportError err)

renderExportError :: ExportError -> Text
renderExportError err =
  case err of
    ExportLengthMismatch label expected actual ->
      "length mismatch for " <> label
        <> ": expected " <> tshow expected
        <> ", actual " <> tshow actual
    ExportDecodeError msg ->
      "decode error: " <> msg

tshow :: Show a => a -> Text
tshow = Text.pack . show

terrainWritesFromPayload :: KM.KeyMap Value -> Either Text TerrainWrites
terrainWritesFromPayload payload =
  terrainWritesFromPayloadWithConfig (lookupChunkSize payload) payload

terrainWritesFromPayloadWithConfig :: Topo.Types.WorldConfig -> KM.KeyMap Value -> Either Text TerrainWrites
terrainWritesFromPayloadWithConfig payloadConfig payload = do
  ensureTerrainPayloadEncoding payload
  terrain <- decodeChunkSection payload "terrain" decodeTerrainChunk payloadConfig
  climate <- decodeChunkSection payload "climate" decodeClimateChunk payloadConfig
  vegetation <- decodeChunkSection payload "vegetation" decodeVegetationChunk payloadConfig
  Right TerrainWrites
    { twrTerrain = terrain
    , twrClimate = climate
    , twrVegetation = vegetation
    }

applyTerrainPayload
  :: Topo.World.TerrainWorld
  -> KM.KeyMap Value
  -> Either Text Topo.World.TerrainWorld
applyTerrainPayload world payload = do
  worldWithHeader <- applyTerrainPayloadHeader world payload
  writes <- terrainWritesFromPayloadWithConfig (twConfig worldWithHeader) payload
  Right (applyTerrainWrites writes worldWithHeader)

applyTerrainPayloadHeader :: TerrainWorld -> KM.KeyMap Value -> Either Text TerrainWorld
applyTerrainPayloadHeader world payload = do
  let config = lookupChunkSizeOr (twConfig world) payload
  hexMeta <- lookupPayloadField "hex_grid" (twHexGrid world) payload
  planet <- lookupPayloadField "planet" (twPlanet world) payload
  slice <- lookupPayloadField "slice" (twSlice world) payload
  Right world
    { twConfig = config
    , twHexGrid = hexMeta
    , twPlanet = planet
    , twSlice = slice
    , twLatMapping = mkLatitudeMapping planet hexMeta slice config
    }

lookupPayloadField
  :: Aeson.FromJSON a
  => Text
  -> a
  -> KM.KeyMap Value
  -> Either Text a
lookupPayloadField fieldName fallback payload =
  case KM.lookup (Key.fromText fieldName) payload of
    Nothing -> Right fallback
    Just value ->
      case Aeson.fromJSON value of
        Aeson.Error err -> Left ("invalid terrain payload field " <> fieldName <> ": " <> Text.pack err)
        Aeson.Success decoded -> Right decoded

decodeChunkSection
  :: KM.KeyMap Value
  -> Text
  -> (Topo.Types.WorldConfig -> BS.ByteString -> Either ExportError a)
  -> Topo.Types.WorldConfig
  -> Either Text (IntMap.IntMap a)
decodeChunkSection payload fieldName decodeChunk config =
  case KM.lookup (Key.fromText fieldName) payload of
    Nothing -> Right IntMap.empty
    Just Null -> Right IntMap.empty
    Just (Object chunkMap) ->
      IntMap.fromList <$> traverse decodeOne (KM.toList chunkMap)
    Just _ -> Left (fieldName <> " payload must be an object")
  where
    decodeOne (chunkKey, rawChunkBytes) = do
      chunkId <- parseChunkId (Key.toText chunkKey)
      bytes <- decodeChunkBytes rawChunkBytes
      decoded <- firstExportError (decodeChunk config bytes)
      Right (chunkId, decoded)

parseChunkId :: Text -> Either Text Int
parseChunkId rawChunkId =
  case readMaybe (Text.unpack rawChunkId) of
    Nothing -> Left ("invalid chunk id: " <> rawChunkId)
    Just chunkId -> Right chunkId

decodeChunkBytes :: Value -> Either Text BS.ByteString
decodeChunkBytes (Aeson.String raw) = decodeBase64Text raw
decodeChunkBytes _ = Left "chunk payload must be a base64 string"

ensureTerrainPayloadEncoding :: KM.KeyMap Value -> Either Text ()
ensureTerrainPayloadEncoding payload =
  case KM.lookup "encoding" payload of
    Just (Aeson.String "base64") -> Right ()
    Just (Aeson.String value) -> Left ("unsupported terrain payload encoding: " <> value)
    Just _ -> Left "terrain payload encoding must be a string"
    Nothing -> Left "terrain payload missing required encoding field"

lookupChunkSize :: KM.KeyMap Value -> Topo.Types.WorldConfig
lookupChunkSize = lookupChunkSizeOr defaultConfig
  where
    defaultConfig = Topo.Types.WorldConfig { Topo.Types.wcChunkSize = 64 }

lookupChunkSizeOr :: Topo.Types.WorldConfig -> KM.KeyMap Value -> Topo.Types.WorldConfig
lookupChunkSizeOr fallback payload =
  case KM.lookup "chunk_size" payload >>= valueToPositiveInt of
    Just chunkSize -> Topo.Types.WorldConfig { Topo.Types.wcChunkSize = chunkSize }
    Nothing -> fallback

valueToPositiveInt :: Value -> Maybe Int
valueToPositiveInt (Number n) =
  let asInteger = floor n :: Integer
  in if fromInteger asInteger == n && asInteger > 0 && asInteger <= fromIntegral (maxBound :: Int)
      then Just (fromInteger asInteger)
      else Nothing
valueToPositiveInt _ = Nothing

base64Alphabet :: Text
base64Alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

encodeBase64Text :: BS.ByteString -> Text
encodeBase64Text bytes = Text.pack (go 0)
  where
    len = BS.length bytes

    at :: Int -> Int
    at index = fromIntegral (BS.index bytes index)

    emit :: Int -> Char
    emit index = Text.index base64Alphabet index

    go :: Int -> String
    go index
      | index >= len = []
      | index + 2 < len =
          let b0 = at index
              b1 = at (index + 1)
              b2 = at (index + 2)
              c0 = emit (b0 `shiftR` 2)
              c1 = emit (((b0 .&. 0x03) `shiftL` 4) .|. (b1 `shiftR` 4))
              c2 = emit (((b1 .&. 0x0F) `shiftL` 2) .|. (b2 `shiftR` 6))
              c3 = emit (b2 .&. 0x3F)
          in c0 : c1 : c2 : c3 : go (index + 3)
      | index + 1 < len =
          let b0 = at index
              b1 = at (index + 1)
              c0 = emit (b0 `shiftR` 2)
              c1 = emit (((b0 .&. 0x03) `shiftL` 4) .|. (b1 `shiftR` 4))
              c2 = emit ((b1 .&. 0x0F) `shiftL` 2)
          in [c0, c1, c2, '=']
      | otherwise =
          let b0 = at index
              c0 = emit (b0 `shiftR` 2)
              c1 = emit ((b0 .&. 0x03) `shiftL` 4)
          in [c0, c1, '=', '=']

decodeBase64Text :: Text -> Either Text BS.ByteString
decodeBase64Text raw = do
  sextets <- traverse decodeChar (Text.unpack raw)
  bytes <- decodeSextets sextets
  Right (BS.pack bytes)
  where
    decodeChar :: Char -> Either Text Int
    decodeChar '=' = Right (-1)
    decodeChar ch =
      case Text.findIndex (== ch) base64Alphabet of
        Just index -> Right index
        Nothing -> Left ("invalid base64 character: " <> Text.singleton ch)

    decodeSextets :: [Int] -> Either Text [Word8]
    decodeSextets values
      | null values = Right []
      | (length values `mod` 4) /= 0 = Left "invalid base64 length"
      | otherwise = go values

    go :: [Int] -> Either Text [Word8]
    go [] = Right []
    go (a:b:c:d:rest)
      | a < 0 || b < 0 = Left "invalid base64 padding"
      | c == (-1) && d /= (-1) = Left "invalid base64 padding"
      | otherwise = do
          let byte0 = fromIntegral (((a `shiftL` 2) .|. (b `shiftR` 4)) .&. 0xFF)
          suffix <- case (c, d) of
            (-1, -1) ->
              if null rest
                then Right []
                else Left "invalid base64 padding location"
            (cVal, -1) | cVal >= 0 ->
              if null rest
                then
                  let byte1 = fromIntegral ((((b .&. 0x0F) `shiftL` 4) .|. (cVal `shiftR` 2)) .&. 0xFF)
                  in Right [byte1]
                else Left "invalid base64 padding location"
            (cVal, dVal) | cVal >= 0 && dVal >= 0 -> do
              let byte1 = fromIntegral ((((b .&. 0x0F) `shiftL` 4) .|. (cVal `shiftR` 2)) .&. 0xFF)
                  byte2 = fromIntegral ((((cVal .&. 0x03) `shiftL` 6) .|. dVal) .&. 0xFF)
              more <- go rest
              Right (byte1 : byte2 : more)
            _ -> Left "invalid base64 sextet"
          Right (byte0 : suffix)
    go _ = Left "invalid base64 quartet"