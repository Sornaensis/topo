{-# LANGUAGE OverloadedStrings #-}

-- | Terrain payload codecs for plugin RPC.
--
-- This module owns the payload contract for terrain/chunk transport,
-- including base64 chunk encoding and decode/apply helpers.
module Topo.Plugin.RPC.Payload
  ( terrainWorldToPayload
  , terrainWorldToCompletePayload
  , decodeTerrainWritesValue
  , decodeTerrainWritesValueWithLimits
  , terrainWritesValueEmpty
  , applyGeneratorTerrainValue
  , applyGeneratorTerrainValueWithLimits
  , encodeBase64Text
  , decodeBase64Text
  ) where

import Control.Monad (foldM, unless, when)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import qualified Data.ByteString as BS
import Data.Foldable (traverse_)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
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
import Topo.Plugin.RPC.Transport
  ( RPCPayloadLimits
  , defaultRPCPayloadLimits
  , rplMaxDecodedTerrainBytes
  )
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
decodeTerrainWritesValue = decodeTerrainWritesValueWithLimits defaultRPCPayloadLimits

-- | Decode terrain writes using an explicit aggregate decoded-byte budget.
decodeTerrainWritesValueWithLimits :: RPCPayloadLimits -> Maybe Value -> Either Text TerrainWrites
decodeTerrainWritesValueWithLimits = decodeTerrainWrites

-- | Report whether an optional simulation @terrain_writes@ payload contains
-- no actual terrain mutations.
terrainWritesValueEmpty :: Maybe Value -> Either Text Bool
terrainWritesValueEmpty Nothing = Right True
terrainWritesValueEmpty (Just Null) = Right True
terrainWritesValueEmpty (Just (Object obj))
  | KM.null obj = Right True
  | hasOnlySummaryKeys obj = validateSummaryOnly defaultConfig obj >> Right True
  | otherwise = terrainWritesObjectEmpty obj
  where
    defaultConfig = Topo.Types.WorldConfig { Topo.Types.wcChunkSize = 64 }
terrainWritesValueEmpty (Just _) = Left "terrain_writes payload must be an object"

-- | Apply a generator terrain payload to a world by decoding and merging
-- chunk updates.
applyGeneratorTerrainValue :: Topo.World.TerrainWorld -> Value -> Either Text Topo.World.TerrainWorld
applyGeneratorTerrainValue = applyGeneratorTerrainValueWithLimits defaultRPCPayloadLimits

-- | Apply a generator payload with explicit decoded payload limits.
applyGeneratorTerrainValueWithLimits
  :: RPCPayloadLimits
  -> Topo.World.TerrainWorld
  -> Value
  -> Either Text Topo.World.TerrainWorld
applyGeneratorTerrainValueWithLimits _ world Null = Right world
applyGeneratorTerrainValueWithLimits limits world (Object obj)
  | KM.null obj = Right world
  | hasOnlySummaryKeys obj = validateSummaryOnly (twConfig world) obj >> Right world
  | otherwise = applyTerrainPayload limits world obj
applyGeneratorTerrainValueWithLimits _ _ _ = Left "generator terrain payload must be an object or null"

decodeTerrainWrites :: RPCPayloadLimits -> Maybe Value -> Either Text TerrainWrites
decodeTerrainWrites _ Nothing = Right emptyTerrainWrites
decodeTerrainWrites _ (Just Null) = Right emptyTerrainWrites
decodeTerrainWrites limits (Just (Object obj))
  | KM.null obj = Right emptyTerrainWrites
  | hasOnlySummaryKeys obj = validateSummaryOnly defaultConfig obj >> Right emptyTerrainWrites
  | otherwise = terrainWritesFromPayload limits obj
  where
    defaultConfig = Topo.Types.WorldConfig { Topo.Types.wcChunkSize = 64 }
decodeTerrainWrites _ (Just _) = Left "terrain_writes payload must be an object"

hasOnlySummaryKeys :: KM.KeyMap Value -> Bool
hasOnlySummaryKeys keyMap = all (`elem` terrainPayloadSummaryKeys) (map Key.toText (KM.keys keyMap))

terrainPayloadSummaryKeys :: [Text]
terrainPayloadSummaryKeys =
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

validateSummaryOnly
  :: Topo.Types.WorldConfig
  -> KM.KeyMap Value
  -> Either Text ()
validateSummaryOnly fallback payload = do
  config <- lookupChunkSizeOrEither fallback payload
  _ <- checkedTileCount config
  traverse_ validateZeroCountField summaryCountFields
  case KM.lookup "encoding" payload of
    Nothing -> Right ()
    Just (String "base64") -> Right ()
    Just (String value) -> Left ("unsupported terrain payload encoding: " <> value)
    Just _ -> Left "terrain payload encoding must be a string"
  where
    validateZeroCountField fieldName =
      case KM.lookup (Key.fromText fieldName) payload of
        Nothing -> Right ()
        Just value -> case valueToNonNegativeInt value of
          Nothing -> Left (fieldName <> " must be a non-negative platform Int")
          Just 0 -> Right ()
          Just declared -> Left
            (fieldName <> " mismatch: declared=" <> tshow declared <> ", actual=0")
    summaryCountFields =
      [ "chunk_count", "climate_count", "river_count", "groundwater_count"
      , "volcanism_count", "glacier_count", "water_body_count"
      , "vegetation_count", "biome_count", "weather_count", "overlay_count"
      ]

terrainWriteSectionKeys :: [Text]
terrainWriteSectionKeys = ["terrain", "climate", "vegetation"]

terrainWritesObjectEmpty :: KM.KeyMap Value -> Either Text Bool
terrainWritesObjectEmpty payload = do
  ensureTerrainPayloadEncoding payload
  let allowedKeys = terrainPayloadSummaryKeys <> terrainWriteSectionKeys
      unsupportedKeys = filter (`notElem` allowedKeys) (map Key.toText (KM.keys payload))
  case unsupportedKeys of
    [] -> do
      sectionsEmpty <- traverse terrainWriteSectionEmpty terrainWriteSectionKeys
      Right (and sectionsEmpty)
    _ -> Left
      ("terrain_writes payload contains unsupported keys: "
        <> Text.intercalate ", " unsupportedKeys)
  where
    terrainWriteSectionEmpty fieldName =
      case KM.lookup (Key.fromText fieldName) payload of
        Nothing -> Right True
        Just Null -> Right True
        Just (Object chunks) -> Right (KM.null chunks)
        Just _ -> Left (fieldName <> " payload must be an object")

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

terrainWritesFromPayload :: RPCPayloadLimits -> KM.KeyMap Value -> Either Text TerrainWrites
terrainWritesFromPayload limits payload = do
  payloadConfig <- lookupChunkSizeOrEither defaultConfig payload
  terrainWritesFromPayloadWithConfig limits payloadConfig payload
  where
    defaultConfig = Topo.Types.WorldConfig { Topo.Types.wcChunkSize = 64 }

terrainWritesFromPayloadWithConfig
  :: RPCPayloadLimits
  -> Topo.Types.WorldConfig
  -> KM.KeyMap Value
  -> Either Text TerrainWrites
terrainWritesFromPayloadWithConfig limits payloadConfig payload = do
  validateTerrainPayload limits payloadConfig payload
  terrain <- decodeChunkSection payload "terrain" decodeTerrainChunk payloadConfig
  climate <- decodeChunkSection payload "climate" decodeClimateChunk payloadConfig
  vegetation <- decodeChunkSection payload "vegetation" decodeVegetationChunk payloadConfig
  Right TerrainWrites
    { twrTerrain = terrain
    , twrClimate = climate
    , twrVegetation = vegetation
    }

applyTerrainPayload
  :: RPCPayloadLimits
  -> Topo.World.TerrainWorld
  -> KM.KeyMap Value
  -> Either Text Topo.World.TerrainWorld
applyTerrainPayload limits world payload = do
  worldWithHeader <- applyTerrainPayloadHeader world payload
  writes <- terrainWritesFromPayloadWithConfig limits (twConfig worldWithHeader) payload
  Right (applyTerrainWrites writes worldWithHeader)

applyTerrainPayloadHeader :: TerrainWorld -> KM.KeyMap Value -> Either Text TerrainWorld
applyTerrainPayloadHeader world payload = do
  config <- lookupChunkSizeOrEither (twConfig world) payload
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

data ChunkBinaryLayout
  = FixedBytesPerTile !Integer
  | GroundwaterBytes
  | RiverBytes

data ChunkSectionSpec = ChunkSectionSpec
  { cssField :: !Text
  , cssCountField :: !Text
  , cssLayout :: !ChunkBinaryLayout
  }

data ValidatedChunk = ValidatedChunk
  { vcSection :: !Text
  , vcChunkId :: !Int
  , vcRawBase64 :: !Text
  , vcDecodedLength :: !Integer
  , vcLayout :: !ChunkBinaryLayout
  }

chunkSectionSpecs :: [ChunkSectionSpec]
chunkSectionSpecs =
  [ ChunkSectionSpec "terrain" "chunk_count" (FixedBytesPerTile 113)
  , ChunkSectionSpec "climate" "climate_count" (FixedBytesPerTile 28)
  , ChunkSectionSpec "rivers" "river_count" RiverBytes
  , ChunkSectionSpec "groundwater" "groundwater_count" GroundwaterBytes
  , ChunkSectionSpec "volcanism" "volcanism_count" (FixedBytesPerTile 26)
  , ChunkSectionSpec "glaciers" "glacier_count" (FixedBytesPerTile 24)
  , ChunkSectionSpec "water_bodies" "water_body_count" (FixedBytesPerTile 14)
  , ChunkSectionSpec "vegetation" "vegetation_count" (FixedBytesPerTile 12)
  , ChunkSectionSpec "biomes" "biome_count" (FixedBytesPerTile 2)
  , ChunkSectionSpec "weather" "weather_count" (FixedBytesPerTile 56)
  ]

-- Validate map cardinality, checked dimensions, encoded lengths, and aggregate
-- decoded size before allocating decoded chunk buffers or vectors.
validateTerrainPayload
  :: RPCPayloadLimits
  -> Topo.Types.WorldConfig
  -> KM.KeyMap Value
  -> Either Text ()
validateTerrainPayload limits config payload = do
  ensureTerrainPayloadEncoding payload
  tileCount <- checkedTileCount config
  chunks <- concat <$> traverse (validateChunkSection limits tileCount payload) chunkSectionSpecs
  let actualDecoded = sum (map vcDecodedLength chunks)
      decodedLimit = toInteger (rplMaxDecodedTerrainBytes limits)
  when (actualDecoded > decodedLimit) $ Left
    ("incoming terrain payload decoded aggregate exceeds limit: actual="
      <> tshow actualDecoded <> " bytes, limit=" <> tshow decodedLimit <> " bytes")
  traverse_ (validateVariableChunk tileCount) chunks

checkedTileCount :: Topo.Types.WorldConfig -> Either Text Integer
checkedTileCount config =
  let side = toInteger (Topo.Types.wcChunkSize config)
      tiles = side * side
  in if side <= 0
      then Left "terrain payload chunk_size must be positive"
      else if tiles > toInteger (maxBound :: Int)
        then Left "terrain payload chunk_size squared exceeds platform Int"
        else Right tiles

validateChunkSection
  :: RPCPayloadLimits
  -> Integer
  -> KM.KeyMap Value
  -> ChunkSectionSpec
  -> Either Text [ValidatedChunk]
validateChunkSection limits tileCount payload spec =
  case KM.lookup (Key.fromText (cssField spec)) payload of
    Nothing -> validateSummaryCount spec payload 0 >> Right []
    Just Null -> validateSummaryCount spec payload 0 >> Right []
    Just (Object chunkMap) -> do
      validateSummaryCount spec payload (KM.size chunkMap)
      let decodedLimit = toInteger (rplMaxDecodedTerrainBytes limits)
          minimumChunkBytes = minimumLayoutBytes tileCount (cssLayout spec)
          chunkCount = toInteger (KM.size chunkMap)
      when (chunkCount * minimumChunkBytes > decodedLimit) $ Left
        ("incoming " <> cssField spec <> " chunk map exceeds decoded limit before base64 decode: actual>="
          <> tshow (chunkCount * minimumChunkBytes) <> " bytes, limit="
          <> tshow decodedLimit <> " bytes")
      snd <$> foldM (validateChunkEntry tileCount spec) (IntSet.empty, []) (KM.toList chunkMap)
    Just _ -> Left (cssField spec <> " payload must be an object")

validateSummaryCount :: ChunkSectionSpec -> KM.KeyMap Value -> Int -> Either Text ()
validateSummaryCount spec payload actual =
  case KM.lookup (Key.fromText (cssCountField spec)) payload of
    Nothing -> Right ()
    Just value -> case valueToNonNegativeInt value of
      Nothing -> Left (cssCountField spec <> " must be a non-negative platform Int")
      Just declared -> unless (declared == actual) $ Left
        (cssCountField spec <> " mismatch: declared=" <> tshow declared
          <> ", actual=" <> tshow actual)

validateChunkEntry
  :: Integer
  -> ChunkSectionSpec
  -> (IntSet.IntSet, [ValidatedChunk])
  -> (Key.Key, Value)
  -> Either Text (IntSet.IntSet, [ValidatedChunk])
validateChunkEntry tileCount spec (seen, chunks) (chunkKey, rawValue) = do
  chunkId <- parseChunkId (Key.toText chunkKey)
  when (chunkId < 0) $ Left ("invalid negative chunk id: " <> Key.toText chunkKey)
  when (IntSet.member chunkId seen) $ Left
    (cssField spec <> " payload contains duplicate numeric chunk id " <> tshow chunkId)
  raw <- case rawValue of
    String value -> Right value
    _ -> Left (cssField spec <> " chunk " <> tshow chunkId <> " must be a base64 string")
  decodedLength <- validatedBase64DecodedLength raw
  validateLayoutLength tileCount (cssField spec) chunkId (cssLayout spec) decodedLength
  let chunk = ValidatedChunk
        { vcSection = cssField spec
        , vcChunkId = chunkId
        , vcRawBase64 = raw
        , vcDecodedLength = decodedLength
        , vcLayout = cssLayout spec
        }
  Right (IntSet.insert chunkId seen, chunk : chunks)

minimumLayoutBytes :: Integer -> ChunkBinaryLayout -> Integer
minimumLayoutBytes tileCount layout = case layout of
  FixedBytesPerTile bytesPerTile -> 4 + bytesPerTile * tileCount
  GroundwaterBytes -> 4 + 16 * tileCount
  RiverBytes -> 12 + 38 * tileCount

validateLayoutLength :: Integer -> Text -> Int -> ChunkBinaryLayout -> Integer -> Either Text ()
validateLayoutLength tileCount section chunkId layout actual = case layout of
  FixedBytesPerTile bytesPerTile -> exact (4 + bytesPerTile * tileCount)
  GroundwaterBytes ->
    let legacy = 4 + 16 * tileCount
        extended = 4 + 28 * tileCount
    in unless (actual == legacy || actual == extended) $ Left
        (prefix <> "expected one of [" <> tshow legacy <> ", " <> tshow extended
          <> "] bytes, actual=" <> tshow actual)
  RiverBytes ->
    let minimumBytes = minimumLayoutBytes tileCount RiverBytes
    in when (actual < minimumBytes) $ Left
        (prefix <> "minimum=" <> tshow minimumBytes <> " bytes, actual=" <> tshow actual)
  where
    prefix = section <> " chunk " <> tshow chunkId <> " length mismatch: "
    exact expected = unless (actual == expected) $ Left
      (prefix <> "expected=" <> tshow expected <> " bytes, actual=" <> tshow actual)

validateVariableChunk :: Integer -> ValidatedChunk -> Either Text ()
validateVariableChunk tileCount chunk = case vcLayout chunk of
  RiverBytes -> do
    bytes <- decodeBase64Text (vcRawBase64 chunk)
    let segmentCountOffset = 4 + 34 * tileCount
    segmentCount <- word32LEAt (vcSection chunk) (vcChunkId chunk) segmentCountOffset bytes
    let expected = 12 + 38 * tileCount + 8 * toInteger segmentCount
        actual = vcDecodedLength chunk
    unless (actual == expected) $ Left
      (vcSection chunk <> " chunk " <> tshow (vcChunkId chunk)
        <> " length mismatch: expected=" <> tshow expected
        <> " bytes from segment count, actual=" <> tshow actual)
  _ -> Right ()

word32LEAt :: Text -> Int -> Integer -> BS.ByteString -> Either Text Integer
word32LEAt section chunkId offset bytes
  | offset < 0 || offset + 4 > toInteger (BS.length bytes) = Left
      (section <> " chunk " <> tshow chunkId <> " missing segment count")
  | offset > toInteger (maxBound :: Int) = Left
      (section <> " chunk " <> tshow chunkId <> " segment count offset exceeds platform Int")
  | otherwise =
      let index = fromInteger offset
          byte n = toInteger (BS.index bytes (index + n))
      in Right (byte 0 .|. (byte 1 `shiftL` 8) .|. (byte 2 `shiftL` 16) .|. (byte 3 `shiftL` 24))

validatedBase64DecodedLength :: Text -> Either Text Integer
validatedBase64DecodedLength raw = do
  let encodedLength = Text.length raw
      padding = Text.length (Text.takeWhileEnd (== '=') raw)
      body = Text.dropEnd padding raw
      alphabetMember ch =
        ('A' <= ch && ch <= 'Z') || ('a' <= ch && ch <= 'z')
          || ('0' <= ch && ch <= '9') || ch == '+' || ch == '/'
  when (encodedLength `mod` 4 /= 0) $ Left "invalid base64 length"
  when (padding > 2) $ Left "invalid base64 padding"
  unless (Text.all alphabetMember body) $ Left "invalid base64 character or padding location"
  let decodedLength = toInteger (encodedLength `div` 4 * 3 - padding)
  Right decodedLength

valueToNonNegativeInt :: Value -> Maybe Int
valueToNonNegativeInt (Number n) =
  let asInteger = floor n :: Integer
  in if fromInteger asInteger == n && asInteger >= 0 && asInteger <= fromIntegral (maxBound :: Int)
      then Just (fromInteger asInteger)
      else Nothing
valueToNonNegativeInt _ = Nothing

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

lookupChunkSizeOrEither
  :: Topo.Types.WorldConfig
  -> KM.KeyMap Value
  -> Either Text Topo.Types.WorldConfig
lookupChunkSizeOrEither fallback payload =
  case KM.lookup "chunk_size" payload of
    Nothing -> Right fallback
    Just value -> case valueToPositiveInt value of
      Just chunkSize -> Right Topo.Types.WorldConfig { Topo.Types.wcChunkSize = chunkSize }
      Nothing -> Left "terrain payload chunk_size must be a positive platform Int"

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