{-# LANGUAGE OverloadedStrings #-}

-- | Terrain payload codecs for plugin RPC.
--
-- This module owns the payload contract for terrain/chunk transport,
-- including base64 chunk encoding and decode/apply helpers.
module Topo.Plugin.RPC.Payload
  ( terrainWorldToPayload
  , terrainWorldToPayloadWithLimits
  , terrainWorldToScopedPayload
  , terrainWorldToScopedPayloadWithLimits
  , terrainWorldToScopedPayloadWithBudget
  , terrainWorldToCompletePayload
  , terrainWorldToCompletePayloadWithLimits
  , decodeTerrainWritesValue
  , decodeTerrainWritesValueWithLimits
  , decodeTerrainWritesValueScopedWithLimits
  , terrainWritesValueEmpty
  , applyGeneratorTerrainValue
  , applyGeneratorTerrainValueWithLimits
  , applyGeneratorTerrainValueScopedWithLimits
  , encodeBase64Text
  , decodeBase64Text
  ) where

import Control.Monad (foldM, unless, when)
import Data.Bits ((.|.), shiftL)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import Data.Foldable (traverse_)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Word (Word64)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Read as TextRead

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
import Topo.Plugin.RPC.Scope
  ( ResolvedInvocationScope(..)
  , TerrainSection(..)
  )
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
terrainWorldToPayload = terrainWorldToPayloadWithLimits defaultRPCPayloadLimits

-- | Encode a terrain payload while consuming the configured decoded-binary
-- budget before each chunk is converted to base64.
terrainWorldToPayloadWithLimits :: RPCPayloadLimits -> Topo.World.TerrainWorld -> Either Text Value
terrainWorldToPayloadWithLimits limits =
  terrainWorldToPayloadWithBudget (Just (toInteger (rplMaxDecodedTerrainBytes limits)))

terrainWorldToPayloadWithBudget :: Maybe Integer -> Topo.World.TerrainWorld -> Either Text Value
terrainWorldToPayloadWithBudget budget world = do
  let config = Topo.World.twConfig world
  (terrainObj, budget1) <- encodeChunkMap budget "terrain" (Topo.World.twTerrain world) (encodeTerrainChunk config)
  (climateObj, budget2) <- encodeChunkMap budget1 "climate" (Topo.World.twClimate world) (encodeClimateChunk config)
  (vegetationObj, _) <- encodeChunkMap budget2 "vegetation" (Topo.World.twVegetation world) (encodeVegetationChunk config)
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

-- | Encode exactly the selected terrain sections and chunk IDs. An empty
-- section set produces an empty object; decode-required geometry metadata is
-- included only when at least one section is granted. Counts for unselected
-- sections are omitted entirely.
terrainWorldToScopedPayload
  :: Set TerrainSection
  -> IntSet.IntSet
  -> Topo.World.TerrainWorld
  -> Either Text Value
terrainWorldToScopedPayload =
  terrainWorldToScopedPayloadWithLimits defaultRPCPayloadLimits

-- | Scoped terrain encoding with one aggregate decoded-byte budget.
terrainWorldToScopedPayloadWithLimits
  :: RPCPayloadLimits
  -> Set TerrainSection
  -> IntSet.IntSet
  -> Topo.World.TerrainWorld
  -> Either Text Value
terrainWorldToScopedPayloadWithLimits limits =
  terrainWorldToScopedPayloadWithBudget (fromIntegral (rplMaxDecodedTerrainBytes limits))

-- | Scoped terrain encoding with an exact resolved decoded-byte ceiling.
terrainWorldToScopedPayloadWithBudget
  :: Word64
  -> Set TerrainSection
  -> IntSet.IntSet
  -> Topo.World.TerrainWorld
  -> Either Text Value
terrainWorldToScopedPayloadWithBudget decodedByteLimit sections chunkIds world
  | Set.null sections = Right (object [])
  | otherwise = do
      let config = Topo.World.twConfig world
          initialBudget = Just (toInteger decodedByteLimit)
          selected chunks = IntMap.restrictKeys chunks chunkIds
      (terrainFields, budget1) <-
        if Set.member TerrainElevation sections
          then do
            let chunks = selected (Topo.World.twTerrain world)
            (encoded, remaining) <- encodeChunkMap initialBudget "terrain" chunks (encodeTerrainChunk config)
            Right (["chunk_count" .= IntMap.size chunks, "terrain" .= Object encoded], remaining)
          else Right ([], initialBudget)
      (climateFields, budget2) <-
        if Set.member TerrainClimate sections
          then do
            let chunks = selected (Topo.World.twClimate world)
            (encoded, remaining) <- encodeChunkMap budget1 "climate" chunks (encodeClimateChunk config)
            Right (["climate_count" .= IntMap.size chunks, "climate" .= Object encoded], remaining)
          else Right ([], budget1)
      vegetationFields <-
        if Set.member TerrainVegetation sections
          then do
            let chunks = selected (Topo.World.twVegetation world)
            (encoded, _) <- encodeChunkMap budget2 "vegetation" chunks (encodeVegetationChunk config)
            Right ["vegetation_count" .= IntMap.size chunks, "vegetation" .= Object encoded]
          else Right []
      Right $ object $
        [ "chunk_size" .= Topo.Types.wcChunkSize config
        , "hex_grid" .= Topo.World.twHexGrid world
        , "planet" .= Topo.World.twPlanet world
        , "slice" .= Topo.World.twSlice world
        , "encoding" .= ("base64" :: Text)
        ] <> terrainFields <> climateFields <> vegetationFields

-- | Encode the complete Writ/full-world export payload.
--
-- This is not used for capability-scoped plugin terrain reads; callers that
-- need every generated/persisted layer must opt in explicitly.
terrainWorldToCompletePayload :: Topo.World.TerrainWorld -> Either Text Value
terrainWorldToCompletePayload = terrainWorldToCompletePayloadWithLimits defaultRPCPayloadLimits

-- | Encode the complete payload with one aggregate decoded-binary budget shared
-- by every chunk section.
terrainWorldToCompletePayloadWithLimits :: RPCPayloadLimits -> Topo.World.TerrainWorld -> Either Text Value
terrainWorldToCompletePayloadWithLimits limits =
  terrainWorldToCompletePayloadWithBudget (Just (toInteger (rplMaxDecodedTerrainBytes limits)))

terrainWorldToCompletePayloadWithBudget :: Maybe Integer -> Topo.World.TerrainWorld -> Either Text Value
terrainWorldToCompletePayloadWithBudget budget world = do
  let config = Topo.World.twConfig world
      weather = getWeatherFromOverlay world
  (terrainObj, budget1) <- encodeChunkMap budget "terrain" (Topo.World.twTerrain world) (encodeTerrainChunk config)
  (climateObj, budget2) <- encodeChunkMap budget1 "climate" (Topo.World.twClimate world) (encodeClimateChunk config)
  (riverObj, budget3) <- encodeChunkMap budget2 "rivers" (Topo.World.twRivers world) (encodeRiverChunk config)
  (groundwaterObj, budget4) <- encodeChunkMap budget3 "groundwater" (Topo.World.twGroundwater world) (encodeGroundwaterChunk config)
  (volcanismObj, budget5) <- encodeChunkMap budget4 "volcanism" (Topo.World.twVolcanism world) (encodeVolcanismChunk config)
  (glacierObj, budget6) <- encodeChunkMap budget5 "glaciers" (Topo.World.twGlaciers world) (encodeGlacierChunk config)
  (waterBodyObj, budget7) <- encodeChunkMap budget6 "water_bodies" (Topo.World.twWaterBodies world) (encodeWaterBodyChunk config)
  (vegetationObj, budget8) <- encodeChunkMap budget7 "vegetation" (Topo.World.twVegetation world) (encodeVegetationChunk config)
  (biomeObj, budget9) <- encodeChunkMap budget8 "biomes" (Topo.World.twTerrain world) (encodeBiomeChunk config)
  (weatherObj, _) <- encodeChunkMap budget9 "weather" weather (encodeWeatherChunk config)
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

-- | Validate and decode terrain writes against one immutable output scope.
-- Authorization is checked before any chunk bytes are decoded.
decodeTerrainWritesValueScopedWithLimits
  :: RPCPayloadLimits
  -> ResolvedInvocationScope
  -> TerrainWorld
  -> Maybe Value
  -> Either Text TerrainWrites
decodeTerrainWritesValueScopedWithLimits limits scope world maybePayload =
  case maybePayload of
    Nothing -> Right emptyTerrainWrites
    Just Null -> Right emptyTerrainWrites
    Just (Object payload)
      | KM.null payload -> Right emptyTerrainWrites
      | otherwise -> do
          validateScopedTerrainOutput limits scope world payload
          terrainWritesFromPayloadWithConfig limits (twConfig world) payload
    Just _ -> Left "terrain_writes payload must be an object"

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

-- | Apply generator writes only after the complete payload has passed exact
-- section, key, chunk, and geometry authorization.
applyGeneratorTerrainValueScopedWithLimits
  :: RPCPayloadLimits
  -> ResolvedInvocationScope
  -> TerrainWorld
  -> Value
  -> Either Text TerrainWorld
applyGeneratorTerrainValueScopedWithLimits _ _ world Null = Right world
applyGeneratorTerrainValueScopedWithLimits _ _ world (Object payload)
  | KM.null payload = Right world
applyGeneratorTerrainValueScopedWithLimits limits scope world (Object payload) = do
  validateScopedTerrainOutput limits scope world payload
  writes <- terrainWritesFromPayloadWithConfig limits (twConfig world) payload
  Right (applyTerrainWrites writes world)
applyGeneratorTerrainValueScopedWithLimits _ _ _ _ =
  Left "generator terrain payload must be an object or null"

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

validateScopedTerrainOutput
  :: RPCPayloadLimits
  -> ResolvedInvocationScope
  -> TerrainWorld
  -> KM.KeyMap Value
  -> Either Text ()
validateScopedTerrainOutput limits scope world payload = do
  let sections = risTerrainOutputSections scope
      allowedChunks = risTerrainOutputChunkIds scope
      sectionSpecs = concatMap scopedSectionFields (Set.toAscList sections)
      allowedKeys = Set.fromList (["encoding", "chunk_size"] <> concatMap (\(field, countField) -> [field, countField]) sectionSpecs)
      unsupportedKeys = filter (`Set.notMember` allowedKeys) (map Key.toText (KM.keys payload))
  unless (null unsupportedKeys) $ Left
    ("terrain output contains unauthorized keys: " <> Text.intercalate ", " unsupportedKeys)
  when (Set.null sections) $ Left "terrain output is not authorized by the resolved invocation scope"
  payloadConfig <- lookupChunkSizeOrEither (twConfig world) payload
  unless (payloadConfig == twConfig world) $ Left
    "terrain output chunk_size does not match the immutable host world geometry"
  traverse_ (validateScopedSectionChunks payload allowedChunks . fst) sectionSpecs
  validateTerrainPayload limits payloadConfig payload

scopedSectionFields :: TerrainSection -> [(Text, Text)]
scopedSectionFields TerrainElevation = [("terrain", "chunk_count")]
scopedSectionFields TerrainClimate = [("climate", "climate_count")]
scopedSectionFields TerrainVegetation = [("vegetation", "vegetation_count")]

validateScopedSectionChunks
  :: KM.KeyMap Value
  -> IntSet.IntSet
  -> Text
  -> Either Text ()
validateScopedSectionChunks payload allowedChunks fieldName =
  case KM.lookup (Key.fromText fieldName) payload of
    Nothing -> Right ()
    Just Null -> Right ()
    Just (Object chunks) -> do
      supplied <- foldKeyMapM collect IntSet.empty chunks
      let unauthorized = supplied `IntSet.difference` allowedChunks
      unless (IntSet.null unauthorized) $ Left
        (fieldName <> " output contains unauthorized chunk IDs: "
          <> Text.intercalate ", " (map tshow (IntSet.toAscList unauthorized)))
    Just _ -> Left (fieldName <> " payload must be an object")
  where
    collect seen key _ = do
      chunkId <- parseChunkId (Key.toText key)
      when (chunkId < 0) $ Left ("invalid negative chunk id: " <> Key.toText key)
      when (IntSet.member chunkId seen) $ Left
        (fieldName <> " payload contains duplicate numeric chunk id " <> tshow chunkId)
      Right (IntSet.insert chunkId seen)

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
  :: Maybe Integer
  -> Text
  -> IntMap.IntMap a
  -> (a -> Either ExportError BS.ByteString)
  -> Either Text (KM.KeyMap Value, Maybe Integer)
encodeChunkMap initialBudget section chunks encodeChunk =
  IntMap.foldlWithKey' encodeOne (Right (KM.empty, initialBudget)) chunks
  where
    encodeOne (Left err) _ _ = Left err
    encodeOne (Right (encodedChunks, budget)) chunkId chunk = do
      binary <- firstExportError (encodeChunk chunk)
      budget' <- consumeEncodedBudget section chunkId (toInteger (BS.length binary)) budget
      let key = Key.fromText (Text.pack (show chunkId))
          encodedText = encodeBase64Text binary
          encodedChunks' = KM.insert key (Aeson.String encodedText) encodedChunks
      Text.length encodedText `seq` encodedChunks' `seq` budget' `seq`
        Right (encodedChunks', budget')

consumeEncodedBudget :: Text -> Int -> Integer -> Maybe Integer -> Either Text (Maybe Integer)
consumeEncodedBudget _ _ _ Nothing = Right Nothing
consumeEncodedBudget section chunkId chunkBytes (Just remaining)
  | chunkBytes <= remaining = Right (Just (remaining - chunkBytes))
  | otherwise = Left
      ("outgoing terrain payload decoded aggregate exceeds limit while encoding "
        <> section <> " chunk " <> tshow chunkId
        <> ": chunk=" <> tshow chunkBytes <> " bytes, remaining="
        <> tshow remaining <> " bytes")

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
  let initialBudget = toInteger (rplMaxDecodedTerrainBytes limits)
  (terrain, budget1) <- decodeChunkSection initialBudget payload "terrain" decodeTerrainChunk payloadConfig
  (climate, budget2) <- decodeChunkSection budget1 payload "climate" decodeClimateChunk payloadConfig
  (vegetation, _) <- decodeChunkSection budget2 payload "vegetation" decodeVegetationChunk payloadConfig
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
  let decodedLimit = toInteger (rplMaxDecodedTerrainBytes limits)
  actualDecoded <- foldM (validateChunkSection limits tileCount payload) 0 chunkSectionSpecs
  when (actualDecoded > decodedLimit) $ Left
    ("incoming terrain payload decoded aggregate exceeds limit: actual="
      <> tshow actualDecoded <> " bytes, limit=" <> tshow decodedLimit <> " bytes")
  validateRiverChunks tileCount payload

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
  -> Integer
  -> ChunkSectionSpec
  -> Either Text Integer
validateChunkSection limits tileCount payload decodedSoFar spec =
  case KM.lookup (Key.fromText (cssField spec)) payload of
    Nothing -> validateSummaryCount spec payload 0 >> Right decodedSoFar
    Just Null -> validateSummaryCount spec payload 0 >> Right decodedSoFar
    Just (Object chunkMap) -> do
      validateSummaryCount spec payload (KM.size chunkMap)
      let decodedLimit = toInteger (rplMaxDecodedTerrainBytes limits)
          minimumChunkBytes = minimumLayoutBytes tileCount (cssLayout spec)
          chunkCount = toInteger (KM.size chunkMap)
      when (decodedSoFar + chunkCount * minimumChunkBytes > decodedLimit) $ Left
        ("incoming " <> cssField spec <> " chunk map exceeds decoded limit before base64 decode: actual>="
          <> tshow (decodedSoFar + chunkCount * minimumChunkBytes) <> " bytes, limit="
          <> tshow decodedLimit <> " bytes")
      (_, decodedTotal) <- foldKeyMapM
        (validateChunkEntry tileCount spec)
        (IntSet.empty, decodedSoFar)
        chunkMap
      Right decodedTotal
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
  -> (IntSet.IntSet, Integer)
  -> Key.Key
  -> Value
  -> Either Text (IntSet.IntSet, Integer)
validateChunkEntry tileCount spec (seen, decodedSoFar) chunkKey rawValue = do
  chunkId <- parseChunkId (Key.toText chunkKey)
  when (chunkId < 0) $ Left ("invalid negative chunk id: " <> Key.toText chunkKey)
  when (IntSet.member chunkId seen) $ Left
    (cssField spec <> " payload contains duplicate numeric chunk id " <> tshow chunkId)
  raw <- case rawValue of
    String value -> Right value
    _ -> Left (cssField spec <> " chunk " <> tshow chunkId <> " must be a base64 string")
  decodedLength <- validatedBase64DecodedLength raw
  validateLayoutLength tileCount (cssField spec) chunkId (cssLayout spec) decodedLength
  let decodedTotal = decodedSoFar + decodedLength
      seen' = IntSet.insert chunkId seen
  seen' `seq` decodedTotal `seq` Right (seen', decodedTotal)

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

validateRiverChunks :: Integer -> KM.KeyMap Value -> Either Text ()
validateRiverChunks tileCount payload =
  case KM.lookup "rivers" payload of
    Nothing -> Right ()
    Just Null -> Right ()
    Just (Object chunkMap) ->
      foldKeyMapM validateOne () chunkMap
    Just _ -> Left "rivers payload must be an object"
  where
    validateOne () chunkKey rawValue = do
      chunkId <- parseChunkId (Key.toText chunkKey)
      raw <- case rawValue of
        String value -> Right value
        _ -> Left ("rivers chunk " <> tshow chunkId <> " must be a base64 string")
      actual <- validatedBase64DecodedLength raw
      bytes <- decodeBase64Text raw
      let segmentCountOffset = 4 + 34 * tileCount
      segmentCount <- word32LEAt "rivers" chunkId segmentCountOffset bytes
      let expected = 12 + 38 * tileCount + 8 * toInteger segmentCount
      unless (actual == expected) $ Left
        ("rivers chunk " <> tshow chunkId
          <> " length mismatch: expected=" <> tshow expected
          <> " bytes from segment count, actual=" <> tshow actual)

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
  when (encodedLength `mod` 4 /= 0) $ Left "invalid base64 length"
  when (padding > 2) $ Left "invalid base64 padding"
  unless (Text.all isBase64AlphabetChar body) $
    Left "invalid base64 character or padding location"
  let decodedLength = toInteger (encodedLength `div` 4 * 3 - padding)
  Right decodedLength

isBase64AlphabetChar :: Char -> Bool
isBase64AlphabetChar ch =
     ('A' <= ch && ch <= 'Z')
  || ('a' <= ch && ch <= 'z')
  || ('0' <= ch && ch <= '9')
  || ch == '+'
  || ch == '/'

valueToNonNegativeInt :: Value -> Maybe Int
valueToNonNegativeInt (Number n) =
  let asInteger = floor n :: Integer
  in if fromInteger asInteger == n && asInteger >= 0 && asInteger <= fromIntegral (maxBound :: Int)
      then Just (fromInteger asInteger)
      else Nothing
valueToNonNegativeInt _ = Nothing

decodeChunkSection
  :: Integer
  -> KM.KeyMap Value
  -> Text
  -> (Topo.Types.WorldConfig -> BS.ByteString -> Either ExportError a)
  -> Topo.Types.WorldConfig
  -> Either Text (IntMap.IntMap a, Integer)
decodeChunkSection initialBudget payload fieldName decodeChunk config =
  case KM.lookup (Key.fromText fieldName) payload of
    Nothing -> Right (IntMap.empty, initialBudget)
    Just Null -> Right (IntMap.empty, initialBudget)
    Just (Object chunkMap) ->
      foldKeyMapM decodeOne (IntMap.empty, initialBudget) chunkMap
    Just _ -> Left (fieldName <> " payload must be an object")
  where
    decodeOne (decodedChunks, remaining) chunkKey rawChunkBytes = do
      chunkId <- parseChunkId (Key.toText chunkKey)
      raw <- case rawChunkBytes of
        Aeson.String value -> Right value
        _ -> Left "chunk payload must be a base64 string"
      decodedLength <- validatedBase64DecodedLength raw
      remaining' <- consumeDecodeBudget fieldName chunkId decodedLength remaining
      bytes <- decodeBase64Text raw
      decoded <- firstExportError (decodeChunk config bytes)
      let decodedChunks' = IntMap.insert chunkId decoded decodedChunks
      decodedChunks' `seq` remaining' `seq` Right (decodedChunks', remaining')

consumeDecodeBudget :: Text -> Int -> Integer -> Integer -> Either Text Integer
consumeDecodeBudget section chunkId chunkBytes remaining
  | chunkBytes <= remaining = Right (remaining - chunkBytes)
  | otherwise = Left
      ("incoming terrain payload decoded aggregate exceeds limit while decoding "
        <> section <> " chunk " <> tshow chunkId
        <> ": chunk=" <> tshow chunkBytes <> " bytes, remaining="
        <> tshow remaining <> " bytes")

foldKeyMapM
  :: (state -> Key.Key -> Value -> Either Text state)
  -> state
  -> KM.KeyMap Value
  -> Either Text state
foldKeyMapM step initial keyMap =
  KM.foldrWithKey
    (\key value continue state -> do
      state' <- step state key value
      state' `seq` continue state')
    Right
    keyMap
    initial

parseChunkId :: Text -> Either Text Int
parseChunkId rawChunkId =
  case TextRead.signed TextRead.decimal rawChunkId :: Either String (Integer, Text) of
    Right (chunkId, rest)
      | Text.null rest
      , chunkId >= toInteger (minBound :: Int)
      , chunkId <= toInteger (maxBound :: Int) -> Right (fromInteger chunkId)
    _ -> Left ("invalid chunk id: " <> rawChunkId)

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

encodeBase64Text :: BS.ByteString -> Text
encodeBase64Text = TextEncoding.decodeUtf8 . Base64.encode

decodeBase64Text :: Text -> Either Text BS.ByteString
decodeBase64Text raw = do
  _ <- validatedBase64DecodedLength raw
  let encoded = TextEncoding.encodeUtf8 raw
  decoded <- case Base64.decode encoded of
    Left err -> Left ("invalid base64: " <> Text.pack err)
    Right bytes -> Right bytes
  if Base64.encode decoded == encoded
    then Right decoded
    else Left "invalid base64 canonical padding"