{-# LANGUAGE OverloadedStrings #-}

-- | IPC handlers for terrain queries and data export:
-- @find_hexes@, @export_terrain_data@.
module Seer.Command.Handlers.Query
  ( handleFindHexes
  , handleExportTerrainData
  , handleExportMeshData
  , handleExportSampleData
  ) where

import Data.Aeson (Value(..), object, (.=), (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.Types as Aeson
import qualified Data.IntMap.Strict as IntMap
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word16)
import qualified Data.Vector.Unboxed as U

import Actor.Data (TerrainSnapshot(..))
import Actor.UI.State (allViewModeExportFields, readUiSnapshotRef)
import Actor.SnapshotReceiver (readTerrainSnapshot)
import Actor.UiActions.Handles (ActorHandles(..))
import Seer.Command.Context (CommandContext(..))
import Seer.World.Persist (snapshotToWorld)
import Topo.Biome.Name (biomeDisplayName)
import Topo.Command.Types (SeerResponse, okResponse, errResponse)
import Topo.Mesh (Mesh(..), meshPatch)
import Topo.Sample (TerrainSampleReal(..), convertSample, sampleTerrain)
import Topo.World (TerrainWorld(..))
import Topo.Types
  ( TerrainChunk(..)
  , ClimateChunk(..)
  , WeatherChunk(..)
  , VegetationChunk(..)
  , RiverChunk(..)
  , BiomeId
  , TerrainForm
  , PlateBoundary
  , DirectionalSlope(..)
  , Region(..)
  , TerrainSample(..)
  , TileCoord(..)
  , Vec3(..)
  , WorldConfig(..)
  , WorldPos(..)
  , biomeIdToCode
  , biomeIdFromCode
  , plateBoundaryToCode
  , terrainFormToCode
  , terrainFormFromCode
  , terrainFormDisplayName
  , waterBodyToCode
  )

-- | Handle @find_hexes@ — search tiles matching filter predicates.
--
-- Params:
-- @{ "filters": [{ "field": "elevation", "op": "gt", "value": 0.5 }],
--    "limit"?: int }@
--
-- All filters are ANDed.  Default limit: 50.
handleFindHexes :: CommandContext -> Int -> Value -> IO SeerResponse
handleFindHexes ctx reqId params = do
  case Aeson.parseMaybe parseFindHexes params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'filters' parameter"
    Just (filters, limit) -> do
      snap <- readTerrainSnapshot (ahTerrainSnapshotRef (ccActorHandles ctx))
      let chunkSize = tsChunkSize snap
      if chunkSize <= 0
        then pure $ okResponse reqId $ object
          [ "matches" .= ([] :: [Value])
          , "count"   .= (0 :: Int)
          ]
        else do
          let tileCount = chunkSize * chunkSize
              results = findMatches snap tileCount filters limit
          pure $ okResponse reqId $ object
            [ "matches" .= results
            , "count"   .= length results
            ]

-- | Handle @export_terrain_data@ — export terrain field data as JSON arrays.
--
-- Params:
-- @{ "chunks"?: [int], "fields"?: ["elevation", "moisture", ...] }@
--
-- Default: all chunks, elevation only.
handleExportTerrainData :: CommandContext -> Int -> Value -> IO SeerResponse
handleExportTerrainData ctx reqId params = do
  snap <- readTerrainSnapshot (ahTerrainSnapshotRef (ccActorHandles ctx))
  let chunkSize = tsChunkSize snap
  if chunkSize <= 0
    then pure $ errResponse reqId "no terrain generated"
    else do
      let opts = maybe defaultExportOpts id (Aeson.parseMaybe parseExportOpts params)
          chunkIds = case eoChunks opts of
            Just ids -> ids
            Nothing  -> IntMap.keys (tsTerrainChunks snap)
          fields = case eoFields opts of
            Just fs -> fs
            Nothing -> ["elevation"]
          chunkEntries = map (exportChunk snap fields) chunkIds
      pure $ okResponse reqId $ object
        [ "chunk_count" .= length chunkEntries
        , "fields"      .= fields
        , "available_fields" .= allViewModeExportFields
        , "data"        .= object
            [ Key.fromText (Text.pack (show cid)) .= val | (cid, val) <- chunkEntries ]
        , "diagnostics" .= [diagnostic "info" "terrain_export_ready" "terrain export JSON payload built"]
        ]

-- | Handle @export_mesh_data@ — export a rectangular terrain mesh patch.
handleExportMeshData :: CommandContext -> Int -> Value -> IO SeerResponse
handleExportMeshData ctx reqId params = do
  worldResult <- currentWorld ctx
  case worldResult of
    Left err -> pure $ errResponse reqId err
    Right world -> do
      let defaultMax = max 0 (wcChunkSize (twConfig world) - 1)
          params' = case params of
            Null -> object []
            _ -> params
      case Aeson.parseMaybe (parseRegionParams defaultMax) params' of
        Nothing -> pure $ errResponse reqId "missing or invalid mesh export region (expected x0,y0,x1,y1 integers)"
        Just region
          | not (validRegion region) ->
              pure $ errResponse reqId "mesh export region must have non-negative, non-overflowing extents"
          | regionCellCount region > maxMeshExportCells ->
              pure $ errResponse reqId ("mesh export region too large; maximum cells: " <> Text.pack (show maxMeshExportCells))
          | otherwise -> do
              let mesh = meshPatch world region
              pure $ okResponse reqId $ object
                [ "format" .= ("topo-mesh-json" :: Text)
                , "region" .= regionJSON region
                , "vertex_count" .= length (meshVertices mesh)
                , "index_count" .= length (meshIndices mesh)
                , "vertices" .= map vec3JSON (meshVertices mesh)
                , "indices" .= meshIndices mesh
                , "diagnostics" .= [diagnostic "info" "mesh_export_ready" "mesh patch export payload built"]
                ]

-- | Handle @export_sample_data@ — export one sampled terrain point.
handleExportSampleData :: CommandContext -> Int -> Value -> IO SeerResponse
handleExportSampleData ctx reqId params = do
  worldResult <- currentWorld ctx
  case worldResult of
    Left err -> pure $ errResponse reqId err
    Right world -> do
      case Aeson.parseMaybe parseSampleParams params of
        Nothing -> pure $ errResponse reqId "missing or invalid sample export parameters (expected x and y numbers)"
        Just (x, y, realUnits) -> do
          let sample = sampleTerrain world (WorldPos x y)
              sampleBody
                | realUnits = sampleRealJSON (convertSample (twUnitScales world) sample)
                | otherwise = sampleJSON sample
          pure $ okResponse reqId $ object
            [ "format" .= ("topo-sample-json" :: Text)
            , "position" .= object ["x" .= x, "y" .= y]
            , "real_units" .= realUnits
            , "sample" .= sampleBody
            , "diagnostics" .= [diagnostic "info" "sample_export_ready" "terrain sample export payload built"]
            ]

currentWorld :: CommandContext -> IO (Either Text TerrainWorld)
currentWorld ctx = do
  snap <- readTerrainSnapshot (ahTerrainSnapshotRef (ccActorHandles ctx))
  if tsChunkSize snap <= 0 || IntMap.null (tsTerrainChunks snap)
    then pure (Left "no terrain generated")
    else do
      ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
      pure (Right (snapshotToWorld ui snap))

maxMeshExportCells :: Integer
maxMeshExportCells = 262144

validRegion :: Region -> Bool
validRegion (RegionRect (TileCoord x0 y0) (TileCoord x1 y1)) =
  x1 >= x0 && y1 >= y0 && x1 < (maxBound :: Int) && y1 < (maxBound :: Int)

regionCellCount :: Region -> Integer
regionCellCount (RegionRect (TileCoord x0 y0) (TileCoord x1 y1)) =
  let width = toInteger x1 - toInteger x0 + 1
      height = toInteger y1 - toInteger y0 + 1
  in width * height

regionJSON :: Region -> Value
regionJSON (RegionRect (TileCoord x0 y0) (TileCoord x1 y1)) = object
  [ "x0" .= x0
  , "y0" .= y0
  , "x1" .= x1
  , "y1" .= y1
  ]

vec3JSON :: Vec3 -> Value
vec3JSON (Vec3 x y z) = object
  [ "x" .= x
  , "y" .= y
  , "z" .= z
  ]

sampleJSON :: TerrainSample -> Value
sampleJSON sample = object
  [ "elevation" .= tsElevation sample
  , "slope" .= slopeJSON (tsDirSlope sample)
  , "curvature" .= tsCurvature sample
  , "hardness" .= tsHardness sample
  , "soil_depth" .= tsSoilDepth sample
  , "moisture" .= tsMoisture sample
  , "fertility" .= tsFertility sample
  , "roughness" .= tsRoughness sample
  , "rock_density" .= tsRockDensity sample
  , "soil_grain" .= tsSoilGrain sample
  , "temperature" .= tsTemperature sample
  , "humidity" .= tsHumidity sample
  , "wind_speed" .= tsWindSpeed sample
  , "pressure" .= tsPressure sample
  , "precipitation" .= tsPrecip sample
  , "biome" .= biomeDisplayName (tsBiomeId sample)
  , "biome_code" .= biomeIdToCode (tsBiomeId sample)
  , "vegetation_cover" .= tsVegCover sample
  , "vegetation_density" .= tsVegDensity sample
  , "relief" .= tsRelief sample
  , "ruggedness" .= tsRuggedness sample
  , "terrain_form" .= Text.pack (terrainFormDisplayName (tsTerrainForm sample))
  , "terrain_form_code" .= terrainFormToCode (tsTerrainForm sample)
  , "water_body_code" .= waterBodyToCode (tsWaterBodyType sample)
  , "river_discharge" .= tsDischarge sample
  , "snowpack" .= tsSnowpack sample
  , "ice_thickness" .= tsIceThickness sample
  ]

sampleRealJSON :: TerrainSampleReal -> Value
sampleRealJSON sample = object
  [ "elevation_m" .= tsrElevation sample
  , "slope_deg" .= tsrSlope sample
  , "curvature" .= tsrCurvature sample
  , "hardness" .= tsrHardness sample
  , "soil_depth_m" .= tsrSoilDepth sample
  , "moisture_pct" .= tsrMoisture sample
  , "fertility" .= tsrFertility sample
  , "roughness" .= tsrRoughness sample
  , "rock_density" .= tsrRockDensity sample
  , "soil_grain" .= tsrSoilGrain sample
  , "temperature_c" .= tsrTemperature sample
  , "humidity_pct" .= tsrHumidity sample
  , "wind_speed_ms" .= tsrWindSpeed sample
  , "pressure_hpa" .= tsrPressure sample
  , "precipitation_mm_year" .= tsrPrecip sample
  , "biome" .= biomeDisplayName (tsrBiomeId sample)
  , "biome_code" .= biomeIdToCode (tsrBiomeId sample)
  , "vegetation_cover" .= tsrVegCover sample
  , "vegetation_density" .= tsrVegDensity sample
  , "relief_m" .= tsrRelief sample
  , "ruggedness" .= tsrRuggedness sample
  , "terrain_form" .= Text.pack (terrainFormDisplayName (tsrTerrainForm sample))
  , "terrain_form_code" .= terrainFormToCode (tsrTerrainForm sample)
  , "water_body_code" .= waterBodyToCode (tsrWaterBodyType sample)
  , "river_discharge" .= tsrDischarge sample
  , "snowpack" .= tsrSnowpack sample
  , "ice_thickness" .= tsrIceThickness sample
  ]

slopeJSON :: DirectionalSlope -> Value
slopeJSON (DirectionalSlope e ne nw w sw se) = object
  [ "e" .= e
  , "ne" .= ne
  , "nw" .= nw
  , "w" .= w
  , "sw" .= sw
  , "se" .= se
  ]

diagnostic :: Text -> Text -> Text -> Value
diagnostic level code message = object
  [ "level" .= level
  , "code" .= code
  , "message" .= message
  ]

-- =====================================================================
-- find_hexes implementation
-- =====================================================================

data HexFilter = HexFilter
  { hfField :: !Text
  , hfOp    :: !FilterOp
  , hfValue :: !Value
  }

data FilterOp = OpEq | OpNeq | OpGt | OpGte | OpLt | OpLte

findMatches :: TerrainSnapshot -> Int -> [HexFilter] -> Int -> [Value]
findMatches snap tileCount filters limit =
  take limit $ concatMap (matchChunk snap tileCount filters) chunkPairs
  where
    chunkPairs = IntMap.toList (tsTerrainChunks snap)

matchChunk :: TerrainSnapshot -> Int -> [HexFilter] -> (Int, TerrainChunk) -> [Value]
matchChunk snap tileCount filters (cid, tc) =
  [ buildMatchResult cid idx tc snap
  | idx <- [0 .. tileCount - 1]
  , all (\f -> evalFilter f cid idx tc snap) filters
  ]

evalFilter :: HexFilter -> Int -> Int -> TerrainChunk -> TerrainSnapshot -> Bool
evalFilter hf _cid idx tc snap = case hfField hf of
  "elevation"    -> compareFloat (hfOp hf) (hfValue hf) (safeIdx (tcElevation tc) idx)
  "moisture"     -> compareFloat (hfOp hf) (hfValue hf) (safeIdx (tcMoisture tc) idx)
  "hardness"     -> compareFloat (hfOp hf) (hfValue hf) (safeIdx (tcHardness tc) idx)
  "fertility"    -> compareFloat (hfOp hf) (hfValue hf) (safeIdx (tcFertility tc) idx)
  "roughness"    -> compareFloat (hfOp hf) (hfValue hf) (safeIdx (tcRoughness tc) idx)
  "biome"        -> compareText  (hfOp hf) (hfValue hf) (biomeAtIdx tc idx)
  "terrain_form" -> compareText  (hfOp hf) (hfValue hf) (formAtIdx tc idx)
  "temperature"  -> compareFloat (hfOp hf) (hfValue hf) (climateField _cid ccTempAvg snap idx)
  "precipitation" -> compareFloat (hfOp hf) (hfValue hf) (climateField _cid ccPrecipAvg snap idx)
  "vegetation_cover" -> compareFloat (hfOp hf) (hfValue hf) (vegField _cid vegCover snap idx)
  "river_discharge"  -> compareFloat (hfOp hf) (hfValue hf) (riverField _cid rcDischarge snap idx)
  "plate_id"     -> compareW16   (hfOp hf) (hfValue hf) (safeIdxW16 (tcPlateId tc) idx)
  _              -> False

climateField :: Int -> (ClimateChunk -> U.Vector Float) -> TerrainSnapshot -> Int -> Maybe Float
climateField cid accessor snap idx =
  case IntMap.lookup cid (tsClimateChunks snap) of
    Nothing -> Nothing
    Just cc -> safeIdx (accessor cc) idx

vegField :: Int -> (VegetationChunk -> U.Vector Float) -> TerrainSnapshot -> Int -> Maybe Float
vegField cid accessor snap idx =
  case IntMap.lookup cid (tsVegetationChunks snap) of
    Nothing -> Nothing
    Just vc -> safeIdx (accessor vc) idx

riverField :: Int -> (RiverChunk -> U.Vector Float) -> TerrainSnapshot -> Int -> Maybe Float
riverField cid accessor snap idx =
  case IntMap.lookup cid (tsRiverChunks snap) of
    Nothing -> Nothing
    Just rc -> safeIdx (accessor rc) idx

compareFloat :: FilterOp -> Value -> Maybe Float -> Bool
compareFloat _ _ Nothing = False
compareFloat op (Number n) (Just v) =
  let target = realToFrac n :: Float
  in case op of
    OpEq  -> v == target
    OpNeq -> v /= target
    OpGt  -> v > target
    OpGte -> v >= target
    OpLt  -> v < target
    OpLte -> v <= target
compareFloat _ _ _ = False

compareText :: FilterOp -> Value -> Maybe Text -> Bool
compareText _ _ Nothing = False
compareText op (String target) (Just v) = case op of
  OpEq  -> v == target
  OpNeq -> v /= target
  _     -> False  -- ordering on text doesn't make sense here
compareText _ _ _ = False

compareW16 :: FilterOp -> Value -> Maybe Word16 -> Bool
compareW16 _ _ Nothing = False
compareW16 op (Number n) (Just v) =
  let target = round (realToFrac n :: Double) :: Word16
  in case op of
    OpEq  -> v == target
    OpNeq -> v /= target
    OpGt  -> v > target
    OpGte -> v >= target
    OpLt  -> v < target
    OpLte -> v <= target
compareW16 _ _ _ = False

biomeAtIdx :: TerrainChunk -> Int -> Maybe Text
biomeAtIdx tc idx
  | idx >= 0 && idx < U.length (tcFlags tc) =
      Just (biomeDisplayName (tcFlags tc U.! idx))
  | otherwise = Nothing

formAtIdx :: TerrainChunk -> Int -> Maybe Text
formAtIdx tc idx
  | idx >= 0 && idx < U.length (tcTerrainForm tc) =
      Just (Text.pack (terrainFormDisplayName (tcTerrainForm tc U.! idx)))
  | otherwise = Nothing

plateBoundaryDisplayName :: PlateBoundary -> Text
plateBoundaryDisplayName boundary =
  case plateBoundaryToCode boundary of
    0 -> "None"
    1 -> "Convergent"
    2 -> "Divergent"
    3 -> "Transform"
    code -> "Unknown (" <> Text.pack (show code) <> ")"

crustDisplayName :: Word16 -> Text
crustDisplayName 0 = "Oceanic"
crustDisplayName 1 = "Continental"
crustDisplayName code = "Unknown (" <> Text.pack (show code) <> ")"

velocityMagnitude :: Float -> Float -> Float
velocityMagnitude vx vy = sqrt (vx * vx + vy * vy)

safeIdx :: U.Vector Float -> Int -> Maybe Float
safeIdx v i
  | i >= 0 && i < U.length v = Just (v U.! i)
  | otherwise = Nothing

safeIdxW16 :: U.Vector Word16 -> Int -> Maybe Word16
safeIdxW16 v i
  | i >= 0 && i < U.length v = Just (v U.! i)
  | otherwise = Nothing

buildMatchResult :: Int -> Int -> TerrainChunk -> TerrainSnapshot -> Value
buildMatchResult cid idx tc _snap = object
  [ "chunk"     .= cid
  , "tile"      .= idx
  , "elevation" .= safeIdx (tcElevation tc) idx
  , "biome"     .= biomeAtIdx tc idx
  , "terrain_form" .= formAtIdx tc idx
  ]

-- =====================================================================
-- export_terrain_data implementation
-- =====================================================================

data ExportOpts = ExportOpts
  { eoChunks :: !(Maybe [Int])
  , eoFields :: !(Maybe [Text])
  }

defaultExportOpts :: ExportOpts
defaultExportOpts = ExportOpts Nothing Nothing

exportChunk :: TerrainSnapshot -> [Text] -> Int -> (Int, Value)
exportChunk snap fields cid =
  case IntMap.lookup cid (tsTerrainChunks snap) of
    Nothing -> (cid, Null)
    Just tc ->
      let fieldEntries = map (exportField cid tc snap) fields
      in (cid, object fieldEntries)

exportField :: Int -> TerrainChunk -> TerrainSnapshot -> Text -> (Aeson.Key, Value)
exportField cid tc snap field = (Key.fromText field, val)
  where
    val = case field of
      "elevation"    -> Aeson.toJSON (U.toList (tcElevation tc))
      "moisture"     -> Aeson.toJSON (U.toList (tcMoisture tc))
      "hardness"     -> Aeson.toJSON (U.toList (tcHardness tc))
      "fertility"    -> Aeson.toJSON (U.toList (tcFertility tc))
      "curvature"    -> Aeson.toJSON (U.toList (tcCurvature tc))
      "roughness"    -> Aeson.toJSON (U.toList (tcRoughness tc))
      "soil_depth"   -> Aeson.toJSON (U.toList (tcSoilDepth tc))
      "biome"        -> Aeson.toJSON [ biomeDisplayName b | b <- U.toList (tcFlags tc) ]
      "biome_code"   -> Aeson.toJSON [ biomeIdToCode b | b <- U.toList (tcFlags tc) ]
      "terrain_form" -> Aeson.toJSON [ Text.pack (terrainFormDisplayName f) | f <- U.toList (tcTerrainForm tc) ]
      "terrain_form_code" -> Aeson.toJSON [ terrainFormToCode f | f <- U.toList (tcTerrainForm tc) ]
      "temperature"  -> climateJson ccTempAvg
      "precipitation" -> climateJson ccPrecipAvg
      "plate_id" -> Aeson.toJSON (U.toList (tcPlateId tc))
      "plate_boundary" -> Aeson.toJSON [ plateBoundaryDisplayName b | b <- U.toList (tcPlateBoundary tc) ]
      "plate_boundary_code" -> Aeson.toJSON [ plateBoundaryToCode b | b <- U.toList (tcPlateBoundary tc) ]
      "plate_hardness" -> Aeson.toJSON (U.toList (tcPlateHardness tc))
      "plate_crust" -> Aeson.toJSON [ crustDisplayName c | c <- U.toList (tcPlateCrust tc) ]
      "plate_crust_code" -> Aeson.toJSON (U.toList (tcPlateCrust tc))
      "plate_age" -> Aeson.toJSON (U.toList (tcPlateAge tc))
      "plate_height" -> Aeson.toJSON (U.toList (tcPlateHeight tc))
      "plate_velocity" -> Aeson.toJSON (zipWith velocityMagnitude (U.toList (tcPlateVelX tc)) (U.toList (tcPlateVelY tc)))
      "plate_velocity_x" -> Aeson.toJSON (U.toList (tcPlateVelX tc))
      "plate_velocity_y" -> Aeson.toJSON (U.toList (tcPlateVelY tc))
      "weather_temperature" -> weatherJson wcTemp
      "weather_humidity" -> weatherJson wcHumidity
      "weather_wind_speed" -> weatherJson wcWindSpd
      "weather_pressure" -> weatherJson wcPressure
      "weather_precipitation" -> weatherJson wcPrecip
      "cloud_cover" -> weatherJson wcCloudCover
      "cloud_water" -> weatherJson wcCloudWater
      "cloud_cover_low" -> weatherJson wcCloudCoverLow
      "cloud_cover_mid" -> weatherJson wcCloudCoverMid
      "cloud_cover_high" -> weatherJson wcCloudCoverHigh
      "vegetation_cover" -> vegetationJson vegCover
      "vegetation_density" -> vegetationJson vegDensity
      "vegetation_albedo" -> vegetationJson vegAlbedo
      "river_discharge" -> case IntMap.lookup cid (tsRiverChunks snap) of
                             Nothing -> Null
                             Just rc -> Aeson.toJSON (U.toList (rcDischarge rc))
      _              -> Null
    climateJson accessor = case IntMap.lookup cid (tsClimateChunks snap) of
      Nothing -> Null
      Just cc -> Aeson.toJSON (U.toList (accessor cc))
    weatherJson accessor = case IntMap.lookup cid (tsWeatherChunks snap) of
      Nothing -> Null
      Just wc -> Aeson.toJSON (U.toList (accessor wc))
    vegetationJson accessor = case IntMap.lookup cid (tsVegetationChunks snap) of
      Nothing -> Null
      Just vc -> Aeson.toJSON (U.toList (accessor vc))

-- =====================================================================
-- Parsing helpers
-- =====================================================================

parseFindHexes :: Value -> Aeson.Parser ([HexFilter], Int)
parseFindHexes = Aeson.withObject "find_hexes" $ \o -> do
  filters <- o .: "filters"
  limit   <- o .:? "limit"
  fs <- mapM parseFilter filters
  pure (fs, maybe 50 (max 1 . min 1000) limit)

parseFilter :: Value -> Aeson.Parser HexFilter
parseFilter = Aeson.withObject "filter" $ \o -> do
  field <- o .: "field"
  opStr <- o .: "op"
  value <- o .: "value"
  op <- case (opStr :: Text) of
    "eq"  -> pure OpEq
    "neq" -> pure OpNeq
    "gt"  -> pure OpGt
    "gte" -> pure OpGte
    "lt"  -> pure OpLt
    "lte" -> pure OpLte
    _     -> fail ("unknown filter op: " <> Text.unpack opStr)
  pure (HexFilter field op value)

parseExportOpts :: Value -> Aeson.Parser ExportOpts
parseExportOpts = Aeson.withObject "export_terrain_data" $ \o ->
  ExportOpts <$> o .:? "chunks" <*> o .:? "fields"

parseRegionParams :: Int -> Value -> Aeson.Parser Region
parseRegionParams defaultMax = Aeson.withObject "export_mesh_data" $ \o -> do
  x0 <- maybe 0 id <$> o .:? "x0"
  y0 <- maybe 0 id <$> o .:? "y0"
  x1 <- maybe defaultMax id <$> o .:? "x1"
  y1 <- maybe defaultMax id <$> o .:? "y1"
  pure (RegionRect (TileCoord x0 y0) (TileCoord x1 y1))

parseSampleParams :: Value -> Aeson.Parser (Float, Float, Bool)
parseSampleParams = Aeson.withObject "export_sample_data" $ \o -> do
  x <- o .: "x"
  y <- o .: "y"
  realUnits <- maybe False id <$> o .:? "real_units"
  pure (x, y, realUnits)
