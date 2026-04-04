{-# LANGUAGE OverloadedStrings #-}

-- | Handlers for terrain data queries:
-- @get_hex@, @get_chunks@, @get_chunk_summary@, @get_terrain_stats@.
module Seer.Command.Handlers.Terrain
  ( handleGetHex
  , handleGetChunks
  , handleGetChunkSummary
  , handleGetTerrainStats
  ) where

import Data.Aeson (Value(..), object, (.=), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Foldable (toList)
import qualified Data.IntMap.Strict as IntMap
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word16, Word32)
import qualified Data.Vector.Unboxed as U

import Actor.Data
  ( TerrainSnapshot(..)
  )
import Actor.SnapshotReceiver (readTerrainSnapshot)
import Actor.UiActions.Handles (ActorHandles(..))
import Seer.Command.Context (CommandContext(..))
import Topo.Biome.Name (biomeDisplayName)
import Topo.Command.Types (SeerResponse, okResponse, errResponse)
import Topo.Types
  ( WorldConfig(..), ChunkId(..), TileCoord(..), TileIndex(..)
  , chunkCoordFromTile, chunkIdFromCoord, tileIndex
  , TerrainChunk(..)
  , ClimateChunk(..)
  , WeatherChunk(..)
  , RiverChunk(..)
  , VegetationChunk(..)
  , BiomeId
  , TerrainForm
  , biomeIdToCode
  , biomeIdFromCode
  , terrainFormToCode
  , terrainFormFromCode
  , terrainFormDisplayName
  )

-- | Handle @get_hex@ — return full terrain data at an axial hex coordinate.
--
-- Params: @{ "q": <int>, "r": <int> }@
handleGetHex :: CommandContext -> Int -> Value -> IO SeerResponse
handleGetHex ctx reqId params =
  case Aeson.parseMaybe parseAxial params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'q' and/or 'r' parameters"
    Just (q, r) -> do
      snap <- readTerrainSnapshot (ahTerrainSnapshotRef (ccActorHandles ctx))
      let chunkSize = tsChunkSize snap
      if chunkSize <= 0
        then pure $ errResponse reqId "no terrain loaded"
        else do
          let cfg = WorldConfig { wcChunkSize = chunkSize }
              (chunkCoord, localCoord) = chunkCoordFromTile cfg (TileCoord q r)
              ChunkId chunkId = chunkIdFromCoord chunkCoord
          case tileIndex cfg localCoord of
            Nothing ->
              pure $ errResponse reqId ("invalid axial coordinate: q=" <> Text.pack (show q) <> ", r=" <> Text.pack (show r))
            Just (TileIndex tileIdx) ->
              case IntMap.lookup chunkId (tsTerrainChunks snap) of
                Nothing ->
                  pure $ errResponse reqId ("no terrain at hex: q=" <> Text.pack (show q) <> ", r=" <> Text.pack (show r))
                Just tc -> do
                  let terrainLayer = object
                        [ "elevation"     .= safeIndex (tcElevation tc) tileIdx
                        , "curvature"     .= safeIndex (tcCurvature tc) tileIdx
                        , "hardness"      .= safeIndex (tcHardness tc) tileIdx
                        , "moisture"      .= safeIndex (tcMoisture tc) tileIdx
                        , "fertility"     .= safeIndex (tcFertility tc) tileIdx
                        , "roughness"     .= safeIndex (tcRoughness tc) tileIdx
                        , "rock_density"  .= safeIndex (tcRockDensity tc) tileIdx
                        , "soil_depth"    .= safeIndex (tcSoilDepth tc) tileIdx
                        , "soil_grain"    .= safeIndex (tcSoilGrain tc) tileIdx
                        , "relief"        .= safeIndex (tcRelief tc) tileIdx
                        , "relief_2ring"  .= safeIndex (tcRelief2Ring tc) tileIdx
                        , "relief_3ring"  .= safeIndex (tcRelief3Ring tc) tileIdx
                        , "micro_relief"  .= safeIndex (tcMicroRelief tc) tileIdx
                        , "ruggedness"    .= safeIndex (tcRuggedness tc) tileIdx
                        , "terrain_form"  .= fmap (Text.pack . terrainFormDisplayName)
                                                  (safeIndexTF (tcTerrainForm tc) tileIdx)
                        , "biome"         .= fmap biomeDisplayName
                                                  (safeIndexBiome (tcFlags tc) tileIdx)
                        , "rock_type"     .= safeIndexW16 (tcRockType tc) tileIdx
                        , "soil_type"     .= safeIndexW16 (tcSoilType tc) tileIdx
                        , "plate_id"      .= safeIndexW16 (tcPlateId tc) tileIdx
                        , "plate_height"  .= safeIndex (tcPlateHeight tc) tileIdx
                        , "plate_hardness" .= safeIndex (tcPlateHardness tc) tileIdx
                        , "plate_age"     .= safeIndex (tcPlateAge tc) tileIdx
                        ]

                      climateLayer = case IntMap.lookup chunkId (tsClimateChunks snap) of
                        Nothing -> Null
                        Just cc -> object
                          [ "temp_avg"            .= safeIndex (ccTempAvg cc) tileIdx
                          , "precip_avg"          .= safeIndex (ccPrecipAvg cc) tileIdx
                          , "wind_dir_avg"        .= safeIndex (ccWindDirAvg cc) tileIdx
                          , "wind_spd_avg"        .= safeIndex (ccWindSpdAvg cc) tileIdx
                          , "humidity_avg"        .= safeIndex (ccHumidityAvg cc) tileIdx
                          , "temp_range"          .= safeIndex (ccTempRange cc) tileIdx
                          , "precip_seasonality"  .= safeIndex (ccPrecipSeasonality cc) tileIdx
                          ]

                      weatherLayer = case IntMap.lookup chunkId (tsWeatherChunks snap) of
                        Nothing -> Null
                        Just wc -> object
                          [ "temp"     .= safeIndex (wcTemp wc) tileIdx
                          , "humidity" .= safeIndex (wcHumidity wc) tileIdx
                          , "wind_dir" .= safeIndex (wcWindDir wc) tileIdx
                          , "wind_spd" .= safeIndex (wcWindSpd wc) tileIdx
                          , "pressure" .= safeIndex (wcPressure wc) tileIdx
                          , "precip"   .= safeIndex (wcPrecip wc) tileIdx
                          ]

                      riverLayer = case IntMap.lookup chunkId (tsRiverChunks snap) of
                        Nothing -> Null
                        Just rc -> object
                          [ "flow_accum"        .= safeIndex (rcFlowAccum rc) tileIdx
                          , "discharge"         .= safeIndex (rcDischarge rc) tileIdx
                          , "channel_depth"     .= safeIndex (rcChannelDepth rc) tileIdx
                          , "river_order"       .= safeIndexW16 (rcRiverOrder rc) tileIdx
                          , "basin_id"          .= safeIndexW32 (rcBasinId rc) tileIdx
                          , "baseflow"          .= safeIndex (rcBaseflow rc) tileIdx
                          , "erosion_potential"  .= safeIndex (rcErosionPotential rc) tileIdx
                          , "deposit_potential"  .= safeIndex (rcDepositPotential rc) tileIdx
                          ]

                      vegLayer = case IntMap.lookup chunkId (tsVegetationChunks snap) of
                        Nothing -> Null
                        Just vc -> object
                          [ "cover"   .= safeIndex (vegCover vc) tileIdx
                          , "albedo"  .= safeIndex (vegAlbedo vc) tileIdx
                          , "density" .= safeIndex (vegDensity vc) tileIdx
                          ]

                  pure $ okResponse reqId $ object
                    [ "q"          .= q
                    , "r"          .= r
                    , "terrain"    .= terrainLayer
                    , "climate"    .= climateLayer
                    , "weather"    .= weatherLayer
                    , "river"      .= riverLayer
                    , "vegetation" .= vegLayer
                    ]

-- | Handle @get_chunks@ — list all chunk IDs with basic stats.
handleGetChunks :: CommandContext -> Int -> Value -> IO SeerResponse
handleGetChunks ctx reqId _params = do
  snap <- readTerrainSnapshot (ahTerrainSnapshotRef (ccActorHandles ctx))
  let chunkSize = tsChunkSize snap
      tileCount = chunkSize * chunkSize
      chunks = map (chunkSummaryBrief tileCount) (IntMap.toList (tsTerrainChunks snap))
  pure $ okResponse reqId $ object
    [ "chunk_count" .= IntMap.size (tsTerrainChunks snap)
    , "chunk_size"  .= chunkSize
    , "tiles_per_chunk" .= tileCount
    , "chunks"      .= chunks
    ]

-- | Handle @get_chunk_summary@ — return per-chunk aggregate statistics.
--
-- Params: @{ "chunk": <int> }@
handleGetChunkSummary :: CommandContext -> Int -> Value -> IO SeerResponse
handleGetChunkSummary ctx reqId params =
  case Aeson.parseMaybe parseChunkId params of
    Nothing ->
      pure $ errResponse reqId "missing or invalid 'chunk' parameter"
    Just chunkId -> do
      snap <- readTerrainSnapshot (ahTerrainSnapshotRef (ccActorHandles ctx))
      case IntMap.lookup chunkId (tsTerrainChunks snap) of
        Nothing ->
          pure $ errResponse reqId ("chunk not found: " <> Text.pack (show chunkId))
        Just tc -> do
          let elev = tcElevation tc
              n    = U.length elev
              elevMin  = U.minimum elev
              elevMax  = U.maximum elev
              elevMean = U.sum elev / fromIntegral n

              -- Dominant biome (most frequent)
              biomes = tcFlags tc
              dominantBiome = findDominantBiome biomes

              -- Moisture stats
              moist = tcMoisture tc
              moistMean = U.sum moist / fromIntegral n

              -- Climate stats (if available)
              climateStats = case IntMap.lookup chunkId (tsClimateChunks snap) of
                Nothing -> Null
                Just cc ->
                  let temps = ccTempAvg cc
                      tn = U.length temps
                  in object
                    [ "temp_avg_mean"   .= (U.sum temps / fromIntegral tn)
                    , "precip_avg_mean" .= (U.sum (ccPrecipAvg cc) / fromIntegral tn)
                    ]

              -- River stats (if available)
              riverStats = case IntMap.lookup chunkId (tsRiverChunks snap) of
                Nothing -> Null
                Just rc ->
                  let disc = rcDischarge rc
                      riverTiles = U.length (U.filter (> 0) disc)
                  in object
                    [ "river_tile_count" .= riverTiles
                    , "max_discharge"    .= if U.null disc then (0 :: Float) else U.maximum disc
                    ]

              -- Terrain form distribution
              formDist = computeTerrainFormDist (tcTerrainForm tc)

          pure $ okResponse reqId $ object
            [ "chunk"          .= chunkId
            , "tile_count"     .= n
            , "elevation_min"  .= elevMin
            , "elevation_max"  .= elevMax
            , "elevation_mean" .= elevMean
            , "moisture_mean"  .= moistMean
            , "dominant_biome" .= dominantBiome
            , "terrain_form_distribution" .= formDist
            , "climate"        .= climateStats
            , "river"          .= riverStats
            ]

-- | Handle @get_terrain_stats@ — return global aggregate terrain statistics.
handleGetTerrainStats :: CommandContext -> Int -> Value -> IO SeerResponse
handleGetTerrainStats ctx reqId _params = do
  snap <- readTerrainSnapshot (ahTerrainSnapshotRef (ccActorHandles ctx))
  let chunks = IntMap.elems (tsTerrainChunks snap)
  if null chunks
    then pure $ okResponse reqId $ object
      [ "status" .= ("no_terrain_data" :: Text)
      , "chunk_count" .= (0 :: Int)
      ]
    else do
      -- Collect all elevation vectors
      let allElev = concatMap (U.toList . tcElevation) chunks
          totalTiles = length allElev
          elevMin  = minimum allElev
          elevMax  = maximum allElev
          elevSum  = sum allElev
          elevMean = elevSum / fromIntegral totalTiles

          -- Biome distribution
          allBiomes = concatMap (U.toList . tcFlags) chunks
          biomeDist = computeBiomeDist allBiomes totalTiles

          -- Terrain form distribution
          allForms = concatMap (U.toList . tcTerrainForm) chunks
          formDist = computeFormDist allForms totalTiles

          -- Moisture
          allMoist = concatMap (U.toList . tcMoisture) chunks
          moistMean = sum allMoist / fromIntegral totalTiles

          -- Climate stats
          climateChunks = IntMap.elems (tsClimateChunks snap)
          allTemps = concatMap (U.toList . ccTempAvg) climateChunks
          allPrecip = concatMap (U.toList . ccPrecipAvg) climateChunks
          tempStats = if null allTemps then Null
                      else object
                        [ "min"  .= minimum allTemps
                        , "max"  .= maximum allTemps
                        , "mean" .= (sum allTemps / fromIntegral (length allTemps))
                        ]
          precipStats = if null allPrecip then Null
                        else object
                          [ "min"  .= minimum allPrecip
                          , "max"  .= maximum allPrecip
                          , "mean" .= (sum allPrecip / fromIntegral (length allPrecip))
                          ]

          -- Vegetation
          vegChunks = IntMap.elems (tsVegetationChunks snap)
          allCover = concatMap (U.toList . vegCover) vegChunks
          vegStats = if null allCover then Null
                     else object
                       [ "cover_mean"   .= (sum allCover / fromIntegral (length allCover))
                       , "density_mean" .= let ds = concatMap (U.toList . vegDensity) vegChunks
                                           in sum ds / fromIntegral (length ds)
                       ]

          -- River stats
          riverChunks = IntMap.elems (tsRiverChunks snap)
          allDischarge = concatMap (U.toList . rcDischarge) riverChunks
          riverTileCount = length (filter (> 0) allDischarge)
          riverStats = if null allDischarge then Null
                       else object
                         [ "river_tile_count" .= riverTileCount
                         , "max_discharge"    .= maximum allDischarge
                         ]

      pure $ okResponse reqId $ object
        [ "chunk_count"    .= IntMap.size (tsTerrainChunks snap)
        , "total_tiles"    .= totalTiles
        , "elevation"      .= object
            [ "min"  .= elevMin
            , "max"  .= elevMax
            , "mean" .= elevMean
            ]
        , "moisture_mean"  .= moistMean
        , "biome_distribution"         .= biomeDist
        , "terrain_form_distribution"  .= formDist
        , "temperature"    .= tempStats
        , "precipitation"  .= precipStats
        , "vegetation"     .= vegStats
        , "river"          .= riverStats
        ]

-- =====================================================================
-- Helpers
-- =====================================================================

parseAxial :: Value -> Aeson.Parser (Int, Int)
parseAxial = Aeson.withObject "params" $ \o ->
  (,) <$> o .: "q" <*> o .: "r"

parseChunkId :: Value -> Aeson.Parser Int
parseChunkId = Aeson.withObject "params" (.: "chunk")

-- | Safe index into an unboxed Float vector.
safeIndex :: U.Vector Float -> Int -> Maybe Float
safeIndex v i
  | i >= 0 && i < U.length v = Just (v U.! i)
  | otherwise = Nothing

-- | Safe index into an unboxed Word16 vector.
safeIndexW16 :: U.Vector Word16 -> Int -> Maybe Word16
safeIndexW16 v i
  | i >= 0 && i < U.length v = Just (v U.! i)
  | otherwise = Nothing

-- | Safe index into an unboxed Word32 vector.
safeIndexW32 :: U.Vector Word32 -> Int -> Maybe Word32
safeIndexW32 v i
  | i >= 0 && i < U.length v = Just (v U.! i)
  | otherwise = Nothing

-- | Safe index for TerrainForm vectors.
safeIndexTF :: U.Vector TerrainForm -> Int -> Maybe TerrainForm
safeIndexTF v i
  | i >= 0 && i < U.length v = Just (v U.! i)
  | otherwise = Nothing

-- | Safe index for BiomeId vectors.
safeIndexBiome :: U.Vector BiomeId -> Int -> Maybe BiomeId
safeIndexBiome v i
  | i >= 0 && i < U.length v = Just (v U.! i)
  | otherwise = Nothing

-- | Brief summary of a chunk for listing.
chunkSummaryBrief :: Int -> (Int, TerrainChunk) -> Value
chunkSummaryBrief tileCount (cid, tc) =
  let elev = tcElevation tc
  in object
    [ "chunk_id"       .= cid
    , "tile_count"     .= tileCount
    , "elevation_min"  .= if U.null elev then (0 :: Float) else U.minimum elev
    , "elevation_max"  .= if U.null elev then (0 :: Float) else U.maximum elev
    ]

-- | Find the most frequently occurring biome in a vector.
findDominantBiome :: U.Vector BiomeId -> Text
findDominantBiome biomes
  | U.null biomes = "none"
  | otherwise =
      let counts = IntMap.fromListWith (+)
            [(fromIntegral (biomeIdToCode b), 1 :: Int) | b <- U.toList biomes]
          (maxCode, _) = IntMap.foldlWithKey'
            (\(bestK, bestV) k v -> if v > bestV then (k, v) else (bestK, bestV))
            (-1, 0) counts
      in case biomeIdFromCode (fromIntegral maxCode) of
           Right bid -> biomeDisplayName bid
           Left _    -> "unknown"

-- | Compute terrain form distribution as @[{name, count}]@.
computeTerrainFormDist :: U.Vector TerrainForm -> [Value]
computeTerrainFormDist forms =
  let counts = IntMap.fromListWith (+)
        [(fromIntegral (terrainFormToCode f), 1 :: Int) | f <- U.toList forms]
  in [ object $
        [ "count" .= cnt
        ] <> case terrainFormFromCode (fromIntegral code) of
               Right tf -> ["name" .= Text.pack (terrainFormDisplayName tf)]
               Left _   -> ["name" .= ("unknown" :: Text)]
     | (code, cnt) <- IntMap.toList counts
     ]

-- | Compute biome distribution as @[{name, count, pct}]@.
computeBiomeDist :: [BiomeId] -> Int -> [Value]
computeBiomeDist biomes total =
  let counts = IntMap.fromListWith (+)
        [(fromIntegral (biomeIdToCode b), 1 :: Int) | b <- biomes]
  in [ object $
        [ "count" .= cnt
        , "pct"   .= (100.0 * fromIntegral cnt / fromIntegral total :: Double)
        ] <> case biomeIdFromCode (fromIntegral code) of
               Right bid -> ["name" .= biomeDisplayName bid]
               Left _    -> ["name" .= ("unknown" :: Text)]
     | (code, cnt) <- IntMap.toList counts
     ]

-- | Compute terrain form distribution with percentages.
computeFormDist :: [TerrainForm] -> Int -> [Value]
computeFormDist forms total =
  let counts = IntMap.fromListWith (+)
        [(fromIntegral (terrainFormToCode f), 1 :: Int) | f <- forms]
  in [ object $
        [ "count" .= cnt
        , "pct"   .= (100.0 * fromIntegral cnt / fromIntegral total :: Double)
        ] <> case terrainFormFromCode (fromIntegral code) of
               Right tf -> ["name" .= Text.pack (terrainFormDisplayName tf)]
               Left _   -> ["name" .= ("unknown" :: Text)]
     | (code, cnt) <- IntMap.toList counts
     ]
