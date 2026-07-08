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
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word16, Word32, Word64)
import qualified Data.Vector.Unboxed as U

import Actor.AtlasCache (terrainSnapshotViewVersion)
import Actor.Data
  ( TerrainGeoContext(..)
  , TerrainSnapshot(..)
  , getTerrainSnapshot
  )
import Actor.PluginManager
  ( getPluginDataResources
  , queryPluginResource
  )
import Actor.Simulation
  ( CloudDeltaMetric(..)
  , CloudDeltaSummary(..)
  , SimulationDagSnapshot(..)
  , WeatherNodeScheduleDiagnostic(..)
  , WeatherPublicationDiagnostic(..)
  , getSimDagSnapshot
  , sdsLastCloudDelta
  , sdsLastWeatherPublication
  , sdsWeatherNodeStatus
  , weatherPublicationKindToText
  )
import Actor.SnapshotReceiver (readTerrainSnapshot)
import Actor.UI.State
  ( UiState(..)
  , ViewMode(..)
  , SourceKind(..)
  , TemporalBasis(..)
  , readUiSnapshotRef
  , sourceKindToText
  , temporalBasisToText
  , viewModeDataSemantics
  , viewModeMetadata
  , viewModeToText
  , vmdsSourceKind
  , vmdsTemporalBasis
  , vmmColorScale
  , vmmDescription
  , vmmExportFields
  , vmmInspectorFields
  , vmmLabel
  , vmmTooltipFields
  , vmmUnitLabel
  )
import Actor.UiActions.Handles (ActorHandles(..))
import Seer.Command.Context (CommandContext(..))
import Seer.Config.SliderConversion (sliderToDomainFloat)
import Seer.Config.SliderRegistry (SliderId(..))
import Seer.Draw.Overlay
  ( TerrainInspectorField(..)
  , TerrainInspectorPluginData(..)
  , TerrainInspectorSection(..)
  , TerrainInspectorView(..)
  , terrainInspectorSectionsObject
  , terrainInspectorViewAtWithPluginData
  )
import Topo.Biome.Name (biomeDisplayName)
import Topo.Calendar (WorldTime(..))
import Topo.Command.Types (SeerResponse, okResponse, errResponse)
import Topo.Plugin.DataResource (DataOperations(..), DataPagination(..), DataResourceSchema(..))
import Topo.Grid.HexDirection (traceIndexInDirection)
import Topo.Hex (HexDirection(..))
import Topo.OceanCurrent (OceanCurrentConfig(..), oceanCurrentOffset)
import Topo.Planet (tileLatLon)
import Topo.Plugin.RPC.DataService (DataQuery(..), QueryResource(..))
import Topo.Types
  ( WorldConfig(..), ChunkId(..), TileCoord(..), TileIndex(..)
  , chunkCoordFromTile, chunkIdFromCoord, tileIndex
  , TerrainChunk(..)
  , ClimateChunk(..)
  , WeatherChunk(..)
  , GlacierChunk(..)
  , GroundwaterChunk(..)
  , RiverChunk(..)
  , VegetationChunk(..)
  , VolcanismChunk(..)
  , WaterBodyChunk(..)
  , WaterBodyType
  , VentActivity
  , VentType
  , BiomeId
  , DirectionalSlope
  , TerrainForm
  , PlateBoundary
  , biomeIdToCode
  , biomeIdFromCode
  , plateBoundaryToCode
  , terrainFormToCode
  , terrainFormFromCode
  , terrainFormDisplayName
  , dsAvgSlope
  , dsMaxSlope
  , dsMinSlope
  , ventActivityToCode
  , ventTypeToCode
  , waterBodyToCode
  )
import Topo.Weather (WeatherNormalsChunk(..), getWeatherNormalsChunkFromStore)
import Topo.Units
  ( defaultUnitScales
  , normDepthToMetres
  , normSlopeToDeg
  , normToC
  , normToHPa
  , normToMetres
  , normToMmYear
  , normToRH
  , normToSoilM
  , normToWindMs
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
      let handles = ccActorHandles ctx
      snap <- readTerrainSnapshot (ahTerrainSnapshotRef handles)
      latestSnap <- getTerrainSnapshot (ahDataHandle handles)
      simDag <- getSimDagSnapshot (ahSimulationHandle handles)
      ui <- readUiSnapshotRef (ccUiSnapshotRef ctx)
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
                  pluginData <- terrainInspectorPluginDataForHex ctx chunkId tileIdx
                  let units = defaultUnitScales
                      elevationNorm = safeIndex (tcElevation tc) tileIdx
                      waterLevelNorm = uiWaterLevel ui
                      waterLevelM = normToMetres units waterLevelNorm
                      elevationM = fmap (normToMetres units) elevationNorm
                      relativeWaterNorm = fmap (\elevation -> elevation - waterLevelNorm) elevationNorm
                      relativeWaterM = fmap (\elevation -> normToMetres units elevation - waterLevelM) elevationNorm
                      terrainForm = safeIndexTF (tcTerrainForm tc) tileIdx
                      biome = safeIndexBiome (tcFlags tc) tileIdx
                      dirSlope = safeIndexDirSlope (tcDirSlope tc) tileIdx
                      avgSlope = fmap dsAvgSlope dirSlope
                      maxSlope = fmap dsMaxSlope dirSlope
                      minSlope = fmap dsMinSlope dirSlope
                      (latDeg, lonDeg) = latLonValuesForApi ui snap q r
                      climateChunk = IntMap.lookup chunkId (tsClimateChunks snap)
                      weatherChunk = IntMap.lookup chunkId (tsWeatherChunks snap)
                      weatherNormalsChunk = getWeatherNormalsChunkFromStore (ChunkId chunkId) (tsOverlayStore snap)
                      riverChunk = IntMap.lookup chunkId (tsRiverChunks snap)
                      groundwaterChunk = IntMap.lookup chunkId (tsGroundwaterChunks snap)
                      waterBodyChunk = IntMap.lookup chunkId (tsWaterBodyChunks snap)
                      glacierChunk = IntMap.lookup chunkId (tsGlacierChunks snap)
                      volcanismChunk = IntMap.lookup chunkId (tsVolcanismChunks snap)
                      vegetationChunk = IntMap.lookup chunkId (tsVegetationChunks snap)
                      waterBodyTypeAt = waterBodyChunk >>= \wb -> safeIndexWaterBody (wbType wb) tileIdx
                      adjacentWaterBodyTypeAt = waterBodyChunk >>= \wb -> safeIndexWaterBody (wbAdjacentType wb) tileIdx
                      oceanCurrentCfg = oceanCurrentConfigFromUiForApi ui
                      isWater = maybe False (< waterLevelNorm) elevationNorm
                      landEast = hasLandAlongChunk snap waterLevelNorm chunkId tileIdx HexE
                      landWest = hasLandAlongChunk snap waterLevelNorm chunkId tileIdx HexW
                      currentOffsetNorm = if isWater then oceanCurrentOffset oceanCurrentCfg (latDeg * pi / 180) landEast landWest else 0
                      currentOffsetC = normToC units (0.5 + currentOffsetNorm) - normToC units 0.5
                      terrainLayer = object
                        [ "elevation"     .= elevationNorm
                        , "elevation_m"   .= elevationM
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
                        , "terrain_form"  .= fmap (Text.pack . terrainFormDisplayName) terrainForm
                        , "terrain_form_code" .= fmap terrainFormToCode terrainForm
                        , "biome"         .= fmap biomeDisplayName biome
                        , "biome_code"    .= fmap biomeIdToCode biome
                        , "rock_type"     .= safeIndexW16 (tcRockType tc) tileIdx
                        , "soil_type"     .= safeIndexW16 (tcSoilType tc) tileIdx
                        , "plate_id"      .= safeIndexW16 (tcPlateId tc) tileIdx
                        , "plate_boundary" .= fmap plateBoundaryDisplayName (safeIndexPlateBoundary (tcPlateBoundary tc) tileIdx)
                        , "plate_boundary_code" .= fmap plateBoundaryToCode (safeIndexPlateBoundary (tcPlateBoundary tc) tileIdx)
                        , "plate_crust" .= fmap crustDisplayName (safeIndexW16 (tcPlateCrust tc) tileIdx)
                        , "plate_crust_code" .= safeIndexW16 (tcPlateCrust tc) tileIdx
                        , "plate_height"  .= safeIndex (tcPlateHeight tc) tileIdx
                        , "plate_hardness" .= safeIndex (tcPlateHardness tc) tileIdx
                        , "plate_age"     .= safeIndex (tcPlateAge tc) tileIdx
                        , "plate_velocity_x" .= safeIndex (tcPlateVelX tc) tileIdx
                        , "plate_velocity_y" .= safeIndex (tcPlateVelY tc) tileIdx
                        , "slope_avg"     .= avgSlope
                        , "slope_max"     .= maxSlope
                        , "slope_min"     .= minSlope
                        ]

                      climateLayer = case climateChunk of
                        Nothing -> Null
                        Just cc -> object
                          [ "temporal_basis"      .= temporalBasisToText LongRunAverage
                          , "source_kind"         .= sourceKindToText GeneratedClimate
                          , "temp_avg"            .= safeIndex (ccTempAvg cc) tileIdx
                          , "precip_avg"          .= safeIndex (ccPrecipAvg cc) tileIdx
                          , "wind_dir_avg"        .= safeIndex (ccWindDirAvg cc) tileIdx
                          , "wind_spd_avg"        .= safeIndex (ccWindSpdAvg cc) tileIdx
                          , "humidity_avg"        .= safeIndex (ccHumidityAvg cc) tileIdx
                          , "temp_range"          .= safeIndex (ccTempRange cc) tileIdx
                          , "precip_seasonality"  .= safeIndex (ccPrecipSeasonality cc) tileIdx
                          ]

                      weatherLayer = case weatherChunk of
                        Nothing -> Null
                        Just wc -> object
                          [ "temporal_basis" .= temporalBasisToText InstantaneousCurrent
                          , "source_kind" .= sourceKindToText SimulatedWeather
                          , "temp"     .= safeIndex (wcTemp wc) tileIdx
                          , "humidity" .= safeIndex (wcHumidity wc) tileIdx
                          , "wind_dir" .= safeIndex (wcWindDir wc) tileIdx
                          , "wind_spd" .= safeIndex (wcWindSpd wc) tileIdx
                          , "pressure" .= safeIndex (wcPressure wc) tileIdx
                          , "precip"   .= safeIndex (wcPrecip wc) tileIdx
                          , "cloud_cover" .= safeIndex (wcCloudCover wc) tileIdx
                          , "cloud_water" .= safeIndex (wcCloudWater wc) tileIdx
                          , "cloud_cover_low" .= safeIndex (wcCloudCoverLow wc) tileIdx
                          , "cloud_cover_mid" .= safeIndex (wcCloudCoverMid wc) tileIdx
                          , "cloud_cover_high" .= safeIndex (wcCloudCoverHigh wc) tileIdx
                          , "cloud_water_low" .= safeIndex (wcCloudWaterLow wc) tileIdx
                          , "cloud_water_mid" .= safeIndex (wcCloudWaterMid wc) tileIdx
                          , "cloud_water_high" .= safeIndex (wcCloudWaterHigh wc) tileIdx
                          ]

                      weatherNormalsLayer = weatherNormalsLayerJSON weatherNormalsChunk tileIdx

                      riverLayer = case riverChunk of
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
                          , "flow_dir"          .= safeIndexInt (rcFlowDir rc) tileIdx
                          , "segment_count"     .= riverSegmentCount rc tileIdx
                          ]

                      waterBodyLayer = case waterBodyChunk of
                        Nothing -> Null
                        Just wb -> object
                          [ "type"          .= fmap waterBodyDisplayName (safeIndexWaterBody (wbType wb) tileIdx)
                          , "type_code"     .= fmap waterBodyToCode (safeIndexWaterBody (wbType wb) tileIdx)
                          , "adjacent_type" .= fmap waterBodyDisplayName (safeIndexWaterBody (wbAdjacentType wb) tileIdx)
                          , "surface_elev"  .= safeIndex (wbSurfaceElev wb) tileIdx
                          , "basin_id"      .= safeIndexW32 (wbBasinId wb) tileIdx
                          , "depth"         .= safeIndex (wbDepth wb) tileIdx
                          ]

                      waterTableLayer = case groundwaterChunk of
                        Nothing -> Null
                        Just gw -> object
                          [ "storage"            .= safeIndex (gwStorage gw) tileIdx
                          , "recharge"           .= safeIndex (gwRecharge gw) tileIdx
                          , "discharge"          .= safeIndex (gwDischarge gw) tileIdx
                          , "basin_id"           .= safeIndexW32 (gwBasinId gw) tileIdx
                          , "infiltration"       .= safeIndex (gwInfiltration gw) tileIdx
                          , "water_table_depth"  .= safeIndex (gwWaterTableDepth gw) tileIdx
                          , "root_zone_moisture" .= safeIndex (gwRootZoneMoisture gw) tileIdx
                          ]

                      glacierLayer = case glacierChunk of
                        Nothing -> Null
                        Just gl -> object
                          [ "snowpack"          .= safeIndex (glSnowpack gl) tileIdx
                          , "ice_thickness"     .= safeIndex (glIceThickness gl) tileIdx
                          , "melt"              .= safeIndex (glMelt gl) tileIdx
                          , "flow"              .= safeIndex (glFlow gl) tileIdx
                          , "erosion_potential" .= safeIndex (glErosionPotential gl) tileIdx
                          , "deposit_potential" .= safeIndex (glDepositPotential gl) tileIdx
                          ]

                      volcanismLayer = case volcanismChunk of
                        Nothing -> Null
                        Just vc -> object
                          [ "vent_type"         .= fmap ventTypeDisplayName (safeIndexVentType (vcVentType vc) tileIdx)
                          , "vent_type_code"    .= fmap ventTypeToCode (safeIndexVentType (vcVentType vc) tileIdx)
                          , "activity"          .= fmap ventActivityDisplayName (safeIndexVentActivity (vcActivity vc) tileIdx)
                          , "activity_code"     .= fmap ventActivityToCode (safeIndexVentActivity (vcActivity vc) tileIdx)
                          , "magma"             .= safeIndex (vcMagma vc) tileIdx
                          , "eruption_count"    .= safeIndexW16 (vcEruptionCount vc) tileIdx
                          , "erupted_total"     .= safeIndex (vcEruptedTotal vc) tileIdx
                          , "lava_potential"    .= safeIndex (vcLavaPotential vc) tileIdx
                          , "ash_potential"     .= safeIndex (vcAshPotential vc) tileIdx
                          , "deposit_potential" .= safeIndex (vcDepositPotential vc) tileIdx
                          ]

                      inspector = terrainInspectorViewAtWithPluginData pluginData ui snap (q, r)
                      inspectorWithWeatherDiagnostics = appendWeatherTimelineDiagnostics latestSnap simDag inspector

                      vegLayer = case vegetationChunk of
                        Nothing -> Null
                        Just vc -> object
                          [ "cover"   .= safeIndex (vegCover vc) tileIdx
                          , "albedo"  .= safeIndex (vegAlbedo vc) tileIdx
                          , "density" .= safeIndex (vegDensity vc) tileIdx
                          ]

                      hypsometryLayer = object
                        [ "elevation_norm" .= elevationNorm
                        , "elevation_m" .= elevationM
                        , "water_level_norm" .= waterLevelNorm
                        , "water_level_m" .= waterLevelM
                        , "relative_water_level_norm" .= relativeWaterNorm
                        , "relative_water_level_m" .= relativeWaterM
                        , "zone" .= fmap (hypsometricZone waterLevelNorm) elevationNorm
                        ]

                      terrainFormMetricsLayer = object
                        [ "terrain_form" .= fmap (Text.pack . terrainFormDisplayName) terrainForm
                        , "terrain_form_code" .= fmap terrainFormToCode terrainForm
                        , "slope_avg_norm" .= avgSlope
                        , "slope_avg_deg" .= fmap (normSlopeToDeg units) avgSlope
                        , "slope_max_norm" .= maxSlope
                        , "slope_max_deg" .= fmap (normSlopeToDeg units) maxSlope
                        , "slope_min_norm" .= minSlope
                        , "slope_min_deg" .= fmap (normSlopeToDeg units) minSlope
                        , "curvature" .= safeIndex (tcCurvature tc) tileIdx
                        , "roughness" .= safeIndex (tcRoughness tc) tileIdx
                        , "ruggedness" .= safeIndex (tcRuggedness tc) tileIdx
                        , "relief" .= safeIndex (tcRelief tc) tileIdx
                        , "relief_2ring" .= safeIndex (tcRelief2Ring tc) tileIdx
                        , "relief_3ring" .= safeIndex (tcRelief3Ring tc) tileIdx
                        , "micro_relief" .= safeIndex (tcMicroRelief tc) tileIdx
                        ]

                      hydrologyLayer = object
                        [ "loaded" .= maybe False (const True) riverChunk
                        , "moisture" .= safeIndex (tcMoisture tc) tileIdx
                        , "river" .= riverLayer
                        ]

                      soilLayer = object
                        [ "soil_type" .= safeIndexW16 (tcSoilType tc) tileIdx
                        , "soil_depth_norm" .= safeIndex (tcSoilDepth tc) tileIdx
                        , "soil_depth_m" .= fmap (normToSoilM units) (safeIndex (tcSoilDepth tc) tileIdx)
                        , "soil_moisture" .= safeIndex (tcMoisture tc) tileIdx
                        , "fertility" .= safeIndex (tcFertility tc) tileIdx
                        , "soil_grain" .= safeIndex (tcSoilGrain tc) tileIdx
                        , "rock_type" .= safeIndexW16 (tcRockType tc) tileIdx
                        , "rock_density" .= safeIndex (tcRockDensity tc) tileIdx
                        , "hardness" .= safeIndex (tcHardness tc) tileIdx
                        ]

                      biomeRefinementLayer = object
                        [ "biome_code" .= fmap biomeIdToCode biome
                        , "biome" .= fmap biomeDisplayName biome
                        , "family" .= fmap biomeFamilyName biome
                        , "refinement" .= fmap biomeRefinementStatus biome
                        , "terrain_form" .= fmap (Text.pack . terrainFormDisplayName) terrainForm
                        , "water_type" .= fmap waterBodyDisplayName waterBodyTypeAt
                        , "adjacent_water_type" .= fmap waterBodyDisplayName adjacentWaterBodyTypeAt
                        , "temp_avg" .= (climateChunk >>= \cc -> safeIndex (ccTempAvg cc) tileIdx)
                        , "temp_avg_c" .= fmap (normToC units) (climateChunk >>= \cc -> safeIndex (ccTempAvg cc) tileIdx)
                        , "precip_avg" .= (climateChunk >>= \cc -> safeIndex (ccPrecipAvg cc) tileIdx)
                        , "precip_avg_mm_year" .= fmap (normToMmYear units) (climateChunk >>= \cc -> safeIndex (ccPrecipAvg cc) tileIdx)
                        , "moisture" .= safeIndex (tcMoisture tc) tileIdx
                        , "fertility" .= safeIndex (tcFertility tc) tileIdx
                        ]

                      climateDiagnosticsLayer = case climateChunk of
                        Nothing -> object
                          [ "loaded" .= False
                          , "status" .= ("not_loaded" :: Text)
                          , "temporal_basis" .= temporalBasisToText LongRunAverage
                          , "source_kind" .= sourceKindToText GeneratedClimate
                          ]
                        Just cc -> object
                          [ "loaded" .= True
                          , "status" .= ("loaded" :: Text)
                          , "temporal_basis" .= temporalBasisToText LongRunAverage
                          , "source_kind" .= sourceKindToText GeneratedClimate
                          , "temp_avg" .= safeIndex (ccTempAvg cc) tileIdx
                          , "temp_avg_c" .= fmap (normToC units) (safeIndex (ccTempAvg cc) tileIdx)
                          , "precip_avg" .= safeIndex (ccPrecipAvg cc) tileIdx
                          , "precip_avg_mm_year" .= fmap (normToMmYear units) (safeIndex (ccPrecipAvg cc) tileIdx)
                          , "humidity_avg_pct" .= fmap normToRH (safeIndex (ccHumidityAvg cc) tileIdx)
                          , "wind_spd_avg_ms" .= fmap (normToWindMs units) (safeIndex (ccWindSpdAvg cc) tileIdx)
                          , "wind_dir_avg_rad" .= safeIndex (ccWindDirAvg cc) tileIdx
                          , "temp_range" .= safeIndex (ccTempRange cc) tileIdx
                          , "precip_seasonality" .= safeIndex (ccPrecipSeasonality cc) tileIdx
                          ]

                      weatherTimelineLayer = object
                        [ "loaded" .= maybe False (const True) weatherChunk
                        , "status" .= (if maybe False (const True) weatherChunk then "loaded" else "not_loaded" :: Text)
                        , "tick" .= uiSimTickCount ui
                        , "world_tick" .= wtTick (tgcWorldTime (tsGeoContext snap))
                        , "auto_tick" .= uiSimAutoTick ui
                        , "tick_rate" .= uiSimTickRate ui
                        , "source" .= ("simulation_tick" :: Text)
                        , "basis" .= temporalBasisToText InstantaneousCurrent
                        , "temporal_basis" .= temporalBasisToText InstantaneousCurrent
                        , "source_kind" .= sourceKindToText SimulatedWeather
                        , "weather_version" .= tsWeatherVersion snap
                        , "published_weather_version" .= tsWeatherVersion snap
                        , "data_weather_version" .= tsWeatherVersion latestSnap
                        , "weather_node_status" .= fmap wnsStatus (sdsWeatherNodeStatus simDag)
                        , "weather_node" .= maybe Null weatherNodeScheduleDiagnosticJSON (sdsWeatherNodeStatus simDag)
                        , "last_publication" .= maybe Null weatherPublicationDiagnosticJSON (sdsLastWeatherPublication simDag)
                        , "cloud_delta" .= maybe Null cloudDeltaSummaryJSON (sdsLastCloudDelta simDag)
                        ]

                      oceanCurrentLayer = object
                        [ "status" .= (if isWater then "estimate_current_ui" else "land" :: Text)
                        , "sample_scope" .= ("same_chunk_2_hexes" :: Text)
                        , "water_tile" .= isWater
                        , "water_type" .= fmap waterBodyDisplayName waterBodyTypeAt
                        , "land_east_2" .= landEast
                        , "land_west_2" .= landWest
                        , "latitude_deg" .= latDeg
                        , "longitude_deg" .= lonDeg
                        , "temp_offset_norm" .= currentOffsetNorm
                        , "temp_offset_c" .= currentOffsetC
                        , "warm_scale" .= occWarmScale oceanCurrentCfg
                        , "cold_scale" .= occColdScale oceanCurrentCfg
                        , "lat_peak_deg" .= occLatPeakDeg oceanCurrentCfg
                        , "lat_width_deg" .= occLatWidthDeg oceanCurrentCfg
                        ]

                      unitsLayer = object
                        [ "elevation" .= object ["normalized" .= elevationNorm, "converted" .= elevationM, "unit" .= ("m" :: Text)]
                        , "water_level" .= object ["normalized" .= waterLevelNorm, "converted" .= waterLevelM, "unit" .= ("m" :: Text)]
                        , "relative_water_level" .= object ["normalized" .= relativeWaterNorm, "converted" .= relativeWaterM, "unit" .= ("m" :: Text)]
                        , "slope_avg" .= object ["normalized" .= avgSlope, "converted" .= fmap (normSlopeToDeg units) avgSlope, "unit" .= ("°" :: Text)]
                        , "temp_avg" .= object ["normalized" .= (climateChunk >>= \cc -> safeIndex (ccTempAvg cc) tileIdx), "converted" .= fmap (normToC units) (climateChunk >>= \cc -> safeIndex (ccTempAvg cc) tileIdx), "unit" .= ("°C" :: Text)]
                        , "precip_avg" .= object ["normalized" .= (climateChunk >>= \cc -> safeIndex (ccPrecipAvg cc) tileIdx), "converted" .= fmap (normToMmYear units) (climateChunk >>= \cc -> safeIndex (ccPrecipAvg cc) tileIdx), "unit" .= ("mm/yr" :: Text)]
                        , "humidity" .= object ["normalized" .= (climateChunk >>= \cc -> safeIndex (ccHumidityAvg cc) tileIdx), "converted" .= fmap normToRH (climateChunk >>= \cc -> safeIndex (ccHumidityAvg cc) tileIdx), "unit" .= ("% RH" :: Text)]
                        , "weather_wind_spd" .= object ["normalized" .= (weatherChunk >>= \wc -> safeIndex (wcWindSpd wc) tileIdx), "converted" .= fmap (normToWindMs units) (weatherChunk >>= \wc -> safeIndex (wcWindSpd wc) tileIdx), "unit" .= ("m/s" :: Text)]
                        , "weather_pressure" .= object ["normalized" .= (weatherChunk >>= \wc -> safeIndex (wcPressure wc) tileIdx), "converted" .= fmap (normToHPa units) (weatherChunk >>= \wc -> safeIndex (wcPressure wc) tileIdx), "unit" .= ("hPa" :: Text)]
                        , "soil_depth" .= object ["normalized" .= safeIndex (tcSoilDepth tc) tileIdx, "converted" .= fmap (normToSoilM units) (safeIndex (tcSoilDepth tc) tileIdx), "unit" .= ("m" :: Text)]
                        , "water_depth" .= object ["normalized" .= (waterBodyChunk >>= \wb -> safeIndex (wbDepth wb) tileIdx), "converted" .= fmap (negate . normDepthToMetres units) (waterBodyChunk >>= \wb -> safeIndex (wbDepth wb) tileIdx), "unit" .= ("m" :: Text)]
                        ]

                      activeMetadata = viewModeMetadata (uiViewMode ui)
                      activeSemantics = viewModeDataSemantics (uiViewMode ui)
                      activeViewLayer = object
                        [ "mode" .= viewModeToText (uiViewMode ui)
                        , "label" .= maybe (viewModeToText (uiViewMode ui)) vmmLabel activeMetadata
                        , "description" .= maybe Null (Aeson.toJSON . vmmDescription) activeMetadata
                        , "basis" .= fmap (temporalBasisToText . vmdsTemporalBasis) activeSemantics
                        , "temporal_basis" .= fmap (temporalBasisToText . vmdsTemporalBasis) activeSemantics
                        , "source_kind" .= fmap (sourceKindToText . vmdsSourceKind) activeSemantics
                        , "weather_version" .= activeWeatherVersion (uiViewMode ui) snap
                        , "published_weather_version" .= activeWeatherVersion (uiViewMode ui) snap
                        , "data_weather_version" .= activeWeatherVersion (uiViewMode ui) latestSnap
                        , "unit" .= maybe Null (maybe Null Aeson.toJSON . vmmUnitLabel) activeMetadata
                        , "color_scale" .= maybe Null (Aeson.toJSON . vmmColorScale) activeMetadata
                        , "tooltip_fields" .= maybe [] vmmTooltipFields activeMetadata
                        , "inspector_fields" .= maybe [] vmmInspectorFields activeMetadata
                        , "export_fields" .= maybe [] vmmExportFields activeMetadata
                        , "values" .= activeViewValues
                        ]
                      activeViewValues = case uiViewMode ui of
                        ViewElevation -> object
                          [ "elevation_norm" .= elevationNorm
                          , "elevation_m" .= elevationM
                          , "relative_water_level_m" .= relativeWaterM
                          , "terrain_form" .= fmap (Text.pack . terrainFormDisplayName) terrainForm
                          , "slope_avg_deg" .= fmap (normSlopeToDeg units) avgSlope
                          ]
                        ViewBiome -> object
                          [ "biome" .= fmap biomeDisplayName biome
                          , "biome_code" .= fmap biomeIdToCode biome
                          , "terrain_form" .= fmap (Text.pack . terrainFormDisplayName) terrainForm
                          , "elevation_m" .= elevationM
                          ]
                        ViewClimate -> object
                          [ "temp_avg" .= (climateChunk >>= \cc -> safeIndex (ccTempAvg cc) tileIdx)
                          , "temp_avg_c" .= fmap (normToC units) (climateChunk >>= \cc -> safeIndex (ccTempAvg cc) tileIdx)
                          , "precip_avg" .= (climateChunk >>= \cc -> safeIndex (ccPrecipAvg cc) tileIdx)
                          , "precip_avg_mm_year" .= fmap (normToMmYear units) (climateChunk >>= \cc -> safeIndex (ccPrecipAvg cc) tileIdx)
                          ]
                        ViewWeather -> object
                          [ "temp" .= (weatherChunk >>= \wc -> safeIndex (wcTemp wc) tileIdx)
                          , "temp_c" .= fmap (normToC units) (weatherChunk >>= \wc -> safeIndex (wcTemp wc) tileIdx)
                          , "humidity" .= (weatherChunk >>= \wc -> safeIndex (wcHumidity wc) tileIdx)
                          , "humidity_pct" .= fmap normToRH (weatherChunk >>= \wc -> safeIndex (wcHumidity wc) tileIdx)
                          , "wind_spd_ms" .= fmap (normToWindMs units) (weatherChunk >>= \wc -> safeIndex (wcWindSpd wc) tileIdx)
                          , "pressure_hpa" .= fmap (normToHPa units) (weatherChunk >>= \wc -> safeIndex (wcPressure wc) tileIdx)
                          , "precip_mm_year" .= fmap (normToMmYear units) (weatherChunk >>= \wc -> safeIndex (wcPrecip wc) tileIdx)
                          ]
                        ViewMoisture -> object
                          [ "moisture" .= safeIndex (tcMoisture tc) tileIdx
                          , "moisture_pct" .= fmap normToRH (safeIndex (tcMoisture tc) tileIdx)
                          , "soil_depth_m" .= fmap (normToSoilM units) (safeIndex (tcSoilDepth tc) tileIdx)
                          ]
                        ViewPrecip -> object
                          [ "precip_avg" .= (climateChunk >>= \cc -> safeIndex (ccPrecipAvg cc) tileIdx)
                          , "precip_avg_mm_year" .= fmap (normToMmYear units) (climateChunk >>= \cc -> safeIndex (ccPrecipAvg cc) tileIdx)
                          , "humidity_pct" .= fmap normToRH (climateChunk >>= \cc -> safeIndex (ccHumidityAvg cc) tileIdx)
                          ]
                        ViewPrecipCurrent -> object
                          [ "precip" .= (weatherChunk >>= \wc -> safeIndex (wcPrecip wc) tileIdx)
                          , "precip_mm_year" .= fmap (normToMmYear units) (weatherChunk >>= \wc -> safeIndex (wcPrecip wc) tileIdx)
                          , "humidity_pct" .= fmap normToRH (weatherChunk >>= \wc -> safeIndex (wcHumidity wc) tileIdx)
                          ]
                        ViewPlateId -> object
                          [ "plate_id" .= safeIndexW16 (tcPlateId tc) tileIdx ]
                        ViewPlateBoundary -> object
                          [ "plate_boundary" .= fmap plateBoundaryDisplayName (safeIndexPlateBoundary (tcPlateBoundary tc) tileIdx)
                          , "plate_boundary_code" .= fmap plateBoundaryToCode (safeIndexPlateBoundary (tcPlateBoundary tc) tileIdx)
                          ]
                        ViewPlateHardness -> object
                          [ "plate_hardness" .= safeIndex (tcPlateHardness tc) tileIdx ]
                        ViewPlateCrust -> object
                          [ "plate_crust" .= fmap crustDisplayName (safeIndexW16 (tcPlateCrust tc) tileIdx)
                          , "plate_crust_code" .= safeIndexW16 (tcPlateCrust tc) tileIdx
                          ]
                        ViewPlateAge -> object
                          [ "plate_age" .= safeIndex (tcPlateAge tc) tileIdx ]
                        ViewPlateHeight -> object
                          [ "plate_height" .= safeIndex (tcPlateHeight tc) tileIdx
                          , "plate_height_m" .= fmap (normToMetres units) (safeIndex (tcPlateHeight tc) tileIdx)
                          ]
                        ViewPlateVelocity -> object
                          [ "plate_velocity_x" .= safeIndex (tcPlateVelX tc) tileIdx
                          , "plate_velocity_y" .= safeIndex (tcPlateVelY tc) tileIdx
                          , "plate_velocity" .= plateVelocityAt tc tileIdx
                          ]
                        ViewVegetation -> object
                          [ "vegetation_cover" .= (vegetationChunk >>= \vc -> safeIndex (vegCover vc) tileIdx)
                          , "vegetation_density" .= (vegetationChunk >>= \vc -> safeIndex (vegDensity vc) tileIdx)
                          , "vegetation_albedo" .= (vegetationChunk >>= \vc -> safeIndex (vegAlbedo vc) tileIdx)
                          , "fertility" .= safeIndex (tcFertility tc) tileIdx
                          ]
                        ViewTerrainForm -> object
                          [ "terrain_form" .= fmap (Text.pack . terrainFormDisplayName) terrainForm
                          , "terrain_form_code" .= fmap terrainFormToCode terrainForm
                          , "slope_avg_deg" .= fmap (normSlopeToDeg units) avgSlope
                          , "elevation_m" .= elevationM
                          ]
                        ViewCloud -> object
                          [ "cloud_cover" .= (weatherChunk >>= \wc -> safeIndex (wcCloudCover wc) tileIdx)
                          , "cloud_cover_pct" .= fmap (* 100) (weatherChunk >>= \wc -> safeIndex (wcCloudCover wc) tileIdx)
                          , "cloud_water" .= (weatherChunk >>= \wc -> safeIndex (wcCloudWater wc) tileIdx)
                          , "cloud_cover_low" .= (weatherChunk >>= \wc -> safeIndex (wcCloudCoverLow wc) tileIdx)
                          , "cloud_cover_mid" .= (weatherChunk >>= \wc -> safeIndex (wcCloudCoverMid wc) tileIdx)
                          , "cloud_cover_high" .= (weatherChunk >>= \wc -> safeIndex (wcCloudCoverHigh wc) tileIdx)
                          , "cloud_water_low" .= (weatherChunk >>= \wc -> safeIndex (wcCloudWaterLow wc) tileIdx)
                          , "cloud_water_mid" .= (weatherChunk >>= \wc -> safeIndex (wcCloudWaterMid wc) tileIdx)
                          , "cloud_water_high" .= (weatherChunk >>= \wc -> safeIndex (wcCloudWaterHigh wc) tileIdx)
                          , "storm_intensity" .= stormIntensityAt weatherChunk tileIdx
                          ]
                        ViewCloudTypical -> object
                          [ "status" .= maybe ("unavailable" :: Text) (const "available") weatherNormalsChunk
                          , "cloud_cover" .= (weatherNormalsChunk >>= \wn -> safeIndex (wncCloudCover wn) tileIdx)
                          , "cloud_cover_pct" .= fmap (* 100) (weatherNormalsChunk >>= \wn -> safeIndex (wncCloudCover wn) tileIdx)
                          , "cloud_water" .= (weatherNormalsChunk >>= \wn -> safeIndex (wncCloudWater wn) tileIdx)
                          , "cloud_cover_low" .= (weatherNormalsChunk >>= \wn -> safeIndex (wncCloudCoverLow wn) tileIdx)
                          , "cloud_cover_mid" .= (weatherNormalsChunk >>= \wn -> safeIndex (wncCloudCoverMid wn) tileIdx)
                          , "cloud_cover_high" .= (weatherNormalsChunk >>= \wn -> safeIndex (wncCloudCoverHigh wn) tileIdx)
                          , "cloud_water_low" .= (weatherNormalsChunk >>= \wn -> safeIndex (wncCloudWaterLow wn) tileIdx)
                          , "cloud_water_mid" .= (weatherNormalsChunk >>= \wn -> safeIndex (wncCloudWaterMid wn) tileIdx)
                          , "cloud_water_high" .= (weatherNormalsChunk >>= \wn -> safeIndex (wncCloudWaterHigh wn) tileIdx)
                          , "storm_intensity" .= normalStormIntensityAt weatherNormalsChunk tileIdx
                          ]
                        ViewOverlay overlayName fieldIndex -> object
                          [ "overlay_name" .= overlayName
                          , "field_index" .= fieldIndex
                          ]

                  pure $ okResponse reqId $ object
                    [ "q"          .= q
                    , "r"          .= r
                    , "terrain"    .= terrainLayer
                    , "hypsometry" .= hypsometryLayer
                    , "terrain_form_metrics" .= terrainFormMetricsLayer
                    , "hydrology"  .= hydrologyLayer
                    , "climate"    .= climateLayer
                    , "climate_diagnostics" .= climateDiagnosticsLayer
                    , "weather"    .= weatherLayer
                    , "weather_snapshot" .= weatherLayer
                    , "weather_normals" .= weatherNormalsLayer
                    , "weather_typical" .= weatherNormalsLayer
                    , "weather_timeline" .= weatherTimelineLayer
                    , "river"      .= riverLayer
                    , "water_body" .= waterBodyLayer
                    , "water_bodies" .= waterBodyLayer
                    , "water_table" .= waterTableLayer
                    , "soil"       .= soilLayer
                    , "biome_refinement" .= biomeRefinementLayer
                    , "vegetation" .= vegLayer
                    , "glacier"    .= glacierLayer
                    , "glacier_snow_ice" .= glacierLayer
                    , "volcanism"  .= volcanismLayer
                    , "ocean_currents" .= oceanCurrentLayer
                    , "units"      .= unitsLayer
                    , "active_view" .= activeViewLayer
                    , "sections"   .= terrainInspectorSectionsObject inspectorWithWeatherDiagnostics
                    ]

terrainInspectorPluginDataForHex :: CommandContext -> Int -> Int -> IO [TerrainInspectorPluginData]
terrainInspectorPluginDataForHex ctx chunkId tileIdx = do
  let pmH = ahPluginManagerHandle (ccActorHandles ctx)
  resources <- getPluginDataResources pmH
  mapM queryOne
    [ (pluginName, schema)
    | (pluginName, schemas) <- Map.toList resources
    , schema <- schemas
    , drsHexBound schema
    , doQueryByHex (drsOperations schema)
    ]
  where
    queryOne (pluginName, schema) = do
      let qr = QueryResource
            { qrResource = drsName schema
            , qrQuery = QueryByHex chunkId tileIdx
            , qrPageSize = inspectorPageSize schema
            , qrPageOffset = inspectorPageOffset schema
            }
      result <- queryPluginResource (ahPluginManagerHandle (ccActorHandles ctx)) pluginName qr
      pure TerrainInspectorPluginData
        { tipdPlugin = pluginName
        , tipdSchema = schema
        , tipdResult = Just result
        }

inspectorPageSize :: DataResourceSchema -> Maybe Int
inspectorPageSize schema
  | doPage (drsOperations schema) =
      let pagination = drsPagination schema
          maxSize = max 1 (dpMaxPageSize pagination)
      in Just (clampInt 1 maxSize (dpDefaultPageSize pagination))
  | otherwise = Nothing

inspectorPageOffset :: DataResourceSchema -> Maybe Int
inspectorPageOffset schema
  | doPage (drsOperations schema) = Just (max 0 (dpDefaultPageOffset (drsPagination schema)))
  | otherwise = Nothing

clampInt :: Int -> Int -> Int -> Int
clampInt lo hi = max lo . min hi

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

-- | Safe index into an unboxed Int vector.
safeIndexInt :: U.Vector Int -> Int -> Maybe Int
safeIndexInt v i
  | i >= 0 && i < U.length v = Just (v U.! i)
  | otherwise = Nothing

safeIndexDirSlope :: U.Vector DirectionalSlope -> Int -> Maybe DirectionalSlope
safeIndexDirSlope v i
  | i >= 0 && i < U.length v = Just (v U.! i)
  | otherwise = Nothing

safeIndexWaterBody :: U.Vector WaterBodyType -> Int -> Maybe WaterBodyType
safeIndexWaterBody v i
  | i >= 0 && i < U.length v = Just (v U.! i)
  | otherwise = Nothing

safeIndexPlateBoundary :: U.Vector PlateBoundary -> Int -> Maybe PlateBoundary
safeIndexPlateBoundary v i
  | i >= 0 && i < U.length v = Just (v U.! i)
  | otherwise = Nothing

plateVelocityAt :: TerrainChunk -> Int -> Maybe Float
plateVelocityAt tc idx = do
  vx <- safeIndex (tcPlateVelX tc) idx
  vy <- safeIndex (tcPlateVelY tc) idx
  pure (sqrt (vx * vx + vy * vy))

stormIntensityAt :: Maybe WeatherChunk -> Int -> Maybe Float
stormIntensityAt weatherChunk tileIdx = do
  wc <- weatherChunk
  water <- safeIndex (wcCloudWater wc) tileIdx
  precip <- safeIndex (wcPrecip wc) tileIdx
  pure (water * min 1 (precip * 3))

normalStormIntensityAt :: Maybe WeatherNormalsChunk -> Int -> Maybe Float
normalStormIntensityAt weatherNormalsChunk tileIdx = do
  wn <- weatherNormalsChunk
  water <- safeIndex (wncCloudWater wn) tileIdx
  precip <- safeIndex (wncPrecip wn) tileIdx
  pure (water * min 1 (precip * 3))

weatherNormalsLayerJSON :: Maybe WeatherNormalsChunk -> Int -> Value
weatherNormalsLayerJSON Nothing _ = object
  [ "loaded" .= False
  , "status" .= ("unavailable" :: Text)
  , "reason" .= ("weather_normals overlay not present" :: Text)
  , "temporal_basis" .= temporalBasisToText TypicalNormal
  , "basis" .= temporalBasisToText TypicalNormal
  , "source_kind" .= sourceKindToText GeneratedClimate
  ]
weatherNormalsLayerJSON (Just normals) tileIdx = object
  [ "loaded" .= True
  , "status" .= ("loaded" :: Text)
  , "temporal_basis" .= temporalBasisToText TypicalNormal
  , "basis" .= temporalBasisToText TypicalNormal
  , "source_kind" .= sourceKindToText GeneratedClimate
  , "temp" .= safeIndex (wncTemp normals) tileIdx
  , "temperature" .= safeIndex (wncTemp normals) tileIdx
  , "humidity" .= safeIndex (wncHumidity normals) tileIdx
  , "wind_dir" .= safeIndex (wncWindDir normals) tileIdx
  , "wind_spd" .= safeIndex (wncWindSpd normals) tileIdx
  , "wind_speed" .= safeIndex (wncWindSpd normals) tileIdx
  , "precip" .= safeIndex (wncPrecip normals) tileIdx
  , "precipitation" .= safeIndex (wncPrecip normals) tileIdx
  , "cloud_cover" .= safeIndex (wncCloudCover normals) tileIdx
  , "cloud_water" .= safeIndex (wncCloudWater normals) tileIdx
  , "cloud_cover_low" .= safeIndex (wncCloudCoverLow normals) tileIdx
  , "cloud_cover_mid" .= safeIndex (wncCloudCoverMid normals) tileIdx
  , "cloud_cover_high" .= safeIndex (wncCloudCoverHigh normals) tileIdx
  , "cloud_water_low" .= safeIndex (wncCloudWaterLow normals) tileIdx
  , "cloud_water_mid" .= safeIndex (wncCloudWaterMid normals) tileIdx
  , "cloud_water_high" .= safeIndex (wncCloudWaterHigh normals) tileIdx
  ]

safeIndexVentType :: U.Vector VentType -> Int -> Maybe VentType
safeIndexVentType v i
  | i >= 0 && i < U.length v = Just (v U.! i)
  | otherwise = Nothing

safeIndexVentActivity :: U.Vector VentActivity -> Int -> Maybe VentActivity
safeIndexVentActivity v i
  | i >= 0 && i < U.length v = Just (v U.! i)
  | otherwise = Nothing

riverSegmentCount :: RiverChunk -> Int -> Maybe Int
riverSegmentCount rc tileIdx = do
  start <- safeIndexInt (rcSegOffsets rc) tileIdx
  end <- safeIndexInt (rcSegOffsets rc) (tileIdx + 1)
  pure (max 0 (end - start))

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

waterBodyDisplayName :: WaterBodyType -> Text
waterBodyDisplayName bodyType =
  case waterBodyToCode bodyType of
    0 -> "Dry"
    1 -> "Ocean"
    2 -> "Lake"
    3 -> "Inland sea"
    code -> "Unknown (" <> Text.pack (show code) <> ")"

ventTypeDisplayName :: VentType -> Text
ventTypeDisplayName ventType =
  case ventTypeToCode ventType of
    0 -> "None"
    1 -> "Shield"
    2 -> "Stratovolcano"
    3 -> "Fissure"
    code -> "Unknown (" <> Text.pack (show code) <> ")"

ventActivityDisplayName :: VentActivity -> Text
ventActivityDisplayName activity =
  case ventActivityToCode activity of
    0 -> "Dormant"
    1 -> "Active"
    2 -> "Erupting"
    code -> "Unknown (" <> Text.pack (show code) <> ")"

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

hypsometricZone :: Float -> Float -> Text
hypsometricZone waterLevel elevation
  | elevation < waterLevel = "submerged"
  | elevation < waterLevel + 0.04 = "coastal"
  | elevation < 0.55 = "lowland"
  | elevation < 0.75 = "upland"
  | otherwise = "highland"

biomeRefinementStatus :: BiomeId -> Text
biomeRefinementStatus biomeId =
  if biomeIdToCode biomeId <= 13 then "family" else "refined"

biomeFamilyName :: BiomeId -> Text
biomeFamilyName biomeId =
  case biomeIdToCode biomeId of
    0 -> "Desert"
    1 -> "Grassland"
    2 -> "Forest"
    3 -> "Tundra"
    4 -> "Rainforest"
    5 -> "Shrubland"
    6 -> "Savanna"
    7 -> "Taiga"
    8 -> "Swamp"
    10 -> "Ocean"
    11 -> "Snow"
    12 -> "Coastal"
    13 -> "Alpine"
    code
      | code `elem` [14, 15, 16, 20, 38, 63] -> "Forest"
      | code `elem` [17, 35, 36, 37] -> "Grassland"
      | code `elem` [18, 47, 48] -> "Shrubland"
      | code `elem` [19, 51, 52, 53, 54, 55] -> "Swamp"
      | code `elem` [21, 56, 57] -> "Snow"
      | code `elem` [22, 49, 50] -> "Savanna"
      | code `elem` [23, 65] -> "Taiga"
      | code `elem` [24, 25, 26, 27, 28, 29] -> "Coastal"
      | code `elem` [30, 31, 32, 33, 34, 64] -> "Desert"
      | code `elem` [39, 46] -> "Rainforest"
      | code `elem` [40, 41, 42] -> "Ocean"
      | code `elem` [43, 44, 45] -> "Tundra"
      | code == 58 -> "Alpine"
      | code `elem` [59, 60] -> "Volcanic"
      | code `elem` [61, 62] -> "Water body"
      | otherwise -> "Unknown"

oceanCurrentConfigFromUiForApi :: UiState -> OceanCurrentConfig
oceanCurrentConfigFromUiForApi ui = OceanCurrentConfig
  { occWarmScale = sliderToDomainFloat SliderOccWarmScale (uiOccWarmScale ui)
  , occColdScale = sliderToDomainFloat SliderOccColdScale (uiOccColdScale ui)
  , occLatPeakDeg = sliderToDomainFloat SliderOccLatPeakDeg (uiOccLatPeakDeg ui)
  , occLatWidthDeg = sliderToDomainFloat SliderOccLatWidthDeg (uiOccLatWidthDeg ui)
  }

latLonValuesForApi :: UiState -> TerrainSnapshot -> Int -> Int -> (Float, Float)
latLonValuesForApi _ terrainSnap tileQ tileR =
  tileLatLon (tgcPlanet geo) (tgcHexGrid geo) (tgcSlice geo) worldConfig (TileCoord tileQ tileR)
  where
    geo = tsGeoContext terrainSnap
    worldConfig = WorldConfig { wcChunkSize = tsChunkSize terrainSnap }

hasLandAlongChunk :: TerrainSnapshot -> Float -> Int -> Int -> HexDirection -> Bool
hasLandAlongChunk terrainSnap waterLevel chunkKey startIdx direction =
  case IntMap.lookup chunkKey (tsTerrainChunks terrainSnap) of
    Nothing -> False
    Just terrainChunk -> any (landAt terrainChunk) [1, 2]
  where
    chunkSize = tsChunkSize terrainSnap
    landAt terrainChunk step =
      let tracedIdx = traceIndexInDirection chunkSize chunkSize direction step startIdx
      in tracedIdx /= startIdx && maybe False (>= waterLevel) (safeIndex (tcElevation terrainChunk) tracedIdx)

appendWeatherTimelineDiagnostics :: TerrainSnapshot -> SimulationDagSnapshot -> TerrainInspectorView -> TerrainInspectorView
appendWeatherTimelineDiagnostics latestSnap simDag inspector = inspector
  { tivSections = map appendToWeatherTimeline (tivSections inspector)
  }
  where
    appendToWeatherTimeline section
      | tisKey section == "weather_timeline" = section
          { tisFields = tisFields section <> weatherTimelineInspectorFields latestSnap simDag
          }
      | otherwise = section

weatherTimelineInspectorFields :: TerrainSnapshot -> SimulationDagSnapshot -> [TerrainInspectorField]
weatherTimelineInspectorFields latestSnap simDag =
  [ inspectorWord64Field "data_weather_version" "Data weather version" (tsWeatherVersion latestSnap)
  , inspectorMaybeTextField "weather_node_status" "Weather node" (wnsStatus <$> sdsWeatherNodeStatus simDag)
  , inspectorMaybeWord64Field "next_fire_tick" "Next fire" (sdsWeatherNodeStatus simDag >>= wnsNextFireTick)
  , inspectorMaybeWord64Field "cadence_ticks" "Cadence" (sdsWeatherNodeStatus simDag >>= wnsCadenceTicks)
  , inspectorMaybeTextField "skip_reason" "Skip reason" (sdsWeatherNodeStatus simDag >>= wnsSkipReason)
  , inspectorMaybeTextField "publication_kind" "Publish kind" (weatherPublicationKindToText . wpdKind <$> sdsLastWeatherPublication simDag)
  , inspectorMaybeBoolField "data_published" "Data published" (wpdDataPublished <$> sdsLastWeatherPublication simDag)
  , inspectorMaybeBoolField "publication_pending" "Publish pending" (wpdPublicationPending <$> sdsLastWeatherPublication simDag)
  , inspectorMaybeBoolField "atlas_work_enqueued" "Atlas queued" (wpdAtlasWorkEnqueued <$> sdsLastWeatherPublication simDag)
  , inspectorMaybeBoolField "cloud_delta_changed" "Cloud delta" (cdsChanged <$> sdsLastCloudDelta simDag)
  , inspectorMaybeIntField "cloud_delta_samples" "Cloud samples" (cdsComparedSamples <$> sdsLastCloudDelta simDag)
  ]

inspectorTextField :: Text -> Text -> Text -> TerrainInspectorField
inspectorTextField key label value = TerrainInspectorField key label value (String value)

inspectorWord64Field :: Text -> Text -> Word64 -> TerrainInspectorField
inspectorWord64Field key label value = TerrainInspectorField key label (Text.pack (show value)) (Aeson.toJSON value)

inspectorIntField :: Text -> Text -> Int -> TerrainInspectorField
inspectorIntField key label value = TerrainInspectorField key label (Text.pack (show value)) (Aeson.toJSON value)

inspectorBoolField :: Text -> Text -> Bool -> TerrainInspectorField
inspectorBoolField key label value = TerrainInspectorField key label (if value then "yes" else "no") (Aeson.toJSON value)

inspectorMissingField :: Text -> Text -> TerrainInspectorField
inspectorMissingField key label = TerrainInspectorField key label "-" Null

inspectorMaybeTextField :: Text -> Text -> Maybe Text -> TerrainInspectorField
inspectorMaybeTextField key label = maybe (inspectorMissingField key label) (inspectorTextField key label)

inspectorMaybeWord64Field :: Text -> Text -> Maybe Word64 -> TerrainInspectorField
inspectorMaybeWord64Field key label = maybe (inspectorMissingField key label) (inspectorWord64Field key label)

inspectorMaybeIntField :: Text -> Text -> Maybe Int -> TerrainInspectorField
inspectorMaybeIntField key label = maybe (inspectorMissingField key label) (inspectorIntField key label)

inspectorMaybeBoolField :: Text -> Text -> Maybe Bool -> TerrainInspectorField
inspectorMaybeBoolField key label = maybe (inspectorMissingField key label) (inspectorBoolField key label)

activeWeatherVersion :: ViewMode -> TerrainSnapshot -> Maybe Word64
activeWeatherVersion ViewWeather snap = Just (tsWeatherVersion snap)
activeWeatherVersion ViewCloud snap = Just (tsWeatherVersion snap)
activeWeatherVersion ViewPrecipCurrent snap = Just (tsWeatherVersion snap)
activeWeatherVersion ViewCloudTypical snap = Just (terrainSnapshotViewVersion ViewCloudTypical snap)
activeWeatherVersion (ViewOverlay "weather" _) snap = Just (tsWeatherVersion snap)
activeWeatherVersion (ViewOverlay "weather_normals" _) snap = Just (tsOverlayVersion snap)
activeWeatherVersion _ _ = Nothing

weatherNodeScheduleDiagnosticJSON :: WeatherNodeScheduleDiagnostic -> Value
weatherNodeScheduleDiagnosticJSON diag = object
  [ "status" .= wnsStatus diag
  , "next_fire_tick" .= wnsNextFireTick diag
  , "cadence_ticks" .= wnsCadenceTicks diag
  , "skip_reason" .= wnsSkipReason diag
  ]

weatherPublicationDiagnosticJSON :: WeatherPublicationDiagnostic -> Value
weatherPublicationDiagnosticJSON diag = object
  [ "tick" .= wpdTick diag
  , "world_time" .= worldTimeJSON (wpdWorldTime diag)
  , "weather_version_before" .= wpdWeatherVersionBefore diag
  , "weather_version_after" .= wpdWeatherVersionAfter diag
  , "published_weather_version" .= wpdPublishedWeatherVersion diag
  , "publication_kind" .= weatherPublicationKindToText (wpdKind diag)
  , "weather_changed" .= wpdWeatherChanged diag
  , "data_published" .= wpdDataPublished diag
  , "publication_pending" .= wpdPublicationPending diag
  , "atlas_work_enqueued" .= wpdAtlasWorkEnqueued diag
  , "atlas_active_weather_view" .= wpdAtlasActiveWeatherView diag
  ]

cloudDeltaSummaryJSON :: CloudDeltaSummary -> Value
cloudDeltaSummaryJSON summary = object
  [ "changed" .= cdsChanged summary
  , "compared_chunks" .= cdsComparedChunks summary
  , "compared_samples" .= cdsComparedSamples summary
  , "cloud_cover" .= cloudDeltaMetricJSON (cdsCloudCover summary)
  , "cloud_water" .= cloudDeltaMetricJSON (cdsCloudWater summary)
  , "precip" .= cloudDeltaMetricJSON (cdsPrecip summary)
  ]

cloudDeltaMetricJSON :: CloudDeltaMetric -> Value
cloudDeltaMetricJSON metric = object
  [ "min_delta" .= cdmMinDelta metric
  , "max_delta" .= cdmMaxDelta metric
  , "mean_abs_delta" .= cdmMeanAbsDelta metric
  ]

worldTimeJSON :: WorldTime -> Value
worldTimeJSON worldTime = object
  [ "tick" .= wtTick worldTime
  , "tick_rate" .= wtTickRate worldTime
  ]

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
