{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Spec.TerrainInspector (spec) where

import Control.Monad (forM_)
import Actor.Data (TerrainGeoContext(..), TerrainSnapshot(..), defaultTerrainGeoContext)
import Actor.UI (UiState(..), ViewMode(..), emptyUiState)
import Actor.UI.State (allBuiltinViewModes)
import Data.Aeson (Value(..))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.IntMap.Strict as IntMap
import Data.List (find)
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Vector.Unboxed as U
import Seer.Draw.Overlay
  ( TerrainInspectorField(..)
  , TerrainInspectorPluginData(..)
  , TerrainInspectorSection(..)
  , TerrainInspectorView(..)
  , terrainInspectorPanelLineHardCap
  , terrainInspectorPanelLinesForHeight
  , terrainInspectorPinnedView
  , terrainInspectorView
  , terrainInspectorViewAt
  , terrainInspectorViewAtWithPluginData
  )
import Spec.Support.OverlayFixtures (mkSparseFloatOverlay)
import Test.Hspec
import Topo.Calendar (CalendarConfig(..), CalendarDate(..), WorldTime(..), mkCalendarConfig, tickToDate)
import Topo.Hex (HexGridMeta(..))
import Topo.Planet (PlanetConfig(..), WorldSlice(..), defaultPlanetConfig, defaultWorldSlice, tileLatLon)
import Topo.Solar (localSolarHour)
import UI.DayNight (mkDayNightFn)
import Topo
  ( ClimateChunk(..)
  , GlacierChunk(..)
  , GroundwaterChunk(..)
  , TerrainChunk(..)
  , VegetationChunk(..)
  , VolcanismChunk(..)
  , WaterBodyChunk(..)
  , WeatherChunk(..)
  , zeroDirSlope
  )
import Topo.Overlay (OverlayProvenance(..), emptyOverlayStore, insertOverlay)
import Topo.Plugin.DataResource
  ( DataFieldDef(..)
  , DataFieldType(..)
  , DataOperations(..)
  , DataResourceSchema(..)
  , currentDataResourceSchemaVersion
  , defaultDataPagination
  , defaultDataResourceVersion
  , noOperations
  )
import Topo.Plugin.RPC.DataService (DataRecord(..), QueryResult(..))
import Topo.Types
  ( TileCoord(..)
  , WorldConfig(..)
  , pattern BiomeDesert
  , pattern BiomeTempRainforest
  , pattern FormFlat
  , pattern PlateBoundaryConvergent
  , pattern PlateBoundaryNone
  , pattern VentActive
  , pattern VentShield
  , pattern WaterLake
  )

spec :: Spec
spec = describe "terrain inspector view model" $ do
  it "is absent until a hover hex is available" $ do
    terrainInspectorView emptyUiState emptyTerrainSnapshot `shouldBe` Nothing

  it "reports the hovered hex and no-data state" $ do
    let ui = emptyUiState { uiHoverHex = Just (3, -2) }
    fmap tivLines (terrainInspectorView ui emptyTerrainSnapshot)
      `shouldBe` Just ["Hex (3, -2)", "No data"]

  it "builds mode-specific lines from terrain samples" $ do
    let ui = emptyUiState { uiHoverHex = Just (0, 0), uiViewMode = ViewElevation }
        Just view = terrainInspectorView ui terrainSnapshotWithChunk
    tivHex view `shouldBe` (0, 0)
    take 1 (tivLines view) `shouldBe` ["Hex (0, 0)"]
    tivLines view `shouldSatisfy` any (Text.isPrefixOf "Elev")
    tivLines view `shouldSatisfy` any (Text.isPrefixOf "Form")
    tivLines view `shouldSatisfy` any (Text.isPrefixOf "Slope")
    map tisKey (tivSections view) `shouldBe` canonicalInspectorSectionKeys

  it "uses the selected context hex for pinned inspectors before hover fallback" $ do
    let ui = emptyUiState
          { uiHoverHex = Just (0, 0)
          , uiContextHex = Just (1, 0)
          }
        Just view = terrainInspectorPinnedView ui terrainSnapshotWithChunk
    tivHex view `shouldBe` (1, 0)

  it "builds compact bounded panel lines with common fields for every view" $ do
    let overlayMode = ViewOverlay "culture" 0
        modes = allBuiltinViewModes <> [overlayMode]
    forM_ modes $ \mode -> do
      let ui = emptyUiState { uiHoverHex = Just (0, 0), uiViewMode = mode }
          Just view = terrainInspectorView ui terrainSnapshotWithDomainLayers
          panel = tivPanelLines view
      length panel `shouldSatisfy` (<= terrainInspectorPanelLineHardCap)
      panel `shouldSatisfy` any (== "Hex (0, 0)")
      panel `shouldSatisfy` any (Text.isPrefixOf "Chunk ")
      panel `shouldSatisfy` any (Text.isPrefixOf "Elev ")
      panel `shouldSatisfy` any (Text.isPrefixOf "Zone ")
      panel `shouldSatisfy` any (Text.isInfixOf "get_hex.sections")

  it "keeps compact panel lines scoped to the active view mode" $ do
    let panelFor mode =
          let ui = emptyUiState { uiHoverHex = Just (0, 0), uiViewMode = mode }
              Just view = terrainInspectorView ui terrainSnapshotWithDomainLayers
          in tivPanelLines view
        climatePanel = panelFor ViewClimate
        platePanel = panelFor ViewPlateId
        biomePanel = panelFor ViewBiome
    climatePanel `shouldSatisfy` any (Text.isInfixOf "Avg climate temp")
    climatePanel `shouldSatisfy` any (Text.isInfixOf "Avg climate precip")
    climatePanel `shouldNotSatisfy` any (Text.isInfixOf "Plate ")
    platePanel `shouldSatisfy` any (Text.isInfixOf "Plate 0")
    platePanel `shouldNotSatisfy` any (Text.isInfixOf "Avg climate temp")
    biomePanel `shouldSatisfy` any (Text.isInfixOf "Biome")
    biomePanel `shouldNotSatisfy` any (Text.isInfixOf "Pressure")

  it "clarifies current simulated Cloud/Storm aggregate render and layer context in inspector lines" $ do
    let ui = emptyUiState { uiHoverHex = Just (0, 0), uiViewMode = ViewCloud }
        Just view = terrainInspectorView ui terrainSnapshotWithDomainLayers
    tivLines view `shouldSatisfy` elem "Current simulated clouds/storm aggregate render"
    tivLines view `shouldSatisfy` elem "  Layer fields: context only"

  it "reports typical cloud normals as unavailable without falling back to current clouds" $ do
    let ui = emptyUiState { uiHoverHex = Just (0, 0), uiViewMode = ViewCloudTypical }
        Just view = terrainInspectorView ui terrainSnapshotWithDomainLayers
    tivLines view `shouldSatisfy` elem "Typical weather normals unavailable"
    tivLines view `shouldSatisfy` elem "No weather_normals overlay is loaded; not using current clouds as a fallback"
    tivPanelLines view `shouldSatisfy` any (Text.isInfixOf "unavailable")

  it "uses the render geo/time context for solar inspector lines" $ do
    let snap = solarTerrainSnapshot
        ui = emptyUiState { uiViewMode = ViewElevation }
        centerView = terrainInspectorViewAt ui snap (8, 8)
        eastView = terrainInspectorViewAt ui snap (12, 8)
        Just dayNightFn = mkDayNightFn snap
        centerLocalLine = expectedLocalLine snap 8 8
        eastLocalLine = expectedLocalLine snap 12 8
    centerLocalLine `shouldNotBe` eastLocalLine
    tivLines centerView `shouldSatisfy` elem "--- Sun ---"
    tivLines centerView `shouldSatisfy` elem centerLocalLine
    tivLines eastView `shouldSatisfy` elem eastLocalLine
    dayNightFn 12 8 `shouldSatisfy` (> dayNightFn 8 8)

  it "clips compact panel lines by available height and truncates long display values" $ do
    let longOverlay = Text.replicate 140 "x"
        ui = emptyUiState
          { uiHoverHex = Just (0, 0)
          , uiViewMode = ViewOverlay longOverlay 0
          }
        Just view = terrainInspectorView ui terrainSnapshotWithDomainLayers
        shortPanel = terrainInspectorPanelLinesForHeight 60 view
    length shortPanel `shouldSatisfy` (<= 2)
    shortPanel `shouldSatisfy` any (Text.isInfixOf "get_hex.sections")
    shortPanel `shouldSatisfy` any (Text.isInfixOf "Export/provenance JSON")
    map Text.length (tivPanelLines view) `shouldSatisfy` all (<= 96)
    tivLines view `shouldSatisfy` any ((> 96) . Text.length)

  it "scopes overlay compact panel details to the selected overlay" $ do
    let culture = mkSparseFloatOverlay "culture" "Culture score" 0.87 (OverlayProvenance 99 3 "plugin:civ" Nothing)
        roads = mkSparseFloatOverlay "roads" "Road density" 0.42 (OverlayProvenance 100 4 "plugin:roads" Nothing)
        snap = terrainSnapshotWithChunk { tsOverlayStore = insertOverlay roads (insertOverlay culture emptyOverlayStore) }
        ui = emptyUiState
          { uiHoverHex = Just (0, 0)
          , uiViewMode = ViewOverlay "roads" 0
          }
        Just view = terrainInspectorView ui snap
        panel = tivPanelLines view
    panel `shouldSatisfy` any (Text.isInfixOf "Overlay roads")
    panel `shouldSatisfy` any (Text.isInfixOf "roads sparse_record")
    panel `shouldSatisfy` any (Text.isInfixOf "roads plugin:roads v4 seed 100")
    panel `shouldNotSatisfy` any (Text.isInfixOf "culture sparse_record")

  it "collapses overlay, plugin, provenance, and export details in the compact default" $ do
    let provenance = OverlayProvenance 99 3 "plugin:civ" Nothing
        overlay = mkSparseFloatOverlay "culture" "Culture score" 0.87 provenance
        snap = terrainSnapshotWithChunk { tsOverlayStore = insertOverlay overlay emptyOverlayStore }
        record = DataRecord (Map.fromList [("id", Number 1), ("name", String "Hillfort")])
        result = QueryResult "settlements" [record] (Just 1)
        pluginData = [TerrainInspectorPluginData "civ" settlementResourceSchema (Just (Right result))]
        ui = emptyUiState { uiHoverHex = Just (0, 0), uiViewMode = ViewElevation }
        view = terrainInspectorViewAtWithPluginData pluginData ui snap (0, 0)
        panel = tivPanelLines view
    panel `shouldSatisfy` any (Text.isInfixOf "overlays 1")
    panel `shouldSatisfy` any (Text.isInfixOf "plugins 1")
    panel `shouldNotSatisfy` any (Text.isInfixOf "Plugin records JSON")
    panel `shouldNotSatisfy` any (Text.isInfixOf "plugin:civ v3 seed 99")
    tivLines view `shouldSatisfy` any (Text.isInfixOf "Plugin records JSON civ:settlements")
    map tisKey (tivSections view) `shouldBe` canonicalInspectorSectionKeys
    map tisKey (tivSections view) `shouldSatisfy` elem "overlay_provenance"
    map tisKey (tivSections view) `shouldSatisfy` elem "export_links"

  it "surfaces populated water body and water-table section fields" $ do
    let ui = emptyUiState { uiHoverHex = Just (0, 0), uiViewMode = ViewElevation }
        Just view = terrainInspectorView ui terrainSnapshotWithWater
    tivLines view `shouldSatisfy` any (Text.isInfixOf "Type Lake")
    tivLines view `shouldSatisfy` any (Text.isInfixOf "Storage 0.25")

  it "surfaces climate, weather, biome, soil, vegetation, glacier, volcanism, and currents" $ do
    let ui = emptyUiState { uiHoverHex = Just (0, 0), uiViewMode = ViewClimate }
        Just view = terrainInspectorView ui terrainSnapshotWithDomainLayers
    map tisKey (tivSections view) `shouldSatisfy` elem "climate_weather"
    map tisKey (tivSections view) `shouldSatisfy` elem "weather_snapshot"
    map tisKey (tivSections view) `shouldSatisfy` elem "weather_normals"
    map tisKey (tivSections view) `shouldSatisfy` elem "biome_refinement"
    map tisKey (tivSections view) `shouldSatisfy` elem "soil"
    map tisKey (tivSections view) `shouldSatisfy` elem "vegetation"
    map tisKey (tivSections view) `shouldSatisfy` elem "glacier_snow_ice"
    map tisKey (tivSections view) `shouldSatisfy` elem "volcanism"
    map tisKey (tivSections view) `shouldSatisfy` elem "ocean_currents"
    tivLines view `shouldSatisfy` elem "--- Average Climate ---"
    tivLines view `shouldSatisfy` elem "--- Current Simulated Weather ---"
    tivLines view `shouldSatisfy` elem "--- Typical Weather Normals ---"
    tivLines view `shouldSatisfy` any (Text.isInfixOf "Avg climate temp")
    tivLines view `shouldSatisfy` any (Text.isInfixOf "Current cloud")
    tivLines view `shouldSatisfy` any (Text.isInfixOf "Basis long_run_average")
    tivLines view `shouldSatisfy` any (Text.isInfixOf "Source climate_average")
    tivLines view `shouldSatisfy` any (Text.isInfixOf "Source weather_snapshot")
    tivLines view `shouldSatisfy` any (Text.isInfixOf "Source weather_normals")
    tivLines view `shouldSatisfy` any (Text.isInfixOf "Weather version")
    tivLines view `shouldSatisfy` any (Text.isInfixOf "Family Rainforest")
    tivLines view `shouldSatisfy` any (Text.isInfixOf "Depth 3.0 m")
    tivLines view `shouldSatisfy` any (Text.isInfixOf "Cover 0.67")
    tivLines view `shouldSatisfy` any (Text.isInfixOf "Ice 0.45")
    tivLines view `shouldSatisfy` any (Text.isInfixOf "Vent Shield")
    tivLines view `shouldSatisfy` any (Text.isInfixOf "Est temp Δ")

  it "surfaces missing overlay state for overlay view mode" $ do
    let ui = emptyUiState
          { uiHoverHex = Just (0, 0)
          , uiViewMode = ViewOverlay "culture" 0
          }
        Just view = terrainInspectorView ui terrainSnapshotWithChunk
    tivLines view `shouldSatisfy` elem "Overlay culture"
    tivLines view `shouldSatisfy` elem "(not loaded)"

  it "surfaces overlay records, schema, provenance, units, and JSON export affordances" $ do
    let provenance = OverlayProvenance 99 3 "plugin:civ" Nothing
        overlay = mkSparseFloatOverlay "culture" "Culture score" 0.87 provenance
        snap = terrainSnapshotWithChunk { tsOverlayStore = insertOverlay overlay emptyOverlayStore }
        ui = emptyUiState { uiHoverHex = Just (0, 0) }
        Just view = terrainInspectorView ui snap
    map tisKey (tivSections view) `shouldSatisfy` elem "overlay_records"
    map tisKey (tivSections view) `shouldSatisfy` elem "overlay_schema"
    map tisKey (tivSections view) `shouldSatisfy` elem "overlay_provenance"
    map tisKey (tivSections view) `shouldSatisfy` elem "unit_conversions"
    map tisKey (tivSections view) `shouldSatisfy` elem "export_links"
    tivLines view `shouldSatisfy` any (Text.isInfixOf "culture sparse_record")
    tivLines view `shouldSatisfy` any (Text.isInfixOf "culture sparse · 1 fields")
    tivLines view `shouldSatisfy` any (Text.isInfixOf "culture plugin:civ v3 seed 99")
    tivLines view `shouldSatisfy` any (Text.isInfixOf "Elevation -5160.0 m")
    tivLines view `shouldSatisfy` any (Text.isInfixOf "Copy hex JSON GET /terrain/hex?q=0&r=0")

  it "surfaces plugin hex-bound data records when service queries provide them" $ do
    let ui = emptyUiState { uiHoverHex = Just (0, 0) }
        record = DataRecord (Map.fromList [("id", Number 1), ("name", String "Hillfort")])
        result = QueryResult "settlements" [record] (Just 1)
        pluginData = [TerrainInspectorPluginData "civ" settlementResourceSchema (Just (Right result))]
        view = terrainInspectorViewAtWithPluginData pluginData ui terrainSnapshotWithChunk (0, 0)
    map tisKey (tivSections view) `shouldSatisfy` elem "plugin_hex_data"
    tivLines view `shouldSatisfy` any (Text.isInfixOf "Settlements 1 records")
    tivLines view `shouldSatisfy` any (Text.isInfixOf "Plugin records JSON civ:settlements")
    let Just pluginSection = find ((== "plugin_hex_data") . tisKey) (tivSections view)
        [pluginField] = tisFields pluginSection
    case tifRaw pluginField of
      Object raw ->
        case KeyMap.lookup "schema" raw of
          Just (Object schemaRaw) -> do
            KeyMap.lookup "hex_bound" schemaRaw `shouldBe` Just (Bool True)
            KeyMap.lookup "hexBound" schemaRaw `shouldBe` Nothing
          _ -> expectationFailure "expected plugin schema raw object"
      _ -> expectationFailure "expected plugin field raw object"

canonicalInspectorSectionKeys :: [Text]
canonicalInspectorSectionKeys =
  [ "coordinates"
  , "elevation_hypsometry"
  , "tectonics_plates"
  , "erosion_terrain_form"
  , "hydrology_rivers"
  , "water_bodies"
  , "water_table"
  , "climate_weather"
  , "weather_snapshot"
  , "weather_normals"
  , "weather_timeline"
  , "biome_refinement"
  , "soil"
  , "vegetation"
  , "glacier_snow_ice"
  , "volcanism"
  , "ocean_currents"
  , "overlay_records"
  , "overlay_schema"
  , "overlay_provenance"
  , "plugin_hex_data"
  , "stage_provenance"
  , "unit_conversions"
  , "export_links"
  ]

emptyTerrainSnapshot :: TerrainSnapshot
emptyTerrainSnapshot = TerrainSnapshot 0 0 0 0 0 0 IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty emptyOverlayStore defaultTerrainGeoContext

settlementResourceSchema :: DataResourceSchema
settlementResourceSchema = DataResourceSchema
  { drsSchemaVersion = currentDataResourceSchemaVersion
  , drsResourceVersion = defaultDataResourceVersion
  , drsName = "settlements"
  , drsLabel = "Settlements"
  , drsHexBound = True
  , drsFields =
      [ DataFieldDef "id" DFInt "Id" False Nothing
      , DataFieldDef "name" DFText "Name" False Nothing
      ]
  , drsOperations = noOperations { doQueryByHex = True }
  , drsKeyField = "id"
  , drsOverlay = Nothing
  , drsPagination = defaultDataPagination
  }

terrainSnapshotWithChunk :: TerrainSnapshot
terrainSnapshotWithChunk = TerrainSnapshot
  { tsVersion = 1
  , tsClimateVersion = 0
  , tsWeatherVersion = 0
  , tsVegetationVersion = 0
  , tsOverlayVersion = 0
  , tsChunkSize = chunkSize
  , tsTerrainChunks = IntMap.singleton 0 (emptyTerrainChunk chunkSize)
  , tsClimateChunks = IntMap.empty
  , tsWeatherChunks = IntMap.empty
  , tsRiverChunks = IntMap.empty
  , tsGroundwaterChunks = IntMap.empty
  , tsVolcanismChunks = IntMap.empty
  , tsGlacierChunks = IntMap.empty
  , tsWaterBodyChunks = IntMap.empty
  , tsVegetationChunks = IntMap.empty
  , tsOverlayStore = emptyOverlayStore
  , tsGeoContext = defaultTerrainGeoContext
  }

solarTerrainSnapshot :: TerrainSnapshot
solarTerrainSnapshot = terrainSnapshotWithChunk
  { tsChunkSize = 16
  , tsTerrainChunks = IntMap.singleton 0 (emptyTerrainChunk 16)
  , tsGeoContext = defaultTerrainGeoContext
      { tgcWorldTime = WorldTime 6 3600
      , tgcPlanet = defaultPlanetConfig { pcAxialTilt = 0 }
      , tgcSlice = defaultWorldSlice { wsLatCenter = 0, wsLonCenter = 0 }
      , tgcHexGrid = HexGridMeta 1000
      }
  }

expectedLocalLine :: TerrainSnapshot -> Int -> Int -> Text
expectedLocalLine snap tileQ tileR = "Local " <> formatInspectorHour localHour
  where
    geo = tsGeoContext snap
    planet = tgcPlanet geo
    calCfg = mkCalendarConfig planet
    calDate = tickToDate calCfg (tgcWorldTime geo)
    worldConfig = WorldConfig { wcChunkSize = tsChunkSize snap }
    (_, lonDeg) = tileLatLon planet (tgcHexGrid geo) (tgcSlice geo) worldConfig (TileCoord tileQ tileR)
    localHour = localSolarHour (realToFrac (ccHoursPerDay calCfg)) (realToFrac (cdHourOfDay calDate)) lonDeg

formatInspectorHour :: Float -> Text
formatInspectorHour hour =
  let hrs = floor hour :: Int
      mins = round ((hour - fromIntegral hrs) * 60) :: Int
  in Text.pack (show hrs) <> ":" <> (if mins < 10 then "0" else "") <> Text.pack (show mins)

terrainSnapshotWithWater :: TerrainSnapshot
terrainSnapshotWithWater = terrainSnapshotWithChunk
  { tsGroundwaterChunks = IntMap.singleton 0 (groundwaterChunk chunkSize)
  , tsWaterBodyChunks = IntMap.singleton 0 (waterBodyChunk chunkSize)
  }

terrainSnapshotWithDomainLayers :: TerrainSnapshot
terrainSnapshotWithDomainLayers = terrainSnapshotWithWater
  { tsTerrainChunks = IntMap.singleton 0 (domainTerrainChunk chunkSize)
  , tsClimateChunks = IntMap.singleton 0 (climateChunk chunkSize)
  , tsWeatherChunks = IntMap.singleton 0 (weatherChunk chunkSize)
  , tsVolcanismChunks = IntMap.singleton 0 (volcanismChunk chunkSize)
  , tsGlacierChunks = IntMap.singleton 0 (glacierChunk chunkSize)
  , tsVegetationChunks = IntMap.singleton 0 (vegetationChunk chunkSize)
  }

chunkSize :: Int
chunkSize = 2

groundwaterChunk :: Int -> GroundwaterChunk
groundwaterChunk size =
  let total = size * size
  in GroundwaterChunk
      { gwStorage = U.replicate total 0.25
      , gwRecharge = U.replicate total 0.1
      , gwDischarge = U.replicate total 0.05
      , gwBasinId = U.replicate total 7
      , gwInfiltration = U.replicate total 0.2
      , gwWaterTableDepth = U.replicate total 0.3
      , gwRootZoneMoisture = U.replicate total 0.4
      }

waterBodyChunk :: Int -> WaterBodyChunk
waterBodyChunk size =
  let total = size * size
  in WaterBodyChunk
      { wbType = U.replicate total WaterLake
      , wbSurfaceElev = U.replicate total 0.52
      , wbBasinId = U.replicate total 7
      , wbDepth = U.replicate total 0.1
      , wbAdjacentType = U.replicate total WaterLake
      }

climateChunk :: Int -> ClimateChunk
climateChunk size =
  let total = size * size
  in ClimateChunk
      { ccTempAvg = U.replicate total 0.62
      , ccPrecipAvg = U.replicate total 0.35
      , ccWindDirAvg = U.replicate total 1.1
      , ccWindSpdAvg = U.replicate total 0.24
      , ccHumidityAvg = U.replicate total 0.58
      , ccTempRange = U.replicate total 0.18
      , ccPrecipSeasonality = U.replicate total 0.42
      }

weatherChunk :: Int -> WeatherChunk
weatherChunk size =
  let total = size * size
  in WeatherChunk
      { wcTemp = U.replicate total 0.64
      , wcHumidity = U.replicate total 0.66
      , wcWindDir = U.replicate total 0.7
      , wcWindSpd = U.replicate total 0.22
      , wcPressure = U.replicate total 0.51
      , wcPrecip = U.replicate total 0.27
      , wcCloudCover = U.replicate total 0.73
      , wcCloudWater = U.replicate total 0.31
      , wcCloudCoverLow = U.replicate total 0.2
      , wcCloudCoverMid = U.replicate total 0.3
      , wcCloudCoverHigh = U.replicate total 0.4
      , wcCloudWaterLow = U.replicate total 0.11
      , wcCloudWaterMid = U.replicate total 0.12
      , wcCloudWaterHigh = U.replicate total 0.13
      }

vegetationChunk :: Int -> VegetationChunk
vegetationChunk size =
  let total = size * size
  in VegetationChunk
      { vegCover = U.replicate total 0.67
      , vegAlbedo = U.replicate total 0.12
      , vegDensity = U.replicate total 0.8
      }

glacierChunk :: Int -> GlacierChunk
glacierChunk size =
  let total = size * size
  in GlacierChunk
      { glSnowpack = U.replicate total 0.3
      , glIceThickness = U.replicate total 0.45
      , glMelt = U.replicate total 0.1
      , glFlow = U.replicate total 0.2
      , glErosionPotential = U.replicate total 0.15
      , glDepositPotential = U.replicate total 0.05
      }

volcanismChunk :: Int -> VolcanismChunk
volcanismChunk size =
  let total = size * size
  in VolcanismChunk
      { vcVentType = U.replicate total VentShield
      , vcActivity = U.replicate total VentActive
      , vcMagma = U.replicate total 0.72
      , vcEruptionCount = U.replicate total 3
      , vcEruptedTotal = U.replicate total 0.44
      , vcLavaPotential = U.replicate total 0.31
      , vcAshPotential = U.replicate total 0.21
      , vcDepositPotential = U.replicate total 0.41
      }

emptyTerrainChunk :: Int -> TerrainChunk
emptyTerrainChunk size =
  let total = size * size
      zerosF = U.replicate total 0
      zerosW = U.replicate total 0
  in TerrainChunk
      { tcElevation = zerosF
      , tcDirSlope = U.replicate total zeroDirSlope
      , tcCurvature = zerosF
      , tcHardness = zerosF
      , tcRockType = zerosW
      , tcSoilType = zerosW
      , tcSoilDepth = zerosF
      , tcMoisture = zerosF
      , tcFertility = zerosF
      , tcRoughness = zerosF
      , tcRockDensity = zerosF
      , tcSoilGrain = zerosF
      , tcRelief = zerosF
      , tcRelief2Ring = zerosF
      , tcRelief3Ring = zerosF
      , tcRuggedness = zerosF
      , tcTerrainForm = U.replicate total FormFlat
      , tcFlags = U.replicate total BiomeDesert
      , tcMicroRelief = zerosF
      , tcPlateId = zerosW
      , tcPlateBoundary = U.replicate total PlateBoundaryNone
      , tcPlateHeight = zerosF
      , tcPlateHardness = zerosF
      , tcPlateCrust = zerosW
      , tcPlateAge = zerosF
      , tcPlateVelX = zerosF
      , tcPlateVelY = zerosF
      }

domainTerrainChunk :: Int -> TerrainChunk
domainTerrainChunk size =
  let total = size * size
  in (emptyTerrainChunk size)
      { tcElevation = U.replicate total 0.5
      , tcHardness = U.replicate total 0.35
      , tcRockType = U.replicate total 2
      , tcSoilType = U.replicate total 4
      , tcSoilDepth = U.replicate total 0.6
      , tcMoisture = U.replicate total 0.55
      , tcFertility = U.replicate total 0.48
      , tcRockDensity = U.replicate total 0.37
      , tcSoilGrain = U.replicate total 0.29
      , tcFlags = U.replicate total BiomeTempRainforest
      , tcPlateBoundary = U.replicate total PlateBoundaryConvergent
      }
