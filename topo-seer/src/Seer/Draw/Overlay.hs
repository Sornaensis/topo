{-# LANGUAGE OverloadedStrings #-}

module Seer.Draw.Overlay
  ( TerrainInspectorView(..)
  , TerrainInspectorSection(..)
  , TerrainInspectorField(..)
  , TerrainInspectorPluginData(..)
  , terrainInspectorView
  , terrainInspectorViewAt
  , terrainInspectorViewAtWithPluginData
  , terrainInspectorPinnedView
  , terrainInspectorPanelLinesForHeight
  , terrainInspectorPanelLineHardCap
  , terrainInspectorSectionsObject
  , drawHoverHex
  , drawHexContext
  , drawTooltip
    -- * Hex drawing primitives (reused by brush preview)
  , RectInt(..)
  , hexSpans
  , spanBounds
  , drawHexSpansSupersampled
  , transformRect
  ) where

import Actor.Data (TerrainGeoContext(..), TerrainSnapshot(..))
import Actor.PluginManager.Types (PluginLifecycleSnapshot(..), pluginLifecycleStateText)
import Actor.UI (LayeredViewState(..), UiState(..), ViewMode(..), baseViewModeToViewMode, layeredViewStateToViewMode)
import Actor.UI.State (SourceKind(..), TemporalBasis(..), sourceKindToText, temporalBasisToText)
import Data.Aeson (Value(..), object, toJSON, (.=))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (find, sort)
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8, Word16, Word32, Word64)
import Linear (V2(..), V4(..))
import qualified SDL
import Seer.Config.SliderConversion (sliderToDomainFloat)
import Seer.Config.SliderRegistry (SliderId(..))
import Topo
  ( BiomeId
  , ChunkCoord(..)
  , ChunkId(..)
  , ClimateChunk(..)
  , DirectionalSlope(..)
  , GlacierChunk(..)
  , GroundwaterChunk(..)
  , HexDirection(..)
  , OceanCurrentConfig(..)
  , PlateBoundary(..)
  , RiverChunk(..)
  , TerrainChunk(..)
  , TerrainForm
  , TileCoord(..)
  , TileIndex(..)
  , VegetationChunk(..)
  , VentActivity
  , VentType
  , VolcanismChunk(..)
  , WaterBodyChunk(..)
  , WaterBodyType
  , WeatherChunk(..)
  , WorldConfig(..)
  , biomeDisplayName
  , biomeIdToCode
  , chunkCoordFromTile
  , chunkIdFromCoord
  , dsAvgSlope
  , dsMaxSlope
  , dsMinSlope
  , allBuiltinStageIds
  , oceanCurrentOffset
  , plateBoundaryToCode
  , stageCanonicalName
  , terrainFormDisplayName
  , tileIndex
  , ventActivityToCode
  , ventTypeToCode
  , waterBodyToCode
  )
import Topo.Grid.HexDirection (traceIndexInDirection)
import Topo.Overlay
  ( Overlay(..)
  , OverlayData(..)
  , OverlayChunk(..)
  , OverlayProvenance(..)
  , OverlayRecord(..)
  , OverlayStore(..)
  , OverlayValue(..)
  , chunkLookup
  , defaultValue
  , floatToOverlayValue
  , lookupOverlay
  , recordField
  )
import Topo.Overlay.Schema
  ( OverlayDeps(..)
  , OverlayFieldDef(..)
  , OverlayFieldType(..)
  , OverlaySchema(..)
  , OverlayStorage(..)
  , overlayFieldTypeName
  )
import Topo.Plugin.DataResource
  ( DataFieldDef(..)
  , DataOperations(..)
  , DataPagination(..)
  , DataResourceSchema(..)
  )
import Topo.Plugin.RPC.DataService
  ( QueryResult(..)
  )
import Topo.Hex (HexGridMeta(..))
import Topo.Planet (PlanetConfig(..), WorldSlice(..), formatLatLon, tileLatLon)
import Topo.Calendar (CalendarConfig(..), WorldTime(..), mkCalendarConfig, tickToDate, yearFraction, CalendarDate(..))
import Topo.Solar (SolarPosition(..), DayInfo(..), tileSolarPos, tileDayInfo, defaultSolarConfig, tileIrradiance, localSolarHour)
import Topo.Weather (WeatherNormalsChunk(..), getWeatherNormalsChunkFromStore)
import Topo.Units
  ( defaultUnitScales
  , normSlopeToDeg
  , normToC
  , normToHPa
  , normDepthToMetres
  , normToMetres
  , normToMmYear
  , normToRH
  , normToSoilM
  , normToWindMs
  )
import UI.Font (FontCache, textSize)
import UI.HexGeometry (hexFillRectAt, hexSpanTextureSize, renderHexRadiusPx, transformWorldRect)
import UI.Theme
import UI.Widgets (Rect(..))
import UI.WidgetsDraw (drawTextLine)
import qualified Data.Vector.Unboxed as U

-- | Pure view model for the hover-hex terrain inspector.
data TerrainInspectorView = TerrainInspectorView
  { tivHex :: !(Int, Int)
  , tivSections :: ![TerrainInspectorSection]
  , tivLines :: ![Text]
    -- ^ Full diagnostic line dump for tests/debugging. The pinned panel uses
    -- 'tivPanelLines' so the canonical structured sections can stay complete.
  , tivPanelLines :: ![Text]
    -- ^ Compact, view-mode-aware, bounded line projection for the pinned panel.
  } deriving (Eq, Show)

-- | Canonical structured inspector section.
data TerrainInspectorSection = TerrainInspectorSection
  { tisKey :: !Text
  , tisTitle :: !Text
  , tisFields :: ![TerrainInspectorField]
  } deriving (Eq, Show)

-- | One display/API field in a terrain-inspector section.
data TerrainInspectorField = TerrainInspectorField
  { tifKey :: !Text
  , tifLabel :: !Text
  , tifValue :: !Text
  , tifRaw :: !Value
  } deriving (Eq, Show)

-- | Plugin data-resource query results to splice into the otherwise pure
-- terrain inspector. UI callers normally provide schema-only entries from
-- 'UiState'; service/API callers can attach per-hex query results.
data TerrainInspectorPluginData = TerrainInspectorPluginData
  { tipdPlugin :: !Text
  , tipdSchema :: !DataResourceSchema
  , tipdResult :: !(Maybe (Either Text QueryResult))
  } deriving (Eq, Show)

terrainInspectorView :: UiState -> TerrainSnapshot -> Maybe TerrainInspectorView
terrainInspectorView ui terrainSnap = do
  hexCoord <- uiHoverHex ui
  pure (terrainInspectorViewAt ui terrainSnap hexCoord)

terrainInspectorPinnedView :: UiState -> TerrainSnapshot -> Maybe TerrainInspectorView
terrainInspectorPinnedView ui terrainSnap = do
  hexCoord <- case uiContextHex ui of
    Just selectedHex -> Just selectedHex
    Nothing -> uiHoverHex ui
  pure (terrainInspectorViewAt ui terrainSnap hexCoord)

terrainInspectorViewAt :: UiState -> TerrainSnapshot -> (Int, Int) -> TerrainInspectorView
terrainInspectorViewAt ui =
  terrainInspectorViewAtWithPluginData (inspectorPluginDataFromUi ui) ui

terrainInspectorViewAtWithPluginData :: [TerrainInspectorPluginData] -> UiState -> TerrainSnapshot -> (Int, Int) -> TerrainInspectorView
terrainInspectorViewAtWithPluginData pluginData ui terrainSnap hexCoord@(q, r) =
  case sampleAt terrainSnap hexCoord of
    Nothing -> TerrainInspectorView
      { tivHex = hexCoord
      , tivSections = []
      , tivLines = [hexHeader q r, "No data"]
      , tivPanelLines = [hexHeader q r, "No data"]
      }
    Just sample ->
      let sections = inspectorSections pluginData ui terrainSnap hexCoord sample
          lines = hexHeader q r : latLonLine ui terrainSnap q r : (sectionsToLines sections <> modeContextLines ui terrainSnap hexCoord sample)
          panelLines = terrainInspectorPanelLines ui terrainSnap hexCoord sample sections
      in TerrainInspectorView
        { tivHex = hexCoord
        , tivSections = sections
        , tivLines = lines
        , tivPanelLines = panelLines
        }

terrainInspectorSectionsObject :: TerrainInspectorView -> Value
terrainInspectorSectionsObject view = toJSON (map sectionObject (tivSections view))
  where
    sectionObject section = object
      [ "key" .= tisKey section
      , "title" .= tisTitle section
      , "fields" .= map fieldObject (tisFields section)
      ]
    fieldObject field = object
      [ "key" .= tifKey field
      , "label" .= tifLabel field
      , "value" .= tifValue field
      , "raw" .= tifRaw field
      ]

drawHoverHex :: SDL.Renderer -> UiState -> Int -> IO ()
drawHoverHex renderer uiSnap hexRadius =
  case uiHoverHex uiSnap of
    Nothing -> pure ()
    Just (q, r) -> do
      let Rect (V2 worldX worldY, V2 worldW worldH) = hexFillRectAt renderHexRadiusPx q r
          (ox, oy) = uiPanOffset uiSnap
          z = uiZoom uiSnap
          -- Texture rendered at hexRadius resolution for crisp edges
          hiResSpans = hexSpans hexRadius
          (tMinX, tMinY, _, _) = spanBounds hiResSpans
          (texW, texH) = hexSpanTextureSize hiResSpans
      hoverTexture <- SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessTarget (V2 (fromIntegral texW) (fromIntegral texH))
      SDL.textureBlendMode hoverTexture SDL.$= SDL.BlendAlphaBlend
      SDL.rendererRenderTarget renderer SDL.$= Just hoverTexture
      SDL.rendererDrawBlendMode renderer SDL.$= SDL.BlendAlphaBlend
      SDL.rendererDrawColor renderer SDL.$= V4 0 0 0 0
      SDL.clear renderer
      SDL.rendererDrawColor renderer SDL.$= colHoverHex
      drawHexSpansSupersampled renderer hiResSpans 1 (tMinX, tMinY)
      SDL.rendererRenderTarget renderer SDL.$= Nothing
      let rect = transformRect (ox, oy) z (RectInt (V2 worldX worldY) (V2 worldW worldH))
          RectInt (V2 tx ty) (V2 tw th) = rect
      SDL.copy renderer hoverTexture Nothing (Just (SDL.Rectangle (SDL.P (V2 (fromIntegral tx) (fromIntegral ty))) (V2 (fromIntegral tw) (fromIntegral th))))
      SDL.destroyTexture hoverTexture

drawHexContext :: SDL.Renderer -> Maybe FontCache -> UiState -> TerrainSnapshot -> V2 Int -> IO ()
drawHexContext renderer fontCache ui terrainSnap (V2 winW winH) =
  if not (uiHexTooltipPinned ui)
    then pure ()
    else case terrainInspectorPinnedView ui terrainSnap of
      Just inspector -> do
        SDL.P (V2 mx my) <- SDL.getAbsoluteMouseLocation
        let sx = fromIntegral mx
            sy = fromIntegral my
            panelAvailableH = max panelLineHeightPx (winH - 16)
            linesToDraw = terrainInspectorPanelLinesForHeight panelAvailableH inspector
            lineHeight = panelLineHeightPx
            hPad = 12
            vPad = panelVerticalPaddingPx
        panelW <- case fontCache of
          Just fc -> do
            widths <- mapM (lineWidth fc) linesToDraw
            pure (maximum (220 : map (+ hPad * 2) widths))
          Nothing -> pure (maximum (220 : map (\line -> Text.length line * 8 + hPad * 2) linesToDraw))
        let panelH = min panelAvailableH (max 40 (vPad * 2 + lineHeight * length linesToDraw))
            px = clamp 8 (winW - panelW - 8) (sx + 12)
            py = clamp 8 (winH - panelH - 8) (sy + 12)
            panelRect = SDL.Rectangle (SDL.P (V2 (fromIntegral px) (fromIntegral py))) (V2 (fromIntegral panelW) (fromIntegral panelH))
        SDL.rendererDrawColor renderer SDL.$= colHexContextBg
        SDL.fillRect renderer (Just panelRect)
        case fontCache of
          Nothing -> pure ()
          Just _ ->
            sequence_
              [ drawTextLine fontCache (V2 (px + hPad) (py + vPad + idx * lineHeight)) textHexContext lineText
              | (idx, lineText) <- zip [0 ..] linesToDraw
              ]
      Nothing -> pure ()
  where
    clamp lo hi value = max lo (min hi value)
    lineWidth fc lineText = do
      V2 width _ <- textSize fc textHexContext lineText
      pure width

drawTooltip :: SDL.Renderer -> Maybe FontCache -> V2 Int -> V2 Int -> Text -> IO ()
drawTooltip renderer fontCache (V2 winW winH) (V2 mx my) tipText = do
  let tipPad = 6
      tipOffsetY = 20
  V2 tw th <- case fontCache of
    Just fc -> textSize fc textTooltip tipText
    Nothing -> pure (V2 (Text.length tipText * 8) 16)
  let boxW = tw + tipPad * 2
      boxH = th + tipPad * 2
      rawX = mx + 8
      rawY = my + tipOffsetY
      x = max 0 (min (winW - boxW) rawX)
      y = max 0 (min (winH - boxH) rawY)
      bgRect = SDL.Rectangle (SDL.P (V2 (fromIntegral x) (fromIntegral y)))
                 (V2 (fromIntegral boxW) (fromIntegral boxH))
  SDL.rendererDrawColor renderer SDL.$= colTooltipBg
  SDL.fillRect renderer (Just bgRect)
  SDL.rendererDrawColor renderer SDL.$= colTooltipBorder
  SDL.drawRect renderer (Just bgRect)
  drawTextLine fontCache (V2 (x + tipPad) (y + tipPad)) textTooltip tipText

data RectInt = RectInt !(V2 Int) !(V2 Int)

transformRect :: (Float, Float) -> Float -> RectInt -> RectInt
transformRect pan z (RectInt pos size) =
  let Rect (screenPos, screenSize) = transformWorldRect pan z (Rect (pos, size))
  in RectInt screenPos screenSize

drawHexSpansSupersampled :: SDL.Renderer -> [(Int, Int, Int)] -> Int -> (Int, Int) -> IO ()
drawHexSpansSupersampled renderer spans scale (minX, minY) =
  mapM_ (drawSpanSupersampled renderer scale minX minY) spans

drawSpanSupersampled :: SDL.Renderer -> Int -> Int -> Int -> (Int, Int, Int) -> IO ()
drawSpanSupersampled renderer scale minX minY (dy, x0, x1) = do
  let y = (dy - minY) * scale
      x = (x0 - minX) * scale
      w = max 1 ((x1 - x0) * scale)
      h = max 1 scale
  SDL.fillRect renderer (Just (SDL.Rectangle (SDL.P (V2 (fromIntegral x) (fromIntegral y))) (V2 (fromIntegral w) (fromIntegral h))))

spanBounds :: [(Int, Int, Int)] -> (Int, Int, Int, Int)
spanBounds spans =
  let ys = map (\(y, _, _) -> y) spans
      xs0 = map (\(_, x0, _) -> x0) spans
      xs1 = map (\(_, _, x1) -> x1) spans
  in (minimum xs0, minimum ys, maximum xs1, maximum ys)

hexSpans :: Int -> [(Int, Int, Int)]
hexSpans size =
  let corners = hexCorners size
      ys = [floor (minimum (map snd corners)) .. ceiling (maximum (map snd corners))]
  in mapMaybe (spanForY corners) ys

hexCorners :: Int -> [(Float, Float)]
hexCorners size =
  let s = fromIntegral size
      angles = [-30, 30, 90, 150, 210, 270]
  in [(s * cos (degToRad angle), s * sin (degToRad angle)) | angle <- angles]

spanForY :: [(Float, Float)] -> Int -> Maybe (Int, Int, Int)
spanForY corners y =
  let yF = fromIntegral y + 0.5
      edges = case corners of
        [] -> []
        (corner : rest) -> zip (corner : rest) (rest ++ [corner])
      xs = mapMaybe (edgeIntersect yF) edges
  in case sort xs of
       x0 : x1 : _ -> Just (y, floor x0, ceiling x1)
       _ -> Nothing

edgeIntersect :: Float -> ((Float, Float), (Float, Float)) -> Maybe Float
edgeIntersect y ((x1, y1), (x2, y2))
  | y1 == y2 = Nothing
  | y >= min y1 y2 && y < max y1 y2 =
      let t = (y - y1) / (y2 - y1)
      in Just (x1 + t * (x2 - x1))
  | otherwise = Nothing

degToRad :: Float -> Float
degToRad degrees = degrees * pi / 180

contextLines :: UiState -> TerrainSnapshot -> (Int, Int) -> [Text]
contextLines ui terrainSnap hexCoord = tivPanelLines (terrainInspectorViewAt ui terrainSnap hexCoord)

latLonLine :: UiState -> TerrainSnapshot -> Int -> Int -> Text
latLonLine ui terrainSnap tileQ tileR =
  let (lat, lon) = latLonValues ui terrainSnap tileQ tileR
  in formatLatLon lat lon

latLonValues :: UiState -> TerrainSnapshot -> Int -> Int -> (Float, Float)
latLonValues _ terrainSnap tileQ tileR =
  tileLatLon planet hex slice worldConfig (TileCoord tileQ tileR)
  where
    (planet, slice, hex, worldConfig) = geoContext terrainSnap

geoContext :: TerrainSnapshot -> (PlanetConfig, WorldSlice, HexGridMeta, WorldConfig)
geoContext terrainSnap =
  let geo = tsGeoContext terrainSnap
      worldConfig = WorldConfig { wcChunkSize = tsChunkSize terrainSnap }
  in (tgcPlanet geo, tgcSlice geo, tgcHexGrid geo, worldConfig)

sectionsToLines :: [TerrainInspectorSection] -> [Text]
sectionsToLines = concatMap sectionLines
  where
    sectionLines section = ("--- " <> tisTitle section <> " ---") : map fieldLine (tisFields section)
    fieldLine field = tifLabel field <> " " <> tifValue field

terrainInspectorPanelLineHardCap :: Int
terrainInspectorPanelLineHardCap = 14

panelLineTextLimit :: Int
panelLineTextLimit = 96

panelLineHeightPx :: Int
panelLineHeightPx = 16

panelVerticalPaddingPx :: Int
panelVerticalPaddingPx = 10

data InspectorPanelLine = InspectorPanelLine
  { iplText :: !Text
  , iplFieldCount :: !Int
  }

terrainInspectorPanelLinesForHeight :: Int -> TerrainInspectorView -> [Text]
terrainInspectorPanelLinesForHeight availableHeight view =
  clipPanelLineTexts lineCap (tivPanelLines view)
  where
    lineCap = clampInt 2 terrainInspectorPanelLineHardCap ((availableHeight - panelVerticalPaddingPx * 2) `div` panelLineHeightPx)

terrainInspectorPanelLines :: UiState -> TerrainSnapshot -> (Int, Int) -> HexSample -> [TerrainInspectorSection] -> [Text]
terrainInspectorPanelLines ui terrainSnap hexCoord sample sections =
  renderPanelLines terrainInspectorPanelLineHardCap totalFields coreLines
  where
    totalFields = sum (map (length . tisFields) sections)
    coreLines =
      commonInspectorPanelLines ui terrainSnap hexCoord sections
        <> modeInspectorPanelLines ui terrainSnap hexCoord sample sections
        <> notableInspectorPanelLines sections
        <> availabilityInspectorPanelLines sections

commonInspectorPanelLines :: UiState -> TerrainSnapshot -> (Int, Int) -> [TerrainInspectorSection] -> [InspectorPanelLine]
commonInspectorPanelLines ui terrainSnap (q, r) sections =
  [ staticPanelLine (hexHeader q r)
  , staticPanelLine (latLonLine ui terrainSnap q r)
  ]
  <> maybe [] pure (chunkPanelLine sections)
  <> fieldPanelLines sections "elevation_hypsometry"
       [ "elevation_m"
       , "relative_water_level_m"
       , "hypsometric_zone"
       ]

modeInspectorPanelLines :: UiState -> TerrainSnapshot -> (Int, Int) -> HexSample -> [TerrainInspectorSection] -> [InspectorPanelLine]
modeInspectorPanelLines ui terrainSnap hexCoord sample sections =
  case projectedViewMode ui of
    ViewElevation -> fields "erosion_terrain_form"
      [ "terrain_form", "slope_avg_deg", "slope_max_deg", "slope_min_deg" ]
    ViewTerrainForm -> fields "erosion_terrain_form"
      [ "terrain_form", "slope_avg_deg", "roughness", "relief" ]
    ViewBiome -> fields "biome_refinement"
      [ "biome", "family", "terrain_form", "precip_avg_mm_year" ]
    ViewClimate -> fields "climate_weather"
      [ "temp_avg_c", "precip_avg_mm_year", "humidity_avg_pct", "wind_spd_avg_ms" ]
    ViewPrecip -> fields "climate_weather"
      [ "precip_avg_mm_year", "humidity_avg_pct", "temp_avg_c", "precip_seasonality" ]
    ViewPrecipCurrent -> fields "weather_snapshot"
      [ "precip_mm_year", "humidity_pct", "temp_c", "pressure_hpa" ]
    ViewWeather -> fields "weather_snapshot"
      [ "temp_c", "humidity_pct", "wind_spd_ms", "pressure_hpa", "precip_mm_year" ]
    ViewCloud -> fields "weather_snapshot"
      [ "cloud_cover", "cloud_water"
      , "cloud_cover_low", "cloud_cover_mid", "cloud_cover_high"
      , "cloud_water_low", "cloud_water_mid", "cloud_water_high"
      ]
    ViewCloudTypical -> fields "weather_normals"
      [ "normal_cloud_cover", "normal_cloud_water"
      , "normal_cloud_cover_low", "normal_cloud_cover_mid", "normal_cloud_cover_high"
      , "normal_cloud_water_low", "normal_cloud_water_mid", "normal_cloud_water_high"
      ]
    ViewMoisture -> fields "soil"
      [ "soil_moisture", "soil_depth_m", "fertility" ]
      <> fields "water_table"
      [ "water_table_depth", "root_zone_moisture" ]
    ViewVegetation -> fields "vegetation"
      [ "cover", "density", "albedo", "fertility" ]
    ViewPlateId -> fields "tectonics_plates"
      [ "plate_id", "boundary", "crust" ]
    ViewPlateBoundary -> fields "tectonics_plates"
      [ "boundary", "plate_id", "velocity_speed" ]
    ViewPlateHardness -> fields "tectonics_plates"
      [ "plate_hardness", "plate_id", "crust" ]
    ViewPlateCrust -> fields "tectonics_plates"
      [ "crust", "plate_height_m", "plate_id" ]
    ViewPlateAge -> fields "tectonics_plates"
      [ "plate_age", "plate_id", "boundary" ]
    ViewPlateHeight -> fields "tectonics_plates"
      [ "plate_height_m", "plate_id", "crust" ]
    ViewPlateVelocity -> fields "tectonics_plates"
      [ "velocity_x", "velocity_y", "velocity_speed", "boundary" ]
    ViewOverlay overlayName fieldIndex ->
      overlayInspectorPanelLines ui terrainSnap hexCoord sample sections overlayName fieldIndex
  where
    fields sectionKey fieldKeys = fieldPanelLinesWithStatus sections sectionKey fieldKeys

notableInspectorPanelLines :: [TerrainInspectorSection] -> [InspectorPanelLine]
notableInspectorPanelLines sections = take 2 $ mapMaybe id
  [ waterBodyPanelLine sections
  , glacierPanelLine sections
  , volcanismPanelLine sections
  , oceanCurrentPanelLine sections
  ]

availabilityInspectorPanelLines :: [TerrainInspectorSection] -> [InspectorPanelLine]
availabilityInspectorPanelLines sections =
  [ staticPanelLine $ "Full data " <> showText (length sections) <> " sections"
      <> " · overlays " <> sectionAvailabilityCount "overlay_records" sections
      <> " · plugins " <> sectionAvailabilityCount "plugin_hex_data" sections
  ]

renderPanelLines :: Int -> Int -> [InspectorPanelLine] -> [Text]
renderPanelLines cap totalFields coreLines
  | cap <= 0 = []
  | otherwise = map (truncatePanelText . iplText) visibleLines <> [truncatePanelText footer]
  where
    visibleLines = take (max 0 (cap - 1)) coreLines
    shownFields = sum (map iplFieldCount visibleLines)
    hiddenFields = max 0 (totalFields - shownFields)
    footer = "… " <> showText hiddenFields <> " hidden fields · Full/API: get_hex.sections · Export/provenance JSON"

clipPanelLineTexts :: Int -> [Text] -> [Text]
clipPanelLineTexts cap linesToClip
  | cap <= 0 = []
  | length linesToClip <= cap = linesToClip
  | cap == 1 = [truncatePanelText (heightFooter (length linesToClip))]
  | otherwise = take (cap - 1) linesToClip <> [truncatePanelText (heightFooter hiddenLines)]
  where
    hiddenLines = length linesToClip - cap + 1
    heightFooter count = "… " <> showText count <> " more lines · get_hex.sections · Export/provenance JSON"

overlayInspectorPanelLines :: UiState -> TerrainSnapshot -> (Int, Int) -> HexSample -> [TerrainInspectorSection] -> Text -> Int -> [InspectorPanelLine]
overlayInspectorPanelLines ui terrainSnap hexCoord sample sections overlayName fieldIndex =
  map staticPanelLine selectedModeLines
    <> fieldPanelLineOrStatus sections "overlay_records" ("overlay_" <> sanitizeKey overlayName)
    <> fieldPanelLineOrStatus sections "overlay_schema" ("schema_" <> sanitizeKey overlayName)
    <> fieldPanelLineOrStatus sections "overlay_provenance" ("provenance_" <> sanitizeKey overlayName)
  where
    safeFieldIndex = max 0 fieldIndex
    modeLines = modeContextLines ui terrainSnap hexCoord sample
    selectedModeLines = case modeLines of
      header : fieldHeader : valueLines -> [header, fieldHeader] <> take 1 (drop safeFieldIndex valueLines)
      otherLines -> otherLines

fieldPanelLines :: [TerrainInspectorSection] -> Text -> [Text] -> [InspectorPanelLine]
fieldPanelLines sections sectionKey fieldKeys =
  mapMaybe (fieldPanelLine sections sectionKey) fieldKeys

fieldPanelLinesWithStatus :: [TerrainInspectorSection] -> Text -> [Text] -> [InspectorPanelLine]
fieldPanelLinesWithStatus sections sectionKey fieldKeys =
  case fieldPanelLines sections sectionKey fieldKeys of
    [] -> maybe [] pure (fieldPanelLine sections sectionKey "status")
    selectedLines -> selectedLines

fieldPanelLineOrStatus :: [TerrainInspectorSection] -> Text -> Text -> [InspectorPanelLine]
fieldPanelLineOrStatus sections sectionKey fieldKey =
  case fieldPanelLine sections sectionKey fieldKey of
    Just selectedLine -> [selectedLine]
    Nothing -> maybe [] pure (fieldPanelLine sections sectionKey "status")

fieldPanelLine :: [TerrainInspectorSection] -> Text -> Text -> Maybe InspectorPanelLine
fieldPanelLine sections sectionKey fieldKey = do
  field <- findInspectorField sections sectionKey fieldKey
  pure (InspectorPanelLine (fieldDisplayLine field) 1)

chunkPanelLine :: [TerrainInspectorSection] -> Maybe InspectorPanelLine
chunkPanelLine sections = do
  chunk <- fieldValue "coordinates" "chunk_id"
  tile <- fieldValue "coordinates" "tile_index"
  size <- fieldValue "coordinates" "chunk_size"
  pure $ InspectorPanelLine ("Chunk " <> chunk <> " · Tile " <> tile <> " · Size " <> size) 3
  where
    fieldValue sectionKey fieldKey = tifValue <$> findInspectorField sections sectionKey fieldKey

waterBodyPanelLine :: [TerrainInspectorSection] -> Maybe InspectorPanelLine
waterBodyPanelLine sections = do
  field <- findInspectorField sections "water_bodies" "type"
  if tifValue field `elem` ["-", "Dry"]
    then Nothing
    else Just (InspectorPanelLine ("Water " <> tifValue field) 1)

glacierPanelLine :: [TerrainInspectorSection] -> Maybe InspectorPanelLine
glacierPanelLine sections = do
  field <- findInspectorField sections "glacier_snow_ice" "ice_thickness"
  if ignorableValue (tifValue field)
    then Nothing
    else Just (InspectorPanelLine ("Ice " <> tifValue field) 1)

volcanismPanelLine :: [TerrainInspectorSection] -> Maybe InspectorPanelLine
volcanismPanelLine sections = do
  field <- findInspectorField sections "volcanism" "vent_type"
  if tifValue field `elem` ["-", "None"]
    then Nothing
    else Just (InspectorPanelLine ("Volcanism " <> tifValue field) 1)

oceanCurrentPanelLine :: [TerrainInspectorSection] -> Maybe InspectorPanelLine
oceanCurrentPanelLine sections = do
  water <- findInspectorField sections "ocean_currents" "water_tile"
  tempDelta <- findInspectorField sections "ocean_currents" "temp_offset_c"
  if tifValue water == "yes"
    then Just (InspectorPanelLine ("Current Δ " <> tifValue tempDelta) 2)
    else Nothing

findInspectorSection :: [TerrainInspectorSection] -> Text -> Maybe TerrainInspectorSection
findInspectorSection sections sectionKey = find ((== sectionKey) . tisKey) sections

findInspectorField :: [TerrainInspectorSection] -> Text -> Text -> Maybe TerrainInspectorField
findInspectorField sections sectionKey fieldKey = do
  section <- findInspectorSection sections sectionKey
  find ((== fieldKey) . tifKey) (tisFields section)

sectionAvailabilityCount :: Text -> [TerrainInspectorSection] -> Text
sectionAvailabilityCount sectionKey sections =
  case findInspectorSection sections sectionKey of
    Nothing -> "0"
    Just section
      | any unavailableStatus (tisFields section) -> "0"
      | otherwise -> showText (length (tisFields section))

unavailableStatus :: TerrainInspectorField -> Bool
unavailableStatus field =
  tifKey field == "status" && tifValue field `elem` ["none", "not_loaded", "no_hex_bound_resources"]

ignorableValue :: Text -> Bool
ignorableValue value = value `elem` ["-", "0.0", "0.0 m", "0.0 °C", "no", "None", "Dormant", "Dry"]

fieldDisplayLine :: TerrainInspectorField -> Text
fieldDisplayLine field = tifLabel field <> " " <> tifValue field

staticPanelLine :: Text -> InspectorPanelLine
staticPanelLine text = InspectorPanelLine text 0

truncatePanelText :: Text -> Text
truncatePanelText text
  | Text.length text <= panelLineTextLimit = text
  | otherwise = Text.take (panelLineTextLimit - 1) text <> "…"

clampInt :: Int -> Int -> Int -> Int
clampInt lo hi value = max lo (min hi value)

projectedViewMode :: UiState -> ViewMode
projectedViewMode ui =
  case layeredViewStateToViewMode (uiViewSelection ui) of
    Just mode -> mode
    Nothing -> baseViewModeToViewMode (lvsBaseView (uiViewSelection ui))

modeContextLines :: UiState -> TerrainSnapshot -> (Int, Int) -> HexSample -> [Text]
modeContextLines ui terrainSnap (q, r) sample = modeLines (projectedViewMode ui) sample
  where
    units = defaultUnitScales
    solarLines = solarLinesFor ui terrainSnap

    modeLines ViewElevation sample' =
      let dirSlope = hsDirSlope sample'
          slopeDeg = normSlopeToDeg units
      in [ "Elev  " <> fmtU (normToMetres units (hsElevation sample')) "m"
         , "Form  " <> Text.pack (terrainFormDisplayName (hsTerrainForm sample'))
         , "Slope " <> fmtU (slopeDeg (hsSlope sample')) "° avg"
         , "      " <> fmtU (slopeDeg (dsMaxSlope dirSlope)) "° max  " <> fmtU (slopeDeg (dsMinSlope dirSlope)) "° min"
         , "  E   " <> fmtU (slopeDeg (dsSlopeE dirSlope)) "°" <> "   W  " <> fmtU (slopeDeg (dsSlopeW dirSlope)) "°"
         , "  NE  " <> fmtU (slopeDeg (dsSlopeNE dirSlope)) "°" <> "   SW " <> fmtU (slopeDeg (dsSlopeSW dirSlope)) "°"
         , "  NW  " <> fmtU (slopeDeg (dsSlopeNW dirSlope)) "°" <> "   SE " <> fmtU (slopeDeg (dsSlopeSE dirSlope)) "°"
         ] ++ solarLines q r
    modeLines ViewBiome sample' =
      [ "Biome  " <> biomeDisplayName (hsBiome sample')
      , "Form   " <> Text.pack (terrainFormDisplayName (hsTerrainForm sample'))
      , "Elev   " <> fmtU (normToMetres units (hsElevation sample')) "m"
      , "Slope  " <> fmtU (normSlopeToDeg units (hsSlope sample')) "°"
      , "Precip " <> fmtU (normToMmYear units (hsPrecipAvg sample')) "mm/yr"
      , "Avg climate humidity " <> fmtU (normToRH (hsHumidity sample')) "% RH"
      , "Fert   " <> fmtF (hsFertility sample')
      , "Veg    " <> fmtF (hsVegCover sample')
      , "VDen   " <> fmtF (hsVegDensity sample')
      ] ++ solarLines q r
    modeLines ViewClimate sample' =
      [ "Average climate temperature " <> fmtU (normToC units (hsTemp sample')) "°C"
      , "Average climate precipitation " <> fmtU (normToMmYear units (hsPrecipAvg sample')) "mm/yr"
      ] ++ solarLines q r
    modeLines ViewWeather sample' =
      [ "Biome " <> biomeDisplayName (hsBiome sample')
      , "Elev  " <> fmtU (normToMetres units (hsElevation sample')) "m"
      , "Slope " <> fmtU (normSlopeToDeg units (hsSlope sample')) "° avg"
      , "Current weather temp " <> fmtU (normToC units (hsWeatherTemp sample')) "°C"
      , "Current weather humid " <> fmtU (normToRH (hsWeatherHumidity sample')) "% RH"
      , "Current weather windD " <> fmtU (hsWeatherWindDir sample') "rad"
      , "Current weather windS " <> fmtU (normToWindMs units (hsWeatherWindSpd sample')) "m/s"
      , "Current weather press " <> fmtU (normToHPa units (hsWeatherPressure sample')) "hPa"
      , "Current weather precip " <> fmtU (normToMmYear units (hsWeatherPrecip sample')) "mm/yr"
      ] ++ solarLines q r
    modeLines ViewMoisture sample' =
      [ "Moist " <> fmtU (normToRH (hsMoisture sample')) "%"
      , "Soil  " <> fmtU (normToSoilM units (hsSoilDepth sample')) "m"
      ]
    modeLines ViewPrecip sample' =
      [ "Average climate precipitation " <> fmtU (normToMmYear units (hsPrecipAvg sample')) "mm/yr"
      , "Average climate humidity " <> fmtU (normToRH (hsHumidity sample')) "% RH"
      ]
    modeLines ViewPrecipCurrent sample' =
      [ "Current weather precipitation " <> fmtU (normToMmYear units (hsWeatherPrecip sample')) "mm/yr"
      , "Current weather humidity " <> fmtU (normToRH (hsWeatherHumidity sample')) "% RH"
      , "Current weather temp " <> fmtU (normToC units (hsWeatherTemp sample')) "°C"
      ] ++ solarLines q r
    modeLines ViewPlateId sample' = ["Plate " <> Text.pack (show (hsPlateId sample'))]
    modeLines ViewPlateBoundary sample' = ["Boundary " <> plateBoundaryDisplayName (hsPlateBoundary sample')]
    modeLines ViewPlateHardness sample' = ["Hardness " <> fmtF (hsPlateHardness sample')]
    modeLines ViewPlateCrust sample' = ["Crust " <> crustDisplayName (hsPlateCrust sample')]
    modeLines ViewPlateAge sample' = ["Age   " <> fmtF (hsPlateAge sample')]
    modeLines ViewPlateHeight sample' = ["Height " <> fmtU (normToMetres units (hsPlateHeight sample')) "m"]
    modeLines ViewPlateVelocity sample' =
      [ "Vel X " <> fmtF (hsPlateVelX sample')
      , "Vel Y " <> fmtF (hsPlateVelY sample')
      , "Speed " <> fmtF (sqrt (hsPlateVelX sample' ** 2 + hsPlateVelY sample' ** 2))
      ]
    modeLines ViewVegetation sample' =
      [ "VCov  " <> fmtF (hsVegCover sample')
      , "VDen  " <> fmtF (hsVegDensity sample')
      , "Fert  " <> fmtF (hsFertility sample')
      , "Moist " <> fmtU (normToRH (hsMoisture sample')) "%"
      ]
    modeLines ViewTerrainForm sample' =
      [ "Form  " <> Text.pack (terrainFormDisplayName (hsTerrainForm sample'))
      , "Slope " <> fmtF (hsSlope sample')
      , "Elev  " <> fmtU (normToMetres units (hsElevation sample')) "m"
      ]
    modeLines ViewCloud sample' =
      let pct v = fmtU (v * 100) "%"
          stormI = hsCloudWater sample' * min 1 (hsWeatherPrecip sample' * 3)
      in [ "Current simulated clouds/storm aggregate render"
         , "Current cloud " <> pct (hsCloudCover sample') <> "  Water " <> fmtF (hsCloudWater sample')
         , "  Layer fields: context only"
         , "  Low  " <> pct (hsCloudCoverLow sample') <> "  " <> fmtF (hsCloudWaterLow sample')
         , "  Mid  " <> pct (hsCloudCoverMid sample') <> "  " <> fmtF (hsCloudWaterMid sample')
         , "  High " <> pct (hsCloudCoverHigh sample') <> "  " <> fmtF (hsCloudWaterHigh sample')
         , "Precp " <> fmtU (normToMmYear units (hsWeatherPrecip sample')) "mm/yr"
         , "WindD " <> fmtU (hsWeatherWindDir sample') "rad"
         , "WindS " <> fmtU (normToWindMs units (hsWeatherWindSpd sample')) "m/s"
         , "Storm " <> fmtF stormI
         ] ++ solarLines q r
    modeLines ViewCloudTypical sample' =
      case hsWeatherNormalsChunk sample' of
        Nothing ->
          [ "Typical weather normals unavailable"
          , "No weather_normals overlay is loaded; not using current clouds as a fallback"
          ]
        Just wn ->
          let idx = hsTileIndex sample'
              value accessor = maybe 0 id (safeIndexMaybe (accessor wn) idx)
              pct v = fmtU (v * 100) "%"
              cover = value wncCloudCover
              water = value wncCloudWater
              precip = value wncPrecip
              stormI = water * min 1 (precip * 3)
          in [ "Typical generated clouds/storm normal"
             , "Typical cloud " <> pct cover <> "  Water " <> fmtF water
             , "  Layer fields: context only"
             , "  Low  " <> pct (value wncCloudCoverLow) <> "  " <> fmtF (value wncCloudWaterLow)
             , "  Mid  " <> pct (value wncCloudCoverMid) <> "  " <> fmtF (value wncCloudWaterMid)
             , "  High " <> pct (value wncCloudCoverHigh) <> "  " <> fmtF (value wncCloudWaterHigh)
             , "Typical precip " <> fmtU (normToMmYear units precip) "mm/yr"
             , "Storm " <> fmtF stormI
             ] ++ solarLines q r
    modeLines (ViewOverlay overlayName fieldIndex) sample' =
      case lookupOverlay overlayName (tsOverlayStore terrainSnap) of
        Nothing -> ["Overlay " <> overlayName, "(not loaded)"]
        Just overlay ->
          let schema = ovSchema overlay
              fields = osFields schema
              fieldHeader = case drop fieldIndex fields of
                (fd:_) -> ofdName fd
                []     -> "field " <> Text.pack (show fieldIndex)
              ChunkId key = chunkIdFromCoord (hsChunk sample')
          in ("Overlay " <> overlayName) : ("Field  " <> fieldHeader) :
               overlayValuesAt overlay key (tsChunkSize terrainSnap) sample' fields

solarLinesFor :: UiState -> TerrainSnapshot -> Int -> Int -> [Text]
solarLinesFor _ terrainSnap tileQ tileR =
  let (planet, slice, hex, worldConfig) = geoContext terrainSnap
      worldTime = tgcWorldTime (tsGeoContext terrainSnap)
      calCfg = mkCalendarConfig planet
      calDate = tickToDate calCfg worldTime
      yf      = realToFrac (yearFraction calCfg worldTime) :: Float
      hpd     = realToFrac (ccHoursPerDay calCfg) :: Float
      calHour = realToFrac (cdHourOfDay calDate) :: Float
      tiltDeg = pcAxialTilt planet
      tile    = TileCoord tileQ tileR
      (latDeg, lonDeg) = tileLatLon planet hex slice worldConfig tile
      latRad  = latDeg * pi / 180
      sp      = tileSolarPos tiltDeg yf hpd calHour latRad lonDeg
      di      = tileDayInfo tiltDeg yf hpd latRad
      irr     = tileIrradiance defaultSolarConfig tiltDeg yf hpd calHour latRad lonDeg
      radToDeg a = a * 180 / pi
      altDeg  = radToDeg (spAltitude sp)
      azDeg   = radToDeg (spAzimuth sp)
      dayH    = diDayLength di
      riseH   = diSunriseHour di
      setH    = diSunsetHour di
      lsh     = localSolarHour hpd calHour lonDeg
      fmtHM h = let hrs = floor h :: Int
                    mins = round ((h - fromIntegral hrs) * 60) :: Int
                in Text.pack (show hrs) <> ":" <> (if mins < 10 then "0" else "") <> Text.pack (show mins)
  in [ "--- Sun ---"
     , "Local " <> fmtHM lsh
     , "Alt   " <> fmtU altDeg "°"  <> "  Az " <> fmtU azDeg "°"
     , "Day   " <> fmtU dayH "h"
     , "Rise  " <> fmtHM riseH <> "  Set " <> fmtHM setH
     , "Irrad " <> fmtF irr
     ]

inspectorSections :: [TerrainInspectorPluginData] -> UiState -> TerrainSnapshot -> (Int, Int) -> HexSample -> [TerrainInspectorSection]
inspectorSections pluginData ui terrainSnap (q, r) sample =
  [ coordinatesSection
  , elevationSection
  , tectonicsSection
  , erosionSection
  , hydrologySection
  , waterBodySection
  , waterTableSection
  , climateSection
  , weatherSection
  , weatherNormalsSection
  , weatherTimelineSection
  , biomeSection
  , soilSection
  , vegetationSection
  , glacierSection
  , volcanismSection
  , oceanCurrentSection
  , overlayRecordsSection
  , overlaySchemaSection
  , overlayProvenanceSection
  , pluginDataSection
  , stageProvenanceSection
  , unitConversionsSection
  , exportLinksSection
  ]
  where
    units = defaultUnitScales
    ChunkCoord chunkQ chunkR = hsChunk sample
    ChunkId chunkKey = chunkIdFromCoord (hsChunk sample)
    TileCoord localQ localR = hsLocal sample
    tileIdx = hsTileIndex sample
    (lat, lon) = latLonValues ui terrainSnap q r
    waterLevel = uiWaterLevel ui
    elevationM = normToMetres units (hsElevation sample)
    relativeWaterM = elevationM - normToMetres units waterLevel
    dirSlope = hsDirSlope sample
    slopeDeg = normSlopeToDeg units
    plateSpeed = sqrt (hsPlateVelX sample ** 2 + hsPlateVelY sample ** 2)
    oceanCurrentCfg = oceanCurrentConfigFromUi ui
    isWater = hsElevation sample < waterLevel
    landEast = hasLandAlongChunk terrainSnap waterLevel sample HexE
    landWest = hasLandAlongChunk terrainSnap waterLevel sample HexW
    currentOffsetNorm = if isWater then oceanCurrentOffset oceanCurrentCfg (lat * pi / 180) landEast landWest else 0
    currentOffsetC = normToC units (0.5 + currentOffsetNorm) - normToC units 0.5

    coordinatesSection = section "coordinates" "Coordinates / Chunk"
      [ intField "q" "Q" q
      , intField "r" "R" r
      , floatUnitField "latitude_deg" "Latitude" lat "°"
      , floatUnitField "longitude_deg" "Longitude" lon "°"
      , intField "chunk_id" "Chunk" chunkKey
      , intField "chunk_q" "Chunk q" chunkQ
      , intField "chunk_r" "Chunk r" chunkR
      , intField "tile_index" "Tile" tileIdx
      , intField "local_q" "Local q" localQ
      , intField "local_r" "Local r" localR
      , intField "chunk_size" "Chunk size" (tsChunkSize terrainSnap)
      ]

    elevationSection = section "elevation_hypsometry" "Elevation / Hypsometry"
      [ floatField "elevation_norm" "Elev norm" (hsElevation sample)
      , floatUnitField "elevation_m" "Elev" elevationM "m"
      , floatField "water_level_norm" "Water level" waterLevel
      , floatUnitField "relative_water_level_m" "Sea delta" relativeWaterM "m"
      , textField "hypsometric_zone" "Zone" (hypsometricZone waterLevel (hsElevation sample))
      ]

    tectonicsSection = section "tectonics_plates" "Tectonics / Plates"
      [ word16Field "plate_id" "Plate" (hsPlateId sample)
      , textField "boundary" "Boundary" (plateBoundaryDisplayName (hsPlateBoundary sample))
      , textField "crust" "Crust" (crustDisplayName (hsPlateCrust sample))
      , floatUnitField "plate_height_m" "Plate h" (normToMetres units (hsPlateHeight sample)) "m"
      , floatField "plate_hardness" "Hardness" (hsPlateHardness sample)
      , floatField "plate_age" "Age" (hsPlateAge sample)
      , floatField "velocity_x" "Vel X" (hsPlateVelX sample)
      , floatField "velocity_y" "Vel Y" (hsPlateVelY sample)
      , floatField "velocity_speed" "Speed" plateSpeed
      ]

    erosionSection = section "erosion_terrain_form" "Erosion / Terrain Form"
      [ textField "terrain_form" "Form" (Text.pack (terrainFormDisplayName (hsTerrainForm sample)))
      , floatUnitField "slope_avg_deg" "Slope" (slopeDeg (hsSlope sample)) "°"
      , floatUnitField "slope_max_deg" "Slope max" (slopeDeg (dsMaxSlope dirSlope)) "°"
      , floatUnitField "slope_min_deg" "Slope min" (slopeDeg (dsMinSlope dirSlope)) "°"
      , floatField "curvature" "Curvature" (hsCurvature sample)
      , floatField "roughness" "Roughness" (hsRoughness sample)
      , floatField "ruggedness" "Rugged" (hsRuggedness sample)
      , floatField "relief" "Relief" (hsRelief sample)
      , floatField "relief_2ring" "Relief2" (hsRelief2Ring sample)
      , floatField "relief_3ring" "Relief3" (hsRelief3Ring sample)
      , floatField "micro_relief" "Micro" (hsMicroRelief sample)
      , floatField "hardness" "Rock hard" (hsHardness sample)
      , word16Field "rock_type" "Rock type" (hsRockType sample)
      , floatField "rock_density" "Rock dens" (hsRockDensity sample)
      , word16Field "soil_type" "Soil type" (hsSoilType sample)
      , floatUnitField "soil_depth_m" "Soil" (normToSoilM units (hsSoilDepth sample)) "m"
      , floatField "soil_grain" "Grain" (hsSoilGrain sample)
      ]

    hydrologySection = section "hydrology_rivers" "Hydrology / Rivers" $
      case hsRiverChunk sample of
        Nothing -> [statusField "not_loaded"]
        Just rc ->
          [ floatField "moisture" "Moist" (hsMoisture sample)
          , maybeFloatField "flow_accum" "Flow acc" (safeIndexMaybe (rcFlowAccum rc) tileIdx)
          , maybeFloatField "discharge" "Dischg" (safeIndexMaybe (rcDischarge rc) tileIdx)
          , maybeFloatField "channel_depth" "Depth" (safeIndexMaybe (rcChannelDepth rc) tileIdx)
          , maybeWord16Field "river_order" "Order" (safeIndexMaybe (rcRiverOrder rc) tileIdx)
          , maybeWord32Field "basin_id" "Basin" (safeIndexMaybe (rcBasinId rc) tileIdx)
          , maybeFloatField "baseflow" "Baseflow" (safeIndexMaybe (rcBaseflow rc) tileIdx)
          , maybeFloatField "erosion_potential" "Erode" (safeIndexMaybe (rcErosionPotential rc) tileIdx)
          , maybeFloatField "deposit_potential" "Deposit" (safeIndexMaybe (rcDepositPotential rc) tileIdx)
          , maybeIntField "flow_dir" "Flow dir" (safeIndexMaybe (rcFlowDir rc) tileIdx)
          , maybeIntField "segment_count" "Segments" (riverSegmentCount rc tileIdx)
          ]

    waterBodySection = section "water_bodies" "Water Bodies" $
      case hsWaterBodyChunk sample of
        Nothing -> [statusField "not_loaded"]
        Just wb ->
          [ maybeTextField "type" "Type" waterBodyDisplayName (safeIndexMaybe (wbType wb) tileIdx)
          , maybeTextField "adjacent_type" "Adjacent" waterBodyDisplayName (safeIndexMaybe (wbAdjacentType wb) tileIdx)
          , maybeUnitField "surface_elevation_m" "Surface" (normToMetres units) "m" (safeIndexMaybe (wbSurfaceElev wb) tileIdx)
          , maybeWord32Field "basin_id" "Basin" (safeIndexMaybe (wbBasinId wb) tileIdx)
          , maybeFloatField "depth_norm" "Depth n" (safeIndexMaybe (wbDepth wb) tileIdx)
          , maybeUnitField "depth_m" "Depth" (negate . normDepthToMetres units) "m" (safeIndexMaybe (wbDepth wb) tileIdx)
          ]

    waterTableSection = section "water_table" "Water Table" $
      case hsGroundwaterChunk sample of
        Nothing -> [statusField "not_loaded"]
        Just gw ->
          [ maybeFloatField "storage" "Storage" (safeIndexMaybe (gwStorage gw) tileIdx)
          , maybeFloatField "recharge" "Recharge" (safeIndexMaybe (gwRecharge gw) tileIdx)
          , maybeFloatField "discharge" "Dischg" (safeIndexMaybe (gwDischarge gw) tileIdx)
          , maybeWord32Field "basin_id" "Basin" (safeIndexMaybe (gwBasinId gw) tileIdx)
          , maybeFloatField "infiltration" "Infil" (safeIndexMaybe (gwInfiltration gw) tileIdx)
          , maybeFloatField "water_table_depth" "WT depth" (safeIndexMaybe (gwWaterTableDepth gw) tileIdx)
          , maybeFloatField "root_zone_moisture" "Root moist" (safeIndexMaybe (gwRootZoneMoisture gw) tileIdx)
          ]

    climateSection = section "climate_weather" "Average Climate" $
      case hsClimateChunk sample of
        Nothing -> [statusField "not_loaded", temporalBasisField LongRunAverage, sourceKindField GeneratedClimate]
        Just cc ->
          [ temporalBasisField LongRunAverage
          , sourceKindField GeneratedClimate
          , maybeFloatField "temp_avg_norm" "Avg climate temp n" (safeIndexMaybe (ccTempAvg cc) tileIdx)
          , maybeUnitField "temp_avg_c" "Avg climate temp" (normToC units) "°C" (safeIndexMaybe (ccTempAvg cc) tileIdx)
          , maybeFloatField "precip_avg_norm" "Avg climate precip n" (safeIndexMaybe (ccPrecipAvg cc) tileIdx)
          , maybeUnitField "precip_avg_mm_year" "Avg climate precip" (normToMmYear units) "mm/yr" (safeIndexMaybe (ccPrecipAvg cc) tileIdx)
          , maybeUnitField "humidity_avg_pct" "Avg climate humid" normToRH "% RH" (safeIndexMaybe (ccHumidityAvg cc) tileIdx)
          , maybeUnitField "wind_spd_avg_ms" "Avg climate wind" (normToWindMs units) "m/s" (safeIndexMaybe (ccWindSpdAvg cc) tileIdx)
          , maybeUnitField "wind_dir_avg_rad" "Avg climate wind dir" id "rad" (safeIndexMaybe (ccWindDirAvg cc) tileIdx)
          , maybeFloatField "temp_range_norm" "Temp range" (safeIndexMaybe (ccTempRange cc) tileIdx)
          , maybeFloatField "precip_seasonality" "Seasonality" (safeIndexMaybe (ccPrecipSeasonality cc) tileIdx)
          ]

    weatherSection = section "weather_snapshot" "Current Simulated Weather" $
      case hsWeatherChunk sample of
        Nothing -> [statusField "not_loaded", temporalBasisField InstantaneousCurrent, sourceKindField SimulatedWeather]
        Just wc ->
          [ temporalBasisField InstantaneousCurrent
          , sourceKindField SimulatedWeather
          , maybeUnitField "temp_c" "Current temp" (normToC units) "°C" (safeIndexMaybe (wcTemp wc) tileIdx)
          , maybeUnitField "humidity_pct" "Current humid" normToRH "% RH" (safeIndexMaybe (wcHumidity wc) tileIdx)
          , maybeUnitField "wind_dir_rad" "Current wind dir" id "rad" (safeIndexMaybe (wcWindDir wc) tileIdx)
          , maybeUnitField "wind_spd_ms" "Current wind" (normToWindMs units) "m/s" (safeIndexMaybe (wcWindSpd wc) tileIdx)
          , maybeUnitField "pressure_hpa" "Current pressure" (normToHPa units) "hPa" (safeIndexMaybe (wcPressure wc) tileIdx)
          , maybeUnitField "precip_mm_year" "Current precip" (normToMmYear units) "mm/yr" (safeIndexMaybe (wcPrecip wc) tileIdx)
          , maybeFloatField "cloud_cover" "Current cloud" (safeIndexMaybe (wcCloudCover wc) tileIdx)
          , maybeFloatField "cloud_water" "Current cloud water" (safeIndexMaybe (wcCloudWater wc) tileIdx)
          , maybeFloatField "cloud_cover_low" "Current cloud low" (safeIndexMaybe (wcCloudCoverLow wc) tileIdx)
          , maybeFloatField "cloud_cover_mid" "Current cloud mid" (safeIndexMaybe (wcCloudCoverMid wc) tileIdx)
          , maybeFloatField "cloud_cover_high" "Current cloud high" (safeIndexMaybe (wcCloudCoverHigh wc) tileIdx)
          , maybeFloatField "cloud_water_low" "Current cloud water low" (safeIndexMaybe (wcCloudWaterLow wc) tileIdx)
          , maybeFloatField "cloud_water_mid" "Current cloud water mid" (safeIndexMaybe (wcCloudWaterMid wc) tileIdx)
          , maybeFloatField "cloud_water_high" "Current cloud water high" (safeIndexMaybe (wcCloudWaterHigh wc) tileIdx)
          ]

    weatherNormalsSection = section "weather_normals" "Typical Weather Normals" $
      case hsWeatherNormalsChunk sample of
        Nothing ->
          [ statusField "unavailable"
          , temporalBasisField TypicalNormal
          , sourceKindField WeatherNormals
          , textField "reason" "Reason" "weather_normals overlay not present"
          ]
        Just wn ->
          [ temporalBasisField TypicalNormal
          , sourceKindField WeatherNormals
          , maybeUnitField "normal_temp_c" "Typical temp" (normToC units) "°C" (safeIndexMaybe (wncTemp wn) tileIdx)
          , maybeUnitField "normal_humidity_pct" "Typical humid" normToRH "% RH" (safeIndexMaybe (wncHumidity wn) tileIdx)
          , maybeUnitField "normal_precip_mm_year" "Typical precip" (normToMmYear units) "mm/yr" (safeIndexMaybe (wncPrecip wn) tileIdx)
          , maybeFloatField "normal_cloud_cover" "Typical cloud" (safeIndexMaybe (wncCloudCover wn) tileIdx)
          , maybeFloatField "normal_cloud_water" "Typical cloud water" (safeIndexMaybe (wncCloudWater wn) tileIdx)
          , maybeFloatField "normal_cloud_cover_low" "Typical cloud low" (safeIndexMaybe (wncCloudCoverLow wn) tileIdx)
          , maybeFloatField "normal_cloud_cover_mid" "Typical cloud mid" (safeIndexMaybe (wncCloudCoverMid wn) tileIdx)
          , maybeFloatField "normal_cloud_cover_high" "Typical cloud high" (safeIndexMaybe (wncCloudCoverHigh wn) tileIdx)
          , maybeFloatField "normal_cloud_water_low" "Typical cloud water low" (safeIndexMaybe (wncCloudWaterLow wn) tileIdx)
          , maybeFloatField "normal_cloud_water_mid" "Typical cloud water mid" (safeIndexMaybe (wncCloudWaterMid wn) tileIdx)
          , maybeFloatField "normal_cloud_water_high" "Typical cloud water high" (safeIndexMaybe (wncCloudWaterHigh wn) tileIdx)
          ]

    weatherTimelineSection = section "weather_timeline" "Weather Timeline"
      [ temporalBasisField InstantaneousCurrent
      , sourceKindField SimulatedWeather
      , word64Field "tick" "Tick" (wtTick (tgcWorldTime (tsGeoContext terrainSnap)))
      , word64Field "weather_version" "Weather version" (tsWeatherVersion terrainSnap)
      , word64Field "published_weather_version" "Published weather version" (tsWeatherVersion terrainSnap)
      ]

    biomeSection = section "biome_refinement" "Biome / Refinement"
      [ word16Field "biome_code" "Code" (biomeIdToCode (hsBiome sample))
      , textField "biome" "Biome" (biomeDisplayName (hsBiome sample))
      , textField "family" "Family" (biomeFamilyName (hsBiome sample))
      , textField "refinement" "Refined" (biomeRefinementStatus (hsBiome sample))
      , textField "terrain_form" "Form" (Text.pack (terrainFormDisplayName (hsTerrainForm sample)))
      , maybeTextField "water_type" "Water" waterBodyDisplayName (hsWaterBodyChunk sample >>= \wb -> safeIndexMaybe (wbType wb) tileIdx)
      , maybeTextField "adjacent_water_type" "Adj water" waterBodyDisplayName (hsWaterBodyChunk sample >>= \wb -> safeIndexMaybe (wbAdjacentType wb) tileIdx)
      , floatUnitField "temp_avg_c" "Avg climate temp" (normToC units (hsTemp sample)) "°C"
      , floatUnitField "precip_avg_mm_year" "Avg climate precip" (normToMmYear units (hsPrecipAvg sample)) "mm/yr"
      , floatField "moisture" "Moist" (hsMoisture sample)
      , floatField "fertility" "Fert" (hsFertility sample)
      ]

    soilSection = section "soil" "Soil"
      [ word16Field "soil_type" "Soil type" (hsSoilType sample)
      , floatField "soil_depth_norm" "Depth n" (hsSoilDepth sample)
      , floatUnitField "soil_depth_m" "Depth" (normToSoilM units (hsSoilDepth sample)) "m"
      , floatField "soil_moisture" "Moist" (hsMoisture sample)
      , floatField "fertility" "Fert" (hsFertility sample)
      , floatField "soil_grain" "Grain" (hsSoilGrain sample)
      , word16Field "rock_type" "Rock type" (hsRockType sample)
      , floatField "rock_density" "Rock dens" (hsRockDensity sample)
      , floatField "hardness" "Hardness" (hsHardness sample)
      ]

    vegetationSection = section "vegetation" "Vegetation" $
      case hsVegetationChunk sample of
        Nothing -> [statusField "not_loaded"]
        Just vc ->
          [ maybeFloatField "cover" "Cover" (safeIndexMaybe (vegCover vc) tileIdx)
          , maybeFloatField "density" "Density" (safeIndexMaybe (vegDensity vc) tileIdx)
          , maybeFloatField "albedo" "Albedo" (safeIndexMaybe (vegAlbedo vc) tileIdx)
          , floatField "fertility" "Fert" (hsFertility sample)
          , floatField "moisture" "Moist" (hsMoisture sample)
          , textField "biome" "Biome" (biomeDisplayName (hsBiome sample))
          ]

    glacierSection = section "glacier_snow_ice" "Glacier / Snow / Ice" $
      case hsGlacierChunk sample of
        Nothing -> [statusField "not_loaded"]
        Just gl ->
          [ maybeFloatField "snowpack" "Snowpack" (safeIndexMaybe (glSnowpack gl) tileIdx)
          , maybeFloatField "ice_thickness" "Ice" (safeIndexMaybe (glIceThickness gl) tileIdx)
          , maybeFloatField "melt" "Melt" (safeIndexMaybe (glMelt gl) tileIdx)
          , maybeFloatField "flow" "Flow" (safeIndexMaybe (glFlow gl) tileIdx)
          , maybeFloatField "erosion_potential" "Erode" (safeIndexMaybe (glErosionPotential gl) tileIdx)
          , maybeFloatField "deposit_potential" "Deposit" (safeIndexMaybe (glDepositPotential gl) tileIdx)
          , floatUnitField "temp_avg_c" "Avg climate temp" (normToC units (hsTemp sample)) "°C"
          , floatUnitField "elevation_m" "Elev" elevationM "m"
          , textField "biome" "Biome" (biomeDisplayName (hsBiome sample))
          ]

    volcanismSection = section "volcanism" "Volcanism" $
      case hsVolcanismChunk sample of
        Nothing -> [statusField "not_loaded"]
        Just vc ->
          [ maybeTextField "vent_type" "Vent" ventTypeDisplayName (safeIndexMaybe (vcVentType vc) tileIdx)
          , maybeTextField "activity" "Activity" ventActivityDisplayName (safeIndexMaybe (vcActivity vc) tileIdx)
          , maybeFloatField "magma" "Magma" (safeIndexMaybe (vcMagma vc) tileIdx)
          , maybeWord16Field "eruption_count" "Eruptions" (safeIndexMaybe (vcEruptionCount vc) tileIdx)
          , maybeFloatField "erupted_total" "Erupted" (safeIndexMaybe (vcEruptedTotal vc) tileIdx)
          , maybeFloatField "lava_potential" "Lava" (safeIndexMaybe (vcLavaPotential vc) tileIdx)
          , maybeFloatField "ash_potential" "Ash" (safeIndexMaybe (vcAshPotential vc) tileIdx)
          , maybeFloatField "deposit_potential" "Deposit" (safeIndexMaybe (vcDepositPotential vc) tileIdx)
          , textField "plate_boundary" "Boundary" (plateBoundaryDisplayName (hsPlateBoundary sample))
          , floatField "rock_density" "Rock dens" (hsRockDensity sample)
          , floatField "soil_grain" "Soil grain" (hsSoilGrain sample)
          ]

    oceanCurrentSection = section "ocean_currents" "Ocean Currents"
      [ textField "status" "Status" (if isWater then "estimate_current_ui" else "land")
      , textField "config_source" "Config" "current UI"
      , textField "sample_scope" "Scope" "same chunk, 2 hexes"
      , boolField "water_tile" "Water" isWater
      , maybeTextField "water_type" "Water type" waterBodyDisplayName (hsWaterBodyChunk sample >>= \wb -> safeIndexMaybe (wbType wb) tileIdx)
      , boolField "land_east_2" "Land east" landEast
      , boolField "land_west_2" "Land west" landWest
      , floatUnitField "latitude_deg" "Latitude" lat "°"
      , floatField "temp_offset_norm" "Est temp offset" currentOffsetNorm
      , floatUnitField "temp_offset_c" "Est temp Δ" currentOffsetC "°C"
      , floatField "warm_scale" "Warm scale" (occWarmScale oceanCurrentCfg)
      , floatField "cold_scale" "Cold scale" (occColdScale oceanCurrentCfg)
      ]

    overlayRecordsSection = section "overlay_records" "Overlay Records"
      (overlayRecordFields terrainSnap chunkKey tileIdx)

    overlaySchemaSection = section "overlay_schema" "Overlay Schema"
      (overlaySchemaFields terrainSnap)

    overlayProvenanceSection = section "overlay_provenance" "Overlay Provenance"
      (overlayProvenanceFields terrainSnap)

    pluginDataSection = section "plugin_hex_data" "Plugin Hex Data"
      (pluginDataFields pluginData chunkKey tileIdx)

    stageProvenanceSection = section "stage_provenance" "Stage Provenance"
      [ word64Field "seed" "Seed" (uiSeed ui)
      , word64Field "terrain_version" "Terrain version" (tsVersion terrainSnap)
      , word64Field "sim_tick" "Sim tick" (uiSimTickCount ui)
      , intField "chunk_id" "Chunk" chunkKey
      , intField "tile_index" "Tile" tileIdx
      , textListField "enabled_stages" "Enabled stages" enabledStageNames
      , textListField "disabled_stages" "Disabled stages" disabledStageNames
      , textListField "enabled_plugins" "Enabled plugins" enabledPluginNames
      , textListField "disabled_plugins" "Disabled plugins" disabledPluginNames
      , pluginLifecycleField (uiPluginLifecycles ui)
      ]

    unitConversionsSection = section "unit_conversions" "Unit Conversions"
      [ conversionField "elevation" "Elevation" (hsElevation sample) elevationM "m"
      , conversionField "water_level" "Water level" waterLevel (normToMetres units waterLevel) "m"
      , conversionField "relative_water_level" "Sea delta" (hsElevation sample - waterLevel) relativeWaterM "m"
      , conversionField "slope_avg" "Slope avg" (hsSlope sample) (slopeDeg (hsSlope sample)) "°"
      , conversionField "temp_avg" "Avg climate temp" (hsTemp sample) (normToC units (hsTemp sample)) "°C"
      , conversionField "precip_avg" "Avg climate precip" (hsPrecipAvg sample) (normToMmYear units (hsPrecipAvg sample)) "mm/yr"
      , conversionField "humidity" "Avg climate humidity" (hsHumidity sample) (normToRH (hsHumidity sample)) "% RH"
      , conversionField "weather_wind_spd" "Current wind speed" (hsWeatherWindSpd sample) (normToWindMs units (hsWeatherWindSpd sample)) "m/s"
      , conversionField "weather_pressure" "Current pressure" (hsWeatherPressure sample) (normToHPa units (hsWeatherPressure sample)) "hPa"
      , conversionField "soil_depth" "Soil depth" (hsSoilDepth sample) (normToSoilM units (hsSoilDepth sample)) "m"
      ]

    exportLinksSection = section "export_links" "Export / Copy JSON"
      (exportLinkFields q r chunkKey tileIdx terrainSnap pluginData)

    enabledStageNames =
      [ stageCanonicalName sid
      | sid <- allBuiltinStageIds
      , not (Set.member sid (uiDisabledStages ui))
      ]
    disabledStageNames = map stageCanonicalName (Set.toList (uiDisabledStages ui))
    enabledPluginNames = filter (`Set.notMember` uiDisabledPlugins ui) (uiPluginNames ui)
    disabledPluginNames = Set.toList (uiDisabledPlugins ui)

section :: Text -> Text -> [TerrainInspectorField] -> TerrainInspectorSection
section = TerrainInspectorSection

textField :: Text -> Text -> Text -> TerrainInspectorField
textField key label value = TerrainInspectorField key label value (String value)

statusField :: Text -> TerrainInspectorField
statusField status = textField "status" "Status" status

temporalBasisField :: TemporalBasis -> TerrainInspectorField
temporalBasisField basis = textField "temporal_basis" "Basis" (temporalBasisToText basis)

sourceKindField :: SourceKind -> TerrainInspectorField
sourceKindField source = textField "source_kind" "Source" (sourceKindToText source)

boolField :: Text -> Text -> Bool -> TerrainInspectorField
boolField key label value = TerrainInspectorField key label (if value then "yes" else "no") (toJSON value)

intField :: Text -> Text -> Int -> TerrainInspectorField
intField key label value = TerrainInspectorField key label (Text.pack (show value)) (toJSON value)

word16Field :: Text -> Text -> Word16 -> TerrainInspectorField
word16Field key label value = intField key label (fromIntegral value)

word64Field :: Text -> Text -> Word64 -> TerrainInspectorField
word64Field key label value = TerrainInspectorField key label (Text.pack (show value)) (toJSON value)

jsonField :: Text -> Text -> Text -> Value -> TerrainInspectorField
jsonField = TerrainInspectorField

textListField :: Text -> Text -> [Text] -> TerrainInspectorField
textListField key label values =
  TerrainInspectorField key label display (toJSON values)
  where
    display = if null values then "-" else Text.intercalate ", " values

conversionField :: Text -> Text -> Float -> Float -> Text -> TerrainInspectorField
conversionField key label normalized converted unit =
  TerrainInspectorField key label (fmtU converted unit) $ object
    [ "normalized" .= normalized
    , "converted" .= converted
    , "unit" .= unit
    ]

floatField :: Text -> Text -> Float -> TerrainInspectorField
floatField key label value = TerrainInspectorField key label (fmtF value) (toJSON value)

floatUnitField :: Text -> Text -> Float -> Text -> TerrainInspectorField
floatUnitField key label value unit = TerrainInspectorField key label (fmtU value unit) (toJSON value)

missingField :: Text -> Text -> TerrainInspectorField
missingField key label = TerrainInspectorField key label "-" Null

maybeTextField :: Text -> Text -> (a -> Text) -> Maybe a -> TerrainInspectorField
maybeTextField key label render = maybe (missingField key label) (\value -> textField key label (render value))

maybeFloatField :: Text -> Text -> Maybe Float -> TerrainInspectorField
maybeFloatField key label = maybe (missingField key label) (floatField key label)

maybeUnitField :: Text -> Text -> (Float -> Float) -> Text -> Maybe Float -> TerrainInspectorField
maybeUnitField key label convert unit = maybe (missingField key label) (\value -> floatUnitField key label (convert value) unit)

maybeIntField :: Text -> Text -> Maybe Int -> TerrainInspectorField
maybeIntField key label = maybe (missingField key label) (intField key label)

maybeWord16Field :: Text -> Text -> Maybe Word16 -> TerrainInspectorField
maybeWord16Field key label = maybe (missingField key label) (word16Field key label)

maybeWord32Field :: Text -> Text -> Maybe Word32 -> TerrainInspectorField
maybeWord32Field key label = maybe (missingField key label) (intField key label . fromIntegral)

safeIndexMaybe :: U.Unbox a => U.Vector a -> Int -> Maybe a
safeIndexMaybe v i
  | i >= 0 && i < U.length v = Just (v U.! i)
  | otherwise = Nothing

riverSegmentCount :: RiverChunk -> Int -> Maybe Int
riverSegmentCount rc tileIdx = do
  start <- safeIndexMaybe (rcSegOffsets rc) tileIdx
  end <- safeIndexMaybe (rcSegOffsets rc) (tileIdx + 1)
  pure (max 0 (end - start))

hypsometricZone :: Float -> Float -> Text
hypsometricZone waterLevel elevation
  | elevation < waterLevel = "submerged"
  | elevation < waterLevel + 0.04 = "coastal"
  | elevation < 0.55 = "lowland"
  | elevation < 0.75 = "upland"
  | otherwise = "highland"

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

biomeRefinementStatus :: BiomeId -> Text
biomeRefinementStatus biomeId =
  let code = biomeIdToCode biomeId
  in if code <= 13 then "family" else "refined"

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
    code | code `elem` [14, 15, 16, 20, 38, 63] -> "Forest"
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

oceanCurrentConfigFromUi :: UiState -> OceanCurrentConfig
oceanCurrentConfigFromUi ui = OceanCurrentConfig
  { occWarmScale = sliderToDomainFloat SliderOccWarmScale (uiOccWarmScale ui)
  , occColdScale = sliderToDomainFloat SliderOccColdScale (uiOccColdScale ui)
  , occLatPeakDeg = sliderToDomainFloat SliderOccLatPeakDeg (uiOccLatPeakDeg ui)
  , occLatWidthDeg = sliderToDomainFloat SliderOccLatWidthDeg (uiOccLatWidthDeg ui)
  }

hasLandAlongChunk :: TerrainSnapshot -> Float -> HexSample -> HexDirection -> Bool
hasLandAlongChunk terrainSnap waterLevel sample direction =
  case IntMap.lookup chunkKey (tsTerrainChunks terrainSnap) of
    Nothing -> False
    Just terrainChunk -> any (landAt terrainChunk) [1, 2]
  where
    ChunkId chunkKey = chunkIdFromCoord (hsChunk sample)
    chunkSize = tsChunkSize terrainSnap
    startIdx = hsTileIndex sample
    landAt terrainChunk step =
      let tracedIdx = traceIndexInDirection chunkSize chunkSize direction step startIdx
      in tracedIdx /= startIdx && maybe False (>= waterLevel) (safeIndexMaybe (tcElevation terrainChunk) tracedIdx)

inspectorPluginDataFromUi :: UiState -> [TerrainInspectorPluginData]
inspectorPluginDataFromUi ui =
  [ TerrainInspectorPluginData pluginName schema Nothing
  | (pluginName, schemas) <- Map.toList (uiDataResources ui)
  , schema <- schemas
  , dataResourceHexQueryable schema
  ]

dataResourceHexQueryable :: DataResourceSchema -> Bool
dataResourceHexQueryable schema =
  drsHexBound schema && doQueryByHex (drsOperations schema)

overlayRecordFields :: TerrainSnapshot -> Int -> Int -> [TerrainInspectorField]
overlayRecordFields terrainSnap chunkKey tileIdx =
  case overlayEntries terrainSnap of
    [] -> [statusField "none"]
    overlays -> map (overlayRecordField chunkKey tileIdx) overlays

overlayRecordField :: Int -> Int -> (Text, Overlay) -> TerrainInspectorField
overlayRecordField chunkKey tileIdx (name, overlay) =
  jsonField ("overlay_" <> sanitizeKey name) name display raw
  where
    (recordStatus, fieldValues) = overlayRecordValues overlay chunkKey tileIdx
    renderedValues =
      [ ofdName fd <> "=" <> maybe "-" formatOverlayValue mValue
      | (fd, mValue) <- fieldValues
      ]
    display = recordStatus <> if null renderedValues then "" else " · " <> Text.intercalate ", " renderedValues
    raw = object
      [ "name" .= name
      , "storage" .= overlayStorageText (osStorage (ovSchema overlay))
      , "status" .= recordStatus
      , "chunk" .= chunkKey
      , "tile" .= tileIdx
      , "fields" .= map overlayFieldValueObject fieldValues
      ]

overlayRecordValues :: Overlay -> Int -> Int -> (Text, [(OverlayFieldDef, Maybe OverlayValue)])
overlayRecordValues overlay chunkKey tileIdx =
  case ovData overlay of
    SparseData chunks ->
      case IntMap.lookup chunkKey chunks of
        Nothing -> ("no_chunk", [])
        Just chunk ->
          case chunkLookup tileIdx chunk of
            Nothing -> ("default_record", [(fd, Just (defaultValue fd)) | fd <- fields])
            Just record -> ("sparse_record", [(fd, recordField idx record) | (idx, fd) <- zip [0..] fields])
    DenseData chunks ->
      case IntMap.lookup chunkKey chunks of
        Nothing -> ("no_chunk", [])
        Just fieldVecs ->
          ("dense_record", [(fd, denseFieldValue idx fd fieldVecs) | (idx, fd) <- zip [0..] fields])
  where
    fields = osFields (ovSchema overlay)
    denseFieldValue idx fd fieldVecs
      | idx < V.length fieldVecs =
          let values = fieldVecs V.! idx
          in if tileIdx >= 0 && tileIdx < U.length values
               then Just (floatToOverlayValue (ofdType fd) (values U.! tileIdx))
               else Nothing
      | otherwise = Nothing

overlaySchemaFields :: TerrainSnapshot -> [TerrainInspectorField]
overlaySchemaFields terrainSnap =
  case overlayEntries terrainSnap of
    [] -> [statusField "none"]
    overlays -> map overlaySchemaField overlays

overlaySchemaField :: (Text, Overlay) -> TerrainInspectorField
overlaySchemaField (name, overlay) =
  jsonField ("schema_" <> sanitizeKey name) name display raw
  where
    schema = ovSchema overlay
    display = overlayStorageText (osStorage schema) <> " · " <> showText (length (osFields schema)) <> " fields"
    raw = object
      [ "schema" .= schema
      , "data" .= overlayDataSummary (ovData overlay)
      , "dependencies" .= overlayDepsObject (osDependencies schema)
      ]

overlayProvenanceFields :: TerrainSnapshot -> [TerrainInspectorField]
overlayProvenanceFields terrainSnap =
  case overlayEntries terrainSnap of
    [] -> [statusField "none"]
    overlays -> map overlayProvenanceField overlays

overlayProvenanceField :: (Text, Overlay) -> TerrainInspectorField
overlayProvenanceField (name, overlay) =
  jsonField ("provenance_" <> sanitizeKey name) name display raw
  where
    provenance = ovProvenance overlay
    display = opSource provenance <> " v" <> showText (opVersion provenance) <> " seed " <> showText (opSeed provenance)
    raw = object
      [ "name" .= name
      , "seed" .= opSeed provenance
      , "version" .= opVersion provenance
      , "source" .= opSource provenance
      ]

pluginDataFields :: [TerrainInspectorPluginData] -> Int -> Int -> [TerrainInspectorField]
pluginDataFields pluginData chunkKey tileIdx =
  case pluginData of
    [] -> [statusField "no_hex_bound_resources"]
    entries -> map (pluginDataField chunkKey tileIdx) entries

pluginDataField :: Int -> Int -> TerrainInspectorPluginData -> TerrainInspectorField
pluginDataField chunkKey tileIdx entry =
  jsonField fieldKey (drsLabel schema) display raw
  where
    pluginName = tipdPlugin entry
    schema = tipdSchema entry
    fieldKey = "resource_" <> sanitizeKey pluginName <> "_" <> sanitizeKey (drsName schema)
    display = case tipdResult entry of
      Nothing -> "query available"
      Just (Left _) -> "query failed"
      Just (Right result) -> showText (length (qrsRecords result)) <> " records"
    raw = object $ baseFields <> resultFields
    baseFields =
      [ "plugin" .= pluginName
      , "resource" .= drsName schema
      , "label" .= drsLabel schema
      , "schema" .= dataResourceSchemaObject schema
      , "query" .= object
          [ "type" .= ("by_hex" :: Text)
          , "chunk" .= chunkKey
          , "tile" .= tileIdx
          ]
      , "http" .= object
          [ "method" .= ("GET" :: Text)
          , "path" .= ("/data/records" :: Text)
          , "query" .= object
              [ "plugin" .= pluginName
              , "resource" .= drsName schema
              , "query" .= ("by_hex" :: Text)
              , "chunk" .= chunkKey
              , "tile" .= tileIdx
              ]
          ]
      ]
    resultFields = case tipdResult entry of
      Nothing -> ["status" .= ("query_available" :: Text)]
      Just (Left err) ->
        [ "status" .= ("query_failed" :: Text)
        , "error" .= err
        ]
      Just (Right result) ->
        [ "status" .= ("loaded" :: Text)
        , "records" .= qrsRecords result
        , "total_count" .= qrsTotalCount result
        ]

dataResourceSchemaObject :: DataResourceSchema -> Value
dataResourceSchemaObject schema = object
  [ "schema_version" .= drsSchemaVersion schema
  , "resource_version" .= drsResourceVersion schema
  , "name" .= drsName schema
  , "label" .= drsLabel schema
  , "hex_bound" .= drsHexBound schema
  , "key_field" .= drsKeyField schema
  , "overlay" .= drsOverlay schema
  , "fields" .= map dataResourceFieldObject (drsFields schema)
  , "operations" .= dataResourceOperationsObject (drsOperations schema)
  , "pagination" .= dataResourcePaginationObject (drsPagination schema)
  ]

dataResourceFieldObject :: DataFieldDef -> Value
dataResourceFieldObject field = object
  [ "name" .= dfName field
  , "type" .= dfType field
  , "label" .= dfLabel field
  , "editable" .= dfEditable field
  , "default" .= dfDefault field
  ]

dataResourceOperationsObject :: DataOperations -> Value
dataResourceOperationsObject ops = object
  [ "list" .= doList ops
  , "get" .= doGet ops
  , "create" .= doCreate ops
  , "update" .= doUpdate ops
  , "delete" .= doDelete ops
  , "query_by_hex" .= doQueryByHex ops
  , "query_by_field" .= doQueryByField ops
  , "sort" .= doSort ops
  , "filter" .= doFilter ops
  , "page" .= doPage ops
  ]

dataResourcePaginationObject :: DataPagination -> Value
dataResourcePaginationObject pagination = object
  [ "default_page_size" .= dpDefaultPageSize pagination
  , "max_page_size" .= dpMaxPageSize pagination
  , "default_page_offset" .= dpDefaultPageOffset pagination
  ]

pluginLifecycleField :: Map.Map Text PluginLifecycleSnapshot -> TerrainInspectorField
pluginLifecycleField lifecycles =
  jsonField "plugin_lifecycles" "Plugin lifecycles" display raw
  where
    entries = Map.toList lifecycles
    displayValues = [name <> ":" <> pluginLifecycleStateText (plsState snapshot) | (name, snapshot) <- entries]
    display = if null displayValues then "-" else Text.intercalate ", " displayValues
    raw = toJSON (map lifecycleObject entries)
    lifecycleObject (name, snapshot) = object
      [ "plugin" .= name
      , "state" .= pluginLifecycleStateText (plsState snapshot)
      , "resources" .= plsResources snapshot
      , "reason" .= plsReason snapshot
      , "error_code" .= plsErrorCode snapshot
      , "error_message" .= plsErrorMessage snapshot
      ]

exportLinkFields :: Int -> Int -> Int -> Int -> TerrainSnapshot -> [TerrainInspectorPluginData] -> [TerrainInspectorField]
exportLinkFields q r chunkKey tileIdx terrainSnap pluginData =
  [ jsonField "copy_hex_json" "Copy hex JSON" ("GET " <> hexPath) hexRaw
  , jsonField "copy_sections_json" "Copy sections JSON" "sections[] in get_hex response" sectionsRaw
  , jsonField "export_chunk_json" "Export chunk JSON" "POST /terrain/export" exportRaw
  , jsonField "copy_overlay_json" "Overlay JSON" overlayDisplay overlayRaw
  , jsonField "copy_plugin_hex_json" "Plugin records JSON" pluginDisplay pluginRaw
  ]
  where
    hexPath = "/terrain/hex?q=" <> showText q <> "&r=" <> showText r
    hexRaw = object
      [ "kind" .= ("copy_as_json" :: Text)
      , "http" .= object
          [ "method" .= ("GET" :: Text)
          , "path" .= ("/terrain/hex" :: Text)
          , "query" .= object ["q" .= q, "r" .= r]
          ]
      , "command" .= object
          [ "method" .= ("get_hex" :: Text)
          , "params" .= object ["q" .= q, "r" .= r]
          ]
      ]
    sectionsRaw = object
      [ "kind" .= ("copy_as_json" :: Text)
      , "source" .= ("terrain.hex.sections" :: Text)
      , "field" .= ("sections" :: Text)
      , "hex" .= object ["q" .= q, "r" .= r, "chunk" .= chunkKey, "tile" .= tileIdx]
      ]
    exportRaw = object
      [ "kind" .= ("export_json" :: Text)
      , "http" .= object
          [ "method" .= ("POST" :: Text)
          , "path" .= ("/terrain/export" :: Text)
          , "body" .= object
              [ "chunks" .= [chunkKey]
              , "fields" .= (["elevation", "moisture", "biome"] :: [Text])
              ]
          ]
      , "command" .= object
          [ "method" .= ("export_terrain_data" :: Text)
          , "params" .= object
              [ "chunks" .= [chunkKey]
              , "fields" .= (["elevation", "moisture", "biome"] :: [Text])
              ]
          ]
      ]
    overlayNamesLoaded = map fst (overlayEntries terrainSnap)
    overlayDisplay = if null overlayNamesLoaded then "no overlays" else Text.intercalate ", " overlayNamesLoaded
    overlayRaw = object
      [ "kind" .= ("copy_as_json" :: Text)
      , "source" .= ("terrain.hex.sections.overlay_records" :: Text)
      , "overlays" .= overlayNamesLoaded
      ]
    pluginResources = [tipdPlugin entry <> ":" <> drsName (tipdSchema entry) | entry <- pluginData]
    pluginDisplay = if null pluginResources then "no hex resources" else Text.intercalate ", " pluginResources
    pluginRaw = object
      [ "kind" .= ("copy_as_json" :: Text)
      , "source" .= ("terrain.hex.sections.plugin_hex_data" :: Text)
      , "resources" .= pluginResources
      , "query" .= object
          [ "type" .= ("by_hex" :: Text)
          , "chunk" .= chunkKey
          , "tile" .= tileIdx
          ]
      ]

overlayEntries :: TerrainSnapshot -> [(Text, Overlay)]
overlayEntries terrainSnap =
  case tsOverlayStore terrainSnap of
    OverlayStore overlays -> Map.toList overlays

overlayFieldValueObject :: (OverlayFieldDef, Maybe OverlayValue) -> Value
overlayFieldValueObject (fd, mValue) = object
  [ "name" .= ofdName fd
  , "type" .= overlayFieldTypeDisplay (ofdType fd)
  , "value" .= maybe Null overlayValueRaw mValue
  ]

overlayFieldTypeDisplay :: OverlayFieldType -> Text
overlayFieldTypeDisplay (OFList inner) = "list<" <> overlayFieldTypeDisplay inner <> ">"
overlayFieldTypeDisplay typ = overlayFieldTypeName typ

overlayValueRaw :: OverlayValue -> Value
overlayValueRaw (OVFloat value) = toJSON value
overlayValueRaw (OVInt value) = toJSON value
overlayValueRaw (OVBool value) = toJSON value
overlayValueRaw (OVText value) = toJSON value
overlayValueRaw (OVList values) = toJSON (map overlayValueRaw (V.toList values))

overlayDataSummary :: OverlayData -> Value
overlayDataSummary (SparseData chunks) = object
  [ "storage" .= ("sparse" :: Text)
  , "chunks" .= IntMap.size chunks
  , "records" .= sum [IntMap.size records | OverlayChunk records <- IntMap.elems chunks]
  ]
overlayDataSummary (DenseData chunks) = object
  [ "storage" .= ("dense" :: Text)
  , "chunks" .= IntMap.size chunks
  , "field_vectors" .= sum [V.length fieldVecs | fieldVecs <- IntMap.elems chunks]
  ]

overlayDepsObject :: OverlayDeps -> Value
overlayDepsObject deps = object
  [ "terrain" .= odTerrain deps
  , "overlays" .= odOverlays deps
  ]

overlayStorageText :: OverlayStorage -> Text
overlayStorageText StorageSparse = "sparse"
overlayStorageText StorageDense = "dense"

sanitizeKey :: Text -> Text
sanitizeKey =
  Text.replace ":" "_"
  . Text.replace "/" "_"
  . Text.replace "." "_"
  . Text.replace "-" "_"
  . Text.replace " " "_"

showText :: Show a => a -> Text
showText = Text.pack . show

data HexSample = HexSample
  { hsChunk :: !ChunkCoord
  , hsLocal :: !TileCoord
  , hsTileIndex :: !Int
  , hsElevation :: !Float
  , hsSlope :: !Float
  , hsDirSlope :: !DirectionalSlope
  , hsCurvature :: !Float
  , hsHardness :: !Float
  , hsRockType :: !Word16
  , hsSoilType :: !Word16
  , hsSoilDepth :: !Float
  , hsMoisture :: !Float
  , hsFertility :: !Float
  , hsRoughness :: !Float
  , hsRockDensity :: !Float
  , hsSoilGrain :: !Float
  , hsRelief :: !Float
  , hsRelief2Ring :: !Float
  , hsRelief3Ring :: !Float
  , hsMicroRelief :: !Float
  , hsRuggedness :: !Float
  , hsBiome :: !BiomeId
  , hsTemp :: !Float
  , hsPrecipAvg :: !Float
  , hsHumidity :: !Float
  , hsWeatherTemp :: !Float
  , hsWeatherHumidity :: !Float
  , hsWeatherWindDir :: !Float
  , hsWeatherWindSpd :: !Float
  , hsWeatherPressure :: !Float
  , hsWeatherPrecip :: !Float
  , hsCloudCover :: !Float
  , hsCloudWater :: !Float
  , hsCloudCoverLow :: !Float
  , hsCloudCoverMid :: !Float
  , hsCloudCoverHigh :: !Float
  , hsCloudWaterLow :: !Float
  , hsCloudWaterMid :: !Float
  , hsCloudWaterHigh :: !Float
  , hsVegCover :: !Float
  , hsVegDensity :: !Float
  , hsPlateId :: !Word16
  , hsPlateBoundary :: !PlateBoundary
  , hsPlateHardness :: !Float
  , hsPlateCrust :: !Word16
  , hsPlateAge :: !Float
  , hsPlateHeight :: !Float
  , hsPlateVelX :: !Float
  , hsPlateVelY :: !Float
  , hsTerrainForm :: !TerrainForm
  , hsClimateChunk :: !(Maybe ClimateChunk)
  , hsWeatherChunk :: !(Maybe WeatherChunk)
  , hsWeatherNormalsChunk :: !(Maybe WeatherNormalsChunk)
  , hsRiverChunk :: !(Maybe RiverChunk)
  , hsGroundwaterChunk :: !(Maybe GroundwaterChunk)
  , hsVolcanismChunk :: !(Maybe VolcanismChunk)
  , hsGlacierChunk :: !(Maybe GlacierChunk)
  , hsWaterBodyChunk :: !(Maybe WaterBodyChunk)
  , hsVegetationChunk :: !(Maybe VegetationChunk)
  }

sampleAt :: TerrainSnapshot -> (Int, Int) -> Maybe HexSample
sampleAt terrainSnap (q, r)
  | tsChunkSize terrainSnap <= 0 = Nothing
  | otherwise = do
      let config = WorldConfig { wcChunkSize = tsChunkSize terrainSnap }
          tile = TileCoord q r
          (chunkCoord, local) = chunkCoordFromTile config tile
          ChunkId key = chunkIdFromCoord chunkCoord
      TileIndex idx <- tileIndex config local
      terrainChunk <- IntMap.lookup key (tsTerrainChunks terrainSnap)
      let climateChunk = IntMap.lookup key (tsClimateChunks terrainSnap)
          weatherChunk = IntMap.lookup key (tsWeatherChunks terrainSnap)
          weatherNormalsChunk = getWeatherNormalsChunkFromStore (ChunkId key) (tsOverlayStore terrainSnap)
          riverChunk = IntMap.lookup key (tsRiverChunks terrainSnap)
          groundwaterChunk = IntMap.lookup key (tsGroundwaterChunks terrainSnap)
          volcanismChunk = IntMap.lookup key (tsVolcanismChunks terrainSnap)
          glacierChunk = IntMap.lookup key (tsGlacierChunks terrainSnap)
          waterBodyChunk = IntMap.lookup key (tsWaterBodyChunks terrainSnap)
          vegetationChunk = IntMap.lookup key (tsVegetationChunks terrainSnap)
          fromMaybeChunk accessor = maybe 0 (maybe 0 id . (`safeIndexMaybe` idx) . accessor)
          dirSlope = tcDirSlope terrainChunk U.! idx
      Just HexSample
        { hsChunk = chunkCoord
        , hsLocal = local
        , hsTileIndex = idx
        , hsElevation = tcElevation terrainChunk U.! idx
        , hsSlope = dsAvgSlope dirSlope
        , hsDirSlope = dirSlope
        , hsCurvature = tcCurvature terrainChunk U.! idx
        , hsHardness = tcHardness terrainChunk U.! idx
        , hsRockType = tcRockType terrainChunk U.! idx
        , hsSoilType = tcSoilType terrainChunk U.! idx
        , hsMoisture = tcMoisture terrainChunk U.! idx
        , hsSoilDepth = tcSoilDepth terrainChunk U.! idx
        , hsFertility = tcFertility terrainChunk U.! idx
        , hsRoughness = tcRoughness terrainChunk U.! idx
        , hsRockDensity = tcRockDensity terrainChunk U.! idx
        , hsSoilGrain = tcSoilGrain terrainChunk U.! idx
        , hsRelief = tcRelief terrainChunk U.! idx
        , hsRelief2Ring = tcRelief2Ring terrainChunk U.! idx
        , hsRelief3Ring = tcRelief3Ring terrainChunk U.! idx
        , hsMicroRelief = tcMicroRelief terrainChunk U.! idx
        , hsRuggedness = tcRuggedness terrainChunk U.! idx
        , hsBiome = tcFlags terrainChunk U.! idx
        , hsTemp = fromMaybeChunk ccTempAvg climateChunk
        , hsPrecipAvg = fromMaybeChunk ccPrecipAvg climateChunk
        , hsHumidity = fromMaybeChunk ccHumidityAvg climateChunk
        , hsWeatherTemp = fromMaybeChunk wcTemp weatherChunk
        , hsWeatherHumidity = fromMaybeChunk wcHumidity weatherChunk
        , hsWeatherWindDir = fromMaybeChunk wcWindDir weatherChunk
        , hsWeatherWindSpd = fromMaybeChunk wcWindSpd weatherChunk
        , hsWeatherPressure = fromMaybeChunk wcPressure weatherChunk
        , hsWeatherPrecip = fromMaybeChunk wcPrecip weatherChunk
        , hsCloudCover = fromMaybeChunk wcCloudCover weatherChunk
        , hsCloudWater = fromMaybeChunk wcCloudWater weatherChunk
        , hsCloudCoverLow = fromMaybeChunk wcCloudCoverLow weatherChunk
        , hsCloudCoverMid = fromMaybeChunk wcCloudCoverMid weatherChunk
        , hsCloudCoverHigh = fromMaybeChunk wcCloudCoverHigh weatherChunk
        , hsCloudWaterLow = fromMaybeChunk wcCloudWaterLow weatherChunk
        , hsCloudWaterMid = fromMaybeChunk wcCloudWaterMid weatherChunk
        , hsCloudWaterHigh = fromMaybeChunk wcCloudWaterHigh weatherChunk
        , hsVegCover = fromMaybeChunk vegCover vegetationChunk
        , hsVegDensity = fromMaybeChunk vegDensity vegetationChunk
        , hsPlateId = tcPlateId terrainChunk U.! idx
        , hsPlateBoundary = tcPlateBoundary terrainChunk U.! idx
        , hsPlateHardness = tcPlateHardness terrainChunk U.! idx
        , hsPlateCrust = tcPlateCrust terrainChunk U.! idx
        , hsPlateAge = tcPlateAge terrainChunk U.! idx
        , hsPlateHeight = tcPlateHeight terrainChunk U.! idx
        , hsPlateVelX = tcPlateVelX terrainChunk U.! idx
        , hsPlateVelY = tcPlateVelY terrainChunk U.! idx
        , hsTerrainForm = tcTerrainForm terrainChunk U.! idx
        , hsClimateChunk = climateChunk
        , hsWeatherChunk = weatherChunk
        , hsWeatherNormalsChunk = weatherNormalsChunk
        , hsRiverChunk = riverChunk
        , hsGroundwaterChunk = groundwaterChunk
        , hsVolcanismChunk = volcanismChunk
        , hsGlacierChunk = glacierChunk
        , hsWaterBodyChunk = waterBodyChunk
        , hsVegetationChunk = vegetationChunk
        }

fmtF :: Float -> Text
fmtF = Text.pack . formatF

formatF :: Float -> String
formatF value =
  let scaled = fromIntegral (round (value * 100) :: Int) / 100 :: Double
  in show scaled

fmtU :: Float -> Text -> Text
fmtU value unit =
  let scaled = fromIntegral (round (value * 10) :: Int) / 10 :: Double
  in Text.pack (show scaled) <> " " <> unit

hexHeader :: Int -> Int -> Text
hexHeader q r = "Hex (" <> Text.pack (show q) <> ", " <> Text.pack (show r) <> ")"

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

------------------------------------------------------------------------
-- Overlay hex context
------------------------------------------------------------------------

-- | Look up all overlay field values at the given hex and format them
-- as display lines.
overlayValuesAt :: Overlay -> Int -> Int -> HexSample -> [OverlayFieldDef] -> [Text]
overlayValuesAt overlay chunkKey chunkSize sample fields =
  let config = WorldConfig { wcChunkSize = chunkSize }
  in case tileIndex config (hsLocal sample) of
       Nothing -> ["(invalid tile)"]
       Just (TileIndex idx) -> case ovData overlay of
         DenseData dm ->
           case IntMap.lookup chunkKey dm of
             Nothing -> ["(no data)"]
             Just fieldVecs ->
               zipWith (denseFieldLine idx fieldVecs) [0..] fields
         SparseData sm ->
           case IntMap.lookup chunkKey sm of
             Nothing -> ["(no data)"]
             Just (OverlayChunk tileMap) ->
               case IntMap.lookup idx tileMap of
                 Nothing -> ["(no record)"]
                 Just (OverlayRecord vals) ->
                   zipWith (sparseFieldLine vals) [0..] fields
  where
    denseFieldLine idx fieldVecs fi fd
      | fi < V.length fieldVecs =
          let vec = fieldVecs V.! fi
          in if idx < U.length vec
               then ofdName fd <> "  " <> fmtF (vec U.! idx)
               else ofdName fd <> "  -"
      | otherwise = ofdName fd <> "  -"
    sparseFieldLine vals fi fd
      | fi < V.length vals = ofdName fd <> "  " <> formatOverlayValue (vals V.! fi)
      | otherwise          = ofdName fd <> "  -"

-- | Format an 'OverlayValue' for display in the hex context panel.
formatOverlayValue :: OverlayValue -> Text
formatOverlayValue (OVFloat f) = fmtF f
formatOverlayValue (OVInt i)   = Text.pack (show i)
formatOverlayValue (OVBool b)  = if b then "Yes" else "No"
formatOverlayValue (OVText t)  = t
formatOverlayValue (OVList vs) =
  "[" <> Text.intercalate ", " (map formatOverlayValue (V.toList vs)) <> "]"
