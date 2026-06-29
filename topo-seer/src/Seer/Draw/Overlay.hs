{-# LANGUAGE OverloadedStrings #-}

module Seer.Draw.Overlay
  ( TerrainInspectorView(..)
  , TerrainInspectorSection(..)
  , TerrainInspectorField(..)
  , terrainInspectorView
  , terrainInspectorViewAt
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

import Actor.Data (TerrainSnapshot(..))
import Actor.UI (UiState(..), ViewMode(..))
import Data.Aeson (Value(..), object, toJSON, (.=))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8, Word16, Word32)
import Linear (V2(..), V4(..))
import qualified SDL
import Seer.Config (mapRange)
import Seer.Config.SliderConversion (sliderToDomainFloat)
import Seer.Config.SliderRegistry (SliderId(..))
import Topo
  ( BiomeId
  , ChunkCoord(..)
  , ChunkId(..)
  , ClimateChunk(..)
  , DirectionalSlope(..)
  , GroundwaterChunk(..)
  , PlateBoundary(..)
  , RiverChunk(..)
  , TerrainChunk(..)
  , TerrainForm
  , TileCoord(..)
  , TileIndex(..)
  , VegetationChunk(..)
  , WaterBodyChunk(..)
  , WaterBodyType
  , WeatherChunk(..)
  , WorldConfig(..)
  , biomeDisplayName
  , chunkCoordFromTile
  , chunkIdFromCoord
  , dsAvgSlope
  , dsMaxSlope
  , dsMinSlope
  , plateBoundaryToCode
  , terrainFormDisplayName
  , tileIndex
  , waterBodyToCode
  )
import Topo.Overlay
  ( Overlay(..)
  , OverlayData(..)
  , OverlayChunk(..)
  , OverlayRecord(..)
  , OverlayValue(..)
  , lookupOverlay
  , recordField
  )
import Topo.Overlay.Schema
  ( OverlayFieldDef(..)
  , OverlayFieldType(..)
  , OverlaySchema(..)
  )
import Topo.Hex (HexGridMeta(..))
import Topo.Planet (PlanetConfig(..), WorldSlice(..), formatLatLon, tileLatitude, tileLongitude)
import Topo.Calendar (CalendarConfig(..), WorldTime(..), mkCalendarConfig, tickToDate, yearFraction, CalendarDate(..))
import Topo.Solar (SolarPosition(..), DayInfo(..), tileSolarPos, tileDayInfo, defaultSolarConfig, tileIrradiance, localSolarHour)
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
import UI.HexPick (axialToScreen, renderHexRadiusPx)
import UI.Theme
import UI.WidgetsDraw (drawTextLine)
import qualified Data.Vector.Unboxed as U

-- | Pure view model for the pinned hover-hex terrain inspector.
data TerrainInspectorView = TerrainInspectorView
  { tivHex :: !(Int, Int)
  , tivSections :: ![TerrainInspectorSection]
  , tivLines :: ![Text]
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

terrainInspectorView :: UiState -> TerrainSnapshot -> Maybe TerrainInspectorView
terrainInspectorView ui terrainSnap = do
  hexCoord <- uiHoverHex ui
  pure (terrainInspectorViewAt ui terrainSnap hexCoord)

terrainInspectorViewAt :: UiState -> TerrainSnapshot -> (Int, Int) -> TerrainInspectorView
terrainInspectorViewAt ui terrainSnap hexCoord@(q, r) =
  case sampleAt terrainSnap hexCoord of
    Nothing -> TerrainInspectorView
      { tivHex = hexCoord
      , tivSections = []
      , tivLines = [hexHeader q r, "No data"]
      }
    Just sample ->
      let sections = inspectorSections ui terrainSnap hexCoord sample
          lines = hexHeader q r : latLonLine ui terrainSnap q r : (sectionsToLines sections <> modeContextLines ui terrainSnap hexCoord sample)
      in TerrainInspectorView
        { tivHex = hexCoord
        , tivSections = sections
        , tivLines = lines
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
      let -- World-space placement rect (renderHexRadiusPx=6 coordinate frame).
          -- Derived from the actual hex geometry half-width so the overlay
          -- matches the atlas-normalised hex extents rather than the wider
          -- floor/ceil scanline bounding box of hexSpans.
          hexHalfW = round (sqrt 3 / 2 * fromIntegral renderHexRadiusPx :: Float)
          (cx, cy) = axialToScreen renderHexRadiusPx q r
          (ox, oy) = uiPanOffset uiSnap
          z = uiZoom uiSnap
          wMinX = -hexHalfW :: Int
          wMinY = -renderHexRadiusPx
          worldX = cx + wMinX
          worldY = cy + wMinY
          worldW = 2 * hexHalfW + 1
          worldH = 2 * renderHexRadiusPx
          -- Texture rendered at hexRadius resolution for crisp edges
          hiResSpans = hexSpans hexRadius
          (tMinX, tMinY, tMaxX, tMaxY) = spanBounds hiResSpans
          texW = max 1 (tMaxX - tMinX + 1)
          texH = max 1 (tMaxY - tMinY + 1)
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
    else case terrainInspectorView ui terrainSnap of
      Just inspector -> do
        SDL.P (V2 mx my) <- SDL.getAbsoluteMouseLocation
        let sx = fromIntegral mx
            sy = fromIntegral my
            linesToDraw = tivLines inspector
            lineHeight = 16
            hPad = 12
            vPad = 10
        panelW <- case fontCache of
          Just fc -> do
            widths <- mapM (lineWidth fc) linesToDraw
            pure (maximum (220 : map (+ hPad * 2) widths))
          Nothing -> pure (maximum (220 : map (\line -> Text.length line * 8 + hPad * 2) linesToDraw))
        let panelH = max 40 (vPad * 2 + lineHeight * length linesToDraw)
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
transformRect (ox, oy) z (RectInt (V2 rx ry) (V2 rw rh)) =
  let fx = (fromIntegral rx + ox) * z
      fy = (fromIntegral ry + oy) * z
      fw = fromIntegral rw * z
      fh = fromIntegral rh * z
  in RectInt (V2 (round fx) (round fy)) (V2 (max 1 (round fw)) (max 1 (round fh)))

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
contextLines ui terrainSnap hexCoord = tivLines (terrainInspectorViewAt ui terrainSnap hexCoord)

latLonLine :: UiState -> TerrainSnapshot -> Int -> Int -> Text
latLonLine ui terrainSnap tileQ tileR =
  let (lat, lon) = latLonValues ui terrainSnap tileQ tileR
  in formatLatLon lat lon

latLonValues :: UiState -> TerrainSnapshot -> Int -> Int -> (Float, Float)
latLonValues ui terrainSnap tileQ tileR =
  let (planet, slice, hex, worldConfig) = geoContext ui terrainSnap
      tile = TileCoord tileQ tileR
      lat = tileLatitude planet hex slice worldConfig tile
      lon = tileLongitude planet hex slice worldConfig tile
  in (lat, lon)

geoContext :: UiState -> TerrainSnapshot -> (PlanetConfig, WorldSlice, HexGridMeta, WorldConfig)
geoContext ui terrainSnap =
  let planet = PlanetConfig
        { pcRadius = mapRange 4778 9557 (uiPlanetRadius ui)
        , pcAxialTilt = mapRange 0 45 (uiAxialTilt ui)
        , pcInsolation = mapRange 0.7 1.3 (uiInsolation ui)
        }
      slice = WorldSlice
        { wsLatCenter = mapRange (-90) 90 (uiSliceLatCenter ui)
        , wsLatExtent = 0
        , wsLonCenter = mapRange (-180) 180 (uiSliceLonCenter ui)
        , wsLonExtent = 0
        }
      hex = HexGridMeta { hexSizeKm = sliderToDomainFloat SliderHexSizeKm (uiHexSizeKm ui) }
      worldConfig = WorldConfig { wcChunkSize = tsChunkSize terrainSnap }
  in (planet, slice, hex, worldConfig)

sectionsToLines :: [TerrainInspectorSection] -> [Text]
sectionsToLines = concatMap sectionLines
  where
    sectionLines section = ("--- " <> tisTitle section <> " ---") : map fieldLine (tisFields section)
    fieldLine field = tifLabel field <> " " <> tifValue field

modeContextLines :: UiState -> TerrainSnapshot -> (Int, Int) -> HexSample -> [Text]
modeContextLines ui terrainSnap (q, r) sample = modeLines (uiViewMode ui) sample
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
      , "Humid  " <> fmtU (normToRH (hsHumidity sample')) "% RH"
      , "Fert   " <> fmtF (hsFertility sample')
      , "Veg    " <> fmtF (hsVegCover sample')
      , "VDen   " <> fmtF (hsVegDensity sample')
      ] ++ solarLines q r
    modeLines ViewClimate sample' =
      [ "Temp  " <> fmtU (normToC units (hsTemp sample')) "°C"
      , "Precip " <> fmtU (normToMmYear units (hsPrecipAvg sample')) "mm/yr"
      ] ++ solarLines q r
    modeLines ViewWeather sample' =
      [ "Biome " <> biomeDisplayName (hsBiome sample')
      , "Elev  " <> fmtU (normToMetres units (hsElevation sample')) "m"
      , "Slope " <> fmtU (normSlopeToDeg units (hsSlope sample')) "° avg"
      , "Temp  " <> fmtU (normToC units (hsWeatherTemp sample')) "°C"
      , "Humid " <> fmtU (normToRH (hsWeatherHumidity sample')) "% RH"
      , "WindD " <> fmtU (hsWeatherWindDir sample') "rad"
      , "WindS " <> fmtU (normToWindMs units (hsWeatherWindSpd sample')) "m/s"
      , "Press " <> fmtU (normToHPa units (hsWeatherPressure sample')) "hPa"
      , "Precp " <> fmtU (normToMmYear units (hsWeatherPrecip sample')) "mm/yr"
      ] ++ solarLines q r
    modeLines ViewMoisture sample' =
      [ "Moist " <> fmtU (normToRH (hsMoisture sample')) "%"
      , "Soil  " <> fmtU (normToSoilM units (hsSoilDepth sample')) "m"
      ]
    modeLines ViewPrecip sample' =
      [ "Precip " <> fmtU (normToMmYear units (hsPrecipAvg sample')) "mm/yr"
      , "Humid " <> fmtU (normToRH (hsHumidity sample')) "% RH"
      ]
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
      in [ "Cloud " <> pct (hsCloudCover sample') <> "  Water " <> fmtF (hsCloudWater sample')
         , "  Low  " <> pct (hsCloudCoverLow sample') <> "  " <> fmtF (hsCloudWaterLow sample')
         , "  Mid  " <> pct (hsCloudCoverMid sample') <> "  " <> fmtF (hsCloudWaterMid sample')
         , "  High " <> pct (hsCloudCoverHigh sample') <> "  " <> fmtF (hsCloudWaterHigh sample')
         , "Precp " <> fmtU (normToMmYear units (hsWeatherPrecip sample')) "mm/yr"
         , "WindD " <> fmtU (hsWeatherWindDir sample') "rad"
         , "WindS " <> fmtU (normToWindMs units (hsWeatherWindSpd sample')) "m/s"
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
solarLinesFor ui terrainSnap tileQ tileR =
  let (planet, slice, hex, worldConfig) = geoContext ui terrainSnap
      calCfg = mkCalendarConfig planet
      worldTime = WorldTime
        { wtTick     = uiSimTickCount ui
        , wtTickRate = realToFrac (uiSimTickRate ui)
        }
      calDate = tickToDate calCfg worldTime
      yf      = realToFrac (yearFraction calCfg worldTime) :: Float
      hpd     = realToFrac (ccHoursPerDay calCfg) :: Float
      calHour = realToFrac (cdHourOfDay calDate) :: Float
      tiltDeg = pcAxialTilt planet
      tile    = TileCoord tileQ tileR
      latDeg  = tileLatitude planet hex slice worldConfig tile
      latRad  = latDeg * pi / 180
      lonDeg  = tileLongitude planet hex slice worldConfig tile
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

inspectorSections :: UiState -> TerrainSnapshot -> (Int, Int) -> HexSample -> [TerrainInspectorSection]
inspectorSections ui terrainSnap (q, r) sample =
  [ coordinatesSection
  , elevationSection
  , tectonicsSection
  , erosionSection
  , hydrologySection
  , waterBodySection
  , waterTableSection
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

section :: Text -> Text -> [TerrainInspectorField] -> TerrainInspectorSection
section = TerrainInspectorSection

textField :: Text -> Text -> Text -> TerrainInspectorField
textField key label value = TerrainInspectorField key label value (String value)

statusField :: Text -> TerrainInspectorField
statusField status = textField "status" "Status" status

intField :: Text -> Text -> Int -> TerrainInspectorField
intField key label value = TerrainInspectorField key label (Text.pack (show value)) (toJSON value)

word16Field :: Text -> Text -> Word16 -> TerrainInspectorField
word16Field key label value = intField key label (fromIntegral value)

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
  , hsRiverChunk :: !(Maybe RiverChunk)
  , hsGroundwaterChunk :: !(Maybe GroundwaterChunk)
  , hsWaterBodyChunk :: !(Maybe WaterBodyChunk)
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
          riverChunk = IntMap.lookup key (tsRiverChunks terrainSnap)
          groundwaterChunk = IntMap.lookup key (tsGroundwaterChunks terrainSnap)
          waterBodyChunk = IntMap.lookup key (tsWaterBodyChunks terrainSnap)
          vegetationChunk = IntMap.lookup key (tsVegetationChunks terrainSnap)
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
        , hsTemp = maybe 0 (\chunk -> ccTempAvg chunk U.! idx) climateChunk
        , hsPrecipAvg = maybe 0 (\chunk -> ccPrecipAvg chunk U.! idx) climateChunk
        , hsHumidity = maybe 0 (\chunk -> wcHumidity chunk U.! idx) weatherChunk
        , hsWeatherTemp = maybe 0 (\chunk -> wcTemp chunk U.! idx) weatherChunk
        , hsWeatherHumidity = maybe 0 (\chunk -> wcHumidity chunk U.! idx) weatherChunk
        , hsWeatherWindDir = maybe 0 (\chunk -> wcWindDir chunk U.! idx) weatherChunk
        , hsWeatherWindSpd = maybe 0 (\chunk -> wcWindSpd chunk U.! idx) weatherChunk
        , hsWeatherPressure = maybe 0 (\chunk -> wcPressure chunk U.! idx) weatherChunk
        , hsWeatherPrecip = maybe 0 (\chunk -> wcPrecip chunk U.! idx) weatherChunk
        , hsCloudCover = maybe 0 (\chunk -> wcCloudCover chunk U.! idx) weatherChunk
        , hsCloudWater = maybe 0 (\chunk -> wcCloudWater chunk U.! idx) weatherChunk
        , hsCloudCoverLow = maybe 0 (\chunk -> wcCloudCoverLow chunk U.! idx) weatherChunk
        , hsCloudCoverMid = maybe 0 (\chunk -> wcCloudCoverMid chunk U.! idx) weatherChunk
        , hsCloudCoverHigh = maybe 0 (\chunk -> wcCloudCoverHigh chunk U.! idx) weatherChunk
        , hsCloudWaterLow = maybe 0 (\chunk -> wcCloudWaterLow chunk U.! idx) weatherChunk
        , hsCloudWaterMid = maybe 0 (\chunk -> wcCloudWaterMid chunk U.! idx) weatherChunk
        , hsCloudWaterHigh = maybe 0 (\chunk -> wcCloudWaterHigh chunk U.! idx) weatherChunk
        , hsVegCover = maybe 0 (\chunk -> vegCover chunk U.! idx) vegetationChunk
        , hsVegDensity = maybe 0 (\chunk -> vegDensity chunk U.! idx) vegetationChunk
        , hsPlateId = tcPlateId terrainChunk U.! idx
        , hsPlateBoundary = tcPlateBoundary terrainChunk U.! idx
        , hsPlateHardness = tcPlateHardness terrainChunk U.! idx
        , hsPlateCrust = tcPlateCrust terrainChunk U.! idx
        , hsPlateAge = tcPlateAge terrainChunk U.! idx
        , hsPlateHeight = tcPlateHeight terrainChunk U.! idx
        , hsPlateVelX = tcPlateVelX terrainChunk U.! idx
        , hsPlateVelY = tcPlateVelY terrainChunk U.! idx
        , hsTerrainForm = tcTerrainForm terrainChunk U.! idx
        , hsRiverChunk = riverChunk
        , hsGroundwaterChunk = groundwaterChunk
        , hsWaterBodyChunk = waterBodyChunk
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
