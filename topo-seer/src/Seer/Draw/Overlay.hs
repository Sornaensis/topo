{-# LANGUAGE OverloadedStrings #-}

module Seer.Draw.Overlay
  ( drawHoverHex
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
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8, Word16)
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
  , PlateBoundary(..)
  , TerrainChunk(..)
  , TerrainForm
  , TileCoord(..)
  , TileIndex(..)
  , VegetationChunk(..)
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
    else case uiHoverHex ui of
      Just (q, r) -> do
        SDL.P (V2 mx my) <- SDL.getAbsoluteMouseLocation
        let sx = fromIntegral mx
            sy = fromIntegral my
            linesToDraw = contextLines ui terrainSnap (q, r)
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
contextLines ui terrainSnap (q, r) =
  case sampleAt terrainSnap (q, r) of
    Nothing -> [hexHeader q r, "No data"]
    Just sample -> hexHeader q r : latLonLine q r : modeLines (uiViewMode ui) sample
  where
    units = defaultUnitScales
    planet = PlanetConfig
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

    latLonLine tileQ tileR =
      let tile = TileCoord tileQ tileR
          lat = tileLatitude planet hex slice worldConfig tile
          lon = tileLongitude planet hex slice worldConfig tile
      in formatLatLon lat lon

    -- Solar info helper: computes sun position, day length, irradiance
    -- for a tile given current simulation time.
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

    solarLines tileQ tileR =
      let tile    = TileCoord tileQ tileR
          latRad  = tileLatitude planet hex slice worldConfig tile
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

    modeLines ViewElevation sample =
      let dirSlope = hsDirSlope sample
          slopeDeg = normSlopeToDeg units
      in [ "Elev  " <> fmtU (normToMetres units (hsElevation sample)) "m"
         , "Form  " <> Text.pack (terrainFormDisplayName (hsTerrainForm sample))
         , "Slope " <> fmtU (slopeDeg (hsSlope sample)) "° avg"
         , "      " <> fmtU (slopeDeg (dsMaxSlope dirSlope)) "° max  " <> fmtU (slopeDeg (dsMinSlope dirSlope)) "° min"
         , "  E   " <> fmtU (slopeDeg (dsSlopeE dirSlope)) "°" <> "   W  " <> fmtU (slopeDeg (dsSlopeW dirSlope)) "°"
         , "  NE  " <> fmtU (slopeDeg (dsSlopeNE dirSlope)) "°" <> "   SW " <> fmtU (slopeDeg (dsSlopeSW dirSlope)) "°"
         , "  NW  " <> fmtU (slopeDeg (dsSlopeNW dirSlope)) "°" <> "   SE " <> fmtU (slopeDeg (dsSlopeSE dirSlope)) "°"
         ] ++ solarLines q r
    modeLines ViewBiome sample =
      [ "Biome  " <> biomeDisplayName (hsBiome sample)
      , "Form   " <> Text.pack (terrainFormDisplayName (hsTerrainForm sample))
      , "Elev   " <> fmtU (normToMetres units (hsElevation sample)) "m"
      , "Slope  " <> fmtU (normSlopeToDeg units (hsSlope sample)) "°"
      , "Precip " <> fmtU (normToMmYear units (hsPrecipAvg sample)) "mm/yr"
      , "Humid  " <> fmtU (normToRH (hsHumidity sample)) "% RH"
      , "Fert   " <> fmtF (hsFertility sample)
      , "Veg    " <> fmtF (hsVegCover sample)
      , "VDen   " <> fmtF (hsVegDensity sample)
      ] ++ solarLines q r
    modeLines ViewClimate sample =
      [ "Temp  " <> fmtU (normToC units (hsTemp sample)) "°C"
      , "Precip " <> fmtU (normToMmYear units (hsPrecipAvg sample)) "mm/yr"
      ] ++ solarLines q r
    modeLines ViewWeather sample =
      [ "Biome " <> biomeDisplayName (hsBiome sample)
      , "Elev  " <> fmtU (normToMetres units (hsElevation sample)) "m"
      , "Slope " <> fmtU (normSlopeToDeg units (hsSlope sample)) "° avg"
      , "Temp  " <> fmtU (normToC units (hsWeatherTemp sample)) "°C"
      , "Humid " <> fmtU (normToRH (hsWeatherHumidity sample)) "% RH"
      , "WindD " <> fmtU (hsWeatherWindDir sample) "rad"
      , "WindS " <> fmtU (normToWindMs units (hsWeatherWindSpd sample)) "m/s"
      , "Press " <> fmtU (normToHPa units (hsWeatherPressure sample)) "hPa"
      , "Precp " <> fmtU (normToMmYear units (hsWeatherPrecip sample)) "mm/yr"
      ] ++ solarLines q r
    modeLines ViewMoisture sample =
      [ "Moist " <> fmtU (normToRH (hsMoisture sample)) "%"
      , "Soil  " <> fmtU (normToSoilM units (hsSoilDepth sample)) "m"
      ]
    modeLines ViewPrecip sample =
      [ "Precip " <> fmtU (normToMmYear units (hsPrecipAvg sample)) "mm/yr"
      , "Humid " <> fmtU (normToRH (hsHumidity sample)) "% RH"
      ]
    modeLines ViewPlateId sample = ["Plate " <> Text.pack (show (hsPlateId sample))]
    modeLines ViewPlateBoundary sample = ["Boundary " <> plateBoundaryDisplayName (hsPlateBoundary sample)]
    modeLines ViewPlateHardness sample = ["Hardness " <> fmtF (hsPlateHardness sample)]
    modeLines ViewPlateCrust sample = ["Crust " <> crustDisplayName (hsPlateCrust sample)]
    modeLines ViewPlateAge sample = ["Age   " <> fmtF (hsPlateAge sample)]
    modeLines ViewPlateHeight sample = ["Height " <> fmtU (normToMetres units (hsPlateHeight sample)) "m"]
    modeLines ViewPlateVelocity sample =
      [ "Vel X " <> fmtF (hsPlateVelX sample)
      , "Vel Y " <> fmtF (hsPlateVelY sample)
      , "Speed " <> fmtF (sqrt (hsPlateVelX sample ** 2 + hsPlateVelY sample ** 2))
      ]
    modeLines ViewVegetation sample =
      [ "VCov  " <> fmtF (hsVegCover sample)
      , "VDen  " <> fmtF (hsVegDensity sample)
      , "Fert  " <> fmtF (hsFertility sample)
      , "Moist " <> fmtU (normToRH (hsMoisture sample)) "%"
      ]
    modeLines ViewTerrainForm sample =
      [ "Form  " <> Text.pack (terrainFormDisplayName (hsTerrainForm sample))
      , "Slope " <> fmtF (hsSlope sample)
      , "Elev  " <> fmtU (normToMetres units (hsElevation sample)) "m"
      ]
    modeLines ViewCloud sample =
      let pct v = fmtU (v * 100) "%"
          stormI = hsCloudWater sample * min 1 (hsWeatherPrecip sample * 3)
      in [ "Cloud " <> pct (hsCloudCover sample) <> "  Water " <> fmtF (hsCloudWater sample)
         , "  Low  " <> pct (hsCloudCoverLow sample) <> "  " <> fmtF (hsCloudWaterLow sample)
         , "  Mid  " <> pct (hsCloudCoverMid sample) <> "  " <> fmtF (hsCloudWaterMid sample)
         , "  High " <> pct (hsCloudCoverHigh sample) <> "  " <> fmtF (hsCloudWaterHigh sample)
         , "Precp " <> fmtU (normToMmYear units (hsWeatherPrecip sample)) "mm/yr"
         , "WindD " <> fmtU (hsWeatherWindDir sample) "rad"
         , "WindS " <> fmtU (normToWindMs units (hsWeatherWindSpd sample)) "m/s"
         , "Storm " <> fmtF stormI
         ] ++ solarLines q r
    modeLines (ViewOverlay overlayName fieldIndex) sample =
      case lookupOverlay overlayName (tsOverlayStore terrainSnap) of
        Nothing -> ["Overlay " <> overlayName, "(not loaded)"]
        Just overlay ->
          let schema = ovSchema overlay
              fields = osFields schema
              fieldHeader = case drop fieldIndex fields of
                (fd:_) -> ofdName fd
                []     -> "field " <> Text.pack (show fieldIndex)
              ChunkId key = chunkIdFromCoord (hsChunk sample)
          in ("Overlay " <> overlayName) : ("Field  " <> fieldHeader) :
               overlayValuesAt overlay key (tsChunkSize terrainSnap) sample fields

data HexSample = HexSample
  { hsChunk :: !ChunkCoord
  , hsLocal :: !TileCoord
  , hsElevation :: !Float
  , hsSlope :: !Float
  , hsDirSlope :: !DirectionalSlope
  , hsMoisture :: !Float
  , hsSoilDepth :: !Float
  , hsFertility :: !Float
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
          vegetationChunk = IntMap.lookup key (tsVegetationChunks terrainSnap)
          dirSlope = tcDirSlope terrainChunk U.! idx
      Just HexSample
        { hsChunk = chunkCoord
        , hsLocal = local
        , hsElevation = tcElevation terrainChunk U.! idx
        , hsSlope = dsAvgSlope dirSlope
        , hsDirSlope = dirSlope
        , hsMoisture = tcMoisture terrainChunk U.! idx
        , hsSoilDepth = tcSoilDepth terrainChunk U.! idx
        , hsFertility = tcFertility terrainChunk U.! idx
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
