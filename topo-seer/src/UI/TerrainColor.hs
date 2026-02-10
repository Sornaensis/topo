module UI.TerrainColor
  ( terrainColor
  , gradientBlueGreen
  , gradientHeat
  , gradientMoisture
  , paletteById
  ) where

import Data.Word (Word8, Word16)
import Linear (V4(..))
import qualified Data.Vector.Unboxed as U
import Topo (BiomeId, PlateBoundary, ClimateChunk(..), TerrainChunk(..), WeatherChunk(..), biomeIdToCode, plateBoundaryToCode)
import Actor.UI (ViewMode(..))

terrainColor :: ViewMode -> Float -> TerrainChunk -> Maybe ClimateChunk -> Maybe WeatherChunk -> Int -> V4 Word8
terrainColor mode waterLevel chunk climateChunk weatherChunk idx =
  case mode of
    ViewElevation -> elevationColor waterLevel (tcElevation chunk U.! idx)
    ViewBiome -> paletteById (biomeIdToCode (tcFlags chunk U.! idx))
    ViewClimate ->
      let value = maybe 0 (\c -> ccTempAvg c U.! idx) climateChunk
      in gradientHeat value
    ViewMoisture -> gradientMoisture (tcMoisture chunk U.! idx)
    ViewPrecip ->
      let value = maybe 0 (\c -> ccPrecipAvg c U.! idx) climateChunk
      in gradientMoisture value
    ViewPlateId -> paletteById (tcPlateId chunk U.! idx)
    ViewPlateBoundary -> boundaryColor (plateBoundaryToCode (tcPlateBoundary chunk U.! idx))
    ViewPlateHardness -> gradientHeat (tcPlateHardness chunk U.! idx)
    ViewPlateCrust -> crustColor (tcPlateCrust chunk U.! idx)
    ViewPlateAge -> gradientHeat (tcPlateAge chunk U.! idx)
    ViewPlateHeight -> gradientBlueGreen (tcPlateHeight chunk U.! idx)
    ViewPlateVelocity -> gradientHeat (plateVelocityMag chunk idx)

elevationColor :: Float -> Float -> V4 Word8
elevationColor waterLevel elev
  | elev < waterLevel =
      let depth = clamp01 ((waterLevel - elev) / 0.2)
          r = toByte (0.08 + depth * 0.05)
          g = toByte (0.25 + depth * 0.15)
          b = toByte (0.55 + depth * 0.35)
      in V4 r g b 255
  | otherwise =
      let scaled = clamp01 ((elev - waterLevel) / max 0.001 (1 - waterLevel))
      in gradientBlueGreen scaled

gradientBlueGreen :: Float -> V4 Word8
gradientBlueGreen value =
  let v = clamp01 value
      r = toByte (v * 0.4)
      g = toByte (0.3 + v * 0.7)
      b = toByte (0.6 + v * 0.4)
  in V4 r g b 255

gradientHeat :: Float -> V4 Word8
gradientHeat value =
  let v = clamp01 value
      r = toByte (0.5 + v * 0.5)
      g = toByte (0.2 + v * 0.4)
      b = toByte (0.2 + v * 0.2)
  in V4 r g b 255

gradientMoisture :: Float -> V4 Word8
gradientMoisture value =
  let v = clamp01 value
      r = toByte (0.1 + v * 0.2)
      g = toByte (0.3 + v * 0.5)
      b = toByte (0.4 + v * 0.6)
  in V4 r g b 255

paletteById :: Word16 -> V4 Word8
paletteById biomeId =
  case biomeId of
    0 -> V4 210 190 120 255 -- desert
    1 -> V4 120 170 90 255 -- grassland
    2 -> V4 50 120 60 255 -- forest
    3 -> V4 130 140 150 255 -- tundra
    4 -> V4 40 110 80 255 -- rainforest
    5 -> V4 160 150 110 255 -- shrubland
    6 -> V4 180 170 90 255 -- savanna
    7 -> V4 60 110 80 255 -- taiga
    8 -> V4 70 130 110 255 -- swamp
    10 -> V4 30 80 160 255 -- ocean
    11 -> V4 230 235 240 255 -- snow
    12 -> V4 220 210 170 255 -- beach
    13 -> V4 150 150 160 255 -- alpine
    _ -> V4 90 140 90 255

boundaryColor :: Word16 -> V4 Word8
boundaryColor boundaryId =
  case boundaryId of
    1 -> V4 210 80 70 255
    2 -> V4 60 160 210 255
    3 -> V4 200 200 90 255
    _ -> V4 40 40 45 255

crustColor :: Word16 -> V4 Word8
crustColor crustId =
  case crustId of
    1 -> V4 160 140 110 255
    _ -> V4 40 90 160 255

plateVelocityMag :: TerrainChunk -> Int -> Float
plateVelocityMag chunk idx =
  let vx = tcPlateVelX chunk U.! idx
      vy = tcPlateVelY chunk U.! idx
  in clamp01 (sqrt (vx * vx + vy * vy))

clamp01 :: Float -> Float
clamp01 value =
  max 0 (min 1 value)

toByte :: Float -> Word8
toByte value =
  fromIntegral (round (clamp01 value * 255))
