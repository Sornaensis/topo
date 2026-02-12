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
    12 -> V4 220 210 170 255 -- coastal
    13 -> V4 150 150 160 255 -- alpine
    -- Forest sub-biomes
    14 -> V4 130 150  70 255 -- tropical dry forest
    15 -> V4  70 140  60 255 -- temperate deciduous forest
    16 -> V4  45 105  65 255 -- temperate coniferous forest
    -- Grassland sub-biomes
    17 -> V4 180 185 110 255 -- steppe
    -- Shrubland sub-biomes
    18 -> V4 170 155  90 255 -- mediterranean
    -- Swamp/Wetland sub-biomes
    19 -> V4  70 120 100 255 -- wetland
    -- Forest sub-biomes (continued)
    20 -> V4  50 115  75 255 -- montane forest
    -- Snow/Ice sub-biomes
    21 -> V4 220 230 245 255 -- ice cap
    -- Savanna sub-biomes
    22 -> V4 195 185 100 255 -- tropical savanna
    -- Taiga/Boreal sub-biomes
    23 -> V4  40  90  60 255 -- boreal forest
    -- Coastal sub-biomes
    24 -> V4 140 155 100 255 -- salt marsh
    25 -> V4 215 200 150 255 -- coastal dunes
    26 -> V4  60 100  70 255 -- mangrove
    27 -> V4 120 140 130 255 -- estuary
    28 -> V4 145 145 155 255 -- rocky shore
    29 -> V4 160 170 130 255 -- coastal scrub
    -- Desert sub-biomes
    30 -> V4 230 200 130 255 -- hot desert
    31 -> V4 195 185 165 255 -- cold desert
    32 -> V4 165 145 110 255 -- rocky desert
    33 -> V4 220 195 120 255 -- sand desert
    34 -> V4 230 230 220 255 -- salt flat
    -- Grassland sub-biomes (continued)
    35 -> V4 140 175  85 255 -- prairie
    36 -> V4 130 185 120 255 -- alpine meadow
    37 -> V4 110 165  80 255 -- floodplain grassland
    -- Forest sub-biomes (continued)
    38 -> V4  80 135 100 255 -- cloud forest
    39 -> V4  35 120  70 255 -- temperate rainforest
    -- Ocean sub-biomes
    40 -> V4  15  50 130 255 -- deep ocean
    41 -> V4  50 100 170 255 -- shallow sea
    42 -> V4  60 170 160 255 -- coral reef
    -- Tundra sub-biomes
    43 -> V4 190 205 210 255 -- arctic tundra
    44 -> V4 175 180 195 255 -- alpine tundra
    45 -> V4 210 210 215 255 -- polar desert
    -- Rainforest sub-biomes
    46 -> V4  20 100  50 255 -- tropical rainforest
    -- Shrubland sub-biomes (continued)
    47 -> V4 160 150 105 255 -- xeric shrubland
    48 -> V4 140 120 130 255 -- moorland
    -- Savanna sub-biomes (continued)
    49 -> V4 155 165  90 255 -- woodland savanna
    50 -> V4 185 180 105 255 -- grassland savanna
    -- Taiga/Boreal sub-biomes (continued)
    51 -> V4  75  95  80 255 -- boreal bog
    -- Swamp/Wetland sub-biomes (continued)
    52 -> V4 110 140 115 255 -- marsh
    53 -> V4 100  85  65 255 -- bog
    54 -> V4 115 135 120 255 -- fen
    55 -> V4  55 110  65 255 -- floodplain forest
    -- Snow/Ice sub-biomes (continued)
    56 -> V4 185 210 235 255 -- glacier
    57 -> V4 235 235 240 255 -- snowfield
    -- Alpine sub-biomes
    58 -> V4 160 155 150 255 -- alpine scree
    -- Volcanic family
    59 -> V4  80  30  20 255 -- lava field
    60 -> V4  95  90  85 255 -- volcanic ash plain
    -- Water body biomes
    61 -> V4  90 155 210 255 -- lake
    62 -> V4  55 120 170 255 -- inland sea
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
