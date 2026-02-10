module Topo.Sample
  ( sampleHeight
  , sampleNormal
  , sampleTerrain
  , interpSample
  , hexAt
  , hexData
  , hexSample
  ) where

import Topo.Hex (hexToWorld, worldToHex)
import Topo.Types
import Topo.World
import qualified Data.Vector.Unboxed as U

sampleHeight :: TerrainWorld -> WorldPos -> Maybe Float
sampleHeight world pos =
  let config = twConfig world
      tile = worldToTile pos
      (chunkCoord, local) = chunkCoordFromTile config tile
      chunkId = chunkIdFromCoord chunkCoord
  in getElevationAt chunkId local world

sampleNormal :: TerrainWorld -> WorldPos -> Vec3
sampleNormal world (WorldPos x y) =
  let h0 = heightAt (WorldPos x y)
      hx = heightAt (WorldPos (x + 1) y)
      hy = heightAt (WorldPos x (y + 1))
      dx = hx - h0
      dy = hy - h0
  in normalize (Vec3 (-dx) (-dy) 1)
  where
    heightAt pos = maybe 0 id (sampleHeight world pos)

sampleTerrain :: TerrainWorld -> WorldPos -> TerrainSample
sampleTerrain world pos =
  let config = twConfig world
      tile = worldToTile pos
      (chunkCoord, local) = chunkCoordFromTile config tile
      chunkId = chunkIdFromCoord chunkCoord
  in sampleAt chunkId local world

interpSample :: TerrainWorld -> WorldPos -> TerrainSample
interpSample world (WorldPos x y) =
  let ix = floor x
      iy = floor y
      fx = x - fromIntegral ix
      fy = y - fromIntegral iy
      s00 = sampleAtTile world (TileCoord ix iy)
      s10 = sampleAtTile world (TileCoord (ix + 1) iy)
      s01 = sampleAtTile world (TileCoord ix (iy + 1))
      s11 = sampleAtTile world (TileCoord (ix + 1) (iy + 1))
  in bilerpSample fx fy s00 s10 s01 s11

hexAt :: TerrainWorld -> WorldPos -> HexCoord
hexAt world pos = worldToHex (twHexGrid world) pos

hexData :: TerrainWorld -> HexCoord -> TerrainSample
hexData world coord =
  let pos = hexToWorld (twHexGrid world) coord
  in sampleTerrain world pos

hexSample :: TerrainWorld -> HexCoord -> TerrainSample
hexSample = hexData

worldToTile :: WorldPos -> TileCoord
worldToTile (WorldPos x y) = TileCoord (floor x) (floor y)

sampleAt :: ChunkId -> TileCoord -> TerrainWorld -> TerrainSample
sampleAt chunkId coord world =
  let config = twConfig world
      terrain = getTerrainChunk chunkId world
      climate = getClimateChunk chunkId world
      weather = getWeatherChunk chunkId world
  in case tileIndex config coord of
      Nothing -> zeroSample
      Just (TileIndex i) ->
        let t = terrain
            c = climate
            w = weather
        in TerrainSample
            { tsElevation = fieldOr 0 (tcElevation <$> t) i
            , tsSlope = fieldOr 0 (tcSlope <$> t) i
            , tsCurvature = fieldOr 0 (tcCurvature <$> t) i
            , tsHardness = fieldOr 0 (tcHardness <$> t) i
            , tsSoilDepth = fieldOr 0 (tcSoilDepth <$> t) i
            , tsMoisture = fieldOr 0 (tcMoisture <$> t) i
            , tsFertility = fieldOr 0 (tcFertility <$> t) i
            , tsRoughness = fieldOr 0 (tcRoughness <$> t) i
            , tsRockDensity = fieldOr 0 (tcRockDensity <$> t) i
            , tsSoilGrain = fieldOr 0 (tcSoilGrain <$> t) i
            , tsTemperature = fieldOr 0 (ccTempAvg <$> c) i
            , tsHumidity = fieldOr 0 (wcHumidity <$> w) i
            , tsWindSpeed = fieldOr 0 (wcWindSpd <$> w) i
            , tsPressure = fieldOr 0 (wcPressure <$> w) i
            , tsPrecip = fieldOr 0 (wcPrecip <$> w) i
            }

fieldOr :: Float -> Maybe (U.Vector Float) -> Int -> Float
fieldOr fallback vec idx =
  case vec of
    Nothing -> fallback
    Just v -> v U.! idx

zeroSample :: TerrainSample
zeroSample = TerrainSample 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

normalize :: Vec3 -> Vec3
normalize (Vec3 x y z) =
  let len = sqrt (x * x + y * y + z * z)
  in if len <= 0
      then Vec3 0 0 1
      else Vec3 (x / len) (y / len) (z / len)

sampleAtTile :: TerrainWorld -> TileCoord -> TerrainSample
sampleAtTile world tile =
  let config = twConfig world
      (chunkCoord, local) = chunkCoordFromTile config tile
      chunkId = chunkIdFromCoord chunkCoord
  in sampleAt chunkId local world

bilerpSample :: Float -> Float -> TerrainSample -> TerrainSample -> TerrainSample -> TerrainSample -> TerrainSample
bilerpSample fx fy s00 s10 s01 s11 =
  let lerp a b t = a + (b - a) * t
      mix a b = lerp a b fx
      blend a b = lerp a b fy
      b2 a00 a10 a01 a11 = blend (mix a00 a10) (mix a01 a11)
  in TerrainSample
      { tsElevation = b2 (tsElevation s00) (tsElevation s10) (tsElevation s01) (tsElevation s11)
      , tsSlope = b2 (tsSlope s00) (tsSlope s10) (tsSlope s01) (tsSlope s11)
      , tsCurvature = b2 (tsCurvature s00) (tsCurvature s10) (tsCurvature s01) (tsCurvature s11)
      , tsHardness = b2 (tsHardness s00) (tsHardness s10) (tsHardness s01) (tsHardness s11)
      , tsSoilDepth = b2 (tsSoilDepth s00) (tsSoilDepth s10) (tsSoilDepth s01) (tsSoilDepth s11)
      , tsMoisture = b2 (tsMoisture s00) (tsMoisture s10) (tsMoisture s01) (tsMoisture s11)
      , tsFertility = b2 (tsFertility s00) (tsFertility s10) (tsFertility s01) (tsFertility s11)
      , tsRoughness = b2 (tsRoughness s00) (tsRoughness s10) (tsRoughness s01) (tsRoughness s11)
      , tsRockDensity = b2 (tsRockDensity s00) (tsRockDensity s10) (tsRockDensity s01) (tsRockDensity s11)
      , tsSoilGrain = b2 (tsSoilGrain s00) (tsSoilGrain s10) (tsSoilGrain s01) (tsSoilGrain s11)
      , tsTemperature = b2 (tsTemperature s00) (tsTemperature s10) (tsTemperature s01) (tsTemperature s11)
      , tsHumidity = b2 (tsHumidity s00) (tsHumidity s10) (tsHumidity s01) (tsHumidity s11)
      , tsWindSpeed = b2 (tsWindSpeed s00) (tsWindSpeed s10) (tsWindSpeed s01) (tsWindSpeed s11)
      , tsPressure = b2 (tsPressure s00) (tsPressure s10) (tsPressure s01) (tsPressure s11)
      , tsPrecip = b2 (tsPrecip s00) (tsPrecip s10) (tsPrecip s01) (tsPrecip s11)
      }
