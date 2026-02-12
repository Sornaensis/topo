-- | Helpers for building and slicing chunk-aligned terrain grids.
module Topo.TerrainGrid
  ( chunkCoordBounds
  , validateTerrainGrid
  , buildElevationGrid
  , buildHardnessGrid
  , buildMoistureGrid
  , buildClimateTempGrid
  , buildClimatePrecipGrid
  , buildPlateHardnessGrid
  , buildPlateBoundaryGrid
  , updateChunkElevationFromGrid
  , updateChunkMoistureFromGrid
  , chunkGridSlice
  , clampCoordGrid
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (find)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Topo.Types
import qualified Data.Vector.Unboxed as U

-- | Determine the bounding chunk coordinates for a non-empty chunk map.
chunkCoordBounds :: IntMap a -> Maybe (ChunkCoord, ChunkCoord)
chunkCoordBounds chunks
  | IntMap.null chunks = Nothing
  | otherwise =
      let (minCx, maxCx, minCy, maxCy) =
            IntMap.foldlWithKey'
              (\(minX, maxX, minY, maxY) key _ ->
                let ChunkCoord cx cy = chunkCoordFromId (ChunkId key)
                in (min minX cx, max maxX cx, min minY cy, max maxY cy))
              (maxBound, minBound, maxBound, minBound)
              chunks
      in Just (ChunkCoord minCx minCy, ChunkCoord maxCx maxCy)

-- | Validate that a terrain chunk map is contiguous and tile-aligned.
validateTerrainGrid :: WorldConfig -> IntMap TerrainChunk -> Either Text (ChunkCoord, ChunkCoord)
validateTerrainGrid config terrain =
  case chunkCoordBounds terrain of
    Nothing -> Left (Text.pack "terrain grid: empty")
    Just bounds@(ChunkCoord minCx minCy, ChunkCoord maxCx maxCy) -> do
      let missing =
            find
              (\coord ->
                let ChunkId key = chunkIdFromCoord coord
                in not (IntMap.member key terrain))
              [ ChunkCoord cx cy
              | cy <- [minCy .. maxCy]
              , cx <- [minCx .. maxCx]
              ]
      case missing of
        Just coord -> Left (Text.pack ("terrain grid: missing chunk " <> show coord))
        Nothing -> do
          let expected = chunkTileCount config
          case firstChunkError expected (IntMap.toList terrain) of
            Just err -> Left err
            Nothing -> Right bounds

firstChunkError :: Int -> [(Int, TerrainChunk)] -> Maybe Text
firstChunkError expected = listToMaybe . foldr step []
  where
    step entry acc =
      case chunkError expected entry of
        Nothing -> acc
        Just err -> err : acc

chunkError :: Int -> (Int, TerrainChunk) -> Maybe Text
chunkError expected (key, chunk) =
  let check label actual =
        if actual == expected
          then Nothing
          else Just (Text.pack ("terrain chunk " <> show key <> " " <> label <> " length " <> show actual <> ", expected " <> show expected))
      fields =
        [ ("elevation", U.length (tcElevation chunk))
        , ("slope", U.length (tcSlope chunk))
        , ("curvature", U.length (tcCurvature chunk))
        , ("hardness", U.length (tcHardness chunk))
        , ("rockType", U.length (tcRockType chunk))
        , ("soilType", U.length (tcSoilType chunk))
        , ("soilDepth", U.length (tcSoilDepth chunk))
        , ("moisture", U.length (tcMoisture chunk))
        , ("fertility", U.length (tcFertility chunk))
        , ("roughness", U.length (tcRoughness chunk))
        , ("rockDensity", U.length (tcRockDensity chunk))
        , ("soilGrain", U.length (tcSoilGrain chunk))
        , ("relief", U.length (tcRelief chunk))
        , ("ruggedness", U.length (tcRuggedness chunk))
        , ("terrainForm", U.length (tcTerrainForm chunk))
        , ("flags", U.length (tcFlags chunk))
        , ("plateId", U.length (tcPlateId chunk))
        , ("plateBoundary", U.length (tcPlateBoundary chunk))
        , ("plateHeight", U.length (tcPlateHeight chunk))
        , ("plateHardness", U.length (tcPlateHardness chunk))
        , ("plateCrust", U.length (tcPlateCrust chunk))
        , ("plateAge", U.length (tcPlateAge chunk))
        , ("plateVelX", U.length (tcPlateVelX chunk))
        , ("plateVelY", U.length (tcPlateVelY chunk))
        ]
  in listToMaybe (mapMaybe (uncurry check) fields)

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = foldr (\x acc -> maybe acc (: acc) (f x)) []

-- | Build a full elevation grid over a contiguous chunk rectangle.
buildElevationGrid :: WorldConfig -> IntMap TerrainChunk -> ChunkCoord -> Int -> Int -> U.Vector Float
buildElevationGrid config terrain (ChunkCoord minCx minCy) gridW gridH =
  let size = wcChunkSize config
      minTileX = minCx * size
      minTileY = minCy * size
      sampleAt idx =
        let x = idx `mod` gridW
            y = idx `div` gridW
            gx = minTileX + x
            gy = minTileY + y
            tile = TileCoord gx gy
            (chunkCoord, local) = chunkCoordFromTile config tile
            ChunkId key = chunkIdFromCoord chunkCoord
        in case IntMap.lookup key terrain of
            Nothing -> 0
            Just chunk ->
              case tileIndex config local of
                Nothing -> 0
                Just (TileIndex i) -> tcElevation chunk U.! i
  in U.generate (gridW * gridH) sampleAt

-- | Build a full hardness grid over a contiguous chunk rectangle.
buildHardnessGrid :: WorldConfig -> IntMap TerrainChunk -> ChunkCoord -> Int -> Int -> U.Vector Float
buildHardnessGrid config terrain (ChunkCoord minCx minCy) gridW gridH =
  let size = wcChunkSize config
      minTileX = minCx * size
      minTileY = minCy * size
      sampleAt idx =
        let x = idx `mod` gridW
            y = idx `div` gridW
            gx = minTileX + x
            gy = minTileY + y
            tile = TileCoord gx gy
            (chunkCoord, local) = chunkCoordFromTile config tile
            ChunkId key = chunkIdFromCoord chunkCoord
        in case IntMap.lookup key terrain of
            Nothing -> 0
            Just chunk ->
              case tileIndex config local of
                Nothing -> 0
                Just (TileIndex i) -> tcHardness chunk U.! i
  in U.generate (gridW * gridH) sampleAt

-- | Build a full moisture grid over a contiguous chunk rectangle.
buildMoistureGrid :: WorldConfig -> IntMap TerrainChunk -> ChunkCoord -> Int -> Int -> U.Vector Float
buildMoistureGrid config terrain (ChunkCoord minCx minCy) gridW gridH =
  let size = wcChunkSize config
      minTileX = minCx * size
      minTileY = minCy * size
      sampleAt idx =
        let x = idx `mod` gridW
            y = idx `div` gridW
            gx = minTileX + x
            gy = minTileY + y
            tile = TileCoord gx gy
            (chunkCoord, local) = chunkCoordFromTile config tile
            ChunkId key = chunkIdFromCoord chunkCoord
        in case IntMap.lookup key terrain of
            Nothing -> 0
            Just chunk ->
              case tileIndex config local of
                Nothing -> 0
                Just (TileIndex i) -> tcMoisture chunk U.! i
  in U.generate (gridW * gridH) sampleAt

-- | Build a full climate temperature grid over a contiguous chunk rectangle.
buildClimateTempGrid :: WorldConfig -> IntMap ClimateChunk -> ChunkCoord -> Int -> Int -> U.Vector Float
buildClimateTempGrid config climate (ChunkCoord minCx minCy) gridW gridH =
  let size = wcChunkSize config
      minTileX = minCx * size
      minTileY = minCy * size
      sampleAt idx =
        let x = idx `mod` gridW
            y = idx `div` gridW
            gx = minTileX + x
            gy = minTileY + y
            tile = TileCoord gx gy
            (chunkCoord, local) = chunkCoordFromTile config tile
            ChunkId key = chunkIdFromCoord chunkCoord
        in case IntMap.lookup key climate of
            Nothing -> 0
            Just chunk ->
              case tileIndex config local of
                Nothing -> 0
                Just (TileIndex i) -> ccTempAvg chunk U.! i
  in U.generate (gridW * gridH) sampleAt

-- | Build a full climate precipitation grid over a contiguous chunk rectangle.
buildClimatePrecipGrid :: WorldConfig -> IntMap ClimateChunk -> ChunkCoord -> Int -> Int -> U.Vector Float
buildClimatePrecipGrid config climate (ChunkCoord minCx minCy) gridW gridH =
  let size = wcChunkSize config
      minTileX = minCx * size
      minTileY = minCy * size
      sampleAt idx =
        let x = idx `mod` gridW
            y = idx `div` gridW
            gx = minTileX + x
            gy = minTileY + y
            tile = TileCoord gx gy
            (chunkCoord, local) = chunkCoordFromTile config tile
            ChunkId key = chunkIdFromCoord chunkCoord
        in case IntMap.lookup key climate of
            Nothing -> 0
            Just chunk ->
              case tileIndex config local of
                Nothing -> 0
                Just (TileIndex i) -> ccPrecipAvg chunk U.! i
  in U.generate (gridW * gridH) sampleAt

-- | Build a full plate-hardness grid over a contiguous chunk rectangle.
buildPlateHardnessGrid :: WorldConfig -> IntMap TerrainChunk -> ChunkCoord -> Int -> Int -> U.Vector Float
buildPlateHardnessGrid config terrain (ChunkCoord minCx minCy) gridW gridH =
  let size = wcChunkSize config
      minTileX = minCx * size
      minTileY = minCy * size
      sampleAt idx =
        let x = idx `mod` gridW
            y = idx `div` gridW
            gx = minTileX + x
            gy = minTileY + y
            tile = TileCoord gx gy
            (chunkCoord, local) = chunkCoordFromTile config tile
            ChunkId key = chunkIdFromCoord chunkCoord
        in case IntMap.lookup key terrain of
            Nothing -> 0
            Just chunk ->
              case tileIndex config local of
                Nothing -> 0
                Just (TileIndex i) -> tcPlateHardness chunk U.! i
  in U.generate (gridW * gridH) sampleAt

-- | Build a full plate-boundary grid over a contiguous chunk rectangle.
buildPlateBoundaryGrid :: WorldConfig -> IntMap TerrainChunk -> ChunkCoord -> Int -> Int -> U.Vector PlateBoundary
buildPlateBoundaryGrid config terrain (ChunkCoord minCx minCy) gridW gridH =
  let size = wcChunkSize config
      minTileX = minCx * size
      minTileY = minCy * size
      sampleAt idx =
        let x = idx `mod` gridW
            y = idx `div` gridW
            gx = minTileX + x
            gy = minTileY + y
            tile = TileCoord gx gy
            (chunkCoord, local) = chunkCoordFromTile config tile
            ChunkId key = chunkIdFromCoord chunkCoord
        in case IntMap.lookup key terrain of
            Nothing -> PlateBoundaryNone
            Just chunk ->
              case tileIndex config local of
                Nothing -> PlateBoundaryNone
                Just (TileIndex i) -> tcPlateBoundary chunk U.! i
  in U.generate (gridW * gridH) sampleAt

-- | Update a chunk elevation from a contiguous grid.
updateChunkElevationFromGrid
  :: WorldConfig
  -> ChunkCoord
  -> Int
  -> U.Vector Float
  -> Int
  -> TerrainChunk
  -> TerrainChunk
updateChunkElevationFromGrid config (ChunkCoord minCx minCy) gridW grid key chunk =
  let ChunkCoord cx cy = chunkCoordFromId (ChunkId key)
      size = wcChunkSize config
      baseX = (cx - minCx) * size
      baseY = (cy - minCy) * size
      n = size * size
      newElev = U.generate n (\i ->
        let x = i `mod` size
            y = i `div` size
            gx = baseX + x
            gy = baseY + y
            gi = gy * gridW + gx
        in grid U.! gi)
  in chunk { tcElevation = newElev }

-- | Update a chunk moisture field from a contiguous grid.
updateChunkMoistureFromGrid
  :: WorldConfig
  -> ChunkCoord
  -> Int
  -> U.Vector Float
  -> Int
  -> TerrainChunk
  -> TerrainChunk
updateChunkMoistureFromGrid config (ChunkCoord minCx minCy) gridW grid key chunk =
  let ChunkCoord cx cy = chunkCoordFromId (ChunkId key)
      size = wcChunkSize config
      baseX = (cx - minCx) * size
      baseY = (cy - minCy) * size
      n = size * size
      newMoisture = U.generate n (\i ->
        let x = i `mod` size
            y = i `div` size
            gx = baseX + x
            gy = baseY + y
            gi = gy * gridW + gx
        in grid U.! gi)
  in chunk { tcMoisture = newMoisture }

-- | Slice a chunk-sized vector from a large grid.
chunkGridSlice :: WorldConfig -> ChunkCoord -> Int -> U.Vector Float -> Int -> U.Vector Float
chunkGridSlice config (ChunkCoord minCx minCy) gridW grid key =
  let ChunkCoord cx cy = chunkCoordFromId (ChunkId key)
      size = wcChunkSize config
      baseX = (cx - minCx) * size
      baseY = (cy - minCy) * size
      n = size * size
  in U.generate n (\i ->
      let x = i `mod` size
          y = i `div` size
          gx = baseX + x
          gy = baseY + y
          gi = gy * gridW + gx
      in grid U.! gi)

-- | Clamp an index to the inclusive grid bounds.
clampCoordGrid :: Int -> Int -> Int
clampCoordGrid size v
  | v < 0 = 0
  | v >= size = size - 1
  | otherwise = v
