{-# LANGUAGE BangPatterns #-}

-- | Helpers for building and slicing chunk-aligned terrain grids.
module Topo.TerrainGrid
  ( chunkCoordBounds
  , validateTerrainGrid
  , buildElevationGrid
  , buildHardnessGrid
  , buildMoistureGrid
  , buildClimateTempGrid
  , buildClimatePrecipGrid
  , buildPlateHeightGrid
  , buildPlateHardnessGrid
  , buildPlateBoundaryGrid
  , buildSlopeGrid
  , buildMaxSlopeGrid
  , buildSoilDepthGrid
  , buildSoilGrainGrid
  , buildSoilTypeGrid
  , updateChunkElevationFromGrid
  , updateChunkMoistureFromGrid
  , chunkGridSlice
  , chunkGridSliceGeneric
  , clampCoordGrid
    -- * Grid-level slope helpers
  , gridSlopeAt
    -- * Grid-level terrain form classification
  , classifyTerrainFormGrid
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (find)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word16)
import Topo.Hex (hexNeighborIndices)
import Topo.Math (clamp01)
import Topo.Parameters (classifyTerrainForm, computeReliefIndex, TerrainFormConfig)
import qualified Topo.TerrainForm.Metrics as TerrainMetrics
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
        , ("dirSlope", U.length (tcDirSlope chunk))
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
        , ("microRelief", U.length (tcMicroRelief chunk))
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

buildChunkGrid
  :: U.Unbox a
  => WorldConfig
  -> IntMap chunk
  -> ChunkCoord
  -> Int
  -> Int
  -> a
  -> (chunk -> Int -> a)
  -> U.Vector a
buildChunkGrid config chunks (ChunkCoord minCx minCy) gridW gridH fallback valueAt =
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
        in case IntMap.lookup key chunks of
            Nothing -> fallback
            Just chunk ->
              case tileIndex config local of
                Nothing -> fallback
                Just (TileIndex i) -> valueAt chunk i
  in U.generate (gridW * gridH) sampleAt

-- | Build a full elevation grid over a contiguous chunk rectangle.
buildElevationGrid :: WorldConfig -> IntMap TerrainChunk -> ChunkCoord -> Int -> Int -> U.Vector Float
buildElevationGrid config terrain minCoord gridW gridH =
  buildChunkGrid config terrain minCoord gridW gridH 0 (\chunk i -> tcElevation chunk U.! i)

-- | Build a full hardness grid over a contiguous chunk rectangle.
buildHardnessGrid :: WorldConfig -> IntMap TerrainChunk -> ChunkCoord -> Int -> Int -> U.Vector Float
buildHardnessGrid config terrain minCoord gridW gridH =
  buildChunkGrid config terrain minCoord gridW gridH 0 (\chunk i -> tcHardness chunk U.! i)

-- | Build a full moisture grid over a contiguous chunk rectangle.
buildMoistureGrid :: WorldConfig -> IntMap TerrainChunk -> ChunkCoord -> Int -> Int -> U.Vector Float
buildMoistureGrid config terrain minCoord gridW gridH =
  buildChunkGrid config terrain minCoord gridW gridH 0 (\chunk i -> tcMoisture chunk U.! i)

-- | Build a full climate temperature grid over a contiguous chunk rectangle.
buildClimateTempGrid :: WorldConfig -> IntMap ClimateChunk -> ChunkCoord -> Int -> Int -> U.Vector Float
buildClimateTempGrid config climate minCoord gridW gridH =
  buildChunkGrid config climate minCoord gridW gridH 0 (\chunk i -> ccTempAvg chunk U.! i)

-- | Build a full climate precipitation grid over a contiguous chunk rectangle.
buildClimatePrecipGrid :: WorldConfig -> IntMap ClimateChunk -> ChunkCoord -> Int -> Int -> U.Vector Float
buildClimatePrecipGrid config climate minCoord gridW gridH =
  buildChunkGrid config climate minCoord gridW gridH 0 (\chunk i -> ccPrecipAvg chunk U.! i)

-- | Build a full plate-height grid over a contiguous chunk rectangle.
buildPlateHeightGrid :: WorldConfig -> IntMap TerrainChunk -> ChunkCoord -> Int -> Int -> U.Vector Float
buildPlateHeightGrid config terrain minCoord gridW gridH =
  buildChunkGrid config terrain minCoord gridW gridH 0 (\chunk i -> tcPlateHeight chunk U.! i)

-- | Build a full plate-hardness grid over a contiguous chunk rectangle.
buildPlateHardnessGrid :: WorldConfig -> IntMap TerrainChunk -> ChunkCoord -> Int -> Int -> U.Vector Float
buildPlateHardnessGrid config terrain minCoord gridW gridH =
  buildChunkGrid config terrain minCoord gridW gridH 0 (\chunk i -> tcPlateHardness chunk U.! i)

-- | Build a full plate-boundary grid over a contiguous chunk rectangle.
buildPlateBoundaryGrid :: WorldConfig -> IntMap TerrainChunk -> ChunkCoord -> Int -> Int -> U.Vector PlateBoundary
buildPlateBoundaryGrid config terrain minCoord gridW gridH =
  buildChunkGrid config terrain minCoord gridW gridH PlateBoundaryNone (\chunk i -> tcPlateBoundary chunk U.! i)

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
chunkGridSlice = chunkGridSliceGeneric

-- | Slice a chunk-sized vector of any unboxed element type from a large grid.
chunkGridSliceGeneric :: U.Unbox a => WorldConfig -> ChunkCoord -> Int -> U.Vector a -> Int -> U.Vector a
chunkGridSliceGeneric config (ChunkCoord minCx minCy) gridW grid key =
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

---------------------------------------------------------------------------
-- Soil and parameter grid builders
---------------------------------------------------------------------------

-- | Build a full average-slope grid over a contiguous chunk rectangle.
--   Extracts 'dsAvgSlope' from each tile's 'DirectionalSlope'.
buildSlopeGrid :: WorldConfig -> IntMap TerrainChunk -> ChunkCoord -> Int -> Int -> U.Vector Float
buildSlopeGrid config terrain minCoord gridW gridH =
  buildChunkGrid config terrain minCoord gridW gridH 0 (\chunk i -> dsAvgSlope (tcDirSlope chunk U.! i))

-- | Build a full max-slope grid over a contiguous chunk rectangle.
--   Extracts 'dsMaxSlope' from each tile's 'DirectionalSlope'.
--
--   Useful for infiltration penalties where the steepest direction
--   determines runoff behaviour.
buildMaxSlopeGrid :: WorldConfig -> IntMap TerrainChunk -> ChunkCoord -> Int -> Int -> U.Vector Float
buildMaxSlopeGrid config terrain minCoord gridW gridH =
  buildChunkGrid config terrain minCoord gridW gridH 0 (\chunk i -> dsMaxSlope (tcDirSlope chunk U.! i))

-- | Build a full soil-depth grid over a contiguous chunk rectangle.
buildSoilDepthGrid :: WorldConfig -> IntMap TerrainChunk -> ChunkCoord -> Int -> Int -> U.Vector Float
buildSoilDepthGrid config terrain minCoord gridW gridH =
  buildChunkGrid config terrain minCoord gridW gridH 0 (\chunk i -> tcSoilDepth chunk U.! i)

-- | Build a full soil-grain grid over a contiguous chunk rectangle.
buildSoilGrainGrid :: WorldConfig -> IntMap TerrainChunk -> ChunkCoord -> Int -> Int -> U.Vector Float
buildSoilGrainGrid config terrain minCoord gridW gridH =
  buildChunkGrid config terrain minCoord gridW gridH 0 (\chunk i -> tcSoilGrain chunk U.! i)

-- | Build a full soil-type grid over a contiguous chunk rectangle.
buildSoilTypeGrid :: WorldConfig -> IntMap TerrainChunk -> ChunkCoord -> Int -> Int -> U.Vector Word16
buildSoilTypeGrid config terrain minCoord gridW gridH =
  buildChunkGrid config terrain minCoord gridW gridH 0 (\chunk i -> tcSoilType chunk U.! i)

---------------------------------------------------------------------------
-- Grid-level slope helpers
---------------------------------------------------------------------------

-- | Maximum absolute elevation difference to any of the 6 hex neighbours.
--
-- Equivalent to 'dsMaxSlope' when computed from the same elevation field,
-- but works on a flat grid without pre-computed 'DirectionalSlope'.
-- Shared by 'Topo.Erosion', 'Topo.Glacier', and 'Topo.Hydrology' to
-- avoid duplicate implementations.
gridSlopeAt :: Int -> Int -> U.Vector Float -> Int -> Float
gridSlopeAt gridW gridH elev i =
  let h0 = elev U.! i
      nbrs = hexNeighborIndices gridW gridH i
  in case nbrs of
       [] -> 0
       _  -> maximum [abs (elev U.! j - h0) | j <- nbrs]

---------------------------------------------------------------------------
-- Grid-level terrain form classification
---------------------------------------------------------------------------

-- | Classify terrain form for every tile on a flat elevation + hardness
-- grid.
--
-- This is a lightweight pre-classification used by erosion, glacier, and
-- hydrology stages /before/ the definitive per-chunk stencil classification
-- in 'applyParameterLayersStage'.  The two classifications use identical
-- logic ('classifyTerrainForm').  Boundary reads use nearest-valid sampling
-- (edge replication), so metrics remain deterministic without injecting
-- synthetic zero-slope edges.
--
-- Soil depth is unavailable at pre-classification time and is set to a
-- neutral default (0.5).  Since 'classifyTerrainForm' does not currently
-- weight soil depth in its cascade, this has no effect on classification
-- results.
--
-- Micro-relief uses the shared 'computeReliefIndex' in ring-only fallback
-- mode.  Post-erosion synthesis can additionally fuse noise and erosion
-- components, which are unavailable at this stage.
classifyTerrainFormGrid
  :: TerrainFormConfig
  -> Float             -- ^ Water level (for elevation ASL)
  -> Int               -- ^ Grid width
  -> Int               -- ^ Grid height
  -> U.Vector Float    -- ^ Elevation grid
  -> U.Vector Float    -- ^ Hardness grid
  -> U.Vector TerrainForm
classifyTerrainFormGrid formCfg waterLevel gridW gridH elev hardness =
  U.generate n $ \i ->
    let !h0 = elev U.! i
        !x  = i `mod` gridW
        !y  = i `div` gridW
        {-# INLINE elevAtClamped #-}
        elevAtClamped gx gy =
          let !cx = clampCoordGrid gridW gx
              !cy = clampCoordGrid gridH gy
          in elev U.! (cy * gridW + cx)

        !metrics = TerrainMetrics.terrainNeighborhoodAt elevAtClamped x y
        !ds = TerrainMetrics.tnDirectionalSlope metrics
        !c = TerrainMetrics.tnCurvature metrics
        !r = TerrainMetrics.tnRelief metrics
        !r2 = TerrainMetrics.tnRelief2Ring metrics
        !r3 = TerrainMetrics.tnRelief3Ring metrics
        !localMin = TerrainMetrics.tnIsLocalMinimum metrics
        !microRelief = computeReliefIndex r r2 r3 Nothing Nothing 0 0

        -- Substrate
        !hard    = hardness U.! i
        !elevASL = h0 - waterLevel

    in classifyTerrainForm formCfg ds r r2 r3 c localMin hard microRelief elevASL
  where
    !n = U.length elev
