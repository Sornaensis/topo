{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Spec.WorldGenValidation (spec) where

import Control.Monad (forM, forM_, unless)
import Data.Aeson (encode)
import Data.Bits ((.&.), (.|.), xor)
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.List (intercalate, sort, sortBy)
import Data.Ord (Down(..), comparing)
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Word (Word64)
import Numeric (showHex)
import System.Environment (lookupEnv)
import Test.Hspec
import Text.Printf (printf)
import qualified Data.Vector.Unboxed as U

import Topo
import Topo.TerrainGrid (buildElevationGrid, validateTerrainGrid)

spec :: Spec
spec = describe "WorldGen validation" $ do
  it "computes deterministic topology metrics for default seed sweeps" $ do
    sweepEnabled <- isSweepEnabled
    let seeds = if sweepEnabled then sweepSeeds else smokeSeeds
        modeLabel = if sweepEnabled then "full sweep" else "CI smoke"

    topologyRuns <- forM validationPresets $ \preset ->
      forM seeds $ runValidationCase StageTopology preset
    let topologyMetrics = concatMap (map vrMetrics) topologyRuns

    repeated <- runValidationCase StageTopology defaultValidationPreset baselineSeed
    case topologyMetrics of
      firstMetric : _ -> vrMetrics repeated `shouldBe` firstMetric
      [] -> expectationFailure "world-gen validation produced no metrics"

    assertStructuralMetrics topologyMetrics
    putStrLn (renderValidationReport modeLabel topologyMetrics)

  it "guards final full-pipeline land topology for configured guard seeds" $ do
    sweepEnabled <- isSweepEnabled
    let guardSeeds = if sweepEnabled then sweepSeeds else [baselineSeed]
    forM_ guardSeeds $ \seed -> do
      topology <- runValidationCase StageTopology defaultValidationPreset seed
      finalRun <- runValidationCase StageFull defaultValidationPreset seed
      let topologyMetrics = vrMetrics topology
          finalMetrics = vrMetrics finalRun
          landShift = abs (wgmElevationLandFraction finalMetrics - wgmElevationLandFraction topologyMetrics)
          largestChanged = largestMaskDeltaFraction
            (wgmGridWidth topologyMetrics)
            (wgmGridHeight topologyMetrics)
            (vrElevationLandMask topology)
            (vrElevationLandMask finalRun)

      putStrLn (renderFinalGuard topologyMetrics finalMetrics landShift largestChanged)
      landShift `shouldSatisfy` (<= 0.05)
      largestChanged `shouldSatisfy` (<= 0.03)

baselineSeed :: Word64
baselineSeed = 42

smokeSeeds :: [Word64]
smokeSeeds = [42, 999999, 7777777]

sweepSeeds :: [Word64]
sweepSeeds = [42, 999999, 7777777, 104729, 271828, 314159, 8675309, 123456789, 20240630]

schemaVersion :: String
schemaVersion = "worldgen-validation-v1"

defaultValidationPreset :: ValidationPreset
defaultValidationPreset = ValidationPreset
  { vpName = "default"
  , vpConfig = defaultWorldGenConfig
  , vpConfigLabel = "defaultWorldGenConfig"
  }

validationPresets :: [ValidationPreset]
validationPresets = [defaultValidationPreset]

data ValidationPreset = ValidationPreset
  { vpName :: !String
  , vpConfig :: !WorldGenConfig
  , vpConfigLabel :: !String
  }

-- | The topology harness measures after StageWaterBody while preserving the
-- full-pipeline stage list.  Post-topology stages are disabled by dependency
-- closure so CI can sweep seeds without running climate/biome/weather work.
data StageMode = StageTopology | StageFull
  deriving (Eq, Show)

data ValidationRun = ValidationRun
  { vrMetrics :: !WorldGenMetrics
  , vrElevationLandMask :: !(U.Vector Bool)
  }

runValidationCase :: StageMode -> ValidationPreset -> Word64 -> IO ValidationRun
runValidationCase mode preset seed = do
  let cfg = vpConfig preset
      worldCfg = WorldConfig { wcChunkSize = 64 }
      world0 = emptyWorldWithPlanet worldCfg (worldHexGrid cfg) (worldPlanet cfg) (worldSlice cfg)
      pipeline = (buildFullPipelineConfig cfg worldCfg seed)
        { pipelineDisabled = disabledRoots mode
        }
      env = TopoEnv { teLogger = \_ -> pure () }
  result <- runPipeline pipeline env world0
  world <- expectPipeline result
  case validationRunFromWorld mode preset seed world of
    Left err -> expectationFailure err >> fail err
    Right value -> pure value

expectPipeline :: Either PipelineError (TerrainWorld, [PipelineSnapshot]) -> IO TerrainWorld
expectPipeline result =
  case result of
    Left err -> expectationFailure (show err) >> fail (show err)
    Right (world, _) -> pure world

disabledRoots :: StageMode -> Set.Set StageId
disabledRoots StageTopology = Set.fromList [StageSoil, StageParameters, StageWaterTable]
disabledRoots StageFull = Set.empty

disabledResolved :: StageMode -> Set.Set StageId
disabledResolved = disabledClosure builtinDependencies . disabledRoots

stageModeName :: StageMode -> String
stageModeName StageTopology = "topology-through-water-body"
stageModeName StageFull = "full-pipeline"

isSweepEnabled :: IO Bool
isSweepEnabled = do
  value <- lookupEnv "TOPO_WORLDGEN_SWEEP"
  pure (value `elem` [Just "1", Just "true", Just "TRUE", Just "yes", Just "YES"])

validationRunFromWorld :: StageMode -> ValidationPreset -> Word64 -> TerrainWorld -> Either String ValidationRun
validationRunFromWorld mode preset seed world = do
  let cfg = vpConfig preset
      worldCfg = twConfig world
      waterLevel = hcWaterLevel (terrainHydrology (worldTerrain cfg))
      terrain = twTerrain world
      waterBodies = twWaterBodies world
  (minCoord@(ChunkCoord minCx minCy), ChunkCoord maxCx maxCy) <-
    firstLeft (Text.unpack . ("terrain grid: " <>)) (validateTerrainGrid worldCfg terrain)

  unless (IntMap.keysSet terrain == IntMap.keysSet waterBodies) $
    Left "water-body chunks do not match generated terrain chunks"

  let chunkSize = wcChunkSize worldCfg
      gridW = (maxCx - minCx + 1) * chunkSize
      gridH = (maxCy - minCy + 1) * chunkSize
      total = gridW * gridH
      waterGrid = buildWaterTypeGrid worldCfg waterBodies minCoord gridW gridH
      elevationGrid = buildElevationGrid worldCfg terrain minCoord gridW gridH
      elevationLandMask = U.map (>= waterLevel) elevationGrid
      elevationLandCount = U.length (U.filter id elevationLandMask)
      landComponents = componentsWhere gridW gridH (\i -> waterGrid U.! i == WaterDry)
      oceanComponents = componentsWhere gridW gridH (\i -> waterGrid U.! i == WaterOcean)
      inlandSeaComponents = componentsWhere gridW gridH (\i -> waterGrid U.! i == WaterInlandSea)
      lakeComponents = componentsWhere gridW gridH (\i -> waterGrid U.! i == WaterLake)
      landCount = sum (map componentSize landComponents)
      waterCount = total - landCount
      landFraction = fraction landCount total
      elevationLandFraction = fraction elevationLandCount total
      sortedLand = sortComponents landComponents
      nonEdgeIslands = filter (isNonEdgeIsland total) landComponents
      chainStats = islandChainStats nonEdgeIslands
      coastHist = coastDirectionHistogram gridW gridH waterGrid
      coastEdges = sum coastHist
      edgeBand = min 16 (max 1 (min gridW gridH `div` 2))
      edgeFractions = edgeWaterFractions edgeBand gridW gridH waterGrid
      edgeFrameWater = frameWaterFraction edgeBand gridW gridH waterGrid
      effectiveGen = effectiveGenConfig cfg worldCfg
      rawEdge = edgePolicyFromOceanEdgeDepth (gcOceanEdgeDepth (terrainGen (worldTerrain cfg)))
      effectiveEdge = edgePolicyFromOceanEdgeDepth (gcOceanEdgeDepth effectiveGen)
      metrics = WorldGenMetrics
        { wgmSchemaVersion = schemaVersion
        , wgmPresetName = vpName preset
        , wgmSeed = seed
        , wgmConfigLabel = vpConfigLabel preset
        , wgmConfigFingerprint = configFingerprint cfg
        , wgmStageMode = stageModeName mode
        , wgmDisabledStages = map (Text.unpack . stageCanonicalName) (Set.toAscList (disabledResolved mode))
        , wgmChunkSize = chunkSize
        , wgmChunkCount = IntMap.size terrain
        , wgmGridWidth = gridW
        , wgmGridHeight = gridH
        , wgmTotalTiles = total
        , wgmWaterLevel = waterLevel
        , wgmLandTiles = landCount
        , wgmWaterTiles = waterCount
        , wgmLandFraction = landFraction
        , wgmWaterFraction = fraction waterCount total
        , wgmElevationLandFraction = elevationLandFraction
        , wgmLabelElevationLandDelta = abs (landFraction - elevationLandFraction)
        , wgmLandComponentCount = length landComponents
        , wgmLargestLandFraction = largestFraction total landComponents
        , wgmTopLandFractions = take 5 (map (componentFraction total) sortedLand)
        , wgmSignificantLandmasses = length (filter (\c -> componentFraction total c >= 0.03) landComponents)
        , wgmEdgeTouchingLandComponents = length (filter componentTouchesEdge landComponents)
        , wgmNonEdgeIslandCount = length nonEdgeIslands
        , wgmNonEdgeIslandFractions = take 8 (map (componentFraction total) (sortComponents nonEdgeIslands))
        , wgmIslandChainClusterCount = icsClusterCount chainStats
        , wgmLargestIslandChainLength = icsLargestLength chainStats
        , wgmIslandChainParticipatingIslands = icsParticipatingIslands chainStats
        , wgmOceanSummary = waterBodySummary total oceanComponents
        , wgmInlandSeaSummary = waterBodySummary total inlandSeaComponents
        , wgmLakeSummary = waterBodySummary total lakeComponents
        , wgmLargestEdgeOceanFraction = largestFraction total (filter componentTouchesEdge oceanComponents)
        , wgmCoastEdges = coastEdges
        , wgmCoastEdgesPerLandTile = if landCount <= 0 then 0 else fromIntegral coastEdges / fromIntegral landCount
        , wgmShapeIndex = if landCount <= 0 then 0 else fromIntegral coastEdges / sqrt (fromIntegral landCount)
        , wgmCoastDirectionHistogram = coastHist
        , wgmCoastDirectionImbalance = directionImbalance coastHist
        , wgmEdgeFrameWidth = edgeBand
        , wgmEdgeFrameWaterFraction = edgeFrameWater
        , wgmPerEdgeWaterFractions = edgeFractions
        , wgmEdgesAbove75Water = length (filter (> 0.75) (edgeFractionValues edgeFractions))
        , wgmContinuousFourEdgeMoat = any componentTouchesAllSides oceanComponents
        , wgmConfiguredEdgePolicy = rawEdge
        , wgmEffectiveEdgePolicy = effectiveEdge
        , wgmImplicitLegacyAutoEdge = edgePolicyIsZero rawEdge && edgePolicyIsLegacyAuto effectiveEdge
        , wgmHomogeneous16Fraction = homogeneousBlockFraction 16 0.90 gridW gridH waterGrid
        }
  pure ValidationRun
    { vrMetrics = metrics
    , vrElevationLandMask = elevationLandMask
    }

firstLeft :: (a -> b) -> Either a c -> Either b c
firstLeft f value = case value of
  Left err -> Left (f err)
  Right ok -> Right ok

buildWaterTypeGrid
  :: WorldConfig
  -> IntMap.IntMap WaterBodyChunk
  -> ChunkCoord
  -> Int
  -> Int
  -> U.Vector WaterBodyType
buildWaterTypeGrid config chunks (ChunkCoord minCx minCy) gridW gridH =
  U.generate (gridW * gridH) $ \idx ->
    let size = wcChunkSize config
        localGridX = idx `mod` gridW
        localGridY = idx `div` gridW
        globalX = minCx * size + localGridX
        globalY = minCy * size + localGridY
        (chunkCoord, localCoord) = chunkCoordFromTile config (TileCoord globalX globalY)
        ChunkId key = chunkIdFromCoord chunkCoord
    in case IntMap.lookup key chunks of
        Nothing -> WaterDry
        Just chunk ->
          case tileIndex config localCoord of
            Nothing -> WaterDry
            Just (TileIndex i) -> wbType chunk U.! i

effectiveGenConfig :: WorldGenConfig -> WorldConfig -> GenConfig
effectiveGenConfig cfg worldCfg =
  let terrain = worldTerrain cfg
      gen0 = terrainGen terrain
      derivedExtent = either (const defaultWorldExtent) id $
        sliceToWorldExtent (worldPlanet cfg) (worldHexGrid cfg) (worldSlice cfg) worldCfg
      gen1 = if gcWorldExtent gen0 == defaultWorldExtent
        then gen0 { gcWorldExtent = derivedExtent }
        else gen0
  in autoOceanEdgeDepth (worldPlanet cfg) (worldSlice cfg) gen1

data WorldGenMetrics = WorldGenMetrics
  { wgmSchemaVersion :: !String
  , wgmPresetName :: !String
  , wgmSeed :: !Word64
  , wgmConfigLabel :: !String
  , wgmConfigFingerprint :: !String
  , wgmStageMode :: !String
  , wgmDisabledStages :: ![String]
  , wgmChunkSize :: !Int
  , wgmChunkCount :: !Int
  , wgmGridWidth :: !Int
  , wgmGridHeight :: !Int
  , wgmTotalTiles :: !Int
  , wgmWaterLevel :: !Float
  , wgmLandTiles :: !Int
  , wgmWaterTiles :: !Int
  , wgmLandFraction :: !Float
  , wgmWaterFraction :: !Float
  , wgmElevationLandFraction :: !Float
  , wgmLabelElevationLandDelta :: !Float
  , wgmLandComponentCount :: !Int
  , wgmLargestLandFraction :: !Float
  , wgmTopLandFractions :: ![Float]
  , wgmSignificantLandmasses :: !Int
  , wgmEdgeTouchingLandComponents :: !Int
  , wgmNonEdgeIslandCount :: !Int
  , wgmNonEdgeIslandFractions :: ![Float]
  , wgmIslandChainClusterCount :: !Int
  , wgmLargestIslandChainLength :: !Int
  , wgmIslandChainParticipatingIslands :: !Int
  , wgmOceanSummary :: !WaterBodySummary
  , wgmInlandSeaSummary :: !WaterBodySummary
  , wgmLakeSummary :: !WaterBodySummary
  , wgmLargestEdgeOceanFraction :: !Float
  , wgmCoastEdges :: !Int
  , wgmCoastEdgesPerLandTile :: !Float
  , wgmShapeIndex :: !Float
  , wgmCoastDirectionHistogram :: ![Int]
  , wgmCoastDirectionImbalance :: !Float
  , wgmEdgeFrameWidth :: !Int
  , wgmEdgeFrameWaterFraction :: !Float
  , wgmPerEdgeWaterFractions :: !EdgeWaterFractions
  , wgmEdgesAbove75Water :: !Int
  , wgmContinuousFourEdgeMoat :: !Bool
  , wgmConfiguredEdgePolicy :: !EdgePolicy
  , wgmEffectiveEdgePolicy :: !EdgePolicy
  , wgmImplicitLegacyAutoEdge :: !Bool
  , wgmHomogeneous16Fraction :: !Float
  } deriving (Eq, Show)

data WaterBodySummary = WaterBodySummary
  { wbsCount :: !Int
  , wbsLargestFraction :: !Float
  , wbsTopFractions :: ![Float]
  } deriving (Eq, Show)

data EdgeWaterFractions = EdgeWaterFractions
  { ewfNorth :: !Float
  , ewfEast :: !Float
  , ewfSouth :: !Float
  , ewfWest :: !Float
  } deriving (Eq, Show)

data EdgePolicy = EdgePolicy
  { epRMin :: !Float
  , epRMax :: !Float
  , epQMax :: !Float
  , epQMin :: !Float
  , epFalloff :: !Float
  } deriving (Eq, Show)

data Component = Component
  { componentSize :: !Int
  , componentSumX :: !Int
  , componentSumY :: !Int
  , componentSideMask :: !Int
  } deriving (Eq, Show)

data IslandChainStats = IslandChainStats
  { icsClusterCount :: !Int
  , icsLargestLength :: !Int
  , icsParticipatingIslands :: !Int
  } deriving (Eq, Show)

componentsWhere :: Int -> Int -> (Int -> Bool) -> [Component]
componentsWhere gridW gridH isMember = go initial []
  where
    total = gridW * gridH
    initial = IntSet.fromDistinctAscList [i | i <- [0 .. total - 1], isMember i]

    go remaining acc
      | IntSet.null remaining = reverse acc
      | otherwise =
          let start = IntSet.findMin remaining
              (component, remaining') = flood remaining [start] 0 0 0 0
          in go remaining' (component : acc)

    flood remaining [] !size !sumX !sumY !mask =
      ( Component
          { componentSize = size
          , componentSumX = sumX
          , componentSumY = sumY
          , componentSideMask = mask
          }
      , remaining
      )
    flood remaining (i:stack) !size !sumX !sumY !mask
      | not (IntSet.member i remaining) = flood remaining stack size sumX sumY mask
      | otherwise =
          let remaining' = IntSet.delete i remaining
              x = i `mod` gridW
              y = i `div` gridW
              neighbors = filter (`IntSet.member` remaining') (hexNeighborIndices gridW gridH i)
          in flood remaining' (neighbors ++ stack)
              (size + 1)
              (sumX + x)
              (sumY + y)
              (mask .|. sideMaskFor gridW gridH x y)

sideNorth, sideEast, sideSouth, sideWest, allSides :: Int
sideNorth = 1
sideEast = 2
sideSouth = 4
sideWest = 8
allSides = sideNorth .|. sideEast .|. sideSouth .|. sideWest

sideMaskFor :: Int -> Int -> Int -> Int -> Int
sideMaskFor gridW gridH x y =
  (if y == 0 then sideNorth else 0) .|.
  (if x == gridW - 1 then sideEast else 0) .|.
  (if y == gridH - 1 then sideSouth else 0) .|.
  (if x == 0 then sideWest else 0)

componentTouchesEdge :: Component -> Bool
componentTouchesEdge component = componentSideMask component /= 0

componentTouchesAllSides :: Component -> Bool
componentTouchesAllSides component = componentSideMask component .&. allSides == allSides

componentCentroidHex :: Component -> HexCoord
componentCentroidHex Component{..}
  | componentSize <= 0 = HexAxial 0 0
  | otherwise = HexAxial
      (round (fromIntegral componentSumX / fromIntegral componentSize :: Float))
      (round (fromIntegral componentSumY / fromIntegral componentSize :: Float))

isNonEdgeIsland :: Int -> Component -> Bool
isNonEdgeIsland total component =
  not (componentTouchesEdge component) && componentFraction total component < 0.01

islandChainStats :: [Component] -> IslandChainStats
islandChainStats islands =
  let clusters = islandClusters islands
      chainClusters = filter ((>= 3) . length) clusters
  in IslandChainStats
      { icsClusterCount = length chainClusters
      , icsLargestLength = maximum (0 : map length chainClusters)
      , icsParticipatingIslands = sum (map length chainClusters)
      }

islandClusters :: [Component] -> [[Int]]
islandClusters islands = go (IntSet.fromDistinctAscList [0 .. length islands - 1]) []
  where
    centroids = map componentCentroidHex islands
    connected a b = hexDistance (centroids !! a) (centroids !! b) <= 40

    go remaining acc
      | IntSet.null remaining = reverse acc
      | otherwise =
          let start = IntSet.findMin remaining
              (cluster, remaining') = flood remaining [start] []
          in go remaining' (cluster : acc)

    flood remaining [] cluster = (cluster, remaining)
    flood remaining (i:stack) cluster
      | not (IntSet.member i remaining) = flood remaining stack cluster
      | otherwise =
          let remaining' = IntSet.delete i remaining
              neighbors = [j | j <- IntSet.toList remaining', connected i j]
          in flood remaining' (neighbors ++ stack) (i : cluster)

sortComponents :: [Component] -> [Component]
sortComponents = sortBy (comparing (Down . componentSize))

componentFraction :: Int -> Component -> Float
componentFraction total component = fraction (componentSize component) total

largestFraction :: Int -> [Component] -> Float
largestFraction total components = fraction (maximum (0 : map componentSize components)) total

waterBodySummary :: Int -> [Component] -> WaterBodySummary
waterBodySummary total components =
  let sortedComponents = sortComponents components
  in WaterBodySummary
      { wbsCount = length components
      , wbsLargestFraction = largestFraction total components
      , wbsTopFractions = take 5 (map (componentFraction total) sortedComponents)
      }

fraction :: Int -> Int -> Float
fraction _ 0 = 0
fraction part whole = fromIntegral part / fromIntegral whole

coastDirectionHistogram :: Int -> Int -> U.Vector WaterBodyType -> [Int]
coastDirectionHistogram gridW gridH waterGrid =
  [ length
      [ ()
      | i <- [0 .. total - 1]
      , waterGrid U.! i == WaterDry
      , Just ni <- [hexNeighborIndexInDirection gridW gridH dir i]
      , waterGrid U.! ni /= WaterDry
      ]
  | dir <- allHexDirections
  ]
  where
    total = gridW * gridH

directionImbalance :: [Int] -> Float
directionImbalance hist =
  let total = sum hist
      meanValue = fromIntegral total / fromIntegral (max 1 (length hist))
  in if meanValue <= 0 then 0 else fromIntegral (maximum (0 : hist)) / meanValue

frameWaterFraction :: Int -> Int -> Int -> U.Vector WaterBodyType -> Float
frameWaterFraction band gridW gridH waterGrid = fraction waterCount frameCount
  where
    total = gridW * gridH
    inFrame i =
      let x = i `mod` gridW
          y = i `div` gridW
      in x < band || x >= gridW - band || y < band || y >= gridH - band
    frameIndices = [i | i <- [0 .. total - 1], inFrame i]
    frameCount = length frameIndices
    waterCount = length [() | i <- frameIndices, waterGrid U.! i /= WaterDry]

edgeWaterFractions :: Int -> Int -> Int -> U.Vector WaterBodyType -> EdgeWaterFractions
edgeWaterFractions band gridW gridH waterGrid = EdgeWaterFractions
  { ewfNorth = edgeFraction (\_x y -> y < band)
  , ewfEast = edgeFraction (\x _y -> x >= gridW - band)
  , ewfSouth = edgeFraction (\_x y -> y >= gridH - band)
  , ewfWest = edgeFraction (\x _y -> x < band)
  }
  where
    total = gridW * gridH
    edgeFraction isEdge =
      let idxs =
            [ i
            | i <- [0 .. total - 1]
            , let x = i `mod` gridW
                  y = i `div` gridW
            , isEdge x y
            ]
          waters = length [() | i <- idxs, waterGrid U.! i /= WaterDry]
      in fraction waters (length idxs)

edgeFractionValues :: EdgeWaterFractions -> [Float]
edgeFractionValues EdgeWaterFractions{..} = [ewfNorth, ewfEast, ewfSouth, ewfWest]

homogeneousBlockFraction :: Int -> Float -> Int -> Int -> U.Vector WaterBodyType -> Float
homogeneousBlockFraction blockSize dominanceThreshold gridW gridH waterGrid = fraction homogeneous totalBlocks
  where
    blockStartsX = [0, blockSize .. gridW - 1]
    blockStartsY = [0, blockSize .. gridH - 1]
    blocks = [(x, y) | y <- blockStartsY, x <- blockStartsX]
    totalBlocks = length blocks
    homogeneous = length (filter blockHomogeneous blocks)

    blockHomogeneous (startX, startY) =
      let endX = min (gridW - 1) (startX + blockSize - 1)
          endY = min (gridH - 1) (startY + blockSize - 1)
          indices = [y * gridW + x | y <- [startY .. endY], x <- [startX .. endX]]
          land = length [() | i <- indices, waterGrid U.! i == WaterDry]
          total = length indices
          water = total - land
          dominance = fraction (max land water) total
      in dominance >= dominanceThreshold

largestMaskDeltaFraction :: Int -> Int -> U.Vector Bool -> U.Vector Bool -> Float
largestMaskDeltaFraction gridW gridH a b
  | U.length a /= U.length b = 1
  | otherwise = largestFraction (gridW * gridH) $
      componentsWhere gridW gridH (\i -> (a U.! i) /= (b U.! i))

edgePolicyFromOceanEdgeDepth :: OceanEdgeDepth -> EdgePolicy
edgePolicyFromOceanEdgeDepth OceanEdgeDepth{..} = EdgePolicy
  { epRMin = oedRMin
  , epRMax = oedRMax
  , epQMax = oedQMax
  , epQMin = oedQMin
  , epFalloff = oedFalloff
  }

edgePolicyIsZero :: EdgePolicy -> Bool
edgePolicyIsZero EdgePolicy{..} = all (== 0) [epRMin, epRMax, epQMax, epQMin, epFalloff]

edgePolicyIsLegacyAuto :: EdgePolicy -> Bool
edgePolicyIsLegacyAuto EdgePolicy{..} =
  epRMin == 0.5 && epRMax == 0.5 && epQMax == 0.5 && epQMin == 0.5 && epFalloff == 16

configFingerprint :: WorldGenConfig -> String
configFingerprint cfg = pad16 (showHex hash "")
  where
    hash :: Word64
    hash = BL.foldl' step 1469598103934665603 (encode cfg)
    step h byte = (h `xor` fromIntegral byte) * 1099511628211
    pad16 s = replicate (max 0 (16 - length s)) '0' <> s

assertStructuralMetrics :: [WorldGenMetrics] -> Expectation
assertStructuralMetrics metrics = forM_ metrics $ \m -> do
  wgmSchemaVersion m `shouldBe` schemaVersion
  wgmGridWidth m `shouldBe` 320
  wgmGridHeight m `shouldBe` 320
  wgmTotalTiles m `shouldBe` 102400
  wgmLandFraction m `shouldSatisfy` between 0.05 0.95
  wgmWaterFraction m `shouldSatisfy` between 0.05 0.95
  wgmLandComponentCount m `shouldSatisfy` (> 0)
  wbsCount (wgmOceanSummary m) `shouldSatisfy` (> 0)
  wbsLargestFraction (wgmOceanSummary m) `shouldSatisfy` between 0.05 0.95
  wgmHomogeneous16Fraction m `shouldSatisfy` between 0 0.95
  wgmEdgeFrameWaterFraction m `shouldSatisfy` between 0 1
  allFinite (metricFloats m) `shouldBe` True
  wgmLandTiles m + wgmWaterTiles m `shouldBe` wgmTotalTiles m
  sum (wgmCoastDirectionHistogram m) `shouldBe` wgmCoastEdges m

between :: Float -> Float -> Float -> Bool
between lo hi x = x >= lo && x <= hi

allFinite :: [Float] -> Bool
allFinite = all (\x -> not (isNaN x) && not (isInfinite x))

metricFloats :: WorldGenMetrics -> [Float]
metricFloats WorldGenMetrics{..} =
  [ wgmWaterLevel
  , wgmLandFraction
  , wgmWaterFraction
  , wgmElevationLandFraction
  , wgmLabelElevationLandDelta
  , wgmLargestLandFraction
  , wgmCoastEdgesPerLandTile
  , wgmShapeIndex
  , wgmCoastDirectionImbalance
  , wgmEdgeFrameWaterFraction
  , wgmHomogeneous16Fraction
  ]
  <> wgmTopLandFractions
  <> wgmNonEdgeIslandFractions
  <> waterBodyFloats wgmOceanSummary
  <> waterBodyFloats wgmInlandSeaSummary
  <> waterBodyFloats wgmLakeSummary
  <> edgeFractionValues wgmPerEdgeWaterFractions

waterBodyFloats :: WaterBodySummary -> [Float]
waterBodyFloats WaterBodySummary{..} = wbsLargestFraction : wbsTopFractions

renderValidationReport :: String -> [WorldGenMetrics] -> String
renderValidationReport modeLabel metrics = unlines $
  [ "WorldGen validation report (" <> modeLabel <> ", schema=" <> schemaVersion <> ")"
  , "Future aesthetic ranges below are report-only; structural smoke assertions are the only hard gates in this task."
  ]
  <> map renderMetric metrics
  <> renderAggregate metrics
  <> renderReportOnlyThresholds metrics

renderMetric :: WorldGenMetrics -> String
renderMetric m =
  printf
    ("preset=%s seed=%s stage=%s config=%s#%s grid=%dx%d tiles=%d " <>
     "land=%s water=%s elevLand=%s components=%d largestLand=%s topLand=%s " <>
     "significantLand=%d edgeLandComponents=%d islands=%d islandChains=%d/%d participating=%d " <>
     "ocean=%s inlandSea=%s lake=%s largestEdgeOcean=%s " <>
     "coastEdges=%d coastPerLand=%s shapeIndex=%s coastDir=%s imbalance=%s " <>
     "edgeFrame%d=%s perEdge(N,E,S,W)=%s edges>75%%=%d fourEdgeMoat=%s " <>
     "homogeneous16=%s rawEdge=%s effectiveEdge=%s implicitLegacyAutoEdge=%s disabled=%s")
    (wgmPresetName m)
    (show (wgmSeed m))
    (wgmStageMode m)
    (wgmConfigLabel m)
    (wgmConfigFingerprint m)
    (wgmGridWidth m)
    (wgmGridHeight m)
    (wgmTotalTiles m)
    (fmt (wgmLandFraction m))
    (fmt (wgmWaterFraction m))
    (fmt (wgmElevationLandFraction m))
    (wgmLandComponentCount m)
    (fmt (wgmLargestLandFraction m))
    (fmtList (wgmTopLandFractions m))
    (wgmSignificantLandmasses m)
    (wgmEdgeTouchingLandComponents m)
    (wgmNonEdgeIslandCount m)
    (wgmIslandChainClusterCount m)
    (wgmLargestIslandChainLength m)
    (wgmIslandChainParticipatingIslands m)
    (renderWaterBodySummary (wgmOceanSummary m))
    (renderWaterBodySummary (wgmInlandSeaSummary m))
    (renderWaterBodySummary (wgmLakeSummary m))
    (fmt (wgmLargestEdgeOceanFraction m))
    (wgmCoastEdges m)
    (fmt (wgmCoastEdgesPerLandTile m))
    (fmt (wgmShapeIndex m))
    (show (wgmCoastDirectionHistogram m))
    (fmt (wgmCoastDirectionImbalance m))
    (wgmEdgeFrameWidth m)
    (fmt (wgmEdgeFrameWaterFraction m))
    (renderEdgeFractions (wgmPerEdgeWaterFractions m))
    (wgmEdgesAbove75Water m)
    (show (wgmContinuousFourEdgeMoat m))
    (fmt (wgmHomogeneous16Fraction m))
    (renderEdgePolicy (wgmConfiguredEdgePolicy m))
    (renderEdgePolicy (wgmEffectiveEdgePolicy m))
    (show (wgmImplicitLegacyAutoEdge m))
    (if null (wgmDisabledStages m) then "none" else intercalate "," (wgmDisabledStages m))

renderWaterBodySummary :: WaterBodySummary -> String
renderWaterBodySummary WaterBodySummary{..} =
  "count=" <> show wbsCount <> ",largest=" <> fmt wbsLargestFraction <> ",top=" <> fmtList wbsTopFractions

renderEdgeFractions :: EdgeWaterFractions -> String
renderEdgeFractions EdgeWaterFractions{..} = fmtList [ewfNorth, ewfEast, ewfSouth, ewfWest]

renderEdgePolicy :: EdgePolicy -> String
renderEdgePolicy EdgePolicy{..} =
  "{rMin=" <> fmt epRMin <> ",rMax=" <> fmt epRMax <>
  ",qMax=" <> fmt epQMax <> ",qMin=" <> fmt epQMin <>
  ",falloff=" <> fmt epFalloff <> "}"

renderAggregate :: [WorldGenMetrics] -> [String]
renderAggregate metrics =
  [ "Aggregate default medians: land=" <> medianOf wgmLandFraction <>
      " largestLand=" <> medianOf wgmLargestLandFraction <>
      " edgeFrame=" <> medianOf wgmEdgeFrameWaterFraction <>
      " largestOcean=" <> medianOf (wbsLargestFraction . wgmOceanSummary) <>
      " homogeneous16=" <> medianOf wgmHomogeneous16Fraction
  ]
  <> if length metrics >= 9
       then [ "Aggregate sweep p10/p90: land=" <> percentilePair wgmLandFraction <>
              " edgeFrame=" <> percentilePair wgmEdgeFrameWaterFraction <>
              " homogeneous16=" <> percentilePair wgmHomogeneous16Fraction
            ]
       else []
  where
    medianOf f = fmt (median (map f metrics))
    percentilePair f =
      let values = map f metrics
      in fmt (nearestPercentile 0.10 values) <> "/" <> fmt (nearestPercentile 0.90 values)

renderReportOnlyThresholds :: [WorldGenMetrics] -> [String]
renderReportOnlyThresholds metrics =
  "Report-only future threshold probes for balanced/default:" :
  map renderProbe probes <>
  [ "Report-only target catalog: archipelago land 0.18-0.35 median with >=2 island chains; large-ocean water 0.60-0.80 median; inland-seas land 0.45-0.65 with inland sea >=0.03."
  ]
  where
    probes =
      [ ("median land", median (map wgmLandFraction metrics), 0.35, 0.50)
      , ("median largest land", median (map wgmLargestLandFraction metrics), 0.25, 0.45)
      , ("median edge-frame water", median (map wgmEdgeFrameWaterFraction metrics), 0.45, 0.75)
      , ("median homogeneous16", median (map wgmHomogeneous16Fraction metrics), 0.00, 0.65)
      ]
    renderProbe (label, value, lo, hi) =
      "  " <> label <> "=" <> fmt value <> " target=" <> fmt lo <> "-" <> fmt hi <>
      " status=" <> if between lo hi value then "inside-target(report-only)" else "outside-target(report-only)"

renderFinalGuard :: WorldGenMetrics -> WorldGenMetrics -> Float -> Float -> String
renderFinalGuard topology finalRun landShift largestChanged =
  printf
    ("WorldGen final-output guard seed=%s topologyElevLand=%s fullElevLand=%s " <>
     "delta=%s largestChangedComponent=%s fullStage=%s")
    (show (wgmSeed topology))
    (fmt (wgmElevationLandFraction topology))
    (fmt (wgmElevationLandFraction finalRun))
    (fmt landShift)
    (fmt largestChanged)
    (wgmStageMode finalRun)

median :: [Float] -> Float
median [] = 0
median values =
  let sortedValues = sort values
      n = length sortedValues
      mid = n `div` 2
  in if odd n
      then sortedValues !! mid
      else (sortedValues !! (mid - 1) + sortedValues !! mid) / 2

nearestPercentile :: Float -> [Float] -> Float
nearestPercentile _ [] = 0
nearestPercentile p values =
  let sortedValues = sort values
      n = length sortedValues
      rank = max 0 (min (n - 1) (ceiling (p * fromIntegral n) - 1))
  in sortedValues !! rank

fmt :: Float -> String
fmt value = printf "%.3f" (realToFrac value :: Double)

fmtList :: [Float] -> String
fmtList values = "[" <> intercalate "," (map fmt values) <> "]"
