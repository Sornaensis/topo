{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Hydrology stages for flow routing and moisture.
module Topo.Hydrology
  ( HydroConfig(..)
  , defaultHydroConfig
  , applyHydrologyStage
  , RiverConfig(..)
  , defaultRiverConfig
  , GroundwaterConfig(..)
  , defaultGroundwaterConfig
  , applyRiverStage
    -- * Flow routing (exported for testing)
  , flowDirections
  , flowDirectionsLand
    -- * Depression filling (exported for testing)
  , fillDepressions
  , breachRemainingSinks
    -- * Sink breaching (exported for testing, deprecated)
  , breachSinksLand
  ) where

import Control.Monad (forM_, when, foldM)
import Control.Monad.ST (ST, runST)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Word (Word16, Word32)
import Topo.Math (clamp01)
import Topo.Hex (hexNeighborIndices)
import Topo.Pipeline (PipelineStage(..))
import Control.Monad.Except (throwError)
import Topo.Plugin (logInfo, getWorldP, putWorldP, PluginError(..))
import Topo.River (RiverTopologyConfig, defaultRiverTopologyConfig, computeRiverSegments)
import Topo.TerrainGrid
  ( buildElevationGrid
  , buildHardnessGrid
  , buildMoistureGrid
  , updateChunkElevationFromGrid
  , updateChunkMoistureFromGrid
  , chunkGridSlice
  , validateTerrainGrid
  )
import Topo.Types
import Topo.World (TerrainWorld(..))
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM

-- | Hydrology configuration.
data HydroConfig = HydroConfig
  { hcWaterLevel :: !Float
  , hcSinkBreachDepth :: !Float
  , hcBaseAccumulation :: !Float
  , hcMinAccumulation :: !Float
  , hcStreamPowerMaxErosion :: !Float
  , hcStreamPowerScale :: !Float
  , hcStreamDepositRatio :: !Float
  , hcRiverCarveMaxDepth :: !Float
  , hcRiverCarveScale :: !Float
  , hcRiverBankThreshold :: !Float
  , hcRiverBankDepth :: !Float
  , hcAlluvialMaxSlope :: !Float
  , hcAlluvialDepositScale :: !Float
  , hcWetErodeScale :: !Float
  , hcCoastalErodeStrength :: !Float
  , hcCoastalRaiseFactor :: !Float
  -- | [0..1] scaling of hardness against erosion intensity.
  , hcHardnessErodeWeight :: !Float
  , hcMoistureBaseWeight :: !Float
  , hcMoistureFlowWeight :: !Float
  , hcMinMoisture :: !Float
  } deriving (Eq, Show)

-- | Default hydrology configuration.
defaultHydroConfig :: HydroConfig
defaultHydroConfig = HydroConfig
  { hcWaterLevel = 0.5
  , hcSinkBreachDepth = 0.02
  , hcBaseAccumulation = 1
  , hcMinAccumulation = 1
  , hcStreamPowerMaxErosion = 0.05
  , hcStreamPowerScale = 0.00005
  , hcStreamDepositRatio = 0.3
  , hcRiverCarveMaxDepth = 0.05
  , hcRiverCarveScale = 0.03
  , hcRiverBankThreshold = 0.35
  , hcRiverBankDepth = 0.01
  , hcAlluvialMaxSlope = 0.08
  , hcAlluvialDepositScale = 0.02
  , hcWetErodeScale = 0.015
  , hcCoastalErodeStrength = 0.02
  , hcCoastalRaiseFactor = 0.5
  , hcHardnessErodeWeight = 0.7
  , hcMoistureBaseWeight = 0.6
  , hcMoistureFlowWeight = 0.7
  , hcMinMoisture = 1
  }

-- | River routing configuration derived from moisture + flow accumulation.
data RiverConfig = RiverConfig
  { rcBaseAccumulation :: !Float
  , rcMinAccumulation :: !Float
  , rcOrderMinAccumulation :: !Float
  , rcDischargeScale :: !Float
  , rcChannelDepthScale :: !Float
  , rcChannelMaxDepth :: !Float
  -- | Water level threshold.  Tiles at or below this elevation are
  -- considered submerged and act as flow sinks during river routing.
  , rcWaterLevel :: !Float
  -- | Depth by which local land-sinks are breached before flow routing.
  -- Eliminates spurious terminus points caused by post-hydrology
  -- depressions.  Same concept as 'hcSinkBreachDepth'.
  , rcSinkBreachDepth :: !Float
  -- | [0..1] scaling of hardness against channel depth.
  , rcHardnessDepthWeight :: !Float
  -- | [0..1] scaling of hardness against erosion potential.
  , rcHardnessErosionWeight :: !Float
  -- | Scale factor for erosion potential.
  , rcErosionScale :: !Float
  -- | Scale factor for deposition potential.
  , rcDepositScale :: !Float
  -- | Slope threshold for alluvial deposition.
  , rcDepositMaxSlope :: !Float
  , rcBaseflowScale :: !Float
  } deriving (Eq, Show)

-- | Default river routing parameters.
defaultRiverConfig :: RiverConfig
defaultRiverConfig = RiverConfig
  { rcBaseAccumulation = 1
  , rcMinAccumulation = 4
  , rcOrderMinAccumulation = 6
  , rcDischargeScale = 0.05
  , rcChannelDepthScale = 0.002
  , rcChannelMaxDepth = 0.2
  , rcWaterLevel = 0.5
  , rcSinkBreachDepth = 0.02
  , rcHardnessDepthWeight = 0.6
  , rcHardnessErosionWeight = 0.5
  , rcErosionScale = 1
  , rcDepositScale = 0.6
  , rcDepositMaxSlope = 0.1
  , rcBaseflowScale = 1
  }

-- | Groundwater storage and baseflow configuration.
data GroundwaterConfig = GroundwaterConfig
  { gwRechargeScale :: !Float
  , gwStorageScale :: !Float
  , gwDischargeScale :: !Float
  , gwPermeability :: !Float
  , gwMinBasinSize :: !Int
  } deriving (Eq, Show)

-- | Default groundwater parameters.
defaultGroundwaterConfig :: GroundwaterConfig
defaultGroundwaterConfig = GroundwaterConfig
  { gwRechargeScale = 0.2
  , gwStorageScale = 0.5
  , gwDischargeScale = 0.1
  , gwPermeability = 0.6
  , gwMinBasinSize = 1
  }

-- | Apply hydrology routing and moisture derivation.
applyHydrologyStage :: HydroConfig -> PipelineStage
applyHydrologyStage cfg = PipelineStage "applyHydrology" "applyHydrology" $ do
  logInfo "applyHydrology: routing flow + moisture"
  world <- getWorldP
  let config = twConfig world
      terrain = twTerrain world
  (ChunkCoord minCx minCy, ChunkCoord maxCx maxCy) <-
    case validateTerrainGrid config terrain of
      Left err -> throwError (PluginInvariantError err)
      Right bounds -> pure bounds
  let size = wcChunkSize config
      gridW = (maxCx - minCx + 1) * size
      gridH = (maxCy - minCy + 1) * size
      elev0 = buildElevationGrid config terrain (ChunkCoord minCx minCy) gridW gridH
      hardness = buildHardnessGrid config terrain (ChunkCoord minCx minCy) gridW gridH
      -- Fill depressions so every land tile drains to the ocean.
      -- The filled surface is used ONLY for flow routing and accumulation;
      -- erosion is applied to the original terrain so the displayed surface
      -- matches the actual flow paths carved by erosion.
      elev1 = fillDepressions (hcWaterLevel cfg) gridW gridH elev0
      flow = flowDirections gridW gridH elev1
      acc = flowAccumulation cfg elev1 flow
      elevCarved = carveRiversGrid cfg gridW gridH elev0 acc hardness
      elevBanks = riverBankErodeGrid cfg gridW gridH elevCarved acc hardness
      elev2 = applyStreamPowerErosion cfg elevBanks flow acc hardness
      elev3 = coastalErodeGrid cfg gridW gridH elev2 hardness
      elev4 = alluvialDepositGrid cfg gridW gridH elev3 acc
      moisture = moistureFromAccumulation cfg elev4 acc
      elev5 = wetErodeGrid cfg gridW gridH elev4 moisture hardness
      terrain' = IntMap.mapWithKey (updateChunkElevationFromGrid config (ChunkCoord minCx minCy) gridW elev5) terrain
      terrain'' = IntMap.mapWithKey (updateChunkMoistureFromGrid config (ChunkCoord minCx minCy) gridW moisture) terrain'
  putWorldP world { twTerrain = terrain'' }

-- | Apply river routing and basin-level groundwater storage.
applyRiverStage :: RiverConfig -> GroundwaterConfig -> PipelineStage
applyRiverStage riverCfg gwCfg = PipelineStage "applyRivers" "applyRivers" $ do
  logInfo "applyRivers: routing rivers + groundwater"
  world <- getWorldP
  let config = twConfig world
      terrain = twTerrain world
  (ChunkCoord minCx minCy, ChunkCoord maxCx maxCy) <-
    case validateTerrainGrid config terrain of
      Left err -> throwError (PluginInvariantError err)
      Right bounds -> pure bounds
  let size = wcChunkSize config
      gridW = (maxCx - minCx + 1) * size
      gridH = (maxCy - minCy + 1) * size
      elev0 = buildElevationGrid config terrain (ChunkCoord minCx minCy) gridW gridH
      hardness = buildHardnessGrid config terrain (ChunkCoord minCx minCy) gridW gridH
      moisture = buildMoistureGrid config terrain (ChunkCoord minCx minCy) gridW gridH
      wl = rcWaterLevel riverCfg
      -- Fill depressions for routing so rivers follow connected drainage
      -- paths to the sea (or large lake).  The filled surface is used
      -- ONLY for flow direction and accumulation; all other calculations
      -- (segments, potentials, Strahler order) use the actual terrain
      -- (elev0) so rivers render consistently with visible topography.
      elevFilled = fillDepressions wl gridW gridH elev0
      flow = flowDirectionsLand wl gridW gridH elevFilled
      acc = flowAccumulationWithBase (rcBaseAccumulation riverCfg) elevFilled flow
      basinIds = basinIdsFromFlow flow
      basinStats = basinRechargeStats basinIds moisture gwCfg
      (basinStorage, basinDischarge, basinSize) = basinStorageStats basinStats gwCfg
      baseflow = basinBaseflow basinIds basinDischarge basinSize (rcBaseflowScale riverCfg)
      riverOrder = strahlerOrder gridW gridH elev0 flow acc (rcOrderMinAccumulation riverCfg)
      discharge = U.zipWith (+) (U.map (* rcDischargeScale riverCfg) acc) baseflow
      depth = riverDepthWithHardness acc hardness riverCfg
      erosionPotential = riverErosionPotential gridW gridH elev0 acc hardness riverCfg
      depositPotential = riverDepositPotential gridW gridH elev0 acc riverCfg
      rivers = RiverChunk
        { rcFlowAccum = acc
        , rcDischarge = discharge
        , rcChannelDepth = depth
        , rcRiverOrder = riverOrder
        , rcBasinId = basinIds
        , rcBaseflow = baseflow
        , rcErosionPotential = erosionPotential
        , rcDepositPotential = depositPotential
        , rcFlowDir = flow
        , rcSegOffsets = segOffsets
        , rcSegEntryEdge = segEntry
        , rcSegExitEdge = segExit
        , rcSegDischarge = segDisc
        , rcSegOrder = segOrd
        }
      (segOffsets, segEntry, segExit, segDisc, segOrd) =
        computeRiverSegments defaultRiverTopologyConfig gridW gridH flow discharge riverOrder elev0 wl
      groundwater = GroundwaterChunk
        { gwStorage = basinPerTile basinIds basinStorage basinSize
        , gwRecharge = U.map (* gwRechargeScale gwCfg) (U.map clamp01 moisture)
        , gwDischarge = basinPerTile basinIds basinDischarge basinSize
        , gwBasinId = basinIds
        }
      rivers' = IntMap.mapWithKey (sliceRiverChunk config (ChunkCoord minCx minCy) gridW rivers) terrain
      groundwater' = IntMap.mapWithKey (sliceGroundwaterChunk config (ChunkCoord minCx minCy) gridW groundwater) terrain
  putWorldP world { twRivers = rivers', twGroundwater = groundwater' }

breachSinks :: HydroConfig -> Int -> Int -> U.Vector Float -> U.Vector Float
breachSinks cfg gridW gridH elev =
  U.generate (U.length elev) (\i ->
    let h0 = elev U.! i
        hmin = neighborMin gridW gridH elev i
        isSink = hmin >= h0
        waterLevel = hcWaterLevel cfg
        breachDepth = hcSinkBreachDepth cfg
    in if isSink && h0 > waterLevel then h0 - breachDepth else h0)

-- | Breach land-sinks (local minima above @waterLevel@) by lowering them
-- by @breachDepth@.  Submerged tiles are left untouched.  This is the
-- same algorithm as 'breachSinks' but parameterised directly instead of
-- pulling fields from a 'HydroConfig', so it can be used by the river
-- stage without constructing a full 'HydroConfig'.
breachSinksLand
  :: Float          -- ^ waterLevel
  -> Float          -- ^ breachDepth
  -> Int -> Int     -- ^ gridW, gridH
  -> U.Vector Float -- ^ elevation grid
  -> U.Vector Float
breachSinksLand waterLevel breachDepth gridW gridH elev =
  U.generate (U.length elev) $ \i ->
    let !h0   = elev U.! i
        !hmin = neighborMin gridW gridH elev i
        isSink = hmin >= h0
    in if isSink && h0 > waterLevel then h0 - breachDepth else h0

-- | Fill land depressions using priority-flood so every land tile has a
-- monotonic flow path to the ocean (or grid boundary).
--
-- Algorithm (Barnes et al. 2014, \"Priority-Flood\"):
--
-- 1. Seed the priority queue with all grid-boundary tiles and all
--    submerged tiles (elevation ≤ @waterLevel@).
-- 2. Pop the lowest-elevation cell from the queue.
-- 3. For each unvisited hex neighbour, set its filled elevation to
--    @max(own_elev, pourer_elev + epsilon)@ and add it to the queue.
-- 4. Repeat until all cells are visited.
--
-- The tiny epsilon (@1e-5@) guarantees a strictly monotonic gradient
-- from filled tiles toward their pour point, so steepest-descent
-- routing always finds a unique downhill path.
--
-- Submerged tiles are never modified.  The result replaces the
-- elevation grid before flow routing.
fillDepressions
  :: Float          -- ^ waterLevel — tiles at or below are ocean seeds
  -> Int -> Int     -- ^ gridW, gridH
  -> U.Vector Float -- ^ elevation grid
  -> U.Vector Float
fillDepressions waterLevel gridW gridH elev = runST $ do
  let !n = gridW * gridH
      -- Tiny elevation increment to maintain strict monotonic gradient
      -- through filled areas.  Must be small enough not to distort
      -- terrain but large enough to break ties in flow routing.
      eps = 1e-5 :: Float
  filled  <- U.thaw elev
  visited <- UM.replicate n False

  -- Seed: boundary cells + submerged cells
  let isBoundary i =
        let !x = i `mod` gridW
            !y = i `div` gridW
        in x == 0 || x == gridW - 1 || y == 0 || y == gridH - 1
      mkSeed i = (elev U.! i, i)
      seeds = [ mkSeed i
              | i <- [0 .. n - 1]
              , isBoundary i || elev U.! i <= waterLevel
              ]

  -- Mark seeds as visited
  forM_ seeds $ \(_, i) -> UM.write visited i True

  -- Priority-flood loop using Data.Set as a min-heap on (elevation, index).
  -- Each (Float, Int) pair is unique by index.
  let loop !queue
        | Set.null queue = pure ()
        | otherwise = do
            let !((h, i), queue') = Set.deleteFindMin queue
            queue'' <- foldM (\q j -> do
              vis <- UM.read visited j
              if vis then pure q
              else do
                UM.write visited j True
                let !hj  = elev U.! j
                    !newH = max hj (h + eps)
                UM.write filled j newH
                pure (Set.insert (newH, j) q)
              ) queue' (hexNeighborIndices gridW gridH i)
            loop queue''

  loop (Set.fromList seeds)
  U.freeze filled

-- | Breach shallow isolated land-sinks by raising them just above the rim.
--
-- A single pass over the grid that finds every land local-minimum
-- (tile whose elevation is ≤ all 6 hex neighbours) and raises it to
-- @min(neighbour_elev) + epsilon@.  This gives steepest-descent
-- routing an exit towards the lowest neighbour.  Only sinks whose
-- depth (rim − floor) is at most @maxBreachDepth@ are modified —
-- deeper sinks are left intact as natural terrain features (lakes).
--
-- Submerged tiles (≤ @waterLevel@) are never modified.
--
-- __Limitation:__ a single parallel pass can only resolve /isolated/
-- single-tile sinks (tile strictly below all neighbours).  Multi-tile
-- flat depressions or plateaux require 'fillDepressions'.
--
-- Note: this function is /not/ currently wired into the pipeline
-- (river routing uses 'fillDepressions' instead), but it is exported
-- for testing and potential future use.
breachRemainingSinks
  :: Float          -- ^ waterLevel — tiles at or below are skipped
  -> Int -> Int     -- ^ gridW, gridH
  -> U.Vector Float -- ^ elevation grid
  -> U.Vector Float
breachRemainingSinks waterLevel gridW gridH elev =
  let eps = 1e-5 :: Float
      -- Only breach sinks whose depth (rim − floor) is at most this.
      -- Deeper sinks are left as natural features (lakes).
      maxBreachDepth = 0.05 :: Float
  in U.imap (\i h0 ->
    if h0 <= waterLevel
      then h0  -- submerged, leave alone
      else
        let nbrs  = hexNeighborIndices gridW gridH i
            nbrHs = map (elev U.!) nbrs
            hmin  = minimum nbrHs
            depth = hmin - h0  -- positive when h0 is below rim
        in if h0 <= hmin && depth <= maxBreachDepth
             then hmin + eps  -- raise just above lowest neighbour
             else h0
    ) elev

-- | Minimum elevation among a tile's 6 hex neighbours (and itself).
neighborMin :: Int -> Int -> U.Vector Float -> Int -> Float
neighborMin gridW gridH elev i =
  let h0 = elev U.! i
      nbrHeights = map (elev U.!) (hexNeighborIndices gridW gridH i)
  in minimum (h0 : nbrHeights)

-- | Compute steepest-descent flow directions using all 6 hex neighbours.
-- Returns the index of the lowest neighbour, or @-1@ if no neighbour is
-- lower (sink).
flowDirections :: Int -> Int -> U.Vector Float -> U.Vector Int
flowDirections gridW gridH elev =
  U.generate (U.length elev) $ \i ->
    let h0 = elev U.! i
        candidates = [(j, elev U.! j) | j <- hexNeighborIndices gridW gridH i]
        lower = filter (\(_, h) -> h < h0) candidates
    in case lower of
         [] -> -1
         _  -> fst (minimumByElevation lower)
  where
    minimumByElevation = foldl1 (\a b -> if snd a <= snd b then a else b)

-- | Like 'flowDirections' but treats submerged tiles (elevation ≤ waterLevel)
-- as terminal sinks.  Flow from a land tile never enters an ocean tile, and
-- submerged tiles themselves receive flow direction @-1@.  This confines
-- flow accumulation to the land surface so that river discharge totals
-- reflect continental drainage rather than ocean-floor topology.
-- Uses all 6 hex neighbours.
flowDirectionsLand :: Float -> Int -> Int -> U.Vector Float -> U.Vector Int
flowDirectionsLand waterLevel gridW gridH elev =
  U.generate (U.length elev) $ \i ->
    let h0 = elev U.! i
    in if h0 <= waterLevel
       then -1  -- submerged tile is a sink
       else
         let candidates = [(j, elev U.! j) | j <- hexNeighborIndices gridW gridH i]
             lowerLand = filter (\(_, h) -> h < h0) candidates
         in case lowerLand of
              [] -> -1
              _  -> fst (minimumByElevation lowerLand)
  where
    minimumByElevation = foldl1 (\a b -> if snd a <= snd b then a else b)

flowAccumulation :: HydroConfig -> U.Vector Float -> U.Vector Int -> U.Vector Float
flowAccumulation cfg elev flow = U.create $ do
  let n = U.length elev
  acc <- UM.replicate n (hcBaseAccumulation cfg)
  let indices = sortBy (comparing (\i -> negate (elev U.! i))) [0 .. n - 1]
  forM_ indices $ \i -> do
    let d = flow U.! i
    if d >= 0
      then do
        v <- UM.read acc i
        UM.modify acc (+ v) d
      else pure ()
  pure acc

applyStreamPowerErosion :: HydroConfig -> U.Vector Float -> U.Vector Int -> U.Vector Float -> U.Vector Float -> U.Vector Float
applyStreamPowerErosion cfg elev flow acc hardness = U.create $ do
  let n = U.length elev
  base <- U.thaw elev
  deposit <- UM.replicate n 0
  forM_ [0 .. n - 1] $ \i -> do
    let d = flow U.! i
    if d >= 0
      then do
        let h0 = elev U.! i
            h1 = elev U.! d
            slope = max 0 (h0 - h1)
            power = (acc U.! i) * slope
            erosion = min (hcStreamPowerMaxErosion cfg) (power * hcStreamPowerScale cfg * hardnessFactor cfg (hardness U.! i))
            depositAmt = erosion * hcStreamDepositRatio cfg
        UM.modify base (\v -> v - erosion) i
        UM.modify deposit (+ depositAmt) d
      else pure ()
  forM_ [0 .. n - 1] $ \i -> do
    d <- UM.read deposit i
    UM.modify base (+ d) i
  pure base

carveRiversGrid :: HydroConfig -> Int -> Int -> U.Vector Float -> U.Vector Float -> U.Vector Float -> U.Vector Float
carveRiversGrid cfg gridW gridH elev acc hardness =
  let maxAcc = max (hcMinAccumulation cfg) (U.maximum acc)
      waterLevel = hcWaterLevel cfg
  in U.imap (carveAt maxAcc waterLevel) elev
  where
    carveAt maxAcc waterLevel i h0 =
      let a = acc U.! i
          flowNorm = clamp01 (a / maxAcc)
          depth = min (hcRiverCarveMaxDepth cfg) (flowNorm * hcRiverCarveScale cfg * hardnessFactor cfg (hardness U.! i))
      in if h0 > waterLevel
          then h0 - depth
          else h0

riverBankErodeGrid :: HydroConfig -> Int -> Int -> U.Vector Float -> U.Vector Float -> U.Vector Float -> U.Vector Float
riverBankErodeGrid cfg gridW gridH elev acc hardness =
  let maxAcc = max (hcMinAccumulation cfg) (U.maximum acc)
      threshold = maxAcc * hcRiverBankThreshold cfg
  in U.generate (U.length elev) (bankAt threshold)
  where
    bankAt threshold i =
      let h0 = elev U.! i
          neighborHigh = any (\j -> acc U.! j > threshold)
                             (hexNeighborIndices gridW gridH i)
          bankDepth = hcRiverBankDepth cfg * hardnessFactor cfg (hardness U.! i)
      in if neighborHigh then h0 - bankDepth else h0

-- | Deposit alluvial sediment where rivers decelerate.
--
-- Deposition occurs on low-slope land tiles, scaled inversely with slope
-- (more deposit where the gradient is gentler) and by normalized flow.
-- A sink-protection guard prevents deposits from creating new local
-- minima: the deposit is clamped so the tile never rises above the
-- elevation of its lowest neighbour.
alluvialDepositGrid :: HydroConfig -> Int -> Int -> U.Vector Float -> U.Vector Float -> U.Vector Float
alluvialDepositGrid cfg gridW gridH elev acc =
  let maxAcc = max (hcMinAccumulation cfg) (U.maximum acc)
      maxSlope = hcAlluvialMaxSlope cfg
      waterLevel = hcWaterLevel cfg
  in U.generate (U.length elev) (depositAt maxAcc maxSlope waterLevel)
  where
    depositAt maxAcc maxSlope waterLevel i =
      let h0 = elev U.! i
          a = acc U.! i
          flowNorm = clamp01 (a / maxAcc)
          slope = gridSlopeAt gridW gridH elev i
          -- Deposit inversely proportional to slope (more on flatter ground)
          slopeFactor = if slope >= maxSlope then 0 else clamp01 (1 - slope / maxSlope)
          rawDeposit = if h0 > waterLevel
                         then flowNorm * slopeFactor * hcAlluvialDepositScale cfg
                         else 0
          -- Sink guard: never raise above the lowest neighbour
          nbrMin = neighborMin gridW gridH elev i
          maxDeposit = if nbrMin > h0 then nbrMin - h0 else 0
      in h0 + min rawDeposit maxDeposit

wetErodeGrid :: HydroConfig -> Int -> Int -> U.Vector Float -> U.Vector Float -> U.Vector Float -> U.Vector Float
wetErodeGrid cfg gridW gridH elev moisture hardness =
  let maxMoist = max (hcMinMoisture cfg) (U.maximum moisture)
      waterLevel = hcWaterLevel cfg
  in U.generate (U.length elev) (wetErodeAt maxMoist waterLevel)
  where
    wetErodeAt maxMoist waterLevel i =
      let h0 = elev U.! i
          m = moisture U.! i / maxMoist
          depth = clamp01 m * hcWetErodeScale cfg * hardnessFactor cfg (hardness U.! i)
      in if h0 > waterLevel then h0 - depth else h0

-- | Maximum absolute elevation difference to any of the 6 hex neighbours.
gridSlopeAt :: Int -> Int -> U.Vector Float -> Int -> Float
gridSlopeAt gridW gridH elev i =
  let h0 = elev U.! i
      nbrs = hexNeighborIndices gridW gridH i
  in case nbrs of
       [] -> 0
       _  -> maximum [abs (elev U.! j - h0) | j <- nbrs]

coastalErodeGrid :: HydroConfig -> Int -> Int -> U.Vector Float -> U.Vector Float -> U.Vector Float
coastalErodeGrid cfg gridW gridH elev hardness =
  let waterLevel = hcWaterLevel cfg
      strength = hcCoastalErodeStrength cfg
      raiseFactor = hcCoastalRaiseFactor cfg
  in U.generate (U.length elev) (coastalAt cfg gridW gridH waterLevel strength raiseFactor elev hardness)

coastalAt :: HydroConfig -> Int -> Int -> Float -> Float -> Float -> U.Vector Float -> U.Vector Float -> Int -> Float
coastalAt cfg gridW gridH waterLevel strength raiseFactor elev hardness i =
  let h0 = elev U.! i
      localStrength = strength * hardnessFactor cfg (hardness U.! i)
      neighbors = map (elev U.!) (hexNeighborIndices gridW gridH i)
      anyWater = any (< waterLevel) neighbors
      anyLand = any (>= waterLevel) neighbors
      lower = min localStrength (h0 - waterLevel)
      raise = min (localStrength * raiseFactor) (waterLevel - h0)
  in if h0 >= waterLevel && anyWater
      then h0 - max 0 lower
      else if h0 < waterLevel && anyLand
        then h0 + max 0 raise
        else h0

moistureFromAccumulation :: HydroConfig -> U.Vector Float -> U.Vector Float -> U.Vector Float
moistureFromAccumulation cfg elev acc =
  let maxAcc = max (hcMinAccumulation cfg) (U.maximum acc)
      waterLevel = hcWaterLevel cfg
  in U.imap (\i a ->
    let base = clamp01 (waterLevel - elev U.! i)
        flowM = clamp01 (a / maxAcc)
    in clamp01 (base * hcMoistureBaseWeight cfg + flowM * hcMoistureFlowWeight cfg)) acc

flowAccumulationWithBase :: Float -> U.Vector Float -> U.Vector Int -> U.Vector Float
flowAccumulationWithBase baseAccum elev flow = U.create $ do
  let n = U.length elev
  acc <- UM.replicate n baseAccum
  let indices = sortBy (comparing (\i -> negate (elev U.! i))) [0 .. n - 1]
  forM_ indices $ \i -> do
    let d = flow U.! i
    if d >= 0
      then do
        v <- UM.read acc i
        UM.modify acc (+ v) d
      else pure ()
  pure acc

basinIdsFromFlow :: U.Vector Int -> U.Vector Word32
basinIdsFromFlow flow = U.map fromIntegral $ U.create $ do
  let n = U.length flow
  ids <- UM.replicate n (-1)
  let resolve i = do
        v <- UM.read ids i
        if v >= 0
          then pure v
          else do
            let d = flow U.! i
            if d < 0
              then UM.write ids i i >> pure i
              else do
                r <- resolve d
                UM.write ids i r
                pure r
  forM_ [0 .. n - 1] resolve
  pure ids

basinRechargeStats :: U.Vector Word32 -> U.Vector Float -> GroundwaterConfig -> IntMap (Float, Int)
basinRechargeStats basinIds moisture cfg =
  let recharge = U.map (* gwRechargeScale cfg) (U.map clamp01 moisture)
  in U.ifoldl'
      (\acc i bid ->
        let key = fromIntegral bid
            value = recharge U.! i
        in IntMap.insertWith
            (\(r1, c1) (r0, c0) -> (r0 + r1, c0 + c1))
            key
            (value, 1)
            acc)
      IntMap.empty
      basinIds

basinStorageStats
  :: IntMap (Float, Int)
  -> GroundwaterConfig
  -> (IntMap Float, IntMap Float, IntMap Int)
basinStorageStats stats cfg =
  let toStorage (rechargeSum, count) =
        let eligible = count >= gwMinBasinSize cfg
            storage = if eligible then rechargeSum * gwStorageScale cfg else 0
            discharge = if eligible then storage * gwDischargeScale cfg * gwPermeability cfg else 0
        in (storage, discharge, count)
  in IntMap.foldlWithKey'
      (\(storageMap, dischargeMap, sizeMap) key value ->
        let (storage, discharge, count) = toStorage value
        in ( IntMap.insert key storage storageMap
           , IntMap.insert key discharge dischargeMap
           , IntMap.insert key count sizeMap
           ))
      (IntMap.empty, IntMap.empty, IntMap.empty)
      stats

basinBaseflow :: U.Vector Word32 -> IntMap Float -> IntMap Int -> Float -> U.Vector Float
basinBaseflow basinIds dischargeMap sizeMap scale =
  U.imap
    (\_ bid ->
      let key = fromIntegral bid
          discharge = IntMap.findWithDefault 0 key dischargeMap
          size = max 1 (IntMap.findWithDefault 1 key sizeMap)
      in (discharge / fromIntegral size) * scale)
    basinIds

basinPerTile :: U.Vector Word32 -> IntMap Float -> IntMap Int -> U.Vector Float
basinPerTile basinIds valueMap sizeMap =
  U.imap
    (\_ bid ->
      let key = fromIntegral bid
          value = IntMap.findWithDefault 0 key valueMap
          size = max 1 (IntMap.findWithDefault 1 key sizeMap)
      in value / fromIntegral size)
    basinIds

riverDepthWithHardness :: U.Vector Float -> U.Vector Float -> RiverConfig -> U.Vector Float
riverDepthWithHardness acc hardness cfg =
  U.imap
    (\i a ->
      if a < rcMinAccumulation cfg
        then 0
        else min (rcChannelMaxDepth cfg) (a * rcChannelDepthScale cfg * riverDepthFactor cfg (hardness U.! i)))
    acc

riverErosionPotential
  :: Int
  -> Int
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Float
  -> RiverConfig
  -> U.Vector Float
riverErosionPotential gridW gridH elev acc hardness cfg =
  let maxAcc = max (rcMinAccumulation cfg) (U.maximum acc)
  in U.imap
      (\i a ->
        if a < rcMinAccumulation cfg
          then 0
          else
            let flowNorm = clamp01 (a / maxAcc)
                slope = gridSlopeAt gridW gridH elev i
                hard = riverErosionFactor cfg (hardness U.! i)
            in flowNorm * slope * rcErosionScale cfg * hard)
      acc

riverDepositPotential
  :: Int
  -> Int
  -> U.Vector Float
  -> U.Vector Float
  -> RiverConfig
  -> U.Vector Float
riverDepositPotential gridW gridH elev acc cfg =
  let maxAcc = max (rcMinAccumulation cfg) (U.maximum acc)
      maxSlope = rcDepositMaxSlope cfg
  in U.imap
      (\i a ->
        if a < rcMinAccumulation cfg
          then 0
          else
            let slope = gridSlopeAt gridW gridH elev i
                flowNorm = clamp01 (a / maxAcc)
                slopeFactor = if slope >= maxSlope then 0 else clamp01 (1 - slope / maxSlope)
            in flowNorm * slopeFactor * rcDepositScale cfg)
      acc

hardnessFactor :: HydroConfig -> Float -> Float
hardnessFactor cfg hard =
  clamp01 (1 - clamp01 hard * hcHardnessErodeWeight cfg)

riverDepthFactor :: RiverConfig -> Float -> Float
riverDepthFactor cfg hard =
  clamp01 (1 - clamp01 hard * rcHardnessDepthWeight cfg)

riverErosionFactor :: RiverConfig -> Float -> Float
riverErosionFactor cfg hard =
  clamp01 (1 - clamp01 hard * rcHardnessErosionWeight cfg)

strahlerOrder
  :: Int
  -> Int
  -> U.Vector Float
  -> U.Vector Int
  -> U.Vector Float
  -> Float
  -> U.Vector Word16
strahlerOrder gridW gridH elev flow acc minAccum = U.create $ do
  let n = U.length elev
  orders <- UM.replicate n (0 :: Word16)
  let indices = sortBy (comparing (\i -> negate (elev U.! i))) [0 .. n - 1]
  forM_ indices $ \i -> do
    let a = acc U.! i
    if a < minAccum
      then UM.write orders i 0
      else do
        ups <- upstreamOrders gridW gridH flow orders i
        case ups of
          [] -> UM.write orders i 1
          _ -> do
            let maxO = maximum ups
                countMax = length (filter (== maxO) ups)
                nextOrder = if countMax >= 2 then maxO + 1 else maxO
            UM.write orders i nextOrder
  pure orders

-- | Compute Strahler stream orders using 6 hex neighbours for upstream
-- detection.
upstreamOrders :: Int -> Int -> U.Vector Int -> UM.MVector s Word16 -> Int -> ST s [Word16]
upstreamOrders gridW gridH flow orders i =
  let incoming = filter (\j -> flow U.! j == i) (hexNeighborIndices gridW gridH i)
  in mapM (UM.read orders) incoming

sliceRiverChunk :: WorldConfig -> ChunkCoord -> Int -> RiverChunk -> Int -> TerrainChunk -> RiverChunk
sliceRiverChunk config minCoord gridW rivers _key _chunk =
  let flowSlice = chunkGridSliceGeneric config minCoord gridW (rcFlowDir rivers) _key
      -- Segment data is global-grid-indexed via offsets; we need to
      -- re-index for the chunk-local tile set.
      size  = wcChunkSize config
      n     = size * size
      ChunkCoord cx cy = chunkCoordFromId (ChunkId _key)
      ChunkCoord minCx minCy = minCoord
      baseX = (cx - minCx) * size
      baseY = (cy - minCy) * size
      globalOffsets = rcSegOffsets rivers
      globalEntry   = rcSegEntryEdge rivers
      globalExit    = rcSegExitEdge rivers
      globalDisc    = rcSegDischarge rivers
      globalOrd     = rcSegOrder rivers
      -- Build local offsets and segment slices
      segCounts = U.generate n (\i ->
        let lx = i `mod` size
            ly = i `div` size
            gx = baseX + lx
            gy = baseY + ly
            gi = gy * gridW + gx
        in if gi + 1 < U.length globalOffsets
           then globalOffsets U.! (gi + 1) - globalOffsets U.! gi
           else 0)
      localOffsets = U.scanl' (+) 0 segCounts
      totalLocalSegs = U.last localOffsets
      -- Collect global segment indices for this chunk
      globalSegIndices = U.concatMap (\i ->
        let lx = i `mod` size
            ly = i `div` size
            gx = baseX + lx
            gy = baseY + ly
            gi = gy * gridW + gx
            start = globalOffsets U.! gi
            end   = globalOffsets U.! (gi + 1)
        in U.generate (end - start) (+ start)) (U.enumFromN 0 n)
      localEntry = U.map (globalEntry U.!) globalSegIndices
      localExit  = U.map (globalExit U.!) globalSegIndices
      localDisc  = U.map (globalDisc U.!) globalSegIndices
      localOrd   = U.map (globalOrd U.!) globalSegIndices
  in RiverChunk
    { rcFlowAccum = chunkGridSlice config minCoord gridW (rcFlowAccum rivers) _key
    , rcDischarge = chunkGridSlice config minCoord gridW (rcDischarge rivers) _key
    , rcChannelDepth = chunkGridSlice config minCoord gridW (rcChannelDepth rivers) _key
    , rcRiverOrder = chunkGridSliceGeneric config minCoord gridW (rcRiverOrder rivers) _key
    , rcBasinId = chunkGridSliceGeneric config minCoord gridW (rcBasinId rivers) _key
    , rcBaseflow = chunkGridSlice config minCoord gridW (rcBaseflow rivers) _key
    , rcErosionPotential = chunkGridSlice config minCoord gridW (rcErosionPotential rivers) _key
    , rcDepositPotential = chunkGridSlice config minCoord gridW (rcDepositPotential rivers) _key
    , rcFlowDir = flowSlice
    , rcSegOffsets = localOffsets
    , rcSegEntryEdge = localEntry
    , rcSegExitEdge = localExit
    , rcSegDischarge = localDisc
    , rcSegOrder = localOrd
    }

sliceGroundwaterChunk :: WorldConfig -> ChunkCoord -> Int -> GroundwaterChunk -> Int -> TerrainChunk -> GroundwaterChunk
sliceGroundwaterChunk config minCoord gridW groundwater _key _chunk =
  GroundwaterChunk
    { gwStorage = chunkGridSlice config minCoord gridW (gwStorage groundwater) _key
    , gwRecharge = chunkGridSlice config minCoord gridW (gwRecharge groundwater) _key
    , gwDischarge = chunkGridSlice config minCoord gridW (gwDischarge groundwater) _key
    , gwBasinId = chunkGridSliceGeneric config minCoord gridW (gwBasinId groundwater) _key
    }

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
