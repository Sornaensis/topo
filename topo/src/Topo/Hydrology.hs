{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
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
    -- * Sink breaching (exported for testing)
  , breachSinksLand
    -- * Grid operations (exported for testing)
  , flowAccumulation
  , piedmontSmoothGrid
  , carveRiversGrid
  , alluvialDepositGrid
  ) where

import Control.Monad (forM_, when, foldM)
import Control.Monad.ST (ST, runST)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Word (Word16, Word32)
import GHC.Generics (Generic)
import Topo.Config.JSON
  (ToJSON(..), FromJSON(..), configOptions, mergeDefaults,
   genericToJSON, genericParseJSON)
import Topo.Math (clamp01)
import Topo.Hex (hexNeighborIndices)
import Topo.Parameters (TerrainFormConfig)
import Topo.Pipeline (PipelineStage(..))
import Topo.Pipeline.Stage (StageId(..))
import Control.Monad.Except (throwError)
import Topo.Plugin (logInfo, getWorldP, putWorldP, PluginError(..))
import Topo.River (RiverTopologyConfig(..), defaultRiverTopologyConfig, computeRiverSegments)
import Topo.TerrainForm.Modifiers
  ( TerrainFormModifiers(..)
  , defaultTerrainFormModifiers
  )
import Topo.TerrainGrid
  ( buildElevationGrid
  , buildHardnessGrid
  , buildMoistureGrid
  , buildPlateHardnessGrid
  , classifyTerrainFormGrid
  , gridSlopeAt
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
--
-- Controls flow routing, erosion, moisture derivation, and sediment
-- transport across the terrain grid.
data HydroConfig = HydroConfig
  { hcWaterLevel :: !Float
    -- ^ Global water level [0..1 normalised elevation].
    -- Tiles at or below this elevation are considered submerged.
  , hcSinkBreachDepth :: !Float
    -- ^ Maximum depth to breach interior sinks during depression
    -- filling (elevation units).
  , hcBaseAccumulation :: !Float
    -- ^ Starting flow accumulation per tile.
  , hcMinAccumulation :: !Float
    -- ^ Floor for flow accumulation (prevents zero-accumulation tiles).
  , hcStreamPowerMaxErosion :: !Float
    -- ^ Maximum stream power erosion per step (elevation units).
  , hcStreamPowerScale :: !Float
    -- ^ Scaling factor for stream power calculation.
  , hcStreamDepositRatio :: !Float
    -- ^ Fraction of eroded material deposited downstream [0..1].
  , hcRiverCarveMaxDepth :: !Float
    -- ^ Maximum river channel carving depth (elevation units).
  , hcRiverCarveScale :: !Float
    -- ^ Scale factor for river carving intensity.
  , hcRiverBankThreshold :: !Float
    -- ^ Flow accumulation threshold for bank erosion to begin.
  , hcRiverBankDepth :: !Float
    -- ^ Bank erosion depth (elevation units).
  , hcAlluvialMaxSlope :: !Float
    -- ^ Maximum slope for alluvial (flat-water) deposits.
  , hcAlluvialDepositScale :: !Float
    -- ^ Alluvial deposit rate (elevation units per step).
  , hcWetErodeScale :: !Float
    -- ^ Wet-area diffuse erosion scale.
  , hcCoastalErodeStrength :: !Float
    -- ^ Coastal erosion strength (elevation units per step).
  , hcCoastalRaiseFactor :: !Float
    -- ^ Fraction of coastal erosion redeposited as coastal raise [0..1].
  -- | [0..1] scaling of hardness against erosion intensity.
  , hcHardnessErodeWeight :: !Float
  , hcMoistureBaseWeight :: !Float
    -- ^ Base moisture weight in the moisture blending formula [0..1].
  , hcMoistureFlowWeight :: !Float
    -- ^ Flow-based moisture weight in the moisture blend [0..1].
  , hcPiedmontSmoothStrength :: !Float
    -- ^ Blend factor toward neighbor mean in the piedmont zone [0..1].
    -- Higher values produce wider, smoother foothills aprons.
  , hcPiedmontSlopeMin :: !Float
    -- ^ Minimum slope for the piedmont zone (normalised elevation units).
    -- Tiles with slope below this are flat plains and are not smoothed.
  , hcPiedmontSlopeMax :: !Float
    -- ^ Maximum slope for the piedmont zone (normalised elevation units).
    -- Tiles steeper than this are mountains and are not smoothed.
  , hcMinMoisture :: !Float
    -- ^ Minimum moisture floor for all tiles.
  } deriving (Eq, Show, Generic)

instance ToJSON HydroConfig where
  toJSON = genericToJSON (configOptions "hc")

instance FromJSON HydroConfig where
  parseJSON v = genericParseJSON (configOptions "hc")
                  (mergeDefaults (toJSON defaultHydroConfig) v)

-- | Default hydrology configuration.
defaultHydroConfig :: HydroConfig
defaultHydroConfig = HydroConfig
  { hcWaterLevel = 0.5
  , hcSinkBreachDepth = 0.02
  , hcBaseAccumulation = 1
  , hcMinAccumulation = 1
  , hcStreamPowerMaxErosion = 0.08
  , hcStreamPowerScale = 0.00005
  , hcStreamDepositRatio = 0.4
  , hcRiverCarveMaxDepth = 0.05
  , hcRiverCarveScale = 0.03
  , hcRiverBankThreshold = 0.35
  , hcRiverBankDepth = 0.01
  , hcAlluvialMaxSlope = 0.12
  , hcAlluvialDepositScale = 0.06
  , hcWetErodeScale = 0.015
  , hcCoastalErodeStrength = 0.04
  , hcCoastalRaiseFactor = 0.6
  , hcHardnessErodeWeight = 0.7
  , hcMoistureBaseWeight = 0.6
  , hcMoistureFlowWeight = 0.7
  , hcPiedmontSmoothStrength = 0.25
  , hcPiedmontSlopeMin = 0.03
  , hcPiedmontSlopeMax = 0.12
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
  } deriving (Eq, Show, Generic)

instance ToJSON RiverConfig where
  toJSON = genericToJSON (configOptions "rc")

instance FromJSON RiverConfig where
  parseJSON v = genericParseJSON (configOptions "rc")
                  (mergeDefaults (toJSON defaultRiverConfig) v)

-- | Default river routing parameters.
defaultRiverConfig :: RiverConfig
defaultRiverConfig = RiverConfig
  { rcBaseAccumulation = 1
  , rcMinAccumulation = 4
  , rcOrderMinAccumulation = 6
  , rcDischargeScale = 0.05
  , rcChannelDepthScale = 0.002
  , rcChannelMaxDepth = 0.2
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
    -- ^ Groundwater recharge rate from surface moisture [0..1].
  , gwStorageScale :: !Float
    -- ^ Groundwater storage capacity scaling.
  , gwDischargeScale :: !Float
    -- ^ Groundwater discharge rate back to surface flow.
  , gwPermeability :: !Float
    -- ^ Soil permeability factor affecting recharge [0..1].
  , gwMinBasinSize :: !Int
    -- ^ Minimum basin size (tiles) for groundwater calculation.
  } deriving (Eq, Show, Generic)

instance ToJSON GroundwaterConfig where
  toJSON = genericToJSON (configOptions "gw")

instance FromJSON GroundwaterConfig where
  parseJSON v = genericParseJSON (configOptions "gw")
                  (mergeDefaults (toJSON defaultGroundwaterConfig) v)

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
--
-- The 'TerrainFormConfig' drives a lightweight pre-classification of
-- terrain forms from the current elevation + plate hardness grids.
-- Per-form modifiers ('defaultTerrainFormModifiers') then adjust river
-- carving, bank erosion, stream-power erosion, deposition, coastal
-- reshaping, piedmont smoothing, and wet-area erosion.
applyHydrologyStage :: HydroConfig -> TerrainFormConfig -> PipelineStage
applyHydrologyStage cfg formCfg = PipelineStage StageHydrology "applyHydrology" "applyHydrology" Nothing [] Nothing $ do
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
      plateHardness = buildPlateHardnessGrid config terrain (ChunkCoord minCx minCy) gridW gridH
      -- Pre-classify terrain forms from current elevation + plate hardness.
      wl = hcWaterLevel cfg
      formGrid = classifyTerrainFormGrid formCfg wl gridW gridH elev0 plateHardness
      modLookup = defaultTerrainFormModifiers
      erosionMult   = U.map (tfmErosionRate . modLookup) formGrid
      depositFactor = U.map (\f -> 1 - tfmDepositSuppression (modLookup f)) formGrid
      smoothResist  = U.map (tfmSmoothResistance . modLookup) formGrid
      flowBonus     = U.map (tfmFlowBonus . modLookup) formGrid
      -- Fill depressions so every land tile drains to the ocean.
      -- The filled surface is used ONLY for flow routing and accumulation;
      -- erosion is applied to the original terrain so the displayed surface
      -- matches the actual flow paths carved by erosion.
      elev1 = fillDepressions wl gridW gridH elev0
      flow = flowDirections gridW gridH elev1
      acc = flowAccumulation cfg flowBonus elev1 flow
      elevCarved = carveRiversGrid cfg gridW gridH elev0 acc hardness erosionMult
      elevBanks = riverBankErodeGrid cfg gridW gridH elevCarved acc hardness erosionMult
      elev2 = applyStreamPowerErosion cfg elevBanks flow acc hardness erosionMult depositFactor
      elev3 = coastalErodeGrid cfg gridW gridH elev2 hardness erosionMult smoothResist
      elev3b = piedmontSmoothGrid cfg gridW gridH elev3 formGrid smoothResist
      elev4 = alluvialDepositGrid cfg gridW gridH elev3b acc depositFactor
      moisture = moistureFromAccumulation cfg elev4 acc
      elev5 = wetErodeGrid cfg gridW gridH elev4 moisture hardness erosionMult
      -- Floor-clamp: the erosion chain (carve → bank → stream-power →
      -- coastal → piedmont → alluvial → wet) can push land tiles at or
      -- below waterLevel, creating inland "puddles".  flowDirectionsLand
      -- treats these as terminal sinks, so rivers terminate there
      -- instead of reaching the coast.  Clamp any originally-land tile
      -- that erosion pushed at-or-below waterLevel back to just above.
      elevFinal = U.imap (\i h ->
        if elev0 U.! i > wl && h <= wl
          then wl + 1e-5
          else h) elev5
      terrain' = IntMap.mapWithKey (updateChunkElevationFromGrid config (ChunkCoord minCx minCy) gridW elevFinal) terrain
      terrain'' = IntMap.mapWithKey (updateChunkMoistureFromGrid config (ChunkCoord minCx minCy) gridW moisture) terrain'
  putWorldP world { twTerrain = terrain'' }

-- | Apply river routing and basin-level groundwater storage.
--
-- The water level parameter is the global sea-level threshold used for
-- flow sinks during river routing.  It is typically sourced from
-- 'hcWaterLevel' via the pipeline orchestrator.
--
-- A 'RiverTopologyConfig' controls segment extraction parameters
-- (discharge thresholds, network pruning, ocean-reachability, etc.).
applyRiverStage
  :: RiverConfig
  -> RiverTopologyConfig
  -> GroundwaterConfig
  -> Float          -- ^ Water level (from 'HydroConfig')
  -> PipelineStage
applyRiverStage riverCfg topoCfg gwCfg waterLevel = PipelineStage StageRivers "applyRivers" "applyRivers" Nothing [] Nothing $ do
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
      wl = waterLevel
      -- Fill depressions for routing so rivers follow connected drainage
      -- paths to the sea (or large lake).  Pre-breach single-tile sinks
      -- first (Phase E.2) to reduce the number of epsilon-gradient flat
      -- zones that fillDepressions would otherwise create.
      elevBreached = breachSinksLand wl 0.02 gridW gridH elev0
      elevFilled = fillDepressions wl gridW gridH elevBreached
      flow = flowDirectionsLand wl gridW gridH elevFilled
      acc = flowAccumulationWithBase (rcBaseAccumulation riverCfg) elevFilled flow
      basinIds = basinIdsFromFlow flow
      basinStats = basinRechargeStats basinIds moisture gwCfg
      (basinStorage, basinDischarge, basinSize) = basinStorageStats basinStats gwCfg
      baseflow = basinBaseflow basinIds basinDischarge basinSize (rcBaseflowScale riverCfg)
      riverOrder = strahlerOrder gridW gridH elevFilled flow acc (rcOrderMinAccumulation riverCfg)
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
        computeRiverSegments topoCfg gridW gridH flow discharge riverOrder elevFilled elev0 wl
      groundwater = GroundwaterChunk
        { gwStorage = basinPerTile basinIds basinStorage basinSize
        , gwRecharge = U.map (* gwRechargeScale gwCfg) (U.map clamp01 moisture)
        , gwDischarge = basinPerTile basinIds basinDischarge basinSize
        , gwBasinId = basinIds
        , gwInfiltration = U.empty
        , gwWaterTableDepth = U.empty
        , gwRootZoneMoisture = U.empty
        }
      rivers' = IntMap.mapWithKey (sliceRiverChunk config (ChunkCoord minCx minCy) gridW rivers) terrain
      groundwater' = IntMap.mapWithKey (sliceGroundwaterChunk config (ChunkCoord minCx minCy) gridW groundwater) terrain
      -- Create lakes at inland depressions where rivers terminate.
      -- Uses the river discharge threshold: any land sink receiving at
      -- least rtMinDischarge worth of flow becomes a lake.
      elevWithLakes = createRiverLakes wl (rtMinDischarge topoCfg) 4 gridW gridH elev0 flow discharge
      terrain' = IntMap.mapWithKey (updateChunkElevationFromGrid config (ChunkCoord minCx minCy) gridW elevWithLakes) terrain
      rivers'' = IntMap.mapWithKey (sliceRiverChunk config (ChunkCoord minCx minCy) gridW rivers) terrain'
      groundwater'' = IntMap.mapWithKey (sliceGroundwaterChunk config (ChunkCoord minCx minCy) gridW groundwater) terrain'
  putWorldP world { twTerrain = terrain', twRivers = rivers'', twGroundwater = groundwater'' }

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
--
-- The breached elevation is floor-clamped at @waterLevel + 1e-5@ so that
-- tiles near sea level (e.g. those placed by the post-erosion clamp at
-- @waterLevel + 1e-5@) are never pushed below @waterLevel@.  Without this,
-- 'fillDepressions' would treat the newly-submerged tile as a seed and
-- 'flowDirectionsLand' would mark it as a terminal sink, causing rivers
-- to display inland delta fans on tiles that still render as land.
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
        -- Floor: never breach below waterLevel.
        !floor = waterLevel + 1e-5
    in if isSink && h0 > waterLevel
       then max floor (h0 - breachDepth)
       else h0

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
      -- Elevation increment to maintain strict monotonic gradient
      -- through filled areas.  Must be larger than rtMinSlope (5e-5) so
      -- that depression-filled zones produce visible river segments,
      -- yet small enough not to distort terrain visually.
      eps = 1e-4 :: Float
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
              , isBoundary i || elev U.! i < waterLevel
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
  :: Float          -- ^ waterLevel — tiles below this are skipped
  -> Int -> Int     -- ^ gridW, gridH
  -> U.Vector Float -- ^ elevation grid
  -> U.Vector Float
breachRemainingSinks waterLevel gridW gridH elev =
  let eps = 1e-5 :: Float
      -- Only breach sinks whose depth (rim − floor) is at most this.
      -- Deeper sinks are left as natural features (lakes).
      maxBreachDepth = 0.05 :: Float
  in U.imap (\i h0 ->
    if h0 < waterLevel
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

-- | Expand inland depressions that receive river discharge into proper
-- lakes.
--
-- After flow routing, submerged tiles (@elev ≤ waterLevel@) that are not
-- connected to the grid boundary form inland depressions.  When rivers
-- drain into such depressions but the submerged area is too small to be
-- classified as a lake by 'Topo.WaterBody' (< @minLakeSize@ tiles), the
-- river appears to terminate on dry land — an inland delta artifact.
--
-- This function:
--
--  1. Computes an ocean mask (boundary-connected submerged tiles via BFS).
--  2. Groups remaining inland submerged tiles into connected components.
--  3. For each inland component that receives significant river discharge
--     (any adjacent land tile has @discharge ≥ minDischarge@ flowing
--     into the component), expands the basin by lowering the cheapest
--     adjacent land tiles to just below @waterLevel@ until the basin
--     reaches @minLakeSize@ tiles.
--
-- Returns the modified elevation grid.  Only tiles that need lowering
-- to form lakes are changed; everything else is untouched.
createRiverLakes
  :: Float          -- ^ waterLevel
  -> Float          -- ^ minDischarge — minimum river discharge to trigger lake
  -> Int            -- ^ minLakeSize — expand basins to at least this many tiles
  -> Int -> Int     -- ^ gridW, gridH
  -> U.Vector Float -- ^ elevation grid (post-erosion)
  -> U.Vector Int   -- ^ flow directions (-1 = sink)
  -> U.Vector Float -- ^ discharge per tile
  -> U.Vector Float -- ^ modified elevation with lake beds
createRiverLakes waterLevel minDischarge minLakeSize gridW gridH elev flow discharge = runST $ do
  let !n = gridW * gridH

  -- Step 1: Ocean mask — BFS from boundary submerged tiles.
  oceanMask <- UM.replicate n False
  do
    queue <- UM.replicate n (0 :: Int)
    headR <- UM.replicate 1 (0 :: Int)
    tailR <- UM.replicate 1 (0 :: Int)
    let enqueue v = do
          t <- UM.read tailR 0
          UM.write queue t v
          UM.write tailR 0 (t + 1)
        dequeue = do
          h <- UM.read headR 0
          t <- UM.read tailR 0
          if h >= t then pure Nothing
          else do
            v <- UM.read queue h
            UM.write headR 0 (h + 1)
            pure (Just v)
    -- Seed: boundary tiles that are submerged
    forM_ [0 .. n - 1] $ \i -> do
      let !x = i `mod` gridW
          !y = i `div` gridW
          onBoundary = x == 0 || x == gridW - 1
                    || y == 0 || y == gridH - 1
      when (onBoundary && elev U.! i < waterLevel) $ do
        UM.write oceanMask i True
        enqueue i
    -- Flood through hex-connected submerged tiles
    let bfsLoop = do
          mi <- dequeue
          case mi of
            Nothing -> pure ()
            Just cur -> do
              forM_ (hexNeighborIndices gridW gridH cur) $ \ni -> do
                vis <- UM.read oceanMask ni
                when (not vis && elev U.! ni < waterLevel) $ do
                  UM.write oceanMask ni True
                  enqueue ni
              bfsLoop
    bfsLoop

  -- Step 2: Find inland submerged components via flood-fill.
  compId <- UM.replicate n (-1 :: Int)
  nextLabel <- UM.replicate 1 (0 :: Int)
  -- Reuse a stack buffer for flood fill
  stackBuf <- UM.replicate n (0 :: Int)

  forM_ [0 .. n - 1] $ \i -> do
    isOcean <- UM.read oceanMask i
    cid <- UM.read compId i
    when (not isOcean && elev U.! i < waterLevel && cid < 0) $ do
      label <- UM.read nextLabel 0
      UM.write nextLabel 0 (label + 1)
      -- Flood-fill this inland component
      UM.write compId i label
      UM.write stackBuf 0 i
      let fillLoop !top
            | top < 0 = pure ()
            | otherwise = do
                cur <- UM.read stackBuf top
                let top' = top - 1
                foldM (\t ni -> do
                  isO <- UM.read oceanMask ni
                  c <- UM.read compId ni
                  if not isO && elev U.! ni < waterLevel && c < 0
                    then do
                      UM.write compId ni label
                      let t' = t + 1
                      UM.write stackBuf t' ni
                      pure t'
                    else pure t
                  ) top' (hexNeighborIndices gridW gridH cur) >>= fillLoop
      fillLoop 0

  numLabels <- UM.read nextLabel 0

  -- Step 3: Compute per-component stats: size and whether it receives
  -- river discharge.
  compSize <- UM.replicate (max 1 numLabels) (0 :: Int)
  compFed  <- UM.replicate (max 1 numLabels) False

  frozenCompId <- U.freeze compId
  forM_ [0 .. n - 1] $ \i -> do
    let cid = frozenCompId U.! i
    when (cid >= 0) $ do
      UM.modify compSize (+ 1) cid
      -- Check if any hex-neighbour is a land tile with significant
      -- discharge flowing INTO this submerged component.
      let hasRiverInflow = any (\ni ->
            elev U.! ni >= waterLevel         -- neighbour is land
            && flow U.! ni == i               -- flows into this tile
            && discharge U.! ni >= minDischarge  -- significant discharge
            ) (hexNeighborIndices gridW gridH i)
      when hasRiverInflow $ UM.write compFed cid True

  -- Step 4: For each river-fed inland component that is too small,
  -- expand by lowering the cheapest adjacent land tiles.
  result <- U.thaw elev

  forM_ [0 .. numLabels - 1] $ \cid -> do
    sz <- UM.read compSize cid
    fed <- UM.read compFed cid
    when (fed && sz < minLakeSize) $ do
      -- Gather the ring of land tiles adjacent to this component,
      -- sorted by elevation (lowest first).  Lower them to just below
      -- waterLevel until the component is large enough.
      let candidates = Set.fromList
            [ (elev U.! ni, ni)
            | i <- [0 .. n - 1]
            , frozenCompId U.! i == cid
            , ni <- hexNeighborIndices gridW gridH i
            , elev U.! ni >= waterLevel
            , frozenCompId U.! ni < 0   -- not already in a component
            ]
      let expandLoop !curSz !frontier
            | curSz >= minLakeSize = pure ()
            | Set.null frontier    = pure ()
            | otherwise = do
                let !((_h, idx), frontier') = Set.deleteFindMin frontier
                UM.write result idx (waterLevel - 1e-4)
                -- Add this tile's land neighbours to the frontier
                let newCandidates =
                      [ (elev U.! ni, ni)
                      | ni <- hexNeighborIndices gridW gridH idx
                      , elev U.! ni >= waterLevel
                      , frozenCompId U.! ni < 0
                      ]
                expandLoop (curSz + 1)
                  (foldl (\s p -> Set.insert p s) frontier' newCandidates)
      expandLoop sz candidates

  U.freeze result

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

-- | Like 'flowDirections' but treats submerged tiles (elevation < waterLevel)
-- as terminal sinks.  Submerged tiles always receive flow direction @-1@;
-- land tiles may flow into submerged neighbours as terminal sinks (river
-- mouths).  This confines flow /accumulation/ to the land surface so that
-- river discharge totals reflect continental drainage rather than
-- ocean-floor topology.
-- Uses all 6 hex neighbours.
flowDirectionsLand :: Float -> Int -> Int -> U.Vector Float -> U.Vector Int
flowDirectionsLand waterLevel gridW gridH elev =
  U.generate (U.length elev) $ \i ->
    let h0 = elev U.! i
    in if h0 < waterLevel
       then -1  -- submerged tile is a sink
       else
         let candidates = [(j, elev U.! j) | j <- hexNeighborIndices gridW gridH i]
             lowerLand = filter (\(_, h) -> h < h0) candidates
         in case lowerLand of
              [] -> -1
              _  -> fst (minimumByElevation lowerLand)
  where
    minimumByElevation = foldl1 (\a b -> if snd a <= snd b then a else b)

-- | Accumulate flow from high to low elevation.
--
-- The @flowBonus@ vector carries a per-tile additive multiplier from
-- terrain form modifiers.  When a tile sends its accumulated flow
-- downstream, the amount is scaled by @1 + bonus@.  Badlands (high
-- runoff), passes (topographic funnelling), and valleys (flow
-- convergence) all amplify downstream accumulation via positive
-- bonuses.  Plateaus (negative bonus) reduce it.
flowAccumulation :: HydroConfig -> U.Vector Float -> U.Vector Float -> U.Vector Int -> U.Vector Float
flowAccumulation cfg flowBonus elev flow = U.create $ do
  let n = U.length elev
  acc <- UM.replicate n (hcBaseAccumulation cfg)
  let indices = sortBy (comparing (\i -> negate (elev U.! i))) [0 .. n - 1]
  forM_ indices $ \i -> do
    let d = flow U.! i
    if d >= 0
      then do
        v <- UM.read acc i
        let bonus = flowBonus U.! i
        UM.modify acc (+ v * (1 + bonus)) d
      else pure ()
  pure acc

applyStreamPowerErosion :: HydroConfig -> U.Vector Float -> U.Vector Int -> U.Vector Float -> U.Vector Float -> U.Vector Float -> U.Vector Float -> U.Vector Float
applyStreamPowerErosion cfg elev flow acc hardness erosionMult depositFactor = U.create $ do
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
            erosion = min (hcStreamPowerMaxErosion cfg)
                          (power * hcStreamPowerScale cfg * hardnessFactor cfg (hardness U.! i))
                    * (erosionMult U.! i)
            depositAmt = erosion * hcStreamDepositRatio cfg
                       * (depositFactor U.! d)
        UM.modify base (\v -> v - erosion) i
        UM.modify deposit (+ depositAmt) d
      else pure ()
  forM_ [0 .. n - 1] $ \i -> do
    d <- UM.read deposit i
    UM.modify base (+ d) i
  pure base

carveRiversGrid :: HydroConfig -> Int -> Int -> U.Vector Float -> U.Vector Float -> U.Vector Float -> U.Vector Float -> U.Vector Float
carveRiversGrid cfg gridW gridH elev acc hardness erosionMult =
  let maxAcc = max (hcMinAccumulation cfg) (U.maximum acc)
      waterLevel = hcWaterLevel cfg
  in U.imap (carveAt maxAcc waterLevel) elev
  where
    carveAt maxAcc waterLevel i h0 =
      let a = acc U.! i
          flowNorm = clamp01 (a / maxAcc)
          depth = min (hcRiverCarveMaxDepth cfg)
                      (flowNorm * hcRiverCarveScale cfg * hardnessFactor cfg (hardness U.! i))
                * (erosionMult U.! i)
      in if h0 > waterLevel
          then h0 - depth
          else h0

riverBankErodeGrid :: HydroConfig -> Int -> Int -> U.Vector Float -> U.Vector Float -> U.Vector Float -> U.Vector Float -> U.Vector Float
riverBankErodeGrid cfg gridW gridH elev acc hardness erosionMult =
  let maxAcc = max (hcMinAccumulation cfg) (U.maximum acc)
      threshold = maxAcc * hcRiverBankThreshold cfg
  in U.generate (U.length elev) (bankAt threshold)
  where
    bankAt threshold i =
      let h0 = elev U.! i
          neighborHigh = any (\j -> acc U.! j > threshold)
                             (hexNeighborIndices gridW gridH i)
          bankDepth = hcRiverBankDepth cfg * hardnessFactor cfg (hardness U.! i)
                    * (erosionMult U.! i)
      in if neighborHigh then h0 - bankDepth else h0

-- | Deposit alluvial sediment where rivers decelerate.
--
-- Deposition occurs on low-slope land tiles, scaled inversely with slope
-- (more deposit where the gradient is gentler) and by normalized flow.
-- A sink-protection guard prevents deposits from creating new local
-- minima: the deposit is clamped so the tile never rises above the
-- elevation of its lowest neighbour.
--
-- The @depositFactor@ vector (1 \u2212 suppression) scales the raw deposit
-- per tile, allowing terrain form modifiers to suppress deposition in
-- e.g. canyons and cliffs.
alluvialDepositGrid :: HydroConfig -> Int -> Int -> U.Vector Float -> U.Vector Float -> U.Vector Float -> U.Vector Float
alluvialDepositGrid cfg gridW gridH elev acc depositFactor =
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
                              * (depositFactor U.! i)
                         else 0
          -- Sink guard: never raise above the lowest neighbour
          nbrMin = neighborMin gridW gridH elev i
          maxDeposit = if nbrMin > h0 then nbrMin - h0 else 0
      in h0 + min rawDeposit maxDeposit

wetErodeGrid :: HydroConfig -> Int -> Int -> U.Vector Float -> U.Vector Float -> U.Vector Float -> U.Vector Float -> U.Vector Float
wetErodeGrid cfg gridW gridH elev moisture hardness erosionMult =
  let maxMoist = max (hcMinMoisture cfg) (U.maximum moisture)
      waterLevel = hcWaterLevel cfg
  in U.generate (U.length elev) (wetErodeAt maxMoist waterLevel)
  where
    wetErodeAt maxMoist waterLevel i =
      let h0 = elev U.! i
          m = moisture U.! i / maxMoist
          depth = clamp01 m * hcWetErodeScale cfg * hardnessFactor cfg (hardness U.! i)
                * (erosionMult U.! i)
      in if h0 > waterLevel then h0 - depth else h0



-- | Two-part coastal reshaping: erode land adjacent to water, raise
-- shallow ocean adjacent to land.
--
-- The @erosionMult@ vector scales the land-lowering amount, and
-- @smoothResist@ scales the ocean-raising amount (inverted: higher
-- resistance means less shelf building).
coastalErodeGrid :: HydroConfig -> Int -> Int -> U.Vector Float -> U.Vector Float -> U.Vector Float -> U.Vector Float -> U.Vector Float
coastalErodeGrid cfg gridW gridH elev hardness erosionMult smoothResist =
  let waterLevel = hcWaterLevel cfg
      strength = hcCoastalErodeStrength cfg
      raiseFactor = hcCoastalRaiseFactor cfg
  in U.generate (U.length elev) (coastalAt cfg gridW gridH waterLevel strength raiseFactor elev hardness erosionMult smoothResist)

coastalAt :: HydroConfig -> Int -> Int -> Float -> Float -> Float -> U.Vector Float -> U.Vector Float -> U.Vector Float -> U.Vector Float -> Int -> Float
coastalAt cfg gridW gridH waterLevel strength raiseFactor elev hardness erosionMult smoothResist i =
  let h0 = elev U.! i
      localStrength = strength * hardnessFactor cfg (hardness U.! i)
      neighbors = map (elev U.!) (hexNeighborIndices gridW gridH i)
      anyWater = any (< waterLevel) neighbors
      anyLand = any (>= waterLevel) neighbors
      lower = min (localStrength * (erosionMult U.! i)) (h0 - waterLevel)
      resist = smoothResist U.! i
      raise = min (localStrength * raiseFactor * (1 - resist)) (waterLevel - h0)
  in if h0 >= waterLevel && anyWater
      then h0 - max 0 lower
      else if h0 < waterLevel && anyLand
        then h0 + max 0 raise
        else h0

-- | Smooth the piedmont (foothills) transition zone.
--
-- Identifies land tiles in the moderate-slope band between
-- @hcPiedmontSlopeMin@ and @hcPiedmontSlopeMax@, then blends their
-- elevation toward the mean of their hex neighbours.  This creates
-- natural bajadas and pediments at mountain bases where talus
-- and alluvial fans smooth the mountain-to-plain transition.
--
-- The primary gate is 'FormFoothill' from the pre-classified terrain
-- form grid.  Tiles not classified as foothills fall back to the
-- fallback slope-band heuristic (slope in @[slopeMin, slopeMax]@ with
-- a steeper neighbour).  This gives the best of both approaches:
-- the form system captures the transition zone directly, while the
-- fallback handles edge cases the classification might miss.
--
-- The @smoothResist@ vector attenuates the blend per tile, so that
-- terrain forms with high smooth resistance (cliffs, escarpments)
-- preserve their morphology even when they fall in the piedmont zone.
--
-- Tiles below sea level are untouched.
piedmontSmoothGrid
  :: HydroConfig
  -> Int                   -- ^ Grid width
  -> Int                   -- ^ Grid height
  -> U.Vector Float        -- ^ Elevation grid
  -> U.Vector TerrainForm  -- ^ Pre-classified terrain form grid
  -> U.Vector Float        -- ^ Per-tile smooth resistance [0..1]
  -> U.Vector Float        -- ^ Smoothed elevation grid
piedmontSmoothGrid cfg gridW gridH elev formGrid smoothResist =
  let !slopeMin  = hcPiedmontSlopeMin cfg
      !slopeMax  = hcPiedmontSlopeMax cfg
      !strength  = hcPiedmontSmoothStrength cfg
      !wl        = hcWaterLevel cfg
  in U.generate (U.length elev) $ \i ->
       let h0   = elev U.! i
           form = formGrid U.! i
           nbrs = hexNeighborIndices gridW gridH i
       in case nbrs of
            [] -> h0
            _  ->
              let slope = maximum [abs (elev U.! j - h0) | j <- nbrs]
                  -- Primary gate: tile is classified as FormFoothill
                  isFoothill = form == FormFoothill
                  -- Fallback gate: slope-band heuristic with steeper
                  -- neighbour requirement (fallback behaviour)
                  hasSteeperNbr = any (\j ->
                    let nSlope = maximum [abs (elev U.! k - elev U.! j)
                                        | k <- hexNeighborIndices gridW gridH j]
                    in nSlope > slopeMax) nbrs
                  inSlopeBand = slope >= slopeMin && slope <= slopeMax && hasSteeperNbr
                  eligible = isFoothill || inSlopeBand
                  nbrMean = sum (map (elev U.!) nbrs) / fromIntegral (length nbrs)
                  -- Smoothstep blend factor: 0 at slopeMin, 1 at slopeMax
                  t = clamp01 ((slope - slopeMin) / max 1e-6 (slopeMax - slopeMin))
                  resist = smoothResist U.! i
                  -- Stronger smoothing in the middle of the band
                  blend = strength * t * (1 - t) * 4 * (1 - resist)
              in if h0 > wl && eligible
                   then h0 + blend * (nbrMean - h0)
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
    , gwInfiltration = U.empty
    , gwWaterTableDepth = U.empty
    , gwRootZoneMoisture = U.empty
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
