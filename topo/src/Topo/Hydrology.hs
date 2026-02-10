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
  ) where

import Control.Monad (forM_)
import Control.Monad.ST (ST)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Word (Word16, Word32)
import Topo.Math (clamp01)
import Topo.Pipeline (PipelineStage(..))
import Control.Monad.Except (throwError)
import Topo.Plugin (logInfo, getWorldP, putWorldP, PluginError(..))
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
  , rcDischargeScale = 0.01
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
      elev1 = breachSinks cfg gridW gridH elev0
      flow = flowDirections gridW gridH elev1
      acc = flowAccumulation cfg elev1 flow
      elevCarved = carveRiversGrid cfg gridW gridH elev1 acc hardness
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
      elev = buildElevationGrid config terrain (ChunkCoord minCx minCy) gridW gridH
      hardness = buildHardnessGrid config terrain (ChunkCoord minCx minCy) gridW gridH
      moisture = buildMoistureGrid config terrain (ChunkCoord minCx minCy) gridW gridH
      flow = flowDirections gridW gridH elev
      acc = flowAccumulationWithBase (rcBaseAccumulation riverCfg) elev flow
      basinIds = basinIdsFromFlow flow
      basinStats = basinRechargeStats basinIds moisture gwCfg
      (basinStorage, basinDischarge, basinSize) = basinStorageStats basinStats gwCfg
      baseflow = basinBaseflow basinIds basinDischarge basinSize (rcBaseflowScale riverCfg)
      riverOrder = strahlerOrder gridW gridH elev flow acc (rcOrderMinAccumulation riverCfg)
      discharge = U.zipWith (+) (U.map (* rcDischargeScale riverCfg) acc) baseflow
      depth = riverDepthWithHardness acc hardness riverCfg
      erosionPotential = riverErosionPotential gridW gridH elev acc hardness riverCfg
      depositPotential = riverDepositPotential gridW gridH elev acc riverCfg
      rivers = RiverChunk
        { rcFlowAccum = acc
        , rcDischarge = discharge
        , rcChannelDepth = depth
        , rcRiverOrder = riverOrder
        , rcBasinId = basinIds
        , rcBaseflow = baseflow
        , rcErosionPotential = erosionPotential
        , rcDepositPotential = depositPotential
        }
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

neighborMin :: Int -> Int -> U.Vector Float -> Int -> Float
neighborMin gridW gridH elev i =
  let x = i `mod` gridW
      y = i `div` gridW
      h0 = elev U.! i
      hL = if x > 0 then elev U.! (i - 1) else h0
      hR = if x + 1 < gridW then elev U.! (i + 1) else h0
      hU = if y > 0 then elev U.! (i - gridW) else h0
      hD = if y + 1 < gridH then elev U.! (i + gridW) else h0
  in minimum [h0, hL, hR, hU, hD]

flowDirections :: Int -> Int -> U.Vector Float -> U.Vector Int
flowDirections gridW gridH elev =
  U.generate (U.length elev) (\i ->
    let x = i `mod` gridW
        y = i `div` gridW
        h0 = elev U.! i
        candidates =
          [ (i - 1, elev U.! (i - 1)) | x > 0 ]
          <> [ (i + 1, elev U.! (i + 1)) | x + 1 < gridW ]
          <> [ (i - gridW, elev U.! (i - gridW)) | y > 0 ]
          <> [ (i + gridW, elev U.! (i + gridW)) | y + 1 < gridH ]
        lower = filter (\(_, h) -> h < h0) candidates
    in case lower of
        [] -> -1
        _ -> fst (minimumByElevation lower))
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
      let x = i `mod` gridW
          y = i `div` gridW
          h0 = elev U.! i
          neighborHigh =
            any (> threshold)
              [ acc U.! (i - 1) | x > 0 ]
            || any (> threshold)
              [ acc U.! (i + 1) | x + 1 < gridW ]
            || any (> threshold)
              [ acc U.! (i - gridW) | y > 0 ]
            || any (> threshold)
              [ acc U.! (i + gridW) | y + 1 < gridH ]
          bankDepth = hcRiverBankDepth cfg * hardnessFactor cfg (hardness U.! i)
      in if neighborHigh then h0 - bankDepth else h0

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
          isLowSlope = slope < maxSlope
          deposit = if h0 > waterLevel && isLowSlope then flowNorm * hcAlluvialDepositScale cfg else 0
      in h0 + deposit

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

gridSlopeAt :: Int -> Int -> U.Vector Float -> Int -> Float
gridSlopeAt gridW gridH elev i =
  let x = i `mod` gridW
      y = i `div` gridW
      h0 = elev U.! i
      hL = if x > 0 then elev U.! (i - 1) else h0
      hR = if x + 1 < gridW then elev U.! (i + 1) else h0
      hU = if y > 0 then elev U.! (i - gridW) else h0
      hD = if y + 1 < gridH then elev U.! (i + gridW) else h0
      dx = max (abs (hR - h0)) (abs (hL - h0))
      dy = max (abs (hD - h0)) (abs (hU - h0))
  in max dx dy

coastalErodeGrid :: HydroConfig -> Int -> Int -> U.Vector Float -> U.Vector Float -> U.Vector Float
coastalErodeGrid cfg gridW gridH elev hardness =
  let waterLevel = hcWaterLevel cfg
      strength = hcCoastalErodeStrength cfg
      raiseFactor = hcCoastalRaiseFactor cfg
  in U.generate (U.length elev) (coastalAt cfg gridW gridH waterLevel strength raiseFactor elev hardness)

coastalAt :: HydroConfig -> Int -> Int -> Float -> Float -> Float -> U.Vector Float -> U.Vector Float -> Int -> Float
coastalAt cfg gridW gridH waterLevel strength raiseFactor elev hardness i =
  let x = i `mod` gridW
      y = i `div` gridW
      h0 = elev U.! i
      localStrength = strength * hardnessFactor cfg (hardness U.! i)
      neighbors =
        [ elev U.! (i - 1) | x > 0 ]
        <> [ elev U.! (i + 1) | x + 1 < gridW ]
        <> [ elev U.! (i - gridW) | y > 0 ]
        <> [ elev U.! (i + gridW) | y + 1 < gridH ]
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

upstreamOrders :: Int -> Int -> U.Vector Int -> UM.MVector s Word16 -> Int -> ST s [Word16]
upstreamOrders gridW gridH flow orders i =
  let x = i `mod` gridW
      y = i `div` gridW
      candidates =
        [ (i - 1) | x > 0 ]
        <> [ (i + 1) | x + 1 < gridW ]
        <> [ (i - gridW) | y > 0 ]
        <> [ (i + gridW) | y + 1 < gridH ]
      incoming = filter (\n -> flow U.! n == i) candidates
  in mapM (UM.read orders) incoming

sliceRiverChunk :: WorldConfig -> ChunkCoord -> Int -> RiverChunk -> Int -> TerrainChunk -> RiverChunk
sliceRiverChunk config minCoord gridW rivers _key _chunk =
  RiverChunk
    { rcFlowAccum = chunkGridSlice config minCoord gridW (rcFlowAccum rivers) _key
    , rcDischarge = chunkGridSlice config minCoord gridW (rcDischarge rivers) _key
    , rcChannelDepth = chunkGridSlice config minCoord gridW (rcChannelDepth rivers) _key
    , rcRiverOrder = chunkGridSliceGeneric config minCoord gridW (rcRiverOrder rivers) _key
    , rcBasinId = chunkGridSliceGeneric config minCoord gridW (rcBasinId rivers) _key
    , rcBaseflow = chunkGridSlice config minCoord gridW (rcBaseflow rivers) _key
    , rcErosionPotential = chunkGridSlice config minCoord gridW (rcErosionPotential rivers) _key
    , rcDepositPotential = chunkGridSlice config minCoord gridW (rcDepositPotential rivers) _key
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
