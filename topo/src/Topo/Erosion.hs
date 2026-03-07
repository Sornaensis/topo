{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Erosion stages and helpers.
--
-- Provides hydraulic and thermal erosion with local deposition, plus
-- coastal smoothing.  Erosion is purely local (neighbor-based, no flow
-- routing) and runs before hypsometric remapping and hydrology.
module Topo.Erosion
  ( ErosionConfig(..)
  , defaultErosionConfig
  , applyErosionStage
    -- * Internals (exported for testing)
  , hydraulicStepGrid
  , thermalStepGrid
  , coastalSmoothGrid
  , minimumNeighborGridIdx
  , computeMicroReliefGrid
  ) where

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl')
import Data.Word (Word64)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Generics (Generic)
import Topo.Config.JSON
  (ToJSON(..), FromJSON(..), configOptions, mergeDefaults,
   genericToJSON, genericParseJSON)
import Topo.BaseHeight (GenConfig(..))
import Topo.Hex (hexNeighborIndices)
import Topo.Math (clamp01, iterateN)
import Topo.Noise (fbm2D)
import Topo.Pipeline (PipelineStage(..))
import Topo.Pipeline.Stage (StageId(..))
import Control.Monad.Except (throwError)
import Topo.Plugin (logInfo, getWorldP, putWorldP, PluginError(..))
import Topo.TerrainForm.Modifiers
  ( TerrainFormModifiers(..)
  , defaultTerrainFormModifiers
  )
import Topo.TerrainGrid
  ( buildElevationGrid
  , buildPlateHardnessGrid
  , clampCoordGrid
  , chunkGridSlice
  , classifyTerrainFormGrid
  , updateChunkElevationFromGrid
  , validateTerrainGrid
  )
import Topo.Types
import Topo.Parameters (TerrainFormConfig, computeReliefIndex)
import qualified Topo.TerrainForm.Metrics as TerrainMetrics
import Topo.World (TerrainWorld(..))

-- | Configuration parameters for erosion.
--
-- Controls both hydraulic and thermal erosion applied to the terrain
-- grid after plate tectonics.
data ErosionConfig = ErosionConfig
  { ecHydraulicIterations :: !Int
    -- ^ Number of hydraulic erosion passes.
  , ecThermalIterations :: !Int
    -- ^ Number of thermal erosion passes.
  , ecRainRate :: !Float
    -- ^ Rainfall rate per hydraulic iteration (elevation units).
  , ecThermalTalus :: !Float
    -- ^ Talus angle threshold for thermal erosion (slope).
  , ecMaxDrop :: !Float
    -- ^ Maximum height drop per hydraulic step (elevation units).
  , ecHydraulicWetFactor :: !Float
    -- ^ Wet-area amplification of hydraulic erosion [0..1].
  , ecHydraulicHardnessFactor :: !Float
    -- ^ Hardness resistance to hydraulic erosion [0..1].
    -- Higher = more resistance.
  , ecThermalStrength :: !Float
    -- ^ Thermal erosion material transfer rate [0..1].
  , ecThermalHardnessFactor :: !Float
    -- ^ Hardness resistance to thermal erosion [0..1].
  , ecThermalWetFactor :: !Float
    -- ^ Wet-area amplification of thermal erosion [0..1].
  , ecHydraulicDepositRatio :: !Float
    -- ^ Fraction of hydraulically eroded material deposited at the
    -- lowest hex neighbour [0..1].  0 = purely subtractive.
  , ecHydraulicDepositMaxSlope :: !Float
    -- ^ Deposition only occurs when the slope to the lowest neighbor
    -- is below this threshold (normalized elevation units).
  , ecThermalDepositRatio :: !Float
    -- ^ Fraction of thermally eroded material deposited at the lowest
    -- hex neighbour [0..1].  Higher than hydraulic because thermal
    -- mass-wasting is a short-range process.
  , ecCoastalSmoothZone :: !Float
    -- ^ Normalized elevation band above sea level where coastal
    -- smoothing applies (e.g. 0.06 ≈ 720 m).
  , ecCoastalSmoothStrength :: !Float
    -- ^ Blend factor toward hex-neighbour mean for coastal
    -- tiles [0..1].
  , ecCoastalSmoothIterations :: !Int
    -- ^ Number of iterative coastal smoothing passes.
    -- Each pass erodes land in the coastal zone and deposits material
    -- on shallow ocean tiles, progressively building a coastal plain
    -- and continental shelf.
  , ecMicroReliefNoiseWeight :: !Float
    -- ^ Blend weight for high-frequency noise contribution.
  , ecMicroReliefErosionWeight :: !Float
    -- ^ Blend weight for erosion-energy contribution.
  , ecMicroReliefNoiseNorm :: !Float
    -- ^ Normalization divisor applied to absolute FBM amplitude.
  , ecMicroReliefErosionNorm :: !Float
    -- ^ Normalization divisor applied to full erosion delta
    -- (@abs(elev0 - elev2)@).
  , ecMicroReliefDetailFrequency :: !Float
    -- ^ Multiplier for high-frequency noise sampling relative to the
    -- base generation frequency.
  } deriving (Eq, Show, Generic)

instance ToJSON ErosionConfig where
  toJSON = genericToJSON (configOptions "ec")

instance FromJSON ErosionConfig where
  parseJSON v = genericParseJSON (configOptions "ec")
                  (mergeDefaults (toJSON defaultErosionConfig) v)

-- | Default erosion configuration.
defaultErosionConfig :: ErosionConfig
defaultErosionConfig = ErosionConfig
  { ecHydraulicIterations = 6
  , ecThermalIterations = 6
    -- ^ Increased from 4; more passes with lower strength gives smoother results.
  , ecRainRate = 0.2
  , ecThermalTalus = 0.04
    -- ^ Reduced from 0.5 (6000m cliff!) to 0.04 (~480m over a hex).
    -- At ~20km hex spacing, 0.04 represents a significant escarpment.
  , ecMaxDrop = 0.5
  , ecHydraulicWetFactor = 0.7
  , ecHydraulicHardnessFactor = 0.7
  , ecThermalStrength = 0.3
    -- ^ Reduced from 0.5 to avoid over-smoothing with the much lower talus.
  , ecThermalHardnessFactor = 0.7
  , ecThermalWetFactor = 0.7
  , ecHydraulicDepositRatio = 0.3
    -- ^ 30% of eroded material deposited at lowest neighbor.
  , ecHydraulicDepositMaxSlope = 0.06
    -- ^ No deposition on slopes steeper than ~720 m over a hex.
  , ecThermalDepositRatio = 0.5
    -- ^ 50% — thermal mass-wasting piles rubble nearby.
  , ecCoastalSmoothZone = 0.06
    -- ^ ~720 m band above sea level gets wave-smoothed.
  , ecCoastalSmoothStrength = 0.3
    -- ^ 30% blend toward neighbor mean.
  , ecCoastalSmoothIterations = 2
    -- ^ 2 passes of iterative coastal erosion/deposition
    -- (reduced from 4 to limit compounding with hypsometry coastal ramp).
  , ecMicroReliefNoiseWeight = 0.7
  , ecMicroReliefErosionWeight = 0.3
  , ecMicroReliefNoiseNorm = 1.75
  , ecMicroReliefErosionNorm = 0.03
  , ecMicroReliefDetailFrequency = 12
  }

-- | Apply hydraulic and thermal erosion (with local deposition) plus
-- coastal smoothing across the terrain grid.
--
-- The 'TerrainFormConfig' is used for a lightweight pre-classification
-- pass that tags each tile with a 'TerrainForm' before erosion runs.
-- Per-form modifiers ('defaultTerrainFormModifiers') then adjust erosion
-- intensity, hardness resistance, deposition, and smoothing per tile.
applyErosionStage :: GenConfig -> ErosionConfig -> TerrainFormConfig -> Float -> PipelineStage
applyErosionStage genCfg cfg formCfg waterLevel = PipelineStage StageErosion "applyErosion" "applyErosion" Nothing [] Nothing $ do
  logInfo "applyErosion: hydraulic + thermal + coastal smooth"
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
      plateHardness = buildPlateHardnessGrid config terrain (ChunkCoord minCx minCy) gridW gridH
      -- Pre-classify terrain forms from current elevation + hardness.
      formGrid = classifyTerrainFormGrid formCfg waterLevel gridW gridH elev0 plateHardness
      -- Pre-compute per-tile modifier vectors from the terrain form grid.
      modLookup = defaultTerrainFormModifiers
      erosionMult   = U.map (tfmErosionRate . modLookup) formGrid
      adjHardness   = U.zipWith (\h f -> clamp01 (h + tfmHardnessBonus (modLookup f))) plateHardness formGrid
      depositFactor = U.map (\f -> 1 - tfmDepositSuppression (modLookup f)) formGrid
      smoothResist  = U.map (tfmSmoothResistance . modLookup) formGrid
      elev1 = iterateN (ecHydraulicIterations cfg) (hydraulicStepGrid gridW gridH waterLevel cfg adjHardness erosionMult depositFactor) elev0
      elev2 = iterateN (ecThermalIterations cfg) (thermalStepGrid gridW gridH waterLevel cfg adjHardness erosionMult depositFactor) elev1
      elev3 = iterateN (max 1 (ecCoastalSmoothIterations cfg)) (coastalSmoothGrid gridW gridH waterLevel cfg smoothResist) elev2
      erosionProxy = U.zipWith (\a b -> abs (a - b)) elev0 elev2
      microReliefGrid = computeMicroReliefGrid
        config
        genCfg
        (ChunkCoord minCx minCy)
        gridW
        gridH
        elev3
        (twSeed world)
        cfg
        erosionProxy
      -- Safety clamp: ensure all elevations are in [0,1] before
      -- downstream stages (hydrology, climate) process them.
      elevClamped = U.map clamp01 elev3
      terrain' = IntMap.mapWithKey
        (\key chunk ->
          let updatedElevation = updateChunkElevationFromGrid config (ChunkCoord minCx minCy) gridW elevClamped key chunk
              micro = chunkGridSlice config (ChunkCoord minCx minCy) gridW microReliefGrid key
          in updatedElevation { tcMicroRelief = micro }
        )
        terrain
  putWorldP world { twTerrain = terrain' }

computeMicroReliefGrid
  :: WorldConfig
  -> GenConfig
  -> ChunkCoord
  -> Int
  -> Int
  -> U.Vector Float
  -> Word64
  -> ErosionConfig
  -> U.Vector Float
  -> U.Vector Float
computeMicroReliefGrid config genCfg (ChunkCoord minCx minCy) gridW gridH elevGrid worldSeed cfg erosionProxy =
  let size = wcChunkSize config
      minTileX = minCx * size
      minTileY = minCy * size
      baseFrequency = max 0 (gcCoordScale genCfg * gcFrequency genCfg)
      detailFrequency = baseFrequency * max 0 (ecMicroReliefDetailFrequency cfg)
      !wNoise = max 0 (ecMicroReliefNoiseWeight cfg)
      !wErode = max 0 (ecMicroReliefErosionWeight cfg)
      normalize denom v
        | denom <= 0 = 0
        | otherwise = clamp01 (v / denom)
      elevAtClamped gx gy =
        let !cx = clampCoordGrid gridW gx
            !cy = clampCoordGrid gridH gy
        in elevGrid U.! (cy * gridW + cx)
      sample idx =
        let x = idx `mod` gridW
            y = idx `div` gridW
            gx = minTileX + x
            gy = minTileY + y
            !metrics = TerrainMetrics.terrainNeighborhoodAt elevAtClamped x y
            !r = TerrainMetrics.tnRelief metrics
            !r2 = TerrainMetrics.tnRelief2Ring metrics
            !r3 = TerrainMetrics.tnRelief3Ring metrics
            fx = (fromIntegral gx + gcOffsetX genCfg) * detailFrequency
            fy = (fromIntegral gy + gcOffsetY genCfg) * detailFrequency
            fbmVal = fbm2D
              (worldSeed + 0x9e3779b97f4a7c15)
              (max 1 (gcOctaves genCfg))
              (gcLacunarity genCfg)
              (gcGain genCfg)
              fx
              fy
            noiseTerm = normalize (ecMicroReliefNoiseNorm cfg) (abs fbmVal)
            erosionTerm = normalize (ecMicroReliefErosionNorm cfg) (erosionProxy U.! idx)
        in computeReliefIndex r r2 r3 (Just noiseTerm) (Just erosionTerm) wNoise wErode
  in U.generate (gridW * gridH) sample

-- | Legacy per-chunk erosion (no deposition, no grid context).
erodeChunk :: WorldConfig -> ErosionConfig -> TerrainChunk -> TerrainChunk
erodeChunk config cfg chunk =
  let size = wcChunkSize config
      elev0 = tcElevation chunk
      elev1 = iterateN (ecHydraulicIterations cfg) (hydraulicStep size cfg) elev0
      elev2 = iterateN (ecThermalIterations cfg) (thermalStep size cfg) elev1
  in chunk { tcElevation = elev2 }

-------------------------------------------------------------------------------
-- Hydraulic erosion (with local deposition)
-------------------------------------------------------------------------------

-- | One pass of hydraulic erosion with optional neighbor deposition.
--
-- Uses mutable ST vectors so that material eroded from a tile can be
-- deposited at its lowest hex neighbour within the same pass.
--
-- The @erosionMult@ and @depositFactor@ vectors carry per-tile modifiers
-- derived from terrain form pre-classification.  @erosionMult@ scales the
-- drop amount (> 1 accelerates, < 1 suppresses); @depositFactor@ scales
-- the deposit amount (0 = full suppression, 1 = no suppression).
hydraulicStepGrid
  :: Int             -- ^ Grid width
  -> Int             -- ^ Grid height
  -> Float           -- ^ Water level
  -> ErosionConfig
  -> U.Vector Float  -- ^ Hardness per tile (pre-adjusted with form bonus)
  -> U.Vector Float  -- ^ Per-tile erosion rate multiplier
  -> U.Vector Float  -- ^ Per-tile deposit factor (1 − suppression)
  -> U.Vector Float  -- ^ Elevation (input)
  -> U.Vector Float  -- ^ Elevation (output)
hydraulicStepGrid gridW gridH waterLevel cfg hardness erosionMult depositFactor elev = runST $ do
  let n = U.length elev
  base <- U.thaw elev
  deposit <- UM.replicate n (0 :: Float)
  forM_ [0 .. n - 1] $ \i -> do
    h0 <- UM.read base i
    let x = i `mod` gridW
        y = i `div` gridW
        (hmin, minIdx) = minimumNeighborGridIdx gridW gridH elev x y h0
        dh = h0 - hmin
    if dh <= 0
      then pure ()
      else do
        let wetFactor = if h0 < waterLevel then ecHydraulicWetFactor cfg else 1
            hard = clamp01 (hardness U.! i)
            rain = ecRainRate cfg * (1 - hard * ecHydraulicHardnessFactor cfg)
            dropAmt = min (ecMaxDrop cfg) (dh * rain) * wetFactor
                    * (erosionMult U.! i)
        UM.write base i (h0 - dropAmt)
        -- Deposit a fraction at the lowest neighbor, but only when
        -- the slope is gentle enough for sediment to settle.
        let depositAmt = dropAmt * ecHydraulicDepositRatio cfg
                       * (depositFactor U.! i)
        if dh < ecHydraulicDepositMaxSlope cfg && minIdx /= i && depositAmt > 0
          then UM.modify deposit (+ depositAmt) minIdx
          else pure ()
  -- Apply accumulated deposits with sink guard.
  forM_ [0 .. n - 1] $ \i -> do
    d <- UM.read deposit i
    if d > 0
      then do
        h <- UM.read base i
        let x = i `mod` gridW
            y = i `div` gridW
            -- Sink guard: never raise above own lowest hex
            -- neighbor (from the original elevation grid).  Pass +∞
            -- as the self value so minimumNeighborGridIdx returns
            -- the true neighbor-only minimum.
            (nbrMin, _) = minimumNeighborGridIdx gridW gridH elev x y (1/0)
            cap = max 0 (nbrMin - h)
            safeD = min d cap
        if safeD > 0
          then UM.write base i (h + safeD)
          else pure ()
      else pure ()
  U.freeze base

-- | Legacy per-chunk hydraulic erosion (pure, no deposition).
hydraulicStep :: Int -> ErosionConfig -> U.Vector Float -> U.Vector Float
hydraulicStep size cfg elev =
  U.generate (U.length elev) (hydraulicAt size cfg elev)

hydraulicAt :: Int -> ErosionConfig -> U.Vector Float -> Int -> Float
hydraulicAt size cfg elev i =
  let x = i `mod` size
      y = i `div` size
      h0 = elev U.! i
      (hmin, _) = minimumNeighborGridIdx size size elev x y h0
      dh = h0 - hmin
      dropAmt = min (ecMaxDrop cfg) (dh * ecRainRate cfg)
  in if dh <= 0 then h0 else h0 - dropAmt

-------------------------------------------------------------------------------
-- Thermal erosion (with local deposition)
-------------------------------------------------------------------------------

-- | One pass of thermal erosion with optional neighbor deposition.
--
-- Material eroded from slopes exceeding the talus threshold is partially
-- deposited at the slope base (lowest hex neighbour), simulating
-- scree / talus accumulation.
--
-- @erosionMult@ and @depositFactor@ carry per-tile terrain-form modifiers
-- identical to those used by 'hydraulicStepGrid'.
thermalStepGrid
  :: Int             -- ^ Grid width
  -> Int             -- ^ Grid height
  -> Float           -- ^ Water level
  -> ErosionConfig
  -> U.Vector Float  -- ^ Hardness per tile (pre-adjusted with form bonus)
  -> U.Vector Float  -- ^ Per-tile erosion rate multiplier
  -> U.Vector Float  -- ^ Per-tile deposit factor (1 − suppression)
  -> U.Vector Float  -- ^ Elevation (input)
  -> U.Vector Float  -- ^ Elevation (output)
thermalStepGrid gridW gridH waterLevel cfg hardness erosionMult depositFactor elev = runST $ do
  let n = U.length elev
  base <- U.thaw elev
  deposit <- UM.replicate n (0 :: Float)
  forM_ [0 .. n - 1] $ \i -> do
    h0 <- UM.read base i
    let x = i `mod` gridW
        y = i `div` gridW
        (hmin, minIdx) = minimumNeighborGridIdx gridW gridH elev x y h0
        slope = h0 - hmin
        excess = slope - ecThermalTalus cfg
    if excess <= 0
      then pure ()
      else do
        let wetFactor = if h0 < waterLevel then ecThermalWetFactor cfg else 1
            hard = clamp01 (hardness U.! i)
            strength = ecThermalStrength cfg * (1 - hard * ecThermalHardnessFactor cfg)
            erodeAmt = min (ecMaxDrop cfg) (excess * strength * wetFactor)
                     * (erosionMult U.! i)
        UM.write base i (h0 - erodeAmt)
        -- Deposit rubble at slope base.
        let depositAmt = erodeAmt * ecThermalDepositRatio cfg
                       * (depositFactor U.! i)
        if minIdx /= i && depositAmt > 0
          then UM.modify deposit (+ depositAmt) minIdx
          else pure ()
  -- Apply accumulated deposits with sink guard.
  forM_ [0 .. n - 1] $ \i -> do
    d <- UM.read deposit i
    if d > 0
      then do
        h <- UM.read base i
        let x = i `mod` gridW
            y = i `div` gridW
            -- Sink guard: never raise above own lowest hex
            -- neighbor.  Pass +∞ as self so the minimum excludes
            -- the target tile itself.
            (nbrMin, _) = minimumNeighborGridIdx gridW gridH elev x y (1/0)
            cap = max 0 (nbrMin - h)
            safeD = min d cap
        if safeD > 0
          then UM.write base i (h + safeD)
          else pure ()
      else pure ()
  U.freeze base

-- | Legacy per-chunk thermal erosion (pure, no deposition).
thermalStep :: Int -> ErosionConfig -> U.Vector Float -> U.Vector Float
thermalStep size cfg elev =
  U.generate (U.length elev) (thermalAt size cfg elev)

thermalAt :: Int -> ErosionConfig -> U.Vector Float -> Int -> Float
thermalAt size cfg elev i =
  let x = i `mod` size
      y = i `div` size
      h0 = elev U.! i
      (hmin, _) = minimumNeighborGridIdx size size elev x y h0
      slope = h0 - hmin
      excess = slope - ecThermalTalus cfg
  in if excess <= 0 then h0 else h0 - min (ecMaxDrop cfg) (excess * ecThermalStrength cfg)

-------------------------------------------------------------------------------
-- Coastal smoothing
-------------------------------------------------------------------------------

-- | Smooth tiles in a narrow elevation band around sea level.
--
-- Simulates wave erosion and sediment deposition: land tiles within
-- @[waterLevel, waterLevel + ecCoastalSmoothZone]@ are blended toward
-- the mean of their in-bounds hex neighbours, and shallow ocean tiles
-- within @[waterLevel - ecCoastalSmoothZone, waterLevel]@ are raised
-- toward the neighbor mean, building a continental shelf.
--
-- The @smoothResist@ vector carries per-tile smoothing resistance derived
-- from terrain form classification.  Tiles with high resistance (cliffs,
-- escarpments, canyons) are smoothed less, preserving their morphology.
--
-- Designed to be applied iteratively (see 'ecCoastalSmoothIterations').
coastalSmoothGrid
  :: Int             -- ^ Grid width
  -> Int             -- ^ Grid height
  -> Float           -- ^ Water level
  -> ErosionConfig
  -> U.Vector Float  -- ^ Per-tile smooth resistance [0..1]
  -> U.Vector Float  -- ^ Elevation (input)
  -> U.Vector Float  -- ^ Elevation (output)
coastalSmoothGrid gridW gridH waterLevel cfg smoothResist elev
  | ecCoastalSmoothStrength cfg <= 0 = elev
  | ecCoastalSmoothZone cfg <= 0     = elev
  | otherwise = U.generate n smooth
  where
    n = U.length elev
    zone = ecCoastalSmoothZone cfg
    zoneTop = waterLevel + zone
    zoneBot = waterLevel - zone
    str = ecCoastalSmoothStrength cfg

    smooth i =
      let h0 = elev U.! i
          -- Use all 6 hex neighbours so the smoothing kernel matches
          -- hydrology's 6-direction drainage topology (Phase A.3).
          nbrs = hexNeighborIndices gridW gridH i
          nbrSum = foldl' (\acc j -> acc + elev U.! j) 0.0 nbrs
          nbrCount = length nbrs
          nbrMean = if nbrCount > 0
                      then nbrSum / fromIntegral nbrCount
                      else h0
      in if h0 > waterLevel && h0 <= zoneTop
           then
             -- Land tile in coastal zone: erode toward neighbor mean
             let t = (h0 - waterLevel) / zone
                 fade = 1 - t  -- strongest right at sea level
                 resist = smoothResist U.! i
                 blend = str * fade * (1 - resist)
                 -- Floor-clamp: never push a land tile at or below
                 -- waterLevel, otherwise flowDirectionsLand treats it
                 -- as ocean and rivers cannot reach the coast.
             in max (waterLevel + 1e-5) (h0 + blend * (nbrMean - h0))
           else if h0 >= zoneBot && h0 < waterLevel
             then
               -- Shallow ocean tile: deposit toward neighbor mean
               -- (raise floor to build continental shelf)
               let t = (waterLevel - h0) / zone
                   fade = 1 - t  -- strongest right at sea level
                   resist = smoothResist U.! i
                   -- Use half strength for deposition to avoid raising
                   -- ocean floor too aggressively
                   blend = str * 0.5 * fade * (1 - resist)
                   target = min waterLevel (h0 + blend * (nbrMean - h0))
               in target
             else h0

-------------------------------------------------------------------------------
-- Neighbor helpers
-------------------------------------------------------------------------------

-- | Find the minimum-elevation hex neighbor, returning both the
-- elevation and the index.  Uses all 6 hex neighbours via
-- 'hexNeighborIndices' so that erosion's deposition targets align
-- with hydrology's 6-direction flow routing.
--
-- If no neighbor is lower than @h0@, returns @(h0, selfIdx)@.
minimumNeighborGridIdx :: Int -> Int -> U.Vector Float -> Int -> Int -> Float -> (Float, Int)
minimumNeighborGridIdx gridW gridH elev x y h0 =
  let ix = y * gridW + x
      pick (!bestH, !bestI) (!candH, !candI)
        | candH < bestH = (candH, candI)
        | otherwise     = (bestH, bestI)
      start = (h0, ix)
  in foldl' (\acc j -> pick acc (elev U.! j, j)) start
       (hexNeighborIndices gridW gridH ix)

