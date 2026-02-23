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
  ) where

import Control.Monad (forM_)
import Control.Monad.ST (runST)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl')
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import GHC.Generics (Generic)
import Topo.Config.JSON
  (ToJSON(..), FromJSON(..), configOptions, mergeDefaults,
   genericToJSON, genericParseJSON)
import Topo.Hex (hexNeighborIndices)
import Topo.Math (clamp01, iterateN)
import Topo.Pipeline (PipelineStage(..))
import Control.Monad.Except (throwError)
import Topo.Plugin (logInfo, getWorldP, putWorldP, PluginError(..))
import Topo.TerrainGrid
  ( buildElevationGrid
  , buildPlateHardnessGrid
  , updateChunkElevationFromGrid
  , validateTerrainGrid
  )
import Topo.Types
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
    -- lowest cardinal neighbor [0..1].  0 = purely subtractive.
  , ecHydraulicDepositMaxSlope :: !Float
    -- ^ Deposition only occurs when the slope to the lowest neighbor
    -- is below this threshold (normalized elevation units).
  , ecThermalDepositRatio :: !Float
    -- ^ Fraction of thermally eroded material deposited at the lowest
    -- cardinal neighbor [0..1].  Higher than hydraulic because thermal
    -- mass-wasting is a short-range process.
  , ecCoastalSmoothZone :: !Float
    -- ^ Normalized elevation band above sea level where coastal
    -- smoothing applies (e.g. 0.06 ≈ 720 m).
  , ecCoastalSmoothStrength :: !Float
    -- ^ Blend factor toward cardinal-neighbor mean for coastal
    -- tiles [0..1].
  , ecCoastalSmoothIterations :: !Int
    -- ^ Number of iterative coastal smoothing passes.
    -- Each pass erodes land in the coastal zone and deposits material
    -- on shallow ocean tiles, progressively building a coastal plain
    -- and continental shelf.
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
  }

-- | Apply hydraulic and thermal erosion (with local deposition) plus
-- coastal smoothing across the terrain grid.
applyErosionStage :: ErosionConfig -> Float -> PipelineStage
applyErosionStage cfg waterLevel = PipelineStage "applyErosion" "applyErosion" $ do
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
      hardness = buildPlateHardnessGrid config terrain (ChunkCoord minCx minCy) gridW gridH
      elev1 = iterateN (ecHydraulicIterations cfg) (hydraulicStepGrid gridW gridH waterLevel cfg hardness) elev0
      elev2 = iterateN (ecThermalIterations cfg) (thermalStepGrid gridW gridH waterLevel cfg hardness) elev1
      elev3 = iterateN (max 1 (ecCoastalSmoothIterations cfg)) (coastalSmoothGrid gridW gridH waterLevel cfg) elev2
      -- Safety clamp: ensure all elevations are in [0,1] before
      -- downstream stages (hydrology, climate) process them.
      elevClamped = U.map clamp01 elev3
      terrain' = IntMap.mapWithKey (updateChunkElevationFromGrid config (ChunkCoord minCx minCy) gridW elevClamped) terrain
  putWorldP world { twTerrain = terrain' }

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
-- deposited at its lowest cardinal neighbor within the same pass.
hydraulicStepGrid
  :: Int             -- ^ Grid width
  -> Int             -- ^ Grid height
  -> Float           -- ^ Water level
  -> ErosionConfig
  -> U.Vector Float  -- ^ Hardness per tile
  -> U.Vector Float  -- ^ Elevation (input)
  -> U.Vector Float  -- ^ Elevation (output)
hydraulicStepGrid gridW gridH waterLevel cfg hardness elev = runST $ do
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
        UM.write base i (h0 - dropAmt)
        -- Deposit a fraction at the lowest neighbor, but only when
        -- the slope is gentle enough for sediment to settle.
        let depositAmt = dropAmt * ecHydraulicDepositRatio cfg
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
            -- Sink guard: never raise above own lowest cardinal
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
      hmin = minimumNeighbor size elev x y h0
      dh = h0 - hmin
      dropAmt = min (ecMaxDrop cfg) (dh * ecRainRate cfg)
  in if dh <= 0 then h0 else h0 - dropAmt

-------------------------------------------------------------------------------
-- Thermal erosion (with local deposition)
-------------------------------------------------------------------------------

-- | One pass of thermal erosion with optional neighbor deposition.
--
-- Material eroded from slopes exceeding the talus threshold is partially
-- deposited at the slope base (lowest cardinal neighbor), simulating
-- scree / talus accumulation.
thermalStepGrid
  :: Int             -- ^ Grid width
  -> Int             -- ^ Grid height
  -> Float           -- ^ Water level
  -> ErosionConfig
  -> U.Vector Float  -- ^ Hardness per tile
  -> U.Vector Float  -- ^ Elevation (input)
  -> U.Vector Float  -- ^ Elevation (output)
thermalStepGrid gridW gridH waterLevel cfg hardness elev = runST $ do
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
        UM.write base i (h0 - erodeAmt)
        -- Deposit rubble at slope base.
        let depositAmt = erodeAmt * ecThermalDepositRatio cfg
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
            -- Sink guard: never raise above own lowest cardinal
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
      hmin = minimumNeighbor size elev x y h0
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
-- the mean of their 4 cardinal neighbors, and shallow ocean tiles
-- within @[waterLevel - ecCoastalSmoothZone, waterLevel]@ are raised
-- toward the neighbor mean, building a continental shelf.
--
-- Designed to be applied iteratively (see 'ecCoastalSmoothIterations').
coastalSmoothGrid
  :: Int             -- ^ Grid width
  -> Int             -- ^ Grid height
  -> Float           -- ^ Water level
  -> ErosionConfig
  -> U.Vector Float  -- ^ Elevation (input)
  -> U.Vector Float  -- ^ Elevation (output)
coastalSmoothGrid gridW gridH waterLevel cfg elev
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
                 blend = str * fade
                 -- Floor-clamp: never push a land tile at or below
                 -- waterLevel, otherwise flowDirectionsLand treats it
                 -- as ocean and rivers cannot reach the coast.
             in max (waterLevel + 1e-5) (h0 + blend * (nbrMean - h0))
           else if h0 >= zoneBot && h0 <= waterLevel
             then
               -- Shallow ocean tile: deposit toward neighbor mean
               -- (raise floor to build continental shelf)
               let t = (waterLevel - h0) / zone
                   fade = 1 - t  -- strongest right at sea level
                   -- Use half strength for deposition to avoid raising
                   -- ocean floor too aggressively
                   blend = str * 0.5 * fade
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

-- | Minimum elevation among cardinal neighbors (legacy, value only).
minimumNeighbor :: Int -> U.Vector Float -> Int -> Int -> Float -> Float
minimumNeighbor size elev x y h0 =
  let ix = y * size + x
      hL = if x > 0 then elev U.! (ix - 1) else h0
      hR = if x + 1 < size then elev U.! (ix + 1) else h0
      hU = if y > 0 then elev U.! (ix - size) else h0
      hD = if y + 1 < size then elev U.! (ix + size) else h0
  in minimum [h0, hL, hR, hU, hD]

-- | Minimum elevation among cardinal neighbors on the full grid (value only).
minimumNeighborGrid :: Int -> Int -> U.Vector Float -> Int -> Int -> Float -> Float
minimumNeighborGrid gridW gridH elev x y h0 =
  let ix = y * gridW + x
      hL = if x > 0 then elev U.! (ix - 1) else h0
      hR = if x + 1 < gridW then elev U.! (ix + 1) else h0
      hU = if y > 0 then elev U.! (ix - gridW) else h0
      hD = if y + 1 < gridH then elev U.! (ix + gridW) else h0
  in minimum [h0, hL, hR, hU, hD]
