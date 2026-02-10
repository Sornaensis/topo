{-# LANGUAGE OverloadedStrings #-}

-- | Erosion stages and helpers.
module Topo.Erosion
  ( ErosionConfig(..)
  , defaultErosionConfig
  , applyErosionStage
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
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
import qualified Data.Vector.Unboxed as U

-- | Configuration parameters for erosion.
data ErosionConfig = ErosionConfig
  { ecHydraulicIterations :: !Int
  , ecThermalIterations :: !Int
  , ecRainRate :: !Float
  , ecThermalTalus :: !Float
  , ecMaxDrop :: !Float
  , ecHydraulicWetFactor :: !Float
  , ecHydraulicHardnessFactor :: !Float
  , ecThermalStrength :: !Float
  , ecThermalHardnessFactor :: !Float
  , ecThermalWetFactor :: !Float
  } deriving (Eq, Show)

-- | Default erosion configuration.
defaultErosionConfig :: ErosionConfig
defaultErosionConfig = ErosionConfig
  { ecHydraulicIterations = 6
  , ecThermalIterations = 4
  , ecRainRate = 0.2
  , ecThermalTalus = 0.5
  , ecMaxDrop = 0.5
  , ecHydraulicWetFactor = 0.7
  , ecHydraulicHardnessFactor = 0.7
  , ecThermalStrength = 0.5
  , ecThermalHardnessFactor = 0.7
  , ecThermalWetFactor = 0.7
  }

-- | Apply hydraulic and thermal erosion across the terrain grid.
applyErosionStage :: ErosionConfig -> Float -> PipelineStage
applyErosionStage cfg waterLevel = PipelineStage "applyErosion" "applyErosion" $ do
  logInfo "applyErosion: hydraulic + thermal"
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
      terrain' = IntMap.mapWithKey (updateChunkElevationFromGrid config (ChunkCoord minCx minCy) gridW elev2) terrain
  putWorldP world { twTerrain = terrain' }

erodeChunk :: WorldConfig -> ErosionConfig -> TerrainChunk -> TerrainChunk
erodeChunk config cfg chunk =
  let size = wcChunkSize config
      elev0 = tcElevation chunk
      elev1 = iterateN (ecHydraulicIterations cfg) (hydraulicStep size cfg) elev0
      elev2 = iterateN (ecThermalIterations cfg) (thermalStep size cfg) elev1
  in chunk { tcElevation = elev2 }

hydraulicStep :: Int -> ErosionConfig -> U.Vector Float -> U.Vector Float
hydraulicStep size cfg elev =
  U.generate (U.length elev) (hydraulicAt size cfg elev)

hydraulicStepGrid :: Int -> Int -> Float -> ErosionConfig -> U.Vector Float -> U.Vector Float -> U.Vector Float
hydraulicStepGrid gridW gridH waterLevel cfg hardness elev =
  U.generate (U.length elev) (hydraulicAtGrid gridW gridH waterLevel cfg hardness elev)

hydraulicAt :: Int -> ErosionConfig -> U.Vector Float -> Int -> Float
hydraulicAt size cfg elev i =
  let x = i `mod` size
      y = i `div` size
      h0 = elev U.! i
      hmin = minimumNeighbor size elev x y h0
      dh = h0 - hmin
      drop = min (ecMaxDrop cfg) (dh * ecRainRate cfg)
  in if dh <= 0 then h0 else h0 - drop

hydraulicAtGrid :: Int -> Int -> Float -> ErosionConfig -> U.Vector Float -> U.Vector Float -> Int -> Float
hydraulicAtGrid gridW gridH waterLevel cfg hardness elev i =
  let x = i `mod` gridW
      y = i `div` gridW
      h0 = elev U.! i
      hmin = minimumNeighborGrid gridW gridH elev x y h0
      dh = h0 - hmin
      wetFactor = if h0 < waterLevel then ecHydraulicWetFactor cfg else 1
      hard = clamp01 (hardness U.! i)
      rain = ecRainRate cfg * (1 - hard * ecHydraulicHardnessFactor cfg)
      drop = min (ecMaxDrop cfg) (dh * rain) * wetFactor
  in if dh <= 0 then h0 else h0 - drop

thermalStep :: Int -> ErosionConfig -> U.Vector Float -> U.Vector Float
thermalStep size cfg elev =
  U.generate (U.length elev) (thermalAt size cfg elev)

thermalStepGrid :: Int -> Int -> Float -> ErosionConfig -> U.Vector Float -> U.Vector Float -> U.Vector Float
thermalStepGrid gridW gridH waterLevel cfg hardness elev =
  U.generate (U.length elev) (thermalAtGrid gridW gridH waterLevel cfg hardness elev)

thermalAt :: Int -> ErosionConfig -> U.Vector Float -> Int -> Float
thermalAt size cfg elev i =
  let x = i `mod` size
      y = i `div` size
      h0 = elev U.! i
      hmin = minimumNeighbor size elev x y h0
      slope = h0 - hmin
      excess = slope - ecThermalTalus cfg
  in if excess <= 0 then h0 else h0 - min (ecMaxDrop cfg) (excess * ecThermalStrength cfg)

thermalAtGrid :: Int -> Int -> Float -> ErosionConfig -> U.Vector Float -> U.Vector Float -> Int -> Float
thermalAtGrid gridW gridH waterLevel cfg hardness elev i =
  let x = i `mod` gridW
      y = i `div` gridW
      h0 = elev U.! i
      hmin = minimumNeighborGrid gridW gridH elev x y h0
      slope = h0 - hmin
      excess = slope - ecThermalTalus cfg
      wetFactor = if h0 < waterLevel then ecThermalWetFactor cfg else 1
      hard = clamp01 (hardness U.! i)
      strength = ecThermalStrength cfg * (1 - hard * ecThermalHardnessFactor cfg)
  in if excess <= 0 then h0 else h0 - min (ecMaxDrop cfg) (excess * strength * wetFactor)

minimumNeighbor :: Int -> U.Vector Float -> Int -> Int -> Float -> Float
minimumNeighbor size elev x y h0 =
  let ix = y * size + x
      hL = if x > 0 then elev U.! (ix - 1) else h0
      hR = if x + 1 < size then elev U.! (ix + 1) else h0
      hU = if y > 0 then elev U.! (ix - size) else h0
      hD = if y + 1 < size then elev U.! (ix + size) else h0
  in minimum [h0, hL, hR, hU, hD]

minimumNeighborGrid :: Int -> Int -> U.Vector Float -> Int -> Int -> Float -> Float
minimumNeighborGrid gridW gridH elev x y h0 =
  let ix = y * gridW + x
      hL = if x > 0 then elev U.! (ix - 1) else h0
      hR = if x + 1 < gridW then elev U.! (ix + 1) else h0
      hU = if y > 0 then elev U.! (ix - gridW) else h0
      hD = if y + 1 < gridH then elev U.! (ix + gridW) else h0
  in minimum [h0, hL, hR, hU, hD]
