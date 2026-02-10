{-# LANGUAGE OverloadedStrings #-}

-- | Glacier and ice dynamics stage.
module Topo.Glacier
  ( GlacierConfig(..)
  , defaultGlacierConfig
  , applyGlacierStage
  ) where

import Control.Monad.Except (throwError)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Topo.Math (clamp01, iterateN)
import Topo.Pipeline (PipelineStage(..))
import Topo.Plugin (PluginError(..), getWorldP, logInfo, putWorldP)
import Topo.TerrainGrid
  ( buildClimatePrecipGrid
  , buildClimateTempGrid
  , buildElevationGrid
  , buildHardnessGrid
  , chunkGridSlice
  , updateChunkElevationFromGrid
  , validateTerrainGrid
  )
import Topo.Types
import Topo.World (TerrainWorld(..))
import qualified Data.Vector.Unboxed as U

-- | Glacier stage configuration.
data GlacierConfig = GlacierConfig
  { -- | Temperature below which snow begins to accumulate.
    gcSnowTemp :: !Float
    -- | Temperature range over which snow accumulation fades out.
  , gcSnowRange :: !Float
    -- | Temperature above which melt starts.
  , gcMeltTemp :: !Float
    -- | Melt rate per temperature unit above the melt threshold.
  , gcMeltRate :: !Float
    -- | Scale factor applied to precipitation when accumulating snow.
  , gcAccumScale :: !Float
    -- | Diffusion iterations for ice flow.
  , gcFlowIterations :: !Int
    -- | Diffusion rate for ice flow.
  , gcFlowRate :: !Float
    -- | [0..1] scaling of hardness against erosion potential.
  , gcHardnessErosionWeight :: !Float
    -- | Scale factor for glacial erosion potential.
  , gcErosionScale :: !Float
    -- | Scale factor for glacial deposition potential.
  , gcDepositScale :: !Float
    -- | Slope threshold for glacial deposition.
  , gcDepositMaxSlope :: !Float
    -- | Scale factor for carving elevation from erosion potential.
  , gcCarveScale :: !Float
    -- | Scale factor for raising elevation from deposition potential.
  , gcDepositRaiseScale :: !Float
  } deriving (Eq, Show)

-- | Default glacier configuration.
defaultGlacierConfig :: GlacierConfig
defaultGlacierConfig = GlacierConfig
  { gcSnowTemp = 0.25
  , gcSnowRange = 0.35
  , gcMeltTemp = 0.4
  , gcMeltRate = 0.2
  , gcAccumScale = 1
  , gcFlowIterations = 3
  , gcFlowRate = 0.2
  , gcHardnessErosionWeight = 0.6
  , gcErosionScale = 0.25
  , gcDepositScale = 0.2
  , gcDepositMaxSlope = 0.1
  , gcCarveScale = 0.01
  , gcDepositRaiseScale = 0.01
  }

-- | Apply glacier accumulation, flow, and erosion potentials.
applyGlacierStage :: GlacierConfig -> PipelineStage
applyGlacierStage cfg = PipelineStage "applyGlaciers" "applyGlaciers" $ do
  logInfo "applyGlaciers: snowpack + ice flow"
  world <- getWorldP
  let config = twConfig world
      terrain = twTerrain world
      climate = twClimate world
  (ChunkCoord minCx minCy, ChunkCoord maxCx maxCy) <-
    case validateTerrainGrid config terrain of
      Left err -> throwError (PluginInvariantError err)
      Right bounds -> pure bounds
  let size = wcChunkSize config
      gridW = (maxCx - minCx + 1) * size
      gridH = (maxCy - minCy + 1) * size
      minCoord = ChunkCoord minCx minCy
      elev0 = buildElevationGrid config terrain minCoord gridW gridH
      hardness = buildHardnessGrid config terrain minCoord gridW gridH
      temp = buildClimateTempGrid config climate minCoord gridW gridH
      precip = buildClimatePrecipGrid config climate minCoord gridW gridH
      snowpack = snowAccumGrid cfg temp precip
      melt = meltGrid cfg temp
      ice0 = U.zipWith (\s m -> max 0 (s - m)) snowpack melt
      ice1 = diffuseGrid gridW gridH (gcFlowRate cfg) (gcFlowIterations cfg) ice0
      flow = U.zipWith (\a b -> abs (a - b)) ice0 ice1
      erosionPotential = glacierErosionPotential gridW gridH elev0 ice1 hardness cfg
      depositPotential = glacierDepositPotential gridW gridH elev0 flow cfg
      elev1 = applyGlacierCarve elev0 erosionPotential depositPotential cfg
      glaciers = GlacierChunk
        { glSnowpack = snowpack
        , glIceThickness = ice1
        , glMelt = melt
        , glFlow = flow
        , glErosionPotential = erosionPotential
        , glDepositPotential = depositPotential
        }
      glaciers' = IntMap.mapWithKey (sliceGlacierChunk config minCoord gridW glaciers) terrain
      terrain' = IntMap.mapWithKey (updateChunkElevationFromGrid config minCoord gridW elev1) terrain
  putWorldP world { twGlaciers = glaciers', twTerrain = terrain' }

snowAccumGrid :: GlacierConfig -> U.Vector Float -> U.Vector Float -> U.Vector Float
snowAccumGrid cfg temp precip =
  let range = max 0.0001 (gcSnowRange cfg)
  in U.zipWith
      (\t p ->
        let factor = clamp01 ((gcSnowTemp cfg - t) / range)
        in p * gcAccumScale cfg * factor)
      temp
      precip

meltGrid :: GlacierConfig -> U.Vector Float -> U.Vector Float
meltGrid cfg temp =
  U.map (\t -> if t > gcMeltTemp cfg then (t - gcMeltTemp cfg) * gcMeltRate cfg else 0) temp

applyGlacierCarve :: U.Vector Float -> U.Vector Float -> U.Vector Float -> GlacierConfig -> U.Vector Float
applyGlacierCarve elev erosion deposit cfg =
  U.zipWith3
    (\h e d -> h - e * gcCarveScale cfg + d * gcDepositRaiseScale cfg)
    elev
    erosion
    deposit

sliceGlacierChunk :: WorldConfig -> ChunkCoord -> Int -> GlacierChunk -> Int -> TerrainChunk -> GlacierChunk
sliceGlacierChunk config minCoord gridW glaciers _key _chunk =
  GlacierChunk
    { glSnowpack = chunkGridSlice config minCoord gridW (glSnowpack glaciers) _key
    , glIceThickness = chunkGridSlice config minCoord gridW (glIceThickness glaciers) _key
    , glMelt = chunkGridSlice config minCoord gridW (glMelt glaciers) _key
    , glFlow = chunkGridSlice config minCoord gridW (glFlow glaciers) _key
    , glErosionPotential = chunkGridSlice config minCoord gridW (glErosionPotential glaciers) _key
    , glDepositPotential = chunkGridSlice config minCoord gridW (glDepositPotential glaciers) _key
    }

diffuseGrid :: Int -> Int -> Float -> Int -> U.Vector Float -> U.Vector Float
diffuseGrid gridW gridH rate iterations =
  iterateN iterations (diffuseStep gridW gridH rate)

diffuseStep :: Int -> Int -> Float -> U.Vector Float -> U.Vector Float
diffuseStep gridW gridH rate values =
  U.generate (U.length values) (diffuseAt gridW gridH rate values)

diffuseAt :: Int -> Int -> Float -> U.Vector Float -> Int -> Float
diffuseAt gridW gridH rate values i =
  let x = i `mod` gridW
      y = i `div` gridW
      v0 = values U.! i
      neighbors =
        [ values U.! (i - 1) | x > 0 ]
        <> [ values U.! (i + 1) | x + 1 < gridW ]
        <> [ values U.! (i - gridW) | y > 0 ]
        <> [ values U.! (i + gridW) | y + 1 < gridH ]
      total = v0 + sum neighbors
      count = 1 + length neighbors
      avg = total / fromIntegral count
      next = v0 + rate * (avg - v0)
  in max 0 next

glacierErosionPotential
  :: Int
  -> Int
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Float
  -> GlacierConfig
  -> U.Vector Float
glacierErosionPotential gridW gridH elev ice hardness cfg =
  U.imap
    (\i t ->
      let slope = clamp01 (gridSlopeAt gridW gridH elev i)
          hard = clamp01 (hardness U.! i)
          factor = clamp01 (1 - hard * gcHardnessErosionWeight cfg)
      in slope * t * gcErosionScale cfg * factor)
    ice

glacierDepositPotential
  :: Int
  -> Int
  -> U.Vector Float
  -> U.Vector Float
  -> GlacierConfig
  -> U.Vector Float
glacierDepositPotential gridW gridH elev flow cfg =
  let maxSlope = max 0.0001 (gcDepositMaxSlope cfg)
  in U.imap
      (\i t ->
        let slope = gridSlopeAt gridW gridH elev i
            slopeFactor = if slope >= maxSlope then 0 else clamp01 (1 - slope / maxSlope)
        in t * slopeFactor * gcDepositScale cfg)
      flow

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
