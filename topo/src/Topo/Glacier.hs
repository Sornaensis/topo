{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Glacier and ice dynamics stage.
module Topo.Glacier
  ( GlacierConfig(..)
  , defaultGlacierConfig
  , applyGlacierStage
    -- * Internals (exported for testing)
  , snowAccumGrid
  , diffuseHexGrid
  ) where

import Control.Monad.Except (throwError)
import GHC.Generics (Generic)
import Topo.Config.JSON
  (ToJSON(..), FromJSON(..), configOptions, mergeDefaults,
   genericToJSON, genericParseJSON)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Topo.Math (clamp01, iterateN)
import Topo.Parameters (TerrainFormConfig)
import Topo.Pipeline (PipelineStage(..))
import Topo.Pipeline.Stage (StageId(..))
import Topo.Plugin (PluginError(..), getWorldP, logInfo, putWorldP)
import Topo.TerrainForm.Modifiers
  ( TerrainFormModifiers(..)
  , defaultTerrainFormModifiers
  )
import Topo.TerrainGrid
  ( buildClimatePrecipGrid
  , buildClimateTempGrid
  , buildElevationGrid
  , buildHardnessGrid
  , buildPlateHardnessGrid
  , chunkGridSlice
  , classifyTerrainFormGrid
  , gridSlopeAt
  , updateChunkElevationFromGrid
  , validateTerrainGrid
  )
import Topo.Hex (hexNeighborIndices)
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
  } deriving (Eq, Show, Generic)

instance ToJSON GlacierConfig where
  toJSON = genericToJSON (configOptions "gc")

instance FromJSON GlacierConfig where
  parseJSON v = genericParseJSON (configOptions "gc")
                  (mergeDefaults (toJSON defaultGlacierConfig) v)

-- | Default glacier configuration.
defaultGlacierConfig :: GlacierConfig
defaultGlacierConfig = GlacierConfig
  { gcSnowTemp = 0.375
  , gcSnowRange = 0.245
  , gcMeltTemp = 0.48
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
--
-- The 'TerrainFormConfig' drives a lightweight pre-classification of
-- terrain forms from the current elevation + hardness grids.  Per-form
-- modifiers ('defaultTerrainFormModifiers') then adjust glacial erosion
-- intensity, deposition, and ice diffusion rates.
applyGlacierStage :: GlacierConfig -> TerrainFormConfig -> Float -> PipelineStage
applyGlacierStage cfg formCfg waterLevel = PipelineStage StageGlacier "applyGlaciers" "applyGlaciers" Nothing [] Nothing $ do
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
      plateHardness = buildPlateHardnessGrid config terrain minCoord gridW gridH
      temp = buildClimateTempGrid config climate minCoord gridW gridH
      precip = buildClimatePrecipGrid config climate minCoord gridW gridH
      -- Pre-classify terrain forms from current elevation + plate hardness.
      formGrid = classifyTerrainFormGrid formCfg waterLevel gridW gridH elev0 plateHardness
      modLookup = defaultTerrainFormModifiers
      erosionMult   = U.map (tfmErosionRate . modLookup) formGrid
      depositFactor = U.map (\f -> 1 - tfmDepositSuppression (modLookup f)) formGrid
      rateVec       = U.map (\f -> gcFlowRate cfg * (1 + tfmFlowBonus (modLookup f))) formGrid
      accumBonus    = U.map (tfmSnowAccumBonus . modLookup) formGrid
      snowpack = snowAccumGrid cfg accumBonus temp precip
      melt = meltGrid cfg temp
      ice0 = U.zipWith (\s m -> max 0 (s - m)) snowpack melt
      ice1 = diffuseHexGrid gridW gridH rateVec (gcFlowIterations cfg) ice0
      flow = U.zipWith (\a b -> abs (a - b)) ice0 ice1
      erosionPotential = glacierErosionPotential gridW gridH elev0 ice1 hardness erosionMult cfg
      depositPotential = glacierDepositPotential gridW gridH elev0 flow depositFactor cfg
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

-- | Per-tile snow accumulation from temperature and precipitation.
--
-- The @accumBonus@ vector carries a per-tile additive multiplier from
-- terrain form modifiers (e.g. plateaus accumulate more snow for
-- ice-cap formation).  Accumulation = @precip * scale * tempFactor
-- * (1 + bonus)@.
snowAccumGrid :: GlacierConfig -> U.Vector Float -> U.Vector Float -> U.Vector Float -> U.Vector Float
snowAccumGrid cfg accumBonus temp precip =
  let range = max 0.0001 (gcSnowRange cfg)
  in U.izipWith
      (\i t p ->
        let factor = clamp01 ((gcSnowTemp cfg - t) / range)
            bonus  = accumBonus U.! i
        in p * gcAccumScale cfg * factor * (1 + bonus))
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

-- | Diffuse glacier ice over the row-major axial grid using the in-bounds
-- hex neighbourhood at each tile.
diffuseHexGrid :: Int -> Int -> U.Vector Float -> Int -> U.Vector Float -> U.Vector Float
diffuseHexGrid gridW gridH rateVec iterations =
  iterateN iterations (diffuseStep gridW gridH rateVec)

diffuseStep :: Int -> Int -> U.Vector Float -> U.Vector Float -> U.Vector Float
diffuseStep gridW gridH rateVec values =
  U.generate (U.length values) (diffuseAt gridW gridH rateVec values)

-- | Diffuse a single tile's value toward the hex-neighbour average.
--
-- The @rateVec@ carries a per-tile diffusion rate that incorporates the
-- base flow rate augmented by 'tfmFlowBonus' from terrain form
-- classification (e.g. passes diffuse faster).
diffuseAt :: Int -> Int -> U.Vector Float -> U.Vector Float -> Int -> Float
diffuseAt gridW gridH rateVec values i =
  let v0 = values U.! i
      rate = rateVec U.! i
      nbrs = hexNeighborIndices gridW gridH i
      nbrVals = [values U.! j | j <- nbrs]
      total = v0 + sum nbrVals
      count = 1 + length nbrs
      avg = total / fromIntegral count
      next = v0 + rate * (avg - v0)
  in max 0 next

-- | Per-tile glacial erosion potential.
--
-- Scaled by the per-tile @erosionMult@ derived from terrain form
-- modifiers (e.g. badlands erode faster, mesas resist).
glacierErosionPotential
  :: Int
  -> Int
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Float  -- ^ Per-tile erosion rate multiplier
  -> GlacierConfig
  -> U.Vector Float
glacierErosionPotential gridW gridH elev ice hardness erosionMult cfg =
  U.imap
    (\i t ->
      let slope = clamp01 (gridSlopeAt gridW gridH elev i)
          hard = clamp01 (hardness U.! i)
          factor = clamp01 (1 - hard * gcHardnessErosionWeight cfg)
      in slope * t * gcErosionScale cfg * factor * (erosionMult U.! i))
    ice

-- | Per-tile glacial deposition potential.
--
-- Scaled by the per-tile @depositFactor@ (1 - suppression) derived
-- from terrain form modifiers (e.g. canyons suppress deposition).
glacierDepositPotential
  :: Int
  -> Int
  -> U.Vector Float
  -> U.Vector Float
  -> U.Vector Float  -- ^ Per-tile deposit factor (1 − suppression)
  -> GlacierConfig
  -> U.Vector Float
glacierDepositPotential gridW gridH elev flow depositFactor cfg =
  let maxSlope = max 0.0001 (gcDepositMaxSlope cfg)
  in U.imap
      (\i t ->
        let slope = gridSlopeAt gridW gridH elev i
            slopeFactor = if slope >= maxSlope then 0 else clamp01 (1 - slope / maxSlope)
        in t * slopeFactor * gcDepositScale cfg * (depositFactor U.! i))
      flow


