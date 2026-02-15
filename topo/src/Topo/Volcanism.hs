{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Volcanism stage for vents, magma, and eruption outputs.
module Topo.Volcanism
  ( VolcanismConfig(..)
  , defaultVolcanismConfig
  , applyVolcanismStage
  ) where

import Control.Monad.Except (throwError)
import GHC.Generics (Generic)
import Topo.Config.JSON
  (ToJSON(..), FromJSON(..), configOptions, mergeDefaults,
   genericToJSON, genericParseJSON)
import Control.Monad.Reader (ask)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Word (Word16, Word64)
import Topo.Math (clamp01)
import Topo.Noise (noise2D)
import Topo.Pipeline (PipelineStage(..))
import Topo.Plugin (PluginEnv(..), PluginError(..), getWorldP, logInfo, putWorldP)
import Topo.TerrainGrid
  ( buildElevationGrid
  , buildPlateBoundaryGrid
  , chunkGridSlice
  , updateChunkElevationFromGrid
  , validateTerrainGrid
  )
import Topo.Types
import Topo.World (TerrainWorld(..))
import qualified Data.Vector.Unboxed as U

-- | Volcanism stage configuration.
--
-- Controls vent generation, eruption thresholds, magma dynamics, and
-- deposit effects on terrain and soil.
data VolcanismConfig = VolcanismConfig
  { vcVentDensityBase :: !Float
    -- ^ Base probability of vent formation [0..1].
  , vcVentThreshold :: !Float
    -- ^ Noise threshold above which a vent forms [0..1].
  , vcBoundaryConvergentWeight :: !Float
    -- ^ Weight of convergent plate boundaries in vent placement [0..1].
  , vcBoundaryDivergentWeight :: !Float
    -- ^ Weight of divergent plate boundaries in vent placement [0..1].
  , vcBoundaryTransformWeight :: !Float
    -- ^ Weight of transform plate boundaries in vent placement [0..1].
  , vcHotspotScale :: !Float
    -- ^ Noise scale for hotspot detection (cycles per tile).
  , vcHotspotThreshold :: !Float
    -- ^ Noise threshold for hotspot activation [0..1].
  , vcActiveThreshold :: !Float
    -- ^ Magma level above which a volcano is considered active [0..1].
  , vcEruptThreshold :: !Float
    -- ^ Magma level above which eruption occurs [0..1].
  , vcMagmaRecharge :: !Float
    -- ^ Magma recharge rate per step.
  , vcEruptMagmaCost :: !Float
    -- ^ Magma consumed per eruption event.
  , vcLavaScale :: !Float
    -- ^ Lava deposit scale [0..1].
  , vcAshScale :: !Float
    -- ^ Ash deposit scale [0..1].
  , vcDepositScale :: !Float
    -- ^ Overall deposit intensity scale [0..1].
  , vcDepositRaiseScale :: !Float
    -- ^ Elevation raise per unit of volcanic deposit (elevation units).
  , vcRockDensityLavaBoost :: !Float
    -- ^ Rock density boost from lava deposits [0..1].
  , vcSoilGrainAshBoost :: !Float
    -- ^ Soil grain size boost from ash deposits [0..1].
  } deriving (Eq, Show, Generic)

instance ToJSON VolcanismConfig where
  toJSON = genericToJSON (configOptions "vc")

instance FromJSON VolcanismConfig where
  parseJSON v = genericParseJSON (configOptions "vc")
                  (mergeDefaults (toJSON defaultVolcanismConfig) v)

-- | Default volcanism configuration.
defaultVolcanismConfig :: VolcanismConfig
defaultVolcanismConfig = VolcanismConfig
  { vcVentDensityBase = 0.05
  , vcVentThreshold = 0.55
  , vcBoundaryConvergentWeight = 0.35
  , vcBoundaryDivergentWeight = 0.25
  , vcBoundaryTransformWeight = 0.2
  , vcHotspotScale = 0.5
  , vcHotspotThreshold = 0.7
  , vcActiveThreshold = 0.2
  , vcEruptThreshold = 0.45
  , vcMagmaRecharge = 1
  , vcEruptMagmaCost = 0.6
  , vcLavaScale = 0.6
  , vcAshScale = 0.4
  , vcDepositScale = 0.8
  , vcDepositRaiseScale = 0.02
  , vcRockDensityLavaBoost = 0.35
  , vcSoilGrainAshBoost = 0.4
  }

-- | Apply volcanism vents and eruption outputs.
applyVolcanismStage :: VolcanismConfig -> PipelineStage
applyVolcanismStage cfg = PipelineStage "applyVolcanism" "applyVolcanism" $ do
  logInfo "applyVolcanism: vents + magma"
  world <- getWorldP
  env <- ask
  let config = twConfig world
      terrain = twTerrain world
  (ChunkCoord minCx minCy, ChunkCoord maxCx maxCy) <-
    case validateTerrainGrid config terrain of
      Left err -> throwError (PluginInvariantError err)
      Right bounds -> pure bounds
  let size = wcChunkSize config
      gridW = (maxCx - minCx + 1) * size
      gridH = (maxCy - minCy + 1) * size
      minCoord = ChunkCoord minCx minCy
      elev0 = buildElevationGrid config terrain minCoord gridW gridH
      boundaries = buildPlateBoundaryGrid config terrain minCoord gridW gridH
      seed = peSeed env
      hotspot = hotspotGrid seed minCoord size gridW gridH
      (ventTypes, activities, magma, eruptionCount, eruptedTotal, lava, ash, deposit) =
        volcanismFields cfg boundaries hotspot
      volcanism = VolcanismChunk
        { vcVentType = ventTypes
        , vcActivity = activities
        , vcMagma = magma
        , vcEruptionCount = eruptionCount
        , vcEruptedTotal = eruptedTotal
        , vcLavaPotential = lava
        , vcAshPotential = ash
        , vcDepositPotential = deposit
        }
      volcanism' = IntMap.mapWithKey (sliceVolcanismChunk config minCoord gridW volcanism) terrain
      elev1 = U.zipWith (+) elev0 (U.map (* vcDepositRaiseScale cfg) deposit)
      terrain' = IntMap.mapWithKey (updateChunkElevationFromGrid config minCoord gridW elev1) terrain
      terrain'' = IntMap.mergeWithKey
        (\_ chunk volc -> Just (applyVolcanicTerrain cfg chunk volc))
        id
        (const IntMap.empty)
        terrain'
        volcanism'
  putWorldP world { twVolcanism = volcanism', twTerrain = terrain'' }

applyVolcanicTerrain :: VolcanismConfig -> TerrainChunk -> VolcanismChunk -> TerrainChunk
applyVolcanicTerrain cfg chunk volc =
  let rockDensity = U.zipWith
        (\base lava -> clamp01 (base + lava * vcRockDensityLavaBoost cfg))
        (tcRockDensity chunk)
        (vcLavaPotential volc)
      soilGrain = U.zipWith
        (\base ash -> clamp01 (base + ash * vcSoilGrainAshBoost cfg))
        (tcSoilGrain chunk)
        (vcAshPotential volc)
  in chunk
      { tcRockDensity = rockDensity
      , tcSoilGrain = soilGrain
      }

hotspotGrid :: Word64 -> ChunkCoord -> Int -> Int -> Int -> U.Vector Float
hotspotGrid seed (ChunkCoord minCx minCy) chunkSize gridW gridH =
  let baseX = minCx * chunkSize
      baseY = minCy * chunkSize
      total = gridW * gridH
  in U.generate total (\i ->
      let x = i `mod` gridW
          y = i `div` gridW
          gx = baseX + x
          gy = baseY + y
      in clamp01 (noise2D seed gx gy))

volcanismFields
  :: VolcanismConfig
  -> U.Vector PlateBoundary
  -> U.Vector Float
  -> ( U.Vector VentType
     , U.Vector VentActivity
     , U.Vector Float
     , U.Vector Word16
     , U.Vector Float
     , U.Vector Float
     , U.Vector Float
     , U.Vector Float
     )
volcanismFields cfg boundaries hotspot =
  let n = U.length hotspot
  in ( U.generate n (ventTypeAt cfg boundaries hotspot)
     , U.generate n (activityAt cfg boundaries hotspot)
     , U.generate n (magmaAt cfg boundaries hotspot)
     , U.generate n (eruptionCountAt cfg boundaries hotspot)
     , U.generate n (eruptionTotalAt cfg boundaries hotspot)
     , U.generate n (lavaAt cfg boundaries hotspot)
     , U.generate n (ashAt cfg boundaries hotspot)
     , U.generate n (depositAt cfg boundaries hotspot)
     )

ventTypeAt :: VolcanismConfig -> U.Vector PlateBoundary -> U.Vector Float -> Int -> VentType
ventTypeAt cfg boundaries hotspot i =
  let boundary = boundaries U.! i
      hotspotVal = hotspot U.! i * vcHotspotScale cfg
      boundaryWeight = boundaryWeightFor cfg boundary
      hotspotActive = hotspotVal >= vcHotspotThreshold cfg
      ventActive = vcVentDensityBase cfg + boundaryWeight + hotspotVal >= vcVentThreshold cfg
  in if not ventActive
      then VentNone
      else if boundaryWeight >= hotspotVal
        then boundaryVentType boundary
        else if hotspotActive then VentShield else VentNone

boundaryVentType :: PlateBoundary -> VentType
boundaryVentType boundary =
  case boundary of
    PlateBoundaryConvergent -> VentStratovolcano
    PlateBoundaryDivergent -> VentFissure
    PlateBoundaryTransform -> VentFissure
    PlateBoundaryNone -> VentNone

activityAt :: VolcanismConfig -> U.Vector PlateBoundary -> U.Vector Float -> Int -> VentActivity
activityAt cfg boundaries hotspot i =
  let magma = magmaAt cfg boundaries hotspot i
  in if magma >= vcEruptThreshold cfg
      then VentErupting
      else if magma >= vcActiveThreshold cfg
        then VentActive
        else VentDormant

magmaAt :: VolcanismConfig -> U.Vector PlateBoundary -> U.Vector Float -> Int -> Float
magmaAt cfg boundaries hotspot i =
  let boundary = boundaries U.! i
      hotspotVal = hotspot U.! i * vcHotspotScale cfg
      boundaryWeight = boundaryWeightFor cfg boundary
      ventScore = vcVentDensityBase cfg + boundaryWeight + hotspotVal
  in if ventScore >= vcVentThreshold cfg
      then clamp01 ventScore * vcMagmaRecharge cfg
      else 0

eruptionCountAt :: VolcanismConfig -> U.Vector PlateBoundary -> U.Vector Float -> Int -> Word16
eruptionCountAt cfg boundaries hotspot i =
  if activityAt cfg boundaries hotspot i == VentErupting then 1 else 0

eruptionTotalAt :: VolcanismConfig -> U.Vector PlateBoundary -> U.Vector Float -> Int -> Float
eruptionTotalAt cfg boundaries hotspot i =
  let magma = magmaAt cfg boundaries hotspot i
  in if magma >= vcEruptThreshold cfg then magma * vcEruptMagmaCost cfg else 0

lavaAt :: VolcanismConfig -> U.Vector PlateBoundary -> U.Vector Float -> Int -> Float
lavaAt cfg boundaries hotspot i =
  let magma = magmaAt cfg boundaries hotspot i
  in if magma >= vcEruptThreshold cfg then magma * vcLavaScale cfg else 0

ashAt :: VolcanismConfig -> U.Vector PlateBoundary -> U.Vector Float -> Int -> Float
ashAt cfg boundaries hotspot i =
  let magma = magmaAt cfg boundaries hotspot i
  in if magma >= vcEruptThreshold cfg then magma * vcAshScale cfg else 0

depositAt :: VolcanismConfig -> U.Vector PlateBoundary -> U.Vector Float -> Int -> Float
depositAt cfg boundaries hotspot i =
  let lava = lavaAt cfg boundaries hotspot i
      ash = ashAt cfg boundaries hotspot i
  in (lava + ash) * vcDepositScale cfg

boundaryWeightFor :: VolcanismConfig -> PlateBoundary -> Float
boundaryWeightFor cfg boundary =
  case boundary of
    PlateBoundaryConvergent -> vcBoundaryConvergentWeight cfg
    PlateBoundaryDivergent -> vcBoundaryDivergentWeight cfg
    PlateBoundaryTransform -> vcBoundaryTransformWeight cfg
    PlateBoundaryNone -> 0

sliceVolcanismChunk :: WorldConfig -> ChunkCoord -> Int -> VolcanismChunk -> Int -> TerrainChunk -> VolcanismChunk
sliceVolcanismChunk config minCoord gridW volcanism _key _chunk =
  VolcanismChunk
    { vcVentType = chunkGridSliceGeneric config minCoord gridW (vcVentType volcanism) _key
    , vcActivity = chunkGridSliceGeneric config minCoord gridW (vcActivity volcanism) _key
    , vcMagma = chunkGridSlice config minCoord gridW (vcMagma volcanism) _key
    , vcEruptionCount = chunkGridSliceGeneric config minCoord gridW (vcEruptionCount volcanism) _key
    , vcEruptedTotal = chunkGridSlice config minCoord gridW (vcEruptedTotal volcanism) _key
    , vcLavaPotential = chunkGridSlice config minCoord gridW (vcLavaPotential volcanism) _key
    , vcAshPotential = chunkGridSlice config minCoord gridW (vcAshPotential volcanism) _key
    , vcDepositPotential = chunkGridSlice config minCoord gridW (vcDepositPotential volcanism) _key
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
