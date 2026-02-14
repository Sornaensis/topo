{-# LANGUAGE OverloadedStrings #-}

-- | Biome classification configuration and pipeline stage.
module Topo.BiomeConfig
  ( BiomeConfig(..)
  , defaultBiomeConfig
  , aridBiomeConfig
  , lushBiomeConfig
  , classifyBiomesStage
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Topo.Biome
  ( BiomeRule
  , BiomeThresholds
  , VegetationConfig(..)
  , classifyBiomesChunk
  , defaultBiomeRules
  , defaultBiomeThresholds
  , defaultVegetationConfig
  , smoothBiomesGrid
  , vegetationDensityChunk
  )
import Topo.Biome.Refine
  ( RefinementConfig(..)
  , defaultRefinementConfig
  , aridRefinementConfig
  , lushRefinementConfig
  , refineBiomesChunk
  )
import Topo.Math (clamp01)
import Topo.Pipeline (PipelineStage(..))
import Topo.Plugin (logInfo, modifyWorldP)
import Topo.TerrainGrid (chunkCoordBounds)
import Topo.Types
import Topo.World (TerrainWorld(..))
import qualified Data.Vector.Unboxed as U

-- | Biome configuration for classification, refinement, and vegetation.
data BiomeConfig = BiomeConfig
  { bcRules :: !BiomeRule
  , bcThresholds :: !BiomeThresholds
  , bcRefinement :: !RefinementConfig
  , bcVegetation :: !VegetationConfig
  , bcSmoothingIterations :: !Int
  , bcVolcanicAshBoost :: !Float
  , bcVolcanicLavaPenalty :: !Float
  } deriving (Eq, Show)

-- | Default biome configuration.
defaultBiomeConfig :: BiomeConfig
defaultBiomeConfig = BiomeConfig
  { bcRules = defaultBiomeRules
  , bcThresholds = defaultBiomeThresholds
  , bcRefinement = defaultRefinementConfig
  , bcVegetation = defaultVegetationConfig
  , bcSmoothingIterations = 1
  , bcVolcanicAshBoost = 0.2
  , bcVolcanicLavaPenalty = 0.35
  }

-- | Dry-biased biome configuration.
aridBiomeConfig :: BiomeConfig
aridBiomeConfig = BiomeConfig
  { bcRules = defaultBiomeRules
  , bcThresholds = defaultBiomeThresholds
  , bcRefinement = aridRefinementConfig
  , bcVegetation = (defaultVegetationConfig { vcBaseDensity = 0.08, vcBiomeBoost = 0.3, vcTempWeight = 0.4, vcPrecipWeight = 0.6 })
  , bcSmoothingIterations = 1
  , bcVolcanicAshBoost = 0.12
  , bcVolcanicLavaPenalty = 0.4
  }

-- | Lush-biased biome configuration.
lushBiomeConfig :: BiomeConfig
lushBiomeConfig = BiomeConfig
  { bcRules = defaultBiomeRules
  , bcThresholds = defaultBiomeThresholds
  , bcRefinement = lushRefinementConfig
  , bcVegetation = (defaultVegetationConfig { vcBaseDensity = 0.35, vcBiomeBoost = 0.8, vcTempWeight = 0.55, vcPrecipWeight = 0.45 })
  , bcSmoothingIterations = 1
  , bcVolcanicAshBoost = 0.28
  , bcVolcanicLavaPenalty = 0.25
  }

-- | Assign biome ids and vegetation density from climate and terrain.
--
-- Stores biome IDs in 'tcFlags' and biome-derived vegetation density
-- in 'vegDensity' on 'VegetationChunk'.  Does /not/ overwrite
-- 'tcFertility', which retains its soil-derived meaning.
classifyBiomesStage :: BiomeConfig -> Float -> PipelineStage
classifyBiomesStage cfg waterLevel = PipelineStage "classifyBiomes" "classifyBiomes" $ do
  logInfo "classifyBiomes: assigning biome ids"
  modifyWorldP $ \world ->
    let config = twConfig world
        terrain = twTerrain world
        climate = twClimate world
        waterBodies = twWaterBodies world
        existingVeg = twVegetation world
        -- 1. Primary classification
        biomes = IntMap.mapWithKey (\k tc ->
          case IntMap.lookup k climate of
            Nothing -> U.empty
            Just cc -> classifyChunk config (bcRules cfg) (bcThresholds cfg) waterLevel
                         (IntMap.lookup k waterBodies) tc cc) terrain
        -- 2. Inter-chunk smoothing on a stitched global grid
        biomes' = smoothBiomesGlobal config (bcSmoothingIterations cfg) biomes
        -- 3. Refinement pass (sub-biome discrimination)
        biomes'' = refineAllChunks (bcRefinement cfg) waterLevel biomes' terrain climate
                     (twWeather world) (twRivers world) (twGroundwater world)
                     (twVolcanism world) (twGlaciers world) waterBodies
        -- 4. Store biome ids in terrain flags
        terrain' = IntMap.mergeWithKey
          (\_ chunk ids -> Just chunk { tcFlags = ids })
          (fmap (\chunk -> chunk { tcFlags = tcFlags chunk }))
          (const IntMap.empty)
          terrain
          biomes''
        -- 5. Vegetation density → VegetationChunk.vegDensity
        densityMap = IntMap.intersectionWith (vegetationChunk config (bcVegetation cfg)) biomes'' climate
        densityMap' = IntMap.mergeWithKey
          (\_ base volc -> Just (applyVolcanicVegetation cfg base volc))
          id
          (const IntMap.empty)
          densityMap
          (twVolcanism world)
        -- Merge density into existing VegetationChunk (preserve cover/albedo)
        vegetation' = IntMap.mergeWithKey
          (\_ vc dens -> Just vc { vegDensity = dens })
          id
          (IntMap.map (\dens -> VegetationChunk
            { vegCover   = U.replicate (U.length dens) 0
            , vegAlbedo  = U.replicate (U.length dens) 0.30
            , vegDensity = dens
            }))
          existingVeg
          densityMap'
    in world { twTerrain = terrain', twVegetation = vegetation' }

classifyChunk :: WorldConfig -> BiomeRule -> BiomeThresholds -> Float -> Maybe WaterBodyChunk -> TerrainChunk -> ClimateChunk -> U.Vector BiomeId
classifyChunk config rules thresholds waterLevel mWb terrain climate =
  let n = chunkTileCount config
      wbt    = maybe (U.replicate n WaterDry) wbType mWb
      adjWbt = maybe (U.replicate n WaterDry) wbAdjacentType mWb
  in classifyBiomesChunk config rules thresholds waterLevel wbt adjWbt
       (ccTempAvg climate) (ccPrecipAvg climate)
       (tcElevation terrain) (tcSlope terrain) (tcRelief terrain)
       (tcMoisture terrain) (tcTerrainForm terrain)

vegetationChunk :: WorldConfig -> VegetationConfig -> U.Vector BiomeId -> ClimateChunk -> U.Vector Float
vegetationChunk _ cfg biomes climate =
  vegetationDensityChunk cfg biomes (ccTempAvg climate) (ccPrecipAvg climate)

applyVolcanicVegetation :: BiomeConfig -> U.Vector Float -> VolcanismChunk -> U.Vector Float
applyVolcanicVegetation cfg base volc =
  let ash = vcAshPotential volc
      lava = vcLavaPotential volc
  in U.zipWith3
      (\b a l -> clamp01 (b + a * bcVolcanicAshBoost cfg - l * bcVolcanicLavaPenalty cfg))
      base
      ash
      lava

-- | Run 'refineBiomesChunk' across all chunks, looking up optional layer
-- data (rivers, groundwater, volcanism, glaciers) per chunk key.
refineAllChunks
  :: RefinementConfig
  -> Float                        -- ^ waterLevel
  -> IntMap (U.Vector BiomeId)    -- ^ smoothed primary biomes per chunk
  -> IntMap TerrainChunk
  -> IntMap ClimateChunk
  -> IntMap WeatherChunk
  -> IntMap RiverChunk
  -> IntMap GroundwaterChunk
  -> IntMap VolcanismChunk
  -> IntMap GlacierChunk
  -> IntMap WaterBodyChunk
  -> IntMap (U.Vector BiomeId)
refineAllChunks rcfg wl biomes terrain climate weather rivers gw volc glac wb =
  IntMap.mapWithKey refineOne biomes
  where
    refineOne k primary =
      case (IntMap.lookup k terrain, IntMap.lookup k climate) of
        (Just tc, Just cc) ->
          let wc = IntMap.findWithDefault defaultWeatherChunk k weather
          in refineBiomesChunk rcfg wl primary tc cc wc
               (IntMap.lookup k rivers)
               (IntMap.lookup k gw)
               (IntMap.lookup k volc)
               (IntMap.lookup k glac)
               (IntMap.lookup k wb)
        _ -> primary  -- missing terrain or climate → pass through

-- | Empty weather chunk for chunks that haven't had weather computed yet.
defaultWeatherChunk :: WeatherChunk
defaultWeatherChunk = WeatherChunk
  { wcTemp     = U.empty
  , wcHumidity = U.empty
  , wcWindDir  = U.empty
  , wcWindSpd  = U.empty
  , wcPressure = U.empty
  , wcPrecip   = U.empty
  }

---------------------------------------------------------------------------
-- Inter-chunk global smoothing
---------------------------------------------------------------------------

-- | Assemble all chunk biome vectors into a single flat grid, run
-- hex-aware majority-vote smoothing at the global scale, then slice the
-- results back into per-chunk vectors.
--
-- Falls back to no smoothing if the chunk map is empty.
smoothBiomesGlobal
  :: WorldConfig
  -> Int                          -- ^ smoothing iterations
  -> IntMap (U.Vector BiomeId)    -- ^ per-chunk biome vectors
  -> IntMap (U.Vector BiomeId)
smoothBiomesGlobal _ iterations biomes
  | iterations <= 0 = biomes
  | IntMap.null biomes = biomes
smoothBiomesGlobal config iterations biomes =
  case chunkCoordBounds biomes of
    Nothing -> biomes
    Just (ChunkCoord minCx minCy, ChunkCoord maxCx maxCy) ->
      let size = wcChunkSize config
          gridW = (maxCx - minCx + 1) * size
          gridH = (maxCy - minCy + 1) * size
          -- Assemble: per-chunk → global grid
          globalGrid = buildBiomeGrid biomes config (ChunkCoord minCx minCy) gridW gridH
          -- Smooth on the global grid (cross-chunk hex neighbours)
          smoothed = smoothBiomesGrid iterations gridW gridH globalGrid
          -- Slice back: global grid → per-chunk vectors
      in IntMap.mapWithKey (\k _ -> sliceBiomeGrid config (ChunkCoord minCx minCy) gridW smoothed k) biomes

-- | Assemble per-chunk biome vectors into a single flat row-major grid.
buildBiomeGrid
  :: IntMap (U.Vector BiomeId)
  -> WorldConfig
  -> ChunkCoord                   -- ^ min chunk coord
  -> Int                          -- ^ gridW
  -> Int                          -- ^ gridH
  -> U.Vector BiomeId
buildBiomeGrid biomeMap config (ChunkCoord minCx minCy) gridW gridH =
  let size = wcChunkSize config
      minTileX = minCx * size
      minTileY = minCy * size
      sampleAt idx =
        let x = idx `mod` gridW
            y = idx `div` gridW
            gx = minTileX + x
            gy = minTileY + y
            tile = TileCoord gx gy
            (chunkCoord, local) = chunkCoordFromTile config tile
            ChunkId key = chunkIdFromCoord chunkCoord
        in case IntMap.lookup key biomeMap of
            Nothing -> BiomeDesert  -- safety fallback
            Just vec ->
              case tileIndex config local of
                Nothing -> BiomeDesert
                Just (TileIndex i)
                  | i < U.length vec -> vec U.! i
                  | otherwise        -> BiomeDesert
  in U.generate (gridW * gridH) sampleAt

-- | Extract a chunk-sized biome vector from a global grid.
sliceBiomeGrid
  :: WorldConfig
  -> ChunkCoord                   -- ^ min chunk coord
  -> Int                          -- ^ gridW
  -> U.Vector BiomeId             -- ^ global grid
  -> Int                          -- ^ chunk key
  -> U.Vector BiomeId
sliceBiomeGrid config (ChunkCoord minCx minCy) gridW grid key =
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
