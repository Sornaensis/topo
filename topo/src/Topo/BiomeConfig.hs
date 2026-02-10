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
  , smoothBiomesChunk
  , vegetationDensityChunk
  )
import Topo.Math (clamp01)
import Topo.Pipeline (PipelineStage(..))
import Topo.Plugin (logInfo, modifyWorldP)
import Topo.Types
import Topo.World (TerrainWorld(..))
import qualified Data.Vector.Unboxed as U

-- | Biome configuration for classification and vegetation.
data BiomeConfig = BiomeConfig
  { bcRules :: !BiomeRule
  , bcThresholds :: !BiomeThresholds
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
  , bcVegetation = (defaultVegetationConfig { vcBaseDensity = 0.35, vcBiomeBoost = 0.8, vcTempWeight = 0.55, vcPrecipWeight = 0.45 })
  , bcSmoothingIterations = 1
  , bcVolcanicAshBoost = 0.28
  , bcVolcanicLavaPenalty = 0.25
  }

-- | Assign biome ids and vegetation from climate and terrain.
classifyBiomesStage :: BiomeConfig -> Float -> PipelineStage
classifyBiomesStage cfg waterLevel = PipelineStage "classifyBiomes" "classifyBiomes" $ do
  logInfo "classifyBiomes: assigning biome ids"
  modifyWorldP $ \world ->
    let config = twConfig world
        terrain = twTerrain world
        climate = twClimate world
        biomes = IntMap.intersectionWith (classifyChunk config (bcRules cfg) (bcThresholds cfg) waterLevel) terrain climate
        biomes' = IntMap.map (smoothBiomesChunk config (bcSmoothingIterations cfg)) biomes
        terrain' = IntMap.mergeWithKey
          (\_ chunk ids -> Just chunk { tcFlags = ids })
          (fmap (\chunk -> chunk { tcFlags = tcFlags chunk }))
          (const IntMap.empty)
          terrain
          biomes'
        vegetation = IntMap.intersectionWith (vegetationChunk config (bcVegetation cfg)) biomes' climate
        vegetation' = IntMap.mergeWithKey
          (\_ base volc -> Just (applyVolcanicVegetation cfg base volc))
          id
          (const IntMap.empty)
          vegetation
          (twVolcanism world)
        terrain'' = IntMap.mergeWithKey
          (\_ chunk dens -> Just chunk { tcFertility = dens })
          (fmap (\chunk -> chunk { tcFertility = tcFertility chunk }))
          (const IntMap.empty)
          terrain'
          vegetation'
    in world { twTerrain = terrain'' }

classifyChunk :: WorldConfig -> BiomeRule -> BiomeThresholds -> Float -> TerrainChunk -> ClimateChunk -> U.Vector BiomeId
classifyChunk config rules thresholds waterLevel terrain climate =
  classifyBiomesChunk config rules thresholds waterLevel (ccTempAvg climate) (ccPrecipAvg climate) (tcElevation terrain)

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
