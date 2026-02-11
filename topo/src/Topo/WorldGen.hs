module Topo.WorldGen
  ( TerrainConfig(..)
  , PrecipConfig(..)
  , WorldGenConfig(..)
  , WorldGenConfigError(..)
  , defaultTerrainConfig
  , defaultPrecipConfig
  , defaultWorldGenConfig
  , aridWorldGenConfig
  , lushWorldGenConfig
  , mkWorldGenConfig
  , validateWorldGenConfig
  , buildPipelineConfig
  , buildFullPipelineConfig
  , buildBaseHeightPipelineConfig
  , autoOceanEdgeDepth
  ) where

import Data.Word (Word64)
import Topo.BaseHeight (GenConfig(..), OceanEdgeDepth(..), defaultGenConfig)
import Topo.BiomeConfig
  ( BiomeConfig(..)
  , aridBiomeConfig
  , classifyBiomesStage
  , defaultBiomeConfig
  , lushBiomeConfig
  )
import Topo.Climate (ClimateConfig(..), defaultClimateConfig, generateClimateStage)
import Topo.Erosion (ErosionConfig(..), applyErosionStage, defaultErosionConfig)
import Topo.Glacier (GlacierConfig(..), applyGlacierStage, defaultGlacierConfig)
import Topo.Gen (generateBaseHeightStage, generatePlateTerrainStage)
import Topo.Hydrology
  ( GroundwaterConfig(..)
  , HydroConfig(..)
  , RiverConfig(..)
  , applyHydrologyStage
  , applyRiverStage
  , defaultGroundwaterConfig
  , defaultHydroConfig
  , defaultRiverConfig
  )
import Topo.Volcanism (VolcanismConfig(..), applyVolcanismStage, defaultVolcanismConfig)
import Topo.Parameters (ParameterConfig(..), applyParameterLayersStage, defaultParameterConfig)
import Topo.Planet
  ( PlanetConfig(..)
  , WorldSlice(..)
  , defaultPlanetConfig
  , defaultWorldSlice
  , sliceToWorldExtent
  )
import Topo.Tectonics (TectonicsConfig(..), defaultTectonicsConfig, generateTectonicsStage)
import Topo.Weather (WeatherConfig(..), defaultWeatherConfig, tickWeatherStage)
import Topo.Pipeline (PipelineConfig(..))
import Topo.Types (WorldConfig(..), defaultWorldExtent)

data TerrainConfig = TerrainConfig
  { terrainGen :: !GenConfig
  , terrainTectonics :: !TectonicsConfig
  , terrainErosion :: !ErosionConfig
  , terrainHydrology :: !HydroConfig
  , terrainRivers :: !RiverConfig
  , terrainGroundwater :: !GroundwaterConfig
  , terrainVolcanism :: !VolcanismConfig
  , terrainGlacier :: !GlacierConfig
  , terrainParameters :: !ParameterConfig
  } deriving (Eq, Show)

data PrecipConfig = PrecipConfig
  { precipEvaporation :: !Float
  , precipRainShadow :: !Float
  , precipBoundary :: !Float
  , precipOrographic :: !Float
  , precipCoastal :: !Float
  } deriving (Eq, Show)

data WorldGenConfig = WorldGenConfig
  { worldTerrain :: !TerrainConfig
  , worldClimate :: !ClimateConfig
  , worldPrecip :: !PrecipConfig
  , worldBiome :: !BiomeConfig
  , worldWeather :: !WeatherConfig
  , worldPlanet :: !PlanetConfig
  , worldSlice :: !WorldSlice
  } deriving (Eq, Show)

data WorldGenConfigError
  = WorldGenNegativeHydraulicIterations !Int
  | WorldGenNegativeThermalIterations !Int
  | WorldGenNegativeWindIterations !Int
  | WorldGenNegativeMoistureIterations !Int
  | WorldGenNegativeCoastalIterations !Int
  | WorldGenNegativeBiomeSmoothing !Int
  | WorldGenNegativeGlacierIterations !Int
  deriving (Eq, Show)

defaultTerrainConfig :: TerrainConfig
defaultTerrainConfig = TerrainConfig
  { terrainGen = defaultGenConfig
  , terrainTectonics = defaultTectonicsConfig
  , terrainErosion = defaultErosionConfig
  , terrainHydrology = defaultHydroConfig
  , terrainRivers = defaultRiverConfig
  , terrainGroundwater = defaultGroundwaterConfig
  , terrainVolcanism = defaultVolcanismConfig
  , terrainGlacier = defaultGlacierConfig
  , terrainParameters = defaultParameterConfig
  }

defaultPrecipConfig :: PrecipConfig
defaultPrecipConfig = PrecipConfig
  { precipEvaporation = 1
  , precipRainShadow = 1
  , precipBoundary = 1
  , precipOrographic = 1
  , precipCoastal = 1
  }

defaultWorldGenConfig :: WorldGenConfig
defaultWorldGenConfig = WorldGenConfig
  { worldTerrain = defaultTerrainConfig
  , worldClimate = defaultClimateConfig
  , worldPrecip = defaultPrecipConfig
  , worldBiome = defaultBiomeConfig
  , worldWeather = defaultWeatherConfig
  , worldPlanet = defaultPlanetConfig
  , worldSlice = defaultWorldSlice
  }

aridWorldGenConfig :: WorldGenConfig
aridWorldGenConfig = defaultWorldGenConfig
  { worldBiome = aridBiomeConfig
  }

lushWorldGenConfig :: WorldGenConfig
lushWorldGenConfig = defaultWorldGenConfig
  { worldBiome = lushBiomeConfig
  }

-- | Construct a world generation config with validation.
mkWorldGenConfig
  :: TerrainConfig
  -> ClimateConfig
  -> PrecipConfig
  -> BiomeConfig
  -> WeatherConfig
  -> PlanetConfig
  -> WorldSlice
  -> Either WorldGenConfigError WorldGenConfig
mkWorldGenConfig terrain climate precip biome weather planet slice =
  validateWorldGenConfig WorldGenConfig
    { worldTerrain = terrain
    , worldClimate = climate
    , worldPrecip = precip
    , worldBiome = biome
    , worldWeather = weather
    , worldPlanet = planet
    , worldSlice = slice
    }

-- | Validate iteration counts for world generation configs.
validateWorldGenConfig :: WorldGenConfig -> Either WorldGenConfigError WorldGenConfig
validateWorldGenConfig cfg = do
  let terrain = worldTerrain cfg
      erosion = terrainErosion terrain
      climate = worldClimate cfg
      biome = worldBiome cfg
      glacier = terrainGlacier terrain
  checkNonNegative WorldGenNegativeHydraulicIterations (ecHydraulicIterations erosion)
  checkNonNegative WorldGenNegativeThermalIterations (ecThermalIterations erosion)
  checkNonNegative WorldGenNegativeWindIterations (ccWindIterations climate)
  checkNonNegative WorldGenNegativeMoistureIterations (ccMoistureIterations climate)
  checkNonNegative WorldGenNegativeCoastalIterations (ccCoastalIterations climate)
  checkNonNegative WorldGenNegativeBiomeSmoothing (bcSmoothingIterations biome)
  checkNonNegative WorldGenNegativeGlacierIterations (gcFlowIterations glacier)
  pure cfg
  where
    checkNonNegative mkErr value =
      if value < 0 then Left (mkErr value) else Right ()

-- | Build the default pipeline using plate-driven terrain only.
buildPipelineConfig :: WorldGenConfig -> Word64 -> PipelineConfig
buildPipelineConfig cfg seed =
  let terrain = worldTerrain cfg
  in PipelineConfig
      { pipelineSeed = seed
      , pipelineStages =
          [ generatePlateTerrainStage (terrainGen terrain) (terrainTectonics terrain)
          ]
      , pipelineSnapshots = False
      }

-- | Build the full terrain/climate/biome/weather pipeline.
--
-- Stage ordering matters:
--   * plate terrain must exist before erosion/hydrology.
--   * volcanism uses plate boundaries and can adjust terrain.
--   * hydrology derives moisture used by rivers, parameter layers, and climate.
--   * river routing depends on hydrology moisture and elevation.
--   * glaciers use climate temperature/precip and can adjust terrain.
--   * climate must precede biome classification and weather ticks.
--
-- Stage seeds are deterministically derived from the pipeline seed and
-- the stage tag, so all sub-generators remain repeatable.
--
-- Automatically derives 'gcWorldExtent' from the planet\/slice config via
-- 'sliceToWorldExtent'.  If the caller has explicitly set @gcWorldExtent@
-- to a non-default value it is preserved as a manual override.
--
-- Exposed slice edges that do not reach the planet boundary receive
-- automatic ocean-edge falloff (see 'autoOceanEdgeDepth').
buildFullPipelineConfig :: WorldGenConfig -> WorldConfig -> Word64 -> PipelineConfig
buildFullPipelineConfig cfg worldCfg seed =
  let terrain = worldTerrain cfg
      precip = worldPrecip cfg
      planet = worldPlanet cfg
      slice = worldSlice cfg
      -- Auto-derive world extent from slice unless manually overridden
      gen0 = terrainGen terrain
      derivedExtent = either (const defaultWorldExtent) id
                        (sliceToWorldExtent planet slice worldCfg)
      gen1 = if gcWorldExtent gen0 == defaultWorldExtent
                then gen0 { gcWorldExtent = derivedExtent }
                else gen0
      -- Auto-derive ocean edge depth for exposed slice edges
      gen2 = autoOceanEdgeDepth planet slice gen1
      terrain' = terrain { terrainGen = gen2 }
      climate = (worldClimate cfg)
        { ccEvaporation = ccEvaporation (worldClimate cfg) * precipEvaporation precip
        , ccRainShadow = ccRainShadow (worldClimate cfg) * precipRainShadow precip
        , ccBoundaryPrecipConvergent = ccBoundaryPrecipConvergent (worldClimate cfg) * precipBoundary precip
        , ccBoundaryPrecipDivergent = ccBoundaryPrecipDivergent (worldClimate cfg) * precipBoundary precip
        , ccBoundaryPrecipTransform = ccBoundaryPrecipTransform (worldClimate cfg) * precipBoundary precip
        , ccOrographicScale = ccOrographicScale (worldClimate cfg) * precipOrographic precip
        , ccCoastalMoistureBoost = ccCoastalMoistureBoost (worldClimate cfg) * precipCoastal precip
        , ccInsolation = pcInsolation planet
        }
  in PipelineConfig
      { pipelineSeed = seed
      , pipelineStages =
          [ generatePlateTerrainStage (terrainGen terrain') (terrainTectonics terrain')
          , applyErosionStage (terrainErosion terrain') (hcWaterLevel (terrainHydrology terrain'))
          , applyVolcanismStage (terrainVolcanism terrain')
          , applyHydrologyStage (terrainHydrology terrain')
          , applyRiverStage (terrainRivers terrain') (terrainGroundwater terrain')
          , applyParameterLayersStage (terrainParameters terrain')
          , generateClimateStage climate (hcWaterLevel (terrainHydrology terrain'))
          , applyGlacierStage (terrainGlacier terrain')
          , classifyBiomesStage (worldBiome cfg) (hcWaterLevel (terrainHydrology terrain'))
            , tickWeatherStage (worldWeather cfg)
          ]
      , pipelineSnapshots = False
      }

-- | Derive ocean edge depth from the slice boundaries.
--
-- When a slice edge does not reach the planet boundary (i.e. latitude
-- extent does not span pole-to-pole, or longitude extent < 360°),
-- a default edge-depth and falloff are injected on the exposed edges.
-- If the user has already configured edge depth (any field > 0),
-- the per-edge values are preserved.
--
-- Default auto-values: depth = 0.8, falloff = 64 tiles.
autoOceanEdgeDepth :: PlanetConfig -> WorldSlice -> GenConfig -> GenConfig
autoOceanEdgeDepth _planet slice gen
  | hasManualEdge = gen
  | otherwise     = gen { gcOceanEdgeDepth = computedEdge }
  where
    oed = gcOceanEdgeDepth gen
    hasManualEdge = oedNorth oed > 0 || oedSouth oed > 0
                 || oedEast oed > 0  || oedWest oed > 0
    autoDepth   = 0.8
    autoFalloff = 64.0
    latN = wsLatCenter slice + wsLatExtent slice / 2
    latS = wsLatCenter slice - wsLatExtent slice / 2
    computedEdge = OceanEdgeDepth
      { oedNorth   = if latN < 89  then autoDepth else 0
      , oedSouth   = if latS > (-89) then autoDepth else 0
      , oedEast    = if wsLonExtent slice < 359 then autoDepth else 0
      , oedWest    = if wsLonExtent slice < 359 then autoDepth else 0
      , oedFalloff = autoFalloff
      }

-- | Build a base-height-only pipeline for noise-driven terrain.
buildBaseHeightPipelineConfig :: GenConfig -> Word64 -> PipelineConfig
buildBaseHeightPipelineConfig gen seed =
  PipelineConfig
    { pipelineSeed = seed
    , pipelineStages = [generateBaseHeightStage gen]
    , pipelineSnapshots = False
    }
