{-# LANGUAGE DeriveGeneric #-}
module Topo.WorldGen
  ( TerrainConfig(..)
  , WorldGenConfig(..)
  , WorldGenConfigError(..)
  , defaultTerrainConfig
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
import GHC.Generics (Generic)
import Topo.Config.JSON (configOptions, mergeDefaults, ToJSON(..), FromJSON(..), genericToJSON, genericParseJSON, Value)
import Topo.BaseHeight (GenConfig(..), OceanEdgeDepth(..), defaultGenConfig)
import Topo.BiomeConfig
  ( BiomeConfig(..)
  , aridBiomeConfig
  , classifyBiomesStage
  , defaultBiomeConfig
  , lushBiomeConfig
  )
import Topo.Climate
  ( ClimateConfig(..)
  , TemperatureConfig(..)
  , WindConfig(..)
  , MoistureConfig(..)
  , PrecipitationConfig(..)
  , BoundaryConfig(..)
  , SeasonalityConfig(..)
  , defaultClimateConfig
  , generateClimateStage
  )
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
import Topo.WaterBody (WaterBodyConfig(..), applyWaterBodyStage, defaultWaterBodyConfig)
import Topo.Parameters
  ( ParameterConfig(..), TerrainFormConfig(..), applyParameterLayersStage
  , defaultParameterConfig, defaultTerrainFormConfig
  )
import Topo.Soil (SoilConfig(..), applySoilStage, defaultSoilConfig)
import Topo.Vegetation
  ( VegetationBootstrapConfig(..), bootstrapVegetationStage
  , defaultVegetationBootstrapConfig
  , BiomeFeedbackConfig(..), defaultBiomeFeedbackConfig
  , updateVegetationFromBiomeStage
  )
import Topo.OceanCurrent
  ( OceanCurrentConfig(..)
  , applyOceanCurrentsStage
  , defaultOceanCurrentConfig
  )
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

-- | Bundle of all terrain-related sub-configs.
--
-- Held inside 'WorldGenConfig' as the 'worldTerrain' field.
-- Each sub-config controls one pipeline stage.
data TerrainConfig = TerrainConfig
  { terrainGen :: !GenConfig
    -- ^ Base height noise sampling.
  , terrainTectonics :: !TectonicsConfig
    -- ^ Plate tectonics generation.
  , terrainErosion :: !ErosionConfig
    -- ^ Hydraulic and thermal erosion.
  , terrainHydrology :: !HydroConfig
    -- ^ Flow routing, moisture, and sediment transport.
  , terrainRivers :: !RiverConfig
    -- ^ River channel carving and routing.
  , terrainGroundwater :: !GroundwaterConfig
    -- ^ Groundwater recharge, storage, and discharge.
  , terrainVolcanism :: !VolcanismConfig
    -- ^ Volcanic vent generation and eruption effects.
  , terrainWaterBody :: !WaterBodyConfig
    -- ^ Water body classification (ocean, lake, inland sea).
  , terrainGlacier :: !GlacierConfig
    -- ^ Glacier accumulation, flow, and erosion.
  , terrainParameters :: !ParameterConfig
    -- ^ Roughness, detail, and rock layer derivation.
  , terrainFormConfig :: !TerrainFormConfig
    -- ^ Terrain form classification thresholds.
  , terrainSoil :: !SoilConfig
    -- ^ Soil fertility and depth derivation.
  , terrainVegetation :: !VegetationBootstrapConfig
    -- ^ Bootstrap vegetation density and albedo.
  } deriving (Eq, Show, Generic)

-- | Serialise with @terrain@ prefix stripped from field names.
instance ToJSON TerrainConfig where
  toJSON = genericToJSON (configOptions "terrain")

-- | Deserialise with defaults for any missing field.
instance FromJSON TerrainConfig where
  parseJSON v = genericParseJSON (configOptions "terrain") (mergeDefaults (toJSON defaultTerrainConfig) v)

-- | Top-level world generation configuration.
--
-- Passed to 'buildFullPipelineConfig' to assemble the complete
-- generation pipeline.  All pipeline stages ultimately receive their
-- parameters from sub-fields of this type.
data WorldGenConfig = WorldGenConfig
  { worldTerrain :: !TerrainConfig
    -- ^ All terrain-stage sub-configs.
  , worldClimate :: !ClimateConfig
    -- ^ Climate model (temperature, wind, moisture, precipitation,
    -- boundary effects, seasonality).
  , worldBiome :: !BiomeConfig
    -- ^ Biome classification rules and refinement.
  , worldWeather :: !WeatherConfig
    -- ^ Weather tick configuration.
  , worldPlanet :: !PlanetConfig
    -- ^ Planetary parameters (radius, axial tilt, insolation).
  , worldSlice :: !WorldSlice
    -- ^ Geographic window (lat\/lon center and extent).
  , worldOceanCurrent :: !OceanCurrentConfig
    -- ^ Ocean current SST modifications.
  , worldBiomeFeedback :: !BiomeFeedbackConfig
    -- ^ Biome→vegetation feedback loop parameters.
  } deriving (Eq, Show, Generic)

-- | Serialise with @world@ prefix stripped from field names.
instance ToJSON WorldGenConfig where
  toJSON = genericToJSON (configOptions "world")

-- | Deserialise with defaults for any missing field.
instance FromJSON WorldGenConfig where
  parseJSON v = genericParseJSON (configOptions "world") (mergeDefaults (toJSON defaultWorldGenConfig) v)

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
  , terrainWaterBody = defaultWaterBodyConfig
  , terrainGlacier = defaultGlacierConfig
  , terrainParameters = defaultParameterConfig
  , terrainFormConfig = defaultTerrainFormConfig
  , terrainSoil = defaultSoilConfig
  , terrainVegetation = defaultVegetationBootstrapConfig
  }

defaultWorldGenConfig :: WorldGenConfig
defaultWorldGenConfig = WorldGenConfig
  { worldTerrain = defaultTerrainConfig
  , worldClimate = defaultClimateConfig
  , worldBiome = defaultBiomeConfig
  , worldWeather = defaultWeatherConfig
  , worldPlanet = defaultPlanetConfig
  , worldSlice = defaultWorldSlice
  , worldOceanCurrent = defaultOceanCurrentConfig
  , worldBiomeFeedback = defaultBiomeFeedbackConfig
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
  -> BiomeConfig
  -> WeatherConfig
  -> PlanetConfig
  -> WorldSlice
  -> Either WorldGenConfigError WorldGenConfig
mkWorldGenConfig terrain climate biome weather planet slice =
  validateWorldGenConfig WorldGenConfig
    { worldTerrain = terrain
    , worldClimate = climate
    , worldBiome = biome
    , worldWeather = weather
    , worldPlanet = planet
    , worldSlice = slice
    , worldOceanCurrent = defaultOceanCurrentConfig
    , worldBiomeFeedback = defaultBiomeFeedbackConfig
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
  checkNonNegative WorldGenNegativeWindIterations (windIterations (ccWind climate))
  checkNonNegative WorldGenNegativeMoistureIterations (moistIterations (ccMoisture climate))
  checkNonNegative WorldGenNegativeCoastalIterations (precCoastalIterations (ccPrecipitation climate))
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
--   * hydrology derives moisture used by rivers and climate.
--   * river routing depends on hydrology moisture and elevation.
--   * climate must precede glaciers (glaciers use temperature/precip).
--   * glaciers can modify elevation, so terrain-shape parameters
--     (slope, curvature, relief, ruggedness, terrain form) run after
--     glaciers to reflect the final surface.
--   * parameter layers must precede biome classification (biomes use
--     slope, terrain form, etc.).
--   * biome classification must precede weather ticks.
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
      climate = worldClimate cfg
      wCfg = worldWeather cfg
      wl = hcWaterLevel (terrainHydrology terrain')
      -- Convergence: extra climate → classify → feedback cycles
      convergenceIters = max 0 (bfcConvergenceIterations (worldBiomeFeedback cfg))
      convergenceStages = concat $ replicate convergenceIters
        [ generateClimateStage climate wCfg wl
        , classifyBiomesStage (worldBiome cfg) wl
        , updateVegetationFromBiomeStage (worldBiomeFeedback cfg) (terrainVegetation terrain')
        ]
  in PipelineConfig
      { pipelineSeed = seed
      , pipelineStages =
          [ generatePlateTerrainStage (terrainGen terrain') (terrainTectonics terrain')
          , applyErosionStage (terrainErosion terrain') wl
          , applyVolcanismStage (terrainVolcanism terrain')
          , applyHydrologyStage (terrainHydrology terrain')
          , applyRiverStage (terrainRivers terrain')
                            (terrainGroundwater terrain') wl
          , applyWaterBodyStage (terrainWaterBody terrain') wl
          , applySoilStage (terrainSoil terrain')
          , bootstrapVegetationStage (terrainVegetation terrain') wl
          , generateClimateStage climate wCfg wl
          , applyOceanCurrentsStage (worldOceanCurrent cfg) wl
          , applyGlacierStage (terrainGlacier terrain')
          , applyParameterLayersStage (terrainParameters terrain') (terrainFormConfig terrain')
          , classifyBiomesStage (worldBiome cfg) wl
          , updateVegetationFromBiomeStage (worldBiomeFeedback cfg) (terrainVegetation terrain')
          ] ++ convergenceStages ++
          [ tickWeatherStage (worldWeather cfg)
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
