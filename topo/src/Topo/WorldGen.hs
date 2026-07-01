{-# LANGUAGE DeriveGeneric #-}
module Topo.WorldGen
  ( TerrainConfig(..)
  , WorldGenConfig(..)
  , WorldGenConfigError(..)
  , defaultTerrainConfig
  , defaultWorldGenConfig
  , continentalWorldGenConfig
  , archipelagoWorldGenConfig
  , largeOceanWorldGenConfig
  , inlandSeaWorldGenConfig
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
import qualified Data.Set as Set
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
import Topo.Hypsometry (HypsometryConfig(..), applyHypsometryStage, defaultHypsometryConfig)
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
import Topo.River (RiverTopologyConfig(..), defaultRiverTopologyConfig)
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
import Topo.WaterTable (WaterTableConfig(..), applyWaterTableStage, defaultWaterTableConfig)
import Topo.OceanCurrent
  ( OceanCurrentConfig(..)
  , applyOceanCurrentsStage
  , defaultOceanCurrentConfig
  )
import Topo.Hex (HexGridMeta, defaultHexGridMeta)
import Topo.Planet
  ( PlanetConfig(..)
  , WorldSlice(..)
  , defaultPlanetConfig
  , defaultWorldSlice
  , sliceToWorldExtent
  )
import Topo.Tectonics (TectonicsConfig(..), defaultTectonicsConfig, generateTectonicsStage)
import Topo.Weather (WeatherConfig(..), defaultWeatherConfig, initWeatherStage)
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
  , terrainRiverTopology :: !RiverTopologyConfig
    -- ^ River segment extraction and pruning.
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
  , terrainHypsometry :: !HypsometryConfig
    -- ^ Hypsometric elevation redistribution.
  , terrainWaterTable :: !WaterTableConfig
    -- ^ Water table infiltration and root-zone moisture.
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
  , worldHexGrid :: !HexGridMeta
    -- ^ Hex grid physical size (flat-to-flat distance in km).
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
  , terrainRiverTopology = defaultRiverTopologyConfig
  , terrainGroundwater = defaultGroundwaterConfig
  , terrainVolcanism = defaultVolcanismConfig
  , terrainWaterBody = defaultWaterBodyConfig
  , terrainGlacier = defaultGlacierConfig
  , terrainParameters = defaultParameterConfig
  , terrainFormConfig = defaultTerrainFormConfig
  , terrainSoil = defaultSoilConfig
  , terrainVegetation = defaultVegetationBootstrapConfig
  , terrainHypsometry = defaultHypsometryConfig
  , terrainWaterTable = defaultWaterTableConfig
  }

defaultWorldGenConfig :: WorldGenConfig
defaultWorldGenConfig = continentalWorldGenConfig

-- | Balanced continental default with explicit soft ocean edges.
--
-- This is the library default terrain shape.  It deliberately configures
-- edge depth so local slices avoid the legacy implicit 0.5-depth moat while
-- still giving exposed slice boundaries ocean access.
continentalWorldGenConfig :: WorldGenConfig
continentalWorldGenConfig = continentalTerrainShape baseWorldGenConfig

-- | Fragmented island chains and volcanic archipelagos.
archipelagoWorldGenConfig :: WorldGenConfig
archipelagoWorldGenConfig = archipelagoTerrainShape baseWorldGenConfig

-- | Broad oceans with a few marginal landmasses and shelves.
largeOceanWorldGenConfig :: WorldGenConfig
largeOceanWorldGenConfig = largeOceanTerrainShape baseWorldGenConfig

-- | Endorheic basins with weak map-edge oceans and inland seas.
inlandSeaWorldGenConfig :: WorldGenConfig
inlandSeaWorldGenConfig = inlandSeaTerrainShape baseWorldGenConfig

baseWorldGenConfig :: WorldGenConfig
baseWorldGenConfig = WorldGenConfig
  { worldTerrain = defaultTerrainConfig
  , worldClimate = defaultClimateConfig
  , worldBiome = defaultBiomeConfig
  , worldWeather = defaultWeatherConfig
  , worldPlanet = defaultPlanetConfig
  , worldSlice = defaultWorldSlice
  , worldHexGrid = defaultHexGridMeta
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

continentalTerrainShape :: WorldGenConfig -> WorldGenConfig
continentalTerrainShape = updateTerrainShape $ \terrain ->
  terrain
    { terrainGen = (terrainGen terrain)
        { gcContinentScale = 0.010
        , gcLandRatio = 0.72
        , gcShelfWidth = 0.23
        , gcCoastSharpness = 1.05
        , gcOceanEdgeDepth = softEdge 0.10 40
        }
    , terrainTectonics = (terrainTectonics terrain)
        { tcPlateSize = 96
        , tcPlateHeightBase = 0.19
        , tcPlateHeightVariance = 0.36
        , tcCrustContinentalBias = 0.24
        , tcCrustOceanicBias = -0.22
        , tcPlateDetailStrength = 0.18
        , tcPlateRidgeStrength = 0.12
        }
    , terrainHydrology = (terrainHydrology terrain)
        { hcWaterLevel = 0.43
        , hcCoastalErodeStrength = 0.035
        }
    , terrainHypsometry = (terrainHypsometry terrain)
        { hpWaterLevel = 0.43
        , hpCoastalRampWidth = 0.10
        , hpCoastalRampStrength = 0.45
        }
    , terrainWaterBody = (terrainWaterBody terrain)
        { wbcOceanEdgeMargin = 2
        , wbcInlandSeaMinSize = 220
        , wbcMinLakeSize = 4
        , wbcMaxBasinDepth = 0.04
        }
    }

archipelagoTerrainShape :: WorldGenConfig -> WorldGenConfig
archipelagoTerrainShape = updateTerrainShape $ \terrain ->
  terrain
    { terrainGen = (terrainGen terrain)
        { gcContinentScale = 0.040
        , gcLandRatio = 0.56
        , gcShelfWidth = 0.15
        , gcCoastSharpness = 1.45
        , gcOceanEdgeDepth = softEdge 0.12 32
        }
    , terrainTectonics = (terrainTectonics terrain)
        { tcPlateSize = 48
        , tcPlateHeightBase = 0.15
        , tcPlateHeightVariance = 0.50
        , tcCrustContinentalBias = 0.24
        , tcCrustOceanicBias = -0.24
        , tcPlateDetailStrength = 0.26
        , tcPlateRidgeStrength = 0.18
        , tcRidgeHeight = 0.10
        }
    , terrainHydrology = (terrainHydrology terrain)
        { hcWaterLevel = 0.48
        , hcCoastalErodeStrength = 0.045
        }
    , terrainHypsometry = (terrainHypsometry terrain)
        { hpWaterLevel = 0.48
        , hpCoastalRampWidth = 0.07
        , hpCoastalRampStrength = 0.55
        }
    , terrainVolcanism = (terrainVolcanism terrain)
        { vcVentDensityBase = 0.07
        , vcDepositRaiseScale = 0.03
        }
    , terrainWaterBody = (terrainWaterBody terrain)
        { wbcOceanEdgeMargin = 2
        , wbcInlandSeaMinSize = 260
        , wbcMinLakeSize = 6
        , wbcMaxBasinDepth = 0.035
        }
    }

largeOceanTerrainShape :: WorldGenConfig -> WorldGenConfig
largeOceanTerrainShape = updateTerrainShape $ \terrain ->
  terrain
    { terrainGen = (terrainGen terrain)
        { gcContinentScale = 0.007
        , gcLandRatio = 0.66
        , gcShelfWidth = 0.28
        , gcCoastSharpness = 1.00
        , gcOceanEdgeDepth = OceanEdgeDepth
            { oedRMin = 0.18
            , oedRMax = 0.04
            , oedQMax = 0.14
            , oedQMin = 0.04
            , oedFalloff = 64
            }
        }
    , terrainTectonics = (terrainTectonics terrain)
        { tcPlateSize = 112
        , tcPlateHeightBase = 0.20
        , tcPlateHeightVariance = 0.36
        , tcCrustContinentalBias = 0.25
        , tcCrustOceanicBias = -0.22
        , tcPlateDetailStrength = 0.16
        , tcPlateRidgeStrength = 0.11
        }
    , terrainHydrology = (terrainHydrology terrain)
        { hcWaterLevel = 0.45
        , hcCoastalErodeStrength = 0.05
        }
    , terrainHypsometry = (terrainHypsometry terrain)
        { hpWaterLevel = 0.45
        , hpCoastalRampWidth = 0.13
        , hpCoastalRampStrength = 0.60
        }
    , terrainWaterBody = (terrainWaterBody terrain)
        { wbcOceanEdgeMargin = 2
        , wbcInlandSeaMinSize = 260
        , wbcMinLakeSize = 4
        , wbcMaxBasinDepth = 0.04
        }
    }

inlandSeaTerrainShape :: WorldGenConfig -> WorldGenConfig
inlandSeaTerrainShape = updateTerrainShape $ \terrain ->
  terrain
    { terrainGen = (terrainGen terrain)
        { gcContinentScale = 0.012
        , gcLandRatio = 0.82
        , gcShelfWidth = 0.29
        , gcCoastSharpness = 0.90
        , gcOceanEdgeDepth = softEdge 0.015 48
        }
    , terrainTectonics = (terrainTectonics terrain)
        { tcPlateSize = 88
        , tcPlateHeightBase = 0.24
        , tcPlateHeightVariance = 0.36
        , tcCrustContinentalBias = 0.26
        , tcCrustOceanicBias = -0.18
        , tcPlateDetailStrength = 0.22
        , tcPlateRidgeStrength = 0.13
        , tcRiftDepth = 0.12
        }
    , terrainHydrology = (terrainHydrology terrain)
        { hcWaterLevel = 0.51
        , hcSinkBreachDepth = 0.015
        , hcCoastalErodeStrength = 0.025
        }
    , terrainRivers = (terrainRivers terrain)
        { rcSinkBreachDepth = 0.015
        , rcMinLakeSize = 3
        }
    , terrainHypsometry = (terrainHypsometry terrain)
        { hpWaterLevel = 0.51
        , hpCoastalRampWidth = 0.06
        , hpCoastalRampStrength = 0.30
        }
    , terrainWaterBody = (terrainWaterBody terrain)
        { wbcOceanEdgeMargin = 1
        , wbcInlandSeaMinSize = 100
        , wbcMinLakeSize = 3
        , wbcMaxBasinDepth = 0.05
        }
    }

updateTerrainShape :: (TerrainConfig -> TerrainConfig) -> WorldGenConfig -> WorldGenConfig
updateTerrainShape f cfg = cfg { worldTerrain = f (worldTerrain cfg) }

softEdge :: Float -> Float -> OceanEdgeDepth
softEdge depth falloff = OceanEdgeDepth
  { oedRMin = depth
  , oedRMax = depth
  , oedQMax = depth
  , oedQMin = depth
  , oedFalloff = falloff
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
    , worldHexGrid = defaultHexGridMeta
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
      , pipelineDisabled = Set.empty
      , pipelineSnapshots = False
      , pipelineOnProgress = \_ -> pure ()
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
      hex = worldHexGrid cfg
      -- Auto-derive world extent from slice unless manually overridden
      gen0 = terrainGen terrain
      derivedExtent = either (const defaultWorldExtent) id
                        (sliceToWorldExtent planet hex slice worldCfg)
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
          , applyErosionStage (terrainGen terrain') (terrainErosion terrain') (terrainFormConfig terrain') wl
          , applyHypsometryStage (terrainHypsometry terrain')
          , applyVolcanismStage (terrainVolcanism terrain')
          , applyHydrologyStage (terrainHydrology terrain') (terrainFormConfig terrain')
          , applyRiverStage (terrainRivers terrain')
                            (terrainRiverTopology terrain')
                            (terrainGroundwater terrain') wl
          , applyWaterBodyStage (terrainWaterBody terrain') wl
          , applySoilStage (terrainSoil terrain')
          , bootstrapVegetationStage (terrainVegetation terrain') wl
          , generateClimateStage climate wCfg wl
          , applyOceanCurrentsStage (worldOceanCurrent cfg) wl
          , applyGlacierStage (terrainGlacier terrain') (terrainFormConfig terrain') wl
          , applyParameterLayersStage (terrainParameters terrain') (terrainFormConfig terrain') wl
          , applyWaterTableStage (terrainWaterTable terrain')
          , classifyBiomesStage (worldBiome cfg) wl
          , updateVegetationFromBiomeStage (worldBiomeFeedback cfg) (terrainVegetation terrain')
          ] ++ convergenceStages ++
          [ initWeatherStage (worldWeather cfg)
          ]
      , pipelineDisabled = Set.empty
      , pipelineSnapshots = False
      , pipelineOnProgress = \_ -> pure ()
      }

-- | Derive ocean edge depth from the slice boundaries.
--
-- When a slice edge does not reach the planet boundary (i.e. latitude
-- extent does not span pole-to-pole, or longitude extent < 360°),
-- a default edge-depth and falloff are injected on the exposed edges.
-- If the user has already configured edge depth (any field > 0),
-- the per-axial-boundary values are preserved.
--
-- Default auto-values: depth = 0.5, falloff = 16 tiles.
autoOceanEdgeDepth :: PlanetConfig -> WorldSlice -> GenConfig -> GenConfig
autoOceanEdgeDepth _planet slice gen
  | hasManualEdge = gen
  | otherwise     = gen { gcOceanEdgeDepth = computedEdge }
  where
    oed = gcOceanEdgeDepth gen
    hasManualEdge = oedRMin oed > 0 || oedRMax oed > 0
                 || oedQMax oed > 0 || oedQMin oed > 0
    autoDepth   = 0.5
    autoFalloff = 16.0
    latN = wsLatCenter slice + wsLatExtent slice / 2
    latS = wsLatCenter slice - wsLatExtent slice / 2
    computedEdge = OceanEdgeDepth
      { oedRMin    = if latN < 89  then autoDepth else 0
      , oedRMax    = if latS > (-89) then autoDepth else 0
      , oedQMax    = if wsLonExtent slice < 359 then autoDepth else 0
      , oedQMin    = if wsLonExtent slice < 359 then autoDepth else 0
      , oedFalloff = autoFalloff
      }

-- | Build a base-height-only pipeline for noise-driven terrain.
buildBaseHeightPipelineConfig :: GenConfig -> Word64 -> PipelineConfig
buildBaseHeightPipelineConfig gen seed =
  PipelineConfig
    { pipelineSeed = seed
    , pipelineStages = [generateBaseHeightStage gen]
    , pipelineDisabled = Set.empty
    , pipelineSnapshots = False
    , pipelineOnProgress = \_ -> pure ()
    }
