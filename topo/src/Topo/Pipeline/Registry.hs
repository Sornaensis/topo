{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Registry-backed metadata for generation pipeline stages.
--
-- The registry is the source of truth for user-facing stage docs and
-- diagnostics surfaces.  Runtime dependencies still live in
-- 'Topo.Pipeline.Dep'; 'stageDocDependencies' joins the registry row to that
-- graph so tests can catch documentation/closure drift.
module Topo.Pipeline.Registry
  ( PipelineStageDoc(..)
  , builtinStageDocs
  , allStageDocs
  , stageDocFor
  , stageDocDependencies
  , stageDocsMarkdown
  ) where

import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text

import Topo.Pipeline.Dep (StageDep(..), builtinDependencies)
import Topo.Pipeline.Stage (StageId(..), allBuiltinStageIds, stageCanonicalName)

-- | Static, registry-owned documentation for a pipeline stage.
data PipelineStageDoc = PipelineStageDoc
  { psdStageId :: !StageId
    -- ^ Stable stage identifier.
  , psdTitle :: !Text
    -- ^ Human-readable title used by diagnostics surfaces.
  , psdSummary :: !Text
    -- ^ Concise stage purpose.
  , psdStageSeedTag :: !Text
    -- ^ Seed derivation tag used by the concrete stage implementation.
  , psdConfigKeys :: ![Text]
    -- ^ Config roots or parameters that feed this stage.
  , psdOutputFields :: ![Text]
    -- ^ World/terrain/inspector fields produced or refreshed by this stage.
  , psdOutputOverlays :: ![Text]
    -- ^ Overlay names or overlay families produced by this stage.
  , psdDiagnosticHints :: ![Text]
    -- ^ User-facing diagnostics hints for common issues.
  } deriving (Eq, Show)

-- | Registry docs for canonical full-pipeline built-in stages in UI order.
builtinStageDocs :: [PipelineStageDoc]
builtinStageDocs = map docForBuiltin allBuiltinStageIds

-- | Registry docs for every built-in stage variant, including alternatives that
-- are not present in the full pipeline by default.
allStageDocs :: [PipelineStageDoc]
allStageDocs = alternativeStageDocs <> builtinStageDocs

-- | Look up a registry doc by stage id.
stageDocFor :: StageId -> Maybe PipelineStageDoc
stageDocFor sid = listToMaybe [doc | doc <- allStageDocs, psdStageId doc == sid]

-- | Direct dependency docs joined from the canonical dependency graph.
stageDocDependencies :: PipelineStageDoc -> [StageId]
stageDocDependencies doc = fromMaybe [] $ do
  StageDep _ requires <- listToMaybe
    [ dep | dep <- builtinDependencies, sdStage dep == psdStageId doc ]
  pure requires

-- | Generate markdown stage documentation from the registry.
stageDocsMarkdown :: [PipelineStageDoc] -> Text
stageDocsMarkdown docs = Text.intercalate "\n\n" (map renderDoc docs)
  where
    renderDoc doc = Text.unlines
      [ "## " <> stageCanonicalName (psdStageId doc) <> " — " <> psdTitle doc
      , psdSummary doc
      , "Dependencies: " <> namesOrNone (map stageCanonicalName (stageDocDependencies doc))
      , "Config: " <> namesOrNone (psdConfigKeys doc)
      , "Outputs: " <> namesOrNone (psdOutputFields doc <> psdOutputOverlays doc)
      , "Diagnostics: " <> namesOrNone (psdDiagnosticHints doc)
      ]

alternativeStageDocs :: [PipelineStageDoc]
alternativeStageDocs =
  [ PipelineStageDoc
      { psdStageId = StageBaseHeight
      , psdTitle = "Base height terrain"
      , psdSummary = "Noise-driven base terrain generator used by small or legacy pipelines."
      , psdStageSeedTag = "generateBaseHeight"
      , psdConfigKeys = ["terrain.gen", "world.extent", "seed"]
      , psdOutputFields = ["terrain.elevation", "terrain.moisture", "terrain.hardness"]
      , psdOutputOverlays = []
      , psdDiagnosticHints = ["Alternative to plate-terrain; not part of the default full pipeline."]
      }
  , PipelineStageDoc
      { psdStageId = StageTectonics
      , psdTitle = "Tectonics"
      , psdSummary = "Standalone tectonic plate metadata generation when used outside plate terrain."
      , psdStageSeedTag = "generateTectonics"
      , psdConfigKeys = ["terrain.tectonics", "seed"]
      , psdOutputFields = ["terrain.plate_id", "terrain.plate_boundary", "terrain.plate_crust", "terrain.plate_velocity"]
      , psdOutputOverlays = []
      , psdDiagnosticHints = ["Alternative tectonics stage; default full pipeline folds this into plate-terrain."]
      }
  ]

-- Keep the per-stage payloads close together so generated docs and the
-- /pipeline route expose the same wording.
docForBuiltin :: StageId -> PipelineStageDoc
docForBuiltin sid = case sid of
  StagePlateTerrain -> doc sid "Plate terrain" "Generates plate-aware base terrain, crust, and plate metadata." "generatePlateTerrain"
    ["terrain.gen", "terrain.tectonics", "planet", "world.slice"]
    ["terrain.elevation", "terrain.hardness", "terrain.plate_id", "terrain.plate_boundary", "terrain.plate_crust", "terrain.plate_height", "terrain.plate_age"]
    []
    ["Root generation stage; disabling it disables most downstream stages."]
  StageErosion -> doc sid "Erosion" "Applies hydraulic, thermal, wind, and coastal erosion to terrain." "applyErosion"
    ["terrain.erosion", "terrain.gen", "terrain.form", "water_level"]
    ["terrain.elevation", "terrain.moisture", "terrain.sediment", "terrain.hardness"]
    []
    ["Requires plate terrain; high iteration counts increase generation time."]
  StageHypsometry -> doc sid "Hypsometry" "Redistributes elevations into configured land/ocean hypsometric profiles." "applyHypsometry"
    ["terrain.hypsometry"]
    ["terrain.elevation", "hypsometry"]
    []
    ["Runs before water and climate classification so elevation bands stay consistent."]
  StageVolcanism -> doc sid "Volcanism" "Adds volcanic vents, lava, ash, and terrain adjustments around tectonic boundaries." "applyVolcanism"
    ["terrain.volcanism"]
    ["volcanism", "terrain.elevation", "terrain.fertility", "terrain.rock_type"]
    []
    ["Depends on plate metadata from plate-terrain."]
  StageHydrology -> doc sid "Hydrology" "Computes flow routing, moisture, groundwater, and hydrologic terrain metrics." "applyHydrology"
    ["terrain.hydrology", "terrain.form"]
    ["hydrology.flow", "terrain.moisture", "groundwater", "terrain.slope"]
    []
    ["Required by rivers, water bodies, and water-table stages."]
  StageRivers -> doc sid "Rivers" "Routes and carves river networks from hydrology and groundwater inputs." "applyRivers"
    ["terrain.rivers", "terrain.river_topology", "terrain.groundwater", "water_level"]
    ["rivers", "terrain.elevation", "terrain.moisture"]
    []
    ["Requires hydrology; disabled hydrology auto-disables river output."]
  StageWaterBody -> doc sid "Water bodies" "Classifies oceans, lakes, inland seas, and water-body connectivity." "applyWaterBodies"
    ["terrain.water_body", "water_level"]
    ["water_body", "water_bodies", "terrain.water_body_type"]
    []
    ["Requires terrain and hydrology so basin classification is stable."]
  StageSoil -> doc sid "Soil" "Derives soil depth, grain, type, and fertility from terrain and moisture." "applySoil"
    ["terrain.soil"]
    ["soil", "terrain.soil_depth", "terrain.soil_grain", "terrain.soil_type", "terrain.fertility"]
    []
    ["Feeds vegetation bootstrap and biome classification."]
  StageVegetation -> doc sid "Vegetation" "Bootstraps vegetation density and albedo before climate and biomes." "bootstrapVegetation"
    ["terrain.vegetation", "water_level"]
    ["vegetation", "terrain.vegetation_density", "terrain.albedo"]
    []
    ["Requires soil; climate uses this initial vegetation signal."]
  StageClimate -> doc sid "Climate" "Generates temperature, precipitation, winds, and climate diagnostics." "generateClimate"
    ["world.climate", "world.weather", "water_level"]
    ["climate.temperature", "climate.precipitation", "climate.wind", "climate_diagnostics"]
    []
    ["Downstream ocean currents, glaciers, biomes, convergence, and weather depend on climate."]
  StageOceanCurrents -> doc sid "Ocean currents" "Derives ocean current and sea-surface temperature modifiers." "applyOceanCurrents"
    ["world.ocean_current", "water_level"]
    ["ocean_currents", "climate.sea_surface_temperature"]
    []
    ["Requires climate so temperature gradients are available."]
  StageGlacier -> doc sid "Glaciers" "Computes glacier accumulation, flow, erosion, and snow/ice diagnostics." "applyGlaciers"
    ["terrain.glacier", "terrain.form", "water_level"]
    ["glacier", "glacier_snow_ice", "terrain.elevation"]
    []
    ["Requires climate and terrain; glaciers may adjust final elevation."]
  StageParameters -> doc sid "Parameter layers" "Refreshes derived terrain parameters and terrain-form metrics after shape changes." "applyParameterLayers"
    ["terrain.parameters", "terrain.form", "water_level"]
    ["terrain.roughness", "terrain.rock_density", "terrain_form", "terrain_form_metrics", "terrain.relief"]
    []
    ["Runs after glaciers so slope, relief, and terrain forms reflect final terrain."]
  StageWaterTable -> doc sid "Water table" "Computes infiltration, water table depth, and root-zone moisture." "applyWaterTable"
    ["terrain.water_table"]
    ["water_table", "terrain.root_moisture"]
    []
    ["Requires hydrology and terrain moisture."]
  StageBiomes -> doc sid "Biomes" "Classifies biomes from climate, vegetation, terrain, and water signals." "classifyBiomes"
    ["world.biome", "water_level"]
    ["biome", "biome_refinement", "terrain.biome_code"]
    []
    ["Requires climate and vegetation; feeds vegetation feedback and weather."]
  StageVegetationFeedback -> doc sid "Vegetation feedback" "Applies biome-to-vegetation feedback and albedo corrections." "updateVegetationFromBiome"
    ["world.biome_feedback", "terrain.vegetation"]
    ["vegetation", "terrain.vegetation_density", "terrain.albedo"]
    []
    ["Requires biome classification; repeated by convergence iterations when configured."]
  StageConvergence -> doc sid "Climate/biome convergence" "Pseudo-stage documenting repeated climate, biome, and vegetation-feedback cycles." "convergence"
    ["world.biome_feedback.convergence_iterations"]
    ["climate", "biome", "vegetation"]
    []
    ["Pseudo-stage for diagnostics; concrete runs appear as repeated climate/biome/feedback stages."]
  StageWeather -> doc sid "Weather" "Initializes weather and generated-normal overlays for the generated world." "initWeather"
    ["world.weather"]
    ["weather", "weather_normals", "weather_snapshot", "weather_timeline"]
    ["weather", "weather_normals"]
    ["Requires climate and biomes so weather starts from classified terrain."]
  StageBaseHeight -> fromMaybe (fallbackDoc sid) (stageDocFor StageBaseHeight)
  StageTectonics -> fromMaybe (fallbackDoc sid) (stageDocFor StageTectonics)
  StagePlugin name -> doc sid ("Plugin " <> name) "Plugin generator stage loaded from a manifest." ("plugin:" <> name)
    ["plugin.config", "plugin.manifest.generator"]
    ["plugin.terrain", "plugin.overlay"]
    []
    ["Plugin stages are documented from manifests at runtime."]

fallbackDoc :: StageId -> PipelineStageDoc
fallbackDoc sid = doc sid (stageCanonicalName sid) "Pipeline stage." (stageCanonicalName sid) [] [] [] []

doc :: StageId -> Text -> Text -> Text -> [Text] -> [Text] -> [Text] -> [Text] -> PipelineStageDoc
doc sid title summary seedTag configKeys outputs overlays hints = PipelineStageDoc
  { psdStageId = sid
  , psdTitle = title
  , psdSummary = summary
  , psdStageSeedTag = seedTag
  , psdConfigKeys = configKeys
  , psdOutputFields = outputs
  , psdOutputOverlays = overlays
  , psdDiagnosticHints = hints
  }

namesOrNone :: [Text] -> Text
namesOrNone [] = "none"
namesOrNone names = Text.intercalate ", " names
