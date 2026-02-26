{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Stable typed identifiers for pipeline stages.
--
-- Every built-in generation stage has a corresponding 'StageId'
-- constructor.  Plugin stages use @'StagePlugin' name@.  The
-- pseudo-stage 'StageConvergence' represents the collapsed
-- convergence iteration group (Climate → Biomes → VegFeedback × N).
--
-- External-facing APIs (plugin manifests, JSON) use kebab-case
-- canonical names via 'stageCanonicalName' / 'parseStageId'.
module Topo.Pipeline.Stage
  ( StageId(..)
  , stageCanonicalName
  , parseStageId
  , allBuiltinStageIds
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Generics (Generic)

-- | Stable identifiers for built-in pipeline stages.
data StageId
  = StagePlateTerrain
    -- ^ Plate-tectonic base terrain generation.
  | StageBaseHeight
    -- ^ Noise-driven base height (alternative to plate terrain).
  | StageTectonics
    -- ^ Standalone tectonic stage (when used independently of plate terrain).
  | StageErosion
    -- ^ Hydraulic and thermal erosion.
  | StageHypsometry
    -- ^ Hypsometric elevation redistribution.
  | StageVolcanism
    -- ^ Volcanic vent generation and eruption effects.
  | StageHydrology
    -- ^ Flow routing, moisture, and sediment transport.
  | StageRivers
    -- ^ River channel carving and routing.
  | StageWaterBody
    -- ^ Water body classification (ocean, lake, inland sea).
  | StageSoil
    -- ^ Soil fertility and depth derivation.
  | StageVegetation
    -- ^ Bootstrap vegetation density and albedo.
  | StageClimate
    -- ^ Climate model generation.
  | StageOceanCurrents
    -- ^ Ocean current SST modifications.
  | StageGlacier
    -- ^ Glacier accumulation, flow, and erosion.
  | StageParameters
    -- ^ Roughness, detail, and rock layer derivation.
  | StageWaterTable
    -- ^ Water table infiltration and root-zone moisture.
  | StageBiomes
    -- ^ Biome classification.
  | StageVegetationFeedback
    -- ^ Biome→vegetation feedback loop.
  | StageWeather
    -- ^ Weather tick (kept during migration; eventually overlay).
  | StageConvergence
    -- ^ Pseudo-stage for the convergence iteration group
    -- (Climate → Biomes → VegFeedback × N).
  | StagePlugin !Text
    -- ^ Dynamic plugin stage, keyed by plugin name.
  deriving (Eq, Ord, Show, Read, Generic)

-- | All built-in (non-plugin) stage identifiers, in canonical pipeline order.
--
-- Does not include 'StagePlugin' (dynamic) or 'StageBaseHeight'/'StageTectonics'
-- (alternative stage variants not used in the full pipeline).
allBuiltinStageIds :: [StageId]
allBuiltinStageIds =
  [ StagePlateTerrain
  , StageErosion
  , StageHypsometry
  , StageVolcanism
  , StageHydrology
  , StageRivers
  , StageWaterBody
  , StageSoil
  , StageVegetation
  , StageClimate
  , StageOceanCurrents
  , StageGlacier
  , StageParameters
  , StageWaterTable
  , StageBiomes
  , StageVegetationFeedback
  , StageConvergence
  , StageWeather
  ]

-- | Map from 'StageId' to its kebab-case canonical name.
--
-- These names are used in plugin manifests, JSON-facing APIs, and log
-- messages.  They are independent of Haskell constructor names.
--
-- >>> stageCanonicalName StagePlateTerrain
-- "plate-terrain"
-- >>> stageCanonicalName (StagePlugin "civilization")
-- "plugin:civilization"
stageCanonicalName :: StageId -> Text
stageCanonicalName sid = case sid of
  StagePlateTerrain      -> "plate-terrain"
  StageBaseHeight        -> "base-height"
  StageTectonics         -> "tectonics"
  StageErosion           -> "erosion"
  StageHypsometry        -> "hypsometry"
  StageVolcanism         -> "volcanism"
  StageHydrology         -> "hydrology"
  StageRivers            -> "rivers"
  StageWaterBody         -> "water-body"
  StageSoil              -> "soil"
  StageVegetation        -> "vegetation"
  StageClimate           -> "climate"
  StageOceanCurrents     -> "ocean-currents"
  StageGlacier           -> "glacier"
  StageParameters        -> "parameters"
  StageWaterTable        -> "water-table"
  StageBiomes            -> "biomes"
  StageVegetationFeedback -> "vegetation-feedback"
  StageWeather           -> "weather"
  StageConvergence       -> "convergence"
  StagePlugin name       -> "plugin:" <> name

-- | Reverse lookup: canonical name → 'StageId'.
--
-- Returns 'Nothing' for unrecognised names.  Plugin stage names must
-- be prefixed with @\"plugin:\"@.
--
-- >>> parseStageId "erosion"
-- Just StageErosion
-- >>> parseStageId "plugin:civilization"
-- Just (StagePlugin "civilization")
-- >>> parseStageId "nonsense"
-- Nothing
parseStageId :: Text -> Maybe StageId
parseStageId txt = case txt of
  "plate-terrain"        -> Just StagePlateTerrain
  "base-height"          -> Just StageBaseHeight
  "tectonics"            -> Just StageTectonics
  "erosion"              -> Just StageErosion
  "hypsometry"           -> Just StageHypsometry
  "volcanism"            -> Just StageVolcanism
  "hydrology"            -> Just StageHydrology
  "rivers"               -> Just StageRivers
  "water-body"           -> Just StageWaterBody
  "soil"                 -> Just StageSoil
  "vegetation"           -> Just StageVegetation
  "climate"              -> Just StageClimate
  "ocean-currents"       -> Just StageOceanCurrents
  "glacier"              -> Just StageGlacier
  "parameters"           -> Just StageParameters
  "water-table"          -> Just StageWaterTable
  "biomes"               -> Just StageBiomes
  "vegetation-feedback"  -> Just StageVegetationFeedback
  "weather"              -> Just StageWeather
  "convergence"          -> Just StageConvergence
  _                      ->
    case Text.stripPrefix "plugin:" txt of
      Just name | not (Text.null name) -> Just (StagePlugin name)
      _                                -> Nothing
