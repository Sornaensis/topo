{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Vegetation bootstrap and biome feedback.
--
-- __Bootstrap__ (pre-climate, stage 9): estimates preliminary vegetation
-- cover and surface albedo so that land evapotranspiration has vegetation
-- data to work with.  At this point 'Topo.WaterTable.applyWaterTableStage'
-- has not yet run, so root-zone moisture is unavailable and the bootstrap
-- uses the coarse 'tcFertility' from 'Topo.Soil'.
--
-- __Water table__ (stage 13.5): 'Topo.WaterTable.applyWaterTableStage'
-- overwrites 'tcFertility' with an improved formula incorporating
-- infiltration, water-table depth, and soil properties.
--
-- __Biome feedback__ (post-biome-classification, stage 15): re-derives
-- vegetation cover from the assigned biome type (e.g. forest -> high
-- cover, desert -> low cover) and blends with the bootstrap estimate.
-- The improved fertility from the water table stage feeds through
-- vegetation density automatically.  This feeds the next weather tick
-- through albedo.
module Topo.Vegetation
  ( VegetationBootstrapConfig(..)
  , defaultVegetationBootstrapConfig
  , bootstrapVegetationStage
  -- * Biome->vegetation feedback
  , BiomeFeedbackConfig(..)
  , defaultBiomeFeedbackConfig
  , updateVegetationFromBiomeStage
  , biomeBaseCover
  , biomeOptimalPrecip
  -- * Per-biome density lookups (used by vegetationDensityChunk)
  , biomeBaseDensity
  , biomeClimateSlope
  -- * Pure per-tile helpers (exported for testing)
  , vegetationPotential
  , vegetationAlbedo
  , roughTemperatureEstimate
  , updateVegChunk
  , densityEpsilon
  ) where

import qualified Data.IntMap.Strict as IntMap
import GHC.Generics (Generic)
import Topo.Config.JSON
  (ToJSON(..), FromJSON(..), configOptions, mergeDefaults,
   genericToJSON, genericParseJSON)
import Topo.Grid.Diffusion (coastalProximityGrid)
import Topo.Math (clamp01)
import Topo.Pipeline (PipelineStage(..))
import Topo.Pipeline.Stage (StageId(..))
import Topo.Planet (PlanetConfig(..), LatitudeMapping(..))
import Topo.Plugin (logInfo, modifyWorldP)
import Topo.TerrainGrid (buildElevationGrid, chunkCoordBounds, chunkGridSlice)
import Topo.Types
import Topo.World (TerrainWorld(..))
import qualified Data.Vector.Unboxed as U

---------------------------------------------------------------------------
-- Configuration
---------------------------------------------------------------------------

-- | Configuration for the vegetation bootstrap model.
data VegetationBootstrapConfig = VegetationBootstrapConfig
  { vbcTempMin        :: !Float
    -- ^ Minimum normalised temperature for any vegetation (default 0.08,
    -- ≈ −24 °C).
  , vbcTempRange      :: !Float
    -- ^ Temperature range over which vegetation scales from 0 → 1
    -- (default 0.50).
  , vbcFertilityBoost :: !Float
    -- ^ Multiplier on fertility in the soil factor (default 0.50).
  , vbcTempWeight     :: !Float
    -- ^ Weight of the temperature factor in the bootstrap potential
    -- blend.  Default: @0.40@.
  , vbcMoistWeight    :: !Float
    -- ^ Weight of the moisture factor in the bootstrap potential blend.
    -- Default: @0.35@.
  , vbcSoilWeight     :: !Float
    -- ^ Weight of the soil factor in the bootstrap potential blend.
    -- Default: @0.25@.
  , vbcAlbedoBase     :: !Float
    -- ^ Base surface albedo term (default 0.15).
  , vbcAlbedoBare     :: !Float
    -- ^ Albedo contribution of bare ground (default 0.25).
  , vbcAlbedoVeg      :: !Float
    -- ^ Albedo contribution of dense vegetation (default 0.10).
  , vbcOceanAlbedo    :: !Float
    -- ^ Fixed albedo for ocean tiles (default 0.06).
  , vbcIceAlbedo      :: !Float
    -- ^ Fixed albedo for ice-covered tiles (default 0.80).
  , vbcMinMoisture    :: !Float
    -- ^ Minimum moisture factor for the vegetation potential calculation.
    -- Interior land tiles far from rivers have near-zero 'tcMoisture'
    -- from flow accumulation; this floor ensures warm lowland tiles
    -- receive some vegetation cover, which in turn enables the climate
    -- stage's land evapotranspiration to function.  Default: @0.25@.
  , vbcCoastalIterations :: !Int
    -- ^ Number of diffusion iterations for the bootstrap coastal
    -- proximity grid.  Higher values spread the influence further
    -- inland.  Default: @30@.
  , vbcCoastalDiffuse    :: !Float
    -- ^ Diffusion factor for coastal proximity (0–1).  Default: @0.6@.
  , vbcCoastalBoost      :: !Float
    -- ^ Maximum moisture boost from coastal proximity.  Tiles adjacent
    -- to ocean receive the full boost; it decays inland with diffusion.
    -- Default: @0.30@.
  } deriving (Eq, Show, Generic)

instance ToJSON VegetationBootstrapConfig where
  toJSON = genericToJSON (configOptions "vbc")

instance FromJSON VegetationBootstrapConfig where
  parseJSON v = genericParseJSON (configOptions "vbc")
                  (mergeDefaults (toJSON defaultVegetationBootstrapConfig) v)

-- | Sensible Earth-like defaults.
defaultVegetationBootstrapConfig :: VegetationBootstrapConfig
defaultVegetationBootstrapConfig = VegetationBootstrapConfig
  { vbcTempMin        = 0.08
  , vbcTempRange      = 0.50
  , vbcFertilityBoost = 0.50
  , vbcTempWeight     = 0.40
  , vbcMoistWeight    = 0.35
  , vbcSoilWeight     = 0.25
  , vbcAlbedoBase     = 0.15
  , vbcAlbedoBare     = 0.25
  , vbcAlbedoVeg      = 0.10
  , vbcOceanAlbedo    = 0.06
  , vbcIceAlbedo      = 0.80
  , vbcMinMoisture    = 0.25
  , vbcCoastalIterations = 30
  , vbcCoastalDiffuse    = 0.6
  , vbcCoastalBoost      = 0.30
  }

---------------------------------------------------------------------------
-- Pipeline stage
---------------------------------------------------------------------------

-- | Compute preliminary vegetation cover and surface albedo.
--
-- Must run after 'Topo.Soil.applySoilStage' (provides soil depth /
-- fertility) and after 'Topo.WaterBody.applyWaterBodyStage' (provides
-- water body type).
--
-- Also computes a rough global coastal proximity grid so that tiles
-- near the coast receive a moisture boost, improving continental-interior
-- vegetation estimates (see GAP-V1 in plan.md).
bootstrapVegetationStage :: VegetationBootstrapConfig -> Float -> PipelineStage
bootstrapVegetationStage cfg waterLevel =
    PipelineStage StageVegetation "bootstrapVegetation" "bootstrapVegetation" Nothing [] Nothing $ do
  logInfo "bootstrapVegetation: estimating cover + albedo"
  modifyWorldP $ \world ->
    let config = twConfig world
        lm     = twLatMapping world
        terrainMap = twTerrain world
        wbMap      = twWaterBodies world
        -- Compute per-chunk coastal proximity from a global grid
        coastalSlices = bootstrapCoastalSlices cfg waterLevel config terrainMap
        vegMap = IntMap.mapWithKey
          (\k tc ->
            let coastalVec = case IntMap.lookup k coastalSlices of
                  Just v  -> v
                  Nothing -> U.replicate (chunkTileCount config) 0
            in deriveVegetationChunk cfg waterLevel config lm
                 k tc (IntMap.lookup k wbMap) coastalVec)
          terrainMap
    in world { twVegetation = vegMap }

---------------------------------------------------------------------------
-- Per-chunk derivation
---------------------------------------------------------------------------

deriveVegetationChunk
  :: VegetationBootstrapConfig
  -> Float            -- ^ water level
  -> WorldConfig
  -> LatitudeMapping
  -> Int              -- ^ chunk key
  -> TerrainChunk
  -> Maybe WaterBodyChunk
  -> U.Vector Float   -- ^ coastal proximity (0–1) per tile
  -> VegetationChunk
deriveVegetationChunk cfg wl wc lm key tc mbWb coastalVec =
  let elev      = tcElevation tc
      moisture  = tcMoisture tc
      soilDep   = tcSoilDepth tc
      fert      = tcFertility tc
      n         = U.length elev

      cs      = wcChunkSize wc
      origin  = chunkOriginTile wc (chunkCoordFromId (ChunkId key))
      TileCoord _ox oy = origin

      insol     = lmInsolation lm
      tiltExp   = 1.0 :: Float  -- latitude → temperature exponent

      cover = U.generate n $ \i ->
        let h  = elev U.! i
            m  = moisture U.! i
            sd = soilDep U.! i
            f  = fert U.! i
            -- Coastal proximity boost: tiles near ocean get extra
            -- moisture credit so continental interiors don't starve.
            cBoost = if i < U.length coastalVec
                       then coastalVec U.! i * vbcCoastalBoost cfg
                       else 0
            mBoosted = clamp01 (m + cBoost)
            TileCoord _lx ly = tileCoordFromIndex wc (TileIndex i)
            gy = oy + ly
            latDeg = fromIntegral gy * lmDegPerTile lm + lmBiasDeg lm
            isWater = case mbWb of
                        Nothing -> h < wl
                        Just wb -> let wt = wbType wb U.! i
                                   in waterBodyToCode wt /= 0
            tEst = roughTemperatureEstimate insol latDeg tiltExp wl h
        in if isWater
             then 0
             else vegetationPotential cfg tEst mBoosted sd f

      albedo = U.generate n $ \i ->
        let v  = cover U.! i
            h  = elev U.! i
            isOcean = case mbWb of
                        Nothing -> h < wl
                        Just wb -> let wt = wbType wb U.! i
                                   in waterBodyToCode wt == 1
        in if isOcean
             then vbcOceanAlbedo cfg
             else vegetationAlbedo cfg v

  in VegetationChunk
      { vegCover   = cover
      , vegAlbedo  = albedo
      , vegDensity = U.replicate n 0
      }

---------------------------------------------------------------------------
-- Pure helpers
---------------------------------------------------------------------------

-- | Rough temperature estimate from latitude and elevation.
--
-- @roughTemperatureEstimate insol latDeg tiltExp waterLevel elev@
--
-- Uses a cos(latitude) curve raised to @tiltExp@, modulated by insolation,
-- minus a lapse-rate penalty for elevation above the water surface.
-- No tectonic or plate corrections — this is intentionally coarse, since
-- the full temperature field is computed later by the climate stage.
{-# INLINE roughTemperatureEstimate #-}
roughTemperatureEstimate
  :: Float  -- ^ insolation (0.7–1.3)
  -> Float  -- ^ latitude of the tile (degrees, −90 to 90)
  -> Float  -- ^ latitude exponent for the temperature curve
  -> Float  -- ^ water level (normalised elevation threshold)
  -> Float  -- ^ tile elevation
  -> Float  -- ^ normalised temperature estimate ∈ [0, 1]
roughTemperatureEstimate insol latDeg tiltExp wl h =
  let latRad  = latDeg * (pi / 180)
      cosLat  = max 0 (cos latRad)
      latFac  = cosLat ** tiltExp
      lapseRate = 0.66 :: Float
      lapse   = if h < wl then 0 else (h - wl) * lapseRate
  in clamp01 (latFac * insol - lapse)

-- | Vegetation potential from temperature, moisture, soil depth, and
-- fertility.
--
-- Uses a weighted average of three factors rather than a pure product,
-- so that a single low factor cannot crush the result to near-zero:
--
-- @V = clamp01 (wTemp × tempFactor + wMoist × moistFactor + wSoil × soilFactor)@
--
-- The weights are normalised internally so they always sum to 1.
{-# INLINE vegetationPotential #-}
vegetationPotential
  :: VegetationBootstrapConfig
  -> Float  -- ^ normalised temperature estimate
  -> Float  -- ^ terrain moisture (0–1)
  -> Float  -- ^ soil depth (0–1)
  -> Float  -- ^ fertility (0–1)
  -> Float  -- ^ vegetation cover (0–1)
vegetationPotential cfg tEst m sd f =
  let tFac = clamp01 ((tEst - vbcTempMin cfg) / max 0.001 (vbcTempRange cfg))
      mFac = max (vbcMinMoisture cfg) m
      sFac = clamp01 (sd * (1 + vbcFertilityBoost cfg * f))
      -- Normalise weights so they sum to 1
      wt = max 0 (vbcTempWeight cfg)
      wm = max 0 (vbcMoistWeight cfg)
      ws = max 0 (vbcSoilWeight cfg)
      totalW = max 0.001 (wt + wm + ws)
  in clamp01 ((wt * tFac + wm * mFac + ws * sFac) / totalW)

-- | Surface albedo from vegetation cover.
--
-- Bare ground → higher albedo, dense vegetation → lower albedo.
-- @α = αBase + (αBare − αVeg) × (1 − vegCover)@
{-# INLINE vegetationAlbedo #-}
vegetationAlbedo
  :: VegetationBootstrapConfig
  -> Float  -- ^ vegetation cover (0–1)
  -> Float  -- ^ surface albedo (0–1)
vegetationAlbedo cfg v =
  clamp01 (vbcAlbedoBase cfg + (vbcAlbedoBare cfg - vbcAlbedoVeg cfg) * (1 - v))

---------------------------------------------------------------------------
-- Biome → vegetation feedback (Phase 7.2)
---------------------------------------------------------------------------

-- | Configuration for the biome-to-vegetation feedback stage.
--
-- After biome classification, this stage re-derives vegetation cover
-- from the assigned biome type and blends with the bootstrap estimate.
-- The result feeds back into subsequent weather ticks through the
-- vegetation albedo channel.
--
-- When 'bfcConvergenceIterations' > 0, the pipeline will re-run
-- climate → classify → feedback that many extra times, allowing
-- albedo-driven temperature changes to converge biome boundaries.
data BiomeFeedbackConfig = BiomeFeedbackConfig
  { bfcBlendWeight :: !Float
    -- ^ Blend factor in [0, 1].
    --   0 = keep bootstrap vegetation unchanged,
    --   1 = fully override with biome-derived cover.
    --   Default 0.85.
  , bfcConvergenceIterations :: !Int
    -- ^ Number of extra climate → classify → feedback cycles.
    -- Each iteration re-runs the climate stage with updated vegetation
    -- albedo, then re-classifies biomes and re-derives cover.
    -- Default: @1@.
  } deriving (Eq, Show, Generic)

instance ToJSON BiomeFeedbackConfig where
  toJSON = genericToJSON (configOptions "bfc")

instance FromJSON BiomeFeedbackConfig where
  parseJSON v = genericParseJSON (configOptions "bfc")
                  (mergeDefaults (toJSON defaultBiomeFeedbackConfig) v)

-- | Sensible default: 85 % biome-derived, 15 % bootstrap; 1 convergence pass.
defaultBiomeFeedbackConfig :: BiomeFeedbackConfig
defaultBiomeFeedbackConfig = BiomeFeedbackConfig
  { bfcBlendWeight = 0.85
  , bfcConvergenceIterations = 1
  }

-- | Re-derive vegetation cover from biome classification.
--
-- Climate-modulated: multiplies 'biomeBaseCover' by a precipitation
-- factor so that dry-margin biomes have less cover than wet-core ones  
-- (see GAP-V5 in plan.md).
--
-- Must run after 'Topo.BiomeConfig.classifyBiomesStage' (biome IDs
-- are stored in 'tcFlags') and before 'Topo.Weather.initWeatherStage'
-- so the updated albedo feeds back into the weather snapshot.
updateVegetationFromBiomeStage
  :: BiomeFeedbackConfig
  -> VegetationBootstrapConfig
  -> PipelineStage
updateVegetationFromBiomeStage bfc vbc =
    PipelineStage StageVegetationFeedback "updateVegetationFromBiome" "updateVegetationFromBiome" Nothing [] Nothing $ do
  logInfo "updateVegetationFromBiome: deriving vegetation from biome type"
  modifyWorldP $ \world ->
    let terrainMap = twTerrain world
        climateMap = twClimate world
        existing   = twVegetation world
        vegMap = IntMap.mapWithKey
          (\k tc ->
            let biomeIds = tcFlags tc
                precip = case IntMap.lookup k climateMap of
                  Just cc -> ccPrecipAvg cc
                  Nothing -> U.replicate (U.length biomeIds) 0.5
                old      = IntMap.lookup k existing
            in updateVegChunk bfc vbc biomeIds precip old
          ) terrainMap
    in world { twVegetation = vegMap }

-- | Update a single chunk's vegetation from biome IDs.
--
-- Climate-modulated: multiplies @biomeBaseCover(bid)@ by
-- @clamp01(precip / biomeOptimalPrecip(bid))@ so that tiles on the dry
-- edge of a biome receive proportionally less vegetation cover.
--
-- Density-modulated: the resulting cover is further scaled by
-- @0.5 + 0.5 * vegDensity@ and clamped to @vegDensity + densityEpsilon@,
-- enforcing the invariant that cover cannot exceed the productivity that
-- sustains it (see 'VegetationChunk' haddock).
--
-- Preserves the existing 'vegDensity' when present; falls back to zero
-- if no prior vegetation chunk exists.
updateVegChunk
  :: BiomeFeedbackConfig
  -> VegetationBootstrapConfig
  -> U.Vector BiomeId
  -> U.Vector Float    -- ^ precipitation per tile
  -> Maybe VegetationChunk
  -> VegetationChunk
updateVegChunk bfc vbc biomeIds precip mbOld =
  let n = U.length biomeIds
      w = clamp01 (bfcBlendWeight bfc)
      density = case mbOld of
        Just old | U.length (vegDensity old) == n -> vegDensity old
        _                                         -> U.replicate n 0
      cover = U.generate n $ \i ->
        let bid = biomeIds U.! i
            p   = if i < U.length precip then precip U.! i else 0.5
            pMod = clamp01 (p / max 0.001 (biomeOptimalPrecip bid))
            biomeCov = biomeBaseCover bid * pMod
            d = density U.! i
            -- Scale cover by density: d=0 → 50% of biomeCov, d=1 → 100%
            densityMod = 0.5 + 0.5 * d
            modCov = biomeCov * densityMod
            -- Enforce invariant: cover ≤ density + epsilon
            cappedCov = min modCov (d + densityEpsilon)
            oldCov   = case mbOld of
              Just old | U.length (vegCover old) > i -> vegCover old U.! i
              _                                      -> 0
        in clamp01 (w * cappedCov + (1 - w) * oldCov)
      albedo = U.generate n $ \i ->
        vegetationAlbedo vbc (cover U.! i)
  in VegetationChunk
      { vegCover   = cover
      , vegAlbedo  = albedo
      , vegDensity = density
      }

-- | Slack for the vegCover ≤ vegDensity + ε invariant.
--
-- Allows a small amount of cover beyond density to avoid clipping artefacts
-- at biome boundaries where density transitions are sharp.
densityEpsilon :: Float
densityEpsilon = 0.05

-- | Base vegetation cover expected for each biome family.
--
-- Dense forests return 0.75-0.92, deserts 0.02-0.05, grasslands 0.30-0.40.
-- Ocean and water biomes return 0.  Used by 'updateVegetationFromBiomeStage'.
biomeBaseCover :: BiomeId -> Float
biomeBaseCover bid
  -- Water biomes: no terrestrial vegetation
  | bid == BiomeOcean || bid == BiomeDeepOcean || bid == BiomeShallowSea
    || bid == BiomeCoralReef || bid == BiomeLake || bid == BiomeInlandSea
  = 0.00
  -- Tropical rainforest: densest terrestrial cover
  | bid == BiomeRainforest || bid == BiomeTropicalRainforest
  = 0.92
  -- Temperate / boreal forests
  | bid == BiomeForest || bid == BiomeTempDeciduousForest
    || bid == BiomeTempConiferousForest || bid == BiomeMontaneForest
    || bid == BiomeCloudForest || bid == BiomeTempRainforest
    || bid == BiomeBorealForest || bid == BiomeFloodplainForest
  = 0.78
  -- Tropical seasonal / monsoon forest (deciduous canopy, less dense)
  | bid == BiomeTropicalSeasonalForest
  = 0.62
  -- Tropical dry forest (seasonal leaf shedding)
  | bid == BiomeTropicalDryForest
  = 0.55
  -- Taiga / boreal
  | bid == BiomeTaiga
  = 0.55
  -- Oceanic boreal (wet maritime boreal coast)
  | bid == BiomeOceanicBoreal
  = 0.65
  -- Wetlands
  | bid == BiomeSwamp || bid == BiomeWetland || bid == BiomeMarsh
    || bid == BiomeFen || bid == BiomeBog || bid == BiomeBorealBog
    || bid == BiomeMangrove
  = 0.60
  -- Savanna
  | bid == BiomeSavanna || bid == BiomeWoodlandSavanna
    || bid == BiomeTropicalSavanna
  = 0.40
  -- Grasslands
  | bid == BiomeGrassland || bid == BiomePrairie
    || bid == BiomeFloodplainGrassland || bid == BiomeGrasslandSavanna
  = 0.35
  -- Coastal vegetated
  | bid == BiomeCoastal || bid == BiomeCoastalDunes
    || bid == BiomeSaltMarsh || bid == BiomeEstuary
    || bid == BiomeRockyShore
  = 0.25
  -- Shrubland / Mediterranean
  | bid == BiomeShrubland || bid == BiomeXericShrubland
    || bid == BiomeMediterranean || bid == BiomeMoorland
    || bid == BiomeCoastalScrub
  = 0.20
  -- Steppe
  | bid == BiomeSteppe
  = 0.15
  -- Alpine meadow
  | bid == BiomeAlpine || bid == BiomeAlpineMeadow
  = 0.10
  -- Tundra
  | bid == BiomeTundra || bid == BiomeArcticTundra
    || bid == BiomeAlpineTundra
  = 0.08
  -- Desert
  | bid == BiomeDesert || bid == BiomeHotDesert || bid == BiomeColdDesert
    || bid == BiomeRockyDesert || bid == BiomeSandDesert
    || bid == BiomeSaltFlat || bid == BiomePolarDesert
  = 0.03
  -- Fog desert (slightly more cover than barren desert)
  | bid == BiomeFogDesert
  = 0.06
  -- Volcanic
  | bid == BiomeLavaField || bid == BiomeVolcanicAshPlain
  = 0.02
  -- Snow / ice
  | bid == BiomeSnow || bid == BiomeIceCap || bid == BiomeGlacier
    || bid == BiomeSnowfield || bid == BiomeAlpineScree
  = 0.01
  -- Fallback
  | otherwise
  = 0.20

-- | Optimal precipitation for each biome family.
--
-- Used in climate-modulated biome feedback: the precipitation modifier
-- is @clamp01(precip / biomeOptimalPrecip(bid))@.  Biomes adapted to
-- arid conditions have low optimal precip (so even modest rain gives
-- them full cover), while moisture-loving biomes have high optimal
-- precip (so dry margins see reduced cover).
biomeOptimalPrecip :: BiomeId -> Float
biomeOptimalPrecip bid
  -- Water biomes: irrelevant (cover = 0 anyway)
  | bid == BiomeOcean || bid == BiomeDeepOcean || bid == BiomeShallowSea
    || bid == BiomeCoralReef || bid == BiomeLake || bid == BiomeInlandSea
  = 1.0
  -- Tropical rainforest: needs very high precipitation
  | bid == BiomeRainforest || bid == BiomeTropicalRainforest
  = 0.75
  -- Temperate / boreal forests
  | bid == BiomeForest || bid == BiomeTempDeciduousForest
    || bid == BiomeTempConiferousForest || bid == BiomeMontaneForest
    || bid == BiomeCloudForest || bid == BiomeTempRainforest
    || bid == BiomeBorealForest || bid == BiomeFloodplainForest
  = 0.50
  -- Tropical seasonal forest (adapted to moderate seasonal drought)
  | bid == BiomeTropicalSeasonalForest
  = 0.45
  -- Tropical dry forest (adapted to seasonal drought)
  | bid == BiomeTropicalDryForest
  = 0.40
  -- Taiga / boreal
  | bid == BiomeTaiga
  = 0.35
  -- Oceanic boreal (higher moisture needs)
  | bid == BiomeOceanicBoreal
  = 0.45
  -- Wetlands: high moisture requirement
  | bid == BiomeSwamp || bid == BiomeWetland || bid == BiomeMarsh
    || bid == BiomeFen || bid == BiomeBog || bid == BiomeBorealBog
    || bid == BiomeMangrove
  = 0.55
  -- Savanna
  | bid == BiomeSavanna || bid == BiomeWoodlandSavanna
    || bid == BiomeTropicalSavanna
  = 0.30
  -- Grasslands
  | bid == BiomeGrassland || bid == BiomePrairie
    || bid == BiomeFloodplainGrassland || bid == BiomeGrasslandSavanna
  = 0.30
  -- Coastal vegetated
  | bid == BiomeCoastal || bid == BiomeCoastalDunes
    || bid == BiomeSaltMarsh || bid == BiomeEstuary
    || bid == BiomeRockyShore
  = 0.30
  -- Shrubland / Mediterranean
  | bid == BiomeShrubland || bid == BiomeXericShrubland
    || bid == BiomeMediterranean || bid == BiomeMoorland
    || bid == BiomeCoastalScrub
  = 0.20
  -- Steppe
  | bid == BiomeSteppe
  = 0.15
  -- Alpine meadow
  | bid == BiomeAlpine || bid == BiomeAlpineMeadow
  = 0.20
  -- Tundra
  | bid == BiomeTundra || bid == BiomeArcticTundra
    || bid == BiomeAlpineTundra
  = 0.15
  -- Desert: even small amounts of rain are "enough" for desert cover
  | bid == BiomeDesert || bid == BiomeHotDesert || bid == BiomeColdDesert
    || bid == BiomeRockyDesert || bid == BiomeSandDesert
    || bid == BiomeSaltFlat || bid == BiomePolarDesert
  = 0.05
  -- Fog desert (fog moisture sustains slightly more vegetation)
  | bid == BiomeFogDesert
  = 0.08
  -- Volcanic
  | bid == BiomeLavaField || bid == BiomeVolcanicAshPlain
  = 0.10
  -- Snow / ice
  | bid == BiomeSnow || bid == BiomeIceCap || bid == BiomeGlacier
    || bid == BiomeSnowfield || bid == BiomeAlpineScree
  = 0.10
  -- Fallback
  | otherwise
  = 0.30

---------------------------------------------------------------------------
-- Per-biome density lookups (Phase 2)
---------------------------------------------------------------------------

-- | Intrinsic base vegetation density for each biome family.
--
-- This is the density a biome achieves in the absence of any climate
-- modulation (climate slope contributes on top).  Values are chosen to
-- match ecological expectations for each biome.
--
-- Used by 'Topo.Biome.vegetationDensityChunk'.
biomeBaseDensity :: BiomeId -> Float
biomeBaseDensity bid
  -- Water biomes: no terrestrial vegetation
  | bid == BiomeOcean || bid == BiomeDeepOcean || bid == BiomeShallowSea
    || bid == BiomeCoralReef || bid == BiomeLake || bid == BiomeInlandSea
  = 0.00
  -- Tropical rainforest: densest terrestrial vegetation
  | bid == BiomeRainforest || bid == BiomeTropicalRainforest
  = 0.45
  -- Temperate / boreal forests
  | bid == BiomeForest || bid == BiomeTempDeciduousForest
    || bid == BiomeTempConiferousForest || bid == BiomeMontaneForest
    || bid == BiomeCloudForest || bid == BiomeTempRainforest
    || bid == BiomeBorealForest || bid == BiomeFloodplainForest
  = 0.35
  -- Tropical seasonal forest
  | bid == BiomeTropicalSeasonalForest
  = 0.30
  -- Tropical dry forest
  | bid == BiomeTropicalDryForest
  = 0.25
  -- Taiga / boreal
  | bid == BiomeTaiga
  = 0.25
  -- Oceanic boreal
  | bid == BiomeOceanicBoreal
  = 0.30
  -- Wetlands
  | bid == BiomeSwamp || bid == BiomeWetland || bid == BiomeMarsh
    || bid == BiomeFen || bid == BiomeBog || bid == BiomeBorealBog
    || bid == BiomeMangrove
  = 0.28
  -- Savanna
  | bid == BiomeSavanna || bid == BiomeWoodlandSavanna
    || bid == BiomeTropicalSavanna
  = 0.18
  -- Grasslands
  | bid == BiomeGrassland || bid == BiomePrairie
    || bid == BiomeFloodplainGrassland || bid == BiomeGrasslandSavanna
  = 0.15
  -- Coastal vegetated
  | bid == BiomeCoastal || bid == BiomeCoastalDunes
    || bid == BiomeSaltMarsh || bid == BiomeEstuary
    || bid == BiomeRockyShore
  = 0.10
  -- Shrubland / Mediterranean
  | bid == BiomeShrubland || bid == BiomeXericShrubland
    || bid == BiomeMediterranean || bid == BiomeMoorland
    || bid == BiomeCoastalScrub
  = 0.10
  -- Steppe
  | bid == BiomeSteppe
  = 0.06
  -- Alpine meadow
  | bid == BiomeAlpine || bid == BiomeAlpineMeadow
  = 0.05
  -- Tundra
  | bid == BiomeTundra || bid == BiomeArcticTundra
    || bid == BiomeAlpineTundra
  = 0.03
  -- Desert
  | bid == BiomeDesert || bid == BiomeHotDesert || bid == BiomeColdDesert
    || bid == BiomeRockyDesert || bid == BiomeSandDesert
    || bid == BiomeSaltFlat || bid == BiomePolarDesert
  = 0.01
  -- Fog desert
  | bid == BiomeFogDesert
  = 0.02
  -- Volcanic
  | bid == BiomeLavaField || bid == BiomeVolcanicAshPlain
  = 0.01
  -- Snow / ice
  | bid == BiomeSnow || bid == BiomeIceCap || bid == BiomeGlacier
    || bid == BiomeSnowfield || bid == BiomeAlpineScree
  = 0.00
  -- Fallback
  | otherwise
  = 0.10

-- | Climate slope: how much climate suitability modulates density.
--
-- @density = biomeBaseDensity(bid) + biomeClimateSlope(bid) * climate@
--
-- High values for climate-sensitive biomes (forests), near-zero for
-- biomes whose density is dominated by non-climatic factors (deserts,
-- ice).
--
-- Used by 'Topo.Biome.vegetationDensityChunk'.
biomeClimateSlope :: BiomeId -> Float
biomeClimateSlope bid
  -- Water biomes: no modulation
  | bid == BiomeOcean || bid == BiomeDeepOcean || bid == BiomeShallowSea
    || bid == BiomeCoralReef || bid == BiomeLake || bid == BiomeInlandSea
  = 0.00
  -- Tropical rainforest: highly climate-responsive
  | bid == BiomeRainforest || bid == BiomeTropicalRainforest
  = 0.50
  -- Temperate / boreal forests
  | bid == BiomeForest || bid == BiomeTempDeciduousForest
    || bid == BiomeTempConiferousForest || bid == BiomeMontaneForest
    || bid == BiomeCloudForest || bid == BiomeTempRainforest
    || bid == BiomeBorealForest || bid == BiomeFloodplainForest
  = 0.45
  -- Tropical seasonal forest
  | bid == BiomeTropicalSeasonalForest
  = 0.40
  -- Tropical dry forest
  | bid == BiomeTropicalDryForest
  = 0.35
  -- Taiga / boreal
  | bid == BiomeTaiga
  = 0.35
  -- Oceanic boreal
  | bid == BiomeOceanicBoreal
  = 0.40
  -- Wetlands
  | bid == BiomeSwamp || bid == BiomeWetland || bid == BiomeMarsh
    || bid == BiomeFen || bid == BiomeBog || bid == BiomeBorealBog
    || bid == BiomeMangrove
  = 0.35
  -- Savanna
  | bid == BiomeSavanna || bid == BiomeWoodlandSavanna
    || bid == BiomeTropicalSavanna
  = 0.30
  -- Grasslands
  | bid == BiomeGrassland || bid == BiomePrairie
    || bid == BiomeFloodplainGrassland || bid == BiomeGrasslandSavanna
  = 0.25
  -- Coastal vegetated
  | bid == BiomeCoastal || bid == BiomeCoastalDunes
    || bid == BiomeSaltMarsh || bid == BiomeEstuary
    || bid == BiomeRockyShore
  = 0.18
  -- Shrubland / Mediterranean
  | bid == BiomeShrubland || bid == BiomeXericShrubland
    || bid == BiomeMediterranean || bid == BiomeMoorland
    || bid == BiomeCoastalScrub
  = 0.20
  -- Steppe
  | bid == BiomeSteppe
  = 0.12
  -- Alpine meadow
  | bid == BiomeAlpine || bid == BiomeAlpineMeadow
  = 0.08
  -- Tundra
  | bid == BiomeTundra || bid == BiomeArcticTundra
    || bid == BiomeAlpineTundra
  = 0.05
  -- Desert
  | bid == BiomeDesert || bid == BiomeHotDesert || bid == BiomeColdDesert
    || bid == BiomeRockyDesert || bid == BiomeSandDesert
    || bid == BiomeSaltFlat || bid == BiomePolarDesert
  = 0.03
  -- Fog desert
  | bid == BiomeFogDesert
  = 0.05
  -- Volcanic
  | bid == BiomeLavaField || bid == BiomeVolcanicAshPlain
  = 0.02
  -- Snow / ice
  | bid == BiomeSnow || bid == BiomeIceCap || bid == BiomeGlacier
    || bid == BiomeSnowfield || bid == BiomeAlpineScree
  = 0.01
  -- Fallback
  | otherwise
  = 0.15

---------------------------------------------------------------------------
-- Bootstrap coastal proximity
---------------------------------------------------------------------------

-- | Compute a per-chunk coastal proximity map from the global terrain.
--
-- Builds a global elevation grid, derives an ocean mask, runs a few
-- diffusion iterations, then slices the result back per chunk.
-- Returns an empty 'IntMap' when the terrain map is empty or has no
-- valid chunk bounds.
bootstrapCoastalSlices
  :: VegetationBootstrapConfig
  -> Float                     -- ^ water level
  -> WorldConfig
  -> IntMap.IntMap TerrainChunk
  -> IntMap.IntMap (U.Vector Float)
bootstrapCoastalSlices cfg waterLevel config terrain
  | IntMap.null terrain = IntMap.empty
  | otherwise =
    case chunkCoordBounds terrain of
      Nothing -> IntMap.empty
      Just (ChunkCoord minCx minCy, ChunkCoord maxCx maxCy) ->
        let size = wcChunkSize config
            gridW = (maxCx - minCx + 1) * size
            gridH = (maxCy - minCy + 1) * size
            elev = buildElevationGrid config terrain (ChunkCoord minCx minCy) gridW gridH
            oceanMask = U.map (\h -> if h < waterLevel then 1 else 0) elev
            coastal = coastalProximityGrid gridW gridH
                        (vbcCoastalIterations cfg) (vbcCoastalDiffuse cfg)
                        oceanMask
        in IntMap.mapWithKey
             (\k _ -> chunkGridSlice config (ChunkCoord minCx minCy) gridW coastal k)
             terrain
