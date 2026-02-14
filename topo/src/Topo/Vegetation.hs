{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Vegetation bootstrap and biome feedback.
--
-- __Bootstrap__ (pre-climate): estimates preliminary vegetation cover and
-- surface albedo so that land evapotranspiration has vegetation data to
-- work with.
--
-- __Biome feedback__ (post-biome-classification): re-derives vegetation
-- cover from the assigned biome type (e.g. forest -> high cover,
-- desert -> low cover) and blends with the bootstrap estimate.  This
-- feeds the next weather tick through albedo.
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
  -- * Pure per-tile helpers (exported for testing)
  , vegetationPotential
  , vegetationAlbedo
  , roughTemperatureEstimate
  ) where

import qualified Data.IntMap.Strict as IntMap
import Topo.Math (clamp01, iterateN)
import Topo.Pipeline (PipelineStage(..))
import Topo.Planet (PlanetConfig(..), WorldSlice(..), hexesPerDegreeLatitude)
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
    -- stage's land evapotranspiration to function.  Default: @0.15@.
  , vbcCoastalIterations :: !Int
    -- ^ Number of diffusion iterations for the bootstrap coastal
    -- proximity grid.  Higher values spread the influence further
    -- inland.  Default: @30@.
  , vbcCoastalDiffuse    :: !Float
    -- ^ Diffusion factor for coastal proximity (0–1).  Default: @0.6@.
  , vbcCoastalBoost      :: !Float
    -- ^ Maximum moisture boost from coastal proximity.  Tiles adjacent
    -- to ocean receive the full boost; it decays inland with diffusion.
    -- Default: @0.20@.
  } deriving (Eq, Show)

-- | Sensible Earth-like defaults.
defaultVegetationBootstrapConfig :: VegetationBootstrapConfig
defaultVegetationBootstrapConfig = VegetationBootstrapConfig
  { vbcTempMin        = 0.08
  , vbcTempRange      = 0.50
  , vbcFertilityBoost = 0.50
  , vbcAlbedoBase     = 0.15
  , vbcAlbedoBare     = 0.25
  , vbcAlbedoVeg      = 0.10
  , vbcOceanAlbedo    = 0.06
  , vbcIceAlbedo      = 0.80
  , vbcMinMoisture    = 0.15
  , vbcCoastalIterations = 30
  , vbcCoastalDiffuse    = 0.6
  , vbcCoastalBoost      = 0.20
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
    PipelineStage "bootstrapVegetation" "bootstrapVegetation" $ do
  logInfo "bootstrapVegetation: estimating cover + albedo"
  modifyWorldP $ \world ->
    let config = twConfig world
        planet = twPlanet world
        slice  = twSlice world
        terrainMap = twTerrain world
        wbMap      = twWaterBodies world
        -- Compute per-chunk coastal proximity from a global grid
        coastalSlices = bootstrapCoastalSlices cfg waterLevel config terrainMap
        vegMap = IntMap.mapWithKey
          (\k tc ->
            let coastalVec = case IntMap.lookup k coastalSlices of
                  Just v  -> v
                  Nothing -> U.replicate (chunkTileCount config) 0
            in deriveVegetationChunk cfg waterLevel config planet slice
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
  -> PlanetConfig
  -> WorldSlice
  -> Int              -- ^ chunk key
  -> TerrainChunk
  -> Maybe WaterBodyChunk
  -> U.Vector Float   -- ^ coastal proximity (0–1) per tile
  -> VegetationChunk
deriveVegetationChunk cfg wl wc planet slice key tc mbWb coastalVec =
  let elev      = tcElevation tc
      moisture  = tcMoisture tc
      soilDep   = tcSoilDepth tc
      fert      = tcFertility tc
      n         = U.length elev

      -- Per-tile latitude (degrees).  Each hex is ~13 mi, so latitude
      -- varies meaningfully across a chunk.
      hpd     = hexesPerDegreeLatitude planet
      cs      = wcChunkSize wc
      origin  = chunkOriginTile wc (chunkCoordFromId (ChunkId key))
      TileCoord _ox oy = origin
      centerY = cs `div` 2

      insol     = pcInsolation planet
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
            latDeg = wsLatCenter slice
                   + fromIntegral (gy - centerY) / hpd
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
      lapseRate = 0.65 :: Float
      lapse   = if h < wl then 0 else (h - wl) * lapseRate
  in clamp01 (latFac * insol - lapse)

-- | Vegetation potential from temperature, moisture, soil depth, and
-- fertility.
--
-- @V = clamp01 (tempFactor × moistFactor × soilFactor)@
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
  in clamp01 (tFac * mFac * sFac)

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
  } deriving (Eq, Show)

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
-- are stored in 'tcFlags') and before 'Topo.Weather.tickWeatherStage'
-- so the updated albedo feeds back into the weather snapshot.
updateVegetationFromBiomeStage
  :: BiomeFeedbackConfig
  -> VegetationBootstrapConfig
  -> PipelineStage
updateVegetationFromBiomeStage bfc vbc =
    PipelineStage "updateVegetationFromBiome" "updateVegetationFromBiome" $ do
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
      cover = U.generate n $ \i ->
        let bid = biomeIds U.! i
            p   = if i < U.length precip then precip U.! i else 0.5
            pMod = clamp01 (p / max 0.001 (biomeOptimalPrecip bid))
            biomeCov = biomeBaseCover bid * pMod
            oldCov   = case mbOld of
              Just old | U.length (vegCover old) > i -> vegCover old U.! i
              _                                      -> 0
        in clamp01 (w * biomeCov + (1 - w) * oldCov)
      albedo = U.generate n $ \i ->
        vegetationAlbedo vbc (cover U.! i)
      density = case mbOld of
        Just old | U.length (vegDensity old) == n -> vegDensity old
        _                                         -> U.replicate n 0
  in VegetationChunk
      { vegCover   = cover
      , vegAlbedo  = albedo
      , vegDensity = density
      }

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
  -- Tropical dry forest (seasonal leaf shedding)
  | bid == BiomeTropicalDryForest
  = 0.55
  -- Taiga / boreal
  | bid == BiomeTaiga
  = 0.55
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
  -- Tropical dry forest (adapted to seasonal drought)
  | bid == BiomeTropicalDryForest
  = 0.40
  -- Taiga / boreal
  | bid == BiomeTaiga
  = 0.35
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
            coastal = diffuseGridBootstrap gridW gridH
                        (vbcCoastalIterations cfg) (vbcCoastalDiffuse cfg)
                        oceanMask
        in IntMap.mapWithKey
             (\k _ -> chunkGridSlice config (ChunkCoord minCx minCy) gridW coastal k)
             terrain

-- | Simple 4-connected diffusion for bootstrap coastal proximity.
--
-- Same algorithm as the climate stage's 'coastalProximityGrid' but kept
-- private here to avoid cross-module coupling.  A future refactoring
-- should extract this to a shared module (see refactorings.md).
diffuseGridBootstrap :: Int -> Int -> Int -> Float -> U.Vector Float -> U.Vector Float
diffuseGridBootstrap gridW gridH iterations factor field =
  iterateN iterations (diffuseOnceBootstrap gridW gridH factor) field

diffuseOnceBootstrap :: Int -> Int -> Float -> U.Vector Float -> U.Vector Float
diffuseOnceBootstrap gridW gridH factor field =
  U.generate (gridW * gridH) $ \i ->
    let x = i `mod` gridW
        y = i `div` gridW
        c = field U.! i
        l = if x > 0          then field U.! (i - 1)     else c
        r = if x + 1 < gridW  then field U.! (i + 1)     else c
        u = if y > 0          then field U.! (i - gridW)  else c
        d = if y + 1 < gridH  then field U.! (i + gridW)  else c
        avg = (l + r + u + d + c) / 5
    in clamp01 (c * (1 - factor) + avg * factor)
