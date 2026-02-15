{-# LANGUAGE DeriveGeneric #-}

-- | Primary biome classification and vegetation density.
--
-- Uses a Whittaker-style temperature × precipitation lookup with
-- temperature-primary mountain biome guards, followed by nearest-centroid
-- fallback for tiles that fall between rule rectangles.
module Topo.Biome
  ( BiomeRule(..)
  , BiomeMap
  , BiomeThresholds(..)
  , defaultBiomeRules
  , defaultBiomeThresholds
  , classifyBiome
  , classifyBiomesChunk
  , constrainSmoothedBiomes
  , smoothBiomesChunk
  , smoothBiomesGrid
  , smoothMountainTransitions
  , BiomeVegetationConfig(..)
  , defaultBiomeVegetationConfig
  , vegetationDensityChunk
  ) where

import GHC.Generics (Generic)
import Data.List (sort)
import qualified Data.Vector.Unboxed as U
import Topo.Config.JSON
  (ToJSON(..), FromJSON(..), configOptions, mergeDefaults,
   genericToJSON, genericParseJSON)
import Topo.Hex (hexNeighborIndices)
import Topo.Types

-- | A rule table mapping biome families to temperature × precipitation
--   rectangles.  Each entry is @(BiomeId, (tempLo, tempHi), (precipLo, precipHi))@.
newtype BiomeRule = BiomeRule
  { brTable :: [(BiomeId, (Float, Float), (Float, Float))]
  } deriving (Eq, Show, Generic)

instance ToJSON BiomeRule where
  toJSON = genericToJSON (configOptions "br")

instance FromJSON BiomeRule where
  parseJSON v = genericParseJSON (configOptions "br")
                  (mergeDefaults (toJSON defaultBiomeRules) v)

newtype BiomeMap = BiomeMap (U.Vector BiomeId)
  deriving (Eq, Show)

-- | Thresholds used by the primary classifier for temperature-primary
--   and terrain-form-based overrides.
--
-- Temperature is the primary axis for mountain biome classification;
-- elevation above sea level acts as a minimum floor to distinguish
-- mountain biomes from flat tundra/polar biomes at the same temperature.
data BiomeThresholds = BiomeThresholds
  { btCoastalBand       :: !Float
  -- ^ Width of the coastal band above the water level (default 0.03).
  , btFallbackBiome     :: !BiomeId
  -- ^ Biome assigned when no rule rectangle matches and the nearest
  --   centroid search produces nothing (safety net; should rarely fire).
  , btIceCapTemp        :: !Float
  -- ^ Maximum temperature for ice-cap classification (default 0.05).
  , btMontanePrecip     :: !Float
  -- ^ Minimum precipitation for montane forest (default 0.30).
  , btCliffSlope        :: !Float
  -- ^ Minimum slope to force alpine/cliff override (default 0.40).
  , btValleyMoisture    :: !Float
  -- ^ Moisture requirement for valley→wetland override (default 0.70).
  , btDepressionMoisture :: !Float
  -- ^ Moisture requirement for depression→wetland override (default 0.60).
  , btPrecipWeight      :: !Float
  -- ^ Weight applied to the precipitation dimension in the nearest-centroid
  --   fallback distance calculation.  Higher values prevent desert from
  --   winning fallback for tiles with non-negligible moisture (default 2.0).
  , btSeasonalitySavannaThreshold :: !Float
  -- ^ Minimum precipitation seasonality for reclassifying tropical/
  --   warm-temperate Forest to Savanna in the moderate-precipitation zone.
  --   Default: @0.40@.
  , btTempRangeGrasslandThreshold :: !Float
  -- ^ Minimum temperature range (continentality) for reclassifying
  --   warm-temperate Shrubland to Grassland.  Low temp-range tiles remain
  --   Shrubland (Mediterranean belt); high range → continental grassland.
  --   Default: @0.15@.
  , btSnowPolarDesertMaxPrecip :: !Float
  -- ^ Maximum precipitation for routing very-cold, very-dry tiles above
  --   sea level to the Tundra family (→ PolarDesert refinement) instead
  --   of Snow.  Default: @0.08@.
  , btReliefSavannaThreshold :: !Float
  -- ^ Minimum terrain relief for reclassifying flat-terrain
  --   Grassland tiles in the warm bands to Savanna.  High-relief
  --   tiles with steppe-like T×P are routed to Savanna for hilly
  --   grassland/savanna refinement.  Default: @0.25@.
  ---------------------------------------------------------------------------
  -- Temperature-primary mountain biome thresholds
  ---------------------------------------------------------------------------
  , btSnowMaxTemp       :: !Float
  -- ^ Maximum temperature for the Snow guard.  Tiles colder than this
  --   with height above sea level exceeding 'btSnowMinASL' become Snow.
  --   Default: @0.20@ (≈ −16 °C).
  , btSnowMinASL        :: !Float
  -- ^ Minimum height above the water level for the Snow guard.
  --   Prevents flat arctic coast from becoming Snow (→ Tundra via rule
  --   table instead).  Default: @0.02@ (= 240 m).
  , btAlpineMaxTemp     :: !Float
  -- ^ Maximum temperature for the Alpine guard.  Tiles colder than this
  --   with height above sea level exceeding 'btAlpineMinASL' become
  --   Alpine.  Default: @0.35@.
  , btAlpineMinASL      :: !Float
  -- ^ Minimum height above sea level for the Alpine guard.
  --   Prevents flat tundra from becoming Alpine.  Default: @0.04@ (= 480 m).
  , btMontaneMaxTemp    :: !Float
  -- ^ Maximum temperature for the Montane guard.  Tiles colder than this
  --   in hilly/mountainous terrain become either Montane Forest (wet) or
  --   Alpine (dry).  Default: @0.55@.
  , btMontaneMinASL     :: !Float
  -- ^ Minimum height above sea level for the Montane guard.
  --   Default: @0.03@ (= 360 m).
  , btMontaneMinSlope   :: !Float
  -- ^ Minimum slope for montane classification when terrain form is
  --   neither hilly nor mountainous.  Default: @0.06@.
  , btMontaneMinHumidity :: !Float
  -- ^ Minimum humidity for montane forest vs dry montane (→ Alpine).
  --   Default: @0.40@.
  } deriving (Eq, Show, Generic)

instance ToJSON BiomeThresholds where
  toJSON = genericToJSON (configOptions "bt")

instance FromJSON BiomeThresholds where
  parseJSON v = genericParseJSON (configOptions "bt")
                  (mergeDefaults (toJSON defaultBiomeThresholds) v)

-- | Vegetation density configuration for biome-driven density.
--
-- This is distinct from 'Topo.Vegetation.VegetationBootstrapConfig' which
-- controls the pre-climate bootstrap.  This config controls how biome
-- classification influences vegetation density in the post-biome pass.
data BiomeVegetationConfig = BiomeVegetationConfig
  { vcBaseDensity  :: !Float
    -- ^ Base vegetation density for all tiles [0..1].
  , vcBiomeBoost   :: !Float
    -- ^ Extra density added for tiles in forest\/wetland biomes [0..1].
  , vcTempWeight   :: !Float
    -- ^ Weight of temperature in the density blend [0..1].
  , vcPrecipWeight :: !Float
    -- ^ Weight of precipitation in the density blend [0..1].
  , vcBoostBiomes  :: ![BiomeId]
    -- ^ Biome families that receive the 'vcBiomeBoost' density bonus.
  , vcMinWeightSum :: !Float
    -- ^ Guard against division by zero in the weight normalisation.
  } deriving (Eq, Show, Generic)

instance ToJSON BiomeVegetationConfig where
  toJSON = genericToJSON (configOptions "vc")

instance FromJSON BiomeVegetationConfig where
  parseJSON v = genericParseJSON (configOptions "vc")
                  (mergeDefaults (toJSON defaultBiomeVegetationConfig) v)

defaultBiomeVegetationConfig :: BiomeVegetationConfig
defaultBiomeVegetationConfig = BiomeVegetationConfig
  { vcBaseDensity  = 0.2
  , vcBiomeBoost   = 0.6
  , vcTempWeight   = 0.6
  , vcPrecipWeight = 0.4
  , vcBoostBiomes  = [ BiomeForest, BiomeRainforest, BiomeSwamp
                      , BiomeTempDeciduousForest, BiomeTempConiferousForest
                      , BiomeTropicalDryForest, BiomeMontaneForest
                      , BiomeCloudForest, BiomeTempRainforest
                      , BiomeTropicalRainforest, BiomeBorealForest
                      , BiomeWetland, BiomeMarsh, BiomeFen
                      , BiomeFloodplainForest, BiomeMangrove
                      , BiomeBog, BiomeBorealBog
                      , BiomeSavanna, BiomeWoodlandSavanna
                      , BiomeTaiga, BiomeSaltMarsh, BiomeEstuary
                      , BiomeTropicalSeasonalForest, BiomeOceanicBoreal
                      ]
  , vcMinWeightSum = 0.0001
  }

-- | Default Whittaker-style biome rules — 15-rule non-overlapping tiling.
--
-- Temperature and precipitation ranges are normalised to [0, 1].
-- The Celsius mapping is @norm * 70 − 30@ (so 0.36 ≈ −5 °C, 0.50 ≈ 5 °C,
-- 0.64 ≈ 15 °C, 0.76 ≈ 23 °C).
--
-- Five temperature bands (polar / boreal / cool-temperate / warm-temperate
-- / tropical) are each subdivided into precipitation zones that tile the
-- [0, 1] × [0, 1] space without gaps or overlaps.  Upper bounds use 1.01
-- at the edges so tiles at exactly T = 1.0 or P = 1.0 are captured
-- without relying on the nearest-centroid fallback.
--
-- Biome families NOT in this table are assigned by the priority overrides
-- in 'classifyBiome' (Alpine, Snow, Coastal, Ocean, Lake, Swamp, etc.).
--
-- Precipitation-weighted fallback (see 'btPrecipWeight') prevents desert
-- from winning at moderate moisture levels for any remaining edge cases.
defaultBiomeRules :: BiomeRule
defaultBiomeRules = BiomeRule
  [ -- ── Polar / Tundra  (T < 0.36, i.e., below −5 °C) ──────────────
    -- Everything below −5 °C is tundra family; refinement picks
    -- polar desert, arctic tundra, or alpine tundra.
    (BiomeTundra,      (0.00, 0.36), (0.00, 1.01))

    -- ── Boreal  (T 0.36–0.50, i.e., −5 °C to 5 °C) ────────────────
  , (BiomeGrassland,   (0.36, 0.50), (0.00, 0.15))
    -- Cold steppe / very dry boreal (refined → Steppe)
  , (BiomeTaiga,       (0.36, 0.50), (0.15, 1.01))
    -- Boreal forest / taiga (wet extreme refined → BorealBog)

    -- ── Cool Temperate  (T 0.50–0.64, i.e., 5 °C to 15 °C) ────────
  , (BiomeGrassland,   (0.50, 0.57), (0.00, 0.10))
    -- Cold steppe: continental interior at 45–55° latitude
    -- (replaces desert at the lower half of cool-temperate band)
  , (BiomeDesert,      (0.57, 0.64), (0.00, 0.10))
    -- True cool / continental desert (Gobi fringe, Patagonia steppe)
  , (BiomeGrassland,   (0.50, 0.64), (0.10, 0.30))
    -- Temperate steppe and grassland
  , (BiomeForest,      (0.50, 0.64), (0.30, 0.75))
    -- Temperate deciduous / coniferous / mixed forest
  , (BiomeRainforest,  (0.50, 0.64), (0.75, 1.01))
    -- Cool temperate rainforest (Valdivian, NZ podocarp)

    -- ── Warm Temperate  (T 0.64–0.76, i.e., 15 °C to 23 °C) ───────
  , (BiomeDesert,      (0.64, 0.76), (0.00, 0.10))
    -- Warm subtropical desert
  , (BiomeShrubland,   (0.64, 0.76), (0.10, 0.30))
    -- Mediterranean / chaparral / warm scrub
  , (BiomeForest,      (0.64, 0.76), (0.30, 0.75))
    -- Warm temperate / subtropical forest
  , (BiomeRainforest,  (0.64, 0.76), (0.75, 1.01))
    -- Warm temperate rainforest (Tasmanian, Atlantic forest fringe)

    -- ── Tropical  (T ≥ 0.76, i.e., above 23 °C) ───────────────────
  , (BiomeDesert,      (0.76, 1.01), (0.00, 0.10))
    -- Hot desert (Sahara, Arabian, Australian interior)
  , (BiomeSavanna,     (0.76, 1.01), (0.10, 0.40))
    -- Tropical savanna (African, Cerrado, Llanos)
  , (BiomeForest,      (0.76, 1.01), (0.40, 0.60))
    -- Tropical dry / seasonal forest — tiles with high seasonality
    -- are reclassified to Savanna by the post-rule seasonality pass
    -- in 'classifyBiome'.
  , (BiomeRainforest,  (0.76, 1.01), (0.60, 1.01))
    -- Tropical rainforest (Amazon, Congo, SE Asian)
  ]

-- | Default thresholds for the primary classifier.
--
-- Temperature-primary mountain biome thresholds ensure that elevation
-- decisions are driven by the (latitude+lapse-rate-adjusted) temperature
-- rather than fixed elevation bands, producing latitude-correct treeline
-- and snowline placement.
defaultBiomeThresholds :: BiomeThresholds
defaultBiomeThresholds = BiomeThresholds
  { btCoastalBand        = 0.03
  , btFallbackBiome      = BiomeGrassland
  , btIceCapTemp         = 0.05
  , btMontanePrecip      = 0.30
  , btCliffSlope         = 0.40
  , btValleyMoisture     = 0.70
  , btDepressionMoisture = 0.60
  , btPrecipWeight       = 2.0
  , btSeasonalitySavannaThreshold = 0.40
  , btTempRangeGrasslandThreshold = 0.15
  , btSnowPolarDesertMaxPrecip = 0.08
  , btReliefSavannaThreshold = 0.25
  -- Temperature-primary mountain biome thresholds
  , btSnowMaxTemp        = 0.20
  , btSnowMinASL         = 0.02
  , btAlpineMaxTemp      = 0.35
  , btAlpineMinASL       = 0.04
  , btMontaneMaxTemp     = 0.55
  , btMontaneMinASL      = 0.03
  , btMontaneMinSlope    = 0.06
  , btMontaneMinHumidity = 0.40
  }

---------------------------------------------------------------------------
-- Primary classification
---------------------------------------------------------------------------

-- | Classify a single tile into a family-level biome.
--
-- Priority order:
--
--   1. Water body type dispatch (ocean / lake / inland sea / dry)
--   2. Coastal override
--   3. Temperature-primary mountain biome guards:
--      a. Snow — very cold + above sea level
--      b. Alpine — cold + above sea level
--      c. Montane — cool + elevated + mountainous terrain (wet→forest, dry→alpine)
--   4. Terrain-form overrides (cliff, valley, depression)
--   5. Whittaker rule-table lookup (temperature × precipitation)
--   6. Seasonality & continentality reclassification
--   7. Nearest-centroid fallback
classifyBiome
  :: BiomeRule
  -> BiomeThresholds
  -> Float          -- ^ waterLevel
  -> WaterBodyType  -- ^ water body classification
  -> WaterBodyType  -- ^ adjacent water body type (highest-priority neighbour)
  -> Float          -- ^ temperature  (normalised 0–1)
  -> Float          -- ^ precipitation (normalised 0–1)
  -> Float          -- ^ elevation    (normalised)
  -> Float          -- ^ slope
  -> Float          -- ^ relief
  -> Float          -- ^ moisture
  -> TerrainForm    -- ^ terrain form classification
  -> Float          -- ^ humidity (annual average, normalised 0–1)
  -> Float          -- ^ temperature range (annual, normalised 0–1)
  -> Float          -- ^ precipitation seasonality (0–1)
  -> BiomeId
classifyBiome (BiomeRule rules) thr wl wbt adjWbt temp precip elev slope relief moisture tform
              humidity tempRange precipSeason
  -- 1. Water body type dispatch
  | wbt == WaterOcean
  = BiomeOcean
  | wbt == WaterLake
  = BiomeLake
  | wbt == WaterInlandSea
  = BiomeInlandSea
  -- 1b. Fallback: tile below water level without water-body classification
  --     (e.g. when WaterBody stage hasn't run yet). Treat as ocean.
  | wbt == WaterDry && elev < wl
  = BiomeOcean
  -- 2. Coastal override (land tiles only from here)
  | elev < wl + btCoastalBand thr && elev >= wl
  = case adjWbt of
      _ | adjWbt == WaterOcean -> BiomeCoastal  -- true ocean coast
        | otherwise            -> BiomeSwamp    -- inland shore / basin → wetland family

  ---------------------------------------------------------------------------
  -- 3. Temperature-primary mountain biome guards
  --
  -- Temperature encodes both latitude and elevation (via lapse rate), so
  -- a temperature-primary classification naturally produces correct
  -- behaviour at all latitudes.  Height above sea level (hASL) is used
  -- only as a *minimum floor* to prevent flat arctic coast / tundra from
  -- being classified as alpine/snow.
  ---------------------------------------------------------------------------

  -- ═══ SNOW / ICE ═══
  -- Very cold + very dry above sea level → polar desert (Tundra family)
  | temp < btSnowMaxTemp thr && hASL > btSnowMinASL thr
    && precip < btSnowPolarDesertMaxPrecip thr
  = BiomeTundra
  -- Very cold + above sea level → permanent snow/ice
  | temp < btSnowMaxTemp thr && hASL > btSnowMinASL thr
  = BiomeSnow

  -- ═══ ALPINE ═══
  -- Cold + elevated above sea level → alpine
  | temp < btAlpineMaxTemp thr && hASL > btAlpineMinASL thr
  = BiomeAlpine

  -- ═══ MONTANE ═══
  -- Cool + elevated + hilly/mountainous terrain (or steep slope)
  -- Wet → montane forest; dry → alpine shrub/grassland
  | temp < btMontaneMaxTemp thr && hASL > btMontaneMinASL thr
    && isMontaneTerrain
  = if precip >= btMontanePrecip thr && humidity >= btMontaneMinHumidity thr
    then BiomeForest    -- montane forest (→ cloud forest / montane RF in refinement)
    else BiomeAlpine    -- dry montane → alpine shrub/grassland

  -- 4. Terrain-form overrides (non-mountain)
  | slope > btCliffSlope thr
  = BiomeAlpine        -- cliff faces → alpine/rock
  | tform == FormValley && moisture >= btValleyMoisture thr
  = BiomeSwamp         -- wet valley → wetland (refined later)
  | tform == FormDepression && moisture >= btDepressionMoisture thr
  = BiomeSwamp         -- wet depression → wetland (refined later)

  -- 5. Rule-table lookup + seasonality/continentality reclassification
  | otherwise
  = let base = case matchRule rules of
                 Just bid -> bid
                 Nothing  -> nearestCentroid rules
    in reclassify base
  where
    -- Height above sea level (water level).
    -- Used as a minimum floor in the temperature-primary mountain guards.
    hASL = elev - wl

    -- Montane terrain: hilly/mountainous form, or slope above threshold.
    isMontaneTerrain =
      tform == FormMountainous || tform == FormHilly
      || slope >= btMontaneMinSlope thr

    -- Precipitation is weighted more heavily than temperature in the
    -- distance metric (controlled by 'btPrecipWeight').  This prevents
    -- desert from winning fallback for tiles with non-negligible moisture.
    pW = btPrecipWeight thr

    matchRule [] = Nothing
    matchRule ((bid, (t0, t1), (p0, p1)) : rest)
      | temp >= t0 && temp < t1 && precip >= p0 && precip < p1 = Just bid
      | otherwise = matchRule rest

    nearestCentroid [] = btFallbackBiome thr
    nearestCentroid ((bid, tRange, pRange) : rest) =
      fst (foldl pick (bid, centroidDist tRange pRange) rest)

    centroidDist (t0, t1) (p0, p1) =
      let tc = (t0 + t1) / 2
          pc = (p0 + p1) / 2
          dt = temp - tc
          dp = precip - pc
      in dt * dt + pW * dp * dp

    pick (bidA, distA) (bidB, tRange, pRange) =
      let distB = centroidDist tRange pRange
      in if distB < distA then (bidB, distB) else (bidA, distA)

    -- 5. Seasonality, continentality, and relief reclassification.
    --
    -- After the rule table produces a family-level biome, we apply
    -- corrections using the three new climate dimensions that the
    -- 2D T×P grid cannot express, plus terrain relief.
    reclassify bid
      -- P1.2: Tropical and warm-temperate Forest at moderate precip
      -- with high seasonality → Savanna (monsoonal / seasonal).
      | bid == BiomeForest
        && temp >= 0.64
        && precip >= 0.30 && precip < 0.60
        && precipSeason >= btSeasonalitySavannaThreshold thr
      = BiomeSavanna
      -- P1.3: Warm-temperate Shrubland with high continentality
      -- (temperature range) → Grassland (continental steppe/prairie).
      | bid == BiomeShrubland
        && temp >= 0.64 && temp < 0.76
        && tempRange >= btTempRangeGrasslandThreshold thr
      = BiomeGrassland
      -- P3.7: High-relief Grassland tiles in warm bands → Savanna.
      -- Hilly/rolling grassland with significant relief looks and
      -- functions more like savanna than flat steppe or prairie.
      | bid == BiomeGrassland
        && temp >= 0.50
        && relief >= btReliefSavannaThreshold thr
      = BiomeSavanna
      | otherwise
      = bid

-- | Classify all tiles in a chunk.
classifyBiomesChunk
  :: WorldConfig
  -> BiomeRule
  -> BiomeThresholds
  -> Float                      -- ^ waterLevel
  -> U.Vector WaterBodyType     -- ^ water body classification
  -> U.Vector WaterBodyType     -- ^ adjacent water body type
  -> U.Vector Float             -- ^ temperature
  -> U.Vector Float             -- ^ precipitation
  -> U.Vector Float             -- ^ elevation
  -> U.Vector Float             -- ^ slope
  -> U.Vector Float             -- ^ relief
  -> U.Vector Float             -- ^ moisture
  -> U.Vector TerrainForm       -- ^ terrain form
  -> U.Vector Float             -- ^ humidity (annual average)
  -> U.Vector Float             -- ^ temperature range (annual)
  -> U.Vector Float             -- ^ precipitation seasonality
  -> U.Vector BiomeId
classifyBiomesChunk _config rules thr wl wbt adjWbt temp precip elev slope relief moisture tform
                    humidity tempRange precipSeason =
  U.generate (U.length temp) $ \i ->
    classifyBiome rules thr wl (wbt U.! i) (adjWbt U.! i)
      (temp U.! i) (precip U.! i) (elev U.! i)
      (slope U.! i) (relief U.! i) (moisture U.! i) (tform U.! i)
      (humidity U.! i) (tempRange U.! i) (precipSeason U.! i)

smoothBiomesChunk :: WorldConfig -> Int -> U.Vector BiomeId -> U.Vector BiomeId
smoothBiomesChunk config iterations biomes
  | iterations <= 0 = biomes
  | otherwise = smoothBiomesChunk config (iterations - 1) (smoothOnce config biomes)

smoothOnce :: WorldConfig -> U.Vector BiomeId -> U.Vector BiomeId
smoothOnce config biomes =
  let size = wcChunkSize config
      n = U.length biomes
  in U.generate n (smoothAt size biomes)

-- | Smooth biomes on a stitched global grid (inter-chunk aware).
--
-- Unlike 'smoothBiomesChunk', which operates within a single chunk
-- (and therefore misses neighbours across chunk boundaries), this
-- function smooths a flat global vector with dimensions
-- @gridW × gridH@ tiles.  Hex neighbours are resolved at the global
-- scale so chunk-boundary tiles see their true 6 neighbours.
smoothBiomesGrid
  :: Int          -- ^ iterations
  -> Int          -- ^ gridW (total tiles wide)
  -> Int          -- ^ gridH (total tiles tall)
  -> U.Vector BiomeId
  -> U.Vector BiomeId
smoothBiomesGrid iterations gridW gridH biomes
  | iterations <= 0 = biomes
  | otherwise = smoothBiomesGrid (iterations - 1) gridW gridH (smoothOnceGrid gridW gridH biomes)

-- | A single pass of majority-vote smoothing on a global grid.
smoothOnceGrid :: Int -> Int -> U.Vector BiomeId -> U.Vector BiomeId
smoothOnceGrid gridW gridH biomes =
  let n = U.length biomes
  in U.generate n (smoothAtGrid gridW gridH biomes)

-- | Smooth a single tile on a global grid using its 6 hex neighbours.
smoothAtGrid :: Int -> Int -> U.Vector BiomeId -> Int -> BiomeId
smoothAtGrid gridW gridH biomes i =
  let b0 = biomes U.! i
      nbs = hexNeighborIndices gridW gridH i
      neighbors = b0 : map (biomes U.!) nbs
  in majority neighbors

-- | Smooth a single tile using its 6 hex neighbours (majority vote).
--
-- The tile itself is included in the vote with weight 1, so it can
-- survive if no neighbour biome has a strict majority.
smoothAt :: Int -> U.Vector BiomeId -> Int -> BiomeId
smoothAt size biomes i =
  let b0 = biomes U.! i
      nbs = hexNeighborIndices size size i
      neighbors = b0 : map (biomes U.!) nbs
  in majority neighbors

majority :: [BiomeId] -> BiomeId
majority xs =
  let counts = foldl (\acc v -> insertCount v acc) [] xs
  in fst (foldl1 (\a b -> if snd a >= snd b then a else b) counts)
  where
    insertCount v [] = [(v, 1)]
    insertCount v ((k, c):rest)
      | v == k = (k, c + 1) : rest
      | otherwise = (k, c) : insertCount v rest

---------------------------------------------------------------------------
-- Post-smoothing temperature constraint
---------------------------------------------------------------------------

-- | Correct biomes that were spread into thermally incompatible tiles
-- by majority-vote smoothing.
--
-- After smoothing, some tiles may have been reassigned to Snow or Alpine
-- despite being too warm.  This pass demotes:
--
--   * Snow tiles with @temp > btSnowMaxTemp@ → Alpine
--   * Alpine tiles with @temp > btAlpineMaxTemp@ → the original
--     pre-smoothing biome (passed as the @original@ vector)
--
-- Runs per-chunk; call once per chunk after global smoothing.
constrainSmoothedBiomes
  :: BiomeThresholds
  -> U.Vector Float             -- ^ temperature (per tile)
  -> U.Vector BiomeId           -- ^ original biomes (pre-smoothing)
  -> U.Vector BiomeId           -- ^ smoothed biomes
  -> U.Vector BiomeId
constrainSmoothedBiomes thr temp original smoothed =
  U.generate (U.length smoothed) $ \i ->
    let bid  = smoothed U.! i
        t    = if i < U.length temp then temp U.! i else 0
        orig = if i < U.length original then original U.! i else bid
    in constrainTile thr t orig bid

-- | Constrain a single tile's biome after smoothing.
constrainTile :: BiomeThresholds -> Float -> BiomeId -> BiomeId -> BiomeId
constrainTile thr t orig bid
  -- Snow too warm → demote to Alpine
  | bid == BiomeSnow && t > btSnowMaxTemp thr
  = BiomeAlpine
  -- Alpine too warm → revert to pre-smoothing biome
  | bid == BiomeAlpine && t > btAlpineMaxTemp thr
  = orig
  | otherwise
  = bid

---------------------------------------------------------------------------
-- Mountain transition smoothing (P3.2)
---------------------------------------------------------------------------

-- | Smooth mountain biome transitions on a global grid to prevent
-- large biome-family jumps (e.g., Snow directly adjacent to Forest).
--
-- For each tile that belongs to the mountain gradient
-- (Forest, Alpine, Snow), this pass calculates the median mountain
-- rank of the tile's hex neighbourhood and shifts the tile toward
-- that median when the gap exceeds one rank step.
--
-- The mountain gradient hierarchy is:
--
--   * Rank 1: Forest (montane forest context before refinement)
--   * Rank 2: Alpine
--   * Rank 3: Snow
--
-- Non-mountain biomes (rank 0) are ignored in the median calculation
-- and are never modified.  This ensures that lowland biomes are not
-- affected, and only mountain-to-mountain transitions are smoothed.
--
-- Run this after 'constrainSmoothedBiomes' and before refinement.
smoothMountainTransitions
  :: Int          -- ^ iterations (typically 1)
  -> Int          -- ^ gridW (total tiles wide)
  -> Int          -- ^ gridH (total tiles tall)
  -> U.Vector BiomeId
  -> U.Vector BiomeId
smoothMountainTransitions iterations gridW gridH biomes
  | iterations <= 0 = biomes
  | otherwise =
      let biomes' = smoothMountainOnce gridW gridH biomes
      in smoothMountainTransitions (iterations - 1) gridW gridH biomes'

-- | A single pass of mountain transition smoothing.
smoothMountainOnce :: Int -> Int -> U.Vector BiomeId -> U.Vector BiomeId
smoothMountainOnce gridW gridH biomes =
  U.generate (U.length biomes) (smoothMountainAt gridW gridH biomes)

-- | Smooth a single mountain tile toward the neighbourhood median rank.
smoothMountainAt :: Int -> Int -> U.Vector BiomeId -> Int -> BiomeId
smoothMountainAt gridW gridH biomes i =
  let b0 = biomes U.! i
      myRank = mountainRank b0
  in case myRank of
       0 -> b0  -- non-mountain: pass through unchanged
       _ ->
         let nbs = hexNeighborIndices gridW gridH i
             neighbourRanks = filter (> 0) (map (mountainRank . (biomes U.!)) nbs)
         in case neighbourRanks of
              [] -> b0  -- no mountain neighbours: keep as-is
              _  ->
                let allRanks = myRank : neighbourRanks
                    medRank  = median allRanks
                in if abs (myRank - medRank) > 1
                   then rankToBiome (myRank + signum (medRank - myRank))
                   else b0

-- | Mountain gradient rank.  Returns 0 for non-mountain biomes.
mountainRank :: BiomeId -> Int
mountainRank bid
  | bid == BiomeForest  = 1
  | bid == BiomeAlpine  = 2
  | bid == BiomeSnow    = 3
  | otherwise           = 0
{-# INLINE mountainRank #-}

-- | Convert a mountain rank back to a family-level biome.
--
-- Rank 0 should never be reached (we skip non-mountain tiles),
-- but we return 'BiomeForest' as a safe fallback.
rankToBiome :: Int -> BiomeId
rankToBiome 1 = BiomeForest
rankToBiome 2 = BiomeAlpine
rankToBiome 3 = BiomeSnow
rankToBiome _ = BiomeForest  -- safe fallback (should not occur)
{-# INLINE rankToBiome #-}

-- | Compute the median of a non-empty list of integers.
--
-- For an even number of elements, returns the lower-median
-- (floor of the average of the two middle elements).
median :: [Int] -> Int
median xs =
  let sorted = sort xs
      n = length sorted
      mid = n `div` 2
  in sorted !! mid
{-# INLINE median #-}

vegetationDensityChunk :: BiomeVegetationConfig -> U.Vector BiomeId -> U.Vector Float -> U.Vector Float -> U.Vector Float
vegetationDensityChunk cfg biomes temp precip =
  U.generate (U.length biomes) (\i ->
    let biome = biomes U.! i
        base = vcBaseDensity cfg
        boost = if biome `elem` vcBoostBiomes cfg then vcBiomeBoost cfg else 0
        tw = max 0 (vcTempWeight cfg)
        pw = max 0 (vcPrecipWeight cfg)
        denom = max (vcMinWeightSum cfg) (tw + pw)
        climate = clamp01 ((temp U.! i * tw + precip U.! i * pw) / denom)
    in clamp01 (base + boost * climate))

clamp01 :: Float -> Float
clamp01 v
  | v < 0 = 0
  | v > 1 = 1
  | otherwise = v
