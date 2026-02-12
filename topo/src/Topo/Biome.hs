-- | Primary biome classification and vegetation density.
--
-- Uses a Whittaker-style temperature × precipitation lookup with
-- elevation and terrain-form overrides, followed by nearest-centroid
-- fallback for tiles that fall between rule rectangles.
module Topo.Biome
  ( BiomeRule(..)
  , BiomeMap
  , BiomeThresholds(..)
  , defaultBiomeRules
  , defaultBiomeThresholds
  , classifyBiome
  , classifyBiomesChunk
  , smoothBiomesChunk
  , VegetationConfig(..)
  , defaultVegetationConfig
  , vegetationDensityChunk
  ) where

import qualified Data.Vector.Unboxed as U
import Topo.Hex (hexNeighborIndices)
import Topo.Types

-- | A rule table mapping biome families to temperature × precipitation
--   rectangles.  Each entry is @(BiomeId, (tempLo, tempHi), (precipLo, precipHi))@.
newtype BiomeRule = BiomeRule
  { brTable :: [(BiomeId, (Float, Float), (Float, Float))]
  } deriving (Eq, Show)

newtype BiomeMap = BiomeMap (U.Vector BiomeId)
  deriving (Eq, Show)

-- | Thresholds used by the primary classifier for elevation-based and
--   terrain-form-based overrides.
data BiomeThresholds = BiomeThresholds
  { btCoastalBand       :: !Float
  -- ^ Width of the coastal band above the water level (default 0.03).
  , btSnowElevation     :: !Float
  -- ^ Elevation above which tiles are classified as snow (default 0.90).
  , btAlpineElevation   :: !Float
  -- ^ Elevation above which tiles are classified as alpine (default 0.75).
  , btFallbackBiome     :: !BiomeId
  -- ^ Biome assigned when no rule rectangle matches and the nearest
  --   centroid search produces nothing (safety net; should rarely fire).
  , btIceCapTemp        :: !Float
  -- ^ Maximum temperature for ice-cap classification (default 0.05).
  , btMontaneLow        :: !Float
  -- ^ Lower elevation bound for montane forest (default 0.52).
  , btMontanePrecip     :: !Float
  -- ^ Minimum precipitation for montane forest (default 0.35).
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
  } deriving (Eq, Show)

-- | Vegetation density configuration.
data VegetationConfig = VegetationConfig
  { vcBaseDensity  :: !Float
  , vcBiomeBoost   :: !Float
  , vcTempWeight   :: !Float
  , vcPrecipWeight :: !Float
  , vcBoostBiomes  :: ![BiomeId]
  , vcMinWeightSum :: !Float
  } deriving (Eq, Show)

defaultVegetationConfig :: VegetationConfig
defaultVegetationConfig = VegetationConfig
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
  , (BiomeDesert,      (0.50, 0.64), (0.00, 0.10))
    -- Cool / continental desert (Gobi fringe, Patagonia steppe)
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
    -- Tropical dry / seasonal forest
  , (BiomeRainforest,  (0.76, 1.01), (0.60, 1.01))
    -- Tropical rainforest (Amazon, Congo, SE Asian)
  ]

-- | Default thresholds for the primary classifier.
defaultBiomeThresholds :: BiomeThresholds
defaultBiomeThresholds = BiomeThresholds
  { btCoastalBand        = 0.03
  , btSnowElevation      = 0.90
  , btAlpineElevation    = 0.75
  , btFallbackBiome      = BiomeGrassland
  , btIceCapTemp         = 0.05
  , btMontaneLow         = 0.52
  , btMontanePrecip      = 0.35
  , btCliffSlope         = 0.40
  , btValleyMoisture     = 0.70
  , btDepressionMoisture = 0.60
  , btPrecipWeight       = 2.0
  }

---------------------------------------------------------------------------
-- Primary classification
---------------------------------------------------------------------------

-- | Classify a single tile into a family-level biome.
--
-- Priority order:
--
--   1. Water body type dispatch (ocean / lake / inland sea / dry)
--   2. Elevation overrides (coastal, snow/ice cap, alpine)
--   3. Terrain-form overrides (cliff, mountainous, valley, depression)
--   4. Whittaker rule-table lookup (temperature × precipitation)
--   5. Nearest-centroid fallback
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
  -> BiomeId
classifyBiome (BiomeRule rules) thr wl wbt adjWbt temp precip elev slope _relief moisture tform
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
  -- 2. Elevation overrides (land tiles only from here)
  | elev < wl + btCoastalBand thr && elev >= wl
  = case adjWbt of
      _ | adjWbt == WaterOcean -> BiomeCoastal  -- true ocean coast
        | otherwise            -> BiomeSwamp    -- inland shore / basin → wetland family
  | elev > btSnowElevation thr && temp < btIceCapTemp thr
  = BiomeSnow          -- ice cap / permanent snow
  | elev > btSnowElevation thr
  = BiomeSnow
  | elev > btAlpineElevation thr
  = BiomeAlpine

  -- 2. Terrain-form overrides
  | slope > btCliffSlope thr
  = BiomeAlpine        -- cliff faces → alpine/rock
  | tform == FormMountainous && elev > btMontaneLow thr && precip >= btMontanePrecip thr
  = BiomeForest        -- montane forest (refined later to BiomeMontaneForest)
  | tform == FormMountainous && elev > btMontaneLow thr
  = BiomeAlpine        -- dry mountainous → alpine
  | tform == FormValley && moisture >= btValleyMoisture thr
  = BiomeSwamp         -- wet valley → wetland (refined later)
  | tform == FormDepression && moisture >= btDepressionMoisture thr
  = BiomeSwamp         -- wet depression → wetland (refined later)

  -- 3. Rule-table lookup
  | otherwise
  = case matchRule rules of
      Just bid -> bid
      Nothing  -> nearestCentroid rules
  where
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
  -> U.Vector BiomeId
classifyBiomesChunk _config rules thr wl wbt adjWbt temp precip elev slope relief moisture tform =
  U.generate (U.length temp) $ \i ->
    classifyBiome rules thr wl (wbt U.! i) (adjWbt U.! i)
      (temp U.! i) (precip U.! i) (elev U.! i)
      (slope U.! i) (relief U.! i) (moisture U.! i) (tform U.! i)

smoothBiomesChunk :: WorldConfig -> Int -> U.Vector BiomeId -> U.Vector BiomeId
smoothBiomesChunk config iterations biomes
  | iterations <= 0 = biomes
  | otherwise = smoothBiomesChunk config (iterations - 1) (smoothOnce config biomes)

smoothOnce :: WorldConfig -> U.Vector BiomeId -> U.Vector BiomeId
smoothOnce config biomes =
  let size = wcChunkSize config
      n = U.length biomes
  in U.generate n (smoothAt size biomes)

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

vegetationDensityChunk :: VegetationConfig -> U.Vector BiomeId -> U.Vector Float -> U.Vector Float -> U.Vector Float
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
