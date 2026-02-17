-- | Phase 3 -- Cross-Module Alignment (Climate / Biome).
--
-- Validates that the precipitation distribution produced by the climate
-- pipeline is compatible with biome classification thresholds under
-- default Earth-like parameters.
--
-- __3.1__ Precipitation percentiles per temperature band are compared
-- against the biome rule cutoffs.  Each wet-biome family (forest,
-- rainforest, taiga) must be reachable — i.e., a non-trivial fraction
-- of tiles in the corresponding temperature band must have precipitation
-- at or above the family's threshold.
--
-- __3.2__ Integration guardrails assert macro-level qualitative outcomes:
-- non-trivial wet-biome share, coast wetter than interior (but interior
-- not desertified), and an orographic wet\/dry dipole around elevation
-- gradients.
module Spec.ClimateAlignment (spec) where

import Test.Hspec

import Topo

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector.Unboxed as U
import Data.Word (Word64)
import Data.List (foldl', sort)

---------------------------------------------------------------------------
-- Tile sample record
---------------------------------------------------------------------------

-- | Per-tile snapshot for statistical analysis.
data TileSample = TileSample
  { tsBiome      :: !BiomeId
  , tsTemp       :: !Float
  , tsPrec       :: !Float
  , tsElev       :: !Float
  , tsHumid     :: !Float
  , tsLat        :: !Float
  , tsIsLand     :: !Bool
  , tsIsCoastal  :: !Bool
  , tsIsInterior :: !Bool
  }

---------------------------------------------------------------------------
-- World generation helpers
---------------------------------------------------------------------------

-- | Seed shared across Phase 3 tests.
phase3Seed :: Word64
phase3Seed = 42

-- | Chunk size for Phase 3 tests.
phase3ChunkSize :: Int
phase3ChunkSize = 8

-- | Water level for Phase 3 tests.
phase3WaterLevel :: Float
phase3WaterLevel = 0.5

-- | Run the full pipeline and collect per-tile samples.
--
-- Uses a wide latitude slice (−50° to +50°, 30° longitude) covering
-- equatorial, subtropical, and temperate bands.
generateAndCollect :: IO [TileSample]
generateAndCollect = do
  let wc    = WorldConfig { wcChunkSize = phase3ChunkSize }
      slice = WorldSlice
        { wsLatCenter = 0
        , wsLatExtent = 100   -- −50° to +50°
        , wsLonCenter = 0
        , wsLonExtent = 30    -- −15° to +15°
        }
      wgc  = defaultWorldGenConfig { worldSlice = slice }
      pipe = buildFullPipelineConfig wgc wc phase3Seed
      env  = TopoEnv { teLogger = \_ -> pure () }
      world0 = emptyWorldWithPlanet wc defaultHexGridMeta defaultPlanetConfig slice
  result <- runPipeline pipe env world0
  case result of
    Left err         -> error ("Phase 3 pipeline failed: " ++ show err)
    Right (world, _) -> pure (collectSamples world)

-- | Extract 'TileSample's from every chunk.
collectSamples :: TerrainWorld -> [TileSample]
collectSamples world =
  let wc      = twConfig world
      planet  = twPlanet world
      slice   = twSlice world
      terrain = twTerrain world
      climate = twClimate world
      wb      = twWaterBodies world
  in concatMap (chunkSamples wc planet slice climate wb)
               (IntMap.toList terrain)

chunkSamples
  :: WorldConfig -> PlanetConfig -> WorldSlice
  -> ChunkMap ClimateChunk
  -> ChunkMap WaterBodyChunk
  -> (Int, TerrainChunk) -> [TileSample]
chunkSamples wc planet slice climate wb (k, tc) =
  let mcc  = IntMap.lookup k climate
      mwbc = IntMap.lookup k wb
      n    = U.length (tcElevation tc)
      coord = chunkCoordFromId (ChunkId k)
      TileCoord ox oy = chunkOriginTile wc coord
  in [ tileSample wc planet slice tc mcc mwbc ox oy i
     | i <- [0 .. n - 1]
     ]

tileSample
  :: WorldConfig -> PlanetConfig -> WorldSlice
  -> TerrainChunk
  -> Maybe ClimateChunk
  -> Maybe WaterBodyChunk
  -> Int -> Int -> Int
  -> TileSample
tileSample wc planet slice tc mcc mwbc ox oy i =
  let TileCoord lx ly = tileCoordFromIndex wc (TileIndex i)
      gx  = ox + lx
      gy  = oy + ly
      lat = tileLatitude planet slice wc (TileCoord gx gy)
      elev  = tcElevation tc U.! i
      biome = tcFlags tc U.! i
      temp  = maybe 0.5 (\cc -> ccTempAvg cc U.! i)    mcc
      prec  = maybe 0.0 (\cc -> ccPrecipAvg cc U.! i)  mcc
      hum   = maybe 0.0 (\cc -> ccHumidityAvg cc U.! i) mcc
      wt    = maybe WaterDry (\w -> wbType w U.! i) mwbc
      adj   = maybe WaterDry (\w -> wbAdjacentType w U.! i) mwbc
      isLand     = wt == WaterDry
      isCoastal  = isLand && adj == WaterOcean
      isInterior = isLand && adj == WaterDry
  in TileSample
    { tsBiome      = biome
    , tsTemp       = temp
    , tsPrec       = prec
    , tsElev       = elev
    , tsHumid     = hum
    , tsLat        = lat
    , tsIsLand     = isLand
    , tsIsCoastal  = isCoastal
    , tsIsInterior = isInterior
    }

---------------------------------------------------------------------------
-- Statistical helpers
---------------------------------------------------------------------------

data Accum = Accum
  { accCount :: !Int
  , accSum   :: !Double
  } deriving Show

emptyAccum :: Accum
emptyAccum = Accum 0 0

addAccum :: Float -> Accum -> Accum
addAccum x (Accum n s) = Accum (n + 1) (s + realToFrac x)

accumMean :: Accum -> Double
accumMean (Accum 0 _) = 0
accumMean (Accum n s) = s / fromIntegral n

-- | Fraction of tiles in @xs@ where the given field exceeds @threshold@.
fractionAbove :: (TileSample -> Float) -> Float -> [TileSample] -> Double
fractionAbove _ _ [] = 0
fractionAbove f threshold xs =
  let n = length xs
      k = foldl' (\acc t -> if f t >= threshold then acc + 1 else acc) (0 :: Int) xs
  in fromIntegral k / fromIntegral n

-- | Count how many samples satisfy a predicate.
countWhere :: (TileSample -> Bool) -> [TileSample] -> Int
countWhere p = foldl' (\n t -> if p t then n + 1 else n) 0

-- | Fraction of @total@ represented by @part@.
fraction :: Int -> Int -> Double
fraction _    0     = 0
fraction part total = fromIntegral part / fromIntegral total

-- | Compute percentile (0–100) of a value list.
percentile :: Int -> [Float] -> Float
percentile _ [] = 0
percentile p xs =
  let sorted = sort xs
      n      = length sorted
      idx    = max 0 (min (n - 1) ((p * n) `div` 100))
  in sorted !! idx

---------------------------------------------------------------------------
-- Biome category helpers
---------------------------------------------------------------------------

-- | "Wet biomes" — families that require non-trivial precipitation.
isWetBiome :: BiomeId -> Bool
isWetBiome b =
     b == BiomeForest
  || b == BiomeRainforest
  || b == BiomeTaiga
  || b == BiomeSwamp
  || b == BiomeTropicalDryForest
  || b == BiomeTempDeciduousForest
  || b == BiomeTempConiferousForest
  || b == BiomeMontaneForest
  || b == BiomeCloudForest
  || b == BiomeTempRainforest
  || b == BiomeTropicalRainforest
  || b == BiomeBorealForest
  || b == BiomeFloodplainForest
  || b == BiomeTropicalSeasonalForest
  || b == BiomeOceanicBoreal
  || b == BiomeWetland
  || b == BiomeMarsh
  || b == BiomeBog
  || b == BiomeFen
  || b == BiomeBorealBog

-- | True when the biome is a desert variant (including refined).
isDesertBiome :: BiomeId -> Bool
isDesertBiome b =
     b == BiomeDesert
  || b == BiomeHotDesert
  || b == BiomeColdDesert
  || b == BiomeRockyDesert
  || b == BiomeSandDesert
  || b == BiomeSaltFlat
  || b == BiomePolarDesert
  || b == BiomeFogDesert

---------------------------------------------------------------------------
-- Temperature bands (matching defaultBiomeRules)
---------------------------------------------------------------------------

-- | Tiles in the boreal band (T 0.36–0.50).
isBorealBand :: TileSample -> Bool
isBorealBand t = tsTemp t >= 0.36 && tsTemp t < 0.50

-- | Tiles in the cool-temperate band (T 0.50–0.64).
isCoolTempBand :: TileSample -> Bool
isCoolTempBand t = tsTemp t >= 0.50 && tsTemp t < 0.64

-- | Tiles in the warm-temperate band (T 0.64–0.76).
isWarmTempBand :: TileSample -> Bool
isWarmTempBand t = tsTemp t >= 0.64 && tsTemp t < 0.76

-- | Tiles in the tropical band (T ≥ 0.76).
isTropicalBand :: TileSample -> Bool
isTropicalBand t = tsTemp t >= 0.76

---------------------------------------------------------------------------
-- 3.1 — Precipitation percentiles per temperature band
---------------------------------------------------------------------------

precipPercentilesSpec :: SpecWith [TileSample]
precipPercentilesSpec = describe "3.1 Precipitation percentiles by temperature band" $ do

  -- Boreal band (T 0.36–0.50): Taiga needs precip ≥ 0.15
  it "boreal band: >=10% of land tiles reach taiga precip threshold (>=0.15)" $ \samples -> do
    let band = filter (\t -> tsIsLand t && isBorealBand t) samples
    length band `shouldSatisfy` (> 0)
    fractionAbove tsPrec 0.15 band `shouldSatisfy` (>= 0.10)

  it "boreal band: p75 precipitation >= 0.15" $ \samples -> do
    let precs = map tsPrec $ filter (\t -> tsIsLand t && isBorealBand t) samples
    length precs `shouldSatisfy` (> 0)
    percentile 75 precs `shouldSatisfy` (>= 0.15)

  -- Cool-temperate band (T 0.50–0.64): Forest needs ≥0.30
  it "cool-temp band: >=5% of land tiles reach forest precip threshold (>=0.30)" $ \samples -> do
    let band = filter (\t -> tsIsLand t && isCoolTempBand t) samples
    length band `shouldSatisfy` (> 0)
    fractionAbove tsPrec 0.30 band `shouldSatisfy` (>= 0.05)

  -- Warm-temperate band (T 0.64–0.76): Forest needs ≥0.30
  it "warm-temp band: >=5% of land tiles reach forest precip threshold (>=0.30)" $ \samples -> do
    let band = filter (\t -> tsIsLand t && isWarmTempBand t) samples
    length band `shouldSatisfy` (> 0)
    fractionAbove tsPrec 0.30 band `shouldSatisfy` (>= 0.05)

  -- Tropical band (T ≥ 0.76): Tropical forest needs ≥0.40, Rainforest ≥0.60
  it "tropical band: >=10% of land tiles reach tropical forest threshold (>=0.40)" $ \samples -> do
    let band = filter (\t -> tsIsLand t && isTropicalBand t) samples
    length band `shouldSatisfy` (> 0)
    fractionAbove tsPrec 0.40 band `shouldSatisfy` (>= 0.10)

  it "tropical band: some tiles reach rainforest threshold (>=0.60)" $ \samples -> do
    let band = filter (\t -> tsIsLand t && isTropicalBand t) samples
    length band `shouldSatisfy` (> 0)
    -- At least a few tiles should be wet enough for rainforest
    fractionAbove tsPrec 0.60 band `shouldSatisfy` (> 0.0)

  -- Cross-band check: no temperature band should be 100% desert
  it "no temperature band is 100% desert" $ \samples -> do
    let land = filter tsIsLand samples
        bands = [isBorealBand, isCoolTempBand, isWarmTempBand, isTropicalBand]
        allDesert band = all (isDesertBiome . tsBiome) (filter band land)
    mapM_ (\bandP -> do
      let tiles = filter bandP land
      if null tiles
        then pure ()  -- band not represented => skip
        else allDesert bandP `shouldBe` False
      ) bands

---------------------------------------------------------------------------
-- 3.2 — Integration guardrails
---------------------------------------------------------------------------

guardrailsSpec :: SpecWith [TileSample]
guardrailsSpec = describe "3.2 Integration guardrails" $ do

  -- 3.2a: Non-trivial wet-biome share under Earth-like defaults
  it "wet-biome share of land > 10%" $ \samples -> do
    let land = filter tsIsLand samples
        nLand = length land
        nWet  = countWhere (isWetBiome . tsBiome) land
        frac  = fraction nWet nLand
    nLand `shouldSatisfy` (> 0)
    frac `shouldSatisfy` (> 0.10)

  -- 3.2b: Continental interior drier than coast but not globally desertified
  it "interior mean precip > 0.10 (not globally desertified)" $ \samples -> do
    let interior = filter (\t -> tsIsInterior t && tsIsLand t) samples
        acc = foldl' (\a t -> addAccum (tsPrec t) a) emptyAccum interior
    accCount acc `shouldSatisfy` (> 0)
    accumMean acc `shouldSatisfy` (> 0.10)

  it "interior desert fraction < 50%" $ \samples -> do
    let interior = filter (\t -> tsIsInterior t && tsIsLand t) samples
        nInt     = length interior
        nDesert  = countWhere (isDesertBiome . tsBiome) interior
        frac     = fraction nDesert nInt
    nInt `shouldSatisfy` (> 0)
    frac `shouldSatisfy` (< 0.50)

  it "coastal mean precip > interior mean precip" $ \samples -> do
    let coastal  = filter (\t -> tsIsCoastal t && tsIsLand t) samples
        interior = filter (\t -> tsIsInterior t && tsIsLand t) samples
        cAcc = foldl' (\a t -> addAccum (tsPrec t) a) emptyAccum coastal
        iAcc = foldl' (\a t -> addAccum (tsPrec t) a) emptyAccum interior
    accCount cAcc `shouldSatisfy` (> 0)
    accCount iAcc `shouldSatisfy` (> 0)
    accumMean cAcc `shouldSatisfy` (> accumMean iAcc)

  -- 3.2c: Orographic wet/dry dipole
  --
  -- Strategy: partition land tiles by elevation.  "High" tiles (top 20%
  -- by elevation) act as mountain ridges.  "Low adjacent" tiles on the
  -- wet side (high precip near high elev) should be wetter than "low
  -- far" tiles (low precip near high elev — leeward shadow side).
  --
  -- A simpler proxy: land tiles at moderate elevation (0.55–0.65,
  -- i.e., just above sea level to foothills) that are near mountains
  -- should show higher precipitation variance than flat lowlands.
  -- The dominant signal is: tiles just below a major elevation
  -- gradient receive more precipitation than tiles just above it
  -- (leeward drying).
  --
  -- We test a more robust invariant: the correlation between elevation
  -- and precipitation among land tiles shows a non-trivial negative
  -- relationship at the high end (very high elevation → less precip
  -- than moderate elevation).
  it "high-elevation land is drier than mid-elevation land" $ \samples -> do
    let land = filter tsIsLand samples
        -- Mid-elevation: 0.55–0.65 (lowlands to foothills)
        midElev = filter (\t -> tsElev t >= 0.55 && tsElev t < 0.65) land
        -- High-elevation: >= 0.75 (mountains)
        highElev = filter (\t -> tsElev t >= 0.75) land
        midAcc  = foldl' (\a t -> addAccum (tsPrec t) a) emptyAccum midElev
        highAcc = foldl' (\a t -> addAccum (tsPrec t) a) emptyAccum highElev
    -- Only assert if both groups have data
    if accCount midAcc > 10 && accCount highAcc > 10
      then accumMean midAcc `shouldSatisfy` (> accumMean highAcc)
      else pure ()  -- insufficient tiles for this check

  it "precipitation variance is non-trivial among land tiles" $ \samples -> do
    let precs = map tsPrec $ filter tsIsLand samples
        n     = length precs
        mean  = sum (map realToFrac precs) / max 1 (fromIntegral n) :: Double
        var   = sum (map (\p -> (realToFrac p - mean) ^ (2 :: Int)) precs)
                / max 1 (fromIntegral n)
    n `shouldSatisfy` (> 0)
    -- Standard deviation should be at least 0.05 (not all tiles
    -- have the same precipitation)
    sqrt var `shouldSatisfy` (> 0.05)

---------------------------------------------------------------------------
-- Entry point
---------------------------------------------------------------------------

spec :: Spec
spec = beforeAll generateAndCollect $
  describe "Climate-Biome Alignment (Phase 3)" $ do
    precipPercentilesSpec
    guardrailsSpec
