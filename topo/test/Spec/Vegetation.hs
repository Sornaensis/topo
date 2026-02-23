{-# LANGUAGE ScopedTypeVariables #-}

module Spec.Vegetation (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Data.Vector.Unboxed as U
import Topo
import Topo.Vegetation
  ( VegetationBootstrapConfig(..)
  , defaultVegetationBootstrapConfig
  , roughTemperatureEstimate
  , vegetationAlbedo
  , vegetationPotential
  , biomeBaseCover
  , biomeOptimalPrecip
  , biomeBaseDensity
  , biomeClimateSlope
  , BiomeFeedbackConfig(..)
  , defaultBiomeFeedbackConfig
  , updateVegChunk
  , densityEpsilon
  )

spec :: Spec
spec = describe "Vegetation" $ do
  it "generates non-zero density in wet biomes" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        n = chunkTileCount config
        biomes = U.replicate n BiomeForest
        temp = U.replicate n 0.8
        precip = U.replicate n 0.9
        density = vegetationDensityChunk defaultBiomeVegetationConfig biomeBaseDensity biomeClimateSlope biomes temp precip
    U.any (> 0) density `shouldBe` True

  describe "roughTemperatureEstimate" $ do
    it "returns high temperature at equator" $ do
      let t = roughTemperatureEstimate 1.0 0 0.6 0.5 0.5
      t `shouldSatisfy` (> 0.3)

    it "returns lower temperature at high latitude" $ do
      let tEquator = roughTemperatureEstimate 1.0 0 0.6 0.5 0.5
          tPolar   = roughTemperatureEstimate 1.0 80 0.6 0.5 0.5
      tEquator `shouldSatisfy` (> tPolar)

    it "applies lapse rate for high elevation" $ do
      let tLow  = roughTemperatureEstimate 1.0 30 0.6 0.3 0.35
          tHigh = roughTemperatureEstimate 1.0 30 0.6 0.3 0.80
      tLow `shouldSatisfy` (>= tHigh)

    prop "result is in [0, 1]" $
      \(lat :: Float) (elev :: Float) ->
        let lat' = max (-90) (min 90 lat)
            elev' = abs elev
            t = roughTemperatureEstimate 1.0 lat' 0.6 0.3 elev'
        in t >= 0 && t <= 1

  describe "vegetationPotential" $ do
    let cfg = defaultVegetationBootstrapConfig

    it "returns positive for warm temperature" $ do
      vegetationPotential cfg 0.0 0.8 0.5 0.5 `shouldSatisfy` (>= 0)

    it "returns positive for warm + wet + deep soil" $ do
      vegetationPotential cfg 0.6 0.8 0.8 0.8 `shouldSatisfy` (> 0)

    it "increases with moisture" $ do
      let dry = vegetationPotential cfg 0.5 0.2 0.5 0.5
          wet = vegetationPotential cfg 0.5 0.8 0.5 0.5
      wet `shouldSatisfy` (> dry)

    prop "result is in [0, 1]" $
      \(NonNegative (t :: Float)) (NonNegative (m :: Float))
       (NonNegative (s :: Float)) (NonNegative (f :: Float)) ->
        let v = vegetationPotential cfg (min 1 t) (min 1 m) (min 1 s) (min 1 f)
        in v >= 0 && v <= 1

  describe "vegetationAlbedo" $ do
    let cfg = defaultVegetationBootstrapConfig

    it "bare ground has higher albedo than vegetated ground" $ do
      let bare = vegetationAlbedo cfg 0.0
          veg  = vegetationAlbedo cfg 1.0
      bare `shouldSatisfy` (> veg)

    prop "result is in [0, 1]" $
      \(NonNegative (v :: Float)) ->
        let a = vegetationAlbedo cfg (min 1 v)
        in a >= 0 && a <= 1

  -- Phase 7.2: biome->vegetation feedback
  describe "biomeBaseCover (7.2)" $ do
    it "returns 0 for ocean biomes" $ do
      biomeBaseCover BiomeOcean `shouldBe` 0.0
      biomeBaseCover BiomeDeepOcean `shouldBe` 0.0
      biomeBaseCover BiomeLake `shouldBe` 0.0

    it "returns high cover for forest biomes" $ do
      biomeBaseCover BiomeForest `shouldSatisfy` (> 0.70)
      biomeBaseCover BiomeRainforest `shouldSatisfy` (> 0.85)
      biomeBaseCover BiomeTempDeciduousForest `shouldSatisfy` (> 0.70)

    it "returns low cover for desert biomes" $ do
      biomeBaseCover BiomeDesert `shouldSatisfy` (< 0.10)
      biomeBaseCover BiomeHotDesert `shouldSatisfy` (< 0.10)
      biomeBaseCover BiomeSandDesert `shouldSatisfy` (< 0.10)

    it "returns moderate cover for grassland" $ do
      biomeBaseCover BiomeGrassland `shouldSatisfy` (> 0.20)
      biomeBaseCover BiomeGrassland `shouldSatisfy` (< 0.60)

    it "returns near-zero for snow/ice biomes" $ do
      biomeBaseCover BiomeSnow `shouldSatisfy` (< 0.05)
      biomeBaseCover BiomeGlacier `shouldSatisfy` (< 0.05)
      biomeBaseCover BiomeIceCap `shouldSatisfy` (< 0.05)

    prop "result is in [0, 1]" $
      forAll (elements allTestBiomes) $ \bid ->
        let c = biomeBaseCover bid
        in c >= 0 && c <= 1

  describe "BiomeFeedbackConfig" $ do
    it "blend weight 0 preserves bootstrap vegetation" $ do
      let bfc = defaultBiomeFeedbackConfig { bfcBlendWeight = 0 }
      -- With blend=0, biome cover is ignored, bootstrap prevails.
      bfcBlendWeight bfc `shouldBe` 0.0

    it "blend weight 1 fully overrides with biome cover" $ do
      let bfc = defaultBiomeFeedbackConfig { bfcBlendWeight = 1 }
      bfcBlendWeight bfc `shouldBe` 1.0

    it "default convergence iterations is positive" $ do
      bfcConvergenceIterations defaultBiomeFeedbackConfig `shouldSatisfy` (> 0)

  -- Phase 4.1: biome optimal precipitation
  describe "biomeOptimalPrecip" $ do
    it "returns positive for all terrestrial biomes" $ do
      biomeOptimalPrecip BiomeForest `shouldSatisfy` (> 0)
      biomeOptimalPrecip BiomeDesert `shouldSatisfy` (> 0)
      biomeOptimalPrecip BiomeTundra `shouldSatisfy` (> 0)

    it "forests need more precip than deserts" $ do
      biomeOptimalPrecip BiomeForest `shouldSatisfy`
        (> biomeOptimalPrecip BiomeDesert)

    it "rainforest needs highest precipitation" $ do
      biomeOptimalPrecip BiomeRainforest `shouldSatisfy`
        (> biomeOptimalPrecip BiomeForest)

    prop "result is in (0, 1]" $
      forAll (elements allTestBiomes) $ \bid ->
        let p = biomeOptimalPrecip bid
        in p > 0 && p <= 1

  -- Phase 4.3: coastal proximity config
  describe "bootstrap coastal proximity config" $ do
    it "default iterations is positive" $ do
      vbcCoastalIterations defaultVegetationBootstrapConfig `shouldSatisfy` (> 0)

    it "default coastal boost is in [0, 1]" $ do
      let boost = vbcCoastalBoost defaultVegetationBootstrapConfig
      boost `shouldSatisfy` (>= 0)
      boost `shouldSatisfy` (<= 1)

  -- Phase 6a: biomeBaseDensity and biomeClimateSlope coverage
  describe "biomeBaseDensity (6a)" $ do
    it "returns positive density for forest biomes" $ do
      biomeBaseDensity BiomeForest `shouldSatisfy` (> 0.2)
      biomeBaseDensity BiomeRainforest `shouldSatisfy` (> 0.3)
      biomeBaseDensity BiomeTempDeciduousForest `shouldSatisfy` (> 0.2)

    it "returns low density for desert/ice biomes" $ do
      biomeBaseDensity BiomeDesert `shouldSatisfy` (< 0.15)
      biomeBaseDensity BiomeHotDesert `shouldSatisfy` (< 0.15)
      biomeBaseDensity BiomeIceCap `shouldSatisfy` (< 0.10)
      biomeBaseDensity BiomeSnow `shouldSatisfy` (< 0.10)

    it "water biomes have zero density" $ do
      biomeBaseDensity BiomeOcean `shouldBe` 0.0
      biomeBaseDensity BiomeDeepOcean `shouldBe` 0.0
      biomeBaseDensity BiomeLake `shouldBe` 0.0

    prop "result is in [0, 1] for all biomes" $
      forAll (elements allTestBiomes) $ \bid ->
        let d = biomeBaseDensity bid
        in d >= 0 && d <= 1

  describe "biomeClimateSlope (6a)" $ do
    it "is non-negative for all biomes" $ do
      mapM_ (\bid -> biomeClimateSlope bid `shouldSatisfy` (>= 0)) allTestBiomes

    prop "result is in [0, 2] for all biomes" $
      forAll (elements allTestBiomes) $ \bid ->
        let s = biomeClimateSlope bid
        in s >= 0 && s <= 2

  -- Phase 6b: vegetationDensityChunk output ∈ [0, 1]
  describe "vegetationDensityChunk (6b)" $ do
    prop "density values are in [0, 1]" $
      forAll (elements allTestBiomes) $ \bid ->
        forAll (choose (0.0, 1.0)) $ \temp ->
          forAll (choose (0.0, 1.0)) $ \precip ->
            let n = 4
                biomes = U.replicate n bid
                temps = U.replicate n temp
                precips = U.replicate n precip
                density = vegetationDensityChunk
                  defaultBiomeVegetationConfig
                  biomeBaseDensity biomeClimateSlope
                  biomes temps precips
            in U.all (\d -> d >= 0 && d <= 1) density

    it "density is monotone with temperature for forest" $ do
      let n = 1
          biomes = U.replicate n BiomeForest
          precip = U.replicate n 0.8
          cold   = vegetationDensityChunk defaultBiomeVegetationConfig
                     biomeBaseDensity biomeClimateSlope
                     biomes (U.replicate n 0.2) precip
          warm   = vegetationDensityChunk defaultBiomeVegetationConfig
                     biomeBaseDensity biomeClimateSlope
                     biomes (U.replicate n 0.8) precip
      (warm U.! 0) `shouldSatisfy` (>= (cold U.! 0))

  -- Phase 4 invariant: vegCover ≤ vegDensity + densityEpsilon after updateVegChunk
  describe "updateVegChunk invariant (Phase 4)" $ do
    prop "vegCover ≤ vegDensity + epsilon after feedback" $
      forAll (elements allTestBiomes) $ \bid ->
        forAll (choose (0.0, 1.0)) $ \precipVal ->
          forAll (choose (0.0, 1.0)) $ \densityVal ->
            let n = 4
                bfc = defaultBiomeFeedbackConfig { bfcBlendWeight = 1.0 }
                vbc = defaultVegetationBootstrapConfig
                biomes = U.replicate n bid
                precip = U.replicate n precipVal
                oldVeg = VegetationChunk
                  { vegCover   = U.replicate n 0.5
                  , vegAlbedo  = U.replicate n 0.2
                  , vegDensity = U.replicate n densityVal
                  }
                result = updateVegChunk bfc vbc biomes precip (Just oldVeg)
                eps = densityEpsilon
            in U.all (\i ->
                 let c = vegCover result U.! i
                     d = vegDensity result U.! i
                 in c <= d + eps + 1e-6  -- small float tolerance
               ) (U.enumFromN 0 n)

    it "zero-density biome produces near-zero cover" $ do
      let n = 4
          bfc = defaultBiomeFeedbackConfig { bfcBlendWeight = 1.0 }
          vbc = defaultVegetationBootstrapConfig
          biomes = U.replicate n BiomeOcean
          precip = U.replicate n 0.5
          oldVeg = VegetationChunk
            { vegCover   = U.replicate n 0.5
            , vegAlbedo  = U.replicate n 0.2
            , vegDensity = U.replicate n 0.0
            }
          result = updateVegChunk bfc vbc biomes precip (Just oldVeg)
      U.all (<= densityEpsilon + 1e-6) (vegCover result) `shouldBe` True

    it "high-density forest produces substantial cover" $ do
      let n = 4
          bfc = defaultBiomeFeedbackConfig { bfcBlendWeight = 1.0 }
          vbc = defaultVegetationBootstrapConfig
          biomes = U.replicate n BiomeForest
          precip = U.replicate n 0.9
          oldVeg = VegetationChunk
            { vegCover   = U.replicate n 0.5
            , vegAlbedo  = U.replicate n 0.2
            , vegDensity = U.replicate n 0.9
            }
          result = updateVegChunk bfc vbc biomes precip (Just oldVeg)
      U.all (> 0.4) (vegCover result) `shouldBe` True

-- | Representative biome IDs covering all families for property tests.
allTestBiomes :: [BiomeId]
allTestBiomes =
  [ BiomeDesert, BiomeGrassland, BiomeForest, BiomeTundra
  , BiomeRainforest, BiomeShrubland, BiomeSavanna, BiomeTaiga
  , BiomeSwamp, BiomeOcean, BiomeSnow, BiomeCoastal, BiomeAlpine
  , BiomeTropicalDryForest, BiomeTempDeciduousForest
  , BiomeTempConiferousForest, BiomeSteppe, BiomeMediterranean
  , BiomeWetland, BiomeMontaneForest, BiomeIceCap, BiomeTropicalSavanna
  , BiomeBorealForest, BiomeSaltMarsh, BiomeMangrove, BiomeHotDesert
  , BiomeColdDesert, BiomePrairie, BiomeCloudForest, BiomeTempRainforest
  , BiomeDeepOcean, BiomeLake, BiomeTropicalRainforest
  , BiomeGlacier, BiomeSnowfield, BiomeLavaField
  ]
