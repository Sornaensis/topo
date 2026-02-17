{-# LANGUAGE OverloadedStrings #-}
module Spec.Biome (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Data.List (nub)
import qualified Data.Text as Text
import Data.Word (Word16)
import qualified Data.Vector.Unboxed as U
import Topo

-- | Ocean sub-biome IDs produced by the refinement pass.
--   Tests must account for the refine stage converting family-level
--   'BiomeOcean' into depth-discriminated sub-biomes.
isOceanFamily :: BiomeId -> Bool
isOceanFamily bid =
     bid == BiomeOcean
  || bid == BiomeDeepOcean
  || bid == BiomeShallowSea
  || bid == BiomeCoralReef

spec :: Spec
spec = describe "Biome" $ do
  it "classifies biomes into terrain flags" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        world0 = emptyWorld config defaultHexGridMeta
        n = chunkTileCount config
        terrain = (emptyTerrainChunk config) { tcElevation = U.replicate n 0.2 }
        climate = ClimateChunk
          { ccTempAvg = U.replicate n 0.8
          , ccPrecipAvg = U.replicate n 0.9
          , ccWindDirAvg = U.replicate n 0
          , ccWindSpdAvg = U.replicate n 0
          , ccHumidityAvg = U.replicate n 0
          , ccTempRange = U.replicate n 0
          , ccPrecipSeasonality = U.replicate n 0
          }
        world1 = setClimateChunk (ChunkId 0) climate
                  (setTerrainChunk (ChunkId 0) terrain world0)
        pipeline = PipelineConfig
          { pipelineSeed = 1
          , pipelineStages = [classifyBiomesStage defaultBiomeConfig 0.5]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world1
    world2 <- expectPipeline result
    case getTerrainChunk (ChunkId 0) world2 of
      Nothing -> expectationFailure "missing terrain chunk"
      Just chunk -> do
        let flags = tcFlags chunk
        U.any (/= BiomeDesert) flags `shouldBe` True

  it "assigns ocean and temperature-primary mountain bands" $ do
    let config = WorldConfig { wcChunkSize = 2 }
        world0 = emptyWorld config defaultHexGridMeta
        n = chunkTileCount config
        terrain = (emptyTerrainChunk config) { tcElevation = U.fromList [-0.1, 0.02, 0.8, 0.95] }
        -- Tile 3 (elev=0.95, hASL=0.85) needs temp < btSnowMaxTemp (0.20)
        -- to trigger the temperature-primary Snow guard.
        climate = ClimateChunk
          { ccTempAvg = U.fromList [0.5, 0.5, 0.5, 0.10]
          , ccPrecipAvg = U.replicate n 0.5
          , ccWindDirAvg = U.replicate n 0
          , ccWindSpdAvg = U.replicate n 0
          , ccHumidityAvg = U.replicate n 0
          , ccTempRange = U.replicate n 0
          , ccPrecipSeasonality = U.replicate n 0
          }
        world1 = setClimateChunk (ChunkId 0) climate
                  (setTerrainChunk (ChunkId 0) terrain world0)
        pipeline = PipelineConfig
          { pipelineSeed = 1
          , pipelineStages = [classifyBiomesStage defaultBiomeConfig 0.1]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world1
    world2 <- expectPipeline result
    case getTerrainChunk (ChunkId 0) world2 of
      Nothing -> expectationFailure "missing terrain chunk"
      Just chunk -> do
        let flags = tcFlags chunk
        -- Tiles 0 & 1 are below water level; refinement pass converts
        -- BiomeOcean into depth-based sub-biomes (deep/shallow/coral).
        flags U.! 0 `shouldSatisfy` isOceanFamily
        flags U.! 1 `shouldSatisfy` isOceanFamily
        flags U.! 3 `shouldBe` BiomeSnow

  it "treats low elevations as ocean when water level rises" $ do
    let config = WorldConfig { wcChunkSize = 1 }
        world0 = emptyWorld config defaultHexGridMeta
        terrain = (emptyTerrainChunk config) { tcElevation = U.fromList [0.04] }
        climate = ClimateChunk
          { ccTempAvg = U.replicate 1 0.5
          , ccPrecipAvg = U.replicate 1 0.5
          , ccWindDirAvg = U.replicate 1 0
          , ccWindSpdAvg = U.replicate 1 0
          , ccHumidityAvg = U.replicate 1 0
          , ccTempRange = U.replicate 1 0
          , ccPrecipSeasonality = U.replicate 1 0
          }
        world1 = setClimateChunk (ChunkId 0) climate
                  (setTerrainChunk (ChunkId 0) terrain world0)
        pipeline = PipelineConfig
          { pipelineSeed = 1
          , pipelineStages = [classifyBiomesStage defaultBiomeConfig 0.05]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world1
    world2 <- expectPipeline result
    case getTerrainChunk (ChunkId 0) world2 of
      Nothing -> expectationFailure "missing terrain chunk"
      -- Refinement converts shallow ocean tile to BiomeShallowSea.
      Just chunk -> tcFlags chunk U.! 0 `shouldSatisfy` isOceanFamily

  it "boosts vegetation fertility with volcanic ash" $ do
    let config = WorldConfig { wcChunkSize = 2 }
        world0 = emptyWorld config defaultHexGridMeta
        n = chunkTileCount config
        terrain = (emptyTerrainChunk config) { tcElevation = U.replicate n 0.3 }
        climate = ClimateChunk
          { ccTempAvg = U.replicate n 0.7
          , ccPrecipAvg = U.replicate n 0.8
          , ccWindDirAvg = U.replicate n 0
          , ccWindSpdAvg = U.replicate n 0
          , ccHumidityAvg = U.replicate n 0
          , ccTempRange = U.replicate n 0
          , ccPrecipSeasonality = U.replicate n 0
          }
        -- Ash potential kept below the volcanic-refine overlay threshold
        -- (default 0.40) so that the biome is NOT overridden to
        -- BiomeVolcanicAshPlain; the vegetation ash-boost still applies.
        volcanism = (emptyVolcanismChunk config)
          { vcAshPotential = U.replicate n 0.35
          , vcLavaPotential = U.replicate n 0
          }
        biomeCfg = defaultBiomeConfig
          { bcVolcanicAshBoost = 0.4
          , bcVolcanicLavaPenalty = 0
          }
        worldBase = setClimateChunk (ChunkId 0) climate
          (setTerrainChunk (ChunkId 0) terrain world0)
        worldAsh = setVolcanismChunk (ChunkId 0) volcanism worldBase
        pipeline = PipelineConfig
          { pipelineSeed = 1
          , pipelineStages = [classifyBiomesStage biomeCfg 0.1]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    baseResult <- runPipeline pipeline env worldBase
    ashResult <- runPipeline pipeline env worldAsh
    worldBase' <- expectPipeline baseResult
    worldAsh' <- expectPipeline ashResult
    -- classifyBiomesStage writes the ash-boosted vegetation density into
    -- VegetationChunk.vegDensity (NOT tcFertility, which stays soil-derived).
    case ( getVegetationChunk (ChunkId 0) worldBase'
         , getVegetationChunk (ChunkId 0) worldAsh'
         ) of
      (Just baseVeg, Just ashVeg) ->
        vegDensity ashVeg U.! 0 `shouldSatisfy` (> vegDensity baseVeg U.! 0)
      _ -> expectationFailure "missing vegetation chunk"

  describe "biomeDisplayName" $ do
    -- Helper: safely construct a BiomeId from a known-valid code
    let mkBid code = case biomeIdFromCode code of
          Right bid -> bid
          Left  _   -> error $ "test setup: unexpected unknown code " ++ show code

    let knownCodes :: [Word16]
        knownCodes = [0..8] ++ [10..65]

    it "returns a non-empty name for every known biome code" $
      mapM_ (\code ->
        let name = biomeDisplayName (mkBid code)
        in name `shouldSatisfy` (not . Text.null)
      ) knownCodes

    it "returns distinct names for distinct known codes" $ do
      let names = map (\code -> biomeDisplayName (mkBid code)) knownCodes
      length names `shouldBe` length (nub names)

    it "maps all primary biome patterns to expected names" $ do
      biomeDisplayName BiomeDesert `shouldBe` "Desert"
      biomeDisplayName BiomeGrassland `shouldBe` "Grassland"
      biomeDisplayName BiomeForest `shouldBe` "Forest"
      biomeDisplayName BiomeTundra `shouldBe` "Tundra"
      biomeDisplayName BiomeRainforest `shouldBe` "Rainforest"
      biomeDisplayName BiomeShrubland `shouldBe` "Shrubland"
      biomeDisplayName BiomeSavanna `shouldBe` "Savanna"
      biomeDisplayName BiomeTaiga `shouldBe` "Taiga"
      biomeDisplayName BiomeSwamp `shouldBe` "Swamp"
      biomeDisplayName BiomeOcean `shouldBe` "Ocean"
      biomeDisplayName BiomeSnow `shouldBe` "Snow"
      biomeDisplayName BiomeCoastal `shouldBe` "Coastal"
      biomeDisplayName BiomeAlpine `shouldBe` "Alpine"

  ---------------------------------------------------------------------------
  -- Phase 5: Revised rule table + weighted fallback properties
  ---------------------------------------------------------------------------

  describe "revised rule table" $ do
    -- Helper that classifies a land tile with no special overrides.
    let classify' t p = classifyBiome
          defaultBiomeRules defaultBiomeThresholds
          0.0           -- waterLevel
          WaterDry      -- wbt
          WaterDry      -- adjWbt
          t p
          0.3           -- elevation (land, no alpine/snow override)
          0.05          -- slope (low, no cliff override)
          0.0           -- relief
          0.5           -- moisture (moderate)
          FormFlat      -- terrain form (no override)
          0.5           -- humidity (moderate)
          0.10          -- tempRange (low, below grassland reclassification)
          0.10          -- precipSeasonality (low, below savanna reclassification)

    it "cold steppe at T=0.50, P=0.05 (not desert)" $ do
      let coldSteppe = classify' 0.50 0.05
      coldSteppe `shouldBe` BiomeGrassland

    it "desert at T=0.58, P=0.05 (above cold steppe band)" $ do
      let coolDesert = classify' 0.58 0.05
      coolDesert `shouldBe` BiomeDesert

    it "not desert at P = 0.12 for warm tile" $ do
      let hotModest = classify' 0.50 0.12
      hotModest `shouldNotBe` BiomeDesert

    it "cool grassland at T=0.55, P=0.20" $ do
      let coolGrass = classify' 0.55 0.20
      coolGrass `shouldBe` BiomeGrassland

    it "rainforest at T=0.85, P=0.80" $ do
      let tropWet = classify' 0.85 0.80
      tropWet `shouldBe` BiomeRainforest

    -- Temperature-primary mountain guards intercept T < 0.35 at
    -- elevation 0.3 (hASL = 0.3 > btSnowMinASL), so we test the
    -- Tundra rule-table band at its warm edge where guards don't fire.
    it "tundra at T=0.35, P=0.20 (warm-edge of tundra band)" $ do
      let polar = classify' 0.35 0.20
      polar `shouldBe` BiomeTundra

    it "temperate forest at T=0.50, P=0.60" $ do
      let tempForest = classify' 0.50 0.60
      tempForest `shouldBe` BiomeForest

    prop "every (T,P) at 0.05 increments matches a rule or fallback (never partial)" $
      \(NonNegative t', NonNegative p') ->
        let t = fromIntegral ((t' :: Int) `mod` 20) * 0.05
            p = fromIntegral ((p' :: Int) `mod` 20) * 0.05
            bid = classify' t p
        in bid `seq` True  -- evaluates without error

    prop "desert only appears when P < 0.10" $
      forAll (choose (0.10, 1.0)) $ \p ->
      forAll (choose (0.0, 1.0))  $ \t ->
        classify' t p /= BiomeDesert

    prop "weighted fallback never selects desert when P > 0.12" $
      forAll (choose (0.12, 1.0)) $ \p ->
      forAll (choose (0.0, 1.0))  $ \t ->
        classify' t p /= BiomeDesert

expectPipeline :: Either PipelineError (TerrainWorld, [PipelineSnapshot]) -> IO TerrainWorld
expectPipeline result =
  case result of
    Left err -> expectationFailure (show err) >> pure (emptyWorld (WorldConfig { wcChunkSize = 1 }) defaultHexGridMeta)
    Right (world, _) -> pure world
