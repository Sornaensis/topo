module Spec.Pipeline (spec) where

import Test.Hspec
import Data.Either (isRight)
import Data.IORef (newIORef, readIORef, modifyIORef')
import Data.Text (pack)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as U
import Topo

spec :: Spec
spec = describe "Pipeline" $ do
  it "creates a base height stage" $ do
    let stage = generateBaseHeightStage defaultGenConfig
    stageName stage `shouldBe` pack "generateBaseHeight"
    stageSeedTag stage `shouldBe` pack "generateBaseHeight"

  it "generates base height across multiple chunks" $ do
    let config = WorldConfig { wcChunkSize = 8 }
        world0 = emptyWorld config defaultHexGridMeta
        pipeline = PipelineConfig
          { pipelineSeed = 42
          , pipelineStages = [generateBaseHeightStage defaultGenConfig]
          , pipelineDisabled = mempty, pipelineSnapshots = False, pipelineOnProgress = \_ -> pure ()
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world0
    world1 <- expectPipeline result
    IntMap.size (twTerrain world1) `shouldSatisfy` (> 1)

  it "respects chunk radius in base height" $ do
    let config = WorldConfig { wcChunkSize = 8 }
        world0 = emptyWorld config defaultHexGridMeta
        gen0 = defaultGenConfig { gcWorldExtent = worldExtentSquareOrDefault 0 }
        gen1 = defaultGenConfig { gcWorldExtent = worldExtentSquareOrDefault 1 }
        pipeline0 = PipelineConfig
          { pipelineSeed = 7
          , pipelineStages = [generateBaseHeightStage gen0]
          , pipelineDisabled = mempty, pipelineSnapshots = False, pipelineOnProgress = \_ -> pure ()
          }
        pipeline1 = PipelineConfig
          { pipelineSeed = 7
          , pipelineStages = [generateBaseHeightStage gen1]
          , pipelineDisabled = mempty, pipelineSnapshots = False, pipelineOnProgress = \_ -> pure ()
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    resultA <- runPipeline pipeline0 env world0
    resultB <- runPipeline pipeline1 env world0
    worldA <- expectPipeline resultA
    worldB <- expectPipeline resultB
    IntMap.size (twTerrain worldA) `shouldBe` 1
    IntMap.size (twTerrain worldB) `shouldSatisfy` (> 1)

  it "respects rectangular world extents in base height" $ do
    let config = WorldConfig { wcChunkSize = 8 }
        world0 = emptyWorld config defaultHexGridMeta
        extent = worldExtentOrDefault 1 2
        gen = defaultGenConfig { gcWorldExtent = extent }
        pipeline = PipelineConfig
          { pipelineSeed = 7
          , pipelineStages = [generateBaseHeightStage gen]
          , pipelineDisabled = mempty, pipelineSnapshots = False, pipelineOnProgress = \_ -> pure ()
          }
        env = TopoEnv { teLogger = \_ -> pure () }
        expected = (2 * 1 + 1) * (2 * 2 + 1)
    result <- runPipeline pipeline env world0
    world1 <- expectPipeline result
    IntMap.size (twTerrain world1) `shouldBe` expected

  it "applies moisture wind-evaporation scaling" $ do
    let config = WorldConfig { wcChunkSize = 8 }
        world0 = emptyWorld config defaultHexGridMeta
        -- Use a minimal single-chunk extent to keep the test fast.
        smallGen = defaultGenConfig { gcWorldExtent = worldExtentSquareOrDefault 0 }
        climateA = defaultClimateConfig
        climateB = defaultClimateConfig
          { ccMoisture = (ccMoisture defaultClimateConfig) { moistWindEvapScale = 0 } }
        wl = hcWaterLevel defaultHydroConfig
        -- Only run terrain generation + climate — skip erosion, biomes, etc.
        mkPipeline cc = PipelineConfig
          { pipelineSeed      = 42
          , pipelineStages    = [ generateBaseHeightStage smallGen
                                , generateClimateStage cc defaultWeatherConfig wl
                                ]
          , pipelineDisabled  = mempty
          , pipelineSnapshots = False
          , pipelineOnProgress = \_ -> pure ()
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    resultA <- runPipeline (mkPipeline climateA) env world0
    resultB <- runPipeline (mkPipeline climateB) env world0
    worldA <- expectPipeline resultA
    worldB <- expectPipeline resultB
    let avg v = U.sum v / fromIntegral (max 1 (U.length v))
        cid = chunkIdFromCoord (ChunkCoord 0 0)
    case (getClimateChunk cid worldA, getClimateChunk cid worldB) of
      (Just ca, Just cb) -> avg (ccPrecipAvg cb) `shouldSatisfy` (<= avg (ccPrecipAvg ca))
      _ -> expectationFailure "missing climate chunks"

  it "validates world gen iteration counts" $ do
    let bad = defaultWorldGenConfig
          { worldTerrain = defaultTerrainConfig
              { terrainErosion = defaultErosionConfig { ecHydraulicIterations = -1 }
              }
          }
        badGlacier = defaultWorldGenConfig
          { worldTerrain = defaultTerrainConfig
              { terrainGlacier = defaultGlacierConfig { gcFlowIterations = -1 }
              }
          }
        ok = defaultWorldGenConfig
    validateWorldGenConfig bad `shouldBe` Left (WorldGenNegativeHydraulicIterations (-1))
    validateWorldGenConfig badGlacier `shouldBe` Left (WorldGenNegativeGlacierIterations (-1))
    isRight (validateWorldGenConfig ok) `shouldBe` True

  -- Phase 1: StageId canonical name round-trip
  describe "StageId" $ do
    it "round-trips all built-in stage IDs through canonical names" $ do
      let ids = allBuiltinStageIds
      mapM_ (\sid -> parseStageId (stageCanonicalName sid) `shouldBe` Just sid) ids

    it "round-trips plugin stage IDs" $ do
      let sid = StagePlugin (pack "civilization")
      parseStageId (stageCanonicalName sid) `shouldBe` Just sid

    it "rejects unknown canonical names" $ do
      parseStageId (pack "nonsense") `shouldBe` Nothing

    it "rejects empty plugin names" $ do
      parseStageId (pack "plugin:") `shouldBe` Nothing

    it "produces distinct canonical names for all built-in stages" $ do
      let names = map stageCanonicalName allBuiltinStageIds
      length (Set.fromList names) `shouldBe` length names

  -- Phase 1: Dependency closure
  describe "Dependency closure" $ do
    it "disabling PlateTerrain disables all dependents" $ do
      let disabled = disabledClosure builtinDependencies (Set.singleton StagePlateTerrain)
      -- Everything depends on PlateTerrain (directly or transitively)
      Set.member StageErosion disabled `shouldBe` True
      Set.member StageClimate disabled `shouldBe` True
      Set.member StageBiomes disabled `shouldBe` True
      Set.member StageWeather disabled `shouldBe` True
      Set.member StageConvergence disabled `shouldBe` True

    it "disabling Climate auto-disables OceanCurrents, Glacier, Biomes, Convergence, Weather" $ do
      let disabled = disabledClosure builtinDependencies (Set.singleton StageClimate)
      Set.member StageOceanCurrents disabled `shouldBe` True
      Set.member StageGlacier disabled `shouldBe` True
      Set.member StageBiomes disabled `shouldBe` True
      Set.member StageVegetationFeedback disabled `shouldBe` True
      Set.member StageConvergence disabled `shouldBe` True
      Set.member StageWeather disabled `shouldBe` True
      -- Erosion is independent of Climate
      Set.member StageErosion disabled `shouldBe` False

    it "disabling a leaf stage has no further effect" $ do
      let disabled = disabledClosure builtinDependencies (Set.singleton StageWeather)
      disabled `shouldBe` Set.singleton StageWeather

    it "disabling Convergence sub-stages auto-disables Convergence" $ do
      -- Convergence depends on Climate, Biomes, VegetationFeedback
      let disabled = disabledClosure builtinDependencies (Set.singleton StageVegetationFeedback)
      Set.member StageConvergence disabled `shouldBe` True

    it "empty seed set produces empty closure" $ do
      disabledClosure builtinDependencies Set.empty `shouldBe` Set.empty

  -- Phase 1: Stage skipping
  describe "Stage skipping" $ do
    it "skips disabled stages during pipeline execution" $ do
      let config = WorldConfig { wcChunkSize = 8 }
          world0 = emptyWorld config defaultHexGridMeta
          pipeline = defaultPipelineConfig
            { pipelineSeed = 42
            , pipelineStages = [generateBaseHeightStage defaultGenConfig]
            , pipelineDisabled = Set.singleton StageBaseHeight
            }
          env = TopoEnv { teLogger = \_ -> pure () }
      result <- runPipeline pipeline env world0
      case result of
        Left err -> expectationFailure (show err)
        Right (world, _) ->
          -- Stage was skipped, so terrain should be empty
          IntMap.size (twTerrain world) `shouldBe` 0

    it "runs non-disabled stages normally" $ do
      let config = WorldConfig { wcChunkSize = 8 }
          world0 = emptyWorld config defaultHexGridMeta
          pipeline = defaultPipelineConfig
            { pipelineSeed = 42
            , pipelineStages = [generateBaseHeightStage defaultGenConfig]
            , pipelineDisabled = Set.singleton StageErosion  -- unrelated stage
            }
          env = TopoEnv { teLogger = \_ -> pure () }
      result <- runPipeline pipeline env world0
      world1 <- expectPipeline result
      IntMap.size (twTerrain world1) `shouldSatisfy` (> 0)

  -- Phase 1: Progress callback
  describe "Progress callback" $ do
    it "fires StageStarted and StageCompleted for executed stages" $ do
      let config = WorldConfig { wcChunkSize = 8 }
          world0 = emptyWorld config defaultHexGridMeta
      ref <- newIORef []
      let pipeline = defaultPipelineConfig
            { pipelineSeed = 42
            , pipelineStages = [generateBaseHeightStage defaultGenConfig]
            , pipelineOnProgress = \p -> modifyIORef' ref (p :)
            }
          env = TopoEnv { teLogger = \_ -> pure () }
      _ <- runPipeline pipeline env world0
      events <- reverse <$> readIORef ref
      length events `shouldBe` 2
      case events of
        (e0 : e1 : _) -> do
          spStatus e0 `shouldBe` StageStarted
          spStatus e1 `shouldBe` StageCompleted
        _ -> expectationFailure "expected 2 progress events"

    it "fires StageSkipped for disabled stages" $ do
      let config = WorldConfig { wcChunkSize = 8 }
          world0 = emptyWorld config defaultHexGridMeta
      ref <- newIORef []
      let pipeline = defaultPipelineConfig
            { pipelineSeed = 42
            , pipelineStages = [generateBaseHeightStage defaultGenConfig]
            , pipelineDisabled = Set.singleton StageBaseHeight
            , pipelineOnProgress = \p -> modifyIORef' ref (p :)
            }
          env = TopoEnv { teLogger = \_ -> pure () }
      _ <- runPipeline pipeline env world0
      events <- readIORef ref
      length events `shouldBe` 1
      case events of
        (e0 : _) -> spStatus e0 `shouldBe` StageSkipped
        _        -> expectationFailure "expected 1 progress event"

    it "reports correct stage indices" $ do
      let config = WorldConfig { wcChunkSize = 8 }
          world0 = emptyWorld config defaultHexGridMeta
          gen = defaultGenConfig { gcWorldExtent = worldExtentSquareOrDefault 0 }
          stage1 = generateBaseHeightStage gen
          stage2 = generateBaseHeightStage gen  -- duplicate for index test
      ref <- newIORef []
      let pipeline = defaultPipelineConfig
            { pipelineSeed = 42
            , pipelineStages = [stage1, stage2]
            , pipelineOnProgress = \p -> modifyIORef' ref (p :)
            }
          env = TopoEnv { teLogger = \_ -> pure () }
      _ <- runPipeline pipeline env world0
      events <- reverse <$> readIORef ref
      -- 2 stages × 2 events each = 4 events
      length events `shouldBe` 4
      case events of
        (e0 : _ : e2 : _) -> do
          spStageIndex e0 `shouldBe` 0
          spStageCount e0 `shouldBe` 2
          spStageIndex e2 `shouldBe` 1
          spStageCount e2 `shouldBe` 2
        _ -> expectationFailure "expected 4 progress events"

  -- Phase 1: Stage identity on constructed stages
  describe "Stage identity" $ do
    it "generateBaseHeightStage has StageBaseHeight id" $ do
      stageId (generateBaseHeightStage defaultGenConfig) `shouldBe` StageBaseHeight

    it "generatePlateTerrainStage has StagePlateTerrain id" $ do
      stageId (generatePlateTerrainStage defaultGenConfig defaultTectonicsConfig) `shouldBe` StagePlateTerrain

    it "buildFullPipelineConfig stages have correct IDs" $ do
      let config = WorldConfig { wcChunkSize = 8 }
          pipeline = buildFullPipelineConfig defaultWorldGenConfig config 42
          ids = map stageId (pipelineStages pipeline)
      -- First stage should be plate terrain
      case ids of
        (firstId : _) -> firstId `shouldBe` StagePlateTerrain
        []            -> expectationFailure "empty pipeline"
      -- Weather should be the last stage
      case reverse ids of
        (lastId : _) -> lastId `shouldBe` StageWeather
        []           -> expectationFailure "empty pipeline"
      -- All built-in IDs (excluding convergence pseudo-stage and plugin) should appear
      StageBiomes `elem` ids `shouldBe` True
      StageClimate `elem` ids `shouldBe` True
      StageErosion `elem` ids `shouldBe` True

expectPipeline :: Either PipelineError (TerrainWorld, [PipelineSnapshot]) -> IO TerrainWorld
expectPipeline result =
  case result of
    Left err -> expectationFailure (show err) >> pure (emptyWorld (WorldConfig { wcChunkSize = 1 }) defaultHexGridMeta)
    Right (world, _) -> pure world
