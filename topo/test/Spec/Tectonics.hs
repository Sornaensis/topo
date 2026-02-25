module Spec.Tectonics (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import qualified Data.Vector.Unboxed as U
import qualified Data.List as List
import Topo
import Topo.Noise (directionalRidge2DAniso)

spec :: Spec
spec = describe "Tectonics" $ do
  it "marks plate ids in flags" $ do
    let config = WorldConfig { wcChunkSize = 8 }
        world0 = emptyWorld config defaultHexGridMeta
        stages =
          [ generatePlateTerrainStage defaultGenConfig defaultTectonicsConfig ]
        pipeline = PipelineConfig
          { pipelineSeed = 123
          , pipelineStages = stages
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world0
    world1 <- expectPipeline result
    case getTerrainChunk (ChunkId 0) world1 of
      Nothing -> expectationFailure "missing chunk"
      Just chunk -> do
        let ids = tcPlateId chunk
        U.any (/= 0) ids `shouldBe` True

  it "produces multiple plate ids in a chunk" $ do
    let config = WorldConfig { wcChunkSize = 16 }
        world0 = emptyWorld config defaultHexGridMeta
        stages =
          [ generatePlateTerrainStage defaultGenConfig defaultTectonicsConfig ]
        pipeline = PipelineConfig
          { pipelineSeed = 987
          , pipelineStages = stages
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world0
    world1 <- expectPipeline result
    case getTerrainChunk (ChunkId 0) world1 of
      Nothing -> expectationFailure "missing chunk"
      Just chunk -> do
        let ids = U.toList (tcPlateId chunk)
            uniqueCount = length (List.nub ids)
        uniqueCount `shouldSatisfy` (> 1)

  it "marks plate boundary types" $ do
    let config = WorldConfig { wcChunkSize = 16 }
        world0 = emptyWorld config defaultHexGridMeta
        stages =
          [ generatePlateTerrainStage defaultGenConfig defaultTectonicsConfig ]
        pipeline = PipelineConfig
          { pipelineSeed = 456
          , pipelineStages = stages
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world0
    world1 <- expectPipeline result
    case getTerrainChunk (ChunkId 0) world1 of
      Nothing -> expectationFailure "missing chunk"
      Just chunk -> do
        let boundaries = tcPlateBoundary chunk
        U.any (/= PlateBoundaryNone) boundaries `shouldBe` True

  it "keeps boundary density in a reasonable range" $ do
    let config = WorldConfig { wcChunkSize = 24 }
        world0 = emptyWorld config defaultHexGridMeta
        stages =
          [ generatePlateTerrainStage defaultGenConfig defaultTectonicsConfig ]
        pipeline = PipelineConfig
          { pipelineSeed = 321
          , pipelineStages = stages
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world0
    world1 <- expectPipeline result
    case getTerrainChunk (ChunkId 0) world1 of
      Nothing -> expectationFailure "missing chunk"
      Just chunk -> do
        let boundaries = tcPlateBoundary chunk
            total = U.length boundaries
            boundaryCount = U.length (U.filter (/= PlateBoundaryNone) boundaries)
            ratio = fromIntegral boundaryCount / max 1 (fromIntegral total)
        ratio `shouldSatisfy` (> 0.01)

  it "produces plate height variation" $ do
    let config = WorldConfig { wcChunkSize = 16 }
        world0 = emptyWorld config defaultHexGridMeta
        stages =
          [ generatePlateTerrainStage defaultGenConfig defaultTectonicsConfig ]
        pipeline = PipelineConfig
          { pipelineSeed = 654
          , pipelineStages = stages
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world0
    world1 <- expectPipeline result
    case getTerrainChunk (ChunkId 0) world1 of
      Nothing -> expectationFailure "missing chunk"
      Just chunk -> do
        let heights = tcPlateHeight chunk
            minH = U.minimum heights
            maxH = U.maximum heights
        maxH - minH `shouldSatisfy` (> 0)

  it "produces plate hardness variation" $ do
    let config = WorldConfig { wcChunkSize = 16 }
        world0 = emptyWorld config defaultHexGridMeta
        stages =
          [ generatePlateTerrainStage defaultGenConfig defaultTectonicsConfig ]
        pipeline = PipelineConfig
          { pipelineSeed = 222
          , pipelineStages = stages
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world0
    world1 <- expectPipeline result
    case getTerrainChunk (ChunkId 0) world1 of
      Nothing -> expectationFailure "missing chunk"
      Just chunk -> do
        let hardness = tcPlateHardness chunk
            minH = U.minimum hardness
            maxH = U.maximum hardness
        maxH - minH `shouldSatisfy` (> 0)

  it "stores plate crust and age" $ do
    let config = WorldConfig { wcChunkSize = 16 }
        world0 = emptyWorld config defaultHexGridMeta
        stages =
          [ generatePlateTerrainStage defaultGenConfig defaultTectonicsConfig ]
        pipeline = PipelineConfig
          { pipelineSeed = 777
          , pipelineStages = stages
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world0
    world1 <- expectPipeline result
    case getTerrainChunk (ChunkId 0) world1 of
      Nothing -> expectationFailure "missing chunk"
      Just chunk -> do
        let crust = tcPlateCrust chunk
            ages = tcPlateAge chunk
        U.any (\v -> v == 0 || v == 1) crust `shouldBe` True
        U.all (\v -> v >= 0 && v <= 1) ages `shouldBe` True

  it "stores plate velocity vectors" $ do
    let config = WorldConfig { wcChunkSize = 16 }
        world0 = emptyWorld config defaultHexGridMeta
        stages =
          [ generatePlateTerrainStage defaultGenConfig defaultTectonicsConfig ]
        pipeline = PipelineConfig
          { pipelineSeed = 888
          , pipelineStages = stages
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world0
    world1 <- expectPipeline result
    case getTerrainChunk (ChunkId 0) world1 of
      Nothing -> expectationFailure "missing chunk"
      Just chunk -> do
        let vx = tcPlateVelX chunk
            vy = tcPlateVelY chunk
            maxSpeed = U.maximum (U.zipWith (\x y -> abs x + abs y) vx vy)
        maxSpeed `shouldSatisfy` (> 0)

  -- ----------------------------------------------------------------
  -- Rift profile property tests
  -- ----------------------------------------------------------------

  describe "riftProfile" $ do
    prop "returns 0 when strength is 0" $ \(NonNegative floorW) (NonNegative shoulderW) ->
      let cfg = defaultTectonicsConfig
            { tcRiftFloorWidth = min 0.9 floorW
            , tcRiftShoulderWidth = min 0.9 shoulderW
            }
      in riftProfile cfg 0 === 0

    prop "returns -1 at boundary centre (strength = 1) when floorWidth > 0" $
      let cfg = defaultTectonicsConfig
      in riftProfile cfg 1.0 === (-1.0)

    prop "result is bounded for all valid strengths" $ \(NonNegative s0) ->
      let s = min 1.0 (s0 :: Float)
          cfg = defaultTectonicsConfig
          result = riftProfile cfg s
          maxShoulder = tcRiftShoulderHeight cfg / max 1e-6 (tcRiftDepth cfg)
      in counterexample ("riftProfile " ++ show s ++ " = " ++ show result) $
           result >= -1.0 .&&. result <= maxShoulder + 0.01

    prop "never exceeds shoulder height (normalised)" $ \(NonNegative s0) ->
      let s = min 1.0 (s0 :: Float)
          cfg = defaultTectonicsConfig
          result = riftProfile cfg s
          shoulderNorm = tcRiftShoulderHeight cfg / max 1e-6 (tcRiftDepth cfg)
      in result <= shoulderNorm + 1e-5

    it "shoulder zone produces positive values" $ do
      let cfg = defaultTectonicsConfig
          -- strength near 0 but > 0 maps to d near 1 → shoulder zone
          shoulderResults = [ riftProfile cfg s
                            | s <- [0.01, 0.05, 0.1, 0.15]
                            ]
      any (> 0) shoulderResults `shouldBe` True

    it "floor zone is flat at -1" $ do
      let cfg = defaultTectonicsConfig
          -- strength close to 1 → d close to 0 → floor zone
          floorResults = [ riftProfile cfg s | s <- [0.95, 0.97, 0.99, 1.0] ]
      all (== (-1.0)) floorResults `shouldBe` True

  -- ----------------------------------------------------------------
  -- Boundary tangent property tests
  -- ----------------------------------------------------------------

  describe "boundaryTangent" $ do
    it "is perpendicular to the centre-to-centre direction" $ do
      let infoA = makePlateInfo (0, 0)
          infoB = makePlateInfo (10, 0)
          (tx, ty) = boundaryTangent infoA infoB
          -- Normal is (1, 0), tangent should be (0, ±1)
      abs tx `shouldSatisfy` (< 1e-5)
      abs ty `shouldSatisfy` (> 0.99)

    it "returns unit-length vector" $ do
      let infoA = makePlateInfo (3, 7)
          infoB = makePlateInfo (15, 22)
          (tx, ty) = boundaryTangent infoA infoB
          len = sqrt (tx * tx + ty * ty)
      abs (len - 1.0) `shouldSatisfy` (< 1e-4)

    it "returns fallback when centres coincide" $ do
      let infoA = makePlateInfo (5, 5)
          infoB = makePlateInfo (5, 5)
          (tx, ty) = boundaryTangent infoA infoB
      -- Fallback is (0, 1)
      abs tx `shouldSatisfy` (< 1e-5)
      abs (ty - 1) `shouldSatisfy` (< 1e-5)

  -- ----------------------------------------------------------------
  -- Anisotropic noise elongation test
  -- ----------------------------------------------------------------

  describe "directionalRidge2DAniso elongation" $ do
    it "produces more variation across the direction than along it" $ do
      -- Sample noise along the direction (Y axis) and across it (X axis).
      -- With elongation = 4, features should be ~4× wider along than across,
      -- meaning the across-axis samples change faster (more variation).
      let seed = 42
          octaves = 3
          lac = 2.0
          gain = 0.5
          scaleAlong = 0.006
          elongation = 4.0
          scaleAcross = scaleAlong * elongation
          dirX = 0 :: Float
          dirY = 1 :: Float
          -- Sample 100 points along the direction axis (Y)
          alongSamples = [ directionalRidge2DAniso seed octaves lac gain
                             scaleAlong scaleAcross 0 (fromIntegral i) dirX dirY
                         | i <- [0 :: Int .. 99] ]
          -- Sample 100 points across the direction axis (X)
          acrossSamples = [ directionalRidge2DAniso seed octaves lac gain
                              scaleAlong scaleAcross (fromIntegral i) 0 dirX dirY
                          | i <- [0 :: Int .. 99] ]
          -- Measure variation as total absolute difference between successive samples
          totalVariation :: [Float] -> Float
          totalVariation xs = sum (zipWith (\a b -> abs (a - b)) xs (drop 1 xs))
          varAlong = totalVariation alongSamples
          varAcross = totalVariation acrossSamples
      -- The across-axis should have at least 2× the variation of the along-axis
      -- (elongation = 4, so we expect roughly 4× but use a conservative bound).
      varAcross `shouldSatisfy` (> varAlong * 1.5)

  -- ----------------------------------------------------------------
  -- boundaryDistanceNormalised property tests
  -- ----------------------------------------------------------------

  describe "boundaryDistanceNormalised" $ do
    it "is 0 far from any boundary" $ do
      -- At plate centre, distance to nearest boundary is large → result ≈ 0
      let cfg = defaultTectonicsConfig { tcPlateSize = 64 }
          -- Sample many points and check the minimum is close to 0
          vals = [ boundaryDistanceNormalised 42 cfg x y
                 | x <- [0, 64 .. 640], y <- [0, 64 .. 640] ]
      minimum vals `shouldSatisfy` (< 0.1)

    it "is in [0,1] for all sampled points" $ do
      let cfg = defaultTectonicsConfig
          vals = [ boundaryDistanceNormalised 123 cfg x y
                 | x <- [-50 .. 50], y <- [-50 .. 50] ]
      all (\v -> v >= 0 && v <= 1) vals `shouldBe` True

-- | Helper to construct a minimal 'PlateInfo' for testing boundary tangent.
makePlateInfo :: (Float, Float) -> PlateInfo
makePlateInfo center = PlateInfo
  { plateInfoId = 0
  , plateInfoCenter = center
  , plateInfoVelocity = (0, 0)
  , plateInfoBaseHeight = 0.5
  , plateInfoBaseHardness = 0.5
  , plateInfoAge = 0.5
  , plateInfoCrust = PlateContinental
  }

expectPipeline :: Either PipelineError (TerrainWorld, [PipelineSnapshot]) -> IO TerrainWorld
expectPipeline result =
  case result of
    Left err -> expectationFailure (show err) >> pure (emptyWorld (WorldConfig { wcChunkSize = 1 }) defaultHexGridMeta)
    Right (world, _) -> pure world
