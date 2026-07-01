module Spec.Tectonics (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import qualified Data.Vector.Unboxed as U
import qualified Data.List as List
import Data.Word (Word16, Word64)
import Topo
import Topo.Noise (directionalRidge2DAniso)

spec :: Spec
spec = describe "Tectonics" $ do
  describe "plate nearest pair lookup" $ do
    it "matches plateDistancePair distances for representative samples" $ do
      mapM_ assertPlatePairDistance nearestPairSamples

    it "matches representative nearest-pair snapshots" $ do
      let actual = map (\(seed, cfg, x, y) -> nearestPairSnapshot seed cfg x y) nearestPairSamples
      length actual `shouldBe` length nearestPairExpected
      mapM_ assertPlatePairSnapshotClose (zip nearestPairExpected actual)

  it "marks plate ids in flags" $ do
    let config = WorldConfig { wcChunkSize = 8 }
        world0 = emptyWorld config defaultHexGridMeta
        stages =
          [ generatePlateTerrainStage defaultGenConfig defaultTectonicsConfig ]
        pipeline = PipelineConfig
          { pipelineSeed = 123
          , pipelineStages = stages
          , pipelineDisabled = mempty, pipelineSnapshots = False, pipelineOnProgress = \_ -> pure ()
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
          , pipelineDisabled = mempty, pipelineSnapshots = False, pipelineOnProgress = \_ -> pure ()
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
          , pipelineDisabled = mempty, pipelineSnapshots = False, pipelineOnProgress = \_ -> pure ()
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
          , pipelineDisabled = mempty, pipelineSnapshots = False, pipelineOnProgress = \_ -> pure ()
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
          , pipelineDisabled = mempty, pipelineSnapshots = False, pipelineOnProgress = \_ -> pure ()
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
          , pipelineDisabled = mempty, pipelineSnapshots = False, pipelineOnProgress = \_ -> pure ()
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
          , pipelineDisabled = mempty, pipelineSnapshots = False, pipelineOnProgress = \_ -> pure ()
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
          , pipelineDisabled = mempty, pipelineSnapshots = False, pipelineOnProgress = \_ -> pure ()
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

  it "applyTectonicsChunkFingerprint" $ do
    let expected =
          [ ("tcElevation.sum", 86.86653)
          , ("tcElevation.weighted", 10880.47)
          , ("tcHardness.sum", 90.48373)
          , ("tcHardness.weighted", 11627.142)
          , ("tcPlateId.sum", 665088.0)
          , ("tcPlateId.weighted", 8.546381e7)
          , ("tcPlateBoundary.sum", 768.0)
          , ("tcPlateBoundary.weighted", 98688.0)
          , ("tcPlateHeight.sum", 86.86653)
          , ("tcPlateHeight.weighted", 10880.47)
          , ("tcPlateHardness.sum", 90.48373)
          , ("tcPlateHardness.weighted", 11627.142)
          , ("tcPlateCrust.sum", 256.0)
          , ("tcPlateCrust.weighted", 32896.0)
          , ("tcPlateAge.sum", 124.295395)
          , ("tcPlateAge.weighted", 15972.001)
          , ("tcPlateVelX.sum", -62.50195)
          , ("tcPlateVelX.weighted", -8031.496)
          , ("tcPlateVelY.sum", -140.30858)
          , ("tcPlateVelY.weighted", -18029.64)
          ]
        actual = tectonicsFieldFingerprint
    length actual `shouldBe` length expected
    mapM_ assertFingerprintClose (zip expected actual)

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

nearestPairSamples :: [(Word64, TectonicsConfig, Int, Int)]
nearestPairSamples =
  [ (42, defaultTectonicsConfig, 0, 0)
  , (42, defaultTectonicsConfig, -17, 29)
  , (99, defaultTectonicsConfig { tcPlateSize = 7 }, 123, -45)
  , (101, defaultTectonicsConfig { tcPlateSize = 16, tcPlateMergeBias = 1 }, 31, 63)
  , (202, defaultTectonicsConfig { tcPlateSize = 23, tcPlateMergeBias = 0, tcBoundaryNoiseStrength = 0 }, -128, 256)
  ]

assertPlatePairDistance :: (Word64, TectonicsConfig, Int, Int) -> Expectation
assertPlatePairDistance (seed, cfg, x, y) = do
  let (_, _, nearestD0, nearestD1) = plateNearestPairAtXY seed cfg x y
      (pairD0, pairD1) = plateDistancePair seed cfg x y
  nearestD0 `shouldBe` pairD0
  nearestD1 `shouldBe` pairD1
  nearestD0 `shouldSatisfy` (>= 0)
  nearestD1 `shouldSatisfy` (>= nearestD0)

type PlateInfoSnapshot = (Word16, (Float, Float), (Float, Float), Float, Float, Float, PlateCrust)

type PlatePairSnapshot = (PlateInfoSnapshot, PlateInfoSnapshot, Float, Float)

nearestPairExpected :: [PlatePairSnapshot]
nearestPairExpected =
  [ ( (3, (-3.9080324, 65.254), (-0.5182072, -0.30242574), 0.17329057, 0.5248912, 0.47953635, PlateContinental)
    , (61417, (-68.912575, 71.0486), (-0.5068382, -0.32111531), 0.20441785, 0.5681231, 0.4129234, PlateContinental)
    , 65.37092
    , 98.97902
    )
  , ( (3, (-3.9080324, 65.254), (-0.5182072, -0.30242574), 0.17329057, 0.5248912, 0.47953635, PlateContinental)
    , (61417, (-68.912575, 71.0486), (-0.5068382, -0.32111531), 0.20441785, 0.5681231, 0.4129234, PlateContinental)
    , 37.0529
    , 62.49265
    )
  , ( (40249, (124.45233, -44.49209), (-0.26388115, -0.5388569), -1.164522e-2, 0.40627396, 0.36679056, PlateOceanic)
    , (37859, (124.61941, -38.775375), (-0.5855772, -0.1307646), 9.5974416e-2, 0.43489024, 0.39816374, PlateContinental)
    , 2.6027744
    , 3.4917572
    )
  , ( (55531, (31.560066, 60.39452), (-0.5561831, 0.2250786), 0.12743488, 0.32141, 0.3171345, PlateContinental)
    , (57143, (28.853426, 78.23545), (-0.58520925, -0.13240147), 6.2030986e-2, 0.3532619, 0.29723424, PlateContinental)
    , 7.694636
    , 11.583889
    )
  , ( (37399, (-136.59427, 253.0), (-0.5369428, -0.2677546), 0.15525189, 0.48486438, 0.48604354, PlateContinental)
    , (40158, (-118.29779, 252.85938), (-0.4501256, -0.39672023), 7.173467e-2, 0.42929927, 0.55062896, PlateContinental)
    , 9.102827
    , 10.197862
    )
  ]

nearestPairSnapshot :: Word64 -> TectonicsConfig -> Int -> Int -> PlatePairSnapshot
nearestPairSnapshot seed cfg x y =
  let (info0, info1, d0, d1) = plateNearestPairAtXY seed cfg x y
  in (plateInfoSnapshot info0, plateInfoSnapshot info1, d0, d1)

plateInfoSnapshot :: PlateInfo -> PlateInfoSnapshot
plateInfoSnapshot info =
  ( plateInfoId info
  , plateInfoCenter info
  , plateInfoVelocity info
  , plateInfoBaseHeight info
  , plateInfoBaseHardness info
  , plateInfoAge info
  , plateInfoCrust info
  )

assertPlatePairSnapshotClose :: (PlatePairSnapshot, PlatePairSnapshot) -> Expectation
assertPlatePairSnapshotClose ((expected0, expected1, expectedD0, expectedD1), (actual0, actual1, actualD0, actualD1)) = do
  assertPlateInfoSnapshotClose (expected0, actual0)
  assertPlateInfoSnapshotClose (expected1, actual1)
  assertFloatClose expectedD0 actualD0
  assertFloatClose expectedD1 actualD1

assertPlateInfoSnapshotClose :: (PlateInfoSnapshot, PlateInfoSnapshot) -> Expectation
assertPlateInfoSnapshotClose ((expectedId, expectedCenter, expectedVelocity, expectedHeight, expectedHardness, expectedAge, expectedCrust), (actualId, actualCenter, actualVelocity, actualHeight, actualHardness, actualAge, actualCrust)) = do
  actualId `shouldBe` expectedId
  assertFloatPairClose expectedCenter actualCenter
  assertFloatPairClose expectedVelocity actualVelocity
  assertFloatClose expectedHeight actualHeight
  assertFloatClose expectedHardness actualHardness
  assertFloatClose expectedAge actualAge
  actualCrust `shouldBe` expectedCrust

assertFloatPairClose :: (Float, Float) -> (Float, Float) -> Expectation
assertFloatPairClose (expectedX, expectedY) (actualX, actualY) = do
  assertFloatClose expectedX actualX
  assertFloatClose expectedY actualY

assertFloatClose :: Float -> Float -> Expectation
assertFloatClose expected actual =
  abs (actual - expected) `shouldSatisfy` (< 1e-4)

assertFingerprintClose :: ((String, Float), (String, Float)) -> Expectation
assertFingerprintClose ((expectedName, expected), (actualName, actual)) = do
  actualName `shouldBe` expectedName
  abs (actual - expected) `shouldSatisfy` (< 1e-4)

tectonicsFieldFingerprint :: [(String, Float)]
tectonicsFieldFingerprint =
  let config = WorldConfig { wcChunkSize = 16 }
      world = emptyWorld config defaultHexGridMeta
      ChunkId key = chunkIdFromCoord (ChunkCoord 0 0)
      chunk = applyTectonicsChunk
        config
        424242
        defaultGenConfig
        (twLatMapping world)
        defaultTectonicsConfig
        key
        (emptyTerrainChunk config)
      floatFingerprint name vec =
        [ (name <> ".sum", U.sum vec)
        , (name <> ".weighted", U.ifoldl' (\acc i v -> acc + fromIntegral (i + 1) * v) 0 vec)
        ]
      word16Fingerprint name vec =
        [ (name <> ".sum", fromIntegral (U.foldl' (\acc v -> acc + fromIntegral v) (0 :: Int) vec))
        , (name <> ".weighted", fromIntegral (U.ifoldl' (\acc i v -> acc + (i + 1) * fromIntegral v) (0 :: Int) vec))
        ]
      boundaryFingerprint name vec =
        [ (name <> ".sum", fromIntegral (U.foldl' (\acc v -> acc + fromIntegral (plateBoundaryToCode v)) (0 :: Int) vec))
        , (name <> ".weighted", fromIntegral (U.ifoldl' (\acc i v -> acc + (i + 1) * fromIntegral (plateBoundaryToCode v)) (0 :: Int) vec))
        ]
  in concat
     [ floatFingerprint "tcElevation" (tcElevation chunk)
     , floatFingerprint "tcHardness" (tcHardness chunk)
     , word16Fingerprint "tcPlateId" (tcPlateId chunk)
     , boundaryFingerprint "tcPlateBoundary" (tcPlateBoundary chunk)
     , floatFingerprint "tcPlateHeight" (tcPlateHeight chunk)
     , floatFingerprint "tcPlateHardness" (tcPlateHardness chunk)
     , word16Fingerprint "tcPlateCrust" (tcPlateCrust chunk)
     , floatFingerprint "tcPlateAge" (tcPlateAge chunk)
     , floatFingerprint "tcPlateVelX" (tcPlateVelX chunk)
     , floatFingerprint "tcPlateVelY" (tcPlateVelY chunk)
     ]

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
