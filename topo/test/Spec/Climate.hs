module Spec.Climate (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Data.Vector.Unboxed as U
import Topo
import Topo.Planet (defaultPlanetConfig, defaultWorldSlice, PlanetConfig(..), WorldSlice(..))

avgVector :: U.Vector Float -> Float
avgVector vec =
  U.sum vec / max 1 (fromIntegral (U.length vec))

spec :: Spec
spec = describe "Climate" $ do
  it "produces temperature and precipitation" $ do
    let config = WorldConfig { wcChunkSize = 8 }
        world0 = emptyWorld config defaultHexGridMeta
        terrain = generateTerrainChunk config (\_ -> 0.2)
        world1 = setTerrainChunk (ChunkId 0) terrain world0
        pipeline = PipelineConfig
          { pipelineSeed = 9
          , pipelineStages = [generateClimateStage defaultClimateConfig 0.5]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world1
    world2 <- expectPipeline result
    case getClimateChunk (ChunkId 0) world2 of
      Nothing -> expectationFailure "missing climate chunk"
      Just chunk -> do
        U.length (ccTempAvg chunk) `shouldBe` chunkTileCount config
        U.length (ccPrecipAvg chunk) `shouldBe` chunkTileCount config
        U.any (> 0) (ccPrecipAvg chunk) `shouldBe` True

  it "produces precipitation across multiple chunks" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        world0 = emptyWorld config defaultHexGridMeta
        low = generateTerrainChunk config (\_ -> 0)
        high = generateTerrainChunk config (\_ -> 0.6)
        world1 = setTerrainChunk (chunkIdFromCoord (ChunkCoord 0 0)) low
              $ setTerrainChunk (chunkIdFromCoord (ChunkCoord 1 0)) high world0
        pipeline = PipelineConfig
          { pipelineSeed = 11
          , pipelineStages = [generateClimateStage defaultClimateConfig 0.5]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world1
    world2 <- expectPipeline result
    case (getClimateChunk (chunkIdFromCoord (ChunkCoord 0 0)) world2,
          getClimateChunk (chunkIdFromCoord (ChunkCoord 1 0)) world2) of
      (Just left, Just right) -> do
        U.any (> 0) (ccPrecipAvg left) `shouldBe` True
        U.any (> 0) (ccPrecipAvg right) `shouldBe` True
      _ -> expectationFailure "missing climate chunks"

  it "biases precipitation near convergent boundaries" $ do
    let config = WorldConfig { wcChunkSize = 8 }
        world0 = emptyWorld config defaultHexGridMeta
        baseTerrain = generateTerrainChunk config (\_ -> 0.6)
        n = chunkTileCount config
        boundaryChunk = baseTerrain { tcPlateBoundary = U.replicate n PlateBoundaryConvergent }
        plainChunk = baseTerrain { tcPlateBoundary = U.replicate n PlateBoundaryNone }
        world1 = setTerrainChunk (chunkIdFromCoord (ChunkCoord 0 0)) boundaryChunk
              $ setTerrainChunk (chunkIdFromCoord (ChunkCoord 1 0)) plainChunk world0
        pipeline = PipelineConfig
          { pipelineSeed = 42
          , pipelineStages = [generateClimateStage defaultClimateConfig 0.5]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world1
    world2 <- expectPipeline result
    case (getClimateChunk (chunkIdFromCoord (ChunkCoord 0 0)) world2,
          getClimateChunk (chunkIdFromCoord (ChunkCoord 1 0)) world2) of
      (Just left, Just right) ->
        avgVector (ccPrecipAvg left) `shouldSatisfy` (> avgVector (ccPrecipAvg right))
      _ -> expectationFailure "missing climate chunks"

  it "cools temperatures near convergent boundaries" $ do
    let config = WorldConfig { wcChunkSize = 8 }
        world0 = emptyWorld config defaultHexGridMeta
        baseTerrain = generateTerrainChunk config (\_ -> 0.6)
        n = chunkTileCount config
        boundaryChunk = baseTerrain { tcPlateBoundary = U.replicate n PlateBoundaryConvergent }
        plainChunk = baseTerrain { tcPlateBoundary = U.replicate n PlateBoundaryNone }
        world1 = setTerrainChunk (chunkIdFromCoord (ChunkCoord 0 0)) boundaryChunk
              $ setTerrainChunk (chunkIdFromCoord (ChunkCoord 1 0)) plainChunk world0
        pipeline = PipelineConfig
          { pipelineSeed = 42
          , pipelineStages = [generateClimateStage defaultClimateConfig 0.5]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world1
    world2 <- expectPipeline result
    case (getClimateChunk (chunkIdFromCoord (ChunkCoord 0 0)) world2,
          getClimateChunk (chunkIdFromCoord (ChunkCoord 1 0)) world2) of
      (Just left, Just right) ->
        avgVector (ccTempAvg left) `shouldSatisfy` (< avgVector (ccTempAvg right))
      _ -> expectationFailure "missing climate chunks"

  it "cools temperatures with faster boundary motion" $ do
    let config = WorldConfig { wcChunkSize = 8 }
        world0 = emptyWorld config defaultHexGridMeta
        baseTerrain = generateTerrainChunk config (\_ -> 0.6)
        n = chunkTileCount config
        slow = baseTerrain
          { tcPlateBoundary = U.replicate n PlateBoundaryConvergent
          , tcPlateHeight = U.replicate n 0.6
          , tcPlateVelX = U.replicate n 0.0
          , tcPlateVelY = U.replicate n 0.0
          }
        fast = baseTerrain
          { tcPlateBoundary = U.replicate n PlateBoundaryConvergent
          , tcPlateHeight = U.replicate n 0.6
          , tcPlateVelX = U.replicate n 1.0
          , tcPlateVelY = U.replicate n 0.0
          }
        world1 = setTerrainChunk (chunkIdFromCoord (ChunkCoord 0 0)) fast
              $ setTerrainChunk (chunkIdFromCoord (ChunkCoord 1 0)) slow world0
        pipeline = PipelineConfig
          { pipelineSeed = 24
          , pipelineStages = [generateClimateStage defaultClimateConfig 0.5]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world1
    world2 <- expectPipeline result
    case (getClimateChunk (chunkIdFromCoord (ChunkCoord 0 0)) world2,
          getClimateChunk (chunkIdFromCoord (ChunkCoord 1 0)) world2) of
      (Just left, Just right) ->
        avgVector (ccTempAvg left) `shouldSatisfy` (< avgVector (ccTempAvg right))
      _ -> expectationFailure "missing climate chunks"

  it "biases precipitation with faster boundary motion" $ do
    let config = WorldConfig { wcChunkSize = 8 }
        waterLevel = 0.5
        motionPrecipBias = 2.0
        world0 = emptyWorld config defaultHexGridMeta
        baseTerrain = generateTerrainChunk config (\_ -> 0.6)
        n = chunkTileCount config
        slow = baseTerrain
          { tcPlateBoundary = U.replicate n PlateBoundaryConvergent
          , tcPlateHeight = U.replicate n 0.6
          , tcPlateVelX = U.replicate n 0.0
          , tcPlateVelY = U.replicate n 0.0
          }
        fast = baseTerrain
          { tcPlateBoundary = U.replicate n PlateBoundaryConvergent
          , tcPlateHeight = U.replicate n 0.6
          , tcPlateVelX = U.replicate n 1.0
          , tcPlateVelY = U.replicate n 0.0
          }
        world1 = setTerrainChunk (chunkIdFromCoord (ChunkCoord 0 0)) fast
              $ setTerrainChunk (chunkIdFromCoord (ChunkCoord 1 0)) slow world0
        climateCfg = defaultClimateConfig
          { ccBoundaryMotionPrecip = motionPrecipBias }
        pipeline = PipelineConfig
          { pipelineSeed = 31
          , pipelineStages = [generateClimateStage climateCfg waterLevel]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world1
    world2 <- expectPipeline result
    case (getClimateChunk (chunkIdFromCoord (ChunkCoord 0 0)) world2,
          getClimateChunk (chunkIdFromCoord (ChunkCoord 1 0)) world2) of
      (Just left, Just right) ->
        avgVector (ccPrecipAvg left) `shouldSatisfy` (> avgVector (ccPrecipAvg right))
      _ -> expectationFailure "missing climate chunks"

  it "cools temperatures over higher plate heights" $ do
    let config = WorldConfig { wcChunkSize = 8 }
        world0 = emptyWorld config defaultHexGridMeta
        baseTerrain = generateTerrainChunk config (\_ -> 0.6)
        n = chunkTileCount config
        highPlate = baseTerrain { tcPlateHeight = U.replicate n 0.9 }
        lowPlate = baseTerrain { tcPlateHeight = U.replicate n 0.2 }
        world1 = setTerrainChunk (chunkIdFromCoord (ChunkCoord 0 0)) highPlate
              $ setTerrainChunk (chunkIdFromCoord (ChunkCoord 1 0)) lowPlate world0
        pipeline = PipelineConfig
          { pipelineSeed = 7
          , pipelineStages = [generateClimateStage defaultClimateConfig 0.5]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world1
    world2 <- expectPipeline result
    case (getClimateChunk (chunkIdFromCoord (ChunkCoord 0 0)) world2,
          getClimateChunk (chunkIdFromCoord (ChunkCoord 1 0)) world2) of
      (Just left, Just right) ->
        avgVector (ccTempAvg left) `shouldSatisfy` (< avgVector (ccTempAvg right))
      _ -> expectationFailure "missing climate chunks"

  it "biases precipitation over higher plate heights" $ do
    let config = WorldConfig { wcChunkSize = 8 }
        world0 = emptyWorld config defaultHexGridMeta
        baseTerrain = generateTerrainChunk config (\_ -> 0.6)
        n = chunkTileCount config
        highPlate = baseTerrain { tcPlateHeight = U.replicate n 0.9 }
        lowPlate = baseTerrain { tcPlateHeight = U.replicate n 0.2 }
        world1 = setTerrainChunk (chunkIdFromCoord (ChunkCoord 0 0)) highPlate
              $ setTerrainChunk (chunkIdFromCoord (ChunkCoord 1 0)) lowPlate world0
        pipeline = PipelineConfig
          { pipelineSeed = 9
          , pipelineStages = [generateClimateStage defaultClimateConfig 0.5]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world1
    world2 <- expectPipeline result
    case (getClimateChunk (chunkIdFromCoord (ChunkCoord 0 0)) world2,
          getClimateChunk (chunkIdFromCoord (ChunkCoord 1 0)) world2) of
      (Just left, Just right) ->
        avgVector (ccPrecipAvg left) `shouldSatisfy` (> avgVector (ccPrecipAvg right))
      _ -> expectationFailure "missing climate chunks"

  it "temperature at equator > temperature at 60° latitude" $ do
    -- Generate climate for a world at the equator and another at 60°N.
    -- The equator world should have higher average temperatures.
    let config = WorldConfig { wcChunkSize = 4 }
        sliceEq  = defaultWorldSlice { wsLatCenter = 0,  wsLatExtent = 10 }
        slice60  = defaultWorldSlice { wsLatCenter = 60, wsLatExtent = 10 }
        worldEq  = emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig sliceEq
        world60  = emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig slice60
        terrain  = generateTerrainChunk config (\_ -> 0.6)
        cid = chunkIdFromCoord (ChunkCoord 0 0)
        setupWorld w = setTerrainChunk cid terrain w
        pipeline = PipelineConfig
          { pipelineSeed = 77
          , pipelineStages = [generateClimateStage defaultClimateConfig 0.5]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    resultEq <- runPipeline pipeline env (setupWorld worldEq)
    result60 <- runPipeline pipeline env (setupWorld world60)
    wEq <- expectPipeline resultEq
    w60 <- expectPipeline result60
    case (getClimateChunk cid wEq, getClimateChunk cid w60) of
      (Just cEq, Just c60) ->
        avgVector (ccTempAvg cEq) `shouldSatisfy` (> avgVector (ccTempAvg c60))
      _ -> expectationFailure "missing climate chunks"

  it "equator slice with defaults matches legacy climate" $ do
    -- A world at the equator with default planet/slice should produce
    -- the same climate as a plain `emptyWorld` (backward compatibility).
    let config = WorldConfig { wcChunkSize = 4 }
        worldDefault = emptyWorld config defaultHexGridMeta
        worldExplicit = emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig defaultWorldSlice
        terrain = generateTerrainChunk config (\_ -> 0.4)
        cid = chunkIdFromCoord (ChunkCoord 0 0)
        pipeline = PipelineConfig
          { pipelineSeed = 88
          , pipelineStages = [generateClimateStage defaultClimateConfig 0.5]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    resultD <- runPipeline pipeline env (setTerrainChunk cid terrain worldDefault)
    resultE <- runPipeline pipeline env (setTerrainChunk cid terrain worldExplicit)
    wD <- expectPipeline resultD
    wE <- expectPipeline resultE
    case (getClimateChunk cid wD, getClimateChunk cid wE) of
      (Just cD, Just cE) -> do
        -- Temperature and precipitation vectors should be identical
        U.toList (ccTempAvg cD) `shouldBe` U.toList (ccTempAvg cE)
        U.toList (ccPrecipAvg cD) `shouldBe` U.toList (ccPrecipAvg cE)
      _ -> expectationFailure "missing climate chunks"

  prop "temperature at equator exceeds 60° for arbitrary valid planets" $
    forAll (choose (4778, 9557)) $ \radius ->
      forAll (choose (0.7, 1.3)) $ \insol ->
        ioProperty $ do
          let planet = defaultPlanetConfig { pcRadius = radius, pcInsolation = insol }
              config = WorldConfig { wcChunkSize = 4 }
              sliceEq  = defaultWorldSlice { wsLatCenter = 0,  wsLatExtent = 10 }
              slice60  = defaultWorldSlice { wsLatCenter = 60, wsLatExtent = 10 }
              worldEq  = emptyWorldWithPlanet config defaultHexGridMeta planet sliceEq
              world60  = emptyWorldWithPlanet config defaultHexGridMeta planet slice60
              terrain  = generateTerrainChunk config (\_ -> 0.6)
              cid = chunkIdFromCoord (ChunkCoord 0 0)
              pipeline = PipelineConfig
                { pipelineSeed = 99
                , pipelineStages = [generateClimateStage defaultClimateConfig 0.5]
                , pipelineSnapshots = False
                }
              env = TopoEnv { teLogger = \_ -> pure () }
          resultEq <- runPipeline pipeline env (setTerrainChunk cid terrain worldEq)
          result60 <- runPipeline pipeline env (setTerrainChunk cid terrain world60)
          wEq <- expectPipelineProp resultEq
          w60 <- expectPipelineProp result60
          case (wEq >>= getClimateChunk cid, w60 >>= getClimateChunk cid) of
            (Just cEq, Just c60) ->
              pure (avgVector (ccTempAvg cEq) > avgVector (ccTempAvg c60))
            _ -> pure False

  it "wind direction differs between trade (15°N) and westerly (45°N) belts" $ do
    -- At Earth defaults the wind belt harmonic is 3, producing 30° bands.
    -- 15°N falls in the trade wind belt and 45°N in the westerlies.
    -- The average wind directions should differ substantially.
    let config = WorldConfig { wcChunkSize = 4 }
        slice15 = defaultWorldSlice { wsLatCenter = 15, wsLatExtent = 5 }
        slice45 = defaultWorldSlice { wsLatCenter = 45, wsLatExtent = 5 }
        world15 = emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig slice15
        world45 = emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig slice45
        terrain = generateTerrainChunk config (\_ -> 0.6)
        cid = chunkIdFromCoord (ChunkCoord 0 0)
        pipeline = PipelineConfig
          { pipelineSeed = 55
          , pipelineStages = [generateClimateStage defaultClimateConfig 0.5]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result15 <- runPipeline pipeline env (setTerrainChunk cid terrain world15)
    result45 <- runPipeline pipeline env (setTerrainChunk cid terrain world45)
    w15 <- expectPipeline result15
    w45 <- expectPipeline result45
    case (getClimateChunk cid w15, getClimateChunk cid w45) of
      (Just c15, Just c45) -> do
        -- Use circular mean: atan2(mean(sin(dir)), mean(cos(dir)))
        let circMean dirs =
              let sinMean = avgVector (U.map sin dirs)
                  cosMean = avgVector (U.map cos dirs)
              in atan2 sinMean cosMean
            mean15 = circMean (ccWindDirAvg c15)
            mean45 = circMean (ccWindDirAvg c45)
            angleDiff = abs (mean15 - mean45)
            -- Normalize to [0, pi] range
            diff = min angleDiff (2 * pi - angleDiff)
        -- Wind direction should differ by at least 0.5 radians (~29°)
        diff `shouldSatisfy` (> 0.5)
      _ -> expectationFailure "missing climate chunks"

expectPipeline :: Either PipelineError (TerrainWorld, [PipelineSnapshot]) -> IO TerrainWorld
expectPipeline result =
  case result of
    Left err -> expectationFailure (show err) >> pure (emptyWorld (WorldConfig { wcChunkSize = 1 }) defaultHexGridMeta)
    Right (world, _) -> pure world

-- | Property-test-friendly variant: returns 'Maybe TerrainWorld' instead
-- of failing via 'expectationFailure' (which throws inside QuickCheck).
expectPipelineProp :: Either PipelineError (TerrainWorld, [PipelineSnapshot]) -> IO (Maybe TerrainWorld)
expectPipelineProp result =
  case result of
    Left _          -> pure Nothing
    Right (world, _) -> pure (Just world)
