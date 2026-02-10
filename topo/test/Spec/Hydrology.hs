module Spec.Hydrology (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Data.Vector.Unboxed as U
import Topo

spec :: Spec
spec = describe "Hydrology" $ do
  it "routes moisture downstream across chunks" $ do
    let config = WorldConfig { wcChunkSize = 2 }
        world0 = emptyWorld config defaultHexGridMeta
        leftChunk = generateTerrainChunk config (\_ -> 1)
        rightChunk = generateTerrainChunk config (\_ -> 0)
        world1 = setTerrainChunk (chunkIdFromCoord (ChunkCoord 0 0)) leftChunk
              $ setTerrainChunk (chunkIdFromCoord (ChunkCoord 1 0)) rightChunk world0
        pipeline = PipelineConfig
          { pipelineSeed = 1
          , pipelineStages = [applyHydrologyStage defaultHydroConfig { hcWaterLevel = 0.2 }]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world1
    world2 <- expectPipeline result
    let avg v = U.sum v / fromIntegral (max 1 (U.length v))
    case (getTerrainChunk (chunkIdFromCoord (ChunkCoord 0 0)) world2,
          getTerrainChunk (chunkIdFromCoord (ChunkCoord 1 0)) world2) of
      (Just left, Just right) -> do
        let leftM = avg (tcMoisture left)
            rightM = avg (tcMoisture right)
        rightM `shouldSatisfy` (> leftM)
      _ -> expectationFailure "missing chunks"

  it "writes river and groundwater chunks" $ do
    let config = WorldConfig { wcChunkSize = 2 }
        world0 = emptyWorld config defaultHexGridMeta
        slopeChunk = generateTerrainChunk config (\(TileCoord x y) -> fromIntegral (x + y))
        moistChunk = slopeChunk { tcMoisture = U.replicate (chunkTileCount config) 0.6 }
        world1 = setTerrainChunk (ChunkId 0) moistChunk world0
        pipeline = PipelineConfig
          { pipelineSeed = 2
          , pipelineStages = [applyRiverStage defaultRiverConfig defaultGroundwaterConfig]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world1
    world2 <- expectPipeline result
    getRiverChunk (ChunkId 0) world2 `shouldSatisfy` isJust
    getGroundwaterChunk (ChunkId 0) world2 `shouldSatisfy` isJust

  it "reduces river depth with higher hardness" $ do
    let config = WorldConfig { wcChunkSize = 2 }
        world0 = emptyWorld config defaultHexGridMeta
        baseChunk = generateTerrainChunk config (\_ -> 1)
        hardness = U.fromList [0, 1, 0, 1]
        chunk = baseChunk
          { tcHardness = hardness
          , tcMoisture = U.replicate (chunkTileCount config) 0.5
          }
        world1 = setTerrainChunk (ChunkId 0) chunk world0
        riverCfg = defaultRiverConfig
          { rcMinAccumulation = 0
          , rcBaseAccumulation = 10
          , rcChannelDepthScale = 0.1
          , rcChannelMaxDepth = 10
          , rcHardnessDepthWeight = 1
          }
        pipeline = PipelineConfig
          { pipelineSeed = 4
          , pipelineStages = [applyRiverStage riverCfg defaultGroundwaterConfig]
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world1
    world2 <- expectPipeline result
    case getRiverChunk (ChunkId 0) world2 of
      Nothing -> expectationFailure "missing river chunk"
      Just rivers -> do
        let depths = rcChannelDepth rivers
            soft = [depths U.! 0, depths U.! 2]
            hard = [depths U.! 1, depths U.! 3]
            avg xs = sum xs / fromIntegral (max 1 (length xs))
        avg hard `shouldSatisfy` (< avg soft)

  prop "respects gwMinBasinSize for storage" $
    forAll (chooseInt (2, 6)) $ \minSize ->
      forAll (choose (0.2, 1.0)) $ \moisture ->
        ioProperty $ do
          let config = WorldConfig { wcChunkSize = 2 }
              world0 = emptyWorld config defaultHexGridMeta
              flatChunk = generateTerrainChunk config (\_ -> 1)
              moistChunk = flatChunk { tcMoisture = U.replicate (chunkTileCount config) moisture }
              world1 = setTerrainChunk (ChunkId 0) moistChunk world0
              gwCfg = defaultGroundwaterConfig { gwMinBasinSize = minSize }
              pipeline = PipelineConfig
                { pipelineSeed = 3
                , pipelineStages = [applyRiverStage defaultRiverConfig gwCfg]
                , pipelineSnapshots = False
                }
              env = TopoEnv { teLogger = \_ -> pure () }
          result <- runPipeline pipeline env world1
          world2 <- expectPipeline result
          case getGroundwaterChunk (ChunkId 0) world2 of
            Nothing -> pure False
            Just chunk -> pure (U.all (== 0) (gwStorage chunk) && U.all (== 0) (gwDischarge chunk))

expectPipeline :: Either PipelineError (TerrainWorld, [PipelineSnapshot]) -> IO TerrainWorld
expectPipeline result =
  case result of
    Left err -> expectationFailure (show err) >> pure (emptyWorld (WorldConfig { wcChunkSize = 1 }) defaultHexGridMeta)
    Right (world, _) -> pure world

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust (Just _) = True
