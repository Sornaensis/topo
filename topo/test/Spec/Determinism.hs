module Spec.Determinism (spec) where

import Test.Hspec
import Topo

spec :: Spec
spec = describe "Determinism" $ do
  it "replays pipeline deterministically" $ do
    let config = WorldConfig { wcChunkSize = 8 }
        world0 = emptyWorld config defaultHexGridMeta
        pipeline = buildPipelineConfig defaultWorldGenConfig 1337
        env = TopoEnv { teLogger = \_ -> pure () }
    resultA <- runPipeline pipeline env world0
    resultB <- runPipeline pipeline env world0
    worldA <- expectPipeline resultA
    worldB <- expectPipeline resultB
    case (encodeWorld worldA, encodeWorld worldB) of
      (Right encodedA, Right encodedB) -> encodedA `shouldBe` encodedB
      (Left err, _) -> expectationFailure (show err)
      (_, Left err) -> expectationFailure (show err)

expectPipeline :: Either PipelineError (TerrainWorld, [PipelineSnapshot]) -> IO TerrainWorld
expectPipeline result =
  case result of
    Left err -> expectationFailure (show err) >> pure (emptyWorld (WorldConfig { wcChunkSize = 1 }) defaultHexGridMeta)
    Right (world, _) -> pure world
