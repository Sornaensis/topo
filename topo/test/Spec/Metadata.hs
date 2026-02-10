module Spec.Metadata (spec) where

import Test.Hspec
import Test.QuickCheck (ioProperty, property)
import Data.Proxy (Proxy(..))
import Data.Text (Text, pack)
import Topo

newtype Tag = Tag Text
  deriving (Eq, Show)

instance Metadata Tag where
  metadataKey _ = pack "tag"
  metadataVersion _ = 2
  metadataEncode (Tag txt) = encodeJsonString txt
  metadataDecode _ payload = Tag <$> decodeJsonString payload

spec :: Spec
spec = describe "Metadata" $ do
  it "stores and retrieves hex metadata" $ do
    let store = putHexMeta (HexAxial 0 0) (Tag (pack "alpha")) emptyMetadataStore
    getHexMeta (HexAxial 0 0) store `shouldBe` Just (Tag (pack "alpha"))

  it "stores and retrieves world metadata" $ do
    let world = emptyWorld (WorldConfig { wcChunkSize = 8 }) defaultHexGridMeta
        world' = putHexMetaWorld (HexAxial 1 2) (Tag (pack "beta")) world
    getHexMetaWorld (HexAxial 1 2) world' `shouldBe` Just (Tag (pack "beta"))

  it "migrates metadata versions" $ do
    let store = putHexMetaWithVersion 1 (HexAxial 0 0) (Tag (pack "alpha")) emptyMetadataStore
        migration = MetadataMigration
          { mmKey = pack "tag"
          , mmFrom = 1
          , mmTo = 2
          , mmMigrate = \(Tag t) -> Tag (t <> pack "-v2")
          }
        store' = migrateMetadataStore [migration] store
    getHexMeta (HexAxial 0 0) store' `shouldBe` Just (Tag (pack "alpha-v2"))
    getHexMetaVersion (Proxy :: Proxy Tag) (HexAxial 0 0) store' `shouldBe` Just 2

  it "stores plate metadata for a hex" $ do
    let config = WorldConfig { wcChunkSize = 8 }
        world0 = emptyWorld config defaultHexGridMeta
        stages =
          [ generatePlateTerrainStage defaultGenConfig defaultTectonicsConfig ]
        pipeline = PipelineConfig
          { pipelineSeed = 1234
          , pipelineStages = stages
          , pipelineSnapshots = False
          }
        env = TopoEnv { teLogger = \_ -> pure () }
    result <- runPipeline pipeline env world0
    world1 <- expectPipeline result
    let hex = HexAxial 0 0
    case plateHexMetaAt world1 hex of
      Nothing -> expectationFailure "missing plate metadata"
      Just meta -> do
        let world2 = storePlateHexMeta hex world1
        getHexMetaWorld hex world2 `shouldBe` Just meta

  it "round-trips plate metadata for in-chunk hexes" $
    property $ \x y -> ioProperty $ do
      let config = WorldConfig { wcChunkSize = 8 }
          world0 = emptyWorld config defaultHexGridMeta
          pipeline = PipelineConfig
            { pipelineSeed = 222
            , pipelineStages = [generatePlateTerrainStage defaultGenConfig defaultTectonicsConfig]
            , pipelineSnapshots = False
            }
          env = TopoEnv { teLogger = \_ -> pure () }
          hx = x `mod` wcChunkSize config
          hy = y `mod` wcChunkSize config
          hex = HexAxial hx hy
      result <- runPipeline pipeline env world0
      world1 <- expectPipeline result
      pure $ case plateHexMetaAt world1 hex of
        Nothing -> False
        Just meta ->
          let world2 = storePlateHexMeta hex world1
          in getHexMetaWorld hex world2 == Just meta

expectPipeline :: Either PipelineError (TerrainWorld, [PipelineSnapshot]) -> IO TerrainWorld
expectPipeline result =
  case result of
    Left err -> expectationFailure (show err) >> pure (emptyWorld (WorldConfig { wcChunkSize = 1 }) defaultHexGridMeta)
    Right (world, _) -> pure world
