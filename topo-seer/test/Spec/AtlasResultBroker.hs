module Spec.AtlasResultBroker (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Data.IORef (newIORef)
import Hyperspace.Actor (ActorSystem, getSingleton, newActorSystem, shutdownActorSystem)
import Linear (V2(..))
import Test.Hspec
import Actor.AtlasCache (AtlasKey(..))
import Actor.AtlasResult (AtlasBuildResult(..))
import Actor.AtlasResultBroker (drainAtlasResultsN, enqueueAtlasResult, atlasResultBrokerActorDef, setAtlasResultRef)
import Actor.Data (TerrainSnapshot(..))
import Actor.UI (ViewMode(..))
import UI.TerrainAtlas (AtlasTileGeometry(..))
import UI.Widgets (Rect(..))

withSystem :: (ActorSystem -> IO a) -> IO a
withSystem = bracket newActorSystem shutdownActorSystem

spec :: Spec
spec = describe "AtlasResultBroker" $ do
  it "drains results in FIFO order" $ withSystem $ \system -> do
    brokerHandle <- getSingleton system atlasResultBrokerActorDef
    ref <- newIORef []
    setAtlasResultRef brokerHandle ref
    let key = AtlasKey ViewElevation 0 (tsVersion sampleTerrainSnapshot)
        tile1 = AtlasTileGeometry (Rect (V2 0 0, V2 1 1)) 1 [] []
        tile2 = AtlasTileGeometry (Rect (V2 1 1, V2 1 1)) 2 [] []
        result1 = AtlasBuildResult
          { abrKey = key
          , abrScale = 1
          , abrTile = tile1
          }
        result2 = AtlasBuildResult
          { abrKey = key
          , abrScale = 2
          , abrTile = tile2
          }
    enqueueAtlasResult brokerHandle result1
    enqueueAtlasResult brokerHandle result2
    threadDelay 50000 -- allow actor to process casts
    drained1 <- drainAtlasResultsN ref 1
    drained2 <- drainAtlasResultsN ref 1
    map abrScale (drained1 <> drained2) `shouldBe` [1, 2]
    map abrKey (drained1 <> drained2) `shouldBe` [key, key]

sampleTerrainSnapshot :: TerrainSnapshot
sampleTerrainSnapshot = TerrainSnapshot 0 0 mempty mempty mempty mempty
