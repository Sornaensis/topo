module Main (main) where

import System.IO (hSetEncoding, stdout, stderr, utf8)
import Test.Hspec
import qualified Spec.HexPick
import qualified Spec.Layout
import qualified Spec.LogActor
import qualified Spec.CacheProperties
import qualified Spec.ConfigSnapshot
import qualified Spec.AtlasResultBroker
import qualified Spec.AtlasScheduleBroker
import qualified Spec.AtlasScheduler
import qualified Spec.SnapshotReceiver
import qualified Spec.TerrainCacheWorker
import qualified Spec.TerrainRender
import qualified Spec.RiverRender
import qualified Spec.TerrainActor
import qualified Spec.PluginManager
import qualified Spec.UiActor
import qualified Spec.Simulation
import qualified Spec.Widgets
import qualified Spec.WidgetTree
import qualified Spec.SliderSpec
import qualified Spec.WorldPersist
import qualified Spec.CommandDispatch
import qualified Spec.EditorBrush
import qualified Spec.EditorHistory
import qualified Spec.ZoomStageProperties

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  hspec $ do
    Spec.LogActor.spec
    Spec.CacheProperties.spec
    Spec.ConfigSnapshot.spec
    Spec.SliderSpec.spec
    Spec.AtlasResultBroker.spec
    Spec.AtlasScheduleBroker.spec
    Spec.AtlasScheduler.spec
    Spec.HexPick.spec
    Spec.Layout.spec
    Spec.SnapshotReceiver.spec
    Spec.TerrainCacheWorker.spec
    Spec.TerrainRender.spec
    Spec.RiverRender.spec
    Spec.TerrainActor.spec
    Spec.PluginManager.spec
    Spec.UiActor.spec
    Spec.Simulation.spec
    Spec.Widgets.spec
    Spec.WidgetTree.spec
    Spec.WorldPersist.spec
    Spec.CommandDispatch.spec
    Spec.EditorBrush.spec
    Spec.EditorHistory.spec
    Spec.ZoomStageProperties.spec
