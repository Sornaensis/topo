module Main (main) where

import Test.Hspec
import qualified Spec.HexPick
import qualified Spec.Layout
import qualified Spec.LogActor
import qualified Spec.CacheProperties
import qualified Spec.ConfigPreset
import qualified Spec.AtlasResultBroker
import qualified Spec.AtlasScheduleBroker
import qualified Spec.AtlasScheduler
import qualified Spec.RenderActor
import qualified Spec.SnapshotReceiver
import qualified Spec.TerrainCacheWorker
import qualified Spec.TerrainRender
import qualified Spec.TerrainActor
import qualified Spec.UiActor
import qualified Spec.Widgets
import qualified Spec.WidgetTree
import qualified Spec.SliderSpec
import qualified Spec.WorldPersist

main :: IO ()
main = hspec $ do
  Spec.LogActor.spec
  Spec.CacheProperties.spec
  Spec.ConfigPreset.spec
  Spec.SliderSpec.spec
  Spec.AtlasResultBroker.spec
  Spec.AtlasScheduleBroker.spec
  Spec.AtlasScheduler.spec
  Spec.HexPick.spec
  Spec.Layout.spec
  Spec.RenderActor.spec
  Spec.SnapshotReceiver.spec
  Spec.TerrainCacheWorker.spec
  Spec.TerrainRender.spec
  Spec.TerrainActor.spec
  Spec.UiActor.spec
  Spec.Widgets.spec
  Spec.WidgetTree.spec
  Spec.WorldPersist.spec
