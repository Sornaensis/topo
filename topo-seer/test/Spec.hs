module Main (main) where

import System.IO (hSetEncoding, stdout, stderr, utf8)
import Test.Hspec
import qualified Spec.HexPick
import qualified Spec.HexAlignment
import qualified Spec.Layout
import qualified Spec.LogActor
import qualified Spec.AppService
import qualified Spec.CacheProperties
import qualified Spec.ConfigRuntime
import qualified Spec.ConfigSnapshot
import qualified Spec.ConfigSliders
import qualified Spec.PipelineControls
import qualified Spec.PipelineIntegrator
import qualified Spec.AtlasResultBroker
import qualified Spec.AtlasScheduleBroker
import qualified Spec.AtlasScheduler
import qualified Spec.AtlasManager
import qualified Spec.AtlasCache
import qualified Spec.RenderLoopAtlasMaintenance
import qualified Spec.RenderLoopSnapshotFreshness
import qualified Spec.WeatherAtlasFlicker
import qualified Spec.AtlasTransparency
import qualified Spec.SnapshotReceiver
import qualified Spec.TerrainCacheWorker
import qualified Spec.TerrainRender
import qualified Spec.TerrainInspector
import qualified Spec.ViewModeRegistry
import qualified Spec.RiverRender
import qualified Spec.TerrainActor
import qualified Spec.PluginManager
import qualified Spec.UiActor
import qualified Spec.DayNight
import qualified Spec.Simulation
import qualified Spec.ScreenshotStorage
import qualified Spec.ScreenshotRequest
import qualified Spec.AutoTick
import qualified Spec.Widgets
import qualified Spec.WidgetTree
import qualified Spec.ViewControls
import qualified Spec.ModalEvents
import qualified Spec.SliderSpec
import qualified Spec.WorldPersist
import qualified Spec.CommandDispatch
import qualified Spec.Component
import qualified Spec.UIDrawSnapshots
import qualified Spec.DataBrowser
import qualified Spec.DataBrowserAppService
import qualified Spec.DataBrowserExecutor
import qualified Spec.OverlayInspector
import qualified Spec.DataDetailPopover
import qualified Spec.DataResourceE2E
import qualified Spec.Headless
import qualified Spec.HeadlessScreenshot
import qualified Spec.HTTP
import qualified Spec.EditorBrush
import qualified Spec.EditorHistory
import qualified Spec.ZoomStageProperties

main :: IO ()
main = do
  handledFixture <- Spec.PluginManager.runFixtureCliIfRequested
  if handledFixture
    then pure ()
    else do
      hSetEncoding stdout utf8
      hSetEncoding stderr utf8
      hspec $ do
        Spec.AppService.spec
        Spec.LogActor.spec
        Spec.CacheProperties.spec
        Spec.ConfigRuntime.spec
        Spec.ConfigSnapshot.spec
        Spec.ConfigSliders.spec
        Spec.PipelineControls.spec
        Spec.PipelineIntegrator.spec
        Spec.SliderSpec.spec
        Spec.AtlasResultBroker.spec
        Spec.AtlasScheduleBroker.spec
        Spec.AtlasScheduler.spec
        Spec.AtlasManager.spec
        Spec.AtlasCache.spec
        Spec.RenderLoopAtlasMaintenance.spec
        Spec.RenderLoopSnapshotFreshness.spec
        Spec.WeatherAtlasFlicker.spec
        Spec.AtlasTransparency.spec
        Spec.HexPick.spec
        Spec.HexAlignment.spec
        Spec.Layout.spec
        Spec.SnapshotReceiver.spec
        Spec.TerrainCacheWorker.spec
        Spec.TerrainRender.spec
        Spec.TerrainInspector.spec
        Spec.ViewModeRegistry.spec
        Spec.RiverRender.spec
        Spec.TerrainActor.spec
        Spec.PluginManager.spec
        Spec.UiActor.spec
        Spec.DayNight.spec
        Spec.Simulation.spec
        Spec.ScreenshotStorage.spec
        Spec.ScreenshotRequest.spec
        Spec.AutoTick.spec
        Spec.Widgets.spec
        Spec.WidgetTree.spec
        Spec.ViewControls.spec
        Spec.ModalEvents.spec
        Spec.WorldPersist.spec
        Spec.CommandDispatch.spec
        Spec.Component.spec
        Spec.UIDrawSnapshots.spec
        Spec.DataBrowser.spec
        Spec.DataBrowserAppService.spec
        Spec.DataBrowserExecutor.spec
        Spec.OverlayInspector.spec
        Spec.DataDetailPopover.spec
        Spec.DataResourceE2E.spec
        Spec.Headless.spec
        Spec.HeadlessScreenshot.spec
        Spec.HTTP.spec
        Spec.EditorBrush.spec
        Spec.EditorHistory.spec
        Spec.ZoomStageProperties.spec
