module Seer.System
  ( runApp
  , runRendererWithScreenshotBroker
  , shouldSkipUnchangedFrame
  , atlasQueueTargetsNewerThan
  , atlasQueueSnapshotRefreshRequired
  , renderSnapshotForceRequired
  , RenderWakeInputs(..)
  , RenderFreshnessDecision(..)
  , SnapshotGatePhase(..)
  , SnapshotGateDiagnostic(..)
  , SnapshotAdvanceState(..)
  , SnapshotLagReasons(..)
  , SnapshotLagState(..)
  , initialSnapshotAdvanceState
  , recordRenderedSnapshot
  , snapshotAdvanceAgeMs
  , initialSnapshotLagState
  , snapshotLagTransition
  , snapshotGateDiagnostic
  , formatSnapshotGateDiagnostic
  , prepareRenderFreshnessDecision
  , retryPublicationRace
  ) where

import Control.Exception (bracket, finally)
import Seer.Config.Runtime (loadConfig)
import Seer.System.Actors
  ( AppActors(..)
  , initialiseAppActors
  , shutdownAppActors
  , startCommandServices
  )
import Seer.Screenshot.Request
  ( ScreenshotRequestRef
  , shutdownScreenshotRequestRef
  )
import Seer.System.Cache (destroyRenderCacheState)
import Seer.System.EventPump (retryPublicationRace)
import Seer.System.Headless (runHeadlessHttp)
import Seer.System.MainLoop
  ( RenderFreshnessDecision(..)
  , RenderWakeInputs(..)
  , SnapshotGatePhase(..)
  , SnapshotGateDiagnostic(..)
  , SnapshotAdvanceState(..)
  , SnapshotLagReasons(..)
  , SnapshotLagState(..)
  , initialSnapshotAdvanceState
  , recordRenderedSnapshot
  , snapshotAdvanceAgeMs
  , initialSnapshotLagState
  , snapshotLagTransition
  , snapshotGateDiagnostic
  , formatSnapshotGateDiagnostic
  , atlasQueueSnapshotRefreshRequired
  , atlasQueueTargetsNewerThan
  , prepareRenderFreshnessDecision
  , renderSnapshotForceRequired
  , runMainLoop
  , shouldSkipUnchangedFrame
  )
import Seer.System.Runtime (RuntimeOptions(..), parseRuntimeOptions)
import Seer.System.Sdl
  ( SdlResources(..)
  , initialiseSdlResources
  , shutdownSdlResources
  )
import Seer.System.ThreadPriority (boostMainThreadPriority, pinMainThreadToCore0)
import System.Environment (getArgs)

runApp :: IO ()
runApp = do
  args <- getArgs
  case parseRuntimeOptions args of
    Left err -> fail err
    Right opts
      | roHeadless opts -> runHeadlessHttp opts
      | otherwise -> runSdlApp opts

runSdlApp :: RuntimeOptions -> IO ()
runSdlApp opts = do
  boostMainThreadPriority
  pinMainThreadToCore0
  runtimeCfg <- loadConfig
  bracket (initialiseAppActors runtimeCfg) shutdownAppActors $ \actors -> do
    startCommandServices opts actors
    bracket initialiseSdlResources shutdownSdlResources $ \sdl -> do
      finalState <- runRendererWithScreenshotBroker (aaScreenshotRef actors)
        (runMainLoop runtimeCfg actors sdl)
      destroyRenderCacheState (srTexturePool sdl) finalState

-- | Close capture under 'finally' before propagating any normal or exceptional
-- renderer-loop exit. This also removes the async-exception window between a
-- normal loop return and broker closure.
runRendererWithScreenshotBroker :: ScreenshotRequestRef -> IO a -> IO a
runRendererWithScreenshotBroker broker action =
  action `finally` shutdownScreenshotRequestRef broker
