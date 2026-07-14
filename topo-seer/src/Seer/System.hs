module Seer.System
  ( runApp
  , runRendererWithScreenshotBroker
  , shouldSkipUnchangedFrame
  ) where

import Control.Exception (finally)
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
import Seer.System.Headless (runHeadlessHttp)
import Seer.System.MainLoop (runMainLoop, shouldSkipUnchangedFrame)
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
  actors <- initialiseAppActors runtimeCfg
  startCommandServices opts actors
  sdl <- initialiseSdlResources
  finalState <- runRendererWithScreenshotBroker (aaScreenshotRef actors)
    (runMainLoop runtimeCfg actors sdl)
  destroyRenderCacheState (srTexturePool sdl) finalState
  shutdownSdlResources sdl
  shutdownAppActors actors

-- | Close capture under 'finally' before propagating any normal or exceptional
-- renderer-loop exit. This also removes the async-exception window between a
-- normal loop return and broker closure.
runRendererWithScreenshotBroker :: ScreenshotRequestRef -> IO a -> IO a
runRendererWithScreenshotBroker broker action =
  action `finally` shutdownScreenshotRequestRef broker
