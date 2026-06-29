module Seer.System
  ( runApp
  ) where

import Seer.Config.Runtime (loadConfig)
import Seer.System.Actors
  ( initialiseAppActors
  , shutdownAppActors
  , startCommandServices
  )
import Seer.System.Cache (destroyRenderCacheState)
import Seer.System.Headless (runHeadlessHttp)
import Seer.System.MainLoop (runMainLoop)
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
  finalState <- runMainLoop runtimeCfg actors sdl
  destroyRenderCacheState (srTexturePool sdl) finalState
  shutdownSdlResources sdl
  shutdownAppActors actors
