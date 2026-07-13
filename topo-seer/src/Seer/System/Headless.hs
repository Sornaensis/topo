module Seer.System.Headless
  ( runHeadlessHttp
  ) where

import Seer.Config.Runtime (defaultConfig, loadConfig)
import Seer.Headless
  ( HeadlessConfig(..)
  , defaultHeadlessConfig
  , headlessServiceContext
  , withHeadlessApp
  )
import Seer.HTTP.Server
  ( headlessHttpAppService
  , runHttpServer
  )
import Seer.System.Runtime (RuntimeOptions(..))

runHeadlessHttp :: RuntimeOptions -> IO ()
runHeadlessHttp opts =
  case roHttp opts of
    Nothing -> fail "--headless requires --http HOST:PORT"
    Just httpCfg -> do
      runtimeCfg <- if roTestMode opts then pure defaultConfig else loadConfig
      let headlessCfg = defaultHeadlessConfig { hcRuntimeConfig = runtimeCfg }
      withHeadlessApp headlessCfg $ \app ->
        runHttpServer httpCfg headlessHttpAppService (headlessServiceContext app)
