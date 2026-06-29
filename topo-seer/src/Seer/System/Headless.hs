module Seer.System.Headless
  ( runHeadlessHttp
  ) where

import Seer.Headless
  ( defaultHeadlessConfig
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
    Just httpCfg -> withHeadlessApp defaultHeadlessConfig $ \app ->
      runHttpServer httpCfg headlessHttpAppService (headlessServiceContext app)
