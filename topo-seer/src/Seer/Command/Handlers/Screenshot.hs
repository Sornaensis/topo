{-# LANGUAGE OverloadedStrings #-}

-- | Command-envelope handler for @take_screenshot@.
--
-- Screenshot behavior lives at the AppService boundary; this adapter only
-- translates the shared structured result into the legacy command envelope.
module Seer.Command.Handlers.Screenshot
  ( handleTakeScreenshot
  ) where

import Data.Aeson (Value)

import Seer.Command.Context (CommandContext, commandServiceContext)
import Seer.Service.Screenshot (rendererScreenshotHandler)
import Seer.Service.Types
  ( ServiceRequest(..)
  , ServiceResponse(..)
  , runServiceHandler
  , serviceErrorText
  )
import Topo.Command.Types (SeerResponse, errResponse, okResponse)

handleTakeScreenshot :: CommandContext -> Int -> Value -> IO SeerResponse
handleTakeScreenshot ctx requestId params = do
  result <- runServiceHandler rendererScreenshotHandler
    (commandServiceContext ctx)
    (ServiceRequest (Just params))
  pure $ case result of
    Right response -> okResponse requestId (serviceResponseBody response)
    Left err -> errResponse requestId (serviceErrorText err)
