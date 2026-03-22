{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Handler for the @generate@ command — triggers terrain generation.
module Seer.Command.Handlers.Generate
  ( handleGenerate
  ) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)

import Actor.Terrain (TerrainReplyOps)
import Actor.UiActions
  ( UiAction(..)
  , UiActionRequest(..)
  , submitUiAction
  )
import Hyperspace.Actor (replyTo)
import Seer.Command.Context (CommandContext(..))
import Topo.Command.Types (SeerResponse, okResponse)

-- | Handle @generate@ — trigger terrain generation using the current
-- seed and slider values.
--
-- This submits a 'UiActionGenerate' to the 'UiActions' actor, which
-- performs the same action as pressing the Generate button in the UI.
-- The command returns immediately with @"status": "generating"@; the
-- actual terrain generation runs asynchronously.
handleGenerate :: CommandContext -> Int -> Value -> IO SeerResponse
handleGenerate ctx reqId _params = do
  let actorHandles = ccActorHandles ctx
      uiActionsH   = ccUiActionsHandle ctx
      request = UiActionRequest
        { uarAction         = UiActionGenerate
        , uarActorHandles   = actorHandles
        , uarTerrainReplyTo = replyTo @TerrainReplyOps uiActionsH
        }
  submitUiAction uiActionsH request
  pure $ okResponse reqId $ object ["status" .= ("generating" :: Text)]
