-- | SDL event routing helpers.
module Seer.Input.Router
  ( isQuit
  ) where

import qualified SDL

-- | Detect quit events.
isQuit :: SDL.Event -> Bool
isQuit event =
  case SDL.eventPayload event of
    SDL.QuitEvent -> True
    _ -> False
