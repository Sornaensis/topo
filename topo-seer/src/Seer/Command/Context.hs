-- | Command context shared by all IPC command handlers.
--
-- Extracted into its own module to avoid circular dependencies between
-- 'Seer.Command.Dispatch' (which imports handlers) and the handler
-- modules (which need the 'CommandContext' type).
module Seer.Command.Context
  ( CommandContext(..)
  ) where

import Actor.UiActions (UiActions)
import Actor.UiActions.Handles (ActorHandles)
import Actor.UI.State (UiSnapshotRef)
import Hyperspace.Actor (ActorHandle, Protocol)
import Seer.Screenshot (ScreenshotRequestRef)

-- | Context shared by all command handlers.
data CommandContext = CommandContext
  { ccActorHandles    :: !ActorHandles
  , ccUiSnapshotRef   :: !UiSnapshotRef
  , ccUiActionsHandle :: !(ActorHandle UiActions (Protocol UiActions))
  , ccScreenshotRef   :: !ScreenshotRequestRef
  }
