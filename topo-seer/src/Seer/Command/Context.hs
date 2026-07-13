-- | Command context shared by internal/test compatibility IPC handlers.
--
-- Extracted into its own module to avoid circular dependencies between
-- 'Seer.Command.Dispatch' (which imports handlers) and the handler
-- modules (which need the 'CommandContext' type).
module Seer.Command.Context
  ( CommandContext(..)
  , commandServiceContext
  , serviceCommandContext
  ) where

import Actor.Log (LogSnapshotRef)
import Actor.UiActions (UiActions)
import Actor.UiActions.Handles (ActorHandles)
import Actor.UI.State (UiSnapshotRef)
import Hyperspace.Actor (ActorHandle, Protocol)
import Seer.Service.Context (ServiceContext(..))
import Seer.Screenshot.Request (ScreenshotRequestRef)
import Seer.Screenshot.Storage (ScreenshotStoragePolicy)

-- | Context shared by all command handlers.
data CommandContext = CommandContext
  { ccActorHandles    :: !ActorHandles
  , ccUiSnapshotRef   :: !UiSnapshotRef
  , ccUiActionsHandle :: !(ActorHandle UiActions (Protocol UiActions))
  , ccScreenshotRef   :: !ScreenshotRequestRef
  , ccScreenshotStoragePolicy :: !ScreenshotStoragePolicy
  , ccLogSnapshotRef  :: !(Maybe LogSnapshotRef)
    -- ^ Log snapshot for @get_logs@.  'Nothing' only in tests.
  }

-- | Adapt the existing internal command IPC context into the service-layer context.
--
-- Future command adapters should perform command-envelope parsing and then call
-- AppService operations with this transport-neutral context.
commandServiceContext :: CommandContext -> ServiceContext
commandServiceContext ctx = ServiceContext
  { svcActorHandles = ccActorHandles ctx
  , svcUiSnapshotRef = ccUiSnapshotRef ctx
  , svcUiActionsHandle = ccUiActionsHandle ctx
  , svcScreenshotRef = ccScreenshotRef ctx
  , svcScreenshotStoragePolicy = ccScreenshotStoragePolicy ctx
  , svcLogSnapshotRef = ccLogSnapshotRef ctx
  , svcEventBus = Nothing
  }

-- | Adapt service-layer context back into the legacy command-handler context.
--
-- This keeps the temporary command-backed AppService implementation isolated
-- while focused service implementations are extracted from command handlers.
serviceCommandContext :: ServiceContext -> CommandContext
serviceCommandContext ctx = CommandContext
  { ccActorHandles = svcActorHandles ctx
  , ccUiSnapshotRef = svcUiSnapshotRef ctx
  , ccUiActionsHandle = svcUiActionsHandle ctx
  , ccScreenshotRef = svcScreenshotRef ctx
  , ccScreenshotStoragePolicy = svcScreenshotStoragePolicy ctx
  , ccLogSnapshotRef = svcLogSnapshotRef ctx
  }
