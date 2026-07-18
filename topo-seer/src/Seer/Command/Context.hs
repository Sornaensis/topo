-- | Command context shared by in-process command handlers.
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
import Seer.DataBrowser.Executor (DataBrowserExecutor)
import Seer.OverlayInspector.Executor.Types (OverlayInspectorExecutor)
import Seer.Service.Context (NestedServiceRunner, ServiceContext(..))
import Seer.Screenshot.Request (ScreenshotRequestRef)
import Seer.Screenshot.Storage (ScreenshotStoragePolicy)

-- | Context shared by all command handlers.
data CommandContext = CommandContext
  { ccNestedServiceRunner :: !NestedServiceRunner
  , ccActorHandles    :: !ActorHandles
  , ccUiSnapshotRef   :: !UiSnapshotRef
  , ccUiActionsHandle :: !(ActorHandle UiActions (Protocol UiActions))
  , ccScreenshotRef   :: !ScreenshotRequestRef
  , ccScreenshotStoragePolicy :: !ScreenshotStoragePolicy
  , ccLogSnapshotRef  :: !(Maybe LogSnapshotRef)
  , ccDataBrowserExecutor :: !DataBrowserExecutor
  , ccOverlayInspectorExecutor :: !OverlayInspectorExecutor
    -- ^ Log snapshot for @get_logs@.  'Nothing' only in tests.
  }

-- | Adapt the in-process command context into the service-layer context.
--
-- Future command adapters should perform command-envelope parsing and then call
-- AppService operations with this transport-neutral context.
commandServiceContext :: CommandContext -> ServiceContext
commandServiceContext ctx = ServiceContext
  { svcNestedServiceRunner = ccNestedServiceRunner ctx
  , svcActorHandles = ccActorHandles ctx
  , svcUiSnapshotRef = ccUiSnapshotRef ctx
  , svcUiActionsHandle = ccUiActionsHandle ctx
  , svcScreenshotRef = ccScreenshotRef ctx
  , svcScreenshotStoragePolicy = ccScreenshotStoragePolicy ctx
  , svcLogSnapshotRef = ccLogSnapshotRef ctx
  , svcEventBus = Nothing
  , svcDataBrowserExecutor = ccDataBrowserExecutor ctx
  , svcOverlayInspectorExecutor = ccOverlayInspectorExecutor ctx
  }

-- | Adapt service-layer context back into the legacy command-handler context.
--
-- This keeps the temporary command-backed AppService implementation isolated
-- while focused service implementations are extracted from command handlers.
serviceCommandContext :: ServiceContext -> CommandContext
serviceCommandContext ctx = CommandContext
  { ccNestedServiceRunner = svcNestedServiceRunner ctx
  , ccActorHandles = svcActorHandles ctx
  , ccUiSnapshotRef = svcUiSnapshotRef ctx
  , ccUiActionsHandle = svcUiActionsHandle ctx
  , ccScreenshotRef = svcScreenshotRef ctx
  , ccScreenshotStoragePolicy = svcScreenshotStoragePolicy ctx
  , ccLogSnapshotRef = svcLogSnapshotRef ctx
  , ccDataBrowserExecutor = svcDataBrowserExecutor ctx
  , ccOverlayInspectorExecutor = svcOverlayInspectorExecutor ctx
  }
