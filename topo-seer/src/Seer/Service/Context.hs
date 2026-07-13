{-# LANGUAGE StrictData #-}

-- | Runtime context shared by service-layer operations.
--
-- This context is owned by the service boundary rather than internal command
-- IPC compatibility. The command layer can adapt its existing
-- 'Seer.Command.Context.CommandContext' into this shape while HTTP, UI, and
-- direct tests can construct it without depending on the command dispatcher.
module Seer.Service.Context
  ( ServiceContext(..)
  ) where

import Actor.Log (LogSnapshotRef)
import Actor.UiActions (UiActions)
import Actor.UiActions.Handles (ActorHandles)
import Actor.UI.State (UiSnapshotRef)
import Hyperspace.Actor (ActorHandle, Protocol)
import Seer.Screenshot.Request (ScreenshotRequestRef)
import Seer.Screenshot.Storage (ScreenshotStoragePolicy)
import Seer.Service.Events (ServiceEventBus)

-- | Application host handles needed by concrete service implementations.
data ServiceContext = ServiceContext
  { svcActorHandles :: !ActorHandles
  , svcUiSnapshotRef :: !UiSnapshotRef
  , svcUiActionsHandle :: !(ActorHandle UiActions (Protocol UiActions))
  , svcScreenshotRef :: !ScreenshotRequestRef
  , svcScreenshotStoragePolicy :: !ScreenshotStoragePolicy
    -- ^ Canonical, immutable authority for optional screenshot persistence.
  , svcLogSnapshotRef :: !(Maybe LogSnapshotRef)
    -- ^ Log snapshot for log reads. 'Nothing' is allowed for focused tests.
  , svcEventBus :: !(Maybe ServiceEventBus)
    -- ^ Optional HTTP/event-stream buffer. 'Nothing' keeps focused tests lightweight.
  }
