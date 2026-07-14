module Seer.System.EventPump
  ( EventPumpEnv(..)
  , hasQuitEvent
  , processEvents
  , coalesceMouseMotion
  ) where

import Actor.Log (LogEntry(..), LogLevel(..), LogSnapshotRef, appendLog)
import Actor.Render (RenderSnapshot(..))
import Actor.SnapshotReceiver (invalidatePublishedSnapshot)
import Actor.UI (UiSnapshotRef, getUiSnapshot)
import Actor.UiActions (UiActions)
import Actor.UiActions.Handles (ActorHandles(..))
import Control.Monad (forM_, when)
import Data.IORef (IORef)
import qualified Data.Text as Text
import Data.Word (Word32)
import GHC.Clock (getMonotonicTimeNSec)
import Hyperspace.Actor (ActorHandle, Protocol)
import qualified SDL
import Seer.Input (handleEvent, isQuit, tickTooltipHover)
import Seer.Input.Actions (mkInputEnv)
import Seer.Input.Context (DragState, TooltipHover, mkInputContext)
import Seer.Screenshot.Request (ScreenshotRequestRef)
import Seer.Screenshot.Storage (ScreenshotStoragePolicy)
import Seer.Timing (nsToMs)

-- | Mutable state and actor endpoints needed to route SDL input.  Keeping this
-- separate from frame rendering makes it explicit that event handling may
-- update actor-backed state, while SDL rendering remains on the main thread.
data EventPumpEnv = EventPumpEnv
  { epeWindow :: !SDL.Window
  , epeActorHandles :: !ActorHandles
  , epeUiActionsHandle :: !(ActorHandle UiActions (Protocol UiActions))
  , epeUiSnapshotRef :: !UiSnapshotRef
  , epeScreenshotRef :: !ScreenshotRequestRef
  , epeScreenshotStoragePolicy :: !ScreenshotStoragePolicy
  , epeLogSnapshotRef :: !(Maybe LogSnapshotRef)
  , epeQuitRef :: !(IORef Bool)
  , epeLineHeightRef :: !(IORef Int)
  , epeMousePosRef :: !(IORef (Int, Int))
  , epeDragRef :: !(IORef (Maybe DragState))
  , epeTooltipHoverRef :: !(IORef TooltipHover)
  }

hasQuitEvent :: [SDL.Event] -> Bool
hasQuitEvent = any isQuit

processEvents :: EventPumpEnv -> Word32 -> [SDL.Event] -> RenderSnapshot -> IO Word32
processEvents env timingLogThresholdMs events renderSnap =
  if null events
    then do
      -- Even when idle, tick the tooltip frame counter; if it fires we bump the
      -- snapshot version so the render loop picks up the updated UI state on
      -- the next frame.
      fired <- tickTooltipHover (epeTooltipHoverRef env) (ahUiHandle (epeActorHandles env))
      when fired $
        invalidateSnapshotFromEnv env
      pure 0
    else do
      handleStart <- getMonotonicTimeNSec
      let coalescedEvents = coalesceMouseMotion events
          actorHandles = epeActorHandles env
          inputEnv = mkInputEnv
            actorHandles
            (epeUiActionsHandle env)
            (epeUiSnapshotRef env)
            (epeScreenshotRef env)
            (epeScreenshotStoragePolicy env)
            (epeLogSnapshotRef env)
            (rsUi renderSnap)
            (rsLog renderSnap)
            (rsData renderSnap)
            (rsTerrain renderSnap)
          inputContext = mkInputContext
            (epeWindow env)
            inputEnv
            (epeQuitRef env)
            (epeLineHeightRef env)
            (epeMousePosRef env)
            (epeDragRef env)
            (epeTooltipHoverRef env)
      forM_ coalescedEvents (handleEvent inputContext)
      _ <- tickTooltipHover (epeTooltipHoverRef env) (ahUiHandle actorHandles)
      afterEvents <- getMonotonicTimeNSec
      invalidateSnapshotFromEnv env
      handleEnd <- getMonotonicTimeNSec
      let elapsed = nsToMs handleStart handleEnd
          eventsMs = nsToMs handleStart afterEvents
          uiSnapMs = nsToMs afterEvents handleEnd
      when (elapsed >= timingLogThresholdMs) $
        appendLog (ahLogHandle actorHandles) (LogEntry LogInfo (Text.pack
          ("handle events took " <> show elapsed <> "ms [" <> show (length events) <> " raw, " <> show (length coalescedEvents) <> " coalesced] dispatch=" <> show eventsMs <> "ms uiSnap=" <> show uiSnapMs <> "ms")))
      pure elapsed

invalidateSnapshotFromEnv :: EventPumpEnv -> IO ()
invalidateSnapshotFromEnv env = do
  let handles = epeActorHandles env
  -- Drain preceding UI casts before committing their invalidation epoch.
  _ <- getUiSnapshot (ahUiHandle handles)
  _ <- invalidatePublishedSnapshot (ahSnapshotVersionRef handles)
  pure ()

-- | Coalesce consecutive mouse motion events, keeping only the last one in
-- each run of consecutive motions.  Non-motion events retain their original
-- relative order.  This prevents thousands of queued motion events from
-- flooding the Ui actor mailbox and causing back-pressure stalls on the render
-- thread.  It also fixes a pan-drift bug where intermediate motions overwrote
-- each other's @setUiPanOffset@ casts against a stale cached snapshot.
coalesceMouseMotion :: [SDL.Event] -> [SDL.Event]
coalesceMouseMotion = go
  where
    go []  = []
    go [x] = [x]
    go (x : xs@(y : _))
      | isMotion x, isMotion y = go xs
      | otherwise              = x : go xs

    isMotion :: SDL.Event -> Bool
    isMotion e = case SDL.eventPayload e of
      SDL.MouseMotionEvent _ -> True
      _                      -> False
