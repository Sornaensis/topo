module Seer.System.EventPump
  ( EventPumpEnv(..)
  , EventPumpResult(..)
  , hasQuitEvent
  , processEvents
  , retryPublicationRace
  , coalesceMouseMotion
  ) where

import Actor.Log (LogSnapshotRef, getLogSnapshot)
import Actor.SnapshotReceiver (RenderSnapshot(..), SnapshotVersion, publishSnapshot, publishSnapshotIfVersion, readCommittedUiAndLog, uiSnapshotUpdate, withLogSnapshot)
import Actor.UI (UiState(..), UiSnapshotRef, getUiSnapshot)
import Actor.UiActions (UiActions, awaitUiActions)
import Actor.UiActions.Handles (ActorHandles(..))
import Control.Monad (forM_, when)
import Data.IORef (IORef, writeIORef)
import Data.Word (Word32)
import GHC.Clock (getMonotonicTimeNSec)
import Hyperspace.Actor (ActorHandle, Protocol)
import qualified SDL
import Seer.DataBrowser.Executor (DataBrowserExecutor)
import Seer.OverlayInspector.Executor.Types (OverlayInspectorExecutor)
import Seer.Service.AppService (AppService)
import Seer.Input (handleEvent, isQuit, modalInputBarrierVisible, tickTooltipHover)
import Seer.Input.Actions (mkInputEnv)
import Seer.Input.Context
  ( DragState
  , InputActionDispatcher
  , TooltipHover
  , drainInputMainThreadActions
  , mkInputContext
  )
import Seer.Screenshot.Request (ScreenshotRequestRef)
import Seer.Screenshot.Storage (ScreenshotStoragePolicy)
import Seer.Timing (nsToMs)

-- | Mutable state and actor endpoints needed to route SDL input.  Keeping this
-- separate from frame rendering makes it explicit that event handling may
-- update actor-backed state, while SDL rendering remains on the main thread.
data EventPumpEnv = EventPumpEnv
  { epeWindow :: !SDL.Window
  , epeAppService :: !AppService
  , epeActorHandles :: !ActorHandles
  , epeUiActionsHandle :: !(ActorHandle UiActions (Protocol UiActions))
  , epeUiSnapshotRef :: !UiSnapshotRef
  , epeScreenshotRef :: !ScreenshotRequestRef
  , epeScreenshotStoragePolicy :: !ScreenshotStoragePolicy
  , epeLogSnapshotRef :: !(Maybe LogSnapshotRef)
  , epeDataBrowserExecutor :: !DataBrowserExecutor
  , epeOverlayInspectorExecutor :: !OverlayInspectorExecutor
  , epeQuitRef :: !(IORef Bool)
  , epeLineHeightRef :: !(IORef Int)
  , epeMousePosRef :: !(IORef (Int, Int))
  , epeDragRef :: !(IORef (Maybe DragState))
  , epeTooltipHoverRef :: !(IORef TooltipHover)
  , epeModalBarrierLatchRef :: !(IORef Bool)
  , epeInputActionDispatcher :: !InputActionDispatcher
  }

hasQuitEvent :: [SDL.Event] -> Bool
hasQuitEvent = any isQuit

data EventPumpResult = EventPumpResult
  { eprElapsedMs :: !Word32
  , eprPublishedVersion :: !(Maybe SnapshotVersion)
  } deriving (Eq, Show)

processEvents :: EventPumpEnv -> Word32 -> [SDL.Event] -> RenderSnapshot -> IO EventPumpResult
processEvents env _timingLogThresholdMs events renderSnap = do
  -- A rendered modal barriers this entire SDL batch even if an early event
  -- closes the actor-owned state. The next rendered snapshot may release it.
  let renderedModalBarrier = modalInputBarrierVisible (rsUi renderSnap)
  when renderedModalBarrier $
    writeIORef (epeModalBarrierLatchRef env) True
  -- SDL APIs requested by completed semantic actions are always marshalled
  -- back onto the event-pump thread, including during otherwise idle frames.
  drainInputMainThreadActions (epeInputActionDispatcher env)
  if null events
    then do
      -- Idle tooltip activation is a same-loop publication just like an SDL
      -- handler mutation.
      fired <- tickTooltipHover (epeTooltipHoverRef env) (ahUiHandle (epeActorHandles env))
      published <- if fired
        then publishSnapshotFromEnvIfChanged env renderSnap
        else pure Nothing
      when renderedModalBarrier $
        writeIORef (epeModalBarrierLatchRef env) False
      pure EventPumpResult
        { eprElapsedMs = 0
        , eprPublishedVersion = published
        }
    else do
      handleStart <- getMonotonicTimeNSec
      let coalescedEvents = coalesceMouseMotion events
          actorHandles = epeActorHandles env
          inputEnv = mkInputEnv
            (epeAppService env)
            actorHandles
            (epeUiActionsHandle env)
            (epeUiSnapshotRef env)
            (epeScreenshotRef env)
            (epeScreenshotStoragePolicy env)
            (epeLogSnapshotRef env)
            (epeDataBrowserExecutor env)
            (epeOverlayInspectorExecutor env)
            (rsUi renderSnap)
            (rsLog renderSnap)
            (rsData renderSnap)
            (rsTerrain renderSnap)
      let inputContext = mkInputContext
            (epeWindow env)
            inputEnv
            (epeQuitRef env)
            (epeLineHeightRef env)
            (epeMousePosRef env)
            (epeDragRef env)
            (epeTooltipHoverRef env)
            (epeModalBarrierLatchRef env)
            (epeInputActionDispatcher env)
      forM_ coalescedEvents (handleEvent inputContext)
      drainInputMainThreadActions (epeInputActionDispatcher env)
      _ <- tickTooltipHover (epeTooltipHoverRef env) (ahUiHandle actorHandles)
      -- A delegated UiActions mutation may publish terrain and enqueue atlas
      -- work after its UI setter. Wait for actor ownership to finish before
      -- comparing direct SDL mutations against the committed snapshot.
      awaitUiActions (epeUiActionsHandle env)
      published <- publishSnapshotFromEnvIfChanged env renderSnap
      handleEnd <- getMonotonicTimeNSec
      when renderedModalBarrier $
        writeIORef (epeModalBarrierLatchRef env) False
      pure EventPumpResult
        { eprElapsedMs = nsToMs handleStart handleEnd
        , eprPublishedVersion = published
        }

-- | Repeat a conditional publication attempt after any lost race. 'Right'
-- means the mutation is represented (with or without a new local version).
retryPublicationRace :: IO (Either () a) -> IO a
retryPublicationRace attempt = do
  result <- attempt
  case result of
    Left () -> retryPublicationRace attempt
    Right value -> pure value

publishSnapshotFromEnvIfChanged :: EventPumpEnv -> RenderSnapshot -> IO (Maybe SnapshotVersion)
publishSnapshotFromEnvIfChanged env previous = retryPublicationRace attempt
  where
    handles = epeActorHandles env

    -- Capture actor state against an observed committed epoch. If another
    -- publisher wins, recapture instead of overwriting it with stale domains.
    attempt = do
      committed <- readCommittedUiAndLog (ahSnapshotVersionRef handles)
      uiSnapshot <- getUiSnapshot (ahUiHandle handles)
      logSnapshot <- getLogSnapshot (ahLogHandle handles)
      case committed of
        Nothing ->
          if uiSnapshot /= rsUi previous || logSnapshot /= rsLog previous
            then Right . Just <$> publishSnapshot
              (ahSnapshotVersionRef handles)
              (withLogSnapshot logSnapshot (uiSnapshotUpdate uiSnapshot))
            else pure (Right Nothing)
        Just (expected, committedUi, committedLog)
          | uiSnapshot == committedUi && logSnapshot == committedLog -> pure (Right Nothing)
          | otherwise -> do
              published <- publishSnapshotIfVersion
                (ahSnapshotVersionRef handles)
                expected
                (withLogSnapshot logSnapshot (uiSnapshotUpdate uiSnapshot))
              pure $ case published of
                Just version -> Right (Just version)
                Nothing -> Left ()

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
