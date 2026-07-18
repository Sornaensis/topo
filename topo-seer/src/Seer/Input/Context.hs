-- | Runtime input context shared between event and widget handlers.
module Seer.Input.Context
  ( DragState(..)
  , TooltipHover
  , InputActionDispatcher
  , newInputActionDispatcher
  , enqueueInputAction
  , enqueueInputMainThreadAction
  , drainInputMainThreadActions
  , InputContext(..)
  , mkInputContext
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
  ( TQueue
  , atomically
  , flushTQueue
  , newTQueueIO
  , readTQueue
  , writeTQueue
  )
import Control.Exception (SomeException, catch)
import Control.Monad (forever, void)
import Data.IORef (IORef)
import Data.Word (Word32)
import qualified SDL
import Seer.Input.Actions (InputEnv)
import UI.WidgetTree (WidgetId)

data DragState = DragState
  { dsStart :: !(Int, Int)
  , dsLast :: !(Int, Int)
  , dsDragging :: !Bool
  }

-- | Pending tooltip hover: which widget is under the cursor and the
-- absolute SDL tick deadline (ms) after which the tooltip fires.
-- A deadline of 0 means the tooltip has already fired and is visible.
type TooltipHover = Maybe (WidgetId, Word32)

-- | FIFO worker for semantic input actions plus callbacks that must return to
-- the SDL thread. One dispatcher is owned by each running application.
data InputActionDispatcher = InputActionDispatcher
  { iadActions :: !(TQueue (IO ()))
  , iadMainThreadActions :: !(TQueue (IO ()))
  }

newInputActionDispatcher :: IO InputActionDispatcher
newInputActionDispatcher = do
  actions <- newTQueueIO
  mainThreadActions <- newTQueueIO
  let dispatcher = InputActionDispatcher actions mainThreadActions
  void $ forkIO $ forever $ do
    action <- atomically (readTQueue actions)
    action `catch` ignoreQueuedException
  pure dispatcher
  where
    -- A failed input action must not terminate the application-owned worker.
    ignoreQueuedException :: SomeException -> IO ()
    ignoreQueuedException _ = pure ()

enqueueInputAction :: InputActionDispatcher -> IO () -> IO ()
enqueueInputAction dispatcher action =
  atomically (writeTQueue (iadActions dispatcher) action)

enqueueInputMainThreadAction :: InputActionDispatcher -> IO () -> IO ()
enqueueInputMainThreadAction dispatcher action =
  atomically (writeTQueue (iadMainThreadActions dispatcher) action)

-- | Run completed SDL callbacks in FIFO order on the event-pump thread.
drainInputMainThreadActions :: InputActionDispatcher -> IO ()
drainInputMainThreadActions dispatcher = do
  actions <- atomically (flushTQueue (iadMainThreadActions dispatcher))
  sequence_ actions

-- | Runtime context for one pass through input handling.
data InputContext = InputContext
  { icWindow :: !SDL.Window
  , icInputEnv :: !InputEnv
  , icQuitRef :: !(IORef Bool)
  , icLineHeightRef :: !(IORef Int)
  , icMousePosRef :: !(IORef (Int, Int))
  , icDragRef :: !(IORef (Maybe DragState))
  , icTooltipHoverRef :: !(IORef TooltipHover)
  , icModalBarrierLatchRef :: !(IORef Bool)
  , icActionDispatcher :: !InputActionDispatcher
  }

mkInputContext
  :: SDL.Window
  -> InputEnv
  -> IORef Bool
  -> IORef Int
  -> IORef (Int, Int)
  -> IORef (Maybe DragState)
  -> IORef TooltipHover
  -> IORef Bool
  -> InputActionDispatcher
  -> InputContext
mkInputContext window inputEnv quitRef lineHeightRef mousePosRef dragRef tooltipHoverRef modalBarrierLatchRef actionDispatcher =
  InputContext
    { icWindow = window
    , icInputEnv = inputEnv
    , icQuitRef = quitRef
    , icLineHeightRef = lineHeightRef
    , icMousePosRef = mousePosRef
    , icDragRef = dragRef
    , icTooltipHoverRef = tooltipHoverRef
    , icModalBarrierLatchRef = modalBarrierLatchRef
    , icActionDispatcher = actionDispatcher
    }
