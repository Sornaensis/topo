-- | Runtime input context shared between event and widget handlers.
module Seer.Input.Context
  ( DragState(..)
  , TooltipHover
  , InputContext(..)
  , mkInputContext
  ) where

import Data.IORef (IORef)
import qualified SDL
import Seer.Input.Actions (InputEnv)
import UI.WidgetTree (WidgetId)

data DragState = DragState
  { dsStart :: !(Int, Int)
  , dsLast :: !(Int, Int)
  , dsDragging :: !Bool
  }

-- | Pending tooltip hover: which widget is under the cursor and how
-- many frames remain before the tooltip fires.
type TooltipHover = Maybe (WidgetId, Int)

-- | Runtime context for one pass through input handling.
data InputContext = InputContext
  { icWindow :: !SDL.Window
  , icInputEnv :: !InputEnv
  , icQuitRef :: !(IORef Bool)
  , icLineHeightRef :: !(IORef Int)
  , icMousePosRef :: !(IORef (Int, Int))
  , icDragRef :: !(IORef (Maybe DragState))
  , icTooltipHoverRef :: !(IORef TooltipHover)
  }

mkInputContext
  :: SDL.Window
  -> InputEnv
  -> IORef Bool
  -> IORef Int
  -> IORef (Int, Int)
  -> IORef (Maybe DragState)
  -> IORef TooltipHover
  -> InputContext
mkInputContext window inputEnv quitRef lineHeightRef mousePosRef dragRef tooltipHoverRef =
  InputContext
    { icWindow = window
    , icInputEnv = inputEnv
    , icQuitRef = quitRef
    , icLineHeightRef = lineHeightRef
    , icMousePosRef = mousePosRef
    , icDragRef = dragRef
    , icTooltipHoverRef = tooltipHoverRef
    }