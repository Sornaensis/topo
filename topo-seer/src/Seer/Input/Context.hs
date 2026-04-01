-- | Runtime input context shared between event and widget handlers.
module Seer.Input.Context
  ( DragState(..)
  , TooltipHover
  , InputContext(..)
  , mkInputContext
  ) where

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