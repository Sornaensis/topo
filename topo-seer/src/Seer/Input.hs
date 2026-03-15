module Seer.Input
  ( isQuit
  , handleEvent
  , handleClick
  , InputContext(..)
  , mkInputContext
  , TooltipHover
  , tickTooltipHover
  , tooltipDelayFrames
  ) where

import Seer.Input.Context (InputContext(..), TooltipHover, mkInputContext)
import Seer.Input.Events (handleEvent, tickTooltipHover, tooltipDelayFrames)
import Seer.Input.Router (isQuit)
import Seer.Input.Widgets (handleClick)
