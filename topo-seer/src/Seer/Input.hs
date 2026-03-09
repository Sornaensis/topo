module Seer.Input
  ( isQuit
  , handleEvent
  , handleClick
  , TooltipHover
  , tickTooltipHover
  , tooltipDelayFrames
  ) where

import Seer.Input.Events (TooltipHover, handleEvent, tickTooltipHover, tooltipDelayFrames)
import Seer.Input.Router (isQuit)
import Seer.Input.Widgets (handleClick)
