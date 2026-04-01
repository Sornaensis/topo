module Seer.Input
  ( isQuit
  , handleEvent
  , handleClick
  , InputContext(..)
  , mkInputContext
  , TooltipHover
  , tickTooltipHover
  , tooltipDelayMs
  ) where

import Seer.Input.Context (InputContext(..), TooltipHover, mkInputContext)
import Seer.Input.Events (handleEvent, tickTooltipHover, tooltipDelayMs)
import Seer.Input.Router (isQuit)
import Seer.Input.Widgets (handleClick)
