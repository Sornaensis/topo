-- | Helpers for config and log scroll calculations.
module Seer.Input.ConfigScroll
  ( ScrollSettings(..)
  , defaultScrollSettings
  , configRowCount
  , computeScrollUpdates
  ) where

import Actor.Log (LogSnapshot(..))
import Actor.UI (ConfigTab(..), UiState(..))
import Linear (V2(..))
import Seer.Draw (seedMaxDigits)
import UI.Layout
import UI.Widgets (Rect(..), containsPoint)

-- | Scroll configuration for the UI panel and log view.
data ScrollSettings = ScrollSettings
  { ssRowHeight :: !Int
  , ssRowGap :: !Int
  , ssLogStep :: !Int
  , ssSeedBaseWidth :: !Int
  , ssSeedDigitWidth :: !Int
  }

-- | Default scroll configuration used by the input layer.
defaultScrollSettings :: ScrollSettings
defaultScrollSettings = ScrollSettings
  { ssRowHeight = 24
  , ssRowGap = 10
  , ssLogStep = 3
  , ssSeedBaseWidth = 120
  , ssSeedDigitWidth = 10
  }

-- | Total number of config rows for each tab.
configRowCount :: ConfigTab -> Int
configRowCount ConfigTerrain = 44
configRowCount ConfigClimate = 23
configRowCount ConfigErosion = 5

-- | Compute scroll updates for config panel and log view.
computeScrollUpdates
  :: ScrollSettings
  -> UiState
  -> LogSnapshot
  -> Int
  -> V2 Int
  -> V2 Int
  -> Int
  -> (Maybe Int, Maybe Int)
computeScrollUpdates settings uiSnap logSnap lineHeight (V2 winW winH) (V2 mx my) dy =
  let logHeight = if lsCollapsed logSnap then 24 else 160
      seedWidth = max (ssSeedBaseWidth settings) (seedMaxDigits * ssSeedDigitWidth settings)
      layout = layoutForSeed (V2 (fromIntegral winW) (fromIntegral winH)) logHeight seedWidth
      Rect (V2 lx ly, V2 lw bodyH) = logBodyRect layout
      scrollArea = configScrollAreaRect layout
      rowHeight = ssRowHeight settings
      gap = ssRowGap settings
      rows = configRowCount (uiConfigTab uiSnap)
      contentHeight = max rowHeight (configRowTopPad + rows * rowHeight + max 0 (rows - 1) * gap)
      Rect (V2 _ sy, V2 _ sh) = scrollArea
      maxConfigOffset = max 0 (contentHeight - sh)
      configDelta = if dy > 0 then -(rowHeight + gap) else rowHeight + gap
      newConfigScroll = max 0 (min maxConfigOffset (uiConfigScroll uiSnap + configDelta))
      total = length (lsEntries logSnap)
      visible = max 1 (bodyH `div` max 1 lineHeight)
      maxOffset = max 0 (total - visible)
      delta = if dy > 0 then negate (ssLogStep settings) else ssLogStep settings
      newScroll = max 0 (min maxOffset (lsScroll logSnap + delta))
      inLog = mx >= lx && mx <= lx + lw && my >= ly && my <= ly + bodyH
      inConfigScroll = uiShowConfig uiSnap && containsPoint scrollArea (V2 mx my)
      configUpdate = if inConfigScroll then Just newConfigScroll else Nothing
      logUpdate = if inLog then Just newScroll else Nothing
  in (configUpdate, logUpdate)
