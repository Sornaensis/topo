-- | Shared getter/setter routing for registry-backed config sliders.
module Seer.Config.SliderAccess
  ( setSliderValue
  , sliderValueForId
  ) where

import Actor.UI (Ui, setUiSliderValue)
import Actor.UI.State (UiState, sliderValueForId)
import Hyperspace.Actor (ActorHandle, Protocol)
import Seer.Config.SliderRegistry (SliderId(..))

setSliderValue :: ActorHandle Ui (Protocol Ui) -> SliderId -> Float -> IO ()
setSliderValue uiHandle sliderIdValue = setUiSliderValue uiHandle sliderIdValue