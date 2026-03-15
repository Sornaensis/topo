-- | Shared setter/update helpers for registry-driven config sliders.
module Seer.Config.SliderState
  ( bumpSliderValue
  , resetSliderDefaults
  , setSliderValue
  ) where

import Actor.UI
  ( Ui
  , getUiSnapshot
  )
import Hyperspace.Actor (ActorHandle, Protocol)
import Seer.Config.SliderRegistry (SliderDef(..), SliderId(..), allSliderDefs)
import qualified Seer.Config.SliderAccess as SliderAccess
import Seer.Config.SliderUi (defaultSliderValueForId, sliderValueForId)

-- | Set a single slider value through the UI actor compatibility facade.
setSliderValue :: ActorHandle Ui (Protocol Ui) -> SliderId -> Float -> IO ()
setSliderValue = SliderAccess.setSliderValue

-- | Bump a slider by a delta using the current UI snapshot as the source.
bumpSliderValue :: ActorHandle Ui (Protocol Ui) -> SliderId -> Float -> IO ()
bumpSliderValue uiHandle sliderIdValue delta = do
  uiSnap <- getUiSnapshot uiHandle
  setSliderValue uiHandle sliderIdValue (sliderValueForId uiSnap sliderIdValue + delta)

resetSliderDefaults :: ActorHandle Ui (Protocol Ui) -> IO ()
resetSliderDefaults uiHandle =
  mapM_
    (\sliderDef ->
      setSliderValue uiHandle (sliderId sliderDef) (defaultSliderValueForId (sliderId sliderDef)))
    allSliderDefs