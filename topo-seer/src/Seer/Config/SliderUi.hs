module Seer.Config.SliderUi
  ( configTabForSliderTab
  , defaultSliderValueForId
  , sliderDefsForConfigTab
  , sliderValueForId
  ) where

import Actor.UI (ConfigTab(..), UiState)
import qualified Seer.Config.SliderAccess as SliderAccess
import Seer.Config.SliderRegistry (SliderDef, SliderId, SliderTab(..), sliderDefaultValueForId, sliderDefsForTab)

configTabForSliderTab :: SliderTab -> ConfigTab
configTabForSliderTab sliderTabValue = case sliderTabValue of
  SliderTabTerrain -> ConfigTerrain
  SliderTabPlanet -> ConfigPlanet
  SliderTabClimate -> ConfigClimate
  SliderTabWeather -> ConfigWeather
  SliderTabBiome -> ConfigBiome
  SliderTabErosion -> ConfigErosion

sliderDefsForConfigTab :: ConfigTab -> [SliderDef]
sliderDefsForConfigTab configTab = case configTab of
  ConfigTerrain -> sliderDefsForTab SliderTabTerrain
  ConfigPlanet -> sliderDefsForTab SliderTabPlanet
  ConfigClimate -> sliderDefsForTab SliderTabClimate
  ConfigWeather -> sliderDefsForTab SliderTabWeather
  ConfigBiome -> sliderDefsForTab SliderTabBiome
  ConfigErosion -> sliderDefsForTab SliderTabErosion
  ConfigPipeline -> []
  ConfigData -> []

defaultSliderValueForId :: SliderId -> Float
defaultSliderValueForId = sliderDefaultValueForId

sliderValueForId :: UiState -> SliderId -> Float
sliderValueForId = SliderAccess.sliderValueForId