module Spec.Support.LayeredView
  ( baseSelection
  , elevationSelection
  , biomeSelection
  , weatherSelection
  , precipitationSelection
  , cloudSelection
  , pluginSelection
  , selectionForAtlasKey
  , layeredSelectionForMode
  ) where

import Data.Text (Text)

import Actor.AtlasCache (AtlasKey(..))
import Actor.UI
  ( BaseViewMode(..)
  , LayeredViewState(..)
  , SkyOverlayMode(..)
  , ViewMode(..)
  , WeatherBasis(..)
  , defaultLayeredViewState
  )

baseSelection :: BaseViewMode -> LayeredViewState
baseSelection base = defaultLayeredViewState { lvsBaseView = base }

elevationSelection :: LayeredViewState
elevationSelection = baseSelection BaseViewElevation

biomeSelection :: LayeredViewState
biomeSelection = baseSelection BaseViewBiome

weatherSelection :: WeatherBasis -> LayeredViewState
weatherSelection basis = defaultLayeredViewState
  { lvsSkyOverlay = Just SkyOverlayWeatherTemperature
  , lvsWeatherBasis = basis
  }

precipitationSelection :: WeatherBasis -> LayeredViewState
precipitationSelection basis = defaultLayeredViewState
  { lvsSkyOverlay = Just SkyOverlayPrecipitation
  , lvsWeatherBasis = basis
  }

cloudSelection :: WeatherBasis -> LayeredViewState
cloudSelection basis = defaultLayeredViewState
  { lvsSkyOverlay = Just SkyOverlayCloud
  , lvsWeatherBasis = basis
  }

pluginSelection :: Text -> Int -> LayeredViewState
pluginSelection name fieldIndex = defaultLayeredViewState
  { lvsSkyOverlay = Just (SkyOverlayPlugin name fieldIndex)
  }

selectionForAtlasKey :: AtlasKey -> LayeredViewState
selectionForAtlasKey key = case key of
  BaseAtlasKey base _ _ -> baseSelection base
  OverlayAtlasKey overlay basis _ _ -> defaultLayeredViewState
    { lvsSkyOverlay = Just overlay
    , lvsWeatherBasis = basis
    }
  AtlasKey mode _ _ -> layeredSelectionForMode mode
  LayeredAtlasKey mode _ _ _ _ -> layeredSelectionForMode mode

-- Test fixtures that still exercise lower-level ViewMode-keyed atlas code use
-- this explicit mapping while all state and render entry points stay layered.
layeredSelectionForMode :: ViewMode -> LayeredViewState
layeredSelectionForMode mode = case mode of
  ViewElevation -> elevationSelection
  ViewBiome -> biomeSelection
  ViewClimate -> weatherSelection WeatherBasisAverage
  ViewWeather -> weatherSelection WeatherBasisCurrent
  ViewMoisture -> baseSelection BaseViewMoisture
  ViewPrecip -> precipitationSelection WeatherBasisAverage
  ViewPrecipCurrent -> precipitationSelection WeatherBasisCurrent
  ViewPlateId -> baseSelection BaseViewPlateId
  ViewPlateBoundary -> baseSelection BaseViewPlateBoundary
  ViewPlateHardness -> baseSelection BaseViewPlateHardness
  ViewPlateCrust -> baseSelection BaseViewPlateCrust
  ViewPlateAge -> baseSelection BaseViewPlateAge
  ViewPlateHeight -> baseSelection BaseViewPlateHeight
  ViewPlateVelocity -> baseSelection BaseViewPlateVelocity
  ViewVegetation -> baseSelection BaseViewVegetation
  ViewTerrainForm -> baseSelection BaseViewTerrainForm
  ViewCloud -> cloudSelection WeatherBasisCurrent
  ViewCloudTypical -> cloudSelection WeatherBasisAverage
  ViewOverlay name fieldIndex -> pluginSelection name fieldIndex
