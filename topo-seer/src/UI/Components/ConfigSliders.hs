{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Pure tab and slider helpers for the non-pipeline config-panel tabs.
--
-- This module keeps the terrain/planet/climate/weather/biome/erosion slider
-- view model, draw commands, hit regions, tooltip metadata, validation
-- display, and input stepping together.  Pipeline/plugin/simulation controls
-- live in 'UI.Components.PipelineControls'; data-browser controls remain in
-- their bespoke panel code.
module UI.Components.ConfigSliders
  ( SliderId(..)
  , SliderPart(..)
  , ConfigTabView(..)
  , ConfigSliderRowView(..)
  , ConfigSliderValidation(..)
  , configTabViews
  , configTabDrawCommands
  , configTabLabelCommands
  , configSliderRowsForTab
  , configSliderRowsForActiveTab
  , configSliderRowsScrolled
  , configSliderValidations
  , configSliderDrawCommands
  , configSliderLabelCommands
  , configSliderValidationCommands
  , configSliderInputValue
  , configSliderInputValueForId
  , configSliderInputValueForRow
  ) where

import Actor.UI (ConfigTab(..), UiState(..), configRowCount)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word8)
import Linear (V2(..), V4)
import Seer.Config.SliderRegistry
  ( SliderDef(..)
  , SliderId(..)
  , SliderPart(..)
  , sliderMinusWidgetId
  , sliderPlusWidgetId
  )
import Seer.Config.SliderSpec (sliderLabelForId, tooltipForWidget)
import Seer.Config.SliderStyle (SliderLabelMode(..), SliderStyle(..), sliderStyleForId)
import Seer.Config.SliderUi (sliderDefsForConfigTab, sliderValueForId)
import UI.DrawCommand (DrawCommand, fillRect, textCentered, textLabelAbove)
import UI.Layout
  ( ConfigParamRowRects(..)
  , Layout
  , configParamRects
  , configRowTopPad
  , configScrollAreaRect
  )
import UI.Theme
  ( colLogErrorText
  , colSliderBtn
  , colSliderTrack
  , colTabActive
  , colTabInactive
  , textPrimary
  )
import UI.WidgetId (WidgetId)
import UI.Widgets (Rect(..))

-- | One tab header in the config panel.
data ConfigTabView = ConfigTabView
  { ctvTab :: !ConfigTab
  , ctvLabel :: !Text
  , ctvRect :: !Rect
  , ctvActive :: !Bool
  } deriving (Eq, Show)

-- | Pure view model for one registry-backed config slider row.
data ConfigSliderRowView = ConfigSliderRowView
  { csrSliderId :: !SliderId
  , csrRawValue :: !Float
  , csrValue :: !Float
  , csrStep :: !Float
  , csrShowButtonLabels :: !Bool
  , csrFillColor :: !(V4 Word8)
  , csrLabel :: !Text
  , csrTooltip :: !(Maybe Text)
  , csrHitRect :: !Rect
  , csrMinusRect :: !Rect
  , csrBarRect :: !Rect
  , csrPlusRect :: !Rect
  , csrMinusWidget :: !WidgetId
  , csrPlusWidget :: !WidgetId
  } deriving (Eq, Show)

-- | Validation copy shown for direct/loaded slider states outside [0,1].
data ConfigSliderValidation = ConfigSliderValidation
  { cslvSliderId :: !SliderId
  , cslvRect :: !Rect
  , cslvMessage :: !Text
  } deriving (Eq, Show)

configTabViews
  :: ConfigTab
  -> (Rect, Rect, Rect, Rect, Rect, Rect, Rect, Rect)
  -> [ConfigTabView]
configTabViews activeTab rects =
  [ ConfigTabView tab label rect (tab == activeTab)
  | ((tab, label), rect) <- zip tabMetadata (tabRectsToList rects)
  ]

configTabDrawCommands :: [ConfigTabView] -> [DrawCommand]
configTabDrawCommands = map $ \tabView ->
  fillRect (if ctvActive tabView then colTabActive else colTabInactive) (ctvRect tabView)

configTabLabelCommands :: [ConfigTabView] -> [DrawCommand]
configTabLabelCommands = map $ \tabView ->
  textCentered textPrimary (ctvRect tabView) (ctvLabel tabView)

configSliderRowsForActiveTab :: UiState -> Layout -> [ConfigSliderRowView]
configSliderRowsForActiveTab ui = configSliderRowsForTab (uiConfigTab ui) ui

configSliderRowsForTab :: ConfigTab -> UiState -> Layout -> [ConfigSliderRowView]
configSliderRowsForTab configTab ui layout =
  map (configSliderRowView ui layout) (sliderDefsForConfigTab configTab)

configSliderRowsScrolled :: UiState -> Layout -> [ConfigSliderRowView]
configSliderRowsScrolled ui layout =
  map (shiftConfigSliderRow (negate (configSliderScrollOffset ui layout)))
    (configSliderRowsForActiveTab ui layout)

configSliderValidations :: [ConfigSliderRowView] -> [ConfigSliderValidation]
configSliderValidations = mapMaybe validateRow
  where
    validateRow row
      | csrRawValue row < 0.0 || csrRawValue row > 1.0 =
          Just ConfigSliderValidation
            { cslvSliderId = csrSliderId row
            , cslvRect = csrBarRect row
            , cslvMessage =
                Text.pack (show (csrSliderId row))
                  <> " normalized value outside [0,1]: "
                  <> Text.pack (show (csrRawValue row))
            }
      | otherwise = Nothing

configSliderDrawCommands :: [ConfigSliderRowView] -> [DrawCommand]
configSliderDrawCommands = concatMap configSliderRowDrawCommands

configSliderLabelCommands :: [ConfigSliderRowView] -> [DrawCommand]
configSliderLabelCommands = concatMap configSliderRowLabelCommands

configSliderValidationCommands :: [ConfigSliderValidation] -> [DrawCommand]
configSliderValidationCommands = map $ \validation ->
  textLabelAbove colLogErrorText (cslvRect validation) (cslvMessage validation)

configSliderInputValue :: Float -> SliderPart -> Float -> Float
configSliderInputValue current sliderPart step =
  clamp01 (current + signedDelta)
  where
    signedDelta = case sliderPart of
      SliderPartMinus -> negate step
      SliderPartPlus -> step

configSliderInputValueForId :: UiState -> SliderId -> SliderPart -> Float
configSliderInputValueForId ui sid sliderPart =
  configSliderInputValue
    (sliderValueForId ui sid)
    sliderPart
    (sliderStyleStep (sliderStyleForId sid))

configSliderInputValueForRow :: ConfigSliderRowView -> SliderPart -> Float
configSliderInputValueForRow row sliderPart =
  configSliderInputValue (csrRawValue row) sliderPart (csrStep row)

configSliderRowView :: UiState -> Layout -> SliderDef -> ConfigSliderRowView
configSliderRowView ui layout sliderDef =
  let sid = sliderId sliderDef
      rawValue = sliderValueForId ui sid
      value = clamp01 rawValue
      sliderStyle = sliderStyleForId sid
      ConfigParamRowRects hitRect minusRect barRect plusRect =
        configParamRects (sliderRowIndex sliderDef) layout
  in ConfigSliderRowView
    { csrSliderId = sid
    , csrRawValue = rawValue
    , csrValue = value
    , csrStep = sliderStyleStep sliderStyle
    , csrShowButtonLabels = sliderStyleLabelMode sliderStyle == SliderLabelFull
    , csrFillColor = sliderStyleFillColor sliderStyle
    , csrLabel = sliderLabelForId sid value
    , csrTooltip = tooltipForWidget (sliderMinusWidgetId sliderDef)
    , csrHitRect = hitRect
    , csrMinusRect = minusRect
    , csrBarRect = barRect
    , csrPlusRect = plusRect
    , csrMinusWidget = sliderMinusWidgetId sliderDef
    , csrPlusWidget = sliderPlusWidgetId sliderDef
    }

configSliderRowDrawCommands :: ConfigSliderRowView -> [DrawCommand]
configSliderRowDrawCommands row =
  [ fillRect colSliderBtn (csrMinusRect row)
  , fillRect colSliderBtn (csrPlusRect row)
  , fillRect colSliderTrack (csrBarRect row)
  , fillRect (csrFillColor row) (sliderFillRect (csrValue row) (csrBarRect row))
  ]

configSliderRowLabelCommands :: ConfigSliderRowView -> [DrawCommand]
configSliderRowLabelCommands row =
  buttonLabels ++ [textLabelAbove textPrimary (csrBarRect row) (csrLabel row)]
  where
    buttonLabels =
      if csrShowButtonLabels row
        then
          [ textCentered textPrimary (csrMinusRect row) "-"
          , textCentered textPrimary (csrPlusRect row) "+"
          ]
        else []

shiftConfigSliderRow :: Int -> ConfigSliderRowView -> ConfigSliderRowView
shiftConfigSliderRow dy row = row
  { csrHitRect = shiftRectY dy (csrHitRect row)
  , csrMinusRect = shiftRectY dy (csrMinusRect row)
  , csrBarRect = shiftRectY dy (csrBarRect row)
  , csrPlusRect = shiftRectY dy (csrPlusRect row)
  }

shiftRectY :: Int -> Rect -> Rect
shiftRectY dy (Rect (V2 x y, V2 w h)) = Rect (V2 x (y + dy), V2 w h)

sliderFillRect :: Float -> Rect -> Rect
sliderFillRect value (Rect (V2 x y, V2 w h)) =
  Rect (V2 x y, V2 fillW h)
  where
    fillW = max 0 (min w (round (fromIntegral w * clamp01 value)))

configSliderScrollOffset :: UiState -> Layout -> Int
configSliderScrollOffset ui layout =
  min maxOffset (uiConfigScroll ui)
  where
    rowHeight = 24
    gap = 10
    rows = configRowCount (uiConfigTab ui) ui
    contentHeight = max rowHeight (configRowTopPad + rows * rowHeight + max 0 (rows - 1) * gap)
    Rect (V2 _ _, V2 _ scrollH) = configScrollAreaRect layout
    maxOffset = max 0 (contentHeight - scrollH)

clamp01 :: Float -> Float
clamp01 = max 0.0 . min 1.0

tabMetadata :: [(ConfigTab, Text)]
tabMetadata =
  [ (ConfigTerrain, "Terrain")
  , (ConfigPlanet, "Planet")
  , (ConfigClimate, "Climate")
  , (ConfigWeather, "Weather")
  , (ConfigBiome, "Biome")
  , (ConfigErosion, "Erosion")
  , (ConfigPipeline, "Pipeline")
  , (ConfigData, "Data")
  ]

tabRectsToList :: (Rect, Rect, Rect, Rect, Rect, Rect, Rect, Rect) -> [Rect]
tabRectsToList (tabTerrain, tabPlanet, tabClimate, tabWeather, tabBiome, tabErosion, tabPipeline, tabData) =
  [tabTerrain, tabPlanet, tabClimate, tabWeather, tabBiome, tabErosion, tabPipeline, tabData]
