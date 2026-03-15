{-# LANGUAGE OverloadedStrings #-}

-- | Slider specifications for the config UI.
--
-- Each 'SliderSpec' bundles a slider's display name and tooltip text.
-- Slider value formatting comes from the shared registry-backed conversion
-- metadata so labels, config writes, and snapshot restore all agree on
-- the same display domain.
--
-- Tooltip text is keyed by 'WidgetId' via 'tooltipForWidget', which
-- maps each minus/plus/bar widget to the same description.
module Seer.Config.SliderSpec
  ( SliderId(..)
  , SliderSpec
  , sliderLabelForId
  , sliderSpecForId
  , tooltipForWidget
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Numeric (showFFloat)
import Seer.Config.Range (mapRange)
import Seer.Config.SliderConversion
  ( SliderLabelDomain(..)
  , SliderValueKind(..)
  , sliderLabelDomain
  )
import Seer.Config.SliderRegistry (SliderId(..), sliderDefForWidget, sliderId)
import Seer.Config.SliderSpec.Data
  ( SliderSpec(ssName, ssTooltip)
  , fallbackSliderSpec
  , sliderSpecEntries
  )
import UI.WidgetId (WidgetId(..))

-- | Format a registry-backed slider label using the shared slider conversion
-- metadata for range and value kind.
sliderLabelForId :: SliderId -> Float -> Text
sliderLabelForId sliderIdValue t =
  let spec = sliderSpecForId sliderIdValue
      SliderLabelDomain lo hi valueKind = sliderLabelDomain sliderIdValue
      val = mapRange lo hi t
      fmt = sliderLabelFormatter valueKind
  in "(?) " <> ssName spec <> ": " <> fmt val <> "/" <> fmt hi

sliderLabelFormatter :: SliderValueKind -> Float -> Text
sliderLabelFormatter valueKind = case valueKind of
  SliderValueFloat -> fmtFloat
  SliderValueInt -> fmtInt
  SliderValueToggle -> fmtInt

-- | Format a float with 2 decimal places.
fmtFloat :: Float -> Text
fmtFloat x = Text.pack (showFFloat (Just 2) x "")

-- | Format a float as a rounded integer.
fmtInt :: Float -> Text
fmtInt x = Text.pack (show (round x :: Int))

-- | Look up tooltip text for a widget.
--
-- Returns 'Just' for every config slider widget ID and 'Nothing' for
-- non-slider widgets.
tooltipForWidget :: WidgetId -> Maybe Text
tooltipForWidget wid = ssTooltip . sliderSpecForId . sliderId <$> sliderDefForWidget wid

-- | Resolve a logical slider identifier to its display specification.
sliderSpecForId :: SliderId -> SliderSpec
sliderSpecForId sliderIdValue = maybe (fallbackSliderSpec sliderIdValue) id (lookup sliderIdValue sliderSpecEntries)
