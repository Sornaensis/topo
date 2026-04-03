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
-- Returns 'Just' for config slider widgets, editor toolbar widgets,
-- and view mode buttons.  Returns 'Nothing' for all other widgets.
tooltipForWidget :: WidgetId -> Maybe Text
tooltipForWidget (WidgetEditorTool idx) = Just (editorToolTooltip idx)
tooltipForWidget WidgetEditorRadiusMinus = Just "Decrease brush radius"
tooltipForWidget WidgetEditorRadiusPlus  = Just "Increase brush radius"
tooltipForWidget WidgetEditorClose       = Just "Close editor toolbar (E)"
tooltipForWidget WidgetEditorReopen      = Just "Open editor toolbar (E)"
tooltipForWidget (WidgetEditorParamMinus _) = Just "Decrease parameter"
tooltipForWidget (WidgetEditorParamPlus  _) = Just "Increase parameter"
tooltipForWidget (WidgetEditorCyclePrev  _) = Just "Previous option"
tooltipForWidget (WidgetEditorCycleNext  _) = Just "Next option"
tooltipForWidget WidgetEditorFalloffPrev    = Just "Previous brush falloff"
tooltipForWidget WidgetEditorFalloffNext    = Just "Next brush falloff"
tooltipForWidget WidgetViewElevation     = Just "Elevation view"
tooltipForWidget WidgetViewBiome         = Just "Biome classification view"
tooltipForWidget WidgetViewClimate       = Just "Climate overview"
tooltipForWidget WidgetViewWeather       = Just "Weather patterns"
tooltipForWidget WidgetViewMoisture      = Just "Soil moisture"
tooltipForWidget WidgetViewPrecip        = Just "Precipitation"
tooltipForWidget WidgetViewVegetation    = Just "Vegetation density"
tooltipForWidget WidgetViewTerrainForm   = Just "Terrain form classification"
tooltipForWidget WidgetViewPlateId       = Just "Tectonic plate ID"
tooltipForWidget WidgetViewPlateBoundary = Just "Plate boundaries"
tooltipForWidget WidgetViewPlateHardness = Just "Plate rock hardness"
tooltipForWidget WidgetViewPlateCrust    = Just "Crustal thickness"
tooltipForWidget WidgetViewPlateAge      = Just "Plate age"
tooltipForWidget WidgetViewPlateHeight   = Just "Plate-driven height"
tooltipForWidget WidgetViewPlateVelocity = Just "Plate velocity"
tooltipForWidget wid = ssTooltip . sliderSpecForId . sliderId <$> sliderDefForWidget wid

-- | Tooltip text for each editor tool index.
editorToolTooltip :: Int -> Text
editorToolTooltip 0 = "Raise terrain"
editorToolTooltip 1 = "Lower terrain"
editorToolTooltip 2 = "Smooth terrain"
editorToolTooltip 3 = "Flatten to reference"
editorToolTooltip 4 = "Add noise"
editorToolTooltip 5 = "Paint biome"
editorToolTooltip 6 = "Paint terrain form"
editorToolTooltip 7 = "Set rock hardness"
editorToolTooltip 8 = "Erode terrain"
editorToolTooltip _ = "Editor tool"

-- | Resolve a logical slider identifier to its display specification.
sliderSpecForId :: SliderId -> SliderSpec
sliderSpecForId sliderIdValue = maybe (fallbackSliderSpec sliderIdValue) id (lookup sliderIdValue sliderSpecEntries)
