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
tooltipForWidget WidgetViewBaseElevation      = Just "Base elevation view"
tooltipForWidget WidgetViewBaseBiome          = Just "Base biome classification view"
tooltipForWidget WidgetViewBaseMoisture       = Just "Base soil moisture view"
tooltipForWidget WidgetViewBaseVegetation     = Just "Base vegetation density view"
tooltipForWidget WidgetViewBaseTerrainForm    = Just "Base terrain form classification"
tooltipForWidget WidgetViewBasePlateId        = Just "Base tectonic plate ID view"
tooltipForWidget WidgetViewBasePlateBoundary  = Just "Base plate boundary view"
tooltipForWidget WidgetViewBasePlateHardness  = Just "Base plate rock hardness view"
tooltipForWidget WidgetViewBasePlateCrust     = Just "Base crustal thickness view"
tooltipForWidget WidgetViewBasePlateAge       = Just "Base plate age view"
tooltipForWidget WidgetViewBasePlateHeight    = Just "Base plate-driven height view"
tooltipForWidget WidgetViewBasePlateVelocity  = Just "Base plate velocity view"
tooltipForWidget WidgetViewOverlayNone        = Just "Disable weather, sky, and plugin overlay layer"
tooltipForWidget WidgetViewOverlayTemperature = Just "Overlay temperature on the selected base; basis chooses climate average or current weather"
tooltipForWidget WidgetViewOverlayPrecipitation = Just "Overlay precipitation on the selected base; basis chooses climate average or current weather"
tooltipForWidget WidgetViewOverlayCloud       = Just "Overlay cloud/storm tint on the selected base; average uses weather_normals, current uses simulated weather"
tooltipForWidget WidgetViewBasisAverage       = Just "Use generated average basis: climate averages for temperature/precipitation, weather_normals for clouds"
tooltipForWidget WidgetViewBasisCurrent       = Just "Use current simulated weather snapshot from the weather overlay"
tooltipForWidget WidgetDayNightToggle    = Just "Toggle day/night shading as a separate top overlay"
tooltipForWidget WidgetViewOverlayPrev   = Just "Cycle to the previous plugin overlay layer"
tooltipForWidget WidgetViewOverlayNext   = Just "Cycle to the next plugin overlay layer"
tooltipForWidget WidgetViewFieldPrev     = Just "Cycle to the previous field in the active plugin overlay"
tooltipForWidget WidgetViewFieldNext     = Just "Cycle to the next field in the active plugin overlay"
tooltipForWidget WidgetOverlayManager    = Just "Open the overlay list and active overlay details"
tooltipForWidget WidgetOverlaySchema     = Just "Inspect the schema for the active overlay"
tooltipForWidget WidgetOverlayProvenance = Just "Inspect persisted provenance for the active overlay"
tooltipForWidget WidgetOverlayExport     = Just "Export the active overlay schema, provenance, and payload"
tooltipForWidget WidgetOverlayImportValidate = Just "Validate an overlay import payload before adopting it"
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
