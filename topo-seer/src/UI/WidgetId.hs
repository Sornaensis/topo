{-# LANGUAGE OverloadedStrings #-}

module UI.WidgetId
  ( WidgetId(..)
  , widgetIdToText
  , widgetIdFromText
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text
import Seer.Config.SliderId (SliderId(..))

data WidgetId
  = WidgetGenerate
  | WidgetLeftToggle
  | WidgetLeftTabTopo
  | WidgetLeftTabView
  | WidgetSeedValue
  | WidgetSeedRandom
  | WidgetChunkMinus
  | WidgetChunkPlus
  | WidgetConfigToggle
  | WidgetConfigTabTerrain
  | WidgetConfigTabPlanet
  | WidgetConfigTabClimate
  | WidgetConfigTabWeather
  | WidgetConfigTabBiome
  | WidgetConfigTabErosion
  | WidgetConfigTabPipeline
  | WidgetConfigPresetSave
  | WidgetConfigPresetLoad
  | WidgetConfigReset
  | WidgetConfigRevert
  -- Slider minus/plus buttons, parameterized by SliderId
  | WidgetSliderMinus !SliderId
  | WidgetSliderPlus !SliderId
  | WidgetViewBaseElevation
  | WidgetViewBaseBiome
  | WidgetViewBaseMoisture
  | WidgetViewBaseVegetation
  | WidgetViewBaseTerrainForm
  | WidgetViewBasePlateId
  | WidgetViewBasePlateBoundary
  | WidgetViewBasePlateHardness
  | WidgetViewBasePlateCrust
  | WidgetViewBasePlateAge
  | WidgetViewBasePlateHeight
  | WidgetViewBasePlateVelocity
  | WidgetViewOverlayNone
  | WidgetViewOverlayTemperature
  | WidgetViewOverlayPrecipitation
  | WidgetViewOverlayCloud
  | WidgetViewBasisAverage
  | WidgetViewBasisCurrent
  -- Legacy single-mode IDs kept for click_widget/API compatibility.
  | WidgetViewElevation
  | WidgetViewBiome
  | WidgetViewClimate
  | WidgetViewWeather
  | WidgetViewMoisture
  | WidgetViewPrecip
  | WidgetViewPrecipCurrent
  | WidgetViewVegetation
  | WidgetViewTerrainForm
  | WidgetViewPlateId
  | WidgetViewPlateBoundary
  | WidgetViewPlateHardness
  | WidgetViewPlateCrust
  | WidgetViewPlateAge
  | WidgetViewPlateHeight
  | WidgetViewPlateVelocity
  | WidgetViewCloud
  | WidgetViewCloudTypical
  | WidgetDayNightToggle
  | WidgetViewOverlayPrev
  -- ^ Previous overlay in the overlay store.
  | WidgetViewOverlayNext
  -- ^ Next overlay in the overlay store.
  | WidgetViewFieldPrev
  -- ^ Previous field within the selected overlay.
  | WidgetViewFieldNext
  -- ^ Next field within the selected overlay.
  | WidgetOverlayManager
    -- ^ Open/inspect the overlay manager surface.
  | WidgetOverlaySchema
    -- ^ Inspect schema for the selected overlay.
  | WidgetOverlayProvenance
    -- ^ Inspect provenance for the selected overlay.
  | WidgetOverlayExport
    -- ^ Open overlay export action surface.
  | WidgetOverlayImportValidate
    -- ^ Open overlay import validation action surface.
  | WidgetLogDebug
  | WidgetLogInfo
  | WidgetLogWarn
  | WidgetLogError
  | WidgetLogHeader
  | WidgetMenuSave
  | WidgetMenuLoad
  | WidgetMenuExit
  | WidgetPresetSaveOk
  | WidgetPresetSaveCancel
  | WidgetPresetLoadOk
  | WidgetPresetLoadCancel
  | WidgetPresetLoadItem
  | WidgetWorldSaveOk
  | WidgetWorldSaveCancel
  | WidgetWorldLoadOk
  | WidgetWorldLoadCancel
  | WidgetWorldLoadItem
  | WidgetPipelineToggle !Text
  | WidgetPluginMoveUp !Text
  | WidgetPluginMoveDown !Text
  | WidgetPluginToggle !Text
  | WidgetPluginExpand !Text
  | WidgetPluginParamSlider !Text !Text
  | WidgetPluginParamCheck !Text !Text
  | WidgetSimTick
  | WidgetSimAutoTick
  -- Data browser (ConfigData tab)
  | WidgetConfigTabData
  | WidgetDataPluginSelect !Text
  | WidgetDataResourceSelect !Text !Text
  | WidgetDataPagePrev !Text !Text
  | WidgetDataPageNext !Text !Text
  | WidgetDataRecordSelect !Int
    -- ^ Click a record row by index to show the detail popover.
  | WidgetDataDetailDismiss
    -- ^ Dismiss the detail popover.
  | WidgetDataFieldToggle !Text
    -- ^ Toggle expand/collapse of a nested field path in the detail popover.
    -- Data browser mutation controls
  | WidgetDataEditToggle
    -- ^ Toggle edit mode in the detail popover.
  | WidgetDataEditSave
    -- ^ Save changes (MutUpdate or MutCreate).
  | WidgetDataEditCancel
    -- ^ Cancel editing / creating, revert to read-only.
  | WidgetDataCreateNew
    -- ^ Open the popover in create-new-record mode.
  | WidgetDataDeleteBtn
    -- ^ Show the delete confirmation modal.
  | WidgetDataDeleteConfirm
    -- ^ Confirm deletion.
  | WidgetDataDeleteCancel
    -- ^ Cancel deletion.
  | WidgetDataFieldTextClick !Text
    -- ^ Click a DFText field to focus it for keyboard input.
  | WidgetDataFieldStepMinus !Text
    -- ^ Minus button for a numeric field stepper.
  | WidgetDataFieldStepPlus !Text
    -- ^ Plus button for a numeric field stepper.
  | WidgetDataFieldBoolToggle !Text
    -- ^ Toggle a DFBool field.
  | WidgetDataFieldEnumPrev !Text
    -- ^ Cycle a DFEnum field to the previous option.
  | WidgetDataFieldEnumNext !Text
    -- ^ Cycle a DFEnum field to the next option.
    -- Editor toolbar
  | WidgetEditorTool !Int
    -- ^ Tool button (0-indexed corresponding to 'EditorTool' enum order).
  | WidgetEditorRadiusMinus
  | WidgetEditorRadiusPlus
  | WidgetEditorClose
  | WidgetEditorReopen
    -- ^ Button shown when the editor toolbar is closed, to reopen it.
    -- Editor param bar
  | WidgetEditorParamMinus !Int
    -- ^ Minus button for a numeric parameter in the param bar (0-based slot).
  | WidgetEditorParamPlus !Int
    -- ^ Plus button for a numeric parameter in the param bar (0-based slot).
  | WidgetEditorCyclePrev !Int
    -- ^ Prev arrow for a cycle selector in the param bar (0-based slot).
  | WidgetEditorCycleNext !Int
    -- ^ Next arrow for a cycle selector in the param bar (0-based slot).
  | WidgetEditorFalloffPrev
    -- ^ Prev arrow for the always-visible falloff cycle selector.
  | WidgetEditorFalloffNext
    -- ^ Next arrow for the always-visible falloff cycle selector.
  deriving (Eq, Show)

-- | Canonical public representation. Nullary and numeric IDs retain their
-- spellings; arbitrary text arguments use the versioned, length-framed @1 encoding.
widgetIdToText :: WidgetId -> Text
widgetIdToText wid = case wid of
  WidgetGenerate -> "WidgetGenerate"
  WidgetLeftToggle -> "WidgetLeftToggle"
  WidgetLeftTabTopo -> "WidgetLeftTabTopo"
  WidgetLeftTabView -> "WidgetLeftTabView"
  WidgetSeedValue -> "WidgetSeedValue"
  WidgetSeedRandom -> "WidgetSeedRandom"
  WidgetChunkMinus -> "WidgetChunkMinus"
  WidgetChunkPlus -> "WidgetChunkPlus"
  WidgetConfigToggle -> "WidgetConfigToggle"
  WidgetConfigTabTerrain -> "WidgetConfigTabTerrain"
  WidgetConfigTabPlanet -> "WidgetConfigTabPlanet"
  WidgetConfigTabClimate -> "WidgetConfigTabClimate"
  WidgetConfigTabWeather -> "WidgetConfigTabWeather"
  WidgetConfigTabBiome -> "WidgetConfigTabBiome"
  WidgetConfigTabErosion -> "WidgetConfigTabErosion"
  WidgetConfigTabPipeline -> "WidgetConfigTabPipeline"
  WidgetConfigPresetSave -> "WidgetConfigPresetSave"
  WidgetConfigPresetLoad -> "WidgetConfigPresetLoad"
  WidgetConfigReset -> "WidgetConfigReset"
  WidgetConfigRevert -> "WidgetConfigRevert"
  WidgetSliderMinus sid -> "WidgetSliderMinus:" <> Text.pack (show sid)
  WidgetSliderPlus sid -> "WidgetSliderPlus:" <> Text.pack (show sid)
  WidgetViewBaseElevation -> "WidgetViewBaseElevation"
  WidgetViewBaseBiome -> "WidgetViewBaseBiome"
  WidgetViewBaseMoisture -> "WidgetViewBaseMoisture"
  WidgetViewBaseVegetation -> "WidgetViewBaseVegetation"
  WidgetViewBaseTerrainForm -> "WidgetViewBaseTerrainForm"
  WidgetViewBasePlateId -> "WidgetViewBasePlateId"
  WidgetViewBasePlateBoundary -> "WidgetViewBasePlateBoundary"
  WidgetViewBasePlateHardness -> "WidgetViewBasePlateHardness"
  WidgetViewBasePlateCrust -> "WidgetViewBasePlateCrust"
  WidgetViewBasePlateAge -> "WidgetViewBasePlateAge"
  WidgetViewBasePlateHeight -> "WidgetViewBasePlateHeight"
  WidgetViewBasePlateVelocity -> "WidgetViewBasePlateVelocity"
  WidgetViewOverlayNone -> "WidgetViewOverlayNone"
  WidgetViewOverlayTemperature -> "WidgetViewOverlayTemperature"
  WidgetViewOverlayPrecipitation -> "WidgetViewOverlayPrecipitation"
  WidgetViewOverlayCloud -> "WidgetViewOverlayCloud"
  WidgetViewBasisAverage -> "WidgetViewBasisAverage"
  WidgetViewBasisCurrent -> "WidgetViewBasisCurrent"
  WidgetViewElevation -> "WidgetViewElevation"
  WidgetViewBiome -> "WidgetViewBiome"
  WidgetViewClimate -> "WidgetViewClimate"
  WidgetViewWeather -> "WidgetViewWeather"
  WidgetViewMoisture -> "WidgetViewMoisture"
  WidgetViewPrecip -> "WidgetViewPrecip"
  WidgetViewPrecipCurrent -> "WidgetViewPrecipCurrent"
  WidgetViewVegetation -> "WidgetViewVegetation"
  WidgetViewTerrainForm -> "WidgetViewTerrainForm"
  WidgetViewPlateId -> "WidgetViewPlateId"
  WidgetViewPlateBoundary -> "WidgetViewPlateBoundary"
  WidgetViewPlateHardness -> "WidgetViewPlateHardness"
  WidgetViewPlateCrust -> "WidgetViewPlateCrust"
  WidgetViewPlateAge -> "WidgetViewPlateAge"
  WidgetViewPlateHeight -> "WidgetViewPlateHeight"
  WidgetViewPlateVelocity -> "WidgetViewPlateVelocity"
  WidgetViewCloud -> "WidgetViewCloud"
  WidgetViewCloudTypical -> "WidgetViewCloudTypical"
  WidgetDayNightToggle -> "WidgetDayNightToggle"
  WidgetViewOverlayPrev -> "WidgetViewOverlayPrev"
  WidgetViewOverlayNext -> "WidgetViewOverlayNext"
  WidgetViewFieldPrev -> "WidgetViewFieldPrev"
  WidgetViewFieldNext -> "WidgetViewFieldNext"
  WidgetOverlayManager -> "WidgetOverlayManager"
  WidgetOverlaySchema -> "WidgetOverlaySchema"
  WidgetOverlayProvenance -> "WidgetOverlayProvenance"
  WidgetOverlayExport -> "WidgetOverlayExport"
  WidgetOverlayImportValidate -> "WidgetOverlayImportValidate"
  WidgetLogDebug -> "WidgetLogDebug"
  WidgetLogInfo -> "WidgetLogInfo"
  WidgetLogWarn -> "WidgetLogWarn"
  WidgetLogError -> "WidgetLogError"
  WidgetLogHeader -> "WidgetLogHeader"
  WidgetMenuSave -> "WidgetMenuSave"
  WidgetMenuLoad -> "WidgetMenuLoad"
  WidgetMenuExit -> "WidgetMenuExit"
  WidgetPresetSaveOk -> "WidgetPresetSaveOk"
  WidgetPresetSaveCancel -> "WidgetPresetSaveCancel"
  WidgetPresetLoadOk -> "WidgetPresetLoadOk"
  WidgetPresetLoadCancel -> "WidgetPresetLoadCancel"
  WidgetPresetLoadItem -> "WidgetPresetLoadItem"
  WidgetWorldSaveOk -> "WidgetWorldSaveOk"
  WidgetWorldSaveCancel -> "WidgetWorldSaveCancel"
  WidgetWorldLoadOk -> "WidgetWorldLoadOk"
  WidgetWorldLoadCancel -> "WidgetWorldLoadCancel"
  WidgetWorldLoadItem -> "WidgetWorldLoadItem"
  WidgetPipelineToggle name -> one "WidgetPipelineToggle" name
  WidgetPluginMoveUp name -> one "WidgetPluginMoveUp" name
  WidgetPluginMoveDown name -> one "WidgetPluginMoveDown" name
  WidgetPluginToggle name -> one "WidgetPluginToggle" name
  WidgetPluginExpand name -> one "WidgetPluginExpand" name
  WidgetPluginParamSlider pluginName paramName -> two "WidgetPluginParamSlider" pluginName paramName
  WidgetPluginParamCheck pluginName paramName -> two "WidgetPluginParamCheck" pluginName paramName
  WidgetSimTick -> "WidgetSimTick"
  WidgetSimAutoTick -> "WidgetSimAutoTick"
  WidgetConfigTabData -> "WidgetConfigTabData"
  WidgetDataPluginSelect name -> one "WidgetDataPluginSelect" name
  WidgetDataResourceSelect pluginName resourceName -> two "WidgetDataResourceSelect" pluginName resourceName
  WidgetDataPagePrev pluginName resourceName -> two "WidgetDataPagePrev" pluginName resourceName
  WidgetDataPageNext pluginName resourceName -> two "WidgetDataPageNext" pluginName resourceName
  WidgetDataRecordSelect index -> intArg "WidgetDataRecordSelect" index
  WidgetDataDetailDismiss -> "WidgetDataDetailDismiss"
  WidgetDataFieldToggle path -> one "WidgetDataFieldToggle" path
  WidgetDataEditToggle -> "WidgetDataEditToggle"
  WidgetDataEditSave -> "WidgetDataEditSave"
  WidgetDataEditCancel -> "WidgetDataEditCancel"
  WidgetDataCreateNew -> "WidgetDataCreateNew"
  WidgetDataDeleteBtn -> "WidgetDataDeleteBtn"
  WidgetDataDeleteConfirm -> "WidgetDataDeleteConfirm"
  WidgetDataDeleteCancel -> "WidgetDataDeleteCancel"
  WidgetDataFieldTextClick path -> one "WidgetDataFieldTextClick" path
  WidgetDataFieldStepMinus path -> one "WidgetDataFieldStepMinus" path
  WidgetDataFieldStepPlus path -> one "WidgetDataFieldStepPlus" path
  WidgetDataFieldBoolToggle path -> one "WidgetDataFieldBoolToggle" path
  WidgetDataFieldEnumPrev path -> one "WidgetDataFieldEnumPrev" path
  WidgetDataFieldEnumNext path -> one "WidgetDataFieldEnumNext" path
  WidgetEditorTool index -> intArg "WidgetEditorTool" index
  WidgetEditorRadiusMinus -> "WidgetEditorRadiusMinus"
  WidgetEditorRadiusPlus -> "WidgetEditorRadiusPlus"
  WidgetEditorClose -> "WidgetEditorClose"
  WidgetEditorReopen -> "WidgetEditorReopen"
  WidgetEditorParamMinus index -> intArg "WidgetEditorParamMinus" index
  WidgetEditorParamPlus index -> intArg "WidgetEditorParamPlus" index
  WidgetEditorCyclePrev index -> intArg "WidgetEditorCyclePrev" index
  WidgetEditorCycleNext index -> intArg "WidgetEditorCycleNext" index
  WidgetEditorFalloffPrev -> "WidgetEditorFalloffPrev"
  WidgetEditorFalloffNext -> "WidgetEditorFalloffNext"
  where
    one constructor argument = "@1:" <> constructor <> frame argument
    two constructor first second = "@1:" <> constructor <> frame first <> frame second
    intArg constructor value = constructor <> ":" <> Text.pack (show value)

frame :: Text -> Text
frame value = Text.pack (show (Text.length value)) <> "#" <> value

-- | Decode canonical IDs and the historical unescaped colon representation.
widgetIdFromText :: Text -> Maybe WidgetId
widgetIdFromText value
  | Just encoded <- Text.stripPrefix "@1:" value = canonicalTextWidgetId encoded
  | Just wid <- lookup value [(widgetIdToText wid, wid) | wid <- nullaryWidgetIds] = Just wid
  | Just rest <- prefix "WidgetSliderMinus" value
  , Just raw <- decodeOne rest
  , Just sid <- lookup raw sliderIds = Just (WidgetSliderMinus sid)
  | Just rest <- prefix "WidgetSliderPlus" value
  , Just raw <- decodeOne rest
  , Just sid <- lookup raw sliderIds = Just (WidgetSliderPlus sid)
  | Just rest <- prefix "WidgetPipelineToggle" value = WidgetPipelineToggle <$> decodeOne rest
  | Just rest <- prefix "WidgetPluginMoveUp" value = WidgetPluginMoveUp <$> decodeOne rest
  | Just rest <- prefix "WidgetPluginMoveDown" value = WidgetPluginMoveDown <$> decodeOne rest
  | Just rest <- prefix "WidgetPluginToggle" value = WidgetPluginToggle <$> decodeOne rest
  | Just rest <- prefix "WidgetPluginExpand" value = WidgetPluginExpand <$> decodeOne rest
  | Just rest <- prefix "WidgetPluginParamSlider" value = uncurry WidgetPluginParamSlider <$> decodeTwo rest
  | Just rest <- prefix "WidgetPluginParamCheck" value = uncurry WidgetPluginParamCheck <$> decodeTwo rest
  | Just rest <- prefix "WidgetDataPluginSelect" value = WidgetDataPluginSelect <$> decodeOne rest
  | Just rest <- prefix "WidgetDataResourceSelect" value = uncurry WidgetDataResourceSelect <$> decodeTwo rest
  | Just rest <- prefix "WidgetDataPagePrev" value = uncurry WidgetDataPagePrev <$> decodeTwo rest
  | Just rest <- prefix "WidgetDataPageNext" value = uncurry WidgetDataPageNext <$> decodeTwo rest
  | Just rest <- prefix "WidgetDataRecordSelect" value = WidgetDataRecordSelect <$> readInt rest
  | Just rest <- prefix "WidgetDataFieldToggle" value = WidgetDataFieldToggle <$> decodeOne rest
  | Just rest <- prefix "WidgetDataFieldTextClick" value = WidgetDataFieldTextClick <$> decodeOne rest
  | Just rest <- prefix "WidgetDataFieldStepMinus" value = WidgetDataFieldStepMinus <$> decodeOne rest
  | Just rest <- prefix "WidgetDataFieldStepPlus" value = WidgetDataFieldStepPlus <$> decodeOne rest
  | Just rest <- prefix "WidgetDataFieldBoolToggle" value = WidgetDataFieldBoolToggle <$> decodeOne rest
  | Just rest <- prefix "WidgetDataFieldEnumPrev" value = WidgetDataFieldEnumPrev <$> decodeOne rest
  | Just rest <- prefix "WidgetDataFieldEnumNext" value = WidgetDataFieldEnumNext <$> decodeOne rest
  | Just rest <- prefix "WidgetEditorTool" value = WidgetEditorTool <$> readInt rest
  | Just rest <- prefix "WidgetEditorParamMinus" value = WidgetEditorParamMinus <$> readInt rest
  | Just rest <- prefix "WidgetEditorParamPlus" value = WidgetEditorParamPlus <$> readInt rest
  | Just rest <- prefix "WidgetEditorCyclePrev" value = WidgetEditorCyclePrev <$> readInt rest
  | Just rest <- prefix "WidgetEditorCycleNext" value = WidgetEditorCycleNext <$> readInt rest
  | otherwise = Nothing
  where
    prefix constructor = Text.stripPrefix (constructor <> ":")
    sliderIds = [(Text.pack (show sid), sid) | sid <- [minBound .. maxBound]]

nullaryWidgetIds :: [WidgetId]
nullaryWidgetIds =
  [ WidgetGenerate, WidgetLeftToggle, WidgetLeftTabTopo, WidgetLeftTabView
  , WidgetSeedValue, WidgetSeedRandom, WidgetChunkMinus, WidgetChunkPlus
  , WidgetConfigToggle, WidgetConfigTabTerrain, WidgetConfigTabPlanet
  , WidgetConfigTabClimate, WidgetConfigTabWeather, WidgetConfigTabBiome
  , WidgetConfigTabErosion, WidgetConfigTabPipeline, WidgetConfigPresetSave
  , WidgetConfigPresetLoad, WidgetConfigReset, WidgetConfigRevert
  , WidgetViewBaseElevation, WidgetViewBaseBiome, WidgetViewBaseMoisture
  , WidgetViewBaseVegetation, WidgetViewBaseTerrainForm, WidgetViewBasePlateId
  , WidgetViewBasePlateBoundary, WidgetViewBasePlateHardness, WidgetViewBasePlateCrust
  , WidgetViewBasePlateAge, WidgetViewBasePlateHeight, WidgetViewBasePlateVelocity
  , WidgetViewOverlayNone, WidgetViewOverlayTemperature, WidgetViewOverlayPrecipitation
  , WidgetViewOverlayCloud, WidgetViewBasisAverage, WidgetViewBasisCurrent
  , WidgetViewElevation, WidgetViewBiome, WidgetViewClimate, WidgetViewWeather
  , WidgetViewMoisture, WidgetViewPrecip, WidgetViewPrecipCurrent, WidgetViewVegetation
  , WidgetViewTerrainForm, WidgetViewPlateId, WidgetViewPlateBoundary
  , WidgetViewPlateHardness, WidgetViewPlateCrust, WidgetViewPlateAge
  , WidgetViewPlateHeight, WidgetViewPlateVelocity, WidgetViewCloud
  , WidgetViewCloudTypical, WidgetDayNightToggle, WidgetViewOverlayPrev
  , WidgetViewOverlayNext, WidgetViewFieldPrev, WidgetViewFieldNext
  , WidgetOverlayManager, WidgetOverlaySchema, WidgetOverlayProvenance
  , WidgetOverlayExport, WidgetOverlayImportValidate, WidgetLogDebug
  , WidgetLogInfo, WidgetLogWarn, WidgetLogError, WidgetLogHeader
  , WidgetMenuSave, WidgetMenuLoad, WidgetMenuExit, WidgetPresetSaveOk
  , WidgetPresetSaveCancel, WidgetPresetLoadOk, WidgetPresetLoadCancel
  , WidgetPresetLoadItem, WidgetWorldSaveOk, WidgetWorldSaveCancel
  , WidgetWorldLoadOk, WidgetWorldLoadCancel, WidgetWorldLoadItem
  , WidgetSimTick, WidgetSimAutoTick, WidgetConfigTabData
  , WidgetDataDetailDismiss, WidgetDataEditToggle, WidgetDataEditSave
  , WidgetDataEditCancel, WidgetDataCreateNew, WidgetDataDeleteBtn
  , WidgetDataDeleteConfirm, WidgetDataDeleteCancel, WidgetEditorRadiusMinus
  , WidgetEditorRadiusPlus, WidgetEditorClose, WidgetEditorReopen
  , WidgetEditorFalloffPrev, WidgetEditorFalloffNext
  ]

-- Canonical framing is namespaced before the constructor so it cannot collide
-- with any historical arbitrary text argument.
canonicalTextWidgetId :: Text -> Maybe WidgetId
canonicalTextWidgetId encoded
  | Just rest <- canonicalPrefix "WidgetPipelineToggle" = WidgetPipelineToggle <$> framedOne rest
  | Just rest <- canonicalPrefix "WidgetPluginMoveUp" = WidgetPluginMoveUp <$> framedOne rest
  | Just rest <- canonicalPrefix "WidgetPluginMoveDown" = WidgetPluginMoveDown <$> framedOne rest
  | Just rest <- canonicalPrefix "WidgetPluginToggle" = WidgetPluginToggle <$> framedOne rest
  | Just rest <- canonicalPrefix "WidgetPluginExpand" = WidgetPluginExpand <$> framedOne rest
  | Just rest <- canonicalPrefix "WidgetPluginParamSlider" = uncurry WidgetPluginParamSlider <$> framedTwo rest
  | Just rest <- canonicalPrefix "WidgetPluginParamCheck" = uncurry WidgetPluginParamCheck <$> framedTwo rest
  | Just rest <- canonicalPrefix "WidgetDataPluginSelect" = WidgetDataPluginSelect <$> framedOne rest
  | Just rest <- canonicalPrefix "WidgetDataResourceSelect" = uncurry WidgetDataResourceSelect <$> framedTwo rest
  | Just rest <- canonicalPrefix "WidgetDataPagePrev" = uncurry WidgetDataPagePrev <$> framedTwo rest
  | Just rest <- canonicalPrefix "WidgetDataPageNext" = uncurry WidgetDataPageNext <$> framedTwo rest
  | Just rest <- canonicalPrefix "WidgetDataFieldToggle" = WidgetDataFieldToggle <$> framedOne rest
  | Just rest <- canonicalPrefix "WidgetDataFieldTextClick" = WidgetDataFieldTextClick <$> framedOne rest
  | Just rest <- canonicalPrefix "WidgetDataFieldStepMinus" = WidgetDataFieldStepMinus <$> framedOne rest
  | Just rest <- canonicalPrefix "WidgetDataFieldStepPlus" = WidgetDataFieldStepPlus <$> framedOne rest
  | Just rest <- canonicalPrefix "WidgetDataFieldBoolToggle" = WidgetDataFieldBoolToggle <$> framedOne rest
  | Just rest <- canonicalPrefix "WidgetDataFieldEnumPrev" = WidgetDataFieldEnumPrev <$> framedOne rest
  | Just rest <- canonicalPrefix "WidgetDataFieldEnumNext" = WidgetDataFieldEnumNext <$> framedOne rest
  | otherwise = Nothing
  where
    canonicalPrefix constructor = Text.stripPrefix constructor encoded

framedOne :: Text -> Maybe Text
framedOne raw = do
  (argument, remainder) <- unframe raw
  if Text.null remainder then Just argument else Nothing

framedTwo :: Text -> Maybe (Text, Text)
framedTwo raw = do
  (first, rest) <- unframe raw
  (second, remainder) <- unframe rest
  if Text.null remainder then Just (first, second) else Nothing

decodeOne :: Text -> Maybe Text
decodeOne = Just

decodeTwo :: Text -> Maybe (Text, Text)
decodeTwo raw = case Text.breakOn ":" raw of
  (first, rest)
    | Text.null rest -> Nothing
    | otherwise -> Just (first, Text.drop 1 rest)

unframe :: Text -> Maybe (Text, Text)
unframe encoded = do
  let (rawLength, withHash) = Text.breakOn "#" encoded
  lengthValue <- readInt rawLength
  if lengthValue < 0 || Text.null withHash
    then Nothing
    else
      let payload = Text.drop 1 withHash
          (argument, remainder) = Text.splitAt lengthValue payload
      in if Text.length argument == lengthValue
           then Just (argument, remainder)
           else Nothing

readInt :: Text -> Maybe Int
readInt raw = case Text.signed Text.decimal raw of
  Right (value, remainder) | Text.null remainder -> Just value
  _ -> Nothing