{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | UI actor state, update algebra, and snapshot protocol.
module Actor.UI.State
  ( Ui
  , ConfigTab(..)
  , configRowCount
  , DataBrowserState(..)
  , emptyDataBrowserState
  , dataBrowserReadPending
  , dataBrowserMutationPending
  , dataBrowserListError
  , dataBrowserMutationError
  , dataBrowserScopedError
  , dataBrowserRowCount
  , pluginRowIndex
  , pluginRowsWithParams
  , builtinStageRowCount
  , LeftTab(..)
  , sliderValueForId
  , UiMenuMode(..)
  , ViewMode(..)
  , BaseViewMode(..)
  , SkyOverlayMode(..)
  , WeatherBasis(..)
  , LayeredViewState(..)
  , ViewSelection
  , defaultOverlayOpacity
  , defaultLayeredViewState
  , allBaseViewModes
  , allBuiltinSkyOverlayModes
  , baseViewModeToText
  , baseViewModeFromText
  , baseViewModeLabel
  , baseViewModeToViewMode
  , baseViewModeFromViewMode
  , baseViewModeMetadataToJSON
  , baseViewModeSummaryToJSON
  , skyOverlayModeToText
  , skyOverlayModeFromText
  , skyOverlayModeLabel
  , skyOverlayModeToViewMode
  , skyOverlayModeFromViewMode
  , skyOverlayModeMetadataToJSON
  , skyOverlayModeSummaryToJSON
  , weatherBasisToText
  , weatherBasisFromText
  , weatherOverlayTemporalBasis
  , weatherOverlaySourceKind
  , layeredViewStateToJSON
  , layeredViewStateDataSemantics
  , effectiveViewSelection
  , uiViewMode
  , viewModeToLayeredViewState
  , legacyViewModeToLayeredViewState
  , layeredViewStateToViewMode
  , layeredViewStateIsLegacyEquivalent
  , ViewModeKind(..)
  , TemporalBasis(..)
  , SourceKind(..)
  , ViewModeDataSemantics(..)
  , ViewModeLegend(..)
  , ViewModeLegendStop(..)
  , ViewModeLegendCategory(..)
  , ViewModeMetadata(..)
  , allBuiltinViewModes
  , allViewModeExportFields
  , viewModeMetadata
  , viewModeMetadataToJSON
  , viewModeSummaryToJSON
  , viewModeKindToText
  , temporalBasisToText
  , temporalBasisFromText
  , sourceKindToText
  , viewModeDataSemantics
  , viewModeWithBasis
  , viewModeFromTextWithBasis
  , viewModeLegendTitle
  , viewModeToText
  , viewModeFromText
  , builtinViewModeFromText
  , viewModeLabel
  , PipelineStageRunState(..)
  , UiState(..)
  , emptyUiState
  , uiWorldTime
  , UiUpdate(..)
  , applyUpdate
  , UiSnapshotReply
  , uiActorDef
  , requestUiSnapshot
  , getUiSnapshot
  , UiSnapshotRef
  , setUiSnapshotRef
  , readUiSnapshotRef
  , newUiSnapshotRef
  , beginUiDataBrowserAction
  , completeUiDataBrowserRequest
  , beginUiOverlayInspectorAction
  , completeUiOverlayInspectorRequest
  ) where

import Actor.PluginManager.Types (PluginLifecycleSnapshot)
import Control.Monad (when)
import Data.Aeson (Value, object, (.=))
import Data.Aeson.Types (Pair)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (find, nub)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import Hyperspace.Actor hiding (stop)
import Hyperspace.Actor.QQ (hyperspace)
import Hyperspace.Actor.Spec (OpTag(..))
import Seer.Config.SliderRegistry (SliderId(..), SliderTab(..), sliderDefaultValueForId, sliderRowCountForTab)
import Seer.Config.Snapshot.Types (ConfigSnapshot)
import Seer.DataBrowser.Lifecycle (DataBrowserAppAction, beginDataBrowserAction)
import Seer.DataBrowser.Model
  ( DataBrowserAsyncError(..)
  , DataBrowserBeginResult(..)
  , DataBrowserCompletion
  , DataBrowserEditBuffer(..)
  , DataBrowserMode(..)
  , DataBrowserModel(..)
  , DataBrowserPagination(..)
  , DataBrowserPendingEnvelope(..)
  , DataBrowserPendingRequest(..)
  , DataBrowserRequestId(..)
  , DataBrowserSelection(..)
  , DataBrowserUi(..)
  , DataBrowserValidationError
  , DataBrowserWorkerRequest(..)
  , completeDataBrowserRequest
  , dataBrowserPageRequestFor
  , dataBrowserPendingDescriptor
  , dataBrowserRequestIsMutation
  )
import Seer.Editor.Types (EditorState(..), defaultEditorState)
import Seer.OverlayInspector.Model
  ( OverlayInspectorAction
  , OverlayInspectorBeginResult(..)
  , OverlayInspectorCompletion
  , OverlayInspectorModel
  , OverlayInspectorRequestId(..)
  , beginOverlayInspectorAction
  , completeOverlayInspectorRequest
  , emptyOverlayInspectorModel
  , openOverlayInspectorView
  , closeOverlayInspectorView
  , setOverlayInspectorFocus
  , setOverlayInspectorScroll
  , moveOverlayInspectorSelection
  , selectOverlayInspectorOverlay
  , setOverlayInspectorImportText
  , setOverlayInspectorImportDraft
  , applyOverlayInspectorValidationPreparation
  , setOverlayInspectorNotice
  , OverlayInspectorView
  , OverlayInspectorFocus
  )
import Seer.Render.ZoomStage (maxCameraZoom)
import Seer.World.Persist.Types (WorldSaveManifest)
import Topo.Calendar (WorldTime(..), simulationTickSeconds)
import Topo.Export (canonicalBasisQualifiedExportFields)
import Topo.Overlay.Schema (OverlayFieldType(..))
import Topo.Pipeline (StageStatus)
import Topo.Pipeline.Stage (StageId)
import Topo.Plugin.DataResource (DataOperations(..), DataResourceSchema(..))
import Topo.Plugin.RPC.DataService (DataRecord)
import Topo.Plugin.RPC.Manifest (RPCParamSpec(..), RPCParamType(..))
import UI.WidgetId (WidgetId)

-- | Which data layer to visualize on the hex map.
data ViewMode
  = ViewElevation
  | ViewBiome
  | ViewClimate
  | ViewWeather
  | ViewMoisture
  | ViewPrecip
  | ViewPlateId
  | ViewPlateBoundary
  | ViewPlateHardness
  | ViewPlateCrust
  | ViewPlateAge
  | ViewPlateHeight
  | ViewPlateVelocity
  | ViewVegetation
  | ViewTerrainForm
  | ViewCloud
  | ViewPrecipCurrent
  | ViewCloudTypical
  | ViewOverlay !Text !Int
  deriving (Eq, Ord, Show)

-- | Terrain or physical base selected independently of any sky/weather
-- overlay.  The constructors intentionally cover legacy non-weather
-- 'ViewMode' values so existing render paths can keep a one-dimensional
-- adapter during the layered migration.
data BaseViewMode
  = BaseViewElevation
  | BaseViewBiome
  | BaseViewMoisture
  | BaseViewPlateId
  | BaseViewPlateBoundary
  | BaseViewPlateHardness
  | BaseViewPlateCrust
  | BaseViewPlateAge
  | BaseViewPlateHeight
  | BaseViewPlateVelocity
  | BaseViewVegetation
  | BaseViewTerrainForm
  deriving (Eq, Ord, Show)

-- | Optional sky/weather overlay selected orthogonally from the base.
-- Plugin overlays remain represented here so dynamic overlay names have a
-- compatibility path back to legacy @overlay:<name>@ view-mode strings.
data SkyOverlayMode
  = SkyOverlayWeatherTemperature
  | SkyOverlayPrecipitation
  | SkyOverlayCloud
  | SkyOverlayPlugin !Text !Int
  deriving (Eq, Ord, Show)

-- | UI-facing weather basis for overlays.  Average cloud overlays map to the
-- existing typical-normal legacy mode while temperature/precipitation map to
-- long-run climate averages.
data WeatherBasis
  = WeatherBasisAverage
  | WeatherBasisCurrent
  deriving (Eq, Ord, Show)

-- | Canonical terrain and overlay selection. Legacy 'ViewMode' values are
-- converted only at explicit compatibility boundaries.
data LayeredViewState = LayeredViewState
  { lvsBaseView       :: !BaseViewMode
  , lvsSkyOverlay     :: !(Maybe SkyOverlayMode)
  , lvsWeatherBasis   :: !WeatherBasis
  , lvsOverlayOpacity :: !Float
  } deriving (Eq, Show)

type ViewSelection = LayeredViewState

defaultOverlayOpacity :: Float
defaultOverlayOpacity = 1.0

defaultLayeredViewState :: LayeredViewState
defaultLayeredViewState = LayeredViewState
  { lvsBaseView = BaseViewElevation
  , lvsSkyOverlay = Nothing
  , lvsWeatherBasis = WeatherBasisCurrent
  , lvsOverlayOpacity = defaultOverlayOpacity
  }

allBaseViewModes :: [BaseViewMode]
allBaseViewModes =
  [ BaseViewElevation
  , BaseViewBiome
  , BaseViewMoisture
  , BaseViewPlateId
  , BaseViewPlateBoundary
  , BaseViewPlateHardness
  , BaseViewPlateCrust
  , BaseViewPlateAge
  , BaseViewPlateHeight
  , BaseViewPlateVelocity
  , BaseViewVegetation
  , BaseViewTerrainForm
  ]

allBuiltinSkyOverlayModes :: [SkyOverlayMode]
allBuiltinSkyOverlayModes =
  [ SkyOverlayWeatherTemperature
  , SkyOverlayPrecipitation
  , SkyOverlayCloud
  ]

baseViewModeToText :: BaseViewMode -> Text
baseViewModeToText BaseViewElevation = "elevation"
baseViewModeToText BaseViewBiome = "biome"
baseViewModeToText BaseViewMoisture = "moisture"
baseViewModeToText BaseViewPlateId = "plate_id"
baseViewModeToText BaseViewPlateBoundary = "plate_boundary"
baseViewModeToText BaseViewPlateHardness = "plate_hardness"
baseViewModeToText BaseViewPlateCrust = "plate_crust"
baseViewModeToText BaseViewPlateAge = "plate_age"
baseViewModeToText BaseViewPlateHeight = "plate_height"
baseViewModeToText BaseViewPlateVelocity = "plate_velocity"
baseViewModeToText BaseViewVegetation = "vegetation"
baseViewModeToText BaseViewTerrainForm = "terrain_form"

baseViewModeFromText :: Text -> Maybe BaseViewMode
baseViewModeFromText raw = case Text.toLower raw of
  "elevation" -> Just BaseViewElevation
  "biome" -> Just BaseViewBiome
  "moisture" -> Just BaseViewMoisture
  "plate_id" -> Just BaseViewPlateId
  "plate-id" -> Just BaseViewPlateId
  "plate_boundary" -> Just BaseViewPlateBoundary
  "plate-boundary" -> Just BaseViewPlateBoundary
  "plate_hardness" -> Just BaseViewPlateHardness
  "plate-hardness" -> Just BaseViewPlateHardness
  "plate_crust" -> Just BaseViewPlateCrust
  "plate-crust" -> Just BaseViewPlateCrust
  "plate_age" -> Just BaseViewPlateAge
  "plate-age" -> Just BaseViewPlateAge
  "plate_height" -> Just BaseViewPlateHeight
  "plate-height" -> Just BaseViewPlateHeight
  "plate_velocity" -> Just BaseViewPlateVelocity
  "plate-velocity" -> Just BaseViewPlateVelocity
  "vegetation" -> Just BaseViewVegetation
  "terrain_form" -> Just BaseViewTerrainForm
  "terrain-form" -> Just BaseViewTerrainForm
  _ -> Nothing

baseViewModeToViewMode :: BaseViewMode -> ViewMode
baseViewModeToViewMode BaseViewElevation = ViewElevation
baseViewModeToViewMode BaseViewBiome = ViewBiome
baseViewModeToViewMode BaseViewMoisture = ViewMoisture
baseViewModeToViewMode BaseViewPlateId = ViewPlateId
baseViewModeToViewMode BaseViewPlateBoundary = ViewPlateBoundary
baseViewModeToViewMode BaseViewPlateHardness = ViewPlateHardness
baseViewModeToViewMode BaseViewPlateCrust = ViewPlateCrust
baseViewModeToViewMode BaseViewPlateAge = ViewPlateAge
baseViewModeToViewMode BaseViewPlateHeight = ViewPlateHeight
baseViewModeToViewMode BaseViewPlateVelocity = ViewPlateVelocity
baseViewModeToViewMode BaseViewVegetation = ViewVegetation
baseViewModeToViewMode BaseViewTerrainForm = ViewTerrainForm

baseViewModeFromViewMode :: ViewMode -> Maybe BaseViewMode
baseViewModeFromViewMode ViewElevation = Just BaseViewElevation
baseViewModeFromViewMode ViewBiome = Just BaseViewBiome
baseViewModeFromViewMode ViewMoisture = Just BaseViewMoisture
baseViewModeFromViewMode ViewPlateId = Just BaseViewPlateId
baseViewModeFromViewMode ViewPlateBoundary = Just BaseViewPlateBoundary
baseViewModeFromViewMode ViewPlateHardness = Just BaseViewPlateHardness
baseViewModeFromViewMode ViewPlateCrust = Just BaseViewPlateCrust
baseViewModeFromViewMode ViewPlateAge = Just BaseViewPlateAge
baseViewModeFromViewMode ViewPlateHeight = Just BaseViewPlateHeight
baseViewModeFromViewMode ViewPlateVelocity = Just BaseViewPlateVelocity
baseViewModeFromViewMode ViewVegetation = Just BaseViewVegetation
baseViewModeFromViewMode ViewTerrainForm = Just BaseViewTerrainForm
baseViewModeFromViewMode _ = Nothing

baseViewModeLabel :: BaseViewMode -> Text
baseViewModeLabel = viewModeLabel . baseViewModeToViewMode

baseViewModeMetadataToJSON :: BaseViewMode -> Value
baseViewModeMetadataToJSON = object . baseViewModeMetadataFields

baseViewModeSummaryToJSON :: Bool -> BaseViewMode -> Value
baseViewModeSummaryToJSON active mode = object (("active" .= active) : baseViewModeMetadataFields mode)

baseViewModeMetadataFields :: BaseViewMode -> [Pair]
baseViewModeMetadataFields mode =
  let legacyMode = baseViewModeToViewMode mode
      legacyField = "legacy_view_mode" .= viewModeToText legacyMode
  in case viewModeMetadata legacyMode of
       Just meta -> legacyField : viewModeMetadataFields meta
       Nothing ->
         [ "name" .= baseViewModeToText mode
         , "label" .= baseViewModeLabel mode
         , legacyField
         ]

skyOverlayModeToText :: SkyOverlayMode -> Text
skyOverlayModeToText SkyOverlayWeatherTemperature = "weather"
skyOverlayModeToText SkyOverlayPrecipitation = "precipitation"
skyOverlayModeToText SkyOverlayCloud = "cloud"
skyOverlayModeToText (SkyOverlayPlugin name _idx) = "overlay:" <> name

skyOverlayModeFromText :: Text -> Maybe SkyOverlayMode
skyOverlayModeFromText raw = case Text.toLower raw of
  "weather" -> Just SkyOverlayWeatherTemperature
  "climate" -> Just SkyOverlayWeatherTemperature
  "temperature" -> Just SkyOverlayWeatherTemperature
  "weather_temperature" -> Just SkyOverlayWeatherTemperature
  "weather-temperature" -> Just SkyOverlayWeatherTemperature
  "precip" -> Just SkyOverlayPrecipitation
  "precipitation" -> Just SkyOverlayPrecipitation
  "current_precipitation" -> Just SkyOverlayPrecipitation
  "precipitation_current" -> Just SkyOverlayPrecipitation
  "cloud" -> Just SkyOverlayCloud
  "cloud_typical" -> Just SkyOverlayCloud
  "typical_cloud" -> Just SkyOverlayCloud
  _
    | Just rest <- Text.stripPrefix "overlay:" raw
    , not (Text.null rest) -> Just (SkyOverlayPlugin rest 0)
    | otherwise -> Nothing

skyOverlayModeLabel :: SkyOverlayMode -> Text
skyOverlayModeLabel SkyOverlayWeatherTemperature = "Weather Temperature"
skyOverlayModeLabel SkyOverlayPrecipitation = "Precipitation"
skyOverlayModeLabel SkyOverlayCloud = "Cloud / Storm"
skyOverlayModeLabel (SkyOverlayPlugin name _idx) = "Overlay: " <> name

skyOverlayModeToViewMode :: WeatherBasis -> SkyOverlayMode -> Maybe ViewMode
skyOverlayModeToViewMode basis overlay = case (overlay, basis) of
  (SkyOverlayWeatherTemperature, WeatherBasisAverage) -> Just ViewClimate
  (SkyOverlayWeatherTemperature, WeatherBasisCurrent) -> Just ViewWeather
  (SkyOverlayPrecipitation, WeatherBasisAverage) -> Just ViewPrecip
  (SkyOverlayPrecipitation, WeatherBasisCurrent) -> Just ViewPrecipCurrent
  (SkyOverlayCloud, WeatherBasisAverage) -> Just ViewCloudTypical
  (SkyOverlayCloud, WeatherBasisCurrent) -> Just ViewCloud
  (SkyOverlayPlugin name idx, _) -> Just (ViewOverlay name idx)

skyOverlayModeFromViewMode :: ViewMode -> Maybe (SkyOverlayMode, WeatherBasis)
skyOverlayModeFromViewMode ViewClimate = Just (SkyOverlayWeatherTemperature, WeatherBasisAverage)
skyOverlayModeFromViewMode ViewWeather = Just (SkyOverlayWeatherTemperature, WeatherBasisCurrent)
skyOverlayModeFromViewMode ViewPrecip = Just (SkyOverlayPrecipitation, WeatherBasisAverage)
skyOverlayModeFromViewMode ViewPrecipCurrent = Just (SkyOverlayPrecipitation, WeatherBasisCurrent)
skyOverlayModeFromViewMode ViewCloud = Just (SkyOverlayCloud, WeatherBasisCurrent)
skyOverlayModeFromViewMode ViewCloudTypical = Just (SkyOverlayCloud, WeatherBasisAverage)
skyOverlayModeFromViewMode (ViewOverlay name idx) = Just (SkyOverlayPlugin name idx, WeatherBasisCurrent)
skyOverlayModeFromViewMode _ = Nothing

skyOverlayModeMetadataToJSON :: SkyOverlayMode -> Value
skyOverlayModeMetadataToJSON = object . skyOverlayModeMetadataFields WeatherBasisCurrent

skyOverlayModeSummaryToJSON :: Bool -> WeatherBasis -> SkyOverlayMode -> Value
skyOverlayModeSummaryToJSON active basis overlay = object (("active" .= active) : skyOverlayModeMetadataFields basis overlay)

skyOverlayModeMetadataFields :: WeatherBasis -> SkyOverlayMode -> [Pair]
skyOverlayModeMetadataFields basis overlay =
  [ "name" .= skyOverlayModeToText overlay
  , "label" .= skyOverlayModeLabel overlay
  , "weather_basis_supported" .= skyOverlayBasisLabels overlay
  , "legacy_view_mode" .= fmap viewModeToText (skyOverlayModeToViewMode basis overlay)
  , "temporal_basis" .= temporalBasisToText (weatherOverlayTemporalBasis overlay basis)
  , "source_kind" .= fmap sourceKindToText (weatherOverlaySourceKind overlay basis)
  ]

skyOverlayBasisLabels :: SkyOverlayMode -> [Text]
skyOverlayBasisLabels (SkyOverlayPlugin _ _) = []
skyOverlayBasisLabels _ = map weatherBasisToText [WeatherBasisAverage, WeatherBasisCurrent]

weatherBasisToText :: WeatherBasis -> Text
weatherBasisToText WeatherBasisAverage = "average"
weatherBasisToText WeatherBasisCurrent = "current"

weatherBasisFromText :: Text -> Maybe WeatherBasis
weatherBasisFromText raw = case temporalBasisFromText raw of
  Just InstantaneousCurrent -> Just WeatherBasisCurrent
  Just LongRunAverage -> Just WeatherBasisAverage
  Just TypicalNormal -> Just WeatherBasisAverage
  Nothing -> case Text.toLower raw of
    "average" -> Just WeatherBasisAverage
    "avg" -> Just WeatherBasisAverage
    "current" -> Just WeatherBasisCurrent
    _ -> Nothing

weatherOverlayTemporalBasis :: SkyOverlayMode -> WeatherBasis -> TemporalBasis
weatherOverlayTemporalBasis SkyOverlayCloud WeatherBasisAverage = TypicalNormal
weatherOverlayTemporalBasis _ WeatherBasisAverage = LongRunAverage
weatherOverlayTemporalBasis _ WeatherBasisCurrent = InstantaneousCurrent

weatherOverlaySourceKind :: SkyOverlayMode -> WeatherBasis -> Maybe SourceKind
weatherOverlaySourceKind (SkyOverlayPlugin _ _) _ = Nothing
weatherOverlaySourceKind SkyOverlayCloud WeatherBasisAverage = Just WeatherNormals
weatherOverlaySourceKind _ WeatherBasisAverage = Just GeneratedClimate
weatherOverlaySourceKind _ WeatherBasisCurrent = Just SimulatedWeather

legacyViewModeToLayeredViewState :: ViewMode -> LayeredViewState
legacyViewModeToLayeredViewState mode =
  case baseViewModeFromViewMode mode of
    Just base -> defaultLayeredViewState { lvsBaseView = base }
    Nothing -> case skyOverlayModeFromViewMode mode of
      Just (overlay, basis) -> defaultLayeredViewState
        { lvsSkyOverlay = Just overlay
        , lvsWeatherBasis = basis
        }
      Nothing -> defaultLayeredViewState

viewModeToLayeredViewState :: ViewMode -> LayeredViewState
viewModeToLayeredViewState = legacyViewModeToLayeredViewState

layeredViewStateToViewMode :: LayeredViewState -> Maybe ViewMode
layeredViewStateToViewMode selection =
  case lvsSkyOverlay selection of
    Nothing -> Just (baseViewModeToViewMode (lvsBaseView selection))
    Just overlay -> skyOverlayModeToViewMode (lvsWeatherBasis selection) overlay

-- | Whether the atlas-producing parts of a layered selection match a legacy
-- view mode. Opacity is draw-time state and therefore does not affect this.
layeredViewStateIsLegacyEquivalent :: LayeredViewState -> Bool
layeredViewStateIsLegacyEquivalent selection =
  case layeredViewStateToViewMode selection of
    Just legacyMode ->
      let legacy = legacyViewModeToLayeredViewState legacyMode
      in lvsBaseView selection == lvsBaseView legacy
        && lvsSkyOverlay selection == lvsSkyOverlay legacy
        && lvsWeatherBasis selection == lvsWeatherBasis legacy
    Nothing -> False

layeredViewStateToJSON :: LayeredViewState -> Value
layeredViewStateToJSON selection = object
  [ "base" .= baseViewModeToText (lvsBaseView selection)
  , "base_mode" .= baseViewModeToText (lvsBaseView selection)
  , "base_label" .= baseViewModeLabel (lvsBaseView selection)
  , "overlay" .= fmap skyOverlayModeToText (lvsSkyOverlay selection)
  , "overlay_mode" .= overlayModeName (lvsSkyOverlay selection)
  , "plugin_overlay" .= pluginOverlayName (lvsSkyOverlay selection)
  , "overlay_field" .= pluginOverlayField (lvsSkyOverlay selection)
  , "overlay_label" .= fmap skyOverlayModeLabel (lvsSkyOverlay selection)
  , "weather_basis" .= weatherBasisToText (lvsWeatherBasis selection)
  , "temporal_basis" .= fmap (temporalBasisToText . vmdsTemporalBasis) semantics
  , "source_kind" .= fmap (sourceKindToText . vmdsSourceKind) semantics
  , "overlay_opacity" .= lvsOverlayOpacity selection
  , "legacy_view_mode" .= fmap viewModeToText (layeredViewStateToViewMode selection)
  ]
  where
    overlayModeName Nothing = Nothing
    overlayModeName (Just (SkyOverlayPlugin _ _)) = Just ("plugin" :: Text)
    overlayModeName (Just overlayMode) = Just (skyOverlayModeToText overlayMode)
    pluginOverlayName (Just (SkyOverlayPlugin name _)) = Just name
    pluginOverlayName _ = Nothing
    pluginOverlayField (Just (SkyOverlayPlugin _ fieldIndex)) = Just fieldIndex
    pluginOverlayField _ = Nothing
    semantics = layeredViewStateDataSemantics selection

layeredViewStateDataSemantics :: LayeredViewState -> Maybe ViewModeDataSemantics
layeredViewStateDataSemantics selection =
  layeredViewStateToViewMode selection >>= viewModeDataSemantics

-- | Scalar modes use a continuous color scale; categorical modes map stable
-- domain codes/names to discrete legend entries.
data ViewModeKind
  = ViewModeScalar
  | ViewModeCategorical
  deriving (Eq, Show)

-- | Time basis for data shown by climate/weather-oriented views.
data TemporalBasis
  = LongRunAverage
  | TypicalNormal
  | InstantaneousCurrent
  deriving (Eq, Ord, Show)

-- | Origin of the data shown by climate/weather-oriented views.
data SourceKind
  = GeneratedClimate
  | SimulatedWeather
  | WeatherNormals
  | ExternalLive
  deriving (Eq, Ord, Show)

data ViewModeDataSemantics = ViewModeDataSemantics
  { vmdsTemporalBasis :: !TemporalBasis
  , vmdsSourceKind    :: !SourceKind
  } deriving (Eq, Show)

data ViewModeLegendStop = ViewModeLegendStop
  { vmlsValue :: !Text
  , vmlsLabel :: !Text
  , vmlsColor :: !Text
  } deriving (Eq, Show)

data ViewModeLegendCategory = ViewModeLegendCategory
  { vmlcValue :: !Text
  , vmlcLabel :: !Text
  , vmlcColor :: !Text
  } deriving (Eq, Show)

data ViewModeLegend
  = ViewModeGradientLegend !Text ![ViewModeLegendStop]
  | ViewModeCategoricalLegend !Text ![ViewModeLegendCategory]
  deriving (Eq, Show)

data ViewModeMetadata = ViewModeMetadata
  { vmmMode            :: !ViewMode
  , vmmName            :: !Text
  , vmmLabel           :: !Text
  , vmmDescription     :: !Text
  , vmmKind            :: !ViewModeKind
  , vmmTemporalBasis   :: !(Maybe TemporalBasis)
  , vmmSourceKind      :: !(Maybe SourceKind)
  , vmmUnitLabel       :: !(Maybe Text)
  , vmmColorScale      :: !Text
  , vmmLegend          :: !ViewModeLegend
  , vmmTooltipFields   :: ![Text]
  , vmmInspectorFields :: ![Text]
  , vmmExportFields    :: ![Text]
  , vmmHttpMetadata    :: ![Text]
  } deriving (Eq, Show)

allBuiltinViewModes :: [ViewMode]
allBuiltinViewModes =
  [ ViewElevation
  , ViewBiome
  , ViewClimate
  , ViewWeather
  , ViewCloud
  , ViewMoisture
  , ViewPrecip
  , ViewPrecipCurrent
  , ViewCloudTypical
  , ViewPlateId
  , ViewPlateBoundary
  , ViewPlateHardness
  , ViewPlateCrust
  , ViewPlateAge
  , ViewPlateHeight
  , ViewPlateVelocity
  , ViewVegetation
  , ViewTerrainForm
  ]

allViewModeExportFields :: [Text]
allViewModeExportFields = nub (concatMap vmmExportFields viewModeRegistry ++ canonicalBasisQualifiedExportFields)

viewModeMetadata :: ViewMode -> Maybe ViewModeMetadata
viewModeMetadata (ViewOverlay _ _) = Nothing
viewModeMetadata mode = find ((== mode) . vmmMode) viewModeRegistry

viewModeToText :: ViewMode -> Text
viewModeToText ViewElevation     = "elevation"
viewModeToText ViewBiome         = "biome"
viewModeToText ViewClimate       = "climate"
viewModeToText ViewWeather       = "weather"
viewModeToText ViewMoisture      = "moisture"
viewModeToText ViewPrecip        = "precipitation"
viewModeToText ViewPlateId       = "plate_id"
viewModeToText ViewPlateBoundary = "plate_boundary"
viewModeToText ViewPlateHardness = "plate_hardness"
viewModeToText ViewPlateCrust    = "plate_crust"
viewModeToText ViewPlateAge      = "plate_age"
viewModeToText ViewPlateHeight   = "plate_height"
viewModeToText ViewPlateVelocity = "plate_velocity"
viewModeToText ViewVegetation    = "vegetation"
viewModeToText ViewTerrainForm   = "terrain_form"
viewModeToText ViewCloud         = "cloud"
viewModeToText ViewPrecipCurrent = "precipitation_current"
viewModeToText ViewCloudTypical  = "cloud_typical"
viewModeToText (ViewOverlay name _idx) = "overlay:" <> name

builtinViewModeFromText :: Text -> Maybe ViewMode
builtinViewModeFromText "elevation"      = Just ViewElevation
builtinViewModeFromText "biome"          = Just ViewBiome
builtinViewModeFromText "climate"        = Just ViewClimate
builtinViewModeFromText "weather"        = Just ViewWeather
builtinViewModeFromText "moisture"       = Just ViewMoisture
builtinViewModeFromText "precipitation"  = Just ViewPrecip
builtinViewModeFromText "plate_id"       = Just ViewPlateId
builtinViewModeFromText "plate_boundary" = Just ViewPlateBoundary
builtinViewModeFromText "plate_hardness" = Just ViewPlateHardness
builtinViewModeFromText "plate_crust"    = Just ViewPlateCrust
builtinViewModeFromText "plate_age"      = Just ViewPlateAge
builtinViewModeFromText "plate_height"   = Just ViewPlateHeight
builtinViewModeFromText "plate_velocity" = Just ViewPlateVelocity
builtinViewModeFromText "vegetation"            = Just ViewVegetation
builtinViewModeFromText "terrain_form"          = Just ViewTerrainForm
builtinViewModeFromText "cloud"                 = Just ViewCloud
builtinViewModeFromText "precipitation_current" = Just ViewPrecipCurrent
builtinViewModeFromText "current_precipitation" = Just ViewPrecipCurrent
builtinViewModeFromText "cloud_typical"         = Just ViewCloudTypical
builtinViewModeFromText "typical_cloud"         = Just ViewCloudTypical
builtinViewModeFromText _                       = Nothing

viewModeFromText :: Text -> Maybe Int -> Maybe ViewMode
viewModeFromText name = viewModeFromTextWithBasis name Nothing

viewModeFromTextWithBasis :: Text -> Maybe TemporalBasis -> Maybe Int -> Maybe ViewMode
viewModeFromTextWithBasis name mBasis mIdx = do
  mode <- case builtinViewModeFromText name of
    Just mode -> Just mode
    Nothing
      | Just rest <- Text.stripPrefix "overlay:" name
      , not (Text.null rest) -> Just (ViewOverlay rest (maybe 0 id mIdx))
      | otherwise -> Nothing
  case mBasis of
    Nothing -> Just mode
    Just basis -> viewModeWithBasis mode basis

viewModeWithBasis :: ViewMode -> TemporalBasis -> Maybe ViewMode
viewModeWithBasis mode basis = case (weatherViewFamily mode, basis) of
  (Just WeatherTemperatureFamily, InstantaneousCurrent) -> Just ViewWeather
  (Just WeatherTemperatureFamily, LongRunAverage)       -> Just ViewClimate
  (Just WeatherTemperatureFamily, TypicalNormal)        -> Nothing
  (Just WeatherPrecipFamily, InstantaneousCurrent)      -> Just ViewPrecipCurrent
  (Just WeatherPrecipFamily, LongRunAverage)            -> Just ViewPrecip
  (Just WeatherPrecipFamily, TypicalNormal)             -> Nothing
  (Just WeatherCloudFamily, InstantaneousCurrent)       -> Just ViewCloud
  (Just WeatherCloudFamily, LongRunAverage)             -> Just ViewCloudTypical
  (Just WeatherCloudFamily, TypicalNormal)              -> Just ViewCloudTypical
  (Nothing, _)                                          -> Nothing

data WeatherViewFamily
  = WeatherTemperatureFamily
  | WeatherPrecipFamily
  | WeatherCloudFamily
  deriving (Eq, Show)

weatherViewFamily :: ViewMode -> Maybe WeatherViewFamily
weatherViewFamily ViewClimate = Just WeatherTemperatureFamily
weatherViewFamily ViewWeather = Just WeatherTemperatureFamily
weatherViewFamily ViewPrecip = Just WeatherPrecipFamily
weatherViewFamily ViewPrecipCurrent = Just WeatherPrecipFamily
weatherViewFamily ViewCloud = Just WeatherCloudFamily
weatherViewFamily ViewCloudTypical = Just WeatherCloudFamily
weatherViewFamily _ = Nothing

viewModeLabel :: ViewMode -> Text
viewModeLabel mode = case viewModeMetadata mode of
  Just meta -> vmmLabel meta
  Nothing -> case mode of
    ViewOverlay name _ -> "Overlay: " <> name
    _ -> viewModeToText mode

viewModeKindToText :: ViewModeKind -> Text
viewModeKindToText ViewModeScalar = "scalar"
viewModeKindToText ViewModeCategorical = "categorical"

temporalBasisToText :: TemporalBasis -> Text
temporalBasisToText LongRunAverage = "long_run_average"
temporalBasisToText TypicalNormal = "typical_normal"
temporalBasisToText InstantaneousCurrent = "instantaneous_current"

temporalBasisFromText :: Text -> Maybe TemporalBasis
temporalBasisFromText raw = case Text.toLower raw of
  "average" -> Just LongRunAverage
  "avg" -> Just LongRunAverage
  "long_run_average" -> Just LongRunAverage
  "long-run-average" -> Just LongRunAverage
  "typical" -> Just TypicalNormal
  "normal" -> Just TypicalNormal
  "typical_normal" -> Just TypicalNormal
  "typical-normal" -> Just TypicalNormal
  "current" -> Just InstantaneousCurrent
  "instantaneous" -> Just InstantaneousCurrent
  "instantaneous_current" -> Just InstantaneousCurrent
  "instantaneous-current" -> Just InstantaneousCurrent
  _ -> Nothing

sourceKindToText :: SourceKind -> Text
sourceKindToText GeneratedClimate = "climate_average"
sourceKindToText SimulatedWeather = "weather_snapshot"
sourceKindToText WeatherNormals = "weather_normals"
sourceKindToText ExternalLive = "external_live"

viewModeDataSemantics :: ViewMode -> Maybe ViewModeDataSemantics
viewModeDataSemantics ViewClimate = climateSemantics
viewModeDataSemantics ViewPrecip = climateSemantics
viewModeDataSemantics ViewPrecipCurrent = weatherSemantics
viewModeDataSemantics ViewWeather = weatherSemantics
viewModeDataSemantics ViewCloud = weatherSemantics
viewModeDataSemantics ViewCloudTypical = normalsSemantics
viewModeDataSemantics (ViewOverlay "weather" _) = weatherSemantics
viewModeDataSemantics (ViewOverlay "weather_normals" _) = normalsSemantics
viewModeDataSemantics _ = Nothing

climateSemantics :: Maybe ViewModeDataSemantics
climateSemantics = Just (ViewModeDataSemantics LongRunAverage GeneratedClimate)

weatherSemantics :: Maybe ViewModeDataSemantics
weatherSemantics = Just (ViewModeDataSemantics InstantaneousCurrent SimulatedWeather)

normalsSemantics :: Maybe ViewModeDataSemantics
normalsSemantics = Just (ViewModeDataSemantics TypicalNormal WeatherNormals)

viewModeLegendTitle :: ViewModeLegend -> Text
viewModeLegendTitle (ViewModeGradientLegend title _) = title
viewModeLegendTitle (ViewModeCategoricalLegend title _) = title

viewModeMetadataToJSON :: ViewModeMetadata -> Value
viewModeMetadataToJSON = object . viewModeMetadataFields

viewModeSummaryToJSON :: Bool -> ViewModeMetadata -> Value
viewModeSummaryToJSON active meta = object (("active" .= active) : viewModeMetadataFields meta)

viewModeMetadataFields :: ViewModeMetadata -> [Pair]
viewModeMetadataFields meta =
  [ "name" .= vmmName meta
  , "label" .= vmmLabel meta
  , "description" .= vmmDescription meta
  , "kind" .= viewModeKindToText (vmmKind meta)
  , "temporal_basis" .= fmap temporalBasisToText (vmmTemporalBasis meta)
  , "source_kind" .= fmap sourceKindToText (vmmSourceKind meta)
  , "unit" .= vmmUnitLabel meta
  , "color_scale" .= vmmColorScale meta
  , "legend" .= viewModeLegendToJSON (vmmLegend meta)
  , "tooltip_fields" .= vmmTooltipFields meta
  , "inspector_fields" .= vmmInspectorFields meta
  , "export_fields" .= vmmExportFields meta
  , "http" .= vmmHttpMetadata meta
  ]

viewModeLegendToJSON :: ViewModeLegend -> Value
viewModeLegendToJSON (ViewModeGradientLegend title stops) = object
  [ "type" .= ("gradient" :: Text)
  , "title" .= title
  , "stops" .= map legendStopToJSON stops
  ]
viewModeLegendToJSON (ViewModeCategoricalLegend title categories) = object
  [ "type" .= ("categorical" :: Text)
  , "title" .= title
  , "categories" .= map legendCategoryToJSON categories
  ]

legendStopToJSON :: ViewModeLegendStop -> Value
legendStopToJSON stop = object
  [ "value" .= vmlsValue stop
  , "label" .= vmlsLabel stop
  , "color" .= vmlsColor stop
  ]

legendCategoryToJSON :: ViewModeLegendCategory -> Value
legendCategoryToJSON category = object
  [ "value" .= vmlcValue category
  , "label" .= vmlcLabel category
  , "color" .= vmlcColor category
  ]

viewModeRegistry :: [ViewModeMetadata]
viewModeRegistry =
  [ scalar ViewElevation "elevation" "Elevation"
      "Hypsometric elevation relative to the configured water level."
      (Just "m") "elevation-blue-green"
      (gradient "Elevation"
        [ stop "0.00" "deep water" "#17468c"
        , stop "water" "water level" "#2b7ab9"
        , stop "0.50" "lowland" "#70a060"
        , stop "1.00" "highland" "#66ffcc"
        ])
      ["elevation_m", "terrain_form", "slope_avg_deg"]
      ["hypsometry.elevation_m", "terrain.terrain_form", "terrain_form_metrics.slope_avg_deg"]
      ["elevation"]
  , categorical ViewBiome "biome" "Biome"
      "Biome classification including water, alpine, desert, forest, grassland, tundra, and wetland families."
      "biome-palette"
      (categories "Biome families"
        [ category "water" "Water" "#1e50a0"
        , category "desert" "Desert" "#d2be78"
        , category "grassland" "Grassland" "#78aa5a"
        , category "forest" "Forest" "#32783c"
        , category "tundra" "Tundra" "#828c96"
        , category "rainforest" "Rainforest" "#286e50"
        , category "snow_ice" "Snow / ice" "#e6ebf0"
        ])
      ["biome", "terrain_form", "elevation_m"]
      ["terrain.biome", "terrain.biome_code", "biome_refinement.family"]
      ["biome", "biome_code"]
  , scalar ViewClimate "climate" "Average Climate Temp"
      "Long-run average generated climate temperature with precipitation context."
      (Just "degC") "heat"
      (gradient "Average temperature"
        [ stop "0.00" "cold" "#802f33"
        , stop "0.50" "temperate" "#b35c5c"
        , stop "1.00" "hot" "#ff9966"
        ])
      ["temp_avg_c", "precip_avg_mm_year"]
      ["climate_diagnostics.temp_avg_c", "climate_diagnostics.precip_avg_mm_year"]
      ["climate_temp_avg"]
  , scalar ViewWeather "weather" "Current Weather Temp"
      "Current simulated weather temperature with humidity, wind, pressure, and precipitation context; use Current Cloud/Storm for aggregate cloud cover and storm tint."
      (Just "degC") "weather-heat"
      (gradient "Current weather temperature"
        [ stop "0.00" "cold" "#802f33"
        , stop "0.50" "mild" "#b35c5c"
        , stop "1.00" "hot" "#ff9966"
        ])
      ["temp_c", "humidity_pct", "wind_spd_ms"]
      ["weather.temp", "weather.humidity", "weather.wind_spd", "weather.pressure", "weather.precip"]
      [ "weather_temp_current", "weather_humidity_current", "weather_wind_spd_current", "weather_pressure_current", "weather_precip_current"
      , "weather_temp_typical", "weather_humidity_typical", "weather_wind_dir_typical", "weather_wind_spd_typical", "weather_precip_typical"
      ]
  , scalar ViewCloud "cloud" "Current Cloud/Storm"
      "Current simulated aggregate cloud cover and cloud-water density with precipitation-derived storm tint; low/mid/high layer fields are inspector/API context, not separate rendered layers."
      (Just "% cover") "cloud-storm"
      (gradient "Aggregate cloud cover"
        [ stop "0.00" "clear" "#242428"
        , stop "0.50" "cloudy" "#a0a0a0"
        , stop "1.00" "storm" "#4c408c"
        ])
      ["cloud_cover_pct", "cloud_water", "storm_intensity"]
      [ "weather.cloud_cover", "weather.cloud_water", "weather.precip"
      , "weather.cloud_cover_low", "weather.cloud_cover_mid", "weather.cloud_cover_high"
      , "weather.cloud_water_low", "weather.cloud_water_mid", "weather.cloud_water_high"
      ]
      [ "weather_cloud_cover_current", "weather_cloud_water_current"
      , "weather_cloud_cover_low_current", "weather_cloud_cover_mid_current", "weather_cloud_cover_high_current"
      , "weather_cloud_water_low_current", "weather_cloud_water_mid_current", "weather_cloud_water_high_current"
      , "weather_cloud_cover_typical", "weather_cloud_water_typical"
      , "weather_cloud_cover_low_typical", "weather_cloud_cover_mid_typical", "weather_cloud_cover_high_typical"
      , "weather_cloud_water_low_typical", "weather_cloud_water_mid_typical", "weather_cloud_water_high_typical"
      ]
  , scalar ViewMoisture "moisture" "Moisture"
      "Terrain soil moisture, shown as relative humidity percent in hover and inspector values."
      (Just "%") "moisture"
      (gradient "Soil moisture"
        [ stop "0.00" "dry" "#1a4d66"
        , stop "0.50" "moist" "#337fb3"
        , stop "1.00" "saturated" "#4dccff"
        ])
      ["moisture_pct", "soil_depth_m"]
      ["terrain.moisture", "soil.soil_depth_m", "soil.soil_moisture"]
      ["moisture"]
  , scalar ViewPrecip "precipitation" "Average Precipitation"
      "Long-run average generated climate precipitation with humidity context."
      (Just "mm/yr") "precipitation-moisture"
      (gradient "Average precipitation"
        [ stop "0.00" "arid" "#1a4d66"
        , stop "0.50" "seasonal" "#337fb3"
        , stop "1.00" "wet" "#4dccff"
        ])
      ["precip_avg_mm_year", "humidity_pct"]
      ["climate_diagnostics.precip_avg_mm_year", "climate_diagnostics.humidity_avg_pct"]
      ["climate_precip_avg"]
  , scalar ViewPrecipCurrent "precipitation_current" "Current Precipitation"
      "Current simulated weather precipitation from the instantaneous WeatherChunk."
      (Just "mm/yr") "precipitation-moisture"
      (gradient "Current simulated precipitation"
        [ stop "0.00" "dry" "#1a4d66"
        , stop "0.50" "showers" "#337fb3"
        , stop "1.00" "storm" "#4dccff"
        ])
      ["precip_mm_year", "humidity_pct"]
      ["weather.precip", "weather.humidity"]
      ["weather_precip_current", "weather_humidity_current"]
  , scalar ViewCloudTypical "cloud_typical" "Typical Cloud Normal"
      "Typical generated cloud normal from the weather_normals layer; reports unavailable when generated normals are missing."
      (Just "% cover") "cloud-storm"
      (gradient "Typical generated cloud normal"
        [ stop "0.00" "clear" "#242428"
        , stop "0.50" "cloudy" "#a0a0a0"
        , stop "1.00" "storm" "#4c408c"
        ])
      ["normal_cloud_cover_pct", "normal_cloud_water", "normal_storm_intensity"]
      [ "weather_normals.cloud_cover", "weather_normals.cloud_water", "weather_normals.precip"
      , "weather_normals.cloud_cover_low", "weather_normals.cloud_cover_mid", "weather_normals.cloud_cover_high"
      , "weather_normals.cloud_water_low", "weather_normals.cloud_water_mid", "weather_normals.cloud_water_high"
      ]
      [ "weather_cloud_cover_typical", "weather_cloud_water_typical"
      , "weather_cloud_cover_low_typical", "weather_cloud_cover_mid_typical", "weather_cloud_cover_high_typical"
      , "weather_cloud_water_low_typical", "weather_cloud_water_mid_typical", "weather_cloud_water_high_typical"
      ]
  , categorical ViewPlateId "plate_id" "Plate ID"
      "Discrete tectonic plate identifier palette."
      "categorical-id-palette"
      (categories "Plate IDs"
        [ category "0" "Plate 0" "#5ac85a"
        , category "1" "Plate 1" "#5aa0c8"
        , category "2" "Plate 2" "#c8c85a"
        , category "n" "Additional IDs" "#8c5ac8"
        ])
      ["plate_id"]
      ["terrain.plate_id"]
      ["plate_id"]
  , categorical ViewPlateBoundary "plate_boundary" "Plate Boundary"
      "Discrete plate boundary classification."
      "plate-boundary"
      (categories "Boundary type"
        [ category "0" "None" "#28282d"
        , category "1" "Convergent" "#d25046"
        , category "2" "Divergent" "#3ca0d2"
        , category "3" "Transform" "#c8c85a"
        ])
      ["plate_boundary"]
      ["terrain.plate_boundary", "terrain.plate_boundary_code"]
      ["plate_boundary", "plate_boundary_code"]
  , scalar ViewPlateHardness "plate_hardness" "Plate Hardness"
      "Normalized tectonic plate hardness."
      (Just "normalized") "heat"
      (gradient "Plate hardness"
        [ stop "0.00" "soft" "#802f33"
        , stop "0.50" "mixed" "#b35c5c"
        , stop "1.00" "hard" "#ff9966"
        ])
      ["plate_hardness"]
      ["terrain.plate_hardness"]
      ["plate_hardness"]
  , categorical ViewPlateCrust "plate_crust" "Plate Crust"
      "Discrete oceanic or continental crust classification."
      "plate-crust"
      (categories "Crust type"
        [ category "0" "Oceanic" "#285aa0"
        , category "1" "Continental" "#a08c6e"
        ])
      ["plate_crust"]
      ["terrain.plate_crust"]
      ["plate_crust", "plate_crust_code"]
  , scalar ViewPlateAge "plate_age" "Plate Age"
      "Normalized tectonic plate age."
      (Just "normalized") "heat"
      (gradient "Plate age"
        [ stop "0.00" "young" "#802f33"
        , stop "0.50" "mature" "#b35c5c"
        , stop "1.00" "old" "#ff9966"
        ])
      ["plate_age"]
      ["terrain.plate_age"]
      ["plate_age"]
  , scalar ViewPlateHeight "plate_height" "Plate Height"
      "Plate height contribution converted to metres for display."
      (Just "m") "blue-green"
      (gradient "Plate height"
        [ stop "0.00" "low" "#1a4d99"
        , stop "0.50" "mid" "#4c99aa"
        , stop "1.00" "high" "#66ffcc"
        ])
      ["plate_height_m"]
      ["terrain.plate_height"]
      ["plate_height"]
  , scalar ViewPlateVelocity "plate_velocity" "Plate Velocity"
      "Normalized tectonic plate velocity magnitude with X/Y components."
      (Just "normalized speed") "heat"
      (gradient "Plate velocity"
        [ stop "0.00" "still" "#802f33"
        , stop "0.50" "drifting" "#b35c5c"
        , stop "1.00" "fast" "#ff9966"
        ])
      ["plate_velocity", "plate_velocity_x", "plate_velocity_y"]
      ["terrain.plate_velocity_x", "terrain.plate_velocity_y"]
      ["plate_velocity", "plate_velocity_x", "plate_velocity_y"]
  , scalar ViewVegetation "vegetation" "Vegetation"
      "Vegetation cover with density, fertility, and moisture context."
      (Just "cover") "vegetation"
      (gradient "Vegetation cover"
        [ stop "0.00" "barren" "#59331a"
        , stop "0.50" "sparse" "#4c8a32"
        , stop "1.00" "dense" "#19d91f"
        ])
      ["vegetation_cover", "vegetation_density", "fertility"]
      ["vegetation.cover", "vegetation.density", "soil.fertility"]
      ["vegetation_cover", "vegetation_density", "vegetation_albedo"]
  , categorical ViewTerrainForm "terrain_form" "Terrain Form"
      "Discrete terrain-form classification derived from slope, relief, and curvature."
      "terrain-form"
      (categories "Terrain form"
        [ category "0" "Flat" "#b4c3a0"
        , category "1" "Rolling" "#a5b478"
        , category "2" "Hilly" "#8c9b50"
        , category "3" "Mountainous" "#826e5a"
        , category "4" "Cliff" "#503c37"
        , category "7" "Ridge" "#be7846"
        , category "12" "Canyon" "#6e465a"
        ])
      ["terrain_form", "slope_avg_deg", "elevation_m"]
      ["terrain_form_metrics.terrain_form", "terrain_form_metrics.slope_avg_deg", "hypsometry.elevation_m"]
      ["terrain_form", "terrain_form_code"]
  ]

scalar :: ViewMode -> Text -> Text -> Text -> Maybe Text -> Text -> ViewModeLegend -> [Text] -> [Text] -> [Text] -> ViewModeMetadata
scalar mode name label description unitLabel colorScale legend tooltipFields inspectorFields exportFields =
  let semantics = viewModeDataSemantics mode
  in ViewModeMetadata
       mode
       name
       label
       description
       ViewModeScalar
       (fmap vmdsTemporalBasis semantics)
       (fmap vmdsSourceKind semantics)
       unitLabel
       colorScale
       legend
       tooltipFields
       inspectorFields
       exportFields
       standardViewModeHttp

categorical :: ViewMode -> Text -> Text -> Text -> Text -> ViewModeLegend -> [Text] -> [Text] -> [Text] -> ViewModeMetadata
categorical mode name label description colorScale legend tooltipFields inspectorFields exportFields =
  let semantics = viewModeDataSemantics mode
  in ViewModeMetadata
       mode
       name
       label
       description
       ViewModeCategorical
       (fmap vmdsTemporalBasis semantics)
       (fmap vmdsSourceKind semantics)
       Nothing
       colorScale
       legend
       tooltipFields
       inspectorFields
       exportFields
       standardViewModeHttp

gradient :: Text -> [ViewModeLegendStop] -> ViewModeLegend
gradient = ViewModeGradientLegend

categories :: Text -> [ViewModeLegendCategory] -> ViewModeLegend
categories = ViewModeCategoricalLegend

stop :: Text -> Text -> Text -> ViewModeLegendStop
stop = ViewModeLegendStop

category :: Text -> Text -> Text -> ViewModeLegendCategory
category = ViewModeLegendCategory

standardViewModeHttp :: [Text]
standardViewModeHttp =
  [ "GET /state/view-modes"
  , "GET /ui/state"
  , "GET /terrain/hex"
  , "POST /ui/view-mode"
  , "POST /terrain/export"
  ]

data ConfigTab
  = ConfigTerrain
  | ConfigPlanet
  | ConfigClimate
  | ConfigWeather
  | ConfigBiome
  | ConfigErosion
  | ConfigPipeline
  | ConfigData
  deriving (Eq, Show)

-- | State for the data browser panel (ConfigData tab).
data DataBrowserState = DataBrowserState
  { dbsSelectedPlugin   :: !(Maybe Text)
  , dbsSelectedResource :: !(Maybe Text)
  , dbsRecords          :: ![DataRecord]
  , dbsPageOffset       :: !Int
  , dbsTotalCount       :: !(Maybe Int)
  , dbsLoading          :: !Bool
  , dbsSelectedRecord   :: !(Maybe DataRecord)
  -- ^ Record shown in the detail popover.
  , dbsSelectedRecordKey :: !(Maybe Value)
  -- ^ Primary key of the selected record (for future update/delete).
  , dbsSelectedRowIndex :: !(Maybe Int)
  -- ^ Row index of the selected record (for popover anchoring).
  , dbsExpandedFields   :: !(Set Text)
  -- ^ Dot-separated field paths currently expanded in the detail popover.
  , dbsEditMode         :: !Bool
  -- ^ Whether the detail popover is in edit mode.
  , dbsCreateMode       :: !Bool
  -- ^ Whether the popover is in create-new-record mode.
  , dbsEditValues       :: !(Map Text Value)
  -- ^ Working copy of field values being edited (dot-path -> Value).
  , dbsFocusedField     :: !(Maybe Text)
  -- ^ Dot-path of the currently focused text field (captures keyboard).
  , dbsTextCursor       :: !Int
  -- ^ Cursor position within the focused text field.
  , dbsDeleteConfirm    :: !Bool
  -- ^ Whether the delete confirmation modal is shown.
  , dbsValidationErrors :: ![DataBrowserValidationError]
  -- ^ Validation or reducer guard errors shown in the detail popover.
  , dbsPendingRequest :: !(Maybe DataBrowserPendingEnvelope)
  -- ^ ID-tagged accepted request and exact target owned by the Ui actor.
  , dbsAsyncError :: !(Maybe DataBrowserAsyncError)
  -- ^ Error scoped to the most recently completed asynchronous operation.
  } deriving (Eq, Show)

-- | Empty initial state for the data browser.
emptyDataBrowserState :: DataBrowserState
emptyDataBrowserState = DataBrowserState
  { dbsSelectedPlugin   = Nothing
  , dbsSelectedResource = Nothing
  , dbsRecords          = []
  , dbsPageOffset       = 0
  , dbsTotalCount       = Nothing
  , dbsLoading          = False
  , dbsSelectedRecord   = Nothing
  , dbsSelectedRecordKey = Nothing
  , dbsSelectedRowIndex = Nothing
  , dbsExpandedFields   = Set.empty
  , dbsEditMode         = False
  , dbsCreateMode       = False
  , dbsEditValues       = Map.empty
  , dbsFocusedField     = Nothing
  , dbsTextCursor       = 0
  , dbsDeleteConfirm    = False
  , dbsValidationErrors = []
  , dbsPendingRequest = Nothing
  , dbsAsyncError = Nothing
  }

-- | A replaceable catalog/resource/list request is active. Only plugin and
-- resource navigation may replace this work.
dataBrowserReadPending :: DataBrowserState -> Bool
dataBrowserReadPending dbs = case dbsPendingRequest dbs of
  Just pending -> not (dataBrowserRequestIsMutation (dbpeRequest pending))
  Nothing -> False

-- | A create/update/delete request is active. Every Data Browser interaction
-- is locked until the matching completion arrives.
dataBrowserMutationPending :: DataBrowserState -> Bool
dataBrowserMutationPending dbs = case dbsPendingRequest dbs of
  Just pending -> dataBrowserRequestIsMutation (dbpeRequest pending)
  Nothing -> False

-- | A read failure is visible only while its target still matches the current
-- browser selection. Catalog failures are global to the list panel.
dataBrowserListError :: DataBrowserState -> Maybe DataBrowserAsyncError
dataBrowserListError dbs = do
  asyncError <- dbsAsyncError dbs
  if readErrorMatchesState dbs asyncError then Just asyncError else Nothing

-- | A mutation failure belongs to the retained create/edit/delete mode and
-- must not reappear after its target or mode changes.
dataBrowserMutationError :: DataBrowserState -> Maybe DataBrowserAsyncError
dataBrowserMutationError dbs = do
  asyncError <- dbsAsyncError dbs
  if mutationErrorMatchesState dbs asyncError then Just asyncError else Nothing

-- | The one operation error currently scoped to visible Data Browser state.
dataBrowserScopedError :: DataBrowserState -> Maybe DataBrowserAsyncError
dataBrowserScopedError dbs = case dataBrowserMutationError dbs of
  Just asyncError -> Just asyncError
  Nothing -> dataBrowserListError dbs

readErrorMatchesState :: DataBrowserState -> DataBrowserAsyncError -> Bool
readErrorMatchesState dbs asyncError = case dbaeRequest asyncError of
  DataBrowserLoadCatalogRequest -> True
  DataBrowserLoadPluginRequest pluginName ->
    dbsSelectedPlugin dbs == Just pluginName
  DataBrowserSelectResourceRequest pluginName resourceName ->
    sameResource dbs pluginName resourceName
  DataBrowserRecordRequest
      (DataBrowserListRecordsRequest pluginName resourceName _ _) ->
    sameResource dbs pluginName resourceName
  DataBrowserRecordRequest _ -> False

mutationErrorMatchesState :: DataBrowserState -> DataBrowserAsyncError -> Bool
mutationErrorMatchesState dbs asyncError = case dbaeRequest asyncError of
  DataBrowserRecordRequest descriptor -> case descriptor of
    DataBrowserCreateRecordRequest pluginName resourceName _ ->
      sameResource dbs pluginName resourceName && dbsCreateMode dbs
    DataBrowserUpdateRecordRequest pluginName resourceName key _ ->
      sameResource dbs pluginName resourceName
        && dbsSelectedRecordKey dbs == Just key
        && dbsEditMode dbs
    DataBrowserDeleteRecordRequest pluginName resourceName key ->
      sameResource dbs pluginName resourceName
        && dbsSelectedRecordKey dbs == Just key
        && dbsDeleteConfirm dbs
    DataBrowserListRecordsRequest {} -> False
  _ -> False

sameResource :: DataBrowserState -> Text -> Text -> Bool
sameResource dbs pluginName resourceName =
  dbsSelectedPlugin dbs == Just pluginName
    && dbsSelectedResource dbs == Just resourceName

-- | Number of rows to display in the data browser tab.
dataBrowserRowCount :: DataBrowserState -> Map Text [DataResourceSchema] -> Int
dataBrowserRowCount dbs resources =
  let pluginCount = Map.size resources
      resourceCount = case dbsSelectedPlugin dbs of
        Nothing   -> 0
        Just pName -> length (Map.findWithDefault [] pName resources)
      recordCount
        | dataBrowserReadPending dbs = 0
        | otherwise = length (dbsRecords dbs)
      selectedSchema = do
        pluginName <- dbsSelectedPlugin dbs
        resourceName <- dbsSelectedResource dbs
        schemas <- Map.lookup pluginName resources
        find ((== resourceName) . drsName) schemas
      unlocked = not (dataBrowserReadPending dbs || dataBrowserMutationPending dbs)
      pageCount
        | unlocked
        , recordCount > 0
        , maybe False (doPage . drsOperations) selectedSchema = 1
        | otherwise = 0
      createCount
        | unlocked
        , maybe False (doCreate . drsOperations) selectedSchema = 1
        | otherwise = 0
      statusCount
        | dataBrowserReadPending dbs = 1
        | Just _ <- dataBrowserListError dbs = 1
        | otherwise = 0
  in max 1 (pluginCount + resourceCount + recordCount + statusCount + pageCount + createCount)

-- | Total number of config widget rows for each tab.
configRowCount :: ConfigTab -> UiState -> Int
configRowCount ConfigTerrain _ = sliderRowCountForTab SliderTabTerrain
configRowCount ConfigPlanet _ = sliderRowCountForTab SliderTabPlanet
configRowCount ConfigClimate _ = sliderRowCountForTab SliderTabClimate
configRowCount ConfigWeather _ = sliderRowCountForTab SliderTabWeather
configRowCount ConfigBiome _ = sliderRowCountForTab SliderTabBiome
configRowCount ConfigErosion _ = sliderRowCountForTab SliderTabErosion
configRowCount ConfigPipeline ui =
  builtinStageRowCount + pluginRowsWithParams ui + simControlRowCount
configRowCount ConfigData ui =
  dataBrowserRowCount (uiDataBrowser ui) (uiDataResources ui)

builtinStageRowCount :: Int
builtinStageRowCount = 18

simControlRowCount :: Int
simControlRowCount = 3

-- | Total row count for all plugins in the pipeline tab,
-- accounting for expanded parameter sub-rows.
pluginRowsWithParams :: UiState -> Int
pluginRowsWithParams ui =
  sum [ 1 + expandedDetailCount name | name <- uiPluginNames ui ]
  where
    expandedDetailCount name
      | Map.findWithDefault False name (uiPluginExpanded ui) =
          length (Map.findWithDefault [] name (uiPluginDiagnosticLines ui))
          + length (Map.findWithDefault [] name (uiPluginParamSpecs ui))
      | otherwise = 0

-- | Compute the absolute row index for the i-th plugin in the pipeline tab,
-- accounting for expanded parameter rows of preceding plugins.
pluginRowIndex :: UiState -> Int -> Int
pluginRowIndex ui i =
  builtinStageRowCount + sum
    [ 1 + expandedDetailCount name
    | (j, name) <- zip [0..] (uiPluginNames ui)
    , j < i
    ]
  where
    expandedDetailCount name
      | Map.findWithDefault False name (uiPluginExpanded ui) =
          length (Map.findWithDefault [] name (uiPluginDiagnosticLines ui))
          + length (Map.findWithDefault [] name (uiPluginParamSpecs ui))
      | otherwise = 0

data LeftTab
  = LeftTopo
  | LeftView
  deriving (Eq, Show)

-- | Active modal overlay mode.
data UiMenuMode
  = MenuNone
  | MenuEscape
  | MenuPresetSave
  | MenuPresetLoad
  | MenuWorldSave
  | MenuWorldLoad
  | MenuOverlayInspector
  deriving (Eq, Show)

data PipelineStageRunState = PipelineStageRunState
  { psrsStageId :: !StageId
  , psrsStageName :: !Text
  , psrsStatus :: !StageStatus
  , psrsElapsedMs :: !(Maybe Int)
  , psrsDetail :: !(Maybe Text)
  } deriving (Eq, Show)

data UiState = UiState
  { uiSeed :: !Word64
  , uiGenerating :: !Bool
  , uiViewSelection :: !LayeredViewState
  , uiChunkSize :: !Int
  , uiShowConfig :: !Bool
  , uiShowLeftPanel :: !Bool
  , uiConfigTab :: !ConfigTab
  , uiConfigScroll :: !Int
  , uiLeftTab :: !LeftTab
  , uiLeftViewScroll :: !Int
  , uiMenuMode :: !UiMenuMode
  , uiPresetInput :: !Text
  , uiPresetList :: ![Text]
  , uiPresetSelected :: !Int
  , uiPresetFilter :: !Text
  , uiContextHex :: !(Maybe (Int, Int))
  , uiContextPos :: !(Maybe (Int, Int))
  , uiHexTooltipPinned :: !Bool
  , uiSeedEditing :: !Bool
  , uiSeedInput :: !Text
  , uiWaterLevel :: !Float
  , uiRenderWaterLevel :: !Float
  , uiOrographicLift :: !Float
  , uiRainShadowLoss :: !Float
  , uiWindDiffuse :: !Float
  , uiRainRate :: !Float
  , uiErosionHydraulic :: !Float
  , uiErosionThermal :: !Float
  , uiErosionTalus :: !Float
  , uiErosionMaxDrop :: !Float
  , uiErosionHydDeposit :: !Float
  , uiErosionDepositSlope :: !Float
  , uiErosionThermDeposit :: !Float
  , uiErosionCoastZone :: !Float
  , uiErosionCoastStrength :: !Float
  , uiErosionCoastIter :: !Float
  , uiHypsometryEnabled :: !Float
  , uiHypsometryLowlandExp :: !Float
  , uiHypsometryHighlandExp :: !Float
  , uiHypsometryPlateauBreak :: !Float
  , uiHypsometryOceanExp :: !Float
  , uiHypsometryCoastalRampWidth :: !Float
  , uiHypsometryCoastalRampStr :: !Float
  , uiGlacierSnowTemp :: !Float
  , uiGlacierSnowRange :: !Float
  , uiGlacierMeltTemp :: !Float
  , uiGlacierMeltRate :: !Float
  , uiGlacierAccumScale :: !Float
  , uiGlacierFlowIters :: !Float
  , uiGlacierFlowRate :: !Float
  , uiGlacierErosionScale :: !Float
  , uiGlacierCarveScale :: !Float
  , uiGlacierDepositScale :: !Float
  , uiVentDensity :: !Float
  , uiVentThreshold :: !Float
  , uiHotspotScale :: !Float
  , uiHotspotThreshold :: !Float
  , uiMagmaRecharge :: !Float
  , uiLavaScale :: !Float
  , uiAshScale :: !Float
  , uiVolcanicDepositScale :: !Float
  , uiSoilMoistureThreshold :: !Float
  , uiSoilHardnessThreshold :: !Float
  , uiSoilFertilityMoistWeight :: !Float
  , uiSoilFertilityDepthWeight :: !Float
  , uiSinkBreachDepth :: !Float
  , uiStreamPowerMaxErosion :: !Float
  , uiRiverCarveMaxDepth :: !Float
  , uiCoastalErodeStrength :: !Float
  , uiHydroHardnessWeight :: !Float
  , uiPiedmontSmooth :: !Float
  , uiPiedmontSlopeMin :: !Float
  , uiPiedmontSlopeMax :: !Float
  , uiMinLakeSize :: !Float
  , uiInlandSeaMinSize :: !Float
  , uiRoughnessScale :: !Float
  , uiEquatorTemp :: !Float
  , uiPoleTemp :: !Float
  , uiLapseRate :: !Float
  , uiLatitudeExponent :: !Float
  , uiPlateHeightCooling :: !Float
  , uiTempNoiseScale :: !Float
  , uiOceanModeration :: !Float
  , uiOceanModerateTemp :: !Float
  , uiAlbedoSensitivity :: !Float
  , uiAlbedoReference :: !Float
  , uiMoistAdvect :: !Float
  , uiMoistLocal :: !Float
  , uiMoistWindEvapScale :: !Float
  , uiMoistEvapNoiseScale :: !Float
  , uiMoistBareEvapFrac :: !Float
  , uiMoistVegTranspFrac :: !Float
  , uiMoistWindETScale :: !Float
  , uiMoistCondensationRate :: !Float
  , uiMoistRecycleRate :: !Float
  , uiMoistITCZStrength :: !Float
  , uiMoistITCZWidth :: !Float
  , uiMoistMinVegFloor :: !Float
  , uiOrographicScale :: !Float
  , uiOrographicStep :: !Float
  , uiCoastalIterations :: !Float
  , uiCoastalDiffuse :: !Float
  , uiCoastalMoistureBoost :: !Float
  , uiWindBeltStrength :: !Float
  , uiWindBeltHarmonics :: !Float
  , uiWindBeltBase :: !Float
  , uiWindBeltRange :: !Float
  , uiWindBeltSpeedScale :: !Float
  , uiWindCoriolisDeflection :: !Float
  , uiGenScale :: !Float
  , uiGenCoordScale :: !Float
  , uiGenOffsetX :: !Float
  , uiGenOffsetY :: !Float
  , uiGenFrequency :: !Float
  , uiGenOctaves :: !Float
  , uiGenLacunarity :: !Float
  , uiGenGain :: !Float
  , uiGenWarpScale :: !Float
  , uiGenWarpStrength :: !Float
  , uiWorldExtentX :: !Float
  , uiWorldExtentY :: !Float
  , uiEdgeDepthNorth :: !Float
  , uiEdgeDepthSouth :: !Float
  , uiEdgeDepthEast :: !Float
  , uiEdgeDepthWest :: !Float
  , uiEdgeDepthFalloff :: !Float
  , uiPanOffset :: !(Float, Float)
  , uiZoom :: !Float
  , uiPlateSize :: !Float
  , uiPlateSpeed :: !Float
  , uiBoundarySharpness :: !Float
  , uiBoundaryNoiseScale :: !Float
  , uiBoundaryNoiseStrength :: !Float
  , uiBoundaryWarpOctaves :: !Float
  , uiBoundaryWarpLacunarity :: !Float
  , uiBoundaryWarpGain :: !Float
  , uiPlateMergeScale :: !Float
  , uiPlateMergeBias :: !Float
  , uiPlateDetailScale :: !Float
  , uiPlateDetailStrength :: !Float
  , uiPlateRidgeStrength :: !Float
  , uiPlateHeightBase :: !Float
  , uiPlateHeightVariance :: !Float
  , uiPlateHardnessBase :: !Float
  , uiPlateHardnessVariance :: !Float
  , uiUplift :: !Float
  , uiRiftDepth :: !Float
  , uiTrenchDepth :: !Float
  , uiRidgeHeight :: !Float
  , uiPlateBiasStrength :: !Float
  , uiPlateBiasCenter :: !Float
  , uiPlateBiasEdge :: !Float
  , uiPlateBiasNorth :: !Float
  , uiPlateBiasSouth :: !Float
  , uiTfcCliffSlope :: !Float
  , uiTfcMountainSlope :: !Float
  , uiTfcMountainRelief :: !Float
  , uiTfcHillSlope :: !Float
  , uiTfcRollingSlope :: !Float
  , uiValleyCurvature :: !Float
  , uiTfcElevGradient :: !Float
  , uiTfcPlateauMaxRelief2Ring :: !Float
  , uiTfcPlateauMaxMicroRelief :: !Float
  , uiTfcRollingNearFactor :: !Float
  , uiRockElevationThreshold :: !Float
  , uiRockHardnessThreshold :: !Float
  , uiRockHardnessSecondary :: !Float
  , uiWindIterations :: !Float
  , uiMoistureIterations :: !Float
  , uiWeatherTick :: !Float
  , uiWeatherPhase :: !Float
  , uiWeatherAmplitude :: !Float
  , uiSeasonCycleLength :: !Float
  , uiJitterAmplitude :: !Float
  , uiPressureBase :: !Float
  , uiPressureTempScale :: !Float
  , uiPressureCoriolisScale :: !Float
  , uiSeasonalBase :: !Float
  , uiSeasonalRange :: !Float
  , uiHumidityNoiseScale :: !Float
  , uiPrecipNoiseScale :: !Float
  , uiWeatherITCZWidth :: !Float
  , uiWeatherITCZPrecipBoost :: !Float
  , uiPressureHumidityScale :: !Float
  , uiPressureGradientWindScale :: !Float
  , uiWindNoiseScale :: !Float
  , uiITCZMigrationScale :: !Float
  , uiCloudRHExponent :: !Float
  , uiCloudAlbedoEffect :: !Float
  , uiCloudPrecipBoost :: !Float
  , uiVegBase :: !Float
  , uiVegBoost :: !Float
  , uiVegTempWeight :: !Float
  , uiVegPrecipWeight :: !Float
  , uiBtCoastalBand :: !Float
  , uiBtSnowMaxTemp :: !Float
  , uiBtAlpineMaxTemp :: !Float
  , uiBtIceCapTemp :: !Float
  , uiBtMontaneMaxTemp :: !Float
  , uiBtMontanePrecip :: !Float
  , uiBtCliffSlope :: !Float
  , uiBtValleyMoisture :: !Float
  , uiBtDepressionMoisture :: !Float
  , uiBtPrecipWeight :: !Float
  , uiVbcTempMin :: !Float
  , uiVbcTempRange :: !Float
  , uiVbcFertilityBoost :: !Float
  , uiVbcAlbedoBase :: !Float
  , uiVbcAlbedoBare :: !Float
  , uiVbcAlbedoVeg :: !Float
  , uiVbcOceanAlbedo :: !Float
  , uiVbcIceAlbedo :: !Float
  , uiBiomeSmoothing :: !Float
  , uiVolcanicAshBoost :: !Float
  , uiVolcanicLavaPenalty :: !Float
  , uiBiomeFeedbackBlend :: !Float
  , uiPlanetRadius :: !Float
  , uiAxialTilt :: !Float
  , uiInsolation :: !Float
  , uiOccWarmScale :: !Float
  , uiOccColdScale :: !Float
  , uiOccLatPeakDeg :: !Float
  , uiOccLatWidthDeg :: !Float
  , uiHexSizeKm :: !Float
  , uiSliceLatCenter :: !Float
  , uiSliceLonCenter :: !Float
  , uiDayNightEnabled :: !Bool
  , uiDisabledStages :: !(Set StageId)
  , uiExplicitDisabledStages :: !(Set StageId)
  , uiPipelineStageRuns :: !(Map StageId PipelineStageRunState)
  , uiDisabledPlugins :: !(Set Text)
  , uiPluginParams :: !(Map Text (Map Text Value))
  , uiPluginNames :: ![Text]
  , uiPluginExpanded :: !(Map Text Bool)
  , uiPluginParamSpecs :: !(Map Text [RPCParamSpec])
  , uiPluginLifecycles :: !(Map Text PluginLifecycleSnapshot)
  , uiPluginDiagnosticLines :: !(Map Text [Text])
  , uiPluginDiagnosticStatuses :: !(Map Text Text)
  , uiSimAutoTick :: !Bool
  , uiSimTickRate :: !Float
  , uiSimTickCount :: !Word64
  , uiHoverHex :: !(Maybe (Int, Int))
  , uiHoverWidget :: !(Maybe WidgetId)
  , uiWorldConfig :: !(Maybe ConfigSnapshot)
  , uiWorldName :: !Text
  , uiWorldSaveInput :: !Text
  , uiWorldList :: ![WorldSaveManifest]
  , uiWorldSelected :: !Int
  , uiWorldFilter :: !Text
  , uiWorldDeleteConfirm :: !Bool
  , uiWorldDeleteTarget :: !(Maybe Text)
  , uiWorldDeleteError :: !(Maybe Text)
  , uiOverlayNames :: ![Text]
  , uiOverlayFields :: ![(Text, OverlayFieldType)]
  , uiOverlayInspector :: !OverlayInspectorModel
  , uiDataBrowser :: !DataBrowserState
  , uiDataResources :: !(Map Text [DataResourceSchema])
  , uiEditor :: !EditorState
  } deriving (Eq, Show)

emptyUiState :: UiState
emptyUiState = UiState
  { uiSeed = 0
  , uiGenerating = False
  , uiViewSelection = defaultLayeredViewState
  , uiChunkSize = 64
  , uiShowConfig = False
  , uiShowLeftPanel = True
  , uiConfigTab = ConfigTerrain
  , uiConfigScroll = 0
  , uiLeftTab = LeftTopo
  , uiLeftViewScroll = 0
  , uiMenuMode = MenuNone
  , uiPresetInput = Text.empty
  , uiPresetList = []
  , uiPresetSelected = 0
  , uiPresetFilter = Text.empty
  , uiContextHex = Nothing
  , uiContextPos = Nothing
  , uiHexTooltipPinned = False
  , uiSeedEditing = False
  , uiSeedInput = Text.empty
  , uiWaterLevel = sliderDefault SliderWaterLevel
  , uiRenderWaterLevel = sliderDefault SliderWaterLevel
  , uiOrographicLift = sliderDefault SliderOrographicLift
  , uiRainShadowLoss = sliderDefault SliderRainShadowLoss
  , uiWindDiffuse = sliderDefault SliderWindDiffuse
  , uiRainRate = sliderDefault SliderErosionRainRate
  , uiErosionHydraulic = sliderDefault SliderErosionHydraulic
  , uiErosionThermal = sliderDefault SliderErosionThermal
  , uiErosionTalus = sliderDefault SliderErosionTalus
  , uiErosionMaxDrop = sliderDefault SliderErosionMaxDrop
  , uiErosionHydDeposit = sliderDefault SliderErosionHydDeposit
  , uiErosionDepositSlope = sliderDefault SliderErosionDepositSlope
  , uiErosionThermDeposit = sliderDefault SliderErosionThermDeposit
  , uiErosionCoastZone = sliderDefault SliderErosionCoastZone
  , uiErosionCoastStrength = sliderDefault SliderErosionCoastStrength
  , uiErosionCoastIter = sliderDefault SliderErosionCoastIter
  , uiHypsometryEnabled = sliderDefault SliderHypsometryEnabled
  , uiHypsometryLowlandExp = sliderDefault SliderHypsometryLowlandExp
  , uiHypsometryHighlandExp = sliderDefault SliderHypsometryHighlandExp
  , uiHypsometryPlateauBreak = sliderDefault SliderHypsometryPlateauBreak
  , uiHypsometryOceanExp = sliderDefault SliderHypsometryOceanExp
  , uiHypsometryCoastalRampWidth = sliderDefault SliderHypsometryCoastalRampWidth
  , uiHypsometryCoastalRampStr = sliderDefault SliderHypsometryCoastalRampStr
  , uiGlacierSnowTemp = sliderDefault SliderGlacierSnowTemp
  , uiGlacierSnowRange = sliderDefault SliderGlacierSnowRange
  , uiGlacierMeltTemp = sliderDefault SliderGlacierMeltTemp
  , uiGlacierMeltRate = sliderDefault SliderGlacierMeltRate
  , uiGlacierAccumScale = sliderDefault SliderGlacierAccumScale
  , uiGlacierFlowIters = sliderDefault SliderGlacierFlowIters
  , uiGlacierFlowRate = sliderDefault SliderGlacierFlowRate
  , uiGlacierErosionScale = sliderDefault SliderGlacierErosionScale
  , uiGlacierCarveScale = sliderDefault SliderGlacierCarveScale
  , uiGlacierDepositScale = sliderDefault SliderGlacierDepositScale
  , uiVentDensity = sliderDefault SliderVentDensity
  , uiVentThreshold = sliderDefault SliderVentThreshold
  , uiHotspotScale = sliderDefault SliderHotspotScale
  , uiHotspotThreshold = sliderDefault SliderHotspotThreshold
  , uiMagmaRecharge = sliderDefault SliderMagmaRecharge
  , uiLavaScale = sliderDefault SliderLavaScale
  , uiAshScale = sliderDefault SliderAshScale
  , uiVolcanicDepositScale = sliderDefault SliderVolcanicDepositScale
  , uiSoilMoistureThreshold = sliderDefault SliderSoilMoistureThreshold
  , uiSoilHardnessThreshold = sliderDefault SliderSoilHardnessThreshold
  , uiSoilFertilityMoistWeight = sliderDefault SliderSoilFertilityMoistWeight
  , uiSoilFertilityDepthWeight = sliderDefault SliderSoilFertilityDepthWeight
  , uiSinkBreachDepth = sliderDefault SliderSinkBreachDepth
  , uiStreamPowerMaxErosion = sliderDefault SliderStreamPowerMaxErosion
  , uiRiverCarveMaxDepth = sliderDefault SliderRiverCarveMaxDepth
  , uiCoastalErodeStrength = sliderDefault SliderCoastalErodeStrength
  , uiHydroHardnessWeight = sliderDefault SliderHydroHardnessWeight
  , uiPiedmontSmooth = sliderDefault SliderPiedmontSmooth
  , uiPiedmontSlopeMin = sliderDefault SliderPiedmontSlopeMin
  , uiPiedmontSlopeMax = sliderDefault SliderPiedmontSlopeMax
  , uiMinLakeSize = sliderDefault SliderMinLakeSize
  , uiInlandSeaMinSize = sliderDefault SliderInlandSeaMinSize
  , uiRoughnessScale = sliderDefault SliderRoughnessScale
  , uiEquatorTemp = sliderDefault SliderEquatorTemp
  , uiPoleTemp = sliderDefault SliderPoleTemp
  , uiLapseRate = sliderDefault SliderLapseRate
  , uiLatitudeExponent = sliderDefault SliderLatitudeExponent
  , uiPlateHeightCooling = sliderDefault SliderPlateHeightCooling
  , uiTempNoiseScale = sliderDefault SliderTempNoiseScale
  , uiOceanModeration = sliderDefault SliderOceanModeration
  , uiOceanModerateTemp = sliderDefault SliderOceanModerateTemp
  , uiAlbedoSensitivity = sliderDefault SliderAlbedoSensitivity
  , uiAlbedoReference = sliderDefault SliderAlbedoReference
  , uiMoistAdvect = sliderDefault SliderMoistAdvect
  , uiMoistLocal = sliderDefault SliderMoistLocal
  , uiMoistWindEvapScale = sliderDefault SliderMoistWindEvapScale
  , uiMoistEvapNoiseScale = sliderDefault SliderMoistEvapNoiseScale
  , uiMoistBareEvapFrac = sliderDefault SliderMoistBareEvapFrac
  , uiMoistVegTranspFrac = sliderDefault SliderMoistVegTranspFrac
  , uiMoistWindETScale = sliderDefault SliderMoistWindETScale
  , uiMoistCondensationRate = sliderDefault SliderMoistCondensationRate
  , uiMoistRecycleRate = sliderDefault SliderMoistRecycleRate
  , uiMoistITCZStrength = sliderDefault SliderMoistITCZStrength
  , uiMoistITCZWidth = sliderDefault SliderMoistITCZWidth
  , uiMoistMinVegFloor = sliderDefault SliderMoistMinVegFloor
  , uiOrographicScale = sliderDefault SliderOrographicScale
  , uiOrographicStep = sliderDefault SliderOrographicStep
  , uiCoastalIterations = sliderDefault SliderCoastalIterations
  , uiCoastalDiffuse = sliderDefault SliderCoastalDiffuse
  , uiCoastalMoistureBoost = sliderDefault SliderCoastalMoistureBoost
  , uiWindBeltStrength = sliderDefault SliderWindBeltStrength
  , uiWindBeltHarmonics = sliderDefault SliderWindBeltHarmonics
  , uiWindBeltBase = sliderDefault SliderWindBeltBase
  , uiWindBeltRange = sliderDefault SliderWindBeltRange
  , uiWindBeltSpeedScale = sliderDefault SliderWindBeltSpeedScale
  , uiWindCoriolisDeflection = sliderDefault SliderWindCoriolisDeflection
  , uiGenScale = sliderDefault SliderGenScale
  , uiGenCoordScale = sliderDefault SliderGenCoordScale
  , uiGenOffsetX = sliderDefault SliderGenOffsetX
  , uiGenOffsetY = sliderDefault SliderGenOffsetY
  , uiGenFrequency = sliderDefault SliderGenFrequency
  , uiGenOctaves = sliderDefault SliderGenOctaves
  , uiGenLacunarity = sliderDefault SliderGenLacunarity
  , uiGenGain = sliderDefault SliderGenGain
  , uiGenWarpScale = sliderDefault SliderGenWarpScale
  , uiGenWarpStrength = sliderDefault SliderGenWarpStrength
  , uiWorldExtentX = sliderDefault SliderExtentX
  , uiWorldExtentY = sliderDefault SliderExtentY
  , uiEdgeDepthNorth = sliderDefault SliderEdgeNorth
  , uiEdgeDepthSouth = sliderDefault SliderEdgeSouth
  , uiEdgeDepthEast = sliderDefault SliderEdgeEast
  , uiEdgeDepthWest = sliderDefault SliderEdgeWest
  , uiEdgeDepthFalloff = sliderDefault SliderEdgeFalloff
  , uiPanOffset = (0, 0)
  , uiZoom = 1
  , uiPlateSize = sliderDefault SliderPlateSize
  , uiPlateSpeed = sliderDefault SliderPlateSpeed
  , uiBoundarySharpness = sliderDefault SliderBoundarySharpness
  , uiBoundaryNoiseScale = sliderDefault SliderBoundaryNoiseScale
  , uiBoundaryNoiseStrength = sliderDefault SliderBoundaryNoiseStrength
  , uiBoundaryWarpOctaves = sliderDefault SliderBoundaryWarpOctaves
  , uiBoundaryWarpLacunarity = sliderDefault SliderBoundaryWarpLacunarity
  , uiBoundaryWarpGain = sliderDefault SliderBoundaryWarpGain
  , uiPlateMergeScale = sliderDefault SliderPlateMergeScale
  , uiPlateMergeBias = sliderDefault SliderPlateMergeBias
  , uiPlateDetailScale = sliderDefault SliderPlateDetailScale
  , uiPlateDetailStrength = sliderDefault SliderPlateDetailStrength
  , uiPlateRidgeStrength = sliderDefault SliderPlateRidgeStrength
  , uiPlateHeightBase = sliderDefault SliderPlateHeightBase
  , uiPlateHeightVariance = sliderDefault SliderPlateHeightVariance
  , uiPlateHardnessBase = sliderDefault SliderPlateHardnessBase
  , uiPlateHardnessVariance = sliderDefault SliderPlateHardnessVariance
  , uiUplift = sliderDefault SliderUplift
  , uiRiftDepth = sliderDefault SliderRiftDepth
  , uiTrenchDepth = sliderDefault SliderTrenchDepth
  , uiRidgeHeight = sliderDefault SliderRidgeHeight
  , uiPlateBiasStrength = sliderDefault SliderPlateBiasStrength
  , uiPlateBiasCenter = sliderDefault SliderPlateBiasCenter
  , uiPlateBiasEdge = sliderDefault SliderPlateBiasEdge
  , uiPlateBiasNorth = sliderDefault SliderPlateBiasNorth
  , uiPlateBiasSouth = sliderDefault SliderPlateBiasSouth
  , uiTfcCliffSlope = sliderDefault SliderTfcCliffSlope
  , uiTfcMountainSlope = sliderDefault SliderTfcMountainSlope
  , uiTfcMountainRelief = sliderDefault SliderTfcMountainRelief
  , uiTfcHillSlope = sliderDefault SliderTfcHillSlope
  , uiTfcRollingSlope = sliderDefault SliderTfcRollingSlope
  , uiValleyCurvature = sliderDefault SliderValleyCurvature
  , uiTfcElevGradient = sliderDefault SliderTfcElevGradient
  , uiTfcPlateauMaxRelief2Ring = sliderDefault SliderTfcPlateauMaxRelief2Ring
  , uiTfcPlateauMaxMicroRelief = sliderDefault SliderTfcPlateauMaxMicroRelief
  , uiTfcRollingNearFactor = sliderDefault SliderTfcRollingNearFactor
  , uiRockElevationThreshold = sliderDefault SliderRockElevationThreshold
  , uiRockHardnessThreshold = sliderDefault SliderRockHardnessThreshold
  , uiRockHardnessSecondary = sliderDefault SliderRockHardnessSecondary
  , uiWindIterations = sliderDefault SliderWindIterations
  , uiMoistureIterations = sliderDefault SliderMoistureIterations
  , uiWeatherTick = sliderDefault SliderWeatherTick
  , uiWeatherPhase = sliderDefault SliderWeatherPhase
  , uiWeatherAmplitude = sliderDefault SliderWeatherAmplitude
  , uiSeasonCycleLength = sliderDefault SliderSeasonCycleLength
  , uiJitterAmplitude = sliderDefault SliderJitterAmplitude
  , uiPressureBase = sliderDefault SliderPressureBase
  , uiPressureTempScale = sliderDefault SliderPressureTempScale
  , uiPressureCoriolisScale = sliderDefault SliderPressureCoriolisScale
  , uiSeasonalBase = sliderDefault SliderSeasonalBase
  , uiSeasonalRange = sliderDefault SliderSeasonalRange
  , uiHumidityNoiseScale = sliderDefault SliderHumidityNoiseScale
  , uiPrecipNoiseScale = sliderDefault SliderPrecipNoiseScale
  , uiWeatherITCZWidth = sliderDefault SliderWeatherITCZWidth
  , uiWeatherITCZPrecipBoost = sliderDefault SliderWeatherITCZPrecipBoost
  , uiPressureHumidityScale = sliderDefault SliderPressureHumidityScale
  , uiPressureGradientWindScale = sliderDefault SliderPressureGradientWindScale
  , uiWindNoiseScale = sliderDefault SliderWindNoiseScale
  , uiITCZMigrationScale = sliderDefault SliderITCZMigrationScale
  , uiCloudRHExponent = sliderDefault SliderCloudRHExponent
  , uiCloudAlbedoEffect = sliderDefault SliderCloudAlbedoEffect
  , uiCloudPrecipBoost = sliderDefault SliderCloudPrecipBoost
  , uiVegBase = sliderDefault SliderVegBase
  , uiVegBoost = sliderDefault SliderVegBoost
  , uiVegTempWeight = sliderDefault SliderVegTempWeight
  , uiVegPrecipWeight = sliderDefault SliderVegPrecipWeight
  , uiBtCoastalBand = sliderDefault SliderBtCoastalBand
  , uiBtSnowMaxTemp = sliderDefault SliderBtSnowMaxTemp
  , uiBtAlpineMaxTemp = sliderDefault SliderBtAlpineMaxTemp
  , uiBtIceCapTemp = sliderDefault SliderBtIceCapTemp
  , uiBtMontaneMaxTemp = sliderDefault SliderBtMontaneMaxTemp
  , uiBtMontanePrecip = sliderDefault SliderBtMontanePrecip
  , uiBtCliffSlope = sliderDefault SliderBtCliffSlope
  , uiBtValleyMoisture = sliderDefault SliderBtValleyMoisture
  , uiBtDepressionMoisture = sliderDefault SliderBtDepressionMoisture
  , uiBtPrecipWeight = sliderDefault SliderBtPrecipWeight
  , uiVbcTempMin = sliderDefault SliderVbcTempMin
  , uiVbcTempRange = sliderDefault SliderVbcTempRange
  , uiVbcFertilityBoost = sliderDefault SliderVbcFertilityBoost
  , uiVbcAlbedoBase = sliderDefault SliderVbcAlbedoBase
  , uiVbcAlbedoBare = sliderDefault SliderVbcAlbedoBare
  , uiVbcAlbedoVeg = sliderDefault SliderVbcAlbedoVeg
  , uiVbcOceanAlbedo = sliderDefault SliderVbcOceanAlbedo
  , uiVbcIceAlbedo = sliderDefault SliderVbcIceAlbedo
  , uiBiomeSmoothing = sliderDefault SliderBiomeSmoothing
  , uiVolcanicAshBoost = sliderDefault SliderVolcanicAshBoost
  , uiVolcanicLavaPenalty = sliderDefault SliderVolcanicLavaPenalty
  , uiBiomeFeedbackBlend = sliderDefault SliderBiomeFeedbackBlend
  , uiPlanetRadius = sliderDefault SliderPlanetRadius
  , uiAxialTilt = sliderDefault SliderAxialTilt
  , uiInsolation = sliderDefault SliderInsolation
  , uiOccWarmScale = sliderDefault SliderOccWarmScale
  , uiOccColdScale = sliderDefault SliderOccColdScale
  , uiOccLatPeakDeg = sliderDefault SliderOccLatPeakDeg
  , uiOccLatWidthDeg = sliderDefault SliderOccLatWidthDeg
  , uiHexSizeKm = sliderDefault SliderHexSizeKm
  , uiSliceLatCenter = sliderDefault SliderSliceLatCenter
  , uiSliceLonCenter = sliderDefault SliderSliceLonCenter
  , uiDayNightEnabled = False
  , uiDisabledStages = Set.empty
  , uiExplicitDisabledStages = Set.empty
  , uiPipelineStageRuns = Map.empty
  , uiDisabledPlugins = Set.empty
  , uiPluginParams = Map.empty
  , uiPluginNames = []
  , uiPluginExpanded = Map.empty
  , uiPluginParamSpecs = Map.empty
  , uiPluginLifecycles = Map.empty
  , uiPluginDiagnosticLines = Map.empty
  , uiPluginDiagnosticStatuses = Map.empty
  , uiSimAutoTick = False
  , uiSimTickRate = 0.5
  , uiSimTickCount = 0
  , uiHoverHex = Nothing
  , uiHoverWidget = Nothing
  , uiWorldConfig = Nothing
  , uiWorldName = Text.pack "Untitled"
  , uiWorldSaveInput = Text.empty
  , uiWorldList = []
  , uiWorldSelected = 0
  , uiWorldFilter = Text.empty
  , uiWorldDeleteConfirm = False
  , uiWorldDeleteTarget = Nothing
  , uiWorldDeleteError = Nothing
  , uiOverlayNames = []
  , uiOverlayFields = []
  , uiOverlayInspector = emptyOverlayInspectorModel
  , uiDataBrowser = emptyDataBrowserState
  , uiDataResources = Map.empty
  , uiEditor = defaultEditorState
  }

-- | Build UI display world time from the canonical simulation clock.
--
-- 'uiSimTickRate' is a wall-clock auto-scheduler control, so it must not
-- influence calendar, solar, or day/night calculations at a fixed tick.
uiWorldTime :: UiState -> WorldTime
uiWorldTime ui = WorldTime
  { wtTick = uiSimTickCount ui
  , wtTickRate = simulationTickSeconds
  }

-- | Canonical layered selection accessor retained to avoid churn at callers
-- that adopted the layered model before it became the sole stored state.
effectiveViewSelection :: UiState -> LayeredViewState
effectiveViewSelection = uiViewSelection

-- | Read-only legacy projection for external protocol compatibility. Internal
-- rendering and scheduling must consume 'uiViewSelection' instead.
uiViewMode :: UiState -> ViewMode
uiViewMode ui =
  case layeredViewStateToViewMode (uiViewSelection ui) of
    Just mode -> mode
    Nothing -> baseViewModeToViewMode (lvsBaseView (uiViewSelection ui))

sliderDefault :: SliderId -> Float
sliderDefault = sliderDefaultValueForId

data UiUpdate
  = SetSeed !Word64
  | SetGenerating !Bool
  | SetViewMode !ViewMode
  | SetViewSelection !LayeredViewState
  | SetBaseViewMode !BaseViewMode
  | SetSkyOverlayMode !(Maybe SkyOverlayMode)
  | SetWeatherBasis !WeatherBasis
  | SetOverlayOpacity !Float
  | SetChunkSize !Int
  | SetShowConfig !Bool
  | SetShowLeftPanel !Bool
  | SetConfigTab !ConfigTab
  | SetConfigScroll !Int
  | SetLeftTab !LeftTab
  | SetLeftViewScroll !Int
  | SetMenuMode !UiMenuMode
  | SetPresetInput !Text
  | SetPresetList ![Text]
  | SetPresetSelected !Int
  | SetPresetFilter !Text
  | SetContextHex !(Maybe (Int, Int))
  | SetContextPos !(Maybe (Int, Int))
  | SetHexTooltipPinned !Bool
  | SetSeedEditing !Bool
  | SetSeedInput !Text
  | SetSliderValue !SliderId !Float
  | SetRenderWaterLevel !Float
  | SetPanOffset !(Float, Float)
  | SetZoom !Float
  | SetHoverHex !(Maybe (Int, Int))
  | SetHoverWidget !(Maybe WidgetId)
  | SetDisabledStages !(Set StageId)
  | SetExplicitDisabledStages !(Set StageId)
  | SetPipelineStageRun !PipelineStageRunState
  | SetDisabledPlugins !(Set Text)
  | SetPluginParam !Text !Text !Value
  | SetPluginNames ![Text]
  | SetPluginExpanded !Text !Bool
  | SetPluginParamSpecs !(Map Text [RPCParamSpec])
  | SetPluginLifecycles !(Map Text PluginLifecycleSnapshot)
  | SetPluginDiagnosticLines !(Map Text [Text])
  | SetPluginDiagnosticStatuses !(Map Text Text)
  | SetDayNightEnabled !Bool
  | SetSimAutoTick !Bool
  | SetSimTickRate !Float
  | SetSimTickCount !Word64
  | SetWorldConfig !(Maybe ConfigSnapshot)
  | SetWorldName !Text
  | SetWorldSaveInput !Text
  | SetWorldList ![WorldSaveManifest]
  | SetWorldSelected !Int
  | SetWorldFilter !Text
  | SetWorldDeleteConfirm !Bool
  | SetWorldDeleteTarget !(Maybe Text)
  | SetWorldDeleteError !(Maybe Text)
  | SetOverlayNames ![Text]
  | SetOverlayFields ![(Text, OverlayFieldType)]
  | SetOverlayInspector !OverlayInspectorModel
  | OpenOverlayInspector !OverlayInspectorView
  | CloseOverlayInspector
  | SetOverlayInspectorFocus !OverlayInspectorFocus
  | SetOverlayInspectorScroll !Int
  | MoveOverlayInspectorSelection !Int
  | SelectOverlayInspectorOverlay !(Maybe Text)
  | SetOverlayInspectorImportText !Text !Int
  | SetOverlayInspectorImportDraft !Value
  | ApplyOverlayInspectorValidationPreparation !(Either Text Value)
  | SetOverlayInspectorNotice !(Maybe Text)
  | SetDataBrowser !DataBrowserState
  | SetDataResources !(Map Text [DataResourceSchema])
  | SetEditor !EditorState

applyUpdate :: UiUpdate -> UiState -> UiState
applyUpdate upd st = case upd of
  SetSeed v -> st { uiSeed = v }
  SetGenerating v -> st { uiGenerating = v }
  SetViewMode v -> applyLayeredViewState (viewModeToLayeredViewState v) st
  SetViewSelection v -> applyLayeredViewState v st
  SetBaseViewMode v -> applyLayeredViewState ((uiViewSelection st) { lvsBaseView = v }) st
  SetSkyOverlayMode v -> applyLayeredViewState ((uiViewSelection st) { lvsSkyOverlay = v }) st
  SetWeatherBasis v -> applyLayeredViewState ((uiViewSelection st) { lvsWeatherBasis = v }) st
  SetOverlayOpacity v -> applyLayeredViewState ((uiViewSelection st) { lvsOverlayOpacity = v }) st
  SetChunkSize v -> st { uiChunkSize = clampChunk v }
  SetShowConfig v -> st { uiShowConfig = v }
  SetShowLeftPanel v -> st { uiShowLeftPanel = v }
  SetConfigTab v -> st { uiConfigTab = v }
  SetConfigScroll v -> st { uiConfigScroll = max 0 v }
  SetLeftTab v -> st { uiLeftTab = v }
  SetLeftViewScroll v -> st { uiLeftViewScroll = max 0 v }
  SetMenuMode v -> st { uiMenuMode = v }
  SetPresetInput v -> st { uiPresetInput = v }
  SetPresetList v -> st { uiPresetList = v }
  SetPresetSelected v -> st { uiPresetSelected = max 0 v }
  SetPresetFilter v -> st { uiPresetFilter = v }
  SetContextHex v -> st { uiContextHex = v }
  SetContextPos v -> st { uiContextPos = v }
  SetHexTooltipPinned v -> st { uiHexTooltipPinned = v }
  SetSeedEditing v -> st { uiSeedEditing = v }
  SetSeedInput v -> st { uiSeedInput = v }
  SetSliderValue sliderIdValue v -> applySliderValue sliderIdValue v st
  SetRenderWaterLevel v -> st { uiRenderWaterLevel = clamp01 v }
  SetPanOffset v -> st { uiPanOffset = v }
  SetZoom v -> st { uiZoom = clampZoom v }
  SetHoverHex v -> st { uiHoverHex = v }
  SetHoverWidget v -> st { uiHoverWidget = v }
  SetDisabledStages v -> st { uiDisabledStages = v }
  SetExplicitDisabledStages v -> st { uiExplicitDisabledStages = v }
  SetPipelineStageRun runState ->
    st { uiPipelineStageRuns = Map.insert (psrsStageId runState) runState (uiPipelineStageRuns st) }
  SetDisabledPlugins v -> st { uiDisabledPlugins = v }
  SetPluginParam pluginName paramName value ->
    let inner = Map.findWithDefault Map.empty pluginName (uiPluginParams st)
    in st
         { uiPluginParams =
             Map.insert pluginName (Map.insert paramName value inner) (uiPluginParams st)
         }
  SetPluginNames v -> st { uiPluginNames = v }
  SetPluginExpanded name expanded ->
    st { uiPluginExpanded = Map.insert name expanded (uiPluginExpanded st) }
  SetPluginParamSpecs v -> st { uiPluginParamSpecs = v }
  SetPluginLifecycles v -> st { uiPluginLifecycles = v }
  SetPluginDiagnosticLines v -> st { uiPluginDiagnosticLines = v }
  SetPluginDiagnosticStatuses v -> st { uiPluginDiagnosticStatuses = v }
  SetDayNightEnabled v -> st { uiDayNightEnabled = v }
  SetSimAutoTick v -> st { uiSimAutoTick = v }
  SetSimTickRate v -> st { uiSimTickRate = clamp01 v }
  SetSimTickCount v -> st { uiSimTickCount = v }
  SetWorldConfig v -> st { uiWorldConfig = v }
  SetWorldName v -> st { uiWorldName = v }
  SetWorldSaveInput v -> st { uiWorldSaveInput = v }
  SetWorldList v -> st { uiWorldList = v }
  SetWorldSelected v -> st { uiWorldSelected = v }
  SetWorldFilter v -> st { uiWorldFilter = v }
  SetWorldDeleteConfirm v -> st { uiWorldDeleteConfirm = v }
  SetWorldDeleteTarget v -> st { uiWorldDeleteTarget = v }
  SetWorldDeleteError v -> st { uiWorldDeleteError = v }
  SetOverlayNames v -> st { uiOverlayNames = v }
  SetOverlayFields v -> st { uiOverlayFields = v }
  SetOverlayInspector v -> st { uiOverlayInspector = v }
  OpenOverlayInspector v -> st
    { uiMenuMode = MenuOverlayInspector
    , uiSeedEditing = False
    , uiDataBrowser = (uiDataBrowser st) { dbsFocusedField = Nothing }
    , uiOverlayInspector = openOverlayInspectorView v (uiOverlayInspector st)
    }
  CloseOverlayInspector -> st
    { uiMenuMode = MenuNone
    , uiOverlayInspector = closeOverlayInspectorView (uiOverlayInspector st)
    }
  SetOverlayInspectorFocus v -> st
    { uiOverlayInspector = setOverlayInspectorFocus v (uiOverlayInspector st) }
  SetOverlayInspectorScroll v -> st
    { uiOverlayInspector = setOverlayInspectorScroll v (uiOverlayInspector st) }
  MoveOverlayInspectorSelection v -> st
    { uiOverlayInspector = moveOverlayInspectorSelection v (uiOverlayInspector st) }
  SelectOverlayInspectorOverlay v -> st
    { uiOverlayInspector = selectOverlayInspectorOverlay v (uiOverlayInspector st) }
  SetOverlayInspectorImportText text cursor -> st
    { uiOverlayInspector = setOverlayInspectorImportText text cursor (uiOverlayInspector st) }
  SetOverlayInspectorImportDraft draft -> st
    { uiOverlayInspector = setOverlayInspectorImportDraft draft (uiOverlayInspector st) }
  ApplyOverlayInspectorValidationPreparation parsed -> st
    { uiOverlayInspector = applyOverlayInspectorValidationPreparation parsed (uiOverlayInspector st) }
  SetOverlayInspectorNotice notice -> st
    { uiOverlayInspector = setOverlayInspectorNotice notice (uiOverlayInspector st) }
  SetDataBrowser v -> st { uiDataBrowser = v }
  SetDataResources v -> st { uiDataResources = v }
  SetEditor v -> st { uiEditor = v }

applyLayeredViewState :: LayeredViewState -> UiState -> UiState
applyLayeredViewState selection st =
  st { uiViewSelection = normalizeLayeredViewState selection }

normalizeLayeredViewState :: LayeredViewState -> LayeredViewState
normalizeLayeredViewState selection =
  selection { lvsOverlayOpacity = clamp01 (lvsOverlayOpacity selection) }

data SliderStateBinding = SliderStateBinding
  { sliderStateGet :: UiState -> Float
  , sliderStatePut :: Float -> UiState -> UiState
  }

sliderStateBindingForId :: SliderId -> SliderStateBinding
sliderStateBindingForId sliderIdValue = case sliderIdValue of
  SliderGenScale -> binding uiGenScale (\value st -> st { uiGenScale = clamp01 value })
  SliderGenCoordScale -> binding uiGenCoordScale (\value st -> st { uiGenCoordScale = clamp01 value })
  SliderGenOffsetX -> binding uiGenOffsetX (\value st -> st { uiGenOffsetX = clamp01 value })
  SliderGenOffsetY -> binding uiGenOffsetY (\value st -> st { uiGenOffsetY = clamp01 value })
  SliderGenFrequency -> binding uiGenFrequency (\value st -> st { uiGenFrequency = clamp01 value })
  SliderGenOctaves -> binding uiGenOctaves (\value st -> st { uiGenOctaves = clamp01 value })
  SliderGenLacunarity -> binding uiGenLacunarity (\value st -> st { uiGenLacunarity = clamp01 value })
  SliderGenGain -> binding uiGenGain (\value st -> st { uiGenGain = clamp01 value })
  SliderGenWarpScale -> binding uiGenWarpScale (\value st -> st { uiGenWarpScale = clamp01 value })
  SliderGenWarpStrength -> binding uiGenWarpStrength (\value st -> st { uiGenWarpStrength = clamp01 value })
  SliderExtentX -> binding uiWorldExtentX (\value st -> st { uiWorldExtentX = clamp01 value })
  SliderExtentY -> binding uiWorldExtentY (\value st -> st { uiWorldExtentY = clamp01 value })
  SliderEdgeNorth -> binding uiEdgeDepthNorth (\value st -> st { uiEdgeDepthNorth = clamp01 value })
  SliderEdgeSouth -> binding uiEdgeDepthSouth (\value st -> st { uiEdgeDepthSouth = clamp01 value })
  SliderEdgeEast -> binding uiEdgeDepthEast (\value st -> st { uiEdgeDepthEast = clamp01 value })
  SliderEdgeWest -> binding uiEdgeDepthWest (\value st -> st { uiEdgeDepthWest = clamp01 value })
  SliderEdgeFalloff -> binding uiEdgeDepthFalloff (\value st -> st { uiEdgeDepthFalloff = clamp01 value })
  SliderPlateSize -> binding uiPlateSize (\value st -> st { uiPlateSize = clamp01 value })
  SliderUplift -> binding uiUplift (\value st -> st { uiUplift = clamp01 value })
  SliderRiftDepth -> binding uiRiftDepth (\value st -> st { uiRiftDepth = clamp01 value })
  SliderPlateSpeed -> binding uiPlateSpeed (\value st -> st { uiPlateSpeed = clamp01 value })
  SliderBoundarySharpness -> binding uiBoundarySharpness (\value st -> st { uiBoundarySharpness = clamp01 value })
  SliderBoundaryNoiseScale -> binding uiBoundaryNoiseScale (\value st -> st { uiBoundaryNoiseScale = clamp01 value })
  SliderBoundaryNoiseStrength -> binding uiBoundaryNoiseStrength (\value st -> st { uiBoundaryNoiseStrength = clamp01 value })
  SliderBoundaryWarpOctaves -> binding uiBoundaryWarpOctaves (\value st -> st { uiBoundaryWarpOctaves = clamp01 value })
  SliderBoundaryWarpLacunarity -> binding uiBoundaryWarpLacunarity (\value st -> st { uiBoundaryWarpLacunarity = clamp01 value })
  SliderBoundaryWarpGain -> binding uiBoundaryWarpGain (\value st -> st { uiBoundaryWarpGain = clamp01 value })
  SliderPlateMergeScale -> binding uiPlateMergeScale (\value st -> st { uiPlateMergeScale = clamp01 value })
  SliderPlateMergeBias -> binding uiPlateMergeBias (\value st -> st { uiPlateMergeBias = clamp01 value })
  SliderPlateDetailScale -> binding uiPlateDetailScale (\value st -> st { uiPlateDetailScale = clamp01 value })
  SliderPlateDetailStrength -> binding uiPlateDetailStrength (\value st -> st { uiPlateDetailStrength = clamp01 value })
  SliderPlateRidgeStrength -> binding uiPlateRidgeStrength (\value st -> st { uiPlateRidgeStrength = clamp01 value })
  SliderPlateHeightBase -> binding uiPlateHeightBase (\value st -> st { uiPlateHeightBase = clamp01 value })
  SliderPlateHeightVariance -> binding uiPlateHeightVariance (\value st -> st { uiPlateHeightVariance = clamp01 value })
  SliderPlateHardnessBase -> binding uiPlateHardnessBase (\value st -> st { uiPlateHardnessBase = clamp01 value })
  SliderPlateHardnessVariance -> binding uiPlateHardnessVariance (\value st -> st { uiPlateHardnessVariance = clamp01 value })
  SliderTrenchDepth -> binding uiTrenchDepth (\value st -> st { uiTrenchDepth = clamp01 value })
  SliderRidgeHeight -> binding uiRidgeHeight (\value st -> st { uiRidgeHeight = clamp01 value })
  SliderPlateBiasStrength -> binding uiPlateBiasStrength (\value st -> st { uiPlateBiasStrength = clamp01 value })
  SliderPlateBiasCenter -> binding uiPlateBiasCenter (\value st -> st { uiPlateBiasCenter = clamp01 value })
  SliderPlateBiasEdge -> binding uiPlateBiasEdge (\value st -> st { uiPlateBiasEdge = clamp01 value })
  SliderPlateBiasNorth -> binding uiPlateBiasNorth (\value st -> st { uiPlateBiasNorth = clamp01 value })
  SliderPlateBiasSouth -> binding uiPlateBiasSouth (\value st -> st { uiPlateBiasSouth = clamp01 value })
  SliderTfcCliffSlope -> binding uiTfcCliffSlope (\value st -> st { uiTfcCliffSlope = clamp01 value })
  SliderTfcMountainSlope -> binding uiTfcMountainSlope (\value st -> st { uiTfcMountainSlope = clamp01 value })
  SliderTfcMountainRelief -> binding uiTfcMountainRelief (\value st -> st { uiTfcMountainRelief = clamp01 value })
  SliderTfcHillSlope -> binding uiTfcHillSlope (\value st -> st { uiTfcHillSlope = clamp01 value })
  SliderTfcRollingSlope -> binding uiTfcRollingSlope (\value st -> st { uiTfcRollingSlope = clamp01 value })
  SliderValleyCurvature -> binding uiValleyCurvature (\value st -> st { uiValleyCurvature = clamp01 value })
  SliderTfcElevGradient -> binding uiTfcElevGradient (\value st -> st { uiTfcElevGradient = clamp01 value })
  SliderTfcPlateauMaxRelief2Ring -> binding uiTfcPlateauMaxRelief2Ring (\value st -> st { uiTfcPlateauMaxRelief2Ring = clamp01 value })
  SliderTfcPlateauMaxMicroRelief -> binding uiTfcPlateauMaxMicroRelief (\value st -> st { uiTfcPlateauMaxMicroRelief = clamp01 value })
  SliderTfcRollingNearFactor -> binding uiTfcRollingNearFactor (\value st -> st { uiTfcRollingNearFactor = clamp01 value })
  SliderRockElevationThreshold -> binding uiRockElevationThreshold (\value st -> st { uiRockElevationThreshold = clamp01 value })
  SliderRockHardnessThreshold -> binding uiRockHardnessThreshold (\value st -> st { uiRockHardnessThreshold = clamp01 value })
  SliderRockHardnessSecondary -> binding uiRockHardnessSecondary (\value st -> st { uiRockHardnessSecondary = clamp01 value })
  SliderPlanetRadius -> binding uiPlanetRadius (\value st -> st { uiPlanetRadius = clamp01 value })
  SliderAxialTilt -> binding uiAxialTilt (\value st -> st { uiAxialTilt = clamp01 value })
  SliderInsolation -> binding uiInsolation (\value st -> st { uiInsolation = clamp01 value })
  SliderOccWarmScale -> binding uiOccWarmScale (\value st -> st { uiOccWarmScale = clamp01 value })
  SliderOccColdScale -> binding uiOccColdScale (\value st -> st { uiOccColdScale = clamp01 value })
  SliderOccLatPeakDeg -> binding uiOccLatPeakDeg (\value st -> st { uiOccLatPeakDeg = clamp01 value })
  SliderOccLatWidthDeg -> binding uiOccLatWidthDeg (\value st -> st { uiOccLatWidthDeg = clamp01 value })
  SliderWaterLevel -> binding uiWaterLevel (\value st -> st { uiWaterLevel = clamp01 value })
  SliderOrographicLift -> binding uiOrographicLift (\value st -> st { uiOrographicLift = clamp01 value })
  SliderRainShadowLoss -> binding uiRainShadowLoss (\value st -> st { uiRainShadowLoss = clamp01 value })
  SliderWindDiffuse -> binding uiWindDiffuse (\value st -> st { uiWindDiffuse = clamp01 value })
  SliderEquatorTemp -> binding uiEquatorTemp (\value st -> st { uiEquatorTemp = clamp01 value })
  SliderPoleTemp -> binding uiPoleTemp (\value st -> st { uiPoleTemp = clamp01 value })
  SliderLapseRate -> binding uiLapseRate (\value st -> st { uiLapseRate = clamp01 value })
  SliderWindIterations -> binding uiWindIterations (\value st -> st { uiWindIterations = clamp01 value })
  SliderMoistureIterations -> binding uiMoistureIterations (\value st -> st { uiMoistureIterations = clamp01 value })
  SliderSliceLatCenter -> binding uiSliceLatCenter (\value st -> st { uiSliceLatCenter = clamp01 value })
  SliderSliceLonCenter -> binding uiSliceLonCenter (\value st -> st { uiSliceLonCenter = clamp01 value })
  SliderLatitudeExponent -> binding uiLatitudeExponent (\value st -> st { uiLatitudeExponent = clamp01 value })
  SliderPlateHeightCooling -> binding uiPlateHeightCooling (\value st -> st { uiPlateHeightCooling = clamp01 value })
  SliderTempNoiseScale -> binding uiTempNoiseScale (\value st -> st { uiTempNoiseScale = clamp01 value })
  SliderOceanModeration -> binding uiOceanModeration (\value st -> st { uiOceanModeration = clamp01 value })
  SliderOceanModerateTemp -> binding uiOceanModerateTemp (\value st -> st { uiOceanModerateTemp = clamp01 value })
  SliderAlbedoSensitivity -> binding uiAlbedoSensitivity (\value st -> st { uiAlbedoSensitivity = clamp01 value })
  SliderAlbedoReference -> binding uiAlbedoReference (\value st -> st { uiAlbedoReference = clamp01 value })
  SliderMoistAdvect -> binding uiMoistAdvect (\value st -> st { uiMoistAdvect = clamp01 value })
  SliderMoistLocal -> binding uiMoistLocal (\value st -> st { uiMoistLocal = clamp01 value })
  SliderMoistWindEvapScale -> binding uiMoistWindEvapScale (\value st -> st { uiMoistWindEvapScale = clamp01 value })
  SliderMoistEvapNoiseScale -> binding uiMoistEvapNoiseScale (\value st -> st { uiMoistEvapNoiseScale = clamp01 value })
  SliderMoistBareEvapFrac -> binding uiMoistBareEvapFrac (\value st -> st { uiMoistBareEvapFrac = clamp01 value })
  SliderMoistVegTranspFrac -> binding uiMoistVegTranspFrac (\value st -> st { uiMoistVegTranspFrac = clamp01 value })
  SliderMoistWindETScale -> binding uiMoistWindETScale (\value st -> st { uiMoistWindETScale = clamp01 value })
  SliderMoistCondensationRate -> binding uiMoistCondensationRate (\value st -> st { uiMoistCondensationRate = clamp01 value })
  SliderMoistRecycleRate -> binding uiMoistRecycleRate (\value st -> st { uiMoistRecycleRate = clamp01 value })
  SliderMoistITCZStrength -> binding uiMoistITCZStrength (\value st -> st { uiMoistITCZStrength = clamp01 value })
  SliderMoistITCZWidth -> binding uiMoistITCZWidth (\value st -> st { uiMoistITCZWidth = clamp01 value })
  SliderOrographicScale -> binding uiOrographicScale (\value st -> st { uiOrographicScale = clamp01 value })
  SliderOrographicStep -> binding uiOrographicStep (\value st -> st { uiOrographicStep = clamp01 value })
  SliderCoastalIterations -> binding uiCoastalIterations (\value st -> st { uiCoastalIterations = clamp01 value })
  SliderCoastalDiffuse -> binding uiCoastalDiffuse (\value st -> st { uiCoastalDiffuse = clamp01 value })
  SliderCoastalMoistureBoost -> binding uiCoastalMoistureBoost (\value st -> st { uiCoastalMoistureBoost = clamp01 value })
  SliderWindBeltStrength -> binding uiWindBeltStrength (\value st -> st { uiWindBeltStrength = clamp01 value })
  SliderWindBeltHarmonics -> binding uiWindBeltHarmonics (\value st -> st { uiWindBeltHarmonics = clamp01 value })
  SliderWindBeltBase -> binding uiWindBeltBase (\value st -> st { uiWindBeltBase = clamp01 value })
  SliderWindBeltRange -> binding uiWindBeltRange (\value st -> st { uiWindBeltRange = clamp01 value })
  SliderWindBeltSpeedScale -> binding uiWindBeltSpeedScale (\value st -> st { uiWindBeltSpeedScale = clamp01 value })
  SliderPiedmontSmooth -> binding uiPiedmontSmooth (\value st -> st { uiPiedmontSmooth = clamp01 value })
  SliderPiedmontSlopeMin -> binding uiPiedmontSlopeMin (\value st -> st { uiPiedmontSlopeMin = clamp01 value })
  SliderPiedmontSlopeMax -> binding uiPiedmontSlopeMax (\value st -> st { uiPiedmontSlopeMax = clamp01 value })
  SliderWindCoriolisDeflection -> binding uiWindCoriolisDeflection (\value st -> st { uiWindCoriolisDeflection = clamp01 value })
  SliderMoistMinVegFloor -> binding uiMoistMinVegFloor (\value st -> st { uiMoistMinVegFloor = clamp01 value })
  SliderWeatherTick -> binding uiWeatherTick (\value st -> st { uiWeatherTick = clamp01 value })
  SliderWeatherPhase -> binding uiWeatherPhase (\value st -> st { uiWeatherPhase = clamp01 value })
  SliderWeatherAmplitude -> binding uiWeatherAmplitude (\value st -> st { uiWeatherAmplitude = clamp01 value })
  SliderSeasonCycleLength -> binding uiSeasonCycleLength (\value st -> st { uiSeasonCycleLength = clamp01 value })
  SliderJitterAmplitude -> binding uiJitterAmplitude (\value st -> st { uiJitterAmplitude = clamp01 value })
  SliderPressureBase -> binding uiPressureBase (\value st -> st { uiPressureBase = clamp01 value })
  SliderPressureTempScale -> binding uiPressureTempScale (\value st -> st { uiPressureTempScale = clamp01 value })
  SliderPressureCoriolisScale -> binding uiPressureCoriolisScale (\value st -> st { uiPressureCoriolisScale = clamp01 value })
  SliderSeasonalBase -> binding uiSeasonalBase (\value st -> st { uiSeasonalBase = clamp01 value })
  SliderSeasonalRange -> binding uiSeasonalRange (\value st -> st { uiSeasonalRange = clamp01 value })
  SliderHumidityNoiseScale -> binding uiHumidityNoiseScale (\value st -> st { uiHumidityNoiseScale = clamp01 value })
  SliderPrecipNoiseScale -> binding uiPrecipNoiseScale (\value st -> st { uiPrecipNoiseScale = clamp01 value })
  SliderWeatherITCZWidth -> binding uiWeatherITCZWidth (\value st -> st { uiWeatherITCZWidth = clamp01 value })
  SliderWeatherITCZPrecipBoost -> binding uiWeatherITCZPrecipBoost (\value st -> st { uiWeatherITCZPrecipBoost = clamp01 value })
  SliderPressureHumidityScale -> binding uiPressureHumidityScale (\value st -> st { uiPressureHumidityScale = clamp01 value })
  SliderPressureGradientWindScale -> binding uiPressureGradientWindScale (\value st -> st { uiPressureGradientWindScale = clamp01 value })
  SliderWindNoiseScale -> binding uiWindNoiseScale (\value st -> st { uiWindNoiseScale = clamp01 value })
  SliderITCZMigrationScale -> binding uiITCZMigrationScale (\value st -> st { uiITCZMigrationScale = clamp01 value })
  SliderCloudRHExponent -> binding uiCloudRHExponent (\value st -> st { uiCloudRHExponent = clamp01 value })
  SliderCloudAlbedoEffect -> binding uiCloudAlbedoEffect (\value st -> st { uiCloudAlbedoEffect = clamp01 value })
  SliderCloudPrecipBoost -> binding uiCloudPrecipBoost (\value st -> st { uiCloudPrecipBoost = clamp01 value })
  SliderVegBase -> binding uiVegBase (\value st -> st { uiVegBase = clamp01 value })
  SliderVegBoost -> binding uiVegBoost (\value st -> st { uiVegBoost = clamp01 value })
  SliderVegTempWeight -> binding uiVegTempWeight (\value st -> st { uiVegTempWeight = clamp01 value })
  SliderVegPrecipWeight -> binding uiVegPrecipWeight (\value st -> st { uiVegPrecipWeight = clamp01 value })
  SliderBtCoastalBand -> binding uiBtCoastalBand (\value st -> st { uiBtCoastalBand = clamp01 value })
  SliderBtSnowMaxTemp -> binding uiBtSnowMaxTemp (\value st -> st { uiBtSnowMaxTemp = clamp01 value })
  SliderBtAlpineMaxTemp -> binding uiBtAlpineMaxTemp (\value st -> st { uiBtAlpineMaxTemp = clamp01 value })
  SliderBtIceCapTemp -> binding uiBtIceCapTemp (\value st -> st { uiBtIceCapTemp = clamp01 value })
  SliderBtMontaneMaxTemp -> binding uiBtMontaneMaxTemp (\value st -> st { uiBtMontaneMaxTemp = clamp01 value })
  SliderBtMontanePrecip -> binding uiBtMontanePrecip (\value st -> st { uiBtMontanePrecip = clamp01 value })
  SliderBtCliffSlope -> binding uiBtCliffSlope (\value st -> st { uiBtCliffSlope = clamp01 value })
  SliderBtValleyMoisture -> binding uiBtValleyMoisture (\value st -> st { uiBtValleyMoisture = clamp01 value })
  SliderBtDepressionMoisture -> binding uiBtDepressionMoisture (\value st -> st { uiBtDepressionMoisture = clamp01 value })
  SliderBtPrecipWeight -> binding uiBtPrecipWeight (\value st -> st { uiBtPrecipWeight = clamp01 value })
  SliderVbcTempMin -> binding uiVbcTempMin (\value st -> st { uiVbcTempMin = clamp01 value })
  SliderVbcTempRange -> binding uiVbcTempRange (\value st -> st { uiVbcTempRange = clamp01 value })
  SliderVbcFertilityBoost -> binding uiVbcFertilityBoost (\value st -> st { uiVbcFertilityBoost = clamp01 value })
  SliderVbcAlbedoBase -> binding uiVbcAlbedoBase (\value st -> st { uiVbcAlbedoBase = clamp01 value })
  SliderVbcAlbedoBare -> binding uiVbcAlbedoBare (\value st -> st { uiVbcAlbedoBare = clamp01 value })
  SliderVbcAlbedoVeg -> binding uiVbcAlbedoVeg (\value st -> st { uiVbcAlbedoVeg = clamp01 value })
  SliderVbcOceanAlbedo -> binding uiVbcOceanAlbedo (\value st -> st { uiVbcOceanAlbedo = clamp01 value })
  SliderVbcIceAlbedo -> binding uiVbcIceAlbedo (\value st -> st { uiVbcIceAlbedo = clamp01 value })
  SliderBiomeSmoothing -> binding uiBiomeSmoothing (\value st -> st { uiBiomeSmoothing = clamp01 value })
  SliderVolcanicAshBoost -> binding uiVolcanicAshBoost (\value st -> st { uiVolcanicAshBoost = clamp01 value })
  SliderVolcanicLavaPenalty -> binding uiVolcanicLavaPenalty (\value st -> st { uiVolcanicLavaPenalty = clamp01 value })
  SliderBiomeFeedbackBlend -> binding uiBiomeFeedbackBlend (\value st -> st { uiBiomeFeedbackBlend = clamp01 value })
  SliderErosionHydraulic -> binding uiErosionHydraulic (\value st -> st { uiErosionHydraulic = clamp01 value })
  SliderErosionThermal -> binding uiErosionThermal (\value st -> st { uiErosionThermal = clamp01 value })
  SliderErosionRainRate -> binding uiRainRate (\value st -> st { uiRainRate = clamp01 value })
  SliderErosionTalus -> binding uiErosionTalus (\value st -> st { uiErosionTalus = clamp01 value })
  SliderErosionMaxDrop -> binding uiErosionMaxDrop (\value st -> st { uiErosionMaxDrop = clamp01 value })
  SliderErosionHydDeposit -> binding uiErosionHydDeposit (\value st -> st { uiErosionHydDeposit = clamp01 value })
  SliderErosionDepositSlope -> binding uiErosionDepositSlope (\value st -> st { uiErosionDepositSlope = clamp01 value })
  SliderErosionThermDeposit -> binding uiErosionThermDeposit (\value st -> st { uiErosionThermDeposit = clamp01 value })
  SliderErosionCoastZone -> binding uiErosionCoastZone (\value st -> st { uiErosionCoastZone = clamp01 value })
  SliderErosionCoastStrength -> binding uiErosionCoastStrength (\value st -> st { uiErosionCoastStrength = clamp01 value })
  SliderErosionCoastIter -> binding uiErosionCoastIter (\value st -> st { uiErosionCoastIter = clamp01 value })
  SliderHypsometryEnabled -> binding uiHypsometryEnabled (\value st -> st { uiHypsometryEnabled = clamp01 value })
  SliderHypsometryLowlandExp -> binding uiHypsometryLowlandExp (\value st -> st { uiHypsometryLowlandExp = clamp01 value })
  SliderHypsometryHighlandExp -> binding uiHypsometryHighlandExp (\value st -> st { uiHypsometryHighlandExp = clamp01 value })
  SliderHypsometryPlateauBreak -> binding uiHypsometryPlateauBreak (\value st -> st { uiHypsometryPlateauBreak = clamp01 value })
  SliderHypsometryOceanExp -> binding uiHypsometryOceanExp (\value st -> st { uiHypsometryOceanExp = clamp01 value })
  SliderHypsometryCoastalRampWidth -> binding uiHypsometryCoastalRampWidth (\value st -> st { uiHypsometryCoastalRampWidth = clamp01 value })
  SliderHypsometryCoastalRampStr -> binding uiHypsometryCoastalRampStr (\value st -> st { uiHypsometryCoastalRampStr = clamp01 value })
  SliderGlacierSnowTemp -> binding uiGlacierSnowTemp (\value st -> st { uiGlacierSnowTemp = clamp01 value })
  SliderGlacierSnowRange -> binding uiGlacierSnowRange (\value st -> st { uiGlacierSnowRange = clamp01 value })
  SliderGlacierMeltTemp -> binding uiGlacierMeltTemp (\value st -> st { uiGlacierMeltTemp = clamp01 value })
  SliderGlacierMeltRate -> binding uiGlacierMeltRate (\value st -> st { uiGlacierMeltRate = clamp01 value })
  SliderGlacierAccumScale -> binding uiGlacierAccumScale (\value st -> st { uiGlacierAccumScale = clamp01 value })
  SliderGlacierFlowIters -> binding uiGlacierFlowIters (\value st -> st { uiGlacierFlowIters = clamp01 value })
  SliderGlacierFlowRate -> binding uiGlacierFlowRate (\value st -> st { uiGlacierFlowRate = clamp01 value })
  SliderGlacierErosionScale -> binding uiGlacierErosionScale (\value st -> st { uiGlacierErosionScale = clamp01 value })
  SliderGlacierCarveScale -> binding uiGlacierCarveScale (\value st -> st { uiGlacierCarveScale = clamp01 value })
  SliderGlacierDepositScale -> binding uiGlacierDepositScale (\value st -> st { uiGlacierDepositScale = clamp01 value })
  SliderVentDensity -> binding uiVentDensity (\value st -> st { uiVentDensity = clamp01 value })
  SliderVentThreshold -> binding uiVentThreshold (\value st -> st { uiVentThreshold = clamp01 value })
  SliderHotspotScale -> binding uiHotspotScale (\value st -> st { uiHotspotScale = clamp01 value })
  SliderHotspotThreshold -> binding uiHotspotThreshold (\value st -> st { uiHotspotThreshold = clamp01 value })
  SliderMagmaRecharge -> binding uiMagmaRecharge (\value st -> st { uiMagmaRecharge = clamp01 value })
  SliderLavaScale -> binding uiLavaScale (\value st -> st { uiLavaScale = clamp01 value })
  SliderAshScale -> binding uiAshScale (\value st -> st { uiAshScale = clamp01 value })
  SliderVolcanicDepositScale -> binding uiVolcanicDepositScale (\value st -> st { uiVolcanicDepositScale = clamp01 value })
  SliderSoilMoistureThreshold -> binding uiSoilMoistureThreshold (\value st -> st { uiSoilMoistureThreshold = clamp01 value })
  SliderSoilHardnessThreshold -> binding uiSoilHardnessThreshold (\value st -> st { uiSoilHardnessThreshold = clamp01 value })
  SliderSoilFertilityMoistWeight -> binding uiSoilFertilityMoistWeight (\value st -> st { uiSoilFertilityMoistWeight = clamp01 value })
  SliderSoilFertilityDepthWeight -> binding uiSoilFertilityDepthWeight (\value st -> st { uiSoilFertilityDepthWeight = clamp01 value })
  SliderSinkBreachDepth -> binding uiSinkBreachDepth (\value st -> st { uiSinkBreachDepth = clamp01 value })
  SliderStreamPowerMaxErosion -> binding uiStreamPowerMaxErosion (\value st -> st { uiStreamPowerMaxErosion = clamp01 value })
  SliderRiverCarveMaxDepth -> binding uiRiverCarveMaxDepth (\value st -> st { uiRiverCarveMaxDepth = clamp01 value })
  SliderCoastalErodeStrength -> binding uiCoastalErodeStrength (\value st -> st { uiCoastalErodeStrength = clamp01 value })
  SliderHydroHardnessWeight -> binding uiHydroHardnessWeight (\value st -> st { uiHydroHardnessWeight = clamp01 value })
  SliderMinLakeSize -> binding uiMinLakeSize (\value st -> st { uiMinLakeSize = clamp01 value })
  SliderInlandSeaMinSize -> binding uiInlandSeaMinSize (\value st -> st { uiInlandSeaMinSize = clamp01 value })
  SliderRoughnessScale -> binding uiRoughnessScale (\value st -> st { uiRoughnessScale = clamp01 value })
  SliderHexSizeKm -> binding uiHexSizeKm (\value st -> st { uiHexSizeKm = clamp01 value })

binding :: (UiState -> Float) -> (Float -> UiState -> UiState) -> SliderStateBinding
binding = SliderStateBinding

sliderValueForId :: UiState -> SliderId -> Float
sliderValueForId ui sliderIdValue = sliderStateGet (sliderStateBindingForId sliderIdValue) ui

applySliderValue :: SliderId -> Float -> UiState -> UiState
applySliderValue sliderIdValue value st =
  sliderStatePut (sliderStateBindingForId sliderIdValue) value st

-- | Shared reference for lock-free UI snapshot reads by the render loop.
type UiSnapshotRef = IORef UiState

-- | Internal actor state wrapping 'UiState' with an optional self-publishing
-- IORef.  The hyperspace actor uses this as its state type so that every
-- 'UiUpdate' mutation triggers a write to the shared IORef.
data UiActorState = UiActorState
  { uasUi :: !UiState
  , uasSnapshotRef :: !(Maybe UiSnapshotRef)
  , uasNextDataBrowserRequestId :: !Word64
  , uasNextOverlayInspectorRequestId :: !Word64
  }

emptyUiActorState :: UiActorState
emptyUiActorState = UiActorState
  { uasUi = emptyUiState
  , uasSnapshotRef = Nothing
  , uasNextDataBrowserRequestId = 1
  , uasNextOverlayInspectorRequestId = 1
  }

-- | Publish the current UI snapshot to the shared 'IORef', if registered.
publishUiSnapshot :: UiActorState -> IO ()
publishUiSnapshot st =
  case uasSnapshotRef st of
    Nothing -> pure ()
    Just ref -> writeIORef ref (uasUi st)

dataBrowserUiFromUiState :: UiState -> DataBrowserUi
dataBrowserUiFromUiState ui = DataBrowserUi resources model
  where
    resources = uiDataResources ui
    state = uiDataBrowser ui
    selection = DataBrowserSelection
      { dbSelectionPlugin = dbsSelectedPlugin state
      , dbSelectionResource = dbsSelectedResource state
      , dbSelectionSchema = schema
      , dbSelectionRecord = dbsSelectedRecord state
      , dbSelectionRecordKey = dbsSelectedRecordKey state
      , dbSelectionRowIndex = dbsSelectedRowIndex state
      }
    schema = do
      pluginName <- dbsSelectedPlugin state
      resourceName <- dbsSelectedResource state
      schemas <- Map.lookup pluginName resources
      find ((== resourceName) . drsName) schemas
    mode
      | dbsDeleteConfirm state = DataBrowserDeleteConfirmMode
      | dbsCreateMode state = DataBrowserCreateMode
      | dbsEditMode state = DataBrowserEditMode
      | dbsSelectedRecord state /= Nothing = DataBrowserViewMode
      | otherwise = DataBrowserBrowseMode
    model = DataBrowserModel
      { dbModelMode = mode
      , dbModelSelection = selection
      , dbModelRecords = dbsRecords state
      , dbModelPagination = DataBrowserPagination
          { dbPaginationOffset = dbsPageOffset state
          , dbPaginationPageSize = schema >>= fst . dataBrowserPageRequestFor
          , dbPaginationTotalCount = dbsTotalCount state
          }
      , dbModelExpandedFields = dbsExpandedFields state
      , dbModelEditBuffer = DataBrowserEditBuffer (dbsEditValues state)
      , dbModelFocusedField = dbsFocusedField state
      , dbModelTextCursor = dbsTextCursor state
      , dbModelValidationErrors = dbsValidationErrors state
      , dbModelPendingRequest = dbsPendingRequest state >>= dataBrowserPendingDescriptor . dbpeRequest
      , dbModelPendingEnvelope = dbsPendingRequest state
      , dbModelAsyncError = dbsAsyncError state
      , dbModelLoading = maybe False (const True) (dbsPendingRequest state)
      }

dataBrowserUiIntoUiState :: DataBrowserUi -> UiState -> UiState
dataBrowserUiIntoUiState browserUi ui = ui
  { uiDataResources = dbuResources browserUi
  , uiDataBrowser = state
  }
  where
    model = dbuModel browserUi
    selection = dbModelSelection model
    state = emptyDataBrowserState
      { dbsSelectedPlugin = dbSelectionPlugin selection
      , dbsSelectedResource = dbSelectionResource selection
      , dbsRecords = dbModelRecords model
      , dbsPageOffset = dbPaginationOffset (dbModelPagination model)
      , dbsTotalCount = dbPaginationTotalCount (dbModelPagination model)
      , dbsLoading = maybe False (const True) (dbModelPendingEnvelope model)
      , dbsSelectedRecord = dbSelectionRecord selection
      , dbsSelectedRecordKey = dbSelectionRecordKey selection
      , dbsSelectedRowIndex = dbSelectionRowIndex selection
      , dbsExpandedFields = dbModelExpandedFields model
      , dbsEditMode = dbModelMode model == DataBrowserEditMode
      , dbsCreateMode = dbModelMode model == DataBrowserCreateMode
      , dbsEditValues = dbEditBufferValues (dbModelEditBuffer model)
      , dbsFocusedField = dbModelFocusedField model
      , dbsTextCursor = dbModelTextCursor model
      , dbsDeleteConfirm = dbModelMode model == DataBrowserDeleteConfirmMode
      , dbsValidationErrors = dbModelValidationErrors model
      , dbsPendingRequest = dbModelPendingEnvelope model
      , dbsAsyncError = dbModelAsyncError model
      }

uiSnapshotTag :: OpTag "uiSnapshot"
uiSnapshotTag = OpTag

[hyperspace|
replyprotocol UiSnapshotReply =
  cast uiSnapshot :: UiState

actor Ui
  state UiActorState
  lifetime Singleton
  schedule pinned 1
  noDeps
  mailbox Unbounded

  cast update :: UiUpdate
  cast snapshotAsync :: () reply UiSnapshotReply
  cast setSnapshotRef :: UiSnapshotRef
  call snapshot :: () -> UiState
  call dataBrowserBegin :: DataBrowserAppAction -> DataBrowserBeginResult
  call dataBrowserComplete :: DataBrowserCompletion -> Bool
  call overlayInspectorBegin :: OverlayInspectorAction -> OverlayInspectorBeginResult
  call overlayInspectorComplete :: OverlayInspectorCompletion -> Bool

  initial emptyUiActorState
  on_ update = \upd st -> do
    let st' = st { uasUi = applyUpdate upd (uasUi st) }
    publishUiSnapshot st'
    pure st'
  onReply snapshotAsync = \() replyTo st -> do
    replyCast replyTo uiSnapshotTag (uasUi st)
    pure st
  on_ setSnapshotRef = \ref st -> do
    let st' = st { uasSnapshotRef = Just ref }
    publishUiSnapshot st'
    pure st'
  onPure snapshot = \() st -> (st, uasUi st)
  on dataBrowserBegin = \action st -> do
    let requestId = DataBrowserRequestId (uasNextDataBrowserRequestId st)
        current = dataBrowserUiFromUiState (uasUi st)
        (next, result) = beginDataBrowserAction requestId current action
        ui' = dataBrowserUiIntoUiState next (uasUi st)
        consumed = case result of
          DataBrowserBeginAccepted {} -> True
          _ -> False
        st' = st
          { uasUi = ui'
          , uasNextDataBrowserRequestId =
              if consumed then uasNextDataBrowserRequestId st + 1 else uasNextDataBrowserRequestId st
          }
    when (ui' /= uasUi st) (publishUiSnapshot st')
    pure (st', result)
  on dataBrowserComplete = \completion st -> do
    let current = dataBrowserUiFromUiState (uasUi st)
        (next, applied) = completeDataBrowserRequest completion current
        ui' = dataBrowserUiIntoUiState next (uasUi st)
        st' = st { uasUi = ui' }
    when applied (publishUiSnapshot st')
    pure (st', applied)
  on overlayInspectorBegin = \action st -> do
    let requestId = OverlayInspectorRequestId (uasNextOverlayInspectorRequestId st)
        (next, result) = beginOverlayInspectorAction requestId action (uiOverlayInspector (uasUi st))
        consumed = case result of
          OverlayInspectorBeginAccepted {} -> True
          _ -> False
        ui' = (uasUi st) { uiOverlayInspector = next }
        st' = st
          { uasUi = ui'
          , uasNextOverlayInspectorRequestId =
              if consumed then uasNextOverlayInspectorRequestId st + 1 else uasNextOverlayInspectorRequestId st
          }
    when (ui' /= uasUi st) (publishUiSnapshot st')
    pure (st', result)
  on overlayInspectorComplete = \completion st -> do
    let (next, applied) = completeOverlayInspectorRequest completion (uiOverlayInspector (uasUi st))
        ui' = (uasUi st) { uiOverlayInspector = next }
        st' = st { uasUi = ui' }
    when applied (publishUiSnapshot st')
    pure (st', applied)
|]

getUiSnapshot :: ActorHandle Ui (Protocol Ui) -> IO UiState
getUiSnapshot handle =
  call @"snapshot" handle #snapshot ()

beginUiDataBrowserAction
  :: ActorHandle Ui (Protocol Ui)
  -> DataBrowserAppAction
  -> IO DataBrowserBeginResult
beginUiDataBrowserAction handle action =
  call @"dataBrowserBegin" handle #dataBrowserBegin action

completeUiDataBrowserRequest
  :: ActorHandle Ui (Protocol Ui)
  -> DataBrowserCompletion
  -> IO Bool
completeUiDataBrowserRequest handle completion =
  call @"dataBrowserComplete" handle #dataBrowserComplete completion

beginUiOverlayInspectorAction
  :: ActorHandle Ui (Protocol Ui)
  -> OverlayInspectorAction
  -> IO OverlayInspectorBeginResult
beginUiOverlayInspectorAction handle action =
  call @"overlayInspectorBegin" handle #overlayInspectorBegin action

completeUiOverlayInspectorRequest
  :: ActorHandle Ui (Protocol Ui)
  -> OverlayInspectorCompletion
  -> IO Bool
completeUiOverlayInspectorRequest handle completion =
  call @"overlayInspectorComplete" handle #overlayInspectorComplete completion

requestUiSnapshot :: ActorHandle Ui (Protocol Ui) -> ReplyTo UiSnapshotReply -> IO ()
requestUiSnapshot handle replyTo =
  castReply @"snapshotAsync" handle replyTo #snapshotAsync ()

-- | Register a shared 'IORef' for self-publishing UI snapshots.
--
-- Once registered, the Ui actor writes its current 'UiState' to this ref
-- after every state change, enabling the render thread to read the latest
-- snapshot without blocking on the actor's mailbox.
setUiSnapshotRef :: ActorHandle Ui (Protocol Ui) -> UiSnapshotRef -> IO ()
setUiSnapshotRef handle ref =
  cast @"setSnapshotRef" handle #setSnapshotRef ref

-- | Read the latest UI snapshot from the shared 'IORef'.
readUiSnapshotRef :: UiSnapshotRef -> IO UiState
readUiSnapshotRef = readIORef

-- | Create a new 'UiSnapshotRef' with an empty initial snapshot.
newUiSnapshotRef :: IO UiSnapshotRef
newUiSnapshotRef = newIORef emptyUiState

clampChunk :: Int -> Int
clampChunk size =
  let minSize = 8
      maxSize = 256
  in max minSize (min maxSize size)

clamp01 :: Float -> Float
clamp01 value =
  max 0 (min 1 value)

clampZoom :: Float -> Float
clampZoom value =
  max 0.4 (min maxCameraZoom value)
