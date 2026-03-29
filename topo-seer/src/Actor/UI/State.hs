{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
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
  , dataBrowserRowCount
  , pluginRowIndex
  , pluginRowsWithParams
  , builtinStageRowCount
  , LeftTab(..)
  , sliderValueForId
  , UiMenuMode(..)
  , ViewMode(..)
  , UiState(..)
  , emptyUiState
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
  ) where

import Data.Aeson (Value)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import Hyperspace.Actor
import Hyperspace.Actor.QQ (hyperspace)
import Hyperspace.Actor.Spec (OpTag(..))
import Seer.Config.SliderRegistry (SliderId(..), SliderTab(..), sliderDefaultValueForId, sliderRowCountForTab)
import Seer.Config.Snapshot.Types (ConfigSnapshot)
import Seer.World.Persist.Types (WorldSaveManifest)
import Topo.Overlay.Schema (OverlayFieldType(..))
import Topo.Pipeline.Stage (StageId)
import Topo.Plugin.DataResource (DataResourceSchema)
import Topo.Plugin.RPC.DataService (DataRecord)
import Topo.Plugin.RPC.Manifest (RPCParamSpec(..), RPCParamType(..))
import UI.WidgetTree (WidgetId)

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
  | ViewOverlay !Text !Int
  deriving (Eq, Show)

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
  }

-- | Number of rows to display in the data browser tab.
--   Header row + one row per plugin with data resources,
--   plus record rows when a resource is selected.
dataBrowserRowCount :: DataBrowserState -> Map Text [DataResourceSchema] -> Int
dataBrowserRowCount dbs resources =
  let pluginCount = Map.size resources
      resourceCount = case dbsSelectedPlugin dbs of
        Nothing   -> 0
        Just pName -> length (Map.findWithDefault [] pName resources)
      recordCount = length (dbsRecords dbs)
  in max 1 (pluginCount + resourceCount + recordCount)

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
  sum [ 1 + expandedParamCount name | name <- uiPluginNames ui ]
  where
    expandedParamCount name
      | Map.findWithDefault False name (uiPluginExpanded ui) =
          length (Map.findWithDefault [] name (uiPluginParamSpecs ui))
      | otherwise = 0

-- | Compute the absolute row index for the i-th plugin in the pipeline tab,
-- accounting for expanded parameter rows of preceding plugins.
pluginRowIndex :: UiState -> Int -> Int
pluginRowIndex ui i =
  builtinStageRowCount + sum
    [ 1 + expandedParamCount name
    | (j, name) <- zip [0..] (uiPluginNames ui)
    , j < i
    ]
  where
    expandedParamCount name
      | Map.findWithDefault False name (uiPluginExpanded ui) =
          length (Map.findWithDefault [] name (uiPluginParamSpecs ui))
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
  deriving (Eq, Show)

data UiState = UiState
  { uiSeed :: !Word64
  , uiGenerating :: !Bool
  , uiViewMode :: !ViewMode
  , uiChunkSize :: !Int
  , uiShowConfig :: !Bool
  , uiShowLeftPanel :: !Bool
  , uiConfigTab :: !ConfigTab
  , uiConfigScroll :: !Int
  , uiLeftTab :: !LeftTab
  , uiMenuMode :: !UiMenuMode
  , uiPresetInput :: !Text
  , uiPresetList :: ![Text]
  , uiPresetSelected :: !Int
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
  , uiBndLandRange :: !Float
  , uiBndTempConvergent :: !Float
  , uiBndTempDivergent :: !Float
  , uiBndTempTransform :: !Float
  , uiBndPrecipConvergent :: !Float
  , uiBndPrecipDivergent :: !Float
  , uiBndPrecipTransform :: !Float
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
  , uiDetailScale :: !Float
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
  , uiBoundaryMotionTemp :: !Float
  , uiBoundaryMotionPrecip :: !Float
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
  , uiDisabledStages :: !(Set StageId)
  , uiDisabledPlugins :: !(Set Text)
  , uiPluginParams :: !(Map Text (Map Text Value))
  , uiPluginNames :: ![Text]
  , uiPluginExpanded :: !(Map Text Bool)
  , uiPluginParamSpecs :: !(Map Text [RPCParamSpec])
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
  , uiOverlayNames :: ![Text]
  , uiOverlayFields :: ![(Text, OverlayFieldType)]
  , uiDataBrowser :: !DataBrowserState
  , uiDataResources :: !(Map Text [DataResourceSchema])
  } deriving (Eq, Show)

emptyUiState :: UiState
emptyUiState = UiState
  { uiSeed = 0
  , uiGenerating = False
  , uiViewMode = ViewElevation
  , uiChunkSize = 64
  , uiShowConfig = False
  , uiShowLeftPanel = True
  , uiConfigTab = ConfigTerrain
  , uiConfigScroll = 0
  , uiLeftTab = LeftTopo
  , uiMenuMode = MenuNone
  , uiPresetInput = Text.empty
  , uiPresetList = []
  , uiPresetSelected = 0
  , uiContextHex = Nothing
  , uiContextPos = Nothing
  , uiHexTooltipPinned = False
  , uiSeedEditing = False
  , uiSeedInput = Text.empty
  , uiWaterLevel = sliderDefault SliderWaterLevel
  , uiRenderWaterLevel = 0.5
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
  , uiBndLandRange = sliderDefault SliderBndLandRange
  , uiBndTempConvergent = 0.467
  , uiBndTempDivergent = 0.4
  , uiBndTempTransform = 0.45
  , uiBndPrecipConvergent = 0.6
  , uiBndPrecipDivergent = 0.5
  , uiBndPrecipTransform = 0.6
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
  , uiEdgeDepthNorth = 0
  , uiEdgeDepthSouth = 0
  , uiEdgeDepthEast = 0
  , uiEdgeDepthWest = 0
  , uiEdgeDepthFalloff = 0
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
  , uiDetailScale = sliderDefault SliderDetailScale
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
  , uiBoundaryMotionTemp = sliderDefault SliderBoundaryMotionTemp
  , uiBoundaryMotionPrecip = sliderDefault SliderBoundaryMotionPrecip
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
  , uiDisabledStages = Set.empty
  , uiDisabledPlugins = Set.empty
  , uiPluginParams = Map.empty
  , uiPluginNames = []
  , uiPluginExpanded = Map.empty
  , uiPluginParamSpecs = Map.empty
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
  , uiOverlayNames = []
  , uiOverlayFields = []
  , uiDataBrowser = emptyDataBrowserState
  , uiDataResources = Map.empty
  }

sliderDefault :: SliderId -> Float
sliderDefault = sliderDefaultValueForId

data UiUpdate
  = SetSeed !Word64
  | SetGenerating !Bool
  | SetViewMode !ViewMode
  | SetChunkSize !Int
  | SetShowConfig !Bool
  | SetShowLeftPanel !Bool
  | SetConfigTab !ConfigTab
  | SetConfigScroll !Int
  | SetLeftTab !LeftTab
  | SetMenuMode !UiMenuMode
  | SetPresetInput !Text
  | SetPresetList ![Text]
  | SetPresetSelected !Int
  | SetContextHex !(Maybe (Int, Int))
  | SetContextPos !(Maybe (Int, Int))
  | SetHexTooltipPinned !Bool
  | SetSeedEditing !Bool
  | SetSeedInput !Text
  | SetSliderValue !SliderId !Float
  | SetRenderWaterLevel !Float
  | SetBndTempConvergent !Float
  | SetBndTempDivergent !Float
  | SetBndTempTransform !Float
  | SetBndPrecipConvergent !Float
  | SetBndPrecipDivergent !Float
  | SetBndPrecipTransform !Float
  | SetPanOffset !(Float, Float)
  | SetZoom !Float
  | SetHoverHex !(Maybe (Int, Int))
  | SetHoverWidget !(Maybe WidgetId)
  | SetDisabledStages !(Set StageId)
  | SetDisabledPlugins !(Set Text)
  | SetPluginParam !Text !Text !Value
  | SetPluginNames ![Text]
  | SetPluginExpanded !Text !Bool
  | SetPluginParamSpecs !(Map Text [RPCParamSpec])
  | SetSimAutoTick !Bool
  | SetSimTickRate !Float
  | SetSimTickCount !Word64
  | SetWorldConfig !(Maybe ConfigSnapshot)
  | SetWorldName !Text
  | SetWorldSaveInput !Text
  | SetWorldList ![WorldSaveManifest]
  | SetWorldSelected !Int
  | SetOverlayNames ![Text]
  | SetOverlayFields ![(Text, OverlayFieldType)]
  | SetDataBrowser !DataBrowserState
  | SetDataResources !(Map Text [DataResourceSchema])

applyUpdate :: UiUpdate -> UiState -> UiState
applyUpdate upd st = case upd of
  SetSeed v -> st { uiSeed = v }
  SetGenerating v -> st { uiGenerating = v }
  SetViewMode v -> st { uiViewMode = v }
  SetChunkSize v -> st { uiChunkSize = clampChunk v }
  SetShowConfig v -> st { uiShowConfig = v }
  SetShowLeftPanel v -> st { uiShowLeftPanel = v }
  SetConfigTab v -> st { uiConfigTab = v }
  SetConfigScroll v -> st { uiConfigScroll = max 0 v }
  SetLeftTab v -> st { uiLeftTab = v }
  SetMenuMode v -> st { uiMenuMode = v }
  SetPresetInput v -> st { uiPresetInput = v }
  SetPresetList v -> st { uiPresetList = v }
  SetPresetSelected v -> st { uiPresetSelected = max 0 v }
  SetContextHex v -> st { uiContextHex = v }
  SetContextPos v -> st { uiContextPos = v }
  SetHexTooltipPinned v -> st { uiHexTooltipPinned = v }
  SetSeedEditing v -> st { uiSeedEditing = v }
  SetSeedInput v -> st { uiSeedInput = v }
  SetSliderValue sliderIdValue v -> applySliderValue sliderIdValue v st
  SetRenderWaterLevel v -> st { uiRenderWaterLevel = clamp01 v }
  SetBndTempConvergent v -> st { uiBndTempConvergent = clamp01 v }
  SetBndTempDivergent v -> st { uiBndTempDivergent = clamp01 v }
  SetBndTempTransform v -> st { uiBndTempTransform = clamp01 v }
  SetBndPrecipConvergent v -> st { uiBndPrecipConvergent = clamp01 v }
  SetBndPrecipDivergent v -> st { uiBndPrecipDivergent = clamp01 v }
  SetBndPrecipTransform v -> st { uiBndPrecipTransform = clamp01 v }
  SetPanOffset v -> st { uiPanOffset = v }
  SetZoom v -> st { uiZoom = clampZoom v }
  SetHoverHex v -> st { uiHoverHex = v }
  SetHoverWidget v -> st { uiHoverWidget = v }
  SetDisabledStages v -> st { uiDisabledStages = v }
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
  SetSimAutoTick v -> st { uiSimAutoTick = v }
  SetSimTickRate v -> st { uiSimTickRate = clamp01 v }
  SetSimTickCount v -> st { uiSimTickCount = v }
  SetWorldConfig v -> st { uiWorldConfig = v }
  SetWorldName v -> st { uiWorldName = v }
  SetWorldSaveInput v -> st { uiWorldSaveInput = v }
  SetWorldList v -> st { uiWorldList = v }
  SetWorldSelected v -> st { uiWorldSelected = v }
  SetOverlayNames v -> st { uiOverlayNames = v }
  SetOverlayFields v -> st { uiOverlayFields = v }
  SetDataBrowser v -> st { uiDataBrowser = v }
  SetDataResources v -> st { uiDataResources = v }

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
  SliderDetailScale -> binding uiDetailScale (\value st -> st { uiDetailScale = clamp01 value })
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
  SliderBoundaryMotionTemp -> binding uiBoundaryMotionTemp (\value st -> st { uiBoundaryMotionTemp = clamp01 value })
  SliderBoundaryMotionPrecip -> binding uiBoundaryMotionPrecip (\value st -> st { uiBoundaryMotionPrecip = clamp01 value })
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
  SliderBndLandRange -> binding uiBndLandRange (\value st -> st { uiBndLandRange = clamp01 value })
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
  }

emptyUiActorState :: UiActorState
emptyUiActorState = UiActorState emptyUiState Nothing

-- | Publish the current UI snapshot to the shared 'IORef', if registered.
publishUiSnapshot :: UiActorState -> IO ()
publishUiSnapshot st =
  case uasSnapshotRef st of
    Nothing -> pure ()
    Just ref -> writeIORef ref (uasUi st)

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
|]

getUiSnapshot :: ActorHandle Ui (Protocol Ui) -> IO UiState
getUiSnapshot handle =
  call @"snapshot" handle #snapshot ()

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
  max 0.4 (min 3 value)
