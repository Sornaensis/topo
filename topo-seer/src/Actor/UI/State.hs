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
  , LeftTab(..)
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
  ) where

import Data.Aeson (Value)
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
import Seer.Config.Snapshot.Types (ConfigSnapshot)
import Seer.World.Persist.Types (WorldSaveManifest)
import Topo.Overlay.Schema (OverlayFieldType(..))
import Topo.Pipeline.Stage (StageId)
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
  deriving (Eq, Show)

-- | Total number of config widget rows for each tab.
configRowCount :: ConfigTab -> UiState -> Int
configRowCount ConfigTerrain _ = 53
configRowCount ConfigPlanet _ = 7
configRowCount ConfigClimate _ = 53
configRowCount ConfigWeather _ = 21
configRowCount ConfigBiome _ = 26
configRowCount ConfigErosion _ = 48
configRowCount ConfigPipeline ui =
  builtinStageRowCount + length (uiPluginNames ui) + simControlRowCount

builtinStageRowCount :: Int
builtinStageRowCount = 18

simControlRowCount :: Int
simControlRowCount = 3

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
  , uiSliceLatCenter :: !Float
  , uiSliceLonCenter :: !Float
  , uiDisabledStages :: !(Set StageId)
  , uiPluginParams :: !(Map Text (Map Text Value))
  , uiPluginNames :: ![Text]
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
  , uiWaterLevel = 0.5
  , uiRenderWaterLevel = 0.5
  , uiOrographicLift = 0.35
  , uiRainShadowLoss = 0.08
  , uiWindDiffuse = 0.5
  , uiRainRate = 0.2
  , uiErosionHydraulic = 0.5
  , uiErosionThermal = 0.4
  , uiErosionTalus = 0.5
  , uiErosionMaxDrop = 0.5
  , uiErosionHydDeposit = 0.375
  , uiErosionDepositSlope = 0.357
  , uiErosionThermDeposit = 0.5
  , uiErosionCoastZone = 0.286
  , uiErosionCoastStrength = 0.375
  , uiErosionCoastIter = 0.429
  , uiHypsometryEnabled = 1.0
  , uiHypsometryLowlandExp = 0.200
  , uiHypsometryHighlandExp = 0.333
  , uiHypsometryPlateauBreak = 0.348
  , uiHypsometryOceanExp = 0.500
  , uiHypsometryCoastalRampWidth = 0.467
  , uiHypsometryCoastalRampStr = 0.600
  , uiGlacierSnowTemp = 0.5
  , uiGlacierSnowRange = 0.417
  , uiGlacierMeltTemp = 0.429
  , uiGlacierMeltRate = 0.2
  , uiGlacierAccumScale = 0.333
  , uiGlacierFlowIters = 0.3
  , uiGlacierFlowRate = 0.2
  , uiGlacierErosionScale = 0.25
  , uiGlacierCarveScale = 0.1
  , uiGlacierDepositScale = 0.2
  , uiVentDensity = 0.25
  , uiVentThreshold = 0.5
  , uiHotspotScale = 0.5
  , uiHotspotThreshold = 0.615
  , uiMagmaRecharge = 0.333
  , uiLavaScale = 0.6
  , uiAshScale = 0.4
  , uiVolcanicDepositScale = 0.8
  , uiSoilMoistureThreshold = 0.7
  , uiSoilHardnessThreshold = 0.45
  , uiSoilFertilityMoistWeight = 0.6
  , uiSoilFertilityDepthWeight = 0.4
  , uiSinkBreachDepth = 0.2
  , uiStreamPowerMaxErosion = 0.25
  , uiRiverCarveMaxDepth = 0.25
  , uiCoastalErodeStrength = 0.2
  , uiHydroHardnessWeight = 0.7
  , uiPiedmontSmooth = 0.4167
  , uiPiedmontSlopeMin = 0.2857
  , uiPiedmontSlopeMax = 0.35
  , uiMinLakeSize = 0.061
  , uiInlandSeaMinSize = 0.333
  , uiRoughnessScale = 0.375
  , uiEquatorTemp = 0.78
  , uiPoleTemp = 0
  , uiLapseRate = 0.65
  , uiLatitudeExponent = 0.615
  , uiPlateHeightCooling = 0.25
  , uiTempNoiseScale = 0.33
  , uiOceanModeration = 0.3
  , uiOceanModerateTemp = 0.5
  , uiAlbedoSensitivity = 0.2
  , uiAlbedoReference = 0.6
  , uiMoistAdvect = 0.85
  , uiMoistLocal = 0.15
  , uiMoistWindEvapScale = 0.3
  , uiMoistEvapNoiseScale = 0.25
  , uiMoistBareEvapFrac = 0.15
  , uiMoistVegTranspFrac = 0.85
  , uiMoistWindETScale = 0.2
  , uiMoistCondensationRate = 0.2
  , uiMoistRecycleRate = 0.35
  , uiMoistITCZStrength = 0.3
  , uiMoistITCZWidth = 0.333
  , uiMoistMinVegFloor = 0.15
  , uiOrographicScale = 0.3
  , uiOrographicStep = 0.2
  , uiCoastalIterations = 0.5
  , uiCoastalDiffuse = 0.5
  , uiCoastalMoistureBoost = 0.4
  , uiWindBeltStrength = 0.6
  , uiWindBeltHarmonics = 0.4
  , uiWindBeltBase = 0.4
  , uiWindBeltRange = 0.6
  , uiWindBeltSpeedScale = 0.6
  , uiWindCoriolisDeflection = 0.287
  , uiBndLandRange = 0.357
  , uiBndTempConvergent = 0.467
  , uiBndTempDivergent = 0.4
  , uiBndTempTransform = 0.45
  , uiBndPrecipConvergent = 0.6
  , uiBndPrecipDivergent = 0.5
  , uiBndPrecipTransform = 0.6
  , uiGenScale = 0.4444
  , uiGenCoordScale = 0.3333
  , uiGenOffsetX = 0.5
  , uiGenOffsetY = 0.5
  , uiGenFrequency = 0.1837
  , uiGenOctaves = 0.5
  , uiGenLacunarity = 0.25
  , uiGenGain = 0.4
  , uiGenWarpScale = 0.3333
  , uiGenWarpStrength = 0.5556
  , uiWorldExtentX = 0.125
  , uiWorldExtentY = 0.125
  , uiEdgeDepthNorth = 0
  , uiEdgeDepthSouth = 0
  , uiEdgeDepthEast = 0
  , uiEdgeDepthWest = 0
  , uiEdgeDepthFalloff = 0
  , uiPanOffset = (0, 0)
  , uiZoom = 1
  , uiPlateSize = 0.45
  , uiPlateSpeed = 0.38
  , uiBoundarySharpness = 0.35
  , uiBoundaryNoiseScale = 0.33
  , uiBoundaryNoiseStrength = 0.45
  , uiBoundaryWarpOctaves = 0.5
  , uiBoundaryWarpLacunarity = 0.25
  , uiBoundaryWarpGain = 0.4
  , uiPlateMergeScale = 0.3
  , uiPlateMergeBias = 0.44
  , uiPlateDetailScale = 0.33
  , uiPlateDetailStrength = 0.35
  , uiPlateRidgeStrength = 0.25
  , uiPlateHeightBase = 0.62
  , uiPlateHeightVariance = 0.65
  , uiPlateHardnessBase = 0.42
  , uiPlateHardnessVariance = 0.4
  , uiUplift = 0.3
  , uiRiftDepth = 0.35
  , uiTrenchDepth = 0.38
  , uiRidgeHeight = 0.33
  , uiDetailScale = 0.5
  , uiPlateBiasStrength = 0.42
  , uiPlateBiasCenter = 0.5
  , uiPlateBiasEdge = 0.5
  , uiPlateBiasNorth = 0.5
  , uiPlateBiasSouth = 0.5
  , uiTfcCliffSlope = 0.2222
  , uiTfcMountainSlope = 0.2222
  , uiTfcMountainRelief = 0.2143
  , uiTfcHillSlope = 0.2105
  , uiTfcRollingSlope = 0.1579
  , uiValleyCurvature = 0.2857
  , uiTfcElevGradient = 0.2495
  , uiTfcPlateauMaxRelief2Ring = 0.2632
  , uiRockElevationThreshold = 0.5714
  , uiRockHardnessThreshold = 0.5714
  , uiRockHardnessSecondary = 0.5
  , uiWindIterations = 0.5
  , uiMoistureIterations = 0.486
  , uiBoundaryMotionTemp = 0.5
  , uiBoundaryMotionPrecip = 0.5
  , uiWeatherTick = 0.2
  , uiWeatherPhase = 0
  , uiWeatherAmplitude = 0.3
  , uiSeasonCycleLength = 0.4786
  , uiJitterAmplitude = 0.36
  , uiPressureBase = 0.5714
  , uiPressureTempScale = 0.4
  , uiPressureCoriolisScale = 0.2
  , uiSeasonalBase = 0.4
  , uiSeasonalRange = 0.6
  , uiHumidityNoiseScale = 0.3333
  , uiPrecipNoiseScale = 0.3
  , uiWeatherITCZWidth = 0.4444
  , uiWeatherITCZPrecipBoost = 0.3
  , uiPressureHumidityScale = 0.2
  , uiPressureGradientWindScale = 0.3
  , uiWindNoiseScale = 0.3333
  , uiITCZMigrationScale = 0.4667
  , uiCloudRHExponent = 0.4
  , uiCloudAlbedoEffect = 0.2667
  , uiCloudPrecipBoost = 0.24
  , uiVegBase = 0.2
  , uiVegBoost = 0.6
  , uiVegTempWeight = 0.6
  , uiVegPrecipWeight = 0.4
  , uiBtCoastalBand = 0.3
  , uiBtSnowMaxTemp = 0.4
  , uiBtAlpineMaxTemp = 0.42
  , uiBtIceCapTemp = 0.25
  , uiBtMontaneMaxTemp = 0.58
  , uiBtMontanePrecip = 0.5
  , uiBtCliffSlope = 0.3333
  , uiBtValleyMoisture = 0.5714
  , uiBtDepressionMoisture = 0.5
  , uiBtPrecipWeight = 0.3333
  , uiVbcTempMin = 0.2667
  , uiVbcTempRange = 0.4444
  , uiVbcFertilityBoost = 0.5
  , uiVbcAlbedoBase = 0.5
  , uiVbcAlbedoBare = 0.375
  , uiVbcAlbedoVeg = 0.3333
  , uiVbcOceanAlbedo = 0.3
  , uiVbcIceAlbedo = 0.7143
  , uiBiomeSmoothing = 0.2
  , uiVolcanicAshBoost = 0.4
  , uiVolcanicLavaPenalty = 0.4375
  , uiBiomeFeedbackBlend = 0.85
  , uiPlanetRadius = 0.3333
  , uiAxialTilt = 0.5209
  , uiInsolation = 0.5
  , uiOccWarmScale = 0.3
  , uiOccColdScale = 0.2
  , uiOccLatPeakDeg = 0.5833
  , uiOccLatWidthDeg = 0.5
  , uiSliceLatCenter = 0.5
  , uiSliceLonCenter = 0.5
  , uiDisabledStages = Set.empty
  , uiPluginParams = Map.empty
  , uiPluginNames = []
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
  }

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
  | SetWaterLevel !Float
  | SetRenderWaterLevel !Float
  | SetOrographicLift !Float
  | SetRainShadowLoss !Float
  | SetWindDiffuse !Float
  | SetRainRate !Float
  | SetErosionHydraulic !Float
  | SetErosionThermal !Float
  | SetErosionTalus !Float
  | SetErosionMaxDrop !Float
  | SetErosionHydDeposit !Float
  | SetErosionDepositSlope !Float
  | SetErosionThermDeposit !Float
  | SetErosionCoastZone !Float
  | SetErosionCoastStrength !Float
  | SetErosionCoastIter !Float
  | SetHypsometryEnabled !Float
  | SetHypsometryLowlandExp !Float
  | SetHypsometryHighlandExp !Float
  | SetHypsometryPlateauBreak !Float
  | SetHypsometryOceanExp !Float
  | SetHypsometryCoastalRampWidth !Float
  | SetHypsometryCoastalRampStr !Float
  | SetGlacierSnowTemp !Float
  | SetGlacierSnowRange !Float
  | SetGlacierMeltTemp !Float
  | SetGlacierMeltRate !Float
  | SetGlacierAccumScale !Float
  | SetGlacierFlowIters !Float
  | SetGlacierFlowRate !Float
  | SetGlacierErosionScale !Float
  | SetGlacierCarveScale !Float
  | SetGlacierDepositScale !Float
  | SetVentDensity !Float
  | SetVentThreshold !Float
  | SetHotspotScale !Float
  | SetHotspotThreshold !Float
  | SetMagmaRecharge !Float
  | SetLavaScale !Float
  | SetAshScale !Float
  | SetVolcanicDepositScale !Float
  | SetSoilMoistureThreshold !Float
  | SetSoilHardnessThreshold !Float
  | SetSoilFertilityMoistWeight !Float
  | SetSoilFertilityDepthWeight !Float
  | SetSinkBreachDepth !Float
  | SetStreamPowerMaxErosion !Float
  | SetRiverCarveMaxDepth !Float
  | SetCoastalErodeStrength !Float
  | SetHydroHardnessWeight !Float
  | SetPiedmontSmooth !Float
  | SetPiedmontSlopeMin !Float
  | SetPiedmontSlopeMax !Float
  | SetMinLakeSize !Float
  | SetInlandSeaMinSize !Float
  | SetRoughnessScale !Float
  | SetEquatorTemp !Float
  | SetPoleTemp !Float
  | SetLapseRate !Float
  | SetLatitudeExponent !Float
  | SetPlateHeightCooling !Float
  | SetTempNoiseScale !Float
  | SetOceanModeration !Float
  | SetOceanModerateTemp !Float
  | SetAlbedoSensitivity !Float
  | SetAlbedoReference !Float
  | SetMoistAdvect !Float
  | SetMoistLocal !Float
  | SetMoistWindEvapScale !Float
  | SetMoistEvapNoiseScale !Float
  | SetMoistBareEvapFrac !Float
  | SetMoistVegTranspFrac !Float
  | SetMoistWindETScale !Float
  | SetMoistCondensationRate !Float
  | SetMoistRecycleRate !Float
  | SetMoistITCZStrength !Float
  | SetMoistITCZWidth !Float
  | SetMoistMinVegFloor !Float
  | SetOrographicScale !Float
  | SetOrographicStep !Float
  | SetCoastalIterations !Float
  | SetCoastalDiffuse !Float
  | SetCoastalMoistureBoost !Float
  | SetWindBeltStrength !Float
  | SetWindBeltHarmonics !Float
  | SetWindBeltBase !Float
  | SetWindBeltRange !Float
  | SetWindBeltSpeedScale !Float
  | SetWindCoriolisDeflection !Float
  | SetBndLandRange !Float
  | SetBndTempConvergent !Float
  | SetBndTempDivergent !Float
  | SetBndTempTransform !Float
  | SetBndPrecipConvergent !Float
  | SetBndPrecipDivergent !Float
  | SetBndPrecipTransform !Float
  | SetGenScale !Float
  | SetGenCoordScale !Float
  | SetGenOffsetX !Float
  | SetGenOffsetY !Float
  | SetGenFrequency !Float
  | SetGenOctaves !Float
  | SetGenLacunarity !Float
  | SetGenGain !Float
  | SetGenWarpScale !Float
  | SetGenWarpStrength !Float
  | SetWorldExtentX !Float
  | SetWorldExtentY !Float
  | SetEdgeDepthNorth !Float
  | SetEdgeDepthSouth !Float
  | SetEdgeDepthEast !Float
  | SetEdgeDepthWest !Float
  | SetEdgeDepthFalloff !Float
  | SetPanOffset !(Float, Float)
  | SetZoom !Float
  | SetPlateSize !Float
  | SetPlateSpeed !Float
  | SetBoundarySharpness !Float
  | SetBoundaryNoiseScale !Float
  | SetBoundaryNoiseStrength !Float
  | SetBoundaryWarpOctaves !Float
  | SetBoundaryWarpLacunarity !Float
  | SetBoundaryWarpGain !Float
  | SetPlateMergeScale !Float
  | SetPlateMergeBias !Float
  | SetPlateDetailScale !Float
  | SetPlateDetailStrength !Float
  | SetPlateRidgeStrength !Float
  | SetPlateHeightBase !Float
  | SetPlateHeightVariance !Float
  | SetPlateHardnessBase !Float
  | SetPlateHardnessVariance !Float
  | SetUplift !Float
  | SetRiftDepth !Float
  | SetTrenchDepth !Float
  | SetRidgeHeight !Float
  | SetDetailScale !Float
  | SetPlateBiasStrength !Float
  | SetPlateBiasCenter !Float
  | SetPlateBiasEdge !Float
  | SetPlateBiasNorth !Float
  | SetPlateBiasSouth !Float
  | SetTfcCliffSlope !Float
  | SetTfcMountainSlope !Float
  | SetTfcMountainRelief !Float
  | SetTfcHillSlope !Float
  | SetTfcRollingSlope !Float
  | SetValleyCurvature !Float
  | SetTfcElevGradient !Float
  | SetTfcPlateauMaxRelief2Ring !Float
  | SetRockElevationThreshold !Float
  | SetRockHardnessThreshold !Float
  | SetRockHardnessSecondary !Float
  | SetWindIterations !Float
  | SetMoistureIterations !Float
  | SetBoundaryMotionTemp !Float
  | SetBoundaryMotionPrecip !Float
  | SetWeatherTick !Float
  | SetWeatherPhase !Float
  | SetWeatherAmplitude !Float
  | SetSeasonCycleLength !Float
  | SetJitterAmplitude !Float
  | SetPressureBase !Float
  | SetPressureTempScale !Float
  | SetPressureCoriolisScale !Float
  | SetSeasonalBase !Float
  | SetSeasonalRange !Float
  | SetHumidityNoiseScale !Float
  | SetPrecipNoiseScale !Float
  | SetWeatherITCZWidth !Float
  | SetWeatherITCZPrecipBoost !Float
  | SetPressureHumidityScale !Float
  | SetPressureGradientWindScale !Float
  | SetWindNoiseScale !Float
  | SetITCZMigrationScale !Float
  | SetCloudRHExponent !Float
  | SetCloudAlbedoEffect !Float
  | SetCloudPrecipBoost !Float
  | SetVegBase !Float
  | SetVegBoost !Float
  | SetVegTempWeight !Float
  | SetVegPrecipWeight !Float
  | SetBtCoastalBand !Float
  | SetBtSnowMaxTemp !Float
  | SetBtAlpineMaxTemp !Float
  | SetBtIceCapTemp !Float
  | SetBtMontaneMaxTemp !Float
  | SetBtMontanePrecip !Float
  | SetBtCliffSlope !Float
  | SetBtValleyMoisture !Float
  | SetBtDepressionMoisture !Float
  | SetBtPrecipWeight !Float
  | SetVbcTempMin !Float
  | SetVbcTempRange !Float
  | SetVbcFertilityBoost !Float
  | SetVbcAlbedoBase !Float
  | SetVbcAlbedoBare !Float
  | SetVbcAlbedoVeg !Float
  | SetVbcOceanAlbedo !Float
  | SetVbcIceAlbedo !Float
  | SetBiomeSmoothing !Float
  | SetVolcanicAshBoost !Float
  | SetVolcanicLavaPenalty !Float
  | SetBiomeFeedbackBlend !Float
  | SetPlanetRadius !Float
  | SetAxialTilt !Float
  | SetInsolation !Float
  | SetOccWarmScale !Float
  | SetOccColdScale !Float
  | SetOccLatPeakDeg !Float
  | SetOccLatWidthDeg !Float
  | SetSliceLatCenter !Float
  | SetSliceLonCenter !Float
  | SetHoverHex !(Maybe (Int, Int))
  | SetHoverWidget !(Maybe WidgetId)
  | SetDisabledStages !(Set StageId)
  | SetPluginParam !Text !Text !Value
  | SetPluginNames ![Text]
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
  SetWaterLevel v -> st { uiWaterLevel = clamp01 v }
  SetRenderWaterLevel v -> st { uiRenderWaterLevel = clamp01 v }
  SetOrographicLift v -> st { uiOrographicLift = clamp01 v }
  SetRainShadowLoss v -> st { uiRainShadowLoss = clamp01 v }
  SetWindDiffuse v -> st { uiWindDiffuse = clamp01 v }
  SetRainRate v -> st { uiRainRate = clamp01 v }
  SetErosionHydraulic v -> st { uiErosionHydraulic = clamp01 v }
  SetErosionThermal v -> st { uiErosionThermal = clamp01 v }
  SetErosionTalus v -> st { uiErosionTalus = clamp01 v }
  SetErosionMaxDrop v -> st { uiErosionMaxDrop = clamp01 v }
  SetErosionHydDeposit v -> st { uiErosionHydDeposit = clamp01 v }
  SetErosionDepositSlope v -> st { uiErosionDepositSlope = clamp01 v }
  SetErosionThermDeposit v -> st { uiErosionThermDeposit = clamp01 v }
  SetErosionCoastZone v -> st { uiErosionCoastZone = clamp01 v }
  SetErosionCoastStrength v -> st { uiErosionCoastStrength = clamp01 v }
  SetErosionCoastIter v -> st { uiErosionCoastIter = clamp01 v }
  SetHypsometryEnabled v -> st { uiHypsometryEnabled = clamp01 v }
  SetHypsometryLowlandExp v -> st { uiHypsometryLowlandExp = clamp01 v }
  SetHypsometryHighlandExp v -> st { uiHypsometryHighlandExp = clamp01 v }
  SetHypsometryPlateauBreak v -> st { uiHypsometryPlateauBreak = clamp01 v }
  SetHypsometryOceanExp v -> st { uiHypsometryOceanExp = clamp01 v }
  SetHypsometryCoastalRampWidth v -> st { uiHypsometryCoastalRampWidth = clamp01 v }
  SetHypsometryCoastalRampStr v -> st { uiHypsometryCoastalRampStr = clamp01 v }
  SetGlacierSnowTemp v -> st { uiGlacierSnowTemp = clamp01 v }
  SetGlacierSnowRange v -> st { uiGlacierSnowRange = clamp01 v }
  SetGlacierMeltTemp v -> st { uiGlacierMeltTemp = clamp01 v }
  SetGlacierMeltRate v -> st { uiGlacierMeltRate = clamp01 v }
  SetGlacierAccumScale v -> st { uiGlacierAccumScale = clamp01 v }
  SetGlacierFlowIters v -> st { uiGlacierFlowIters = clamp01 v }
  SetGlacierFlowRate v -> st { uiGlacierFlowRate = clamp01 v }
  SetGlacierErosionScale v -> st { uiGlacierErosionScale = clamp01 v }
  SetGlacierCarveScale v -> st { uiGlacierCarveScale = clamp01 v }
  SetGlacierDepositScale v -> st { uiGlacierDepositScale = clamp01 v }
  SetVentDensity v -> st { uiVentDensity = clamp01 v }
  SetVentThreshold v -> st { uiVentThreshold = clamp01 v }
  SetHotspotScale v -> st { uiHotspotScale = clamp01 v }
  SetHotspotThreshold v -> st { uiHotspotThreshold = clamp01 v }
  SetMagmaRecharge v -> st { uiMagmaRecharge = clamp01 v }
  SetLavaScale v -> st { uiLavaScale = clamp01 v }
  SetAshScale v -> st { uiAshScale = clamp01 v }
  SetVolcanicDepositScale v -> st { uiVolcanicDepositScale = clamp01 v }
  SetSoilMoistureThreshold v -> st { uiSoilMoistureThreshold = clamp01 v }
  SetSoilHardnessThreshold v -> st { uiSoilHardnessThreshold = clamp01 v }
  SetSoilFertilityMoistWeight v -> st { uiSoilFertilityMoistWeight = clamp01 v }
  SetSoilFertilityDepthWeight v -> st { uiSoilFertilityDepthWeight = clamp01 v }
  SetSinkBreachDepth v -> st { uiSinkBreachDepth = clamp01 v }
  SetStreamPowerMaxErosion v -> st { uiStreamPowerMaxErosion = clamp01 v }
  SetRiverCarveMaxDepth v -> st { uiRiverCarveMaxDepth = clamp01 v }
  SetCoastalErodeStrength v -> st { uiCoastalErodeStrength = clamp01 v }
  SetHydroHardnessWeight v -> st { uiHydroHardnessWeight = clamp01 v }
  SetPiedmontSmooth v -> st { uiPiedmontSmooth = clamp01 v }
  SetPiedmontSlopeMin v -> st { uiPiedmontSlopeMin = clamp01 v }
  SetPiedmontSlopeMax v -> st { uiPiedmontSlopeMax = clamp01 v }
  SetMinLakeSize v -> st { uiMinLakeSize = clamp01 v }
  SetInlandSeaMinSize v -> st { uiInlandSeaMinSize = clamp01 v }
  SetRoughnessScale v -> st { uiRoughnessScale = clamp01 v }
  SetEquatorTemp v -> st { uiEquatorTemp = clamp01 v }
  SetPoleTemp v -> st { uiPoleTemp = clamp01 v }
  SetLapseRate v -> st { uiLapseRate = clamp01 v }
  SetLatitudeExponent v -> st { uiLatitudeExponent = clamp01 v }
  SetPlateHeightCooling v -> st { uiPlateHeightCooling = clamp01 v }
  SetTempNoiseScale v -> st { uiTempNoiseScale = clamp01 v }
  SetOceanModeration v -> st { uiOceanModeration = clamp01 v }
  SetOceanModerateTemp v -> st { uiOceanModerateTemp = clamp01 v }
  SetAlbedoSensitivity v -> st { uiAlbedoSensitivity = clamp01 v }
  SetAlbedoReference v -> st { uiAlbedoReference = clamp01 v }
  SetMoistAdvect v -> st { uiMoistAdvect = clamp01 v }
  SetMoistLocal v -> st { uiMoistLocal = clamp01 v }
  SetMoistWindEvapScale v -> st { uiMoistWindEvapScale = clamp01 v }
  SetMoistEvapNoiseScale v -> st { uiMoistEvapNoiseScale = clamp01 v }
  SetMoistBareEvapFrac v -> st { uiMoistBareEvapFrac = clamp01 v }
  SetMoistVegTranspFrac v -> st { uiMoistVegTranspFrac = clamp01 v }
  SetMoistWindETScale v -> st { uiMoistWindETScale = clamp01 v }
  SetMoistCondensationRate v -> st { uiMoistCondensationRate = clamp01 v }
  SetMoistRecycleRate v -> st { uiMoistRecycleRate = clamp01 v }
  SetMoistITCZStrength v -> st { uiMoistITCZStrength = clamp01 v }
  SetMoistITCZWidth v -> st { uiMoistITCZWidth = clamp01 v }
  SetMoistMinVegFloor v -> st { uiMoistMinVegFloor = clamp01 v }
  SetOrographicScale v -> st { uiOrographicScale = clamp01 v }
  SetOrographicStep v -> st { uiOrographicStep = clamp01 v }
  SetCoastalIterations v -> st { uiCoastalIterations = clamp01 v }
  SetCoastalDiffuse v -> st { uiCoastalDiffuse = clamp01 v }
  SetCoastalMoistureBoost v -> st { uiCoastalMoistureBoost = clamp01 v }
  SetWindBeltStrength v -> st { uiWindBeltStrength = clamp01 v }
  SetWindBeltHarmonics v -> st { uiWindBeltHarmonics = clamp01 v }
  SetWindBeltBase v -> st { uiWindBeltBase = clamp01 v }
  SetWindBeltRange v -> st { uiWindBeltRange = clamp01 v }
  SetWindBeltSpeedScale v -> st { uiWindBeltSpeedScale = clamp01 v }
  SetWindCoriolisDeflection v -> st { uiWindCoriolisDeflection = clamp01 v }
  SetBndLandRange v -> st { uiBndLandRange = clamp01 v }
  SetBndTempConvergent v -> st { uiBndTempConvergent = clamp01 v }
  SetBndTempDivergent v -> st { uiBndTempDivergent = clamp01 v }
  SetBndTempTransform v -> st { uiBndTempTransform = clamp01 v }
  SetBndPrecipConvergent v -> st { uiBndPrecipConvergent = clamp01 v }
  SetBndPrecipDivergent v -> st { uiBndPrecipDivergent = clamp01 v }
  SetBndPrecipTransform v -> st { uiBndPrecipTransform = clamp01 v }
  SetGenScale v -> st { uiGenScale = clamp01 v }
  SetGenCoordScale v -> st { uiGenCoordScale = clamp01 v }
  SetGenOffsetX v -> st { uiGenOffsetX = clamp01 v }
  SetGenOffsetY v -> st { uiGenOffsetY = clamp01 v }
  SetGenFrequency v -> st { uiGenFrequency = clamp01 v }
  SetGenOctaves v -> st { uiGenOctaves = clamp01 v }
  SetGenLacunarity v -> st { uiGenLacunarity = clamp01 v }
  SetGenGain v -> st { uiGenGain = clamp01 v }
  SetGenWarpScale v -> st { uiGenWarpScale = clamp01 v }
  SetGenWarpStrength v -> st { uiGenWarpStrength = clamp01 v }
  SetWorldExtentX v -> st { uiWorldExtentX = clamp01 v }
  SetWorldExtentY v -> st { uiWorldExtentY = clamp01 v }
  SetEdgeDepthNorth v -> st { uiEdgeDepthNorth = clamp01 v }
  SetEdgeDepthSouth v -> st { uiEdgeDepthSouth = clamp01 v }
  SetEdgeDepthEast v -> st { uiEdgeDepthEast = clamp01 v }
  SetEdgeDepthWest v -> st { uiEdgeDepthWest = clamp01 v }
  SetEdgeDepthFalloff v -> st { uiEdgeDepthFalloff = clamp01 v }
  SetPanOffset v -> st { uiPanOffset = v }
  SetZoom v -> st { uiZoom = clampZoom v }
  SetPlateSize v -> st { uiPlateSize = clamp01 v }
  SetPlateSpeed v -> st { uiPlateSpeed = clamp01 v }
  SetBoundarySharpness v -> st { uiBoundarySharpness = clamp01 v }
  SetBoundaryNoiseScale v -> st { uiBoundaryNoiseScale = clamp01 v }
  SetBoundaryNoiseStrength v -> st { uiBoundaryNoiseStrength = clamp01 v }
  SetBoundaryWarpOctaves v -> st { uiBoundaryWarpOctaves = clamp01 v }
  SetBoundaryWarpLacunarity v -> st { uiBoundaryWarpLacunarity = clamp01 v }
  SetBoundaryWarpGain v -> st { uiBoundaryWarpGain = clamp01 v }
  SetPlateMergeScale v -> st { uiPlateMergeScale = clamp01 v }
  SetPlateMergeBias v -> st { uiPlateMergeBias = clamp01 v }
  SetPlateDetailScale v -> st { uiPlateDetailScale = clamp01 v }
  SetPlateDetailStrength v -> st { uiPlateDetailStrength = clamp01 v }
  SetPlateRidgeStrength v -> st { uiPlateRidgeStrength = clamp01 v }
  SetPlateHeightBase v -> st { uiPlateHeightBase = clamp01 v }
  SetPlateHeightVariance v -> st { uiPlateHeightVariance = clamp01 v }
  SetPlateHardnessBase v -> st { uiPlateHardnessBase = clamp01 v }
  SetPlateHardnessVariance v -> st { uiPlateHardnessVariance = clamp01 v }
  SetUplift v -> st { uiUplift = clamp01 v }
  SetRiftDepth v -> st { uiRiftDepth = clamp01 v }
  SetTrenchDepth v -> st { uiTrenchDepth = clamp01 v }
  SetRidgeHeight v -> st { uiRidgeHeight = clamp01 v }
  SetDetailScale v -> st { uiDetailScale = clamp01 v }
  SetPlateBiasStrength v -> st { uiPlateBiasStrength = clamp01 v }
  SetPlateBiasCenter v -> st { uiPlateBiasCenter = clamp01 v }
  SetPlateBiasEdge v -> st { uiPlateBiasEdge = clamp01 v }
  SetPlateBiasNorth v -> st { uiPlateBiasNorth = clamp01 v }
  SetPlateBiasSouth v -> st { uiPlateBiasSouth = clamp01 v }
  SetTfcCliffSlope v -> st { uiTfcCliffSlope = clamp01 v }
  SetTfcMountainSlope v -> st { uiTfcMountainSlope = clamp01 v }
  SetTfcMountainRelief v -> st { uiTfcMountainRelief = clamp01 v }
  SetTfcHillSlope v -> st { uiTfcHillSlope = clamp01 v }
  SetTfcRollingSlope v -> st { uiTfcRollingSlope = clamp01 v }
  SetValleyCurvature v -> st { uiValleyCurvature = clamp01 v }
  SetTfcElevGradient v -> st { uiTfcElevGradient = clamp01 v }
  SetTfcPlateauMaxRelief2Ring v -> st { uiTfcPlateauMaxRelief2Ring = clamp01 v }
  SetRockElevationThreshold v -> st { uiRockElevationThreshold = clamp01 v }
  SetRockHardnessThreshold v -> st { uiRockHardnessThreshold = clamp01 v }
  SetRockHardnessSecondary v -> st { uiRockHardnessSecondary = clamp01 v }
  SetWindIterations v -> st { uiWindIterations = clamp01 v }
  SetMoistureIterations v -> st { uiMoistureIterations = clamp01 v }
  SetBoundaryMotionTemp v -> st { uiBoundaryMotionTemp = clamp01 v }
  SetBoundaryMotionPrecip v -> st { uiBoundaryMotionPrecip = clamp01 v }
  SetWeatherTick v -> st { uiWeatherTick = clamp01 v }
  SetWeatherPhase v -> st { uiWeatherPhase = clamp01 v }
  SetWeatherAmplitude v -> st { uiWeatherAmplitude = clamp01 v }
  SetSeasonCycleLength v -> st { uiSeasonCycleLength = clamp01 v }
  SetJitterAmplitude v -> st { uiJitterAmplitude = clamp01 v }
  SetPressureBase v -> st { uiPressureBase = clamp01 v }
  SetPressureTempScale v -> st { uiPressureTempScale = clamp01 v }
  SetPressureCoriolisScale v -> st { uiPressureCoriolisScale = clamp01 v }
  SetSeasonalBase v -> st { uiSeasonalBase = clamp01 v }
  SetSeasonalRange v -> st { uiSeasonalRange = clamp01 v }
  SetHumidityNoiseScale v -> st { uiHumidityNoiseScale = clamp01 v }
  SetPrecipNoiseScale v -> st { uiPrecipNoiseScale = clamp01 v }
  SetWeatherITCZWidth v -> st { uiWeatherITCZWidth = clamp01 v }
  SetWeatherITCZPrecipBoost v -> st { uiWeatherITCZPrecipBoost = clamp01 v }
  SetPressureHumidityScale v -> st { uiPressureHumidityScale = clamp01 v }
  SetPressureGradientWindScale v -> st { uiPressureGradientWindScale = clamp01 v }
  SetWindNoiseScale v -> st { uiWindNoiseScale = clamp01 v }
  SetITCZMigrationScale v -> st { uiITCZMigrationScale = clamp01 v }
  SetCloudRHExponent v -> st { uiCloudRHExponent = clamp01 v }
  SetCloudAlbedoEffect v -> st { uiCloudAlbedoEffect = clamp01 v }
  SetCloudPrecipBoost v -> st { uiCloudPrecipBoost = clamp01 v }
  SetVegBase v -> st { uiVegBase = clamp01 v }
  SetVegBoost v -> st { uiVegBoost = clamp01 v }
  SetVegTempWeight v -> st { uiVegTempWeight = clamp01 v }
  SetVegPrecipWeight v -> st { uiVegPrecipWeight = clamp01 v }
  SetBtCoastalBand v -> st { uiBtCoastalBand = clamp01 v }
  SetBtSnowMaxTemp v -> st { uiBtSnowMaxTemp = clamp01 v }
  SetBtAlpineMaxTemp v -> st { uiBtAlpineMaxTemp = clamp01 v }
  SetBtIceCapTemp v -> st { uiBtIceCapTemp = clamp01 v }
  SetBtMontaneMaxTemp v -> st { uiBtMontaneMaxTemp = clamp01 v }
  SetBtMontanePrecip v -> st { uiBtMontanePrecip = clamp01 v }
  SetBtCliffSlope v -> st { uiBtCliffSlope = clamp01 v }
  SetBtValleyMoisture v -> st { uiBtValleyMoisture = clamp01 v }
  SetBtDepressionMoisture v -> st { uiBtDepressionMoisture = clamp01 v }
  SetBtPrecipWeight v -> st { uiBtPrecipWeight = clamp01 v }
  SetVbcTempMin v -> st { uiVbcTempMin = clamp01 v }
  SetVbcTempRange v -> st { uiVbcTempRange = clamp01 v }
  SetVbcFertilityBoost v -> st { uiVbcFertilityBoost = clamp01 v }
  SetVbcAlbedoBase v -> st { uiVbcAlbedoBase = clamp01 v }
  SetVbcAlbedoBare v -> st { uiVbcAlbedoBare = clamp01 v }
  SetVbcAlbedoVeg v -> st { uiVbcAlbedoVeg = clamp01 v }
  SetVbcOceanAlbedo v -> st { uiVbcOceanAlbedo = clamp01 v }
  SetVbcIceAlbedo v -> st { uiVbcIceAlbedo = clamp01 v }
  SetBiomeSmoothing v -> st { uiBiomeSmoothing = clamp01 v }
  SetVolcanicAshBoost v -> st { uiVolcanicAshBoost = clamp01 v }
  SetVolcanicLavaPenalty v -> st { uiVolcanicLavaPenalty = clamp01 v }
  SetBiomeFeedbackBlend v -> st { uiBiomeFeedbackBlend = clamp01 v }
  SetPlanetRadius v -> st { uiPlanetRadius = clamp01 v }
  SetAxialTilt v -> st { uiAxialTilt = clamp01 v }
  SetInsolation v -> st { uiInsolation = clamp01 v }
  SetOccWarmScale v -> st { uiOccWarmScale = clamp01 v }
  SetOccColdScale v -> st { uiOccColdScale = clamp01 v }
  SetOccLatPeakDeg v -> st { uiOccLatPeakDeg = clamp01 v }
  SetOccLatWidthDeg v -> st { uiOccLatWidthDeg = clamp01 v }
  SetSliceLatCenter v -> st { uiSliceLatCenter = clamp01 v }
  SetSliceLonCenter v -> st { uiSliceLonCenter = clamp01 v }
  SetHoverHex v -> st { uiHoverHex = v }
  SetHoverWidget v -> st { uiHoverWidget = v }
  SetDisabledStages v -> st { uiDisabledStages = v }
  SetPluginParam pluginName paramName value ->
    let inner = Map.findWithDefault Map.empty pluginName (uiPluginParams st)
    in st
         { uiPluginParams =
             Map.insert pluginName (Map.insert paramName value inner) (uiPluginParams st)
         }
  SetPluginNames v -> st { uiPluginNames = v }
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

uiSnapshotTag :: OpTag "uiSnapshot"
uiSnapshotTag = OpTag

[hyperspace|
replyprotocol UiSnapshotReply =
  cast uiSnapshot :: UiState

actor Ui
  state UiState
  lifetime Singleton
  schedule pinned 1
  noDeps
  mailbox Unbounded

  cast update :: UiUpdate
  cast snapshotAsync :: () reply UiSnapshotReply
  call snapshot :: () -> UiState

  initial emptyUiState
  onPure_ update = \upd st -> applyUpdate upd st
  onReply snapshotAsync = \() replyTo st -> do
    replyCast replyTo uiSnapshotTag st
    pure st
  onPure snapshot = \() st -> (st, st)
|]

getUiSnapshot :: ActorHandle Ui (Protocol Ui) -> IO UiState
getUiSnapshot handle =
  call @"snapshot" handle #snapshot ()

requestUiSnapshot :: ActorHandle Ui (Protocol Ui) -> ReplyTo UiSnapshotReply -> IO ()
requestUiSnapshot handle replyTo =
  castReply @"snapshotAsync" handle replyTo #snapshotAsync ()

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
