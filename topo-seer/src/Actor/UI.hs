{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Actor.UI
  ( Ui
  , ConfigTab(..)
  , LeftTab(..)
  , UiMenuMode(..)
  , ViewMode(..)
  , UiState(..)
  , emptyUiState
  , uiActorDef
  , setUiSeed
  , setUiGenerating
  , setUiViewMode
  , setUiChunkSize
  , setUiShowConfig
  , setUiConfigScroll
  , setUiShowLeftPanel
  , setUiWaterLevel
  , setUiRenderWaterLevel
  , setUiEvaporation
  , setUiRainShadow
  , setUiWindDiffuse
  , setUiRainRate
  , setUiErosionHydraulic
  , setUiErosionThermal
  , setUiErosionTalus
  , setUiErosionMaxDrop
  , setUiGlacierSnowTemp
  , setUiGlacierSnowRange
  , setUiGlacierMeltTemp
  , setUiGlacierMeltRate
  , setUiGlacierAccumScale
  , setUiGlacierFlowIters
  , setUiGlacierFlowRate
  , setUiGlacierErosionScale
  , setUiGlacierCarveScale
  , setUiGlacierDepositScale
  , setUiVentDensity
  , setUiVentThreshold
  , setUiHotspotScale
  , setUiHotspotThreshold
  , setUiMagmaRecharge
  , setUiLavaScale
  , setUiAshScale
  , setUiVolcanicDepositScale
  , setUiSoilMoistureThreshold
  , setUiSoilHardnessThreshold
  , setUiSoilFertilityMoistWeight
  , setUiSoilFertilityDepthWeight
  , setUiSinkBreachDepth
  , setUiStreamPowerMaxErosion
  , setUiRiverCarveMaxDepth
  , setUiCoastalErodeStrength
  , setUiHydroHardnessWeight
  , setUiMinLakeSize
  , setUiInlandSeaMinSize
  , setUiRoughnessScale
  , setUiEquatorTemp
  , setUiPoleTemp
  , setUiLapseRate
  , setUiLatitudeExponent
  , setUiPlateHeightCooling
  , setUiTempNoiseScale
  , setUiOceanModeration
  , setUiOceanModerateTemp
  , setUiAlbedoSensitivity
  , setUiAlbedoReference
  , setUiMoistAdvect
  , setUiMoistLocal
  , setUiMoistWindEvapScale
  , setUiMoistEvapNoiseScale
  , setUiMoistLandETCoeff
  , setUiMoistBareEvapFrac
  , setUiMoistVegTranspFrac
  , setUiMoistWindETScale
  , setUiMoistCondensationRate
  , setUiMoistRecycleRate
  , setUiMoistITCZStrength
  , setUiMoistITCZWidth
  , setUiOrographicScale
  , setUiOrographicStep
  , setUiCoastalIterations
  , setUiCoastalDiffuse
  , setUiCoastalMoistureBoost
  , setUiWindBeltStrength
  , setUiWindBeltHarmonics
  , setUiWindBeltBase
  , setUiWindBeltRange
  , setUiWindBeltSpeedScale
  , setUiBndLandRange
  , setUiBndTempConvergent
  , setUiBndTempDivergent
  , setUiBndTempTransform
  , setUiBndPrecipConvergent
  , setUiBndPrecipDivergent
  , setUiBndPrecipTransform
  , setUiConfigTab
  , setUiLeftTab
  , setUiGenScale
  , setUiGenCoordScale
  , setUiGenOffsetX
  , setUiGenOffsetY
  , setUiGenFrequency
  , setUiGenOctaves
  , setUiGenLacunarity
  , setUiGenGain
  , setUiGenWarpScale
  , setUiGenWarpStrength
  , setUiWorldExtentX
  , setUiWorldExtentY
  , setUiEdgeDepthNorth
  , setUiEdgeDepthSouth
  , setUiEdgeDepthEast
  , setUiEdgeDepthWest
  , setUiEdgeDepthFalloff
  , setUiPanOffset
  , setUiZoom
  , setUiPlateSize
  , setUiPlateSpeed
  , setUiBoundarySharpness
  , setUiBoundaryNoiseScale
  , setUiBoundaryNoiseStrength
  , setUiBoundaryWarpOctaves
  , setUiBoundaryWarpLacunarity
  , setUiBoundaryWarpGain
  , setUiPlateMergeScale
  , setUiPlateMergeBias
  , setUiPlateDetailScale
  , setUiPlateDetailStrength
  , setUiPlateRidgeStrength
  , setUiPlateHeightBase
  , setUiPlateHeightVariance
  , setUiPlateHardnessBase
  , setUiPlateHardnessVariance
  , setUiUplift
  , setUiRiftDepth
  , setUiTrenchDepth
  , setUiRidgeHeight
  , setUiDetailScale
  , setUiPlateBiasStrength
  , setUiPlateBiasCenter
  , setUiPlateBiasEdge
  , setUiPlateBiasNorth
  , setUiPlateBiasSouth
  , setUiTfcCliffSlope
  , setUiTfcMountainSlope
  , setUiTfcMountainRelief
  , setUiTfcHillSlope
  , setUiTfcRollingSlope
  , setUiValleyCurvature
  , setUiRockElevationThreshold
  , setUiRockHardnessThreshold
  , setUiRockHardnessSecondary
  , setUiWindIterations
  , setUiMoistureIterations
  , setUiBoundaryMotionTemp
  , setUiBoundaryMotionPrecip
  , setUiWeatherTick
  , setUiWeatherPhase
  , setUiWeatherAmplitude
  , setUiSeasonCycleLength
  , setUiJitterAmplitude
  , setUiPressureBase
  , setUiPressureTempScale
  , setUiPressureCoriolisScale
  , setUiSeasonalBase
  , setUiSeasonalRange
  , setUiHumidityNoiseScale
  , setUiPrecipNoiseScale
  , setUiWeatherITCZWidth
  , setUiWeatherITCZPrecipBoost
  , setUiPressureHumidityScale
  , setUiPressureGradientWindScale
  , setUiWindNoiseScale
  , setUiITCZMigrationScale
  , setUiCloudRHExponent
  , setUiCloudAlbedoEffect
  , setUiCloudPrecipBoost
  , setUiVegBase
  , setUiVegBoost
  , setUiVegTempWeight
  , setUiVegPrecipWeight
  , setUiBtCoastalBand
  , setUiBtSnowElevation
  , setUiBtAlpineElevation
  , setUiBtIceCapTemp
  , setUiBtMontaneLow
  , setUiBtMontanePrecip
  , setUiBtCliffSlope
  , setUiBtValleyMoisture
  , setUiBtDepressionMoisture
  , setUiBtPrecipWeight
  , setUiVbcTempMin
  , setUiVbcTempRange
  , setUiVbcFertilityBoost
  , setUiVbcAlbedoBase
  , setUiVbcAlbedoBare
  , setUiVbcAlbedoVeg
  , setUiVbcOceanAlbedo
  , setUiVbcIceAlbedo
  , setUiBiomeSmoothing
  , setUiVolcanicAshBoost
  , setUiVolcanicLavaPenalty
  , setUiBiomeFeedbackBlend
  , setUiPlanetRadius
  , setUiAxialTilt
  , setUiInsolation
  , setUiOccWarmScale
  , setUiOccColdScale
  , setUiOccLatPeakDeg
  , setUiOccLatWidthDeg
  , setUiSliceLatCenter
  , setUiSliceLonCenter
  , setUiHoverHex
  , setUiHoverWidget
  , setUiMenuMode
  , setUiPresetInput
  , setUiPresetList
  , setUiPresetSelected
  , setUiWorldConfig
  , setUiWorldName
  , setUiWorldSaveInput
  , setUiWorldList
  , setUiWorldSelected
  , setUiContextHex
  , setUiContextPos
  , setUiSeedEditing
  , setUiSeedInput
  , UiSnapshotReply
  , requestUiSnapshot
  , getUiSnapshot
  ) where

import Data.Word (Word64)
import Data.Text (Text)
import qualified Data.Text as Text
import Hyperspace.Actor
import Hyperspace.Actor.QQ (hyperspace)
import Hyperspace.Actor.Spec (OpTag(..))
import Seer.Config.Preset.Types (ConfigPreset)
import Seer.World.Persist.Types (WorldSaveManifest)
import UI.WidgetTree (WidgetId)

data ViewMode
  = ViewElevation
  | ViewBiome
  | ViewClimate
  | ViewMoisture
  | ViewPrecip
  | ViewPlateId
  | ViewPlateBoundary
  | ViewPlateHardness
  | ViewPlateCrust
  | ViewPlateAge
  | ViewPlateHeight
  | ViewPlateVelocity
  deriving (Eq, Show)

data ConfigTab
  = ConfigTerrain
  | ConfigPlanet
  | ConfigClimate
  | ConfigWeather
  | ConfigBiome
  | ConfigErosion
  deriving (Eq, Show)

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
  , uiSeedEditing :: !Bool
  , uiSeedInput :: !Text
  , uiWaterLevel :: !Float
  , uiRenderWaterLevel :: !Float
  , uiEvaporation :: !Float
  , uiRainShadow :: !Float
  , uiWindDiffuse :: !Float
  , uiRainRate :: !Float
  , uiErosionHydraulic :: !Float
  , uiErosionThermal :: !Float
  , uiErosionTalus :: !Float
  , uiErosionMaxDrop :: !Float
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
  , uiMoistLandETCoeff :: !Float
  , uiMoistBareEvapFrac :: !Float
  , uiMoistVegTranspFrac :: !Float
  , uiMoistWindETScale :: !Float
  , uiMoistCondensationRate :: !Float
  , uiMoistRecycleRate :: !Float
  , uiMoistITCZStrength :: !Float
  , uiMoistITCZWidth :: !Float
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
  , uiBtSnowElevation :: !Float
  , uiBtAlpineElevation :: !Float
  , uiBtIceCapTemp :: !Float
  , uiBtMontaneLow :: !Float
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
  , uiHoverHex :: !(Maybe (Int, Int))
  , uiHoverWidget :: !(Maybe WidgetId)
  , uiWorldConfig :: !(Maybe ConfigPreset)
  , uiWorldName :: !Text
  , uiWorldSaveInput :: !Text
  , uiWorldList :: ![WorldSaveManifest]
  , uiWorldSelected :: !Int
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
  , uiSeedEditing = False
  , uiSeedInput = Text.empty
  , uiWaterLevel = 0.5
  , uiRenderWaterLevel = 0.5
  , uiEvaporation = 0.25
  , uiRainShadow = 0.4
  , uiWindDiffuse = 0.5
  , uiRainRate = 0.2
  , uiErosionHydraulic = 0.5
  , uiErosionThermal = 0.4
  , uiErosionTalus = 0.5
  , uiErosionMaxDrop = 0.5
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
  , uiMoistLandETCoeff = 0.65
  , uiMoistBareEvapFrac = 0.15
  , uiMoistVegTranspFrac = 0.85
  , uiMoistWindETScale = 0.2
  , uiMoistCondensationRate = 0.4
  , uiMoistRecycleRate = 0.35
  , uiMoistITCZStrength = 0.3
  , uiMoistITCZWidth = 0.333
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
  , uiTfcCliffSlope = 0.4286
  , uiTfcMountainSlope = 0.3333
  , uiTfcMountainRelief = 0.4444
  , uiTfcHillSlope = 0.3333
  , uiTfcRollingSlope = 0.1579
  , uiValleyCurvature = 0.2857
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
  , uiSeasonCycleLength = 0.4786    -- maps to 365 in [30..730]
  , uiJitterAmplitude = 0.36        -- maps to 0.18 in [0..0.5]
  , uiPressureBase = 0.5714         -- maps to 0.7 in [0.3..1.0]
  , uiPressureTempScale = 0.4       -- maps to 0.4 in [0..1]
  , uiPressureCoriolisScale = 0.2   -- maps to 0.1 in [0..0.5]
  , uiSeasonalBase = 0.4            -- maps to 0.4 in [0..1]
  , uiSeasonalRange = 0.6           -- maps to 1.2 in [0..2]
  , uiHumidityNoiseScale = 0.3333   -- maps to 0.1 in [0..0.3]
  , uiPrecipNoiseScale = 0.3        -- maps to 0.15 in [0..0.5]
  , uiWeatherITCZWidth = 0.4444     -- maps to 10 in [2..20]
  , uiWeatherITCZPrecipBoost = 0.3  -- maps to 0.3 in [0..1]
  , uiPressureHumidityScale = 0.2   -- maps to 0.1 in [0..0.5]
  , uiPressureGradientWindScale = 0.3 -- maps to 0.3 in [0..1]
  , uiWindNoiseScale = 0.3333       -- maps to 0.1 in [0..0.3]
  , uiITCZMigrationScale = 0.4667   -- maps to 0.7 in [0..1.5]
  , uiCloudRHExponent = 0.4         -- maps to 1.5 in [0.5..3.0]
  , uiCloudAlbedoEffect = 0.2667    -- maps to 0.08 in [0..0.3]
  , uiCloudPrecipBoost = 0.24       -- maps to 0.12 in [0..0.5]
  , uiVegBase = 0.2
  , uiVegBoost = 0.6
  , uiVegTempWeight = 0.6
  , uiVegPrecipWeight = 0.4
  , uiBtCoastalBand = 0.3
  , uiBtSnowElevation = 0.8
  , uiBtAlpineElevation = 0.7
  , uiBtIceCapTemp = 0.25
  , uiBtMontaneLow = 0.44
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
  , uiPlanetRadius = 0.3333   -- maps to 6371 in [4778..9557]
  , uiAxialTilt = 0.5209      -- maps to 23.44 in [0..45]
  , uiInsolation = 0.5        -- maps to 1.0 in [0.7..1.3]
  , uiOccWarmScale = 0.3      -- maps to 0.06 in [0..0.2]
  , uiOccColdScale = 0.2      -- maps to 0.04 in [0..0.2]
  , uiOccLatPeakDeg = 0.5833  -- maps to 35 in [0..60]
  , uiOccLatWidthDeg = 0.5    -- maps to 25 in [5..45]
  , uiSliceLatCenter = 0.5    -- maps to 0 in [-90..90]
  , uiSliceLonCenter = 0.5    -- maps to 0 in [-180..180]
  , uiHoverHex = Nothing
  , uiHoverWidget = Nothing
  , uiWorldConfig = Nothing
  , uiWorldName = Text.pack "Untitled"
  , uiWorldSaveInput = Text.empty
  , uiWorldList = []
  , uiWorldSelected = 0
  }

-- | Sum type encoding all possible UI state mutations.
--
-- Sent as a single cast message to the 'Ui' actor; the public API
-- ('setUiSeed', 'setUiGenerating', etc.) constructs the appropriate
-- constructor and forwards it.
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
  | SetSeedEditing !Bool
  | SetSeedInput !Text
  | SetWaterLevel !Float
  | SetRenderWaterLevel !Float
  | SetEvaporation !Float
  | SetRainShadow !Float
  | SetWindDiffuse !Float
  | SetRainRate !Float
  | SetErosionHydraulic !Float
  | SetErosionThermal !Float
  | SetErosionTalus !Float
  | SetErosionMaxDrop !Float
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
  | SetMoistLandETCoeff !Float
  | SetMoistBareEvapFrac !Float
  | SetMoistVegTranspFrac !Float
  | SetMoistWindETScale !Float
  | SetMoistCondensationRate !Float
  | SetMoistRecycleRate !Float
  | SetMoistITCZStrength !Float
  | SetMoistITCZWidth !Float
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
  | SetBtSnowElevation !Float
  | SetBtAlpineElevation !Float
  | SetBtIceCapTemp !Float
  | SetBtMontaneLow !Float
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
  | SetWorldConfig !(Maybe ConfigPreset)
  | SetWorldName !Text
  | SetWorldSaveInput !Text
  | SetWorldList ![WorldSaveManifest]
  | SetWorldSelected !Int

-- | Apply a 'UiUpdate' to the current 'UiState'.  Total and exhaustive.
applyUpdate :: UiUpdate -> UiState -> UiState
applyUpdate upd st = case upd of
  SetSeed v            -> st { uiSeed = v }
  SetGenerating v      -> st { uiGenerating = v }
  SetViewMode v        -> st { uiViewMode = v }
  SetChunkSize v       -> st { uiChunkSize = clampChunk v }
  SetShowConfig v      -> st { uiShowConfig = v }
  SetShowLeftPanel v   -> st { uiShowLeftPanel = v }
  SetConfigTab v       -> st { uiConfigTab = v }
  SetConfigScroll v    -> st { uiConfigScroll = max 0 v }
  SetLeftTab v         -> st { uiLeftTab = v }
  SetMenuMode v        -> st { uiMenuMode = v }
  SetPresetInput v     -> st { uiPresetInput = v }
  SetPresetList v      -> st { uiPresetList = v }
  SetPresetSelected v  -> st { uiPresetSelected = max 0 v }
  SetContextHex v      -> st { uiContextHex = v }
  SetContextPos v      -> st { uiContextPos = v }
  SetSeedEditing v     -> st { uiSeedEditing = v }
  SetSeedInput v       -> st { uiSeedInput = v }
  SetWaterLevel v      -> st { uiWaterLevel = clamp01 v }
  SetRenderWaterLevel v -> st { uiRenderWaterLevel = clamp01 v }
  SetEvaporation v     -> st { uiEvaporation = clamp01 v }
  SetRainShadow v      -> st { uiRainShadow = clamp01 v }
  SetWindDiffuse v     -> st { uiWindDiffuse = clamp01 v }
  SetRainRate v        -> st { uiRainRate = clamp01 v }
  SetErosionHydraulic v -> st { uiErosionHydraulic = clamp01 v }
  SetErosionThermal v  -> st { uiErosionThermal = clamp01 v }
  SetErosionTalus v    -> st { uiErosionTalus = clamp01 v }
  SetErosionMaxDrop v  -> st { uiErosionMaxDrop = clamp01 v }
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
  SetMinLakeSize v -> st { uiMinLakeSize = clamp01 v }
  SetInlandSeaMinSize v -> st { uiInlandSeaMinSize = clamp01 v }
  SetRoughnessScale v -> st { uiRoughnessScale = clamp01 v }
  SetEquatorTemp v     -> st { uiEquatorTemp = clamp01 v }
  SetPoleTemp v        -> st { uiPoleTemp = clamp01 v }
  SetLapseRate v       -> st { uiLapseRate = clamp01 v }
  SetLatitudeExponent v -> st { uiLatitudeExponent = clamp01 v }
  SetPlateHeightCooling v -> st { uiPlateHeightCooling = clamp01 v }
  SetTempNoiseScale v  -> st { uiTempNoiseScale = clamp01 v }
  SetOceanModeration v -> st { uiOceanModeration = clamp01 v }
  SetOceanModerateTemp v -> st { uiOceanModerateTemp = clamp01 v }
  SetAlbedoSensitivity v -> st { uiAlbedoSensitivity = clamp01 v }
  SetAlbedoReference v -> st { uiAlbedoReference = clamp01 v }
  SetMoistAdvect v     -> st { uiMoistAdvect = clamp01 v }
  SetMoistLocal v      -> st { uiMoistLocal = clamp01 v }
  SetMoistWindEvapScale v -> st { uiMoistWindEvapScale = clamp01 v }
  SetMoistEvapNoiseScale v -> st { uiMoistEvapNoiseScale = clamp01 v }
  SetMoistLandETCoeff v -> st { uiMoistLandETCoeff = clamp01 v }
  SetMoistBareEvapFrac v -> st { uiMoistBareEvapFrac = clamp01 v }
  SetMoistVegTranspFrac v -> st { uiMoistVegTranspFrac = clamp01 v }
  SetMoistWindETScale v -> st { uiMoistWindETScale = clamp01 v }
  SetMoistCondensationRate v -> st { uiMoistCondensationRate = clamp01 v }
  SetMoistRecycleRate v -> st { uiMoistRecycleRate = clamp01 v }
  SetMoistITCZStrength v -> st { uiMoistITCZStrength = clamp01 v }
  SetMoistITCZWidth v  -> st { uiMoistITCZWidth = clamp01 v }
  SetOrographicScale v      -> st { uiOrographicScale = clamp01 v }
  SetOrographicStep v       -> st { uiOrographicStep = clamp01 v }
  SetCoastalIterations v    -> st { uiCoastalIterations = clamp01 v }
  SetCoastalDiffuse v       -> st { uiCoastalDiffuse = clamp01 v }
  SetCoastalMoistureBoost v -> st { uiCoastalMoistureBoost = clamp01 v }
  SetWindBeltStrength v   -> st { uiWindBeltStrength = clamp01 v }
  SetWindBeltHarmonics v  -> st { uiWindBeltHarmonics = clamp01 v }
  SetWindBeltBase v       -> st { uiWindBeltBase = clamp01 v }
  SetWindBeltRange v      -> st { uiWindBeltRange = clamp01 v }
  SetWindBeltSpeedScale v -> st { uiWindBeltSpeedScale = clamp01 v }
  SetBndLandRange v       -> st { uiBndLandRange = clamp01 v }
  SetBndTempConvergent v  -> st { uiBndTempConvergent = clamp01 v }
  SetBndTempDivergent v   -> st { uiBndTempDivergent = clamp01 v }
  SetBndTempTransform v   -> st { uiBndTempTransform = clamp01 v }
  SetBndPrecipConvergent v -> st { uiBndPrecipConvergent = clamp01 v }
  SetBndPrecipDivergent v -> st { uiBndPrecipDivergent = clamp01 v }
  SetBndPrecipTransform v -> st { uiBndPrecipTransform = clamp01 v }
  SetGenScale v        -> st { uiGenScale = clamp01 v }
  SetGenCoordScale v   -> st { uiGenCoordScale = clamp01 v }
  SetGenOffsetX v      -> st { uiGenOffsetX = clamp01 v }
  SetGenOffsetY v      -> st { uiGenOffsetY = clamp01 v }
  SetGenFrequency v    -> st { uiGenFrequency = clamp01 v }
  SetGenOctaves v      -> st { uiGenOctaves = clamp01 v }
  SetGenLacunarity v   -> st { uiGenLacunarity = clamp01 v }
  SetGenGain v         -> st { uiGenGain = clamp01 v }
  SetGenWarpScale v    -> st { uiGenWarpScale = clamp01 v }
  SetGenWarpStrength v -> st { uiGenWarpStrength = clamp01 v }
  SetWorldExtentX v    -> st { uiWorldExtentX = clamp01 v }
  SetWorldExtentY v    -> st { uiWorldExtentY = clamp01 v }
  SetEdgeDepthNorth v  -> st { uiEdgeDepthNorth = clamp01 v }
  SetEdgeDepthSouth v  -> st { uiEdgeDepthSouth = clamp01 v }
  SetEdgeDepthEast v   -> st { uiEdgeDepthEast = clamp01 v }
  SetEdgeDepthWest v   -> st { uiEdgeDepthWest = clamp01 v }
  SetEdgeDepthFalloff v -> st { uiEdgeDepthFalloff = clamp01 v }
  SetPanOffset v       -> st { uiPanOffset = v }
  SetZoom v            -> st { uiZoom = clampZoom v }
  SetPlateSize v       -> st { uiPlateSize = clamp01 v }
  SetPlateSpeed v      -> st { uiPlateSpeed = clamp01 v }
  SetBoundarySharpness v -> st { uiBoundarySharpness = clamp01 v }
  SetBoundaryNoiseScale v -> st { uiBoundaryNoiseScale = clamp01 v }
  SetBoundaryNoiseStrength v -> st { uiBoundaryNoiseStrength = clamp01 v }
  SetBoundaryWarpOctaves v -> st { uiBoundaryWarpOctaves = clamp01 v }
  SetBoundaryWarpLacunarity v -> st { uiBoundaryWarpLacunarity = clamp01 v }
  SetBoundaryWarpGain v -> st { uiBoundaryWarpGain = clamp01 v }
  SetPlateMergeScale v -> st { uiPlateMergeScale = clamp01 v }
  SetPlateMergeBias v  -> st { uiPlateMergeBias = clamp01 v }
  SetPlateDetailScale v -> st { uiPlateDetailScale = clamp01 v }
  SetPlateDetailStrength v -> st { uiPlateDetailStrength = clamp01 v }
  SetPlateRidgeStrength v -> st { uiPlateRidgeStrength = clamp01 v }
  SetPlateHeightBase v -> st { uiPlateHeightBase = clamp01 v }
  SetPlateHeightVariance v -> st { uiPlateHeightVariance = clamp01 v }
  SetPlateHardnessBase v -> st { uiPlateHardnessBase = clamp01 v }
  SetPlateHardnessVariance v -> st { uiPlateHardnessVariance = clamp01 v }
  SetUplift v          -> st { uiUplift = clamp01 v }
  SetRiftDepth v       -> st { uiRiftDepth = clamp01 v }
  SetTrenchDepth v     -> st { uiTrenchDepth = clamp01 v }
  SetRidgeHeight v     -> st { uiRidgeHeight = clamp01 v }
  SetDetailScale v     -> st { uiDetailScale = clamp01 v }
  SetPlateBiasStrength v -> st { uiPlateBiasStrength = clamp01 v }
  SetPlateBiasCenter v -> st { uiPlateBiasCenter = clamp01 v }
  SetPlateBiasEdge v   -> st { uiPlateBiasEdge = clamp01 v }
  SetPlateBiasNorth v  -> st { uiPlateBiasNorth = clamp01 v }
  SetPlateBiasSouth v  -> st { uiPlateBiasSouth = clamp01 v }
  SetTfcCliffSlope v  -> st { uiTfcCliffSlope = clamp01 v }
  SetTfcMountainSlope v -> st { uiTfcMountainSlope = clamp01 v }
  SetTfcMountainRelief v -> st { uiTfcMountainRelief = clamp01 v }
  SetTfcHillSlope v   -> st { uiTfcHillSlope = clamp01 v }
  SetTfcRollingSlope v -> st { uiTfcRollingSlope = clamp01 v }
  SetValleyCurvature v -> st { uiValleyCurvature = clamp01 v }
  SetRockElevationThreshold v -> st { uiRockElevationThreshold = clamp01 v }
  SetRockHardnessThreshold v -> st { uiRockHardnessThreshold = clamp01 v }
  SetRockHardnessSecondary v -> st { uiRockHardnessSecondary = clamp01 v }
  SetWindIterations v  -> st { uiWindIterations = clamp01 v }
  SetMoistureIterations v -> st { uiMoistureIterations = clamp01 v }
  SetBoundaryMotionTemp v -> st { uiBoundaryMotionTemp = clamp01 v }
  SetBoundaryMotionPrecip v -> st { uiBoundaryMotionPrecip = clamp01 v }
  SetWeatherTick v     -> st { uiWeatherTick = clamp01 v }
  SetWeatherPhase v    -> st { uiWeatherPhase = clamp01 v }
  SetWeatherAmplitude v -> st { uiWeatherAmplitude = clamp01 v }
  SetSeasonCycleLength v -> st { uiSeasonCycleLength = clamp01 v }
  SetJitterAmplitude v -> st { uiJitterAmplitude = clamp01 v }
  SetPressureBase v    -> st { uiPressureBase = clamp01 v }
  SetPressureTempScale v -> st { uiPressureTempScale = clamp01 v }
  SetPressureCoriolisScale v -> st { uiPressureCoriolisScale = clamp01 v }
  SetSeasonalBase v    -> st { uiSeasonalBase = clamp01 v }
  SetSeasonalRange v   -> st { uiSeasonalRange = clamp01 v }
  SetHumidityNoiseScale v -> st { uiHumidityNoiseScale = clamp01 v }
  SetPrecipNoiseScale v -> st { uiPrecipNoiseScale = clamp01 v }
  SetWeatherITCZWidth v -> st { uiWeatherITCZWidth = clamp01 v }
  SetWeatherITCZPrecipBoost v -> st { uiWeatherITCZPrecipBoost = clamp01 v }
  SetPressureHumidityScale v -> st { uiPressureHumidityScale = clamp01 v }
  SetPressureGradientWindScale v -> st { uiPressureGradientWindScale = clamp01 v }
  SetWindNoiseScale v  -> st { uiWindNoiseScale = clamp01 v }
  SetITCZMigrationScale v -> st { uiITCZMigrationScale = clamp01 v }
  SetCloudRHExponent v -> st { uiCloudRHExponent = clamp01 v }
  SetCloudAlbedoEffect v -> st { uiCloudAlbedoEffect = clamp01 v }
  SetCloudPrecipBoost v -> st { uiCloudPrecipBoost = clamp01 v }
  SetVegBase v         -> st { uiVegBase = clamp01 v }
  SetVegBoost v        -> st { uiVegBoost = clamp01 v }
  SetVegTempWeight v   -> st { uiVegTempWeight = clamp01 v }
  SetVegPrecipWeight v -> st { uiVegPrecipWeight = clamp01 v }
  SetBtCoastalBand v -> st { uiBtCoastalBand = clamp01 v }
  SetBtSnowElevation v -> st { uiBtSnowElevation = clamp01 v }
  SetBtAlpineElevation v -> st { uiBtAlpineElevation = clamp01 v }
  SetBtIceCapTemp v -> st { uiBtIceCapTemp = clamp01 v }
  SetBtMontaneLow v -> st { uiBtMontaneLow = clamp01 v }
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
  SetPlanetRadius v    -> st { uiPlanetRadius = clamp01 v }
  SetAxialTilt v       -> st { uiAxialTilt = clamp01 v }
  SetInsolation v      -> st { uiInsolation = clamp01 v }
  SetOccWarmScale v    -> st { uiOccWarmScale = clamp01 v }
  SetOccColdScale v    -> st { uiOccColdScale = clamp01 v }
  SetOccLatPeakDeg v   -> st { uiOccLatPeakDeg = clamp01 v }
  SetOccLatWidthDeg v  -> st { uiOccLatWidthDeg = clamp01 v }
  SetSliceLatCenter v  -> st { uiSliceLatCenter = clamp01 v }
  SetSliceLonCenter v  -> st { uiSliceLonCenter = clamp01 v }
  SetHoverHex v        -> st { uiHoverHex = v }
  SetHoverWidget v     -> st { uiHoverWidget = v }
  SetWorldConfig v     -> st { uiWorldConfig = v }
  SetWorldName v       -> st { uiWorldName = v }
  SetWorldSaveInput v  -> st { uiWorldSaveInput = v }
  SetWorldList v       -> st { uiWorldList = v }
  SetWorldSelected v   -> st { uiWorldSelected = v }

uiSnapshotTag :: OpTag "uiSnapshot"
uiSnapshotTag = OpTag

[hyperspace|
-- | Reply protocol for emitting UI snapshots.
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

setUiSeed :: ActorHandle Ui (Protocol Ui) -> Word64 -> IO ()
setUiSeed handle seed =
  cast @"update" handle #update (SetSeed seed)

setUiGenerating :: ActorHandle Ui (Protocol Ui) -> Bool -> IO ()
setUiGenerating handle flag =
  cast @"update" handle #update (SetGenerating flag)

setUiViewMode :: ActorHandle Ui (Protocol Ui) -> ViewMode -> IO ()
setUiViewMode handle mode =
  cast @"update" handle #update (SetViewMode mode)

setUiChunkSize :: ActorHandle Ui (Protocol Ui) -> Int -> IO ()
setUiChunkSize handle size =
  cast @"update" handle #update (SetChunkSize size)

setUiShowConfig :: ActorHandle Ui (Protocol Ui) -> Bool -> IO ()
setUiShowConfig handle flag =
  cast @"update" handle #update (SetShowConfig flag)

setUiShowLeftPanel :: ActorHandle Ui (Protocol Ui) -> Bool -> IO ()
setUiShowLeftPanel handle flag =
  cast @"update" handle #update (SetShowLeftPanel flag)

setUiConfigTab :: ActorHandle Ui (Protocol Ui) -> ConfigTab -> IO ()
setUiConfigTab handle tab =
  cast @"update" handle #update (SetConfigTab tab)

setUiConfigScroll :: ActorHandle Ui (Protocol Ui) -> Int -> IO ()
setUiConfigScroll handle newScroll =
  cast @"update" handle #update (SetConfigScroll newScroll)

setUiLeftTab :: ActorHandle Ui (Protocol Ui) -> LeftTab -> IO ()
setUiLeftTab handle tab =
  cast @"update" handle #update (SetLeftTab tab)

-- | Set the active modal overlay mode.
setUiMenuMode :: ActorHandle Ui (Protocol Ui) -> UiMenuMode -> IO ()
setUiMenuMode handle mode =
  cast @"update" handle #update (SetMenuMode mode)

-- | Set the text buffer for the preset save dialog.
setUiPresetInput :: ActorHandle Ui (Protocol Ui) -> Text -> IO ()
setUiPresetInput handle txt =
  cast @"update" handle #update (SetPresetInput txt)

-- | Set the cached list of preset names for the preset load dialog.
setUiPresetList :: ActorHandle Ui (Protocol Ui) -> [Text] -> IO ()
setUiPresetList handle names =
  cast @"update" handle #update (SetPresetList names)

-- | Set the selected preset index in the load dialog.
setUiPresetSelected :: ActorHandle Ui (Protocol Ui) -> Int -> IO ()
setUiPresetSelected handle idx =
  cast @"update" handle #update (SetPresetSelected idx)

-- | Set the captured world config for revert support.
setUiWorldConfig :: ActorHandle Ui (Protocol Ui) -> Maybe ConfigPreset -> IO ()
setUiWorldConfig handle cfg =
  cast @"update" handle #update (SetWorldConfig cfg)

-- | Set the world name displayed in the top bar.
setUiWorldName :: ActorHandle Ui (Protocol Ui) -> Text -> IO ()
setUiWorldName handle name =
  cast @"update" handle #update (SetWorldName name)

-- | Set the text buffer for the world save dialog.
setUiWorldSaveInput :: ActorHandle Ui (Protocol Ui) -> Text -> IO ()
setUiWorldSaveInput handle txt =
  cast @"update" handle #update (SetWorldSaveInput txt)

-- | Set the cached list of world manifests for the world load dialog.
setUiWorldList :: ActorHandle Ui (Protocol Ui) -> [WorldSaveManifest] -> IO ()
setUiWorldList handle worlds =
  cast @"update" handle #update (SetWorldList worlds)

-- | Set the selected world index in the load dialog.
setUiWorldSelected :: ActorHandle Ui (Protocol Ui) -> Int -> IO ()
setUiWorldSelected handle idx =
  cast @"update" handle #update (SetWorldSelected idx)

setUiContextHex :: ActorHandle Ui (Protocol Ui) -> Maybe (Int, Int) -> IO ()
setUiContextHex handle hex =
  cast @"update" handle #update (SetContextHex hex)

setUiContextPos :: ActorHandle Ui (Protocol Ui) -> Maybe (Int, Int) -> IO ()
setUiContextPos handle pos =
  cast @"update" handle #update (SetContextPos pos)

setUiSeedEditing :: ActorHandle Ui (Protocol Ui) -> Bool -> IO ()
setUiSeedEditing handle flag =
  cast @"update" handle #update (SetSeedEditing flag)

setUiSeedInput :: ActorHandle Ui (Protocol Ui) -> Text -> IO ()
setUiSeedInput handle input =
  cast @"update" handle #update (SetSeedInput input)

setUiWaterLevel :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiWaterLevel handle value =
  cast @"update" handle #update (SetWaterLevel value)

-- | Commit the render water level (used in 'AtlasKey').
--
-- Only called on Apply\/Generate to avoid atlas invalidation on +/- clicks.
setUiRenderWaterLevel :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiRenderWaterLevel handle value =
  cast @"update" handle #update (SetRenderWaterLevel value)

setUiEvaporation :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiEvaporation handle value =
  cast @"update" handle #update (SetEvaporation value)

setUiRainShadow :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiRainShadow handle value =
  cast @"update" handle #update (SetRainShadow value)

setUiWindDiffuse :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiWindDiffuse handle value =
  cast @"update" handle #update (SetWindDiffuse value)

setUiRainRate :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiRainRate handle value =
  cast @"update" handle #update (SetRainRate value)

setUiErosionHydraulic :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiErosionHydraulic handle value =
  cast @"update" handle #update (SetErosionHydraulic value)

setUiErosionThermal :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiErosionThermal handle value =
  cast @"update" handle #update (SetErosionThermal value)

setUiErosionTalus :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiErosionTalus handle value =
  cast @"update" handle #update (SetErosionTalus value)

setUiErosionMaxDrop :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiErosionMaxDrop handle value =
  cast @"update" handle #update (SetErosionMaxDrop value)

setUiGlacierSnowTemp :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiGlacierSnowTemp handle value =
  cast @"update" handle #update (SetGlacierSnowTemp value)

setUiGlacierSnowRange :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiGlacierSnowRange handle value =
  cast @"update" handle #update (SetGlacierSnowRange value)

setUiGlacierMeltTemp :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiGlacierMeltTemp handle value =
  cast @"update" handle #update (SetGlacierMeltTemp value)

setUiGlacierMeltRate :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiGlacierMeltRate handle value =
  cast @"update" handle #update (SetGlacierMeltRate value)

setUiGlacierAccumScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiGlacierAccumScale handle value =
  cast @"update" handle #update (SetGlacierAccumScale value)

setUiGlacierFlowIters :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiGlacierFlowIters handle value =
  cast @"update" handle #update (SetGlacierFlowIters value)

setUiGlacierFlowRate :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiGlacierFlowRate handle value =
  cast @"update" handle #update (SetGlacierFlowRate value)

setUiGlacierErosionScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiGlacierErosionScale handle value =
  cast @"update" handle #update (SetGlacierErosionScale value)

setUiGlacierCarveScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiGlacierCarveScale handle value =
  cast @"update" handle #update (SetGlacierCarveScale value)

setUiGlacierDepositScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiGlacierDepositScale handle value =
  cast @"update" handle #update (SetGlacierDepositScale value)

setUiVentDensity :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiVentDensity handle value =
  cast @"update" handle #update (SetVentDensity value)

setUiVentThreshold :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiVentThreshold handle value =
  cast @"update" handle #update (SetVentThreshold value)

setUiHotspotScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiHotspotScale handle value =
  cast @"update" handle #update (SetHotspotScale value)

setUiHotspotThreshold :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiHotspotThreshold handle value =
  cast @"update" handle #update (SetHotspotThreshold value)

setUiMagmaRecharge :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiMagmaRecharge handle value =
  cast @"update" handle #update (SetMagmaRecharge value)

setUiLavaScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiLavaScale handle value =
  cast @"update" handle #update (SetLavaScale value)

setUiAshScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiAshScale handle value =
  cast @"update" handle #update (SetAshScale value)

setUiVolcanicDepositScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiVolcanicDepositScale handle value =
  cast @"update" handle #update (SetVolcanicDepositScale value)

setUiSoilMoistureThreshold :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiSoilMoistureThreshold handle value =
  cast @"update" handle #update (SetSoilMoistureThreshold value)

setUiSoilHardnessThreshold :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiSoilHardnessThreshold handle value =
  cast @"update" handle #update (SetSoilHardnessThreshold value)

setUiSoilFertilityMoistWeight :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiSoilFertilityMoistWeight handle value =
  cast @"update" handle #update (SetSoilFertilityMoistWeight value)

setUiSoilFertilityDepthWeight :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiSoilFertilityDepthWeight handle value =
  cast @"update" handle #update (SetSoilFertilityDepthWeight value)

setUiSinkBreachDepth :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiSinkBreachDepth handle value =
  cast @"update" handle #update (SetSinkBreachDepth value)

setUiStreamPowerMaxErosion :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiStreamPowerMaxErosion handle value =
  cast @"update" handle #update (SetStreamPowerMaxErosion value)

setUiRiverCarveMaxDepth :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiRiverCarveMaxDepth handle value =
  cast @"update" handle #update (SetRiverCarveMaxDepth value)

setUiCoastalErodeStrength :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiCoastalErodeStrength handle value =
  cast @"update" handle #update (SetCoastalErodeStrength value)

setUiHydroHardnessWeight :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiHydroHardnessWeight handle value =
  cast @"update" handle #update (SetHydroHardnessWeight value)

setUiMinLakeSize :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiMinLakeSize handle value =
  cast @"update" handle #update (SetMinLakeSize value)

setUiInlandSeaMinSize :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiInlandSeaMinSize handle value =
  cast @"update" handle #update (SetInlandSeaMinSize value)

setUiRoughnessScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiRoughnessScale handle value =
  cast @"update" handle #update (SetRoughnessScale value)

setUiEquatorTemp :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiEquatorTemp handle value =
  cast @"update" handle #update (SetEquatorTemp value)

setUiPoleTemp :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPoleTemp handle value =
  cast @"update" handle #update (SetPoleTemp value)

setUiLapseRate :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiLapseRate handle value =
  cast @"update" handle #update (SetLapseRate value)

setUiLatitudeExponent :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiLatitudeExponent handle value =
  cast @"update" handle #update (SetLatitudeExponent value)

setUiPlateHeightCooling :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPlateHeightCooling handle value =
  cast @"update" handle #update (SetPlateHeightCooling value)

setUiTempNoiseScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiTempNoiseScale handle value =
  cast @"update" handle #update (SetTempNoiseScale value)

setUiOceanModeration :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiOceanModeration handle value =
  cast @"update" handle #update (SetOceanModeration value)

setUiOceanModerateTemp :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiOceanModerateTemp handle value =
  cast @"update" handle #update (SetOceanModerateTemp value)

setUiAlbedoSensitivity :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiAlbedoSensitivity handle value =
  cast @"update" handle #update (SetAlbedoSensitivity value)

setUiAlbedoReference :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiAlbedoReference handle value =
  cast @"update" handle #update (SetAlbedoReference value)

setUiMoistAdvect :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiMoistAdvect handle value =
  cast @"update" handle #update (SetMoistAdvect value)

setUiMoistLocal :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiMoistLocal handle value =
  cast @"update" handle #update (SetMoistLocal value)

setUiMoistWindEvapScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiMoistWindEvapScale handle value =
  cast @"update" handle #update (SetMoistWindEvapScale value)

setUiMoistEvapNoiseScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiMoistEvapNoiseScale handle value =
  cast @"update" handle #update (SetMoistEvapNoiseScale value)

setUiMoistLandETCoeff :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiMoistLandETCoeff handle value =
  cast @"update" handle #update (SetMoistLandETCoeff value)

setUiMoistBareEvapFrac :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiMoistBareEvapFrac handle value =
  cast @"update" handle #update (SetMoistBareEvapFrac value)

setUiMoistVegTranspFrac :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiMoistVegTranspFrac handle value =
  cast @"update" handle #update (SetMoistVegTranspFrac value)

setUiMoistWindETScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiMoistWindETScale handle value =
  cast @"update" handle #update (SetMoistWindETScale value)

setUiMoistCondensationRate :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiMoistCondensationRate handle value =
  cast @"update" handle #update (SetMoistCondensationRate value)

setUiMoistRecycleRate :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiMoistRecycleRate handle value =
  cast @"update" handle #update (SetMoistRecycleRate value)

setUiMoistITCZStrength :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiMoistITCZStrength handle value =
  cast @"update" handle #update (SetMoistITCZStrength value)

setUiMoistITCZWidth :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiMoistITCZWidth handle value =
  cast @"update" handle #update (SetMoistITCZWidth value)

setUiOrographicScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiOrographicScale handle value =
  cast @"update" handle #update (SetOrographicScale value)

setUiOrographicStep :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiOrographicStep handle value =
  cast @"update" handle #update (SetOrographicStep value)

setUiCoastalIterations :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiCoastalIterations handle value =
  cast @"update" handle #update (SetCoastalIterations value)

setUiCoastalDiffuse :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiCoastalDiffuse handle value =
  cast @"update" handle #update (SetCoastalDiffuse value)

setUiCoastalMoistureBoost :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiCoastalMoistureBoost handle value =
  cast @"update" handle #update (SetCoastalMoistureBoost value)

setUiWindBeltStrength :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiWindBeltStrength handle value =
  cast @"update" handle #update (SetWindBeltStrength value)

setUiWindBeltHarmonics :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiWindBeltHarmonics handle value =
  cast @"update" handle #update (SetWindBeltHarmonics value)

setUiWindBeltBase :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiWindBeltBase handle value =
  cast @"update" handle #update (SetWindBeltBase value)

setUiWindBeltRange :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiWindBeltRange handle value =
  cast @"update" handle #update (SetWindBeltRange value)

setUiWindBeltSpeedScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiWindBeltSpeedScale handle value =
  cast @"update" handle #update (SetWindBeltSpeedScale value)

setUiBndLandRange :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiBndLandRange handle value =
  cast @"update" handle #update (SetBndLandRange value)

setUiBndTempConvergent :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiBndTempConvergent handle value =
  cast @"update" handle #update (SetBndTempConvergent value)

setUiBndTempDivergent :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiBndTempDivergent handle value =
  cast @"update" handle #update (SetBndTempDivergent value)

setUiBndTempTransform :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiBndTempTransform handle value =
  cast @"update" handle #update (SetBndTempTransform value)

setUiBndPrecipConvergent :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiBndPrecipConvergent handle value =
  cast @"update" handle #update (SetBndPrecipConvergent value)

setUiBndPrecipDivergent :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiBndPrecipDivergent handle value =
  cast @"update" handle #update (SetBndPrecipDivergent value)

setUiBndPrecipTransform :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiBndPrecipTransform handle value =
  cast @"update" handle #update (SetBndPrecipTransform value)

setUiGenScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiGenScale handle value =
  cast @"update" handle #update (SetGenScale value)

setUiGenCoordScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiGenCoordScale handle value =
  cast @"update" handle #update (SetGenCoordScale value)

setUiGenOffsetX :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiGenOffsetX handle value =
  cast @"update" handle #update (SetGenOffsetX value)

setUiGenOffsetY :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiGenOffsetY handle value =
  cast @"update" handle #update (SetGenOffsetY value)

setUiGenFrequency :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiGenFrequency handle value =
  cast @"update" handle #update (SetGenFrequency value)

setUiGenOctaves :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiGenOctaves handle value =
  cast @"update" handle #update (SetGenOctaves value)

setUiGenLacunarity :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiGenLacunarity handle value =
  cast @"update" handle #update (SetGenLacunarity value)

setUiGenGain :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiGenGain handle value =
  cast @"update" handle #update (SetGenGain value)

setUiGenWarpScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiGenWarpScale handle value =
  cast @"update" handle #update (SetGenWarpScale value)

setUiGenWarpStrength :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiGenWarpStrength handle value =
  cast @"update" handle #update (SetGenWarpStrength value)

setUiWorldExtentX :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiWorldExtentX handle value =
  cast @"update" handle #update (SetWorldExtentX value)

setUiWorldExtentY :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiWorldExtentY handle value =
  cast @"update" handle #update (SetWorldExtentY value)

setUiEdgeDepthNorth :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiEdgeDepthNorth handle value =
  cast @"update" handle #update (SetEdgeDepthNorth value)

setUiEdgeDepthSouth :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiEdgeDepthSouth handle value =
  cast @"update" handle #update (SetEdgeDepthSouth value)

setUiEdgeDepthEast :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiEdgeDepthEast handle value =
  cast @"update" handle #update (SetEdgeDepthEast value)

setUiEdgeDepthWest :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiEdgeDepthWest handle value =
  cast @"update" handle #update (SetEdgeDepthWest value)

setUiEdgeDepthFalloff :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiEdgeDepthFalloff handle value =
  cast @"update" handle #update (SetEdgeDepthFalloff value)

setUiPanOffset :: ActorHandle Ui (Protocol Ui) -> (Float, Float) -> IO ()
setUiPanOffset handle value =
  cast @"update" handle #update (SetPanOffset value)

setUiZoom :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiZoom handle value =
  cast @"update" handle #update (SetZoom value)

setUiPlateSize :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPlateSize handle value =
  cast @"update" handle #update (SetPlateSize value)

setUiPlateSpeed :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPlateSpeed handle value =
  cast @"update" handle #update (SetPlateSpeed value)

setUiBoundarySharpness :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiBoundarySharpness handle value =
  cast @"update" handle #update (SetBoundarySharpness value)

setUiBoundaryNoiseScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiBoundaryNoiseScale handle value =
  cast @"update" handle #update (SetBoundaryNoiseScale value)

setUiBoundaryNoiseStrength :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiBoundaryNoiseStrength handle value =
  cast @"update" handle #update (SetBoundaryNoiseStrength value)

setUiBoundaryWarpOctaves :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiBoundaryWarpOctaves handle value =
  cast @"update" handle #update (SetBoundaryWarpOctaves value)

setUiBoundaryWarpLacunarity :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiBoundaryWarpLacunarity handle value =
  cast @"update" handle #update (SetBoundaryWarpLacunarity value)

setUiBoundaryWarpGain :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiBoundaryWarpGain handle value =
  cast @"update" handle #update (SetBoundaryWarpGain value)

setUiPlateMergeScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPlateMergeScale handle value =
  cast @"update" handle #update (SetPlateMergeScale value)

setUiPlateMergeBias :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPlateMergeBias handle value =
  cast @"update" handle #update (SetPlateMergeBias value)

setUiPlateDetailScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPlateDetailScale handle value =
  cast @"update" handle #update (SetPlateDetailScale value)

setUiPlateDetailStrength :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPlateDetailStrength handle value =
  cast @"update" handle #update (SetPlateDetailStrength value)

setUiPlateRidgeStrength :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPlateRidgeStrength handle value =
  cast @"update" handle #update (SetPlateRidgeStrength value)

setUiPlateHeightBase :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPlateHeightBase handle value =
  cast @"update" handle #update (SetPlateHeightBase value)

setUiPlateHeightVariance :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPlateHeightVariance handle value =
  cast @"update" handle #update (SetPlateHeightVariance value)

setUiPlateHardnessBase :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPlateHardnessBase handle value =
  cast @"update" handle #update (SetPlateHardnessBase value)

setUiPlateHardnessVariance :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPlateHardnessVariance handle value =
  cast @"update" handle #update (SetPlateHardnessVariance value)

setUiUplift :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiUplift handle value =
  cast @"update" handle #update (SetUplift value)

setUiRiftDepth :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiRiftDepth handle value =
  cast @"update" handle #update (SetRiftDepth value)

setUiTrenchDepth :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiTrenchDepth handle value =
  cast @"update" handle #update (SetTrenchDepth value)

setUiRidgeHeight :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiRidgeHeight handle value =
  cast @"update" handle #update (SetRidgeHeight value)

setUiDetailScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiDetailScale handle value =
  cast @"update" handle #update (SetDetailScale value)

setUiPlateBiasStrength :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPlateBiasStrength handle value =
  cast @"update" handle #update (SetPlateBiasStrength value)

setUiPlateBiasCenter :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPlateBiasCenter handle value =
  cast @"update" handle #update (SetPlateBiasCenter value)

setUiPlateBiasEdge :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPlateBiasEdge handle value =
  cast @"update" handle #update (SetPlateBiasEdge value)

setUiPlateBiasNorth :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPlateBiasNorth handle value =
  cast @"update" handle #update (SetPlateBiasNorth value)

setUiPlateBiasSouth :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPlateBiasSouth handle value =
  cast @"update" handle #update (SetPlateBiasSouth value)

setUiTfcCliffSlope :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiTfcCliffSlope handle value =
  cast @"update" handle #update (SetTfcCliffSlope value)

setUiTfcMountainSlope :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiTfcMountainSlope handle value =
  cast @"update" handle #update (SetTfcMountainSlope value)

setUiTfcMountainRelief :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiTfcMountainRelief handle value =
  cast @"update" handle #update (SetTfcMountainRelief value)

setUiTfcHillSlope :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiTfcHillSlope handle value =
  cast @"update" handle #update (SetTfcHillSlope value)

setUiTfcRollingSlope :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiTfcRollingSlope handle value =
  cast @"update" handle #update (SetTfcRollingSlope value)

setUiValleyCurvature :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiValleyCurvature handle value =
  cast @"update" handle #update (SetValleyCurvature value)

setUiRockElevationThreshold :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiRockElevationThreshold handle value =
  cast @"update" handle #update (SetRockElevationThreshold value)

setUiRockHardnessThreshold :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiRockHardnessThreshold handle value =
  cast @"update" handle #update (SetRockHardnessThreshold value)

setUiRockHardnessSecondary :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiRockHardnessSecondary handle value =
  cast @"update" handle #update (SetRockHardnessSecondary value)

setUiWindIterations :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiWindIterations handle value =
  cast @"update" handle #update (SetWindIterations value)

setUiMoistureIterations :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiMoistureIterations handle value =
  cast @"update" handle #update (SetMoistureIterations value)

setUiBoundaryMotionTemp :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiBoundaryMotionTemp handle value =
  cast @"update" handle #update (SetBoundaryMotionTemp value)

setUiBoundaryMotionPrecip :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiBoundaryMotionPrecip handle value =
  cast @"update" handle #update (SetBoundaryMotionPrecip value)

setUiWeatherTick :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiWeatherTick handle value =
  cast @"update" handle #update (SetWeatherTick value)

setUiWeatherPhase :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiWeatherPhase handle value =
  cast @"update" handle #update (SetWeatherPhase value)

setUiWeatherAmplitude :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiWeatherAmplitude handle value =
  cast @"update" handle #update (SetWeatherAmplitude value)

setUiSeasonCycleLength :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiSeasonCycleLength handle value =
  cast @"update" handle #update (SetSeasonCycleLength value)

setUiJitterAmplitude :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiJitterAmplitude handle value =
  cast @"update" handle #update (SetJitterAmplitude value)

setUiPressureBase :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPressureBase handle value =
  cast @"update" handle #update (SetPressureBase value)

setUiPressureTempScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPressureTempScale handle value =
  cast @"update" handle #update (SetPressureTempScale value)

setUiPressureCoriolisScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPressureCoriolisScale handle value =
  cast @"update" handle #update (SetPressureCoriolisScale value)

setUiSeasonalBase :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiSeasonalBase handle value =
  cast @"update" handle #update (SetSeasonalBase value)

setUiSeasonalRange :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiSeasonalRange handle value =
  cast @"update" handle #update (SetSeasonalRange value)

setUiHumidityNoiseScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiHumidityNoiseScale handle value =
  cast @"update" handle #update (SetHumidityNoiseScale value)

setUiPrecipNoiseScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPrecipNoiseScale handle value =
  cast @"update" handle #update (SetPrecipNoiseScale value)

setUiWeatherITCZWidth :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiWeatherITCZWidth handle value =
  cast @"update" handle #update (SetWeatherITCZWidth value)

setUiWeatherITCZPrecipBoost :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiWeatherITCZPrecipBoost handle value =
  cast @"update" handle #update (SetWeatherITCZPrecipBoost value)

setUiPressureHumidityScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPressureHumidityScale handle value =
  cast @"update" handle #update (SetPressureHumidityScale value)

setUiPressureGradientWindScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPressureGradientWindScale handle value =
  cast @"update" handle #update (SetPressureGradientWindScale value)

setUiWindNoiseScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiWindNoiseScale handle value =
  cast @"update" handle #update (SetWindNoiseScale value)

setUiITCZMigrationScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiITCZMigrationScale handle value =
  cast @"update" handle #update (SetITCZMigrationScale value)

setUiCloudRHExponent :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiCloudRHExponent handle value =
  cast @"update" handle #update (SetCloudRHExponent value)

setUiCloudAlbedoEffect :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiCloudAlbedoEffect handle value =
  cast @"update" handle #update (SetCloudAlbedoEffect value)

setUiCloudPrecipBoost :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiCloudPrecipBoost handle value =
  cast @"update" handle #update (SetCloudPrecipBoost value)

setUiVegBase :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiVegBase handle value =
  cast @"update" handle #update (SetVegBase value)

setUiVegBoost :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiVegBoost handle value =
  cast @"update" handle #update (SetVegBoost value)

setUiVegTempWeight :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiVegTempWeight handle value =
  cast @"update" handle #update (SetVegTempWeight value)

setUiVegPrecipWeight :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiVegPrecipWeight handle value =
  cast @"update" handle #update (SetVegPrecipWeight value)

-- | Set coastal band biome threshold (0–1 normalized).
setUiBtCoastalBand :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiBtCoastalBand handle value =
  cast @"update" handle #update (SetBtCoastalBand value)

-- | Set snow elevation biome threshold (0–1 normalized).
setUiBtSnowElevation :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiBtSnowElevation handle value =
  cast @"update" handle #update (SetBtSnowElevation value)

-- | Set alpine elevation biome threshold (0–1 normalized).
setUiBtAlpineElevation :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiBtAlpineElevation handle value =
  cast @"update" handle #update (SetBtAlpineElevation value)

-- | Set ice cap temperature biome threshold (0–1 normalized).
setUiBtIceCapTemp :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiBtIceCapTemp handle value =
  cast @"update" handle #update (SetBtIceCapTemp value)

-- | Set montane low elevation biome threshold (0–1 normalized).
setUiBtMontaneLow :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiBtMontaneLow handle value =
  cast @"update" handle #update (SetBtMontaneLow value)

-- | Set montane precipitation biome threshold (0–1 normalized).
setUiBtMontanePrecip :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiBtMontanePrecip handle value =
  cast @"update" handle #update (SetBtMontanePrecip value)

-- | Set cliff slope biome threshold (0–1 normalized).
setUiBtCliffSlope :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiBtCliffSlope handle value =
  cast @"update" handle #update (SetBtCliffSlope value)

-- | Set valley moisture biome threshold (0–1 normalized).
setUiBtValleyMoisture :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiBtValleyMoisture handle value =
  cast @"update" handle #update (SetBtValleyMoisture value)

-- | Set depression moisture biome threshold (0–1 normalized).
setUiBtDepressionMoisture :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiBtDepressionMoisture handle value =
  cast @"update" handle #update (SetBtDepressionMoisture value)

-- | Set precipitation weight for biome classification (0–1 normalized).
setUiBtPrecipWeight :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiBtPrecipWeight handle value =
  cast @"update" handle #update (SetBtPrecipWeight value)

-- | Set vegetation bootstrap minimum temperature (0–1 normalized).
setUiVbcTempMin :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiVbcTempMin handle value =
  cast @"update" handle #update (SetVbcTempMin value)

-- | Set vegetation bootstrap temperature range (0–1 normalized).
setUiVbcTempRange :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiVbcTempRange handle value =
  cast @"update" handle #update (SetVbcTempRange value)

-- | Set vegetation bootstrap fertility boost (0–1 normalized).
setUiVbcFertilityBoost :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiVbcFertilityBoost handle value =
  cast @"update" handle #update (SetVbcFertilityBoost value)

-- | Set base surface albedo (0–1 normalized).
setUiVbcAlbedoBase :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiVbcAlbedoBase handle value =
  cast @"update" handle #update (SetVbcAlbedoBase value)

-- | Set bare ground albedo (0–1 normalized).
setUiVbcAlbedoBare :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiVbcAlbedoBare handle value =
  cast @"update" handle #update (SetVbcAlbedoBare value)

-- | Set vegetation albedo reduction (0–1 normalized).
setUiVbcAlbedoVeg :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiVbcAlbedoVeg handle value =
  cast @"update" handle #update (SetVbcAlbedoVeg value)

-- | Set ocean surface albedo (0–1 normalized).
setUiVbcOceanAlbedo :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiVbcOceanAlbedo handle value =
  cast @"update" handle #update (SetVbcOceanAlbedo value)

-- | Set ice surface albedo (0–1 normalized).
setUiVbcIceAlbedo :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiVbcIceAlbedo handle value =
  cast @"update" handle #update (SetVbcIceAlbedo value)

-- | Set biome smoothing iterations (0–1 normalized, maps to 0–5).
setUiBiomeSmoothing :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiBiomeSmoothing handle value =
  cast @"update" handle #update (SetBiomeSmoothing value)

-- | Set volcanic ash fertility boost (0–1 normalized).
setUiVolcanicAshBoost :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiVolcanicAshBoost handle value =
  cast @"update" handle #update (SetVolcanicAshBoost value)

-- | Set volcanic lava vegetation penalty (0–1 normalized).
setUiVolcanicLavaPenalty :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiVolcanicLavaPenalty handle value =
  cast @"update" handle #update (SetVolcanicLavaPenalty value)

-- | Set biome feedback blend weight (0–1 normalized).
setUiBiomeFeedbackBlend :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiBiomeFeedbackBlend handle value =
  cast @"update" handle #update (SetBiomeFeedbackBlend value)

setUiPlanetRadius :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiPlanetRadius handle value =
  cast @"update" handle #update (SetPlanetRadius value)

setUiAxialTilt :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiAxialTilt handle value =
  cast @"update" handle #update (SetAxialTilt value)

setUiInsolation :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiInsolation handle value =
  cast @"update" handle #update (SetInsolation value)

setUiOccWarmScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiOccWarmScale handle value =
  cast @"update" handle #update (SetOccWarmScale value)

setUiOccColdScale :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiOccColdScale handle value =
  cast @"update" handle #update (SetOccColdScale value)

setUiOccLatPeakDeg :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiOccLatPeakDeg handle value =
  cast @"update" handle #update (SetOccLatPeakDeg value)

setUiOccLatWidthDeg :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiOccLatWidthDeg handle value =
  cast @"update" handle #update (SetOccLatWidthDeg value)

setUiSliceLatCenter :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiSliceLatCenter handle value =
  cast @"update" handle #update (SetSliceLatCenter value)

setUiSliceLonCenter :: ActorHandle Ui (Protocol Ui) -> Float -> IO ()
setUiSliceLonCenter handle value =
  cast @"update" handle #update (SetSliceLonCenter value)

setUiHoverHex :: ActorHandle Ui (Protocol Ui) -> Maybe (Int, Int) -> IO ()
setUiHoverHex handle hex =
  cast @"update" handle #update (SetHoverHex hex)

-- | Set the currently hovered widget (for tooltip display).
setUiHoverWidget :: ActorHandle Ui (Protocol Ui) -> Maybe WidgetId -> IO ()
setUiHoverWidget handle wid =
  cast @"update" handle #update (SetHoverWidget wid)

getUiSnapshot :: ActorHandle Ui (Protocol Ui) -> IO UiState
getUiSnapshot handle =
  call @"snapshot" handle #snapshot ()

-- | Request a UI snapshot via a reply-capable cast.
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
