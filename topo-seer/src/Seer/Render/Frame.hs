module Seer.Render.Frame
  ( renderFrame
  ) where

import Actor.AtlasResultBroker (AtlasResultRef)
import Actor.AtlasScheduleBroker (AtlasScheduleRef)
import Actor.AtlasScheduler (AtlasScheduler)
import Actor.Data (DataSnapshot(..), TerrainSnapshot(..))
import Actor.Log (Log, LogEntry(..), LogLevel(..), appendLog)
import Actor.Log (LogSnapshot(..))
import Actor.Render (RenderSnapshot(..))
import Actor.SnapshotReceiver (SnapshotVersion)
import Actor.UI (LeftTab(..), UiState(..), ViewMode(..))
import Control.Monad (when)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (isNothing)
import Data.Word (Word32, Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.IO (Handle, hPutStrLn, hFlush)
import Hyperspace.Actor (ActorHandle, Protocol)
import Linear (V2(..), V4(..))
import qualified SDL
import qualified Data.Text as Text
import Seer.Draw
  ( drawChunkControl
  , drawConfigPanel
  , drawHoverHex
  , drawLeftTabs
  , drawSeedControl
  , drawStatusBars
  , drawViewModeButtons
  , seedMaxDigits
  , viewColor
  )
import Seer.Render.Atlas
  ( AtlasTextureCache
  , drawAtlas
  , drainAtlasBuildResults
  , resolveAtlasTiles
  , scheduleAtlasBuilds
  , zoomTextureScale
  )
import Seer.Render.Terrain
  ( TerrainCache(..)
  , updateChunkTextures
  , drawTerrain
  )
import Seer.Render.Ui (drawUiOverlay)
import Seer.Timing (nsToMs, timedMs)
import UI.Font (FontCache)
import UI.Layout
import UI.TerrainCache (ChunkTextureCache(..), emptyChunkTextureCache)
import UI.TerrainAtlas (TerrainAtlasTile(..))
import UI.Widgets (Rect(..))
import UI.WidgetsDraw (rectToSDL)

-- | Render one UI frame and schedule atlas work if needed.
renderFrame
  :: SDL.Renderer
  -> SDL.Window
  -> SnapshotVersion
  -> RenderSnapshot
  -> TerrainCache
  -> ChunkTextureCache
  -> AtlasTextureCache
  -> ActorHandle Log (Protocol Log)
  -> ActorHandle AtlasScheduler (Protocol AtlasScheduler)
  -> AtlasScheduleRef
  -> AtlasResultRef
  -> Int
  -> Bool
  -> Bool
  -> Bool
  -> Word32
  -> Maybe FontCache
  -> Bool
  -> Handle
  -> IO (Bool, ChunkTextureCache, AtlasTextureCache, Bool)
renderFrame renderer window snapshotVersion snapshot terrainCache textureCache atlasCache logHandle atlasSchedulerHandle scheduleRef resultRef atlasUploadsPerFrame shouldDrainAtlas shouldScheduleAtlas shouldUpdateChunkTextures timingLogThresholdMs fontCache renderTargetOk traceH = do
  tStart <- getMonotonicTimeNSec
  SDL.rendererDrawBlendMode renderer SDL.$= SDL.BlendAlphaBlend
  ((V2 winW winH), windowSizeElapsed) <- timedMs (SDL.get (SDL.windowSize window))
  loggedWindowSize <- logTiming logHandle timingLogThresholdMs (Text.pack "window size") windowSizeElapsed Nothing
  let UiState { uiViewMode = mode } = rsUi snapshot
      logSnap = rsLog snapshot
      dataSnap = rsData snapshot
      terrainSnap = rsTerrain snapshot
      generating = uiGenerating (rsUi snapshot)
      (r, g, b) = viewColor mode (dsTerrainChunks dataSnap) (dsBiomeChunks dataSnap)
      logHeight = if lsCollapsed logSnap then 24 else 160
      seedWidth = max 120 (seedMaxDigits * 10)
      layout = layoutForSeed (V2 (fromIntegral winW) (fromIntegral winH)) logHeight seedWidth
      Rect (V2 _ panelY, V2 _ panelH) = logPanelRect layout
      buttonRect = leftGenButtonRect layout
      configToggle = configToggleRect layout
      configPanel = configPanelRect layout
      (tabTerrain, tabPlanet, tabClimate, tabWeather, tabBiome, tabErosion) = configTabRects layout
      configPresetSave = configPresetSaveRect layout
      configPresetLoad = configPresetLoadRect layout
      configReset = configResetRect layout
      configRevert = configRevertRect layout
      configScrollArea = configScrollAreaRect layout
      configScrollBar = configScrollBarRect layout
      configWaterMinus = configWaterMinusRect layout
      configWaterPlus = configWaterPlusRect layout
      configWaterBar = configWaterBarRect layout
      configEvapMinus = configEvapMinusRect layout
      configEvapPlus = configEvapPlusRect layout
      configEvapBar = configEvapBarRect layout
      configRainShadowMinus = configRainShadowMinusRect layout
      configRainShadowPlus = configRainShadowPlusRect layout
      configRainShadowBar = configRainShadowBarRect layout
      configWindDiffuseMinus = configWindDiffuseMinusRect layout
      configWindDiffusePlus = configWindDiffusePlusRect layout
      configWindDiffuseBar = configWindDiffuseBarRect layout
      configErosionHydraulicMinus = configErosionHydraulicMinusRect layout
      configErosionHydraulicPlus = configErosionHydraulicPlusRect layout
      configErosionHydraulicBar = configErosionHydraulicBarRect layout
      configErosionThermalMinus = configErosionThermalMinusRect layout
      configErosionThermalPlus = configErosionThermalPlusRect layout
      configErosionThermalBar = configErosionThermalBarRect layout
      configErosionRainRateMinus = configErosionRainRateMinusRect layout
      configErosionRainRatePlus = configErosionRainRatePlusRect layout
      configErosionRainRateBar = configErosionRainRateBarRect layout
      configErosionTalusMinus = configErosionTalusMinusRect layout
      configErosionTalusPlus = configErosionTalusPlusRect layout
      configErosionTalusBar = configErosionTalusBarRect layout
      configErosionMaxDropMinus = configErosionMaxDropMinusRect layout
      configErosionMaxDropPlus = configErosionMaxDropPlusRect layout
      configErosionMaxDropBar = configErosionMaxDropBarRect layout
      configGlacierSnowTempMinus = configGlacierSnowTempMinusRect layout
      configGlacierSnowTempPlus = configGlacierSnowTempPlusRect layout
      configGlacierSnowTempBar = configGlacierSnowTempBarRect layout
      configGlacierSnowRangeMinus = configGlacierSnowRangeMinusRect layout
      configGlacierSnowRangePlus = configGlacierSnowRangePlusRect layout
      configGlacierSnowRangeBar = configGlacierSnowRangeBarRect layout
      configGlacierMeltTempMinus = configGlacierMeltTempMinusRect layout
      configGlacierMeltTempPlus = configGlacierMeltTempPlusRect layout
      configGlacierMeltTempBar = configGlacierMeltTempBarRect layout
      configGlacierMeltRateMinus = configGlacierMeltRateMinusRect layout
      configGlacierMeltRatePlus = configGlacierMeltRatePlusRect layout
      configGlacierMeltRateBar = configGlacierMeltRateBarRect layout
      configGlacierAccumScaleMinus = configGlacierAccumScaleMinusRect layout
      configGlacierAccumScalePlus = configGlacierAccumScalePlusRect layout
      configGlacierAccumScaleBar = configGlacierAccumScaleBarRect layout
      configGlacierFlowItersMinus = configGlacierFlowItersMinusRect layout
      configGlacierFlowItersPlus = configGlacierFlowItersPlusRect layout
      configGlacierFlowItersBar = configGlacierFlowItersBarRect layout
      configGlacierFlowRateMinus = configGlacierFlowRateMinusRect layout
      configGlacierFlowRatePlus = configGlacierFlowRatePlusRect layout
      configGlacierFlowRateBar = configGlacierFlowRateBarRect layout
      configGlacierErosionScaleMinus = configGlacierErosionScaleMinusRect layout
      configGlacierErosionScalePlus = configGlacierErosionScalePlusRect layout
      configGlacierErosionScaleBar = configGlacierErosionScaleBarRect layout
      configGlacierCarveScaleMinus = configGlacierCarveScaleMinusRect layout
      configGlacierCarveScalePlus = configGlacierCarveScalePlusRect layout
      configGlacierCarveScaleBar = configGlacierCarveScaleBarRect layout
      configGlacierDepositScaleMinus = configGlacierDepositScaleMinusRect layout
      configGlacierDepositScalePlus = configGlacierDepositScalePlusRect layout
      configGlacierDepositScaleBar = configGlacierDepositScaleBarRect layout
      configVentDensityMinus = configVentDensityMinusRect layout
      configVentDensityPlus = configVentDensityPlusRect layout
      configVentDensityBar = configVentDensityBarRect layout
      configVentThresholdMinus = configVentThresholdMinusRect layout
      configVentThresholdPlus = configVentThresholdPlusRect layout
      configVentThresholdBar = configVentThresholdBarRect layout
      configHotspotScaleMinus = configHotspotScaleMinusRect layout
      configHotspotScalePlus = configHotspotScalePlusRect layout
      configHotspotScaleBar = configHotspotScaleBarRect layout
      configHotspotThresholdMinus = configHotspotThresholdMinusRect layout
      configHotspotThresholdPlus = configHotspotThresholdPlusRect layout
      configHotspotThresholdBar = configHotspotThresholdBarRect layout
      configMagmaRechargeMinus = configMagmaRechargeMinusRect layout
      configMagmaRechargePlus = configMagmaRechargePlusRect layout
      configMagmaRechargeBar = configMagmaRechargeBarRect layout
      configLavaScaleMinus = configLavaScaleMinusRect layout
      configLavaScalePlus = configLavaScalePlusRect layout
      configLavaScaleBar = configLavaScaleBarRect layout
      configAshScaleMinus = configAshScaleMinusRect layout
      configAshScalePlus = configAshScalePlusRect layout
      configAshScaleBar = configAshScaleBarRect layout
      configVolcanicDepositScaleMinus = configVolcanicDepositScaleMinusRect layout
      configVolcanicDepositScalePlus = configVolcanicDepositScalePlusRect layout
      configVolcanicDepositScaleBar = configVolcanicDepositScaleBarRect layout
      configSoilMoistureThresholdMinus = configSoilMoistureThresholdMinusRect layout
      configSoilMoistureThresholdPlus = configSoilMoistureThresholdPlusRect layout
      configSoilMoistureThresholdBar = configSoilMoistureThresholdBarRect layout
      configSoilHardnessThresholdMinus = configSoilHardnessThresholdMinusRect layout
      configSoilHardnessThresholdPlus = configSoilHardnessThresholdPlusRect layout
      configSoilHardnessThresholdBar = configSoilHardnessThresholdBarRect layout
      configSoilFertilityMoistWeightMinus = configSoilFertilityMoistWeightMinusRect layout
      configSoilFertilityMoistWeightPlus = configSoilFertilityMoistWeightPlusRect layout
      configSoilFertilityMoistWeightBar = configSoilFertilityMoistWeightBarRect layout
      configSoilFertilityDepthWeightMinus = configSoilFertilityDepthWeightMinusRect layout
      configSoilFertilityDepthWeightPlus = configSoilFertilityDepthWeightPlusRect layout
      configSoilFertilityDepthWeightBar = configSoilFertilityDepthWeightBarRect layout
      configSinkBreachDepthMinus = configSinkBreachDepthMinusRect layout
      configSinkBreachDepthPlus = configSinkBreachDepthPlusRect layout
      configSinkBreachDepthBar = configSinkBreachDepthBarRect layout
      configStreamPowerMaxErosionMinus = configStreamPowerMaxErosionMinusRect layout
      configStreamPowerMaxErosionPlus = configStreamPowerMaxErosionPlusRect layout
      configStreamPowerMaxErosionBar = configStreamPowerMaxErosionBarRect layout
      configRiverCarveMaxDepthMinus = configRiverCarveMaxDepthMinusRect layout
      configRiverCarveMaxDepthPlus = configRiverCarveMaxDepthPlusRect layout
      configRiverCarveMaxDepthBar = configRiverCarveMaxDepthBarRect layout
      configCoastalErodeStrengthMinus = configCoastalErodeStrengthMinusRect layout
      configCoastalErodeStrengthPlus = configCoastalErodeStrengthPlusRect layout
      configCoastalErodeStrengthBar = configCoastalErodeStrengthBarRect layout
      configHydroHardnessWeightMinus = configHydroHardnessWeightMinusRect layout
      configHydroHardnessWeightPlus = configHydroHardnessWeightPlusRect layout
      configHydroHardnessWeightBar = configHydroHardnessWeightBarRect layout
      configMinLakeSizeMinus = configMinLakeSizeMinusRect layout
      configMinLakeSizePlus = configMinLakeSizePlusRect layout
      configMinLakeSizeBar = configMinLakeSizeBarRect layout
      configInlandSeaMinSizeMinus = configInlandSeaMinSizeMinusRect layout
      configInlandSeaMinSizePlus = configInlandSeaMinSizePlusRect layout
      configInlandSeaMinSizeBar = configInlandSeaMinSizeBarRect layout
      configRoughnessScaleMinus = configRoughnessScaleMinusRect layout
      configRoughnessScalePlus = configRoughnessScalePlusRect layout
      configRoughnessScaleBar = configRoughnessScaleBarRect layout
      configEquatorTempMinus = configEquatorTempMinusRect layout
      configEquatorTempPlus = configEquatorTempPlusRect layout
      configEquatorTempBar = configEquatorTempBarRect layout
      configPoleTempMinus = configPoleTempMinusRect layout
      configPoleTempPlus = configPoleTempPlusRect layout
      configPoleTempBar = configPoleTempBarRect layout
      configLapseRateMinus = configLapseRateMinusRect layout
      configLapseRatePlus = configLapseRatePlusRect layout
      configLapseRateBar = configLapseRateBarRect layout
      configWindIterationsMinus = configWindIterationsMinusRect layout
      configWindIterationsPlus = configWindIterationsPlusRect layout
      configWindIterationsBar = configWindIterationsBarRect layout
      configMoistureIterationsMinus = configMoistureIterationsMinusRect layout
      configMoistureIterationsPlus = configMoistureIterationsPlusRect layout
      configMoistureIterationsBar = configMoistureIterationsBarRect layout
      configWeatherTickMinus = configWeatherTickMinusRect layout
      configWeatherTickPlus = configWeatherTickPlusRect layout
      configWeatherTickBar = configWeatherTickBarRect layout
      configWeatherPhaseMinus = configWeatherPhaseMinusRect layout
      configWeatherPhasePlus = configWeatherPhasePlusRect layout
      configWeatherPhaseBar = configWeatherPhaseBarRect layout
      configWeatherAmplitudeMinus = configWeatherAmplitudeMinusRect layout
      configWeatherAmplitudePlus = configWeatherAmplitudePlusRect layout
      configWeatherAmplitudeBar = configWeatherAmplitudeBarRect layout
      configSeasonCycleLengthMinus = configSeasonCycleLengthMinusRect layout
      configSeasonCycleLengthPlus = configSeasonCycleLengthPlusRect layout
      configSeasonCycleLengthBar = configSeasonCycleLengthBarRect layout
      configJitterAmplitudeMinus = configJitterAmplitudeMinusRect layout
      configJitterAmplitudePlus = configJitterAmplitudePlusRect layout
      configJitterAmplitudeBar = configJitterAmplitudeBarRect layout
      configPressureBaseMinus = configPressureBaseMinusRect layout
      configPressureBasePlus = configPressureBasePlusRect layout
      configPressureBaseBar = configPressureBaseBarRect layout
      configPressureTempScaleMinus = configPressureTempScaleMinusRect layout
      configPressureTempScalePlus = configPressureTempScalePlusRect layout
      configPressureTempScaleBar = configPressureTempScaleBarRect layout
      configPressureCoriolisScaleMinus = configPressureCoriolisScaleMinusRect layout
      configPressureCoriolisScalePlus = configPressureCoriolisScalePlusRect layout
      configPressureCoriolisScaleBar = configPressureCoriolisScaleBarRect layout
      configSeasonalBaseMinus = configSeasonalBaseMinusRect layout
      configSeasonalBasePlus = configSeasonalBasePlusRect layout
      configSeasonalBaseBar = configSeasonalBaseBarRect layout
      configSeasonalRangeMinus = configSeasonalRangeMinusRect layout
      configSeasonalRangePlus = configSeasonalRangePlusRect layout
      configSeasonalRangeBar = configSeasonalRangeBarRect layout
      configHumidityNoiseScaleMinus = configHumidityNoiseScaleMinusRect layout
      configHumidityNoiseScalePlus = configHumidityNoiseScalePlusRect layout
      configHumidityNoiseScaleBar = configHumidityNoiseScaleBarRect layout
      configPrecipNoiseScaleMinus = configPrecipNoiseScaleMinusRect layout
      configPrecipNoiseScalePlus = configPrecipNoiseScalePlusRect layout
      configPrecipNoiseScaleBar = configPrecipNoiseScaleBarRect layout
      configWeatherITCZWidthMinus = configWeatherITCZWidthMinusRect layout
      configWeatherITCZWidthPlus = configWeatherITCZWidthPlusRect layout
      configWeatherITCZWidthBar = configWeatherITCZWidthBarRect layout
      configWeatherITCZPrecipBoostMinus = configWeatherITCZPrecipBoostMinusRect layout
      configWeatherITCZPrecipBoostPlus = configWeatherITCZPrecipBoostPlusRect layout
      configWeatherITCZPrecipBoostBar = configWeatherITCZPrecipBoostBarRect layout
      configPressureHumidityScaleMinus = configPressureHumidityScaleMinusRect layout
      configPressureHumidityScalePlus = configPressureHumidityScalePlusRect layout
      configPressureHumidityScaleBar = configPressureHumidityScaleBarRect layout
      configPressureGradientWindScaleMinus = configPressureGradientWindScaleMinusRect layout
      configPressureGradientWindScalePlus = configPressureGradientWindScalePlusRect layout
      configPressureGradientWindScaleBar = configPressureGradientWindScaleBarRect layout
      configWindNoiseScaleMinus = configWindNoiseScaleMinusRect layout
      configWindNoiseScalePlus = configWindNoiseScalePlusRect layout
      configWindNoiseScaleBar = configWindNoiseScaleBarRect layout
      configITCZMigrationScaleMinus = configITCZMigrationScaleMinusRect layout
      configITCZMigrationScalePlus = configITCZMigrationScalePlusRect layout
      configITCZMigrationScaleBar = configITCZMigrationScaleBarRect layout
      configCloudRHExponentMinus = configCloudRHExponentMinusRect layout
      configCloudRHExponentPlus = configCloudRHExponentPlusRect layout
      configCloudRHExponentBar = configCloudRHExponentBarRect layout
      configCloudAlbedoEffectMinus = configCloudAlbedoEffectMinusRect layout
      configCloudAlbedoEffectPlus = configCloudAlbedoEffectPlusRect layout
      configCloudAlbedoEffectBar = configCloudAlbedoEffectBarRect layout
      configCloudPrecipBoostMinus = configCloudPrecipBoostMinusRect layout
      configCloudPrecipBoostPlus = configCloudPrecipBoostPlusRect layout
      configCloudPrecipBoostBar = configCloudPrecipBoostBarRect layout
      configVegBaseMinus = configVegBaseMinusRect layout
      configVegBasePlus = configVegBasePlusRect layout
      configVegBaseBar = configVegBaseBarRect layout
      configVegBoostMinus = configVegBoostMinusRect layout
      configVegBoostPlus = configVegBoostPlusRect layout
      configVegBoostBar = configVegBoostBarRect layout
      configVegTempWeightMinus = configVegTempWeightMinusRect layout
      configVegTempWeightPlus = configVegTempWeightPlusRect layout
      configVegTempWeightBar = configVegTempWeightBarRect layout
      configVegPrecipWeightMinus = configVegPrecipWeightMinusRect layout
      configVegPrecipWeightPlus = configVegPrecipWeightPlusRect layout
      configVegPrecipWeightBar = configVegPrecipWeightBarRect layout
      configBtCoastalBandMinus = configBtCoastalBandMinusRect layout
      configBtCoastalBandPlus = configBtCoastalBandPlusRect layout
      configBtCoastalBandBar = configBtCoastalBandBarRect layout
      configBtSnowMaxTempMinus = configBtSnowMaxTempMinusRect layout
      configBtSnowMaxTempPlus = configBtSnowMaxTempPlusRect layout
      configBtSnowMaxTempBar = configBtSnowMaxTempBarRect layout
      configBtAlpineMaxTempMinus = configBtAlpineMaxTempMinusRect layout
      configBtAlpineMaxTempPlus = configBtAlpineMaxTempPlusRect layout
      configBtAlpineMaxTempBar = configBtAlpineMaxTempBarRect layout
      configBtIceCapTempMinus = configBtIceCapTempMinusRect layout
      configBtIceCapTempPlus = configBtIceCapTempPlusRect layout
      configBtIceCapTempBar = configBtIceCapTempBarRect layout
      configBtMontaneMaxTempMinus = configBtMontaneMaxTempMinusRect layout
      configBtMontaneMaxTempPlus = configBtMontaneMaxTempPlusRect layout
      configBtMontaneMaxTempBar = configBtMontaneMaxTempBarRect layout
      configBtMontanePrecipMinus = configBtMontanePrecipMinusRect layout
      configBtMontanePrecipPlus = configBtMontanePrecipPlusRect layout
      configBtMontanePrecipBar = configBtMontanePrecipBarRect layout
      configBtCliffSlopeMinus = configBtCliffSlopeMinusRect layout
      configBtCliffSlopePlus = configBtCliffSlopePlusRect layout
      configBtCliffSlopeBar = configBtCliffSlopeBarRect layout
      configBtValleyMoistureMinus = configBtValleyMoistureMinusRect layout
      configBtValleyMoisturePlus = configBtValleyMoisturePlusRect layout
      configBtValleyMoistureBar = configBtValleyMoistureBarRect layout
      configBtDepressionMoistureMinus = configBtDepressionMoistureMinusRect layout
      configBtDepressionMoisturePlus = configBtDepressionMoisturePlusRect layout
      configBtDepressionMoistureBar = configBtDepressionMoistureBarRect layout
      configBtPrecipWeightMinus = configBtPrecipWeightMinusRect layout
      configBtPrecipWeightPlus = configBtPrecipWeightPlusRect layout
      configBtPrecipWeightBar = configBtPrecipWeightBarRect layout
      configVbcTempMinMinus = configVbcTempMinMinusRect layout
      configVbcTempMinPlus = configVbcTempMinPlusRect layout
      configVbcTempMinBar = configVbcTempMinBarRect layout
      configVbcTempRangeMinus = configVbcTempRangeMinusRect layout
      configVbcTempRangePlus = configVbcTempRangePlusRect layout
      configVbcTempRangeBar = configVbcTempRangeBarRect layout
      configVbcFertilityBoostMinus = configVbcFertilityBoostMinusRect layout
      configVbcFertilityBoostPlus = configVbcFertilityBoostPlusRect layout
      configVbcFertilityBoostBar = configVbcFertilityBoostBarRect layout
      configVbcAlbedoBaseMinus = configVbcAlbedoBaseMinusRect layout
      configVbcAlbedoBasePlus = configVbcAlbedoBasePlusRect layout
      configVbcAlbedoBaseBar = configVbcAlbedoBaseBarRect layout
      configVbcAlbedoBareMinus = configVbcAlbedoBareMinusRect layout
      configVbcAlbedoBarePlus = configVbcAlbedoBarePlusRect layout
      configVbcAlbedoBareBar = configVbcAlbedoBareBarRect layout
      configVbcAlbedoVegMinus = configVbcAlbedoVegMinusRect layout
      configVbcAlbedoVegPlus = configVbcAlbedoVegPlusRect layout
      configVbcAlbedoVegBar = configVbcAlbedoVegBarRect layout
      configVbcOceanAlbedoMinus = configVbcOceanAlbedoMinusRect layout
      configVbcOceanAlbedoPlus = configVbcOceanAlbedoPlusRect layout
      configVbcOceanAlbedoBar = configVbcOceanAlbedoBarRect layout
      configVbcIceAlbedoMinus = configVbcIceAlbedoMinusRect layout
      configVbcIceAlbedoPlus = configVbcIceAlbedoPlusRect layout
      configVbcIceAlbedoBar = configVbcIceAlbedoBarRect layout
      configBiomeSmoothingMinus = configBiomeSmoothingMinusRect layout
      configBiomeSmoothingPlus = configBiomeSmoothingPlusRect layout
      configBiomeSmoothingBar = configBiomeSmoothingBarRect layout
      configVolcanicAshBoostMinus = configVolcanicAshBoostMinusRect layout
      configVolcanicAshBoostPlus = configVolcanicAshBoostPlusRect layout
      configVolcanicAshBoostBar = configVolcanicAshBoostBarRect layout
      configVolcanicLavaPenaltyMinus = configVolcanicLavaPenaltyMinusRect layout
      configVolcanicLavaPenaltyPlus = configVolcanicLavaPenaltyPlusRect layout
      configVolcanicLavaPenaltyBar = configVolcanicLavaPenaltyBarRect layout
      configBiomeFeedbackBlendMinus = configBiomeFeedbackBlendMinusRect layout
      configBiomeFeedbackBlendPlus = configBiomeFeedbackBlendPlusRect layout
      configBiomeFeedbackBlendBar = configBiomeFeedbackBlendBarRect layout
      configBoundaryMotionTempMinus = configBoundaryMotionTempMinusRect layout
      configBoundaryMotionTempPlus = configBoundaryMotionTempPlusRect layout
      configBoundaryMotionTempBar = configBoundaryMotionTempBarRect layout
      configBoundaryMotionPrecipMinus = configBoundaryMotionPrecipMinusRect layout
      configBoundaryMotionPrecipPlus = configBoundaryMotionPrecipPlusRect layout
      configBoundaryMotionPrecipBar = configBoundaryMotionPrecipBarRect layout
      configPlanetRadiusMinus = configPlanetRadiusMinusRect layout
      configPlanetRadiusPlus = configPlanetRadiusPlusRect layout
      configPlanetRadiusBar = configPlanetRadiusBarRect layout
      configAxialTiltMinus = configAxialTiltMinusRect layout
      configAxialTiltPlus = configAxialTiltPlusRect layout
      configAxialTiltBar = configAxialTiltBarRect layout
      configInsolationMinus = configInsolationMinusRect layout
      configInsolationPlus = configInsolationPlusRect layout
      configInsolationBar = configInsolationBarRect layout
      configOccWarmScaleMinus = configOccWarmScaleMinusRect layout
      configOccWarmScalePlus = configOccWarmScalePlusRect layout
      configOccWarmScaleBar = configOccWarmScaleBarRect layout
      configOccColdScaleMinus = configOccColdScaleMinusRect layout
      configOccColdScalePlus = configOccColdScalePlusRect layout
      configOccColdScaleBar = configOccColdScaleBarRect layout
      configOccLatPeakDegMinus = configOccLatPeakDegMinusRect layout
      configOccLatPeakDegPlus = configOccLatPeakDegPlusRect layout
      configOccLatPeakDegBar = configOccLatPeakDegBarRect layout
      configOccLatWidthDegMinus = configOccLatWidthDegMinusRect layout
      configOccLatWidthDegPlus = configOccLatWidthDegPlusRect layout
      configOccLatWidthDegBar = configOccLatWidthDegBarRect layout
      configSliceLatCenterMinus = configSliceLatCenterMinusRect layout
      configSliceLatCenterPlus = configSliceLatCenterPlusRect layout
      configSliceLatCenterBar = configSliceLatCenterBarRect layout
      configSliceLonCenterMinus = configSliceLonCenterMinusRect layout
      configSliceLonCenterPlus = configSliceLonCenterPlusRect layout
      configSliceLonCenterBar = configSliceLonCenterBarRect layout
      configLatitudeExponentMinus = configLatitudeExponentMinusRect layout
      configLatitudeExponentPlus = configLatitudeExponentPlusRect layout
      configLatitudeExponentBar = configLatitudeExponentBarRect layout
      configPlateHeightCoolingMinus = configPlateHeightCoolingMinusRect layout
      configPlateHeightCoolingPlus = configPlateHeightCoolingPlusRect layout
      configPlateHeightCoolingBar = configPlateHeightCoolingBarRect layout
      configTempNoiseScaleMinus = configTempNoiseScaleMinusRect layout
      configTempNoiseScalePlus = configTempNoiseScalePlusRect layout
      configTempNoiseScaleBar = configTempNoiseScaleBarRect layout
      configOceanModerationMinus = configOceanModerationMinusRect layout
      configOceanModerationPlus = configOceanModerationPlusRect layout
      configOceanModerationBar = configOceanModerationBarRect layout
      configOceanModerateTempMinus = configOceanModerateTempMinusRect layout
      configOceanModerateTempPlus = configOceanModerateTempPlusRect layout
      configOceanModerateTempBar = configOceanModerateTempBarRect layout
      configAlbedoSensitivityMinus = configAlbedoSensitivityMinusRect layout
      configAlbedoSensitivityPlus = configAlbedoSensitivityPlusRect layout
      configAlbedoSensitivityBar = configAlbedoSensitivityBarRect layout
      configAlbedoReferenceMinus = configAlbedoReferenceMinusRect layout
      configAlbedoReferencePlus = configAlbedoReferencePlusRect layout
      configAlbedoReferenceBar = configAlbedoReferenceBarRect layout
      configMoistAdvectMinus = configMoistAdvectMinusRect layout
      configMoistAdvectPlus = configMoistAdvectPlusRect layout
      configMoistAdvectBar = configMoistAdvectBarRect layout
      configMoistLocalMinus = configMoistLocalMinusRect layout
      configMoistLocalPlus = configMoistLocalPlusRect layout
      configMoistLocalBar = configMoistLocalBarRect layout
      configMoistWindEvapScaleMinus = configMoistWindEvapScaleMinusRect layout
      configMoistWindEvapScalePlus = configMoistWindEvapScalePlusRect layout
      configMoistWindEvapScaleBar = configMoistWindEvapScaleBarRect layout
      configMoistEvapNoiseScaleMinus = configMoistEvapNoiseScaleMinusRect layout
      configMoistEvapNoiseScalePlus = configMoistEvapNoiseScalePlusRect layout
      configMoistEvapNoiseScaleBar = configMoistEvapNoiseScaleBarRect layout
      configMoistLandETCoeffMinus = configMoistLandETCoeffMinusRect layout
      configMoistLandETCoeffPlus = configMoistLandETCoeffPlusRect layout
      configMoistLandETCoeffBar = configMoistLandETCoeffBarRect layout
      configMoistBareEvapFracMinus = configMoistBareEvapFracMinusRect layout
      configMoistBareEvapFracPlus = configMoistBareEvapFracPlusRect layout
      configMoistBareEvapFracBar = configMoistBareEvapFracBarRect layout
      configMoistVegTranspFracMinus = configMoistVegTranspFracMinusRect layout
      configMoistVegTranspFracPlus = configMoistVegTranspFracPlusRect layout
      configMoistVegTranspFracBar = configMoistVegTranspFracBarRect layout
      configMoistWindETScaleMinus = configMoistWindETScaleMinusRect layout
      configMoistWindETScalePlus = configMoistWindETScalePlusRect layout
      configMoistWindETScaleBar = configMoistWindETScaleBarRect layout
      configMoistCondensationRateMinus = configMoistCondensationRateMinusRect layout
      configMoistCondensationRatePlus = configMoistCondensationRatePlusRect layout
      configMoistCondensationRateBar = configMoistCondensationRateBarRect layout
      configMoistRecycleRateMinus = configMoistRecycleRateMinusRect layout
      configMoistRecycleRatePlus = configMoistRecycleRatePlusRect layout
      configMoistRecycleRateBar = configMoistRecycleRateBarRect layout
      configMoistITCZStrengthMinus = configMoistITCZStrengthMinusRect layout
      configMoistITCZStrengthPlus = configMoistITCZStrengthPlusRect layout
      configMoistITCZStrengthBar = configMoistITCZStrengthBarRect layout
      configMoistITCZWidthMinus = configMoistITCZWidthMinusRect layout
      configMoistITCZWidthPlus = configMoistITCZWidthPlusRect layout
      configMoistITCZWidthBar = configMoistITCZWidthBarRect layout
      configOrographicScaleMinus = configOrographicScaleMinusRect layout
      configOrographicScalePlus = configOrographicScalePlusRect layout
      configOrographicScaleBar = configOrographicScaleBarRect layout
      configOrographicStepMinus = configOrographicStepMinusRect layout
      configOrographicStepPlus = configOrographicStepPlusRect layout
      configOrographicStepBar = configOrographicStepBarRect layout
      configCoastalIterationsMinus = configCoastalIterationsMinusRect layout
      configCoastalIterationsPlus = configCoastalIterationsPlusRect layout
      configCoastalIterationsBar = configCoastalIterationsBarRect layout
      configCoastalDiffuseMinus = configCoastalDiffuseMinusRect layout
      configCoastalDiffusePlus = configCoastalDiffusePlusRect layout
      configCoastalDiffuseBar = configCoastalDiffuseBarRect layout
      configCoastalMoistureBoostMinus = configCoastalMoistureBoostMinusRect layout
      configCoastalMoistureBoostPlus = configCoastalMoistureBoostPlusRect layout
      configCoastalMoistureBoostBar = configCoastalMoistureBoostBarRect layout
      configWindBeltStrengthMinus = configWindBeltStrengthMinusRect layout
      configWindBeltStrengthPlus = configWindBeltStrengthPlusRect layout
      configWindBeltStrengthBar = configWindBeltStrengthBarRect layout
      configWindBeltHarmonicsMinus = configWindBeltHarmonicsMinusRect layout
      configWindBeltHarmonicsPlus = configWindBeltHarmonicsPlusRect layout
      configWindBeltHarmonicsBar = configWindBeltHarmonicsBarRect layout
      configWindBeltBaseMinus = configWindBeltBaseMinusRect layout
      configWindBeltBasePlus = configWindBeltBasePlusRect layout
      configWindBeltBaseBar = configWindBeltBaseBarRect layout
      configWindBeltRangeMinus = configWindBeltRangeMinusRect layout
      configWindBeltRangePlus = configWindBeltRangePlusRect layout
      configWindBeltRangeBar = configWindBeltRangeBarRect layout
      configWindBeltSpeedScaleMinus = configWindBeltSpeedScaleMinusRect layout
      configWindBeltSpeedScalePlus = configWindBeltSpeedScalePlusRect layout
      configWindBeltSpeedScaleBar = configWindBeltSpeedScaleBarRect layout
      configBndLandRangeMinus = configBndLandRangeMinusRect layout
      configBndLandRangePlus = configBndLandRangePlusRect layout
      configBndLandRangeBar = configBndLandRangeBarRect layout
      configBndTempConvergentMinus = configBndTempConvergentMinusRect layout
      configBndTempConvergentPlus = configBndTempConvergentPlusRect layout
      configBndTempConvergentBar = configBndTempConvergentBarRect layout
      configBndTempDivergentMinus = configBndTempDivergentMinusRect layout
      configBndTempDivergentPlus = configBndTempDivergentPlusRect layout
      configBndTempDivergentBar = configBndTempDivergentBarRect layout
      configBndTempTransformMinus = configBndTempTransformMinusRect layout
      configBndTempTransformPlus = configBndTempTransformPlusRect layout
      configBndTempTransformBar = configBndTempTransformBarRect layout
      configBndPrecipConvergentMinus = configBndPrecipConvergentMinusRect layout
      configBndPrecipConvergentPlus = configBndPrecipConvergentPlusRect layout
      configBndPrecipConvergentBar = configBndPrecipConvergentBarRect layout
      configBndPrecipDivergentMinus = configBndPrecipDivergentMinusRect layout
      configBndPrecipDivergentPlus = configBndPrecipDivergentPlusRect layout
      configBndPrecipDivergentBar = configBndPrecipDivergentBarRect layout
      configBndPrecipTransformMinus = configBndPrecipTransformMinusRect layout
      configBndPrecipTransformPlus = configBndPrecipTransformPlusRect layout
      configBndPrecipTransformBar = configBndPrecipTransformBarRect layout
      configGenScaleMinus = configGenScaleMinusRect layout
      configGenScalePlus = configGenScalePlusRect layout
      configGenScaleBar = configGenScaleBarRect layout
      configGenCoordScaleMinus = configGenCoordScaleMinusRect layout
      configGenCoordScalePlus = configGenCoordScalePlusRect layout
      configGenCoordScaleBar = configGenCoordScaleBarRect layout
      configGenOffsetXMinus = configGenOffsetXMinusRect layout
      configGenOffsetXPlus = configGenOffsetXPlusRect layout
      configGenOffsetXBar = configGenOffsetXBarRect layout
      configGenOffsetYMinus = configGenOffsetYMinusRect layout
      configGenOffsetYPlus = configGenOffsetYPlusRect layout
      configGenOffsetYBar = configGenOffsetYBarRect layout
      configGenFrequencyMinus = configGenFrequencyMinusRect layout
      configGenFrequencyPlus = configGenFrequencyPlusRect layout
      configGenFrequencyBar = configGenFrequencyBarRect layout
      configGenOctavesMinus = configGenOctavesMinusRect layout
      configGenOctavesPlus = configGenOctavesPlusRect layout
      configGenOctavesBar = configGenOctavesBarRect layout
      configGenLacunarityMinus = configGenLacunarityMinusRect layout
      configGenLacunarityPlus = configGenLacunarityPlusRect layout
      configGenLacunarityBar = configGenLacunarityBarRect layout
      configGenGainMinus = configGenGainMinusRect layout
      configGenGainPlus = configGenGainPlusRect layout
      configGenGainBar = configGenGainBarRect layout
      configGenWarpScaleMinus = configGenWarpScaleMinusRect layout
      configGenWarpScalePlus = configGenWarpScalePlusRect layout
      configGenWarpScaleBar = configGenWarpScaleBarRect layout
      configGenWarpStrengthMinus = configGenWarpStrengthMinusRect layout
      configGenWarpStrengthPlus = configGenWarpStrengthPlusRect layout
      configGenWarpStrengthBar = configGenWarpStrengthBarRect layout
      configExtentXMinus = configExtentXMinusRect layout
      configExtentXPlus = configExtentXPlusRect layout
      configExtentXBar = configExtentXBarRect layout
      configExtentYMinus = configExtentYMinusRect layout
      configExtentYPlus = configExtentYPlusRect layout
      configExtentYBar = configExtentYBarRect layout
      configEdgeNorthMinus = configEdgeNorthMinusRect layout
      configEdgeNorthPlus = configEdgeNorthPlusRect layout
      configEdgeNorthBar = configEdgeNorthBarRect layout
      configEdgeSouthMinus = configEdgeSouthMinusRect layout
      configEdgeSouthPlus = configEdgeSouthPlusRect layout
      configEdgeSouthBar = configEdgeSouthBarRect layout
      configEdgeEastMinus = configEdgeEastMinusRect layout
      configEdgeEastPlus = configEdgeEastPlusRect layout
      configEdgeEastBar = configEdgeEastBarRect layout
      configEdgeWestMinus = configEdgeWestMinusRect layout
      configEdgeWestPlus = configEdgeWestPlusRect layout
      configEdgeWestBar = configEdgeWestBarRect layout
      configEdgeFalloffMinus = configEdgeFalloffMinusRect layout
      configEdgeFalloffPlus = configEdgeFalloffPlusRect layout
      configEdgeFalloffBar = configEdgeFalloffBarRect layout
      configPlateSizeMinus = configPlateSizeMinusRect layout
      configPlateSizePlus = configPlateSizePlusRect layout
      configPlateSizeBar = configPlateSizeBarRect layout
      configUpliftMinus = configUpliftMinusRect layout
      configUpliftPlus = configUpliftPlusRect layout
      configUpliftBar = configUpliftBarRect layout
      configRiftDepthMinus = configRiftDepthMinusRect layout
      configRiftDepthPlus = configRiftDepthPlusRect layout
      configRiftDepthBar = configRiftDepthBarRect layout
      configDetailScaleMinus = configDetailScaleMinusRect layout
      configDetailScalePlus = configDetailScalePlusRect layout
      configDetailScaleBar = configDetailScaleBarRect layout
      configPlateSpeedMinus = configPlateSpeedMinusRect layout
      configPlateSpeedPlus = configPlateSpeedPlusRect layout
      configPlateSpeedBar = configPlateSpeedBarRect layout
      configBoundarySharpnessMinus = configBoundarySharpnessMinusRect layout
      configBoundarySharpnessPlus = configBoundarySharpnessPlusRect layout
      configBoundarySharpnessBar = configBoundarySharpnessBarRect layout
      configBoundaryNoiseScaleMinus = configBoundaryNoiseScaleMinusRect layout
      configBoundaryNoiseScalePlus = configBoundaryNoiseScalePlusRect layout
      configBoundaryNoiseScaleBar = configBoundaryNoiseScaleBarRect layout
      configBoundaryNoiseStrengthMinus = configBoundaryNoiseStrengthMinusRect layout
      configBoundaryNoiseStrengthPlus = configBoundaryNoiseStrengthPlusRect layout
      configBoundaryNoiseStrengthBar = configBoundaryNoiseStrengthBarRect layout
      configBoundaryWarpOctavesMinus = configBoundaryWarpOctavesMinusRect layout
      configBoundaryWarpOctavesPlus = configBoundaryWarpOctavesPlusRect layout
      configBoundaryWarpOctavesBar = configBoundaryWarpOctavesBarRect layout
      configBoundaryWarpLacunarityMinus = configBoundaryWarpLacunarityMinusRect layout
      configBoundaryWarpLacunarityPlus = configBoundaryWarpLacunarityPlusRect layout
      configBoundaryWarpLacunarityBar = configBoundaryWarpLacunarityBarRect layout
      configBoundaryWarpGainMinus = configBoundaryWarpGainMinusRect layout
      configBoundaryWarpGainPlus = configBoundaryWarpGainPlusRect layout
      configBoundaryWarpGainBar = configBoundaryWarpGainBarRect layout
      configPlateMergeScaleMinus = configPlateMergeScaleMinusRect layout
      configPlateMergeScalePlus = configPlateMergeScalePlusRect layout
      configPlateMergeScaleBar = configPlateMergeScaleBarRect layout
      configPlateMergeBiasMinus = configPlateMergeBiasMinusRect layout
      configPlateMergeBiasPlus = configPlateMergeBiasPlusRect layout
      configPlateMergeBiasBar = configPlateMergeBiasBarRect layout
      configPlateDetailScaleMinus = configPlateDetailScaleMinusRect layout
      configPlateDetailScalePlus = configPlateDetailScalePlusRect layout
      configPlateDetailScaleBar = configPlateDetailScaleBarRect layout
      configPlateDetailStrengthMinus = configPlateDetailStrengthMinusRect layout
      configPlateDetailStrengthPlus = configPlateDetailStrengthPlusRect layout
      configPlateDetailStrengthBar = configPlateDetailStrengthBarRect layout
      configPlateRidgeStrengthMinus = configPlateRidgeStrengthMinusRect layout
      configPlateRidgeStrengthPlus = configPlateRidgeStrengthPlusRect layout
      configPlateRidgeStrengthBar = configPlateRidgeStrengthBarRect layout
      configPlateHeightBaseMinus = configPlateHeightBaseMinusRect layout
      configPlateHeightBasePlus = configPlateHeightBasePlusRect layout
      configPlateHeightBaseBar = configPlateHeightBaseBarRect layout
      configPlateHeightVarianceMinus = configPlateHeightVarianceMinusRect layout
      configPlateHeightVariancePlus = configPlateHeightVariancePlusRect layout
      configPlateHeightVarianceBar = configPlateHeightVarianceBarRect layout
      configPlateHardnessBaseMinus = configPlateHardnessBaseMinusRect layout
      configPlateHardnessBasePlus = configPlateHardnessBasePlusRect layout
      configPlateHardnessBaseBar = configPlateHardnessBaseBarRect layout
      configPlateHardnessVarianceMinus = configPlateHardnessVarianceMinusRect layout
      configPlateHardnessVariancePlus = configPlateHardnessVariancePlusRect layout
      configPlateHardnessVarianceBar = configPlateHardnessVarianceBarRect layout
      configTrenchDepthMinus = configTrenchDepthMinusRect layout
      configTrenchDepthPlus = configTrenchDepthPlusRect layout
      configTrenchDepthBar = configTrenchDepthBarRect layout
      configRidgeHeightMinus = configRidgeHeightMinusRect layout
      configRidgeHeightPlus = configRidgeHeightPlusRect layout
      configRidgeHeightBar = configRidgeHeightBarRect layout
      configPlateBiasStrengthMinus = configPlateBiasStrengthMinusRect layout
      configPlateBiasStrengthPlus = configPlateBiasStrengthPlusRect layout
      configPlateBiasStrengthBar = configPlateBiasStrengthBarRect layout
      configPlateBiasCenterMinus = configPlateBiasCenterMinusRect layout
      configPlateBiasCenterPlus = configPlateBiasCenterPlusRect layout
      configPlateBiasCenterBar = configPlateBiasCenterBarRect layout
      configPlateBiasEdgeMinus = configPlateBiasEdgeMinusRect layout
      configPlateBiasEdgePlus = configPlateBiasEdgePlusRect layout
      configPlateBiasEdgeBar = configPlateBiasEdgeBarRect layout
      configPlateBiasNorthMinus = configPlateBiasNorthMinusRect layout
      configPlateBiasNorthPlus = configPlateBiasNorthPlusRect layout
      configPlateBiasNorthBar = configPlateBiasNorthBarRect layout
      configPlateBiasSouthMinus = configPlateBiasSouthMinusRect layout
      configPlateBiasSouthPlus = configPlateBiasSouthPlusRect layout
      configPlateBiasSouthBar = configPlateBiasSouthBarRect layout
      configTfcCliffSlopeMinus = configTfcCliffSlopeMinusRect layout
      configTfcCliffSlopePlus = configTfcCliffSlopePlusRect layout
      configTfcCliffSlopeBar = configTfcCliffSlopeBarRect layout
      configTfcMountainSlopeMinus = configTfcMountainSlopeMinusRect layout
      configTfcMountainSlopePlus = configTfcMountainSlopePlusRect layout
      configTfcMountainSlopeBar = configTfcMountainSlopeBarRect layout
      configTfcMountainReliefMinus = configTfcMountainReliefMinusRect layout
      configTfcMountainReliefPlus = configTfcMountainReliefPlusRect layout
      configTfcMountainReliefBar = configTfcMountainReliefBarRect layout
      configTfcHillSlopeMinus = configTfcHillSlopeMinusRect layout
      configTfcHillSlopePlus = configTfcHillSlopePlusRect layout
      configTfcHillSlopeBar = configTfcHillSlopeBarRect layout
      configTfcRollingSlopeMinus = configTfcRollingSlopeMinusRect layout
      configTfcRollingSlopePlus = configTfcRollingSlopePlusRect layout
      configTfcRollingSlopeBar = configTfcRollingSlopeBarRect layout
      configValleyCurvatureMinus = configValleyCurvatureMinusRect layout
      configValleyCurvaturePlus = configValleyCurvaturePlusRect layout
      configValleyCurvatureBar = configValleyCurvatureBarRect layout
      configRockElevationThresholdMinus = configRockElevationThresholdMinusRect layout
      configRockElevationThresholdPlus = configRockElevationThresholdPlusRect layout
      configRockElevationThresholdBar = configRockElevationThresholdBarRect layout
      configRockHardnessThresholdMinus = configRockHardnessThresholdMinusRect layout
      configRockHardnessThresholdPlus = configRockHardnessThresholdPlusRect layout
      configRockHardnessThresholdBar = configRockHardnessThresholdBarRect layout
      configRockHardnessSecondaryMinus = configRockHardnessSecondaryMinusRect layout
      configRockHardnessSecondaryPlus = configRockHardnessSecondaryPlusRect layout
      configRockHardnessSecondaryBar = configRockHardnessSecondaryBarRect layout
      leftPanel = leftPanelRect layout
      leftToggle = leftToggleRect layout
      (leftTabTopo, leftTabView) = leftTabRects layout
      seedLabel = configSeedLabelRect layout
      seedValue = configSeedValueRect layout
      seedRandom = configSeedRandomRect layout
      chunkMinus = leftChunkMinusRect layout
      chunkPlus = leftChunkPlusRect layout
      chunkValue = configChunkValueRect layout
      logFilters = logFilterRects layout
      (viewRect1, viewRect2, viewRect3, viewRect4, viewRect5, viewRect6, viewRect7, viewRect8, viewRect9, viewRect10, viewRect11, viewRect12) = leftViewRects layout
      buttonLabel = if uiGenerating (rsUi snapshot) then V4 120 120 120 255 else V4 80 160 240 255
  tAfterLet <- getMonotonicTimeNSec
  SDL.rendererDrawColor renderer SDL.$= V4 r g b 255
  SDL.clear renderer
  tAfterClear <- getMonotonicTimeNSec
  let atlasScale = zoomTextureScale (uiZoom (rsUi snapshot))
      dataReady = dsTerrainChunks dataSnap == IntMap.size (tsTerrainChunks terrainSnap)
  (loggedSchedule, loggedScheduleDrain, loggedScheduleEnqueue) <-
    if shouldScheduleAtlas
      then do
        (jobCount, drainMs, enqueueMs) <- scheduleAtlasBuilds renderTargetOk dataReady atlasSchedulerHandle scheduleRef snapshotVersion snapshot
        let totalMs = drainMs + enqueueMs
        totalLogged <- logTiming logHandle timingLogThresholdMs (Text.pack "atlas schedule") totalMs (Just jobCount)
        drainLogged <- logTiming logHandle timingLogThresholdMs (Text.pack "atlas schedule drain") drainMs (Just jobCount)
        enqueueLogged <- logTiming logHandle timingLogThresholdMs (Text.pack "atlas schedule enqueue") enqueueMs (Just jobCount)
        pure (totalLogged, drainLogged, enqueueLogged)
      else pure (False, False, False)
  tAfterSchedule <- getMonotonicTimeNSec
  (atlasCache', uploadCount, uploadMs, uploadTextureMs) <- if shouldDrainAtlas
    then do
      ((cache', count, createMs), elapsed) <- timedMs $ do
        (cacheNext, count, createMs) <- drainAtlasBuildResults renderTargetOk atlasUploadsPerFrame renderer atlasCache resultRef
        pure (cacheNext, count, createMs)
      pure (cache', count, elapsed, createMs)
    else pure (atlasCache, 0, 0, 0)
  loggedUpload <-
    if shouldDrainAtlas && uploadCount > 0
      then logTiming logHandle timingLogThresholdMs (Text.pack "atlas upload") uploadMs (Just uploadCount)
      else pure False
  loggedTextureCreate <-
    if shouldDrainAtlas && uploadTextureMs >= timingLogThresholdMs
      then logTiming logHandle timingLogThresholdMs (Text.pack "atlas texture create") uploadTextureMs (Just uploadCount)
      else pure False
  tAfterDrain <- getMonotonicTimeNSec
  (atlasToDraw, atlasCache'', loggedAtlasResolve) <- do
    ((resolvedTiles, resolvedCache), elapsed) <- timedMs (resolveAtlasTiles renderTargetOk snapshot atlasCache' atlasScale)
    logged <- logTiming logHandle timingLogThresholdMs (Text.pack "atlas resolve") elapsed Nothing
    pure (resolvedTiles, resolvedCache, logged)
  tAfterResolve <- getMonotonicTimeNSec
  (textureCache', loggedChunkTexture) <-
    if renderTargetOk
      then pure (emptyChunkTextureCache, False)
      else if shouldUpdateChunkTextures
        then do
          (updatedCache, elapsed) <- timedMs (updateChunkTextures renderer terrainCache atlasScale textureCache)
          logged <- logTiming logHandle timingLogThresholdMs (Text.pack "chunk texture build") elapsed Nothing
          pure (updatedCache, logged)
        else pure (textureCache, False)
  loggedDraw <- case atlasToDraw of
    Just tiles -> do
      (_, elapsed) <- timedMs (drawAtlas renderer tiles (uiPanOffset (rsUi snapshot)) (uiZoom (rsUi snapshot)) (V2 (fromIntegral winW) (fromIntegral winH)))
      logTiming logHandle timingLogThresholdMs (Text.pack "draw atlas") elapsed Nothing
    Nothing ->
      if renderTargetOk
        then pure False
        else do
          (_, elapsed) <- timedMs (drawTerrain renderer terrainSnap terrainCache textureCache' (uiPanOffset (rsUi snapshot)) (uiZoom (rsUi snapshot)) (V2 (fromIntegral winW) (fromIntegral winH)))
          logTiming logHandle timingLogThresholdMs (Text.pack "draw terrain") elapsed Nothing
  tAfterDraw <- getMonotonicTimeNSec
  loggedHover <- do
    (_, elapsed) <- timedMs (drawHoverHex renderer (rsUi snapshot) atlasScale)
    logTiming logHandle timingLogThresholdMs (Text.pack "draw hover") elapsed Nothing
  loggedChrome <- do
    (_, elapsed) <- timedMs $ do
      let configColor = if uiShowConfig (rsUi snapshot) then V4 140 160 200 255 else V4 100 120 160 255
      SDL.rendererDrawColor renderer SDL.$= configColor
      SDL.fillRect renderer (Just (rectToSDL configToggle))
      let leftColor = if uiShowLeftPanel (rsUi snapshot) then V4 140 160 200 255 else V4 100 120 160 255
      SDL.rendererDrawColor renderer SDL.$= leftColor
      SDL.fillRect renderer (Just (rectToSDL leftToggle))
      when (uiShowLeftPanel (rsUi snapshot)) $ do
        SDL.rendererDrawColor renderer SDL.$= V4 35 45 60 230
        SDL.fillRect renderer (Just (rectToSDL leftPanel))
        drawLeftTabs renderer (rsUi snapshot) (leftTabTopo, leftTabView)
        case uiLeftTab (rsUi snapshot) of
          LeftTopo -> do
            drawChunkControl renderer (rsUi snapshot) chunkMinus chunkValue chunkPlus
            drawSeedControl renderer (rsUi snapshot) seedValue seedRandom
            SDL.rendererDrawColor renderer SDL.$= buttonLabel
            SDL.fillRect renderer (Just (rectToSDL buttonRect))
            drawStatusBars renderer fontCache (rsUi snapshot) dataSnap layout
          LeftView ->
            drawViewModeButtons renderer mode (viewRect1, viewRect2, viewRect3, viewRect4, viewRect5, viewRect6, viewRect7, viewRect8, viewRect9, viewRect10, viewRect11, viewRect12)
      drawConfigPanel renderer (rsUi snapshot) configPanel (tabTerrain, tabPlanet, tabClimate, tabWeather, tabBiome, tabErosion) configPresetSave configPresetLoad configReset configRevert configScrollArea configScrollBar
        (configWaterMinus, configWaterBar, configWaterPlus)
        (configEvapMinus, configEvapBar, configEvapPlus)
        (configRainShadowMinus, configRainShadowBar, configRainShadowPlus)
        (configWindDiffuseMinus, configWindDiffuseBar, configWindDiffusePlus)
        (configEquatorTempMinus, configEquatorTempBar, configEquatorTempPlus)
        (configPoleTempMinus, configPoleTempBar, configPoleTempPlus)
        (configLapseRateMinus, configLapseRateBar, configLapseRatePlus)
        (configWindIterationsMinus, configWindIterationsBar, configWindIterationsPlus)
        (configMoistureIterationsMinus, configMoistureIterationsBar, configMoistureIterationsPlus)
        (configWeatherTickMinus, configWeatherTickBar, configWeatherTickPlus)
        (configWeatherPhaseMinus, configWeatherPhaseBar, configWeatherPhasePlus)
        (configWeatherAmplitudeMinus, configWeatherAmplitudeBar, configWeatherAmplitudePlus)
        (configSeasonCycleLengthMinus, configSeasonCycleLengthBar, configSeasonCycleLengthPlus)
        (configJitterAmplitudeMinus, configJitterAmplitudeBar, configJitterAmplitudePlus)
        (configPressureBaseMinus, configPressureBaseBar, configPressureBasePlus)
        (configPressureTempScaleMinus, configPressureTempScaleBar, configPressureTempScalePlus)
        (configPressureCoriolisScaleMinus, configPressureCoriolisScaleBar, configPressureCoriolisScalePlus)
        (configSeasonalBaseMinus, configSeasonalBaseBar, configSeasonalBasePlus)
        (configSeasonalRangeMinus, configSeasonalRangeBar, configSeasonalRangePlus)
        (configHumidityNoiseScaleMinus, configHumidityNoiseScaleBar, configHumidityNoiseScalePlus)
        (configPrecipNoiseScaleMinus, configPrecipNoiseScaleBar, configPrecipNoiseScalePlus)
        (configWeatherITCZWidthMinus, configWeatherITCZWidthBar, configWeatherITCZWidthPlus)
        (configWeatherITCZPrecipBoostMinus, configWeatherITCZPrecipBoostBar, configWeatherITCZPrecipBoostPlus)
        (configPressureHumidityScaleMinus, configPressureHumidityScaleBar, configPressureHumidityScalePlus)
        (configPressureGradientWindScaleMinus, configPressureGradientWindScaleBar, configPressureGradientWindScalePlus)
        (configWindNoiseScaleMinus, configWindNoiseScaleBar, configWindNoiseScalePlus)
        (configITCZMigrationScaleMinus, configITCZMigrationScaleBar, configITCZMigrationScalePlus)
        (configCloudRHExponentMinus, configCloudRHExponentBar, configCloudRHExponentPlus)
        (configCloudAlbedoEffectMinus, configCloudAlbedoEffectBar, configCloudAlbedoEffectPlus)
        (configCloudPrecipBoostMinus, configCloudPrecipBoostBar, configCloudPrecipBoostPlus)
        (configVegBaseMinus, configVegBaseBar, configVegBasePlus)
        (configVegBoostMinus, configVegBoostBar, configVegBoostPlus)
        (configVegTempWeightMinus, configVegTempWeightBar, configVegTempWeightPlus)
        (configVegPrecipWeightMinus, configVegPrecipWeightBar, configVegPrecipWeightPlus)
        (configBtCoastalBandMinus, configBtCoastalBandBar, configBtCoastalBandPlus)
        (configBtSnowMaxTempMinus, configBtSnowMaxTempBar, configBtSnowMaxTempPlus)
        (configBtAlpineMaxTempMinus, configBtAlpineMaxTempBar, configBtAlpineMaxTempPlus)
        (configBtIceCapTempMinus, configBtIceCapTempBar, configBtIceCapTempPlus)
        (configBtMontaneMaxTempMinus, configBtMontaneMaxTempBar, configBtMontaneMaxTempPlus)
        (configBtMontanePrecipMinus, configBtMontanePrecipBar, configBtMontanePrecipPlus)
        (configBtCliffSlopeMinus, configBtCliffSlopeBar, configBtCliffSlopePlus)
        (configBtValleyMoistureMinus, configBtValleyMoistureBar, configBtValleyMoisturePlus)
        (configBtDepressionMoistureMinus, configBtDepressionMoistureBar, configBtDepressionMoisturePlus)
        (configBtPrecipWeightMinus, configBtPrecipWeightBar, configBtPrecipWeightPlus)
        (configVbcTempMinMinus, configVbcTempMinBar, configVbcTempMinPlus)
        (configVbcTempRangeMinus, configVbcTempRangeBar, configVbcTempRangePlus)
        (configVbcFertilityBoostMinus, configVbcFertilityBoostBar, configVbcFertilityBoostPlus)
        (configVbcAlbedoBaseMinus, configVbcAlbedoBaseBar, configVbcAlbedoBasePlus)
        (configVbcAlbedoBareMinus, configVbcAlbedoBareBar, configVbcAlbedoBarePlus)
        (configVbcAlbedoVegMinus, configVbcAlbedoVegBar, configVbcAlbedoVegPlus)
        (configVbcOceanAlbedoMinus, configVbcOceanAlbedoBar, configVbcOceanAlbedoPlus)
        (configVbcIceAlbedoMinus, configVbcIceAlbedoBar, configVbcIceAlbedoPlus)
        (configBiomeSmoothingMinus, configBiomeSmoothingBar, configBiomeSmoothingPlus)
        (configVolcanicAshBoostMinus, configVolcanicAshBoostBar, configVolcanicAshBoostPlus)
        (configVolcanicLavaPenaltyMinus, configVolcanicLavaPenaltyBar, configVolcanicLavaPenaltyPlus)
        (configBiomeFeedbackBlendMinus, configBiomeFeedbackBlendBar, configBiomeFeedbackBlendPlus)
        (configBoundaryMotionTempMinus, configBoundaryMotionTempBar, configBoundaryMotionTempPlus)
        (configBoundaryMotionPrecipMinus, configBoundaryMotionPrecipBar, configBoundaryMotionPrecipPlus)
        (configPlanetRadiusMinus, configPlanetRadiusBar, configPlanetRadiusPlus)
        (configAxialTiltMinus, configAxialTiltBar, configAxialTiltPlus)
        (configInsolationMinus, configInsolationBar, configInsolationPlus)
        (configOccWarmScaleMinus, configOccWarmScaleBar, configOccWarmScalePlus)
        (configOccColdScaleMinus, configOccColdScaleBar, configOccColdScalePlus)
        (configOccLatPeakDegMinus, configOccLatPeakDegBar, configOccLatPeakDegPlus)
        (configOccLatWidthDegMinus, configOccLatWidthDegBar, configOccLatWidthDegPlus)
        (configSliceLatCenterMinus, configSliceLatCenterBar, configSliceLatCenterPlus)
        (configSliceLonCenterMinus, configSliceLonCenterBar, configSliceLonCenterPlus)
        (configLatitudeExponentMinus, configLatitudeExponentBar, configLatitudeExponentPlus)
        (configPlateHeightCoolingMinus, configPlateHeightCoolingBar, configPlateHeightCoolingPlus)
        (configTempNoiseScaleMinus, configTempNoiseScaleBar, configTempNoiseScalePlus)
        (configOceanModerationMinus, configOceanModerationBar, configOceanModerationPlus)
        (configOceanModerateTempMinus, configOceanModerateTempBar, configOceanModerateTempPlus)
        (configAlbedoSensitivityMinus, configAlbedoSensitivityBar, configAlbedoSensitivityPlus)
        (configAlbedoReferenceMinus, configAlbedoReferenceBar, configAlbedoReferencePlus)
        (configMoistAdvectMinus, configMoistAdvectBar, configMoistAdvectPlus)
        (configMoistLocalMinus, configMoistLocalBar, configMoistLocalPlus)
        (configMoistWindEvapScaleMinus, configMoistWindEvapScaleBar, configMoistWindEvapScalePlus)
        (configMoistEvapNoiseScaleMinus, configMoistEvapNoiseScaleBar, configMoistEvapNoiseScalePlus)
        (configMoistLandETCoeffMinus, configMoistLandETCoeffBar, configMoistLandETCoeffPlus)
        (configMoistBareEvapFracMinus, configMoistBareEvapFracBar, configMoistBareEvapFracPlus)
        (configMoistVegTranspFracMinus, configMoistVegTranspFracBar, configMoistVegTranspFracPlus)
        (configMoistWindETScaleMinus, configMoistWindETScaleBar, configMoistWindETScalePlus)
        (configMoistCondensationRateMinus, configMoistCondensationRateBar, configMoistCondensationRatePlus)
        (configMoistRecycleRateMinus, configMoistRecycleRateBar, configMoistRecycleRatePlus)
        (configMoistITCZStrengthMinus, configMoistITCZStrengthBar, configMoistITCZStrengthPlus)
        (configMoistITCZWidthMinus, configMoistITCZWidthBar, configMoistITCZWidthPlus)
        (configOrographicScaleMinus, configOrographicScaleBar, configOrographicScalePlus)
        (configOrographicStepMinus, configOrographicStepBar, configOrographicStepPlus)
        (configCoastalIterationsMinus, configCoastalIterationsBar, configCoastalIterationsPlus)
        (configCoastalDiffuseMinus, configCoastalDiffuseBar, configCoastalDiffusePlus)
        (configCoastalMoistureBoostMinus, configCoastalMoistureBoostBar, configCoastalMoistureBoostPlus)
        (configWindBeltStrengthMinus, configWindBeltStrengthBar, configWindBeltStrengthPlus)
        (configWindBeltHarmonicsMinus, configWindBeltHarmonicsBar, configWindBeltHarmonicsPlus)
        (configWindBeltBaseMinus, configWindBeltBaseBar, configWindBeltBasePlus)
        (configWindBeltRangeMinus, configWindBeltRangeBar, configWindBeltRangePlus)
        (configWindBeltSpeedScaleMinus, configWindBeltSpeedScaleBar, configWindBeltSpeedScalePlus)
        (configBndLandRangeMinus, configBndLandRangeBar, configBndLandRangePlus)
        (configBndTempConvergentMinus, configBndTempConvergentBar, configBndTempConvergentPlus)
        (configBndTempDivergentMinus, configBndTempDivergentBar, configBndTempDivergentPlus)
        (configBndTempTransformMinus, configBndTempTransformBar, configBndTempTransformPlus)
        (configBndPrecipConvergentMinus, configBndPrecipConvergentBar, configBndPrecipConvergentPlus)
        (configBndPrecipDivergentMinus, configBndPrecipDivergentBar, configBndPrecipDivergentPlus)
        (configBndPrecipTransformMinus, configBndPrecipTransformBar, configBndPrecipTransformPlus)
        (configGenScaleMinus, configGenScaleBar, configGenScalePlus)
        (configGenCoordScaleMinus, configGenCoordScaleBar, configGenCoordScalePlus)
        (configGenOffsetXMinus, configGenOffsetXBar, configGenOffsetXPlus)
        (configGenOffsetYMinus, configGenOffsetYBar, configGenOffsetYPlus)
        (configGenFrequencyMinus, configGenFrequencyBar, configGenFrequencyPlus)
        (configGenOctavesMinus, configGenOctavesBar, configGenOctavesPlus)
        (configGenLacunarityMinus, configGenLacunarityBar, configGenLacunarityPlus)
        (configGenGainMinus, configGenGainBar, configGenGainPlus)
        (configGenWarpScaleMinus, configGenWarpScaleBar, configGenWarpScalePlus)
        (configGenWarpStrengthMinus, configGenWarpStrengthBar, configGenWarpStrengthPlus)
        (configExtentXMinus, configExtentXBar, configExtentXPlus)
        (configExtentYMinus, configExtentYBar, configExtentYPlus)
        (configEdgeNorthMinus, configEdgeNorthBar, configEdgeNorthPlus)
        (configEdgeSouthMinus, configEdgeSouthBar, configEdgeSouthPlus)
        (configEdgeEastMinus, configEdgeEastBar, configEdgeEastPlus)
        (configEdgeWestMinus, configEdgeWestBar, configEdgeWestPlus)
        (configEdgeFalloffMinus, configEdgeFalloffBar, configEdgeFalloffPlus)
        (configPlateSizeMinus, configPlateSizeBar, configPlateSizePlus)
        (configUpliftMinus, configUpliftBar, configUpliftPlus)
        (configRiftDepthMinus, configRiftDepthBar, configRiftDepthPlus)
        (configDetailScaleMinus, configDetailScaleBar, configDetailScalePlus)
        (configPlateSpeedMinus, configPlateSpeedBar, configPlateSpeedPlus)
        (configBoundarySharpnessMinus, configBoundarySharpnessBar, configBoundarySharpnessPlus)
        (configBoundaryNoiseScaleMinus, configBoundaryNoiseScaleBar, configBoundaryNoiseScalePlus)
        (configBoundaryNoiseStrengthMinus, configBoundaryNoiseStrengthBar, configBoundaryNoiseStrengthPlus)
        (configBoundaryWarpOctavesMinus, configBoundaryWarpOctavesBar, configBoundaryWarpOctavesPlus)
        (configBoundaryWarpLacunarityMinus, configBoundaryWarpLacunarityBar, configBoundaryWarpLacunarityPlus)
        (configBoundaryWarpGainMinus, configBoundaryWarpGainBar, configBoundaryWarpGainPlus)
        (configPlateMergeScaleMinus, configPlateMergeScaleBar, configPlateMergeScalePlus)
        (configPlateMergeBiasMinus, configPlateMergeBiasBar, configPlateMergeBiasPlus)
        (configPlateDetailScaleMinus, configPlateDetailScaleBar, configPlateDetailScalePlus)
        (configPlateDetailStrengthMinus, configPlateDetailStrengthBar, configPlateDetailStrengthPlus)
        (configPlateRidgeStrengthMinus, configPlateRidgeStrengthBar, configPlateRidgeStrengthPlus)
        (configPlateHeightBaseMinus, configPlateHeightBaseBar, configPlateHeightBasePlus)
        (configPlateHeightVarianceMinus, configPlateHeightVarianceBar, configPlateHeightVariancePlus)
        (configPlateHardnessBaseMinus, configPlateHardnessBaseBar, configPlateHardnessBasePlus)
        (configPlateHardnessVarianceMinus, configPlateHardnessVarianceBar, configPlateHardnessVariancePlus)
        (configTrenchDepthMinus, configTrenchDepthBar, configTrenchDepthPlus)
        (configRidgeHeightMinus, configRidgeHeightBar, configRidgeHeightPlus)
        (configPlateBiasStrengthMinus, configPlateBiasStrengthBar, configPlateBiasStrengthPlus)
        (configPlateBiasCenterMinus, configPlateBiasCenterBar, configPlateBiasCenterPlus)
        (configPlateBiasEdgeMinus, configPlateBiasEdgeBar, configPlateBiasEdgePlus)
        (configPlateBiasNorthMinus, configPlateBiasNorthBar, configPlateBiasNorthPlus)
        (configPlateBiasSouthMinus, configPlateBiasSouthBar, configPlateBiasSouthPlus)
        (configTfcCliffSlopeMinus, configTfcCliffSlopeBar, configTfcCliffSlopePlus)
        (configTfcMountainSlopeMinus, configTfcMountainSlopeBar, configTfcMountainSlopePlus)
        (configTfcMountainReliefMinus, configTfcMountainReliefBar, configTfcMountainReliefPlus)
        (configTfcHillSlopeMinus, configTfcHillSlopeBar, configTfcHillSlopePlus)
        (configTfcRollingSlopeMinus, configTfcRollingSlopeBar, configTfcRollingSlopePlus)
        (configValleyCurvatureMinus, configValleyCurvatureBar, configValleyCurvaturePlus)
        (configRockElevationThresholdMinus, configRockElevationThresholdBar, configRockElevationThresholdPlus)
        (configRockHardnessThresholdMinus, configRockHardnessThresholdBar, configRockHardnessThresholdPlus)
        (configRockHardnessSecondaryMinus, configRockHardnessSecondaryBar, configRockHardnessSecondaryPlus)
        (configErosionHydraulicMinus, configErosionHydraulicBar, configErosionHydraulicPlus)
        (configErosionThermalMinus, configErosionThermalBar, configErosionThermalPlus)
        (configErosionRainRateMinus, configErosionRainRateBar, configErosionRainRatePlus)
        (configErosionTalusMinus, configErosionTalusBar, configErosionTalusPlus)
        (configErosionMaxDropMinus, configErosionMaxDropBar, configErosionMaxDropPlus)
        (configGlacierSnowTempMinus, configGlacierSnowTempBar, configGlacierSnowTempPlus)
        (configGlacierSnowRangeMinus, configGlacierSnowRangeBar, configGlacierSnowRangePlus)
        (configGlacierMeltTempMinus, configGlacierMeltTempBar, configGlacierMeltTempPlus)
        (configGlacierMeltRateMinus, configGlacierMeltRateBar, configGlacierMeltRatePlus)
        (configGlacierAccumScaleMinus, configGlacierAccumScaleBar, configGlacierAccumScalePlus)
        (configGlacierFlowItersMinus, configGlacierFlowItersBar, configGlacierFlowItersPlus)
        (configGlacierFlowRateMinus, configGlacierFlowRateBar, configGlacierFlowRatePlus)
        (configGlacierErosionScaleMinus, configGlacierErosionScaleBar, configGlacierErosionScalePlus)
        (configGlacierCarveScaleMinus, configGlacierCarveScaleBar, configGlacierCarveScalePlus)
        (configGlacierDepositScaleMinus, configGlacierDepositScaleBar, configGlacierDepositScalePlus)
        (configVentDensityMinus, configVentDensityBar, configVentDensityPlus)
        (configVentThresholdMinus, configVentThresholdBar, configVentThresholdPlus)
        (configHotspotScaleMinus, configHotspotScaleBar, configHotspotScalePlus)
        (configHotspotThresholdMinus, configHotspotThresholdBar, configHotspotThresholdPlus)
        (configMagmaRechargeMinus, configMagmaRechargeBar, configMagmaRechargePlus)
        (configLavaScaleMinus, configLavaScaleBar, configLavaScalePlus)
        (configAshScaleMinus, configAshScaleBar, configAshScalePlus)
        (configVolcanicDepositScaleMinus, configVolcanicDepositScaleBar, configVolcanicDepositScalePlus)
        (configSoilMoistureThresholdMinus, configSoilMoistureThresholdBar, configSoilMoistureThresholdPlus)
        (configSoilHardnessThresholdMinus, configSoilHardnessThresholdBar, configSoilHardnessThresholdPlus)
        (configSoilFertilityMoistWeightMinus, configSoilFertilityMoistWeightBar, configSoilFertilityMoistWeightPlus)
        (configSoilFertilityDepthWeightMinus, configSoilFertilityDepthWeightBar, configSoilFertilityDepthWeightPlus)
        (configSinkBreachDepthMinus, configSinkBreachDepthBar, configSinkBreachDepthPlus)
        (configStreamPowerMaxErosionMinus, configStreamPowerMaxErosionBar, configStreamPowerMaxErosionPlus)
        (configRiverCarveMaxDepthMinus, configRiverCarveMaxDepthBar, configRiverCarveMaxDepthPlus)
        (configCoastalErodeStrengthMinus, configCoastalErodeStrengthBar, configCoastalErodeStrengthPlus)
        (configHydroHardnessWeightMinus, configHydroHardnessWeightBar, configHydroHardnessWeightPlus)
        (configMinLakeSizeMinus, configMinLakeSizeBar, configMinLakeSizePlus)
        (configInlandSeaMinSizeMinus, configInlandSeaMinSizeBar, configInlandSeaMinSizePlus)
        (configRoughnessScaleMinus, configRoughnessScaleBar, configRoughnessScalePlus)
    logTiming logHandle timingLogThresholdMs (Text.pack "draw chrome") elapsed Nothing
  tAfterChrome <- getMonotonicTimeNSec
  loggedUi <- do
    (_, elapsed) <- timedMs (drawUiOverlay renderer fontCache snapshot terrainSnap layout logFilters (V2 (fromIntegral winW) (fromIntegral winH)))
    logTiming logHandle timingLogThresholdMs (Text.pack "draw ui") elapsed Nothing
  tAfterUi <- getMonotonicTimeNSec
  loggedPresent <- do
    (_, elapsed) <- timedMs (SDL.present renderer)
    logTiming logHandle timingLogThresholdMs (Text.pack "present") elapsed Nothing
  tEnd <- getMonotonicTimeNSec
  let totalMs = nsToMs tStart tEnd
  when (totalMs >= 100) $ do
    hPutStrLn traceH $ "  FRAME total=" <> show totalMs
      <> " winSize=" <> show windowSizeElapsed
      <> " let=" <> show (nsToMs tStart tAfterLet)
      <> " clear=" <> show (nsToMs tAfterLet tAfterClear)
      <> " sched=" <> show (nsToMs tAfterClear tAfterSchedule)
      <> " drain=" <> show (nsToMs tAfterSchedule tAfterDrain)
      <> " resolve=" <> show (nsToMs tAfterDrain tAfterResolve)
      <> " draw=" <> show (nsToMs tAfterResolve tAfterDraw)
      <> " hover=" <> show (nsToMs tAfterDraw tAfterDraw)
      <> " chrome=" <> show (nsToMs tAfterDraw tAfterChrome)
      <> " ui=" <> show (nsToMs tAfterChrome tAfterUi)
      <> " present=" <> show (nsToMs tAfterUi tEnd)
      <> " rtOk=" <> show renderTargetOk
      <> " dataReady=" <> show dataReady
      <> " atlas=" <> show (isNothing atlasToDraw)
    hFlush traceH
  let didLog = loggedWindowSize || loggedSchedule || loggedScheduleDrain || loggedScheduleEnqueue || loggedUpload || loggedTextureCreate || loggedAtlasResolve || loggedChunkTexture || loggedDraw || loggedHover || loggedChrome || loggedUi || loggedPresent
  pure (renderTargetOk && dataReady && isNothing atlasToDraw, textureCache', atlasCache'', didLog)

logTiming :: ActorHandle Log (Protocol Log) -> Word32 -> Text.Text -> Word32 -> Maybe Int -> IO Bool
logTiming handle thresholdMs label elapsed maybeCount =
  if elapsed >= thresholdMs
    then do
      let countText = case maybeCount of
            Nothing -> ""
            Just count -> " (count=" <> show count <> ")"
          message = Text.pack (Text.unpack label <> " took " <> show elapsed <> "ms" <> countText)
      appendLog handle (LogEntry LogInfo message)
      pure True
    else pure False
