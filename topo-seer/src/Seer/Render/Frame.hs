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
      buttonRect = genButtonRect layout
      configToggle = configToggleRect layout
      configPanel = configPanelRect layout
      (tabTerrain, tabClimate, tabErosion) = configTabRects layout
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
      configSliceLatCenterMinus = configSliceLatCenterMinusRect layout
      configSliceLatCenterPlus = configSliceLatCenterPlusRect layout
      configSliceLatCenterBar = configSliceLatCenterBarRect layout
      configSliceLonCenterMinus = configSliceLonCenterMinusRect layout
      configSliceLonCenterPlus = configSliceLonCenterPlusRect layout
      configSliceLonCenterBar = configSliceLonCenterBarRect layout
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
      leftPanel = leftPanelRect layout
      leftToggle = leftToggleRect layout
      (leftTabTopo, leftTabView) = leftTabRects layout
      seedLabel = configSeedLabelRect layout
      seedValue = configSeedValueRect layout
      seedRandom = configSeedRandomRect layout
      chunkMinus = chunkMinusRect layout
      chunkPlus = chunkPlusRect layout
      chunkValue = configChunkValueRect layout
      logFilters = logFilterRects layout
      (viewRect1, viewRect2, viewRect3, viewRect4, viewRect5, viewRect6, viewRect7, viewRect8, viewRect9, viewRect10, viewRect11, viewRect12) = viewRects layout
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
      drawConfigPanel renderer (rsUi snapshot) configPanel (tabTerrain, tabClimate, tabErosion) configPresetSave configPresetLoad configReset configRevert configScrollArea configScrollBar
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
        (configVegBaseMinus, configVegBaseBar, configVegBasePlus)
        (configVegBoostMinus, configVegBoostBar, configVegBoostPlus)
        (configVegTempWeightMinus, configVegTempWeightBar, configVegTempWeightPlus)
        (configVegPrecipWeightMinus, configVegPrecipWeightBar, configVegPrecipWeightPlus)
        (configBoundaryMotionTempMinus, configBoundaryMotionTempBar, configBoundaryMotionTempPlus)
        (configBoundaryMotionPrecipMinus, configBoundaryMotionPrecipBar, configBoundaryMotionPrecipPlus)
        (configPlanetRadiusMinus, configPlanetRadiusBar, configPlanetRadiusPlus)
        (configAxialTiltMinus, configAxialTiltBar, configAxialTiltPlus)
        (configInsolationMinus, configInsolationBar, configInsolationPlus)
        (configSliceLatCenterMinus, configSliceLatCenterBar, configSliceLatCenterPlus)
        (configSliceLonCenterMinus, configSliceLonCenterBar, configSliceLonCenterPlus)
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
        (configErosionHydraulicMinus, configErosionHydraulicBar, configErosionHydraulicPlus)
        (configErosionThermalMinus, configErosionThermalBar, configErosionThermalPlus)
        (configErosionRainRateMinus, configErosionRainRateBar, configErosionRainRatePlus)
        (configErosionTalusMinus, configErosionTalusBar, configErosionTalusPlus)
        (configErosionMaxDropMinus, configErosionMaxDropBar, configErosionMaxDropPlus)
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

timedMs :: IO a -> IO (a, Word32)
timedMs action = do
  start <- getMonotonicTimeNSec
  result <- action
  end <- getMonotonicTimeNSec
  pure (result, nsToMs start end)

nsToMs :: Word64 -> Word64 -> Word32
nsToMs start end =
  fromIntegral ((end - start) `div` 1000000)

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
