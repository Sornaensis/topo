{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Command routing for UI actions.
module Actor.UiActions.Command
  ( UiAction(..)
  , ActorHandles(..)
  , UiActionRequest(..)
  , runUiAction
  , enqueueAtlasRebuildForTerrain
  , enqueueViewportRefreshForCurrentUi
  , isElevationTool
  ) where

import Actor.UiActions.Handles
  ( ActorHandles(..)
  , publishUiAndLogMutation
  , publishUiMutation
  )
import Actor.PluginManager
  ( LoadedPlugin(..)
  , PluginManager
  , getDisabledPlugins
  , getLoadedPlugins
  , getPluginDataResources
  , PluginSimulationPlan(..)
  , getPluginOrder
  , getPluginOverlaySchemas
  , getPluginSimulationPlan
  , pluginDiagnosticState
  , pluginDiagnosticStateText
  , pluginPanelDiagnosticLines
  , refreshManifests
  )
import Actor.PluginManager.PipelineIntegrator
  ( PluginPipelineDiagnostic(..)
  , PluginPipelineInput(..)
  , PluginPipelinePlan(..)
  , buildPluginPipelinePlan
  , pluginNamesDisabledByStage
  , pluginPipelineAvailableDependencyKeys
  )
import Actor.Simulation (Simulation, clearSimWorld)
import Actor.AtlasManager (AtlasJob(..), atlasJobsForSelection, atlasJobsForSelectionTransition, enqueueAtlasBuild)
import Seer.Render.Viewport (AtlasViewportCoverage, currentAtlasViewportCoverage)
import Seer.Render.ZoomStage (ZoomStage(..), orderedZoomStagesForZoom, stageForZoom)
import Actor.Data
  ( Data
  , TerrainSnapshot(..)
  , getDataSnapshot
  , getTerrainSnapshot
  , setTerrainChunkData
  )
import Actor.Log (Log, LogEntry(..), LogLevel(..), appendLog, getLogSnapshot)
import Actor.SnapshotReceiver
  ( SnapshotVersion
  , dataAndTerrainSnapshotUpdate
  , publishSnapshot
  , terrainSnapshotUpdate
  , withLogSnapshot
  , withUiSnapshot
  )
import Actor.Terrain
  ( Terrain
  , TerrainGenRequest(..)
  , TerrainReplyOps
  , startTerrainGen
  )
import Actor.UI
  ( BaseViewMode
  , ConfigTab(..)
  , LayeredViewState(..)
  , SkyOverlayMode
  , Ui
  , UiState(..)
  , ViewMode(..)
  , WeatherBasis
  , emptyUiState
  , getUiSnapshot
  , uiChunkSize
  , uiConfigTab
  , uiDisabledStages
  , setUiChunkSize
  , setUiConfigTab
  , setUiDataResources
  , setUiEditor
  , setUiGenerating
  , uiDayNightEnabled
  , uiEditor
  , uiRenderWaterLevel
  , setUiSeed
  , uiSeedEditing
  , setUiSeedEditing
  , setUiSeedInput
  , uiSeed
  , setUiPluginNames
  , setUiPluginParamSpecs
  , setUiPluginLifecycles
  , setUiPluginDiagnosticLines
  , setUiPluginDiagnosticStatuses
  , setUiDisabledPlugins
  , setUiOverlayNames
  , setUiViewSelection
  , uiWaterLevel
  , uiWorldConfig
  , uiZoom
  , setUiRenderWaterLevel
  , setUiWorldConfig
  , setUiDayNightEnabled
  )
import Seer.Config.Snapshot (snapshotFromUi, applySnapshotToUi)
import Seer.Config.SliderState (resetSliderDefaults)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.IntMap.Strict as IntMap
import Data.IORef (readIORef, writeIORef)
import Hyperspace.Actor (ActorHandle, Protocol, ReplyTo)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Seer.Config (configFromUi, configSummary)
import Seer.Editor.Brush (applyBrushStroke, applyErodeStroke, applyFlattenStroke, applyNoiseStroke, applyPaintBiomeStroke, applyPaintFormStroke, applySetHardnessStroke, applySmoothStroke)
import Seer.Editor.History (EditAction(..), pushEdit, undoEdit, redoEdit)
import Seer.Editor.Types (EditorState(..), EditorTool(..), BrushSettings(..))
import Topo (ChunkId(..), HexCoord(..), TileCoord(..), WorldConfig(..), chunkCoordFromTile, chunkIdFromCoord)
import Topo.BiomeConfig (BiomeConfig(..), classifyChunk)
import Topo.Hex (hexDisc)
import Topo.Overlay.Schema (OverlaySchema(..))
import Topo.Parameters.Recompute (recomputeDerivedChunks)
import Topo.Plugin.RPC.Manifest (rmParameters)
import Topo.Types (TerrainChunk(..))
import Topo.WorldGen (WorldGenConfig(..), TerrainConfig(..), buildFullPipelineConfig)
import qualified Data.Vector.Unboxed as U

-- | UI-triggered actions that can be executed asynchronously.
data UiAction
  = UiActionGenerate
  | UiActionReset
  | UiActionRevert
  | UiActionSetViewSelection !LayeredViewState
  | UiActionSetBaseViewMode !BaseViewMode
  | UiActionSetSkyOverlayMode !(Maybe SkyOverlayMode)
  | UiActionSetWeatherBasis !WeatherBasis
  | UiActionSetOverlayOpacity !Float
  | UiActionRebuildAtlas !ViewMode
  | UiActionToggleDayNight
  | UiActionRefreshViewport !(Maybe (Int, Int))
  | UiActionBrushStroke !(Int, Int)
    -- ^ Apply the current editor brush at the given hex @(q, r)@.
  | UiActionClearFlattenRef
    -- ^ Clear the cached flatten reference between discrete remote strokes.
  | UiActionUndo
    -- ^ Undo the last terrain edit.
  | UiActionRedo
    -- ^ Redo the last undone terrain edit.
  deriving (Eq, Show)

-- | Request payload for running a 'UiAction' on the UI actions actor.
data UiActionRequest = UiActionRequest
  { uarAction :: !UiAction
  , uarActorHandles :: !ActorHandles
  , uarTerrainReplyTo :: !(ReplyTo TerrainReplyOps)
  }

runUiAction :: UiActionRequest -> IO ()
runUiAction req =
  case uarAction req of
    UiActionGenerate ->
      logTimed req "Generate" (startGeneration req >> rebuildAtlas req)
    UiActionReset ->
      logTimed req "Config Reset" (resetConfig req)
    UiActionRevert ->
      logTimed req "Config Revert" (revertConfig req)
    UiActionSetViewSelection selection ->
      logTimed req "View selection" (setViewSelectionAndRebuild req selection)
    UiActionSetBaseViewMode base ->
      logTimed req "View base" (mutateViewSelectionAndRebuild req (\selection -> selection { lvsBaseView = base }))
    UiActionSetSkyOverlayMode overlay ->
      logTimed req "View overlay" (mutateViewSelectionAndRebuild req (\selection -> selection { lvsSkyOverlay = overlay }))
    UiActionSetWeatherBasis basis ->
      logTimed req "View weather basis" (mutateViewSelectionAndRebuild req (\selection -> selection { lvsWeatherBasis = basis }))
    UiActionSetOverlayOpacity opacity ->
      logTimed req "View overlay opacity" (mutateViewSelectionAndRebuild req (\selection -> selection { lvsOverlayOpacity = opacity }))
    UiActionRebuildAtlas mode ->
      logTimed req ("Rebuild Atlas " <> viewModeLabel mode) (rebuildAtlasFor req mode)
    UiActionToggleDayNight ->
      logTimed req "Toggle Day/Night" (toggleDayNight req)
    UiActionRefreshViewport mbWindowSize ->
      logTimed req "Refresh Viewport" (refreshViewport req mbWindowSize)
    UiActionBrushStroke hex ->
      logTimed req "Brush Stroke" (applyBrush req hex)
    UiActionClearFlattenRef ->
      logTimed req "Clear Flatten Ref" (clearFlattenRef req)
    UiActionUndo ->
      logTimed req "Undo" (undoBrush req)
    UiActionRedo ->
      logTimed req "Redo" (redoBrush req)

logTimed :: UiActionRequest -> Text -> IO () -> IO ()
logTimed _ _ action = action

viewModeLabel :: ViewMode -> Text
viewModeLabel mode =
  case mode of
    ViewElevation -> "Elevation"
    ViewBiome -> "Biome"
    ViewClimate -> "Avg Temp"
    ViewWeather -> "Cur Temp"
    ViewMoisture -> "Moisture"
    ViewPrecip -> "Avg Precip"
    ViewPrecipCurrent -> "Cur Precip"
    ViewPlateId -> "PlateId"
    ViewPlateBoundary -> "PlateBoundary"
    ViewPlateHardness -> "PlateHardness"
    ViewPlateCrust -> "PlateCrust"
    ViewPlateAge -> "PlateAge"
    ViewPlateHeight -> "PlateHeight"
    ViewPlateVelocity -> "PlateVelocity"
    ViewVegetation -> "Vegetation"
    ViewTerrainForm -> "TerrainForm"
    ViewCloud -> "Cur Cloud"
    ViewCloudTypical -> "Typ Cloud"
    ViewOverlay name _fieldIdx -> "Overlay:" <> name

startGeneration :: UiActionRequest -> IO ()
startGeneration req = do
  let handles = uarActorHandles req
      uiHandle = ahUiHandle handles
      logHandle = ahLogHandle handles
      pluginHandle = ahPluginManagerHandle handles
  uiSnap <- getUiSnapshot uiHandle
  setUiGenerating uiHandle True
  clearSimWorld (ahSimulationHandle handles) ()
  -- Commit the pending water level so atlas/terrain caches use the applied value
  setUiRenderWaterLevel uiHandle (uiWaterLevel uiSnap)
  -- Capture config snapshot for revert support
  setUiWorldConfig uiHandle (Just (snapshotFromUi uiSnap "world"))
  appendLog logHandle (LogEntry LogInfo (configSummary uiSnap))
  -- Hot-reload plugin manifests and collect plugin metadata for pipeline planning.
  refreshManifests pluginHandle
  overlaySchemas <- getPluginOverlaySchemas pluginHandle
  simulationPlan <- getPluginSimulationPlan pluginHandle (Just (map osName overlaySchemas))
  -- Populate plugin UI state from discovered plugins
  pluginOrder <- getPluginOrder pluginHandle
  loadedPlugins <- getLoadedPlugins pluginHandle
  dataResources <- getPluginDataResources pluginHandle
  let pluginMap = Map.fromList [(lpName lp, lp) | lp <- loadedPlugins]
      paramSpecs = Map.fromList
        [ (name, rmParameters (lpManifest lp))
        | name <- pluginOrder
        , Just lp <- [Map.lookup name pluginMap]
        ]
      lifecycles = Map.fromList [(lpName lp, lpLifecycle lp) | lp <- loadedPlugins]
  disabledPlugins <- getDisabledPlugins pluginHandle
  let cfg = configFromUi uiSnap
      disabledByStage = pluginNamesDisabledByStage (uiDisabledStages uiSnap)
      effectiveDisabledPlugins = disabledPlugins <> disabledByStage
      availableDeps = pluginPipelineAvailableDependencyKeys (uiDisabledStages uiSnap) effectiveDisabledPlugins loadedPlugins
      pluginPipelineInput = PluginPipelineInput
        { ppiPlugins = loadedPlugins
        , ppiPluginOrder = pluginOrder
        , ppiDisabledPlugins = disabledPlugins
        , ppiDisabledStages = uiDisabledStages uiSnap
        }
      pluginPlan = buildPluginPipelinePlan pluginPipelineInput
        (buildFullPipelineConfig cfg (WorldConfig { wcChunkSize = uiChunkSize uiSnap }) (uiSeed uiSnap))
      resolverLines = Map.fromListWith (<>)
        [ (ppdPlugin diag, ["Generator resolver: " <> ppdMessage diag])
        | diag <- pppDiagnostics pluginPlan
        , ppdBlocking diag
        ]
      resolverBlocked = Map.keysSet resolverLines
      diagnosticLines = Map.fromList
        [ ( lpName lp
          , pluginPanelDiagnosticLines effectiveDisabledPlugins availableDeps lp
              <> Map.findWithDefault [] (lpName lp) resolverLines
          )
        | lp <- loadedPlugins
        ]
      diagnosticStatuses = Map.fromList
        [ ( lpName lp
          , if Set.member (lpName lp) resolverBlocked && not (Set.member (lpName lp) effectiveDisabledPlugins)
              then "WaitingForDependencies"
              else pluginDiagnosticStateText (pluginDiagnosticState effectiveDisabledPlugins availableDeps lp)
          )
        | lp <- loadedPlugins
        ]
  setUiPluginNames uiHandle pluginOrder
  setUiPluginParamSpecs uiHandle paramSpecs
  setUiPluginLifecycles uiHandle lifecycles
  setUiPluginDiagnosticLines uiHandle diagnosticLines
  setUiPluginDiagnosticStatuses uiHandle diagnosticStatuses
  setUiDisabledPlugins uiHandle disabledPlugins
  setUiDataResources uiHandle dataResources
  setUiOverlayNames uiHandle (map osName overlaySchemas)
  let request = TerrainGenRequest
        { tgrSeed = uiSeed uiSnap
        , tgrWorldConfig = WorldConfig { wcChunkSize = uiChunkSize uiSnap }
        , tgrGenConfig = cfg
        , tgrDisabledStages = uiDisabledStages uiSnap
        , tgrPluginPipeline = pluginPipelineInput
        , tgrOverlaySchemas = overlaySchemas
        , tgrSimHandle = ahSimulationHandle handles
        , tgrSimNodes = pspExecutableNodes simulationPlan
        }
  startTerrainGen (ahTerrainHandle handles) (uarTerrainReplyTo req) request
  -- UiActions owns publication for widget-triggered generation. Drain all UI
  -- and log casts before exposing the generating state to render/API readers.
  _ <- publishUiAndLogMutation handles
  pure ()

rebuildAtlas :: UiActionRequest -> IO ()
rebuildAtlas req = do
  let handles = uarActorHandles req
  uiSnap <- getUiSnapshot (ahUiHandle handles)
  enqueueAtlasRebuildFor handles uiSnap

rebuildAtlasFor :: UiActionRequest -> ViewMode -> IO ()
rebuildAtlasFor req _mode = do
  let handles = uarActorHandles req
  uiSnap <- getUiSnapshot (ahUiHandle handles)
  enqueueAtlasRebuildFor handles uiSnap

-- | Toggle day/night through the UiActions actor so rapid clicks serialize
-- against the latest UI state before publishing a rebuild version.
toggleDayNight :: UiActionRequest -> IO ()
toggleDayNight req = do
  let handles = uarActorHandles req
      uiHandle = ahUiHandle handles
  uiSnap <- getUiSnapshot uiHandle
  appendLog (ahLogHandle handles) (LogEntry LogDebug "Toggle Day/Night")
  setUiDayNightEnabled uiHandle (not (uiDayNightEnabled uiSnap))
  postToggle <- getUiSnapshot uiHandle
  (terrainSnap, snapshotVersion) <- publishLatestTerrainSnapshot handles postToggle
  enqueueAtlasRebuildForTerrainWithForce True handles postToggle snapshotVersion terrainSnap

publishLatestTerrainSnapshot :: ActorHandles -> UiState -> IO (TerrainSnapshot, SnapshotVersion)
publishLatestTerrainSnapshot handles uiSnapshot = do
  terrainSnap <- getTerrainSnapshot (ahDataHandle handles)
  logSnapshot <- getLogSnapshot (ahLogHandle handles)
  snapshotVersion <- publishSnapshot
    (ahSnapshotVersionRef handles)
    (withLogSnapshot logSnapshot
      (withUiSnapshot uiSnapshot
        (terrainSnapshotUpdate (ahTerrainSnapshotRef handles) terrainSnap)))
  pure (terrainSnap, snapshotVersion)

enqueueAtlasRebuildFor :: ActorHandles -> UiState -> IO ()
enqueueAtlasRebuildFor handles uiSnap = do
  (terrainSnap, snapshotVersion) <- publishLatestTerrainSnapshot handles uiSnap
  enqueueAtlasRebuildForTerrain handles uiSnap snapshotVersion terrainSnap

-- | Enqueue a full ordered atlas rebuild using an already-captured terrain snapshot.
enqueueAtlasRebuildForTerrain :: ActorHandles -> UiState -> SnapshotVersion -> TerrainSnapshot -> IO ()
enqueueAtlasRebuildForTerrain handles uiSnap snapshotVersion terrainSnap =
  enqueueAtlasRebuildForTerrainWithForce False handles uiSnap snapshotVersion terrainSnap

enqueueAtlasRebuildForTerrainWithForce :: Bool -> ActorHandles -> UiState -> SnapshotVersion -> TerrainSnapshot -> IO ()
enqueueAtlasRebuildForTerrainWithForce forceRebuild handles uiSnap snapshotVersion terrainSnap = do
  let selection = uiViewSelection uiSnap
      -- Enqueue the current zoom stage first so the visible tiles are
      -- prioritised by the scheduler's round-robin dispatch.
      orderedStages = orderedZoomStagesForZoom (uiZoom uiSnap)
      jobs = map (\job -> job { ajForceRebuild = forceRebuild || ajForceRebuild job }) $
        atlasJobsForSelection snapshotVersion selection (uiRenderWaterLevel uiSnap) terrainSnap orderedStages Nothing
  mapM_ (enqueueAtlasBuild (ahAtlasManagerHandle handles)) jobs

-- | Enqueue only the layer keys that changed during a view-selection transition.
enqueueAtlasTransitionForTerrain :: ActorHandles -> UiState -> UiState -> SnapshotVersion -> TerrainSnapshot -> IO ()
enqueueAtlasTransitionForTerrain handles previousUi uiSnap snapshotVersion terrainSnap = do
  let previousSelection = uiViewSelection previousUi
      selection = uiViewSelection uiSnap
      orderedStages = orderedZoomStagesForZoom (uiZoom uiSnap)
      jobs = atlasJobsForSelectionTransition
        snapshotVersion
        previousSelection
        (uiRenderWaterLevel previousUi)
        selection
        (uiRenderWaterLevel uiSnap)
        terrainSnap
        orderedStages
        Nothing
  mapM_ (enqueueAtlasBuild (ahAtlasManagerHandle handles)) jobs

-- | Viewport-only atlas refresh: only rebuild the current zoom stage.
--
-- Used for zoom scroll and pan drag-release where the atlas key has not
-- changed — only the viewport moved.  This avoids enqueueing work for
-- all 5 zoom stages when only the visible one needs new tiles.
refreshViewport :: UiActionRequest -> Maybe (Int, Int) -> IO ()
refreshViewport req mbWindowSize = do
  _ <- enqueueViewportRefreshForCurrentUiWithWindow (uarActorHandles req) mbWindowSize
  pure ()

enqueueViewportRefreshForCurrentUi :: ActorHandles -> IO SnapshotVersion
enqueueViewportRefreshForCurrentUi handles =
  enqueueViewportRefreshForCurrentUiWithWindow handles Nothing

enqueueViewportRefreshForCurrentUiWithWindow :: ActorHandles -> Maybe (Int, Int) -> IO SnapshotVersion
enqueueViewportRefreshForCurrentUiWithWindow handles mbWindowSize = do
  uiSnap <- getUiSnapshot (ahUiHandle handles)
  (terrainSnap, snapshotVersion) <- publishLatestTerrainSnapshot handles uiSnap
  let selection = uiViewSelection uiSnap
      currentStage = stageForZoom (uiZoom uiSnap)
      viewportCoverage = viewportCoverageFor terrainSnap uiSnap mbWindowSize currentStage
      jobs = atlasJobsForSelection snapshotVersion selection (uiRenderWaterLevel uiSnap) terrainSnap [currentStage] viewportCoverage
  mapM_ (enqueueAtlasBuild (ahAtlasManagerHandle handles)) jobs
  pure snapshotVersion

viewportCoverageFor :: TerrainSnapshot -> UiState -> Maybe (Int, Int) -> ZoomStage -> Maybe AtlasViewportCoverage
viewportCoverageFor terrainSnap uiSnap mbWindowSize stage = do
  windowSize <- mbWindowSize
  if tsChunkSize terrainSnap > 0
    then Just $
      currentAtlasViewportCoverage
        (WorldConfig { wcChunkSize = tsChunkSize terrainSnap })
        (uiPanOffset uiSnap)
        (uiZoom uiSnap)
        windowSize
        terrainSnap
        stage
    else Nothing

setViewSelectionAndRebuild :: UiActionRequest -> LayeredViewState -> IO ()
setViewSelectionAndRebuild req selection =
  setViewSelectionFromPrevious req (const selection)

mutateViewSelectionAndRebuild :: UiActionRequest -> (LayeredViewState -> LayeredViewState) -> IO ()
mutateViewSelectionAndRebuild req mutate =
  setViewSelectionFromPrevious req (mutate . uiViewSelection)

setViewSelectionFromPrevious :: UiActionRequest -> (UiState -> LayeredViewState) -> IO ()
setViewSelectionFromPrevious req chooseSelection = do
  let handles = uarActorHandles req
      uiHandle = ahUiHandle handles
  previousUi <- getUiSnapshot uiHandle
  setUiViewSelection uiHandle (chooseSelection previousUi)
  uiSnap <- getUiSnapshot uiHandle
  (terrainSnap, snapshotVersion) <- publishLatestTerrainSnapshot handles uiSnap
  enqueueAtlasTransitionForTerrain handles previousUi uiSnap snapshotVersion terrainSnap

-- | Revert config sliders to the values captured at the last generation.
-- No-op if no world has been generated yet.
revertConfig :: UiActionRequest -> IO ()
revertConfig req = do
  let handles = uarActorHandles req
      uiHandle = ahUiHandle handles
      logHandle = ahLogHandle handles
  uiSnap <- getUiSnapshot uiHandle
  case uiWorldConfig uiSnap of
    Just preset -> do
      applySnapshotToUi preset uiHandle
      appendLog logHandle (LogEntry LogInfo "Config reverted to last generation")
    Nothing ->
      appendLog logHandle (LogEntry LogWarn "No world config to revert to")
  _ <- publishUiAndLogMutation handles
  pure ()

resetConfig :: UiActionRequest -> IO ()
resetConfig req = do
  let defaults = emptyUiState
      handles = uarActorHandles req
      uiHandle = ahUiHandle handles
  setUiSeed uiHandle (uiSeed defaults)
  setUiSeedInput uiHandle (Text.pack (show (uiSeed defaults)))
  setUiSeedEditing uiHandle (uiSeedEditing defaults)
  setUiChunkSize uiHandle (uiChunkSize defaults)
  setUiViewSelection uiHandle (uiViewSelection defaults)
  setUiRenderWaterLevel uiHandle (uiRenderWaterLevel defaults)
  setUiConfigTab uiHandle (uiConfigTab defaults)
  setUiWorldConfig uiHandle Nothing
  resetSliderDefaults uiHandle
  _ <- publishUiAndLogMutation handles
  pure ()

-- | Apply the current editor brush at the given hex coordinate.
--
-- Reads the editor state from the UI snapshot, applies the brush
-- to the terrain chunks, writes the modified chunks back to the
-- Data actor, bumps the snapshot version, and triggers an atlas
-- rebuild.
--
-- For 'ToolFlatten', captures the center tile elevation as the
-- flatten reference on the first stroke (when 'editorFlattenRef' is
-- 'Nothing').  For 'ToolNoise', increments the stroke counter to
-- seed each stroke uniquely.
applyBrush :: UiActionRequest -> (Int, Int) -> IO ()
applyBrush req hex = do
  let handles = uarActorHandles req
      uiHandle = ahUiHandle handles
      dataHandle = ahDataHandle handles
  uiSnap <- getUiSnapshot uiHandle
  let editor = uiEditor uiSnap
  terrainSnap <- getTerrainSnapshot dataHandle
  let cfg = WorldConfig { wcChunkSize = tsChunkSize terrainSnap }
      tool = editorTool editor
      brush = editorBrush editor
      oldChunks = tsTerrainChunks terrainSnap
      genCfg = configFromUi uiSnap
      terrain = worldTerrain genCfg
  -- Capture flatten reference on first stroke
  editor' <- case tool of
    ToolFlatten
      | Nothing <- editorFlattenRef editor -> do
          let centerElev = lookupCenterElev cfg oldChunks hex
          let e = editor { editorFlattenRef = Just centerElev }
          setUiEditor uiHandle e
          pure e
    _ -> pure editor
  -- Bump stroke id for noise tool
  editor'' <- case tool of
    ToolNoise -> do
      let sid = editorStrokeId editor' + 1
          e = editor' { editorStrokeId = sid }
      setUiEditor uiHandle e
      pure e
    _ -> pure editor'
  let brushed = case tool of
        ToolRaise  -> applyBrushStroke cfg tool brush hex oldChunks
        ToolLower  -> applyBrushStroke cfg tool brush hex oldChunks
        ToolSmooth -> applySmoothStroke cfg brush
                        (editorSmoothPasses editor'') hex oldChunks
        ToolFlatten ->
          let ref = case editorFlattenRef editor'' of
                Just r  -> r
                Nothing -> 0.5  -- fallback; should not happen
          in applyFlattenStroke cfg brush ref hex oldChunks
        ToolNoise ->
          let worldSeed = fromIntegral (uiSeed uiSnap)
          in applyNoiseStroke cfg brush worldSeed
               (editorStrokeId editor'') (editorNoiseFrequency editor'')
               hex oldChunks
        ToolPaintBiome ->
          applyPaintBiomeStroke cfg brush (editorBiomeId editor'') hex oldChunks
        ToolPaintForm ->
          applyPaintFormStroke cfg brush (editorFormOverride editor'') hex oldChunks
        ToolSetHardness ->
          applySetHardnessStroke cfg brush (editorHardnessTarget editor'') hex oldChunks
        ToolErode ->
          applyErodeStroke cfg brush (editorErodePasses editor'')
            (terrainErosion terrain)
            (terrainFormConfig terrain)
            (uiWaterLevel uiSnap)
            hex
            oldChunks
      -- Recompute derived terrain fields (slope, curvature, relief, etc.)
      -- for chunks affected by the brush stroke.
      paramCfg = terrainParameters terrain
      formCfg = terrainFormConfig terrain
      waterLvl = uiWaterLevel uiSnap
      affectedTiles = [ (q, r)
                       | HexAxial q r <- hexDisc (HexAxial (fst hex) (snd hex))
                                                 (brushRadius brush)
                       ]
      -- PaintForm writes tcTerrainForm directly; running recomputeDerivedChunks
      -- would immediately overwrite it with the elevation-derived form, erasing
      -- the manual paint.  PaintBiome only writes tcFlags and doesn't change
      -- elevation, so re-derive is wasteful but harmless; skip it too.
      newChunks
        | tool == ToolPaintForm || tool == ToolPaintBiome = brushed
        | otherwise = recomputeDerivedChunks cfg paramCfg formCfg waterLvl
                        affectedTiles brushed
      -- Reclassify biomes for tools that change terrain form (directly or via
      -- elevation/hardness), so the biome overlay stays up-to-date.
      reclassifiedChunks
        | needsBiomeReclassify tool =
            let biomeCfg      = worldBiome genCfg
                climateChunks = tsClimateChunks terrainSnap
            in IntMap.mapWithKey (\k tc ->
                 case IntMap.lookup k climateChunks of
                   Nothing -> tc
                   Just cc ->
                     tc { tcFlags = classifyChunk cfg (bcRules biomeCfg)
                            (bcThresholds biomeCfg) waterLvl Nothing tc cc }
               ) newChunks
        | otherwise = newChunks
      -- Convert modified chunks to the (ChunkId, TerrainChunk) list format
      changed = [ (ChunkId k, v)
                 | (k, v) <- IntMap.toList reclassifiedChunks
                 , case IntMap.lookup k oldChunks of
                     Just old -> old /= v
                     Nothing  -> True
                 ]
  -- Only write and rebuild if something changed
  case changed of
    [] -> do
      -- Flatten/noise bookkeeping can mutate the editor even when the brush
      -- produces no terrain delta; do not leave that UI state unpublished.
      _ <- publishUiMutation handles
      pure ()
    _  -> do
      -- Record undo action before writing
      let oldChanged = IntMap.fromList
            [ (k, old)
            | (ChunkId k, _) <- changed
            , Just old <- [IntMap.lookup k oldChunks]
            ]
          newChanged = IntMap.fromList
            [ (k, v)
            | (ChunkId k, v) <- changed
            ]
          action = EditAction
            { eaDescription = toolLabel tool
            , eaOldChunks   = oldChanged
            , eaNewChunks   = newChanged
            }
          histRef = ahHistoryRef handles
      hist <- readIORef histRef
      writeIORef histRef (pushEdit action hist)
      -- Merge changed chunks back into the full terrain map before writing.
      -- The data actor's terrain setter replaces the entire chunk map.
      writeTerrainChunkMap dataHandle (tsChunkSize terrainSnap)
        (IntMap.union newChanged oldChunks)
      -- Refresh the snapshot refs so the render pipeline picks up changes
      terrainSnap' <- getTerrainSnapshot dataHandle
      dataSnap' <- getDataSnapshot dataHandle
      publishedUi <- getUiSnapshot uiHandle
      snapshotVersion <- publishSnapshot
        (ahSnapshotVersionRef handles)
        (withUiSnapshot publishedUi
          (dataAndTerrainSnapshotUpdate
            (ahDataSnapshotRef handles) dataSnap'
            (ahTerrainSnapshotRef handles) terrainSnap'))
      enqueueAtlasRebuildForTerrain handles publishedUi snapshotVersion terrainSnap'

-- | Human-readable label for each editor tool.
toolLabel :: EditorTool -> Text
toolLabel ToolRaise       = "Raise"
toolLabel ToolLower       = "Lower"
toolLabel ToolSmooth      = "Smooth"
toolLabel ToolFlatten     = "Flatten"
toolLabel ToolNoise       = "Noise"
toolLabel ToolPaintBiome  = "Paint Biome"
toolLabel ToolPaintForm   = "Paint Form"
toolLabel ToolSetHardness = "Set Hardness"
toolLabel ToolErode       = "Erode"

-- | Does this tool modify elevation and therefore require biome reclassification?
isElevationTool :: EditorTool -> Bool
isElevationTool ToolRaise   = True
isElevationTool ToolLower   = True
isElevationTool ToolSmooth  = True
isElevationTool ToolFlatten = True
isElevationTool ToolNoise   = True
isElevationTool ToolErode   = True
isElevationTool _           = False

-- | Does this tool require biome reclassification after the stroke?
--
-- Elevation tools change elevation → terrain form and biome-driving
-- conditions change.  'ToolSetHardness' changes the hardness input to
-- 'classifyTerrainForm', which may flip canyon\/mesa\/badlands
-- assignments, so the newly-derived form requires a biome update.
-- 'ToolPaintForm' directly sets 'tcTerrainForm' and 'classifyChunk'
-- reads that field, so biomes must be re-classified after a form paint.
needsBiomeReclassify :: EditorTool -> Bool
needsBiomeReclassify ToolSetHardness = True
needsBiomeReclassify ToolPaintForm   = True
needsBiomeReclassify tool            = isElevationTool tool

clearFlattenRef :: UiActionRequest -> IO ()
clearFlattenRef req = do
  let uiHandle = ahUiHandle (uarActorHandles req)
  uiSnap <- getUiSnapshot uiHandle
  let editor = uiEditor uiSnap
  if editorFlattenRef editor == Nothing
    then pure ()
    else do
      setUiEditor uiHandle (editor { editorFlattenRef = Nothing })
      _ <- publishUiMutation (uarActorHandles req)
      pure ()

-- | Undo the most recent terrain edit by restoring the old chunk state.
undoBrush :: UiActionRequest -> IO ()
undoBrush req = do
  let handles = uarActorHandles req
      histRef = ahHistoryRef handles
  hist <- readIORef histRef
  case undoEdit hist of
    Nothing -> publishUiMutation handles >> pure ()
    Just (action, hist') -> do
      writeIORef histRef hist'
      applyChunkRestore handles (eaOldChunks action)

-- | Redo the most recently undone terrain edit.
redoBrush :: UiActionRequest -> IO ()
redoBrush req = do
  let handles = uarActorHandles req
      histRef = ahHistoryRef handles
  hist <- readIORef histRef
  case redoEdit hist of
    Nothing -> publishUiMutation handles >> pure ()
    Just (action, hist') -> do
      writeIORef histRef hist'
      applyChunkRestore handles (eaNewChunks action)

-- | Write a set of terrain chunks back to the data actor, refresh
-- snapshots, and trigger an atlas rebuild.
applyChunkRestore :: ActorHandles -> IntMap.IntMap TerrainChunk -> IO ()
applyChunkRestore handles chunks = do
  let dataHandle = ahDataHandle handles
      uiHandle   = ahUiHandle handles
  terrainSnap <- getTerrainSnapshot dataHandle
  writeTerrainChunkMap dataHandle (tsChunkSize terrainSnap)
    (IntMap.union chunks (tsTerrainChunks terrainSnap))
  terrainSnap' <- getTerrainSnapshot dataHandle
  dataSnap'    <- getDataSnapshot dataHandle
  uiSnap <- getUiSnapshot uiHandle
  snapshotVersion <- publishSnapshot
    (ahSnapshotVersionRef handles)
    (withUiSnapshot uiSnap
      (dataAndTerrainSnapshotUpdate
        (ahDataSnapshotRef handles) dataSnap'
        (ahTerrainSnapshotRef handles) terrainSnap'))
  enqueueAtlasRebuildForTerrain handles uiSnap snapshotVersion terrainSnap'

writeTerrainChunkMap
  :: ActorHandle Data (Protocol Data)
  -> Int
  -> IntMap.IntMap TerrainChunk
  -> IO ()
writeTerrainChunkMap dataHandle chunkSize chunks =
  setTerrainChunkData dataHandle chunkSize
    [ (ChunkId key, chunk)
    | (key, chunk) <- IntMap.toList chunks
    ]

-- | Look up the elevation at a hex tile, returning 0.5 if the tile
-- is not loaded.
lookupCenterElev :: WorldConfig -> IntMap.IntMap TerrainChunk -> (Int, Int) -> Float
lookupCenterElev cfg chunks (q, r) =
  let (chunkCoord, TileCoord lx ly) = chunkCoordFromTile cfg (TileCoord q r)
      ChunkId !key = chunkIdFromCoord chunkCoord
  in case IntMap.lookup key chunks of
       Nothing -> 0.5
       Just chunk ->
         let !csize = wcChunkSize cfg
             !idx = ly * csize + lx
             elev = tcElevation chunk
         in if idx < 0 || idx >= U.length elev
              then 0.5
              else elev `U.unsafeIndex` idx
