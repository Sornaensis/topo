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
  ) where

import Actor.UiActions.Handles (ActorHandles(..))
import Actor.AtlasCache (AtlasKey(..))
import Actor.PluginManager (LoadedPlugin(..), PluginManager, getDisabledPlugins, getLoadedPlugins, getPluginDataResources, getPluginOrder, getPluginOverlaySchemas, getPluginStages, refreshManifests)
import Actor.Simulation (Simulation)
import Actor.AtlasManager (AtlasJob(..), AtlasManager, enqueueAtlasBuild)
import Actor.Data
  ( Data
  , TerrainSnapshot(..)
  , getDataSnapshot
  , getTerrainSnapshot
  , setTerrainChunkData
  )
import Actor.Log (Log, LogEntry(..), LogLevel(..), appendLog)
import Actor.SnapshotReceiver (bumpSnapshotVersion, writeDataSnapshot, writeTerrainSnapshot)
import Actor.Terrain
  ( Terrain
  , TerrainGenRequest(..)
  , TerrainReplyOps
  , startTerrainGen
  )
import Actor.UI
  ( ConfigTab(..)
  , Ui
  , ViewMode(..)
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
  , uiEditor
  , uiRenderWaterLevel
  , setUiSeed
  , uiSeedEditing
  , setUiSeedEditing
  , setUiSeedInput
  , uiSeed
  , setUiPluginNames
  , setUiPluginParamSpecs
  , setUiDisabledPlugins
  , setUiOverlayNames
  , setUiViewMode
  , uiViewMode
  , uiWaterLevel
  , uiWorldConfig
  , setUiRenderWaterLevel
  , setUiWorldConfig
  )
import Seer.Config.Snapshot (snapshotFromUi, applySnapshotToUi)
import Seer.Config.SliderState (resetSliderDefaults)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.IntMap.Strict as IntMap
import GHC.Clock (getMonotonicTimeNSec)
import Hyperspace.Actor (ActorHandle, Protocol, ReplyTo)
import Numeric (showFFloat)
import qualified Data.Map.Strict as Map
import Seer.Config (applyUiConfig, configSummary)
import Seer.Editor.Brush (applyBrushStroke, applyFlattenStroke, applyNoiseStroke, applySmoothStroke)
import Seer.Editor.Types (EditorState(..), EditorTool(..), BrushSettings(..))
import Topo (ChunkId(..), HexCoord(..), TileCoord(..), WorldConfig(..), chunkCoordFromTile, chunkIdFromCoord)
import Topo.Hex (hexDisc)
import Topo.Overlay.Schema (OverlaySchema(..))
import Topo.Parameters.Recompute (recomputeDerivedChunks)
import Topo.Plugin.RPC.Manifest (rmParameters)
import Topo.Types (TerrainChunk(..))
import Topo.WorldGen (WorldGenConfig(..), TerrainConfig(..), defaultWorldGenConfig)
import qualified Data.Vector.Unboxed as U

-- | UI-triggered actions that can be executed asynchronously.
data UiAction
  = UiActionGenerate
  | UiActionReset
  | UiActionRevert
  | UiActionSetViewMode !ViewMode
  | UiActionRebuildAtlas !ViewMode
  | UiActionBrushStroke !(Int, Int)
    -- ^ Apply the current editor brush at the given hex @(q, r)@.
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
    UiActionSetViewMode mode ->
      logTimed req ("View " <> viewModeLabel mode) (setViewMode req mode >> rebuildAtlasFor req mode)
    UiActionRebuildAtlas mode ->
      logTimed req ("Rebuild Atlas " <> viewModeLabel mode) (rebuildAtlasFor req mode)
    UiActionBrushStroke hex ->
      logTimed req "Brush Stroke" (applyBrush req hex)

logTimed :: UiActionRequest -> Text -> IO () -> IO ()
logTimed req label action = do
  start <- getMonotonicTimeNSec
  action
  end <- getMonotonicTimeNSec
  let elapsedMs = fromIntegral (end - start) / nsPerMs
      message = label <> " took " <> Text.pack (showFFloat (Just timingPrecisionDigits) elapsedMs "ms")
  appendLog (ahLogHandle handles) (LogEntry LogDebug message)
  where
    handles = uarActorHandles req

nsPerMs :: Double
nsPerMs = 1e6

timingPrecisionDigits :: Int
timingPrecisionDigits = 2

viewModeLabel :: ViewMode -> Text
viewModeLabel mode =
  case mode of
    ViewElevation -> "Elevation"
    ViewBiome -> "Biome"
    ViewClimate -> "Climate"
    ViewWeather -> "Weather"
    ViewMoisture -> "Moisture"
    ViewPrecip -> "Precip"
    ViewPlateId -> "PlateId"
    ViewPlateBoundary -> "PlateBoundary"
    ViewPlateHardness -> "PlateHardness"
    ViewPlateCrust -> "PlateCrust"
    ViewPlateAge -> "PlateAge"
    ViewPlateHeight -> "PlateHeight"
    ViewPlateVelocity -> "PlateVelocity"
    ViewVegetation -> "Vegetation"
    ViewTerrainForm -> "TerrainForm"
    ViewOverlay name _fieldIdx -> "Overlay:" <> name

startGeneration :: UiActionRequest -> IO ()
startGeneration req = do
  let handles = uarActorHandles req
      uiHandle = ahUiHandle handles
      logHandle = ahLogHandle handles
      pluginHandle = ahPluginManagerHandle handles
  uiSnap <- getUiSnapshot uiHandle
  setUiGenerating uiHandle True
  -- Commit the pending water level so atlas/terrain caches use the applied value
  setUiRenderWaterLevel uiHandle (uiWaterLevel uiSnap)
  -- Capture config snapshot for revert support
  setUiWorldConfig uiHandle (Just (snapshotFromUi uiSnap "world"))
  bumpSnapshotVersion (ahSnapshotVersionRef handles)
  appendLog logHandle (LogEntry LogInfo (configSummary uiSnap))
  -- Hot-reload plugin manifests and collect plugin pipeline stages
  refreshManifests pluginHandle
  pluginStages <- getPluginStages pluginHandle
  overlaySchemas <- getPluginOverlaySchemas pluginHandle
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
  disabledPlugins <- getDisabledPlugins pluginHandle
  setUiPluginNames uiHandle pluginOrder
  setUiPluginParamSpecs uiHandle paramSpecs
  setUiDisabledPlugins uiHandle disabledPlugins
  setUiDataResources uiHandle dataResources
  setUiOverlayNames uiHandle (map osName overlaySchemas)
  let cfg = applyUiConfig uiSnap defaultWorldGenConfig
      request = TerrainGenRequest
        { tgrSeed = uiSeed uiSnap
        , tgrWorldConfig = WorldConfig { wcChunkSize = uiChunkSize uiSnap }
        , tgrGenConfig = cfg
        , tgrDisabledStages = uiDisabledStages uiSnap
        , tgrExtraStages = pluginStages
        , tgrOverlaySchemas = overlaySchemas
        , tgrSimHandle = ahSimulationHandle handles
        }
  startTerrainGen (ahTerrainHandle handles) (uarTerrainReplyTo req) request

rebuildAtlas :: UiActionRequest -> IO ()
rebuildAtlas req = do
  uiSnap <- getUiSnapshot (ahUiHandle (uarActorHandles req))
  rebuildAtlasFor req (uiViewMode uiSnap)

rebuildAtlasFor :: UiActionRequest -> ViewMode -> IO ()
rebuildAtlasFor req mode = do
  let handles = uarActorHandles req
  terrainSnap <- getTerrainSnapshot (ahDataHandle handles)
  uiSnap <- getUiSnapshot (ahUiHandle handles)
  let atlasKey = AtlasKey mode (uiRenderWaterLevel uiSnap) (tsVersion terrainSnap)
      scales = [1 .. 6]
      job scale = AtlasJob
        { ajKey = atlasKey
        , ajViewMode = mode
        , ajWaterLevel = uiRenderWaterLevel uiSnap
        , ajTerrain = terrainSnap
        , ajScale = scale
        }
  mapM_ (enqueueAtlasBuild (ahAtlasManagerHandle handles) . job) scales

setViewMode :: UiActionRequest -> ViewMode -> IO ()
setViewMode req mode =
  setUiViewMode (ahUiHandle (uarActorHandles req)) mode

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

resetConfig :: UiActionRequest -> IO ()
resetConfig req = do
  let defaults = emptyUiState
      uiHandle = ahUiHandle (uarActorHandles req)
  setUiSeed uiHandle (uiSeed defaults)
  setUiSeedInput uiHandle (Text.pack (show (uiSeed defaults)))
  setUiSeedEditing uiHandle (uiSeedEditing defaults)
  setUiChunkSize uiHandle (uiChunkSize defaults)
  setUiViewMode uiHandle (uiViewMode defaults)
  setUiRenderWaterLevel uiHandle (uiRenderWaterLevel defaults)
  setUiConfigTab uiHandle (uiConfigTab defaults)
  resetSliderDefaults uiHandle

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
      -- Recompute derived terrain fields (slope, curvature, relief, etc.)
      -- for chunks affected by the brush stroke.
      genCfg = applyUiConfig uiSnap defaultWorldGenConfig
      terrain = worldTerrain genCfg
      paramCfg = terrainParameters terrain
      formCfg = terrainFormConfig terrain
      waterLvl = uiWaterLevel uiSnap
      affectedTiles = [ (q, r)
                       | HexAxial q r <- hexDisc (HexAxial (fst hex) (snd hex))
                                                 (brushRadius brush)
                       ]
      newChunks = recomputeDerivedChunks cfg paramCfg formCfg waterLvl
                    affectedTiles brushed
      -- Convert modified chunks to the (ChunkId, TerrainChunk) list format
      changed = [ (ChunkId k, v)
                 | (k, v) <- IntMap.toList newChunks
                 , case IntMap.lookup k oldChunks of
                     Just old -> old /= v
                     Nothing  -> True
                 ]
  -- Only write and rebuild if something changed
  case changed of
    [] -> pure ()
    _  -> do
      setTerrainChunkData dataHandle (tsChunkSize terrainSnap) changed
      -- Refresh the snapshot refs so the render pipeline picks up changes
      terrainSnap' <- getTerrainSnapshot dataHandle
      dataSnap' <- getDataSnapshot dataHandle
      writeTerrainSnapshot (ahTerrainSnapshotRef handles) terrainSnap'
      writeDataSnapshot (ahDataSnapshotRef handles) dataSnap'
      bumpSnapshotVersion (ahSnapshotVersionRef handles)
      rebuildAtlasFor req (uiViewMode uiSnap)

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
