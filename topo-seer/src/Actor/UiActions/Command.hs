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
import Actor.PluginManager (PluginManager, getPluginOverlaySchemas, getPluginStages, refreshManifests)
import Actor.Simulation (Simulation)
import Actor.AtlasManager (AtlasJob(..), AtlasManager, enqueueAtlasBuild)
import Actor.Data
  ( Data
  , TerrainSnapshot(..)
  , getTerrainSnapshot
  )
import Actor.Log (Log, LogEntry(..), LogLevel(..), appendLog)
import Actor.SnapshotReceiver (bumpSnapshotVersion)
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
  , setUiGenerating
  , uiRenderWaterLevel
  , setUiSeed
  , uiSeedEditing
  , setUiSeedEditing
  , setUiSeedInput
  , uiSeed
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
import GHC.Clock (getMonotonicTimeNSec)
import Hyperspace.Actor (ActorHandle, Protocol, ReplyTo)
import Numeric (showFFloat)
import Seer.Config (applyUiConfig, configSummary)
import Topo (WorldConfig(..))
import Topo.WorldGen (defaultWorldGenConfig)

-- | UI-triggered actions that can be executed asynchronously.
data UiAction
  = UiActionGenerate
  | UiActionReset
  | UiActionRevert
  | UiActionSetViewMode !ViewMode
  | UiActionRebuildAtlas !ViewMode
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
