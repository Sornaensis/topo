{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Command routing for UI actions.
module Actor.UiActions.Command
  ( UiAction(..)
  , UiActionRequest(..)
  , runUiAction
  ) where

import Actor.AtlasCache (AtlasKey(..))
import Actor.PluginManager (PluginManager, getPluginOverlaySchemas, getPluginStages, refreshManifests)
import Actor.Simulation (Simulation)
import Actor.AtlasManager (AtlasJob(..), AtlasManager, enqueueAtlasBuild)
import Actor.Data
  ( Data
  , TerrainSnapshot(..)
  , getTerrainSnapshot
  )
import Actor.Log (Log, LogEntry(..), LogLevel(..), LogSnapshotReply, appendLog, requestLogSnapshot)
import Actor.SnapshotReceiver (SnapshotReceiver)
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
  , UiSnapshotReply
  , getUiSnapshot
  , requestUiSnapshot
  , uiChunkSize
  , uiDisabledStages
  , setUiChunkSize
  , setUiConfigTab
  , setUiGenerating
  , uiRenderWaterLevel
  , setUiSeed
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
import Hyperspace.Actor (ActorHandle, Protocol, ReplyTo, replyTo)
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
  , uarUiHandle :: !(ActorHandle Ui (Protocol Ui))
  , uarLogHandle :: !(ActorHandle Log (Protocol Log))
  , uarDataHandle :: !(ActorHandle Data (Protocol Data))
  , uarTerrainHandle :: !(ActorHandle Terrain (Protocol Terrain))
  , uarAtlasHandle :: !(ActorHandle AtlasManager (Protocol AtlasManager))
  , uarTerrainReplyTo :: !(ReplyTo TerrainReplyOps)
  , uarSnapshotHandle :: !(ActorHandle SnapshotReceiver (Protocol SnapshotReceiver))
  -- ^ Reply channel for terrain generation progress and result payloads.
  , uarPluginManagerHandle :: !(ActorHandle PluginManager (Protocol PluginManager))
  -- ^ Handle for querying plugin stages during generation.
  , uarSimulationHandle :: !(ActorHandle Simulation (Protocol Simulation))
  -- ^ Handle for the simulation actor (receives world after generation).
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
  appendLog (uarLogHandle req) (LogEntry LogDebug message)
  requestLogSnapshot (uarLogHandle req) (replyTo @LogSnapshotReply (uarSnapshotHandle req))

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
  let uiHandle = uarUiHandle req
      logHandle = uarLogHandle req
      pluginHandle = uarPluginManagerHandle req
  uiSnap <- getUiSnapshot uiHandle
  setUiGenerating uiHandle True
  -- Commit the pending water level so atlas/terrain caches use the applied value
  setUiRenderWaterLevel uiHandle (uiWaterLevel uiSnap)
  -- Capture config snapshot for revert support
  setUiWorldConfig uiHandle (Just (snapshotFromUi uiSnap "world"))
  requestUiSnapshot uiHandle (replyTo @UiSnapshotReply (uarSnapshotHandle req))
  appendLog logHandle (LogEntry LogInfo (configSummary uiSnap))
  requestLogSnapshot logHandle (replyTo @LogSnapshotReply (uarSnapshotHandle req))
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
        , tgrSimHandle = uarSimulationHandle req
        }
  startTerrainGen (uarTerrainHandle req) (uarTerrainReplyTo req) request

rebuildAtlas :: UiActionRequest -> IO ()
rebuildAtlas req = do
  uiSnap <- getUiSnapshot (uarUiHandle req)
  rebuildAtlasFor req (uiViewMode uiSnap)

rebuildAtlasFor :: UiActionRequest -> ViewMode -> IO ()
rebuildAtlasFor req mode = do
  terrainSnap <- getTerrainSnapshot (uarDataHandle req)
  uiSnap <- getUiSnapshot (uarUiHandle req)
  let atlasKey = AtlasKey mode (uiRenderWaterLevel uiSnap) (tsVersion terrainSnap)
      scales = [1 .. 6]
      job scale = AtlasJob
        { ajKey = atlasKey
        , ajViewMode = mode
        , ajWaterLevel = uiRenderWaterLevel uiSnap
        , ajTerrain = terrainSnap
        , ajScale = scale
        }
  mapM_ (enqueueAtlasBuild (uarAtlasHandle req) . job) scales

setViewMode :: UiActionRequest -> ViewMode -> IO ()
setViewMode req mode =
  setUiViewMode (uarUiHandle req) mode

-- | Revert config sliders to the values captured at the last generation.
-- No-op if no world has been generated yet.
revertConfig :: UiActionRequest -> IO ()
revertConfig req = do
  let uiHandle = uarUiHandle req
      logHandle = uarLogHandle req
  uiSnap <- getUiSnapshot uiHandle
  case uiWorldConfig uiSnap of
    Just preset -> do
      applySnapshotToUi preset uiHandle
      appendLog logHandle (LogEntry LogInfo "Config reverted to last generation")
    Nothing ->
      appendLog logHandle (LogEntry LogWarn "No world config to revert to")

resetConfig :: UiActionRequest -> IO ()
resetConfig req = do
  let uiHandle = uarUiHandle req
  setUiSeed uiHandle 0
  setUiSeedInput uiHandle "0"
  setUiSeedEditing uiHandle False
  setUiChunkSize uiHandle 64
  setUiViewMode uiHandle ViewElevation
  setUiRenderWaterLevel uiHandle 0.5
  setUiConfigTab uiHandle ConfigTerrain
  resetSliderDefaults uiHandle
