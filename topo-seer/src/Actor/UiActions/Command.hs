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
  , startTerrainGen
  )
import Actor.UiActions.Protocol (TerrainReplyOps)
import Actor.UI
  ( ConfigTab(..)
  , Ui
  , UiState(..)
  , ViewMode(..)
  , UiSnapshotReply
  , getUiSnapshot
  , requestUiSnapshot
  , setUiBoundaryMotionPrecip
  , setUiBoundaryMotionTemp
  , setUiBoundaryNoiseScale
  , setUiBoundaryNoiseStrength
  , setUiBoundarySharpness
  , setUiBoundaryWarpGain
  , setUiBoundaryWarpLacunarity
  , setUiBoundaryWarpOctaves
  , setUiChunkSize
  , setUiConfigTab
  , setUiDetailScale
  , setUiErosionHydraulic
  , setUiErosionMaxDrop
  , setUiErosionTalus
  , setUiErosionThermal
  , setUiEquatorTemp
  , setUiEvaporation
  , setUiGenCoordScale
  , setUiGenFrequency
  , setUiGenGain
  , setUiGenLacunarity
  , setUiGenOctaves
  , setUiGenOffsetX
  , setUiGenOffsetY
  , setUiGenScale
  , setUiGenWarpScale
  , setUiGenWarpStrength
  , setUiWorldExtentX
  , setUiWorldExtentY
  , setUiEdgeDepthNorth
  , setUiEdgeDepthSouth
  , setUiEdgeDepthEast
  , setUiEdgeDepthWest
  , setUiEdgeDepthFalloff
  , setUiGenerating
  , setUiLapseRate
  , setUiMoistureIterations
  , setUiPlateBiasCenter
  , setUiPlateBiasEdge
  , setUiPlateBiasNorth
  , setUiPlateBiasSouth
  , setUiPlateBiasStrength
  , setUiPlateDetailScale
  , setUiPlateDetailStrength
  , setUiPlateHardnessBase
  , setUiPlateHardnessVariance
  , setUiPlateHeightBase
  , setUiPlateHeightVariance
  , setUiPlateMergeBias
  , setUiPlateMergeScale
  , setUiPlateRidgeStrength
  , setUiPlateSize
  , setUiPlateSpeed
  , setUiPoleTemp
  , setUiRainRate
  , setUiRainShadow
  , setUiRidgeHeight
  , setUiRiftDepth
  , setUiSeed
  , setUiSeedEditing
  , setUiSeedInput
  , setUiPlanetRadius
  , setUiAxialTilt
  , setUiInsolation
  , setUiSliceLatCenter
  , setUiSliceLonCenter
  , setUiTrenchDepth
  , setUiUplift
  , setUiVegBase
  , setUiVegBoost
  , setUiVegPrecipWeight
  , setUiVegTempWeight
  , setUiViewMode
  , setUiWaterLevel
  , setUiRenderWaterLevel
  , setUiWeatherAmplitude
  , setUiWeatherPhase
  , setUiWeatherTick
  , setUiWindDiffuse
  , setUiWindIterations
  , setUiWorldConfig
  )
import Seer.Config.Preset (presetFromUi, applyPresetToUi)
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
    ViewMoisture -> "Moisture"
    ViewPrecip -> "Precip"
    ViewPlateId -> "PlateId"
    ViewPlateBoundary -> "PlateBoundary"
    ViewPlateHardness -> "PlateHardness"
    ViewPlateCrust -> "PlateCrust"
    ViewPlateAge -> "PlateAge"
    ViewPlateHeight -> "PlateHeight"
    ViewPlateVelocity -> "PlateVelocity"

startGeneration :: UiActionRequest -> IO ()
startGeneration req = do
  let uiHandle = uarUiHandle req
      logHandle = uarLogHandle req
  uiSnap <- getUiSnapshot uiHandle
  setUiGenerating uiHandle True
  -- Commit the pending water level so atlas/terrain caches use the applied value
  setUiRenderWaterLevel uiHandle (uiWaterLevel uiSnap)
  -- Capture config snapshot for revert support
  setUiWorldConfig uiHandle (Just (presetFromUi uiSnap "world"))
  requestUiSnapshot uiHandle (replyTo @UiSnapshotReply (uarSnapshotHandle req))
  appendLog logHandle (LogEntry LogInfo (configSummary uiSnap))
  requestLogSnapshot logHandle (replyTo @LogSnapshotReply (uarSnapshotHandle req))
  let cfg = applyUiConfig uiSnap defaultWorldGenConfig
      request = TerrainGenRequest
        { tgrSeed = uiSeed uiSnap
        , tgrWorldConfig = WorldConfig { wcChunkSize = uiChunkSize uiSnap }
        , tgrGenConfig = cfg
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
      applyPresetToUi preset uiHandle
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
  setUiWaterLevel uiHandle 0.5
  setUiRenderWaterLevel uiHandle 0.5
  setUiEvaporation uiHandle 0.25
  setUiRainShadow uiHandle 0.4
  setUiWindDiffuse uiHandle 0.5
  setUiRainRate uiHandle 0.2
  setUiErosionHydraulic uiHandle 0.5
  setUiErosionThermal uiHandle 0.4
  setUiErosionTalus uiHandle 0.5
  setUiErosionMaxDrop uiHandle 0.5
  setUiEquatorTemp uiHandle 1
  setUiPoleTemp uiHandle 0
  setUiLapseRate uiHandle 0.25
  setUiConfigTab uiHandle ConfigTerrain
  setUiGenScale uiHandle 0.4444
  setUiGenCoordScale uiHandle 0.3333
  setUiGenOffsetX uiHandle 0.5
  setUiGenOffsetY uiHandle 0.5
  setUiGenFrequency uiHandle 0.1837
  setUiGenOctaves uiHandle 0.5
  setUiGenLacunarity uiHandle 0.25
  setUiGenGain uiHandle 0.4
  setUiGenWarpScale uiHandle 0.3333
  setUiGenWarpStrength uiHandle 0.5556
  setUiWorldExtentX uiHandle 0.125
  setUiWorldExtentY uiHandle 0.125
  setUiEdgeDepthNorth uiHandle 0
  setUiEdgeDepthSouth uiHandle 0
  setUiEdgeDepthEast uiHandle 0
  setUiEdgeDepthWest uiHandle 0
  setUiEdgeDepthFalloff uiHandle 0
  setUiPlateSize uiHandle 0.45
  setUiPlateSpeed uiHandle 0.38
  setUiBoundarySharpness uiHandle 0.35
  setUiBoundaryNoiseScale uiHandle 0.33
  setUiBoundaryNoiseStrength uiHandle 0.45
  setUiBoundaryWarpOctaves uiHandle 0.5
  setUiBoundaryWarpLacunarity uiHandle 0.25
  setUiBoundaryWarpGain uiHandle 0.4
  setUiPlateMergeScale uiHandle 0.3
  setUiPlateMergeBias uiHandle 0.44
  setUiPlateDetailScale uiHandle 0.33
  setUiPlateDetailStrength uiHandle 0.35
  setUiPlateRidgeStrength uiHandle 0.25
  setUiPlateHeightBase uiHandle 0.62
  setUiPlateHeightVariance uiHandle 0.65
  setUiPlateHardnessBase uiHandle 0.42
  setUiPlateHardnessVariance uiHandle 0.4
  setUiUplift uiHandle 0.3
  setUiRiftDepth uiHandle 0.35
  setUiTrenchDepth uiHandle 0.38
  setUiRidgeHeight uiHandle 0.33
  setUiDetailScale uiHandle 0.5
  setUiPlateBiasStrength uiHandle 0.42
  setUiPlateBiasCenter uiHandle 0.5
  setUiPlateBiasEdge uiHandle 0.5
  setUiPlateBiasNorth uiHandle 0.5
  setUiPlateBiasSouth uiHandle 0.5
  setUiWindIterations uiHandle 0.5
  setUiMoistureIterations uiHandle 0.5
  setUiBoundaryMotionTemp uiHandle 0.5
  setUiBoundaryMotionPrecip uiHandle 0.5
  setUiWeatherTick uiHandle 0.2
  setUiWeatherPhase uiHandle 0
  setUiWeatherAmplitude uiHandle 0.3
  setUiVegBase uiHandle 0.2
  setUiVegBoost uiHandle 0.6
  setUiVegTempWeight uiHandle 0.6
  setUiVegPrecipWeight uiHandle 0.4
  setUiPlanetRadius uiHandle 0.3333
  setUiAxialTilt uiHandle 0.5209
  setUiInsolation uiHandle 0.5
  setUiSliceLatCenter uiHandle 0.5
  setUiSliceLonCenter uiHandle 0.5
