{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Terrain progress/result handling for UI actions.
module Actor.UiActions.Terrain
  ( UiActionHandles(..)
  , handleTerrainProgress
  , handleTerrainLog
  , handleTerrainFailure
  , applyTerrainResult
  ) where

import Actor.AtlasManager (AtlasManager, atlasJobsForSelection, enqueueAtlasBuild)
import Seer.Render.ZoomStage (orderedZoomStagesForZoom)
import Actor.Data
  ( Data
  , DataSnapshot(..)
  , TerrainSnapshot(..)
  , getTerrainSnapshot
  , getDataSnapshot
  , setBiomeChunkCount
  , setClimateChunkData
  , setGlacierChunkData
  , setGroundwaterChunkData
  , setLastSeed
  , setOverlayStoreData
  , setTerrainGeoContextData
  , setRiverChunkData
  , setTerrainChunkCount
  , setTerrainChunkData
  , setVegetationChunkData
  , setVolcanismChunkData
  , setWaterBodyChunkData
  , setWeatherChunkData
  )
import Actor.Log (Log, LogEntry(..), LogLevel(..), appendLog, getLogSnapshot)
import Actor.Simulation (Simulation, cancelSimWorldTransition)
import Actor.SnapshotReceiver
  ( RenderSnapshot(..)
  , DataSnapshotRef
  , SnapshotVersion
  , SnapshotVersionRef
  , TerrainSnapshotRef
  , dataAndTerrainSnapshotUpdate
  , logSnapshotUpdate
  , publishSnapshot
  , uiSnapshotUpdate
  , withLogSnapshot
  , withUiSnapshot
  )
import Actor.Terrain (TerrainGenProgress(..), TerrainGenResult(..))
import Actor.UI
  ( PipelineStageRunState(..)
  , Ui
  , UiState(..)
  , effectiveViewSelection
  , getUiSnapshot
  , setUiGenerating
  , setUiOverlayNames
  , setUiPipelineStageRun
  )
import Data.Text (Text)
import qualified Data.Text as Text
import Hyperspace.Actor (ActorHandle, Protocol)
import Topo.Overlay (overlayNames)
import Topo.Pipeline (StageStatus(..))

-- | Handles cached from the last UI action request.
data UiActionHandles = UiActionHandles
  { uahLog :: !(ActorHandle Log (Protocol Log))
  , uahData :: !(ActorHandle Data (Protocol Data))
  , uahUi :: !(ActorHandle Ui (Protocol Ui))
  , uahAtlas :: !(ActorHandle AtlasManager (Protocol AtlasManager))
  , uahDataSnapshotRef :: !DataSnapshotRef
  , uahTerrainSnapshotRef :: !TerrainSnapshotRef
  , uahSnapshotVersionRef :: !SnapshotVersionRef
  , uahSimulation :: !(ActorHandle Simulation (Protocol Simulation))
  }

handleTerrainProgress :: UiActionHandles -> TerrainGenProgress -> IO ()
handleTerrainProgress handles progressMsg = do
  setUiPipelineStageRun (uahUi handles) PipelineStageRunState
    { psrsStageId = tgpStageId progressMsg
    , psrsStageName = tgpStageName progressMsg
    , psrsStatus = tgpStageStatus progressMsg
    , psrsElapsedMs = tgpStageElapsedMs progressMsg
    , psrsDetail = tgpStageDetail progressMsg
    }
  appendLog (uahLog handles) (LogEntry LogInfo (renderTerrainProgress progressMsg))
  uiSnapshot <- getUiSnapshot (uahUi handles)
  logSnapshot <- getLogSnapshot (uahLog handles)
  _ <- publishSnapshot
    (uahSnapshotVersionRef handles)
    (withLogSnapshot logSnapshot (uiSnapshotUpdate uiSnapshot))
  pure ()

handleTerrainLog :: UiActionHandles -> LogEntry -> IO ()
handleTerrainLog handles entry = do
  appendLog (uahLog handles) entry
  logSnapshot <- getLogSnapshot (uahLog handles)
  _ <- publishSnapshot
    (uahSnapshotVersionRef handles)
    (logSnapshotUpdate logSnapshot)
  pure ()

-- | Clear the generation/transition latches after a failed pipeline and commit
-- the final UI plus all failure logs delivered earlier by the Terrain actor.
handleTerrainFailure :: UiActionHandles -> Text -> IO ()
handleTerrainFailure handles _ = do
  setUiGenerating (uahUi handles) False
  cancelSimWorldTransition (uahSimulation handles)
  uiSnapshot <- getUiSnapshot (uahUi handles)
  logSnapshot <- getLogSnapshot (uahLog handles)
  _ <- publishSnapshot
    (uahSnapshotVersionRef handles)
    (withLogSnapshot logSnapshot (uiSnapshotUpdate uiSnapshot))
  pure ()

renderTerrainProgress :: TerrainGenProgress -> Text
renderTerrainProgress progressMsg =
  let idx = tgpStageIndex progressMsg
      total = tgpStageCount progressMsg
      name = tgpStageName progressMsg
      status = stageStatusText (tgpStageStatus progressMsg)
      prefix = "terrain: stage " <> toText idx <> "/" <> toText total <> " " <> status <> " "
      detail = maybe "" (" — " <>) (tgpStageDetail progressMsg)
      timing = case tgpStageElapsedMs progressMsg of
        Nothing -> ""
        Just elapsed -> " (elapsed " <> toText elapsed <> "ms)"
  in prefix <> name <> detail <> timing

applyTerrainResult :: UiActionHandles -> TerrainGenResult -> IO ()
applyTerrainResult handles resultMsg = do
  let chunkSize = tgrResultChunkSize resultMsg
      terrainChunks = tgrResultTerrainChunks resultMsg
      climateChunks = tgrResultClimateChunks resultMsg
      weatherChunks = tgrResultWeatherChunks resultMsg
      riverChunks   = tgrResultRiverChunks resultMsg
      groundwaterChunks = tgrResultGroundwaterChunks resultMsg
      volcanismChunks = tgrResultVolcanismChunks resultMsg
      glacierChunks = tgrResultGlacierChunks resultMsg
      waterBodyChunks = tgrResultWaterBodyChunks resultMsg
      vegetationChunks = tgrResultVegetationChunks resultMsg
      overlayStore = tgrResultOverlayStore resultMsg
      geoContext = tgrResultGeoContext resultMsg
      terrainCount = tgrResultTerrainCount resultMsg
      biomeCount = tgrResultBiomeCount resultMsg
  setTerrainGeoContextData (uahData handles) geoContext
  setTerrainChunkData (uahData handles) chunkSize []
  setClimateChunkData (uahData handles) chunkSize []
  setWeatherChunkData (uahData handles) chunkSize []
  setRiverChunkData   (uahData handles) chunkSize []
  setGroundwaterChunkData (uahData handles) chunkSize []
  setVolcanismChunkData (uahData handles) chunkSize []
  setGlacierChunkData (uahData handles) chunkSize []
  setWaterBodyChunkData (uahData handles) chunkSize []
  setVegetationChunkData (uahData handles) chunkSize []
  setOverlayStoreData (uahData handles) overlayStore
  setTerrainChunkData (uahData handles) chunkSize terrainChunks
  setClimateChunkData (uahData handles) chunkSize climateChunks
  setWeatherChunkData (uahData handles) chunkSize weatherChunks
  setRiverChunkData   (uahData handles) chunkSize riverChunks
  setGroundwaterChunkData (uahData handles) chunkSize groundwaterChunks
  setVolcanismChunkData (uahData handles) chunkSize volcanismChunks
  setGlacierChunkData (uahData handles) chunkSize glacierChunks
  setWaterBodyChunkData (uahData handles) chunkSize waterBodyChunks
  setVegetationChunkData (uahData handles) chunkSize vegetationChunks
  setTerrainChunkCount (uahData handles) terrainCount
  setBiomeChunkCount (uahData handles) biomeCount
  setLastSeed (uahData handles) (tgrResultSeed resultMsg)
  let dataSnap = DataSnapshot
        { dsTerrainChunks = terrainCount
        , dsBiomeChunks = biomeCount
        , dsLastSeed = Just (tgrResultSeed resultMsg)
        }
  setUiOverlayNames (uahUi handles) (overlayNames overlayStore)
  -- Fetch the authoritative snapshot from the Data actor so the version
  -- stamp matches what rebuildAtlasFor (view-mode buttons) will see.
  terrainSnap <- getTerrainSnapshot (uahData handles)
  dataSnap <- getDataSnapshot (uahData handles)
  setUiGenerating (uahUi handles) False
  appendLog (uahLog handles) (LogEntry LogInfo ("terrain: generation complete, awaiting render pipeline, chunks=" <> toText terrainCount))
  -- These calls are mailbox barriers for every externally visible completion
  -- write. The atlas is stamped with this same coherent publication epoch.
  uiSnapForAtlas <- getUiSnapshot (uahUi handles)
  logSnapForAtlas <- getLogSnapshot (uahLog handles)
  snapshotVersion <- publishSnapshot
    (uahSnapshotVersionRef handles)
    (withLogSnapshot logSnapForAtlas
      (withUiSnapshot uiSnapForAtlas
        (dataAndTerrainSnapshotUpdate
          (uahDataSnapshotRef handles) dataSnap
          (uahTerrainSnapshotRef handles) terrainSnap)))
  cancelSimWorldTransition (uahSimulation handles)
  rebuildAtlas handles snapshotVersion terrainSnap uiSnapForAtlas

rebuildAtlas :: UiActionHandles -> SnapshotVersion -> TerrainSnapshot -> UiState -> IO ()
rebuildAtlas handles snapshotVersion terrainSnap uiSnap = do
  let selection = effectiveViewSelection uiSnap
      -- Enqueue the current zoom stage first so the visible tiles are
      -- prioritised by the scheduler's round-robin dispatch.
      orderedStages = orderedZoomStagesForZoom (uiZoom uiSnap)
      jobs = atlasJobsForSelection snapshotVersion selection (uiRenderWaterLevel uiSnap) terrainSnap orderedStages Nothing
  -- Build only the current view selection's layers. Other modes build
  -- on-demand when the user switches to them.
  mapM_ (enqueueAtlasBuild (uahAtlas handles)) jobs

toText :: Show a => a -> Text
toText = Text.pack . show

stageStatusText :: StageStatus -> Text
stageStatusText StageStarted = "started"
stageStatusText StageRunning = "running"
stageStatusText StageCompleted = "completed"
stageStatusText StageSkipped = "skipped"

