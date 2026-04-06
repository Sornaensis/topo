{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Terrain progress/result handling for UI actions.
module Actor.UiActions.Terrain
  ( UiActionHandles(..)
  , handleTerrainProgress
  , handleTerrainLog
  , applyTerrainResult
  ) where

import Actor.AtlasCache (AtlasKey(..))
import Actor.AtlasManager (AtlasJob(..), AtlasManager, enqueueAtlasBuild)
import Seer.Render.ZoomStage (ZoomStage(..), allZoomStages)
import Actor.Data
  ( Data
  , DataSnapshot(..)
  , TerrainSnapshot(..)
  , getTerrainSnapshot
  , getDataSnapshot
  , setBiomeChunkCount
  , setClimateChunkData
  , setLastSeed
  , setRiverChunkData
  , setTerrainChunkCount
  , setTerrainChunkData
  , setVegetationChunkData
  , setWeatherChunkData
  )
import Actor.Log (Log, LogEntry(..), LogLevel(..), appendLog)
import Actor.Render (RenderSnapshot(..))
import Actor.SnapshotReceiver
  ( DataSnapshotRef
  , TerrainSnapshotRef
  , SnapshotVersionRef
  , writeDataSnapshot
  , writeTerrainSnapshot
  , bumpSnapshotVersion
  )
import Actor.Terrain (TerrainGenProgress(..), TerrainGenResult(..))
import Actor.UI (Ui, UiState(..), getUiSnapshot, setUiGenerating)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Hyperspace.Actor (ActorHandle, Protocol)
import Seer.Timing (nsToMs)

-- | Handles cached from the last UI action request.
data UiActionHandles = UiActionHandles
  { uahLog :: !(ActorHandle Log (Protocol Log))
  , uahData :: !(ActorHandle Data (Protocol Data))
  , uahUi :: !(ActorHandle Ui (Protocol Ui))
  , uahAtlas :: !(ActorHandle AtlasManager (Protocol AtlasManager))
  , uahDataSnapshotRef :: !DataSnapshotRef
  , uahTerrainSnapshotRef :: !TerrainSnapshotRef
  , uahSnapshotVersionRef :: !SnapshotVersionRef
  }

handleTerrainProgress :: UiActionHandles -> TerrainGenProgress -> IO ()
handleTerrainProgress handles progressMsg =
  appendLog (uahLog handles) (LogEntry LogInfo (renderTerrainProgress progressMsg))

handleTerrainLog :: UiActionHandles -> LogEntry -> IO ()
handleTerrainLog handles entry =
  appendLog (uahLog handles) entry

renderTerrainProgress :: TerrainGenProgress -> Text
renderTerrainProgress progressMsg =
  let idx = tgpStageIndex progressMsg
      total = tgpStageCount progressMsg
      name = tgpStageName progressMsg
      prefix = "terrain: stage " <> toText idx <> "/" <> toText total <> " "
      timing = case tgpStageElapsedMs progressMsg of
        Nothing -> ""
        Just elapsed -> " (prev stage " <> toText elapsed <> "ms)"
  in prefix <> name <> timing

applyTerrainResult :: UiActionHandles -> TerrainGenResult -> IO ()
applyTerrainResult handles resultMsg = do
  start <- getMonotonicTimeNSec
  let logStep label stepStart stepEnd = appendLog (uahLog handles) (LogEntry LogDebug (label <> " took " <> Text.pack (show (nsToMs stepStart stepEnd)) <> "ms"))
  let chunkSize = tgrResultChunkSize resultMsg
      terrainChunks = tgrResultTerrainChunks resultMsg
      climateChunks = tgrResultClimateChunks resultMsg
      weatherChunks = tgrResultWeatherChunks resultMsg
      riverChunks   = tgrResultRiverChunks resultMsg
      vegetationChunks = tgrResultVegetationChunks resultMsg
      terrainCount = tgrResultTerrainCount resultMsg
      biomeCount = tgrResultBiomeCount resultMsg
  setTerrainChunkData (uahData handles) chunkSize []
  setClimateChunkData (uahData handles) chunkSize []
  setWeatherChunkData (uahData handles) chunkSize []
  setRiverChunkData   (uahData handles) chunkSize []
  setVegetationChunkData (uahData handles) chunkSize []
  setTerrainChunkData (uahData handles) chunkSize terrainChunks
  setClimateChunkData (uahData handles) chunkSize climateChunks
  setWeatherChunkData (uahData handles) chunkSize weatherChunks
  setRiverChunkData   (uahData handles) chunkSize riverChunks
  setVegetationChunkData (uahData handles) chunkSize vegetationChunks
  setTerrainChunkCount (uahData handles) terrainCount
  setBiomeChunkCount (uahData handles) biomeCount
  setLastSeed (uahData handles) (tgrResultSeed resultMsg)
  let dataSnap = DataSnapshot
        { dsTerrainChunks = terrainCount
        , dsBiomeChunks = biomeCount
        , dsLastSeed = Just (tgrResultSeed resultMsg)
        }
  uiStart <- getMonotonicTimeNSec
  uiSnap <- getUiSnapshot (uahUi handles)
  uiEnd <- getMonotonicTimeNSec
  logStep "terrain: get ui snapshot" uiStart uiEnd
  -- Fetch the authoritative snapshot from the Data actor so the version
  -- stamp matches what rebuildAtlasFor (view-mode buttons) will see.
  terrainSnap <- getTerrainSnapshot (uahData handles)
  dataSnap <- getDataSnapshot (uahData handles)
  -- Write per-domain snapshot refs directly — no actor roundtrip.
  castStart <- getMonotonicTimeNSec
  writeDataSnapshot (uahDataSnapshotRef handles) dataSnap
  writeTerrainSnapshot (uahTerrainSnapshotRef handles) terrainSnap
  bumpSnapshotVersion (uahSnapshotVersionRef handles)
  castEnd <- getMonotonicTimeNSec
  logStep "terrain: write snapshot refs" castStart castEnd
  atlasStart <- getMonotonicTimeNSec
  rebuildAtlas handles terrainSnap uiSnap
  atlasEnd <- getMonotonicTimeNSec
  logStep "terrain: enqueue atlas" atlasStart atlasEnd
  setUiGenerating (uahUi handles) False
  bumpSnapshotVersion (uahSnapshotVersionRef handles)
  completeNs <- getMonotonicTimeNSec
  appendLog (uahLog handles) (LogEntry LogInfo ("terrain: generation complete, awaiting render pipeline, chunks=" <> toText terrainCount <> " t=" <> nsText completeNs))
  end <- getMonotonicTimeNSec
  let elapsedMs :: Double
      elapsedMs = fromIntegral (end - start) / 1e6
      message = "terrain: apply result took " <> Text.pack (show elapsedMs) <> "ms t=" <> nsText end
  appendLog (uahLog handles) (LogEntry LogDebug message)

rebuildAtlas :: UiActionHandles -> TerrainSnapshot -> UiState -> IO ()
rebuildAtlas handles terrainSnap uiSnap = do
  start <- getMonotonicTimeNSec
  let currentMode = uiViewMode uiSnap
      mkJob stage =
        let atlasKey = AtlasKey currentMode (uiRenderWaterLevel uiSnap) (tsVersion terrainSnap)
        in AtlasJob
          { ajKey        = atlasKey
          , ajViewMode   = currentMode
          , ajWaterLevel = uiRenderWaterLevel uiSnap
          , ajTerrain    = terrainSnap
          , ajHexRadius  = zsHexRadius stage
          , ajAtlasScale = zsAtlasScale stage
          }
  -- Build only the current view mode (5 jobs for 5 zoom stages).
  -- Other modes build on-demand when the user switches to them.
  mapM_ (enqueueAtlasBuild (uahAtlas handles) . mkJob) allZoomStages
  end <- getMonotonicTimeNSec
  logElapsed (uahLog handles) "terrain: enqueue atlas jobs" start end

toText :: Show a => a -> Text
toText = Text.pack . show

logElapsed :: ActorHandle Log (Protocol Log) -> Text -> Word64 -> Word64 -> IO ()
logElapsed logHandle label start end =
  let elapsedMs :: Double
      elapsedMs = fromIntegral (end - start) / 1e6
      message = label <> " took " <> Text.pack (show elapsedMs) <> "ms"
  in appendLog logHandle (LogEntry LogDebug message)

nsText :: Word64 -> Text
nsText ns = Text.pack (show ns)
