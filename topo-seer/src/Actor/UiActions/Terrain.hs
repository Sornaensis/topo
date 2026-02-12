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
import Actor.Data
  ( Data
  , DataSnapshot(..)
  , TerrainSnapshot(..)
  , getTerrainSnapshot
  , setBiomeChunkCount
  , setClimateChunkData
  , setLastSeed
  , setRiverChunkData
  , setTerrainChunkCount
  , setTerrainChunkData
  , setWeatherChunkData
  )
import Actor.Log (Log, LogEntry(..), LogLevel(..), LogSnapshot(..), LogSnapshotReply, appendLog, requestLogSnapshot)
import Actor.Render (RenderSnapshot(..))
import Actor.SnapshotReceiver (SnapshotReceiver, SnapshotRef, SnapshotVersion(..))
import Actor.Terrain (TerrainGenProgress(..), TerrainGenResult(..))
import Actor.UI (Ui, UiState(..), UiSnapshotReply, getUiSnapshot, requestUiSnapshot, setUiGenerating)
import Data.IORef (atomicModifyIORef')
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word32, Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Hyperspace.Actor (ActorHandle, Protocol, cast, replyTo)

-- | Handles cached from the last UI action request.
data UiActionHandles = UiActionHandles
  { uahLog :: !(ActorHandle Log (Protocol Log))
  , uahData :: !(ActorHandle Data (Protocol Data))
  , uahUi :: !(ActorHandle Ui (Protocol Ui))
  , uahAtlas :: !(ActorHandle AtlasManager (Protocol AtlasManager))
  , uahSnapshot :: !(ActorHandle SnapshotReceiver (Protocol SnapshotReceiver))
  , uahSnapshotRef :: !(Maybe SnapshotRef)
  }

handleTerrainProgress :: UiActionHandles -> TerrainGenProgress -> IO ()
handleTerrainProgress handles progressMsg =
  appendLog (uahLog handles) (LogEntry LogInfo (renderTerrainProgress progressMsg))
    >> requestLogSnapshot (uahLog handles) (replyTo @LogSnapshotReply (uahSnapshot handles))

handleTerrainLog :: UiActionHandles -> LogEntry -> IO ()
handleTerrainLog handles entry = do
  appendLog (uahLog handles) entry
  requestLogSnapshot (uahLog handles) (replyTo @LogSnapshotReply (uahSnapshot handles))

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
      terrainCount = tgrResultTerrainCount resultMsg
      biomeCount = tgrResultBiomeCount resultMsg
  setTerrainChunkData (uahData handles) chunkSize []
  setClimateChunkData (uahData handles) chunkSize []
  setWeatherChunkData (uahData handles) chunkSize []
  setRiverChunkData   (uahData handles) chunkSize []
  setTerrainChunkData (uahData handles) chunkSize terrainChunks
  setClimateChunkData (uahData handles) chunkSize climateChunks
  setWeatherChunkData (uahData handles) chunkSize weatherChunks
  setRiverChunkData   (uahData handles) chunkSize riverChunks
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
  castStart <- getMonotonicTimeNSec
  castSnapshots handles dataSnap terrainSnap uiSnap
  castEnd <- getMonotonicTimeNSec
  logStep "terrain: cast snapshots" castStart castEnd
  atlasStart <- getMonotonicTimeNSec
  rebuildAtlas handles terrainSnap uiSnap
  atlasEnd <- getMonotonicTimeNSec
  logStep "terrain: enqueue atlas" atlasStart atlasEnd
  setUiGenerating (uahUi handles) False
  requestUiSnapshot (uahUi handles) (replyTo @UiSnapshotReply (uahSnapshot handles))
  -- Write directly to the SnapshotReceiver's IORef so the render thread
  -- sees terrain data immediately, even if the SnapshotReceiver actor is
  -- CPU-starved by atlas workers.
  publishSnapshotDirect (uahSnapshotRef handles) uiSnap dataSnap terrainSnap
  completeNs <- getMonotonicTimeNSec
  appendLog (uahLog handles) (LogEntry LogInfo ("terrain: generation complete, awaiting render pipeline, chunks=" <> toText terrainCount <> " t=" <> nsText completeNs))
  requestLogSnapshot (uahLog handles) (replyTo @LogSnapshotReply (uahSnapshot handles))
  end <- getMonotonicTimeNSec
  let elapsedMs :: Double
      elapsedMs = fromIntegral (end - start) / 1e6
      message = "terrain: apply result took " <> Text.pack (show elapsedMs) <> "ms t=" <> nsText end
  appendLog (uahLog handles) (LogEntry LogDebug message)

castSnapshots :: UiActionHandles -> DataSnapshot -> TerrainSnapshot -> UiState -> IO ()
castSnapshots handles dataSnap terrainSnap uiSnap = do
  cast @"dataSnapshot" (uahSnapshot handles) #dataSnapshot dataSnap
  cast @"terrainSnapshot" (uahSnapshot handles) #terrainSnapshot terrainSnap
  cast @"uiSnapshot" (uahSnapshot handles) #uiSnapshot uiSnap

rebuildAtlas :: UiActionHandles -> TerrainSnapshot -> UiState -> IO ()
rebuildAtlas handles terrainSnap uiSnap = do
  start <- getMonotonicTimeNSec
  let atlasKey = AtlasKey (uiViewMode uiSnap) (uiRenderWaterLevel uiSnap) (tsVersion terrainSnap)
      scales = [1 .. 6]
      job scale = AtlasJob
        { ajKey = atlasKey
        , ajViewMode = uiViewMode uiSnap
        , ajWaterLevel = uiRenderWaterLevel uiSnap
        , ajTerrain = terrainSnap
        , ajScale = scale
        }
  mapM_ (enqueueAtlasBuild (uahAtlas handles) . job) scales
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

-- | Write the render snapshot directly to the shared IORef, bypassing the
-- SnapshotReceiver actor.  This ensures the render thread sees terrain data
-- immediately even when the SnapshotReceiver is CPU-starved by atlas workers.
publishSnapshotDirect :: Maybe SnapshotRef -> UiState -> DataSnapshot -> TerrainSnapshot -> IO ()
publishSnapshotDirect Nothing _ _ _ = pure ()
publishSnapshotDirect (Just ref) uiSnap dataSnap terrainSnap =
  atomicModifyIORef' ref $ \(SnapshotVersion v, oldSnap) ->
    let v' = SnapshotVersion (v + 1)
        newSnap = oldSnap
          { rsUi = uiSnap { uiGenerating = False }
          , rsData = dataSnap
          , rsTerrain = terrainSnap
          }
    in ((v', newSnap), ())

nsText :: Word64 -> Text
nsText ns = Text.pack (show ns)

nsToMs :: Word64 -> Word64 -> Word32
nsToMs start end =
  fromIntegral ((end - start) `div` 1000000)
