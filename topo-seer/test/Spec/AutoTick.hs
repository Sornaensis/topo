{-# LANGUAGE OverloadedStrings #-}

module Spec.AutoTick (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, readMVar, tryPutMVar)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified Data.IntMap.Strict as IntMap
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import GHC.Clock (getMonotonicTimeNSec)
import System.Timeout (timeout)
import Test.Hspec

import Actor.AtlasCache (atlasKeyVersion)
import Actor.AtlasManager (AtlasJob(..), drainAtlasJobs)
import Actor.Data (TerrainSnapshot(..), getTerrainSnapshot, replaceTerrainData)
import Actor.Simulation
  ( SimulationDagSnapshot(..)
  , SimulationTickLogEntry(..)
  , SimulationDagNodeSnapshot(..)
  , SimulationNodeBinding(..)
  , getSimDagSnapshot
  , setSimWorld
  , setSimWorldWithNodes
  )
import Actor.SnapshotReceiver (readSnapshotVersion)
import Actor.UI (UiState(..), ViewMode(..), getUiSnapshot)
import Actor.UiActions (ActorHandles(..))
import Seer.Command.Dispatch (CommandContext(..), dispatchCommand)
import Seer.Headless
  ( HeadlessApp
  , defaultHeadlessConfig
  , headlessCommandContext
  , withHeadlessApp
  )
import Seer.Render.ZoomStage (allZoomStages)
import Topo
  ( ChunkId(..)
  , ClimateChunk(..)
  , WeatherChunk(..)
  , WorldConfig(..)
  , defaultHexGridMeta
  , emptyClimateChunk
  , emptyWorldWithPlanet
  , generateTerrainChunk
  , setClimateChunk
  , setTerrainChunk
  )
import Topo.Command.Types (SeerCommand(..), SeerResponse(..))
import Topo.Overlay (Overlay(..), OverlayData(..), OverlayProvenance(..), insertOverlay, lookupOverlay)
import Topo.Overlay.Schema
  ( OverlayDeps(..)
  , OverlayFieldDef(..)
  , OverlayFieldType(..)
  , OverlaySchema(..)
  , OverlayStorage(..)
  )
import Topo.Planet (defaultPlanetConfig, defaultWorldSlice)
import Topo.Simulation (SimContext(..), SimNode(..), SimNodeId(..))
import Topo.Weather (weatherChunkToOverlay, weatherOverlaySchema)
import Topo.World (TerrainWorld(..))

spec :: Spec
spec = describe "AutoTick scheduler" $ do
  it "skips unavailable worlds without queuing manual pending ticks" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      rsp <- dispatch app "set_sim_auto_tick" (object ["enabled" .= True, "rate" .= (1.0 :: Double)])
      srSuccess rsp `shouldBe` True

      threadDelay 250000

      let handles = appHandles app
      ui <- getUiSnapshot (ahUiHandle handles)
      uiSimTickCount ui `shouldBe` 0
      dag <- getSimDagSnapshot (ahSimulationHandle handles)
      sdsPendingTick dag `shouldBe` Nothing
      sdsTickLogs dag `shouldBe` []

  it "pauses at a zero normalized rate" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      installWorld app
      rsp <- dispatch app "set_sim_auto_tick" (object ["enabled" .= True, "rate" .= (0.0 :: Double)])
      srSuccess rsp `shouldBe` True

      threadDelay 250000

      ui <- getUiSnapshot (ahUiHandle (appHandles app))
      uiSimAutoTick ui `shouldBe` True
      uiSimTickRate ui `shouldBe` 0
      uiSimTickCount ui `shouldBe` 0

  it "ticks a headless world through the same state-update path as manual ticks" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      installWorld app
      let handles = appHandles app
      terrainSnap0 <- getTerrainSnapshot (ahDataHandle handles)
      let terrainVersion0 = tsVersion terrainSnap0
          weatherVersion0 = tsWeatherVersion terrainSnap0
      version0 <- readSnapshotVersion (ahSnapshotVersionRef handles)

      rsp <- dispatch app "set_sim_auto_tick" (object ["enabled" .= True, "rate" .= (1.0 :: Double)])
      srSuccess rsp `shouldBe` True

      advanced <- awaitTrue 100 $ do
        ui <- getUiSnapshot (ahUiHandle handles)
        pure (uiSimTickCount ui >= 1)
      advanced `shouldBe` True

      terrainSnap <- getTerrainSnapshot (ahDataHandle handles)
      tsVersion terrainSnap `shouldBe` terrainVersion0
      tsWeatherVersion terrainSnap `shouldSatisfy` (> weatherVersion0)
      version1 <- readSnapshotVersion (ahSnapshotVersionRef handles)
      version1 `shouldSatisfy` (> version0)
      dag <- getSimDagSnapshot (ahSimulationHandle handles)
      sdsPendingTick dag `shouldBe` Nothing
      map stleStatus (sdsTickLogs dag) `shouldSatisfy` elem (Text.pack "completed")

  it "auto ticks builtin weather and plugin simulation nodes through the actor DAG" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      installPluginWorld app
      let handles = appHandles app

      rsp <- dispatch app "set_sim_auto_tick" (object ["enabled" .= True, "rate" .= (1.0 :: Double)])
      srSuccess rsp `shouldBe` True

      advanced <- awaitTrue 100 $ do
        ui <- getUiSnapshot (ahUiHandle handles)
        pure (uiSimTickCount ui >= 1)
      advanced `shouldBe` True

      terrainSnap <- getTerrainSnapshot (ahDataHandle handles)
      pluginValue <- case lookupOverlay pluginOverlayName (tsOverlayStore terrainSnap) >>= firstPluginOverlayValue of
        Just value -> pure value
        Nothing -> expectationFailure "Expected plugin overlay after auto tick" >> pure 0
      pluginValue `shouldBe` 1

      dag <- getSimDagSnapshot (ahSimulationHandle handles)
      sdsLevels dag `shouldBe` [[Text.pack "weather"], [pluginOverlayName]]
      let pluginNodes = filter ((== Just pluginOverlayName) . sdnsPlugin) (sdsNodes dag)
      case pluginNodes of
        [node] -> sdnsStatus node `shouldBe` Text.pack "completed"
        _ -> expectationFailure "Expected one executable plugin node in auto tick DAG"

  it "keeps commands responsive and atlas queues bounded while max-rate auto ticking" $ do
    completed <- timeout 7000000 $
      withHeadlessApp defaultHeadlessConfig $ \app -> do
        workerStarted <- newEmptyMVar
        runCountRef <- newIORef (0 :: Int)
        installResponsiveWorld app workerStarted runCountRef
        let handles = appHandles app

        viewRsp <- dispatch app "set_view_mode" (object ["mode" .= (Text.pack "weather")])
        srSuccess viewRsp `shouldBe` True
        _ <- drainAtlasJobs (ahAtlasManagerHandle handles)

        rsp <- dispatch app "set_sim_auto_tick" (object ["enabled" .= True, "rate" .= (1.0 :: Double)])
        srSuccess rsp `shouldBe` True

        workerStartedResult <- timeout 1000000 (readMVar workerStarted)
        workerStartedResult `shouldBe` Just ()

        (latencyMs, stateRspResult) <- timedMillis $
          timeout 250000 (dispatch app "get_sim_state" Null)
        case stateRspResult of
          Nothing -> expectationFailure "get_sim_state timed out while auto tick worker was in-flight"
          Just stateRsp -> srSuccess stateRsp `shouldBe` True
        latencyMs `shouldSatisfy` (< 250)

        advanced <- awaitTrue 100 $ do
          ui <- getUiSnapshot (ahUiHandle handles)
          pure (uiSimTickCount ui >= 2)
        advanced `shouldBe` True

        uiDuring <- getUiSnapshot (ahUiHandle handles)
        runsDuring <- readRunCount runCountRef
        runsDuring `shouldSatisfy` (<= fromIntegral (uiSimTickCount uiDuring) + 1)
        dagDuring <- getSimDagSnapshot (ahSimulationHandle handles)
        sdsPendingTick dagDuring `shouldBe` Nothing

        stopRsp <- dispatch app "set_sim_auto_tick" (object ["enabled" .= False])
        srSuccess stopRsp `shouldBe` True
        idle <- awaitStoppedAutoTickIdle handles runCountRef
        idle `shouldBe` True
        dagAfterIdle <- getSimDagSnapshot (ahSimulationHandle handles)
        sdsPendingTick dagAfterIdle `shouldBe` Nothing

        jobs <- drainAtlasJobs (ahAtlasManagerHandle handles)
        length jobs `shouldSatisfy` (\n -> n > 0 && n <= length allZoomStages)
        all ((== ViewWeather) . ajViewMode) jobs `shouldBe` True
        terrainSnap <- getTerrainSnapshot (ahDataHandle handles)
        all ((== tsWeatherVersion terrainSnap) . atlasKeyVersion . ajKey) jobs `shouldBe` True
        dag <- getSimDagSnapshot (ahSimulationHandle handles)
        sdsPendingTick dag `shouldBe` Nothing
        map stleStatus (sdsTickLogs dag) `shouldSatisfy` notElem (Text.pack "failed")
    completed `shouldBe` Just ()

appHandles :: HeadlessApp -> ActorHandles
appHandles app = ccActorHandles (headlessCommandContext app)

dispatch :: HeadlessApp -> Text -> Value -> IO SeerResponse
dispatch app method params = dispatchCommand (headlessCommandContext app) SeerCommand
  { scId = 1
  , scMethod = method
  , scParams = params
  }

installWorld :: HeadlessApp -> IO ()
installWorld app = do
  let handles = appHandles app
  replaceTerrainData (ahDataHandle handles) testWorld
  _ <- getTerrainSnapshot (ahDataHandle handles)
  setSimWorld (ahSimulationHandle handles) testWorld
  dag <- getSimDagSnapshot (ahSimulationHandle handles)
  sdsAvailable dag `shouldBe` True

installPluginWorld :: HeadlessApp -> IO ()
installPluginWorld app = do
  let handles = appHandles app
  replaceTerrainData (ahDataHandle handles) pluginTestWorld
  _ <- getTerrainSnapshot (ahDataHandle handles)
  setSimWorldWithNodes (ahSimulationHandle handles) pluginTestWorld [pluginSimulationBinding]
  dag <- getSimDagSnapshot (ahSimulationHandle handles)
  sdsAvailable dag `shouldBe` True

installResponsiveWorld :: HeadlessApp -> MVar () -> IORef Int -> IO ()
installResponsiveWorld app workerStarted runCountRef = do
  let handles = appHandles app
  replaceTerrainData (ahDataHandle handles) responsiveTestWorld
  _ <- getTerrainSnapshot (ahDataHandle handles)
  setSimWorldWithNodes (ahSimulationHandle handles) responsiveTestWorld [responsiveSimulationBinding workerStarted runCountRef]
  dag <- getSimDagSnapshot (ahSimulationHandle handles)
  sdsAvailable dag `shouldBe` True

awaitTrue :: Int -> IO Bool -> IO Bool
awaitTrue 0 action = action
awaitTrue retries action = do
  ok <- action
  if ok
    then pure True
    else do
      threadDelay 20000
      awaitTrue (retries - 1) action

timedMillis :: IO a -> IO (Double, a)
timedMillis action = do
  started <- getMonotonicTimeNSec
  result <- action
  finished <- getMonotonicTimeNSec
  pure (fromIntegral (finished - started) / (1e6 :: Double), result)

readRunCount :: IORef Int -> IO Int
readRunCount ref = atomicModifyIORef' ref (\n -> (n, n))

awaitStoppedAutoTickIdle :: ActorHandles -> IORef Int -> IO Bool
awaitStoppedAutoTickIdle handles runCountRef = awaitTrue 20 $ do
  ui0 <- getUiSnapshot (ahUiHandle handles)
  runs0 <- readRunCount runCountRef
  threadDelay 300000
  ui1 <- getUiSnapshot (ahUiHandle handles)
  runs1 <- readRunCount runCountRef
  dag1 <- getSimDagSnapshot (ahSimulationHandle handles)
  let tick0 = fromIntegral (uiSimTickCount ui0) :: Int
      tick1 = fromIntegral (uiSimTickCount ui1) :: Int
      nodeStatuses = map sdnsStatus (sdsNodes dag1)
  pure
    ( not (uiSimAutoTick ui1)
      && runs0 == runs1
      && tick0 == tick1
      && runs1 == tick1
      && sdsPendingTick dag1 == Nothing
      && notElem (Text.pack "running") nodeStatuses
    )

testWorld :: TerrainWorld
testWorld = withSeedWeather
  (setClimateChunk (ChunkId 0) climate (setTerrainChunk (ChunkId 0) terrain world0))
  (ChunkId 0)
  climate
  where
    config = WorldConfig { wcChunkSize = 8 }
    terrain = generateTerrainChunk config (const 0.5)
    climate0 = emptyClimateChunk config
    tileCount = U.length (ccTempAvg climate0)
    climate = testClimate tileCount climate0
    world0 = emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig defaultWorldSlice

pluginTestWorld :: TerrainWorld
pluginTestWorld = withSeedPluginOverlay tileCount 0 $
  withSeedWeather
    (setClimateChunk (ChunkId 0) climate (setTerrainChunk (ChunkId 0) terrain world0))
    (ChunkId 0)
    climate
  where
    config = WorldConfig { wcChunkSize = 8 }
    terrain = generateTerrainChunk config (const 0.5)
    climate0 = emptyClimateChunk config
    tileCount = U.length (ccTempAvg climate0)
    climate = testClimate tileCount climate0
    world0 = emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig defaultWorldSlice

responsiveTestWorld :: TerrainWorld
responsiveTestWorld = withSeedPluginOverlay tileCount 0 $
  withSeedWeather
    (setClimateChunk (ChunkId 0) climate (setTerrainChunk (ChunkId 0) terrain world0))
    (ChunkId 0)
    climate
  where
    config = WorldConfig { wcChunkSize = 16 }
    terrain = generateTerrainChunk config (const 0.5)
    climate0 = emptyClimateChunk config
    tileCount = U.length (ccTempAvg climate0)
    climate = testClimate tileCount climate0
    world0 = emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig defaultWorldSlice

testClimate :: Int -> ClimateChunk -> ClimateChunk
testClimate tileCount climate0 = climate0
  { ccTempAvg = U.replicate tileCount 0.55
  , ccPrecipAvg = U.replicate tileCount 0.45
  , ccWindDirAvg = U.replicate tileCount 0.3
  , ccWindSpdAvg = U.replicate tileCount 0.25
  , ccHumidityAvg = U.replicate tileCount 0.4
  }

withSeedWeather :: TerrainWorld -> ChunkId -> ClimateChunk -> TerrainWorld
withSeedWeather world (ChunkId chunkId) climate =
  world { twOverlays = insertOverlay weatherOverlay (twOverlays world) }
  where
    weatherOverlay = Overlay
      { ovSchema = weatherOverlaySchema
      , ovData = DenseData (IntMap.singleton chunkId (weatherChunkToOverlay (mkSeedWeatherChunk climate)))
      , ovProvenance = OverlayProvenance
          { opSeed = 0
          , opVersion = 1
          , opSource = Text.pack "auto-tick-spec"
          }
      }

mkSeedWeatherChunk :: ClimateChunk -> WeatherChunk
mkSeedWeatherChunk climate = WeatherChunk
  { wcTemp = ccTempAvg climate
  , wcHumidity = ccHumidityAvg climate
  , wcWindDir = ccWindDirAvg climate
  , wcWindSpd = ccWindSpdAvg climate
  , wcPressure = U.replicate tileCount 0.5
  , wcPrecip = ccPrecipAvg climate
  , wcCloudCover = U.replicate tileCount 0.0
  , wcCloudWater = U.replicate tileCount 0.0
  , wcCloudCoverLow  = U.replicate tileCount 0.0
  , wcCloudCoverMid  = U.replicate tileCount 0.0
  , wcCloudCoverHigh = U.replicate tileCount 0.0
  , wcCloudWaterLow  = U.replicate tileCount 0.0
  , wcCloudWaterMid  = U.replicate tileCount 0.0
  , wcCloudWaterHigh = U.replicate tileCount 0.0
  }
  where
    tileCount = U.length (ccTempAvg climate)

pluginOverlayName :: Text
pluginOverlayName = Text.pack "plugin-sim"

pluginOverlaySchema :: OverlaySchema
pluginOverlaySchema = OverlaySchema
  { osName = pluginOverlayName
  , osVersion = Text.pack "1.0.0"
  , osDescription = Text.pack "Auto tick plugin overlay"
  , osFields =
      [ OverlayFieldDef
          { ofdName = Text.pack "value"
          , ofdType = OFFloat
          , ofdDefault = Number 0
          , ofdIndexed = False
          , ofdRenamedFrom = Nothing
          }
      ]
  , osStorage = StorageDense
  , osDependencies = OverlayDeps { odTerrain = False, odOverlays = [Text.pack "weather"] }
  , osFieldIndex = Map.fromList [(Text.pack "value", 0)]
  }

seedPluginOverlay :: Int -> Float -> Overlay
seedPluginOverlay tileCount value = Overlay
  { ovSchema = pluginOverlaySchema
  , ovData = DenseData (IntMap.singleton 0 (V.singleton (U.replicate tileCount value)))
  , ovProvenance = OverlayProvenance
      { opSeed = 0
      , opVersion = 1
      , opSource = Text.pack "auto-tick-spec-plugin"
      }
  }

withSeedPluginOverlay :: Int -> Float -> TerrainWorld -> TerrainWorld
withSeedPluginOverlay tileCount value world =
  world { twOverlays = insertOverlay (seedPluginOverlay tileCount value) (twOverlays world) }

firstPluginOverlayValue :: Overlay -> Maybe Float
firstPluginOverlayValue overlay = case ovData overlay of
  DenseData chunks -> do
    (_chunkId, fields) <- IntMap.lookupMin chunks
    values <- fields V.!? 0
    if U.null values then Nothing else Just (values U.! 0)
  SparseData{} -> Nothing

incrementPluginOverlay :: Overlay -> Overlay
incrementPluginOverlay overlay = overlay
  { ovData = case ovData overlay of
      DenseData chunks -> DenseData (fmap (V.map (U.map (+ 1))) chunks)
      sparse@SparseData{} -> sparse
  }

pluginSimulationBinding :: SimulationNodeBinding
pluginSimulationBinding = SimulationNodeBinding
  { snbNode = SimNodeReader
      { snrId = SimNodeId pluginOverlayName
      , snrOverlayName = pluginOverlayName
      , snrDependencies = [SimNodeId (Text.pack "weather")]
      , snrReadTick = \ctx overlay ->
          if Map.member (Text.pack "weather") (scOverlays ctx)
            then pure (Right (incrementPluginOverlay overlay))
            else pure (Left (Text.pack "missing weather dependency"))
      }
  , snbKind = Text.pack "plugin"
  , snbPlugin = Just pluginOverlayName
  }

responsiveSimulationBinding :: MVar () -> IORef Int -> SimulationNodeBinding
responsiveSimulationBinding workerStarted runCountRef = SimulationNodeBinding
  { snbNode = SimNodeReader
      { snrId = SimNodeId (Text.pack "responsive-slow-plugin")
      , snrOverlayName = pluginOverlayName
      , snrDependencies = [SimNodeId (Text.pack "weather")]
      , snrReadTick = \ctx overlay ->
          if Map.member (Text.pack "weather") (scOverlays ctx)
            then do
              _ <- atomicModifyIORef' runCountRef (\n -> let n' = n + 1 in (n', n'))
              _ <- tryPutMVar workerStarted ()
              threadDelay 150000
              pure (Right (incrementPluginOverlay overlay))
            else pure (Left (Text.pack "missing weather dependency"))
      }
  , snbKind = Text.pack "plugin"
  , snbPlugin = Just pluginOverlayName
  }
