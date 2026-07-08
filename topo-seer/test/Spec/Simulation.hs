{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Spec.Simulation (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, readMVar, tryPutMVar)
import Control.Exception (bracket)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Aeson (Value(..), toJSON)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Hyperspace.Actor (ActorHandle, ActorSystem, Protocol, get, newActorSystem, shutdownActorSystem)
import System.Timeout (timeout)
import Test.Hspec
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

import Actor.AtlasCache (atlasKeyFor, atlasKeyVersion, terrainSnapshotViewVersion)
import Actor.AtlasManager (AtlasJob(..), AtlasManager, drainAtlasJobs)
import Actor.Data (Data, DataSnapshot(..), TerrainSnapshot(..), defaultTerrainGeoContext, getTerrainSnapshot, replaceTerrainData)
import Actor.Log (Log, getLogSnapshot, leMessage, lsEntries)
import Actor.Simulation
  ( AutoTickStepResult(..)
  , Simulation
  , SimulationDagNodeSnapshot(..)
  , SimulationDagSnapshot(..)
  , SimulationNodeBinding(..)
  , SimulationTickLogEntry(..)
  , autoTickStep
  , autoTickWeatherPublishIntervalNs
  , flushSimWeatherPublication
  , getSimDagSnapshot
  , requestSimTick
  , rebindSimNodes
  , setSimHandles
  , setSimWorld
  , setSimWorldWithNodes
  )
import Actor.SnapshotReceiver
  ( SnapshotVersionRef
  , TerrainSnapshotRef
  , newDataSnapshotRef
  , newTerrainSnapshotRef
  , newSnapshotVersionRef
  , readSnapshotVersion
  , readTerrainSnapshot
  )
import Actor.UI
  ( Ui
  , ViewMode(..)
  , getUiSnapshot
  , setUiDayNightEnabled
  , setUiSimAutoTick
  , setUiSimTickRate
  , setUiViewMode
  , setUiZoom
  , uiRenderWaterLevel
  , uiSimTickCount
  , uiZoom
  )
import Seer.Render.ZoomStage (ZoomStage(..), allZoomStages, orderedZoomStagesForZoom, stageForZoom)

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
import Topo.Overlay (Overlay(..), OverlayData(..), OverlayProvenance(..), emptyOverlayStore, insertOverlay, lookupOverlay)
import Topo.Overlay.Schema
  ( OverlayDeps(..)
  , OverlayFieldDef(..)
  , OverlayFieldType(..)
  , OverlaySchema(..)
  , OverlayStorage(..)
  )
import Topo.Planet (defaultPlanetConfig, defaultWorldSlice)
import Topo.Simulation
  ( SimContext(..)
  , SimNode(..)
  , SimNodeId(..)
  , SimulationCatchUpPolicy(..)
  , SimulationScheduleState(..)
  , hourlyScheduleDecl
  )
import Topo.Weather (WeatherConfig(..), defaultWeatherConfig, weatherChunkToOverlay, weatherOverlaySchema)
import Topo.World (TerrainWorld(..))
import Topo.WorldGen (WorldGenConfig(..), defaultWorldGenConfig)

withSystem :: (ActorSystem -> IO a) -> IO a
withSystem = bracket newActorSystem shutdownActorSystem

awaitTrue :: Int -> IO Bool -> IO Bool
awaitTrue 0 action = action
awaitTrue retries action = do
  ok <- action
  if ok
    then pure True
    else do
      threadDelay 20000
      awaitTrue (retries - 1) action

containsText :: Text -> [Text] -> Bool
containsText needle = any (Text.isInfixOf needle)

firstMatchIndex :: Text -> [Text] -> Maybe Int
firstMatchIndex needle = go 0
  where
    go _ [] = Nothing
    go idx (message : rest)
      | Text.isInfixOf needle message = Just idx
      | otherwise = go (idx + 1) rest

firstWeatherTemp :: IntMap.IntMap WeatherChunk -> Maybe Float
firstWeatherTemp chunks = do
  (_key, weatherChunk) <- IntMap.lookupMin chunks
  if U.null (wcTemp weatherChunk)
    then Nothing
    else Just (wcTemp weatherChunk U.! 0)

atlasJobStage :: AtlasJob -> (Int, Int)
atlasJobStage job = (ajHexRadius job, ajAtlasScale job)

zoomStagePair :: ZoomStage -> (Int, Int)
zoomStagePair stage = (zsHexRadius stage, zsAtlasScale stage)

seedWeatherOverlay :: IntMap.IntMap WeatherChunk -> Overlay
seedWeatherOverlay = seedWeatherOverlayWithSchedule Nothing

seedWeatherOverlayWithSchedule :: Maybe SimulationScheduleState -> IntMap.IntMap WeatherChunk -> Overlay
seedWeatherOverlayWithSchedule schedule weatherChunks = Overlay
  { ovSchema = weatherOverlaySchema
  , ovData = DenseData (IntMap.map weatherChunkToOverlay weatherChunks)
  , ovProvenance = OverlayProvenance
      { opSeed = 0
      , opVersion = 1
      , opSource = Text.pack "simulation-spec"
      , opSchedule = schedule
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

withSeedWeather :: TerrainWorld -> ChunkId -> ClimateChunk -> TerrainWorld
withSeedWeather world (ChunkId chunkId) climate =
  world
    { twOverlays = insertOverlay weatherOverlay (twOverlays world)
    }
  where
    weatherOverlay = seedWeatherOverlay (IntMap.singleton chunkId (mkSeedWeatherChunk climate))

withSeedWeatherSchedule :: SimulationScheduleState -> TerrainWorld -> ChunkId -> ClimateChunk -> TerrainWorld
withSeedWeatherSchedule schedule world (ChunkId chunkId) climate =
  world
    { twOverlays = insertOverlay weatherOverlay (twOverlays world)
    }
  where
    weatherOverlay = seedWeatherOverlayWithSchedule (Just schedule) (IntMap.singleton chunkId (mkSeedWeatherChunk climate))

pluginOverlayName :: Text
pluginOverlayName = Text.pack "plugin-sim"

pluginOverlaySchema :: OverlaySchema
pluginOverlaySchema = OverlaySchema
  { osName = pluginOverlayName
  , osVersion = Text.pack "1.0.0"
  , osDescription = Text.pack "Simulation test plugin overlay"
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
      , opSource = Text.pack "simulation-spec-plugin"
      , opSchedule = Nothing
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
      , snrSchedule = hourlyScheduleDecl
      , snrReadTick = \ctx overlay ->
          if Map.member (Text.pack "weather") (scOverlays ctx)
            then pure (Right (incrementPluginOverlay overlay))
            else pure (Left (Text.pack "missing weather dependency"))
      }
  , snbKind = Text.pack "plugin"
  , snbPlugin = Just pluginOverlayName
  }

coalescingTestWorld :: TerrainWorld
coalescingTestWorld = withSeedWeather
  (setClimateChunk (ChunkId 0) climate (setTerrainChunk (ChunkId 0) terrain world0))
  (ChunkId 0)
  climate
  where
    config = WorldConfig { wcChunkSize = 8 }
    terrain = generateTerrainChunk config (const 0.5)
    climate0 = emptyClimateChunk config
    tileCount = U.length (ccTempAvg climate0)
    climate = climate0
      { ccTempAvg = U.replicate tileCount 0.6
      , ccPrecipAvg = U.replicate tileCount 0.5
      , ccWindDirAvg = U.replicate tileCount 0.4
      , ccWindSpdAvg = U.replicate tileCount 0.35
      , ccHumidityAvg = U.replicate tileCount 0.5
      }
    world0 = emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig defaultWorldSlice

withConfiguredSimulation
  :: ( ActorHandle Simulation (Protocol Simulation)
     -> ActorHandle Data (Protocol Data)
     -> ActorHandle Ui (Protocol Ui)
     -> TerrainSnapshotRef
     -> SnapshotVersionRef
     -> ActorHandle AtlasManager (Protocol AtlasManager)
     -> IO a
     )
  -> IO a
withConfiguredSimulation action = withSystem $ \system -> do
  simHandle <- get @Simulation system
  dataHandle <- get @Data system
  logHandle <- get @Log system
  uiHandle <- get @Ui system
  dataSnapshotRef <- newDataSnapshotRef (DataSnapshot 0 0 Nothing)
  terrainSnapshotRef <- newTerrainSnapshotRef emptyTerrainSnap
  snapshotVersionRef <- newSnapshotVersionRef
  atlasHandle <- get @AtlasManager system

  setSimHandles simHandle dataHandle logHandle uiHandle dataSnapshotRef terrainSnapshotRef snapshotVersionRef atlasHandle
  action simHandle dataHandle uiHandle terrainSnapshotRef snapshotVersionRef atlasHandle
  where
    emptyTerrainSnap = TerrainSnapshot 0 0 0 0 0 0 mempty mempty mempty mempty mempty mempty mempty mempty mempty emptyOverlayStore defaultTerrainGeoContext

installCoalescingWorld
  :: ActorHandle Data (Protocol Data)
  -> ActorHandle Simulation (Protocol Simulation)
  -> IO TerrainSnapshot
installCoalescingWorld dataHandle simHandle = do
  replaceTerrainData dataHandle coalescingTestWorld
  initialTerrainSnap <- getTerrainSnapshot dataHandle
  setSimWorld simHandle coalescingTestWorld
  dag <- getSimDagSnapshot simHandle
  sdsAvailable dag `shouldBe` True
  pure initialTerrainSnap

waitPastAutoWeatherPublishInterval :: IO ()
waitPastAutoWeatherPublishInterval =
  threadDelay (fromIntegral (autoTickWeatherPublishIntervalNs `div` 1000 + 20000))

emptyTerrainSnapshotForTests :: TerrainSnapshot
emptyTerrainSnapshotForTests =
  TerrainSnapshot 0 0 0 0 0 0 mempty mempty mempty mempty mempty mempty mempty mempty mempty emptyOverlayStore defaultTerrainGeoContext

spec :: Spec
spec = describe "Simulation actor" $ do
  it "binds world before processing first post-load tick request" $ withSystem $ \system -> do
    simHandle <- get @Simulation system
    dataHandle <- get @Data system
    logHandle <- get @Log system
    uiHandle <- get @Ui system
    dataSnapshotRef <- newDataSnapshotRef (DataSnapshot 0 0 Nothing)
    terrainSnapshotRef <- newTerrainSnapshotRef emptyTerrainSnapshotForTests
    snapshotVersionRef <- newSnapshotVersionRef
    atlasHandle <- get @AtlasManager system

    setSimHandles simHandle dataHandle logHandle uiHandle dataSnapshotRef terrainSnapshotRef snapshotVersionRef atlasHandle

    let config = WorldConfig { wcChunkSize = 8 }
        chunk = generateTerrainChunk config (const 0.5)
        climate0 = emptyClimateChunk config
        tileCount = U.length (ccTempAvg climate0)
        climate = climate0
          { ccTempAvg = U.replicate tileCount 0.55
          , ccPrecipAvg = U.replicate tileCount 0.45
          , ccWindDirAvg = U.replicate tileCount 0.3
          , ccWindSpdAvg = U.replicate tileCount 0.25
          , ccHumidityAvg = U.replicate tileCount 0.4
          }
        world0 = emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig defaultWorldSlice
        world1 = withSeedWeather
          (setClimateChunk (ChunkId 0) climate (setTerrainChunk (ChunkId 0) chunk world0))
          (ChunkId 0)
          climate

    replaceTerrainData dataHandle world1
    _ <- getTerrainSnapshot dataHandle
    setSimWorld simHandle world1
    requestSimTick simHandle 1

    tickAdvanced <- awaitTrue 500 $ do
      uiSnap <- getUiSnapshot uiHandle
      pure (uiSimTickCount uiSnap >= 1)
    tickAdvanced `shouldBe` True

    -- Poll until the log actor has processed the "tick completed" cast.
    logReady <- awaitTrue 500 $ do
      snap <- getLogSnapshot logHandle
      let msgs = map leMessage (lsEntries snap)
      pure (containsText (Text.pack "simulation: tick 1 completed") msgs)
    logReady `shouldBe` True

    logSnap <- getLogSnapshot logHandle
    let messages = map leMessage (lsEntries logSnap)
        acceptedIdx = firstMatchIndex (Text.pack "simulation: setWorld accepted") messages
        completedIdx = firstMatchIndex (Text.pack "simulation: tick 1 completed") messages

    acceptedIdx `shouldSatisfy` maybe False (const True)
    completedIdx `shouldSatisfy` maybe False (const True)
    case (acceptedIdx, completedIdx) of
      (Just accepted, Just completed) -> accepted `shouldSatisfy` (<= completed)
      _ -> expectationFailure "Expected setWorld acceptance and tick completion log entries"

  it "applies a queued tick requested before world binding" $ withSystem $ \system -> do
    simHandle <- get @Simulation system
    dataHandle <- get @Data system
    logHandle <- get @Log system
    uiHandle <- get @Ui system
    dataSnapshotRef <- newDataSnapshotRef (DataSnapshot 0 0 Nothing)
    terrainSnapshotRef <- newTerrainSnapshotRef emptyTerrainSnapshotForTests
    snapshotVersionRef <- newSnapshotVersionRef
    atlasHandle <- get @AtlasManager system

    setSimHandles simHandle dataHandle logHandle uiHandle dataSnapshotRef terrainSnapshotRef snapshotVersionRef atlasHandle

    requestSimTick simHandle 1

    let config = WorldConfig { wcChunkSize = 8 }
        chunk = generateTerrainChunk config (const 0.5)
        climate0 = emptyClimateChunk config
        tileCount = U.length (ccTempAvg climate0)
        climate = climate0
          { ccTempAvg = U.replicate tileCount 0.55
          , ccPrecipAvg = U.replicate tileCount 0.45
          , ccWindDirAvg = U.replicate tileCount 0.3
          , ccWindSpdAvg = U.replicate tileCount 0.25
          , ccHumidityAvg = U.replicate tileCount 0.4
          }
        world0 = emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig defaultWorldSlice
        world1 = withSeedWeather
          (setClimateChunk (ChunkId 0) climate (setTerrainChunk (ChunkId 0) chunk world0))
          (ChunkId 0)
          climate

    replaceTerrainData dataHandle world1
    _ <- getTerrainSnapshot dataHandle
    setSimWorld simHandle world1

    tickAdvanced <- awaitTrue 500 $ do
      uiSnap <- getUiSnapshot uiHandle
      pure (uiSimTickCount uiSnap >= 1)
    tickAdvanced `shouldBe` True

    -- Poll until the log actor has processed the "tick completed" cast.
    logReady <- awaitTrue 500 $ do
      snap <- getLogSnapshot logHandle
      let msgs = map leMessage (lsEntries snap)
      pure (containsText (Text.pack "simulation: tick 1 completed") msgs)
    logReady `shouldBe` True

    logSnap <- getLogSnapshot logHandle
    let messages = map leMessage (lsEntries logSnap)
    containsText (Text.pack "simulation: tick deferred (not ready)") messages `shouldBe` True
    containsText (Text.pack "simulation: tick 1 completed") messages `shouldBe` True

  it "processes tick requests and publishes updated state" $ withSystem $ \system -> do
    simHandle <- get @Simulation system
    dataHandle <- get @Data system
    logHandle <- get @Log system
    uiHandle <- get @Ui system
    dataSnapshotRef <- newDataSnapshotRef (DataSnapshot 0 0 Nothing)
    terrainSnapshotRef <- newTerrainSnapshotRef emptyTerrainSnapshotForTests
    snapshotVersionRef <- newSnapshotVersionRef
    atlasHandle <- get @AtlasManager system

    setSimHandles simHandle dataHandle logHandle uiHandle dataSnapshotRef terrainSnapshotRef snapshotVersionRef atlasHandle

    let config = WorldConfig { wcChunkSize = 8 }
        chunk = generateTerrainChunk config (const 0.5)
        climate0 = emptyClimateChunk config
        tileCount = U.length (ccTempAvg climate0)
        climate = climate0
          { ccTempAvg = U.replicate tileCount 0.6
          , ccPrecipAvg = U.replicate tileCount 0.5
          , ccWindDirAvg = U.replicate tileCount 0.4
          , ccWindSpdAvg = U.replicate tileCount 0.35
          , ccHumidityAvg = U.replicate tileCount 0.5
          }
        world0 = emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig defaultWorldSlice
        world1 = withSeedWeather
          (setClimateChunk (ChunkId 0) climate (setTerrainChunk (ChunkId 0) chunk world0))
          (ChunkId 0)
          climate

    replaceTerrainData dataHandle world1
    initialTerrainSnap <- getTerrainSnapshot dataHandle
    let baseVersion0 = tsVersion initialTerrainSnap
        weatherVersion0 = tsWeatherVersion initialTerrainSnap
    setSimWorld simHandle world1
    setUiDayNightEnabled uiHandle True
    uiSnapBeforeTick <- getUiSnapshot uiHandle
    let waterLevel0 = uiRenderWaterLevel uiSnapBeforeTick
        elevationKey0 = atlasKeyFor ViewElevation waterLevel0 initialTerrainSnap
        weatherKey0 = atlasKeyFor ViewWeather waterLevel0 initialTerrainSnap
    requestSimTick simHandle 1

    tickAdvanced <- awaitTrue 500 $ do
      uiSnap <- getUiSnapshot uiHandle
      pure (uiSimTickCount uiSnap >= 1)
    tickAdvanced `shouldBe` True

    terrainSnap <- getTerrainSnapshot dataHandle
    tsVersion terrainSnap `shouldBe` baseVersion0
    tsWeatherVersion terrainSnap `shouldSatisfy` (> weatherVersion0)
    atlasKeyFor ViewElevation waterLevel0 terrainSnap `shouldBe` elevationKey0
    atlasKeyFor ViewWeather waterLevel0 terrainSnap `shouldNotBe` weatherKey0
    tempAfterTick1 <- case firstWeatherTemp (tsWeatherChunks terrainSnap) of
      Just t -> pure t
      Nothing -> expectationFailure "Expected weather chunks after tick 1" >> pure 0

    requestSimTick simHandle 2
    tickAdvanced2 <- awaitTrue 500 $ do
      uiSnap <- getUiSnapshot uiHandle
      pure (uiSimTickCount uiSnap >= 2)
    tickAdvanced2 `shouldBe` True

    terrainSnap2 <- getTerrainSnapshot dataHandle
    tempAfterTick2 <- case firstWeatherTemp (tsWeatherChunks terrainSnap2) of
      Just t -> pure t
      Nothing -> expectationFailure "Expected weather chunks after tick 2" >> pure tempAfterTick1
    tempAfterTick2 `shouldNotBe` tempAfterTick1

    snapshotPublished <- awaitTrue 500 $ do
      terrainRef <- readTerrainSnapshot terrainSnapshotRef
      pure (tsWeatherVersion terrainRef > weatherVersion0 && tsVersion terrainRef == baseVersion0)
    snapshotPublished `shouldBe` True

    logSnap <- getLogSnapshot logHandle
    let messages = map leMessage (lsEntries logSnap)
    containsText (Text.pack "simulation: tick") messages `shouldBe` True
    atlasJobs <- drainAtlasJobs atlasHandle
    length atlasJobs `shouldBe` 5
    map ajViewMode atlasJobs `shouldBe` replicate 5 ViewElevation
    map ajKey atlasJobs `shouldSatisfy` all (== elevationKey0)

    dagSnapshot <- getSimDagSnapshot simHandle
    sdsAvailable dagSnapshot `shouldBe` True
    map sdnsStatus (sdsNodes dagSnapshot) `shouldSatisfy` elem (Text.pack "completed")
    map stleStatus (sdsTickLogs dagSnapshot) `shouldSatisfy` elem (Text.pack "completed")

  it "coalesces rapid visible ViewWeather auto ticks and eventually publishes the latest weather" $
    withConfiguredSimulation $ \simHandle dataHandle uiHandle terrainSnapshotRef snapshotVersionRef atlasHandle -> do
      initialTerrainSnap <- installCoalescingWorld dataHandle simHandle
      setUiViewMode uiHandle ViewWeather
      setUiZoom uiHandle 2.5
      setUiSimAutoTick uiHandle True
      setUiSimTickRate uiHandle 1.0
      uiSnap <- getUiSnapshot uiHandle
      version0 <- readSnapshotVersion snapshotVersionRef
      let waterLevel = uiRenderWaterLevel uiSnap
          currentStage = zoomStagePair (stageForZoom (uiZoom uiSnap))
          initialWeatherVersion = tsWeatherVersion initialTerrainSnap
      _ <- drainAtlasJobs atlasHandle

      autoTickStep simHandle Nothing `shouldReturn` AutoTickApplied 1
      version1 <- readSnapshotVersion snapshotVersionRef
      version1 `shouldSatisfy` (> version0)
      terrainSnap1 <- getTerrainSnapshot dataHandle
      publishedSnap1 <- readTerrainSnapshot terrainSnapshotRef
      tsWeatherVersion terrainSnap1 `shouldSatisfy` (> initialWeatherVersion)
      tsWeatherVersion publishedSnap1 `shouldBe` tsWeatherVersion terrainSnap1

      autoTickStep simHandle Nothing `shouldReturn` AutoTickApplied 2
      version2 <- readSnapshotVersion snapshotVersionRef
      terrainSnap2 <- getTerrainSnapshot dataHandle
      publishedSnap2 <- readTerrainSnapshot terrainSnapshotRef
      version2 `shouldBe` version1
      tsWeatherVersion terrainSnap2 `shouldSatisfy` (> tsWeatherVersion terrainSnap1)
      tsWeatherVersion publishedSnap2 `shouldBe` tsWeatherVersion publishedSnap1

      autoTickStep simHandle Nothing `shouldReturn` AutoTickApplied 3
      version3 <- readSnapshotVersion snapshotVersionRef
      terrainSnap3 <- getTerrainSnapshot dataHandle
      publishedSnap3 <- readTerrainSnapshot terrainSnapshotRef
      version3 `shouldBe` version1
      tsWeatherVersion terrainSnap3 `shouldSatisfy` (> tsWeatherVersion terrainSnap2)
      tsWeatherVersion publishedSnap3 `shouldBe` tsWeatherVersion publishedSnap1

      uiAfterRapid <- getUiSnapshot uiHandle
      uiSimTickCount uiAfterRapid `shouldBe` 3
      dagAfterRapid <- getSimDagSnapshot simHandle
      sdsLastTick dagAfterRapid `shouldBe` 3

      rapidJobs <- drainAtlasJobs atlasHandle
      length rapidJobs `shouldBe` 1
      map ajViewMode rapidJobs `shouldBe` [ViewWeather]
      map atlasJobStage rapidJobs `shouldBe` [currentStage]
      map (atlasKeyVersion . ajKey) rapidJobs `shouldBe` [tsWeatherVersion publishedSnap1]
      map (tsWeatherVersion . ajTerrain) rapidJobs `shouldBe` [tsWeatherVersion publishedSnap1]
      map ajSnapshotVersion rapidJobs `shouldBe` [version1]

      waitPastAutoWeatherPublishInterval
      autoTickStep simHandle Nothing `shouldReturn` AutoTickApplied 4
      version4 <- readSnapshotVersion snapshotVersionRef
      version4 `shouldSatisfy` (> version3)
      terrainSnap4 <- getTerrainSnapshot dataHandle
      publishedSnap4 <- readTerrainSnapshot terrainSnapshotRef
      tsWeatherVersion terrainSnap4 `shouldSatisfy` (> tsWeatherVersion terrainSnap3)
      tsWeatherVersion publishedSnap4 `shouldBe` tsWeatherVersion terrainSnap4
      let latestWeatherKey = atlasKeyFor ViewWeather waterLevel terrainSnap4
      latestJobs <- drainAtlasJobs atlasHandle
      length latestJobs `shouldBe` 1
      map ajViewMode latestJobs `shouldBe` [ViewWeather]
      map ajKey latestJobs `shouldBe` [latestWeatherKey]
      map atlasJobStage latestJobs `shouldBe` [currentStage]
      map (tsWeatherVersion . ajTerrain) latestJobs `shouldBe` [tsWeatherVersion terrainSnap4]
      map ajSnapshotVersion latestJobs `shouldBe` [version4]

  it "publishes consecutive manual ViewCloud ticks immediately" $
    withConfiguredSimulation $ \simHandle dataHandle uiHandle terrainSnapshotRef snapshotVersionRef atlasHandle -> do
      initialTerrainSnap <- installCoalescingWorld dataHandle simHandle
      setUiViewMode uiHandle ViewCloud
      uiSnap <- getUiSnapshot uiHandle
      version0 <- readSnapshotVersion snapshotVersionRef
      let waterLevel = uiRenderWaterLevel uiSnap
          expectedStages = map zoomStagePair (orderedZoomStagesForZoom (uiZoom uiSnap))
          assertManualPublish tickTarget previousVersion previousWeatherVersion = do
            requestSimTick simHandle tickTarget
            published <- awaitTrue 500 $ do
              version <- readSnapshotVersion snapshotVersionRef
              pure (version > previousVersion)
            published `shouldBe` True
            version <- readSnapshotVersion snapshotVersionRef
            uiAfterTick <- getUiSnapshot uiHandle
            uiSimTickCount uiAfterTick `shouldBe` tickTarget
            dagAfterTick <- getSimDagSnapshot simHandle
            sdsLastTick dagAfterTick `shouldBe` tickTarget
            terrainSnap <- getTerrainSnapshot dataHandle
            publishedSnap <- readTerrainSnapshot terrainSnapshotRef
            tsWeatherVersion terrainSnap `shouldSatisfy` (> previousWeatherVersion)
            tsWeatherVersion publishedSnap `shouldBe` tsWeatherVersion terrainSnap
            let cloudKey = atlasKeyFor ViewCloud waterLevel terrainSnap
            terrainSnapshotViewVersion ViewCloud publishedSnap `shouldBe` tsWeatherVersion terrainSnap
            atlasKeyVersion cloudKey `shouldBe` tsWeatherVersion terrainSnap
            atlasJobs <- drainAtlasJobs atlasHandle
            length atlasJobs `shouldBe` length allZoomStages
            map ajViewMode atlasJobs `shouldBe` replicate (length allZoomStages) ViewCloud
            map atlasJobStage atlasJobs `shouldBe` expectedStages
            map ajKey atlasJobs `shouldSatisfy` all (== cloudKey)
            map (tsWeatherVersion . ajTerrain) atlasJobs `shouldSatisfy` all (== tsWeatherVersion terrainSnap)
            map ajSnapshotVersion atlasJobs `shouldSatisfy` all (== version)
            pure (version, tsWeatherVersion terrainSnap)
      _ <- drainAtlasJobs atlasHandle

      (version1, weatherVersion1) <- assertManualPublish 1 version0 (tsWeatherVersion initialTerrainSnap)
      _ <- assertManualPublish 2 version1 weatherVersion1
      pure ()

  it "coalesces day/night auto publications and flushes the latest snapshot on disable" $
    withConfiguredSimulation $ \simHandle dataHandle uiHandle terrainSnapshotRef snapshotVersionRef atlasHandle -> do
      _ <- installCoalescingWorld dataHandle simHandle
      setUiViewMode uiHandle ViewElevation
      setUiDayNightEnabled uiHandle True
      setUiZoom uiHandle 2.5
      setUiSimAutoTick uiHandle True
      setUiSimTickRate uiHandle 1.0
      uiSnap <- getUiSnapshot uiHandle
      let currentStage = zoomStagePair (stageForZoom (uiZoom uiSnap))
          expectedBackfillStages = map zoomStagePair (orderedZoomStagesForZoom (uiZoom uiSnap))
      _ <- drainAtlasJobs atlasHandle

      autoTickStep simHandle Nothing `shouldReturn` AutoTickApplied 1
      version1 <- readSnapshotVersion snapshotVersionRef
      terrainSnap1 <- getTerrainSnapshot dataHandle
      publishedSnap1 <- readTerrainSnapshot terrainSnapshotRef
      tsWeatherVersion publishedSnap1 `shouldBe` tsWeatherVersion terrainSnap1

      autoTickStep simHandle Nothing `shouldReturn` AutoTickApplied 2
      autoTickStep simHandle Nothing `shouldReturn` AutoTickApplied 3
      version3 <- readSnapshotVersion snapshotVersionRef
      terrainSnap3 <- getTerrainSnapshot dataHandle
      publishedSnap3 <- readTerrainSnapshot terrainSnapshotRef
      version3 `shouldBe` version1
      tsWeatherVersion terrainSnap3 `shouldSatisfy` (> tsWeatherVersion terrainSnap1)
      tsWeatherVersion publishedSnap3 `shouldBe` tsWeatherVersion publishedSnap1
      uiAfterRapid <- getUiSnapshot uiHandle
      uiSimTickCount uiAfterRapid `shouldBe` 3
      dagAfterRapid <- getSimDagSnapshot simHandle
      sdsLastTick dagAfterRapid `shouldBe` 3

      rapidJobs <- drainAtlasJobs atlasHandle
      length rapidJobs `shouldBe` 1
      map ajViewMode rapidJobs `shouldBe` [ViewElevation]
      map atlasJobStage rapidJobs `shouldBe` [currentStage]
      map (tsWeatherVersion . ajTerrain) rapidJobs `shouldBe` [tsWeatherVersion publishedSnap1]
      map ajSnapshotVersion rapidJobs `shouldBe` [version1]

      setUiSimAutoTick uiHandle False
      flushed <- flushSimWeatherPublication simHandle
      flushed `shouldBe` True
      versionAfterFlush <- readSnapshotVersion snapshotVersionRef
      versionAfterFlush `shouldSatisfy` (> version3)
      latestTerrainSnap <- getTerrainSnapshot dataHandle
      latestPublishedSnap <- readTerrainSnapshot terrainSnapshotRef
      tsWeatherVersion latestPublishedSnap `shouldBe` tsWeatherVersion latestTerrainSnap
      flushedJobs <- drainAtlasJobs atlasHandle
      length flushedJobs `shouldBe` length allZoomStages
      map ajViewMode flushedJobs `shouldBe` replicate (length allZoomStages) ViewElevation
      map atlasJobStage flushedJobs `shouldBe` expectedBackfillStages
      map (tsWeatherVersion . ajTerrain) flushedJobs `shouldSatisfy` all (== tsWeatherVersion latestTerrainSnap)
      map ajSnapshotVersion flushedJobs `shouldSatisfy` all (== versionAfterFlush)

  it "keeps weather-only auto ticks render-stable for ViewElevation with day/night disabled" $
    withConfiguredSimulation $ \simHandle dataHandle uiHandle terrainSnapshotRef snapshotVersionRef atlasHandle -> do
      initialTerrainSnap <- installCoalescingWorld dataHandle simHandle
      setUiViewMode uiHandle ViewElevation
      setUiDayNightEnabled uiHandle False
      setUiSimAutoTick uiHandle True
      setUiSimTickRate uiHandle 1.0
      uiSnap <- getUiSnapshot uiHandle
      version0 <- readSnapshotVersion snapshotVersionRef
      let waterLevel = uiRenderWaterLevel uiSnap
      _ <- drainAtlasJobs atlasHandle

      requestSimTick simHandle 1
      manualPublished <- awaitTrue 500 $ do
        version <- readSnapshotVersion snapshotVersionRef
        pure (version > version0)
      manualPublished `shouldBe` True
      baselineVersion <- readSnapshotVersion snapshotVersionRef
      baselineTerrainSnap <- getTerrainSnapshot dataHandle
      baselinePublishedSnap <- readTerrainSnapshot terrainSnapshotRef
      tsWeatherVersion baselineTerrainSnap `shouldSatisfy` (> tsWeatherVersion initialTerrainSnap)
      tsWeatherVersion baselinePublishedSnap `shouldBe` tsWeatherVersion baselineTerrainSnap
      let elevationKey0 = atlasKeyFor ViewElevation waterLevel baselinePublishedSnap
          weatherKey0 = atlasKeyFor ViewWeather waterLevel baselinePublishedSnap
      baselineJobs <- drainAtlasJobs atlasHandle
      length baselineJobs `shouldBe` 0

      autoTickStep simHandle Nothing `shouldReturn` AutoTickApplied 2
      versionAfterAuto <- readSnapshotVersion snapshotVersionRef
      versionAfterAuto `shouldSatisfy` (> baselineVersion)
      dataAfterAuto <- getTerrainSnapshot dataHandle
      publishedAfterAuto <- readTerrainSnapshot terrainSnapshotRef
      tsWeatherVersion dataAfterAuto `shouldSatisfy` (> tsWeatherVersion baselineTerrainSnap)
      tsWeatherVersion publishedAfterAuto `shouldBe` tsWeatherVersion baselinePublishedSnap
      atlasKeyFor ViewElevation waterLevel publishedAfterAuto `shouldBe` elevationKey0
      atlasKeyFor ViewWeather waterLevel publishedAfterAuto `shouldBe` weatherKey0
      autoJobs <- drainAtlasJobs atlasHandle
      length autoJobs `shouldBe` 0
      uiAfterAuto <- getUiSnapshot uiHandle
      uiSimTickCount uiAfterAuto `shouldBe` 2
      dagAfterAuto <- getSimDagSnapshot simHandle
      sdsLastTick dagAfterAuto `shouldBe` 2

  it "coalesces visible ViewCloud auto ticks and schedules only published weather atlas jobs" $ withSystem $ \system -> do
    simHandle <- get @Simulation system
    dataHandle <- get @Data system
    logHandle <- get @Log system
    uiHandle <- get @Ui system
    dataSnapshotRef <- newDataSnapshotRef (DataSnapshot 0 0 Nothing)
    terrainSnapshotRef <- newTerrainSnapshotRef emptyTerrainSnapshotForTests
    snapshotVersionRef <- newSnapshotVersionRef
    atlasHandle <- get @AtlasManager system

    setSimHandles simHandle dataHandle logHandle uiHandle dataSnapshotRef terrainSnapshotRef snapshotVersionRef atlasHandle

    let config = WorldConfig { wcChunkSize = 8 }
        chunk = generateTerrainChunk config (const 0.5)
        climate0 = emptyClimateChunk config
        tileCount = U.length (ccTempAvg climate0)
        climate = climate0
          { ccTempAvg = U.replicate tileCount 0.6
          , ccPrecipAvg = U.replicate tileCount 0.5
          , ccWindDirAvg = U.replicate tileCount 0.4
          , ccWindSpdAvg = U.replicate tileCount 0.35
          , ccHumidityAvg = U.replicate tileCount 0.5
          }
        world0 = emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig defaultWorldSlice
        world1 = withSeedWeather
          (setClimateChunk (ChunkId 0) climate (setTerrainChunk (ChunkId 0) chunk world0))
          (ChunkId 0)
          climate

    replaceTerrainData dataHandle world1
    initialTerrainSnap <- getTerrainSnapshot dataHandle
    setSimWorld simHandle world1
    setUiViewMode uiHandle ViewCloud
    uiSnapBeforeTick <- getUiSnapshot uiHandle
    version0 <- readSnapshotVersion snapshotVersionRef
    _ <- drainAtlasJobs atlasHandle
    let waterLevel0 = uiRenderWaterLevel uiSnapBeforeTick
        baseVersion0 = tsVersion initialTerrainSnap
        weatherVersion0 = tsWeatherVersion initialTerrainSnap
        cloudVersion0 = terrainSnapshotViewVersion ViewCloud initialTerrainSnap
        cloudKey0 = atlasKeyFor ViewCloud waterLevel0 initialTerrainSnap
        elevationKey0 = atlasKeyFor ViewElevation waterLevel0 initialTerrainSnap

    autoTickStep simHandle Nothing `shouldReturn` AutoTickApplied 1

    version1 <- readSnapshotVersion snapshotVersionRef
    version1 `shouldSatisfy` (> version0)
    terrainSnap <- getTerrainSnapshot dataHandle
    tsVersion terrainSnap `shouldBe` baseVersion0
    tsWeatherVersion terrainSnap `shouldSatisfy` (> weatherVersion0)
    let cloudVersion1 = terrainSnapshotViewVersion ViewCloud terrainSnap
        cloudKey1 = atlasKeyFor ViewCloud waterLevel0 terrainSnap
    cloudVersion1 `shouldBe` tsWeatherVersion terrainSnap
    cloudVersion1 `shouldNotBe` cloudVersion0
    cloudKey1 `shouldNotBe` cloudKey0
    atlasKeyVersion cloudKey1 `shouldBe` tsWeatherVersion terrainSnap
    atlasKeyFor ViewElevation waterLevel0 terrainSnap `shouldBe` elevationKey0

    publishedSnap <- readTerrainSnapshot terrainSnapshotRef
    tsWeatherVersion publishedSnap `shouldBe` tsWeatherVersion terrainSnap

    atlasJobs <- drainAtlasJobs atlasHandle
    length atlasJobs `shouldBe` length allZoomStages
    map atlasJobStage atlasJobs `shouldBe`
      map zoomStagePair (orderedZoomStagesForZoom (uiZoom uiSnapBeforeTick))
    map ajViewMode atlasJobs `shouldBe` replicate (length allZoomStages) ViewCloud
    map ajKey atlasJobs `shouldSatisfy` all (== cloudKey1)
    map (atlasKeyVersion . ajKey) atlasJobs `shouldSatisfy` all (== tsWeatherVersion terrainSnap)
    map ajSnapshotVersion atlasJobs `shouldSatisfy` all (== version1)

    autoTickStep simHandle Nothing `shouldReturn` AutoTickApplied 2

    version2 <- readSnapshotVersion snapshotVersionRef
    version2 `shouldBe` version1
    terrainSnap2 <- getTerrainSnapshot dataHandle
    tsVersion terrainSnap2 `shouldBe` baseVersion0
    tsWeatherVersion terrainSnap2 `shouldSatisfy` (> tsWeatherVersion terrainSnap)
    let cloudKey2 = atlasKeyFor ViewCloud waterLevel0 terrainSnap2
    cloudKey2 `shouldNotBe` cloudKey1
    atlasKeyVersion cloudKey2 `shouldBe` tsWeatherVersion terrainSnap2

    publishedSnap2 <- readTerrainSnapshot terrainSnapshotRef
    tsWeatherVersion publishedSnap2 `shouldBe` tsWeatherVersion terrainSnap

    atlasJobs2 <- drainAtlasJobs atlasHandle
    length atlasJobs2 `shouldBe` 0

    threadDelay 260000
    autoTickStep simHandle Nothing `shouldReturn` AutoTickApplied 3

    version3 <- readSnapshotVersion snapshotVersionRef
    version3 `shouldSatisfy` (> version2)
    terrainSnap3 <- getTerrainSnapshot dataHandle
    tsVersion terrainSnap3 `shouldBe` baseVersion0
    tsWeatherVersion terrainSnap3 `shouldSatisfy` (> tsWeatherVersion terrainSnap2)
    let cloudKey3 = atlasKeyFor ViewCloud waterLevel0 terrainSnap3
    cloudKey3 `shouldNotBe` cloudKey2
    atlasKeyVersion cloudKey3 `shouldBe` tsWeatherVersion terrainSnap3

    publishedSnap3 <- readTerrainSnapshot terrainSnapshotRef
    tsWeatherVersion publishedSnap3 `shouldBe` tsWeatherVersion terrainSnap3

    atlasJobs3 <- drainAtlasJobs atlasHandle
    length atlasJobs3 `shouldBe` length allZoomStages
    map atlasJobStage atlasJobs3 `shouldBe`
      map zoomStagePair (orderedZoomStagesForZoom (uiZoom uiSnapBeforeTick))
    map ajViewMode atlasJobs3 `shouldBe` replicate (length allZoomStages) ViewCloud
    map ajKey atlasJobs3 `shouldSatisfy` all (== cloudKey3)
    map (atlasKeyVersion . ajKey) atlasJobs3 `shouldSatisfy` all (== tsWeatherVersion terrainSnap3)
    map ajSnapshotVersion atlasJobs3 `shouldSatisfy` all (== version3)

  it "keeps high-rate visible auto ticks queued to the latest current stage" $ withSystem $ \system -> do
    simHandle <- get @Simulation system
    dataHandle <- get @Data system
    logHandle <- get @Log system
    uiHandle <- get @Ui system
    dataSnapshotRef <- newDataSnapshotRef (DataSnapshot 0 0 Nothing)
    terrainSnapshotRef <- newTerrainSnapshotRef emptyTerrainSnapshotForTests
    snapshotVersionRef <- newSnapshotVersionRef
    atlasHandle <- get @AtlasManager system

    setSimHandles simHandle dataHandle logHandle uiHandle dataSnapshotRef terrainSnapshotRef snapshotVersionRef atlasHandle

    let config = WorldConfig { wcChunkSize = 8 }
        chunk = generateTerrainChunk config (const 0.5)
        climate0 = emptyClimateChunk config
        tileCount = U.length (ccTempAvg climate0)
        climate = climate0
          { ccTempAvg = U.replicate tileCount 0.6
          , ccPrecipAvg = U.replicate tileCount 0.5
          , ccWindDirAvg = U.replicate tileCount 0.4
          , ccWindSpdAvg = U.replicate tileCount 0.35
          , ccHumidityAvg = U.replicate tileCount 0.5
          }
        world0 = emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig defaultWorldSlice
        world1 = withSeedWeather
          (setClimateChunk (ChunkId 0) climate (setTerrainChunk (ChunkId 0) chunk world0))
          (ChunkId 0)
          climate

    replaceTerrainData dataHandle world1
    _ <- getTerrainSnapshot dataHandle
    setSimWorld simHandle world1
    setUiViewMode uiHandle ViewCloud
    setUiZoom uiHandle 2.5
    setUiSimAutoTick uiHandle True
    setUiSimTickRate uiHandle 1.0
    uiSnapBeforeTick <- getUiSnapshot uiHandle
    _ <- drainAtlasJobs atlasHandle
    let waterLevel0 = uiRenderWaterLevel uiSnapBeforeTick
        currentStage = stageForZoom (uiZoom uiSnapBeforeTick)
        expectedStage = zoomStagePair currentStage

    autoTickStep simHandle Nothing `shouldReturn` AutoTickApplied 1
    firstSnap <- getTerrainSnapshot dataHandle
    let firstCloudKey = atlasKeyFor ViewCloud waterLevel0 firstSnap

    threadDelay 260000
    autoTickStep simHandle Nothing `shouldReturn` AutoTickApplied 2

    version <- readSnapshotVersion snapshotVersionRef
    terrainSnap <- getTerrainSnapshot dataHandle
    let latestCloudKey = atlasKeyFor ViewCloud waterLevel0 terrainSnap
    latestCloudKey `shouldNotBe` firstCloudKey

    atlasJobs <- drainAtlasJobs atlasHandle
    length atlasJobs `shouldBe` 1
    map ajViewMode atlasJobs `shouldBe` [ViewCloud]
    map ajKey atlasJobs `shouldBe` [latestCloudKey]
    map atlasJobStage atlasJobs `shouldBe` [expectedStage]
    map ajSnapshotVersion atlasJobs `shouldBe` [version]

  it "keeps high-rate non-weather overlay auto ticks on full current-first backfill" $ withSystem $ \system -> do
    simHandle <- get @Simulation system
    dataHandle <- get @Data system
    logHandle <- get @Log system
    uiHandle <- get @Ui system
    dataSnapshotRef <- newDataSnapshotRef (DataSnapshot 0 0 Nothing)
    terrainSnapshotRef <- newTerrainSnapshotRef emptyTerrainSnapshotForTests
    snapshotVersionRef <- newSnapshotVersionRef
    atlasHandle <- get @AtlasManager system

    setSimHandles simHandle dataHandle logHandle uiHandle dataSnapshotRef terrainSnapshotRef snapshotVersionRef atlasHandle

    let config = WorldConfig { wcChunkSize = 8 }
        chunk = generateTerrainChunk config (const 0.5)
        climate0 = emptyClimateChunk config
        tileCount = U.length (ccTempAvg climate0)
        climate = climate0
          { ccTempAvg = U.replicate tileCount 0.6
          , ccPrecipAvg = U.replicate tileCount 0.5
          , ccWindDirAvg = U.replicate tileCount 0.4
          , ccWindSpdAvg = U.replicate tileCount 0.35
          , ccHumidityAvg = U.replicate tileCount 0.5
          }
        world0 = emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig defaultWorldSlice
        world1 = withSeedPluginOverlay tileCount 0 $
          withSeedWeather
            (setClimateChunk (ChunkId 0) climate (setTerrainChunk (ChunkId 0) chunk world0))
            (ChunkId 0)
            climate
        viewMode = ViewOverlay pluginOverlayName 0

    replaceTerrainData dataHandle world1
    _ <- getTerrainSnapshot dataHandle
    setSimWorldWithNodes simHandle world1 [pluginSimulationBinding]
    setUiViewMode uiHandle viewMode
    setUiZoom uiHandle 2.5
    setUiSimAutoTick uiHandle True
    setUiSimTickRate uiHandle 1.0
    uiSnapBeforeTick <- getUiSnapshot uiHandle
    _ <- drainAtlasJobs atlasHandle

    autoTickStep simHandle Nothing `shouldReturn` AutoTickApplied 1

    version <- readSnapshotVersion snapshotVersionRef
    terrainSnap <- getTerrainSnapshot dataHandle
    let overlayKey = atlasKeyFor viewMode (uiRenderWaterLevel uiSnapBeforeTick) terrainSnap
    atlasJobs <- drainAtlasJobs atlasHandle
    length atlasJobs `shouldBe` length allZoomStages
    map ajViewMode atlasJobs `shouldBe` replicate (length allZoomStages) viewMode
    map ajKey atlasJobs `shouldSatisfy` all (== overlayKey)
    map atlasJobStage atlasJobs `shouldBe`
      map zoomStagePair (orderedZoomStagesForZoom (uiZoom uiSnapBeforeTick))
    map ajSnapshotVersion atlasJobs `shouldSatisfy` all (== version)

  it "coalesces hidden auto-tick snapshot publication while manual and visible updates publish immediately" $ withSystem $ \system -> do
    simHandle <- get @Simulation system
    dataHandle <- get @Data system
    logHandle <- get @Log system
    uiHandle <- get @Ui system
    dataSnapshotRef <- newDataSnapshotRef (DataSnapshot 0 0 Nothing)
    terrainSnapshotRef <- newTerrainSnapshotRef emptyTerrainSnapshotForTests
    snapshotVersionRef <- newSnapshotVersionRef
    atlasHandle <- get @AtlasManager system

    setSimHandles simHandle dataHandle logHandle uiHandle dataSnapshotRef terrainSnapshotRef snapshotVersionRef atlasHandle

    let config = WorldConfig { wcChunkSize = 8 }
        world1 = (emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig defaultWorldSlice)
          { twOverlays = insertOverlay (seedWeatherOverlay IntMap.empty) emptyOverlayStore
          }

    replaceTerrainData dataHandle world1
    _ <- getTerrainSnapshot dataHandle
    setSimWorld simHandle world1
    dag0 <- getSimDagSnapshot simHandle
    version0 <- readSnapshotVersion snapshotVersionRef

    autoTickStep simHandle (Just (sdsWorldEpoch dag0)) `shouldReturn` AutoTickApplied 1
    version1 <- readSnapshotVersion snapshotVersionRef
    version1 `shouldSatisfy` (> version0)

    dag1 <- getSimDagSnapshot simHandle
    autoTickStep simHandle (Just (sdsWorldEpoch dag1)) `shouldReturn` AutoTickApplied 2
    version2 <- readSnapshotVersion snapshotVersionRef
    version2 `shouldBe` version1
    uiAfterAuto <- getUiSnapshot uiHandle
    uiSimTickCount uiAfterAuto `shouldBe` 2
    publishedAfterHiddenAuto <- readTerrainSnapshot terrainSnapshotRef
    tsWeatherVersion publishedAfterHiddenAuto `shouldBe` 0
    hiddenAtlasJobs <- drainAtlasJobs atlasHandle
    length hiddenAtlasJobs `shouldBe` 0

    requestSimTick simHandle 3
    manualPublished <- awaitTrue 500 $ do
      version <- readSnapshotVersion snapshotVersionRef
      pure (version > version2)
    manualPublished `shouldBe` True
    version3 <- readSnapshotVersion snapshotVersionRef
    uiAfterManual <- getUiSnapshot uiHandle
    uiSimTickCount uiAfterManual `shouldBe` 3

    setUiViewMode uiHandle (ViewOverlay (Text.pack "weather") 0)
    dag3 <- getSimDagSnapshot simHandle
    autoTickStep simHandle (Just (sdsWorldEpoch dag3)) `shouldReturn` AutoTickApplied 4
    version4 <- readSnapshotVersion snapshotVersionRef
    version4 `shouldSatisfy` (> version3)

  it "does not publish weather-only auto ticks for an unchanged non-weather overlay view" $ withSystem $ \system -> do
    simHandle <- get @Simulation system
    dataHandle <- get @Data system
    logHandle <- get @Log system
    uiHandle <- get @Ui system
    dataSnapshotRef <- newDataSnapshotRef (DataSnapshot 0 0 Nothing)
    terrainSnapshotRef <- newTerrainSnapshotRef emptyTerrainSnapshotForTests
    snapshotVersionRef <- newSnapshotVersionRef
    atlasHandle <- get @AtlasManager system

    setSimHandles simHandle dataHandle logHandle uiHandle dataSnapshotRef terrainSnapshotRef snapshotVersionRef atlasHandle

    let config = WorldConfig { wcChunkSize = 8 }
        chunk = generateTerrainChunk config (const 0.5)
        climate0 = emptyClimateChunk config
        tileCount = U.length (ccTempAvg climate0)
        climate = climate0
          { ccTempAvg = U.replicate tileCount 0.6
          , ccPrecipAvg = U.replicate tileCount 0.5
          , ccWindDirAvg = U.replicate tileCount 0.4
          , ccWindSpdAvg = U.replicate tileCount 0.35
          , ccHumidityAvg = U.replicate tileCount 0.5
          }
        world0 = emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig defaultWorldSlice
        world1 = withSeedPluginOverlay tileCount 0 $
          withSeedWeather
            (setClimateChunk (ChunkId 0) climate (setTerrainChunk (ChunkId 0) chunk world0))
            (ChunkId 0)
            climate

    replaceTerrainData dataHandle world1
    initialTerrainSnap <- getTerrainSnapshot dataHandle
    setSimWorld simHandle world1
    setUiViewMode uiHandle (ViewOverlay pluginOverlayName 0)
    version0 <- readSnapshotVersion snapshotVersionRef
    _ <- drainAtlasJobs atlasHandle

    autoTickStep simHandle Nothing `shouldReturn` AutoTickApplied 1

    version1 <- readSnapshotVersion snapshotVersionRef
    version1 `shouldSatisfy` (> version0)
    terrainSnap <- getTerrainSnapshot dataHandle
    tsWeatherVersion terrainSnap `shouldSatisfy` (> tsWeatherVersion initialTerrainSnap)
    publishedSnap <- readTerrainSnapshot terrainSnapshotRef
    tsWeatherVersion publishedSnap `shouldBe` 0
    atlasJobs <- drainAtlasJobs atlasHandle
    length atlasJobs `shouldBe` 0
    uiAfterAuto <- getUiSnapshot uiHandle
    uiSimTickCount uiAfterAuto `shouldBe` 1

  it "publishes manual time-only ticks when every node is skipped" $ withSystem $ \system -> do
    simHandle <- get @Simulation system
    dataHandle <- get @Data system
    logHandle <- get @Log system
    uiHandle <- get @Ui system
    dataSnapshotRef <- newDataSnapshotRef (DataSnapshot 0 0 Nothing)
    terrainSnapshotRef <- newTerrainSnapshotRef emptyTerrainSnapshotForTests
    snapshotVersionRef <- newSnapshotVersionRef
    atlasHandle <- get @AtlasManager system

    setSimHandles simHandle dataHandle logHandle uiHandle dataSnapshotRef terrainSnapshotRef snapshotVersionRef atlasHandle

    let config = WorldConfig { wcChunkSize = 8 }
        chunk = generateTerrainChunk config (const 0.5)
        climate0 = emptyClimateChunk config
        tileCount = U.length (ccTempAvg climate0)
        climate = climate0
          { ccTempAvg = U.replicate tileCount 0.55
          , ccPrecipAvg = U.replicate tileCount 0.45
          , ccWindDirAvg = U.replicate tileCount 0.3
          , ccWindSpdAvg = U.replicate tileCount 0.25
          , ccHumidityAvg = U.replicate tileCount 0.4
          }
        delayedSchedule = SimulationScheduleState
          { schedIntervalTicks = 10
          , schedPhaseTicks = 0
          , schedLastFireTick = Nothing
          , schedNextFireTick = 10
          , schedCatchUpPolicy = RunOnceIfDue
          }
        genConfig = defaultWorldGenConfig
          { worldWeather = defaultWeatherConfig { wcTickSeconds = 10 }
          }
        world0 = emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig defaultWorldSlice
        world1 = (withSeedWeatherSchedule delayedSchedule
          (setClimateChunk (ChunkId 0) climate (setTerrainChunk (ChunkId 0) chunk world0))
          (ChunkId 0)
          climate)
          { twGenConfig = Just (toJSON genConfig)
          }

    replaceTerrainData dataHandle world1
    terrainSnap0 <- getTerrainSnapshot dataHandle
    version0 <- readSnapshotVersion snapshotVersionRef
    setSimWorld simHandle world1
    requestSimTick simHandle 1

    tickAdvanced <- awaitTrue 500 $ do
      uiSnap <- getUiSnapshot uiHandle
      pure (uiSimTickCount uiSnap >= 1)
    tickAdvanced `shouldBe` True

    version1 <- readSnapshotVersion snapshotVersionRef
    version1 `shouldSatisfy` (> version0)
    terrainSnap1 <- getTerrainSnapshot dataHandle
    tsWeatherVersion terrainSnap1 `shouldBe` tsWeatherVersion terrainSnap0
    dagSnapshot <- getSimDagSnapshot simHandle
    sdsLastTick dagSnapshot `shouldBe` 1
    let weatherNodes = filter ((== Text.pack "weather") . sdnsNodeId) (sdsNodes dagSnapshot)
    case weatherNodes of
      [node] -> do
        sdnsStatus node `shouldBe` Text.pack "skipped"
        sdnsScheduleIntervalTicks node `shouldBe` Just 10
        sdnsSchedulePhaseTicks node `shouldBe` Just 0
        sdnsScheduleLastFireTick node `shouldBe` Nothing
        sdnsScheduleNextFireTick node `shouldBe` Just 10
        sdnsScheduleDue node `shouldBe` Just False
      _ -> expectationFailure "Expected one skipped weather node in simulation DAG"
    map stleStatus (sdsTickLogs dagSnapshot) `shouldSatisfy` elem (Text.pack "skipped")

  it "keeps control calls responsive and folds only the latest queued tick while a worker runs" $ withSystem $ \system -> do
    simHandle <- get @Simulation system
    dataHandle <- get @Data system
    logHandle <- get @Log system
    uiHandle <- get @Ui system
    dataSnapshotRef <- newDataSnapshotRef (DataSnapshot 0 0 Nothing)
    terrainSnapshotRef <- newTerrainSnapshotRef emptyTerrainSnapshotForTests
    snapshotVersionRef <- newSnapshotVersionRef
    atlasHandle <- get @AtlasManager system

    setSimHandles simHandle dataHandle logHandle uiHandle dataSnapshotRef terrainSnapshotRef snapshotVersionRef atlasHandle

    started <- newEmptyMVar
    runCountRef <- newIORef (0 :: Int)
    let config = WorldConfig { wcChunkSize = 8 }
        chunk = generateTerrainChunk config (const 0.5)
        climate0 = emptyClimateChunk config
        tileCount = U.length (ccTempAvg climate0)
        climate = climate0
          { ccTempAvg = U.replicate tileCount 0.6
          , ccPrecipAvg = U.replicate tileCount 0.5
          , ccWindDirAvg = U.replicate tileCount 0.4
          , ccWindSpdAvg = U.replicate tileCount 0.35
          , ccHumidityAvg = U.replicate tileCount 0.5
          }
        world0 = emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig defaultWorldSlice
        world1 = withSeedPluginOverlay tileCount 0 $
          withSeedWeather
            (setClimateChunk (ChunkId 0) climate (setTerrainChunk (ChunkId 0) chunk world0))
            (ChunkId 0)
            climate
        slowBinding = SimulationNodeBinding
          { snbNode = SimNodeReader
              { snrId = SimNodeId (Text.pack "slow-plugin")
              , snrOverlayName = pluginOverlayName
              , snrDependencies = []
              , snrSchedule = hourlyScheduleDecl
              , snrReadTick = \_ overlay -> do
                  modifyIORef' runCountRef (+ 1)
                  _ <- tryPutMVar started ()
                  threadDelay 300000
                  pure (Right overlay)
              }
          , snbKind = Text.pack "plugin"
          , snbPlugin = Just pluginOverlayName
          }

    replaceTerrainData dataHandle world1
    _ <- getTerrainSnapshot dataHandle
    setSimWorldWithNodes simHandle world1 [slowBinding]
    requestSimTick simHandle 1

    workerStarted <- timeout 1000000 (readMVar started)
    workerStarted `shouldBe` Just ()

    responsive <- timeout 100000 (getSimDagSnapshot simHandle)
    responsive `shouldSatisfy` maybe False sdsAvailable

    requestSimTick simHandle 2
    requestSimTick simHandle 5

    tickAdvanced <- awaitTrue 500 $ do
      uiSnap <- getUiSnapshot uiHandle
      pure (uiSimTickCount uiSnap >= 5)
    tickAdvanced `shouldBe` True

    runCount <- readIORef runCountRef
    runCount `shouldBe` 5

  it "defaults schedules during bind and preserves cursors across rebind" $ withSystem $ \system -> do
    simHandle <- get @Simulation system

    let config = WorldConfig { wcChunkSize = 8 }
        tileCount = wcChunkSize config * wcChunkSize config
        baseWorld = emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig defaultWorldSlice
        world0 = withSeedPluginOverlay tileCount 0 $
          baseWorld
            { twOverlays = insertOverlay (seedWeatherOverlay IntMap.empty) emptyOverlayStore
            }

    setSimWorldWithNodes simHandle world0 [pluginSimulationBinding]
    dag0 <- getSimDagSnapshot simHandle
    let weatherNodes0 = filter ((== Text.pack "weather") . sdnsNodeId) (sdsNodes dag0)
        pluginNodes0 = filter ((== Just pluginOverlayName) . sdnsPlugin) (sdsNodes dag0)
    case weatherNodes0 of
      [node] -> do
        sdnsScheduleLastFireTick node `shouldBe` Nothing
        sdnsScheduleNextFireTick node `shouldBe` Just 1
      _ -> expectationFailure "Expected one default-scheduled weather node"
    case pluginNodes0 of
      [node] -> do
        sdnsScheduleLastFireTick node `shouldBe` Nothing
        sdnsScheduleNextFireTick node `shouldBe` Just 1
      _ -> expectationFailure "Expected one default-scheduled plugin node"

    let cadenceWeather = defaultWeatherConfig { wcTickSeconds = 6 }
        cadenceGenConfig = defaultWorldGenConfig { worldWeather = cadenceWeather }
        withCadenceConfig world = world { twGenConfig = Just (toJSON cadenceGenConfig) }
        missingScheduleWorld = withCadenceConfig (baseWorld
          { twOverlays = insertOverlay (seedWeatherOverlay IntMap.empty) emptyOverlayStore
          })

    setSimWorld simHandle missingScheduleWorld
    dagCadence <- getSimDagSnapshot simHandle
    let cadenceWeatherNodes = filter ((== Text.pack "weather") . sdnsNodeId) (sdsNodes dagCadence)
    case cadenceWeatherNodes of
      [node] -> do
        sdnsScheduleIntervalTicks node `shouldBe` Just 6
        sdnsSchedulePhaseTicks node `shouldBe` Just 0
        sdnsScheduleCatchUp node `shouldBe` Just (Text.pack "run_once_if_due")
        sdnsScheduleNextFireTick node `shouldBe` Just 6
      _ -> expectationFailure "Expected one cadence-scheduled weather node"

    let pluginSchedule = SimulationScheduleState
          { schedIntervalTicks = 6
          , schedPhaseTicks = 2
          , schedLastFireTick = Just 14
          , schedNextFireTick = 20
          , schedCatchUpPolicy = SkipMissed
          }
        legacyWeatherSchedule = SimulationScheduleState
          { schedIntervalTicks = 1
          , schedPhaseTicks = 0
          , schedLastFireTick = Just 4
          , schedNextFireTick = 5
          , schedCatchUpPolicy = RunOnceIfDue
          }
        pluginOverlay = (seedPluginOverlay tileCount 0)
          { ovProvenance = (ovProvenance (seedPluginOverlay tileCount 0))
              { opSchedule = Just pluginSchedule
              }
          }
        weatherOverlay = seedWeatherOverlayWithSchedule (Just legacyWeatherSchedule) IntMap.empty
        worldWithPersistedSchedule = withCadenceConfig (baseWorld
          { twOverlays = insertOverlay pluginOverlay
              (insertOverlay weatherOverlay emptyOverlayStore)
          })

    setSimWorldWithNodes simHandle worldWithPersistedSchedule [pluginSimulationBinding]
    rebindSimNodes simHandle [pluginSimulationBinding] `shouldReturn` True
    dag1 <- getSimDagSnapshot simHandle
    let weatherNodes1 = filter ((== Text.pack "weather") . sdnsNodeId) (sdsNodes dag1)
        pluginNodes1 = filter ((== Just pluginOverlayName) . sdnsPlugin) (sdsNodes dag1)
    case weatherNodes1 of
      [node] -> do
        sdnsScheduleIntervalTicks node `shouldBe` Just 6
        sdnsSchedulePhaseTicks node `shouldBe` Just 0
        sdnsScheduleLastFireTick node `shouldBe` Nothing
        sdnsScheduleNextFireTick node `shouldBe` Just 6
      _ -> expectationFailure "Expected one rebased weather node"
    case pluginNodes1 of
      [node] -> do
        sdnsScheduleIntervalTicks node `shouldBe` Just 6
        sdnsSchedulePhaseTicks node `shouldBe` Just 2
        sdnsScheduleLastFireTick node `shouldBe` Just 14
        sdnsScheduleNextFireTick node `shouldBe` Just 20
      _ -> expectationFailure "Expected one plugin node with preserved schedule"

    let matchingWeatherSchedule = SimulationScheduleState
          { schedIntervalTicks = 6
          , schedPhaseTicks = 0
          , schedLastFireTick = Just 12
          , schedNextFireTick = 18
          , schedCatchUpPolicy = RunOnceIfDue
          }
        matchingWeatherWorld = withCadenceConfig (baseWorld
          { twOverlays = insertOverlay
              (seedWeatherOverlayWithSchedule (Just matchingWeatherSchedule) IntMap.empty)
              emptyOverlayStore
          })
    setSimWorld simHandle matchingWeatherWorld
    dag2 <- getSimDagSnapshot simHandle
    let weatherNodes2 = filter ((== Text.pack "weather") . sdnsNodeId) (sdsNodes dag2)
    case weatherNodes2 of
      [node] -> do
        sdnsScheduleIntervalTicks node `shouldBe` Just 6
        sdnsScheduleLastFireTick node `shouldBe` Just 12
        sdnsScheduleNextFireTick node `shouldBe` Just 18
      _ -> expectationFailure "Expected one preserved weather node"

  it "uses world generation weather config for builtin weather and falls back for legacy configs" $ withSystem $ \system -> do
    simHandle <- get @Simulation system
    dataHandle <- get @Data system
    logHandle <- get @Log system
    uiHandle <- get @Ui system
    dataSnapshotRef <- newDataSnapshotRef (DataSnapshot 0 0 Nothing)
    terrainSnapshotRef <- newTerrainSnapshotRef emptyTerrainSnapshotForTests
    snapshotVersionRef <- newSnapshotVersionRef
    atlasHandle <- get @AtlasManager system

    setSimHandles simHandle dataHandle logHandle uiHandle dataSnapshotRef terrainSnapshotRef snapshotVersionRef atlasHandle

    let config = WorldConfig { wcChunkSize = 8 }
        chunk = generateTerrainChunk config (const 0.5)
        climate0 = emptyClimateChunk config
        tileCount = U.length (ccTempAvg climate0)
        climate = climate0
          { ccTempAvg = U.replicate tileCount 0.90
          , ccPrecipAvg = U.replicate tileCount 0.25
          , ccWindDirAvg = U.replicate tileCount 0.0
          , ccWindSpdAvg = U.replicate tileCount 0.0
          , ccHumidityAvg = U.replicate tileCount 0.50
          }
        seedWeather = (mkSeedWeatherChunk climate)
          { wcTemp = U.replicate tileCount 0.05
          , wcHumidity = U.replicate tileCount 0.20
          , wcWindDir = U.replicate tileCount 0.0
          , wcWindSpd = U.replicate tileCount 0.0
          , wcCloudCover = U.replicate tileCount 0.0
          , wcCloudWater = U.replicate tileCount 0.0
          , wcCloudCoverLow = U.replicate tileCount 0.0
          , wcCloudCoverMid = U.replicate tileCount 0.0
          , wcCloudCoverHigh = U.replicate tileCount 0.0
          , wcCloudWaterLow = U.replicate tileCount 0.0
          , wcCloudWaterMid = U.replicate tileCount 0.0
          , wcCloudWaterHigh = U.replicate tileCount 0.0
          }
        customWeather = defaultWeatherConfig
          { wcSeasonAmplitude = 0
          , wcJitterAmplitude = 0
          , wcITCZPrecipBoost = 0
          , wcPressureGradientWindScale = 0
          , wcCloudAlbedoEffect = 0
          , wcCloudPrecipBoost = 0
          , wcCloudFormationRate = 0
          , wcCloudDissipationRate = 0
          , wcCloudGreenhouseCoeff = 0
          , wcAdvectDt = 0
          , wcClimatePullStrength = 1
          , wcWeatherDiffuseFactor = 0
          }
        genConfig = defaultWorldGenConfig { worldWeather = customWeather }
        world0 = emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig defaultWorldSlice
        baseWorld = (setClimateChunk (ChunkId 0) climate (setTerrainChunk (ChunkId 0) chunk world0))
          { twOverlays = insertOverlay (seedWeatherOverlay (IntMap.singleton 0 seedWeather)) emptyOverlayStore
          }
        worldWithGenConfig cfg = baseWorld { twGenConfig = cfg }
        runBoundWorldTick world = do
          replaceTerrainData dataHandle world
          _ <- getTerrainSnapshot dataHandle
          setSimWorld simHandle world
          autoTickStep simHandle Nothing `shouldReturn` AutoTickApplied 1
          getTerrainSnapshot dataHandle

    fallbackSnap <- runBoundWorldTick (worldWithGenConfig Nothing)
    invalidSnap <- runBoundWorldTick (worldWithGenConfig (Just (String (Text.pack "not-world-gen-config"))))
    customSnap <- runBoundWorldTick (worldWithGenConfig (Just (toJSON genConfig)))

    tsWeatherChunks invalidSnap `shouldBe` tsWeatherChunks fallbackSnap
    tsWeatherChunks customSnap `shouldNotBe` tsWeatherChunks fallbackSnap
    fallbackTemp <- case firstWeatherTemp (tsWeatherChunks fallbackSnap) of
      Just t -> pure t
      Nothing -> expectationFailure "Expected fallback weather chunks after tick" >> pure 0
    customTemp <- case firstWeatherTemp (tsWeatherChunks customSnap) of
      Just t -> pure t
      Nothing -> expectationFailure "Expected custom weather chunks after tick" >> pure fallbackTemp
    customTemp `shouldSatisfy` (> fallbackTemp + 0.25)

  it "executes plugin simulation nodes with builtin weather on manual ticks" $ withSystem $ \system -> do
    simHandle <- get @Simulation system
    dataHandle <- get @Data system
    logHandle <- get @Log system
    uiHandle <- get @Ui system
    dataSnapshotRef <- newDataSnapshotRef (DataSnapshot 0 0 Nothing)
    terrainSnapshotRef <- newTerrainSnapshotRef emptyTerrainSnapshotForTests
    snapshotVersionRef <- newSnapshotVersionRef
    atlasHandle <- get @AtlasManager system

    setSimHandles simHandle dataHandle logHandle uiHandle dataSnapshotRef terrainSnapshotRef snapshotVersionRef atlasHandle

    let config = WorldConfig { wcChunkSize = 8 }
        chunk = generateTerrainChunk config (const 0.5)
        climate0 = emptyClimateChunk config
        tileCount = U.length (ccTempAvg climate0)
        climate = climate0
          { ccTempAvg = U.replicate tileCount 0.6
          , ccPrecipAvg = U.replicate tileCount 0.5
          , ccWindDirAvg = U.replicate tileCount 0.4
          , ccWindSpdAvg = U.replicate tileCount 0.35
          , ccHumidityAvg = U.replicate tileCount 0.5
          }
        world0 = emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig defaultWorldSlice
        world1 = withSeedPluginOverlay tileCount 0 $
          withSeedWeather
            (setClimateChunk (ChunkId 0) climate (setTerrainChunk (ChunkId 0) chunk world0))
            (ChunkId 0)
            climate
        tempBefore = firstWeatherTemp (IntMap.singleton 0 (mkSeedWeatherChunk climate))

    replaceTerrainData dataHandle world1
    initialTerrainSnap <- getTerrainSnapshot dataHandle
    let baseVersion0 = tsVersion initialTerrainSnap
        overlayVersion0 = tsOverlayVersion initialTerrainSnap
    setSimWorldWithNodes simHandle world1 [pluginSimulationBinding]
    requestSimTick simHandle 1

    tickAdvanced <- awaitTrue 500 $ do
      uiSnap <- getUiSnapshot uiHandle
      pure (uiSimTickCount uiSnap >= 1)
    tickAdvanced `shouldBe` True

    terrainSnap <- getTerrainSnapshot dataHandle
    tsVersion terrainSnap `shouldBe` baseVersion0
    tsOverlayVersion terrainSnap `shouldSatisfy` (> overlayVersion0)
    tempAfter <- case firstWeatherTemp (tsWeatherChunks terrainSnap) of
      Just t -> pure t
      Nothing -> expectationFailure "Expected weather chunks after manual plugin tick" >> pure 0
    Just tempAfter `shouldNotBe` tempBefore

    pluginValue <- case lookupOverlay pluginOverlayName (tsOverlayStore terrainSnap) >>= firstPluginOverlayValue of
      Just value -> pure value
      Nothing -> expectationFailure "Expected plugin overlay after manual tick" >> pure 0
    pluginValue `shouldBe` 1

    dagSnapshot <- getSimDagSnapshot simHandle
    sdsLevels dagSnapshot `shouldBe` [[Text.pack "weather"], [pluginOverlayName]]
    let weatherNodes = filter ((== Text.pack "weather") . sdnsNodeId) (sdsNodes dagSnapshot)
    case weatherNodes of
      [node] -> do
        sdnsKind node `shouldBe` Text.pack "builtin"
        sdnsPlugin node `shouldBe` Nothing
        sdnsScheduleIntervalTicks node `shouldBe` Just 1
        sdnsSchedulePhaseTicks node `shouldBe` Just 0
        sdnsScheduleCatchUp node `shouldBe` Just (Text.pack "run_once_if_due")
        sdnsScheduleLastFireTick node `shouldBe` Just 1
        sdnsScheduleNextFireTick node `shouldBe` Just 2
        sdnsScheduleDue node `shouldBe` Just False
      _ -> expectationFailure "Expected one weather node in simulation DAG"
    let pluginNodes = filter ((== Just pluginOverlayName) . sdnsPlugin) (sdsNodes dagSnapshot)
    case pluginNodes of
      [node] -> do
        sdnsKind node `shouldBe` Text.pack "plugin"
        sdnsPlugin node `shouldBe` Just pluginOverlayName
        sdnsDependencies node `shouldBe` [Text.pack "weather"]
        sdnsStatus node `shouldBe` Text.pack "completed"
        sdnsScheduleIntervalTicks node `shouldBe` Just 1
        sdnsSchedulePhaseTicks node `shouldBe` Just 0
        sdnsScheduleCatchUp node `shouldBe` Just (Text.pack "run_once_if_due")
        sdnsScheduleLastFireTick node `shouldBe` Just 1
        sdnsScheduleNextFireTick node `shouldBe` Just 2
        sdnsScheduleDue node `shouldBe` Just False
      _ -> expectationFailure "Expected one executable plugin node in simulation DAG"
