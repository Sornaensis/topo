{-# LANGUAGE DataKinds #-}

module Spec.Simulation (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import qualified Data.IntMap.Strict as IntMap
import Data.Text (Text)
import qualified Data.Text as Text
import Hyperspace.Actor (ActorSystem, getSingleton, newActorSystem, shutdownActorSystem)
import Test.Hspec
import qualified Data.Vector.Unboxed as U

import Actor.AtlasManager (atlasManagerActorDef)
import Actor.Data (DataSnapshot(..), TerrainSnapshot(..), dataActorDef, getTerrainSnapshot, tsVersion, tsWeatherChunks)
import Actor.Log (getLogSnapshot, leMessage, logActorDef, lsEntries)
import Actor.Simulation (requestSimTick, setSimHandles, setSimWorld, simulationActorDef)
import Actor.SnapshotReceiver
  ( newDataSnapshotRef
  , newTerrainSnapshotRef
  , newSnapshotVersionRef
  , readTerrainSnapshot
  )
import Actor.UI (getUiSnapshot, uiActorDef, uiSimTickCount)

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
import Topo.Overlay (Overlay(..), OverlayData(..), OverlayProvenance(..), emptyOverlayStore, insertOverlay)
import Topo.Planet (defaultPlanetConfig, defaultWorldSlice)
import Topo.Weather (weatherChunkToOverlay, weatherOverlaySchema)
import Topo.World (TerrainWorld(..))

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

seedWeatherOverlay :: IntMap.IntMap WeatherChunk -> Overlay
seedWeatherOverlay weatherChunks = Overlay
  { ovSchema = weatherOverlaySchema
  , ovData = DenseData (IntMap.map weatherChunkToOverlay weatherChunks)
  , ovProvenance = OverlayProvenance
      { opSeed = 0
      , opVersion = 1
        , opSource = Text.pack "simulation-spec"
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

spec :: Spec
spec = describe "Simulation actor" $ do
  it "binds world before processing first post-load tick request" $ withSystem $ \system -> do
    simHandle <- getSingleton system simulationActorDef
    dataHandle <- getSingleton system dataActorDef
    logHandle <- getSingleton system logActorDef
    uiHandle <- getSingleton system uiActorDef
    dataSnapshotRef <- newDataSnapshotRef (DataSnapshot 0 0 Nothing)
    terrainSnapshotRef <- newTerrainSnapshotRef (TerrainSnapshot 0 0 mempty mempty mempty mempty mempty emptyOverlayStore)
    snapshotVersionRef <- newSnapshotVersionRef
    atlasHandle <- getSingleton system atlasManagerActorDef

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

    setSimWorld simHandle world1
    requestSimTick simHandle 1

    tickAdvanced <- awaitTrue 500 $ do
      uiSnap <- getUiSnapshot uiHandle
      pure (uiSimTickCount uiSnap >= 1)
    tickAdvanced `shouldBe` True

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
    simHandle <- getSingleton system simulationActorDef
    dataHandle <- getSingleton system dataActorDef
    logHandle <- getSingleton system logActorDef
    uiHandle <- getSingleton system uiActorDef
    dataSnapshotRef <- newDataSnapshotRef (DataSnapshot 0 0 Nothing)
    terrainSnapshotRef <- newTerrainSnapshotRef (TerrainSnapshot 0 0 mempty mempty mempty mempty mempty emptyOverlayStore)
    snapshotVersionRef <- newSnapshotVersionRef
    atlasHandle <- getSingleton system atlasManagerActorDef

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

    setSimWorld simHandle world1

    tickAdvanced <- awaitTrue 500 $ do
      uiSnap <- getUiSnapshot uiHandle
      pure (uiSimTickCount uiSnap >= 1)
    tickAdvanced `shouldBe` True

    logSnap <- getLogSnapshot logHandle
    let messages = map leMessage (lsEntries logSnap)
    containsText (Text.pack "simulation: tick deferred (not ready)") messages `shouldBe` True
    containsText (Text.pack "simulation: tick 1 completed") messages `shouldBe` True

  it "processes tick requests and publishes updated state" $ withSystem $ \system -> do
    simHandle <- getSingleton system simulationActorDef
    dataHandle <- getSingleton system dataActorDef
    logHandle <- getSingleton system logActorDef
    uiHandle <- getSingleton system uiActorDef
    dataSnapshotRef <- newDataSnapshotRef (DataSnapshot 0 0 Nothing)
    terrainSnapshotRef <- newTerrainSnapshotRef (TerrainSnapshot 0 0 mempty mempty mempty mempty mempty emptyOverlayStore)
    snapshotVersionRef <- newSnapshotVersionRef
    atlasHandle <- getSingleton system atlasManagerActorDef

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

    setSimWorld simHandle world1
    requestSimTick simHandle 1

    tickAdvanced <- awaitTrue 500 $ do
      uiSnap <- getUiSnapshot uiHandle
      pure (uiSimTickCount uiSnap >= 1)
    tickAdvanced `shouldBe` True

    terrainSnap <- getTerrainSnapshot dataHandle
    tsVersion terrainSnap `shouldSatisfy` (> 0)
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
      pure (tsVersion terrainRef > 0)
    snapshotPublished `shouldBe` True

    logSnap <- getLogSnapshot logHandle
    let messages = map leMessage (lsEntries logSnap)
    containsText (Text.pack "simulation: tick") messages `shouldBe` True
