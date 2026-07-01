{-# LANGUAGE OverloadedStrings #-}

module Spec.AutoTick (spec) where

import Control.Concurrent (threadDelay)
import qualified Data.IntMap.Strict as IntMap
import Data.Aeson (Value(..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector.Unboxed as U
import Test.Hspec

import Actor.Data (TerrainSnapshot(..), getTerrainSnapshot)
import Actor.Simulation
  ( SimulationDagSnapshot(..)
  , SimulationTickLogEntry(..)
  , getSimDagSnapshot
  , setSimWorld
  )
import Actor.SnapshotReceiver (readSnapshotVersion)
import Actor.UI (UiState(..), getUiSnapshot)
import Actor.UiActions (ActorHandles(..))
import Seer.Command.Dispatch (CommandContext(..), dispatchCommand)
import Seer.Headless
  ( HeadlessApp
  , defaultHeadlessConfig
  , headlessCommandContext
  , withHeadlessApp
  )
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
import Topo.Overlay (Overlay(..), OverlayData(..), OverlayProvenance(..), insertOverlay)
import Topo.Planet (defaultPlanetConfig, defaultWorldSlice)
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
      version0 <- readSnapshotVersion (ahSnapshotVersionRef handles)

      rsp <- dispatch app "set_sim_auto_tick" (object ["enabled" .= True, "rate" .= (1.0 :: Double)])
      srSuccess rsp `shouldBe` True

      advanced <- awaitTrue 100 $ do
        ui <- getUiSnapshot (ahUiHandle handles)
        pure (uiSimTickCount ui >= 1)
      advanced `shouldBe` True

      terrainSnap <- getTerrainSnapshot (ahDataHandle handles)
      tsVersion terrainSnap `shouldSatisfy` (> 0)
      version1 <- readSnapshotVersion (ahSnapshotVersionRef handles)
      version1 `shouldSatisfy` (> version0)
      dag <- getSimDagSnapshot (ahSimulationHandle handles)
      sdsPendingTick dag `shouldBe` Nothing
      map stleStatus (sdsTickLogs dag) `shouldSatisfy` elem (Text.pack "completed")

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
  setSimWorld (ahSimulationHandle handles) testWorld
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
    climate = climate0
      { ccTempAvg = U.replicate tileCount 0.55
      , ccPrecipAvg = U.replicate tileCount 0.45
      , ccWindDirAvg = U.replicate tileCount 0.3
      , ccWindSpdAvg = U.replicate tileCount 0.25
      , ccHumidityAvg = U.replicate tileCount 0.4
      }
    world0 = emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig defaultWorldSlice

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
