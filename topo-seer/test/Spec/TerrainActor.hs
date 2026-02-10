{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Spec.TerrainActor (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Hyperspace.Actor (ActorSystem, call, getSingleton, newActorSystem, replyTo, shutdownActorSystem)
import Hyperspace.Actor.QQ (hyperspace)
import Test.Hspec
import Actor.Log (LogEntry)
import Actor.Terrain
  ( TerrainGenProgress(..)
  , TerrainGenRequest(..)
  , TerrainGenResult(..)
  , TerrainReplyOps
  , terrainActorDef
  , startTerrainGen
  )
import Topo (WorldConfig(..))
import Topo.WorldGen (defaultWorldGenConfig)

newtype TerrainReplyState = TerrainReplyState
  { trsResult :: Maybe TerrainGenResult
  } deriving (Eq, Show)

emptyTerrainReplyState :: TerrainReplyState
emptyTerrainReplyState = TerrainReplyState
  { trsResult = Nothing
  }

[hyperspace|
actor TerrainReplyTest
  state TerrainReplyState
  lifetime Singleton
  noDeps

  reply TerrainReplyOps

  cast progress :: TerrainGenProgress
  cast result :: TerrainGenResult
  cast logMessage :: LogEntry
  call snapshot :: () -> TerrainReplyState

  initial emptyTerrainReplyState
  on_ progress = \_ st -> pure st
  on_ result = \resultMsg st -> pure st { trsResult = Just resultMsg }
  on_ logMessage = \_ st -> pure st
  onPure snapshot = \() st -> (st, st)
|]

withSystem :: (ActorSystem -> IO a) -> IO a
withSystem = bracket newActorSystem shutdownActorSystem

await :: Int -> IO (Maybe a) -> IO (Maybe a)
await 0 action = action
await n action = do
  result <- action
  case result of
    Just _ -> pure result
    Nothing -> threadDelay 20000 >> await (n - 1) action

spec :: Spec
spec = describe "TerrainActor" $ do
  it "runs generation and replies with a result" $ withSystem $ \system -> do
    terrainHandle <- getSingleton system terrainActorDef
    replyHandle <- getSingleton system terrainReplyTestActorDef
    let req = TerrainGenRequest
          { tgrSeed = 123
          , tgrWorldConfig = WorldConfig { wcChunkSize = 8 }
          , tgrGenConfig = defaultWorldGenConfig
          }
    startTerrainGen terrainHandle (replyTo @TerrainReplyOps replyHandle) req
    result <- await 50 (trsResult <$> call @"snapshot" replyHandle #snapshot ())
    case result of
      Nothing -> expectationFailure "Expected a terrain generation reply"
      Just replyMsg ->
        tgrResultSeed replyMsg `shouldBe` 123
