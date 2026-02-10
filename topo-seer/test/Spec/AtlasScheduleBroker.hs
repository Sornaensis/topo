module Spec.AtlasScheduleBroker (spec) where

import Control.Exception (bracket)
import Test.Hspec
import Actor.AtlasScheduleBroker
  ( AtlasScheduleReport(..)
  , atlasScheduleBrokerActorDef
  , getAtlasScheduleReport
  , updateAtlasScheduleReport
  )
import Actor.SnapshotReceiver (SnapshotVersion(..))
import Hyperspace.Actor (ActorSystem, getSingleton, newActorSystem, shutdownActorSystem)

withSystem :: (ActorSystem -> IO a) -> IO a
withSystem = bracket newActorSystem shutdownActorSystem

spec :: Spec
spec = describe "AtlasScheduleBroker" $ do
  it "returns the latest report" $ withSystem $ \system -> do
    brokerHandle <- getSingleton system atlasScheduleBrokerActorDef
    let report1 = AtlasScheduleReport
          { asrSnapshotVersion = SnapshotVersion 1
          , asrJobCount = 1
          , asrDrainMs = 3
          , asrEnqueueMs = 5
          }
        report2 = AtlasScheduleReport
          { asrSnapshotVersion = SnapshotVersion 2
          , asrJobCount = 4
          , asrDrainMs = 7
          , asrEnqueueMs = 11
          }
    updateAtlasScheduleReport brokerHandle report1
    updateAtlasScheduleReport brokerHandle report2
    mbReport <- getAtlasScheduleReport brokerHandle
    mbReport `shouldBe` Just report2
