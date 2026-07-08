module Spec.AtlasScheduleBroker (spec) where

import Test.Hspec
import Actor.AtlasScheduleBroker
  ( AtlasScheduleReport(..)
  , emptyAtlasScheduleReport
  , newAtlasScheduleRef
  , readAtlasScheduleRef
  , writeAtlasScheduleReport
  )
import Actor.SnapshotReceiver (SnapshotVersion(..))

spec :: Spec
spec = describe "AtlasScheduleBroker" $ do
  it "returns the latest report" $ do
    ref <- newAtlasScheduleRef
    let report1 = (emptyAtlasScheduleReport (SnapshotVersion 1))
          { asrJobCount = 1
          , asrDrainMs = 3
          , asrEnqueueMs = 5
          , asrJobsAvailable = 2
          , asrJobsDispatched = 1
          }
        report2 = (emptyAtlasScheduleReport (SnapshotVersion 2))
          { asrJobCount = 4
          , asrDrainMs = 7
          , asrEnqueueMs = 11
          , asrJobsAvailable = 4
          , asrJobsDispatched = 4
          }
    writeAtlasScheduleReport ref report1
    writeAtlasScheduleReport ref report2
    mbReport <- readAtlasScheduleRef ref
    mbReport `shouldBe` Just report2
