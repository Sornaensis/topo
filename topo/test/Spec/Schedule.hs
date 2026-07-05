{-# LANGUAGE OverloadedStrings #-}

module Spec.Schedule (spec) where

import qualified Data.Map.Strict as Map
import Test.Hspec

import Topo.Overlay
  ( Overlay(..)
  , OverlayProvenance(..)
  , emptyOverlay
  , emptyOverlayProvenance
  , emptyOverlayStore
  , insertOverlay
  , lookupOverlay
  )
import Topo.Overlay.Schema
  ( OverlayDeps(..)
  , OverlaySchema(..)
  , OverlayStorage(..)
  )
import Topo.Simulation
  ( SimNode(..)
  , SimNodeId(..)
  , SimulationCatchUpPolicy(..)
  , SimulationScheduleDecl(..)
  , SimulationScheduleState(..)
  , defaultScheduleDecl
  , ensureOverlaySchedule
  , ensureWorldOverlaySchedules
  , hourlyScheduleDecl
  , reconcileOverlaySchedule
  , initialScheduleAt
  , markScheduleFired
  , normalizeScheduleDecl
  , scheduleDue
  , scheduleStateMatchesDecl
  )
import Topo.Hex (defaultHexGridMeta)
import Topo.Types (WorldConfig(..))
import Topo.World (TerrainWorld(..), emptyWorld)

spec :: Spec
spec = describe "Simulation.Schedule" $ do
  it "initializes hourly schedules after the current tick" $ do
    let state = initialScheduleAt 0 hourlyScheduleDecl
    schedIntervalTicks state `shouldBe` 1
    schedPhaseTicks state `shouldBe` 0
    schedLastFireTick state `shouldBe` Nothing
    schedNextFireTick state `shouldBe` 1
    schedCatchUpPolicy state `shouldBe` RunOnceIfDue

  it "normalizes interval and phase declarations" $ do
    let decl = SimulationScheduleDecl
          { schedDeclIntervalTicks = 0
          , schedDeclPhaseTicks = 7
          , schedDeclCatchUpPolicy = SkipMissed
          }
    normalizeScheduleDecl decl `shouldBe` SimulationScheduleDecl
      { schedDeclIntervalTicks = 1
      , schedDeclPhaseTicks = 0
      , schedDeclCatchUpPolicy = SkipMissed
      }

  it "detects due schedules and advances after a fired target tick" $ do
    let decl = SimulationScheduleDecl 3 1 RunOnceIfDue
        state0 = initialScheduleAt 0 decl
    schedNextFireTick state0 `shouldBe` 1
    scheduleDue 0 state0 `shouldBe` False
    scheduleDue 1 state0 `shouldBe` True
    let state1 = markScheduleFired 7 state0
    schedLastFireTick state1 `shouldBe` Just 7
    schedNextFireTick state1 `shouldBe` 10

  it "fills missing overlay schedules without overwriting persisted state" $ do
    let overlay0 = emptyOverlay scheduleSchema
        filled = ensureOverlaySchedule 0 defaultScheduleDecl overlay0
    (schedNextFireTick <$> opSchedule (ovProvenance filled)) `shouldBe` Just 1

    let existingSchedule = SimulationScheduleState
          { schedIntervalTicks = 24
          , schedPhaseTicks = 3
          , schedLastFireTick = Just 51
          , schedNextFireTick = 75
          , schedCatchUpPolicy = SkipMissed
          }
        overlayWithSchedule = overlay0
          { ovProvenance = emptyOverlayProvenance
              { opSchedule = Just existingSchedule
              }
          }
        preserved = ensureOverlaySchedule 0 defaultScheduleDecl overlayWithSchedule
    opSchedule (ovProvenance preserved) `shouldBe` Just existingSchedule

  it "fills world overlay schedules from node declarations" $ do
    let overlay = emptyOverlay scheduleSchema
        world0 = (emptyWorld (WorldConfig { wcChunkSize = 4 }) defaultHexGridMeta)
          { twOverlays = insertOverlay overlay emptyOverlayStore
          }
        world1 = ensureWorldOverlaySchedules [scheduleNode] world0
    case lookupOverlay "scheduled" (twOverlays world1) of
      Nothing -> expectationFailure "expected scheduled overlay"
      Just loaded ->
        (schedNextFireTick <$> opSchedule (ovProvenance loaded)) `shouldBe` Just 1

  it "rebases overlay schedules when declarations change" $ do
    let decl6 = defaultScheduleDecl { schedDeclIntervalTicks = 6 }
        matching = SimulationScheduleState
          { schedIntervalTicks = 6
          , schedPhaseTicks = 0
          , schedLastFireTick = Just 12
          , schedNextFireTick = 18
          , schedCatchUpPolicy = RunOnceIfDue
          }
        mismatched = matching { schedIntervalTicks = 1, schedNextFireTick = 13 }
        catchUpMismatch = matching { schedCatchUpPolicy = SkipMissed }
        withExisting sched = (emptyOverlay scheduleSchema)
          { ovProvenance = emptyOverlayProvenance { opSchedule = Just sched }
          }
        preserved = reconcileOverlaySchedule 12 decl6 (withExisting matching)
        rebased = reconcileOverlaySchedule 12 decl6 (withExisting mismatched)
        rebasedCatchUp = reconcileOverlaySchedule 12 decl6 (withExisting catchUpMismatch)
    scheduleStateMatchesDecl decl6 matching `shouldBe` True
    scheduleStateMatchesDecl decl6 catchUpMismatch `shouldBe` False
    opSchedule (ovProvenance preserved) `shouldBe` Just matching
    opSchedule (ovProvenance rebasedCatchUp) `shouldBe` opSchedule (ovProvenance rebased)
    case opSchedule (ovProvenance rebased) of
      Nothing -> expectationFailure "expected rebased schedule"
      Just sched -> do
        schedIntervalTicks sched `shouldBe` 6
        schedPhaseTicks sched `shouldBe` 0
        schedLastFireTick sched `shouldBe` Nothing
        schedNextFireTick sched `shouldBe` 18
        schedCatchUpPolicy sched `shouldBe` RunOnceIfDue

scheduleSchema :: OverlaySchema
scheduleSchema = OverlaySchema
  { osName = "scheduled"
  , osVersion = "1.0.0"
  , osDescription = "Schedule test overlay"
  , osFields = []
  , osStorage = StorageSparse
  , osDependencies = OverlayDeps False []
  , osFieldIndex = Map.empty
  }

scheduleNode :: SimNode
scheduleNode = SimNodeReader
  { snrId = SimNodeId "scheduled"
  , snrOverlayName = "scheduled"
  , snrDependencies = []
  , snrSchedule = defaultScheduleDecl
  , snrReadTick = \_ctx overlay -> pure (Right overlay)
  }
