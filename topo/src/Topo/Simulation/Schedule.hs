-- | Pure simulation schedule declarations and durable per-overlay state.
--
-- A schedule declaration is the static cadence a node asks for.  A schedule
-- state is the persisted overlay-local cursor derived from that declaration.
-- The default catch-up policy is 'RunOnceIfDue': once a target tick is at or
-- past 'schedNextFireTick', the scheduler should run the node at most once and
-- then advance the cursor beyond that target tick.
module Topo.Simulation.Schedule
  ( SimulationCatchUpPolicy(..)
  , SimulationScheduleDecl(..)
  , SimulationScheduleState(..)
  , defaultCatchUpPolicy
  , defaultScheduleDecl
  , hourlyScheduleDecl
  , normalizeScheduleDecl
  , normalizeScheduleState
  , initialScheduleAt
  , scheduleDue
  , markScheduleFired
  , ensureNextFireAfter
  , catchUpPolicyTag
  , catchUpPolicyFromTag
  ) where

import Data.Word (Word64, Word8)

-- | Policy for handling ticks missed while a world was paused or unloaded.
data SimulationCatchUpPolicy
  = RunOnceIfDue
    -- ^ If one or more intervals are due, run the node once and advance past
    -- the target tick.  This is the default policy used by built-in nodes.
  | SkipMissed
    -- ^ Advance past missed intervals without requiring one run per interval.
  deriving (Eq, Show)

-- | Static schedule declaration attached to a simulation node.
data SimulationScheduleDecl = SimulationScheduleDecl
  { schedDeclIntervalTicks :: !Word64
    -- ^ Cadence in simulation ticks.  Values below 1 are normalized to 1.
  , schedDeclPhaseTicks :: !Word64
    -- ^ Phase within the interval.  Normalized to @< interval@.
  , schedDeclCatchUpPolicy :: !SimulationCatchUpPolicy
    -- ^ How a scheduler should handle missed due ticks.
  } deriving (Eq, Show)

-- | Persisted per-overlay schedule cursor.
data SimulationScheduleState = SimulationScheduleState
  { schedIntervalTicks :: !Word64
    -- ^ Cadence in simulation ticks.  Values below 1 are normalized to 1 by
    -- helper functions and rejected by storage decoding.
  , schedPhaseTicks :: !Word64
    -- ^ Phase within the interval.
  , schedLastFireTick :: !(Maybe Word64)
    -- ^ Last target tick at which this schedule fired, if any.
  , schedNextFireTick :: !Word64
    -- ^ Next target tick at which this schedule is due.
  , schedCatchUpPolicy :: !SimulationCatchUpPolicy
    -- ^ Catch-up behavior for missed due intervals.
  } deriving (Eq, Show)

-- | Default catch-up policy used by built-in hourly declarations.
defaultCatchUpPolicy :: SimulationCatchUpPolicy
defaultCatchUpPolicy = RunOnceIfDue

-- | Hourly schedule: one simulation tick equals one world hour.
hourlyScheduleDecl :: SimulationScheduleDecl
hourlyScheduleDecl = SimulationScheduleDecl
  { schedDeclIntervalTicks = 1
  , schedDeclPhaseTicks = 0
  , schedDeclCatchUpPolicy = defaultCatchUpPolicy
  }

-- | Default simulation schedule declaration.
defaultScheduleDecl :: SimulationScheduleDecl
defaultScheduleDecl = hourlyScheduleDecl

-- | Normalize a declaration so interval is at least 1 and phase is in range.
normalizeScheduleDecl :: SimulationScheduleDecl -> SimulationScheduleDecl
normalizeScheduleDecl decl =
  let interval = max 1 (schedDeclIntervalTicks decl)
  in decl
    { schedDeclIntervalTicks = interval
    , schedDeclPhaseTicks = schedDeclPhaseTicks decl `mod` interval
    }

-- | Normalize a persisted state for use by pure scheduling helpers.
normalizeScheduleState :: SimulationScheduleState -> SimulationScheduleState
normalizeScheduleState state =
  let interval = max 1 (schedIntervalTicks state)
  in state
    { schedIntervalTicks = interval
    , schedPhaseTicks = schedPhaseTicks state `mod` interval
    }

-- | Build initial persisted state from a static declaration.
--
-- The initial next-fire tick is strictly after the supplied current tick.  For
-- the default hourly declaration, @initialScheduleAt 0 hourlyScheduleDecl@ has
-- @schedNextFireTick == 1@.
initialScheduleAt :: Word64 -> SimulationScheduleDecl -> SimulationScheduleState
initialScheduleAt currentTick decl0 =
  let decl = normalizeScheduleDecl decl0
      interval = schedDeclIntervalTicks decl
      phase = schedDeclPhaseTicks decl
  in SimulationScheduleState
    { schedIntervalTicks = interval
    , schedPhaseTicks = phase
    , schedLastFireTick = Nothing
    , schedNextFireTick = nextScheduledTickAfter currentTick interval phase
    , schedCatchUpPolicy = schedDeclCatchUpPolicy decl
    }

-- | Test whether a schedule is due at the supplied target tick.
scheduleDue :: Word64 -> SimulationScheduleState -> Bool
scheduleDue targetTick state = targetTick >= schedNextFireTick state

-- | Mark a schedule as fired at the supplied target tick.
--
-- This records the fire tick and advances the next-fire cursor strictly beyond
-- the target tick, so a catch-up run happens at most once for that target.
markScheduleFired :: Word64 -> SimulationScheduleState -> SimulationScheduleState
markScheduleFired targetTick state =
  (ensureNextFireAfter targetTick state)
    { schedLastFireTick = Just targetTick
    }

-- | Ensure the next-fire cursor is strictly after the supplied target tick.
ensureNextFireAfter :: Word64 -> SimulationScheduleState -> SimulationScheduleState
ensureNextFireAfter targetTick state0 =
  let state = normalizeScheduleState state0
  in if schedNextFireTick state > targetTick
       then state
       else state
         { schedNextFireTick = nextScheduledTickAfter
             targetTick
             (schedIntervalTicks state)
             (schedPhaseTicks state)
         }

-- | Stable binary tag for a catch-up policy.
catchUpPolicyTag :: SimulationCatchUpPolicy -> Word8
catchUpPolicyTag RunOnceIfDue = 0
catchUpPolicyTag SkipMissed = 1

-- | Decode a stable binary catch-up policy tag.
catchUpPolicyFromTag :: Word8 -> Maybe SimulationCatchUpPolicy
catchUpPolicyFromTag 0 = Just RunOnceIfDue
catchUpPolicyFromTag 1 = Just SkipMissed
catchUpPolicyFromTag _ = Nothing

nextScheduledTickAfter :: Word64 -> Word64 -> Word64 -> Word64
nextScheduledTickAfter currentTick interval phase
  | currentTick < phase = phase
  | otherwise =
      let periodsElapsed = (currentTick - phase) `div` interval
      in phase + (periodsElapsed + 1) * interval
