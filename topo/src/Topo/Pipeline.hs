{-# LANGUAGE OverloadedStrings #-}

-- | Pipeline execution engine.
--
-- Runs a sequence of 'PipelineStage's, threading 'TerrainWorld' state
-- through each stage.  Stages may be disabled via 'pipelineDisabled';
-- the dependency closure is computed automatically and dependents of
-- disabled stages are auto-skipped.  Progress is reported through an
-- optional 'pipelineOnProgress' callback.
module Topo.Pipeline
  ( -- * Stage identity
    module Topo.Pipeline.Stage
    -- * Dependencies
  , module Topo.Pipeline.Dep
    -- * Pipeline types
  , PipelineStage(..)
  , PipelineConfig(..)
  , PipelineSnapshot(..)
  , PipelineError(..)
  , StageProgress(..)
  , StageStatus(..)
  , defaultPipelineConfig
    -- * Execution
  , runPipeline
  ) where

import Control.Monad (foldM)
import Control.Monad.IO.Class (liftIO)
import Data.Bits (xor)
import Data.Char (ord)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import Topo.Pipeline.Dep (builtinDependencies, disabledClosure)
import Topo.Pipeline.Stage (StageId, stageCanonicalName)
import Topo.Plugin
import Topo.World (TerrainWorld)

-- | A single pipeline stage with a typed identity.
data PipelineStage = PipelineStage
  { stageId      :: !StageId
    -- ^ Typed stage identifier (used for dependency resolution and UI).
  , stageName    :: !Text
    -- ^ Human-readable display name.
  , stageSeedTag :: !Text
    -- ^ Seed derivation tag (deterministic sub-seed).
  , stageRun     :: PluginM ()
    -- ^ Stage action to execute.
  }

-- | Pipeline configuration including stage list, disabling, and progress.
data PipelineConfig = PipelineConfig
  { pipelineSeed       :: !Word64
    -- ^ Master seed for deterministic generation.
  , pipelineStages     :: ![PipelineStage]
    -- ^ Ordered list of stages to run.
  , pipelineDisabled   :: !(Set StageId)
    -- ^ Explicitly disabled stages (dependents auto-disabled).
  , pipelineSnapshots  :: !Bool
    -- ^ Whether to capture world snapshots after each stage.
  , pipelineOnProgress :: !(StageProgress -> IO ())
    -- ^ Progress callback invoked before/after each stage.
  }

-- | A sensible default: no stages, no disabling, no snapshots, silent progress.
defaultPipelineConfig :: PipelineConfig
defaultPipelineConfig = PipelineConfig
  { pipelineSeed       = 0
  , pipelineStages     = []
  , pipelineDisabled   = Set.empty
  , pipelineSnapshots  = False
  , pipelineOnProgress = \_ -> pure ()
  }

-- | Per-stage progress report.
data StageProgress = StageProgress
  { spStageIndex :: !Int
    -- ^ Zero-based index of this stage in the pipeline.
  , spStageCount :: !Int
    -- ^ Total number of stages in the pipeline.
  , spStageId    :: !StageId
    -- ^ Which stage this report is about.
  , spStageName  :: !Text
    -- ^ Human-readable stage name.
  , spStatus     :: !StageStatus
    -- ^ Current status of the stage.
  } deriving (Eq, Show)

-- | Status of a pipeline stage.
data StageStatus
  = StageStarted
  | StageCompleted
  | StageSkipped
  deriving (Eq, Show)

data PipelineSnapshot = PipelineSnapshot
  { snapshotName  :: !Text
  , snapshotWorld :: !TerrainWorld
  }

-- | Pipeline failures surfaced from plugin stages.
data PipelineError
  = PipelinePluginError !PluginError
  deriving (Eq, Show)

-- | Execute a pipeline, skipping disabled stages and their dependents.
--
-- Returns the final 'TerrainWorld' and any accumulated snapshots.
-- The resolved set of all disabled stages (including auto-disabled
-- dependents) is computed from 'pipelineDisabled' using
-- 'disabledClosure'.
runPipeline :: PipelineConfig -> TopoEnv -> TerrainWorld -> IO (Either PipelineError (TerrainWorld, [PipelineSnapshot]))
runPipeline config env world = do
  let logger = teLogger env
      pluginEnv = PluginEnv
        { peLogger = logger
        , peSeed = pipelineSeed config
        , peCaps = allowAllCapabilities
        }
      allDisabled = disabledClosure builtinDependencies (pipelineDisabled config)
      stages = pipelineStages config
      stageCount = length stages
      onProgress = pipelineOnProgress config
  (result, world') <- runTopoM env world $
    foldM (runStage pluginEnv allDisabled stageCount onProgress (pipelineSnapshots config))
          (Right (0, []))
          stages
  pure $ case result of
    Left err -> Left (PipelinePluginError err)
    Right (_, snapshots) -> Right (world', snapshots)

runStage
  :: PluginEnv
  -> Set StageId
  -> Int
  -> (StageProgress -> IO ())
  -> Bool
  -> Either PluginError (Int, [PipelineSnapshot])
  -> PipelineStage
  -> TopoM (Either PluginError (Int, [PipelineSnapshot]))
runStage pluginEnv disabled stageCount onProgress snapshotsEnabled acc stage =
  case acc of
    Left err -> pure (Left err)
    Right (idx, snapshots) -> do
      let sid  = stageId stage
          name = stageName stage
          progress status = StageProgress
            { spStageIndex = idx
            , spStageCount = stageCount
            , spStageId    = sid
            , spStageName  = name
            , spStatus     = status
            }
      if Set.member sid disabled
        then do
          topoLog ("stage:skip " <> name <> " (" <> stageCanonicalName sid <> ")")
          liftIO (onProgress (progress StageSkipped))
          pure (Right (idx + 1, snapshots))
        else do
          let stageEnv = pluginEnv { peSeed = deriveStageSeed (peSeed pluginEnv) (stageSeedTag stage) }
          topoLog ("stage:start " <> name)
          liftIO (onProgress (progress StageStarted))
          result <- runPluginM stageEnv (stageRun stage)
          topoLog ("stage:end " <> name)
          case result of
            Left err -> pure (Left err)
            Right _ -> do
              liftIO (onProgress (progress StageCompleted))
              if snapshotsEnabled
                then do
                  w <- getWorld
                  pure (Right (idx + 1, snapshots <> [PipelineSnapshot name w]))
                else pure (Right (idx + 1, snapshots))

-- | Derive a stage-specific seed from the pipeline master seed.
deriveStageSeed :: Word64 -> Text -> Word64
deriveStageSeed base tag =
  let salt = hashText tag
  in base `xor` salt

hashText :: Text -> Word64
hashText = Text.foldl' step 1469598103934665603
  where
    step h c = (h `xor` fromIntegral (ord c)) * 1099511628211
