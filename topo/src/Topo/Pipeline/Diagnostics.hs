{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Pure diagnostics for pipeline stage dependency closure and readiness.
module Topo.Pipeline.Diagnostics
  ( PipelineStageDiagnostic(..)
  , pipelineStageDiagnostics
  , inferExplicitDisabledRoots
  , stageDiagnosticFor
  , stageDiagnosticStatusText
  ) where

import Data.Maybe (listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text

import Topo.Pipeline.Dep (StageDep(..), dependentsOf, disabledClosure)
import Topo.Pipeline.Stage (StageId, stageCanonicalName)

-- | Dependency and enablement diagnostics for one stage.
data PipelineStageDiagnostic = PipelineStageDiagnostic
  { psdiagStageId :: !StageId
  , psdiagEnabled :: !Bool
  , psdiagExplicitlyDisabled :: !Bool
  , psdiagAutoDisabled :: !Bool
  , psdiagDependencies :: ![StageId]
  , psdiagDependents :: ![StageId]
  , psdiagDisabledBy :: ![StageId]
  , psdiagDiagnostics :: ![Text]
  } deriving (Eq, Show)

-- | Compute diagnostics for a stage list using the same closure semantics as
-- pipeline execution.  The stage list controls output order and may be a
-- registry/UI subset of a larger graph.
pipelineStageDiagnostics :: [StageDep] -> [StageId] -> Set StageId -> [PipelineStageDiagnostic]
pipelineStageDiagnostics deps stageIds explicitDisabled = map diagnose stageIds
  where
    allDisabled = disabledClosure deps explicitDisabled
    diagnose sid =
      let dependencies = directDependencies deps sid
          dependents = dependentsOf deps sid
          enabled = not (Set.member sid allDisabled)
          explicit = Set.member sid explicitDisabled
          autoDisabled = not explicit && Set.member sid allDisabled
          disabledBy = [dep | dep <- dependencies, Set.member dep allDisabled]
      in PipelineStageDiagnostic
        { psdiagStageId = sid
        , psdiagEnabled = enabled
        , psdiagExplicitlyDisabled = explicit
        , psdiagAutoDisabled = autoDisabled
        , psdiagDependencies = dependencies
        , psdiagDependents = dependents
        , psdiagDisabledBy = disabledBy
        , psdiagDiagnostics = diagnosticLines enabled explicit autoDisabled dependencies disabledBy dependents
        }

-- | Infer the minimal explicit disabled roots from a closure-style disabled set.
--
-- Older UI state stores the closure rather than only user toggles.  A disabled
-- stage with a disabled direct dependency can be explained as auto-disabled;
-- roots without disabled direct dependencies are treated as explicit toggles.
inferExplicitDisabledRoots :: [StageDep] -> Set StageId -> Set StageId
inferExplicitDisabledRoots deps disabled = Set.filter isRoot disabled
  where
    isRoot sid = not (any (`Set.member` disabled) (directDependencies deps sid))

-- | Look up one stage diagnostic from a computed list.
stageDiagnosticFor :: StageId -> [PipelineStageDiagnostic] -> Maybe PipelineStageDiagnostic
stageDiagnosticFor sid diagnostics = listToMaybe [diag | diag <- diagnostics, psdiagStageId diag == sid]

-- | Compact stable status text for API/UI surfaces.
stageDiagnosticStatusText :: PipelineStageDiagnostic -> Text
stageDiagnosticStatusText diag
  | psdiagEnabled diag = "enabled"
  | psdiagExplicitlyDisabled diag = "disabled"
  | psdiagAutoDisabled diag = "auto-disabled"
  | otherwise = "unknown"

directDependencies :: [StageDep] -> StageId -> [StageId]
directDependencies deps sid =
  case [requires | StageDep depSid requires <- deps, depSid == sid] of
    (requires:_) -> requires
    [] -> []

diagnosticLines :: Bool -> Bool -> Bool -> [StageId] -> [StageId] -> [StageId] -> [Text]
diagnosticLines enabled explicit autoDisabled dependencies disabledBy dependents
  | enabled =
      [ "ready: dependencies satisfied"
          <> listSuffix "dependencies" dependencies
          <> listSuffix "dependents" dependents
      ]
  | explicit =
      [ "disabled explicitly; dependents are auto-disabled by dependency closure"
          <> listSuffix "dependents" dependents
      ]
  | autoDisabled =
      [ "auto-disabled by dependency closure"
          <> listSuffix "disabled_by" disabledBy
      ]
  | otherwise = ["diagnostic state unavailable"]

listSuffix :: Text -> [StageId] -> Text
listSuffix _ [] = ""
listSuffix label ids = "; " <> label <> "=" <> Text.intercalate "," (map stageCanonicalName ids)
