{-# LANGUAGE OverloadedStrings #-}

-- | Dependency declarations for pipeline stages.
--
-- Each built-in stage declares which other stages must run before it.
-- 'disabledClosure' computes the transitive set of stages that must be
-- disabled when some stages are manually turned off.
module Topo.Pipeline.Dep
  ( StageDep(..)
  , builtinDependencies
  , disabledClosure
  , dependentsOf
  ) where

import Data.Set (Set)
import qualified Data.Set as Set
import Topo.Pipeline.Stage (StageId(..))

-- | A stage together with the stages it directly requires.
data StageDep = StageDep
  { sdStage    :: !StageId
    -- ^ The stage being described.
  , sdRequires :: ![StageId]
    -- ^ Direct prerequisite stages.
  } deriving (Eq, Show)

-- | The canonical dependency graph for built-in pipeline stages.
--
-- Stage ordering follows the natural generation sequence.  Each entry
-- lists only *direct* dependencies; use 'disabledClosure' to compute
-- the full transitive set when disabling stages.
builtinDependencies :: [StageDep]
builtinDependencies =
  [ StageDep StagePlateTerrain  []
  , StageDep StageErosion       [StagePlateTerrain]
  , StageDep StageHypsometry    [StagePlateTerrain]
  , StageDep StageVolcanism     [StagePlateTerrain]
  , StageDep StageHydrology     [StagePlateTerrain]
  , StageDep StageRivers        [StageHydrology]
  , StageDep StageWaterBody     [StagePlateTerrain, StageHydrology]
  , StageDep StageSoil          [StagePlateTerrain]
  , StageDep StageVegetation    [StagePlateTerrain, StageSoil]
  , StageDep StageClimate       [StagePlateTerrain, StageVegetation]
  , StageDep StageOceanCurrents [StageClimate]
  , StageDep StageGlacier       [StageClimate, StagePlateTerrain]
  , StageDep StageParameters    [StagePlateTerrain]
  , StageDep StageWaterTable    [StagePlateTerrain, StageHydrology]
  , StageDep StageBiomes        [StageClimate, StagePlateTerrain, StageVegetation]
  , StageDep StageVegetationFeedback [StageBiomes]
  , StageDep StageConvergence   [StageClimate, StageBiomes, StageVegetationFeedback]
  , StageDep StageWeather       [StageClimate, StageBiomes]
  ]

-- | Compute the transitive closure of disabled stages.
--
-- Given a dependency graph and a seed set of explicitly disabled stages,
-- returns the full set of stages that must be disabled — including all
-- stages that transitively depend on any disabled stage.
--
-- >>> disabledClosure builtinDependencies (Set.singleton StageClimate)
-- Set containing StageClimate, StageOceanCurrents, StageGlacier, StageBiomes,
--   StageVegetationFeedback, StageConvergence, StageWeather
disabledClosure :: [StageDep] -> Set StageId -> Set StageId
disabledClosure deps seed = go seed
  where
    go disabled =
      let newlyDisabled = Set.fromList
            [ sdStage dep
            | dep <- deps
            , not (Set.member (sdStage dep) disabled)
            , any (`Set.member` disabled) (sdRequires dep)
            ]
      in if Set.null newlyDisabled
           then disabled
           else go (Set.union disabled newlyDisabled)

-- | Find all stages that directly depend on a given stage.
--
-- Useful for UI tooltips explaining why a stage was auto-disabled.
dependentsOf :: [StageDep] -> StageId -> [StageId]
dependentsOf deps target =
  [ sdStage dep
  | dep <- deps
  , target `elem` sdRequires dep
  ]
