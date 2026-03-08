{-# LANGUAGE OverloadedStrings #-}

-- | Hydrology stages for flow routing and moisture.
module Topo.Hydrology
  ( HydroConfig(..)
  , defaultHydroConfig
  , applyHydrologyStage
  , RiverConfig(..)
  , defaultRiverConfig
  , GroundwaterConfig(..)
  , defaultGroundwaterConfig
  , applyRiverStage
    -- * Flow routing (exported for testing)
  , flowDirections
  , flowDirectionsLand
    -- * Depression filling (exported for testing)
  , fillDepressions
  , breachRemainingSinks
    -- * Sink breaching (exported for testing)
  , breachSinksLand
    -- * Grid operations (exported for testing)
  , flowAccumulation
  , piedmontSmoothGrid
  , carveRiversGrid
  , alluvialDepositGrid
  ) where

import qualified Data.IntMap.Strict as IntMap
import Topo.Hydrology.Compat
  ( alluvialDepositGrid
  , breachRemainingSinks
  , breachSinksLand
  , carveRiversGrid
  , fillDepressions
  , flowAccumulation
  , flowDirections
  , flowDirectionsLand
  , piedmontSmoothGrid
  )
import Topo.Hydrology.Config
  ( GroundwaterConfig(..)
  , HydroConfig(..)
  , RiverConfig(..)
  , defaultGroundwaterConfig
  , defaultHydroConfig
  , defaultRiverConfig
  )
import qualified Topo.Hydrology.River as River
import qualified Topo.Hydrology.StageHydrology as StageHydrology
import Topo.Parameters (TerrainFormConfig)
import Topo.Pipeline (PipelineStage(..))
import Topo.Pipeline.Stage (StageId(..))
import Control.Monad.Except (throwError)
import Topo.Plugin (logInfo, getWorldP, putWorldP, PluginError(..))
import Topo.River (RiverTopologyConfig(..))
import Topo.TerrainGrid
  ( buildElevationGrid
  , buildHardnessGrid
  , buildPlateHardnessGrid
  , updateChunkElevationFromGrid
  , updateChunkMoistureFromGrid
  , validateTerrainGrid
  )
import Topo.Types
import Topo.World (TerrainWorld(..))

-- | Apply hydrology routing and moisture derivation.
--
-- The 'TerrainFormConfig' drives a lightweight pre-classification of
-- terrain forms from the current elevation + plate hardness grids.
-- Per-form modifiers ('defaultTerrainFormModifiers') then adjust river
-- carving, bank erosion, stream-power erosion, deposition, coastal
-- reshaping, piedmont smoothing, and wet-area erosion.
applyHydrologyStage :: HydroConfig -> TerrainFormConfig -> PipelineStage
applyHydrologyStage cfg formCfg = PipelineStage StageHydrology "applyHydrology" "applyHydrology" Nothing [] Nothing $ do
  logInfo "applyHydrology: routing flow + moisture"
  world <- getWorldP
  let config = twConfig world
      terrain = twTerrain world
  (ChunkCoord minCx minCy, ChunkCoord maxCx maxCy) <-
    case validateTerrainGrid config terrain of
      Left err -> throwError (PluginInvariantError err)
      Right bounds -> pure bounds
  let size = wcChunkSize config
      gridW = (maxCx - minCx + 1) * size
      gridH = (maxCy - minCy + 1) * size
      elev0 = buildElevationGrid config terrain (ChunkCoord minCx minCy) gridW gridH
      hardness = buildHardnessGrid config terrain (ChunkCoord minCx minCy) gridW gridH
      plateHardness = buildPlateHardnessGrid config terrain (ChunkCoord minCx minCy) gridW gridH
      (elevFinal, moisture) =
        StageHydrology.computeHydrologyGrids
          cfg
          formCfg
          gridW
          gridH
          elev0
          hardness
          plateHardness
      terrain' = IntMap.mapWithKey (updateChunkElevationFromGrid config (ChunkCoord minCx minCy) gridW elevFinal) terrain
      terrain'' = IntMap.mapWithKey (updateChunkMoistureFromGrid config (ChunkCoord minCx minCy) gridW moisture) terrain'
  putWorldP world { twTerrain = terrain'' }

-- | Apply river routing and basin-level groundwater storage.
--
-- The water level parameter is the global sea-level threshold used for
-- flow sinks during river routing.  It is typically sourced from
-- 'hcWaterLevel' via the pipeline orchestrator.
--
-- A 'RiverTopologyConfig' controls segment extraction parameters
-- (discharge thresholds, network pruning, ocean-reachability, etc.).
applyRiverStage
  :: RiverConfig
  -> RiverTopologyConfig
  -> GroundwaterConfig
  -> Float          -- ^ Water level (from 'HydroConfig')
  -> PipelineStage
applyRiverStage riverCfg topoCfg gwCfg waterLevel = PipelineStage StageRivers "applyRivers" "applyRivers" Nothing [] Nothing $ do
  logInfo "applyRivers: routing rivers + groundwater"
  world <- getWorldP
  let config = twConfig world
      terrain = twTerrain world
  (ChunkCoord minCx minCy, ChunkCoord maxCx maxCy) <-
    case validateTerrainGrid config terrain of
      Left err -> throwError (PluginInvariantError err)
      Right bounds -> pure bounds
  let size = wcChunkSize config
      gridW = (maxCx - minCx + 1) * size
      gridH = (maxCy - minCy + 1) * size
      (terrain', rivers'', groundwater'') =
        River.buildRiverStageWorldLayers
          config
          (ChunkCoord minCx minCy)
          gridW
          gridH
          terrain
          riverCfg
          topoCfg
          gwCfg
          waterLevel
  putWorldP world { twTerrain = terrain', twRivers = rivers'', twGroundwater = groundwater'' }


