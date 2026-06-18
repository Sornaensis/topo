{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Seer.Service.Simulation
  ( SimulationService(..)
  , SimulationStateRequest(..)
  , SimulationStateResponse(..)
  , SimulationSetAutoTickRequest(..)
  , SimulationSetAutoTickResponse(..)
  , SimulationTickRequest(..)
  , SimulationTickResponse(..)
  , SimulationDagRequest(..)
  , SimulationDagResponse(..)
  , SimulationDagNodeSummary(..)
  , simulationStateOperation
  , simulationSetAutoTickOperation
  , simulationTickOperation
  , simulationDagOperation
  , simulationServiceGroup
  , simulationServiceOperationSpecs
  ) where

import Data.Text (Text)
import Data.Word (Word64)
import Topo.Simulation (SimNodeId)

import Seer.Service.Types

data SimulationService = SimulationService
  { simulationGetState :: !ServiceHandler
  , simulationSetAutoTick :: !ServiceHandler
  , simulationTick :: !ServiceHandler
  , simulationGetDag :: !ServiceHandler
  }

data SimulationStateRequest = SimulationStateRequest
  deriving (Eq, Show)

data SimulationStateResponse = SimulationStateResponse
  { simulationAutoTick :: !Bool
  , simulationTickRate :: !Float
  , simulationTickCount :: !Word64
  } deriving (Eq, Show)

data SimulationSetAutoTickRequest = SimulationSetAutoTickRequest
  { simulationSetAutoTickEnabled :: !Bool
  , simulationSetAutoTickRate :: !(Maybe Float)
  } deriving (Eq, Show)

data SimulationSetAutoTickResponse = SimulationSetAutoTickResponse
  { simulationAutoTickEnabled :: !Bool
  , simulationAutoTickRate :: !(Maybe Float)
  } deriving (Eq, Show)

newtype SimulationTickRequest = SimulationTickRequest
  { simulationRequestedTickCount :: Maybe Int
  } deriving (Eq, Show)

data SimulationTickResponse = SimulationTickResponse
  { simulationRequestedTicks :: !Int
  , simulationTargetTick :: !Word64
  } deriving (Eq, Show)

data SimulationDagRequest = SimulationDagRequest
  deriving (Eq, Show)

data SimulationDagNodeSummary = SimulationDagNodeSummary
  { simulationDagNodeId :: !SimNodeId
  , simulationDagNodeOverlay :: !Text
  , simulationDagNodeDependencies :: ![SimNodeId]
  , simulationDagNodeWritesTerrain :: !Bool
  } deriving (Eq, Show)

data SimulationDagResponse = SimulationDagResponse
  { simulationDagNodes :: ![SimulationDagNodeSummary]
  , simulationDagLevels :: ![[SimNodeId]]
  , simulationDagTerrainWriters :: ![SimNodeId]
  } deriving (Eq, Show)

simulationServiceGroup :: ServiceGroupSpec
simulationServiceGroup = ServiceGroupSpec "simulation" simulationServiceOperationSpecs

simulationServiceOperationSpecs :: [ServiceOperationSpec]
simulationServiceOperationSpecs =
  [ typedServiceOperationSpec simulationStateOperation
  , typedServiceOperationSpec simulationSetAutoTickOperation
  , typedServiceOperationSpec simulationTickOperation
  , typedServiceOperationSpec simulationDagOperation
  ]

simulationStateOperation :: TypedServiceOperation SimulationStateRequest SimulationStateResponse
simulationStateOperation = typedOperation $
  operationSpec "simulation.state" "get_sim_state" "Read simulation state."

simulationSetAutoTickOperation :: TypedServiceOperation SimulationSetAutoTickRequest SimulationSetAutoTickResponse
simulationSetAutoTickOperation = typedOperation $
  operationSpec "simulation.autoTick.set" "set_sim_auto_tick" "Enable or disable automatic simulation ticking."

simulationTickOperation :: TypedServiceOperation SimulationTickRequest SimulationTickResponse
simulationTickOperation = typedOperation $
  operationSpec "simulation.tick" "sim_tick" "Run one simulation tick."

simulationDagOperation :: TypedServiceOperation SimulationDagRequest SimulationDagResponse
simulationDagOperation = typedOperation $
  operationSpec "simulation.dag" "get_sim_dag" "Read simulation DAG status and topology."
