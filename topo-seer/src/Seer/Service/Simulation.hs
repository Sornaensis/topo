{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Seer.Service.Simulation
  ( SimulationService(..)
  , simulationServiceGroup
  , simulationServiceOperationSpecs
  ) where

import Seer.Service.Types

data SimulationService = SimulationService
  { simulationGetState :: !ServiceHandler
  , simulationSetAutoTick :: !ServiceHandler
  , simulationTick :: !ServiceHandler
  }

simulationServiceGroup :: ServiceGroupSpec
simulationServiceGroup = ServiceGroupSpec "simulation" simulationServiceOperationSpecs

simulationServiceOperationSpecs :: [ServiceOperationSpec]
simulationServiceOperationSpecs =
  [ operationSpec "simulation.state" "get_sim_state" "Read simulation state."
  , operationSpec "simulation.autoTick.set" "set_sim_auto_tick" "Enable or disable automatic simulation ticking."
  , operationSpec "simulation.tick" "sim_tick" "Run one simulation tick."
  ]
