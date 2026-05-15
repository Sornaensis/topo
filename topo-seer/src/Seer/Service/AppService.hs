{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Top-level service surface for topo-seer.
--
-- 'AppService' composes focused service groups that cover the existing command
-- dispatch categories.  Follow-up extraction work can provide concrete
-- implementations for these interfaces and make command IPC, UI actions, and
-- HTTP call through the same behaviour boundary.
module Seer.Service.AppService
  ( AppService(..)
  , appServiceGroups
  , appServiceOperationSpecs
  , appServiceOperationMethods
    -- * Focused services
  , module Seer.Service.State
  , module Seer.Service.Config
  , module Seer.Service.World
  , module Seer.Service.Terrain
  , module Seer.Service.Editor
  , module Seer.Service.Pipeline
  , module Seer.Service.Plugin
  , module Seer.Service.DataResource
  , module Seer.Service.Simulation
  , module Seer.Service.Log
  , module Seer.Service.Screenshot
  , module Seer.Service.UI
  , module Seer.Service.Context
  , module Seer.Service.Types
  ) where

import Data.Text (Text)

import Seer.Service.Config
import Seer.Service.Context
import Seer.Service.DataResource
import Seer.Service.Editor
import Seer.Service.Log
import Seer.Service.Pipeline
import Seer.Service.Plugin
import Seer.Service.Screenshot
import Seer.Service.Simulation
import Seer.Service.State
import Seer.Service.Terrain
import Seer.Service.Types
import Seer.Service.UI
import Seer.Service.World

-- | Aggregate service record split along stable behaviour boundaries.
data AppService = AppService
  { appState :: !StateService
  , appConfig :: !ConfigService
  , appWorld :: !WorldService
  , appTerrain :: !TerrainService
  , appEditor :: !EditorService
  , appPipeline :: !PipelineService
  , appPlugins :: !PluginService
  , appDataResources :: !DataResourceService
  , appSimulation :: !SimulationService
  , appLogs :: !LogService
  , appScreenshots :: !ScreenshotService
  , appUi :: !UiService
  }

-- | Focused service groups in the order used by diagnostics/docs/tests.
appServiceGroups :: [ServiceGroupSpec]
appServiceGroups =
  [ stateServiceGroup
  , configServiceGroup
  , worldServiceGroup
  , terrainServiceGroup
  , editorServiceGroup
  , pipelineServiceGroup
  , pluginServiceGroup
  , dataResourceServiceGroup
  , simulationServiceGroup
  , logServiceGroup
  , screenshotServiceGroup
  , uiServiceGroup
  ]

appServiceOperationSpecs :: [ServiceOperationSpec]
appServiceOperationSpecs = concatMap serviceGroupOperations appServiceGroups

appServiceOperationMethods :: [Text]
appServiceOperationMethods = serviceOperationMethods appServiceOperationSpecs
