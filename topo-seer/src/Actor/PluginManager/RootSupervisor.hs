{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | The public PluginManager actor protocol and root supervision loop.
-- The actor keeps external messages stable while delegating scanner,
-- lifecycle, routing, and integration work to focused modules.
module Actor.PluginManager.RootSupervisor
  ( PluginManager
  , pluginManagerActorDef
  ) where

import Data.Aeson (Value)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import Data.Text (Text)
import Hyperspace.Actor
import Hyperspace.Actor.QQ (hyperspace)
import System.Directory (createDirectoryIfMissing)

import Actor.PluginManager.Config
  ( loadDisabledPlugins
  , loadPluginOrder
  , saveDisabledPlugins
  , savePluginConfig
  , savePluginOrder
  )
import Actor.PluginManager.DataResourceRouter
  ( mutatePluginDataResource
  , queryPluginDataResource
  )
import Actor.PluginManager.PipelineIntegrator
  ( buildPluginOverlaySchemas
  , buildPluginStages
  )
import Actor.PluginManager.PluginSupervisor
  ( disconnectPlugin
  , refreshAllManifests
  , shutdownPlugin
  )
import Actor.PluginManager.ResourceRegistry
  ( buildPluginDataResources
  , collectPluginDataDirs
  )
import Actor.PluginManager.Scanner
  ( pluginsBaseDir
  , scanPluginDirs
  )
import Actor.PluginManager.SimulationIntegrator (notifyPluginsWorldChanged)
import Actor.PluginManager.Types
  ( LoadedPlugin(..)
  , PluginManagerState(..)
  , emptyPluginManagerState
  , setParamOnPlugin
  )
import Topo.Overlay.Schema (OverlaySchema)
import Topo.Pipeline (PipelineStage)
import Topo.Plugin.DataResource (DataResourceSchema)
import Topo.Plugin.RPC
  ( MutateResource
  , MutateResult
  , QueryResource
  , QueryResult
  )

[hyperspace|
actor PluginManager
  state PluginManagerState
  lifetime Singleton
  schedule pinned 1
  noDeps
  mailbox Unbounded

  cast discover :: ()
  call getPlugins :: () -> [LoadedPlugin]
  call getStages :: () -> [PipelineStage]
  call getOverlaySchemas :: () -> [OverlaySchema]
  call getOrder :: () -> [Text]
  call getDisabled :: () -> Set Text
  cast setParam :: (Text, Text, Value)
  cast setOrder :: [Text]
  cast setDisabled :: Set Text
  cast refresh :: ()
  cast shutdown :: ()
  call getDataResources :: () -> Map Text [DataResourceSchema]
  call queryData :: (Text, QueryResource) -> Either Text QueryResult
  call mutateData :: (Text, MutateResource) -> Either Text MutateResult
  cast notifyWorld :: Maybe Text
  call getDataDirs :: () -> [(Text, FilePath)]

  initial emptyPluginManagerState
  on_ discover = \() st -> do
    baseDir <- pluginsBaseDir
    createDirectoryIfMissing True baseDir
    plugins <- scanPluginDirs baseDir
    orderTxt <- loadPluginOrder baseDir
    disabled <- loadDisabledPlugins baseDir
    pure st
      { pmsPlugins = Map.fromList [(lpName p, p) | p <- plugins]
      , pmsPluginOrder = orderTxt
      , pmsBaseDir = baseDir
      , pmsDisabledPlugins = disabled
      }
  onPure getPlugins = \() st ->
    (st, Map.elems (pmsPlugins st))
  onPure getStages = \() st ->
    (st, buildPluginStages st)
  onPure getOverlaySchemas = \() st ->
    (st, buildPluginOverlaySchemas st)
  onPure getOrder = \() st ->
    (st, pmsPluginOrder st)
  onPure getDisabled = \() st ->
    (st, pmsDisabledPlugins st)
  on_ setParam = \(pluginName, paramName, value) st -> do
    let st' = st { pmsPlugins = Map.adjust (setParamOnPlugin paramName value) pluginName (pmsPlugins st) }
    case Map.lookup pluginName (pmsPlugins st') of
      Just lp -> savePluginConfig (lpDirectory lp) (lpParams lp)
      Nothing -> pure ()
    pure st'
  on_ setOrder = \order st -> do
    savePluginOrder (pmsBaseDir st) order
    pure st { pmsPluginOrder = order }
  on_ setDisabled = \disabled st -> do
    saveDisabledPlugins (pmsBaseDir st) disabled
    pure st { pmsDisabledPlugins = disabled }
  on_ refresh = \() st -> do
    plugins' <- refreshAllManifests (pmsBaseDir st) (pmsPlugins st)
    pure st { pmsPlugins = plugins' }
  on_ shutdown = \() st -> do
    mapM_ shutdownPlugin (Map.elems (pmsPlugins st))
    pure st { pmsPlugins = Map.map disconnectPlugin (pmsPlugins st) }
  onPure getDataResources = \() st ->
    (st, buildPluginDataResources st)
  on queryData = \(pluginName, qr) st -> do
    result <- queryPluginDataResource pluginName qr st
    pure (st, result)
  on mutateData = \(pluginName, mr) st -> do
    result <- mutatePluginDataResource pluginName mr st
    pure (st, result)
  on_ notifyWorld = \mWorldPath st -> do
    notifyPluginsWorldChanged mWorldPath (Map.elems (pmsPlugins st))
    pure st
  onPure getDataDirs = \() st ->
    (st, collectPluginDataDirs st)|]
