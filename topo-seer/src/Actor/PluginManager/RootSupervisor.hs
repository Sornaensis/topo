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
import Data.Time (getCurrentTime)
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
  , handlePluginRuntimeFailure
  , markPluginStarting
  , markPluginStopping
  , observePluginRuntime
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
  ( DataResourceErrorCode(..)
  , MutateResource
  , MutateResult
  , QueryResource
  , QueryResult
  , dataResourceFailureFromText
  , drfCode
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
  call getRefreshSnapshot :: () -> (FilePath, [LoadedPlugin])
  cast finishRefresh :: [LoadedPlugin]
  cast shutdown :: ()
  cast finishShutdown :: ()
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
  on getPlugins = \() st -> do
    st' <- observePluginRuntimes st
    pure (st', Map.elems (pmsPlugins st'))
  on getStages = \() st -> do
    st' <- observePluginRuntimes st
    pure (st', buildPluginStages st')
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
    startingAt <- getCurrentTime
    pure st { pmsPlugins = Map.map (markPluginStarting startingAt) (pmsPlugins st) }
  onPure getRefreshSnapshot = \() st ->
    (st, (pmsBaseDir st, Map.elems (pmsPlugins st)))
  on_ finishRefresh = \plugins st ->
    pure st { pmsPlugins = Map.fromList [(lpName p, p) | p <- plugins] }
  on_ shutdown = \() st -> do
    stoppingAt <- getCurrentTime
    pure st { pmsPlugins = Map.map (markPluginStopping stoppingAt) (pmsPlugins st) }
  on_ finishShutdown = \() st -> do
    stoppedAt <- getCurrentTime
    pure st { pmsPlugins = Map.map (disconnectPlugin stoppedAt) (pmsPlugins st) }
  onPure getDataResources = \() st ->
    (st, buildPluginDataResources st)
  on queryData = \(pluginName, qr) st -> do
    result <- queryPluginDataResource pluginName qr st
    st' <- markRuntimeFailureOnConnectedDataError pluginName "data_query_failed" result st
    pure (st', result)
  on mutateData = \(pluginName, mr) st -> do
    result <- mutatePluginDataResource pluginName mr st
    st' <- markRuntimeFailureOnConnectedDataError pluginName "data_mutation_failed" result st
    pure (st', result)
  on_ notifyWorld = \mWorldPath st -> do
    notifyPluginsWorldChanged mWorldPath (Map.elems (pmsPlugins st))
    pure st
  onPure getDataDirs = \() st ->
    (st, collectPluginDataDirs st)|]

observePluginRuntimes :: PluginManagerState -> IO PluginManagerState
observePluginRuntimes st = do
  plugins' <- traverse observePluginRuntime (pmsPlugins st)
  pure st { pmsPlugins = plugins' }

markRuntimeFailureOnConnectedDataError
  :: Text
  -> Text
  -> Either Text a
  -> PluginManagerState
  -> IO PluginManagerState
markRuntimeFailureOnConnectedDataError pluginName errorCode result st =
  case result of
    Left err
      | dataResourceErrorMarksRuntimeFailure err ->
          markRuntimeFailureOnConnectedError pluginName errorCode result st
    _ -> pure st

markRuntimeFailureOnConnectedError
  :: Text
  -> Text
  -> Either Text a
  -> PluginManagerState
  -> IO PluginManagerState
markRuntimeFailureOnConnectedError pluginName errorCode result st =
  case (result, Map.lookup pluginName (pmsPlugins st)) of
    (Left err, Just lp@LoadedPlugin { lpConnection = Just _ }) -> do
      lp' <- handlePluginRuntimeFailure errorCode err lp
      pure st { pmsPlugins = Map.insert pluginName lp' (pmsPlugins st) }
    _ -> pure st

dataResourceErrorMarksRuntimeFailure :: Text -> Bool
dataResourceErrorMarksRuntimeFailure err =
  case drfCode (dataResourceFailureFromText err) of
    PluginUnavailable -> True
    ExternalDataSourceUnavailable -> True
    DataResourceTimeout -> True
    DataResourceInternalError -> True
    _ -> False
