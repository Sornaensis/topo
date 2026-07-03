{-# LANGUAGE OverloadedStrings #-}

-- | World/simulation integration for connected plugin sessions.
module Actor.PluginManager.SimulationIntegrator
  ( notifyPluginWorldChanged
  , notifyPluginsWorldChanged
  , PluginSimulationPlan(..)
  , PluginSimulationNodeDiagnostic(..)
  , buildPluginSimulationPlan
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Word (Word64)
import qualified Data.Text as Text

import Actor.PluginManager.PipelineIntegrator (orderPlugins)
import Actor.PluginManager.Types
  ( LoadedPlugin(..)
  , PluginLifecycleSnapshot(..)
  , PluginLifecycleState(..)
  , PluginManagerState(..)
  , PluginStatus(..)
  , pluginLifecycleStateText
  , requiresRuntimeConnection
  )
import Actor.Simulation (SimulationNodeBinding(..))
import Topo.Overlay.Schema (OverlaySchema(..))
import Topo.Plugin (Capability(..))
import Topo.Plugin.RPC
  ( RPCConnection(..)
  , RPCManifest(..)
  , RPCSimulationDecl(..)
  , RPCStartPolicy(..)
  , manifestWritesTerrain
  , rpcSimNode
  , sendWorldChanged
  )
import Topo.Simulation.Schedule
  ( SimulationScheduleDecl(..)
  , catchUpPolicyText
  , scheduleDeclError
  )

-- | Executable plugin sim nodes plus declaration diagnostics for the current
-- plugin-manager view and optional bound-world overlay set.
data PluginSimulationPlan = PluginSimulationPlan
  { pspExecutableNodes :: ![SimulationNodeBinding]
  , pspDiagnostics :: ![PluginSimulationNodeDiagnostic]
  }

-- | User/API-facing status for every plugin simulation declaration.  A node is
-- executable only when it is included in the actor-owned Simulation DAG.
data PluginSimulationNodeDiagnostic = PluginSimulationNodeDiagnostic
  { psndId :: !Text
  , psndPlugin :: !Text
  , psndOverlay :: !Text
  , psndDependencies :: ![Text]
  , psndWritesTerrain :: !Bool
  , psndScheduleIntervalTicks :: !Word64
  , psndSchedulePhaseTicks :: !Word64
  , psndScheduleCatchUp :: !Text
  , psndEnabled :: !Bool
  , psndExecutable :: !Bool
  , psndStatus :: !Text
  , psndStatusDetail :: !(Maybe Text)
  } deriving (Eq, Show)

-- | Send a world-change notification to all connected plugins.
notifyPluginsWorldChanged :: Maybe Text -> [LoadedPlugin] -> IO ()
notifyPluginsWorldChanged mWorldPath plugins =
  mapM_ (notifyPluginWorldChanged mWorldPath) plugins

-- | Send a 'MsgWorldChanged' notification to a single plugin.
-- No-op if the plugin is not connected.
notifyPluginWorldChanged :: Maybe Text -> LoadedPlugin -> IO ()
notifyPluginWorldChanged mWorldPath lp =
  case (lpStatus lp, lpConnection lp) of
    (PluginConnected, Just conn) -> do
      _ <- sendWorldChanged conn mWorldPath
      pure ()
    _ -> pure ()

-- | Build executable plugin simulation nodes and truthful diagnostics from one
-- consistent PluginManager snapshot.  Dependencies are simulation node IDs, not
-- generator-stage names; only executable nodes are allowed as dependencies.
buildPluginSimulationPlan :: Maybe [Text] -> PluginManagerState -> PluginSimulationPlan
buildPluginSimulationPlan mOverlayNames st = PluginSimulationPlan
  { pspExecutableNodes =
      [ SimulationNodeBinding
          { snbNode = rpcSimNode (connectionForPlugin (lrPlugin local) conn)
          , snbKind = "plugin"
          , snbPlugin = Just (lrName local)
          }
      | local <- executableLocals
      , Just conn <- [lpConnection (lrPlugin local)]
      ]
  , pspDiagnostics = map (diagnosticFor disabled allDeclaredNames executableNameSet executableWriterNames) locals
  }
  where
    disabled = pmsDisabledPlugins st
    overlaySet = Set.fromList <$> mOverlayNames
    ordered = orderPlugins (pmsPluginOrder st) (Map.elems (pmsPlugins st))
    locals =
      [ mkLocal disabled overlaySet plugin simDecl
      | plugin <- ordered
      , Just simDecl <- [rmSimulation (lpManifest plugin)]
      ]
    executableLocals = resolveExecutableLocals locals
    executableNameSet = Set.fromList (map lrName executableLocals)
    executableWriterNames = Set.fromList [lrName local | local <- executableLocals, lrWritesTerrain local]
    allDeclaredNames = Set.fromList (map lrName locals)

builtinSimulationNodeIds :: Set.Set Text
builtinSimulationNodeIds = Set.singleton "weather"

canWriteOverlay :: RPCManifest -> Bool
canWriteOverlay manifest = CapWriteOverlay `elem` rmCapabilities manifest
  || CapWriteWorld `elem` rmCapabilities manifest

policyDisabled :: LoadedPlugin -> Bool
policyDisabled lp =
  requiresRuntimeConnection (lpManifest lp) && not (rspAutoStart (lpStartPolicy lp))

connectionForPlugin :: LoadedPlugin -> RPCConnection -> RPCConnection
connectionForPlugin lp conn = conn
  { rpcManifest = lpManifest lp
  , rpcParams = lpParams lp
  , rpcRequestTimeoutMicros = requestTimeoutMicros (lpStartPolicy lp)
  }

requestTimeoutMicros :: RPCStartPolicy -> Maybe Int
requestTimeoutMicros policy
  | rspRequestTimeoutMs policy <= 0 = Nothing
  | otherwise = Just (rspRequestTimeoutMs policy * 1000)

data LocalSimDecl = LocalSimDecl
  { lrName :: !Text
  , lrPlugin :: !LoadedPlugin
  , lrSimulation :: !RPCSimulationDecl
  , lrSchedule :: !SimulationScheduleDecl
  , lrWritesTerrain :: !Bool
  , lrLocalIssue :: !(Maybe Text)
  }

mkLocal :: Set.Set Text -> Maybe (Set.Set Text) -> LoadedPlugin -> RPCSimulationDecl -> LocalSimDecl
mkLocal disabled overlaySet plugin simDecl = LocalSimDecl
  { lrName = pluginName
  , lrPlugin = plugin
  , lrSimulation = simDecl
  , lrSchedule = rsdSchedule simDecl
  , lrWritesTerrain = manifestWritesTerrain manifest
  , lrLocalIssue = firstJust
      [ if Set.member pluginName disabled
          then Just "disabled by user"
          else Nothing
      , if policyDisabled plugin
          then Just "disabled by plugin start policy; runtime auto_start=false"
          else Nothing
      , if rmName manifest /= pluginName
          then Just ("manifest name " <> rmName manifest <> " does not match loaded plugin name " <> pluginName)
          else Nothing
      , if Set.member pluginName builtinSimulationNodeIds
          then Just ("simulation node id " <> pluginName <> " collides with a built-in node")
          else Nothing
      , fmap ("invalid simulation schedule: " <>) (scheduleDeclError (rsdSchedule simDecl))
      , case lpOverlaySchema plugin of
          Nothing -> Just "overlay schema is not loaded"
          Just schema
            | osName schema /= pluginName ->
                Just ("overlay schema name " <> osName schema <> " does not match plugin-owned overlay " <> pluginName)
            | otherwise -> Nothing
      , case overlaySet of
          Nothing -> Just "no world is currently bound"
          Just names
            | Set.member pluginName names -> Nothing
            | otherwise -> Just ("plugin-owned overlay " <> pluginName <> " is not present in the bound world")
      , case lpLifecycle plugin of
          lifecycle
            | plsState lifecycle /= LifecycleReady ->
                Just ("plugin lifecycle is " <> pluginLifecycleStateText (plsState lifecycle)
                  <> maybe "" (": " <>) (plsErrorMessage lifecycle))
            | otherwise -> Nothing
      , case (lpStatus plugin, lpConnection plugin) of
          (PluginConnected, Just _) -> Nothing
          (PluginError err, _) -> Just ("plugin runtime error: " <> err)
          (status, _) -> Just ("plugin runtime is not connected: " <> pluginStatusLabel status)
      , if canWriteOverlay manifest
          then Nothing
          else Just "manifest missing writeOverlay/writeWorld capability required for simulation overlay updates"
      ]
  }
  where
    pluginName = lpName plugin
    manifest = lpManifest plugin

resolveExecutableLocals :: [LocalSimDecl] -> [LocalSimDecl]
resolveExecutableLocals locals = go Set.empty Set.empty [] locals
  where
    go executableNames writerNames acc remaining =
      case partitionReady executableNames writerNames remaining of
        ([], _) -> acc
        (ready, rest) ->
          let executableNames' = Set.union executableNames (Set.fromList (map lrName ready))
              writerNames' = Set.union writerNames (Set.fromList [lrName local | local <- ready, lrWritesTerrain local])
          in go executableNames' writerNames' (acc <> ready) rest

    partitionReady executableNames writerNames = foldr step ([], [])
      where
        step local (ready, rest)
          | localReady executableNames writerNames local = (local : ready, rest)
          | otherwise = (ready, local : rest)

localReady :: Set.Set Text -> Set.Set Text -> LocalSimDecl -> Bool
localReady executableNames writerNames local =
  lrLocalIssue local == Nothing
    && all dependencyExecutable (rsdDependencies (lrSimulation local))
    && not (readerDependsOnWriter writerNames local)
  where
    dependencyExecutable dep = Set.member dep builtinSimulationNodeIds || Set.member dep executableNames

readerDependsOnWriter :: Set.Set Text -> LocalSimDecl -> Bool
readerDependsOnWriter writerNames local =
  not (lrWritesTerrain local)
    && any (`Set.member` writerNames) (rsdDependencies (lrSimulation local))

diagnosticFor :: Set.Set Text -> Set.Set Text -> Set.Set Text -> Set.Set Text -> LocalSimDecl -> PluginSimulationNodeDiagnostic
diagnosticFor disabled allDeclaredNames executableNames executableWriterNames local = PluginSimulationNodeDiagnostic
  { psndId = lrName local
  , psndPlugin = lrName local
  , psndOverlay = lrName local
  , psndDependencies = deps
  , psndWritesTerrain = lrWritesTerrain local
  , psndScheduleIntervalTicks = schedDeclIntervalTicks schedule
  , psndSchedulePhaseTicks = schedDeclPhaseTicks schedule
  , psndScheduleCatchUp = catchUpPolicyText (schedDeclCatchUpPolicy schedule)
  , psndEnabled = not (Set.member (lrName local) disabled) && not (policyDisabled plugin)
  , psndExecutable = executable
  , psndStatus = status
  , psndStatusDetail = Just detail
  }
  where
    plugin = lrPlugin local
    deps = rsdDependencies (lrSimulation local)
    schedule = lrSchedule local
    executable = Set.member (lrName local) executableNames
    missingDeps = [dep | dep <- deps, not (Set.member dep builtinSimulationNodeIds), not (Set.member dep allDeclaredNames)]
    blockedDeps = [dep | dep <- deps, Set.member dep allDeclaredNames, not (Set.member dep executableNames)]
    writerDeps = [dep | dep <- deps, Set.member dep executableWriterNames]
    (status, detail)
      | executable = ("Ready", "Ready; simulation node executes in the actor-owned tick DAG.")
      | Just issue <- lrLocalIssue local = (statusForLocalIssue issue, issue)
      | not (null missingDeps) =
          ("WaitingForDependencies", "missing simulation dependencies: " <> Text.intercalate ", " missingDeps)
      | not (lrWritesTerrain local) && not (null writerDeps) =
          ("WaitingForDependencies", "reader simulation node depends on terrain writer(s), unsupported by the two-phase executor: " <> Text.intercalate ", " writerDeps)
      | not (null blockedDeps) =
          ("WaitingForDependencies", "simulation dependencies are not executable: " <> Text.intercalate ", " blockedDeps)
      | otherwise =
          ("WaitingForDependencies", "cycle or unresolved simulation dependencies prevent execution")

statusForLocalIssue :: Text -> Text
statusForLocalIssue issue
  | "disabled" `Text.isPrefixOf` issue = "Disabled"
  | "plugin runtime error" `Text.isPrefixOf` issue = "Failed"
  | "plugin lifecycle is failed" `Text.isPrefixOf` Text.toLower issue = "Failed"
  | "plugin lifecycle is degraded" `Text.isPrefixOf` Text.toLower issue = "Degraded"
  | otherwise = "WaitingForDependencies"

pluginStatusLabel :: PluginStatus -> Text
pluginStatusLabel status = case status of
  PluginIdle -> "idle"
  PluginConnected -> "connected"
  PluginError err -> "error: " <> err
  PluginDisconnected -> "disconnected"

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Nothing:rest) = firstJust rest
firstJust (Just value:_) = Just value
