{-# LANGUAGE OverloadedStrings #-}

-- | Backend-neutral plugin data-resource registry views.
module Actor.PluginManager.ResourceRegistry
  ( buildPluginDataResources
  , collectPluginDataDirs
  , collectPluginExternalDataSources
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)

import Actor.PluginManager.Types
  ( ExternalDataSourceGrantBrokerState(..)
  , ExternalDataSourceGrantKey(..)
  , LoadedPlugin(..)
  , PluginLifecycleSnapshot(..)
  , PluginLifecycleState(..)
  , PluginManagerState(..)
  , PluginStatus(..)
  , externalDataSourceGrantBrokerPhaseApplied
  )
import Seer.World.Persist.Types (WorldExternalDataSourceSnapshot(..))
import Topo.Plugin.DataResource (DataResourceSchema)
import Topo.Plugin.RPC
  ( RPCConnection(..)
  , RPCExternalDataSourceAccessMode(..)
  , RPCExternalDataSourceAvailability(..)
  , RPCExternalDataSourceDecl(..)
  , RPCExternalDataSourceGrant(..)
  , RPCExternalDataSourceGrantMessage(..)
  , RPCExternalDataSourceHealth(..)
  , RPCExternalDataSourceRef(..)
  , RPCExternalDataSourceStatus(..)
  , RPCExternalDataSourceStatusState(..)
  , RPCManifest(..)
  )

-- | Collect data resource schemas from all loaded plugins.
-- Returns a map from plugin name to its declared data resources.
buildPluginDataResources :: PluginManagerState -> Map Text [DataResourceSchema]
buildPluginDataResources st =
  Map.mapMaybe extractResources (pmsPlugins st)
  where
    extractResources lp =
      case rmDataResources (lpManifest lp) of
        [] -> Nothing
        rs -> Just rs

-- | Collect @(pluginName, dataDir)@ pairs from all plugins that
-- declared a data directory via the handshake.
collectPluginDataDirs :: PluginManagerState -> [(Text, FilePath)]
collectPluginDataDirs st =
  [ (lpName lp, dir)
  | lp <- Map.elems (pmsPlugins st)
  , lpStatus lp == PluginConnected
  , Just conn <- [lpConnection lp]
  , Just dir <- [rpcDataDirectory conn]
  ]

-- | Collect backend-neutral external data-source declarations and references
-- from loaded plugin manifests for world-save metadata.
collectPluginExternalDataSources :: PluginManagerState -> [WorldExternalDataSourceSnapshot]
collectPluginExternalDataSources st =
  [ WorldExternalDataSourceSnapshot
      { wedssPlugin = lpName lp
      , wedssProvidedSources = providedSources
      , wedssConsumedRefs = consumedRefs
      }
  | lp <- plugins
  , let manifest = lpManifest lp
        providedSources = markProvidedSources lp (rmExternalDataSources manifest)
        consumedRefs = map (markConsumedRefUnavailable lp) (rmExternalDataSourceRefs manifest)
  , not (null providedSources && null consumedRefs)
  ]
  where
    plugins = Map.elems (pmsPlugins st)
    disabled = pmsDisabledPlugins st
    providerReady = Map.fromList
      [(lpName lp, pluginExternalProviderReady disabled lp) | lp <- plugins]
    activeGrants = pmsExternalDataSourceGrants st

    markProvidedSources lp sources
      | pluginExternalProviderReady disabled lp = sources
      | otherwise = map (markSourceUnavailable (lpName lp) unavailableProviderReason) sources

    markConsumedRefUnavailable lp ref = case activeGrantForRef lp ref of
      Just grantState -> ref
        { redsrProvider = Just (edsgkProvider (edsgbsKey grantState))
        , redsrStatus = redsgmStatus (edsgbsMessage grantState)
        }
      Nothing -> case redsrProvider ref of
        Just providerName
          | Map.findWithDefault False providerName providerReady -> ref
          | otherwise ->
              ref
                { redsrStatus =
                    unavailableStatus providerName unavailableProviderReason (redsrStatus ref)
                }
        _ -> ref

    activeGrantForRef lp ref =
      let matches grantState =
            let key = edsgbsKey grantState
            in externalDataSourceGrantBrokerPhaseApplied (edsgbsState grantState)
              && edsgkConsumer key == lpName lp
              && edsgkRef key == redsrName ref
      in case filter matches (Map.elems activeGrants) of
        grantState:_ -> Just grantState
        [] -> Nothing

pluginExternalProviderReady :: Set.Set Text -> LoadedPlugin -> Bool
pluginExternalProviderReady disabled lp =
  not (Set.member (lpName lp) disabled)
    && plsState (lpLifecycle lp) == LifecycleReady

unavailableProviderReason :: Text
unavailableProviderReason = "provider plugin is unavailable"

markSourceUnavailable :: Text -> Text -> RPCExternalDataSourceDecl -> RPCExternalDataSourceDecl
markSourceUnavailable providerName reason source = source
  { redsdStatus = unavailableStatus providerName reason (redsdStatus source)
  , redsdGrants = map (markGrantUnavailable providerName reason) (redsdGrants source)
  }

markGrantUnavailable :: Text -> Text -> RPCExternalDataSourceGrant -> RPCExternalDataSourceGrant
markGrantUnavailable providerName reason grant = grant
  { redsgStatus = unavailableStatus providerName reason (redsgStatus grant)
  }

unavailableStatus :: Text -> Text -> RPCExternalDataSourceStatus -> RPCExternalDataSourceStatus
unavailableStatus providerName reason status = status
  { redssState = ExternalStatusUnavailable
  , redssMessage = Just reason
  , redssProviderId = Just providerName
  , redssAvailability = Just ExternalAvailabilityUnavailable
  , redssHealth = Just ExternalHealthUnhealthy
  , redssAccessMode = Just ExternalAccessModeDisabled
  , redssCapabilityScope = []
  }
