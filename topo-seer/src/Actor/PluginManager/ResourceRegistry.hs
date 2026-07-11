{-# LANGUAGE OverloadedStrings #-}

-- | Backend-neutral plugin data-resource registry views.
module Actor.PluginManager.ResourceRegistry
  ( buildPluginDataResources
  , collectPluginDataDirs
  , collectPluginExternalDataSources
  ) where

import Data.Aeson (Value(..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import System.Directory (canonicalizePath, createDirectoryIfMissing, pathIsSymbolicLink)
import System.FilePath (normalise, splitDirectories, (</>))
import System.Info (os)

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
import Seer.World.Persist.Types
  ( WorldExternalDataSourceSnapshot(..)
  , WorldPluginDataDirectory(..)
  )
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

-- | Collect host-derived plugin data roots for connected plugins whose
-- handshake negotiated a data archive directory.
--
-- The handshake value is only used as the validated archive destination. The
-- source path is always the canonical host-created @TOPO_PLUGIN_DATA_ROOT@
-- below the plugin directory, never a plugin-supplied path.
collectPluginDataDirs :: PluginManagerState -> IO [WorldPluginDataDirectory]
collectPluginDataDirs st = do
  dirs <- traverse pluginDataDirectory (Map.elems (pmsPlugins st))
  pure [dir | Just dir <- dirs]
  where
    pluginDataDirectory lp
      | lpStatus lp /= PluginConnected = pure Nothing
      | otherwise = case lpConnection lp of
          Nothing -> pure Nothing
          Just conn -> case rpcDataDirectory conn of
            Nothing -> pure Nothing
            Just archiveDir -> do
              mSourceRoot <- canonicalPluginDataRoot lp
              pure $ do
                sourceRoot <- mSourceRoot
                pure WorldPluginDataDirectory
                  { wpddPlugin = lpName lp
                  , wpddSourceDirectory = sourceRoot
                  , wpddArchiveDirectory = Text.pack archiveDir
                  }

canonicalPluginDataRoot :: LoadedPlugin -> IO (Maybe FilePath)
canonicalPluginDataRoot lp = do
  let pluginDir = lpDirectory lp
      dataRoot = pluginDir </> "data"
  createDirectoryIfMissing True dataRoot
  dataRootIsLink <- pathIsSymbolicLink dataRoot
  if dataRootIsLink
    then pure Nothing
    else do
      canonicalPluginDir <- canonicalizePath pluginDir
      canonicalDataRoot <- canonicalizePath dataRoot
      let pluginKey = pathBoundaryKey canonicalPluginDir
          dataKey = pathBoundaryKey canonicalDataRoot
      pure $ if pathWithin pluginKey dataKey
        then Just canonicalDataRoot
        else Nothing

pathWithin :: [FilePath] -> [FilePath] -> Bool
pathWithin parent child =
  length child > length parent && parent == take (length parent) child

pathBoundaryKey :: FilePath -> [FilePath]
pathBoundaryKey path =
  let segments = splitDirectories (normalise path)
  in if os == "mingw32" then map (map toLowerChar) segments else segments

toLowerChar :: Char -> Char
toLowerChar ch
  | 'A' <= ch && ch <= 'Z' = toEnum (fromEnum ch + 32)
  | otherwise = ch

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
        , redsrStatus = (redsrStatus ref)
            { redssProviderId = Just (edsgkProvider (edsgbsKey grantState)) }
        }
      Nothing -> case redsrProvider ref of
        Just providerName
          | brokerStatusPhase (redsrStatus ref) /= Nothing -> ref
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

brokerStatusPhase :: RPCExternalDataSourceStatus -> Maybe Text
brokerStatusPhase status = do
  Object fields <- redssDiagnostics status
  String phase <- KM.lookup (Key.fromText "brokerPhase") fields
  pure phase

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
  , redssMessage = Just (unavailableStatusReason reason status)
  , redssProviderId = Just providerName
  , redssAvailability = Just ExternalAvailabilityUnavailable
  , redssHealth = Just ExternalHealthUnhealthy
  , redssAccessMode = Just ExternalAccessModeDisabled
  , redssCapabilityScope = []
  }

unavailableStatusReason :: Text -> RPCExternalDataSourceStatus -> Text
unavailableStatusReason fallback status =
  case redssMessage status of
    Just message
      | redssState status == ExternalStatusUnavailable
        && "external data-source status refresh failed" `Text.isPrefixOf` message -> message
    _ -> fallback
