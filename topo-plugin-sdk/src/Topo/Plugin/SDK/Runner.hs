{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

-- | Plugin process runner.
--
-- This module provides 'runPlugin', the entry point for a plugin
-- executable, plus explicit manifest-writing helpers for install/package
-- workflows. topo-seer discovers only directories that already contain
-- @manifest.json@; it does not execute unknown plugin directories to
-- bootstrap manifests.
--
-- === Usage
--
-- @
-- import Topo.Plugin.SDK
--
-- main :: IO ()
-- main = runPluginWithManifestCommand myPluginDef
-- @
--
-- Authors can then run @my-plugin --topo-write-manifest ~/.topo/plugins/my-plugin@
-- during installation to write @manifest.json@ without starting the RPC
-- transport loop. Normal host launches fall through to 'runPlugin'.
module Topo.Plugin.SDK.Runner
  ( -- * Entry point
    runPlugin
  , runPluginWithManifestCommand
  , runPluginSession
  , runPluginSessionWithLimits
  , SDKSessionError(..)
    -- * Manifest generation
  , pluginManifestFileName
  , generateManifest
  , writeManifest
  , writePluginManifest
  , writePluginManifestToDirectory
  ) where

import Control.Exception (Exception, SomeException, catch, finally, fromException, onException, throwIO)
import Data.Aeson (Value(..), (.=), object)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List (nub)
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Word (Word64)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.Environment (getArgs, lookupEnv, unsetEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeDirectory)
import System.IO (stderr, stdin, stdout)
import System.IO.Unsafe (unsafePerformIO)

import Topo.Plugin.RPC.Manifest
  ( Capability(..)
  , RPCCapability
  , RPCGeneratorDecl(..)
  , RPCInvocationScopes(..)
  , RPCInvocationScopeDecl(..)
  , RPCScopeInput(..)
  , RPCScopeOutput(..)
  , RPCScopeBudgets(..)
  , RPCChunkSelector(..)
  , TerrainSection(..)
  , legacyGeneratorScope
  , legacySimulationScope
  , RPCManifest(..)
  , RPCManifestRuntime(..)
  , RPCOverlayDecl(..)
  , RPCParamSpec(..)
  , RPCParamType(..)
  , RPCSimulationDecl(..)
  , RPCUIHints(..)
  , manifestV3
  )
import Topo.Plugin.RPC.Protocol
  ( RPCEnvelope(..)
  , RPCMessageType(..)
  , InvokeGenerator(..)
  , InvokeSimulation(..)
  , GeneratorResult(..)
  , SimulationResult(..)
  , Handshake(..)
  , HandshakeAck(..)
  , WorldChanged(..)
  , Heartbeat(..)
  , HealthStatus(..)
  , PluginProgress(..)
  , currentProtocolVersion
  , maximumSupportedProtocolVersion
  , minimumSupportedProtocolVersion
  , handshakeAuthProof
  , encodeMessageLazy
  , decodeMessage
  )
import Topo.Plugin.RPC.ExternalDataSource
  ( RPCExternalDataSourceGrantMessage(..)
  , RPCExternalDataSourceGrantRevocation(..)
  , RPCExternalDataSourceOperation(..)
  , RPCExternalDataSourceOperationResult(..)
  , RPCExternalDataSourceStatusRequest(..)
  , externalDataSourceStatusReportFromManifest
  )
import Topo.Plugin.RPC.Transport
  ( Transport(..)
  , TransportError(..)
  , RPCPayloadLimits
  , defaultRPCPayloadLimits
  , mkRPCPayloadLimits
  , rplMaxFrameSizeBytes
  , sendLazyMessageWithLimit
  , recvMessageWithLimit
  , closeTransport
  , connectPluginFromEnvironment
  , pluginAuthTokenEnv
  , pluginSessionEnv
  , readRPCPayloadLimitsFromEnvironment
  )
import Topo.Hex (defaultHexGridMeta)
import Topo.Plugin.DataResource (DataResourceSchema(..), DataOperations(..))
import Topo.Plugin.DataResource.Validation
  ( validateQueryResourceRequest
  , validateQueryResult
  )
import Topo.Plugin.RPC.DataService
  ( DataMutation(..), DataQuery(..), DataResourceErrorCode(..)
  , DataResourceFailure(..), QueryResource(..), QueryResult(..), DataRecord(..)
  , MutateResource(..), MutateResult(..)
  , dataResourceErrorCodeText, dataResourceErrorRPCCode, dataResourceFailureFromText
  )
import Topo.Plugin.SDK.Payload (decodeTerrainPayloadWithLimits)
import Topo.Plugin.RPC
  ( applyGeneratorTerrainValueScopedWithLimits
  , decodeTerrainWritesValueScopedWithLimits
  )
import Topo.Plugin.RPC.Scope
  ( RPCDataOperation(..)
  , RPCInvocationKind(..)
  , RPCInvocationScopeBinding(..)
  , ResolvedInvocationScope(..)
  , ScopeError(..)
  , validateInvocationScopeBinding
  )
import Topo.Simulation.Schedule (defaultScheduleDecl)
import Topo.Types (WorldConfig(..))
import Topo.World (TerrainWorld, emptyWorld)

import Topo.Plugin.SDK.Types

------------------------------------------------------------------------
-- Manifest generation
------------------------------------------------------------------------

-- | Convert a 'ParamType' to the RPC manifest equivalent.
toRPCParamType :: ParamType -> RPCParamType
toRPCParamType PFloat = ParamFloat
toRPCParamType PInt   = ParamInt
toRPCParamType PBool  = ParamBool

-- | Convert a 'ParamDef' to an RPC parameter spec.
toRPCParamSpec :: ParamDef -> RPCParamSpec
toRPCParamSpec pd = RPCParamSpec
  { rpsName    = paramName pd
  , rpsLabel   = paramLabel pd
  , rpsType    = toRPCParamType (paramType pd)
  , rpsRange   = case (paramMin pd, paramMax pd) of
      (Just lo, Just hi) -> Just (lo, hi)
      _                  -> Nothing
  , rpsDefault = paramDefault pd
  , rpsTooltip = paramTooltip pd
  }

-- | Generate an 'RPCManifest' from a 'PluginDef'.
--
-- Capabilities are safely inferred from the plugin's declared generator,
-- simulation, and data resources, then merged with explicit capabilities from
-- 'pdCapabilities'. Generator terrain output is implicit in generator
-- participation; @writeTerrain@/@writeWorld@ are for simulation terrain
-- writers.
generateManifest :: PluginDef -> RPCManifest
generateManifest pd = RPCManifest
  { rmManifestVersion = manifestV3
  , rmName          = pdName pd
  , rmVersion       = pdVersion pd
  , rmRuntime       = RPCManifestRuntime
      { rmrProtocolMin = protocolMin pd
      , rmrProtocolMax = protocolMax pd
      , rmrTopoMin = pdRuntimeTopoMin pd
      , rmrTopoMax = pdRuntimeTopoMax pd
      }
  , rmDescription   = sdkDescription pd
  , rmUiHints       = sdkUiHints pd
  , rmGenerator     = generatorDecl pd
  , rmSimulation    = simulationDecl pd
  , rmInvocationScopes = invocationScopes pd
  , rmOverlay       = fmap (\f -> RPCOverlayDecl (Text.pack f)) (pdSchemaFile pd)
  , rmCapabilities  = sdkCapabilities pd
  , rmParameters    = map toRPCParamSpec (pdParams pd)
  , rmDataResources = map drdSchema (pdDataResources pd)
  , rmDataDirectory = fmap Text.pack (pdDataDirectory pd)
  , rmExternalDataSources = pdExternalDataSources pd
  , rmExternalDataSourceRefs = pdExternalDataSourceRefs pd
  , rmStartPolicy   = pdStartPolicy pd
  }

protocolMin :: PluginDef -> Int
protocolMin pd = fst (protocolRange pd)

protocolMax :: PluginDef -> Int
protocolMax pd = snd (protocolRange pd)

protocolRange :: PluginDef -> (Int, Int)
protocolRange pd
  | supportsV4 && supportsV5 && hasScoped =
      (minimumSupportedProtocolVersion, maximumSupportedProtocolVersion)
  | supportsV5 && hasScoped =
      (maximumSupportedProtocolVersion, maximumSupportedProtocolVersion)
  | supportsV4 && not hasScoped = (currentProtocolVersion, currentProtocolVersion)
  | otherwise = error
      "PluginDef mixes generator/simulation adapters without a complete protocol-v4 or protocol-v5 implementation"
  where
    hasScoped = isJust scopedGenerator || isJust scopedSimulation
    generatorParticipates = isJust legacyGenerator || isJust scopedGenerator
    simulationParticipates = isJust legacySimulation || isJust scopedSimulation
    supportsV4 = (not generatorParticipates || isJust legacyGenerator)
      && (not simulationParticipates || isJust legacySimulation)
    supportsV5 = (not generatorParticipates || isJust scopedGenerator)
      && (not simulationParticipates || isJust scopedSimulation)
    legacyGenerator = pdGenerator pd
    legacySimulation = pdSimulation pd
    scopedGenerator = pdGeneratorScope pd
    scopedSimulation = pdSimulationScope pd

generatorDecl :: PluginDef -> Maybe RPCGeneratorDecl
generatorDecl pd = case pdGeneratorScope pd of
  Just scoped -> Just RPCGeneratorDecl
    { rgdInsertAfter = gsdInsertAfter scoped
    , rgdRequires = gsdRequires scoped
    }
  Nothing -> fmap (\legacy -> RPCGeneratorDecl
    { rgdInsertAfter = gdInsertAfter legacy
    , rgdRequires = gdRequires legacy
    }) (pdGenerator pd)

simulationDecl :: PluginDef -> Maybe RPCSimulationDecl
simulationDecl pd = case pdSimulationScope pd of
  Just scoped -> Just RPCSimulationDecl
    { rsdDependencies = ssdDependencies scoped
    , rsdSchedule = maybe defaultScheduleDecl id (ssdSchedule scoped)
    }
  Nothing -> fmap (\legacy -> RPCSimulationDecl
    { rsdDependencies = sdDependencies legacy
    , rsdSchedule = maybe defaultScheduleDecl id (sdSchedule legacy)
    }) (pdSimulation pd)

invocationScopes :: PluginDef -> Maybe RPCInvocationScopes
invocationScopes pd
  | not (isJust generatorScope || isJust simulationScope) = Nothing
  | otherwise = Just RPCInvocationScopes
      { riscVersion = 1
      , riscGenerator = generatorScope
      , riscSimulation = simulationScope
      }
  where
    generatorScope = case pdGeneratorScope pd of
      Just scoped -> Just (gsdScope scoped)
      Nothing -> fmap (const legacyGen) (pdGenerator pd)
    simulationScope = case pdSimulationScope pd of
      Just scoped -> Just (ssdScope scoped)
      Nothing -> fmap (\legacy -> legacySimulationScope (sdDependencies legacy) legacyBudgets)
        (pdSimulation pd)
    broadGenerator = legacyGeneratorScope legacyBudgets
    legacyGen = broadGenerator
      { risdOutput = (risdOutput broadGenerator)
          { rsoOwnedOverlay = pdSchemaFile pd /= Nothing }
      }
    legacyBudgets = RPCScopeBudgets maxBound maxBound maxBound

sdkDescription :: PluginDef -> Text
sdkDescription pd = case pdDescription pd of
  Just description -> description
  Nothing -> pdName pd <> " v" <> pdVersion pd

sdkUiHints :: PluginDef -> RPCUIHints
sdkUiHints pd =
  let hints = pdUiHints pd
  in case ruiDisplayName hints of
      Nothing -> hints { ruiDisplayName = Just (pdName pd) }
      Just _  -> hints

sdkCapabilities :: PluginDef -> [RPCCapability]
sdkCapabilities pd = nub (inferCapabilities pd <> pdCapabilities pd)

-- | Infer safe capabilities from the plugin definition.
--
-- Generator and simulation participation infer terrain input access. Generator
-- terrain output is implicit in 'pdGenerator' participation, so it never needs
-- inferred 'CapWriteTerrain' or 'CapWriteWorld'. Terrain writes are
-- intentionally not inferred from the mere presence of a simulation callback:
-- ordinary simulation plugins usually update only their owned overlay. Plugins
-- that return simulation terrain writes should request 'CapWriteTerrain'
-- explicitly via 'pdCapabilities'. Generator-only plugins that return overlay
-- output should explicitly request 'CapWriteOverlay'; manifests that already
-- have 'CapWriteWorld' also satisfy the host overlay-write check.
inferCapabilities :: PluginDef -> [RPCCapability]
inferCapabilities pd = concat
  [ [CapLog]
  , [CapReadTerrain | legacyTerrain || scopedTerrainRead]
  , [CapReadOverlay | legacySimulation || scopedOverlayRead]
  , [CapWriteTerrain | scopedSimulationTerrainWrite]
  , [CapWriteOverlay | legacySimulation || scopedOverlayWrite]
  , [CapDataRead | hasData]
  , [CapDataWrite | hasData && hasDataWriteOps]
  ]
  where
    legacyTerrain = isJust (pdGenerator pd) || legacySimulation
    legacySimulation = isJust (pdSimulation pd)
    scopedDeclarations =
      [gsdScope scope | Just scope <- [pdGeneratorScope pd]] <>
      [ssdScope scope | Just scope <- [pdSimulationScope pd]]
    scopedTerrainRead = any (not . null . rsiTerrainSections . risdInput) scopedDeclarations
    scopedOverlayRead = any (\scope -> rsiOwnOverlay (risdInput scope)
      || not (null (rsiDependencyOverlays (risdInput scope)))) scopedDeclarations
    scopedSimulationTerrainWrite = maybe False
      (not . null . rsoTerrainSections . risdOutput . ssdScope) (pdSimulationScope pd)
    scopedOverlayWrite = any (rsoOwnedOverlay . risdOutput) scopedDeclarations
    hasData = not (null (pdDataResources pd))
    hasDataWriteOps = any hasWriteOps (pdDataResources pd)
    hasWriteOps drd =
      let ops = drsOperations (drdSchema drd)
      in doCreate ops || doUpdate ops || doDelete ops

-- | Standard manifest filename used by topo-seer discovery.
pluginManifestFileName :: FilePath
pluginManifestFileName = "manifest.json"

-- | Write the manifest JSON to a file.
writeManifest :: FilePath -> RPCManifest -> IO ()
writeManifest path manifest =
  BL.writeFile path (Aeson.encode manifest)

-- | Generate and write a plugin manifest without opening any RPC transport.
--
-- Use this from packaging or install steps when you want a concrete
-- @manifest.json@ from a 'PluginDef'. The parent directory is created when
-- needed.
writePluginManifest :: FilePath -> PluginDef -> IO ()
writePluginManifest path pd = do
  createDirectoryIfMissing True (takeDirectory path)
  writeManifest path (generateManifest pd)

-- | Generate @manifest.json@ in a plugin install directory without opening any
-- RPC transport. Returns the written manifest path.
writePluginManifestToDirectory :: FilePath -> PluginDef -> IO FilePath
writePluginManifestToDirectory pluginDir pd = do
  createDirectoryIfMissing True pluginDir
  let manifestPath = pluginDir </> pluginManifestFileName
  writePluginManifest manifestPath pd
  pure manifestPath

------------------------------------------------------------------------
-- Entry point
------------------------------------------------------------------------

-- | Fatal SDK transport failure. Send errors carry the known envelope type,
-- request id, actual encoded size, configured limit, and transport cause.
data SDKSessionError
  = SDKReceiveFailure !TransportError
  | SDKSendFailure !RPCMessageType !(Maybe Word64) !Int !Integer !TransportError
  | SDKRequestPayloadRejected !RPCMessageType !Word64 !Int !Integer
  deriving (Show)

instance Exception SDKSessionError

-- | Run a plugin.
--
-- This is the standard host-launched entry point for a plugin executable.
-- It refreshes @manifest.json@ in the plugin working directory, connects to the
-- host-created production endpoint advertised in the TOPO_PLUGIN_* environment,
-- then enters the length-prefixed RPC message loop. If no endpoint environment
-- is present, stdin/stdout compatibility is available only when
-- @TOPO_PLUGIN_STDIO_COMPAT=1@ is explicitly set for a test or development
-- harness. Do not rely on this startup write for discovery; use
-- 'runPluginWithManifestCommand' or 'writePluginManifestToDirectory' during
-- install/package steps so @manifest.json@ exists before topo-seer scans.
--
-- The message loop dispatches:
--
-- * @invoke_generator@ → 'gdRun' from 'pdGenerator'
-- * @invoke_simulation@ → 'sdTick' from 'pdSimulation'
-- * @shutdown@ → clean exit
--
-- === Example
--
-- @
-- main :: IO ()
-- main = runPlugin myPluginDef
-- @
runPlugin :: PluginDef -> IO ()
runPlugin pd = do
  -- Refresh manifest for already-discovered, host-launched plugins.
  cwd <- getCurrentDirectory
  _ <- writePluginManifestToDirectory cwd pd

  -- Connect via the host-created endpoint; stdio requires explicit
  -- test/development compatibility opt-in.
  limitsResult <- readRPCPayloadLimitsFromEnvironment
  case limitsResult of
    Left err -> do
      TextIO.hPutStrLn stderr ("SDK: invalid RPC payload limits: " <> err)
      exitFailure
    Right limits -> do
      transportResult <- connectPluginFromEnvironment (pdName pd) stdin stdout
      case transportResult of
        Left err -> do
          TextIO.hPutStrLn stderr ("SDK: transport error: " <> Text.pack (show err))
          exitFailure
        Right transport -> do
          -- Build default param map
          let defaultParams = Map.fromList
                [ (paramName p, paramDefault p)
                | p <- pdParams pd
                ]
          runPluginSessionWithLimits limits pd transport defaultParams
            `finally` closeTransport transport

-- | Entry point wrapper that supports an explicit manifest-only install action.
--
-- Supported commands:
--
-- * @--topo-write-manifest@ writes @manifest.json@ in the current directory.
-- * @--topo-write-manifest DIR@ writes @DIR/manifest.json@.
-- * @--topo-write-manifest-file PATH@ writes an explicit manifest file path.
--
-- Any other argument list runs the normal RPC plugin entry point. The manifest
-- commands return after writing and never connect to the RPC transport.
runPluginWithManifestCommand :: PluginDef -> IO ()
runPluginWithManifestCommand pd = do
  args <- getArgs
  case args of
    ["--topo-write-manifest"] -> do
      _ <- writePluginManifestToDirectory "." pd
      pure ()
    ["--topo-write-manifest", pluginDir] -> do
      _ <- writePluginManifestToDirectory pluginDir pd
      pure ()
    ["--topo-write-manifest-file", manifestPath] ->
      writePluginManifest manifestPath pd
    flag:_
      | flag == "--topo-write-manifest" || flag == "--topo-write-manifest-file" -> do
          TextIO.hPutStrLn stderr "Usage: plugin [--topo-write-manifest [DIR] | --topo-write-manifest-file PATH]"
          exitFailure
    _ -> runPlugin pd

-- | Run the SDK dispatch loop on an already-connected transport.
--
-- Intended for in-process integration tests and hosts that manage
-- transport lifecycle externally.
runPluginSession :: PluginDef -> Transport -> Map Text Value -> IO ()
runPluginSession = runPluginSessionWithLimits defaultRPCPayloadLimits

-- | Run an embedded SDK session with explicit symmetric payload limits.
runPluginSessionWithLimits
  :: RPCPayloadLimits
  -> PluginDef
  -> Transport
  -> Map Text Value
  -> IO ()
runPluginSessionWithLimits limits pd transport params = do
  resetExternalOperationCache pd
  negotiatedVersion <- newIORef (protocolMax pd)
  messageLoop limits pd transport params Nothing negotiatedVersion
    `onException` closeTransport transport

type ExternalOperationKey = (RPCExternalDataSourceOperation, Text)

type ExternalOperationCache = Map ExternalOperationKey RPCExternalDataSourceOperationResult

externalOperationCaches :: IORef (Map Text ExternalOperationCache)
externalOperationCaches = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE externalOperationCaches #-}

resetExternalOperationCache :: PluginDef -> IO ()
resetExternalOperationCache pd =
  atomicModifyIORef' externalOperationCaches $ \caches ->
    (Map.delete (pdName pd) caches, ())

lookupExternalOperationResult
  :: PluginDef
  -> RPCExternalDataSourceOperation
  -> Maybe Text
  -> IO (Maybe RPCExternalDataSourceOperationResult)
lookupExternalOperationResult _ _ Nothing = pure Nothing
lookupExternalOperationResult pd operation (Just operationId) =
  atomicModifyIORef' externalOperationCaches $ \caches ->
    (caches, Map.lookup (operation, operationId) =<< Map.lookup (pdName pd) caches)

cacheExternalOperationResult :: PluginDef -> RPCExternalDataSourceOperationResult -> IO ()
cacheExternalOperationResult pd result
  | not (redsoAccepted result && redsoApplied result) = pure ()
  | otherwise =
      atomicModifyIORef' externalOperationCaches $ \caches ->
        let pluginName = pdName pd
            cache = Map.findWithDefault Map.empty pluginName caches
            key = (redsoOperation result, redsoOperationId result)
            cache' = Map.filter (not . invalidatedByAppliedOperation result) cache
        in (Map.insert pluginName (Map.insert key result cache') caches, ())

invalidatedByAppliedOperation :: RPCExternalDataSourceOperationResult -> RPCExternalDataSourceOperationResult -> Bool
invalidatedByAppliedOperation applied cached =
  redsoOperation applied /= redsoOperation cached
    && sameExternalOperationBinding applied cached

sameExternalOperationBinding :: RPCExternalDataSourceOperationResult -> RPCExternalDataSourceOperationResult -> Bool
sameExternalOperationBinding a b =
  redsoProviderId a == redsoProviderId b
    && redsoConsumerId a == redsoConsumerId b
    && redsoSource a == redsoSource b
    && redsoGrant a == redsoGrant b

data LaunchAuth = LaunchAuth
  { laSessionId :: !Text
  , laAuthToken :: !Text
  }

readLaunchAuthFromEnvironment :: IO (Maybe LaunchAuth)
readLaunchAuthFromEnvironment = do
  mSession <- lookupEnv pluginSessionEnv
  mAuthToken <- lookupEnv pluginAuthTokenEnv
  pure $ case (mSession, mAuthToken) of
    (Just sessionId, Just authToken) -> Just LaunchAuth
      { laSessionId = Text.pack sessionId
      , laAuthToken = Text.pack authToken
      }
    _ -> Nothing

launchAuthProof :: Maybe LaunchAuth -> Maybe Text -> Maybe (Text, Text)
launchAuthProof (Just launchAuth) (Just challenge) = Just
  ( laSessionId launchAuth
  , handshakeAuthProof (laSessionId launchAuth) (laAuthToken launchAuth) challenge
  )
launchAuthProof _ _ = Nothing

consumeLaunchAuthProofFromEnvironment :: Maybe Text -> IO (Maybe (Text, Text))
consumeLaunchAuthProofFromEnvironment mChallenge = do
  launchAuth <- readLaunchAuthFromEnvironment
  let proof = launchAuthProof launchAuth mChallenge
  scrubLaunchAuthEnvironment
  pure proof

scrubLaunchAuthEnvironment :: IO ()
scrubLaunchAuthEnvironment = do
  unsetEnv pluginSessionEnv
  unsetEnv pluginAuthTokenEnv

-- | Main message loop. Reads RPC envelopes with the same limit used for every
-- response. Any receive or send failure aborts the session deterministically.
messageLoop :: RPCPayloadLimits -> PluginDef -> Transport -> Map Text Value -> Maybe FilePath -> IORef Int -> IO ()
messageLoop limits pd transport params worldPath negotiatedVersionRef = do
  result <- recvMessageWithLimit (fromIntegral (rplMaxFrameSizeBytes limits)) transport
  nextState <- case result of
    Left err -> throwIO (SDKReceiveFailure err)
    Right bs -> case decodeMessage bs of
      Left decodeErr -> do
        sendErrorResponse limits transport Nothing 1 ("Failed to decode RPC message: " <> decodeErr)
        next params worldPath
      Right envelope -> dispatch envelope `catch` handleSDKRequestFailure
  case nextState of
    Nothing -> pure ()
    Just (nextParams, nextWorldPath) ->
      messageLoop limits pd transport nextParams nextWorldPath negotiatedVersionRef
  where
    next nextParams nextWorldPath = pure (Just (nextParams, nextWorldPath))

    handleSDKRequestFailure :: SDKSessionError -> IO (Maybe (Map Text Value, Maybe FilePath))
    handleSDKRequestFailure err = case err of
      SDKRequestPayloadRejected _ _ _ _ -> next params worldPath
      _ -> throwIO err

    preserveStateOnRejected nextParams nextWorld action = catch action $ \err -> case err of
      SDKRequestPayloadRejected _ _ _ _ -> next nextParams nextWorld
      _ -> throwIO err

    dispatch :: RPCEnvelope -> IO (Maybe (Map Text Value, Maybe FilePath))
    dispatch envelope = case envType envelope of
      MsgShutdown -> pure Nothing

      MsgHandshake -> case Aeson.fromJSON (envPayload envelope) of
        Aeson.Error err -> do
          sendErrorResponse limits transport (envRequestId envelope) 8
            ("Invalid handshake payload: " <> Text.pack err)
          next params worldPath
        Aeson.Success (hs :: Handshake) -> do
          mAuthProof <- consumeLaunchAuthProofFromEnvironment (hsAuthChallenge hs)
          let newWorldPath = fmap Text.unpack (hsWorldPath hs)
              requestedVersion = hsProtocolVersion hs
              runtime = rmRuntime (generateManifest pd)
              supported = requestedVersion >= rmrProtocolMin runtime
                && requestedVersion <= rmrProtocolMax runtime
              ack = HandshakeAck
                { haProtocolVersion = requestedVersion
                , haDataDirectory = fmap Text.pack (pdDataDirectory pd)
                , haResources = map drdSchema (pdDataResources pd)
                , haSessionId = fst <$> mAuthProof
                , haAuthProof = snd <$> mAuthProof
                }
          if not supported
            then do
              sendErrorResponse limits transport (envRequestId envelope) 8
                "Handshake protocol is outside this plugin definition's advertised range"
              next params worldPath
            else do
              writeIORef negotiatedVersionRef requestedVersion
              sendSDKEnvelope limits transport RPCEnvelope
                { envType = MsgHandshakeAck
                , envPayload = Aeson.toJSON ack
                , envRequestId = envRequestId envelope
                }
              next params newWorldPath

      MsgWorldChanged -> case Aeson.fromJSON (envPayload envelope) of
        Aeson.Error _ -> next params worldPath
        Aeson.Success (wc :: WorldChanged) ->
          next params (fmap Text.unpack (wchWorldPath wc))

      MsgQueryResource -> case Aeson.fromJSON (envPayload envelope) of
        Aeson.Error err -> do
          sendDataResourceErrorResponse limits transport (envRequestId envelope)
            SchemaValidationFailed ("Invalid query payload: " <> Text.pack err)
          next params worldPath
        Aeson.Success (qr :: QueryResource) ->
          case findHandler (qrResource qr) (pdDataResources pd) of
            Nothing -> do
              sendDataResourceErrorResponse limits transport (envRequestId envelope)
                ResourceNotFound ("Unknown resource: " <> qrResource qr)
              next params worldPath
            Just drd -> case validateQueryResourceRequest (drdSchema drd) qr of
              Just failure -> do
                sendDataResourceFailureResponse limits transport (envRequestId envelope) failure
                next params worldPath
              Nothing -> case (Map.lookup (qrResource qr) (pdScopedDataHandlers pd) >>= sdhQuery,
                  dhQuery (drdHandler drd)) of
                (Nothing, Nothing) -> do
                  sendDataResourceErrorResponse limits transport (envRequestId envelope)
                    OperationNotSupported ("Resource '" <> qrResource qr <> "' does not support queries")
                  next params worldPath
                (scopedHandler, legacyHandler) -> do
                  runResult <- case scopedHandler of
                    Just handler -> catchPluginResult (handler
                      (makeScopedDataQueryContext limits params worldPath transport
                        (envRequestId envelope) qr) (qrQuery qr))
                    Nothing -> catchPluginResult (maybe
                      (pure (Left "query callback is unavailable"))
                      (\handler -> handler
                        (makeDataContext limits params worldPath transport (envRequestId envelope))
                        (qrQuery qr)) legacyHandler)
                  case runResult of
                    Left errMsg -> sendDataResourceFailureResponse limits transport
                      (envRequestId envelope) (dataResourceFailureFromText errMsg)
                    Right queryResult -> case validateQueryResult (drdSchema drd) qr queryResult of
                      Just failure -> sendDataResourceFailureResponse limits transport
                        (envRequestId envelope) failure
                      Nothing -> sendSDKEnvelope limits transport RPCEnvelope
                        { envType = MsgQueryResult
                        , envPayload = Aeson.toJSON queryResult
                        , envRequestId = envRequestId envelope
                        }
                  next params worldPath

      MsgMutateResource -> case Aeson.fromJSON (envPayload envelope) of
        Aeson.Error err -> do
          sendDataResourceErrorResponse limits transport (envRequestId envelope)
            SchemaValidationFailed ("Invalid mutate payload: " <> Text.pack err)
          next params worldPath
        Aeson.Success (mr :: MutateResource) ->
          case findHandler (mrResource mr) (pdDataResources pd) of
            Nothing -> do
              sendDataResourceErrorResponse limits transport (envRequestId envelope)
                ResourceNotFound ("Unknown resource: " <> mrResource mr)
              next params worldPath
            Just drd -> case mutationSupportError (drdSchema drd) (mrMutation mr) of
              Just errMsg -> do
                sendDataResourceErrorResponse limits transport (envRequestId envelope)
                  OperationNotSupported errMsg
                next params worldPath
              Nothing -> case (Map.lookup (mrResource mr) (pdScopedDataHandlers pd) >>= sdhMutate,
                  dhMutate (drdHandler drd)) of
                (Nothing, Nothing) -> do
                  sendDataResourceErrorResponse limits transport (envRequestId envelope)
                    OperationNotSupported ("Resource '" <> mrResource mr <> "' does not support mutations")
                  next params worldPath
                (scopedHandler, legacyHandler) -> do
                  runResult <- case scopedHandler of
                    Just handler -> catchPluginResult (handler
                      (makeScopedDataMutationContext limits params worldPath transport
                        (envRequestId envelope) mr) (mrMutation mr))
                    Nothing -> catchPluginResult (maybe
                      (pure (Left "mutation callback is unavailable"))
                      (\handler -> handler
                        (makeDataContext limits params worldPath transport (envRequestId envelope))
                        (mrMutation mr)) legacyHandler)
                  case runResult of
                    Left errMsg -> sendDataResourceFailureResponse limits transport
                      (envRequestId envelope) (dataResourceFailureFromText errMsg)
                    Right mutationResult -> sendSDKEnvelope limits transport RPCEnvelope
                      { envType = MsgMutateResult
                      , envPayload = Aeson.toJSON mutationResult
                      , envRequestId = envRequestId envelope
                      }
                  next params worldPath

      MsgInvokeGenerator -> case Aeson.fromJSON (envPayload envelope) of
        Aeson.Error err -> do
          sendErrorResponse limits transport (envRequestId envelope) 6
            ("Invalid invoke_generator payload: " <> Text.pack err)
          next params worldPath
        Aeson.Success (ig :: InvokeGenerator) -> do
          negotiatedVersion <- readIORef negotiatedVersionRef
          let mergedParams = Map.union (igConfig ig) params
              logFn = sendLogMessage limits transport (envRequestId envelope)
              progressFn = sendProgress limits transport (envRequestId envelope)
          preserveStateOnRejected mergedParams worldPath $
            if negotiatedVersion <= currentProtocolVersion
              then case pdGenerator pd of
                Nothing -> rejectInvocation 2
                  (if isJust (pdGeneratorScope pd)
                    then "protocol 4 generator invocation requires the legacy adapter"
                    else "Plugin has no generator") mergedParams
                Just legacy -> case makeTerrainContext limits mergedParams (igTerrain ig) Nothing Map.empty
                    (igSeed ig) worldPath logFn progressFn of
                  Left err -> rejectInvocation 6 ("Invalid terrain payload: " <> err) mergedParams
                  Right ctx -> do
                    runResult <- catchPluginResult (gdRun legacy ctx)
                    either (sendErrorResponse limits transport (envRequestId envelope) 3)
                      (sendGeneratorResult limits transport (envRequestId envelope)) runResult
                    next mergedParams worldPath
              else case igInvocationScope ig of
                Just binding -> case pdGeneratorScope pd of
                  Nothing -> rejectInvocation 6 "generator scope is unknown to this plugin" mergedParams
                  Just scoped -> case prepareGeneratorContext limits pd scoped binding mergedParams ig worldPath logFn progressFn of
                    Left err -> rejectInvocation 6 err mergedParams
                    Right (ctx, validationWorld) -> do
                      runResult <- catchPluginResult (gsdRun scoped ctx)
                      case runResult >>= validateGeneratorResult limits (gcScope ctx) validationWorld of
                        Left errMsg -> sendErrorResponse limits transport (envRequestId envelope) 3 errMsg
                        Right generatorResult -> sendGeneratorResult limits transport
                          (envRequestId envelope) generatorResult
                      next mergedParams worldPath
                Nothing -> rejectInvocation 6
                  "protocol 5 generator invocation is missing its scope binding" mergedParams
          where
            rejectInvocation code err nextParams = do
              sendErrorResponse limits transport (envRequestId envelope) code err
              next nextParams worldPath

      MsgInvokeSimulation -> case Aeson.fromJSON (envPayload envelope) of
        Aeson.Error err -> do
          sendErrorResponse limits transport (envRequestId envelope) 7
            ("Invalid invoke_simulation payload: " <> Text.pack err)
          next params worldPath
        Aeson.Success (is' :: InvokeSimulation) -> do
          negotiatedVersion <- readIORef negotiatedVersionRef
          let mergedParams = Map.union (isConfig is') params
              logFn = sendLogMessage limits transport (envRequestId envelope)
              progressFn = sendProgress limits transport (envRequestId envelope)
          preserveStateOnRejected mergedParams worldPath $
            if negotiatedVersion <= currentProtocolVersion
              then case pdSimulation pd of
                Nothing -> rejectInvocation 4
                  (if isJust (pdSimulationScope pd)
                    then "protocol 4 simulation invocation requires the legacy adapter"
                    else "Plugin has no simulation") mergedParams
                Just legacy -> case makeTerrainContext limits mergedParams (isTerrain is') (Just (isOwnOverlay is'))
                    (valueObjectToMap (isOverlays is')) 0 worldPath logFn progressFn of
                  Left err -> rejectInvocation 7 ("Invalid terrain payload: " <> err) mergedParams
                  Right ctx -> do
                    runResult <- catchPluginResult (sdTick legacy ctx)
                    either (sendErrorResponse limits transport (envRequestId envelope) 5)
                      (sendSimulationResult limits transport (envRequestId envelope)) runResult
                    next mergedParams worldPath
              else case isInvocationScope is' of
                Just binding -> case pdSimulationScope pd of
                  Nothing -> rejectInvocation 7 "simulation scope is unknown to this plugin" mergedParams
                  Just scoped -> case prepareSimulationContext limits pd scoped binding mergedParams is' worldPath logFn progressFn of
                    Left err -> rejectInvocation 7 err mergedParams
                    Right (ctx, validationWorld) -> do
                      runResult <- catchPluginResult (ssdTick scoped ctx)
                      case runResult >>= validateSimulationResult limits pd (scScope ctx) validationWorld of
                        Left errMsg -> sendErrorResponse limits transport (envRequestId envelope) 5 errMsg
                        Right simulationResult -> sendSimulationResult limits transport
                          (envRequestId envelope) simulationResult
                      next mergedParams worldPath
                Nothing -> rejectInvocation 7
                  "protocol 5 simulation invocation is missing its scope binding" mergedParams
          where
            rejectInvocation code err nextParams = do
              sendErrorResponse limits transport (envRequestId envelope) code err
              next nextParams worldPath

      MsgHeartbeat -> do
        sendSDKEnvelope limits transport RPCEnvelope
          { envType = MsgHeartbeat
          , envPayload = Aeson.toJSON (Heartbeat { hbStatus = "ok" })
          , envRequestId = envRequestId envelope
          }
        next params worldPath

      MsgHealthCheck -> do
        sendSDKEnvelope limits transport RPCEnvelope
          { envType = MsgHealthStatus
          , envPayload = Aeson.toJSON (HealthStatus True "ok")
          , envRequestId = envRequestId envelope
          }
        next params worldPath

      MsgExternalDataSourceStatusRequest -> case Aeson.fromJSON (envPayload envelope) of
        Aeson.Error err -> do
          sendErrorResponseIfCorrelated limits transport envelope 10
            ("Invalid external data-source status payload: " <> Text.pack err)
          next params worldPath
        Aeson.Success (request :: RPCExternalDataSourceStatusRequest) -> do
          sendResponseIfCorrelated limits transport envelope RPCEnvelope
            { envType = MsgExternalDataSourceStatus
            , envPayload = Aeson.toJSON
                (externalDataSourceStatusReportFromManifest (generateManifest pd) request)
            , envRequestId = envRequestId envelope
            }
          next params worldPath

      MsgExternalDataSourceGrant -> case Aeson.fromJSON (envPayload envelope) of
        Aeson.Error err -> do
          sendErrorResponseIfCorrelated limits transport envelope 11
            ("Invalid external data-source grant payload: " <> Text.pack err)
          next params worldPath
        Aeson.Success (grant :: RPCExternalDataSourceGrantMessage) -> do
          cachedResult <- case envRequestId envelope of
            Nothing -> pure Nothing
            Just _ -> lookupExternalOperationResult pd ExternalDataSourceGrantOperation
              (redsgmOperationId grant)
          case cachedResult of
            Just value -> sendCachedExternalDataSourceOperationResult limits transport envelope value
            Nothing -> do
              handlerResult <- runExternalDataSourceGrantHandler pd grant
              operationResult <- case handlerResult of
                Left err -> sendExternalDataSourceGrantFailure limits transport envelope pd grant err
                Right () -> sendExternalDataSourceGrantSuccess limits transport envelope pd grant
              mapM_ (cacheExternalOperationResult pd) operationResult
          next params worldPath

      MsgExternalDataSourceRevoke -> case Aeson.fromJSON (envPayload envelope) of
        Aeson.Error err -> do
          sendErrorResponseIfCorrelated limits transport envelope 12
            ("Invalid external data-source revocation payload: " <> Text.pack err)
          next params worldPath
        Aeson.Success (revocation :: RPCExternalDataSourceGrantRevocation) -> do
          cachedResult <- case envRequestId envelope of
            Nothing -> pure Nothing
            Just _ -> lookupExternalOperationResult pd ExternalDataSourceRevokeOperation
              (redsrvOperationId revocation)
          case cachedResult of
            Just value -> sendCachedExternalDataSourceOperationResult limits transport envelope value
            Nothing -> do
              handlerResult <- runExternalDataSourceRevocationHandler pd revocation
              operationResult <- case handlerResult of
                Left err -> sendExternalDataSourceRevocationFailure limits transport envelope pd revocation err
                Right () -> sendExternalDataSourceRevocationSuccess limits transport envelope pd revocation
              mapM_ (cacheExternalOperationResult pd) operationResult
          next params worldPath

      _ -> next params worldPath

catchPluginResult :: IO (Either Text a) -> IO (Either Text a)
catchPluginResult action = catch action $ \err ->
  case fromException err :: Maybe SDKSessionError of
    Just transportFailure -> throwIO transportFailure
    Nothing -> pure (Left (Text.pack (show (err :: SomeException))))

------------------------------------------------------------------------
-- External data-source callbacks
------------------------------------------------------------------------

runExternalDataSourceGrantHandler :: PluginDef -> RPCExternalDataSourceGrantMessage -> IO (Either Text ())
runExternalDataSourceGrantHandler pd grant =
  case pdOnExternalDataSourceGrant pd of
    Nothing -> pure (Right ())
    Just handler -> catch
      (handler grant >> pure (Right ()))
      (\e -> pure (Left ("external data-source grant handler failed: " <> Text.pack (show (e :: SomeException)))))

runExternalDataSourceRevocationHandler :: PluginDef -> RPCExternalDataSourceGrantRevocation -> IO (Either Text ())
runExternalDataSourceRevocationHandler pd revocation =
  case pdOnExternalDataSourceRevocation pd of
    Nothing -> pure (Right ())
    Just handler -> catch
      (handler revocation >> pure (Right ()))
      (\e -> pure (Left ("external data-source revocation handler failed: " <> Text.pack (show (e :: SomeException)))))

------------------------------------------------------------------------
-- External data-source ACK helpers
------------------------------------------------------------------------

sendExternalDataSourceGrantSuccess :: RPCPayloadLimits -> Transport -> RPCEnvelope -> PluginDef -> RPCExternalDataSourceGrantMessage -> IO (Maybe RPCExternalDataSourceOperationResult)
sendExternalDataSourceGrantSuccess limits transport request pd grant =
  case (envRequestId request, redsgmOperationId grant) of
    (Nothing, _) -> pure Nothing
    (Just requestId, Nothing) -> do
      sendErrorResponse limits transport (Just requestId) 11 "External data-source grant payload is missing operationId"
      pure Nothing
    (Just requestId, Just operationId) -> do
      let result = RPCExternalDataSourceOperationResult
            { redsoOperationId = operationId
            , redsoOperationEpoch = redsgmOperationEpoch grant
            , redsoOperation = ExternalDataSourceGrantOperation
            , redsoProviderId = redsgmProviderId grant
            , redsoConsumerId = externalGrantConsumerId pd grant
            , redsoSource = redsgmSource grant
            , redsoGrant = redsgmGrant grant
            , redsoAccepted = True
            , redsoApplied = True
            , redsoStatus = "applied"
            , redsoMessage = Just "external data-source grant applied"
            , redsoError = Nothing
            , redsoDiagnostics = Nothing
            }
      sendExternalDataSourceOperationResult limits transport (Just requestId) result
      pure (Just result)

sendExternalDataSourceGrantFailure :: RPCPayloadLimits -> Transport -> RPCEnvelope -> PluginDef -> RPCExternalDataSourceGrantMessage -> Text -> IO (Maybe RPCExternalDataSourceOperationResult)
sendExternalDataSourceGrantFailure limits transport request pd grant err =
  case (envRequestId request, redsgmOperationId grant) of
    (Nothing, _) -> pure Nothing
    (Just requestId, Nothing) -> do
      sendErrorResponse limits transport (Just requestId) 13 err
      pure Nothing
    (Just requestId, Just operationId) -> do
      let result = RPCExternalDataSourceOperationResult
            { redsoOperationId = operationId
            , redsoOperationEpoch = redsgmOperationEpoch grant
            , redsoOperation = ExternalDataSourceGrantOperation
            , redsoProviderId = redsgmProviderId grant
            , redsoConsumerId = externalGrantConsumerId pd grant
            , redsoSource = redsgmSource grant
            , redsoGrant = redsgmGrant grant
            , redsoAccepted = False
            , redsoApplied = False
            , redsoStatus = "failed"
            , redsoMessage = Nothing
            , redsoError = Just err
            , redsoDiagnostics = Nothing
            }
      sendExternalDataSourceOperationResult limits transport (Just requestId) result
      pure (Just result)

sendExternalDataSourceRevocationSuccess :: RPCPayloadLimits -> Transport -> RPCEnvelope -> PluginDef -> RPCExternalDataSourceGrantRevocation -> IO (Maybe RPCExternalDataSourceOperationResult)
sendExternalDataSourceRevocationSuccess limits transport request pd revocation =
  case (envRequestId request, redsrvOperationId revocation) of
    (Nothing, _) -> pure Nothing
    (Just requestId, Nothing) -> do
      sendErrorResponse limits transport (Just requestId) 12 "External data-source revocation payload is missing operationId"
      pure Nothing
    (Just requestId, Just operationId) -> do
      let result = RPCExternalDataSourceOperationResult
            { redsoOperationId = operationId
            , redsoOperationEpoch = redsrvOperationEpoch revocation
            , redsoOperation = ExternalDataSourceRevokeOperation
            , redsoProviderId = redsrvProviderId revocation
            , redsoConsumerId = externalRevocationConsumerId pd revocation
            , redsoSource = redsrvSource revocation
            , redsoGrant = redsrvGrant revocation
            , redsoAccepted = True
            , redsoApplied = True
            , redsoStatus = "applied"
            , redsoMessage = Just "external data-source revocation applied"
            , redsoError = Nothing
            , redsoDiagnostics = Nothing
            }
      sendExternalDataSourceOperationResult limits transport (Just requestId) result
      pure (Just result)

sendExternalDataSourceRevocationFailure :: RPCPayloadLimits -> Transport -> RPCEnvelope -> PluginDef -> RPCExternalDataSourceGrantRevocation -> Text -> IO (Maybe RPCExternalDataSourceOperationResult)
sendExternalDataSourceRevocationFailure limits transport request pd revocation err =
  case (envRequestId request, redsrvOperationId revocation) of
    (Nothing, _) -> pure Nothing
    (Just requestId, Nothing) -> do
      sendErrorResponse limits transport (Just requestId) 14 err
      pure Nothing
    (Just requestId, Just operationId) -> do
      let result = RPCExternalDataSourceOperationResult
            { redsoOperationId = operationId
            , redsoOperationEpoch = redsrvOperationEpoch revocation
            , redsoOperation = ExternalDataSourceRevokeOperation
            , redsoProviderId = redsrvProviderId revocation
            , redsoConsumerId = externalRevocationConsumerId pd revocation
            , redsoSource = redsrvSource revocation
            , redsoGrant = redsrvGrant revocation
            , redsoAccepted = False
            , redsoApplied = False
            , redsoStatus = "failed"
            , redsoMessage = Nothing
            , redsoError = Just err
            , redsoDiagnostics = Nothing
            }
      sendExternalDataSourceOperationResult limits transport (Just requestId) result
      pure (Just result)

externalGrantConsumerId :: PluginDef -> RPCExternalDataSourceGrantMessage -> Text
externalGrantConsumerId pd grant = case redsgmConsumerId grant of
  Just consumerId -> consumerId
  Nothing -> pdName pd

externalRevocationConsumerId :: PluginDef -> RPCExternalDataSourceGrantRevocation -> Text
externalRevocationConsumerId pd revocation = case redsrvConsumerId revocation of
  Just consumerId -> consumerId
  Nothing -> pdName pd

sendCachedExternalDataSourceOperationResult :: RPCPayloadLimits -> Transport -> RPCEnvelope -> RPCExternalDataSourceOperationResult -> IO ()
sendCachedExternalDataSourceOperationResult limits transport request result =
  case envRequestId request of
    Nothing -> pure ()
    Just requestId -> sendExternalDataSourceOperationResult limits transport (Just requestId) result

sendExternalDataSourceOperationResult :: RPCPayloadLimits -> Transport -> Maybe Word64 -> RPCExternalDataSourceOperationResult -> IO ()
sendExternalDataSourceOperationResult limits transport requestId result =
  sendSDKEnvelope limits transport RPCEnvelope
    { envType = MsgExternalDataSourceOperationResult
    , envPayload = Aeson.toJSON result
    , envRequestId = requestId
    }

------------------------------------------------------------------------
-- Response helpers
------------------------------------------------------------------------

-- All SDK writes pass through this function. An oversized response fails before
-- any bytes are written and the typed exception terminates the session.
sendSDKEnvelope :: RPCPayloadLimits -> Transport -> RPCEnvelope -> IO ()
sendSDKEnvelope limits transport envelope = do
  let encoded = encodeMessageLazy envelope
      actual = fromIntegral (BL.length encoded)
      limitInt = fromIntegral (rplMaxFrameSizeBytes limits)
      limit = toInteger limitInt
  result <- sendLazyMessageWithLimit limitInt transport encoded
  case result of
    Right () -> pure ()
    Left (TransportFramingError _)
      | Just requestId <- envRequestId envelope -> do
          let message = "outgoing RPC payload exceeds configured limit: type="
                <> Text.pack (show (envType envelope))
                <> ", actual=" <> Text.pack (show actual)
                <> " bytes, limit=" <> Text.pack (show limit) <> " bytes"
              compactError = RPCEnvelope
                { envType = MsgError
                , envPayload = object
                    [ "code" .= (15 :: Int)
                    , "message" .= message
                    ]
                , envRequestId = Just requestId
                }
              compactBytes = encodeMessageLazy compactError
              compactLength = fromIntegral (BL.length compactBytes)
          compactResult <- sendLazyMessageWithLimit limitInt transport compactBytes
          case compactResult of
            Right () -> throwIO
              (SDKRequestPayloadRejected (envType envelope) requestId actual limit)
            Left compactErr -> throwIO
              (SDKSendFailure MsgError (Just requestId) compactLength limit compactErr)
    Left err -> throwIO (SDKSendFailure (envType envelope) (envRequestId envelope) actual limit err)

sendResponseIfCorrelated :: RPCPayloadLimits -> Transport -> RPCEnvelope -> RPCEnvelope -> IO ()
sendResponseIfCorrelated limits transport request response =
  case envRequestId request of
    Nothing -> pure ()
    Just _ -> sendSDKEnvelope limits transport response

sendErrorResponseIfCorrelated :: RPCPayloadLimits -> Transport -> RPCEnvelope -> Int -> Text -> IO ()
sendErrorResponseIfCorrelated limits transport request code msg =
  case envRequestId request of
    Nothing -> pure ()
    Just requestId -> sendErrorResponse limits transport (Just requestId) code msg

-- | Send an error response.
sendErrorResponse :: RPCPayloadLimits -> Transport -> Maybe Word64 -> Int -> Text -> IO ()
sendErrorResponse limits transport requestId code msg =
  sendErrorResponseWithDataResourceCode limits transport requestId code msg Nothing

sendDataResourceErrorResponse :: RPCPayloadLimits -> Transport -> Maybe Word64 -> DataResourceErrorCode -> Text -> IO ()
sendDataResourceErrorResponse limits transport requestId code msg =
  sendDataResourceFailureResponse limits transport requestId (DataResourceFailure code msg)

sendDataResourceFailureResponse :: RPCPayloadLimits -> Transport -> Maybe Word64 -> DataResourceFailure -> IO ()
sendDataResourceFailureResponse limits transport requestId failure =
  sendErrorResponseWithDataResourceCode
    limits
    transport
    requestId
    (dataResourceErrorRPCCode (drfCode failure))
    (drfMessage failure)
    (Just (drfCode failure))

sendErrorResponseWithDataResourceCode :: RPCPayloadLimits -> Transport -> Maybe Word64 -> Int -> Text -> Maybe DataResourceErrorCode -> IO ()
sendErrorResponseWithDataResourceCode limits transport requestId code msg mDataResourceCode =
  sendSDKEnvelope limits transport RPCEnvelope
    { envType = MsgError
    , envPayload = object $
        [ "code" .= code
        , "message" .= msg
        ] <>
        [ "data_resource_error" .= dataResourceErrorCodeText dataCode
        | Just dataCode <- [mDataResourceCode]
        ]
    , envRequestId = requestId
    }

-- | Send a generator result payload.
sendGeneratorResult :: RPCPayloadLimits -> Transport -> Maybe Word64 -> GeneratorTickResult -> IO ()
sendGeneratorResult limits transport requestId result =
  sendSDKEnvelope limits transport RPCEnvelope
    { envType = MsgGeneratorResult
    , envPayload = Aeson.toJSON GeneratorResult
        { grTerrain = gtrTerrain result
        , grOverlay = gtrOverlay result
        , grMetadata = gtrMetadata result
        }
    , envRequestId = requestId
    }

-- | Send a simulation result payload.
sendSimulationResult :: RPCPayloadLimits -> Transport -> Maybe Word64 -> SimulationTickResult -> IO ()
sendSimulationResult limits transport requestId result =
  sendSDKEnvelope limits transport RPCEnvelope
    { envType = MsgSimulationResult
    , envPayload = Aeson.toJSON SimulationResult
        { srOverlay = strOverlay result
        , srTerrainWrites = strTerrainWrites result
        }
    , envRequestId = requestId
    }

valueObjectToMap :: Value -> Map Text Value
valueObjectToMap (Object keyMap) =
  Map.fromList
    [ (Key.toText key, value)
    | (key, value) <- KM.toList keyMap
    ]
valueObjectToMap _ = Map.empty

prepareGeneratorContext
  :: RPCPayloadLimits -> PluginDef -> GeneratorScopeDef
  -> RPCInvocationScopeBinding -> Map Text Value -> InvokeGenerator
  -> Maybe FilePath -> (Text -> IO ()) -> (Text -> Double -> IO ())
  -> Either Text (GeneratorContext, TerrainWorld)
prepareGeneratorContext limits pd definition binding params invocation worldPath logFn progressFn = do
  scope <- validateHostScope pd InvocationGenerator (gsdScope definition) binding
  validateTerrainInput scope (igTerrain invocation)
  validationWorld <- decodeTerrainPayloadWithLimits (scopeTerrainLimits limits scope)
    (igTerrain invocation)
  let terrainAvailable = not (Set.null (risTerrainInputSections scope))
      context = GeneratorContext
        { gcParams = params
        , gcTerrain = if terrainAvailable then Just validationWorld else Nothing
        , gcTerrainPayload = if terrainAvailable then Just (igTerrain invocation) else Nothing
        , gcSeed = igSeed invocation
        , gcScope = scope
        , gcLog = logFn
        , gcProgress = progressFn
        , gcWorldPath = worldPath
        }
  Right (context, validationWorld)

prepareSimulationContext
  :: RPCPayloadLimits -> PluginDef -> SimulationScopeDef
  -> RPCInvocationScopeBinding -> Map Text Value -> InvokeSimulation
  -> Maybe FilePath -> (Text -> IO ()) -> (Text -> Double -> IO ())
  -> Either Text (SimulationContext, TerrainWorld)
prepareSimulationContext limits pd definition binding params invocation worldPath logFn progressFn = do
  scope <- validateHostScope pd InvocationSimulation (ssdScope definition) binding
  validateOverlayInputBudget scope (isOverlays invocation) (isOwnOverlay invocation)
  validateTerrainInput scope (isTerrain invocation)
  validationWorld <- decodeTerrainPayloadWithLimits (scopeTerrainLimits limits scope)
    (isTerrain invocation)
  suppliedOverlays <- case isOverlays invocation of
    Object _ -> Right (valueObjectToMap (isOverlays invocation))
    _ -> Left "dependency overlay collection must be an object"
  let grants = Map.keysSet (risDependencyOverlayChunkIds scope)
      suppliedNames = Map.keysSet suppliedOverlays
  if suppliedNames == grants
    then Right ()
    else Left ("host dependency overlay names do not exactly match the resolved scope: supplied="
      <> Text.intercalate "," (Set.toAscList suppliedNames)
      <> ", granted=" <> Text.intercalate "," (Set.toAscList grants))
  mapM_ (\(name, value) -> case Map.lookup name (risDependencyOverlayChunkIds scope) of
      Nothing -> Left ("dependency overlay is outside resolved scope: " <> name)
      Just chunks -> validateOverlayPayloadChunks ("dependency overlay " <> name) chunks value)
    (Map.toList suppliedOverlays)
  let ownAvailable = risOwnedOverlayIdentity scope /= Nothing
  if ownAvailable
    then validateOverlayPayloadChunks "own overlay input"
      (risOwnOverlayReadChunkIds scope) (isOwnOverlay invocation)
    else case isOwnOverlay invocation of
      Null -> Right ()
      _ -> Left "host supplied own overlay without a resolved read grant"
  let terrainAvailable = not (Set.null (risTerrainInputSections scope))
      context = SimulationContext
        { scParams = params
        , scTerrain = if terrainAvailable then Just validationWorld else Nothing
        , scTerrainPayload = if terrainAvailable then Just (isTerrain invocation) else Nothing
        , scOwnOverlay = if ownAvailable then Just (isOwnOverlay invocation) else Nothing
        , scOverlays = Map.restrictKeys suppliedOverlays grants
        , scWorldTime = isWorldTime invocation
        , scDeltaTicks = isDeltaTicks invocation
        , scCalendar = isCalendar invocation
        , scScope = scope
        , scLog = logFn
        , scProgress = progressFn
        , scWorldPath = worldPath
        }
  Right (context, validationWorld)

validateHostScope
  :: PluginDef -> RPCInvocationKind -> RPCInvocationScopeDecl
  -> RPCInvocationScopeBinding -> Either Text ResolvedInvocationScope
validateHostScope pd expectedKind declaration binding = do
  descriptor <- maybe (Left "protocol 5 invocation scope must include its descriptor") Right
    (risbDescriptor binding)
  mapScopeError (validateInvocationScopeBinding maximumSupportedProtocolVersion
    (Just descriptor) (Just binding))
  if risKind descriptor == expectedKind
    then Right ()
    else Left "resolved invocation scope kind does not match the callback"
  validateResolvedNarrowing pd (sdkCapabilities pd) declaration descriptor
  Right descriptor

mapScopeError :: Either ScopeError a -> Either Text a
mapScopeError = either (Left . (\err -> sePath err <> ": " <> seMessage err)) Right

validateResolvedNarrowing
  :: PluginDef -> [RPCCapability] -> RPCInvocationScopeDecl -> ResolvedInvocationScope
  -> Either Text ()
validateResolvedNarrowing pd capabilities declaration scope = do
  subset "terrain input sections" (risTerrainInputSections scope)
    (Set.fromList (rsiTerrainSections input))
  subset "terrain output sections" (risTerrainOutputSections scope)
    (Set.fromList (rsoTerrainSections output))
  subset "dependency overlays" (Map.keysSet (risDependencyOverlayChunkIds scope))
    (Set.fromList (rsiDependencyOverlays input))
  require (rsiOwnOverlay input || IntSet.null (risOwnOverlayReadChunkIds scope))
    "resolved scope widened own-overlay input"
  require (rsoOwnedOverlay output || risOwnedOverlayIdentity scope == Nothing)
    "resolved scope widened owned-overlay output"
  require (risOwnedOverlayIdentity scope == Nothing
      || risOwnedOverlayIdentity scope == Just (pdName pd))
    "resolved scope names an owned overlay belonging to another plugin"
  require (risOwnedOverlayIdentity scope /= Nothing
      || IntSet.null (risOwnOverlayWriteChunkIds scope))
    "resolved scope grants own-overlay write chunks without an owned overlay"
  require (risDataResource scope == Nothing)
    "generator/simulation scope contains data-resource authority"
  require (rsoGeneratorMetadata output || not (risGeneratorMetadataOutput scope))
    "resolved scope widened generator metadata output"
  require (budgetLE (risBudgets scope) (risdBudgets declaration))
    "resolved scope widened declared budgets"
  require (hasAny [CapReadTerrain, CapReadWorld] || Set.null (risTerrainInputSections scope))
    "resolved scope grants terrain input without a capability"
  require (hasAny [CapReadOverlay, CapReadWorld]
      || Map.null (risDependencyOverlayChunkIds scope))
    "resolved scope grants dependency overlays without a capability"
  require (hasAny [CapWriteOverlay, CapWriteWorld]
      || risOwnedOverlayIdentity scope == Nothing)
    "resolved scope grants overlay output without a capability"
  require (risKind scope == InvocationGenerator
      || hasAny [CapWriteTerrain, CapWriteWorld]
      || Set.null (risTerrainOutputSections scope))
    "resolved scope grants simulation terrain output without a capability"
  validateResolvedSelector "terrain input" (rsiChunkSelector input)
    (risTerrainInputSections scope) (risTerrainInputChunkIds scope) scope
  validateResolvedSelector "terrain output" (rsoChunkSelector output)
    (risTerrainOutputSections scope) (risTerrainOutputChunkIds scope) scope
  where
    input = risdInput declaration
    output = risdOutput declaration
    hasAny requested = any (`elem` capabilities) requested
    subset label actual allowed = require (actual `Set.isSubsetOf` allowed)
      ("resolved scope widened " <> label)
    require True _ = Right ()
    require False message = Left message
    budgetLE actual allowed =
      rsbTerrainBytes actual <= rsbTerrainBytes allowed
      && rsbOverlayBytes actual <= rsbOverlayBytes allowed
      && rsbOutputBytes actual <= rsbOutputBytes allowed

validateResolvedSelector
  :: Text -> RPCChunkSelector -> Set TerrainSection -> IntSet
  -> ResolvedInvocationScope -> Either Text ()
validateResolvedSelector _ _ sections _ _ | Set.null sections = Right ()
validateResolvedSelector _ SelectAllInvocationChunks _ _ _ = Right ()
validateResolvedSelector label SelectCallerChunks _ _ _ =
  Left (label <> " uses an unavailable caller chunk selector")
validateResolvedSelector label selector _ actual scope = do
  named <- case selector of
    SelectOverlayUnion names -> traverse chunksFor names
    SelectOverlayIntersection names -> traverse chunksFor names
  let expected = case selector of
        SelectOverlayUnion _ -> IntSet.unions named
        SelectOverlayIntersection _ -> case named of
          first:rest -> foldl IntSet.intersection first rest
          [] -> IntSet.empty
  if actual == expected
    then Right ()
    else Left (label <> " chunks do not match the declared overlay selector")
  where
    chunksFor "$own" = Right (risOwnOverlayReadChunkIds scope)
    chunksFor name = maybe
      (Left (label <> " selector references an overlay absent from the resolved scope: " <> name))
      Right (Map.lookup name (risDependencyOverlayChunkIds scope))

scopeTerrainLimits :: RPCPayloadLimits -> ResolvedInvocationScope -> RPCPayloadLimits
scopeTerrainLimits fallback scope =
  let budget = rsbTerrainBytes (risBudgets scope)
      bounded = fromInteger (min (toInteger budget) (toInteger (maxBound :: Int)))
  in case mkRPCPayloadLimits (max 1 bounded) of
      Left _ -> fallback
      Right scoped -> scoped

validateOverlayInputBudget :: ResolvedInvocationScope -> Value -> Value -> Either Text ()
validateOverlayInputBudget scope dependencies ownOverlay =
  let payload = object ["dependencies" .= dependencies, "own" .= ownOverlay]
      actual = toInteger (BL.length (Aeson.encode payload))
      budget = rsbOverlayBytes (risBudgets scope)
  in if actual <= toInteger budget
      then Right ()
      else Left ("overlay input exceeds resolved budget: actual="
        <> Text.pack (show actual) <> ", limit=" <> Text.pack (show budget))

validateTerrainInput :: ResolvedInvocationScope -> Value -> Either Text ()
validateTerrainInput scope Null
  | Set.null (risTerrainInputSections scope) = Right ()
  | otherwise = Left "host omitted granted terrain input"
validateTerrainInput scope (Object payload)
  | Set.null allowedSections =
      if KM.null payload then Right ()
      else Left "host supplied terrain fields when the resolved scope grants no terrain"
  | otherwise = do
      let allowedKeys = Set.fromList
            (["chunk_size", "hex_grid", "planet", "slice", "encoding"]
              <> concatMap sectionKeys (Set.toAscList allowedSections))
          extras = Set.fromList (map Key.toText (KM.keys payload)) `Set.difference` allowedKeys
      if Set.null extras then Right () else Left
        ("host supplied terrain keys outside resolved scope: "
          <> Text.intercalate ", " (Set.toAscList extras))
      mapM_ validateGrantedSection (Set.toAscList allowedSections)
  where
    allowedSections = risTerrainInputSections scope
    allowedChunks = risTerrainInputChunkIds scope
    sectionKeys TerrainElevation = ["chunk_count", "terrain"]
    sectionKeys TerrainClimate = ["climate_count", "climate"]
    sectionKeys TerrainVegetation = ["vegetation_count", "vegetation"]
    sectionField TerrainElevation = "terrain"
    sectionField TerrainClimate = "climate"
    sectionField TerrainVegetation = "vegetation"
    validateGrantedSection section =
      let field = sectionField section
      in case KM.lookup (Key.fromText field) payload of
          Nothing -> Left ("host omitted granted terrain section: " <> field)
          Just (Object chunks) -> validateChunkKeys field allowedChunks (KM.keys chunks)
          Just _ -> Left (field <> " terrain input must be an object")
validateTerrainInput _ _ = Left "terrain input must be an object"

validateChunkKeys :: Text -> IntSet -> [Key.Key] -> Either Text ()
validateChunkKeys field allowed = go IntSet.empty
  where
    go _ [] = Right ()
    go seen (key:rest) = case reads (Text.unpack (Key.toText key)) of
      [(chunkId, "")]
        | chunkId < (0 :: Int) -> Left ("negative chunk ID in " <> field)
        | IntSet.member chunkId seen -> Left ("duplicate numeric chunk ID in " <> field)
        | IntSet.notMember chunkId allowed -> Left
            (field <> " input contains chunk outside resolved scope: " <> Text.pack (show chunkId))
        | otherwise -> go (IntSet.insert chunkId seen) rest
      _ -> Left ("invalid chunk ID in " <> field <> ": " <> Key.toText key)

validateOverlayPayloadChunks :: Text -> IntSet -> Value -> Either Text ()
validateOverlayPayloadChunks label allowed (Object payload) = do
  requireExactKeys label ["storage", "chunks"] payload
  storage <- case KM.lookup "storage" payload of
    Just (String "sparse") -> Right ("sparse" :: Text)
    Just (String "dense") -> Right "dense"
    _ -> Left (label <> " has an invalid storage tag")
  case KM.lookup "chunks" payload of
    Just (Array chunks) -> validateOverlayChunkList label storage allowed (foldr (:) [] chunks)
    _ -> Left (label <> ".chunks must be an array")
validateOverlayPayloadChunks label _ _ = Left (label <> " must be an object")

validateOverlayChunkList :: Text -> Text -> IntSet -> [Value] -> Either Text ()
validateOverlayChunkList label storage allowed = go IntSet.empty
  where
    go _ [] = Right ()
    go seen (Object chunk:rest) = do
      requireExactKeys (label <> " chunk")
        (if storage == "sparse" then ["chunk_id", "tiles"] else ["chunk_id", "fields"])
        chunk
      chunkId <- requiredNonNegativeInt (label <> " chunk_id") "chunk_id" chunk
      if IntSet.member chunkId seen
        then Left (label <> " contains duplicate chunk IDs")
        else if IntSet.notMember chunkId allowed
          then Left (label <> " contains a chunk outside resolved scope: " <> Text.pack (show chunkId))
          else do
            if storage == "sparse" then validateSparseTiles label chunk else Right ()
            go (IntSet.insert chunkId seen) rest
    go _ (_:_) = Left (label <> " chunks must be objects")

validateSparseTiles :: Text -> KM.KeyMap Value -> Either Text ()
validateSparseTiles label chunk = case KM.lookup "tiles" chunk of
  Just (Array tiles) -> go IntSet.empty (foldr (:) [] tiles)
  _ -> Left (label <> " sparse chunk tiles must be an array")
  where
    go _ [] = Right ()
    go seen (Object tile:rest) = do
      requireExactKeys (label <> " sparse tile") ["tile", "fields"] tile
      tileId <- requiredNonNegativeInt (label <> " tile") "tile" tile
      if IntSet.member tileId seen
        then Left (label <> " contains duplicate tile IDs")
        else go (IntSet.insert tileId seen) rest
    go _ (_:_) = Left (label <> " sparse tiles must be objects")

requiredNonNegativeInt :: Text -> Key.Key -> KM.KeyMap Value -> Either Text Int
requiredNonNegativeInt label key values = case KM.lookup key values of
  Nothing -> Left (label <> " is missing")
  Just raw -> case Aeson.fromJSON raw of
    Aeson.Error _ -> Left (label <> " must be an integer")
    Aeson.Success value
      | value < 0 -> Left (label <> " must be non-negative")
      | otherwise -> Right value

requireExactKeys :: Text -> [Text] -> KM.KeyMap Value -> Either Text ()
requireExactKeys label expected values =
  let actual = Set.fromList (map Key.toText (KM.keys values))
  in if actual == Set.fromList expected
      then Right ()
      else Left (label <> " contains missing or unsupported fields")

validateGeneratorResult
  :: RPCPayloadLimits -> ResolvedInvocationScope -> TerrainWorld
  -> GeneratorTickResult -> Either Text GeneratorTickResult
validateGeneratorResult limits scope world result = do
  validateResolvedOutputBudget scope (Aeson.toJSON GeneratorResult
    { grTerrain = gtrTerrain result
    , grOverlay = gtrOverlay result
    , grMetadata = gtrMetadata result
    })
  _ <- applyGeneratorTerrainValueScopedWithLimits limits scope world (gtrTerrain result)
  case gtrOverlay result of
    Nothing -> Right ()
    Just _ -> Left "scoped generator result emitted an unavailable owned overlay"
  case gtrMetadata result of
    Nothing -> Right ()
    Just _ | risGeneratorMetadataOutput scope -> Right ()
    Just _ -> Left "generator metadata output is not granted"
  Right result

validateSimulationResult
  :: RPCPayloadLimits -> PluginDef -> ResolvedInvocationScope -> TerrainWorld
  -> SimulationTickResult -> Either Text SimulationTickResult
validateSimulationResult limits pd scope world result = do
  validateResolvedOutputBudget scope (Aeson.toJSON SimulationResult
    { srOverlay = strOverlay result
    , srTerrainWrites = strTerrainWrites result
    })
  case risOwnedOverlayIdentity scope of
    Just identity
      | identity /= pdName pd -> Left "resolved owned-overlay identity does not match this plugin"
      | otherwise -> validateOverlayPayloadChunks "simulation owned-overlay output"
          (risOwnOverlayWriteChunkIds scope) (strOverlay result)
    Nothing -> case strOverlay result of
      Object values | KM.null values -> Right ()
      Null -> Right ()
      _ -> Left "simulation result emitted an unavailable owned overlay"
  _ <- decodeTerrainWritesValueScopedWithLimits limits scope world (strTerrainWrites result)
  Right result

validateResolvedOutputBudget :: ResolvedInvocationScope -> Value -> Either Text ()
validateResolvedOutputBudget scope payload =
  let actual = toInteger (BL.length (Aeson.encode payload))
      allowed = toInteger (rsbOutputBytes (risBudgets scope))
  in if actual <= allowed
      then Right ()
      else Left ("callback result exceeds resolved output budget: actual="
        <> Text.pack (show actual) <> ", limit=" <> Text.pack (show allowed))

makeTerrainContext
  :: RPCPayloadLimits
  -> Map Text Value
  -> Value
  -> Maybe Value
  -> Map Text Value
  -> Word64
  -> Maybe FilePath
  -> (Text -> IO ())
  -> (Text -> Double -> IO ())
  -> Either Text PluginContext
makeTerrainContext limits params terrainPayload ownOverlay overlays seed worldPath logFn progressFn = do
  world <- decodeTerrainPayloadWithLimits limits terrainPayload
  Right PluginContext
    { pcWorld = world
    , pcParams = params
    , pcTerrain = terrainPayload
    , pcOwnOverlay = ownOverlay
    , pcOverlays = overlays
    , pcSeed = seed
    , pcLog = logFn
    , pcProgress = progressFn
    , pcWorldPath = worldPath
    }

-- | Send a log message to the host.
sendLogMessage :: RPCPayloadLimits -> Transport -> Maybe Word64 -> Text -> IO ()
sendLogMessage limits transport requestId msg =
  sendSDKEnvelope limits transport RPCEnvelope
    { envType = MsgLog
    , envPayload = object
        [ "level" .= ("info" :: Text)
        , "message" .= msg
        ]
    , envRequestId = requestId
    }

-- | Send progress to the host.
--
-- SDK progress fractions are absolute for the current invocation. Finite
-- values are clamped to [0,1]; non-finite values are mapped defensively before
-- JSON encoding so plugins never emit invalid JSON numbers.
sendProgress :: RPCPayloadLimits -> Transport -> Maybe Word64 -> Text -> Double -> IO ()
sendProgress limits transport requestId msg fraction =
  sendSDKEnvelope limits transport RPCEnvelope
    { envType = MsgProgress
    , envPayload = Aeson.toJSON (PluginProgress msg (sanitizeProgressFraction fraction))
    , envRequestId = requestId
    }

sanitizeProgressFraction :: Double -> Double
sanitizeProgressFraction fraction
  | isNaN fraction = 0
  | isInfinite fraction = if fraction > 0 then 1 else 0
  | fraction < 0 = 0
  | fraction > 1 = 1
  | otherwise = fraction

------------------------------------------------------------------------
-- Data service helpers
------------------------------------------------------------------------

-- | Find a data resource definition by resource name.
findHandler :: Text -> [DataResourceDef] -> Maybe DataResourceDef
findHandler name = foldr go Nothing
  where
    go drd acc
      | drsName (drdSchema drd) == name = Just drd
      | otherwise = acc

mutationSupportError :: DataResourceSchema -> DataMutation -> Maybe Text
mutationSupportError schema mutation = case mutation of
  MutCreate _
    | doCreate ops -> Nothing
    | otherwise -> unsupported "create"
  MutUpdate _ _
    | doUpdate ops -> Nothing
    | otherwise -> unsupported "update"
  MutDelete _
    | doDelete ops -> Nothing
    | otherwise -> unsupported "delete"
  MutSetHex _ _ _
    | drsHexBound schema && (doCreate ops || doUpdate ops) -> Nothing
    | otherwise -> unsupported "set_hex"
  where
    ops = drsOperations schema
    name = drsName schema
    unsupported op = Just ("Resource '" <> name <> "' does not support " <> op <> " mutations")

makeScopedDataQueryContext
  :: RPCPayloadLimits -> Map Text Value -> Maybe FilePath -> Transport
  -> Maybe Word64 -> QueryResource -> DataContext
makeScopedDataQueryContext limits params worldPath transport requestId request = DataContext
  { dcResource = qrResource request
  , dcOperation = queryOperation (qrQuery request)
  , dcPageOffset = qrPageOffset request
  , dcPageSize = qrPageSize request
  , dcQuery = Just (qrQuery request)
  , dcMutation = Nothing
  , dcParams = params
  , dcLog = sendLogMessage limits transport requestId
  , dcProgress = sendProgress limits transport requestId
  , dcWorldPath = worldPath
  }

makeScopedDataMutationContext
  :: RPCPayloadLimits -> Map Text Value -> Maybe FilePath -> Transport
  -> Maybe Word64 -> MutateResource -> DataContext
makeScopedDataMutationContext limits params worldPath transport requestId request = DataContext
  { dcResource = mrResource request
  , dcOperation = mutationOperation (mrMutation request)
  , dcPageOffset = Nothing
  , dcPageSize = Nothing
  , dcQuery = Nothing
  , dcMutation = Just (mrMutation request)
  , dcParams = params
  , dcLog = sendLogMessage limits transport requestId
  , dcProgress = sendProgress limits transport requestId
  , dcWorldPath = worldPath
  }

queryOperation :: DataQuery -> RPCDataOperation
queryOperation QueryAll = DataList
queryOperation (QueryByKey _) = DataGet
queryOperation (QueryByHex _ _) = DataQueryByHex
queryOperation (QueryByField _ _) = DataQueryByField

mutationOperation :: DataMutation -> RPCDataOperation
mutationOperation (MutCreate _) = DataCreate
mutationOperation (MutUpdate _ _) = DataUpdate
mutationOperation (MutDelete _) = DataDelete
mutationOperation (MutSetHex _ _ _) = DataUpdate

-- | Build the explicit legacy data callback adapter context. New plugins
-- should use 'pdScopedDataHandlers', which never fabricates world authority.
makeDataContext :: RPCPayloadLimits -> Map Text Value -> Maybe FilePath -> Transport -> Maybe Word64 -> PluginContext
makeDataContext limits params worldPath transport requestId = PluginContext
  { pcWorld      = stubWorld
  , pcParams     = params
  , pcTerrain    = Object mempty
  , pcOwnOverlay = Nothing
  , pcOverlays   = Map.empty
  , pcSeed       = 0
  , pcLog        = sendLogMessage limits transport requestId
  , pcProgress   = sendProgress limits transport requestId
  , pcWorldPath  = worldPath
  }

------------------------------------------------------------------------
-- Stub world
------------------------------------------------------------------------

-- | Default chunk size used for stub worlds in the SDK runner.
stubChunkSize :: Int
stubChunkSize = 64

-- | A minimal empty world used as placeholder context.
--
-- The host will populate this with actual terrain data via the RPC
-- payload before invoking callbacks.
stubWorld :: TerrainWorld
stubWorld = emptyWorld (WorldConfig { wcChunkSize = stubChunkSize }) defaultHexGridMeta
