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

import Control.Concurrent (forkFinally, forkIO, killThread)
import Control.Concurrent.MVar
  ( MVar, newEmptyMVar, newMVar, putMVar, readMVar, tryTakeMVar, withMVar )
import Control.Concurrent.STM
  ( TChan, TMVar, atomically, newEmptyTMVarIO, newTChanIO, orElse
  , putTMVar, readTChan, readTMVar, writeTChan
  )
import Control.Exception
  ( Exception, SomeException, catch, finally, fromException, mask_, onException, throwIO )
import Control.Monad (foldM)
import Data.Aeson (Value(..), (.=), object)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List (nub)
import Data.Maybe (isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Word (Word32, Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory
  ( createDirectoryIfMissing, doesFileExist, getCurrentDirectory
  , getTemporaryDirectory, removeFile
  )
import System.Environment (getArgs, lookupEnv, unsetEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeDirectory)
import System.IO (hClose, openBinaryTempFile, stderr, stdin, stdout)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

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
  , pluginProtocolEnv
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
import Topo.Plugin.SDK.Payload
  ( decodeTerrainPayloadWithLimits
  , decodeTerrainSnapshotHeader
  , diffTerrainWorldAgainstSnapshot
  , terrainChunkSourceFromFiles
  , terrainChunkSourceFromRecords
  , terrainChunkSourceToRecords
  , terrainChunkRecordsToPayload
  , terrainChunkUpdatesFromWorld
  , writeTerrainWritesToSink
  )
import Topo.Plugin.RPC
  ( applyGeneratorTerrainValueScopedWithLimits
  , decodeTerrainWritesValueScopedWithLimits
  )
import Topo.Plugin.RPC.Stream
  ( NegotiatedStreamV1(..)
  , StreamCancel(..)
  , StreamCodec(..)
  , StreamEffect(..)
  , StreamEnd(..)
  , StreamEnvelope(..)
  , StreamFailure(..)
  , StreamFailureClass(..)
  , StreamId(..)
  , StreamOpen(..)
  , StreamPayloadKind(..)
  , StreamProposal
  , StreamProtocolError(..)
  , StreamRecord(..)
  , StreamRecordKey(..)
  , StreamRole(..)
  , consumeStreamRecord
  , defaultStreamProposal
  , encodeStreamRecord
  , endOutboundStream
  , expireStreams
  , markParentResultReceived
  , negotiateStreamV1
  , newStreamMachine
  , openOutboundStream
  , parentResultReady
  , receiveStreamEnvelopeWithFrameBytes
  , registerStreamRequest
  , sendOutboundRecord
  , streamEnvelopeFrameBytes
  , streamFailureEffects
  , streamRecordsDigest
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
import Topo.World (TerrainWorld(..), emptyWorld, emptyWorldWithPlanet)

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
      || isJust streamingGenerator || isJust streamingSimulation
    generatorParticipates = isJust legacyGenerator || isJust scopedGenerator
      || isJust streamingGenerator
    simulationParticipates = isJust legacySimulation || isJust scopedSimulation
      || isJust streamingSimulation
    supportsV4 = (not generatorParticipates || isJust legacyGenerator)
      && (not simulationParticipates || isJust legacySimulation)
    supportsV5 = (not generatorParticipates || isJust scopedGenerator || isJust streamingGenerator)
      && (not simulationParticipates || isJust scopedSimulation || isJust streamingSimulation)
    legacyGenerator = pdGenerator pd
    legacySimulation = pdSimulation pd
    scopedGenerator = pdGeneratorScope pd
    scopedSimulation = pdSimulationScope pd
    streamingGenerator = pdStreamingGenerator pd
    streamingSimulation = pdStreamingSimulation pd

generatorDecl :: PluginDef -> Maybe RPCGeneratorDecl
generatorDecl pd = case pdStreamingGenerator pd of
  Just streaming -> Just RPCGeneratorDecl
    { rgdInsertAfter = stgdInsertAfter streaming
    , rgdRequires = stgdRequires streaming
    }
  Nothing -> case pdGeneratorScope pd of
    Just scoped -> Just RPCGeneratorDecl
      { rgdInsertAfter = gsdInsertAfter scoped
      , rgdRequires = gsdRequires scoped
      }
    Nothing -> fmap (\legacy -> RPCGeneratorDecl
      { rgdInsertAfter = gdInsertAfter legacy
      , rgdRequires = gdRequires legacy
      }) (pdGenerator pd)

simulationDecl :: PluginDef -> Maybe RPCSimulationDecl
simulationDecl pd = case pdStreamingSimulation pd of
  Just streaming -> Just RPCSimulationDecl
    { rsdDependencies = stsdDependencies streaming
    , rsdSchedule = maybe defaultScheduleDecl id (stsdSchedule streaming)
    }
  Nothing -> case pdSimulationScope pd of
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
    generatorScope = case pdStreamingGenerator pd of
      Just streaming -> Just (stgdScope streaming)
      Nothing -> case pdGeneratorScope pd of
        Just scoped -> Just (gsdScope scoped)
        Nothing -> fmap (const legacyGen) (pdGenerator pd)
    simulationScope = case pdStreamingSimulation pd of
      Just streaming -> Just (stsdScope streaming)
      Nothing -> case pdSimulationScope pd of
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
      [ssdScope scope | Just scope <- [pdSimulationScope pd]] <>
      [stgdScope scope | Just scope <- [pdStreamingGenerator pd]] <>
      [stsdScope scope | Just scope <- [pdStreamingSimulation pd]]
    scopedTerrainRead = any (not . null . rsiTerrainSections . risdInput) scopedDeclarations
    scopedOverlayRead = any (\scope -> rsiOwnOverlay (risdInput scope)
      || not (null (rsiDependencyOverlays (risdInput scope)))) scopedDeclarations
    scopedSimulationTerrainWrite = any
      (not . null . rsoTerrainSections . risdOutput)
      ([ssdScope scope | Just scope <- [pdSimulationScope pd]] <>
       [stsdScope scope | Just scope <- [pdStreamingSimulation pd]])
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
  | SDKPeerCancelled !Text
  deriving (Show)

instance Exception SDKSessionError

data SDKSessionShutdown = SDKSessionShutdown
  deriving (Show)

instance Exception SDKSessionShutdown

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
  negotiatedStreams <- newIORef Nothing
  nextPluginStreamId <- newIORef 2
  inbound <- newTChanIO
  sendLock <- newMVar ()
  readerThread <- forkIO (transportReader limits transport inbound)
  (messageLoop limits pd transport sendLock inbound params Nothing negotiatedVersion negotiatedStreams nextPluginStreamId
    `catch` \SDKSessionShutdown -> pure ())
    `finally` killThread readerThread
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

validateSelectedProtocolEnvironment :: Int -> IO (Either Text ())
validateSelectedProtocolEnvironment requested = do
  selected <- lookupEnv pluginProtocolEnv
  pure $ case selected of
    Nothing -> Right ()
    Just raw -> case reads raw of
      [(value, "")]
        | value == requested -> Right ()
        | otherwise -> Left
            ("handshake protocol does not match TOPO_PLUGIN_PROTOCOL: selected="
              <> Text.pack (show value) <> ", handshake=" <> Text.pack (show requested))
      _ -> Left "TOPO_PLUGIN_PROTOCOL is not a valid protocol version"

type InboundQueue = TChan (Either TransportError BS.ByteString)

transportReader :: RPCPayloadLimits -> Transport -> InboundQueue -> IO ()
transportReader limits transport inbound = do
  result <- recvMessageWithLimit (fromIntegral (rplMaxFrameSizeBytes limits)) transport
  atomically (writeTChan inbound result)
  case result of
    Left _ -> pure ()
    Right _ -> transportReader limits transport inbound

readInbound :: InboundQueue -> IO (Either TransportError BS.ByteString)
readInbound = atomically . readTChan

monotonicMicros :: IO Word64
monotonicMicros = (`div` 1000) <$> getMonotonicTimeNSec

saturatingAdd :: Word64 -> Word64 -> Word64
saturatingAdd left right
  | maxBound - left < right = maxBound
  | otherwise = left + right

streamInvocationDeadline :: NegotiatedStreamV1 -> IO Word64
streamInvocationDeadline streamLimits = do
  now <- monotonicMicros
  pure (saturatingAdd now (nsvIdleTimeoutMicros streamLimits))

readInboundUntil
  :: Word64 -> InboundQueue
  -> IO (Either Text (Either TransportError BS.ByteString))
readInboundUntil deadline inbound = do
  now <- monotonicMicros
  if now >= deadline
    then pure (Left "stream deadline expired")
    else do
      let remaining = deadline - now
          micros = fromInteger (min (toInteger remaining) (toInteger (maxBound :: Int)))
      result <- timeout micros (readInbound inbound)
      pure (maybe (Left "stream deadline expired") Right result)

data ActiveInbound
  = ActiveStream !BS.ByteString !StreamEnvelope
  | ActiveContinue
  | ActiveShutdown

classifyActiveInbound
  :: RPCPayloadLimits -> Transport -> MVar () -> BS.ByteString -> IO ActiveInbound
classifyActiveInbound limits transport sendLock bytes =
  case decodeMessage bytes of
    Left err -> throwIO (SDKReceiveFailure
      (TransportFramingError ("invalid frame during streamed invocation: " <> err)))
    Right envelope -> case envType envelope of
      MsgStreamOpen -> decodeStream
      MsgStreamData -> decodeStream
      MsgStreamWindow -> decodeStream
      MsgStreamEnd -> decodeStream
      MsgStreamCancel -> decodeStream
      MsgStreamError -> decodeStream
      MsgHeartbeat -> do
        withMVar sendLock $ \_ -> sendSDKEnvelope limits transport RPCEnvelope
          { envType = MsgHeartbeat
          , envPayload = Aeson.toJSON (Heartbeat "ok")
          , envRequestId = envRequestId envelope
          }
        pure ActiveContinue
      MsgHealthCheck -> do
        withMVar sendLock $ \_ -> sendSDKEnvelope limits transport RPCEnvelope
          { envType = MsgHealthStatus
          , envPayload = Aeson.toJSON (HealthStatus True "ok")
          , envRequestId = envRequestId envelope
          }
        pure ActiveContinue
      MsgShutdown -> pure ActiveShutdown
      _ -> throwIO (SDKReceiveFailure (TransportFramingError
        "pipelined request is not allowed while a streamed invocation is active"))
  where
    decodeStream = case Aeson.eitherDecodeStrict' bytes of
      Left err -> throwIO (SDKReceiveFailure
        (TransportFramingError ("invalid stream frame: " <> Text.pack err)))
      Right frame -> pure (ActiveStream bytes frame)

classifyActiveInboundUntil
  :: Word64 -> RPCPayloadLimits -> Transport -> MVar () -> BS.ByteString
  -> IO (Either Text ActiveInbound)
classifyActiveInboundUntil deadline limits transport sendLock bytes = do
  now <- monotonicMicros
  if now >= deadline
    then pure (Left "stream deadline expired")
    else do
      let remaining = fromInteger (min (toInteger (deadline - now))
            (toInteger (maxBound :: Int)))
      classified <- timeout remaining
        (classifyActiveInbound limits transport sendLock bytes)
      case classified of
        Just active -> pure (Right active)
        Nothing -> do
          -- The timeout may have interrupted a framed write. Closing is the
          -- only safe recovery because a partial frame cannot be rolled back.
          closeTransport transport
          throwIO (SDKReceiveFailure TransportClosed)

-- | Main message loop. Reads RPC envelopes with the same limit used for every
-- response. Any receive or send failure aborts the session deterministically.
messageLoop
  :: RPCPayloadLimits -> PluginDef -> Transport -> MVar () -> InboundQueue
  -> Map Text Value -> Maybe FilePath
  -> IORef Int -> IORef (Maybe NegotiatedStreamV1) -> IORef Word64 -> IO ()
messageLoop limits pd transport sendLock inbound params worldPath negotiatedVersionRef negotiatedStreamsRef nextStreamIdRef = do
  result <- readInbound inbound
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
      messageLoop limits pd transport sendLock inbound nextParams nextWorldPath negotiatedVersionRef negotiatedStreamsRef nextStreamIdRef
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
          selectedEnvironment <- validateSelectedProtocolEnvironment requestedVersion
          if not supported
            then do
              sendErrorResponse limits transport (envRequestId envelope) 8
                "Handshake protocol is outside this plugin definition's advertised range"
              next params worldPath
            else case selectedEnvironment of
              Left selectedErr -> do
                sendErrorResponse limits transport (envRequestId envelope) 8 selectedErr
                next params worldPath
              Right () -> case negotiateSDKStreams limits
                  (isJust (pdStreamingGenerator pd) || isJust (pdStreamingSimulation pd))
                  requestedVersion (envPayload envelope) of
                Left streamErr -> do
                  sendErrorResponse limits transport (envRequestId envelope) 8 streamErr
                  next params worldPath
                Right negotiated -> do
                  writeIORef negotiatedVersionRef requestedVersion
                  writeIORef negotiatedStreamsRef negotiated
                  let ackPayload = case (Aeson.toJSON ack, negotiated) of
                        (Object fields, Just streamLimits) -> Object
                          (KM.insert "stream_v1" (Aeson.toJSON streamLimits) fields)
                        (value, _) -> value
                  sendSDKEnvelope limits transport RPCEnvelope
                    { envType = MsgHandshakeAck
                    , envPayload = ackPayload
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
              logFn message = withMVar sendLock $ \_ ->
                sendLogMessage limits transport (envRequestId envelope) message
              progressFn message fraction = withMVar sendLock $ \_ ->
                sendProgress limits transport (envRequestId envelope) message fraction
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
                Nothing -> rejectInvocation 6
                  "protocol 5 generator invocation is missing its scope binding" mergedParams
                Just binding -> do
                  negotiatedStreams <- readIORef negotiatedStreamsRef
                  case negotiatedStreams of
                    Nothing -> case pdGeneratorScope pd of
                      Nothing -> sendErrorResponse limits transport (envRequestId envelope) 6
                        "protocol 5 invocation was not preceded by stream_v1 negotiation"
                      Just scoped -> case prepareGeneratorContext limits pd scoped binding mergedParams ig worldPath logFn progressFn of
                        Left err -> sendErrorResponse limits transport (envRequestId envelope) 6 err
                        Right (ctx, validationWorld) -> do
                          runResult <- catchPluginResult (gsdRun scoped ctx)
                          case runResult >>= validateGeneratorResult limits (gcScope ctx) validationWorld of
                            Left errMsg -> sendErrorResponse limits transport (envRequestId envelope) 3 errMsg
                            Right generatorResult -> sendGeneratorResult limits transport
                              (envRequestId envelope) generatorResult
                    Just streamLimits -> handleV5GeneratorInvocation limits streamLimits pd transport sendLock inbound
                      nextStreamIdRef mergedParams worldPath envelope ig binding logFn progressFn
                  next mergedParams worldPath
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
              logFn message = withMVar sendLock $ \_ ->
                sendLogMessage limits transport (envRequestId envelope) message
              progressFn message fraction = withMVar sendLock $ \_ ->
                sendProgress limits transport (envRequestId envelope) message fraction
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
                Nothing -> rejectInvocation 7
                  "protocol 5 simulation invocation is missing its scope binding" mergedParams
                Just binding -> do
                  negotiatedStreams <- readIORef negotiatedStreamsRef
                  case negotiatedStreams of
                    Nothing -> case pdSimulationScope pd of
                      Nothing -> sendErrorResponse limits transport (envRequestId envelope) 7
                        "protocol 5 invocation was not preceded by stream_v1 negotiation"
                      Just scoped -> case prepareSimulationContext limits pd scoped binding mergedParams is' worldPath logFn progressFn of
                        Left err -> sendErrorResponse limits transport (envRequestId envelope) 7 err
                        Right (ctx, validationWorld) -> do
                          runResult <- catchPluginResult (ssdTick scoped ctx)
                          case runResult >>= validateSimulationResult limits pd (scScope ctx) validationWorld of
                            Left errMsg -> sendErrorResponse limits transport (envRequestId envelope) 5 errMsg
                            Right simulationResult -> sendSimulationResult limits transport
                              (envRequestId envelope) simulationResult
                    Just streamLimits -> handleV5SimulationInvocation limits streamLimits pd transport sendLock inbound
                      nextStreamIdRef mergedParams worldPath envelope is' binding logFn progressFn
                  next mergedParams worldPath
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
-- Protocol-v5 streamed invocations
------------------------------------------------------------------------

-- The v5 wire additions intentionally live beside the v4 payload fields.
-- Older Aeson decoders ignore them, while the selected-v5 runner requires and
-- validates them before granting any receive credit.
data SnapshotReference = SnapshotReference
  { snapshotScopeId :: !Text
  , snapshotHeader :: !TerrainSnapshotHeader
  , snapshotStreamIds :: !(Set StreamId)
  }

data DeltaValue
  = DeltaUpdate !BS.ByteString
  | DeltaRemoval

type DeltaMap = Map (TerrainSection, Int) DeltaValue

data SpoolChunk = SpoolChunk
  { spoolPath :: !FilePath
  , spoolNextPart :: !Word32
  , spoolNextOffset :: !Word64
  }

data SnapshotSpool = SnapshotSpool
  { spoolRecordKeys :: !(Set StreamRecordKey)
  , spoolChunks :: !(Map (TerrainSection, Int) SpoolChunk)
  }

negotiateSDKStreams
  :: RPCPayloadLimits -> Bool -> Int -> Value -> Either Text (Maybe NegotiatedStreamV1)
negotiateSDKStreams _ _ protocolVersion _ | protocolVersion <= currentProtocolVersion = Right Nothing
negotiateSDKStreams limits requirePeerOffer _ payload = do
  let local = defaultStreamProposal (fromIntegral (rplMaxFrameSizeBytes limits))
  case payload of
    Object fields -> case KM.lookup "stream_v1" fields of
      Nothing | requirePeerOffer -> Left "protocol 5 handshake omits stream_v1"
      Nothing -> Right Nothing
      Just peerValue -> do
        peer <- case Aeson.fromJSON peerValue of
          Aeson.Error err -> Left ("invalid stream_v1 offer: " <> Text.pack err)
          Aeson.Success proposal -> Right (proposal :: StreamProposal)
        Just <$> negotiateStreamV1 local peer
    _ -> Left "protocol 5 handshake payload must be an object"

parseSnapshotReference :: ResolvedInvocationScope -> Value -> Either Text (Maybe SnapshotReference)
parseSnapshotReference scope (Object fields) = case KM.lookup "terrain_snapshot" fields of
  Nothing
    | Set.null (risTerrainInputSections scope) -> Right Nothing
    | otherwise -> Left "protocol 5 invocation omitted its terrain_snapshot reference"
  Just value -> Just <$> case AesonTypes.parseEither parser value of
    Left err -> Left (Text.pack err)
    Right reference -> Right reference
  where
    parser = Aeson.withObject "terrain_snapshot" $ \snapshot -> do
      payloadVersion <- snapshot Aeson..: "payload_version"
      if payloadVersion /= (1 :: Int) then fail "unsupported terrain_snapshot payload version" else pure ()
      scopeId <- snapshot Aeson..: "scope_id"
      if scopeId /= risScopeId scope then fail "terrain_snapshot scope_id does not match invocation scope" else pure ()
      headerValue <- snapshot Aeson..: "header"
      header <- either (fail . Text.unpack) pure (decodeTerrainSnapshotHeader headerValue)
      streamIds <- Set.fromList <$> snapshot Aeson..: "stream_ids"
      if Set.null streamIds && not (Set.null (risTerrainInputSections scope))
        then fail "terrain_snapshot stream_ids must not be empty for granted terrain input"
        else pure ()
      pure (SnapshotReference scopeId header streamIds)
parseSnapshotReference _ _ = Left "protocol 5 invocation payload must be an object"

handleV5GeneratorInvocation
  :: RPCPayloadLimits -> NegotiatedStreamV1 -> PluginDef -> Transport -> MVar () -> InboundQueue
  -> IORef Word64 -> Map Text Value -> Maybe FilePath -> RPCEnvelope -> InvokeGenerator
  -> RPCInvocationScopeBinding -> (Text -> IO ()) -> (Text -> Double -> IO ()) -> IO ()
handleV5GeneratorInvocation limits streamLimits pd transport sendLock inbound nextStreamIdRef params worldPath envelope invocation binding logFn progressFn =
  case (pdStreamingGenerator pd, pdGeneratorScope pd) of
    (Nothing, Nothing) -> failRequest "generator scope is unknown to this plugin"
    (Just _, Just _) -> failRequest "plugin defines both native and staging v5 generator callbacks"
    (native, staged) -> case validateHostScope pd InvocationGenerator declaration binding of
      Left err -> failRequest err
      Right scope -> case parseSnapshotReference scope (envPayload envelope) of
        Left err -> failRequest err
        Right reference -> do
          deadline <- streamInvocationDeadline streamLimits
          snapshotResult <- receiveSnapshot limits streamLimits transport sendLock inbound deadline requestId scope reference
          case snapshotResult of
            Left err -> failRequest err
            Right (source, terrainHeader, cleanup, validationWorld) -> flip finally cleanup $ do
              deltaRef <- newIORef (Map.empty, 0 :: Word64)
              (cancellation, cancelCallback) <- newCancellation
              let guardedSource = fmap (guardTerrainChunkSource deadline cancellation) source
                  sink = makeDeltaSink streamLimits scope terrainHeader deadline cancellation deltaRef
              callbackResult <- runWithStreamCancellation limits streamLimits transport sendLock inbound deadline requestId cancelCallback $ case native of
                Just definition -> do
                  result <- catchPluginResult (stgdRun definition StreamingGeneratorContext
                    { stgcParams = params
                    , stgcTerrainHeader = terrainHeader
                    , stgcTerrain = guardedSource
                    , stgcTerrainDelta = sink
                    , stgcSeed = igSeed invocation
                    , stgcScope = scope
                    , stgcCancellation = cancellation
                    , stgcLog = logFn
                    , stgcProgress = progressFn
                    , stgcWorldPath = worldPath
                    })
                  pure (result >>= validateStreamingGeneratorResult scope)
                Nothing -> case staged of
                  Nothing -> pure (Left "generator callback is unavailable")
                  Just definition -> do
                    terrainPayloadResult <- case guardedSource of
                      Nothing -> pure $ case terrainHeader of
                        Nothing -> Right (igTerrain invocation)
                        Just header -> terrainChunkRecordsToPayload header []
                      Just terrainSource -> do
                        recordsResult <- terrainChunkSourceToRecords terrainSource
                        pure (recordsResult >>= terrainChunkRecordsToPayload (tcsHeader terrainSource))
                    case terrainPayloadResult of
                      Left err -> pure (Left err)
                      Right terrainPayload -> case prepareGeneratorContext limits pd definition binding params
                          invocation { igTerrain = terrainPayload } worldPath logFn progressFn of
                        Left err -> pure (Left err)
                        Right (context, baseWorld) -> do
                          result <- catchPluginResult (gsdRun definition context)
                          case result >>= validateGeneratorResult limits scope baseWorld of
                            Left err -> pure (Left err)
                            Right legacyResult -> case applyGeneratorTerrainValueScopedWithLimits
                                limits scope baseWorld (gtrTerrain legacyResult) of
                              Left err -> pure (Left err)
                              Right updated -> do
                                diffResult <- case guardedSource of
                                  Just terrainSource -> diffTerrainWorldAgainstSnapshot scope terrainSource updated sink
                                  Nothing -> case terrainChunkUpdatesFromWorld scope updated of
                                    Left err -> pure (Left err)
                                    Right updates -> writeUpdatesToSink sink updates
                                pure (diffResult >> Right StreamingGeneratorResult
                                  { stgrOverlay = gtrOverlay legacyResult
                                  , stgrMetadata = gtrMetadata legacyResult
                                  })
              case callbackResult of
                Left err -> failRequest err
                Right inlineResult -> do
                  deltas <- fst <$> readIORef deltaRef
                  sendStreamingGeneratorResult limits streamLimits transport sendLock inbound deadline nextStreamIdRef
                    requestId scope inlineResult deltas
  where
    requestId = maybe 0 id (envRequestId envelope)
    declaration = maybe (maybe (error "unreachable") gsdScope (pdGeneratorScope pd))
      stgdScope (pdStreamingGenerator pd)
    failRequest err = sendErrorResponse limits transport (envRequestId envelope) 3 err

handleV5SimulationInvocation
  :: RPCPayloadLimits -> NegotiatedStreamV1 -> PluginDef -> Transport -> MVar () -> InboundQueue
  -> IORef Word64 -> Map Text Value -> Maybe FilePath -> RPCEnvelope -> InvokeSimulation
  -> RPCInvocationScopeBinding -> (Text -> IO ()) -> (Text -> Double -> IO ()) -> IO ()
handleV5SimulationInvocation limits streamLimits pd transport sendLock inbound nextStreamIdRef params worldPath envelope invocation binding logFn progressFn =
  case (pdStreamingSimulation pd, pdSimulationScope pd) of
    (Nothing, Nothing) -> failRequest "simulation scope is unknown to this plugin"
    (Just _, Just _) -> failRequest "plugin defines both native and staging v5 simulation callbacks"
    (native, staged) -> case validateHostScope pd InvocationSimulation declaration binding of
      Left err -> failRequest err
      Right scope -> case validateStreamingSimulationInputs scope invocation of
        Left err -> failRequest err
        Right (ownOverlay, overlays) -> case parseSnapshotReference scope (envPayload envelope) of
          Left err -> failRequest err
          Right reference -> do
            deadline <- streamInvocationDeadline streamLimits
            snapshotResult <- receiveSnapshot limits streamLimits transport sendLock inbound deadline requestId scope reference
            case snapshotResult of
              Left err -> failRequest err
              Right (source, terrainHeader, cleanup, validationWorld) -> flip finally cleanup $ do
                deltaRef <- newIORef (Map.empty, 0 :: Word64)
                (cancellation, cancelCallback) <- newCancellation
                let guardedSource = fmap (guardTerrainChunkSource deadline cancellation) source
                    sink = makeDeltaSink streamLimits scope terrainHeader deadline cancellation deltaRef
                callbackResult <- runWithStreamCancellation limits streamLimits transport sendLock inbound deadline requestId cancelCallback $ case native of
                  Just definition -> do
                    result <- catchPluginResult (stsdTick definition StreamingSimulationContext
                      { stscParams = params
                      , stscTerrainHeader = terrainHeader
                      , stscTerrain = guardedSource
                      , stscTerrainDelta = sink
                      , stscOwnOverlay = ownOverlay
                      , stscOverlays = overlays
                      , stscWorldTime = isWorldTime invocation
                      , stscDeltaTicks = isDeltaTicks invocation
                      , stscCalendar = isCalendar invocation
                      , stscScope = scope
                      , stscCancellation = cancellation
                      , stscLog = logFn
                      , stscProgress = progressFn
                      , stscWorldPath = worldPath
                      })
                    pure (result >>= validateStreamingSimulationResult limits pd scope validationWorld)
                  Nothing -> case staged of
                    Nothing -> pure (Left "simulation callback is unavailable")
                    Just definition -> do
                      terrainPayloadResult <- case guardedSource of
                        Nothing -> pure $ case terrainHeader of
                          Nothing -> Right (isTerrain invocation)
                          Just header -> terrainChunkRecordsToPayload header []
                        Just terrainSource -> do
                          recordsResult <- terrainChunkSourceToRecords terrainSource
                          pure (recordsResult >>= terrainChunkRecordsToPayload (tcsHeader terrainSource))
                      case terrainPayloadResult of
                        Left err -> pure (Left err)
                        Right terrainPayload -> case prepareSimulationContext limits pd definition binding params
                            invocation { isTerrain = terrainPayload } worldPath logFn progressFn of
                          Left err -> pure (Left err)
                          Right (context, baseWorld) -> do
                            result <- catchPluginResult (ssdTick definition context)
                            case result >>= validateSimulationResult limits pd scope baseWorld of
                              Left err -> pure (Left err)
                              Right legacyResult -> case decodeTerrainWritesValueScopedWithLimits
                                  limits scope baseWorld (strTerrainWrites legacyResult) of
                                Left err -> pure (Left err)
                                Right writes -> do
                                  sinkResult <- writeTerrainWritesToSink (twConfig baseWorld) sink writes
                                  pure (sinkResult >> Right StreamingSimulationResult
                                    { stsrOverlay = strOverlay legacyResult })
                case callbackResult of
                  Left err -> failRequest err
                  Right inlineResult -> do
                    deltas <- fst <$> readIORef deltaRef
                    sendStreamingSimulationResult limits streamLimits transport sendLock inbound deadline nextStreamIdRef
                      requestId scope inlineResult deltas
  where
    requestId = maybe 0 id (envRequestId envelope)
    declaration = maybe (maybe (error "unreachable") ssdScope (pdSimulationScope pd))
      stsdScope (pdStreamingSimulation pd)
    failRequest err = sendErrorResponse limits transport (envRequestId envelope) 5 err

validateStreamingSimulationInputs
  :: ResolvedInvocationScope -> InvokeSimulation -> Either Text (Maybe Value, Map Text Value)
validateStreamingSimulationInputs scope invocation = do
  validateOverlayInputBudget scope (isOverlays invocation) (isOwnOverlay invocation)
  overlays <- case isOverlays invocation of
    Object _ -> Right (valueObjectToMap (isOverlays invocation))
    _ -> Left "dependency overlay collection must be an object"
  let granted = Map.keysSet (risDependencyOverlayChunkIds scope)
  if Map.keysSet overlays == granted then pure ()
    else Left "host dependency overlay names do not exactly match the resolved scope"
  mapM_ (\(name, value) -> maybe
      (Left ("dependency overlay is outside resolved scope: " <> name))
      (\chunks -> validateOverlayPayloadChunks ("dependency overlay " <> name) chunks value)
      (Map.lookup name (risDependencyOverlayChunkIds scope))) (Map.toList overlays)
  own <- case risOwnedOverlayIdentity scope of
    Nothing -> case isOwnOverlay invocation of
      Null -> Right Nothing
      _ -> Left "host supplied own overlay without a resolved read grant"
    Just _ -> validateOverlayPayloadChunks "own overlay input"
      (risOwnOverlayReadChunkIds scope) (isOwnOverlay invocation) >> Right (Just (isOwnOverlay invocation))
  Right (own, overlays)

receiveSnapshot
  :: RPCPayloadLimits -> NegotiatedStreamV1 -> Transport -> MVar () -> InboundQueue
  -> Word64 -> Word64 -> ResolvedInvocationScope -> Maybe SnapshotReference
  -> IO (Either Text (Maybe TerrainChunkSource, Maybe TerrainSnapshotHeader, IO (), TerrainWorld))
receiveSnapshot _ _ _ _ _ _ _ scope Nothing
  | Set.null (risTerrainInputSections scope) = pure (Right (Nothing, Nothing, pure (), stubWorld))
receiveSnapshot limits streamLimits transport sendLock inbound deadline requestId scope (Just reference)
  | requestId == 0 = pure (Left "streamed invocation requires a nonzero correlated request ID")
  | otherwise = do
      now <- monotonicMicros
      case registerStreamRequest requestId scope deadline
          (newStreamMachine StreamPlugin streamLimits) >>= markParentResultReceived requestId
            (snapshotStreamIds reference) of
        Left failure -> pure (Left (sfMessage failure))
        Right machine -> loop machine (SnapshotSpool Set.empty Map.empty)
  where
    loop machine spool = (do
      current <- monotonicMicros
      checked <- expireMachine current machine
      case checked of
        Left err -> failWith spool err
        Right liveMachine
          | parentResultReady requestId liveMachine -> finishSnapshot liveMachine spool
          | otherwise -> do
              received <- readInboundUntil deadline inbound
              case received of
                Left _ -> do
                  expiredAt <- monotonicMicros
                  expired <- expireMachine expiredAt liveMachine
                  failWith spool (either id (const "stream deadline expired") expired)
                Right (Left err) -> failWith spool
                  ("stream receive failed: " <> Text.pack (show err))
                Right (Right bytes) -> do
                  classified <- classifyActiveInboundUntil deadline limits transport sendLock bytes
                  case classified of
                    Left _ -> do
                      expiredAt <- monotonicMicros
                      expired <- expireMachine expiredAt liveMachine
                      failWith spool (either id (const "stream deadline expired") expired)
                    Right active -> case active of
                      ActiveContinue -> loop liveMachine spool
                      ActiveShutdown -> do
                        cleanupSnapshotSpool spool
                        throwIO SDKSessionShutdown
                      ActiveStream original frame -> handleStream liveMachine spool original frame)
      `onException` cleanupSnapshotSpool spool

    finishSnapshot machine spool = do
      current <- monotonicMicros
      checked <- expireMachine current machine
      case checked of
        Left err -> failWith spool err
        Right _ -> do
          let files =
                [ (section, chunkId, spoolPath chunk)
                | ((section, chunkId), chunk) <- Map.toAscList (spoolChunks spool)
                ]
              cleanup = cleanupSnapshotSpool spool
              header = snapshotHeader reference
              world = emptyWorldWithPlanet
                (WorldConfig (tshChunkSize header)) (tshHexGrid header)
                (tshPlanet header) (tshSlice header)
          sourceResult <- terrainChunkSourceFromFiles header files
            `catch` \(err :: SomeException) -> pure
              (Left ("failed to validate staged terrain snapshot: " <> Text.pack (show err)))
          finishedAt <- monotonicMicros
          if finishedAt >= deadline
            then cleanup >> pure (Left "stream deadline expired")
            else case sourceResult of
              Left err -> cleanup >> pure (Left err)
              Right source -> pure (Right
                ( if Set.null (risTerrainInputSections scope) then Nothing else Just source
                , Just header
                , cleanup
                , world
                ))

    handleStream machine spool bytes frame = case frame of
      StreamOpenEnvelope open | soPayloadKind open /= TerrainSnapshot -> do
        _ <- sendStreamEffectsLocked streamLimits transport sendLock
          [SendStreamError (StreamProtocolError (soStreamId open)
            (soParentRequestId open) "wrong_payload_kind"
            "referenced invocation input stream is not terrain_snapshot")]
        failWith spool "referenced invocation input stream is not terrain_snapshot"
      _ -> do
        now <- monotonicMicros
        let (machine1, outcome) = receiveStreamEnvelopeWithFrameBytes now
              (fromIntegral (BS.length bytes)) frame machine
        case outcome of
          Left failure -> do
            _ <- sendStreamEffectsLocked streamLimits transport sendLock
              (streamFailureEffects failure)
            case sfClass failure of
              StreamConnectionCorruption -> do
                cleanupSnapshotSpool spool
                throwIO (SDKReceiveFailure (TransportFramingError (sfMessage failure)))
              StreamRequestFailure -> failWith spool (sfMessage failure)
          Right effects -> do
            sent <- sendStreamEffectsLocked streamLimits transport sendLock effects
            case sent of
              Left err -> failWith spool err
              Right () -> consumeDelivered machine1 spool effects

    consumeDelivered machine spool [] = loop machine spool
    consumeDelivered machine spool (effect:rest) = case effect of
      DeliverStreamRecord streamId _ key raw -> do
        spooled <- spoolSnapshotFragment spool key raw
          `catch` \(err :: SomeException) -> pure
            (Left ("failed to spool terrain snapshot: " <> Text.pack (show err)))
        case spooled of
          Left err -> failWith spool err
          Right spool' -> do
            now <- monotonicMicros
            case consumeStreamRecord now streamId machine of
              Left failure -> failWith spool' (sfMessage failure)
              Right (machine', more) -> do
                sent <- sendStreamEffectsLocked streamLimits transport sendLock more
                case sent of
                  Left err -> failWith spool' err
                  Right () -> consumeDelivered machine' spool' rest
      ParentRequestCancelled _ reason -> failWith spool reason
      ConnectionMustClose reason -> failWith spool reason
      _ -> consumeDelivered machine spool rest

    expireMachine now machine = do
      let (machine', effects) = expireStreams now machine
      sent <- sendStreamEffectsLockedBounded streamLimits transport sendLock effects
      pure (machine' <$ sent)

    failWith spool err = cleanupSnapshotSpool spool >> pure (Left err)
receiveSnapshot _ _ _ _ _ _ _ _ Nothing = pure (Left "host omitted granted terrain snapshot")

spoolSnapshotFragment
  :: SnapshotSpool -> StreamRecordKey -> BS.ByteString
  -> IO (Either Text SnapshotSpool)
spoolSnapshotFragment spool key raw
  | Set.member key (spoolRecordKeys spool) =
      pure (Left "terrain snapshot contains duplicate records across streams")
  | srkPart key == maxBound = pure (Left "terrain snapshot contains a removal record")
  | otherwise = case Map.lookup logicalKey (spoolChunks spool) of
      Nothing
        | srkPart key /= 0 || srkOffset key /= 0 ->
            pure (Left "terrain snapshot chunk does not begin at part and offset zero")
        | otherwise -> do
            temp <- getTemporaryDirectory
            (path, handle) <- openBinaryTempFile temp "topo-sdk-snapshot.chunk"
            let cleanupCreated = do
                  hClose handle `catch` \(_ :: SomeException) -> pure ()
                  removeFile path `catch` \(_ :: SomeException) -> pure ()
            (hClose handle >> BS.writeFile path raw >> pure (Right (insertChunk path)))
              `onException` cleanupCreated
      Just chunk
        | srkPart key /= spoolNextPart chunk ->
            pure (Left "terrain snapshot chunk parts are not contiguous")
        | srkOffset key /= spoolNextOffset chunk ->
            pure (Left "terrain snapshot chunk offsets are not contiguous")
        | otherwise -> do
            BS.appendFile (spoolPath chunk) raw
            pure (Right (advanceChunk chunk))
  where
    logicalKey = (srkSection key, srkChunkId key)
    amount = fromIntegral (BS.length raw)
    withKey chunks = SnapshotSpool
      (Set.insert key (spoolRecordKeys spool)) chunks
    insertChunk path = withKey (Map.insert logicalKey
      (SpoolChunk path 1 amount) (spoolChunks spool))
    advanceChunk chunk = withKey (Map.insert logicalKey chunk
      { spoolNextPart = spoolNextPart chunk + 1
      , spoolNextOffset = spoolNextOffset chunk + amount
      } (spoolChunks spool))

cleanupSnapshotSpool :: SnapshotSpool -> IO ()
cleanupSnapshotSpool spool = mapM_ remove
  [spoolPath chunk | chunk <- Map.elems (spoolChunks spool)]
  where
    remove path = (do
      exists <- doesFileExist path
      if exists then removeFile path else pure ())
      `catch` \(_ :: SomeException) -> pure ()

guardTerrainChunkSource
  :: Word64 -> InvocationCancellation -> TerrainChunkSource -> TerrainChunkSource
guardTerrainChunkSource deadline cancellation source = TerrainChunkSource
  { tcsHeader = tcsHeader source
  , tcsFoldChunks = \initial step -> do
      active <- ensureInvocationActive deadline cancellation
      case active of
        Left err -> pure (Left err)
        Right () -> tcsFoldChunks source initial $ \acc record -> do
          before <- ensureInvocationActive deadline cancellation
          case before of
            Left err -> pure (Left err)
            Right () -> do
              stepped <- step acc record
              case stepped of
                Left err -> pure (Left err)
                Right next -> do
                  after <- ensureInvocationActive deadline cancellation
                  pure (next <$ after)
  }

ensureInvocationActive :: Word64 -> InvocationCancellation -> IO (Either Text ())
ensureInvocationActive deadline cancellation = do
  reason <- icCancellationReason cancellation
  case reason of
    Just value -> pure (Left value)
    Nothing -> do
      now <- monotonicMicros
      pure $ if now >= deadline then Left "stream deadline expired" else Right ()

makeDeltaSink
  :: NegotiatedStreamV1 -> ResolvedInvocationScope -> Maybe TerrainSnapshotHeader
  -> Word64 -> InvocationCancellation -> IORef (DeltaMap, Word64) -> TerrainDeltaSink
makeDeltaSink streamLimits scope header deadline cancellation ref = TerrainDeltaSink
  { tdsWriteChunk = \update -> do
      active <- ensureInvocationActive deadline cancellation
      case active of
        Left err -> pure (Left err)
        Right () -> case header of
          Nothing -> pure (Left "terrain delta updates require immutable snapshot geometry")
          Just snapshotHeader -> case terrainChunkSourceFromRecords snapshotHeader
              [TerrainChunkRecord (tcuSection update) (tcuChunkId update) (tcuBytes update)] of
            Left err -> pure (Left ("invalid terrain delta chunk: " <> err))
            Right _ -> add (tcuSection update) (tcuChunkId update)
              (DeltaUpdate (tcuBytes update)) (fromIntegral (BS.length (tcuBytes update)))
  , tdsRemoveChunk = \removal -> add
      (tcrmSection removal) (tcrmChunkId removal) DeltaRemoval 0
  }
  where
    add section chunkId value amount = do
      active <- ensureInvocationActive deadline cancellation
      case active of
        Left err -> pure (Left err)
        Right () -> atomicModifyIORef' ref $ \current@(entries, used) ->
          let key = (section, chunkId)
              allowed = Set.member section (risTerrainOutputSections scope)
                && IntSet.member chunkId (risTerrainOutputChunkIds scope)
              scopeBudget = rsbOutputBytes (risBudgets scope)
              byteBudget = min scopeBudget (nsvMaxUncompressedBytes streamLimits)
              existingItems = sum
                [ fromIntegral (length (fragmentDelta streamLimits (entryKey, entryValue)))
                | (entryKey, entryValue) <- Map.toList entries
                ]
              newItems = fromIntegral (length (fragmentDelta streamLimits (key, value)))
              itemBudget = nsvMaxItems streamLimits
          in if not allowed
              then (current, Left "terrain delta key is outside the resolved output scope")
              else if Map.member key entries
                then (current, Left "terrain delta contains a duplicate update/removal key")
                else if amount > byteBudget || used > byteBudget - amount
                  then (current, Left "terrain delta exceeds the negotiated or resolved output byte budget")
                  else if newItems > itemBudget || existingItems > itemBudget - newItems
                    then (current, Left "terrain delta exceeds the negotiated item limit")
                    else ((Map.insert key value entries, used + amount), Right ())

newCancellation :: IO (InvocationCancellation, Text -> IO ())
newCancellation = do
  reasonRef <- newIORef Nothing
  signal <- newEmptyMVar
  let cancel reason = do
        first <- atomicModifyIORef' reasonRef $ \current -> case current of
          Nothing -> (Just reason, True)
          Just _ -> (current, False)
        if first then putMVar signal reason else pure ()
  pure
    ( InvocationCancellation
        { icCancellationReason = readIORef reasonRef
        , icAwaitCancellation = readMVar signal
        }
    , cancel
    )

-- Run user code away from the sole transport reader so peer controls and the
-- absolute invocation deadline can interrupt a slow callback. The worker is
-- always joined before staged snapshot files are released.
runWithStreamCancellation
  :: RPCPayloadLimits -> NegotiatedStreamV1 -> Transport -> MVar () -> InboundQueue
  -> Word64 -> Word64 -> (Text -> IO ())
  -> IO (Either Text a) -> IO (Either Text a)
runWithStreamCancellation limits _ transport sendLock inbound deadline requestId cancel action = do
  callbackResult <- newEmptyTMVarIO
  callbackThread <- forkFinally action $ \result ->
    atomically (putTMVar callbackResult result)
  let await remaining = timeout remaining $ atomically $
        (Right <$> readTChan inbound) `orElse` (Left <$> readTMVar callbackResult)
      terminateWorker = mask_ $ do
        token <- tryTakeMVar sendLock
        case token of
          Just () -> do
            -- Holding the token prevents the callback from entering a framed
            -- send while it is asynchronously cancelled.
            result <- (killThread callbackThread
              >> atomically (readTMVar callbackResult))
              `finally` putMVar sendLock ()
            pure (False, result)
          Nothing -> do
            -- The callback may be inside a framed write. Abort the connection
            -- before cancellation rather than risk continuing a partial frame.
            closeTransport transport
            killThread callbackThread
            result <- atomically (readTMVar callbackResult)
            pure (True, result)
      stop reason = do
        cancel reason
        (connectionClosed, _) <- terminateWorker
        if connectionClosed
          then throwIO (SDKReceiveFailure TransportClosed)
          else pure (Left reason)
      stopShutdown = do
        cancel "host requested shutdown"
        _ <- terminateWorker
        throwIO SDKSessionShutdown
      loop = do
        now <- monotonicMicros
        if now >= deadline
          then stop "stream deadline expired"
          else do
            let remaining = fromInteger (min (toInteger (deadline - now))
                  (toInteger (maxBound :: Int)))
            event <- await remaining
            case event of
              Nothing -> stop "stream deadline expired"
              Just (Left result) -> do
                finishedAt <- monotonicMicros
                if finishedAt >= deadline
                  then stop "stream deadline expired"
                  else case result of
                    Right value -> pure value
                    Left err -> case fromException err :: Maybe SDKSessionError of
                      Just sdkErr -> throwIO sdkErr
                      Nothing -> case fromException err :: Maybe SDKSessionShutdown of
                        Just shutdown -> throwIO shutdown
                        Nothing -> pure (Left (Text.pack (show err)))
              Just (Right received) -> case received of
                Left err -> do
                  cancel ("stream transport disconnected during callback: "
                    <> Text.pack (show err))
                  _ <- terminateWorker
                  throwIO (SDKReceiveFailure err)
                Right bytes -> do
                  classified <- classifyActiveInboundUntil deadline limits transport sendLock bytes
                  case classified of
                    Left reason -> stop reason
                    Right active -> case active of
                      ActiveContinue -> loop
                      ActiveShutdown -> stopShutdown
                      ActiveStream _ frame -> case frame of
                        StreamCancelEnvelope value
                          | scParentRequestId value == requestId -> stop (scReason value)
                        StreamErrorEnvelope value
                          | speParentRequestId value == requestId -> stop (speMessage value)
                        _ -> throwIO (SDKReceiveFailure (TransportFramingError
                          "unexpected stream frame while a plugin callback is active"))
  loop `onException` do
    _ <- terminateWorker
    pure ()

validateStreamingGeneratorResult
  :: ResolvedInvocationScope -> StreamingGeneratorResult -> Either Text StreamingGeneratorResult
validateStreamingGeneratorResult scope result = do
  case stgrOverlay result of
    Nothing -> pure ()
    Just overlay -> case risOwnedOverlayIdentity scope of
      Nothing -> Left "streaming generator emitted an unavailable owned overlay"
      Just _ -> validateOverlayPayloadChunks "generator owned-overlay output"
        (risOwnOverlayWriteChunkIds scope) overlay
  case stgrMetadata result of
    Nothing -> pure ()
    Just _ | risGeneratorMetadataOutput scope -> pure ()
    Just _ -> Left "generator metadata output is not granted"
  pure result

validateStreamingSimulationResult
  :: RPCPayloadLimits -> PluginDef -> ResolvedInvocationScope -> TerrainWorld
  -> StreamingSimulationResult -> Either Text StreamingSimulationResult
validateStreamingSimulationResult limits pd scope world result = do
  _ <- validateSimulationResult limits pd scope world SimulationTickResult
    { strOverlay = stsrOverlay result, strTerrainWrites = Nothing }
  pure result

sendStreamingGeneratorResult
  :: RPCPayloadLimits -> NegotiatedStreamV1 -> Transport -> MVar () -> InboundQueue
  -> Word64 -> IORef Word64 -> Word64
  -> ResolvedInvocationScope -> StreamingGeneratorResult -> DeltaMap -> IO ()
sendStreamingGeneratorResult limits streamLimits transport sendLock inbound deadline nextId requestId scope result deltas =
  sendStreamingResult limits streamLimits transport sendLock inbound deadline nextId requestId scope MsgGeneratorResult deltas $ \deltaRef ->
    object $ ["terrain" .= object []]
      <> ["overlay" .= overlay | Just overlay <- [stgrOverlay result]]
      <> ["metadata" .= metadata | Just metadata <- [stgrMetadata result]]
      <> ["terrain_delta" .= value | Just value <- [deltaRef]]

sendStreamingSimulationResult
  :: RPCPayloadLimits -> NegotiatedStreamV1 -> Transport -> MVar () -> InboundQueue
  -> Word64 -> IORef Word64 -> Word64
  -> ResolvedInvocationScope -> StreamingSimulationResult -> DeltaMap -> IO ()
sendStreamingSimulationResult limits streamLimits transport sendLock inbound deadline nextId requestId scope result deltas =
  sendStreamingResult limits streamLimits transport sendLock inbound deadline nextId requestId scope MsgSimulationResult deltas $ \deltaRef ->
    object $ ["overlay" .= stsrOverlay result]
      <> ["terrain_delta" .= value | Just value <- [deltaRef]]

sendStreamingResult
  :: RPCPayloadLimits -> NegotiatedStreamV1 -> Transport -> MVar () -> InboundQueue
  -> Word64 -> IORef Word64 -> Word64
  -> ResolvedInvocationScope -> RPCMessageType -> DeltaMap
  -> (Maybe Value -> Value) -> IO ()
sendStreamingResult limits streamLimits transport sendLock inbound deadline nextId requestId scope resultType deltas buildPayload = do
  streamId <- if Map.null deltas then pure Nothing else Just . StreamId <$>
    atomicModifyIORef' nextId (\value -> (value + 2, value))
  let deltaReference = fmap (\sid -> object
        [ "payload_version" .= (1 :: Int)
        , "scope_id" .= risScopeId scope
        , "stream_ids" .= [sid]
        ]) streamId
      payload = buildPayload deltaReference
      inlineBytes = fromIntegral (BL.length (Aeson.encode payload))
      deltaBytes = sum
        [ fromIntegral (BS.length raw)
        | DeltaUpdate raw <- Map.elems deltas
        ]
      outputBudget = rsbOutputBytes (risBudgets scope)
      preflight = case streamId of
        Nothing -> Right ()
        Just sid -> preflightDeltaStream streamLimits requestId scope sid deltas
  now <- monotonicMicros
  if now >= deadline
    then sendErrorResponse limits transport (Just requestId) 15 "stream deadline expired"
    else if deltaBytes > outputBudget || inlineBytes > outputBudget - deltaBytes
      then sendErrorResponse limits transport (Just requestId) 15
        "combined terrain delta and inline overlay/metadata exceed the resolved output budget"
      else case preflight of
        Left err -> sendErrorResponse limits transport (Just requestId) 15 err
        Right () -> do
          sendSDKEnvelope limits transport RPCEnvelope
            { envType = resultType, envPayload = payload, envRequestId = Just requestId }
          case streamId of
            Nothing -> pure ()
            Just sid -> sendDeltaStream limits streamLimits transport sendLock inbound deadline requestId scope sid deltas
              `catch` \err -> case err of
                SDKPeerCancelled _ -> pure ()
                _ -> throwIO err

sendDeltaStream
  :: RPCPayloadLimits -> NegotiatedStreamV1 -> Transport -> MVar () -> InboundQueue
  -> Word64 -> Word64 -> ResolvedInvocationScope -> StreamId -> DeltaMap -> IO ()
sendDeltaStream limits streamLimits transport sendLock inbound deadline requestId scope streamId deltas = do
  let fragments = concatMap (fragmentDelta streamLimits) (Map.toAscList deltas)
      encodedRecords = zipWith (\sequenceNo (key, raw) ->
        encodeStreamRecord StreamIdentity streamId requestId sequenceNo key raw) [0..] fragments
      records = zip encodedRecords fragments
      totalBytes = sum (map (fromIntegral . BS.length . snd) fragments)
      digest = streamRecordsDigest fragments
      open = StreamOpen
        { soStreamId = streamId
        , soParentRequestId = requestId
        , soScopeId = risScopeId scope
        , soPayloadKind = TerrainDelta
        , soPayloadVersion = 1
        , soSections = Set.fromList [section | ((section, _), _) <- Map.toList deltas]
        , soChunkIds = IntSet.fromList [chunkId | ((_, chunkId), _) <- Map.toList deltas]
        , soMetadata = object []
        , soCodec = StreamIdentity
        , soTotalItems = Just (fromIntegral (length records))
        , soTotalBytes = Just totalBytes
        , soFinalSha256 = Just digest
        }
      start = newStreamMachine StreamPlugin streamLimits
  now <- monotonicMicros
  case registerStreamRequest requestId scope deadline start
      >>= markParentResultReceived requestId (Set.singleton streamId)
      >>= openOutboundStream now open of
    Left failure -> throwIO (SDKReceiveFailure (TransportFramingError (sfMessage failure)))
    Right machine -> do
      sendStreamFrameLocked streamLimits transport sendLock (StreamOpenEnvelope open)
      (machine', _) <- sendRecords machine 0 records
      checked <- expireMachine machine'
      let end = StreamEnd streamId requestId (fromIntegral (length records)) totalBytes digest
      endedAt <- monotonicMicros
      case endOutboundStream endedAt end checked of
        Left failure -> throwIO (SDKReceiveFailure (TransportFramingError (sfMessage failure)))
        Right _ -> sendStreamFrameLocked streamLimits transport sendLock (StreamEndEnvelope end)
  where
    sendRecords machine credit remaining = do
      liveMachine <- expireMachine machine
      case remaining of
        [] -> pure (liveMachine, credit)
        (record, (_, raw)):rest
          | srUncompressedLength record <= credit -> do
              now <- monotonicMicros
              case sendOutboundRecord now record raw liveMachine of
                Left failure -> throwIO
                  (SDKReceiveFailure (TransportFramingError (sfMessage failure)))
                Right machine' -> do
                  sendStreamFrameLocked streamLimits transport sendLock
                    (StreamDataEnvelope record)
                  sendRecords machine' (credit - srUncompressedLength record) rest
          | otherwise -> do
              received <- readInboundUntil deadline inbound
              case received of
                Left _ -> expireMachine liveMachine >>= \_ ->
                  throwIO (SDKPeerCancelled "stream deadline expired")
                Right (Left err) -> throwIO (SDKReceiveFailure err)
                Right (Right bytes) -> do
                  classified <- classifyActiveInboundUntil deadline limits transport sendLock bytes
                  case classified of
                    Left reason -> expireMachine liveMachine >>= \_ ->
                      throwIO (SDKPeerCancelled reason)
                    Right active -> case active of
                      ActiveContinue -> sendRecords liveMachine credit remaining
                      ActiveShutdown -> throwIO SDKSessionShutdown
                      ActiveStream original frame -> do
                        frameAt <- monotonicMicros
                        let (machine', outcome) = receiveStreamEnvelopeWithFrameBytes frameAt
                              (fromIntegral (BS.length original)) frame liveMachine
                        case outcome of
                          Left failure -> do
                            _ <- sendStreamEffectsLocked streamLimits transport sendLock
                              (streamFailureEffects failure)
                            case frame of
                              StreamCancelEnvelope value -> throwIO
                                (SDKPeerCancelled (scReason value))
                              StreamErrorEnvelope value -> throwIO
                                (SDKPeerCancelled (speMessage value))
                              _ -> throwIO (SDKReceiveFailure
                                (TransportFramingError (sfMessage failure)))
                          Right effects -> do
                            sent <- sendStreamEffectsLocked streamLimits transport sendLock effects
                            case sent of
                              Left reason -> throwIO (SDKPeerCancelled reason)
                              Right () -> case frame of
                                StreamWindowEnvelope sid parent amount
                                  | sid == streamId && parent == requestId ->
                                      sendRecords machine' (credit + amount) remaining
                                StreamCancelEnvelope value -> throwIO
                                  (SDKPeerCancelled (scReason value))
                                StreamErrorEnvelope value -> throwIO
                                  (SDKPeerCancelled (speMessage value))
                                _ -> sendRecords machine' credit remaining

    expireMachine machine = do
      now <- monotonicMicros
      let (machine', effects) = expireStreams now machine
      sent <- sendStreamEffectsLockedBounded streamLimits transport sendLock effects
      case sent of
        Left reason -> throwIO (SDKPeerCancelled reason)
        Right () -> pure machine'

preflightDeltaStream
  :: NegotiatedStreamV1 -> Word64 -> ResolvedInvocationScope -> StreamId
  -> DeltaMap -> Either Text ()
preflightDeltaStream limits requestId scope streamId deltas = do
  let fragments = concatMap (fragmentDelta limits) (Map.toAscList deltas)
      records = zipWith (\sequenceNo (key, raw) ->
        encodeStreamRecord StreamIdentity streamId requestId sequenceNo key raw) [0..] fragments
      totalBytes = sum (map (fromIntegral . BS.length . snd) fragments)
      digest = streamRecordsDigest fragments
      open = StreamOpen streamId requestId (risScopeId scope) TerrainDelta 1
        (Set.fromList [section | ((section, _), _) <- Map.toList deltas])
        (IntSet.fromList [chunkId | ((_, chunkId), _) <- Map.toList deltas])
        (object []) StreamIdentity (Just (fromIntegral (length records)))
        (Just totalBytes) (Just digest)
  if fromIntegral (length records) > nsvMaxItems limits
    then Left "terrain delta exceeds the negotiated item limit"
    else if totalBytes > nsvMaxUncompressedBytes limits
      then Left "terrain delta exceeds the negotiated uncompressed-byte limit"
      else if streamEnvelopeFrameBytes (StreamOpenEnvelope open) > nsvMaxFrameBytes limits
        then Left "terrain delta stream_open exceeds the negotiated frame limit"
        else case filter ((> nsvMaxFrameBytes limits) . streamEnvelopeFrameBytes . StreamDataEnvelope) records of
          [] -> Right ()
          _ -> Left "terrain delta record exceeds the negotiated frame limit"

fragmentDelta :: NegotiatedStreamV1 -> ((TerrainSection, Int), DeltaValue) -> [(StreamRecordKey, BS.ByteString)]
fragmentDelta _ ((section, chunkId), DeltaRemoval) =
  [(StreamRecordKey section chunkId maxBound 0, BS.empty)]
fragmentDelta limits ((section, chunkId), DeltaUpdate raw) = go 0 0 raw
  where
    frameAllowance = if nsvMaxFrameBytes limits > 768 then nsvMaxFrameBytes limits - 768 else 1
    pieceSize = fromIntegral (max 1 (min (nsvReceiveWindowBytes limits)
      (frameAllowance * 3 `div` 4)))
    go _ _ remaining | BS.null remaining = []
    go part offset remaining =
      let (piece, rest) = BS.splitAt pieceSize remaining
      in (StreamRecordKey section chunkId part offset, piece)
        : go (part + 1) (offset + fromIntegral (BS.length piece)) rest

sendStreamEffectsLocked
  :: NegotiatedStreamV1 -> Transport -> MVar () -> [StreamEffect]
  -> IO (Either Text ())
sendStreamEffectsLocked streamLimits transport sendLock effects =
  withMVar sendLock $ \_ -> sendStreamEffects streamLimits transport effects

sendStreamEffectsLockedBounded
  :: NegotiatedStreamV1 -> Transport -> MVar () -> [StreamEffect]
  -> IO (Either Text ())
sendStreamEffectsLockedBounded streamLimits transport sendLock effects = do
  sent <- timeout 1000000
    (sendStreamEffectsLocked streamLimits transport sendLock effects)
  case sent of
    Just result -> pure result
    Nothing -> do
      -- Expiry cleanup must not wait forever for a peer that stopped reading.
      -- A timed-out framed write makes this connection unusable.
      closeTransport transport
      throwIO (SDKReceiveFailure TransportClosed)

sendStreamEffects
  :: NegotiatedStreamV1 -> Transport -> [StreamEffect] -> IO (Either Text ())
sendStreamEffects streamLimits transport = foldM sendOne (Right ())
  where
    sendOne (Left err) _ = pure (Left err)
    sendOne (Right ()) effect = case effect of
      GrantStreamWindow sid requestId credit -> send
        (StreamWindowEnvelope sid requestId credit)
      SendStreamCancel cancel -> send (StreamCancelEnvelope cancel)
      SendStreamError streamErr -> send (StreamErrorEnvelope streamErr)
      ParentRequestCancelled _ reason -> pure (Left reason)
      ConnectionMustClose reason -> pure (Left reason)
      _ -> pure (Right ())
    send frame = (sendStreamFrame streamLimits transport frame >> pure (Right ()))
      `catch` handleSendFailure
    handleSendFailure err@(SDKSendFailure _ _ _ _ transportErr) =
      case transportErr of
        -- Framing limits are validated before the transport writes a header,
        -- so the parent request may still receive a compact error response.
        TransportFramingError _ -> pure (Left (Text.pack (show err)))
        _ -> do
          -- An I/O failure may follow a partial header or payload write. The
          -- framing boundary is no longer trustworthy: close and abort the
          -- whole session rather than trying another frame on this transport.
          closeTransport transport
          throwIO err
    handleSendFailure (err :: SDKSessionError) = do
      closeTransport transport
      throwIO err

sendStreamFrameLocked
  :: NegotiatedStreamV1 -> Transport -> MVar () -> StreamEnvelope -> IO ()
sendStreamFrameLocked streamLimits transport sendLock frame =
  withMVar sendLock $ \_ -> sendStreamFrame streamLimits transport frame

sendStreamFrame :: NegotiatedStreamV1 -> Transport -> StreamEnvelope -> IO ()
sendStreamFrame streamLimits transport frame = do
  let encoded = Aeson.encode frame
      actual = fromIntegral (BL.length encoded)
      limit = fromIntegral (nsvMaxFrameBytes streamLimits)
  result <- sendLazyMessageWithLimit limit transport encoded
  case result of
    Right () -> pure ()
    Left err -> throwIO (SDKSendFailure (streamFrameType frame)
      (Just (streamRequestId frame)) actual (toInteger limit) err)

streamFrameType :: StreamEnvelope -> RPCMessageType
streamFrameType frame = case frame of
  StreamOpenEnvelope _ -> MsgStreamOpen
  StreamDataEnvelope _ -> MsgStreamData
  StreamWindowEnvelope _ _ _ -> MsgStreamWindow
  StreamEndEnvelope _ -> MsgStreamEnd
  StreamCancelEnvelope _ -> MsgStreamCancel
  StreamErrorEnvelope _ -> MsgStreamError

streamRequestId :: StreamEnvelope -> Word64
streamRequestId frame = case frame of
  StreamOpenEnvelope value -> soParentRequestId value
  StreamDataEnvelope value -> srParentRequestId value
  StreamWindowEnvelope _ value _ -> value
  StreamEndEnvelope value -> seParentRequestId value
  StreamCancelEnvelope value -> scParentRequestId value
  StreamErrorEnvelope value -> speParentRequestId value

writeUpdatesToSink :: TerrainDeltaSink -> [TerrainChunkUpdate] -> IO (Either Text ())
writeUpdatesToSink sink = foldM step (Right ())
  where
    step (Left err) _ = pure (Left err)
    step (Right ()) update = tdsWriteChunk sink update

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
      let geometryKeys = Set.fromList ["chunk_size", "hex_grid", "planet", "slice", "encoding"]
          supplied = Set.fromList (map Key.toText (KM.keys payload))
      in if supplied `Set.isSubsetOf` geometryKeys then Right ()
          else Left "host supplied terrain chunk fields when the resolved scope grants no terrain"
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
