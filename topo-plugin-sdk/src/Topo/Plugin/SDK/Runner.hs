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
import Data.List (nub)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, atomicModifyIORef', newIORef)
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
  , handshakeAuthProof
  , encodeMessage
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
  , rplMaxFrameSizeBytes
  , sendMessageWithLimit
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
  ( DataMutation(..), DataResourceErrorCode(..)
  , DataResourceFailure(..), QueryResource(..), QueryResult(..), DataRecord(..)
  , MutateResource(..), MutateResult(..)
  , dataResourceErrorCodeText, dataResourceErrorRPCCode, dataResourceFailureFromText
  )
import Topo.Plugin.SDK.Payload (decodeTerrainPayloadWithLimits)
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
      { rmrProtocolMin = currentProtocolVersion
      , rmrProtocolMax = currentProtocolVersion
      , rmrTopoMin = pdRuntimeTopoMin pd
      , rmrTopoMax = pdRuntimeTopoMax pd
      }
  , rmDescription   = sdkDescription pd
  , rmUiHints       = sdkUiHints pd
  , rmGenerator     = fmap toGenDecl (pdGenerator pd)
  , rmSimulation    = fmap toSimDecl (pdSimulation pd)
  , rmOverlay       = fmap (\f -> RPCOverlayDecl (Text.pack f)) (pdSchemaFile pd)
  , rmCapabilities  = sdkCapabilities pd
  , rmParameters    = map toRPCParamSpec (pdParams pd)
  , rmDataResources = map drdSchema (pdDataResources pd)
  , rmDataDirectory = fmap Text.pack (pdDataDirectory pd)
  , rmExternalDataSources = pdExternalDataSources pd
  , rmExternalDataSourceRefs = pdExternalDataSourceRefs pd
  , rmStartPolicy   = pdStartPolicy pd
  }
  where
    toGenDecl gd = RPCGeneratorDecl
      { rgdInsertAfter = gdInsertAfter gd
      , rgdRequires    = gdRequires gd
      }
    toSimDecl sd = RPCSimulationDecl
      { rsdDependencies = sdDependencies sd
      , rsdSchedule = maybe defaultScheduleDecl id (sdSchedule sd)
      }

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
  , [CapReadTerrain | hasGen || hasSim]
  , [CapReadOverlay | hasSim]
  , [CapWriteOverlay | hasSim]
  , [CapDataRead | hasData]
  , [CapDataWrite | hasData && hasDataWriteOps]
  ]
  where
    hasGen = case pdGenerator pd of { Just _ -> True; Nothing -> False }
    hasSim = case pdSimulation pd of { Just _ -> True; Nothing -> False }
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
  messageLoop limits pd transport params Nothing
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
messageLoop :: RPCPayloadLimits -> PluginDef -> Transport -> Map Text Value -> Maybe FilePath -> IO ()
messageLoop limits pd transport params worldPath = do
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
      messageLoop limits pd transport nextParams nextWorldPath
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
              ack = HandshakeAck
                { haProtocolVersion = currentProtocolVersion
                , haDataDirectory = fmap Text.pack (pdDataDirectory pd)
                , haResources = map drdSchema (pdDataResources pd)
                , haSessionId = fst <$> mAuthProof
                , haAuthProof = snd <$> mAuthProof
                }
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
              Nothing -> case dhQuery (drdHandler drd) of
                Nothing -> do
                  sendDataResourceErrorResponse limits transport (envRequestId envelope)
                    OperationNotSupported ("Resource '" <> qrResource qr <> "' does not support queries")
                  next params worldPath
                Just handler -> do
                  let ctx = makeDataContext limits params worldPath transport (envRequestId envelope)
                  runResult <- catchPluginResult (handler ctx (qrQuery qr))
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
              Nothing -> case dhMutate (drdHandler drd) of
                Nothing -> do
                  sendDataResourceErrorResponse limits transport (envRequestId envelope)
                    OperationNotSupported ("Resource '" <> mrResource mr <> "' does not support mutations")
                  next params worldPath
                Just handler -> do
                  let ctx = makeDataContext limits params worldPath transport (envRequestId envelope)
                  runResult <- catchPluginResult (handler ctx (mrMutation mr))
                  case runResult of
                    Left errMsg -> sendDataResourceFailureResponse limits transport
                      (envRequestId envelope) (dataResourceFailureFromText errMsg)
                    Right mutationResult -> sendSDKEnvelope limits transport RPCEnvelope
                      { envType = MsgMutateResult
                      , envPayload = Aeson.toJSON mutationResult
                      , envRequestId = envRequestId envelope
                      }
                  next params worldPath

      MsgInvokeGenerator -> case pdGenerator pd of
        Nothing -> do
          sendErrorResponse limits transport (envRequestId envelope) 2 "Plugin has no generator"
          next params worldPath
        Just gd -> case Aeson.fromJSON (envPayload envelope) of
          Aeson.Error err -> do
            sendErrorResponse limits transport (envRequestId envelope) 6
              ("Invalid invoke_generator payload: " <> Text.pack err)
            next params worldPath
          Aeson.Success (ig :: InvokeGenerator) -> do
            let mergedParams = Map.union (igConfig ig) params
            preserveStateOnRejected mergedParams worldPath $
              case makeTerrainContext limits mergedParams (igTerrain ig) Nothing Map.empty
                (igSeed ig) worldPath
                (sendLogMessage limits transport (envRequestId envelope))
                (sendProgress limits transport (envRequestId envelope)) of
                Left err -> do
                  sendErrorResponse limits transport (envRequestId envelope) 6
                    ("Invalid terrain payload: " <> err)
                  next mergedParams worldPath
                Right ctx -> do
                  runResult <- catchPluginResult (gdRun gd ctx)
                  case runResult of
                    Left errMsg -> sendErrorResponse limits transport (envRequestId envelope) 3 errMsg
                    Right generatorResult -> sendGeneratorResult limits transport
                      (envRequestId envelope) generatorResult
                  next mergedParams worldPath

      MsgInvokeSimulation -> case pdSimulation pd of
        Nothing -> do
          sendErrorResponse limits transport (envRequestId envelope) 4 "Plugin has no simulation"
          next params worldPath
        Just sd -> case Aeson.fromJSON (envPayload envelope) of
          Aeson.Error err -> do
            sendErrorResponse limits transport (envRequestId envelope) 7
              ("Invalid invoke_simulation payload: " <> Text.pack err)
            next params worldPath
          Aeson.Success (is' :: InvokeSimulation) -> do
            let mergedParams = Map.union (isConfig is') params
            preserveStateOnRejected mergedParams worldPath $
              case makeTerrainContext limits mergedParams (isTerrain is') (Just (isOwnOverlay is'))
                (valueObjectToMap (isOverlays is')) 0 worldPath
                (sendLogMessage limits transport (envRequestId envelope))
                (sendProgress limits transport (envRequestId envelope)) of
                Left err -> do
                  sendErrorResponse limits transport (envRequestId envelope) 7
                    ("Invalid terrain payload: " <> err)
                  next mergedParams worldPath
                Right ctx -> do
                  runResult <- catchPluginResult (sdTick sd ctx)
                  case runResult of
                    Left errMsg -> sendErrorResponse limits transport (envRequestId envelope) 5 errMsg
                    Right simulationResult -> sendSimulationResult limits transport
                      (envRequestId envelope) simulationResult
                  next mergedParams worldPath

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
  let encoded = encodeMessage envelope
      actual = BS.length encoded
      limitInt = fromIntegral (rplMaxFrameSizeBytes limits)
      limit = toInteger limitInt
  result <- sendMessageWithLimit limitInt transport encoded
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
              compactBytes = encodeMessage compactError
          compactResult <- sendMessageWithLimit limitInt transport compactBytes
          case compactResult of
            Right () -> throwIO
              (SDKRequestPayloadRejected (envType envelope) requestId actual limit)
            Left compactErr -> throwIO
              (SDKSendFailure MsgError (Just requestId) (BS.length compactBytes) limit compactErr)
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

-- | Build a 'PluginContext' for data service callbacks.
--
-- Data service handlers don't receive a terrain payload, so the
-- context uses a stub world and empty terrain.
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
