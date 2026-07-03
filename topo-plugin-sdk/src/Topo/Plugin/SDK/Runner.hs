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
    -- * Manifest generation
  , pluginManifestFileName
  , generateManifest
  , writeManifest
  , writePluginManifest
  , writePluginManifestToDirectory
  ) where

import Control.Exception (SomeException, catch)
import Data.Aeson (Value(..), (.=), object)
import Data.List (nub)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Word (Word64)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeDirectory)
import System.IO (stderr, stdin, stdout)

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
  , encodeMessage
  , decodeMessage
  )
import Topo.Plugin.RPC.ExternalDataSource
  ( RPCExternalDataSourceGrantMessage(..)
  , RPCExternalDataSourceGrantRevocation(..)
  , RPCExternalDataSourceStatusRequest(..)
  , externalDataSourceStatusReportFromManifest
  )
import Topo.Plugin.RPC.Transport
  ( Transport(..)
  , sendMessage
  , recvMessage
  , closeTransport
  , connectPluginFromEnvironment
  )
import Topo.Hex (defaultHexGridMeta)
import Topo.Plugin.DataResource (DataResourceSchema(..), DataOperations(..))
import Topo.Plugin.RPC.DataService
  ( DataMutation(..), DataQuery(..), DataResourceErrorCode(..)
  , DataResourceFailure(..), QueryResource(..), QueryResult(..), DataRecord(..)
  , MutateResource(..), MutateResult(..)
  , dataResourceErrorCodeText, dataResourceErrorRPCCode, dataResourceFailureFromText
  )
import Topo.Plugin.SDK.Payload (decodeTerrainPayload)
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
-- 'pdCapabilities'.
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
-- Terrain writes are intentionally not inferred from the mere presence of a
-- simulation callback: ordinary simulation plugins usually update only their
-- owned overlay. Plugins that return terrain writes should request
-- 'CapWriteTerrain' explicitly via 'pdCapabilities'.
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
  transportResult <- connectPluginFromEnvironment (pdName pd) stdin stdout
  case transportResult of
    Left err -> do
      TextIO.hPutStrLn stderr ("SDK: transport error: " <> Text.pack (show err))
      pure ()
    Right transport -> do
      -- Build default param map
      let defaultParams = Map.fromList
            [ (paramName p, paramDefault p)
            | p <- pdParams pd
            ]
      runPluginSession pd transport defaultParams
      closeTransport transport

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
runPluginSession pd transport params = messageLoop pd transport params Nothing

-- | Main message loop.  Reads RPC envelopes and dispatches them.
messageLoop :: PluginDef -> Transport -> Map Text Value -> Maybe FilePath -> IO ()
messageLoop pd transport params worldPath = do
  result <- recvMessage transport
  case result of
    Left _err -> do
      -- Transport error (EOF, broken pipe) — shut down
      pure ()
    Right bs -> case decodeMessage bs of
      Left _decodeErr -> do
        -- Bad message — send error response and continue
        sendErrorResponse transport Nothing 1 "Failed to decode RPC message"
        messageLoop pd transport params worldPath
      Right envelope -> case envType envelope of

        MsgShutdown -> do
          -- Clean exit
          pure ()

        MsgHandshake -> do
          case Aeson.fromJSON (envPayload envelope) of
            Aeson.Error _ -> do
              sendErrorResponse transport (envRequestId envelope) 8 "Invalid handshake payload"
              messageLoop pd transport params worldPath
            Aeson.Success (hs :: Handshake) -> do
              let newWorldPath = fmap Text.unpack (hsWorldPath hs)
                  ack = HandshakeAck
                    { haProtocolVersion = currentProtocolVersion
                    , haDataDirectory   = fmap Text.pack (pdDataDirectory pd)
                    , haResources       = map drdSchema (pdDataResources pd)
                    }
                  ackEnvelope = RPCEnvelope
                    { envType    = MsgHandshakeAck
                    , envPayload = Aeson.toJSON ack
                    , envRequestId = envRequestId envelope
                    }
              _ <- sendMessage transport (encodeMessage ackEnvelope)
              messageLoop pd transport params newWorldPath

        MsgWorldChanged -> do
          case Aeson.fromJSON (envPayload envelope) of
            Aeson.Error _ ->
              messageLoop pd transport params worldPath
            Aeson.Success (wc :: WorldChanged) -> do
              let newWorldPath = fmap Text.unpack (wchWorldPath wc)
              messageLoop pd transport params newWorldPath

        MsgQueryResource -> do
          case Aeson.fromJSON (envPayload envelope) of
            Aeson.Error err -> do
              sendDataResourceErrorResponse transport (envRequestId envelope) SchemaValidationFailed ("Invalid query payload: " <> Text.pack err)
              messageLoop pd transport params worldPath
            Aeson.Success (qr :: QueryResource) -> do
              let resourceName = qrResource qr
              case findHandler resourceName (pdDataResources pd) of
                Nothing -> do
                  sendDataResourceErrorResponse transport (envRequestId envelope) ResourceNotFound ("Unknown resource: " <> resourceName)
                  messageLoop pd transport params worldPath
                Just drd -> case querySupportError (drdSchema drd) qr of
                  Just errMsg -> do
                    sendDataResourceErrorResponse transport (envRequestId envelope) QueryUnsupported errMsg
                    messageLoop pd transport params worldPath
                  Nothing -> case dhQuery (drdHandler drd) of
                    Nothing -> do
                      sendDataResourceErrorResponse transport (envRequestId envelope) OperationNotSupported ("Resource '" <> resourceName <> "' does not support queries")
                      messageLoop pd transport params worldPath
                    Just handler -> do
                      let ctx = makeDataContext params worldPath transport (envRequestId envelope)
                      runResult <- catch
                        (handler ctx (qrQuery qr))
                        (\e -> pure (Left (Text.pack (show (e :: SomeException)))))
                      case runResult of
                        Left errMsg ->
                          sendDataResourceFailureResponse transport (envRequestId envelope) (dataResourceFailureFromText errMsg)
                        Right result -> do
                          let resEnv = RPCEnvelope
                                { envType    = MsgQueryResult
                                , envPayload = Aeson.toJSON result
                                , envRequestId = envRequestId envelope
                                }
                          _ <- sendMessage transport (encodeMessage resEnv)
                          pure ()
                      messageLoop pd transport params worldPath

        MsgMutateResource -> do
          case Aeson.fromJSON (envPayload envelope) of
            Aeson.Error err -> do
              sendDataResourceErrorResponse transport (envRequestId envelope) SchemaValidationFailed ("Invalid mutate payload: " <> Text.pack err)
              messageLoop pd transport params worldPath
            Aeson.Success (mr :: MutateResource) -> do
              let resourceName = mrResource mr
              case findHandler resourceName (pdDataResources pd) of
                Nothing -> do
                  sendDataResourceErrorResponse transport (envRequestId envelope) ResourceNotFound ("Unknown resource: " <> resourceName)
                  messageLoop pd transport params worldPath
                Just drd -> case mutationSupportError (drdSchema drd) (mrMutation mr) of
                  Just errMsg -> do
                    sendDataResourceErrorResponse transport (envRequestId envelope) OperationNotSupported errMsg
                    messageLoop pd transport params worldPath
                  Nothing -> case dhMutate (drdHandler drd) of
                    Nothing -> do
                      sendDataResourceErrorResponse transport (envRequestId envelope) OperationNotSupported ("Resource '" <> resourceName <> "' does not support mutations")
                      messageLoop pd transport params worldPath
                    Just handler -> do
                      let ctx = makeDataContext params worldPath transport (envRequestId envelope)
                      runResult <- catch
                        (handler ctx (mrMutation mr))
                        (\e -> pure (Left (Text.pack (show (e :: SomeException)))))
                      case runResult of
                        Left errMsg ->
                          sendDataResourceFailureResponse transport (envRequestId envelope) (dataResourceFailureFromText errMsg)
                        Right result -> do
                          let resEnv = RPCEnvelope
                                { envType    = MsgMutateResult
                                , envPayload = Aeson.toJSON result
                                , envRequestId = envRequestId envelope
                                }
                          _ <- sendMessage transport (encodeMessage resEnv)
                          pure ()
                      messageLoop pd transport params worldPath

        MsgInvokeGenerator -> do
          case pdGenerator pd of
            Nothing -> do
              sendErrorResponse transport (envRequestId envelope) 2 "Plugin has no generator"
              messageLoop pd transport params worldPath
            Just gd -> do
              case Aeson.fromJSON (envPayload envelope) of
                Aeson.Error _ -> do
                  sendErrorResponse transport (envRequestId envelope) 6 "Invalid invoke_generator payload"
                  messageLoop pd transport params worldPath
                Aeson.Success (ig :: InvokeGenerator) -> do
                  let mergedParams = Map.union (igConfig ig) params
                  case makeTerrainContext
                    mergedParams
                    (igTerrain ig)
                    Nothing
                    Map.empty
                    (igSeed ig)
                    worldPath
                    (sendLogMessage transport (envRequestId envelope))
                    (sendProgress transport (envRequestId envelope)) of
                    Left err -> do
                      sendErrorResponse transport (envRequestId envelope) 6 ("Invalid terrain payload: " <> err)
                      messageLoop pd transport mergedParams worldPath
                    Right ctx -> do
                      runResult <- catch
                        (gdRun gd ctx)
                        (\e -> pure (Left (Text.pack (show (e :: SomeException)))))
                      case runResult of
                        Left errMsg ->
                          sendErrorResponse transport (envRequestId envelope) 3 errMsg
                        Right generatorResult ->
                          sendGeneratorResult transport (envRequestId envelope) generatorResult
                      messageLoop pd transport mergedParams worldPath

        MsgInvokeSimulation -> do
          case pdSimulation pd of
            Nothing -> do
              sendErrorResponse transport (envRequestId envelope) 4 "Plugin has no simulation"
              messageLoop pd transport params worldPath
            Just sd -> do
              case Aeson.fromJSON (envPayload envelope) of
                Aeson.Error _ -> do
                  sendErrorResponse transport (envRequestId envelope) 7 "Invalid invoke_simulation payload"
                  messageLoop pd transport params worldPath
                Aeson.Success (is' :: InvokeSimulation) -> do
                  let mergedParams = Map.union (isConfig is') params
                  case makeTerrainContext
                    mergedParams
                    (isTerrain is')
                    (Just (isOwnOverlay is'))
                    (valueObjectToMap (isOverlays is'))
                    0
                    worldPath
                    (sendLogMessage transport (envRequestId envelope))
                    (sendProgress transport (envRequestId envelope)) of
                    Left err -> do
                      sendErrorResponse transport (envRequestId envelope) 7 ("Invalid terrain payload: " <> err)
                      messageLoop pd transport mergedParams worldPath
                    Right ctx -> do
                      runResult <- catch
                        (sdTick sd ctx)
                        (\e -> pure (Left (Text.pack (show (e :: SomeException)))))
                      case runResult of
                        Left errMsg ->
                          sendErrorResponse transport (envRequestId envelope) 5 errMsg
                        Right simulationResult ->
                          sendSimulationResult transport (envRequestId envelope) simulationResult
                      messageLoop pd transport mergedParams worldPath

        MsgHeartbeat -> do
          let hbEnvelope = RPCEnvelope
                { envType = MsgHeartbeat
                , envPayload = Aeson.toJSON (Heartbeat { hbStatus = "ok" })
                , envRequestId = envRequestId envelope
                }
          _ <- sendMessage transport (encodeMessage hbEnvelope)
          messageLoop pd transport params worldPath

        MsgHealthCheck -> do
          let healthEnvelope = RPCEnvelope
                { envType = MsgHealthStatus
                , envPayload = Aeson.toJSON (HealthStatus
                    { hstHealthy = True
                    , hstMessage = "ok"
                    })
                , envRequestId = envRequestId envelope
                }
          _ <- sendMessage transport (encodeMessage healthEnvelope)
          messageLoop pd transport params worldPath

        MsgExternalDataSourceStatusRequest -> do
          case Aeson.fromJSON (envPayload envelope) of
            Aeson.Error err -> do
              sendErrorResponseIfCorrelated transport envelope 10 ("Invalid external data-source status payload: " <> Text.pack err)
              messageLoop pd transport params worldPath
            Aeson.Success (request :: RPCExternalDataSourceStatusRequest) -> do
              let statusEnvelope = RPCEnvelope
                    { envType = MsgExternalDataSourceStatus
                    , envPayload = Aeson.toJSON (externalDataSourceStatusReportFromManifest (generateManifest pd) request)
                    , envRequestId = envRequestId envelope
                    }
              sendResponseIfCorrelated transport envelope statusEnvelope
              messageLoop pd transport params worldPath

        MsgExternalDataSourceGrant -> do
          case Aeson.fromJSON (envPayload envelope) of
            Aeson.Error err -> do
              sendErrorResponseIfCorrelated transport envelope 11 ("Invalid external data-source grant payload: " <> Text.pack err)
              messageLoop pd transport params worldPath
            Aeson.Success (grant :: RPCExternalDataSourceGrantMessage) -> do
              handlerResult <- runExternalDataSourceGrantHandler pd grant
              case handlerResult of
                Left err -> sendErrorResponseIfCorrelated transport envelope 13 err
                Right () -> pure ()
              messageLoop pd transport params worldPath

        MsgExternalDataSourceRevoke -> do
          case Aeson.fromJSON (envPayload envelope) of
            Aeson.Error err -> do
              sendErrorResponseIfCorrelated transport envelope 12 ("Invalid external data-source revocation payload: " <> Text.pack err)
              messageLoop pd transport params worldPath
            Aeson.Success (revocation :: RPCExternalDataSourceGrantRevocation) -> do
              handlerResult <- runExternalDataSourceRevocationHandler pd revocation
              case handlerResult of
                Left err -> sendErrorResponseIfCorrelated transport envelope 14 err
                Right () -> pure ()
              messageLoop pd transport params worldPath

        -- Ignore unknown message types
        _ -> messageLoop pd transport params worldPath

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
-- Response helpers
------------------------------------------------------------------------

sendResponseIfCorrelated :: Transport -> RPCEnvelope -> RPCEnvelope -> IO ()
sendResponseIfCorrelated transport request response =
  case envRequestId request of
    Nothing -> pure ()
    Just _ -> do
      _ <- sendMessage transport (encodeMessage response)
      pure ()

sendErrorResponseIfCorrelated :: Transport -> RPCEnvelope -> Int -> Text -> IO ()
sendErrorResponseIfCorrelated transport request code msg =
  case envRequestId request of
    Nothing -> pure ()
    Just requestId -> sendErrorResponse transport (Just requestId) code msg

-- | Send an error response.
sendErrorResponse :: Transport -> Maybe Word64 -> Int -> Text -> IO ()
sendErrorResponse transport requestId code msg =
  sendErrorResponseWithDataResourceCode transport requestId code msg Nothing

sendDataResourceErrorResponse :: Transport -> Maybe Word64 -> DataResourceErrorCode -> Text -> IO ()
sendDataResourceErrorResponse transport requestId code msg =
  sendDataResourceFailureResponse transport requestId (DataResourceFailure code msg)

sendDataResourceFailureResponse :: Transport -> Maybe Word64 -> DataResourceFailure -> IO ()
sendDataResourceFailureResponse transport requestId failure =
  sendErrorResponseWithDataResourceCode
    transport
    requestId
    (dataResourceErrorRPCCode (drfCode failure))
    (drfMessage failure)
    (Just (drfCode failure))

sendErrorResponseWithDataResourceCode :: Transport -> Maybe Word64 -> Int -> Text -> Maybe DataResourceErrorCode -> IO ()
sendErrorResponseWithDataResourceCode transport requestId code msg mDataResourceCode = do
  let envelope = RPCEnvelope
        { envType = MsgError
        , envPayload = object $
            [ "code"    .= code
            , "message" .= msg
            ] <>
            [ "data_resource_error" .= dataResourceErrorCodeText dataCode
            | Just dataCode <- [mDataResourceCode]
            ]
        , envRequestId = requestId
        }
  _ <- sendMessage transport (encodeMessage envelope)
  pure ()

-- | Send a generator result payload.
sendGeneratorResult :: Transport -> Maybe Word64 -> GeneratorTickResult -> IO ()
sendGeneratorResult transport requestId result = do
  let envelope = RPCEnvelope
        { envType = MsgGeneratorResult
        , envPayload = Aeson.toJSON GeneratorResult
            { grTerrain  = gtrTerrain result
            , grOverlay  = gtrOverlay result
            , grMetadata = gtrMetadata result
            }
        , envRequestId = requestId
        }
  _ <- sendMessage transport (encodeMessage envelope)
  pure ()

-- | Send a simulation result payload.
sendSimulationResult :: Transport -> Maybe Word64 -> SimulationTickResult -> IO ()
sendSimulationResult transport requestId result = do
  let envelope = RPCEnvelope
        { envType = MsgSimulationResult
        , envPayload = Aeson.toJSON SimulationResult
            { srOverlay       = strOverlay result
            , srTerrainWrites = strTerrainWrites result
            }
        , envRequestId = requestId
        }
  _ <- sendMessage transport (encodeMessage envelope)
  pure ()

valueObjectToMap :: Value -> Map Text Value
valueObjectToMap (Object keyMap) =
  Map.fromList
    [ (Key.toText key, value)
    | (key, value) <- KM.toList keyMap
    ]
valueObjectToMap _ = Map.empty

makeTerrainContext
  :: Map Text Value
  -> Value
  -> Maybe Value
  -> Map Text Value
  -> Word64
  -> Maybe FilePath
  -> (Text -> IO ())
  -> (Text -> Double -> IO ())
  -> Either Text PluginContext
makeTerrainContext params terrainPayload ownOverlay overlays seed worldPath logFn progressFn = do
  world <- decodeTerrainPayload terrainPayload
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
sendLogMessage :: Transport -> Maybe Word64 -> Text -> IO ()
sendLogMessage transport requestId msg = do
  let envelope = RPCEnvelope
        { envType = MsgLog
        , envPayload = object
            [ "level"   .= ("info" :: Text)
            , "message" .= msg
            ]
        , envRequestId = requestId
        }
  _ <- sendMessage transport (encodeMessage envelope)
  pure ()

-- | Send progress to the host.
--
-- SDK progress fractions are absolute for the current invocation. Finite
-- values are clamped to [0,1]; non-finite values are mapped defensively before
-- JSON encoding so plugins never emit invalid JSON numbers.
sendProgress :: Transport -> Maybe Word64 -> Text -> Double -> IO ()
sendProgress transport requestId msg fraction = do
  let envelope = RPCEnvelope
        { envType = MsgProgress
        , envPayload = Aeson.toJSON (PluginProgress msg (sanitizeProgressFraction fraction))
        , envRequestId = requestId
        }
  _ <- sendMessage transport (encodeMessage envelope)
  pure ()

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

querySupportError :: DataResourceSchema -> QueryResource -> Maybe Text
querySupportError schema qr
  | hasPageRequest qr && not (doPage ops) =
      Just ("Resource '" <> name <> "' does not support paged queries")
  | otherwise = case qrQuery qr of
      QueryAll
        | doList ops -> Nothing
        | otherwise -> Just ("Resource '" <> name <> "' does not support list queries")
      QueryByKey _
        | doGet ops -> Nothing
        | otherwise -> Just ("Resource '" <> name <> "' does not support get queries")
      QueryByHex _ _
        | doQueryByHex ops -> Nothing
        | otherwise -> Just ("Resource '" <> name <> "' does not support hex queries")
      QueryByField _ _
        | doQueryByField ops -> Nothing
        | otherwise -> Just ("Resource '" <> name <> "' does not support field queries")
  where
    ops = drsOperations schema
    name = drsName schema
    hasPageRequest request = qrPageSize request /= Nothing || qrPageOffset request /= Nothing

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
makeDataContext :: Map Text Value -> Maybe FilePath -> Transport -> Maybe Word64 -> PluginContext
makeDataContext params worldPath transport requestId = PluginContext
  { pcWorld      = stubWorld
  , pcParams     = params
  , pcTerrain    = Object mempty
  , pcOwnOverlay = Nothing
  , pcOverlays   = Map.empty
  , pcSeed       = 0
  , pcLog        = sendLogMessage transport requestId
  , pcProgress   = sendProgress transport requestId
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
