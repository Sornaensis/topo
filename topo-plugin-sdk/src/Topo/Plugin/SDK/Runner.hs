{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

-- | Plugin process runner.
--
-- This module provides 'runPlugin', the entry point for a plugin
-- executable.  It:
--
-- 1. Generates a manifest from the 'PluginDef'
-- 2. Writes it to @manifest.json@ in the plugin's working directory
-- 3. Connects to the host-created named pipe (Windows) or Unix socket
-- 4. Enters a message loop that dispatches incoming RPC messages
--    to the appropriate callback
-- 5. Shuts down cleanly when the host sends a @shutdown@ message
--
-- === Usage
--
-- @
-- import Topo.Plugin.SDK
--
-- main :: IO ()
-- main = runPlugin myPluginDef
-- @
module Topo.Plugin.SDK.Runner
  ( -- * Entry point
    runPlugin
  , runPluginSession
    -- * Manifest generation
  , generateManifest
  , writeManifest
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
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
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
  , currentProtocolVersion
  , encodeMessage
  , decodeMessage
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

-- | Write the manifest JSON to a file.
writeManifest :: FilePath -> RPCManifest -> IO ()
writeManifest path manifest =
  BL.writeFile path (Aeson.encode manifest)

------------------------------------------------------------------------
-- Entry point
------------------------------------------------------------------------

-- | Run a plugin.
--
-- This is the standard entry point for a plugin executable.
-- It generates and writes a manifest, connects to the host-created
-- production endpoint advertised in the TOPO_PLUGIN_* environment,
-- then enters the length-prefixed RPC message loop. If no endpoint
-- environment is present, stdin/stdout compatibility is available only
-- when @TOPO_PLUGIN_STDIO_COMPAT=1@ is explicitly set for a test or
-- development harness.
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
  -- Write manifest
  cwd <- getCurrentDirectory
  let manifest = generateManifest pd
      manifestPath = cwd </> "manifest.json"
  writeManifest manifestPath manifest

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
                    (sendLogMessage transport (envRequestId envelope)) of
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
                    (sendLogMessage transport (envRequestId envelope)) of
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

        -- Ignore unknown message types
        _ -> messageLoop pd transport params worldPath

------------------------------------------------------------------------
-- Response helpers
------------------------------------------------------------------------

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
  -> Either Text PluginContext
makeTerrainContext params terrainPayload ownOverlay overlays seed worldPath logFn = do
  world <- decodeTerrainPayload terrainPayload
  Right PluginContext
    { pcWorld = world
    , pcParams = params
    , pcTerrain = terrainPayload
    , pcOwnOverlay = ownOverlay
    , pcOverlays = overlays
    , pcSeed = seed
    , pcLog = logFn
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
sendProgress :: Transport -> Maybe Word64 -> Text -> Double -> IO ()
sendProgress transport requestId msg fraction = do
  let envelope = RPCEnvelope
        { envType = MsgProgress
        , envPayload = object
            [ "message"  .= msg
            , "fraction" .= fraction
            ]
        , envRequestId = requestId
        }
  _ <- sendMessage transport (encodeMessage envelope)
  pure ()

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
