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
-- 3. Listens on a named pipe (Windows) or Unix socket
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
  , RPCOverlayDecl(..)
  , RPCParamSpec(..)
  , RPCParamType(..)
  , RPCSimulationDecl(..)
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
  , currentProtocolVersion
  , encodeMessage
  , decodeMessage
  )
import Topo.Plugin.RPC.Transport
  ( Transport(..)
  , sendMessage
  , recvMessage
  , closeTransport
  , connectPlugin
  )
import Topo.Hex (defaultHexGridMeta)
import Topo.Plugin.DataResource (DataResourceSchema(..), DataOperations(..))
import Topo.Plugin.RPC.DataService
  ( QueryResource(..), QueryResult(..), DataRecord(..)
  , MutateResource(..), MutateResult(..)
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
-- Capabilities are inferred from the plugin's declared generator
-- and simulation definitions.
generateManifest :: PluginDef -> RPCManifest
generateManifest pd = RPCManifest
  { rmName          = pdName pd
  , rmVersion       = pdVersion pd
  , rmDescription   = pdName pd <> " v" <> pdVersion pd
  , rmGenerator     = fmap toGenDecl (pdGenerator pd)
  , rmSimulation    = fmap toSimDecl (pdSimulation pd)
  , rmOverlay       = fmap (\f -> RPCOverlayDecl (Text.pack f)) (pdSchemaFile pd)
  , rmCapabilities  = inferCapabilities pd
  , rmParameters    = map toRPCParamSpec (pdParams pd)
  , rmDataResources = map drdSchema (pdDataResources pd)
  , rmDataDirectory = fmap Text.pack (pdDataDirectory pd)
  }
  where
    toGenDecl gd = RPCGeneratorDecl
      { rgdInsertAfter = gdInsertAfter gd
      , rgdRequires    = gdRequires gd
      }
    toSimDecl sd = RPCSimulationDecl
      { rsdDependencies = sdDependencies sd
      }

-- | Infer capabilities from the plugin definition.
inferCapabilities :: PluginDef -> [RPCCapability]
inferCapabilities pd = concat
  [ [CapLog]
  , [CapReadTerrain | hasGen || hasSim]
  , [CapWriteTerrain | hasSim]
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
-- It generates and writes a manifest, then enters a message loop
-- reading from stdin and writing to stdout using the length-prefixed
-- RPC protocol.
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

  -- Connect via stdin/stdout (host launches plugin and connects pipes)
  transportResult <- connectPlugin (pdName pd) stdin stdout
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
        sendErrorResponse transport 1 "Failed to decode RPC message"
        messageLoop pd transport params worldPath
      Right envelope -> case envType envelope of

        MsgShutdown -> do
          -- Clean exit
          pure ()

        MsgHandshake -> do
          case Aeson.fromJSON (envPayload envelope) of
            Aeson.Error _ -> do
              sendErrorResponse transport 8 "Invalid handshake payload"
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
              sendErrorResponse transport 9 ("Invalid query payload: " <> Text.pack err)
              messageLoop pd transport params worldPath
            Aeson.Success (qr :: QueryResource) -> do
              let resourceName = qrResource qr
              case findHandler resourceName (pdDataResources pd) of
                Nothing -> do
                  sendErrorResponse transport 9 ("Unknown resource: " <> resourceName)
                  messageLoop pd transport params worldPath
                Just drd -> case dhQuery (drdHandler drd) of
                  Nothing -> do
                    sendErrorResponse transport 9 ("Resource '" <> resourceName <> "' does not support queries")
                    messageLoop pd transport params worldPath
                  Just handler -> do
                    let ctx = makeDataContext params worldPath transport
                    runResult <- catch
                      (handler ctx (qrQuery qr))
                      (\e -> pure (Left (Text.pack (show (e :: SomeException)))))
                    case runResult of
                      Left errMsg ->
                        sendErrorResponse transport 9 errMsg
                      Right result -> do
                        let resEnv = RPCEnvelope
                              { envType    = MsgQueryResult
                              , envPayload = Aeson.toJSON result
                              }
                        _ <- sendMessage transport (encodeMessage resEnv)
                        pure ()
                    messageLoop pd transport params worldPath

        MsgMutateResource -> do
          case Aeson.fromJSON (envPayload envelope) of
            Aeson.Error err -> do
              sendErrorResponse transport 10 ("Invalid mutate payload: " <> Text.pack err)
              messageLoop pd transport params worldPath
            Aeson.Success (mr :: MutateResource) -> do
              let resourceName = mrResource mr
              case findHandler resourceName (pdDataResources pd) of
                Nothing -> do
                  sendErrorResponse transport 10 ("Unknown resource: " <> resourceName)
                  messageLoop pd transport params worldPath
                Just drd -> case dhMutate (drdHandler drd) of
                  Nothing -> do
                    sendErrorResponse transport 10 ("Resource '" <> resourceName <> "' does not support mutations")
                    messageLoop pd transport params worldPath
                  Just handler -> do
                    let ctx = makeDataContext params worldPath transport
                    runResult <- catch
                      (handler ctx (mrMutation mr))
                      (\e -> pure (Left (Text.pack (show (e :: SomeException)))))
                    case runResult of
                      Left errMsg ->
                        sendErrorResponse transport 10 errMsg
                      Right result -> do
                        let resEnv = RPCEnvelope
                              { envType    = MsgMutateResult
                              , envPayload = Aeson.toJSON result
                              }
                        _ <- sendMessage transport (encodeMessage resEnv)
                        pure ()
                    messageLoop pd transport params worldPath

        MsgInvokeGenerator -> do
          case pdGenerator pd of
            Nothing -> do
              sendErrorResponse transport 2 "Plugin has no generator"
              messageLoop pd transport params worldPath
            Just gd -> do
              case Aeson.fromJSON (envPayload envelope) of
                Aeson.Error _ -> do
                  sendErrorResponse transport 6 "Invalid invoke_generator payload"
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
                    (sendLogMessage transport) of
                    Left err -> do
                      sendErrorResponse transport 6 ("Invalid terrain payload: " <> err)
                      messageLoop pd transport mergedParams worldPath
                    Right ctx -> do
                      runResult <- catch
                        (gdRun gd ctx)
                        (\e -> pure (Left (Text.pack (show (e :: SomeException)))))
                      case runResult of
                        Left errMsg ->
                          sendErrorResponse transport 3 errMsg
                        Right generatorResult ->
                          sendGeneratorResult transport generatorResult
                      messageLoop pd transport mergedParams worldPath

        MsgInvokeSimulation -> do
          case pdSimulation pd of
            Nothing -> do
              sendErrorResponse transport 4 "Plugin has no simulation"
              messageLoop pd transport params worldPath
            Just sd -> do
              case Aeson.fromJSON (envPayload envelope) of
                Aeson.Error _ -> do
                  sendErrorResponse transport 7 "Invalid invoke_simulation payload"
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
                    (sendLogMessage transport) of
                    Left err -> do
                      sendErrorResponse transport 7 ("Invalid terrain payload: " <> err)
                      messageLoop pd transport mergedParams worldPath
                    Right ctx -> do
                      runResult <- catch
                        (sdTick sd ctx)
                        (\e -> pure (Left (Text.pack (show (e :: SomeException)))))
                      case runResult of
                        Left errMsg ->
                          sendErrorResponse transport 5 errMsg
                        Right simulationResult ->
                          sendSimulationResult transport simulationResult
                      messageLoop pd transport mergedParams worldPath

        -- Ignore unknown message types
        _ -> messageLoop pd transport params worldPath

------------------------------------------------------------------------
-- Response helpers
------------------------------------------------------------------------

-- | Send an error response.
sendErrorResponse :: Transport -> Int -> Text -> IO ()
sendErrorResponse transport code msg = do
  let envelope = RPCEnvelope
        { envType = MsgError
        , envPayload = object
            [ "code"    .= code
            , "message" .= msg
            ]
        }
  _ <- sendMessage transport (encodeMessage envelope)
  pure ()

-- | Send a generator result payload.
sendGeneratorResult :: Transport -> GeneratorTickResult -> IO ()
sendGeneratorResult transport result = do
  let envelope = RPCEnvelope
        { envType = MsgGeneratorResult
        , envPayload = Aeson.toJSON GeneratorResult
            { grTerrain  = gtrTerrain result
            , grOverlay  = gtrOverlay result
            , grMetadata = gtrMetadata result
            }
        }
  _ <- sendMessage transport (encodeMessage envelope)
  pure ()

-- | Send a simulation result payload.
sendSimulationResult :: Transport -> SimulationTickResult -> IO ()
sendSimulationResult transport result = do
  let envelope = RPCEnvelope
        { envType = MsgSimulationResult
        , envPayload = Aeson.toJSON SimulationResult
            { srOverlay       = strOverlay result
            , srTerrainWrites = strTerrainWrites result
            }
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
sendLogMessage :: Transport -> Text -> IO ()
sendLogMessage transport msg = do
  let envelope = RPCEnvelope
        { envType = MsgLog
        , envPayload = object
            [ "level"   .= ("info" :: Text)
            , "message" .= msg
            ]
        }
  _ <- sendMessage transport (encodeMessage envelope)
  pure ()

-- | Send progress to the host.
sendProgress :: Transport -> Text -> Double -> IO ()
sendProgress transport msg fraction = do
  let envelope = RPCEnvelope
        { envType = MsgProgress
        , envPayload = object
            [ "message"  .= msg
            , "fraction" .= fraction
            ]
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

-- | Build a 'PluginContext' for data service callbacks.
--
-- Data service handlers don't receive a terrain payload, so the
-- context uses a stub world and empty terrain.
makeDataContext :: Map Text Value -> Maybe FilePath -> Transport -> PluginContext
makeDataContext params worldPath transport = PluginContext
  { pcWorld      = stubWorld
  , pcParams     = params
  , pcTerrain    = Object mempty
  , pcOwnOverlay = Nothing
  , pcOverlays   = Map.empty
  , pcSeed       = 0
  , pcLog        = sendLogMessage transport
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
