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
    -- * Manifest generation
  , generateManifest
  , writeManifest
  ) where

import Control.Exception (SomeException, catch)
import Data.Aeson (Value(..), (.=), object)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Word (Word64)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import System.IO (hFlush, stderr, stdin, stdout)

import Topo.Plugin.RPC.Manifest
  ( RPCCapability(..)
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
  , encodeMessage
  , decodeMessage
  )
import Topo.Plugin.RPC.Transport
  ( Transport(..)
  , TransportError
  , sendMessage
  , recvMessage
  , closeTransport
  , connectPlugin
  )
import Topo.Hex (defaultHexGridMeta)
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
  { rmName         = pdName pd
  , rmVersion      = pdVersion pd
  , rmDescription  = pdName pd <> " v" <> pdVersion pd
  , rmGenerator    = fmap toGenDecl (pdGenerator pd)
  , rmSimulation   = fmap toSimDecl (pdSimulation pd)
  , rmOverlay      = fmap (\f -> RPCOverlayDecl (Text.pack f)) (pdSchemaFile pd)
  , rmCapabilities = inferCapabilities pd
  , rmParameters   = map toRPCParamSpec (pdParams pd)
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
  [ [CapRPCLog]
  , [CapReadTerrain | hasGen || hasSim]
  , [CapWriteTerrain | hasSim]
  , [CapReadOverlay | hasSim]
  , [CapWriteOverlay | hasSim]
  ]
  where
    hasGen = case pdGenerator pd of { Just _ -> True; Nothing -> False }
    hasSim = case pdSimulation pd of { Just _ -> True; Nothing -> False }

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
      messageLoop pd transport defaultParams
      closeTransport transport

-- | Main message loop.  Reads RPC envelopes and dispatches them.
messageLoop :: PluginDef -> Transport -> Map Text Value -> IO ()
messageLoop pd transport params = do
  result <- recvMessage transport
  case result of
    Left _err -> do
      -- Transport error (EOF, broken pipe) — shut down
      pure ()
    Right bs -> case decodeMessage bs of
      Left _decodeErr -> do
        -- Bad message — send error response and continue
        sendErrorResponse transport 1 "Failed to decode RPC message"
        messageLoop pd transport params
      Right envelope -> case envType envelope of

        MsgShutdown -> do
          -- Clean exit
          pure ()

        MsgInvokeGenerator -> do
          case pdGenerator pd of
            Nothing -> do
              sendErrorResponse transport 2 "Plugin has no generator"
              messageLoop pd transport params
            Just gd -> do
              -- Parse the invocation payload
              let mergedParams = case Aeson.fromJSON (envPayload envelope) of
                    Aeson.Success (ig :: InvokeGenerator) ->
                      Map.union (igConfig ig) params
                    Aeson.Error _ -> params
                  ctx = PluginContext
                    { pcWorld  = stubWorld  -- Populated by host payload
                    , pcParams = mergedParams
                    , pcSeed   = extractSeed envelope
                    , pcLog    = sendLogMessage transport
                    }
              runResult <- catch
                (gdRun gd ctx)
                (\e -> pure (Left (Text.pack (show (e :: SomeException)))))
              case runResult of
                Left errMsg ->
                  sendErrorResponse transport 3 errMsg
                Right () ->
                  sendGeneratorResult transport
              messageLoop pd transport mergedParams

        MsgInvokeSimulation -> do
          case pdSimulation pd of
            Nothing -> do
              sendErrorResponse transport 4 "Plugin has no simulation"
              messageLoop pd transport params
            Just sd -> do
              let mergedParams = case Aeson.fromJSON (envPayload envelope) of
                    Aeson.Success (is' :: InvokeSimulation) ->
                      Map.union (isConfig is') params
                    Aeson.Error _ -> params
                  ctx = PluginContext
                    { pcWorld  = stubWorld
                    , pcParams = mergedParams
                    , pcSeed   = 0
                    , pcLog    = sendLogMessage transport
                    }
              runResult <- catch
                (sdTick sd ctx)
                (\e -> pure (Left (Text.pack (show (e :: SomeException)))))
              case runResult of
                Left errMsg ->
                  sendErrorResponse transport 5 errMsg
                Right () ->
                  sendSimulationResult transport
              messageLoop pd transport mergedParams

        -- Ignore unknown message types
        _ -> messageLoop pd transport params

------------------------------------------------------------------------
-- Response helpers
------------------------------------------------------------------------

-- | Extract seed from an invoke_generator payload.
extractSeed :: RPCEnvelope -> Word64
extractSeed env = case Aeson.fromJSON (envPayload env) of
  Aeson.Success (ig :: InvokeGenerator) -> igSeed ig
  Aeson.Error _ -> 0

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

-- | Send a generator result (stub — empty terrain).
sendGeneratorResult :: Transport -> IO ()
sendGeneratorResult transport = do
  let envelope = RPCEnvelope
        { envType = MsgGeneratorResult
        , envPayload = Aeson.toJSON GeneratorResult
            { grTerrain  = object []
            , grOverlay  = Nothing
            , grMetadata = Nothing
            }
        }
  _ <- sendMessage transport (encodeMessage envelope)
  pure ()

-- | Send a simulation result (stub — empty overlay).
sendSimulationResult :: Transport -> IO ()
sendSimulationResult transport = do
  let envelope = RPCEnvelope
        { envType = MsgSimulationResult
        , envPayload = Aeson.toJSON SimulationResult
            { srOverlay       = object []
            , srTerrainWrites = Nothing
            }
        }
  _ <- sendMessage transport (encodeMessage envelope)
  pure ()

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
