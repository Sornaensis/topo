{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | High-level plugin RPC client API.
--
-- This module provides 'RPCConnection' (an active plugin session)
-- and functions to invoke generator stages and simulation ticks
-- over the transport layer.  It also provides constructors that
-- produce 'PipelineStage' and 'SimNode' values from an RPC
-- connection, integrating plugins into the generator pipeline and
-- simulation DAG respectively.
--
-- = Lifecycle
--
-- 1. Parse the plugin manifest ('parseManifestFile')
-- 2. Launch the plugin process with a host-created transport endpoint
-- 3. Accept the plugin connection to get a 'Transport'
-- 4. Create an 'RPCConnection' with 'newRPCConnection'
-- 5. Use 'rpcGeneratorStage' and\/or 'rpcSimNode' to integrate
-- 6. On shutdown, call 'rpcShutdown' then 'closeTransport'
module Topo.Plugin.RPC
  ( -- * Connection
    RPCConnection(..)
  , RPCSession
  , newRPCConnection
    -- * Handshake
  , HandshakeAuthChallenge(..)
  , performHandshake
  , performHandshakeWithAuth
  , sendWorldChanged
  , sendHeartbeat
  , checkHealth
    -- * External data-source grants/status
  , sendExternalDataSourceGrant
  , sendExternalDataSourceGrantRevocation
  , revokeExternalDataSourceGrant
  , requestExternalDataSourceStatus
  , checkExternalDataSourceStatus
    -- * Invocation
  , invokeGenerator
  , invokeSimulation
  , rpcShutdown
    -- * Data service
  , queryResource
  , mutateResource
    -- * Pipeline / DAG integration
  , rpcGeneratorStage
  , rpcSimNode
    -- * Terrain payload helpers
  , terrainWorldToPayload
  , terrainWorldToCompletePayload
  , decodeTerrainWritesValue
  , applyGeneratorTerrainValue
  , encodeBase64Text
  , decodeBase64Text
    -- * Errors
  , RPCError(..)
  , rpcErrorText
  , rpcErrorDataResourceFailure
    -- * Re-exports
  , module Topo.Plugin.RPC.Manifest
  , module Topo.Plugin.RPC.Transport
  , module Topo.Plugin.RPC.Protocol
  , module Topo.Plugin.RPC.DataService
  , module Topo.Plugin.RPC.ExternalDataSource
  ) where

import Control.Concurrent
  ( MVar
  , ThreadId
  , forkIO
  , modifyMVar
  , modifyMVar_
  , newEmptyMVar
  , newMVar
  , takeMVar
  , tryPutMVar
  )
import Control.Exception (SomeException, mask, onException, try)
import Control.Monad (forM_, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import Data.Char (chr, ord)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)

import qualified Data.Aeson as Aeson
import Data.Aeson (Value(..), (.:), (.:?), (.=), object)
import qualified Data.Aeson.Types as AesonTypes (Parser, parseMaybe)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import System.Timeout (timeout)

import Topo.Overlay (Overlay(..), insertOverlay, lookupOverlay)
import Topo.Overlay.JSON (overlayFromJSON, overlayToJSON)
import Topo.Pipeline (PipelineStage(..))
import Topo.Pipeline.Stage (StageId(..))
import Topo.Plugin (Capability(..), PluginEnv(..), PluginError(..), getWorld, liftTopo, logInfo, putWorld)
import Topo.Calendar (CalendarDate(..), WorldTime(..))
import Topo.Simulation
  ( SimNode(..)
  , SimNodeId(..)
  , SimContext(..)
  , TerrainWrites(..)
  , applyTerrainWrites
  , defaultScheduleDecl
  , emptyTerrainWrites
  )
import qualified Topo.Types
import qualified Topo.World

import Topo.Plugin.RPC.Manifest
import qualified Topo.Plugin.RPC.Payload as Payload
import Topo.Plugin.RPC.Protocol
import Topo.Plugin.RPC.Transport
import Topo.Plugin.RPC.DataService
import Topo.Plugin.RPC.ExternalDataSource
import Topo.Plugin.DataResource (DataResourceSchema)

------------------------------------------------------------------------
-- Errors
------------------------------------------------------------------------

-- | Errors from the RPC layer.
data RPCError
  = RPCTransportError !TransportError
    -- ^ Underlying transport failure.
  | RPCProtocolError !Text
    -- ^ Invalid message format or unexpected message type.
  | RPCPluginError !Int !Text
    -- ^ Plugin-reported error with code and message.
  | RPCDataResourceError !DataResourceErrorCode !Text
    -- ^ Standardized data-resource error reported by a plugin.
  | RPCTimeout !Text
    -- ^ Plugin did not respond in time.
  deriving (Eq, Show)

-- | Shared session state for request correlation over one transport.
data RPCSession = RPCSession
  { rpcsWriteLock :: !(MVar ())
  , rpcsPending :: !(MVar (Map Word64 RPCPending))
  , rpcsNextRequestId :: !(IORef Word64)
  , rpcsReceiverThread :: !(MVar (Maybe ThreadId))
  }

-- | A request awaiting a correlated response.
data RPCPending = RPCPending
  { rpResult :: !(MVar (Either RPCError RPCEnvelope))
  , rpOnProgress :: !(PluginProgress -> IO ())
  , rpOnLog :: !(PluginLog -> IO ())
  }

newRPCSession :: IO RPCSession
newRPCSession = do
  writeLock <- newMVar ()
  pending <- newMVar Map.empty
  nextRequestId <- newIORef 1
  receiverThread <- newMVar Nothing
  pure RPCSession
    { rpcsWriteLock = writeLock
    , rpcsPending = pending
    , rpcsNextRequestId = nextRequestId
    , rpcsReceiverThread = receiverThread
    }

------------------------------------------------------------------------
-- Connection
------------------------------------------------------------------------

-- | An active RPC session with a plugin.
data RPCConnection = RPCConnection
  { rpcManifest         :: !RPCManifest
    -- ^ The plugin's parsed manifest.
  , rpcTransport        :: !Transport
    -- ^ The underlying transport handle.
  , rpcParams           :: !(Map Text Value)
    -- ^ Current parameter values for this plugin.
  , rpcProtocolVersion  :: !Int
    -- ^ Negotiated protocol version from handshake.
  , rpcDataDirectory    :: !(Maybe FilePath)
    -- ^ Resolved absolute path to the plugin's data directory.
  , rpcResources        :: ![DataResourceSchema]
    -- ^ Data resource schemas received from handshake.
  , rpcRequestTimeoutMicros :: !(Maybe Int)
    -- ^ Per-request timeout budget. Nothing means no request timeout.
  , rpcSession :: !RPCSession
    -- ^ Shared request correlation and receive loop state.
  , rpcRuntimeFailure :: !(IORef (Maybe RPCError))
    -- ^ First unobserved transport/protocol timeout failure for supervisor handling.
  }

-- | Create an 'RPCConnection' from a manifest and transport.
--
-- The connection starts with default protocol version and no data
-- resources.  Call 'performHandshake' after creation to negotiate
-- capabilities and receive data resource schemas.
newRPCConnection :: RPCManifest -> Transport -> Map Text Value -> RPCConnection
newRPCConnection manifest transport params = unsafePerformIO $ do
  failureRef <- newIORef Nothing
  session <- newRPCSession
  let sanitizedParams = sanitizeRPCManifestParams manifest params
  pure RPCConnection
    { rpcManifest        = manifest
    , rpcTransport       = transport
    , rpcParams          = sanitizedParams
    , rpcProtocolVersion = currentProtocolVersion
    , rpcDataDirectory   = Nothing
    , rpcResources       = []
    , rpcRequestTimeoutMicros = timeoutMicrosFromMs (rspRequestTimeoutMs (rmStartPolicy manifest))
    , rpcSession = session
    , rpcRuntimeFailure = failureRef
    }
{-# NOINLINE newRPCConnection #-}

------------------------------------------------------------------------
-- Internal helpers
------------------------------------------------------------------------

-- | Send an envelope and wait for a correlated response envelope.
rpcCall :: Maybe (IORef (Maybe RPCError)) -> Maybe Int -> Text -> RPCConnection -> RPCEnvelope -> IO (Either RPCError RPCEnvelope)
rpcCall failureRef mTimeout timeoutMessage conn envelope =
  rpcCallWithProgress failureRef mTimeout timeoutMessage conn envelope (\_ -> pure ()) (\_ -> pure ())

-- | Send an envelope, collecting correlated progress\/log messages until a
-- final response envelope arrives.  Returns the final envelope.
rpcCallWithProgress
  :: Maybe (IORef (Maybe RPCError))
  -> Maybe Int
  -> Text
  -> RPCConnection
  -> RPCEnvelope
  -> (PluginProgress -> IO ())
  -> (PluginLog -> IO ())
  -> IO (Either RPCError RPCEnvelope)
rpcCallWithProgress failureRef mTimeout timeoutMessage conn envelope onProgress onLog = mask $ \restore -> do
  let session = rpcSession conn
      transport = rpcTransport conn
  requestId <- nextRPCRequestId session
  done <- newEmptyMVar
  let pending = RPCPending
        { rpResult = done
        , rpOnProgress = onProgress
        , rpOnLog = onLog
        }
      requestEnvelope = envelope { envRequestId = Just requestId }
      cleanupPending = do
        _ <- removePending session requestId
        pure ()
  registerPending session requestId pending
  restore (sendAndAwaitPendingResult failureRef mTimeout timeoutMessage conn transport session requestId done requestEnvelope)
    `onException` cleanupPending

startRPCReceiver :: Maybe (IORef (Maybe RPCError)) -> RPCConnection -> IO ()
startRPCReceiver failureRef conn =
  let receiverFailureRef = case failureRef of
        Just ref -> Just ref
        Nothing -> Just (rpcRuntimeFailure conn)
  in modifyMVar_ (rpcsReceiverThread (rpcSession conn)) $ \mReceiver ->
    case mReceiver of
      Just _ -> pure mReceiver
      Nothing -> Just <$> forkIO (rpcReceiverLoop receiverFailureRef conn)

rpcReceiverLoop :: Maybe (IORef (Maybe RPCError)) -> RPCConnection -> IO ()
rpcReceiverLoop failureRef conn = loop
  where
    session = rpcSession conn
    transport = rpcTransport conn

    loop = do
      recvResult <- recvMessage transport
      case recvResult of
        Left err -> do
          let rpcErr = RPCTransportError err
          recordRuntimeFailureIfNeeded failureRef rpcErr
          completeAllPending session rpcErr
          closeTransport transport
          pure ()
        Right bs -> case decodeMessage bs of
          Left err -> do
            let rpcErr = RPCProtocolError err
            recordRuntimeFailureIfNeeded failureRef rpcErr
            completeAllPending session rpcErr
            closeTransport transport
            pure ()
          Right envelope -> do
            dispatchIncomingEnvelope session envelope
            loop

sendCorrelatedMessage :: RPCSession -> Transport -> RPCEnvelope -> IO (Either TransportError ())
sendCorrelatedMessage session transport envelope =
  modifyMVar (rpcsWriteLock session) $ \() -> do
    result <- sendMessage transport (encodeMessage envelope)
    pure ((), result)

sendOneWay :: RPCConnection -> RPCEnvelope -> IO (Either RPCError ())
sendOneWay conn envelope = do
  result <- sendCorrelatedMessage (rpcSession conn) (rpcTransport conn) envelope
  case result of
    Left err -> do
      let rpcErr = RPCTransportError err
      recordRuntimeFailureIfNeeded (Just (rpcRuntimeFailure conn)) rpcErr
      pure (Left rpcErr)
    Right () -> pure (Right ())

sendAndAwaitPendingResult
  :: Maybe (IORef (Maybe RPCError))
  -> Maybe Int
  -> Text
  -> RPCConnection
  -> Transport
  -> RPCSession
  -> Word64
  -> MVar (Either RPCError RPCEnvelope)
  -> RPCEnvelope
  -> IO (Either RPCError RPCEnvelope)
sendAndAwaitPendingResult failureRef mTimeout timeoutMessage conn transport session requestId done envelope = do
  result <- case mTimeout of
    Nothing -> Just <$> sendThenWait
    Just micros -> timeout micros sendThenWait
  case result of
    Nothing -> do
      _ <- removePending session requestId
      let err = RPCTimeout timeoutMessage
      recordRuntimeFailure failureRef err
      pure (Left err)
    Just value -> do
      recordResultFailure failureRef value
      pure value
  where
    sendThenWait = do
      sendResult <- sendCorrelatedMessage session transport envelope
      case sendResult of
        Left err -> do
          _ <- removePending session requestId
          pure (Left (RPCTransportError err))
        Right () -> do
          startRPCReceiver failureRef conn
          takeMVar done

nextRPCRequestId :: RPCSession -> IO Word64
nextRPCRequestId session =
  atomicModifyIORef' (rpcsNextRequestId session) $ \current ->
    let requestId = if current == 0 then 1 else current
        nextId = if requestId == maxBound then 1 else requestId + 1
    in (nextId, requestId)

registerPending :: RPCSession -> Word64 -> RPCPending -> IO ()
registerPending session requestId pending =
  modifyMVar_ (rpcsPending session) $ \pendingMap ->
    pure (Map.insert requestId pending pendingMap)

removePending :: RPCSession -> Word64 -> IO (Maybe RPCPending)
removePending session requestId =
  modifyMVar (rpcsPending session) $ \pendingMap ->
    let (mPending, pendingMap') = Map.updateLookupWithKey (\_ _ -> Nothing) requestId pendingMap
    in pure (pendingMap', mPending)

completeAllPending :: RPCSession -> RPCError -> IO ()
completeAllPending session err = do
  pendingMap <- modifyMVar (rpcsPending session) $ \pending -> pure (Map.empty, pending)
  forM_ (Map.elems pendingMap) $ \pending -> do
    _ <- tryPutMVar (rpResult pending) (Left err)
    pure ()

dispatchIncomingEnvelope :: RPCSession -> RPCEnvelope -> IO ()
dispatchIncomingEnvelope session envelope =
  case envType envelope of
    MsgProgress -> lookupPending session envelope >>= maybe (pure ()) (`handleInterimEnvelope` envelope)
    MsgLog -> lookupPending session envelope >>= maybe (pure ()) (`handleInterimEnvelope` envelope)
    MsgExternalDataSourceOperationResult ->
      case envRequestId envelope of
        Nothing -> pure ()
        Just _ -> dispatchFinalEnvelope session envelope
    _ -> dispatchFinalEnvelope session envelope

dispatchFinalEnvelope :: RPCSession -> RPCEnvelope -> IO ()
dispatchFinalEnvelope session envelope = do
  mPending <- removePendingForEnvelope session envelope
  case mPending of
    Nothing -> pure ()
    Just pending -> handleFinalEnvelope pending envelope

lookupPending :: RPCSession -> RPCEnvelope -> IO (Maybe RPCPending)
lookupPending session envelope =
  modifyMVar (rpcsPending session) $ \pendingMap ->
    pure (pendingMap, case envRequestId envelope of
      Just requestId -> Map.lookup requestId pendingMap
      Nothing -> case Map.elems pendingMap of
        [pending] -> Just pending
        _ -> Nothing)

removePendingForEnvelope :: RPCSession -> RPCEnvelope -> IO (Maybe RPCPending)
removePendingForEnvelope session envelope =
  case envRequestId envelope of
    Just requestId -> removePending session requestId
    Nothing -> removeSolePending session

removeSolePending :: RPCSession -> IO (Maybe RPCPending)
removeSolePending session =
  modifyMVar (rpcsPending session) $ \pendingMap ->
    case Map.elems pendingMap of
      [pending] -> pure (Map.empty, Just pending)
      _ -> pure (pendingMap, Nothing)

handleInterimEnvelope :: RPCPending -> RPCEnvelope -> IO ()
handleInterimEnvelope pending envelope =
  case envType envelope of
    MsgProgress ->
      case Aeson.fromJSON (envPayload envelope) of
        Aeson.Success progress -> ignoreCallbackException (rpOnProgress pending progress)
        Aeson.Error _ -> pure ()
    MsgLog ->
      case Aeson.fromJSON (envPayload envelope) of
        Aeson.Success logMsg -> ignoreCallbackException (rpOnLog pending logMsg)
        Aeson.Error _ -> pure ()
    _ -> pure ()

handleFinalEnvelope :: RPCPending -> RPCEnvelope -> IO ()
handleFinalEnvelope pending envelope = do
  let result = case envType envelope of
        MsgError -> Left (decodePluginErrorPayload (envPayload envelope))
        _ -> Right envelope
  _ <- tryPutMVar (rpResult pending) result
  pure ()

ignoreCallbackException :: IO () -> IO ()
ignoreCallbackException action = do
  _ <- try action :: IO (Either SomeException ())
  pure ()

recordResultFailure :: Maybe (IORef (Maybe RPCError)) -> Either RPCError a -> IO ()
recordResultFailure failureRef result =
  case result of
    Left err -> recordRuntimeFailureIfNeeded failureRef err
    _ -> pure ()

recordRuntimeFailureIfNeeded :: Maybe (IORef (Maybe RPCError)) -> RPCError -> IO ()
recordRuntimeFailureIfNeeded failureRef err =
  when (isRuntimeConnectionFailure err) (recordRuntimeFailure failureRef err)

recordRuntimeFailure :: Maybe (IORef (Maybe RPCError)) -> RPCError -> IO ()
recordRuntimeFailure Nothing _ = pure ()
recordRuntimeFailure (Just failureRef) err =
  atomicModifyIORef' failureRef $ \current ->
    case current of
      Nothing -> (Just err, ())
      Just existing -> (Just existing, ())

isRuntimeConnectionFailure :: RPCError -> Bool
isRuntimeConnectionFailure rpcError = case rpcError of
  RPCTransportError _ -> True
  RPCProtocolError _ -> True
  RPCTimeout _ -> True
  RPCPluginError _ _ -> False
  RPCDataResourceError _ _ -> False

-- | Decode a plugin @error@ envelope, preserving standardized data-resource
-- codes when a plugin or SDK supplies one.
decodePluginErrorPayload :: Value -> RPCError
decodePluginErrorPayload payload =
  case AesonTypes.parseMaybe parseStandardPluginError payload of
    Just (code, message, Just dataCode) -> RPCDataResourceError dataCode message
    Just (code, message, Nothing) -> RPCPluginError code message
    Nothing -> RPCProtocolError "invalid plugin error payload"

parseStandardPluginError :: Value -> AesonTypes.Parser (Int, Text, Maybe DataResourceErrorCode)
parseStandardPluginError = Aeson.withObject "PluginError" $ \o -> do
  code <- o .: "code"
  message <- o .: "message"
  mDataResourceCodeValue <- o .:? "data_resource_error"
  mErrorCodeValue <- o .:? "error_code"
  let mStandardCode = firstJust
        [ mDataResourceCodeValue >>= dataResourceErrorCodeFromValue
        , mErrorCodeValue >>= dataResourceErrorCodeFromValue
        , dataResourceErrorFromRPCCode code
        ]
  pure (code, message, mStandardCode)

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Nothing:xs) = firstJust xs
firstJust (Just x:_) = Just x

-- | Interpret an RPC failure as a data-resource failure at data-service call
-- sites.  General plugin errors are classified from legacy free text.
rpcErrorDataResourceFailure :: RPCError -> DataResourceFailure
rpcErrorDataResourceFailure rpcError = case rpcError of
  RPCDataResourceError code message -> DataResourceFailure code message
  RPCTimeout message -> DataResourceFailure DataResourceTimeout message
  RPCTransportError transportError -> DataResourceFailure PluginUnavailable (Text.pack (show transportError))
  RPCProtocolError message -> DataResourceFailure DataResourceInternalError message
  RPCPluginError _ message -> dataResourceFailureFromText message

timeoutMicrosFromMs :: Int -> Maybe Int
timeoutMicrosFromMs millis
  | millis <= 0 = Nothing
  | otherwise = Just (millis * 1000)

------------------------------------------------------------------------
-- Invocation
------------------------------------------------------------------------

-- | Invoke the plugin's generator stage.
--
-- Sends an @invoke_generator@ message with terrain data and config,
-- collects progress updates, and returns the generator result.
invokeGenerator
  :: RPCConnection
  -> Word64
  -- ^ Deterministic seed supplied by the caller.
  -> Value
  -- ^ Encoded terrain data (relevant chunks).
  -> IO (Either RPCError GeneratorResult)
invokeGenerator conn seed terrainData =
  invokeGeneratorWithProgress conn seed terrainData (\_ -> pure ()) (\_ -> pure ())

invokeGeneratorWithProgress
  :: RPCConnection
  -> Word64
  -> Value
  -> (PluginProgress -> IO ())
  -> (PluginLog -> IO ())
  -> IO (Either RPCError GeneratorResult)
invokeGeneratorWithProgress conn seed terrainData onProgress onLog = do
  let manifest = rpcManifest conn
      scopedTerrainData = generatorTerrainInputPayload manifest terrainData
      envelope = RPCEnvelope
        { envType = MsgInvokeGenerator
        , envPayload = Aeson.toJSON InvokeGenerator
          { igPayloadVersion = 1
          , igStageId = "plugin:" <> rmName manifest
          , igSeed    = seed
          , igConfig  = rpcParams conn
          , igTerrain = scopedTerrainData
          }
        , envRequestId = Nothing
        }
  result <- rpcCallWithProgress (Just (rpcRuntimeFailure conn)) (rpcRequestTimeoutMicros conn) "plugin generator request timed out" conn envelope onProgress onLog
  case result of
    Left err  -> pure (Left err)
    Right env ->
      case Aeson.fromJSON (envPayload env) of
        Aeson.Success gr -> pure (Right gr)
        Aeson.Error err  -> pure (Left (RPCProtocolError (Text.pack err)))

-- | Invoke the plugin's simulation tick.
--
-- Sends an @invoke_simulation@ message with context and overlay data.
invokeSimulation
  :: RPCConnection
  -> SimContext
  -> Overlay
  -> (PluginProgress -> IO ())
  -> (PluginLog -> IO ())
  -> IO (Either RPCError SimulationResult)
invokeSimulation conn ctx overlay onProgress onLog = do
  let manifest = rpcManifest conn
      policy = simulationPayloadPolicy manifest
      terrainPayloadResult
        | sppIncludeTerrain policy = Payload.terrainWorldToPayload (scTerrain ctx)
        | otherwise = Right Null
      overlaysPayload
        | sppIncludeOverlays policy = overlaysToJSON (scOverlays ctx)
        | otherwise = Object mempty
      ownOverlayPayload
        | sppIncludeOwnOverlay policy = overlayToJSON overlay
        | otherwise = Null
  case terrainPayloadResult of
    Left err -> pure (Left (RPCProtocolError err))
    Right terrainPayload -> do
      let envelope = RPCEnvelope
            { envType = MsgInvokeSimulation
            , envPayload = Aeson.toJSON InvokeSimulation
              { isPayloadVersion = 1
              , isNodeId     = rmName manifest
                , isWorldTime  = wtTick (scWorldTime ctx)
                , isDeltaTicks = scDeltaTicks ctx
                , isCalendar   = calendarToJSON (scCalendar ctx)
                , isConfig     = rpcParams conn
                , isTerrain    = terrainPayload
                , isOverlays   = overlaysPayload
                , isOwnOverlay = ownOverlayPayload
                }
            , envRequestId = Nothing
            }
      result <- rpcCallWithProgress (Just (rpcRuntimeFailure conn)) (rpcRequestTimeoutMicros conn) "plugin simulation request timed out" conn envelope onProgress onLog
      case result of
        Left err  -> pure (Left err)
        Right env ->
          case Aeson.fromJSON (envPayload env) of
            Aeson.Success sr -> pure (Right sr)
            Aeson.Error err  -> pure (Left (RPCProtocolError (Text.pack err)))

-- | Send a shutdown message to the plugin.
rpcShutdown :: RPCConnection -> IO ()
rpcShutdown conn = do
  let envelope = RPCEnvelope
        { envType = MsgShutdown
        , envPayload = object []
        , envRequestId = Nothing
        }
  _ <- sendOneWay conn envelope
  pure ()

------------------------------------------------------------------------
-- Handshake
------------------------------------------------------------------------

-- | Host-side launch authentication challenge for the initial handshake.
--
-- The host chooses the challenge and precomputes the expected proof from the
-- launch session id and token.  The token itself is never sent in the RPC
-- handshake.
data HandshakeAuthChallenge = HandshakeAuthChallenge
  { hacSessionId :: !Text
  , hacChallenge :: !Text
  , hacExpectedProof :: !Text
  } deriving (Eq, Show)

-- | Perform the protocol handshake with a connected plugin.
--
-- This compatibility wrapper omits launch-auth verification.  Production plugin
-- launches should call 'performHandshakeWithAuth' with a challenge.
performHandshake
  :: RPCConnection
  -> Maybe Text
  -- ^ World save path, or 'Nothing' if no world is loaded.
  -> IO (Either RPCError RPCConnection)
performHandshake conn worldPath = performHandshakeWithAuth conn worldPath Nothing

-- | Perform the protocol handshake and optionally verify launch credentials.
--
-- Sends 'MsgHandshake' with the current world path and, for production launches,
-- an auth challenge.  The plugin must answer with 'MsgHandshakeAck' containing
-- the expected session id and proof before the connection is marked ready.
performHandshakeWithAuth
  :: RPCConnection
  -> Maybe Text
  -- ^ World save path, or 'Nothing' if no world is loaded.
  -> Maybe HandshakeAuthChallenge
  -- ^ Expected launch credentials and challenge for production startup.
  -> IO (Either RPCError RPCConnection)
performHandshakeWithAuth conn worldPath mAuth = do
  let envelope = RPCEnvelope
        { envType = MsgHandshake
        , envPayload = Aeson.toJSON Handshake
          { hsProtocolVersion  = currentProtocolVersion
          , hsWorldPath        = worldPath
          , hsHostCapabilities = ["query", "mutate"] <> ["launch_auth" | Just _ <- [mAuth]]
          , hsAuthChallenge    = hacChallenge <$> mAuth
          }
        , envRequestId = Nothing
        }
  result <- rpcCall
    (Just (rpcRuntimeFailure conn))
    (timeoutMicrosFromMs (rspStartupTimeoutMs (rmStartPolicy (rpcManifest conn))))
    "plugin handshake timed out"
    conn
    envelope
  case result of
    Left err -> pure (Left err)
    Right env -> case envType env of
      MsgHandshakeAck ->
        case Aeson.fromJSON (envPayload env) of
          Aeson.Success ack
            | haProtocolVersion ack /= currentProtocolVersion ->
                pure (Left (RPCProtocolError
                  ("plugin protocol version mismatch: host="
                   <> Text.pack (show currentProtocolVersion)
                   <> ", plugin="
                   <> Text.pack (show (haProtocolVersion ack)))))
            | Just authErr <- validateHandshakeAuth mAuth ack ->
                pure (Left (RPCProtocolError authErr))
            | let resourceErrors = validateHandshakeDataResources (rpcManifest conn) (haResources ack)
            , not (null resourceErrors) ->
                pure (Left (RPCProtocolError
                  ("invalid handshake data resources: " <> renderManifestErrors resourceErrors)))
            | otherwise -> pure (Right conn
                { rpcProtocolVersion = haProtocolVersion ack
                , rpcDataDirectory   = fmap Text.unpack (haDataDirectory ack)
                , rpcResources       = haResources ack
                })
          Aeson.Error err -> pure (Left (RPCProtocolError (Text.pack err)))
      MsgError ->
        pure (Left (decodePluginErrorPayload (envPayload env)))
      other ->
        pure (Left (RPCProtocolError
          ("unexpected response to handshake: " <> Text.pack (show other))))

validateHandshakeAuth :: Maybe HandshakeAuthChallenge -> HandshakeAck -> Maybe Text
validateHandshakeAuth Nothing _ = Nothing
validateHandshakeAuth (Just expected) ack
  | haSessionId ack /= Just (hacSessionId expected) =
      Just "plugin launch session missing or mismatched during handshake"
  | haAuthProof ack /= Just (hacExpectedProof expected) =
      Just "plugin launch auth proof missing or mismatched during handshake"
  | otherwise = Nothing

-- | Notify the plugin that the world save path has changed.
--
-- This is a one-way notification; no response is expected.
sendWorldChanged :: RPCConnection -> Maybe Text -> IO (Either RPCError ())
sendWorldChanged conn worldPath = do
  let envelope = RPCEnvelope
        { envType = MsgWorldChanged
        , envPayload = Aeson.toJSON WorldChanged
          { wchWorldPath = worldPath
          }
        , envRequestId = Nothing
        }
  sendOneWay conn envelope

-- | Send a heartbeat probe and wait for the plugin heartbeat response.
sendHeartbeat :: RPCConnection -> IO (Either RPCError Heartbeat)
sendHeartbeat conn = do
  let envelope = RPCEnvelope
        { envType = MsgHeartbeat
        , envPayload = Aeson.toJSON (Heartbeat { hbStatus = "ping" })
        , envRequestId = Nothing
        }
  result <- rpcCall (Just (rpcRuntimeFailure conn)) (rpcRequestTimeoutMicros conn) "plugin heartbeat timed out" conn envelope
  case result of
    Left err -> pure (Left err)
    Right env -> case envType env of
      MsgHeartbeat -> decodeRPCPayload env
      other -> pure (Left (RPCProtocolError ("unexpected heartbeat response: " <> Text.pack (show other))))

-- | Ask the plugin for a health snapshot.
checkHealth :: RPCConnection -> IO (Either RPCError HealthStatus)
checkHealth conn = do
  let envelope = RPCEnvelope
        { envType = MsgHealthCheck
        , envPayload = object []
        , envRequestId = Nothing
        }
  result <- rpcCall (Just (rpcRuntimeFailure conn)) (rpcRequestTimeoutMicros conn) "plugin health check timed out" conn envelope
  case result of
    Left err -> pure (Left err)
    Right env -> case envType env of
      MsgHealthStatus -> decodeRPCPayload env
      other -> pure (Left (RPCProtocolError ("unexpected health response: " <> Text.pack (show other))))

-- | Notify a plugin that the host has brokered an external data-source grant
-- and wait for the correlated ACK/result.
sendExternalDataSourceGrant
  :: RPCConnection
  -> RPCExternalDataSourceGrantMessage
  -> IO (Either RPCError RPCExternalDataSourceOperationResult)
sendExternalDataSourceGrant conn grant = do
  let envelope = RPCEnvelope
        { envType = MsgExternalDataSourceGrant
        , envPayload = Aeson.toJSON grant
        , envRequestId = Nothing
        }
  result <- rpcCall (Just (rpcRuntimeFailure conn)) (rpcRequestTimeoutMicros conn) "plugin external data-source grant timed out" conn envelope
  case result of
    Left err -> pure (Left err)
    Right env -> validateExternalDataSourceOperationResult
      ExternalDataSourceGrantOperation
      (redsgmOperationId grant)
      (redsgmOperationEpoch grant)
      (redsgmProviderId grant)
      (redsgmConsumerId grant)
      (redsgmSource grant)
      (redsgmGrant grant)
      env

-- | Notify a plugin that a previously brokered external data-source grant has
-- been revoked or marked unusable, and wait for the correlated ACK/result.
sendExternalDataSourceGrantRevocation
  :: RPCConnection
  -> RPCExternalDataSourceGrantRevocation
  -> IO (Either RPCError RPCExternalDataSourceOperationResult)
sendExternalDataSourceGrantRevocation conn revocation = do
  let envelope = RPCEnvelope
        { envType = MsgExternalDataSourceRevoke
        , envPayload = Aeson.toJSON revocation
        , envRequestId = Nothing
        }
  result <- rpcCall (Just (rpcRuntimeFailure conn)) (rpcRequestTimeoutMicros conn) "plugin external data-source revocation timed out" conn envelope
  case result of
    Left err -> pure (Left err)
    Right env -> validateExternalDataSourceOperationResult
      ExternalDataSourceRevokeOperation
      (redsrvOperationId revocation)
      (redsrvOperationEpoch revocation)
      (redsrvProviderId revocation)
      (redsrvConsumerId revocation)
      (redsrvSource revocation)
      (redsrvGrant revocation)
      env

-- | Alias for 'sendExternalDataSourceGrantRevocation'.
revokeExternalDataSourceGrant
  :: RPCConnection
  -> RPCExternalDataSourceGrantRevocation
  -> IO (Either RPCError RPCExternalDataSourceOperationResult)
revokeExternalDataSourceGrant = sendExternalDataSourceGrantRevocation

validateExternalDataSourceOperationResult
  :: RPCExternalDataSourceOperation
  -> Maybe Text
  -> Maybe Word64
  -> Text
  -> Maybe Text
  -> Text
  -> Text
  -> RPCEnvelope
  -> IO (Either RPCError RPCExternalDataSourceOperationResult)
validateExternalDataSourceOperationResult expectedOperation expectedOperationId expectedEpoch expectedProvider expectedConsumer expectedSource expectedGrant env =
  case envType env of
    MsgExternalDataSourceOperationResult ->
      case Aeson.fromJSON (envPayload env) of
        Aeson.Error err -> pure (Left (RPCProtocolError
          ("invalid external data-source operation result payload: " <> Text.pack err)))
        Aeson.Success operationResult ->
          case externalOperationProtocolError
              expectedOperation
              expectedOperationId
              expectedEpoch
              expectedProvider
              expectedConsumer
              expectedSource
              expectedGrant
              operationResult of
            Just err -> pure (Left (RPCProtocolError err))
            Nothing -> pure (Right operationResult)
    other -> pure (Left (RPCProtocolError
      ("unexpected external data-source " <> externalOperationText expectedOperation
        <> " response: " <> Text.pack (show other))))

externalOperationProtocolError
  :: RPCExternalDataSourceOperation
  -> Maybe Text
  -> Maybe Word64
  -> Text
  -> Maybe Text
  -> Text
  -> Text
  -> RPCExternalDataSourceOperationResult
  -> Maybe Text
externalOperationProtocolError expectedOperation expectedOperationId expectedEpoch expectedProvider expectedConsumer expectedSource expectedGrant operationResult = firstJust
  [ expectOperation expectedOperation (redsoOperation operationResult)
  , expectMaybeText "operationId" expectedOperationId (redsoOperationId operationResult)
  , expectMaybeWord64 "operationEpoch" expectedEpoch (redsoOperationEpoch operationResult)
  , expectText "providerId" expectedProvider (redsoProviderId operationResult)
  , expectMaybeText "consumerId" expectedConsumer (redsoConsumerId operationResult)
  , expectText "source" expectedSource (redsoSource operationResult)
  , expectText "grant" expectedGrant (redsoGrant operationResult)
  ]

expectOperation :: RPCExternalDataSourceOperation -> RPCExternalDataSourceOperation -> Maybe Text
expectOperation expected actual
  | actual == expected = Nothing
  | otherwise = Just
      ("external data-source operation result operation mismatch: expected "
        <> externalOperationText expected <> ", got " <> externalOperationText actual)

expectText :: Text -> Text -> Text -> Maybe Text
expectText field expected actual
  | actual == expected = Nothing
  | otherwise = Just
      ("external data-source operation result " <> field <> " mismatch: expected "
        <> expected <> ", got " <> actual)

expectMaybeText :: Text -> Maybe Text -> Text -> Maybe Text
expectMaybeText _ Nothing _ = Nothing
expectMaybeText field (Just expected) actual = expectText field expected actual

expectMaybeWord64 :: Text -> Maybe Word64 -> Maybe Word64 -> Maybe Text
expectMaybeWord64 _ Nothing _ = Nothing
expectMaybeWord64 field (Just expected) actual
  | actual == Just expected = Nothing
  | otherwise = Just
      ("external data-source operation result " <> field <> " mismatch: expected "
        <> Text.pack (show expected) <> ", got " <> Text.pack (show actual))

externalOperationText :: RPCExternalDataSourceOperation -> Text
externalOperationText ExternalDataSourceGrantOperation = "grant"
externalOperationText ExternalDataSourceRevokeOperation = "revoke"

-- | Request a backend-neutral status snapshot for a plugin's external
-- data-source declarations, grants, and consumer references.
requestExternalDataSourceStatus
  :: RPCConnection
  -> RPCExternalDataSourceStatusRequest
  -> IO (Either RPCError RPCExternalDataSourceStatusReport)
requestExternalDataSourceStatus conn request = do
  let envelope = RPCEnvelope
        { envType = MsgExternalDataSourceStatusRequest
        , envPayload = Aeson.toJSON request
        , envRequestId = Nothing
        }
  result <- rpcCall (Just (rpcRuntimeFailure conn)) (rpcRequestTimeoutMicros conn) "plugin external data-source status request timed out" conn envelope
  case result of
    Left err -> pure (Left err)
    Right env -> do
      decoded <- case envType env of
        MsgExternalDataSourceStatus -> decodeRPCPayload env
        other -> pure (Left (RPCProtocolError ("unexpected external data-source status response: " <> Text.pack (show other))))
      recordResultFailure (Just (rpcRuntimeFailure conn)) decoded
      pure decoded

-- | Alias for 'requestExternalDataSourceStatus'.
checkExternalDataSourceStatus
  :: RPCConnection
  -> RPCExternalDataSourceStatusRequest
  -> IO (Either RPCError RPCExternalDataSourceStatusReport)
checkExternalDataSourceStatus = requestExternalDataSourceStatus

decodeRPCPayload :: Aeson.FromJSON a => RPCEnvelope -> IO (Either RPCError a)
decodeRPCPayload env =
  case Aeson.fromJSON (envPayload env) of
    Aeson.Success value -> pure (Right value)
    Aeson.Error err -> pure (Left (RPCProtocolError (Text.pack err)))

------------------------------------------------------------------------
-- Data service
------------------------------------------------------------------------

-- | Query a plugin's data resource.
--
-- Sends 'MsgQueryResource' and waits for 'MsgQueryResult'.  The
-- plugin-manager data-resource surface is request/response-only, so
-- correlated progress/log messages are consumed by the RPC layer and
-- intentionally ignored at this boundary.
queryResource :: RPCConnection -> QueryResource -> IO (Either RPCError QueryResult)
queryResource conn qr = do
  let envelope = RPCEnvelope
        { envType    = MsgQueryResource
        , envPayload = Aeson.toJSON qr
        , envRequestId = Nothing
        }
  result <- rpcCallWithProgress (Just (rpcRuntimeFailure conn)) (rpcRequestTimeoutMicros conn) "plugin data query timed out" conn envelope
              (\_ -> pure ())
              (\_ -> pure ())
  case result of
    Left err  -> pure (Left err)
    Right env ->
      case Aeson.fromJSON (envPayload env) of
        Aeson.Success r -> pure (Right r)
        Aeson.Error err -> pure (Left (RPCProtocolError (Text.pack err)))

-- | Mutate a plugin's data resource.
--
-- Sends 'MsgMutateResource' and waits for 'MsgMutateResult'.  The
-- plugin-manager data-resource surface is request/response-only, so
-- correlated progress/log messages are consumed by the RPC layer and
-- intentionally ignored at this boundary.
mutateResource :: RPCConnection -> MutateResource -> IO (Either RPCError MutateResult)
mutateResource conn mr = do
  let envelope = RPCEnvelope
        { envType    = MsgMutateResource
        , envPayload = Aeson.toJSON mr
        , envRequestId = Nothing
        }
  result <- rpcCallWithProgress (Just (rpcRuntimeFailure conn)) (rpcRequestTimeoutMicros conn) "plugin data mutation timed out" conn envelope
              (\_ -> pure ())
              (\_ -> pure ())
  case result of
    Left err  -> pure (Left err)
    Right env ->
      case Aeson.fromJSON (envPayload env) of
        Aeson.Success r -> pure (Right r)
        Aeson.Error err -> pure (Left (RPCProtocolError (Text.pack err)))

------------------------------------------------------------------------
-- Pipeline / DAG integration
------------------------------------------------------------------------

-- | Create a 'PipelineStage' from an RPC connection.
--
-- The stage's action sends terrain data to the plugin, receives
-- modified chunks back, and merges them into the world.
rpcGeneratorStage :: RPCConnection -> PipelineStage
rpcGeneratorStage conn =
  let manifest = rpcManifest conn
  in PipelineStage
    { stageId   = StagePlugin (rmName manifest)
    , stageName = rmName manifest
    , stageSeedTag = "plugin:" <> rmName manifest
    , stageOverlayProduces = if manifestHasOverlay manifest then Just (rmName manifest) else Nothing
    , stageOverlayReads = []
    , stageOverlaySchema = Nothing
    , stageRun  = do
        logInfo ("plugin:" <> rmName manifest <> ": invoking generator")
        world <- liftTopo getWorld
        case generatorStageTerrainPayload manifest world of
          Left err ->
            throwError (PluginInvariantError ("rpc generator encode failed: " <> err))
          Right terrainPayload -> do
            seed <- asks peSeed
            reportProgress <- asks peProgress
            result <- liftIO (invokeGeneratorWithProgress conn seed terrainPayload
              (reportProgress . formatPluginProgressDetail (rmName manifest))
              (\_ -> pure ()))
            case result of
              Left err -> throwError (PluginInvariantError ("rpc generator failed: " <> rpcErrorText err))
              Right generatorResult ->
                case applyGeneratorResult manifest world generatorResult of
                  Left mergeErr ->
                    throwError (PluginInvariantError ("rpc generator merge failed: " <> mergeErr))
                  Right mergedWorld -> do
                    liftTopo (putWorld mergedWorld)
                    logInfo ("plugin:" <> rmName manifest <> ": generator complete")
    }

-- | Create a 'SimNode' from an RPC connection.
--
-- Dispatches to 'SimNodeReader' or 'SimNodeWriter' based on
-- whether the manifest declares @writeTerrain@ capability.
rpcSimNode :: RPCConnection -> SimNode
rpcSimNode conn =
  let manifest = rpcManifest conn
      policy   = simulationPayloadPolicy manifest
      nodeId   = SimNodeId (rmName manifest)
      name     = rmName manifest
      deps     = case rmSimulation manifest of
        Just sd -> map SimNodeId (rsdDependencies sd)
        Nothing -> []
      schedule = maybe defaultScheduleDecl rsdSchedule (rmSimulation manifest)
  in if manifestWritesTerrain manifest
    then SimNodeWriter
      { snwId           = nodeId
      , snwOverlayName  = name
      , snwDependencies = deps
      , snwSchedule     = schedule
      , snwWriteTick    = \ctx overlay -> do
          if not (sppRequireWriteOverlay policy)
            then pure (Left "manifest missing writeOverlay/writeWorld capability")
            else do
              result <- invokeSimulation conn ctx overlay (reportSimulationProgress ctx name) ignoreLog
              case result of
                Left err -> pure (Left (rpcErrorText err))
                Right sr -> do
                  let decodedOverlay = preserveHostProvenance overlay
                        <$> overlayFromJSON (ovSchema overlay) (srOverlay sr)
                      decodedWrites = Payload.decodeTerrainWritesValue (srTerrainWrites sr)
                  pure $ case (decodedOverlay, decodedWrites) of
                    (Right nextOverlay, Right writes) -> Right (nextOverlay, writes)
                    (Left overlayErr, _) -> Left overlayErr
                    (_, Left writesErr) -> Left writesErr
      }
    else SimNodeReader
      { snrId           = nodeId
      , snrOverlayName  = name
      , snrDependencies = deps
      , snrSchedule     = schedule
      , snrReadTick     = \ctx overlay -> do
          if not (sppRequireWriteOverlay policy)
            then pure (Left "manifest missing writeOverlay/writeWorld capability")
            else do
              result <- invokeSimulation conn ctx overlay (reportSimulationProgress ctx name) ignoreLog
              pure $ case result of
                Left err -> Left (rpcErrorText err)
                Right sr -> do
                  rejectUnauthorizedTerrainWrites manifest (srTerrainWrites sr)
                  preserveHostProvenance overlay <$> overlayFromJSON (ovSchema overlay) (srOverlay sr)
      }
  where
    reportSimulationProgress ctx pluginName progress =
      scReportProgress ctx (formatPluginProgressDetail pluginName progress)
    ignoreLog _ = pure ()

formatPluginProgressDetail :: Text -> PluginProgress -> Text
formatPluginProgressDetail pluginName progress =
  "plugin:" <> pluginName <> ": " <> ppMessage progress
    <> " (fraction=" <> Text.pack (show (ppFraction progress))
    <> ", percent=" <> Text.pack (show (round (ppFraction progress * 100) :: Int))
    <> "%)"

preserveHostProvenance :: Overlay -> Overlay -> Overlay
preserveHostProvenance existing decoded =
  decoded { ovProvenance = ovProvenance existing }

rejectUnauthorizedTerrainWrites :: RPCManifest -> Maybe Value -> Either Text ()
rejectUnauthorizedTerrainWrites manifest rawWrites =
  case Payload.terrainWritesValueEmpty rawWrites of
    Left err -> Left (unauthorizedTerrainWritesText manifest
      <> " (could not verify terrain_writes were empty: " <> err <> ")")
    Right True -> Right ()
    Right False -> Left (unauthorizedTerrainWritesText manifest)

unauthorizedTerrainWritesText :: RPCManifest -> Text
unauthorizedTerrainWritesText manifest =
  "unauthorized terrain write attempt via terrain_writes by plugin "
    <> rmName manifest
    <> ": manifest missing writeTerrain/writeWorld capability"

hasCapability :: RPCManifest -> Capability -> Bool
hasCapability manifest capability = capability `elem` rmCapabilities manifest

canReadTerrain :: RPCManifest -> Bool
canReadTerrain manifest =
  hasCapability manifest CapReadTerrain || hasCapability manifest CapReadWorld

generatorTerrainInputPayload :: RPCManifest -> Value -> Value
generatorTerrainInputPayload manifest terrainData
  | canReadTerrain manifest = terrainData
  | otherwise = Null

generatorStageTerrainPayload :: RPCManifest -> Topo.World.TerrainWorld -> Either Text Value
generatorStageTerrainPayload manifest world
  | canReadTerrain manifest = Payload.terrainWorldToPayload world
  | otherwise = Right Null

canReadOverlay :: RPCManifest -> Bool
canReadOverlay manifest =
  hasCapability manifest CapReadOverlay || hasCapability manifest CapReadWorld

canWriteOverlay :: RPCManifest -> Bool
canWriteOverlay manifest =
  hasCapability manifest CapWriteOverlay || hasCapability manifest CapWriteWorld

data SimulationPayloadPolicy = SimulationPayloadPolicy
  { sppIncludeTerrain :: !Bool
  , sppIncludeOverlays :: !Bool
  , sppIncludeOwnOverlay :: !Bool
  , sppRequireWriteOverlay :: !Bool
  }

simulationPayloadPolicy :: RPCManifest -> SimulationPayloadPolicy
simulationPayloadPolicy manifest =
  SimulationPayloadPolicy
    { sppIncludeTerrain = canReadTerrain manifest
    , sppIncludeOverlays = canReadOverlay manifest
    , sppIncludeOwnOverlay = canReadOverlay manifest || canWriteOverlay manifest
    , sppRequireWriteOverlay = canWriteOverlay manifest
    }

calendarToJSON :: CalendarDate -> Value
calendarToJSON calendarDate = object
  [ "year" .= cdYear calendarDate
  , "dayOfYear" .= cdDayOfYear calendarDate
  , "hourOfDay" .= cdHourOfDay calendarDate
  ]

overlaysToJSON :: Map Text Overlay -> Value
overlaysToJSON overlays =
  object [ Key.fromText name .= overlayToJSON overlay | (name, overlay) <- Map.toList overlays ]

rpcErrorText :: RPCError -> Text
rpcErrorText rpcError =
  case rpcError of
    RPCTransportError transportError -> Text.pack (show transportError)
    RPCProtocolError message -> message
    RPCPluginError code message -> "plugin error " <> Text.pack (show code) <> ": " <> message
    RPCDataResourceError code message -> dataResourceFailureText (DataResourceFailure code message)
    RPCTimeout message -> "timeout: " <> message

applyGeneratorResult
  :: RPCManifest
  -> Topo.World.TerrainWorld
  -> GeneratorResult
  -> Either Text Topo.World.TerrainWorld
applyGeneratorResult manifest world result = do
  worldWithTerrain <- Payload.applyGeneratorTerrainValue world (grTerrain result)
  applyGeneratorOverlayPayload manifest worldWithTerrain (grOverlay result)

terrainWorldToPayload :: Topo.World.TerrainWorld -> Either Text Value
terrainWorldToPayload = Payload.terrainWorldToPayload

terrainWorldToCompletePayload :: Topo.World.TerrainWorld -> Either Text Value
terrainWorldToCompletePayload = Payload.terrainWorldToCompletePayload

decodeTerrainWritesValue :: Maybe Value -> Either Text TerrainWrites
decodeTerrainWritesValue = Payload.decodeTerrainWritesValue

applyGeneratorTerrainValue :: Topo.World.TerrainWorld -> Value -> Either Text Topo.World.TerrainWorld
applyGeneratorTerrainValue = Payload.applyGeneratorTerrainValue

encodeBase64Text :: BS.ByteString -> Text
encodeBase64Text = Payload.encodeBase64Text

decodeBase64Text :: Text -> Either Text BS.ByteString
decodeBase64Text = Payload.decodeBase64Text

applyGeneratorOverlayPayload
  :: RPCManifest
  -> Topo.World.TerrainWorld
  -> Maybe Value
  -> Either Text Topo.World.TerrainWorld
applyGeneratorOverlayPayload _ world Nothing = Right world
applyGeneratorOverlayPayload manifest world (Just overlayValue)
  | not (manifestHasOverlay manifest) =
      Left ("plugin " <> rmName manifest
        <> " returned generator overlay output for overlay " <> rmName manifest
        <> " but manifest is missing overlay declaration")
  | not (canWriteOverlay manifest) =
      Left ("plugin " <> rmName manifest
        <> " returned generator overlay output for overlay " <> rmName manifest
        <> " but manifest is missing writeOverlay/writeWorld capability")
  | otherwise =
      case lookupOverlay (rmName manifest) (Topo.World.twOverlays world) of
        Nothing -> Left ("plugin " <> rmName manifest
          <> " returned generator overlay output for overlay " <> rmName manifest
          <> " but host has no registered overlay surface")
        Just existingOverlay -> do
          decodedOverlay <- overlayFromJSON (ovSchema existingOverlay) overlayValue
          let nextOverlay = preserveHostProvenance existingOverlay decodedOverlay
              nextOverlays = insertOverlay nextOverlay (Topo.World.twOverlays world)
          Right world { Topo.World.twOverlays = nextOverlays }
