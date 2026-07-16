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
  , newRPCConnectionWithLimits
  , RPCFailureEvent(..)
  , RPCFailureSource(..)
  , awaitRPCFailureEvent
  , peekRPCFailureEvent
  , claimRPCFailureEvent
  , sameRPCConnection
  , claimRPCSupervisorMonitor
  , releaseRPCSupervisorMonitor
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
  , terrainWorldToPayloadWithLimits
  , terrainWorldToScopedPayload
  , terrainWorldToScopedPayloadWithLimits
  , terrainWorldToCompletePayload
  , terrainWorldToCompletePayloadWithLimits
  , decodeTerrainWritesValue
  , decodeTerrainWritesValueWithLimits
  , applyGeneratorTerrainValue
  , applyGeneratorTerrainValueWithLimits
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
  , module Topo.Plugin.RPC.Scope
  , module Topo.Plugin.RPC.Stream
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
  , readMVar
  , takeMVar
  , tryPutMVar
  , tryTakeMVar
  )
import Control.Exception (SomeException, mask, onException, try)
import Control.Monad (forM_, unless, when)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Char (chr, ord)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntSet as IntSet
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)

import qualified Data.Aeson as Aeson
import Data.Aeson (Value(..), (.:), (.:?), (.=), object)
import qualified Data.Aeson.Types as AesonTypes (Parser, parseMaybe)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import System.Timeout (timeout)

import Topo.Overlay (Overlay(..), insertOverlay, lookupOverlay, overlayChunkIds)
import Topo.Overlay.JSON
  ( overlayFromJSON
  , overlayFromScopedJSON
  , overlayToJSON
  , overlayToScopedJSON
  )
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
import Topo.Plugin.RPC.Scope
import Topo.Plugin.RPC.Stream
import Topo.Plugin.RPC.Transport
import Topo.Plugin.RPC.DataService
import Topo.Plugin.RPC.ExternalDataSource
import Topo.Plugin.DataResource (DataResourceSchema(..))
import qualified Topo.Plugin.DataResource.Validation as DataValidation

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

-- | Source identity for a fatal connection failure. Request identities let
-- callers distinguish their own timeout from a concurrent transport failure.
data RPCFailureSource
  = RPCFailureTransport
  | RPCFailureRequest !Word64
  deriving (Eq, Show)

-- | A level-triggered fatal runtime failure notification.
data RPCFailureEvent = RPCFailureEvent
  { rpfeIdentity :: !Word64
  , rpfeSource :: !RPCFailureSource
  , rpfeError :: !RPCError
  } deriving (Eq, Show)

data RPCFailureSlot = RPCFailureSlot
  { rpfsCurrent :: !(MVar (Maybe RPCFailureEvent))
  , rpfsWakeup :: !(MVar ())
  , rpfsNextIdentity :: !(IORef Word64)
  , rpfsSupervisorClaimed :: !(MVar Bool)
  }

newRPCFailureSlot :: IO RPCFailureSlot
newRPCFailureSlot = RPCFailureSlot
  <$> newMVar Nothing
  <*> newEmptyMVar
  <*> newIORef 1
  <*> newMVar False

-- | Block until this connection has an unclaimed fatal failure.
awaitRPCFailureEvent :: RPCConnection -> IO RPCFailureEvent
awaitRPCFailureEvent conn = loop
  where
    slot = rpcRuntimeFailure conn
    loop = do
      current <- readMVar (rpfsCurrent slot)
      case current of
        Just event -> pure event
        Nothing -> takeMVar (rpfsWakeup slot) >> loop

-- | Inspect the current fatal failure without claiming it.
peekRPCFailureEvent :: RPCConnection -> IO (Maybe RPCFailureEvent)
peekRPCFailureEvent = readMVar . rpfsCurrent . rpcRuntimeFailure

-- | Claim exactly the supplied event. A stale event cannot clear a newer
-- failure recorded by the receiver.
claimRPCFailureEvent :: RPCConnection -> RPCFailureEvent -> IO Bool
claimRPCFailureEvent conn expected = do
  claimed <- modifyMVar (rpfsCurrent slot) $ \current ->
    case current of
      Just actual | rpfeIdentity actual == rpfeIdentity expected -> pure (Nothing, True)
      _ -> pure (current, False)
  when claimed $ do
    _ <- tryTakeMVar (rpfsWakeup slot)
    pure ()
  pure claimed
  where
    slot = rpcRuntimeFailure conn

-- | Physical connection identity used alongside runtime generations. This is
-- required for embedded connection-only runtimes whose legacy generation is 0.
sameRPCConnection :: RPCConnection -> RPCConnection -> Bool
sameRPCConnection a b =
  rpfsCurrent (rpcRuntimeFailure a) == rpfsCurrent (rpcRuntimeFailure b)

-- | Claim installation of the single actor-facing monitor set for an embedded
-- connection-only runtime.
claimRPCSupervisorMonitor :: RPCConnection -> IO Bool
claimRPCSupervisorMonitor conn =
  modifyMVar (rpfsSupervisorClaimed (rpcRuntimeFailure conn)) $ \claimed ->
    pure (True, not claimed)

releaseRPCSupervisorMonitor :: RPCConnection -> IO ()
releaseRPCSupervisorMonitor conn =
  modifyMVar_ (rpfsSupervisorClaimed (rpcRuntimeFailure conn)) (const (pure False))

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
  , rpcPayloadLimits   :: !RPCPayloadLimits
    -- ^ Symmetric framed and decoded payload limits for this session.
  , rpcProtocolVersion  :: !Int
    -- ^ Highest manifest/host overlap, echoed by the handshake.
  , rpcStreamV1 :: !(Maybe NegotiatedStreamV1)
    -- ^ Negotiated v5 streaming limits; absent on the unchanged v4 path.
  , rpcDataDirectory    :: !(Maybe FilePath)
    -- ^ Validated relative archive directory from the handshake. This is never
    -- used as a source path; the host derives the source from its launch data
    -- root.
  , rpcResources        :: ![DataResourceSchema]
    -- ^ Data resource schemas received from handshake.
  , rpcResourcesNegotiated :: !Bool
    -- ^ Whether the handshake resource list is authoritative, including an
    -- explicitly empty narrowing.
  , rpcRequestTimeoutMicros :: !(Maybe Int)
    -- ^ Per-request timeout budget. Nothing means no request timeout.
  , rpcSession :: !RPCSession
    -- ^ Shared request correlation and receive loop state.
  , rpcRuntimeFailure :: !RPCFailureSlot
    -- ^ First unclaimed fatal failure and its level-triggered notification.
  }

-- | Create an 'RPCConnection' from a manifest and transport.
--
-- The connection starts with default protocol version and no data
-- resources.  Call 'performHandshake' after creation to negotiate
-- capabilities and receive data resource schemas.
newRPCConnection :: RPCManifest -> Transport -> Map Text Value -> RPCConnection
newRPCConnection = newRPCConnectionWithLimits defaultRPCPayloadLimits

-- | Create an embedded or production RPC connection with explicit limits.
newRPCConnectionWithLimits
  :: RPCPayloadLimits
  -> RPCManifest
  -> Transport
  -> Map Text Value
  -> RPCConnection
newRPCConnectionWithLimits payloadLimits manifest transport params = unsafePerformIO $ do
  failureSlot <- newRPCFailureSlot
  session <- newRPCSession
  let sanitizedParams = sanitizeRPCManifestParams manifest params
  pure RPCConnection
    { rpcManifest        = manifest
    , rpcTransport       = transport
    , rpcParams          = sanitizedParams
    , rpcPayloadLimits   = payloadLimits
    , rpcProtocolVersion = either (const minimumSupportedProtocolVersion) id
        (manifestProtocolVersion manifest)
    , rpcStreamV1 = Nothing
    , rpcDataDirectory   = Nothing
    , rpcResources       = []
    , rpcResourcesNegotiated = False
    , rpcRequestTimeoutMicros = timeoutMicrosFromMs (rspRequestTimeoutMs (rmStartPolicy manifest))
    , rpcSession = session
    , rpcRuntimeFailure = failureSlot
    }
{-# NOINLINE newRPCConnectionWithLimits #-}

------------------------------------------------------------------------
-- Internal helpers
------------------------------------------------------------------------

-- | Send an envelope and wait for a correlated response envelope. The flag
-- controls whether this operation's timeout is fatal to the runtime; transport
-- and receiver protocol failures are always fatal.
rpcCall :: Bool -> Maybe Int -> Text -> RPCConnection -> RPCEnvelope -> IO (Either RPCError RPCEnvelope)
rpcCall fatalTimeout mTimeout timeoutMessage conn envelope =
  rpcCallWithProgress fatalTimeout mTimeout timeoutMessage conn envelope (\_ -> pure ()) (\_ -> pure ())

-- | Send an envelope, collecting correlated progress\/log messages until a
-- final response envelope arrives.  Returns the final envelope.
rpcCallWithProgress
  :: Bool
  -> Maybe Int
  -> Text
  -> RPCConnection
  -> RPCEnvelope
  -> (PluginProgress -> IO ())
  -> (PluginLog -> IO ())
  -> IO (Either RPCError RPCEnvelope)
rpcCallWithProgress fatalTimeout mTimeout timeoutMessage conn envelope onProgress onLog = mask $ \restore -> do
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
  restore (sendAndAwaitPendingResult fatalTimeout mTimeout timeoutMessage conn transport session requestId done requestEnvelope)
    `onException` cleanupPending

startRPCReceiver :: RPCConnection -> IO ()
startRPCReceiver conn =
  modifyMVar_ (rpcsReceiverThread (rpcSession conn)) $ \mReceiver ->
    case mReceiver of
      Just _ -> pure mReceiver
      Nothing -> Just <$> forkIO (rpcReceiverLoop conn)

rpcReceiverLoop :: RPCConnection -> IO ()
rpcReceiverLoop conn = loop
  where
    session = rpcSession conn
    transport = rpcTransport conn

    loop = do
      recvResult <- recvMessageWithLimit
        (fromIntegral (rplMaxFrameSizeBytes (rpcPayloadLimits conn))) transport
      case recvResult of
        Left err -> do
          let rpcErr = RPCTransportError err
          recordRuntimeFailureIfNeeded conn RPCFailureTransport rpcErr
          completeAllPending session rpcErr
          closeTransport transport
          pure ()
        Right bs -> case decodeMessage bs of
          Left err -> do
            let rpcErr = RPCProtocolError err
            recordRuntimeFailureIfNeeded conn RPCFailureTransport rpcErr
            completeAllPending session rpcErr
            closeTransport transport
            pure ()
          Right envelope -> do
            mProtocolFailure <- dispatchIncomingEnvelope session envelope
            case mProtocolFailure of
              Nothing -> loop
              Just rpcErr -> do
                recordRuntimeFailureIfNeeded conn RPCFailureTransport rpcErr
                completeAllPending session rpcErr
                closeTransport transport

sendCorrelatedMessage
  :: RPCPayloadLimits
  -> RPCSession
  -> Transport
  -> RPCEnvelope
  -> IO (Either TransportError ())
sendCorrelatedMessage limits session transport envelope =
  modifyMVar (rpcsWriteLock session) $ \() -> do
    let encoded = encodeMessageLazy envelope
        actual = toInteger (BL.length encoded)
        limit = toInteger (rplMaxFrameSizeBytes limits)
    result <- sendLazyMessageWithLimit (fromInteger limit) transport encoded
    pure ((), either
      (Left . contextualizeEnvelopeTransportError envelope actual limit)
      Right result)

contextualizeEnvelopeTransportError
  :: RPCEnvelope
  -> Integer
  -> Integer
  -> TransportError
  -> TransportError
contextualizeEnvelopeTransportError envelope actual limit err =
  let context = "outgoing RPC type=" <> Text.pack (show (envType envelope))
        <> maybe "" (\requestId -> ", request=" <> Text.pack (show requestId)) (envRequestId envelope)
        <> ", actual=" <> Text.pack (show actual) <> " bytes"
        <> ", limit=" <> Text.pack (show limit) <> " bytes: "
  in case err of
      TransportFramingError message -> TransportFramingError (context <> message)
      TransportSendFailed message -> TransportSendFailed (context <> message)
      _ -> err

isLocalOutgoingFramingFailure :: TransportError -> Bool
isLocalOutgoingFramingFailure TransportFramingError{} = True
isLocalOutgoingFramingFailure _ = False

sendOneWay :: RPCConnection -> RPCEnvelope -> IO (Either RPCError ())
sendOneWay conn envelope = do
  result <- sendCorrelatedMessage
    (rpcPayloadLimits conn) (rpcSession conn) (rpcTransport conn) envelope
  case result of
    Left err -> do
      let rpcErr = RPCTransportError err
      unless (isLocalOutgoingFramingFailure err) $
        recordRuntimeFailureIfNeeded conn RPCFailureTransport rpcErr
      pure (Left rpcErr)
    Right () -> pure (Right ())

sendAndAwaitPendingResult
  :: Bool
  -> Maybe Int
  -> Text
  -> RPCConnection
  -> Transport
  -> RPCSession
  -> Word64
  -> MVar (Either RPCError RPCEnvelope)
  -> RPCEnvelope
  -> IO (Either RPCError RPCEnvelope)
sendAndAwaitPendingResult fatalTimeout mTimeout timeoutMessage conn transport session requestId done envelope = do
  result <- case mTimeout of
    Nothing -> Just <$> sendThenWait
    Just micros -> timeout micros sendThenWait
  case result of
    Nothing -> do
      _ <- removePending session requestId
      let err = RPCTimeout timeoutMessage
      when fatalTimeout (recordRuntimeFailure conn (RPCFailureRequest requestId) err)
      pure (Left err)
    Just value -> do
      case value of
        Left (RPCTransportError err) | isLocalOutgoingFramingFailure err -> pure ()
        _ -> do
          let source = case value of
                Left RPCTransportError{} -> RPCFailureTransport
                _ -> RPCFailureRequest requestId
          recordResultFailure conn source value
      pure value
  where
    sendThenWait = do
      sendResult <- sendCorrelatedMessage (rpcPayloadLimits conn) session transport envelope
      case sendResult of
        Left err -> do
          _ <- removePending session requestId
          pure (Left (RPCTransportError err))
        Right () -> do
          startRPCReceiver conn
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

dispatchIncomingEnvelope :: RPCSession -> RPCEnvelope -> IO (Maybe RPCError)
dispatchIncomingEnvelope session envelope =
  case envType envelope of
    MsgProgress -> lookupPending session envelope >>= \pending ->
      handleInterimEnvelope pending envelope
    MsgLog -> lookupPending session envelope >>= \pending ->
      handleInterimEnvelope pending envelope
    MsgExternalDataSourceOperationResult ->
      case envRequestId envelope of
        Nothing -> pure Nothing
        Just _ -> dispatchFinalEnvelope session envelope >> pure Nothing
    MsgStreamOpen -> unsupportedStreamFrame
    MsgStreamData -> unsupportedStreamFrame
    MsgStreamWindow -> unsupportedStreamFrame
    MsgStreamEnd -> unsupportedStreamFrame
    MsgStreamCancel -> unsupportedStreamFrame
    MsgStreamError -> unsupportedStreamFrame
    _ -> dispatchFinalEnvelope session envelope >> pure Nothing
  where
    unsupportedStreamFrame = pure (Just (RPCProtocolError
      "stream frame reached an RPC session without a registered stream adapter"))

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

handleInterimEnvelope :: Maybe RPCPending -> RPCEnvelope -> IO (Maybe RPCError)
handleInterimEnvelope mPending envelope =
  case envType envelope of
    MsgProgress ->
      case Aeson.fromJSON (envPayload envelope) of
        Aeson.Success progress -> do
          maybe (pure ()) (\pending -> ignoreCallbackException (rpOnProgress pending progress)) mPending
          pure Nothing
        Aeson.Error err -> pure (Just (RPCProtocolError
          ("invalid progress payload: " <> Text.pack err)))
    MsgLog ->
      case Aeson.fromJSON (envPayload envelope) of
        Aeson.Success logMsg -> do
          maybe (pure ()) (\pending -> ignoreCallbackException (rpOnLog pending logMsg)) mPending
          pure Nothing
        Aeson.Error err -> pure (Just (RPCProtocolError
          ("invalid log payload: " <> Text.pack err)))
    _ -> pure Nothing

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

recordResultFailure :: RPCConnection -> RPCFailureSource -> Either RPCError a -> IO ()
recordResultFailure conn source result =
  case result of
    Left err -> recordRuntimeFailureIfNeeded conn source err
    _ -> pure ()

recordRuntimeFailureIfNeeded :: RPCConnection -> RPCFailureSource -> RPCError -> IO ()
recordRuntimeFailureIfNeeded conn source err =
  when (isRuntimeConnectionFailure err) (recordRuntimeFailure conn source err)

recordRuntimeFailure :: RPCConnection -> RPCFailureSource -> RPCError -> IO ()
recordRuntimeFailure conn source err = do
  let slot = rpcRuntimeFailure conn
  eventIdentity <- atomicModifyIORef' (rpfsNextIdentity slot) $ \current ->
    let next = if current == maxBound then 1 else current + 1
    in (next, current)
  inserted <- modifyMVar (rpfsCurrent slot) $ \current ->
    case current of
      Nothing -> pure
        ( Just RPCFailureEvent
            { rpfeIdentity = eventIdentity
            , rpfeSource = source
            , rpfeError = err
            }
        , True
        )
      Just existing -> pure (Just existing, False)
  when inserted $ do
    _ <- tryPutMVar (rpfsWakeup slot) ()
    pure ()

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
invokeGeneratorWithProgress conn seed terrainData onProgress onLog =
  invokeGeneratorWithResolvedScope conn Nothing seed terrainData onProgress onLog

invokeGeneratorWithResolvedScope
  :: RPCConnection
  -> Maybe ResolvedInvocationScope
  -> Word64
  -> Value
  -> (PluginProgress -> IO ())
  -> (PluginLog -> IO ())
  -> IO (Either RPCError GeneratorResult)
invokeGeneratorWithResolvedScope conn maybeScope seed terrainData onProgress onLog
  | rpcProtocolVersion conn >= 5 = pure (Left (RPCProtocolError
      "protocol 5 generator invocation requires an explicit scoped stream adapter"))
  | generatorScopeDeclaration (rpcManifest conn) /= Nothing, maybeScope == Nothing =
      pure (Left (RPCProtocolError
        "scoped generator invocation requires exact host world facts; use rpcGeneratorStage"))
  | otherwise = do
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
              , igInvocationScope = invocationScopeBinding manifest InvocationGenerator maybeScope
              }
            , envRequestId = Nothing
            }
      result <- rpcCallWithProgress True (rpcRequestTimeoutMicros conn) "plugin generator request timed out" conn envelope onProgress onLog
      case result of
        Left err  -> pure (Left err)
        Right env -> trackRuntimeResult conn $ do
          validateScopedResultPayload manifest InvocationGenerator maybeScope (envPayload env)
          case Aeson.fromJSON (envPayload env) of
            Aeson.Success gr -> Right gr
            Aeson.Error err  -> Left (RPCProtocolError (Text.pack err))

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
invokeSimulation conn ctx overlay onProgress onLog
  | rpcProtocolVersion conn >= 5 = pure (Left (RPCProtocolError
      "protocol 5 simulation invocation requires an explicit scoped stream adapter"))
  | otherwise = case resolveSimulationInvocationScope conn ctx overlay of
      Left err -> pure (Left (RPCProtocolError err))
      Right scope -> invokeSimulationWithResolvedScope conn scope ctx overlay onProgress onLog

invokeSimulationWithResolvedScope
  :: RPCConnection
  -> ResolvedInvocationScope
  -> SimContext
  -> Overlay
  -> (PluginProgress -> IO ())
  -> (PluginLog -> IO ())
  -> IO (Either RPCError SimulationResult)
invokeSimulationWithResolvedScope conn scope ctx overlay onProgress onLog = do
  let manifest = rpcManifest conn
      policy = simulationPayloadPolicy manifest
      hasExplicitScope = simulationScopeDeclaration manifest /= Nothing
      terrainPayloadResult
        | hasExplicitScope, Set.null (risTerrainInputSections scope) = Right Null
        | hasExplicitScope = Payload.terrainWorldToScopedPayloadWithBudget
            (rsbTerrainBytes (risBudgets scope))
            (risTerrainInputSections scope)
            (risTerrainInputChunkIds scope)
            (scTerrain ctx)
        | sppIncludeTerrain policy = Payload.terrainWorldToPayloadWithLimits
            (rpcPayloadLimits conn) (scTerrain ctx)
        | otherwise = Right Null
      overlaysPayload
        | hasExplicitScope = scopedOverlaysToJSON
            (risDependencyOverlayChunkIds scope) (scOverlays ctx)
        | sppIncludeOverlays policy = overlaysToJSON (scOverlays ctx)
        | otherwise = Object mempty
      ownOverlayPayload
        | hasExplicitScope && risOwnedOverlayIdentity scope == Just (rmName manifest) =
            overlayToScopedJSON (risOwnOverlayReadChunkIds scope) overlay
        | hasExplicitScope = Null
        | sppIncludeOwnOverlay policy = overlayToJSON overlay
        | otherwise = Null
  case (terrainPayloadResult, validateOverlayInputBudget hasExplicitScope scope overlaysPayload ownOverlayPayload) of
    (Left err, _) -> pure (Left (RPCProtocolError err))
    (_, Left err) -> pure (Left (RPCProtocolError err))
    (Right terrainPayload, Right ()) -> do
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
              , isInvocationScope = invocationScopeBinding manifest InvocationSimulation (Just scope)
              }
            , envRequestId = Nothing
            }
      result <- rpcCallWithProgress True (rpcRequestTimeoutMicros conn) "plugin simulation request timed out" conn envelope onProgress onLog
      case result of
        Left err  -> pure (Left err)
        Right env -> trackRuntimeResult conn $ do
          validateScopedResultPayload manifest InvocationSimulation (Just scope) (envPayload env)
          case Aeson.fromJSON (envPayload env) of
            Aeson.Success sr -> Right sr
            Aeson.Error err  -> Left (RPCProtocolError (Text.pack err))

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
  let selectedProtocol = rpcProtocolVersion conn
      streamProposal = defaultStreamProposal
        (fromIntegral (rplMaxFrameSizeBytes (rpcPayloadLimits conn)))
      handshakeValue = Aeson.toJSON Handshake
        { hsProtocolVersion  = selectedProtocol
        , hsWorldPath        = worldPath
        , hsHostCapabilities = ["query", "mutate"] <> ["launch_auth" | Just _ <- [mAuth]]
        , hsAuthChallenge    = hacChallenge <$> mAuth
        }
      negotiatedHandshake = case handshakeValue of
        Object fields | selectedProtocol >= 5 -> Object
          (KM.insert "stream_v1" (Aeson.toJSON streamProposal) fields)
        value -> value
      envelope = RPCEnvelope
        { envType = MsgHandshake
        , envPayload = negotiatedHandshake
        , envRequestId = Nothing
        }
  result <- rpcCall
    True
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
            | haProtocolVersion ack /= selectedProtocol ->
                pure (Left (RPCProtocolError
                  ("plugin protocol version mismatch: selected="
                   <> Text.pack (show selectedProtocol)
                   <> ", plugin="
                   <> Text.pack (show (haProtocolVersion ack)))))
            | Just authErr <- validateHandshakeAuth mAuth ack ->
                pure (Left (RPCProtocolError authErr))
            | let resourceErrors = validateHandshakeDataResources (rpcManifest conn) (haResources ack)
            , not (null resourceErrors) ->
                pure (Left (RPCProtocolError
                  ("invalid handshake data resources: " <> renderManifestErrors resourceErrors)))
            | otherwise -> case validateHandshakeDataDirectory (rpcManifest conn) (haDataDirectory ack) of
                Left dirErr -> pure (Left (RPCProtocolError dirErr))
                Right normalizedDataDirectory ->
                  case negotiateHandshakeStream selectedProtocol streamProposal (envPayload env) of
                    Left streamErr -> pure (Left (RPCProtocolError streamErr))
                    Right negotiated -> pure (Right conn
                      { rpcProtocolVersion = haProtocolVersion ack
                      , rpcStreamV1 = negotiated
                      , rpcDataDirectory   = fmap Text.unpack normalizedDataDirectory
                      , rpcResources       = haResources ack
                      , rpcResourcesNegotiated = True
                      })
          Aeson.Error err -> pure (Left (RPCProtocolError (Text.pack err)))
      MsgError ->
        pure (Left (decodePluginErrorPayload (envPayload env)))
      other ->
        pure (Left (RPCProtocolError
          ("unexpected response to handshake: " <> Text.pack (show other))))

negotiateHandshakeStream
  :: Int
  -> StreamProposal
  -> Value
  -> Either Text (Maybe NegotiatedStreamV1)
negotiateHandshakeStream protocolVersion localProposal payload
  | protocolVersion <= 4 = Right Nothing
  | otherwise = do
      peerValue <- case payload of
        Object fields -> maybe (Left "protocol 5 handshake_ack omits stream_v1") Right
          (KM.lookup "stream_v1" fields)
        _ -> Left "protocol 5 handshake_ack payload is not an object"
      peerProposal <- case Aeson.fromJSON peerValue of
        Aeson.Error err -> Left ("invalid stream_v1 handshake acknowledgement: " <> Text.pack err)
        Aeson.Success proposal -> Right proposal
      Just <$> negotiateStreamV1 localProposal peerProposal

validateHandshakeAuth :: Maybe HandshakeAuthChallenge -> HandshakeAck -> Maybe Text
validateHandshakeAuth Nothing _ = Nothing
validateHandshakeAuth (Just expected) ack
  | haSessionId ack /= Just (hacSessionId expected) =
      Just "plugin launch session missing or mismatched during handshake"
  | haAuthProof ack /= Just (hacExpectedProof expected) =
      Just "plugin launch auth proof missing or mismatched during handshake"
  | otherwise = Nothing

validateHandshakeDataDirectory :: RPCManifest -> Maybe Text -> Either Text (Maybe Text)
validateHandshakeDataDirectory _ Nothing = Right Nothing
validateHandshakeDataDirectory manifest (Just rawDir) = do
  normalizedDir <- normalizeSafeHandshakeDataDirectory rawDir
  normalizedManifestDir <- case rmDataDirectory manifest of
    Nothing -> Left
      ("invalid handshake data_directory: manifest does not declare dataDirectory")
    Just manifestDir -> normalizeSafeHandshakeDataDirectory manifestDir
  if normalizedDir == normalizedManifestDir || childOf normalizedManifestDir normalizedDir
    then Right (Just normalizedDir)
    else Left
      ("invalid handshake data_directory: " <> quoteText normalizedDir
        <> " must match or narrow manifest dataDirectory " <> quoteText normalizedManifestDir)

normalizeSafeHandshakeDataDirectory :: Text -> Either Text Text
normalizeSafeHandshakeDataDirectory raw
  | Text.null raw = unsafeDirectory
  | Text.isPrefixOf "/" raw || Text.isPrefixOf "\\" raw = unsafeDirectory
  | Text.any (== ':') raw = unsafeDirectory
  | otherwise = case handshakePathSegments raw of
      [] -> unsafeDirectory
      segments
        | any unsafeHandshakePathSegment segments -> unsafeDirectory
        | otherwise -> Right (Text.intercalate "/" segments)
  where
    unsafeDirectory = Left
      ("invalid handshake data_directory: must be a safe relative path without absolute roots, drive prefixes, '.', or '..' segments: "
        <> quoteText raw)

handshakePathSegments :: Text -> [Text]
handshakePathSegments = Text.splitOn "/" . Text.replace "\\" "/"

unsafeHandshakePathSegment :: Text -> Bool
unsafeHandshakePathSegment segment =
  Text.null segment || segment == "." || segment == ".."

childOf :: Text -> Text -> Bool
childOf parent child = (parent <> "/") `Text.isPrefixOf` child

quoteText :: Text -> Text
quoteText value = "'" <> value <> "'"

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
  result <- rpcCall True (rpcRequestTimeoutMicros conn) "plugin heartbeat timed out" conn envelope
  case result of
    Left err -> pure (Left err)
    Right env -> do
      decoded <- case envType env of
        MsgHeartbeat -> decodeRPCPayload env
        other -> pure (Left (RPCProtocolError ("unexpected heartbeat response: " <> Text.pack (show other))))
      trackRuntimeResult conn decoded

-- | Ask the plugin for a health snapshot.
checkHealth :: RPCConnection -> IO (Either RPCError HealthStatus)
checkHealth conn = do
  let envelope = RPCEnvelope
        { envType = MsgHealthCheck
        , envPayload = object []
        , envRequestId = Nothing
        }
  result <- rpcCall True (rpcRequestTimeoutMicros conn) "plugin health check timed out" conn envelope
  case result of
    Left err -> pure (Left err)
    Right env -> do
      decoded <- case envType env of
        MsgHealthStatus -> decodeRPCPayload env
        other -> pure (Left (RPCProtocolError ("unexpected health response: " <> Text.pack (show other))))
      trackRuntimeResult conn decoded

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
  result <- rpcCall False (rpcRequestTimeoutMicros conn) "plugin external data-source grant timed out" conn envelope
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
      env >>= trackRuntimeResult conn

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
  result <- rpcCall False (rpcRequestTimeoutMicros conn) "plugin external data-source revocation timed out" conn envelope
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
      env >>= trackRuntimeResult conn

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
  result <- rpcCall False (rpcRequestTimeoutMicros conn) "plugin external data-source status request timed out" conn envelope
  case result of
    Left err -> pure (Left err)
    Right env -> do
      decoded <- case envType env of
        MsgExternalDataSourceStatus -> decodeRPCPayload env
        other -> pure (Left (RPCProtocolError ("unexpected external data-source status response: " <> Text.pack (show other))))
      recordResultFailure conn RPCFailureTransport decoded
      pure decoded

-- | Alias for 'requestExternalDataSourceStatus'.
checkExternalDataSourceStatus
  :: RPCConnection
  -> RPCExternalDataSourceStatusRequest
  -> IO (Either RPCError RPCExternalDataSourceStatusReport)
checkExternalDataSourceStatus = requestExternalDataSourceStatus

trackRuntimeResult :: RPCConnection -> Either RPCError a -> IO (Either RPCError a)
trackRuntimeResult conn result = do
  recordResultFailure conn RPCFailureTransport result
  pure result

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
  result <- rpcCallWithProgress True (rpcRequestTimeoutMicros conn) "plugin data query timed out" conn envelope
              (\_ -> pure ())
              (\_ -> pure ())
  case result of
    Left err  -> pure (Left err)
    Right env -> do
      decoded <- case envType env of
        MsgQueryResult -> decodeRPCPayload env
        other -> pure (Left (RPCProtocolError
          ("unexpected data query response: " <> Text.pack (show other))))
      validated <- case decoded of
        Left err -> pure (Left err)
        Right queryResult -> case findResourceSchema (qrResource qr) (rpcResources conn) of
          Nothing -> pure (Right queryResult)
          Just schema -> case DataValidation.validateQueryResult schema qr queryResult of
            Nothing -> pure (Right queryResult)
            Just failure -> pure (Left (RPCDataResourceError (drfCode failure) (drfMessage failure)))
      trackRuntimeResult conn validated

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
  result <- rpcCallWithProgress True (rpcRequestTimeoutMicros conn) "plugin data mutation timed out" conn envelope
              (\_ -> pure ())
              (\_ -> pure ())
  case result of
    Left err  -> pure (Left err)
    Right env -> do
      decoded <- case envType env of
        MsgMutateResult -> decodeRPCPayload env
        other -> pure (Left (RPCProtocolError
          ("unexpected data mutation response: " <> Text.pack (show other))))
      trackRuntimeResult conn decoded

findResourceSchema :: Text -> [DataResourceSchema] -> Maybe DataResourceSchema
findResourceSchema resourceName = foldr (\schema found ->
  if drsName schema == resourceName then Just schema else found) Nothing

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
        case resolveGeneratorInvocationScope conn world of
          Left err ->
            throwError (PluginInvariantError ("rpc generator scope resolution failed: " <> err))
          Right scope -> case generatorStageTerrainPayload (rpcPayloadLimits conn) manifest scope world of
            Left err ->
              throwError (PluginInvariantError ("rpc generator encode failed: " <> err))
            Right terrainPayload -> do
              seed <- asks peSeed
              reportProgress <- asks peProgress
              result <- liftIO (invokeGeneratorWithResolvedScope conn (Just scope) seed terrainPayload
                (reportProgress . formatPluginProgressDetail (rmName manifest))
                (\_ -> pure ()))
              case result of
                Left err -> throwError (PluginInvariantError ("rpc generator failed: " <> rpcErrorText err))
                Right generatorResult ->
                  case applyGeneratorResult (rpcPayloadLimits conn) manifest (Just scope) world generatorResult of
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
            else case resolveSimulationInvocationScope conn ctx overlay of
              Left err -> pure (Left err)
              Right scope -> do
                result <- invokeSimulationWithResolvedScope conn scope ctx overlay (reportSimulationProgress ctx name) ignoreLog
                case result of
                  Left err -> pure (Left (rpcErrorText err))
                  Right sr -> pure $ do
                    nextOverlay <- validateSimulationOverlayOutput manifest scope overlay (srOverlay sr)
                    writes <- if simulationScopeDeclaration manifest == Nothing
                      then Payload.decodeTerrainWritesValueWithLimits
                        (rpcPayloadLimits conn) (srTerrainWrites sr)
                      else Payload.decodeTerrainWritesValueScopedWithLimits
                        (rpcPayloadLimits conn) scope (scTerrain ctx) (srTerrainWrites sr)
                    Right (nextOverlay, writes)
      }
    else SimNodeReader
      { snrId           = nodeId
      , snrOverlayName  = name
      , snrDependencies = deps
      , snrSchedule     = schedule
      , snrReadTick     = \ctx overlay -> do
          if not (sppRequireWriteOverlay policy)
            then pure (Left "manifest missing writeOverlay/writeWorld capability")
            else case resolveSimulationInvocationScope conn ctx overlay of
              Left err -> pure (Left err)
              Right scope -> do
                result <- invokeSimulationWithResolvedScope conn scope ctx overlay (reportSimulationProgress ctx name) ignoreLog
                pure $ case result of
                  Left err -> Left (rpcErrorText err)
                  Right sr -> do
                    rejectUnauthorizedTerrainWrites manifest (srTerrainWrites sr)
                    validateSimulationOverlayOutput manifest scope overlay (srOverlay sr)
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

generatorStageTerrainPayload
  :: RPCPayloadLimits
  -> RPCManifest
  -> ResolvedInvocationScope
  -> Topo.World.TerrainWorld
  -> Either Text Value
generatorStageTerrainPayload limits manifest scope world
  | generatorScopeDeclaration manifest /= Nothing
  , not (Set.null (risTerrainInputSections scope)) =
      Payload.terrainWorldToScopedPayloadWithBudget
        (rsbTerrainBytes (risBudgets scope))
        (risTerrainInputSections scope) (risTerrainInputChunkIds scope) world
  | generatorScopeDeclaration manifest /= Nothing = Right Null
  | canReadTerrain manifest = Payload.terrainWorldToPayloadWithLimits limits world
  | otherwise = Right Null

resolveGeneratorInvocationScope
  :: RPCConnection
  -> Topo.World.TerrainWorld
  -> Either Text ResolvedInvocationScope
resolveGeneratorInvocationScope conn world =
  firstScopeError $ resolveInvocationScope
    (rpcProtocolVersion conn)
    (generatorScopeDeclaration manifest)
    (rmCapabilities manifest)
    RPCInvocationContext
      { ricKind = InvocationGenerator
      , ricWorldChunkIds = worldChunkIds world
      , ricCallerChunkIds = Nothing
      , ricAllowsCallerChunks = False
      , ricDependencyOverlayNames = Set.empty
      , ricOverlayChunkIds = Map.empty
      , ricOwnedOverlayName = ownedName
      , ricOwnOverlayChunkIds = maybe IntSet.empty overlayChunkIds ownedOverlay
      , ricDataResourceDeclarations = Map.empty
      , ricDataResourceRequest = Nothing
      , ricAvailableBudgets = invocationBudgets conn
      }
  where
    manifest = rpcManifest conn
    ownedName = if manifestHasOverlay manifest then Just (rmName manifest) else Nothing
    ownedOverlay = ownedName >>= \name -> lookupOverlay name (Topo.World.twOverlays world)

resolveSimulationInvocationScope
  :: RPCConnection
  -> SimContext
  -> Overlay
  -> Either Text ResolvedInvocationScope
resolveSimulationInvocationScope conn ctx ownOverlay = do
  scope <- firstScopeError $ resolveInvocationScope
    (rpcProtocolVersion conn)
    (simulationScopeDeclaration manifest)
    (rmCapabilities manifest)
    RPCInvocationContext
      { ricKind = InvocationSimulation
      , ricWorldChunkIds = worldIds
      , ricCallerChunkIds = Nothing
      , ricAllowsCallerChunks = False
      , ricDependencyOverlayNames = Map.keysSet (scOverlays ctx)
      , ricOverlayChunkIds = fmap overlayChunkIds (scOverlays ctx)
      , ricOwnedOverlayName = Just (rmName manifest)
      , ricOwnOverlayChunkIds = overlayChunkIds ownOverlay
      , ricDataResourceDeclarations = Map.empty
      , ricDataResourceRequest = Nothing
      , ricAvailableBudgets = invocationBudgets conn
      }
  when (simulationScopeDeclaration manifest /= Nothing) $ do
    unless (risOwnedOverlayIdentity scope == Just (rmName manifest)) $
      Left "scoped simulation must authorize output for the plugin-owned overlay"
    unless (risOwnOverlayReadChunkIds scope == overlayChunkIds ownOverlay) $
      Left "scoped simulation writing a whole overlay must receive the complete own overlay"
    unless (risOwnOverlayWriteChunkIds scope == worldIds) $
      Left "scoped simulation whole-overlay output must authorize the complete world chunk domain"
  Right scope
  where
    manifest = rpcManifest conn
    worldIds = worldChunkIds (scTerrain ctx)

worldChunkIds :: Topo.World.TerrainWorld -> IntSet.IntSet
worldChunkIds = IntMap.keysSet . Topo.World.twTerrain

invocationBudgets :: RPCConnection -> RPCScopeBudgets
invocationBudgets conn = RPCScopeBudgets
  { rsbTerrainBytes = fromIntegral (rplMaxDecodedTerrainBytes (rpcPayloadLimits conn))
  , rsbOverlayBytes = fromIntegral (rplMaxFrameSizeBytes (rpcPayloadLimits conn))
  , rsbOutputBytes = fromIntegral (rplMaxFrameSizeBytes (rpcPayloadLimits conn))
  }

generatorScopeDeclaration :: RPCManifest -> Maybe RPCInvocationScopeDecl
generatorScopeDeclaration manifest = rmInvocationScopes manifest >>= riscGenerator

simulationScopeDeclaration :: RPCManifest -> Maybe RPCInvocationScopeDecl
simulationScopeDeclaration manifest = rmInvocationScopes manifest >>= riscSimulation

invocationScopeBinding
  :: RPCManifest
  -> RPCInvocationKind
  -> Maybe ResolvedInvocationScope
  -> Maybe RPCInvocationScopeBinding
invocationScopeBinding manifest kind maybeScope
  | declarationForKind manifest kind == Nothing = Nothing
  | otherwise = fmap (\scope -> RPCInvocationScopeBinding (risScopeId scope) (Just scope)) maybeScope

declarationForKind :: RPCManifest -> RPCInvocationKind -> Maybe RPCInvocationScopeDecl
declarationForKind manifest InvocationGenerator = generatorScopeDeclaration manifest
declarationForKind manifest InvocationSimulation = simulationScopeDeclaration manifest
declarationForKind _ InvocationDataResource = Nothing

firstScopeError :: Either ScopeError a -> Either Text a
firstScopeError = either (Left . renderScopeError) Right

renderScopeError :: ScopeError -> Text
renderScopeError err = sePath err <> ": " <> seMessage err

validateOverlayInputBudget
  :: Bool
  -> ResolvedInvocationScope
  -> Value
  -> Value
  -> Either Text ()
validateOverlayInputBudget False _ _ _ = Right ()
validateOverlayInputBudget True scope dependencies ownOverlay =
  let actual = fromIntegral (BL.length (Aeson.encode (object
        [ "dependencies" .= dependencies
        , "own" .= ownOverlay
        ]))) :: Word64
      limit = rsbOverlayBytes (risBudgets scope)
  in when (actual > limit) $ Left
      ("scoped overlay input exceeds resolved budget: actual=" <> Text.pack (show actual)
        <> " bytes, limit=" <> Text.pack (show limit) <> " bytes")

validateScopedResultPayload
  :: RPCManifest
  -> RPCInvocationKind
  -> Maybe ResolvedInvocationScope
  -> Value
  -> Either RPCError ()
validateScopedResultPayload manifest kind maybeScope payload
  | declarationForKind manifest kind == Nothing = Right ()
  | otherwise = case maybeScope of
      Nothing -> Left (RPCProtocolError "scoped result validation is missing the host-resolved scope")
      Just scope -> case payload of
        Object fields -> do
          let allowed = Set.fromList (case kind of
                InvocationGenerator -> ["terrain", "overlay", "metadata"]
                InvocationSimulation -> ["overlay", "terrain_writes"]
                InvocationDataResource -> [])
              unknown = filter (`Set.notMember` allowed) (map Key.toText (KM.keys fields))
              actual = fromIntegral (BL.length (Aeson.encode payload)) :: Word64
              limit = rsbOutputBytes (risBudgets scope)
          unless (null unknown) $ Left (RPCProtocolError
            ("scoped result contains unsupported keys: " <> Text.intercalate ", " unknown))
          when (actual > limit) $ Left (RPCProtocolError
            ("scoped result exceeds resolved output budget: actual=" <> Text.pack (show actual)
              <> " bytes, limit=" <> Text.pack (show limit) <> " bytes"))
        _ -> Left (RPCProtocolError "scoped result payload must be an object")

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

scopedOverlaysToJSON :: Map Text IntSet.IntSet -> Map Text Overlay -> Value
scopedOverlaysToJSON grants overlays = object
  [ Key.fromText name .= overlayToScopedJSON chunkIds overlay
  | (name, chunkIds) <- Map.toList grants
  , Just overlay <- [Map.lookup name overlays]
  ]

rpcErrorText :: RPCError -> Text
rpcErrorText rpcError =
  case rpcError of
    RPCTransportError transportError -> Text.pack (show transportError)
    RPCProtocolError message -> message
    RPCPluginError code message -> "plugin error " <> Text.pack (show code) <> ": " <> message
    RPCDataResourceError code message -> dataResourceFailureText (DataResourceFailure code message)
    RPCTimeout message -> "timeout: " <> message

applyGeneratorResult
  :: RPCPayloadLimits
  -> RPCManifest
  -> Maybe ResolvedInvocationScope
  -> Topo.World.TerrainWorld
  -> GeneratorResult
  -> Either Text Topo.World.TerrainWorld
applyGeneratorResult limits manifest maybeScope world result = do
  validateGeneratorMetadataOutput manifest maybeScope (grMetadata result)
  worldWithTerrain <- case (generatorScopeDeclaration manifest, maybeScope) of
    (Just _, Just scope) -> Payload.applyGeneratorTerrainValueScopedWithLimits
      limits scope world (grTerrain result)
    _ -> Payload.applyGeneratorTerrainValueWithLimits limits world (grTerrain result)
  applyGeneratorOverlayPayload manifest maybeScope worldWithTerrain (grOverlay result)

validateGeneratorMetadataOutput
  :: RPCManifest
  -> Maybe ResolvedInvocationScope
  -> Maybe Value
  -> Either Text ()
validateGeneratorMetadataOutput _ _ Nothing = Right ()
validateGeneratorMetadataOutput _ _ (Just Null) = Right ()
validateGeneratorMetadataOutput manifest _ (Just _)
  | generatorScopeDeclaration manifest == Nothing = Right ()
  | otherwise = Left
      ("plugin " <> rmName manifest
        <> " returned generator metadata, but scoped metadata output has no bounded host consumer")

terrainWorldToPayload :: Topo.World.TerrainWorld -> Either Text Value
terrainWorldToPayload = Payload.terrainWorldToPayload

terrainWorldToPayloadWithLimits :: RPCPayloadLimits -> Topo.World.TerrainWorld -> Either Text Value
terrainWorldToPayloadWithLimits = Payload.terrainWorldToPayloadWithLimits

terrainWorldToScopedPayload
  :: Set.Set TerrainSection
  -> IntSet.IntSet
  -> Topo.World.TerrainWorld
  -> Either Text Value
terrainWorldToScopedPayload = Payload.terrainWorldToScopedPayload

terrainWorldToScopedPayloadWithLimits
  :: RPCPayloadLimits
  -> Set.Set TerrainSection
  -> IntSet.IntSet
  -> Topo.World.TerrainWorld
  -> Either Text Value
terrainWorldToScopedPayloadWithLimits = Payload.terrainWorldToScopedPayloadWithLimits

terrainWorldToCompletePayload :: Topo.World.TerrainWorld -> Either Text Value
terrainWorldToCompletePayload = Payload.terrainWorldToCompletePayload

terrainWorldToCompletePayloadWithLimits :: RPCPayloadLimits -> Topo.World.TerrainWorld -> Either Text Value
terrainWorldToCompletePayloadWithLimits = Payload.terrainWorldToCompletePayloadWithLimits

decodeTerrainWritesValue :: Maybe Value -> Either Text TerrainWrites
decodeTerrainWritesValue = Payload.decodeTerrainWritesValue

decodeTerrainWritesValueWithLimits :: RPCPayloadLimits -> Maybe Value -> Either Text TerrainWrites
decodeTerrainWritesValueWithLimits = Payload.decodeTerrainWritesValueWithLimits

applyGeneratorTerrainValue :: Topo.World.TerrainWorld -> Value -> Either Text Topo.World.TerrainWorld
applyGeneratorTerrainValue = Payload.applyGeneratorTerrainValue

applyGeneratorTerrainValueWithLimits
  :: RPCPayloadLimits
  -> Topo.World.TerrainWorld
  -> Value
  -> Either Text Topo.World.TerrainWorld
applyGeneratorTerrainValueWithLimits = Payload.applyGeneratorTerrainValueWithLimits

encodeBase64Text :: BS.ByteString -> Text
encodeBase64Text = Payload.encodeBase64Text

decodeBase64Text :: Text -> Either Text BS.ByteString
decodeBase64Text = Payload.decodeBase64Text

applyGeneratorOverlayPayload
  :: RPCManifest
  -> Maybe ResolvedInvocationScope
  -> Topo.World.TerrainWorld
  -> Maybe Value
  -> Either Text Topo.World.TerrainWorld
applyGeneratorOverlayPayload _ _ world Nothing = Right world
applyGeneratorOverlayPayload manifest maybeScope world (Just overlayValue)
  | not (manifestHasOverlay manifest) =
      Left ("plugin " <> rmName manifest
        <> " returned generator overlay output for overlay " <> rmName manifest
        <> " but manifest is missing overlay declaration")
  | not (canWriteOverlay manifest) =
      Left ("plugin " <> rmName manifest
        <> " returned generator overlay output for overlay " <> rmName manifest
        <> " but manifest is missing writeOverlay/writeWorld capability")
  | Just scope <- maybeScope
  , generatorScopeDeclaration manifest /= Nothing
  , risOwnedOverlayIdentity scope /= Just (rmName manifest) =
      Left ("plugin " <> rmName manifest <> " returned overlay output outside its resolved owned-overlay scope")
  | Just scope <- maybeScope
  , generatorScopeDeclaration manifest /= Nothing
  , risOwnOverlayWriteChunkIds scope /= worldChunkIds world =
      Left "generator whole-overlay output requires the complete world chunk domain"
  | otherwise =
      case lookupOverlay (rmName manifest) (Topo.World.twOverlays world) of
        Nothing -> Left ("plugin " <> rmName manifest
          <> " returned generator overlay output for overlay " <> rmName manifest
          <> " but host has no registered overlay surface")
        Just existingOverlay -> do
          decodedOverlay <- case maybeScope of
            Just _ | generatorScopeDeclaration manifest /= Nothing ->
              overlayFromScopedJSON (ovSchema existingOverlay) overlayValue
            _ -> overlayFromJSON (ovSchema existingOverlay) overlayValue
          case maybeScope of
            Just scope | generatorScopeDeclaration manifest /= Nothing ->
              rejectOverlayChunksOutsideScope scope decodedOverlay
            _ -> Right ()
          let nextOverlay = preserveHostProvenance existingOverlay decodedOverlay
              nextOverlays = insertOverlay nextOverlay (Topo.World.twOverlays world)
          Right world { Topo.World.twOverlays = nextOverlays }

validateSimulationOverlayOutput
  :: RPCManifest
  -> ResolvedInvocationScope
  -> Overlay
  -> Value
  -> Either Text Overlay
validateSimulationOverlayOutput manifest scope existing payload = do
  when (simulationScopeDeclaration manifest /= Nothing
      && risOwnedOverlayIdentity scope /= Just (rmName manifest)) $
    Left "simulation result is not authorized for the plugin-owned overlay"
  decoded <- if simulationScopeDeclaration manifest /= Nothing
    then overlayFromScopedJSON (ovSchema existing) payload
    else overlayFromJSON (ovSchema existing) payload
  when (simulationScopeDeclaration manifest /= Nothing) $
    rejectOverlayChunksOutsideScope scope decoded
  Right (preserveHostProvenance existing decoded)

rejectOverlayChunksOutsideScope :: ResolvedInvocationScope -> Overlay -> Either Text ()
rejectOverlayChunksOutsideScope scope overlay =
  let unauthorized = overlayChunkIds overlay `IntSet.difference` risOwnOverlayWriteChunkIds scope
  in unless (IntSet.null unauthorized) $ Left
      ("overlay output contains unauthorized chunk IDs: "
        <> Text.intercalate ", " (map (Text.pack . show) (IntSet.toAscList unauthorized)))
