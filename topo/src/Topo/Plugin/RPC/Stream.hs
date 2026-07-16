{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Protocol-v5 application streaming core.  Framing remains the RPC
-- transport's four-byte little-endian length followed by one JSON envelope.
-- This module is deliberately independent of generator/simulation callbacks:
-- callers register parent requests and feed the resulting effects to their
-- existing serialized writer.
module Topo.Plugin.RPC.Stream
  ( StreamRole(..)
  , StreamCodec(..)
  , StreamPayloadKind(..)
  , StreamProposal(..)
  , NegotiatedStreamV1(..)
  , defaultStreamProposal
  , negotiateStreamV1
  , StreamId(..)
  , StreamRecordKey(..)
  , StreamOpen(..)
  , StreamRecord(..)
  , StreamEnd(..)
  , StreamCancel(..)
  , StreamProtocolError(..)
  , StreamEnvelope(..)
  , StreamFailureClass(..)
  , StreamFailure(..)
  , StreamEffect(..)
  , StreamMachine
  , newStreamMachine
  , registerStreamRequest
  , markParentResultReceived
  , parentResultReady
  , finishStreamRequest
  , openOutboundStream
  , sendOutboundRecord
  , endOutboundStream
  , receiveStreamEnvelope
  , receiveStreamEnvelopeWithFrameBytes
  , consumeStreamRecord
  , completedStreamRecords
  , cancelStreamRequest
  , expireStreams
  , disconnectStreams
  , activeStreamCount
  , activeRequestCount
  , encodeStreamRecord
  , streamEnvelopeFrameBytes
  , streamRecordDigest
  , streamRecordsDigest
  , streamFailureEffects
  ) where

import Control.Monad (forM_, unless, when)
import Crypto.Hash (Context, Digest, SHA256, hash, hashFinalize, hashInit, hashUpdate)
import qualified Codec.Compression.Zstd as Zstd
import Data.Aeson
  ( FromJSON(..), ToJSON(..), Value, (.:), (.:?), (.=), object
  , withObject, withText
  )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntSet as IntSet
import Data.Foldable (toList)
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Maybe (isJust)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Word (Word32, Word64)
import GHC.Generics (Generic)

import Topo.Plugin.RPC.Scope
  ( RPCScopeBudgets(..), ResolvedInvocationScope(..), TerrainSection(..) )

-- | The host owns odd IDs and the plugin owns even IDs. Zero is never valid.
data StreamRole = StreamHost | StreamPlugin
  deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON StreamRole where
  toJSON StreamHost = "host"
  toJSON StreamPlugin = "plugin"
instance FromJSON StreamRole where
  parseJSON = withText "StreamRole" $ \t -> case t of
    "host" -> pure StreamHost
    "plugin" -> pure StreamPlugin
    _ -> fail "stream role must be host or plugin"

data StreamCodec = StreamIdentity | StreamZstd
  deriving (Eq, Ord, Show, Read, Generic)
instance ToJSON StreamCodec where
  toJSON StreamIdentity = "identity"
  toJSON StreamZstd = "zstd"
instance FromJSON StreamCodec where
  parseJSON = withText "StreamCodec" $ \t -> case t of
    "identity" -> pure StreamIdentity
    "zstd" -> pure StreamZstd
    _ -> fail "unsupported stream codec"

data StreamPayloadKind = TerrainSnapshot | TerrainDelta
  deriving (Eq, Ord, Show, Read, Generic)
instance ToJSON StreamPayloadKind where
  toJSON TerrainSnapshot = "terrain_snapshot"
  toJSON TerrainDelta = "terrain_delta"
instance FromJSON StreamPayloadKind where
  parseJSON = withText "StreamPayloadKind" $ \t -> case t of
    "terrain_snapshot" -> pure TerrainSnapshot
    "terrain_delta" -> pure TerrainDelta
    _ -> fail "unsupported stream payload kind"

-- | Bounded stream_v1 handshake offer. Identity must always be offered.
data StreamProposal = StreamProposal
  { spVersion :: !Int
  , spCodecs :: !(Set StreamCodec)
  , spMaxFrameBytes :: !Word64
  , spMaxConcurrentStreams :: !Int
  , spMaxUncompressedBytes :: !Word64
  , spMaxItems :: !Word64
  , spReceiveWindowBytes :: !Word64
  , spMaxRequests :: !Int
  , spAggregateStagedBytes :: !Word64
  , spIdleTimeoutMicros :: !Word64
  } deriving (Eq, Show, Generic)

instance ToJSON StreamProposal where
  toJSON p = object
    [ "version" .= spVersion p, "codecs" .= Set.toAscList (spCodecs p)
    , "max_frame_bytes" .= spMaxFrameBytes p
    , "max_concurrent_streams" .= spMaxConcurrentStreams p
    , "max_uncompressed_bytes" .= spMaxUncompressedBytes p
    , "max_items" .= spMaxItems p
    , "receive_window_bytes" .= spReceiveWindowBytes p
    , "max_requests" .= spMaxRequests p
    , "aggregate_staged_bytes" .= spAggregateStagedBytes p
    , "idle_timeout_micros" .= spIdleTimeoutMicros p
    ]
instance FromJSON StreamProposal where
  parseJSON = withObject "StreamProposal" $ \o -> StreamProposal
    <$> o .: "version" <*> (Set.fromList <$> o .: "codecs")
    <*> o .: "max_frame_bytes" <*> o .: "max_concurrent_streams"
    <*> o .: "max_uncompressed_bytes" <*> o .: "max_items"
    <*> o .: "receive_window_bytes" <*> o .: "max_requests"
    <*> o .: "aggregate_staged_bytes" <*> o .: "idle_timeout_micros"

data NegotiatedStreamV1 = NegotiatedStreamV1
  { nsvCodecs :: !(Set StreamCodec)
  , nsvMaxFrameBytes :: !Word64
  , nsvMaxConcurrentStreams :: !Int
  , nsvMaxUncompressedBytes :: !Word64
  , nsvMaxItems :: !Word64
  , nsvReceiveWindowBytes :: !Word64
  , nsvMaxRequests :: !Int
  , nsvAggregateStagedBytes :: !Word64
  , nsvIdleTimeoutMicros :: !Word64
  } deriving (Eq, Show, Generic)

instance ToJSON NegotiatedStreamV1 where
  toJSON n = object
    [ "version" .= (1 :: Int), "codecs" .= Set.toAscList (nsvCodecs n)
    , "max_frame_bytes" .= nsvMaxFrameBytes n
    , "max_concurrent_streams" .= nsvMaxConcurrentStreams n
    , "max_uncompressed_bytes" .= nsvMaxUncompressedBytes n
    , "max_items" .= nsvMaxItems n
    , "receive_window_bytes" .= nsvReceiveWindowBytes n
    , "max_requests" .= nsvMaxRequests n
    , "aggregate_staged_bytes" .= nsvAggregateStagedBytes n
    , "idle_timeout_micros" .= nsvIdleTimeoutMicros n
    ]
instance FromJSON NegotiatedStreamV1 where
  parseJSON = withObject "NegotiatedStreamV1" $ \o -> do
    version <- o .: "version"
    unless (version == (1 :: Int)) (fail "unsupported stream version")
    negotiated <- NegotiatedStreamV1 <$> (Set.fromList <$> o .: "codecs")
      <*> o .: "max_frame_bytes" <*> o .: "max_concurrent_streams"
      <*> o .: "max_uncompressed_bytes" <*> o .: "max_items"
      <*> o .: "receive_window_bytes" <*> o .: "max_requests"
      <*> o .: "aggregate_staged_bytes" <*> o .: "idle_timeout_micros"
    unless (Set.member StreamIdentity (nsvCodecs negotiated))
      (fail "negotiated stream limits omit identity")
    unless (and [nsvMaxFrameBytes negotiated > 0,
                 nsvMaxUncompressedBytes negotiated > 0,
                 nsvMaxItems negotiated > 0,
                 nsvReceiveWindowBytes negotiated > 0,
                 nsvAggregateStagedBytes negotiated > 0,
                 nsvIdleTimeoutMicros negotiated > 0]
            && nsvMaxConcurrentStreams negotiated > 0
            && nsvMaxRequests negotiated > 0)
      (fail "negotiated stream limits contain a non-positive bound")
    pure negotiated

defaultStreamProposal :: Word64 -> StreamProposal
defaultStreamProposal frameBytes = StreamProposal
  { spVersion = 1
  , spCodecs = Set.singleton StreamIdentity
  , spMaxFrameBytes = frameBytes
  , spMaxConcurrentStreams = 16
  , spMaxUncompressedBytes = 256 * 1024 * 1024
  , spMaxItems = 1024 * 1024
  , spReceiveWindowBytes = min frameBytes (4 * 1024 * 1024)
  , spMaxRequests = 64
  , spAggregateStagedBytes = 64 * 1024 * 1024
  , spIdleTimeoutMicros = 30 * 1000 * 1000
  }

-- | Intersect features and take the lower value for every resource bound.
-- Invalid, zero, or identity-free offers are rejected rather than repaired.
negotiateStreamV1 :: StreamProposal -> StreamProposal -> Either Text NegotiatedStreamV1
negotiateStreamV1 local peer = do
  validateProposal "local" local
  validateProposal "peer" peer
  -- Identity is the only codec enabled by this core. StreamZstd remains a
  -- reserved wire value until exact single-frame boundary validation can be
  -- guaranteed by the compression backend.
  let codecs = Set.intersection (Set.singleton StreamIdentity)
        (Set.intersection (spCodecs local) (spCodecs peer))
  unless (Set.member StreamIdentity codecs) (Left "stream_v1 identity codec is mandatory")
  pure NegotiatedStreamV1
    { nsvCodecs = codecs
    , nsvMaxFrameBytes = min (spMaxFrameBytes local) (spMaxFrameBytes peer)
    , nsvMaxConcurrentStreams = min (spMaxConcurrentStreams local) (spMaxConcurrentStreams peer)
    , nsvMaxUncompressedBytes = min (spMaxUncompressedBytes local) (spMaxUncompressedBytes peer)
    , nsvMaxItems = min (spMaxItems local) (spMaxItems peer)
    , nsvReceiveWindowBytes = min (spReceiveWindowBytes local) (spReceiveWindowBytes peer)
    , nsvMaxRequests = min (spMaxRequests local) (spMaxRequests peer)
    , nsvAggregateStagedBytes = min (spAggregateStagedBytes local) (spAggregateStagedBytes peer)
    , nsvIdleTimeoutMicros = min (spIdleTimeoutMicros local) (spIdleTimeoutMicros peer)
    }
  where
    validateProposal who p = do
      unless (spVersion p == 1) (Left (who <> " stream version is not stream_v1"))
      unless (Set.member StreamIdentity (spCodecs p)) (Left (who <> " stream offer omits identity"))
      unless (and [spMaxFrameBytes p > 0, spMaxUncompressedBytes p > 0,
                   spMaxItems p > 0, spReceiveWindowBytes p > 0,
                   spAggregateStagedBytes p > 0, spIdleTimeoutMicros p > 0]
              && spMaxConcurrentStreams p > 0 && spMaxRequests p > 0)
        (Left (who <> " stream offer contains a non-positive bound"))

newtype StreamId = StreamId { unStreamId :: Word64 }
  deriving (Eq, Ord, Show, Read, Generic)
instance ToJSON StreamId where toJSON = toJSON . unStreamId
instance FromJSON StreamId where
  parseJSON v = do
    value <- parseJSON v
    if value == (0 :: Word64) then fail "stream ID zero is reserved" else pure (StreamId value)

data StreamRecordKey = StreamRecordKey
  { srkSection :: !TerrainSection
  , srkChunkId :: !Int
  , srkPart :: !Word32
  , srkOffset :: !Word64
  } deriving (Eq, Ord, Show, Read, Generic)
instance ToJSON StreamRecordKey where
  toJSON k = object ["section" .= srkSection k, "chunk_id" .= srkChunkId k,
                     "part" .= srkPart k, "offset" .= srkOffset k]
instance FromJSON StreamRecordKey where
  parseJSON = withObject "StreamRecordKey" $ \o -> StreamRecordKey
    <$> o .: "section" <*> o .: "chunk_id" <*> o .: "part" <*> o .: "offset"

data StreamOpen = StreamOpen
  { soStreamId :: !StreamId
  , soParentRequestId :: !Word64
  , soScopeId :: !Text
  , soPayloadKind :: !StreamPayloadKind
  , soPayloadVersion :: !Int
  , soSections :: !(Set TerrainSection)
  , soChunkIds :: !IntSet.IntSet
  , soMetadata :: !Value
  , soCodec :: !StreamCodec
  , soTotalItems :: !(Maybe Word64)
  , soTotalBytes :: !(Maybe Word64)
  , soFinalSha256 :: !(Maybe Text)
  } deriving (Eq, Show, Generic)
instance ToJSON StreamOpen where
  toJSON x = object ["stream_id" .= soStreamId x, "request_id" .= soParentRequestId x,
    "scope_id" .= soScopeId x, "payload_kind" .= soPayloadKind x,
    "payload_version" .= soPayloadVersion x,
    "sections" .= Set.toAscList (soSections x),
    "chunk_ids" .= IntSet.toAscList (soChunkIds x), "metadata" .= soMetadata x,
    "codec" .= soCodec x, "total_items" .= soTotalItems x,
    "total_bytes" .= soTotalBytes x, "sha256" .= soFinalSha256 x]
instance FromJSON StreamOpen where
  parseJSON = withObject "StreamOpen" $ \o -> StreamOpen <$> o .: "stream_id"
    <*> o .: "request_id" <*> o .: "scope_id" <*> o .: "payload_kind"
    <*> o .: "payload_version" <*> (Set.fromList <$> o .: "sections")
    <*> (IntSet.fromList <$> o .: "chunk_ids") <*> o .: "metadata" <*> o .: "codec"
    <*> o .:? "total_items" <*> o .:? "total_bytes" <*> o .:? "sha256"

data StreamRecord = StreamRecord
  { srStreamId :: !StreamId
  , srParentRequestId :: !Word64
  , srSequence :: !Word64
  , srKey :: !StreamRecordKey
  , srCompressedLength :: !Word64
  , srUncompressedLength :: !Word64
  , srData :: !Text
  , srSha256 :: !Text
  } deriving (Eq, Show, Generic)
instance ToJSON StreamRecord where
  toJSON x = object ["stream_id" .= srStreamId x, "request_id" .= srParentRequestId x,
    "seq" .= srSequence x, "key" .= srKey x,
    "compressed_length" .= srCompressedLength x,
    "uncompressed_length" .= srUncompressedLength x,
    "data" .= srData x, "sha256" .= srSha256 x]
instance FromJSON StreamRecord where
  parseJSON = withObject "StreamRecord" $ \o -> StreamRecord <$> o .: "stream_id"
    <*> o .: "request_id" <*> o .: "seq" <*> o .: "key"
    <*> o .: "compressed_length" <*> o .: "uncompressed_length"
    <*> o .: "data" <*> o .: "sha256"

data StreamEnd = StreamEnd
  { seStreamId :: !StreamId, seParentRequestId :: !Word64
  , seTotalItems :: !Word64, seTotalBytes :: !Word64, seSha256 :: !Text
  } deriving (Eq, Show, Generic)
instance ToJSON StreamEnd where
  toJSON x = object
    ["stream_id" .= seStreamId x, "request_id" .= seParentRequestId x,
     "total_items" .= seTotalItems x, "total_bytes" .= seTotalBytes x, "sha256" .= seSha256 x]
instance FromJSON StreamEnd where
  parseJSON = withObject "StreamEnd" $ \o -> StreamEnd
    <$> o .: "stream_id" <*> o .: "request_id" <*> o .: "total_items"
    <*> o .: "total_bytes" <*> o .: "sha256"

data StreamCancel = StreamCancel
  { scStreamId :: !StreamId, scParentRequestId :: !Word64, scReason :: !Text }
  deriving (Eq, Show, Generic)
instance ToJSON StreamCancel where
  toJSON x = object
    ["stream_id" .= scStreamId x, "request_id" .= scParentRequestId x, "reason" .= scReason x]
instance FromJSON StreamCancel where
  parseJSON = withObject "StreamCancel" $ \o -> StreamCancel
    <$> o .: "stream_id" <*> o .: "request_id" <*> o .: "reason"

data StreamProtocolError = StreamProtocolError
  { speStreamId :: !StreamId, speParentRequestId :: !Word64
  , speCode :: !Text, speMessage :: !Text }
  deriving (Eq, Show, Generic)
instance ToJSON StreamProtocolError where
  toJSON x = object
    ["stream_id" .= speStreamId x, "request_id" .= speParentRequestId x,
     "code" .= speCode x, "message" .= speMessage x]
instance FromJSON StreamProtocolError where
  parseJSON = withObject "StreamProtocolError" $ \o -> StreamProtocolError
    <$> o .: "stream_id" <*> o .: "request_id" <*> o .: "code" <*> o .: "message"

data StreamEnvelope
  = StreamOpenEnvelope !StreamOpen
  | StreamDataEnvelope !StreamRecord
  | StreamWindowEnvelope !StreamId !Word64 !Word64
  | StreamEndEnvelope !StreamEnd
  | StreamCancelEnvelope !StreamCancel
  | StreamErrorEnvelope !StreamProtocolError
  deriving (Eq, Show, Generic)
instance ToJSON StreamEnvelope where
  toJSON frame = case frame of
    StreamOpenEnvelope x -> tagged "stream_open" (toJSON x)
    StreamDataEnvelope x -> tagged "stream_data" (toJSON x)
    StreamWindowEnvelope sid requestId credit -> tagged "stream_window" (object
      ["stream_id" .= sid, "request_id" .= requestId, "credit" .= credit])
    StreamEndEnvelope x -> tagged "stream_end" (toJSON x)
    StreamCancelEnvelope x -> tagged "stream_cancel" (toJSON x)
    StreamErrorEnvelope x -> tagged "stream_error" (toJSON x)
    where tagged t payload = object
            ["id" .= streamEnvelopeRequestId frame, "type" .= (t :: Text), "payload" .= payload]
instance FromJSON StreamEnvelope where
  parseJSON = withObject "StreamEnvelope" $ \o -> do
    outerRequestId <- o .: "id"
    tag <- o .: "type"
    payload <- o .: "payload"
    frame <- case (tag :: Text) of
      "stream_open" -> StreamOpenEnvelope <$> parseJSON payload
      "stream_data" -> StreamDataEnvelope <$> parseJSON payload
      "stream_window" -> withObject "StreamWindow" (\p -> StreamWindowEnvelope
        <$> p .: "stream_id" <*> p .: "request_id" <*> p .: "credit") payload
      "stream_end" -> StreamEndEnvelope <$> parseJSON payload
      "stream_cancel" -> StreamCancelEnvelope <$> parseJSON payload
      "stream_error" -> StreamErrorEnvelope <$> parseJSON payload
      _ -> fail "unknown stream envelope type"
    unless (outerRequestId == streamEnvelopeRequestId frame)
      (fail "stream envelope id does not match payload request_id")
    pure frame

streamEnvelopeRequestId :: StreamEnvelope -> Word64
streamEnvelopeRequestId frame = case frame of
  StreamOpenEnvelope x -> soParentRequestId x
  StreamDataEnvelope x -> srParentRequestId x
  StreamWindowEnvelope _ requestId _ -> requestId
  StreamEndEnvelope x -> seParentRequestId x
  StreamCancelEnvelope x -> scParentRequestId x
  StreamErrorEnvelope x -> speParentRequestId x

streamEnvelopeStreamId :: StreamEnvelope -> StreamId
streamEnvelopeStreamId frame = case frame of
  StreamOpenEnvelope x -> soStreamId x
  StreamDataEnvelope x -> srStreamId x
  StreamWindowEnvelope sid _ _ -> sid
  StreamEndEnvelope x -> seStreamId x
  StreamCancelEnvelope x -> scStreamId x
  StreamErrorEnvelope x -> speStreamId x

-- | Canonical encoded frame bytes, including the outer request correlation ID.
-- Integrations should also set the transport receive bound to the negotiated
-- frame limit so oversized input is rejected before payload allocation.
streamEnvelopeFrameBytes :: StreamEnvelope -> Word64
streamEnvelopeFrameBytes = fromIntegral . BL.length . Aeson.encode

data StreamFailureClass = StreamRequestFailure | StreamConnectionCorruption
  deriving (Eq, Ord, Show, Read, Generic)
data StreamFailure = StreamFailure
  { sfClass :: !StreamFailureClass
  , sfRequestId :: !(Maybe Word64)
  , sfStreamId :: !(Maybe StreamId)
  , sfMessage :: !Text
  , sfReplyWithError :: !Bool
    -- ^ Whether a locally detected request failure needs a stream_error reply.
  , sfRecoveryEffects :: ![StreamEffect]
    -- ^ Window grants made possible by deterministic request cleanup.
  } deriving (Eq, Show, Generic)

data StreamEffect
  = GrantStreamWindow !StreamId !Word64 !Word64
  | DeliverStreamRecord !StreamId !Word64 !StreamRecordKey !BS.ByteString
  | SendStreamCancel !StreamCancel
  | SendStreamError !StreamProtocolError
  | ParentRequestCancelled !Word64 !Text
  | StreamCompleted !StreamId !Word64
  | ConnectionMustClose !Text
  deriving (Eq, Show)

data StreamDirection = Inbound | Outbound deriving (Eq, Show)
data StreamStatus = StreamActive | StreamEnded deriving (Eq, Show)
data StreamState = StreamState
  { ssOpen :: !StreamOpen, ssDirection :: !StreamDirection, ssStatus :: !StreamStatus
  , ssNextSequence :: !Word64, ssLastKey :: !(Maybe StreamRecordKey)
  , ssCreditGranted :: !Word64, ssCreditUsed :: !Word64
  , ssItems :: !Word64, ssBytes :: !Word64
  , ssHash :: !(Context SHA256)
  , ssStaged :: !(Seq (StreamRecordKey, BS.ByteString))
  , ssCompleted :: !(Seq StreamRecordKey)
  , ssLastActivity :: !Word64
  }
data ParentState = ParentState
  { psScope :: !ResolvedInvocationScope, psDeadline :: !Word64
  , psStreams :: !(Set StreamId), psReferencedStreams :: !(Maybe (Set StreamId))
  , psTerrainBytes :: !Word64, psOutputBytes :: !Word64
  , psTerrainReserved :: !Word64, psOutputReserved :: !Word64, psFailed :: !Bool
  } deriving (Eq, Show)
data StreamMachine = StreamMachine
  { smLocalRole :: !StreamRole, smLimits :: !NegotiatedStreamV1
  , smStreams :: !(Map StreamId StreamState)
  , smHighestHostId :: !Word64, smHighestPluginId :: !Word64
  , smRetiredStreams :: !(Map StreamId Word64)
  , smParents :: !(Map Word64 ParentState), smAggregateStaged :: !Word64
  , smInboundReserved :: !Word64
  }

newStreamMachine :: StreamRole -> NegotiatedStreamV1 -> StreamMachine
newStreamMachine role limits = StreamMachine role limits Map.empty 0 0 Map.empty Map.empty 0 0

registerStreamRequest :: Word64 -> ResolvedInvocationScope -> Word64 -> StreamMachine -> Either StreamFailure StreamMachine
registerStreamRequest requestId scope deadline machine
  | requestId == 0 = Left (connectionFailure "parent request ID zero is reserved")
  | Map.member requestId (smParents machine) = Left (connectionFailure "duplicate parent request ID")
  | Map.size (smParents machine) >= nsvMaxRequests (smLimits machine) =
      Left (requestFailure requestId Nothing "concurrent stream request quota exceeded")
  | Text.null (risScopeId scope) = Left (requestFailure requestId Nothing "resolved scope ID is empty")
  | otherwise = Right machine { smParents = Map.insert requestId
      (ParentState scope deadline Set.empty Nothing 0 0 0 0 False) (smParents machine) }

markParentResultReceived :: Word64 -> Set StreamId -> StreamMachine -> Either StreamFailure StreamMachine
markParentResultReceived requestId referenced machine = do
  parent <- lookupParent requestId machine
  when (isJust (psReferencedStreams parent))
    (Left (connectionFailureFor requestId (StreamId 0) "duplicate parent result"))
  when (Set.size referenced > nsvMaxConcurrentStreams (smLimits machine))
    (Left (requestFailure requestId Nothing "parent references too many streams"))
  when (any ((== 0) . unStreamId) (Set.toList referenced))
    (Left (requestFailure requestId Nothing "parent references reserved stream ID zero"))
  unless (psStreams parent `Set.isSubsetOf` referenced)
    (Left (requestFailure requestId Nothing "parent result omits an opened stream"))
  forM_ (Set.toList referenced) $ \sid -> case Map.lookup sid (smStreams machine) of
    Just state -> unless (soParentRequestId (ssOpen state) == requestId)
      (Left (requestFailure requestId (Just sid) "parent references a stream owned by another request"))
    Nothing -> pure ()
  pure machine { smParents = Map.insert requestId
    parent { psReferencedStreams = Just referenced } (smParents machine) }

parentResultReady :: Word64 -> StreamMachine -> Bool
parentResultReady requestId machine = case Map.lookup requestId (smParents machine) of
  Nothing -> False
  Just parent -> not (psFailed parent) && case psReferencedStreams parent of
    Nothing -> False
    Just referenced -> all streamEndedAndConsumed (Set.toList referenced)
  where
    streamEndedAndConsumed sid = maybe False
      (\state -> soParentRequestId (ssOpen state) == requestId
        && ssStatus state == StreamEnded && null (ssStaged state))
      (Map.lookup sid (smStreams machine))

-- | Release a successful parent and all decoded/spooled records only after the
-- parent result and every referenced stream are final.
finishStreamRequest :: Word64 -> StreamMachine -> Either StreamFailure StreamMachine
finishStreamRequest requestId machine = do
  parent <- lookupParent requestId machine
  unless (parentResultReady requestId machine)
    (Left (requestFailure requestId Nothing "parent result is not stream-complete"))
  let streams = Set.toList (psStreams parent)
      retired = retireStreams streams machine
  pure retired
    { smStreams = foldr Map.delete (smStreams retired) streams
    , smParents = Map.delete requestId (smParents retired)
    }

openOutboundStream :: Word64 -> StreamOpen -> StreamMachine -> Either StreamFailure StreamMachine
openOutboundStream now open machine = do
  ensureOutboundFrame machine (StreamOpenEnvelope open)
  openStream now Outbound open machine

sendOutboundRecord :: Word64 -> StreamRecord -> BS.ByteString -> StreamMachine -> Either StreamFailure StreamMachine
sendOutboundRecord now record raw machine = do
  ensureOutboundFrame machine (StreamDataEnvelope record)
  state <- lookupStream (srStreamId record) machine
  unless (ssDirection state == Outbound) (Left (connectionFor record "data sent on inbound stream"))
  validateRecordHeader machine state record
  unless (srUncompressedLength record <= availableCredit state)
    (Left (connectionFor record "stream data exceeds receiver credit"))
  let limits = smLimits machine
  when (srCompressedLength record > nsvMaxFrameBytes limits
        || srUncompressedLength record > nsvMaxUncompressedBytes limits
        || ssBytes state > nsvMaxUncompressedBytes limits - srUncompressedLength record
        || ssItems state >= nsvMaxItems limits)
    (Left (requestFor record "outbound record exceeds negotiated quotas"))
  parent <- lookupParent (srParentRequestId record) machine
  validateScopeRecord parent (soPayloadKind (ssOpen state)) record
  validateParentAvailableByteBudget parent (soPayloadKind (ssOpen state)) record
  validateCanonicalBase64DeclaredLength (srCompressedLength record) (srData record)
    `mapLeft` requestFor record
  encoded <- canonicalBase64Decode (srData record) `mapLeft` requestFor record
  unless (fromIntegral (BS.length encoded) == srCompressedLength record)
    (Left (requestFor record "outbound compressed length mismatch"))
  decoded <- decodeRecordCodec (soCodec (ssOpen state)) (srUncompressedLength record) encoded
    `mapLeft` requestFor record
  unless (decoded == raw && fromIntegral (BS.length raw) == srUncompressedLength record)
    (Left (requestFor record "outbound raw data or length mismatch"))
  unless (streamRecordDigest raw == srSha256 record)
    (Left (requestFor record "outbound record digest mismatch"))
  let state' = advanceState now record raw state
      parent' = addParentBytes (soPayloadKind (ssOpen state)) (srUncompressedLength record) parent
  pure machine
    { smStreams = Map.insert (srStreamId record) state' (smStreams machine)
    , smParents = Map.insert (srParentRequestId record) parent' (smParents machine)
    }

endOutboundStream :: Word64 -> StreamEnd -> StreamMachine -> Either StreamFailure StreamMachine
endOutboundStream now end machine = do
  ensureOutboundFrame machine (StreamEndEnvelope end)
  state <- lookupStream (seStreamId end) machine
  let requestId = seParentRequestId end; sid = seStreamId end; open = ssOpen state
      digest = finalStreamDigest (ssHash state) (ssItems state) (ssBytes state)
  unless (ssDirection state == Outbound && ssStatus state == StreamActive)
    (Left (connectionFailureFor requestId sid "outbound end targets a non-active stream"))
  unless (soParentRequestId open == requestId)
    (Left (connectionFailureFor requestId sid "outbound end parent request mismatch"))
  unless (seTotalItems end == ssItems state && seTotalBytes end == ssBytes state
          && seSha256 end == digest)
    (Left (requestFailure requestId (Just sid) "outbound stream end integrity mismatch"))
  maybe (pure ()) (\n -> unless (n == ssItems state)
    (Left (requestFailure requestId (Just sid) "outbound item total differs from open"))) (soTotalItems open)
  maybe (pure ()) (\n -> unless (n == ssBytes state)
    (Left (requestFailure requestId (Just sid) "outbound byte total differs from open"))) (soTotalBytes open)
  maybe (pure ()) (\h -> unless (h == digest)
    (Left (requestFailure requestId (Just sid) "outbound digest differs from open"))) (soFinalSha256 open)
  pure machine { smStreams = Map.insert sid
    state { ssStatus = StreamEnded, ssLastActivity = now } (smStreams machine) }

receiveStreamEnvelope :: Word64 -> StreamEnvelope -> StreamMachine
  -> (StreamMachine, Either StreamFailure [StreamEffect])
receiveStreamEnvelope now envelope =
  receiveStreamEnvelopeWithFrameBytes now (streamEnvelopeFrameBytes envelope) envelope

-- | Receive a decoded envelope while enforcing its exact original wire length.
-- Live transports must use this entry point with the length-prefix value;
-- 'receiveStreamEnvelope' is for canonical in-process values and tests.
receiveStreamEnvelopeWithFrameBytes
  :: Word64
  -> Word64
  -> StreamEnvelope
  -> StreamMachine
  -> (StreamMachine, Either StreamFailure [StreamEffect])
receiveStreamEnvelopeWithFrameBytes now actualFrameBytes envelope machine =
  case ensureInboundFrameBytes machine envelope actualFrameBytes
       >> applyEnvelope now envelope machine of
    Right result -> result
    Left failure -> cleanupFailure failure machine

applyEnvelope :: Word64 -> StreamEnvelope -> StreamMachine
  -> Either StreamFailure (StreamMachine, Either StreamFailure [StreamEffect])
applyEnvelope now envelope machine = case envelope of
  StreamOpenEnvelope open -> do
    machine' <- openStream now Inbound open machine
    let sid = soStreamId open
        parent = smParents machine' Map.! soParentRequestId open
        credit = initialCredit machine' parent open
        state = (smStreams machine' Map.! sid) { ssCreditGranted = credit }
        parent' = addParentReservation (soPayloadKind open) credit parent
        withCredit = machine'
          { smStreams = Map.insert sid state (smStreams machine')
          , smParents = Map.insert (soParentRequestId open) parent' (smParents machine')
          , smInboundReserved = smInboundReserved machine' + credit
          }
    pure (withCredit, Right
      [GrantStreamWindow sid (soParentRequestId open) credit | credit > 0])
  StreamDataEnvelope record -> receiveRecord now record machine
  StreamWindowEnvelope sid requestId credit -> receiveWindow now sid requestId credit machine
  StreamEndEnvelope end -> receiveEnd now end machine
  StreamCancelEnvelope cancel -> receiveCancel cancel machine
  StreamErrorEnvelope err -> receivePeerError err machine

openStream :: Word64 -> StreamDirection -> StreamOpen -> StreamMachine -> Either StreamFailure StreamMachine
openStream now direction open machine = do
  let sid = soStreamId open; requestId = soParentRequestId open; limits = smLimits machine
  parent <- lookupParent requestId machine
  unless (validOwnedId (if direction == Inbound then opposite (smLocalRole machine) else smLocalRole machine) sid)
    (Left (connectionFailureFor requestId sid "stream ID violates role partition or is zero"))
  unless (unStreamId sid > highestIdFor owner machine)
    (Left (connectionFailureFor requestId sid "stream ID was reused or is not monotonic"))
  when (Map.size (smStreams machine) >= nsvMaxConcurrentStreams limits)
    (Left (requestFailure requestId (Just sid) "retained stream registry quota exceeded"))
  case psReferencedStreams parent of
    Just referenced | not (Set.member sid referenced) ->
      Left (requestFailure requestId (Just sid) "stream is not referenced by the parent result")
    _ -> pure ()
  unless (soScopeId open == risScopeId (psScope parent))
    (Left (requestFailure requestId (Just sid) "stream scope ID does not match parent request"))
  unless (soPayloadVersion open == 1)
    (Left (requestFailure requestId (Just sid) "unsupported stream payload version"))
  let (allowedSections, allowedChunks) = case soPayloadKind open of
        TerrainSnapshot -> (risTerrainInputSections (psScope parent), risTerrainInputChunkIds (psScope parent))
        TerrainDelta -> (risTerrainOutputSections (psScope parent), risTerrainOutputChunkIds (psScope parent))
  unless (soSections open `Set.isSubsetOf` allowedSections)
    (Left (requestFailure requestId (Just sid) "stream_open sections widen the resolved scope"))
  unless (soChunkIds open `IntSet.isSubsetOf` allowedChunks)
    (Left (requestFailure requestId (Just sid) "stream_open chunks widen the resolved scope"))
  unless (soCodec open == StreamIdentity
          && Set.member StreamIdentity (nsvCodecs limits))
    (Left (requestFailure requestId (Just sid) "stream codec is reserved or was not negotiated"))
  maybe (pure ()) (\n -> unless (n <= nsvMaxItems limits)
    (Left (requestFailure requestId (Just sid) "declared item total exceeds limit"))) (soTotalItems open)
  maybe (pure ()) (\n -> unless (n <= nsvMaxUncompressedBytes limits)
    (Left (requestFailure requestId (Just sid) "declared byte total exceeds limit"))) (soTotalBytes open)
  maybe (pure ()) (\n -> unless (n <= parentBudgetRemaining parent (soPayloadKind open))
    (Left (requestFailure requestId (Just sid) "declared byte total exceeds resolved scope budget"))) (soTotalBytes open)
  unless (soMetadata open == object [])
    (Left (requestFailure requestId (Just sid) "stream_open metadata must be empty in payload version 1"))
  let state = StreamState open direction StreamActive 0 Nothing 0 0 0 0 hashInit Seq.empty Seq.empty now
      parent' = parent { psStreams = Set.insert sid (psStreams parent) }
      machine' = setHighestId owner (unStreamId sid) machine
  pure machine'
    { smStreams = Map.insert sid state (smStreams machine')
    , smParents = Map.insert requestId parent' (smParents machine')
    }
  where owner = if direction == Inbound then opposite (smLocalRole machine) else smLocalRole machine

receiveRecord :: Word64 -> StreamRecord -> StreamMachine
  -> Either StreamFailure (StreamMachine, Either StreamFailure [StreamEffect])
receiveRecord now record machine = do
  state <- lookupStream (srStreamId record) machine
  unless (ssDirection state == Inbound) (Left (connectionFor record "received data on outbound stream"))
  validateRecordHeader machine state record
  unless (srUncompressedLength record <= availableCredit state)
    (Left (connectionFor record "stream data exceeds granted credit"))
  let limits = smLimits machine
      requestId = srParentRequestId record
  when (srCompressedLength record > nsvMaxFrameBytes limits)
    (Left (requestFor record "compressed record exceeds negotiated frame limit"))
  when (srUncompressedLength record > nsvMaxUncompressedBytes limits
        || ssBytes state > nsvMaxUncompressedBytes limits - srUncompressedLength record)
    (Left (requestFor record "uncompressed stream byte quota exceeded"))
  when (ssItems state >= nsvMaxItems limits)
    (Left (requestFor record "stream item quota exceeded"))
  parent <- lookupParent requestId machine
  validateScopeRecord parent (soPayloadKind (ssOpen state)) record
  let amount = srUncompressedLength record
      aggregateLimit = nsvAggregateStagedBytes limits
  unless (amount <= smInboundReserved machine)
    (Left (connectionFor record "record exceeds globally reserved receive credit"))
  unless (amount <= aggregateLimit && smAggregateStaged machine <= aggregateLimit - amount)
    (Left (requestFor record "aggregate decoded quota has no reserved capacity"))
  validateParentByteBudget parent (soPayloadKind (ssOpen state)) record
  validateCanonicalBase64DeclaredLength (srCompressedLength record) (srData record)
    `mapLeft` requestFor record
  encoded <- canonicalBase64Decode (srData record) `mapLeft` requestFor record
  unless (fromIntegral (BS.length encoded) == srCompressedLength record)
    (Left (requestFor record "declared compressed length mismatch"))
  raw <- decodeRecordCodec (soCodec (ssOpen state)) (srUncompressedLength record) encoded
    `mapLeft` requestFor record
  unless (streamRecordDigest raw == srSha256 record)
    (Left (requestFor record "record SHA-256 mismatch"))
  when (smAggregateStaged machine + srUncompressedLength record > nsvAggregateStagedBytes limits)
    (Left (requestFor record "aggregate staged byte quota exceeded"))
  let state' = (advanceState now record raw state)
        { ssStaged = ssStaged state Seq.|> (srKey record, raw) }
      parent' = addParentBytes (soPayloadKind (ssOpen state)) amount
        (removeParentReservation (soPayloadKind (ssOpen state)) amount parent)
      machine' = machine
        { smStreams = Map.insert (srStreamId record) state' (smStreams machine)
        , smParents = Map.insert requestId parent' (smParents machine)
        , smAggregateStaged = smAggregateStaged machine + amount
        , smInboundReserved = smInboundReserved machine - amount
        }
  pure (machine', Right [DeliverStreamRecord (srStreamId record) requestId (srKey record) raw])

validateRecordHeader :: StreamMachine -> StreamState -> StreamRecord -> Either StreamFailure ()
validateRecordHeader _ state record = do
  let open = ssOpen state
  unless (ssStatus state == StreamActive) (Left (connectionFor record "data follows terminal stream state"))
  unless (srParentRequestId record == soParentRequestId open)
    (Left (connectionFor record "stream frame parent request mismatch"))
  unless (srSequence record == ssNextSequence state)
    (Left (connectionFor record "stream sequence gap, duplicate, or reorder"))
  unless (Set.member (srkSection (srKey record)) (soSections open))
    (Left (requestFor record "record section was not declared by stream_open"))
  unless (IntSet.member (srkChunkId (srKey record)) (soChunkIds open))
    (Left (requestFor record "record chunk was not declared by stream_open"))
  case ssLastKey state of
    Just previous | srKey record <= previous ->
      Left (connectionFor record "record key is duplicate or non-canonical")
    _ -> pure ()

validateScopeRecord :: ParentState -> StreamPayloadKind -> StreamRecord -> Either StreamFailure ()
validateScopeRecord parent kind record = do
  let scope = psScope parent; key = srKey record
      (sections, chunks) = case kind of
        TerrainSnapshot -> (risTerrainInputSections scope, risTerrainInputChunkIds scope)
        TerrainDelta -> (risTerrainOutputSections scope, risTerrainOutputChunkIds scope)
  unless (Set.member (srkSection key) sections)
    (Left (requestFor record "record section is outside the exact resolved scope"))
  unless (IntSet.member (srkChunkId key) chunks)
    (Left (requestFor record "record chunk is outside the exact resolved scope"))

receiveWindow :: Word64 -> StreamId -> Word64 -> Word64 -> StreamMachine
  -> Either StreamFailure (StreamMachine, Either StreamFailure [StreamEffect])
receiveWindow now sid requestId credit machine = do
  state <- lookupStream sid machine
  unless (ssDirection state == Outbound && ssStatus state == StreamActive)
    (Left (connectionFailureFor requestId sid "window targets a non-active outbound stream"))
  unless (soParentRequestId (ssOpen state) == requestId)
    (Left (connectionFailureFor requestId sid "window parent request mismatch"))
  when (credit == 0 || maxBound - ssCreditGranted state < credit)
    (Left (connectionFailureFor requestId sid "invalid or overflowing stream credit"))
  let state' = state { ssCreditGranted = ssCreditGranted state + credit, ssLastActivity = now }
  pure (machine { smStreams = Map.insert sid state' (smStreams machine) }, Right [])

receiveEnd :: Word64 -> StreamEnd -> StreamMachine
  -> Either StreamFailure (StreamMachine, Either StreamFailure [StreamEffect])
receiveEnd now end machine = case Map.lookup sid (smStreams machine) of
  Nothing -> retiredTerminalResult requestId sid "end targets unknown stream" machine
  Just state -> do
    let open = ssOpen state
    unless (ssDirection state == Inbound && ssStatus state == StreamActive)
      (Left (connectionFailureFor requestId sid "end targets a non-active inbound stream"))
    unless (soParentRequestId open == requestId)
      (Left (connectionFailureFor requestId sid "end parent request mismatch"))
    let digest = finalStreamDigest (ssHash state) (ssItems state) (ssBytes state)
    unless (seTotalItems end == ssItems state && seTotalBytes end == ssBytes state)
      (Left (requestFailure requestId (Just sid) "stream end totals mismatch"))
    unless (seSha256 end == digest)
      (Left (requestFailure requestId (Just sid) "stream final SHA-256 mismatch"))
    maybe (pure ()) (\n -> unless (n == ssItems state)
      (Left (requestFailure requestId (Just sid) "stream item total differs from open declaration"))) (soTotalItems open)
    maybe (pure ()) (\n -> unless (n == ssBytes state)
      (Left (requestFailure requestId (Just sid) "stream byte total differs from open declaration"))) (soTotalBytes open)
    maybe (pure ()) (\h -> unless (h == digest)
      (Left (requestFailure requestId (Just sid) "stream digest differs from open declaration"))) (soFinalSha256 open)
    let releasedCredit = availableCredit state
        state' = state { ssStatus = StreamEnded, ssLastActivity = now }
        parent = smParents machine Map.! requestId
        parent' = removeParentReservation (soPayloadKind open) releasedCredit parent
    let released = machine
          { smStreams = Map.insert sid state' (smStreams machine)
          , smParents = Map.insert requestId parent' (smParents machine)
          , smInboundReserved = smInboundReserved machine - releasedCredit
          }
        (redistributed, grants) = redistributeInboundCredit released
    pure (redistributed, Right (StreamCompleted sid requestId : grants))
  where
    requestId = seParentRequestId end
    sid = seStreamId end

receivePeerError :: StreamProtocolError -> StreamMachine
  -> Either StreamFailure (StreamMachine, Either StreamFailure [StreamEffect])
receivePeerError err machine = case Map.lookup sid (smStreams machine) of
  Nothing -> retiredTerminalResult requestId sid "stream_error targets unknown stream" machine
  Just state -> do
    unless (ssStatus state == StreamActive)
      (Left (connectionFailureFor requestId sid "stream_error targets a terminal stream"))
    unless (soParentRequestId (ssOpen state) == requestId)
      (Left (connectionFailureFor requestId sid "stream_error parent request mismatch"))
    Left (peerRequestFailure requestId sid
      ("peer stream error " <> speCode err <> ": " <> speMessage err))
  where
    requestId = speParentRequestId err
    sid = speStreamId err

receiveCancel :: StreamCancel -> StreamMachine
  -> Either StreamFailure (StreamMachine, Either StreamFailure [StreamEffect])
receiveCancel cancel machine = case Map.lookup sid (smStreams machine) of
  Nothing -> case Map.lookup sid (smRetiredStreams machine) of
    Just retiredRequestId
      | retiredRequestId == requestId -> pure (machine, Right [])
      | otherwise -> Left (connectionFailureFor requestId sid "cancel parent request mismatch")
    Nothing -> Left (connectionFailureFor requestId sid "cancel targets unknown stream")
  Just state
    | soParentRequestId (ssOpen state) /= requestId ->
        Left (connectionFailureFor requestId sid "cancel parent request mismatch")
    | ssStatus state == StreamEnded -> pure (machine, Right [])
    | otherwise ->
        let (machine', effects) = cleanupRequest requestId
              ("peer cancelled stream: " <> scReason cancel) machine
        in pure (machine', Right effects)
  where sid = scStreamId cancel; requestId = scParentRequestId cancel

consumeStreamRecord :: Word64 -> StreamId -> StreamMachine -> Either StreamFailure (StreamMachine, [StreamEffect])
consumeStreamRecord now sid machine = do
  state <- lookupStream sid machine
  case Seq.viewl (ssStaged state) of
    Seq.EmptyL -> Left (connectionFailureFor (soParentRequestId (ssOpen state)) sid "no staged stream record to consume")
    (key, raw) Seq.:< rest -> do
      let amount = fromIntegral (BS.length raw); requestId = soParentRequestId (ssOpen state)
          remaining = nsvMaxUncompressedBytes (smLimits machine) - ssCreditGranted state
          declaredRemaining = maybe maxBound (\total -> total - ssCreditGranted state)
            (soTotalBytes (ssOpen state))
          replenish = if ssStatus state == StreamActive
            then min amount (min remaining declaredRemaining) else 0
          state' = state { ssStaged = rest, ssCompleted = ssCompleted state Seq.|> key
                         , ssCreditGranted = ssCreditGranted state + replenish, ssLastActivity = now }
          parent = smParents machine Map.! requestId
          parent' = addParentReservation (soPayloadKind (ssOpen state)) replenish parent
          machine' = machine
            { smStreams = Map.insert sid state' (smStreams machine)
            , smParents = Map.insert requestId parent' (smParents machine)
            , smAggregateStaged = smAggregateStaged machine - amount
            , smInboundReserved = smInboundReserved machine + replenish
            }
          effects = [GrantStreamWindow sid requestId replenish | replenish > 0 && ssStatus state == StreamActive]
      if ssStatus state == StreamActive
        then pure (machine', effects)
        else let (redistributed, grants) = redistributeInboundCredit machine'
             in pure (redistributed, grants)

completedStreamRecords :: StreamId -> StreamMachine -> Either StreamFailure [StreamRecordKey]
completedStreamRecords sid machine = do
  state <- lookupStream sid machine
  unless (ssStatus state == StreamEnded && null (ssStaged state))
    (Left (requestFailure (soParentRequestId (ssOpen state)) (Just sid) "stream is not fully ended and consumed"))
  pure (toList (ssCompleted state))

cancelStreamRequest :: Word64 -> Text -> StreamMachine -> (StreamMachine, [StreamEffect])
cancelStreamRequest = cleanupRequest

expireStreams :: Word64 -> StreamMachine -> (StreamMachine, [StreamEffect])
expireStreams now machine = foldl' expire (machine, []) expired
  where
    idle = nsvIdleTimeoutMicros (smLimits machine)
    expired = Set.toList $ Set.fromList
      [ soParentRequestId (ssOpen state)
      | state <- Map.elems (smStreams machine)
      , ssStatus state == StreamActive
      , now >= ssLastActivity state && now - ssLastActivity state >= idle
      ] <> Set.fromList
      [ requestId | (requestId, parent) <- Map.toList (smParents machine)
      , now >= psDeadline parent ]
    expire (m, effects) requestId = let (m', more) = cleanupRequest requestId "stream deadline expired" m
                                    in (m', effects <> more)

disconnectStreams :: Text -> StreamMachine -> (StreamMachine, [StreamEffect])
disconnectStreams reason machine =
  (machine { smStreams = Map.empty, smRetiredStreams = Map.empty
           , smParents = Map.empty, smAggregateStaged = 0, smInboundReserved = 0 }
  , [ParentRequestCancelled requestId reason | requestId <- Map.keys (smParents machine)])

activeStreamCount :: StreamMachine -> Int
activeStreamCount = length . filter ((== StreamActive) . ssStatus) . Map.elems . smStreams
activeRequestCount :: StreamMachine -> Int
activeRequestCount = Map.size . smParents

cleanupFailure :: StreamFailure -> StreamMachine -> (StreamMachine, Either StreamFailure [StreamEffect])
cleanupFailure failure machine = case sfClass failure of
  StreamConnectionCorruption ->
    let (clean, _) = disconnectStreams (sfMessage failure) machine
    in (clean, Left failure) -- caller closes connection; no partial result is retained
  StreamRequestFailure -> case sfRequestId failure of
    Nothing -> (machine, Left failure)
    Just requestId ->
      let (clean, effects) = cleanupRequest requestId (sfMessage failure) machine
          recovery = [effect | effect@(GrantStreamWindow _ _ _) <- effects]
      in (clean, Left failure { sfRecoveryEffects = recovery })

cleanupRequest :: Word64 -> Text -> StreamMachine -> (StreamMachine, [StreamEffect])
cleanupRequest requestId reason machine = case Map.lookup requestId (smParents machine) of
  Nothing -> (machine, [])
  Just parent ->
    let streams = Set.toList (psStreams parent)
        retired = retireStreams streams machine
        released = sum [sum (map (fromIntegral . BS.length . snd) (toList (ssStaged state)))
                       | sid <- streams, Just state <- [Map.lookup sid (smStreams machine)]]
        releasedCredit = sum [availableCredit state
                       | sid <- streams, Just state <- [Map.lookup sid (smStreams machine)]
                       , ssDirection state == Inbound, ssStatus state == StreamActive]
        streamMap = foldr Map.delete (smStreams machine) streams
        maxReasonChars = fromIntegral (min (fromIntegral (maxBound :: Int))
          (nsvMaxFrameBytes (smLimits machine) `div` 6))
        boundedReason = Text.take maxReasonChars reason
        cancelEffects =
          [ SendStreamCancel cancel
          | sid <- streams
          , let cancel = StreamCancel sid requestId boundedReason
          , streamEnvelopeFrameBytes (StreamCancelEnvelope cancel)
              <= nsvMaxFrameBytes (smLimits machine)
          ]
        effects = cancelEffects <> [ParentRequestCancelled requestId reason]
        cleaned = retired
          { smStreams = streamMap
          , smParents = Map.delete requestId (smParents retired)
          , smAggregateStaged = smAggregateStaged retired - released
          , smInboundReserved = smInboundReserved retired - releasedCredit
          }
        (redistributed, grants) = redistributeInboundCredit cleaned
    in (redistributed, effects <> grants)

-- Redistribute capacity released by an ended, consumed, or cancelled stream so
-- streams that initially opened with zero credit cannot remain permanently
-- stalled behind a completed peer.
redistributeInboundCredit :: StreamMachine -> (StreamMachine, [StreamEffect])
redistributeInboundCredit machine = foldl' grantOne (machine, []) candidates
  where
    candidates =
      [ sid
      | (sid, state) <- Map.toAscList (smStreams machine)
      , ssDirection state == Inbound
      , ssStatus state == StreamActive
      ]
    grantOne (current, effects) sid = case Map.lookup sid (smStreams current) of
      Nothing -> (current, effects)
      Just state -> case Map.lookup requestId (smParents current) of
        Nothing -> (current, effects)
        Just parent
          | credit == 0 -> (current, effects)
          | otherwise ->
              let state' = state { ssCreditGranted = ssCreditGranted state + credit }
                  parent' = addParentReservation kind credit parent
                  current' = current
                    { smStreams = Map.insert sid state' (smStreams current)
                    , smParents = Map.insert requestId parent' (smParents current)
                    , smInboundReserved = smInboundReserved current + credit
                    }
              in (current', effects <> [GrantStreamWindow sid requestId credit])
          where
            open = ssOpen state
            kind = soPayloadKind open
            window = nsvReceiveWindowBytes (smLimits current)
            windowRoom = if availableCredit state >= window
              then 0 else window - availableCredit state
            streamRoom = nsvMaxUncompressedBytes (smLimits current) - ssCreditGranted state
            declaredRoom = maybe maxBound (\total -> total - ssCreditGranted state)
              (soTotalBytes open)
            credit = minimum
              [ windowRoom, streamRoom, declaredRoom
              , aggregateCapacityRemaining current
              , parentBudgetRemaining parent kind
              ]
        where requestId = soParentRequestId (ssOpen state)

aggregateCapacityRemaining :: StreamMachine -> Word64
aggregateCapacityRemaining machine
  | used >= limit = 0
  | otherwise = limit - used
  where
    limit = nsvAggregateStagedBytes (smLimits machine)
    used = smAggregateStaged machine + smInboundReserved machine

parentBudgetRemaining :: ParentState -> StreamPayloadKind -> Word64
parentBudgetRemaining parent kind
  | used >= limit = 0
  | otherwise = limit - used
  where
    budgets = risBudgets (psScope parent)
    (limit, used) = case kind of
      TerrainSnapshot -> (rsbTerrainBytes budgets, psTerrainBytes parent + psTerrainReserved parent)
      TerrainDelta -> (rsbOutputBytes budgets, psOutputBytes parent + psOutputReserved parent)

validateParentByteBudget :: ParentState -> StreamPayloadKind -> StreamRecord -> Either StreamFailure ()
validateParentByteBudget parent kind record =
  unless (srUncompressedLength record <= reserved)
    (Left (requestFor record "record exceeds the resolved invocation byte budget"))
  where
    reserved = case kind of
      TerrainSnapshot -> psTerrainReserved parent
      TerrainDelta -> psOutputReserved parent

validateParentAvailableByteBudget :: ParentState -> StreamPayloadKind -> StreamRecord -> Either StreamFailure ()
validateParentAvailableByteBudget parent kind record =
  unless (srUncompressedLength record <= parentBudgetRemaining parent kind)
    (Left (requestFor record "record exceeds the resolved invocation byte budget"))

addParentBytes :: StreamPayloadKind -> Word64 -> ParentState -> ParentState
addParentBytes TerrainSnapshot amount parent = parent { psTerrainBytes = psTerrainBytes parent + amount }
addParentBytes TerrainDelta amount parent = parent { psOutputBytes = psOutputBytes parent + amount }

addParentReservation :: StreamPayloadKind -> Word64 -> ParentState -> ParentState
addParentReservation TerrainSnapshot amount parent = parent { psTerrainReserved = psTerrainReserved parent + amount }
addParentReservation TerrainDelta amount parent = parent { psOutputReserved = psOutputReserved parent + amount }

removeParentReservation :: StreamPayloadKind -> Word64 -> ParentState -> ParentState
removeParentReservation TerrainSnapshot amount parent = parent { psTerrainReserved = psTerrainReserved parent - amount }
removeParentReservation TerrainDelta amount parent = parent { psOutputReserved = psOutputReserved parent - amount }

initialCredit :: StreamMachine -> ParentState -> StreamOpen -> Word64
initialCredit machine parent open = minimum
  [ nsvReceiveWindowBytes (smLimits machine)
  , nsvMaxUncompressedBytes (smLimits machine)
  , maybe maxBound id (soTotalBytes open)
  , aggregateCapacityRemaining machine
  , parentBudgetRemaining parent (soPayloadKind open)
  ]

advanceState :: Word64 -> StreamRecord -> BS.ByteString -> StreamState -> StreamState
advanceState now record raw state = state
  { ssNextSequence = ssNextSequence state + 1, ssLastKey = Just (srKey record)
  , ssCreditUsed = ssCreditUsed state + srUncompressedLength record
  , ssItems = ssItems state + 1, ssBytes = ssBytes state + srUncompressedLength record
  , ssHash = hashUpdate (ssHash state) (canonicalRecordBytes (srKey record) raw)
  , ssLastActivity = now
  }

availableCredit :: StreamState -> Word64
availableCredit state = ssCreditGranted state - ssCreditUsed state

validOwnedId :: StreamRole -> StreamId -> Bool
validOwnedId role (StreamId value) = value /= 0 && odd value == (role == StreamHost)

ownerOfId :: StreamId -> StreamRole
ownerOfId (StreamId value) = if odd value then StreamHost else StreamPlugin

highestIdFor :: StreamRole -> StreamMachine -> Word64
highestIdFor StreamHost = smHighestHostId
highestIdFor StreamPlugin = smHighestPluginId

setHighestId :: StreamRole -> Word64 -> StreamMachine -> StreamMachine
setHighestId StreamHost value machine = machine { smHighestHostId = value }
setHighestId StreamPlugin value machine = machine { smHighestPluginId = value }

retireStreams :: [StreamId] -> StreamMachine -> StreamMachine
retireStreams streamIds machine = machine { smRetiredStreams = trimRetired combined }
  where
    additions = Map.fromList
      [ (sid, soParentRequestId (ssOpen state))
      | sid <- streamIds
      , Just state <- [Map.lookup sid (smStreams machine)]
      ]
    combined = Map.union additions (smRetiredStreams machine)
    limit = max 0 (nsvMaxConcurrentStreams (smLimits machine))
    trimRetired retired
      | Map.size retired <= limit = retired
      | otherwise = trimRetired (Map.deleteMin retired)

retiredTerminalResult
  :: Word64
  -> StreamId
  -> Text
  -> StreamMachine
  -> Either StreamFailure (StreamMachine, Either StreamFailure [StreamEffect])
retiredTerminalResult requestId sid unknownMessage machine =
  case Map.lookup sid (smRetiredStreams machine) of
    Just retiredRequestId
      | retiredRequestId == requestId -> Right (machine, Right [])
      | otherwise -> Left (connectionFailureFor requestId sid "terminal frame parent request mismatch")
    Nothing -> Left (connectionFailureFor requestId sid unknownMessage)

ensureInboundFrameBytes :: StreamMachine -> StreamEnvelope -> Word64 -> Either StreamFailure ()
ensureInboundFrameBytes machine envelope actualFrameBytes =
  when (actualFrameBytes > nsvMaxFrameBytes (smLimits machine))
    (Left (connectionFailureFor (streamEnvelopeRequestId envelope)
      (streamEnvelopeStreamId envelope) "stream frame exceeds negotiated frame limit"))

ensureOutboundFrame :: StreamMachine -> StreamEnvelope -> Either StreamFailure ()
ensureOutboundFrame machine envelope =
  when (streamEnvelopeFrameBytes envelope > nsvMaxFrameBytes (smLimits machine))
    (Left (requestFailure (streamEnvelopeRequestId envelope)
      (Just (streamEnvelopeStreamId envelope)) "stream frame exceeds negotiated frame limit"))

opposite :: StreamRole -> StreamRole
opposite StreamHost = StreamPlugin
opposite StreamPlugin = StreamHost

lookupParent :: Word64 -> StreamMachine -> Either StreamFailure ParentState
lookupParent requestId machine = maybe
  (Left (connectionFailureFor requestId (StreamId 0) "stream references unknown parent request")) Right
  (Map.lookup requestId (smParents machine))
lookupStream :: StreamId -> StreamMachine -> Either StreamFailure StreamState
lookupStream sid machine = maybe (Left (connectionFailure ("unknown or cleaned stream ID " <> Text.pack (show sid)))) Right
  (Map.lookup sid (smStreams machine))

encodeStreamRecord :: StreamCodec -> StreamId -> Word64 -> Word64 -> StreamRecordKey -> BS.ByteString -> StreamRecord
encodeStreamRecord codec sid requestId sequenceNo key raw = StreamRecord
  { srStreamId = sid, srParentRequestId = requestId, srSequence = sequenceNo, srKey = key
  , srCompressedLength = fromIntegral (BS.length encoded)
  , srUncompressedLength = fromIntegral (BS.length raw)
  , srData = TextEncoding.decodeUtf8 (Base64.encode encoded)
  , srSha256 = streamRecordDigest raw
  }
  where encoded = case codec of
          StreamIdentity -> raw
          StreamZstd -> Zstd.compress 3 raw

streamRecordDigest :: BS.ByteString -> Text
streamRecordDigest bytes = Text.pack (show (hash bytes :: Digest SHA256))

-- | Final digest for canonical ordered uncompressed records.
streamRecordsDigest :: [(StreamRecordKey, BS.ByteString)] -> Text
streamRecordsDigest records = finalStreamDigest context items bytes
  where
    context = foldl' (\ctx (key, raw) -> hashUpdate ctx (canonicalRecordBytes key raw))
      (hashInit :: Context SHA256) records
    items = fromIntegral (length records)
    bytes = sum (map (fromIntegral . BS.length . snd) records)

finalStreamDigest :: Context SHA256 -> Word64 -> Word64 -> Text
finalStreamDigest context items bytes = Text.pack (show digest)
  where
    digest = hashFinalize (hashUpdate context (canonicalTotalsBytes items bytes)) :: Digest SHA256

canonicalTotalsBytes :: Word64 -> Word64 -> BS.ByteString
canonicalTotalsBytes items bytes = BS.concat (map framed
  ["stream_v1_totals", TextEncoding.encodeUtf8 (Text.pack (show items)),
   TextEncoding.encodeUtf8 (Text.pack (show bytes))])
  where framed value = TextEncoding.encodeUtf8 (Text.pack (show (BS.length value))) <> ":" <> value

canonicalRecordBytes :: StreamRecordKey -> BS.ByteString -> BS.ByteString
canonicalRecordBytes key raw = BS.concat (map framed
  [ TextEncoding.encodeUtf8 (terrainSectionText (srkSection key))
  , TextEncoding.encodeUtf8 (Text.pack (show (srkChunkId key)))
  , TextEncoding.encodeUtf8 (Text.pack (show (srkPart key)))
  , TextEncoding.encodeUtf8 (Text.pack (show (srkOffset key)))
  , raw
  ])
  where framed value = TextEncoding.encodeUtf8 (Text.pack (show (BS.length value))) <> ":" <> value

terrainSectionText :: TerrainSection -> Text
terrainSectionText TerrainElevation = "terrain"
terrainSectionText TerrainClimate = "climate"
terrainSectionText TerrainVegetation = "vegetation"

validateCanonicalBase64DeclaredLength :: Word64 -> Text -> Either Text ()
validateCanonicalBase64DeclaredLength declared raw = do
  expected <- canonicalBase64Length declared
  unless (fromIntegral (Text.length raw) == expected)
    (Left "base64 text length does not match declared compressed length")
  where
    canonicalBase64Length bytes
      | bytes > maxBound - 2 = Left "declared compressed length overflows base64 size"
      | groups > maxBound `div` 4 = Left "declared compressed length overflows base64 size"
      | otherwise = Right (groups * 4)
      where groups = (bytes + 2) `div` 3

canonicalBase64Decode :: Text -> Either Text BS.ByteString
canonicalBase64Decode raw = do
  unless (Text.length raw `mod` 4 == 0) (Left "base64 length is not a multiple of four")
  let encoded = TextEncoding.encodeUtf8 raw
  decoded <- either (Left . Text.pack) Right (Base64.decode encoded)
  unless (Base64.encode decoded == encoded) (Left "base64 is not canonical")
  pure decoded

decodeRecordCodec :: StreamCodec -> Word64 -> BS.ByteString -> Either Text BS.ByteString
decodeRecordCodec StreamIdentity declared encoded
  | fromIntegral (BS.length encoded) /= declared = Left "identity uncompressed length mismatch"
  | otherwise = Right encoded
decodeRecordCodec StreamZstd _ _ =
  Left "zstd stream records are reserved until exact single-frame validation is available"

connectionFailure :: Text -> StreamFailure
connectionFailure message = StreamFailure StreamConnectionCorruption Nothing Nothing message False []
connectionFailureFor :: Word64 -> StreamId -> Text -> StreamFailure
connectionFailureFor requestId sid message =
  StreamFailure StreamConnectionCorruption (Just requestId) (Just sid) message False []
requestFailure :: Word64 -> Maybe StreamId -> Text -> StreamFailure
requestFailure requestId sid message =
  StreamFailure StreamRequestFailure (Just requestId) sid message True []
peerRequestFailure :: Word64 -> StreamId -> Text -> StreamFailure
peerRequestFailure requestId sid message =
  StreamFailure StreamRequestFailure (Just requestId) (Just sid) message False []
connectionFor :: StreamRecord -> Text -> StreamFailure
connectionFor r = connectionFailureFor (srParentRequestId r) (srStreamId r)
requestFor :: StreamRecord -> Text -> StreamFailure
requestFor r = requestFailure (srParentRequestId r) (Just (srStreamId r))

-- | Effects an integration layer should serialize after a failed receive.
streamFailureEffects :: StreamFailure -> [StreamEffect]
streamFailureEffects failure = baseEffects <> sfRecoveryEffects failure
  where
    baseEffects = case sfClass failure of
      StreamConnectionCorruption -> [ConnectionMustClose (sfMessage failure)]
      StreamRequestFailure -> case (sfRequestId failure, sfStreamId failure) of
        (Just requestId, Just sid)
          | sfReplyWithError failure ->
              [ SendStreamError (StreamProtocolError sid requestId "stream_rejected" (sfMessage failure))
              , ParentRequestCancelled requestId (sfMessage failure)
              ]
          | otherwise -> [ParentRequestCancelled requestId (sfMessage failure)]
        (Just requestId, Nothing) -> [ParentRequestCancelled requestId (sfMessage failure)]
        _ -> []

mapLeft :: Either Text a -> (Text -> StreamFailure) -> Either StreamFailure a
mapLeft value f = either (Left . f) Right value
