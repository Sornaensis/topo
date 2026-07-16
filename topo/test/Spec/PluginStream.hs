{-# LANGUAGE OverloadedStrings #-}

module Spec.PluginStream (spec) where

import Control.Concurrent.STM (atomically)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Word (Word64)
import Data.Aeson (object)
import Test.Hspec

import Topo.Plugin.RPC.Protocol
import Topo.Plugin.RPC.Scope
import Topo.Plugin.RPC.Stream

spec :: Spec
spec = describe "RPC protocol v5 stream_v1 core" $ do
  describe "version and negotiation" $ do
    it "selects the highest 4..5 overlap and downgrades to v4" $ do
      selectProtocolVersion 4 5 `shouldBe` Right 5
      selectProtocolVersion 3 4 `shouldBe` Right 4
      selectProtocolVersion 6 7 `shouldSatisfy` either (const True) (const False)

    it "intersects features and narrows every peer bound" $ do
      let local = (defaultStreamProposal 4096)
            { spCodecs = Set.fromList [StreamIdentity, StreamZstd]
            , spMaxConcurrentStreams = 8 }
          peer = (defaultStreamProposal 2048)
            { spMaxConcurrentStreams = 3, spReceiveWindowBytes = 512 }
      let negotiated = either (error . show) id (negotiateStreamV1 local peer)
      nsvCodecs negotiated `shouldBe` Set.singleton StreamIdentity
      nsvMaxFrameBytes negotiated `shouldBe` 2048
      nsvMaxConcurrentStreams negotiated `shouldBe` 3
      nsvReceiveWindowBytes negotiated `shouldBe` 512

    it "rejects an identity-free offer" $ do
      let peer = (defaultStreamProposal 4096) { spCodecs = Set.singleton StreamZstd }
      negotiateStreamV1 (defaultStreamProposal 4096) peer
        `shouldSatisfy` either (const True) (const False)

    it "keeps zstd reserved until exact single-frame decoding is available" $ do
      let offer = (defaultStreamProposal 4096)
            { spCodecs = Set.fromList [StreamIdentity, StreamZstd] }
          negotiated = either (error . show) id (negotiateStreamV1 offer offer)
      nsvCodecs negotiated `shouldBe` Set.singleton StreamIdentity

    it "enforces and releases the stream-event byte quota independently of event count" $ do
      quota <- newStreamByteQuotaIO 100
      atomically (tryReserveStreamBytes quota 60) `shouldReturn` True
      atomically (tryReserveStreamBytes quota 41) `shouldReturn` False
      atomically (streamBytesInUse quota) `shouldReturn` 60
      atomically (releaseStreamBytes quota 60)
      atomically (tryReserveStreamBytes quota 100) `shouldReturn` True
      atomically (clearStreamBytes quota)
      atomically (streamBytesInUse quota) `shouldReturn` 0
      let limits = negotiatedLimits
            { nsvMaxFrameBytes = 80
            , nsvReceiveWindowBytes = 40
            , nsvAggregateStagedBytes = 30
            }
      streamQueueByteCapacity limits `shouldBe` 220
      let frame = StreamDataEnvelope (encodeStreamRecord StreamIdentity (StreamId 2) 7 0
            (StreamRecordKey TerrainElevation 1 0 0) "abc")
          wireBytes = streamEnvelopeFrameBytes frame
          retained = streamQueuedFrameRetainedBytes wireBytes frame
      retained `shouldBe` wireBytes * 2 + 6
      frameQuota <- newStreamByteQuotaIO (retained * 2 - 1)
      atomically (tryReserveStreamBytes frameQuota retained) `shouldReturn` True
      atomically (tryReserveStreamBytes frameQuota retained) `shouldReturn` False
      atomically (releaseStreamBytes frameQuota retained)
      atomically (streamBytesInUse frameQuota) `shouldReturn` 0

    it "round-trips the outer request id and rejects payload mismatches" $ do
      let frame = StreamDataEnvelope (encodeStreamRecord StreamIdentity (StreamId 2) 7 0
            (StreamRecordKey TerrainElevation 1 0 0) "abc")
      Aeson.fromJSON (Aeson.toJSON frame) `shouldBe` Aeson.Success frame
      let mismatched = case Aeson.toJSON frame of
            Aeson.Object fields -> Aeson.Object (KM.insert "id" (Aeson.toJSON (8 :: Word64)) fields)
            value -> value
      (Aeson.fromJSON mismatched :: Aeson.Result StreamEnvelope)
        `shouldSatisfy` isAesonError

  describe "bidirectional state and scope binding" $ do
    it "rejects a role-partition violation as connection corruption" $ do
      let machine0 = registeredMachine
          badOpen = streamOpen (StreamId 3) "scope-1" TerrainSnapshot
          (_, result) = receiveStreamEnvelope 1 (StreamOpenEnvelope badOpen) machine0
      result `shouldSatisfy` isConnectionFailure

    it "validates exact scope before decoding record data" $ do
      let machine0 = registeredMachine
          open = streamOpen (StreamId 2) "scope-1" TerrainSnapshot
          (machine1, Right _) = receiveStreamEnvelope 1 (StreamOpenEnvelope open) machine0
          bad = (encodeStreamRecord StreamIdentity (StreamId 2) 7 0
            (StreamRecordKey TerrainElevation 99 0 0) "abc") { srData = "not base64" }
          (machine2, result) = receiveStreamEnvelope 2 (StreamDataEnvelope bad) machine1
      result `shouldSatisfy` isRequestFailure
      activeStreamCount machine2 `shouldBe` 0
      activeRequestCount machine2 `shouldBe` 0

    it "enforces credit, sequence, canonical keys, integrity, and final totals" $ do
      let limits = negotiatedLimits { nsvReceiveWindowBytes = 3 }
          machine0 = either (error . show) id
            (registerStreamRequest 7 resolvedScope 1000 (newStreamMachine StreamHost limits))
          open = streamOpen (StreamId 2) "scope-1" TerrainSnapshot
          (machine1, Right [GrantStreamWindow _ _ 3]) =
            receiveStreamEnvelope 1 (StreamOpenEnvelope open) machine0
          key = StreamRecordKey TerrainElevation 1 0 0
          record = encodeStreamRecord StreamIdentity (StreamId 2) 7 0 key "abc"
          (machine2, Right [_]) = receiveStreamEnvelope 2 (StreamDataEnvelope record) machine1
      let overCredit = encodeStreamRecord StreamIdentity (StreamId 2) 7 1
            (StreamRecordKey TerrainElevation 2 0 0) "z"
          (_, overResult) = receiveStreamEnvelope 3 (StreamDataEnvelope overCredit) machine2
      overResult `shouldSatisfy` isConnectionFailure

      let (machine3, effects) = either (error . show) id (consumeStreamRecord 3 (StreamId 2) machine2)
      effects `shouldBe` [GrantStreamWindow (StreamId 2) 7 3]
      let end = StreamEnd (StreamId 2) 7 1 3 (streamRecordsDigest [(key, "abc")])
          (machine4, endResult) = receiveStreamEnvelope 4 (StreamEndEnvelope end) machine3
      endResult `shouldBe` Right [StreamCompleted (StreamId 2) 7]
      let readyMachine = either (error . show) id
            (markParentResultReceived 7 (Set.singleton (StreamId 2)) machine4)
      parentResultReady 7 readyMachine `shouldBe` True
      completedStreamRecords (StreamId 2) machine4 `shouldBe` Right [key]

    it "rejects data before credit and duplicate stream IDs" $ do
      let machine0 = registeredMachine
          open = streamOpen (StreamId 2) "scope-1" TerrainSnapshot
          (machine1, Right _) = receiveStreamEnvelope 1 (StreamOpenEnvelope open) machine0
          (_, duplicateResult) = receiveStreamEnvelope 2 (StreamOpenEnvelope open) machine1
      duplicateResult `shouldSatisfy` isConnectionFailure

    it "does not complete a parent from another request's stream" $ do
      let first = registeredMachine
          second = either (error . show) id
            (registerStreamRequest 8 resolvedScope 1000 first)
          open = streamOpen (StreamId 2) "scope-1" TerrainSnapshot
          (opened, Right _) = receiveStreamEnvelope 1 (StreamOpenEnvelope open) second
          end = StreamEnd (StreamId 2) 7 0 0 (streamRecordsDigest [])
          (ended, Right _) = receiveStreamEnvelope 2 (StreamEndEnvelope end) opened
      let crossResult = markParentResultReceived 8 (Set.singleton (StreamId 2)) ended
      isRequestFailure crossResult `shouldBe` True

    it "waits for every referenced stream to open, end, and be consumed" $ do
      let referenced = Set.singleton (StreamId 2)
          machine0 = either (error . show) id
            (markParentResultReceived 7 referenced registeredMachine)
      parentResultReady 7 machine0 `shouldBe` False
      let open = streamOpen (StreamId 2) "scope-1" TerrainSnapshot
          (machine1, Right _) = receiveStreamEnvelope 1 (StreamOpenEnvelope open) machine0
          key = StreamRecordKey TerrainElevation 1 0 0
          record = encodeStreamRecord StreamIdentity (StreamId 2) 7 0 key "abc"
          (machine2, Right _) = receiveStreamEnvelope 2 (StreamDataEnvelope record) machine1
          end = StreamEnd (StreamId 2) 7 1 3 (streamRecordsDigest [(key, "abc")])
          (machine3, Right _) = receiveStreamEnvelope 3 (StreamEndEnvelope end) machine2
      parentResultReady 7 machine3 `shouldBe` False
      let (machine4, _) = either (error . show) id (consumeStreamRecord 4 (StreamId 2) machine3)
      parentResultReady 7 machine4 `shouldBe` True
      activeRequestCount (either (error . show) id (finishStreamRequest 7 machine4)) `shouldBe` 0

    it "reserves aggregate credit before decode and across concurrent opens" $ do
      let limits = negotiatedLimits { nsvAggregateStagedBytes = 3, nsvReceiveWindowBytes = 3 }
          start = either (error . show) id
            (registerStreamRequest 7 resolvedScope 1000 (newStreamMachine StreamHost limits))
          (one, Right firstEffects) = receiveStreamEnvelope 1
            (StreamOpenEnvelope (streamOpen (StreamId 2) "scope-1" TerrainSnapshot)) start
          (_, Right secondEffects) = receiveStreamEnvelope 1
            (StreamOpenEnvelope (streamOpen (StreamId 4) "scope-1" TerrainSnapshot)) one
      firstEffects `shouldBe` [GrantStreamWindow (StreamId 2) 7 3]
      secondEffects `shouldBe` []

    it "redistributes released aggregate credit to a zero-window stream" $ do
      let limits = negotiatedLimits { nsvAggregateStagedBytes = 3, nsvReceiveWindowBytes = 3 }
          start = either (error . show) id
            (registerStreamRequest 7 resolvedScope 1000 (newStreamMachine StreamHost limits))
          (one, Right _) = receiveStreamEnvelope 1
            (StreamOpenEnvelope (streamOpen (StreamId 2) "scope-1" TerrainSnapshot)) start
          (two, Right []) = receiveStreamEnvelope 1
            (StreamOpenEnvelope (streamOpen (StreamId 4) "scope-1" TerrainSnapshot)) one
          key = StreamRecordKey TerrainElevation 1 0 0
          record = encodeStreamRecord StreamIdentity (StreamId 2) 7 0 key "abc"
          (staged, Right _) = receiveStreamEnvelope 2 (StreamDataEnvelope record) two
          (consumed, _) = either (error . show) id (consumeStreamRecord 3 (StreamId 2) staged)
          end = StreamEnd (StreamId 2) 7 1 3 (streamRecordsDigest [(key, "abc")])
          (_, Right endEffects) = receiveStreamEnvelope 4 (StreamEndEnvelope end) consumed
      endEffects `shouldBe`
        [StreamCompleted (StreamId 2) 7, GrantStreamWindow (StreamId 4) 7 3]

    it "enforces resolved invocation budgets across fragmented streams" $ do
      let tinyScope = resolvedScope { risBudgets = RPCScopeBudgets 2 4096 2 }
          start = either (error . show) id
            (registerStreamRequest 7 tinyScope 1000 (newStreamMachine StreamHost negotiatedLimits))
          open = (streamOpen (StreamId 2) "scope-1" TerrainSnapshot) { soTotalBytes = Just 3 }
          (_, result) = receiveStreamEnvelope 1 (StreamOpenEnvelope open) start
      result `shouldSatisfy` isRequestFailure

    it "enforces resolved invocation budgets on outbound streams" $ do
      let tinyScope = resolvedScope { risBudgets = RPCScopeBudgets 2 4096 2 }
          start = either (error . show) id
            (registerStreamRequest 7 tinyScope 1000 (newStreamMachine StreamHost negotiatedLimits))
          open = (streamOpen (StreamId 1) "scope-1" TerrainSnapshot)
          opened = either (error . show) id (openOutboundStream 1 open start)
          (credited, Right []) = receiveStreamEnvelope 2
            (StreamWindowEnvelope (StreamId 1) 7 3) opened
          record = encodeStreamRecord StreamIdentity (StreamId 1) 7 0
            (StreamRecordKey TerrainElevation 1 0 0) "abc"
      isRequestFailure (sendOutboundRecord 3 record "abc" credited) `shouldBe` True

    it "enforces the negotiated bound against the complete JSON frame" $ do
      let open = streamOpen (StreamId 2) "scope-1" TerrainSnapshot
          frame = StreamOpenEnvelope open
          frameBytes = streamEnvelopeFrameBytes frame
          limits = negotiatedLimits { nsvMaxFrameBytes = frameBytes }
          start = either (error . show) id
            (registerStreamRequest 7 resolvedScope 1000 (newStreamMachine StreamHost limits))
          (_, result) = receiveStreamEnvelopeWithFrameBytes 1 (frameBytes + 1) frame start
      result `shouldSatisfy` isConnectionFailure

    it "bounds retained terminal streams and rejects forged peer errors" $ do
      let limits = negotiatedLimits { nsvMaxConcurrentStreams = 1 }
          start = either (error . show) id
            (registerStreamRequest 7 resolvedScope 1000 (newStreamMachine StreamHost limits))
          open = (streamOpen (StreamId 2) "scope-1" TerrainSnapshot)
            { soTotalItems = Just 0, soTotalBytes = Just 0 }
          (one, Right _) = receiveStreamEnvelope 1 (StreamOpenEnvelope open) start
          end = StreamEnd (StreamId 2) 7 0 0 (streamRecordsDigest [])
          (ended, Right _) = receiveStreamEnvelope 2 (StreamEndEnvelope end) one
          (_, registryResult) = receiveStreamEnvelope 3
            (StreamOpenEnvelope (streamOpen (StreamId 4) "scope-1" TerrainSnapshot)) ended
          forged = StreamProtocolError (StreamId 6) 7 "forged" "bad"
          (_, forgedResult) = receiveStreamEnvelope 3 (StreamErrorEnvelope forged) start
      registryResult `shouldSatisfy` isRequestFailure
      forgedResult `shouldSatisfy` isConnectionFailure

    it "rejects zstd opens because the optional codec is not safely enabled" $ do
      let start = registeredMachine
          open = (streamOpen (StreamId 2) "scope-1" TerrainSnapshot) { soCodec = StreamZstd }
          (_, result) = receiveStreamEnvelope 1 (StreamOpenEnvelope open) start
      result `shouldSatisfy` isRequestFailure

    it "distinguishes unknown IDs from idempotent and terminal cancel races" $ do
      let open = (streamOpen (StreamId 2) "scope-1" TerrainSnapshot)
            { soTotalItems = Just 0, soTotalBytes = Just 0 }
          (opened, Right _) = receiveStreamEnvelope 1 (StreamOpenEnvelope open) registeredMachine
          unknown = StreamCancel (StreamId 4) 7 "unknown"
          (_, unknownResult) = receiveStreamEnvelope 2 (StreamCancelEnvelope unknown) opened
      unknownResult `shouldSatisfy` isConnectionFailure

      let end = StreamEnd (StreamId 2) 7 0 0 (streamRecordsDigest [])
          (ended, Right _) = receiveStreamEnvelope 2 (StreamEndEnvelope end) opened
          cancel = StreamCancel (StreamId 2) 7 "late"
          (terminal, terminalResult) = receiveStreamEnvelope 3 (StreamCancelEnvelope cancel) ended
      terminalResult `shouldBe` Right []
      activeRequestCount terminal `shouldBe` 1

      let (cleaned, _) = cancelStreamRequest 7 "local" opened
          (again, againResult) = receiveStreamEnvelope 3 (StreamCancelEnvelope cancel) cleaned
      againResult `shouldBe` Right []
      activeRequestCount again `shouldBe` 0
      let (_, lateEndResult) = receiveStreamEnvelope 4 (StreamEndEnvelope end) cleaned
          peerError = StreamProtocolError (StreamId 2) 7 "late" "lost race"
          (_, lateErrorResult) = receiveStreamEnvelope 4 (StreamErrorEnvelope peerError) cleaned
      lateEndResult `shouldBe` Right []
      lateErrorResult `shouldBe` Right []

    it "cancels on peer stream_error without replying with another error" $ do
      let open = streamOpen (StreamId 2) "scope-1" TerrainSnapshot
          (opened, Right _) = receiveStreamEnvelope 1 (StreamOpenEnvelope open) registeredMachine
          peerError = StreamProtocolError (StreamId 2) 7 "rejected" "peer failed"
          (cleaned, result) = receiveStreamEnvelope 2 (StreamErrorEnvelope peerError) opened
      activeRequestCount cleaned `shouldBe` 0
      case result of
        Left failure -> do
          sfClass failure `shouldBe` StreamRequestFailure
          streamFailureEffects failure `shouldSatisfy` all (\effect -> case effect of
            SendStreamError _ -> False
            _ -> True)
        Right _ -> expectationFailure "expected peer stream_error failure"

    it "cleans all staged bytes and requests on deadline/disconnect" $ do
      let machine0 = either (error . show) id
            (registerStreamRequest 7 resolvedScope 5 (newStreamMachine StreamHost negotiatedLimits))
          open = streamOpen (StreamId 2) "scope-1" TerrainSnapshot
          (machine1, Right _) = receiveStreamEnvelope 1 (StreamOpenEnvelope open) machine0
          (expired, effects) = expireStreams 5 machine1
      activeStreamCount expired `shouldBe` 0
      activeRequestCount expired `shouldBe` 0
      effects `shouldSatisfy` (not . null)
      let (disconnected, _) = disconnectStreams "closed" machine1
      activeStreamCount disconnected `shouldBe` 0
      activeRequestCount disconnected `shouldBe` 0

negotiatedLimits :: NegotiatedStreamV1
negotiatedLimits = either (error . show) id
  (negotiateStreamV1 (defaultStreamProposal 4096) (defaultStreamProposal 4096))

registeredMachine :: StreamMachine
registeredMachine = either (error . show) id
  (registerStreamRequest 7 resolvedScope 1000 (newStreamMachine StreamHost negotiatedLimits))

streamOpen :: StreamId -> Text -> StreamPayloadKind -> StreamOpen
streamOpen sid scopeId kind = StreamOpen
  { soStreamId = sid, soParentRequestId = 7
  , soScopeId = scopeId, soPayloadKind = kind
  , soPayloadVersion = 1
  , soSections = Set.singleton TerrainElevation
  , soChunkIds = IntSet.fromList [1, 2]
  , soMetadata = object [], soCodec = StreamIdentity
  , soTotalItems = Nothing, soTotalBytes = Nothing, soFinalSha256 = Nothing
  }

resolvedScope :: ResolvedInvocationScope
resolvedScope = ResolvedInvocationScope
  { risScopeId = "scope-1"
  , risKind = InvocationGenerator
  , risTerrainInputSections = Set.singleton TerrainElevation
  , risTerrainInputChunkIds = IntSet.fromList [1, 2]
  , risDependencyOverlayChunkIds = Map.empty
  , risOwnOverlayReadChunkIds = IntSet.empty
  , risTerrainOutputSections = Set.singleton TerrainElevation
  , risTerrainOutputChunkIds = IntSet.fromList [1, 2]
  , risOwnedOverlayIdentity = Nothing
  , risOwnOverlayWriteChunkIds = IntSet.empty
  , risGeneratorMetadataOutput = False
  , risDataResource = Nothing
  , risBudgets = RPCScopeBudgets 4096 4096 4096
  }

isConnectionFailure :: Either StreamFailure a -> Bool
isConnectionFailure (Left failure) = sfClass failure == StreamConnectionCorruption
isConnectionFailure _ = False

isRequestFailure :: Either StreamFailure a -> Bool
isRequestFailure (Left failure) = sfClass failure == StreamRequestFailure
isRequestFailure _ = False

isAesonError :: Aeson.Result a -> Bool
isAesonError Aeson.Error{} = True
isAesonError _ = False
