{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spec.PluginRPC (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, bracket, try)
import Data.Aeson (Value(..), (.=), object, encode, eitherDecode)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Either (isLeft, isRight)
import Data.List (isInfixOf, isPrefixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)
import System.Directory (doesPathExist)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Info (os)
import System.IO (stdin, stdout)
import System.IO.Temp (withSystemTempFile)
import System.Timeout (timeout)

import Topo.Plugin.RPC (RPCError(..), checkHealth, newRPCConnection, rpcErrorText, sendHeartbeat)
import Topo.Plugin.RPC.Manifest
import Topo.Plugin.RPC.Protocol
import Topo.Plugin.RPC.Transport
  ( Transport
  , TransportConfig(..)
  , TransportEndpoint(..)
  , TransportError(..)
  , TransportServer(..)
  , closeTransport
  , connectPluginEndpoint
  , connectPluginFromEnvironment
  , defaultTransportConfig
  , endpointKindText
  , openPluginServer
  , pluginEndpointEnv
  , pluginEndpointKindEnv
  , pluginPipeName
  , pluginStdioCompatibilityEnv
  , recvMessage
  , recvMessageWithLimit
  , sendMessage
  , sendMessageWithLimit
  )
import Topo.Plugin.DataResource

------------------------------------------------------------------------
-- Arbitrary instances for QuickCheck
------------------------------------------------------------------------

instance Arbitrary Text where
  arbitrary = Text.pack <$> listOf1 (elements (['a'..'z'] <> ['0'..'9'] <> ['_']))
  shrink t = map Text.pack . shrink $ Text.unpack t

instance Arbitrary RPCMessageType where
  arbitrary = elements
    [ MsgInvokeGenerator
    , MsgInvokeSimulation
    , MsgShutdown
    , MsgProgress
    , MsgLog
    , MsgGeneratorResult
    , MsgSimulationResult
    , MsgError
    , MsgHandshake
    , MsgHandshakeAck
    , MsgWorldChanged
    , MsgQueryResource
    , MsgQueryResult
    , MsgMutateResource
    , MsgMutateResult
    , MsgHeartbeat
    , MsgHealthCheck
    , MsgHealthStatus
    ]

instance Arbitrary RPCEnvelope where
  arbitrary = RPCEnvelope <$> arbitrary <*> pure (object []) <*> arbitrary

instance Arbitrary PluginLogLevel where
  arbitrary = elements
    [ PluginLogDebug
    , PluginLogInfo
    , PluginLogWarn
    , PluginLogError
    ]

instance Arbitrary PluginProgress where
  arbitrary = PluginProgress
    <$> arbitrary
    <*> choose (0.0, 1.0)

instance Arbitrary PluginLog where
  arbitrary = PluginLog
    <$> arbitrary
    <*> arbitrary

instance Arbitrary PluginError where
  arbitrary = Topo.Plugin.RPC.Protocol.PluginError
    <$> choose (1, 999)
    <*> arbitrary

instance Arbitrary Capability where
  arbitrary = elements
    [ CapLog
    , CapNoise
    , CapReadTerrain
    , CapWriteTerrain
    , CapReadOverlay
    , CapWriteOverlay
    , CapReadWorld
    , CapWriteWorld
    , CapDataRead
    , CapDataWrite
    ]

instance Arbitrary RPCParamType where
  arbitrary = elements [ParamFloat, ParamInt, ParamBool]

instance Arbitrary RPCParamSpec where
  arbitrary = do
    name  <- arbitrary
    label <- arbitrary
    ty    <- arbitrary
    tip   <- arbitrary
    pure RPCParamSpec
      { rpsName    = name
      , rpsLabel   = label
      , rpsType    = ty
      , rpsRange   = Nothing
      , rpsDefault = Aeson.Number 0
      , rpsTooltip = tip
      }

instance Arbitrary RPCOverlayDecl where
  arbitrary = RPCOverlayDecl <$> arbitrary

instance Arbitrary RPCSimulationDecl where
  arbitrary = RPCSimulationDecl <$> listOf arbitrary

instance Arbitrary RPCGeneratorDecl where
  arbitrary = RPCGeneratorDecl
    <$> arbitrary
    <*> listOf arbitrary

instance Arbitrary DataFieldType where
  arbitrary = oneof
    [ pure DFText
    , pure DFInt
    , pure DFFloat
    , pure DFDouble
    , pure DFBool
    , pure DFFixed2
    , pure DFFixed3
    , pure DFFixed4
    , DFEnum <$> listOf1 arbitrary
    ]

instance Arbitrary DataConstructorDef where
  arbitrary = DataConstructorDef
    <$> arbitrary
    <*> listOf (oneof [pure DFText, pure DFInt, pure DFFloat, pure DFBool])

instance Arbitrary DataFieldDef where
  arbitrary = DataFieldDef
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> pure Nothing

instance Arbitrary DataOperations where
  arbitrary = DataOperations
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary DataResourceSchema where
  arbitrary = do
    name <- arbitrary
    label <- arbitrary
    hexBound <- arbitrary
    keyName <- arbitrary
    let keyField = DataFieldDef keyName DFText "Key" False Nothing
    extraFields <- listOf arbitrary
    ops <- arbitrary
    ov <- arbitrary
    pure DataResourceSchema
      { drsName       = name
      , drsLabel      = label
      , drsHexBound   = hexBound
      , drsFields     = keyField : extraFields
      , drsOperations = ops
      , drsKeyField   = keyName
      , drsOverlay    = ov
      }

instance Arbitrary Handshake where
  arbitrary = Handshake
    <$> pure currentProtocolVersion
    <*> arbitrary
    <*> listOf (elements ["query", "mutate", "subscribe"])

instance Arbitrary HandshakeAck where
  arbitrary = HandshakeAck
    <$> pure currentProtocolVersion
    <*> arbitrary
    <*> listOf arbitrary

instance Arbitrary WorldChanged where
  arbitrary = WorldChanged <$> arbitrary

instance Arbitrary Heartbeat where
  arbitrary = Heartbeat <$> arbitrary

instance Arbitrary HealthStatus where
  arbitrary = HealthStatus <$> arbitrary <*> arbitrary

instance Arbitrary RPCManifest where
  arbitrary = do
    name <- Text.pack <$> listOf1 (elements ['a'..'z'])
    ver  <- Text.pack <$> listOf1 (elements (['0'..'9'] <> ['.']))
    desc <- arbitrary
    gen  <- arbitrary
    sim  <- arbitrary
    ov   <- arbitrary
    caps <- listOf arbitrary
    params <- listOf arbitrary
    -- Ensure structural validity: sim requires overlay
    let ov' = case sim of
          Just _  -> case ov of
            Nothing -> Just (RPCOverlayDecl "test.toposchema")
            x       -> x
          Nothing -> ov
    dataRes <- listOf arbitrary
    dataDir <- arbitrary
    pure RPCManifest
      { rmName         = name
      , rmVersion      = ver
      , rmDescription  = desc
      , rmGenerator    = gen
      , rmSimulation   = sim
      , rmOverlay      = ov'
      , rmCapabilities = caps
      , rmParameters   = params
      , rmDataResources = dataRes
      , rmDataDirectory = dataDir
      , rmStartPolicy   = defaultRPCStartPolicy
      }

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Encode a value to strict bytestring (for parseManifest tests).
jsonBS :: Aeson.ToJSON a => a -> BS.ByteString
jsonBS = BL.toStrict . Aeson.encode

-- | A minimal valid manifest as a JSON bytestring.
minimalManifestBS :: BS.ByteString
minimalManifestBS = jsonBS $ object
  [ "name"      .= ("test-plugin" :: Text)
  , "version"   .= ("0.1.0" :: Text)
  , "generator" .= object
      [ "insertAfter" .= ("biomes" :: Text) ]
  ]

-- | A full manifest with all fields populated.
fullManifestBS :: BS.ByteString
fullManifestBS = jsonBS $ object
  [ "name"         .= ("civilization" :: Text)
  , "version"      .= ("1.0.0" :: Text)
  , "description"  .= ("Civilization sim overlay" :: Text)
  , "generator"    .= object
      [ "insertAfter" .= ("biomes" :: Text)
      , "requires"    .= (["biomes", "rivers"] :: [Text])
      ]
  , "simulation" .= object
      [ "dependencies" .= (["weather"] :: [Text]) ]
  , "overlay" .= object
      [ "schemaFile" .= ("civilization.toposchema" :: Text) ]
  , "capabilities" .= (["readTerrain", "readOverlay", "writeOverlay", "log"] :: [Text])
  , "config" .= object
      [ "parameters" .=
          [ object
              [ "name"    .= ("growth_rate" :: Text)
              , "label"   .= ("Growth Rate" :: Text)
              , "type"    .= ("float" :: Text)
              , "default" .= (1.0 :: Double)
              , "range"   .= ([0.0, 10.0] :: [Double])
              , "tooltip" .= ("Speed of civilization growth" :: Text)
              ]
          ]
      ]
  ]

------------------------------------------------------------------------
-- Test suite
------------------------------------------------------------------------

spec :: Spec
spec = describe "Plugin.RPC" $ do

  ------------------------------------
  -- Manifest parsing
  ------------------------------------
  describe "Manifest parsing" $ do
    it "parses a minimal manifest" $ do
      let result = parseManifest minimalManifestBS
      result `shouldSatisfy` isRight
      case result of
        Right m -> do
          rmName m `shouldBe` "test-plugin"
          rmVersion m `shouldBe` "0.1.0"
          rmDescription m `shouldBe` ""
          rmSimulation m `shouldBe` Nothing
          rmOverlay m `shouldBe` Nothing
          rmCapabilities m `shouldBe` []
          rmParameters m `shouldBe` []
          case rmGenerator m of
            Just g  -> rgdInsertAfter g `shouldBe` "biomes"
            Nothing -> expectationFailure "expected generator declaration"
        Left err -> expectationFailure (Text.unpack err)

    it "parses a full manifest with all fields" $ do
      let result = parseManifest fullManifestBS
      result `shouldSatisfy` isRight
      case result of
        Right m -> do
          rmName m `shouldBe` "civilization"
          rmVersion m `shouldBe` "1.0.0"
          rmDescription m `shouldBe` "Civilization sim overlay"
          manifestHasGenerator m `shouldBe` True
          manifestHasSimulation m `shouldBe` True
          manifestHasOverlay m `shouldBe` True
          rmCapabilities m `shouldSatisfy` (elem CapReadTerrain)
          rmCapabilities m `shouldSatisfy` (elem CapWriteOverlay)
          length (rmParameters m) `shouldBe` 1
          case rmParameters m of
            [p] -> do
              rpsName p `shouldBe` "growth_rate"
              rpsType p `shouldBe` ParamFloat
              rpsTooltip p `shouldBe` "Speed of civilization growth"
              rpsRange p `shouldSatisfy` \r -> case r of
                Just _  -> True
                Nothing -> False
            _ -> expectationFailure "expected exactly one parameter"
          case rmGenerator m of
            Just g -> do
              rgdInsertAfter g `shouldBe` "biomes"
              rgdRequires g `shouldBe` ["biomes", "rivers"]
            Nothing -> expectationFailure "expected generator"
          case rmSimulation m of
            Just s -> rsdDependencies s `shouldBe` ["weather"]
            Nothing -> expectationFailure "expected simulation"
          case rmOverlay m of
            Just o -> rodSchemaFile o `shouldBe` "civilization.toposchema"
            Nothing -> expectationFailure "expected overlay"
        Left err -> expectationFailure (Text.unpack err)

    it "rejects invalid JSON" $ do
      let result = parseManifest "not valid json"
      result `shouldSatisfy` isLeft

    it "rejects JSON missing required name field" $ do
      let bs = jsonBS $ object [ "version" .= ("1.0" :: Text) ]
      parseManifest bs `shouldSatisfy` isLeft

    it "rejects JSON missing required version field" $ do
      let bs = jsonBS $ object [ "name" .= ("x" :: Text) ]
      parseManifest bs `shouldSatisfy` isLeft

    it "defaults missing description to empty" $ do
      case parseManifest minimalManifestBS of
        Right m -> rmDescription m `shouldBe` ""
        Left _  -> expectationFailure "parse failed"

    it "defaults missing capabilities to empty list" $ do
      case parseManifest minimalManifestBS of
        Right m -> rmCapabilities m `shouldBe` []
        Left _  -> expectationFailure "parse failed"

    it "defaults missing generator requires to empty list" $ do
      let bs = jsonBS $ object
            [ "name"      .= ("p" :: Text)
            , "version"   .= ("1" :: Text)
            , "generator" .= object [ "insertAfter" .= ("base" :: Text) ]
            ]
      case parseManifest bs of
        Right m -> case rmGenerator m of
          Just g  -> rgdRequires g `shouldBe` []
          Nothing -> expectationFailure "expected generator"
        Left _ -> expectationFailure "parse failed"

    it "defaults missing simulation dependencies to empty list" $ do
      let bs = jsonBS $ object
            [ "name"       .= ("p" :: Text)
            , "version"    .= ("1" :: Text)
            , "simulation" .= object []
            , "overlay"    .= object [ "schemaFile" .= ("x" :: Text) ]
            ]
      case parseManifest bs of
        Right m -> case rmSimulation m of
          Just s  -> rsdDependencies s `shouldBe` []
          Nothing -> expectationFailure "expected simulation"
        Left _ -> expectationFailure "parse failed"

  ------------------------------------
  -- Manifest validation
  ------------------------------------
  describe "Manifest validation" $ do
    it "validates a well-formed manifest" $ do
      case parseManifest fullManifestBS of
        Right m -> validateManifest m `shouldBe` []
        Left _  -> expectationFailure "parse failed"

    it "detects empty name" $ do
      let m = baseManifest { rmName = "" }
      validateManifest m `shouldSatisfy` elem ManifestEmptyName

    it "detects empty version" $ do
      let m = baseManifest { rmVersion = "" }
      validateManifest m `shouldSatisfy` elem ManifestEmptyVersion

    it "detects simulation without overlay" $ do
      let m = baseManifest
            { rmSimulation = Just (RPCSimulationDecl [])
            , rmOverlay    = Nothing
            }
      validateManifest m `shouldSatisfy` elem ManifestSimWithoutOverlay

    it "detects writeTerrain without simulation" $ do
      let m = baseManifest
            { rmCapabilities = [CapWriteTerrain]
            , rmSimulation   = Nothing
            }
      validateManifest m `shouldSatisfy` elem ManifestWriteTerrainWithoutSim

    it "detects no participation (neither generator nor simulation)" $ do
      let m = baseManifest
            { rmGenerator  = Nothing
            , rmSimulation = Nothing
            }
      validateManifest m `shouldSatisfy` elem ManifestNoParticipation

    it "accepts generator-only manifest" $ do
      let m = baseManifest
            { rmGenerator  = Just (RPCGeneratorDecl "biomes" [])
            , rmSimulation = Nothing
            , rmOverlay    = Nothing
            }
      validateManifest m `shouldBe` []

    it "accepts simulation-with-overlay manifest" $ do
      let m = baseManifest
            { rmGenerator  = Nothing
            , rmSimulation = Just (RPCSimulationDecl [])
            , rmOverlay    = Just (RPCOverlayDecl "test.toposchema")
            }
      validateManifest m `shouldBe` []

  ------------------------------------
  -- Manifest queries
  ------------------------------------
  describe "Manifest queries" $ do
    it "manifestWritesTerrain detects CapWriteTerrain" $ do
      let m = baseManifest { rmCapabilities = [CapWriteTerrain, CapLog] }
      manifestWritesTerrain m `shouldBe` True

    it "manifestWritesTerrain returns False without it" $ do
      let m = baseManifest { rmCapabilities = [CapLog] }
      manifestWritesTerrain m `shouldBe` False

    it "manifestHasGenerator detects generator decl" $ do
      manifestHasGenerator baseManifest `shouldBe` True

    it "manifestHasGenerator returns False for sim-only" $ do
      let m = baseManifest { rmGenerator = Nothing }
      manifestHasGenerator m `shouldBe` False

    it "manifestHasSimulation detects simulation decl" $ do
      let m = baseManifest
            { rmSimulation = Just (RPCSimulationDecl [])
            , rmOverlay    = Just (RPCOverlayDecl "x")
            }
      manifestHasSimulation m `shouldBe` True

    it "manifestHasOverlay detects overlay decl" $ do
      let m = baseManifest { rmOverlay = Just (RPCOverlayDecl "x") }
      manifestHasOverlay m `shouldBe` True

  ------------------------------------
  -- Manifest JSON round-trip (property)
  ------------------------------------
  describe "Manifest JSON round-trip" $ do
    prop "RPCManifest survives toJSON/fromJSON" $ \(m :: RPCManifest) ->
      let encoded = Aeson.encode (Aeson.toJSON m)
          decoded = Aeson.eitherDecode encoded :: Either String RPCManifest
      in case decoded of
          Right m' -> m' === m
          Left err -> counterexample ("decode failed: " <> err) False

    prop "RPCCapability round-trips" $ \(cap :: Capability) ->
      Aeson.fromJSON (Aeson.toJSON cap) === Aeson.Success cap

    prop "RPCParamType round-trips" $ \(ty :: RPCParamType) ->
      Aeson.fromJSON (Aeson.toJSON ty) === Aeson.Success ty

  ------------------------------------
  -- Handshake protocol round-trips
  ------------------------------------
  describe "Handshake protocol" $ do
    prop "Handshake round-trips" $ \(hs :: Handshake) ->
      Aeson.fromJSON (Aeson.toJSON hs) === Aeson.Success hs

    prop "HandshakeAck round-trips" $ \(ha :: HandshakeAck) ->
      Aeson.fromJSON (Aeson.toJSON ha) === Aeson.Success ha

    prop "WorldChanged round-trips" $ \(wc :: WorldChanged) ->
      Aeson.fromJSON (Aeson.toJSON wc) === Aeson.Success wc

    prop "Heartbeat round-trips" $ \(hb :: Heartbeat) ->
      Aeson.fromJSON (Aeson.toJSON hb) === Aeson.Success hb

    prop "HealthStatus round-trips" $ \(health :: HealthStatus) ->
      Aeson.fromJSON (Aeson.toJSON health) === Aeson.Success health

    it "Handshake with no world path round-trips" $ do
      let hs = Handshake currentProtocolVersion Nothing ["query"]
      Aeson.fromJSON (Aeson.toJSON hs) `shouldBe` Aeson.Success hs

    it "HandshakeAck with no data directory round-trips" $ do
      let ha = HandshakeAck currentProtocolVersion Nothing []
      Aeson.fromJSON (Aeson.toJSON ha) `shouldBe` Aeson.Success ha

    it "WorldChanged with Nothing round-trips" $ do
      let wc = WorldChanged Nothing
      Aeson.fromJSON (Aeson.toJSON wc) `shouldBe` Aeson.Success wc

    it "encodes MsgHandshake as expected string" $
      Aeson.toJSON MsgHandshake `shouldBe` Aeson.String "handshake"

    it "encodes MsgHandshakeAck as expected string" $
      Aeson.toJSON MsgHandshakeAck `shouldBe` Aeson.String "handshake_ack"

    it "encodes MsgWorldChanged as expected string" $
      Aeson.toJSON MsgWorldChanged `shouldBe` Aeson.String "world_changed"

  ------------------------------------
  -- Protocol message types
  ------------------------------------
  describe "Protocol message types" $ do
    describe "RPCMessageType JSON codec" $ do
      prop "round-trips through JSON" $ \(mt :: RPCMessageType) ->
        Aeson.fromJSON (Aeson.toJSON mt) === Aeson.Success mt

      it "encodes MsgInvokeGenerator as expected string" $
        Aeson.toJSON MsgInvokeGenerator `shouldBe` Aeson.String "invoke_generator"

      it "encodes MsgShutdown as expected string" $
        Aeson.toJSON MsgShutdown `shouldBe` Aeson.String "shutdown"

      it "encodes heartbeat and health message tags" $ do
        Aeson.toJSON MsgHeartbeat `shouldBe` Aeson.String "heartbeat"
        Aeson.toJSON MsgHealthCheck `shouldBe` Aeson.String "health_check"
        Aeson.toJSON MsgHealthStatus `shouldBe` Aeson.String "health_status"

    describe "PluginLogLevel JSON codec" $ do
      prop "round-trips through JSON" $ \(ll :: PluginLogLevel) ->
        Aeson.fromJSON (Aeson.toJSON ll) === Aeson.Success ll

      it "encodes PluginLogDebug as \"debug\"" $
        Aeson.toJSON PluginLogDebug `shouldBe` Aeson.String "debug"

      it "encodes PluginLogError as \"error\"" $
        Aeson.toJSON PluginLogError `shouldBe` Aeson.String "error"

  ------------------------------------
  -- Protocol envelope encode/decode
  ------------------------------------
  describe "Protocol envelope" $ do
    prop "RPCEnvelope encode/decode round-trips" $ \(env :: RPCEnvelope) ->
      decodeMessage (encodeMessage env) === Right env

    it "decodes a hand-crafted envelope" $ do
      let env = RPCEnvelope MsgProgress (Aeson.toJSON (PluginProgress "working" 0.5)) (Just 99)
          bs  = encodeMessage env
      decodeMessage bs `shouldBe` Right env

    it "preserves request ids and accepts legacy envelopes without ids" $ do
      let correlated = RPCEnvelope MsgHealthCheck (object []) (Just 1234)
          legacyBytes = "{\"type\":\"health_check\",\"payload\":{}}"
      decodeMessage (encodeMessage correlated) `shouldBe` Right correlated
      decodeMessage legacyBytes `shouldBe` Right (RPCEnvelope MsgHealthCheck (object []) Nothing)

    it "rejects invalid bytes" $
      decodeMessage "not json" `shouldSatisfy` isLeft

  ------------------------------------
  -- RPC session correlation and timeouts
  ------------------------------------
  describe "RPC session" $ do
    it "correlates concurrent in-flight health checks by request id" $
      withConnectedTransports "rpc-concurrent-health" $ \host plugin -> do
        let conn = newRPCConnection baseManifest host Map.empty
        firstDone <- newEmptyMVar
        _ <- forkIO (checkHealth conn >>= putMVar firstDone)
        firstReq <- recvEnvelopeFrom plugin
        envType firstReq `shouldBe` MsgHealthCheck
        secondDone <- newEmptyMVar
        _ <- forkIO (checkHealth conn >>= putMVar secondDone)
        secondReq <- recvEnvelopeFrom plugin
        envType secondReq `shouldBe` MsgHealthCheck
        envRequestId firstReq `shouldNotBe` envRequestId secondReq
        sendEnvelopeTo plugin (healthResponse secondReq "second")
        secondResult <- takeHealthResult secondDone
        hstMessage secondResult `shouldBe` "second"
        sendEnvelopeTo plugin (healthResponse firstReq "first")
        firstResult <- takeHealthResult firstDone
        hstMessage firstResult `shouldBe` "first"

    it "returns clear per-request timeout errors" $
      withConnectedTransports "rpc-timeout-health" $ \host _plugin -> do
        let manifest = baseManifest
              { rmStartPolicy = defaultRPCStartPolicy { rspRequestTimeoutMs = 50 }
              }
            conn = newRPCConnection manifest host Map.empty
        result <- timeout 500000 (checkHealth conn)
        case result of
          Nothing -> expectationFailure "health check hung past timeout"
          Just (Left err@(RPCTimeout msg)) -> do
            msg `shouldSatisfy` Text.isInfixOf "health check"
            rpcErrorText err `shouldSatisfy` Text.isInfixOf "timeout"
          Just other -> expectationFailure ("expected timeout, got " <> show other)

    it "round-trips heartbeat responses with correlation ids" $
      withConnectedTransports "rpc-heartbeat" $ \host plugin -> do
        let conn = newRPCConnection baseManifest host Map.empty
        done <- newEmptyMVar
        _ <- forkIO (sendHeartbeat conn >>= putMVar done)
        request <- recvEnvelopeFrom plugin
        envType request `shouldBe` MsgHeartbeat
        sendEnvelopeTo plugin (RPCEnvelope
          { envType = MsgHeartbeat
          , envPayload = Aeson.toJSON (Heartbeat "ok")
          , envRequestId = envRequestId request
          })
        heartbeat <- takeHeartbeatResult done
        hbStatus heartbeat `shouldBe` "ok"

  ------------------------------------
  -- Protocol message round-trips
  ------------------------------------
  describe "Protocol message JSON round-trips" $ do
    prop "PluginProgress round-trips" $ \(pp :: PluginProgress) ->
      Aeson.fromJSON (Aeson.toJSON pp) === Aeson.Success pp

    prop "PluginLog round-trips" $ \(pl :: PluginLog) ->
      Aeson.fromJSON (Aeson.toJSON pl) === Aeson.Success pl

    prop "PluginError round-trips" $ \(pe :: PluginError) ->
      Aeson.fromJSON (Aeson.toJSON pe) === Aeson.Success pe

    it "InvokeGenerator round-trips" $ do
      let ig = InvokeGenerator
            { igPayloadVersion = 1
            , igStageId = "plugin:test"
            , igSeed    = 42
            , igConfig  = Map.fromList [("rate", Aeson.Number 1.5)]
            , igTerrain = object ["chunks" .= ([] :: [Value])]
            }
      Aeson.fromJSON (Aeson.toJSON ig) `shouldBe` Aeson.Success ig

    it "InvokeSimulation round-trips" $ do
      let is = InvokeSimulation
            { isPayloadVersion = 1
            , isNodeId     = "civ"
            , isWorldTime  = 100
            , isDeltaTicks = 1
            , isCalendar   = Null
            , isConfig     = Map.empty
            , isTerrain    = Null
            , isOverlays   = Null
            , isOwnOverlay = Null
            }
      Aeson.fromJSON (Aeson.toJSON is) `shouldBe` Aeson.Success is

    it "rejects InvokeGenerator with unknown payload_version" $
      let payload = object
            [ "payload_version" .= (2 :: Int)
            , "stage_id" .= ("plugin:test" :: Text)
            , "seed" .= (42 :: Word64)
            , "config" .= object []
            , "terrain" .= object []
            ]
      in case Aeson.fromJSON payload :: Aeson.Result InvokeGenerator of
          Aeson.Error _ -> pure ()
          Aeson.Success _ -> expectationFailure "expected parse failure for unknown payload_version"

    it "rejects InvokeSimulation with unknown payload_version" $
      let payload = object
            [ "payload_version" .= (99 :: Int)
            , "node_id" .= ("civ" :: Text)
            , "world_time" .= (100 :: Word64)
            , "delta_ticks" .= (1 :: Word64)
            , "calendar" .= Null
            , "config" .= object []
            , "terrain" .= Null
            , "overlays" .= Null
            , "own_overlay" .= Null
            ]
      in case Aeson.fromJSON payload :: Aeson.Result InvokeSimulation of
          Aeson.Error _ -> pure ()
          Aeson.Success _ -> expectationFailure "expected parse failure for unknown payload_version"

    it "GeneratorResult round-trips" $ do
      let gr = GeneratorResult
            { grTerrain  = object []
            , grOverlay  = Just (object ["pop" .= (100 :: Int)])
            , grMetadata = Nothing
            }
      Aeson.fromJSON (Aeson.toJSON gr) `shouldBe` Aeson.Success gr

    it "GeneratorResult without optional fields round-trips" $ do
      let gr = GeneratorResult
            { grTerrain  = object []
            , grOverlay  = Nothing
            , grMetadata = Nothing
            }
      Aeson.fromJSON (Aeson.toJSON gr) `shouldBe` Aeson.Success gr

    it "SimulationResult round-trips" $ do
      let sr = SimulationResult
            { srOverlay       = object ["pop" .= (200 :: Int)]
            , srTerrainWrites = Nothing
            }
      Aeson.fromJSON (Aeson.toJSON sr) `shouldBe` Aeson.Success sr

    it "SimulationResult with terrain writes round-trips" $ do
      let sr = SimulationResult
            { srOverlay       = object []
            , srTerrainWrites = Just (object ["c0" .= (1 :: Int)])
            }
      Aeson.fromJSON (Aeson.toJSON sr) `shouldBe` Aeson.Success sr

  ------------------------------------
  -- Transport pipe name generation
  ------------------------------------
  describe "Transport" $ do
    it "generates a non-empty pipe name" $ do
      let pn = pluginPipeName defaultTransportConfig "my-plugin"
      length pn `shouldSatisfy` (> 0)

    it "includes the plugin name in the pipe name" $ do
      let pn = pluginPipeName defaultTransportConfig "civilization"
      pn `shouldSatisfy` \fp -> "civilization" `isInfixOf` fp

    it "generates distinct names for distinct plugins" $ do
      let pn1 = pluginPipeName defaultTransportConfig "alpha"
          pn2 = pluginPipeName defaultTransportConfig "beta"
      pn1 `shouldNotBe` pn2

    it "rejects oversized outgoing frames with a clear framing error" $
      withConnectedTransports "frame-send-limit" $ \host _plugin -> do
        result <- sendMessageWithLimit 4 host "12345"
        case result of
          Left (TransportFramingError msg) -> msg `shouldSatisfy` Text.isInfixOf "max size"
          other -> expectationFailure ("expected frame size error, got " <> show other)

    it "rejects oversized incoming frames before reading the payload" $
      withConnectedTransports "frame-recv-limit" $ \host plugin -> do
        shouldReturnWithin "plugin send oversized payload" (sendMessage plugin "12345") (Right ())
        result <- recvMessageWithLimit 4 host
        case result of
          Left (TransportFramingError msg) -> msg `shouldSatisfy` Text.isInfixOf "max size"
          other -> expectationFailure ("expected frame size error, got " <> show other)

    it "rejects implicit stdio fallback without the test/development flag" $
      withCleanPluginTransportEnvironment $ do
        connection <- connectPluginFromEnvironment "stdio-disabled" stdin stdout
        case connection of
          Left (TransportConnectionFailed msg) -> do
            msg `shouldSatisfy` Text.isInfixOf (Text.pack pluginEndpointEnv)
            msg `shouldSatisfy` Text.isInfixOf (Text.pack pluginStdioCompatibilityEnv)
          Left err -> expectationFailure ("unexpected transport error: " <> show err)
          Right _ -> expectationFailure "expected missing endpoint environment to reject implicit stdio fallback"

    it "allows stdio fallback only when explicitly enabled for tests" $
      withCleanPluginTransportEnvironment $ do
        setEnv pluginStdioCompatibilityEnv "1"
        withSystemTempFile "topo-plugin-stdio-read" $ \_ readH ->
          withSystemTempFile "topo-plugin-stdio-write" $ \_ writeH -> do
            connection <- connectPluginFromEnvironment "stdio-enabled" readH writeH
            case connection of
              Left err -> expectationFailure ("expected stdio compatibility transport, got: " <> show err)
              Right _ -> pure ()

    it "connects over a host-created Unix socket endpoint" $
      onUnix $ withTransportServer "socket-smoke" $ \server -> do
        pathExists <- doesPathExist (teAddress (tsEndpoint server))
        pathExists `shouldBe` True
        done <- newEmptyMVar
        _ <- forkIO (clientEcho "socket-smoke-client" (tsEndpoint server) "ping" "pong" done)
        host <- requireAccept server
        shouldReturnWithin "host send ping" (sendMessage host "ping") (Right ())
        shouldReturnWithin "host receive pong" (recvMessage host) (Right "pong")
        closeTransport host
        takeClientResult done `shouldReturn` Right ()
        doesPathExist (teAddress (tsEndpoint server)) `shouldReturn` False

    it "removes stale Unix socket endpoints when accept times out" $
      onUnix $ bracket acquireTimingOutServer tsClose $ \server -> do
        let socketPath = teAddress (tsEndpoint server)
        doesPathExist socketPath `shouldReturn` True
        acceptResult <- tsAccept server
        case acceptResult of
          Left (TransportConnectionFailed msg) -> msg `shouldSatisfy` Text.isInfixOf "timed out"
          Left err -> expectationFailure ("expected accept timeout, got " <> show err)
          Right transport -> closeTransport transport >> expectationFailure "accept unexpectedly succeeded"
        doesPathExist socketPath `shouldReturn` False

    it "allocates distinct Unix socket endpoints for concurrent plugin startup" $
      onUnix $ withTransportServer "same-plugin" $ \serverA ->
        withTransportServer "same-plugin" $ \serverB -> do
          teAddress (tsEndpoint serverA) `shouldNotBe` teAddress (tsEndpoint serverB)
          doneA <- newEmptyMVar
          doneB <- newEmptyMVar
          _ <- forkIO (clientEcho "same-plugin-a" (tsEndpoint serverA) "alpha" "ack-alpha" doneA)
          _ <- forkIO (clientEcho "same-plugin-b" (tsEndpoint serverB) "beta" "ack-beta" doneB)
          hostA <- requireAccept serverA
          hostB <- requireAccept serverB
          shouldReturnWithin "host A send alpha" (sendMessage hostA "alpha") (Right ())
          shouldReturnWithin "host B send beta" (sendMessage hostB "beta") (Right ())
          shouldReturnWithin "host A receive ack" (recvMessage hostA) (Right "ack-alpha")
          shouldReturnWithin "host B receive ack" (recvMessage hostB) (Right "ack-beta")
          closeTransport hostA
          closeTransport hostB
          takeClientResult doneA `shouldReturn` Right ()
          takeClientResult doneB `shouldReturn` Right ()

    it "escapes unsafe Windows named-pipe endpoint names" $
      onWindows $ withTransportServer "bad/name\\with:chars" $ \server -> do
        let address = teAddress (tsEndpoint server)
            prefix = "\\\\.\\pipe\\"
            pipeName = drop (length prefix) address
        address `shouldSatisfy` (prefix `isPrefixOf`)
        pipeName `shouldSatisfy` \suffix -> not (any (`elem` ['\\', '/', ':']) suffix)

    it "connects over a host-created Windows named pipe endpoint" $
      onWindows $ withTransportServer "pipe-smoke" $ \server -> do
        done <- newEmptyMVar
        _ <- forkIO (clientEcho "pipe-smoke-client" (tsEndpoint server) "ping" "pong" done)
        host <- requireAccept server
        shouldReturnWithin "host send pipe ping" (sendMessage host "ping") (Right ())
        shouldReturnWithin "host receive pipe pong" (recvMessage host) (Right "pong")
        closeTransport host
        takeClientResult done `shouldReturn` Right ()

    it "connects from TOPO_PLUGIN_* endpoint environment to a Windows named pipe" $
      onWindows $ withTransportServer "pipe-env" $ \server -> do
        done <- newEmptyMVar
        _ <- forkIO (clientEchoFromEnvironment "pipe-env-client" (tsEndpoint server) "env-ping" "env-pong" done)
        host <- requireAccept server
        shouldReturnWithin "host send env ping" (sendMessage host "env-ping") (Right ())
        shouldReturnWithin "host receive env pong" (recvMessage host) (Right "env-pong")
        closeTransport host
        takeClientResult done `shouldReturn` Right ()

    it "allocates distinct Windows named-pipe endpoints for concurrent plugin startup" $
      onWindows $ withTransportServer "same-plugin" $ \serverA ->
        withTransportServer "same-plugin" $ \serverB -> do
          teAddress (tsEndpoint serverA) `shouldNotBe` teAddress (tsEndpoint serverB)
          doneA <- newEmptyMVar
          doneB <- newEmptyMVar
          _ <- forkIO (clientEcho "same-plugin-pipe-a" (tsEndpoint serverA) "alpha" "ack-alpha" doneA)
          _ <- forkIO (clientEcho "same-plugin-pipe-b" (tsEndpoint serverB) "beta" "ack-beta" doneB)
          hostA <- requireAccept serverA
          hostB <- requireAccept serverB
          shouldReturnWithin "host A send pipe alpha" (sendMessage hostA "alpha") (Right ())
          shouldReturnWithin "host B send pipe beta" (sendMessage hostB "beta") (Right ())
          shouldReturnWithin "host A receive pipe ack" (recvMessage hostA) (Right "ack-alpha")
          shouldReturnWithin "host B receive pipe ack" (recvMessage hostB) (Right "ack-beta")
          closeTransport hostA
          closeTransport hostB
          takeClientResult doneA `shouldReturn` Right ()
          takeClientResult doneB `shouldReturn` Right ()

onUnix :: IO () -> IO ()
onUnix action =
  if os == "mingw32"
    then pendingWith "Unix domain socket transport is not available on Windows"
    else action

onWindows :: IO () -> IO ()
onWindows action =
  if os == "mingw32"
    then action
    else pendingWith "Windows named-pipe transport is not available on this host"

withConnectedTransports :: Text -> (Transport -> Transport -> IO a) -> IO a
withConnectedTransports name action =
  withTransportServer name $ \server ->
    bracket (acquire server) cleanup (uncurry action)
  where
    acquire server = do
      pluginResultVar <- newEmptyMVar
      _ <- forkIO $ connectPluginEndpoint (name <> "-plugin") (tsEndpoint server) >>= putMVar pluginResultVar
      host <- requireAccept server
      pluginResult <- takeMVar pluginResultVar
      case pluginResult of
        Left err -> do
          closeTransport host
          expectationFailure ("client connect failed: " <> show err)
          fail "client connect"
        Right plugin -> pure (host, plugin)

    cleanup (host, plugin) = do
      closeTransport host
      closeTransport plugin

recvEnvelopeFrom :: Transport -> IO RPCEnvelope
recvEnvelopeFrom transport = do
  result <- timeout transportTestTimeoutMicros (recvMessage transport)
  case result of
    Nothing -> expectationFailure "timed out waiting for RPC envelope" >> fail "recv timeout"
    Just (Left err) -> expectationFailure ("recv failed: " <> show err) >> fail "recv failed"
    Just (Right bytes) -> case decodeMessage bytes of
      Left err -> expectationFailure ("decode failed: " <> Text.unpack err) >> fail "decode failed"
      Right envelope -> pure envelope

sendEnvelopeTo :: Transport -> RPCEnvelope -> IO ()
sendEnvelopeTo transport envelope = do
  result <- sendMessage transport (encodeMessage envelope)
  case result of
    Left err -> expectationFailure ("send failed: " <> show err)
    Right () -> pure ()

healthResponse :: RPCEnvelope -> Text -> RPCEnvelope
healthResponse request message = RPCEnvelope
  { envType = MsgHealthStatus
  , envPayload = Aeson.toJSON (HealthStatus
      { hstHealthy = True
      , hstMessage = message
      })
  , envRequestId = envRequestId request
  }

takeHealthResult :: MVar (Either RPCError HealthStatus) -> IO HealthStatus
takeHealthResult done = do
  result <- timeout transportTestTimeoutMicros (takeMVar done)
  case result of
    Nothing -> expectationFailure "timed out waiting for health result" >> fail "health result timeout"
    Just (Left err) -> expectationFailure ("health check failed: " <> show err) >> fail "health failed"
    Just (Right health) -> pure health

takeHeartbeatResult :: MVar (Either RPCError Heartbeat) -> IO Heartbeat
takeHeartbeatResult done = do
  result <- timeout transportTestTimeoutMicros (takeMVar done)
  case result of
    Nothing -> expectationFailure "timed out waiting for heartbeat result" >> fail "heartbeat timeout"
    Just (Left err) -> expectationFailure ("heartbeat failed: " <> show err) >> fail "heartbeat failed"
    Just (Right heartbeat) -> pure heartbeat

withTransportServer :: Text -> (TransportServer -> IO a) -> IO a
withTransportServer pluginName = bracket acquire tsClose
  where
    acquire = do
      serverResult <- openPluginServer defaultTransportConfig { tcTimeout = 1000 } pluginName
      case serverResult of
        Left err -> expectationFailure ("openPluginServer failed: " <> show err) >> fail "openPluginServer"
        Right server -> pure server

acquireTimingOutServer :: IO TransportServer
acquireTimingOutServer = do
  serverResult <- openPluginServer defaultTransportConfig { tcTimeout = 10 } "socket-stale"
  case serverResult of
    Left err -> expectationFailure ("openPluginServer failed: " <> show err) >> fail "openPluginServer"
    Right server -> pure server

requireAccept :: TransportServer -> IO Transport
requireAccept server = do
  acceptResult <- tsAccept server
  case acceptResult of
    Left err -> expectationFailure ("accept failed: " <> show err) >> fail "accept"
    Right transport -> pure transport

clientEcho :: Text -> TransportEndpoint -> BS.ByteString -> BS.ByteString -> MVarResult -> IO ()
clientEcho name endpoint expected reply done = do
  result <- try $ do
    connection <- connectPluginEndpoint name endpoint
    case connection of
      Left err -> pure (Left ("connect failed: " <> show err))
      Right transport -> do
        received <- recvMessage transport
        case received of
          Left err -> do
            closeTransport transport
            pure (Left ("recv failed: " <> show err))
          Right payload
            | payload /= expected -> do
                closeTransport transport
                pure (Left ("unexpected payload: " <> show payload))
            | otherwise -> do
                sent <- sendMessage transport reply
                closeTransport transport
                case sent of
                  Left err -> pure (Left ("send failed: " <> show err))
                  Right () -> pure (Right ())
  case result of
    Left (err :: SomeException) -> putMVar done (Left (show err))
    Right outcome -> putMVar done outcome

clientEchoFromEnvironment :: Text -> TransportEndpoint -> BS.ByteString -> BS.ByteString -> MVarResult -> IO ()
clientEchoFromEnvironment name endpoint expected reply done = do
  result <- try $ withEndpointEnvironment endpoint $ do
    connection <- connectPluginFromEnvironment name stdin stdout
    case connection of
      Left err -> pure (Left ("connect failed: " <> show err))
      Right transport -> do
        received <- recvMessage transport
        case received of
          Left err -> do
            closeTransport transport
            pure (Left ("recv failed: " <> show err))
          Right payload
            | payload /= expected -> do
                closeTransport transport
                pure (Left ("unexpected payload: " <> show payload))
            | otherwise -> do
                sent <- sendMessage transport reply
                closeTransport transport
                case sent of
                  Left err -> pure (Left ("send failed: " <> show err))
                  Right () -> pure (Right ())
  case result of
    Left (err :: SomeException) -> putMVar done (Left (show err))
    Right outcome -> putMVar done outcome

withEndpointEnvironment :: TransportEndpoint -> IO a -> IO a
withEndpointEnvironment endpoint = bracket setup restore . const
  where
    setup = do
      oldEndpoint <- lookupEnv pluginEndpointEnv
      oldKind <- lookupEnv pluginEndpointKindEnv
      setEnv pluginEndpointEnv (teAddress endpoint)
      setEnv pluginEndpointKindEnv (Text.unpack (endpointKindText (teKind endpoint)))
      pure (oldEndpoint, oldKind)
    restore (oldEndpoint, oldKind) = do
      restoreEnv pluginEndpointEnv oldEndpoint
      restoreEnv pluginEndpointKindEnv oldKind
    restoreEnv key = maybe (unsetEnv key) (setEnv key)

withCleanPluginTransportEnvironment :: IO a -> IO a
withCleanPluginTransportEnvironment = bracket setup restore . const
  where
    setup = do
      oldEndpoint <- lookupEnv pluginEndpointEnv
      oldKind <- lookupEnv pluginEndpointKindEnv
      oldStdioCompatibility <- lookupEnv pluginStdioCompatibilityEnv
      unsetEnv pluginEndpointEnv
      unsetEnv pluginEndpointKindEnv
      unsetEnv pluginStdioCompatibilityEnv
      pure (oldEndpoint, oldKind, oldStdioCompatibility)
    restore (oldEndpoint, oldKind, oldStdioCompatibility) = do
      restoreEnv pluginEndpointEnv oldEndpoint
      restoreEnv pluginEndpointKindEnv oldKind
      restoreEnv pluginStdioCompatibilityEnv oldStdioCompatibility
    restoreEnv key = maybe (unsetEnv key) (setEnv key)

shouldReturnWithin :: (Eq a, Show a) => String -> IO a -> a -> IO ()
shouldReturnWithin label action expected = do
  result <- timeout transportTestTimeoutMicros action
  case result of
    Nothing -> expectationFailure (label <> " timed out")
    Just actual -> actual `shouldBe` expected

type MVarResult = MVar (Either String ())

takeClientResult :: MVarResult -> IO (Either String ())
takeClientResult done = do
  result <- timeout transportTestTimeoutMicros (takeMVar done)
  case result of
    Nothing -> pure (Left "client timed out")
    Just outcome -> pure outcome

transportTestTimeoutMicros :: Int
transportTestTimeoutMicros = 2000000

------------------------------------------------------------------------
-- Test fixtures
------------------------------------------------------------------------

-- | A base valid manifest for constructing validation test cases.
baseManifest :: RPCManifest
baseManifest = RPCManifest
  { rmName         = "test-plugin"
  , rmVersion      = "0.1.0"
  , rmDescription  = ""
  , rmGenerator    = Just (RPCGeneratorDecl "biomes" [])
  , rmSimulation   = Nothing
  , rmOverlay      = Nothing
  , rmCapabilities = []
  , rmParameters   = []
  , rmDataResources = []
  , rmDataDirectory = Nothing
  , rmStartPolicy   = defaultRPCStartPolicy
  }
