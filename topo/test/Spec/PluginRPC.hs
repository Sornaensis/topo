{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spec.PluginRPC (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Control.Concurrent (MVar, forkIO, killThread, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, bracket, finally, try)
import Data.Char (toLower)
import Data.Aeson (Value(..), (.=), object, encode, eitherDecode)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Either (isLeft, isRight)
import Data.List (isInfixOf, isPrefixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Data.Word (Word64)
import System.Directory (doesFileExist, doesPathExist, getCurrentDirectory)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Info (os)
import System.FilePath ((</>))
import System.IO (stdin, stdout)
import System.IO.Temp (withSystemTempFile)
import System.Timeout (timeout)

import Topo.Plugin.RPC
  ( DataMutation(..)
  , DataQuery(..)
  , DataRecord(..)
  , MutateResource(..)
  , MutateResult(..)
  , QueryResource(..)
  , QueryResult(..)
  , RPCConnection(..)
  , HandshakeAuthChallenge(..)
  , RPCError(..)
  , checkHealth
  , invokeGenerator
  , invokeSimulation
  , mutateResource
  , newRPCConnection
  , performHandshakeWithAuth
  , queryResource
  , rpcErrorText
  , rpcGeneratorStage
  , rpcSimNode
  , sendExternalDataSourceGrant
  , sendExternalDataSourceGrantRevocation
  , sendHeartbeat
  , terrainWorldToPayload
  )
import Topo.Plugin.RPC.Manifest
import Topo.Plugin.RPC.Protocol
import Topo.Plugin.RPC.ExternalDataSource
import Topo.Pipeline (PipelineStage(..))
import qualified Topo.Plugin as PluginCore
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
import Topo.Calendar (CalendarDate(..), WorldTime(..), simulationTickSeconds)
import Topo.Hex (defaultHexGridMeta)
import Topo.Overlay (Overlay, emptyOverlay, insertOverlay, lookupOverlay)
import Topo.Overlay.JSON (overlayToJSON)
import Topo.Overlay.Schema
  ( OverlayFieldDef(..)
  , OverlayFieldType(..)
  , OverlaySchema(..)
  , OverlayStorage(..)
  , emptyOverlayDeps
  )
import Topo.Simulation (SimContext(..), SimNode(..), terrainWritesEmpty)
import Topo.Simulation.Schedule (SimulationCatchUpPolicy(..), SimulationScheduleDecl(..), defaultScheduleDecl)
import Topo.Types (ChunkId(..), WorldConfig(..))
import Topo.World (TerrainWorld(..), emptyTerrainChunk, emptyWorld, setTerrainChunk)

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
    , MsgExternalDataSourceGrant
    , MsgExternalDataSourceRevoke
    , MsgExternalDataSourceStatusRequest
    , MsgExternalDataSourceStatus
    , MsgExternalDataSourceOperationResult
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

instance Arbitrary SimulationCatchUpPolicy where
  arbitrary = elements [RunOnceIfDue, SkipMissed]

instance Arbitrary SimulationScheduleDecl where
  arbitrary = do
    Positive interval <- arbitrary
    phase <- choose (0, interval - 1)
    catchUp <- arbitrary
    pure SimulationScheduleDecl
      { schedDeclIntervalTicks = interval
      , schedDeclPhaseTicks = phase
      , schedDeclCatchUpPolicy = catchUp
      }

instance Arbitrary RPCSimulationDecl where
  arbitrary = RPCSimulationDecl <$> listOf arbitrary <*> arbitrary

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
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary DataPagination where
  arbitrary = do
    Positive defaultSize <- arbitrary
    Positive maxSize <- arbitrary
    NonNegative defaultOffset <- arbitrary
    pure DataPagination
      { dpDefaultPageSize = defaultSize
      , dpMaxPageSize = max defaultSize maxSize
      , dpDefaultPageOffset = defaultOffset
      }

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
    pagination <- arbitrary
    Positive schemaVersion <- arbitrary
    Positive resourceVersion <- arbitrary
    pure DataResourceSchema
      { drsSchemaVersion = schemaVersion
      , drsResourceVersion = resourceVersion
      , drsName       = name
      , drsLabel      = label
      , drsHexBound   = hexBound
      , drsFields     = keyField : extraFields
      , drsOperations = ops
      , drsKeyField   = keyName
      , drsOverlay    = ov
      , drsPagination = pagination
      }

instance Arbitrary Handshake where
  arbitrary = Handshake
    <$> pure currentProtocolVersion
    <*> arbitrary
    <*> listOf (elements ["query", "mutate", "subscribe", "launch_auth"])
    <*> arbitrary

instance Arbitrary HandshakeAck where
  arbitrary = HandshakeAck
    <$> pure currentProtocolVersion
    <*> arbitrary
    <*> listOf arbitrary
    <*> arbitrary
    <*> arbitrary

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
      { rmManifestVersion = manifestV3
      , rmName         = name
      , rmVersion      = ver
      , rmRuntime      = defaultRPCManifestRuntime
      , rmDescription  = desc
      , rmUiHints      = defaultRPCUIHints
      , rmGenerator    = gen
      , rmSimulation   = sim
      , rmOverlay      = ov'
      , rmCapabilities = caps
      , rmParameters   = params
      , rmDataResources = dataRes
      , rmDataDirectory = dataDir
      , rmExternalDataSources = []
      , rmExternalDataSourceRefs = []
      , rmStartPolicy   = defaultRPCStartPolicy
      }

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Encode a value to strict bytestring (for parseManifest tests).
jsonBS :: Aeson.ToJSON a => a -> BS.ByteString
jsonBS = BL.toStrict . Aeson.encode

isInvalidNameField :: ManifestError -> Bool
isInvalidNameField (ManifestInvalidField "name" _) = True
isInvalidNameField _ = False

isInvalidOverlaySchemaField :: ManifestError -> Bool
isInvalidOverlaySchemaField (ManifestInvalidField "overlay.schemaFile" _) = True
isInvalidOverlaySchemaField _ = False

isInvalidSimulationScheduleField :: ManifestError -> Bool
isInvalidSimulationScheduleField (ManifestInvalidField "simulation.schedule" _) = True
isInvalidSimulationScheduleField _ = False

isExternalGrantCapabilityError :: ManifestError -> Bool
isExternalGrantCapabilityError (ManifestInvalidField field detail) =
  "externalDataSources.ledger.grants.write.capabilities" `Text.isPrefixOf` field
    && "grant capabilities must be declared" `Text.isInfixOf` detail
isExternalGrantCapabilityError _ = False

isExternalGrantAccessCapabilityError :: ManifestError -> Bool
isExternalGrantAccessCapabilityError (ManifestInvalidField field detail) =
  "externalDataSources.ledger.grants.write.capabilities" `Text.isPrefixOf` field
    && "grant access 'write' requires capability 'mutate'" `Text.isInfixOf` detail
isExternalGrantAccessCapabilityError _ = False

isExternalGrantResourceError :: ManifestError -> Bool
isExternalGrantResourceError (ManifestInvalidField field detail) =
  "externalDataSources.ledger.grants.read.resources" `Text.isPrefixOf` field
    && "grant resources require matching source resources" `Text.isInfixOf` detail
isExternalGrantResourceError _ = False

manifestRuntimeJSON :: Value
manifestRuntimeJSON = object
  [ "protocol" .= object
      [ "min" .= currentProtocolVersion
      , "max" .= currentProtocolVersion
      ]
  ]

-- | A minimal valid manifest as a JSON bytestring.
minimalManifestBS :: BS.ByteString
minimalManifestBS = jsonBS $ object
  [ "manifestVersion" .= manifestV3
  , "name"      .= ("test-plugin" :: Text)
  , "version"   .= ("0.1.0" :: Text)
  , "runtime"   .= manifestRuntimeJSON
  , "generator" .= object
      [ "insertAfter" .= ("biomes" :: Text) ]
  ]

readGoldenJSON :: FilePath -> IO Value
readGoldenJSON relPath = do
  bytes <- readGoldenBytes relPath
  case Aeson.eitherDecode bytes of
    Left err -> expectationFailure ("failed to decode golden JSON " <> relPath <> ": " <> err) >> pure Null
    Right value -> pure value

readGoldenBytes :: FilePath -> IO BL.ByteString
readGoldenBytes relPath = do
  path <- findGoldenPath relPath
  BL.readFile path

findGoldenPath :: FilePath -> IO FilePath
findGoldenPath relPath = do
  cwd <- getCurrentDirectory
  let candidates = [cwd </> relPath, cwd </> ".." </> relPath]
  go candidates
  where
    go [] = expectationFailure ("missing golden file: " <> relPath) >> pure relPath
    go (candidate:rest) = do
      exists <- doesFileExist candidate
      if exists then pure candidate else go rest

-- | A full manifest with all fields populated.
fullManifestBS :: BS.ByteString
fullManifestBS = jsonBS $ object
  [ "manifestVersion" .= manifestV3
  , "name"         .= ("civilization" :: Text)
  , "version"      .= ("1.0.0" :: Text)
  , "runtime"      .= manifestRuntimeJSON
  , "description"  .= ("Civilization sim overlay" :: Text)
  , "generator"    .= object
      [ "insertAfter" .= ("biomes" :: Text)
      , "requires"    .= (["biomes", "rivers"] :: [Text])
      ]
  , "simulation" .= object
      [ "dependencies" .= (["weather"] :: [Text])
      , "interval_ticks" .= (1 :: Word64)
      , "phase_ticks" .= (0 :: Word64)
      , "catch_up" .= ("run_once_if_due" :: Text)
      ]
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
          fmap rsdSchedule (rmSimulation m) `shouldBe` Just defaultScheduleDecl
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

    it "defaults missing simulation schedule fields to hourly" $ do
      let manifestBytes = jsonBS $ object
            [ "manifestVersion" .= manifestV3
            , "name" .= ("scheduled-default" :: Text)
            , "version" .= ("1.0.0" :: Text)
            , "runtime" .= manifestRuntimeJSON
            , "simulation" .= object ["dependencies" .= ([] :: [Text])]
            , "overlay" .= object ["schemaFile" .= ("scheduled-default.toposchema" :: Text)]
            , "capabilities" .= (["writeOverlay"] :: [Text])
            ]
      case parseManifest manifestBytes of
        Right manifest -> fmap rsdSchedule (rmSimulation manifest) `shouldBe` Just defaultScheduleDecl
        Left err -> expectationFailure (Text.unpack err)

    it "surfaces invalid interval and phase schedule diagnostics" $ do
      let scheduled name fields = jsonBS $ object
            [ "manifestVersion" .= manifestV3
            , "name" .= (name :: Text)
            , "version" .= ("1.0.0" :: Text)
            , "runtime" .= manifestRuntimeJSON
            , "simulation" .= object fields
            , "overlay" .= object ["schemaFile" .= (name <> ".toposchema")]
            , "capabilities" .= (["writeOverlay"] :: [Text])
            ]
          expectScheduleDiagnostic bytes =
            case parseManifest bytes of
              Left err -> expectationFailure (Text.unpack err)
              Right manifest -> validateManifest manifest
                `shouldSatisfy` any isInvalidSimulationScheduleField
      expectScheduleDiagnostic $ scheduled "bad-interval"
        [ "dependencies" .= ([] :: [Text])
        , "interval_ticks" .= (0 :: Word64)
        ]
      expectScheduleDiagnostic $ scheduled "bad-phase"
        [ "dependencies" .= ([] :: [Text])
        , "interval_ticks" .= (3 :: Word64)
        , "phase_ticks" .= (3 :: Word64)
        ]

    it "rejects unknown simulation catch-up policies" $ do
      let manifestBytes = jsonBS $ object
            [ "manifestVersion" .= manifestV3
            , "name" .= ("bad-catch-up" :: Text)
            , "version" .= ("1.0.0" :: Text)
            , "runtime" .= manifestRuntimeJSON
            , "simulation" .= object
                [ "dependencies" .= ([] :: [Text])
                , "catch_up" .= ("replay_all" :: Text)
                ]
            , "overlay" .= object ["schemaFile" .= ("bad-catch-up.toposchema" :: Text)]
            , "capabilities" .= (["writeOverlay"] :: [Text])
            ]
      parseManifest manifestBytes `shouldSatisfy` isLeft

    it "rejects invalid JSON" $ do
      let result = parseManifest "not valid json"
      result `shouldSatisfy` isLeft

    it "rejects JSON missing required name field" $ do
      let bs = jsonBS $ object
            [ "manifestVersion" .= manifestV3
            , "version" .= ("1.0" :: Text)
            , "runtime" .= manifestRuntimeJSON
            ]
      parseManifest bs `shouldSatisfy` isLeft

    it "rejects JSON missing required version field" $ do
      let bs = jsonBS $ object
            [ "manifestVersion" .= manifestV3
            , "name" .= ("x" :: Text)
            , "runtime" .= manifestRuntimeJSON
            ]
      parseManifest bs `shouldSatisfy` isLeft

    it "rejects JSON missing required manifestVersion field" $ do
      let bs = jsonBS $ object
            [ "name" .= ("x" :: Text)
            , "version" .= ("1.0" :: Text)
            , "runtime" .= manifestRuntimeJSON
            ]
      parseManifest bs `shouldSatisfy` isLeft

    it "rejects JSON missing required runtime field" $ do
      let bs = jsonBS $ object
            [ "manifestVersion" .= manifestV3
            , "name" .= ("x" :: Text)
            , "version" .= ("1.0" :: Text)
            ]
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
            [ "manifestVersion" .= manifestV3
            , "name"      .= ("p" :: Text)
            , "version"   .= ("1" :: Text)
            , "runtime"   .= manifestRuntimeJSON
            , "generator" .= object [ "insertAfter" .= ("base" :: Text) ]
            ]
      case parseManifest bs of
        Right m -> case rmGenerator m of
          Just g  -> rgdRequires g `shouldBe` []
          Nothing -> expectationFailure "expected generator"
        Left _ -> expectationFailure "parse failed"

    it "defaults missing simulation dependencies to empty list" $ do
      let bs = jsonBS $ object
            [ "manifestVersion" .= manifestV3
            , "name"       .= ("p" :: Text)
            , "version"    .= ("1" :: Text)
            , "runtime"    .= manifestRuntimeJSON
            , "simulation" .= object []
            , "overlay"    .= object [ "schemaFile" .= ("x" :: Text) ]
            ]
      case parseManifest bs of
        Right m -> case rmSimulation m of
          Just s -> do
            rsdDependencies s `shouldBe` []
            rsdSchedule s `shouldBe` defaultScheduleDecl
          Nothing -> expectationFailure "expected simulation"
        Left _ -> expectationFailure "parse failed"

    it "rejects malformed parameter ranges with an actionable parse error" $ do
      let bs = jsonBS $ object
            [ "manifestVersion" .= manifestV3
            , "name"      .= ("p" :: Text)
            , "version"   .= ("1" :: Text)
            , "runtime"   .= manifestRuntimeJSON
            , "generator" .= object [ "insertAfter" .= ("base" :: Text) ]
            , "config"    .= object
                [ "parameters" .=
                    [ object
                        [ "name" .= ("bad_range" :: Text)
                        , "label" .= ("Bad range" :: Text)
                        , "type" .= ("float" :: Text)
                        , "default" .= (0.0 :: Double)
                        , "range" .= ([0.0 :: Double])
                        ]
                    ]
                ]
            ]
      parseManifest bs `shouldSatisfy` isLeft

    it "parses manifest v3 runtime, UI hints, and external data source declarations" $ do
      case Aeson.fromJSON manifestV3ProviderExample of
        Aeson.Success m -> do
          rmManifestVersion m `shouldBe` manifestV3
          rmrProtocolMin (rmRuntime m) `shouldBe` currentProtocolVersion
          rmrProtocolMax (rmRuntime m) `shouldBe` currentProtocolVersion
          ruiDisplayName (rmUiHints m) `shouldBe` Just "Civilization"
          case rmExternalDataSources m of
            [source] -> do
              redsdConnection source `shouldBe` Just (object ["handle" .= ("provider-owned:settlement-ledger" :: Text)])
              redsdConfigRefs source `shouldBe`
                [ RPCExternalDataSourceConfigRef
                    { redscrName = "settlement-ledger-binding"
                    , redscrOrigin = ExternalConfigProvider
                    , redscrKey = "civilization.settlement-ledger"
                    , redscrRequired = True
                    , redscrCompatibility = Just "manifest-v3"
                    , redscrMetadata = Just (object ["handle" .= ("provider-owned:settlement-ledger" :: Text)])
                    }
                ]
              redssProviderId (redsdStatus source) `shouldBe` Just "civilization"
              redssAvailability (redsdStatus source) `shouldBe` Just ExternalAvailabilityAvailable
              redssHealth (redsdStatus source) `shouldBe` Just ExternalHealthHealthy
              redssAccessMode (redsdStatus source) `shouldBe` Just ExternalAccessModeReadOnly
              redssCapabilityScope (redsdStatus source) `shouldBe` [ExternalSourceQuery, ExternalSourceHealth]
              redssVersion (redsdStatus source) `shouldBe` Just "settlement-ledger.v1"
              redssCompatibility (redsdStatus source) `shouldBe` Just "manifest-v3"
              redssDiagnostics (redsdStatus source) `shouldBe` Just (object ["reportedBy" .= ("civilization" :: Text)])
              map redsgName (redsdGrants source) `shouldBe` ["settlement-read"]
              case redsdGrants source of
                [grant] -> do
                  redsgAccess grant `shouldBe` [ExternalAccessRead]
                  redsgCapabilities grant `shouldBe` [ExternalSourceQuery, ExternalSourceHealth]
                  redsgStatus grant `shouldBe` defaultRPCExternalDataSourceStatus
                    { redssState = ExternalStatusReady
                    , redssMessage = Just "Read grant can be brokered to dependent plugins"
                    , redssProviderId = Just "civilization"
                    , redssAvailability = Just ExternalAvailabilityAvailable
                    , redssHealth = Just ExternalHealthHealthy
                    , redssAccessMode = Just ExternalAccessModeReadOnly
                    , redssCapabilityScope = [ExternalSourceQuery, ExternalSourceHealth]
                    , redssVersion = Just "settlement-read.v1"
                    , redssCompatibility = Just "manifest-v3"
                    , redssDiagnostics = Just (object ["grant" .= ("settlement-read" :: Text)])
                    }
                  redsgConfigRefs grant `shouldBe`
                    [ RPCExternalDataSourceConfigRef
                        { redscrName = "settlement-read-binding"
                        , redscrOrigin = ExternalConfigProvider
                        , redscrKey = "civilization.settlement-read"
                        , redscrRequired = True
                        , redscrCompatibility = Just "manifest-v3"
                        , redscrMetadata = Just (object ["grant" .= ("settlement-read" :: Text)])
                        }
                    ]
                _ -> expectationFailure "expected exactly one external data-source grant"
            _ -> expectationFailure "expected exactly one external data source"
          validateManifest m `shouldBe` []
        Aeson.Error err -> expectationFailure err

    it "parses manifest v3 external data source references" $ do
      case Aeson.fromJSON manifestV3ConsumerExample of
        Aeson.Success m -> do
          rmManifestVersion m `shouldBe` manifestV3
          length (rmExternalDataSourceRefs m) `shouldBe` 1
          case rmExternalDataSourceRefs m of
            [ref] -> do
              redsrProvider ref `shouldBe` Just "civilization"
              redsrAccess ref `shouldBe` [ExternalAccessRead]
              redsrGrant ref `shouldBe` Just "settlement-read"
              redsrReference ref `shouldBe` Just (object ["binding" .= ("trade-routes:settlements" :: Text)])
              redsrConfigRefs ref `shouldBe`
                [ RPCExternalDataSourceConfigRef
                    { redscrName = "settlements-binding"
                    , redscrOrigin = ExternalConfigDeployment
                    , redscrKey = "trade-routes.settlements"
                    , redscrRequired = True
                    , redscrCompatibility = Just "manifest-v3"
                    , redscrMetadata = Just (object ["binding" .= ("trade-routes:settlements" :: Text)])
                    }
                ]
              redssProviderId (redsrStatus ref) `shouldBe` Just "civilization"
              redssAvailability (redsrStatus ref) `shouldBe` Just ExternalAvailabilityUnknown
              redssHealth (redsrStatus ref) `shouldBe` Just ExternalHealthUnknown
              redssAccessMode (redsrStatus ref) `shouldBe` Just ExternalAccessModeReadOnly
              redssCapabilityScope (redsrStatus ref) `shouldBe` [ExternalSourceQuery]
              redssVersion (redsrStatus ref) `shouldBe` Just "settlement-ledger.v1"
              redssCompatibility (redsrStatus ref) `shouldBe` Just "manifest-v3"
              redssDiagnostics (redsrStatus ref) `shouldBe` Just (object ["resolution" .= ("dependency-startup" :: Text)])
            _ -> expectationFailure "expected exactly one external data source reference"
          validateManifest m `shouldBe` []
        Aeson.Error err -> expectationFailure err

    it "parses backend-neutral external data-source config reference origins" $ do
      let origins =
            [ ("user", ExternalConfigUser)
            , ("provider", ExternalConfigProvider)
            , ("environment", ExternalConfigEnvironment)
            , ("deployment", ExternalConfigDeployment)
            ]
      mapM_
        (\(raw, expected) ->
          case Aeson.fromJSON (String raw) of
            Aeson.Success origin -> origin `shouldBe` expected
            Aeson.Error err -> expectationFailure err)
        origins
      case Aeson.fromJSON
          (object
            [ "name" .= ("env-settlements" :: Text)
            , "origin" .= ("environment" :: Text)
            , "key" .= ("TOPO_SETTLEMENT_LEDGER" :: Text)
            , "compatibility" .= ("manifest-v3" :: Text)
            , "metadata" .= object ["scope" .= ("deployment" :: Text)]
            ]) of
        Aeson.Success configRef -> do
          redscrName configRef `shouldBe` "env-settlements"
          redscrOrigin configRef `shouldBe` ExternalConfigEnvironment
          redscrKey configRef `shouldBe` "TOPO_SETTLEMENT_LEDGER"
          redscrRequired configRef `shouldBe` True
          redscrCompatibility configRef `shouldBe` Just "manifest-v3"
          redscrMetadata configRef `shouldBe` Just (object ["scope" .= ("deployment" :: Text)])
        Aeson.Error err -> expectationFailure err

    it "rejects external data-source grants outside declared capabilities" $ do
      let source = RPCExternalDataSourceDecl
            { redsdName = "ledger"
            , redsdLabel = "Ledger"
            , redsdDescription = ""
            , redsdKind = "catalog"
            , redsdCapabilities = [ExternalSourceQuery]
            , redsdResources = ["items"]
            , redsdStatus = defaultRPCExternalDataSourceStatus { redssState = ExternalStatusReady }
            , redsdConnection = Nothing
            , redsdConfigRefs = []
            , redsdGrants =
                [ RPCExternalDataSourceGrant
                    { redsgName = "write"
                    , redsgAccess = [ExternalAccessWrite]
                    , redsgCapabilities = [ExternalSourceMutate]
                    , redsgResources = ["items"]
                    , redsgStatus = defaultRPCExternalDataSourceStatus { redssState = ExternalStatusReady }
                    , redsgReference = Nothing
                    , redsgConfigRefs = []
                    }
                ]
            , redsdUiHints = defaultRPCUIHints
            }
          manifest = baseManifest
            { rmExternalDataSources = [source]
            }
      validateManifest manifest `shouldSatisfy` any isExternalGrantCapabilityError

    it "rejects external data-source grant access without required capabilities" $ do
      let source = RPCExternalDataSourceDecl
            { redsdName = "ledger"
            , redsdLabel = "Ledger"
            , redsdDescription = ""
            , redsdKind = "catalog"
            , redsdCapabilities = [ExternalSourceQuery, ExternalSourceMutate]
            , redsdResources = ["items"]
            , redsdStatus = defaultRPCExternalDataSourceStatus { redssState = ExternalStatusReady }
            , redsdConnection = Nothing
            , redsdConfigRefs = []
            , redsdGrants =
                [ RPCExternalDataSourceGrant
                    { redsgName = "write"
                    , redsgAccess = [ExternalAccessWrite]
                    , redsgCapabilities = [ExternalSourceQuery]
                    , redsgResources = ["items"]
                    , redsgStatus = defaultRPCExternalDataSourceStatus { redssState = ExternalStatusReady }
                    , redsgReference = Nothing
                    , redsgConfigRefs = []
                    }
                ]
            , redsdUiHints = defaultRPCUIHints
            }
          manifest = baseManifest
            { rmExternalDataSources = [source]
            }
      validateManifest manifest `shouldSatisfy` any isExternalGrantAccessCapabilityError

    it "rejects external data-source grant resources without source resources" $ do
      let source = RPCExternalDataSourceDecl
            { redsdName = "ledger"
            , redsdLabel = "Ledger"
            , redsdDescription = ""
            , redsdKind = "catalog"
            , redsdCapabilities = [ExternalSourceQuery]
            , redsdResources = []
            , redsdStatus = defaultRPCExternalDataSourceStatus { redssState = ExternalStatusReady }
            , redsdConnection = Nothing
            , redsdConfigRefs = []
            , redsdGrants =
                [ RPCExternalDataSourceGrant
                    { redsgName = "read"
                    , redsgAccess = [ExternalAccessRead]
                    , redsgCapabilities = [ExternalSourceQuery]
                    , redsgResources = ["items"]
                    , redsgStatus = defaultRPCExternalDataSourceStatus { redssState = ExternalStatusReady }
                    , redsgReference = Nothing
                    , redsgConfigRefs = []
                    }
                ]
            , redsdUiHints = defaultRPCUIHints
            }
          manifest = baseManifest
            { rmExternalDataSources = [source]
            }
      validateManifest manifest `shouldSatisfy` any isExternalGrantResourceError

    it "validates backend-neutral external data-source config references" $ do
      let source = RPCExternalDataSourceDecl
            { redsdName = "ledger"
            , redsdLabel = "Ledger"
            , redsdDescription = ""
            , redsdKind = "catalog"
            , redsdCapabilities = [ExternalSourceQuery]
            , redsdResources = []
            , redsdStatus = defaultRPCExternalDataSourceStatus { redssState = ExternalStatusReady }
            , redsdConnection = Nothing
            , redsdConfigRefs =
                [ RPCExternalDataSourceConfigRef
                    { redscrName = ""
                    , redscrOrigin = ExternalConfigEnvironment
                    , redscrKey = ""
                    , redscrRequired = True
                    , redscrCompatibility = Just ""
                    , redscrMetadata = Just (String "not-object")
                    }
                ]
            , redsdGrants = []
            , redsdUiHints = defaultRPCUIHints
            }
          manifest = baseManifest { rmExternalDataSources = [source] }
          messages = map manifestErrorMessage (validateManifest manifest)
      messages `shouldSatisfy` any (Text.isInfixOf "configRefs.<empty>.name")
      messages `shouldSatisfy` any (Text.isInfixOf "config reference key")
      messages `shouldSatisfy` any (Text.isInfixOf "configRefs.<empty>.compatibility")
      messages `shouldSatisfy` any (Text.isInfixOf "configRefs.<empty>.metadata")

    it "rejects external data source status without required state" $ do
      case Aeson.fromJSON (object []) :: Aeson.Result RPCExternalDataSourceStatus of
        Aeson.Error _ -> pure ()
        Aeson.Success status -> expectationFailure ("unexpected status: " <> show status)

    it "rejects null external data-source status metadata fields" $ do
      let rejectNull :: Text -> Expectation
          rejectNull field =
            case Aeson.fromJSON (object ["state" .= ("ready" :: Text), AesonKey.fromText field .= Null]) :: Aeson.Result RPCExternalDataSourceStatus of
              Aeson.Error _ -> pure ()
              Aeson.Success status -> expectationFailure ("unexpected status: " <> show status)
      rejectNull "providerId"
      rejectNull "provider_id"
      rejectNull "availability"
      rejectNull "health"
      rejectNull "accessMode"
      rejectNull "access_mode"
      rejectNull "capabilityScope"
      rejectNull "capability_scope"
      rejectNull "version"
      rejectNull "compatibility"
      rejectNull "diagnostics"
      rejectNull "observedAt"
      rejectNull "observed_at"
      rejectNull "fresh"

    it "parses optional external data-source freshness metadata" $ do
      let encoded = object
            [ "state" .= ("ready" :: Text)
            , "observedAt" .= ("2026-07-10T12:34:56Z" :: Text)
            , "fresh" .= False
            ]
      case Aeson.fromJSON encoded :: Aeson.Result RPCExternalDataSourceStatus of
        Aeson.Success status -> do
          redssObservedAt status `shouldSatisfy` maybe False (const True)
          redssFresh status `shouldBe` False
        Aeson.Error err -> expectationFailure err

    it "validates backend-neutral external data-source status metadata" $ do
      let source = RPCExternalDataSourceDecl
            { redsdName = "ledger"
            , redsdLabel = "Ledger"
            , redsdDescription = ""
            , redsdKind = "catalog"
            , redsdCapabilities = [ExternalSourceQuery]
            , redsdResources = []
            , redsdStatus = defaultRPCExternalDataSourceStatus
                { redssState = ExternalStatusReady
                , redssCapabilityScope = [ExternalSourceMutate]
                , redssDiagnostics = Just (String "not-object")
                }
            , redsdConnection = Nothing
            , redsdConfigRefs = []
            , redsdGrants = []
            , redsdUiHints = defaultRPCUIHints
            }
          manifest = baseManifest { rmExternalDataSources = [source] }
          messages = map manifestErrorMessage (validateManifest manifest)
      messages `shouldSatisfy` any (Text.isInfixOf "status.capabilityScope")
      messages `shouldSatisfy` any (Text.isInfixOf "diagnostics")

    it "round-trips external data-source protocol message tags and aliases" $ do
      Aeson.fromJSON (String "external_data_source_grant") `shouldBe` Aeson.Success MsgExternalDataSourceGrant
      Aeson.fromJSON (String "external_data_source_grant_revoked") `shouldBe` Aeson.Success MsgExternalDataSourceRevoke
      Aeson.fromJSON (String "external_data_source_status_check") `shouldBe` Aeson.Success MsgExternalDataSourceStatusRequest
      Aeson.fromJSON (String "external_data_source_grant_ack") `shouldBe` Aeson.Success MsgExternalDataSourceOperationResult
      Aeson.fromJSON (String "external_data_source_revoke_result") `shouldBe` Aeson.Success MsgExternalDataSourceOperationResult
      Aeson.toJSON MsgExternalDataSourceStatus `shouldBe` String "external_data_source_status"
      Aeson.toJSON MsgExternalDataSourceOperationResult `shouldBe` String "external_data_source_operation_result"

    it "encodes backend-neutral external data-source grant, revocation, and operation result payloads" $ do
      let status = defaultRPCExternalDataSourceStatus
            { redssState = ExternalStatusReady
            , redssProviderId = Just "civilization"
            , redssAvailability = Just ExternalAvailabilityAvailable
            , redssHealth = Just ExternalHealthHealthy
            , redssAccessMode = Just ExternalAccessModeReadOnly
            , redssCapabilityScope = [ExternalSourceQuery, ExternalSourceHealth]
            , redssDiagnostics = Just (object ["reportedBy" .= ("provider" :: Text)])
            }
          grant = RPCExternalDataSourceGrantMessage
            { redsgmOperationId = Just "grant-op-1"
            , redsgmOperationEpoch = Just 1
            , redsgmProviderId = "civilization"
            , redsgmConsumerId = Just "trade-routes"
            , redsgmSource = "settlement-ledger"
            , redsgmGrant = "settlement-read"
            , redsgmAccess = [ExternalAccessRead]
            , redsgmResources = ["settlements"]
            , redsgmCapabilityScope = [ExternalSourceQuery, ExternalSourceHealth]
            , redsgmStatus = status
            , redsgmReference = Just (object ["binding" .= ("opaque" :: Text)])
            , redsgmConfigRefs = []
            , redsgmDiagnostics = Just (object ["scope" .= ("startup" :: Text)])
            }
      eitherDecode (encode grant) `shouldBe` Right grant
      let revoked = RPCExternalDataSourceGrantRevocation
            { redsrvOperationId = Just "revoke-op-1"
            , redsrvOperationEpoch = Just 2
            , redsrvProviderId = "civilization"
            , redsrvConsumerId = Just "trade-routes"
            , redsrvSource = "settlement-ledger"
            , redsrvGrant = "settlement-read"
            , redsrvReason = Just "provider unavailable"
            , redsrvStatus = revokedExternalDataSourceStatus "civilization" (Just "provider unavailable")
            , redsrvReference = Just (object ["binding" .= ("opaque" :: Text)])
            , redsrvDiagnostics = Just (object ["reportedBy" .= ("host" :: Text)])
            }
      eitherDecode (encode revoked) `shouldBe` Right revoked
      redssAccessMode (redsrvStatus revoked) `shouldBe` Just ExternalAccessModeDisabled
      let operationResult = RPCExternalDataSourceOperationResult
            { redsoOperationId = "grant-op-1"
            , redsoOperationEpoch = Just 1
            , redsoOperation = ExternalDataSourceGrantOperation
            , redsoProviderId = "civilization"
            , redsoConsumerId = "trade-routes"
            , redsoSource = "settlement-ledger"
            , redsoGrant = "settlement-read"
            , redsoAccepted = True
            , redsoApplied = True
            , redsoStatus = "applied"
            , redsoMessage = Just "grant applied"
            , redsoError = Nothing
            , redsoDiagnostics = Just (object ["reportedBy" .= ("consumer" :: Text)])
            }
      eitherDecode (encode operationResult) `shouldBe` Right operationResult
      let legacyGrant = object
            [ "providerId" .= ("civilization" :: Text)
            , "consumerId" .= ("trade-routes" :: Text)
            , "source" .= ("settlement-ledger" :: Text)
            , "grant" .= ("settlement-read" :: Text)
            , "status" .= status
            ]
      case Aeson.fromJSON legacyGrant of
        Aeson.Success decodedGrant -> do
          redsgmOperationId decodedGrant `shouldBe` Nothing
          redsgmOperationEpoch decodedGrant `shouldBe` Nothing
        Aeson.Error err -> expectationFailure err
      let aliasedGrant = object
            [ "broker_operation_id" .= ("grant-op-alias" :: Text)
            , "epoch" .= (7 :: Word64)
            , "provider" .= ("civilization" :: Text)
            , "consumer" .= ("trade-routes" :: Text)
            , "sourceName" .= ("settlement-ledger" :: Text)
            , "grantName" .= ("settlement-read" :: Text)
            , "status" .= status
            ]
      case Aeson.fromJSON aliasedGrant of
        Aeson.Success decodedGrant -> do
          redsgmOperationId decodedGrant `shouldBe` Just "grant-op-alias"
          redsgmOperationEpoch decodedGrant `shouldBe` Just 7
        Aeson.Error err -> expectationFailure err
      let legacyRevocation = object
            [ "providerId" .= ("civilization" :: Text)
            , "consumerId" .= ("trade-routes" :: Text)
            , "source" .= ("settlement-ledger" :: Text)
            , "grant" .= ("settlement-read" :: Text)
            ]
      case Aeson.fromJSON legacyRevocation of
        Aeson.Success decodedRevocation -> do
          redsrvOperationId decodedRevocation `shouldBe` Nothing
          redsrvOperationEpoch decodedRevocation `shouldBe` Nothing
        Aeson.Error err -> expectationFailure err
      let aliasedOperationResult = object
            [ "operation_id" .= ("revoke-op-alias" :: Text)
            , "operation_epoch" .= (8 :: Word64)
            , "action" .= ("revoked" :: Text)
            , "provider" .= ("civilization" :: Text)
            , "consumer" .= ("trade-routes" :: Text)
            , "sourceName" .= ("settlement-ledger" :: Text)
            , "grantName" .= ("settlement-read" :: Text)
            , "accepted" .= True
            , "applied" .= True
            , "status" .= ("applied" :: Text)
            , "message" .= Null
            , "errorMessage" .= Null
            ]
      case Aeson.fromJSON aliasedOperationResult of
        Aeson.Success decodedResult -> do
          redsoOperationId decodedResult `shouldBe` "revoke-op-alias"
          redsoOperationEpoch decodedResult `shouldBe` Just 8
          redsoOperation decodedResult `shouldBe` ExternalDataSourceRevokeOperation
          redsoMessage decodedResult `shouldBe` Nothing
          redsoError decodedResult `shouldBe` Nothing
        Aeson.Error err -> expectationFailure err

    it "reports external data-source status snapshots from manifests without backend internals" $ do
      case Aeson.fromJSON manifestV3ProviderExample of
        Aeson.Error err -> expectationFailure err
        Aeson.Success providerManifest -> do
          let request = RPCExternalDataSourceStatusRequest
                { redssrProviderId = Just "civilization"
                , redssrConsumerId = Nothing
                , redssrSources = []
                , redssrGrants = []
                , redssrIncludeDiagnostics = False
                , redssrReference = Nothing
                }
              report = externalDataSourceStatusReportFromManifest providerManifest request
          map redsstSource (redssReportStatuses report) `shouldBe` ["settlement-ledger", "settlement-ledger"]
          map redsstGrant (redssReportStatuses report) `shouldBe` [Nothing, Just "settlement-read"]
          map redsstDiagnostics (redssReportStatuses report) `shouldBe` [Nothing, Nothing]
          let grantRequest = request
                { redssrGrants = ["settlement-read"]
                , redssrIncludeDiagnostics = True
                }
              grantReport = externalDataSourceStatusReportFromManifest providerManifest grantRequest
          map redsstGrant (redssReportStatuses grantReport) `shouldBe` [Just "settlement-read"]
          map redsstDiagnostics (redssReportStatuses grantReport) `shouldBe` [Just (object ["grant" .= ("settlement-read" :: Text)])]

    it "stamps applied external data-source reports and marks omitted entries stale" $ do
      let observedAt = read "1970-01-01 00:00:00 UTC" :: UTCTime
          readyStatus = defaultRPCExternalDataSourceStatus { redssState = ExternalStatusReady }
          grant name = RPCExternalDataSourceGrant
            { redsgName = name
            , redsgAccess = [ExternalAccessRead]
            , redsgCapabilities = [ExternalSourceQuery]
            , redsgResources = ["records"]
            , redsgStatus = readyStatus
            , redsgReference = Nothing
            , redsgConfigRefs = []
            }
          source = RPCExternalDataSourceDecl
            { redsdName = "ledger"
            , redsdLabel = "Ledger"
            , redsdDescription = ""
            , redsdKind = "catalog"
            , redsdCapabilities = [ExternalSourceQuery]
            , redsdResources = ["records"]
            , redsdStatus = readyStatus
            , redsdConnection = Nothing
            , redsdConfigRefs = []
            , redsdGrants = [grant "included", grant "omitted"]
            , redsdUiHints = defaultRPCUIHints
            }
          entry mGrant = RPCExternalDataSourceStatusEntry
            { redsstProviderId = "provider"
            , redsstConsumerId = Nothing
            , redsstSource = "ledger"
            , redsstGrant = mGrant
            , redsstAccess = maybe [] (const [ExternalAccessRead]) mGrant
            , redsstResources = ["records"]
            , redsstCapabilityScope = [ExternalSourceQuery]
            , redsstStatus = readyStatus
            , redsstReference = Nothing
            , redsstConfigRefs = []
            , redsstDiagnostics = Nothing
            }
          report = RPCExternalDataSourceStatusReport
            { redssReportStatuses = [entry Nothing, entry (Just "included")]
            , redssReportDiagnostics = Nothing
            }
          consumerOnlyReport = RPCExternalDataSourceStatusReport
            { redssReportStatuses = [(entry (Just "included")) { redsstConsumerId = Just "consumer" }]
            , redssReportDiagnostics = Nothing
            }
          manifest = baseManifest { rmExternalDataSources = [source] }
          applied = applyExternalDataSourceStatusReport observedAt "provider" report manifest
          consumerOnlyApplied = applyExternalDataSourceStatusReport observedAt "provider" consumerOnlyReport manifest
      case rmExternalDataSources applied of
        [appliedSource] -> do
          redssObservedAt (redsdStatus appliedSource) `shouldBe` Just observedAt
          redssFresh (redsdStatus appliedSource) `shouldBe` True
          case redsdGrants appliedSource of
            [included, omitted] -> do
              redssObservedAt (redsgStatus included) `shouldBe` Just observedAt
              redssFresh (redsgStatus included) `shouldBe` True
              redssObservedAt (redsgStatus omitted) `shouldBe` Nothing
              redssFresh (redsgStatus omitted) `shouldBe` False
              redssMessage (redsgStatus omitted) `shouldSatisfy` maybe False (Text.isInfixOf "omitted grant")
              redssState (redsgStatus omitted) `shouldBe` ExternalStatusReady
            other -> expectationFailure ("expected two grants, got " <> show other)
        other -> expectationFailure ("expected one source, got " <> show other)
      case rmExternalDataSources consumerOnlyApplied of
        [consumerOnlySource] -> do
          redssFresh (redsdStatus consumerOnlySource) `shouldBe` False
          redssMessage (redsdStatus consumerOnlySource) `shouldSatisfy` maybe False (Text.isInfixOf "omitted source")
          case redsdGrants consumerOnlySource of
            [included, _omitted] -> do
              redssObservedAt (redsgStatus included) `shouldBe` Nothing
              redssFresh (redsgStatus included) `shouldBe` False
              redssMessage (redsgStatus included) `shouldSatisfy` maybe False (Text.isInfixOf "omitted grant")
            other -> expectationFailure ("expected two consumer-only grants, got " <> show other)
        other -> expectationFailure ("expected one consumer-only source, got " <> show other)

    it "classifies external data-source availability for startup gates" $ do
      case Aeson.fromJSON manifestV3ConsumerExample of
        Aeson.Error err -> expectationFailure err
        Aeson.Success consumerManifest ->
          case externalDataSourceManifestStartupDecision consumerManifest of
            ExternalDataSourceStartupBlocked dependency reason -> do
              dependency `shouldBe` "civilization:settlement-ledger:settlement-read"
              reason `shouldSatisfy` Text.isInfixOf "required external data source"
            other -> expectationFailure ("expected startup block, got " <> show other)
      let degradedSource = RPCExternalDataSourceDecl
            { redsdName = "ledger"
            , redsdLabel = "Ledger"
            , redsdDescription = ""
            , redsdKind = "catalog"
            , redsdCapabilities = [ExternalSourceQuery]
            , redsdResources = []
            , redsdStatus = defaultRPCExternalDataSourceStatus { redssState = ExternalStatusDegraded }
            , redsdConnection = Nothing
            , redsdConfigRefs = []
            , redsdGrants = []
            , redsdUiHints = defaultRPCUIHints
            }
          degradedManifest = baseManifest { rmExternalDataSources = [degradedSource] }
      case externalDataSourceManifestStartupDecision degradedManifest of
        ExternalDataSourceStartupDegraded dependency reason -> do
          dependency `shouldBe` "ledger"
          reason `shouldSatisfy` Text.isInfixOf "degraded"
        other -> expectationFailure ("expected startup degradation, got " <> show other)

  ------------------------------------
  -- Manifest v3 schema and golden docs
  ------------------------------------
  describe "Manifest v3 schema and golden docs" $ do
    it "matches the committed JSON Schema golden" $ do
      golden <- readGoldenJSON "docs/plugin-dev/manifest-v3.schema.json"
      golden `shouldBe` manifestV3Schema

    it "matches the provider example golden" $ do
      golden <- readGoldenJSON "docs/plugin-dev/examples/manifest-v3-provider.json"
      golden `shouldBe` manifestV3ProviderExample

    it "matches the consumer example golden" $ do
      golden <- readGoldenJSON "docs/plugin-dev/examples/manifest-v3-consumer.json"
      golden `shouldBe` manifestV3ConsumerExample

    it "keeps schema and examples backend-neutral" $ do
      bytes <- mconcat <$> traverse readGoldenBytes
        [ "docs/plugin-dev/manifest-v3.schema.json"
        , "docs/plugin-dev/examples/manifest-v3-provider.json"
        , "docs/plugin-dev/examples/manifest-v3-consumer.json"
        ]
      let lowered = map toLower (BLC.unpack bytes)
          schemaText = map toLower (BLC.unpack (encode manifestV3Schema))
      lowered `shouldNotSatisfy` isInfixOf "sqlite"
      schemaText `shouldSatisfy` isInfixOf "migrations, schemas, connection details, and consistency rules"
      schemaText `shouldSatisfy` isInfixOf "must not prescribe backend-specific migration tables or schema rules"

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

    it "detects path-like plugin names before executable resolution" $ do
      let m = baseManifest { rmName = "../escape" }
      validateManifest m `shouldSatisfy` any isInvalidNameField

    it "detects overlay schema paths that escape the plugin directory" $ do
      let m = baseManifest { rmOverlay = Just (RPCOverlayDecl "../escape.toposchema") }
      validateManifest m `shouldSatisfy` any isInvalidOverlaySchemaField

    it "detects empty version" $ do
      let m = baseManifest { rmVersion = "" }
      validateManifest m `shouldSatisfy` elem ManifestEmptyVersion

    it "detects simulation without overlay" $ do
      let m = baseManifest
            { rmSimulation = Just RPCSimulationDecl
                { rsdDependencies = []
                , rsdSchedule = defaultScheduleDecl
                }
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
            , rmSimulation = Just RPCSimulationDecl
                { rsdDependencies = []
                , rsdSchedule = defaultScheduleDecl
                }
            , rmOverlay    = Just (RPCOverlayDecl "test.toposchema")
            }
      validateManifest m `shouldBe` []

    it "detects invalid simulation schedule declarations" $ do
      let invalidInterval = baseManifest
            { rmGenerator = Nothing
            , rmSimulation = Just RPCSimulationDecl
                { rsdDependencies = []
                , rsdSchedule = defaultScheduleDecl { schedDeclIntervalTicks = 0 }
                }
            , rmOverlay = Just (RPCOverlayDecl "test.toposchema")
            }
          invalidPhase = invalidInterval
            { rmSimulation = Just RPCSimulationDecl
                { rsdDependencies = []
                , rsdSchedule = defaultScheduleDecl
                    { schedDeclIntervalTicks = 3
                    , schedDeclPhaseTicks = 3
                    }
                }
            }
      validateManifest invalidInterval `shouldSatisfy` any isInvalidSimulationScheduleField
      validateManifest invalidPhase `shouldSatisfy` any isInvalidSimulationScheduleField

    it "validates parameter defaults and numeric ranges before runtime use" $ do
      let assertParamError expectedDetail spec =
            map manifestErrorMessage (validateManifest baseManifest { rmParameters = [spec] })
              `shouldSatisfy` any (Text.isInfixOf expectedDetail)
          mkParam name ty range def = RPCParamSpec
            { rpsName = name
            , rpsLabel = name
            , rpsType = ty
            , rpsRange = range
            , rpsDefault = def
            , rpsTooltip = ""
            }
      assertParamError "default must be a boolean" $
        mkParam "bool_default" ParamBool Nothing (Number 0)
      assertParamError "default must be a numeric" $
        mkParam "float_default" ParamFloat Nothing (String "0.5")
      assertParamError "default must be an integral" $
        mkParam "int_default" ParamInt Nothing (Number 1.5)
      assertParamError "integer range bounds must be integral" $
        mkParam "int_range" ParamInt (Just (Number 0.5, Number 2)) (Number 1)
      assertParamError "default must be within" $
        mkParam "default_range" ParamFloat (Just (Number 0, Number 1)) (Number 2)
      assertParamError "range bounds must be numeric" $
        mkParam "non_numeric_range" ParamFloat (Just (String "low", Number 1)) (Number 0.5)
      assertParamError "range bounds must be strictly increasing" $
        mkParam "float_range" ParamFloat (Just (Number 2, Number 1)) (Number 1)
      assertParamError "range bounds must be strictly increasing" $
        mkParam "int_non_increasing" ParamInt (Just (Number 2, Number 2)) (Number 2)
      assertParamError "bool parameters must not declare" $
        mkParam "bool_range" ParamBool (Just (Number 0, Number 1)) (Bool True)
      case eitherDecode "1.0000000000000000000000000000000000001" of
        Right justAboveOne -> do
          let preciseRange = Just (Number 0, Number 1)
              preciseSpec = mkParam "precise" ParamFloat preciseRange (Number 0.5)
          validateRPCParamValue preciseSpec justAboveOne `shouldSatisfy` isLeft
          assertParamError "default must be within" $
            mkParam "precise_default" ParamFloat preciseRange justAboveOne
        Left err -> expectationFailure err

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
            { rmSimulation = Just RPCSimulationDecl
                { rsdDependencies = []
                , rsdSchedule = defaultScheduleDecl
                }
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
      let hs = Handshake currentProtocolVersion Nothing ["query"] Nothing
      Aeson.fromJSON (Aeson.toJSON hs) `shouldBe` Aeson.Success hs

    it "Handshake carries an auth challenge without using the endpoint name as a credential" $ do
      let hs = Handshake currentProtocolVersion Nothing ["query", "launch_auth"] (Just "nonce-1")
      Aeson.fromJSON (Aeson.toJSON hs) `shouldBe` Aeson.Success hs
      hsAuthChallenge hs `shouldBe` Just "nonce-1"

    it "HandshakeAck with no data directory round-trips" $ do
      let ha = HandshakeAck currentProtocolVersion Nothing [] Nothing Nothing
      Aeson.fromJSON (Aeson.toJSON ha) `shouldBe` Aeson.Success ha

    it "HandshakeAck carries session id and auth proof" $ do
      let proof = handshakeAuthProof "session-1" "token-1" "nonce-1"
          ha = HandshakeAck currentProtocolVersion Nothing [] (Just "session-1") (Just proof)
      Aeson.fromJSON (Aeson.toJSON ha) `shouldBe` Aeson.Success ha
      proof `shouldNotBe` "token-1"

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

    it "removes cancelled pending requests before later legacy responses" $
      withConnectedTransports "rpc-cancelled-health" $ \host plugin -> do
        let conn = newRPCConnection baseManifest host Map.empty
        cancelledDone <- newEmptyMVar
        worker <- forkIO $ do
          _ <- checkHealth conn `finally` putMVar cancelledDone ()
          pure ()
        firstReq <- recvEnvelopeFrom plugin
        envType firstReq `shouldBe` MsgHealthCheck
        killThread worker
        cancelled <- timeout transportTestTimeoutMicros (takeMVar cancelledDone)
        cancelled `shouldBe` Just ()
        secondDone <- newEmptyMVar
        _ <- forkIO (checkHealth conn >>= putMVar secondDone)
        secondReq <- recvEnvelopeFrom plugin
        envType secondReq `shouldBe` MsgHealthCheck
        sendEnvelopeTo plugin ((healthResponse secondReq "after-cancel") { envRequestId = Nothing })
        secondResult <- takeHealthResult secondDone
        hstMessage secondResult `shouldBe` "after-cancel"

    it "ignores uncorrelated external data-source operation results while an RPC is pending" $
      withConnectedTransports "rpc-external-operation-result-ignored" $ \host plugin -> do
        let conn = newRPCConnection baseManifest host Map.empty
        done <- newEmptyMVar
        _ <- forkIO (checkHealth conn >>= putMVar done)
        request <- recvEnvelopeFrom plugin
        envType request `shouldBe` MsgHealthCheck
        sendEnvelopeTo plugin RPCEnvelope
          { envType = MsgExternalDataSourceOperationResult
          , envPayload = Aeson.toJSON (RPCExternalDataSourceOperationResult
              { redsoOperationId = "ignored-op"
              , redsoOperationEpoch = Nothing
              , redsoOperation = ExternalDataSourceGrantOperation
              , redsoProviderId = "provider"
              , redsoConsumerId = "consumer"
              , redsoSource = "source"
              , redsoGrant = "grant"
              , redsoAccepted = True
              , redsoApplied = True
              , redsoStatus = "applied"
              , redsoMessage = Nothing
              , redsoError = Nothing
              , redsoDiagnostics = Nothing
              })
          , envRequestId = Nothing
          }
        premature <- timeout 200000 (takeMVar done)
        premature `shouldBe` Nothing
        sendEnvelopeTo plugin (healthResponse request "after-ignored-ack")
        health <- takeHealthResult done
        hstMessage health `shouldBe` "after-ignored-ack"

    it "waits for correlated external data-source grant ACKs" $
      withConnectedTransports "rpc-external-grant-ack" $ \host plugin -> do
        let conn = newRPCConnection baseManifest host Map.empty
        done <- newEmptyMVar
        _ <- forkIO (sendExternalDataSourceGrant conn externalGrantMessageFixture >>= putMVar done)
        request <- recvEnvelopeFrom plugin
        envType request `shouldBe` MsgExternalDataSourceGrant
        _ <- requireRequestId request
        sendEnvelopeTo plugin (externalOperationResultEnvelope request ExternalDataSourceGrantOperation True Nothing)
        result <- timeout transportTestTimeoutMicros (takeMVar done)
        case result of
          Just (Right operationResult) -> do
            redsoOperation operationResult `shouldBe` ExternalDataSourceGrantOperation
            redsoAccepted operationResult `shouldBe` True
            redsoApplied operationResult `shouldBe` True
            redsoStatus operationResult `shouldBe` "applied"
            redsoMessage operationResult `shouldBe` Just "operation applied"
          other -> expectationFailure ("expected successful external grant ACK result, got " <> show other)

    it "preserves external data-source grant rejection ACK details" $
      withConnectedTransports "rpc-external-grant-rejected" $ \host plugin -> do
        let conn = newRPCConnection baseManifest host Map.empty
        done <- newEmptyMVar
        _ <- forkIO (sendExternalDataSourceGrant conn externalGrantMessageFixture >>= putMVar done)
        request <- recvEnvelopeFrom plugin
        envType request `shouldBe` MsgExternalDataSourceGrant
        sendEnvelopeTo plugin (externalOperationResultEnvelope request ExternalDataSourceGrantOperation False (Just "consumer rejected grant"))
        result <- timeout transportTestTimeoutMicros (takeMVar done)
        case result of
          Just (Right operationResult) -> do
            redsoAccepted operationResult `shouldBe` False
            redsoApplied operationResult `shouldBe` False
            redsoStatus operationResult `shouldBe` "failed"
            redsoError operationResult `shouldBe` Just "consumer rejected grant"
          other -> expectationFailure ("expected rejected grant ACK result, got " <> show other)

    it "reports mismatched external data-source operation ACKs as protocol errors" $
      withConnectedTransports "rpc-external-revoke-protocol-error" $ \host plugin -> do
        let conn = newRPCConnection baseManifest host Map.empty
        done <- newEmptyMVar
        _ <- forkIO (sendExternalDataSourceGrantRevocation conn externalRevocationFixture >>= putMVar done)
        request <- recvEnvelopeFrom plugin
        envType request `shouldBe` MsgExternalDataSourceRevoke
        sendEnvelopeTo plugin (externalOperationResultEnvelope request ExternalDataSourceGrantOperation True Nothing)
        result <- timeout transportTestTimeoutMicros (takeMVar done)
        case result of
          Just (Left (RPCProtocolError msg)) -> msg `shouldSatisfy` Text.isInfixOf "operation mismatch"
          other -> expectationFailure ("expected external revoke protocol error, got " <> show other)

    it "times out external data-source grants without ACKs" $
      withConnectedTransports "rpc-external-grant-timeout" $ \host _plugin -> do
        let manifest = baseManifest
              { rmStartPolicy = defaultRPCStartPolicy { rspRequestTimeoutMs = 50 }
              }
            conn = newRPCConnection manifest host Map.empty
        result <- timeout 500000 (sendExternalDataSourceGrant conn externalGrantMessageFixture)
        case result of
          Just (Left (RPCTimeout msg)) -> msg `shouldSatisfy` Text.isInfixOf "external data-source grant"
          other -> expectationFailure ("expected external grant timeout, got " <> show other)

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

    it "accepts a plugin handshake with a matching launch session proof" $
      withConnectedTransports "rpc-handshake-auth-ok" $ \host plugin -> do
        let conn = newRPCConnection baseManifest host Map.empty
            auth = HandshakeAuthChallenge
              { hacSessionId = "session-ok"
              , hacChallenge = "nonce-ok"
              , hacExpectedProof = handshakeAuthProof "session-ok" "token-ok" "nonce-ok"
              }
        done <- newEmptyMVar
        _ <- forkIO (performHandshakeWithAuth conn Nothing (Just auth) >>= putMVar done)
        request <- recvEnvelopeFrom plugin
        envType request `shouldBe` MsgHandshake
        case Aeson.fromJSON (envPayload request) of
          Aeson.Error err -> expectationFailure err
          Aeson.Success (hs :: Handshake) -> do
            hsAuthChallenge hs `shouldBe` Just "nonce-ok"
            hsHostCapabilities hs `shouldSatisfy` elem "launch_auth"
        sendEnvelopeTo plugin RPCEnvelope
          { envType = MsgHandshakeAck
          , envPayload = Aeson.toJSON (HandshakeAck
              currentProtocolVersion
              Nothing
              []
              (Just "session-ok")
              (Just (handshakeAuthProof "session-ok" "token-ok" "nonce-ok")))
          , envRequestId = envRequestId request
          }
        result <- timeout transportTestTimeoutMicros (takeMVar done)
        case result of
          Just (Right conn') -> rpcProtocolVersion conn' `shouldBe` currentProtocolVersion
          _ -> expectationFailure "expected authenticated handshake success"

    it "rejects missing launch session proof during handshake" $
      withConnectedTransports "rpc-handshake-auth-missing" $ \host plugin -> do
        let conn = newRPCConnection baseManifest host Map.empty
            auth = HandshakeAuthChallenge
              { hacSessionId = "session-missing"
              , hacChallenge = "nonce-missing"
              , hacExpectedProof = handshakeAuthProof "session-missing" "token-missing" "nonce-missing"
              }
        done <- newEmptyMVar
        _ <- forkIO (performHandshakeWithAuth conn Nothing (Just auth) >>= putMVar done)
        request <- recvEnvelopeFrom plugin
        envType request `shouldBe` MsgHandshake
        sendEnvelopeTo plugin RPCEnvelope
          { envType = MsgHandshakeAck
          , envPayload = Aeson.toJSON (HandshakeAck currentProtocolVersion Nothing [] Nothing Nothing)
          , envRequestId = envRequestId request
          }
        result <- timeout transportTestTimeoutMicros (takeMVar done)
        case result of
          Just (Left (RPCProtocolError msg)) -> msg `shouldSatisfy` Text.isInfixOf "launch session"
          _ -> expectationFailure "expected missing launch session rejection"

    it "rejects mismatched launch session id during handshake" $
      withConnectedTransports "rpc-handshake-session-mismatch" $ \host plugin -> do
        let conn = newRPCConnection baseManifest host Map.empty
            auth = HandshakeAuthChallenge
              { hacSessionId = "session-expected"
              , hacChallenge = "nonce-session"
              , hacExpectedProof = handshakeAuthProof "session-expected" "token-session" "nonce-session"
              }
        done <- newEmptyMVar
        _ <- forkIO (performHandshakeWithAuth conn Nothing (Just auth) >>= putMVar done)
        request <- recvEnvelopeFrom plugin
        envType request `shouldBe` MsgHandshake
        sendEnvelopeTo plugin RPCEnvelope
          { envType = MsgHandshakeAck
          , envPayload = Aeson.toJSON (HandshakeAck
              currentProtocolVersion
              Nothing
              []
              (Just "session-wrong")
              (Just (handshakeAuthProof "session-wrong" "token-session" "nonce-session")))
          , envRequestId = envRequestId request
          }
        result <- timeout transportTestTimeoutMicros (takeMVar done)
        case result of
          Just (Left (RPCProtocolError msg)) -> msg `shouldSatisfy` Text.isInfixOf "launch session"
          _ -> expectationFailure "expected mismatched launch session rejection"

    it "rejects mismatched launch auth proof during handshake" $
      withConnectedTransports "rpc-handshake-auth-mismatch" $ \host plugin -> do
        let conn = newRPCConnection baseManifest host Map.empty
            auth = HandshakeAuthChallenge
              { hacSessionId = "session-proof"
              , hacChallenge = "nonce-proof"
              , hacExpectedProof = handshakeAuthProof "session-proof" "token-proof" "nonce-proof"
              }
        done <- newEmptyMVar
        _ <- forkIO (performHandshakeWithAuth conn Nothing (Just auth) >>= putMVar done)
        request <- recvEnvelopeFrom plugin
        envType request `shouldBe` MsgHandshake
        sendEnvelopeTo plugin RPCEnvelope
          { envType = MsgHandshakeAck
          , envPayload = Aeson.toJSON (HandshakeAck currentProtocolVersion Nothing [] (Just "session-proof") (Just "wrong-proof"))
          , envRequestId = envRequestId request
          }
        result <- timeout transportTestTimeoutMicros (takeMVar done)
        case result of
          Just (Left (RPCProtocolError msg)) -> msg `shouldSatisfy` Text.isInfixOf "auth proof"
          _ -> expectationFailure "expected mismatched auth proof rejection"

    it "accepts handshake data resources that stay within the manifest declaration" $ do
      let manifest = baseManifest
            { rmCapabilities = [CapDataRead, CapDataWrite]
            , rmDataResources = [handshakeDataResourceFixture]
            }
      result <- performTestHandshakeAck "rpc-handshake-data-resource-valid" manifest [handshakeDataResourceFixture]
      case result of
        Right conn' -> rpcResources conn' `shouldBe` [handshakeDataResourceFixture]
        Left err -> expectationFailure ("expected handshake success, got " <> show err)

    it "rejects handshake data resources when the manifest lacks dataRead" $ do
      let manifest = baseManifest
            { rmCapabilities = []
            , rmDataResources = [handshakeDataResourceFixture]
            }
      result <- performTestHandshakeAck "rpc-handshake-data-resource-no-read" manifest [handshakeDataResourceFixture]
      case result of
        Left (RPCProtocolError msg) -> msg `shouldSatisfy` Text.isInfixOf "dataRead"
        other -> expectationFailure ("expected dataRead handshake rejection, got " <> handshakeResultSummary other)

    it "rejects write-capable handshake data resources when the manifest lacks dataWrite" $ do
      let readOnlyManifestResource = handshakeDataResourceFixture
            { drsOperations = noOperations { doList = True, doGet = True, doPage = True }
            }
          manifest = baseManifest
            { rmCapabilities = [CapDataRead]
            , rmDataResources = [readOnlyManifestResource]
            }
      result <- performTestHandshakeAck "rpc-handshake-data-resource-no-write" manifest [handshakeDataResourceFixture]
      case result of
        Left (RPCProtocolError msg) -> do
          msg `shouldSatisfy` Text.isInfixOf "dataWrite"
          msg `shouldSatisfy` Text.isInfixOf "create"
        other -> expectationFailure ("expected dataWrite handshake rejection, got " <> handshakeResultSummary other)

    it "rejects handshake data resources not declared by the manifest" $ do
      let manifest = baseManifest
            { rmCapabilities = [CapDataRead]
            , rmDataResources = [handshakeDataResourceFixture]
            }
          unknownResource = handshakeDataResourceFixture
            { drsName = "unexpected_records"
            }
      result <- performTestHandshakeAck "rpc-handshake-data-resource-unknown" manifest [unknownResource]
      case result of
        Left (RPCProtocolError msg) -> msg `shouldSatisfy` Text.isInfixOf "declared by manifest"
        other -> expectationFailure ("expected unknown resource handshake rejection, got " <> handshakeResultSummary other)

    it "rejects handshake data-resource schema widening" $ do
      let manifestResource = handshakeDataResourceFixture
            { drsFields = [DataFieldDef "id" DFText "ID" False Nothing]
            , drsOperations = noOperations { doList = True, doPage = True }
            , drsPagination = defaultDataPagination { dpMaxPageSize = 50 }
            }
          widenedResource = handshakeDataResourceFixture
            { drsPagination = defaultDataPagination { dpMaxPageSize = 100 }
            }
          manifest = baseManifest
            { rmCapabilities = [CapDataRead, CapDataWrite]
            , rmDataResources = [manifestResource]
            }
      result <- performTestHandshakeAck "rpc-handshake-data-resource-widen" manifest [widenedResource]
      case result of
        Left (RPCProtocolError msg) -> do
          msg `shouldSatisfy` Text.isInfixOf "cannot add fields"
          msg `shouldSatisfy` Text.isInfixOf "create"
          msg `shouldSatisfy` Text.isInfixOf "maxPageSize"
        other -> expectationFailure ("expected schema widening handshake rejection, got " <> handshakeResultSummary other)

    it "rejects invalid or duplicate handshake data-resource schemas" $ do
      let invalidResource = handshakeDataResourceFixture
            { drsKeyField = "missing_id"
            }
          manifest = baseManifest
            { rmCapabilities = [CapDataRead, CapDataWrite]
            , rmDataResources = [handshakeDataResourceFixture]
            }
      invalidResult <- performTestHandshakeAck "rpc-handshake-data-resource-invalid" manifest [invalidResource]
      case invalidResult of
        Left (RPCProtocolError msg) -> msg `shouldSatisfy` Text.isInfixOf "keyField"
        other -> expectationFailure ("expected invalid schema handshake rejection, got " <> handshakeResultSummary other)
      let emptyFieldResource = handshakeDataResourceFixture
            { drsFields = DataFieldDef "" DFText "Empty" False Nothing : drsFields handshakeDataResourceFixture
            }
      emptyFieldResult <- performTestHandshakeAck "rpc-handshake-data-resource-empty-field" manifest [emptyFieldResource]
      case emptyFieldResult of
        Left (RPCProtocolError msg) -> msg `shouldSatisfy` Text.isInfixOf "field names must be non-empty"
        other -> expectationFailure ("expected empty field handshake rejection, got " <> handshakeResultSummary other)
      duplicateResult <- performTestHandshakeAck "rpc-handshake-data-resource-duplicate" manifest [handshakeDataResourceFixture, handshakeDataResourceFixture]
      case duplicateResult of
        Left (RPCProtocolError msg) -> msg `shouldSatisfy` Text.isInfixOf "duplicate"
        other -> expectationFailure ("expected duplicate resource handshake rejection, got " <> handshakeResultSummary other)

    it "sends the caller-provided seed and terrain in invoke_generator requests when readTerrain is declared" $
      withConnectedTransports "rpc-generator-seed" $ \host plugin -> do
        let explicitSeed = 0x123456789abcdef0 :: Word64
            terrainPayload = object ["marker" .= ("terrain" :: Text)]
            generatorResult = GeneratorResult
              { grTerrain = Null
              , grOverlay = Nothing
              , grMetadata = Just (object ["ok" .= True])
              }
            manifest = baseManifest { rmCapabilities = [CapReadTerrain] }
            conn = newRPCConnection manifest host Map.empty
        done <- newEmptyMVar
        _ <- forkIO (invokeGenerator conn explicitSeed terrainPayload >>= putMVar done)
        request <- recvEnvelopeFrom plugin
        envType request `shouldBe` MsgInvokeGenerator
        case Aeson.fromJSON (envPayload request) of
          Aeson.Error err -> expectationFailure ("failed to decode invoke_generator payload: " <> err)
          Aeson.Success invoke -> do
            igSeed invoke `shouldBe` explicitSeed
            igTerrain invoke `shouldBe` terrainPayload
        sendEnvelopeTo plugin RPCEnvelope
          { envType = MsgGeneratorResult
          , envPayload = Aeson.toJSON generatorResult
          , envRequestId = envRequestId request
          }
        result <- timeout transportTestTimeoutMicros (takeMVar done)
        result `shouldBe` Just (Right generatorResult)

    it "omits generator terrain input without readTerrain or readWorld capability" $
      withConnectedTransports "rpc-generator-no-terrain-read" $ \host plugin -> do
        let terrainPayload = object ["marker" .= ("terrain" :: Text)]
            generatorResult = GeneratorResult
              { grTerrain = Null
              , grOverlay = Nothing
              , grMetadata = Nothing
              }
            conn = newRPCConnection baseManifest host Map.empty
        done <- newEmptyMVar
        _ <- forkIO (invokeGenerator conn 99 terrainPayload >>= putMVar done)
        request <- recvEnvelopeFrom plugin
        envType request `shouldBe` MsgInvokeGenerator
        case Aeson.fromJSON (envPayload request) of
          Aeson.Error err -> expectationFailure ("failed to decode invoke_generator payload: " <> err)
          Aeson.Success invoke -> igTerrain invoke `shouldBe` Null
        sendEnvelopeTo plugin RPCEnvelope
          { envType = MsgGeneratorResult
          , envPayload = Aeson.toJSON generatorResult
          , envRequestId = envRequestId request
          }
        result <- timeout transportTestTimeoutMicros (takeMVar done)
        result `shouldBe` Just (Right generatorResult)

    it "does not require writeTerrain capability for generator terrain output" $
      withConnectedTransports "rpc-generator-implicit-terrain-write" $ \host plugin -> do
        terrainPayload <- nonEmptyGeneratorTerrainPayload
        let manifest = baseManifest { rmCapabilities = [CapReadTerrain] }
            conn = newRPCConnection manifest host Map.empty
            caps = pluginCaps [CapLog, CapReadTerrain]
            generatorResult = GeneratorResult
              { grTerrain = terrainPayload
              , grOverlay = Nothing
              , grMetadata = Nothing
              }
        done <- newEmptyMVar
        _ <- forkIO (runGeneratorStageWithCaps caps conn generatorStageWorld >>= putMVar done)
        request <- recvEnvelopeFrom plugin
        envType request `shouldBe` MsgInvokeGenerator
        sendGeneratorResult plugin request generatorResult
        (outcome, worldAfter) <- takeGeneratorStageResult done
        outcome `shouldBe` Right ()
        twTerrain worldAfter `shouldBe` twTerrain generatorTerrainWriteWorld

    it "rejects generator overlay output without a manifest overlay declaration even when a same-name overlay exists" $
      withConnectedTransports "rpc-generator-overlay-no-decl" $ \host plugin -> do
        let schema = overlaySchemaNamed "test-plugin"
            initialOverlay = emptyOverlay schema
            world = worldWithRegisteredOverlay initialOverlay
            conn = newRPCConnection baseManifest host Map.empty
            overlayPayload = overlayPayloadWithValue 7
            generatorResult = GeneratorResult
              { grTerrain = Null
              , grOverlay = Just overlayPayload
              , grMetadata = Nothing
              }
        done <- newEmptyMVar
        _ <- forkIO (runGeneratorStageWithCaps PluginCore.allowAllCapabilities conn world >>= putMVar done)
        request <- recvEnvelopeFrom plugin
        envType request `shouldBe` MsgInvokeGenerator
        sendGeneratorResult plugin request generatorResult
        (outcome, worldAfter) <- takeGeneratorStageResult done
        expectPluginInvariantContaining "overlay declaration" outcome
        lookupOverlay "test-plugin" (twOverlays worldAfter) `shouldBe` Just initialOverlay

    it "rejects generator overlay output without writeOverlay or writeWorld capability" $
      withConnectedTransports "rpc-generator-overlay-no-write" $ \host plugin -> do
        let schema = overlaySchemaNamed "test-plugin"
            initialOverlay = emptyOverlay schema
            world = worldWithRegisteredOverlay initialOverlay
            manifest = baseManifest
              { rmOverlay = Just (RPCOverlayDecl "test-plugin.toposchema")
              , rmCapabilities = []
              }
            conn = newRPCConnection manifest host Map.empty
            generatorResult = GeneratorResult
              { grTerrain = Null
              , grOverlay = Just (overlayPayloadWithValue 11)
              , grMetadata = Nothing
              }
        done <- newEmptyMVar
        _ <- forkIO (runGeneratorStageWithCaps PluginCore.allowAllCapabilities conn world >>= putMVar done)
        request <- recvEnvelopeFrom plugin
        envType request `shouldBe` MsgInvokeGenerator
        sendGeneratorResult plugin request generatorResult
        (outcome, _) <- takeGeneratorStageResult done
        expectPluginInvariantContaining "writeOverlay/writeWorld" outcome

    it "rejects generator overlay output without a registered host overlay" $
      withConnectedTransports "rpc-generator-overlay-no-host-overlay" $ \host plugin -> do
        let manifest = baseManifest
              { rmOverlay = Just (RPCOverlayDecl "test-plugin.toposchema")
              , rmCapabilities = [CapWriteOverlay]
              }
            conn = newRPCConnection manifest host Map.empty
            generatorResult = GeneratorResult
              { grTerrain = Null
              , grOverlay = Just (overlayPayloadWithValue 13)
              , grMetadata = Nothing
              }
        done <- newEmptyMVar
        _ <- forkIO (runGeneratorStageWithCaps PluginCore.allowAllCapabilities conn generatorStageWorld >>= putMVar done)
        request <- recvEnvelopeFrom plugin
        envType request `shouldBe` MsgInvokeGenerator
        sendGeneratorResult plugin request generatorResult
        (outcome, _) <- takeGeneratorStageResult done
        expectPluginInvariantContaining "registered overlay surface" outcome

    it "applies generator overlay output only with declaration, write capability, and registered host overlay" $
      withConnectedTransports "rpc-generator-overlay-allowed" $ \host plugin -> do
        let schema = overlaySchemaNamed "test-plugin"
            initialOverlay = emptyOverlay schema
            world = worldWithRegisteredOverlay initialOverlay
            manifest = baseManifest
              { rmOverlay = Just (RPCOverlayDecl "test-plugin.toposchema")
              , rmCapabilities = [CapWriteOverlay]
              }
            conn = newRPCConnection manifest host Map.empty
            overlayPayload = overlayPayloadWithValue 17
            generatorResult = GeneratorResult
              { grTerrain = Null
              , grOverlay = Just overlayPayload
              , grMetadata = Nothing
              }
        done <- newEmptyMVar
        _ <- forkIO (runGeneratorStageWithCaps PluginCore.allowAllCapabilities conn world >>= putMVar done)
        request <- recvEnvelopeFrom plugin
        envType request `shouldBe` MsgInvokeGenerator
        sendGeneratorResult plugin request generatorResult
        (outcome, worldAfter) <- takeGeneratorStageResult done
        outcome `shouldBe` Right ()
        fmap overlayToJSON (lookupOverlay "test-plugin" (twOverlays worldAfter)) `shouldBe` Just overlayPayload

    it "routes correlated simulation progress to the invokeSimulation callback and completes" $
      withConnectedTransports "rpc-simulation-progress" $ \host plugin -> do
        let conn = newRPCConnection simulationProgressManifest host Map.empty
        progressSeen <- newEmptyMVar
        done <- newEmptyMVar
        _ <- forkIO (invokeSimulation conn testSimContext testOverlay (putMVar progressSeen) (\_ -> pure ()) >>= putMVar done)
        request <- recvEnvelopeFrom plugin
        envType request `shouldBe` MsgInvokeSimulation
        requestId <- requireRequestId request
        sendEnvelopeTo plugin RPCEnvelope
          { envType = MsgProgress
          , envPayload = Aeson.toJSON (PluginProgress "halfway" 0.5)
          , envRequestId = Just requestId
          }
        observed <- timeout transportTestTimeoutMicros (takeMVar progressSeen)
        observed `shouldBe` Just (PluginProgress "halfway" 0.5)
        sendEnvelopeTo plugin RPCEnvelope
          { envType = MsgSimulationResult
          , envPayload = Aeson.toJSON simulationResultFixture
          , envRequestId = Just requestId
          }
        result <- timeout transportTestTimeoutMicros (takeMVar done)
        result `shouldBe` Just (Right simulationResultFixture)

    it "ignores progress with no matching pending request and still completes the final response" $
      withConnectedTransports "rpc-simulation-progress-wrong-id" $ \host plugin -> do
        let conn = newRPCConnection simulationProgressManifest host Map.empty
        progressSeen <- newEmptyMVar
        done <- newEmptyMVar
        _ <- forkIO (invokeSimulation conn testSimContext testOverlay (putMVar progressSeen) (\_ -> pure ()) >>= putMVar done)
        request <- recvEnvelopeFrom plugin
        envType request `shouldBe` MsgInvokeSimulation
        requestId <- requireRequestId request
        sendEnvelopeTo plugin RPCEnvelope
          { envType = MsgProgress
          , envPayload = Aeson.toJSON (PluginProgress "wrong" 0.9)
          , envRequestId = Just (requestId + 1000)
          }
        sendEnvelopeTo plugin RPCEnvelope
          { envType = MsgSimulationResult
          , envPayload = Aeson.toJSON simulationResultFixture
          , envRequestId = Just requestId
          }
        result <- timeout transportTestTimeoutMicros (takeMVar done)
        result `shouldBe` Just (Right simulationResultFixture)
        ignored <- timeout 200000 (takeMVar progressSeen)
        ignored `shouldBe` Nothing

    it "rejects simulation reader terrain_writes without writeTerrain or writeWorld" $
      withConnectedTransports "rpc-simulation-reader-terrain-write-reject" $ \host plugin -> do
        let conn = newRPCConnection simulationProgressManifest host Map.empty
        writesPayload <- nonEmptyTerrainWritesPayload
        done <- newEmptyMVar
        case rpcSimNode conn of
          SimNodeReader{snrReadTick = runTick} -> do
            _ <- forkIO (runTick testSimContext testOverlay >>= putMVar done)
            request <- recvEnvelopeFrom plugin
            envType request `shouldBe` MsgInvokeSimulation
            sendSimulationResult plugin request (simulationResultFixture
              { srOverlay = overlayPayloadWithValue 23
              , srTerrainWrites = Just writesPayload
              })
            result <- timeout transportTestTimeoutMicros (takeMVar done)
            case result of
              Just (Left msg) -> do
                msg `shouldSatisfy` Text.isInfixOf "unauthorized terrain write attempt"
                msg `shouldSatisfy` Text.isInfixOf "writeTerrain/writeWorld"
              _ -> expectationFailure "expected terrain write rejection"
          _ -> expectationFailure "expected SimNodeReader"

    it "rejects malformed simulation reader terrain_writes without writeTerrain or writeWorld" $
      withConnectedTransports "rpc-simulation-reader-terrain-write-malformed" $ \host plugin -> do
        let conn = newRPCConnection simulationProgressManifest host Map.empty
            malformedWrites = object
              [ "encoding" .= ("base64" :: Text)
              , "rivers" .= object ["0" .= ("AA==" :: Text)]
              ]
        done <- newEmptyMVar
        case rpcSimNode conn of
          SimNodeReader{snrReadTick = runTick} -> do
            _ <- forkIO (runTick testSimContext testOverlay >>= putMVar done)
            request <- recvEnvelopeFrom plugin
            envType request `shouldBe` MsgInvokeSimulation
            sendSimulationResult plugin request (simulationResultFixture
              { srOverlay = overlayPayloadWithValue 25
              , srTerrainWrites = Just malformedWrites
              })
            result <- timeout transportTestTimeoutMicros (takeMVar done)
            case result of
              Just (Left msg) -> do
                msg `shouldSatisfy` Text.isInfixOf "unauthorized terrain write attempt"
                msg `shouldSatisfy` Text.isInfixOf "unsupported keys"
              _ -> expectationFailure "expected malformed terrain write rejection"
          _ -> expectationFailure "expected SimNodeReader"

    it "allows harmless empty simulation reader terrain_writes summaries" $
      withConnectedTransports "rpc-simulation-reader-empty-terrain-writes" $ \host plugin -> do
        let conn = newRPCConnection simulationProgressManifest host Map.empty
            overlayPayload = overlayPayloadWithValue 29
            emptyWritesSummary = object ["chunk_count" .= (0 :: Int), "encoding" .= ("base64" :: Text)]
        done <- newEmptyMVar
        case rpcSimNode conn of
          SimNodeReader{snrReadTick = runTick} -> do
            _ <- forkIO (runTick testSimContext testOverlay >>= putMVar done)
            request <- recvEnvelopeFrom plugin
            envType request `shouldBe` MsgInvokeSimulation
            sendSimulationResult plugin request (simulationResultFixture
              { srOverlay = overlayPayload
              , srTerrainWrites = Just emptyWritesSummary
              })
            result <- timeout transportTestTimeoutMicros (takeMVar done)
            case result of
              Just (Right nextOverlay) -> overlayToJSON nextOverlay `shouldBe` overlayPayload
              _ -> expectationFailure "expected successful reader overlay update"
          _ -> expectationFailure "expected SimNodeReader"

    it "keeps simulation writer terrain_writes enabled with writeTerrain and writeWorld" $ do
      let runWriterCapability (terrainCapability, suffix) =
            withConnectedTransports
              ("rpc-simulation-writer-terrain-write-allowed-" <> suffix)
              $ \host plugin -> do
                let manifest = simulationProgressManifest
                      { rmCapabilities = [CapWriteOverlay, terrainCapability]
                      }
                    conn = newRPCConnection manifest host Map.empty
                writesPayload <- nonEmptyTerrainWritesPayload
                done <- newEmptyMVar
                case rpcSimNode conn of
                  SimNodeWriter{snwWriteTick = runTick} -> do
                    _ <- forkIO (runTick testSimContext testOverlay >>= putMVar done)
                    request <- recvEnvelopeFrom plugin
                    envType request `shouldBe` MsgInvokeSimulation
                    sendSimulationResult plugin request (simulationResultFixture
                      { srOverlay = overlayPayloadWithValue 31
                      , srTerrainWrites = Just writesPayload
                      })
                    result <- timeout transportTestTimeoutMicros (takeMVar done)
                    case result of
                      Just (Right (_, writes)) -> terrainWritesEmpty writes `shouldBe` False
                      _ -> expectationFailure "expected writer terrain writes"
                  _ -> expectationFailure "expected SimNodeWriter"
      mapM_ runWriterCapability
        [ (CapWriteTerrain, "writeTerrain" :: Text)
        , (CapWriteWorld, "writeWorld")
        ]

    it "intentionally consumes and ignores data-resource query progress" $
      withConnectedTransports "rpc-query-progress-ignored" $ \host plugin -> do
        let conn = newRPCConnection baseManifest host Map.empty
            requestPayload = QueryResource "records" QueryAll Nothing Nothing
            queryResult = QueryResult "records" [DataRecord (Map.singleton "id" (String "alpha"))] (Just 1)
        done <- newEmptyMVar
        _ <- forkIO (queryResource conn requestPayload >>= putMVar done)
        request <- recvEnvelopeFrom plugin
        envType request `shouldBe` MsgQueryResource
        requestId <- requireRequestId request
        sendEnvelopeTo plugin RPCEnvelope
          { envType = MsgProgress
          , envPayload = Aeson.toJSON (PluginProgress "query halfway" 0.5)
          , envRequestId = Just requestId
          }
        sendEnvelopeTo plugin RPCEnvelope
          { envType = MsgQueryResult
          , envPayload = Aeson.toJSON queryResult
          , envRequestId = Just requestId
          }
        result <- timeout transportTestTimeoutMicros (takeMVar done)
        result `shouldBe` Just (Right queryResult)

    it "intentionally consumes and ignores data-resource mutation progress" $
      withConnectedTransports "rpc-mutate-progress-ignored" $ \host plugin -> do
        let conn = newRPCConnection baseManifest host Map.empty
            recordPayload = DataRecord (Map.singleton "id" (String "alpha"))
            requestPayload = MutateResource "records" (MutCreate recordPayload)
            mutateResult = MutateResult True Nothing (Just recordPayload) Nothing
        done <- newEmptyMVar
        _ <- forkIO (mutateResource conn requestPayload >>= putMVar done)
        request <- recvEnvelopeFrom plugin
        envType request `shouldBe` MsgMutateResource
        requestId <- requireRequestId request
        sendEnvelopeTo plugin RPCEnvelope
          { envType = MsgProgress
          , envPayload = Aeson.toJSON (PluginProgress "mutation halfway" 0.5)
          , envRequestId = Just requestId
          }
        sendEnvelopeTo plugin RPCEnvelope
          { envType = MsgMutateResult
          , envPayload = Aeson.toJSON mutateResult
          , envRequestId = Just requestId
          }
        result <- timeout transportTestTimeoutMicros (takeMVar done)
        result `shouldBe` Just (Right mutateResult)

    it "rejects a data-resource query response with the wrong message type" $
      withConnectedTransports "rpc-query-wrong-response-type" $ \host plugin -> do
        let conn = newRPCConnection baseManifest host Map.empty
            requestPayload = QueryResource "records" QueryAll Nothing Nothing
            queryResult = QueryResult "records" [DataRecord (Map.singleton "id" (String "alpha"))] (Just 1)
        done <- newEmptyMVar
        _ <- forkIO (queryResource conn requestPayload >>= putMVar done)
        request <- recvEnvelopeFrom plugin
        envType request `shouldBe` MsgQueryResource
        requestId <- requireRequestId request
        sendEnvelopeTo plugin RPCEnvelope
          { envType = MsgMutateResult
          , envPayload = Aeson.toJSON queryResult
          , envRequestId = Just requestId
          }
        result <- timeout transportTestTimeoutMicros (takeMVar done)
        case result of
          Just (Left (RPCProtocolError msg)) -> do
            msg `shouldSatisfy` Text.isInfixOf "unexpected data query response"
            msg `shouldSatisfy` Text.isInfixOf "MsgMutateResult"
          other -> expectationFailure ("expected query response type rejection, got " <> show other)

    it "rejects a data-resource mutation response with the wrong message type" $
      withConnectedTransports "rpc-mutate-wrong-response-type" $ \host plugin -> do
        let conn = newRPCConnection baseManifest host Map.empty
            recordPayload = DataRecord (Map.singleton "id" (String "alpha"))
            requestPayload = MutateResource "records" (MutCreate recordPayload)
            mutateResult = MutateResult True Nothing (Just recordPayload) Nothing
        done <- newEmptyMVar
        _ <- forkIO (mutateResource conn requestPayload >>= putMVar done)
        request <- recvEnvelopeFrom plugin
        envType request `shouldBe` MsgMutateResource
        requestId <- requireRequestId request
        sendEnvelopeTo plugin RPCEnvelope
          { envType = MsgQueryResult
          , envPayload = Aeson.toJSON mutateResult
          , envRequestId = Just requestId
          }
        result <- timeout transportTestTimeoutMicros (takeMVar done)
        case result of
          Just (Left (RPCProtocolError msg)) -> do
            msg `shouldSatisfy` Text.isInfixOf "unexpected data mutation response"
            msg `shouldSatisfy` Text.isInfixOf "MsgQueryResult"
          other -> expectationFailure ("expected mutation response type rejection, got " <> show other)

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
            pipeNames = case break (== '|') address of
              (readPipe, '|':writePipe) -> [readPipe, writePipe]
              _ -> [address]
            suffixes = map (drop (length prefix)) pipeNames
        pipeNames `shouldSatisfy` all (prefix `isPrefixOf`)
        suffixes `shouldSatisfy` all (not . any (`elem` ['\\', '/', ':']))

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
      closeTransport plugin
      closeTransport host

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

performTestHandshakeAck :: Text -> RPCManifest -> [DataResourceSchema] -> IO (Either RPCError RPCConnection)
performTestHandshakeAck name manifest resources =
  withConnectedTransports name $ \host plugin -> do
    let conn = newRPCConnection manifest host Map.empty
        ack = HandshakeAck currentProtocolVersion Nothing resources Nothing Nothing
    done <- newEmptyMVar
    _ <- forkIO (performHandshakeWithAuth conn Nothing Nothing >>= putMVar done)
    request <- recvEnvelopeFrom plugin
    envType request `shouldBe` MsgHandshake
    sendEnvelopeTo plugin RPCEnvelope
      { envType = MsgHandshakeAck
      , envPayload = Aeson.toJSON ack
      , envRequestId = envRequestId request
      }
    result <- timeout transportTestTimeoutMicros (takeMVar done)
    case result of
      Nothing -> expectationFailure "timed out waiting for handshake result" >> fail "handshake result timeout"
      Just value -> pure value

handshakeResultSummary :: Either RPCError RPCConnection -> String
handshakeResultSummary (Left err) = "Left " <> show err
handshakeResultSummary (Right _) = "Right <connection>"

healthResponse :: RPCEnvelope -> Text -> RPCEnvelope
healthResponse request message = RPCEnvelope
  { envType = MsgHealthStatus
  , envPayload = Aeson.toJSON (HealthStatus
      { hstHealthy = True
      , hstMessage = message
      })
  , envRequestId = envRequestId request
  }

externalOperationResultEnvelope
  :: RPCEnvelope
  -> RPCExternalDataSourceOperation
  -> Bool
  -> Maybe Text
  -> RPCEnvelope
externalOperationResultEnvelope request operation accepted mError = RPCEnvelope
  { envType = MsgExternalDataSourceOperationResult
  , envPayload = Aeson.toJSON RPCExternalDataSourceOperationResult
      { redsoOperationId = operationId
      , redsoOperationEpoch = operationEpoch
      , redsoOperation = operation
      , redsoProviderId = "provider"
      , redsoConsumerId = "consumer"
      , redsoSource = "source"
      , redsoGrant = "grant"
      , redsoAccepted = accepted
      , redsoApplied = accepted
      , redsoStatus = if accepted then "applied" else "failed"
      , redsoMessage = if accepted then Just "operation applied" else Nothing
      , redsoError = mError
      , redsoDiagnostics = Nothing
      }
  , envRequestId = envRequestId request
  }
  where
    (operationId, operationEpoch) = case operation of
      ExternalDataSourceGrantOperation -> ("grant-op-test", Just 1)
      ExternalDataSourceRevokeOperation -> ("revoke-op-test", Just 2)

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

requireRequestId :: RPCEnvelope -> IO Word64
requireRequestId envelope =
  case envRequestId envelope of
    Nothing -> expectationFailure "expected correlated request id" >> fail "missing request id"
    Just requestId -> pure requestId

sendGeneratorResult :: Transport -> RPCEnvelope -> GeneratorResult -> IO ()
sendGeneratorResult transport request generatorResult =
  sendEnvelopeTo transport RPCEnvelope
    { envType = MsgGeneratorResult
    , envPayload = Aeson.toJSON generatorResult
    , envRequestId = envRequestId request
    }

sendSimulationResult :: Transport -> RPCEnvelope -> SimulationResult -> IO ()
sendSimulationResult transport request simulationResult =
  sendEnvelopeTo transport RPCEnvelope
    { envType = MsgSimulationResult
    , envPayload = Aeson.toJSON simulationResult
    , envRequestId = envRequestId request
    }

pluginCaps :: [Capability] -> PluginCore.PluginCapabilities
pluginCaps caps = PluginCore.PluginCapabilities (Set.fromList caps)

runGeneratorStageWithCaps
  :: PluginCore.PluginCapabilities
  -> RPCConnection
  -> TerrainWorld
  -> IO (Either PluginCore.PluginError (), TerrainWorld)
runGeneratorStageWithCaps caps conn world =
  PluginCore.runTopoM topoEnv world $
    PluginCore.runPluginM pluginEnv (stageRun (rpcGeneratorStage conn))
  where
    topoEnv = PluginCore.TopoEnv { PluginCore.teLogger = \_ -> pure () }
    pluginEnv = PluginCore.PluginEnv
      { PluginCore.peLogger = \_ -> pure ()
      , PluginCore.peProgress = \_ -> pure ()
      , PluginCore.peSeed = 123
      , PluginCore.peCaps = caps
      }

takeGeneratorStageResult
  :: MVar (Either PluginCore.PluginError (), TerrainWorld)
  -> IO (Either PluginCore.PluginError (), TerrainWorld)
takeGeneratorStageResult done = do
  result <- timeout transportTestTimeoutMicros (takeMVar done)
  case result of
    Nothing -> expectationFailure "timed out waiting for generator stage result" >> fail "generator stage timeout"
    Just outcome -> pure outcome

expectPluginInvariantContaining :: Text -> Either PluginCore.PluginError () -> Expectation
expectPluginInvariantContaining expected outcome =
  case outcome of
    Left (PluginCore.PluginInvariantError msg) -> do
      msg `shouldSatisfy` Text.isInfixOf "test-plugin"
      msg `shouldSatisfy` Text.isInfixOf expected
    other -> expectationFailure ("expected plugin invariant containing " <> Text.unpack expected <> ", got " <> show other)

generatorStageWorld :: TerrainWorld
generatorStageWorld = emptyWorld (WorldConfig { wcChunkSize = 64 }) defaultHexGridMeta

generatorTerrainWriteWorld :: TerrainWorld
generatorTerrainWriteWorld =
  setTerrainChunk (ChunkId 0) (emptyTerrainChunk config) generatorStageWorld
  where
    config = WorldConfig { wcChunkSize = 64 }

nonEmptyGeneratorTerrainPayload :: IO Value
nonEmptyGeneratorTerrainPayload =
  case terrainWorldToPayload generatorTerrainWriteWorld of
    Left err -> do
      expectationFailure ("failed to encode generator terrain payload: " <> Text.unpack err)
      fail "generator terrain payload"
    Right payload -> pure payload

nonEmptyTerrainWritesPayload :: IO Value
nonEmptyTerrainWritesPayload =
  case terrainWorldToPayload worldWithTerrainWrite of
    Left err -> do
      expectationFailure ("failed to encode terrain writes payload: " <> Text.unpack err)
      fail "terrain writes payload"
    Right payload -> pure payload
  where
    config = WorldConfig { wcChunkSize = 4 }
    worldWithTerrainWrite =
      setTerrainChunk (ChunkId 0) (emptyTerrainChunk config) (emptyWorld config defaultHexGridMeta)

worldWithRegisteredOverlay :: Overlay -> TerrainWorld
worldWithRegisteredOverlay overlay =
  generatorStageWorld { twOverlays = insertOverlay overlay (twOverlays generatorStageWorld) }

overlaySchemaNamed :: Text -> OverlaySchema
overlaySchemaNamed name = testOverlaySchema { osName = name }

overlayPayloadWithValue :: Double -> Value
overlayPayloadWithValue value = object
  [ "storage" .= ("sparse" :: Text)
  , "chunks" .=
      [ object
          [ "chunk_id" .= (0 :: Int)
          , "tiles" .=
              [ object
                  [ "tile" .= (0 :: Int)
                  , "fields" .= [value]
                  ]
              ]
          ]
      ]
  ]

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
  { rmManifestVersion = manifestV3
  , rmName         = "test-plugin"
  , rmVersion      = "0.1.0"
  , rmRuntime      = defaultRPCManifestRuntime
  , rmDescription  = ""
  , rmUiHints      = defaultRPCUIHints
  , rmGenerator    = Just (RPCGeneratorDecl "biomes" [])
  , rmSimulation   = Nothing
  , rmOverlay      = Nothing
  , rmCapabilities = []
  , rmParameters   = []
  , rmDataResources = []
  , rmDataDirectory = Nothing
  , rmExternalDataSources = []
  , rmExternalDataSourceRefs = []
  , rmStartPolicy   = defaultRPCStartPolicy
  }

handshakeDataResourceFixture :: DataResourceSchema
handshakeDataResourceFixture = DataResourceSchema
  { drsSchemaVersion = currentDataResourceSchemaVersion
  , drsResourceVersion = defaultDataResourceVersion
  , drsName = "records"
  , drsLabel = "Records"
  , drsHexBound = False
  , drsFields =
      [ DataFieldDef "id" DFText "ID" False Nothing
      , DataFieldDef "name" DFText "Name" False Nothing
      ]
  , drsOperations = noOperations
      { doList = True
      , doGet = True
      , doCreate = True
      , doPage = True
      }
  , drsKeyField = "id"
  , drsOverlay = Nothing
  , drsPagination = defaultDataPagination
  }

externalGrantMessageFixture :: RPCExternalDataSourceGrantMessage
externalGrantMessageFixture = RPCExternalDataSourceGrantMessage
  { redsgmOperationId = Just "grant-op-test"
  , redsgmOperationEpoch = Just 1
  , redsgmProviderId = "provider"
  , redsgmConsumerId = Just "consumer"
  , redsgmSource = "source"
  , redsgmGrant = "grant"
  , redsgmAccess = [ExternalAccessRead]
  , redsgmResources = ["records"]
  , redsgmCapabilityScope = [ExternalSourceQuery]
  , redsgmStatus = defaultRPCExternalDataSourceStatus { redssState = ExternalStatusReady }
  , redsgmReference = Nothing
  , redsgmConfigRefs = []
  , redsgmDiagnostics = Nothing
  }

externalRevocationFixture :: RPCExternalDataSourceGrantRevocation
externalRevocationFixture = RPCExternalDataSourceGrantRevocation
  { redsrvOperationId = Just "revoke-op-test"
  , redsrvOperationEpoch = Just 2
  , redsrvProviderId = "provider"
  , redsrvConsumerId = Just "consumer"
  , redsrvSource = "source"
  , redsrvGrant = "grant"
  , redsrvReason = Just "test revoke"
  , redsrvStatus = revokedExternalDataSourceStatus "provider" (Just "test revoke")
  , redsrvReference = Nothing
  , redsrvDiagnostics = Nothing
  }

simulationProgressManifest :: RPCManifest
simulationProgressManifest = baseManifest
  { rmName = "sim-progress"
  , rmGenerator = Nothing
  , rmSimulation = Just RPCSimulationDecl
      { rsdDependencies = []
      , rsdSchedule = defaultScheduleDecl
      }
  , rmOverlay = Just (RPCOverlayDecl "sim-progress.toposchema")
  , rmCapabilities = [CapWriteOverlay]
  }

testSimContext :: SimContext
testSimContext = SimContext
  { scTerrain = emptyWorld (WorldConfig { wcChunkSize = 64 }) defaultHexGridMeta
  , scCalendar = CalendarDate
      { cdYear = 0
      , cdDayOfYear = 0
      , cdHourOfDay = 0
      }
  , scWorldTime = WorldTime
      { wtTick = 0
      , wtTickRate = simulationTickSeconds
      }
  , scDeltaTicks = 1
  , scOverlays = Map.empty
  , scReportProgress = \_ -> pure ()
  }

testOverlay :: Overlay
testOverlay = emptyOverlay testOverlaySchema

testOverlaySchema :: OverlaySchema
testOverlaySchema = OverlaySchema
  { osName = "sim_progress"
  , osVersion = "1.0.0"
  , osDescription = "simulation progress routing test schema"
  , osFields =
      [ OverlayFieldDef
          { ofdName = "value"
          , ofdType = OFFloat
          , ofdDefault = Number 0
          , ofdIndexed = False
          , ofdRenamedFrom = Nothing
          }
      ]
  , osStorage = StorageSparse
  , osDependencies = emptyOverlayDeps
  , osFieldIndex = Map.fromList [("value", 0)]
  }

simulationResultFixture :: SimulationResult
simulationResultFixture = SimulationResult
  { srOverlay = object ["ok" .= True]
  , srTerrainWrites = Nothing
  }
