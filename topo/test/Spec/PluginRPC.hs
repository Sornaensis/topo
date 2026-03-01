{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spec.PluginRPC (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

import Data.Aeson (Value(..), (.=), object, encode, eitherDecode)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Either (isLeft, isRight)
import Data.List (isInfixOf)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word64)

import Topo.Plugin.RPC.Manifest
import Topo.Plugin.RPC.Protocol
import Topo.Plugin.RPC.Transport (pluginPipeName, defaultTransportConfig)

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
    ]

instance Arbitrary RPCEnvelope where
  arbitrary = RPCEnvelope <$> arbitrary <*> pure (object [])

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
    [ CapReadTerrain
    , CapReadOverlay
    , CapWriteOverlay
    , CapWriteTerrain
    , CapLog
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
    pure RPCManifest
      { rmName         = name
      , rmVersion      = ver
      , rmDescription  = desc
      , rmGenerator    = gen
      , rmSimulation   = sim
      , rmOverlay      = ov'
      , rmCapabilities = caps
      , rmParameters   = params
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
      let env = RPCEnvelope MsgProgress (Aeson.toJSON (PluginProgress "working" 0.5))
          bs  = encodeMessage env
      decodeMessage bs `shouldBe` Right env

    it "rejects invalid bytes" $
      decodeMessage "not json" `shouldSatisfy` isLeft

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
  }
