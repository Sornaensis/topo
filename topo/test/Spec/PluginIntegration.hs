{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | End-to-end integration tests for the plugin system.
--
-- Exercises the full library-level plugin lifecycle:
--
-- 1. Parse a manifest (simulating discovery)
-- 2. Create an RPC connection with mock transport
-- 3. Build a 'PipelineStage' from the connection
-- 4. Build a 'SimNode' from the connection
-- 5. Verify stage identity, sim node shape, and wiring
--
-- Also tests that the SDK's 'generateManifest' produces a manifest
-- that round-trips through the manifest parser.
module Spec.PluginIntegration (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (NonEmptyList(..))

import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, bracket, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (Value(..), (.=), object)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector.Unboxed as U
import Data.Word (Word64, Word8)
import System.IO (stdin, stdout)
import System.Timeout (timeout)

import Topo.Calendar (CalendarDate(..))
import Topo.Overlay (emptyOverlay, insertOverlay)
import Topo.Overlay.Schema (OverlaySchema(..), OverlayStorage(..), OverlayDeps(..))
import Topo.Pipeline (PipelineConfig(..), PipelineStage(..), defaultPipelineConfig, runPipeline)
import Topo.Pipeline.Stage (StageId(..))
import Topo.Hex (HexGridMeta(..), defaultHexGridMeta)
import Topo.Planet (PlanetConfig(..), WorldSlice(..), defaultPlanetConfig, defaultWorldSlice)
import Topo.Plugin.RPC
  ( RPCConnection(..)
  , RPCManifest(..)
  , RPCGeneratorDecl(..)
  , RPCSimulationDecl(..)
  , RPCOverlayDecl(..)
  , Capability(..)
  , GeneratorResult(..)
  , InvokeGenerator(..)
  , RPCCapability
  , RPCEnvelope(..)
  , RPCMessageType(..)
  , RPCParamSpec(..)
  , RPCParamType(..)
  , defaultRPCManifestRuntime
  , defaultRPCStartPolicy
  , defaultRPCUIHints
  , manifestV3
  , newRPCConnection
  , rpcGeneratorStage
  , rpcSimNode
  , parseManifest
  , validateManifest
  , manifestHasGenerator
  , manifestHasSimulation
  , manifestHasOverlay
  , manifestWritesTerrain
  , decodeTerrainWritesValue
  , applyGeneratorTerrainValue
  , terrainWorldToPayload
  , decodeBase64Text
  , encodeBase64Text
  , decodeMessage
  , encodeMessage
  )
import Topo.Plugin (PluginEnv(..), TopoEnv(..))
import Topo.Plugin.RPC.Transport
  ( Transport(..)
  , TransportConfig(..)
  , TransportServer(..)
  , closeTransport
  , connectPluginEndpoint
  , defaultTransportConfig
  , openPluginServer
  , recvMessage
  , sendMessage
  )
import Topo.Simulation
  ( SimContext(..), SimNode(..), SimNodeId(..)
  , SimulationCatchUpPolicy(..), SimulationScheduleDecl(..)
  , defaultScheduleDecl, simNodeSchedule, twrTerrain
  )
import Topo.Types (ChunkId(..), TerrainChunk, TileCoord(..), WorldConfig(..), tcElevation)
import Topo.Weather (weatherOverlaySchema)
import Topo.World
  ( TerrainWorld(..)
  , emptyClimateChunk
  , emptyVegetationChunk
  , emptyWorldWithPlanet
  , generateTerrainChunk
  , setClimateChunk
  , setTerrainChunk
  , setVegetationChunk
  )

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

-- | Create a mock transport (closed handles, for structural tests only).
-- Not suitable for actual message passing.
mockTransport :: Text -> IO Transport
mockTransport name = do
  -- Use stdin/stdout placeholders; structural tests don't perform
  -- transport I/O, but strict fields require defined handles.
  pure Transport
    { tReadHandle  = stdin
    , tWriteHandle = stdout
    , tPluginName  = name
    }

capturePipelineGeneratorSeed :: Word64 -> RPCManifest -> IO (Word64, Word64)
capturePipelineGeneratorSeed pipelineSeed manifest =
  withConnectedTransports ("pipeline-seed-" <> rmName manifest) $ \host plugin -> do
    controlSeedVar <- newEmptyMVar
    capturedRequest <- newEmptyMVar
    _ <- forkIO (serveGeneratorEchoOnce plugin capturedRequest)
    let conn = newRPCConnection manifest host Map.empty
        rpcStage = rpcGeneratorStage conn
        config = defaultPipelineConfig
          { pipelineSeed = pipelineSeed
          , pipelineStages =
              [ captureSeedStage (stageSeedTag rpcStage) controlSeedVar
              , rpcStage
              ]
          }
        env = TopoEnv { teLogger = \_ -> pure () }
        world = mkTestWorld (WorldConfig { wcChunkSize = 8 })
    pipelineResult <- timeout transportTestTimeoutMicros (runPipeline config env world)
    case pipelineResult of
      Nothing -> expectationFailure "pipeline run timed out waiting for generator RPC"
      Just (Left err) -> expectationFailure ("pipeline failed: " <> show err)
      Just (Right _) -> pure ()
    controlSeed <- takeMVarWithin "control stage seed" controlSeedVar
    request <- takeCapturedGeneratorRequest capturedRequest
    pure (controlSeed, igSeed request)

captureSeedStage :: Text -> MVar Word64 -> PipelineStage
captureSeedStage seedTag seedVar = PipelineStage
  { stageId = StageBaseHeight
  , stageName = "control seed"
  , stageSeedTag = seedTag
  , stageOverlayProduces = Nothing
  , stageOverlayReads = []
  , stageOverlaySchema = Nothing
  , stageRun = asks peSeed >>= liftIO . putMVar seedVar
  }

serveGeneratorEchoOnce :: Transport -> MVar (Either String InvokeGenerator) -> IO ()
serveGeneratorEchoOnce plugin captured = do
  outcome <- try receiveAndRespond :: IO (Either SomeException InvokeGenerator)
  putMVar captured $ case outcome of
    Left err -> Left (show err)
    Right request -> Right request
  where
    receiveAndRespond = do
      request <- recvEnvelopeFrom plugin
      case envType request of
        MsgInvokeGenerator -> pure ()
        other -> fail ("expected invoke_generator request, got " <> show other)
      invoke <- case Aeson.fromJSON (envPayload request) of
        Aeson.Error err -> fail ("failed to decode invoke_generator payload: " <> err)
        Aeson.Success decoded -> pure decoded
      sendEnvelopeTo plugin RPCEnvelope
        { envType = MsgGeneratorResult
        , envPayload = Aeson.toJSON GeneratorResult
            { grTerrain = igTerrain invoke
            , grOverlay = Nothing
            , grMetadata = Nothing
            }
        , envRequestId = envRequestId request
        }
      pure invoke

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

takeMVarWithin :: String -> MVar a -> IO a
takeMVarWithin label var = do
  result <- timeout transportTestTimeoutMicros (takeMVar var)
  case result of
    Nothing -> expectationFailure ("timed out waiting for " <> label) >> fail "mvar timeout"
    Just value -> pure value

takeCapturedGeneratorRequest :: MVar (Either String InvokeGenerator) -> IO InvokeGenerator
takeCapturedGeneratorRequest var = do
  result <- takeMVarWithin "captured generator request" var
  case result of
    Left err -> expectationFailure err >> fail "generator capture failed"
    Right request -> pure request

withTransportServer :: Text -> (TransportServer -> IO a) -> IO a
withTransportServer pluginName = bracket acquire tsClose
  where
    acquire = do
      serverResult <- openPluginServer defaultTransportConfig { tcTimeout = 1000 } pluginName
      case serverResult of
        Left err -> expectationFailure ("openPluginServer failed: " <> show err) >> fail "openPluginServer"
        Right server -> pure server

requireAccept :: TransportServer -> IO Transport
requireAccept server = do
  acceptResult <- tsAccept server
  case acceptResult of
    Left err -> expectationFailure ("accept failed: " <> show err) >> fail "accept failed"
    Right transport -> pure transport

transportTestTimeoutMicros :: Int
transportTestTimeoutMicros = 2000000

-- | A civilization-style manifest for integration tests.
civManifest :: RPCManifest
civManifest = RPCManifest
  { rmManifestVersion = manifestV3
  , rmName         = "civilization"
  , rmVersion      = "1.0.0"
  , rmRuntime      = defaultRPCManifestRuntime
  , rmDescription  = "Civilization simulation overlay"
  , rmUiHints      = defaultRPCUIHints
  , rmGenerator    = Just RPCGeneratorDecl
      { rgdInsertAfter = "biomes"
      , rgdRequires    = ["biomes", "rivers"]
      }
  , rmSimulation   = Just RPCSimulationDecl
      { rsdDependencies = ["weather"]
      , rsdSchedule = defaultScheduleDecl
      }
  , rmOverlay      = Just RPCOverlayDecl
      { rodSchemaFile = "civilization.toposchema"
      }
  , rmCapabilities   = [CapReadTerrain, CapReadOverlay, CapWriteOverlay, CapLog]
  , rmParameters     =
      [ RPCParamSpec
          { rpsName    = "growth_rate"
          , rpsLabel   = "Growth Rate"
          , rpsType    = ParamFloat
          , rpsRange   = Just (Aeson.Number 0.0, Aeson.Number 0.5)
          , rpsDefault = Aeson.Number 0.02
          , rpsTooltip = "Population growth fraction per tick"
          }
      ]
  , rmDataResources  = []
  , rmDataDirectory  = Nothing
  , rmExternalDataSources = []
  , rmExternalDataSourceRefs = []
  , rmStartPolicy    = defaultRPCStartPolicy
  }

-- | A generator-only manifest (no simulation, no overlay).
genOnlyManifest :: RPCManifest
genOnlyManifest = RPCManifest
  { rmManifestVersion = manifestV3
  , rmName         = "terrain-roughen"
  , rmVersion      = "0.1.0"
  , rmRuntime      = defaultRPCManifestRuntime
  , rmDescription  = "Roughen terrain elevation"
  , rmUiHints      = defaultRPCUIHints
  , rmGenerator    = Just RPCGeneratorDecl
      { rgdInsertAfter = "erosion"
      , rgdRequires    = ["erosion"]
      }
  , rmSimulation   = Nothing
  , rmOverlay      = Nothing
  , rmCapabilities   = [CapReadTerrain, CapLog]
  , rmParameters     = []
  , rmDataResources  = []
  , rmDataDirectory  = Nothing
  , rmExternalDataSources = []
  , rmExternalDataSourceRefs = []
  , rmStartPolicy    = defaultRPCStartPolicy
  }

-- | A sim-only manifest with writeTerrain capability.
writerManifest :: RPCManifest
writerManifest = RPCManifest
  { rmManifestVersion = manifestV3
  , rmName         = "terrain-writer"
  , rmVersion      = "0.1.0"
  , rmRuntime      = defaultRPCManifestRuntime
  , rmDescription  = "Writes terrain during simulation"
  , rmUiHints      = defaultRPCUIHints
  , rmGenerator    = Nothing
  , rmSimulation   = Just RPCSimulationDecl
      { rsdDependencies = []
      , rsdSchedule = defaultScheduleDecl
      }
  , rmOverlay      = Just RPCOverlayDecl
      { rodSchemaFile = "writer.toposchema"
      }
  , rmCapabilities   = [CapWriteTerrain, CapReadTerrain, CapWriteOverlay, CapLog]
  , rmParameters     = []
  , rmDataResources  = []
  , rmDataDirectory  = Nothing
  , rmExternalDataSources = []
  , rmExternalDataSourceRefs = []
  , rmStartPolicy    = defaultRPCStartPolicy
  }

------------------------------------------------------------------------
-- Tests
------------------------------------------------------------------------

spec :: Spec
spec = describe "Plugin Integration" $ do

  ------------------------------------
  -- Manifest → Connection → Stage
  ------------------------------------
  describe "Generator stage from manifest" $ do
    it "produces a PipelineStage with plugin StageId" $ do
      transport <- mockTransport "civilization"
      let conn = newRPCConnection civManifest transport Map.empty
          stage = rpcGeneratorStage conn
      stageId stage `shouldBe` StagePlugin "civilization"

    it "uses plugin name as stage name" $ do
      transport <- mockTransport "civilization"
      let conn = newRPCConnection civManifest transport Map.empty
          stage = rpcGeneratorStage conn
      stageName stage `shouldBe` "civilization"

    it "sets seed tag to plugin:<name>" $ do
      transport <- mockTransport "civilization"
      let conn = newRPCConnection civManifest transport Map.empty
          stage = rpcGeneratorStage conn
      stageSeedTag stage `shouldBe` "plugin:civilization"

    it "works for generator-only plugins" $ do
      transport <- mockTransport "terrain-roughen"
      let conn = newRPCConnection genOnlyManifest transport Map.empty
          stage = rpcGeneratorStage conn
      stageId stage `shouldBe` StagePlugin "terrain-roughen"

    it "passes the pipeline-derived stage seed to RPC generator invocations" $ do
      let pipelineSeed = 424242
      (controlSeed, rpcSeed) <- capturePipelineGeneratorSeed pipelineSeed civManifest
      (controlSeedAgain, rpcSeedAgain) <- capturePipelineGeneratorSeed pipelineSeed civManifest
      (changedControlSeed, changedRPCSeed) <- capturePipelineGeneratorSeed (pipelineSeed + 1) civManifest
      let renamedManifest = civManifest { rmName = "civilization-alt" }
      (renamedControlSeed, renamedRPCSeed) <- capturePipelineGeneratorSeed pipelineSeed renamedManifest

      rpcSeed `shouldBe` controlSeed
      rpcSeedAgain `shouldBe` controlSeedAgain
      rpcSeedAgain `shouldBe` rpcSeed
      rpcSeed `shouldNotBe` pipelineSeed
      changedRPCSeed `shouldBe` changedControlSeed
      changedRPCSeed `shouldNotBe` rpcSeed
      renamedRPCSeed `shouldBe` renamedControlSeed
      renamedRPCSeed `shouldNotBe` rpcSeed

  ------------------------------------
  -- Manifest → Connection → SimNode
  ------------------------------------
  describe "Simulation node from manifest" $ do
    it "creates a SimNodeReader for non-writer plugins" $ do
      transport <- mockTransport "civilization"
      let conn = newRPCConnection civManifest transport Map.empty
          node = rpcSimNode conn
      case node of
        SimNodeReader {} -> pure ()
        SimNodeWriter {} -> expectationFailure "expected SimNodeReader"

    it "creates a SimNodeWriter for writeTerrain plugins" $ do
      transport <- mockTransport "terrain-writer"
      let conn = newRPCConnection writerManifest transport Map.empty
          node = rpcSimNode conn
      case node of
        SimNodeWriter {} -> pure ()
        SimNodeReader {} -> expectationFailure "expected SimNodeWriter"

    it "creates a SimNodeWriter for coarse writeWorld capability" $ do
      transport <- mockTransport "terrain-writer"
      let coarseWriter = writerManifest
            { rmCapabilities = [CapWriteWorld, CapReadWorld, CapWriteOverlay, CapLog] }
          conn = newRPCConnection coarseWriter transport Map.empty
          node = rpcSimNode conn
      case node of
        SimNodeWriter {} -> pure ()
        SimNodeReader {} -> expectationFailure "expected SimNodeWriter"

    it "sets correct overlay name on sim node" $ do
      transport <- mockTransport "civilization"
      let conn = newRPCConnection civManifest transport Map.empty
          node = rpcSimNode conn
      case node of
        SimNodeReader { snrOverlayName = name } -> name `shouldBe` "civilization"
        SimNodeWriter { snwOverlayName = name } -> name `shouldBe` "civilization"

    it "sets correct SimNodeId" $ do
      transport <- mockTransport "civilization"
      let conn = newRPCConnection civManifest transport Map.empty
          node = rpcSimNode conn
      case node of
        SimNodeReader { snrId = nid } -> nid `shouldBe` SimNodeId "civilization"
        SimNodeWriter { snwId = nid } -> nid `shouldBe` SimNodeId "civilization"

    it "wires simulation dependencies from manifest" $ do
      transport <- mockTransport "civilization"
      let conn = newRPCConnection civManifest transport Map.empty
          node = rpcSimNode conn
      case node of
        SimNodeReader { snrDependencies = deps } ->
          deps `shouldBe` [SimNodeId "weather"]
        SimNodeWriter { snwDependencies = deps } ->
          deps `shouldBe` [SimNodeId "weather"]

    it "wires simulation schedule from manifest" $ do
      transport <- mockTransport "civilization"
      let schedule = SimulationScheduleDecl 6 2 SkipMissed
          manifest = civManifest
            { rmSimulation = Just RPCSimulationDecl
                { rsdDependencies = ["weather"]
                , rsdSchedule = schedule
                }
            }
          conn = newRPCConnection manifest transport Map.empty
      simNodeSchedule (rpcSimNode conn) `shouldBe` schedule

    it "rejects reader simulation tick without writeOverlay capability" $ do
      transport <- mockTransport "civilization"
      let manifest = civManifest { rmCapabilities = [CapReadTerrain, CapReadOverlay, CapLog] }
          conn = newRPCConnection manifest transport Map.empty
          node = rpcSimNode conn
          ctx = mkSimContext (mkTestWorld (WorldConfig { wcChunkSize = 8 }))
          overlay = emptyOverlay testOverlaySchema
      case node of
        SimNodeReader { snrReadTick = readTick } -> do
          result <- readTick ctx overlay
          result `shouldBe` Left "manifest missing writeOverlay capability"
        SimNodeWriter {} -> expectationFailure "expected SimNodeReader"

    it "rejects writer simulation tick without writeOverlay capability" $ do
      transport <- mockTransport "terrain-writer"
      let manifest = writerManifest { rmCapabilities = [CapWriteTerrain, CapReadTerrain, CapLog] }
          conn = newRPCConnection manifest transport Map.empty
          node = rpcSimNode conn
          ctx = mkSimContext (mkTestWorld (WorldConfig { wcChunkSize = 8 }))
          overlay = emptyOverlay writerOverlaySchema
      case node of
        SimNodeWriter { snwWriteTick = writeTick } -> do
          result <- writeTick ctx overlay
          case result of
            Left err -> err `shouldBe` "manifest missing writeOverlay capability"
            Right _ -> expectationFailure "expected capability rejection"
        SimNodeReader {} -> expectationFailure "expected SimNodeWriter"

  ------------------------------------
  -- Connection parameter threading
  ------------------------------------
  describe "Connection parameters" $ do
    it "stores parameters in the RPC connection" $ do
      transport <- mockTransport "civilization"
      let params = Map.fromList [("growth_rate", Aeson.Number 0.05)]
          conn = newRPCConnection civManifest transport params
      rpcParams conn `shouldBe` params

    it "manifests round-trips with parameters preserved" $ do
      let manifest = civManifest
      rmParameters manifest `shouldSatisfy` (not . null)
      case rmParameters manifest of
        (p:_) -> do
          rpsName p `shouldBe` "growth_rate"
          rpsType p `shouldBe` ParamFloat
        [] -> expectationFailure "expected at least one parameter"

  ------------------------------------
  -- Manifest validation in context
  ------------------------------------
  describe "Manifest validation in context" $ do
    it "validates civilization manifest cleanly" $
      validateManifest civManifest `shouldBe` []

    it "validates generator-only manifest cleanly" $
      validateManifest genOnlyManifest `shouldBe` []

    it "validates writer manifest cleanly" $
      validateManifest writerManifest `shouldBe` []

    it "detects writer capability correctly" $ do
      manifestWritesTerrain civManifest `shouldBe` False
      manifestWritesTerrain writerManifest `shouldBe` True

    it "detects participation correctly" $ do
      manifestHasGenerator civManifest `shouldBe` True
      manifestHasSimulation civManifest `shouldBe` True
      manifestHasOverlay civManifest `shouldBe` True
      manifestHasGenerator genOnlyManifest `shouldBe` True
      manifestHasSimulation genOnlyManifest `shouldBe` False
      manifestHasOverlay genOnlyManifest `shouldBe` False

  ------------------------------------
  -- Manifest JSON round-trip in context
  ------------------------------------
  describe "Manifest JSON round-trip" $ do
    it "civManifest survives encode/decode" $ do
      let encoded = BL.toStrict (Aeson.encode civManifest)
      case parseManifest encoded of
        Left err -> expectationFailure ("parse failed: " <> show err)
        Right parsed -> do
          rmName parsed `shouldBe` "civilization"
          rmVersion parsed `shouldBe` "1.0.0"
          manifestHasGenerator parsed `shouldBe` True
          manifestHasSimulation parsed `shouldBe` True
          manifestHasOverlay parsed `shouldBe` True
          length (rmParameters parsed) `shouldBe` 1

    it "genOnlyManifest survives encode/decode" $ do
      let encoded = BL.toStrict (Aeson.encode genOnlyManifest)
      case parseManifest encoded of
        Left err -> expectationFailure ("parse failed: " <> show err)
        Right parsed -> do
          rmName parsed `shouldBe` "terrain-roughen"
          manifestHasGenerator parsed `shouldBe` True
          manifestHasSimulation parsed `shouldBe` False
          validateManifest parsed `shouldBe` []

    it "writerManifest survives encode/decode" $ do
      let encoded = BL.toStrict (Aeson.encode writerManifest)
      case parseManifest encoded of
        Left err -> expectationFailure ("parse failed: " <> show err)
        Right parsed -> do
          rmName parsed `shouldBe` "terrain-writer"
          manifestWritesTerrain parsed `shouldBe` True
          manifestHasSimulation parsed `shouldBe` True
          validateManifest parsed `shouldBe` []

  ------------------------------------
  -- Data flow (real transport)
  ------------------------------------
  describe "RPC data flow" $ do
    prop "property: decodeBase64Text (encodeBase64Text bs) == Right bs" $ \(ws :: [Word8]) ->
      let bytes = BS.pack ws
      in decodeBase64Text (encodeBase64Text bytes) == Right bytes

    it "decodes non-empty simulation terrain_writes into TerrainWrites" $ do
      let config = WorldConfig { wcChunkSize = 8 }
          updatedChunk = generateTerrainChunk config (const 0.77)
      terrainWritesPayload <-
        case encodeTerrainWritesPayload config [(0, updatedChunk)] of
          Left err -> expectationFailure err >> fail "encode failed"
          Right payload -> pure payload
      writes <- case decodeTerrainWritesValue (Just terrainWritesPayload) of
        Left err -> expectationFailure (show err) >> fail "decode failed"
        Right decoded -> pure decoded
      IntMap.size (twrTerrain writes) `shouldBe` 1

    it "applies non-empty generator terrain payload to world" $ do
      let config = WorldConfig { wcChunkSize = 8 }
          originalWorld = mkTestWorld config
          updatedChunk = generateTerrainChunk config (const 0.93)
      terrainPayload <-
        case encodeTerrainWritesPayload config [(0, updatedChunk)] of
          Left err -> expectationFailure err >> fail "encode failed"
          Right payload -> pure payload
      mergedWorld <- case applyGeneratorTerrainValue originalWorld terrainPayload of
        Left err -> expectationFailure (show err) >> fail "apply failed"
        Right world -> pure world
      case IntMap.lookup 0 (twTerrain mergedWorld) of
        Nothing -> expectationFailure "expected merged terrain chunk"
        Just mergedChunk ->
          U.head (tcElevation mergedChunk) `shouldBe` 0.93

    prop "property: terrain payload round-trips through terrainWorldToPayload/applyGeneratorTerrainValue"
      $ \(NonEmpty values0 :: NonEmptyList Float) ->
          let values = take 64 values0
              config = WorldConfig { wcChunkSize = 8 }
              originalWorld = mkTestWorld config
              updatedChunk = generateTerrainChunk config (\tile ->
                let TileCoord x y = tile
                    index = (x * 31 + y * 17) `mod` length values
                in values !! index)
              payloadResult = encodeTerrainWritesPayload config [(0, updatedChunk)]
          in case payloadResult of
              Left _ -> False
              Right terrainPayload ->
                case applyGeneratorTerrainValue originalWorld terrainPayload of
                  Left _ -> False
                  Right mergedWorld ->
                    case IntMap.lookup 0 (twTerrain mergedWorld) of
                      Nothing -> False
                      Just mergedChunk ->
                        tcElevation mergedChunk == tcElevation updatedChunk

    it "keeps capability-scoped terrain payload separate from overlays and weather" $ do
      let config = WorldConfig { wcChunkSize = 8 }
          world = (mkTestWorld config)
            { twOverlays = insertOverlay (emptyOverlay weatherOverlaySchema) (twOverlays (mkTestWorld config)) }
      payload <- case terrainWorldToPayload world of
        Left err -> expectationFailure (show err) >> fail "payload failed"
        Right value -> pure value
      case payload of
        Object obj -> do
          KM.lookup "overlays" obj `shouldBe` Nothing
          KM.lookup "weather" obj `shouldBe` Nothing
        _ -> expectationFailure "expected terrain payload object"

    prop "property: terrain payload round-trips terrain/climate/vegetation chunk sections"
      $ \(NonEmpty rawIds0 :: NonEmptyList Int) ->
          let config = WorldConfig { wcChunkSize = 8 }
              dedupeInts = foldl' addUnique []
              addUnique acc n
                | n `elem` acc = acc
                | otherwise = acc <> [n]
              addChunk world cidInt =
                let cid = ChunkId cidInt
                    terrain = generateTerrainChunk config (const (fromIntegral cidInt / 100.0))
                    climate = emptyClimateChunk config
                    vegetation = emptyVegetationChunk config
                in setVegetationChunk cid vegetation (setClimateChunk cid climate (setTerrainChunk cid terrain world))
              rawIds = take 8 rawIds0
              chunkIds = take 4 (dedupeInts (map (\n -> abs n `mod` 16) rawIds))
              buildWorld = foldl' addChunk (emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig defaultWorldSlice) chunkIds
              payloadResult = terrainWorldToPayload buildWorld
          in case payloadResult of
              Left _ -> False
              Right payload ->
                case applyGeneratorTerrainValue (mkTestWorld config) payload of
                  Left _ -> False
                  Right mergedWorld ->
                    all (\cid -> IntMap.member cid (twTerrain mergedWorld)) chunkIds
                    && all (\cid -> IntMap.member cid (twClimate mergedWorld)) chunkIds
                    && all (\cid -> IntMap.member cid (twVegetation mergedWorld)) chunkIds

    it "preserves hex grid and geographic metadata through terrain payload round-trip" $ do
      let config = WorldConfig { wcChunkSize = 8 }
          hexMeta = HexGridMeta { hexSizeKm = 11.0 }
          planet = defaultPlanetConfig
            { pcRadius = 7000.0
            , pcAxialTilt = 15.0
            , pcInsolation = 0.9
            }
          slice = defaultWorldSlice
            { wsLatCenter = 12.5
            , wsLatExtent = 24.0
            , wsLonCenter = -45.0
            , wsLonExtent = 80.0
            }
          payloadResult = terrainWorldToPayload (emptyWorldWithPlanet config hexMeta planet slice)
      payload <- case payloadResult of
        Left err -> expectationFailure (Text.unpack err) >> fail "encode failed"
        Right terrainPayload -> pure terrainPayload
      mergedWorld <- case applyGeneratorTerrainValue (mkTestWorld config) payload of
        Left err -> expectationFailure (show err) >> fail "apply failed"
        Right world -> pure world
      twConfig mergedWorld `shouldBe` config
      twHexGrid mergedWorld `shouldBe` hexMeta
      twPlanet mergedWorld `shouldBe` planet
      twSlice mergedWorld `shouldBe` slice

    prop "property: terrain_writes payload decode preserves terrain chunk count"
      $ \(NonEmpty chunkValues0 :: NonEmptyList Float) ->
          let values = take 16 chunkValues0
              config = WorldConfig { wcChunkSize = 8 }
              mkChunk v = generateTerrainChunk config (const v)
              chunks = zip [0 ..] (map mkChunk values)
              payloadResult = encodeTerrainWritesPayload config chunks
          in case payloadResult of
              Left _ -> False
              Right payload ->
                case decodeTerrainWritesValue (Just payload) of
                  Left _ -> False
                  Right writes ->
                    IntMap.size (twrTerrain writes) == length chunks

mkTestWorld :: WorldConfig -> TerrainWorld
mkTestWorld config =
  let terrainChunk = generateTerrainChunk config (const 0.21)
      climateChunk = emptyClimateChunk config
      baseWorld = emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig defaultWorldSlice
  in setClimateChunk (ChunkId 0) climateChunk (setTerrainChunk (ChunkId 0) terrainChunk baseWorld)

encodeTerrainWritesPayload
  :: WorldConfig
  -> [(Int, TerrainChunk)]
  -> Either String Value
encodeTerrainWritesPayload config chunks =
  case terrainWorldToPayload payloadWorld of
    Left err -> Left (Text.unpack err)
    Right payload -> Right payload
  where
    terrainMap = IntMap.fromList chunks
    payloadWorld =
      (emptyWorldWithPlanet config defaultHexGridMeta defaultPlanetConfig defaultWorldSlice)
        { twTerrain = terrainMap
        }

mkSimContext :: TerrainWorld -> SimContext
mkSimContext world = SimContext
  { scTerrain = world
  , scCalendar = CalendarDate { cdYear = 0, cdDayOfYear = 0, cdHourOfDay = 0 }
  , scWorldTime = twWorldTime world
  , scDeltaTicks = 1
  , scOverlays = Map.empty
  }

testOverlaySchema :: OverlaySchema
testOverlaySchema = OverlaySchema
  { osName = "civilization"
  , osVersion = "1.0.0"
  , osDescription = ""
  , osFields = []
  , osStorage = StorageSparse
  , osDependencies = OverlayDeps True []
  , osFieldIndex = Map.empty
  }

writerOverlaySchema :: OverlaySchema
writerOverlaySchema = testOverlaySchema { osName = "terrain-writer" }
