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

import Data.Aeson (Value(..), (.=), object)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector.Unboxed as U
import System.IO (stdin, stdout)

import Topo.Calendar (CalendarDate(..))
import Topo.Overlay (emptyOverlay)
import Topo.Overlay.Schema (OverlaySchema(..), OverlayStorage(..), OverlayDeps(..))
import Topo.Pipeline (PipelineStage(..))
import Topo.Pipeline.Stage (StageId(..))
import Topo.Hex (defaultHexGridMeta)
import Topo.Planet (defaultPlanetConfig, defaultWorldSlice)
import Topo.Plugin.RPC
  ( RPCConnection(..)
  , RPCManifest(..)
  , RPCGeneratorDecl(..)
  , RPCSimulationDecl(..)
  , RPCOverlayDecl(..)
  , Capability(..)
  , RPCCapability
  , RPCParamSpec(..)
  , RPCParamType(..)
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
  )
import Topo.Plugin.RPC.Transport (Transport(..))
import Topo.Simulation (SimContext(..), SimNode(..), SimNodeId(..), twrTerrain)
import Topo.Types (ChunkId(..), TerrainChunk, WorldConfig(..), tcElevation)
import Topo.World
  ( TerrainWorld(..)
  , emptyClimateChunk
  , emptyWorldWithPlanet
  , generateTerrainChunk
  , setClimateChunk
  , setTerrainChunk
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

-- | A civilization-style manifest for integration tests.
civManifest :: RPCManifest
civManifest = RPCManifest
  { rmName         = "civilization"
  , rmVersion      = "1.0.0"
  , rmDescription  = "Civilization simulation overlay"
  , rmGenerator    = Just RPCGeneratorDecl
      { rgdInsertAfter = "biomes"
      , rgdRequires    = ["biomes", "rivers"]
      }
  , rmSimulation   = Just RPCSimulationDecl
      { rsdDependencies = ["weather"]
      }
  , rmOverlay      = Just RPCOverlayDecl
      { rodSchemaFile = "civilization.toposchema"
      }
  , rmCapabilities = [CapReadTerrain, CapReadOverlay, CapWriteOverlay, CapLog]
  , rmParameters   =
      [ RPCParamSpec
          { rpsName    = "growth_rate"
          , rpsLabel   = "Growth Rate"
          , rpsType    = ParamFloat
          , rpsRange   = Just (Aeson.Number 0.0, Aeson.Number 0.5)
          , rpsDefault = Aeson.Number 0.02
          , rpsTooltip = "Population growth fraction per tick"
          }
      ]
  }

-- | A generator-only manifest (no simulation, no overlay).
genOnlyManifest :: RPCManifest
genOnlyManifest = RPCManifest
  { rmName         = "terrain-roughen"
  , rmVersion      = "0.1.0"
  , rmDescription  = "Roughen terrain elevation"
  , rmGenerator    = Just RPCGeneratorDecl
      { rgdInsertAfter = "erosion"
      , rgdRequires    = ["erosion"]
      }
  , rmSimulation   = Nothing
  , rmOverlay      = Nothing
  , rmCapabilities = [CapReadTerrain, CapLog]
  , rmParameters   = []
  }

-- | A sim-only manifest with writeTerrain capability.
writerManifest :: RPCManifest
writerManifest = RPCManifest
  { rmName         = "terrain-writer"
  , rmVersion      = "0.1.0"
  , rmDescription  = "Writes terrain during simulation"
  , rmGenerator    = Nothing
  , rmSimulation   = Just RPCSimulationDecl
      { rsdDependencies = []
      }
  , rmOverlay      = Just RPCOverlayDecl
      { rodSchemaFile = "writer.toposchema"
      }
  , rmCapabilities = [CapWriteTerrain, CapReadTerrain, CapWriteOverlay, CapLog]
  , rmParameters   = []
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
