{-# LANGUAGE OverloadedStrings #-}

module Spec.Payload (spec) where

import Data.Aeson (Value(..), (.=), object)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import qualified Data.IntSet as IntSet
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Test.Hspec

import Topo.Hex (HexGridMeta(..), defaultHexGridMeta)
import Topo.Overlay (Overlay, emptyOverlay, overlayName)
import Topo.Overlay.Schema
  ( OverlaySchema(..)
  , OverlayStorage(..)
  , OverlayFieldDef(..)
  , OverlayFieldType(..)
  , emptyOverlayDeps
  )
import Topo.Export (encodeTerrainChunk)
import Topo.Planet (PlanetConfig(..), WorldSlice(..), defaultPlanetConfig, defaultWorldSlice)
import Topo.Plugin.RPC.Scope
  ( RPCInvocationKind(..), RPCScopeBudgets(..), ResolvedInvocationScope(..)
  , TerrainSection(..)
  )
import Topo.Plugin.RPC.Transport (mkRPCPayloadLimits)
import Topo.Plugin.SDK.Payload
import Topo.Plugin.SDK.Types
import Topo.Simulation (TerrainWrites, emptyTerrainWrites)
import Topo.Types (ChunkId(..), WorldConfig(..))
import Topo.World
  ( TerrainWorld(..), emptyTerrainChunk, emptyWorld, emptyWorldWithPlanet, setTerrainChunk )

spec :: Spec
spec = describe "SDK payload helpers" $ do
  it "decodes own overlay payload using schema" $ do
    let overlay = emptyOverlay testSchema
        context = testContext { pcOwnOverlay = Just (encodeOverlayPayload overlay) }
    case decodeOwnOverlay testSchema context of
      Left err -> expectationFailure (show err)
      Right decoded -> overlayName decoded `shouldBe` overlayName overlay

  it "decodes dependency overlay payload by name" $ do
    let overlay = emptyOverlay testSchema
        context = testContext
          { pcOverlays = Map.fromList [("weather", encodeOverlayPayload overlay)]
          }
    case decodeDependencyOverlay testSchema "weather" context of
      Left err -> expectationFailure (show err)
      Right decoded -> overlayName decoded `shouldBe` "test_overlay"

  it "round-trips terrain payload encode/decode" $ do
    let world = emptyWorldWithPlanet
          (WorldConfig { wcChunkSize = 64 })
          (HexGridMeta { hexSizeKm = 11.0 })
          (defaultPlanetConfig { pcRadius = 7000.0, pcAxialTilt = 15.0, pcInsolation = 0.9 })
          (defaultWorldSlice { wsLatCenter = 12.5, wsLatExtent = 24.0, wsLonCenter = -45.0, wsLonExtent = 80.0 })
    case encodeTerrainPayload world >>= decodeTerrainPayload of
      Left err -> expectationFailure (show err)
      Right decoded -> do
        twConfig decoded `shouldBe` WorldConfig { wcChunkSize = 64 }
        twHexGrid decoded `shouldBe` twHexGrid world
        twPlanet decoded `shouldBe` twPlanet world
        twSlice decoded `shouldBe` twSlice world

  it "applies explicit terrain budgets to all SDK result constructors" $ do
    let config = WorldConfig { wcChunkSize = 1 }
        world = setTerrainChunk (ChunkId 0) (emptyTerrainChunk config)
          (emptyWorld config defaultHexGridMeta)
        overlay = emptyOverlay testSchema
    lowLimits <- case mkRPCPayloadLimits 100 of
      Left err -> expectationFailure (show err) >> fail "limits"
      Right value -> pure value
    exactLimits <- case mkRPCPayloadLimits 156 of
      Left err -> expectationFailure (show err) >> fail "limits"
      Right value -> pure value
    zeroTerrainLimits <- case mkRPCPayloadLimits 1 of
      Left err -> expectationFailure (show err) >> fail "limits"
      Right value -> pure value
    encodeTerrainPayloadWithLimits lowLimits world
      `shouldSatisfy` either (\err -> showsContains err "decoded aggregate exceeds limit") (const False)
    generatorResultFromTerrainWithLimits exactLimits world
      `shouldSatisfy` either (const False) (const True)
    generatorResultFromTerrainAndOverlayWithLimits exactLimits world overlay
      `shouldSatisfy` either (const False) (const True)
    encodeTerrainWritesPayloadWithLimits zeroTerrainLimits memptyTerrainWrites
      `shouldSatisfy` either (const False) isObject
    simulationResultWithTerrainWritesWithLimits zeroTerrainLimits overlay memptyTerrainWrites
      `shouldSatisfy` either (const False) (maybe False isObject . strTerrainWrites)

  it "rejects duplicate streamed snapshot keys before exposing a source" $ do
    let config = WorldConfig { wcChunkSize = 1 }
        header = TerrainSnapshotHeader 1 defaultHexGridMeta defaultPlanetConfig defaultWorldSlice
    raw <- case encodeTerrainChunk config (emptyTerrainChunk config) of
      Left err -> expectationFailure (show err) >> fail "encode"
      Right value -> pure value
    let record = TerrainChunkRecord TerrainElevation 0 raw
    case terrainChunkSourceFromRecords header [record, record] of
      Left _ -> pure ()
      Right _ -> expectationFailure "duplicate snapshot keys were accepted"

  it "emits deterministic baseline-aware removals through the delta sink" $ do
    let config = WorldConfig { wcChunkSize = 1 }
        header = TerrainSnapshotHeader 1 defaultHexGridMeta defaultPlanetConfig defaultWorldSlice
        scope = ResolvedInvocationScope
          { risScopeId = "payload-diff"
          , risKind = InvocationGenerator
          , risTerrainInputSections = Set.singleton TerrainElevation
          , risTerrainInputChunkIds = IntSet.singleton 0
          , risDependencyOverlayChunkIds = Map.empty
          , risOwnOverlayReadChunkIds = IntSet.empty
          , risTerrainOutputSections = Set.singleton TerrainElevation
          , risTerrainOutputChunkIds = IntSet.singleton 0
          , risOwnedOverlayIdentity = Nothing
          , risOwnOverlayWriteChunkIds = IntSet.empty
          , risGeneratorMetadataOutput = False
          , risDataResource = Nothing
          , risBudgets = RPCScopeBudgets 100000 100000 100000
          }
    raw <- case encodeTerrainChunk config (emptyTerrainChunk config) of
      Left err -> expectationFailure (show err) >> fail "encode"
      Right value -> pure value
    source <- case terrainChunkSourceFromRecords header
        [TerrainChunkRecord TerrainElevation 0 raw] of
      Left err -> expectationFailure (show err) >> fail "source"
      Right value -> pure value
    changes <- newIORef ([] :: [Either TerrainChunkRemoval TerrainChunkUpdate])
    let sink = TerrainDeltaSink
          { tdsWriteChunk = \update -> modifyIORef' changes (<> [Right update]) >> pure (Right ())
          , tdsRemoveChunk = \removal -> modifyIORef' changes (<> [Left removal]) >> pure (Right ())
          }
        world = emptyWorld config defaultHexGridMeta
    diffTerrainWorldAgainstSnapshot scope source world sink `shouldReturn` Right ()
    readIORef changes `shouldReturn` [Left (TerrainChunkRemoval TerrainElevation 0)]

  it "builds simulation result with encoded writes" $ do
    let overlay = emptyOverlay testSchema
    case simulationResultWithTerrainWrites overlay memptyTerrainWrites of
      Left err -> expectationFailure (show err)
      Right result -> do
        strOverlay result `shouldBe` encodeOverlayPayload overlay
        strTerrainWrites result `shouldSatisfy` maybe False isObject

showsContains :: Show a => a -> String -> Bool
showsContains actual expected = expected `isInfixOf` show actual

isObject :: Value -> Bool
isObject (Object _) = True
isObject _ = False

memptyTerrainWrites :: TerrainWrites
memptyTerrainWrites = emptyTerrainWrites

testContext :: PluginContext
testContext = PluginContext
  { pcWorld = emptyWorld (WorldConfig { wcChunkSize = 64 }) defaultHexGridMeta
  , pcParams = Map.empty
  , pcTerrain = object ["chunk_size" .= (64 :: Int)]
  , pcOwnOverlay = Nothing
  , pcOverlays = Map.empty
  , pcSeed = 0
  , pcLog = \_ -> pure ()
  , pcProgress = \_ _ -> pure ()
  , pcWorldPath = Nothing
  }

testSchema :: OverlaySchema
testSchema = OverlaySchema
  { osName = "test_overlay"
  , osVersion = "1.0.0"
  , osDescription = "payload helper test schema"
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
