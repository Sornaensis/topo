{-# LANGUAGE OverloadedStrings #-}

module Spec.Payload (spec) where

import Data.Aeson (Value(..), (.=), object)
import qualified Data.Map.Strict as Map
import Test.Hspec

import Topo.Hex (defaultHexGridMeta)
import Topo.Overlay (Overlay, emptyOverlay, overlayName)
import Topo.Overlay.Schema (OverlaySchema(..), OverlayStorage(..), OverlayField(..), OverlayFieldType(..))
import Topo.Plugin.SDK.Payload
import Topo.Plugin.SDK.Types
import Topo.Simulation (TerrainWrites, emptyTerrainWrites)
import Topo.Types (WorldConfig(..))
import Topo.World (emptyWorld)

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
    let world = emptyWorld (WorldConfig { wcChunkSize = 64 }) defaultHexGridMeta
    case encodeTerrainPayload world >>= decodeTerrainPayload of
      Left err -> expectationFailure (show err)
      Right decoded -> wcChunkSize (twConfig decoded) `shouldBe` 64

  it "builds simulation result with encoded writes" $ do
    let overlay = emptyOverlay testSchema
    case simulationResultWithTerrainWrites overlay memptyTerrainWrites of
      Left err -> expectationFailure (show err)
      Right result -> do
        strOverlay result `shouldBe` encodeOverlayPayload overlay
        strTerrainWrites result `shouldSatisfy` maybe False isObject

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
  }

testSchema :: OverlaySchema
testSchema = OverlaySchema
  { osName = "test_overlay"
  , osVersion = "1.0.0"
  , osStorage = StorageSparse
  , osFields =
      [ OverlayField
          { ofName = "value"
          , ofType = OFFloat
          , ofDefault = Number 0
          , ofIndexed = False
          }
      ]
  , osDependencies = []
  }
