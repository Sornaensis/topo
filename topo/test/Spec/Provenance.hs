module Spec.Provenance (spec) where

import Test.Hspec
import qualified Data.Text as Text
import Topo
import Topo.Storage
  ( MapProvenance(..)
  , WorldProvenance(..)
  , decodeWorldWithProvenance
  , encodeWorldWithProvenance
  )

spec :: Spec
spec = describe "Provenance" $ do
  it "roundtrips world provenance" $ do
    let config = WorldConfig { wcChunkSize = 4 }
        world = emptyWorld config defaultHexGridMeta
        terrainProv = MapProvenance { mpSeed = 10, mpVersion = 2, mpParams = Text.pack "terrain=default" }
        climateProv = MapProvenance { mpSeed = 20, mpVersion = 3, mpParams = Text.pack "climate=default" }
        biomeProv = MapProvenance { mpSeed = 40, mpVersion = 5, mpParams = Text.pack "biome=default" }
        prov = WorldProvenance
          { wpSeed = 42
          , wpVersion = 7
          , wpNotes = Text.pack "test"
          , wpTerrain = terrainProv
          , wpClimate = climateProv
          , wpBiome = biomeProv
          }
    case encodeWorldWithProvenance prov world of
      Left err -> expectationFailure (show err)
      Right encoded ->
        case decodeWorldWithProvenance encoded of
          Left err -> expectationFailure (show err)
          Right (prov', _) -> prov' `shouldBe` prov
