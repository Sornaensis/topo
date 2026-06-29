{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Spec.TerrainInspector (spec) where

import Actor.Data (TerrainSnapshot(..))
import Actor.UI (UiState(..), ViewMode(..), emptyUiState)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Text as Text
import qualified Data.Vector.Unboxed as U
import Seer.Draw.Overlay (TerrainInspectorView(..), terrainInspectorView)
import Test.Hspec
import Topo
  ( TerrainChunk(..)
  , zeroDirSlope
  )
import Topo.Overlay (emptyOverlayStore)
import Topo.Types (pattern BiomeDesert, pattern FormFlat, pattern PlateBoundaryNone)

spec :: Spec
spec = describe "terrain inspector view model" $ do
  it "is absent until a hover hex is available" $ do
    terrainInspectorView emptyUiState emptyTerrainSnapshot `shouldBe` Nothing

  it "reports the hovered hex and no-data state" $ do
    let ui = emptyUiState { uiHoverHex = Just (3, -2) }
    fmap tivLines (terrainInspectorView ui emptyTerrainSnapshot)
      `shouldBe` Just ["Hex (3, -2)", "No data"]

  it "builds mode-specific lines from terrain samples" $ do
    let ui = emptyUiState { uiHoverHex = Just (0, 0), uiViewMode = ViewElevation }
        Just view = terrainInspectorView ui terrainSnapshotWithChunk
    tivHex view `shouldBe` (0, 0)
    take 1 (tivLines view) `shouldBe` ["Hex (0, 0)"]
    tivLines view `shouldSatisfy` any (Text.isPrefixOf "Elev")
    tivLines view `shouldSatisfy` any (Text.isPrefixOf "Form")
    tivLines view `shouldSatisfy` any (Text.isPrefixOf "Slope")

  it "surfaces missing overlay state for overlay view mode" $ do
    let ui = emptyUiState
          { uiHoverHex = Just (0, 0)
          , uiViewMode = ViewOverlay "culture" 0
          }
        Just view = terrainInspectorView ui terrainSnapshotWithChunk
    tivLines view `shouldSatisfy` elem "Overlay culture"
    tivLines view `shouldSatisfy` elem "(not loaded)"

emptyTerrainSnapshot :: TerrainSnapshot
emptyTerrainSnapshot = TerrainSnapshot 0 0 IntMap.empty IntMap.empty IntMap.empty IntMap.empty IntMap.empty emptyOverlayStore

terrainSnapshotWithChunk :: TerrainSnapshot
terrainSnapshotWithChunk = TerrainSnapshot
  { tsVersion = 1
  , tsChunkSize = chunkSize
  , tsTerrainChunks = IntMap.singleton 0 (emptyTerrainChunk chunkSize)
  , tsClimateChunks = IntMap.empty
  , tsWeatherChunks = IntMap.empty
  , tsRiverChunks = IntMap.empty
  , tsVegetationChunks = IntMap.empty
  , tsOverlayStore = emptyOverlayStore
  }

chunkSize :: Int
chunkSize = 2

emptyTerrainChunk :: Int -> TerrainChunk
emptyTerrainChunk size =
  let total = size * size
      zerosF = U.replicate total 0
      zerosW = U.replicate total 0
  in TerrainChunk
      { tcElevation = zerosF
      , tcDirSlope = U.replicate total zeroDirSlope
      , tcCurvature = zerosF
      , tcHardness = zerosF
      , tcRockType = zerosW
      , tcSoilType = zerosW
      , tcSoilDepth = zerosF
      , tcMoisture = zerosF
      , tcFertility = zerosF
      , tcRoughness = zerosF
      , tcRockDensity = zerosF
      , tcSoilGrain = zerosF
      , tcRelief = zerosF
      , tcRelief2Ring = zerosF
      , tcRelief3Ring = zerosF
      , tcRuggedness = zerosF
      , tcTerrainForm = U.replicate total FormFlat
      , tcFlags = U.replicate total BiomeDesert
      , tcMicroRelief = zerosF
      , tcPlateId = zerosW
      , tcPlateBoundary = U.replicate total PlateBoundaryNone
      , tcPlateHeight = zerosF
      , tcPlateHardness = zerosF
      , tcPlateCrust = zerosW
      , tcPlateAge = zerosF
      , tcPlateVelX = zerosF
      , tcPlateVelY = zerosF
      }
