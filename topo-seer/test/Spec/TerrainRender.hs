{-# LANGUAGE PatternSynonyms #-}

module Spec.TerrainRender (spec) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as U
import Test.Hspec
import Linear (V2(..))
import Topo (WorldConfig(..), TerrainChunk(..), zeroDirSlope)
import Topo.Types (pattern BiomeDesert, pattern FormFlat, pattern PlateBoundaryNone)
import Actor.UI (ViewMode(..))
import UI.HexPick (renderHexRadiusPx)
import UI.TerrainRender (ChunkGeometry(..), buildChunkGeometry)
import UI.Widgets (Rect(..))

spec :: Spec
spec = describe "Terrain render geometry" $ do
  it "builds expected vertex and index counts" $ do
    let size = 2
        config = WorldConfig { wcChunkSize = size }
        chunk = emptyTerrainChunk size
        geometry = buildChunkGeometry renderHexRadiusPx config ViewElevation 0 IntMap.empty IntMap.empty IntMap.empty Nothing Nothing 0 chunk
        tileCount = size * size
    SV.length (cgVertices geometry) `shouldBe` tileCount * 7
    SV.length (cgIndices geometry) `shouldBe` tileCount * 18

  it "produces non-zero bounds" $ do
    let size = 3
        config = WorldConfig { wcChunkSize = size }
        chunk = emptyTerrainChunk size
        geometry = buildChunkGeometry renderHexRadiusPx config ViewElevation 0 IntMap.empty IntMap.empty IntMap.empty Nothing Nothing 0 chunk
        Rect (V2 _ _ , V2 w h) = cgBounds geometry
    w `shouldSatisfy` (> 0)
    h `shouldSatisfy` (> 0)

emptyTerrainChunk :: Int -> TerrainChunk
emptyTerrainChunk size =
  let total = size * size
      zerosF = U.replicate total 0
      zerosW = U.replicate total 0
      biomeZeros = U.replicate total BiomeDesert
      boundaryZeros = U.replicate total PlateBoundaryNone
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
      , tcFlags = biomeZeros
      , tcMicroRelief = zerosF
      , tcPlateId = zerosW
    , tcPlateBoundary = boundaryZeros
    , tcPlateHeight = zerosF
    , tcPlateHardness = zerosF
    , tcPlateCrust = zerosW
    , tcPlateAge = zerosF
    , tcPlateVelX = zerosF
    , tcPlateVelY = zerosF
      }
