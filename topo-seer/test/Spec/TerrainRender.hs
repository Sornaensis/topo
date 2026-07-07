{-# LANGUAGE PatternSynonyms #-}

module Spec.TerrainRender (spec) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector.Storable as SV
import Data.Word (Word8)
import qualified Data.Vector.Unboxed as U
import Test.Hspec
import Linear (V2(..))
import qualified SDL.Raw.Types as Raw
import Topo (WorldConfig(..), TerrainChunk(..), zeroDirSlope)
import Topo.Types (pattern BiomeDesert, pattern FormFlat, pattern PlateBoundaryNone)
import Actor.UI (ViewMode(..))
import UI.HexGeometry (hexCenterF, renderHexRadiusPx)
import UI.TerrainRender (ChunkGeometry(..), buildChunkGeometry, buildDayNightGeometry)
import UI.Widgets (Rect(..))

spec :: Spec
spec = describe "Terrain render geometry" $ do
  it "builds expected vertex and index counts" $ do
    let size = 2
        config = WorldConfig { wcChunkSize = size }
        chunk = emptyTerrainChunk size
        geometry = buildChunkGeometry renderHexRadiusPx config ViewElevation 0 IntMap.empty IntMap.empty IntMap.empty Nothing 0 chunk
        tileCount = size * size
    SV.length (cgVertices geometry) `shouldBe` tileCount * 7
    SV.length (cgIndices geometry) `shouldBe` tileCount * 18

  it "produces non-zero bounds" $ do
    let size = 3
        config = WorldConfig { wcChunkSize = size }
        chunk = emptyTerrainChunk size
        geometry = buildChunkGeometry renderHexRadiusPx config ViewElevation 0 IntMap.empty IntMap.empty IntMap.empty Nothing 0 chunk
        Rect (V2 _ _ , V2 w h) = cgBounds geometry
    w `shouldSatisfy` (> 0)
    h `shouldSatisfy` (> 0)

  it "keeps fallback geometry vertices local to cgBounds" $ do
    let size = 1
        config = WorldConfig { wcChunkSize = size }
        chunk = emptyTerrainChunk size
        geometry = buildChunkGeometry renderHexRadiusPx config ViewElevation 0 IntMap.empty IntMap.empty IntMap.empty Nothing 0 chunk
        Rect (V2 bx by, _) = cgBounds geometry
        Raw.Vertex (Raw.FPoint localX localY) _ _ = cgVertices geometry SV.! 0
        (centerX, centerY) = hexCenterF renderHexRadiusPx 0 0
    realToFrac bx + realToFrac localX `shouldSatisfy` closeTo centerX
    realToFrac by + realToFrac localY `shouldSatisfy` closeTo centerY

  it "builds a transparent black day/night overlay at full brightness" $ do
    let size = 1
        config = WorldConfig { wcChunkSize = size }
        chunk = emptyTerrainChunk size
        geometry = buildDayNightGeometry renderHexRadiusPx config (\_ _ -> 1.0) 0 chunk
    map vertexColor (SV.toList (cgVertices geometry)) `shouldSatisfy` all (== (0, 0, 0, 0))

  it "builds a high-alpha black day/night overlay for deep night" $ do
    let size = 1
        config = WorldConfig { wcChunkSize = size }
        chunk = emptyTerrainChunk size
        geometry = buildDayNightGeometry renderHexRadiusPx config (\_ _ -> 0.15) 0 chunk
    map vertexColor (SV.toList (cgVertices geometry)) `shouldSatisfy` all (== (0, 0, 0, 217))

vertexColor :: Raw.Vertex -> (Word8, Word8, Word8, Word8)
vertexColor (Raw.Vertex _ (Raw.Color r g b a) _) = (r, g, b, a)

closeTo :: Float -> Float -> Bool
closeTo expected actual = abs (actual - expected) < 0.001

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
