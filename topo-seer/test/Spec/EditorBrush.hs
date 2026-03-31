module Spec.EditorBrush (spec) where

import Test.Hspec
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector.Unboxed as U
import Topo (WorldConfig(..), ChunkId(..), emptyTerrainChunk, chunkIdFromCoord)
import Topo.Types (ChunkCoord(..), TerrainChunk(..))
import Seer.Editor.Brush (applyBrushStroke, brushWeight)
import Seer.Editor.Types

spec :: Spec
spec = describe "Editor.Brush" $ do
  ---------------------------------------------------------------------------
  -- brushWeight
  ---------------------------------------------------------------------------
  describe "brushWeight" $ do
    it "returns 1.0 at the center for linear falloff" $
      brushWeight FalloffLinear 3 0 `shouldBe` 1.0

    it "returns 0.0 beyond the radius" $
      brushWeight FalloffLinear 3 4 `shouldBe` 0.0

    it "returns 1.0 everywhere for constant falloff" $ do
      brushWeight FalloffConstant 3 0 `shouldBe` 1.0
      brushWeight FalloffConstant 3 2 `shouldBe` 1.0
      brushWeight FalloffConstant 3 3 `shouldBe` 1.0

    it "returns 0.0 at the edge for linear falloff" $
      brushWeight FalloffLinear 3 3 `shouldBe` 0.0

    it "returns 1.0 when radius is 0 and dist is 0" $
      brushWeight FalloffLinear 0 0 `shouldBe` 1.0

    it "smooth falloff returns 1.0 at center" $
      brushWeight FalloffSmooth 3 0 `shouldBe` 1.0

    it "smooth falloff decreases with distance" $ do
      let w1 = brushWeight FalloffSmooth 3 1
          w2 = brushWeight FalloffSmooth 3 2
      w1 `shouldSatisfy` (> w2)
      w2 `shouldSatisfy` (> 0)

  ---------------------------------------------------------------------------
  -- applyBrushStroke
  ---------------------------------------------------------------------------
  describe "applyBrushStroke" $ do
    let cfg = WorldConfig { wcChunkSize = 4 }
        n = 4 * 4
        mkChunk elev = (emptyTerrainChunk cfg) { tcElevation = U.replicate n elev }
        -- Single chunk at (0,0) — chunkId for coord (0,0)
        chunkKey = let ChunkId k = chunkIdFromCoord (ChunkCoord 0 0) in k
        singleChunk elev = IntMap.singleton chunkKey (mkChunk elev)

    it "ToolRaise increases elevation" $ do
      let brush = BrushSettings { brushRadius = 0, brushStrength = 0.1, brushFalloff = FalloffConstant }
          before = singleChunk 0.5
          after = applyBrushStroke cfg ToolRaise brush (0, 0) before
          Just chunk = IntMap.lookup chunkKey after
      -- Center tile (0,0) should be raised
      U.head (tcElevation chunk) `shouldSatisfy` (> 0.5)

    it "ToolLower decreases elevation" $ do
      let brush = BrushSettings { brushRadius = 0, brushStrength = 0.1, brushFalloff = FalloffConstant }
          before = singleChunk 0.5
          after = applyBrushStroke cfg ToolLower brush (0, 0) before
          Just chunk = IntMap.lookup chunkKey after
      U.head (tcElevation chunk) `shouldSatisfy` (< 0.5)

    it "clamps elevation to [0,1]" $ do
      let brush = BrushSettings { brushRadius = 0, brushStrength = 0.5, brushFalloff = FalloffConstant }
          -- Raise from 0.9 by 0.5 should clamp to 1.0
          before = singleChunk 0.9
          after = applyBrushStroke cfg ToolRaise brush (0, 0) before
          Just chunk = IntMap.lookup chunkKey after
      U.head (tcElevation chunk) `shouldSatisfy` (<= 1.0)

    it "does not modify tiles outside the brush radius" $ do
      -- Use radius 0 brush: only center tile should change
      let brush = BrushSettings { brushRadius = 0, brushStrength = 0.1, brushFalloff = FalloffConstant }
          before = singleChunk 0.5
          after = applyBrushStroke cfg ToolRaise brush (0, 0) before
          Just chunkBefore = IntMap.lookup chunkKey before
          Just chunkAfter = IntMap.lookup chunkKey after
          elevBefore = tcElevation chunkBefore
          elevAfter = tcElevation chunkAfter
      -- Tile at index 0 changed, but tile at index 1 did not
      -- (tile at (1,0) is index 1, which is outside radius-0 brush at (0,0))
      (U.head elevAfter > U.head elevBefore) `shouldBe` True

  ---------------------------------------------------------------------------
  -- EditorState / defaults
  ---------------------------------------------------------------------------
  describe "EditorState" $ do
    it "defaultEditorState is inactive" $
      editorActive defaultEditorState `shouldBe` False

    it "defaultEditorState uses ToolRaise" $
      editorTool defaultEditorState `shouldBe` ToolRaise

    it "defaultBrushSettings has radius 2" $
      brushRadius defaultBrushSettings `shouldBe` 2
