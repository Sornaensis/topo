{-# LANGUAGE PatternSynonyms #-}

module Spec.EditorBrush (spec, emptyTerrainChunk) where

import Test.Hspec
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector.Unboxed as U
import Topo (WorldConfig(..), ChunkId(..), emptyTerrainChunk, chunkIdFromCoord)
import Topo.Types (ChunkCoord(..), TerrainChunk(..), BiomeId, TerrainForm, pattern BiomeDesert, pattern BiomeGrassland, pattern FormFlat, pattern FormHilly)
import Seer.Editor.Brush (applyBrushStroke, applySmoothStroke, applyFlattenStroke, applyNoiseStroke, applyPaintBiomeStroke, applyPaintFormStroke, applySetHardnessStroke, brushWeight)
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

    it "defaultEditorState has 1 smooth pass" $
      editorSmoothPasses defaultEditorState `shouldBe` 1

    it "defaultEditorState has noise frequency 1.0" $
      editorNoiseFrequency defaultEditorState `shouldBe` 1.0

    it "defaultEditorState has no flatten reference" $
      editorFlattenRef defaultEditorState `shouldBe` Nothing

    it "defaultEditorState has stroke id 0" $
      editorStrokeId defaultEditorState `shouldBe` 0

  ---------------------------------------------------------------------------
  -- applySmoothStroke
  ---------------------------------------------------------------------------
  describe "applySmoothStroke" $ do
    let cfg = WorldConfig { wcChunkSize = 4 }
        n = 4 * 4
        mkChunk elev = (emptyTerrainChunk cfg) { tcElevation = U.replicate n elev }
        chunkKey = let ChunkId k = chunkIdFromCoord (ChunkCoord 0 0) in k

    it "moves a spike toward neighbours" $ do
      -- Center at 1.0, all surroundings at 0.0 → should decrease
      let base = mkChunk 0.0
          -- Set tile (0,0) = index 0 to 1.0
          spikeElev = U.imap (\i e -> if i == 0 then 1.0 else e) (tcElevation base)
          chunks = IntMap.singleton chunkKey (base { tcElevation = spikeElev })
          brush = BrushSettings { brushRadius = 0, brushStrength = 0.5, brushFalloff = FalloffConstant }
          after = applySmoothStroke cfg brush 1 (0, 0) chunks
          Just chunk = IntMap.lookup chunkKey after
      -- The spike tile should have decreased (moved toward 0)
      U.head (tcElevation chunk) `shouldSatisfy` (< 1.0)

    it "does not change a flat field" $ do
      let chunks = IntMap.singleton chunkKey (mkChunk 0.5)
          brush = BrushSettings { brushRadius = 1, brushStrength = 1.0, brushFalloff = FalloffConstant }
          after = applySmoothStroke cfg brush 3 (0, 0) chunks
          Just chunk = IntMap.lookup chunkKey after
      -- All tiles are at 0.5, so smoothing should keep them near 0.5
      U.head (tcElevation chunk) `shouldSatisfy` (\e -> abs (e - 0.5) < 0.01)

  ---------------------------------------------------------------------------
  -- applyFlattenStroke
  ---------------------------------------------------------------------------
  describe "applyFlattenStroke" $ do
    let cfg = WorldConfig { wcChunkSize = 4 }
        n = 4 * 4
        mkChunk elev = (emptyTerrainChunk cfg) { tcElevation = U.replicate n elev }
        chunkKey = let ChunkId k = chunkIdFromCoord (ChunkCoord 0 0) in k

    it "moves elevation toward the reference" $ do
      let chunks = IntMap.singleton chunkKey (mkChunk 0.2)
          brush = BrushSettings { brushRadius = 0, brushStrength = 0.5, brushFalloff = FalloffConstant }
          after = applyFlattenStroke cfg brush 0.8 (0, 0) chunks
          Just chunk = IntMap.lookup chunkKey after
      -- Should move from 0.2 toward 0.8
      U.head (tcElevation chunk) `shouldSatisfy` (> 0.2)
      U.head (tcElevation chunk) `shouldSatisfy` (< 0.8)

    it "keeps elevation at the reference when already there" $ do
      let chunks = IntMap.singleton chunkKey (mkChunk 0.5)
          brush = BrushSettings { brushRadius = 0, brushStrength = 1.0, brushFalloff = FalloffConstant }
          after = applyFlattenStroke cfg brush 0.5 (0, 0) chunks
          Just chunk = IntMap.lookup chunkKey after
      U.head (tcElevation chunk) `shouldSatisfy` (\e -> abs (e - 0.5) < 0.001)

  ---------------------------------------------------------------------------
  -- applyNoiseStroke
  ---------------------------------------------------------------------------
  describe "applyNoiseStroke" $ do
    let cfg = WorldConfig { wcChunkSize = 4 }
        n = 4 * 4
        mkChunk elev = (emptyTerrainChunk cfg) { tcElevation = U.replicate n elev }
        chunkKey = let ChunkId k = chunkIdFromCoord (ChunkCoord 0 0) in k

    it "modifies elevation" $ do
      let chunks = IntMap.singleton chunkKey (mkChunk 0.5)
          brush = BrushSettings { brushRadius = 0, brushStrength = 0.3, brushFalloff = FalloffConstant }
          -- Use (1,1) instead of (0,0): fbm2D returns 0 at the origin
          after = applyNoiseStroke cfg brush 42 1 1.0 (1, 1) chunks
          Just chunk = IntMap.lookup chunkKey after
          -- Tile (1,1) → local index 1*4 + 1 = 5
      (tcElevation chunk U.! 5) `shouldSatisfy` (/= 0.5)

    it "produces different results for different stroke IDs" $ do
      let chunks = IntMap.singleton chunkKey (mkChunk 0.5)
          brush = BrushSettings { brushRadius = 0, brushStrength = 0.3, brushFalloff = FalloffConstant }
          after1 = applyNoiseStroke cfg brush 42 1 1.0 (1, 1) chunks
          after2 = applyNoiseStroke cfg brush 42 2 1.0 (1, 1) chunks
          Just c1 = IntMap.lookup chunkKey after1
          Just c2 = IntMap.lookup chunkKey after2
      (tcElevation c1 U.! 5) `shouldSatisfy` (/= (tcElevation c2 U.! 5))

    it "clamps elevation to [0,1]" $ do
      let chunks = IntMap.singleton chunkKey (mkChunk 0.99)
          brush = BrushSettings { brushRadius = 0, brushStrength = 1.0, brushFalloff = FalloffConstant }
          after = applyNoiseStroke cfg brush 42 1 1.0 (0, 0) chunks
          Just chunk = IntMap.lookup chunkKey after
      U.head (tcElevation chunk) `shouldSatisfy` (<= 1.0)
      U.head (tcElevation chunk) `shouldSatisfy` (>= 0.0)

  ---------------------------------------------------------------------------
  -- applyPaintBiomeStroke
  ---------------------------------------------------------------------------
  describe "applyPaintBiomeStroke" $ do
    let cfg = WorldConfig { wcChunkSize = 4 }
        n = 4 * 4
        mkChunk' = emptyTerrainChunk cfg
        chunkKey = let ChunkId k = chunkIdFromCoord (ChunkCoord 0 0) in k

    it "paints the target biome onto the center tile" $ do
      let chunks = IntMap.singleton chunkKey mkChunk'
          brush = BrushSettings { brushRadius = 0, brushStrength = 1.0, brushFalloff = FalloffConstant }
          after = applyPaintBiomeStroke cfg brush BiomeGrassland (0, 0) chunks
          Just chunk = IntMap.lookup chunkKey after
      U.head (tcFlags chunk) `shouldBe` BiomeGrassland

    it "does not paint tiles below the weight threshold" $ do
      -- radius 2 with linear falloff: tiles at distance 2 get weight 0 (< 0.5)
      let chunks = IntMap.singleton chunkKey mkChunk'
          brush = BrushSettings { brushRadius = 2, brushStrength = 1.0, brushFalloff = FalloffLinear }
          after = applyPaintBiomeStroke cfg brush BiomeGrassland (0, 0) chunks
          Just chunk = IntMap.lookup chunkKey after
      -- Center (dist 0, weight 1.0) should be painted
      U.head (tcFlags chunk) `shouldBe` BiomeGrassland

  ---------------------------------------------------------------------------
  -- applyPaintFormStroke
  ---------------------------------------------------------------------------
  describe "applyPaintFormStroke" $ do
    let cfg = WorldConfig { wcChunkSize = 4 }
        mkChunk' = emptyTerrainChunk cfg
        chunkKey = let ChunkId k = chunkIdFromCoord (ChunkCoord 0 0) in k

    it "paints the target terrain form" $ do
      let chunks = IntMap.singleton chunkKey mkChunk'
          brush = BrushSettings { brushRadius = 0, brushStrength = 1.0, brushFalloff = FalloffConstant }
          after = applyPaintFormStroke cfg brush FormHilly (0, 0) chunks
          Just chunk = IntMap.lookup chunkKey after
      U.head (tcTerrainForm chunk) `shouldBe` FormHilly

    it "overwrites existing terrain form" $ do
      let base = mkChunk'
          forms = U.replicate (4*4) FormHilly
          chunk0 = base { tcTerrainForm = forms }
          chunks = IntMap.singleton chunkKey chunk0
          brush = BrushSettings { brushRadius = 0, brushStrength = 1.0, brushFalloff = FalloffConstant }
          after = applyPaintFormStroke cfg brush FormFlat (0, 0) chunks
          Just chunk = IntMap.lookup chunkKey after
      U.head (tcTerrainForm chunk) `shouldBe` FormFlat

  ---------------------------------------------------------------------------
  -- applySetHardnessStroke
  ---------------------------------------------------------------------------
  describe "applySetHardnessStroke" $ do
    let cfg = WorldConfig { wcChunkSize = 4 }
        n = 4 * 4
        mkChunk elev = (emptyTerrainChunk cfg) { tcHardness = U.replicate n elev }
        chunkKey = let ChunkId k = chunkIdFromCoord (ChunkCoord 0 0) in k

    it "moves hardness toward the target" $ do
      let chunks = IntMap.singleton chunkKey (mkChunk 0.2)
          brush = BrushSettings { brushRadius = 0, brushStrength = 0.5, brushFalloff = FalloffConstant }
          after = applySetHardnessStroke cfg brush 0.8 (0, 0) chunks
          Just chunk = IntMap.lookup chunkKey after
      U.head (tcHardness chunk) `shouldSatisfy` (> 0.2)
      U.head (tcHardness chunk) `shouldSatisfy` (< 0.8)

    it "clamps hardness to [0,1]" $ do
      let chunks = IntMap.singleton chunkKey (mkChunk 0.5)
          brush = BrushSettings { brushRadius = 0, brushStrength = 1.0, brushFalloff = FalloffConstant }
          after = applySetHardnessStroke cfg brush 1.0 (0, 0) chunks
          Just chunk = IntMap.lookup chunkKey after
      U.head (tcHardness chunk) `shouldSatisfy` (<= 1.0)
      U.head (tcHardness chunk) `shouldSatisfy` (>= 0.0)
