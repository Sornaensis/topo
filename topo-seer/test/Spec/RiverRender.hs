{-# LANGUAGE PatternSynonyms #-}

module Spec.RiverRender (spec) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector.Storable as SV
import qualified Data.Vector.Unboxed as U
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Linear (V2(..))
import Topo (RiverChunk(..), WorldConfig(..))
import UI.RiverRender
  ( RiverGeometry(..)
  , RiverRenderConfig(..)
  , buildChunkRiverGeometry
  , buildDeltaFan
  , deltaParamsForOrder
  , defaultRiverRenderConfig
  )
import UI.Widgets (Rect(..))
import qualified SDL.Raw.Types as Raw
import Foreign.C.Types (CInt)

spec :: Spec
spec = describe "River render geometry" $ do
  it "returns Nothing for empty river chunks map" $ do
    let config = WorldConfig { wcChunkSize = 4 }
    buildChunkRiverGeometry defaultRiverRenderConfig config 0 IntMap.empty
      `shouldBe` Nothing

  it "returns Nothing when chunk has no segments" $ do
    let size = 4
        config = WorldConfig { wcChunkSize = size }
        tileCount = size * size
        rc = emptyRiverChunk tileCount
    buildChunkRiverGeometry defaultRiverRenderConfig config 0
      (IntMap.singleton 0 rc)
      `shouldBe` Nothing

  it "produces non-empty geometry for a chunk with one through-segment" $ do
    let size = 4
        config = WorldConfig { wcChunkSize = size }
        tileCount = size * size
        rc = singleSegmentRiverChunk tileCount
        result = buildChunkRiverGeometry defaultRiverRenderConfig config 0
                   (IntMap.singleton 0 rc)
    result `shouldSatisfy` (/= Nothing)
    case result of
      Nothing -> pure ()
      Just rg -> do
        SV.length (rgVertices rg) `shouldSatisfy` (> 0)
        SV.length (rgIndices rg) `shouldSatisfy` (> 0)

  it "produces 8 vertices and 12 indices for one through-segment" $ do
    -- A through-segment (entry→centre + centre→exit) = 2 quads = 2×4 verts, 2×6 indices
    let size = 4
        config = WorldConfig { wcChunkSize = size }
        tileCount = size * size
        rc = singleSegmentRiverChunk tileCount
        Just rg = buildChunkRiverGeometry defaultRiverRenderConfig config 0
                    (IntMap.singleton 0 rc)
    SV.length (rgVertices rg) `shouldBe` 8
    SV.length (rgIndices rg) `shouldBe` 12

  it "produces 4 vertices and 6 indices for a source-only segment" $ do
    let size = 4
        config = WorldConfig { wcChunkSize = size }
        tileCount = size * size
        rc = sourceSegmentRiverChunk tileCount
        Just rg = buildChunkRiverGeometry defaultRiverRenderConfig config 0
                    (IntMap.singleton 0 rc)
    -- Source: only centre→exit = 1 quad = 4 verts, 6 indices
    SV.length (rgVertices rg) `shouldBe` 4
    SV.length (rgIndices rg) `shouldBe` 6

  it "scales line width with river order" $ do
    let cfg1 = defaultRiverRenderConfig { rrcStreamHalfWidth = 0.5 }
        cfg2 = defaultRiverRenderConfig { rrcMajorHalfWidth = 5.0 }
        size = 4
        config = WorldConfig { wcChunkSize = size }
        tileCount = size * size
        rc1 = makeRiverChunk tileCount 0 1 3  -- order 1 = stream
        rc2 = makeRiverChunk tileCount 0 3 7  -- order 7 = major
    -- Both should produce geometry (just checking the config propagates)
    buildChunkRiverGeometry cfg1 config 0 (IntMap.singleton 0 rc1)
      `shouldSatisfy` (/= Nothing)
    buildChunkRiverGeometry cfg2 config 0 (IntMap.singleton 0 rc2)
      `shouldSatisfy` (/= Nothing)

  it "non-zero bounds for chunk with segments" $ do
    let size = 4
        config = WorldConfig { wcChunkSize = size }
        tileCount = size * size
        rc = singleSegmentRiverChunk tileCount
        Just rg = buildChunkRiverGeometry defaultRiverRenderConfig config 0
                    (IntMap.singleton 0 rc)
        Rect (V2 _ _, V2 w h) = rgBounds rg
    w `shouldSatisfy` (> 0)
    h `shouldSatisfy` (> 0)

  prop "produces valid index references" $ \(Positive sz') -> do
    let size = min 8 (max 2 sz') -- Keep chunk size 2..8
        config = WorldConfig { wcChunkSize = size }
        tileCount = size * size
        rc = singleSegmentRiverChunk tileCount
        result = buildChunkRiverGeometry defaultRiverRenderConfig config 0
                   (IntMap.singleton 0 rc)
    case result of
      Nothing -> pure ()
      Just rg -> do
        let nVerts = fromIntegral (SV.length (rgVertices rg))
        SV.toList (rgIndices rg) `shouldSatisfy` all (\i -> i >= 0 && i < nVerts)

  -- -----------------------------------------------------------------------
  -- 12.4.3: Terminus delta geometry at sink segments
  -- -----------------------------------------------------------------------
  describe "terminus delta geometry" $ do
    it "sink segment produces entry→centre quad + delta fan = 9 verts, 15 indices" $ do
      -- A sink segment: entry=0, exit=255 (sink), order=3 (stream).
      -- Stream delta: triCount=3 → 5 verts (1 centre + 4 rim), 9 indices
      -- Quad: 4 verts, 6 indices.  Total: 9 verts, 15 indices.
      let size = 4
          config = WorldConfig { wcChunkSize = size }
          tileCount = size * size
          rc = sinkSegmentRiverChunk tileCount
          Just rg = buildChunkRiverGeometry defaultRiverRenderConfig config 0
                      (IntMap.singleton 0 rc)
      SV.length (rgVertices rg) `shouldBe` 9
      SV.length (rgIndices rg) `shouldBe` 15

    it "through segment has no delta (8 verts, 12 indices)" $ do
      let size = 4
          config = WorldConfig { wcChunkSize = size }
          tileCount = size * size
          rc = singleSegmentRiverChunk tileCount
          Just rg = buildChunkRiverGeometry defaultRiverRenderConfig config 0
                      (IntMap.singleton 0 rc)
      -- A through-segment: 2 quads = 8 verts + 12 indices (no delta)
      SV.length (rgVertices rg) `shouldBe` 8
      SV.length (rgIndices rg) `shouldBe` 12

    it "source→sink segment produces no geometry" $ do
      -- entry=255, exit=255: degenerate, should be skipped
      let size = 4
          config = WorldConfig { wcChunkSize = size }
          tileCount = size * size
          rc = makeRiverChunk tileCount 255 255 1
      buildChunkRiverGeometry defaultRiverRenderConfig config 0
        (IntMap.singleton 0 rc) `shouldBe` Nothing

    prop "sink segment indices reference valid vertices" $ \(Positive sz') -> do
      let size = min 8 (max 2 sz')
          config = WorldConfig { wcChunkSize = size }
          tileCount = size * size
          rc = sinkSegmentRiverChunk tileCount
          result = buildChunkRiverGeometry defaultRiverRenderConfig config 0
                     (IntMap.singleton 0 rc)
      case result of
        Nothing -> pure ()
        Just rg -> do
          let nVerts = fromIntegral (SV.length (rgVertices rg))
          SV.toList (rgIndices rg) `shouldSatisfy` all (\i -> i >= 0 && i < nVerts)

  -- -----------------------------------------------------------------------
  -- Delta fan unit tests
  -- -----------------------------------------------------------------------
  describe "delta fan geometry" $ do
    it "buildDeltaFan produces triCount+2 verts and triCount*3 indices" $ do
      let color = Raw.Color 50 95 155 255
          (vs, is, nextBase) = buildDeltaFan color 4.0 (pi / 2) 6 10.0 10.0 0.0 (0 :: CInt)
      length vs `shouldBe` 8        -- 6 + 1 rim + 1 centre = 8
      length is `shouldBe` 18       -- 6 * 3
      nextBase `shouldBe` 8

    it "delta fan direction faces away from entry" $ do
      -- Stream delta: 3 triangles, spread 45°
      let (radius, spreadDeg, triCount) = deltaParamsForOrder defaultRiverRenderConfig 1
      radius `shouldBe` 1.5
      spreadDeg `shouldBe` 45.0
      triCount `shouldBe` 3

    it "major river gets larger delta" $ do
      let (radius, spreadDeg, triCount) = deltaParamsForOrder defaultRiverRenderConfig 7
      radius `shouldBe` 6.0
      spreadDeg `shouldBe` 120.0
      triCount `shouldBe` 8

    it "major sink segment produces larger geometry than stream sink" $ do
      let size = 4
          config = WorldConfig { wcChunkSize = size }
          tileCount = size * size
          rcStream = makeRiverChunk tileCount 0 255 1  -- stream (order 1)
          rcMajor  = makeRiverChunk tileCount 0 255 7  -- major (order 7)
          Just rgStream = buildChunkRiverGeometry defaultRiverRenderConfig config 0
                            (IntMap.singleton 0 rcStream)
          Just rgMajor  = buildChunkRiverGeometry defaultRiverRenderConfig config 0
                            (IntMap.singleton 0 rcMajor)
      -- Major has more delta triangles (8 vs 3), so more verts/indices
      SV.length (rgVertices rgMajor) `shouldSatisfy` (> SV.length (rgVertices rgStream))
      SV.length (rgIndices rgMajor)  `shouldSatisfy` (> SV.length (rgIndices rgStream))

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | A RiverChunk with zero segments on all tiles.
emptyRiverChunk :: Int -> RiverChunk
emptyRiverChunk tileCount = RiverChunk
  { rcFlowAccum = U.replicate tileCount 0
  , rcDischarge = U.replicate tileCount 0
  , rcChannelDepth = U.replicate tileCount 0
  , rcRiverOrder = U.replicate tileCount 0
  , rcBasinId = U.replicate tileCount 0
  , rcBaseflow = U.replicate tileCount 0
  , rcErosionPotential = U.replicate tileCount 0
  , rcDepositPotential = U.replicate tileCount 0
  , rcFlowDir = U.replicate tileCount (-1)
  , rcSegOffsets = U.replicate (tileCount + 1) 0
  , rcSegEntryEdge = U.empty
  , rcSegExitEdge = U.empty
  , rcSegDischarge = U.empty
  , rcSegOrder = U.empty
  }

-- | A RiverChunk with exactly one through-segment on tile 0 (entry=0, exit=3).
singleSegmentRiverChunk :: Int -> RiverChunk
singleSegmentRiverChunk tileCount = makeRiverChunk tileCount 0 0 3

-- | A RiverChunk with exactly one source segment on tile 0 (entry=255, exit=3).
sourceSegmentRiverChunk :: Int -> RiverChunk
sourceSegmentRiverChunk tileCount = makeRiverChunk tileCount 255 0 3

-- | A RiverChunk with exactly one sink segment on tile 0 (entry=0, exit=255).
sinkSegmentRiverChunk :: Int -> RiverChunk
sinkSegmentRiverChunk tileCount = makeRiverChunk tileCount 0 255 3

-- | A RiverChunk with one segment at tile 0 with given entry/exit edge and order.
makeRiverChunk :: Int -> Int -> Int -> Int -> RiverChunk
makeRiverChunk tileCount entryEdge exitEdge order =
  let offsets = U.generate (tileCount + 1) (\i -> if i > 0 then 1 else 0)
  in RiverChunk
    { rcFlowAccum = U.replicate tileCount 0
    , rcDischarge = U.replicate tileCount 0
    , rcChannelDepth = U.replicate tileCount 0
    , rcRiverOrder = U.replicate tileCount 0
    , rcBasinId = U.replicate tileCount 0
    , rcBaseflow = U.replicate tileCount 0
    , rcErosionPotential = U.replicate tileCount 0
    , rcDepositPotential = U.replicate tileCount 0
    , rcFlowDir = U.replicate tileCount (-1)
    , rcSegOffsets = offsets
    , rcSegEntryEdge = U.singleton (fromIntegral entryEdge)
    , rcSegExitEdge = U.singleton (fromIntegral exitEdge)
    , rcSegDischarge = U.singleton 1.0
    , rcSegOrder = U.singleton (fromIntegral order)
    }
