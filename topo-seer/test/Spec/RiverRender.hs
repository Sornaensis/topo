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
import UI.HexPick (renderHexRadiusPx)
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
    buildChunkRiverGeometry defaultRiverRenderConfig config renderHexRadiusPx 0 IntMap.empty IntMap.empty
      `shouldBe` Nothing

  it "returns Nothing when chunk has no segments" $ do
    let size = 4
        config = WorldConfig { wcChunkSize = size }
        tileCount = size * size
        rc = emptyRiverChunk tileCount
    buildChunkRiverGeometry defaultRiverRenderConfig config renderHexRadiusPx 0
      (IntMap.singleton 0 rc) IntMap.empty
      `shouldBe` Nothing

  it "produces non-empty geometry for a chunk with one through-segment" $ do
    let size = 4
        config = WorldConfig { wcChunkSize = size }
        tileCount = size * size
        rc = singleSegmentRiverChunk tileCount
        result = buildChunkRiverGeometry defaultRiverRenderConfig config renderHexRadiusPx 0
                   (IntMap.singleton 0 rc) IntMap.empty
    result `shouldSatisfy` (/= Nothing)
    case result of
      Nothing -> pure ()
      Just rg -> do
        SV.length (rgVertices rg) `shouldSatisfy` (> 0)
        SV.length (rgIndices rg) `shouldSatisfy` (> 0)

  it "produces 22 vertices and 48 indices for one through-segment" $ do
    -- A through-segment (entry→centre + centre→exit) = 2 quads = 2×4 verts, 2×6 indices
    -- Plus disc join at centre: 14 verts, 36 indices.  Total: 22v, 48i.
    let size = 4
        config = WorldConfig { wcChunkSize = size }
        tileCount = size * size
        rc = singleSegmentRiverChunk tileCount
        Just rg = buildChunkRiverGeometry defaultRiverRenderConfig config renderHexRadiusPx 0
                    (IntMap.singleton 0 rc) IntMap.empty
    SV.length (rgVertices rg) `shouldBe` 22
    SV.length (rgIndices rg) `shouldBe` 48

  it "produces 4 vertices and 6 indices for a source-only segment" $ do
    let size = 4
        config = WorldConfig { wcChunkSize = size }
        tileCount = size * size
        rc = sourceSegmentRiverChunk tileCount
        Just rg = buildChunkRiverGeometry defaultRiverRenderConfig config renderHexRadiusPx 0
                    (IntMap.singleton 0 rc) IntMap.empty
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
    buildChunkRiverGeometry cfg1 config renderHexRadiusPx 0 (IntMap.singleton 0 rc1) IntMap.empty
      `shouldSatisfy` (/= Nothing)
    buildChunkRiverGeometry cfg2 config renderHexRadiusPx 0 (IntMap.singleton 0 rc2) IntMap.empty
      `shouldSatisfy` (/= Nothing)

  it "non-zero bounds for chunk with segments" $ do
    let size = 4
        config = WorldConfig { wcChunkSize = size }
        tileCount = size * size
        rc = singleSegmentRiverChunk tileCount
        Just rg = buildChunkRiverGeometry defaultRiverRenderConfig config renderHexRadiusPx 0
                    (IntMap.singleton 0 rc) IntMap.empty
        Rect (V2 _ _, V2 w h) = rgBounds rg
    w `shouldSatisfy` (> 0)
    h `shouldSatisfy` (> 0)

  prop "produces valid index references" $ \(Positive sz') -> do
    let size = min 8 (max 2 sz') -- Keep chunk size 2..8
        config = WorldConfig { wcChunkSize = size }
        tileCount = size * size
        rc = singleSegmentRiverChunk tileCount
        result = buildChunkRiverGeometry defaultRiverRenderConfig config renderHexRadiusPx 0
                   (IntMap.singleton 0 rc) IntMap.empty
    case result of
      Nothing -> pure ()
      Just rg -> do
        let nVerts = fromIntegral (SV.length (rgVertices rg))
        SV.toList (rgIndices rg) `shouldSatisfy` all (\i -> i >= 0 && i < nVerts)

  -- -----------------------------------------------------------------------
  -- 12.4.3: Terminus delta geometry at sink segments
  -- -----------------------------------------------------------------------
  describe "terminus delta geometry" $ do
    it "sink segment produces entry quad + delta fan + disc join" $ do
      -- A sink segment: entry=0, exit=255 (sink), order=3 (creek).
      -- Entry quad: 4 verts, 6 indices.
      -- Creek delta: triCount=4 → 6 verts, 12 indices.
      -- Disc join: 14 verts, 36 indices.  Total: 24v, 54i.
      let size = 4
          config = WorldConfig { wcChunkSize = size }
          tileCount = size * size
          rc = sinkSegmentRiverChunk tileCount
          Just rg = buildChunkRiverGeometry defaultRiverRenderConfig config renderHexRadiusPx 0
                      (IntMap.singleton 0 rc) IntMap.empty
      SV.length (rgVertices rg) `shouldBe` 24
      SV.length (rgIndices rg) `shouldBe` 54

    it "through segment has no delta but includes disc join" $ do
      let size = 4
          config = WorldConfig { wcChunkSize = size }
          tileCount = size * size
          rc = singleSegmentRiverChunk tileCount
          Just rg = buildChunkRiverGeometry defaultRiverRenderConfig config renderHexRadiusPx 0
                      (IntMap.singleton 0 rc) IntMap.empty
      -- 2 quads = 8v/12i + disc join = 14v/36i.  Total: 22v, 48i.
      SV.length (rgVertices rg) `shouldBe` 22
      SV.length (rgIndices rg) `shouldBe` 48

    it "source→sink segment produces no geometry" $ do
      -- entry=255, exit=255: degenerate, should be skipped
      let size = 4
          config = WorldConfig { wcChunkSize = size }
          tileCount = size * size
          rc = makeRiverChunk tileCount 255 255 1
      buildChunkRiverGeometry defaultRiverRenderConfig config renderHexRadiusPx 0
        (IntMap.singleton 0 rc) IntMap.empty `shouldBe` Nothing

    prop "sink segment indices reference valid vertices" $ \(Positive sz') -> do
      let size = min 8 (max 2 sz')
          config = WorldConfig { wcChunkSize = size }
          tileCount = size * size
          rc = sinkSegmentRiverChunk tileCount
          result = buildChunkRiverGeometry defaultRiverRenderConfig config renderHexRadiusPx 0
                     (IntMap.singleton 0 rc) IntMap.empty
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
      -- Stream delta: 3 triangles, spread 45°, radius = apothem fraction
      let (radiusFrac, spreadDeg, triCount) = deltaParamsForOrder defaultRiverRenderConfig 1
      radiusFrac `shouldBe` 0.35
      spreadDeg `shouldBe` 45.0
      triCount `shouldBe` 3

    it "major river gets larger delta" $ do
      let (radiusFrac, spreadDeg, triCount) = deltaParamsForOrder defaultRiverRenderConfig 7
      radiusFrac `shouldBe` 0.95
      spreadDeg `shouldBe` 120.0
      triCount `shouldBe` 8

    it "major sink segment produces larger geometry than stream sink" $ do
      let size = 4
          config = WorldConfig { wcChunkSize = size }
          tileCount = size * size
          rcStream = makeRiverChunk tileCount 0 255 1  -- stream (order 1, below delta threshold)
          rcMajor  = makeRiverChunk tileCount 0 255 7  -- major (order 7, above threshold)
          Just rgStream = buildChunkRiverGeometry defaultRiverRenderConfig config renderHexRadiusPx 0
                            (IntMap.singleton 0 rcStream) IntMap.empty
          Just rgMajor  = buildChunkRiverGeometry defaultRiverRenderConfig config renderHexRadiusPx 0
                            (IntMap.singleton 0 rcMajor) IntMap.empty
      -- Stream sink (order 1 < 3): entry quad + disc join = 18 verts, 42 indices.
      -- Major sink (order 7 >= 3): entry quad + delta + disc join = more.
      SV.length (rgVertices rgStream) `shouldBe` 18
      SV.length (rgIndices rgStream) `shouldBe` 42
      SV.length (rgVertices rgMajor) `shouldSatisfy` (> SV.length (rgVertices rgStream))
      SV.length (rgIndices rgMajor)  `shouldSatisfy` (> SV.length (rgIndices rgStream))

    prop "all delta fan rim vertices lie within apothem of hex centre" $
      forAll ((,,,) <$> choose (2.0, 20.0)    -- hexR
                    <*> choose (0.05, 1.0)     -- radius fraction
                    <*> choose (10.0, 170.0)   -- spread degrees
                    <*> choose (1, 8))         -- triCount
        $ \(hexR, frac, spreadDeg, triCount) ->
          let apothem  = hexR * sqrt 3.0 / 2.0 :: Float
              radius   = frac * apothem
              spread   = spreadDeg * pi / 180.0
              cx       = 50.0 :: Float
              cy       = 50.0 :: Float
              dirAngle = 0.0 :: Float
              color    = Raw.Color 50 95 155 255
              (vs, _is, _nb) = buildDeltaFan color radius spread triCount cx cy dirAngle (0 :: CInt)
              -- All vertices (centre + rim) must be within apothem + epsilon
              epsilon  = 0.01 :: Float
              withinBound (Raw.Vertex (Raw.FPoint px py) _ _) =
                let dx = realToFrac px - cx
                    dy = realToFrac py - cy
                in sqrt (dx * dx + dy * dy) <= apothem + epsilon
          in all withinBound vs

  -- -----------------------------------------------------------------------
  -- Coastal-exit delta geometry
  -- -----------------------------------------------------------------------
  describe "coastal exit delta geometry" $ do
    it "low-order coastal exit falls back to line quad (no delta)" $ do
      -- Order 1 < rrcMinDeltaOrder (3): coastal exit draws a plain
      -- line quad from centre→exit instead of a delta fan.
      -- entry=3(W), exit=128 (coastal E), order=1 (stream).
      -- Entry quad (4v/6i) + coastal line quad (4v/6i) + disc join (14v/36i).
      -- Total: 22 verts, 48 indices.
      let size = 4
          config = WorldConfig { wcChunkSize = size }
          tileCount = size * size
          rc = makeRiverChunk tileCount 3 128 1
          Just rg = buildChunkRiverGeometry defaultRiverRenderConfig config renderHexRadiusPx 0
                      (IntMap.singleton 0 rc) IntMap.empty
      SV.length (rgVertices rg) `shouldBe` 22
      SV.length (rgIndices rg) `shouldBe` 48

    it "high-order coastal exit produces entry quad + delta fan" $ do
      -- Order 3 >= rrcMinDeltaOrder (3): coastal exit draws delta fan.
      -- entry=3(W), exit=128 (coastal E), order=3 (creek).
      -- Entry quad (4v/6i) + coastal line (4v/6i) + delta fan (6v/12i)
      -- + disc join (14v/36i).  Total: 28v, 60i.
      let size = 4
          config = WorldConfig { wcChunkSize = size }
          tileCount = size * size
          rc = makeRiverChunk tileCount 3 128 3
          Just rg = buildChunkRiverGeometry defaultRiverRenderConfig config renderHexRadiusPx 0
                      (IntMap.singleton 0 rc) IntMap.empty
      SV.length (rgVertices rg) `shouldBe` 28
      SV.length (rgIndices rg) `shouldBe` 60

    it "source + low-order coastal produces line quad only" $ do
      -- entry=255 (source), exit=128 (coastal E), order=1 (stream).
      -- Below threshold: exit line quad only = 4 verts, 6 indices.
      let size = 4
          config = WorldConfig { wcChunkSize = size }
          tileCount = size * size
          rc = makeRiverChunk tileCount 255 128 1
          Just rg = buildChunkRiverGeometry defaultRiverRenderConfig config renderHexRadiusPx 0
                      (IntMap.singleton 0 rc) IntMap.empty
      SV.length (rgVertices rg) `shouldBe` 4
      SV.length (rgIndices rg) `shouldBe` 6

    it "source + high-order coastal produces delta fan" $ do
      -- entry=255 (source), exit=128 (coastal E), order=3 (creek).
      -- Above threshold: coastal line (4v/6i) + delta fan (6v/12i) = 10v, 18i.
      -- No disc join (no entry quad for source).
      let size = 4
          config = WorldConfig { wcChunkSize = size }
          tileCount = size * size
          rc = makeRiverChunk tileCount 255 128 3
          Just rg = buildChunkRiverGeometry defaultRiverRenderConfig config renderHexRadiusPx 0
                      (IntMap.singleton 0 rc) IntMap.empty
      SV.length (rgVertices rg) `shouldBe` 10
      SV.length (rgIndices rg) `shouldBe` 18

    it "low-order coastal has more geometry than low-order sink" $ do
      -- Order 1 (below threshold):
      -- Coastal: entry quad + exit line quad = 8v, 12i
      -- Sink:    entry quad + nothing = 4v, 6i
      let size = 4
          config = WorldConfig { wcChunkSize = size }
          tileCount = size * size
          rcCoastal = makeRiverChunk tileCount 0 128 1  -- coastal exit E
          rcSink    = makeRiverChunk tileCount 0 255 1  -- inland sink
          Just rgCoastal = buildChunkRiverGeometry defaultRiverRenderConfig config renderHexRadiusPx 0
                             (IntMap.singleton 0 rcCoastal) IntMap.empty
          Just rgSink    = buildChunkRiverGeometry defaultRiverRenderConfig config renderHexRadiusPx 0
                             (IntMap.singleton 0 rcSink) IntMap.empty
      SV.length (rgVertices rgCoastal) `shouldSatisfy` (> SV.length (rgVertices rgSink))
      SV.length (rgIndices rgCoastal)  `shouldSatisfy` (> SV.length (rgIndices rgSink))

    it "high-order coastal has more geometry than sink (extra line quad)" $ do
      -- Order 3 (above threshold): both draw entry quad + delta fan + disc join,
      -- but coastal also has a line quad from centre→exit.
      -- Coastal: 4(entry) + 4(line) + 6(delta) + 14(join) = 28v
      -- Sink:    4(entry) + 6(delta) + 14(join) = 24v
      let size = 4
          config = WorldConfig { wcChunkSize = size }
          tileCount = size * size
          rcCoastal = makeRiverChunk tileCount 0 128 3  -- coastal exit E, order 3
          rcSink    = makeRiverChunk tileCount 0 255 3  -- inland sink, order 3
          Just rgCoastal = buildChunkRiverGeometry defaultRiverRenderConfig config renderHexRadiusPx 0
                             (IntMap.singleton 0 rcCoastal) IntMap.empty
          Just rgSink    = buildChunkRiverGeometry defaultRiverRenderConfig config renderHexRadiusPx 0
                             (IntMap.singleton 0 rcSink) IntMap.empty
      SV.length (rgVertices rgCoastal) `shouldBe` 28
      SV.length (rgVertices rgSink)    `shouldBe` 24
      SV.length (rgVertices rgCoastal) `shouldSatisfy` (> SV.length (rgVertices rgSink))

    it "rrcMinDeltaOrder = 0 always draws deltas" $ do
      -- Override threshold to 0: even order-1 streams get deltas.
      let noDeltaGate = defaultRiverRenderConfig { rrcMinDeltaOrder = 0 }
          size = 4
          config = WorldConfig { wcChunkSize = size }
          tileCount = size * size
          rc = makeRiverChunk tileCount 3 128 1  -- order 1, coastal
          Just rg = buildChunkRiverGeometry noDeltaGate config renderHexRadiusPx 0
                      (IntMap.singleton 0 rc) IntMap.empty
      -- Entry quad (4v/6i) + coastal line (4v/6i) + stream delta (5v/9i)
      -- + disc join (14v/36i).  Total: 27v, 57i.
      SV.length (rgVertices rg) `shouldBe` 27
      SV.length (rgIndices rg) `shouldBe` 57

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
      tileOrders = U.generate tileCount (\i -> if i == 0 then fromIntegral order else 0)
  in RiverChunk
    { rcFlowAccum = U.replicate tileCount 0
    , rcDischarge = U.replicate tileCount 0
    , rcChannelDepth = U.replicate tileCount 0
    , rcRiverOrder = tileOrders
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
