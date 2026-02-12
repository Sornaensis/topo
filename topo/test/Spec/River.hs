{-# LANGUAGE PatternSynonyms #-}

module Spec.River (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.Word (Word8, Word16)
import qualified Data.Vector.Unboxed as U
import Topo.River
  ( RiverTopologyConfig(..)
  , defaultRiverTopologyConfig
  , computeRiverSegments
  , gridDirToHexEdge
  , oppositeEdge
  )
import Topo.Types
  ( HexEdge(..)
  , RiverSize(..)
  , pattern EdgeE
  , pattern EdgeW
  , pattern EdgeNE
  , pattern EdgeNW
  , pattern EdgeSW
  , pattern EdgeSE
  , pattern EdgeNone
  , pattern RiverStream
  , pattern RiverCreek
  , pattern RiverRiver
  , pattern RiverMajor
  , riverSizeFromOrder
  )

spec :: Spec
spec = describe "River Topology" $ do

  -- -----------------------------------------------------------------------
  -- 9.4.1: 3-tile linear chain A->B->C produces correct entry/exit edges
  -- -----------------------------------------------------------------------
  describe "linear chain A->B->C" $ do
    it "produces segments with correct entry/exit edges" $ do
      -- 3x1 grid: A(0)->B(1)->C(2)
      -- flow: [1, 2, -1]
      -- discharge above min threshold for all: 5.0, 10.0, 15.0
      -- order: 1, 1, 1
      let gridW = 3
          gridH = 1
          flow      = U.fromList [1, 2, -1 :: Int]
          discharge = U.fromList [5.0, 10.0, 15.0 :: Float]
          order     = U.fromList [1, 1, 1 :: Word16]
          elev      = U.replicate 3 (0.8 :: Float)
          wl        = 0.5 :: Float
          cfg       = defaultRiverTopologyConfig { rtMinDischarge = 1.0, rtMinNetworkTiles = 1 }
          (offsets, entry, exit, segDisc, segOrd) =
            computeRiverSegments cfg gridW gridH flow discharge order elev wl

      -- Tile 0 (headwater, flows right to 1):
      -- exit edge = E (right on grid -> edge 0)
      -- entry = 255 (source)
      let seg0Start = offsets U.! 0
          seg0End   = offsets U.! 1
      (seg0End - seg0Start) `shouldSatisfy` (>= 1)
      entry U.! seg0Start `shouldBe` 255    -- source
      exit  U.! seg0Start `shouldBe` 0      -- East

      -- Tile 1 (receives from tile 0, flows right to 2):
      -- exit edge = E (0)
      -- entry edge = opposite of tile 0's exit to tile 1 = opposite(E) = W (3)
      let seg1Start = offsets U.! 1
          seg1End   = offsets U.! 2
      (seg1End - seg1Start) `shouldSatisfy` (>= 1)
      entry U.! seg1Start `shouldBe` 3      -- West (opposite of East)
      exit  U.! seg1Start `shouldBe` 0      -- East

      -- Tile 2 (receives from tile 1, sink):
      -- entry edge = opposite(E) = W (3)
      -- exit = 255 (sink)
      let seg2Start = offsets U.! 2
          seg2End   = offsets U.! 3
      (seg2End - seg2Start) `shouldSatisfy` (>= 1)
      entry U.! seg2Start `shouldBe` 3      -- West
      exit  U.! seg2Start `shouldBe` 255    -- sink

  -- -----------------------------------------------------------------------
  -- 9.4.2: Confluence A->C, B->C, C->D produces two segments at tile C
  -- -----------------------------------------------------------------------
  describe "confluence A->C, B->C, C->D" $ do
    it "produces two segments at the confluence tile" $ do
      -- 4x1 grid: A(0)->C(2), B(1)->C(2), C(2)->D(3)
      -- But flow must be index-based: flow[0]=2, flow[1]=2, flow[2]=3, flow[3]=-1
      -- However, tile 0 flowing to tile 2 skips tile 1 — that's not a cardinal
      -- neighbor in a 4x1 grid (d=2, not adjacent). Let's use a 2x2 grid instead.
      --
      -- 2x2 grid:
      --   (0,0)=0  (1,0)=1
      --   (0,1)=2  (1,1)=3
      --
      -- Flow: 0->1, 2->3, 1->3, 3->-1
      -- So tile 3 receives from both tile 1 and tile 2 (confluence)
      let gridW = 2
          gridH = 2
          flow      = U.fromList [1, 3, 3, -1 :: Int]
          discharge = U.fromList [3.0, 6.0, 3.0, 12.0 :: Float]
          order     = U.fromList [1, 1, 1, 2 :: Word16]
          elev      = U.replicate 4 (0.8 :: Float)
          wl        = 0.5 :: Float
          cfg       = defaultRiverTopologyConfig { rtMinDischarge = 1.0, rtMinNetworkTiles = 1 }
          (offsets, entry, exit, _, _) =
            computeRiverSegments cfg gridW gridH flow discharge order elev wl

      -- Tile 3 should have 2 segments (from tile 1 and tile 2)
      let seg3Start = offsets U.! 3
          seg3End   = offsets U.! 4
      (seg3End - seg3Start) `shouldBe` 2

  -- -----------------------------------------------------------------------
  -- 9.4.3: Every segment's exit edge is the opposite of the downstream
  -- tile's entry edge from the same segment
  -- -----------------------------------------------------------------------
  describe "exit-entry edge consistency" $ do
    prop "exit edge of upstream tile is opposite of entry edge at downstream tile" $
      -- Use a simple 4x1 linear chain to verify
      forAll (choose (2.0, 20.0)) $ \baseDisc ->
        let gridW = 4
            gridH = 1
            flow      = U.fromList [1, 2, 3, -1 :: Int]
            discharge = U.fromList [baseDisc, baseDisc * 2, baseDisc * 3, baseDisc * 4 :: Float]
            order     = U.fromList [1, 1, 1, 1 :: Word16]
            elev      = U.replicate 4 (0.8 :: Float)
            wl        = 0.5 :: Float
            cfg       = defaultRiverTopologyConfig { rtMinDischarge = 1.0, rtMinNetworkTiles = 1 }
            (offsets, entryVec, exitVec, _, _) =
              computeRiverSegments cfg gridW gridH flow discharge order elev wl
            -- For tiles 1,2,3 that have upstream: check that
            -- the downstream's entry == opposite(upstream's exit)
            checkTile i =
              let segStart = offsets U.! i
                  segEnd   = offsets U.! (i + 1)
                  segCount = segEnd - segStart
              in segCount >= 1
                   && (let entryE = HexEdge (entryVec U.! segStart)
                       in entryE == oppositeEdge (HexEdge (exitVec U.! (offsets U.! (i - 1)))))
        in checkTile 1 && checkTile 2

  -- -----------------------------------------------------------------------
  -- 9.4.4: Total segment count >= number of tiles with discharge >= threshold
  -- -----------------------------------------------------------------------
  describe "segment count" $ do
    prop "total segments >= tiles with discharge above threshold" $
      forAll (choose (1.0, 5.0)) $ \minDisc ->
        let gridW = 3
            gridH = 2
            n     = gridW * gridH
            flow      = U.fromList [1, 2, -1, 4, 5, -1 :: Int]
            discharge = U.fromList [3.0, 5.0, 8.0, 3.0, 5.0, 8.0 :: Float]
            order     = U.fromList [1, 1, 2, 1, 1, 2 :: Word16]
            elev      = U.replicate 6 (0.8 :: Float)
            wl        = 0.5 :: Float
            cfg       = defaultRiverTopologyConfig { rtMinDischarge = minDisc, rtMinNetworkTiles = 1 }
            (offsets, _, _, _, _) =
              computeRiverSegments cfg gridW gridH flow discharge order elev wl
            totalSegs  = offsets U.! n
            tilesAbove = U.length (U.filter (>= minDisc) discharge)
        in totalSegs >= tilesAbove

  -- -----------------------------------------------------------------------
  -- 11.3: Submerged tiles produce zero segments
  -- -----------------------------------------------------------------------
  describe "submerged tile filtering" $ do
    it "tiles at or below waterLevel produce no segments" $ do
      -- 3x1 grid: all tiles have high discharge but are submerged (elev <= wl)
      let gridW = 3
          gridH = 1
          flow      = U.fromList [1, 2, -1 :: Int]
          discharge = U.fromList [5.0, 10.0, 15.0 :: Float]
          order     = U.fromList [1, 1, 1 :: Word16]
          elev      = U.replicate 3 (0.3 :: Float)  -- below waterLevel
          wl        = 0.5 :: Float
          cfg       = defaultRiverTopologyConfig { rtMinDischarge = 1.0, rtMinNetworkTiles = 1 }
          (offsets, _, _, _, _) =
            computeRiverSegments cfg gridW gridH flow discharge order elev wl
          totalSegs = offsets U.! 3
      totalSegs `shouldBe` 0

    it "mixed land/ocean: only land tiles produce segments" $ do
      -- 4x1: tiles 0,1 are land (0.8), tiles 2,3 are ocean (0.3)
      -- flow: 0->1->2->3->-1, all discharge high
      let gridW = 4
          gridH = 1
          flow      = U.fromList [1, 2, 3, -1 :: Int]
          discharge = U.fromList [5.0, 10.0, 15.0, 20.0 :: Float]
          order     = U.fromList [1, 1, 1, 1 :: Word16]
          elev      = U.fromList [0.8, 0.8, 0.3, 0.3 :: Float]
          wl        = 0.5 :: Float
          cfg       = defaultRiverTopologyConfig { rtMinDischarge = 1.0, rtMinNetworkTiles = 1 }
          (offsets, _, _, _, _) =
            computeRiverSegments cfg gridW gridH flow discharge order elev wl
      -- Tiles 0 and 1 should have segments; tiles 2 and 3 should not
      let segs01 = offsets U.! 2  -- total segments for tiles 0-1
          segs23 = (offsets U.! 4) - (offsets U.! 2)
      segs01 `shouldSatisfy` (> 0)
      segs23 `shouldBe` 0

    prop "no segments when all tiles are submerged" $
      forAll (choose (0.0, 0.5)) $ \maxElev ->
        let gridW = 3
            gridH = 2
            n     = gridW * gridH
            flow      = U.fromList [1, 2, -1, 4, 5, -1 :: Int]
            discharge = U.fromList [3.0, 5.0, 8.0, 3.0, 5.0, 8.0 :: Float]
            order     = U.fromList [1, 1, 2, 1, 1, 2 :: Word16]
            elev      = U.replicate 6 maxElev
            wl        = 0.5 :: Float
            cfg       = defaultRiverTopologyConfig { rtMinDischarge = 1.0, rtMinNetworkTiles = 1 }
            (offsets, _, _, _, _) =
              computeRiverSegments cfg gridW gridH flow discharge order elev wl
            totalSegs = offsets U.! n
        in totalSegs == 0

  -- -----------------------------------------------------------------------
  -- 9.4.5: riverSizeFromOrder classification
  -- -----------------------------------------------------------------------
  describe "riverSizeFromOrder" $ do
    it "maps order 0 -> Stream" $
      riverSizeFromOrder 0 `shouldBe` RiverStream
    it "maps order 1 -> Stream" $
      riverSizeFromOrder 1 `shouldBe` RiverStream
    it "maps order 2 -> Stream" $
      riverSizeFromOrder 2 `shouldBe` RiverStream
    it "maps order 3 -> Creek" $
      riverSizeFromOrder 3 `shouldBe` RiverCreek
    it "maps order 4 -> River" $
      riverSizeFromOrder 4 `shouldBe` RiverRiver
    it "maps order 5 -> River" $
      riverSizeFromOrder 5 `shouldBe` RiverRiver
    it "maps order 6 -> Major" $
      riverSizeFromOrder 6 `shouldBe` RiverMajor
    it "maps order 7 -> Major" $
      riverSizeFromOrder 7 `shouldBe` RiverMajor

  -- -----------------------------------------------------------------------
  -- gridDirToHexEdge tests  
  -- -----------------------------------------------------------------------
  describe "gridDirToHexEdge" $ do
    it "right neighbor maps to East edge" $
      gridDirToHexEdge 4 0 1 `shouldBe` EdgeE
    it "left neighbor maps to West edge" $
      gridDirToHexEdge 4 1 0 `shouldBe` EdgeW
    it "dx=0, dy=-1 maps to NW (axial, no parity)" $
      -- (0,2)=8 -> (0,1)=4, dx=0, dy=-1 -> NW
      gridDirToHexEdge 4 8 4 `shouldBe` EdgeNW
    it "dx=0, dy=-1 maps to NW (any row)" $
      -- (0,1)=4 -> (0,0)=0, dx=0, dy=-1 -> NW
      gridDirToHexEdge 4 4 0 `shouldBe` EdgeNW
    it "dx=0, dy=+1 maps to SE (axial, no parity)" $
      -- (0,0)=0 -> (0,1)=4, dx=0, dy=+1 -> SE
      gridDirToHexEdge 4 0 4 `shouldBe` EdgeSE
    it "dx=0, dy=+1 maps to SE (any row)" $
      -- (0,1)=4 -> (0,2)=8, dx=0, dy=+1 -> SE
      gridDirToHexEdge 4 4 8 `shouldBe` EdgeSE
    it "dx=1, dy=-1 maps to NE" $
      -- (1,1)=5 -> (2,0)=2, dx=1, dy=-1 -> NE
      gridDirToHexEdge 4 5 2 `shouldBe` EdgeNE
    it "dx=-1, dy=+1 maps to SW" $
      -- (2,0)=2 -> (1,1)=5, dx=-1, dy=+1 -> SW
      gridDirToHexEdge 4 2 5 `shouldBe` EdgeSW
    it "non-adjacent tiles return EdgeNone" $
      gridDirToHexEdge 4 0 5 `shouldBe` EdgeNone  -- (0,0)->(1,1), dx=1 dy=1

  describe "oppositeEdge" $ do
    it "E <-> W" $ do
      oppositeEdge EdgeE  `shouldBe` EdgeW
      oppositeEdge EdgeW  `shouldBe` EdgeE
    it "NE <-> SW" $ do
      oppositeEdge EdgeNE `shouldBe` EdgeSW
      oppositeEdge EdgeSW `shouldBe` EdgeNE
    it "NW <-> SE" $ do
      oppositeEdge EdgeNW `shouldBe` EdgeSE
      oppositeEdge EdgeSE `shouldBe` EdgeNW
    it "EdgeNone -> EdgeNone" $
      oppositeEdge EdgeNone `shouldBe` EdgeNone
  -- -----------------------------------------------------------------------
  -- 12.4.2: Network pruning — short networks produce zero segments
  -- -----------------------------------------------------------------------
  describe "network pruning (rtMinNetworkTiles)" $ do
    it "prunes a 3-tile network when minNetworkTiles = 5" $ do
      -- 3x1 linear chain: 0->1->2->sink.  Only 3 tiles.
      let gridW = 3
          gridH = 1
          flow      = U.fromList [1, 2, -1 :: Int]
          discharge = U.fromList [5.0, 10.0, 15.0 :: Float]
          order     = U.fromList [1, 1, 1 :: Word16]
          elev      = U.replicate 3 (0.8 :: Float)
          wl        = 0.5 :: Float
          cfg       = defaultRiverTopologyConfig
                        { rtMinDischarge = 1.0, rtMinNetworkTiles = 5 }
          (offsets, _, _, _, _) =
            computeRiverSegments cfg gridW gridH flow discharge order elev wl
      -- Network has 3 tiles < 5, so all segments should be pruned.
      (offsets U.! 3) `shouldBe` 0

    it "retains a 5-tile network when minNetworkTiles = 5" $ do
      -- 5x1 linear chain: 0->1->2->3->4->sink
      let gridW = 5
          gridH = 1
          flow      = U.fromList [1, 2, 3, 4, -1 :: Int]
          discharge = U.fromList [3.0, 5.0, 8.0, 12.0, 16.0 :: Float]
          order     = U.fromList [1, 1, 1, 1, 1 :: Word16]
          elev      = U.replicate 5 (0.8 :: Float)
          wl        = 0.5 :: Float
          cfg       = defaultRiverTopologyConfig
                        { rtMinDischarge = 1.0, rtMinNetworkTiles = 5 }
          (offsets, _, _, _, _) =
            computeRiverSegments cfg gridW gridH flow discharge order elev wl
      -- Network has exactly 5 tiles >= 5, so all segments should be kept.
      (offsets U.! 5) `shouldSatisfy` (> 0)

    it "prunes only the short network in a mixed grid" $ do
      -- 5x2 grid: two independent networks
      -- Row 0: 0->1->2->3->4->sink (5 tiles, retained)
      -- Row 1: 5->6->sink (2 tiles, pruned)  tiles 7,8,9 not in any network
      let gridW = 5
          gridH = 2
          flow      = U.fromList [ 1,  2,   3,   4,  -1    -- row 0
                                 , 6, -1,  -1,  -1,  -1 :: Int]  -- row 1
          discharge = U.fromList [3.0, 5.0, 8.0, 12.0, 16.0    -- row 0
                                 ,3.0, 5.0, 0.0,  0.0,  0.0 :: Float]  -- row 1
          order     = U.fromList [1, 1, 1, 1, 1
                                 ,1, 1, 0, 0, 0 :: Word16]
          elev      = U.replicate 10 (0.8 :: Float)
          wl        = 0.5 :: Float
          cfg       = defaultRiverTopologyConfig
                        { rtMinDischarge = 1.0, rtMinNetworkTiles = 3 }
          (offsets, _, _, _, _) =
            computeRiverSegments cfg gridW gridH flow discharge order elev wl
      -- Row 0 network: 5 tiles >= 3 → retained
      let row0Segs = offsets U.! 5 - offsets U.! 0
      row0Segs `shouldSatisfy` (> 0)
      -- Row 1 network: 2 tiles < 3 → pruned
      let row1Segs = offsets U.! 10 - offsets U.! 5
      row1Segs `shouldBe` 0

    prop "no network shorter than rtMinNetworkTiles produces segments" $
      forAll (choose (2, 6)) $ \minNet ->
        -- Build a single linear chain of 1..minNet-1 tiles and verify it's pruned
        forAll (choose (1, minNet - 1)) $ \chainLen ->
          let gridW = max chainLen 1
              gridH = 1
              flow      = U.generate gridW (\i -> if i + 1 < gridW then i + 1 else -1)
              discharge = U.generate gridW (\i -> fromIntegral (i + 2) :: Float)
              order     = U.replicate gridW (1 :: Word16)
              elev      = U.replicate gridW (0.8 :: Float)
              wl        = 0.5 :: Float
              cfg       = defaultRiverTopologyConfig
                            { rtMinDischarge = 1.0, rtMinNetworkTiles = minNet }
              (offsets, _, _, _, _) =
                computeRiverSegments cfg gridW gridH flow discharge order elev wl
          in offsets U.! gridW == 0

    it "pruning disabled when rtMinNetworkTiles = 1" $ do
      let gridW = 2
          gridH = 1
          flow      = U.fromList [1, -1 :: Int]
          discharge = U.fromList [5.0, 10.0 :: Float]
          order     = U.fromList [1, 1 :: Word16]
          elev      = U.replicate 2 (0.8 :: Float)
          wl        = 0.5 :: Float
          cfg       = defaultRiverTopologyConfig
                        { rtMinDischarge = 1.0, rtMinNetworkTiles = 1 }
          (offsets, _, _, _, _) =
            computeRiverSegments cfg gridW gridH flow discharge order elev wl
      (offsets U.! 2) `shouldSatisfy` (> 0)