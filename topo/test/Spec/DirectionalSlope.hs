{-# LANGUAGE ScopedTypeVariables #-}

module Spec.DirectionalSlope (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Data.Vector.Unboxed as U
import Topo

-- | Arbitrary instance for DirectionalSlope.
instance Arbitrary DirectionalSlope where
  arbitrary = DirectionalSlope
    <$> arbitrary <*> arbitrary <*> arbitrary
    <*> arbitrary <*> arbitrary <*> arbitrary

-- | Generate a DirectionalSlope with all non-negative components.
genNonNegSlope :: Gen DirectionalSlope
genNonNegSlope = DirectionalSlope
  <$> (getNonNegative <$> arbitrary)
  <*> (getNonNegative <$> arbitrary)
  <*> (getNonNegative <$> arbitrary)
  <*> (getNonNegative <$> arbitrary)
  <*> (getNonNegative <$> arbitrary)
  <*> (getNonNegative <$> arbitrary)

spec :: Spec
spec = describe "DirectionalSlope" $ do

  ---------------------------------------------------------------------------
  -- zeroDirSlope
  ---------------------------------------------------------------------------
  describe "zeroDirSlope" $ do
    it "has all fields equal to 0" $ do
      dsSlopeE  zeroDirSlope `shouldBe` 0
      dsSlopeNE zeroDirSlope `shouldBe` 0
      dsSlopeNW zeroDirSlope `shouldBe` 0
      dsSlopeW  zeroDirSlope `shouldBe` 0
      dsSlopeSW zeroDirSlope `shouldBe` 0
      dsSlopeSE zeroDirSlope `shouldBe` 0

    it "dsAvgSlope is 0" $
      dsAvgSlope zeroDirSlope `shouldBe` 0

    it "dsMaxSlope is 0" $
      dsMaxSlope zeroDirSlope `shouldBe` 0

    it "dsMinSlope is 0" $
      dsMinSlope zeroDirSlope `shouldBe` 0

    it "dsAsymmetry is 0" $
      dsAsymmetry zeroDirSlope `shouldBe` 0

  ---------------------------------------------------------------------------
  -- Derived scalar properties
  ---------------------------------------------------------------------------
  describe "dsAvgSlope" $ do
    prop "is non-negative" $ \(ds :: DirectionalSlope) ->
      dsAvgSlope ds >= 0

    prop "equals abs k for uniform slope k" $ \(k :: Float) ->
      let ds = DirectionalSlope k k k k k k
      in abs (dsAvgSlope ds - abs k) < 1e-5

  describe "dsMaxSlope" $ do
    prop "is non-negative" $ \(ds :: DirectionalSlope) ->
      dsMaxSlope ds >= 0

    prop "is >= dsAvgSlope" $ \(ds :: DirectionalSlope) ->
      dsMaxSlope ds >= dsAvgSlope ds - 1e-6

    prop "is >= dsMinSlope" $ \(ds :: DirectionalSlope) ->
      dsMaxSlope ds >= dsMinSlope ds - 1e-6

  describe "dsMinSlope" $ do
    prop "is non-negative" $ \(ds :: DirectionalSlope) ->
      dsMinSlope ds >= 0

    prop "is <= dsMaxSlope" $ \(ds :: DirectionalSlope) ->
      dsMinSlope ds <= dsMaxSlope ds + 1e-6

  describe "dsAsymmetry" $ do
    prop "is non-negative" $ \(ds :: DirectionalSlope) ->
      dsAsymmetry ds >= -1e-6

    prop "is 0 for uniform slope" $ \(k :: Float) ->
      let ds = DirectionalSlope k k k k k k
      in abs (dsAsymmetry ds) < 1e-5

    prop "equals dsMaxSlope - dsMinSlope" $ \(ds :: DirectionalSlope) ->
      abs (dsAsymmetry ds - (dsMaxSlope ds - dsMinSlope ds)) < 1e-5

  ---------------------------------------------------------------------------
  -- dsSlopeIn round-trip with record fields
  ---------------------------------------------------------------------------
  describe "dsSlopeIn consistency" $ do
    prop "dsSlopeIn HexE == dsSlopeE" $ \(ds :: DirectionalSlope) ->
      dsSlopeIn HexE ds === dsSlopeE ds
    prop "dsSlopeIn HexNE == dsSlopeNE" $ \(ds :: DirectionalSlope) ->
      dsSlopeIn HexNE ds === dsSlopeNE ds
    prop "dsSlopeIn HexNW == dsSlopeNW" $ \(ds :: DirectionalSlope) ->
      dsSlopeIn HexNW ds === dsSlopeNW ds
    prop "dsSlopeIn HexW == dsSlopeW" $ \(ds :: DirectionalSlope) ->
      dsSlopeIn HexW ds === dsSlopeW ds
    prop "dsSlopeIn HexSW == dsSlopeSW" $ \(ds :: DirectionalSlope) ->
      dsSlopeIn HexSW ds === dsSlopeSW ds
    prop "dsSlopeIn HexSE == dsSlopeSE" $ \(ds :: DirectionalSlope) ->
      dsSlopeIn HexSE ds === dsSlopeSE ds

  ---------------------------------------------------------------------------
  -- dsSteepestDescent properties
  ---------------------------------------------------------------------------
  describe "dsSteepestDescent" $ do
    prop "returns Nothing when all slopes >= 0" $
      forAll genNonNegSlope $ \ds ->
        dsSteepestDescent ds === Nothing

    it "finds the correct direction for a single downhill" $ do
      let ds = DirectionalSlope 0.1 0.2 0.3 0.4 (-0.8) 0.1
      dsSteepestDescent ds `shouldBe` Just HexSW

    prop "returned direction has negative slope" $ \(ds :: DirectionalSlope) ->
      case dsSteepestDescent ds of
        Nothing -> True
        Just d  -> dsSlopeIn d ds < 0

    prop "returned direction has the most negative slope" $ \(ds :: DirectionalSlope) ->
      case dsSteepestDescent ds of
        Nothing -> True
        Just d  ->
          let s = dsSlopeIn d ds
          in all (\d' -> dsSlopeIn d' ds >= s) allHexDirections

  ---------------------------------------------------------------------------
  -- Eq instance
  ---------------------------------------------------------------------------
  describe "Eq" $ do
    prop "reflexive" $ \(ds :: DirectionalSlope) ->
      ds === ds

    it "zeroDirSlope equals itself" $
      zeroDirSlope `shouldBe` zeroDirSlope

  ---------------------------------------------------------------------------
  -- Serialization roundtrip (via encodeTerrainChunk / decodeTerrainChunk)
  ---------------------------------------------------------------------------
  describe "serialization roundtrip" $ do
    it "encodeTerrainChunk / decodeTerrainChunk preserves tcDirSlope" $ do
      let config = WorldConfig { wcChunkSize = 4 }
          n = 16 -- 4*4
          slopeVec = U.fromList
            [ DirectionalSlope (fromIntegral i * 0.01) (fromIntegral i * 0.02)
                               (fromIntegral i * 0.03) (fromIntegral i * 0.04)
                               (fromIntegral i * 0.05) (fromIntegral i * 0.06)
            | i <- [0 .. n - 1 :: Int]
            ]
          chunk = (emptyTerrainChunk config) { tcDirSlope = slopeVec }
      case encodeTerrainChunk config chunk of
        Left err -> expectationFailure ("encode failed: " <> show err)
        Right encoded ->
          case decodeTerrainChunk config encoded of
            Left err -> expectationFailure ("decode failed: " <> show err)
            Right decoded ->
              U.toList (tcDirSlope decoded) `shouldBe` U.toList slopeVec
