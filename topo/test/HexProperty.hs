module HexProperty (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Topo

spec :: Spec
spec = describe "Hex properties" $ do
  prop "tileIndex roundtrip" $ \x y ->
    let size = 16
        config = WorldConfig { wcChunkSize = size }
        coord = TileCoord (abs x `mod` size) (abs y `mod` size)
    in case tileIndex config coord of
        Nothing -> False
        Just idx -> tileCoordFromIndex config idx == coord
