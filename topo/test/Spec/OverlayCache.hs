{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spec.OverlayCache (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.List (nub)
import qualified Data.Text as Text

import Topo.Overlay
  ( OverlayChunk(..)
  , OverlayRecord(..)
  , OverlayValue(..)
  , emptyOverlayChunk
  )
import Topo.Overlay.Cache
  ( OverlayCacheKey(..)
  , cacheClearState
  , cacheDeleteState
  , cacheInsertState
  , cacheLookupState
  , emptyOverlayCacheState
  )
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Vector as V

spec :: Spec
spec = describe "Overlay.Cache" $ do
  it "insert then lookup returns cached chunk" $ do
    let key = OverlayCacheKey "weather" 3
        chunk = sampleChunk
        state1 = cacheInsertState key chunk emptyOverlayCacheState
    cacheLookupState key state1 `shouldBe` Just chunk

  it "delete removes a cached entry" $ do
    let key = OverlayCacheKey "weather" 4
        state1 = cacheInsertState key sampleChunk emptyOverlayCacheState
        state2 = cacheDeleteState key state1
    cacheLookupState key state2 `shouldBe` Nothing

  it "clear removes all entries" $ do
    let keyA = OverlayCacheKey "weather" 1
        keyB = OverlayCacheKey "civilization" 2
        state1 = cacheInsertState keyA sampleChunk
               $ cacheInsertState keyB sampleChunk emptyOverlayCacheState
        state2 = cacheClearState state1
    cacheLookupState keyA state2 `shouldBe` Nothing
    cacheLookupState keyB state2 `shouldBe` Nothing

  it "last insert wins for the same key" $ do
    let key = OverlayCacheKey "weather" 7
        chunkA = sampleChunk
        chunkB = emptyOverlayChunk
        state1 = cacheInsertState key chunkA emptyOverlayCacheState
        state2 = cacheInsertState key chunkB state1
    cacheLookupState key state2 `shouldBe` Just chunkB

  it "property: all inserted unique keys are retrievable" $
    property $ \(overlayNames :: [String], chunkIds :: [NonNegative Int]) ->
      let keys = take 20 [ OverlayCacheKey (Text.pack name) cid
                         | (name, NonNegative cid) <- zip overlayNames chunkIds
                         , not (null name)
                         ]
          uniqueKeys = nub keys
          state = foldr (\k st -> cacheInsertState k sampleChunk st) emptyOverlayCacheState uniqueKeys
      in all (\k -> cacheLookupState k state == Just sampleChunk) uniqueKeys

sampleChunk :: OverlayChunk
sampleChunk =
  let rec = OverlayRecord (V.fromList [OVFloat 1.0])
  in OverlayChunk (IntMap.singleton 0 rec)
