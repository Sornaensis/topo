module Spec.AtlasCache (spec) where

import Test.Hspec
import Data.Maybe (isNothing)
import qualified Data.IntMap.Strict as IntMap
import Foreign.Ptr (Ptr, intPtrToPtr)
import Linear (V2(..))
import qualified SDL
import Unsafe.Coerce (unsafeCoerce)
import Actor.AtlasCache (AtlasKey(..))
import Actor.UI (ViewMode(..))
import Seer.Render.Atlas
  ( AtlasTextureCache(..)
  , emptyAtlasTextureCache
  , setAtlasKey
  , storeAtlasTiles
  , getNearestAtlas
  , touchAtlasScale
  , drainAtlasPending
  , evictIfNeeded
  , resolveEffectiveStage
  , collectAtlasTextures
  )
import Seer.Render.ZoomStage (ZoomStage(..))
import UI.TerrainAtlas (TerrainAtlasTile(..))
import UI.Widgets (Rect(..))

-- | Create a distinguishable mock SDL.Texture from an integer tag.
-- These MUST NOT be passed to any real SDL function.
mockTexture :: Int -> SDL.Texture
mockTexture n = unsafeCoerce (intPtrToPtr (fromIntegral n) :: Ptr ())

-- | Build a test tile with a given texture tag, hexRadius, and bounds.
mkTile :: Int -> Int -> Rect -> TerrainAtlasTile
mkTile tag hr bounds = TerrainAtlasTile
  { tatTexture   = mockTexture tag
  , tatBounds    = bounds
  , tatScale     = 1
  , tatHexRadius = hr
  }

-- | Standard test rect.
testRect :: Rect
testRect = Rect (V2 0 0, V2 64 64)

-- | Convenience: different rect for second tile.
testRect2 :: Rect
testRect2 = Rect (V2 64 0, V2 64 64)

-- | Two keys that differ only in view mode.
keyA, keyB :: AtlasKey
keyA = AtlasKey ViewElevation 0 False 1
keyB = AtlasKey ViewBiome     0 False 1

-- | Key that differs in day/night flag.
keyDayNight :: AtlasKey
keyDayNight = AtlasKey ViewElevation 0 True 1

spec :: Spec
spec = describe "AtlasTextureCache" $ do

  -- -------------------------------------------------------------------
  -- setAtlasKey
  -- -------------------------------------------------------------------
  describe "setAtlasKey" $ do
    it "is idempotent for the same key" $ do
      let cache0 = (emptyAtlasTextureCache 4) { atcKey = Just keyA }
          cache1 = setAtlasKey keyA cache0
      atcKey cache1 `shouldBe` atcKey cache0
      IntMap.size (atcCaches cache1) `shouldBe` IntMap.size (atcCaches cache0)
      length (atcPending cache1) `shouldBe` length (atcPending cache0)

    it "clears atcCaches on key change" $ do
      let tiles  = [mkTile 1 6 testRect]
          cache0 = (emptyAtlasTextureCache 4) { atcKey = Just keyA, atcCaches = IntMap.singleton 6 tiles }
          cache1 = setAtlasKey keyB cache0
      atcKey cache1 `shouldBe` Just keyB
      IntMap.null (atcCaches cache1) `shouldBe` True

    it "moves cache textures to atcPending on key change" $ do
      let tiles  = [mkTile 1 6 testRect, mkTile 2 6 testRect2]
          cache0 = (emptyAtlasTextureCache 4) { atcKey = Just keyA, atcCaches = IntMap.singleton 6 tiles }
          cache1 = setAtlasKey keyB cache0
      length (atcPending cache1) `shouldBe` 2

    it "preserves atcLast on key change" $ do
      let lastTiles = [mkTile 10 6 testRect]
          cache0 = (emptyAtlasTextureCache 4)
            { atcKey  = Just keyA
            , atcCaches = IntMap.empty
            , atcLast = Just (keyA, lastTiles)
            }
          cache1 = setAtlasKey keyB cache0
      fmap fst (atcLast cache1) `shouldBe` Just keyA
      fmap (length . snd) (atcLast cache1) `shouldBe` Just 1

    -- Regression: use-after-free when atcLast tiles are aliased with atcCaches
    it "does NOT move atcLast textures to atcPending (no aliasing)" $ do
      let sharedTiles = [mkTile 100 6 testRect, mkTile 101 6 testRect2]
          cache0 = (emptyAtlasTextureCache 4)
            { atcKey  = Just keyA
            , atcCaches = IntMap.singleton 6 sharedTiles
            , atcLast = Just (keyA, sharedTiles)  -- aliased!
            }
          cache1 = setAtlasKey keyB cache0
      -- The textures from atcLast (100, 101) must NOT appear in atcPending
      -- because atcLast still references them for fallback display.
      null (atcPending cache1) `shouldBe` True

    it "moves only non-aliased cache textures to pending" $ do
      let lastTile  = mkTile 100 6 testRect
          cacheTile = mkTile 200 10 testRect2
          cache0 = (emptyAtlasTextureCache 4)
            { atcKey  = Just keyA
            , atcCaches = IntMap.fromList [(6, [lastTile]), (10, [cacheTile])]
            , atcLast = Just (keyA, [lastTile])
            }
          cache1 = setAtlasKey keyB cache0
      -- Only cacheTile (200) should be pending; lastTile (100) is protected
      map (\t -> t == mockTexture 200) (atcPending cache1) `shouldBe` [True]

  -- -------------------------------------------------------------------
  -- storeAtlasTiles
  -- -------------------------------------------------------------------
  describe "storeAtlasTiles" $ do
    it "stores tiles when key matches" $ do
      let cache0 = (emptyAtlasTextureCache 4) { atcKey = Just keyA }
          tiles  = [mkTile 1 6 testRect]
          cache1 = storeAtlasTiles keyA 6 tiles cache0
      IntMap.member 6 (atcCaches cache1) `shouldBe` True

    it "discards tiles to pending when key mismatches" $ do
      let cache0 = (emptyAtlasTextureCache 4) { atcKey = Just keyA }
          tiles  = [mkTile 50 10 testRect]
          cache1 = storeAtlasTiles keyB 10 tiles cache0
      IntMap.null (atcCaches cache1) `shouldBe` True
      length (atcPending cache1) `shouldBe` 1

    it "sets atcKey when storing into empty cache" $ do
      let cache0 = emptyAtlasTextureCache 4
          tiles  = [mkTile 1 6 testRect]
          cache1 = storeAtlasTiles keyA 6 tiles cache0
      atcKey cache1 `shouldBe` Just keyA

  -- -------------------------------------------------------------------
  -- getNearestAtlas
  -- -------------------------------------------------------------------
  describe "getNearestAtlas" $ do
    it "returns exact scale match" $ do
      let tiles6  = [mkTile 1 6 testRect]
          tiles10 = [mkTile 2 10 testRect]
          cache   = (emptyAtlasTextureCache 4)
            { atcKey = Just keyA
            , atcCaches = IntMap.fromList [(6, tiles6), (10, tiles10)]
            }
          result = getNearestAtlas keyA 6 cache
      fmap length result `shouldBe` Just 1
      fmap (tatHexRadius . head) result `shouldBe` Just 6

    it "returns nearest scale when exact is missing" $ do
      let tiles8  = [mkTile 1 8 testRect]
          tiles20 = [mkTile 2 20 testRect]
          cache   = (emptyAtlasTextureCache 4)
            { atcKey = Just keyA
            , atcCaches = IntMap.fromList [(8, tiles8), (20, tiles20)]
            }
          result = getNearestAtlas keyA 10 cache
      fmap length result `shouldBe` Just 1
      fmap (tatHexRadius . head) result `shouldBe` Just 8

    it "returns Nothing on key mismatch" $ do
      let tiles = [mkTile 1 6 testRect]
          cache = (emptyAtlasTextureCache 4)
            { atcKey = Just keyA
            , atcCaches = IntMap.singleton 6 tiles
            }
      isNothing (getNearestAtlas keyB 6 cache) `shouldBe` True

  -- -------------------------------------------------------------------
  -- touchAtlasScale
  -- -------------------------------------------------------------------
  describe "touchAtlasScale" $ do
    it "moves scale to front of LRU" $ do
      let cache0 = (emptyAtlasTextureCache 4) { atcLru = [6, 10, 18] }
          cache1 = touchAtlasScale 10 cache0
      atcLru cache1 `shouldBe` [10, 6, 18]

    it "adds new scale to LRU" $ do
      let cache0 = (emptyAtlasTextureCache 4) { atcLru = [6] }
          cache1 = touchAtlasScale 10 cache0
      atcLru cache1 `shouldBe` [10, 6]

  -- -------------------------------------------------------------------
  -- drainAtlasPending
  -- -------------------------------------------------------------------
  describe "drainAtlasPending" $ do
    it "returns pending textures and empties the list" $ do
      let texs   = [mockTexture 1, mockTexture 2]
          cache0 = (emptyAtlasTextureCache 4) { atcPending = texs }
          (drained, cache1) = drainAtlasPending cache0
      length drained `shouldBe` 2
      all (`elem` texs) drained `shouldBe` True
      null (atcPending cache1) `shouldBe` True

    it "returns empty list when no textures are pending" $ do
      let (drained, _) = drainAtlasPending (emptyAtlasTextureCache 4)
      null drained `shouldBe` True

  -- -------------------------------------------------------------------
  -- evictIfNeeded
  -- -------------------------------------------------------------------
  describe "evictIfNeeded" $ do
    it "does not evict when within capacity" $ do
      let cache0 = (emptyAtlasTextureCache 4)
            { atcCaches = IntMap.fromList [(6, [mkTile 1 6 testRect]), (10, [mkTile 2 10 testRect])]
            , atcLru = [6, 10]
            }
          cache1 = evictIfNeeded cache0
      IntMap.size (atcCaches cache1) `shouldBe` 2
      null (atcPending cache1) `shouldBe` True

    it "evicts LRU entries beyond maxEntries" $ do
      let cache0 = (emptyAtlasTextureCache 2)
            { atcCaches = IntMap.fromList
                [ (6,  [mkTile 1 6 testRect])
                , (10, [mkTile 2 10 testRect])
                , (18, [mkTile 3 18 testRect])
                ]
            , atcLru = [18, 10, 6]  -- 18 most recent, 6 least recent
            }
          cache1 = evictIfNeeded cache0
      IntMap.size (atcCaches cache1) `shouldBe` 2
      IntMap.member 18 (atcCaches cache1) `shouldBe` True
      IntMap.member 10 (atcCaches cache1) `shouldBe` True
      IntMap.member 6  (atcCaches cache1) `shouldBe` False
      length (atcPending cache1) `shouldBe` 1

  -- -------------------------------------------------------------------
  -- resolveEffectiveStage
  -- -------------------------------------------------------------------
  describe "resolveEffectiveStage" $ do
    let stage0 = ZoomStage { zsHexRadius = 6,  zsAtlasScale = 1, zsZoomMin = 0.0, zsZoomMax = 1.0 }
        stage1 = ZoomStage { zsHexRadius = 10, zsAtlasScale = 2, zsZoomMin = 1.0, zsZoomMax = 2.2 }

    it "commits on first call" $ do
      let cache0 = emptyAtlasTextureCache 4
          (stage, cache1) = resolveEffectiveStage 1000 stage0 cache0
      stage `shouldBe` stage0
      atcCommittedStage cache1 `shouldBe` Just stage0

    it "delays switch within hysteresis window" $ do
      let cache0 = (emptyAtlasTextureCache 4) { atcCommittedStage = Just stage0, atcStageChangeNs = 0 }
          -- First call at t=1000 with different stage starts timer
          (stage1a, cache1) = resolveEffectiveStage 1000 stage1 cache0
          -- Second call at t=1001 (1ns later, still within 300ms)
          (stage1b, _cache2) = resolveEffectiveStage 1001 stage1 cache1
      stage1a `shouldBe` stage0  -- returns old committed stage
      stage1b `shouldBe` stage0  -- still within hysteresis

    it "commits after hysteresis expires" $ do
      let cache0 = (emptyAtlasTextureCache 4) { atcCommittedStage = Just stage0, atcStageChangeNs = 0 }
          (_stageA, cache1) = resolveEffectiveStage 1000 stage1 cache0
          -- Now advance past 300ms (300_000_000 ns)
          (stageB, cache2) = resolveEffectiveStage (1000 + 300000001) stage1 cache1
      stageB `shouldBe` stage1
      atcCommittedStage cache2 `shouldBe` Just stage1

    it "resets timer when stage returns to committed" $ do
      let cache0 = (emptyAtlasTextureCache 4) { atcCommittedStage = Just stage0, atcStageChangeNs = 0 }
          -- Start hysteresis for stage1
          (_stageA, cache1) = resolveEffectiveStage 1000 stage1 cache0
          -- Return to committed stage
          (_stageB, cache2) = resolveEffectiveStage 2000 stage0 cache1
      atcStageChangeNs cache1 `shouldSatisfy` (> 0)
      atcStageChangeNs cache2 `shouldBe` 0

  -- -------------------------------------------------------------------
  -- collectAtlasTextures
  -- -------------------------------------------------------------------
  describe "collectAtlasTextures" $ do
    it "gathers textures from caches and pending" $ do
      let cache = (emptyAtlasTextureCache 4)
            { atcCaches = IntMap.singleton 6 [mkTile 1 6 testRect]
            , atcPending = [mockTexture 99]
            }
      length (collectAtlasTextures cache) `shouldBe` 2
