module Spec.AtlasCache (spec) where

import Test.Hspec
import Data.Maybe (isNothing, isJust)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as Map
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

-- | Two keys that differ only in view mode (same terrain version).
keyA, keyB :: AtlasKey
keyA = AtlasKey ViewElevation 0 False 1
keyB = AtlasKey ViewBiome     0 False 1

-- | Key with a different terrain version (stale).
keyStale :: AtlasKey
keyStale = AtlasKey ViewElevation 0 False 999

-- | Helper to count total scales across all keys in the nested Map.
totalScales :: Map.Map AtlasKey (IntMap.IntMap [TerrainAtlasTile]) -> Int
totalScales = sum . map IntMap.size . Map.elems

spec :: Spec
spec = describe "AtlasTextureCache" $ do

  -- -------------------------------------------------------------------
  -- setAtlasKey (multi-key: O(1) pointer update, no flushing)
  -- -------------------------------------------------------------------
  describe "setAtlasKey" $ do
    it "is idempotent for the same key" $ do
      let cache0 = (emptyAtlasTextureCache 30) { atcKey = Just keyA }
          cache1 = setAtlasKey keyA cache0
      atcKey cache1 `shouldBe` atcKey cache0
      totalScales (atcCaches cache1) `shouldBe` totalScales (atcCaches cache0)
      length (atcPending cache1) `shouldBe` length (atcPending cache0)

    it "preserves atcCaches on key change (multi-key)" $ do
      let tiles  = [mkTile 1 6 testRect]
          cache0 = storeAtlasTiles keyA 6 tiles
                     ((emptyAtlasTextureCache 30) { atcKey = Just keyA })
          cache1 = setAtlasKey keyB cache0
      atcKey cache1 `shouldBe` Just keyB
      -- Tiles for keyA must still be in the cache
      isJust (Map.lookup keyA (atcCaches cache1)) `shouldBe` True

    it "does not add anything to atcPending on key change" $ do
      let tiles  = [mkTile 1 6 testRect, mkTile 2 6 testRect2]
          cache0 = storeAtlasTiles keyA 6 tiles
                     ((emptyAtlasTextureCache 30) { atcKey = Just keyA })
          cache1 = setAtlasKey keyB cache0
      null (atcPending cache1) `shouldBe` True

    it "preserves atcLast on key change" $ do
      let lastTiles = [mkTile 10 6 testRect]
          cache0 = (emptyAtlasTextureCache 30)
            { atcKey  = Just keyA
            , atcCaches = Map.empty
            , atcLast = Just (keyA, lastTiles)
            }
          cache1 = setAtlasKey keyB cache0
      fmap fst (atcLast cache1) `shouldBe` Just keyA
      fmap (length . snd) (atcLast cache1) `shouldBe` Just 1

  -- -------------------------------------------------------------------
  -- storeAtlasTiles (multi-key nested Map)
  -- -------------------------------------------------------------------
  describe "storeAtlasTiles" $ do
    it "stores tiles for the active key" $ do
      let cache0 = (emptyAtlasTextureCache 30) { atcKey = Just keyA }
          tiles  = [mkTile 1 6 testRect]
          cache1 = storeAtlasTiles keyA 6 tiles cache0
      isJust (Map.lookup keyA (atcCaches cache1)) `shouldBe` True
      case Map.lookup keyA (atcCaches cache1) of
        Just bucket -> IntMap.member 6 bucket `shouldBe` True
        Nothing     -> expectationFailure "keyA bucket missing"

    it "stores tiles for a non-active key with matching terrain version" $ do
      let cache0 = (emptyAtlasTextureCache 30) { atcKey = Just keyA }
          tiles  = [mkTile 50 10 testRect]
          cache1 = storeAtlasTiles keyB 10 tiles cache0
      -- keyB has same terrain version (1) as keyA, so stored
      isJust (Map.lookup keyB (atcCaches cache1)) `shouldBe` True
      null (atcPending cache1) `shouldBe` True

    it "discards tiles with stale terrain version to pending" $ do
      let cache0 = (emptyAtlasTextureCache 30) { atcKey = Just keyA }
          tiles  = [mkTile 50 10 testRect]
          cache1 = storeAtlasTiles keyStale 10 tiles cache0
      Map.null (atcCaches cache1) `shouldBe` True
      length (atcPending cache1) `shouldBe` 1

    it "sets atcKey when storing into empty cache" $ do
      let cache0 = emptyAtlasTextureCache 30
          tiles  = [mkTile 1 6 testRect]
          cache1 = storeAtlasTiles keyA 6 tiles cache0
      atcKey cache1 `shouldBe` Just keyA

    it "replaces tiles at the same (key, scale) and pending old" $ do
      let cache0 = (emptyAtlasTextureCache 30) { atcKey = Just keyA }
          tiles1 = [mkTile 1 6 testRect]
          tiles2 = [mkTile 2 6 testRect]  -- same bounds replaces tile 1
          cache1 = storeAtlasTiles keyA 6 tiles1 cache0
          cache2 = storeAtlasTiles keyA 6 tiles2 cache1
      -- Old tile 1 should be in pending
      length (atcPending cache2) `shouldBe` 1

    it "coexists tiles for different keys" $ do
      let cache0 = (emptyAtlasTextureCache 30) { atcKey = Just keyA }
          cache1 = storeAtlasTiles keyA 6 [mkTile 1 6 testRect] cache0
          cache2 = storeAtlasTiles keyB 6 [mkTile 2 6 testRect] cache1
      Map.size (atcCaches cache2) `shouldBe` 2

  -- -------------------------------------------------------------------
  -- getNearestAtlas (looks up by any key)
  -- -------------------------------------------------------------------
  describe "getNearestAtlas" $ do
    it "returns exact scale match for specified key" $ do
      let cache0 = (emptyAtlasTextureCache 30) { atcKey = Just keyA }
          cache1 = storeAtlasTiles keyA 6 [mkTile 1 6 testRect]
                 $ storeAtlasTiles keyA 10 [mkTile 2 10 testRect] cache0
          result = getNearestAtlas keyA 6 cache1
      fmap length result `shouldBe` Just 1
      fmap (tatHexRadius . head) result `shouldBe` Just 6

    it "returns nearest scale when exact is missing" $ do
      let cache0 = (emptyAtlasTextureCache 30) { atcKey = Just keyA }
          cache1 = storeAtlasTiles keyA 8 [mkTile 1 8 testRect]
                 $ storeAtlasTiles keyA 20 [mkTile 2 20 testRect] cache0
          result = getNearestAtlas keyA 10 cache1
      fmap length result `shouldBe` Just 1
      fmap (tatHexRadius . head) result `shouldBe` Just 8

    it "returns Nothing when key is not in cache" $ do
      let cache0 = (emptyAtlasTextureCache 30) { atcKey = Just keyA }
          cache1 = storeAtlasTiles keyA 6 [mkTile 1 6 testRect] cache0
      isNothing (getNearestAtlas keyStale 6 cache1) `shouldBe` True

    it "looks up non-active key that is present in cache" $ do
      let cache0 = (emptyAtlasTextureCache 30) { atcKey = Just keyA }
          cache1 = storeAtlasTiles keyA 6 [mkTile 1 6 testRect]
                 $ storeAtlasTiles keyB 6 [mkTile 2 6 testRect] cache0
          -- Switch active key to keyB, but look up keyA
          cache2 = setAtlasKey keyB cache1
          result = getNearestAtlas keyA 6 cache2
      isJust result `shouldBe` True

  -- -------------------------------------------------------------------
  -- touchAtlasScale (uses active key in LRU)
  -- -------------------------------------------------------------------
  describe "touchAtlasScale" $ do
    it "moves (key, scale) to front of LRU" $ do
      let cache0 = (emptyAtlasTextureCache 30)
            { atcKey = Just keyA
            , atcLru = [(keyA, 6), (keyA, 10), (keyA, 18)]
            }
          cache1 = touchAtlasScale 10 cache0
      atcLru cache1 `shouldBe` [(keyA, 10), (keyA, 6), (keyA, 18)]

    it "adds new (key, scale) to LRU" $ do
      let cache0 = (emptyAtlasTextureCache 30)
            { atcKey = Just keyA
            , atcLru = [(keyA, 6)]
            }
          cache1 = touchAtlasScale 10 cache0
      atcLru cache1 `shouldBe` [(keyA, 10), (keyA, 6)]

    it "is a no-op when no active key" $ do
      let cache0 = (emptyAtlasTextureCache 30) { atcLru = [] }
          cache1 = touchAtlasScale 10 cache0
      null (atcLru cache1) `shouldBe` True

  -- -------------------------------------------------------------------
  -- drainAtlasPending
  -- -------------------------------------------------------------------
  describe "drainAtlasPending" $ do
    it "returns pending textures and empties the list" $ do
      let texs   = [mockTexture 1, mockTexture 2]
          cache0 = (emptyAtlasTextureCache 30) { atcPending = texs }
          (drained, cache1) = drainAtlasPending cache0
      length drained `shouldBe` 2
      all (`elem` texs) drained `shouldBe` True
      null (atcPending cache1) `shouldBe` True

    it "returns empty list when no textures are pending" $ do
      let (drained, _) = drainAtlasPending (emptyAtlasTextureCache 30)
      null drained `shouldBe` True

  -- -------------------------------------------------------------------
  -- evictIfNeeded (global eviction across all keys)
  -- -------------------------------------------------------------------
  describe "evictIfNeeded" $ do
    it "does not evict when within capacity" $ do
      let cache0 = (emptyAtlasTextureCache 30) { atcKey = Just keyA }
          cache1 = storeAtlasTiles keyA 6 [mkTile 1 6 testRect]
                 $ storeAtlasTiles keyA 10 [mkTile 2 10 testRect] cache0
      -- storeAtlasTiles calls evictIfNeeded internally; check nothing evicted
      totalScales (atcCaches cache1) `shouldBe` 2
      null (atcPending cache1) `shouldBe` True

    it "evicts LRU entries beyond maxEntries" $ do
      let cache0 = (emptyAtlasTextureCache 2) { atcKey = Just keyA }
          -- Store 3 scales but max is 2
          cache1 = storeAtlasTiles keyA 18 [mkTile 3 18 testRect]
                 $ storeAtlasTiles keyA 10 [mkTile 2 10 testRect]
                 $ storeAtlasTiles keyA 6 [mkTile 1 6 testRect] cache0
      -- scale 6 (first stored) should be evicted as LRU
      totalScales (atcCaches cache1) `shouldBe` 2
      length (atcPending cache1) `shouldBe` 1

    it "evicts across different keys" $ do
      let cache0 = (emptyAtlasTextureCache 2) { atcKey = Just keyA }
          cache1 = storeAtlasTiles keyB 6 [mkTile 3 6 testRect]
                 $ storeAtlasTiles keyA 10 [mkTile 2 10 testRect]
                 $ storeAtlasTiles keyA 6 [mkTile 1 6 testRect] cache0
      -- 3 entries stored, max 2 → oldest (keyA, 6) evicted
      totalScales (atcCaches cache1) `shouldBe` 2
      length (atcPending cache1) `shouldBe` 1

  -- -------------------------------------------------------------------
  -- resolveEffectiveStage
  -- -------------------------------------------------------------------
  describe "resolveEffectiveStage" $ do
    let stage0 = ZoomStage { zsHexRadius = 6,  zsAtlasScale = 1, zsZoomMin = 0.0, zsZoomMax = 1.0 }
        stage1 = ZoomStage { zsHexRadius = 10, zsAtlasScale = 2, zsZoomMin = 1.0, zsZoomMax = 2.2 }

    it "commits on first call" $ do
      let cache0 = emptyAtlasTextureCache 30
          (stage, _blend, cache1) = resolveEffectiveStage 1000 stage0 cache0
      stage `shouldBe` stage0
      atcCommittedStage cache1 `shouldBe` Just stage0

    it "delays switch within hysteresis window" $ do
      let cache0 = (emptyAtlasTextureCache 30) { atcCommittedStage = Just stage0, atcStageChangeNs = 0 }
          -- First call at t=1000 with different stage starts timer
          (stage1a, blend1a, cache1) = resolveEffectiveStage 1000 stage1 cache0
          -- Second call at t=1001 (1ns later, still within 300ms)
          (stage1b, _blend1b, _cache2) = resolveEffectiveStage 1001 stage1 cache1
      stage1a `shouldBe` stage0  -- returns old committed stage
      stage1b `shouldBe` stage0  -- still within hysteresis
      fmap fst blend1a `shouldBe` Just stage1  -- blend target is the new stage

    it "commits after hysteresis expires" $ do
      let cache0 = (emptyAtlasTextureCache 30) { atcCommittedStage = Just stage0, atcStageChangeNs = 0 }
          (_stageA, _blendA, cache1) = resolveEffectiveStage 1000 stage1 cache0
          -- Now advance past 300ms (300_000_000 ns)
          (stageB, blendB, cache2) = resolveEffectiveStage (1000 + 300000001) stage1 cache1
      stageB `shouldBe` stage1
      blendB `shouldBe` Nothing
      atcCommittedStage cache2 `shouldBe` Just stage1

    it "resets timer when stage returns to committed" $ do
      let cache0 = (emptyAtlasTextureCache 30) { atcCommittedStage = Just stage0, atcStageChangeNs = 0 }
          -- Start hysteresis for stage1
          (_stageA, _blendA, cache1) = resolveEffectiveStage 1000 stage1 cache0
          -- Return to committed stage
          (_stageB, blendB, cache2) = resolveEffectiveStage 2000 stage0 cache1
      atcStageChangeNs cache1 `shouldSatisfy` (> 0)
      atcStageChangeNs cache2 `shouldBe` 0
      blendB `shouldBe` Nothing

    it "returns blend factor of 0 at the start of transition" $ do
      let cache0 = (emptyAtlasTextureCache 30) { atcCommittedStage = Just stage0, atcStageChangeNs = 0 }
          (_stage, blend, _cache1) = resolveEffectiveStage 1000 stage1 cache0
      fmap snd blend `shouldBe` Just 0

    it "returns blend factor near 1 just before hysteresis expires" $ do
      let cache0 = (emptyAtlasTextureCache 30) { atcCommittedStage = Just stage0, atcStageChangeNs = 0 }
          (_stageA, _blendA, cache1) = resolveEffectiveStage 1000 stage1 cache0
          -- 299ms later (just before 300ms threshold)
          (_stageB, blendB, _cache2) = resolveEffectiveStage (1000 + 299000000) stage1 cache1
      case blendB of
        Just (_, b) -> b `shouldSatisfy` (> 0.9)
        Nothing     -> expectationFailure "expected blend target during transition"

    it "returns smoothstep blend at midpoint" $ do
      let cache0 = (emptyAtlasTextureCache 30) { atcCommittedStage = Just stage0, atcStageChangeNs = 0 }
          (_stageA, _blendA, cache1) = resolveEffectiveStage 1000 stage1 cache0
          -- Half-way through hysteresis (150ms)
          (_stageB, blendB, _cache2) = resolveEffectiveStage (1000 + 150000000) stage1 cache1
      case blendB of
        Just (_, b) -> b `shouldSatisfy` (\v -> v > 0.4 && v < 0.6)
        Nothing     -> expectationFailure "expected blend target during transition"

  -- -------------------------------------------------------------------
  -- collectAtlasTextures (gathers from all keys)
  -- -------------------------------------------------------------------
  describe "collectAtlasTextures" $ do
    it "gathers textures from all keys and pending" $ do
      let cache0 = (emptyAtlasTextureCache 30) { atcKey = Just keyA }
          cache1 = storeAtlasTiles keyB 6 [mkTile 2 6 testRect]
                 $ storeAtlasTiles keyA 6 [mkTile 1 6 testRect] cache0
          cache2 = cache1 { atcPending = [mockTexture 99] }
      length (collectAtlasTextures cache2) `shouldBe` 3
