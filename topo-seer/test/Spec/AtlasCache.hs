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
  , resolveAtlasFallback
  , resolveAtlasPure
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
keyA = AtlasKey ViewElevation 0 1
keyB = AtlasKey ViewBiome     0 1

-- | Key with a different terrain version (stale).
keyStale :: AtlasKey
keyStale = AtlasKey ViewElevation 0 999

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

  -- -------------------------------------------------------------------
  -- resolveAtlasFallback (key-mismatch detection)
  -- -------------------------------------------------------------------
  describe "resolveAtlasFallback" $ do
    it "returns no mismatch when exact tiles are found" $ do
      let tiles = [mkTile 1 6 testRect]
          (result, mismatch) = resolveAtlasFallback keyA (Just tiles) (emptyAtlasTextureCache 30)
      fmap length result `shouldBe` Just 1
      mismatch `shouldBe` False

    it "returns no mismatch when atcLast key matches" $ do
      let lastTiles = [mkTile 10 6 testRect]
          cache = (emptyAtlasTextureCache 30) { atcLast = Just (keyA, lastTiles) }
          (result, mismatch) = resolveAtlasFallback keyA Nothing cache
      fmap length result `shouldBe` Just 1
      mismatch `shouldBe` False

    it "returns mismatch when atcLast key differs (view mode switch)" $ do
      let lastTiles = [mkTile 10 6 testRect]
          cache = (emptyAtlasTextureCache 30) { atcLast = Just (keyA, lastTiles) }
          -- Expected key is keyB (different view mode) but atcLast has keyA
          (result, mismatch) = resolveAtlasFallback keyB Nothing cache
      isJust result `shouldBe` True
      mismatch `shouldBe` True

    it "returns mismatch when atcLast differs in water level" $ do
      let keyWet = AtlasKey ViewElevation 0.5 1
          keyDry = AtlasKey ViewElevation 0.0 1
          lastTiles = [mkTile 10 6 testRect]
          cache = (emptyAtlasTextureCache 30) { atcLast = Just (keyWet, lastTiles) }
          (result, mismatch) = resolveAtlasFallback keyDry Nothing cache
      isJust result `shouldBe` True
      mismatch `shouldBe` True

    it "returns no mismatch and Nothing when atcLast is empty" $ do
      let (result, mismatch) = resolveAtlasFallback keyA Nothing (emptyAtlasTextureCache 30)
      isNothing result `shouldBe` True
      mismatch `shouldBe` False

    it "returns no mismatch and Nothing when atcLast has empty tiles" $ do
      let cache = (emptyAtlasTextureCache 30) { atcLast = Just (keyA, []) }
          (result, mismatch) = resolveAtlasFallback keyB Nothing cache
      isNothing result `shouldBe` True
      mismatch `shouldBe` False

    it "prefers exact tiles over atcLast even when atcLast mismatches" $ do
      let exactTiles = [mkTile 1 6 testRect]
          lastTiles = [mkTile 10 6 testRect]
          cache = (emptyAtlasTextureCache 30) { atcLast = Just (keyA, lastTiles) }
          (result, mismatch) = resolveAtlasFallback keyB (Just exactTiles) cache
      fmap length result `shouldBe` Just 1
      mismatch `shouldBe` False

    it "falls back to atcLast when exact tiles list is empty" $ do
      let lastTiles = [mkTile 10 6 testRect]
          cache = (emptyAtlasTextureCache 30) { atcLast = Just (keyA, lastTiles) }
          (result, mismatch) = resolveAtlasFallback keyB (Just []) cache
      isJust result `shouldBe` True
      mismatch `shouldBe` True

  -- -------------------------------------------------------------------
  -- View mode switch sequence (multi-frame simulation)
  --
  -- These tests model the real render-loop state machine by threading
  -- cache state through the same pure operations that resolveAtlasTiles
  -- and renderFrame use, verifying the mismatch flag and cache updates
  -- across multiple simulated frames.
  -- -------------------------------------------------------------------
  describe "view mode switch sequence" $ do
    it "mismatch persists until correct-mode tiles are stored" $ do
      -- Frame 0: Elevation tiles are displayed and cached.
      let elevTiles = [mkTile 1 6 testRect]
          cache0 = storeAtlasTiles keyA 6 elevTiles
                     ((emptyAtlasTextureCache 30) { atcKey = Just keyA })
          -- Simulate resolveAtlasTiles setting atcLast after a successful lookup
          cache0' = cache0 { atcLast = Just (keyA, elevTiles) }

      -- Frame 1: User switches to Biome (keyB).
      -- renderFrame calls setAtlasKey, then resolveAtlasTiles does
      -- getNearestAtlas for the new key and falls back to atcLast.
      let cache1 = setAtlasKey keyB cache0'
          lookupB1 = getNearestAtlas keyB 6 cache1  -- no biome tiles yet
          (tiles1, mismatch1) = resolveAtlasFallback keyB lookupB1 cache1
      isNothing lookupB1 `shouldBe` True
      isJust tiles1 `shouldBe` True         -- falls back to elevation tiles
      mismatch1 `shouldBe` True             -- KEY: triggers retry

      -- Cache unchanged because atcLast is NOT updated on fallback
      -- (cacheWithLast only updates when atlasTiles is Just non-empty)
      fmap fst (atcLast cache1) `shouldBe` Just keyA  -- still elevation key

      -- Frame 2: Still no biome tiles yet, mismatch must persist.
      let lookupB2 = getNearestAtlas keyB 6 cache1
          (_tiles2, mismatch2) = resolveAtlasFallback keyB lookupB2 cache1
      mismatch2 `shouldBe` True             -- still mismatched

      -- Frame 3: Worker delivers biome tiles (stored via storeAtlasTiles).
      let biomeTiles = [mkTile 50 6 testRect]
          cache3 = storeAtlasTiles keyB 6 biomeTiles cache1
          lookupB3 = getNearestAtlas keyB 6 cache3
          (tiles3, mismatch3) = resolveAtlasFallback keyB lookupB3 cache3
      isJust lookupB3 `shouldBe` True       -- biome tiles now in cache
      isJust tiles3 `shouldBe` True
      mismatch3 `shouldBe` False            -- KEY: mismatch clears

    it "switching back to a cached mode has no mismatch" $ do
      -- Both modes pre-cached
      let elevTiles = [mkTile 1 6 testRect]
          biomeTiles = [mkTile 2 6 testRect]
          cache0 = storeAtlasTiles keyB 6 biomeTiles
                 $ storeAtlasTiles keyA 6 elevTiles
                     ((emptyAtlasTextureCache 30) { atcKey = Just keyA })
          cache0' = cache0 { atcLast = Just (keyA, elevTiles) }

      -- Switch to biome: tiles already cached → no mismatch
      let cache1 = setAtlasKey keyB cache0'
          lookupB = getNearestAtlas keyB 6 cache1
          (_tilesB, mismatchB) = resolveAtlasFallback keyB lookupB cache1
      isJust lookupB `shouldBe` True
      mismatchB `shouldBe` False

      -- Switch back to elevation: still cached → no mismatch
      let cache2 = setAtlasKey keyA cache1
          lookupA = getNearestAtlas keyA 6 cache2
          (_tilesA, mismatchA) = resolveAtlasFallback keyA lookupA cache2
      isJust lookupA `shouldBe` True
      mismatchA `shouldBe` False

    it "needsRetry is True during mismatch and False after resolution" $ do
      -- Simulate the renderFrame return value formula:
      -- needsRetry = renderTargetOk && dataReady && (isNothing atlasToDraw || keyMismatch)
      let renderTargetOk = True
          dataReady = True
          needsRetry mTiles km = renderTargetOk && dataReady && (isNothing mTiles || km)

      -- No tiles at all → retry (pre-existing behavior)
      needsRetry Nothing False `shouldBe` True

      -- Correct tiles displayed → no retry
      needsRetry (Just [mkTile 1 6 testRect]) False `shouldBe` False

      -- Stale tiles from atcLast → retry (the fix)
      needsRetry (Just [mkTile 1 6 testRect]) True `shouldBe` True

      -- Not data-ready → no retry regardless of mismatch
      let needsRetryNoData mTiles km = renderTargetOk && False && (isNothing mTiles || km)
      needsRetryNoData (Just [mkTile 1 6 testRect]) True `shouldBe` False

  -- -------------------------------------------------------------------
  -- cacheWithLast update invariant
  --
  -- resolveAtlasTiles only updates atcLast when the EXACT-match lookup
  -- (getNearestAtlas for the expected key) returns Just non-empty.
  -- Fallback through atcLast must NOT overwrite atcLast, otherwise the
  -- mismatch flag disappears on the next frame.  These tests verify
  -- that invariant by simulating the cacheWithLast logic from
  -- resolveAtlasTiles as a pure helper.
  -- -------------------------------------------------------------------
  describe "cacheWithLast update invariant" $ do
    -- Mirror the cacheWithLast logic from resolveAtlasTiles:
    --   case atlasTiles of
    --     Just tiles | not (null tiles) -> cache { atcLast = Just (key, tiles) }
    --     _ -> cache
    let cacheWithLast key atlasTiles cache = case atlasTiles of
          Just tiles | not (null tiles) ->
            cache { atcLast = Just (key, tiles) }
          _ -> cache

    it "updates atcLast when exact-match tiles are found" $ do
      let tiles = [mkTile 1 6 testRect]
          cache0 = emptyAtlasTextureCache 30
          cache1 = cacheWithLast keyA (Just tiles) cache0
      fmap fst (atcLast cache1) `shouldBe` Just keyA
      fmap (length . snd) (atcLast cache1) `shouldBe` Just 1

    it "does NOT update atcLast on fallback (Nothing lookup)" $ do
      let oldTiles = [mkTile 10 6 testRect]
          cache0 = (emptyAtlasTextureCache 30) { atcLast = Just (keyA, oldTiles) }
          cache1 = cacheWithLast keyB Nothing cache0
      -- atcLast must still point to keyA, NOT keyB
      fmap fst (atcLast cache1) `shouldBe` Just keyA

    it "does NOT update atcLast when exact lookup returns empty list" $ do
      let oldTiles = [mkTile 10 6 testRect]
          cache0 = (emptyAtlasTextureCache 30) { atcLast = Just (keyA, oldTiles) }
          cache1 = cacheWithLast keyB (Just []) cache0
      fmap fst (atcLast cache1) `shouldBe` Just keyA

    it "overwrites atcLast when a new key's tiles arrive" $ do
      let oldTiles = [mkTile 10 6 testRect]
          newTiles = [mkTile 20 6 testRect]
          cache0 = (emptyAtlasTextureCache 30) { atcLast = Just (keyA, oldTiles) }
          cache1 = cacheWithLast keyB (Just newTiles) cache0
      fmap fst (atcLast cache1) `shouldBe` Just keyB

  -- -------------------------------------------------------------------
  -- Rapid multi-mode switching (A→B→C before any tiles arrive)
  --
  -- When the user quickly cycles through several view modes, the cache
  -- must keep every mode's stale/missing state correct and mismatch
  -- must fire for each intermediate mode until tiles arrive.
  -- -------------------------------------------------------------------
  describe "rapid multi-mode switching" $ do
    let keyC = AtlasKey ViewClimate 0 1

    it "A→B→C: mismatch fires on every mode without cached tiles" $ do
      let elevTiles = [mkTile 1 6 testRect]
          cache0 = storeAtlasTiles keyA 6 elevTiles
                     ((emptyAtlasTextureCache 30) { atcKey = Just keyA })
          cache0' = cache0 { atcLast = Just (keyA, elevTiles) }

      -- Switch to B
      let cacheB = setAtlasKey keyB cache0'
          (_, mismatchB) = resolveAtlasFallback keyB (getNearestAtlas keyB 6 cacheB) cacheB
      mismatchB `shouldBe` True

      -- Immediately switch to C (no tiles for B arrived)
      let cacheC = setAtlasKey keyC cacheB
          (_, mismatchC) = resolveAtlasFallback keyC (getNearestAtlas keyC 6 cacheC) cacheC
      mismatchC `shouldBe` True

    it "A→B→C: tiles arriving for B do not resolve C's mismatch" $ do
      let elevTiles = [mkTile 1 6 testRect]
          cache0 = storeAtlasTiles keyA 6 elevTiles
                     ((emptyAtlasTextureCache 30) { atcKey = Just keyA })
          cache0' = cache0 { atcLast = Just (keyA, elevTiles) }

      -- Switch through B to C
      let cacheC = setAtlasKey keyC (setAtlasKey keyB cache0')

      -- Worker delivers B tiles (from earlier request)
      let biomeTiles = [mkTile 50 6 testRect]
          cacheC' = storeAtlasTiles keyB 6 biomeTiles cacheC

      -- C still has no tiles → mismatch still True
      let lookupC = getNearestAtlas keyC 6 cacheC'
          (_, mismatchC) = resolveAtlasFallback keyC lookupC cacheC'
      isNothing lookupC `shouldBe` True
      mismatchC `shouldBe` True

      -- B tiles ARE in cache (queryable even though not active)
      isJust (getNearestAtlas keyB 6 cacheC') `shouldBe` True

    it "A→B→C→A: returning to A uses cached tiles, no mismatch" $ do
      let elevTiles = [mkTile 1 6 testRect]
          cache0 = storeAtlasTiles keyA 6 elevTiles
                     ((emptyAtlasTextureCache 30) { atcKey = Just keyA })
          cache0' = cache0 { atcLast = Just (keyA, elevTiles) }

      -- Cycle through B, C, back to A
      let cacheA = setAtlasKey keyA
                 $ setAtlasKey keyC
                 $ setAtlasKey keyB cache0'
          lookupA = getNearestAtlas keyA 6 cacheA
          (_, mismatchA) = resolveAtlasFallback keyA lookupA cacheA
      isJust lookupA `shouldBe` True
      mismatchA `shouldBe` False

  -- -------------------------------------------------------------------
  -- Worker delivers tiles for non-active (old) key during switch
  -- -------------------------------------------------------------------
  describe "cross-key tile delivery" $ do
    it "tiles arriving for old view mode are stored (same terrain version)" $ do
      let cache0 = (emptyAtlasTextureCache 30) { atcKey = Just keyB }
          -- Worker delivers elevation tiles while biome is active
          elevTiles = [mkTile 1 6 testRect]
          cache1 = storeAtlasTiles keyA 6 elevTiles cache0
      -- keyA and keyB share terrain version 1, so stored
      isJust (Map.lookup keyA (atcCaches cache1)) `shouldBe` True
      null (atcPending cache1) `shouldBe` True

    it "tiles from stale terrain generation are discarded" $ do
      let cache0 = (emptyAtlasTextureCache 30) { atcKey = Just keyB }
          -- Worker delivers tiles with terrain version 999 (stale)
          staleTiles = [mkTile 1 6 testRect]
          cache1 = storeAtlasTiles keyStale 6 staleTiles cache0
      Map.null (atcCaches cache1) `shouldBe` True
      length (atcPending cache1) `shouldBe` 1

    it "newly stored tiles for active key immediately clear mismatch" $ do
      let elevTiles = [mkTile 1 6 testRect]
          cache0 = storeAtlasTiles keyA 6 elevTiles
                     ((emptyAtlasTextureCache 30) { atcKey = Just keyA })
          cache0' = cache0 { atcLast = Just (keyA, elevTiles) }

      -- Switch to B, mismatch fires
      let cacheB = setAtlasKey keyB cache0'
          (_, mm1) = resolveAtlasFallback keyB (getNearestAtlas keyB 6 cacheB) cacheB
      mm1 `shouldBe` True

      -- Worker delivers biome tiles in the same frame
      let biomeTiles = [mkTile 50 6 testRect]
          cacheB' = storeAtlasTiles keyB 6 biomeTiles cacheB
          lookupB = getNearestAtlas keyB 6 cacheB'
          (_, mm2) = resolveAtlasFallback keyB lookupB cacheB'
      mm2 `shouldBe` False

  -- -------------------------------------------------------------------
  -- Terrain generation version bump
  --
  -- When the user triggers a new generation, all atlas keys change
  -- version.  The old atcLast may hold tiles from the previous
  -- generation that no longer match any key.
  -- -------------------------------------------------------------------
  describe "terrain version bump" $ do
    let keyAv1 = AtlasKey ViewElevation 0 1
        keyAv2 = AtlasKey ViewElevation 0 2

    it "version bump causes mismatch even for same view mode" $ do
      let oldTiles = [mkTile 1 6 testRect]
          cache0 = (emptyAtlasTextureCache 30)
            { atcKey  = Just keyAv2
            , atcLast = Just (keyAv1, oldTiles)
            }
          lookup' = getNearestAtlas keyAv2 6 cache0  -- no v2 tiles yet
          (tiles, mismatch) = resolveAtlasFallback keyAv2 lookup' cache0
      isNothing lookup' `shouldBe` True
      isJust tiles `shouldBe` True  -- falls back to v1 tiles
      mismatch `shouldBe` True      -- version differs

    it "mismatch clears once new-generation tiles arrive" $ do
      let oldTiles = [mkTile 1 6 testRect]
          cache0 = (emptyAtlasTextureCache 30)
            { atcKey  = Just keyAv2
            , atcLast = Just (keyAv1, oldTiles)
            }
          -- New-gen tiles stored
          newTiles = [mkTile 100 6 testRect]
          cache1 = storeAtlasTiles keyAv2 6 newTiles cache0
          lookup' = getNearestAtlas keyAv2 6 cache1
          (_, mismatch) = resolveAtlasFallback keyAv2 lookup' cache1
      isJust lookup' `shouldBe` True
      mismatch `shouldBe` False

    it "old-generation tiles are discarded to pending after version bump" $ do
      let cache0 = (emptyAtlasTextureCache 30) { atcKey = Just keyAv2 }
          oldTiles = [mkTile 1 6 testRect]
          cache1 = storeAtlasTiles keyAv1 6 oldTiles cache0
      -- keyAv1 has version 1 ≠ active version 2 → stale
      Map.null (atcCaches cache1) `shouldBe` True
      length (atcPending cache1) `shouldBe` 1

  -- -------------------------------------------------------------------
  -- Water level change (same view mode, different water level)
  -- -------------------------------------------------------------------
  describe "water level change" $ do
    let keyWet = AtlasKey ViewElevation 0.5 1
        keyDry = AtlasKey ViewElevation 0.0 1

    it "water level change is a key switch that triggers mismatch" $ do
      let wetTiles = [mkTile 1 6 testRect]
          cache0 = storeAtlasTiles keyWet 6 wetTiles
                     ((emptyAtlasTextureCache 30) { atcKey = Just keyWet })
          cache0' = cache0 { atcLast = Just (keyWet, wetTiles) }

      let cache1 = setAtlasKey keyDry cache0'
          lookup' = getNearestAtlas keyDry 6 cache1
          (_, mismatch) = resolveAtlasFallback keyDry lookup' cache1
      isNothing lookup' `shouldBe` True  -- no dry tiles
      mismatch `shouldBe` True

    it "water level tiles coexist in cache without flushing" $ do
      let wetTiles = [mkTile 1 6 testRect]
          dryTiles = [mkTile 2 6 testRect]
          cache0 = storeAtlasTiles keyDry 6 dryTiles
                 $ storeAtlasTiles keyWet 6 wetTiles
                     ((emptyAtlasTextureCache 30) { atcKey = Just keyWet })
      Map.size (atcCaches cache0) `shouldBe` 2
      isJust (getNearestAtlas keyWet 6 cache0) `shouldBe` True
      isJust (getNearestAtlas keyDry 6 cache0) `shouldBe` True

    it "toggling water level back uses cached tiles, no mismatch" $ do
      let wetTiles = [mkTile 1 6 testRect]
          dryTiles = [mkTile 2 6 testRect]
          cache0 = storeAtlasTiles keyDry 6 dryTiles
                 $ storeAtlasTiles keyWet 6 wetTiles
                     ((emptyAtlasTextureCache 30) { atcKey = Just keyWet })
          cache0' = cache0 { atcLast = Just (keyWet, wetTiles) }

      -- Switch to dry
      let cache1 = setAtlasKey keyDry cache0'
          lookupDry = getNearestAtlas keyDry 6 cache1
          (_, mmDry) = resolveAtlasFallback keyDry lookupDry cache1
      mmDry `shouldBe` False  -- dry tiles already cached

      -- Switch back to wet
      let cache2 = setAtlasKey keyWet cache1
          lookupWet = getNearestAtlas keyWet 6 cache2
          (_, mmWet) = resolveAtlasFallback keyWet lookupWet cache2
      mmWet `shouldBe` False  -- wet tiles still cached

  -- -------------------------------------------------------------------
  -- Full render-loop state machine via resolveAtlasPure
  --
  -- These tests call the actual extracted pure function that
  -- resolveAtlasTiles delegates to, so the composition of
  -- getNearestAtlas + resolveAtlasFallback + cacheWithLast + touchLRU
  -- is tested through production code — not reimplemented in a test
  -- helper.
  -- -------------------------------------------------------------------
  describe "full render-loop state machine (resolveAtlasPure)" $ do
    -- Reusable frame stepper using the REAL resolveAtlasPure.
    -- Simulates: optional worker delivery → resolveAtlasPure → needsRetry.
    let stepFrame :: AtlasKey -> Maybe [TerrainAtlasTile] -> AtlasTextureCache
                  -> (Bool, Maybe [TerrainAtlasTile], AtlasTextureCache)
        stepFrame expectedKey workerTiles cache0 =
          let cache1 = setAtlasKey expectedKey cache0
              -- Store any worker-delivered tiles (same as drainAtlasBuildResults)
              cache2 = case workerTiles of
                Just ts | not (null ts) -> storeAtlasTiles expectedKey 6 ts cache1
                _                       -> cache1
              -- Call the REAL pure core
              (atlasToDraw, keyMismatch, cache3) =
                resolveAtlasPure True True expectedKey 6 cache2
              -- needsRetry formula (mirrors renderFrame)
              needsRetry = isNothing atlasToDraw || keyMismatch
          in (needsRetry, atlasToDraw, cache3)

    it "fresh cache needs retry until first tiles arrive" $ do
      let cache0 = emptyAtlasTextureCache 30
          (retry1, tiles1, cache1) = stepFrame keyA Nothing cache0
      retry1 `shouldBe` True
      isNothing tiles1 `shouldBe` True

      let (retry2, tiles2, _cache2) = stepFrame keyA (Just [mkTile 1 6 testRect]) cache1
      retry2 `shouldBe` False
      isJust tiles2 `shouldBe` True

    it "view mode switch: retry persists across frames until tiles arrive" $ do
      let elevTiles = [mkTile 1 6 testRect]
          cache0 = storeAtlasTiles keyA 6 elevTiles
                     ((emptyAtlasTextureCache 30) { atcKey = Just keyA })
          cache0' = cache0 { atcLast = Just (keyA, elevTiles) }

      let (retry1, tiles1, cache1) = stepFrame keyB Nothing cache0'
      retry1 `shouldBe` True
      isJust tiles1 `shouldBe` True   -- fallback from atcLast

      let (retry2, _tiles2, cache2) = stepFrame keyB Nothing cache1
      retry2 `shouldBe` True

      let (retry3, tiles3, _cache3) = stepFrame keyB (Just [mkTile 50 6 testRect]) cache2
      retry3 `shouldBe` False
      isJust tiles3 `shouldBe` True

    it "atcLast updates only on exact match, never on fallback" $ do
      let elevTiles = [mkTile 1 6 testRect]
          cache0 = storeAtlasTiles keyA 6 elevTiles
                     ((emptyAtlasTextureCache 30) { atcKey = Just keyA })
          cache0' = cache0 { atcLast = Just (keyA, elevTiles) }

      -- Fallback frame: atcLast must stay keyA
      let (_ret1, _til1, cache1) = stepFrame keyB Nothing cache0'
      fmap fst (atcLast cache1) `shouldBe` Just keyA

      let (_ret2, _til2, cache2) = stepFrame keyB Nothing cache1
      fmap fst (atcLast cache2) `shouldBe` Just keyA

      -- Tiles arrive: atcLast updates to keyB
      let (_ret3, _til3, cache3) = stepFrame keyB (Just [mkTile 50 6 testRect]) cache2
      fmap fst (atcLast cache3) `shouldBe` Just keyB

    it "five-mode round-trip: each switch resolves correctly" $ do
      let keyC = AtlasKey ViewClimate    0 1
          keyD = AtlasKey ViewMoisture   0 1
          keyE = AtlasKey ViewPrecip     0 1
          allKeys = [keyA, keyB, keyC, keyD, keyE]
          seed cache (k, tag) = storeAtlasTiles k 6 [mkTile tag 6 testRect] cache
          cache0 = foldl seed ((emptyAtlasTextureCache 30) { atcKey = Just keyA })
                     (zip allKeys [1..])
          cache0' = cache0 { atcLast = Just (keyA, [mkTile 1 6 testRect]) }

      let checkMode cache key = do
            let (tiles, mismatch, cache') = resolveAtlasPure True True key 6 (setAtlasKey key cache)
            isJust tiles `shouldBe` True
            mismatch `shouldBe` False
            pure cache'
      cache1 <- checkMode cache0' keyB
      cache2 <- checkMode cache1 keyC
      cache3 <- checkMode cache2 keyD
      cache4 <- checkMode cache3 keyE
      _      <- checkMode cache4 keyA  -- full cycle back
      pure ()

    it "resolveAtlasPure returns no tiles when not renderTargetOk" $ do
      let elevTiles = [mkTile 1 6 testRect]
          cache0 = storeAtlasTiles keyA 6 elevTiles
                     ((emptyAtlasTextureCache 30) { atcKey = Just keyA })
      let (tiles, mismatch, _) = resolveAtlasPure False True keyA 6 cache0
      isNothing tiles `shouldBe` True
      mismatch `shouldBe` False

    it "resolveAtlasPure returns no tiles when not dataReady" $ do
      let elevTiles = [mkTile 1 6 testRect]
          cache0 = storeAtlasTiles keyA 6 elevTiles
                     ((emptyAtlasTextureCache 30) { atcKey = Just keyA })
      let (tiles, mismatch, _) = resolveAtlasPure True False keyA 6 cache0
      isNothing tiles `shouldBe` True
      mismatch `shouldBe` False

    it "resolveAtlasPure falls back with mismatch when not dataReady but atcLast exists" $ do
      let elevTiles = [mkTile 1 6 testRect]
          cache0 = (emptyAtlasTextureCache 30)
            { atcKey  = Just keyB
            , atcLast = Just (keyA, elevTiles)
            }
      let (tiles, mismatch, _) = resolveAtlasPure True False keyB 6 cache0
      -- dataReady=False → getNearestAtlas skipped → falls back to atcLast
      isJust tiles `shouldBe` True
      mismatch `shouldBe` True

    it "resolveAtlasPure touches LRU for resolved tiles" $ do
      let tiles6  = [mkTile 1 6 testRect]
          tiles10 = [mkTile 2 10 testRect]
          cache0 = storeAtlasTiles keyA 10 tiles10
                 $ storeAtlasTiles keyA 6 tiles6
                     ((emptyAtlasTextureCache 30) { atcKey = Just keyA })
      -- Resolve at hexRadius 6 → should touch (keyA, 6) in LRU
      let (_, _, cache1) = resolveAtlasPure True True keyA 6 cache0
      case atcLru cache1 of
        ((k, s):_) -> do k `shouldBe` keyA; s `shouldBe` 6
        []         -> expectationFailure "LRU should not be empty"
