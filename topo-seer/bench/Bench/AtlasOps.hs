{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Benchmarks modelling real UI interactions with the atlas texture cache.
--
-- Each benchmark group corresponds to a user action (view mode switch,
-- zoom scroll, pan release, tile drain loop, full frame resolve) and
-- exercises the same pure functions the render thread calls.
module Bench.AtlasOps (benchmarks) where

import Control.DeepSeq (NFData(..), rwhnf)
import Foreign.Ptr (Ptr, intPtrToPtr)
import Test.Tasty.Bench
import Unsafe.Coerce (unsafeCoerce)

import Linear (V2(..))
import qualified SDL

import Actor.AtlasCache (AtlasKey(..))
import Actor.UI.State (ViewMode(..))
import Seer.Render.Atlas
  ( AtlasTextureCache(..)
  , emptyAtlasTextureCache
  , evictIfNeeded
  , getNearestAtlas
  , resolveAtlasFallback
  , resolveAtlasPure
  , resolveEffectiveStage
  , setAtlasKey
  , storeAtlasTiles
  , touchAtlasScale
  )
import Seer.Render.ZoomStage (ZoomStage(..), allZoomStages, stageForZoom)
import UI.TerrainAtlas (TerrainAtlasTile(..))
import UI.Widgets (Rect(..))

------------------------------------------------------------------------
-- Orphan NFData instances (mock textures, never forced to real SDL)
------------------------------------------------------------------------

instance NFData TerrainAtlasTile where
  rnf (TerrainAtlasTile t b s h) = rwhnf t `seq` rwhnf b `seq` rnf s `seq` rnf h

instance NFData ZoomStage where
  rnf (ZoomStage hr as zmin zmax) = rnf hr `seq` rnf as `seq` rnf zmin `seq` rnf zmax

instance NFData AtlasKey where
  rnf (AtlasKey vm wl v) = rwhnf vm `seq` rnf wl `seq` rnf v

instance NFData AtlasTextureCache where
  rnf c = rwhnf (atcKey c) `seq` rwhnf (atcCaches c) `seq` rnf (atcMaxEntries c)
    `seq` rwhnf (atcLru c) `seq` rwhnf (atcPending c) `seq` rwhnf (atcLast c)
    `seq` rwhnf (atcCommittedStage c) `seq` rnf (atcStageChangeNs c)
    `seq` rwhnf (atcDayNight c)

------------------------------------------------------------------------
-- Mock helpers (no real SDL textures — never passed to SDL)
------------------------------------------------------------------------

mockTexture :: Int -> SDL.Texture
mockTexture n = unsafeCoerce (intPtrToPtr (fromIntegral n) :: Ptr ())

mkTile :: Int -> Int -> Rect -> TerrainAtlasTile
mkTile tag hr bounds = TerrainAtlasTile
  { tatTexture   = mockTexture tag
  , tatBounds    = bounds
  , tatScale     = 1
  , tatHexRadius = hr
  }

tileRect :: Int -> Rect
tileRect i = Rect (V2 (i * 64) 0, V2 64 64)

------------------------------------------------------------------------
-- Shared keys & stages
------------------------------------------------------------------------

keyElev, keyBiome, keyClimate, keyMoisture :: AtlasKey
keyElev    = AtlasKey ViewElevation 0.3 1
keyBiome   = AtlasKey ViewBiome     0.3 1
keyClimate = AtlasKey ViewClimate   0.3 1
keyMoisture = AtlasKey ViewMoisture 0.3 1

keyElevV2 :: AtlasKey
keyElevV2 = AtlasKey ViewElevation 0.3 2  -- terrain version 2

-- | All five zoom-stage hex radii.
allHexRadii :: [Int]
allHexRadii = map zsHexRadius allZoomStages  -- [6, 10, 18, 32, 50]

------------------------------------------------------------------------
-- Pre-built caches at various sizes
------------------------------------------------------------------------

-- | 16 tiles per scale, stored for one key across all 5 zoom stages.
populatedCache :: AtlasTextureCache
populatedCache =
  let cache0 = setAtlasKey keyElev (emptyAtlasTextureCache 50)
      tiles hr = [mkTile (hr * 100 + i) hr (tileRect i) | i <- [0..15]]
  in foldl (\c hr -> storeAtlasTiles keyElev hr (tiles hr) c) cache0 allHexRadii

-- | Cache with 3 view modes populated (Elev, Biome, Climate), 5 scales each.
multiKeyCache :: AtlasTextureCache
multiKeyCache =
  let cache0 = setAtlasKey keyElev (emptyAtlasTextureCache 50)
      tiles modeIdx hr = [mkTile (modeIdx * 1000 + hr * 100 + i) hr (tileRect i) | i <- [0..15]]
      storeMode c (idx, k) = foldl (\c' hr -> storeAtlasTiles k hr (tiles idx hr) c') c allHexRadii
  in foldl storeMode cache0 (zip [0..] [keyElev, keyBiome, keyClimate])

-- | Cache near eviction pressure: 45 entries in a cache with maxEntries=50.
pressureCache :: AtlasTextureCache
pressureCache =
  let cache0 = setAtlasKey keyElev (emptyAtlasTextureCache 50)
      keys = [AtlasKey ViewElevation 0.3 1, AtlasKey ViewBiome 0.3 1,
              AtlasKey ViewClimate 0.3 1, AtlasKey ViewMoisture 0.3 1,
              AtlasKey ViewPrecip 0.3 1, AtlasKey ViewWeather 0.3 1,
              AtlasKey ViewTerrainForm 0.3 1, AtlasKey ViewPlateId 0.3 1,
              AtlasKey ViewPlateBoundary 0.3 1]
      tiles modeIdx hr = [mkTile (modeIdx * 1000 + hr * 100 + i) hr (tileRect i) | i <- [0..7]]
      storeMode c (idx, k) = foldl (\c' hr -> storeAtlasTiles k hr (tiles idx hr) c') c allHexRadii
  in foldl storeMode cache0 (zip [0..] keys)

------------------------------------------------------------------------
-- Benchmarks
------------------------------------------------------------------------

benchmarks :: Benchmark
benchmarks = bgroup "AtlasOps"
  [ viewModeSwitchGroup
  , zoomScrollGroup
  , panReleaseGroup
  , tileDrainGroup
  , frameResolveGroup
  ]

-- | View mode switch: user presses a view-mode button.
-- The render thread sets the new key, then resolveAtlasPure is called
-- every frame until tiles arrive.  Measures the fallback/mismatch path
-- and the hot path once tiles are cached.
viewModeSwitchGroup :: Benchmark
viewModeSwitchGroup = bgroup "viewModeSwitch"
  [ bench "setAtlasKey (switch to new mode)" $
      whnf (setAtlasKey keyBiome) populatedCache

  , bench "resolveAtlasPure/fallback (no tiles for new mode)" $
      let cache = setAtlasKey keyBiome populatedCache
      in nf (resolveAtlasPure True True keyBiome 6) cache

  , bench "resolveAtlasPure/hit (cached mode)" $
      nf (resolveAtlasPure True True keyElev 6) populatedCache

  , bench "storeAtlasTiles (first tiles for new mode)" $
      let cache = setAtlasKey keyBiome populatedCache
          tiles = [mkTile (2000 + i) 6 (tileRect i) | i <- [0..15]]
      in nf (storeAtlasTiles keyBiome 6 tiles) cache

  , bench "resolveAtlasPure after store (mismatch resolved)" $
      let cache0 = setAtlasKey keyBiome populatedCache
          tiles  = [mkTile (2000 + i) 6 (tileRect i) | i <- [0..15]]
          cache1 = storeAtlasTiles keyBiome 6 tiles cache0
      in nf (resolveAtlasPure True True keyBiome 6) cache1

  , bench "multi-key: 3 modes cached, switch to 4th" $
      let cache = setAtlasKey keyMoisture multiKeyCache
      in nf (resolveAtlasPure True True keyMoisture 6) cache

  , bench "multi-key: switch back to cached mode" $
      let cache = setAtlasKey keyElev multiKeyCache
      in nf (resolveAtlasPure True True keyElev 6) cache
  ]

-- | Zoom scroll: user scrolls the mouse wheel.
-- resolveEffectiveStage applies hysteresis; getNearestAtlas does
-- nearest-scale lookup; storeAtlasTiles accumulates new-scale tiles.
zoomScrollGroup :: Benchmark
zoomScrollGroup = bgroup "zoomScroll"
  [ bench "resolveEffectiveStage/same stage" $
      let (_, _, cache1) = resolveEffectiveStage 1000 (stageForZoom 1.5) (emptyAtlasTextureCache 50)
      in nf (resolveEffectiveStage 2000 (stageForZoom 1.5)) cache1

  , bench "resolveEffectiveStage/cross boundary (hysteresis)" $
      let stage1 = stageForZoom 1.5
          stage2 = stageForZoom 2.5
          (_, _, cache1) = resolveEffectiveStage 1000 stage1 (emptyAtlasTextureCache 50)
      in nf (resolveEffectiveStage 2000 stage2) cache1

  , bench "getNearestAtlas/exact scale" $
      nf (getNearestAtlas keyElev 6) populatedCache

  , bench "getNearestAtlas/nearest scale (miss)" $
      nf (getNearestAtlas keyElev 12) populatedCache

  , bench "storeAtlasTiles/new scale arriving" $
      let tiles = [mkTile (5000 + i) 18 (tileRect i) | i <- [0..15]]
      in nf (storeAtlasTiles keyElev 18 tiles) populatedCache

  , bench "stageForZoom (per-scroll)" $
      nf stageForZoom 3.7
  ]

-- | Pan drag-release: user pans the map and releases.
-- The same key+scale gets new tiles with shifted bounds (merge path).
panReleaseGroup :: Benchmark
panReleaseGroup = bgroup "panRelease"
  [ bench "storeAtlasTiles/merge (shifted bounds)" $
      let newTiles = [mkTile (3000 + i) 6 (Rect (V2 (i * 64 + 32) 0, V2 64 64)) | i <- [0..15]]
      in nf (storeAtlasTiles keyElev 6 newTiles) populatedCache

  , bench "storeAtlasTiles/replace (same bounds)" $
      let newTiles = [mkTile (4000 + i) 6 (tileRect i) | i <- [0..15]]
      in nf (storeAtlasTiles keyElev 6 newTiles) populatedCache

  , bench "touchAtlasScale (LRU update)" $
      nf (touchAtlasScale 6) populatedCache
  ]

-- | Tile drain loop: worker results arrive and get stored.
-- Exercises storeAtlasTiles + evictIfNeeded under multi-key pressure.
tileDrainGroup :: Benchmark
tileDrainGroup = bgroup "tileDrain"
  [ bench "store 16 tiles (single batch)" $
      let tiles = [mkTile (6000 + i) 6 (tileRect i) | i <- [0..15]]
      in nf (storeAtlasTiles keyElev 6 tiles) (setAtlasKey keyElev (emptyAtlasTextureCache 50))

  , bench "store 5 batches (one per zoom stage)" $
      let cache0 = setAtlasKey keyElev (emptyAtlasTextureCache 50)
          tiles hr = [mkTile (hr * 100 + i) hr (tileRect i) | i <- [0..15]]
      in nf (\c -> foldl (\c' hr -> storeAtlasTiles keyElev hr (tiles hr) c') c allHexRadii) cache0

  , bench "stale tiles discarded (version mismatch)" $
      let cache = setAtlasKey keyElevV2 populatedCache
          staleTiles = [mkTile (7000 + i) 6 (tileRect i) | i <- [0..15]]
      in nf (storeAtlasTiles keyElev 6 staleTiles) cache

  , bench "evictIfNeeded (near capacity)" $
      let tiles = [mkTile (8000 + i) 50 (tileRect i) | i <- [0..7]]
          cache = storeAtlasTiles (AtlasKey ViewPlateAge 0.3 1) 50 tiles pressureCache
      in nf evictIfNeeded cache

  , bench "store under eviction pressure (10 modes × 5 scales)" $
      let tiles = [mkTile (9000 + i) 6 (tileRect i) | i <- [0..7]]
          newKey = AtlasKey ViewPlateAge 0.3 1
      in nf (storeAtlasTiles newKey 6 tiles) pressureCache
  ]

-- | Full frame resolve: the complete resolveAtlasPure as called each frame.
-- Benchmarks the steady-state hot path and the cold-start path.
frameResolveGroup :: Benchmark
frameResolveGroup = bgroup "frameResolve"
  [ bench "steady state (exact hit)" $
      nf (resolveAtlasPure True True keyElev 6) populatedCache

  , bench "steady state (multi-key cache, exact hit)" $
      nf (resolveAtlasPure True True keyElev 6) multiKeyCache

  , bench "cold start (empty cache)" $
      nf (resolveAtlasPure True True keyElev 6) (setAtlasKey keyElev (emptyAtlasTextureCache 50))

  , bench "fallback with mismatch (stale atcLast)" $
      let cache0 = populatedCache { atcLast = Just (keyElev, [mkTile 99 6 (tileRect 0)]) }
          cache1 = setAtlasKey keyBiome cache0
      in nf (resolveAtlasPure True True keyBiome 6) cache1

  , bench "not renderTargetOk (skip)" $
      nf (resolveAtlasPure False True keyElev 6) populatedCache

  , bench "not dataReady (skip)" $
      nf (resolveAtlasPure True False keyElev 6) populatedCache

  , bench "resolveAtlasFallback/hit" $
      nf (resolveAtlasFallback keyElev (Just [mkTile 1 6 (tileRect 0)])) populatedCache

  , bench "resolveAtlasFallback/mismatch" $
      let cache = populatedCache { atcLast = Just (keyElev, [mkTile 99 6 (tileRect 0)]) }
      in nf (resolveAtlasFallback keyBiome Nothing) cache
  ]
