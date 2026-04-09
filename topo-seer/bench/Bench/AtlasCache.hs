{-# LANGUAGE OverloadedStrings #-}

-- | Benchmarks for atlas tile geometry building and texture upload.
--
-- Pure benchmarks ('buildAtlasTileGeometry', 'composeTilesFromGeometry',
-- 'attachRiverOverlay') run without SDL.  SDL-dependent benchmarks
-- ('renderAtlasTileTextures', 'drawAtlas') initialise a hidden window
-- and renderer via 'withSDL'.
module Bench.AtlasCache (benchmarks) where

import Control.DeepSeq (NFData(..), rwhnf)
import qualified Data.IntMap.Strict as IntMap
import Linear (V2(..))
import qualified SDL
import Test.Tasty (withResource)
import Test.Tasty.Bench

import Actor.UI.State (ViewMode(..))
import Fixtures
import Seer.Render.Atlas (drawAtlas)
import Topo (TerrainChunk)
import Topo.Overlay (OverlayStore, emptyOverlayStore)
import UI.DayNight (mkDayNightFn)
import UI.RiverRender (RiverGeometry(..), buildChunkRiverGeometry, defaultRiverRenderConfig)
import UI.TerrainAtlas
  ( AtlasChunkGeometry(..)
  , AtlasTileGeometry(..)
  , TerrainAtlasTile(..)
  , buildAtlasTileGeometry
  , composeTilesFromGeometry
  , attachRiverOverlay
  , renderAtlasTileTextures
  )
import UI.TerrainRender (ChunkGeometry, buildChunkGeometry)
import UI.TexturePool (TexturePool, newTexturePool, releaseTexture)

------------------------------------------------------------------------
-- NFData instances (atlas geometry types contain storable vectors)
------------------------------------------------------------------------

instance NFData AtlasChunkGeometry where
  rnf (AtlasChunkGeometry v i) = rnf v `seq` rnf i

instance NFData AtlasTileGeometry where
  rnf (AtlasTileGeometry b s h cs rs) =
    rwhnf b `seq` rnf s `seq` rnf h `seq` rnf cs `seq` rnf rs

------------------------------------------------------------------------
-- SDL environment (created once per benchmark group)
------------------------------------------------------------------------

data SDLEnv = SDLEnv
  { sdlRenderer    :: !SDL.Renderer
  , sdlWindow      :: !SDL.Window
  , sdlTexturePool :: !TexturePool
  }

initSDLEnv :: IO SDLEnv
initSDLEnv = do
  SDL.initialize [SDL.InitVideo]
  window   <- SDL.createWindow "bench" SDL.defaultWindow { SDL.windowInitialSize = V2 256 256 }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  pool     <- newTexturePool renderer 32
  pure SDLEnv { sdlRenderer = renderer, sdlWindow = window, sdlTexturePool = pool }

cleanupSDLEnv :: SDLEnv -> IO ()
cleanupSDLEnv env = do
  SDL.destroyRenderer (sdlRenderer env)
  SDL.destroyWindow (sdlWindow env)
  SDL.quit

------------------------------------------------------------------------
-- Shared fixtures
------------------------------------------------------------------------

geoMap16 :: IntMap.IntMap ChunkGeometry
geoMap16 = IntMap.mapWithKey (\k tc ->
  buildChunkGeometry 6 benchWorldConfig ViewElevation 0.3
    climateMap16 weatherMap16 vegMap16
    Nothing k tc) terrainMap16

riverGeoMap :: IntMap.IntMap RiverGeometry
riverGeoMap = IntMap.mapMaybeWithKey (\k _ ->
  buildChunkRiverGeometry defaultRiverRenderConfig benchWorldConfig 6 k
    (IntMap.singleton k populatedRiverChunk) IntMap.empty
  ) terrainMap16

sampleTileGeometry :: [AtlasTileGeometry]
sampleTileGeometry =
  buildAtlasTileGeometry ViewElevation 0.3
    terrainMap16 climateMap16 weatherMap16 vegMap16
    emptyOverlayStore benchWorldConfig
    6   -- hexRadiusPx
    1   -- atlasScale

-- | Day/night function for chunkSize=8.
dayNightFn :: Maybe (Int -> Int -> Float)
dayNightFn = mkDayNightFn benchUiState 8

sampleTileGeometryDayNight :: [AtlasTileGeometry]
sampleTileGeometryDayNight =
  buildAtlasTileGeometry ViewElevation 0.3
    terrainMap16 climateMap16 weatherMap16 vegMap16
    emptyOverlayStore benchWorldConfig
    6 1

sampleTileGeometryOverlay :: [AtlasTileGeometry]
sampleTileGeometryOverlay =
  buildAtlasTileGeometry (ViewOverlay "bench_overlay" 0) 0.3
    terrainMap16 climateMap16 weatherMap16 vegMap16
    benchOverlayStoreDense benchWorldConfig
    6 1

-- | 64-chunk tile geometry for high-tile-count benchmarks.
sampleTileGeometry64 :: [AtlasTileGeometry]
sampleTileGeometry64 =
  buildAtlasTileGeometry ViewElevation 0.3
    terrainMap64 climateMap64 weatherMap64 vegMap64
    emptyOverlayStore benchWorldConfig
    6 1

-- | 64-chunk geometry map for composeTilesFromGeometry.
geoMap64 :: IntMap.IntMap ChunkGeometry
geoMap64 = IntMap.mapWithKey (\k tc ->
  buildChunkGeometry 6 benchWorldConfig ViewElevation 0.3
    climateMap64 weatherMap64 vegMap64
    Nothing k tc) terrainMap64

------------------------------------------------------------------------
-- Benchmarks
------------------------------------------------------------------------

benchmarks :: Benchmark
benchmarks = bgroup "AtlasCache"
  [ bgroup "buildAtlasTileGeometry"
    [ bench "ViewElevation/16chunks" $ nf (buildAtlasGeo ViewElevation) terrainMap16
    , bench "ViewBiome/16chunks"     $ nf (buildAtlasGeo ViewBiome)     terrainMap16
    , bench "ViewClimate/16chunks"   $ nf (buildAtlasGeo ViewClimate)   terrainMap16
    , bench "ViewOverlay/dense/16chunks" $
        nf (buildAtlasGeoOverlay (ViewOverlay "bench_overlay" 0) benchOverlayStoreDense) terrainMap16
    ]
  , bgroup "composeTilesFromGeometry"
    [ bench "16chunks/scale1" $ nf (composeTilesFromGeometry geoMap16 6) 1
    , bench "16chunks/scale2" $ nf (composeTilesFromGeometry geoMap16 6) 2
    , bench "64chunks/scale1" $ nf (composeTilesFromGeometry geoMap64 6) 1
    ]
  , bgroup "attachRiverOverlay"
    -- Pass the river map as the nf argument so GHC cannot constant-fold
    -- the application, and ensure non-empty geometry is overlaid.
    [ bench "16chunks" $ nf (\rg -> attachRiverOverlay rg sampleTileGeometry) riverGeoMap
    ]
  , withResource initSDLEnv cleanupSDLEnv $ \getEnv ->
      bgroup "SDL"
        [ bench "renderAtlasTileTextures/16chunks" $ whnfIO $ do
            env <- getEnv
            tiles <- renderAtlasTileTextures (sdlTexturePool env) (sdlRenderer env) sampleTileGeometry
            mapM_ (releaseTexture (sdlTexturePool env) . tatTexture) tiles
        , bench "renderAtlasTileTextures/overlay" $ whnfIO $ do
            env <- getEnv
            tiles <- renderAtlasTileTextures (sdlTexturePool env) (sdlRenderer env) sampleTileGeometryOverlay
            mapM_ (releaseTexture (sdlTexturePool env) . tatTexture) tiles
        , bench "drawAtlas/16chunks/zoom1" $ whnfIO $ do
            env <- getEnv
            tiles <- renderAtlasTileTextures (sdlTexturePool env) (sdlRenderer env) sampleTileGeometry
            drawAtlas (sdlRenderer env) tiles (0, 0) 1.0 (V2 256 256)
            mapM_ (releaseTexture (sdlTexturePool env) . tatTexture) tiles
        , bench "drawAtlas/64chunks/zoom1" $ whnfIO $ do
            env <- getEnv
            tiles <- renderAtlasTileTextures (sdlTexturePool env) (sdlRenderer env) sampleTileGeometry64
            drawAtlas (sdlRenderer env) tiles (0, 0) 1.0 (V2 800 600)
            mapM_ (releaseTexture (sdlTexturePool env) . tatTexture) tiles
        , bench "drawAtlas/64chunks/zoom0.5" $ whnfIO $ do
            env <- getEnv
            tiles <- renderAtlasTileTextures (sdlTexturePool env) (sdlRenderer env) sampleTileGeometry64
            drawAtlas (sdlRenderer env) tiles (0, 0) 0.5 (V2 800 600)
            mapM_ (releaseTexture (sdlTexturePool env) . tatTexture) tiles
        ]
  ]

buildAtlasGeo :: ViewMode -> IntMap.IntMap TerrainChunk -> [AtlasTileGeometry]
buildAtlasGeo vm tm =
  buildAtlasTileGeometry vm 0.3
    tm climateMap16 weatherMap16 vegMap16
    emptyOverlayStore benchWorldConfig
    6   -- hexRadiusPx
    1   -- atlasScale

buildAtlasGeoOverlay :: ViewMode -> OverlayStore -> IntMap.IntMap TerrainChunk -> [AtlasTileGeometry]
buildAtlasGeoOverlay vm store tm =
  buildAtlasTileGeometry vm 0.3
    tm climateMap16 weatherMap16 vegMap16
    store benchWorldConfig
    6 1
