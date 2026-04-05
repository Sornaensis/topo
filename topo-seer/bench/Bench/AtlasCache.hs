{-# LANGUAGE OverloadedStrings #-}

-- | Benchmarks for atlas tile geometry building and texture upload.
--
-- Pure benchmarks ('buildAtlasTileGeometry', 'composeTilesFromGeometry',
-- 'attachRiverOverlay') run without SDL.  SDL-dependent benchmarks
-- ('renderAtlasTileTextures', 'drawAtlas') initialise a hidden window
-- and renderer via 'withSDL'.
module Bench.AtlasCache (benchmarks) where

import Control.DeepSeq (NFData(..), rwhnf)
import Control.Exception (bracket_)
import qualified Data.IntMap.Strict as IntMap
import Linear (V2(..))
import qualified SDL
import Test.Tasty.Bench

import Actor.UI.State (ViewMode(..))
import Fixtures
import Seer.Render.Atlas (drawAtlas)
import Topo (TerrainChunk)
import Topo.Overlay (emptyOverlayStore)
import UI.RiverRender (RiverGeometry(..))
import UI.TerrainAtlas
  ( AtlasChunkGeometry(..)
  , AtlasTileGeometry(..)
  , buildAtlasTileGeometry
  , composeTilesFromGeometry
  , attachRiverOverlay
  , renderAtlasTileTextures
  )
import UI.TerrainRender (ChunkGeometry(..), buildChunkGeometry)

------------------------------------------------------------------------
-- NFData instances (atlas geometry types contain storable vectors)
------------------------------------------------------------------------

instance NFData AtlasChunkGeometry where
  rnf (AtlasChunkGeometry v i) = rnf v `seq` rnf i

instance NFData AtlasTileGeometry where
  rnf (AtlasTileGeometry b s h cs rs) =
    rwhnf b `seq` rnf s `seq` rnf h `seq` rnf cs `seq` rnf rs

------------------------------------------------------------------------
-- SDL bracket
------------------------------------------------------------------------

withSDL :: (SDL.Renderer -> IO a) -> IO a
withSDL action =
  bracket_
    (SDL.initialize [SDL.InitVideo])
    SDL.quit
    $ do
      window   <- SDL.createWindow "bench" SDL.defaultWindow { SDL.windowInitialSize = V2 256 256 }
      renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
      result   <- action renderer
      SDL.destroyRenderer renderer
      SDL.destroyWindow window
      pure result

------------------------------------------------------------------------
-- Shared fixtures
------------------------------------------------------------------------

terrainMap16 :: IntMap.IntMap TerrainChunk
terrainMap16 = IntMap.fromList [(i, benchTerrainChunk) | i <- [0..15]]

geoMap16 :: IntMap.IntMap ChunkGeometry
geoMap16 = IntMap.mapWithKey (\k tc ->
  buildChunkGeometry 6 benchWorldConfig ViewElevation 0.3
    (IntMap.singleton k benchClimateChunk)
    (IntMap.singleton k benchWeatherChunk)
    (IntMap.singleton k benchVegetationChunk)
    Nothing Nothing k tc) terrainMap16

riverGeoMap :: IntMap.IntMap RiverGeometry
riverGeoMap = IntMap.empty

sampleTileGeometry :: [AtlasTileGeometry]
sampleTileGeometry =
  buildAtlasTileGeometry ViewElevation 0.3
    terrainMap16
    (IntMap.fromList [(i, benchClimateChunk)    | i <- [0..15]])
    (IntMap.fromList [(i, benchWeatherChunk)     | i <- [0..15]])
    (IntMap.fromList [(i, benchVegetationChunk)  | i <- [0..15]])
    emptyOverlayStore
    benchWorldConfig
    6   -- hexRadiusPx
    1   -- atlasScale

------------------------------------------------------------------------
-- Benchmarks
------------------------------------------------------------------------

benchmarks :: Benchmark
benchmarks = bgroup "AtlasCache"
  [ bgroup "buildAtlasTileGeometry"
    [ bench "ViewElevation/16chunks" $ nf (buildAtlasGeo ViewElevation) terrainMap16
    , bench "ViewBiome/16chunks"     $ nf (buildAtlasGeo ViewBiome)     terrainMap16
    , bench "ViewClimate/16chunks"   $ nf (buildAtlasGeo ViewClimate)   terrainMap16
    ]
  , bgroup "composeTilesFromGeometry"
    [ bench "16chunks/scale1" $ nf (composeTilesFromGeometry geoMap16 6) 1
    , bench "16chunks/scale2" $ nf (composeTilesFromGeometry geoMap16 6) 2
    ]
  , bgroup "attachRiverOverlay"
    [ bench "16chunks" $ nf (attachRiverOverlay riverGeoMap) sampleTileGeometry
    ]
  , bgroup "renderAtlasTileTextures"
    [ bench "16chunks" $ whnfIO $ withSDL $ \renderer ->
        renderAtlasTileTextures renderer sampleTileGeometry
    ]
  , bgroup "drawAtlas"
    [ bench "16chunks/zoom1" $ whnfIO $ withSDL $ \renderer -> do
        tiles <- renderAtlasTileTextures renderer sampleTileGeometry
        drawAtlas renderer tiles (0, 0) 1.0 (V2 256 256)
    ]
  ]

buildAtlasGeo :: ViewMode -> IntMap.IntMap TerrainChunk -> [AtlasTileGeometry]
buildAtlasGeo vm tm =
  buildAtlasTileGeometry vm 0.3
    tm
    (IntMap.fromList [(i, benchClimateChunk)    | i <- [0..15]])
    (IntMap.fromList [(i, benchWeatherChunk)     | i <- [0..15]])
    (IntMap.fromList [(i, benchVegetationChunk)  | i <- [0..15]])
    emptyOverlayStore
    benchWorldConfig
    6   -- hexRadiusPx
    1   -- atlasScale
