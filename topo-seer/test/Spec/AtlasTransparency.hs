{-# LANGUAGE OverloadedStrings #-}

module Spec.AtlasTransparency (spec) where

import Control.Exception (bracket, finally)
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Foreign.C.String (peekCString)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (free, mallocBytes)
import Foreign.Ptr (castPtr, nullPtr)
import Linear (V2(..), V4(..))
import qualified SDL
import SDL.Internal.Types (Renderer(..))
import qualified SDL.Raw.Enum as RawEnum
import qualified SDL.Raw.Error as RawError
import qualified SDL.Raw.Video as RawVideo
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Hspec

import Seer.Render.Atlas (drawAtlas, drawAtlasAlpha)
import UI.TerrainAtlas (TerrainAtlasTile(..))
import UI.Widgets (Rect(..))
import UI.WidgetsDraw (rectToSDL)

spec :: Spec
spec = describe "atlas alpha compositing" $ do
  it "respects transparent margins at full-opacity atlas draw" $
    withDummyRenderer $ \renderer ->
      withTexture renderer (4, 4) [(Rect (V2 1 1, V2 2 2), sourceRed)] $ \texture -> do
        clearFrame renderer backgroundGreen
        drawAtlas renderer [mkTile texture (Rect (V2 0 0, V2 4 4))] (0, 0) 1 (V2 frameW frameH)
        pixels <- capturePixels renderer frameW frameH
        pixelAt pixels 0 0 `shouldBe` backgroundGreen
        pixelAt pixels 1 1 `shouldSatisfy` closeToColor sourceRed 1
        assertTextureReadyForReuse texture

  it "overrides stale fallback textures left in BlendNone" $
    withDummyRenderer $ \renderer ->
      withTexture renderer (4, 4) [(Rect (V2 1 1, V2 2 2), sourceRed)] $ \texture -> do
        SDL.textureBlendMode texture SDL.$= SDL.BlendNone
        clearFrame renderer backgroundGreen
        drawAtlasAlpha renderer [mkTile texture (Rect (V2 0 0, V2 4 4))] (0, 0) 1 (V2 frameW frameH) 255
        pixels <- capturePixels renderer frameW frameH
        pixelAt pixels 0 0 `shouldBe` backgroundGreen
        pixelAt pixels 1 1 `shouldSatisfy` closeToColor sourceRed 1
        assertTextureReadyForReuse texture

  it "keeps transparent margins safe across cross-fade alpha endpoints" $
    withDummyRenderer $ \renderer ->
      withTexture renderer (4, 4) [(Rect (V2 1 1, V2 2 2), sourceRed)] $ \texture -> do
        let tile = mkTile texture (Rect (V2 0 0, V2 4 4))
        forM_ [255, 254, 128, 1, 0] $ \alpha -> do
          clearFrame renderer backgroundGreen
          drawAtlasAlpha renderer [tile] (0, 0) 1 (V2 frameW frameH) alpha
          pixels <- capturePixels renderer frameW frameH
          pixelAt pixels 0 0 `shouldBe` backgroundGreen
          pixelAt pixels 1 1 `shouldSatisfy` closeToColor (blendOver alpha sourceRed backgroundGreen) 3
          assertTextureReadyForReuse texture

  it "does not expose framebuffer seams for fractional pan and zoom copy bounds" $
    withDummyRenderer $ \renderer ->
      withTexture renderer (4, 4) [(Rect (V2 0 0, V2 4 4), sourceRed)] $ \leftTexture ->
      withTexture renderer (4, 4) [(Rect (V2 0 0, V2 4 4), sourceBlue)] $ \rightTexture -> do
        clearFrame renderer backgroundGreen
        let leftTile = mkTile leftTexture (Rect (V2 0 0, V2 4 4))
            rightTile = mkTile rightTexture (Rect (V2 4 0, V2 4 4))
        drawAtlas renderer [leftTile, rightTile] (0.25, 0.25) 1.25 (V2 frameW frameH)
        pixels <- capturePixels renderer frameW frameH
        forM_ [(4, 2), (5, 2), (6, 2)] $ \(x, y) -> do
          let px = pixelAt pixels x y
          px `shouldSatisfy` (not . closeToColor backgroundGreen 1)
          px `shouldSatisfy` (not . isBlack)

  it "composites day-night overlay alpha without erasing the base atlas" $
    withDummyRenderer $ \renderer ->
      withTexture renderer (4, 4) [(Rect (V2 0 0, V2 4 4), baseTerrain)] $ \baseTexture ->
      withTexture renderer (4, 4) [(Rect (V2 1 1, V2 2 2), V4 0 0 0 128)] $ \overlayTexture -> do
        clearFrame renderer backgroundGreen
        let bounds = Rect (V2 0 0, V2 4 4)
        drawAtlas renderer [mkTile baseTexture bounds] (0, 0) 1 (V2 frameW frameH)
        drawAtlas renderer [mkTile overlayTexture bounds] (0, 0) 1 (V2 frameW frameH)
        pixels <- capturePixels renderer frameW frameH
        pixelAt pixels 0 0 `shouldSatisfy` closeToColor baseTerrain 1
        pixelAt pixels 1 1 `shouldSatisfy` closeToColor (blendOver 128 (V4 0 0 0 255) baseTerrain) 3
        assertTextureReadyForReuse overlayTexture

  it "leaves the framebuffer unchanged for an empty/loading atlas tile set" $
    withDummyRenderer $ \renderer -> do
      clearFrame renderer backgroundGreen
      drawAtlasAlpha renderer [] (0, 0) 1 (V2 frameW frameH) 255
      pixels <- capturePixels renderer frameW frameH
      pixelAt pixels 0 0 `shouldBe` backgroundGreen
      pixelAt pixels 8 8 `shouldBe` backgroundGreen

frameW, frameH :: Int
frameW = 16
frameH = 16

backgroundGreen, sourceRed, sourceBlue, baseTerrain :: V4 Word8
backgroundGreen = V4 10 100 30 255
sourceRed = V4 200 20 10 255
sourceBlue = V4 20 40 210 255
baseTerrain = V4 120 80 40 255

mkTile :: SDL.Texture -> Rect -> TerrainAtlasTile
mkTile texture bounds = TerrainAtlasTile
  { tatTexture = texture
  , tatBounds = bounds
  , tatScale = 1
  , tatHexRadius = 6
  }

withDummyRenderer :: (SDL.Renderer -> IO a) -> IO a
withDummyRenderer action = withDummySdl $ do
  SDL.initialize [SDL.InitVideo]
  let windowConfig = SDL.defaultWindow { SDL.windowInitialSize = V2 (fromIntegral frameW) (fromIntegral frameH) }
      rendererConfig = SDL.defaultRenderer { SDL.rendererTargetTexture = True }
      draw = bracket (SDL.createWindow "topo-seer-atlas-alpha-test" windowConfig) SDL.destroyWindow $ \window ->
        bracket (SDL.createRenderer window (-1) rendererConfig) SDL.destroyRenderer action
  draw `finally` SDL.quit

withDummySdl :: IO a -> IO a
withDummySdl action = do
  oldVideo <- lookupEnv "SDL_VIDEODRIVER"
  oldRender <- lookupEnv "SDL_RENDER_DRIVER"
  setEnv "SDL_VIDEODRIVER" "dummy"
  setEnv "SDL_RENDER_DRIVER" "software"
  action `finally` do
    restoreEnv "SDL_VIDEODRIVER" oldVideo
    restoreEnv "SDL_RENDER_DRIVER" oldRender

restoreEnv :: String -> Maybe String -> IO ()
restoreEnv name oldValue = case oldValue of
  Nothing -> unsetEnv name
  Just value -> setEnv name value

withTexture :: SDL.Renderer -> (Int, Int) -> [(Rect, V4 Word8)] -> (SDL.Texture -> IO a) -> IO a
withTexture renderer size paints = bracket (makeTexture renderer size paints) SDL.destroyTexture

makeTexture :: SDL.Renderer -> (Int, Int) -> [(Rect, V4 Word8)] -> IO SDL.Texture
makeTexture renderer (w, h) paints = do
  texture <- SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessTarget (V2 (fromIntegral w) (fromIntegral h))
  SDL.rendererRenderTarget renderer SDL.$= Just texture
  SDL.rendererDrawBlendMode renderer SDL.$= SDL.BlendNone
  SDL.rendererDrawColor renderer SDL.$= V4 0 0 0 0
  SDL.clear renderer
  forM_ paints $ \(rect, color) -> do
    SDL.rendererDrawColor renderer SDL.$= color
    SDL.fillRect renderer (Just (rectToSDL rect))
  SDL.rendererRenderTarget renderer SDL.$= Nothing
  SDL.textureBlendMode texture SDL.$= SDL.BlendAlphaBlend
  SDL.textureAlphaMod texture SDL.$= 255
  pure texture

clearFrame :: SDL.Renderer -> V4 Word8 -> IO ()
clearFrame renderer color = do
  SDL.rendererRenderTarget renderer SDL.$= Nothing
  SDL.rendererDrawBlendMode renderer SDL.$= SDL.BlendNone
  SDL.rendererDrawColor renderer SDL.$= color
  SDL.clear renderer

capturePixels :: SDL.Renderer -> Int -> Int -> IO BS.ByteString
capturePixels (Renderer rawRenderer) w h = do
  let bytesPerPixel = 4 :: CInt
      pitch = fromIntegral w * bytesPerPixel
      bufSize = fromIntegral (pitch * fromIntegral h)
  buf <- mallocBytes bufSize
  rc <- RawVideo.renderReadPixels
          rawRenderer
          nullPtr
          RawEnum.SDL_PIXELFORMAT_ABGR8888
          (castPtr buf)
          pitch
  if rc /= 0
    then do
      free buf
      err <- RawError.getError >>= peekCString
      expectationFailure ("SDL_RenderReadPixels failed: " <> err)
      pure BS.empty
    else do
      bytes <- BS.packCStringLen (castPtr buf, bufSize)
      free buf
      pure bytes

pixelAt :: BS.ByteString -> Int -> Int -> V4 Word8
pixelAt pixels x y =
  let idx = ((y * frameW) + x) * 4
  in V4 (BS.index pixels idx)
        (BS.index pixels (idx + 1))
        (BS.index pixels (idx + 2))
        (BS.index pixels (idx + 3))

blendOver :: Word8 -> V4 Word8 -> V4 Word8 -> V4 Word8
blendOver alphaMod (V4 sr sg sb _sa) (V4 dr dg db da) =
  V4 (blendChannel sr dr)
     (blendChannel sg dg)
     (blendChannel sb db)
     da
  where
    a = fromIntegral alphaMod :: Float
    blendChannel src dst =
      round ((fromIntegral src * a + fromIntegral dst * (255 - a)) / 255 :: Float)

closeToColor :: V4 Word8 -> Int -> V4 Word8 -> Bool
closeToColor (V4 er eg eb ea) tolerance (V4 ar ag ab aa) =
  close er ar && close eg ag && close eb ab && close ea aa
  where
    close expected actual = abs (fromIntegral expected - fromIntegral actual :: Int) <= tolerance

isBlack :: V4 Word8 -> Bool
isBlack (V4 r g b _a) = r <= 2 && g <= 2 && b <= 2

assertTextureReadyForReuse :: SDL.Texture -> IO ()
assertTextureReadyForReuse texture = do
  blendMode <- SDL.get (SDL.textureBlendMode texture)
  alphaMod <- SDL.get (SDL.textureAlphaMod texture)
  blendMode `shouldBe` SDL.BlendAlphaBlend
  alphaMod `shouldBe` 255
