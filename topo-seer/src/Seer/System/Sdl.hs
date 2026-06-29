{-# LANGUAGE OverloadedStrings #-}

module Seer.System.Sdl
  ( SdlResources(..)
  , initialiseSdlResources
  , shutdownSdlResources
  ) where

import Linear (V2(..))
import qualified SDL
import qualified SDL.Font as Font
import UI.Font (FontCache, destroyFontCache, initFontCacheMaybe)
import UI.Layout (minUsableWindowHeight, minUsableWindowWidth)
import UI.TexturePool (TexturePool, destroyTexturePool, newTexturePool)

data SdlResources = SdlResources
  { srWindow :: !SDL.Window
  , srRenderer :: !SDL.Renderer
  , srRenderTargetOk :: !Bool
  , srFontCache :: !(Maybe FontCache)
  , srTexturePool :: !TexturePool
  }

-- | Create SDL resources on the main thread.  The renderer stays owned by the
-- caller's thread; worker, command, and HTTP threads only communicate through
-- refs and actor handles.
initialiseSdlResources :: IO SdlResources
initialiseSdlResources = do
  SDL.initialize [SDL.InitVideo]
  Font.initialize
  window <- SDL.createWindow "Topo Seer" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  SDL.setWindowMode window SDL.FullscreenDesktop
  SDL.windowMinimumSize window SDL.$= V2 (fromIntegral minUsableWindowWidth) (fromIntegral minUsableWindowHeight)
  renderTargetOk <- SDL.renderTargetSupported renderer
  fontCache <- initFontCacheMaybe renderer 14
    [ "C:\\Windows\\Fonts\\segoeui.ttf"
    , "C:\\Windows\\Fonts\\consola.ttf"
    , "C:\\Windows\\Fonts\\arial.ttf"
    ]
  texturePool <- newTexturePool renderer 32
  pure SdlResources
    { srWindow = window
    , srRenderer = renderer
    , srRenderTargetOk = renderTargetOk
    , srFontCache = fontCache
    , srTexturePool = texturePool
    }

shutdownSdlResources :: SdlResources -> IO ()
shutdownSdlResources sdl = do
  destroyTexturePool (srTexturePool sdl)
  maybe (pure ()) destroyFontCache (srFontCache sdl)
  Font.quit
  SDL.destroyWindow (srWindow sdl)
  SDL.quit
