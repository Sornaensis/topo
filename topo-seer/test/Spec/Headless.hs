{-# LANGUAGE OverloadedStrings #-}

module Spec.Headless (spec) where

import Control.Exception (bracket, finally)
import Data.Aeson (Value(..))
import qualified Data.Aeson.KeyMap as KM
import Linear (V2(..), V4(..))
import qualified SDL
import System.Environment (lookupEnv, setEnv, unsetEnv)
import Test.Hspec

import Seer.Command.Dispatch (dispatchCommand)
import Seer.Headless
  ( HeadlessConfig(..)
  , defaultHeadlessConfig
  , headlessCommandContext
  , withHeadlessApp
  )
import Topo.Command.Types (SeerCommand(..), SeerResponse(..))
import UI.DrawCommand (clearClip, clipTo, fillRect, line, strokeRect, textCentered)
import UI.DrawCommand.SDL (interpretDrawCommands)
import UI.Widgets (Rect(..))

spec :: Spec
spec = describe "Seer.Headless" $ do
  it "starts actors and dispatches commands without an SDL window" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      rsp <- dispatchCommand (headlessCommandContext app) SeerCommand
        { scId = 1
        , scMethod = "get_state"
        , scParams = Null
        }
      srSuccess rsp `shouldBe` True
      case srResult rsp of
        Object result -> do
          KM.member "seed" result `shouldBe` True
          KM.member "view_mode" result `shouldBe` True
        _ -> expectationFailure "expected JSON object result"

  it "uses the configured deterministic seed for repeatable tests" $
    withHeadlessApp defaultHeadlessConfig { hcSeed = 123 } $ \app -> do
      rsp <- dispatchCommand (headlessCommandContext app) SeerCommand
        { scId = 2
        , scMethod = "get_state"
        , scParams = Null
        }
      srSuccess rsp `shouldBe` True
      case srResult rsp of
        Object result -> KM.lookup "seed" result `shouldBe` Just (Number 123)
        _ -> expectationFailure "expected JSON object result"

  it "interprets draw commands on an SDL dummy renderer" $
    withDummySdl $ do
      SDL.initialize [SDL.InitVideo]
      let windowConfig = SDL.defaultWindow { SDL.windowInitialSize = V2 64 64 }
          drawSmoke =
            bracket (SDL.createWindow "topo-seer-headless-test" windowConfig) SDL.destroyWindow $ \window ->
              bracket (SDL.createRenderer window (-1) SDL.defaultRenderer) SDL.destroyRenderer $ \renderer -> do
                interpretDrawCommands renderer Nothing
                  [ fillRect (V4 10 20 30 255) (Rect (V2 0 0, V2 32 32))
                  , strokeRect (V4 200 210 220 255) (Rect (V2 4 4, V2 24 24))
                  , textCentered (V4 255 255 255 255) (Rect (V2 0 0, V2 32 16)) "Topo"
                  , line (V4 255 0 0 255) (V2 0 0) (V2 31 31)
                  , clipTo (Rect (V2 0 0, V2 16 16))
                  , clearClip
                  ]
                SDL.present renderer
      drawSmoke `finally` SDL.quit

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
