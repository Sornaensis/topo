{-# LANGUAGE OverloadedStrings #-}

module Spec.Headless (spec) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar, tryReadMVar)
import Control.Exception (bracket, finally)
import Data.Aeson (Value(..), object)
import qualified Data.Aeson.KeyMap as KM
import Linear (V2(..), V4(..))
import qualified SDL
import System.Directory
  ( canonicalizePath
  , createDirectory
  , doesDirectoryExist
  , getTemporaryDirectory
  , removePathForcibly
  )
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.Timeout (timeout)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Test.Hspec

import Seer.Command.Dispatch (CommandContext(..), dispatchCommand)
import Seer.Config.Runtime (TopoSeerConfig(..))
import Seer.DataBrowser.Executor (submitDataBrowserAction, waitDataBrowserExecutorIdle)
import Seer.DataBrowser.Lifecycle (DataBrowserAppAction(..))
import Seer.DataBrowser.Model (DataBrowserBeginResult(..))
import Seer.Headless
  ( HeadlessConfig(..)
  , defaultHeadlessConfig
  , headlessCommandContext
  , headlessServiceContext
  , withHeadlessApp
  )
import Seer.Screenshot.Storage (ScreenshotStoragePolicy(..))
import Seer.Service.Context (ServiceContext(..))
import Seer.Service.Types (ServiceResponse(..))
import Seer.Command.Types (SeerCommand(..), SeerResponse(..))
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

  it "defaults screenshot storage to disabled across runtime contexts" $
    withHeadlessApp defaultHeadlessConfig $ \app -> do
      ccScreenshotStoragePolicy (headlessCommandContext app)
        `shouldBe` ScreenshotStorageDisabled
      svcScreenshotStoragePolicy (headlessServiceContext app)
        `shouldBe` ScreenshotStorageDisabled

  it "initialises one canonical screenshot root for headless contexts" $
    withFreshTempDir $ \base -> do
      let configuredRoot = base </> "screenshots" </> "."
          runtimeCfg = (hcRuntimeConfig defaultHeadlessConfig)
            { cfgScreenshotSaveDirectory = Just configuredRoot }
          headlessCfg = defaultHeadlessConfig { hcRuntimeConfig = runtimeCfg }
      withHeadlessApp headlessCfg $ \app -> do
        _ <- canonicalizePath configuredRoot
        doesDirectoryExist configuredRoot `shouldReturn` True
        ccScreenshotStoragePolicy (headlessCommandContext app)
          `shouldSatisfy` isStorageEnabled
        svcScreenshotStoragePolicy (headlessServiceContext app)
          `shouldSatisfy` isStorageEnabled

  it "cancels and joins every Data Browser worker before bracketed teardown returns" $ do
    started <- newEmptyMVar
    release <- newEmptyMVar
    finished <- newEmptyMVar
    executorIdle <- newEmptyMVar
    let gatedService _ _ =
          (putMVar started () >> readMVar release >> pure (Right (ServiceResponse (object []))))
            `finally` putMVar finished ()
    result <- timeout 30000000 $
      withHeadlessApp defaultHeadlessConfig $ \app -> do
        let executor = ccDataBrowserExecutor (headlessCommandContext app)
        begun <- submitDataBrowserAction executor gatedService DataBrowserLoadPlugins
        case begun of
          DataBrowserBeginAccepted {} -> pure ()
          other -> expectationFailure ("expected accepted Data Browser worker, got " <> show other)
        expectWithin "headless worker start" (takeMVar started)
        tryReadMVar finished `shouldReturn` Nothing
        _ <- forkIO $ waitDataBrowserExecutorIdle executor >> putMVar executorIdle ()
        pure ()
    result `shouldBe` Just ()
    expectWithin "headless executor idle after teardown" (takeMVar executorIdle)
    tryReadMVar finished `shouldReturn` Just ()

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

expectWithin :: String -> IO a -> IO a
expectWithin label action = do
  result <- timeout 1000000 action
  case result of
    Just value -> pure value
    Nothing -> expectationFailure (label <> " deadlocked") >> fail "unreachable"

isStorageEnabled :: ScreenshotStoragePolicy -> Bool
isStorageEnabled (ScreenshotStorageEnabled _) = True
isStorageEnabled ScreenshotStorageDisabled = False

withFreshTempDir :: (FilePath -> IO a) -> IO a
withFreshTempDir action = do
  temp <- getTemporaryDirectory
  stamp <- round . (* (1000000 :: POSIXTime)) <$> getPOSIXTime
  let root = temp </> ("topo-screenshot-headless-" <> show (stamp :: Integer))
  bracket (createDirectory root >> pure root) removePathForcibly action

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
