{-# LANGUAGE OverloadedStrings #-}

module Spec.ScreenshotStorage (spec) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, tryTakeMVar)
import Control.Exception (IOException, bracket, displayException, try)
import Control.Monad (forM_)
import qualified Data.ByteString as ByteString
import Data.Either (isLeft)
import Data.List (isPrefixOf)
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import System.Directory
  ( canonicalizePath
  , createDirectory
  , createDirectoryLink
  , createFileLink
  , doesDirectoryExist
  , getTemporaryDirectory
  , listDirectory
  , removePathForcibly
  )
import System.FilePath ((</>))
import Test.Hspec

import Seer.Screenshot.Storage
  ( ScreenshotSaveError(..)
  , saveScreenshotPng
  , saveScreenshotPngWithCommitHook
  , validateScreenshotPngPath
  )

spec :: Spec
spec = describe "sandboxed screenshot PNG persistence" $ do
  describe "path validation" $ do
    it "rejects rooted, traversal, ambiguous, and non-lowercase-PNG paths" $ do
      let invalidPaths =
            [ ""
            , "/shot.png"
            , "C:/shot.png"
            , "//server/share/shot.png"
            , "folder\\shot.png"
            , "folder//shot.png"
            , "./shot.png"
            , "folder/../shot.png"
            , "folder/.png"
            , "folder/shot"
            , "folder/shot.PNG"
            , "folder/shot.png/"
            , "folder:name/shot.png"
            , "folder./shot.png"
            , "folder /shot.png"
            , "folder/CON.png"
            , "folder/com1.capture.png"
            , "folder/nul/shot.png"
            , "folder/control\NULname.png"
            ]
      forM_ invalidPaths $ \path ->
        validateScreenshotPngPath path `shouldBe` Left ScreenshotInvalidPath

    it "preserves ordinary Unicode and spaces in normalized relative output" $
      validateScreenshotPngPath "maps/été world/山 view.png"
        `shouldBe` Right "maps/été world/山 view.png"

  it "creates nested parents, writes exact bytes, and returns only the relative path" $
    withFreshTempDir "nested" $ \root -> do
      canonicalRoot <- canonicalizePath root
      let bytes = "\137PNG\r\n\SUB\ncomplete"
      result <- saveScreenshotPng canonicalRoot "maps/region one/view.png" bytes
      result `shouldBe` Right "maps/region one/view.png"
      ByteString.readFile (root </> "maps" </> "region one" </> "view.png")
        `shouldReturn` bytes
      doesDirectoryExist (root </> "maps" </> "region one") `shouldReturn` True
      assertNoTemporaryFiles (root </> "maps" </> "region one")

  it "atomically replaces an existing regular PNG without stale temp files" $
    withFreshTempDir "replace" $ \root -> do
      canonicalRoot <- canonicalizePath root
      let destination = root </> "view.png"
          oldBytes = ByteString.replicate (1024 * 1024) 17
          newBytes = ByteString.replicate (1024 * 1024) 93
      ByteString.writeFile destination oldBytes
      saveScreenshotPng canonicalRoot "view.png" newBytes
        `shouldReturn` Right "view.png"
      ByteString.readFile destination `shouldReturn` newBytes
      assertNoTemporaryFiles root

  it "shows concurrent readers only complete old or new PNG bytes" $
    withFreshTempDir "concurrent-read" $ \root -> do
      canonicalRoot <- canonicalizePath root
      let destination = root </> "view.png"
          oldBytes = ByteString.replicate (1024 * 1024) 31
          newBytes = ByteString.replicate (1024 * 1024) 47
      ByteString.writeFile destination oldBytes
      finished <- newEmptyMVar
      _ <- forkIO $ do
        result <- saveScreenshotPngWithCommitHook
          (threadDelay 50000)
          canonicalRoot
          "view.png"
          newBytes
        putMVar finished result
      (result, observed) <- observeUntilFinished finished destination []
      result `shouldBe` Right "view.png"
      observed `shouldSatisfy` (not . null)
      observed `shouldSatisfy` all (\bytes -> bytes == oldBytes || bytes == newBytes)
      ByteString.readFile destination `shouldReturn` newBytes

  it "preserves the old PNG and cleans its temp after a post-write failure" $
    withFreshTempDir "commit-failure" $ \root -> do
      canonicalRoot <- canonicalizePath root
      let destination = root </> "view.png"
          oldBytes = "complete-old-png"
      ByteString.writeFile destination oldBytes
      saveScreenshotPngWithCommitHook
        (ioError (userError "injected post-temp failure"))
        canonicalRoot
        "view.png"
        "complete-new-png"
        `shouldReturn` Left ScreenshotUnexpectedIO
      ByteString.readFile destination `shouldReturn` oldBytes
      assertNoTemporaryFiles root

  it "rejects file-valued parents and directory-valued destinations as conflicts" $
    withFreshTempDir "conflict" $ \root -> do
      canonicalRoot <- canonicalizePath root
      writeFile (root </> "occupied") "not a directory"
      createDirectory (root </> "directory.png")
      saveScreenshotPng canonicalRoot "occupied/view.png" "png"
        `shouldReturn` Left ScreenshotDestinationConflict
      saveScreenshotPng canonicalRoot "directory.png" "png"
        `shouldReturn` Left ScreenshotDestinationConflict
      assertNoTemporaryFiles root

  it "classifies a missing initialized root as unavailable" $
    withFreshTempDir "unavailable" $ \root -> do
      canonicalRoot <- canonicalizePath root
      removePathForcibly root
      saveScreenshotPng canonicalRoot "view.png" "png"
        `shouldReturn` Left ScreenshotStorageUnavailable

  it "rejects linked descendant parents without writing outside the sandbox" $
    withFreshTempDir "parent-link" $ \base -> do
      let root = base </> "root"
          outside = base </> "outside"
          linkedParent = root </> "linked"
      createDirectory root
      createDirectory outside
      linkResult <- try (createDirectoryLink outside linkedParent) :: IO (Either IOException ())
      case linkResult of
        Left err -> pendingWith ("directory links unavailable: " <> displayException err)
        Right () -> do
          canonicalRoot <- canonicalizePath root
          saveScreenshotPng canonicalRoot "linked/view.png" "png"
            `shouldReturn` Left ScreenshotUnsafePath
          listDirectory outside `shouldReturn` []
          removePathForcibly outside
          saveScreenshotPng canonicalRoot "linked/view.png" "png"
            `shouldReturn` Left ScreenshotUnsafePath

  it "rejects a linked destination and leaves its target unchanged" $
    withFreshTempDir "destination-link" $ \base -> do
      let root = base </> "root"
          outside = base </> "outside.png"
          linkedDestination = root </> "view.png"
      createDirectory root
      ByteString.writeFile outside "outside"
      linkResult <- try (createFileLink outside linkedDestination) :: IO (Either IOException ())
      case linkResult of
        Left err -> pendingWith ("file links unavailable: " <> displayException err)
        Right () -> do
          canonicalRoot <- canonicalizePath root
          saveScreenshotPng canonicalRoot "view.png" "replacement"
            `shouldReturn` Left ScreenshotUnsafePath
          ByteString.readFile outside `shouldReturn` "outside"
          removePathForcibly outside
          saveScreenshotPng canonicalRoot "view.png" "replacement"
            `shouldReturn` Left ScreenshotUnsafePath
          assertNoTemporaryFiles root

  it "never includes the absolute sandbox in typed failures" $
    withFreshTempDir "non-leaking" $ \root -> do
      canonicalRoot <- canonicalizePath root
      result <- saveScreenshotPng canonicalRoot "../outside.png" "png"
      result `shouldSatisfy` isLeft
      show result `shouldNotContain` root

observeUntilFinished
  :: MVar (Either ScreenshotSaveError Text)
  -> FilePath
  -> [ByteString.ByteString]
  -> IO (Either ScreenshotSaveError Text, [ByteString.ByteString])
observeUntilFinished done destination observed = do
  completed <- tryTakeMVar done
  case completed of
    Just result -> pure (result, observed)
    Nothing -> do
      bytes <- ByteString.readFile destination
      observeUntilFinished done destination (bytes : observed)

assertNoTemporaryFiles :: FilePath -> Expectation
assertNoTemporaryFiles directory = do
  entries <- listDirectory directory
  filter (".topo-screenshot-" `isPrefixOf`) entries `shouldBe` []

withFreshTempDir :: String -> (FilePath -> IO a) -> IO a
withFreshTempDir label action = do
  temp <- getTemporaryDirectory
  stamp <- round . (* (1000000 :: POSIXTime)) <$> getPOSIXTime
  let root = temp </> ("topo-screenshot-writer-" <> label <> "-" <> show (stamp :: Integer))
  bracket (createDirectory root >> pure root) cleanup action
  where
    cleanup path = do
      _ <- try (removePathForcibly path) :: IO (Either IOException ())
      pure ()
