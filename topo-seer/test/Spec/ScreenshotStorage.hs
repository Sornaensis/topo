{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec.ScreenshotStorage (spec) where


import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
  ( MVar
  , modifyMVar_
  , newEmptyMVar
  , newMVar
  , putMVar
  , readMVar
  , takeMVar
  )
import Control.Exception (IOException, bracket, displayException, try)
import Control.Monad (forM_, replicateM)
import qualified Data.ByteString as ByteString
import Data.Either (isLeft)
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import System.Directory
  ( createDirectory
  , createDirectoryLink
  , createFileLink
  , getTemporaryDirectory
  , listDirectory
  , removeFile
  , removePathForcibly
  , renameDirectory
  )
import System.FilePath ((</>))
import System.Timeout (timeout)
import Test.Hspec

import Seer.Screenshot.Storage
  ( ScreenshotSaveError(..)
  , ScreenshotStoragePolicy(..)
  , ScreenshotStorageRoot
  , initialiseScreenshotStorage
  , saveScreenshotPng
  , saveScreenshotPngWithCommitHook
  , validateScreenshotPngPath
  )

spec :: Spec
spec = describe "capability-confined screenshot PNG persistence" $ do
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
      validateScreenshotPngPath (Text.replicate 256 "a" <> ".png")
        `shouldBe` Left ScreenshotInvalidPath
      validateScreenshotPngPath (Text.replicate 128 "é" <> ".png")
        `shouldBe` Left ScreenshotInvalidPath

    it "preserves ordinary Unicode and spaces in normalized relative output" $
      validateScreenshotPngPath "maps/été world/山 view.png"
        `shouldBe` Right "maps/été world/山 view.png"

#if defined(linux_HOST_OS)
  it "fails closed when a Linux filesystem does not support unnamed publication" $ do
    policy <- initialiseScreenshotStorage (Just "/proc")
    case policy of
      ScreenshotStorageDisabled -> expectationFailure "configured storage was disabled"
      ScreenshotStorageEnabled storage ->
        saveScreenshotPng storage "view.png" "png"
          `shouldReturn` Left ScreenshotStorageUnavailable
#endif

#if defined(mingw32_HOST_OS) || defined(linux_HOST_OS)
  it "creates nested parents, writes exact bytes, and returns only the relative path" $
    withFreshStorage "nested" $ \root storage -> do
      let bytes = "\137PNG\r\n\SUB\ncomplete"
      saveScreenshotPng storage "maps/region one/view.png" bytes
        `shouldReturn` Right "maps/region one/view.png"
      ByteString.readFile (root </> "maps" </> "region one" </> "view.png")
        `shouldReturn` bytes
      assertNoTemporaryFiles (root </> "maps" </> "region one")

  it "is create-only and leaves an existing regular PNG unchanged" $
    withFreshStorage "existing" $ \root storage -> do
      let destination = root </> "view.png"
      ByteString.writeFile destination "sentinel"
      saveScreenshotPng storage "view.png" "replacement"
        `shouldReturn` Left ScreenshotDestinationConflict
      ByteString.readFile destination `shouldReturn` "sentinel"
      assertNoTemporaryFiles root

  it "publishes exactly one complete image for simultaneous create attempts" $
    withFreshStorage "concurrent-create" $ \root storage -> do
      arrived <- newMVar (0 :: Int)
      release <- newEmptyMVar
      results <- replicateM 2 newEmptyMVar
      let payloads = [ByteString.replicate (1024 * 1024) 31, ByteString.replicate (1024 * 1024) 47]
          hook = do
            modifyMVar_ arrived (pure . (+ 1))
            _ <- readMVar release
            pure ()
      forM_ (zip payloads results) $ \(payload, resultVar) -> do
        _ <- forkIO $ saveScreenshotPngWithCommitHook hook storage "view.png" payload
          >>= putMVar resultVar
        pure ()
      waitForCount arrived 2
      putMVar release ()
      outcomes <- awaitWithin "simultaneous create results" (mapM takeMVar results)
      length (filter (== Right "view.png") outcomes) `shouldBe` 1
      length (filter (== Left ScreenshotDestinationConflict) outcomes) `shouldBe` 1
      finalBytes <- ByteString.readFile (root </> "view.png")
      finalBytes `shouldSatisfy` (`elem` payloads)
      assertNoTemporaryFiles root

  it "keeps using the retained root identity after replacement by a fresh directory" $
    withFreshStorage "root-replaced" $ \root storage -> do
      let displaced = root <> "-retained"
      renameDirectory root displaced
      createDirectory root
      saveScreenshotPng storage "view.png" "confined"
        `shouldReturn` Right "view.png"
      listDirectory root `shouldReturn` []
      ByteString.readFile (displaced </> "view.png") `shouldReturn` "confined"
      assertNoTemporaryFiles displaced

  it "does not follow a symlink installed at the configured root after initialization" $
    withFreshStorage "root-link-replaced" $ \root storage -> do
      let displaced = root <> "-retained"
          outside = root <> "-outside"
      renameDirectory root displaced
      createDirectory outside
      linkResult <- try (createDirectoryLink outside root) :: IO (Either IOException ())
      case linkResult of
        Left err -> pendingWith ("directory links unavailable: " <> displayException err)
        Right () -> do
          saveScreenshotPng storage "view.png" "confined"
            `shouldReturn` Right "view.png"
          listDirectory outside `shouldReturn` []
          ByteString.readFile (displaced </> "view.png") `shouldReturn` "confined"
          assertNoTemporaryFiles displaced

  it "keeps using retained descendant identity when replaced by a fresh directory" $
    withFreshStorage "parent-replaced" $ \root storage -> do
      let parent = root </> "maps"
          displaced = root </> "maps-retained"
          replaceParent = do
            renameDirectory parent displaced
            createDirectory parent
      createDirectory parent
      saveScreenshotPngWithCommitHook replaceParent storage "maps/view.png" "confined"
        `shouldReturn` Right "maps/view.png"
      listDirectory parent `shouldReturn` []
      ByteString.readFile (displaced </> "view.png") `shouldReturn` "confined"
      assertNoTemporaryFiles displaced

  it "does not follow a symlink installed at a retained parent before publication" $
    withFreshStorage "parent-link-replaced" $ \root storage -> do
      let parent = root </> "maps"
          displaced = root </> "maps-retained"
          outside = root <> "-outside"
          replaceParent = do
            renameDirectory parent displaced
            createDirectoryLink outside parent
      createDirectory parent
      createDirectory outside
      linkProbe <- try (createDirectoryLink outside (root </> "link-probe"))
        :: IO (Either IOException ())
      case linkProbe of
        Left err -> pendingWith ("directory links unavailable: " <> displayException err)
        Right () -> do
          removePathForcibly (root </> "link-probe")
          saveScreenshotPngWithCommitHook replaceParent storage "maps/view.png" "confined"
            `shouldReturn` Right "maps/view.png"
          listDirectory outside `shouldReturn` []
          ByteString.readFile (displaced </> "view.png") `shouldReturn` "confined"
          assertNoTemporaryFiles displaced

  it "confines coordinated writers when their retained parent is replaced" $
    withFreshStorage "coordinated-replacement" $ \root storage -> do
      let parent = root </> "maps"
          displaced = root </> "maps-retained"
          payloads = [("one.png", "complete-one"), ("two.png", "complete-two")]
      createDirectory parent
      arrived <- newMVar (0 :: Int)
      release <- newEmptyMVar
      results <- replicateM 2 newEmptyMVar
      let hook = do
            modifyMVar_ arrived (pure . (+ 1))
            _ <- readMVar release
            pure ()
      forM_ (zip payloads results) $ \((name, payload), resultVar) -> do
        _ <- forkIO $ saveScreenshotPngWithCommitHook hook storage ("maps/" <> name) payload
          >>= putMVar resultVar
        pure ()
      waitForCount arrived 2
      renameDirectory parent displaced
      createDirectory parent
      putMVar release ()
      outcomes <- awaitWithin "coordinated replacement results" (mapM takeMVar results)
      outcomes `shouldBe` [Right "maps/one.png", Right "maps/two.png"]
      listDirectory parent `shouldReturn` []
      forM_ payloads $ \(name, payload) ->
        ByteString.readFile (displaced </> Text.unpack name) `shouldReturn` payload
      assertNoTemporaryFiles displaced

  it "loses create-new publication to an inserted destination without changing it" $
    withFreshStorage "destination-race" $ \root storage -> do
      let destination = root </> "view.png"
      saveScreenshotPngWithCommitHook
        (ByteString.writeFile destination "sentinel")
        storage
        "view.png"
        "replacement"
        `shouldReturn` Left ScreenshotDestinationConflict
      ByteString.readFile destination `shouldReturn` "sentinel"
      assertNoTemporaryFiles root

  it "leaves no namespace staging file after a pre-publication failure" $
    withFreshStorage "commit-failure" $ \root storage -> do
      saveScreenshotPngWithCommitHook
        (ioError (userError "injected pre-publication failure"))
        storage
        "view.png"
        "complete-new-png"
        `shouldReturn` Left ScreenshotUnexpectedIO
      listDirectory root `shouldReturn` []

  it "rejects file-valued parents and directory-valued destinations as conflicts" $
    withFreshStorage "conflict" $ \root storage -> do
      writeFile (root </> "occupied") "not a directory"
      createDirectory (root </> "directory.png")
      saveScreenshotPng storage "occupied/view.png" "png"
        `shouldReturn` Left ScreenshotDestinationConflict
      saveScreenshotPng storage "directory.png" "png"
        `shouldReturn` Left ScreenshotDestinationConflict
      assertNoTemporaryFiles root

  it "rejects linked descendant parents without writing outside the sandbox" $
    withFreshStorage "parent-link" $ \root storage -> do
      let outside = root <> "-outside"
          linkedParent = root </> "linked"
      createDirectory outside
      linkResult <- try (createDirectoryLink outside linkedParent) :: IO (Either IOException ())
      case linkResult of
        Left err -> pendingWith ("directory links unavailable: " <> displayException err)
        Right () -> do
          saveScreenshotPng storage "linked/view.png" "png"
            `shouldReturn` Left ScreenshotUnsafePath
          listDirectory outside `shouldReturn` []
          removePathForcibly outside

  it "treats a linked destination as a create-only conflict and preserves its target" $
    withFreshStorage "destination-link" $ \root storage -> do
      let outside = root <> "-outside.png"
          linkedDestination = root </> "view.png"
      ByteString.writeFile outside "outside"
      linkResult <- try (createFileLink outside linkedDestination) :: IO (Either IOException ())
      case linkResult of
        Left err -> pendingWith ("file links unavailable: " <> displayException err)
        Right () -> do
          saveScreenshotPng storage "view.png" "replacement"
            `shouldReturn` Left ScreenshotDestinationConflict
          ByteString.readFile outside `shouldReturn` "outside"

  it "never includes the storage path in public values or typed failures" $
    withFreshStorage "non-leaking" $ \root storage -> do
      show (ScreenshotStorageEnabled storage) `shouldNotContain` root
      result <- saveScreenshotPng storage "../outside.png" "png"
      result `shouldSatisfy` isLeft
      show result `shouldNotContain` root
#else
  it "fails configured persistence closed on unsupported POSIX platforms" $
    withFreshStorage "unsupported" $ \_ storage ->
      saveScreenshotPng storage "view.png" "png"
        `shouldReturn` Left ScreenshotStorageUnavailable
#endif

waitForCount :: MVar Int -> Int -> IO ()
waitForCount counter expected =
  awaitWithin "workers reaching the commit hook" loop
  where
    loop = do
      current <- readMVar counter
      if current >= expected then pure () else threadDelay 1000 >> loop

awaitWithin :: String -> IO a -> IO a
awaitWithin label action = do
  result <- timeout 10000000 action
  case result of
    Just value -> pure value
    Nothing -> fail ("timed out waiting for " <> label)

assertNoTemporaryFiles :: FilePath -> Expectation
assertNoTemporaryFiles directory = do
  entries <- listDirectory directory
  filter (".topo-screenshot-" `isPrefixOf`) entries `shouldBe` []

withFreshStorage :: String -> (FilePath -> ScreenshotStorageRoot -> IO a) -> IO a
withFreshStorage label action =
  withFreshTempDir label $ \root -> do
    policy <- initialiseScreenshotStorage (Just root)
    case policy of
      ScreenshotStorageDisabled -> expectationFailure "configured storage was disabled" >> error "unreachable"
      ScreenshotStorageEnabled storage -> do
#if defined(linux_HOST_OS)
        probe <- saveScreenshotPng storage ".topo-capability-probe.png" "probe"
        case probe of
          Right _ -> removeFile (root </> ".topo-capability-probe.png")
          Left ScreenshotStorageUnavailable ->
            pendingWith "temporary filesystem lacks O_TMPFILE/linkat(AT_EMPTY_PATH) support"
          Left err -> expectationFailure ("storage capability probe failed: " <> show err)
#endif
        action root storage

withFreshTempDir :: String -> (FilePath -> IO a) -> IO a
withFreshTempDir label action = do
  temp <- getTemporaryDirectory
  stamp <- round . (* (1000000 :: POSIXTime)) <$> getPOSIXTime
  let root = temp </> ("topo-screenshot-writer-" <> label <> "-" <> show (stamp :: Integer))
  bracket (createDirectory root >> pure root) cleanup action
  where
    cleanup path = do
      _ <- try (removePathForcibly path) :: IO (Either IOException ())
      _ <- try (removePathForcibly (path <> "-retained")) :: IO (Either IOException ())
      _ <- try (removePathForcibly (path <> "-outside")) :: IO (Either IOException ())
      _ <- try (removePathForcibly (path <> "-outside.png")) :: IO (Either IOException ())
      pure ()
