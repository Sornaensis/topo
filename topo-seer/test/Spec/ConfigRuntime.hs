{-# LANGUAGE OverloadedStrings #-}

module Spec.ConfigRuntime (spec) where

import Control.Exception (IOException, bracket, displayException, try)
import Control.Monad (forM_)
import Data.Aeson (eitherDecode, encode)
import Data.List (isInfixOf)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import System.Directory
  ( canonicalizePath
  , createDirectory
  , createDirectoryLink
  , doesPathExist
  , getTemporaryDirectory
  , removePathForcibly
  , withCurrentDirectory
  )
import System.FilePath (addTrailingPathSeparator, (</>))
import Test.Hspec

import Seer.Config.Runtime (TopoSeerConfig(..), defaultConfig)
import Seer.Screenshot.Storage
  ( ScreenshotStoragePolicy(..)
  , initialiseScreenshotStorage
  )

spec :: Spec
spec = describe "screenshot storage runtime configuration" $ do
  it "decodes omitted and null settings as disabled and round-trips the field" $ do
    cfgScreenshotSaveDirectory defaultConfig `shouldBe` Nothing
    fmap cfgScreenshotSaveDirectory (eitherDecode "{}")
      `shouldBe` Right Nothing
    fmap cfgScreenshotSaveDirectory
      (eitherDecode "{\"screenshotSaveDirectory\":null}")
      `shouldBe` Right Nothing
    eitherDecode (encode defaultConfig) `shouldBe` Right defaultConfig

  it "performs no filesystem work when storage is disabled" $
    withFreshTempDir "disabled" $ \base -> do
      let candidate = base </> "screenshots"
      withCurrentDirectory base $
        initialiseScreenshotStorage Nothing `shouldReturn` ScreenshotStorageDisabled
      doesPathExist candidate `shouldReturn` False

  it "creates and canonicalizes an absolute configured root" $
    withFreshTempDir "enabled" $ \base -> do
      let configuredRoot = base </> "screenshots" </> "."
      policy <- initialiseScreenshotStorage (Just configuredRoot)
      canonicalRoot <- canonicalizePath configuredRoot
      policy `shouldBe` ScreenshotStorageEnabled canonicalRoot
      doesPathExist configuredRoot `shouldReturn` True

  it "rejects a relative configured root" $
    expectIOExceptionContaining "must be absolute" $
      initialiseScreenshotStorage (Just ("relative" </> "screenshots"))

  it "rejects file-valued and uncreatable roots" $
    withFreshTempDir "invalid" $ \base -> do
      let fileRoot = base </> "not-a-directory"
      writeFile fileRoot "occupied"
      expectIOExceptionContaining "not a directory" $
        initialiseScreenshotStorage (Just fileRoot)
      expectIOExceptionContaining "invalid screenshotSaveDirectory" $
        initialiseScreenshotStorage (Just (fileRoot </> "child"))

  it "rejects a configured root that is itself a symlink or reparse point" $
    withFreshTempDir "link" $ \base -> do
      let target = base </> "target"
          linkedRoot = base </> "linked-root"
      createDirectory target
      linkResult <- try (createDirectoryLink target linkedRoot) :: IO (Either IOException ())
      case linkResult of
        Left err -> pendingWith ("directory links unavailable: " <> displayException err)
        Right () -> forM_
          [ linkedRoot
          , addTrailingPathSeparator linkedRoot
          , linkedRoot </> "."
          ] $ \configuredSpelling ->
            expectIOExceptionContaining "symbolic link or reparse point" $
              initialiseScreenshotStorage (Just configuredSpelling)

expectIOExceptionContaining :: String -> IO a -> Expectation
expectIOExceptionContaining expected action = do
  result <- try (action >> pure ()) :: IO (Either IOException ())
  case result of
    Left err -> displayException err `shouldSatisfy` isInfixOf expected
    Right () -> expectationFailure ("expected IOException containing " <> show expected)

withFreshTempDir :: String -> (FilePath -> IO a) -> IO a
withFreshTempDir label action = do
  temp <- getTemporaryDirectory
  stamp <- round . (* (1000000 :: POSIXTime)) <$> getPOSIXTime
  let root = temp </> ("topo-screenshot-storage-" <> label <> "-" <> show (stamp :: Integer))
  bracket (createDirectory root >> pure root) removePathForcibly action
