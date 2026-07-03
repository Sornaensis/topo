{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Exception (bracket)
import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import System.Directory
  ( createDirectory
  , doesFileExist
  , getTemporaryDirectory
  , removeFile
  , removePathForcibly
  )
import System.Environment (withArgs)
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import Test.Hspec

import qualified Topo.Plugin.Example as Example
import Topo.Plugin.RPC.Manifest
  ( Capability(..)
  , RPCManifest(..), RPCManifestRuntime(..), RPCUIHints(..)
  , manifestV3, parseManifestFile, validateManifest
  )
import Topo.Plugin.RPC.Protocol (currentProtocolVersion)
import Topo.Plugin.SDK (generateManifest, pluginManifestFileName)

main :: IO ()
main = hspec $ do
  describe "topo-plugin-example manifest" $ do
    it "generates a valid manifest v3 from PluginDef" $ do
      let manifest = generateManifest Example.terrainRoughenPlugin
      rmManifestVersion manifest `shouldBe` manifestV3
      rmrProtocolMin (rmRuntime manifest) `shouldBe` currentProtocolVersion
      rmrProtocolMax (rmRuntime manifest) `shouldBe` currentProtocolVersion
      rmrTopoMin (rmRuntime manifest) `shouldBe` Just "1.0"
      ruiDisplayName (rmUiHints manifest) `shouldBe` Just "Terrain Roughen"
      rmCapabilities manifest `shouldSatisfy` elem CapLog
      rmCapabilities manifest `shouldSatisfy` elem CapReadTerrain
      rmCapabilities manifest `shouldSatisfy` notElem CapWriteTerrain
      length (rmParameters manifest) `shouldBe` 2
      validateManifest manifest `shouldBe` []

    it "round-trips through manifest JSON" $ do
      let encoded = Aeson.encode (generateManifest Example.terrainRoughenPlugin)
      case Aeson.eitherDecode encoded of
        Left err -> expectationFailure err
        Right (decoded :: RPCManifest) -> do
          rmName decoded `shouldBe` "terrain-roughen"
          validateManifest decoded `shouldBe` []

    it "packages an install directory with a generated manifest.json beside the executable" $
      withTempDir "topo-plugin-example-install" $ \pluginDir -> do
        let executablePath = pluginDir </> "terrain-roughen"
        writeFile executablePath "installed executable placeholder"
        withArgs ["--topo-write-manifest", pluginDir] Example.main
        let manifestPath = pluginDir </> pluginManifestFileName
        doesFileExist executablePath `shouldReturn` True
        doesFileExist manifestPath `shouldReturn` True
        readGeneratedManifest manifestPath $ \manifest -> do
          rmName manifest `shouldBe` "terrain-roughen"
          doesFileExist (pluginDir </> Text.unpack (rmName manifest)) `shouldReturn` True
          validateManifest manifest `shouldBe` []

withTempDir :: String -> (FilePath -> IO a) -> IO a
withTempDir label action = bracket setup cleanup action
  where
    setup = do
      tmp <- getTemporaryDirectory
      (path, handle) <- openTempFile tmp label
      hClose handle
      removeFile path
      createDirectory path
      pure path
    cleanup dir = removePathForcibly dir

readGeneratedManifest :: FilePath -> (RPCManifest -> Expectation) -> Expectation
readGeneratedManifest path check = do
  result <- parseManifestFile path
  case result of
    Left err -> expectationFailure ("manifest parse failed: " <> show err)
    Right manifest -> check manifest
