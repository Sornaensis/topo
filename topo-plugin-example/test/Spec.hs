{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Data.Aeson as Aeson
import Test.Hspec

import Topo.Plugin.Example (terrainRoughenPlugin)
import Topo.Plugin.RPC.Manifest
  ( Capability(..)
  , RPCManifest(..), RPCManifestRuntime(..), RPCUIHints(..)
  , manifestV3, validateManifest
  )
import Topo.Plugin.RPC.Protocol (currentProtocolVersion)
import Topo.Plugin.SDK (generateManifest)

main :: IO ()
main = hspec $ do
  describe "topo-plugin-example manifest" $ do
    it "generates a valid manifest v3 from PluginDef" $ do
      let manifest = generateManifest terrainRoughenPlugin
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
      let encoded = Aeson.encode (generateManifest terrainRoughenPlugin)
      case Aeson.eitherDecode encoded of
        Left err -> expectationFailure err
        Right (decoded :: RPCManifest) -> do
          rmName decoded `shouldBe` "terrain-roughen"
          validateManifest decoded `shouldBe` []
