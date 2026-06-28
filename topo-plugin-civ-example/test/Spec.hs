{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified Data.Aeson as Aeson
import Test.Hspec

import Topo.Plugin.CivExample (civPlugin)
import Topo.Plugin.RPC.Manifest
  ( Capability(..)
  , RPCExternalDataSourceCapability(..)
  , RPCExternalDataSourceDecl(..)
  , RPCExternalDataSourceGrant(..)
  , RPCManifest(..), RPCManifestRuntime(..), RPCUIHints(..)
  , manifestV3, validateManifest
  )
import Topo.Plugin.RPC.Protocol (currentProtocolVersion)
import Topo.Plugin.SDK (generateManifest)

main :: IO ()
main = hspec $ do
  describe "topo-plugin-civ-example manifest" $ do
    it "generates a valid manifest v3 from PluginDef" $ do
      let manifest = generateManifest civPlugin
      rmManifestVersion manifest `shouldBe` manifestV3
      rmrProtocolMin (rmRuntime manifest) `shouldBe` currentProtocolVersion
      rmrProtocolMax (rmRuntime manifest) `shouldBe` currentProtocolVersion
      rmrTopoMin (rmRuntime manifest) `shouldBe` Just "1.0"
      ruiDisplayName (rmUiHints manifest) `shouldBe` Just "Civilization"
      rmGenerator manifest `shouldSatisfy` maybe False (const True)
      rmSimulation manifest `shouldSatisfy` maybe False (const True)
      rmOverlay manifest `shouldSatisfy` maybe False (const True)
      rmDataDirectory manifest `shouldBe` Just "civilization-data"
      length (rmDataResources manifest) `shouldBe` 2
      length (rmExternalDataSources manifest) `shouldBe` 1
      rmCapabilities manifest `shouldSatisfy` elem CapLog
      rmCapabilities manifest `shouldSatisfy` elem CapReadTerrain
      rmCapabilities manifest `shouldSatisfy` elem CapReadOverlay
      rmCapabilities manifest `shouldSatisfy` elem CapWriteOverlay
      rmCapabilities manifest `shouldSatisfy` elem CapDataRead
      rmCapabilities manifest `shouldSatisfy` notElem CapWriteTerrain
      rmCapabilities manifest `shouldSatisfy` notElem CapDataWrite
      validateManifest manifest `shouldBe` []

    it "preserves backend-neutral external data-source declarations" $ do
      let manifest = generateManifest civPlugin
      case rmExternalDataSources manifest of
        [source] -> do
          redsdName source `shouldBe` "settlement-ledger"
          redsdKind source `shouldBe` "catalog"
          redsdCapabilities source `shouldBe` [ExternalSourceQuery, ExternalSourceHealth]
          redsdResources source `shouldBe` ["settlements", "cultures"]
          map redsgName (redsdGrants source) `shouldBe` ["settlement-read"]
        _ -> expectationFailure "expected exactly one external data source"

    it "round-trips through manifest JSON" $ do
      let encoded = Aeson.encode (generateManifest civPlugin)
      case Aeson.eitherDecode encoded of
        Left err -> expectationFailure err
        Right (decoded :: RPCManifest) -> do
          rmName decoded `shouldBe` "civilization"
          validateManifest decoded `shouldBe` []
