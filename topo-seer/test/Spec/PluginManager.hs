{-# LANGUAGE OverloadedStrings #-}

module Spec.PluginManager (spec) where

import Control.Exception (bracket)
import qualified Data.ByteString as BS
import Hyperspace.Actor (getSingleton, newActorSystem, shutdownActorSystem)
import System.Directory
  ( copyFile
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , findExecutable
  , getCurrentDirectory
  , getHomeDirectory
  , listDirectory
  , removePathForcibly
  )
import System.FilePath ((</>))
import System.Info (os)
import System.Environment (lookupEnv)
import Test.Hspec

import Actor.PluginManager
  ( LoadedPlugin(..)
  , PluginStatus(..)
  , discoverPlugins
  , getLoadedPlugins
  , getPluginOverlaySchemas
  , getPluginStages
  , refreshManifests
  , pluginManagerActorDef
  )
import Topo.Overlay.Schema (OverlaySchema(..))

spec :: Spec
spec = describe "PluginManager" $ do
  it "loads declared .toposchema files during discovery" $ do
    withTestPluginDir testPluginName testManifestJSON testSchemaJSON $ do
      bracket newActorSystem shutdownActorSystem $ \system -> do
        pluginManagerHandle <- getSingleton system pluginManagerActorDef
        discoverPlugins pluginManagerHandle
        schemas <- getPluginOverlaySchemas pluginManagerHandle
        map osName schemas `shouldSatisfy` elem "copilot_test_overlay"

  it "launches plugin subprocesses and exposes generator stages after refresh" $ do
    if os /= "mingw32"
      then pendingWith "Windows-only subprocess fixture"
      else withTestExecutablePluginDir testLaunchPluginName testLaunchManifestJSON testLaunchCmd $ do
        bracket newActorSystem shutdownActorSystem $ \system -> do
          pluginManagerHandle <- getSingleton system pluginManagerActorDef
          discoverPlugins pluginManagerHandle
          refreshManifests pluginManagerHandle
          stages <- getPluginStages pluginManagerHandle
          loaded <- getLoadedPlugins pluginManagerHandle
          let launchPlugin = filter ((== "copilot-test-plugin-launch") . lpName) loaded
          length stages `shouldSatisfy` (> 0)
          map lpStatus launchPlugin `shouldSatisfy` elem PluginConnected

  it "launches a real topo-plugin-example executable when available" $ do
    if os /= "mingw32"
      then pendingWith "Windows-only subprocess fixture"
      else do
        mExampleExe <- findTopoPluginExampleExecutable
        case mExampleExe of
          Nothing -> pendingWith "topo-plugin-example executable not found (set TOPO_PLUGIN_EXAMPLE_EXE to enable)"
          Just exampleExe -> withRealExecutablePluginDir realPluginName realPluginManifestJSON exampleExe $ do
            bracket newActorSystem shutdownActorSystem $ \system -> do
              pluginManagerHandle <- getSingleton system pluginManagerActorDef
              discoverPlugins pluginManagerHandle
              refreshManifests pluginManagerHandle
              stages <- getPluginStages pluginManagerHandle
              loaded <- getLoadedPlugins pluginManagerHandle
              let launchPlugin = filter ((== "terrain-roughen") . lpName) loaded
              length stages `shouldSatisfy` (> 0)
              map lpStatus launchPlugin `shouldSatisfy` elem PluginConnected

withTestPluginDir :: String -> BS.ByteString -> BS.ByteString -> IO a -> IO a
withTestPluginDir pluginName manifestJSON schemaJSON action =
  bracket setup teardown (const action)
  where
    setup = do
      home <- getHomeDirectory
      let pluginDir = home </> ".topo" </> "plugins" </> pluginName
      createDirectoryIfMissing True pluginDir
      BS.writeFile (pluginDir </> "manifest.json") manifestJSON
      BS.writeFile (pluginDir </> "test.toposchema") schemaJSON
      pure pluginDir

    teardown = removePathForcibly

withTestExecutablePluginDir :: String -> BS.ByteString -> BS.ByteString -> IO a -> IO a
withTestExecutablePluginDir pluginName manifestJSON executableScript action =
  bracket setup teardown (const action)
  where
    setup = do
      home <- getHomeDirectory
      let pluginDir = home </> ".topo" </> "plugins" </> pluginName
      createDirectoryIfMissing True pluginDir
      BS.writeFile (pluginDir </> "manifest.json") manifestJSON
      BS.writeFile (pluginDir </> (pluginName <> ".cmd")) executableScript
      pure pluginDir

    teardown = removePathForcibly

withRealExecutablePluginDir :: String -> BS.ByteString -> FilePath -> IO a -> IO a
withRealExecutablePluginDir pluginName manifestJSON executablePath action =
  bracket setup teardown (const action)
  where
    setup = do
      home <- getHomeDirectory
      let pluginDir = home </> ".topo" </> "plugins" </> pluginName
          targetExe = pluginDir </> pluginName <> ".exe"
      createDirectoryIfMissing True pluginDir
      BS.writeFile (pluginDir </> "manifest.json") manifestJSON
      copyFile executablePath targetExe
      pure pluginDir

    teardown = removePathForcibly

findTopoPluginExampleExecutable :: IO (Maybe FilePath)
findTopoPluginExampleExecutable = do
  fromEnv <- lookupEnv "TOPO_PLUGIN_EXAMPLE_EXE"
  case fromEnv of
    Just path -> do
      exists <- doesFileExist path
      if exists then pure (Just path) else pure Nothing
    Nothing -> do
      fromPath <- findExecutable "topo-plugin-example.exe"
      case fromPath of
        Just path -> pure (Just path)
        Nothing -> do
          cwd <- getCurrentDirectory
          let candidateRoots = [cwd, cwd </> ".."]
          findFirstJustM findUnderStackInstall candidateRoots

findUnderStackInstall :: FilePath -> IO (Maybe FilePath)
findUnderStackInstall root = do
  let installDir = root </> ".stack-work" </> "install"
  exists <- doesDirectoryExist installDir
  if not exists
    then pure Nothing
    else findExecutableRecursively installDir "topo-plugin-example.exe"

findExecutableRecursively :: FilePath -> FilePath -> IO (Maybe FilePath)
findExecutableRecursively dir executableName = do
  entries <- listDirectory dir
  findFirstJustM checkEntry entries
  where
    checkEntry entry = do
      let path = dir </> entry
      isDir <- doesDirectoryExist path
      if isDir
        then findExecutableRecursively path executableName
        else if entry == executableName
          then pure (Just path)
          else pure Nothing

findFirstJustM :: (a -> IO (Maybe b)) -> [a] -> IO (Maybe b)
findFirstJustM _ [] = pure Nothing
findFirstJustM f (x:xs) = do
  result <- f x
  case result of
    Just value -> pure (Just value)
    Nothing -> findFirstJustM f xs

testPluginName :: String
testPluginName = "copilot-test-plugin-manager-schema"

testManifestJSON :: BS.ByteString
testManifestJSON =
  "{\n"
    <> "  \"name\": \"copilot-test-plugin-manager-schema\",\n"
    <> "  \"version\": \"0.1.0\",\n"
    <> "  \"generator\": { \"insertAfter\": \"erosion\" },\n"
    <> "  \"overlay\": { \"schemaFile\": \"test.toposchema\" }\n"
    <> "}\n"

testSchemaJSON :: BS.ByteString
testSchemaJSON =
  "{\n"
    <> "  \"name\": \"copilot_test_overlay\",\n"
    <> "  \"version\": \"1.0.0\",\n"
    <> "  \"storage\": \"sparse\",\n"
    <> "  \"fields\": [\n"
    <> "    {\n"
    <> "      \"name\": \"value\",\n"
    <> "      \"type\": \"float\",\n"
    <> "      \"default\": 0.0,\n"
    <> "      \"indexed\": false\n"
    <> "    }\n"
    <> "  ]\n"
    <> "}\n"

testLaunchPluginName :: String
testLaunchPluginName = "copilot-test-plugin-launch"

testLaunchManifestJSON :: BS.ByteString
testLaunchManifestJSON =
  "{\n"
    <> "  \"name\": \"copilot-test-plugin-launch\",\n"
    <> "  \"version\": \"0.1.0\",\n"
    <> "  \"generator\": { \"insertAfter\": \"erosion\" }\n"
    <> "}\n"

testLaunchCmd :: BS.ByteString
testLaunchCmd =
  "@echo off\r\n"
    <> "exit /b 0\r\n"

realPluginName :: String
realPluginName = "terrain-roughen"

realPluginManifestJSON :: BS.ByteString
realPluginManifestJSON =
  "{\n"
    <> "  \"name\": \"terrain-roughen\",\n"
    <> "  \"version\": \"0.1.0\",\n"
    <> "  \"generator\": { \"insertAfter\": \"erosion\" }\n"
    <> "}\n"
