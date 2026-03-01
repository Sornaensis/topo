{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Unified world-bundle persistence coordinator.
--
-- A world bundle consists of:
--
-- * the core @.topo@ world file, and
-- * an adjacent @.topolay@ directory containing overlay schema/data files.
--
-- This module coordinates the two lower-level persistence layers:
-- 'Topo.Storage' (core world) and 'Topo.Overlay.Storage' (overlay sidecar).
module Topo.Persistence.WorldBundle
  ( BundleLoadPolicy(..)
  , WorldBundleError(..)
  , BundleSaveHooks(..)
  , defaultBundleSaveHooks
  , saveWorldBundle
  , saveWorldBundleWithProvenance
  , saveWorldBundleWithProvenanceAndHooks
  , loadWorldBundle
  , loadWorldBundleWithProvenance
  ) where

import qualified Data.Map.Strict as Map
import Control.Exception (IOException, try)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , removePathForcibly
  , renameDirectory
  )
import System.FilePath ((</>), takeDirectory, takeFileName)

import Topo.Overlay (OverlayStore(..), overlayNames)
import Topo.Overlay.Storage
  ( OverlayStorageError(..)
  , overlayDirPath
  , overlaySchemaPath
  , saveOverlayStore
  , loadOverlayStore
  )
import Topo.Overlay.Schema (OverlaySchema, osName, parseOverlaySchema)
import Topo.Storage
  ( StorageError
  , WorldProvenance
  , emptyProvenance
  , saveWorldWithProvenance
  , loadWorldWithProvenance
  )
import Topo.World (TerrainWorld(..))

-- | Overlay loading policy for world-bundle reads.
data BundleLoadPolicy
  = StrictManifest
  | BestEffort
  deriving (Eq, Show)

-- | Unified world-bundle persistence errors.
data WorldBundleError
  = BundleStorageError !StorageError
  | BundleOverlayError !OverlayStorageError
  | BundleMissingOverlays ![Text]
  | BundleExtraOverlays ![Text]
  | BundleSchemaReadError !Text
  | BundleAtomicRenameError !Text
  deriving (Show)

data BundleSaveHooks = BundleSaveHooks
  { bshAfterBackupRename :: IO ()
  }

defaultBundleSaveHooks :: BundleSaveHooks
defaultBundleSaveHooks = BundleSaveHooks
  { bshAfterBackupRename = pure ()
  }

-- | Save a world bundle using empty provenance.
saveWorldBundle :: FilePath -> TerrainWorld -> IO (Either WorldBundleError ())
saveWorldBundle topoPath = saveWorldBundleWithProvenance topoPath emptyProvenance []

-- | Save a world bundle (@.topo@ + @.topolay@ sidecar).
--
-- The saved world's overlay manifest is normalised from the current
-- overlay store to keep load-time expectations explicit. Extra files
-- are written into the same staging directory and committed atomically
-- with the world bundle (relative to the world directory).
saveWorldBundleWithProvenance
  :: FilePath
  -> WorldProvenance
  -> [(FilePath, BS.ByteString)]
  -> TerrainWorld
  -> IO (Either WorldBundleError ())
saveWorldBundleWithProvenance topoPath prov extraFiles world =
  saveWorldBundleWithProvenanceAndHooks defaultBundleSaveHooks topoPath prov extraFiles world

saveWorldBundleWithProvenanceAndHooks
  :: BundleSaveHooks
  -> FilePath
  -> WorldProvenance
  -> [(FilePath, BS.ByteString)]
  -> TerrainWorld
  -> IO (Either WorldBundleError ())
saveWorldBundleWithProvenanceAndHooks hooks topoPath prov extraFiles world = do
  let overlays = twOverlays world
      world' = world { twOverlayManifest = overlayNames overlays }
      targetDir = takeDirectory topoPath
      targetName = takeFileName topoPath
      stagingDir = targetDir <> ".saving"
      backupDir = targetDir <> ".old"
      stagingTopo = stagingDir </> targetName
      stagingSidecar = overlayDirPath stagingTopo

  cleanResult <- cleanupAtomicDirs stagingDir backupDir
  case cleanResult of
    Left err -> pure (Left err)
    Right () -> do
      createResult <- safeIO "create staging dir" (createDirectoryIfMissing True stagingDir)
      case createResult of
        Left err -> pure (Left err)
        Right () -> do
          topoResult <- saveWorldWithProvenance stagingTopo prov world'
          case topoResult of
            Left err -> pure (Left (BundleStorageError err))
            Right () -> do
              overlayResult <- saveOverlayStore stagingSidecar overlays
              case overlayResult of
                Left err -> pure (Left (BundleOverlayError err))
                Right () -> do
                  extraResult <- writeExtraFiles stagingDir extraFiles
                  case extraResult of
                    Left err -> pure (Left err)
                    Right () -> do
                      commitResult <- commitAtomicBundle hooks targetDir stagingDir backupDir
                      pure commitResult

-- | Load a world bundle using a load policy.
--
-- * 'StrictManifest': if the manifest declares overlays, all must be
--   present and load successfully from sidecar files.
-- * 'BestEffort': if sidecar loading fails, keep overlays already
--   present in the loaded @.topo@ world.
loadWorldBundle
  :: BundleLoadPolicy
  -> FilePath
  -> IO (Either WorldBundleError TerrainWorld)
loadWorldBundle policy topoPath = do
  result <- loadWorldBundleWithProvenance policy topoPath
  pure (fmap snd result)

-- | Load a world bundle with provenance.
loadWorldBundleWithProvenance
  :: BundleLoadPolicy
  -> FilePath
  -> IO (Either WorldBundleError (WorldProvenance, TerrainWorld))
loadWorldBundleWithProvenance policy topoPath = do
  topoResult <- loadWorldWithProvenance topoPath
  case topoResult of
    Left err -> pure (Left (BundleStorageError err))
    Right (prov, world) -> do
      let expected = twOverlayManifest world
          sidecarDir = overlayDirPath topoPath
      schemaResult <- readManifestSchemas sidecarDir expected
      case schemaResult of
        Left err ->
          pure $ case policy of
            StrictManifest -> Left err
            BestEffort     -> Right (prov, world)
        Right schemas -> do
          overlaysResult <- loadOverlayStore sidecarDir schemas
          case overlaysResult of
            Left err ->
              pure $ case policy of
                StrictManifest -> Left (BundleOverlayError err)
                BestEffort     -> Right (prov, world)
            Right overlays ->
              pure (Right (prov, world { twOverlays = mergeOverlays world overlays }))

-- Keep weather/legacy overlays loaded from .topo unless explicitly
-- replaced by sidecar entries.
mergeOverlays :: TerrainWorld -> OverlayStore -> OverlayStore
mergeOverlays world (OverlayStore sidecar) =
  let OverlayStore core = twOverlays world
  in OverlayStore (Map.union sidecar core)

readManifestSchemas
  :: FilePath
  -> [Text]
  -> IO (Either WorldBundleError [OverlaySchema])
readManifestSchemas _ [] = pure (Right [])
readManifestSchemas sidecarDir names = do
  missing <- findMissing names
  if null missing
    then go names []
    else pure (Left (BundleMissingOverlays missing))
  where
    findMissing [] = pure []
    findMissing (name:rest) = do
      let schemaFile = overlaySchemaPath sidecarDir name
      exists <- doesFileExist schemaFile
      next <- findMissing rest
      pure (if exists then next else name : next)

    go [] acc = pure (Right (reverse acc))
    go (name:rest) acc = do
      let schemaFile = overlaySchemaPath sidecarDir name
      bytesResult <- safeRead schemaFile
      case bytesResult of
        Left err -> pure (Left err)
        Right bytes ->
          case parseOverlaySchema bytes of
            Left parseErr ->
              pure (Left (BundleSchemaReadError ("failed to parse schema for " <> name <> ": " <> parseErr)))
            Right schema
              | osName schema /= name ->
                  pure (Left (BundleSchemaReadError ("schema name mismatch for " <> name)))
              | otherwise -> go rest (schema : acc)

cleanupAtomicDirs :: FilePath -> FilePath -> IO (Either WorldBundleError ())
cleanupAtomicDirs stagingDir backupDir = do
  clearStaging <- clearOne stagingDir
  case clearStaging of
    Left err -> pure (Left err)
    Right () -> clearOne backupDir
  where
    clearOne path = do
      exists <- doesDirectoryExist path
      if exists
        then do
          cleanup <- safeIO "cleanup stale atomic directory" (removePathForcibly path)
          case cleanup of
            Left err -> pure (Left err)
            Right () -> pure (Right ())
        else pure (Right ())

writeExtraFiles :: FilePath -> [(FilePath, BS.ByteString)] -> IO (Either WorldBundleError ())
writeExtraFiles _ [] = pure (Right ())
writeExtraFiles stagingDir files = go files
  where
    go [] = pure (Right ())
    go ((relPath, bytes):rest) = do
      let target = stagingDir </> relPath
      dirResult <- safeIO "create extra file directory" (createDirectoryIfMissing True (takeDirectory target))
      case dirResult of
        Left err -> pure (Left err)
        Right () -> do
          writeResult <- safeIO "write extra bundle file" (BS.writeFile target bytes)
          case writeResult of
            Left err -> pure (Left err)
            Right () -> go rest

commitAtomicBundle :: BundleSaveHooks -> FilePath -> FilePath -> FilePath -> IO (Either WorldBundleError ())
commitAtomicBundle hooks targetDir stagingDir backupDir = do
  targetExists <- doesDirectoryExist targetDir
  if targetExists
    then do
      backupOld <- safeIO "rename existing world to backup" (renameDirectory targetDir backupDir)
      case backupOld of
        Left err -> pure (Left err)
        Right () -> do
          hookResult <- safeIO "post-backup commit hook" (bshAfterBackupRename hooks)
          case hookResult of
            Left err -> do
              _ <- restoreBackupIfNeeded targetDir backupDir
              pure (Left err)
            Right () -> finalize
    else finalize
  where
    restoreBackupIfNeeded target backup = do
      backupExists <- doesDirectoryExist backup
      targetExists <- doesDirectoryExist target
      if backupExists && not targetExists
        then safeIO "restore backup world" (renameDirectory backup target)
        else pure (Right ())

    finalize = do
      moveNew <- safeIO "rename staging world into place" (renameDirectory stagingDir targetDir)
      case moveNew of
        Left err -> do
          _ <- restoreBackupIfNeeded targetDir backupDir
          pure (Left err)
        Right () -> do
          backupExists <- doesDirectoryExist backupDir
          if backupExists
            then do
              clearBackup <- safeIO "remove old world backup" (removePathForcibly backupDir)
              case clearBackup of
                Left err -> pure (Left err)
                Right () -> pure (Right ())
            else pure (Right ())

safeRead :: FilePath -> IO (Either WorldBundleError BS.ByteString)
safeRead path = do
  result <- try @IOException (BS.readFile path)
  pure $ case result of
    Left err -> Left (BundleSchemaReadError ("failed to read schema file: " <> Text.pack (show err)))
    Right bytes -> Right bytes

safeIO :: Text -> IO a -> IO (Either WorldBundleError a)
safeIO context ioAction = do
  result <- try @IOException ioAction
  pure $ case result of
    Left err -> Left (BundleAtomicRenameError (context <> ": " <> Text.pack (show err)))
    Right val -> Right val
