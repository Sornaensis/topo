{-# LANGUAGE OverloadedStrings #-}

-- | Atomic directory commit helpers for multi-file persistence flows.
--
-- The staged directory is promoted by directory rename, with optional
-- backup/restore handling for an existing target directory.
module Topo.Persistence.AtomicDirectory
  ( cleanupAtomicDirs
  , commitAtomicDirectory
  ) where

import Data.Text (Text)
import System.Directory
  ( doesDirectoryExist
  , removePathForcibly
  , renameDirectory
  )

-- | Remove stale staging/backup directories before an atomic save.
cleanupAtomicDirs
  :: (Text -> IO () -> IO (Either e ()))
  -> FilePath
  -> FilePath
  -> IO (Either e ())
cleanupAtomicDirs safeRun stagingDir backupDir = do
  clearStaging <- clearOne stagingDir
  case clearStaging of
    Left err -> pure (Left err)
    Right () -> clearOne backupDir
  where
    clearOne path = do
      exists <- doesDirectoryExist path
      if exists
        then do
          cleanup <- safeRun "cleanup stale atomic directory" (removePathForcibly path)
          case cleanup of
            Left err -> pure (Left err)
            Right () -> pure (Right ())
        else pure (Right ())

-- | Promote a staging directory into place atomically.
--
-- If the target exists, it is first moved to backup. If promotion fails,
-- backup restoration is attempted. On success, backup is removed.
commitAtomicDirectory
  :: (Text -> IO () -> IO (Either e ()))
  -> IO ()
  -> FilePath
  -> FilePath
  -> FilePath
  -> IO (Either e ())
commitAtomicDirectory safeRun afterBackupRename targetDir stagingDir backupDir = do
  targetExists <- doesDirectoryExist targetDir
  if targetExists
    then do
      backupOld <- safeRun "rename existing world to backup" (renameDirectory targetDir backupDir)
      case backupOld of
        Left err -> pure (Left err)
        Right () -> do
          hookResult <- safeRun "post-backup commit hook" afterBackupRename
          case hookResult of
            Left err -> do
              _ <- restoreBackupIfNeeded targetDir backupDir
              pure (Left err)
            Right () -> finalize
    else finalize
  where
    restoreBackupIfNeeded target backup = do
      backupExists <- doesDirectoryExist backup
      currentTargetExists <- doesDirectoryExist target
      if backupExists && not currentTargetExists
        then safeRun "restore backup world" (renameDirectory backup target)
        else pure (Right ())

    finalize = do
      moveNew <- safeRun "rename staging world into place" (renameDirectory stagingDir targetDir)
      case moveNew of
        Left err -> do
          _ <- restoreBackupIfNeeded targetDir backupDir
          pure (Left err)
        Right () -> do
          backupExists <- doesDirectoryExist backupDir
          if backupExists
            then do
              clearBackup <- safeRun "remove old world backup" (removePathForcibly backupDir)
              case clearBackup of
                Left err -> pure (Left err)
                Right () -> pure (Right ())
            else pure (Right ())