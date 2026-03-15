{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}

-- | Config snapshot persistence and conversion for topo-seer.
--
-- A 'ConfigSnapshot' stores real domain-value generation parameters
-- (via 'WorldGenConfig') rather than normalised slider positions.
-- This module provides bidirectional conversion between UI slider
-- state and 'ConfigSnapshot', plus file I/O.
module Seer.Config.Snapshot
  ( -- * Re-exports from Types
    ConfigSnapshot(..)
  , currentSnapshotVersion
  , defaultSnapshot
    -- * Conversion
  , snapshotFromUi
  , applySnapshotToUi
    -- * File I/O
  , snapshotDir
  , saveSnapshot
  , loadSnapshot
  , listSnapshots
  ) where

import Control.Exception (IOException, try)
import Data.Aeson (eitherDecodeStrict', encode)
import Data.List (sort)
import Data.Text (Text)
import System.Directory
  ( createDirectoryIfMissing
  , getHomeDirectory
  , listDirectory
  , renameFile
  , removeFile
  , doesFileExist
  )
import System.FilePath ((</>), takeExtension, dropExtension)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text

import Actor.UI
  ( Ui
  , UiState(..)
  , setUiSeed
  , setUiChunkSize
  , setUiRenderWaterLevel
  )

import Hyperspace.Actor (ActorHandle, Protocol)
import Seer.Config (configFromUi)
import Seer.Config.SliderRegistry (SliderDef(..), SliderId(..), allSliderDefs)
import Seer.Config.SliderSnapshot (snapshotSliderValueForId)
import Seer.Config.SliderState (setSliderValue)
import Seer.Config.Snapshot.Types

-- ---------------------------------------------------------------------------
-- Conversion: UI → Snapshot
-- ---------------------------------------------------------------------------

-- | Build a 'ConfigSnapshot' from the current UI slider state.
--
-- The generation config is produced by 'Seer.Config.configFromUi' which
-- maps every normalised @[0, 1]@ slider value to its real domain value.
snapshotFromUi :: UiState -> Text -> ConfigSnapshot
snapshotFromUi ui name = ConfigSnapshot
  { csName             = name
  , csVersion          = currentSnapshotVersion
  , csSeed             = uiSeed ui
  , csChunkSize        = uiChunkSize ui
  , csRenderWaterLevel = uiRenderWaterLevel ui
  , csGenConfig        = configFromUi ui
  }

-- ---------------------------------------------------------------------------
-- Conversion: Snapshot → UI  (inverse mapping)
-- ---------------------------------------------------------------------------

-- | Restore all slider values from a 'ConfigSnapshot' by sending
-- shared slider-setter messages to the UI actor.
--
-- Domain values in 'WorldGenConfig' are reverse-mapped to normalised
-- @[0, 1]@ slider positions using 'unmapRange' / 'unmapIntRange'.
-- Metadata fields ('csName', 'csVersion') are not applied.
applySnapshotToUi :: ConfigSnapshot -> ActorHandle Ui (Protocol Ui) -> IO ()
applySnapshotToUi cs h = do
  -- Seed / chunk size / render water level
  setUiSeed h (csSeed cs)
  setUiChunkSize h (csChunkSize cs)
  setUiRenderWaterLevel h (csRenderWaterLevel cs)
  mapM_ (applySnapshotSliderValue cs h . sliderId) allSliderDefs

applySnapshotSliderValue :: ConfigSnapshot -> ActorHandle Ui (Protocol Ui) -> SliderId -> IO ()
applySnapshotSliderValue cs h sliderIdValue =
  setSliderValue h sliderIdValue (snapshotSliderValueForId (csGenConfig cs) sliderIdValue)

-- ---------------------------------------------------------------------------
-- File I/O
-- ---------------------------------------------------------------------------

-- | Return the config snapshot directory (@~\/.topo\/configs\/@), creating
-- it if it does not exist.
snapshotDir :: IO FilePath
snapshotDir = do
  home <- getHomeDirectory
  let dir = home </> ".topo" </> "configs"
  createDirectoryIfMissing True dir
  pure dir

-- | Write a 'ConfigSnapshot' atomically (write to temp, then rename).
-- Returns @Left@ on IO error.
saveSnapshot :: FilePath -> ConfigSnapshot -> IO (Either Text ())
saveSnapshot path cs = do
  result <- try @IOException $ do
    let tmpPath = path <> ".tmp"
    BSL.writeFile tmpPath (encode cs)
    targetExists <- doesFileExist path
    if targetExists
      then removeFile path >> renameFile tmpPath path
      else renameFile tmpPath path
  pure $ case result of
    Left err -> Left (Text.pack (show err))
    Right () -> Right ()

-- | Load a 'ConfigSnapshot' from a JSON file.
loadSnapshot :: FilePath -> IO (Either Text ConfigSnapshot)
loadSnapshot path = do
  result <- try @IOException (BS.readFile path)
  pure $ case result of
    Left err -> Left (Text.pack (show err))
    Right bs ->
      case eitherDecodeStrict' bs of
        Right cs -> Right cs
        Left err -> Left (Text.pack err)

-- | List all config names in the snapshot directory, sorted
-- alphabetically.  Names are returned without the @.json@ extension.
listSnapshots :: IO [Text]
listSnapshots = do
  dir <- snapshotDir
  entries <- listDirectory dir
  let names = sort
        [ Text.pack (dropExtension f)
        | f <- entries
        , takeExtension f == ".json"
        ]
  pure names
