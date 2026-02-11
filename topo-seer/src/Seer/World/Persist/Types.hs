{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Cycle-free types for world persistence.
--
-- This module contains 'WorldSaveManifest' and its JSON instances
-- without depending on 'Actor.UI', allowing both 'Actor.UI' and
-- 'Seer.World.Persist' to import it freely.
module Seer.World.Persist.Types
  ( WorldSaveManifest(..)
  , defaultManifestTime
  , manifestJsonOptions
  ) where

import Data.Aeson
  ( FromJSON(..)
  , ToJSON(..)
  , genericToJSON
  , withObject
  , (.:?)
  , (.!=)
  )
import Data.Aeson.Types (Options(..))
import qualified Data.Aeson.Types as Aeson
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Word (Word64)
import GHC.Generics (Generic)

-- | Metadata recorded alongside each saved world.
data WorldSaveManifest = WorldSaveManifest
  { wsmName       :: Text
  , wsmSeed       :: Word64
  , wsmChunkSize  :: Int
  , wsmCreatedAt  :: UTCTime
  , wsmChunkCount :: Int
  } deriving (Eq, Show, Generic)

-- | JSON field name options: strip @\"wsm\"@ prefix, camelCase to
-- snake_case.
manifestJsonOptions :: Options
manifestJsonOptions = Aeson.defaultOptions
  { fieldLabelModifier = Aeson.camelTo2 '_' . drop 3
  }

instance ToJSON WorldSaveManifest where
  toJSON = genericToJSON manifestJsonOptions

-- | Forward-compatible parser — every field defaults sensibly so that
-- manifests saved by newer versions can still be loaded.
instance FromJSON WorldSaveManifest where
  parseJSON = withObject "WorldSaveManifest" $ \o ->
    WorldSaveManifest
      <$> o .:? "name"        .!= ""
      <*> o .:? "seed"        .!= 0
      <*> o .:? "chunk_size"  .!= 64
      <*> o .:? "created_at"  .!= defaultManifestTime
      <*> o .:? "chunk_count" .!= 0

-- | Epoch time used as a default when the field is missing.
defaultManifestTime :: UTCTime
defaultManifestTime = read "2025-01-01 00:00:00 UTC"
