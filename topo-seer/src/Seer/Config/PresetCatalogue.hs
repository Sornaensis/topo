{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Pure metadata and library values for the built-in preset namespace.
module Seer.Config.PresetCatalogue
  ( PresetSource(..)
  , PresetCatalogueEntry(..)
  , builtinPresetDefinitions
  , builtinPresetEntries
  , userPresetEntry
  , presetCatalogueLabel
  , presetCatalogueMatches
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import Topo.WorldGen
  ( WorldGenConfig
  , archipelagoWorldGenConfig
  , aridWorldGenConfig
  , continentalWorldGenConfig
  , inlandSeaWorldGenConfig
  , largeOceanWorldGenConfig
  , lushWorldGenConfig
  )

data PresetSource = PresetBuiltin | PresetUser
  deriving (Eq, Show)

data PresetCatalogueEntry = PresetCatalogueEntry
  { presetCatalogueId :: !Text
  , presetCatalogueName :: !Text
  , presetCatalogueSource :: !PresetSource
  , presetCatalogueReadOnly :: !Bool
  } deriving (Eq, Show)

-- | The single maintained definition table for built-in IDs, display metadata,
-- and the corresponding @Topo.WorldGen@ values.
builtinPresetDefinitions :: [(PresetCatalogueEntry, WorldGenConfig)]
builtinPresetDefinitions =
  [ (builtinEntry "builtin:continental" "Continental", continentalWorldGenConfig)
  , (builtinEntry "builtin:archipelago" "Archipelago", archipelagoWorldGenConfig)
  , (builtinEntry "builtin:large-ocean" "Large Ocean", largeOceanWorldGenConfig)
  , (builtinEntry "builtin:inland-sea" "Inland Sea", inlandSeaWorldGenConfig)
  , (builtinEntry "builtin:arid" "Arid", aridWorldGenConfig)
  , (builtinEntry "builtin:lush" "Lush", lushWorldGenConfig)
  ]

builtinPresetEntries :: [PresetCatalogueEntry]
builtinPresetEntries = map fst builtinPresetDefinitions

userPresetEntry :: Text -> PresetCatalogueEntry
userPresetEntry name = PresetCatalogueEntry
  { presetCatalogueId = name
  , presetCatalogueName = name
  , presetCatalogueSource = PresetUser
  , presetCatalogueReadOnly = False
  }

presetCatalogueLabel :: Text -> Text
presetCatalogueLabel presetId = case findBuiltinEntry presetId of
  Just entry -> presetCatalogueName entry <> " [built-in]"
  Nothing -> presetId

presetCatalogueMatches :: Text -> Text -> Bool
presetCatalogueMatches query presetId =
  let needle = Text.toLower query
  in needle `Text.isInfixOf` Text.toLower presetId
      || needle `Text.isInfixOf` Text.toLower (presetCatalogueLabel presetId)

builtinEntry :: Text -> Text -> PresetCatalogueEntry
builtinEntry presetId name = PresetCatalogueEntry
  { presetCatalogueId = presetId
  , presetCatalogueName = name
  , presetCatalogueSource = PresetBuiltin
  , presetCatalogueReadOnly = True
  }

findBuiltinEntry :: Text -> Maybe PresetCatalogueEntry
findBuiltinEntry presetId = case filter ((== presetId) . presetCatalogueId) builtinPresetEntries of
  entry:_ -> Just entry
  [] -> Nothing
