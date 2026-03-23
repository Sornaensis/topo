{-# LANGUAGE OverloadedStrings #-}

-- | MCP resource URI routing.
--
-- Maps @topo:\/\/@ resource URIs to IPC commands sent to topo-seer.
-- Each resource read translates into a 'SeerCommand' sent over IPC,
-- and the 'SeerResponse' result is formatted as MCP 'ResourceContent'.
module Topo.MCP.Resources
  ( allResourceDefs
  , allResourceTemplateDefs
  , handleResourceRead
  , parseResourceUri  -- exported for testing
  ) where

import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Lazy as BL

import Topo.Command.Types (SeerResponse(..))
import Topo.MCP.Types
import Topo.MCP.IPC (IpcConnectionRef, sendCommand, withConnection)

-- | All static resource definitions advertised by this server.
allResourceDefs :: [ResourceDef]
allResourceDefs =
  [ ResourceDef
      { rdUri         = "topo://state"
      , rdName        = "Application State"
      , rdDescription = "High-level topo-seer state: seed, view mode, config tab, generation status"
      , rdMimeType    = "application/json"
      }
  , ResourceDef
      { rdUri         = "topo://sliders"
      , rdName        = "All Sliders"
      , rdDescription = "All slider values grouped by config tab, with domain ranges"
      , rdMimeType    = "application/json"
      }
  , ResourceDef
      { rdUri         = "topo://view-modes"
      , rdName        = "View Modes"
      , rdDescription = "Available view modes and which is currently active"
      , rdMimeType    = "application/json"
      }
  -- Phase 3 resources
  , ResourceDef
      { rdUri         = "topo://world"
      , rdName        = "World Metadata"
      , rdDescription = "World metadata: seed, chunk size, chunk count, total tiles, overlay names"
      , rdMimeType    = "application/json"
      }
  , ResourceDef
      { rdUri         = "topo://generation-status"
      , rdName        = "Generation Status"
      , rdDescription = "Whether terrain generation is in progress and chunk count"
      , rdMimeType    = "application/json"
      }
  , ResourceDef
      { rdUri         = "topo://chunks"
      , rdName        = "Terrain Chunks"
      , rdDescription = "List of all terrain chunks with elevation min/max"
      , rdMimeType    = "application/json"
      }
  , ResourceDef
      { rdUri         = "topo://terrain-stats"
      , rdName        = "Terrain Statistics"
      , rdDescription = "Global aggregate terrain statistics: elevation, biome/form distribution, climate, vegetation, rivers"
      , rdMimeType    = "application/json"
      }
  , ResourceDef
      { rdUri         = "topo://overlays"
      , rdName        = "Overlays"
      , rdDescription = "Loaded overlay names and count"
      , rdMimeType    = "application/json"
      }
  , ResourceDef
      { rdUri         = "topo://worlds"
      , rdName        = "Saved Worlds"
      , rdDescription = "Saved worlds from ~/.topo/worlds/ with metadata"
      , rdMimeType    = "application/json"
      }
  -- Phase 4 resources
  , ResourceDef
      { rdUri         = "topo://presets"
      , rdName        = "Config Presets"
      , rdDescription = "Saved config presets from ~/.topo/configs/"
      , rdMimeType    = "application/json"
      }
  ]

-- | All resource template definitions advertised by this server.
allResourceTemplateDefs :: [ResourceTemplateDef]
allResourceTemplateDefs =
  [ ResourceTemplateDef
      { rtdUriTemplate = "topo://sliders/{tab}"
      , rtdName        = "Sliders by Tab"
      , rtdDescription = "Slider values for a specific config tab (terrain, planet, climate, weather, biome, erosion)"
      , rtdMimeType    = "application/json"
      }
  , ResourceTemplateDef
      { rtdUriTemplate = "topo://slider/{name}"
      , rtdName        = "Single Slider"
      , rtdDescription = "Detailed info for a single slider by name"
      , rtdMimeType    = "application/json"
      }
  -- Phase 3 templates
  , ResourceTemplateDef
      { rtdUriTemplate = "topo://hex/{chunk}/{tile}"
      , rtdName        = "Hex Data"
      , rtdDescription = "Full terrain data at a hex coordinate (all layers)"
      , rtdMimeType    = "application/json"
      }
  , ResourceTemplateDef
      { rtdUriTemplate = "topo://chunk/{id}"
      , rtdName        = "Chunk Summary"
      , rtdDescription = "Aggregate statistics for a specific terrain chunk"
      , rtdMimeType    = "application/json"
      }
  , ResourceTemplateDef
      { rtdUriTemplate = "topo://enums/{type}"
      , rtdName        = "Domain Enums"
      , rtdDescription = "Enumerate valid values for a domain type (biome, terrain_form, water_body_type, etc.)"
      , rtdMimeType    = "application/json"
      }
  ]

-- | Handle a resources/read request.
--
-- Routes the URI to the appropriate IPC command and returns the result.
handleResourceRead
  :: IpcConnectionRef
  -> Text      -- ^ URI to read
  -> IO (Either Text ResourceReadResult)
handleResourceRead connRef uri = case parseResourceUri uri of
  Nothing -> pure $ Left ("unknown resource URI: " <> uri)
  Just (method, params) -> do
    result <- withConnection connRef $ \conn -> do
      (rsp, conn') <- sendCommand conn method params
      pure (rsp, conn')
    case result of
      Left err -> pure $ Left err
      Right rsp
        | srSuccess rsp ->
            let jsonText = Text.decodeUtf8 (BL.toStrict (Aeson.encode (srResult rsp)))
            in pure $ Right ResourceReadResult
              { rrrContents =
                  [ ResourceContent
                      { rcUri      = uri
                      , rcMimeType = "application/json"
                      , rcText     = jsonText
                      }
                  ]
              }
        | otherwise ->
            case srError rsp of
              Just err -> pure $ Left err
              Nothing  -> pure $ Left "unknown error"

-- | Parse a topo:// URI into an IPC method name and params.
parseResourceUri :: Text -> Maybe (Text, Value)
parseResourceUri uri
  -- Static resources
  | uri == "topo://state"             = Just ("get_state", object [])
  | uri == "topo://sliders"           = Just ("get_sliders", object [])
  | uri == "topo://view-modes"        = Just ("get_view_modes", object [])
  | uri == "topo://world"             = Just ("get_world_meta", object [])
  | uri == "topo://generation-status" = Just ("get_generation_status", object [])
  | uri == "topo://chunks"            = Just ("get_chunks", object [])
  | uri == "topo://terrain-stats"     = Just ("get_terrain_stats", object [])
  | uri == "topo://overlays"          = Just ("get_overlays", object [])
  | uri == "topo://worlds"            = Just ("list_worlds", object [])
  | uri == "topo://presets"           = Just ("list_presets", object [])
  -- Template: topo://sliders/{tab}
  | Just tab <- Text.stripPrefix "topo://sliders/" uri
  , not (Text.null tab)
  = Just ("get_sliders", object ["tab" .= tab])
  -- Template: topo://slider/{name}
  | Just name <- Text.stripPrefix "topo://slider/" uri
  , not (Text.null name)
  = Just ("get_slider", object ["name" .= name])
  -- Template: topo://hex/{chunk}/{tile}
  | Just rest <- Text.stripPrefix "topo://hex/" uri
  , (chunkStr, rest') <- Text.breakOn "/" rest
  , Just tileStr <- Text.stripPrefix "/" rest'
  , not (Text.null chunkStr)
  , not (Text.null tileStr)
  , Just chunkId <- readMaybeInt chunkStr
  , Just tileIdx <- readMaybeInt tileStr
  = Just ("get_hex", object ["chunk" .= chunkId, "tile" .= tileIdx])
  -- Template: topo://chunk/{id}
  | Just idStr <- Text.stripPrefix "topo://chunk/" uri
  , not (Text.null idStr)
  , Just chunkId <- readMaybeInt idStr
  = Just ("get_chunk_summary", object ["chunk" .= chunkId])
  -- Template: topo://enums/{type}
  | Just enumType <- Text.stripPrefix "topo://enums/" uri
  , not (Text.null enumType)
  = Just ("get_enums", object ["type" .= enumType])
  | otherwise = Nothing

-- | Try to read an Int from Text.
readMaybeInt :: Text -> Maybe Int
readMaybeInt t = case reads (Text.unpack t) of
  [(n, "")] -> Just n
  _         -> Nothing
