{-# LANGUAGE OverloadedStrings #-}

module Topo.Pipeline.OverlayLifecycle
  ( registerProducedOverlay
  ) where

import Data.Text (Text)

import Topo.Overlay (emptyOverlay, insertOverlay, lookupOverlay)
import Topo.Overlay.Schema (OverlaySchema, osName)
import Topo.World (TerrainWorld(..))

registerProducedOverlay
  :: Maybe Text
  -> Maybe OverlaySchema
  -> TerrainWorld
  -> TerrainWorld
registerProducedOverlay maybeProducedName maybeSchema world =
  case maybeProducedName of
    Nothing -> world
    Just producedName ->
      case lookupOverlay producedName (twOverlays world) of
        Just _ -> world
        Nothing ->
          case maybeSchema of
            Nothing -> world
            Just schema
              | osName schema == producedName ->
                  let overlayStore' = insertOverlay (emptyOverlay schema) (twOverlays world)
                      manifest' = dedupeManifest (producedName : twOverlayManifest world)
                  in world
                      { twOverlays = overlayStore'
                      , twOverlayManifest = manifest'
                      }
              | otherwise -> world
  where
    dedupeManifest [] = []
    dedupeManifest (name:rest) = name : filter (/= name) (dedupeManifest rest)