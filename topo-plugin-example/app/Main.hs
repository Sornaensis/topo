{-# LANGUAGE OverloadedStrings #-}

-- | Example topo plugin: terrain roughening.
--
-- Demonstrates the minimum viable plugin using the topo Plugin SDK.
-- This plugin registers as a generator stage that runs after erosion,
-- logging a greeting message.
--
-- To use: build and install the executable, then place a symlink or
-- copy in @~\/.topo\/plugins\/terrain-roughen\/@ alongside the
-- generated @manifest.json@.
module Main (main) where

import Data.Aeson (Value(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack)
import Topo.Plugin.SDK

-- | Plugin definition.
--
-- Registers a generator stage inserted after the erosion stage.
-- The generator logs a message and completes successfully.
terrainRoughenPlugin :: PluginDef
terrainRoughenPlugin = defaultPluginDef
  { pdName    = "terrain-roughen"
  , pdVersion = "0.1.0"
  , pdParams  =
      [ ParamDef
          { paramName    = "roughness"
          , paramLabel   = "Roughness Amount"
          , paramType    = PFloat
          , paramDefault = Number 0.3
          , paramMin     = Just (Number 0.0)
          , paramMax     = Just (Number 1.0)
          , paramTooltip = "How much roughness to add to terrain"
          }
      , ParamDef
          { paramName    = "iterations"
          , paramLabel   = "Roughen Passes"
          , paramType    = PInt
          , paramDefault = Number 2
          , paramMin     = Just (Number 1)
          , paramMax     = Just (Number 10)
          , paramTooltip = "Number of roughening passes"
          }
      ]
  , pdGenerator = Just GeneratorDef
      { gdInsertAfter = "erosion"
      , gdRequires    = ["erosion"]
      , gdRun         = \ctx -> do
          pcLog ctx "terrain-roughen: generator invoked"
          pcLog ctx ("terrain-roughen: roughness = " <> showParam (pcParams ctx) "roughness")
                    case decodeTerrainPayload (pcTerrain ctx) of
                        Left decodeErr ->
                            pure (Left ("terrain-roughen: failed to decode terrain payload: " <> decodeErr))
                        Right terrainWorld -> do
                            pcLog ctx "terrain-roughen: terrain payload decoded"
                            -- Actual terrain modification would go here:
                            -- 1. Modify terrainWorld based on pcParams
                            -- 2. Return modified terrain via typed helper
                            case generatorResultFromTerrain terrainWorld of
                                Left encodeErr ->
                                    pure (Left ("terrain-roughen: failed to encode terrain payload: " <> encodeErr))
                                Right result -> do
                                    pcLog ctx "terrain-roughen: generator complete"
                                    pure (Right result)
      }
  }

-- | Look up a parameter value and show it, with a fallback.
showParam :: Map Text Value -> Text -> Text
showParam params key = case Map.lookup key params of
  Just (Number n) -> pack (show n)
  Just (String s) -> s
  Just (Bool b)   -> pack (show b)
  Just _          -> "<complex>"
  Nothing         -> "<not set>"

main :: IO ()
main = runPlugin terrainRoughenPlugin
