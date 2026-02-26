{-# LANGUAGE OverloadedStrings #-}

-- | Topo Plugin SDK — the public API for building topo plugins.
--
-- This module re-exports everything a plugin author needs to define
-- and run a topo plugin.  Import this single module in your plugin's
-- @Main.hs@.
--
-- === Quick Start
--
-- @
-- import Topo.Plugin.SDK
--
-- main :: IO ()
-- main = runPlugin myPlugin
--
-- myPlugin :: PluginDef
-- myPlugin = defaultPluginDef
--   { pdName    = "my-terrain-mod"
--   , pdVersion = "0.1.0"
--   , pdGenerator = Just GeneratorDef
--       { gdInsertAfter = "erosion"
--       , gdRequires    = ["erosion"]
--       , gdRun         = \\ctx -> do
--           pcLog ctx "Hello from my plugin!"
--           pure (Right ())
--       }
--   }
-- @
--
-- === Architecture
--
-- A topo plugin is a standalone executable that communicates with
-- topo-seer via length-prefixed JSON messages over named pipes
-- (Windows) or Unix domain sockets.
--
-- The SDK handles:
--
-- * Manifest generation from your 'PluginDef'
-- * Transport connection and message framing
-- * RPC message dispatch (generator invocation, simulation ticks)
-- * Clean shutdown handling
--
-- You only need to implement the business logic in 'GeneratorDef'
-- and\/or 'SimulationDef' callbacks.
module Topo.Plugin.SDK
  ( -- * Plugin definition
    PluginDef(..)
  , defaultPluginDef
    -- * Parameters
  , ParamDef(..)
  , ParamType(..)
    -- * Generator
  , GeneratorDef(..)
    -- * Simulation
  , SimulationDef(..)
    -- * Context
  , PluginContext(..)
    -- * Entry point
  , runPlugin
    -- * Manifest utilities
  , generateManifest
  , writeManifest
  ) where

import Topo.Plugin.SDK.Types
import Topo.Plugin.SDK.Runner (runPlugin, generateManifest, writeManifest)
