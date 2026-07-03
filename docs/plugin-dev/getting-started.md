# Getting Started

## Overview

A topo plugin is a standalone Haskell executable that communicates with
topo-seer over a host-created named pipe (Windows) or Unix domain socket
(Linux/macOS) advertised through launch environment variables. The
`topo-plugin-sdk` package handles all transport and protocol details —
you only define your plugin logic.

## Quick Start

### 1. Create a project

Create a standalone Stack project that depends on `topo-plugin-sdk`.

### 2. Define your PluginDef

Use `runPluginWithManifestCommand` so the same executable has a manifest-only
install action and a normal host-launched RPC action:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Topo.Plugin.SDK

myPlugin :: PluginDef
myPlugin = defaultPluginDef
  { pdName = "my-plugin"
  , pdVersion = "0.1.0"
  , pdGenerator = Just GeneratorDef
      { gdInsertAfter = "erosion"
      , gdRequires = ["erosion"]
      , gdRun = \ctx -> do
          pcLog ctx "my-plugin: running"
          pure (Right defaultGeneratorTickResult)
      }
  }

main :: IO ()
main = runPluginWithManifestCommand myPlugin
```

### 3. Build, install, and package discovery files

`stack install` only copies the executable. Generate `manifest.json` explicitly
before topo-seer discovery:

```bash
PLUGIN_DIR="$HOME/.topo/plugins/my-plugin"
stack install --local-bin-path "$PLUGIN_DIR"
"$PLUGIN_DIR/my-plugin" --topo-write-manifest "$PLUGIN_DIR"
```

Copy any schema files referenced by the manifest, such as `.toposchema` overlay
schemas, into the same directory. Non-Haskell plugins can hand-write and package
manifest v3 JSON instead of using the SDK command.

### 4. Run topo-seer

topo-seer discovers only plugin directories that already contain
`manifest.json`. It will not execute unmanifested directories to bootstrap a
manifest.

## Next Steps

- [Key Concepts](concepts.md) — understand the pipeline and overlay model
- [SDK Reference](sdk-reference.md) — full type reference
- [Overlay Schemas](overlay-schemas.md) — define custom data layers
