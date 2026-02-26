# Plugin Developer Guide

## Overview

Topo plugins extend the world generation pipeline and simulation DAG with
custom logic. A plugin is a standalone Haskell executable that communicates
with topo-seer over length-prefixed JSON messages on named pipes (Windows) or
Unix domain sockets.

The **topo-plugin-sdk** package provides the SDK types and runner so you only
need to define your plugin logic — the SDK handles manifest generation,
transport connection, and message dispatch.

## Quick Start

### 1. Create a standalone project

Plugins live in their own repository, **outside** the topo source tree.
They depend on `topo-plugin-sdk` as a Git extra-dep.

```
mkdir my-plugin && cd my-plugin
git init
```

Create `package.yaml`:

```yaml
name: my-plugin
version: 0.1.0.0
license: MPL-2.0
license-file: LICENSE

executables:
  my-plugin:
    main: Main.hs
    source-dirs: app
    dependencies:
      - base >=4.18 && <5
      - aeson
      - containers
      - text
      - topo-plugin-sdk
```

Create `stack.yaml` pointing at the SDK:

```yaml
resolver: lts-24.2

packages:
  - .

extra-deps:
  - git: https://github.com/Sornaensis/topo
    commit: <latest-commit-hash>
    subdirs:
      - topo-plugin-sdk
```

> **Tip:** Pin `commit:` to a released tag or known-good commit so your
> plugin builds reproducibly.

### 2. Define your plugin

Create `app/Main.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Aeson (Value(..))
import Topo.Plugin.SDK

myPlugin :: PluginDef
myPlugin = defaultPluginDef
  { pdName    = "my-plugin"
  , pdVersion = "0.1.0"
  , pdParams  =
      [ ParamDef
          { paramName    = "strength"
          , paramLabel   = "Effect Strength"
          , paramType    = PFloat
          , paramDefault = Number 0.5
          , paramMin     = Just (Number 0.0)
          , paramMax     = Just (Number 1.0)
          , paramTooltip = "How strong the effect is"
          }
      ]
  , pdGenerator = Just GeneratorDef
      { gdInsertAfter = "erosion"
      , gdRequires    = ["erosion"]
      , gdRun         = \ctx -> do
          pcLog ctx "my-plugin: running generator"
          -- Your terrain modification logic here
          pure (Right ())
      }
  }

main :: IO ()
main = runPlugin myPlugin
```

### 3. Build and install

Build and install in one step using `stack install`. The `--local-bin-path`
flag tells Stack where to copy the built executable, and it creates the
target directory automatically:

```
stack install --local-bin-path ~/.topo/plugins/my-plugin/
```

On Windows (PowerShell):

```powershell
stack install --local-bin-path "$env:USERPROFILE\.topo\plugins\my-plugin\"
```

That's it — the SDK auto-generates `manifest.json` on the plugin's first
run, so no manual manifest setup is required.

## Architecture

### Plugin Lifecycle

1. **Discovery** — topo-seer scans `~/.topo/plugins/` for directories
   containing a `manifest.json`.

2. **Manifest** — the SDK generates `manifest.json` from your `PluginDef` on
   first run. The manifest declares the plugin's identity, capabilities,
   pipeline position, and parameters.

3. **Connection** — topo-seer launches your plugin executable and connects
   via named pipes.

4. **Invocation** — the host sends `invoke_generator` or `invoke_simulation`
   messages. Your callbacks receive a `PluginContext` with the current world
   state, parameters, and seed.

5. **Shutdown** — the host sends a `shutdown` message, and the SDK exits
   cleanly.

### Message Protocol

Messages are length-prefixed JSON envelopes:

```
┌──────────────┬──────────────────┐
│ length (4 B) │ payload (N bytes)│
└──────────────┴──────────────────┘
```

The envelope format:

```json
{ "type": "invoke_generator", "payload": { ... } }
```

Message types:
- **Host → Plugin**: `invoke_generator`, `invoke_simulation`, `shutdown`
- **Plugin → Host**: `progress`, `log`, `generator_result`, `simulation_result`, `error`

## SDK Types

### PluginDef

The top-level plugin definition. Use `defaultPluginDef` as a starting point.

| Field          | Type                   | Description                          |
|----------------|------------------------|--------------------------------------|
| `pdName`       | `Text`                 | Unique plugin identifier             |
| `pdVersion`    | `Text`                 | Version string (informational)       |
| `pdParams`     | `[ParamDef]`           | User-facing configuration parameters |
| `pdSchemaFile` | `Maybe FilePath`       | Overlay schema file path             |
| `pdGenerator`  | `Maybe GeneratorDef`   | Generator pipeline participation     |
| `pdSimulation` | `Maybe SimulationDef`  | Simulation DAG participation         |

### ParamDef

A user-facing parameter shown as a slider or checkbox in topo-seer.

| Field          | Type           | Description                    |
|----------------|----------------|--------------------------------|
| `paramName`    | `Text`         | Internal key                   |
| `paramLabel`   | `Text`         | UI display label               |
| `paramType`    | `ParamType`    | `PFloat`, `PInt`, or `PBool`   |
| `paramDefault` | `Value`        | Default JSON value             |
| `paramMin`     | `Maybe Value`  | Optional minimum               |
| `paramMax`     | `Maybe Value`  | Optional maximum               |
| `paramTooltip` | `Text`         | Tooltip on hover               |

### GeneratorDef

| Field           | Type                                    | Description                        |
|-----------------|-----------------------------------------|------------------------------------|
| `gdInsertAfter` | `Text`                                  | Stage to insert after              |
| `gdRequires`    | `[Text]`                                | Required preceding stages          |
| `gdRun`         | `PluginContext -> IO (Either Text ())`   | Generator implementation           |

### SimulationDef

| Field            | Type                                    | Description                        |
|------------------|-----------------------------------------|------------------------------------|
| `sdDependencies` | `[Text]`                                | Overlay dependencies               |
| `sdTick`         | `PluginContext -> IO (Either Text ())`   | Simulation tick implementation     |

### PluginContext

Runtime context provided to callbacks.

| Field      | Type                | Description                       |
|------------|---------------------|-----------------------------------|
| `pcWorld`  | `TerrainWorld`      | Current terrain state             |
| `pcParams` | `Map Text Value`    | Current parameter values          |
| `pcSeed`   | `Word64`            | World generation seed             |
| `pcLog`    | `Text -> IO ()`     | Log to topo-seer                  |

## Manifest Format

The SDK auto-generates `manifest.json`. Manual editing is possible:

```json
{
  "name": "my-plugin",
  "version": "0.1.0",
  "description": "My terrain plugin",
  "generator": {
    "insertAfter": "erosion",
    "requires": ["erosion"]
  },
  "capabilities": ["readTerrain", "log"],
  "config": {
    "parameters": [
      {
        "name": "strength",
        "label": "Effect Strength",
        "type": "float",
        "default": 0.5,
        "range": [0.0, 1.0],
        "tooltip": "How strong the effect is"
      }
    ]
  }
}
```

### Capabilities

| Capability       | Description                           |
|------------------|---------------------------------------|
| `readTerrain`    | Read terrain chunk data               |
| `readOverlay`    | Read declared overlay dependencies    |
| `writeOverlay`   | Write to the owned overlay            |
| `writeTerrain`   | Mutate terrain (sim writer node only) |
| `log`            | Send log messages to host             |

Capabilities are inferred from your `PluginDef` — generators get
`readTerrain` + `log`, simulation nodes additionally get overlay access.

## Plugin Configuration Persistence

Parameter values are saved per-plugin in
`~/.topo/plugins/<name>/config.json`. These values persist across
topo-seer sessions. They are **not** included in preset files — plugin
config is independent of generation presets.

## Example: topo-plugin-example

See
[topo-plugin-example/app/Main.hs](../topo-plugin-example/app/Main.hs)
for a complete minimal plugin that registers a generator stage with
configurable parameters.
