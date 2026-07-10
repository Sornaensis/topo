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
          reportPluginProgress ctx "my-plugin: applying terrain effect" 0.5
          -- Your terrain modification logic here
          pure (Right defaultGeneratorTickResult)
      }
  }

main :: IO ()
main = runPluginWithManifestCommand myPlugin
```

### 3. Build, install, and package the manifest

`stack install` copies only the executable. topo-seer discovery requires
`manifest.json` to exist in the plugin directory before discovery, so SDK
plugins should run the explicit manifest-only command during packaging:

```bash
PLUGIN_DIR="$HOME/.topo/plugins/my-plugin"
stack install --local-bin-path "$PLUGIN_DIR"
"$PLUGIN_DIR/my-plugin" --topo-write-manifest "$PLUGIN_DIR"
```

On Windows (PowerShell):

```powershell
$PluginDir = "$env:USERPROFILE\.topo\plugins\my-plugin"
stack install --local-bin-path "$PluginDir"
& "$PluginDir\my-plugin.exe" --topo-write-manifest "$PluginDir"
```

If your executable component name differs from `pdName`, copy or rename the
installed executable so the plugin directory contains a launch executable named
for the manifest `name`. If your manifest references schemas or other sidecar
assets, copy those files into the same plugin directory during this install
step. Non-Haskell plugins can hand-write/package manifest v3 JSON directly.

Topo-seer does **not** execute unmanifested plugin directories to bootstrap
manifests; automatic host scanning only reads existing `manifest.json` files.
A subdirectory missing `manifest.json` is surfaced as a degraded
`manifest_missing` diagnostic using the directory name so users know to
generate or package the manifest.

## Architecture

### Plugin Lifecycle

1. **Packaging** — install the executable, then generate/copy `manifest.json`
   (and any referenced `.toposchema` files) into the plugin directory.

2. **Discovery** — topo-seer scans `~/.topo/plugins/` for directories
   containing an existing `manifest.json`. It will not launch unknown
   executables just to create one.

3. **Manifest** — SDK plugins generate `manifest.json` from `PluginDef` via an
   explicit manifest-only install command. The manifest declares the plugin's
   identity, capabilities, pipeline position, and parameters.

4. **Connection** — topo-seer launches your plugin executable with
   `TOPO_PLUGIN_ENDPOINT` and `TOPO_PLUGIN_ENDPOINT_KIND` set, then the SDK
   connects to that host-created named pipe (Windows) or Unix domain socket
   (Linux/macOS).

5. **Invocation** — the host sends `invoke_generator` or `invoke_simulation`
   messages. Your callbacks receive a `PluginContext` with the current world
   state, parameters, and seed.

6. **Shutdown** — the host sends a `shutdown` message, and the SDK exits
   cleanly.

### Message Protocol

Messages are length-prefixed JSON envelopes on the host-created transport:

```
┌──────────────┬──────────────────┐
│ length (4 B) │ payload (N bytes)│
└──────────────┴──────────────────┘
```

The envelope format includes an optional correlation ID:

```json
{ "id": 42, "type": "invoke_generator", "payload": { "payload_version": 1 } }
```

Protocol v4 message groups (v3 messages plus launch auth/session proof):

- **Lifecycle/liveness:** `handshake`, `handshake_ack`, `world_changed`,
  `heartbeat`, `health_check`, `health_status`, `shutdown`.
- **Generation/simulation:** `invoke_generator`, `generator_result`,
  `invoke_simulation`, `simulation_result`, with interim `progress` and `log`
  messages and final `error` failures.
- **Data resources:** `query_resource`, `query_result`, `mutate_resource`, and
  `mutate_result` for plugin-owned schemas declared in the manifest/handshake.
- **Backend-neutral external data sources:** `external_data_source_grant`,
  `external_data_source_revoke`, `external_data_source_operation_result`,
  `external_data_source_status_request`, and `external_data_source_status` for
  provider-owned sources, grants, ACK/result payloads, status, opaque
  references, and opaque config refs.

See [Plugin Dev: RPC Protocol](plugin-dev/rpc-protocol.md) for the full
contract-tested wire reference.

## SDK Types

### PluginDef

The top-level plugin definition. Use `defaultPluginDef` as a starting point.

| Field | Type | Description |
|-------|------|-------------|
| `pdName` | `Text` | Unique plugin identifier |
| `pdVersion` | `Text` | Version string (informational) |
| `pdDescription` | `Maybe Text` | Manifest v3 description (`Nothing` uses a default) |
| `pdRuntimeTopoMin` / `pdRuntimeTopoMax` | `Maybe Text` | Optional Topo host version bounds |
| `pdParams` | `[ParamDef]` | User-facing configuration parameters |
| `pdSchemaFile` | `Maybe FilePath` | Overlay schema file path |
| `pdGenerator` | `Maybe GeneratorDef` | Generator pipeline participation |
| `pdSimulation` | `Maybe SimulationDef` | Plugin simulation declaration; dependencies are simulation node IDs and may reference host built-ins like `weather` |
| `pdCapabilities` | `[RPCCapability]` | Explicit extra capabilities, such as `CapWriteTerrain` |
| `pdDataDirectory` | `Maybe FilePath` | Plugin data directory under the world save |
| `pdDataResources` | `[DataResourceDef]` | Data-service resource schemas and handlers |
| `pdUiHints` | `RPCUIHints` | Manifest v3 UI presentation hints |
| `pdExternalDataSources` | `[RPCExternalDataSourceDecl]` | Provider-owned external data sources, grants, status, and opaque connection metadata |
| `pdExternalDataSourceRefs` | `[RPCExternalDataSourceRef]` | Consumed external data sources, requested grants, status, and opaque reference metadata |
| `pdStartPolicy` | `RPCStartPolicy` | Host-side process supervision policy |

External data-source declarations keep migrations, backing schemas, connection
details, and consistency rules provider-owned (or external-system-owned). Topo
may surface status/errors, backend-neutral provider/availability/health/access
policy metadata, and opaque diagnostics, but it must not prescribe
backend-specific migration tables, locks, writer policies, or schema rules.
Access grants are brokered only when their generic capabilities cover the
requested access (`read` -> `query`, `write` -> `mutate`, `admin` -> `migrate`).

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
| `gdRun`         | `PluginContext -> IO (Either Text GeneratorTickResult)` | Generator implementation |

### SimulationDef

| Field            | Type                                    | Description                        |
|------------------|-----------------------------------------|------------------------------------|
| `sdDependencies` | `[Text]`                                | Simulation node ID dependencies; may include host built-ins such as `weather` |
| `sdSchedule`     | `Maybe SimulationScheduleDecl`          | Optional cadence; `Nothing` emits the hourly default |
| `sdTick`         | `PluginContext -> IO (Either Text SimulationTickResult)` | Simulation tick implementation |

### PluginContext

Runtime context provided to callbacks.

| Field | Type | Description |
|--------|------|-------------|
| `pcWorld` | `TerrainWorld` | Current terrain state |
| `pcParams` | `Map Text Value` | Current parameter values |
| `pcTerrain` | `Value` | Raw terrain payload from the host invocation |
| `pcOwnOverlay` | `Maybe Value` | Plugin-owned overlay payload for simulation ticks |
| `pcOverlays` | `Map Text Value` | Dependency overlay payloads keyed by overlay name |
| `pcSeed` | `Word64` | World generation seed |
| `pcLog` | `Text -> IO ()` | Log to topo-seer |
| `pcProgress` | `Text -> Double -> IO ()` | Emit interim progress for the current generator, simulation, query, or mutation callback |
| `pcWorldPath` | `Maybe FilePath` | Current world save directory path, when known |

Use `reportPluginProgress ctx message fraction` (or
`pcProgress ctx message fraction`) for progress updates. Fractions are absolute
progress for the current invocation, conventionally `0.0` through `1.0`
inclusive; callbacks are not required to emit either endpoint, and the final
result or error remains authoritative. The SDK clamps finite values into
`[0,1]` and maps non-finite values defensively before JSON encoding.
`PluginContext(..)` is exported, so adding `pcProgress` is source-breaking for
plugins that manually construct, positionally pattern-match, or exhaustively
pattern-match the record; update those sites to include or ignore the new field.

## Manifest Format

The SDK generates manifest v3 `manifest.json` from `PluginDef` when you run the
manifest-only install command (or call `writePluginManifestToDirectory`). Manual
editing is possible for non-Haskell plugins:

```json
{
  "manifestVersion": 3,
  "name": "my-plugin",
  "version": "0.1.0",
  "runtime": { "protocol": { "min": 4, "max": 4 } },
  "description": "My terrain plugin",
  "ui": { "displayName": "My Plugin", "category": "Generation" },
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

Full field reference, JSON Schema, and provider/consumer examples are in
[Plugin Dev: Manifest Format](plugin-dev/manifest.md).

### Capabilities

| Capability       | Description                           |
|------------------|---------------------------------------|
| `readTerrain`    | Read terrain chunk data               |
| `readOverlay`    | Read declared overlay dependencies    |
| `writeOverlay`   | Write to the owned overlay            |
| `writeTerrain`   | Mutate terrain (sim writer node only) |
| `dataRead`       | Read or expose plugin data resources  |
| `dataWrite`      | Mutate plugin data resources          |
| `log`            | Send log messages to host             |

Capabilities are inferred from your `PluginDef` — generators get
`readTerrain` + `log`, simulation nodes additionally get overlay access, and
data-resource plugins get data capabilities matching their operations. Terrain
writes are not inferred from every simulation; add `CapWriteTerrain` to
`pdCapabilities` only for simulation plugins that return terrain writes.

Manifest v3 also supports backend-neutral `externalDataSources` and
`externalDataSourceRefs` for provider-owned data shared between plugins. These
fields name providers, sources, capabilities, access grants, resources, status,
health/policy metadata, version/compatibility markers, and opaque diagnostics;
they do not make the host own the external data. Plugin/provider failures make
brokered grants unavailable for routing until the provider reports readiness;
world save/load preserves opaque references and metadata without reconnecting or
cleaning up provider-owned stores.

## Plugin Configuration Persistence

Parameter values are saved per-plugin in
`~/.topo/plugins/<name>/config.json`. These values persist across
topo-seer sessions. They are **not** included in preset files — plugin
config is independent of generation presets.

## First-party example packaging

`topo-plugin-example` packages the executable plus generated manifest. The
manifest name is `terrain-roughen`, so copy the Stack-installed executable to
that launch name before discovery:

```bash
PLUGIN_DIR="$HOME/.topo/plugins/terrain-roughen"
stack install topo-plugin-example:exe:topo-plugin-example --local-bin-path "$PLUGIN_DIR"
cp "$PLUGIN_DIR/topo-plugin-example" "$PLUGIN_DIR/terrain-roughen"
"$PLUGIN_DIR/terrain-roughen" --topo-write-manifest "$PLUGIN_DIR"
```

`topo-plugin-civ-example` also packages the schema referenced by the manifest;
its launch executable must be named `civilization`:

```bash
PLUGIN_DIR="$HOME/.topo/plugins/civilization"
stack install topo-plugin-civ-example:exe:topo-plugin-civ-example --local-bin-path "$PLUGIN_DIR"
cp "$PLUGIN_DIR/topo-plugin-civ-example" "$PLUGIN_DIR/civilization"
cp topo-plugin-civ-example/civilization.toposchema "$PLUGIN_DIR/"
"$PLUGIN_DIR/civilization" --topo-write-manifest "$PLUGIN_DIR"
```

See [topo-plugin-example/app/Main.hs](../topo-plugin-example/app/Main.hs) for a
complete minimal plugin that registers a generator stage with configurable
parameters.
