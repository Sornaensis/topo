# Plugin Developer Guide

Documentation for building plugins that extend topo's world generation
pipeline and simulation system.

## Contents

### Getting Started

| Document | Description |
|----------|-------------|
| [Getting Started](getting-started.md) | Quick start: create, build, install a plugin |
| [Key Concepts](concepts.md) | Pipeline, overlays, simulation — what you need to know |

### Reference

| Document | Description |
|----------|-------------|
| [SDK Types Reference](sdk-reference.md) | `PluginDef`, `ParamDef`, `GeneratorDef`, `SimulationDef`, `PluginContext` |
| [Overlay Schemas](overlay-schemas.md) | Writing `.toposchema` files for your plugin data |
| [Manifest Format](manifest.md) | `manifest.json` structure and auto-generation |
| [RPC Protocol](rpc-protocol.md) | Wire protocol for advanced/non-Haskell plugins |

### Examples

| Document | Description |
|----------|-------------|
| [Examples](examples.md) | Walkthroughs of `topo-plugin-example` and `topo-plugin-civ-example` |

## Prerequisites

- GHC and Stack (see root README for versions)
- `topo-plugin-sdk` as a dependency (via Git extra-dep)
- A packaging step that installs the executable, writes `manifest.json` before
  discovery, and copies any referenced schemas or sidecar assets
- No direct dependency on `topo` internals is required

Topo-seer discovery is manifest-first: it reads existing `manifest.json` files
under `~/.topo/plugins/` and will not execute unmanifested plugin directories to
bootstrap manifests. SDK users can expose this install action with
`runPluginWithManifestCommand`; non-Haskell plugins can hand-write manifest v3.
