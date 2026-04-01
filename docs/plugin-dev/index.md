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
- No direct dependency on `topo` internals is required
