# Plugin Manifest

> **Module:** `Topo.Plugin.RPC.Manifest` (366 LOC)
> **Status:** Stub

## Overview

Parsing and validation for `manifest.json` files that declare plugin
identity, capabilities, pipeline participation, and parameters.

## Manifest Fields

| Field | Type | Description |
|-------|------|-------------|
| `name` | Text | Unique plugin identifier |
| `version` | Text | Plugin version |
| `description` | Text | Human-readable description |
| `generator` | Object? | Pipeline generator participation |
| `simulation` | Object? | Simulation DAG participation |
| `capabilities` | [Text] | Declared capabilities |
| `config.parameters` | [ParamDef] | User-facing config parameters |
| `overlaySchema` | FilePath? | Path to `.toposchema` file |

## Auto-Generation

The SDK auto-generates `manifest.json` from a `PluginDef` on the
plugin's first run. See [Plugin Dev: Getting Started](../../plugin-dev/getting-started.md).
