# Manifest Format

> **Status:** Stub — to be expanded from existing `plugins.md`

## Overview

Every plugin directory contains a `manifest.json` declaring the plugin's
identity, capabilities, pipeline position, and parameters.

The SDK auto-generates this from your `PluginDef` on first run.

## Example

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

## Fields

<!-- TODO: Full field reference -->

## Auto-Generation vs Manual Editing

The SDK generates `manifest.json` automatically. Manual editing is
possible for advanced use cases but not recommended — keep your
`PluginDef` as the source of truth.
