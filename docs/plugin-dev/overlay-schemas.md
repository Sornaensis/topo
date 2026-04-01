# Overlay Schemas for Plugins

> **Status:** Stub

## Overview

If your plugin stores per-tile data, you define an overlay schema as a
`.toposchema` file. The schema declares field names, types, storage
mode, and dependencies.

## Example Schema

```json
{
  "name": "civilization",
  "version": "1.0.0",
  "description": "Civilization overlay for population and development",
  "storage": "sparse",
  "fields": [
    { "name": "population", "type": "int", "default": 0, "nullable": false },
    { "name": "development", "type": "float", "default": 0.0, "nullable": false },
    { "name": "faction", "type": "enum", "default": 0, "nullable": false,
      "enum": { "values": ["none", "red", "blue", "green"] } }
  ],
  "dependencies": {
    "terrain": true,
    "overlays": []
  }
}
```

## Storage Modes

| Mode | When to use |
|------|-------------|
| `"sparse"` | Data is present only on some tiles (cities, resources) |
| `"dense"` | Data on every tile (temperature, moisture) |

## Field Types

<!-- TODO: Full field type reference -->

## Linking to PluginDef

Set `pdSchemaFile` in your `PluginDef` to the path of your `.toposchema`
file relative to the plugin directory.

## See Also

- [.toposchema format spec](../specs/toposchema-format.md)
