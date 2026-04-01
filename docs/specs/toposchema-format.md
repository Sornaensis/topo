# .toposchema JSON Format

> **Status:** Stub
> **Module:** `Topo.Overlay.Schema`

## Overview

Overlay schema files are UTF-8 JSON describing the typed field
structure of an overlay. They are stored alongside `.topolay` files
in the overlay sidecar directory.

## Schema Structure

```json
{
  "name": "<text>",
  "version": "<text>",
  "description": "<text>",
  "storage": "dense" | "sparse",
  "fields": [ <field-descriptor>, ... ],
  "dependencies": {
    "terrain": <bool>,
    "overlays": [ "<overlay-name>", ... ]
  }
}
```

## Field Descriptor

```json
{
  "name": "<text>",
  "type": "int" | "float" | "bool" | "text" | "enum",
  "default": <json-value>,
  "nullable": <bool>
}
```

For enum-typed fields, additional metadata:

```json
{
  "name": "faction",
  "type": "enum",
  "default": 0,
  "nullable": false,
  "enum": {
    "values": ["none", "red", "blue", "green"]
  }
}
```

## Validation

On load, the schema is validated against the binary payload header.
Field names and types must match exactly.

## Migration

`migrateOverlayData` in `Topo.Overlay.Storage` handles migration
between schema versions when field sets change.
