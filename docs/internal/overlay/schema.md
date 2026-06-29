# Overlay Schema

> **Module:** `Topo.Overlay.Schema` (336 LOC)
> **Status:** Implemented with schema viewer/export validation surfaces

## Overview

`OverlaySchema` defines the typed field structure of an overlay.
Schemas are persisted as `.toposchema` JSON files and used for
validation, migration, and wire-format encoding.

## Schema Structure

- `name` — overlay identifier
- `version` — schema version string
- `description` — human-readable description
- `fields` — array of typed field descriptors
- `storage` — `"dense"` or `"sparse"`
- `dependencies` — terrain and overlay dependencies

## Field Types

| Type | Haskell | JSON |
|------|---------|------|
| Float | `Float` | `"float"` |
| Int | `Int` | `"int"` |
| Bool | `Bool` | `"bool"` |
| Text | `Text` | `"text"` |
| Enum | `Int` (tag) | `"enum"` + metadata |

## UI/API Surface

Topo-seer schema inspection is available through:

- `GET /overlays/schema?overlay=<name>`
- command/AppService method `get_overlay_schema`
- widget action `WidgetOverlaySchema`

The response includes `format: "toposchema"`, the canonical schema JSON, field summaries, and diagnostics. Import validation uses the same parser via `POST /overlays/import/validate` and returns structured diagnostics instead of mutating runtime state.

## See Also

- [Format spec](../../specs/toposchema-format.md)
- [Overlay overview](overview.md)
