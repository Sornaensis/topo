# Overlay Schema

> **Module:** `Topo.Overlay.Schema` (336 LOC)
> **Status:** Stub

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

## See Also

- [Format spec](../../specs/toposchema-format.md)
