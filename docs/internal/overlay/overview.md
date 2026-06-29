# Overlay System Overview

> **Module:** `Topo.Overlay` (354 LOC)
> **Status:** Implemented core model with topo-seer manager/API surfaces

## Overview

The overlay system is the primary extensibility mechanism. Overlays are
schema-driven, per-chunk data layers that sit alongside core terrain
data. They enable plugins and simulation nodes to attach arbitrary
geographic information to the hex grid.

## Storage Modes

| Mode | Implementation | Use Case |
|------|---------------|----------|
| **Sparse** | Boxed array-of-structs per chunk | Rarely-populated data |
| **Dense** | Struct-of-arrays with unboxed `Vector Float` | Continuous fields |

## OverlayStore and Manager Surfaces

`OverlayStore` manages multiple named overlays. Operations:
- Register overlay with schema
- Read/write per-chunk overlay data
- Query overlay existence and metadata

Topo-seer exposes the manager through UI widgets and HTTP/AppService routes:

| Surface | Route / command | Purpose |
|---------|-----------------|---------|
| Overlay manager | `GET /overlays`, `get_overlays` | Lists overlays, active overlay/field, schema field summaries, chunk counts, dependencies, provenance, and diagnostics. |
| Active overlay selector | `PUT /overlays/current`, `set_overlay` | Sets `overlay` and optional `field_index`; invalid names/field indices return diagnostics. |
| Field selector | `GET /overlays/fields`, `POST /overlays/fields/cycle` | Lists/cycles fields for the active or named overlay. |
| Schema viewer | `GET /overlays/schema`, `get_overlay_schema` | Returns the loaded `.toposchema` JSON and field diagnostics. |
| Provenance inspector | `GET /overlays/provenance`, `get_overlay_provenance` | Returns the `.topolay` provenance header. |
| Export/import | `POST /overlays/export`, `POST /overlays/import/validate` | Exports schema/provenance/payload JSON and validates import payloads before adoption. |

## Key Modules

| Module | Purpose |
|--------|---------|
| [Schema](schema.md) | Field definitions and `.toposchema` files |
| [Storage](storage.md) | `.topolay` binary persistence |
| [Indexing](indexing.md) | Secondary field indices for queries |
| [Provenance](provenance.md) | Seed/version/source tracking |

## For Plugin Developers

See the [Plugin Developer Guide](../../plugin-dev/overlay-schemas.md)
for a user-facing walkthrough of creating overlay schemas.
