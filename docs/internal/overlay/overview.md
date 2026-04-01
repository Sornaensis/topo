# Overlay System Overview

> **Module:** `Topo.Overlay` (354 LOC)
> **Status:** Stub

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

## OverlayStore

`OverlayStore` manages multiple named overlays. Operations:
- Register overlay with schema
- Read/write per-chunk overlay data
- Query overlay existence and metadata

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
