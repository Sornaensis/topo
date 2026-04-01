# Overlay Storage

> **Modules:** `Topo.Overlay.Storage` (797 LOC), `Topo.Overlay.Storage.ChunkIndex` (165 LOC)
> **Status:** Stub

## Overview

Full-file `.topolay` persistence for overlay data. Handles schema
serialization, binary data encoding, batch I/O, and schema migration.

## File Format

See [.topolay format spec](../../specs/topolay-format.md) for the
binary layout.

## Key Functions

- `saveOverlayStore` — write all overlays to sidecar directory
- `loadOverlayStore` — read overlays with schema validation
- `migrateOverlayData` — migrate between schema versions

## ChunkIndex

`Topo.Overlay.Storage.ChunkIndex` — reserved chunk-index flags for
future random-access chunk loading. Currently unset (bit 0 in flags).
