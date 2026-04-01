# Metadata System

> **Modules:** `Topo.Metadata` (291 LOC), `Topo.PlateMetadata` (176 LOC)
> **Status:** Stub

## Overview

`Topo.Metadata` provides a type-safe, versioned, extensible metadata
store. Metadata can be attached per-hex and per-region, with support
for migration across schema versions.

## Key Types

- **`Metadata` class** — existential class with versioned encode/decode
- **`MetadataStore`** — per-hex and per-region keyed storage

## PlateMetadata

`Topo.PlateMetadata` provides a `PlateHexMeta` convenience type for
snapshotting plate tectonic fields at a single hex. Implements the
`Metadata` class for persistence.

## Design

The metadata system uses an existential approach: any type implementing
the `Metadata` class can be stored and retrieved by key. This enables
extensibility without modifying core types.
