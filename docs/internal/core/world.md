# World Container

> **Module:** `Topo.World` (498 LOC)
> **Status:** Stub

## Overview

`Topo.World` defines `TerrainWorld`, the mutable container that holds all
per-chunk data maps. Every pipeline stage reads from and writes to a
`TerrainWorld`.

## Key Types

- **`TerrainWorld`** — the top-level world state: chunk maps for terrain,
  climate, weather, rivers, volcanism, glaciers, water bodies, vegetation,
  and groundwater, plus metadata, provenance, overlay manifest, and
  overlay store.

## Key Functions

<!-- TODO: Document primary accessors -->

| Function | Signature | Purpose |
|----------|-----------|---------|
| `emptyWorld` | | Construct a blank world |
| `getTerrainChunk` | | Read a terrain chunk by ID |
| `setTerrainChunk` | | Write a terrain chunk by ID |
| `worldExtent` | | Query world dimensions |

## Chunk Map Architecture

Each data layer is stored as an `IntMap` keyed by chunk ID. Chunk IDs
are deterministic from hex coordinates via `Topo.Hex`.

## Relationship to Pipeline

Pipeline stages receive a `TerrainWorld` and return a modified copy.
The pipeline engine threads the world state through all stages
sequentially.
