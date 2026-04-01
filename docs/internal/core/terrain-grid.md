# Terrain Grid

> **Module:** `Topo.TerrainGrid` (565 LOC)
> **Status:** Stub

## Overview

`Topo.TerrainGrid` builds global grids from chunk maps for cross-chunk
stencil operations (erosion, flow routing, climate advection). It then
writes results back to chunk maps.

This is key infrastructure: many generators cannot operate on isolated
chunks because they need neighbour data across chunk boundaries.

## Key Functions

<!-- TODO: Document grid builders and updaters -->

| Function | Purpose |
|----------|---------|
| `buildElevationGrid` | Global elevation array from terrain chunks |
| `buildMoistureGrid` | Global moisture array |
| `buildClimateGrid` | Global climate array |
| `updateChunksFromGrid` | Write grid results back to chunks |
| `gridSlope` | Compute slope at a grid cell |
| `classifyTerrainForm` | Assign terrain form from local geometry |

## Design Notes

Grid operations are O(world-size) and allocate temporary arrays. They
are called once per pipeline stage, not per-tile.
