# Hex Grid

> **Module:** `Topo.Hex` (202 LOC)
> **Status:** Stub

## Overview

`Topo.Hex` defines the hexagonal coordinate system used throughout the
library. All spatial operations — neighbour lookup, distance calculation,
directional queries — go through this module.

## Coordinate Systems

- **Axial coordinates** — the primary representation (`q`, `r`)
- **Cube coordinates** — used for distance and rotation calculations

## Key Types

- `HexCoord` — axial hex coordinate
- `HexDirection` — 6-direction enum (NE, E, SE, SW, W, NW)
- `ChunkCoord` — chunk-level coordinate

## Key Functions

<!-- TODO: Document with signatures -->

| Function | Purpose |
|----------|---------|
| `hexNeighbours` | All 6 neighbours of a hex |
| `hexDistance` | Manhattan distance between hexes |
| `hexDirection` | Direction between adjacent hexes |
| `hexToChunk` | Map hex coordinate to chunk ID |
| `chunkHexes` | All hexes in a given chunk |

## Directional Slope

The module also provides directional slope queries used by terrain
parameter derivation (see [Parameters](../terrain/parameters.md)).
