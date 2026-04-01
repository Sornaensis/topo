# Core Types Reference

> **Module:** `Topo.Types` (1853 LOC)
> **Status:** Stub — needs full type catalogue

## Overview

`Topo.Types` is the gravity centre of the library. Every module imports
it. It defines:

- All per-tile chunk data structures
- The tile-level query type (`TerrainSample`)
- World configuration (`WorldConfig`, `WorldExtent`)
- Biome vocabulary (56+ `BiomeId` pattern synonyms)
- Terrain form classification (15 forms)
- Chunk coordinate system
- All supporting newtypes and enums

## Chunk Data Structures

<!-- TODO: Document each chunk type with its fields and purpose -->

| Chunk Type | Fields | Purpose |
|------------|--------|---------|
| `TerrainChunk` | | Elevation, hardness, crust, plate ID |
| `ClimateChunk` | | Temperature, moisture, precipitation, wind |
| `WeatherChunk` | | Dynamic weather state per tile |
| `RiverChunk` | | River segments, discharge |
| `VolcanismChunk` | | Vent data, eruption state |
| `GlacierChunk` | | Ice thickness, flow direction |
| `WaterBodyChunk` | | Water body classification, depth |
| `VegetationChunk` | | Canopy cover, density, albedo |
| `GroundwaterChunk` | | Water table depth, infiltration |

## Configuration Types

<!-- TODO: Document WorldConfig, WorldExtent, etc. -->

## BiomeId Reference

See [Biome ID Reference](../biome/biome-ids.md) for the complete
56+ biome vocabulary.

## Terrain Forms

<!-- TODO: List all 15 terrain form constructors -->

## Design Notes

- This module is 1853 LOC and is a candidate for splitting into
  submodules (e.g., `Types.Chunk`, `Types.Biome`, `Types.Config`).
- All values are normalised to `[0,1]` or `[-1,1]` internally.
