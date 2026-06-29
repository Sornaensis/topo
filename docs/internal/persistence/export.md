# Chunk Codecs

> **Module:** `Topo.Export` (1087 LOC)
> **Status:** Implemented with terrain, overlay, mesh, and sample service/API surfaces

## Overview

Binary encode/decode for all chunk types. Provides per-chunk and
per-region export functions used by `Topo.Storage`.

## Supported Chunk Types

| Chunk Type | Versions |
|------------|----------|
| Terrain | V1, V2, V3 |
| Climate | V1, V2 |
| Weather | V1 |
| River | V1 |
| Groundwater | V1 |
| Volcanism | V1 |
| Glacier | V1 |
| Vegetation | V1, V2 |
| Water body | V1 |
| Biome | V1 |

## Topo-seer Export/API Surfaces

Persisted/exportable formats have UI actions, AppService methods, HTTP routes, and diagnostics:

| Format | Route / command | Payload |
|--------|-----------------|---------|
| Terrain chunk JSON | `POST /terrain/export`, `export_terrain_data` | Selected fields per chunk plus `available_fields` and diagnostics. |
| Overlay JSON | `POST /overlays/export`, `export_overlay_data` | `.toposchema` JSON, `.topolay` provenance, and storage-aware overlay payload. |
| Overlay import validation | `POST /overlays/import/validate`, `validate_overlay_import` | Validates schema + payload and returns `valid` plus diagnostics. |
| Mesh patch JSON | `POST /terrain/mesh/export`, `export_mesh_data` | Region, vertices, indices, counts, and diagnostics. |
| Terrain sample JSON | `POST /terrain/sample/export`, `export_sample_data` | Normalized or real-unit sample at `x`,`y` with diagnostics. |

Save/load responses include the persisted world bundle formats (`world.topo`, `world.topolay`, `config.json`, `meta.json`) and success diagnostics; failures propagate as service/HTTP error envelopes with the failed operation context.

## Design Notes

At 1087 LOC this module is a refactoring candidate. Could be split
into per-chunk-type encoder/decoder modules.
