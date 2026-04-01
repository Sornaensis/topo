# Chunk Codecs

> **Module:** `Topo.Export` (1087 LOC)
> **Status:** Stub

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

## Design Notes

At 1087 LOC this module is a refactoring candidate. Could be split
into per-chunk-type encoder/decoder modules.
