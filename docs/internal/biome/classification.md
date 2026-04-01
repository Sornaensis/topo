# Biome Classification

> **Module:** `Topo.Biome` (724 LOC)
> **Status:** Stub

## Overview

Primary Whittaker-style temperature × precipitation biome classification.

## Algorithm

1. **BiomeRule lookup** — temperature/precipitation centroid matching
2. **Nearest-centroid fallback** — when no rule matches exactly
3. **Per-chunk smoothing** — reduce noise at biome boundaries
4. **Mountain transition** — smooth biome transitions with elevation
5. **Vegetation density** — derived per-biome base density

## Classification Space

Biomes are assigned in normalised temperature × moisture space.
The Whittaker diagram is discretised into rule regions.

## Convergence

This stage participates in the climate ↔ biome convergence loop.
Vegetation albedo from biome assignment feeds back into temperature
calculation.
