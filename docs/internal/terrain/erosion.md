# Erosion

> **Module:** `Topo.Erosion` (467 LOC)
> **Status:** Stub

## Overview

Hydraulic and thermal erosion simulation. Operates on the global
elevation grid (cross-chunk) with local deposition and coastal smoothing.

## Erosion Types

- **Hydraulic** — water-driven erosion along steepest descent
- **Thermal** — temperature-driven weathering (talus slopes)
- **Coastal** — ocean-edge smoothing

## Key Parameters

<!-- TODO: Document ErosionConfig -->

- Iteration counts
- Erosion rates (hydraulic, thermal)
- Hardness resistance scaling
- Deposition threshold
- Coastal smoothing radius

## Terrain Form Interaction

Erosion rates are modulated by terrain form via
`TerrainForm.Modifiers` — e.g., badlands erode faster, mountain
ridges resist more.
