# Soil

> **Module:** `Topo.Soil` (163 LOC)
> **Status:** Stub

## Overview

Early-pipeline soil derivation. Computes soil type, depth, grain size,
and fertility from geological inputs.

## Inputs

- Rock hardness (from tectonics)
- Moisture (from base climate or hydrology)
- Elevation

## Outputs

Written to terrain chunks:
- Soil type
- Soil depth
- Grain size
- Base fertility

## Position in Pipeline

Runs after erosion, before vegetation bootstrap. Provides the substrate
that vegetation and biome classification build on.
