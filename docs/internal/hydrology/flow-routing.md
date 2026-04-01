# Flow Routing

> **Module:** `Topo.Hydrology` (1240 LOC)
> **Status:** Stub

## Overview

The largest module in the library. Implements flow routing and moisture
distribution across the full terrain grid.

## Algorithm Steps

1. **Flow direction** — steepest-descent per tile
2. **Depression filling** — eliminate local minima that trap flow
3. **Sink breaching** — carve channels through low barriers
4. **Flow accumulation** — count upstream contributing area
5. **Piedmont smoothing** — smooth transition at mountain bases
6. **River carving** — incise channels proportional to discharge
7. **Alluvial deposition** — deposit sediment in low-gradient areas

## Pipeline Stages

- `applyHydrologyStage` — flow routing + moisture distribution
- `applyRiverStage` — river topology extraction (see [Rivers](rivers.md))

## Design Notes

This module at 1240 LOC exceeds the recommended ~500 LOC threshold
and is a refactoring candidate.
