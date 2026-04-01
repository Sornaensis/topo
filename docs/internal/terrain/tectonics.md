# Tectonics

> **Module:** `Topo.Tectonics` (830 LOC)
> **Status:** Stub

## Overview

Plate tectonics engine. Generates tectonic plates, classifies boundaries,
and modifies elevation and material properties based on plate interactions.

## Algorithm

- Plate seed generation (Voronoi-based)
- Boundary classification: convergent, divergent, transform
- Rift profile generation for divergent boundaries
- Boundary shaping noise
- Per-tile output: plate ID, boundary type, crust type (oceanic/continental),
  hardness, elevation modifications

## Configuration

<!-- TODO: Document TectonicsConfig fields -->

## Output

Writes to `TerrainChunk`:
- `tcPlateId` — tectonic plate assignment
- `tcBoundaryType` — boundary classification
- `tcCrustType` — oceanic vs continental
- `tcHardness` — material hardness from plate composition
- Elevation modifications from convergent/divergent dynamics
