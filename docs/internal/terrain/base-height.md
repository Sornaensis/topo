# Base Height Generation

> **Module:** `Topo.BaseHeight` (187 LOC)
> **Status:** Stub

## Overview

Pure noise-based heightmap generation. Produces the initial elevation
field before plate tectonics modify it.

## Algorithm

- FBM noise with configurable octaves and persistence
- Domain warping for organic continent shapes
- Continent-scale mask controlling land/ocean ratio
- Ocean edge depth falloff

## Key Types

- **`GenConfig`** — noise parameters, land ratio, ocean depth

## Key Functions

- `sampleBaseHeightAt` — pure per-tile elevation sampling
