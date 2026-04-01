# Sampling API

> **Module:** `Topo.Sample` (328 LOC)
> **Status:** Stub

## Overview

Public query API for reading terrain data. This is the primary
read-side boundary for consumers.

## Key Functions

| Function | Purpose |
|----------|---------|
| `sampleHeight` | Elevation at a point |
| `sampleTerrain` | Full `TerrainSample` at a point |
| `interpSample` | Bilinear interpolation between tiles |
| `hexAt` | Hex coordinate from world position |
| `hexData` | All data layers for a hex tile |
