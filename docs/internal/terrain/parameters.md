# Terrain Parameters

> **Module:** `Topo.Parameters` (869 LOC)
> **Status:** Stub

## Overview

Derives secondary terrain parameters from raw elevation and material
data. These derived layers feed biome classification, vegetation,
and display.

## Derived Parameters

| Parameter | Description |
|-----------|-------------|
| Directional slope (×6) | Slope in each hex direction |
| Curvature | Convexity/concavity |
| Relief | Local elevation range |
| Ruggedness | Terrain roughness index |
| Terrain form | Classification into 15 types |
| Rock type | Derived from hardness + crust |
| Soil type | Surface material classification |
| Roughness | Surface roughness for climate |
| Soil depth | Weathering-derived depth |
| Fertility | Soil nutrient potential |

## Cross-Chunk Operations

Uses `TerrainGrid` for neighbour lookups across chunk boundaries.
