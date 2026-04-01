# Units & Conversion

> **Modules:** `Topo.Units` (294 LOC), `Topo.Units.Config` (766 LOC)
> **Status:** Stub

## Overview

The unit system enforces a strict boundary between normalised internal
values and real-world physical units. Generators never import `Units` —
only consumers and export tools use it.

## Topo.Units

Conversion functions from normalised ranges to physical quantities:

| Function | Converts to |
|----------|-------------|
| `normToC` | Temperature in °C |
| `normToMetres` | Elevation in metres |
| `normToMmYear` | Precipitation in mm/year |
| etc. | |

All conversions are parameterised by `UnitScales`.

## Topo.Units.Config

Real-world-unit configuration variants with bidirectional conversion:

- `RealTemperatureConfig` ↔ normalised temperature config
- `RealGlacierConfig` ↔ normalised glacier config
- `RealBiomeThresholds` ↔ normalised biome thresholds
- `RealForestConfig` ↔ normalised forest config
- etc.

These allow users to configure the system in intuitive physical units
while generators work in normalised space.

## Design Principle

> All internal values are normalised to [0,1] or [-1,1]. Physical units
> exist only at configuration input and display/export output boundaries.
