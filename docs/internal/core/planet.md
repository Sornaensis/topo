# Planet Model

> **Module:** `Topo.Planet` (420 LOC)
> **Status:** Stub

## Overview

`Topo.Planet` defines planetary-scale parameters and the mapping between
hex grid coordinates and geographic (latitude/longitude) coordinates.

## Key Types

- **`PlanetConfig`** — radius, axial tilt, base insolation
- **World slicing** — a "window" into a planet surface

## Scale

1 hex ≈ 13 miles. This constant drives all spatial calculations.

## Key Functions

<!-- TODO: Document latitude mapping, insolation, etc. -->

| Function | Purpose |
|----------|---------|
| `hexToLatitude` | Convert hex row to latitude |
| `latitudeInsolation` | Solar input at latitude + season |
| `planetWindow` | Define the world extent on the planet |
