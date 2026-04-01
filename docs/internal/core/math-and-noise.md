# Math & Noise

> **Modules:** `Topo.Math` (57 LOC), `Topo.Noise` (156 LOC), `Topo.Seed` (12 LOC)
> **Status:** Stub

## Overview

Shared numeric primitives and deterministic procedural noise functions
used by all terrain generators.

## Topo.Math

Small utility functions shared across generators:

| Function | Purpose |
|----------|---------|
| `clamp01` | Clamp to [0,1] |
| `clampLat` | Clamp latitude range |
| `lerp` | Linear interpolation |
| `smoothstep` | Hermite smoothstep |
| `iterateN` | Apply function N times |

## Topo.Noise

Deterministic procedural noise. All functions are pure and seeded.

| Function | Purpose |
|----------|---------|
| `noise2D` | 2D value noise |
| `noise2DContinuous` | Smooth 2D noise |
| `hashSeed` | Deterministic seed mixing |
| `fbm2D` | Fractal Brownian motion |
| `ridgedFbm2D` | Ridged multi-fractal |
| `domainWarp2D` | Domain warping |
| `directionalRidge2D` | Directional ridge noise |

## Topo.Seed

Thin wrapper for deterministic per-tick seed derivation:

```haskell
deriveOverlaySeed :: Word64 -> Word64 -> Word64
```

Uses the golden ratio constant `0x9E3779B97F4A7C15` for mixing.
