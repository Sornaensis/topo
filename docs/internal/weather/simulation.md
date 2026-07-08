# Weather Simulation

> **Modules:** `Topo.Weather.Tick` (438 LOC), `Topo.Weather.Operators` (111 LOC)
> **Status:** Stub

## Overview

Tick-based weather simulation. Each tick applies physical operators
to the weather fields and injects stochastic perturbation.

## Simulation Steps (per tick)

1. **Advection** — wind carries temperature/humidity
2. **Diffusion** — smooth spatial gradients
3. **Condensation** — humidity → precipitation when saturated
4. **Pressure gradient wind** — update wind from pressure field
5. **Stochastic perturbation** — seeded random jitter

## Topo.Weather.Tick

`weatherSimNode` produces a `SimNode` for the simulation DAG.
Initialises from `initWeatherStage`.

## Topo.Weather.Operators

Low-level grid operators: advection, diffusion, condensation,
pressure-gradient wind, angle normalization/interpolation.

## Determinism and Source Boundaries

Weather is deterministic given the same seed and tick sequence. Seed derivation
uses `deriveOverlaySeed(worldSeed, tick)`.

The built-in simulation produces generated/simulated weather only. API and export
DTOs label this current basis as `instantaneous_current` with source kind
`simulated_generated_weather`. Typical normals are generated from climate and
labelled `typical_normal`/`generated_climate`. Neither path represents external
live weather data unless a future external data-source integration is explicitly
configured and surfaced with a distinct source kind.
