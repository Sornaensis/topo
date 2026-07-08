# Weather Overview

> **Module:** `Topo.Weather` (41 LOC)
> **Status:** Stub

## Overview

Public facade for the weather simulation subsystem. Re-exports from
submodules for a stable API surface.

## Submodules

| Module | Purpose |
|--------|---------|
| [Weather Config](config.md) | 20+ parameters |
| [Weather Grid](grid.md) | Overlay↔grid conversion |
| [Weather Simulation](simulation.md) | Tick-based evolution |

## Relationship to Climate

Climate is the static, generated **long-run average** basis stored in core
`.topo` climate chunks. Weather is the dynamic, tick-evolving
**instantaneous current** basis stored in the `weather` overlay sidecar. Weather
initialises from climate and then diverges deterministically with each simulation
tick.

Topo also writes a `weather_normals` overlay when generated typical cloud/weather
normals are available. That overlay is a **typical normal** basis derived from the
same generated climate averages and weather configuration; it is not an observed
or rolling average of the simulated weather timeline.

Built-in climate, current weather, and typical weather normals are deterministic
simulated/generated data. They are not external live weather observations unless
a future external data-source feature is explicitly configured and identified as
such in API/source metadata.

## Public Semantics

Consumers should prefer basis-qualified names:

| Basis | Source kind | Storage | Canonical examples |
|-------|-------------|---------|--------------------|
| `long_run_average` | `generated_climate` | core `.topo` climate chunks | `climate_temp_avg`, `climate_precip_avg` |
| `instantaneous_current` | `simulated_generated_weather` | `weather` overlay sidecar | `weather_temp_current`, `weather_precip_current`, `weather_cloud_cover_current` |
| `typical_normal` | `generated_climate` | `weather_normals` overlay sidecar | `weather_temp_typical`, `weather_cloud_cover_typical` |

Legacy export names such as `temperature`, `precipitation`,
`weather_temperature`, and `cloud_cover` remain aliases for compatibility, but
docs and diagnostics identify their basis.
