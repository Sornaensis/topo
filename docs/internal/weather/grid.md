# Weather Grid

> **Module:** `Topo.Weather.Grid` (221 LOC)
> **Status:** Stub

## Overview

Conversion between weather overlay data and global weather grids.

## Overlay Schemas

Current simulated weather uses the dense `weather` overlay. It is the
`instantaneous_current` basis with `simulated_generated_weather` source kind.

| Field | Type | Description |
|-------|------|-------------|
| temperature | Float | Current simulated temperature |
| humidity | Float | Current simulated relative humidity |
| wind_dir | Float | Current simulated wind direction (radians) |
| wind_speed | Float | Current simulated wind speed |
| pressure | Float | Current simulated atmospheric pressure proxy |
| precipitation | Float | Current simulated precipitation rate |
| cloud_cover / cloud_water | Float | Current aggregate cloud fraction / water |
| cloud_cover_low/mid/high | Float | Current cloud cover by altitude band |
| cloud_water_low/mid/high | Float | Current cloud water by altitude band |

Generated typical normals use a separate dense `weather_normals` overlay. It is
the `typical_normal` basis with `generated_climate` source kind and mirrors the
weather fields except that values are deterministic normals derived from climate
averages and `WeatherConfig`.

## Functions

- Grid builders: overlay → per-field arrays
- Grid writers: per-field arrays → overlay
- Persistence/export layers keep current fields and typical-normal fields
  separate; code should not fall back from typical normals to current clouds.
