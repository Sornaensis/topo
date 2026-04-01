# Climate Configuration

> **Module:** `Topo.Climate.Config` (633 LOC)
> **Status:** Stub

## Overview

Five cohesive sub-configs bundled into `ClimateConfig`:

| Sub-Config | Controls |
|------------|----------|
| `TemperatureConfig` | Base temp, lapse rate, ocean moderation |
| `WindConfig` | Belt boundaries, strengths, ITCZ position |
| `MoistureConfig` | Ocean evaporation rate, transport decay |
| `PrecipitationConfig` | Orographic lift, saturation curves |
| `BoundaryConfig` | Land/ocean transition smoothing |
| `SeasonalityConfig` | Axial tilt effect scaling |

All fields have sensible defaults and JSON serialization via
`Config.JSON.configOptions`.

## Temperature Normalization

`TemperatureConfig` fields `tmpEquatorTemp` and `tmpPoleTemp` store
**normalized [0,1] values**, not raw Celsius. The slider UI exposes
a Celsius domain (−50 °C to +50 °C); translations between the two
representations live in `Seer.Config.SliderConfig.Data`:

| Direction | Helper | Formula |
|-----------|--------|---------|
| Celsius → norm | `tempCelsiusToNorm` | `(c + 50) / 100` |
| norm → Celsius | `tempNormToCelsius` | `n × 100 − 50` |

Other temperature-related slider fields (lapse rate, latitude exponent,
etc.) are stored directly in their domain units and do **not** need this
conversion.

## Latitude Coverage

The generated world covers ±`(ExtentY × ChunkSize) / hexesPerDegreeLat`
degrees from `SliceLatCenter`.  With defaults (ExtentY = 2,
ChunkSize = 64, Earth radius) this is roughly **±24°**—an entirely
tropical/subtropical slice.  Increase `ExtentY` or shift
`SliceLatCenter` toward a pole to produce temperate and polar climates.
