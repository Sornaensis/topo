# World Generation

> **Module:** `Topo.WorldGen` (391 LOC)
> **Status:** Stub

## Overview

`Topo.WorldGen` is the top-level generation orchestrator. It bundles
all stage-specific configs into `WorldGenConfig` and assembles a
`PipelineConfig` from it.

## Key Types

- **`WorldGenConfig`** — bundles every stage's configuration
- **`buildPipelineConfig`** — assemble pipeline from config
- **`buildFullPipelineConfig`** — assemble with all stages enabled

## Named Presets

| Preset | Character |
|--------|-----------|
| `defaultWorldGenConfig` | Balanced Earth-like terrain |
| `aridWorldGenConfig` | Hot, dry, desert-heavy |
| `lushWorldGenConfig` | Wet, forested, river-heavy |

## Configuration Flow

```
WorldGenConfig
  ├── BaseHeight.GenConfig
  ├── Tectonics.TectonicsConfig
  ├── Erosion.ErosionConfig
  ├── ClimateConfig
  ├── BiomeConfig
  ├── WeatherConfig
  ├── ... (all stage configs)
  │
  └──► buildPipelineConfig ──► PipelineConfig ──► Pipeline.run
```
