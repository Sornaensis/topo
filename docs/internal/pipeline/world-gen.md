# World Generation Pipeline Assembly

> **Module:** `Topo.WorldGen`

`Topo.WorldGen` is the top-level builder for generation pipeline presets. It
collects terrain, climate, biome, weather, planet, grid, ocean-current, and
biome-feedback configuration into `WorldGenConfig`, validates iteration counts,
then assembles `PipelineConfig` values consumed by `Topo.Pipeline.runPipeline`.

## Core types and builders

| Name | Role |
| --- | --- |
| `TerrainConfig` | Terrain-stage sub-config bundle (`terrainGen`, `terrainErosion`, `terrainHydrology`, and related fields). |
| `WorldGenConfig` | Top-level config bundle for terrain, climate, biome, weather, planet/slice/grid, ocean currents, and biome feedback. |
| `WorldGenConfigError` | Validation errors for negative iteration counts. |
| `defaultWorldGenConfig` | Balanced default preset. |
| `aridWorldGenConfig` | Default preset with arid biome tuning. |
| `lushWorldGenConfig` | Default preset with lush biome tuning. |
| `mkWorldGenConfig` | Constructor that validates supplied major config sections. |
| `validateWorldGenConfig` | Checks non-negative iteration counts for erosion, climate moisture/wind/coastal passes, biome smoothing, and glacier flow. |
| `buildPipelineConfig` | Plate-terrain-only default builder. |
| `buildFullPipelineConfig` | Full terrain/climate/biome/weather builder. |
| `buildBaseHeightPipelineConfig` | Legacy/small-pipeline base-height-only builder. |
| `autoOceanEdgeDepth` | Adds ocean-edge falloff for slices that expose non-planet-boundary edges unless explicitly configured. |

## Configuration flow

```text
WorldGenConfig
  ├── worldTerrain :: TerrainConfig
  │   ├── terrainGen, terrainTectonics, terrainErosion
  │   ├── terrainHydrology, terrainRivers, terrainGroundwater
  │   ├── terrainVolcanism, terrainWaterBody, terrainGlacier
  │   ├── terrainParameters, terrainFormConfig, terrainSoil
  │   ├── terrainVegetation, terrainHypsometry, terrainWaterTable
  │   └── ...
  ├── worldClimate, worldBiome, worldWeather
  ├── worldPlanet, worldSlice, worldHexGrid
  ├── worldOceanCurrent, worldBiomeFeedback
  └── buildFullPipelineConfig ──► PipelineConfig ──► runPipeline
```

`buildFullPipelineConfig` derives the generation extent from `worldPlanet`,
`worldHexGrid`, `worldSlice`, and `WorldConfig` when `GenConfig` still uses the
default extent. It also applies `autoOceanEdgeDepth` for exposed slice edges.

## Full pipeline order

The full builder emits stages in the same canonical order documented in the
[Stage Reference](stages.md):

1. `plate-terrain`
2. `erosion`
3. `hypsometry`
4. `volcanism`
5. `hydrology`
6. `rivers`
7. `water-body`
8. `soil`
9. `vegetation`
10. `climate`
11. `ocean-currents`
12. `glacier`
13. `parameters`
14. `water-table`
15. `biomes`
16. `vegetation-feedback`
17. zero or more convergence iterations of `climate`, `biomes`, and
    `vegetation-feedback`
18. `weather`

Ordering constraints in the builder mirror `builtinDependencies`: plate terrain
precedes physical terrain refinements, hydrology precedes rivers/water bodies,
soil and vegetation precede climate, climate precedes ocean currents/glaciers
and biomes, and biomes precede weather initialization.

## Presets

| Preset | Character |
| --- | --- |
| `defaultWorldGenConfig` | Balanced Earth-like terrain and climate defaults. |
| `aridWorldGenConfig` | Default terrain/climate with arid biome tuning. |
| `lushWorldGenConfig` | Default terrain/climate with lush biome tuning. |

The presets are ordinary `WorldGenConfig` values; callers can override any
nested config field before building a pipeline.

## Alternative builders

`buildPipelineConfig` emits only `plate-terrain` and is useful for callers that
need a minimal plate-driven terrain seed. `buildBaseHeightPipelineConfig` emits
only `base-height`, the noise-driven alternative stage that is outside the
canonical full pipeline.
