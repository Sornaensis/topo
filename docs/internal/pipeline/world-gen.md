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
| `defaultWorldGenConfig` | Tuned balanced continental default preset. |
| `continentalWorldGenConfig` | Named alias for the balanced/default continent preset. |
| `archipelagoWorldGenConfig` | Terrain-shape preset for fragmented island chains. |
| `largeOceanWorldGenConfig` | Terrain-shape preset for large oceans and marginal seas. |
| `inlandSeaWorldGenConfig` | Terrain-shape preset for enclosed basins and inland seas. |
| `aridWorldGenConfig` | Default terrain shape with arid biome tuning. |
| `lushWorldGenConfig` | Default terrain shape with lush biome tuning. |
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
| `defaultWorldGenConfig` / `continentalWorldGenConfig` | Balanced Earth-like continental terrain. Uses explicit soft ocean-edge depth (`0.10`, falloff `40`) so local slices do not receive the legacy implicit moat, targets roughly 35–50% land across validation seeds, and keeps continent mask, shelf, coast, plate height, and water-body knobs tuned together. |
| `archipelagoWorldGenConfig` | Fragmented island-chain terrain with higher ocean level, smaller plates, sharper coasts, stronger explicit ocean edges, and light volcanism accenting island arcs. |
| `largeOceanWorldGenConfig` | Large-ocean terrain with one or two strong ocean-source edges, broad shelves/coastal ramps, and a few marginal landmasses. |
| `inlandSeaWorldGenConfig` | Enclosed-basin terrain with weak explicit edges to disable implicit auto-ocean bias, higher land support, and lower water-body edge thresholds for inland seas and lakes. |
| `aridWorldGenConfig` | Default terrain shape with arid biome tuning. |
| `lushWorldGenConfig` | Default terrain shape with lush biome tuning. |

The presets are ordinary `WorldGenConfig` values; callers can override any
nested config field before building a pipeline.  The shape presets intentionally
set non-slider `GenConfig` fields such as `gcContinentScale`, `gcLandRatio`,
`gcShelfWidth`, and `gcCoastSharpness`, plus water-body fields where needed.
`topo-seer` snapshots persist the full `WorldGenConfig` and preserve those
non-slider fields when a snapshot is restored through the UI.

## Alternative builders

`buildPipelineConfig` emits only `plate-terrain` and is useful for callers that
need a minimal plate-driven terrain seed. `buildBaseHeightPipelineConfig` emits
only `base-height`, the noise-driven alternative stage that is outside the
canonical full pipeline.
