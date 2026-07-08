# Pipeline Stage Reference

> **Modules:** `Topo.Pipeline.Stage`, `Topo.Pipeline.Dep`,
> `Topo.Pipeline.Registry`, `Topo.Pipeline.Diagnostics`

This reference is contract-tested from the compiled stage registry. The section
between the generated markers must match `stageDocsMarkdown builtinStageDocs`;
`Spec.PluginDocs` fails when the committed documentation drifts from the
registry, canonical stage IDs, or dependency graph.

## Canonical identifiers

Every built-in generation step has a typed `StageId`. External-facing surfaces
(plugin manifests, API JSON, diagnostics, logs, and UI labels) use kebab-case
canonical names from `stageCanonicalName`. Dynamic plugin stages use
`plugin:<manifest.name>` and parse through `StagePlugin`.

`allBuiltinStageIds` is the canonical full-pipeline order. It excludes
`StagePlugin` and the alternative/legacy `StageBaseHeight` and `StageTectonics`
variants, which are documented below but are not part of the default full
pipeline.

## Generated built-in registry

<!-- BEGIN GENERATED: pipeline-stage-registry -->
## plate-terrain — Plate terrain
Generates plate-aware base terrain, crust, and plate metadata.
Dependencies: none
Config: terrain.gen, terrain.tectonics, planet, world.slice
Outputs: terrain.elevation, terrain.hardness, terrain.plate_id, terrain.plate_boundary, terrain.plate_crust, terrain.plate_height, terrain.plate_age
Diagnostics: Root generation stage; disabling it disables most downstream stages.


## erosion — Erosion
Applies hydraulic, thermal, wind, and coastal erosion to terrain.
Dependencies: plate-terrain
Config: terrain.erosion, terrain.gen, terrain.form, water_level
Outputs: terrain.elevation, terrain.moisture, terrain.sediment, terrain.hardness
Diagnostics: Requires plate terrain; high iteration counts increase generation time.


## hypsometry — Hypsometry
Redistributes elevations into configured land/ocean hypsometric profiles.
Dependencies: plate-terrain
Config: terrain.hypsometry
Outputs: terrain.elevation, hypsometry
Diagnostics: Runs before water and climate classification so elevation bands stay consistent.


## volcanism — Volcanism
Adds volcanic vents, lava, ash, and terrain adjustments around tectonic boundaries.
Dependencies: plate-terrain
Config: terrain.volcanism
Outputs: volcanism, terrain.elevation, terrain.fertility, terrain.rock_type
Diagnostics: Depends on plate metadata from plate-terrain.


## hydrology — Hydrology
Computes flow routing, moisture, groundwater, and hydrologic terrain metrics.
Dependencies: plate-terrain
Config: terrain.hydrology, terrain.form
Outputs: hydrology.flow, terrain.moisture, groundwater, terrain.slope
Diagnostics: Required by rivers, water bodies, and water-table stages.


## rivers — Rivers
Routes and carves river networks from hydrology and groundwater inputs.
Dependencies: hydrology
Config: terrain.rivers, terrain.river_topology, terrain.groundwater, water_level
Outputs: rivers, terrain.elevation, terrain.moisture
Diagnostics: Requires hydrology; disabled hydrology auto-disables river output.


## water-body — Water bodies
Classifies oceans, lakes, inland seas, and water-body connectivity.
Dependencies: plate-terrain, hydrology
Config: terrain.water_body, water_level
Outputs: water_body, water_bodies, terrain.water_body_type
Diagnostics: Requires terrain and hydrology so basin classification is stable.


## soil — Soil
Derives soil depth, grain, type, and fertility from terrain and moisture.
Dependencies: plate-terrain
Config: terrain.soil
Outputs: soil, terrain.soil_depth, terrain.soil_grain, terrain.soil_type, terrain.fertility
Diagnostics: Feeds vegetation bootstrap and biome classification.


## vegetation — Vegetation
Bootstraps vegetation density and albedo before climate and biomes.
Dependencies: plate-terrain, soil
Config: terrain.vegetation, water_level
Outputs: vegetation, terrain.vegetation_density, terrain.albedo
Diagnostics: Requires soil; climate uses this initial vegetation signal.


## climate — Climate
Generates temperature, precipitation, winds, and climate diagnostics.
Dependencies: plate-terrain, vegetation
Config: world.climate, world.weather, water_level
Outputs: climate.temperature, climate.precipitation, climate.wind, climate_diagnostics
Diagnostics: Downstream ocean currents, glaciers, biomes, convergence, and weather depend on climate.


## ocean-currents — Ocean currents
Derives ocean current and sea-surface temperature modifiers.
Dependencies: climate
Config: world.ocean_current, water_level
Outputs: ocean_currents, climate.sea_surface_temperature
Diagnostics: Requires climate so temperature gradients are available.


## glacier — Glaciers
Computes glacier accumulation, flow, erosion, and snow/ice diagnostics.
Dependencies: climate, plate-terrain
Config: terrain.glacier, terrain.form, water_level
Outputs: glacier, glacier_snow_ice, terrain.elevation
Diagnostics: Requires climate and terrain; glaciers may adjust final elevation.


## parameters — Parameter layers
Refreshes derived terrain parameters and terrain-form metrics after shape changes.
Dependencies: plate-terrain
Config: terrain.parameters, terrain.form, water_level
Outputs: terrain.roughness, terrain.rock_density, terrain_form, terrain_form_metrics, terrain.relief
Diagnostics: Runs after glaciers so slope, relief, and terrain forms reflect final terrain.


## water-table — Water table
Computes infiltration, water table depth, and root-zone moisture.
Dependencies: plate-terrain, hydrology
Config: terrain.water_table
Outputs: water_table, terrain.root_moisture
Diagnostics: Requires hydrology and terrain moisture.


## biomes — Biomes
Classifies biomes from climate, vegetation, terrain, and water signals.
Dependencies: climate, plate-terrain, vegetation
Config: world.biome, water_level
Outputs: biome, biome_refinement, terrain.biome_code
Diagnostics: Requires climate and vegetation; feeds vegetation feedback and weather.


## vegetation-feedback — Vegetation feedback
Applies biome-to-vegetation feedback and albedo corrections.
Dependencies: biomes
Config: world.biome_feedback, terrain.vegetation
Outputs: vegetation, terrain.vegetation_density, terrain.albedo
Diagnostics: Requires biome classification; repeated by convergence iterations when configured.


## convergence — Climate/biome convergence
Pseudo-stage documenting repeated climate, biome, and vegetation-feedback cycles.
Dependencies: climate, biomes, vegetation-feedback
Config: world.biome_feedback.convergence_iterations
Outputs: climate, biome, vegetation
Diagnostics: Pseudo-stage for diagnostics; concrete runs appear as repeated climate/biome/feedback stages.


## weather — Weather
Initializes weather and generated-normal overlays for the generated world.
Dependencies: climate, biomes
Config: world.weather
Outputs: weather, weather_normals, weather_snapshot, weather_timeline, weather, weather_normals
Diagnostics: Requires climate and biomes so weather starts from classified terrain.
<!-- END GENERATED: pipeline-stage-registry -->

## Alternative stage variants

`StageBaseHeight` (`base-height`) is a noise-driven base terrain generator used
by small or legacy pipelines. It is an alternative to `plate-terrain`, not a
member of `allBuiltinStageIds` for the default full pipeline.

`StageTectonics` (`tectonics`) is a standalone tectonic metadata stage for
custom pipelines. The default full pipeline folds tectonic work into
`plate-terrain`.

Dynamic plugin stages use `StagePlugin name` and serialize as `plugin:<name>`.
Plugin stages are inserted from manifest `generator.insertAfter` and
`generator.requires` declarations after dependency resolution.

## Dependency graph

`builtinDependencies` is the direct dependency graph used by stage disabling,
registry docs, API/UI diagnostics, and tests. Each `StageDep` records one stage
and the stages that must run before it. The generated registry above joins each
`PipelineStageDoc` row to this graph through `stageDocDependencies`; this keeps
documentation, dependency closure, and diagnostics aligned.

Important dependency-shape examples:

- `plate-terrain` is the root for most physical terrain stages.
- `hydrology` feeds `rivers`, `water-body`, and `water-table`.
- `soil` feeds `vegetation`, which feeds `climate` and then `biomes`.
- `biomes` feeds `vegetation-feedback`, `convergence`, and `weather`.
- `weather` is a leaf in the built-in dependency graph.

## Disable diagnostics

`disabledClosure builtinDependencies explicitDisabled` returns every stage that
must be skipped when a user disables one or more roots. If `climate` is disabled,
for example, `ocean-currents`, `glacier`, `biomes`, `vegetation-feedback`,
`convergence`, and `weather` become auto-disabled.

`pipelineStageDiagnostics` produces API/UI-ready status rows with:

| Field | Meaning |
| --- | --- |
| `psdiagEnabled` | The stage remains runnable after dependency closure. |
| `psdiagExplicitlyDisabled` | The user or API disabled this stage directly. |
| `psdiagAutoDisabled` | The stage is disabled because one dependency is disabled. |
| `psdiagDependencies` | Direct prerequisites from `builtinDependencies`. |
| `psdiagDependents` | Direct dependents from `dependentsOf`. |
| `psdiagDisabledBy` | Disabled direct prerequisites explaining auto-disablement. |
| `psdiagDiagnostics` | Stable user-facing diagnostic strings. |

`inferExplicitDisabledRoots` accepts older closure-style disabled sets and
recovers the minimal explicit roots by removing stages that already have a
disabled direct dependency.
