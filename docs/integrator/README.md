# Integrating the Topo library

Topo's reusable Haskell API lives in the `topo` package. `topo-seer` is the
SDL application and HTTP/OpenAPI host; it is not the boundary for direct
library calls. `topo-plugin-sdk` is for out-of-process plugins, and
`topo-plugin-example` plus `topo-plugin-civ-example` are maintained examples.
See each package's `package.yaml` for the canonical exposed-module list and
Haddock for the complete API. This page highlights useful entry points rather
than duplicating that surface.

## Choosing an entry point

`Topo` is the convenient umbrella import for ordinary generation and world
work. Specialist contracts are imported directly:

- `Topo.WorldGen` validates generation configuration and builds pipelines.
  `buildPipelineConfig` is the plate-terrain-only builder,
  `buildFullPipelineConfig` assembles the ordered terrain/climate/biome/weather
  pipeline, and `buildBaseHeightPipelineConfig` is the noise-only alternative.
  The continental, archipelago, large-ocean, inland-sea, arid, and lush values
  are ready-made `WorldGenConfig` presets.
- `Topo.Pipeline` runs a `PipelineConfig` with `runPipeline`. It writes the
  master seed into the world, derives a stable seed from each stage's
  `stageSeedTag`, closes the disabled set over stage dependencies, reports
  ordered progress, and can return post-stage `PipelineSnapshot`s.
  `Topo.Pipeline.Registry` exposes registry-backed stage descriptions,
  including the base-height and standalone-tectonics alternatives;
  `Topo.Pipeline.Diagnostics` explains enabled, explicitly disabled, and
  dependency-disabled stages.
- `Topo.Overlay`, `Topo.Overlay.Schema`, `Topo.Overlay.JSON`, and
  `Topo.Overlay.Export` cover typed overlay construction and interchange.
  `Topo.Overlay.Storage.loadOverlayChunk` is the sparse, viewport-oriented
  chunk reader: it accepts indexed or sequential and compressed or
  uncompressed files, returns an empty chunk for an absent non-negative ID,
  and rejects dense overlays.
- `Topo.Storage` is the low-level `.topo` codec. Use
  `Topo.Persistence.WorldBundle` when terrain and overlays must be saved or
  loaded together. See [file formats](file-formats.md).
- `Topo.Export` owns exact chunk codecs and region exports. Decoders validate
  element counts, structural lengths, enum codes, and trailing bytes. Use
  `canonicalBasisQualifiedExportFields` for climate/weather JSON field names.

## Deterministic generation and helpers

A generation is reproducible when its world configuration, master seed,
pipeline order, and stage configuration are unchanged. `runPipeline` derives
stage seeds from the master seed and a stable FNV-style hash of the stage tag;
the determinism specification compares complete encoded worlds from repeated
runs.

For lower-level integrations:

- `Topo.Hydrology` exports pure flow-direction, depression-fill, sink-breach,
  flow-accumulation, smoothing, river-carving, and alluvial-deposition helpers.
  `Topo.Hydrology.StageHydrology.computeHydrologyGrids` performs the complete
  pure grid calculation used by the hydrology stage.
- `Topo.Grid.Diffusion` and `Topo.Grid.HexDirection` provide row-major axial
  grid helpers. Hex-direction order is `E, NE, NW, W, SW, SE`; diffusion omits
  off-grid neighbours, while directional stepping remains at the boundary.
- `Topo.TerrainGrid`, `Topo.Grid.*`, and the chunk codecs let an integrator work
  below the complete world-generation pipeline without going through an
  application endpoint.

## Simulation contract

`Topo.Simulation` defines overlay-owning reader and writer nodes;
`Topo.Simulation.DAG` builds and executes their dependency graph. Independent
readers execute concurrently by dependency wavefront. Writers execute
sequentially afterwards and observe accumulated earlier writer changes.
`mergeTerrainWrites` gives the right-hand argument precedence for the same
chunk.

A node receives terrain through `SimContext.scTerrain`, with `twOverlays`
stripped to prevent stale or undeclared access. Its declared dependency
overlays are supplied separately in `scOverlays`.
`Topo.Simulation.Pipeline.runSimulationTickPipeline` performs exactly one
hourly tick: it evaluates due nodes against the target hour, applies successful
terrain and overlay updates, advances world time, and persists overlay-local
schedule cursors. A failed run does not advance time or those cursors.

## Source and tests

The authoritative public-module list is
[`topo/package.yaml`](../../topo/package.yaml), and the umbrella is
[`Topo.hs`](../../topo/src/Topo.hs). Maintained specifications under
[`topo/test/Spec`](../../topo/test/Spec) cover pipeline closure and progress,
byte-level determinism, hydrology/grid behavior, storage round trips, and
simulation ordering and atomic failure behavior.
