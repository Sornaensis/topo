# Pipeline Overview

> **Module:** `Topo.Pipeline`
> **Stage registry:** `Topo.Pipeline.Registry`
> **Diagnostics:** `Topo.Pipeline.Diagnostics`

The pipeline engine runs an ordered list of `PipelineStage` values, threading a
`TerrainWorld` through deterministic generation stages. Stage identity,
dependency closure, registry documentation, and UI/API diagnostics are all typed
around `StageId` rather than ad-hoc strings.

## Core types

| Type | Role |
| --- | --- |
| `PipelineConfig` | Master seed, stage list, explicit disabled set, snapshot flag, and progress callback. |
| `PipelineStage` | One terrain/world transformation with a `StageId`, display name, seed tag, overlay declarations, and `PluginM` action. |
| `StageProgress` | Progress event emitted before/after/skipping stages. |
| `StageStatus` | `StageStarted`, `StageCompleted`, or `StageSkipped`. |
| `PipelineSnapshot` | Optional world snapshot captured after a completed stage. |
| `PipelineError` | Plugin/runtime or overlay dependency failure surfaced to callers. |

## Execution model

`runPipeline` seeds the initial world, validates overlay dependencies, derives a
per-stage seed from the master seed and `stageSeedTag`, then runs each stage in
order. For each stage it:

1. checks whether the stage is in the disabled closure;
2. emits `StageSkipped` and logs `stage:skip` when disabled;
3. registers produced overlays before running enabled stages;
4. emits `StageStarted`, runs the `PluginM` action, then emits
   `StageCompleted`;
5. appends a `PipelineSnapshot` when snapshots are enabled.

Plugin generator stages use the same `PipelineStage` abstraction as built-in
stages through `rpcGeneratorStage`, with `StagePlugin name` identity and
manifest-derived insertion/dependency metadata.

## Dependency closure

`Topo.Pipeline.Dep.builtinDependencies` records direct dependencies between
built-in stages. `disabledClosure` computes every dependent that must be skipped
when a user disables a stage. This prevents invalid partial worlds such as
running weather without climate or biomes.

`Topo.Pipeline.Diagnostics.pipelineStageDiagnostics` uses the same closure logic
to report whether each stage is enabled, explicitly disabled, or auto-disabled,
plus the dependencies that explain the status.

## Registry-backed docs

`Topo.Pipeline.Registry` stores human-readable metadata for stage IDs:
summary, seed tag, config roots, outputs, overlay outputs, and diagnostic hints.
The stage reference is generated/contract-tested from this registry so docs,
UI/API diagnostics, and dependency metadata do not drift.

See [Stage Reference](stages.md) for the generated registry section and exact
dependency table.
