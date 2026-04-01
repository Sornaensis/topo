# Pipeline Overview

> **Module:** `Topo.Pipeline` (247 LOC)
> **Status:** Stub

## Overview

The pipeline engine runs an ordered sequence of `PipelineStage`s,
threading `TerrainWorld` state through each stage. It supports:

- **Stage disabling** with automatic dependency closure
- **Progress callbacks** for UI integration
- **Snapshot capture** at each stage for debugging/visualization

## Key Types

- **`PipelineConfig`** — list of stages + callback configuration
- **`PipelineStage`** — a named transformation `TerrainWorld -> IO TerrainWorld`

## Execution Model

Stages run sequentially. Each stage receives the current world and
returns a modified world. The pipeline tracks which stages have run
and reports progress.

## Stage Disabling

When a stage is disabled, `Pipeline.Dep.disabledClosure` computes the
transitive set of all stages that depend on it. Those are automatically
disabled too, preventing broken invariants.

## See Also

- [Stage Reference](stages.md) — all built-in stage IDs
- [World Generation](world-gen.md) — config presets and stage assembly
