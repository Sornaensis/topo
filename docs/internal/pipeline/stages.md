# Stage Reference

> **Modules:** `Topo.Pipeline.Stage` (167 LOC), `Topo.Pipeline.Dep` (86 LOC)
> **Status:** Stub

## Overview

Every built-in generation step has a typed `StageId`. Stage IDs use
kebab-case canonical names for serialization.

## Built-in Stages

<!-- TODO: List all 20+ StageId constructors with their canonical names -->

| StageId Constructor | Canonical Name | Domain |
|---------------------|---------------|--------|
| `StageBaseHeight` | `"base-height"` | Geology |
| `StageTectonics` | `"tectonics"` | Geology |
| `StageHypsometry` | `"hypsometry"` | Geology |
| `StageErosion` | `"erosion"` | Geology |
| `StageVolcanism` | `"volcanism"` | Geology |
| `StageSoil` | `"soil"` | Ecology |
| `StageVegetationBootstrap` | `"vegetation-bootstrap"` | Ecology |
| `StageClimate` | `"climate"` | Climate |
| `StageBiome` | `"biome"` | Biome |
| `StageConvergence` | `"convergence"` | Climate↔Biome |
| ... | ... | ... |
| `StagePlugin` | (user-defined) | Plugin |

## Dependency Graph

`Topo.Pipeline.Dep.builtinDependencies` defines the dependency DAG
between stages. Key ordering constraints:

- Tectonics before erosion
- Erosion before hydrology
- Climate before biome
- Biome convergence iterates climate ↔ biome
- Hydrology after biome convergence
- Weather init after climate

## disabledClosure

Given a set of manually disabled stages, `disabledClosure` computes
all stages that must also be disabled transitively.
