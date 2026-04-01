# Sim Nodes

> **Module:** `Topo.Simulation` (188 LOC)
> **Status:** Stub

## Overview

Core simulation types used by the DAG executor and plugin system.

## Key Types

| Type | Description |
|------|-------------|
| `SimNode` | A simulation participant (reader or writer) |
| `SimContext` | Runtime context for a sim node tick |
| `TerrainWrites` | Accumulated terrain mutations from a writer node |

## Node Types

- **Reader** — reads terrain + dependency overlays, writes to own overlay
- **Writer** — reads everything, can also mutate terrain chunks

## Ownership

Each `SimNode` owns exactly one overlay. It can read terrain and
declared dependency overlays, but only writes to its owned overlay.
