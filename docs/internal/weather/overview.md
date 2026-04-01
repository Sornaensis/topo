# Weather Overview

> **Module:** `Topo.Weather` (41 LOC)
> **Status:** Stub

## Overview

Public facade for the weather simulation subsystem. Re-exports from
submodules for a stable API surface.

## Submodules

| Module | Purpose |
|--------|---------|
| [Weather Config](config.md) | 20+ parameters |
| [Weather Grid](grid.md) | Overlay↔grid conversion |
| [Weather Simulation](simulation.md) | Tick-based evolution |

## Relationship to Climate

Climate is the static base state. Weather is the dynamic, tick-evolving
layer on top. Weather initialises from climate and then diverges
stochastically each tick.
