# Water Bodies

> **Module:** `Topo.WaterBody` (379 LOC)
> **Status:** Stub

## Overview

Water body classification via flood-fill from the flow-routed terrain.

## Classification

| Type | Definition |
|------|------------|
| Ocean | Grid-edge-touching water |
| Inland sea | Large landlocked water body |
| Lake | Small landlocked water body |

## Key Output

- Per-tile water body ID and type
- Pour-point surface elevation for each basin
