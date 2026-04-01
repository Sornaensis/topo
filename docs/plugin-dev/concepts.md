# Key Concepts

> **Status:** Stub

## The Generation Pipeline

Topo generates terrain in a fixed sequence of stages. Plugins can
insert custom generator stages at specified positions in this pipeline.

```
... → erosion → [your generator] → hydrology → ...
```

Your generator receives the current `TerrainWorld` state and can read/
modify terrain data according to your declared capabilities.

## Overlays

Overlays are schema-driven data layers attached to the hex grid. If
your plugin needs to store custom per-tile data (e.g., civilization
populations, resource deposits), you define an overlay schema and the
system handles persistence, indexing, and wire transport.

## Simulation DAG

After initial generation, plugins can participate in tick-based
simulation by registering `SimulationDef` nodes. Each tick, your
node reads terrain + declared dependency overlays and writes to
your owned overlay.

## Capabilities

Plugins declare capabilities in their manifest. The host enforces
these — a generator plugin cannot write overlays unless it declares
`writeOverlay`.

| Capability | Scope |
|------------|-------|
| `readTerrain` | Read terrain chunk data |
| `readOverlay` | Read dependency overlays |
| `writeOverlay` | Write to owned overlay |
| `writeTerrain` | Mutate terrain (writer sim nodes only) |
| `log` | Send log messages to host |

## Parameters

Plugins can define user-facing parameters (sliders, checkboxes) that
appear in topo-seer's UI. Values are persisted per-plugin.
