# Key Concepts

> **Status:** Stub

## The Generation Pipeline

Topo generates terrain in a fixed sequence of stages. Plugins can
insert custom generator stages at specified positions in this pipeline.

```
... → erosion → [your generator] → hydrology → ...
```

Your generator receives the current `TerrainWorld` state only when it declares
`readTerrain` or `readWorld`. Its returned terrain is merged as part of
`generator` participation; overlay output still requires an owned overlay and
`writeOverlay` or `writeWorld`.

## Overlays

Overlays are schema-driven data layers attached to the hex grid. If
your plugin needs to store custom per-tile data (e.g., civilization
populations, resource deposits), you define an overlay schema and the
system handles persistence, indexing, and wire transport.

## Simulation DAG

After initial generation, plugins can participate in tick-based
simulation by declaring a plugin simulation node. The host-owned
simulation DAG always includes built-in nodes such as `weather`; those
built-ins are not plugin declarations. A plugin `simulation.dependencies`
list names simulation node IDs, so it may depend on host built-ins like
`weather` or on other eligible plugin simulation declarations. Each tick,
terrain input, dependency overlays, owned-overlay input, overlay output, and
terrain writes are all scoped by the node's declared capabilities.

## Capabilities

Plugins declare capabilities in their manifest. The host enforces these.
Generator terrain output is implicit in `generator` participation, but a
generator plugin cannot write overlays unless it declares `writeOverlay` (or
`writeWorld`). `readTerrain`/`readWorld` controls terrain input delivery, while
`writeTerrain`/`writeWorld` selects simulation terrain-writer nodes.

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
