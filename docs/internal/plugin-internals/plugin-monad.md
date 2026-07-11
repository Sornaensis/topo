# Plugin Monad

> **Module:** `Topo.Plugin` (222 LOC)
> **Status:** Stub

## Overview

Core plugin monad `PluginM` — a capability-gated execution context
for plugin code running inside the host process.

## Monad Stack

```haskell
PluginM = ReaderT PluginEnv (StateT PluginState (ExceptT PluginError IO))
```

## Capabilities

Operations are gated by declared capabilities. Generator terrain output is
implicit in `generator` participation, `readTerrain`/`readWorld` controls terrain
input delivery, `writeTerrain`/`writeWorld` selects simulation terrain writers,
and generator or simulation overlay output requires `writeOverlay` or
`writeWorld`.

| Capability | Operations |
|------------|-----------|
| `readTerrain` | Read terrain chunk data |
| `readOverlay` | Read declared dependency overlays |
| `writeOverlay` | Write to the owned overlay |
| `writeTerrain` | Mutate terrain (sim writer only) |
| `log` | Send log messages to host |

## TopoM

`TopoM` — host-side counterpart for running plugin-originated
operations within the host's context.
