# World Bundle

> **Module:** `Topo.Persistence.WorldBundle` (279 LOC)
> **Status:** Stub

## Overview

Unified persistence coordinator. Saves and loads the complete world
state: `.topo` terrain file + `.topolay/` overlay sidecar directory.

## On-Disk Layout

```
<world-dir>/
  world.topo              # core terrain binary
  world.topolay/          # overlay sidecar directory
    <name>.toposchema     # JSON overlay schema
    <name>.topolay         # binary overlay payload
```

## Save

Atomic write: writes to `<world-dir>.saving/`, then renames into place.
Includes provenance for all overlays.

## Load

1. Read `world.topo`
2. Validate overlay manifest against sidecar directory
3. Load `.topolay` payloads with schema validation

## Load Policies

| Policy | Behaviour |
|--------|-----------|
| `StrictManifest` | Missing overlay → error |
| `BestEffort` | Missing overlay → warning, continue |

## See Also

- [World Bundle Format Spec](../../specs/world-bundle-format.md)
- [World Storage](world-storage.md)
- [Overlay Storage](../overlay/storage.md)
