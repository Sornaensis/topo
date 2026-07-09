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
Includes provenance for all overlays. `meta.json` also records
`weather_layers` entries that describe each persisted climate/weather basis:
core generated climate averages, the current `weather` overlay when present, and
the generated typical `weather_normals` overlay when present.

## Load

1. Read `world.topo`
2. Validate overlay manifest against sidecar directory
3. Load `.topolay` payloads with schema validation

Topo-seer `save_world` / `load_world` and their HTTP routes (`POST /worlds/save`, `POST /worlds/load`) return the bundle format list and diagnostics on success. Failures include the save/load context in the service error message so UI dialogs and HTTP clients can report which persisted format failed.

Current weather is persisted in the `weather` overlay sidecar. When runtime
snapshots carry current `tsWeatherChunks` but no sidecar overlay has been
materialized yet, save conversion creates the dense `weather` overlay so the
current simulated state survives load. Generated typical normals remain in the
`weather_normals` overlay. Both overlays carry overlay provenance, and the save
manifest declares their basis/source metadata (`climate_average`,
`weather_snapshot`, or `weather_normals`). Layered view selection is UI-only:
load does not restore the previously selected base/sky overlay or average/current
weather basis, so the UI starts from its default elevation base with no sky
overlay and current weather basis.

## Load Policies

| Policy | Behaviour |
|--------|-----------|
| `StrictManifest` | Missing overlay → error |
| `BestEffort` | Missing overlay → warning, continue |

## See Also

- [World Bundle Format Spec](../../specs/world-bundle-format.md)
- [World Storage](world-storage.md)
- [Overlay Storage](../overlay/storage.md)
