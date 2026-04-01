# World Bundle Format

> Migrated from the original `docs/world-bundle-format.md`.
> Canonical reference for the on-disk world-bundle persistence contract.

## Overview

A saved world is a directory containing one core terrain file plus a
sidecar overlay directory:

```text
<world-dir>/
  world.topo
  world.topolay/
    <name>.toposchema
    <name>.topolay
```

- `world.topo` stores core terrain/world state and the authoritative
  `twOverlayManifest` list.
- `world.topolay/` stores overlay schemas (`.toposchema`) and overlay
  payloads (`.topolay`).

Loading a world in strict mode requires every manifest overlay name to
have both sidecar files.

## Core Terrain File (`.topo`)

See [.topo Binary Format](topo-binary-format.md).

## Overlay Sidecar Directory (`world.topolay/`)

For each overlay `name`:
- Schema: `world.topolay/<name>.toposchema`
- Payload: `world.topolay/<name>.topolay`

The manifest in `world.topo` is authoritative for expected overlays.

## Unified Save/Load Contract

`Topo.Persistence.WorldBundle` is the canonical persistence API.

- **Save:** writes to `<world-dir>.saving/`, then commits by rename.
- **Load:** reads `world.topo`, validates/loads sidecar overlays
  against manifest policy.

### Load Policies

| Policy | Behaviour |
|--------|-----------|
| `StrictManifest` | Missing overlay → error |
| `BestEffort` | Missing overlay → warning, continue |

## Provenance

Each overlay includes per-overlay provenance in `.topolay`:

| Field | Type | Description |
|-------|------|-------------|
| `opSeed` | `Word64` | Base world seed |
| `opVersion` | `Word32` | Monotonic overlay version |
| `opSource` | `Text` | Producer identifier |

## Compatibility Notes

- Overlay payload extension is `.topolay` (not `.dat`).
- Header flags bit 0 indicates chunk index table presence.
- Header flags bit 1 indicates zstd-compressed chunk payload entries.
- Uncompressed files (`flags = 0x00`) remain readable.

