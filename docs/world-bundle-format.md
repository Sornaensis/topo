# World Bundle Format

This document defines the on-disk world-bundle persistence contract used by Topo.

## Overview

A saved world is a directory containing one core terrain file plus a sidecar overlay directory:

```text
<world-dir>/
  world.topo
  world.topolay/
    <name>.toposchema
    <name>.topolay
```

- `world.topo` stores core terrain/world state and the authoritative `twOverlayManifest` list.
- `world.topolay/` stores overlay schemas (`.toposchema`) and overlay payloads (`.topolay`).

Loading a world in strict mode requires every manifest overlay name to have both sidecar files.

## Core Terrain File (`.topo`)

`world.topo` is encoded/decoded by `Topo.Storage`.

- Current binary file version: `20`
- Contains terrain/climate/hydrology/volcanism/glacier/waterbody/vegetation chunk maps, world metadata/provenance, unit scales, simulation time, seed, and overlay manifest names.
- Overlay payloads are not stored in `world.topo`; they are stored in `world.topolay/`.

For exact historical migration details and section ordering, see `Topo.Storage` module comments and version notes.

## Overlay Sidecar Directory (`world.topolay/`)

For each overlay `name`:

- schema: `world.topolay/<name>.toposchema`
- payload: `world.topolay/<name>.topolay`

The manifest in `world.topo` is authoritative for expected overlays.

## Overlay Schema File (`.toposchema`)

Schema files are UTF-8 JSON and parsed by `Topo.Overlay.Schema`.

Primary schema fields:

- `name` (text)
- `version` (text)
- `description` (text)
- `fields` (array of field descriptors)
- `storage` (`"dense"` or `"sparse"`)
- `dependencies`
  - `terrain` (bool)
  - `overlays` (array of overlay names)

Field descriptor:

- `name` (text)
- `type` (`int`, `float`, `bool`, `text`, `enum`)
- `default` (JSON value)
- `nullable` (bool)
- optional enum metadata for enum-typed fields

## Overlay Payload File (`.topolay`)

Overlay payload binaries are encoded/decoded by `Topo.Overlay.Storage`.

### Header

In order:

1. `Word32LE` overlay name hash (FNV-1a)
2. `Word32LE` schema-version byte length
3. schema-version bytes (UTF-8)
4. `Word8` storage mode (`0x00` sparse, `0x01` dense)
5. `Word32LE` field count
6. repeated field descriptors:
   - `Word32LE` field-name byte length
   - field-name bytes (UTF-8)
   - `Word8` field type tag
7. provenance block:
   - `Word64LE` `opSeed`
   - `Word32LE` `opVersion`
   - `Word32LE` `opSource` byte length
   - `opSource` bytes (UTF-8)
8. `Word8` flags
  - bit 0: chunk index table present
  - bit 1: zstd chunk compression
9. `Word32LE` chunk count

Unknown flags are rejected by readers.

### Chunk payloads (uncompressed)

For each chunk:

1. `Word32LE` chunk id
2. `Word32LE` payload byte length
3. payload bytes

### Chunk payloads (zstd compressed)

When header flags bit 1 is set, each chunk entry is:

1. `Word32LE` chunk id
2. `Word32LE` compressed payload byte length
3. `Word32LE` uncompressed payload byte length
4. zstd-compressed payload bytes

Readers must reject malformed compressed entries, including:

- missing `uncompressed payload byte length`
- decompressed size mismatch against stored uncompressed length

### Chunk index table

When header flags bit 0 is set, a chunk index table is appended after all
chunk payload entries:

1. `Word32LE` index entry count
2. repeated index entries:
  - `Word32LE` chunk id
  - `Word64LE` file offset to the chunk entry

Payload encoding depends on schema field order/types and storage mode.

## Provenance

Each overlay includes per-overlay provenance persisted in `.topolay`:

- `opSeed` (`Word64`): base world seed
- `opVersion` (`Word32`): monotonic overlay version
- `opSource` (`Text`): producer identifier (plugin/builtin)

Canonical per-tick seed derivation is:

```haskell
deriveOverlaySeed :: Word64 -> Word64 -> Word64
deriveOverlaySeed worldSeed tick =
  worldSeed + tick * 0x9E3779B97F4A7C15
```

## Unified Save/Load Contract

`Topo.Persistence.WorldBundle` is the canonical application-level persistence API.

- Save: writes all world-bundle files to `<world-dir>.saving`, then commits by rename/swap.
- Load: reads `world.topo`, validates/loads sidecar overlays against manifest policy.

`StrictManifest` fails when required overlays are missing or invalid.
`BestEffort` tolerates sidecar failures and keeps already-loaded core overlays.

## Compatibility Notes

- Overlay payload extension is `.topolay` (not `.dat`).
- Uncompressed files (`flags = 0x00`) remain readable.

