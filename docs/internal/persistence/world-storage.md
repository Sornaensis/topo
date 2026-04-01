# World Storage

> **Module:** `Topo.Storage` (746 LOC)
> **Status:** Stub

## Overview

Core `.topo` world file I/O. Binary encode/decode with versioned
format and migration support.

## Current Version

Binary file version: **20**

## Contents

The `.topo` file stores:
- Terrain chunk maps (all data layers)
- World metadata and provenance
- Unit scales
- Simulation time
- World seed
- Overlay manifest names (list of expected overlay names)

## Key Functions

| Function | Purpose |
|----------|---------|
| `saveWorld` | Encode and write world to file |
| `loadWorld` | Read and decode world from file |

## Version History

The module contains decoders for historical versions with migration
paths. See source comments for version-by-version changelog.

## Note

Overlay payloads are NOT stored in `.topo` — only the manifest of
expected overlay names. Overlay data lives in the `.topolay/` sidecar
directory (see [World Bundle](world-bundle.md)).
