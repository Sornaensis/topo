# .topo Binary Format

> **Status:** Stub
> **Module:** `Topo.Storage`
> **Current version:** 20

## Overview

The `.topo` file is the core terrain binary. It stores all per-chunk
data layers, world metadata, provenance, unit scales, simulation time,
seed, and the overlay manifest.

## File Structure

<!-- TODO: Document exact binary layout from Topo.Storage -->

```
┌──────────────────────────┐
│ Magic bytes / header     │
│ File version (Word32LE)  │
│ World metadata           │
│ Provenance               │
│ Unit scales              │
│ Simulation time          │
│ World seed               │
│ Overlay manifest names   │
│ Chunk maps:              │
│   ├─ Terrain chunks      │
│   ├─ Climate chunks      │
│   ├─ Weather chunks      │
│   ├─ River chunks        │
│   ├─ Volcanism chunks    │
│   ├─ Glacier chunks      │
│   ├─ Water body chunks   │
│   ├─ Vegetation chunks   │
│   └─ Groundwater chunks  │
└──────────────────────────┘
```

## Version History

<!-- TODO: Document major version changes -->

| Version | Changes |
|---------|---------|
| 20 | Current |
| 17+ | Added overlay manifest |
| ... | ... |

## Chunk Encoding

Individual chunks are encoded/decoded by `Topo.Export`. See
[Chunk Codecs](../internal/persistence/export.md).
