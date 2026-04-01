# Overlay Indexing

> **Modules:** `Topo.Overlay.Index` (190 LOC), `Topo.Overlay.Indexed` (264 LOC)
> **Status:** Stub

## Overview

Secondary field indices for efficient overlay queries. Indices are
built on demand from overlay data, not persisted.

## Query Types

| Query | Description |
|-------|-------------|
| Int lookup | Tiles with a specific int field value |
| Float range | Tiles with float field in [low, high] |
| Bool filter | All true-tiles or false-tiles |

## Indexed Overlay

`Topo.Overlay.Indexed` wraps an overlay with a dirty-flag rebuild
strategy. Mutations mark the index dirty; queries rebuild if needed.
