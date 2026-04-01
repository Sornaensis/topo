# Overlay Provenance

> **Module:** `Topo.Overlay.Provenance` (29 LOC)
> **Status:** Stub

## Overview

Per-overlay provenance metadata persisted in the `.topolay` binary header.

## Fields

| Field | Type | Description |
|-------|------|-------------|
| `opSeed` | `Word64` | Base world seed |
| `opVersion` | `Word32` | Monotonic overlay version counter |
| `opSource` | `Text` | Producer identifier (plugin name or `"builtin"`) |

## Seed Derivation

The per-tick derived seed is not stored. Only the base seed is recorded.
Canonical derivation: `deriveOverlaySeed(worldSeed, tick)`.
