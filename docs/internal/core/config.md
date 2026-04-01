# Config Utilities

> **Modules:** `Topo.Config` (31 LOC), `Topo.Config.JSON` (123 LOC)
> **Status:** Stub

## Overview

Shared JSON serialization infrastructure used by every config type
in the library.

## Topo.Config

Deprecated compatibility shim. Re-exports from dedicated config modules.

## Topo.Config.JSON

| Function | Purpose |
|----------|---------|
| `configOptions` | Aeson options with prefix stripping |
| `mergeDefaults` | Forward-compatible JSON parsing (merge incoming JSON with defaults) |

Every config type uses `configOptions` for its `FromJSON`/`ToJSON`
instances. `mergeDefaults` enables loading configs from older versions
that may be missing new fields.
