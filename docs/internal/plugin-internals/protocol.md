# RPC Protocol

> **Module:** `Topo.Plugin.RPC.Protocol` (357 LOC)
> **Status:** Stub

## Overview

JSON-encoded, length-prefixed message protocol between host and plugins.

## Message Direction

### Host → Plugin

| Type | Purpose |
|------|---------|
| `invoke_generator` | Run the generator callback |
| `invoke_simulation` | Run a simulation tick |
| `shutdown` | Clean shutdown request |

### Plugin → Host

| Type | Purpose |
|------|---------|
| `progress` | Progress update (0–100%) |
| `log` | Log message |
| `generator_result` | Generator output |
| `simulation_result` | Simulation tick output |
| `error` | Error report |

## Wire Format

```
┌──────────────┬──────────────────┐
│ length (4 B) │ payload (N bytes)│
└──────────────┴──────────────────┘
```

Payload is a JSON envelope: `{ "type": "...", "payload": { ... } }`
