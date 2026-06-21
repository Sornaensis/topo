# RPC Protocol Reference

> **Status:** Stub — for advanced / non-Haskell plugin developers

## Overview

If you're building a plugin in a language other than Haskell (i.e.,
not using `topo-plugin-sdk`), you need to implement the RPC protocol
directly.

## Transport

Production launches use a host-created endpoint advertised through environment
variables:

- `TOPO_PLUGIN_ENDPOINT` — endpoint address created by topo-seer.
- `TOPO_PLUGIN_ENDPOINT_KIND` — `named-pipe` on Windows, `unix` on Linux/macOS.

Do not assume a fixed endpoint name; the host may include unique suffixes for
concurrent plugin startup. Stdio is reserved for explicit test/development
compatibility harnesses, not production launch.

## Message Framing

```
┌──────────────────┬──────────────────┐
│ length (4B LE)   │ payload (N bytes)│
└──────────────────┴──────────────────┘
```

## Envelope Format

```json
{ "type": "<message-type>", "payload": { ... } }
```

## Message Types

### Host → Plugin

<!-- TODO: Full message schemas -->

| Type | Payload |
|------|---------|
| `invoke_generator` | World state, parameters, seed |
| `invoke_simulation` | World state, overlay data, tick |
| `shutdown` | (empty) |

### Plugin → Host

| Type | Payload |
|------|---------|
| `progress` | Percentage (0–100) |
| `log` | Message text |
| `generator_result` | Modified terrain data |
| `simulation_result` | Modified overlay data |
| `error` | Error description |

## Terrain Data Encoding

<!-- TODO: Document base64 chunk encoding -->

## See Also

- [Internal protocol docs](../internal/plugin-internals/protocol.md)
