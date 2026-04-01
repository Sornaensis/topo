# RPC Protocol Reference

> **Status:** Stub — for advanced / non-Haskell plugin developers

## Overview

If you're building a plugin in a language other than Haskell (i.e.,
not using `topo-plugin-sdk`), you need to implement the RPC protocol
directly.

## Transport

- **Windows:** Named pipes at `\\.\pipe\topo-plugin-<name>`
- **Linux/macOS:** Unix domain sockets at `/tmp/topo-plugin-<name>.sock`

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
