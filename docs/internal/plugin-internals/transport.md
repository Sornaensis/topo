# Transport Layer

> **Module:** `Topo.Plugin.RPC.Transport` (216 LOC)
> **Status:** Stub

## Overview

Platform-agnostic transport abstraction for plugin communication.

## Backends

| Platform | Transport |
|----------|-----------|
| Windows | Named pipes |
| Linux/macOS | Unix domain sockets |

## Message Framing

All messages are length-prefixed (4-byte little-endian length header).
The transport handles framing; protocol logic operates on complete
message bytes.
