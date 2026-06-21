# Transport Layer

> **Module:** `Topo.Plugin.RPC.Transport` (216 LOC)
> **Status:** Stub

## Overview

Platform-agnostic transport abstraction for plugin communication. Production
launches create an endpoint on the host side and pass `TOPO_PLUGIN_ENDPOINT`
plus `TOPO_PLUGIN_ENDPOINT_KIND` to the plugin process. Stdio compatibility is
available only to explicit test/development harnesses via
`TOPO_PLUGIN_STDIO_COMPAT=1`.

## Backends

| Platform | Transport |
|----------|-----------|
| Windows | Named pipes |
| Linux/macOS | Unix domain sockets |

## Message Framing

All messages are length-prefixed (4-byte little-endian length header).
The transport handles framing; protocol logic operates on complete
message bytes.
