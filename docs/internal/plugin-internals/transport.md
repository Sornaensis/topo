# Transport Layer Internals

> **Module:** `Topo.Plugin.RPC.Transport`

The transport layer owns endpoint creation, plugin-side connection, and binary
message framing for plugin RPC. Protocol modules operate on complete JSON
envelope bytes; transport code is responsible for platform endpoints, timeouts,
frame length encoding, frame-size limits, and close/error reporting.

## Production backends

| Platform | Endpoint kind | Notes |
| --- | --- | --- |
| Windows | `named-pipe` | Host-created `\\.\pipe\...` endpoint. |
| Linux/macOS | `unix` | Host-created Unix domain socket with owner-only permissions. |

The host creates the endpoint, launches the plugin, and passes the endpoint
through environment variables. Plugins connect to that endpoint; they must not
assume a stable global pipe/socket name. Stdio compatibility is only available
when `TOPO_PLUGIN_STDIO_COMPAT=1` is explicitly set for tests or development
harnesses, and production launches strip that variable.

Endpoint uniqueness is not the authentication boundary. Unix sockets live under
an owner-only temporary directory and Windows named pipe names are unique per
launch, but protocol v4 still verifies launch session/token proof during the RPC
handshake as defense-in-depth against same-host endpoint races or
accidental/malicious local clients.

## Launch environment

| Variable | Constant | Purpose |
| --- | --- | --- |
| `TOPO_PLUGIN_ID` | `pluginIdEnv` | Host-assigned plugin identifier. |
| `TOPO_PLUGIN_PROTOCOL` | `pluginProtocolEnv` | Expected RPC protocol version. |
| `TOPO_PLUGIN_ENDPOINT` | `pluginEndpointEnv` | Host-created endpoint address. |
| `TOPO_PLUGIN_ENDPOINT_KIND` | `pluginEndpointKindEnv` | `named-pipe` or `unix`. |
| `TOPO_PLUGIN_STDIO_COMPAT` | `pluginStdioCompatibilityEnv` | Explicit stdio test/development opt-in. |
| `TOPO_PLUGIN_SESSION` | `pluginSessionEnv` | Opaque launch session ID. |
| `TOPO_PLUGIN_AUTH_TOKEN` | `pluginAuthTokenEnv` | Opaque launch token. |
| `TOPO_PLUGIN_WORLD_ID` | `pluginWorldIdEnv` | Active world identifier or sentinel; advisory launch metadata for plugin authors/diagnostics, not a confinement boundary. |
| `TOPO_PLUGIN_DATA_ROOT` | `pluginDataRootEnv` | Writable plugin data root selected and created by the host; advisory metadata and save-bundling source, not a sandbox or confinement boundary. |

`endpointKindText` emits `unix` or `named-pipe`; `parseEndpointKind` also accepts
legacy spellings such as `unix_socket`, `named_pipe`, and `pipe`.

## Frame format

All protocol messages are framed as:

```text
┌──────────────┬──────────────────┐
│ length (4 B) │ payload (N bytes)│
└──────────────┴──────────────────┘
```

The length prefix is a little-endian `Word32` containing the byte count of the
payload that follows. The payload is a UTF-8 JSON `RPCEnvelope` encoded by
`Topo.Plugin.RPC.Protocol.encodeMessage`.

`sendMessage`/`recvMessage` use the default maximum frame size;
`sendMessageWithLimit`/`recvMessageWithLimit` allow tests and callers to enforce
custom limits. Oversized, truncated, malformed, unsupported, or closed transports
return `TransportError` values instead of protocol-level `RPCError`s.

## Key types and functions

| Name | Purpose |
| --- | --- |
| `Transport` | Read/write handles plus plugin name for diagnostics. |
| `TransportConfig` | Pipe/socket directory and connection timeout. |
| `TransportEndpointKind` | `TransportEndpointUnixSocket` or `TransportEndpointNamedPipe`. |
| `TransportEndpoint` | Kind plus address. |
| `TransportServer` | Host-side endpoint plus accept/close actions. |
| `openPluginServer` | Allocate a host endpoint for one plugin connection. |
| `connectPluginEndpoint` | Plugin-side connect to a supplied endpoint. |
| `connectPluginFromEnvironment` | Plugin-side connect using launch environment. |
| `closeTransport` | Close underlying handles/socket resources. |
| `pluginPipeName` | Escaped platform pipe/socket name helper. |

## Error boundary

`TransportError` describes connection, send, receive, framing, unsupported, and
closed-handle failures. `Topo.Plugin.RPC` wraps these as `RPCTransportError` and
records runtime failures for supervisor handling. Transport code should not
interpret manifest fields, dependency graph state, data-resource records, or
external data-source backend details; it only moves framed bytes across the
selected endpoint.
