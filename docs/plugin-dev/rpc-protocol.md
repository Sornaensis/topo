# RPC Protocol Reference

This page is for non-Haskell plugin authors and for SDK users who need to
inspect the wire contract. Haskell plugins should normally use
`topo-plugin-sdk`; the SDK implements this protocol, emits manifest v3, and
handles transport, framing, correlation, payload codecs, and default external
status reporting.

## Source of truth and drift checks

The compiled contract lives in these modules:

- `Topo.Plugin.RPC.Protocol` — envelope, message tags, handshake/liveness,
  generator/simulation payloads, progress/log/error payloads, and protocol
  version.
- `Topo.Plugin.RPC.DataService` — `query_resource`, `query_result`,
  `mutate_resource`, and `mutate_result` payloads.
- `Topo.Plugin.RPC.ExternalDataSource` — backend-neutral grant, revoke, status
  request, and status report payloads.
- `Topo.Plugin.RPC.Payload` — terrain/world payload JSON and base64 chunk
  helpers.
- `Topo.Plugin.RPC.Transport` — platform endpoint selection and length-prefixed
  frames.

`Spec.PluginDocs` contract-tests this reference against the compiled RPC
message tags, and `Spec.PluginRPC` tests JSON codecs, framing, transport limits,
manifest examples, data-service payloads, and external data-source status/grant
payloads.

## Protocol version

The current RPC protocol version is **4** (`currentProtocolVersion`). Manifest
v3 advertises compatible protocol bounds in `runtime.protocol.min` and
`runtime.protocol.max`; the host rejects plugins whose range does not include
protocol 4.

Protocol 4 includes the protocol 3 envelope, heartbeat, health, bounded-frame,
data-service, and external data-source message groups, plus required production
launch authentication. The host sends an `auth_challenge` nonce during the
startup handshake and rejects a plugin before it becomes ready unless the
`handshake_ack` proves the launch `session_id` and `TOPO_PLUGIN_AUTH_TOKEN`.
This is a major-version bump because protocol 3 plugins cannot prove they
received the host launch environment.

## Production transport

The host creates an endpoint, launches the plugin process, and passes endpoint
metadata in environment variables. Plugins connect to that host-created endpoint;
they must not invent a fixed pipe/socket name.

| Environment variable | Meaning |
| --- | --- |
| `TOPO_PLUGIN_ID` | Host-assigned plugin identifier. |
| `TOPO_PLUGIN_PROTOCOL` | Protocol version expected by the host. |
| `TOPO_PLUGIN_ENDPOINT` | Host-created endpoint address. |
| `TOPO_PLUGIN_ENDPOINT_KIND` | `named-pipe` on Windows, `unix` on Linux/macOS. |
| `TOPO_PLUGIN_SESSION` | Opaque launch/session identifier. |
| `TOPO_PLUGIN_AUTH_TOKEN` | Opaque launch token for the session. |
| `TOPO_PLUGIN_WORLD_ID` | Active world identifier or a host sentinel; host-provided metadata for plugin authors/diagnostics, not currently host-enforced behavior. |
| `TOPO_PLUGIN_DATA_ROOT` | Writable plugin data root selected and created by the host; metadata for plugin authors/diagnostics unless a future contract assigns host-enforced behavior. |

`TOPO_PLUGIN_STDIO_COMPAT=1` is only an explicit test/development compatibility
mode and is stripped from production plugin launches. Production launch uses
named pipes on Windows and Unix domain sockets on Unix-like systems.

### Local endpoint threat model

Unix socket endpoints are created under an owner-only temporary directory.
Windows named pipe names are unique per launch. Those endpoint names are still
not treated as authentication: a same-host process can race, guess, inherit, or
accidentally reuse local endpoint metadata. The protocol 4 session/token proof is
defense-in-depth against endpoint races and accidental or malicious local clients
that did not receive the host launch environment.

## Frame format

Every frame is a little-endian length prefix followed by a UTF-8 JSON payload:

```text
┌──────────────────┬──────────────────┐
│ length (4B LE)   │ payload (N bytes)│
└──────────────────┴──────────────────┘
```

The length is a `Word32` byte count for the JSON payload only. The transport
rejects truncated frames, oversized frames when a limit is supplied, and closed
or unsupported endpoints.

## Envelope format

Every payload is wrapped in an envelope:

```json
{
  "id": 42,
  "type": "invoke_generator",
  "payload": { "payload_version": 1 }
}
```

| Field | Required | Description |
| --- | --- | --- |
| `type` | Yes | One of the canonical message tags listed below. |
| `payload` | Yes | Message-specific JSON value, normally an object. |
| `id` | No | Host-assigned request correlation ID. Responses, interim progress, and logs for a request echo the same ID. |

The host treats `progress` and `log` as interim messages for the matching `id`.
Any other correlated message completes the request; `error` completes it as a
failure.

## Canonical message registry

These are the canonical tags emitted by the compiled `ToJSON RPCMessageType`
instance. The decoder also accepts a few older aliases for external data-source
messages, but new plugins should emit the canonical tags below.

| Tag | Direction | Payload type | Expected response |
| --- | --- | --- | --- |
| `handshake` | Host → Plugin | `Handshake` | `handshake_ack` or `error` |
| `handshake_ack` | Plugin → Host | `HandshakeAck` | Final handshake response |
| `world_changed` | Host → Plugin | `WorldChanged` | One-way notification |
| `invoke_generator` | Host → Plugin | `InvokeGenerator` | `generator_result` or `error`; may emit `progress`/`log` first |
| `invoke_simulation` | Host → Plugin | `InvokeSimulation` | `simulation_result` or `error`; may emit `progress`/`log` first |
| `shutdown` | Host → Plugin | Empty object | One-way shutdown request |
| `progress` | Plugin → Host | `PluginProgress` | Interim correlated update |
| `log` | Plugin → Host | `PluginLog` | Interim correlated update |
| `generator_result` | Plugin → Host | `GeneratorResult` | Final generator response |
| `simulation_result` | Plugin → Host | `SimulationResult` | Final simulation response |
| `error` | Plugin → Host | `PluginError` | Final failure response |
| `query_resource` | Host → Plugin | `QueryResource` | `query_result` or `error`; may emit `progress`/`log` first |
| `query_result` | Plugin → Host | `QueryResult` | Final data query response |
| `mutate_resource` | Host → Plugin | `MutateResource` | `mutate_result` or `error`; may emit `progress`/`log` first |
| `mutate_result` | Plugin → Host | `MutateResult` | Final data mutation response |
| `heartbeat` | Host ↔ Plugin | `Heartbeat` | `heartbeat` |
| `health_check` | Host → Plugin | Empty object | `health_status` or `error` |
| `health_status` | Plugin → Host | `HealthStatus` | Final health response |
| `external_data_source_grant` | Host → Plugin | `RPCExternalDataSourceGrantMessage` | Grant notification carrying an `operationId` |
| `external_data_source_revoke` | Host → Plugin | `RPCExternalDataSourceGrantRevocation` | Revocation notification carrying an `operationId` |
| `external_data_source_operation_result` | Plugin → Host | `RPCExternalDataSourceOperationResult` | ACK/result for grant or revoke application |
| `external_data_source_status_request` | Host → Plugin | `RPCExternalDataSourceStatusRequest` | `external_data_source_status` or `error` |
| `external_data_source_status` | Plugin → Host | `RPCExternalDataSourceStatusReport` | Final status response |

## Lifecycle payloads

### `handshake`

Sent by the host immediately after transport connection:

```json
{
  "protocol_version": 4,
  "world_path": "C:/worlds/demo",
  "host_capabilities": ["query", "mutate", "launch_auth"],
  "auth_challenge": "challenge-..."
}
```

`world_path` may be absent or `null` when no world is loaded. Host capabilities
are backend-neutral capabilities for host-brokered services, not storage engine
identifiers. Production launches include `launch_auth` and `auth_challenge`;
in-process tests or explicit stdio compatibility may omit them.

### `handshake_ack`

Returned by the plugin:

```json
{
  "protocol_version": 4,
  "data_directory": "civilization",
  "resources": [],
  "session_id": "session-...",
  "auth_proof": "hex-hmac-sha256"
}
```

The protocol version must equal 4. `data_directory` is relative to the world
save path. `resources` contains `DataResourceSchema` values also valid in
manifest v3 `dataResources`. When `auth_challenge` is present, `session_id` must
match `TOPO_PLUGIN_SESSION` and `auth_proof` must be
`HMAC-SHA256(TOPO_PLUGIN_AUTH_TOKEN, "topo-plugin-launch-auth-v1\n4\n" <> session_id <> "\n" <> auth_challenge)` encoded as lowercase hex. The auth token itself is never sent.

### `world_changed`

A one-way notification after the active world path changes:

```json
{ "world_path": "C:/worlds/next" }
```

Plugins that manage data directories should re-resolve their paths when this
arrives.

### `heartbeat`, `health_check`, and `health_status`

`heartbeat` carries a short status string, usually `"ping"` from the host and
`"ok"` from the plugin:

```json
{ "status": "ok" }
```

`health_check` uses an empty payload. `health_status` returns:

```json
{ "healthy": true, "message": "ready" }
```

## Generator and simulation payloads

### `invoke_generator`

```json
{
  "payload_version": 1,
  "stage_id": "plugin:civilization",
  "seed": 0,
  "config": { "growth_rate": 0.02 },
  "terrain": { "encoding": "base64", "terrain": {} }
}
```

| Field | Meaning |
| --- | --- |
| `payload_version` | Generator payload contract version; currently `1`. |
| `stage_id` | Canonical stage ID, normally `plugin:<manifest.name>`. |
| `seed` | Deterministic generation seed supplied by the host path. |
| `config` | Plugin parameter map keyed by manifest/SDK parameter name. |
| `terrain` | Terrain input payload from `terrainWorldToPayload` when the manifest has `readTerrain` or `readWorld`; otherwise `null`. |

### `generator_result`

```json
{
  "terrain": { "encoding": "base64", "terrain": {} },
  "overlay": null,
  "metadata": { "notes": "optional" }
}
```

`terrain` is applied back to the world with `applyGeneratorTerrainValue`.
Generator terrain output is implicit in `generator` participation and does not
require `writeTerrain` or `writeWorld`. `overlay` is optional and only
meaningful for plugins that own an overlay, and applying it requires
`writeOverlay` or `writeWorld`. `metadata` is optional opaque JSON.

### `invoke_simulation`

```json
{
  "payload_version": 1,
  "node_id": "civilization",
  "world_time": 120,
  "delta_ticks": 1,
  "calendar": { "year": 1, "dayOfYear": 12, "hourOfDay": 8 },
  "config": {},
  "terrain": null,
  "overlays": {},
  "own_overlay": null
}
```

| Field | Meaning |
| --- | --- |
| `payload_version` | Simulation payload contract version; currently `1`. |
| `node_id` | Simulation node ID, equal to the manifest plugin name. |
| `world_time` / `delta_ticks` | Tick counters from the simulation context. |
| `calendar` | Calendar date object with `year`, `dayOfYear`, and `hourOfDay`. |
| `config` | Plugin parameter map. |
| `terrain` | Terrain payload when the manifest has `readTerrain` or `readWorld`; otherwise `null`. |
| `overlays` | Dependency overlays when the manifest has `readOverlay` or `readWorld`; otherwise `{}`. |
| `own_overlay` | Current owned overlay when the manifest can read/write overlays; otherwise `null`. |

Simulation plugins that return overlay updates must declare `writeOverlay` (or
`writeWorld`). Plugins that return `terrain_writes` must also declare
`writeTerrain` (or `writeWorld`) so the host inserts them as terrain writers;
these write capabilities select simulation terrain writers and are not required
for generator terrain output.

### `simulation_result`

```json
{
  "overlay": {},
  "terrain_writes": null
}
```

`overlay` is decoded against the plugin-owned overlay schema. `terrain_writes`
is optional and uses the same base64 chunk sections as terrain payloads.

## Progress, logs, and errors

`progress` and `log` are interim messages and should echo the request `id`.

```json
{ "message": "classifying settlements", "fraction": 0.5 }
```

A `progress.fraction` is absolute progress for the current invocation,
conventionally `0.0` through `1.0` inclusive. Plugins are not required to emit
`0.0` or `1.0`, and the final result or `error` remains authoritative. SDK
callbacks sanitize before encoding by clamping finite values into `[0,1]` and
mapping `NaN`/infinities to finite defensive values so wire JSON never contains
invalid numbers.

```json
{ "level": "info", "message": "settlements loaded" }
```

Log levels are `debug`, `info`, `warn`, and `error`. Plugin errors are final
responses:

```json
{ "code": 1009, "message": "external data-source unavailable" }
```

Data-resource errors may also use standardized `error_code` values in
`mutate_result`; the host maps those to HTTP/service errors.

## Data-service payloads

Data resources are plugin-owned. The host sends queries and mutations only for
resources declared in the manifest or handshake.

### `query_resource`

```json
{
  "resource": "settlements",
  "query": { "type": "by_hex", "chunk": 4, "tile": 17 },
  "page_size": 50,
  "page_offset": 0
}
```

Supported query types are `all`, `by_key`, `by_hex`, and `by_field`.

### `query_result`

```json
{
  "resource": "settlements",
  "records": [{ "id": "s1", "population": 1200 }],
  "total_count": 1
}
```

Records are JSON objects. The plugin owns interpretation and validation beyond
the declared schema.

### `mutate_resource`

```json
{
  "resource": "settlements",
  "mutation": { "type": "update", "key": "s1", "record": { "population": 1300 } }
}
```

Supported mutation types are `create`, `update`, `delete`, and `set_hex`.

### `mutate_result`

```json
{
  "success": false,
  "error": "external data-source unavailable",
  "error_code": "external_data_source_unavailable"
}
```

`record` is present on successful create/update responses when the plugin wants
to return the stored value.

## Terrain payload encoding

The host sends capability-scoped terrain payloads with these stable keys:

- summary counts: `chunk_count`, `climate_count`, `river_count`,
  `vegetation_count`;
- world metadata: `chunk_size`, `hex_grid`, `planet`, `slice`;
- `encoding`, currently always `base64`;
- chunk maps: `terrain`, `climate`, and `vegetation`.

Chunk maps are objects keyed by chunk ID. Each value is a base64 string produced
from the binary `Topo.Export` chunk codec for that layer. Simulation terrain
writes use the same `encoding`, `chunk_size`, `terrain`, `climate`, and
`vegetation` shape and may omit layers that did not change.

## Backend-neutral external data-source payloads

External data-source messages coordinate provider-owned sources and grants. They
intentionally carry provider/source names, generic capabilities, status, opaque
references, and opaque config references; they do not define a database engine,
connection string format, migration table, lock protocol, writer policy, or host
schema authority. Plugin authors normally declare providers/consumers in the
manifest and let topo broker grants automatically after refresh/handshake. The
low-level grant/revoke RPC helpers expose a stable `operationId` and optional
`operationEpoch` so plugins can emit an `external_data_source_operation_result`;
host retry/state-machine behavior is layered above this protocol shape.

### `external_data_source_grant`

```json
{
  "operationId": "external-data-source:grant:trade-routes:settlements:civilization:settlement-ledger:settlement-read",
  "operationEpoch": 1,
  "providerId": "civilization",
  "consumerId": "trade-routes",
  "source": "settlement-ledger",
  "grant": "settlement-read",
  "access": ["read"],
  "resources": ["settlements"],
  "capabilityScope": ["query", "health"],
  "status": { "state": "ready", "providerId": "civilization" },
  "reference": { "handle": "grant:settlement-read" },
  "configRefs": [],
  "diagnostics": { "reportedBy": "host" }
}
```

### `external_data_source_revoke`

```json
{
  "operationId": "external-data-source:revoke:trade-routes:settlements:civilization:settlement-ledger:settlement-read",
  "operationEpoch": 2,
  "providerId": "civilization",
  "consumerId": "trade-routes",
  "source": "settlement-ledger",
  "grant": "settlement-read",
  "reason": "provider unavailable",
  "status": { "state": "unavailable", "providerId": "civilization" }
}
```

### `external_data_source_operation_result`

```json
{
  "operationId": "external-data-source:grant:trade-routes:settlements:civilization:settlement-ledger:settlement-read",
  "operationEpoch": 1,
  "operation": "grant",
  "providerId": "civilization",
  "consumerId": "trade-routes",
  "source": "settlement-ledger",
  "grant": "settlement-read",
  "accepted": true,
  "applied": true,
  "status": "applied",
  "message": "grant applied",
  "error": null,
  "diagnostics": { "reportedBy": "consumer" }
}
```

`operation` is `grant` or `revoke`. `accepted` means the consumer accepted the
operation for processing; `applied` means the grant/revoke took effect in the
consumer runtime. `status` is stable text such as `accepted`, `applied`,
`rejected`, or `failed`; `message`, `error`, and `diagnostics` are optional.
Older grant/revoke payloads without `operationId` remain accepted for decoding,
but new brokered payloads should include it so ACK/result messages can be
correlated without relying on transport send success.

### `external_data_source_status_request`

```json
{
  "providerId": "civilization",
  "sources": ["settlement-ledger"],
  "grants": ["settlement-read"],
  "includeDiagnostics": true
}
```

Empty `sources` and `grants` ask the plugin to report every declared provider
source, grant, and consumer reference.

### `external_data_source_status`

```json
{
  "statuses": [
    {
      "providerId": "civilization",
      "source": "settlement-ledger",
      "grant": "settlement-read",
      "access": ["read"],
      "resources": ["settlements"],
      "capabilityScope": ["query", "health"],
      "status": { "state": "ready", "availability": "available" },
      "reference": { "handle": "grant:settlement-read" },
      "configRefs": []
    }
  ]
}
```

Status states are `unknown`, `unconfigured`, `ready`, `degraded`, and
`unavailable`. Only `ready` is brokerable for provider/grant routing; other
states are surfaced as diagnostics until the provider reports readiness again.
A transport-level send only means topo wrote the grant/revoke envelope to the
consumer transport. Consumer acceptance/application is represented separately by
`external_data_source_operation_result` and the matching `operationId`.

## See also

- [Manifest v3](manifest.md)
- [Plugin examples](examples.md)
- [Internal protocol notes](../internal/plugin-internals/protocol.md)
- [Internal RPC client notes](../internal/plugin-internals/rpc.md)
