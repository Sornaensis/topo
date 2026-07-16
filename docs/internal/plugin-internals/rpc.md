# RPC Client Internals

> **Module:** `Topo.Plugin.RPC`

`Topo.Plugin.RPC` is the host-side facade over the plugin wire protocol. It
combines manifest data, an open transport, request-correlation state, payload
codecs, and integration helpers that turn a connected plugin into pipeline and
simulation nodes.

## Responsibilities

- Own `RPCConnection`, the host's active session state for one plugin process.
- Select the highest protocol-4/5 manifest overlap before launch, verify its
  handshake echo and launch auth, and maintain negotiated data/stream metadata.
- Send correlated generator, simulation, health, heartbeat, data-service, and
  external data-source status requests.
- Send one-way shutdown, world-change, external grant, and revocation messages.
- Convert RPC generator callbacks into `PipelineStage` values.
- Convert RPC simulation callbacks into `SimNodeReader` or `SimNodeWriter`
  values based on manifest capabilities.
- Keep terrain, overlay, and external data-source payload handling
  backend-neutral and capability-scoped.

`Topo.Plugin.RPC.Protocol`, `DataService`, `ExternalDataSource`, `Payload`, and
`Transport` own the lower-level contracts. This module orchestrates those
contracts; it should not duplicate manifest validation, dependency resolution,
process supervision, or backend-specific external data-source behavior.

## Connection lifecycle

A normal host session follows this order:

1. Parse and validate manifest v3 with `Topo.Plugin.RPC.Manifest`.
2. Launch the plugin process with a host-created endpoint from
   `Topo.Plugin.RPC.Transport`.
3. Accept/connect the transport.
4. Build `RPCConnection` with `newRPCConnection` (default limits) or
   `newRPCConnectionWithLimits` (explicit embedded limits).
5. Call `performHandshake` before using data resources or runtime callbacks.
6. Insert `rpcGeneratorStage` and/or `rpcSimNode` into the pipeline/simulation
   integration points.
7. Use `rpcShutdown` and `closeTransport` during orderly shutdown.

`newRPCConnection` starts with the highest manifest/host protocol overlap, no runtime resources,
default request timeout from `rmStartPolicy`, the default 64 MiB/48 MiB payload
limits, and an empty runtime-failure slot. `newRPCConnectionWithLimits` preserves
the same behavior while allowing embedded hosts to use an explicitly constructed
`RPCPayloadLimits`. Production sessions receive the host-selected frame limit
from the process launcher.
`performHandshake` updates `rpcProtocolVersion`, `rpcStreamV1`,
`rpcDataDirectory`, and `rpcResources` after a successful `handshake_ack`.
Protocol 4 leaves `rpcStreamV1` absent; protocol 5 requires a bounded
`stream_v1` offer and stores the narrowed intersection.

## `RPCConnection` fields

| Field | Purpose |
| --- | --- |
| `rpcManifest` | Parsed manifest used for names, capabilities, timeouts, and integration decisions. |
| `rpcTransport` | Connected transport handle. |
| `rpcParams` | Current plugin parameter map sent on generator/simulation calls. |
| `rpcPayloadLimits` | Symmetric frame and decoded terrain budgets used by every send, receive, and terrain decode. |
| `rpcProtocolVersion` | Highest manifest/host overlap (4 or 5), echoed by handshake. |
| `rpcStreamV1` | Narrowed stream limits/features for v5; absent for v4. |
| `rpcDataDirectory` | Resolved plugin data directory returned by handshake. |
| `rpcResources` | Data-resource schemas returned by handshake. |
| `rpcRequestTimeoutMicros` | Request timeout derived from `startPolicy.request_timeout_ms`. |
| `rpcSession` | Shared write lock, pending request map, next request ID, and receive-loop flag. |
| `rpcRuntimeFailure` | First unobserved runtime transport/protocol failure for supervisor handling. |
| `rpcNextHostStreamId` | Monotonic, non-wrapping odd stream ID allocator for protocol-v5 host snapshot streams. |
| `rpcHighestPluginStreamId` | Connection-wide high-water mark preventing reuse of even plugin stream IDs. |
| `rpcStreamInvocationLock` | Serializes streamed invocations so connection-wide quotas cannot be multiplied; ordinary RPC remains concurrent. |
| `rpcReceiveFrameLimit` | Shared receive bound narrowed after the v5 handshake and observed by the long-lived receiver. |

## Request correlation

`rpcCall` and `rpcCallWithProgress` allocate a host `id`, register an
`RPCPending`, send the envelope, then wait for the correlated final response.
The receive loop routes `progress` and `log` as interim callback messages. Any
other correlated message completes the request; `error` becomes an `RPCError`.
Timeout, transport failure, or decode failure removes the pending request and
records a runtime failure.

Uncorrelated one-way transport writes (`sendOneWay`) are used for `shutdown`,
`world_changed`, `external_data_source_grant`, and `external_data_source_revoke`.
Grant/revoke payloads now carry stable operation identifiers so future broker
state can correlate `external_data_source_operation_result` ACK/result messages
without treating a transport write as consumer-applied state.

## Public operation groups

| Function(s) | Message(s) | Notes |
| --- | --- | --- |
| `performHandshake` / `performHandshakeWithAuth` | `handshake` → `handshake_ack` | Uses startup timeout, validates plugin protocol version, and verifies launch session/proof when challenged. |
| `sendWorldChanged` | `world_changed` | One-way notification after save/load/unload changes. |
| `sendHeartbeat` | `heartbeat` → `heartbeat` | Liveness probe with timeout. |
| `checkHealth` | `health_check` → `health_status` | Runtime health probe. |
| `invokeGenerator` | `invoke_generator` → `generator_result` | Sends capability-scoped terrain input and current params; accepts interim progress/log. |
| `invokeSimulation` | `invoke_simulation` → `simulation_result` | Sends capability-scoped terrain/overlay payloads; accepts interim progress/log. |
| `queryResource` | `query_resource` → `query_result` | Host query into plugin-owned data resource. |
| `mutateResource` | `mutate_resource` → `mutate_result` | Host mutation into plugin-owned data resource. |
| `sendExternalDataSourceGrant` | `external_data_source_grant` | Backend-neutral grant notification carrying `operationId`/optional epoch. |
| `sendExternalDataSourceGrantRevocation` / `revokeExternalDataSourceGrant` | `external_data_source_revoke` | Backend-neutral revocation notification carrying `operationId`/optional epoch. |
| `requestExternalDataSourceStatus` / `checkExternalDataSourceStatus` | `external_data_source_status_request` → `external_data_source_status` | Backend-neutral source/grant/ref status probe. |
| Future ACK handling | `external_data_source_operation_result` | Plugin result payload with `operationId`, optional `operationEpoch`, provider/consumer/source/grant, `accepted`, `applied`, `status`, `message`, `error`, and `diagnostics`. |
| `rpcShutdown` | `shutdown` | One-way clean shutdown request. |

## Pipeline integration

For negotiated protocol 5, generator and simulation integrations register a
stream-aware pending request. The ordinary receive loop continues to route
progress, logs, health/control replies, and unrelated correlated requests while
stream frames are delivered to that request's adapter. Terrain snapshots are
encoded directly from canonical section/chunk order under receiver byte credit;
the parent envelope contains only the exact scope ID, immutable geometry header,
and odd host stream IDs, not an inline base64 terrain object. Plugin-owned even
`terrain_delta` streams are validated and written to bounded host temp spools.
Credit is replenished only after a record is durably staged. Complete files are
decoded against the baseline geometry after all stream totals/digests and the
parent result succeed. Temp files are removed on success, timeout, cancellation,
disconnect, malformed data, or supervisor transport closure.

`rpcGeneratorStage` wraps a manifest generator as a `PipelineStage`:

- `stageId = StagePlugin rmName`;
- `stageName = rmName`;
- `stageSeedTag = "plugin:" <> rmName`;
- `stageOverlayProduces = Just rmName` when the manifest owns an overlay;
- `stageRun` sends `invoke_generator` with current terrain input only when the
  manifest has `readTerrain` or `readWorld`; otherwise the terrain input is
  `null`.
- Protocol-4 `generator_result.terrain` is merged with
  `applyGeneratorTerrainValue`. Protocol 5 requires terrain output through the
  referenced `terrain_delta`; the fully decoded patch is applied to a copied
  baseline and the world is published once. Generator terrain output remains
  implicit in `generator` participation and does not require `writeTerrain` or
  `writeWorld`.
- Optional generator overlay payloads are applied only when the manifest owns an
  overlay and has `writeOverlay` or `writeWorld`.

Generator payload delivery is capability-scoped by the manifest/SDK contract:
plugins should request only the input/overlay capabilities their callbacks
require, and returned payloads are merged through typed terrain/overlay decoders
rather than through ad-hoc JSON mutation.

## Simulation integration

`rpcSimNode` wraps a manifest simulation as either:

- `SimNodeReader` when the manifest does not declare terrain writes; or
- `SimNodeWriter` when `manifestWritesTerrain` sees `writeTerrain` or
  `writeWorld`.

`writeTerrain`/`writeWorld` selects simulation terrain-writer nodes only; it is
not required for generator terrain output. The simulation payload policy is
derived from manifest capabilities:

| Capability condition | Payload behavior |
| --- | --- |
| `readTerrain` or `readWorld` | Include `terrain`; otherwise send `null`. |
| `readOverlay` or `readWorld` | Include dependency `overlays`; otherwise send `{}`. |
| `readOverlay`, `writeOverlay`, or `writeWorld` | Include `own_overlay`; otherwise send `null`. |
| `writeOverlay` or `writeWorld` | Required before applying returned overlay updates. |
| `writeTerrain` or `writeWorld` | Creates a terrain-writer simulation node and decodes `terrain_writes`. |

Both reader and writer paths decode returned overlay JSON against the
plugin-owned overlay schema. Protocol-4 writer paths decode `terrain_writes`
through the connection-limited decoder. Protocol-5 writers instead validate the
complete `terrain_delta` spool and return its replacements together with the
validated overlay; streamed simulation removals are rejected because the
established `TerrainWrites` merge type cannot represent deletion. Terrain decoding checks
chunk dimensions and arithmetic, map cardinality, aggregate decoded bytes,
base64 lengths/padding, exact section lengths (including river segment data),
and trailing bytes before allocating chunk vectors.

## Error mapping

`RPCError` separates transport, protocol, plugin, data-resource, and timeout
failures:

| Constructor | Source |
| --- | --- |
| `RPCTransportError` | `TransportError` from send/receive/close/framing. |
| `RPCProtocolError` | Unexpected message type or payload decode failure. |
| `RPCPluginError` | Plugin `error` envelope with numeric code and message. |
| `RPCDataResourceError` | Standardized plugin data-resource failure. |
| `RPCTimeout` | Request/handshake/health/status timeout. |

`rpcErrorText` renders failures for pipeline, simulation, and supervisor
surfaces. Data-resource errors render through `DataResourceFailure` so service,
HTTP, command, and UI layers can share stable failure text.

## External data-source boundary

External data-source functions broker provider-owned coordination data only.
`RPCExternalDataSourceGrantMessage`, `RPCExternalDataSourceGrantRevocation`,
`RPCExternalDataSourceOperationResult`, `RPCExternalDataSourceStatusRequest`,
and `RPCExternalDataSourceStatusReport` carry provider IDs, sources, grants,
generic access/capability scopes, resources, backend-neutral status, opaque
`reference` objects, opaque `configRefs`, and opaque diagnostics. Grant/revoke
payloads add stable broker `operationId` values (and optional epochs); operation
results echo the ID and report `accepted`, `applied`, `status`, `message`, and
`error` without requiring the host to know backend internals.

This module must not add backend-specific migration tables, schema names,
connection-string parsing, lock protocols, consistency rules, cleanup routines,
or privileged storage assumptions. Provider plugins, adapters, users, or
deployment environments own those details; the host only routes, preserves, and
reports the neutral contract.
