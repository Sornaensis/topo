# RPC Protocol Internals

> **Module:** `Topo.Plugin.RPC.Protocol`
> **Companion modules:** `Topo.Plugin.RPC.DataService`,
> `Topo.Plugin.RPC.ExternalDataSource`, `Topo.Plugin.RPC.Payload`,
> `Topo.Plugin.RPC.Transport`

This document records the compiled host↔plugin RPC contract. It is not a
separate specification: the source of truth is the Haskell types and JSON
instances listed above. `Spec.PluginDocs` checks that this page names every
canonical `RPCMessageType` tag, while `Spec.PluginRPC` exercises the codecs,
framing, data-service payloads, manifest examples, and external data-source
payloads.

## Version and envelope

`currentProtocolVersion` is **4**. The corresponding manifest contract is
manifest v3, whose `runtime.protocol.min`/`max` range must include `4`.
Protocol 4 is a major-version bump for production launch authentication: the
host sends an `auth_challenge` nonce during startup and requires the plugin to
return the launch `session_id` and an HMAC-SHA256 `auth_proof` before the
supervisor can mark the plugin ready.

Every frame payload is an `RPCEnvelope` JSON object:

```json
{ "id": 1, "type": "invoke_generator", "payload": { "payload_version": 1 } }
```

| JSON field | Haskell field | Notes |
| --- | --- | --- |
| `type` | `envType :: RPCMessageType` | Required discriminator. |
| `payload` | `envPayload :: Value` | Required message-specific payload. |
| `id` | `envRequestId :: Maybe Word64` | Optional correlation ID assigned by the host. |

`encodeMessage` and `decodeMessage` only encode/decode the JSON envelope. The
transport module adds/removes the 4-byte little-endian frame length prefix.

## Canonical message tags

The `ToJSON RPCMessageType` instance emits these tags. The decoder accepts a few
legacy aliases for external data-source messages, but all host/SDK code should
emit the canonical tags below.

| Constructor | JSON tag | Direction | Payload type |
| --- | --- | --- | --- |
| `MsgInvokeGenerator` | `invoke_generator` | Host → Plugin | `InvokeGenerator` |
| `MsgInvokeSimulation` | `invoke_simulation` | Host → Plugin | `InvokeSimulation` |
| `MsgShutdown` | `shutdown` | Host → Plugin | Empty object |
| `MsgProgress` | `progress` | Plugin → Host | `PluginProgress` |
| `MsgLog` | `log` | Plugin → Host | `PluginLog` |
| `MsgGeneratorResult` | `generator_result` | Plugin → Host | `GeneratorResult` |
| `MsgSimulationResult` | `simulation_result` | Plugin → Host | `SimulationResult` |
| `MsgError` | `error` | Plugin → Host | `PluginError` |
| `MsgHandshake` | `handshake` | Host → Plugin | `Handshake` |
| `MsgHandshakeAck` | `handshake_ack` | Plugin → Host | `HandshakeAck` |
| `MsgWorldChanged` | `world_changed` | Host → Plugin | `WorldChanged` |
| `MsgQueryResource` | `query_resource` | Host → Plugin | `QueryResource` |
| `MsgQueryResult` | `query_result` | Plugin → Host | `QueryResult` |
| `MsgMutateResource` | `mutate_resource` | Host → Plugin | `MutateResource` |
| `MsgMutateResult` | `mutate_result` | Plugin → Host | `MutateResult` |
| `MsgHeartbeat` | `heartbeat` | Host ↔ Plugin | `Heartbeat` |
| `MsgHealthCheck` | `health_check` | Host → Plugin | Empty object |
| `MsgHealthStatus` | `health_status` | Plugin → Host | `HealthStatus` |
| `MsgExternalDataSourceGrant` | `external_data_source_grant` | Host → Plugin | `RPCExternalDataSourceGrantMessage` |
| `MsgExternalDataSourceRevoke` | `external_data_source_revoke` | Host → Plugin | `RPCExternalDataSourceGrantRevocation` |
| `MsgExternalDataSourceOperationResult` | `external_data_source_operation_result` | Plugin → Host | `RPCExternalDataSourceOperationResult` |
| `MsgExternalDataSourceStatusRequest` | `external_data_source_status_request` | Host → Plugin | `RPCExternalDataSourceStatusRequest` |
| `MsgExternalDataSourceStatus` | `external_data_source_status` | Plugin → Host | `RPCExternalDataSourceStatusReport` |

## Core payload types

### Invocation payloads

`InvokeGenerator` fields:

| Field | Haskell field | Notes |
| --- | --- | --- |
| `payload_version` | `igPayloadVersion` | Must be `1`. |
| `stage_id` | `igStageId` | Canonical stage ID such as `plugin:civilization`. |
| `seed` | `igSeed` | Generation seed supplied by the host path. |
| `config` | `igConfig` | Parameter map. |
| `terrain` | `igTerrain` | Terrain payload from `Topo.Plugin.RPC.Payload`. |

`InvokeSimulation` fields:

| Field | Haskell field | Notes |
| --- | --- | --- |
| `payload_version` | `isPayloadVersion` | Must be `1`. |
| `node_id` | `isNodeId` | Simulation node/plugin ID. |
| `world_time` | `isWorldTime` | Tick counter. |
| `delta_ticks` | `isDeltaTicks` | Ticks since last run. |
| `calendar` | `isCalendar` | Calendar object from `calendarToJSON`. |
| `config` | `isConfig` | Parameter map. |
| `terrain` | `isTerrain` | Included only when manifest capabilities allow terrain reads. |
| `overlays` | `isOverlays` | Included only when manifest capabilities allow overlay reads. |
| `own_overlay` | `isOwnOverlay` | Included when the plugin can read/write its owned overlay. |

Both payloads reject unsupported `payload_version` values at JSON parse time.

### Result and diagnostic payloads

| Payload | Fields |
| --- | --- |
| `PluginProgress` | `message`, `fraction` where `fraction` is `0.0..1.0` by convention. |
| `PluginLog` | `level` (`debug`, `info`, `warn`, `error`) and `message`. |
| `GeneratorResult` | Required `terrain`; optional `overlay` and `metadata`. |
| `SimulationResult` | Required `overlay`; optional `terrain_writes`. |
| `PluginError` | Numeric `code` plus human-readable `message`. |
| `Heartbeat` | Short `status` text. |
| `HealthStatus` | Boolean `healthy` and human-readable `message`. |

### Handshake payloads

`Handshake` carries `protocol_version`, optional `world_path`,
`host_capabilities`, and optional `auth_challenge`. Production launches include
`launch_auth` in `host_capabilities` and set `auth_challenge`; explicit stdio or
in-process test sessions may omit it. `HandshakeAck` echoes `protocol_version`,
may return a relative `data_directory`, may return `resources`
(`DataResourceSchema` values), and must include `session_id` plus `auth_proof`
when challenged. The proof is `handshakeAuthProof` over the launch session id,
`TOPO_PLUGIN_AUTH_TOKEN`, and challenge; the token is not sent on the wire.
`WorldChanged` carries a new optional `world_path` and expects no response.

## Data-service protocol

`Topo.Plugin.RPC.DataService` owns resource query/mutation payloads. These
messages are still transported in `RPCEnvelope` values.

| Message | Payload | Key fields |
| --- | --- | --- |
| `query_resource` | `QueryResource` | `resource`, `query`, optional `page_size`, optional `page_offset` |
| `query_result` | `QueryResult` | `resource`, `records`, optional `total_count` |
| `mutate_resource` | `MutateResource` | `resource`, `mutation` |
| `mutate_result` | `MutateResult` | `success`, optional `error`, optional `record`, optional `error_code` |

`DataQuery` variants serialize as `{ "type": "all" }`, `by_key`, `by_hex`, or
`by_field`. `DataMutation` variants serialize as `create`, `update`, `delete`,
or `set_hex`. Standardized data-resource failures use snake-case
`DataResourceErrorCode` values and remain plugin-owned: the host can route,
validate, and report them, but it does not interpret plugin storage internals.

## External data-source protocol

`Topo.Plugin.RPC.ExternalDataSource` owns backend-neutral coordination payloads.
The contract is deliberately about provider/source/grant identity, generic
capability scope, access modes, resources, status, opaque references,
`configRefs`, and diagnostics. It does not make topo core a storage backend,
migration runner, lock manager, schema authority, writer coordinator, or repair
process.

| Message | Payload | Purpose |
| --- | --- | --- |
| `external_data_source_grant` | `RPCExternalDataSourceGrantMessage` | Host notification that a provider-owned grant has been brokered to a consumer plugin; new payloads carry `operationId` and optional `operationEpoch`. |
| `external_data_source_revoke` | `RPCExternalDataSourceGrantRevocation` | Host notification that a grant is revoked/unusable; new payloads carry `operationId` and optional `operationEpoch`. |
| `external_data_source_operation_result` | `RPCExternalDataSourceOperationResult` | Plugin ACK/result for grant or revoke with `operationId`, optional `operationEpoch`, `operation`, `providerId`, `consumerId`, `source`, `grant`, `accepted`, `applied`, `status`, `message`, `error`, and `diagnostics`. |
| `external_data_source_status_request` | `RPCExternalDataSourceStatusRequest` | Request source, grant, and consumer-reference status entries. |
| `external_data_source_status` | `RPCExternalDataSourceStatusReport` | Return backend-neutral status entries and optional diagnostics. |

Status reports are derived from manifests by
`externalDataSourceStatusReportFromManifest` for the SDK default path. Entries
include `providerId`, optional `consumerId`, `source`, optional `grant`,
`access`, `resources`, `capabilityScope`, `status`, optional opaque
`reference`, `configRefs`, and optional opaque `diagnostics`. Grant/revoke
payloads accept older JSON without `operationId`, but broker-generated payloads
set it so `RPCExternalDataSourceOperationResult` can be correlated without
mistaking host transport success for consumer-applied state.

PluginManager brokers declared consumer refs automatically. The dependency
resolver selects an exact provider when `provider` is present, otherwise an
eligible provider in deterministic dependency/startup order. A required ref only
blocks startup when no ready, capability/access/resource-compatible provider
source+grant can be resolved; optional unresolved refs become diagnostics. After
the refresh is committed and the consumer is connected, the host sends
`external_data_source_grant` messages with stable operation IDs and tracks them
by consumer/provider/source/grant so disable/failure/shutdown/restart or
source/grant incompatibility can produce `external_data_source_revoke`. Provider
crash, transport failure, provider-reported failure, shutdown, or restart-limit
failure should revoke routing by marking provider/grant availability unavailable
until a fresh ready status arrives.

## Terrain payload contract

`Topo.Plugin.RPC.Payload` produces capability-scoped terrain payloads. The
normal plugin read payload includes summary counts, `chunk_size`, `hex_grid`,
`planet`, `slice`, `encoding = "base64"`, and chunk maps for `terrain`,
`climate`, and `vegetation`. Complete world payloads are opt-in and include many
more generated/persisted layers.

Chunk maps are keyed by chunk ID and store base64 strings of binary
`Topo.Export` chunks. `decodeTerrainWritesValue` accepts missing or empty
`terrain_writes` as no-op writes and otherwise decodes the same base64 chunk
sections for terrain, climate, and vegetation.

## Session dispatch rules

The high-level RPC client starts one receive loop per `RPCConnection` session.
For correlated requests:

1. allocate the next `Word64` request ID;
2. add an `RPCPending` entry with callbacks for interim progress/log messages;
3. send the envelope with `id` set;
4. route `progress` and `log` with that ID to callbacks;
5. remove the pending request when a non-interim correlated envelope arrives;
6. decode `error` as `RPCPluginError`/`RPCDataResourceError` and all other final
   payloads with the expected type-specific decoder.

Transport/protocol failures complete all pending requests, record runtime
failure for supervisor handling, and close the transport.
