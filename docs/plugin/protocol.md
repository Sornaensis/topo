# Plugin RPC protocol

This reference summarizes the current host/SDK contract. Haskell definitions in
`Topo.Plugin.RPC.{Protocol,Scope,Stream,Transport,DataService,ExternalDataSource}`
and `Topo.Plugin.SDK.*` are authoritative.

## Framing, versions, and handshake

Every message uses the same transport framing: a four-byte little-endian
`Word32` JSON-byte length followed by that many UTF-8 JSON bytes. The ordinary
envelope is:

```json
{"id":42,"type":"invoke_generator","payload":{}}
```

`id` is optional for uncorrelated traffic. Responses, logs, and progress for a
request echo its ID. The default maximum frame is 64 MiB and the corresponding
decoded-terrain budget is 48 MiB; host launch configuration may narrow both.
Production transport is a host-created Unix-domain socket or Windows named
pipe. Standard I/O compatibility requires an explicit development/test opt-in.

The host supports protocol **4 through 5**. `selectProtocolVersion` chooses the
highest overlap between that range and `runtime.protocol`; no overlap fails
before launch. `currentProtocolVersion` remains 4 as the default for SDKs that
do not opt into v5 behavior, while `maximumSupportedProtocolVersion` is 5.
The handshake ACK must echo the host-selected version. Production launch
authentication proves a host challenge with a session-bound HMAC; the auth
token itself is never sent on the RPC connection.

Protocol v4 uses materialized single-frame generator and simulation payloads.
For generator and simulation invocations, protocol v5 adds required resolved
invocation scopes and can additionally negotiate `stream_v1`; it preserves the
same outer framing.

## Manifest and adapter negotiation

`generateManifest` derives a range from complete callback implementations:

- complete legacy callbacks only: `4..4`;
- complete scoped or streaming callbacks only: `5..5`;
- complete legacy and scoped/streaming implementations: `4..5`;
- a mixed set that cannot handle every participating generator/simulation in
  either version is rejected.

Generator and simulation participation at protocol v5 requires
`invocationScopes.version` 1 and the corresponding explicit declaration.
Protocol v4 may omit it; the host then resolves the broad
`legacyGeneratorScope` or `legacySimulationScope` compatibility declaration.
A materialized scoped callback is valid v5 without streaming. `stream_v1` is
proposed by the SDK only for native `StreamingGeneratorDef` or
`StreamingSimulationDef` participation.

## Resolved invocation scopes

A declaration limits input terrain sections, chunk selection, dependency and
owned-overlay access, output sections, and decoded-byte budgets. Resolution is
an intersection of the declaration, manifest capabilities, exact invocation
facts, and host limits. The result is immutable and receives a stable SHA-256
`scopeId`; object/map insertion order does not change the digest.

Terrain sections are `terrain`, `climate`, and `vegetation`. Chunk selectors
are all invocation chunks, a dependency-overlay union/intersection, or caller
chunks when that invocation type actually supplies them. Budgets cover terrain
input, overlay input, and output.

Important boundaries:

- A generator cannot request dependency overlays or owned-overlay input. It may
  receive granted terrain and may output granted terrain, metadata only where
  the contract permits it, and a seed for its declared owned overlay when the
  manifest capability and resolved output permit that.
- A simulation receives a complete owned overlay and must be authorized to
  replace that complete overlay. A selector may narrow terrain and dependency
  overlay chunks, but not the owned-overlay replacement.
- A data-resource scope resolves to one exact resource, operation, page, and
  query location. It grants no terrain or overlay sections.

A generator or simulation invocation carries `RPCInvocationScopeBinding`. At
v5 the binding is required; a supplied inline descriptor must match both the
host-resolved value and its digest. Missing or forged bindings fail before
payload use. V4 generator and simulation callers may omit the binding.
Data-resource requests use their exact query/mutation identity to construct a
narrow `DataContext` instead of carrying this binding.

The SDK reflects these boundaries in its contexts:

| callback | data boundary |
|---|---|
| `PluginContext` | broad v4 compatibility world, raw terrain, owned/dependency overlays, seed, parameters |
| `GeneratorContext` | parameters, seed, resolved scope, and optional granted terrain; no overlay inputs |
| `SimulationContext` | optional granted terrain, complete owned overlay, exact dependency overlays, time/calendar, resolved scope; no seed |
| streaming contexts | the scoped fields plus validated `TerrainChunkSource`, bounded `TerrainDeltaSink`, and cancellation |
| `DataContext` | exact resource/operation/page/query/mutation, parameters, logging/progress, world path; no world, terrain, overlay, or seed |

Scoped result constructors enforce output sections, chunk IDs, and
owned-overlay identity and fail closed on widening. SDK dispatch/result
validation applies the resolved output-byte budget before sending the result.

## `stream_v1`

Streaming is an optional v5 application feature. Handshake proposals intersect
codecs and take the lower value for every frame, stream, item, byte, request,
staging, receive-window, and timeout limit. Identity is mandatory and is the
only codec currently enabled. `zstd` is a reserved wire value and is rejected
by current negotiation.

Stream messages (`stream_open`, `stream_data`, `stream_window`, `stream_end`,
`stream_cancel`, `stream_error`) are ordinary length-prefixed JSON envelopes.
The host owns odd stream IDs, the plugin owns even IDs, zero is reserved, and
IDs increase monotonically and are never reused.

Each stream is bound to one parent request and its resolved `scopeId`. The open
record declares snapshot/delta kind, sections, chunks, codec, and optional final
totals. Data records are canonically ordered by section/chunk/part/offset,
sequence checked, credit-windowed, length checked, and SHA-256 checked before
they are exposed to a callback. Sections and chunks cannot widen the resolved
scope. `stream_end` verifies final item count, byte count, and aggregate digest.
A parent result is not complete until every referenced stream has opened,
ended, and had all decoded records consumed. Cancellation, deadlines, budget
failure, and disconnect deterministically clean up the parent and its streams.

## Data resources

Manifest `dataResources` statically authorize plugin-owned query/mutation
surfaces; the runtime handshake may narrow but not widen them. Query and
mutation payloads identify a resource and operation and return correlated
results or structured data-resource failure codes. Prefer `ScopedDataHandler`
and `DataContext`; broad `DataHandler`/`PluginContext` callbacks are explicit
compatibility adapters.

## External data sources

External-source messages broker access to provider-owned systems rather than
implementing plugin data resources. Manifest declarations and references carry
backend-neutral source/provider names, access, capabilities, resources,
grants, statuses, opaque references, and opaque configuration keys. Providers
retain ownership of backend selection, migrations, schemas, connection
semantics, locking, and consistency.

Capabilities are `query`, `mutate`, `subscribe`, `migrate`, and `health`.
Consumer access maps `read` to `query`, `write` to `mutate`, and `admin` to
`migrate`. Configuration origins are user, provider, environment, or
deployment; Topo preserves each key without interpreting it as a path or
connection string.

Wire traffic includes grant, revoke, operation result/ACK, status request, and
status report messages. Operations carry identity so applied grants and
revocations can be acknowledged idempotently. Source and grant status may
include provider ID, availability, health, access mode, capability scope,
version/compatibility markers, and opaque diagnostics. A manifest-declared
`ready` state is not a current runtime observation until the host stamps a
successful provider report; report omissions make retained status stale.
Required unresolved or unavailable consumer references can block startup,
while optional references and provider degradation produce degraded startup.

The committed [schema](manifest-v3.schema.json),
[provider example](examples/provider.json), and
[consumer example](examples/consumer.json) are direct encodings of
`manifestV3Schema`, `manifestV3ProviderExample`, and
`manifestV3ConsumerExample`. The examples intentionally advertise protocol
`4..4`; their scope declarations demonstrate manifest structure and do not
change those canonical runtime bounds.
