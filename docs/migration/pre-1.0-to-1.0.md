# Migrating pre-1.0 automation and plugins to Topo 1.0

Topo 1.0 has one public automation surface: direct HTTP/OpenAPI served by
`topo-seer`. The former `topo-mcp` bridge has been removed, and legacy command
IPC is internal/test compatibility only. This guide collects the migration work
for old MCP or command clients, plugin authors moving to manifest/protocol v3,
backend-neutral external data-source declarations, and compatibility policy.

## Upgrade checklist

- Launch `topo-seer` directly with HTTP enabled and discover operations from
  `GET /openapi.json`.
- Replace old MCP `tools/list`, `tools/call`, `resources/list`, and
  `resources/read` usage with resource-oriented HTTP routes.
- Use the [MCP-to-HTTP parity matrix](../inventory/mcp-http-parity.md) only to
  translate retired tool/resource names; do not target command-compatible routes
  for new automation.
- Remove dependencies on the retired `topo-mcp` package, stdio JSON-RPC
  transport, `topo://` resource URIs, and external command IPC usage.
- Regenerate or update plugin manifests to `manifestVersion: 3` with protocol
  bounds that include RPC protocol `3`.
- Move external data-source provisioning, migrations, backing schemas,
  connection details, locking, and repair policy into provider plugins,
  adapters, deployment configuration, or external systems.

## Automation clients: MCP and command IPC to HTTP/OpenAPI

Run the 1.0 automation host directly:

```sh
stack exec topo-seer -- --headless --http 127.0.0.1:7373
curl http://127.0.0.1:7373/health
curl http://127.0.0.1:7373/openapi.json
```

The OpenAPI document is the operation catalog for clients. Typical
translations are:

| Pre-1.0 usage | 1.0 HTTP/OpenAPI replacement |
|---|---|
| MCP `ping` | `GET /health` |
| MCP `initialize` / `tools/list` | `GET /version` and `GET /openapi.json` |
| MCP `resources/read topo://state` | `GET /state` |
| MCP `resources/read topo://hex/{q}/{r}` | `GET /terrain/hex?q={q}&r={r}` |
| MCP `tools/call set_seed` | `POST /ui/seed` with body `{ "seed": 123 }` |
| MCP `tools/call take_screenshot` | `POST /screenshots` |

If a pre-1.0 client called command IPC methods directly, use the parity
matrix's old command-method column to find the corresponding primary
HTTP/OpenAPI target. Do not migrate external clients to `POST /commands/<method>`.

HTTP returns JSON directly instead of MCP JSON-RPC result envelopes or
`topo://` resource text. Route misses use HTTP `404`, validation and service
failures use the HTTP error envelope, and token-protected non-loopback bindings
use `Authorization: Bearer TOKEN` as described in the
[HTTP/OpenAPI contract guide](../api/README.md).

The compatibility command routes under `POST /commands/<method>` exist only for
in-repo compatibility and tests while service extraction continues. They are
not a public 1.0 automation path and are not a migration target for client
integrations.

## Plugin authors: manifest v3 and RPC protocol v3

Topo 1.0 supports manifest v3 and RPC protocol v3 as the public plugin
contract. Haskell plugins should update to the 1.0 `topo-plugin-sdk` and let the
SDK emit `manifest.json`; non-Haskell plugins should write the same v3 JSON
shape directly.

Manifest updates to check:

- Set `manifestVersion` to `3`.
- Declare `runtime.protocol.min` and `runtime.protocol.max` so the range
  includes protocol `3`.
- Declare at least one participation surface: `generator`, `simulation`,
  `dataResources`, `externalDataSources`, or `externalDataSourceRefs`.
- Keep capabilities minimal and explicit. Data-resource plugins need `dataRead`
  and, for create/update/delete operations, `dataWrite`.
- Use `externalDataSources` for provider-owned sources and
  `externalDataSourceRefs` for consumer dependencies.
- Keep `connection`, `reference`, `configRefs.metadata`, and diagnostics opaque;
  the host must not interpret them as a built-in database contract.

Protocol updates to check:

- Use length-prefixed JSON envelopes with canonical v3 message tags such as
  `handshake`, `invoke_generator`, `invoke_simulation`, `query_resource`,
  `mutate_resource`, and the `external_data_source_*` grant/status messages.
- Preserve request correlation by echoing `id` on responses, progress, and logs.
- Use the host-created endpoint from `TOPO_PLUGIN_ENDPOINT` and
  `TOPO_PLUGIN_ENDPOINT_KIND`; production launch is Windows named pipes or Unix
  domain sockets.
- Treat `TOPO_PLUGIN_STDIO_COMPAT=1` as test/development compatibility only,
  not as the production transport.

See the [manifest v3 reference](../plugin-dev/manifest.md) and
[RPC protocol reference](../plugin-dev/rpc-protocol.md) for the contract-tested
field and message registry.

## Backend-neutral external data sources

External data-source declarations describe provider-owned data that Topo can
broker, display, and route by capability. The core contract is intentionally
backend-neutral:

- Providers advertise source names, resource names, generic capabilities,
  grants, status, health/policy metadata, opaque connection handles, and opaque
  configuration references.
- Consumers request provider/source/grant combinations and access modes such as
  `read`, `write`, or `admin`; the host brokers those requests only when the
  provider grants matching generic capabilities (`query`, `mutate`, `migrate`).
- Status values such as `ready`, `degraded`, and `unavailable` control whether a
  grant is brokerable. Provider crashes, transport failures, shutdown, or
  restart-limit failures make grants unavailable until refreshed status reports
  readiness again.
- Provider plugins, adapters, deployment tooling, or external systems own
  provisioning, migrations, backing schemas, connection strings, locks, writer
  coordination, consistency rules, and repair procedures.

SQLite may appear in examples, fixtures, or optional adapters, but it is not a
privileged Topo 1.0 shared-state specification and must not be treated as the
host-owned external data-source backend.

## Compatibility and deprecation policy

- **HTTP/OpenAPI:** API major version `1` permits additive routes, fields, enum
  values, examples, and schemas. Breaking route, payload, authentication, or
  error-envelope changes require a new major API version or a documented
  migration window.
- **MCP:** The MCP bridge was a pre-1.0 transition path and has been removed.
  There is no 1.0 MCP compatibility promise.
- **Command IPC:** Command IPC remains internal/test compatibility only. It is
  not publicly deprecated because it is not a public 1.0 interface, and it must
  not be documented as an automation exception.
- **Plugins:** Manifest v3 and RPC protocol v3 are the supported 1.0 plugin
  contract. Older manifests/protocols may appear only in migration notes or
  host diagnostics and should be upgraded before relying on 1.0 compatibility.
- **Data resources:** Plugin-owned data-resource schemas and backend-neutral
  external data-source status/grant metadata can evolve additively. Provider
  backends and storage migrations are outside the core Topo spec.

## Related documents

- [Topo 1.0 release notes](../release-notes/1.0.md)
- [HTTP/OpenAPI contract](../api/README.md)
- [MCP-to-HTTP parity matrix](../inventory/mcp-http-parity.md)
- [Manifest v3 reference](../plugin-dev/manifest.md)
- [RPC protocol reference](../plugin-dev/rpc-protocol.md)
- [Plugin system guide](../plugins.md)
