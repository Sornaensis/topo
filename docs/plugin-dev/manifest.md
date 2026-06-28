# Manifest Format

Topo plugins declare their public contract in `manifest.json`. The Haskell SDK
writes this file from `PluginDef` when the plugin starts, but non-Haskell
plugins can produce the same JSON directly.

The current contract is **manifest v3**. It describes plugin identity, runtime
and RPC protocol compatibility, capabilities, parameters, generator/simulation
participation, owned overlays, data resources, backend-neutral external data
sources, and UI hints.

Canonical machine-readable files:

- [Manifest v3 JSON Schema](manifest-v3.schema.json)
- [Provider example](examples/manifest-v3-provider.json)
- [Consumer example](examples/manifest-v3-consumer.json)

## Minimal shape

```json
{
  "manifestVersion": 3,
  "name": "terrain-roughen",
  "version": "0.1.0",
  "runtime": {
    "protocol": { "min": 3, "max": 3 }
  },
  "generator": {
    "insertAfter": "erosion",
    "requires": ["erosion"]
  },
  "capabilities": ["readTerrain", "log"]
}
```

`name` is the stable plugin identifier and should match the plugin directory.
`version` is the plugin version. `manifestVersion` must be `3` for this
contract.

## Runtime and protocol bounds

`runtime.protocol.min` and `runtime.protocol.max` declare the inclusive RPC
message contract versions the plugin supports. A plugin should keep this range
as narrow as its tested compatibility. `runtime.topo.min` and
`runtime.topo.max` are optional opaque Topo release strings for plugins that
want to state host release bounds.

## Capabilities

Capabilities are the permissions the plugin asks the host to grant:

| Capability | Meaning |
| --- | --- |
| `log` | Send log messages to the host |
| `noise` | Use host-provided deterministic noise helpers |
| `readTerrain` / `readWorld` | Read terrain/world payloads |
| `writeTerrain` / `writeWorld` | Return terrain/world writes |
| `readOverlay` | Read dependency overlays |
| `writeOverlay` | Write the plugin-owned overlay |
| `dataRead` | Expose or consume readable data resources |
| `dataWrite` | Expose writable data-resource operations |

The SDK infers common capabilities from `PluginDef`. Hand-written manifests
should request only what the plugin needs.

## Generator, simulation, and overlay declarations

`generator` inserts a plugin stage into the world-generation pipeline:

```json
"generator": {
  "insertAfter": "biomes",
  "requires": ["biomes", "rivers"]
}
```

`simulation` declares simulation DAG dependencies by overlay or node name:

```json
"simulation": { "dependencies": ["weather"] }
```

`overlay.schemaFile` points to the plugin-owned `.toposchema` file relative to
the plugin directory. A simulation plugin that writes an overlay should declare
both `simulation` and `overlay`.

## Parameters and UI hints

`config.parameters` describes user-editable settings. Supported parameter
scalar types are `float`, `int`, and `bool`. Numeric parameters can include a
`range` of `[min, max]`.

`ui` is optional and purely presentational:

```json
"ui": {
  "displayName": "Civilization",
  "category": "Simulation",
  "tags": ["settlements", "culture"],
  "icon": "settlement",
  "docsUrl": "https://example.invalid/civilization",
  "order": 20
}
```

The host may use these hints in plugin lists, diagnostics, and data browsers;
they do not affect dependency ordering or permissions.

## Data resources

`dataResources` declares resource schemas that the host can browse or edit via
the plugin data-service RPC. Each resource includes fields, operation flags,
and a key field. If any resource is declared, the manifest must request
`dataRead`; resources with `create`, `update`, or `delete` should also request
`dataWrite`.

Field types may be scalar (`text`, `int`, `float`, `double`, `bool`, `fixed2`,
`fixed3`, `fixed4`) or structured (`enum`, `record`, `adt`). The plugin owns
interpretation and validation beyond the advertised schema.

## Backend-neutral external data sources

Manifest v3 can describe data that remains owned by a provider plugin. The
contract intentionally uses opaque source names, provider plugin IDs, generic
capabilities, access grants, resource names, lifecycle/status metadata, and
opaque connection/reference metadata. It does not require or expose a storage
engine, connection string, file layout, or host-owned migration plan.

Providers advertise sources with `externalDataSources`:

```json
"externalDataSources": [
  {
    "name": "settlement-ledger",
    "label": "Settlement Ledger",
    "kind": "catalog",
    "capabilities": ["query", "health"],
    "resources": ["settlements"],
    "status": { "state": "ready" },
    "connection": { "handle": "provider-owned:settlement-ledger" },
    "grants": [
      {
        "name": "settlement-read",
        "access": ["read"],
        "capabilities": ["query", "health"],
        "resources": ["settlements"],
        "status": { "state": "ready" },
        "reference": { "handle": "grant:settlement-read" }
      }
    ]
  }
]
```

Consumers declare dependencies with `externalDataSourceRefs`:

```json
"externalDataSourceRefs": [
  {
    "name": "settlements",
    "provider": "civilization",
    "source": "settlement-ledger",
    "required": true,
    "access": ["read"],
    "resources": ["settlements"],
    "grant": "settlement-read",
    "status": { "state": "unknown" },
    "reference": { "binding": "trade-routes:settlements" }
  }
]
```

Grant names are provider-defined and backend-neutral. `connection` and
`reference` are opaque JSON objects for provider-owned handles or binding
metadata; topo may broker and display them, but does not interpret them as a
host-owned backing store. Status states are `unknown`, `unconfigured`, `ready`,
`degraded`, and `unavailable`; provider/grant summaries are brokerable only
when status is `ready`. Manifest status is declarative startup metadata; runtime
health can be refined by plugin handshakes and diagnostics.

## SDK generation

The SDK converts `PluginDef` to manifest v3 with:

- `manifestVersion = 3`
- protocol bounds set to the current RPC protocol version, plus optional
  `pdRuntimeTopoMin` / `pdRuntimeTopoMax` host bounds
- `pdDescription`, `pdUiHints`, and `pdStartPolicy` as manifest v3 metadata
  (with `ui.displayName` defaulting to `pdName`)
- safe capabilities inferred from generator, simulation, and data-resource
  fields, plus explicit `pdCapabilities` for non-inferable permissions such as
  simulation terrain writes
- `pdExternalDataSources` and `pdExternalDataSourceRefs` copied into the
  manifest unchanged

Manual editing is supported for non-Haskell plugins, but SDK users should keep
`PluginDef` as the source of truth.
