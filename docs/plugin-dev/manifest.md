# Manifest Format

Topo plugins declare their public contract in `manifest.json`. topo-seer reads
this file during discovery and will not execute unmanifested plugin directories
to bootstrap one. Haskell SDK plugins should write the file from `PluginDef`
during an explicit build/install packaging step; non-Haskell plugins can produce
the same JSON directly.

The current contract is **manifest v3**. It describes plugin identity, runtime
and RPC protocol compatibility, capabilities, parameters, generator/simulation
participation, owned overlays, data resources, backend-neutral external data
sources, and UI hints.

Canonical machine-readable files:

- [Manifest v3 JSON Schema](manifest-v3.schema.json)
- [Provider example](examples/manifest-v3-provider.json)
- [Consumer example](examples/manifest-v3-consumer.json)

`Topo.Plugin.RPC.Manifest` generates those values as `manifestV3Schema`,
`manifestV3ProviderExample`, and `manifestV3ConsumerExample`. `Spec.PluginRPC`
compares the committed JSON files with the compiled values, and
`Spec.PluginDocs` checks that this reference names every top-level manifest
schema field.

## Top-level field reference

| Field | Required | Source type | Meaning |
| --- | --- | --- | --- |
| `manifestVersion` | Yes | `rmManifestVersion` | Must be manifest v3 (`3`). |
| `name` | Yes | `rmName` | Stable plugin identifier; must not be a path. |
| `version` | Yes | `rmVersion` | Plugin version string. |
| `runtime` | Yes | `rmRuntime` | Protocol bounds and optional opaque Topo release bounds. |
| `description` | No | `rmDescription` | Human-readable summary. |
| `ui` | No | `rmUiHints` | Presentation hints only. |
| `generator` | No | `rmGenerator` | Generation-pipeline insertion and prerequisites. |
| `simulation` | No | `rmSimulation` | Simulation DAG dependencies. |
| `overlay` | No | `rmOverlay` | Plugin-owned `.toposchema` path. |
| `capabilities` | No | `rmCapabilities` | Host permissions requested by the plugin. |
| `config` | No | `rmParameters` | User-facing `parameters`. |
| `dataResources` | No | `rmDataResources` | Plugin-owned data-service schemas. |
| `dataDirectory` | No | `rmDataDirectory` | Relative data directory under the world save. |
| `externalDataSources` | No | `rmExternalDataSources` | Provider-owned external source declarations. |
| `externalDataSourceRefs` | No | `rmExternalDataSourceRefs` | Consumer references to provider-owned sources/grants. |
| `startPolicy` | No | `rmStartPolicy` | Host process supervision overrides. |

A manifest must declare at least one participation surface: `generator`,
`simulation`, `dataResources`, `externalDataSources`, or
`externalDataSourceRefs`.

## Discovery and packaging contract

Discovery is manifest-first. topo-seer scans plugin directories for an existing
`manifest.json`; it does **not** launch unknown executables to ask them to
create a manifest. `stack install` alone copies only the executable, so SDK
plugins need a packaging command that also writes `manifest.json`, ensures the
launch executable is available under the manifest `name`, and copies any
referenced sidecar files.

For SDK plugins, use `runPluginWithManifestCommand` as the executable entry
point and run the manifest-only branch during install:

```bash
PLUGIN_DIR="$HOME/.topo/plugins/my-plugin"
stack install --local-bin-path "$PLUGIN_DIR"
"$PLUGIN_DIR/my-plugin" --topo-write-manifest "$PLUGIN_DIR"
```

If `overlay.schemaFile` or another manifest field references a file such as
`civilization.toposchema`, copy that file into the plugin directory before
starting topo-seer. Non-Haskell plugins can hand-write and package manifest v3
JSON directly.

## Minimal shape

```json
{
  "manifestVersion": 3,
  "name": "terrain-roughen",
  "version": "0.1.0",
  "runtime": {
    "protocol": { "min": 4, "max": 4 }
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
message contract versions the plugin supports. The current SDK emits protocol 4
bounds because production startup requires launch auth/session proof in the
handshake. A plugin should keep this range as narrow as its tested
compatibility. `runtime.topo.min` and `runtime.topo.max` are optional opaque
Topo release strings for plugins that want to state host release bounds.

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

`simulation` declares a plugin simulation node, separate from host built-in simulation nodes such as `weather`. Its `dependencies` list contains simulation node IDs, not generator stage names or plugin startup dependencies, and may reference host built-ins like `weather` when the plugin should tick after built-in weather. Missing schedule fields default to hourly (`interval_ticks: 1`, `phase_ticks: 0`, `catch_up: "run_once_if_due"`). `interval_ticks` must be at least `1`, `phase_ticks` must be non-negative and less than `interval_ticks`, and `catch_up` must be one of the documented enum values:

```json
"simulation": {
  "dependencies": ["weather"],
  "interval_ticks": 1,
  "phase_ticks": 0,
  "catch_up": "run_once_if_due"
}
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

Manifest v3 can describe data that remains owned by a provider plugin, adapter,
or external system. The contract intentionally uses opaque source names,
provider plugin IDs, generic capabilities, access grants, resource names,
lifecycle/status metadata, backend-neutral health/policy fields, opaque
connection/reference metadata, and explicit opaque configuration references.
It does not require or expose a storage engine, connection string, file layout,
or host-owned migration plan.

Migrations, backing schemas, connection details, and consistency rules are
owned by the provider plugin, adapter, or external system. Topo may surface
status and error messages from those owners, but core manifest validation must
not prescribe backend-specific migration tables, table/schema naming rules, or
consistency policies.

Providers advertise sources with `externalDataSources`:

```json
"externalDataSources": [
  {
    "name": "settlement-ledger",
    "label": "Settlement Ledger",
    "kind": "catalog",
    "capabilities": ["query", "health"],
    "resources": ["settlements"],
    "status": {
      "state": "ready",
      "providerId": "civilization",
      "availability": "available",
      "health": "healthy",
      "accessMode": "read_only",
      "capabilityScope": ["query", "health"],
      "version": "settlement-ledger.v1",
      "compatibility": "manifest-v3",
      "diagnostics": { "reportedBy": "civilization" }
    },
    "connection": { "handle": "provider-owned:settlement-ledger" },
    "configRefs": [
      {
        "name": "settlement-ledger-binding",
        "origin": "provider",
        "key": "civilization.settlement-ledger",
        "required": true,
        "compatibility": "manifest-v3",
        "metadata": { "handle": "provider-owned:settlement-ledger" }
      }
    ],
    "grants": [
      {
        "name": "settlement-read",
        "access": ["read"],
        "capabilities": ["query", "health"],
        "resources": ["settlements"],
        "status": {
          "state": "ready",
          "providerId": "civilization",
          "availability": "available",
          "health": "healthy",
          "accessMode": "read_only",
          "capabilityScope": ["query", "health"],
          "version": "settlement-read.v1",
          "compatibility": "manifest-v3",
          "diagnostics": { "grant": "settlement-read" }
        },
        "reference": { "handle": "grant:settlement-read" },
        "configRefs": [
          {
            "name": "settlement-read-binding",
            "origin": "provider",
            "key": "civilization.settlement-read",
            "required": true,
            "compatibility": "manifest-v3",
            "metadata": { "grant": "settlement-read" }
          }
        ]
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
    "status": {
      "state": "unknown",
      "providerId": "civilization",
      "availability": "unknown",
      "health": "unknown",
      "accessMode": "read_only",
      "capabilityScope": ["query"],
      "version": "settlement-ledger.v1",
      "compatibility": "manifest-v3",
      "diagnostics": { "resolution": "dependency-startup" }
    },
    "reference": { "binding": "trade-routes:settlements" },
    "configRefs": [
      {
        "name": "settlements-binding",
        "origin": "deployment",
        "key": "trade-routes.settlements",
        "required": true,
        "compatibility": "manifest-v3",
        "metadata": { "binding": "trade-routes:settlements" }
      }
    ]
  }
]
```

Grant names are provider-defined and backend-neutral. In the normal 1.0 path,
topo automatically resolves each consumer `externalDataSourceRefs` entry to a
ready provider source/grant during plugin refresh and sends the resulting
`external_data_source_grant` notification after the consumer handshake. Exact
`provider` refs bind only to that plugin; omitted `provider` refs use the same
deterministic dependency ordering as startup. Topo only brokers a requested
access mode when the source and grant are `ready`, resource-compatible, and the
grant carries the corresponding generic capability: `read` requires `query`,
`write` requires `mutate`, and `admin` requires `migrate`. Provider plugins,
adapters, or external systems still own concrete authorization, locks, writer
coordination, and repair behavior.

`connection` and `reference` are opaque JSON objects for provider-owned handles
or binding metadata. `configRefs` are structured only enough to record a local
reference name, an origin (`user`, `provider`, `environment`, or `deployment`),
an opaque key, whether the binding is required, optional compatibility text, and
opaque metadata. Topo may broker and display these values, but does not interpret
them as a host-owned backing store, migration runner, schema authority, or
consistency coordinator. Even when a source advertises the `migrate`
capability, it is a provider operation/status capability rather than a
topo-owned migration table or schema rule. Status states are `unknown`,
`unconfigured`, `ready`, `degraded`, and `unavailable`; provider/grant
summaries are brokerable only when status is `ready`. Required refs that cannot
be resolved to a ready grant block startup with `external_data_source_blocked`;
optional refs start with diagnostics. On provider disable, plugin crash,
transport failure, provider-reported failure, shutdown, restart, or refreshed
source/grant incompatibility, topo sends `external_data_source_revoke` for any
active brokered grants and treats provider sources and grants as
unavailable/revoked for dependency routing until a fresh ready manifest/runtime
status is available. It does not delete provider data or run backend cleanup.

Optional status metadata can carry a backend-neutral `providerId`,
`availability`, `health`, `accessMode`, `capabilityScope`, `version`,
`compatibility`, and opaque `diagnostics` object. Manifest status is declarative
startup metadata; runtime health can be refined by plugin handshakes and
diagnostics. World save/load preserves external data-source declarations,
consumer references, opaque config references, compatibility markers, and
opaque connection/reference metadata in the world manifest for compatibility
checks, but loading a world does not reconnect, migrate, lock, or repair
provider-owned stores.

## SDK generation

The SDK converts `PluginDef` to manifest v3 with `generateManifest`. Use
`writePluginManifestToDirectory` or the `runPluginWithManifestCommand`
`--topo-write-manifest` branch to write that manifest without starting the RPC
transport loop. Lower-level `writeManifest` remains available when you already
have an `RPCManifest` value.

The generated manifest includes:

- `manifestVersion = 3`
- protocol bounds set to the current RPC protocol version, plus optional
  `pdRuntimeTopoMin` / `pdRuntimeTopoMax` host bounds
- `pdDescription`, `pdUiHints`, and `pdStartPolicy` as manifest v3 metadata
  (with `ui.displayName` defaulting to `pdName`)
- safe capabilities inferred from generator, simulation, and data-resource
  fields, plus explicit `pdCapabilities` for non-inferable permissions such as
  simulation terrain writes
- `pdExternalDataSources` and `pdExternalDataSourceRefs` copied into the
  manifest unchanged, including any opaque external data-source `configRefs`

Manual editing is supported for non-Haskell plugins, but SDK users should keep
`PluginDef` as the source of truth and generate/copy `manifest.json` during
build or install before topo-seer discovery.
