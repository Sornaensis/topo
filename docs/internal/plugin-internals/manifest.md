# Plugin Manifest Internals

> **Module:** `Topo.Plugin.RPC.Manifest`
> **Contract:** manifest v3

`Topo.Plugin.RPC.Manifest` owns the typed representation, JSON codecs,
validation helpers, generated schema value, and golden example values for
`manifest.json`.

## Responsibilities

- Decode and encode plugin manifests used by the host, SDK, and tests.
- Keep the manifest contract versioned with `manifestVersion = 3`.
- Provide `manifestV3Schema`, `manifestV3ProviderExample`, and
  `manifestV3ConsumerExample` for docs/golden verification.
- Validate structural invariants that are local to one manifest.
- Preserve a backend-neutral contract for provider-owned external data sources.

Dependency resolution, startup ordering, and host-side grant enforcement are
separate M6/M7 responsibilities and should not be added to this module.

## Top-level typed fields

| Field | Haskell field | Notes |
| --- | --- | --- |
| `manifestVersion` | `rmManifestVersion` | Must be `manifestV3` (`3`) |
| `name` | `rmName` | Stable plugin identifier |
| `version` | `rmVersion` | Plugin version string |
| `runtime` | `rmRuntime` | RPC protocol and optional Topo release bounds |
| `description` | `rmDescription` | Human-readable summary |
| `ui` | `rmUiHints` | Optional presentation hints |
| `generator` | `rmGenerator` | Pipeline insertion declaration |
| `simulation` | `rmSimulation` | Simulation DAG declaration |
| `overlay` | `rmOverlay` | Owned overlay schema file |
| `capabilities` | `rmCapabilities` | Host permissions requested by plugin |
| `config.parameters` | `rmParameters` | User-facing parameter specs |
| `dataResources` | `rmDataResources` | Plugin data-service schemas |
| `dataDirectory` | `rmDataDirectory` | Relative world-save data directory |
| `externalDataSources` | `rmExternalDataSources` | Provider-owned source declarations |
| `externalDataSourceRefs` | `rmExternalDataSourceRefs` | Consumer references to provider-owned sources |
| `startPolicy` | `rmStartPolicy` | Process supervision defaults/overrides |

The parser requires `manifestVersion`, `runtime`, and `runtime.protocol`
because they are part of the v3 contract. Encoding always emits those fields
and any non-empty v3 declarations.

## Validation boundaries

`validateManifest` checks only manifest-local invariants:

- supported manifest version
- non-empty name/version
- simulation declarations must include an owned overlay
- terrain-write capability requires a simulation declaration
- the plugin must participate via generator, simulation, data resources, or
  external data-source provider/consumer declarations
- data resources require matching data capabilities

The function does not resolve graph dependencies, verify provider availability,
or validate runtime grants.

## External data-source model

External data-source fields are intentionally provider-owned and backend-neutral:

- Provider declarations use a source `name`, free-form `kind`, generic
  capabilities (`query`, `mutate`, `subscribe`, `migrate`, `health`), resource
  names, UI hints, declarative lifecycle/status, optional grant declarations,
  and optional opaque `connection` metadata.
- Grant declarations use provider-defined names, access modes (`read`, `write`,
  `admin`), capability/resource subsets, declarative status, and optional opaque
  `reference` metadata.
- Consumer references use local `name`, optional provider plugin ID, source
  name, optional provider grant name, required/optional flag, access grants,
  resource names, UI hints, declarative status, and optional opaque `reference`
  metadata.
- Status states are `unknown`, `unconfigured`, `ready`, `degraded`, and
  `unavailable`. Optional status metadata can carry provider ID, availability,
  health, access-mode, capability-scope, version, compatibility, and opaque
  diagnostics fields. Resolver-facing provider/grant summaries treat only
  `ready` as available; other states remain declared but unavailable until
  runtime or diagnostics report readiness.

No host-owned storage details belong in the manifest. Migrations, backing
schemas, connection details, and consistency rules are owned by the provider
plugin, adapter, or external system. If a provider includes connection or
binding details, those JSON objects are opaque provider metadata: topo may
preserve, broker, or report them, but does not parse them into a host-owned
storage backend, migration table, schema rule, or consistency coordinator.

## Golden docs/tests

The committed schema and examples live in `docs/plugin-dev/`:

- `manifest-v3.schema.json`
- `examples/manifest-v3-provider.json`
- `examples/manifest-v3-consumer.json`

`Spec.PluginRPC` decodes those files and compares them with the generated
`Value`s from `Topo.Plugin.RPC.Manifest`, then parses and validates the example
manifests. This keeps public docs aligned with the compiled contract.
