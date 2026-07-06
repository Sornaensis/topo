# SDK Types Reference

> **Status:** Stub — to be expanded from existing `plugins.md`

## PluginDef

Top-level plugin definition. Use `defaultPluginDef` as a starting point.

| Field | Type | Description |
|-------|------|-------------|
| `pdName` | `Text` | Unique plugin identifier |
| `pdVersion` | `Text` | Version string |
| `pdDescription` | `Maybe Text` | Manifest v3 description (`Nothing` uses a default) |
| `pdRuntimeTopoMin` / `pdRuntimeTopoMax` | `Maybe Text` | Optional Topo host version bounds |
| `pdParams` | `[ParamDef]` | User-facing parameters |
| `pdSchemaFile` | `Maybe FilePath` | Overlay schema file |
| `pdGenerator` | `Maybe GeneratorDef` | Generator participation |
| `pdSimulation` | `Maybe SimulationDef` | Plugin simulation declaration; dependencies are simulation node IDs and may reference host built-ins like `weather` |
| `pdCapabilities` | `[RPCCapability]` | Explicit extra capabilities, for example `CapWriteTerrain` |
| `pdDataDirectory` | `Maybe FilePath` | Plugin data directory under the world save |
| `pdDataResources` | `[DataResourceDef]` | Data-service resource schemas and handlers |
| `pdUiHints` | `RPCUIHints` | Manifest v3 UI presentation hints |
| `pdExternalDataSources` | `[RPCExternalDataSourceDecl]` | Provider-owned external data sources, grants, status, opaque config refs, and connection metadata |
| `pdExternalDataSourceRefs` | `[RPCExternalDataSourceRef]` | Consumed external data sources, requested grants, status, opaque config refs, and reference metadata |
| `pdStartPolicy` | `RPCStartPolicy` | Host-side process supervision policy |

External data-source grants use `RPCExternalDataSourceGrant`; topo treats all
`connection`/`reference` values, `configRefs`, and status `diagnostics` as
opaque provider-owned metadata. Config refs record a local name, origin (`user`,
`provider`, `environment`, or `deployment`), opaque key, required flag,
compatibility marker, and optional metadata. Status can carry backend-neutral
provider ID, availability, health, access-mode, capability-scope, version, and
compatibility markers. The normal SDK path is declarative: providers populate
`pdExternalDataSources`, consumers populate `pdExternalDataSourceRefs`, and the
host automatically sends grant/revocation callbacks when a ready compatible
provider grant is resolved or becomes unavailable. Access is brokered only when
the grant has the generic capability for that access (`read` -> `query`,
`write` -> `mutate`, `admin` -> `migrate`). Migrations, backing schemas,
connection details, failure cleanup, locks, writer coordination, and consistency
rules remain with provider plugins, adapters, or external systems; the SDK only
serializes backend-neutral declarations, grants, status/errors, config
references, and opaque metadata.

## ParamDef

<!-- TODO: Full reference -->

## GeneratorDef

| Field | Type | Description |
|-------|------|-------------|
| `gdInsertAfter` | `Text` | Generation stage to run after |
| `gdRequires` | `[Text]` | Required earlier generation stages |
| `gdRun` | `PluginContext -> IO (Either Text GeneratorTickResult)` | Generator callback |

`defaultGeneratorTickResult` returns an empty terrain payload with no overlay or metadata.
Use `generatorResultFromTerrain` or related payload helpers when returning modified terrain.

## SimulationDef

| Field | Type | Description |
|-------|------|-------------|
| `sdDependencies` | `[Text]` | Simulation node IDs that tick before this plugin declaration; may include host built-ins such as `weather` |
| `sdSchedule` | `Maybe SimulationScheduleDecl` | Optional cadence; `Nothing` emits the hourly default |
| `sdTick` | `PluginContext -> IO (Either Text SimulationTickResult)` | Simulation callback |

`defaultSimulationTickResult` returns an empty overlay payload with no terrain writes.
Simulation plugins that return terrain writes should add `CapWriteTerrain` to `pdCapabilities`.

## PluginContext

Runtime context provided to callbacks.

| Field | Type | Description |
|-------|------|-------------|
| `pcWorld` | `TerrainWorld` | Current terrain state |
| `pcParams` | `Map Text Value` | Current parameter values |
| `pcTerrain` | `Value` | Raw terrain payload from the host invocation |
| `pcOwnOverlay` | `Maybe Value` | Plugin-owned overlay payload for simulation ticks |
| `pcOverlays` | `Map Text Value` | Dependency overlay payloads keyed by overlay name |
| `pcSeed` | `Word64` | World generation seed |
| `pcLog` | `Text -> IO ()` | Log to host |
| `pcProgress` | `Text -> Double -> IO ()` | Emit interim progress for the current generator, simulation, query, or mutation callback |
| `pcWorldPath` | `Maybe FilePath` | Current world save directory path, when known |

Use `reportPluginProgress ctx message fraction` for a named helper around
`pcProgress`; the longer name avoids confusion with the host-side
`Topo.Plugin.reportProgress`. Fractions are absolute for the current invocation,
conventionally `0.0` through `1.0` inclusive. The final result or error remains
authoritative, and callbacks do not need to emit either endpoint. The SDK
clamps finite values into `[0,1]` and maps `NaN`/infinities defensively before
JSON encoding.

`PluginContext(..)` is exported. The added `pcProgress` field is therefore
source-breaking for plugins that manually construct `PluginContext` values or
pattern-match the record exhaustively/positionally; add a `pcProgress` field or
use record wildcards/field selectors as appropriate.
