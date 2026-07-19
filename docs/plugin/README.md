# Writing a Topo plugin

A Topo plugin is a standalone executable using `topo-plugin-sdk`. Import
`Topo.Plugin.SDK` for the normal authoring surface; the specialist modules
listed in [`topo-plugin-sdk/package.yaml`](../../topo-plugin-sdk/package.yaml)
remain available when narrower imports are useful.

## Define and run the plugin

Build a `PluginDef` from `defaultPluginDef`, supplying identity and any
parameters, generator, simulation, overlay, data-resource, or external-source
participation. Use the SDK entry point in the executable:

```haskell
import Topo.Plugin.SDK

main :: IO ()
main = runPluginWithManifestCommand myPlugin

myPlugin :: PluginDef
myPlugin = defaultPluginDef
  { pdName = "my-plugin"
  , pdVersion = "0.1.0"
  , pdGeneratorScope = Just GeneratorScopeDef
      { gsdInsertAfter = "erosion"
      , gsdRequires = ["erosion"]
      , gsdScope = myScope
      , gsdRun = runGenerator
      }
  }
```

`generateManifest` derives manifest v3, protocol bounds, participation, scopes,
and safely inferred capabilities from that same definition. Scoped simulation
terrain output and scoped owned-overlay output are inferred from their
declarations. Additional capabilities must be explicit when behavior is not
represented by a scope—for example, broad v4 simulation terrain writes or a
broad v4 generator-only overlay output.

See [the protocol reference](protocol.md) before selecting legacy, scoped, or
streaming callbacks. In particular, protocol v5 generator and simulation
participation requires explicit invocation scope version 1.

## Package before discovery

Install each plugin as a directory containing:

- `manifest.json` generated from the `PluginDef`;
- an executable named exactly as the manifest `name` (`.exe` is also accepted
  on Windows);
- every relative `overlay.schemaFile`, when an overlay is declared.

`runPluginWithManifestCommand` provides manifest-only install actions which
return without opening an RPC transport:

```text
my-plugin --topo-write-manifest
my-plugin --topo-write-manifest INSTALL_DIR
my-plugin --topo-write-manifest-file PATH
```

The first writes to the current directory and the second writes
`INSTALL_DIR/manifest.json`. Packaging code may instead call
`writePluginManifest` or `writePluginManifestToDirectory` directly.

Topo discovery reads `manifest.json`, validates it and its referenced schema,
and only then permits launch. A directory with no manifest is reported as
degraded with `manifest_missing`; discovery and manifest refresh do not execute
it. A normal host launch calls `runPlugin`, which refreshes an already
discovered manifest in the plugin working directory, but that write cannot
bootstrap discovery.

## Choose the callback boundary

- `GeneratorDef` and `SimulationDef` are broad protocol-v4 compatibility
  adapters using `PluginContext`.
- `GeneratorScopeDef` and `SimulationScopeDef` are materialized protocol-v5
  adapters. They receive only their resolved sections/chunks and use
  fail-closed result helpers such as `generatorResultFromScopedTerrain` and
  `simulationResultFromScopedOverlay`.
- `StreamingGeneratorDef` and `StreamingSimulationDef` are native v5 callbacks.
  They replace materialized terrain with a validated replayable
  `TerrainChunkSource`, a bounded `TerrainDeltaSink`, and cooperative
  cancellation.
- Prefer `ScopedDataHandler` for data resources. Its `DataContext` identifies
  the exact resource, operation, page/query or mutation, parameters,
  logging/progress, and world path. It intentionally contains no terrain,
  overlay, world, or seed. `DataHandler` remains an explicit broad
  compatibility adapter.

A generator may receive scoped terrain and may seed its declared owned overlay,
but it receives no overlay inputs. A simulation may receive scoped terrain,
exact dependency overlays, and the complete plugin-owned overlay; replacement
of that owned overlay is whole-overlay, even when terrain/dependency selectors
are narrower.

## External data sources

External data sources are provider-owned and backend-neutral. A provider
manifest advertises names, capabilities, resource scopes, grants, status, and
opaque connection/config references. Consumer references request access to a
named provider source and optional grant. Topo validates and brokers those
contracts but does not interpret the provider's backend, migrations, schemas,
connection details, locking, or consistency rules.

This contract is distinct from plugin-owned data-resource query/mutation
handlers. External capabilities are `query`, `mutate`, `subscribe`, `migrate`,
and `health`; consumer access `read`, `write`, and `admin` requires respectively
`query`, `mutate`, and `migrate`. A manifest's declared ready state is
diagnostic until a current provider report is host-observed. A required
unavailable reference can block startup; optional references and provider
degradation degrade it.

Use the generated [manifest v3 schema](manifest-v3.schema.json) and canonical
[provider](examples/provider.json) and [consumer](examples/consumer.json)
examples as structural references. The Haskell types in
`Topo.Plugin.RPC.Manifest` remain authoritative.

## Maintained examples

`topo-plugin-example` defines `terrainRoughenPlugin`. It supplies both a broad
v4 adapter and an elevation-only scoped v5 adapter, so its generated manifest
advertises protocol 4 through 5. Its seed/iteration/tile hash is deterministic,
and every result elevation is clamped to `[0,1]`.

`topo-plugin-civ-example` defines `civPlugin` (version 1.1.0). Generation seeds
a deterministic sparse civilization overlay from elevation, climate, and
vegetation. Its hourly simulation evolves the owned overlay without terrain
writes; enabling trade changes both `trade_value` and `food_supply`, normalized
fields stay bounded, and population is capped. Its v5 simulation scope requests
no terrain, requests only the `weather` dependency and complete owned overlay,
and returns only the owned overlay.

The example's immutable `settlements` and `cultures` handlers and its
`settlement-ledger` external source are sample-catalogue demonstrations. They
do not mirror generated civilization state. The generated/evolving overlay is
the world-state example.
