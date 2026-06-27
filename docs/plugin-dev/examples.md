# Plugin Examples

This page summarizes the checked-in plugin examples and the manifest v3 golden
examples used by tests.

## topo-plugin-example

A minimal generator plugin that roughens terrain after the built-in erosion
stage.

- Source: [`topo-plugin-example/app/Main.hs`](../../topo-plugin-example/app/Main.hs)
- Demonstrates: `PluginDef`, `GeneratorDef`, numeric `ParamDef`s,
  SDK-generated manifest v3, and terrain payload helpers.
- Expected manifest shape: generator-only, `readTerrain` + `log` capabilities,
  runtime protocol bounds, and `ui.displayName` defaulted from `pdName`.

## topo-plugin-civ-example

A larger plugin that owns an overlay, seeds it during generation, ticks it
during simulation, and exposes data resources.

- Source: [`topo-plugin-civ-example/app/Main.hs`](../../topo-plugin-civ-example/app/Main.hs)
- Schema: [`topo-plugin-civ-example/civilization.toposchema`](../../topo-plugin-civ-example/civilization.toposchema)
- Demonstrates: overlay schemas, `GeneratorDef`, `SimulationDef`, parameters,
  data resources, and SDK data-service handlers.

## Manifest v3 golden examples

The manifest contract includes two docs/golden examples that are decoded and
compared against generated Haskell values in `Spec.PluginRPC`:

- [Provider manifest](examples/manifest-v3-provider.json) — declares generator,
  simulation, overlay, data resource schema, and a provider-owned external data
  source.
- [Consumer manifest](examples/manifest-v3-consumer.json) — declares a plugin
  that consumes the provider-owned source through a backend-neutral reference.

Use these JSON files as starting points when building non-Haskell plugins or
when reviewing manifest-contract changes.
