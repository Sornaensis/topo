# Topo documentation

This directory is the documentation entry point for the Topo workspace. Choose
the guide for the way you use the project:

- **Users:** [getting started](user/getting-started.md) and the
  [topo-seer workflow guide](user/topo-seer.md).
- **Operators and HTTP clients:** the [operator guide](operator/README.md) and
  its generated [OpenAPI mirror](operator/openapi.json).
- **Haskell integrators:** the [library guide](integrator/README.md) and
  [file-format reference](integrator/file-formats.md).
- **Plugin authors:** the [plugin guide](plugin/README.md),
  [protocol reference](plugin/protocol.md), generated
  [manifest schema](plugin/manifest-v3.schema.json), and canonical
  [provider](plugin/examples/provider.json) and
  [consumer](plugin/examples/consumer.json) examples.
- **Contributors:** the [local development guide](contributor/README.md).
- **Upgrading users and clients:** the [1.0 migration guide](migration/1.0.md).

## Sources of truth

Documentation explains the maintained behavior but does not define it. Use
these sources when code and prose disagree:

- A package's `package.yaml` exposed modules and its public umbrella exports
  define its public Haskell API surface. Haddock, source, and tests define the
  details.
- `Seer.Service.AppService.appServiceOperationSpecs`,
  `Seer.HTTP.Server.publicHttpRouteSpecs`, and the runtime-generated
  `openApiDocument` define service and HTTP behavior. Prose must not become a
  separately maintained operation inventory.
- Generated values in `Topo.Plugin.RPC.Manifest` define the manifest schema and
  examples. `Topo.Plugin.RPC.Protocol`, `Topo.Plugin.RPC.Scope`,
  `Topo.Plugin.RPC.Stream`, `Topo.Plugin.RPC.Transport`, and the SDK tests define
  protocol behavior.
- Storage and schema modules, together with their round-trip tests, define file
  formats.

`operator/openapi.json` and the JSON files under `plugin/` are generated
publication mirrors. Drift tests compare them with their canonical Haskell
generators. Regenerate them after changing those sources.

Markdown is explanatory and audience-oriented. It is not a feature-status,
waiver, or roadmap database. Change source and tests first, then update generated
artifacts and prose in the same change.
