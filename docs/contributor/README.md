# Contributing locally

Run development commands from the repository root. The ordinary local workflow
is:

```console
stack build
stack test
```

Use focused targets while iterating, then run the complete suite before
submitting a change. Common examples are:

```console
stack test topo:test:topo-test
stack test topo-seer:test:topo-seer-test
stack test topo-seer:test:topo-seer-test --test-arguments "--match Documentation"
```

A behavior change should update its source, focused specifications, generated
mirrors when applicable, and audience documentation together.

## Hpack and Cabal files

Each package's `package.yaml` is the Hpack source. Do not edit a generated
`.cabal` file by hand. Change `package.yaml` when component metadata or explicit
module lists require it, run `stack build` to invoke Hpack, and include the
resulting `.cabal` update. A newly discovered test module may require only the
source file and regenerated Cabal output when no explicit Hpack metadata changes.
Review the generated diff to ensure it contains only the intended synchronization.

## Generated documentation artifacts

Refresh generated publications in this order:

1. Change the canonical Haskell generator, contract, and its behavioral tests.
2. Encode the canonical value to its existing publication path without
   hand-editing the JSON.
3. Update explanatory Markdown affected by the contract change.
4. Run the focused drift specifications, followed by `stack test`.

The maintained mirrors are:

- `docs/operator/openapi.json`, generated from
  `openApiDocument publicHttpRouteSpecs` and checked by `Spec.HTTP`;
- `docs/plugin/manifest-v3.schema.json` and the provider and consumer examples,
  generated from `Topo.Plugin.RPC.Manifest` values and checked by
  `Spec.PluginRPC`.

Keep JSON behavioral equality in those specifications. Documentation
consistency checks cover the approved tree, links, anchors, and obsolete
references; they do not duplicate the JSON parsers.
