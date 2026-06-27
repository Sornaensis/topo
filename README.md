# topo world generator library

- `topo`: library for large terrain data generation and manipulation
- `topo-seer`: SDL application and direct HTTP/OpenAPI host for interacting with the generator
- `topo-plugin-sdk`: helpers for external topo plugins
- `topo-plugin-example` / `topo-plugin-civ-example`: fixture plugins

## Quick start

- Build all packages: `stack build`
- Run tests: `stack test`
- Run the SDL app: `stack exec topo-seer`
- Run the headless HTTP/OpenAPI host:
  `stack exec topo-seer -- --headless --http 127.0.0.1:7373`
- Run the Writ worldbuilding smoke workflow:
  `stack test topo:test:topo-test --test-arguments "--match Writ"`

## HTTP/OpenAPI automation

Topo 1.0 automation goes directly through the `topo-seer` HTTP server. The
OpenAPI document is served by the running process and is the operation catalog
for client tooling:

```sh
curl http://127.0.0.1:7373/health
curl http://127.0.0.1:7373/openapi.json
curl http://127.0.0.1:7373/state
curl 'http://127.0.0.1:7373/terrain/hex?q=0&r=0'
curl -X POST http://127.0.0.1:7373/ui/seed \
  -H 'Content-Type: application/json' \
  -d '{"seed":123}'
```

The retired MCP bridge is not a 1.0 automation path. Migration notes and the
old-tool/resource mapping live in `docs/inventory/mcp-http-parity.md`; new
clients should prefer resource-oriented HTTP routes over compatibility command
fallbacks.

## Runtime dependencies

`topo-seer` is the only SDL executable. It depends on SDL2, SDL2_ttf, and
Hyperspace through the Git extra-deps pinned in `stack.yaml`; the native SDL
runtime libraries still need to be available on systems that launch the app.
The core `topo` library, HTTP headless mode, plugin SDK, and plugin examples can
be built and tested without starting the SDL UI.

## Hyperspace

topo-seer uses [Hyperspace](https://github.com/Sornaensis/hyperspace) for its
actor runtime.
