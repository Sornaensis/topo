# Topo 1.0 workspace

Topo is a Haskell workspace for procedural terrain generation, the `topo-seer`
runtime, and the plugin SDK/examples used by the 1.0 release. The supported 1.0
automation surface is direct HTTP/OpenAPI served by `topo-seer`; MCP and legacy
command IPC are not public automation paths.

## Supported package roles

| Package | Version | 1.0 role |
|---|---:|---|
| `topo` | `1.0.0.0` | Core terrain, world, overlay, simulation, persistence, and format library. Includes `topo:test:topo-test`. |
| `topo-seer` | `1.0.0.0` | SDL UI executable plus the direct HTTP/OpenAPI host. It is the only package that launches the app, opens an SDL window, or serves the 1.0 HTTP API. |
| `topo-plugin-sdk` | `1.0.0.0` | SDK types, runner, and fixtures for external plugin authors. |
| `topo-plugin-example` | `1.0.0.0` | Minimal generator plugin example and test fixture. |
| `topo-plugin-civ-example` | `1.0.0.0` | Civilization/data-resource plugin example and test fixture. |

## Quick start

Run commands from the repository root.

```sh
# Build and test the whole workspace.
stack build
stack test

# Build and test only the topo-seer runtime package.
stack build topo-seer:exe:topo-seer
stack test topo-seer:test:topo-seer-test

# Launch the SDL UI.
stack exec topo-seer --

# Launch the SDL UI with HTTP/OpenAPI enabled.
stack exec topo-seer -- --http 127.0.0.1:7373

# Launch the headless HTTP/OpenAPI host for automation or CI.
stack exec topo-seer -- --headless --http 127.0.0.1:7373

# Run the Writ worldbuilding smoke workflow.
stack test topo:test:topo-test --test-arguments "--match Writ"
```

Headless mode starts the service/actor runtime without an SDL window and requires
an HTTP binding (`--headless --http HOST:PORT`). Use loopback (`127.0.0.1`) for
local automation unless you intentionally need remote access.

## HTTP/OpenAPI automation

Topo 1.0 automation goes directly through the `topo-seer` HTTP server. The
OpenAPI document is served by the running process and is the operation catalog
for client tooling:

```sh
# In one terminal:
stack exec topo-seer -- --headless --http 127.0.0.1:7373

# In another terminal:
curl http://127.0.0.1:7373/health
curl http://127.0.0.1:7373/openapi.json
curl http://127.0.0.1:7373/state
curl 'http://127.0.0.1:7373/terrain/hex?q=0&r=0'
curl -X POST http://127.0.0.1:7373/ui/seed \
  -H 'Content-Type: application/json' \
  -d '{"seed":123}'
```

Loopback bindings may run without a token. Non-loopback bindings require
`--http-token TOKEN` before the server starts:

```sh
stack exec topo-seer -- --headless --http 0.0.0.0:7373 --http-token TOKEN
curl http://127.0.0.1:7373/health
curl http://127.0.0.1:7373/openapi.json \
  -H 'Authorization: Bearer TOKEN'
```

When a token is configured, `GET /health` remains unauthenticated for readiness
checks and every other route, including `GET /openapi.json`, requires
`Authorization: Bearer TOKEN`.

The retired MCP bridge and legacy command IPC are not 1.0 automation paths.
Migration guidance lives in `docs/migration/pre-1.0-to-1.0.md`, release notes
live in `docs/release-notes/1.0.md`, and the old-tool/resource mapping remains
in `docs/inventory/mcp-http-parity.md`; new clients should use resource-oriented
HTTP/OpenAPI routes only. Command IPC remains internal/test compatibility while
service extraction continues, with no public 1.0 exception.

## Release packaging

Reproducible source tarball, executable staging, and `v1.0.0` tag instructions
live in [docs/release.md](docs/release.md). User-facing changes are summarized
in [CHANGELOG.md](CHANGELOG.md) and [docs/release-notes/1.0.md](docs/release-notes/1.0.md).

## Runtime dependencies

`topo-seer` is the only SDL executable. It depends on SDL2, SDL2_ttf, and
Hyperspace through the Git extra-deps pinned in `stack.yaml`; the native SDL
runtime libraries still need to be available on systems that launch the app.
The core `topo` library, plugin SDK, and plugin examples can be built and tested
without starting the SDL UI. Headless `topo-seer` runs without creating an SDL
window, but it is still the `topo-seer` executable and uses the same package
closure.

## Hyperspace

`topo-seer` uses [Hyperspace](https://github.com/Sornaensis/hyperspace) for its
actor runtime.
