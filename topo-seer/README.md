# topo-seer

topo-seer is an SDL-based editor, UI, and direct HTTP/OpenAPI host for working
with topo terrains.

## Status
- **1.0 runtime surface:** SDL UI plus direct HTTP/OpenAPI from this executable.
- **Automation support:** direct resource-oriented HTTP/OpenAPI from this executable.
- **Support:** best-effort via repository issues.

## Quick start

Run commands from the repository root.

```sh
# Build the topo-seer executable and library.
stack build topo-seer:exe:topo-seer

# Run the topo-seer test suite.
stack test topo-seer:test:topo-seer-test

# Launch the SDL UI.
stack exec topo-seer --

# Launch the SDL UI with HTTP/OpenAPI enabled.
stack exec topo-seer -- --http 127.0.0.1:7373

# Launch the headless HTTP/OpenAPI host for local automation.
stack exec topo-seer -- --headless --http 127.0.0.1:7373
```

## Runtime modes

- **SDL UI:** `stack exec topo-seer --` starts the interactive editor and render
  loop.
- **SDL UI + HTTP:** add `--http HOST:PORT` to serve the same HTTP/OpenAPI API
  while the UI is running.
- **Headless HTTP:** `--headless --http HOST:PORT` starts the service/actor
  runtime and HTTP server without creating an SDL window. `--headless` requires
  `--http`; use this mode for local automation.

The HTTP binding accepts `--http HOST:PORT` or `--http=HOST:PORT`. The
`topo-seer` executable links SDL2/SDL2_ttf in every mode, so the native runtime
libraries must be available even when you run headless.

## HTTP/OpenAPI automation

The supported automation path is direct HTTP to `topo-seer`; generated OpenAPI
is served at `GET /openapi.json` from the same route metadata used by dispatch
and tests. See the [operator guide](../docs/operator/README.md) and its generated
[OpenAPI publication mirror](../docs/operator/openapi.json).

```sh
# Start the local headless host.
stack exec topo-seer -- --headless --http 127.0.0.1:7373

# Discover the live contract and basic server metadata.
curl http://127.0.0.1:7373/health
curl http://127.0.0.1:7373/openapi.json
curl http://127.0.0.1:7373/version

# Read state and query terrain through resource-oriented routes.
curl http://127.0.0.1:7373/state
curl 'http://127.0.0.1:7373/terrain/hex?q=0&r=0'

# Mutate UI/service state through JSON requests.
curl -X POST http://127.0.0.1:7373/ui/seed \
  -H 'Content-Type: application/json' \
  -d '{"seed":123}'
```

Loopback (`127.0.0.1`, `localhost`, and other loopback addresses) is the
default safe binding and may run without a token. Non-loopback bindings require
a non-empty bearer token before the server starts:

```sh
stack exec topo-seer -- --headless --http 0.0.0.0:7373 --http-token TOKEN
curl http://127.0.0.1:7373/health
curl http://127.0.0.1:7373/openapi.json \
  -H 'Authorization: Bearer TOKEN'
```

When `--http-token TOKEN` is configured, `GET /health` remains unauthenticated
for readiness checks and every other route, including `GET /openapi.json`,
requires `Authorization: Bearer TOKEN`.

Clients should use the live OpenAPI document as the operation catalogue. The
[1.0 migration guide](../docs/migration/1.0.md) covers removed routes and current
resource-oriented replacements.

## Documentation

The workspace [documentation index](../docs/README.md) links the maintained user,
operator, integrator, plugin-author, contributor, and migration guides. Runtime
workflows begin in the [topo-seer user guide](../docs/user/topo-seer.md).

## Related workspace packages

- `topo`: core terrain/world/overlay/simulation/persistence library.
- `topo-plugin-sdk`: SDK types and runner for external plugins.
- `topo-plugin-example` and `topo-plugin-civ-example`: maintained plugin examples
  and fixtures for generator, simulation, and data-resource behavior.

## Architecture overview

topo-seer is built around a render-thread-owned SDL loop and a Hyperspace actor
system for async work. Shared runtime snapshots and render work queues use
IORef-backed channels so the SDL thread never shares SDL handles with workers.

### Actor graph (high level)

```text
UI input (SDL events)
	-> UiActions actor
		 -> Terrain actor (world generation)
		 -> Data actor (terrain/climate/weather snapshots)
		 -> Log actor (log entries)
		 -> Simulation actor (ticks and world updates)

Snapshot refs (IORef-backed)
	<- Ui/Log/Data/Simulation writes
	-> Render loop assembles RenderSnapshot per frame

Atlas pipeline
	Render loop -> AtlasScheduler / AtlasManager (jobs)
						 -> AtlasWorker actors (CPU build)
						 -> Atlas result ref
						 -> AtlasCache (GPU textures)

Terrain cache pipeline
	Render loop -> TerrainCacheWorker (CPU build)
						 -> Terrain cache ref
```

### Thread ownership and boundaries

**Render thread only (SDL/GPU resources):**
- `SDL.Renderer`, `SDL.Window`, `SDL.Texture`, `FontCache`
- Terrain texture cache (`ChunkTextureCache`)
- Atlas texture uploads

**Worker actors (CPU-only):**
- Atlas geometry builds (`AtlasWorker`)
- Terrain cache builds (`TerrainCacheWorker`)
- World generation (`Terrain` actor)

**Actor messaging and IORef-backed data channels only:**
- Snapshot updates are published to per-domain snapshot refs.
- Render loop assembles `RenderSnapshot` per frame from the latest refs.
- Atlas scheduling/results use `AtlasScheduleRef` and `AtlasResultRef`.
- Terrain cache results use `TerrainCacheRef`.

## Data boundaries and message types

- `RenderSnapshot` contains `UiState`, `LogSnapshot`, `DataSnapshot`, and `TerrainSnapshot`.
- `TerrainCacheBuildRequest` carries a cache key, UI state, terrain snapshot, and reply channel.
- `TerrainCacheBuildResult` carries the cache key and built `TerrainCache`.
- `AtlasBuildResult` carries an atlas key, scale, and tile geometry.

These messages are designed to be CPU-only data with no SDL handles.

## Notes for contributors

Follow the [local contributor guide](../docs/contributor/README.md). Keep
render-thread ownership strict (no SDL handles in workers), prefer total
functions and explicit error handling, and add tests for public API or protocol
changes.
