# topo-seer

topo-seer is an SDL-based editor, UI, and direct HTTP/OpenAPI host for working
with topo terrains.

## Status
- **Stability:** active development, APIs may change.
- **Support:** best-effort via repository issues.

## Quick start

From the repo root:

- Build and run the SDL UI:
	- `stack run topo-seer`
- Run the direct headless HTTP/OpenAPI host:
	- `stack exec topo-seer -- --headless --http 127.0.0.1:7373`
- Run the SDL UI with HTTP enabled:
	- `stack exec topo-seer -- --http 127.0.0.1:7373`
- Build tests (no execution):
	- `stack test topo-seer --no-run-tests`

## HTTP/OpenAPI automation

The supported automation path is direct HTTP to `topo-seer`; generated OpenAPI
is served at `GET /openapi.json` from the same route metadata used by dispatch
and tests. Loopback (`127.0.0.1`) is the default safe binding. Non-loopback
bindings require `--http-token TOKEN`, and protected requests must include
`Authorization: Bearer TOKEN`.

```sh
# Discover the live contract and basic server metadata.
curl http://127.0.0.1:7373/openapi.json
curl http://127.0.0.1:7373/version

# Read state and query terrain through resource-oriented routes.
curl http://127.0.0.1:7373/state
curl 'http://127.0.0.1:7373/terrain/hex?q=0&r=0'

# Mutate UI/service state through JSON requests.
curl -X POST http://127.0.0.1:7373/ui/seed \
	-H 'Content-Type: application/json' \
	-d '{"seed":123}'

# Authenticated request when a token is configured.
curl http://127.0.0.1:7373/state \
	-H 'Authorization: Bearer TOKEN'
```

MCP was a transition bridge and is retired for 1.0. Use the parity matrix at
`../docs/inventory/mcp-http-parity.md` only to migrate old MCP tool/resource
names to HTTP/OpenAPI routes; do not build new automation against MCP or command
IPC.

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

- Keep render-thread ownership strict (no SDL handles in workers).
- Prefer total functions and explicit error handling.
- Add tests for any new public API or protocol changes.
