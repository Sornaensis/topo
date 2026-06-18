# topo-seer

topo-seer is an SDL-based editor and UI for working with topo terrains.

## Status
- **Stability:** active development, APIs may change.
- **Support:** best-effort via repository issues.

## Quick start

From the repo root:

- Build and run:
	- `stack run topo-seer`
- Build tests (no execution):
	- `stack test topo-seer --no-run-tests`

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
