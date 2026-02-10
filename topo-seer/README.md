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

topo-seer is built around a render-thread-owned SDL loop and an actor system (Hyperspace) for async work.

### Actor graph (high level)

```text
UI input (SDL events)
	-> UiActions actor
		 -> Terrain actor (world generation)
		 -> Data actor (terrain/climate/weather snapshots)
		 -> Log actor (log entries)

SnapshotReceiver (cached composite snapshot)
	<- Ui/Log/Data replies
	-> Render loop pulls RenderSnapshot per frame

Atlas pipeline
	Render loop -> AtlasManager (jobs)
						 -> AtlasWorker (CPU build)
						 -> AtlasResultBroker (results)
						 -> AtlasCache (GPU textures)

Terrain cache pipeline
	Render loop -> TerrainCacheWorker (CPU build)
						 -> TerrainCacheBroker (latest result)
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

**Actor messaging only (no shared mutable state across threads):**
- Snapshot updates flow into `SnapshotReceiver` via reply casts.
- Render loop pulls `RenderSnapshot` per frame (no polling threads).
- Atlas results go through `AtlasResultBroker`.
- Terrain cache results go through `TerrainCacheBroker`.

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
